use nihav_core::codecs::*;
use nihav_codec_support::codecs::ZERO_MV;
use super::super::vp78::PredMode;
use super::super::vp78dsp::*;
use super::super::vp7data::HIGH_EDGE_VAR_THR;
use super::super::vp7dsp::*;
use super::blocks::*;
use super::coder::*;
use super::mb_coding::*;
use super::models::*;
use super::motion_est::*;
use super::rdo::*;

const MBT_Q_OFFSET: usize = 3;

pub struct LoopParams {
    pub loop_sharpness:     u8,
    pub loop_filter_level:  u8,
    pub lf_simple:          bool,
}

pub struct FrameEncoder {
    mb_w:               usize,
    mb_h:               usize,
    pub loop_params:    LoopParams,

    sblocks:            Vec<SrcBlock>,
    res:                Vec<Residue>,
    mbtypes:            Vec<MBType>,
    recon:              Vec<SrcBlock>,
    features:           Vec<u8>,
    has_features:       bool,

    pctx:               PredContext,

    me_mode:            MVSearchMode,
    me_range:           i16,
    mc_buf1:            NAVideoBufferRef<u8>,
    mc_buf2:            NAVideoBufferRef<u8>,
    mv_search_last:     Box<dyn MVSearch + Send>,
    mv_search_gold:     Box<dyn MVSearch + Send>,
}

impl FrameEncoder {
    pub fn new(mc_buf1: NAVideoBufferRef<u8>, mc_buf2: NAVideoBufferRef<u8>) -> Self {
        let me_mode = MVSearchMode::default();

        Self {
            mb_w:       0,
            mb_h:       0,

            sblocks:    Vec::new(),
            res:        Vec::new(),
            mbtypes:    Vec::new(),
            recon:      Vec::new(),
            features:   Vec::new(),
            has_features:   false,

            pctx:       PredContext::new(),

            loop_params: LoopParams {
                    loop_filter_level:  0,
                    loop_sharpness:     0,
                    lf_simple:          true,
                },
            me_mode,
            me_range:   0,
            mv_search_last: me_mode.create_search(),
            mv_search_gold: me_mode.create_search(),
            mc_buf1, mc_buf2,
        }
    }
    pub fn resize(&mut self, mb_w: usize, mb_h: usize) {
        self.mb_w = mb_w;
        self.mb_h = mb_h;

        self.pctx.resize(mb_w, mb_h);

        self.sblocks.clear();
        self.sblocks.reserve(mb_w * mb_h);
        self.res.clear();
        self.res.reserve(mb_w * mb_h);
        self.mbtypes.clear();
        self.mbtypes.reserve(mb_w * mb_h);
        self.recon.clear();
        self.recon.reserve(mb_w * mb_h);
        self.features.clear();
        self.features.reserve(mb_w * mb_h);
    }
    pub fn set_me_params(&mut self, me_mode: MVSearchMode, me_range: i16, version: u8) {
        self.me_range = me_range;
        if self.me_mode != me_mode {
            self.me_mode = me_mode;
            self.mv_search_last = me_mode.create_search();
            self.mv_search_gold = me_mode.create_search();
        }
        self.pctx.version = version;
    }
    pub fn load_frame(&mut self, vbuf: &NAVideoBuffer<u8>) {
        load_blocks(vbuf, &mut self.sblocks);
    }

    pub fn mb_tree_search(&mut self, ref_frm: NAVideoBufferRef<u8>, mb_map: &[usize], new_mb_map: &mut [usize], mb_weights: &mut [usize]) {
        let mut mv_est = MVEstimator::new(ref_frm, self.mc_buf1.clone(), self.me_range);
        self.mv_search_last.preinit(&mv_est);
        let mut mb_idx = 0;
        new_mb_map.copy_from_slice(mb_map);
        for (mb_y, mb_row) in self.sblocks.chunks(self.mb_w).enumerate() {
            for (mb_x, blk) in mb_row.iter().enumerate() {
                let (mv, _) = self.mv_search_last.search_mb(&mut mv_est, blk, mb_x, mb_y);

                if mv != ZERO_MV {
                    let new_x = ((((mb_x as isize) * 64 + (mv.x as isize) + 32) >> 6).max(0) as usize).min(self.mb_w - 1);
                    let new_y = ((((mb_y as isize) * 64 + (mv.y as isize) + 32) >> 6).max(0) as usize).min(self.mb_h - 1);
                    let nidx = new_x + new_y * self.mb_w;
                    new_mb_map[mb_idx] = mb_map[nidx];
                }
                mb_weights[new_mb_map[mb_idx]] += 1;
                mb_idx += 1;
            }
        }
    }

    pub fn intra_blocks(&mut self, base_q: usize, metric: &RateDistMetric, models: &VP7Models, mbt_map: Option<&[usize]>) {
        self.mbtypes.clear();
        self.pctx.reset();
        self.pctx.reset_intra();
        self.res.clear();
        self.recon.clear();
        self.features.clear();

        self.has_features = false;
        if base_q > MBT_Q_OFFSET {
            if let Some(map) = mbt_map {
                let sum: usize = map.iter().sum();
                let size = map.len();
                let avg = (sum + size / 2) / size;
                for &val in map.iter() {
                    if val > avg {
                        self.features.push(1);
                        self.has_features = true;
                    } else {
                        self.features.push(0);
                    }
                }
            } else {
                for _ in 0..(self.mb_w * self.mb_h) {
                    self.features.push(0);
                }
            }
        } else {
            for _ in 0..(self.mb_w * self.mb_h) {
                self.features.push(0);
            }
        }

        let mut imctx = IntraModePredCtx {
                metric,
                models,
                tr:         [0; 4],
                q:          base_q,
                ipred_y:    IPredContext::default(),
                ipred_u:    IPredContext::default(),
                ipred_v:    IPredContext::default(),
                pctx:       BlockPCtx::default(),
            };

        for (mb_y, mb_row) in self.sblocks.chunks_mut(self.mb_w).enumerate() {
            imctx.ipred_y.has_top = mb_y != 0;
            imctx.ipred_u.has_top = mb_y != 0;
            imctx.ipred_v.has_top = mb_y != 0;

            for (mb_x, sblk) in mb_row.iter().enumerate() {
                self.pctx.fill_ipred(0, mb_x, &mut imctx.ipred_y);
                self.pctx.fill_ipred(1, mb_x, &mut imctx.ipred_u);
                self.pctx.fill_ipred(2, mb_x, &mut imctx.ipred_v);
                self.pctx.fill_pctx(mb_x, &mut imctx.pctx);
                if self.has_features {
                    imctx.q = if self.features[mb_x + mb_y * self.mb_w] != 0 {
                            base_q - MBT_Q_OFFSET
                        } else {
                            base_q
                        };
                }

                let mut res = Residue::new();
                let mut newblk = SrcBlock::default();

                imctx.tr = self.pctx.get_ipred_tr(mb_x);
                let mut mb_type = select_intra_mode(sblk, &mut newblk, &mut res, &imctx, MAX_DIST, MBType::InterNoMV(false, [0;4]));

                let use_i4 = match mb_type {
                        MBType::Intra(best_ymode, best_cmode) => {
                            sblk.apply_ipred_luma(best_ymode, &imctx.ipred_y, &mut res);
                            newblk.fill_ipred_luma(best_ymode, &imctx.ipred_y);
                            sblk.apply_ipred_chroma(best_cmode, &imctx.ipred_u, &imctx.ipred_v, &mut res);
                            newblk.fill_ipred_chroma(best_cmode, &imctx.ipred_u, &imctx.ipred_v);
                            res.fdct();
                            res.fdct_dc_block();

                            self.pctx.ymodes.set_mode(mb_x, best_ymode);

                            false
                        },
                        MBType::Intra4x4(ref i4_modes, ref mut i4ctx, best_cmode) => {
                            sblk.apply_ipred_chroma(best_cmode, &imctx.ipred_u, &imctx.ipred_v, &mut res);
                            newblk.fill_ipred_chroma(best_cmode, &imctx.ipred_u, &imctx.ipred_v);
                            res.fdct();

                            self.pctx.ymodes.set_modes4x4(mb_x, i4_modes, i4ctx);

                            true
                        },
                        _ => unreachable!(),
                };

                res.quant(imctx.q);
                self.pctx.set_nz(mb_x, &res);
                let mut recon = res.clone();
                self.res.push(res);
                self.mbtypes.push(mb_type);

                if !use_i4 {
                    recon.add_residue(&mut newblk);
                } else {
                    recon.add_residue_chroma(&mut newblk);
                }

                self.pctx.update_mb(&newblk, mb_x);
                self.recon.push(newblk);
            }
            self.pctx.update_mb_row();
        }
    }
    pub fn inter_blocks(&mut self, q: usize, metric: &RateDistMetric, models: &VP7Models, last_frame: &NABufferType, gold_frame: &NABufferType) {
        self.has_features = false;

        let mut mv_est_last = MVEstimator::new(last_frame.get_vbuf().unwrap(), self.mc_buf1.clone(), self.me_range);
        self.mv_search_last.preinit(&mv_est_last);
        let mut mv_est_gold = if let Some(gbuf) = gold_frame.get_vbuf() {
                let mv_est = MVEstimator::new(gbuf, self.mc_buf2.clone(), self.me_range);
                self.mv_search_gold.preinit(&mv_est);
                Some(mv_est)
            } else {
                None
            };

        self.mbtypes.clear();
        self.pctx.reset();
        self.pctx.save_dc_pred();
        self.res.clear();
        self.recon.clear();
        self.features.clear();

        let mut imctx = IntraModePredCtx {
                metric,
                models,
                tr:         [0; 4],
                q,
                ipred_y:    IPredContext::default(),
                ipred_u:    IPredContext::default(),
                ipred_v:    IPredContext::default(),
                pctx:       BlockPCtx::default(),
            };

        for (mb_y, mb_row) in self.sblocks.chunks_mut(self.mb_w).enumerate() {
            imctx.ipred_y.has_top = mb_y != 0;
            imctx.ipred_u.has_top = mb_y != 0;
            imctx.ipred_v.has_top = mb_y != 0;

            for (mb_x, sblk) in mb_row.iter().enumerate() {
                self.pctx.fill_ipred(0, mb_x, &mut imctx.ipred_y);
                self.pctx.fill_ipred(1, mb_x, &mut imctx.ipred_u);
                self.pctx.fill_ipred(2, mb_x, &mut imctx.ipred_v);
                self.pctx.fill_pctx(mb_x, &mut imctx.pctx);

                let mut res = Residue::new();
                let mut newblk = SrcBlock::default();

                let (mvprobs, nearest_mv, near_mv, pred_mv) = self.pctx.find_mv_pred(mb_x, mb_y);

                let (mv, _dist) = self.mv_search_last.search_mb(&mut mv_est_last, sblk, mb_x, mb_y);

                mv_est_last.get_mb(&mut newblk, mb_x, mb_y, mv);
                let mv_nits_dist = metric.calc_metric(0, inter_mv_nits(mv, &mvprobs, nearest_mv, near_mv, pred_mv, models));
                let last_dist = calc_inter_mb_dist(sblk, &newblk, &mut res, &imctx, self.pctx.get_y2_dc_pred(true)) + mv_nits_dist;

                let (gmv, gold_dist) = if last_dist > SMALL_DIST {
                        if let Some(ref mut mv_est) = &mut mv_est_gold {
                            let (gmv, _gdist) = self.mv_search_gold.search_mb(mv_est, sblk, mb_x, mb_y);
                            mv_est.get_mb(&mut newblk, mb_x, mb_y, gmv);
                            let mv_nits_dist = metric.calc_metric(0, inter_mv_nits(gmv, &mvprobs, nearest_mv, near_mv, pred_mv, models));
                            let gdist = calc_inter_mb_dist(sblk, &newblk, &mut res, &imctx, self.pctx.get_y2_dc_pred(false)) + mv_nits_dist;
                            (gmv, gdist)
                        } else {
                            (ZERO_MV, MAX_DIST)
                        }
                    } else {
                        (ZERO_MV, MAX_DIST)
                    };

                let (last, mut inter_dist, mv, mv_est) = if last_dist < gold_dist {
                        (true, last_dist, mv, &mut mv_est_last)
                    } else if let Some (ref mut mv_est) = &mut mv_est_gold {
                        (false, gold_dist, gmv, mv_est)
                    } else {
                        unreachable!()
                    };

                let mut mb_type = if mv == ZERO_MV {
                        MBType::InterNoMV(last, mvprobs)
                    } else if mv == nearest_mv {
                        MBType::InterNearest(last, mvprobs)
                    } else if mv == near_mv {
                        MBType::InterNear(last, mvprobs)
                    } else {
                        MBType::InterMV(last, mvprobs, mv - pred_mv)
                    };
                if inter_dist > SMALL_DIST {
                    if let MBType::InterMV(_, _, _) = mb_type { // xxx: maybe do it for all types?
                        let mv_search = if last { &mut self.mv_search_last } else { &mut self.mv_search_gold };
                        if let Some((mbt, dist)) = try_inter_split(sblk, &mut newblk, &mut res, mvprobs, nearest_mv, near_mv, pred_mv, last, mb_x, mb_y, mv_search, mv_est, &mut self.pctx, &imctx, inter_dist) {
                            mb_type = mbt;
                            inter_dist = dist;
                        }
                    }
                }

                if inter_dist > SMALL_DIST {
                    imctx.tr = self.pctx.get_ipred_tr(mb_x);
                    mb_type = select_intra_mode(sblk, &mut newblk, &mut res, &imctx, inter_dist, mb_type);
                }

                self.mbtypes.push(mb_type);
                res.reset();
                match mb_type {
                    MBType::Intra(ymode, cmode) => {
                        newblk.fill_ipred_luma(ymode, &imctx.ipred_y);
                        newblk.fill_ipred_chroma(cmode, &imctx.ipred_u, &imctx.ipred_v);
                        self.pctx.ymodes.set_mode(mb_x, ymode);
                        self.pctx.fill_mv(mb_x, mb_y, ZERO_MV);
                    },
                    MBType::Intra4x4(ref i4_modes, ref mut i4ctx, cmode) => {
                        newblk.fill_ipred_chroma(cmode, &imctx.ipred_u, &imctx.ipred_v);
                        self.pctx.ymodes.set_modes4x4(mb_x, i4_modes, i4ctx);
                        self.pctx.fill_mv(mb_x, mb_y, ZERO_MV);
                    },
                    MBType::InterNoMV(_, _) |
                    MBType::InterNearest(_, _) |
                    MBType::InterNear(_, _) |
                    MBType::InterMV(_, _, _) => {
                        mv_est.get_mb(&mut newblk, mb_x, mb_y, mv);
                        self.pctx.fill_mv(mb_x, mb_y, mv);
                        self.pctx.ymodes.set_mode(mb_x, PredMode::Inter);
                    },
                    MBType::InterSplitMV(_, _, _, _, _) => {
                        self.pctx.ymodes.set_mode(mb_x, PredMode::Inter);
                        recon_split_mb(&mut newblk, mb_x, mb_y, &self.pctx.mvs, self.pctx.mv_stride, mv_est);
                    },
                };
                if let MBType::Intra4x4(_, _, _) = mb_type {
                    res.set_chroma_from_diff(&sblk.chroma, &newblk.chroma);
                    res.fdct();
                } else {
                    res.set_luma_from_diff(&sblk.luma, &newblk.luma);
                    res.set_chroma_from_diff(&sblk.chroma, &newblk.chroma);
                    res.fdct();
                    res.fdct_dc_block();
                    if !mb_type.is_intra() {
                        requant_y2_dc(&mut res.dcs[0], q);
                        self.pctx.predict_y2_dc(&mut res.dcs[0], last);
                    }
                }

                res.quant(q);
                self.pctx.set_nz(mb_x, &res);
                let mut recon = res.clone();
                self.res.push(res);
                self.features.push(0);
                if let MBType::Intra4x4(_, _, _) = mb_type {
                    recon.add_residue_chroma(&mut newblk);
                } else {
                    recon.add_residue(&mut newblk);
                }
                self.pctx.update_mb(&newblk, mb_x);
                self.recon.push(newblk);
            }
            self.pctx.update_mb_row();
        }
    }
    pub fn encode_features(&self, bc: &mut BoolEncoder, q: usize, models: &VP7Models) -> EncoderResult<()> {
        if self.has_features {
            // first feature - quantiser
            bc.put_bool(true, 128)?;
            bc.put_byte(models.feature_present[0])?;
            for &prob in models.feature_tree_probs[0].iter() {
                bc.put_bool(prob != 255, 128)?;
                if prob != 255 {
                    bc.put_byte(prob)?;
                }
            }
            bc.put_bool(true, 128)?;
            bc.put_bits((q - MBT_Q_OFFSET) as u32, 7)?;
            for _ in 1..4 {
                bc.put_bool(false, 128)?; // other quants
            }

            // other features (
            for _ in 1..4 {
                bc.put_bool(false, 128)?;
            }
        } else {
            for _ in 0..4 {
                bc.put_bool(false, 128)?;
            }
        }
        Ok(())
    }
    pub fn encode_mb_types(&self, bc: &mut BoolEncoder, is_intra: bool, models: &VP7Models) -> EncoderResult<()> {
        for (mb_type, &feature) in self.mbtypes.iter().zip(self.features.iter()) {
            if self.has_features {
                bc.encode_feature(0, if feature == 0 { None } else { Some(0) }, models)?;
            }
            bc.encode_mb_type(is_intra, mb_type, models)?;
        }
        Ok(())
    }
    pub fn encode_residues(&mut self, bc: &mut BoolEncoder, models: &VP7Models) -> EncoderResult<()> {
        self.pctx.reset();
        //self.pctx.restore_dc_pred();
        for (_mb_y, mb_row) in self.res.chunks(self.mb_w).enumerate() {
            for (mb_x, blk) in mb_row.iter().enumerate() {
                if blk.has_dc {
                    let pctx = (self.pctx.nz_y2_left as u8) + (self.pctx.nz_y2_top[mb_x] as u8);
                    bc.encode_subblock(&blk.dcs, 1, pctx, models)?;
                    let has_nz = blk.dcs.has_nz();
                    self.pctx.nz_y2_left = has_nz;
                    self.pctx.nz_y2_top[mb_x] = has_nz;
                }
                let ytype = if blk.has_dc { 0 } else { 3 };
                for (y, blk_row) in blk.luma.chunks(4).enumerate() {
                    for (x, blk) in blk_row.iter().enumerate() {
                        let pctx = (self.pctx.nz_y_left[y] as u8) + (self.pctx.nz_y_top[mb_x * 4 + x] as u8);
                        bc.encode_subblock(blk, ytype, pctx, models)?;
                        let has_nz = blk.has_nz();
                        self.pctx.nz_y_left[y] = has_nz;
                        self.pctx.nz_y_top[mb_x * 4 + x] = has_nz;
                    }
                }

                for (c, chroma) in blk.chroma.iter().enumerate() {
                    for (y, blk_row) in chroma.chunks(2).enumerate() {
                        for (x, blk) in blk_row.iter().enumerate() {
                            let pctx = (self.pctx.nz_c_left[c][y] as u8) + (self.pctx.nz_c_top[c][mb_x * 2 + x] as u8);
                            bc.encode_subblock(blk, 2, pctx, models)?;
                            let has_nz = blk.has_nz();
                            self.pctx.nz_c_left[c][y] = has_nz;
                            self.pctx.nz_c_top[c][mb_x * 2 + x] = has_nz;
                        }
                    }
                }
            }
            self.pctx.update_mb_row();
        }
        Ok(())
    }
    pub fn generate_models(&mut self, is_intra: bool, stats: &mut VP7ModelsStat) {
        stats.reset();
        let est = Estimator::new();
        self.pctx.reset();
        if self.has_features {
            for &feat in self.features.iter() {
                est.estimate_feature(0, if feat == 0 { None } else { Some(0) }, stats);
            }
        }
        for (mbt_row, mb_row) in self.mbtypes.chunks(self.mb_w).zip(self.res.chunks(self.mb_w)) {
            for (mb_x, (mbtype, blk)) in mbt_row.iter().zip(mb_row.iter()).enumerate() {
                est.estimate_mb_type(is_intra, mbtype, stats);
                if blk.has_dc {
                    let pctx = (self.pctx.nz_y2_left as u8) + (self.pctx.nz_y2_top[mb_x] as u8);
                    est.estimate_subblock(&blk.dcs, 1, pctx, stats);
                    let has_nz = blk.dcs.has_nz();
                    self.pctx.nz_y2_left = has_nz;
                    self.pctx.nz_y2_top[mb_x] = has_nz;
                }
                let ytype = if blk.has_dc { 0 } else { 3 };
                for (y, blk_row) in blk.luma.chunks(4).enumerate() {
                    for (x, blk) in blk_row.iter().enumerate() {
                        let pctx = (self.pctx.nz_y_left[y] as u8) + (self.pctx.nz_y_top[mb_x * 4 + x] as u8);
                        est.estimate_subblock(blk, ytype, pctx, stats);
                        let has_nz = blk.has_nz();
                        self.pctx.nz_y_left[y] = has_nz;
                        self.pctx.nz_y_top[mb_x * 4 + x] = has_nz;
                    }
                }

                for (c, chroma) in blk.chroma.iter().enumerate() {
                    for (y, blk_row) in chroma.chunks(2).enumerate() {
                        for (x, blk) in blk_row.iter().enumerate() {
                            let pctx = (self.pctx.nz_c_left[c][y] as u8) + (self.pctx.nz_c_top[c][mb_x * 2 + x] as u8);
                            est.estimate_subblock(blk, 2, pctx, stats);
                            let has_nz = blk.has_nz();
                            self.pctx.nz_c_left[c][y] = has_nz;
                            self.pctx.nz_c_top[c][mb_x * 2 + x] = has_nz;
                        }
                    }
                }
            }
            self.pctx.update_mb_row();
        }
    }
    pub fn reconstruct_frame(&mut self, frm: &mut NASimpleVideoFrame<u8>, is_intra: bool) {
        let mut yidx = frm.offset[0];
        let mut uidx = frm.offset[1];
        let mut vidx = frm.offset[2];
        let ystride = frm.stride[0];
        let ustride = frm.stride[1];
        let vstride = frm.stride[2];

        for (mb_y, (f_row, mb_row)) in self.features.chunks(self.mb_w).zip(self.recon.chunks(self.mb_w)).enumerate() {
            for (mb_x, (&feature, sblk)) in f_row.iter().zip(mb_row.iter()).enumerate() {
                let dst = &mut frm.data[yidx + mb_x * 16..];
                for (dst, src) in dst.chunks_mut(ystride).zip(sblk.luma.chunks(16)) {
                    dst[..16].copy_from_slice(src);
                }
                let dst = &mut frm.data[uidx + mb_x * 8..];
                for (dst, src) in dst.chunks_mut(ustride).zip(sblk.chroma[0].chunks(8)) {
                    dst[..8].copy_from_slice(src);
                }
                let dst = &mut frm.data[vidx + mb_x * 8..];
                for (dst, src) in dst.chunks_mut(vstride).zip(sblk.chroma[1].chunks(8)) {
                    dst[..8].copy_from_slice(src);
                }

                let loop_str = if feature != 2 {
                        self.loop_params.loop_filter_level
                    } else { 0 }; //todo
                loop_filter_mb(frm, mb_x, mb_y, loop_str, &self.loop_params, is_intra);
            }
            yidx += ystride * 16;
            uidx += ustride * 8;
            vidx += vstride * 8;
        }
    }
}

fn loop_filter_mb(dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize, loop_str: u8, loop_params: &LoopParams, is_intra: bool) {
    let edge_thr    = i16::from(loop_str) + 2;
    let luma_thr    = i16::from(loop_str);
    let chroma_thr  = i16::from(loop_str) * 2;
    let inner_thr   = if loop_params.loop_sharpness == 0 {
            i16::from(loop_str)
        } else {
            let bound1 = i16::from(9 - loop_params.loop_sharpness);
            let shift = (loop_params.loop_sharpness + 3) >> 2;
            (i16::from(loop_str) >> shift).min(bound1)
        };
    let hev_thr     = i16::from(HIGH_EDGE_VAR_THR[if is_intra { 1 } else { 0 }][loop_str as usize]);

    let ystride = dframe.stride[0];
    let ustride = dframe.stride[1];
    let vstride = dframe.stride[2];
    let ypos = dframe.offset[0] + mb_x * 16 + mb_y * 16 * ystride;
    let upos = dframe.offset[1] + mb_x *  8 + mb_y *  8 * ustride;
    let vpos = dframe.offset[2] + mb_x *  8 + mb_y *  8 * vstride;

    let (loop_edge, loop_inner) = if loop_params.lf_simple {
            (simple_loop_filter as LoopFilterFunc, simple_loop_filter as LoopFilterFunc)
        } else {
            (normal_loop_filter_edge as LoopFilterFunc, normal_loop_filter_inner as LoopFilterFunc)
        };

    if mb_x > 0 {
        loop_edge(dframe.data, ypos, 1, ystride, 16, edge_thr, inner_thr, hev_thr);
        loop_edge(dframe.data, upos, 1, ustride,  8, edge_thr, inner_thr, hev_thr);
        loop_edge(dframe.data, vpos, 1, vstride,  8, edge_thr, inner_thr, hev_thr);
    }
    if mb_y > 0 {
        loop_edge(dframe.data, ypos, ystride, 1, 16, edge_thr, inner_thr, hev_thr);
        loop_edge(dframe.data, upos, ustride, 1,  8, edge_thr, inner_thr, hev_thr);
        loop_edge(dframe.data, vpos, vstride, 1,  8, edge_thr, inner_thr, hev_thr);
    }

    for y in 1..4 {
        loop_inner(dframe.data, ypos + y * 4 * ystride, ystride, 1, 16, luma_thr, inner_thr, hev_thr);
    }
    loop_inner(dframe.data, upos + 4 * ustride, ustride, 1, 8, chroma_thr, inner_thr, hev_thr);
    loop_inner(dframe.data, vpos + 4 * vstride, vstride, 1, 8, chroma_thr, inner_thr, hev_thr);

    for x in 1..4 {
        loop_inner(dframe.data, ypos + x * 4, 1, ystride, 16, luma_thr, inner_thr, hev_thr);
    }
    loop_inner(dframe.data, upos + 4, 1, ustride, 8, chroma_thr, inner_thr, hev_thr);
    loop_inner(dframe.data, vpos + 4, 1, vstride, 8, chroma_thr, inner_thr, hev_thr);
}
