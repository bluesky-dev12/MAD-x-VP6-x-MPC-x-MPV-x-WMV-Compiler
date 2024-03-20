use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::vpcommon::*;
use super::vp78::*;
use super::vp78data::*;
use super::vp78dsp::*;
use super::vp7data::*;
use super::vp7dsp::*;

const PITCH_MODE_NORMAL: u8 = 0;
const PITCH_MODE_FOUR:   u8 = 1;
const PITCH_MODE_X2:     u8 = 2;
const PITCH_MODE_X4:     u8 = 3;

#[derive(Clone,Copy,Default)]
struct MBFeature {
    present_prob:   u8,
    tree_probs:     [u8; 3],
    def_val:        [u8; 4],
}

struct SBParams<'a> {
    coef_probs: &'a [[[[u8; 11]; 3]; 8]; 4],
    scan:       &'a [usize; 16],
    qmat:       &'a [i16; 16],
}

fn decode_subblock(bc: &mut BoolCoder, coeffs: &mut [i16; 16], ctype: usize, pctx: u8, sbparams: &SBParams) -> u8 {
    let mut has_nz = 0;
    let start = if ctype != 0 { 0 } else { 1 };
    *coeffs = [0; 16];
    let mut cval = pctx as usize;
    for idx in start..16 {
        let probs = &sbparams.coef_probs[ctype][COEF_BANDS[idx]][cval];
        let tok                         = bc.read_tree(COEF_TREE, probs);
        if tok == DCTToken::EOB { break; }
        let level = expand_token(bc, tok);
        coeffs[sbparams.scan[idx]] = level.wrapping_mul(sbparams.qmat[idx]);
        cval = level.abs().min(2) as usize;
        has_nz |= cval;
    }
    if has_nz > 0 { 1 } else { 0 }
}

#[derive(Clone,Copy,Default)]
struct MBInfo {
    mb_type:    VPMBType,
    ymode:      PredMode,
    uvmode:     PredMode,
    loop_str:   u8,
    upd_gf:     bool,
}

#[derive(Default)]
struct DecoderState {
    features:           [Option<MBFeature>; 4],

    fading:             bool,
    fade_alpha:         u16,
    fade_beta:          u16,

    lf_simple:          bool,
    loop_filter_level:  u8,
    loop_sharpness:     u8,

    is_intra:           bool,
    version:            u8,

    kf_ymode_prob:      [u8; 4],
    kf_uvmode_prob:     [u8; 3],

    prob_intra_pred:    u8,
    prob_last_pred:     u8,

    coef_probs:         [[[[u8; 11]; 3]; 8]; 4],
    mv_probs:           [[u8; 17]; 2],

    force_quant:        Option<u8>,
    force_loop_str:     Option<u8>,
    force_gf_update:    bool,
    force_pitch:        Option<u8>,

    has_y2:             bool,
    pdc_pred_val:       [i16; 2],
    pdc_pred_count:     [usize; 2],

    ipred_ctx_y:        IPredContext,
    ipred_ctx_u:        IPredContext,
    ipred_ctx_v:        IPredContext,
}

impl DecoderState {
    fn reset(&mut self) {
        self.kf_ymode_prob.copy_from_slice(Y_MODE_TREE_PROBS);
        self.kf_uvmode_prob.copy_from_slice(UV_MODE_TREE_PROBS);
        self.coef_probs.copy_from_slice(&DEFAULT_DCT_PROBS);
        self.mv_probs.copy_from_slice(&DEFAULT_MV_PROBS);
    }
}

fn decode_mv_component(bc: &mut BoolCoder, probs: &[u8; 17]) -> i16 {
    let val = if !bc.read_prob(probs[0]) {
                                     bc.read_tree(SMALL_MV_TREE, &probs[2..9])
        } else {
            let raw_probs = &probs[9..];
            let mut raw = 0;
            for ord in LONG_VECTOR_ORDER.iter() {
                raw                  |= (bc.read_prob(raw_probs[*ord]) as i16) << *ord;
            }
            if (raw & 0xF0) != 0 {
                raw                  |= (bc.read_prob(raw_probs[3]) as i16) << 3;
            } else {
                raw |= 1 << 3;
            }
            raw
        };
    if (val == 0) || !bc.read_prob(probs[1]) {
        val
    } else {
        -val
    }
}

struct VP7Decoder {
    info:           NACodecInfoRef,

    shuf:           VPShuffler,
    width:          usize,
    height:         usize,
    mb_w:           usize,
    mb_h:           usize,
    mb_info:        Vec<MBInfo>,
    mvs:            Vec<MV>,
    mv_stride:      usize,

    ymodes:         Vec<PredMode>,
    ymode_stride:   usize,
    uvmodes:        Vec<PredMode>,
    uvmode_stride:  usize,

    dstate:         DecoderState,
    pcache:         PredCache,

    coeffs:         [[i16; 16]; 25],
    scan:           [usize; 16],
    qmat:           [[[i16; 16]; 3]; 5],

    mc_buf:         NAVideoBufferRef<u8>,

    tmp_scan:       [usize; 16],
}

impl VP7Decoder {
    fn new() -> Self {
        let vt = alloc_video_buffer(NAVideoInfo::new(128, 128, false, YUV420_FORMAT), 4).unwrap();
        let mut scan = [0; 16];
        scan.copy_from_slice(&DEFAULT_SCAN_ORDER);
        let mc_buf = vt.get_vbuf().unwrap();
        Self {
            info:           NACodecInfoRef::default(),

            shuf:           VPShuffler::new(),
            width:          0,
            height:         0,
            mb_w:           0,
            mb_h:           0,
            mb_info:        Vec::new(),
            mvs:            Vec::new(),
            mv_stride:      0,

            ymodes:         Vec::new(),
            ymode_stride:   0,
            uvmodes:        Vec::new(),
            uvmode_stride:  0,

            dstate:         DecoderState::default(),
            pcache:         PredCache::new(),

            coeffs:         [[0; 16]; 25],
            scan,
            tmp_scan:       [0; 16],
            qmat:           [[[0; 16]; 3]; 5],

            mc_buf,
        }
    }
    fn set_dimensions(&mut self, width: usize, height: usize) {
        if (width == self.width) && (height == self.height) {
            return;
        }
        self.width  = width;
        self.height = height;
        self.mb_w   = (self.width  + 15) >> 4;
        self.mb_h   = (self.height + 15) >> 4;
        self.mb_info.resize(self.mb_w * self.mb_h, MBInfo::default());
        self.mv_stride = self.mb_w * 4;
        self.mvs.resize(self.mv_stride * self.mb_h * 4, ZERO_MV);

        self.ymode_stride   = self.mb_w * 4;
        self.uvmode_stride  = self.mb_w;
        self.ymodes.resize(self.ymode_stride * self.mb_h * 4, PredMode::default());
        self.uvmodes.resize(self.uvmode_stride * self.mb_h, PredMode::default());

        self.pcache.resize(self.mb_w);
    }
    #[allow(clippy::field_reassign_with_default)]
    fn read_features(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        for (i, feat) in self.dstate.features.iter_mut().enumerate() {
            if bc.read_bool() {
                let mut feature = MBFeature::default();
                feature.present_prob        = bc.read_byte();
                for tp in feature.tree_probs.iter_mut() {
                    if bc.read_bool() {
                        *tp                 = bc.read_byte();
                    } else {
                        *tp = 255;
                    }
                }
                if i != 2 {
                    let fbits = match i {
                            0 => 7,
                            1 => 6,
                            _ => if self.dstate.version == 0 { 8 } else { 5 },
                        };
                    for dval in feature.def_val.iter_mut() {
                        if bc.read_bool() {
                            *dval                   = bc.read_bits(fbits) as u8;
                        } else {
                            *dval = 0;
                        }
                    }
                }
                *feat = Some(feature);
            } else {
                *feat = None;
            }
        }
        Ok(())
    }
    fn read_dct_coef_prob_upd(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        for i in 0..4 {
            for j in 0..8 {
                for k in 0..3 {
                    for l in 0..11 {
                        if bc.read_prob(DCT_UPDATE_PROBS[i][j][k][l]) {
                            self.dstate.coef_probs[i][j][k][l]  = bc.read_byte();
                        }
                    }
                }
            }
        }
        Ok(())
    }
    fn read_mv_prob_upd(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        for comp in 0..2 {
            for i in 0..17 {
                if bc.read_prob(MV_UPDATE_PROBS[comp][i]) {
                    self.dstate.mv_probs[comp][i]   = bc.read_probability();
                }
            }
        }
        Ok(())
    }
    fn decode_mb_features(&mut self, bc: &mut BoolCoder, _mb_x: usize, _mb_y: usize) -> DecoderResult<()> {
        self.dstate.force_quant        = None;
        self.dstate.force_loop_str     = None;
        self.dstate.force_gf_update    = false;
        self.dstate.force_pitch        = None;
        for (i, feat) in self.dstate.features.iter().enumerate() {
            if let Some(feat) = feat {
                let present             = bc.read_prob(feat.present_prob);
                if present {
                    let ftype_idx       = bc.read_tree(FEATURE_TREE, &feat.tree_probs);
                    let val = feat.def_val[ftype_idx];
                    match i {
                        0 => self.dstate.force_quant        = Some(ftype_idx as u8),
                        1 => self.dstate.force_loop_str     = Some(val),
                        2 => self.dstate.force_gf_update    = true,
                        _ => self.dstate.force_pitch        = Some(val),
                    };
                }
            }
        }
        Ok(())
    }
    fn decode_residue(&mut self, bc: &mut BoolCoder, mb_x: usize, mb_idx: usize, use_last: bool) {
        let qmat_idx = if let Some(idx) = self.dstate.force_quant { (idx as usize) + 1 } else { 0 };
        let mut sbparams = SBParams {
                scan:       &DEFAULT_SCAN_ORDER,
                qmat:       &self.qmat[qmat_idx][2],
                coef_probs: &self.dstate.coef_probs,
            };
        let mut has_ac = [false; 25];
        let ytype;
        if self.dstate.has_y2 {
            let pred = &self.pcache.y2_pred;
            let pidx = pred.xpos + mb_x;
            let pctx = self.pcache.y2_pred_left + pred.data[pidx - pred.stride];

            let has_nz = decode_subblock(bc, &mut self.coeffs[24], 1, pctx, &sbparams);
            self.pcache.y2_pred.data[pidx] = has_nz;
            self.pcache.y2_pred_left = has_nz;
            has_ac[24] = has_nz > 0;

            ytype = 0;
        } else {
            let pred = &mut self.pcache.y2_pred;
            let pidx = pred.xpos + mb_x;
            pred.data[pidx] = pred.data[pidx - pred.stride];

            ytype = 3;
        }
        sbparams.scan = &self.scan;
        sbparams.qmat = &self.qmat[qmat_idx][0];
        for i in 0..16 {
            let bx = i & 3;
            let by = i >> 2;
            let pred = &self.pcache.y_pred;
            let pidx = pred.xpos + mb_x * 4 + bx + by * pred.stride;
            let pctx = self.pcache.y_pred_left[by] + pred.data[pidx - pred.stride];

            let has_nz = decode_subblock(bc, &mut self.coeffs[i], ytype, pctx, &sbparams);
            self.pcache.y_pred.data[pidx] = has_nz;
            self.pcache.y_pred_left[by]   = has_nz;
            has_ac[i] = has_nz > 0;
        }
        sbparams.qmat = &self.qmat[qmat_idx][1];
        for i in 16..20 {
            let bx = i & 1;
            let by = (i >> 1) & 1;
            let pred = &self.pcache.u_pred;
            let pidx = pred.xpos + mb_x * 2 + bx + by * pred.stride;
            let pctx = self.pcache.u_pred_left[by] + pred.data[pidx - pred.stride];

            let has_nz = decode_subblock(bc, &mut self.coeffs[i], 2, pctx, &sbparams);
            self.pcache.u_pred.data[pidx] = has_nz;
            self.pcache.u_pred_left[by]   = has_nz;
            has_ac[i] = has_nz > 0;
        }
        for i in 20..24 {
            let bx = i & 1;
            let by = (i >> 1) & 1;
            let pred = &self.pcache.v_pred;
            let pidx = pred.xpos + mb_x * 2 + bx + by * pred.stride;
            let pctx = self.pcache.v_pred_left[by] + pred.data[pidx - pred.stride];

            let has_nz = decode_subblock(bc, &mut self.coeffs[i], 2, pctx, &sbparams);
            self.pcache.v_pred.data[pidx] = has_nz;
            self.pcache.v_pred_left[by]   = has_nz;
            has_ac[i] = has_nz > 0;
        }

        if self.dstate.has_y2 {
            let y2block = &mut self.coeffs[24];
            if self.mb_info[mb_idx].mb_type != VPMBType::Intra {
                let mut dc = y2block[0];
                let pdc_idx = if use_last { 0 } else { 1 };
                let pval = self.dstate.pdc_pred_val[pdc_idx];

                if self.dstate.pdc_pred_count[pdc_idx] > 3 {
                    dc += pval;
                    y2block[0] = dc;
                }
                if (pval == 0) || (dc == 0) || ((pval ^ dc) < 0) {
                    self.dstate.pdc_pred_count[pdc_idx]  = 0;
                } else if dc == pval {
                    self.dstate.pdc_pred_count[pdc_idx] += 1;
                }
                self.dstate.pdc_pred_val[pdc_idx] = dc;
            }
            if has_ac[24] {
                idct4x4(y2block);
            } else if y2block[0] != 0 {
                idct4x4_dc(y2block);
            }
            for i in 0..16 {
                self.coeffs[i][0] = self.coeffs[24][i];
            }
        }
        for i in 0..24 {
            if has_ac[i] {
                idct4x4(&mut self.coeffs[i]);
            } else if self.coeffs[i][0] != 0 {
                idct4x4_dc(&mut self.coeffs[i]);
            }
        }
    }

    fn set_qmat(&mut self, y_dc_q: usize, y_ac_q: usize, y2_dc_q: usize, y2_ac_q: usize, uv_dc_q: usize, uv_ac_q: usize) {
        self.qmat[0][0][0] = Y_DC_QUANTS[y_dc_q];
        for i in 1..16 {
            self.qmat[0][0][i] = Y_AC_QUANTS[y_ac_q];
        }
        self.qmat[0][1][0] = UV_DC_QUANTS[uv_dc_q];
        for i in 1..16 {
            self.qmat[0][1][i] = UV_AC_QUANTS[uv_ac_q];
        }
        self.qmat[0][2][0] = Y2_DC_QUANTS[y2_dc_q];
        for i in 1..16 {
            self.qmat[0][2][i] = Y2_AC_QUANTS[y2_ac_q];
        }
        if let Some(ref feat) = self.dstate.features[0] {
            for j in 0..4 {
                let q = feat.def_val[j] as usize;
                self.qmat[j + 1][0][0] = Y_DC_QUANTS[q];
                for i in 1..16 {
                    self.qmat[j + 1][0][i] = Y_AC_QUANTS[q];
                }
                self.qmat[j + 1][1][0] = UV_DC_QUANTS[q];
                for i in 1..16 {
                    self.qmat[j + 1][1][i] = UV_AC_QUANTS[q];
                }
                self.qmat[j + 1][2][0] = Y2_DC_QUANTS[q];
                for i in 1..16 {
                    self.qmat[j + 1][2][i] = Y2_AC_QUANTS[q];
                }
            }
        }
    }
    fn fill_ymode(&mut self, mb_x: usize, mb_y: usize, ymode: PredMode) {
        let mut iidx = mb_x * 4 + mb_y * 4 * self.ymode_stride;
        for _ in 0..4 {
            for x in 0..4 {
                self.ymodes[iidx + x] = ymode;
            }
            iidx += self.ymode_stride;
        }
    }
    fn fill_mv(&mut self, mb_x: usize, mb_y: usize, mv: MV) {
        let mut iidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        for _ in 0..4 {
            for x in 0..4 {
                self.mvs[iidx + x] = mv;
            }
            iidx += self.mb_w * 4;
        }
    }
    fn find_mv_pred(&self, mb_x: usize, mb_y: usize) -> ([u8; 4], MV, MV, MV) {
        let mut nearest_mv = ZERO_MV;
        let mut near_mv = ZERO_MV;

        let mut ct: [u8; 4] = [0; 4];

        let start = if self.dstate.version == 0 { 1 } else { 0 };
        let mvwrap = (self.mb_w as isize) + 1;
        for (yoff, xoff, weight, blk_no) in CAND_POS.iter() {
            let cx = (mb_x as isize) + (*xoff as isize);
            let cy = (mb_y as isize) + (*yoff as isize);
            let mvpos = cx + cy * mvwrap;
            if (mvpos < start) || ((mvpos % mvwrap) == (mvwrap - 1)) {
                ct[0] += weight;
                continue;
            }
            let cx = (mvpos % mvwrap) as usize;
            let cy = (mvpos / mvwrap) as usize;
            let bx = (*blk_no as usize) & 3;
            let by = (*blk_no as usize) >> 2;
            let blk_pos = cx * 4 + bx + (cy * 4 + by) * self.mv_stride;
            let mv = self.mvs[blk_pos];
            if mv == ZERO_MV {
                ct[0] += weight;
                continue;
            }
            let idx;
            if (nearest_mv == ZERO_MV) || (nearest_mv == mv) {
                nearest_mv = mv;
                idx = 1;
            } else if near_mv == ZERO_MV {
                near_mv = mv;
                idx = 2;
            } else {
                idx = if mv == near_mv { 2 } else { 3 };
            }
            ct[idx] += weight;
        }
        let pred_mv = if ct[1] > ct[2] {
                if ct[1] >= ct[0] { nearest_mv } else { ZERO_MV }
            } else {
                if ct[2] >= ct[0] { near_mv } else { ZERO_MV }
            };

        let mvprobs = [INTER_MODE_PROBS[ct[0] as usize][0],
                       INTER_MODE_PROBS[ct[1] as usize][1],
                       INTER_MODE_PROBS[ct[2] as usize][2],
                       INTER_MODE_PROBS[ct[2] as usize][3]];

        (mvprobs, nearest_mv, near_mv, pred_mv)
    }
    fn get_split_mv(&self, bc: &mut BoolCoder, mb_x: usize, mb_y: usize, bx: usize, by: usize, pred_mv: MV) -> MV {
        let mode                        = bc.read_tree(SUB_MV_REF_TREE, &SUB_MV_REF_PROBS);
        let mvidx = mb_x * 4 + bx + (mb_y * 4 + by) * self.mv_stride;
        match mode {
            SubMVRef::Left => {
                if (mb_x > 0) || (bx > 0) {
                    self.mvs[mvidx - 1]
                } else {
                    ZERO_MV
                }
            },
            SubMVRef::Above => {
                if (mb_y > 0) || (by > 0) {
                    self.mvs[mvidx - self.mv_stride]
                } else {
                    ZERO_MV
                }
            },
            SubMVRef::Zero => ZERO_MV,
            SubMVRef::New => {
                let dmy = decode_mv_component(bc, &self.dstate.mv_probs[0]);
                let dmx = decode_mv_component(bc, &self.dstate.mv_probs[1]);
                pred_mv + MV{ x: dmx, y: dmy }
            },
        }
    }
    fn do_split_mv(&mut self, bc: &mut BoolCoder, mb_x: usize, mb_y: usize, pred_mv: MV) -> DecoderResult<()> {
        let split_mode                  = bc.read_tree(MV_SPLIT_MODE_TREE, &MV_SPLIT_MODE_PROBS);
        let mut mvidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        match split_mode {
            MVSplitMode::TopBottom => {
                let top_mv = self.get_split_mv(bc, mb_x, mb_y, 0, 0, pred_mv);
                for _ in 0..2 {
                    for x in 0..4 { self.mvs[mvidx + x] = top_mv; }
                    mvidx += self.mv_stride;
                }
                let bot_mv = self.get_split_mv(bc, mb_x, mb_y, 0, 2, pred_mv);
                for _ in 2..4 {
                    for x in 0..4 { self.mvs[mvidx + x] = bot_mv; }
                    mvidx += self.mv_stride;
                }
            },
            MVSplitMode::LeftRight => {
                let left_mv  = self.get_split_mv(bc, mb_x, mb_y, 0, 0, pred_mv);
                self.mvs[mvidx + 1] = left_mv;
                let right_mv = self.get_split_mv(bc, mb_x, mb_y, 2, 0, pred_mv);
                for _ in 0..4 {
                    self.mvs[mvidx + 0] = left_mv;
                    self.mvs[mvidx + 1] = left_mv;
                    self.mvs[mvidx + 2] = right_mv;
                    self.mvs[mvidx + 3] = right_mv;
                    mvidx += self.mv_stride;
                }
            },
            MVSplitMode::Quarters => {
                for y in (0..4).step_by(2) {
                    for x in (0..4).step_by(2) {
                        self.mvs[mvidx + x] = self.get_split_mv(bc, mb_x, mb_y, x, y, pred_mv);
                        self.mvs[mvidx + x + 1] = self.mvs[mvidx + x];
                    }
                    for x in 0..4 {
                        self.mvs[mvidx + x + self.mv_stride] = self.mvs[mvidx + x];
                    }
                    mvidx += self.mv_stride * 2;
                }
            },
            MVSplitMode::Sixteenths => {
                for y in 0..4 {
                    for x in 0..4 {
                        self.mvs[mvidx + x] = self.get_split_mv(bc, mb_x, mb_y, x, y, pred_mv);
                    }
                    mvidx += self.mv_stride;
                }
            },
        };
        Ok(())
    }

    fn add_residue(&self, dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize, do_luma: bool, pitch_mode: u8) {
        if do_luma {
            let ydst = &mut dframe.data[dframe.offset[0]..];
            let ystride = dframe.stride[0];
            let mut yoff = mb_x * 16 + mb_y * 16 * ystride;
            match pitch_mode {
                PITCH_MODE_NORMAL => {
                    for y in 0..4 {
                        for x in 0..4 {
                            add_coeffs4x4(ydst, yoff + x * 4, ystride, &self.coeffs[x + y * 4]);
                        }
                        yoff += 4 * ystride;
                    }
                },
                PITCH_MODE_FOUR => {
                    for y in 0..16 {
                        add_coeffs16x1(ydst, yoff, &self.coeffs[y]);
                        yoff += ystride;
                    }
                },
                PITCH_MODE_X2 => {
                    for y in 0..2 {
                        for x in 0..4 {
                            add_coeffs4x4(ydst, yoff + x * 4, ystride * 2, &self.coeffs[x + y * 4]);
                        }
                        yoff += 8 * ystride;
                    }
                    yoff -= 15 * ystride;
                    for y in 2..4 {
                        for x in 0..4 {
                            add_coeffs4x4(ydst, yoff + x * 4, ystride * 2, &self.coeffs[x + y * 4]);
                        }
                        yoff += 8 * ystride;
                    }
                },
                PITCH_MODE_X4 => {
                    for y in 0..4 {
                        for x in 0..4 {
                            add_coeffs4x4(ydst, yoff + x * 4, ystride * 4, &self.coeffs[x + y * 4]);
                        }
                        yoff += ystride;
                    }
                },
                _ => unreachable!(),
            };
        }
        let dst = &mut dframe.data[0..];
        let mut uoff = dframe.offset[1] + mb_x * 8 + mb_y * 8 * dframe.stride[1];
        let ustride = dframe.stride[1];
        let mut voff = dframe.offset[2] + mb_x * 8 + mb_y * 8 * dframe.stride[2];
        let vstride = dframe.stride[2];
        if (pitch_mode == PITCH_MODE_NORMAL) || (pitch_mode == PITCH_MODE_FOUR) {
            for y in 0..2 {
                for x in 0..2 {
                    add_coeffs4x4(dst, uoff + x * 4, ustride, &self.coeffs[16 + x + y * 2]);
                    add_coeffs4x4(dst, voff + x * 4, vstride, &self.coeffs[20 + x + y * 2]);
                }
                uoff += ustride * 4;
                voff += vstride * 4;
            }
        } else {
            for y in 0..2 {
                for x in 0..2 {
                    add_coeffs4x4(dst, uoff + x * 4, ustride * 2, &self.coeffs[16 + x + y * 2]);
                    add_coeffs4x4(dst, voff + x * 4, vstride * 2, &self.coeffs[20 + x + y * 2]);
                }
                uoff += ustride;
                voff += vstride;
            }
        }
    }
    fn recon_intra_mb(&mut self, dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize) -> DecoderResult<()> {
        let pitch = self.dstate.force_pitch.unwrap_or(0);
        let pitch_mode = (pitch >> 3) & 3;

        let mb_idx = mb_x + mb_y * self.mb_w;
        let has_top = mb_y > 0;
        let has_left = mb_x > 0;
        let ydst = &mut dframe.data[dframe.offset[0]..];
        let ystride = dframe.stride[0];
        let mut yoff = mb_x * 16 + mb_y * 16 * ystride;
        let ipred_ctx_y = &mut self.dstate.ipred_ctx_y;
        ipred_ctx_y.has_top  = has_top;
        ipred_ctx_y.has_left = has_left;
        let is_normal = self.mb_info[mb_idx].ymode != PredMode::BPred;
        if is_normal {
            ipred_ctx_y.fill(ydst, yoff, ystride, 16, 16);
            match self.mb_info[mb_idx].ymode {
                PredMode::DCPred => IPred16x16::ipred_dc(ydst, yoff, ystride, ipred_ctx_y),
                PredMode::HPred  => IPred16x16::ipred_h (ydst, yoff, ystride, ipred_ctx_y),
                PredMode::VPred  => IPred16x16::ipred_v (ydst, yoff, ystride, ipred_ctx_y),
                PredMode::TMPred => IPred16x16::ipred_tm(ydst, yoff, ystride, ipred_ctx_y),
                _ => unreachable!(),
            };
        } else {
            validate!((pitch_mode == PITCH_MODE_NORMAL) || (pitch_mode == PITCH_MODE_X2));
            let mut iidx = mb_x * 4 + mb_y * 4 * self.ymode_stride;
            let mut tr_save = [0x80u8; 16];
            if pitch_mode == PITCH_MODE_X2 {
                // reorganise coefficient data for interlaced case
                for y in (0..4).step_by(2) {
                    for x in 0..4 {
                        let mut tmpblock = [0i16; 16 * 2];
                        let eidx = x + y * 4;
                        let oidx = x + y * 4 + 4;
                        for i in 0..4 {
                            for j in 0..4 {
                                tmpblock[i * 8 + 0 + j] = self.coeffs[eidx][i * 4 + j];
                                tmpblock[i * 8 + 4 + j] = self.coeffs[oidx][i * 4 + j];
                            }
                        }
                        self.coeffs[eidx].copy_from_slice(&tmpblock[0..16]);
                        self.coeffs[oidx].copy_from_slice(&tmpblock[16..32]);
                    }
                }
            }
            let tr_edge = if has_top { ydst[yoff - ystride + 15] } else { 0x80 };
            for y in 0..4 {
                for x in 0..4 {
                    ipred_ctx_y.has_left = has_left || x > 0;
                    let bmode = self.ymodes[iidx + x];
                    let cur_yoff = yoff + x * 4;
                    let has_tr = ipred_ctx_y.has_top && ((x < 3) || ((y == 0) && (mb_y < self.mb_w - 1)));
                    let has_dl = ipred_ctx_y.has_left && (x == 0) && (y < 3);
                    ipred_ctx_y.fill(ydst, cur_yoff, ystride,
                                     if has_tr { 8 } else { 4 },
                                     if has_dl { 8 } else { 4 });
                    if !has_tr {
                        for i in 0..4 {
                            ipred_ctx_y.top[i + 4] = tr_save[x * 4 + i];
                        }
                    } else {
                        for i in 0..4 {
                            tr_save[x * 4 + i] = ipred_ctx_y.top[i + 4];
                        }
                    }
                    if (mb_x == self.mb_w - 1) && has_top && (x == 3) {
                        for i in 0..4 {
                            ipred_ctx_y.top[i + 4] = tr_edge;
                        }
                    }
                    match bmode {
                        PredMode::DCPred => IPred4x4::ipred_dc(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::TMPred => IPred4x4::ipred_tm(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::HPred  => IPred4x4::ipred_he(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::VPred  => IPred4x4::ipred_ve(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::LDPred => IPred4x4::ipred_ld(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::RDPred => IPred4x4::ipred_rd(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::VRPred => IPred4x4::ipred_vr(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::VLPred => IPred4x4::ipred_vl(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::HDPred => IPred4x4::ipred_hd(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::HUPred => IPred4x4::ipred_hu(ydst, cur_yoff, ystride, ipred_ctx_y),
                        _ => unreachable!(),
                    };
                    add_coeffs4x4(ydst, cur_yoff, ystride, &self.coeffs[x + y * 4]);
                }
                ipred_ctx_y.has_top = true;
                yoff += 4 * ystride;
                iidx += self.ymode_stride;
            }
        }
        let dst = &mut dframe.data[0..];
        let uoff = dframe.offset[1] + mb_x * 8 + mb_y * 8 * dframe.stride[1];
        let ustride = dframe.stride[1];
        let voff = dframe.offset[2] + mb_x * 8 + mb_y * 8 * dframe.stride[2];
        let vstride = dframe.stride[2];
        let ipred_ctx_u = &mut self.dstate.ipred_ctx_u;
        let ipred_ctx_v = &mut self.dstate.ipred_ctx_v;
        ipred_ctx_u.has_top  = has_top;
        ipred_ctx_v.has_top  = has_top;
        ipred_ctx_u.has_left = has_left;
        ipred_ctx_v.has_left = has_left;
        ipred_ctx_u.fill(dst, uoff, ustride, 8, 8);
        ipred_ctx_v.fill(dst, voff, vstride, 8, 8);
        match self.mb_info[mb_idx].uvmode {
            PredMode::DCPred => {
                IPred8x8::ipred_dc(dst, uoff, ustride, ipred_ctx_u);
                IPred8x8::ipred_dc(dst, voff, vstride, ipred_ctx_v);
            },
            PredMode::HPred => {
                IPred8x8::ipred_h(dst, uoff, ustride, ipred_ctx_u);
                IPred8x8::ipred_h(dst, voff, vstride, ipred_ctx_v);
            },
            PredMode::VPred => {
                IPred8x8::ipred_v(dst, uoff, ustride, ipred_ctx_u);
                IPred8x8::ipred_v(dst, voff, vstride, ipred_ctx_v);
            },
            PredMode::TMPred => {
                IPred8x8::ipred_tm(dst, uoff, ustride, ipred_ctx_u);
                IPred8x8::ipred_tm(dst, voff, vstride, ipred_ctx_v);
            },
            _ => unreachable!(),
        };
        self.add_residue(dframe, mb_x, mb_y, is_normal, pitch_mode);
        Ok(())
    }
    fn recon_inter_mb(&mut self, dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize, use_last: bool) {
        let pitch = self.dstate.force_pitch.unwrap_or(0);
        let pitch_dmode = (pitch >> 3) & 3;
        let pitch_smode = pitch & 7;

        let refframe = (if use_last { self.shuf.get_last() } else { self.shuf.get_golden() }).unwrap();
        let single_mv = self.mb_info[mb_x + mb_y * self.mb_w].mb_type != VPMBType::InterFourMV;
        let mut iidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        let mc_buf = self.mc_buf.get_data_mut().unwrap();

        let dst = &mut dframe.data[0..];
        let ystride = dframe.stride[0];
        let mut yoff = dframe.offset[0] + mb_x * 16 + mb_y * 16 * ystride;
        if pitch_smode == 0 {
            if single_mv {
                mc_block16x16(dst, yoff, ystride, mb_x * 16, mb_y * 16,
                              self.mvs[iidx].x * 2, self.mvs[iidx].y * 2, refframe.clone(), 0, mc_buf);
            } else {
                for y in 0..4 {
                    for x in 0..4 {
                        mc_block4x4(dst, yoff + x * 4, ystride, mb_x * 16 + x * 4, mb_y * 16 + y * 4,
                                    self.mvs[iidx + x].x * 2, self.mvs[iidx + x].y * 2, refframe.clone(), 0, mc_buf);
                    }
                    yoff += 4 * ystride;
                    iidx += self.mv_stride;
                }
            }
        } else {
            if single_mv {
                mc_block_special(dst, yoff, ystride, mb_x * 16, mb_y * 16,
                                 self.mvs[iidx].x * 2, self.mvs[iidx].y * 2,
                                 refframe.clone(), 0, mc_buf, 16, pitch_smode);
            } else {
                for y in 0..4 {
                    for x in 0..4 {
                        mc_block_special(dst, yoff + x * 4, ystride,
                                         mb_x * 16 + x * 4, mb_y * 16 + y * 4,
                                         self.mvs[iidx + x].x * 2, self.mvs[iidx + x].y * 2,
                                         refframe.clone(), 0, mc_buf, 4, pitch_smode);
                    }
                    yoff += 4 * ystride;
                    iidx += self.mv_stride;
                }
            }
        }

        let mut iidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        let mut uoff = dframe.offset[1] + mb_x * 8 + mb_y * 8 * dframe.stride[1];
        let ustride = dframe.stride[1];
        let mut voff = dframe.offset[2] + mb_x * 8 + mb_y * 8 * dframe.stride[2];
        let vstride = dframe.stride[2];
        if single_mv {
            let chroma_mv = self.mvs[iidx];

            if pitch_smode == 0 {
                mc_block8x8(dst, uoff, ustride, mb_x * 8, mb_y * 8, chroma_mv.x, chroma_mv.y, refframe.clone(), 1, mc_buf);
                mc_block8x8(dst, voff, vstride, mb_x * 8, mb_y * 8, chroma_mv.x, chroma_mv.y, refframe,         2, mc_buf);
            } else {
                mc_block_special(dst, uoff, ustride, mb_x * 8, mb_y * 8, chroma_mv.x, chroma_mv.y,
                                 refframe.clone(), 1, mc_buf, 8, pitch_smode);
                mc_block_special(dst, voff, vstride, mb_x * 8, mb_y * 8, chroma_mv.x, chroma_mv.y,
                                 refframe,         2, mc_buf, 8, pitch_smode);
            }
        } else {
            for y in 0..2 {
                for x in 0..2 {
                    let mut chroma_mv = self.mvs[iidx + x * 2] + self.mvs[iidx + x * 2 + 1]
                                       + self.mvs[iidx + x * 2 + self.mv_stride]
                                       + self.mvs[iidx + x * 2 + self.mv_stride + 1];
                    if chroma_mv.x < 0 {
                        chroma_mv.x += 1;
                    } else {
                        chroma_mv.x += 2;
                    }
                    if chroma_mv.y < 0 {
                        chroma_mv.y += 1;
                    } else {
                        chroma_mv.y += 2;
                    }
                    chroma_mv.x >>= 2;
                    chroma_mv.y >>= 2;

                    if pitch_smode == 0 {
                        mc_block4x4(dst, uoff + x * 4, ustride, mb_x * 8 + x * 4, mb_y * 8 + y * 4,
                                    chroma_mv.x, chroma_mv.y, refframe.clone(), 1, mc_buf);
                        mc_block4x4(dst, voff + x * 4, vstride, mb_x * 8 + x * 4, mb_y * 8 + y * 4,
                                    chroma_mv.x, chroma_mv.y, refframe.clone(), 2, mc_buf);
                    } else {
                        mc_block_special(dst, uoff + x * 4, ustride, mb_x * 8 + x * 4, mb_y * 8 + y * 4,
                                         chroma_mv.x, chroma_mv.y, refframe.clone(), 1, mc_buf,
                                         4, pitch_smode);
                        mc_block_special(dst, voff + x * 4, vstride, mb_x * 8 + x * 4, mb_y * 8 + y * 4,
                                         chroma_mv.x, chroma_mv.y, refframe.clone(), 2, mc_buf,
                                         4, pitch_smode);
                    }
                }
                uoff += ustride * 4;
                voff += vstride * 4;
                iidx += 2 * self.mv_stride;
            }
        }
        self.add_residue(dframe, mb_x, mb_y, true, pitch_dmode);
    }
    fn loop_filter_mb(&mut self, dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize, loop_str: u8) {
        let edge_thr    = i16::from(loop_str) + 2;
        let luma_thr    = i16::from(loop_str);
        let chroma_thr  = i16::from(loop_str) * 2;
        let inner_thr   = if self.dstate.loop_sharpness == 0 {
                i16::from(loop_str)
            } else {
                let bound1 = i16::from(9 - self.dstate.loop_sharpness);
                let shift = (self.dstate.loop_sharpness + 3) >> 2;
                (i16::from(loop_str) >> shift).min(bound1)
            };
        let hev_thr     = i16::from(HIGH_EDGE_VAR_THR[if self.dstate.is_intra { 1 } else { 0 }][loop_str as usize]);

        let ystride = dframe.stride[0];
        let ustride = dframe.stride[1];
        let vstride = dframe.stride[2];
        let ypos = dframe.offset[0] + mb_x * 16 + mb_y * 16 * ystride;
        let upos = dframe.offset[1] + mb_x *  8 + mb_y *  8 * ustride;
        let vpos = dframe.offset[2] + mb_x *  8 + mb_y *  8 * vstride;

        let (loop_edge, loop_inner) = if self.dstate.lf_simple {
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
}

impl NADecoder for VP7Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = YUV420_FORMAT;
            let myvinfo = NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, fmt);
            let myinfo = NACodecTypeInfo::Video(myvinfo);
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            supp.pool_u8.set_dec_bufs(4);
            supp.pool_u8.prealloc_video(NAVideoInfo::new(myvinfo.get_width(), myvinfo.get_height(), false, vinfo.get_format()), 4)?;
            self.set_dimensions(myvinfo.get_width(), myvinfo.get_height());
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        validate!(src.len() > 4);

        let frame_tag = read_u24le(src.as_slice())?;
        self.dstate.is_intra    = (frame_tag & 1) == 0;
        self.dstate.version     = ((frame_tag >> 1) & 7) as u8;
        let part2_off           = (frame_tag >> 4) as usize;
        let part1_off           = if self.dstate.version == 0 { 4 } else { 3 };

        validate!(src.len() > part1_off + part2_off);
        let mut bc      = BoolCoder::new(&src[part1_off..][..part2_off])?;
        let mut bc_main = BoolCoder::new(&src[part1_off + part2_off..])?;
        if self.dstate.is_intra {
            let width                   = bc.read_bits(12) as usize;
            let height                  = bc.read_bits(12) as usize;
            let _scalev                 = bc.read_bits(2);
            let _scaleh                 = bc.read_bits(2);
            validate!((width > 0) && (height > 0));
            self.set_dimensions(width, height);

            self.dstate.reset();
            self.scan.copy_from_slice(&DEFAULT_SCAN_ORDER);
        } else {
            if !self.shuf.has_refs() {
                return Err(DecoderError::MissingReference);
            }
        }

        self.read_features(&mut bc)?;

        let y_ac_q                      = bc.read_bits(7) as usize;
        let y_dc_q                      = if bc.read_bool() { bc.read_bits(7) as usize } else { y_ac_q };
        let y2_dc_q                     = if bc.read_bool() { bc.read_bits(7) as usize } else { y_ac_q };
        let y2_ac_q                     = if bc.read_bool() { bc.read_bits(7) as usize } else { y_ac_q };
        let uv_dc_q                     = if bc.read_bool() { bc.read_bits(7) as usize } else { y_ac_q };
        let uv_ac_q                     = if bc.read_bool() { bc.read_bits(7) as usize } else { y_ac_q };
        self.set_qmat(y_dc_q, y_ac_q, y2_dc_q, y2_ac_q, uv_dc_q, uv_ac_q);

        let update_gf                   = if self.dstate.is_intra { true } else { bc.read_bool() };

        let mut has_fading_feature = true;
        let mut keep_probs = true;
        if self.dstate.version != 0 {
            keep_probs = bc.read_bool();
            if self.dstate.is_intra {
                has_fading_feature = true;
            } else {
                has_fading_feature = bc.read_bool();
            }
        }

        if has_fading_feature {
            self.dstate.fading          = bc.read_bool();
            if self.dstate.fading {
                self.dstate.fade_alpha  = bc.read_sbits(8) as u16;
                self.dstate.fade_beta   = bc.read_sbits(8) as u16;
                if let Some(pframe) = self.shuf.get_last() {
                    let mut fframe = supp.pool_u8.get_free().unwrap();
                    let mut dframe = NASimpleVideoFrame::from_video_buf(&mut fframe).unwrap();
                    fade_frame(pframe, &mut dframe, self.dstate.fade_alpha, self.dstate.fade_beta);
                    self.shuf.add_frame(fframe);
                }
            }
        } else {
            self.dstate.fading = false;
        }

        if self.dstate.version == 0 {
            self.dstate.lf_simple       = bc.read_bool();
        }

        if bc.read_bool() {
            for i in 1..16 {
                self.scan[i]            = DEFAULT_SCAN_ORDER[bc.read_bits(4) as usize];
            }
        }

        if self.dstate.version != 0 {
            self.dstate.lf_simple       = bc.read_bool();
        } else {
            self.dstate.lf_simple = false;
        }

        self.dstate.loop_filter_level   = bc.read_bits(6) as u8;
        self.dstate.loop_sharpness      = bc.read_bits(3) as u8;

        self.read_dct_coef_prob_upd(&mut bc)?;

        if !self.dstate.is_intra {
            self.dstate.prob_intra_pred             = bc.read_byte();
            self.dstate.prob_last_pred              = bc.read_byte();
            if bc.read_bool() {
                for i in 0..4 {
                    self.dstate.kf_ymode_prob[i]    = bc.read_byte();
                }
            }
            if bc.read_bool() {
                for i in 0..3 {
                    self.dstate.kf_uvmode_prob[i]   = bc.read_byte();
                }
            }
            self.read_mv_prob_upd(&mut bc)?;
        }
        if !keep_probs {
            self.tmp_scan.copy_from_slice(&self.scan);
        }

        let vinfo = NAVideoInfo::new(self.width, self.height, false, YUV420_FORMAT);
        let ret = supp.pool_u8.get_free();
        if ret.is_none() {
            return Err(DecoderError::AllocError);
        }
        let mut buf = ret.unwrap();
        if buf.get_info() != vinfo {
            self.shuf.clear();
            supp.pool_u8.reset();
            supp.pool_u8.prealloc_video(vinfo, 4)?;
            let ret = supp.pool_u8.get_free();
            if ret.is_none() {
                return Err(DecoderError::AllocError);
            }
            buf = ret.unwrap();
        }
        let mut dframe = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();

        let mut mb_idx = 0;
        self.pcache.reset();
        if self.dstate.is_intra || (self.dstate.version > 0) {
            self.dstate.pdc_pred_val    = [0; 2];
            self.dstate.pdc_pred_count  = [0; 2];
        }
        let mut use_last = true;
        for mb_y in 0..self.mb_h {
            for mb_x in 0..self.mb_w {
                self.decode_mb_features(&mut bc, mb_x, mb_y)?;
                self.dstate.has_y2 = true;
                if self.dstate.is_intra {
                    let ymode           = bc.read_tree(KF_Y_MODE_TREE, KF_Y_MODE_TREE_PROBS);
                    if ymode == PredMode::BPred {
                        self.dstate.has_y2 = false;
                        let mut iidx = mb_x * 4 + mb_y * 4 * self.ymode_stride;
                        for y in 0..4 {
                            for x in 0..4 {
                                let top_mode = if (y > 0) || (mb_y > 0) {
                                        self.ymodes[iidx + x - self.ymode_stride]
                                    } else {
                                        PredMode::DCPred
                                    };
                                let left_mode = if (x > 0) || (mb_x > 0) {
                                        self.ymodes[iidx + x - 1]
                                    } else {
                                        PredMode::DCPred
                                    };
                                let top_idx  = top_mode.to_b_index();
                                let left_idx = left_mode.to_b_index();
                                let bmode = bc.read_tree(B_MODE_TREE, &KF_B_MODE_TREE_PROBS[top_idx][left_idx]);
                                self.ymodes[iidx + x] = bmode;
                            }
                            iidx += self.ymode_stride;
                        }
                    } else {
                        self.fill_ymode(mb_x, mb_y, ymode.to_b_mode());
                    }
                    let uvmode          = bc.read_tree(UV_MODE_TREE, KF_UV_MODE_TREE_PROBS);
                    self.mb_info[mb_idx].mb_type    = VPMBType::Intra;
                    self.mb_info[mb_idx].ymode      = ymode;
                    self.mb_info[mb_idx].uvmode     = uvmode;
                } else if !bc.read_prob(self.dstate.prob_intra_pred) {
                    let ymode           = bc.read_tree(Y_MODE_TREE, &self.dstate.kf_ymode_prob);
                    if ymode == PredMode::BPred {
                        self.dstate.has_y2 = false;
                        let mut iidx = mb_x * 4 + mb_y * 4 * self.ymode_stride;
                        for _y in 0..4 {
                            for x in 0..4 {
                                let bmode = bc.read_tree(B_MODE_TREE, B_MODE_TREE_PROBS);
                                self.ymodes[iidx + x] = bmode;
                            }
                            iidx += self.ymode_stride;
                        }
                    } else {
                        self.fill_ymode(mb_x, mb_y, PredMode::Inter);
                    }
                    let uvmode          = bc.read_tree(UV_MODE_TREE, &self.dstate.kf_uvmode_prob);
                    self.mb_info[mb_idx].mb_type    = VPMBType::Intra;
                    self.mb_info[mb_idx].ymode      = ymode;
                    self.mb_info[mb_idx].uvmode     = uvmode;
                    self.fill_mv(mb_x, mb_y, ZERO_MV);
                } else {
                    use_last            = !bc.read_prob(self.dstate.prob_last_pred);

                    let (mvprobs, nearest_mv, near_mv, pred_mv) = self.find_mv_pred(mb_x, mb_y);
                    let mbtype          = bc.read_tree(MV_REF_TREE, &mvprobs);

                    match mbtype {
                        VPMBType::InterNearest => {
                            self.fill_mv(mb_x, mb_y, nearest_mv);
                        },
                        VPMBType::InterNear => {
                            self.fill_mv(mb_x, mb_y, near_mv);
                        },
                        VPMBType::InterNoMV => {
                            self.fill_mv(mb_x, mb_y, ZERO_MV);
                        },
                        VPMBType::InterMV => {
                            let dmy     = decode_mv_component(&mut bc, &self.dstate.mv_probs[0]);
                            let dmx     = decode_mv_component(&mut bc, &self.dstate.mv_probs[1]);
                            let new_mv = pred_mv + MV{ x: dmx, y: dmy };
                            self.fill_mv(mb_x, mb_y, new_mv);
                        },
                        VPMBType::InterFourMV => {
                            self.do_split_mv(&mut bc, mb_x, mb_y, pred_mv)?;
                        },
                        _ => unreachable!(),
                    };

                    self.fill_ymode(mb_x, mb_y, PredMode::Inter);
                    self.mb_info[mb_idx].mb_type    = mbtype;
                    self.mb_info[mb_idx].ymode      = PredMode::Inter;
                    self.mb_info[mb_idx].uvmode     = PredMode::Inter;
                }
                self.decode_residue(&mut bc_main, mb_x, mb_idx, use_last);
                match self.mb_info[mb_idx].mb_type {
                    VPMBType::Intra => {
                        self.recon_intra_mb(&mut dframe, mb_x, mb_y)?;
                    },
                    _ => {
                        self.recon_inter_mb(&mut dframe, mb_x, mb_y, use_last);
                    },
                }
                if let Some(loop_str) = self.dstate.force_loop_str {
                    self.mb_info[mb_idx].loop_str = loop_str;
                } else {
                    self.mb_info[mb_idx].loop_str = self.dstate.loop_filter_level;
                }
                self.mb_info[mb_idx].upd_gf = self.dstate.force_gf_update;
                mb_idx += 1;
            }
            self.pcache.update_row();
        }
        let mut mb_idx = 0;
        for mb_y in 0..self.mb_h {
            for mb_x in 0..self.mb_w {
                let loop_str = self.mb_info[mb_idx].loop_str;
                self.loop_filter_mb(&mut dframe, mb_x, mb_y, loop_str);
                mb_idx += 1;
            }
        }
        if !update_gf && self.dstate.features[2].is_some() {
            let gf = self.shuf.get_golden().unwrap();
            let mut new_gf = supp.pool_u8.get_copy(&gf).unwrap();
            let dframe = NASimpleVideoFrame::from_video_buf(&mut new_gf).unwrap();
            let mut mb_idx = 0;
            let mc_buf = self.mc_buf.get_data_mut().unwrap();
            for mb_y in 0..self.mb_h {
                for mb_x in 0..self.mb_w {
                    if self.mb_info[mb_idx].upd_gf {
                        mc_block16x16(dframe.data, dframe.offset[0] + mb_x * 16 + mb_y * 16 * dframe.stride[0], dframe.stride[0], mb_x * 16, mb_y * 16, 0, 0, buf.clone(), 0, mc_buf);
                        mc_block8x8(dframe.data, dframe.offset[1] + mb_x * 8 + mb_y * 8 * dframe.stride[1], dframe.stride[1], mb_x * 8, mb_y * 8, 0, 0, buf.clone(), 1, mc_buf);
                        mc_block8x8(dframe.data, dframe.offset[2] + mb_x * 8 + mb_y * 8 * dframe.stride[2], dframe.stride[2], mb_x * 8, mb_y * 8, 0, 0, buf.clone(), 2, mc_buf);
                    }
                    mb_idx += 1;
                }
            }
            self.shuf.add_golden_frame(new_gf);
        }

        if !keep_probs {
            self.scan.copy_from_slice(&self.tmp_scan);
        }
        if update_gf {
            self.shuf.add_golden_frame(buf.clone());
        }
        self.shuf.add_frame(buf.clone());

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(self.dstate.is_intra);
        frm.set_frame_type(if self.dstate.is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.shuf.clear();
    }
}

impl NAOptionHandler for VP7Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(VP7Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;

    #[test]
    fn test_vp7() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample from https://trac.ffmpeg.org/ticket/5580
        test_decoding("avi", "vp7", "assets/Duck/interlaced_blit_pitch.avi", Some(12), &dmx_reg,
                      &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0xb79fb6f8, 0xed51ac9e, 0x9e423456, 0xc0918e7f],
                            [0xbf8d1274, 0x83515e15, 0x8c0887de, 0xfbfd05d3],
                            [0x8ad00466, 0x80b6cbfb, 0x54de408e, 0x9efbc05e],
                            [0x144122c5, 0x6897b553, 0x93474d29, 0x1a1274ec],
                            [0x06ff5d07, 0x55825d38, 0x072b0a78, 0xfcb5020f],
                            [0xfd01591b, 0xc42113e7, 0xc5a5550f, 0xb30f3b02],
                            [0x155e0d6e, 0x96d75e06, 0x9bd7ce87, 0xacf868e1],
                            [0xfd79103a, 0x695d21d3, 0xfeacb5b4, 0x1d869d08],
                            [0xf4bcfeac, 0x0d2c305c, 0x11416c96, 0x626a5ef6],
                            [0x3579b66c, 0x0a7d7dc0, 0xe80b0395, 0xf6a70661],
                            [0x5773768c, 0x813442e9, 0x4dd6f793, 0xb10fe55f],
                            [0xcaaf0ddb, 0x65c2410e, 0x95da5bba, 0x3b90128e],
                            [0x74773773, 0xe1dbadeb, 0x57aaf64b, 0x9c21e3c7]]));
    }
}
