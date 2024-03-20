use nihav_codec_support::codecs::ZERO_MV;

use super::super::rv40data::*;

use super::*;
use super::dsp::*;
use super::motion_est::MotionEstimator;

const PRED_TYPES8: [PredType8x8; 4] = [
    PredType8x8::DC, PredType8x8::Hor, PredType8x8::Ver, PredType8x8::Plane
];

fn calc_dist(src1: &[u8], stride1: usize, src2: &[u8], stride2: usize, width: usize, height: usize) -> u32 {
    let mut sum = 0u32;
    for (line1, line2) in src1.chunks(stride1).zip(src2.chunks(stride2)).take(height) {
        sum += line1[..width].iter().zip(line2.iter()).fold(0u32,
            |acc, (&a, &b)| { let diff = u32::from(a.max(b)) - u32::from(a.min(b)); acc + diff * diff });
    }
    sum
}

struct SingleMacroblock {
    cand_blk:   RefMBData,
    pred_blk:   RefMBData,
    ref_blk:    RefMBData,

    wblk1:      RefMBData,
    wblk2:      RefMBData,

    tmpc:       [Block; 25],

    ratio1:     u32,
    ratio2:     u32,

    tmp_tx:     [Block; 25],
}

impl SingleMacroblock {
    fn new() -> Self {
        Self {
            cand_blk:   RefMBData::new(),
            pred_blk:   RefMBData::new(),
            ref_blk:    RefMBData::new(),
            wblk1:      RefMBData::new(),
            wblk2:      RefMBData::new(),
            tmpc:       [Block::new(); 25],
            ratio1:     0,
            ratio2:     0,
            tmp_tx:     [Block::new(); 25],
        }
    }
    fn load(&mut self, src: &[u8], offsets: [usize; 3], strides: [usize; 3]) {
        for (dst, src) in self.ref_blk.y.chunks_mut(16).zip(src[offsets[0]..].chunks(strides[0])) {
            dst.copy_from_slice(&src[..16]);
        }
        for (dst, src) in self.ref_blk.u.chunks_mut(8).zip(src[offsets[1]..].chunks(strides[1])) {
            dst.copy_from_slice(&src[..8]);
        }
        for (dst, src) in self.ref_blk.v.chunks_mut(8).zip(src[offsets[2]..].chunks(strides[2])) {
            dst.copy_from_slice(&src[..8]);
        }
    }
    fn recon_pred_part(&mut self, mbt: MacroblockType, ref_p: &NAVideoBuffer<u8>, ref_n: &NAVideoBuffer<u8>, mb_x: usize, mb_y: usize) {
        let (xpos, ypos) = (mb_x * 16, mb_y * 16);

        match mbt {
            MacroblockType::Intra16x16(_) => unreachable!(),
            MacroblockType::Intra4x4(_) => unreachable!(),
            MacroblockType::Inter16x16(mv) |
            MacroblockType::InterMix(mv) |
            MacroblockType::Backward(mv) => {
                luma_mc(&mut self.pred_blk.y, 16, ref_n, xpos, ypos, mv, true);
                chroma_mc(&mut self.pred_blk.u, 8, ref_n, xpos / 2, ypos / 2, 1, mv, true);
                chroma_mc(&mut self.pred_blk.v, 8, ref_n, xpos / 2, ypos / 2, 2, mv, true);
            },
            MacroblockType::PSkip => {
                luma_mc(&mut self.pred_blk.y, 16, ref_n, xpos, ypos, ZERO_MV, true);
                chroma_mc(&mut self.pred_blk.u, 8, ref_n, xpos / 2, ypos / 2, 1, ZERO_MV, true);
                chroma_mc(&mut self.pred_blk.v, 8, ref_n, xpos / 2, ypos / 2, 2, ZERO_MV, true);
            },
            MacroblockType::Inter16x8(mvs) => {
                let mvs = [mvs[0], mvs[0], mvs[1], mvs[1]];
                for (i, &mv) in mvs.iter().enumerate() {
                    let xadd = i & 1;
                    let yadd = i >> 1;
                    luma_mc(&mut self.pred_blk.y[xadd * 8 + yadd * 8 * 16..], 16, ref_n, xpos + xadd * 8, ypos + yadd * 8, mv, false);
                    chroma_mc(&mut self.pred_blk.u[xadd * 4 + yadd * 4 * 8..], 8, ref_n, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 1, mv, false);
                    chroma_mc(&mut self.pred_blk.v[xadd * 4 + yadd * 4 * 8..], 8, ref_n, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 2, mv, false);
                }
            },
            MacroblockType::Inter8x16(mvs) => {
                let mvs = [mvs[0], mvs[1], mvs[0], mvs[1]];
                for (i, &mv) in mvs.iter().enumerate() {
                    let xadd = i & 1;
                    let yadd = i >> 1;
                    luma_mc(&mut self.pred_blk.y[xadd * 8 + yadd * 8 * 16..], 16, ref_n, xpos + xadd * 8, ypos + yadd * 8, mv, false);
                    chroma_mc(&mut self.pred_blk.u[xadd * 4 + yadd * 4 * 8..], 8, ref_n, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 1, mv, false);
                    chroma_mc(&mut self.pred_blk.v[xadd * 4 + yadd * 4 * 8..], 8, ref_n, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 2, mv, false);
                }
            },
            MacroblockType::Inter8x8(mvs) => {
                for (i, &mv) in mvs.iter().enumerate() {
                    let xadd = i & 1;
                    let yadd = i >> 1;
                    luma_mc(&mut self.pred_blk.y[xadd * 8 + yadd * 8 * 16..], 16, ref_n, xpos + xadd * 8, ypos + yadd * 8, mv, false);
                    chroma_mc(&mut self.pred_blk.u[xadd * 4 + yadd * 4 * 8..], 8, ref_n, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 1, mv, false);
                    chroma_mc(&mut self.pred_blk.v[xadd * 4 + yadd * 4 * 8..], 8, ref_n, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 2, mv, false);
                }
            },
            MacroblockType::Forward(mv) => {
                luma_mc(&mut self.pred_blk.y, 16, ref_p, xpos, ypos, mv, true);
                chroma_mc(&mut self.pred_blk.u, 8, ref_p, xpos / 2, ypos / 2, 1, mv, true);
                chroma_mc(&mut self.pred_blk.v, 8, ref_p, xpos / 2, ypos / 2, 2, mv, true);
            },
            MacroblockType::Bidir(fmv, bmv) => {
                luma_mc(&mut self.wblk1.y, 16, ref_p, xpos, ypos, fmv, true);
                chroma_mc(&mut self.wblk1.u, 8, ref_p, xpos / 2, ypos / 2, 1, fmv, true);
                chroma_mc(&mut self.wblk1.v, 8, ref_p, xpos / 2, ypos / 2, 2, fmv, true);
                luma_mc(&mut self.wblk2.y, 16, ref_n, xpos, ypos, bmv, true);
                chroma_mc(&mut self.wblk2.u, 8, ref_n, xpos / 2, ypos / 2, 1, bmv, true);
                chroma_mc(&mut self.wblk2.v, 8, ref_n, xpos / 2, ypos / 2, 2, bmv, true);
                self.pred_blk.avg(&self.wblk1, self.ratio1, &self.wblk2, self.ratio2);
            },
            MacroblockType::BSkip(fmvs, bmvs) => {
                for (i, (&fmv, &bmv)) in fmvs.iter().zip(bmvs.iter()).enumerate() {
                    let xadd = i & 1;
                    let yadd = i >> 1;
                    luma_mc(&mut self.wblk1.y[xadd * 8 + yadd * 8 * 16..], 16, ref_p, xpos + xadd * 8, ypos + yadd * 8, fmv, false);
                    chroma_mc(&mut self.wblk1.u[xadd * 4 + yadd * 4 * 8..], 8, ref_p, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 1, fmv, false);
                    chroma_mc(&mut self.wblk1.v[xadd * 4 + yadd * 4 * 8..], 8, ref_p, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 2, fmv, false);
                    luma_mc(&mut self.wblk2.y[xadd * 8 + yadd * 8 * 16..], 16, ref_n, xpos + xadd * 8, ypos + yadd * 8, bmv, false);
                    chroma_mc(&mut self.wblk2.u[xadd * 4 + yadd * 4 * 8..], 8, ref_n, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 1, bmv, false);
                    chroma_mc(&mut self.wblk2.v[xadd * 4 + yadd * 4 * 8..], 8, ref_n, xpos / 2 + xadd * 4, ypos / 2 + xadd * 4, 2, bmv, false);
                }
                self.pred_blk.avg(&self.wblk1, self.ratio1, &self.wblk2, self.ratio2);
            },
        };
    }
    fn get_diff_metric(&mut self, be: &mut BitsEstimator, rdm: &RateDistMetric, best_m: u32, q_dc: usize, q_ac: usize, is16: bool, mut bits: u32) -> (u32, u32) {
        self.pred_blk.calc_coeffs(&self.ref_blk, &mut self.tmpc, q_dc, q_ac, is16);
        self.tmp_tx.copy_from_slice(&self.tmpc);
        if is16 {
            bits += be.block_bits(&self.tmpc[24], 24);
        }
        for blk in self.tmpc[..16].iter() {
            bits += be.block_bits(blk, 0);
        }
        for blk in self.tmpc[16..24].iter() {
            bits += be.block_bits(blk, 16);
        }
        let cdist = rdm.get_metric(bits, 0);
        if cdist > best_m {
            return (cdist, 0);
        }

        for blk in self.tmpc[..16].iter_mut() {
            blk.dequant(q_ac, q_ac);
        }
        let (cq_dc, cq_ac) = chroma_quants(q_ac);
        for blk in self.tmpc[16..24].iter_mut() {
            blk.dequant(cq_dc, cq_ac);
        }
        if is16 {
            let (blocks, dc_blk) = self.tmpc.split_at_mut(24);
            dc_blk[0].dequant_dcs(q_dc, q_ac);
            dc_blk[0].itransform_dcs();
            for (blk, &dc) in blocks.iter_mut().zip(dc_blk[0].coeffs.iter()) {
                blk.coeffs[0] = dc;
            }
        }

        self.cand_blk.copy_from(&self.pred_blk);
        let mut dist = 0;
        for (i, blk) in self.tmpc[..16].iter_mut().enumerate() {
            let off = (i & 3) * 4 + (i >> 2) * 4 * 16;
            if !blk.is_empty() {
                blk.itransform_4x4();
                blk.add_to(&mut self.cand_blk.y[off..], 16);
            }
            dist += calc_dist(&self.cand_blk.y[off..], 16, &self.ref_blk.y[off..], 16, 4, 4);
            let cdist = rdm.get_metric(bits, dist);
            if cdist > best_m {
                return (cdist, 0);
            }
        }
        let (_, cpart) = self.tmpc.split_at_mut(16);
        let (upart, vpart) = cpart.split_at_mut(4);
        for (i, (ublk, vblk)) in upart.iter_mut().zip(vpart.iter_mut()).enumerate() {
            let off = (i & 1) * 4 + (i >> 1) * 4 * 8;
            ublk.itransform_4x4();
            vblk.itransform_4x4();
            ublk.add_to(&mut self.cand_blk.u[off..], 8);
            vblk.add_to(&mut self.cand_blk.v[off..], 8);
            dist += calc_dist(&self.cand_blk.u[off..], 8, &self.ref_blk.u[off..], 8, 4, 4);
            dist += calc_dist(&self.cand_blk.v[off..], 8, &self.ref_blk.v[off..], 8, 4, 4);

            let cdist = rdm.get_metric(bits, dist);
            if cdist > best_m {
                return (cdist, 0);
            }
        }

        (rdm.get_metric(bits, dist), bits)
    }
    fn get_skip_metric(&self, rdm: &RateDistMetric, best_m: u32) -> (u32, u32) {
        let bits = 1;
        let mut dist = calc_dist(&self.pred_blk.y, 16, &self.ref_blk.y, 16, 16, 16);
        let cdist = rdm.get_metric(bits, dist);
        if cdist > best_m {
            return (cdist, 0);
        }
        dist += calc_dist(&self.pred_blk.u, 8, &self.ref_blk.u, 8, 8, 8);
        let cdist = rdm.get_metric(bits, dist);
        if cdist > best_m {
            return (cdist, 0);
        }
        dist += calc_dist(&self.pred_blk.v, 8, &self.ref_blk.v, 8, 8, 8);

        (rdm.get_metric(bits, dist), bits)
    }
    fn put_mb(dst: &mut NASimpleVideoFrame<u8>, cblk: &RefMBData, mb_x: usize, mb_y: usize) {
        for (dline, sline) in dst.data[dst.offset[0] + mb_x * 16 + mb_y * 16 * dst.stride[0]..].chunks_mut(dst.stride[0]).zip(cblk.y.chunks(16)) {
            dline[..16].copy_from_slice(sline);
        }
        for (dline, sline) in dst.data[dst.offset[1] + mb_x * 8 + mb_y * 8 * dst.stride[1]..].chunks_mut(dst.stride[1]).zip(cblk.u.chunks(8)) {
            dline[..8].copy_from_slice(sline);
        }
        for (dline, sline) in dst.data[dst.offset[2] + mb_x * 8 + mb_y * 8 * dst.stride[2]..].chunks_mut(dst.stride[2]).zip(cblk.v.chunks(8)) {
            dline[..8].copy_from_slice(sline);
        }
    }
}

pub struct MacroblockDecider {
    pub q:          usize,
        has_top:    bool,
        has_left:   bool,
        has_tl:     bool,
        has_tr:     bool,
        mb_x:       usize,
        mb_y:       usize,
        best_mbt:   MacroblockType,
        best_dist:  u32,
        best_bits:  u32,
        ipred_y:    IntraPred16x16,
        ipred_u:    IntraPred16x16,
        ipred_v:    IntraPred16x16,
        top_y:      Vec<u8>,
        top_u:      Vec<u8>,
        top_v:      Vec<u8>,
        tr_d:       u32,
        tr_b:       u32,
        mb:         SingleMacroblock,
        best_coef:  [Block; 25],
        best_blk:   RefMBData,
}

impl MacroblockDecider {
    pub fn new() -> Self {
        Self {
            q:          0,
            has_top:    false,
            has_left:   false,
            has_tl:     false,
            has_tr:     false,
            mb_x:       0,
            mb_y:       0,
            ipred_y:    IntraPred16x16::new(),
            ipred_u:    IntraPred16x16::new(),
            ipred_v:    IntraPred16x16::new(),
            top_y:      Vec::new(),
            top_u:      Vec::new(),
            top_v:      Vec::new(),
            tr_b:       0,
            tr_d:       0,
            best_mbt:   MacroblockType::default(),
            best_dist:  0,
            best_bits:  0,
            mb:         SingleMacroblock::new(),
            best_coef:  [Block::new(); 25],
            best_blk:   RefMBData::new(),
        }
    }
    pub fn resize(&mut self, mb_w: usize) {
        self.top_y.resize((mb_w + 1) * 16 + 1, 0);
        self.top_u.resize((mb_w + 1) *  8 + 1, 0);
        self.top_v.resize((mb_w + 1) *  8 + 1, 0);
    }
    pub fn set_b_distance(&mut self, tr_b: u32, tr_d: u32) {
        let (ratio1, ratio2) = if tr_d != 0 {
                (((tr_d - tr_b) << 14) / tr_d, (tr_b << 14) / tr_d)
            } else { (1 << 13, 1 << 13) };
        self.tr_b = tr_b;
        self.tr_d = tr_d;
        self.mb.ratio1 = ratio1;
        self.mb.ratio2 = ratio2;
    }
    pub fn load_mb(&mut self, src: &[u8], offsets: [usize; 3], strides: [usize; 3], sstate: &SliceState) {
        self.has_top  = sstate.has_t;
        self.has_left = sstate.has_l;
        self.has_tl   = sstate.has_tl;
        self.has_tr   = sstate.has_tr;
        self.mb_x     = sstate.mb_x;
        self.mb_y     = sstate.mb_y;

        self.ipred_y.top[1..].copy_from_slice(&self.top_y[self.mb_x * 16 + 1..][..16]);
        self.ipred_u.top[1..9].copy_from_slice(&self.top_u[self.mb_x * 8 + 1..][..8]);
        self.ipred_v.top[1..9].copy_from_slice(&self.top_v[self.mb_x * 8 + 1..][..8]);

        self.mb.load(src, offsets, strides);

        self.best_mbt = MacroblockType::default();
        self.best_dist = std::u32::MAX;
        self.best_bits = 0;
    }
    pub fn try_b_coding(&mut self, ref_p: &NAVideoBuffer<u8>, ref_n: &NAVideoBuffer<u8>, be: &mut BitsEstimator, me: &mut MotionEstimator, rdm: &RateDistMetric, mbstate: &MBState, refine: bool) {
        let q_dc = usize::from(RV40_QUANT_DC[1][self.q]);

        let blk8_idx = mbstate.get_blk8_idx(self.mb_x, self.mb_y);
        let mut smb_f = [ZERO_MV; 4];
        let mut smb_b = [ZERO_MV; 4];
        for (i, (fwd, bwd)) in smb_f.iter_mut().zip(smb_b.iter_mut()).enumerate() {
            let ref_mv = mbstate.ref_mv[blk8_idx + (i & 1) + (i >> 1) * mbstate.blk8_stride];
            let (fm, bm) = ref_mv.scale(self.tr_d, self.tr_b);
            *fwd = fm;
            *bwd = bm;
        }
        self.mb.recon_pred_part(MacroblockType::BSkip(smb_f, smb_b), ref_p, ref_n, self.mb_x, self.mb_y);
        be.set_mb_type(MBType::Skip);
        let (cur_dist, cur_bits) = self.mb.get_skip_metric(rdm, self.best_dist);
        if cur_dist < self.best_dist {
            self.best_dist = cur_dist;
            self.best_bits = cur_bits;
            self.best_mbt = MacroblockType::BSkip(smb_f, smb_b);
            self.best_blk.copy_from(&self.mb.pred_blk);
            if self.best_dist < rdm.good_enough {
                return;
            }
        }

        let fwd_cand = [
            -mbstate.ref_mv[blk8_idx],
            mbstate.fwd_mv[blk8_idx - 1],
            mbstate.fwd_mv[blk8_idx - 1 - mbstate.blk8_stride],
            mbstate.fwd_mv[blk8_idx -     mbstate.blk8_stride],
            mbstate.fwd_mv[blk8_idx + 2 - mbstate.blk8_stride]
        ];
        let (fmv, _fdist) = me.search_mb_p(ref_p, &self.mb.ref_blk, self.mb_x, self.mb_y, &fwd_cand);
        be.set_mb_type(MBType::Forward);
        let bcost = be.estimate_mb_hdr(&[fmv]);
        self.mb.recon_pred_part(MacroblockType::Forward(fmv), ref_p, ref_n, self.mb_x, self.mb_y);
        let (cur_dist, cur_bits) = self.mb.get_diff_metric(be, rdm, self.best_dist, q_dc, self.q, false, bcost);
        if cur_dist < self.best_dist {
            self.best_dist = cur_dist;
            self.best_bits = cur_bits;
            self.best_mbt = MacroblockType::Forward(fmv);
            self.best_coef.copy_from_slice(&self.mb.tmp_tx);
            self.best_blk.copy_from(&self.mb.cand_blk);
            if self.best_dist < rdm.good_enough {
                return;
            }
        }

        let bwd_cand = [
            mbstate.ref_mv[blk8_idx],
            mbstate.bwd_mv[blk8_idx - 1],
            mbstate.bwd_mv[blk8_idx - 1 - mbstate.blk8_stride],
            mbstate.bwd_mv[blk8_idx -     mbstate.blk8_stride],
            mbstate.bwd_mv[blk8_idx + 2 - mbstate.blk8_stride]
        ];
        let (bmv, _bdist) = me.search_mb_p(ref_n, &self.mb.ref_blk, self.mb_x, self.mb_y, &bwd_cand);
        be.set_mb_type(MBType::Backward);
        let bcost = be.estimate_mb_hdr(&[bmv]);
        self.mb.recon_pred_part(MacroblockType::Backward(bmv), ref_p, ref_n, self.mb_x, self.mb_y);
        let (cur_dist, cur_bits) = self.mb.get_diff_metric(be, rdm, self.best_dist, q_dc, self.q, false, bcost);
        if cur_dist < self.best_dist {
            self.best_dist = cur_dist;
            self.best_bits = cur_bits;
            self.best_mbt = MacroblockType::Backward(bmv);
            self.best_coef.copy_from_slice(&self.mb.tmp_tx);
            self.best_blk.copy_from(&self.mb.cand_blk);
            if self.best_dist < rdm.good_enough {
                return;
            }
        }

        be.set_mb_type(MBType::Bidir);
        let (i_fmv, i_bmv) = if !refine {
                (fmv, bmv)
            } else {
                let mut b_searcher = SearchB::new(ref_p, ref_n, self.mb_x, self.mb_y, [self.mb.ratio1, self.mb.ratio2]);
                b_searcher.search_mb(&self.mb.ref_blk, [fmv, bmv])
            };

        let bcost = be.estimate_mb_hdr(&[i_fmv, i_bmv]);
        self.mb.recon_pred_part(MacroblockType::Bidir(i_fmv, i_bmv), ref_p, ref_n, self.mb_x, self.mb_y);
        let (cur_dist, cur_bits) = self.mb.get_diff_metric(be, rdm, self.best_dist, q_dc, self.q, false, bcost);
        if cur_dist < self.best_dist {
            self.best_dist = cur_dist;
            self.best_bits = cur_bits;
            self.best_coef.copy_from_slice(&self.mb.tmp_tx);
            self.best_blk.copy_from(&self.mb.cand_blk);
            self.best_mbt = MacroblockType::Bidir(i_fmv, i_bmv);
        }
    }
    pub fn try_p_coding(&mut self, ref_pic: &NAVideoBuffer<u8>, be: &mut BitsEstimator, me: &mut MotionEstimator, rdm: &RateDistMetric, mbstate: &MBState) {
        let q_dc = usize::from(RV40_QUANT_DC[1][self.q]);

        self.mb.recon_pred_part(MacroblockType::Inter16x16(ZERO_MV), ref_pic, ref_pic, self.mb_x, self.mb_y);
        be.set_mb_type(MBType::Skip);
        let (cur_dist, cur_bits) = self.mb.get_skip_metric(rdm, self.best_dist);
        if cur_dist < self.best_dist {
            self.best_dist = cur_dist;
            self.best_bits = cur_bits;
            self.best_mbt = MacroblockType::PSkip;
            self.best_blk.copy_from(&self.mb.pred_blk);
            if self.best_dist < rdm.good_enough {
                return;
            }
        }

        let blk8_idx = mbstate.get_blk8_idx(self.mb_x, self.mb_y);
        let mv_cand = [
            mbstate.fwd_mv[blk8_idx - 1],
            mbstate.fwd_mv[blk8_idx - 1 - mbstate.blk8_stride],
            mbstate.fwd_mv[blk8_idx -     mbstate.blk8_stride],
            mbstate.fwd_mv[blk8_idx + 2 - mbstate.blk8_stride]
        ];
        let (mv, pdist) = me.search_mb_p(ref_pic, &self.mb.ref_blk, self.mb_x, self.mb_y, &mv_cand);

        self.mb.recon_pred_part(MacroblockType::Inter16x16(mv), ref_pic, ref_pic, self.mb_x, self.mb_y);

        be.set_mb_type(MBType::P16x16);
        let pcost = be.estimate_mb_hdr(&[mv]);
        let (cur_dist, cur_bits) = self.mb.get_diff_metric(be, rdm, self.best_dist, q_dc, self.q, false, pcost);
        if cur_dist < self.best_dist {
            self.best_mbt = MacroblockType::Inter16x16(mv);
            self.best_dist = cur_dist;
            self.best_bits = cur_bits;
            self.best_coef.copy_from_slice(&self.mb.tmp_tx);
            self.best_blk.copy_from(&self.mb.cand_blk);
        }
        be.set_mb_type(MBType::P16x16Mix);
        let p16cost = be.estimate_mb_hdr(&[mv]);
        let (cur_dist16, cur_bits16) = self.mb.get_diff_metric(be, rdm, self.best_dist, q_dc, self.q, true,  p16cost);
        if cur_dist16 < self.best_dist {
            self.best_mbt = MacroblockType::InterMix(mv);
            self.best_dist = cur_dist16;
            self.best_bits = cur_bits16;
            self.best_coef.copy_from_slice(&self.mb.tmp_tx);
            self.best_blk.copy_from(&self.mb.cand_blk);
        }

        if pdist > rdm.p_split_thr {
            let xpos = self.mb_x * 16;
            let ypos = self.mb_y * 16;

            let mv_cand = [
                mv,
                mbstate.fwd_mv[blk8_idx - 1],
                mbstate.fwd_mv[blk8_idx - 1 - mbstate.blk8_stride],
                mbstate.fwd_mv[blk8_idx -     mbstate.blk8_stride],
                mbstate.fwd_mv[blk8_idx - 1 + mbstate.blk8_stride],
                mbstate.fwd_mv[blk8_idx + 2 - mbstate.blk8_stride],
                mbstate.fwd_mv[blk8_idx + 1 - mbstate.blk8_stride]
            ];

            let (mv0, pdist0) = me.search_blk8(ref_pic, &self.mb.ref_blk, xpos,     ypos,     &mv_cand);
            let (mv1, pdist1) = me.search_blk8(ref_pic, &self.mb.ref_blk, xpos + 8, ypos,     &mv_cand);
            let (mv2, pdist2) = me.search_blk8(ref_pic, &self.mb.ref_blk, xpos,     ypos + 8, &mv_cand);
            let (mv3, pdist3) = me.search_blk8(ref_pic, &self.mb.ref_blk, xpos + 8, ypos + 8, &mv_cand);
            if pdist0 + pdist1 + pdist2 + pdist3 < pdist - pdist / 4 {
                let mvs = [mv0, mv1, mv2, mv3];
                let (cand_mbt, cand_mbtype) = if mv0 == mv1 && mv2 == mv3 {
                        (MBType::P16x8, MacroblockType::Inter16x8([mv0, mv2]))
                    } else if mv0 == mv2 && mv1 == mv3 {
                        (MBType::P8x16, MacroblockType::Inter8x16([mv0, mv1]))
                    } else {
                        (MBType::P8x8, MacroblockType::Inter8x8(mvs))
                    };
                be.set_mb_type(cand_mbt);
                let pcost = be.estimate_mb_hdr(&mvs);

                self.mb.recon_pred_part(MacroblockType::Inter8x8(mvs), ref_pic, ref_pic, self.mb_x, self.mb_y);
                let (cur_dist, cur_bits) = self.mb.get_diff_metric(be, rdm, self.best_dist, q_dc, self.q, false, pcost);
                if cur_dist < self.best_dist {
                    self.best_dist = cur_dist;
                    self.best_mbt = cand_mbtype;
                    self.best_bits = cur_bits;
                    self.best_coef.copy_from_slice(&self.mb.tmp_tx);
                    self.best_blk.copy_from(&self.mb.cand_blk);
                }
            }
        }
    }
    fn recon_intra_16_pred(&mut self, ptype: PredType8x8) {
        self.ipred_y.apply16(ptype, &mut self.mb.pred_blk.y, 16);
        self.ipred_u.apply8(ptype, &mut self.mb.pred_blk.u, 8);
        self.ipred_v.apply8(ptype, &mut self.mb.pred_blk.v, 8);
    }
    pub fn try_intra_16_pred(&mut self, be: &mut BitsEstimator, rdm: &RateDistMetric) {
        if self.best_dist < rdm.good_enough {
            return;
        }
        let pred_types_try: &[PredType8x8] = match (self.has_top, self.has_left) {
                (false, false) => &[PredType8x8::DC128],
                (true,  false) => &[PredType8x8::TopDC],
                (false, true)  => &[PredType8x8::LeftDC],
                _ => &PRED_TYPES8,
            };

        be.set_mb_type(MBType::Intra16);
        let hdr_cost = be.estimate_mb_hdr(&[]);
        for &ptype in pred_types_try.iter() {
            if !self.has_tl && matches!(ptype, PredType8x8::Plane) {
                continue;
            }
            self.recon_intra_16_pred(ptype);
            let q_dc = usize::from(RV40_QUANT_DC[0][self.q]);
            let (cur_dist, cur_bits) = self.mb.get_diff_metric(be, rdm, self.best_dist, q_dc, self.q, true, hdr_cost);
            if cur_dist < self.best_dist {
                self.best_mbt = MacroblockType::Intra16x16(ptype);
                self.best_dist  = cur_dist;
                self.best_bits  = cur_bits;
                self.best_coef.copy_from_slice(&self.mb.tmp_tx);
                self.best_blk.copy_from(&self.mb.cand_blk);
                if cur_dist < rdm.good_enough {
                    break;
                }
            }
        }
    }
    pub fn try_intra_4x4_pred(&mut self, be: &mut BitsEstimator, rdm: &RateDistMetric, mbstate: &mut MBState) {
        const PRED4_DEF: &[PredType4x4] = &[ PredType4x4::DC128 ];
        const PRED4_NO_TOP: &[PredType4x4] = &[ PredType4x4::Hor, PredType4x4::LeftDC ];
        const PRED4_NO_LEFT: &[PredType4x4] = &[ PredType4x4::Ver, PredType4x4::TopDC ];
        const PRED4_FULL: &[PredType4x4] = &[
            PredType4x4::Ver, PredType4x4::Hor, PredType4x4::DC,
            PredType4x4::DiagDownLeft, PredType4x4::DiagDownRight,
            PredType4x4::VerRight, PredType4x4::HorDown,
            PredType4x4::VerLeft,  PredType4x4::HorUp
        ];
        const PRED4_FULL_NO_LD: &[PredType4x4] = &[
            PredType4x4::Ver, PredType4x4::Hor, PredType4x4::DC,
            PredType4x4::DiagDownLeftNoDown, PredType4x4::DiagDownRight,
            PredType4x4::VerRight, PredType4x4::HorDown,
            PredType4x4::VerLeftNoDown,  PredType4x4::HorUpNoDown
        ];

        if self.best_dist < rdm.good_enough {
            return;
        }
        be.set_mb_type(MBType::Intra);

        let (tr_y, tr_u, tr_v) = if self.has_tr {
                let mut tr_y = [0; 4];
                let mut tr_u = [0; 4];
                let mut tr_v = [0; 4];
                tr_y.copy_from_slice(&self.top_y[self.mb_x * 16 + 16 + 1..][..4]);
                tr_u.copy_from_slice(&self.top_u[self.mb_x * 8 + 8 + 1..][..4]);
                tr_v.copy_from_slice(&self.top_v[self.mb_x * 8 + 8 + 1..][..4]);
                (tr_y, tr_u, tr_v)
            } else {
                ([self.ipred_y.top[16]; 4], [self.ipred_u.top[8]; 4], [self.ipred_v.top[8]; 4])
            };
        let mut ipred4 = BlockIntra4Pred::new(&self.ipred_y, &self.ipred_u, &self.ipred_v, tr_y, tr_u, tr_v, self.has_left);

        let q_ac = self.q;
        let (cq_dc, cq_ac) = chroma_quants(self.q);
        let mut tot_dist = 0;
        let mut tot_bits = be.estimate_mb_hdr(&[]);
        let mut modes = [PredType4x4::DC; 16];
        let mut tblk = Block::new();
        let mut has_t = self.has_top;

        for y in 0..4 {
            let mut has_l = self.has_left;
            let mut has_ld = has_l && y != 3;
            for x in 0..4 {
                let list = match (has_l, has_t) {
                        (true,  true) if has_ld => PRED4_FULL,
                        (true,  true) => PRED4_FULL_NO_LD,
                        (false, true) => PRED4_NO_LEFT,
                        (true, false) => PRED4_NO_TOP,
                        _ => PRED4_DEF,
                    };

                let do_chroma = ((x & 1) == 0) && ((y & 1) == 0);

                let mut best_mode = PRED4_DEF[0];
                let mut best_cdist = std::u32::MAX;
                let mut best_dist = 0;
                let mut best_bits = 0;
                for &try_mode in list.iter() {
                    ipred4.pred_block(&mut self.mb.cand_blk, x, y, try_mode);
                    let off = x * 4 + y * 4 * 16;
                    let (mut cur_dist, mut cur_bits) = Self::blk4_diff(&self.mb.cand_blk.y[off..], &self.mb.ref_blk.y[off..], 16, q_ac, q_ac, be);
                    if do_chroma {
                        let off = x * 2 + y * 2 * 8;
                        let (du, bu) = Self::blk4_diff(&self.mb.cand_blk.u[off..], &self.mb.ref_blk.u[off..], 8, cq_dc, cq_ac, be);
                        let (dv, bv) = Self::blk4_diff(&self.mb.cand_blk.v[off..], &self.mb.ref_blk.v[off..], 8, cq_dc, cq_ac, be);
                        cur_dist += du + dv;
                        cur_bits += bu + bv;
                    }

                    let cand_dist = rdm.get_metric(cur_bits, cur_dist);
                    if cand_dist < best_cdist {
                        best_cdist = cand_dist;
                        best_mode  = try_mode;
                        best_dist  = cur_dist;
                        best_bits  = cur_bits;
                    }
                }

                ipred4.pred_block(&mut self.mb.cand_blk, x, y, best_mode);

                let off = x * 4 + y * 4 * 16;
                tblk.from_diff(&self.mb.ref_blk.y[off..], &self.mb.cand_blk.y[off..], 16);
                tblk.transform_4x4();
                tblk.quant(q_ac, q_ac);
                self.mb.tmp_tx[x + y * 4] = tblk;
                if !tblk.is_empty() {
                    tblk.dequant(q_ac, q_ac);
                    tblk.itransform_4x4();
                    tblk.add_to(&mut self.mb.cand_blk.y[off..], 16);
                }
                if do_chroma {
                    let off = x * 2 + y * 2 * 8;
                    let mut dests = [&mut self.mb.cand_blk.u[off..], &mut self.mb.cand_blk.v[off..]];
                    let sources = [&self.mb.ref_blk.u[off..], &self.mb.ref_blk.v[off..]];
                    for (comp, (dblk, &sblk)) in dests.iter_mut().zip(sources.iter()).enumerate() {
                        tblk.from_diff(sblk, dblk, 8);
                        tblk.transform_4x4();
                        tblk.quant(cq_dc, cq_ac);
                        self.mb.tmp_tx[16 + comp * 4 + x / 2 + y] = tblk;
                        if !tblk.is_empty() {
                            tblk.dequant(cq_dc, cq_ac);
                            tblk.itransform_4x4();
                            tblk.add_to(dblk, 8);
                        }
                    }
                }

                ipred4.update_from(&self.mb.cand_blk, x, y);

                tot_dist += best_dist;
                tot_bits += best_bits;

                let cand_dist = rdm.get_metric(tot_bits, tot_dist);
                if cand_dist > self.best_dist {
                    return;
                }

                modes[x + y * 4] = best_mode;

                has_l = true;
                has_ld = false;
            }
            has_t = true;
        }

        mbstate.set_ipred4x4(self.mb_x, self.mb_y, &modes);

        if !self.has_top {
            let mut code = 0usize;
            for &el in modes[..4].iter() {
                code = code * 2 + if el.to_index() == 0 { 0 } else { 1 };
            }
            tot_bits += u32::from(RV40_AIC_TOP_BITS[code]);
        }

        let ystart = if self.has_top { 0 } else { 1 };
        for y in ystart..4 {
            let mut x = 0;
            while x < 4 {
                let (lctx, tctx, trctx) = mbstate.get_ipred4x4_ctx(self.mb_x, self.mb_y, x, y);
                let ctx_word = if x < 3 {
                        ((trctx & 0xF) as u16) + (((tctx & 0xF) as u16) << 4) + (((lctx & 0xF) as u16) << 8)
                    } else { 0xFFF };
                if let Some(idx) = RV40_AIC_PATTERNS.iter().position(|&x| x == ctx_word) {
                    let code = modes[x + y * 4].to_index() * 9 + modes[x + y * 4 + 1].to_index();
                    tot_bits += u32::from(RV40_AIC_MODE2_BITS[idx][code as usize]);
                    x += 2;
                } else if tctx != -1 && lctx != -1 {
                    let idx = (tctx + lctx * 10) as usize;
                    let code = modes[x + y * 4].to_index() as usize;
                    tot_bits += u32::from(RV40_AIC_MODE1_BITS[idx][code]);
                    x += 1;
                } else {
                    match lctx {
                        -1 if tctx < 2 => tot_bits += 1,
                        0 | 2 => tot_bits += 1,
                        _ => {},
                    };
                    x += 1;
                }
            }
        }

        let cand_dist = rdm.get_metric(tot_bits, tot_dist);
        if cand_dist < self.best_dist {
            self.best_dist = cand_dist;
            self.best_mbt = MacroblockType::Intra4x4(modes);
            self.best_bits = tot_bits;
            self.best_coef.copy_from_slice(&self.mb.tmp_tx);
            self.best_blk.copy_from(&self.mb.cand_blk);
        }
    }
    pub fn get_est_bits(&self) -> u32 { self.best_bits }
    pub fn get_macroblock(&mut self) -> Macroblock {
        let mut coeffs = [Block::new(); 25];
        if !self.best_mbt.is_skip() {
            coeffs.copy_from_slice(&self.best_coef);
        }
        Macroblock {
            mb_type:    self.best_mbt.clone(),
            coeffs,
        }
    }
    pub fn recon_mb(&mut self, dst: &mut NASimpleVideoFrame<u8>) {
        let src_mb = &self.best_blk;
        SingleMacroblock::put_mb(dst, src_mb, self.mb_x, self.mb_y);

        self.top_y[self.mb_x * 16 + 1..][..16].copy_from_slice(&src_mb.y[15 * 16..]);
        self.top_u[self.mb_x * 8 + 1..][..8].copy_from_slice(&src_mb.u[7 * 8..]);
        self.top_v[self.mb_x * 8 + 1..][..8].copy_from_slice(&src_mb.v[7 * 8..]);

        self.ipred_y.top[0] = self.ipred_y.top[16];
        self.ipred_y.left[0] = self.ipred_y.top[0];
        self.ipred_u.top[0] = self.ipred_u.top[8];
        self.ipred_u.left[0] = self.ipred_u.top[0];
        self.ipred_v.top[0] = self.ipred_v.top[8];
        self.ipred_v.left[0] = self.ipred_v.top[0];

        for (left, src) in self.ipred_y.left[1..].iter_mut().zip(src_mb.y.chunks_exact(16)) {
            *left = src[15];
        }
        for (left, src) in self.ipred_u.left[1..9].iter_mut().zip(src_mb.u.chunks_exact(8)) {
            *left = src[7];
        }
        for (left, src) in self.ipred_v.left[1..9].iter_mut().zip(src_mb.v.chunks_exact(8)) {
            *left = src[7];
        }
    }
    fn blk4_diff(pred: &[u8], refsrc: &[u8], stride: usize, q_dc: usize, q_ac: usize, be: &mut BitsEstimator) -> (u32, u32) {
        let mut blk = Block::new();
        blk.from_diff(refsrc, pred, stride);
        blk.transform_4x4();
        blk.quant(q_dc, q_ac);
        let bits = be.block_bits(&blk, 0);
        if !blk.is_empty() {
            blk.dequant(q_dc, q_ac);
            blk.itransform_4x4();
        }
        let mut dist = 0u32;
        for (diffs, (pred, refsrc)) in blk.coeffs.chunks(4).zip(pred.chunks(stride).zip(refsrc.chunks(stride))) {
            for (&diff, (&p, &r)) in diffs.iter().zip(pred.iter().zip(refsrc.iter())) {
                let new = (i32::from(p) + i32::from(diff)).max(0).min(255);
                let expected = i32::from(r);
                dist += ((new - expected) * (new - expected)) as u32;
            }
        }
        (dist, bits)
    }
}
