use nihav_core::frame::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::super::vpcommon::*;
use super::VP56DCPred;
use super::dsp::*;
use super::rdo::*;

/*#[cfg(debug_assertions)]
use std::io::Write;
#[cfg(debug_assertions)]
use std::fs::File;
#[cfg(debug_assertions)]
pub fn dump_pgm(vbuf: &NAVideoBuffer<u8>, name: &str) {
    let dst = vbuf.get_data();
    let (w, h) = vbuf.get_dimensions(0);
    let mut file = File::create(name).unwrap();
    file.write_all(format!("P5\n{} {}\n255\n", w, h * 3 / 2).as_bytes()).unwrap();
    for row in dst[vbuf.get_offset(0)..].chunks(vbuf.get_stride(0)).take(h).rev() {
        file.write_all(row).unwrap();
    }
    for (row1, row2) in dst[vbuf.get_offset(1)..].chunks(vbuf.get_stride(1)).take(h / 2).zip(dst[vbuf.get_offset(2)..].chunks(vbuf.get_stride(2))).rev() {
       file.write_all(row1).unwrap();
       file.write_all(row2).unwrap();
    }
}*/

pub type Coeffs = [[i16; 64]; 6];

#[derive(Clone)]
pub struct ResidueMB {
    pub coeffs:     Coeffs,
}

impl ResidueMB {
    fn new() -> Self {
        Self {
            coeffs: [[0; 64]; 6],
        }
    }
    fn fdct(&mut self) {
        for blk in self.coeffs.iter_mut() {
            vp_fdct(blk);
        }
    }
    fn idct(&mut self) {
        for blk in self.coeffs.iter_mut() {
            vp_idct(blk);
        }
    }
    fn quant(&mut self, q: usize) {
        for blk in self.coeffs.iter_mut() {
            if blk[0] != 0 {
                blk[0] /= VP56_DC_QUANTS[q] * 4;
            }
            for coef in blk[1..].iter_mut() {
                if *coef != 0 {
                    *coef /= VP56_AC_QUANTS[q] * 4;
                }
            }
        }
    }
    fn dequant(&mut self, q: usize) {
        for blk in self.coeffs.iter_mut() {
            if blk[0] != 0 {
                blk[0] *= VP56_DC_QUANTS[q] * 4;
            }
            for coef in blk[1..].iter_mut() {
                if *coef != 0 {
                    *coef *= VP56_AC_QUANTS[q] * 4;
                }
            }
        }
    }
    fn dequant_from(&mut self, src: &Self, q: usize) {
        for (dblk, sblk) in self.coeffs.iter_mut().zip(src.coeffs.iter()) {
            dblk[0] = if sblk[0] != 0 { sblk[0] * VP56_DC_QUANTS[q] * 4 } else { 0 };
            for (dcoef, &scoef) in dblk[1..].iter_mut().zip(sblk[1..].iter()) {
                *dcoef = if scoef != 0 { scoef * VP56_AC_QUANTS[q] * 4 } else { 0 };
            }
        }
    }
    fn fill(&self, dst: &mut [[u8; 64]; 6]) {
        for (dblk, sblk) in dst.iter_mut().zip(self.coeffs.iter()) {
            for (dcoef, &scoef) in dblk.iter_mut().zip(sblk.iter()) {
                *dcoef = scoef as u8;
            }
        }
    }
}

#[derive(Clone)]
pub struct InterMB {
    pub residue:    ResidueMB,
    pub reference:  Coeffs,
    pub mv:         [MV; 4],
}

impl InterMB {
    fn new() -> Self {
        Self {
            residue:    ResidueMB::new(),
            reference:  [[0; 64]; 6],
            mv:         [ZERO_MV; 4],
        }
    }
}

const VP56_DC_QUANTS: [i16; 64] = [
    47, 47, 47, 47, 45, 43, 43, 43,
    43, 43, 42, 41, 41, 40, 40, 40,
    40, 35, 35, 35, 35, 33, 33, 33,
    33, 32, 32, 32, 27, 27, 26, 26,
    25, 25, 24, 24, 23, 23, 19, 19,
    19, 19, 18, 18, 17, 16, 16, 16,
    16, 16, 15, 11, 11, 11, 10, 10,
     9,  8,  7,  5,  3,  3,  2,  2
];
const VP56_AC_QUANTS: [i16; 64] = [
    94, 92, 90, 88, 86, 82, 78, 74,
    70, 66, 62, 58, 54, 53, 52, 51,
    50, 49, 48, 47, 46, 45, 44, 43,
    42, 40, 39, 37, 36, 35, 34, 33,
    32, 31, 30, 29, 28, 27, 26, 25,
    24, 23, 22, 21, 20, 19, 18, 17,
    16, 15, 14, 13, 12, 11, 10,  9,
     8,  7,  6,  5,  4,  3,  2,  1
];

const VP56_FILTER_LIMITS: [u8; 64] = [
    14, 14, 13, 13, 12, 12, 10, 10,
    10, 10,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  7,  7,  7,  7,
     7,  7,  6,  6,  6,  6,  6,  6,
     5,  5,  5,  5,  4,  4,  4,  4,
     4,  4,  4,  3,  3,  3,  3,  2
];

#[derive(Default)]
pub struct FrameEncoder {
    pub quant:      usize,
    pub src_mbs:    Vec<ResidueMB>,
    pub intra_mbs:  Vec<ResidueMB>,
    pub inter_mbs:  Vec<InterMB>,
    pub fourmv_mbs: Vec<InterMB>,
    pub golden_mbs: Vec<InterMB>,

    pub mb_types:   Vec<VPMBType>,
    pub num_mv:     Vec<u8>,
    pub coded_mv:   Vec<[MV; 4]>,
    pub fmv_sub:    Vec<[VPMBType; 4]>,

    pub mb_w:       usize,
    pub mb_h:       usize,

    pub me_mode:    MVSearchMode,
    pub me_range:   i16,
}

macro_rules! read_block {
    ($dst: expr, $src: expr, $stride: expr) => {
        for (drow, srow) in $dst.chunks_mut(8).zip($src.chunks($stride).take(8)) {
            for (dst, &src) in drow.iter_mut().zip(srow.iter()) {
                *dst = i16::from(src);
            }
        }
    }
}

macro_rules! write_block {
    ($dst: expr, $src: expr, $stride: expr) => {
        for (drow, srow) in $dst.chunks_mut($stride).take(8).zip($src.chunks(8)) {
            drow[..8].copy_from_slice(srow);
        }
    }
}

impl FrameEncoder {
    pub fn new() -> Self { Self::default() }
    pub fn resize(&mut self, mb_w: usize, mb_h: usize) {
        self.mb_w = mb_w;
        self.mb_h = mb_h;

        let num_mbs = self.mb_w * self.mb_h;
        self.src_mbs.clear();
        self.src_mbs.reserve(num_mbs);
        self.intra_mbs.clear();
        self.intra_mbs.reserve(num_mbs);
        self.inter_mbs.clear();
        self.inter_mbs.reserve(num_mbs);
        self.fourmv_mbs.clear();
        self.fourmv_mbs.reserve(num_mbs);
        self.golden_mbs.clear();
        self.golden_mbs.reserve(num_mbs);

        self.mb_types.clear();
        self.mb_types.reserve(num_mbs);
        self.num_mv.clear();
        self.num_mv.reserve(num_mbs);
        self.coded_mv.clear();
        self.coded_mv.reserve(num_mbs);
        self.fmv_sub.clear();
        self.fmv_sub.reserve(num_mbs);
    }
    pub fn set_quant(&mut self, quant: usize) { self.quant = quant; }
    pub fn read_mbs(&mut self, vbuf: &NAVideoBuffer<u8>) {
        let src = vbuf.get_data();
        let y = &src[vbuf.get_offset(0)..];
        let ystride = vbuf.get_stride(0);
        let u = &src[vbuf.get_offset(1)..];
        let ustride = vbuf.get_stride(1);
        let v = &src[vbuf.get_offset(2)..];
        let vstride = vbuf.get_stride(2);
        let (w, _) = vbuf.get_dimensions(0);

        self.src_mbs.clear();
        for (ys, (us, vs)) in y.chunks(ystride * 16).zip(u.chunks(ustride * 8).zip(v.chunks(vstride * 8))) {
            for x in (0..w).step_by(16) {
                let mut mb = ResidueMB::new();
                for (i, blk) in mb.coeffs[..4].iter_mut().enumerate() {
                    read_block!(blk, ys[x + (i & 1) * 8 + (i >> 1) * 8 * ystride..], ystride);
                }
                read_block!(mb.coeffs[4], us[x/2..], ustride);
                read_block!(mb.coeffs[5], vs[x/2..], vstride);
                self.src_mbs.push(mb);
            }
        }
    }
    pub fn reconstruct_frame(&mut self, dc_pred: &mut VP56DCPred, mut vbuf: NAVideoBufferRef<u8>) {
        let mut blocks = [[0u8; 64]; 6];

        let mut yoff = vbuf.get_offset(0);
        let mut uoff = vbuf.get_offset(1);
        let mut voff = vbuf.get_offset(2);
        let ystride = vbuf.get_stride(0);
        let ustride = vbuf.get_stride(1);
        let vstride = vbuf.get_stride(2);
        let dst = vbuf.get_data_mut().unwrap();

        dc_pred.reset();

        let quant = self.quant;
        let mut mb_pos = 0;
        for _mb_y in 0..self.mb_h {
            for mb_x in 0..self.mb_w {
                let mb_type = self.mb_types[mb_pos];
                let mb = self.get_mb_mut(mb_pos);
                for (i, blk) in mb.coeffs.iter_mut().enumerate() {
                    dc_pred.predict_dc(mb_type, i, blk, false);
                }
                mb.dequant(quant);
                mb.idct();
                let mb = self.get_mb(mb_pos);
                if mb_type.is_intra() {
                    for (dblk, sblk) in blocks.iter_mut().zip(mb.coeffs.iter()) {
                        for (dcoef, &scoef) in dblk.iter_mut().zip(sblk.iter()) {
                            *dcoef = (scoef + 128).max(0).min(255) as u8;
                        }
                    }
                } else {
                    let res_mb = match mb_type.get_ref_id() {
                            0 => unreachable!(),
                            1 => if mb_type != VPMBType::InterFourMV {
                                    &self.inter_mbs[mb_pos].reference
                                } else {
                                    &self.fourmv_mbs[mb_pos].reference
                                },
                            _ => &self.golden_mbs[mb_pos].reference,
                        };

                    for (dblk, (sblk1, sblk2)) in blocks.iter_mut().zip(mb.coeffs.iter().zip(res_mb.iter())) {
                        for (dcoef, (&scoef1, &scoef2)) in dblk.iter_mut().zip(sblk1.iter().zip(sblk2.iter())) {
                            *dcoef = (scoef1 + scoef2).max(0).min(255) as u8;
                        }
                    }
                }

                for i in 0..4 {
                    write_block!(&mut dst[yoff + mb_x * 16 + (i & 1) * 8 + (i >> 1) * 8 * ystride..],
                                 blocks[i], ystride);
                }
                write_block!(&mut dst[uoff + mb_x * 8..], blocks[4], ustride);
                write_block!(&mut dst[voff + mb_x * 8..], blocks[5], vstride);

                dc_pred.next_mb();
                mb_pos += 1;
            }
            yoff += ystride * 16;
            uoff += ustride * 8;
            voff += vstride * 8;
            dc_pred.update_row();
        }
        /*#[cfg(debug_assertions)]
        dump_pgm(&vbuf, "/home/kst/devel/NihAV-rust/assets/test_out/debug.pgm");*/
    }
    pub fn get_mb(&self, mb_pos: usize) -> &ResidueMB {
        let mb_type = self.mb_types[mb_pos];
        match mb_type.get_ref_id() {
            0 => &self.intra_mbs[mb_pos],
            1 => if mb_type != VPMBType::InterFourMV {
                    &self.inter_mbs[mb_pos].residue
                } else {
                    &self.fourmv_mbs[mb_pos].residue
                },
            _ => &self.golden_mbs[mb_pos].residue,
        }
    }
    fn get_mb_mut(&mut self, mb_pos: usize) -> &mut ResidueMB {
        let mb_type = self.mb_types[mb_pos];
        match mb_type.get_ref_id() {
            0 => &mut self.intra_mbs[mb_pos],
            1 => if mb_type != VPMBType::InterFourMV {
                    &mut self.inter_mbs[mb_pos].residue
                } else {
                    &mut self.fourmv_mbs[mb_pos].residue
                },
            _ => &mut self.golden_mbs[mb_pos].residue,
        }
    }
    pub fn prepare_intra_blocks(&mut self) {
        self.intra_mbs.clear();
        self.mb_types.clear();
        for smb in self.src_mbs.iter() {
            let mut dmb = smb.clone();
            dmb.fdct();
            for blk in dmb.coeffs.iter_mut() {
                blk[0] -= 4096;
            }
            dmb.quant(self.quant);
            self.mb_types.push(VPMBType::Intra);
            self.intra_mbs.push(dmb);
        }
    }
    pub fn prepare_inter_blocks(&mut self, golden: bool) {
        let inter_mbs = if !golden { &mut self.inter_mbs } else { &mut self.golden_mbs };
        for (mb_idx, mb) in inter_mbs.iter_mut().enumerate() {
            mb.residue.fdct();
            mb.residue.quant(self.quant);
            self.mb_types[mb_idx] = VPMBType::InterMV;
        }
    }
    pub fn estimate_mvs(&mut self, ref_frame: NAVideoBufferRef<u8>, mc_buf: NAVideoBufferRef<u8>, golden: bool) {
        let loop_thr = i16::from(VP56_FILTER_LIMITS[self.quant]);

        let inter_mbs = if !golden { &mut self.inter_mbs } else { &mut self.golden_mbs };

        if inter_mbs.is_empty() {
            for _ in 0..self.mb_w * self.mb_h {
                inter_mbs.push(InterMB::new());
            }
        }

        let mut cur_blk = [[0u8; 64]; 6];

        let mut mv_est = MVEstimator::new(ref_frame, mc_buf, loop_thr, self.me_range);

        let mut mv_search = self.me_mode.create_search();

        let mut mb_pos = 0;
        for (mb_y, row) in inter_mbs.chunks_mut(self.mb_w).enumerate() {
            for (mb_x, mb) in row.iter_mut().enumerate() {
                self.src_mbs[mb_pos].fill(&mut cur_blk);

                let (best_mv, _best_dist) = mv_search.search_mb(&mut mv_est, &cur_blk, mb_x, mb_y);
                mb.mv[3] = best_mv;

                for i in 0..4 {
                    mv_est.mc_block(i, 0, mb_x * 16 + (i & 1) * 8, mb_y * 16 + (i >> 1) * 8, best_mv);
                    sub_blk(&mut mb.residue.coeffs[i], &cur_blk[i], &mv_est.ref_blk[i]);
                }
                for plane in 1..3 {
                    mv_est.mc_block(plane + 3, plane, mb_x * 8, mb_y * 8, best_mv);
                    sub_blk(&mut mb.residue.coeffs[plane + 3], &cur_blk[plane + 3], &mv_est.ref_blk[plane + 3]);
                }

                for (dblk, sblk) in mb.reference.iter_mut().zip(mv_est.ref_blk.iter()) {
                    for (dst, &src) in dblk.iter_mut().zip(sblk.iter()) {
                        *dst = i16::from(src);
                    }
                }
                mb_pos += 1;
            }
        }
    }
    fn estimate_fourmv(&mut self, ref_frame: NAVideoBufferRef<u8>, mc_buf: NAVideoBufferRef<u8>, mb_x: usize, mb_y: usize) -> bool {
        let loop_thr = i16::from(VP56_FILTER_LIMITS[self.quant]);

        if self.fourmv_mbs.is_empty() {
            for _ in 0..self.mb_w * self.mb_h {
                self.fourmv_mbs.push(InterMB::new());
            }
        }
        if self.fmv_sub.is_empty() {
            self.fmv_sub.resize(self.mb_w * self.mb_h, [VPMBType::Intra; 4]);
        }

        let mb_pos = mb_x + mb_y * self.mb_w;
        let mb = &mut self.fourmv_mbs[mb_pos];

        let mut cur_blk = [[0u8; 64]; 6];
        self.src_mbs[mb_pos].fill(&mut cur_blk);

        let mut mv_est = MVEstimator::new(ref_frame, mc_buf, loop_thr, self.me_range);

        let mut mv_search = self.me_mode.create_search();

        for i in 0..4 {
            let xpos = mb_x * 16 + (i &  1) * 8;
            let ypos = mb_y * 16 + (i >> 1) * 8;
            let (best_mv, _best_dist) = mv_search.search_blk(&mut mv_est, &cur_blk[i], xpos, ypos);
            mb.mv[i] = best_mv;
        }
        let mvsum = mb.mv[0] + mb.mv[1] + mb.mv[2] + mb.mv[3];
        let chroma_mv = MV{ x: mvsum.x / 4, y: mvsum.y / 4};

        for (i, blk) in mb.residue.coeffs[..4].iter_mut().enumerate() {
            let xpos = mb_x * 16 + (i &  1) * 8;
            let ypos = mb_y * 16 + (i >> 1) * 8;
            mv_est.mc_block(i, 0, xpos, ypos, mb.mv[i]);
            sub_blk(blk, &cur_blk[i], &mv_est.ref_blk[i]);
        }
        for plane in 1..3 {
            mv_est.mc_block(plane + 3, plane, mb_x * 8, mb_y * 8, chroma_mv);
            sub_blk(&mut mb.residue.coeffs[plane + 3], &cur_blk[plane + 3], &mv_est.ref_blk[plane + 3]);
        }

        for (dblk, sblk) in mb.reference.iter_mut().zip(mv_est.ref_blk.iter()) {
            for (dst, &src) in dblk.iter_mut().zip(sblk.iter()) {
                *dst = i16::from(src);
            }
        }

        (mb.mv[0] != mb.mv[1]) || (mb.mv[0] != mb.mv[2]) || (mb.mv[0] != mb.mv[3])
    }
    pub fn select_inter_blocks(&mut self, ref_frame: NAVideoBufferRef<u8>, mc_buf: NAVideoBufferRef<u8>, has_golden_frame: bool, lambda: f32) {
        let mut tmp_mb = ResidueMB::new();
        for mb_idx in 0..self.mb_w * self.mb_h {
            tmp_mb.dequant_from(&self.intra_mbs[mb_idx], self.quant);
            tmp_mb.idct();
            for blk in tmp_mb.coeffs.iter_mut() {
                for coef in blk.iter_mut() {
                    *coef = (*coef + 128).max(0).min(255);
                }
            }
            let intra_dist = calc_mb_dist(&self.src_mbs[mb_idx], &tmp_mb);
            let intra_nits = estimate_intra_mb_nits(&self.intra_mbs[mb_idx].coeffs, self.quant);
            let intra_cost = (intra_dist as f32) + lambda * (intra_nits as f32);

            tmp_mb.dequant_from(&self.inter_mbs[mb_idx].residue, self.quant);
            tmp_mb.idct();
            for (blk, res) in tmp_mb.coeffs.iter_mut().zip(self.inter_mbs[mb_idx].reference.iter()) {
                for (coef, add) in blk.iter_mut().zip(res.iter()) {
                    *coef = (*coef + add).max(0).min(255);
                }
            }
            let inter_dist = calc_mb_dist(&self.src_mbs[mb_idx], &tmp_mb);
            let mut inter_nits = estimate_inter_mb_nits(&self.inter_mbs[mb_idx], self.quant, false);
            if self.inter_mbs[mb_idx].mv[3] != ZERO_MV {
                inter_nits += estimate_mv_nits(self.inter_mbs[mb_idx].mv[3]);
            }
            let mut inter_cost = (inter_dist as f32) + lambda * (inter_nits as f32);

            if inter_cost < intra_cost {
                self.mb_types[mb_idx] = VPMBType::InterMV;

                if inter_dist > 512 {
                    self.estimate_fourmv(ref_frame.clone(), mc_buf.clone(), mb_idx % self.mb_w, mb_idx / self.mb_w);
                    self.fourmv_mbs[mb_idx].residue.fdct();
                    self.fourmv_mbs[mb_idx].residue.quant(self.quant);

                    tmp_mb.dequant_from(&self.fourmv_mbs[mb_idx].residue, self.quant);
                    tmp_mb.idct();
                    for (blk, res) in tmp_mb.coeffs.iter_mut().zip(self.fourmv_mbs[mb_idx].reference.iter()) {
                        for (coef, add) in blk.iter_mut().zip(res.iter()) {
                            *coef = (*coef + add).max(0).min(255);
                        }
                    }
                    let fourmv_dist = calc_mb_dist(&self.src_mbs[mb_idx], &tmp_mb);
                    let fourmv_nits = estimate_inter_mb_nits(&self.fourmv_mbs[mb_idx], self.quant, true);
                    let fourmv_cost = (fourmv_dist as f32) + lambda * (fourmv_nits as f32);
                    if fourmv_cost < inter_cost {
                        self.mb_types[mb_idx] = VPMBType::InterFourMV;
                        inter_cost = fourmv_cost;
                    }
                }
            }

            if has_golden_frame {
                tmp_mb.dequant_from(&self.golden_mbs[mb_idx].residue, self.quant);
                tmp_mb.idct();
                for (blk, res) in tmp_mb.coeffs.iter_mut().zip(self.golden_mbs[mb_idx].reference.iter()) {
                    for (coef, add) in blk.iter_mut().zip(res.iter()) {
                        *coef = (*coef + add).max(0).min(255);
                    }
                }
                let golden_dist = calc_mb_dist(&self.src_mbs[mb_idx], &tmp_mb);
                let golden_nits = estimate_inter_mb_nits(&self.golden_mbs[mb_idx], self.quant, false);
                let golden_cost = (golden_dist as f32) + lambda * (golden_nits as f32);

                if (self.mb_types[mb_idx].is_intra() && golden_cost < intra_cost) ||
                    (!self.mb_types[mb_idx].is_intra() && golden_cost < inter_cost) {
                    self.mb_types[mb_idx] = VPMBType::GoldenMV;
                }
            }
        }
    }
    fn motion_est_mb(src_mb: &ResidueMB, cur_blk: &mut [[u8; 64]; 6], mb: &mut InterMB, mv_search: &mut Box<dyn MVSearch+Send>, mv_est: &mut MVEstimator, mb_x: usize, mb_y: usize) {
        src_mb.fill(cur_blk);
        let (best_mv, _best_dist) = mv_search.search_mb(mv_est, cur_blk, mb_x, mb_y);
        mb.mv[3] = best_mv;

        for i in 0..4 {
            mv_est.mc_block(i, 0, mb_x * 16 + (i & 1) * 8, mb_y * 16 + (i >> 1) * 8, best_mv);
            sub_blk(&mut mb.residue.coeffs[i], &cur_blk[i], &mv_est.ref_blk[i]);
        }
        for plane in 1..3 {
            mv_est.mc_block(plane + 3, plane, mb_x * 8, mb_y * 8, best_mv);
            sub_blk(&mut mb.residue.coeffs[plane + 3], &cur_blk[plane + 3], &mv_est.ref_blk[plane + 3]);
        }

        for (dblk, sblk) in mb.reference.iter_mut().zip(mv_est.ref_blk.iter()) {
            for (dst, &src) in dblk.iter_mut().zip(sblk.iter()) {
                *dst = i16::from(src);
            }
        }
    }
    pub fn select_inter_blocks_fast(&mut self, ref_frame: NAVideoBufferRef<u8>, gold_frame: Option<NAVideoBufferRef<u8>>, mc_buf: NAVideoBufferRef<u8>, lambda: f32) {
        let loop_thr = i16::from(VP56_FILTER_LIMITS[self.quant]);

        if self.inter_mbs.is_empty() {
            for _ in 0..self.mb_w * self.mb_h {
                self.inter_mbs.push(InterMB::new());
            }
        }
        if self.golden_mbs.is_empty() {
            for _ in 0..self.mb_w * self.mb_h {
                self.golden_mbs.push(InterMB::new());
            }
        }

        let mut cur_blk = [[0u8; 64]; 6];

        let mut mv_est = MVEstimator::new(ref_frame.clone(), mc_buf.clone(), loop_thr, self.me_range);
        let mut mv_est_g = if let Some(gold_frm) = gold_frame {
                Some(MVEstimator::new(gold_frm, mc_buf.clone(), loop_thr, self.me_range))
            } else {
                None
            };

        let mut mv_search = self.me_mode.create_search();

        let mut tmp_mb = ResidueMB::new();

        let mut mb_idx = 0;
        for mb_y in 0..self.mb_h {
            for mb_x in 0..self.mb_w {
                let smb = &self.src_mbs[mb_idx];

                let inter_mb = &mut self.inter_mbs[mb_idx];
                Self::motion_est_mb(smb, &mut cur_blk, inter_mb, &mut mv_search, &mut mv_est, mb_x, mb_y);
                inter_mb.residue.fdct();
                inter_mb.residue.quant(self.quant);
                self.mb_types[mb_idx] = VPMBType::InterMV;

                tmp_mb.dequant_from(&inter_mb.residue, self.quant);
                tmp_mb.idct();
                for (blk, res) in tmp_mb.coeffs.iter_mut().zip(inter_mb.reference.iter()) {
                    for (coef, add) in blk.iter_mut().zip(res.iter()) {
                        *coef = (*coef + add).max(0).min(255);
                    }
                }
                let mut best_dist = calc_mb_dist(smb, &tmp_mb);
                let mut inter_nits = estimate_inter_mb_nits(inter_mb, self.quant, false);
                if inter_mb.mv[3] != ZERO_MV {
                    inter_nits += estimate_mv_nits(inter_mb.mv[3]);
                }
                let mut best_cost = (best_dist as f32) + lambda * (inter_nits as f32);
                if best_dist > 512 {
                    self.estimate_fourmv(ref_frame.clone(), mc_buf.clone(), mb_idx % self.mb_w, mb_idx / self.mb_w);
                    self.fourmv_mbs[mb_idx].residue.fdct();
                    self.fourmv_mbs[mb_idx].residue.quant(self.quant);

                    let smb = &self.src_mbs[mb_idx];
                    tmp_mb.dequant_from(&self.fourmv_mbs[mb_idx].residue, self.quant);
                    tmp_mb.idct();
                    for (blk, res) in tmp_mb.coeffs.iter_mut().zip(self.fourmv_mbs[mb_idx].reference.iter()) {
                        for (coef, add) in blk.iter_mut().zip(res.iter()) {
                            *coef = (*coef + add).max(0).min(255);
                        }
                    }
                    let fourmv_dist = calc_mb_dist(smb, &tmp_mb);
                    let fourmv_nits = estimate_inter_mb_nits(&self.fourmv_mbs[mb_idx], self.quant, true);
                    let fourmv_cost = (fourmv_dist as f32) + lambda * (fourmv_nits as f32);
                    if fourmv_cost < best_cost {
                        self.mb_types[mb_idx] = VPMBType::InterFourMV;
                        best_cost = fourmv_cost;
                        best_dist = fourmv_dist;
                    }
                }
                let smb = &self.src_mbs[mb_idx];
                if best_dist > 512 {
                    if let Some(ref mut mve_gold) = mv_est_g {
                        let gold_mb = &mut self.golden_mbs[mb_idx];
                        Self::motion_est_mb(smb, &mut cur_blk, gold_mb, &mut mv_search, mve_gold, mb_x, mb_y);
                        gold_mb.residue.fdct();
                        gold_mb.residue.quant(self.quant);

                        tmp_mb.dequant_from(&gold_mb.residue, self.quant);
                        tmp_mb.idct();
                        for (blk, res) in tmp_mb.coeffs.iter_mut().zip(gold_mb.reference.iter()) {
                            for (coef, add) in blk.iter_mut().zip(res.iter()) {
                                *coef = (*coef + add).max(0).min(255);
                            }
                        }
                        let golden_dist = calc_mb_dist(smb, &tmp_mb);
                        let golden_nits = estimate_inter_mb_nits(gold_mb, self.quant, false);
                        let golden_cost = (golden_dist as f32) + lambda * (golden_nits as f32);
                        if golden_cost < best_cost {
                            self.mb_types[mb_idx] = VPMBType::GoldenMV;
                            best_cost = golden_cost;
                            best_dist = golden_dist;
                        }
                    }
                }
                if best_dist > 512 {
                    let intra_mb = &mut self.intra_mbs[mb_idx];
                    *intra_mb = smb.clone();
                    intra_mb.fdct();
                    for blk in intra_mb.coeffs.iter_mut() {
                        blk[0] -= 4096;
                    }
                    intra_mb.quant(self.quant);

                    tmp_mb.dequant_from(intra_mb, self.quant);
                    tmp_mb.idct();
                    for blk in tmp_mb.coeffs.iter_mut() {
                        for coef in blk.iter_mut() {
                            *coef = (*coef + 128).max(0).min(255);
                        }
                    }
                    let intra_dist = calc_mb_dist(smb, &tmp_mb);
                    let intra_nits = estimate_intra_mb_nits(&intra_mb.coeffs, self.quant);
                    let intra_cost = (intra_dist as f32) + lambda * (intra_nits as f32);
                    if intra_cost < best_cost {
                        self.mb_types[mb_idx] = VPMBType::Intra;
                    }
                }

                mb_idx += 1;
            }
        }
    }
    pub fn decide_frame_type(&self) -> (bool, bool) {
        let mut intra_count = 0usize;
        let mut non_intra   = 0usize;
        for mb_type in self.mb_types.iter() {
            if mb_type.is_intra() {
                intra_count += 1;
            } else {
                non_intra += 1;
            }
        }
        (intra_count > non_intra * 3, intra_count > non_intra)
    }
    fn find_mv_pred(&self, mb_x: usize, mb_y: usize, ref_id: u8) -> (usize, MV, MV, MV) {
        const CAND_POS: [(i8, i8); 12] = [
            (-1,  0), ( 0, -1),
            (-1, -1), (-1,  1),
            (-2,  0), ( 0, -2),
            (-1, -2), (-2, -1),
            (-2,  1), (-1,  2),
            (-2, -2), (-2,  2)
        ];

        let mut nearest_mv = ZERO_MV;
        let mut near_mv = ZERO_MV;
        let mut pred_mv = ZERO_MV;
        let mut num_mv: usize = 0;

        for (i, (yoff, xoff)) in CAND_POS.iter().enumerate() {
            let cx = (mb_x as isize) + (*xoff as isize);
            let cy = (mb_y as isize) + (*yoff as isize);
            if (cx < 0) || (cy < 0) {
                continue;
            }
            let cx = cx as usize;
            let cy = cy as usize;
            if (cx >= self.mb_w) || (cy >= self.mb_h) {
                continue;
            }
            let mb_pos = cx + cy * self.mb_w;
            let mv = match self.mb_types[mb_pos].get_ref_id() {
                    0 => ZERO_MV,
                    1 => if self.mb_types[mb_pos] != VPMBType::InterFourMV {
                            self.inter_mbs[mb_pos].mv[3]
                        } else {
                            self.fourmv_mbs[mb_pos].mv[3]
                        },
                    _ => self.golden_mbs[mb_pos].mv[3],
                };
            if (self.mb_types[mb_pos].get_ref_id() != ref_id) || (mv == ZERO_MV) {
                continue;
            }
            if num_mv == 0 {
                nearest_mv = mv;
                num_mv += 1;
                if i < 2 {
                    pred_mv = mv;
                }
            } else if mv != nearest_mv {
                near_mv = mv;
                num_mv += 1;
                break;
            }
        }

        (num_mv, nearest_mv, near_mv, pred_mv)
    }
    pub fn predict_mvs(&mut self) {
        let mut mb_idx = 0;
        self.num_mv.clear();
        if self.coded_mv.is_empty() {
            self.coded_mv.resize(self.mb_w * self.mb_h, [ZERO_MV; 4]);
        }
        for mb_y in 0..self.mb_h {
            for mb_x in 0..self.mb_w {
                let (num_mv, nearest_mv, near_mv, pred_mv) = self.find_mv_pred(mb_x, mb_y, VP_REF_INTER);
                let mb_type = self.mb_types[mb_idx];
                self.num_mv.push(num_mv as u8);
                let golden = mb_type.get_ref_id() == VP_REF_GOLDEN;
                let mv = if !golden { self.inter_mbs[mb_idx].mv[3] } else { self.golden_mbs[mb_idx].mv[3] };

                let mb_type = if mb_type == VPMBType::Intra {
                        VPMBType::Intra
                    } else if mb_type == VPMBType::InterFourMV {
                        for i in 0..4 {
                            let mv = self.fourmv_mbs[mb_idx].mv[i];
                            self.coded_mv[mb_idx][i] = ZERO_MV;
                            if mv == ZERO_MV {
                                self.fmv_sub[mb_idx][i] = VPMBType::InterNoMV;
                            } else {
                                self.fmv_sub[mb_idx][i] = match num_mv {
                                        0 => {
                                            self.coded_mv[mb_idx][i] = mv - pred_mv;
                                            VPMBType::InterMV
                                        },
                                        1 => {
                                            if nearest_mv == mv {
                                                VPMBType::InterNearest
                                            } else {
                                                self.coded_mv[mb_idx][i] = mv - pred_mv;
                                                VPMBType::InterMV
                                            }
                                        },
                                        _ => {
                                            if nearest_mv == mv {
                                                VPMBType::InterNearest
                                            } else if near_mv == mv {
                                                VPMBType::InterNear
                                            } else {
                                                self.coded_mv[mb_idx][i] = mv - pred_mv;
                                                VPMBType::InterMV
                                            }
                                        },
                                    };
                            }
                        }
                        VPMBType::InterFourMV
                    } else if mv == ZERO_MV {
                        if !golden {
                            VPMBType::InterNoMV
                        } else {
                            VPMBType::GoldenNoMV
                        }
                    } else if mb_type.get_ref_id() == VP_REF_INTER {
                        self.coded_mv[mb_idx][3] = mv;
                        match num_mv {
                            0 => VPMBType::InterMV,
                            1 => {
                                if nearest_mv == mv {
                                    VPMBType::InterNearest
                                } else {
                                    self.coded_mv[mb_idx][3] = mv - pred_mv;
                                    VPMBType::InterMV
                                }
                            },
                            _ => {
                                if nearest_mv == mv {
                                    VPMBType::InterNearest
                                } else if near_mv == mv {
                                    VPMBType::InterNear
                                } else {
                                    self.coded_mv[mb_idx][3] = mv - pred_mv;
                                    VPMBType::InterMV
                                }
                            },
                        }
                    } else {
                        let (num_mv, nearest_mv, near_mv, pred_mv) = self.find_mv_pred(mb_x, mb_y, VP_REF_GOLDEN);
                        self.coded_mv[mb_idx][3] = ZERO_MV;
                        match num_mv {
                            0 => {
                                self.coded_mv[mb_idx][3] = mv - pred_mv;
                                VPMBType::GoldenMV
                            },
                            1 => {
                                if nearest_mv == mv {
                                    VPMBType::GoldenNearest
                                } else {
                                    self.coded_mv[mb_idx][3] = mv - pred_mv;
                                    VPMBType::GoldenMV
                                }
                            },
                            _ => {
                                if nearest_mv == mv {
                                    VPMBType::GoldenNearest
                                } else if near_mv == mv {
                                    VPMBType::GoldenNear
                                } else {
                                    self.coded_mv[mb_idx][3] = mv - pred_mv;
                                    VPMBType::GoldenMV
                                }
                            },
                        }
                    };
                self.mb_types[mb_idx] = mb_type;
                mb_idx += 1;
            }
        }
    }
    pub fn apply_dc_prediction(&mut self, dc_pred: &mut VP56DCPred) {
        dc_pred.reset();

        let mut mb_idx = 0;
        for _mb_y in 0..self.mb_h {
            for _mb_x in 0..self.mb_w {
                let mb_type = self.mb_types[mb_idx];
                let mb = self.get_mb_mut(mb_idx);
                for (i, blk) in mb.coeffs.iter_mut().enumerate() {
                    dc_pred.predict_dc(mb_type, i, blk, true);
                }
                dc_pred.next_mb();
                mb_idx += 1;
            }
            dc_pred.update_row();
        }
    }
}
