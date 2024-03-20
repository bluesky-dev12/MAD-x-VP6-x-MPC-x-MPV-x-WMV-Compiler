use nihav_core::frame::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::super::vpcommon::*;
use super::super::vp6dsp::*;
use super::super::vp6data::*;
use super::ResidueMB;
use crate::codecs::vpenc::motion_est::*;
pub use crate::codecs::vpenc::motion_est::MVSearchMode;


const C1S7: i32 = 64277;
const C2S6: i32 = 60547;
const C3S5: i32 = 54491;
const C4S4: i32 = 46341;
const C5S3: i32 = 36410;
const C6S2: i32 = 25080;
const C7S1: i32 = 12785;

fn mul16(a: i32, b: i32) -> i32 {
    let res = a * b;
    (res + if res < 0 { 0xFFFF } else { 0 }) >> 16
}

macro_rules! fdct_step {
    ($s0:expr, $s1:expr, $s2:expr, $s3:expr, $s4:expr, $s5:expr, $s6:expr, $s7:expr,
     $d0:expr, $d1:expr, $d2:expr, $d3:expr, $d4:expr, $d5:expr, $d6:expr, $d7:expr) => {
        let t_g  = i32::from($s0) + i32::from($s7);
        let t_c  = i32::from($s0) - i32::from($s7);
        let t_a  = i32::from($s1) + i32::from($s2);
        let t_h  = i32::from($s1) - i32::from($s2);
        let t_e1 = i32::from($s3) + i32::from($s4);
        let t_d  = i32::from($s3) - i32::from($s4);
        let t_f  = i32::from($s5) + i32::from($s6);
        let t_b  = i32::from($s5) - i32::from($s6);

        let t_b1 = t_h + t_b;
        let t_h  = t_h - t_b;
        let t_a1 = t_a - t_f;
        let t_f  = t_a + t_f;
        let t_e  = t_g + t_e1;
        let t_g  = t_g - t_e1;

        $d2 = (mul16(C2S6, t_g) + mul16(C6S2, t_h)).max(-32768).min(32767) as i16;
        $d6 = (mul16(C6S2, t_g) - mul16(C2S6, t_h)).max(-32768).min(32767) as i16;
        $d0 = mul16(C4S4, t_e + t_f).max(-32768).min(32767) as i16;
        $d4 = mul16(C4S4, t_e - t_f).max(-32768).min(32767) as i16;
        let t_a = t_c + mul16(C4S4, t_a1);
        let t_c = t_c - mul16(C4S4, t_a1);
        let t_b = t_d + mul16(C4S4, t_b1);
        let t_d = t_d - mul16(C4S4, t_b1);
        $d3 = (mul16(C3S5, t_c) - mul16(C5S3, t_d)).max(-32768).min(32767) as i16;
        $d5 = (mul16(C5S3, t_c) + mul16(C3S5, t_d)).max(-32768).min(32767) as i16;
        $d1 = (mul16(C1S7, t_a) + mul16(C7S1, t_b)).max(-32768).min(32767) as i16;
        $d7 = (mul16(C7S1, t_a) - mul16(C1S7, t_b)).max(-32768).min(32767) as i16;
    }
}

#[allow(clippy::erasing_op)]
pub fn vp_fdct(blk: &mut [i16; 64]) {
    for row in blk.chunks_mut(8) {
        fdct_step!(row[0], row[1], row[2], row[3], row[4], row[5], row[6], row[7],
                   row[0], row[1], row[2], row[3], row[4], row[5], row[6], row[7]);
    }
    for i in 0..8 {
        fdct_step!(blk[0 * 8 + i], blk[1 * 8 + i], blk[2 * 8 + i], blk[3 * 8 + i],
                   blk[4 * 8 + i], blk[5 * 8 + i], blk[6 * 8 + i], blk[7 * 8 + i],
                   blk[0 * 8 + i], blk[1 * 8 + i], blk[2 * 8 + i], blk[3 * 8 + i],
                   blk[4 * 8 + i], blk[5 * 8 + i], blk[6 * 8 + i], blk[7 * 8 + i]);
    }
}

pub trait MVSearchModeCreate {
    fn create_search(&self) -> Box<dyn MVSearch + Send>;
}

impl MVSearchModeCreate for MVSearchMode {
    fn create_search(&self) -> Box<dyn MVSearch + Send> {
        match *self {
            MVSearchMode::Full      => Box::new(FullMVSearch::new()),
            MVSearchMode::Diamond   => Box::new(DiaSearch::new()),
            MVSearchMode::Hexagon   => Box::new(HexSearch::new()),
            _ => unreachable!(),
        }
    }
}

const MAX_DIST: u32 = std::u32::MAX;
const DIST_THRESH: u32 = 256;

trait FromPixels {
    fn from_pixels(self) -> Self;
}

impl FromPixels for MV {
    fn from_pixels(self) -> MV {
        MV { x: self.x * 4, y: self.y * 4 }
    }
}

pub trait MVSearch {
    fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_blk: &[[u8; 64]; 6], mb_x: usize, mb_y: usize) -> (MV, u32);
    fn search_blk(&mut self, mv_est: &mut MVEstimator, cur_blk: &[u8; 64], xpos: usize, ypos: usize) -> (MV, u32);
}

pub struct FullMVSearch {}

impl FullMVSearch {
    pub fn new() -> Self { Self{} }
}

impl MVSearch for FullMVSearch {
    fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_blk: &[[u8; 64]; 6], mb_x: usize, mb_y: usize) -> (MV, u32) {
        let mut best_dist = MAX_DIST;
        let mut best_mv = ZERO_MV;

        let mut cur_mv = ZERO_MV;
        for ytry in 0..mv_est.mv_range * 2 + 1 {
            let dy = if (ytry & 1) == 0 { ytry >> 1 } else { -((ytry + 1) >> 1) };
            cur_mv.y = dy * 4;
            for xtry in 0..mv_est.mv_range * 2 + 1 {
                let dx = if (xtry & 1) == 0 { xtry >> 1 } else { -((xtry + 1) >> 1) };
                cur_mv.x = dx * 4;

                let dist = mv_est.sad_mb(cur_blk, mb_x, mb_y, cur_mv, best_dist);

                if dist < best_dist {
                    best_dist = dist;
                    best_mv = cur_mv;
                }
            }
        }
        (best_mv, best_dist)
    }
    fn search_blk(&mut self, mv_est: &mut MVEstimator, cur_blk: &[u8; 64], xpos: usize, ypos: usize) -> (MV, u32) {
        let mut best_dist = MAX_DIST;
        let mut best_mv = ZERO_MV;

        let mut cur_mv = ZERO_MV;
        for ytry in 0..mv_est.mv_range * 2 + 1 {
            let dy = if (ytry & 1) == 0 { ytry >> 1 } else { -((ytry + 1) >> 1) };
            cur_mv.y = dy * 4;
            for xtry in 0..mv_est.mv_range * 2 + 1 {
                let dx = if (xtry & 1) == 0 { xtry >> 1 } else { -((xtry + 1) >> 1) };
                cur_mv.x = dx * 4;

                let dist = mv_est.sad_blk(cur_blk, xpos, ypos, cur_mv, best_dist);

                if dist < best_dist {
                    best_dist = dist;
                    best_mv = cur_mv;
                }
            }
        }
        (best_mv, best_dist)
    }
}

macro_rules! pattern_search {
    ($struct_name: ident, $patterns: expr) => {
        pub struct $struct_name {
            point:  [MV; $patterns.len()],
            dist:   [u32; $patterns.len()],
            steps:  &'static [MV; $patterns.len()],
        }

        impl $struct_name {
            pub fn new() -> Self {
                Self {
                    point:  $patterns,
                    dist:   [MAX_DIST; $patterns.len()],
                    steps:  &$patterns,
                }
            }
            fn reset(&mut self) {
                self.point = $patterns;
                self.dist  = [MAX_DIST; $patterns.len()];
            }
            fn set_new_point(&mut self, start: MV, dist: u32) {
                for (dst, &src) in self.point.iter_mut().zip(self.steps.iter()) {
                    *dst = src + start;
                }
                self.dist = [MAX_DIST; $patterns.len()];
                self.dist[0] = dist;
            }
            fn update(&mut self, step: MV) {
                let mut new_point = self.point;
                let mut new_dist = [MAX_DIST; $patterns.len()];

                for point in new_point.iter_mut() {
                    *point += step;
                }

                for (new_point, new_dist) in new_point.iter_mut().zip(new_dist.iter_mut()) {
                    for (&old_point, &old_dist) in self.point.iter().zip(self.dist.iter()) {
                        if *new_point == old_point {
                            *new_dist = old_dist;
                            break;
                        }
                    }
                }
                self.point = new_point;
                self.dist  = new_dist;
            }
        }

        impl MVSearch for $struct_name {
            fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_blk: &[[u8; 64]; 6], mb_x: usize, mb_y: usize) -> (MV, u32) {
                search_template!(self, mv_est, cur_blk, mb_x, mb_y, sad_mb, DIST_THRESH)
            }
            fn search_blk(&mut self, mv_est: &mut MVEstimator, cur_blk: &[u8; 64], xpos: usize, ypos: usize) -> (MV, u32) {
                search_template!(self, mv_est, cur_blk, xpos, ypos, sad_blk, DIST_THRESH)
            }
        }
    }
}

pattern_search!(DiaSearch, DIA_PATTERN);
pattern_search!(HexSearch, HEX_PATTERN);

pub struct MVEstimator {
    pub ref_blk:        [[u8; 64]; 6],
    mc_buf:         NAVideoBufferRef<u8>,
    ref_frame:      NAVideoBufferRef<u8>,
    adv_profile:    bool,
    bicubic:        bool,
    autosel_pm:     bool,
    mv_thresh:      u8,
    var_thresh:     u16,
    filter_alpha:   usize,
    loop_thr:       i16,
    mv_range:       i16,
}

impl MVEstimator {
    pub fn new(ref_frame: NAVideoBufferRef<u8>, mc_buf: NAVideoBufferRef<u8>, loop_thr: i16, mv_range: i16) -> Self {
        Self {
            ref_blk:        [[0; 64]; 6],
            ref_frame, mc_buf,
            adv_profile:    false,
            bicubic:        false,
            autosel_pm:     false,
            mv_thresh:      0,
            var_thresh:     0,
            filter_alpha:   0,
            loop_thr,
            mv_range,
        }
    }
    pub fn mc_block(&mut self, dst_idx: usize, plane: usize, x: usize, y: usize, mv: MV) {
        let is_luma = (plane != 1) && (plane != 2);
        let (sx, sy, mx, my, msx, msy) = if is_luma {
                (mv.x >> 2, mv.y >> 2, (mv.x & 3) << 1, (mv.y & 3) << 1, mv.x / 4, mv.y / 4)
            } else {
                (mv.x >> 3, mv.y >> 3, mv.x & 7, mv.y & 7, mv.x / 8, mv.y / 8)
            };
        let tmp_blk = self.mc_buf.get_data_mut().unwrap();
        get_block(tmp_blk, 16, self.ref_frame.clone(), plane, x, y, sx, sy);
        if (msx & 7) != 0 {
            let foff = (8 - (sx & 7)) as usize;
            let off = 2 + foff;
            vp31_loop_filter(tmp_blk, off, 1, 16, 12, self.loop_thr);
        }
        if (msy & 7) != 0 {
            let foff = (8 - (sy & 7)) as usize;
            let off = (2 + foff) * 16;
            vp31_loop_filter(tmp_blk, off, 16, 1, 12, self.loop_thr);
        }
        let copy_mode = (mx == 0) && (my == 0);
        let mut bicubic = !copy_mode && is_luma && self.bicubic;
        if is_luma && !copy_mode && self.adv_profile {
            if !self.autosel_pm {
                bicubic = true;
            } else {
                let mv_limit = 1 << (self.mv_thresh + 1);
                if (mv.x.abs() <= mv_limit) && (mv.y.abs() <= mv_limit) {
                    let mut var_off = 16 * 2 + 2;
                    if mv.x < 0 { var_off += 1; }
                    if mv.y < 0 { var_off += 16; }
                    let var = calc_variance(&tmp_blk[var_off..], 16);
                    if var >= self.var_thresh {
                        bicubic = true;
                    }
                }
            }
        }
        let dst = &mut self.ref_blk[dst_idx];
        if copy_mode {
            let src = &tmp_blk[2 * 16 + 2..];
            for (dline, sline) in dst.chunks_mut(8).zip(src.chunks(16)).take(8) {
                dline.copy_from_slice(&sline[..8]);
            }
        } else if bicubic {
            let coeff_h = &VP6_BICUBIC_COEFFS[self.filter_alpha][mx as usize];
            let coeff_v = &VP6_BICUBIC_COEFFS[self.filter_alpha][my as usize];
            mc_bicubic(dst, 8, tmp_blk, 16 * 2 + 2, 16, coeff_h, coeff_v);
        } else {
            mc_bilinear(dst, 8, tmp_blk, 16 * 2 + 2, 16, mx as u16, my as u16);
        }
    }
    fn sad_mb(&mut self, cur_blk: &[[u8; 64]; 6], mb_x: usize, mb_y: usize, cur_mv: MV, best_dist: u32) -> u32 {
        let mut dist = 0;
        for i in 0..4 {
            self.mc_block(i, 0, mb_x * 16 + (i & 1) * 8, mb_y * 16 + (i >> 1) * 8, cur_mv);
            dist += sad(&cur_blk[i], &self.ref_blk[i]);
            if dist > best_dist {
                break;
            }
        }
        if dist <= best_dist {
            for plane in 1..3 {
                self.mc_block(plane + 3, plane, mb_x * 8, mb_y * 8, cur_mv);
                dist += sad(&cur_blk[plane + 3], &self.ref_blk[plane + 3]);
                if dist > best_dist {
                    break;
                }
            }
        }
        dist
    }
    fn sad_blk(&mut self, cur_blk: &[u8; 64], xpos: usize, ypos: usize, cur_mv: MV, _: u32) -> u32 {
        self.mc_block(0, 0, xpos, ypos, cur_mv);
        sad(cur_blk, &self.ref_blk[0])
    }
}

fn sad(src1: &[u8; 64], src2: &[u8; 64]) -> u32 {
    let mut sum = 0;
    for (&p1, &p2) in src1.iter().zip(src2.iter()) {
        sum += (i32::from(p1) - i32::from(p2)).unsigned_abs();
    }
    sum
}

pub fn sub_blk(dst: &mut [i16; 64], src1: &[u8; 64], src2: &[u8; 64]) {
    for (dst, (&p1, &p2)) in dst.iter_mut().zip(src1.iter().zip(src2.iter())) {
        *dst = i16::from(p1) - i16::from(p2);
    }
}

pub fn calc_mb_dist(mb1: &ResidueMB, mb2: &ResidueMB) -> u32 {
    let mut sum = 0;
    for (blk1, blk2) in mb1.coeffs.iter().zip(mb2.coeffs.iter()) {
        for (&c1, &c2) in blk1.iter().zip(blk2.iter()) {
            sum += (i32::from(c1) - i32::from(c2)).unsigned_abs();
        }
    }
    sum
}
