use nihav_core::frame::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::super::vp78dsp::*;
use super::blocks::{SrcBlock, get_block_difference};
use crate::codecs::vpenc::motion_est::*;
pub use crate::codecs::vpenc::motion_est::MVSearchMode;

pub trait MVSearchModeCreate {
    fn create_search(&self) -> Box<dyn MVSearch + Send>;
}

impl MVSearchModeCreate for MVSearchMode {
    fn create_search(&self) -> Box<dyn MVSearch + Send> {
        match *self {
            MVSearchMode::SEA       => Box::new(EliminationSearch::new()),
            MVSearchMode::Diamond   => Box::new(DiaSearch::new()),
            MVSearchMode::Hexagon   => Box::new(HexSearch::new()),
            MVSearchMode::EPZS      => Box::new(EPZSearch::new()),
            _ => unreachable!(),
        }
    }
}

const MAX_DIST: u32 = std::u32::MAX;
const DIST_THRESH: u32 = 256;
pub const LARGE_BLK8_DIST: u32 = 256;

trait FromPixels {
    fn from_pixels(self) -> Self;
}

impl FromPixels for MV {
    fn from_pixels(self) -> MV {
        MV { x: self.x * 8, y: self.y * 8 }
    }
}

pub trait MVSearch {
    fn preinit(&mut self, mv_est: &MVEstimator);
    fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_mb: &SrcBlock, mb_x: usize, mb_y: usize) -> (MV, u32);
    fn search_blk8(&mut self, mv_est: &mut MVEstimator, ref_blk: &[u8; 64], xpos: usize, ypos: usize, cand_mvs: &[MV]) -> (MV, u32);
    fn search_blk4(&mut self, mv_est: &mut MVEstimator, ref_blk: &[u8; 16], xpos: usize, ypos: usize, cand_mvs: &[MV]) -> (MV, u32);
}

#[derive(Default)]
pub struct EliminationSearch {
    msa:    [Vec<u16>; 3],
    stride: [usize; 3],
}

impl EliminationSearch {
    const BLOCK_SIZE: usize = 4;
    pub fn new() -> Self { Self::default() }
    fn get_rdist(&self, xpos: usize, ypos: usize, bavg: &[u16; 3]) -> i32 {
        let luma_off = xpos + ypos * self.stride[0];
        let chroma_off = (xpos / 2) + (ypos / 2) * self.stride[1];

        let mut luma_avg = 0;
        for row in self.msa[0][luma_off..].chunks(self.stride[0]).take(16).step_by(Self::BLOCK_SIZE) {
            for &el in row.iter().take(16).step_by(Self::BLOCK_SIZE) {
                luma_avg += el;
            }
        }
        let mut chroma_avg = [0; 2];
        for chroma in 0..1 {
            for row in self.msa[chroma + 1][chroma_off..].chunks(self.stride[1]).take(8).step_by(Self::BLOCK_SIZE) {
                for &el in row.iter().take(8).step_by(Self::BLOCK_SIZE) {
                    chroma_avg[chroma] += el;
                }
            }
        }

        (i32::from(bavg[0]) - i32::from(luma_avg)).abs() +
        (i32::from(bavg[1]) - i32::from(chroma_avg[0])).abs() +
        (i32::from(bavg[2]) - i32::from(chroma_avg[1])).abs()
    }
}

impl MVSearch for EliminationSearch {
    fn preinit(&mut self, mv_est: &MVEstimator) {
        let data = mv_est.ref_frame.get_data();
        for (plane, msa) in self.msa.iter_mut().enumerate() {
            let (width, height) = mv_est.ref_frame.get_dimensions(plane);
            self.stride[plane] = width + 1 - Self::BLOCK_SIZE;
            msa.clear();
            msa.reserve(self.stride[plane] * (height + 1 - Self::BLOCK_SIZE));

            let mut off = mv_est.ref_frame.get_offset(plane);
            let stride = mv_est.ref_frame.get_stride(plane);
            for _ in 0..(height + 1 - Self::BLOCK_SIZE) {
                for x in 0..(width + 1 - Self::BLOCK_SIZE) {
                    let mut sum = 0;
                    for j in 0..Self::BLOCK_SIZE {
                        for i in 0..Self::BLOCK_SIZE {
                            sum += u16::from(data[off + x + i + j * stride]);
                        }
                    }
                    msa.push(sum);
                }
                off += stride;
            }
        }
    }
    fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_mb: &SrcBlock, mb_x: usize, mb_y: usize) -> (MV, u32) {
        let mut best_dist = MAX_DIST;
        let mut best_mv = ZERO_MV;

        let mut cur_mv = ZERO_MV;

        let (width, height) = mv_est.ref_frame.get_dimensions(0);
        let mut bavg = [0; 3];
        for blk in cur_mb.luma_blocks() {
            bavg[0] += blk.iter().fold(0u16, |acc, &x| acc + u16::from(x));
        }
        for chroma in 0..2 {
            for blk in cur_mb.chroma_blocks(chroma) {
                bavg[chroma + 1] += blk.iter().fold(0u16, |acc, &x| acc + u16::from(x));
            }
        }
        let mut rough_dist = std::i32::MAX;
        for ytry in 0..mv_est.mv_range * 2 + 1 {
            let dy = if (ytry & 1) == 0 { ytry >> 1 } else { -((ytry + 1) >> 1) };
            let ypos = (mb_y as isize) * 16 + (dy as isize);
            if ypos < 0 || (ypos + 16) > (height as isize) {
                continue;
            }
            let ypos = ypos as usize;
            cur_mv.y = dy * 8;
            for xtry in 0..mv_est.mv_range * 2 + 1 {
                let dx = if (xtry & 1) == 0 { xtry >> 1 } else { -((xtry + 1) >> 1) };
                let xpos = (mb_x as isize) * 16 + (dx as isize);
                if xpos < 0 || (xpos + 16) > (width as isize) {
                    continue;
                }
                let xpos = xpos as usize;

                let rdist = self.get_rdist(xpos, ypos, &bavg);
                if rdist > rough_dist {
                    continue;
                }
                rough_dist = rdist;

                cur_mv.x = dx * 8;

                let dist = mv_est.sad_mb(cur_mb, mb_x, mb_y, cur_mv, best_dist);

                if dist < best_dist {
                    best_dist = dist;
                    best_mv = cur_mv;
                    if dist <= DIST_THRESH {
                        return (best_mv, best_dist);
                    }
                }
            }
        }
        (best_mv, best_dist)
    }
    fn search_blk8(&mut self, mv_est: &mut MVEstimator, ref_blk: &[u8; 64], xpos_orig: usize, ypos_orig: usize, _cand_mvs: &[MV]) -> (MV, u32) {
        let mut best_dist = MAX_DIST;
        let mut best_mv = ZERO_MV;

        let mut cur_mv = ZERO_MV;

        let (width, height) = mv_est.ref_frame.get_dimensions(0);
        let bavg = ref_blk.iter().fold(0u16, |acc, &x| acc + u16::from(x));

        let mut rough_dist = std::i32::MAX;
        for ytry in 0..mv_est.mv_range * 2 + 1 {
            let dy = if (ytry & 1) == 0 { ytry >> 1 } else { -((ytry + 1) >> 1) };
            let ypos = (ypos_orig as isize) + (dy as isize);
            if ypos < 0 || (ypos + 8) > (height as isize) {
                continue;
            }
            let ypos = ypos as usize;
            cur_mv.y = dy * 8;
            for xtry in 0..mv_est.mv_range * 2 + 1 {
                let dx = if (xtry & 1) == 0 { xtry >> 1 } else { -((xtry + 1) >> 1) };
                let xpos = (xpos_orig as isize) + (dx as isize);
                if xpos < 0 || (xpos + 8) > (width as isize) {
                    continue;
                }
                let xpos = xpos as usize;

                let luma_off = xpos + ypos * self.stride[0];
                let mut cur_avg = 0;
                for row in self.msa[0][luma_off..].chunks(self.stride[0]).take(8).step_by(Self::BLOCK_SIZE) {
                    for &el in row.iter().take(8).step_by(Self::BLOCK_SIZE) {
                        cur_avg += el;
                    }
                }

                let rdist = (i32::from(cur_avg) - i32::from(bavg)).abs();
                if rdist > rough_dist {
                    continue;
                }
                rough_dist = rdist;

                cur_mv.x = dx * 8;

                let dist = mv_est.sad_blk8(ref_blk, xpos_orig, ypos_orig, cur_mv, best_dist);

                if dist < best_dist {
                    best_dist = dist;
                    best_mv = cur_mv;
                    if dist <= DIST_THRESH / 4 {
                        return (best_mv, best_dist);
                    }
                }
            }
        }
        (best_mv, best_dist)
    }
    fn search_blk4(&mut self, mv_est: &mut MVEstimator, ref_blk: &[u8; 16], xpos_orig: usize, ypos_orig: usize, _cand_mvs: &[MV]) -> (MV, u32) {
        let mut best_dist = MAX_DIST;
        let mut best_mv = ZERO_MV;

        let mut cur_mv = ZERO_MV;

        let (width, height) = mv_est.ref_frame.get_dimensions(0);
        let bavg = ref_blk.iter().fold(0u16, |acc, &x| acc + u16::from(x));

        let mut rough_dist = std::i32::MAX;
        for ytry in 0..mv_est.mv_range * 2 + 1 {
            let dy = if (ytry & 1) == 0 { ytry >> 1 } else { -((ytry + 1) >> 1) };
            let ypos = (ypos_orig as isize) + (dy as isize);
            if ypos < 0 || (ypos + 4) > (height as isize) {
                continue;
            }
            let ypos = ypos as usize;
            cur_mv.y = dy * 8;
            for xtry in 0..mv_est.mv_range * 2 + 1 {
                let dx = if (xtry & 1) == 0 { xtry >> 1 } else { -((xtry + 1) >> 1) };
                let xpos = (xpos_orig as isize) + (dx as isize);
                if xpos < 0 || (xpos + 4) > (width as isize) {
                    continue;
                }
                let xpos = xpos as usize;

                let luma_off = xpos + ypos * self.stride[0];
                let cur_avg = self.msa[0][luma_off];

                let rdist = (i32::from(cur_avg) - i32::from(bavg)).abs();
                if rdist > rough_dist {
                    continue;
                }
                rough_dist = rdist;

                cur_mv.x = dx * 8;

                let dist = mv_est.sad_blk4(ref_blk, xpos_orig, ypos_orig, cur_mv, best_dist);

                if dist < best_dist {
                    best_dist = dist;
                    best_mv = cur_mv;
                    if dist <= DIST_THRESH / 16 {
                        return (best_mv, best_dist);
                    }
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
            fn preinit(&mut self, _mv_est: &MVEstimator) {}
            fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_mb: &SrcBlock, mb_x: usize, mb_y: usize) -> (MV, u32) {
                search_template!(self, mv_est, cur_mb, mb_x, mb_y, sad_mb, DIST_THRESH)
            }
            fn search_blk8(&mut self, mv_est: &mut MVEstimator, ref_blk: &[u8; 64], xpos: usize, ypos: usize, _cand_mvs: &[MV]) -> (MV, u32) {
                search_template!(self, mv_est, ref_blk, xpos, ypos, sad_blk8, DIST_THRESH / 4)
            }
            fn search_blk4(&mut self, mv_est: &mut MVEstimator, ref_blk: &[u8; 16], xpos: usize, ypos: usize, _cand_mvs: &[MV]) -> (MV, u32) {
                search_template!(self, mv_est, ref_blk, xpos, ypos, sad_blk4, DIST_THRESH / 16)
            }
        }
    }
}

pattern_search!(DiaSearch, DIA_PATTERN);
pattern_search!(HexSearch, HEX_PATTERN);

pub struct EPZSearch {
    point:  [MV; DIA_PATTERN.len()],
    dist:   [u32; DIA_PATTERN.len()],
    steps:  &'static [MV; DIA_PATTERN.len()],
}

impl EPZSearch {
    pub fn new() -> Self {
        Self {
            point:  DIA_PATTERN,
            dist:   [MAX_DIST; DIA_PATTERN.len()],
            steps:  &DIA_PATTERN,
        }
    }
    fn reset(&mut self) {
        self.point = DIA_PATTERN;
        self.dist  = [MAX_DIST; DIA_PATTERN.len()];
    }
    fn set_new_point(&mut self, start: MV, dist: u32) {
        for (dst, &src) in self.point.iter_mut().zip(self.steps.iter()) {
            *dst = src + start;
        }
        self.dist = [MAX_DIST; DIA_PATTERN.len()];
        self.dist[0] = dist;
    }
    fn update(&mut self, step: MV) {
        let mut new_point = self.point;
        let mut new_dist = [MAX_DIST; DIA_PATTERN.len()];

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

impl MVSearch for EPZSearch {
    fn preinit(&mut self, _mv_est: &MVEstimator) {}
    fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_mb: &SrcBlock, mb_x: usize, mb_y: usize) -> (MV, u32) {
        search_template!(self, mv_est, cur_mb, mb_x, mb_y, sad_mb, DIST_THRESH)
    }
    fn search_blk8(&mut self, mv_est: &mut MVEstimator, ref_blk: &[u8; 64], xpos: usize, ypos: usize, cand_mvs: &[MV]) -> (MV, u32) {
        let mut best_mv = ZERO_MV;
        let zero_dist = mv_est.sad_blk8(ref_blk, xpos, ypos, best_mv, MAX_DIST);
        let mut best_dist = zero_dist;
        if best_dist > DIST_THRESH {
            for &cmv in cand_mvs[1..].iter() {
                let dist = mv_est.sad_blk8(ref_blk, xpos, ypos, cmv, best_dist);
                if dist < best_dist {
                    best_dist = dist;
                    best_mv   = cmv;
                    if best_dist <= DIST_THRESH {
                        break;
                    }
                }
            }
            if best_dist > DIST_THRESH {
                return search_template!(self, mv_est, ref_blk, xpos, ypos, sad_blk8, DIST_THRESH / 4, best_mv, best_dist, false);
            }
        }
        (best_mv, best_dist)
    }
    fn search_blk4(&mut self, mv_est: &mut MVEstimator, ref_blk: &[u8; 16], xpos: usize, ypos: usize, cand_mvs: &[MV]) -> (MV, u32) {
        let mut best_mv = ZERO_MV;
        let zero_dist = mv_est.sad_blk4(ref_blk, xpos, ypos, best_mv, MAX_DIST);
        let mut best_dist = zero_dist;
        if best_dist > DIST_THRESH {
            for &cmv in cand_mvs[1..].iter() {
                let dist = mv_est.sad_blk4(ref_blk, xpos, ypos, cmv, best_dist);
                if dist < best_dist {
                    best_dist = dist;
                    best_mv   = cmv;
                    if best_dist <= DIST_THRESH {
                        break;
                    }
                }
            }
            if best_dist > DIST_THRESH {
                return search_template!(self, mv_est, ref_blk, xpos, ypos, sad_blk4, DIST_THRESH / 16, best_mv, best_dist, false);
            }
        }
        (best_mv, best_dist)
    }
}

pub struct MVEstimator {
    pub ref_mb:     SrcBlock,
    pub blk8:       [u8; 64],
    pub blk4:       [u8; 16],
    mc_buf:         NAVideoBufferRef<u8>,
    ref_frame:      NAVideoBufferRef<u8>,
    mv_range:       i16,
count: usize,
}

#[allow(dead_code)]
impl MVEstimator {
    pub fn new(ref_frame: NAVideoBufferRef<u8>, mc_buf: NAVideoBufferRef<u8>, mv_range: i16) -> Self {
        Self {
            ref_mb:         SrcBlock::new(),
            blk8:           [0; 64],
            blk4:           [0; 16],
            ref_frame, mc_buf,
            mv_range,
count: 0,
        }
    }
    pub fn get_mb(&mut self, dst: &mut SrcBlock, mb_x: usize, mb_y: usize, cur_mv: MV) {
        let tmp_blk = self.mc_buf.get_data_mut().unwrap();
        mc_block16x16(&mut dst.luma, 0, 16, mb_x * 16, mb_y * 16, cur_mv.x * 2, cur_mv.y * 2, self.ref_frame.clone(), 0, tmp_blk);
        mc_block8x8(&mut dst.chroma[0], 0, 8, mb_x * 8, mb_y * 8, cur_mv.x, cur_mv.y, self.ref_frame.clone(), 1, tmp_blk);
        mc_block8x8(&mut dst.chroma[1], 0, 8, mb_x * 8, mb_y * 8, cur_mv.x, cur_mv.y, self.ref_frame.clone(), 2, tmp_blk);
    }
    pub fn get_blk8(&mut self, dst: &mut [u8; 64], plane: usize, x: usize, y: usize, mut cur_mv: MV) {
        if plane == 0 {
            cur_mv.x *= 2;
            cur_mv.y *= 2;
        }
        mc_block8x8(dst, 0, 8, x, y, cur_mv.x, cur_mv.y, self.ref_frame.clone(), plane, self.mc_buf.get_data_mut().unwrap());
    }
    fn sad_blk8(&mut self, refblk: &[u8; 64], x: usize, y: usize, cur_mv: MV, _best_dist: u32) -> u32 {
        mc_block8x8(&mut self.blk8, 0, 8, x, y, cur_mv.x * 2, cur_mv.y * 2, self.ref_frame.clone(), 0, self.mc_buf.get_data_mut().unwrap());
self.count += 1;
        sad8x8(&self.blk8, refblk)
    }
    pub fn get_blk4(&mut self, dst: &mut [u8; 16], plane: usize, x: usize, y: usize, mut cur_mv: MV) {
        if plane == 0 {
            cur_mv.x *= 2;
            cur_mv.y *= 2;
        }
        mc_block4x4(dst, 0, 4, x, y, cur_mv.x, cur_mv.y, self.ref_frame.clone(), plane, self.mc_buf.get_data_mut().unwrap());
    }
    fn sad_blk4(&mut self, refblk: &[u8; 16], x: usize, y: usize, cur_mv: MV, _best_dist: u32) -> u32 {
        mc_block4x4(&mut self.blk4, 0, 4, x, y, cur_mv.x * 2, cur_mv.y * 2, self.ref_frame.clone(), 0, self.mc_buf.get_data_mut().unwrap());
        sad4x4(&self.blk4, refblk)
    }
    fn sad_mb(&mut self, cur_mb: &SrcBlock, mb_x: usize, mb_y: usize, cur_mv: MV, best_dist: u32) -> u32 {
        let tmp_blk = self.mc_buf.get_data_mut().unwrap();

        mc_block16x16(&mut self.ref_mb.luma, 0, 16, mb_x * 16, mb_y * 16, cur_mv.x * 2, cur_mv.y * 2, self.ref_frame.clone(), 0, tmp_blk);
        mc_block8x8(&mut self.ref_mb.chroma[0], 0, 8, mb_x * 8, mb_y * 8, cur_mv.x, cur_mv.y, self.ref_frame.clone(), 1, tmp_blk);
        mc_block8x8(&mut self.ref_mb.chroma[1], 0, 8, mb_x * 8, mb_y * 8, cur_mv.x, cur_mv.y, self.ref_frame.clone(), 2, tmp_blk);
        let mut dist = 0;
        let mut diff = [0; 16];
        for (sblk, dblk) in self.ref_mb.luma_blocks().zip(cur_mb.luma_blocks()) {
            get_block_difference(&mut diff, &sblk, &dblk);
            dist += sad(&diff);
            if dist > best_dist {
                break;
            }
        }
        'chroma_loop: for chroma in 0..2 {
            for (sblk, dblk) in self.ref_mb.chroma_blocks(chroma).zip(cur_mb.chroma_blocks(chroma)) {
                get_block_difference(&mut diff, &sblk, &dblk);
                dist += sad(&diff);
                if dist > best_dist {
                    break 'chroma_loop;
                }
            }
        }
        dist
    }
}

fn sad(diff: &[i16; 16]) -> u32 {
    diff.iter().fold(0u32, |acc, &x| acc + ((i32::from(x) * i32::from(x)) as u32))
}
fn sad8x8(blk1: &[u8; 64], blk2: &[u8; 64]) -> u32 {
    let mut sum = 0u32;
    for (&a, &b) in blk1.iter().zip(blk2.iter()) {
        let diff = i32::from(a) - i32::from(b);
        sum += (diff * diff) as u32;
    }
    sum
}
fn sad4x4(blk1: &[u8; 16], blk2: &[u8; 16]) -> u32 {
    let mut sum = 0u32;
    for (&a, &b) in blk1.iter().zip(blk2.iter()) {
        let diff = i32::from(a) - i32::from(b);
        sum += (diff * diff) as u32;
    }
    sum
}
