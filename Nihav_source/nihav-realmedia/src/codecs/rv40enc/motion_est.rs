use nihav_core::frame::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use std::str::FromStr;
use super::dsp::{RefMBData, luma_mc, chroma_mc};

#[derive(Clone,Copy,PartialEq,Default)]
pub enum MVSearchMode {
    Dummy,
    Diamond,
    #[default]
    Hexagon,
    UMH,
}

impl MVSearchMode {
    pub const fn get_possible_modes() -> &'static [&'static str] {
        &["diamond", "hexagon", "umh"]
    }
    fn create(self) -> Box<dyn MVSearch+Send> {
        match self {
            MVSearchMode::Dummy   => Box::new(DummySearcher{}),
            MVSearchMode::Diamond => Box::new(DiaSearch::new()),
            MVSearchMode::Hexagon => Box::new(HexSearch::new()),
            MVSearchMode::UMH     => Box::new(UnevenHexSearch::new()),
        }
    }
}

impl std::fmt::Display for MVSearchMode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            MVSearchMode::Diamond => write!(f, "diamond"),
            MVSearchMode::Hexagon => write!(f, "hexagon"),
            MVSearchMode::UMH     => write!(f, "umh"),
            MVSearchMode::Dummy   => write!(f, "dummy"),
        }
    }
}

impl FromStr for MVSearchMode {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "diamond" => Ok(MVSearchMode::Diamond),
            "hexagon" => Ok(MVSearchMode::Hexagon),
            "umh"     => Ok(MVSearchMode::UMH),
            "dummy"   => Ok(MVSearchMode::Dummy),
            _ => Err(()),
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

const DIA_PATTERN: [MV; 9] = [
    ZERO_MV,
    MV {x: -2, y:  0},
    MV {x: -1, y:  1},
    MV {x:  0, y:  2},
    MV {x:  1, y:  1},
    MV {x:  2, y:  0},
    MV {x:  1, y: -1},
    MV {x:  0, y: -2},
    MV {x: -1, y: -1}
];

const HEX_PATTERN: [MV; 7] = [
    ZERO_MV,
    MV {x: -2, y:  0},
    MV {x: -1, y:  2},
    MV {x:  1, y:  2},
    MV {x:  2, y:  0},
    MV {x:  1, y: -2},
    MV {x: -1, y: -2}
];

const REFINEMENT: [MV; 4] = [
    MV {x: -1, y:  0},
    MV {x:  0, y:  1},
    MV {x:  1, y:  0},
    MV {x:  0, y: -1}
];

macro_rules! search_template {
    ($self: expr, $mv_est: expr, $cur_blk: expr, $mb_x: expr, $mb_y: expr, $sad_func: ident, $threshold: expr) => ({
        search_template!($self, $mv_est, $cur_blk, $mb_x, $mb_y, $sad_func, $threshold, ZERO_MV, MAX_DIST, true)
    });
    ($self: expr, $mv_est: expr, $cur_blk: expr, $mb_x: expr, $mb_y: expr, $sad_func: ident, $threshold: expr, $start_mv: expr, $best_dist: expr, $fullpel_stage: expr) => ({
        let mut best_dist = $best_dist;
        let mut best_mv = $start_mv;

        let mut min_dist;
        let mut min_idx;

        if $fullpel_stage {
            $self.reset();
            loop {
                let mut cur_best_dist = best_dist;
                for (dist, &point) in $self.dist.iter_mut().zip($self.point.iter()) {
                    if *dist == MAX_DIST {
                        *dist = $mv_est.$sad_func($cur_blk, $mb_x, $mb_y, point.from_pixels(), cur_best_dist);
                        cur_best_dist = cur_best_dist.min(*dist);
                        if *dist <= $threshold {
                            break;
                        }
                    }
                }
                min_dist = $self.dist[0];
                min_idx = 0;
                for (i, &dist) in $self.dist.iter().enumerate().skip(1) {
                    if dist < min_dist {
                        min_dist = dist;
                        min_idx = i;
                        if dist <= $threshold {
                            break;
                        }
                    }
                }
                if min_dist <= $threshold || min_idx == 0 || best_dist == min_dist || $self.point[min_idx].x.abs() >= $mv_est.mv_range || $self.point[min_idx].y.abs() >= $mv_est.mv_range {
                    break;
                }
                best_dist = min_dist;
                $self.update($self.steps[min_idx]);
            }
            best_dist = min_dist;
            best_mv   = $self.point[min_idx];
            if best_dist <= $threshold {
                return (best_mv.from_pixels(), best_dist);
            }
            for &step in REFINEMENT.iter() {
                let mv = best_mv + step;
                let dist = $mv_est.$sad_func($cur_blk, $mb_x, $mb_y, mv.from_pixels(), MAX_DIST);
                if best_dist > dist {
                    best_dist = dist;
                    best_mv = mv;
                }
            }
            best_mv = best_mv.from_pixels();
            if best_dist <= $threshold {
                return (best_mv, best_dist);
            }
        }

        // subpel refinement
        $self.set_new_point(best_mv, best_dist);
        loop {
            let mut cur_best_dist = best_dist;
            for (dist, &point) in $self.dist.iter_mut().zip($self.point.iter()) {
                if *dist == MAX_DIST {
                    *dist = $mv_est.$sad_func($cur_blk, $mb_x, $mb_y, point, cur_best_dist);
                    cur_best_dist = cur_best_dist.min(*dist);
                    if *dist <= $threshold {
                        break;
                    }
                }
            }
            min_dist = $self.dist[0];
            min_idx = 0;
            for (i, &dist) in $self.dist.iter().enumerate().skip(1) {
                if dist < min_dist {
                    min_dist = dist;
                    min_idx = i;
                    if dist <= $threshold {
                        break;
                    }
                }
            }
            if min_dist <= $threshold || min_idx == 0 || best_dist == min_dist || $self.point[min_idx].x.abs() >= $mv_est.mv_range * 8 || $self.point[min_idx].y.abs() >= $mv_est.mv_range * 8 {
                break;
            }
            best_dist = min_dist;
            $self.update($self.steps[min_idx]);
        }
        best_dist = min_dist;
        best_mv   = $self.point[min_idx];
        if best_dist <= $threshold {
            return (best_mv, best_dist);
        }
        for &step in REFINEMENT.iter() {
            let mv = best_mv + step;
            let dist = $mv_est.$sad_func($cur_blk, $mb_x, $mb_y, mv, MAX_DIST);
            if best_dist > dist {
                best_dist = dist;
                best_mv = mv;
            }
        }
        (best_mv, best_dist)
    });
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
            fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_mb: &RefMBData, mb_x: usize, mb_y: usize, _cand_mvs: &[MV]) -> (MV, u32) {
                search_template!(self, mv_est, cur_mb, mb_x, mb_y, sad_mb, DIST_THRESH)
            }
            fn search_blk8(&mut self, mv_est: &mut MVEstimator, ref_blk: &RefMBData, xpos: usize, ypos: usize, _cand_mvs: &[MV]) -> (MV, u32) {
                search_template!(self, mv_est, ref_blk, xpos, ypos, sad_blk8, DIST_THRESH / 4)
            }
        }
    }
}

pattern_search!(DiaSearch, DIA_PATTERN);
pattern_search!(HexSearch, HEX_PATTERN);

const LARGE_HEX_PATTERN: [MV; 16] = [
    MV { x: -4, y:  0 },
    MV { x: -4, y:  1 },
    MV { x: -4, y:  2 },
    MV { x: -2, y:  3 },
    MV { x:  0, y:  4 },
    MV { x:  2, y:  3 },
    MV { x:  4, y:  2 },
    MV { x:  4, y:  1 },
    MV { x:  4, y:  0 },
    MV { x:  4, y: -1 },
    MV { x:  4, y: -2 },
    MV { x: -2, y: -3 },
    MV { x:  0, y: -4 },
    MV { x: -2, y: -3 },
    MV { x: -4, y: -2 },
    MV { x: -4, y: -1 }
];

const UNSYMM_CROSS: [MV; 4] = [
    MV { x: -2, y:  0 },
    MV { x:  0, y:  1 },
    MV { x:  2, y:  0 },
    MV { x:  0, y: -1 }
];

#[derive(Default)]
struct UniqueSet<T:Copy+Default> {
    list:   [T; 16],
    count:  usize,
}

impl<T:Copy+Default+PartialEq> UniqueSet<T> {
    fn new() -> Self { Self::default() }
    fn clear(&mut self) { self.count = 0; }
    fn get_list(&self) -> &[T] { &self.list[..self.count] }
    fn add(&mut self, val: T) {
        if self.count < self.list.len() && !self.get_list().contains(&val) {
            self.list[self.count] = val;
            self.count += 1;
        }
    }
}

trait MVOps {
    fn scale(self, scale: i16) -> Self;
    fn is_in_range(self, range: i16) -> bool;
}

impl MVOps for MV {
    fn scale(self, scale: i16) -> MV {
        MV { x: self.x * scale, y: self.y * scale }
    }
    fn is_in_range(self, range: i16) -> bool {
        self.x.abs() <= range && self.y.abs() <= range
    }
}

macro_rules! single_search_step {
    ($start:expr, $best_dist:expr, $mv_est:expr, $sad_func:ident, $ref_blk:expr, $xpos:expr, $ypos:expr, $pattern:expr, $scale:expr, $dist_thr:expr) => {{
        let mut best_mv = $start;
        let mut best_dist = $best_dist;
        for point in $pattern.iter() {
            let mv = point.scale($scale) + $start;
            if !mv.is_in_range($mv_est.mv_range * 4) {
                continue;
            }
            let dist = $mv_est.$sad_func($ref_blk, $xpos, $ypos, mv, best_dist);
            if dist < best_dist {
                best_mv = mv;
                best_dist = dist;
                if best_dist < $dist_thr {
                    break;
                }
            }
        }
        (best_mv, best_dist, best_mv != $start)
    }}
}

struct UnevenHexSearch {
    mv_list:    UniqueSet<MV>,
}

impl UnevenHexSearch {
    fn new() -> Self {
        Self {
            mv_list:    UniqueSet::new(),
        }
    }
    fn get_cand_mv(&mut self, cand_mvs: &[MV]) -> MV {
        self.mv_list.clear();
        for &mv in cand_mvs.iter() {
            self.mv_list.add(mv);
        }
        match self.mv_list.count {
            1 => self.mv_list.list[0],
            3 => MV::pred(self.mv_list.list[0], self.mv_list.list[1], self.mv_list.list[2]),
            _ => {
                let sum = self.mv_list.get_list().iter().fold((0i32, 0i32),
                    |acc, mv| (acc.0 + i32::from(mv.x), acc.1 + i32::from(mv.y)));
                MV {x: (sum.0 / (self.mv_list.count as i32)) as i16,
                    y: (sum.1 / (self.mv_list.count as i32)) as i16}
            },
        }
    }
}

macro_rules! umh_search_template {
    ($cand_mv:expr, $cutoff:expr, $mv_est:expr, $sad_func:ident, $ref_blk:expr, $xpos:expr, $ypos:expr) => {{
        let cand_mv = $cand_mv;
        let best_dist = $mv_est.$sad_func($ref_blk, $xpos, $ypos, cand_mv, MAX_DIST);
        if best_dist < $cutoff {
            return (cand_mv, best_dist);
        }

        // step 1 - small refinement search
        let (mut cand_mv, mut best_dist, _) = single_search_step!(cand_mv, best_dist, $mv_est, $sad_func, $ref_blk, $xpos, $ypos, DIA_PATTERN, 1, $cutoff);
        if best_dist < $cutoff {
            return (cand_mv, best_dist);
        }

        // step 2 - unsymmetrical cross search
        loop {
            let (mv, dist, changed) = single_search_step!(cand_mv, best_dist, $mv_est, $sad_func, $ref_blk, $xpos, $ypos, UNSYMM_CROSS, 4, $cutoff);
            if !changed {
                break;
            }
            cand_mv = mv;
            best_dist = dist;
            if best_dist < $cutoff {
                return (mv, dist);
            }
        }

        // step 3 - multi-hexagon grid search
        let mut scale = 4;
        while scale > 0 {
            let (mv, dist, changed) = single_search_step!(cand_mv, best_dist, $mv_est, $sad_func, $ref_blk, $xpos, $ypos, LARGE_HEX_PATTERN, scale, $cutoff);
            if !changed {
                break;
            }
            cand_mv = mv;
            best_dist = dist;
            if best_dist < $cutoff {
                return (mv, dist);
            }
            scale >>= 1;
        }
        // step 4 - final hexagon search
        let (cand_mv, best_dist, _) = single_search_step!(cand_mv, best_dist, $mv_est, $sad_func, $ref_blk, $xpos, $ypos, HEX_PATTERN, 1, $cutoff);
        if best_dist > $cutoff {
            let (mv, dist, _) = single_search_step!(cand_mv, best_dist, $mv_est, $sad_func, $ref_blk, $xpos, $ypos, DIA_PATTERN, 1, $cutoff);
            (mv, dist)
        } else {
            (cand_mv, best_dist)
        }
    }}
}

impl MVSearch for UnevenHexSearch {
    fn search_mb(&mut self, mv_est: &mut MVEstimator, cur_mb: &RefMBData, mb_x: usize, mb_y: usize, cand_mvs: &[MV]) -> (MV, u32) {
        let cand_mv = self.get_cand_mv(cand_mvs);
        let cutoff = mv_est.cutoff_thr;
        umh_search_template!(cand_mv, cutoff, mv_est, sad_mb, cur_mb, mb_x, mb_y)
    }
    fn search_blk8(&mut self, mv_est: &mut MVEstimator, ref_blk: &RefMBData, xpos: usize, ypos: usize, cand_mvs: &[MV]) -> (MV, u32) {
        let cand_mv = self.get_cand_mv(cand_mvs);
        let cutoff = mv_est.cutoff_thr / 4;
        umh_search_template!(cand_mv, cutoff, mv_est, sad_blk8, ref_blk, xpos, ypos)
    }
}

struct MVEstimator<'a> {
    pic:        &'a NAVideoBuffer<u8>,
    mv_range:   i16,
    cutoff_thr: u32,
}

macro_rules! sad {
    ($src1:expr, $src2:expr) => {
        $src1.iter().zip($src2.iter()).fold(0u32, |acc, (&a, &b)|
            acc + (((i32::from(a) - i32::from(b)) * (i32::from(a) - i32::from(b))) as u32))
    }
}

impl<'a> MVEstimator<'a> {
    fn sad_mb(&self, ref_mb: &RefMBData, mb_x: usize, mb_y: usize, mv: MV, cur_best_dist: u32) -> u32 {
        let mut dst = RefMBData::new();
        luma_mc(&mut dst.y, 16, self.pic, mb_x * 16, mb_y * 16, mv, true);

        let mut dist = 0;
        for (dline, sline) in dst.y.chunks(16).zip(ref_mb.y.chunks(16)) {
            dist += sad!(dline, sline);
            if dist > cur_best_dist {
                return dist;
            }
        }
        chroma_mc(&mut dst.u, 8, self.pic, mb_x * 8, mb_y * 8, 1, mv, true);
        dist += sad!(dst.u, ref_mb.u);
        if dist > cur_best_dist {
            return dist;
        }
        chroma_mc(&mut dst.v, 8, self.pic, mb_x * 8, mb_y * 8, 2, mv, true);
        dist += sad!(dst.v, ref_mb.v);

        dist
    }
    fn sad_blk8(&self, ref_mb: &RefMBData, xpos: usize, ypos: usize, mv: MV, cur_best_dist: u32) -> u32 {
        let mut cur_y = [0; 64];
        let mut cur_u = [0; 16];
        let mut cur_v = [0; 16];

        let mut dist = 0;

        let y_off = (xpos & 8) + (ypos & 8) * 16;
        luma_mc(&mut cur_y, 8, self.pic, xpos, ypos, mv, false);
        for (dline, sline) in cur_y.chunks(8).zip(ref_mb.y[y_off..].chunks(16)) {
            dist += sad!(dline, sline);
            if dist > cur_best_dist {
                return dist;
            }
        }

        let c_off = (xpos & 8) / 2 + (ypos & 8) * 4;
        chroma_mc(&mut cur_u, 4, self.pic, xpos / 2, ypos / 2, 1, mv, false);
        for (dline, sline) in cur_u.chunks(4).zip(ref_mb.u[c_off..].chunks(8)) {
            dist += sad!(dline, sline);
            if dist > cur_best_dist {
                return dist;
            }
        }
        chroma_mc(&mut cur_v, 4, self.pic, xpos / 2, ypos / 2, 2, mv, false);
        for (dline, sline) in cur_v.chunks(4).zip(ref_mb.v[c_off..].chunks(8)) {
            dist += sad!(dline, sline);
            if dist > cur_best_dist {
                return dist;
            }
        }

        dist
    }
}

trait MVSearch {
    fn search_mb(&mut self, mv_est: &mut MVEstimator, ref_mb: &RefMBData, mb_x: usize, mb_y: usize, cand_mvs: &[MV]) -> (MV, u32);
    fn search_blk8(&mut self, mv_est: &mut MVEstimator, ref_blk: &RefMBData, xpos: usize, ypos: usize, cand_mvs: &[MV]) -> (MV, u32);
}

struct DummySearcher {}

impl MVSearch for DummySearcher {
    fn search_mb(&mut self, _mv_est: &mut MVEstimator, _ref_mb: &RefMBData, _mb_x: usize, _mb_y: usize, _cand_mvs: &[MV]) -> (MV, u32) {
        (ZERO_MV, std::u32::MAX / 2)
    }
    fn search_blk8(&mut self, _mv_est: &mut MVEstimator, _ref_mb: &RefMBData, _xpos: usize, _ypos: usize, _cand_mvs: &[MV]) -> (MV, u32) {
        (ZERO_MV, std::u32::MAX / 2)
    }
}

pub struct MotionEstimator {
    pub range:      i16,
    pub thresh:     u32,
        mode:       MVSearchMode,
        srch:       Box<dyn MVSearch+Send>,
}

impl MotionEstimator {
    pub fn new() -> Self {
        let mode = MVSearchMode::default();
        Self {
            range:      64,
            thresh:     32,
            mode,
            srch:       mode.create(),
        }
    }
    pub fn get_mode(&self) -> MVSearchMode { self.mode }
    pub fn set_mode(&mut self, new_mode: MVSearchMode) {
        if self.mode != new_mode {
            self.mode = new_mode;
            self.srch = self.mode.create();
        }
    }
    pub fn search_mb_p(&mut self, pic: &NAVideoBuffer<u8>, refmb: &RefMBData, mb_x: usize, mb_y: usize, cand_mvs: &[MV]) -> (MV, u32) {
        let mut mv_est = MVEstimator {
            mv_range:   self.range,
            cutoff_thr: self.thresh,
            pic,
        };
        self.srch.search_mb(&mut mv_est, refmb, mb_x, mb_y, cand_mvs)
    }
    pub fn search_blk8(&mut self, pic: &NAVideoBuffer<u8>, refmb: &RefMBData, xoff: usize, yoff: usize, cand_mvs: &[MV]) -> (MV, u32) {
        let mut mv_est = MVEstimator {
            mv_range:   self.range,
            cutoff_thr: self.thresh,
            pic,
        };
        self.srch.search_blk8(&mut mv_est, refmb, xoff, yoff, cand_mvs)
    }
}

pub struct SearchB<'a> {
    ref_p:      &'a NAVideoBuffer<u8>,
    ref_n:      &'a NAVideoBuffer<u8>,
    xpos:       usize,
    ypos:       usize,
    ratios:     [u32; 2],
    tmp1:       RefMBData,
    tmp2:       RefMBData,
    pred_blk:   RefMBData,
}

impl<'a> SearchB<'a> {
    pub fn new(ref_p: &'a NAVideoBuffer<u8>, ref_n: &'a NAVideoBuffer<u8>, mb_x: usize, mb_y: usize, ratios: [u32; 2]) -> Self {
        Self {
            ref_p, ref_n,
            xpos:       mb_x * 16,
            ypos:       mb_y * 16,
            ratios,
            tmp1:       RefMBData::new(),
            tmp2:       RefMBData::new(),
            pred_blk:   RefMBData::new(),
        }
    }
    pub fn search_mb(&mut self, ref_mb: &RefMBData, cand_mvs: [MV; 2]) -> (MV, MV) {
        let mut best_cand = cand_mvs;
        let mut best_dist = self.interp_b_dist(ref_mb, best_cand, MAX_DIST);

        loop {
            let mut improved = false;
            for &fmv_add in DIA_PATTERN.iter() {
                for &bmv_add in DIA_PATTERN.iter() {
                    let cand = [best_cand[0] + fmv_add.from_pixels(),
                                best_cand[1] + bmv_add.from_pixels()];
                    let dist = self.interp_b_dist(ref_mb, cand, best_dist);
                    if dist < best_dist {
                        best_dist = dist;
                        best_cand = cand;
                        improved = true;
                    }
                }
            }
            if !improved {
                break;
            }
        }

        for &fmv_add in REFINEMENT.iter() {
            for &bmv_add in REFINEMENT.iter() {
                let cand = [best_cand[0] + fmv_add, best_cand[1] + bmv_add];
                let dist = self.interp_b_dist(ref_mb, cand, best_dist);
                if dist < best_dist {
                    best_dist = dist;
                    best_cand = cand;
                }
            }
        }

        (best_cand[0], best_cand[1])
    }
    fn interp_b_dist(&mut self, ref_mb: &RefMBData, cand_mv: [MV; 2], cur_best_dist: u32) -> u32 {
        let [fmv, bmv] = cand_mv;
        luma_mc(&mut self.tmp1.y, 16, self.ref_p, self.xpos, self.ypos, fmv, true);
        chroma_mc(&mut self.tmp1.u, 8, self.ref_p, self.xpos / 2, self.ypos / 2, 1, fmv, true);
        chroma_mc(&mut self.tmp1.v, 8, self.ref_p, self.xpos / 2, self.ypos / 2, 2, fmv, true);
        luma_mc(&mut self.tmp2.y, 16, self.ref_n, self.xpos, self.ypos, bmv, true);
        chroma_mc(&mut self.tmp2.u, 8, self.ref_n, self.xpos / 2, self.ypos / 2, 1, bmv, true);
        chroma_mc(&mut self.tmp2.v, 8, self.ref_n, self.xpos / 2, self.ypos / 2, 2, bmv, true);
        self.pred_blk.avg(&self.tmp1, self.ratios[0], &self.tmp2, self.ratios[1]);

        let mut dist = 0;
        for (dline, sline) in self.pred_blk.y.chunks(16).zip(ref_mb.y.chunks(16)) {
            dist += sad!(dline, sline);
            if dist > cur_best_dist {
                return dist;
            }
        }
        dist += sad!(self.pred_blk.u, ref_mb.u);
        if dist > cur_best_dist {
            return dist;
        }
        dist += sad!(self.pred_blk.v, ref_mb.v);

        dist
    }
}

macro_rules! hadamard {
    ($s0:expr, $s1:expr, $s2:expr, $s3:expr, $d0:expr, $d1:expr, $d2:expr, $d3:expr) => {
        let t0 = $s0 + $s1;
        let t1 = $s0 - $s1;
        let t2 = $s2 + $s3;
        let t3 = $s2 - $s3;
        $d0 = t0 + t2;
        $d2 = t0 - t2;
        $d1 = t1 + t3;
        $d3 = t1 - t3;
    }
}

pub struct FrameComplexityEstimate {
    ref_frm:    NAVideoBufferRef<u8>,
    cur_frm:    NAVideoBufferRef<u8>,
    nxt_frm:    NAVideoBufferRef<u8>,
    width:      usize,
    height:     usize,
}

impl FrameComplexityEstimate {
    pub fn new() -> Self {
        let vinfo = NAVideoInfo::new(24, 24, false, YUV420_FORMAT);
        let vt = alloc_video_buffer(vinfo, 4).unwrap();
        let buf = vt.get_vbuf().unwrap();
        Self {
            ref_frm:    buf.clone(),
            cur_frm:    buf.clone(),
            nxt_frm:    buf,
            width:      0,
            height:     0,
        }
    }
    pub fn resize(&mut self, width: usize, height: usize) {
        if width != self.width || height != self.height {
            self.width  = width;
            self.height = height;

            let vinfo = NAVideoInfo::new(self.width / 2, self.height / 2, false, YUV420_FORMAT);
            let vt = alloc_video_buffer(vinfo, 4).unwrap();
            self.ref_frm = vt.get_vbuf().unwrap();
            let frm = self.ref_frm.get_data_mut().unwrap();
            for el in frm.iter_mut() {
                *el = 0x80;
            }
            let vt = alloc_video_buffer(vinfo, 4).unwrap();
            self.cur_frm = vt.get_vbuf().unwrap();
            let vt = alloc_video_buffer(vinfo, 4).unwrap();
            self.nxt_frm = vt.get_vbuf().unwrap();
        }
    }
    pub fn set_current(&mut self, frm: &NAVideoBuffer<u8>) {
        Self::downscale(&mut self.cur_frm, frm);
    }
    pub fn get_complexity(&self, ftype: FrameType) -> u32 {
        match ftype {
            FrameType::I => Self::calculate_i_cplx(&self.cur_frm),
            FrameType::P => Self::calculate_mv_diff(&self.ref_frm, &self.cur_frm),
            _ => 0,
        }
    }
    pub fn decide_b_frame(&mut self, frm1: &NAVideoBuffer<u8>, frm2: &NAVideoBuffer<u8>) -> bool {
        Self::downscale(&mut self.cur_frm, frm1);
        Self::downscale(&mut self.nxt_frm, frm2);
        let diff_ref_cur = Self::calculate_mv_diff(&self.ref_frm, &self.cur_frm);
        let diff_cur_nxt = Self::calculate_mv_diff(&self.cur_frm, &self.nxt_frm);

        // simple rule - if complexity ref->cur and cur->next is about the same this should be a B-frame
        let ddiff = diff_ref_cur.max(diff_cur_nxt) - diff_ref_cur.min(diff_cur_nxt);
        if ddiff < 256 {
            true
        } else {
            let mut order = 0;
            while (ddiff << order) < diff_ref_cur.min(diff_cur_nxt) {
                order += 1;
            }
            order > 2
        }
    }
    pub fn update_ref(&mut self) {
        std::mem::swap(&mut self.ref_frm, &mut self.cur_frm);
    }

    fn add_mv(mb_x: usize, mb_y: usize, mv: MV) -> (usize, usize) {
        (((mb_x * 16) as isize + (mv.x as isize)) as usize,
         ((mb_y * 16) as isize + (mv.y as isize)) as usize)
    }
    fn calculate_i_cplx(frm: &NAVideoBuffer<u8>) -> u32 {
        let (w, h) = frm.get_dimensions(0);
        let src = frm.get_data();
        let stride = frm.get_stride(0);
        let mut sum = 0;
        let mut offset = 0;
        for y in (0..h).step_by(4) {
            for x in (0..w).step_by(4) {
                sum += Self::satd_i(src, offset + x, stride, x > 0, y > 0);
            }
            offset += stride * 4;
        }
        sum
    }
    fn calculate_mv_diff(ref_frm: &NAVideoBuffer<u8>, cur_frm: &NAVideoBuffer<u8>) -> u32 {
        let (w, h) = ref_frm.get_dimensions(0);
        let mut sum = 0;
        for mb_y in 0..(h / 16) {
            for mb_x in 0..(w / 16) {
                sum += Self::satd_mb_diff(ref_frm, cur_frm, mb_x, mb_y);
            }
        }
        sum
    }
    fn satd_mb_diff(ref_frm: &NAVideoBuffer<u8>, cur_frm: &NAVideoBuffer<u8>, mb_x: usize, mb_y: usize) -> u32 {
        let mv = Self::search_mv(ref_frm, cur_frm, mb_x, mb_y);
        let mut sum = 0;
        let src0 = ref_frm.get_data();
        let src1 = cur_frm.get_data();
        let stride = ref_frm.get_stride(0);
        let (src_x, src_y) = Self::add_mv(mb_x, mb_y, mv);
        for y in (0..16).step_by(4) {
            for x in (0..16).step_by(4) {
                sum += Self::satd(&src0[src_x     + x + (src_y     + y) * stride..],
                                  &src1[mb_x * 16 + x + (mb_y * 16 + y) * stride..],
                                  stride);
            }
        }
        sum
    }
    fn search_mv(ref_frm: &NAVideoBuffer<u8>, cur_frm: &NAVideoBuffer<u8>, mb_x: usize, mb_y: usize) -> MV {
        let stride = ref_frm.get_stride(0);
        let (w, h) = ref_frm.get_dimensions(0);
        let (v_edge, h_edge) = (w - 16, h - 16);
        let ref_src = ref_frm.get_data();
        let cur_src = cur_frm.get_data();
        let cur_src = &cur_src[mb_x * 16 + mb_y * 16 * stride..];

        let mut best_mv = ZERO_MV;
        let mut best_dist = Self::sad(cur_src, ref_src, mb_x, mb_y, stride, best_mv);
        if best_dist == 0 {
            return best_mv;
        }

        for step in (0..=2).rev() {
            let mut changed = true;
            while changed {
                changed = false;
                for &mv in DIA_PATTERN[1..].iter() {
                    let cand_mv = best_mv + mv.scale(1 << step);
                    let (cx, cy) = Self::add_mv(mb_x, mb_y, cand_mv);
                    if cx > v_edge || cy > h_edge {
                        continue;
                    }
                    let cand_dist = Self::sad(cur_src, ref_src, mb_x, mb_y, stride, cand_mv);
                    if cand_dist < best_dist {
                        best_dist = cand_dist;
                        best_mv = cand_mv;
                        if best_dist == 0 {
                            return best_mv;
                        }
                        changed = true;
                    }
                }
            }
        }
        best_mv
    }
    fn sad(cur_src: &[u8], src: &[u8], mb_x: usize, mb_y: usize, stride: usize, mv: MV) -> u32 {
        let (src_x, src_y) = Self::add_mv(mb_x, mb_y, mv);
        let mut sum = 0;
        for (line1, line2) in cur_src.chunks(stride).zip(src[src_x + src_y * stride..].chunks(stride)).take(16) {
            sum += line1[..16].iter().zip(line2[..16].iter()).fold(0u32,
                 |acc, (&a, &b)| acc + u32::from(a.max(b) - a.min(b)) * u32::from(a.max(b) - a.min(b)));
        }
        sum
    }
    fn satd_i(src: &[u8], mut offset: usize, stride: usize, has_left: bool, has_top: bool) -> u32 {
        let mut diffs = [0; 16];
        match (has_left, has_top) {
            (true, true) => {
                for row in diffs.chunks_exact_mut(4) {
                    let mut left = i16::from(src[offset - 1]);
                    let mut tl = i16::from(src[offset - stride - 1]);
                    for (x, dst) in row.iter_mut().enumerate() {
                        let cur = i16::from(src[offset + x]);
                        let top = i16::from(src[offset + x - stride]);

                        *dst = cur - (top + left + tl - top.min(left).min(tl) - top.max(left).max(tl));

                        left = cur;
                        tl = top;
                    }

                    offset += stride;
                }
            },
            (true, false) => {
                for (dst, (left, cur)) in diffs.chunks_exact_mut(4).zip(
                     src[offset - 1..].chunks(stride).zip(src[offset..].chunks(stride))) {
                    for (dst, (&left, &cur)) in dst.iter_mut().zip(left.iter().zip(cur.iter())) {
                        *dst = i16::from(cur) - i16::from(left);
                    }
                }
            },
            (false, true) => {
                for (dst, (top, cur)) in diffs.chunks_exact_mut(4).zip(
                     src[offset - stride..].chunks(stride).zip(src[offset..].chunks(stride))) {
                    for (dst, (&top, &cur)) in dst.iter_mut().zip(top.iter().zip(cur.iter())) {
                        *dst = i16::from(cur) - i16::from(top);
                    }
                }
            },
            (false, false) => {
                for (dst, src) in diffs.chunks_exact_mut(4).zip(src[offset..].chunks(stride)) {
                    for (dst, &src) in dst.iter_mut().zip(src.iter()) {
                        *dst = i16::from(src) - 128;
                    }
                }
            },
        };
        for row in diffs.chunks_exact_mut(4) {
            hadamard!(row[0], row[1], row[2], row[3], row[0], row[1], row[2], row[3]);
        }
        for i in 0..4 {
            hadamard!(diffs[i], diffs[i + 4], diffs[i + 8], diffs[i + 12],
                      diffs[i], diffs[i + 4], diffs[i + 8], diffs[i + 12]);
        }
        diffs.iter().fold(0u32, |acc, x| acc + (x.unsigned_abs() as u32))
    }
    fn satd(src0: &[u8], src1: &[u8], stride: usize) -> u32 {
        let mut diffs = [0; 16];
        for (dst, (src0, src1)) in diffs.chunks_exact_mut(4).zip(
                src0.chunks(stride).zip(src1.chunks(stride))) {
            hadamard!(i16::from(src0[0]) - i16::from(src1[0]),
                      i16::from(src0[1]) - i16::from(src1[1]),
                      i16::from(src0[2]) - i16::from(src1[2]),
                      i16::from(src0[3]) - i16::from(src1[3]),
                      dst[0], dst[1], dst[2], dst[3]);
        }
        for i in 0..4 {
            hadamard!(diffs[i], diffs[i + 4], diffs[i + 8], diffs[i + 12],
                      diffs[i], diffs[i + 4], diffs[i + 8], diffs[i + 12]);
        }
        diffs.iter().fold(0u32, |acc, x| acc + (x.unsigned_abs() as u32))
    }
    fn downscale(dst: &mut NAVideoBuffer<u8>, src: &NAVideoBuffer<u8>) {
        let dst = NASimpleVideoFrame::from_video_buf(dst).unwrap();
        let sdata = src.get_data();
        for plane in 0..3 {
            let cur_w = dst.width[plane];
            let cur_h = dst.height[plane];
            let doff = dst.offset[plane];
            let soff = src.get_offset(plane);
            let dstride = dst.stride[plane];
            let sstride = src.get_stride(plane);
            for (dline, sstrip) in dst.data[doff..].chunks_exact_mut(dstride).zip(
                    sdata[soff..].chunks_exact(sstride * 2)).take(cur_h) {
                let (line0, line1) = sstrip.split_at(sstride);
                for (dst, (src0, src1)) in dline.iter_mut().zip(
                        line0.chunks_exact(2).zip(line1.chunks_exact(2))).take(cur_w) {
                    *dst = ((u16::from(src0[0]) + u16::from(src0[1]) +
                             u16::from(src1[0]) + u16::from(src1[1]) + 2) >> 2) as u8;
                }
            }
        }
    }
}
