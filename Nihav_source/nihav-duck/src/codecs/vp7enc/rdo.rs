use super::blocks::*;
use super::coder::*;

static mut COUNTER: usize = 0;

pub const SMALL_DIST: u32 = 256;
pub const MAX_DIST: u32 = std::u32::MAX;

const INTER_TO_INTRA_RATIO: f32 = 0.85;

pub struct RateDistMetric {
    pub lambda: f32,
}

impl RateDistMetric {
    pub fn new() -> Self {
        Self {
            lambda:     1.0,
        }
    }
    pub fn calc_metric(&self, dist: u32, nits: u32) -> u32 {
        ((dist as f32) + self.lambda * (nits as f32) + 0.5) as u32
    }
    pub fn adjust_br(&mut self, cur_size: usize, tgt_size: usize) {
        let low_limit = tgt_size - tgt_size / 8;
        let up_limit  = tgt_size + tgt_size / 8;
        if cur_size < low_limit {
            self.lambda = (self.lambda - 0.1).max(0.0);
        } else if cur_size > up_limit {
            self.lambda = (self.lambda + 0.1).min(16.0);
        }
    }

    pub fn block_dist(&self, src: &[u8; 16], new: &[u8; 16], q: usize, ctype: usize, pctx: u8, probs: &[[[u8; 11]; 3]; 8]) -> (u32, bool) {
        let mut diff = [0i16; 16];
        get_block_difference(&mut diff, src, new);
        diff.fdct();
        diff.quant(q, ctype);
        let has_nz = diff.has_nz();
        let nits = estimate_subblock_nits(&diff, ctype, pctx, probs);
        diff.dequant(q, ctype);
        diff.idct();
        let dist = get_difference_dist(src, new, &diff);
unsafe {COUNTER += 1;}
        (self.calc_metric(dist, nits), has_nz)
    }
}

#[derive(Default)]
pub struct BitRateControl {
    tb_num:     u32,
    tb_den:     u32,
    key_int:    u32,
    bitrate:    u32,
    force_q:    Option<usize>,
    bitpool:    u32,
    fpos:       u32,
    kpos:       u32,
    num_mb:     u32,
}

impl BitRateControl {
    pub fn new() -> Self { Self::default() }
    fn reset(&mut self) {
        self.fpos = 0;
        self.kpos = 0;
        self.bitpool = self.bitrate;
    }
    pub fn set_params(&mut self, tb_num: u32, tb_den: u32, bitrate: u32, key_int: u8, num_mb: usize) {
        self.tb_num  = tb_num;
        self.tb_den  = tb_den;
        self.bitrate = bitrate;
        self.key_int = u32::from(key_int);
        self.num_mb  = num_mb as u32;
        self.reset();
    }
    pub fn has_bitrate(&self) -> bool { self.bitrate != 0 }
    pub fn get_quant(&self) -> Option<usize> { self.force_q }
    pub fn set_quant(&mut self, q: Option<usize>) {
        if self.force_q != q {
            self.force_q = q;
            self.reset();
        }
    }
    pub fn set_key_interval(&mut self, key_int: u8) {
        let key_int = u32::from(key_int);
        if self.key_int != key_int {
            self.key_int = key_int;
            self.reset();
        }
    }
    pub fn get_target_size(&self, is_intra: bool) -> u32 {
        if self.bitrate != 0 && self.force_q.is_none() {
            let pool_frames = self.tb_den - self.fpos;
            if self.key_int <= 1 { // all intra
                if self.bitpool == 0 || pool_frames == 0 {
                    self.bitrate * self.tb_num / self.tb_den
                } else {
                    self.bitpool / pool_frames
                }
            } else {
                let full_gop_weight = 1.0 + ((self.key_int - 1) as f32) * INTER_TO_INTRA_RATIO;
                let i_bits = if self.bitpool == 0 || pool_frames == 0 {
                        let gop_size = self.bitrate * self.tb_num * self.key_int / self.tb_den;
                        (gop_size as f32) / full_gop_weight
                    } else {
                        let full_gops = pool_frames / self.key_int;
                        let weight = (full_gops as f32) * full_gop_weight + ((pool_frames % self.key_int) as f32) * INTER_TO_INTRA_RATIO;
                        (self.bitpool as f32) / weight
                    };
                if is_intra {
                    (i_bits + 0.5) as u32
                } else {
                    (i_bits * INTER_TO_INTRA_RATIO + 0.5) as u32
                }
            }
        } else {
            0
        }
    }
    fn pred_nits_per_mb(is_intra: bool, q: usize) -> f32 {
        let fq = q as f32;
        match (is_intra, q) {
            (true, 0..=6) => 3434.0 + fq * fq * 7.5 - fq * 195.0,
            (true, _) => 2500.0 - (fq - 6.0).ln() * 500.0,
            (false, 0..=10) => 1595.0 + fq * fq * 3.4 - fq * 125.0,
            (false, _) => 800.0 - (fq - 8.0).ln() * 155.0,
        }
    }
    #[allow(dead_code)]
    // todo use for refining maybe
    pub fn predict_size(&self, is_intra: bool, q: usize) -> u32 {
        let min_size = if is_intra { 200 * 8 } else { 50 * 8 };
        let nits_per_mb = Self::pred_nits_per_mb(is_intra, q);
        ((nits_per_mb * (self.num_mb as f32) / 8.0) as u32).max(min_size)
    }
    pub fn get_frame_quant(&self, is_intra: bool) -> usize {
        if let Some(q) = self.force_q {
            q
        } else {
            let expected_size = self.get_target_size(is_intra);
            let nits_per_mb = ((expected_size * 8) as f32) / (self.num_mb as f32);
            if is_intra {
                if nits_per_mb > 2500.0 { // simple search
                    if nits_per_mb > Self::pred_nits_per_mb(is_intra, 3) {
                        if nits_per_mb > Self::pred_nits_per_mb(is_intra, 1) {
                            0
                        } else if nits_per_mb > Self::pred_nits_per_mb(is_intra, 2) {
                            1
                        } else {
                            2
                        }
                    } else {
                        if nits_per_mb > Self::pred_nits_per_mb(is_intra, 4) {
                            3
                        } else if nits_per_mb > Self::pred_nits_per_mb(is_intra, 5) {
                            4
                        } else {
                            5
                        }
                    }
                } else {
                    ((((2500.0 - nits_per_mb) / 500.0).exp() + 6.0) as usize).min(127)
                }
            } else {
                if nits_per_mb > 680.0 { // simple search
                    let (start, end) = if nits_per_mb > Self::pred_nits_per_mb(is_intra, 5) {
                            if nits_per_mb > Self::pred_nits_per_mb(is_intra, 3) {
                                (0, 3)
                            } else {
                                (3, 5)
                            }
                        } else if nits_per_mb > Self::pred_nits_per_mb(is_intra, 7) {
                            (5, 7)
                        } else {
                            (7, 10)
                        };
                    let mut q = end;
                    for qq in start..end {
                        if nits_per_mb > Self::pred_nits_per_mb(is_intra, qq) {
                            q = qq;
                            break;
                        }
                    }
                    q
                } else {
                    ((((800.0 - nits_per_mb) / 155.0).exp() + 6.0) as usize).max(10).min(127)
                }
            }
        }
    }
    pub fn update(&mut self, size: usize) {
        self.kpos += 1;
        if self.kpos == self.key_int {
            self.kpos = 0;
        }
        if self.bitrate == 0 || self.force_q.is_some() {
            return;
        }
        self.fpos += self.tb_num;
        while self.fpos >= self.tb_den {
            self.fpos -= self.tb_den;
            self.bitpool += self.bitrate;
        }
        self.bitpool = self.bitpool.saturating_sub((size * 8) as u32);
    }
}
