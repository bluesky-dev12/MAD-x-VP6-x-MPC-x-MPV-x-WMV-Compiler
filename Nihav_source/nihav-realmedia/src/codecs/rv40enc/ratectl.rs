use nihav_core::frame::FrameType;

pub struct RateDistMetric {
    pub lambda:         f32,
    pub good_enough:    u32,
    pub p_split_thr:    u32,
}

impl RateDistMetric {
    pub fn new() -> Self {
        Self {
            lambda:         1.0,
            good_enough:    256,
            p_split_thr:   8192,
        }
    }
    pub fn get_metric(&self, bits: u32, dist: u32) -> u32 {
        ((bits as f32) + (dist as f32) * 0.1 * self.lambda).ceil() as u32
    }
}

#[derive(Clone,Copy)]
struct BitrateCounter {
    factors:    [f32; 32],
    last_q:     usize,
    proj_size:  usize,
    intra:      bool,
}

impl BitrateCounter {
    fn new(intra: bool) -> Self {
        let mut obj = Self {
            factors:    [0.0; 32],
            last_q:     0,
            proj_size:  0,
            intra
        };
        obj.reset();
        obj
    }
    fn reset(&mut self) {
        if self.intra {
            self.last_q = 8;
            for (q, dst) in self.factors.iter_mut().enumerate() {
                let q = q as f32;
                *dst = (-0.1 * q + 2.95) / 100.0;
            }
        } else {
            self.last_q = 10;
            for (q, dst) in self.factors.iter_mut().enumerate() {
                let q = q as f32;
                *dst = 100.0 / (8.2 * q * q + 51.0 * q + 3411.0);
            }
        }
    }
    fn init_metric(&self, metric: &mut RateDistMetric, q_add: usize) {
        let q = (self.last_q + q_add).min(31);
        const THRESHOLDS: [(u32, u32); 4] = [
            (256, 8192), (128, 8192), (64, 4196), (32, 2048)
        ];
        let (ge_thr, ps_thr) = THRESHOLDS[q / 8];
        metric.good_enough = ge_thr;
        metric.p_split_thr = ps_thr;
        metric.lambda = 1.0;
    }
    fn update_stats(&mut self, fsize: usize) {
        if fsize < self.proj_size - self.proj_size / 8 {
            let mut inv_fac = 1.0 / self.factors[self.last_q];
            if inv_fac > 1.0 {
                inv_fac -= 0.5;
            }
            self.factors[self.last_q] = 1.0 / inv_fac;
        } else if fsize > self.proj_size + self.proj_size / 8 {
            let mut inv_fac = 1.0 / self.factors[self.last_q];
            if inv_fac < 200.0 {
                inv_fac += 0.5;
            }
            self.factors[self.last_q] = 1.0 / inv_fac;
        }
    }
    fn get_est_size(&self, complexity: u32, q: usize) -> usize {
        ((complexity as f32) * self.factors[q]).ceil() as usize
    }
    fn get_quant(&mut self, target: usize, complexity: u32) -> usize {
        let tgt_31 = self.get_est_size(complexity, 31);
        let tgt_0  = self.get_est_size(complexity, 0);
        if target < tgt_31 {
            self.last_q = 31;
            self.proj_size = tgt_31;
        } else if target > tgt_0 {
            self.last_q = 0;
            self.proj_size = tgt_0;
        } else { //xxx: do binary search?
            for q in (0..31).rev() {
                let expected_size = self.get_est_size(complexity, q);
                if target >= (expected_size - expected_size / 8) &&
                   target <= (expected_size + expected_size / 8) {
                    self.proj_size = expected_size;
                    self.last_q = q;
                }
            }
        }
        self.last_q
    }
    fn get_last_quant(&self) -> usize { self.last_q }
}

const TIMEBASE: u32 = 1000;

pub struct BitRateControl {
    force_quant:    Option<usize>,
    force_quality:  Option<u8>,
    br_counter:     [BitrateCounter; 2],

    bitrate:        u32,
    tpos:           u32,
    bitpool:        usize,

    duration:       u32,
    dcount:         u32,

    pub b_offset:   usize,
}

impl BitRateControl {
    pub fn new() -> Self {
        Self {
            force_quant:    None,
            force_quality:  None,
            br_counter:     [BitrateCounter::new(true), BitrateCounter::new(false)],

            bitrate:        0,
            tpos:           0,
            bitpool:        0,

            duration:       0,
            dcount:         0,

            b_offset:       4,
        }
    }
    pub fn rate_ctl_in_use(&self) -> bool {
        self.force_quant.is_none() && self.force_quality.is_none() && self.bitrate != 0
    }
    pub fn set_bitrate(&mut self, bitrate: u32) {
        self.bitrate = bitrate;
        for br in self.br_counter.iter_mut() {
            br.reset();
        }

        self.bitpool = (self.bitrate as usize) * 2;
        self.tpos = 0;
    }
    pub fn set_force_quant(&mut self, force_q: Option<usize>) { self.force_quant = force_q; }
    pub fn get_force_quant(&self) -> i8 {
        if let Some(q) = self.force_quant {
            q as i8
        } else {
            -1
        }
    }
    pub fn set_force_quality(&mut self, force_q: Option<u8>) { self.force_quality = force_q; }
    pub fn get_force_quality(&self) -> i8 {
        if let Some(q) = self.force_quality {
            q as i8
        } else {
            -1
        }
    }
    pub fn get_quant(&mut self, ftype: FrameType, complexity: u32) -> usize {
        if let Some(q) = self.force_quant {
            q
        } else if self.force_quality.is_some() {
            4
        } else if ftype != FrameType::B {
            let tgt = self.get_target_size(ftype);
            self.br_counter[if ftype == FrameType::I { 0 } else { 1 }].get_quant(tgt, complexity)
        } else {
            (self.br_counter[1].get_last_quant() + self.b_offset).min(31)
        }
    }
    pub fn get_last_quant(&self, ftype: FrameType) -> usize {
        match ftype {
            FrameType::I => self.br_counter[0].get_last_quant(),
            FrameType::P => self.br_counter[1].get_last_quant(),
            _ => (self.br_counter[1].get_last_quant() + self.b_offset).min(31),
        }
    }
    pub fn init_metric(&self, ftype: FrameType, metric: &mut RateDistMetric) {
        if let Some(q) = self.force_quality {
            metric.lambda = (q as f32) / 50.0;
        } else {
            match ftype {
                FrameType::I => {
                    self.br_counter[0].init_metric(metric, 0);
                },
                FrameType::P => {
                    self.br_counter[1].init_metric(metric, 0);
                },
                _ => {
                    self.br_counter[1].init_metric(metric, self.b_offset);
                },
            };
        }
    }
    pub fn update_stats(&mut self, ftype: FrameType, fsize: usize, ts_diff: u32) {
        if self.bitrate > 0 {
            if ts_diff > 0 && self.duration < std::u32::MAX / 2 {
                self.duration += ts_diff;
                self.dcount += 1;
            }
            self.tpos += ts_diff;
            while self.tpos >= TIMEBASE {
                self.tpos -= TIMEBASE;
                self.bitpool += self.bitrate as usize;
            }
            self.bitpool = self.bitpool.saturating_sub(fsize * 8).max(1024);
        }
        match ftype {
            FrameType::I => self.br_counter[0].update_stats(fsize),
            FrameType::P => self.br_counter[1].update_stats(fsize),
            _ => {},
        };
    }
    pub fn get_target_size(&self, ftype: FrameType) -> usize {
        if self.bitrate == 0 || self.bitpool == 0 {
            return 0;
        }
        let bitpool_limit = (self.bitrate + self.bitrate / 8) as usize;
        let bitpool_avail = self.bitpool.min(bitpool_limit);
        let target_size = if self.dcount > 0 {
                let avg_len = ((self.duration + self.dcount / 2) / self.dcount).max(1);
                bitpool_avail * (avg_len as usize) / ((TIMEBASE - self.tpos) as usize)
            } else {
                bitpool_avail / 10
            };
        let tgt_bits = match ftype {
                FrameType::I => target_size * 3,
                FrameType::B => target_size * 3 / 4,
                _ => target_size,
            };
        (tgt_bits + 7) / 8
    }
}
