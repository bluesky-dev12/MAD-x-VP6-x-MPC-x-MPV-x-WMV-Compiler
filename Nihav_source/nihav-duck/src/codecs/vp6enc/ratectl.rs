use super::rdo::*;

pub struct RateControl {
    pub lambda:     f32,
    tgt_br:         u32,
    budget:         isize,
    cur_time:       u32,
    ts_num:         u32,
    ts_den:         u32,
    mb_w:           usize,
    mb_h:           usize,
    projected:      usize,
}

// todo intra/inter decision, better allocation for intra frames
impl RateControl {
    pub fn new() -> Self {
        Self {
            lambda:     1.0,
            tgt_br:     0,
            budget:     0,
            cur_time:   0,
            ts_num:     0,
            ts_den:     0,
            mb_w:       0,
            mb_h:       0,
            projected:  0,
        }
    }
    pub fn init(&mut self, mb_w: usize, mb_h: usize, bitrate: u32, ts_num: u32, ts_den: u32) {
        self.mb_w   = mb_w;
        self.mb_h   = mb_h;
        self.lambda = 1.0;
        self.cur_time = 0;
        if bitrate == 0 || ts_num == 0 || ts_den == 0 {
            self.tgt_br = 0;
            self.budget = 0;
        } else {
            self.tgt_br     = bitrate;
            self.budget     = bitrate as isize;
            self.ts_num     = ts_num;
            self.ts_den     = ts_den;
        }
    }
    pub fn guess_quant(&mut self, intra: bool, huffman: bool) -> usize {
        let fsize = self.get_target_frame_size(intra);
        self.projected = fsize;
        if fsize > 0 {
            for q in 0..64 {
                let est_fsize = estimate_frame_size(intra, huffman, q, self.mb_w, self.mb_h);
                if fsize < est_fsize - est_fsize / 10 {
                    return q.saturating_sub(1);
                }
                if fsize < est_fsize + est_fsize / 10 {
                    return q;
                }
            }
            63
        } else {
            42
        }
    }
    pub fn update(&mut self, dsize: usize) {
        const LAMBDA_STEP: f32 = 1.0 / 32.0;

        if self.tgt_br == 0 {
            return;
        }
        if (self.projected > dsize + dsize / 10) && self.lambda > LAMBDA_STEP {
            self.lambda -= LAMBDA_STEP;
        } else if self.projected < dsize - dsize / 10 {
            self.lambda += LAMBDA_STEP;
        }
        self.budget -= dsize as isize;
        self.cur_time += self.ts_num;
        while self.cur_time >= self.ts_den {
            self.cur_time -= self.ts_den;
            self.budget += self.tgt_br as isize;
        }
    }
    fn get_target_frame_size(&self, intra: bool) -> usize {
        if self.tgt_br == 0 {
            0
        } else {
            let mut avg_fsize = self.budget / ((self.ts_den - self.cur_time) as isize);
            if avg_fsize > 0 {
                // todo better intra/inter selection
                if intra {
                    avg_fsize *= 3;
                }
                avg_fsize as usize
            } else {
                (self.tgt_br as usize) * (self.ts_num as usize) / (self.ts_den as usize) / 2
            }
        }
    }
}
