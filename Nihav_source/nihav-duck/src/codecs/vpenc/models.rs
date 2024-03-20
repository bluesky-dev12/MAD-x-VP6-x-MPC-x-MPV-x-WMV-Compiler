#[derive(Clone,Copy,Default)]
pub struct ProbCounter {
    pub zeroes: u32,
    pub total:  u32,
}

// bits to code zero probability multiplied by eight
pub const PROB_BITS: [u8; 256] = [
     0, 64, 56, 51, 48, 45, 43, 42,
    40, 39, 37, 36, 35, 34, 34, 33,
    32, 31, 31, 30, 29, 29, 28, 28,
    27, 27, 26, 26, 26, 25, 25, 24,
    24, 24, 23, 23, 23, 22, 22, 22,
    21, 21, 21, 21, 20, 20, 20, 20,
    19, 19, 19, 19, 18, 18, 18, 18,
    18, 17, 17, 17, 17, 17, 16, 16,
    16, 16, 16, 15, 15, 15, 15, 15,
    15, 14, 14, 14, 14, 14, 14, 14,
    13, 13, 13, 13, 13, 13, 13, 12,
    12, 12, 12, 12, 12, 12, 12, 11,
    11, 11, 11, 11, 11, 11, 11, 11,
    10, 10, 10, 10, 10, 10, 10, 10,
    10,  9,  9,  9,  9,  9,  9,  9,
     9,  9,  9,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  8,  8,  7,  7,
     7,  7,  7,  7,  7,  7,  7,  7,
     7,  7,  6,  6,  6,  6,  6,  6,
     6,  6,  6,  6,  6,  6,  6,  5,
     5,  5,  5,  5,  5,  5,  5,  5,
     5,  5,  5,  5,  5,  5,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  3,  3,
     3,  3,  3,  3,  3,  3,  3,  3,
     3,  3,  3,  3,  3,  3,  3,  2,
     2,  2,  2,  2,  2,  2,  2,  2,
     2,  2,  2,  2,  2,  2,  2,  2,
     2,  1,  1,  1,  1,  1,  1,  1,
     1,  1,  1,  1,  1,  1,  1,  1,
     1,  1,  1,  1,  1,  1,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0
];

impl ProbCounter {
    pub fn add(&mut self, b: bool) {
        if !b {
            self.zeroes += 1;
        }
        self.total += 1;
    }
    pub fn to_prob(self) -> u8 {
        if self.total > 0 {
            (((self.zeroes << 8) / self.total).min(254) & !1).max(1) as u8
        } else {
            128
        }
    }
    pub fn to_prob_worthy(&self, old_prob: u8) -> u8 {
        if self.total > 0 {
            let new_prob = self.to_prob();
            let new_bits = Self::est_bits(new_prob, self.zeroes, self.total);
            let old_bits = Self::est_bits(old_prob, self.zeroes, self.total);

            if new_bits + 7 < old_bits {
                new_prob
            } else {
                old_prob
            }
        } else {
            old_prob
        }
    }
    pub fn est_bits(prob: u8, zeroes: u32, total: u32) -> u32 {
        (u32::from(PROB_BITS[prob as usize]) * zeroes + u32::from(PROB_BITS[256 - (prob as usize)]) * (total - zeroes) + 7) >> 3
    }
}
