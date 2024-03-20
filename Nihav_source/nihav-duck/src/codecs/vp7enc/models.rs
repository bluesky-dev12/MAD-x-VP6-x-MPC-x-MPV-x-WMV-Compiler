use super::super::vp7data::DEFAULT_MV_PROBS;
use super::super::vp78data::*;
pub use crate::codecs::vpenc::models::*;

#[derive(Clone,Copy)]
pub struct VP7Models {
    pub coef_probs:         [[[[u8; 11]; 3]; 8]; 4],
    pub mv_probs:           [[u8; 17]; 2],
    pub kf_ymode_prob:      [u8; 4],
    pub kf_uvmode_prob:     [u8; 3],
    pub prob_intra_pred:    u8,
    pub prob_last_pred:     u8,
    pub feature_present:    [u8; 4],
    pub feature_tree_probs: [[u8; 3]; 4],
}

impl VP7Models {
    pub fn new() -> Self {
        let mut obj: Self = unsafe { std::mem::zeroed() };
        obj.reset();
        obj
    }
    pub fn reset(&mut self) {
        self.coef_probs.copy_from_slice(&DEFAULT_DCT_PROBS);
        self.mv_probs.copy_from_slice(&DEFAULT_MV_PROBS);
        self.kf_ymode_prob.copy_from_slice(Y_MODE_TREE_PROBS);
        self.kf_uvmode_prob.copy_from_slice(UV_MODE_TREE_PROBS);
    }
}

pub trait VP7ProbCounter {
    fn to_prob8(self) -> u8;
    fn to_prob8_worthy(&self, ref_prob: &mut u8);
}

impl VP7ProbCounter for ProbCounter {
    fn to_prob8(self) -> u8 {
        if self.total > 0 {
            ((self.zeroes << 8) / self.total).min(255).max(1) as u8
        } else {
            128
        }
    }
    fn to_prob8_worthy(&self, ref_prob: &mut u8) {
        if self.total > 0 {
            let new_prob = self.to_prob();
            let new_bits = Self::est_bits(new_prob, self.zeroes, self.total);
            let old_bits = Self::est_bits(*ref_prob, self.zeroes, self.total);

            if new_bits + 8 < old_bits {
                *ref_prob = new_prob;
            }
        }
    }
}

#[derive(Clone,Copy,Default)]
pub struct VP7ModelsStat {
    pub coef_probs:         [[[[ProbCounter; 11]; 3]; 8]; 4],
    pub mv_probs:           [[ProbCounter; 17]; 2],
    pub kf_ymode_prob:      [ProbCounter; 4],
    pub kf_uvmode_prob:     [ProbCounter; 3],
    pub prob_intra_pred:    ProbCounter,
    pub prob_last_pred:     ProbCounter,
    pub feature_present:    [ProbCounter; 4],
    pub feature_tree_probs: [[ProbCounter; 3]; 4],
}

impl VP7ModelsStat {
    pub fn new() -> Self { Self::default() }
    pub fn reset(&mut self) {
        *self = Self::default();
    }
    pub fn generate(&self, dst: &mut VP7Models, is_intra: bool) {
        for (dst, src) in dst.feature_present.iter_mut().zip(self.feature_present.iter()) {
            *dst = src.to_prob8();
        }
        for (dst, src) in dst.feature_tree_probs.iter_mut().zip(self.feature_tree_probs.iter()) {
            for (dst, src) in dst.iter_mut().zip(src.iter()) {
                if src.total != 0 {
                    *dst = src.to_prob8();
                } else {
                    *dst = 255;
                }
            }
        }
        for (dst, src) in dst.coef_probs.iter_mut().zip(self.coef_probs.iter()) {
            for (dst, src) in dst.iter_mut().zip(src.iter()) {
                for (dst, src) in dst.iter_mut().zip(src.iter()) {
                    for (dst, src) in dst.iter_mut().zip(src.iter()) {
                        src.to_prob8_worthy(dst);
                    }
                }
            }
        }

        if !is_intra {
            dst.prob_intra_pred = self.prob_intra_pred.to_prob8();
            dst.prob_last_pred = self.prob_last_pred.to_prob8();

            for (dmv, smv) in dst.mv_probs.iter_mut().zip(self.mv_probs.iter()) {
                for (dp, sp) in dmv.iter_mut().zip(smv.iter()) {
                    *dp = sp.to_prob_worthy(*dp);
                }
            }

            for (dp, sp) in dst.kf_ymode_prob.iter_mut().zip(self.kf_ymode_prob.iter()) {
                sp.to_prob8_worthy(dp);
            }
            for (dp, sp) in dst.kf_uvmode_prob.iter_mut().zip(self.kf_uvmode_prob.iter()) {
                sp.to_prob8_worthy(dp);
            }
       }
    }
}
