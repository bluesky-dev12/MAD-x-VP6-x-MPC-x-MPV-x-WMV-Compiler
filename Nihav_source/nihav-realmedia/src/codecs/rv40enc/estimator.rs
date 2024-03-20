use nihav_core::frame::FrameType;
use nihav_codec_support::codecs::MV;
use super::types::*;
use super::super::rv40data::*;

pub struct BitsEstimator {
    ftype:          FrameType,
    pred_mbt:       MBType,
    cur_mbt:        MBType,
}

impl BitsEstimator {
    pub fn new() -> Self {
        Self {
            ftype:          FrameType::I,
            pred_mbt:       MBType::Invalid,
            cur_mbt:        MBType::Invalid,
        }
    }
    pub fn set_frame_type(&mut self, ftype: FrameType) {
        self.ftype = ftype;
    }
    pub fn set_quant(&mut self, _q: usize) {
    }
    pub fn set_pred_mb_type(&mut self, most_prob_type: MBType) {
        self.pred_mbt = most_prob_type;
    }
    pub fn set_mb_type(&mut self, mbt: MBType) {
        self.cur_mbt = mbt;
    }
    pub fn estimate_mb_hdr(&self, mvs: &[MV]) -> u32 {
        if self.ftype == FrameType::I {
            return 1;
        }
        let hdr_cw_bits = if self.ftype == FrameType::P {
                RV40_PTYPE_BITS[self.pred_mbt.to_code()][self.cur_mbt.to_code()]
            } else {
                RV40_BTYPE_BITS[self.pred_mbt.to_code()][self.cur_mbt.to_code()]
            };
        let mv_bits = mvs.iter().fold(0u32, |acc, &mv| acc + Self::mv_cost(mv));
        u32::from(hdr_cw_bits) + mv_bits
    }
    fn block_no_to_type(&self, blk_no: usize) -> usize {
        match blk_no {
            0..=15 => {
                match self.cur_mbt {
                    MBType::Intra16 | MBType::P16x16Mix => 2,
                    MBType::Intra => 1,
                    _ => 0,
                }
            },
            24 => 3,
            _ if self.cur_mbt.is_intra() => 4,
            _ => 5,
        }
    }
    pub fn block_bits(&self, blk: &Block, blk_no: usize) -> u32 {
        let btype = self.block_no_to_type(blk_no);

        const EXPECTED_BLOCK_BITS: [[u8; 17]; 6] = [
            [ 0, 7, 12, 17, 22, 26, 31, 35, 39, 45, 51, 56, 61, 66,  85, 103, 117],
            [ 0, 7, 13, 19, 26, 30, 36, 43, 49, 57, 65, 74, 87, 99, 115, 131, 147],
            [ 0, 7, 14, 20, 25, 30, 35, 40, 45, 50, 56, 62, 69, 76,  84,  93, 113],
            [ 2, 9, 13, 20, 25, 29, 33, 38, 43, 48, 54, 62, 71, 82,  98, 116, 141],
            [ 0, 5, 12, 18, 24, 30, 35, 42, 48, 53, 62, 69, 78, 87,  97, 106, 121],
            [ 0, 6, 12, 17, 22, 27, 33, 40, 47, 53, 60, 66, 73, 80,  85,  85, 103]
        ];
        EXPECTED_BLOCK_BITS[btype][blk.count_nz()].into()
    }
    pub fn mv_cost(mv: MV) -> u32 {
        let xval = mv.x.abs() * 2 + 1;
        let yval = mv.y.abs() * 2 + 1;
        (15 - xval.leading_zeros()) * 2 + (15 - yval.leading_zeros()) * 2 + 2
    }
    pub fn decide_set(hist: &[usize; 17]) -> usize {
        let max_val = hist[16];
        let threshold = max_val - max_val / 4;
        if hist[3] > threshold {
            2
        } else if hist[6] > threshold {
            1
        } else {
            0
        }
    }
}
