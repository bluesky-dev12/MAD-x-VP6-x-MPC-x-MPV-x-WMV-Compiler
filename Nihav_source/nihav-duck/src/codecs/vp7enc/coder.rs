use nihav_core::codecs::EncoderResult;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use crate::codecs::vpenc::coder::*;
use super::super::vpcommon::*;
use super::super::vp78::*;
use super::super::vp78data::*;
use super::super::vp7data::*;
use super::blocks::MBType;
use super::models::*;
pub use crate::codecs::vpenc::coder::{BoolEncoder, Estimator};

const KF_Y_MODE_TREE: &[TokenSeq<PredMode>] = &[
    bit_seq!(PredMode::BPred;         F;       0),
    bit_seq!(PredMode::DCPred;        T, F, F; 0, 1, 2),
    bit_seq!(PredMode::VPred;         T, F, T; 0, 1, 2),
    bit_seq!(PredMode::HPred;         T, T, F; 0, 1, 3),
    bit_seq!(PredMode::TMPred;        T, T, T; 0, 1, 3),
];

const Y_MODE_TREE: &[TokenSeq<PredMode>] = &[
    bit_seq!(PredMode::DCPred;        F;       0),
    bit_seq!(PredMode::VPred;         T, F, F; 0, 1, 2),
    bit_seq!(PredMode::HPred;         T, F, T; 0, 1, 2),
    bit_seq!(PredMode::TMPred;        T, T, F; 0, 1, 3),
    bit_seq!(PredMode::BPred;         T, T, T; 0, 1, 3),
];

const UV_MODE_TREE: &[TokenSeq<PredMode>] = &[
    bit_seq!(PredMode::DCPred;        F;       0),
    bit_seq!(PredMode::VPred;         T, F;    0, 1),
    bit_seq!(PredMode::HPred;         T, T, F; 0, 1, 2),
    bit_seq!(PredMode::TMPred;        T, T, T; 0, 1, 2),
];

const B_MODE_TREE: &[TokenSeq<PredMode>] = &[
    bit_seq!(PredMode::DCPred;        F;                    0),
    bit_seq!(PredMode::TMPred;        T, F;                 0, 1),
    bit_seq!(PredMode::VPred;         T, T, F;              0, 1, 2),
    bit_seq!(PredMode::HPred;         T, T, T, F, F;        0, 1, 2, 3, 4),
    bit_seq!(PredMode::RDPred;        T, T, T, F, T, F;     0, 1, 2, 3, 4, 5),
    bit_seq!(PredMode::VRPred;        T, T, T, F, T, T;     0, 1, 2, 3, 4, 5),
    bit_seq!(PredMode::LDPred;        T, T, T, T, F;        0, 1, 2, 3, 6),
    bit_seq!(PredMode::VLPred;        T, T, T, T, T, F;     0, 1, 2, 3, 6, 7),
    bit_seq!(PredMode::HDPred;        T, T, T, T, T, T, F;  0, 1, 2, 3, 6, 7, 8),
    bit_seq!(PredMode::HUPred;        T, T, T, T, T, T, T;  0, 1, 2, 3, 6, 7, 8),
];

const MV_REF_TREE: &[TokenSeq<VPMBType>] = &[
    bit_seq!(VPMBType::InterNoMV;       F;              0),
    bit_seq!(VPMBType::InterNearest;    T, F;           0, 1),
    bit_seq!(VPMBType::InterNear;       T, T, F;        0, 1, 2),
    bit_seq!(VPMBType::InterMV;         T, T, T, F;     0, 1, 2, 3),
    bit_seq!(VPMBType::InterFourMV;     T, T, T, T;     0, 1, 2, 3),
];

const COEF_TREE: &[TokenSeq<DCTToken>] = &[
    bit_seq!(DCTToken::EOB;         F;                   0),
    bit_seq!(DCTToken::Zero;        T, F;                0, 1),
    bit_seq!(DCTToken::One;         T, T, F;             0, 1, 2),
    bit_seq!(DCTToken::Two;         T, T, T, F, F;       0, 1, 2, 3, 4),
    bit_seq!(DCTToken::Three;       T, T, T, F, T, F;    0, 1, 2, 3, 4, 5),
    bit_seq!(DCTToken::Four;        T, T, T, F, T, T;    0, 1, 2, 3, 4, 5),
    bit_seq!(DCTToken::Cat1;        T, T, T, T, F, F;    0, 1, 2, 3, 6, 7),
    bit_seq!(DCTToken::Cat2;        T, T, T, T, F, T;    0, 1, 2, 3, 6, 7),
    bit_seq!(DCTToken::Cat3;        T, T, T, T, T, F, F; 0, 1, 2, 3, 6, 8, 9),
    bit_seq!(DCTToken::Cat4;        T, T, T, T, T, F, T; 0, 1, 2, 3, 6, 8, 9),
    bit_seq!(DCTToken::Cat5;        T, T, T, T, T, T, F; 0, 1, 2, 3, 6, 8, 10),
    bit_seq!(DCTToken::Cat6;        T, T, T, T, T, T, T; 0, 1, 2, 3, 6, 8, 10),
];

const MV_TREE: &[TokenSeq<i16>] = &[
    bit_seq!(0; F, F, F, F; 0, 2, 3, 4),
    bit_seq!(1; F, F, F, T; 0, 2, 3, 4),
    bit_seq!(2; F, F, T, F; 0, 2, 3, 5),
    bit_seq!(3; F, F, T, T; 0, 2, 3, 5),
    bit_seq!(4; F, T, F, F; 0, 2, 6, 7),
    bit_seq!(5; F, T, F, T; 0, 2, 6, 7),
    bit_seq!(6; F, T, T, F; 0, 2, 6, 8),
    bit_seq!(7; F, T, T, T; 0, 2, 6, 8),
    bit_seq!(8; T;          0),
];

const MV_SPLIT_MODE_TREE: &[TokenSeq<MVSplitMode>] = &[
    bit_seq!(MVSplitMode::Sixteenths;   F;          0),
    bit_seq!(MVSplitMode::Quarters;     T, F;       0, 1),
    bit_seq!(MVSplitMode::TopBottom;    T, T, F;    0, 1, 2),
    bit_seq!(MVSplitMode::LeftRight;    T, T, T;    0, 1, 2),
];

const SUB_MV_REF_TREE: &[TokenSeq<SubMVRef>] = &[
    bit_seq!(SubMVRef::Left;    F;          0),
    bit_seq!(SubMVRef::Above;   T, F;       0, 1),
    bit_seq!(SubMVRef::Zero;    T, T, F;    0, 1, 2),
    bit_seq!(SubMVRef::New;     T, T, T;    0, 1, 2),
];

const FEATURE_TREE: &[TokenSeq<u8>] = &[
    bit_seq!(0; F, F; 0, 1),
    bit_seq!(1; F, T; 0, 1),
    bit_seq!(2; T, F; 0, 2),
    bit_seq!(3; T, T; 0, 2)
];

pub trait VP7BoolEncoder {
    fn put_byte(&mut self, val: u8) -> EncoderResult<()>;
    fn write_large_coef(&mut self, val: i16, cat: usize) -> EncoderResult<()>;
    fn encode_subblock(&mut self, blk: &[i16; 16], ctype: usize, pctx: u8, models: &VP7Models) -> EncoderResult<()>;
    fn encode_mv_component(&mut self, val: i16, probs: &[u8; 17]) -> EncoderResult<()>;
    fn encode_mv(&mut self, mv: MV, models: &VP7Models) -> EncoderResult<()>;
    fn encode_sub_mv(&mut self, stype: SubMVRef, mv: MV, models: &VP7Models) -> EncoderResult<()>;
    fn encode_feature(&mut self, id: usize, feat: Option<u8>, models: &VP7Models) -> EncoderResult<()>;
    fn encode_mb_type(&mut self, is_intra: bool, mb_type: &MBType, models: &VP7Models) -> EncoderResult<()>;
}

impl<'a, 'b> VP7BoolEncoder for BoolEncoder<'a, 'b> {
    fn put_byte(&mut self, val: u8) -> EncoderResult<()> {
        self.put_bits(u32::from(val), 8)
    }
    fn write_large_coef(&mut self, val: i16, cat: usize) -> EncoderResult<()> {
        let base = VP56_COEF_BASE[cat];
        let mut probs = VP56_COEF_ADD_PROBS[cat].iter();
        let add = val.abs() - base;
        let mut mask = 1 << (VP6_COEF_ADD_BITS[cat] - 1);
        while mask != 0 {
            self.put_bool((add & mask) != 0, *probs.next().unwrap())?;
            mask >>= 1;
        }
        self.put_bool(val < 0, 128)?;

        Ok(())
    }
    fn encode_subblock(&mut self, blk: &[i16; 16], ctype: usize, pctx: u8, models: &VP7Models) -> EncoderResult<()> {
        let probs = &models.coef_probs[ctype];

        let start = if ctype != 0 { 0 } else { 1 };
        let mut cval = pctx as usize;

        let mut last = 16;
        for &idx in DEFAULT_SCAN_ORDER.iter().skip(start) {
            if blk[idx] != 0 {
                last = idx;
            }
        }

        if last == 16 {
            self.write_el(DCTToken::EOB, COEF_TREE, &probs[COEF_BANDS[start]][cval])?;
            return Ok(());
        }

        for i in start..16 {
            let val = blk[DEFAULT_SCAN_ORDER[i]];
            let token = match val.abs() {
                    0 => DCTToken::Zero,
                    1 => DCTToken::One,
                    2 => DCTToken::Two,
                    3 => DCTToken::Three,
                    4 => DCTToken::Four,
                    5..=6 => DCTToken::Cat1,
                    7 ..=10 => DCTToken::Cat2,
                    11..=18 => DCTToken::Cat3,
                    19..=34 => DCTToken::Cat4,
                    35..=66 => DCTToken::Cat5,
                    _       => DCTToken::Cat6,
                };
            self.write_el(token, COEF_TREE, &probs[COEF_BANDS[i]][cval])?;
            match token {
                DCTToken::Zero => {},
                DCTToken::One |
                DCTToken::Two |
                DCTToken::Three |
                DCTToken::Four => self.put_bool(val < 0, 128)?,
                DCTToken::Cat1 => self.write_large_coef(val, 0)?,
                DCTToken::Cat2 => self.write_large_coef(val, 1)?,
                DCTToken::Cat3 => self.write_large_coef(val, 2)?,
                DCTToken::Cat4 => self.write_large_coef(val, 3)?,
                DCTToken::Cat5 => self.write_large_coef(val, 4)?,
                DCTToken::Cat6 => self.write_large_coef(val, 5)?,
                DCTToken::EOB => {},
            };
            cval = val.abs().min(2) as usize;

            if DEFAULT_SCAN_ORDER[i] == last {
                if DEFAULT_SCAN_ORDER[i] != 15 {
                    self.write_el(DCTToken::EOB, COEF_TREE, &probs[COEF_BANDS[i + 1]][cval])?;
                }
                break;
            }
        }
        Ok(())
    }
    fn encode_mv_component(&mut self, val: i16, probs: &[u8; 17]) -> EncoderResult<()> {
        let aval = val.abs();
        self.write_el(aval.min(8), MV_TREE, probs)?;
        if aval >= 8 {
            for &ord in LONG_VECTOR_ORDER.iter() {
                self.put_bool(((aval >> ord) & 1) != 0, probs[ord + 9])?;
            }
            if (aval & 0xF0) != 0 {
                self.put_bool((aval & (1 << 3)) != 0, probs[3 + 9])?;
            }
        }
        if val != 0 {
            self.put_bool(val < 0, probs[1])?;
        }
        Ok(())
    }
    fn encode_mv(&mut self, mv: MV, models: &VP7Models) -> EncoderResult<()> {
        self.encode_mv_component(mv.y, &models.mv_probs[0])?;
        self.encode_mv_component(mv.x, &models.mv_probs[1])?;
        Ok(())
    }
    fn encode_sub_mv(&mut self, stype: SubMVRef, mv: MV, models: &VP7Models) -> EncoderResult<()> {
        self.write_el(stype, SUB_MV_REF_TREE, &SUB_MV_REF_PROBS)?;
        if stype == SubMVRef::New {
            self.encode_mv_component(mv.y, &models.mv_probs[0])?;
            self.encode_mv_component(mv.x, &models.mv_probs[1])?;
        }
        Ok(())
    }
    fn encode_feature(&mut self, id: usize, feat: Option<u8>, models: &VP7Models) -> EncoderResult<()> {
        self.put_bool(feat.is_some(), models.feature_present[id])?;
        if let Some(num) = feat {
            self.write_el(num, FEATURE_TREE, &models.feature_tree_probs[id])?;
        }
        Ok(())
    }
    fn encode_mb_type(&mut self, is_intra: bool, mb_type: &MBType, models: &VP7Models) -> EncoderResult<()> {
        if !is_intra {
            self.put_bool(!mb_type.is_intra(), models.prob_intra_pred)?;
            if !mb_type.is_intra() {
                let last = mb_type.get_last();
                self.put_bool(!last, models.prob_last_pred)?;
            }
        }
        match *mb_type {
            MBType::Intra(ymode, uvmode) => {
                if is_intra {
                    self.write_el(ymode, KF_Y_MODE_TREE, KF_Y_MODE_TREE_PROBS)?;
                    self.write_el(uvmode, UV_MODE_TREE, KF_UV_MODE_TREE_PROBS)?;
                } else {
                    self.write_el(ymode, Y_MODE_TREE, &models.kf_ymode_prob)?;
                    self.write_el(uvmode, UV_MODE_TREE, &models.kf_uvmode_prob)?;
                }
            },
            MBType::Intra4x4(ymodes, yctx, uvmode) => {
                if is_intra {
                    self.write_el(PredMode::BPred, KF_Y_MODE_TREE, KF_Y_MODE_TREE_PROBS)?;
                    for (&ypred, &yctx) in ymodes.iter().zip(yctx.iter()) {
                        let top_idx  = (yctx / 10) as usize;
                        let left_idx = (yctx % 10) as usize;
                        self.write_el(ypred, B_MODE_TREE, &KF_B_MODE_TREE_PROBS[top_idx][left_idx])?;
                    }
                    self.write_el(uvmode, UV_MODE_TREE, KF_UV_MODE_TREE_PROBS)?;
                } else {
                    self.write_el(PredMode::BPred, Y_MODE_TREE, &models.kf_ymode_prob)?;
                    for &ypred in ymodes.iter() {
                        self.write_el(ypred, B_MODE_TREE, B_MODE_TREE_PROBS)?;
                    }
                    self.write_el(uvmode, UV_MODE_TREE, &models.kf_uvmode_prob)?;
                }
            },
            MBType::InterNoMV(_last, ref mv_probs) => {
                self.write_el(VPMBType::InterNoMV, MV_REF_TREE, mv_probs)?;
            },
            MBType::InterNearest(_last, ref mv_probs) => {
                self.write_el(VPMBType::InterNearest, MV_REF_TREE, mv_probs)?;
            },
            MBType::InterNear(_last, ref mv_probs) => {
                self.write_el(VPMBType::InterNear, MV_REF_TREE, mv_probs)?;
            },
            MBType::InterMV(_last, ref mv_probs, mv) => {
                self.write_el(VPMBType::InterMV, MV_REF_TREE, mv_probs)?;
                self.encode_mv(mv, models)?;
            },
            MBType::InterSplitMV(_last, ref mv_probs, split_mode, stypes, mvs) => {
                self.write_el(VPMBType::InterFourMV, MV_REF_TREE, mv_probs)?;
                self.write_el(split_mode, MV_SPLIT_MODE_TREE, &MV_SPLIT_MODE_PROBS)?;
                match split_mode {
                    MVSplitMode::TopBottom | MVSplitMode::LeftRight => {
                        for (&stype, &mv) in stypes.iter().zip(mvs.iter()).take(2) {
                            self.encode_sub_mv(stype, mv, models)?;
                        }
                    },
                    MVSplitMode::Quarters => {
                        for (&stype, &mv) in stypes.iter().zip(mvs.iter()).take(4) {
                            self.encode_sub_mv(stype, mv, models)?;
                        }
                    },
                    MVSplitMode::Sixteenths => {
                        for (&stype, &mv) in stypes.iter().zip(mvs.iter()) {
                            self.encode_sub_mv(stype, mv, models)?;
                        }
                    },
                };
            },
        };
        Ok(())
    }
}

pub fn encode_dct_coef_prob_upd(bc: &mut BoolEncoder, coef_probs: &[[[[u8; 11]; 3]; 8]; 4], prev: &[[[[u8; 11]; 3]; 8]; 4]) -> EncoderResult<()> {
    for ((new, old), upd) in coef_probs.iter().zip(prev.iter()).zip(DCT_UPDATE_PROBS.iter()) {
        for ((new, old), upd) in new.iter().zip(old.iter()).zip(upd.iter()) {
            for ((new, old), upd) in new.iter().zip(old.iter()).zip(upd.iter()) {
                for ((&new, &old), &upd) in new.iter().zip(old.iter()).zip(upd.iter()) {
                    bc.put_bool(new != old, upd)?;
                    if new != old {
                        bc.put_byte(new)?;
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn encode_mv_prob_upd(bc: &mut BoolEncoder, mv_probs: &[[u8; 17]; 2], prev: &[[u8; 17]; 2]) -> EncoderResult<()> {
    for ((new, old), upd) in mv_probs.iter().zip(prev.iter()).zip(MV_UPDATE_PROBS.iter()) {
        for ((&new, &old), &upd) in new.iter().zip(old.iter()).zip(upd.iter()) {
            bc.put_bool(new != old, upd)?;
            if new != old {
                bc.put_bits(u32::from(new) >> 1, 7)?;
            }
        }
    }
    Ok(())
}

pub trait VP7Estimator {
    fn estimate_subblock(&self, blk: &[i16; 16], ctype: usize, pctx: u8, models: &mut VP7ModelsStat);
    fn estimate_mv_component(&self, val: i16, probs: &mut [ProbCounter; 17]);
    fn estimate_mv(&self, mv: MV, models: &mut VP7ModelsStat);
    fn estimate_sub_mv(&self, stype: SubMVRef, mv: MV, models: &mut VP7ModelsStat);
    fn estimate_mb_type(&self, is_intra: bool, mb_type: &MBType, models: &mut VP7ModelsStat);
    fn estimate_feature(&self, id: usize, feat: Option<u8>, models: &mut VP7ModelsStat);
}

impl VP7Estimator for Estimator {
    fn estimate_subblock(&self, blk: &[i16; 16], ctype: usize, pctx: u8, models: &mut VP7ModelsStat) {
        let probs = &mut models.coef_probs[ctype];

        let start = if ctype != 0 { 0 } else { 1 };
        let mut cval = pctx as usize;

        let mut last = 16;
        for &idx in DEFAULT_SCAN_ORDER.iter().skip(start) {
            if blk[idx] != 0 {
                last = idx;
            }
        }

        if last == 16 {
            self.write_el(DCTToken::EOB, COEF_TREE, &mut probs[COEF_BANDS[start]][cval]);
            return;
        }

        for i in start..16 {
            let val = blk[DEFAULT_SCAN_ORDER[i]];
            let token = match val.abs() {
                    0 => DCTToken::Zero,
                    1 => DCTToken::One,
                    2 => DCTToken::Two,
                    3 => DCTToken::Three,
                    4 => DCTToken::Four,
                    5..=6 => DCTToken::Cat1,
                    7 ..=10 => DCTToken::Cat2,
                    11..=18 => DCTToken::Cat3,
                    19..=34 => DCTToken::Cat4,
                    35..=66 => DCTToken::Cat5,
                    _       => DCTToken::Cat6,
                };
            self.write_el(token, COEF_TREE, &mut probs[COEF_BANDS[i]][cval]);
            cval = val.abs().min(2) as usize;

            if DEFAULT_SCAN_ORDER[i] == last {
                if DEFAULT_SCAN_ORDER[i] != 15 {
                    self.write_el(DCTToken::EOB, COEF_TREE, &mut probs[COEF_BANDS[i + 1]][cval]);
                }
                break;
            }
        }
    }
    fn estimate_mv_component(&self, val: i16, probs: &mut [ProbCounter; 17]) {
        let aval = val.abs();
        self.write_el(aval.min(8), MV_TREE, probs);
        if aval >= 8 {
            for &ord in LONG_VECTOR_ORDER.iter() {
                probs[ord + 9].add(((aval >> ord) & 1) != 0);
            }
            if (aval & 0xF0) != 0 {
                probs[3 + 9].add((aval & (1 << 3)) != 0);
            }
        }
        if val != 0 {
            probs[1].add(val < 0);
        }
    }
    fn estimate_mv(&self, mv: MV, models: &mut VP7ModelsStat) {
        self.estimate_mv_component(mv.y, &mut models.mv_probs[0]);
        self.estimate_mv_component(mv.x, &mut models.mv_probs[1]);
    }
    fn estimate_sub_mv(&self, stype: SubMVRef, mv: MV, models: &mut VP7ModelsStat) {
        if stype == SubMVRef::New {
            self.estimate_mv_component(mv.y, &mut models.mv_probs[0]);
            self.estimate_mv_component(mv.x, &mut models.mv_probs[1]);
        }
    }
    fn estimate_mb_type(&self, is_intra: bool, mb_type: &MBType, models: &mut VP7ModelsStat) {
        if !is_intra {
            models.prob_intra_pred.add(!mb_type.is_intra());
            if !mb_type.is_intra() {
                let last = mb_type.get_last();
                models.prob_last_pred.add(!last);
            }
        }
        match *mb_type {
            MBType::Intra(ymode, cmode) => {
                if !is_intra {
                    self.write_el(ymode, Y_MODE_TREE, &mut models.kf_ymode_prob);
                    self.write_el(cmode, UV_MODE_TREE, &mut models.kf_uvmode_prob);
                }
            },
            MBType::Intra4x4(_, _, cmode) => {
                if !is_intra {
                    self.write_el(PredMode::BPred, Y_MODE_TREE, &mut models.kf_ymode_prob);
                    self.write_el(cmode, UV_MODE_TREE, &mut models.kf_uvmode_prob);
                }
            },
            MBType::InterMV(_last, _, mv) => {
                self.estimate_mv(mv, models);
            },
            MBType::InterSplitMV(_last, _, split_mode, stypes, mvs) => {
                match split_mode {
                    MVSplitMode::TopBottom | MVSplitMode::LeftRight => {
                        for (&stype, &mv) in stypes.iter().zip(mvs.iter()).take(2) {
                            self.estimate_sub_mv(stype, mv, models);
                        }
                    },
                    MVSplitMode::Quarters => {
                        for (&stype, &mv) in stypes.iter().zip(mvs.iter()).take(4) {
                            self.estimate_sub_mv(stype, mv, models);
                        }
                    },
                    MVSplitMode::Sixteenths => {
                        for (&stype, &mv) in stypes.iter().zip(mvs.iter()) {
                            self.estimate_sub_mv(stype, mv, models);
                        }
                    },
                };
            },
            _ => {},
        };
    }
    fn estimate_feature(&self, id: usize, feat: Option<u8>, models: &mut VP7ModelsStat) {
        models.feature_present[id].add(feat.is_some());
        if let Some(num) = feat {
            self.write_el(num, FEATURE_TREE, &mut models.feature_tree_probs[id]);
        }
    }
}

fn code_nits<T: PartialEq>(el: T, tree: &[TokenSeq<T>], probs: &[u8]) -> u32 {
    let mut nits = 0;
    for entry in tree.iter() {
        if entry.val == el {
            for seq in entry.seq.iter() {
                nits += Estimator::est_nits(seq.bit, probs[seq.idx as usize]);
            }
            return nits;
        }
    }
    0
}
pub fn b_mode_nits(mode: PredMode) -> u32 {
    code_nits(mode, B_MODE_TREE, &KF_B_MODE_TREE_PROBS[2][2]) // todo find better context
}
fn mv_component_nits(val: i16, probs: &[u8; 17]) -> u32 {
    let aval = val.abs();
    let mut nits = code_nits(aval.min(8), MV_TREE, probs);
    if aval >= 8 {
        for &ord in LONG_VECTOR_ORDER.iter() {
            nits += Estimator::est_nits(((aval >> ord) & 1) != 0, probs[ord + 9]);
        }
        if (aval & 0xF0) != 0 {
            nits += Estimator::est_nits((aval & (1 << 3)) != 0, probs[3 + 9]);
        }
    }
    if val != 0 {
        nits += u32::from(PROB_BITS[128]);
    }
    nits
}
pub fn inter_mv_nits(mv: MV, mvprobs: &[u8; 4], nearest_mv: MV, near_mv: MV, pred_mv: MV, models: &VP7Models) -> u32 {
    if mv == ZERO_MV {
        code_nits(VPMBType::InterNoMV, MV_REF_TREE, mvprobs)
    } else if mv == nearest_mv {
        code_nits(VPMBType::InterNearest, MV_REF_TREE, mvprobs)
    } else if mv == near_mv {
        code_nits(VPMBType::InterNear, MV_REF_TREE, mvprobs)
    } else {
        let dmv = mv - pred_mv;
        let mut nits = code_nits(VPMBType::InterMV, MV_REF_TREE, mvprobs);
        nits += mv_component_nits(dmv.y, &models.mv_probs[0]);
        nits += mv_component_nits(dmv.x, &models.mv_probs[1]);
        nits
    }
}
pub fn sub_mv_mode_nits(mode: MVSplitMode) -> u32 {
    code_nits(mode, MV_SPLIT_MODE_TREE, &MV_SPLIT_MODE_PROBS)
}
pub fn sub_mv_nits(mv: MV, left_mv: MV, top_mv: MV, pred_mv: MV, models: &VP7Models) -> (SubMVRef, u32) {
    if mv == ZERO_MV {
        (SubMVRef::Zero, code_nits(SubMVRef::Zero, SUB_MV_REF_TREE, &SUB_MV_REF_PROBS))
    } else if mv == left_mv {
        (SubMVRef::Left, code_nits(SubMVRef::Left, SUB_MV_REF_TREE, &SUB_MV_REF_PROBS))
    } else if mv == top_mv {
        (SubMVRef::Above, code_nits(SubMVRef::Above, SUB_MV_REF_TREE, &SUB_MV_REF_PROBS))
    } else {
        let dmv = mv - pred_mv;
        let mut nits = code_nits(SubMVRef::New, SUB_MV_REF_TREE, &SUB_MV_REF_PROBS);
        nits += mv_component_nits(dmv.y, &models.mv_probs[0]);
        nits += mv_component_nits(dmv.x, &models.mv_probs[1]);
        (SubMVRef::New, nits)
    }
}
fn est_large_coef(val: i16, cat: usize) -> u32 {
    let base = VP56_COEF_BASE[cat];
    let mut probs = VP56_COEF_ADD_PROBS[cat].iter();
    let add = val.abs() - base;
    let mut mask = 1 << (VP6_COEF_ADD_BITS[cat] - 1);
    let mut nits = 0;
    while mask != 0 {
        nits += Estimator::est_nits((add & mask) != 0, *probs.next().unwrap());
        mask >>= 1;
    }
    nits += u32::from(PROB_BITS[128]);

    nits
}

pub fn estimate_subblock_nits(blk: &[i16; 16], ctype: usize, pctx: u8, probs: &[[[u8; 11]; 3]; 8]) -> u32 {
    let start = if ctype != 0 { 0 } else { 1 };
    let mut cval = pctx as usize;

    let mut last = 16;
    for &idx in DEFAULT_SCAN_ORDER.iter().skip(start) {
        if blk[idx] != 0 {
            last = idx;
        }
    }

    if last == 16 {
        return code_nits(DCTToken::EOB, COEF_TREE, &probs[COEF_BANDS[start]][cval]);
    }

    let mut nits = 0;
    for i in start..16 {
        let val = blk[DEFAULT_SCAN_ORDER[i]];
        let token = match val.abs() {
                0 => DCTToken::Zero,
                1 => DCTToken::One,
                2 => DCTToken::Two,
                3 => DCTToken::Three,
                4 => DCTToken::Four,
                5..=6 => DCTToken::Cat1,
                7 ..=10 => DCTToken::Cat2,
                11..=18 => DCTToken::Cat3,
                19..=34 => DCTToken::Cat4,
                35..=66 => DCTToken::Cat5,
                _       => DCTToken::Cat6,
            };
        nits += code_nits(token, COEF_TREE, &probs[COEF_BANDS[i]][cval]);
        nits += match token {
                DCTToken::Zero => 0,
                DCTToken::One |
                DCTToken::Two |
                DCTToken::Three |
                DCTToken::Four => u32::from(PROB_BITS[128]),
                DCTToken::Cat1 => est_large_coef(val, 0),
                DCTToken::Cat2 => est_large_coef(val, 1),
                DCTToken::Cat3 => est_large_coef(val, 2),
                DCTToken::Cat4 => est_large_coef(val, 3),
                DCTToken::Cat5 => est_large_coef(val, 4),
                DCTToken::Cat6 => est_large_coef(val, 5),
                DCTToken::EOB => 0,
            };
        cval = val.abs().min(2) as usize;

        if DEFAULT_SCAN_ORDER[i] == last {
            if DEFAULT_SCAN_ORDER[i] != 15 {
                nits += code_nits(DCTToken::EOB, COEF_TREE, &probs[COEF_BANDS[i + 1]][cval]);
            }
            break;
        }
    }
    nits
}

const VP6_COEF_ADD_BITS: [u8; 6] = [ 1, 2, 3, 4, 5, 11 ];
