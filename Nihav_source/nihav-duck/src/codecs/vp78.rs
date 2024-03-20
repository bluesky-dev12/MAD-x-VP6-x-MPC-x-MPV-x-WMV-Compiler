use nihav_codec_support::data::GenericCache;
use super::vpcommon::*;

pub enum VPTreeDef<T: Copy> {
    Index(u8),
    Value(T),
}

pub trait VPTreeReader {
    fn read_tree<T:Copy>(&mut self, tree_def: &[VPTreeDef<T>], tree_prob: &[u8]) -> T;
}

impl<'a> VPTreeReader for BoolCoder<'a> {
    fn read_tree<T:Copy>(&mut self, tree_def: &[VPTreeDef<T>], tree_prob: &[u8]) -> T {
        let mut idx = 0;

        loop {
            let bit = self.read_prob(tree_prob[idx >> 1]);
            match tree_def[idx + (bit as usize)] {
                VPTreeDef::Value(v) => return v,
                VPTreeDef::Index(ix) => { idx = ix as usize; },
            };
        }
    }
}

#[repr(u8)]
#[derive(Clone,Copy,PartialEq,Debug,Default)]
pub enum PredMode {
    #[default]
    DCPred,
    HPred,
    VPred,
    TMPred,
    BPred,

    //sub-block prediction modes
    LDPred,
    RDPred,
    VRPred,
    VLPred,
    HDPred,
    HUPred,

    Inter,
}

impl PredMode {
    pub fn to_b_mode(self) -> Self {
        if self == PredMode::DCPred {
            self
        } else {
            PredMode::TMPred
        }
    }
    pub fn to_b_index(self) -> usize {
        match self {
            PredMode::DCPred    => 0,
            PredMode::TMPred    => 1,
            PredMode::VPred     => 2,
            PredMode::HPred     => 3,
            PredMode::LDPred    => 4,
            PredMode::RDPred    => 5,
            PredMode::VRPred    => 6,
            PredMode::VLPred    => 7,
            PredMode::HDPred    => 8,
            PredMode::HUPred    => 9,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone,Copy,PartialEq)]
pub enum DCTToken {
    Zero,
    One,
    Two,
    Three,
    Four,
    Cat1,
    Cat2,
    Cat3,
    Cat4,
    Cat5,
    Cat6,
    EOB,
}

pub fn expand_token(bc: &mut BoolCoder, token: DCTToken) -> i16 {
    let cat;
    match token {
        DCTToken::Zero  => return 0,
        DCTToken::One   => return if bc.read_bool() { -1 } else { 1 },
        DCTToken::Two   => return if bc.read_bool() { -2 } else { 2 },
        DCTToken::Three => return if bc.read_bool() { -3 } else { 3 },
        DCTToken::Four  => return if bc.read_bool() { -4 } else { 4 },
        DCTToken::Cat1  => cat = 0,
        DCTToken::Cat2  => cat = 1,
        DCTToken::Cat3  => cat = 2,
        DCTToken::Cat4  => cat = 3,
        DCTToken::Cat5  => cat = 4,
        DCTToken::Cat6  => cat = 5,
        _ => unreachable!(),
    };
    let mut add = 0i16;
    let add_probs = &VP56_COEF_ADD_PROBS[cat];
    for prob in add_probs.iter() {
        if *prob == 128 { break; }
        add                                 = (add << 1) | (bc.read_prob(*prob) as i16);
    }
    let sign                                = bc.read_bool();
    let level = VP56_COEF_BASE[cat] + add;
    if !sign {
        level
    } else {
        -level
    }
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum MVSplitMode {
    TopBottom,
    LeftRight,
    Quarters,
    Sixteenths,
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum SubMVRef {
    Left,
    Above,
    New,
    Zero,
}

pub struct PredCache {
    pub y_pred:         GenericCache<u8>,
    pub u_pred:         GenericCache<u8>,
    pub v_pred:         GenericCache<u8>,
    pub y2_pred:        GenericCache<u8>,
    pub y_pred_left:    [u8; 4],
    pub u_pred_left:    [u8; 2],
    pub v_pred_left:    [u8; 2],
    pub y2_pred_left:   u8,
}

impl PredCache {
    pub fn new() -> Self {
        Self {
            y_pred:         GenericCache::new(1, 1, 0),
            u_pred:         GenericCache::new(1, 1, 0),
            v_pred:         GenericCache::new(1, 1, 0),
            y2_pred:        GenericCache::new(1, 1, 0),
            y_pred_left:    [0; 4],
            u_pred_left:    [0; 2],
            v_pred_left:    [0; 2],
            y2_pred_left:   0,
        }
    }
    pub fn resize(&mut self, mb_w: usize) {
        self.y_pred     = GenericCache::new(4, mb_w * 4 + 1, 0);
        self.u_pred     = GenericCache::new(2, mb_w * 2 + 1, 0);
        self.v_pred     = GenericCache::new(2, mb_w * 2 + 1, 0);
        self.y2_pred    = GenericCache::new(1, mb_w     + 1, 0);
    }
    pub fn reset(&mut self) {
        self.y_pred.reset();
        self.u_pred.reset();
        self.v_pred.reset();
        self.y2_pred.reset();
        self.y_pred_left = [0; 4];
        self.u_pred_left = [0; 2];
        self.v_pred_left = [0; 2];
        self.y2_pred_left = 0;
    }
    pub fn update_row(&mut self) {
        self.y_pred.update_row();
        self.u_pred.update_row();
        self.v_pred.update_row();
        self.y2_pred.update_row();
    }
    pub fn reset_left(&mut self) {
        self.y_pred_left = [0; 4];
        self.u_pred_left = [0; 2];
        self.v_pred_left = [0; 2];
        self.y2_pred_left = 0;
    }
}

pub const Y_MODE_TREE: &[VPTreeDef<PredMode>] = &[
    VPTreeDef::Value(PredMode::DCPred), VPTreeDef::Index(2),
    VPTreeDef::Index(4),                VPTreeDef::Index(6),
    VPTreeDef::Value(PredMode::VPred),  VPTreeDef::Value(PredMode::HPred),
    VPTreeDef::Value(PredMode::TMPred), VPTreeDef::Value(PredMode::BPred),
];
pub const KF_Y_MODE_TREE: &[VPTreeDef<PredMode>] = &[
    VPTreeDef::Value(PredMode::BPred),  VPTreeDef::Index(2),
    VPTreeDef::Index(4),                VPTreeDef::Index(6),
    VPTreeDef::Value(PredMode::DCPred), VPTreeDef::Value(PredMode::VPred),
    VPTreeDef::Value(PredMode::HPred),  VPTreeDef::Value(PredMode::TMPred),
];
pub const UV_MODE_TREE: &[VPTreeDef<PredMode>] = &[
    VPTreeDef::Value(PredMode::DCPred), VPTreeDef::Index(2),
    VPTreeDef::Value(PredMode::VPred),  VPTreeDef::Index(4),
    VPTreeDef::Value(PredMode::HPred),  VPTreeDef::Value(PredMode::TMPred)
];
pub const B_MODE_TREE: &[VPTreeDef<PredMode>] = &[
    VPTreeDef::Value(PredMode::DCPred), VPTreeDef::Index(2),
    VPTreeDef::Value(PredMode::TMPred), VPTreeDef::Index(4),
    VPTreeDef::Value(PredMode::VPred),  VPTreeDef::Index(6),
    VPTreeDef::Index(8),                VPTreeDef::Index(12),
    VPTreeDef::Value(PredMode::HPred),  VPTreeDef::Index(10),
    VPTreeDef::Value(PredMode::RDPred), VPTreeDef::Value(PredMode::VRPred),
    VPTreeDef::Value(PredMode::LDPred), VPTreeDef::Index(14),
    VPTreeDef::Value(PredMode::VLPred), VPTreeDef::Index(16),
    VPTreeDef::Value(PredMode::HDPred), VPTreeDef::Value(PredMode::HUPred)
];

pub const FEATURE_TREE: &[VPTreeDef<usize>] = &[
    VPTreeDef::Index(2), VPTreeDef::Index(4),
    VPTreeDef::Value(0), VPTreeDef::Value(1),
    VPTreeDef::Value(2), VPTreeDef::Value(3)
];

pub const COEF_TREE: &[VPTreeDef<DCTToken>] = &[
    VPTreeDef::Value(DCTToken::EOB),    VPTreeDef::Index(2),
    VPTreeDef::Value(DCTToken::Zero),   VPTreeDef::Index(4),
    VPTreeDef::Value(DCTToken::One),    VPTreeDef::Index(6),
    VPTreeDef::Index(8),                VPTreeDef::Index(12),
    VPTreeDef::Value(DCTToken::Two),    VPTreeDef::Index(10),
    VPTreeDef::Value(DCTToken::Three),  VPTreeDef::Value(DCTToken::Four),
    VPTreeDef::Index(14),               VPTreeDef::Index(16),
    VPTreeDef::Value(DCTToken::Cat1),   VPTreeDef::Value(DCTToken::Cat2),
    VPTreeDef::Index(18),               VPTreeDef::Index(20),
    VPTreeDef::Value(DCTToken::Cat3),   VPTreeDef::Value(DCTToken::Cat4),
    VPTreeDef::Value(DCTToken::Cat5),   VPTreeDef::Value(DCTToken::Cat6)
];

pub const MV_REF_TREE: &[VPTreeDef<VPMBType>] = &[
    VPTreeDef::Value(VPMBType::InterNoMV),      VPTreeDef::Index(2),
    VPTreeDef::Value(VPMBType::InterNearest),   VPTreeDef::Index(4),
    VPTreeDef::Value(VPMBType::InterNear),      VPTreeDef::Index(6),
    VPTreeDef::Value(VPMBType::InterMV),        VPTreeDef::Value(VPMBType::InterFourMV)
];
pub const SMALL_MV_TREE: &[VPTreeDef<i16>] = &[
    VPTreeDef::Index(2),    VPTreeDef::Index(8),
    VPTreeDef::Index(4),    VPTreeDef::Index(6),
    VPTreeDef::Value(0),    VPTreeDef::Value(1),
    VPTreeDef::Value(2),    VPTreeDef::Value(3),
    VPTreeDef::Index(10),   VPTreeDef::Index(12),
    VPTreeDef::Value(4),    VPTreeDef::Value(5),
    VPTreeDef::Value(6),    VPTreeDef::Value(7)
];
pub const MV_SPLIT_MODE_TREE: &[VPTreeDef<MVSplitMode>] = &[
    VPTreeDef::Value(MVSplitMode::Sixteenths),  VPTreeDef::Index(2),
    VPTreeDef::Value(MVSplitMode::Quarters),    VPTreeDef::Index(4),
    VPTreeDef::Value(MVSplitMode::TopBottom),   VPTreeDef::Value(MVSplitMode::LeftRight)
];
pub const SUB_MV_REF_TREE: &[VPTreeDef<SubMVRef>] = &[
    VPTreeDef::Value(SubMVRef::Left),   VPTreeDef::Index(2),
    VPTreeDef::Value(SubMVRef::Above),  VPTreeDef::Index(4),
    VPTreeDef::Value(SubMVRef::Zero),   VPTreeDef::Value(SubMVRef::New)
];

