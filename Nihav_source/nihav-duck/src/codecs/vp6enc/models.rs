use nihav_codec_support::codecs::ZIGZAG;
use super::super::vp6data::*;
use crate::codecs::vpenc::models::*;
pub use crate::codecs::vpenc::models::PROB_BITS;

#[derive(Clone,Copy,Default)]
pub struct VP56MVModel {
    pub nz_prob:        u8,
    pub sign_prob:      u8,
    pub raw_probs:      [u8; 8],
    pub tree_probs:     [u8; 7],
}

#[derive(Clone,Copy,Default)]
pub struct VP56MBTypeModel {
    pub probs:          [u8; 10],
}

#[derive(Clone,Copy,Default)]
pub struct VP56CoeffModel {
    pub dc_token_probs: [[[u8; 5]; 6]; 6],
    pub dc_value_probs: [u8; 11],
    pub ac_val_probs:   [[[u8; 11]; 6]; 3],
}

#[derive(Clone)]
pub struct VP6Models {
    pub scan_order:         [usize; 64],
    pub scan:               [usize; 64],
    pub zigzag:             [usize; 64],
    pub zero_run_probs:     [[u8; 14]; 2],
}

const MAX_HUFF_ELEMS: usize = 12;
#[derive(Clone,Copy,Default)]
pub struct VP6Huff {
    pub codes:  [u16; MAX_HUFF_ELEMS],
    pub bits:   [u8; MAX_HUFF_ELEMS],
}

#[derive(Clone,Copy,Default)]
struct Node {
    weight:     u16,
    sym:        i8,
    ch0:        usize,
    ch1:        usize,
}

fn prob2weight(a: u8, b: u8) -> u8 {
    let w = ((u16::from(a) * u16::from(b)) >> 8) as u8;
    if w == 0 {
        1
    } else {
        w
    }
}

impl VP6Huff {
    pub fn build_codes(&mut self, probs: &[u8; 11]) {
        let mut weights = [0u8; 12];

        weights[11] = prob2weight( probs[0],  probs[ 1]);
        weights[ 0] = prob2weight( probs[0], !probs[ 1]);
        weights[ 1] = prob2weight(!probs[0],  probs[ 2]);
        let lvroot  = prob2weight(!probs[0], !probs[ 2]);
        let tworoot = prob2weight( lvroot,    probs[ 3]);
        let hlroot  = prob2weight( lvroot,   !probs[ 3]);
        weights[ 2] = prob2weight( tworoot,   probs[ 4]);
        let root34  = prob2weight( tworoot,  !probs[ 4]);
        weights[ 3] = prob2weight( root34,    probs[ 5]);
        weights[ 4] = prob2weight( root34,   !probs[ 5]);
        let c1root  = prob2weight( hlroot,    probs[ 6]);
        let c34root = prob2weight( hlroot,   !probs[ 6]);
        weights[ 5] = prob2weight( c1root,    probs[ 7]);
        weights[ 6] = prob2weight( c1root,   !probs[ 7]);
        let c3root  = prob2weight( c34root,   probs[ 8]);
        let c4root  = prob2weight( c34root,  !probs[ 8]);
        weights[ 7] = prob2weight( c3root,    probs[ 9]);
        weights[ 8] = prob2weight( c3root,   !probs[ 9]);
        weights[ 9] = prob2weight( c4root,    probs[10]);
        weights[10] = prob2weight( c4root,   !probs[10]);

        self.build(&weights);
    }
    pub fn build_codes_zero_run(&mut self, probs: &[u8; 14]) {
        let mut weights = [0u8; 9];

        let root   = prob2weight( probs[0],  probs[1]);
        weights[0] = prob2weight( root,      probs[2]);
        weights[1] = prob2weight( root,     !probs[2]);

        let root   = prob2weight( probs[0], !probs[1]);
        weights[2] = prob2weight( root,      probs[3]);
        weights[3] = prob2weight( root,     !probs[3]);

        let root   = prob2weight(!probs[0],  probs[4]);
        weights[8] = prob2weight(!probs[0], !probs[4]);
        let root1  = prob2weight( root,      probs[5]);
        let root2  = prob2weight( root,     !probs[5]);
        weights[4] = prob2weight( root1,     probs[6]);
        weights[5] = prob2weight( root1,    !probs[6]);
        weights[6] = prob2weight( root2,     probs[7]);
        weights[7] = prob2weight( root2,    !probs[7]);

        self.build(&weights);
    }
    fn build(&mut self, weights: &[u8]) {
        let mut nodes = [Node::default(); MAX_HUFF_ELEMS * 2];
        let mut nlen = 0;

        for w in weights.iter().rev() {
            let weight = u16::from(*w);
            let mut pos = nlen;
            for i in 0..nlen {
                if nodes[i].weight > weight {
                    pos = i;
                    break;
                }
            }
            for j in (pos..nlen).rev() {
                nodes[j + 1] = nodes[j];
            }
            nodes[pos].weight = weight;
            nodes[pos].sym    = (weights.len() - nlen - 1) as i8;
            nlen += 1;
        }

        let mut low = 0;
        for _ in 0..nlen-1 {
            let nnode = Node {
                    weight: nodes[low + 0].weight + nodes[low + 1].weight,
                    sym:    -1,
                    ch0:    low + 0,
                    ch1:    low + 1,
                };
            low += 2;
            let mut pos = low;
            while (pos < nlen) && (nodes[pos].weight < nnode.weight) {
                pos += 1;
            }
            for j in (pos..nlen).rev() {
                nodes[j + 1] = nodes[j];
            }
            nodes[pos] = nnode;
            nlen += 1;
        }
        self.get_codes(&nodes, nlen - 1, 0, 0);
        for i in nlen..self.codes.len() {
            self.codes[i]   = self.codes[0];
            self.bits[i]    = self.bits[0];
        }
    }
    fn get_codes(&mut self, nodes: &[Node], pos: usize, code: u16, len: u8) {
        if nodes[pos].sym >= 0 {
            self.codes[nodes[pos].sym as usize] = code;
            self.bits [nodes[pos].sym as usize] = len;
        } else {
            self.get_codes(nodes, nodes[pos].ch0, (code << 1) | 0, len + 1);
            self.get_codes(nodes, nodes[pos].ch1, (code << 1) | 1, len + 1);
        }
    }
}

#[derive(Clone,Copy,Default)]
pub struct VP6HuffModels {
    pub dc_token_tree:      [VP6Huff; 2],
    pub ac_token_tree:      [[[VP6Huff; 6]; 3]; 2],
    pub zero_run_tree:      [VP6Huff; 2],
}

impl VP6Models {
    fn new() -> Self {
        Self {
            scan_order:         [0; 64],
            scan:               [0; 64],
            zigzag:             [0; 64],
            zero_run_probs:     [[0; 14]; 2],
        }
    }
}

#[derive(Clone)]
pub struct VP56Models {
    pub mv_models:          [VP56MVModel; 2],
    pub mbtype_models:      [[VP56MBTypeModel; 10]; 3],
    pub coeff_models:       [VP56CoeffModel; 2],
    pub prob_xmitted:       [[u8; 20]; 3],
    pub vp6models:          VP6Models,
    pub vp6huff:            VP6HuffModels,
}

impl VP56Models {
    pub fn new() -> Self {
        Self {
            mv_models:      [VP56MVModel::default(); 2],
            mbtype_models:  [[VP56MBTypeModel::default(); 10]; 3],
            coeff_models:   [VP56CoeffModel::default(); 2],
            prob_xmitted:   [[0; 20]; 3],
            vp6models:      VP6Models::new(),
            vp6huff:        VP6HuffModels::default(),
        }
    }
    pub fn reset(&mut self, interlaced: bool) {
        for (i, mdl) in self.mv_models.iter_mut().enumerate() {
            mdl.nz_prob         = NZ_PROBS[i];
            mdl.sign_prob       = 128;
            mdl.raw_probs.copy_from_slice(&RAW_PROBS[i]);
            mdl.tree_probs.copy_from_slice(&TREE_PROBS[i]);
        }

        for mdl in self.coeff_models.iter_mut() {
            mdl.dc_value_probs = [128; 11];
            mdl.ac_val_probs = [[[128; 11]; 6]; 3];
        }
        self.vp6models.zero_run_probs.copy_from_slice(&ZERO_RUN_PROBS);
        reset_scan(&mut self.vp6models, interlaced);
    }
    pub fn reset_mbtype_models(&mut self) {
        const DEFAULT_XMITTED_PROBS: [[u8; 20]; 3] = [
            [ 42,  69, 2, 1, 7, 1, 42, 44, 22, 6, 3, 1, 2, 0, 5, 1, 1, 0, 0, 0 ],
            [  8, 229, 1, 1, 8, 0,  0,  0,  0, 0, 2, 1, 1, 0, 0, 0, 1, 1, 0, 0 ],
            [ 35, 122, 1, 1, 6, 1, 34, 46,  0, 0, 2, 1, 1, 0, 1, 0, 1, 1, 0, 0 ]
        ];
        self.prob_xmitted.copy_from_slice(&DEFAULT_XMITTED_PROBS);
    }
}

pub fn reset_scan(model: &mut VP6Models, interlaced: bool) {
    if !interlaced {
        model.scan_order.copy_from_slice(&VP6_DEFAULT_SCAN_ORDER);
    } else {
        model.scan_order.copy_from_slice(&VP6_INTERLACED_SCAN_ORDER);
    }
    for i in 0..64 { model.scan[i] = i; }
    model.zigzag.copy_from_slice(&ZIGZAG);
}

#[derive(Clone,Copy,Default)]
pub struct VP56MVModelStat {
    pub nz_prob:        ProbCounter,
    pub sign_prob:      ProbCounter,
    pub raw_probs:      [ProbCounter; 8],
    pub tree_probs:     [ProbCounter; 7],
}

#[derive(Clone,Copy,Default)]
pub struct VP56CoeffModelStat {
    pub dc_token_probs: [[[ProbCounter; 5]; 6]; 6],
    pub dc_value_probs: [ProbCounter; 11],
    pub ac_val_probs:   [[[ProbCounter; 11]; 6]; 3],
}

#[derive(Default)]
pub struct VP6ModelsStat {
    pub zero_run_probs:     [[ProbCounter; 14]; 2],
}

pub struct VP56ModelsStat {
    pub mv_models:          [VP56MVModelStat; 2],
    pub mbtype_models:      [[[usize; 10]; 10]; 3],
    pub coeff_models:       [VP56CoeffModelStat; 2],
    pub vp6models:          VP6ModelsStat,
}

impl VP56ModelsStat {
    pub fn new() -> Self {
        Self {
            mv_models:      [VP56MVModelStat::default(); 2],
            mbtype_models:  [[[0; 10]; 10]; 3],
            coeff_models:   [VP56CoeffModelStat::default(); 2],
            vp6models:      VP6ModelsStat::default(),
        }
    }
    pub fn reset(&mut self) {
        self.mv_models      = [VP56MVModelStat::default(); 2];
        self.mbtype_models  = [[[0; 10]; 10]; 3];
        self.coeff_models   = [VP56CoeffModelStat::default(); 2];
        self.vp6models      = VP6ModelsStat::default();
    }
    pub fn generate(&self, dst: &mut VP56Models, is_intra: bool) {
        if !is_intra {
            for (dmv, smv) in dst.mv_models.iter_mut().zip(self.mv_models.iter()) {
                dmv.nz_prob = smv.nz_prob.to_prob_worthy(dmv.nz_prob);
                dmv.sign_prob = smv.sign_prob.to_prob_worthy(dmv.sign_prob);
                for (dp, sp) in dmv.raw_probs.iter_mut().zip(smv.raw_probs.iter()) {
                    *dp = sp.to_prob_worthy(*dp);
                }
                for (dp, sp) in dmv.tree_probs.iter_mut().zip(smv.tree_probs.iter()) {
                    *dp = sp.to_prob_worthy(*dp);
                }
            }
            for (xmit, mdl) in dst.prob_xmitted.iter_mut().zip(self.mbtype_models.iter()) {
                Self::generate_prob_xmitted(xmit, mdl);
            }
        }
        for (dmv, smv) in dst.coeff_models.iter_mut().zip(self.coeff_models.iter()) {
            for (dp, sp) in dmv.dc_value_probs.iter_mut().zip(smv.dc_value_probs.iter()) {
                *dp = sp.to_prob_worthy(*dp);
            }
            for (dp, sp) in dmv.ac_val_probs.iter_mut().zip(smv.ac_val_probs.iter()) {
                for (dp, sp) in dp.iter_mut().zip(sp.iter()) {
                    for (dp, sp) in dp.iter_mut().zip(sp.iter()) {
                        *dp = sp.to_prob_worthy(*dp);
                    }
                }
            }
        }
        for (dp, sp) in dst.vp6models.zero_run_probs.iter_mut().zip(self.vp6models.zero_run_probs.iter()) {
            for (dp, sp) in dp.iter_mut().zip(sp.iter()) {
                *dp = sp.to_prob_worthy(*dp);
            }
        }
    }
    /*
        VPMBType::InterNoMV     => 0,
        VPMBType::Intra         => 1,
        VPMBType::InterMV       => 2,
        VPMBType::InterNearest  => 3,
        VPMBType::InterNear     => 4,
        VPMBType::GoldenNoMV    => 5,
        VPMBType::GoldenMV      => 6,
        VPMBType::InterFourMV   => 7,
        VPMBType::GoldenNearest => 8,
        VPMBType::GoldenNear    => 9,
    */
    fn generate_prob_xmitted(probs: &mut [u8; 20], mbtype: &[[usize; 10]; 10]) {
        let mut sums = [0; 20];
        let mut total = 0;
        for (last, row) in mbtype.iter().enumerate() {
            for (cur, &count) in row.iter().enumerate() {
                if last == cur {
                    sums[cur * 2 + 1] = count;
                } else {
                    sums[cur * 2] += count;
                }
                total += count;
            }
        }
        if total != 0 {
            let mut sum = 0;
            for (dprob, &sprob) in probs.iter_mut().zip(sums.iter()) {
                if sprob != 0 {
                    *dprob = ((sprob * 256 + total - 1) / total).min(255) as u8;
                    sum += u16::from(*dprob);
                } else {
                    *dprob = 0;
                }
            }
            while sum > 256 {
                for prob in probs.iter_mut() {
                    if *prob > 1 {
                        *prob -= 1;
                        sum -= 1;
                        if sum == 256 {
                            break;
                        }
                    }
                }
            }
        } else {
            *probs = [0; 20];
        }
    }
}
