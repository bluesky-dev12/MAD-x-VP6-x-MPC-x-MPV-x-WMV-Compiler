use nihav_codec_support::codecs::{MV, ZERO_MV};

pub trait RV34MVOps {
    fn scale(&self, trd: u32, trb: u32) -> (MV, MV);
    fn diff_gt_3(self, other: Self) -> bool;
}

impl RV34MVOps for MV {
    fn scale(&self, trd: u32, trb: u32) -> (MV, MV) {
        const TR_SHIFT: u8 = 14;
        const TR_BIAS: i32 = 1 << (TR_SHIFT - 1);

        let ratio = ((trb as i32) << TR_SHIFT) / (trd as i32);
        let mv_f = MV {
                x: (((self.x as i32) * ratio + TR_BIAS) >> TR_SHIFT) as i16,
                y: (((self.y as i32) * ratio + TR_BIAS) >> TR_SHIFT) as i16
            };
        let mv_b = mv_f - *self;
        (mv_f, mv_b)
    }
    fn diff_gt_3(self, other: Self) -> bool {
        let diff = self - other;
        diff.x.abs() > 3 || diff.y.abs() > 3
    }
}

#[derive(Debug,Clone,Copy)]
pub enum PredType4x4 {
    Ver,
    Hor,
    DC,
    DiagDownLeft,
    DiagDownRight,
    VerRight,
    HorDown,
    VerLeft,
    HorUp,
    LeftDC,
    TopDC,
    DC128,
    DiagDownLeftNoDown,
    HorUpNoDown,
    VerLeftNoDown
}

#[derive(Debug,Clone,Copy)]
pub enum PredType8x8 {
    DC,
    Hor,
    Ver,
    Plane,
    LeftDC,
    TopDC,
    DC128
}

pub trait ToIndex {
    fn to_index(self) -> i8;
}

impl ToIndex for PredType8x8 {
    fn to_index(self) -> i8 {
        match self {
            PredType8x8::Ver => 1,
            PredType8x8::Hor => 2,
            PredType8x8::Plane => 3,
            _ => 0,
        }
    }
}

impl ToIndex for PredType4x4 {
    fn to_index(self) -> i8 {
        match self {
            PredType4x4::Ver => 1,
            PredType4x4::Hor => 2,
            PredType4x4::DiagDownRight => 3,
            PredType4x4::DiagDownLeft | PredType4x4::DiagDownLeftNoDown => 4,
            PredType4x4::VerRight => 5,
            PredType4x4::VerLeft | PredType4x4::VerLeftNoDown => 6,
            PredType4x4::HorUp |PredType4x4::HorUpNoDown => 7,
            PredType4x4::HorDown => 8,
            _ => 0, // DC predictions
        }
    }
}

#[derive(Clone,Copy,Default)]
pub struct Block {
    pub coeffs: [i16; 16],
}

impl Block {
    pub fn new() -> Self { Self::default() }
    pub fn is_empty(&self) -> bool {
        for &el in self.coeffs.iter() {
            if el != 0 {
                return false;
            }
        }
        true
    }
    pub fn count_nz(&self) -> usize {
        self.coeffs.iter().filter(|&&x| x != 0).count()
    }
}
impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut out = String::new();
        for row in self.coeffs.chunks(4) {
            out += format!(" {:3} {:3} {:3} {:3}\n", row[0], row[1], row[2], row[3]).as_str();
        }
        write!(f, "{}", out)
    }
}

#[derive(Clone,Copy,Default)]
pub struct DeblockInfo {
    pub is_strong:  bool,
    pub q:          u8,
    pub cbp_y:      u16,
    pub cbp_c:      u8,
    pub deblock_y:  u16,
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum MBType {
    Intra,
    Intra16,
    Skip,
    P16x16,
    P16x16Mix,
    P16x8,
    P8x16,
    P8x8,
    Direct,
    Bidir,
    Forward,
    Backward,
    Invalid,
}

impl MBType {
    pub fn is_intra(self) -> bool { matches!(self, MBType::Intra | MBType::Intra16) }
    fn get_weight(self) -> u8 {
        match self {
            MBType::Intra     => 0,
            MBType::Intra16   => 1,
            MBType::Skip      => unreachable!(),
            MBType::P16x16    => 2,
            MBType::P16x16Mix => 10,
            MBType::P16x8     => 7,
            MBType::P8x16     => 8,
            MBType::P8x8      => 3,
            MBType::Direct    => 6,
            MBType::Bidir     => 9,
            MBType::Forward   => 4,
            MBType::Backward  => 5,
            MBType::Invalid     => unreachable!(),
        }
    }
    pub fn to_code(self) -> usize {
        match self {
            MBType::Intra                     => 0,
            MBType::Intra16                   => 1,
            MBType::P16x16 | MBType::Forward  => 2,
            MBType::P8x8   | MBType::Backward => 3,
            MBType::P16x8  | MBType::Bidir    => 4,
            MBType::P8x16  | MBType::Direct   => 5,
            MBType::P16x16Mix                 => 6,
            _ => unreachable!(),
        }
    }
    pub fn has_dir_mv(self, fwd: bool) -> bool {
        match self {
            MBType::Bidir => true,
            MBType::Forward if fwd => true,
            MBType::Backward if !fwd => true,
            _ => false,
        }
    }
}

#[derive(Default)]
pub struct SliceState {
    pub has_t:      bool,
    pub has_l:      bool,
    pub has_tl:     bool,
    pub has_tr:     bool,
    pub mb_x:       usize,
    pub mb_y:       usize,
}

impl SliceState {
    pub fn new() -> Self { Self::default() }
}

#[derive(Default)]
pub struct MBState {
    pub mb_type:        Vec<MBType>,
    pub ipred:          Vec<i8>,
    pub fwd_mv:         Vec<MV>,
    pub bwd_mv:         Vec<MV>,
    pub ref_mv:         Vec<MV>,
    pub mb_stride:      usize,
    pub blk8_stride:    usize,
    pub blk4_stride:    usize,
}

impl MBState {
    pub fn new() -> Self { Self::default() }
    pub fn resize(&mut self, mb_w: usize, mb_h: usize) {
        self.mb_stride   = mb_w + 2;
        self.blk8_stride = mb_w * 2 + 2;
        self.blk4_stride = mb_w * 4 + 2;

        self.mb_type.resize(self.mb_stride * (mb_h + 1), MBType::Invalid);
        self.ipred.resize(self.blk4_stride * (mb_h * 4 + 1), -1);
        self.fwd_mv.resize(self.blk8_stride * (mb_w * 2 + 1), ZERO_MV);
        self.bwd_mv.resize(self.blk8_stride * (mb_w * 2 + 1), ZERO_MV);
        self.ref_mv.resize(self.blk8_stride * (mb_w * 2 + 1), ZERO_MV);
    }
    pub fn reset(&mut self) {
        for el in self.mb_type.iter_mut() {
            *el = MBType::Invalid;
        }
        for el in self.ipred.iter_mut() {
            *el = -1;
        }
    }
    fn set_mv(&mut self, blk8_idx: usize, fwd: bool, mv: MV) {
        if fwd {
            self.fwd_mv[blk8_idx] = mv;
            self.fwd_mv[blk8_idx + 1] = mv;
            self.fwd_mv[blk8_idx + self.blk8_stride] = mv;
            self.fwd_mv[blk8_idx + self.blk8_stride + 1] = mv;
        } else {
            self.bwd_mv[blk8_idx] = mv;
            self.bwd_mv[blk8_idx + 1] = mv;
            self.bwd_mv[blk8_idx + self.blk8_stride] = mv;
            self.bwd_mv[blk8_idx + self.blk8_stride + 1] = mv;
        }
    }
    pub fn get_mb_idx(&self, mb_x: usize, mb_y: usize) -> usize {
        mb_x + 1 + (mb_y + 1) * self.mb_stride
    }
    pub fn get_blk8_idx(&self, mb_x: usize, mb_y: usize) -> usize {
        mb_x * 2 + 1 + (mb_y * 2 + 1) * self.blk8_stride
    }
    pub fn get_blk4_idx(&self, mb_x: usize, mb_y: usize) -> usize {
        mb_x * 4 + 1 + (mb_y * 4 + 1) * self.blk4_stride
    }
    pub fn update(&mut self, mb_type: &MacroblockType, mb_x: usize, mb_y: usize) {
        let mb_idx = self.get_mb_idx(mb_x, mb_y);
        let blk8_idx = self.get_blk8_idx(mb_x, mb_y);
        let blk4_idx = self.get_blk4_idx(mb_x, mb_y);

        for row in self.ipred[blk4_idx..].chunks_mut(self.blk4_stride).take(4) {
            for el in row[..4].iter_mut() {
                *el = 0;
            }
        }

        match *mb_type {
            MacroblockType::Intra16x16(ptype) => {
                self.mb_type[mb_idx] = MBType::Intra16;
                let pred_id = ptype.to_index();
                for row in self.ipred[blk4_idx..].chunks_mut(self.blk4_stride).take(4) {
                    for el in row[..4].iter_mut() {
                        *el = pred_id;
                    }
                }
                self.set_mv(blk8_idx, true,  ZERO_MV);
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::Intra4x4(ptypes) => {
                self.mb_type[mb_idx] = MBType::Intra;
                for (dst, src) in self.ipred[blk4_idx..].chunks_mut(self.blk4_stride).zip(ptypes.chunks(4)) {
                    for (dst, &ptype) in dst.iter_mut().zip(src.iter()) {
                        *dst = ptype.to_index();
                    }
                }
                self.set_mv(blk8_idx, true,  ZERO_MV);
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::PSkip => {
                self.mb_type[mb_idx] = MBType::Skip;
                self.set_mv(blk8_idx, true,  ZERO_MV);
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::Inter16x16(mv) => {
                self.mb_type[mb_idx] = MBType::P16x16;
                self.set_mv(blk8_idx, true,  mv);
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::InterMix(mv) => {
                self.mb_type[mb_idx] = MBType::P16x16Mix;
                self.set_mv(blk8_idx, true,  mv);
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::Inter16x8(mvs) => {
                self.mb_type[mb_idx] = MBType::P16x8;
                self.fwd_mv[blk8_idx] = mvs[0];
                self.fwd_mv[blk8_idx + 1] = mvs[0];
                self.fwd_mv[blk8_idx + self.blk8_stride] = mvs[1];
                self.fwd_mv[blk8_idx + self.blk8_stride + 1] = mvs[1];
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::Inter8x16(mvs) => {
                self.mb_type[mb_idx] = MBType::P8x16;
                self.fwd_mv[blk8_idx] = mvs[0];
                self.fwd_mv[blk8_idx + 1] = mvs[1];
                self.fwd_mv[blk8_idx + self.blk8_stride] = mvs[0];
                self.fwd_mv[blk8_idx + self.blk8_stride + 1] = mvs[1];
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::Inter8x8(mvs) => {
                self.mb_type[mb_idx] = MBType::P8x8;
                self.fwd_mv[blk8_idx] = mvs[0];
                self.fwd_mv[blk8_idx + 1] = mvs[1];
                self.fwd_mv[blk8_idx + self.blk8_stride] = mvs[2];
                self.fwd_mv[blk8_idx + self.blk8_stride + 1] = mvs[3];
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::BSkip(fmvs, bmvs) => {
                self.mb_type[mb_idx] = MBType::Skip;
                self.fwd_mv[blk8_idx] = fmvs[0];
                self.fwd_mv[blk8_idx + 1] = fmvs[1];
                self.fwd_mv[blk8_idx + self.blk8_stride] = fmvs[0];
                self.fwd_mv[blk8_idx + self.blk8_stride + 1] = fmvs[1];
                self.bwd_mv[blk8_idx] = bmvs[0];
                self.bwd_mv[blk8_idx + 1] = bmvs[1];
                self.bwd_mv[blk8_idx + self.blk8_stride] = bmvs[0];
                self.bwd_mv[blk8_idx + self.blk8_stride + 1] = bmvs[1];
            },
            /*MacroblockType::Direct(fmv, bmv) => {
                self.mb_type[mb_idx] = MBType::Direct;
                self.set_mv(blk8_idx, true,  fmv);
                self.set_mv(blk8_idx, false, bmv);
            },*/
            MacroblockType::Bidir(fmv, bmv) => {
                self.mb_type[mb_idx] = MBType::Bidir;
                self.set_mv(blk8_idx, true,  fmv);
                self.set_mv(blk8_idx, false, bmv);
            },
            MacroblockType::Forward(mv) => {
                self.mb_type[mb_idx] = MBType::Forward;
                self.set_mv(blk8_idx, true,  mv);
                self.set_mv(blk8_idx, false, ZERO_MV);
            },
            MacroblockType::Backward(mv) => {
                self.mb_type[mb_idx] = MBType::Backward;
                self.set_mv(blk8_idx, true,  ZERO_MV);
                self.set_mv(blk8_idx, false, mv);
            },
        };
    }
    pub fn get_pred_mbtype(&self, sstate: &SliceState, is_b: bool) -> MBType {
        let mut cand = [MBType::Invalid; 4];
        let mut ccount = 0;

        let mb_idx = self.get_mb_idx(sstate.mb_x, sstate.mb_y);
        if sstate.has_t {
            cand[ccount] = self.mb_type[mb_idx - self.mb_stride];
            ccount += 1;
            if sstate.has_tr {
                cand[ccount] = self.mb_type[mb_idx - self.mb_stride + 1];
                ccount += 1;
            }
        }
        if sstate.has_l {
            cand[ccount] = self.mb_type[mb_idx - 1];
            ccount += 1;
        }
        if sstate.has_tl {
            cand[ccount] = self.mb_type[mb_idx - self.mb_stride - 1];
            ccount += 1;
        }
        if !is_b {
            for el in cand[..ccount].iter_mut() {
                if *el == MBType::Skip {
                    *el = MBType::P16x16;
                }
            }
        } else {
            for el in cand[..ccount].iter_mut() {
                if *el == MBType::Skip {
                    *el = MBType::Direct;
                }
            }
        }
        match ccount {
            0 => MBType::Intra,
            1 => cand[0],
            2 => if cand[0].get_weight() <= cand[1].get_weight() { cand[0] } else { cand[1] },
            _ => {
                const MBTYPE_FROM_WEIGHT: [MBType; 11] = [
                    MBType::Intra,   MBType::Intra16,  MBType::P16x16,   MBType::P8x8,
                    MBType::Forward, MBType::Backward, MBType::Direct,   MBType::P16x8,
                    MBType::P8x16,   MBType::Bidir,    MBType::P16x16Mix
                ];

                let mut counts = [0; 12];
                for el in cand[..ccount].iter() {
                    counts[usize::from(el.get_weight())] += 1;
                }
                let mut best_idx = 0;
                let mut best_wgt = 0;
                for (idx, &weight) in counts.iter().enumerate() {
                    if weight > best_wgt {
                        best_idx = idx;
                        best_wgt = weight;
                    }
                }
                MBTYPE_FROM_WEIGHT[best_idx]
            },
        }
    }
    pub fn get_ipred4x4_ctx(&self, mb_x: usize, mb_y: usize, x: usize, y: usize) -> (i8, i8, i8) {
        let blk4_idx = self.get_blk4_idx(mb_x, mb_y) + x + y * self.blk4_stride;
        (self.ipred[blk4_idx - 1],
         self.ipred[blk4_idx - self.blk4_stride],
         self.ipred[blk4_idx - self.blk4_stride + 1])
    }
    pub fn set_ipred4x4(&mut self, mb_x: usize, mb_y: usize, modes: &[PredType4x4; 16]) {
        let blk4_idx = self.get_blk4_idx(mb_x, mb_y);
        for (dst, src) in self.ipred[blk4_idx..].chunks_mut(self.blk4_stride).zip(modes.chunks(4)) {
            for (dst, src) in dst.iter_mut().zip(src.iter()) {
                *dst = src.to_index();
            }
        }
    }
    fn get_mv(&self, idx: usize, fwd: bool) -> MV {
        if fwd {
            self.fwd_mv[idx]
        } else {
            self.bwd_mv[idx]
        }
    }
    pub fn get_diff_mv(&self, sstate: &SliceState, w16: bool, xoff: usize, yoff: usize) -> MV {
        let blk8_idx = self.get_blk8_idx(sstate.mb_x, sstate.mb_y) + xoff + yoff * self.blk8_stride;

        let cur_mv = self.get_mv(blk8_idx, true);

        if (yoff == 0 && !sstate.has_t) && (xoff == 0 && !sstate.has_l) {
            return cur_mv;
        }

        let left_mv = if sstate.has_l || (xoff != 0) { self.get_mv(blk8_idx - 1, true) } else { ZERO_MV };
        let top_mv = if sstate.has_t || (yoff != 0) { self.get_mv(blk8_idx - self.blk8_stride, true) } else { left_mv };
        let has_tr = match xoff + yoff * 2 {
                0 if w16 => sstate.has_tr,
                0 => sstate.has_t,
                1 => sstate.has_tr,
                2 if w16 => false,
                2 => true,
                _ => false,
            };
        let has_tl = match xoff + yoff * 2 {
                0 => sstate.has_tl,
                1 => sstate.has_t,
                2 => sstate.has_l,
                _ => true,
            };
        let mv_c = if has_tr {
                self.get_mv(blk8_idx - self.blk8_stride + if w16 { 2 } else { 1 }, true)
            } else if has_tl {
                self.get_mv(blk8_idx - self.blk8_stride - 1, true)
            } else {
                return cur_mv - left_mv;
            };

        cur_mv - MV::pred(left_mv, top_mv, mv_c)
    }
    pub fn get_diff_mv_b(&self, sstate: &SliceState, fwd: bool) -> MV {
        let mb_idx = self.get_mb_idx(sstate.mb_x, sstate.mb_y);
        let blk8_idx = self.get_blk8_idx(sstate.mb_x, sstate.mb_y);

        let mut pred_mv = [ZERO_MV; 3];
        let mut pcount = 0;

        let cur_mv = self.get_mv(blk8_idx, fwd);

        if sstate.has_l && self.mb_type[mb_idx - 1].has_dir_mv(fwd) {
            pred_mv[pcount] = self.get_mv(blk8_idx - 1, fwd);
            pcount += 1;
        }
        if !sstate.has_t {
            return cur_mv - pred_mv[0];
        }
        if self.mb_type[mb_idx - self.mb_stride].has_dir_mv(fwd) {
            pred_mv[pcount] = self.get_mv(blk8_idx - self.blk8_stride, fwd);
            pcount += 1;
        }
        if sstate.has_tr {
            if self.mb_type[mb_idx - self.mb_stride + 1].has_dir_mv(fwd) {
                pred_mv[pcount] = self.get_mv(blk8_idx - self.blk8_stride + 2, fwd);
                pcount += 1;
            }
        } else if sstate.has_tl && self.mb_type[mb_idx - self.mb_stride - 1].has_dir_mv(fwd) {
            pred_mv[pcount] = self.get_mv(blk8_idx - self.blk8_stride - 1, fwd);
            pcount += 1;
        }
        let pred_mv = match pcount {
                3 => MV::pred(pred_mv[0], pred_mv[1], pred_mv[2]),
                2 => MV{ x: (pred_mv[0].x + pred_mv[1].x) / 2, y: (pred_mv[0].y + pred_mv[1].y) / 2 },
                1 => pred_mv[0],
                _ => ZERO_MV,
            };
        cur_mv - pred_mv
    }
    pub fn swap_mvs(&mut self) {
        std::mem::swap(&mut self.fwd_mv, &mut self.ref_mv);
    }
    pub fn fill_deblock(&self, dblk: &mut DeblockInfo, sstate: &SliceState) {
        if dblk.is_strong {
            dblk.deblock_y = 0xFFFF;
            return;
        }
        let mut hmvmask = 0;
        let mut vmvmask = 0;

        let mut blk8_idx = self.get_blk8_idx(sstate.mb_x, sstate.mb_y);
        for y in 0..2 {
            for x in 0..2 {
                let shift = x * 2 + y * 8;
                let cur_mv = self.get_mv(blk8_idx + x, true);
                if (x > 0) || (sstate.mb_x > 0) {
                    let left_mv = self.get_mv(blk8_idx + x - 1, true);
                    if cur_mv.diff_gt_3(left_mv) {
                        vmvmask |= 0x11 << shift;
                    }
                }
                if (y > 0) || (sstate.mb_y > 0) {
                    let top_mv = self.get_mv(blk8_idx + x - self.blk8_stride, true);
                    if cur_mv.diff_gt_3(top_mv) {
                        hmvmask |= 0x03 << shift;
                    }
                }
            }
            blk8_idx += self.blk8_stride;
        }
        if sstate.mb_y == 0 { hmvmask &= !0x000F; }
        if sstate.mb_x == 0 { vmvmask &= !0x1111; }

        dblk.deblock_y = dblk.cbp_y | hmvmask | vmvmask;
    }
}

#[derive(Clone)]
pub enum MacroblockType {
    Intra16x16(PredType8x8),
    Intra4x4([PredType4x4; 16]),
    PSkip,
    Inter16x16(MV),
    InterMix(MV),
    Inter16x8([MV; 2]),
    Inter8x16([MV; 2]),
    Inter8x8([MV; 4]),
    BSkip([MV; 4], [MV; 4]),
    //Direct(MV, MV),
    Bidir(MV, MV),
    Forward(MV),
    Backward(MV),
}

impl Default for MacroblockType {
    fn default() -> Self { Self::Intra16x16(PredType8x8::DC) }
}

impl MacroblockType {
    pub fn is_intra(&self) -> bool {
        matches!(*self, MacroblockType::Intra16x16(_) | MacroblockType::Intra4x4(_))
    }
    pub fn is_16(&self) -> bool {
        matches!(*self, MacroblockType::Intra16x16(_) | MacroblockType::InterMix(_))
    }
    pub fn is_skip(&self) -> bool {
        matches!(*self, MacroblockType::PSkip | MacroblockType::BSkip(_, _))
    }
}

pub struct Macroblock {
    pub mb_type:    MacroblockType,
    pub coeffs:     [Block; 25],
}
