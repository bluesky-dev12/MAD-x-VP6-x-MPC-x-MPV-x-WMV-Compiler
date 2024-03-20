use nihav_core::frame::{NAVideoBuffer, NASimpleVideoFrame};
use nihav_codec_support::codecs::{MV, ZERO_MV};
use nihav_codec_support::data::GenericCache;
use super::SimplifiedSliceRefs;
use super::pic_ref::FrameMBInfo;

#[derive(Clone,Copy)]
pub struct SimpleFrame<'a> {
    pub data:   &'a [u8],
    pub offset: [usize; 3],
    pub stride: [usize; 3],
}

impl<'a> SimpleFrame<'a> {
    pub fn new(buf: &'a NAVideoBuffer<u8>) -> Self {
        let mut offset = [0; 3];
        let mut stride = [0; 3];
        for (plane, (offs, strd)) in offset.iter_mut().zip(stride.iter_mut()).enumerate() {
            *offs = buf.get_offset(plane);
            *strd = buf.get_stride(plane);
        }
        Self {
            data:   buf.get_data(),
            offset, stride
        }
    }
}

#[repr(u8)]
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum BMode {
    L0,
    L1,
    Bi,
}

#[derive(Clone,Copy,Debug,PartialEq,Default)]
pub enum MBType {
    #[default]
    Intra4x4,
    Intra8x8,
    Intra16x16(u8, u8, u8),
    PCM,

    P16x16,
    P16x8,
    P8x16,
    P8x8,
    P8x8Ref0,
    PSkip,

    Direct,
    B16x16(BMode),
    B16x8(BMode, BMode),
    B8x16(BMode, BMode),
    B8x8,
    BSkip,
}

impl MBType {
    pub fn is_intra(self) -> bool {
        matches!(self, MBType::Intra4x4 | MBType::Intra8x8 | MBType::Intra16x16(_, _, _) | MBType::PCM)
    }
    pub fn is_intra16x16(self) -> bool {
        matches!(self, MBType::Intra16x16(_, _, _))
    }
    pub fn is_skip(self) -> bool {
        matches!(self, MBType::PSkip | MBType::BSkip)
    }
    pub fn is_4x4(self) -> bool { self.num_parts() == 4 }
    pub fn is_l0(self, part: usize) -> bool {
        match self {
            MBType::B16x16(mode) => mode == BMode::L0,
            MBType::B16x8(mode0, mode1) | MBType::B8x16(mode0, mode1) => {
                if part == 0 {
                    mode0 == BMode::L0
                } else {
                    mode1 == BMode::L0
                }
            },
            MBType::Direct | MBType::BSkip => false,
            _ => true,
        }
    }
    pub fn is_l1(self, part: usize) -> bool {
        match self {
            MBType::B16x16(mode) => mode == BMode::L1,
            MBType::B16x8(mode0, mode1) | MBType::B8x16(mode0, mode1) => {
                if part == 0 {
                    mode0 == BMode::L1
                } else {
                    mode1 == BMode::L1
                }
            },
            _ => false,
        }
    }
    pub fn num_parts(self) -> usize {
        match self {
            MBType::Intra4x4 | MBType::Intra8x8 | MBType::Intra16x16(_, _, _) | MBType::PCM |
            MBType::PSkip |
            MBType::Direct | MBType::BSkip
                => 1,
            MBType::P16x16 |
            MBType::B16x16(_)
                => 1,
            MBType::P16x8 | MBType::P8x16 |
            MBType::B16x8(_, _) | MBType::B8x16(_, _)
                => 2,
            _ => 4,
        }
    }
    pub fn size(self) -> (usize, usize) {
        match self {
            MBType::Intra4x4 |
            MBType::Intra8x8 |
            MBType::Intra16x16(_, _, _) |
            MBType::PCM |
            MBType::P16x16 |
            MBType::PSkip |
            MBType::Direct |
            MBType::B16x16(_) |
            MBType::BSkip
                => (16, 16),
            MBType::P16x8 | MBType::B16x8(_, _) => (16, 8),
            MBType::P8x16 | MBType::B8x16(_, _) => (8, 16),
            _ => (8, 8),
        }
    }
}

#[derive(Clone,Copy,Debug,PartialEq,Default)]
pub enum SubMBType {
    P8x8,
    P8x4,
    P4x8,
    P4x4,
    #[default]
    Direct8x8,
    B8x8(BMode),
    B8x4(BMode),
    B4x8(BMode),
    B4x4(BMode),
}

impl SubMBType {
    pub fn num_parts(self) -> usize {
        match self {
            SubMBType::P8x8 | SubMBType::Direct8x8 | SubMBType::B8x8(_) => 1,
            SubMBType::P4x4 | SubMBType::B4x4(_) => 4,
            _ => 2,
        }
    }
    pub fn size(self) -> (usize, usize) {
        match self {
            SubMBType::P8x8 | SubMBType::Direct8x8 | SubMBType::B8x8(_) => (8, 8),
            SubMBType::P8x4 | SubMBType::B8x4(_) => (8, 4),
            SubMBType::P4x8 | SubMBType::B4x8(_) => (4, 8),
            SubMBType::P4x4 | SubMBType::B4x4(_) => (4, 4),
        }
    }
    pub fn is_l0(self) -> bool {
        match self {
            SubMBType::B8x8(mode) | SubMBType::B8x4(mode) |
            SubMBType::B4x8(mode) | SubMBType::B4x4(mode) => {
                mode == BMode::L0
            },
            _ => true,
        }
    }
    pub fn is_l1(self) -> bool {
        match self {
            SubMBType::B8x8(mode) | SubMBType::B8x4(mode) |
            SubMBType::B4x8(mode) | SubMBType::B4x4(mode) => {
                mode == BMode::L1
            },
            _ => false,
        }
    }
}

#[repr(u8)]
#[derive(Clone,Copy,Debug,PartialEq,Default)]
pub enum CompactMBType {
    Intra4x4,
    Intra8x8,
    Intra16x16,
    PCM,

    P16x16,
    P16x8,
    P8x16,
    P8x8,
    P8x8Ref0,
    PSkip,

    Direct,
    B16x16,
    B16x8,
    B8x16,
    B8x8,
    BSkip,

    #[default]
    None,
}

impl CompactMBType {
    pub fn is_intra(self) -> bool {
        matches!(self, CompactMBType::Intra4x4 | CompactMBType::Intra8x8 | CompactMBType::Intra16x16)
    }
    pub fn is_intra16orpcm(self) -> bool {
        matches!(self, CompactMBType::Intra16x16 | CompactMBType::PCM)
    }
    pub fn is_skip(self) -> bool {
        matches!(self, CompactMBType::PSkip | CompactMBType::BSkip)
    }
    pub fn is_direct(self) -> bool {
        matches!(self, CompactMBType::BSkip | CompactMBType::Direct | CompactMBType::None)
    }
    pub fn is_inter(self) -> bool {
        !self.is_intra() && !self.is_skip() && self != CompactMBType::PCM
    }
    pub fn is_16x16_ref(self) -> bool {
        matches!(self,
            CompactMBType::Intra4x4 |
            CompactMBType::Intra8x8 |
            CompactMBType::Intra16x16 |
            CompactMBType::PCM |
            CompactMBType::P16x16 |
            CompactMBType::B16x16)
    }
}

impl From<MBType> for CompactMBType {
    fn from(mbtype: MBType) -> Self {
        match mbtype {
            MBType::Intra4x4 => CompactMBType::Intra4x4,
            MBType::Intra8x8 => CompactMBType::Intra8x8,
            MBType::Intra16x16(_, _, _) => CompactMBType::Intra16x16,
            MBType::PCM => CompactMBType::PCM,
            MBType::P16x16 => CompactMBType::P16x16,
            MBType::P16x8 => CompactMBType::P16x8,
            MBType::P8x16 => CompactMBType::P8x16,
            MBType::P8x8 => CompactMBType::P8x8,
            MBType::P8x8Ref0 => CompactMBType::P8x8Ref0,
            MBType::PSkip => CompactMBType::PSkip,
            MBType::Direct => CompactMBType::Direct,
            MBType::B16x16(_) => CompactMBType::B16x16,
            MBType::B16x8(_, _) => CompactMBType::B16x8,
            MBType::B8x16(_, _) => CompactMBType::B8x16,
            MBType::B8x8 => CompactMBType::B8x8,
            MBType::BSkip => CompactMBType::BSkip,
        }
    }
}

#[repr(u8)]
#[derive(Clone,Copy,Debug,PartialEq,Default)]
pub enum IntraPredMode {
    Vertical,
    Horizontal,
    DC,
    DiagDownLeft,
    DiagDownRight,
    VerRight,
    HorDown,
    VerLeft,
    HorUp,
    #[default]
    None,
}

impl IntraPredMode {
    pub fn is_none(self) -> bool { self == IntraPredMode::None }
    pub fn into_pred_idx(self) -> i8 {
        if !self.is_none() {
            self as u8 as i8
        } else {
            -1
        }
    }
}

impl From<u8> for IntraPredMode {
    fn from(val: u8) -> Self {
        match val {
            0 => IntraPredMode::Vertical,
            1 => IntraPredMode::Horizontal,
            2 => IntraPredMode::DC,
            3 => IntraPredMode::DiagDownLeft,
            4 => IntraPredMode::DiagDownRight,
            5 => IntraPredMode::VerRight,
            6 => IntraPredMode::HorDown,
            7 => IntraPredMode::VerLeft,
            8 => IntraPredMode::HorUp,
            _ => IntraPredMode::None,
        }
    }
}

impl From<IntraPredMode> for u8 {
    fn from(val: IntraPredMode) -> Self {
        match val {
            IntraPredMode::Vertical      => 0,
            IntraPredMode::Horizontal    => 1,
            IntraPredMode::DC            => 2,
            IntraPredMode::DiagDownLeft  => 3,
            IntraPredMode::DiagDownRight => 4,
            IntraPredMode::VerRight      => 5,
            IntraPredMode::HorDown       => 6,
            IntraPredMode::VerLeft       => 7,
            IntraPredMode::HorUp         => 8,
            _ => 9,
        }
    }
}

pub const MISSING_POC: u16 = 0xFFFF;

#[derive(Clone,Copy,Debug)]
pub struct PicRef {
    ref_idx: u8
}

pub const MISSING_REF: PicRef = PicRef { ref_idx: 0xFF };
pub const INVALID_REF: PicRef = PicRef { ref_idx: 0xFE };
pub const ZERO_REF: PicRef = PicRef { ref_idx: 0 };
const DIRECT_FLAG: u8 = 0x40;

impl PicRef {
    pub fn new(ref_idx: u8) -> Self {
        Self { ref_idx }
    }
    pub fn not_avail(self) -> bool {
        self == MISSING_REF || self == INVALID_REF
    }
    pub fn index(self) -> usize { (self.ref_idx & !DIRECT_FLAG) as usize }
    pub fn is_direct(self) -> bool { (self.ref_idx & DIRECT_FLAG) != 0 }
    pub fn set_direct(&mut self) { self.ref_idx |= DIRECT_FLAG; }
    fn min_pos(self, other: Self) -> Self {
        match (self.not_avail(), other.not_avail()) {
            (true,  true)   => self,
            (false, true)   => self,
            (true,  false)  => other,
            (false, false)  => PicRef::new((self.ref_idx & !DIRECT_FLAG).min(other.ref_idx & !DIRECT_FLAG)),
        }
    }
}

impl Default for PicRef {
    fn default() -> Self { MISSING_REF }
}

impl PartialEq for PicRef {
    fn eq(&self, other: &Self) -> bool {
        (self.ref_idx | DIRECT_FLAG) == (other.ref_idx | DIRECT_FLAG)
    }
}

impl std::fmt::Display for PicRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == MISSING_REF {
            write!(f, "-1")
        } else if *self == INVALID_REF {
            write!(f, "-2")
        } else {
            write!(f, "{}", self.ref_idx & !DIRECT_FLAG)
        }
    }
}

#[derive(Clone,Copy,Default)]
pub struct MBData {
    pub mb_type:        CompactMBType,
    pub cbp:            u8,
    pub coded_flags:    u32,
    pub cmode:          u8,
    pub qp_y:           u8,
    pub qp_u:           u8,
    pub qp_v:           u8,
    pub transform_8x8:  bool,
}

pub fn blk4_to_blk8(blk4: usize) -> usize {
    /*const MAP: [usize; 16] = [ 0, 0, 1, 1, 0, 0, 1, 1, 2, 2, 3, 3, 2, 2, 3, 3 ];
    MAP[blk4 & 0xF]*/
    ((blk4 & 2) >> 1) | ((blk4 & 8) >> 2)
}

#[derive(Clone,Copy)]
pub struct Blk8Data {
    pub ref_idx:    [PicRef; 2],
    pub ncoded_c:   [u8; 2],
}

impl Default for Blk8Data {
    fn default() -> Self {
        Self {
            ref_idx:        [MISSING_REF; 2],
            ncoded_c:       [0; 2],
        }
    }
}

#[derive(Clone,Copy,Default)]
pub struct Blk4Data {
    pub ncoded:     u8,
    pub ipred:      IntraPredMode,
    pub mv:         [MV; 2],
    pub mvd:        [MV; 2],
}

pub struct SliceState {
    pub mb_x:           usize,
    pub mb_y:           usize,
    pub mb_w:           usize,
    pub mb_h:           usize,
    pub mb_start:       usize,

    pub mb:             GenericCache<MBData>,
    pub blk8:           GenericCache<Blk8Data>,
    pub blk4:           GenericCache<Blk4Data>,

    pub deblock:        [u8; 16],

    pub has_top:        bool,
    pub has_left:       bool,

    pub top_line_y:     Vec<u8>,
    pub left_y:         [u8; 17], // first element is top-left
    pub top_line_c:     [Vec<u8>; 2],
    pub left_c:         [[u8; 9]; 2],
}

const BLK4_TO_D8: [usize; 16] = [ 0, 0, 3, 3, 0, 0, 3, 3, 12, 12, 15, 15, 12, 12, 15, 15 ];

impl SliceState {
    pub fn new() -> Self {
        Self {
            mb_x:       0,
            mb_y:       0,
            mb_w:       0,
            mb_h:       0,
            mb_start:   0,
            mb:         GenericCache::new(0, 0, MBData::default()),
            blk8:       GenericCache::new(0, 0, Blk8Data::default()),
            blk4:       GenericCache::new(0, 0, Blk4Data::default()),

            deblock:    [0; 16],

            has_top:    false,
            has_left:   false,

            top_line_y: Vec::new(),
            left_y:     [0; 17],
            top_line_c: [Vec::new(), Vec::new()],
            left_c:     [[0; 9]; 2],
        }
    }
    pub fn reset(&mut self, mb_w: usize, mb_h: usize, mb_pos: usize) {
        self.mb_w     = mb_w;
        self.mb_h     = mb_h;
        self.mb_start = mb_pos;
        if mb_w > 0 {
            self.mb_x = mb_pos % mb_w;
            self.mb_y = mb_pos / mb_w;
        } else {
            self.mb_x = 0;
            self.mb_y = 0;
        }
        self.mb    = GenericCache::new(1, mb_w     + 2, MBData::default());
        self.blk8  = GenericCache::new(2, mb_w * 2 + 2, Blk8Data::default());
        self.blk4  = GenericCache::new(4, mb_w * 4 + 2, Blk4Data::default());

        self.has_top  = false;
        self.has_left = false;

        self.top_line_y.resize(mb_w * 16 + 1, 0x80);
        self.top_line_c[0].resize(mb_w *  8 + 1, 0x80);
        self.top_line_c[1].resize(mb_w *  8 + 1, 0x80);
        self.left_y = [0x80; 17];
        self.left_c = [[0x80; 9]; 2];
    }
    pub fn save_ipred_context(&mut self, frm: &NASimpleVideoFrame<u8>) {
        let dstoff = self.mb_x * 16;
        let srcoff = frm.offset[0] + self.mb_x * 16 + self.mb_y * 16 * frm.stride[0];
        self.left_y[0] = self.top_line_y[dstoff + 15];
        self.top_line_y[dstoff..][..16].copy_from_slice(&frm.data[srcoff + frm.stride[0] * 15..][..16]);
        for (dst, src) in self.left_y[1..].iter_mut().zip(frm.data[srcoff..].chunks(frm.stride[0])) {
            *dst = src[15];
        }
        for chroma in 0..2 {
            let cstride = frm.stride[chroma + 1];
            let dstoff = self.mb_x * 8;
            let srcoff = frm.offset[chroma + 1] + self.mb_x * 8 + self.mb_y * 8 * cstride;
            self.left_c[chroma][0] = self.top_line_c[chroma][dstoff + 7];
            self.top_line_c[chroma][dstoff..][..8].copy_from_slice(&frm.data[srcoff + cstride * 7..][..8]);
            for (dst, src) in self.left_c[chroma][1..].iter_mut().zip(frm.data[srcoff..].chunks(cstride)) {
                *dst = src[7];
            }
        }
    }
    pub fn fill_deblock(&mut self, frefs: &SimplifiedSliceRefs, deblock_mode: u8, is_s: bool) {
        if deblock_mode == 1 {
            return;
        }

        self.deblock = [0; 16];

        let tx8x8 = self.get_cur_mb().transform_8x8;

        let cur_intra     = self.get_cur_mb().mb_type.is_intra();
        let left_intra    = self.get_left_mb().mb_type.is_intra();
        let mut top_intra = self.get_top_mb().mb_type.is_intra();

        let mut coded_cache  = [false; 25];
        let mut mv_cache     = [[ZERO_MV; 2]; 25];
        let mut ref_cache    = [[INVALID_REF; 2]; 25];

        if self.mb_y != 0 || self.has_top {
            for (x, (cc, mv)) in coded_cache[1..5].iter_mut().zip(mv_cache[1..5].iter_mut()).enumerate() {
                let blk4 = self.get_top_blk4(x);
                *cc = blk4.ncoded != 0;
                *mv = blk4.mv;
                if (x & 1) == 0 {
                    let blk8 = self.get_top_blk8(x / 2);
                    ref_cache[x + 1] = blk8.ref_idx;
                } else {
                    ref_cache[x + 1] = ref_cache[x];
                }
            }
        }
        for (y, (ccs, mvs)) in coded_cache[5..].chunks_exact_mut(5).zip(
                mv_cache[5..].chunks_exact_mut(5)).enumerate() {
            if self.has_left || self.mb_x != 0 {
                let blk4 = self.get_left_blk4(y * 4);
                ccs[0] = blk4.ncoded != 0;
                mvs[0] = blk4.mv;
                if (y & 1) == 0 {
                    let blk8 = self.get_left_blk8(y);
                    ref_cache[y * 5 + 5] = blk8.ref_idx;
                } else {
                    ref_cache[y * 5 + 5] = ref_cache[y * 5];
                }
            }
            for (x, (cc, mv)) in ccs[1..].iter_mut().zip(mvs[1..].iter_mut()).enumerate() {
                let blk4 = self.get_cur_blk4(x + y * 4);
                *cc = blk4.ncoded != 0;
                *mv = blk4.mv;
                ref_cache[x + 1 + (y + 1) * 5] = if ((x & 1) == 0) && ((y & 1) == 0) {
                        self.get_cur_blk8(x / 2 + y).ref_idx
                    } else {
                        ref_cache[(x & !1) + 1 + ((y & !1) + 1) * 5]
                    };
            }
        }

        for (y, (((top_ccs, cur_ccs), (top_mvs, cur_mvs)), (cur_refs, top_refs))) in
                coded_cache.chunks_exact(5).take(4).zip(coded_cache[5..].chunks_exact(5)).zip(
                    mv_cache.chunks_exact(5).zip(mv_cache[5..].chunks_exact(5))).zip(
                ref_cache[5..].chunks_exact(5).zip(ref_cache.chunks_exact(5))).enumerate() {
            let can_do_top = y != 0 || (self.mb_y != 0 && (self.has_top || deblock_mode != 2));
            if can_do_top && (!tx8x8 || (y & 1) == 0) {
                if is_s || cur_intra || top_intra {
                    let val = if y == 0 { 0x40 } else { 0x30 };
                    for el in self.deblock[y * 4..][..4].iter_mut() { *el |= val; }
                } else {
                    for (x, (((&cur_cc, &top_cc), (cur_mv, top_mv)), (&cur_ref, &top_ref))) in
                            cur_ccs[1..].iter().zip(top_ccs[1..].iter()).zip(
                                cur_mvs[1..].iter().zip(top_mvs[1..].iter())).zip(
                            cur_refs[1..].iter().zip(
                                top_refs[1..].iter())).take(4).enumerate() {
                        if cur_cc || top_cc {
                            self.deblock[y * 4 + x] |= 0x20;
                        } else {
                            if mvdiff4(cur_mv, top_mv) || !frefs.cmp_refs(cur_ref, top_ref) {
                                self.deblock[y * 4 + x] |= 0x10;
                            }
                        }
                    }
                }
            }
            let mut lleft_intra = left_intra;
            for (x, (((&cur_cc, &left_cc), (cur_mv, left_mv)), (&cur_ref, &left_ref))) in
                    cur_ccs[1..].iter().zip(cur_ccs.iter()).zip(
                        cur_mvs[1..].iter().zip(cur_mvs.iter())).zip(
                    cur_refs[1..].iter().zip(cur_refs.iter())).enumerate() {
                let skip_8 = tx8x8 && (x & 1) != 0;
                let can_do_left = x > 0 || self.has_left || (self.mb_x != 0 && deblock_mode != 2);
                if !can_do_left {
                    continue;
                }
                if skip_8 {
                } else if is_s || cur_intra || lleft_intra {
                    self.deblock[y * 4 + x] |= if x == 0 { 4 } else { 3 };
                } else if cur_cc || left_cc {
                    self.deblock[y * 4 + x] |= 2;
                } else {
                    if mvdiff4(cur_mv, left_mv) || !frefs.cmp_refs(cur_ref, left_ref) {
                        self.deblock[y * 4 + x] |= 1;
                    }
                }
                lleft_intra = cur_intra;
            }
            top_intra = cur_intra;
        }
    }
    pub fn next_mb(&mut self) {
        self.mb_x += 1;
        self.has_left = true;
        if self.mb_x == self.mb_w {
            self.mb_x = 0;
            self.mb_y += 1;
            self.mb.update_row();
            self.blk8.update_row();
            self.blk4.update_row();

            self.has_left = false;
        }
        self.has_top = self.mb_x + self.mb_y * self.mb_w >= self.mb_start + self.mb_w;
    }
    pub fn get_cur_mb_idx(&self) -> usize { self.mb.xpos + self.mb_x }
    pub fn get_cur_blk8_idx(&self, blk_no: usize) -> usize {
        self.blk8.xpos + self.mb_x * 2 + (blk_no & 1) + (blk_no >> 1) * self.blk8.stride
    }
    pub fn get_cur_blk4_idx(&self, blk_no: usize) -> usize {
        self.blk4.xpos + self.mb_x * 4 + (blk_no & 3) + (blk_no >> 2) * self.blk4.stride
    }
    pub fn get_cur_mb(&mut self) -> &mut MBData {
        let idx = self.get_cur_mb_idx();
        &mut self.mb.data[idx]
    }
    pub fn get_left_mb(&self) -> &MBData {
        &self.mb.data[self.get_cur_mb_idx() - 1]
    }
    pub fn get_top_mb(&self) -> &MBData {
        &self.mb.data[self.get_cur_mb_idx() - self.mb.stride]
    }
    pub fn get_cur_blk8(&mut self, blk_no: usize) -> &mut Blk8Data {
        let idx = self.get_cur_blk8_idx(blk_no);
        &mut self.blk8.data[idx]
    }
    pub fn get_left_blk8(&self, blk_no: usize) -> &Blk8Data {
        &self.blk8.data[self.get_cur_blk8_idx(blk_no) - 1]
    }
    pub fn get_top_blk8(&self, blk_no: usize) -> &Blk8Data {
        &self.blk8.data[self.get_cur_blk8_idx(blk_no) - self.blk8.stride]
    }
    pub fn get_cur_blk4(&mut self, blk_no: usize) -> &mut Blk4Data {
        let idx = self.get_cur_blk4_idx(blk_no);
        &mut self.blk4.data[idx]
    }
    pub fn get_left_blk4(&self, blk_no: usize) -> &Blk4Data {
        &self.blk4.data[self.get_cur_blk4_idx(blk_no) - 1]
    }
    pub fn get_top_blk4(&self, blk_no: usize) -> &Blk4Data {
        &self.blk4.data[self.get_cur_blk4_idx(blk_no) - self.blk4.stride]
    }

    pub fn apply_to_blk8<F: (Fn(&mut Blk8Data))>(&mut self, f: F) {
        let start = self.get_cur_blk8_idx(0);
        for row in self.blk8.data[start..].chunks_mut(self.blk8.stride).take(2) {
            for el in row[..2].iter_mut() {
                f(el);
            }
        }
    }
    pub fn apply_to_blk4<F: (Fn(&mut Blk4Data))>(&mut self, f: F) {
        let start = self.get_cur_blk4_idx(0);
        for row in self.blk4.data[start..].chunks_mut(self.blk4.stride).take(4) {
            for el in row[..4].iter_mut() {
                f(el);
            }
        }
    }

    pub fn fill_ipred(&mut self, imode: IntraPredMode) {
        self.apply_to_blk4(|blk| blk.ipred = imode);
    }
    pub fn fill_ncoded(&mut self, nc: u8) {
        self.apply_to_blk4(|blk| blk.ncoded = nc);
        self.apply_to_blk8(|blk| blk.ncoded_c = [nc; 2]);
    }
    pub fn reset_mb_mv(&mut self) {
        self.apply_to_blk8(|blk| blk.ref_idx = [INVALID_REF; 2]);
    }

    pub fn get_mv_ctx(&self, xoff: usize, yoff: usize, ref_l: usize) -> (usize, usize) {
        let blk_no = xoff / 4 + yoff;
        let mv_a = self.get_left_blk4(blk_no).mvd[ref_l];
        let mv_b = self.get_top_blk4(blk_no).mvd[ref_l];
        let mv = mv_a + mv_b;
        let ctx0 = if mv.x < 3 { 0 } else if mv.x <= 32 { 1 } else { 2 };
        let ctx1 = if mv.y < 3 { 0 } else if mv.y <= 32 { 1 } else { 2 };
        (ctx0, ctx1)
    }
    pub fn get_mv_ref_ctx(&self, xoff: usize, yoff: usize, ref_l: usize) -> usize {
        let blk_no = xoff / 8 + (yoff / 8) * 2;
        let mut ctx = 0;
        let left_ref = self.get_left_blk8(blk_no).ref_idx[ref_l];
        let top_ref = self.get_top_blk8(blk_no).ref_idx[ref_l];
        if !left_ref.not_avail() && !left_ref.is_direct() && left_ref.index() > 0 {
            ctx += 1;
        }
        if !top_ref.not_avail() && !top_ref.is_direct() && top_ref.index() > 0 {
            ctx += 2;
        }
        ctx
    }
    #[allow(clippy::if_same_then_else)]
    pub fn predict(&mut self, xpos: usize, ypos: usize, bw: usize, bh: usize, ref_l: usize, diff_mv: MV, ref_idx: PicRef) {
        let midx = self.get_cur_blk4_idx(0) + xpos / 4 + ypos / 4 * self.blk4.stride;
        let ridx = self.get_cur_blk8_idx(0) + xpos / 8 + ypos / 8 * self.blk8.stride;
        let ridx_c = self.get_cur_blk8_idx(0) + (xpos + bw) / 8 + ypos / 8 * self.blk8.stride - if (ypos & 4) == 0 { self.blk8.stride } else { 0 };

        let mv_a = self.blk4.data[midx - 1].mv[ref_l];
        let mv_b = self.blk4.data[midx - self.blk4.stride].mv[ref_l];
        let mut mv_c = self.blk4.data[midx - self.blk4.stride + bw / 4].mv[ref_l];

        let rx = if (xpos & 4) != 0 { 0 } else { 1 };
        let ry = if (ypos & 4) != 0 { 0 } else { self.blk8.stride };
        let ref_a = self.blk8.data[ridx - rx].ref_idx[ref_l];
        let ref_b = self.blk8.data[ridx - ry].ref_idx[ref_l];
        let mut ref_c = self.blk8.data[ridx_c].ref_idx[ref_l];

        if ref_c == MISSING_REF || (((xpos + bw) & 4) == 0 && (ypos & 4) != 0) {
            mv_c = self.blk4.data[midx - self.blk4.stride - 1].mv[ref_l];
            ref_c = self.blk8.data[ridx - rx - ry].ref_idx[ref_l];
        }

        let pred_mv = if bw == 16 && bh == 8 && ypos == 0 && ref_b == ref_idx {
                mv_b
            } else if bw == 16 && bh == 8 && ypos != 0 && ref_a == ref_idx {
                mv_a
            } else if bw == 8 && bh == 16 && xpos == 0 && ref_a == ref_idx {
                mv_a
            } else if bw == 8 && bh == 16 && xpos != 0 && ref_c == ref_idx {
                mv_c
            } else if ref_b == MISSING_REF && ref_c == MISSING_REF {
                mv_a
            } else {
                let count = ((ref_a == ref_idx) as u8) + ((ref_b == ref_idx) as u8) + ((ref_c == ref_idx) as u8);
                if count == 1 {
                    if ref_a == ref_idx {
                        mv_a
                    } else if ref_b == ref_idx {
                        mv_b
                    } else {
                        mv_c
                    }
                } else {
                    MV::pred(mv_a, mv_b, mv_c)
                }
            };

        let mv = pred_mv + diff_mv;
        self.fill_mv (xpos, ypos, bw, bh, ref_l, mv);
        self.fill_ref(xpos, ypos, bw, bh, ref_l, ref_idx);
    }
    pub fn predict_pskip(&mut self) {
        let midx = self.get_cur_blk4_idx(0);
        let ridx = self.get_cur_blk8_idx(0);

        let mv_a = self.blk4.data[midx - 1].mv[0];
        let mv_b = self.blk4.data[midx - self.blk4.stride].mv[0];
        let mut mv_c = self.blk4.data[midx - self.blk4.stride + 4].mv[0];

        let ref_a = self.blk8.data[ridx - 1].ref_idx[0];
        let ref_b = self.blk8.data[ridx - self.blk8.stride].ref_idx[0];
        let mut ref_c = self.blk8.data[ridx - self.blk8.stride + 2].ref_idx[0];

        if ref_c == MISSING_REF {
            mv_c = self.blk4.data[midx - self.blk4.stride - 1].mv[0];
            ref_c = self.blk8.data[ridx - self.blk8.stride - 1].ref_idx[0];
        }

        let ref_idx = ZERO_REF;
        let mv = if ref_a == MISSING_REF || ref_b == MISSING_REF || (ref_a == ZERO_REF && mv_a == ZERO_MV) || (ref_b == ZERO_REF && mv_b == ZERO_MV) {
                ZERO_MV
            } else {
                let count = ((ref_a == ref_idx) as u8) + ((ref_b == ref_idx) as u8) + ((ref_c == ref_idx) as u8);
                if count == 1 {
                    if ref_a == ref_idx {
                        mv_a
                    } else if ref_b == ref_idx {
                        mv_b
                    } else {
                        mv_c
                    }
                } else {
                    MV::pred(mv_a, mv_b, mv_c)
                }
            };

        self.fill_mv (0, 0, 16, 16, 0, mv);
        self.fill_ref(0, 0, 16, 16, 0, ref_idx);
    }
    pub fn predict_direct_mb(&mut self, frame_refs: &SimplifiedSliceRefs, temporal_mv: bool, direct_8x8: bool, cur_id: u16) {
        let (col_mb, r1_poc, r1_long) = frame_refs.get_colocated_info(self.mb_x, self.mb_y);
        if direct_8x8 {
            for blk4 in 0..16 {
                let (mv0, ref0, mv1, ref1) = self.get_direct_mv(frame_refs, &col_mb, r1_poc, r1_long, temporal_mv, cur_id, BLK4_TO_D8[blk4]);
                self.get_cur_blk4(blk4).mv = [mv0, mv1];
                self.get_cur_blk8(blk4_to_blk8(blk4)).ref_idx = [ref0, ref1];
            }
        } else if col_mb.mb_type.is_16x16_ref() || !temporal_mv {
            let (mv0, ref0, mv1, ref1) = self.get_direct_mv(frame_refs, &col_mb, r1_poc, r1_long, temporal_mv, cur_id, 0);
            self.apply_to_blk4(|blk4| blk4.mv = [mv0, mv1]);
            self.apply_to_blk8(|blk8| blk8.ref_idx = [ref0, ref1]);
        } else {
            for blk4 in 0..16 {
                let (mv0, ref0, mv1, ref1) = self.get_direct_mv(frame_refs, &col_mb, r1_poc, r1_long, temporal_mv, cur_id, blk4);
                self.get_cur_blk4(blk4).mv = [mv0, mv1];
                self.get_cur_blk8(blk4_to_blk8(blk4)).ref_idx = [ref0, ref1];
            }
        }
    }
    pub fn predict_direct_sub(&mut self, frame_refs: &SimplifiedSliceRefs, temporal_mv: bool, direct8x8: bool, cur_id: u16, blk4: usize) {
        let src_blk = if !direct8x8 { blk4 } else { BLK4_TO_D8[blk4] };
        let (mbi, r1_poc, r1_long) = frame_refs.get_colocated_info(self.mb_x, self.mb_y);
        let (mv0, ref0, mv1, ref1) = self.get_direct_mv(frame_refs, &mbi, r1_poc, r1_long, temporal_mv, cur_id, src_blk);
        self.get_cur_blk4(blk4).mv = [mv0, mv1];
        self.get_cur_blk8(blk4_to_blk8(blk4)).ref_idx = [ref0, ref1];
    }
    #[allow(clippy::nonminimal_bool)]
    pub fn get_direct_mv(&self, frame_refs: &SimplifiedSliceRefs, mbi: &FrameMBInfo, r1_poc: u16, r1_long: bool, temporal_mv: bool, cur_id: u16, blk4: usize) -> (MV, PicRef, MV, PicRef) {
        let blk8 = blk4_to_blk8(blk4);
        let (col_mv, r0_poc, col_idx) = if mbi.ref_poc[blk8] == [MISSING_POC; 2] {
                (ZERO_MV, MISSING_POC, MISSING_REF)
            } else if mbi.ref_poc[blk8][0] != MISSING_POC {
                (mbi.mv[blk4][0], mbi.ref_poc[blk8][0], mbi.ref_idx[blk8][0])
            } else {
                (mbi.mv[blk4][1], mbi.ref_poc[blk8][1], mbi.ref_idx[blk8][1])
            };
        let (col_ref, r0_long) = frame_refs.map_ref0(r0_poc);
        if temporal_mv {
            let td = (i32::from(r1_poc) - i32::from(r0_poc)).max(-128).min(127);
            if r0_long || td == 0 {
                (col_mv, col_ref, ZERO_MV, ZERO_REF)
            } else {
                let tx = (16384 + (td / 2).abs()) / td;
                let tb = (i32::from(cur_id) - i32::from(r0_poc)).max(-128).min(127);
                let scale = ((tb * tx + 32) >> 6).max(-1024).min(1023);
                let mv0 = MV {
                        x: ((i32::from(col_mv.x) * scale + 128) >> 8) as i16,
                        y: ((i32::from(col_mv.y) * scale + 128) >> 8) as i16,
                    };
                let mv1 = mv0 - col_mv;
                (mv0, col_ref, mv1, ZERO_REF)
            }
        } else {
            let blk4 = 0; // we generate the same MV prediction for the whole MB
            let blk8 = blk4_to_blk8(blk4);
            let midx = self.get_cur_blk4_idx(blk4);
            let ridx = self.get_cur_blk8_idx(blk8);
            let ridx_c = self.get_cur_blk8_idx(blk8) + 16 / 8 - self.blk8.stride;

            let mv_a = self.blk4.data[midx - 1].mv;
            let mv_b = self.blk4.data[midx - self.blk4.stride].mv;
            let mut mv_c = self.blk4.data[midx - self.blk4.stride + 16 / 4].mv;

            let ref_a = self.blk8.data[ridx - 1].ref_idx;
            let ref_b = self.blk8.data[ridx - self.blk8.stride].ref_idx;
            let mut ref_c = self.blk8.data[ridx_c].ref_idx;

            if ref_c == [MISSING_REF; 2] {
                mv_c = self.blk4.data[midx - self.blk4.stride - 1].mv;
                ref_c = self.blk8.data[ridx - self.blk8.stride - 1].ref_idx;
            }
            let mut refs = [INVALID_REF; 2];
            for cur_ref in [ref_a, ref_b, ref_c].iter() {
                refs[0] = refs[0].min_pos(cur_ref[0]);
                refs[1] = refs[1].min_pos(cur_ref[1]);
            }
            if refs == [INVALID_REF; 2] {
                return (ZERO_MV, ZERO_REF, ZERO_MV, ZERO_REF);
            }

            let mut col_zero = true;
            if r1_long || col_idx != ZERO_REF {
                col_zero = false;
            }
            if col_mv.x.abs() > 1 || col_mv.y.abs() > 1 {
                col_zero = false;
            }
            let mut mvs = [ZERO_MV; 2];
            for ref_l in 0..2 {
                if mbi.mb_type.is_intra() || (!refs[ref_l].not_avail() && !(refs[ref_l] == ZERO_REF && col_zero)) {
                    let ref_idx = refs[ref_l];
                    mvs[ref_l] = if ref_b[ref_l] == MISSING_REF && ref_c[ref_l] == MISSING_REF {
                            mv_a[ref_l]
                        } else {
                            let count = ((ref_a[ref_l] == ref_idx) as u8) + ((ref_b[ref_l] == ref_idx) as u8) + ((ref_c[ref_l] == ref_idx) as u8);
                            if count == 1 {
                                if ref_a[ref_l] == ref_idx {
                                    mv_a[ref_l]
                                } else if ref_b[ref_l] == ref_idx {
                                    mv_b[ref_l]
                                } else {
                                    mv_c[ref_l]
                                }
                            } else {
                                MV::pred(mv_a[ref_l], mv_b[ref_l], mv_c[ref_l])
                            }
                        };
                }
            }
            (mvs[0], refs[0], mvs[1], refs[1])
        }
    }
    pub fn fill_mv(&mut self, xpos: usize, ypos: usize, bw: usize, bh: usize, ref_l: usize, mv: MV) {
        let start = self.get_cur_blk4_idx(0) + xpos / 4 + ypos / 4 * self.blk4.stride;
        for row in self.blk4.data[start..].chunks_mut(self.blk4.stride).take(bh / 4) {
            for blk in row[..bw / 4].iter_mut() {
                blk.mv[ref_l] = mv;
            }
        }
    }
    pub fn fill_mvd(&mut self, xpos: usize, ypos: usize, bw: usize, bh: usize, ref_l: usize, mv: MV) {
        let mvd = MV{ x: mv.x.abs().min(128), y: mv.y.abs().min(128) };
        let start = self.get_cur_blk4_idx(0) + xpos / 4 + ypos / 4 * self.blk4.stride;
        for row in self.blk4.data[start..].chunks_mut(self.blk4.stride).take(bh / 4) {
            for blk in row[..bw / 4].iter_mut() {
                blk.mvd[ref_l] = mvd;
            }
        }
    }
    pub fn fill_ref(&mut self, xpos: usize, ypos: usize, bw: usize, bh: usize, ref_l: usize, ref_idx: PicRef) {
        let start = self.get_cur_blk8_idx(0) + xpos / 8 + ypos / 8 * self.blk8.stride;
        if bw < 8 || bh < 8 {
            self.blk8.data[start].ref_idx[ref_l] = ref_idx;
        } else {
            for row in self.blk8.data[start..].chunks_mut(self.blk8.stride).take(bh / 8) {
                for blk in row[..bw / 8].iter_mut() {
                    blk.ref_idx[ref_l] = ref_idx;
                }
            }
        }
    }
}

#[cfg(not(target_arch="x86_64"))]
fn mvdiff4(mv1: &[MV; 2], mv2: &[MV; 2]) -> bool {
    let mvd0 = mv1[0] - mv2[0];
    let mvd1 = mv1[1] - mv2[1];
    (mvd0.x.abs() >= 4) || (mvd0.y.abs() >= 4) || (mvd1.x.abs() >= 4) || (mvd1.y.abs() >= 4)
}

#[cfg(target_arch="x86_64")]
fn mvdiff4(mv1: &[MV; 2], mv2: &[MV; 2]) -> bool {
    unsafe {
        let mut flag = false;
        let ptr = std::mem::transmute::<*const MV, *const u64>(mv1.as_ptr());
        let mut m0 = *ptr;
        let ptr = std::mem::transmute::<*const MV, *const u64>(mv2.as_ptr());
        let mut m1 = *ptr;
        for _ in 0..4 {
            let tmp = m0.wrapping_sub(m1) as u16;
            flag |= tmp.wrapping_add(3) > 6;
            m0 >>= 16;
            m1 >>= 16;
        }
        flag
    }
}
