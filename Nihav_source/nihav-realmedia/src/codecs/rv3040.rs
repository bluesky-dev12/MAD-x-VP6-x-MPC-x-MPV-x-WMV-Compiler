use nihav_core::formats::YUV420_FORMAT;
use nihav_core::frame::{NABufferType, NAVideoInfo, NAVideoBuffer, NAVideoBufferRef, FrameType, alloc_video_buffer};
use nihav_core::codecs::{NADecoderSupport, DecoderError, DecoderResult};
use nihav_codec_support::codecs::{MV, ZERO_MV, IPBShuffler};
use nihav_core::io::bitreader::{BitReader,BitReaderMode};
use nihav_core::io::intcode::*;
use nihav_codec_support::data::GenericCache;
use std::mem;

use super::rv34codes::*;
use super::rv34dsp::*;

trait RV34MVScale {
    fn scale(&self, trd: u16, trb: u16) -> (MV, MV);
}

const TR_SHIFT: u8 = 14;
const TR_BIAS: i32 = 1 << (TR_SHIFT - 1);

impl RV34MVScale for MV {
    fn scale(&self, trd: u16, trb: u16) -> (MV, MV) {
        let ratio = ((trb as i32) << TR_SHIFT) / (trd as i32);
        let mv_f = MV {
                x: (((self.x as i32) * ratio + TR_BIAS) >> TR_SHIFT) as i16,
                y: (((self.y as i32) * ratio + TR_BIAS) >> TR_SHIFT) as i16
            };
        let mv_b = mv_f - *self;
        (mv_f, mv_b)
    }
}

#[derive(Clone,Copy)]
pub struct RV34SliceHeader {
    pub ftype:  FrameType,
    pub quant:  u8,
    pub pts:    u16,
    pub width:  usize,
    pub height: usize,
    pub start:  usize,
    pub end:    usize,
    pub set_idx: usize,
    pub deblock: bool,
}

impl RV34SliceHeader {
    pub fn fits(&self, cmp: &RV34SliceHeader) -> bool {
        (self.ftype  == cmp.ftype)  &&
        (self.pts    == cmp.pts)    &&
        (self.width  == cmp.width)  &&
        (self.height == cmp.height)
    }
}

#[allow(dead_code)]
#[derive(Debug,Clone,Copy,PartialEq)]
pub enum MBType {
    MBIntra,
    MBIntra16,
    MBSkip,
    MBP16x16,
    MBP16x16Mix,
    MBP16x8,
    MBP8x16,
    MBP8x8,
    MBDirect,
    MBBidir,
    MBForward,
    MBBackward,
    Invalid,
}

impl MBType {
    pub fn is_intra(self) -> bool {
        (self == MBType::MBIntra) || (self == MBType::MBIntra16)
    }
    pub fn is_16(self) -> bool {
        (self == MBType::MBIntra16) || (self == MBType::MBP16x16Mix)
    }
    pub fn is_intra_or_16(self) -> bool {
        self.is_intra() || self.is_16()
    }
    pub fn get_num_mvs(self) -> usize {
        match self {
            MBType::MBIntra | MBType::MBIntra16 |
            MBType::MBSkip | MBType::MBDirect                       => 0,
            MBType::MBP16x16 | MBType::MBP16x16Mix |
            MBType::MBForward | MBType::MBBackward                  => 1,
            MBType::MBP16x8 | MBType::MBP8x16 | MBType::MBBidir     => 2,
            MBType::MBP8x8                                          => 4,
            MBType::Invalid => unreachable!(),
        }
    }
    pub fn is_fwd(self) -> bool {
        matches!(self,
            MBType::MBP16x16 | MBType::MBP16x16Mix |
            MBType::MBP16x8 | MBType::MBP8x16 | MBType::MBP8x8 |
            MBType::MBForward)
    }
    pub fn is_bwd(self) -> bool {
        matches!(self, MBType::MBBidir | MBType::MBBackward)
    }
    pub fn has_mv_dir(self, fwd: bool) -> bool {
        match self {
            MBType::MBBidir             => true,
            MBType::MBForward  if  fwd  => true,
            MBType::MBBackward if !fwd  => true,
            _ => false,
        }
    }
    pub fn is_nomv(self) -> bool {
        matches!(self, MBType::MBIntra | MBType::MBIntra16 | MBType::MBSkip | MBType::MBDirect)
    }
    /*pub fn is_16x16(self) -> bool {
        match self {
            MBType::MBP16x8 | MBType::MBP8x16 | MBType::MBP8x8 => false,
            _ => true,
        }
    }*/
    fn get_weight(self) -> usize {
        match self {
            MBType::MBIntra     => 0,
            MBType::MBIntra16   => 1,
            MBType::MBSkip      => unreachable!(),
            MBType::MBP16x16    => 2,
            MBType::MBP16x16Mix => 10,
            MBType::MBP16x8     => 7,
            MBType::MBP8x16     => 8,
            MBType::MBP8x8      => 3,
            MBType::MBDirect    => 6,
            MBType::MBBidir     => 9,
            MBType::MBForward   => 4,
            MBType::MBBackward  => 5,
            MBType::Invalid     => unreachable!(),
        }
    }
}

const MBTYPE_FROM_WEIGHT: [MBType; 11] = [
    MBType::MBIntra,    MBType::MBIntra16,  MBType::MBP16x16,   MBType::MBP8x8,
    MBType::MBForward,  MBType::MBBackward, MBType::MBDirect,   MBType::MBP16x8,
    MBType::MBP8x16,    MBType::MBBidir,    MBType::MBP16x16Mix,
];

#[derive(Clone,Copy)]
pub struct MBInfo {
    pub mbtype:     MBType,
    pub skip_run:   usize,
    pub dquant:     bool,
}

#[derive(Clone,Copy)]
pub struct RV34MBInfo {
    pub mbtype: MBType,
    pub cbp:    u32,
    pub deblock:u16,
    pub cbp_c:  u8, // for deblocking purposes
    pub q:      u8,
}

struct IntraModeState {
    cache:  GenericCache<i8>,
}

const RV34_INTRA_PRED4: [PredType4x4; 9] = [
    PredType4x4::DC, PredType4x4::Ver, PredType4x4::Hor,
    PredType4x4::DiagDownRight, PredType4x4::DiagDownLeft,
    PredType4x4::VerRight, PredType4x4::VerLeft,
    PredType4x4::HorUp, PredType4x4::HorDown
];

const RV34_INTRA_PRED16: [PredType8x8; 4] = [
    PredType8x8::DC, PredType8x8::Ver, PredType8x8::Hor, PredType8x8::Plane
];

impl IntraModeState {
    fn new(mb_w: usize) -> Self {
        let stride = 1 + mb_w * 4 + 1;
        IntraModeState { cache: GenericCache::new(4, stride, -1) }
    }
    fn reset(&mut self) { self.cache.reset(); }
    fn update(&mut self) { self.cache.update_row(); }
    fn get_pos(&self, xpos: usize) -> usize {
        self.cache.stride + 1 + xpos * 4
    }
    fn set_mb_x(&mut self, mb_x: usize) {
        self.cache.xpos = self.get_pos(mb_x);
    }
    fn fill_block(&mut self, val: i8) {
        let mut pos = self.cache.xpos;
        for _ in 0..4 {
            for j in 0..4 {
                self.cache.data[pos + j] = val;
            }
            pos += self.cache.stride;
        }
    }
    fn get_pred16_type(&self, has_top: bool, has_left: bool) -> PredType8x8 {
        if !has_top && !has_left { return PredType8x8::DC128; }
        let mut im = RV34_INTRA_PRED16[self.cache.data[self.cache.xpos] as usize];
        if !has_top {
            im = match im {
                    PredType8x8::Plane | PredType8x8::Ver => PredType8x8::Hor,
                    PredType8x8::DC => PredType8x8::LeftDC,
                    _   => im,
                 };
        } else if !has_left {
            im = match im {
                    PredType8x8::Plane | PredType8x8::Hor => PredType8x8::Ver,
                    PredType8x8::DC => PredType8x8::TopDC,
                    _   => im,
                 };
        }
        im
    }
    fn get_pred8_type(&self, has_top: bool, has_left: bool) -> PredType8x8 {
        if !has_top && !has_left { return PredType8x8::DC128; }
        let mut im = RV34_INTRA_PRED16[self.cache.data[self.cache.xpos] as usize];
        im = match im { PredType8x8::Plane => PredType8x8::DC, _ => im };
        if !has_top {
            im = match im {
                    PredType8x8::Plane | PredType8x8::Ver => PredType8x8::Hor,
                    PredType8x8::DC => PredType8x8::LeftDC,
                    _   => im,
                 };
        } else if !has_left {
            im = match im {
                    PredType8x8::Plane | PredType8x8::Hor => PredType8x8::Ver,
                    PredType8x8::DC => PredType8x8::TopDC,
                    _   => im,
                 };
        }
        im
    }
    fn get_pred4_type(&self, x: usize, y: usize, has_top: bool, has_left: bool) -> PredType4x4 {
        let no_up = !has_top && (y == 0);
        let no_left = !has_left && (x == 0);
        if no_up && no_left { return PredType4x4::DC128; }
        let no_down = !has_left || (x != 0) || (y == 3);

        let mut im = RV34_INTRA_PRED4[self.cache.data[self.cache.xpos + x + y * self.cache.stride] as usize];

        if no_up {
            im = match im {
                    PredType4x4::Ver => PredType4x4::Hor,
                    PredType4x4::DC  => PredType4x4::LeftDC,
                    _                => im,
                 };
        } else if no_left {
            im = match im {
                    PredType4x4::Hor            => PredType4x4::Ver,
                    PredType4x4::DC             => PredType4x4::TopDC,
                    PredType4x4::DiagDownLeft   => PredType4x4::DiagDownLeftNoDown,
                    _                           => im,
                 };
        }
        if no_down {
            im = match im {
                    PredType4x4::DiagDownLeft   => PredType4x4::DiagDownLeftNoDown,
                    PredType4x4::HorUp          => PredType4x4::HorUpNoDown,
                    PredType4x4::VerLeft        => PredType4x4::VerLeftNoDown,
                    _                           => im,
                 };
        }
        im
    }
    //todo merge
    fn get_pred4_type_chroma(&self, x: usize, y: usize, has_top: bool, has_left: bool) -> PredType4x4 {
        let no_up = !has_top && (y == 0);
        let no_left = !has_left && (x == 0);
        if no_up && no_left { return PredType4x4::DC128; }
        let no_down = !has_left || (x != 0) || (y == 1);

        let mut im = RV34_INTRA_PRED4[self.cache.data[self.cache.xpos + x * 2 + y * 2 * self.cache.stride] as usize];

        if no_up {
            im = match im {
                    PredType4x4::Ver => PredType4x4::Hor,
                    PredType4x4::DC  => PredType4x4::LeftDC,
                    _                => im,
                 };
        } else if no_left {
            im = match im {
                    PredType4x4::Hor            => PredType4x4::Ver,
                    PredType4x4::DC             => PredType4x4::TopDC,
                    PredType4x4::DiagDownLeft   => PredType4x4::DiagDownLeftNoDown,
                    _                           => im,
                 };
        }
        if no_down {
            im = match im {
                    PredType4x4::DiagDownLeft   => PredType4x4::DiagDownLeftNoDown,
                    PredType4x4::HorUp          => PredType4x4::HorUpNoDown,
                    PredType4x4::VerLeft        => PredType4x4::VerLeftNoDown,
                    _                           => im,
                 };
        }
        im
    }
}

pub struct MVInfo {
    pub mv_b:   Vec<MV>,
    pub mv_f:   Vec<MV>,
    pub w:      usize,
    pub h:      usize,
    pub has_b:  Vec<bool>,
    pub has_f:  Vec<bool>,
}

impl MVInfo {
    fn new() -> Self {
        Self { mv_b: Vec::new(), mv_f: Vec::new(), w: 0, h: 0, has_b: Vec::new(), has_f: Vec::new() }
    }
    fn resize(&mut self, mb_w: usize, mb_h: usize) {
        self.w = mb_w * 2;
        self.h = mb_h * 2;
        self.reset();
    }
    fn reset(&mut self) {
        let size = self.w * self.h;
        self.mv_f.clear();
        self.mv_f.resize(size, ZERO_MV);
        self.mv_b.clear();
        self.mv_b.resize(size, ZERO_MV);
        self.has_f.clear();
        self.has_f.resize(size >> 2, false);
        self.has_b.clear();
        self.has_b.resize(size >> 2, false);
    }
    fn fill(&mut self, mb_x: usize, mb_y: usize, fwd: bool, mv: MV) {
        let idx = mb_x * 2 + mb_y * 2 * self.w;
        if fwd {
            self.mv_f[idx          + 0] = mv;
            self.mv_f[idx          + 1] = mv;
            self.mv_f[idx + self.w + 0] = mv;
            self.mv_f[idx + self.w + 1] = mv;
        } else {
            self.mv_b[idx          + 0] = mv;
            self.mv_b[idx          + 1] = mv;
            self.mv_b[idx + self.w + 0] = mv;
            self.mv_b[idx + self.w + 1] = mv;
        }
    }
    fn get_mv_by_idx(&self, idx: usize, fwd: bool) -> MV {
        if fwd { self.mv_f[idx] } else { self.mv_b[idx] }
    }
    fn pred_mv(&self, idx: usize, fwd: bool, has_top: bool, has_left: bool, has_tr: bool, has_tl: bool, is16: bool) -> MV {
        if !has_top && !has_left { return ZERO_MV; }
        let left_mv = if has_left { self.get_mv_by_idx(idx - 1, fwd) } else { ZERO_MV };
        let top_mv  = if has_top  { self.get_mv_by_idx(idx - self.w, fwd) } else { left_mv };
        let tr_add = if is16 { 2 } else { 1 };
        let tr_mv;
        if has_tr {
            tr_mv = self.get_mv_by_idx(idx - self.w + tr_add, fwd);
        } else if has_tl {
            tr_mv = self.get_mv_by_idx(idx - self.w - 1, fwd);
        } else {
            tr_mv = left_mv;
        }
        MV::pred(left_mv, top_mv, tr_mv)
    }
    pub fn pred_mb_mv(&self, mb_x: usize, mb_y: usize, fwd: bool, has_top: bool, has_left: bool, has_tr: bool, has_tl: bool) -> MV {
        self.pred_mv(mb_x * 2 + mb_y * 2 * self.w, fwd, has_top, has_left, has_tr, has_tl, true)
    }
    fn set_mb(&mut self, mb_x: usize, mb_y: usize, mbtype: MBType, ref_mvi: &Self, mvs: &[MV], sstate: &SState) {
        let mb_idx = mb_x + mb_y * (self.w >> 1);
        self.has_f[mb_idx] = mbtype.is_fwd();
        self.has_b[mb_idx] = mbtype.is_bwd();
        if mbtype.is_nomv() {
            self.fill(mb_x, mb_y, true,  ZERO_MV);
            self.fill(mb_x, mb_y, false, ZERO_MV);
            return;
        }
        if mbtype.is_fwd() {
            self.fill(mb_x, mb_y, false, ZERO_MV);
        } else if mbtype.is_bwd() {
            self.fill(mb_x, mb_y, true, ZERO_MV);
        }
        let idx = mb_x * 2 + mb_y * 2 * self.w;

        match mbtype {
            MBType::MBSkip => {
                    self.fill(mb_x, mb_y, true, ZERO_MV/*pred_mv*/);
                },
            MBType::MBP16x16 |
            MBType::MBP16x16Mix => {
                    let pred_mv = self.pred_mv(idx, mbtype.is_fwd(), sstate.has_top, sstate.has_left, sstate.has_tr, sstate.has_tl, true);
                    let new_mv = mvs[0] + pred_mv;
                    self.fill(mb_x, mb_y, true, new_mv);
                },
            MBType::MBP16x8 => {
                    let pred_mv = self.pred_mv(idx, mbtype.is_fwd(), sstate.has_top, sstate.has_left, sstate.has_tr, sstate.has_tl, true);
                    let new_mv = mvs[0] + pred_mv;
                    self.mv_f[idx + 0] = new_mv;
                    self.mv_f[idx + 1] = new_mv;

                    let idx2 = idx + self.w;
                    let pred_mv = self.pred_mv(idx2, true, true, sstate.has_left, false, sstate.has_left, true);
                    let new_mv = mvs[1] + pred_mv;
                    self.mv_f[idx2 + 0] = new_mv;
                    self.mv_f[idx2 + 1] = new_mv;
                },
            MBType::MBP8x16 => {
                    let pred_mv = self.pred_mv(idx, true, sstate.has_top, sstate.has_left, sstate.has_top, sstate.has_tl, false);
                    let new_mv = mvs[0] + pred_mv;
                    self.mv_f[idx]          = new_mv;
                    self.mv_f[idx + self.w] = new_mv;

                    let pred_mv = self.pred_mv(idx + 1, true, sstate.has_top, true, sstate.has_tr, sstate.has_top, false);
                    let new_mv = mvs[1] + pred_mv;
                    self.mv_f[idx          + 1] = new_mv;
                    self.mv_f[idx + self.w + 1] = new_mv;
                },
            MBType::MBP8x8 => {
                    let mut idx8 = idx;
                    let mut has_top = sstate.has_top;
                    for y in 0..2 {
                        for x in 0..2 {
                            let has_left = (x > 0) || sstate.has_left;
                            let has_tr = if y > 0 { x == 0 } else if x == 0 { sstate.has_top } else { sstate.has_tr };
                            let has_tl;
                            if y == 0 {
                                has_tl = if x == 0 { sstate.has_tl } else { sstate.has_top };
                            } else {
                                has_tl = if x == 0 { sstate.has_left } else { true };
                            }
                            let pred_mv = self.pred_mv(idx8 + x, true, has_top, has_left, has_tr, has_tl, false);
                            let new_mv = mvs[x + y * 2] + pred_mv;
                            self.mv_f[idx8 + x] = new_mv;
                        }
                        has_top = true;
                        idx8 += self.w;
                    }
                },
            MBType::MBDirect => {
                    let mut cum_mv_f = ZERO_MV;
                    let mut cum_mv_b = ZERO_MV;
                    let mut idx8 = idx;
                    for _ in 0..2 {
                        for x in 0..2 {
                            let (mv_f, mv_b) = ref_mvi.mv_f[idx8 + x].scale(sstate.trd, sstate.trb);
                            cum_mv_f += mv_f;
                            cum_mv_b += mv_b;
                        }
                        idx8 += self.w;
                    }
                    cum_mv_f.x >>= 2;
                    cum_mv_f.y >>= 2;
                    cum_mv_b.x >>= 2;
                    cum_mv_b.y >>= 2;
                    self.fill(mb_x, mb_y, true,  cum_mv_f);
                    self.fill(mb_x, mb_y, false, cum_mv_b);
                },
            MBType::MBBidir => {
                    let pred_mv_f = ZERO_MV;
                    let new_mv = pred_mv_f + mvs[0];
                    self.fill(mb_x, mb_y, true,  new_mv);
                    let pred_mv_b = ZERO_MV;
                    let new_mv = pred_mv_b + mvs[1];
                    self.fill(mb_x, mb_y, false, new_mv);
                },
            MBType::MBForward => {
                    let pred_mv = self.pred_mv(idx, mbtype.is_fwd(), sstate.has_top, sstate.has_left, sstate.has_tr, sstate.has_tl, true);
                    let new_mv = mvs[0] + pred_mv;
                    self.fill(mb_x, mb_y, true,  new_mv);
                },
            MBType::MBBackward => {
                    let pred_mv = self.pred_mv(idx, mbtype.is_fwd(), sstate.has_top, sstate.has_left, sstate.has_tr, sstate.has_tl, true);
                    let new_mv = mvs[0] + pred_mv;
                    self.fill(mb_x, mb_y, false, new_mv);
                },
            _ => {},
        }
    }
    pub fn get_mv(&self, mb_x: usize, mb_y: usize, x: usize, y: usize, fwd: bool) -> MV {
        let idx = mb_x * 2 + x + (mb_y * 2 + y) * self.w;
        if fwd { self.mv_f[idx] }
        else   { self.mv_b[idx] }
    }
    fn mv_gt_3(&self, mb_x: usize, mb_y: usize, x: usize, y: usize, vert: bool) -> bool {
        let idx = mb_x * 2 + x + (mb_y * 2 + y) * self.w;
        let off = if vert { self.w } else { 1 };
        let diffx = self.mv_f[idx].x - self.mv_f[idx - off].x;
        let diffy = self.mv_f[idx].y - self.mv_f[idx - off].y;
        (diffx < -3) || (diffx > 3) || (diffy < -3) || (diffy > 3)
    }
}

pub trait RV34BitstreamDecoder {
    fn decode_slice_header(&mut self, br: &mut BitReader, old_w: usize, old_h: usize) -> DecoderResult<RV34SliceHeader>;
    fn decode_intra_pred(&mut self, br: &mut BitReader, types: &mut [i8], pos: usize, tstride: usize, has_top: bool) -> DecoderResult<()>;
    fn quant_dc(&self, is_intra: bool, q: u8) -> u8;
    fn decode_inter_mb_hdr(&mut self, br: &mut BitReader, ftype: FrameType, mbtype: MBType) -> DecoderResult<MBInfo>;
    fn predict_b_mv(&self, sstate: &SState, mvi: &MVInfo, mbtype: MBType, mvs: &[MV], mbinfo: &[RV34MBInfo]) -> (MV, MV);
}

pub trait RV34DSP {
    fn loop_filter(&self, frame: &mut NAVideoBuffer<u8>, ftype: FrameType, mbinfo: &[RV34MBInfo], mb_w: usize, mb_h: usize, row: usize);
    fn do_luma_mc(&self, frame: &mut NAVideoBuffer<u8>, prev_frame: &NAVideoBuffer<u8>, x: usize, y: usize, mv: MV, use16: bool, avg: bool);
    fn do_chroma_mc(&self, frame: &mut NAVideoBuffer<u8>, prev_frame: &NAVideoBuffer<u8>, x: usize, y: usize, comp: usize, mv: MV, use8: bool, avg: bool);
}

fn parse_slice_offsets(src: &[u8], offsets: &mut Vec<usize>) -> DecoderResult<()> {
    let num_slices = (src[0] as usize) + 1;
    let ini_off = num_slices * 8 + 1;
    offsets.clear();

    if ini_off >= src.len() { return Err(DecoderError::ShortData); }

    let mut br = BitReader::new(&src[1..ini_off], BitReaderMode::BE);

    for i in 0..num_slices {
        br.skip(32)?;
        let off = br.read(32)? as usize;
        if (i == 0) && (off != 0) {
            return Err(DecoderError::InvalidData);
        }
        if (i > 0) && (off <= offsets[i - 1]) {
            return Err(DecoderError::InvalidData);
        }
        offsets.push(off);
    }

    Ok(())
}

fn decode_slice_header(br: &mut BitReader, bd: &mut dyn RV34BitstreamDecoder, slice_no: usize, slice_offs: &[usize], old_width: usize, old_height: usize) -> DecoderResult<RV34SliceHeader> {
    validate!(slice_no < slice_offs.len());
    br.seek((slice_offs[slice_no] * 8) as u32)?;
    let mut shdr = bd.decode_slice_header(br, old_width, old_height)?;
    if ((shdr.width == 0) || (shdr.height == 0)) && (shdr.ftype != FrameType::I) {
        return Err(DecoderError::MissingReference);
    }
    if slice_no < slice_offs.len() - 1 {
        let cur_pos = br.tell() as u32;
        br.seek((slice_offs[slice_no + 1] * 8) as u32)?;
        if let Ok(nhdr) = bd.decode_slice_header(br, shdr.width, shdr.height) {
            validate!(nhdr.start > shdr.start);
            shdr.end = nhdr.start;
        } else {
            if slice_no + 2 < slice_offs.len() {
                br.seek((slice_offs[slice_no + 2] * 8) as u32)?;
                if let Ok(nhdr) = bd.decode_slice_header(br, shdr.width, shdr.height) {
                    validate!(nhdr.start > shdr.start);
                    shdr.end = nhdr.start;
                } else {
                    shdr.end = ((shdr.width + 15) >> 4) * ((shdr.height + 15) >> 4);
                }
            } else {
                shdr.end = ((shdr.width + 15) >> 4) * ((shdr.height + 15) >> 4);
            }
        }
        br.seek(cur_pos)?;
    } else {
        shdr.end = ((shdr.width + 15) >> 4) * ((shdr.height + 15) >> 4);
    }
    Ok(shdr)
}

const RV34_MB_MAX_SIZES: [usize; 6] = [ 0x2F, 0x62, 0x18B, 0x62F, 0x18BF, 0x23FF ];
const RV34_SLICE_START_BITS: [u8; 6] = [ 6, 7, 9, 11, 13, 14 ];

pub fn get_slice_start_offset_bits(w: usize, h: usize) -> u8 {
    if (w == 0) || (h == 0) {
        return 0;
    }
    let mb_size = ((w + 15) >> 4) * ((h + 15) >> 4) - 1;
    let mut idx: usize = 0;
    while (idx < 5) && (RV34_MB_MAX_SIZES[idx] < mb_size) { idx += 1; }
    RV34_SLICE_START_BITS[idx]
}

const RV34_DQUANT_TAB: [[i8; 2]; 32] = [
    [  0, 0 ], [  2, 1 ], [ -1, 1 ], [ -1, 1 ], [ -1, 1 ], [ -1, 1 ], [ -1, 1 ], [ -1, 1 ],
    [ -1, 1 ], [ -1, 1 ], [ -1, 1 ], [ -2, 2 ], [ -2, 2 ], [ -2, 2 ], [ -2, 2 ], [ -2, 2 ],
    [ -2, 2 ], [ -2, 2 ], [ -2, 2 ], [ -2, 2 ], [ -2, 2 ], [ -3, 3 ], [ -3, 3 ], [ -3, 3 ],
    [ -3, 3 ], [ -3, 3 ], [ -3, 3 ], [ -3, 3 ], [ -3, 3 ], [ -3, 2 ], [ -3, 1 ], [ -3,-5 ]
];

const RV34_QUANT_TAB: [u16; 32] = [
     60,   67,   76,   85,   96,  108,  121,  136,
    152,  171,  192,  216,  242,  272,  305,  341,
    383,  432,  481,  544,  606,  683,  767,  854,
    963, 1074, 1212, 1392, 1566, 1708, 1978, 2211
];

const RV34_CHROMA_QUANT_DC: [u8; 32] = [
     0,  0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,
    14, 15, 15, 16, 17, 18, 18, 19, 20, 20, 21, 21, 22, 22, 23, 23
];
const RV34_CHROMA_QUANT_AC: [u8; 32] = [
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    16, 17, 17, 18, 19, 20, 20, 21, 22, 22, 23, 23, 24, 24, 25, 25
];

fn decode_dquant(br: &mut BitReader, q: u8) -> DecoderResult<u8> {
    if br.read_bool()? {
        let diff = RV34_DQUANT_TAB[q as usize][br.read(1)? as usize];
        let qp = (q as i8) + diff;
        validate!((qp > 0) && (qp < 32));
        Ok(qp as u8)
    } else {
        let qp = br.read(5)? as u8;
        Ok(qp)
    }
}

pub struct SState {
    pub mb_x:       usize,
    pub mb_y:       usize,
    pub mb_w:       usize,
    pub mb_h:       usize,
    pub cbp:        u32,
    pub q:          u8,
    pub q_dc:       u8,
    pub set_idx:    usize,
    pub has_left:   bool,
    pub has_top:    bool,
    pub has_tl:     bool,
    pub has_tr:     bool,
    pub trd:        u16,
    pub trb:        u16,
}

impl SState {
    fn new() -> Self {
        Self {
            mb_x:       0,
            mb_y:       0,
            mb_w:       0,
            mb_h:       0,
            cbp:        0,
            q:          0,
            q_dc:       0,
            set_idx:    0,
            has_left:   false,
            has_top:    false,
            has_tl:     false,
            has_tr:     false,
            trd:        0,
            trb:        0,
        }
    }
}

struct MBHist {
    is_p:   bool,
    hist:   [MBType; 4],
    count:  usize,
}

impl MBHist {
    fn new(ftype: FrameType) -> Self { Self { is_p: ftype == FrameType::P, hist: [MBType::Invalid; 4], count: 0 } }
    fn add(&mut self, mbt: MBType) {
        let mbt2 = match mbt {
                MBType::MBSkip if  self.is_p => MBType::MBP16x16,
                MBType::MBSkip if !self.is_p => MBType::MBDirect,
                _ => mbt,
            };
        self.hist[self.count] = mbt2;
        self.count += 1;
    }
    fn get_mbtype(&self) -> MBType {
        if self.count == 0 {
            MBType::MBIntra
        } else if self.count == 1 {
            self.hist[0]
        } else if self.count == 2 {
            if self.hist[0].get_weight() <= self.hist[1].get_weight() {
                self.hist[0]
            } else {
                self.hist[1]
            }
        } else {
            let mut w: [usize; 12] = [0; 12];
            for i in 0..self.count { w[self.hist[i].get_weight()] += 1; }
            let mut nz_idx = 0;
            for i in 0..12 {
                if w[i] == self.count { return MBTYPE_FROM_WEIGHT[i]; }
                if (w[i] > w[nz_idx]) || (w[nz_idx] == 0) { nz_idx = i; }
            }

            MBTYPE_FROM_WEIGHT[nz_idx]
        }
    }
}

fn decode_mv(br: &mut BitReader) -> DecoderResult<MV> {
    let x = br.read_code_signed(IntCodeType::Gamma)? as i16;
    let y = br.read_code_signed(IntCodeType::Gamma)? as i16;
    Ok(MV{ x, y })
}

fn do_mc_16x16(dsp: &mut dyn RV34DSP, buf: &mut NAVideoBuffer<u8>, prevbuf: &NAVideoBuffer<u8>, mb_x: usize, mb_y: usize, mv: MV, avg: bool) {
    dsp.do_luma_mc  (buf, prevbuf, mb_x * 16, mb_y * 16,    mv, true, avg);
    dsp.do_chroma_mc(buf, prevbuf, mb_x *  8, mb_y *  8, 1, mv, true, avg);
    dsp.do_chroma_mc(buf, prevbuf, mb_x *  8, mb_y *  8, 2, mv, true, avg);
}

fn do_mc_8x8(dsp: &mut dyn RV34DSP, buf: &mut NAVideoBuffer<u8>, prevbuf: &NAVideoBuffer<u8>, mb_x: usize, xoff: usize, mb_y: usize, yoff: usize, mv: MV, avg: bool) {
    dsp.do_luma_mc  (buf, prevbuf, mb_x * 16 + xoff * 8, mb_y * 16 + yoff * 8,    mv, false, avg);
    dsp.do_chroma_mc(buf, prevbuf, mb_x *  8 + xoff * 4, mb_y *  8 + yoff * 4, 1, mv, false, avg);
    dsp.do_chroma_mc(buf, prevbuf, mb_x *  8 + xoff * 4, mb_y *  8 + yoff * 4, 2, mv, false, avg);
}

fn do_avg(cdsp: &RV34CommonDSP, buf: &mut NAVideoBuffer<u8>, avg_buf: &NAVideoBuffer<u8>, mb_x: usize, xb: usize, mb_y: usize, yb: usize, size: usize, ratio1: u32, ratio2: u32) {
    for comp in 0..3 {
        let xoff = if comp == 0 { mb_x * 16 + xb * 8 } else { mb_x * 8 + xb * 4 };
        let yoff = if comp == 0 { mb_y * 16 + yb * 8 } else { mb_y * 8 + yb * 4 };
        let csize = if comp == 0 { size } else { size >> 1 };
        let dstride = buf.get_stride(comp);
        let doffset = buf.get_offset(comp) + xoff + yoff * dstride;
        let data = buf.get_data_mut().unwrap();
        let dst: &mut [u8] = data.as_mut_slice();

        let sstride = avg_buf.get_stride(comp);
        let soffset = avg_buf.get_offset(comp);
        let data = avg_buf.get_data();
        let src: &[u8] = data.as_slice();

        if ratio1 == ratio2 {
            cdsp.avg(dst, doffset, dstride, src, soffset, sstride, csize);
        } else {
            cdsp.weight(dst, doffset, dstride, src, soffset, sstride, ratio2, ratio1, csize);
        }
    }
}

pub struct RV34Decoder {
    is_rv30:    bool,
    coderead:   RV34Codes,
    dsp:        Box<dyn RV34DSP + Send>,
    cdsp:       RV34CommonDSP,
    width:      usize,
    height:     usize,
    ipbs:       IPBShuffler,
    mvi:        MVInfo,
    ref_mvi:    MVInfo,
    last_ts:    u16,
    next_ts:    u16,
    ratio1:     u32,
    ratio2:     u32,
    is_b:       bool,
    mbinfo:     Vec<RV34MBInfo>,
    avg_buf:    NAVideoBufferRef<u8>,
    base_ts:    u64,
}

impl RV34Decoder {
    pub fn new(is_rv30: bool, dsp: Box<dyn RV34DSP + Send>) -> Self {
        let tmp_vinfo = NAVideoInfo::new(16, 16, false, YUV420_FORMAT);
        let vt = alloc_video_buffer(tmp_vinfo, 4).unwrap();
        let vb = vt.get_vbuf();
        let avg_buf = vb.unwrap();
        RV34Decoder {
            is_rv30,
            coderead:   RV34Codes::new(),
            dsp,
            cdsp:       RV34CommonDSP::new(),
            ipbs:       IPBShuffler::new(),
            mvi:        MVInfo::new(),
            ref_mvi:    MVInfo::new(),
            mbinfo:     Vec::new(),
            width: 0, height: 0,
            last_ts: 0, next_ts: 0,
            ratio1: 0, ratio2: 0,
            is_b:       false,
            avg_buf,
            base_ts:    0,
        }
    }
    fn decode_mb_header_intra(&mut self, bd: &mut dyn RV34BitstreamDecoder, br: &mut BitReader, is_i16: bool, im: &mut IntraModeState, q: u8, has_top: bool, has_dq: bool) -> DecoderResult<MBInfo> {
        if is_i16 {
            let imode = br.read(2)? as i8;
            im.fill_block(imode);
            Ok(MBInfo { mbtype: MBType::MBIntra16, skip_run: 0, dquant: false })
        } else {
            let dq = if !has_dq {
                    if !self.is_rv30 { !br.read_bool()? } else { false }
                } else { false };
            if dq {
                decode_dquant(br, q)?;
            }
            bd.decode_intra_pred(br, im.cache.data.as_mut_slice(), im.cache.xpos, im.cache.stride, has_top)?;
            Ok(MBInfo { mbtype: MBType::MBIntra, skip_run: 0, dquant: dq })
        }
    }
    fn decode_mb_header_inter(&mut self, bd: &mut dyn RV34BitstreamDecoder, br: &mut BitReader, ftype: FrameType, mbtype: MBType, im: &mut IntraModeState, q: u8, has_top: bool) -> DecoderResult<MBInfo> {
        let hdr = bd.decode_inter_mb_hdr(br, ftype, mbtype)?;
        validate!(hdr.mbtype != MBType::Invalid);
        if hdr.dquant {
            decode_dquant(br, q)?;
        }
        if hdr.mbtype.is_intra() {
            return self.decode_mb_header_intra(bd, br, hdr.mbtype.is_16(), im, q, has_top, true);
        }
        Ok(hdr)
    }

    fn decode_mb_intra(&mut self, sstate: &SState, imode: &IntraModeState, buf: &mut NAVideoBuffer<u8>, br: &mut BitReader, is_16: bool) -> DecoderResult<()> {
        let mut cur_cbp = sstate.cbp;
        {
            let q_dc = RV34_QUANT_TAB[sstate.q_dc as usize];
            let q_ac = RV34_QUANT_TAB[sstate.q as usize];
            let luma_set = if is_16 { 2 } else { 1 };
            let mut coeffs16: [i16; 16] = [0; 16];
            if is_16 {
                let has_ac = self.coderead.decode_block(br, &mut coeffs16, 3, 0, q_dc, q_dc, q_ac)?;
                if has_ac {
                    self.cdsp.transform16(&mut coeffs16);
                } else {
                    self.cdsp.transform16_dc(&mut coeffs16);
                }
            }
            let stride = buf.get_stride(0);
            let mut offset = buf.get_offset(0) + sstate.mb_x * 16 + sstate.mb_y * 16 * stride;
            let data = buf.get_data_mut().unwrap();
            let framebuf: &mut [u8] = data.as_mut_slice();

            if is_16 {
                let im16 = imode.get_pred16_type(sstate.has_top, sstate.has_left);
                self.cdsp.ipred16x16[im16 as usize](framebuf, offset, stride);
            }

            for y in 0..4 {
                for x in 0..4 {
                    let mut coeffs: [i16; 16] = [0; 16];
                    let has_ac;
                    if (cur_cbp & 1) != 0 {
                        has_ac = self.coderead.decode_block(br, &mut coeffs, luma_set, 0, q_ac, q_ac, q_ac)?;
                    } else {
                        has_ac = false;
                    }
                    if is_16 {
                        coeffs[0] = coeffs16[x + y * 4];
                    } else {
                        let noright = (sstate.mb_x == sstate.mb_w - 1) && (x == 3);
                        let has_top = sstate.has_top || (y > 0);
                        let im = imode.get_pred4_type(x, y, sstate.has_top, sstate.has_left);
                        let topright: [u8; 4] = if (noright && sstate.has_top && y == 0) || (x == 3 && y > 0) {
                                let i = offset + x * 4 - stride;
                                [framebuf[i + 3], framebuf[i + 3], framebuf[i + 3], framebuf[i + 3]]
                            } else if has_top {
                                let i = offset + x * 4 - stride;
                                [framebuf[i + 4], framebuf[i + 5], framebuf[i + 6], framebuf[i + 7]]
                            } else {
                                [0; 4]
                            };
                        self.cdsp.ipred4x4[im as usize](framebuf, offset + x*4, stride, &topright);
                    }
                    if has_ac {
                        self.cdsp.transform(&mut coeffs);
                    } else {
                        self.cdsp.transform_dc(&mut coeffs);
                    }
                    self.cdsp.add_coeffs(framebuf, offset + x * 4, stride, &coeffs);
                    cur_cbp >>= 1;
                }
                offset += stride * 4;
            }
        }
        let q_dc = RV34_QUANT_TAB[RV34_CHROMA_QUANT_DC[sstate.q as usize] as usize];
        let q_ac = RV34_QUANT_TAB[RV34_CHROMA_QUANT_AC[sstate.q as usize] as usize];
        let chroma_set = 0;
        for comp in 1..3 {
            let stride = buf.get_stride(comp);
            let mut offset = buf.get_offset(comp) + sstate.mb_x * 8 + sstate.mb_y * 8 * stride;
            let data = buf.get_data_mut().unwrap();
            let framebuf: &mut [u8] = data.as_mut_slice();
            if is_16 {
                let im8 = imode.get_pred8_type(sstate.has_top, sstate.has_left);
                self.cdsp.ipred8x8[im8 as usize](framebuf, offset, stride);
            }
            for y in 0..2 {
                for x in 0..2 {
                    let mut coeffs: [i16; 16] = [0; 16];
                    let has_ac;
                    if (cur_cbp & 1) != 0 {
                        has_ac = self.coderead.decode_block(br, &mut coeffs, chroma_set, 1, q_dc, q_ac, q_ac)?;
                    } else {
                        has_ac = false;
                    }
                    if !is_16 {
                        let noright = (sstate.mb_x == sstate.mb_w - 1) && (x == 1);
                        let has_top = sstate.has_top || (y > 0);
                        let im = imode.get_pred4_type_chroma(x, y, sstate.has_top, sstate.has_left);
                        let topright: [u8; 4] = if (noright && sstate.has_top && y == 0) || (x == 1 && y > 0) {
                                let i = offset + x * 4 - stride;
                                [framebuf[i + 3], framebuf[i + 3], framebuf[i + 3], framebuf[i + 3]]
                            } else if has_top {
                                let i = offset + x * 4 - stride;
                                [framebuf[i + 4], framebuf[i + 5], framebuf[i + 6], framebuf[i + 7]]
                            } else {
                                [0; 4]
                            };
                        self.cdsp.ipred4x4[im as usize](framebuf, offset + x*4, stride, &topright);
                    }
                    if has_ac {
                        self.cdsp.transform(&mut coeffs);
                    } else {
                        self.cdsp.transform_dc(&mut coeffs);
                    }
                    self.cdsp.add_coeffs(framebuf, offset + x * 4, stride, &coeffs);
                    cur_cbp >>= 1;
                }
                offset += stride * 4;
            }
        }
        Ok(())
    }

    fn do_mc(&mut self, buf: &mut NAVideoBuffer<u8>, mbh: &MBInfo, sstate: &SState) {
        let mb_x = sstate.mb_x;
        let mb_y = sstate.mb_y;
        match mbh.mbtype {
            MBType::MBP16x16 | MBType::MBP16x16Mix => {
                    if let Some(ref prevbuf) = self.ipbs.get_lastref() {
                        let mv = self.mvi.get_mv(mb_x, mb_y, 0, 0, true);
                        do_mc_16x16(self.dsp.as_mut(), buf, prevbuf, mb_x, mb_y, mv, false);
                    }
                },
            MBType::MBForward => {
                    if let Some(ref fwdbuf) = self.ipbs.get_b_fwdref() {
                        let mv = self.mvi.get_mv(mb_x, mb_y, 0, 0, true);
                        do_mc_16x16(self.dsp.as_mut(), buf, fwdbuf, mb_x, mb_y, mv, false);
                    }
                },
            MBType::MBBackward => {
                    if let Some(ref bwdbuf) = self.ipbs.get_b_bwdref() {
                        let mv = self.mvi.get_mv(mb_x, mb_y, 0, 0, false);
                        do_mc_16x16(self.dsp.as_mut(), buf, bwdbuf, mb_x, mb_y, mv, false);
                    }
                },
            MBType::MBP8x8 | MBType::MBP8x16 | MBType::MBP16x8 => {
                    if let Some(ref prevbuf) = self.ipbs.get_lastref() {
                        for y in 0..2 {
                            for x in 0..2 {
                                let mv = self.mvi.get_mv(mb_x, mb_y, x, y, true);
                                do_mc_8x8(self.dsp.as_mut(), buf, prevbuf, mb_x, x, mb_y, y, mv, false);
                            }
                        }
                    }
                },
            MBType::MBSkip if !self.is_b => {
                    if let Some(ref prevbuf) = self.ipbs.get_lastref() {
                        do_mc_16x16(self.dsp.as_mut(), buf, prevbuf, mb_x, mb_y, ZERO_MV, false);
                    }
                },
            MBType::MBSkip | MBType::MBDirect => {
                    if let (Some(ref fwdbuf), Some(ref bwdbuf)) = (self.ipbs.get_b_fwdref(), self.ipbs.get_b_bwdref()) {
                        for y in 0..2 {
                            for x in 0..2 {
                                let (mv_f, mv_b) = self.ref_mvi.get_mv(mb_x, mb_y, x, y, true).scale(sstate.trd, sstate.trb);
                                do_mc_8x8(self.dsp.as_mut(), buf, fwdbuf, mb_x, x, mb_y, y, mv_f, false);
                                do_mc_8x8(self.dsp.as_mut(), &mut self.avg_buf, bwdbuf, mb_x, x, mb_y, y, mv_b, true);
                                do_avg(&self.cdsp, buf, &self.avg_buf, mb_x, x, mb_y, y, 8, self.ratio1, self.ratio2);
                            }
                        }
                    }
                },
            MBType::MBBidir => {
                    if let (Some(ref fwdbuf), Some(ref bwdbuf)) = (self.ipbs.get_b_fwdref(), self.ipbs.get_b_bwdref()) {
                        let mv_f = self.mvi.get_mv(mb_x, mb_y, 0, 0, true);
                        let mv_b = self.mvi.get_mv(mb_x, mb_y, 0, 0, false);
                        do_mc_16x16(self.dsp.as_mut(), buf, fwdbuf, mb_x, mb_y, mv_f, false);
                        do_mc_16x16(self.dsp.as_mut(), &mut self.avg_buf, bwdbuf, mb_x, mb_y, mv_b, true);
                        do_avg(&self.cdsp, buf, &self.avg_buf, mb_x, 0, mb_y, 0, 16, self.ratio1, self.ratio2);
                    }
                },
            _ => {},
        };
    }
    fn decode_mb_inter(&mut self, sstate: &SState, mbh: &MBInfo, buf: &mut NAVideoBuffer<u8>, br: &mut BitReader, is_16: bool) -> DecoderResult<()> {
        self.do_mc(buf, mbh, sstate);

        let mut cur_cbp = sstate.cbp;

        {
            let q_dc = RV34_QUANT_TAB[sstate.q_dc as usize];
            let q_ac = RV34_QUANT_TAB[sstate.q as usize];
            let luma_set = if is_16 { 2 } else { 0 };
            let mut coeffs16: [i16; 16] = [0; 16];
            if is_16 {
                let has_ac = self.coderead.decode_block(br, &mut coeffs16, 3, 0, q_dc, q_dc, q_ac)?;
                if has_ac {
                    self.cdsp.transform16(&mut coeffs16);
                } else {
                    self.cdsp.transform16_dc(&mut coeffs16);
                }
            }
            let stride = buf.get_stride(0);
            let mut offset = buf.get_offset(0) + sstate.mb_x * 16 + sstate.mb_y * 16 * stride;
            let data = buf.get_data_mut().unwrap();
            let framebuf: &mut [u8] = data.as_mut_slice();

            for y in 0..4 {
                for x in 0..4 {
                    let mut coeffs: [i16; 16] = [0; 16];
                    let has_ac;
                    if (cur_cbp & 1) != 0 {
                        has_ac = self.coderead.decode_block(br, &mut coeffs, luma_set, 0, q_ac, q_ac, q_ac)?;
                    } else {
                        has_ac = false;
                    }
                    if is_16 {
                        coeffs[0] = coeffs16[x + y * 4];
                    }
                    if has_ac {
                        self.cdsp.transform(&mut coeffs);
                    } else {
                        self.cdsp.transform_dc(&mut coeffs);
                    }
                    self.cdsp.add_coeffs(framebuf, offset + x * 4, stride, &coeffs);
                    cur_cbp >>= 1;
                }
                offset += stride * 4;
            }
        }
        if is_16 {
            self.coderead.select_codes(false, sstate.q, sstate.set_idx, false);
        }
        let q_dc = RV34_QUANT_TAB[RV34_CHROMA_QUANT_DC[sstate.q as usize] as usize];
        let q_ac = RV34_QUANT_TAB[RV34_CHROMA_QUANT_AC[sstate.q as usize] as usize];
        let chroma_set = 1;
        for comp in 1..3 {
            let stride = buf.get_stride(comp);
            let mut offset = buf.get_offset(comp) + sstate.mb_x * 8 + sstate.mb_y * 8 * stride;
            let data = buf.get_data_mut().unwrap();
            let framebuf: &mut [u8] = data.as_mut_slice();
            for _ in 0..2 {
                for x in 0..2 {
                    let mut coeffs: [i16; 16] = [0; 16];
                    let has_ac;
                    if (cur_cbp & 1) != 0 {
                        has_ac = self.coderead.decode_block(br, &mut coeffs, chroma_set, 1, q_dc, q_ac, q_ac)?;
                    } else {
                        has_ac = false;
                    }
                    if has_ac {
                        self.cdsp.transform(&mut coeffs);
                    } else {
                        self.cdsp.transform_dc(&mut coeffs);
                    }
                    self.cdsp.add_coeffs(framebuf, offset + x * 4, stride, &coeffs);
                    cur_cbp >>= 1;
                }
                offset += stride * 4;
            }
        }
        Ok(())
    }
    fn fill_deblock_flags(&self, sstate: &SState, mb_pos: usize, mbinfo: &mut [RV34MBInfo]) {
        let mbt = mbinfo[mb_pos].mbtype;
        let mut hmvmask = 0;
        let mut vmvmask = 0;

        for y in 0..2 {
            for x in 0..2 {
                let shift = x * 2 + y * 8;
                if ((x > 0) || (sstate.mb_x > 0)) && self.mvi.mv_gt_3(sstate.mb_x, sstate.mb_y, x, y, false) {
                    vmvmask |= 0x11 << shift;
                }
                if ((y > 0) || sstate.has_top) && self.mvi.mv_gt_3(sstate.mb_x, sstate.mb_y, x, y, true) {
                    hmvmask |= 0x03 << shift;
                }
            }
        }
        if !sstate.has_top  { hmvmask &= !0x000F; }
        if sstate.mb_x == 0 { vmvmask &= !0x1111; }
        if self.is_rv30 {
            vmvmask |= (vmvmask & 0x4444) >> 1;
            hmvmask |= (hmvmask & 0x0F00) >> 4;
            if sstate.mb_x > 0 {
                mbinfo[mb_pos - 1].deblock |= (vmvmask & 0x1111) << 3;
            }
            if sstate.has_top {

                mbinfo[mb_pos - sstate.mb_w].deblock |= (hmvmask & 0xF) << 12;
            }
        }
        if mbt.is_intra_or_16() {
            mbinfo[mb_pos].deblock = 0xFFFF;
            mbinfo[mb_pos].cbp_c   = 0xFF;
        } else {
            mbinfo[mb_pos].deblock = (mbinfo[mb_pos].cbp as u16) | hmvmask | vmvmask;
            mbinfo[mb_pos].cbp_c   = (mbinfo[mb_pos].cbp >> 16) as u8;
        }
    }

    #[allow(clippy::cognitive_complexity)]
    pub fn parse_frame(&mut self, supp: &mut NADecoderSupport, src: &[u8], bd: &mut dyn RV34BitstreamDecoder) -> DecoderResult<(NABufferType, FrameType, u64)> {
        let mut slice_offs: Vec<usize> = Vec::new();
        parse_slice_offsets(src, &mut slice_offs)?;
        let ini_off = slice_offs.len() * 8 + 1;

        let mut br = BitReader::new(&src[ini_off..], BitReaderMode::BE);
        let hdr0 = decode_slice_header(&mut br, bd, 0, slice_offs.as_slice(), self.width, self.height)?;
        validate!((hdr0.width != 0) && (hdr0.height != 0));
        self.width  = hdr0.width;
        self.height = hdr0.height;
        let mb_w = (hdr0.width  + 15) >> 4;
        let mb_h = (hdr0.height + 15) >> 4;
        let mut mb_pos: usize = 0;
        let mut slice = hdr0;
        let mut slice_no: usize = 1;
        let is_intra = hdr0.ftype == FrameType::I;
        let mut skip_run: usize = 0;
        let mut imode = IntraModeState::new(mb_w);
        let mut q = hdr0.quant;

        let mut sstate = SState::new();
        let mut mbinfo: Vec<RV34MBInfo> = Vec::with_capacity(mb_w * mb_h);

        self.is_b = hdr0.ftype == FrameType::B;
        if hdr0.ftype != FrameType::B {
            self.last_ts = self.next_ts;
            self.next_ts = hdr0.pts;
            if self.last_ts > self.next_ts {
                self.base_ts += 1 << 13;
            }
        }
        match hdr0.ftype {
            FrameType::P => {
                if self.ipbs.get_lastref().is_none() {
                    return Err(DecoderError::MissingReference);
                }
            },
            FrameType::B => {
                if self.ipbs.get_lastref().is_none() {
                    return Err(DecoderError::MissingReference);
                }
                if self.ipbs.get_nextref().is_none() {
                    return Err(DecoderError::MissingReference);
                }
            },
            _ => {},
        };
        let ts_diff = (self.next_ts << 3).wrapping_sub(hdr0.pts << 3) >> 3;
        let ts = self.base_ts + (self.next_ts as u64) - (ts_diff as u64);
        sstate.trd = (self.next_ts << 3).wrapping_sub(self.last_ts << 3) >> 3;
        sstate.trb = (hdr0.pts << 3).wrapping_sub(self.last_ts << 3) >> 3;
        if sstate.trb != 0 {
            self.ratio1 = ((sstate.trb as u32)                         << 14) / (sstate.trd as u32);
            self.ratio2 = (((sstate.trd as u32) - (sstate.trb as u32)) << 14) / (sstate.trd as u32);
        } else {
            self.ratio1 = 1 << 14 >> 1;
            self.ratio2 = 1 << 14 >> 1;
        }
        //todo validate against ref frame

        let vinfo = NAVideoInfo::new(hdr0.width, hdr0.height, false, YUV420_FORMAT);
        let ret = supp.pool_u8.get_free();
        if ret.is_none() {
            return Err(DecoderError::AllocError);
        }
        let mut buf = ret.unwrap();
        if buf.get_info() != vinfo {
            self.ipbs.clear();
            supp.pool_u8.reset();
            supp.pool_u8.prealloc_video(vinfo, 4)?;
            let ret = supp.pool_u8.get_free();
            if ret.is_none() {
                return Err(DecoderError::AllocError);
            }
            buf = ret.unwrap();
        }

        sstate.q = q;
        sstate.has_top = false;
        sstate.mb_w = mb_w;
        sstate.mb_h = mb_h;
        sstate.set_idx = hdr0.set_idx;

        self.mvi.resize(mb_w, mb_h);
        for mb_y in 0..mb_h {
            sstate.mb_y = mb_y;
            sstate.has_left = false;
            for mb_x in 0..mb_w {
                sstate.mb_x = mb_x;
                if mb_pos == slice.end {
                    slice = decode_slice_header(&mut br, bd, slice_no, &slice_offs, self.width, self.height)?;
                    validate!(slice.fits(&hdr0));
                    q = slice.quant;
                    slice_no += 1;
                    imode.reset();
                    sstate.q        = q;
                    sstate.has_top  = false;
                    sstate.has_left = false;
                    sstate.set_idx  = slice.set_idx;
                }
                sstate.has_top = (mb_pos - slice.start) >= mb_w;
                sstate.has_tl  = sstate.has_top && (mb_x > 0) && (mb_pos > slice.start + mb_w);
                sstate.has_tr  = (mb_x < mb_w - 1) && (mb_pos - slice.start >= mb_w - 1);
                imode.set_mb_x(mb_x);
                let mbh = if is_intra {
                        let is_i16 = br.read_bool()?;
                        self.decode_mb_header_intra(bd, &mut br, is_i16, &mut imode, q, sstate.has_top, false)?
                    } else {
                        if skip_run == 0 {
                            let mbtype;
                            if self.is_rv30 {
                                mbtype = MBType::Invalid;
                            } else {
                                let mut hist = MBHist::new(hdr0.ftype);
                                if sstate.has_top  {
                                    hist.add(mbinfo[mb_pos - mb_w].mbtype);
                                    if sstate.has_tr   { hist.add(mbinfo[mb_pos - mb_w + 1].mbtype); }
                                }
                                if sstate.has_left { hist.add(mbinfo[mb_pos - 1].mbtype); }
                                if sstate.has_tl   { hist.add(mbinfo[mb_pos - mb_w - 1].mbtype); }
                                mbtype = hist.get_mbtype();
                            }
                            self.decode_mb_header_inter(bd, &mut br, hdr0.ftype, mbtype, &mut imode, q, sstate.has_top)?
                        } else {
                            skip_run -= 1;
                            MBInfo { mbtype: MBType::MBSkip, skip_run: 0, dquant: false }
                        }
                    };
                if !mbh.mbtype.is_intra() {
                    let mut mvs: [MV; 4] = [ZERO_MV; 4];
                    for i in 0..mbh.mbtype.get_num_mvs() {
                        mvs[i] = decode_mv(&mut br)?;
                    }
                    if !self.is_b {
                        self.mvi.set_mb(mb_x, mb_y, mbh.mbtype, &self.ref_mvi, &mvs, &sstate);
                    } else {
                        let (mv_f, mv_b) = bd.predict_b_mv(&sstate, &self.mvi, mbh.mbtype, &mvs, mbinfo.as_slice());
                        self.mvi.fill(mb_x, mb_y, true,  mv_f);
                        self.mvi.fill(mb_x, mb_y, false, mv_b);
                    }
                }
                let cbp;
                let is_16 = (mbh.mbtype == MBType::MBIntra16) || (mbh.mbtype == MBType::MBP16x16Mix);
                if mbh.mbtype == MBType::MBSkip {
                    cbp = 0;
                    if mbh.skip_run > 0 {
                        skip_run = mbh.skip_run;
                    }
                } else {
                    self.coderead.select_codes(mbh.mbtype.is_intra(), q, slice.set_idx, is_16);
                    if mbh.mbtype == MBType::MBP16x16Mix {
                        self.coderead.select_codes(true, q, slice.set_idx, true);
                    }
                    cbp = self.coderead.decode_cbp(&mut br)?;
                }
                sstate.cbp = cbp;
                if is_intra || mbh.mbtype.is_intra() {
                    sstate.q_dc = bd.quant_dc(true, q);
                    self.decode_mb_intra(&sstate, &imode, &mut buf, &mut br, is_16)?;
                } else {
                    sstate.q_dc = bd.quant_dc(false, q);
                    imode.fill_block(0);
                    self.decode_mb_inter(&sstate, &mbh, &mut buf, &mut br, is_16)?;
                }

                let mi = RV34MBInfo { cbp, q, mbtype: mbh.mbtype, deblock: 0, cbp_c: 0 };
                mbinfo.push(mi);
                if is_intra {
                    mbinfo[mb_pos].deblock = 0xFFFF;
                    mbinfo[mb_pos].cbp_c   = 0xFF;
                } else {
                    self.fill_deblock_flags(&sstate, mb_pos, &mut mbinfo);
                }
                sstate.has_left = true;
                mb_pos += 1;
            }
            if hdr0.deblock && (mb_y >= 1) {
                self.dsp.loop_filter(&mut buf, hdr0.ftype, &mbinfo, mb_w, mb_h, mb_y - 1);
            }
            imode.update();
        }
        if hdr0.deblock {
            self.dsp.loop_filter(&mut buf, hdr0.ftype, &mbinfo, mb_w, mb_h, mb_h - 1);
        }
        if !self.is_b {
            self.ipbs.add_frame(buf.clone());
            mem::swap(&mut self.mvi, &mut self.ref_mvi);
            mem::swap(&mut self.mbinfo, &mut mbinfo);
        }

        Ok((NABufferType::Video(buf), hdr0.ftype, ts))
    }
    pub fn flush(&mut self) {
        self.ipbs.clear();
    }
}
