use nihav_core::frame::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use nihav_codec_support::data::GenericCache;
use super::super::vp78::{PredMode, MVSplitMode, SubMVRef};
use super::super::vp78data::*;
use super::super::vp78dsp::*;
use super::super::vp7data::*;
use super::super::vp7dsp::*;

#[derive(Clone,Copy)]
pub enum MBType {
    Intra(PredMode, PredMode),
    Intra4x4([PredMode; 16], [u8; 16], PredMode),
    InterNoMV(bool, [u8; 4]),
    InterNearest(bool, [u8; 4]),
    InterNear(bool, [u8; 4]),
    InterMV(bool, [u8; 4], MV),
    InterSplitMV(bool, [u8; 4], MVSplitMode, [SubMVRef; 16], [MV; 16]),
}

impl MBType {
    pub fn is_intra(&self) -> bool {
        matches!(*self, MBType::Intra(_, _) | MBType::Intra4x4(_, _, _))
    }
    pub fn get_last(&self) -> bool {
        match *self {
            MBType::InterNoMV(last, _) |
            MBType::InterNearest(last, _) |
            MBType::InterNear(last, _) |
            MBType::InterMV(last, _, _) |
            MBType::InterSplitMV(last, _, _, _, _) => last,
            _ => false,
        }
    }
}

impl Default for MBType {
    fn default() -> Self { MBType::Intra(PredMode::DCPred, PredMode::DCPred) }
}

pub fn get_block_difference(dst: &mut [i16; 16], src1: &[u8; 16], src2: &[u8; 16]) {
    for (dst, (&src1, &src2)) in dst.iter_mut().zip(src1.iter().zip(src2.iter())) {
        *dst = i16::from(src1) - i16::from(src2);
    }
}
pub fn get_difference_dist(old: &[u8; 16], new: &[u8; 16], diff: &[i16; 16]) -> u32 {
    let mut dist = 0;
    for ((&old, &new), &diff) in old.iter().zip(new.iter()).zip(diff.iter()) {
        let nval = (i16::from(new) + diff).max(0).min(255);
        let oval = i16::from(old);
        dist += (i32::from(nval - oval) * i32::from(nval - oval)) as u32;
    }
    dist
}

pub fn requant_y2_dc(val: &mut i16, q: usize) {
    *val = *val / Y2_DC_QUANTS[q] * Y2_DC_QUANTS[q];
}

pub trait DCTBlock {
    fn has_nz(&self) -> bool;
    fn fdct(&mut self);
    fn idct(&mut self);
    fn requant_y(&mut self, q: usize);
    fn quant(&mut self, q: usize, ctype: usize);
    fn dequant(&mut self, q: usize, ctype: usize);
}

impl DCTBlock for [i16; 16] {
    fn has_nz(&self) -> bool {
        for &el in self.iter() {
            if el != 0 {
                return true;
            }
        }
        false
    }
    #[allow(clippy::erasing_op)]
    #[allow(clippy::identity_op)]
    fn fdct(&mut self) {
        let mut tmp = [0i16; 16];
        for i in 0..4 {
            let s0 = i32::from(self[i + 4 * 0]);
            let s1 = i32::from(self[i + 4 * 1]);
            let s2 = i32::from(self[i + 4 * 2]);
            let s3 = i32::from(self[i + 4 * 3]);

            let t0 = (s0 + s3).wrapping_mul(23170) + 0x2000;
            let t1 = (s1 + s2).wrapping_mul(23170);
            let t2 = s0 - s3;
            let t3 = s1 - s2;
            let t4 = t2.wrapping_mul(30274) + t3.wrapping_mul(12540) + 0x2000;
            let t5 = t2.wrapping_mul(12540) - t3.wrapping_mul(30274) + 0x2000;

            tmp[i + 0 * 4] = ((t0 + t1) >> 14) as i16;
            tmp[i + 1 * 4] = ( t4       >> 14) as i16;
            tmp[i + 2 * 4] = ((t0 - t1) >> 14) as i16;
            tmp[i + 3 * 4] = ( t5       >> 14) as i16;
        }
        for (src, dst) in tmp.chunks(4).zip(self.chunks_mut(4)) {
            let s0 = i32::from(src[0]);
            let s1 = i32::from(src[1]);
            let s2 = i32::from(src[2]);
            let s3 = i32::from(src[3]);

            let t0 = (s0 + s3).wrapping_mul(23170) + 0x8000;
            let t1 = (s1 + s2).wrapping_mul(23170);
            let t2 = s0 - s3;
            let t3 = s1 - s2;
            let t4 = t2.wrapping_mul(30274) + t3.wrapping_mul(12540) + 0x8000;
            let t5 = t2.wrapping_mul(12540) - t3.wrapping_mul(30274) + 0x8000;

            dst[0] = ((t0 + t1) >> 16) as i16;
            dst[1] = ( t4       >> 16) as i16;
            dst[2] = ((t0 - t1) >> 16) as i16;
            dst[3] = ( t5       >> 16) as i16;
        }
    }
    fn idct(&mut self) { idct4x4(self) }
    fn requant_y(&mut self, q: usize) {
        self[0] = self[0] / Y_DC_QUANTS[q] * Y_DC_QUANTS[q];
        for el in self[1..].iter_mut() {
            *el = *el / Y_AC_QUANTS[q] * Y_AC_QUANTS[q];
        }
    }
    fn quant(&mut self, q: usize, ctype: usize) {
        let (q_dc, q_ac) = match ctype {
                0 | 3 => (Y_DC_QUANTS[q],  Y_AC_QUANTS[q]),
                2     => (UV_DC_QUANTS[q], UV_AC_QUANTS[q]),
                _     => (Y2_DC_QUANTS[q], Y2_AC_QUANTS[q]),
            };
        self[0] /= q_dc;
        for el in self[1..].iter_mut() {
            *el /= q_ac;
        }
    }
    fn dequant(&mut self, q: usize, ctype: usize) {
        let (q_dc, q_ac) = match ctype {
                0 | 3 => (Y_DC_QUANTS[q],  Y_AC_QUANTS[q]),
                2     => (UV_DC_QUANTS[q], UV_AC_QUANTS[q]),
                _     => (Y2_DC_QUANTS[q], Y2_AC_QUANTS[q]),
            };
        self[0] *= q_dc;
        for el in self[1..].iter_mut() {
            *el *= q_ac;
        }
    }
}

pub trait IPredBlock16 {
    fn ipred16(&mut self, stride: usize, mode: PredMode, ipred: &IPredContext);
}
pub trait IPredBlock8 {
    fn ipred8 (&mut self, stride: usize, mode: PredMode, ipred: &IPredContext);
}
pub trait IPredBlock4 {
    fn ipred4 (&mut self, stride: usize, mode: PredMode, ipred: &IPredContext);
}

impl IPredBlock16 for [u8; 256] {
    fn ipred16(&mut self, stride: usize, mode: PredMode, ipred: &IPredContext) {
        match mode {
            PredMode::DCPred => IPred16x16::ipred_dc(self, 0, stride, ipred),
            PredMode::HPred  => IPred16x16::ipred_h (self, 0, stride, ipred),
            PredMode::VPred  => IPred16x16::ipred_v (self, 0, stride, ipred),
            PredMode::TMPred => IPred16x16::ipred_tm(self, 0, stride, ipred),
            _ => {},
        }
    }
}
impl IPredBlock8 for [u8; 64] {
    fn ipred8(&mut self, stride: usize, mode: PredMode, ipred: &IPredContext) {
        match mode {
            PredMode::DCPred => IPred8x8::ipred_dc(self, 0, stride, ipred),
            PredMode::HPred  => IPred8x8::ipred_h (self, 0, stride, ipred),
            PredMode::VPred  => IPred8x8::ipred_v (self, 0, stride, ipred),
            PredMode::TMPred => IPred8x8::ipred_tm(self, 0, stride, ipred),
            _ => {},
        }
    }
}
impl IPredBlock4 for &mut [u8] {
    fn ipred4(&mut self, stride: usize, mode: PredMode, ipred: &IPredContext) {
        match mode {
            PredMode::DCPred => IPred4x4::ipred_dc(self, 0, stride, ipred),
            PredMode::HPred  => IPred4x4::ipred_he(self, 0, stride, ipred),
            PredMode::VPred  => IPred4x4::ipred_ve(self, 0, stride, ipred),
            PredMode::TMPred => IPred4x4::ipred_tm(self, 0, stride, ipred),
            PredMode::LDPred => IPred4x4::ipred_ld(self, 0, stride, ipred),
            PredMode::RDPred => IPred4x4::ipred_rd(self, 0, stride, ipred),
            PredMode::VRPred => IPred4x4::ipred_vr(self, 0, stride, ipred),
            PredMode::VLPred => IPred4x4::ipred_vl(self, 0, stride, ipred),
            PredMode::HDPred => IPred4x4::ipred_hd(self, 0, stride, ipred),
            PredMode::HUPred => IPred4x4::ipred_hu(self, 0, stride, ipred),
            _ => {},
        }
    }
}
impl IPredBlock4 for [u8; 16] {
    fn ipred4(&mut self, stride: usize, mode: PredMode, ipred: &IPredContext) {
        (self as &mut [u8]).ipred4(stride, mode, ipred);
    }
}

pub struct LumaIterator<'a> {
    luma:   &'a [u8; 256],
    blkno:  usize,
}

impl<'a> Iterator for LumaIterator<'a> {
    type Item = [u8; 16];
    fn next(&mut self) -> Option<Self::Item> {
        if self.blkno < 16 {
            let mut blk = [0; 16];
            let off = (self.blkno & 3) * 4 + (self.blkno >> 2) * 16 * 4;
            for (dst, src) in blk.chunks_exact_mut(4).zip(self.luma[off..].chunks(16)) {
                dst.copy_from_slice(&src[..4]);
            }
            self.blkno += 1;
            Some(blk)
        } else {
            None
        }
    }
}

pub struct ChromaIterator<'a> {
    chroma: &'a [u8; 64],
    blkno:  usize,
}

impl<'a> Iterator for ChromaIterator<'a> {
    type Item = [u8; 16];
    fn next(&mut self) -> Option<Self::Item> {
        if self.blkno < 4 {
            let mut blk = [0; 16];
            let off = (self.blkno & 1) * 4 + (self.blkno >> 1) * 8 * 4;
            for (dst, src) in blk.chunks_exact_mut(4).zip(self.chroma[off..].chunks(8)) {
                dst.copy_from_slice(&src[..4]);
            }
            self.blkno += 1;
            Some(blk)
        } else {
            None
        }
    }
}

pub struct SrcBlock {
    pub luma:   [u8; 256],
    pub chroma: [[u8; 64]; 2],
}

impl Default for SrcBlock {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

impl SrcBlock {
    pub fn new() -> Self { Self::default() }
    pub fn is_flat(&self) -> bool {
        let y0 = self.luma[0];
        for &el in self.luma[1..].iter() {
            if el != y0 {
                return false;
            }
        }
        true
    }
    pub fn apply_ipred_luma(&self, mode: PredMode, ipred: &IPredContext, dst: &mut Residue) {
        let mut tmp = [0; 256];
        tmp.ipred16(16, mode, ipred);
        dst.set_luma_from_diff(&self.luma, &tmp);
    }
    pub fn fill_ipred_luma(&mut self, mode: PredMode, ipred: &IPredContext) {
        self.luma.ipred16(16, mode, ipred);
    }
    pub fn apply_ipred_chroma(&self, mode: PredMode, ipred_u: &IPredContext, ipred_v: &IPredContext, dst: &mut Residue) {
        let mut tmp = [[0u8; 64]; 2];
        tmp[0].ipred8(8, mode, ipred_u);
        tmp[1].ipred8(8, mode, ipred_v);
        dst.set_chroma_from_diff(&self.chroma, &tmp);
    }
    pub fn fill_ipred_chroma(&mut self, mode: PredMode, ipred_u: &IPredContext, ipred_v: &IPredContext) {
        self.chroma[0].ipred8(8, mode, ipred_u);
        self.chroma[1].ipred8(8, mode, ipred_v);
    }

    pub fn luma_blocks(&self) -> LumaIterator {
        LumaIterator{ luma: &self.luma, blkno: 0 }
    }
    pub fn chroma_blocks(&self, plane: usize) -> ChromaIterator {
        ChromaIterator{ chroma: &self.chroma[plane], blkno: 0 }
    }
}

#[derive(Clone)]
pub struct Residue {
    pub luma:   [[i16; 16]; 16],
    pub dcs:    [i16; 16],
    pub chroma: [[[i16; 16]; 4]; 2],
    pub has_dc: bool,
    pub q:      u8,
}

impl Default for Residue {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

impl Residue {
    pub fn new() -> Self { Self::default() }
    pub fn reset(&mut self) {
        self.has_dc = false;
        self.q = 242;
    }
    pub fn add_residue(&mut self, dst: &mut SrcBlock) {
        self.dequant();
        self.idct();

        for (dst, src) in dst.luma.chunks_mut(16 * 4).zip(self.luma.chunks(4)) {
            for (x, blk) in src.iter().enumerate() {
                for (drow, srow) in dst[x * 4..].chunks_mut(16).zip(blk.chunks(4)) {
                    for (del, &sel) in drow.iter_mut().zip(srow.iter()) {
                        *del = (i16::from(*del) + sel).max(0).min(255) as u8;
                    }
                }
            }
        }
        for (dchroma, schroma) in dst.chroma.iter_mut().zip(self.chroma.iter()) {
            for (dst, src) in dchroma.chunks_mut(8 * 4).zip(schroma.chunks(2)) {
                for (x, blk) in src.iter().enumerate() {
                    for (drow, srow) in dst[x * 4..].chunks_mut(8).zip(blk.chunks(4)) {
                        for (del, &sel) in drow.iter_mut().zip(srow.iter()) {
                            *del = (i16::from(*del) + sel).max(0).min(255) as u8;
                        }
                    }
                }
            }
        }
    }
    pub fn add_residue_chroma(&mut self, dst: &mut SrcBlock) {
        let q = self.q as usize;
        for (dchroma, schroma) in dst.chroma.iter_mut().zip(self.chroma.iter_mut()) {
            for (dst, src) in dchroma.chunks_mut(8 * 4).zip(schroma.chunks_mut(2)) {
                for (x, blk) in src.iter_mut().enumerate() {
                    blk[0] *= UV_DC_QUANTS[q];
                    for el in blk[1..].iter_mut() {
                        if *el != 0 {
                            *el *= UV_AC_QUANTS[q];
                        }
                    }
                    blk.idct();
                    for (drow, srow) in dst[x * 4..].chunks_mut(8).zip(blk.chunks(4)) {
                        for (del, &sel) in drow.iter_mut().zip(srow.iter()) {
                            *del = (i16::from(*del) + sel).max(0).min(255) as u8;
                        }
                    }
                }
            }
        }
    }
    pub fn set_luma_from_diff(&mut self, blk1: &[u8; 256], blk2: &[u8; 256]) {
        for (dst, (src1, src2)) in self.luma.chunks_mut(4).zip(blk1.chunks(16 * 4).zip(blk2.chunks(16 * 4))) {
            for (x, blk) in dst.iter_mut().enumerate() {
                for (dst, (row1, row2)) in blk.chunks_mut(4).zip(src1[x * 4..].chunks(16).zip(src2[x * 4..].chunks(16))) {
                    for (dst, (&a, &b)) in dst.iter_mut().zip(row1.iter().zip(row2.iter())) {
                        *dst = i16::from(a) - i16::from(b);
                    }
                }
            }
        }
    }
    pub fn set_chroma_from_diff(&mut self, blk1: &[[u8; 64]; 2], blk2: &[[u8; 64]; 2]) {
        for (chroma, (src1, src2)) in self.chroma.iter_mut().zip(blk1.iter().zip(blk2.iter())) {
            for (dst, (src1, src2)) in chroma.chunks_mut(2).zip(src1.chunks(8 * 4).zip(src2.chunks(8 * 4))) {
                for (x, blk) in dst.iter_mut().enumerate() {
                    for (dst, (row1, row2)) in blk.chunks_mut(4).zip(src1[x * 4..].chunks(8).zip(src2[x * 4..].chunks(8))) {
                        for (dst, (&a, &b)) in dst.iter_mut().zip(row1.iter().zip(row2.iter())) {
                            *dst = i16::from(a) - i16::from(b);
                        }
                    }
                }
            }
        }
    }
    pub fn fdct(&mut self) {
        self.fdct_luma();
        self.fdct_chroma();
    }
    pub fn fdct_luma(&mut self) {
        for blk in self.luma.iter_mut() {
            blk.fdct();
        }
    }
    pub fn fdct_chroma(&mut self) {
        for chroma in self.chroma.iter_mut() {
            for blk in chroma.iter_mut() {
                blk.fdct();
            }
        }
    }
    pub fn fdct_dc_block(&mut self) {
        for (dc, blk) in self.dcs.iter_mut().zip(self.luma.iter_mut()) {
            *dc = blk[0];
            blk[0] = 0;
        }
        self.dcs.fdct();
        self.has_dc = true;
    }
    pub fn idct(&mut self) {
        self.idct_luma();
        self.idct_chroma();
    }
    pub fn idct_luma(&mut self) {
        if self.has_dc {
            self.dcs.idct();
            for (&dc, blk) in self.dcs.iter().zip(self.luma.iter_mut()) {
                blk[0] = dc;
            }
        }
        for blk in self.luma.iter_mut() {
            blk.idct();
        }
    }
    pub fn idct_chroma(&mut self) {
        for chroma in self.chroma.iter_mut() {
            for blk in chroma.iter_mut() {
                blk.idct();
            }
        }
    }
    pub fn quant(&mut self, q: usize) {
        self.quant_luma(q);
        self.quant_chroma(q);
        self.q = q as u8;
    }
    pub fn quant_luma(&mut self, q: usize) {
        if self.has_dc {
            self.dcs[0] /= Y2_DC_QUANTS[q];
            for el in self.dcs[1..].iter_mut() {
                if *el != 0 {
                    *el /= Y2_AC_QUANTS[q];
                }
            }
        }
        for blk in self.luma.iter_mut() {
            blk[0] /= Y_DC_QUANTS[q];
            for el in blk[1..].iter_mut() {
                if *el != 0 {
                    *el /= Y_AC_QUANTS[q];
                }
            }
        }
        self.q = q as u8;
    }
    pub fn quant_chroma(&mut self, q: usize) {
        for chroma in self.chroma.iter_mut() {
            for blk in chroma.iter_mut() {
                blk[0] /= UV_DC_QUANTS[q];
                for el in blk[1..].iter_mut() {
                    if *el != 0 {
                        *el /= UV_AC_QUANTS[q];
                    }
                }
            }
        }
        self.q = q as u8;
    }
    pub fn dequant(&mut self) {
        self.dequant_luma();
        self.dequant_chroma();
    }
    pub fn dequant_luma(&mut self) {
        let q = self.q as usize;
        if self.has_dc {
            self.dcs[0] *= Y2_DC_QUANTS[q];
            for el in self.dcs[1..].iter_mut() {
                if *el != 0 {
                    *el *= Y2_AC_QUANTS[q];
                }
            }
        }
        for blk in self.luma.iter_mut() {
            blk[0] *= Y_DC_QUANTS[q];
            for el in blk[1..].iter_mut() {
                if *el != 0 {
                    *el *= Y_AC_QUANTS[q];
                }
            }
        }
    }
    pub fn dequant_chroma(&mut self) {
        let q = self.q as usize;
        for chroma in self.chroma.iter_mut() {
            for blk in chroma.iter_mut() {
                blk[0] *= UV_DC_QUANTS[q];
                for el in blk[1..].iter_mut() {
                    if *el != 0 {
                        *el *= UV_AC_QUANTS[q];
                    }
                }
            }
        }
    }
}

pub fn load_blocks(src: &NAVideoBuffer<u8>, sblocks: &mut Vec<SrcBlock>) {
    let data = src.get_data();
    let y = &data[src.get_offset(0)..];
    let u = &data[src.get_offset(1)..];
    let v = &data[src.get_offset(2)..];
    let ystride = src.get_stride(0);
    let ustride = src.get_stride(1);
    let vstride = src.get_stride(2);
    let (width, height) = src.get_dimensions(0);

    sblocks.clear();
    for (ystrip, (ustrip, vstrip)) in y.chunks(ystride * 16).take((height + 15) / 16).zip(u.chunks(ustride * 8).zip(v.chunks(vstride * 8))) {
        for x in (0..width).step_by(16) {
            let mut sblk = SrcBlock::default();

            for (dst, src) in sblk.luma.chunks_mut(16).zip(ystrip[x..].chunks(ystride)) {
                dst.copy_from_slice(&src[..16]);
            }
            for (dst, src) in sblk.chroma[0].chunks_mut(8).zip(ustrip[x / 2..].chunks(ustride)) {
                dst.copy_from_slice(&src[..8]);
            }
            for (dst, src) in sblk.chroma[1].chunks_mut(8).zip(vstrip[x / 2..].chunks(vstride)) {
                dst.copy_from_slice(&src[..8]);
            }
            sblocks.push(sblk);
        }
    }
}

pub struct YModePred {
    pub cache:  GenericCache<PredMode>,
}

impl YModePred {
    fn resize(&mut self, mb_w: usize) {
        self.cache = GenericCache::new(4, mb_w * 4 + 1, PredMode::DCPred);
    }
    pub fn set_mode(&mut self, mb_x: usize, mode: PredMode) {
        for row in self.cache.data[self.cache.xpos + mb_x * 4..].chunks_mut(self.cache.stride).take(4) {
            for el in row[..4].iter_mut() {
                *el = mode.to_b_mode();
            }
        }
    }
    pub fn set_modes4x4(&mut self, mb_x: usize, imodes: &[PredMode; 16], ctx: &mut [u8; 16]) {
        let mut off = self.cache.xpos + mb_x * 4;
        for y in 0..4 {
            for x in 0..4 {
                let top_idx = self.cache.data[off + x - self.cache.stride].to_b_index();
                let left_idx = self.cache.data[off + x - 1].to_b_index();
                self.cache.data[off + x] = imodes[x + y * 4];
                ctx[x + y * 4] = ((top_idx * 10) + left_idx) as u8;
            }
            off += self.cache.stride;
        }
    }
}

impl Default for YModePred {
    fn default() -> Self {
        Self {
            cache: GenericCache::new(0, 0, PredMode::DCPred)
        }
    }
}

#[derive(Default)]
pub struct BlockPCtx {
    pub nz_y2:      u8,
    pub nz_y_top:   [bool; 4],
    pub nz_y_left:  [bool; 4],
    pub nz_c_top:   [[bool; 2]; 2],
    pub nz_c_left:  [[bool; 2]; 2],
}

#[derive(Default)]
pub struct PredContext {
    pub mb_w:       usize,
    pub mb_h:       usize,

    pub top_line_y: Vec<u8>,
    pub top_line_u: Vec<u8>,
    pub top_line_v: Vec<u8>,
    pub tl_y:       u8,
    pub tl_u:       u8,
    pub tl_v:       u8,

    pub left_y:     [u8; 16],
    pub left_u:     [u8; 16],
    pub left_v:     [u8; 16],

    pub dc_last:    [i16; 2],
    pub dc_count:   [usize; 2],
    dc_last_saved:  [i16; 2],
    dc_count_saved: [usize; 2],
    pub nz_y2_top:  Vec<bool>,
    pub nz_y2_left: bool,
    pub nz_y_top:   Vec<bool>,
    pub nz_y_left:  [bool; 4],
    pub nz_c_top:   [Vec<bool>; 2],
    pub nz_c_left:  [[bool; 2]; 2],

    pub ymodes:     YModePred,

    pub mvs:        Vec<MV>,
    pub mv_stride:  usize,
    pub version:    u8,
}

impl PredContext {
    pub fn new() -> Self { Self::default() }
    pub fn resize(&mut self, mb_w: usize, mb_h: usize) {
        self.mb_w = mb_w;
        self.mb_h = mb_h;

        self.top_line_y.resize(mb_w * 16 + 1, 0);
        self.top_line_u.resize(mb_w * 8 + 1, 0);
        self.top_line_v.resize(mb_w * 8 + 1, 0);

        self.nz_y2_top.resize(mb_w, false);
        self.nz_y_top.resize(mb_w * 4, false);
        self.nz_c_top[0].resize(mb_w * 2, false);
        self.nz_c_top[1].resize(mb_w * 2, false);

        self.ymodes.resize(mb_w);

        self.mv_stride = mb_w * 4;
        self.mvs.resize(self.mv_stride * mb_h * 4, ZERO_MV);
    }

    pub fn reset(&mut self) {
        for el in self.top_line_y.iter_mut() { *el = 0x80; }
        for el in self.top_line_u.iter_mut() { *el = 0x80; }
        for el in self.top_line_v.iter_mut() { *el = 0x80; }
        self.left_y = [0x80; 16];
        self.left_u = [0x80; 16];
        self.left_v = [0x80; 16];
        self.tl_y = 0x80;
        self.tl_u = 0x80;
        self.tl_v = 0x80;

        for el in self.nz_y_top.iter_mut() { *el = false; }
        self.nz_y_left = [false; 4];
        for el in self.nz_y2_top.iter_mut() { *el = false; }
        self.nz_y2_left = false;
        for el in self.nz_c_top[0].iter_mut() { *el = false; }
        for el in self.nz_c_top[1].iter_mut() { *el = false; }
        self.nz_c_left = [[false; 2]; 2];

        self.ymodes.cache.reset();

        for mv in self.mvs.iter_mut() { *mv = ZERO_MV; }
    }
    pub fn reset_intra(&mut self) {
        self.dc_last  = [0; 2];
        self.dc_count = [0; 2];
        self.dc_last_saved  = [0; 2];
        self.dc_count_saved = [0; 2];
    }
    pub fn save_dc_pred(&mut self) {
        self.dc_last_saved  = self.dc_last;
        self.dc_count_saved = self.dc_count;
    }
    #[allow(dead_code)]
    pub fn restore_dc_pred(&mut self) {
        self.dc_last  = self.dc_last_saved;
        self.dc_count = self.dc_count_saved;
    }
    pub fn update_mb_row(&mut self) {
        self.left_y = [0x80; 16];
        self.left_u = [0x80; 16];
        self.left_v = [0x80; 16];
        self.tl_y = 0x80;
        self.tl_u = 0x80;
        self.tl_v = 0x80;
        self.ymodes.cache.update_row();
    }
    pub fn update_mb(&mut self, sblk: &SrcBlock, mb_x: usize) {
        for (dst, src) in self.left_y.iter_mut().zip(sblk.luma.chunks_exact(16)) {
            *dst = src[15];
        }
        self.tl_y = self.top_line_y[mb_x * 16 + 16];
        self.top_line_y[mb_x * 16 + 1..][..16].copy_from_slice(&sblk.luma[15 * 16..]);

        for (dst, src) in self.left_u.iter_mut().zip(sblk.chroma[0].chunks_exact(8)) {
            *dst = src[7];
        }
        self.tl_u = self.top_line_u[mb_x * 8 + 8];
        self.top_line_u[mb_x * 8 + 1..][..8].copy_from_slice(&sblk.chroma[0][7 * 8..]);

        for (dst, src) in self.left_v.iter_mut().zip(sblk.chroma[1].chunks_exact(8)) {
            *dst = src[7];
        }
        self.tl_v = self.top_line_v[mb_x * 8 + 8];
        self.top_line_v[mb_x * 8 + 1..][..8].copy_from_slice(&sblk.chroma[1][7 * 8..]);
    }
    pub fn fill_ipred(&mut self, plane: usize, mb_x: usize, ipred: &mut IPredContext) {
        match plane {
            0 => {
                if ipred.has_top {
                    ipred.top.copy_from_slice(&self.top_line_y[mb_x * 16 + 1..][..16]);
                    ipred.tl = self.tl_y;
                }
                ipred.left.copy_from_slice(&self.left_y);
                ipred.has_left = mb_x > 0;
            },
            1 => {
                if ipred.has_top {
                    ipred.top[..8].copy_from_slice(&self.top_line_u[mb_x * 8 + 1..][..8]);
                    ipred.tl = self.tl_u;
                }
                ipred.left.copy_from_slice(&self.left_u);
                ipred.has_left = mb_x > 0;
            },
            _ => {
                if ipred.has_top {
                    ipred.top[..8].copy_from_slice(&self.top_line_v[mb_x * 8 + 1..][..8]);
                    ipred.tl = self.tl_v;
                }
                ipred.left.copy_from_slice(&self.left_v);
                ipred.has_left = mb_x > 0;
            },
        }
    }
    pub fn get_ipred_tr(&self, mb_x: usize) -> [u8; 4] {
        if mb_x < self.mb_w - 1 {
            let mut tr = [0; 4];
            tr.copy_from_slice(&self.top_line_y[mb_x * 16 + 1 + 16..][..4]);
            tr
        } else {
            [0x80; 4]
        }
    }
    pub fn fill_pctx(&self, mb_x: usize, pctx: &mut BlockPCtx) {
        pctx.nz_y2     = (self.nz_y2_left as u8) + (self.nz_y2_top[mb_x] as u8);
        pctx.nz_y_left = self.nz_y_left;
        pctx.nz_y_top.copy_from_slice(&self.nz_y_top[mb_x * 4..][..4]);
        pctx.nz_c_left = self.nz_c_left;
        pctx.nz_c_top  = [[self.nz_c_top[0][mb_x * 2], self.nz_c_top[0][mb_x * 2 + 1]],
                          [self.nz_c_top[1][mb_x * 2], self.nz_c_top[1][mb_x * 2 + 1]]];
    }
    pub fn set_nz(&mut self, mb_x: usize, blk: &Residue) {
        if blk.has_dc {
            let has_nz = blk.dcs.has_nz();
            self.nz_y2_left = has_nz;
            self.nz_y2_top[mb_x] = has_nz;
        }
        for (y, blk_row) in blk.luma.chunks(4).enumerate() {
            for (x, blk) in blk_row.iter().enumerate() {
                let has_nz = blk.has_nz();
                self.nz_y_left[y] = has_nz;
                self.nz_y_top[mb_x * 4 + x] = has_nz;
            }
        }
        for (c, chroma) in blk.chroma.iter().enumerate() {
            for (y, blk_row) in chroma.chunks(2).enumerate() {
                for (x, blk) in blk_row.iter().enumerate() {
                    let has_nz = blk.has_nz();
                    self.nz_c_left[c][y] = has_nz;
                    self.nz_c_top[c][mb_x * 2 + x] = has_nz;
                }
            }
        }
    }

    pub fn get_y2_dc_pred(&self, last: bool) -> i16 {
        let ref_id = !last as usize;
        if self.dc_count[ref_id] > 3 {
            self.dc_last[ref_id]
        } else {
            0
        }
    }
    pub fn predict_y2_dc(&mut self, dc: &mut i16, last: bool) {
        let ref_id = !last as usize;
        let pdc = self.dc_last[ref_id];
        let orig_dc = *dc;

        if self.dc_count[ref_id] > 3 {
            *dc -= pdc;
        }

        if (pdc == 0) || (orig_dc == 0) || ((pdc ^ orig_dc) < 0) {
            self.dc_count[ref_id] = 0;
        } else if pdc == orig_dc {
            self.dc_count[ref_id] += 1;
        }
        self.dc_last[ref_id] = orig_dc;
    }

    pub fn fill_mv(&mut self, mb_x: usize, mb_y: usize, mv: MV) {
        let mut iidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        for _ in 0..4 {
            for x in 0..4 {
                self.mvs[iidx + x] = mv;
            }
            iidx += self.mb_w * 4;
        }
    }
    pub fn find_mv_pred(&self, mb_x: usize, mb_y: usize) -> ([u8; 4], MV, MV, MV) {
        let mut nearest_mv = ZERO_MV;
        let mut near_mv = ZERO_MV;

        let mut ct: [u8; 4] = [0; 4];

        let start = if self.version == 0 { 1 } else { 0 };
        let mvwrap = (self.mb_w as isize) + 1;
        for (yoff, xoff, weight, blk_no) in CAND_POS.iter() {
            let cx = (mb_x as isize) + (*xoff as isize);
            let cy = (mb_y as isize) + (*yoff as isize);
            let mvpos = cx + cy * mvwrap;
            if (mvpos < start) || ((mvpos % mvwrap) == (mvwrap - 1)) {
                ct[0] += weight;
                continue;
            }
            let cx = (mvpos % mvwrap) as usize;
            let cy = (mvpos / mvwrap) as usize;
            let bx = (*blk_no as usize) & 3;
            let by = (*blk_no as usize) >> 2;
            let blk_pos = cx * 4 + bx + (cy * 4 + by) * self.mv_stride;
            let mv = self.mvs[blk_pos];
            if mv == ZERO_MV {
                ct[0] += weight;
                continue;
            }
            let idx;
            if (nearest_mv == ZERO_MV) || (nearest_mv == mv) {
                nearest_mv = mv;
                idx = 1;
            } else if near_mv == ZERO_MV {
                near_mv = mv;
                idx = 2;
            } else {
                idx = if mv == near_mv { 2 } else { 3 };
            }
            ct[idx] += weight;
        }
        let pred_mv = if ct[1] > ct[2] {
                if ct[1] >= ct[0] { nearest_mv } else { ZERO_MV }
            } else {
                if ct[2] >= ct[0] { near_mv } else { ZERO_MV }
            };

        let mvprobs = [INTER_MODE_PROBS[ct[0] as usize][0],
                       INTER_MODE_PROBS[ct[1] as usize][1],
                       INTER_MODE_PROBS[ct[2] as usize][2],
                       INTER_MODE_PROBS[ct[2] as usize][3]];

        (mvprobs, nearest_mv, near_mv, pred_mv)
    }
}
