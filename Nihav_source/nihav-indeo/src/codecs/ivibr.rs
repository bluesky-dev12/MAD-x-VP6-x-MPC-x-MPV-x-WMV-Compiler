use std::mem;
use nihav_core::io::bitreader::*;
//use io::intcode::*;
use nihav_core::codecs::*;
use nihav_core::frame::NABufferRef;
use super::ivi::*;
use super::ividsp::*;

pub fn scale_mv(val: i32, scale: u8) -> i32 {
    (val + (if val > 0 { 1 } else { 0 }) + i32::from(scale) - 1) >> scale
}

#[derive(Clone,Copy)]
pub struct IVICodebook {
    len:    usize,
    bits:   [u8; 16],
    offs:   [u32; 16],
}

impl IVICodebook {
    pub fn init(&self) -> Self {
        let mut cb = *self;
        let mut base: u32 = 0;
        for i in 0..cb.len {
            cb.offs[i] = base;
            base += 1 << cb.bits[i];
        }
        cb
    }
}

pub const IVI_CB_ZERO: IVICodebook = IVICodebook { len: 0, bits: [0; 16], offs: [0; 16] };

const IVI_REV0: [u32; 1] = [0];
const IVI_REV1: [u32; 2] = [0, 1];
const IVI_REV2: [u32; 4] = [0, 2, 1, 3];
const IVI_REV3: [u32; 8] = [0, 4, 2, 6, 1, 5, 3, 7];
const IVI_REV4: [u32; 16] = [ 0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15];
const IVI_REV5: [u32; 32] = [ 0, 16, 8, 24, 4, 20, 12, 28, 2, 18, 10, 26, 6, 22, 14, 30, 1, 17, 9, 25, 5, 21, 13, 29, 3, 19, 11, 27, 7, 23, 15, 31];
const IVI_REV6: [u32; 64] = [ 0, 32, 16, 48, 8, 40, 24, 56, 4, 36, 20, 52, 12, 44, 28, 60, 2, 34, 18, 50, 10, 42, 26, 58, 6, 38, 22, 54, 14, 46, 30, 62, 1, 33, 17, 49, 9, 41, 25, 57, 5, 37, 21, 53, 13, 45, 29, 61, 3, 35, 19, 51, 11, 43, 27, 59, 7, 39, 23, 55, 15, 47, 31, 63];
const IVI_REV7: [u32; 128] = [ 0, 64, 32, 96, 16, 80, 48, 112, 8, 72, 40, 104, 24, 88, 56, 120, 4, 68, 36, 100, 20, 84, 52, 116, 12, 76, 44, 108, 28, 92, 60, 124, 2, 66, 34, 98, 18, 82, 50, 114, 10, 74, 42, 106, 26, 90, 58, 122, 6, 70, 38, 102, 22, 86, 54, 118, 14, 78, 46, 110, 30, 94, 62, 126, 1, 65, 33, 97, 17, 81, 49, 113, 9, 73, 41, 105, 25, 89, 57, 121, 5, 69, 37, 101, 21, 85, 53, 117, 13, 77, 45, 109, 29, 93, 61, 125, 3, 67, 35, 99, 19, 83, 51, 115, 11, 75, 43, 107, 27, 91, 59, 123, 7, 71, 39, 103, 23, 87, 55, 119, 15, 79, 47, 111, 31, 95, 63, 127];
const IVI_REV8: [u32; 256] = [ 0, 128, 64, 192, 32, 160, 96, 224, 16, 144, 80, 208, 48, 176, 112, 240, 8, 136, 72, 200, 40, 168, 104, 232, 24, 152, 88, 216, 56, 184, 120, 248, 4, 132, 68, 196, 36, 164, 100, 228, 20, 148, 84, 212, 52, 180, 116, 244, 12, 140, 76, 204, 44, 172, 108, 236, 28, 156, 92, 220, 60, 188, 124, 252, 2, 130, 66, 194, 34, 162, 98, 226, 18, 146, 82, 210, 50, 178, 114, 242, 10, 138, 74, 202, 42, 170, 106, 234, 26, 154, 90, 218, 58, 186, 122, 250, 6, 134, 70, 198, 38, 166, 102, 230, 22, 150, 86, 214, 54, 182, 118, 246, 14, 142, 78, 206, 46, 174, 110, 238, 30, 158, 94, 222, 62, 190, 126, 254, 1, 129, 65, 193, 33, 161, 97, 225, 17, 145, 81, 209, 49, 177, 113, 241, 9, 137, 73, 201, 41, 169, 105, 233, 25, 153, 89, 217, 57, 185, 121, 249, 5, 133, 69, 197, 37, 165, 101, 229, 21, 149, 85, 213, 53, 181, 117, 245, 13, 141, 77, 205, 45, 173, 109, 237, 29, 157, 93, 221, 61, 189, 125, 253, 3, 131, 67, 195, 35, 163, 99, 227, 19, 147, 83, 211, 51, 179, 115, 243, 11, 139, 75, 203, 43, 171, 107, 235, 27, 155, 91, 219, 59, 187, 123, 251, 7, 135, 71, 199, 39, 167, 103, 231, 23, 151, 87, 215, 55, 183, 119, 247, 15, 143, 79, 207, 47, 175, 111, 239, 31, 159, 95, 223, 63, 191, 127, 255];

const IVI_REVS: [&[u32]; 9] = [ &IVI_REV0, &IVI_REV1, &IVI_REV2, &IVI_REV3, &IVI_REV4, &IVI_REV5, &IVI_REV6, &IVI_REV7, &IVI_REV8];

pub trait IVICodebookReader {
    fn read_ivi_codebook_desc(&mut self, mb_cb: bool, try_default: bool) -> DecoderResult<IVICodebook>;
    fn read_ivi_cb(&mut self, cb: &IVICodebook) -> BitReaderResult<u32>;
    fn read_ivi_cb_s(&mut self, cb: &IVICodebook) -> BitReaderResult<i32>;
}

impl<'a> IVICodebookReader for BitReader<'a> {
    fn read_ivi_codebook_desc(&mut self, mb_cb: bool, desc_coded: bool) -> DecoderResult<IVICodebook> {
        if !desc_coded {
            if mb_cb {
                Ok(IVI_MB_CB[7].init())
            } else {
                Ok(IVI_BLK_CB[7].init())
            }
        } else {
            let idx = self.read(3)? as usize;
            if idx != 7 {
                if mb_cb {
                    Ok(IVI_MB_CB[idx].init())
                } else {
                    Ok(IVI_BLK_CB[idx].init())
                }
            } else {
                let mut cb = IVI_CB_ZERO;
                cb.len = self.read(4)? as usize;
                if cb.len == 0 { return Err(DecoderError::InvalidData); }
                for i in 0..cb.len {
                    cb.bits[i] = self.read(4)? as u8;
                }
                Ok(cb.init())
            }
        }
    }
    #[inline(always)]
    fn read_ivi_cb(&mut self, cb: &IVICodebook) -> BitReaderResult<u32> {
/*        let pfx = if cb.len == 1 { 0 } else { self.read_code(UintCodeType::LimitedUnary((cb.len - 1) as u32, 0))? as usize };
        let nbits = cb.bits[pfx];
        let mut base: u32 = 0;
        for i in 0..pfx { base += 1 << cb.bits[i]; }
        let rval = self.read(nbits)?;
        let add = reverse_bits(rval, nbits);
        Ok(base + add)*/
        if cb.len > 1 {
            let len = (!self.peek(16)).trailing_zeros() as usize;
            let pfx;
            if len >= cb.len - 1 {
                pfx = cb.len - 1;
                self.skip((cb.len - 1) as u32)?;
            } else {
                pfx = len;
                self.skip((len + 1) as u32)?;
            }
            let nbits = cb.bits[pfx];
            let base = cb.offs[pfx];
            let rval = self.read(nbits)?;
            let add = IVI_REVS[nbits as usize][rval as usize];
            Ok(base + add)
        } else {
            let nbits = cb.bits[0];
            Ok(IVI_REVS[nbits as usize][self.read(nbits)? as usize])
        }
    }
    #[inline(always)]
    fn read_ivi_cb_s(&mut self, cb: &IVICodebook) -> BitReaderResult<i32> {
        let v = self.read_ivi_cb(cb)?;
        if v == 0 {
            Ok(0)
        } else {
            let sign = (v & 1) == 1;
            let val = (v >> 1) as i32;
            if sign {
                Ok(val + 1)
            } else {
                Ok(-val)
            }
        }
    }
}

pub const IVI_MB_CB: &[IVICodebook; 8] = &[
    IVICodebook { len:  8, bits: [ 0, 4, 5, 4, 4, 4, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 12, bits: [ 0, 2, 2, 3, 3, 3, 3, 5, 3, 2, 2, 2, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 12, bits: [ 0, 2, 3, 4, 3, 3, 3, 3, 4, 3, 2, 2, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 12, bits: [ 0, 3, 4, 4, 3, 3, 3, 3, 3, 2, 2, 2, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 13, bits: [ 0, 4, 4, 3, 3, 3, 3, 2, 3, 3, 2, 1, 1, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len:  9, bits: [ 0, 4, 4, 4, 4, 3, 3, 3, 2, 0, 0, 0, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 10, bits: [ 0, 4, 4, 4, 4, 3, 3, 2, 2, 2, 0, 0, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 12, bits: [ 0, 4, 4, 4, 3, 3, 2, 3, 2, 2, 2, 2, 0, 0, 0, 0 ], offs: [0; 16] }
];

pub const IVI_BLK_CB: &[IVICodebook; 8] = &[
    IVICodebook { len: 10, bits: [ 1, 2, 3, 4, 4, 7, 5, 5, 4, 1, 0, 0, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 11, bits: [ 2, 3, 4, 4, 4, 7, 5, 4, 3, 3, 2, 0, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 12, bits: [ 2, 4, 5, 5, 5, 5, 6, 4, 4, 3, 1, 1, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 13, bits: [ 3, 3, 4, 4, 5, 6, 6, 4, 4, 3, 2, 1, 1, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 11, bits: [ 3, 4, 4, 5, 5, 5, 6, 5, 4, 2, 2, 0, 0, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 13, bits: [ 3, 4, 5, 5, 5, 5, 6, 4, 3, 3, 2, 1, 1, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len: 13, bits: [ 3, 4, 5, 5, 5, 6, 5, 4, 3, 3, 2, 1, 1, 0, 0, 0 ], offs: [0; 16] },
    IVICodebook { len:  9, bits: [ 3, 4, 4, 5, 5, 5, 6, 5, 5, 0, 0, 0, 0, 0, 0, 0 ], offs: [0; 16] }
];

#[allow(unused_variables)]
#[allow(clippy::many_single_char_names)]
fn read_trans_band_header(br: &mut BitReader, w: usize, h: usize, dst: &mut [i16], dstride: usize) -> DecoderResult<()> {
    let color_plane     = br.read(2)?;
    let bit_depth       = br.read(3)?;
    let dirty_rects     = br.read(8)? as usize;
    for i in 0..dirty_rects {
        let x = br.read(16)?;
        let y = br.read(16)?;
        let l = br.read(16)?;
        let r = br.read(16)?;
    }
    let has_trans_color = br.read_bool()?;
    if has_trans_color {
        let r = br.read(8)?;
        let g = br.read(8)?;
        let b = br.read(8)?;
    }

    br.skip(1)?;

    let mut cb = IVI_CB_ZERO;
    cb.len = br.read(4)? as usize;
    if cb.len == 0 { return Err(DecoderError::InvalidData); }
    for i in 0..cb.len {
        cb.bits[i] = br.read(4)? as u8;
    }
    cb = cb.init();
    br.align();

let tile_start = br.tell();
    let empty = br.read_bool()?;
    if !empty {
        br.read_bool()?;
        let mut len = br.read(8)? as usize;
        if len == 255 {
           len = br.read(24)? as usize;
        }
        br.align();
let tile_end = tile_start + len * 8;

        let first_val = br.read_bool()?;

        let mut dec_size = 0;
        let mut x = 0;
        let mut y = 0;
        let mut fill = if !first_val { 255-128 } else { 0-128 };
        let tr_w = (w + 31) & !31;
        while br.tell() < tile_end {
            let code = br.read_ivi_cb(&cb)? as usize;
            if code == 0 {
                dec_size += 255;
                for _ in 0..255 {
                    if (x < w) && (y < h) {
                        dst[x + y * dstride] = fill;
                    }
                    x += 1;
                    if x == tr_w {
                        x = 0;
                        y += 1;
                    }
                }
            } else {
                dec_size += code;
                for _ in 0..code {
                    if (x < w) && (y < h) {
                        dst[x + y * dstride] = fill;
                    }
                    x += 1;
                    if x == tr_w {
                        x = 0;
                        y += 1;
                    }
                }
                fill = !fill;
            }
        }
        br.align();
    } else {
    }

    Ok(())
}

#[allow(clippy::cast_lossless)]
fn decode_block8x8(br: &mut BitReader, blk_cb: &IVICodebook, rvmap: &RVMap, tables: &TxParams8x8, is_intra: bool, is_2d: bool, prev_dc: &mut i32, quant: u8, coeffs: &mut [i32; 64], transform: TrFunc) -> DecoderResult<()> {
    let mut idx: isize = -1;
    let quant_mat = if is_intra { tables.quant_intra } else { tables.quant_inter };
    while idx <= 64 {
        let c = br.read_ivi_cb(blk_cb)?;
        if c == rvmap.eob_sym { break; }
        let run;
        let val: i32;
        if c != rvmap.esc_sym {
            validate!(c < 256);
            run = rvmap.runtab[c as usize] as isize;
            val = rvmap.valtab[c as usize] as i32;
        } else {
            run = (br.read_ivi_cb(blk_cb)? as isize) + 1;
            let lo = br.read_ivi_cb(blk_cb)?;
            let hi = br.read_ivi_cb(blk_cb)?;
            let v = (hi << 6) + lo;
            if v == 0 {
                val = 0; // should not happen but still...
            } else {
                let vv = (v >> 1) as i32;
                if (v & 1) != 0 {
                    val = vv + 1;
                } else {
                    val = -vv;
                }
            }
        }
        idx += run;
        validate!((0..64).contains(&idx));

        let spos = tables.scan[idx as usize];
        let q = (u32::from(quant_mat[spos]) * u32::from(quant)) >> 9;
        if q > 1 {
            let qq = q as i32;
            let bias = (((q ^ 1) - 1) >> 1) as i32;
            coeffs[spos] = val * qq;
            if val > 0 { coeffs[spos] += bias; }
            else       { coeffs[spos] -= bias; }
        } else {
            coeffs[spos] = val;
        }
    }
    if is_intra && is_2d {
        *prev_dc += coeffs[0];
        coeffs[0] = *prev_dc;
    }
    (transform)(coeffs);
    Ok(())
}
#[allow(clippy::cast_lossless)]
fn decode_block4x4(br: &mut BitReader, blk_cb: &IVICodebook, rvmap: &RVMap, tables: &TxParams4x4, is_intra: bool, is_2d: bool, prev_dc: &mut i32, quant: u8, coeffs: &mut [i32; 64], transform: TrFunc) -> DecoderResult<()> {
    let mut idx: isize = -1;
    let quant_mat = if is_intra { tables.quant_intra } else { tables.quant_inter };
    while idx <= 64 {
        let c = br.read_ivi_cb(blk_cb)?;
        if c == rvmap.eob_sym { break; }
        let run;
        let val: i32;
        if c != rvmap.esc_sym {
            validate!(c < 256);
            run = rvmap.runtab[c as usize] as isize;
            val = rvmap.valtab[c as usize] as i32;
        } else {
            run = (br.read_ivi_cb(blk_cb)? as isize) + 1;
            let lo = br.read_ivi_cb(blk_cb)?;
            let hi = br.read_ivi_cb(blk_cb)?;
            let v = (hi << 6) + lo;
            if v == 0 {
                val = 0; // should not happen but still...
            } else {
                if (v & 1) != 0 {
                    val = ((v >> 1) as i32) + 1;
                } else {
                    val = -((v >> 1) as i32);
                }
            }
        }
        idx += run;
        validate!((0..16).contains(&idx));

        let spos = tables.scan[idx as usize];
        let q = (u32::from(quant_mat[spos]) * u32::from(quant)) >> 9;
        if q > 1 {
            let qq = q as i32;
            let bias = (((q ^ 1) - 1) >> 1) as i32;
            coeffs[spos] = val * qq;
            if val > 0 { coeffs[spos] += bias; }
            else       { coeffs[spos] -= bias; }
        } else {
            coeffs[spos] = val;
        }
    }
    if is_intra && is_2d {
        *prev_dc += coeffs[0];
        coeffs[0] = *prev_dc;
    }
    (transform)(coeffs);
    Ok(())
}

fn put_block(frame: &mut [i16], offs: usize, stride: usize, blk: &[i32], blk_size: usize) {
    unsafe {
        let mut dptr = frame.as_mut_ptr().add(offs);
        for y in 0..blk_size {
            for x in 0..blk_size {
                *dptr.add(x) = blk[x + y * blk_size] as i16;
            }
            dptr = dptr.add(stride);
        }
    }
}

fn add_block(frame: &mut [i16], offs: usize, stride: usize, blk: &[i32], blk_size: usize) {
    unsafe {
        let mut dptr = frame.as_mut_ptr().add(offs);
        for y in 0..blk_size {
            for x in 0..blk_size {
                *dptr.add(x) = (*dptr.add(x)).wrapping_add(blk[x + y * blk_size] as i16);
            }
            dptr = dptr.add(stride);
        }
    }
}

struct FrameData {
    plane_buf:      [Vec<i16>; 4],
    plane_stride:   [usize; 4],
    pic_hdr:        PictureHeader,
}

fn align(val: usize, bits: u8) -> usize {
    let mask = (1 << bits) - 1;
    (val + mask) & !mask
}

impl FrameData {
    fn new() -> NABufferRef<Self> {
        NABufferRef::new(FrameData {
            plane_buf:      [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            plane_stride:   [0, 0, 0, 0],
            pic_hdr:        PictureHeader::new_null(IVIFrameType::Intra),
        })
    }
    fn realloc(&mut self, pic_hdr: &PictureHeader) -> DecoderResult<()> {
        let width  = align(pic_hdr.width,  6);
        let height = align(pic_hdr.height, 6);

        let stride = width;
        self.plane_buf[0].resize(stride * height, 0);
        self.plane_stride[0] = stride;
        for plane in 1..3 {
            self.plane_buf[plane].resize((stride >> 1) * (height >> 1), 0);
            self.plane_stride[plane] = stride >> 1;
        }
        if pic_hdr.transparent {
            self.plane_buf[3].resize(stride * height, 0);
            self.plane_stride[3] = stride;
        }
        self.pic_hdr = *pic_hdr;
        Ok(())
    }
    fn fill_plane(&mut self, vb: &mut NAVideoBuffer<u8>, plane: usize) {
        let dplane = if (plane == 1) || (plane == 2) { plane ^ 3 } else { plane };
        let (w, h)   = vb.get_dimensions(dplane);
        let mut didx = vb.get_offset(dplane);
        let dstride  = vb.get_stride(dplane);
        let dst      = vb.get_data_mut().unwrap();
        let src      = &self.plane_buf[plane];
        let mut sidx = 0;
        let sstride  = self.plane_stride[plane];
        for _ in 0..h {
            for x in 0..w {
                dst[didx + x] = clip8(src[sidx + x] + 128);
            }
            didx += dstride;
            sidx += sstride;
        }
    }
}

#[allow(clippy::many_single_char_names)]
fn do_mc(dst: &mut [i16], dstride: usize, src: &[i16], sstride: usize, x: usize, y: usize, l: usize, r: usize, t: usize, b: usize, mv_x: i32, mv_y: i32, is_hpel: bool, blk_size: usize) {
    let (xoff, yoff, mv_mode) = if is_hpel {
            (mv_x >> 1, mv_y >> 1, ((mv_x & 1) + (mv_y & 1) * 2) as u8)
        } else{
            (mv_x, mv_y, 0)
        };
    let xpos = (x as isize) + (xoff as isize);
    let ypos = (y as isize) + (yoff as isize);
    if (xpos < (l as isize)) || ((xpos as usize) + blk_size + ((mv_mode &  1) as usize) > r) ||
       (ypos < (t as isize)) || ((ypos as usize) + blk_size + ((mv_mode >> 1) as usize) > b) {
//println!(" copy from {},{} of {}-{},{}-{} {}x{}!", xpos, ypos, l,r,t,b,blk_size,blk_size);
        return;
    }
    let sidx = (xpos as usize) + (ypos as usize) * sstride;
    if blk_size == 8 {
        ivi_mc_put(dst, dstride, &src[sidx..], sstride, mv_mode, 8, 8);
    } else {
        ivi_mc_put(dst, dstride, &src[sidx..], sstride, mv_mode, 4, 4);
    }
}

#[allow(clippy::many_single_char_names)]
fn do_mc_b(dst: &mut [i16], dstride: usize, src1: &[i16], sstride1: usize, src2: &[i16], sstride2: usize, x: usize, y: usize, l: usize, r: usize, t: usize, b: usize, mv_x: i32, mv_y: i32, mv2_x: i32, mv2_y: i32, is_hpel: bool, blk_size: usize) {
    let (xoff1, yoff1, mv_mode1) = if is_hpel {
            (mv_x >> 1, mv_y >> 1, ((mv_x & 1) + (mv_y & 1) * 2) as u8)
        } else{
            (mv_x, mv_y, 0)
        };
    let xpos1 = (x as isize) + (xoff1 as isize);
    let ypos1 = (y as isize) + (yoff1 as isize);
    if (xpos1 < (l as isize)) || ((xpos1 as usize) + blk_size + ((mv_mode1 &  1) as usize) > r) ||
       (ypos1 < (t as isize)) || ((ypos1 as usize) + blk_size + ((mv_mode1 >> 1) as usize) > b) {
        return;
    }
    let sidx1 = (xpos1 as usize) + (ypos1 as usize) * sstride1;
    let (xoff2, yoff2, mv_mode2) = if is_hpel {
            (mv2_x >> 1, mv2_y >> 1, ((mv2_x & 1) + (mv2_y & 1) * 2) as u8)
        } else{
            (mv2_x, mv2_y, 0)
        };
    let xpos2 = (x as isize) + (xoff2 as isize);
    let ypos2 = (y as isize) + (yoff2 as isize);
    if (xpos2 < (l as isize)) || ((xpos2 as usize) + blk_size + ((mv_mode2 &  1) as usize) > r) ||
       (ypos2 < (t as isize)) || ((ypos2 as usize) + blk_size + ((mv_mode2 >> 1) as usize) > b) {
        return;
    }
    let sidx2 = (xpos2 as usize) + (ypos2 as usize) * sstride2;
    ivi_mc_avg(dst, dstride, &src1[sidx1..], sstride1, mv_mode1,  &src2[sidx2..], sstride2, mv_mode2, blk_size, blk_size);
}

pub trait IndeoXParser {
    fn decode_picture_header(&mut self, br: &mut BitReader) -> DecoderResult<PictureHeader>;
    fn decode_band_header(&mut self, br: &mut BitReader, pic_hdr: &PictureHeader, plane: usize, band: usize) -> DecoderResult<BandHeader>;
    fn decode_mb_info(&mut self, br: &mut BitReader, pic_hdr: &PictureHeader, band_hdr: &BandHeader, tile: &mut IVITile, ref_tile: Option<&IVITile>, mv_scale: u8) -> DecoderResult<()>;
    fn recombine_plane(&mut self, src: &[i16], sstride: usize, dst: &mut [u8], dstride: usize, w: usize, h: usize);
}

const MISSING_REF: usize = 42;

pub struct IVIDecoder {
    ftype:      IVIFrameType,
    frames:     [NABufferRef<FrameData>; 4],
    cur_frame:  usize,
    prev_frame: usize,
    next_frame: usize,
    iref_0:     usize,
    iref_1:     usize,
    scal_ref:   usize,
    vinfo:      NAVideoInfo,
    vinfoa:     NAVideoInfo,
    bref:       Option<NABufferType>,

    bands:      Vec<BandHeader>,
    band_tiles: usize,
    tiles:      Vec<IVITile>,
    num_tiles:  [[usize; 4]; 4],
    tile_start: [[usize; 4]; 4],

    scalable:   bool,
}

impl IVIDecoder {
    pub fn new(scalable: bool) -> Self {
        let mut bands: Vec<BandHeader> = Vec::with_capacity(12);
        bands.resize(12, BandHeader::new_empty(42, 42));
        IVIDecoder {
            ftype:      IVIFrameType::NULL,
            frames:     [FrameData::new(), FrameData::new(), FrameData::new(), FrameData::new()],
            cur_frame: 0, prev_frame: MISSING_REF, next_frame: MISSING_REF,
            iref_0: MISSING_REF, iref_1: MISSING_REF, scal_ref: MISSING_REF,
            vinfo:  NAVideoInfo::new(0, 0, false, YUV410_FORMAT),
            vinfoa: NAVideoInfo::new(0, 0, false, YUVA410_FORMAT),
            bref: None,

            bands,
            band_tiles: 0,
            tiles: Vec::new(), tile_start: [[0; 4]; 4], num_tiles: [[0; 4]; 4],

            scalable,
        }
    }

    fn realloc(&mut self, pic_hdr: &PictureHeader) -> DecoderResult<()> {
        let planes = if pic_hdr.transparent { 4 } else { 3 };

        //self.bands.clear();
        self.tiles.clear();
        self.num_tiles  = [[0; 4]; 4];
        self.tile_start = [[0; 4]; 4];
        let mut tstart: usize = 0;
        for plane in 0..planes {
            let is_luma = (plane != 1) && (plane != 2);
            let bands = if is_luma { pic_hdr.luma_bands } else { pic_hdr.chroma_bands };
            let mut band_w = if is_luma { pic_hdr.width   } else { (pic_hdr.width   + 3) >> 2 };
            let mut band_h = if is_luma { pic_hdr.height  } else { (pic_hdr.height  + 3) >> 2 };
            let mut tile_w = if is_luma { pic_hdr.slice_w } else { (pic_hdr.slice_w + 3) >> 2 };
            let mut tile_h = if is_luma { pic_hdr.slice_h } else { (pic_hdr.slice_h + 3) >> 2 };
            if bands > 1 {
                band_w = (band_w + 1) >> 1;
                band_h = (band_h + 1) >> 1;
                if plane == 0 {
                    tile_w = (tile_w + 1) >> 1;
                    tile_h = (tile_h + 1) >> 1;
                }
            }
            if plane == 0 {
                self.band_tiles = ((band_w + tile_w - 1) / tile_w) * ((band_h + tile_h - 1) / tile_h);
            }
            for band in 0..bands {
                self.tile_start[plane][band] = tstart;
                let band_xoff = if (band & 1) == 1 { band_w } else { 0 };
                let band_yoff = if (band & 2) == 2 { band_h } else { 0 };
                let mut y = 0;
                while y < band_h {
                    let cur_h = if y + tile_h <= band_h { tile_h } else { band_h - y };
                    let mut x = 0;
                    while x < band_w {
                        let cur_w = if x + tile_w <= band_w { tile_w } else { band_w - x };
                        let tile = IVITile::new(band_xoff + x, band_yoff + y, cur_w, cur_h);
                        self.tiles.push(tile);
                        self.num_tiles[plane][band] += 1;
                        tstart += 1;
                        x += tile_w;
                    }
                    y += tile_h;
                }
            }
        }
        Ok(())
    }
    fn decode_band(&mut self, pic_hdr: &PictureHeader, dec: &mut dyn IndeoXParser, br: &mut BitReader, plane_no: usize, band_no: usize) -> DecoderResult<()> {
        let bidx = match plane_no {
            0 => { band_no },
            _ => { pic_hdr.luma_bands + plane_no - 1 },
        };
        let prev_band = if bidx >= self.bands.len() { BandHeader::new_empty(plane_no, band_no) } else { self.bands[bidx].clone() };
        let mut band = dec.decode_band_header(br, pic_hdr, plane_no, band_no)?;
        if let TxType::None = band.ttype {
            validate!(band.plane_no == prev_band.plane_no);
            validate!(band.band_no  == prev_band.band_no);
            validate!(band.blk_size == prev_band.blk_size);
            band.tr    = prev_band.tr;
            band.ttype = prev_band.ttype;
        };

        let tstart = self.tile_start[band.plane_no][band.band_no];
        let tend   = tstart + self.num_tiles[band.plane_no][band.band_no];
        let mb_size = band.mb_size;
        let (tr, tr_dc) = match band.ttype {
            TxType::Transform4(_) => { ivi_get_transform4x4_funcs(band.tr) },
            TxType::Transform8(_) => { ivi_get_transform8x8_funcs(band.tr) },
            _ => { ivi_get_transform4x4_funcs(band.tr) },
        };
        for tile_no in tstart..tend {
            {
                let tile = &mut self.tiles[tile_no];
                let mb_w = (tile.w + mb_size - 1) / mb_size;
                let mb_h = (tile.h + mb_size - 1) / mb_size;
                tile.mb_w = mb_w;
                tile.mb_h = mb_h;
                tile.mb.clear();
                tile.mb.resize(mb_w * mb_h, MB::new(0, 0));
            }

            let tile_start = br.tell();
            if !br.read_bool()? {
                let res = br.read_bool()?;
                validate!(res);
                let mut len = br.read(8)? as usize;
                if len == 255 {
                    len = br.read(24)? as usize;
                }
                br.align();
                validate!(len > 0);
                let tile_end = (tile_start & !7) + len * 8;
                validate!(tile_end > br.tell());
                validate!(tile_end <= br.tell() + (br.left() as usize));
                {
                    let ref_tile: Option<&IVITile>;
                    let mv_scale;
                    if (plane_no == 0) && (band_no == 0) {
                        mv_scale = 0;
                    } else {
                        mv_scale = (((self.bands[0].mb_size >> 3) as i8) - ((band.mb_size >> 3) as i8)) as u8;
                    }
                    let (ref_tiles, cur_tiles) = self.tiles.split_at_mut(tile_no);
                    let tile = &mut cur_tiles[0];
                    if plane_no != 0 || band_no != 0 {
                        let rtile = &ref_tiles[tile_no % self.band_tiles];
                        if (tile.mb_w != rtile.mb_w) || (tile.mb_h != rtile.mb_h) {
                            ref_tile = None;
                        } else {
                            ref_tile = Some(rtile);
                        }
                    } else {
                        ref_tile = None;
                    }
                    dec.decode_mb_info(br, pic_hdr, &band, tile, ref_tile, mv_scale)?;
                }

                self.decode_tile(br, &band, tile_no, tr, tr_dc)?;
                br.align();
let skip_part = tile_end - br.tell();
br.skip(skip_part as u32)?;
            } else {
                {
                    let ref_tile: Option<&IVITile>;
                    let mv_scale;
                    if (plane_no == 0) && (band_no == 0) {
                        mv_scale = 0;
                    } else {
                        mv_scale = (((self.bands[0].mb_size >> 3) as i8) - ((band.mb_size >> 3) as i8)) as u8;
                    }
                    let (ref_tiles, cur_tiles) = self.tiles.split_at_mut(tile_no);
                    let tile = &mut cur_tiles[0];
                    if plane_no != 0 || band_no != 0 {
                        let rtile = &ref_tiles[0];
                        if (tile.mb_w != rtile.mb_w) || (tile.mb_h != rtile.mb_h) {
                            ref_tile = None;
                        } else {
                            ref_tile = Some(rtile);
                        }
                    } else {
                        ref_tile = None;
                    }
                    let mut mb_idx = 0;
                    for mb_y in 0..tile.mb_h {
                        for mb_x in 0..tile.mb_w {
                            let mut mb = MB::new(tile.pos_x + mb_x * band.mb_size, tile.pos_y + mb_y * band.mb_size);
                            mb.mtype = MBType::Inter;
                            mb.cbp   = 0;
                            if band.inherit_mv {
                                if let Some(tileref) = ref_tile {
                                    let mx = tileref.mb[mb_idx].mv_x;
                                    let my = tileref.mb[mb_idx].mv_y;
                                    mb.mv_x = scale_mv(mx, mv_scale);
                                    mb.mv_y = scale_mv(my, mv_scale);
                                }
                            }
                            tile.mb[mb_idx] = mb;
                            mb_idx += 1;
                        }
                    }
                }
                self.decode_tile(br, &band, tile_no, tr, tr_dc)?;
            }
        }
        self.bands[bidx] = band;
        br.align();
        Ok(())
    }
    fn decode_tile(&mut self, br: &mut BitReader, band: &BandHeader, tile_no: usize, tr: TrFunc, transform_dc: TrFuncDC) -> DecoderResult<()> {
        let mut mb_idx = 0;
        let mut prev_dc: i32 = 0;
        let tile = &mut self.tiles[tile_no];
        let mut frame = self.frames[self.cur_frame].clone();

        let stride = frame.plane_stride[band.plane_no];
        let mut dstidx = tile.pos_x + tile.pos_y * stride;
        let dst = &mut frame.plane_buf[band.plane_no];
        let pos_x = tile.pos_x;
        let pos_y = tile.pos_y;
        let tile_w = (tile.w + 15) & !15;
        let tile_h = (tile.h + 15) & !15;
        for mb_y in 0..tile.mb_h {
            for mb_x in 0..tile.mb_w {
                let mb = &mut tile.mb[mb_idx];

                let is_intra = mb.mtype == MBType::Intra;

                if band.mb_size != band.blk_size {
                    let mut cbp = mb.cbp;
                    for blk_no in 0..4 {
                        let mut blk: [i32; 64] = [0; 64];
                        let boff = (blk_no & 1) * band.blk_size + (blk_no >> 1) * band.blk_size * stride + mb_x * band.mb_size;
                        if !is_intra {
                            if mb.mtype != MBType::Bidir {
                                let idx;
                                if mb.mtype != MBType::Backward {
                                    idx = self.prev_frame;
                                } else {
                                    idx = self.next_frame;
                                }
                                let pf = &self.frames[idx];
                                do_mc(&mut dst[dstidx + boff..], stride,
                                      &pf.plane_buf[band.plane_no], pf.plane_stride[band.plane_no],
                                      pos_x + mb_x * band.mb_size + (blk_no & 1) * band.blk_size,
                                      pos_y + mb_y * band.mb_size + (blk_no >> 1) * band.blk_size,
                                      pos_x, pos_x + tile_w, pos_y, pos_y + tile_h,
                                      mb.mv_x, mb.mv_y, band.halfpel, band.blk_size);
                            } else {
                                let pf = &self.frames[self.prev_frame];
                                let nf = &self.frames[self.next_frame];
                                do_mc_b(&mut dst[dstidx + boff..], stride,
                                      &pf.plane_buf[band.plane_no], pf.plane_stride[band.plane_no],
                                      &nf.plane_buf[band.plane_no], nf.plane_stride[band.plane_no],
                                      pos_x + mb_x * band.mb_size + (blk_no & 1) * band.blk_size,
                                      pos_y + mb_y * band.mb_size + (blk_no >> 1) * band.blk_size,
                                      pos_x, pos_x + tile_w, pos_y, pos_y + tile_h,
                                      mb.mv_x, mb.mv_y, mb.mv2_x, mb.mv2_y, band.halfpel,
                                      band.blk_size);
                            }
                        }
                        if (cbp & 1) != 0 {
                            if let TxType::Transform8(ref params) = band.ttype {
                                decode_block8x8(br, &band.blk_cb, &band.rvmap, params, is_intra, band.tr.is_2d(), &mut prev_dc, mb.q, &mut blk, tr)?;
                                if is_intra {
                                    put_block(dst, dstidx + boff, stride, &blk, 8);
                                } else {
                                    add_block(dst, dstidx + boff, stride, &blk, 8);
                                }
                            }
                            if let TxType::Transform4(ref params) = band.ttype {
                                decode_block4x4(br, &band.blk_cb, &band.rvmap, params, is_intra, band.tr.is_2d(), &mut prev_dc, mb.q, &mut blk, tr)?;
                                if is_intra {
                                    put_block(dst, dstidx + boff, stride, &blk, 4);
                                } else {
                                    add_block(dst, dstidx + boff, stride, &blk, 4);
                                }
                            }
                        } else {
                            if is_intra {
                                (transform_dc)(&mut blk, prev_dc);
                                put_block(dst, dstidx + boff, stride, &blk, band.blk_size);
                            }
                        }
                        cbp >>= 1;
                    }
                } else {
                    let mut blk: [i32; 64] = [0; 64];
                    if !is_intra {
                        if mb.mtype != MBType::Bidir {
                            let idx;
                            if mb.mtype != MBType::Backward {
                                idx = self.prev_frame;
                            } else {
                                idx = self.next_frame;
                            }
                            let pf = &self.frames[idx];
                            do_mc(&mut dst[dstidx + mb_x * band.blk_size..], stride,
                                  &pf.plane_buf[band.plane_no], pf.plane_stride[band.plane_no],
                                  pos_x + mb_x * band.mb_size,
                                  pos_y + mb_y * band.mb_size,
                                  pos_x, pos_x + tile_w, pos_y, pos_y + tile_h,
                                  mb.mv_x, mb.mv_y, band.halfpel, band.blk_size);
                        } else {
                            let pf = &self.frames[self.prev_frame];
                            let nf = &self.frames[self.next_frame];
                            do_mc_b(&mut dst[dstidx + mb_x * band.blk_size..], stride,
                                    &pf.plane_buf[band.plane_no], pf.plane_stride[band.plane_no],
                                    &nf.plane_buf[band.plane_no], nf.plane_stride[band.plane_no],
                                    pos_x + mb_x * band.mb_size,
                                    pos_y + mb_y * band.mb_size,
                                    pos_x, pos_x + tile_w, pos_y, pos_y + tile_h,
                                    mb.mv_x, mb.mv_y, mb.mv2_x, mb.mv2_y, band.halfpel,
                                    band.blk_size);
                        }
                    }
                    if mb.cbp != 0 {
                        if let TxType::Transform8(ref params) = band.ttype {
                            decode_block8x8(br, &band.blk_cb, &band.rvmap, params, is_intra, band.tr.is_2d(), &mut prev_dc, mb.q, &mut blk, tr)?;
                        }
                        if let TxType::Transform4(ref params) = band.ttype {
                            decode_block4x4(br, &band.blk_cb, &band.rvmap, params, is_intra, band.tr.is_2d(), &mut prev_dc, mb.q, &mut blk, tr)?;
                        }
                        if is_intra {
                            if band.blk_size == 8 {
                                put_block(dst, dstidx + mb_x * band.blk_size, stride, &blk, 8);
                            } else {
                                put_block(dst, dstidx + mb_x * band.blk_size, stride, &blk, 4);
                            }
                        } else {
                            if band.blk_size == 8 {
                                add_block(dst, dstidx + mb_x * band.blk_size, stride, &blk, 8);
                            } else {
                                add_block(dst, dstidx + mb_x * band.blk_size, stride, &blk, 4);
                            }
                        }
                    } else {
                        if is_intra {
                            (transform_dc)(&mut blk, prev_dc);
                            if band.blk_size == 8 {
                                put_block(dst, dstidx + mb_x * band.blk_size, stride, &blk, 8);
                            } else {
                                put_block(dst, dstidx + mb_x * band.blk_size, stride, &blk, 4);
                            }
                        }
                    }
                }
                mb_idx += 1;
            }
            dstidx += stride * band.mb_size;
        }
        Ok(())
    }

    fn find_unused_frame(&self) -> usize {
        for fno in 0..4 {
            if (fno != self.iref_0) && (fno != self.iref_1) && (fno != self.scal_ref) {
                return fno;
            }
        }
        unreachable!();
    }

    fn decode_single_frame(&mut self, dec: &mut dyn IndeoXParser, br: &mut BitReader) -> DecoderResult<NABufferType> {
        let pic_hdr = dec.decode_picture_header(br)?;
        self.ftype = pic_hdr.ftype;
        if pic_hdr.ftype.is_null() {
            return Ok(NABufferType::None);
        }

        self.cur_frame = self.find_unused_frame();
        match self.ftype {
            IVIFrameType::Inter => {
                    self.prev_frame = self.iref_0;
                    if self.prev_frame == MISSING_REF {
                        return Err(DecoderError::MissingReference);
                    }
                },
            IVIFrameType::InterDroppable => {
                    self.prev_frame = self.scal_ref;
                    if self.prev_frame == MISSING_REF {
                        return Err(DecoderError::MissingReference);
                    }
                },
            IVIFrameType::InterScal => {
                    self.prev_frame = self.scal_ref;
                    if self.prev_frame == MISSING_REF {
                        return Err(DecoderError::MissingReference);
                    }
                },
            IVIFrameType::Bidir => {
                    self.prev_frame = self.iref_1;
                    self.next_frame = self.iref_0;
                    if (self.prev_frame == MISSING_REF) || (self.next_frame == MISSING_REF) {
                        return Err(DecoderError::MissingReference);
                    }
                },
            _ => {},
        };

        let mut vinfo = if pic_hdr.transparent { self.vinfoa } else { self.vinfo };
        vinfo.set_width(pic_hdr.width);
        vinfo.set_height(pic_hdr.height);
        let mut buftype = alloc_video_buffer(vinfo, 0)?;
        self.realloc(&pic_hdr)?;
        self.frames[self.cur_frame].realloc(&pic_hdr)?;

        if !self.scalable {
            for plane in 0..3 {
                let num_bands = if plane == 0 { pic_hdr.luma_bands } else { pic_hdr.chroma_bands };
                for band in 0..num_bands {
                    self.decode_band(&pic_hdr, dec, br, plane, band)?;
                }
                if let NABufferType::Video(ref mut vb) = buftype {
                    let mut frame = self.frames[self.cur_frame].clone();
                    if num_bands == 1 {
                        frame.fill_plane(vb, plane);
                    } else {
                        let dplane = if (plane == 1) || (plane == 2) { plane ^ 3 } else { plane };
                        let (w, h)  = vb.get_dimensions(dplane);
                        let dstride = vb.get_stride(dplane);
                        let off     = vb.get_offset(dplane);
                        let dst = vb.get_data_mut().unwrap();
                        dec.recombine_plane(&frame.plane_buf[plane], frame.plane_stride[plane], &mut dst[off..], dstride, w, h);
                    }
                }
            }
        } else {
            let mut bands_decoded = [[false; 10]; 3];
            let mut num_decoded = 0;

            let num_bands = [ pic_hdr.luma_bands, pic_hdr.chroma_bands, pic_hdr.chroma_bands ];

            for plane in 0..3 {
                for band in 0..num_bands[plane] {
                    if br.peek(8) == 0x01 { // skipped scalable bands
                        br.skip(8)?;
                        continue;
                    }
                    self.decode_band(&pic_hdr, dec, br, plane, band)?;
                    bands_decoded[plane][band] = true;
                    num_decoded += 1;
                }
            }
            if (num_decoded < num_bands[0] + num_bands[1] + num_bands[2]) && (br.left() > 0) {
                validate!(br.read(8)? == 0x00);
                while br.peek(8) == 0x00 {
                    if br.skip(8).is_err() { // happens at the end of data
                        break;
                    }
                }
                validate!((br.tell() & 31) == 0);

                for plane in 0..3 {
                    for band in 0..num_bands[plane] {
                        if bands_decoded[plane][band] || br.left() == 0 {
                            continue;
                        }
                        self.decode_band(&pic_hdr, dec, br, plane, band)?;
                        bands_decoded[plane][band] = true;
                        num_decoded += 1;
                    }
                }
            }

            if let NABufferType::Video(ref mut vb) = buftype {
                for plane in 0..3 {
                    let mut frame = self.frames[self.cur_frame].clone();
                    if num_bands[plane] == 1 {
                        frame.fill_plane(vb, plane);
                    } else {
                        let dplane = if (plane == 1) || (plane == 2) { plane ^ 3 } else { plane };
                        let (w, h)  = vb.get_dimensions(dplane);
                        let dstride = vb.get_stride(dplane);
                        let off     = vb.get_offset(dplane);
                        let dst = vb.get_data_mut().unwrap();
                        dec.recombine_plane(&frame.plane_buf[plane], frame.plane_stride[plane], &mut dst[off..], dstride, w, h);
                    }
                }
            }
        }
        if pic_hdr.transparent {
            let mut frame = self.frames[self.cur_frame].clone();
            let stride = frame.plane_stride[3];
            read_trans_band_header(br, pic_hdr.width, pic_hdr.height, &mut frame.plane_buf[3], stride)?;
            if let NABufferType::Video(ref mut vb) = buftype {
                frame.fill_plane(vb, 3);
            }
        }

        match self.ftype {
            IVIFrameType::Intra | IVIFrameType::Intra1 | IVIFrameType::Inter => {
                    self.iref_1   = self.iref_0;
                    self.iref_0   = self.cur_frame;
                    self.scal_ref = self.cur_frame;
                },
            IVIFrameType::InterScal => {
                    self.scal_ref = self.cur_frame;
                },
            _ => {},
        };

        Ok(buftype)
    }

    pub fn decode_frame(&mut self, dec: &mut dyn IndeoXParser, br: &mut BitReader) -> DecoderResult<NABufferType> {
        let res = self.decode_single_frame(dec, br)?;
        if (self.ftype == IVIFrameType::Intra) && (br.left() > 16) {
            loop {
                if br.left() < 8 { break; }
                if br.read(8)? == 0 { break; }
            }
            loop {
                if br.left() < 8 { break; }
                if br.peek(8) != 0 { break; }
                br.skip(8)?;
            }
            if br.left() > 24 {
                let seq = br.peek(21);
                if seq == 0xBFFF8 {
                    let res2 = self.decode_single_frame(dec, br);
                    if let Ok(res) = res2 {
                        self.bref = Some(res);
                    }
                }
                self.ftype = IVIFrameType::Intra;
            }
        }
        if self.bref.is_some() && self.ftype == IVIFrameType::Inter {
            let mut bref: Option<NABufferType> = Some(res);
            mem::swap(&mut bref, &mut self.bref);
            return Ok(bref.unwrap());
        }
        if let NABufferType::None = res {
            if self.bref.is_some() {
                let mut bref: Option<NABufferType> = None;
                mem::swap(&mut bref, &mut self.bref);
                self.ftype = IVIFrameType::Inter;
                return Ok(bref.unwrap());
            }
        }
        Ok(res)
    }

    pub fn flush(&mut self) {
        self.prev_frame = MISSING_REF;
        self.next_frame = MISSING_REF;
        self.iref_0     = MISSING_REF;
        self.iref_1     = MISSING_REF;
        self.scal_ref   = MISSING_REF;
    }

    pub fn is_intra(&self) -> bool {
        self.ftype.is_intra()
    }
    pub fn get_frame_type(&self) -> FrameType {
        match self.ftype {
            IVIFrameType::Intra             => { FrameType::I },
            IVIFrameType::Intra1            => { FrameType::I },
            IVIFrameType::Inter             => { FrameType::P },
            IVIFrameType::InterDroppable    => { FrameType::P },
            IVIFrameType::InterScal         => { FrameType::P },
            IVIFrameType::Bidir             => { FrameType::B },
            _                               => { FrameType::Skip },
        }
    }
}

pub struct RVMap {
    pub eob_sym:    u32,
    pub esc_sym:    u32,
    pub runtab:     [u8; 256],
    pub valtab:     [i8; 256],
}

impl Clone for RVMap {
    fn clone(&self) -> RVMap {
        let mut runtab: [u8; 256] = [0; 256];
        let mut valtab: [i8; 256] = [0; 256];
        runtab.copy_from_slice(&self.runtab);
        valtab.copy_from_slice(&self.valtab);
        RVMap { eob_sym: self.eob_sym, esc_sym: self.esc_sym, runtab, valtab }
    }
}

pub const IVI_ZERO_RVMAP: RVMap = RVMap { eob_sym: 0, esc_sym: 0, runtab: [0; 256], valtab: [0; 256] };

pub static IVI_RVMAPS: [RVMap; 9] = [
    RVMap { eob_sym: 5, esc_sym: 2, runtab: [
     1,  1,  0,  1,  1,  0,  1,  1,  2,  2,  1,  1,  1,  1,  3,  3,
     1,  1,  2,  2,  1,  1,  4,  4,  1,  1,  1,  1,  2,  2,  5,  5,
     1,  1,  3,  3,  1,  1,  6,  6,  1,  2,  1,  2,  7,  7,  1,  1,
     8,  8,  1,  1,  4,  2,  1,  4,  2,  1,  3,  3,  1,  1,  1,  9,
     9,  1,  2,  1,  2,  1,  5,  5,  1,  1, 10, 10,  1,  1,  3,  3,
     2,  2,  1,  1, 11, 11,  6,  4,  4,  1,  6,  1,  2,  1,  2, 12,
     8,  1, 12,  7,  8,  7,  1, 16,  1, 16,  1,  3,  3, 13,  1, 13,
     2,  2,  1, 15,  1,  5, 14, 15,  1,  5, 14,  1, 17,  8, 17,  8,
     1,  4,  4,  2,  2,  1, 25, 25, 24, 24,  1,  3,  1,  3,  1,  8,
     6,  7,  6,  1, 18,  8, 18,  1,  7, 23,  2,  2, 23,  1,  1, 21,
    22,  9,  9, 22, 19,  1, 21,  5, 19,  5,  1, 33, 20, 33, 20,  8,
     4,  4,  1, 32,  2,  2,  8,  3, 32, 26,  3,  1,  7,  7, 26,  6,
     1,  6,  1,  1, 16,  1, 10,  1, 10,  2, 16, 29, 28,  2, 29, 28,
     1, 27,  5,  8,  5, 27,  1,  8,  3,  7,  3, 31, 41, 31,  1, 41,
     6,  1,  6,  7,  4,  4,  1,  1,  2,  1,  2, 11, 34, 30, 11,  1,
    30, 15, 15, 34, 36, 40, 36, 40, 35, 35, 37, 37, 39, 39, 38, 38 ],
        valtab: [
      1,  -1,   0,   2,  -2,   0,   3,  -3,   1,  -1,   4,  -4,   5,  -5,   1,  -1,
      6,  -6,   2,  -2,   7,  -7,   1,  -1,   8,  -8,   9,  -9,   3,  -3,   1,  -1,
     10, -10,   2,  -2,  11, -11,   1,  -1,  12,   4, -12,  -4,   1,  -1,  13, -13,
      1,  -1,  14, -14,   2,   5,  15,  -2,  -5, -15,  -3,   3,  16, -16,  17,   1,
     -1, -17,   6,  18,  -6, -18,   2,  -2,  19, -19,   1,  -1,  20, -20,   4,  -4,
      7,  -7,  21, -21,   1,  -1,   2,   3,  -3,  22,  -2, -22,   8,  23,  -8,   1,
      2, -23,  -1,   2,  -2,  -2,  24,   1, -24,  -1,  25,   5,  -5,   1, -25,  -1,
      9,  -9,  26,   1, -26,   3,   1,  -1,  27,  -3,  -1, -27,   1,   3,  -1,  -3,
     28,  -4,   4,  10, -10, -28,   1,  -1,   1,  -1,  29,   6, -29,  -6,  30,  -4,
      3,   3,  -3, -30,   1,   4,  -1,  31,  -3,   1,  11, -11,  -1, -31,  32,  -1,
     -1,   2,  -2,   1,   1, -32,   1,   4,  -1,  -4,  33,  -1,   1,   1,  -1,   5,
      5,  -5, -33,  -1, -12,  12,  -5,  -7,   1,   1,   7,  34,   4,  -4,  -1,   4,
    -34,  -4,  35,  36,  -2, -35,  -2, -36,   2,  13,   2,  -1,   1, -13,   1,  -1,
     37,   1,  -5,   6,   5,  -1,  38,  -6,  -8,   5,   8,  -1,   1,   1, -37,  -1,
      5,  39,  -5,  -5,   6,  -6, -38, -39, -14,  40,  14,   2,   1,   1,  -2, -40,
     -1,  -2,   2,  -1,  -1,  -1,   1,   1,   1,  -1,   1,  -1,   1,  -1,   1,  -1 ],
    },
    RVMap { eob_sym: 0, esc_sym: 38, runtab: [
     0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  8,  6,  8,  7,
     7,  9,  9, 10, 10, 11, 11,  1, 12,  1, 12, 13, 13, 16, 14, 16,
    14, 15, 15, 17, 17, 18,  0, 18, 19, 20, 21, 19, 22, 21, 20, 22,
    25, 24,  2, 25, 24, 23, 23,  2, 26, 28, 26, 28, 29, 27, 29, 27,
    33, 33,  1, 32,  1,  3, 32, 30, 36,  3, 36, 30, 31, 31, 35, 34,
    37, 41, 34, 35, 37,  4, 41,  4, 49,  8,  8, 49, 40, 38,  5, 38,
    40, 39,  5, 39, 42, 43, 42,  7, 57,  6, 43, 44,  6, 50,  7, 44,
    57, 48, 50, 48, 45, 45, 46, 47, 51, 46, 47, 58,  1, 51, 58,  1,
    52, 59, 53,  9, 52, 55, 55, 59, 53, 56, 54, 56, 54,  9, 64, 64,
    60, 63, 60, 63, 61, 62, 61, 62,  2, 10,  2, 10, 11,  1, 11, 13,
    12,  1, 12, 13, 16, 16,  8,  8, 14,  3,  3, 15, 14, 15,  4,  4,
     1, 17, 17,  5,  1,  7,  7,  5,  6,  1,  2,  2,  6, 22,  1, 25,
    21, 22,  8, 24,  1, 21, 25, 24,  8, 18, 18, 23,  9, 20, 23, 33,
    29, 33, 20,  1, 19,  1, 29, 36,  9, 36, 19, 41, 28, 57, 32,  3,
    28,  3,  1, 27, 49, 49,  1, 32, 26, 26,  2,  4,  4,  7, 57, 41,
     2,  7, 10,  5, 37, 16, 10, 27,  8,  8, 13, 16, 37, 13,  1,  5 ],
        valtab: [
     0,   1,  -1,   1,  -1,   1,  -1,   1,  -1,   1,  -1,   1,   1,  -1,  -1,   1,
    -1,   1,  -1,   1,  -1,   1,  -1,   2,   1,  -2,  -1,   1,  -1,   1,   1,  -1,
    -1,   1,  -1,   1,  -1,   1,   0,  -1,   1,   1,   1,  -1,   1,  -1,  -1,  -1,
     1,   1,   2,  -1,  -1,   1,  -1,  -2,   1,   1,  -1,  -1,   1,   1,  -1,  -1,
     1,  -1,   3,   1,  -3,   2,  -1,   1,   1,  -2,  -1,  -1,  -1,   1,   1,   1,
     1,   1,  -1,  -1,  -1,   2,  -1,  -2,   1,   2,  -2,  -1,   1,   1,   2,  -1,
    -1,   1,  -2,  -1,   1,   1,  -1,   2,   1,   2,  -1,   1,  -2,  -1,  -2,  -1,
    -1,   1,   1,  -1,   1,  -1,   1,   1,   1,  -1,  -1,   1,   4,  -1,  -1,  -4,
     1,   1,   1,   2,  -1,  -1,   1,  -1,  -1,   1,  -1,  -1,   1,  -2,   1,  -1,
     1,   1,  -1,  -1,   1,   1,  -1,  -1,   3,   2,  -3,  -2,   2,   5,  -2,   2,
     2,  -5,  -2,  -2,  -2,   2,  -3,   3,   2,   3,  -3,   2,  -2,  -2,   3,  -3,
     6,   2,  -2,   3,  -6,   3,  -3,  -3,   3,   7,  -4,   4,  -3,   2,  -7,   2,
     2,  -2,  -4,   2,   8,  -2,  -2,  -2,   4,   2,  -2,   2,   3,   2,  -2,  -2,
     2,   2,  -2,  -8,  -2,   9,  -2,   2,  -3,  -2,   2,  -2,   2,   2,   2,   4,
    -2,  -4,  10,   2,   2,  -2,  -9,  -2,   2,  -2,   5,   4,  -4,   4,  -2,   2,
    -5,  -4,  -3,   4,   2,  -3,   3,  -2,  -5,   5,   3,   3,  -2,  -3, -10,  -4 ],
    },
    RVMap { eob_sym: 2, esc_sym: 11, runtab: [
     1,  1,  0,  2,  2,  1,  1,  3,  3,  4,  4,  0,  1,  1,  5,  5,
     2,  2,  6,  6,  7,  7,  1,  8,  1,  8,  3,  3,  9,  9,  1,  2,
     2,  1,  4, 10,  4, 10, 11, 11,  1,  5, 12, 12,  1,  5, 13, 13,
     3,  3,  6,  6,  2,  2, 14, 14, 16, 16, 15,  7, 15,  8,  8,  7,
     1,  1, 17, 17,  4,  4,  1,  1, 18, 18,  2,  2,  5,  5, 25,  3,
     9,  3, 25,  9, 19, 24, 19, 24,  1, 21, 20,  1, 21, 22, 20, 22,
    23, 23,  8,  6, 33,  6,  8, 33,  7,  7, 26, 26,  1, 32,  1, 32,
    28,  4, 28, 10, 29, 27, 27, 10, 41,  4, 29,  2,  2, 41, 36, 31,
    49, 31, 34, 30, 34, 36, 30, 35,  1, 49, 11,  5, 35, 11,  1,  3,
     3,  5, 37, 37,  8, 40,  8, 40, 12, 12, 42, 42,  1, 38, 16, 57,
     1,  6, 16, 39, 38,  6,  7,  7, 13, 13, 39, 43,  2, 43, 57,  2,
    50,  9, 44,  9, 50,  4, 15, 48, 44,  4,  1, 15, 48, 14, 14,  1,
    45, 45,  8,  3,  5,  8, 51, 47,  3, 46, 46, 47,  5, 51,  1, 17,
    17, 58,  1, 58,  2, 52, 52,  2, 53,  7, 59,  6,  6, 56, 53, 55,
     7, 55,  1, 54, 59, 56, 54, 10,  1, 10,  4, 60,  1, 60,  8,  4,
     8, 64, 64, 61,  1, 63,  3, 63, 62, 61,  5, 11,  5,  3, 11, 62 ],
        valtab: [
      1,  -1,   0,   1,  -1,   2,  -2,   1,  -1,   1,  -1,   0,   3,  -3,   1,  -1,
      2,  -2,   1,  -1,   1,  -1,   4,   1,  -4,  -1,   2,  -2,   1,  -1,   5,   3,
     -3,  -5,   2,   1,  -2,  -1,   1,  -1,   6,   2,   1,  -1,  -6,  -2,   1,  -1,
      3,  -3,   2,  -2,   4,  -4,   1,  -1,   1,  -1,   1,   2,  -1,   2,  -2,  -2,
      7,  -7,   1,  -1,   3,  -3,   8,  -8,   1,  -1,   5,  -5,   3,  -3,   1,   4,
      2,  -4,  -1,  -2,   1,   1,  -1,  -1,   9,   1,   1,  -9,  -1,   1,  -1,  -1,
      1,  -1,   3,  -3,   1,   3,  -3,  -1,   3,  -3,   1,  -1,  10,   1, -10,  -1,
      1,   4,  -1,   2,   1,  -1,   1,  -2,   1,  -4,  -1,   6,  -6,  -1,   1,   1,
      1,  -1,   1,   1,  -1,  -1,  -1,   1,  11,  -1,  -2,   4,  -1,   2, -11,   5,
     -5,  -4,  -1,   1,   4,   1,  -4,  -1,  -2,   2,   1,  -1,  12,   1,  -2,   1,
    -12,   4,   2,   1,  -1,  -4,   4,  -4,   2,  -2,  -1,   1,   7,  -1,  -1,  -7,
     -1,  -3,   1,   3,   1,   5,   2,   1,  -1,  -5,  13,  -2,  -1,   2,  -2, -13,
      1,  -1,   5,   6,   5,  -5,   1,   1,  -6,   1,  -1,  -1,  -5,  -1,  14,   2,
     -2,   1, -14,  -1,   8,   1,  -1,  -8,   1,   5,   1,   5,  -5,   1,  -1,   1,
     -5,  -1,  15,   1,  -1,  -1,  -1,   3, -15,  -3,   6,   1,  16,  -1,   6,  -6,
     -6,   1,  -1,   1, -16,   1,   7,  -1,   1,  -1,  -6,  -3,   6,  -7,   3,  -1 ]
    },
    RVMap { eob_sym: 0, esc_sym: 35, runtab: [
     0,  1,  1,  2,  2,  3,  3,  4,  4,  1,  1,  5,  5,  6,  6,  7,
     7,  8,  8,  9,  9,  2,  2, 10, 10,  1,  1, 11, 11, 12, 12,  3,
     3, 13, 13,  0, 14, 14, 16, 15, 16, 15,  4,  4, 17,  1, 17,  1,
     5,  5, 18, 18,  2,  2,  6,  6,  8, 19,  7,  8,  7, 19, 20, 20,
    21, 21, 22, 24, 22, 24, 23, 23,  1,  1, 25, 25,  3,  3, 26, 26,
     9,  9, 27, 27, 28, 28, 33, 29,  4, 33, 29,  1,  4,  1, 32, 32,
     2,  2, 31, 10, 30, 10, 30, 31, 34, 34,  5,  5, 36, 36, 35, 41,
    35, 11, 41, 11, 37,  1,  8,  8, 37,  6,  1,  6, 40,  7,  7, 40,
    12, 38, 12, 39, 39, 38, 49, 13, 49, 13,  3, 42,  3, 42, 16, 16,
    43, 43, 14, 14,  1,  1, 44, 15, 44, 15,  2,  2, 57, 48, 50, 48,
    57, 50,  4, 45, 45,  4, 46, 47, 47, 46,  1, 51,  1, 17, 17, 51,
     8,  9,  9,  5, 58,  8, 58,  5, 52, 52, 55, 56, 53, 56, 55, 59,
    59, 53, 54,  1,  6, 54,  7,  7,  6,  1,  2,  3,  2,  3, 64, 60,
    60, 10, 10, 64, 61, 62, 61, 63,  1, 63, 62,  1, 18, 24, 18,  4,
    25,  4,  8, 21, 21,  1, 24, 22, 25, 22,  8, 11, 19, 11, 23,  1,
    20, 23, 19, 20,  5, 12,  5,  1, 16,  2, 12, 13,  2, 13,  1, 16 ],
        valtab: [
      0,   1,  -1,   1,  -1,   1,  -1,   1,  -1,   2,  -2,   1,  -1,   1,  -1,   1,
     -1,   1,  -1,   1,  -1,   2,  -2,   1,  -1,   3,  -3,   1,  -1,   1,  -1,   2,
     -2,   1,  -1,   0,   1,  -1,   1,   1,  -1,  -1,   2,  -2,   1,   4,  -1,  -4,
      2,  -2,   1,  -1,  -3,   3,   2,  -2,   2,   1,   2,  -2,  -2,  -1,   1,  -1,
      1,  -1,   1,   1,  -1,  -1,   1,  -1,   5,  -5,   1,  -1,   3,  -3,   1,  -1,
      2,  -2,   1,  -1,   1,  -1,   1,   1,   3,  -1,  -1,   6,  -3,  -6,  -1,   1,
      4,  -4,   1,   2,   1,  -2,  -1,  -1,   1,  -1,   3,  -3,   1,  -1,   1,   1,
     -1,   2,  -1,  -2,   1,   7,  -3,   3,  -1,   3,  -7,  -3,   1,  -3,   3,  -1,
      2,   1,  -2,   1,  -1,  -1,   1,   2,  -1,  -2,  -4,  -1,   4,   1,   2,  -2,
      1,  -1,  -2,   2,   8,  -8,  -1,   2,   1,  -2,  -5,   5,   1,  -1,  -1,   1,
     -1,   1,   4,  -1,   1,  -4,  -1,  -1,   1,   1,   9,   1,  -9,   2,  -2,  -1,
     -4,   3,  -3,  -4,  -1,   4,   1,   4,   1,  -1,   1,  -1,   1,   1,  -1,   1,
     -1,  -1,  -1,  10,   4,   1,   4,  -4,  -4, -10,   6,   5,  -6,  -5,   1,  -1,
      1,   3,  -3,  -1,   1,  -1,  -1,  -1,  11,   1,   1, -11,  -2,  -2,   2,   5,
     -2,  -5,  -5,   2,  -2,  12,   2,  -2,   2,   2,   5,  -3,  -2,   3,  -2, -12,
     -2,   2,   2,   2,  -5,   3,   5,  13,  -3,   7,  -3,  -3,  -7,   3, -13,   3 ]
    },
    RVMap { eob_sym: 0, esc_sym: 34, runtab: [
     0,  1,  1,  1,  2,  2,  1,  3,  3,  1,  1,  1,  4,  4,  1,  5,
     2,  1,  5,  2,  1,  1,  6,  6,  1,  1,  1,  1,  1,  7,  3,  1,
     2,  3,  0,  1,  2,  7,  1,  1,  1,  8,  1,  1,  8,  1,  1,  1,
     9,  1,  9,  1,  2,  1,  1,  2,  1,  1, 10,  4,  1, 10,  1,  4,
     1,  1,  1,  1,  1,  3,  1,  1,  1,  3,  2,  1,  5,  1,  1,  1,
     2,  5,  1, 11,  1, 11,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     2,  1,  6,  1,  6,  1,  1,  2,  1,  1,  1,  1,  1,  1,  1, 12,
     3,  1, 12,  1,  1,  1,  2,  1,  1,  3,  1,  1,  1,  1,  1,  1,
     4,  1,  1,  1,  2,  1,  1,  4,  1,  1,  1,  1,  1,  1,  2,  1,
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  3,  1,  2,  1,  1,  5,
     1,  1,  1,  1,  1,  7,  1,  7,  1,  1,  2,  3,  1,  1,  1,  1,
     5,  1,  1,  1,  1,  1,  1,  2, 13,  1,  1,  1,  1,  1,  1,  1,
     1,  1,  1,  1,  1,  1,  1,  1, 13,  2,  1,  1,  4,  1,  1,  1,
     3,  1,  6,  1,  1,  1, 14,  1,  1,  1,  1,  1, 14,  6,  1,  1,
     1,  1, 15,  2,  4,  1,  2,  3, 15,  1,  1,  1,  8,  1,  1,  8,
     1,  1,  1,  1,  1,  1,  1,  1,  2,  1,  1,  1,  1,  1,  1,  1 ],
        valtab: [
      0,   1,  -1,   2,   1,  -1,  -2,   1,  -1,   3,  -3,   4,   1,  -1,  -4,   1,
      2,   5,  -1,  -2,  -5,   6,   1,  -1,  -6,   7,  -7,   8,  -8,   1,   2,   9,
      3,  -2,   0,  -9,  -3,  -1,  10, -10,  11,   1, -11,  12,  -1, -12,  13, -13,
      1,  14,  -1, -14,   4,  15, -15,  -4,  16, -16,   1,   2,  17,  -1, -17,  -2,
     18, -18,  19, -19,  20,   3, -20,  21, -21,  -3,   5,  22,   2, -22, -23,  23,
     -5,  -2,  24,   1, -24,  -1,  25, -25,  26, -26, -27,  27,  28,  29, -28, -29,
      6,  30,   2, -31,  -2, -30,  31,  -6, -32,  32,  33, -33,  34, -35, -34,   1,
      4, -36,  -1,  35,  37,  36,   7, -37,  38,  -4, -38,  39,  41,  40, -40, -39,
      3,  42, -43, -41,  -7, -42,  43,  -3,  44, -44,  45, -45,  46,  47,   8, -47,
    -48, -46,  50, -50,  48,  49,  51, -49,  52, -52,   5, -51,  -8, -53,  53,   3,
    -56,  56,  55,  54, -54,   2,  60,  -2, -55,  58,   9,  -5,  59,  57, -57, -63,
     -3, -58, -60, -61,  61, -59, -62,  -9,   1,  64,  62,  69, -64,  63,  65, -67,
    -68,  66, -65,  68, -66, -69,  67, -70,  -1,  10,  71, -71,   4,  73,  72,  70,
      6, -76,  -3,  74, -78, -74,   1,  78,  80, -72, -75,  76,  -1,   3, -73,  79,
     75,  77,   1,  11,  -4, -79, -10,  -6,  -1, -77, -83, -80,   2,  81, -84,  -2,
     83, -81,  82, -82,  84, -87, -86,  85, -11, -85,  86, -89,  87, -88,  88,  89 ]
    },
    RVMap { eob_sym: 2, esc_sym: 33, runtab: [
     1,  1,  0,  2,  1,  2,  1,  3,  3,  1,  1,  4,  4,  2,  2,  1,
     1,  5,  5,  6,  1,  6,  1,  7,  7,  3,  3,  2,  8,  2,  8,  1,
     1,  0,  9,  9,  1,  1, 10,  4, 10,  4, 11, 11,  2,  1,  2,  1,
    12, 12,  3,  3,  1,  1, 13,  5,  5, 13, 14,  1,  1, 14,  2,  2,
     6,  6, 15,  1,  1, 15, 16,  4,  7, 16,  4,  7,  1,  1,  3,  3,
     8,  8,  2,  2,  1,  1, 17, 17,  1,  1, 18, 18,  5,  5,  2,  2,
     1,  1,  9, 19,  9, 19, 20,  3,  3, 20,  1, 10, 21,  1, 10,  4,
     4, 21, 22,  6,  6, 22,  1,  1, 23, 24,  2,  2, 23, 24, 11,  1,
     1, 11,  7, 25,  7,  1,  1, 25,  8,  8,  3, 26,  3,  1, 12,  2,
     2, 26,  1, 12,  5,  5, 27,  4,  1,  4,  1, 27, 28,  1, 28, 13,
     1, 13,  2, 29,  2,  1, 32,  6,  1, 30, 14, 29, 14,  6,  3, 31,
     3,  1, 30,  1, 32, 31, 33,  9, 33,  1,  1,  7,  9,  7,  2,  2,
     1,  1,  4, 36, 34,  4,  5, 10, 10,  5, 34,  1,  1, 35,  8,  8,
    36,  3, 35,  1, 15,  3,  2,  1, 16, 15, 16,  2, 37,  1, 37,  1,
     1,  1,  6,  6, 38,  1, 38, 11,  1, 39, 39, 40, 11,  2, 41,  4,
    40,  1,  2,  4,  1,  1,  1, 41,  3,  1,  3,  1,  5,  7,  5,  7 ],
        valtab: [
      1,  -1,   0,   1,   2,  -1,  -2,   1,  -1,   3,  -3,   1,  -1,   2,  -2,   4,
     -4,   1,  -1,   1,   5,  -1,  -5,   1,  -1,   2,  -2,   3,   1,  -3,  -1,   6,
     -6,   0,   1,  -1,   7,  -7,   1,   2,  -1,  -2,   1,  -1,   4,   8,  -4,  -8,
      1,  -1,   3,  -3,   9,  -9,   1,   2,  -2,  -1,   1,  10, -10,  -1,   5,  -5,
      2,  -2,   1,  11, -11,  -1,   1,   3,   2,  -1,  -3,  -2,  12, -12,   4,  -4,
      2,  -2,  -6,   6,  13, -13,   1,  -1,  14, -14,   1,  -1,   3,  -3,   7,  -7,
     15, -15,   2,   1,  -2,  -1,   1,   5,  -5,  -1, -16,   2,   1,  16,  -2,   4,
     -4,  -1,   1,   3,  -3,  -1,  17, -17,   1,   1,  -8,   8,  -1,  -1,   2,  18,
    -18,  -2,   3,   1,  -3,  19, -19,  -1,   3,  -3,   6,   1,  -6,  20,   2,   9,
     -9,  -1, -20,  -2,   4,  -4,   1,  -5,  21,   5, -21,  -1,   1, -22,  -1,   2,
     22,  -2,  10,   1, -10,  23,   1,   4, -23,   1,   2,  -1,  -2,  -4,  -7,   1,
      7, -24,  -1,  24,  -1,  -1,   1,   3,  -1, -25,  25,   4,  -3,  -4,  11, -11,
     26, -26,   6,   1,   1,  -6,  -5,  -3,   3,   5,  -1, -27,  27,   1,   4,  -4,
     -1,  -8,  -1,  28,   2,   8, -12, -28,  -2,  -2,   2,  12,  -1,  29,   1, -29,
     30, -30,   5,  -5,   1, -31,  -1,   3,  31,  -1,   1,   1,  -3, -13,   1,  -7,
     -1, -32,  13,   7,  32,  33, -33,  -1,  -9, -34,   9,  34,  -6,   5,   6,  -5 ]
    },
    RVMap { eob_sym: 2, esc_sym: 13, runtab: [
     1,  1,  0,  1,  1,  2,  2,  1,  1,  3,  3,  1,  1,  0,  2,  2,
     4,  1,  4,  1,  1,  1,  5,  5,  1,  1,  6,  6,  2,  2,  1,  1,
     3,  3,  7,  7,  1,  1,  8,  8,  1,  1,  2,  2,  1,  9,  1,  9,
     4,  4, 10,  1,  1, 10,  1,  1, 11, 11,  3,  3,  1,  2,  1,  2,
     1,  1, 12, 12,  5,  5,  1,  1, 13,  1,  1, 13,  2,  2,  1,  1,
     6,  6,  1,  1,  4, 14,  4, 14,  3,  1,  3,  1,  1,  1, 15,  7,
    15,  2,  2,  7,  1,  1,  1,  8,  1,  8, 16, 16,  1,  1,  1,  1,
     2,  1,  1,  2,  1,  1,  3,  5,  5,  3,  4,  1,  1,  4,  1,  1,
    17, 17,  9,  1,  1,  9,  2,  2,  1,  1, 10, 10,  1,  6,  1,  1,
     6, 18,  1,  1, 18,  1,  1,  1,  2,  2,  3,  1,  3,  1,  1,  1,
     4,  1, 19,  1, 19,  7,  1,  1, 20,  1,  4, 20,  1,  7, 11,  2,
     1, 11, 21,  2,  8,  5,  1,  8,  1,  5, 21,  1,  1,  1, 22,  1,
     1, 22,  1,  1,  3,  3,  1, 23,  2, 12, 24,  1,  1,  2,  1,  1,
    12, 23,  1,  1, 24,  1,  1,  1,  4,  1,  1,  1,  2,  1,  6,  6,
     4,  2,  1,  1,  1,  1,  1,  1,  1, 14, 13,  3,  1, 25,  9, 25,
    14,  1,  9,  3, 13,  1,  1,  1,  1,  1, 10,  1,  1,  2, 10,  2 ],
        valtab: [
     -20,  -1,   0,   2,  -2,   1,  -1,   3,  -3,   1,  -1,   4,  -4,   0,   2,  -2,
       1,   5,  -1,  -5,   6,  -6,   1,  -1,   7,  -7,   1,  -1,   3,  -3,   8,  -8,
       2,  -2,   1,  -1,   9,  -9,   1,  -1,  10, -10,   4,  -4,  11,   1, -11,  -1,
       2,  -2,   1,  12, -12,  -1,  13, -13,   1,  -1,   3,  -3,  14,   5, -14,  -5,
     -15,  15,  -1,   1,   2,  -2,  16, -16,   1,  17, -17,  -1,   6,  -6,  18, -18,
       2,  -2, -19,  19,  -3,   1,   3,  -1,   4,  20,  -4,   1, -21,  21,   1,   2,
      -1,  -7,   7,  -2,  22, -22,  23,   2, -23,  -2,   1,  -1, -24,  24, -25,  25,
      -8, -26,  26,   8, -27,  27,   5,   3,  -3,  -5,  -4,  28, -28,   4,  29, -29,
       1,  -1,  -2, -30,  30,   2,   9,  -9, -31,  31,   2,  -2, -32,   3,  32, -33,
      -3,   1,  33, -34,  -1,  34, -35,  35, -10,  10,  -6,  36,   6, -36,  37, -37,
      -5,  38,   1, -38,  -1,   3,  39, -39,  -1,  40,   5,   1, -40,  -3,   2, -11,
     -41,  -2,   1,  11,  -3,  -4,  41,   3,  42,   4,  -1, -43, -42,  43,   1, -44,
      45,  -1,  44, -45,  -7,   7, -46,   1, -12,   2,   1, -47,  46,  12,  47,  48,
      -2,  -1, -48,  49,  -1, -50, -49,  50,  -6, -51,  51,  52, -13,  53,  -4,   4,
       6,  13, -53, -52, -54,  55,  54, -55, -56,  -2,   2,  -8,  56,   1,  -3,  -1,
       2,  58,   3,   8,  -2,  57, -58, -60, -59, -57,  -3,  60,  59, -14,   3,  14 ]
    },
    RVMap { eob_sym: 2, esc_sym: 38, runtab: [
     1,  1,  0,  2,  2,  1,  1,  3,  3,  4,  4,  5,  5,  1,  1,  6,
     6,  2,  2,  7,  7,  8,  8,  1,  1,  3,  3,  9,  9, 10, 10,  1,
     1,  2,  2,  4,  4, 11,  0, 11, 12, 12, 13, 13,  1,  1,  5,  5,
    14, 14, 15, 16, 15, 16,  3,  3,  1,  6,  1,  6,  2,  2,  7,  7,
     8,  8, 17, 17,  1,  1,  4,  4, 18, 18,  2,  2,  1, 19,  1, 20,
    19, 20, 21, 21,  3,  3, 22, 22,  5,  5, 24,  1,  1, 23,  9, 23,
    24,  9,  2,  2, 10,  1,  1, 10,  6,  6, 25,  4,  4, 25,  7,  7,
    26,  8,  1,  8,  3,  1, 26,  3, 11, 11, 27, 27,  2, 28,  1,  2,
    28,  1, 12, 12,  5,  5, 29, 13, 13, 29, 32,  1,  1, 33, 31, 30,
    32,  4, 30, 33,  4, 31,  3, 14,  1,  1,  3, 34, 34,  2,  2, 14,
     6,  6, 35, 36, 35, 36,  1, 15,  1, 16, 16, 15,  7,  9,  7,  9,
    37,  8,  8, 37,  1,  1, 39,  2, 38, 39,  2, 40,  5, 38, 40,  5,
     3,  3,  4,  4, 10, 10,  1,  1,  1,  1, 41,  2, 41,  2,  6,  6,
     1,  1, 11, 42, 11, 43,  3, 42,  3, 17,  4, 43,  1, 17,  7,  1,
     8, 44,  4,  7, 44,  5,  8,  2,  5,  1,  2, 48, 45,  1, 12, 45,
    12, 48, 13, 13,  1,  9,  9, 46,  1, 46, 47, 47, 49, 18, 18, 49 ],
        valtab: [
      1,  -1,   0,   1,  -1,   2,  -2,   1,  -1,   1,  -1,   1,  -1,   3,  -3,   1,
     -1,  -2,   2,   1,  -1,   1,  -1,   4,  -4,  -2,   2,   1,  -1,   1,  -1,   5,
     -5,  -3,   3,   2,  -2,   1,   0,  -1,   1,  -1,   1,  -1,   6,  -6,   2,  -2,
      1,  -1,   1,   1,  -1,  -1,  -3,   3,   7,   2,  -7,  -2,  -4,   4,   2,  -2,
      2,  -2,   1,  -1,   8,  -8,   3,  -3,   1,  -1,  -5,   5,   9,   1,  -9,   1,
     -1,  -1,   1,  -1,  -4,   4,   1,  -1,   3,  -3,   1, -10,  10,   1,   2,  -1,
     -1,  -2,   6,  -6,   2,  11, -11,  -2,   3,  -3,   1,  -4,   4,  -1,   3,  -3,
      1,   3,  12,  -3,  -5, -12,  -1,   5,   2,  -2,   1,  -1,  -7,   1,  13,   7,
     -1, -13,   2,  -2,   4,  -4,   1,   2,  -2,  -1,   1,  14, -14,   1,   1,   1,
     -1,  -5,  -1,  -1,   5,  -1,  -6,   2, -15,  15,   6,   1,  -1,  -8,   8,  -2,
     -4,   4,   1,   1,  -1,  -1,  16,   2, -16,  -2,   2,  -2,   4,   3,  -4,  -3,
     -1,  -4,   4,   1, -17,  17,  -1,  -9,   1,   1,   9,   1,  -5,  -1,  -1,   5,
     -7,   7,   6,  -6,   3,  -3,  18, -18,  19, -19,   1, -10,  -1,  10,  -5,   5,
     20, -20,  -3,   1,   3,   1,   8,  -1,  -8,   2,   7,  -1, -21,  -2,   5,  21,
      5,  -1,  -7,  -5,   1,  -6,  -5, -11,   6,  22,  11,   1,   1, -22,  -3,  -1,
      3,  -1,   3,  -3, -23,   4,  -4,   1,  23,  -1,   1,  -1,   1,  -2,   2,  -1 ]
    },
    RVMap { eob_sym: 4, esc_sym: 11, runtab: [
     1,  1,  1,  1,  0,  2,  2,  1,  1,  3,  3,  0,  1,  1,  2,  2,
     4,  4,  1,  1,  5,  5,  1,  1,  2,  2,  3,  3,  6,  6,  1,  1,
     7,  7,  8,  1,  8,  2,  2,  1,  4,  4,  1,  3,  1,  3,  9,  9,
     2,  2,  1,  5,  1,  5, 10, 10,  1,  1, 11, 11,  3,  6,  3,  4,
     4,  6,  2,  2,  1, 12,  1, 12,  7, 13,  7, 13,  1,  1,  8,  8,
     2,  2, 14, 14, 16, 15, 16,  5,  5,  1,  3, 15,  1,  3,  4,  4,
     1,  1, 17, 17,  2,  2,  6,  6,  1, 18,  1, 18, 22, 21, 22, 21,
    25, 24, 25, 19,  9, 20,  9, 23, 19, 24, 20,  3, 23,  7,  3,  1,
     1,  7, 28, 26, 29,  5, 28, 26,  5,  8, 29,  4,  8, 27,  2,  2,
     4, 27,  1,  1, 10, 36, 10, 33, 33, 36, 30,  1, 32, 32,  1, 30,
     6, 31, 31, 35,  3,  6, 11, 11,  3,  2, 35,  2, 34,  1, 34,  1,
    37, 37, 12,  7, 12,  5, 41,  5,  4,  7,  1,  8, 13,  4,  1, 41,
    13, 38,  8, 38,  9,  1, 40, 40,  9,  1, 39,  2,  2, 49, 39, 42,
     3,  3, 14, 16, 49, 14, 16, 42, 43, 43,  6,  6, 15,  1,  1, 15,
    44, 44,  1,  1, 50, 48,  4,  5,  4,  7,  5,  2, 10, 10, 48,  7,
    50, 45,  2,  1, 45,  8,  8,  1, 46, 46,  3, 47, 47,  3,  1,  1 ],
        valtab: [
      1,  -1,   2,  -2,   0,   1,  -1,   3,  -3,   1,  -1,   0,   4,  -4,   2,  -2,
      1,  -1,   5,  -5,   1,  -1,   6,  -6,   3,  -3,   2,  -2,   1,  -1,   7,  -7,
      1,  -1,   1,   8,  -1,   4,  -4,  -8,   2,  -2,   9,   3,  -9,  -3,   1,  -1,
      5,  -5,  10,   2, -10,  -2,   1,  -1,  11, -11,   1,  -1,  -4,   2,   4,   3,
     -3,  -2,   6,  -6,  12,   1, -12,  -1,   2,   1,  -2,  -1,  13, -13,   2,  -2,
      7,  -7,   1,  -1,   1,   1,  -1,   3,  -3,  14,   5,  -1, -14,  -5,   4,  -4,
     15, -15,   1,  -1,   8,  -8,  -3,   3,  16,   1, -16,  -1,   1,   1,  -1,  -1,
      1,   1,  -1,   1,   2,   1,  -2,   1,  -1,  -1,  -1,   6,  -1,   3,  -6,  17,
    -17,  -3,   1,   1,   1,   4,  -1,  -1,  -4,   3,  -1,   5,  -3,  -1,  -9,   9,
     -5,   1,  18, -18,   2,   1,  -2,   1,  -1,  -1,   1,  19,  -1,   1, -19,  -1,
      4,   1,  -1,   1,   7,  -4,  -2,   2,  -7,  10,  -1, -10,   1,  20,  -1, -20,
      1,  -1,   2,   4,  -2,   5,   1,  -5,   6,  -4,  21,   4,   2,  -6, -21,  -1,
     -2,   1,  -4,  -1,  -3,  22,  -1,   1,   3, -22,  -1,  11, -11,   1,   1,   1,
      8,  -8,   2,   2,  -1,  -2,  -2,  -1,   1,  -1,  -5,   5,   2,  23, -23,  -2,
      1,  -1,  24, -24,  -1,  -1,   7,   6,  -7,   5,  -6,  12,  -3,   3,   1,  -5,
      1,   1, -12,  25,  -1,  -5,   5, -25,  -1,   1,   9,   1,  -1,  -9,  26, -26 ]
    }
];
