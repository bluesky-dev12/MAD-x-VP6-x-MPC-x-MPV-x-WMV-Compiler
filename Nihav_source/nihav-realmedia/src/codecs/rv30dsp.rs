use nihav_core::frame::{FrameType, NAVideoBuffer};
use nihav_codec_support::codecs::MV;
use nihav_codec_support::codecs::blockdsp::edge_emu;
use super::rv3040::{RV34DSP, RV34MBInfo};

fn clip8(a: i16) -> u8 {
    if a < 0 { 0 }
    else if a > 255 { 255 }
    else { a as u8 }
}

fn rv3_filter_h(dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize, bsize: usize, c1: i16, c2: i16) {
    for _ in 0..bsize {
        for x in 0..bsize {
            dst[didx + x] = clip8((-((src[sidx + x - 1] as i16) + (src[sidx + x + 2] as i16)) + (src[sidx + x + 0] as i16) * c1 + (src[sidx + x + 1] as i16) * c2 + 8) >> 4);
        }
        sidx += sstride;
        didx += dstride;
    }
}

fn rv3_filter_v(dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize, bsize: usize, c1: i16, c2: i16) {
    for _ in 0..bsize {
        for x in 0..bsize {
            dst[didx + x] = clip8((-((src[sidx + x - 1 * sstride] as i16) + (src[sidx + x + 2 * sstride] as i16)) + (src[sidx + x + 0 * sstride] as i16) * c1 + (src[sidx + x + 1 * sstride] as i16) * c2 + 8) >> 4);
        }
        sidx += sstride;
        didx += dstride;
    }
}

macro_rules! mc_matrix {
    ($s: ident, $o: expr, $c1: expr) => (
            ($c1 * 6) * ($s[$o] as i32) + ($c1 * 9) * ($s[$o + 1] as i32) + ($c1) * ($s[$o + 2] as i32)
        );
    ($s: ident, $o: expr, $c1: expr, $d1: expr, $d2: expr) => (
            -($c1) * ($s[$o - 1] as i32) + ($c1 * $d1) * ($s[$o] as i32) + ($c1 * $d2) * ($s[$o + 1] as i32) + -($c1) * ($s[$o + 2] as i32)
        );
    ($s: ident, $o: expr, $ss: expr, $c1: expr, $c2: expr, $d1: expr, $d2: expr) => (
        ((mc_matrix!($s, $o -     $ss,  -1, $d1, $d2) +
          mc_matrix!($s, $o          , $c1, $d1, $d2) +
          mc_matrix!($s, $o +     $ss, $c2, $d1, $d2) +
          mc_matrix!($s, $o + 2 * $ss,  -1, $d1, $d2) + 128) >> 8) as i16
        );
    (m22; $s: ident, $o: expr, $ss: expr) => (
        ((mc_matrix!($s, $o + 0 * $ss, 6) +
          mc_matrix!($s, $o + 1 * $ss, 9) +
          mc_matrix!($s, $o + 2 * $ss, 1) + 128) >> 8) as i16
        );
}

macro_rules! mc_func {
    (copy; $name: ident, $size: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            for _ in 0..$size {
                let d = &mut dst[didx..][..$size];
                let s = &src[sidx..][..$size];
                for x in 0..$size { d[x] = s[x]; }
                didx += dstride;
                sidx += sstride;
            }
        }
        );
    (hor; $name: ident, $c1: expr, $c2: expr, $size: expr) => (
        fn $name (dst: &mut [u8], didx: usize, dstride: usize, src: &[u8], sidx: usize, sstride: usize) {
            rv3_filter_h(dst, didx, dstride, src, sidx, sstride, $size, $c1, $c2);
        }
        );
    (ver; $name: ident, $c1: expr, $c2: expr, $size: expr) => (
        fn $name (dst: &mut [u8], didx: usize, dstride: usize, src: &[u8], sidx: usize, sstride: usize) {
            rv3_filter_v(dst, didx, dstride, src, sidx, sstride, $size, $c1, $c2);
        }
        );
    (m11; $name: ident, $size: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            for _ in 0..$size {
                for x in 0..$size {
                    dst[didx + x] = clip8(mc_matrix!(src, sidx + x, sstride, 12, 6, 12, 6));
                }
                didx += dstride;
                sidx += sstride;
            }
        }
        );
    (m12; $name: ident, $size: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            for _ in 0..$size {
                for x in 0..$size {
                    dst[didx + x] = clip8(mc_matrix!(src, sidx + x, sstride, 6, 12, 12, 6));
                }
                didx += dstride;
                sidx += sstride;
            }
        }
        );
    (m21; $name: ident, $size: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            for _ in 0..$size {
                for x in 0..$size {
                    dst[didx + x] = clip8(mc_matrix!(src, sidx + x, sstride, 12, 6, 6, 12));
                }
                didx += dstride;
                sidx += sstride;
            }
        }
        );
    (m22; $name: ident, $size: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            for _ in 0..$size {
                for x in 0..$size {
                    dst[didx + x] = clip8(mc_matrix!(m22; src, sidx + x, sstride));
                }
                didx += dstride;
                sidx += sstride;
            }
        }
        );
}
mc_func!(copy; copy_16, 16);
mc_func!(copy; copy_8,   8);
mc_func!(hor;  luma_mc_10_16, 12, 6, 16);
mc_func!(hor;  luma_mc_20_16, 6, 12, 16);
mc_func!(hor;  luma_mc_10_8,  12, 6,  8);
mc_func!(hor;  luma_mc_20_8,  6, 12,  8);
mc_func!(ver;  luma_mc_01_16, 12, 6, 16);
mc_func!(ver;  luma_mc_02_16, 6, 12, 16);
mc_func!(ver;  luma_mc_01_8,  12, 6,  8);
mc_func!(ver;  luma_mc_02_8,  6, 12,  8);
mc_func!(m11;  luma_mc_11_16,        16);
mc_func!(m11;  luma_mc_11_8,          8);
mc_func!(m21;  luma_mc_21_16,        16);
mc_func!(m21;  luma_mc_21_8,          8);
mc_func!(m12;  luma_mc_12_16,        16);
mc_func!(m12;  luma_mc_12_8,          8);
mc_func!(m22;  luma_mc_22_16,        16);
mc_func!(m22;  luma_mc_22_8,          8);

const RV30_CHROMA_FRAC1: [u16; 3] = [ 8, 5, 3 ];
const RV30_CHROMA_FRAC2: [u16; 3] = [ 0, 3, 5 ];
fn rv30_chroma_mc(dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize, size: usize, x: usize, y: usize) {
    if (x == 0) && (y == 0) {
        for _ in 0..size {
            dst[didx..][..size].copy_from_slice(&src[sidx..][..size]);
            didx += dstride;
            sidx += sstride;
        }
        return;
    }
    let a = RV30_CHROMA_FRAC1[x] * RV30_CHROMA_FRAC1[y];
    let b = RV30_CHROMA_FRAC2[x] * RV30_CHROMA_FRAC1[y];
    let c = RV30_CHROMA_FRAC1[x] * RV30_CHROMA_FRAC2[y];
    let d = RV30_CHROMA_FRAC2[x] * RV30_CHROMA_FRAC2[y];
    for _ in 0..size {
        for x in 0..size {
            dst[didx + x] = ((a * (src[sidx + x] as u16)
                            + b * (src[sidx + x + 1] as u16)
                            + c * (src[sidx + x + sstride] as u16)
                            + d * (src[sidx + x + 1 + sstride] as u16) + 32) >> 6) as u8;
        }
        didx += dstride;
        sidx += sstride;
    }
}

#[allow(clippy::type_complexity)]
pub struct RV30DSP {
    luma_mc: [[fn (&mut [u8], usize, usize, &[u8], usize, usize); 9]; 2],
}

impl RV30DSP {
    pub fn new() -> Self {
        RV30DSP {
            luma_mc: [
                    [ copy_16,       luma_mc_10_16,  luma_mc_20_16,
                      luma_mc_01_16, luma_mc_11_16,  luma_mc_21_16,
                      luma_mc_02_16, luma_mc_12_16,  luma_mc_22_16 ],
                    [ copy_8,        luma_mc_10_8,   luma_mc_20_8,
                      luma_mc_01_8,  luma_mc_11_8,   luma_mc_21_8,
                      luma_mc_02_8,  luma_mc_12_8,   luma_mc_22_8  ] ],
        }
    }
}

macro_rules! el {
    ($src: ident, $o: expr) => ($src[$o] as i16);
}

fn clip_symm(a: i16, lim: i16) -> i16 {
    if a < -lim {
        -lim
    } else if a > lim {
        lim
    } else {
        a
    }
}

const RV30_LOOP_FILTER_STRENGTH: [i16; 32] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5
];

macro_rules! test_bit {
    ($pat: expr, $x: expr) => ( (($pat >> $x) & 1) != 0 )
}

fn rv30_loop_filter4(pix: &mut [u8], mut off: usize, step: usize, stride: usize, lim: i16) {
    for _ in 0..4 {
        let a = el!(pix, off - 2*step);
        let b = el!(pix, off -   step);
        let c = el!(pix, off);
        let d = el!(pix, off +   step);
        let diff0 = ((a - d) - (b - c) * 4) >> 3;
        let diff = clip_symm(diff0, lim);
        pix[off - step] = clip8(b + diff);
        pix[off       ] = clip8(c - diff);
        off += stride;
    }
}

fn rv30_div_mv(mv: i16) -> (i16, usize) {
    let i = mv / 3;
    let f = mv - i * 3;
    if f < 0 {
        (i - 1, (f + 3) as usize)
    } else {
        (i, f as usize)
    }
}

fn check_pos(x: usize, y: usize, size: usize, w: usize, h: usize, dx: i16, dy: i16, e0: isize, e1: isize, e2: isize, e3: isize) -> bool {
    let xn = (x as isize) + (dx as isize);
    let yn = (y as isize) + (dy as isize);

    (xn - e0 >= 0) && (xn + (size as isize) + e1 <= (w as isize)) && (yn - e2 >= 0) && (yn + (size as isize) + e3 <= (h as isize))
}

const RV30_EDGE1: [isize; 3] = [ 0, 1, 1 ];
const RV30_EDGE2: [isize; 3] = [ 0, 2, 2 ];

impl RV34DSP for RV30DSP {
    #[allow(clippy::cognitive_complexity)]
    fn loop_filter(&self, frame: &mut NAVideoBuffer<u8>, _ftype: FrameType, mbinfo: &[RV34MBInfo], mb_w: usize, _mb_h: usize, row: usize) {
        let mut offs:   [usize; 3] = [0; 3];
        let mut stride: [usize; 3] = [0; 3];

        for comp in 0..3 {
            stride[comp] = frame.get_stride(comp);
            let start = if comp == 0 { row * 16 } else { row * 8 };
            offs[comp] = frame.get_offset(comp) + start * stride[comp];
        }

        let data = frame.get_data_mut().unwrap();
        let dst: &mut [u8] = data.as_mut_slice();

        // vertical filter
        let mut left_cbp = 0;
        let mut left_lim = 0;
        let mut left_dbk = 0;
        let mut mb_pos: usize = row * mb_w;
        for mb_x in 0..mb_w {
            let cur_lim = RV30_LOOP_FILTER_STRENGTH[mbinfo[mb_pos].q as usize];
            let cur_dbk = mbinfo[mb_pos].deblock;
            let cur_cbp = mbinfo[mb_pos].cbp_c;
            let xstart = if mb_x == 0 { 1 } else { 0 };
            for y in 0..4 {
                let yoff = offs[0] + mb_x * 16 + y * 4 * stride[0];
                for x in xstart..4 {
                    let cs = x + y*4;
                    let loc_lim;

                    if test_bit!(cur_dbk, cs) {
                        loc_lim = cur_lim;
                    } else if (x == 0) && test_bit!(left_dbk, cs + 3) {
                        loc_lim = left_lim;
                    } else if (x != 0) && test_bit!(cur_dbk,  cs - 1) {
                        loc_lim = cur_lim;
                    } else {
                        loc_lim = 0;
                    }
                    if loc_lim != 0 {
                        rv30_loop_filter4(dst, yoff + x * 4, 1, stride[0], loc_lim);
                    }
                }
            }

            for comp in 1..3 {
                for y in 0..2 {
                    let coff = offs[comp] + mb_x * 8 + y * 4 * stride[comp];
                    for x in xstart..2 {
                        let cs = x + y * 2 + (comp - 1) * 4;
                        let loc_lim;

                        if test_bit!(cur_cbp, cs) {
                            loc_lim = cur_lim;
                        } else if (x == 0) && test_bit!(left_cbp, cs + 1) {
                            loc_lim = left_lim;
                        } else if (x != 0) && test_bit!(cur_cbp,  cs - 1) {
                            loc_lim = cur_lim;
                        } else {
                            loc_lim = 0;
                        }
                        if loc_lim != 0 {
                            rv30_loop_filter4(dst, coff + x * 4, 1, stride[comp], loc_lim);
                        }
                    }
                }
            }

            left_lim = cur_lim;
            left_dbk = cur_dbk;
            left_cbp = cur_cbp;
            mb_pos += 1;
        }

        // horizontal filter
        let mut mb_pos: usize = row * mb_w;
        for mb_x in 0..mb_w {
            let cur_lim = RV30_LOOP_FILTER_STRENGTH[mbinfo[mb_pos].q as usize];
            let cur_dbk = mbinfo[mb_pos].deblock;
            let cur_cbp = mbinfo[mb_pos].cbp_c;
            let ystart = if row == 0 { 1 } else { 0 };
            let top_lim;
            let top_dbk;
            let top_cbp;
            if row > 0 {
                top_lim = RV30_LOOP_FILTER_STRENGTH[mbinfo[mb_pos - mb_w].q as usize];
                top_dbk = mbinfo[mb_pos - mb_w].deblock;
                top_cbp = mbinfo[mb_pos - mb_w].cbp_c;
            } else {
                top_lim = 0;
                top_dbk = 0;
                top_cbp = 0;
            }
            for y in ystart..4 {
                let yoff = offs[0] + mb_x * 16 + y * 4 * stride[0];
                for x in 0..4 {
                    let cs = x + y*4;
                    let loc_lim;

                    if test_bit!(cur_dbk, cs) {
                        loc_lim = cur_lim;
                    } else if (y == 0) && test_bit!(top_dbk, cs + 12) {
                        loc_lim = top_lim;
                    } else if (y != 0) && test_bit!(cur_dbk, cs - 4) {
                        loc_lim = cur_lim;
                    } else {
                        loc_lim = 0;
                    }
                    if loc_lim != 0 {
                        rv30_loop_filter4(dst, yoff + x * 4, stride[0], 1, loc_lim);
                    }
                }
            }

            for comp in 1..3 {
                for y in ystart..2 {
                    let coff = offs[comp] + mb_x * 8 + y * 4 * stride[comp];
                    for x in 0..2 {
                        let cs = x + y * 2 + (comp - 1) * 4;
                        let loc_lim;

                        if test_bit!(cur_cbp, cs) {
                            loc_lim = cur_lim;
                        } else if (y == 0) && test_bit!(top_cbp, cs + 2) {
                            loc_lim = top_lim;
                        } else if (y != 0) && test_bit!(cur_cbp, cs - 2) {
                            loc_lim = cur_lim;
                        } else {
                            loc_lim = 0;
                        }
                        if loc_lim != 0 {
                            rv30_loop_filter4(dst, coff + x * 4, stride[comp], 1, loc_lim);
                        }
                    }
                }
            }

            mb_pos += 1;
        }
    }
    fn do_luma_mc(&self, frame: &mut NAVideoBuffer<u8>, prev_frame: &NAVideoBuffer<u8>, x: usize, y: usize, mv: MV, use16: bool, avg: bool) {
        let size: usize = if use16 { 16 } else { 8 };
        let dstride = frame.get_stride(0);
        let doffset = frame.get_offset(0) + (if !avg { x + y * dstride } else { 0 });
        let data = frame.get_data_mut().unwrap();
        let dst: &mut [u8] = data.as_mut_slice();

        let (w_, h_) = prev_frame.get_dimensions(0);
        let w = (w_ + 15) & !15;
        let h = (h_ + 15) & !15;

        let (dx, cx) = rv30_div_mv(mv.x);
        let (dy, cy) = rv30_div_mv(mv.y);
        let mode = cx + cy * 3;

        if check_pos(x, y, size, w, h, dx, dy, RV30_EDGE1[cx], RV30_EDGE2[cx], RV30_EDGE1[cy], RV30_EDGE2[cy]) {
            let sstride = prev_frame.get_stride(0);
            let mut soffset = prev_frame.get_offset(0) + x + y * sstride;
            let data = prev_frame.get_data();
            let src: &[u8] = data.as_slice();
            soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
            self.luma_mc[if use16 { 0 } else { 1 }][mode](dst, doffset, dstride, src, soffset, sstride);
        } else {
            let mut ebuf: [u8; 32*20] = [0; 32*20];
            edge_emu(prev_frame, (x as isize) + (dx as isize) - 1, (y as isize) + (dy as isize) - 1, 16+3, 16+3, &mut ebuf, 32, 0, 4);
            self.luma_mc[if use16 { 0 } else { 1 }][mode](dst, doffset, dstride, &ebuf, 32 + 1, 32);
        }
    }
    fn do_chroma_mc(&self, frame: &mut NAVideoBuffer<u8>, prev_frame: &NAVideoBuffer<u8>, x: usize, y: usize, comp: usize, mv: MV, use8: bool, avg: bool) {
        let size: usize = if use8 { 8 } else { 4 };
        let dstride = frame.get_stride(comp);
        let doffset = frame.get_offset(comp) + (if !avg { x + y * dstride } else { 0 });
        let data = frame.get_data_mut().unwrap();
        let dst: &mut [u8] = data.as_mut_slice();

        let (w_, h_) = prev_frame.get_dimensions(comp);
        let w = (w_ + 7) & !7;
        let h = (h_ + 7) & !7;

        let (dx, cx) = rv30_div_mv(mv.x / 2);
        let (dy, cy) = rv30_div_mv(mv.y / 2);

        if check_pos(x, y, size, w, h, dx, dy, 0, 1, 0, 1) {
            let sstride = prev_frame.get_stride(comp);
            let mut soffset = prev_frame.get_offset(comp) + x + y * sstride;
            let data = prev_frame.get_data();
            let src: &[u8] = data.as_slice();
            soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
            rv30_chroma_mc(dst, doffset, dstride, src, soffset, sstride, size, cx, cy);
        } else {
            let mut ebuf: [u8; 16*10] = [0; 16*10];
            edge_emu(prev_frame, (x as isize) + (dx as isize), (y as isize) + (dy as isize), 8+1, 8+1, &mut ebuf, 16, comp, 4);
            rv30_chroma_mc(dst, doffset, dstride, &ebuf, 0, 16, size, cx, cy);
        }
    }
}
