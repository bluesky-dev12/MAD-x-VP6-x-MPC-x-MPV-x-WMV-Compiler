use nihav_core::frame::{NAVideoBuffer, NASimpleVideoFrame};
use nihav_codec_support::codecs::MV;
use nihav_codec_support::codecs::blockdsp::edge_emu;

fn clip8(val: i16) -> u8 { val.min(255).max(0) as u8 }

macro_rules! el {
    ($s: ident, $o: expr) => ( $s[$o] as i16 )
}

macro_rules! filter {
    (01; $s: ident, $o: expr, $step: expr) => (
            clip8(((      el!($s, $o - 2 * $step)
                     -5 * el!($s, $o - 1 * $step)
                    +52 * el!($s, $o - 0 * $step)
                    +20 * el!($s, $o + 1 * $step)
                     -5 * el!($s, $o + 2 * $step)
                        + el!($s, $o + 3 * $step) + 32) >> 6) as i16)
        );
    (02; $s: ident, $o: expr, $step: expr) => (
            clip8(((      el!($s, $o - 2 * $step)
                     -5 * el!($s, $o - 1 * $step)
                    +20 * el!($s, $o - 0 * $step)
                    +20 * el!($s, $o + 1 * $step)
                     -5 * el!($s, $o + 2 * $step)
                        + el!($s, $o + 3 * $step) + 16) >> 5) as i16)
        );
    (03; $s: ident, $o: expr, $step: expr) => (
            clip8(((      el!($s, $o - 2 * $step)
                     -5 * el!($s, $o - 1 * $step)
                    +20 * el!($s, $o - 0 * $step)
                    +52 * el!($s, $o + 1 * $step)
                     -5 * el!($s, $o + 2 * $step)
                        + el!($s, $o + 3 * $step) + 32) >> 6) as i16)
        );
}

macro_rules! filter_row {
    ($d: ident, $do: expr, $s: ident, $so: expr, $step: expr, $size: expr, $mode: expr) => ({
            match $mode {
                1 => {
                        for x in 0..$size {
                            $d[$do + x] = filter!(01; $s, $so + x, $step);
                        }
                    },
                2 => {
                        for x in 0..$size {
                            $d[$do + x] = filter!(02; $s, $so + x, $step);
                        }
                    },
                3 => {
                        for x in 0..$size {
                            $d[$do + x] = filter!(03; $s, $so + x, $step);
                        }
                    },
                _ => {},
            };
        });
}

#[allow(clippy::cognitive_complexity)]
fn luma_mc(dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize, w: usize, h: usize, cx: usize, cy: usize) {
    if (cx == 0) && (cy == 0) {
        for _ in 0..h {
            dst[didx..][..w].copy_from_slice(&src[sidx..][..w]);
            didx += dstride;
            sidx += sstride;
        }
    } else if cy == 0 {
        for _ in 0..h {
            filter_row!(dst, didx, src, sidx, 1, w, cx);
            didx += dstride;
            sidx += sstride;
        }
    } else if cx == 0 {
        for _ in 0..h {
            filter_row!(dst, didx, src, sidx, sstride, w, cy);
            didx += dstride;
            sidx += sstride;
        }
    } else if (cx != 3) || (cy != 3) {
        let mut tmp: [u8; 70 * 64] = [0; 70 * 64];
        for y in 0..h+5 {
            filter_row!(tmp, y * 64, src, sidx - sstride * 2, 1, w, cx);
            sidx += sstride;
        }
        for y in 0..h {
            filter_row!(dst, didx, tmp, (y + 2) * 64, 64, w, cy);
            didx += dstride;
        }
    } else {
        for _ in 0..h {
            for x in 0..w {
                dst[didx + x] = ((el!(src, sidx + x) + el!(src, sidx + x + 1) +
                                  el!(src, sidx + x + sstride) + el!(src, sidx + x + 1 + sstride) + 2) >> 2) as u8;
            }
            didx += dstride;
            sidx += sstride;
        }
    }
}

fn chroma_mc(dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize, w: usize, h: usize, x: usize, y: usize) {
    if (x == 0) && (y == 0) {
        for _ in 0..h {
            dst[didx..][..w].copy_from_slice(&src[sidx..][..w]);
            didx += dstride;
            sidx += sstride;
        }
        return;
    }
    if (x > 0) && (y > 0) {
        // 3,3 case is the same as 3,2 for some reason
        let ymod = if (x == 3) && (y == 3) { 2 } else { y };
        let a = ((4 - x) * (4 - ymod)) as u16;
        let b = ((    x) * (4 - ymod)) as u16;
        let c = ((4 - x) * (    ymod)) as u16;
        let d = ((    x) * (    ymod)) as u16;
        for _ in 0..h {
            for x in 0..w {
                dst[didx + x] = ((a * (src[sidx + x] as u16)
                                + b * (src[sidx + x + 1] as u16)
                                + c * (src[sidx + x + sstride] as u16)
                                + d * (src[sidx + x + 1 + sstride] as u16) + 8) >> 4) as u8;
            }
            didx += dstride;
            sidx += sstride;
        }
    } else {
        let a = ((4 - x) * (4 - y)) as u16;
        let e = ((    x) * (4 - y) + (4 - x) * (    y)) as u16;
        let step = if y > 0 { sstride } else { 1 };
        for _ in 0..h {
            for x in 0..w {
                dst[didx + x] = ((a * (src[sidx + x] as u16)
                                + e * (src[sidx + x + step] as u16) + 8) >> 4) as u8;
            }
            didx += dstride;
            sidx += sstride;
        }
    }
}

fn check_pos(x: usize, y: usize, cw: usize, ch: usize, w: usize, h: usize, dx: i16, dy: i16, e0: isize, e1: isize, e2: isize, e3: isize) -> bool {
    let xn = (x as isize) + (dx as isize);
    let yn = (y as isize) + (dy as isize);

    (xn - e0 >= 0) && (xn + (cw as isize) + e1 <= (w as isize)) && (yn - e2 >= 0) && (yn + (ch as isize) + e3 <= (h as isize))
}

macro_rules! diff{
    ($src: ident, $e1: expr, $e2: expr) => (
            ($src[$e1] as i16) - ($src[$e2] as i16)
        )
}
macro_rules! strength{
    ($el: expr, $lim: expr) => (if $el.abs() < $lim { 3 } else { 1 })
}
fn clip_symm(val: i16, lim: i16) -> i16 { val.max(-lim).min(lim) }

fn filter_luma_edge(dst: &mut [u8], mut offset: usize, step: usize, stride: usize, mode1: u8, mode2: u8, lim1: i16, lim2: i16) {
    let mut diff_q1q0: [i16; 4] = [0; 4];
    let mut diff_p1p0: [i16; 4] = [0; 4];
    for i in 0..4 {
        let off = offset + i * stride;
        diff_q1q0[i] = diff!(dst, off - 2 * step, off - step);
        diff_p1p0[i] = diff!(dst, off +     step, off);
    }
    let str_p = strength!(diff_q1q0[0] + diff_q1q0[1] + diff_q1q0[2] + diff_q1q0[3], lim2);
    let str_q = strength!(diff_p1p0[0] + diff_p1p0[1] + diff_p1p0[2] + diff_p1p0[3], lim2);
    if str_p + str_q > 2 {
        let msum = ((mode1 + mode2 + str_q + str_p) >> 1) as i16;
        let (maxprod, weak) = if (str_q == 1) || (str_p == 1) { (384, true) } else { (256, false) };
        for y in 0..4 {
            let diff_p0q0 = diff!(dst, offset, offset - step);
            if (diff_p0q0 != 0) && (((lim1 * diff_p0q0.abs()) & !0x7F) <= maxprod) {
                let diff_q1q2 = diff!(dst, offset - 2 * step, offset - 3 * step);
                let diff_p1p2 = diff!(dst, offset +     step, offset + 2 * step);
                let delta = if weak {
                        clip_symm((diff_p0q0 + 1) >> 1, msum >> 1)
                    } else {
                        let diff_strg = (diff!(dst, offset - 2 * step, offset + step) + 4 * diff_p0q0 + 4) >> 3;
                        clip_symm(diff_strg, msum)
                    };
                dst[offset - step] = clip8((dst[offset - step] as i16) + delta);
                dst[offset]        = clip8((dst[offset]        as i16) - delta);
                if (str_q != 1) && (diff_q1q2.abs() <= (lim2 >> 2)) {
                    let diff = (diff_q1q0[y] + diff_q1q2 - delta) >> 1;
                    let delta_q1 = if weak {
                            clip_symm(diff, (mode1 >> 1) as i16)
                        } else {
                            clip_symm(diff, mode1 as i16)
                        };
                    dst[offset - 2 * step] = clip8((dst[offset - 2 * step] as i16) - delta_q1);
                }
                if (str_p != 1) && (diff_p1p2.abs() <= (lim2 >> 2)) {
                    let diff = (diff_p1p0[y] + diff_p1p2 + delta) >> 1;
                    let delta_p1 = if weak {
                            clip_symm(diff, (mode2 >> 1) as i16)
                        } else {
                            clip_symm(diff, mode2 as i16)
                        };
                    dst[offset + step] = clip8((dst[offset + step] as i16) - delta_p1);
                }
            }
            offset += stride;
        }
    }
}
fn filter_chroma_edge(dst: &mut [u8], mut offset: usize, step: usize, stride: usize, mode1: u8, mode2: u8, lim1: i16, lim2: i16) {
    let diff_q = 4 * diff!(dst, offset - 2 * step, offset - step).abs();
    let diff_p = 4 * diff!(dst, offset +     step, offset       ).abs();
    let str_q = strength!(diff_q, lim2);
    let str_p = strength!(diff_p, lim2);
    if str_p + str_q > 2 {
        let msum = ((mode1 + mode2 + str_q + str_p) >> 1) as i16;
        let (maxprod, weak) = if (str_q == 1) || (str_p == 1) { (384, true) } else { (256, false) };
        for _ in 0..2 {
            let diff_pq = diff!(dst, offset, offset - step);
            if (diff_pq != 0) && (((lim1 * diff_pq.abs()) & !0x7F) <= maxprod) {
                let delta = if weak {
                        clip_symm((diff_pq + 1) >> 1, msum >> 1)
                    } else {
                        let diff_strg = (diff!(dst, offset - 2 * step, offset + step) + 4 * diff_pq + 4) >> 3;
                        clip_symm(diff_strg, msum)
                    };
                dst[offset - step] = clip8((dst[offset - step] as i16) + delta);
                dst[offset]        = clip8((dst[offset]        as i16) - delta);
            }
            offset += stride;
        }
    }
}

pub struct RV60DeblockParams {
    pub deblock_chroma: bool,
    pub width:          usize,
    pub height:         usize,
    pub dblkstride:     usize,
}

pub struct RV60DSP {}
/*pub fn rv6_transform4x4_dc(coeffs: &mut [i16]) {
    let dc = (((coeffs[0] * 13 + 0x10) >> 5) * 13 + 0x10) >> 5;
    for el in coeffs.iter_mut().take(16) {
        *el = dc;
    }
}*/

impl RV60DSP {
    pub fn new() -> Self { Self{} }
    pub fn transform4x4(&self, blk: &mut [i16]) {
        let mut tmp: [i32; 4 * 4] = [0; 4 * 4];

        for i in 0..4 {
            let a = blk[i + 0 * 4] as i32;
            let b = blk[i + 1 * 4] as i32;
            let c = blk[i + 2 * 4] as i32;
            let d = blk[i + 3 * 4] as i32;

            let t0 = 13 * (a + c);
            let t1 = 13 * (a - c);
            let t2 = 7 * b - 17 * d;
            let t3 = 7 * d + 17 * b;
            tmp[i + 0 * 4] = (t0 + t3 + 0x10) >> 5;
            tmp[i + 1 * 4] = (t1 + t2 + 0x10) >> 5;
            tmp[i + 2 * 4] = (t1 - t2 + 0x10) >> 5;
            tmp[i + 3 * 4] = (t0 - t3 + 0x10) >> 5;
        }
        for (dst, src) in blk.chunks_mut(4).zip(tmp.chunks(4)) {
            let a = src[0];
            let b = src[1];
            let c = src[2];
            let d = src[3];

            let t0 = 13 * (a + c);
            let t1 = 13 * (a - c);
            let t2 = 7 * b - 17 * d;
            let t3 = 7 * d + 17 * b;
            dst[0] = ((t0 + t3 + 0x10) >> 5) as i16;
            dst[1] = ((t1 + t2 + 0x10) >> 5) as i16;
            dst[2] = ((t1 - t2 + 0x10) >> 5) as i16;
            dst[3] = ((t0 - t3 + 0x10) >> 5) as i16;
        }
    }
    /*pub fn transform8x8_dc(&self, blk: &mut [i16]) {
        assert!(blk.len() >= 8 * 8);
        let dc = (((coeffs[0] * 37 + 0x40) >> 7) * 37 + 0x40) >> 7;
        for el in coeffs.iter_mut().take(8 * 8) {
            *el = dc;
        }
    }*/
    pub fn transform8x8(&self, blk: &mut [i16]) {
        assert!(blk.len() >= 8 * 8);
        let mut tmp: [i32; 8 * 8] = [0; 8 * 8];
        for i in 0..8 {
            let s0 = blk[i + 0 * 8] as i32;
            let s1 = blk[i + 1 * 8] as i32;
            let s2 = blk[i + 2 * 8] as i32;
            let s3 = blk[i + 3 * 8] as i32;
            let s4 = blk[i + 4 * 8] as i32;
            let s5 = blk[i + 5 * 8] as i32;
            let s6 = blk[i + 6 * 8] as i32;
            let s7 = blk[i + 7 * 8] as i32;

            let t0 = 37 * (s0 + s4);
            let t1 = 37 * (s0 - s4);
            let t2 = 48 * s2 + 20 * s6;
            let t3 = 20 * s2 - 48 * s6;
            let t4 = t0 + t2;
            let t5 = t0 - t2;
            let t6 = t1 + t3;
            let t7 = t1 - t3;
            let t8 = 51 * s1 + 43 * s3 + 29 * s5 + 10 * s7;
            let t9 = 43 * s1 - 10 * s3 - 51 * s5 - 29 * s7;
            let ta = 29 * s1 - 51 * s3 + 10 * s5 + 43 * s7;
            let tb = 10 * s1 - 29 * s3 + 43 * s5 - 51 * s7;
            tmp[i + 0 * 8] = (t4 + t8 + 0x40) >> 7;
            tmp[i + 1 * 8] = (t6 + t9 + 0x40) >> 7;
            tmp[i + 2 * 8] = (t7 + ta + 0x40) >> 7;
            tmp[i + 3 * 8] = (t5 + tb + 0x40) >> 7;
            tmp[i + 4 * 8] = (t5 - tb + 0x40) >> 7;
            tmp[i + 5 * 8] = (t7 - ta + 0x40) >> 7;
            tmp[i + 6 * 8] = (t6 - t9 + 0x40) >> 7;
            tmp[i + 7 * 8] = (t4 - t8 + 0x40) >> 7;
        }
        for (dst, src) in blk.chunks_mut(8).zip(tmp.chunks(8)) {
            let s0 = src[0];
            let s1 = src[1];
            let s2 = src[2];
            let s3 = src[3];
            let s4 = src[4];
            let s5 = src[5];
            let s6 = src[6];
            let s7 = src[7];

            let t0 = 37 * (s0 + s4);
            let t1 = 37 * (s0 - s4);
            let t2 = 48 * s2 + 20 * s6;
            let t3 = 20 * s2 - 48 * s6;
            let t4 = t0 + t2;
            let t5 = t0 - t2;
            let t6 = t1 + t3;
            let t7 = t1 - t3;
            let t8 = 51 * s1 + 43 * s3 + 29 * s5 + 10 * s7;
            let t9 = 43 * s1 - 10 * s3 - 51 * s5 - 29 * s7;
            let ta = 29 * s1 - 51 * s3 + 10 * s5 + 43 * s7;
            let tb = 10 * s1 - 29 * s3 + 43 * s5 - 51 * s7;
            dst[0] = ((t4 + t8 + 0x40) >> 7) as i16;
            dst[1] = ((t6 + t9 + 0x40) >> 7) as i16;
            dst[2] = ((t7 + ta + 0x40) >> 7) as i16;
            dst[3] = ((t5 + tb + 0x40) >> 7) as i16;
            dst[4] = ((t5 - tb + 0x40) >> 7) as i16;
            dst[5] = ((t7 - ta + 0x40) >> 7) as i16;
            dst[6] = ((t6 - t9 + 0x40) >> 7) as i16;
            dst[7] = ((t4 - t8 + 0x40) >> 7) as i16;
        }
    }
    /*pub fn transform16x16_dc(&self, blk: &mut [i16; 16 * 16]) {
        let dc = (((coeffs[0] * 26 + 0x40) >> 7) * 26 + 0x40) >> 7;
        for el in coeffs.iter_mut() {
            *el = dc;
        }
    }*/
    #[allow(non_snake_case)]
    fn transform16(blk: &mut [i16; 16 * 16], off: usize, step: usize) {
        let src0 = blk[off +  0 * step] as i32;
        let src1 = blk[off +  1 * step] as i32;
        let src2 = blk[off +  2 * step] as i32;
        let src3 = blk[off +  3 * step] as i32;
        let src4 = blk[off +  4 * step] as i32;
        let src5 = blk[off +  5 * step] as i32;
        let src6 = blk[off +  6 * step] as i32;
        let src7 = blk[off +  7 * step] as i32;
        let src8 = blk[off +  8 * step] as i32;
        let src9 = blk[off +  9 * step] as i32;
        let srcA = blk[off + 10 * step] as i32;
        let srcB = blk[off + 11 * step] as i32;
        let srcC = blk[off + 12 * step] as i32;
        let srcD = blk[off + 13 * step] as i32;
        let srcE = blk[off + 14 * step] as i32;
        let srcF = blk[off + 15 * step] as i32;
        let t0 = 26 * (src0 + src8);
        let t1 = 26 * (src0 - src8);
        let t2 = 14 * src4 - 34 * srcC;
        let t3 = 34 * src4 + 14 * srcC;
        let t4 = t0 + t3;
        let t5 = t0 - t3;
        let t6 = t1 + t2;
        let t7 = t1 - t2;
        let tmp00 = 31 * src2 +  -7 * src6 + -36 * srcA + -20 * srcE;
        let tmp01 = 36 * src2 +  31 * src6 +  20 * srcA +   7 * srcE;
        let tmp02 = 20 * src2 + -36 * src6 +   7 * srcA +  31 * srcE;
        let tmp03 =  7 * src2 + -20 * src6 +  31 * srcA + -36 * srcE;
        let tm0 = t4 + tmp01;
        let tm1 = t4 - tmp01;
        let tm2 = t5 + tmp03;
        let tm3 = t5 - tmp03;
        let tm4 = t6 + tmp00;
        let tm5 = t6 - tmp00;
        let tm6 = t7 + tmp02;
        let tm7 = t7 - tmp02;
        let tt0 = 37 * src1 +  35 * src3 +  32 * src5 +  28 * src7 +  23 * src9 +  17 * srcB +  11 * srcD +   4 * srcF;
        let tt1 = 35 * src1 +  23 * src3 +   4 * src5 + -17 * src7 + -32 * src9 + -37 * srcB + -28 * srcD + -11 * srcF;
        let tt2 = 32 * src1 +   4 * src3 + -28 * src5 + -35 * src7 + -11 * src9 +  23 * srcB +  37 * srcD +  17 * srcF;
        let tt3 = 28 * src1 + -17 * src3 + -35 * src5 +   4 * src7 +  37 * src9 +  11 * srcB + -32 * srcD + -23 * srcF;
        let tt4 = 23 * src1 + -32 * src3 + -11 * src5 +  37 * src7 +  -4 * src9 + -35 * srcB +  17 * srcD +  28 * srcF;
        let tt5 = 17 * src1 + -37 * src3 +  23 * src5 +  11 * src7 + -35 * src9 +  28 * srcB +   4 * srcD + -32 * srcF;
        let tt6 = 11 * src1 + -28 * src3 +  37 * src5 + -32 * src7 +  17 * src9 +   4 * srcB + -23 * srcD +  35 * srcF;
        let tt7 =  4 * src1 + -11 * src3 +  17 * src5 + -23 * src7 +  28 * src9 + -32 * srcB +  35 * srcD + -37 * srcF;
        blk[off +  0 * step] = ((tm0 + tt0 + 64) >> 7) as i16;
        blk[off +  1 * step] = ((tm4 + tt1 + 64) >> 7) as i16;
        blk[off +  2 * step] = ((tm6 + tt2 + 64) >> 7) as i16;
        blk[off +  3 * step] = ((tm2 + tt3 + 64) >> 7) as i16;
        blk[off +  4 * step] = ((tm3 + tt4 + 64) >> 7) as i16;
        blk[off +  5 * step] = ((tm7 + tt5 + 64) >> 7) as i16;
        blk[off +  6 * step] = ((tm5 + tt6 + 64) >> 7) as i16;
        blk[off +  7 * step] = ((tm1 + tt7 + 64) >> 7) as i16;
        blk[off +  8 * step] = ((tm1 - tt7 + 64) >> 7) as i16;
        blk[off +  9 * step] = ((tm5 - tt6 + 64) >> 7) as i16;
        blk[off + 10 * step] = ((tm7 - tt5 + 64) >> 7) as i16;
        blk[off + 11 * step] = ((tm3 - tt4 + 64) >> 7) as i16;
        blk[off + 12 * step] = ((tm2 - tt3 + 64) >> 7) as i16;
        blk[off + 13 * step] = ((tm6 - tt2 + 64) >> 7) as i16;
        blk[off + 14 * step] = ((tm4 - tt1 + 64) >> 7) as i16;
        blk[off + 15 * step] = ((tm0 - tt0 + 64) >> 7) as i16;
    }
    pub fn transform16x16(&self, blk: &mut [i16; 16 * 16]) {
        for i in 0..16 {
            Self::transform16(blk, i, 16);
        }
        for i in 0..16 {
            Self::transform16(blk, i * 16, 1);
        }
    }

    pub fn add_block(&self, dst: &mut [u8], mut doff: usize, dstride: usize, blk: &[i16], size: usize) {
        for y in 0..size {
            for x in 0..size {
                dst[doff + x] = clip8((dst[doff + x] as i16) + blk[x + y * size]);
            }
            doff += dstride;
        }
    }
    fn avg(&self, dst: &mut [u8], mut didx: usize, dstride: usize,
               src: &[u8], mut sidx: usize, sstride: usize,
               w: usize, h: usize) {
        for _ in 0..h {
            for x in 0..w {
                dst[didx + x] = (((dst[didx + x] as u16) + (src[sidx + x] as u16)) >> 1) as u8;
            }
            didx += dstride;
            sidx += sstride;
        }
    }
    pub fn do_avg(&self, frame: &mut NASimpleVideoFrame<u8>, prev_frame: &NAVideoBuffer<u8>, x: usize, y: usize, w: usize, h: usize) {
        for comp in 0..3 {
            let dstride = frame.stride[comp];
            let sstride = prev_frame.get_stride(comp);
            let doff = if comp == 0 { x + y * dstride } else { frame.offset[comp] + (x >> 1) + (y >> 1) * dstride };
            let soff = prev_frame.get_offset(comp);
            let dst = &mut frame.data;
            let sdata = prev_frame.get_data();
            let src: &[u8] = sdata.as_slice();

            if comp == 0 {
                self.avg(dst, doff, dstride, src, soff, sstride, w, h);
            } else {
                self.avg(dst, doff, dstride, src, soff, sstride, w >> 1, h >> 1);
            }
        }
    }
    pub fn do_mc(&self, frame: &mut NASimpleVideoFrame<u8>, prev_frame: &NAVideoBuffer<u8>, x: usize, y: usize, w: usize, h: usize, mv: MV, avg: bool) {
        { // luma
            let dstride = frame.stride[0];
            let doffset = frame.offset[0] + (if !avg { x + y * dstride } else { 0 });
            let dst = &mut frame.data;

            let (w_, h_) = prev_frame.get_dimensions(0);
            let fw = (w_ + 15) & !15;
            let fh = (h_ + 15) & !15;

            let dx = mv.x >> 2;
            let cx = (mv.x & 3) as usize;
            let dy = mv.y >> 2;
            let cy = (mv.y & 3) as usize;

            if check_pos(x, y, w, h, fw, fh, dx, dy, RV60_EDGE1[cx], RV60_EDGE2[cx], RV60_EDGE1[cy], RV60_EDGE2[cy]) {
                let sstride = prev_frame.get_stride(0);
                let mut soffset = prev_frame.get_offset(0) + x + y * sstride;
                let data = prev_frame.get_data();
                let src: &[u8] = data.as_slice();
                soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
                luma_mc(dst, doffset, dstride, src, soffset, sstride, w, h, cx, cy);
            } else {
                let mut ebuf: [u8; 70*70] = [0; 70*70];
                edge_emu(prev_frame, (x as isize) + (dx as isize) - 2, (y as isize) + (dy as isize) - 2, w+5, h+5, &mut ebuf, 70, 0, 4);
                luma_mc(dst, doffset, dstride, &ebuf, 70*2 + 2, 70, w, h, cx, cy);
            }
        }
        let (w_, h_) = prev_frame.get_dimensions(1);
        let fw = (w_ + 7) & !7;
        let fh = (h_ + 7) & !7;
        let mvx = mv.x / 2;
        let mvy = mv.y / 2;
        let dx = mvx >> 2;
        let cx = (mvx & 3) as usize;
        let dy = mvy >> 2;
        let cy = (mvy & 3) as usize;
        let cw = w >> 1;
        let ch = h >> 1;

        for comp in 1..3 { // chroma
            let dstride = frame.stride[comp];
            let doffset = frame.offset[comp] + (if !avg { (x >> 1) + (y >> 1) * dstride } else { 0 });
            if check_pos(x >> 1, y >> 1, cw, ch, fw, fh, dx, dy, 0, 1, 0, 1) {
                let sstride = prev_frame.get_stride(comp);
                let mut soffset = prev_frame.get_offset(comp) + (x >> 1) + (y >> 1) * sstride;
                let data = prev_frame.get_data();
                let src: &[u8] = data.as_slice();
                soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
                chroma_mc(frame.data, doffset, dstride, src, soffset, sstride, cw, ch, cx, cy);
            } else {
                let mut ebuf: [u8; 40*40] = [0; 40*40];
                edge_emu(prev_frame, ((x >> 1) as isize) + (dx as isize), ((y >> 1) as isize) + (dy as isize), cw+1, ch+1, &mut ebuf, 40, comp, 3);
                chroma_mc(frame.data, doffset, dstride, &ebuf, 0, 40, cw, ch, cx, cy);
            }
        }
    }
    fn deblock_edge4_ver(&self, frame: &mut NASimpleVideoFrame<u8>, xpos: usize, ypos: usize,
                         dblk_l: u8, dblk_r: u8, deblock_chroma: bool) {
        let qp_l  = dblk_l >> 2;
        let str_l = dblk_l & 3;
        let qp_r  = dblk_r >> 2;
        let str_r = dblk_r & 3;
        let dl_l = &RV60_DEB_LIMITS[qp_l as usize];
        let dl_r = &RV60_DEB_LIMITS[qp_r as usize];
        let mode_l = if str_l != 0 { dl_l[(str_l - 1) as usize] } else { 0 };
        let mode_r = if str_r != 0 { dl_r[(str_r - 1) as usize] } else { 0 };
        let lim1 = dl_r[2] as i16;
        let lim2 = (dl_r[3] * 4) as i16;
        {
            let stride = frame.stride[0];
            let offset = frame.offset[0] + xpos + ypos * stride;
            filter_luma_edge(frame.data, offset, 1, stride, mode_l, mode_r, lim1, lim2);
        }
        if ((str_l | str_r) >= 2) && deblock_chroma {
            for comp in 1..3 {
                let stride = frame.stride[comp];
                let offset = frame.offset[comp] + (xpos >> 1) + (ypos >> 1) * stride;
                filter_chroma_edge(frame.data, offset, 1, stride, mode_l, mode_r, lim1, lim2);
            }
        }
    }
    fn deblock_edge4_hor(&self, frame: &mut NASimpleVideoFrame<u8>, xpos: usize, ypos: usize,
                         dblk_t: u8, dblk_d: u8, deblock_chroma: bool) {
        let qp_t  = dblk_t >> 2;
        let str_t = dblk_t & 3;
        let qp_d  = dblk_d >> 2;
        let str_d = dblk_d & 3;
        let dl_t = &RV60_DEB_LIMITS[qp_t as usize];
        let dl_d = &RV60_DEB_LIMITS[qp_d as usize];
        let mode_t = if str_t != 0 { dl_t[(str_t - 1) as usize] } else { 0 };
        let mode_d = if str_d != 0 { dl_d[(str_d - 1) as usize] } else { 0 };
        let lim1 = dl_d[2] as i16;
        let lim2 = (dl_d[3] * 4) as i16;
        {
            let stride = frame.stride[0];
            let offset = frame.offset[0] + xpos + ypos * stride;
            filter_luma_edge(frame.data, offset, stride, 1, mode_t, mode_d, lim1, lim2);
        }
        if ((str_t | str_d) >= 2) && deblock_chroma {
            for comp in 1..3 {
                let stride = frame.stride[comp];
                let offset = frame.offset[comp] + (xpos >> 1) + (ypos >> 1) * stride;
                filter_chroma_edge(frame.data, offset, stride, 1, mode_t, mode_d, lim1, lim2);
            }
        }
    }
    fn deblock8x8(&self, dparams: &RV60DeblockParams, frame: &mut NASimpleVideoFrame<u8>,
                  xpos: usize, ypos: usize, top_str: &[u8], left_str: &[u8], dblkpos: usize) {
        if xpos > 0 {
            if ypos > 0 {
                let str_l = left_str[dblkpos - dparams.dblkstride - 1];
                let str_r = left_str[dblkpos - dparams.dblkstride];
                if ((str_l | str_r) & 3) != 0 {
                    self.deblock_edge4_ver(frame, xpos, ypos - 4, str_l, str_r, dparams.deblock_chroma);
                }
            }
            {
                let str_l = left_str[dblkpos - 1];
                let str_r = left_str[dblkpos];
                if ((str_l | str_r) & 3) != 0 {
                    self.deblock_edge4_ver(frame, xpos, ypos + 0, str_l, str_r, dparams.deblock_chroma);
                }
            }
            if ypos + 8 >= dparams.height {
                let str_l = left_str[dblkpos + dparams.dblkstride - 1];
                let str_r = left_str[dblkpos + dparams.dblkstride];
                if ((str_l | str_r) & 3) != 0 {
                    self.deblock_edge4_ver(frame, xpos, ypos + 4, str_l, str_r, dparams.deblock_chroma);
                }
            }
        }
        if ypos > 0 {
            if xpos > 0 {
                let str_t = top_str[dblkpos - dparams.dblkstride - 1];
                let str_d = top_str[dblkpos - 1];
                if ((str_t | str_d) & 3) != 0 {
                    self.deblock_edge4_hor(frame, xpos - 4, ypos, str_t, str_d, dparams.deblock_chroma);
                }
            }
            {
                let str_t = top_str[dblkpos - dparams.dblkstride];
                let str_d = top_str[dblkpos];
                if ((str_t | str_d) & 3) != 0 {
                    self.deblock_edge4_hor(frame, xpos + 0, ypos, str_t, str_d, dparams.deblock_chroma);
                }
            }
            if xpos + 8 >= dparams.width {
                let str_t = top_str[dblkpos - dparams.dblkstride + 1];
                let str_d = top_str[dblkpos + 1];
                if ((str_t | str_d) & 3) != 0 {
                    self.deblock_edge4_hor(frame, xpos + 4, ypos, str_t, str_d, dparams.deblock_chroma);
                }
            }
        }
    }
    pub fn do_deblock(&self, dparams: &RV60DeblockParams, frame: &mut NASimpleVideoFrame<u8>,
                      xpos: usize, ypos: usize, size: usize, top_str: &[u8], left_str: &[u8], dpos: usize) {
        for x in 0..(size >> 3) {
            self.deblock8x8(dparams, frame, xpos + x * 8, ypos,
                            top_str, left_str, dpos + x * 2);
        }
        for y in 1..(size >> 3) {
            self.deblock8x8(dparams, frame, xpos, ypos + y * 8,
                            top_str, left_str, dpos + y * 2 * dparams.dblkstride);
        }
    }
}

const RV60_DEB_LIMITS: [[u8; 4]; 32] = [
    [ 0, 0, 128,  0 ], [ 0, 0, 128,  0 ], [ 0, 0, 128,  0 ], [ 0, 0, 128,  0 ],
    [ 0, 0, 128,  0 ], [ 0, 0, 128,  0 ], [ 0, 0, 128,  0 ], [ 0, 0, 128,  0 ],
    [ 0, 0, 128,  3 ], [ 0, 1, 128,  3 ], [ 0, 1, 122,  3 ], [ 1, 1,  96,  4 ],
    [ 1, 1,  75,  4 ], [ 1, 1,  59,  4 ], [ 1, 1,  47,  6 ], [ 1, 1,  37,  6 ],
    [ 1, 1,  29,  6 ], [ 1, 2,  23,  7 ], [ 1, 2,  18,  8 ], [ 1, 2,  15,  8 ],
    [ 1, 2,  13,  9 ], [ 2, 3,  11,  9 ], [ 2, 3,  10, 10 ], [ 2, 3,   9, 10 ],
    [ 2, 4,   8, 11 ], [ 3, 4,   7, 11 ], [ 3, 5,   6, 12 ], [ 3, 5,   5, 13 ],
    [ 3, 5,   4, 14 ], [ 4, 7,   3, 15 ], [ 5, 8,   2, 16 ], [ 5, 9,   1, 17 ]
];

#[derive(Clone)]
pub struct IntraPredContext {
    pub t:      [u8; 129], // 0 - TL or 0x80, two block sizes or replicated last val from block0
    pub l:      [u8; 129],
    pub has_t:  bool,
    pub has_tr: bool,
    pub has_l:  bool,
    pub has_ld: bool,
}

impl IntraPredContext {
    pub fn new() -> Self {
        Self {
            t: [0x80; 129], l: [0x80; 129], has_t: false, has_tr: false, has_l: false, has_ld: false,
        }
    }
    pub fn pred_dc(&self, dst: &mut [u8], mut doff: usize, dstride: usize, size: usize, filter: bool) {
        let dc;
        if !self.has_t && !self.has_l {
            dc = 0x80;
        } else {
            let mut sum = 0;
            if self.has_t {
                for x in 0..size { sum += self.t[x + 1] as u16; }
            }
            if self.has_l {
                for y in 0..size { sum += self.l[y + 1] as u16; }
            }
            if self.has_t && self.has_l {
                dc = ((sum + (size as u16)) / ((size as u16) * 2)) as u8;
            } else {
                dc = ((sum + ((size >> 1) as u16)) / (size as u16)) as u8;
            }
        }
        for _ in 0..size {
            for x in 0..size { dst[doff + x] = dc; }
            doff += dstride;
        }
        if filter && self.has_t && self.has_l {
            doff -= dstride * size;
            dst[doff] = (((self.t[1] as u16) + (self.l[1] as u16) + 2 * (dst[doff] as u16) + 2) >> 2) as u8;
            for x in 1..size {
                dst[doff + x] = (((self.t[x + 1] as u16) + 3 * (dst[doff + x] as u16) + 2) >> 2) as u8;
            }
            for y in 1..size {
                doff += dstride;
                dst[doff] = (((self.l[y + 1] as u16) + 3 * (dst[doff] as u16) + 2) >> 2) as u8;
            }
        }
    }
    pub fn pred_plane(&self, dst: &mut [u8], mut doff: usize, dstride: usize, size: usize) {
        let lastl = self.l[size + 1] as i32;
        let lastt = self.t[size + 1] as i32;
        let mut tmp1: [i32; 64] = [0; 64];
        let mut tmp2: [i32; 64] = [0; 64];
        for i in 0..size {
            tmp1[i] = lastl - (self.t[i + 1] as i32);
            tmp2[i] = lastt - (self.l[i + 1] as i32);
        }
        let shift = match size {
                4   => 3,
                8   => 4,
                16  => 5,
                32  => 6,
                _   => 7,
            };
        let mut top_ref: [i32; 64] = [0; 64];
        let mut left_ref:[i32; 64] = [0; 64];
        for i in 0..size {
            top_ref [i] = (self.t[i + 1] as i32) << (shift - 1);
            left_ref[i] = (self.l[i + 1] as i32) << (shift - 1);
        }
        for y in 0..size {
            let add = tmp2[y];
            let mut sum = left_ref[y] + (size as i32);
            for x in 0..size {
                let v = tmp1[x] + top_ref[x];
                sum += add;
                top_ref[x] = v;
                dst[doff + x] = ((sum + v) >> shift) as u8;
            }
            doff += dstride;
        }
    }
    fn pred_hor_angle(dst: &mut [u8], doff: usize, dstride: usize, size: usize, weight: i16, src: &[u8]) {
        let mut sum = 0;
        for x in 0..size {
            sum += weight;
            let off = ((sum >> 5) + 32) as usize;
            let frac = (sum & 0x1F) as u16;
            if frac == 0 {
                for y in 0..size {
                    dst[doff + x + y * dstride] = src[off + y];
                }
            } else {
                for y in 0..size {
                    let a = src[off + y + 0] as u16;
                    let b = src[off + y + 1] as u16;
                    dst[doff + x + y * dstride] = (((32 - frac) * a + frac * b + 0x10) >> 5) as u8;
                }
            }
        }
    }
    fn pred_ver_angle(dst: &mut [u8], mut doff: usize, dstride: usize, size: usize, weight: i16, src: &[u8]) {
        let mut sum = 0;
        for _ in 0..size {
            sum += weight;
            let off = ((sum >> 5) + 32) as usize;
            let frac = (sum & 0x1F) as u16;
            if frac == 0 {
                dst[doff..][..size].copy_from_slice(&src[off..][..size]);
            } else {
                for x in 0..size {
                    let a = src[off + x + 0] as u16;
                    let b = src[off + x + 1] as u16;
                    dst[doff + x] = (((32 - frac) * a + frac * b + 0x10) >> 5) as u8;
                }
            }
            doff += dstride;
        }
    }
    fn filter_weak(dst: &mut [u8], src: &[u8], size: usize) {
        dst[0] = src[0];
        for i in 1..size-1 {
            dst[i] = (((src[i - 1] as u16) + 2 * (src[i] as u16) + (src[i + 1] as u16) + 2) >> 2) as u8;
        }
        dst[size - 1] = src[size - 1];
    }
    fn filter_bilin32(dst: &mut [u8], v0: u8, v1: u8, size: usize) {
        let diff = (v1 as i16) - (v0 as i16);
        let mut sum = ((v0 as i16) << 5) + (1 << (5 - 1));
        for i in 0..size {
            dst[i] = (sum >> 5) as u8;
            sum += diff;
        }
    }
    #[allow(clippy::cognitive_complexity)]
    pub fn pred_angle(&self, dst: &mut [u8], mut doff: usize, dstride: usize, size: usize, angle: usize, filter: bool) {
        let mut filtered1: [u8; 96] = [0; 96];
        let mut filtered2: [u8; 96] = [0; 96];
        if angle == 0 {
            self.pred_plane(dst, doff, dstride, size);
        } else if angle == 1 {
            self.pred_dc(dst, doff, dstride, size, filter);
        } else if angle <= 9 {
            let ang_weight = RV60_IPRED_ANGLE[10 - angle];
            let add_size = (size * (ang_weight as usize) + 31) >> 5;
            if size <= 16 {
                Self::filter_weak(&mut filtered1[32..], &self.l[1..], size + add_size);
            } else {
                Self::filter_bilin32(&mut filtered1[32..], self.l[1], self.l[33], 32);
                Self::filter_bilin32(&mut filtered1[64..], self.l[32], self.l[64], add_size);
            }
            Self::pred_hor_angle(dst, doff, dstride, size, ang_weight as i16, &filtered1);
        } else if angle == 10 {
            if size <= 16 {
                Self::filter_weak(&mut filtered1[32..], &self.l[1..], size);
            } else {
                Self::filter_bilin32(&mut filtered1[32..], self.l[1], self.l[33], 32);
            }
            for y in 0..size {
                for x in 0..size {
                    dst[doff + x] = filtered1[32 + y];
                }
                doff += dstride;
            }
            if filter {
                doff -= dstride * size;
                let tl = self.t[0] as i16;
                for x in 0..size {
                    dst[doff + x] = clip8((dst[doff + x] as i16) + (((self.t[x + 1] as i16) - tl) >> 1));
                }
            }
        } else if angle <= 17 {
            let ang_weight = RV60_IPRED_ANGLE    [angle - 10];
            let inv_angle  = RV60_IPRED_INV_ANGLE[angle - 10];
            let add_size = (size * (ang_weight as usize) + 31) >> 5;
            if size <= 16 {
                for i in 0..=size {
                    filtered1[32-1 + i] = self.l[i];
                }
                for i in 0..=size {
                    filtered2[32-1 + i] = self.t[i];
                }
            } else {
                filtered1[32-1] = self.l[0];
                Self::filter_bilin32(&mut filtered1[32..], self.l[0], self.l[32], 32);
                filtered2[32-1] = self.t[0];
                Self::filter_bilin32(&mut filtered2[32..], self.t[0], self.t[32], 32);
            }
            if add_size > 1 {
                let mut sum = 0x80;
                for i in 1..add_size {
                    sum += inv_angle;
                    let pos = ((sum >> 8) + 32 - 1) as usize;
                    filtered1[32 - 1 - i] = filtered2[pos];
                }
            }
            Self::pred_hor_angle(dst, doff, dstride, size, -(ang_weight as i16), &filtered1);
        } else if angle <= 25 {
            let ang_weight = RV60_IPRED_ANGLE[26 - angle];
            let inv_angle  = RV60_IPRED_INV_ANGLE[26 - angle];
            let add_size = (size * (ang_weight as usize) + 31) >> 5;
            if size <= 16 {
                for i in 0..=size {
                    filtered1[32-1 + i] = self.t[i];
                }
                for i in 0..=size {
                    filtered2[32-1 + i] = self.l[i];
                }
            } else {
                filtered1[32-1] = self.t[0];
                Self::filter_bilin32(&mut filtered1[32..], self.t[0], self.t[32], 32);
                filtered2[32-1] = self.l[0];
                Self::filter_bilin32(&mut filtered2[32..], self.l[0], self.l[32], 32);
            }
            if add_size > 1 {
                let mut sum = 0x80;
                for i in 1..add_size {
                    sum += inv_angle;
                    let pos = ((sum >> 8) + 32 - 1) as usize;
                    filtered1[32 - 1 - i] = filtered2[pos];
                }
            }
            Self::pred_ver_angle(dst, doff, dstride, size, -(ang_weight as i16), &filtered1);
        } else if angle == 26 {
            if size <= 16 {
                Self::filter_weak(&mut filtered1[32..], &self.t[1..], size);
            } else {
                Self::filter_bilin32(&mut filtered1[32..], self.t[1], self.t[33], 32);
            }
            for _ in 0..size {
                dst[doff..][..size].copy_from_slice(&filtered1[32..][..size]);
                doff += dstride;
            }
            if filter {
                doff -= dstride * size;
                let tl = self.l[0] as i16;
                for y in 0..size {
                    dst[doff] = clip8((dst[doff] as i16) + (((self.l[y + 1] as i16) - tl) >> 1));
                    doff += dstride;
                }
            }
        } else if angle <= 34 {
            let ang_weight = RV60_IPRED_ANGLE[angle - 26];
            let add_size = (size * (ang_weight as usize) + 31) >> 5;
            if size <= 16 {
                Self::filter_weak(&mut filtered1[32..], &self.t[1..], size + add_size);
            } else {
                Self::filter_bilin32(&mut filtered1[32..], self.t[1], self.t[33], 32);
                Self::filter_bilin32(&mut filtered1[64..], self.t[32], self.t[64], add_size);
            }
            Self::pred_ver_angle(dst, doff, dstride, size, ang_weight as i16, &filtered1);
        } else {
            unreachable!();
        }
    }
}

const RV60_IPRED_ANGLE: [u8; 9] = [ 0, 2, 5, 9, 13, 17, 21, 26, 32 ];
const RV60_IPRED_INV_ANGLE: [i16; 9] = [ 0, 4096, 1638, 910, 630, 482, 390, 315, 256 ];
const RV60_EDGE1: [isize; 4] = [ 0, 2, 2, 2 ];
const RV60_EDGE2: [isize; 4] = [ 0, 3, 3, 3 ];

