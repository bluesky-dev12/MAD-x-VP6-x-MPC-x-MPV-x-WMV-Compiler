use nihav_core::frame::{FrameType, NAVideoBuffer};
use nihav_codec_support::codecs::MV;
use nihav_codec_support::codecs::blockdsp::edge_emu;
use super::rv3040::{RV34DSP, RV34MBInfo};

fn clip8(a: i16) -> u8 {
    if a < 0 { 0 }
    else if a > 255 { 255 }
    else { a as u8 }
}

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
    (33; $s: ident, $o: expr, $stride: expr) => (
            clip8(((  el!($s, $o)
                    + el!($s, $o + 1)
                    + el!($s, $o + $stride)
                    + el!($s, $o + 1 + $stride) + 2) >> 2) as i16)
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
    (mc01; $name: ident, $size: expr, $ver: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let step = if $ver { sstride } else { 1 };
            for _ in 0..$size {
                for x in 0..$size {
                    dst[didx + x] = filter!(01; src, sidx + x, step);
                }
                sidx += sstride;
                didx += dstride;
            }
        }
        );
    (mc02; $name: ident, $size: expr, $ver: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let step = if $ver { sstride } else { 1 };
            for _ in 0..$size {
                for x in 0..$size {
                    dst[didx + x] = filter!(02; src, sidx + x, step);
                }
                sidx += sstride;
                didx += dstride;
            }
        }
        );
    (mc03; $name: ident, $size: expr, $ver: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let step = if $ver { sstride } else { 1 };
            for _ in 0..$size {
                for x in 0..$size {
                    dst[didx + x] = filter!(03; src, sidx + x, step);
                }
                sidx += sstride;
                didx += dstride;
            }
        }
        );
    (cm01; $name: ident, $size: expr, $ofilt: ident) => (
        fn $name (dst: &mut [u8], didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let mut buf: [u8; ($size + 5) * $size] = [0; ($size + 5) * $size];
            let mut bidx = 0;
            let bstride = $size;
            sidx -= sstride * 2;
            for _ in 0..$size+5 {
                for x in 0..$size { buf[bidx + x] = filter!(01; src, sidx + x, 1); }
                bidx += bstride;
                sidx += sstride;
            }
            $ofilt(dst, didx, dstride, &buf, 2*bstride, $size);
        }
        );
    (cm02; $name: ident, $size: expr, $ofilt: ident) => (
        fn $name (dst: &mut [u8], didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let mut buf: [u8; ($size + 5) * $size] = [0; ($size + 5) * $size];
            let mut bidx = 0;
            let bstride = $size;
            sidx -= sstride * 2;
            for _ in 0..$size+5 {
                for x in 0..$size { buf[bidx + x] = filter!(02; src, sidx + x, 1); }
                bidx += bstride;
                sidx += sstride;
            }
            $ofilt(dst, didx, dstride, &buf, 2*bstride, $size);
        }
        );
    (cm03; $name: ident, $size: expr, $ofilt: ident) => (
        fn $name (dst: &mut [u8], didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let mut buf: [u8; ($size + 5) * $size] = [0; ($size + 5) * $size];
            let mut bidx = 0;
            let bstride = $size;
            sidx -= sstride * 2;
            for _ in 0..$size+5 {
                for x in 0..$size { buf[bidx + x] = filter!(03; src, sidx + x, 1); }
                bidx += bstride;
                sidx += sstride;
            }
            $ofilt(dst, didx, dstride, &buf, 2*bstride, $size);
        }
        );
    (mc33; $name: ident, $size: expr) => (
        fn $name (dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            for _ in 0..$size {
                for x in 0..$size { dst[didx + x] = filter!(33; src, sidx + x, sstride); }
                sidx += sstride;
                didx += dstride;
            }
        }
        );
}
mc_func!(copy; copy_16, 16);
mc_func!(copy; copy_8,   8);
mc_func!(mc01; luma_mc_10_16, 16, false);
mc_func!(mc01; luma_mc_10_8,   8, false);
mc_func!(mc02; luma_mc_20_16, 16, false);
mc_func!(mc02; luma_mc_20_8,   8, false);
mc_func!(mc03; luma_mc_30_16, 16, false);
mc_func!(mc03; luma_mc_30_8,   8, false);
mc_func!(mc01; luma_mc_01_16, 16, true);
mc_func!(mc01; luma_mc_01_8,   8, true);
mc_func!(mc02; luma_mc_02_16, 16, true);
mc_func!(mc02; luma_mc_02_8,   8, true);
mc_func!(mc03; luma_mc_03_16, 16, true);
mc_func!(mc03; luma_mc_03_8,   8, true);
mc_func!(cm01; luma_mc_11_16, 16, luma_mc_01_16);
mc_func!(cm01; luma_mc_11_8,   8, luma_mc_01_8);
mc_func!(cm01; luma_mc_12_16, 16, luma_mc_02_16);
mc_func!(cm01; luma_mc_12_8,   8, luma_mc_02_8);
mc_func!(cm01; luma_mc_13_16, 16, luma_mc_03_16);
mc_func!(cm01; luma_mc_13_8,   8, luma_mc_03_8);
mc_func!(cm02; luma_mc_21_16, 16, luma_mc_01_16);
mc_func!(cm02; luma_mc_21_8,   8, luma_mc_01_8);
mc_func!(cm02; luma_mc_22_16, 16, luma_mc_02_16);
mc_func!(cm02; luma_mc_22_8,   8, luma_mc_02_8);
mc_func!(cm02; luma_mc_23_16, 16, luma_mc_03_16);
mc_func!(cm02; luma_mc_23_8,   8, luma_mc_03_8);
mc_func!(cm03; luma_mc_31_16, 16, luma_mc_01_16);
mc_func!(cm03; luma_mc_31_8,   8, luma_mc_01_8);
mc_func!(cm03; luma_mc_32_16, 16, luma_mc_02_16);
mc_func!(cm03; luma_mc_32_8,   8, luma_mc_02_8);
mc_func!(mc33; luma_mc_33_16, 16);
mc_func!(mc33; luma_mc_33_8,   8);

const RV40_CHROMA_BIAS: [[u16; 4]; 4] = [
    [ 0, 4, 8, 4 ],
    [ 8, 7, 8, 7 ],
    [ 0, 8, 4, 8 ],
    [ 8, 7, 8, 7 ]
];

fn rv40_chroma_mc(dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize, size: usize, x: usize, y: usize) {
    if (x == 0) && (y == 0) {
        for _ in 0..size {
            dst[didx..][..size].copy_from_slice(&src[sidx..][..size]);
            didx += dstride;
            sidx += sstride;
        }
        return;
    }
    let bias = RV40_CHROMA_BIAS[y >> 1][x >> 1];
    if (x > 0) && (y > 0) {
        let a = ((4 - x) * (4 - y)) as u16;
        let b = ((    x) * (4 - y)) as u16;
        let c = ((4 - x) * (    y)) as u16;
        let d = ((    x) * (    y)) as u16;
        for _ in 0..size {
            for x in 0..size {
                dst[didx + x] = ((a * (src[sidx + x] as u16)
                                + b * (src[sidx + x + 1] as u16)
                                + c * (src[sidx + x + sstride] as u16)
                                + d * (src[sidx + x + 1 + sstride] as u16) + bias) >> 4) as u8;
            }
            didx += dstride;
            sidx += sstride;
        }
    } else {
        let a = ((4 - x) * (4 - y)) as u16;
        let e = ((    x) * (4 - y) + (4 - x) * (    y)) as u16;
        let step = if y > 0 { sstride } else { 1 };
        for _ in 0..size {
            for x in 0..size {
                dst[didx + x] = ((a * (src[sidx + x] as u16)
                                + e * (src[sidx + x + step] as u16) + bias) >> 4) as u8;
            }
            didx += dstride;
            sidx += sstride;
        }
    }
}

#[allow(clippy::type_complexity)]
pub struct RV40DSP {
    luma_mc: [[fn (&mut [u8], usize, usize, &[u8], usize, usize); 16]; 2],
}

impl RV40DSP {
    pub fn new() -> Self {
        RV40DSP {
            luma_mc: [
                    [ copy_16,       luma_mc_10_16,  luma_mc_20_16, luma_mc_30_16,
                      luma_mc_01_16, luma_mc_11_16,  luma_mc_21_16, luma_mc_31_16,
                      luma_mc_02_16, luma_mc_12_16,  luma_mc_22_16, luma_mc_32_16,
                      luma_mc_03_16, luma_mc_13_16,  luma_mc_23_16, luma_mc_33_16 ],
                    [ copy_8,        luma_mc_10_8,   luma_mc_20_8,  luma_mc_30_8,
                      luma_mc_01_8,  luma_mc_11_8,   luma_mc_21_8,  luma_mc_31_8,
                      luma_mc_02_8,  luma_mc_12_8,   luma_mc_22_8,  luma_mc_32_8,
                      luma_mc_03_8,  luma_mc_13_8,   luma_mc_23_8,  luma_mc_33_8 ] ],
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

fn rv40_weak_loop_filter4(pix: &mut [u8], mut off: usize, step: usize, stride: usize,
                          filter_p1: bool, filter_q1: bool, alpha: i16, beta: i16,
                          lim_p0q0: i16, lim_p1: i16, lim_q1: i16) {
    for _ in 0..4 {
        let p0 = el!(pix, off -   step);
        let q0 = el!(pix, off);

        let t = q0 - p0;
        if t == 0 {
            off += stride;
            continue;
        }

        let u = (alpha * t.wrapping_abs()) >> 7;
        if u > (if filter_p1 && filter_q1 { 2 } else { 3 }) {
            off += stride;
            continue;
        }

        let p2 = el!(pix, off - 3*step);
        let p1 = el!(pix, off - 2*step);
        let q1 = el!(pix, off +   step);
        let q2 = el!(pix, off + 2*step);

        let strength;
        if filter_p1 && filter_q1 {
            strength = (t << 2) + (p1 - q1);
        } else {
            strength = t << 2;
        }

        let diff = clip_symm((strength + 4) >> 3, lim_p0q0);
        pix[off - step] = clip8(p0 + diff);
        pix[off       ] = clip8(q0 - diff);

        if filter_p1 && ((p1 - p2).wrapping_abs() <= beta) {
            let p1_diff = ((p1 - p0) + (p1 - p2) - diff) >> 1;
            pix[off - 2*step] = clip8(p1 - clip_symm(p1_diff, lim_p1));
        }

        if filter_q1 && ((q1 - q2).wrapping_abs() <= beta) {
            let q1_diff = ((q1 - q0) + (q1 - q2) + diff) >> 1;
            pix[off + step] = clip8(q1 - clip_symm(q1_diff, lim_q1));
        }

        off += stride;
    }
}

fn rv40_weak_loop_filter4_h(pix: &mut [u8], off: usize, stride: usize,
                            filter_p1: bool, filter_q1: bool, alpha: i16, beta: i16,
                            lim_p0q0: i16, lim_p1: i16, lim_q1: i16) {
    rv40_weak_loop_filter4(pix, off, stride, 1, filter_p1, filter_q1, alpha, beta, lim_p0q0, lim_p1, lim_q1);
}
#[allow(clippy::eq_op)]
fn rv40_weak_loop_filter4_v(pix: &mut [u8], off: usize, stride: usize,
                            filter_p1: bool, filter_q1: bool, alpha: i16, beta: i16,
                            lim_p0q0: i16, lim_p1: i16, lim_q1: i16) {
    let src = &mut pix[off - 3..][..stride * 3 + 3 + 3];
    for ch in src.chunks_mut(stride).take(4) {
        assert!(ch.len() >= 3 + 3);
        let p0 = el!(ch, 3 - 1);
        let q0 = el!(ch, 3);

        let t = q0 - p0;
        if t == 0 {
            continue;
        }

        let u = (alpha * t.wrapping_abs()) >> 7;
        if u > (if filter_p1 && filter_q1 { 2 } else { 3 }) {
            continue;
        }

        let p2 = el!(ch, 3 - 3);
        let p1 = el!(ch, 3 - 2);
        let q1 = el!(ch, 3 + 1);
        let q2 = el!(ch, 3 + 2);

        let strength;
        if filter_p1 && filter_q1 {
            strength = (t << 2) + (p1 - q1);
        } else {
            strength = t << 2;
        }

        let diff = clip_symm((strength + 4) >> 3, lim_p0q0);
        ch[3 - 1] = clip8(p0 + diff);
        ch[3    ] = clip8(q0 - diff);

        if filter_p1 && ((p1 - p2).wrapping_abs() <= beta) {
            let p1_diff = ((p1 - p0) + (p1 - p2) - diff) >> 1;
            ch[3 - 2] = clip8(p1 - clip_symm(p1_diff, lim_p1));
        }

        if filter_q1 && ((q1 - q2).wrapping_abs() <= beta) {
            let q1_diff = ((q1 - q0) + (q1 - q2) + diff) >> 1;
            ch[3 + 1] = clip8(q1 - clip_symm(q1_diff, lim_q1));
        }
    }
}


const RV40_DITHER_L: [i16; 16] = [
    0x40, 0x50, 0x20, 0x60, 0x30, 0x50, 0x40, 0x30,
    0x50, 0x40, 0x50, 0x30, 0x60, 0x20, 0x50, 0x40
];
const RV40_DITHER_R: [i16; 16] = [
    0x40, 0x30, 0x60, 0x20, 0x50, 0x30, 0x30, 0x40,
    0x40, 0x40, 0x50, 0x30, 0x20, 0x60, 0x30, 0x40
];

fn sfilter(a: i16, b: i16, c: i16, d: i16, e: i16, dither: i16, clip: bool, lims: i16) -> i16 {
    let val = (25 * (a + e) + 26 * (b + c + d) + dither) >> 7;
    if clip {
        if val < c - lims {
            c - lims
        } else if val > c + lims {
            c + lims
        } else {
            val
        }
    } else {
        val
    }
}

fn rv40_strong_loop_filter4(pix: &mut [u8], mut off: usize, step: usize, stride: usize,
                            alpha: i16, lims: i16, dmode: usize, chroma: bool) {
    for i in 0..4 {
        let p0 = el!(pix, off -   step);
        let q0 = el!(pix, off);

        let t = q0 - p0;
        if t == 0 {
            off += stride;
            continue;
        }

        let fmode = (alpha * t.wrapping_abs()) >> 7;
        if fmode > 1 {
            off += stride;
            continue;
        }

        let p3 = el!(pix, off - 4*step);
        let p2 = el!(pix, off - 3*step);
        let p1 = el!(pix, off - 2*step);
        let q1 = el!(pix, off +   step);
        let q2 = el!(pix, off + 2*step);
        let q3 = el!(pix, off + 3*step);

        let np0 = sfilter(p2, p1, p0, q0, q1,     RV40_DITHER_L[dmode + i], fmode != 0, lims);
        let nq0 = sfilter(    p1, p0, q0, q1, q2, RV40_DITHER_R[dmode + i], fmode != 0, lims);

        let np1 = sfilter(p3, p2, p1, np0, q0,              RV40_DITHER_L[dmode + i], fmode != 0, lims);
        let nq1 = sfilter(             p0, nq0, q1, q2, q3, RV40_DITHER_R[dmode + i], fmode != 0, lims);

        pix[off - 2*step] = np1 as u8;
        pix[off -   step] = np0 as u8;
        pix[off]          = nq0 as u8;
        pix[off +   step] = nq1 as u8;

        if !chroma {
            let np2 = sfilter(np0, np1, p2, p3, p2, 64, false, 0);
            let nq2 = sfilter(nq0, nq1, q2, q3, q2, 64, false, 0);
            pix[off - 3*step] = np2 as u8;
            pix[off + 2*step] = nq2 as u8;
        }

        off += stride;
    }
}

fn rv40_loop_strength(pix: &[u8], off: usize, step: usize, stride: usize,
                      beta: i16, beta2: i16, edge: bool) -> (bool, bool, bool) {
    let mut sum_p1p0 = 0;
    let mut sum_q1q0 = 0;

    let mut off1 = off;
    for _ in 0..4 {
        sum_p1p0 += el!(pix, off1 - 2 * step) - el!(pix, off1 - step);
        sum_q1q0 += el!(pix, off1 +     step) - el!(pix, off1);
        off1 += stride;
    }

    let filter_p1 = sum_p1p0.wrapping_abs() < beta * 4;
    let filter_q1 = sum_q1q0.wrapping_abs() < beta * 4;

    if (!filter_p1 || !filter_q1) || !edge {
        return (false, filter_p1, filter_q1);
    }

    let mut sum_p1p2 = 0;
    let mut sum_q1q2 = 0;

    let mut off1 = off;
    for _ in 0..4 {
        sum_p1p2 += el!(pix, off1 - 2 * step) - el!(pix, off1 - 3 * step);
        sum_q1q2 += el!(pix, off1 +     step) - el!(pix, off1 + 2 * step);
        off1 += stride;
    }

    let strong = (sum_p1p2.wrapping_abs() < beta2) && (sum_q1q2.wrapping_abs() < beta2);

    (strong, filter_p1, filter_q1)
}

fn rv40_loop_strength_h(pix: &[u8], off: usize, stride: usize,
                        beta: i16, beta2: i16, edge: bool) -> (bool, bool, bool) {
    rv40_loop_strength(pix, off, stride, 1, beta, beta2, edge)
}

#[allow(clippy::eq_op)]
fn rv40_loop_strength_v(pix: &[u8], off: usize, stride: usize,
                        beta: i16, beta2: i16, edge: bool) -> (bool, bool, bool) {
    let src = &pix[off - 3..][..stride * 3 + 3 + 3];
    let mut sum_p1p0 = 0;
    let mut sum_q1q0 = 0;

    for ch in src.chunks(stride).take(4) {
        assert!(ch.len() >= 3 + 3);
        sum_p1p0 += el!(ch, 3 - 2) - el!(ch, 3 - 1);
        sum_q1q0 += el!(ch, 3 + 1) - el!(ch, 3);
    }

    let filter_p1 = sum_p1p0.wrapping_abs() < beta * 4;
    let filter_q1 = sum_q1q0.wrapping_abs() < beta * 4;

    if (!filter_p1 || !filter_q1) || !edge {
        return (false, filter_p1, filter_q1);
    }

    let mut sum_p1p2 = 0;
    let mut sum_q1q2 = 0;

    for ch in src.chunks(stride).take(4) {
        assert!(ch.len() >= 3 + 3);
        sum_p1p2 += el!(ch, 3 - 2) - el!(ch, 3 - 3);
        sum_q1q2 += el!(ch, 3 + 1) - el!(ch, 3 + 2);
    }

    let strong = (sum_p1p2.wrapping_abs() < beta2) && (sum_q1q2.wrapping_abs() < beta2);

    (strong, filter_p1, filter_q1)
}

fn rv40_loop_filter4_h(pix: &mut [u8], off: usize, stride: usize,
                     dmode: usize, lim_p1: i16, lim_q1: i16, alpha: i16, beta: i16, beta2: i16,
                     chroma: bool, edge: bool) {
    let (strong, filter_p1, filter_q1) = rv40_loop_strength_h(pix, off, stride, beta, beta2, edge);
    let lims = (filter_p1 as i16) + (filter_q1 as i16) + ((lim_p1 + lim_q1) >> 1) + 1;

    if strong {
        rv40_strong_loop_filter4(pix, off, stride, 1, alpha, lims, dmode, chroma);
    } else if filter_p1 && filter_q1 {
        rv40_weak_loop_filter4_h(pix, off, stride, true, true, alpha, beta,
                                 lims, lim_p1, lim_q1);
    } else if filter_p1 || filter_q1 {
        rv40_weak_loop_filter4_h(pix, off, stride, filter_p1, filter_q1, alpha, beta,
                                 lims >> 1, lim_p1 >> 1, lim_q1 >> 1);
    }
}

fn rv40_loop_filter4_v(pix: &mut [u8], off: usize, stride: usize,
                     dmode: usize, lim_p1: i16, lim_q1: i16, alpha: i16, beta: i16, beta2: i16,
                     chroma: bool, edge: bool) {
    let (strong, filter_p1, filter_q1) = rv40_loop_strength_v(pix, off, stride, beta, beta2, edge);
    let lims = (filter_p1 as i16) + (filter_q1 as i16) + ((lim_p1 + lim_q1) >> 1) + 1;

    if strong {
        rv40_strong_loop_filter4(pix, off, 1, stride, alpha, lims, dmode, chroma);
    } else if filter_p1 && filter_q1 {
        rv40_weak_loop_filter4_v(pix, off, stride, true, true, alpha, beta,
                                 lims, lim_p1, lim_q1);
    } else if filter_p1 || filter_q1 {
        rv40_weak_loop_filter4_v(pix, off, stride, filter_p1, filter_q1, alpha, beta,
                                 lims >> 1, lim_p1 >> 1, lim_q1 >> 1);
    }
}

const RV40_ALPHA_TAB: [i16; 32] = [
    128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 122,  96,  75,  59,  47,  37,
     29,  23,  18,  15,  13,  11,  10,   9,
      8,   7,   6,   5,   4,   3,   2,   1
];

const RV40_BETA_TAB: [i16; 32] = [
     0,  0,  0,  0,  0,  0,  0,  0,  3,  3,  3,  4,  4,  4,  6,  6,
     6,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 13, 14, 15, 16, 17
];

const RV40_FILTER_CLIP_TBL: [[i16; 32]; 3] = [
  [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ], [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 5, 5
  ], [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
    1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 7, 8, 9
  ]
];

macro_rules! test_bit {
    ($pat: expr, $x: expr) => ( (($pat >> $x) & 1) != 0 )
}

fn check_pos(x: usize, y: usize, size: usize, w: usize, h: usize, dx: i16, dy: i16, e0: isize, e1: isize, e2: isize, e3: isize) -> bool {
    let xn = (x as isize) + (dx as isize);
    let yn = (y as isize) + (dy as isize);

    (xn - e0 >= 0) && (xn + (size as isize) + e1 <= (w as isize)) && (yn - e2 >= 0) && (yn + (size as isize) + e3 <= (h as isize))
}

const RV40_EDGE1: [isize; 4] = [ 0, 2, 2, 2 ];
const RV40_EDGE2: [isize; 4] = [ 0, 3, 3, 3 ];

const Y_TOP_ROW_MASK:   u32 = 0x000F;
const Y_BOT_ROW_MASK:   u32 = 0xF000;
const Y_LEFT_COL_MASK:  u32 = 0x1111;
const Y_RIGHT_COL_MASK: u32 = 0x8888;
const C_TOP_ROW_MASK:   u32 = 0x3;
const C_BOT_ROW_MASK:   u32 = 0xC;
const C_LEFT_COL_MASK:  u32 = 0x5;
const C_RIGHT_COL_MASK: u32 = 0xA;

impl RV34DSP for RV40DSP {
    #[allow(clippy::cognitive_complexity)]
    fn loop_filter(&self, frame: &mut NAVideoBuffer<u8>, _ftype: FrameType, mbinfo: &[RV34MBInfo], mb_w: usize, mb_h: usize, row: usize) {
        // todo proper B-frame filtering?
        let mut offs:   [usize; 3] = [0; 3];
        let mut stride: [usize; 3] = [0; 3];
        let (w, h) = frame.get_dimensions(0);
        let small_frame = w * h <= 176*144;

        for comp in 0..3 {
            stride[comp] = frame.get_stride(comp);
            let start = if comp == 0 { row * 16 } else { row * 8 };
            offs[comp] = frame.get_offset(comp) + start * stride[comp];
        }

        let data = frame.get_data_mut().unwrap();
        let dst: &mut [u8] = data.as_mut_slice();

        let is_last_row = row == mb_h - 1;

        let mut mb_pos: usize = row * mb_w;
        let mut left_q: usize = 0;
        let mut left_cbp = 0;
        let mut left_dbk = 0;
        for mb_x in 0..mb_w {
            let q = mbinfo[mb_pos].q as usize;
            let alpha = RV40_ALPHA_TAB[q];
            let beta  = RV40_BETA_TAB[q];
            let beta_y = if small_frame { beta * 4 } else { beta * 3 };
            let beta_c = beta * 3;

            let is_strong = mbinfo[mb_pos].mbtype.is_intra_or_16();
            let top_is_strong = row > 0 && mbinfo[mb_pos - mb_w].mbtype.is_intra_or_16();
            let left_is_strong = mb_x > 0 && mbinfo[mb_pos - 1].mbtype.is_intra_or_16();
            let bot_is_strong = !is_last_row && mbinfo[mb_pos + mb_w].mbtype.is_intra_or_16();

            let cur_dbk = mbinfo[mb_pos].deblock;
            let cur_cbp = if is_strong { 0xFFFFFF } else { mbinfo[mb_pos].cbp };

            let (top_cbp, top_dbk) = if row > 0 {
                    (if top_is_strong { 0xFFFFFF } else { mbinfo[mb_pos - mb_w].cbp }, mbinfo[mb_pos - mb_w].deblock)
                } else {
                    (0, 0)
                };
            let (bot_cbp, bot_dbk) = if !is_last_row {
                    (mbinfo[mb_pos + mb_w].cbp, mbinfo[mb_pos + mb_w].deblock)
                } else {
                    (0, 0)
                };

            let y_cbp = cur_cbp & 0xFFFF;
            let y_to_deblock = (cur_dbk as u32) | ((bot_dbk as u32) << 16);
            let mut y_h_deblock = y_to_deblock | ((y_cbp << 4) & !Y_TOP_ROW_MASK) | ((top_cbp & Y_BOT_ROW_MASK) >> 12);
            let mut y_v_deblock = y_to_deblock | ((y_cbp << 1) & !Y_LEFT_COL_MASK) | ((left_cbp & Y_RIGHT_COL_MASK) >> 3);

            if mb_x == 0 {
                y_v_deblock &= !Y_LEFT_COL_MASK;
            }
            if row == 0 {
                y_h_deblock &= !Y_TOP_ROW_MASK;
            }
            if is_last_row || is_strong || bot_is_strong {
                y_h_deblock &= !(Y_TOP_ROW_MASK << 16);
            }

            for y in 0..4 {
                let yoff = offs[0] + mb_x * 16 + y * 4 * stride[0];
                for x in 0..4 {
                    let bpos = x + y * 4;
                    let ver_strong = (x == 0) && (mb_x > 0) && (is_strong || left_is_strong);

                    let cur_strength: usize;
                    if is_strong {
                        cur_strength = 2;
                    } else if test_bit!(cur_dbk, bpos) {
                        cur_strength = 1;
                    } else {
                        cur_strength = 0;
                    }

                    let left_strength: usize;
                    if x > 0 {
                        if is_strong {
                            left_strength = 2;
                        } else if test_bit!(cur_dbk, bpos - 1) {
                            left_strength = 1;
                        } else {
                            left_strength = 0;
                        }
                    } else if mb_x > 0 {
                        if left_is_strong {
                            left_strength = 2;
                        } else if test_bit!(left_dbk, bpos + 3) {
                            left_strength = 1;
                        } else {
                            left_strength = 0;
                        }
                    } else {
                        left_strength = 0;
                    }

                    let bot_strength: usize;
                    if y < 3 {
                        if is_strong {
                            bot_strength = 2;
                        } else if test_bit!(cur_dbk, bpos + 4) {
                            bot_strength = 1;
                        } else {
                            bot_strength = 0;
                        }
                    } else if !is_last_row {
                        if mbinfo[mb_pos + mb_w].mbtype.is_intra_or_16() {
                            bot_strength = 2;
                        } else if test_bit!(bot_dbk, x) {
                            bot_strength = 1;
                        } else {
                            bot_strength = 0;
                        }
                    } else {
                        bot_strength = 0;
                    }

                    let top_strength: usize;
                    if y > 0 {
                        if is_strong {
                            top_strength = 2;
                        } else if test_bit!(cur_dbk, bpos - 4) {
                            top_strength = 1;
                        } else {
                            top_strength = 0;
                        }
                    } else if row > 0 {
                        if top_is_strong {
                            top_strength = 2;
                        } else if test_bit!(top_dbk, bpos + 12) {
                            top_strength = 1;
                        } else {
                            top_strength = 0;
                        }
                    } else {
                        top_strength = 0;
                    }

                    let l_q = if x > 0 { q } else { left_q };
                    let top_q = if row > 0 { mbinfo[mb_pos - mb_w].q as usize } else { 0 };

                    let lim_cur     = RV40_FILTER_CLIP_TBL [cur_strength][q];
                    let lim_top     = RV40_FILTER_CLIP_TBL [top_strength][top_q];
                    let lim_left    = RV40_FILTER_CLIP_TBL[left_strength][l_q];
                    let lim_bottom  = RV40_FILTER_CLIP_TBL [bot_strength][q];

                    let dmode = if y > 0 { x + y * 4 } else { x * 4 };

                    if test_bit!(y_h_deblock, bpos + 4) {
                        rv40_loop_filter4_h(dst, yoff + 4 * stride[0] + x * 4, stride[0],
                                            dmode, lim_cur, lim_bottom, alpha, beta, beta_y, false, false);
                    }
                    if test_bit!(y_v_deblock, bpos) && !ver_strong {
                        rv40_loop_filter4_v(dst, yoff + x * 4, stride[0],
                                            dmode, lim_left, lim_cur, alpha, beta, beta_y, false, false);
                    }
                    if (y == 0) && test_bit!(y_h_deblock, bpos) && (is_strong || top_is_strong) {
                        rv40_loop_filter4_h(dst, yoff + x * 4, stride[0],
                                            dmode, lim_top, lim_cur, alpha, beta, beta_y, false, true);
                    }
                    if test_bit!(y_v_deblock, bpos) && ver_strong {
                        rv40_loop_filter4_v(dst, yoff + x * 4, stride[0],
                                            dmode, lim_left, lim_cur, alpha, beta, beta_y, false, true);
                    }
                }
            }

            for comp in 1..3 {
                let cshift = 16 - 4 + comp * 4;
                let c_cur_cbp  = (cur_cbp  >> cshift) & 0xF;
                let c_top_cbp  = (top_cbp  >> cshift) & 0xF;
                let c_left_cbp = (left_cbp >> cshift) & 0xF;
                let c_bot_cbp  = (bot_cbp  >> cshift) & 0xF;

                let c_deblock = c_cur_cbp | (c_bot_cbp << 4);
                let mut c_v_deblock = c_deblock | ((c_cur_cbp << 1) & !C_LEFT_COL_MASK) | ((c_left_cbp & C_RIGHT_COL_MASK) >> 1);
                let mut c_h_deblock = c_deblock | ((c_cur_cbp & C_TOP_ROW_MASK) << 2) | ((c_top_cbp & C_BOT_ROW_MASK) >> 2);
                if mb_x == 0 {
                    c_v_deblock &= !C_LEFT_COL_MASK;
                }
                if row == 0 {
                    c_h_deblock &= !C_TOP_ROW_MASK;
                }
                if is_last_row || is_strong || bot_is_strong {
                    c_h_deblock &= !(C_TOP_ROW_MASK << 4);
                }

                for y in 0..2 {
                    let coff = offs[comp] + mb_x * 8 + y * 4 * stride[comp];
                    for x in 0..2 {
                        let bpos = x + y * 2;

                        let ver_strong = (x == 0) && (is_strong || left_is_strong);

                        let cur_strength: usize;
                        if is_strong {
                            cur_strength = 2;
                        } else if test_bit!(c_cur_cbp, bpos) {
                            cur_strength = 1;
                        } else {
                            cur_strength = 0;
                        }

                        let left_strength: usize;
                        if x > 0 {
                            if is_strong {
                                left_strength = 2;
                            } else if test_bit!(c_cur_cbp, bpos - 1) {
                                left_strength = 1;
                            } else {
                                left_strength = 0;
                            }
                        } else if mb_x > 0 {
                            if left_is_strong {
                                left_strength = 2;
                            } else if test_bit!(c_left_cbp, bpos + 1) {
                                left_strength = 1;
                            } else {
                                left_strength = 0;
                            }
                        } else {
                            left_strength = 0;
                        }

                        let bot_strength: usize;
                        if y != 3 {
                            if is_strong {
                                bot_strength = 2;
                            } else if test_bit!(c_cur_cbp, bpos + 2) {
                                bot_strength = 1;
                            } else {
                                bot_strength = 0;
                            }
                        } else if !is_last_row {
                            if mbinfo[mb_pos + mb_w].mbtype.is_intra_or_16() {
                                bot_strength = 2;
                            } else if test_bit!(c_bot_cbp, x) {
                                bot_strength = 1;
                            } else {
                                bot_strength = 0;
                            }
                        } else {
                            bot_strength = 0;
                        }

                        let top_strength: usize;
                        if y > 0 {
                            if is_strong {
                                top_strength = 2;
                            } else if test_bit!(c_cur_cbp, bpos - 2) {
                                top_strength = 1;
                            } else {
                                top_strength = 0;
                            }
                        } else if row > 0 {
                            if top_is_strong {
                                top_strength = 2;
                            } else if test_bit!(c_top_cbp, bpos + 2) {
                                top_strength = 1;
                            } else {
                                top_strength = 0;
                            }
                        } else {
                            top_strength = 0;
                        }

                        let l_q = if x > 0 { q } else { left_q };
                        let top_q = if row > 0 { mbinfo[mb_pos - mb_w].q as usize } else { 0 };

                        let lim_cur     = RV40_FILTER_CLIP_TBL [cur_strength][q];
                        let lim_top     = RV40_FILTER_CLIP_TBL [top_strength][top_q];
                        let lim_left    = RV40_FILTER_CLIP_TBL[left_strength][l_q];
                        let lim_bottom  = RV40_FILTER_CLIP_TBL [bot_strength][q];

                        if test_bit!(c_h_deblock, bpos + 2) {
                            rv40_loop_filter4_h(dst, coff + 4 * stride[comp] + x * 4, stride[comp],
                                                x * 8, lim_cur, lim_bottom, alpha, beta, beta_c, true, false);
                        }
                        if test_bit!(c_v_deblock, bpos) && !ver_strong {
                            rv40_loop_filter4_v(dst, coff + x * 4, stride[comp],
                                                y * 8, lim_left, lim_cur, alpha, beta, beta_c, true, false);
                        }
                        if (y == 0) && test_bit!(c_h_deblock, bpos) && (is_strong || top_is_strong) {
                            rv40_loop_filter4_h(dst, coff + x * 4, stride[comp],
                                                x * 8, lim_top, lim_cur, alpha, beta, beta_c, true, true);
                        }
                        if test_bit!(c_v_deblock, bpos) && ver_strong {
                            rv40_loop_filter4_v(dst, coff + x * 4, stride[comp],
                                                y * 8, lim_left, lim_cur, alpha, beta, beta_c, true, true);
                        }
                    }
                }
            }

            left_q = q;
            left_dbk = cur_dbk;
            left_cbp = cur_cbp;

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

        let dx = mv.x >> 2;
        let cx = (mv.x & 3) as usize;
        let dy = mv.y >> 2;
        let cy = (mv.y & 3) as usize;
        let mode = cx + cy * 4;

        if check_pos(x, y, size, w, h, dx, dy, RV40_EDGE1[cx], RV40_EDGE2[cx], RV40_EDGE1[cy], RV40_EDGE2[cy]) {
            let sstride = prev_frame.get_stride(0);
            let mut soffset = prev_frame.get_offset(0) + x + y * sstride;
            let data = prev_frame.get_data();
            let src: &[u8] = data.as_slice();
            soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
            self.luma_mc[if use16 { 0 } else { 1 }][mode](dst, doffset, dstride, src, soffset, sstride);
        } else {
            let mut ebuf: [u8; 32*22] = [0; 32*22];
            edge_emu(prev_frame, (x as isize) + (dx as isize) - 2, (y as isize) + (dy as isize) - 2, 16+5, 16+5, &mut ebuf, 32, 0, 4);
            self.luma_mc[if use16 { 0 } else { 1 }][mode](dst, doffset, dstride, &ebuf, 32*2 + 2, 32);
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

        let mvx = mv.x / 2;
        let mvy = mv.y / 2;
        let dx = mvx >> 2;
        let mut cx = (mvx & 3) as usize;
        let dy = mvy >> 2;
        let mut cy = (mvy & 3) as usize;

        if (cx == 3) && (cy == 3) {
            cx = 2;
            cy = 2;
        }

        if check_pos(x, y, size, w, h, dx, dy, 0, 1, 0, 1) {
            let sstride = prev_frame.get_stride(comp);
            let mut soffset = prev_frame.get_offset(comp) + x + y * sstride;
            let data = prev_frame.get_data();
            let src: &[u8] = data.as_slice();
            soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
            rv40_chroma_mc(dst, doffset, dstride, src, soffset, sstride, size, cx, cy);
        } else {
            let mut ebuf: [u8; 16*10] = [0; 16*10];
            edge_emu(prev_frame, (x as isize) + (dx as isize), (y as isize) + (dy as isize), 8+1, 8+1, &mut ebuf, 16, comp, 4);
            rv40_chroma_mc(dst, doffset, dstride, &ebuf, 0, 16, size, cx, cy);
        }
    }
}
