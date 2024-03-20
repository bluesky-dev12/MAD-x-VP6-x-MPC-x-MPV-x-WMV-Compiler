use nihav_core::frame::NAVideoBuffer;
use nihav_codec_support::codecs::MV;
use nihav_codec_support::codecs::blockdsp::edge_emu;
use super::clip8;

pub fn luma_mc(dst: &mut [u8], dstride: usize, pic: &NAVideoBuffer<u8>, xpos: usize, ypos: usize, mv: MV, is16: bool) {
    const RV40_EDGE1: [isize; 4] = [ 0, 2, 2, 2 ];
    const RV40_EDGE2: [isize; 4] = [ 0, 3, 3, 3 ];
    let dx = mv.x >> 2;
    let cx = (mv.x & 3) as usize;
    let dy = mv.y >> 2;
    let cy = (mv.y & 3) as usize;
    let mode = cx + cy * 4;

    let (w_, h_) = pic.get_dimensions(0);
    let w = (w_ + 15) & !15;
    let h = (h_ + 15) & !15;
    let (bsize, mc_func) = if is16 { (16, LUMA_MC_16[mode]) } else { (8, LUMA_MC_8[mode]) };

    if check_pos(xpos, ypos, bsize, w, h, dx, dy, RV40_EDGE1[cx], RV40_EDGE2[cx], RV40_EDGE1[cy], RV40_EDGE2[cy]) {
        let sstride = pic.get_stride(0);
        let mut soffset = pic.get_offset(0) + xpos + ypos * sstride;
        let data = pic.get_data();
        let src: &[u8] = data.as_slice();
        soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
        (mc_func)(dst, dstride, src, soffset, sstride);
    } else {
        let mut ebuf = [0u8; 32 * 22];
        edge_emu(pic, (xpos as isize) + (dx as isize) - 2, (ypos as isize) + (dy as isize) - 2, 16+5, 16+5, &mut ebuf, 32, 0, 4);
        (mc_func)(dst, dstride, &ebuf, 32 * 2 + 2, 32);
    }
}

pub fn chroma_mc(dst: &mut [u8], dstride: usize, pic: &NAVideoBuffer<u8>, xpos: usize, ypos: usize, comp: usize, mv: MV, is16: bool) {
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

    let (w_, h_) = pic.get_dimensions(0);
    let w = ((w_ + 15) & !15) >> 1;
    let h = ((h_ + 15) & !15) >> 1;
    let bsize = if is16 { 8 } else { 4 };

    if check_pos(xpos, ypos, bsize, w, h, dx, dy, 0, 1, 0, 1) {
        let sstride = pic.get_stride(comp);
        let mut soffset = pic.get_offset(comp) + xpos + ypos * sstride;
        let data = pic.get_data();
        let src: &[u8] = data.as_slice();
        soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
        rv40_chroma_mc(dst, dstride, src, soffset, sstride, bsize, cx, cy);
    } else {
        let mut ebuf = [0u8; 16 * 10];
        edge_emu(pic, (xpos as isize) + (dx as isize), (ypos as isize) + (dy as isize), bsize + 1, bsize + 1, &mut ebuf, 16, comp, 4);
        rv40_chroma_mc(dst, dstride, &ebuf, 0, 16, bsize, cx, cy);
    }
}

fn check_pos(x: usize, y: usize, size: usize, width: usize, height: usize, dx: i16, dy: i16, e0: isize, e1: isize, e2: isize, e3: isize) -> bool {
    let xn = (x as isize) + (dx as isize);
    let yn = (y as isize) + (dy as isize);

    (xn - e0 >= 0) && (xn + (size as isize) + e1 <= (width as isize)) && (yn - e2 >= 0) && (yn + (size as isize) + e3 <= (height as isize))
}

type MCFunc = fn (&mut [u8], usize, &[u8], usize, usize);

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
        fn $name (dst: &mut [u8], dstride: usize, src: &[u8], sidx: usize, sstride: usize) {
            for (dline, sline) in dst.chunks_mut(dstride).zip(src[sidx..].chunks(sstride)).take($size) {
                dline[..$size].copy_from_slice(&sline[..$size]);
            }
        }
        );
    (mc01; $name: ident, $size: expr, $ver: expr) => (
        fn $name (dst: &mut [u8], dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let step = if $ver { sstride } else { 1 };
            for dline in dst.chunks_mut(dstride).take($size) {
                for (x, el) in dline[..$size].iter_mut().enumerate() {
                    *el = filter!(01; src, sidx + x, step);
                }
                sidx += sstride;
            }
        }
        );
    (mc02; $name: ident, $size: expr, $ver: expr) => (
        fn $name (dst: &mut [u8], dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let step = if $ver { sstride } else { 1 };
            for dline in dst.chunks_mut(dstride).take($size) {
                for (x, el) in dline[..$size].iter_mut().enumerate() {
                    *el = filter!(02; src, sidx + x, step);
                }
                sidx += sstride;
            }
        }
        );
    (mc03; $name: ident, $size: expr, $ver: expr) => (
        fn $name (dst: &mut [u8], dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let step = if $ver { sstride } else { 1 };
            for dline in dst.chunks_mut(dstride).take($size) {
                for (x, el) in dline[..$size].iter_mut().enumerate() {
                    *el = filter!(03; src, sidx + x, step);
                }
                sidx += sstride;
            }
        }
        );
    (cm01; $name: ident, $size: expr, $ofilt: ident) => (
        fn $name (dst: &mut [u8], dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let mut buf: [u8; ($size + 5) * $size] = [0; ($size + 5) * $size];
            let mut bidx = 0;
            let bstride = $size;
            sidx -= sstride * 2;
            for _ in 0..$size+5 {
                for x in 0..$size { buf[bidx + x] = filter!(01; src, sidx + x, 1); }
                bidx += bstride;
                sidx += sstride;
            }
            $ofilt(dst, dstride, &buf, 2*bstride, $size);
        }
        );
    (cm02; $name: ident, $size: expr, $ofilt: ident) => (
        fn $name (dst: &mut [u8], dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let mut buf: [u8; ($size + 5) * $size] = [0; ($size + 5) * $size];
            let mut bidx = 0;
            let bstride = $size;
            sidx -= sstride * 2;
            for _ in 0..$size+5 {
                for x in 0..$size { buf[bidx + x] = filter!(02; src, sidx + x, 1); }
                bidx += bstride;
                sidx += sstride;
            }
            $ofilt(dst, dstride, &buf, 2*bstride, $size);
        }
        );
    (cm03; $name: ident, $size: expr, $ofilt: ident) => (
        fn $name (dst: &mut [u8], dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            let mut buf: [u8; ($size + 5) * $size] = [0; ($size + 5) * $size];
            let mut bidx = 0;
            let bstride = $size;
            sidx -= sstride * 2;
            for _ in 0..$size+5 {
                for x in 0..$size { buf[bidx + x] = filter!(03; src, sidx + x, 1); }
                bidx += bstride;
                sidx += sstride;
            }
            $ofilt(dst, dstride, &buf, 2*bstride, $size);
        }
        );
    (mc33; $name: ident, $size: expr) => (
        fn $name (dst: &mut [u8], dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
            for dline in dst.chunks_mut(dstride).take($size) {
                for (x, el) in dline[..$size].iter_mut().enumerate() {
                    *el = filter!(33; src, sidx + x, sstride);
                }
                sidx += sstride;
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

const LUMA_MC_16: [MCFunc; 16] = [
    copy_16,       luma_mc_10_16,  luma_mc_20_16, luma_mc_30_16,
    luma_mc_01_16, luma_mc_11_16,  luma_mc_21_16, luma_mc_31_16,
    luma_mc_02_16, luma_mc_12_16,  luma_mc_22_16, luma_mc_32_16,
    luma_mc_03_16, luma_mc_13_16,  luma_mc_23_16, luma_mc_33_16
];
const LUMA_MC_8: [MCFunc; 16] = [
    copy_8,        luma_mc_10_8,   luma_mc_20_8,  luma_mc_30_8,
    luma_mc_01_8,  luma_mc_11_8,   luma_mc_21_8,  luma_mc_31_8,
    luma_mc_02_8,  luma_mc_12_8,   luma_mc_22_8,  luma_mc_32_8,
    luma_mc_03_8,  luma_mc_13_8,   luma_mc_23_8,  luma_mc_33_8
];

#[allow(clippy::many_single_char_names)]
fn rv40_chroma_mc(dst: &mut [u8], dstride: usize, src: &[u8], mut sidx: usize, sstride: usize, size: usize, x: usize, y: usize) {
    const RV40_CHROMA_BIAS: [[u16; 4]; 4] = [
        [ 0, 4, 8, 4 ],
        [ 8, 7, 8, 7 ],
        [ 0, 8, 4, 8 ],
        [ 8, 7, 8, 7 ]
    ];

    if (x == 0) && (y == 0) {
        for (dline, sline) in dst.chunks_mut(dstride).zip(src[sidx..].chunks(sstride)).take(size) {
            dline[..size].copy_from_slice(&sline[..size]);
        }
        return;
    }
    let bias = RV40_CHROMA_BIAS[y >> 1][x >> 1];
    if (x > 0) && (y > 0) {
        let a = ((4 - x) * (4 - y)) as u16;
        let b = ((    x) * (4 - y)) as u16;
        let c = ((4 - x) * (    y)) as u16;
        let d = ((    x) * (    y)) as u16;
        for dline in dst.chunks_mut(dstride).take(size) {
            for (x, el) in dline[..size].iter_mut().enumerate() {
                *el = ((a * (src[sidx + x] as u16)
                      + b * (src[sidx + x + 1] as u16)
                      + c * (src[sidx + x + sstride] as u16)
                      + d * (src[sidx + x + 1 + sstride] as u16) + bias) >> 4) as u8;
            }
            sidx += sstride;
        }
    } else {
        let a = ((4 - x) * (4 - y)) as u16;
        let e = ((    x) * (4 - y) + (4 - x) * (    y)) as u16;
        let step = if y > 0 { sstride } else { 1 };
        for dline in dst.chunks_mut(dstride).take(size) {
            for (x, el) in dline[..size].iter_mut().enumerate() {
                *el = ((a * (src[sidx + x] as u16)
                      + e * (src[sidx + x + step] as u16) + bias) >> 4) as u8;
            }
            sidx += sstride;
        }
    }
}
