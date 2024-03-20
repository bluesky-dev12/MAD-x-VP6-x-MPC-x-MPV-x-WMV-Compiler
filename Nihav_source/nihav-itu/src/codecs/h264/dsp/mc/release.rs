use super::clip_u8;

const TMP_BUF_STRIDE: usize = 32;

fn interp_block1(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize, hor: bool, avg0: bool) {
    unsafe {
        let step = if hor { 1 } else { sstride };
        let avgidx = if avg0 { step * 2 } else { step * 3 };
        let mut src = src.as_ptr();
        let mut dst = dst.as_mut_ptr();
        for _ in 0..h {
            for _ in 0..w {
                let t = clip_u8((       i16::from(*src)
                                 - 5  * i16::from(*src.add(step))
                                 + 20 * i16::from(*src.add(step * 2))
                                 + 20 * i16::from(*src.add(step * 3))
                                 - 5  * i16::from(*src.add(step * 4))
                                 +      i16::from(*src.add(step * 5))
                                 + 16) >> 5);
                *dst = ((u16::from(t) + u16::from(*src.add(avgidx)) + 1) >> 1) as u8;
                src = src.add(1);
                dst = dst.add(1);
            }
            dst = dst.sub(w).add(dstride);
            src = src.sub(w).add(sstride);
        }
    }
}

fn interp_block2(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize, hor: bool) {
    unsafe {
        let step = if hor { 1 } else { sstride };
        let mut pix = dst.as_mut_ptr();
        let mut src = src.as_ptr();
        for _ in 0..h {
            for x in 0..w {
                *pix.add(x) = clip_u8((       i16::from(*src)
                                       - 5  * i16::from(*src.add(step))
                                       + 20 * i16::from(*src.add(step * 2))
                                       + 20 * i16::from(*src.add(step * 3))
                                       - 5  * i16::from(*src.add(step * 4))
                                       +      i16::from(*src.add(step * 5))
                                       + 16) >> 5);
                src = src.add(1);
            }
            pix = pix.add(dstride);
            src = src.sub(w);
            src = src.add(sstride);
        }
    }
}

fn mc_avg_tmp(dst: &mut [u8], dstride: usize, w: usize, h: usize, tmp: &[u8], tmp2: &[u8]) {
    unsafe {
        let mut src1 = tmp.as_ptr();
        let mut src2 = tmp2.as_ptr();
        let mut dst = dst.as_mut_ptr();
        for _ in 0..h {
            for x in 0..w {
                let a = *src1.add(x);
                let b = *src2.add(x);
                *dst.add(x) = ((u16::from(a) + u16::from(b) + 1) >> 1) as u8;
            }
            dst = dst.add(dstride);
            src1 = src1.add(TMP_BUF_STRIDE);
            src2 = src2.add(TMP_BUF_STRIDE);
        }
    }
}

fn h264_mc00(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    unsafe {
        let mut src = src.as_ptr();
        let mut dst = dst.as_mut_ptr();
        for _ in 0..h {
            std::ptr::copy_nonoverlapping(src, dst, w);
            src = src.add(sstride);
            dst = dst.add(dstride);
        }
    }
}

fn h264_mc01(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    interp_block1(dst, dstride, &src[sstride * 2..], sstride, w, h, true, true);
}

fn h264_mc02(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    interp_block2(dst, dstride, &src[sstride * 2..], sstride, w, h, true);
}

fn h264_mc03(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    interp_block1(dst, dstride, &src[sstride * 2..], sstride, w, h, true, false);
}

fn h264_mc10(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    interp_block1(dst, dstride, &src[2..], sstride, w, h, false, true);
}

fn h264_mc11(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp : [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    let mut tmp2: [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    h264_mc02(&mut tmp,  TMP_BUF_STRIDE, src, sstride, w, h);
    h264_mc20(&mut tmp2, TMP_BUF_STRIDE, src, sstride, w, h);
    mc_avg_tmp(dst, dstride, w, h, &tmp, &tmp2);
}

fn h264_mc12(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp : [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    let mut tmp2: [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    h264_mc02(&mut tmp,  TMP_BUF_STRIDE, src, sstride, w, h);
    h264_mc22(&mut tmp2, TMP_BUF_STRIDE, src, sstride, w, h);
    mc_avg_tmp(dst, dstride, w, h, &tmp, &tmp2);
}

fn h264_mc13(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp : [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    let mut tmp2: [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    h264_mc02(&mut tmp,  TMP_BUF_STRIDE, src, sstride, w, h);
    h264_mc20(&mut tmp2, TMP_BUF_STRIDE, &src[1..], sstride, w, h);
    mc_avg_tmp(dst, dstride, w, h, &tmp, &tmp2);
}

fn h264_mc20(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    interp_block2(dst, dstride, &src[2..], sstride, w, h, false);
}

fn h264_mc21(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp : [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    let mut tmp2: [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    h264_mc22(&mut tmp,  TMP_BUF_STRIDE, src, sstride, w, h);
    h264_mc20(&mut tmp2, TMP_BUF_STRIDE, src, sstride, w, h);
    mc_avg_tmp(dst, dstride, w, h, &tmp, &tmp2);
}

fn h264_mc22(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp: [i32; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    unsafe {
        let mut src = src.as_ptr();
        let mut dst = tmp.as_mut_ptr();
        for _ in 0..h {
            for _ in 0..w+5 {
                *dst =        i32::from(*src)
                       - 5  * i32::from(*src.add(sstride))
                       + 20 * i32::from(*src.add(sstride * 2))
                       + 20 * i32::from(*src.add(sstride * 3))
                       - 5  * i32::from(*src.add(sstride * 4))
                       +      i32::from(*src.add(sstride * 5));
                dst = dst.add(1);
                src = src.add(1);
            }
            src = src.sub(w+5).add(sstride);
            dst = dst.sub(w+5).add(TMP_BUF_STRIDE);
        }
    }
    unsafe {
        let mut dst = dst.as_mut_ptr();
        let mut src = tmp.as_ptr();
        for _ in 0..h {
            for _ in 0..w {
                *dst = clip_u8(((*src - 5 * *src.add(1) + 20 * *src.add(2) + 20 * *src.add(3) - 5 * *src.add(4) + *src.add(5) + 512) >> 10) as i16);
                dst = dst.add(1);
                src = src.add(1);
            }
            dst = dst.sub(w).add(dstride);
            src = src.sub(w).add(TMP_BUF_STRIDE);
        }
    }
}

fn h264_mc23(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp : [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    let mut tmp2: [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    h264_mc22(&mut tmp,  TMP_BUF_STRIDE, src, sstride, w, h);
    h264_mc20(&mut tmp2, TMP_BUF_STRIDE, &src[1..], sstride, w, h);
    mc_avg_tmp(dst, dstride, w, h, &tmp, &tmp2);
}

fn h264_mc30(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    interp_block1(dst, dstride, &src[2..], sstride, w, h, false, false);
}

fn h264_mc31(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp : [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    let mut tmp2: [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    h264_mc20(&mut tmp,  TMP_BUF_STRIDE, src, sstride, w, h);
    h264_mc02(&mut tmp2, TMP_BUF_STRIDE, &src[sstride..], sstride, w, h);
    mc_avg_tmp(dst, dstride, w, h, &tmp, &tmp2);
}

fn h264_mc32(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp : [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    let mut tmp2: [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    h264_mc22(&mut tmp,  TMP_BUF_STRIDE, src, sstride, w, h);
    h264_mc02(&mut tmp2, TMP_BUF_STRIDE, &src[sstride..], sstride, w, h);
    mc_avg_tmp(dst, dstride, w, h, &tmp, &tmp2);
}

fn h264_mc33(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, w: usize, h: usize) {
    let mut tmp : [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    let mut tmp2: [u8; TMP_BUF_STRIDE * 16] = unsafe { let arr = std::mem::MaybeUninit::uninit(); arr.assume_init() };
    h264_mc20(&mut tmp,  TMP_BUF_STRIDE, &src[1..], sstride, w, h);
    h264_mc02(&mut tmp2, TMP_BUF_STRIDE, &src[sstride..], sstride, w, h);
    mc_avg_tmp(dst, dstride, w, h, &tmp, &tmp2);
}


fn chroma_interp(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, dx: u16, dy: u16, w: usize, h: usize) {
    let a0 = 8 - dx;
    let a1 = dx;
    let b0 = 8 - dy;
    let b1 = dy;

    if a0 == 8 && b0 == 8 {
        unsafe {
            let mut src = src.as_ptr();
            let mut dst = dst.as_mut_ptr();
            for _ in 0..h {
                std::ptr::copy_nonoverlapping(src, dst, w);
                src = src.add(sstride);
                dst = dst.add(dstride);
            }
        }
    } else if a0 == 8 {
        unsafe {
            let mut src0 = src.as_ptr();
            let mut src1 = src0.add(sstride);
            let mut dst = dst.as_mut_ptr();
            for _ in 0..h {
                for x in 0..w {
                    let a = *src0.add(x);
                    let b = *src1.add(x);
                    *dst.add(x) = ((u16::from(a) * b0 + u16::from(b) * b1 + 4) >> 3) as u8;
                }
                src0 = src0.add(sstride);
                src1 = src1.add(sstride);
                dst = dst.add(dstride);
            }
        }
    } else if b0 == 8 {
        unsafe {
            let mut src = src.as_ptr();
            let mut dst = dst.as_mut_ptr();
            for _ in 0..h {
                let mut a = *src;
                for x in 0..w {
                    let b = *src.add(x + 1);
                    *dst.add(x) = ((u16::from(a) * a0 + u16::from(b) * a1 + 4) >> 3) as u8;
                    a = b;
                }
                src = src.add(sstride);
                dst = dst.add(dstride);
            }
        }
    } else {
        unsafe {
            let mut src0 = src.as_ptr();
            let mut src1 = src0.add(sstride);
            let mut dst = dst.as_mut_ptr();
            for _ in 0..h {
                let mut a = *src0;
                let mut c = *src1;
                for x in 0..w {
                    let b = *src0.add(x + 1);
                    let d = *src1.add(x + 1);
                    *dst.add(x) = ((u16::from(a) * a0 * b0 + u16::from(b) * a1 * b0 + u16::from(c) * a0 * b1 + u16::from(d) * a1 * b1 + 0x20) >> 6) as u8;
                    a = b;
                    c = d;
                }
                src0 = src0.add(sstride);
                src1 = src1.add(sstride);
                dst = dst.add(dstride);
            }
        }
    }
}

pub fn chroma_interp_8(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, dx: u16, dy: u16, h: usize) {
    chroma_interp(dst, dstride, src, sstride, dx, dy, 8, h);
}

pub fn chroma_interp_4(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, dx: u16, dy: u16, h: usize) {
    chroma_interp(dst, dstride, src, sstride, dx, dy, 4, h);
}

pub fn chroma_interp_2(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, dx: u16, dy: u16, h: usize) {
    chroma_interp(dst, dstride, src, sstride, dx, dy, 2, h);
}

macro_rules! luma_mc {
    ($orig:ident, $func4:ident, $func8:ident, $func16:ident) => {
        fn $func4(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, h: usize) {
            $orig(dst, dstride, src, sstride, 4, h);
        }
        fn $func8(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, h: usize) {
            $orig(dst, dstride, src, sstride, 8, h);
        }
        fn $func16(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, h: usize) {
            $orig(dst, dstride, src, sstride, 16, h);
        }
    }
}

luma_mc!(h264_mc00, h264_mc00_4, h264_mc00_8, h264_mc00_16);
luma_mc!(h264_mc01, h264_mc01_4, h264_mc01_8, h264_mc01_16);
luma_mc!(h264_mc02, h264_mc02_4, h264_mc02_8, h264_mc02_16);
luma_mc!(h264_mc03, h264_mc03_4, h264_mc03_8, h264_mc03_16);
luma_mc!(h264_mc10, h264_mc10_4, h264_mc10_8, h264_mc10_16);
luma_mc!(h264_mc11, h264_mc11_4, h264_mc11_8, h264_mc11_16);
luma_mc!(h264_mc12, h264_mc12_4, h264_mc12_8, h264_mc12_16);
luma_mc!(h264_mc13, h264_mc13_4, h264_mc13_8, h264_mc13_16);
luma_mc!(h264_mc20, h264_mc20_4, h264_mc20_8, h264_mc20_16);
luma_mc!(h264_mc21, h264_mc21_4, h264_mc21_8, h264_mc21_16);
luma_mc!(h264_mc22, h264_mc22_4, h264_mc22_8, h264_mc22_16);
luma_mc!(h264_mc23, h264_mc23_4, h264_mc23_8, h264_mc23_16);
luma_mc!(h264_mc30, h264_mc30_4, h264_mc30_8, h264_mc30_16);
luma_mc!(h264_mc31, h264_mc31_4, h264_mc31_8, h264_mc31_16);
luma_mc!(h264_mc32, h264_mc32_4, h264_mc32_8, h264_mc32_16);
luma_mc!(h264_mc33, h264_mc33_4, h264_mc33_8, h264_mc33_16);

pub const H264_LUMA_INTERP: &[[super::MCFunc; 16]; 3] = &[
  [
    h264_mc00_4, h264_mc01_4, h264_mc02_4, h264_mc03_4,
    h264_mc10_4, h264_mc11_4, h264_mc12_4, h264_mc13_4,
    h264_mc20_4, h264_mc21_4, h264_mc22_4, h264_mc23_4,
    h264_mc30_4, h264_mc31_4, h264_mc32_4, h264_mc33_4
  ], [
    h264_mc00_8, h264_mc01_8, h264_mc02_8, h264_mc03_8,
    h264_mc10_8, h264_mc11_8, h264_mc12_8, h264_mc13_8,
    h264_mc20_8, h264_mc21_8, h264_mc22_8, h264_mc23_8,
    h264_mc30_8, h264_mc31_8, h264_mc32_8, h264_mc33_8
  ], [
    h264_mc00_16, h264_mc01_16, h264_mc02_16, h264_mc03_16,
    h264_mc10_16, h264_mc11_16, h264_mc12_16, h264_mc13_16,
    h264_mc20_16, h264_mc21_16, h264_mc22_16, h264_mc23_16,
    h264_mc30_16, h264_mc31_16, h264_mc32_16, h264_mc33_16
  ]
];

impl super::RegisterSIMD for super::H264MC {
    fn register_simd(&mut self) {}
}
