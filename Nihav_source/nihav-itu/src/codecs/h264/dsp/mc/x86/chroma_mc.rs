use std::arch::asm;

#[cfg(target_arch = "x86")]
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
    unsafe {
        match (dx, dy) {
            (0, 0) => {
                asm!(
                    "lea       {stmp}, [{src} + {sstride} * 2]",
                    "lea       {dtmp}, [{dst} + {dstride} * 2]",
                    "2:",
                    "movq      xmm0, [{src}]",
                    "movq      xmm1, [{src} + {sstride}]",
                    "movq      xmm2, [{stmp}]",
                    "movq      xmm3, [{stmp} + {sstride}]",
                    "movq      [{dst}],              xmm0",
                    "lea       {src}, [{src} + {sstride} * 4]",
                    "movq      [{dst} + {dstride}],  xmm1",
                    "lea       {stmp}, [{stmp} + {sstride} * 4]",
                    "movq      [{dtmp}],             xmm2",
                    "lea       {dst}, [{dst} + {dstride} * 4]",
                    "movq      [{dtmp} + {dstride}], xmm3",
                    "lea       {dtmp}, [{dtmp} + {dstride} * 4]",
                    "sub       {h}, 4",
                    "jnz       2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = in(reg) sstride,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = in(reg) dstride,
                    h = inout(reg) h => _,
                    stmp = out(reg) _,
                    dtmp = out(reg) _,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                );
            },
            (0, _) => {
                asm!(
                    "pxor      xmm0, xmm0",
                    "movd      xmm3, {a0:e}",
                    "movd      xmm4, {a1:e}",
                    "mov       {a1:e}, 0x0004",
                    "movd      xmm5, {a1:e}",
                    "pshuflw   xmm3, xmm3, 0",
                    "pshuflw   xmm4, xmm4, 0",
                    "pshuflw   xmm5, xmm5, 0",
                    "movlhps   xmm3, xmm3",
                    "movlhps   xmm4, xmm4",
                    "movlhps   xmm5, xmm5",
                    "movq      xmm6, [{src}]",
                    "add       {src}, {sstride}",
                    "punpcklbw xmm6, xmm0",
                    "2:",
                    "movaps    xmm1, xmm6",
                    "movq      xmm2, [{src}]",
                    "punpcklbw xmm2, xmm0",
                    "movaps    xmm6, xmm2",
                    "pmullw    xmm1, xmm3",
                    "pmullw    xmm2, xmm4",
                    "add       {src}, {sstride}",
                    "paddw     xmm1, xmm2",
                    "paddw     xmm1, xmm5",
                    "psraw     xmm1, 3",
                    "packuswb  xmm1, xmm1",
                    "movq      [{dst}], xmm1",
                    "add       {dst}, {dstride}",
                    "dec       {h}",
                    "jnz       2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = in(reg) sstride,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = in(reg) dstride,
                    h = inout(reg) h => _,
                    a0 = in(reg) i32::from(8 - dy),
                    a1 = inout(reg) i32::from(dy) => _,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                    out("xmm4") _,
                    out("xmm5") _,
                    out("xmm6") _,
                );
            },
            (_, 0) => {
                asm!(
                    "pxor      xmm0, xmm0",
                    "movd      xmm3, {a0:e}",
                    "movd      xmm4, {a1:e}",
                    "mov       {a1:e}, 0x0004",
                    "movd      xmm5, {a1:e}",
                    "pshuflw   xmm3, xmm3, 0",
                    "pshuflw   xmm4, xmm4, 0",
                    "pshuflw   xmm5, xmm5, 0",
                    "movlhps   xmm3, xmm3",
                    "movlhps   xmm4, xmm4",
                    "movlhps   xmm5, xmm5",
                    "2:",
                    "movq      xmm1, [{src}]",
                    "movq      xmm2, [{src} + 1]",
                    "punpcklbw xmm1, xmm0",
                    "punpcklbw xmm2, xmm0",
                    "pmullw    xmm1, xmm3",
                    "pmullw    xmm2, xmm4",
                    "add       {src}, {sstride}",
                    "paddw     xmm1, xmm2",
                    "paddw     xmm1, xmm5",
                    "psraw     xmm1, 3",
                    "packuswb  xmm1, xmm1",
                    "movq      [{dst}], xmm1",
                    "add       {dst}, {dstride}",
                    "dec       {h}",
                    "jnz       2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = inout(reg) sstride => _,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = inout(reg) dstride => _,
                    h = inout(reg) h => _,
                    a0 = inout(reg) i32::from(8 - dx) => _,
                    a1 = inout(reg) i32::from(dx) => _,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                    out("xmm4") _,
                    out("xmm5") _,
                );
            },
            #[cfg(target_arch = "x86")]
            _ => chroma_interp(dst, dstride, src, sstride, dx, dy, 8, h),
            #[cfg(target_arch = "x86_64")]
            _ => {
                asm!(
                    "pxor      xmm0, xmm0",
                    "movd      xmm3, {a0:e}",
                    "movd      xmm4, {a1:e}",
                    "movd      xmm5, {b0:e}",
                    "movd      xmm6, {b1:e}",
                    "mov       {a1:e}, 0x0020",
                    "movd      xmm7, {a1:e}",
                    "pshuflw   xmm3, xmm3, 0",
                    "pshuflw   xmm4, xmm4, 0",
                    "pshuflw   xmm5, xmm5, 0",
                    "pshuflw   xmm6, xmm6, 0",
                    "pshuflw   xmm7, xmm7, 0",
                    "movlhps   xmm3, xmm3",
                    "movlhps   xmm4, xmm4",
                    "movlhps   xmm5, xmm5",
                    "movlhps   xmm6, xmm6",
                    "movlhps   xmm7, xmm7",

                    "movq      xmm8,  [{src}]",
                    "movq      xmm2,  [{src} + 1]",
                    "punpcklbw xmm8,  xmm0",
                    "punpcklbw xmm2,  xmm0",
                    "pmullw    xmm8,  xmm3",
                    "pmullw    xmm2,  xmm4",
                    "add       {src}, {sstride}",
                    "paddw     xmm8,  xmm2",

                    "2:",
                    "movq      xmm1,  [{src}]",
                    "movq      xmm2,  [{src} + 1]",
                    "punpcklbw xmm1,  xmm0",
                    "punpcklbw xmm2,  xmm0",
                    "pmullw    xmm1,  xmm3",
                    "pmullw    xmm2,  xmm4",
                    "add       {src}, {sstride}",
                    "paddw     xmm1,  xmm2",
                    "movaps    xmm2,  xmm8",
                    "movaps    xmm8,  xmm1",

                    "pmullw    xmm1, xmm6",
                    "pmullw    xmm2, xmm5",
                    "paddw     xmm1, xmm2",
                    "paddw     xmm1, xmm7",
                    "psraw     xmm1, 6",
                    "packuswb  xmm1, xmm1",
                    "movq      [{dst}], xmm1",
                    "add       {dst}, {dstride}",
                    "dec       {h}",
                    "jnz       2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = inout(reg) sstride => _,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = inout(reg) dstride => _,
                    h = inout(reg) h => _,
                    a0 = inout(reg) i32::from(8 - dx) => _,
                    a1 = inout(reg) i32::from(dx) => _,
                    b0 = inout(reg) i32::from(8 - dy) => _,
                    b1 = inout(reg) i32::from(dy) => _,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                    out("xmm4") _,
                    out("xmm5") _,
                    out("xmm6") _,
                    out("xmm7") _,
                    out("xmm8") _,
                );
            },
        };
    }
}

pub fn chroma_interp_4(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, dx: u16, dy: u16, h: usize) {
    unsafe {
        match (dx, dy) {
            (0, 0) => {
                asm!(
                    "2:",
                    "movd      xmm0, [{src}]",
                    "movd      xmm1, [{src} + {sstride}]",
                    "movd      [{dst}],              xmm0",
                    "lea       {src}, [{src} + {sstride} * 2]",
                    "movd      [{dst} + {dstride}],  xmm1",
                    "lea       {dst}, [{dst} + {dstride} * 2]",
                    "sub       {h}, 2",
                    "jnz       2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = in(reg) sstride,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = in(reg) dstride,
                    h = inout(reg) h => _,
                    out("xmm0") _,
                    out("xmm1") _,
                );
            },
            (0, _) => {
                asm!(
                    "pxor      xmm0, xmm0",
                    "movd      xmm3, {a0:e}",
                    "movd      xmm4, {a1:e}",
                    "mov       {a1:e}, 0x0004",
                    "movd      xmm5, {a1:e}",
                    "pshuflw   xmm3, xmm3, 0",
                    "pshuflw   xmm4, xmm4, 0",
                    "pshuflw   xmm5, xmm5, 0",
                    "movd      xmm6, [{src}]",
                    "add       {src}, {sstride}",
                    "punpcklbw xmm6, xmm0",
                    "2:",
                    "movaps    xmm1, xmm6",
                    "movd      xmm2, [{src}]",
                    "punpcklbw xmm2, xmm0",
                    "movaps    xmm6, xmm2",
                    "pmullw    xmm1, xmm3",
                    "pmullw    xmm2, xmm4",
                    "add       {src}, {sstride}",
                    "paddw     xmm1, xmm2",
                    "paddw     xmm1, xmm5",
                    "psraw     xmm1, 3",
                    "packuswb  xmm1, xmm1",
                    "movd      [{dst}], xmm1",
                    "add       {dst}, {dstride}",
                    "dec       {h}",
                    "jnz       2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = inout(reg) sstride => _,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = inout(reg) dstride => _,
                    h = inout(reg) h => _,
                    a0 = inout(reg) i32::from(8 - dy) => _,
                    a1 = inout(reg) i32::from(dy) => _,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                    out("xmm4") _,
                    out("xmm5") _,
                    out("xmm6") _,
                );
            },
            (_, 0) => {
                asm!(
                    "pxor      xmm0, xmm0",
                    "movd      xmm3, {a0:e}",
                    "movd      xmm4, {a1:e}",
                    "mov       {a1:e}, 0x0004",
                    "movd      xmm5, {a1:e}",
                    "pshuflw   xmm3, xmm3, 0",
                    "pshuflw   xmm4, xmm4, 0",
                    "pshuflw   xmm5, xmm5, 0",
                    "2:",
                    "movd      xmm1, [{src}]",
                    "movd      xmm2, [{src} + 1]",
                    "punpcklbw xmm1, xmm0",
                    "punpcklbw xmm2, xmm0",
                    "pmullw    xmm1, xmm3",
                    "pmullw    xmm2, xmm4",
                    "add       {src}, {sstride}",
                    "paddw     xmm1, xmm2",
                    "paddw     xmm1, xmm5",
                    "psraw     xmm1, 3",
                    "packuswb  xmm1, xmm1",
                    "movd      [{dst}], xmm1",
                    "add       {dst}, {dstride}",
                    "dec       {h}",
                    "jnz       2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = inout(reg) sstride => _,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = inout(reg) dstride => _,
                    h = inout(reg) h => _,
                    a0 = inout(reg) i32::from(8 - dx) => _,
                    a1 = inout(reg) i32::from(dx) => _,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                    out("xmm4") _,
                    out("xmm5") _,
                );
            },
            #[cfg(target_arch = "x86")]
            _ => chroma_interp(dst, dstride, src, sstride, dx, dy, 4, h),
            #[cfg(target_arch = "x86_64")]
            _ => {
                asm!(
                    "pxor      xmm0, xmm0",
                    "movd      xmm3, {a0:e}",
                    "movd      xmm4, {a1:e}",
                    "movd      xmm5, {b0:e}",
                    "movd      xmm6, {b1:e}",
                    "mov       {a1:e}, 0x0020",
                    "movd      xmm7, {a1:e}",
                    "pshuflw   xmm3, xmm3, 0",
                    "pshuflw   xmm4, xmm4, 0",
                    "pshuflw   xmm5, xmm5, 0",
                    "pshuflw   xmm6, xmm6, 0",
                    "pshuflw   xmm7, xmm7, 0",

                    "movd      xmm8,  [{src}]",
                    "movd      xmm2,  [{src} + 1]",
                    "punpcklbw xmm8,  xmm0",
                    "punpcklbw xmm2,  xmm0",
                    "pmullw    xmm8,  xmm3",
                    "pmullw    xmm2,  xmm4",
                    "add       {src}, {sstride}",
                    "paddw     xmm8,  xmm2",

                    "2:",
                    "movd      xmm1,  [{src}]",
                    "movd      xmm2,  [{src} + 1]",
                    "punpcklbw xmm1,  xmm0",
                    "punpcklbw xmm2,  xmm0",
                    "pmullw    xmm1,  xmm3",
                    "pmullw    xmm2,  xmm4",
                    "add       {src}, {sstride}",
                    "paddw     xmm1,  xmm2",
                    "movaps    xmm2,  xmm8",
                    "movaps    xmm8,  xmm1",

                    "pmullw    xmm1, xmm6",
                    "pmullw    xmm2, xmm5",
                    "paddw     xmm1, xmm2",
                    "paddw     xmm1, xmm7",
                    "psraw     xmm1, 6",
                    "packuswb  xmm1, xmm1",
                    "movd      [{dst}], xmm1",
                    "add       {dst}, {dstride}",
                    "dec       {h}",
                    "jnz       2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = inout(reg) sstride => _,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = inout(reg) dstride => _,
                    h = inout(reg) h => _,
                    a0 = inout(reg) i32::from(8 - dx) => _,
                    a1 = inout(reg) i32::from(dx) => _,
                    b0 = inout(reg) i32::from(8 - dy) => _,
                    b1 = inout(reg) i32::from(dy) => _,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                    out("xmm4") _,
                    out("xmm5") _,
                    out("xmm6") _,
                    out("xmm7") _,
                    out("xmm8") _,
                );
            },
        };
    }
}

#[inline]
fn chr_interp2(a: u8, b: u8, b0: u16, b1: u16) -> u8 {
    ((u16::from(a) * b0 + u16::from(b) * b1 + 4) >> 3) as u8
}
#[inline]
fn chr_interp4(a: u8, b: u8, c: u8, d: u8, a0: u16, a1: u16, b0: u16, b1: u16) -> u8 {
    ((u16::from(a) * a0 * b0 + u16::from(b) * a1 * b0 + u16::from(c) * a0 * b1 + u16::from(d) * a1 * b1 + 0x20) >> 6) as u8
}

pub fn chroma_interp_2(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, dx: u16, dy: u16, h: usize) {
    let a0 = 8 - dx;
    let a1 = dx;
    let b0 = 8 - dy;
    let b1 = dy;

    if a0 == 8 && b0 == 8 {
        unsafe {
            let mut src = src.as_ptr();
            let mut dst = dst.as_mut_ptr();
            std::ptr::copy_nonoverlapping(src, dst, 2);
            src = src.add(sstride);
            dst = dst.add(dstride);
            std::ptr::copy_nonoverlapping(src, dst, 2);
            if h == 4 {
                src = src.add(sstride);
                dst = dst.add(dstride);
                std::ptr::copy_nonoverlapping(src, dst, 2);
                src = src.add(sstride);
                dst = dst.add(dstride);
                std::ptr::copy_nonoverlapping(src, dst, 2);
            }
        }
    } else if a0 == 8 {
        unsafe {
            let mut src0 = src.as_ptr();
            let mut src1 = src0.add(sstride);
            let mut dst = dst.as_mut_ptr();
            *dst                  = chr_interp2(*src0,                  *src1,                  b0, b1);
            *dst.add(1)           = chr_interp2(*src0.add(1),           *src1.add(1),           b0, b1);
            *dst.add(dstride)     = chr_interp2(*src0.add(sstride),     *src1.add(sstride),     b0, b1);
            *dst.add(dstride + 1) = chr_interp2(*src0.add(sstride + 1), *src1.add(sstride + 1), b0, b1);
            if h == 4 {
                src0 = src0.add(sstride * 2);
                src1 = src1.add(sstride * 2);
                dst = dst.add(dstride * 2);
                *dst                  = chr_interp2(*src0,                  *src1,                  b0, b1);
                *dst.add(1)           = chr_interp2(*src0.add(1),           *src1.add(1),           b0, b1);
                *dst.add(dstride)     = chr_interp2(*src0.add(sstride),     *src1.add(sstride),     b0, b1);
                *dst.add(dstride + 1) = chr_interp2(*src0.add(sstride + 1), *src1.add(sstride + 1), b0, b1);
            }
        }
    } else if b0 == 8 {
        unsafe {
            let mut src = src.as_ptr();
            let mut dst = dst.as_mut_ptr();
            let (a, b, c) = (*src, *src.add(1), *src.add(2));
            *dst                  = chr_interp2(a, b, a0, a1);
            *dst.add(1)           = chr_interp2(b, c, a0, a1);
            let (a, b, c) = (*src.add(sstride), *src.add(sstride + 1), *src.add(sstride + 2));
            *dst.add(dstride)     = chr_interp2(a, b, a0, a1);
            *dst.add(dstride + 1) = chr_interp2(b, c, a0, a1);
            if h == 4 {
                src = src.add(sstride * 2);
                dst = dst.add(dstride * 2);
                let (a, b, c) = (*src, *src.add(1), *src.add(2));
                *dst                  = chr_interp2(a, b, a0, a1);
                *dst.add(1)           = chr_interp2(b, c, a0, a1);
                let (a, b, c) = (*src.add(sstride), *src.add(sstride + 1), *src.add(sstride + 2));
                *dst.add(dstride)     = chr_interp2(a, b, a0, a1);
                *dst.add(dstride + 1) = chr_interp2(b, c, a0, a1);
            }
        }
    } else {
        unsafe {
            let height = h;
            let mut src0 = src.as_ptr();
            let mut src1 = src0.add(sstride);
            let mut dst = dst.as_mut_ptr();

            let (a, b, c) = (*src0, *src0.add(1), *src0.add(2));
            let (d, e, f) = (*src1, *src1.add(1), *src1.add(2));
            let (g, h, i) = (*src1.add(sstride), *src1.add(sstride + 1), *src1.add(sstride + 2));
            *dst                  = chr_interp4(a, b, d, e, a0, a1, b0, b1);
            *dst.add(1)           = chr_interp4(b, c, e, f, a0, a1, b0, b1);
            *dst.add(dstride)     = chr_interp4(d, e, g, h, a0, a1, b0, b1);
            *dst.add(dstride + 1) = chr_interp4(e, f, h, i, a0, a1, b0, b1);
            if height == 4 {
                src0 = src0.add(sstride * 3);
                src1 = src1.add(sstride * 3);
                dst  = dst.add(dstride * 2);
                let (a, b, c) = (*src0, *src0.add(1), *src0.add(2));
                let (d, e, f) = (*src1, *src1.add(1), *src1.add(2));
                *dst                  = chr_interp4(g, h, a, b, a0, a1, b0, b1);
                *dst.add(1)           = chr_interp4(h, i, b, c, a0, a1, b0, b1);
                *dst.add(dstride)     = chr_interp4(a, b, d, e, a0, a1, b0, b1);
                *dst.add(dstride + 1) = chr_interp4(b, c, e, f, a0, a1, b0, b1);
            }
        }
    }
}

