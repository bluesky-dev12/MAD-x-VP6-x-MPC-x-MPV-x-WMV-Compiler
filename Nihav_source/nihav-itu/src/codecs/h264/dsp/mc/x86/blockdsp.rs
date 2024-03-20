use std::arch::asm;

macro_rules! avg_template {
    ($name: ident, $mov: expr) => {
        pub fn $name(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bh: usize) {
            unsafe {
                asm!(
                    "2:",
                    concat!($mov, "   xmm1, [{src}]"),
                    concat!($mov, "   xmm3, [{src} + {sstride}]"),
                    concat!($mov, "   xmm0, [{dst}]"),
                    concat!($mov, "   xmm2, [{dst} + {dstride}]"),
                    "lea    {src}, [{src} + {sstride} * 2]",
                    "pavgb  xmm0, xmm1",
                    "pavgb  xmm2, xmm3",
                    concat!($mov, "   [{dst}], xmm0"),
                    concat!($mov, "   [{dst} + {dstride}], xmm2"),
                    "lea    {dst}, [{dst} + {dstride} * 2]",
                    "sub    {h}, 2",
                    "jnz    2b",
                    src = inout(reg) src.as_ptr() => _,
                    sstride = in(reg) sstride,
                    dst = inout(reg) dst.as_mut_ptr() => _,
                    dstride = in(reg) dstride,
                    h = inout(reg) bh => _,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                );
            }
        }
    }
}

avg_template!(avg_4, "movd");
avg_template!(avg_8, "movq");

pub fn avg_16(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bh: usize) {
    unsafe {
        asm!(
            "lea    {stmp}, [{src} + {sstride} * 2]",
            "lea    {dtmp}, [{dst} + {dstride} * 2]",
            "2:",
            "movaps xmm0, [{src}]",
            "movaps xmm1, [{src} + {sstride}]",
            "movaps xmm2, [{stmp}]",
            "movaps xmm3, [{stmp} + {sstride}]",
            "pavgb  xmm0, [{dst}]",
            "pavgb  xmm1, [{dst} + {dstride}]",
            "pavgb  xmm2, [{dtmp}]",
            "pavgb  xmm3, [{dtmp} + {dstride}]",
            "lea    {src},  [{src} + {sstride} * 4]",
            "movaps [{dst}], xmm0",
            "lea    {stmp}, [{stmp} + {sstride} * 4]",
            "movaps [{dst} + {dstride}], xmm1",
            "lea    {dst},  [{dst} + {dstride} * 4]",
            "movaps [{dtmp}], xmm2",
            "movaps [{dtmp} + {dstride}], xmm3",
            "lea    {dtmp}, [{dtmp} + {dstride} * 4]",
            "sub    {h}, 4",
            "jnz    2b",
            src = inout(reg) src.as_ptr() => _,
            sstride = in(reg) sstride,
            dst = inout(reg) dst.as_mut_ptr() => _,
            dstride = in(reg) dstride,
            h = inout(reg) bh => _,
            stmp = out(reg) _,
            dtmp = out(reg) _,
            out("xmm0") _,
            out("xmm1") _,
            out("xmm2") _,
            out("xmm3") _,
        );
    }
}

macro_rules! put_block_weighted {
    ($func:ident, $width:expr, $load:expr, $store:expr) => {
        pub fn $func(dst: &mut [u8], stride: usize, src: &[u8], h: usize, wparams: [i8; 3]) {
            if wparams == [1, 0, 0] {
                for (dst, src) in dst.chunks_mut(stride).zip(src.chunks(16)).take(h) {
                    dst[..$width].copy_from_slice(&src[..$width]);
                }
            } else {
                let weight = i32::from(wparams[0]);
                let offset = i32::from(wparams[1]);
                let wshift = i32::from(wparams[2]);
                let bias = (1 << wshift) >> 1;

                unsafe {
                    asm!(
                        "xorps      xmm0, xmm0",
                        "movd       xmm1, {weight:e}",
                        "movd       xmm2, {offset:e}",
                        "movd       xmm3, {wshift:e}",
                        "movd       xmm4, {bias:e}",
                        "pshuflw    xmm1, xmm1, 0",
                        "pshuflw    xmm2, xmm2, 0",
                        "pshuflw    xmm4, xmm4, 0",
                        "movlhps    xmm1, xmm1",
                        "movlhps    xmm2, xmm2",
                        "movlhps    xmm4, xmm4",
                        "2:",
                        concat!($load, " xmm5, [{src}]"),
                        "add        {src}, 16",
                        "movaps     xmm7, xmm5",
                        "punpcklbw  xmm5, xmm0",
                        "punpckhbw  xmm7, xmm0",
                        "pmullw     xmm5, xmm1",
                        "pmullw     xmm7, xmm1",
                        "paddw      xmm5, xmm4",
                        "paddw      xmm7, xmm4",
                        "psraw      xmm5, xmm3",
                        "psraw      xmm7, xmm3",
                        "paddw      xmm5, xmm2",
                        "paddw      xmm7, xmm2",
                        "packuswb   xmm5, xmm7",
                        concat!($store, " [{dst}], xmm5"),
                        "add        {dst}, {stride}",
                        "dec        {h}",
                        "jnz        2b",
                        h = inout(reg) h => _,
                        src = inout(reg) src.as_ptr() => _,
                        dst = inout(reg) dst.as_mut_ptr() => _,
                        stride = in(reg) stride,
                        weight = in(reg) weight,
                        offset = in(reg) offset,
                        wshift = in(reg) wshift,
                        bias = in(reg) bias,
                        out("xmm0") _,
                        out("xmm1") _,
                        out("xmm2") _,
                        out("xmm3") _,
                        out("xmm4") _,
                        out("xmm5") _,
                        out("xmm7") _,
                    );
                }
            }
        }
    }
}

put_block_weighted!(put_block_weighted_16, 16, "movups", "movaps");
put_block_weighted!(put_block_weighted_8, 8, "movq", "movq");
put_block_weighted!(put_block_weighted_4, 4, "movd", "movd");

macro_rules! put_block_weighted2 {
    ($func:ident, $mov:expr) => {
        pub fn $func(dst: &mut [u8], stride: usize, src0: &[u8], src1: &[u8], h: usize, wparams: [i8; 5]) {
            if wparams == [1, 0, 1, 0, 0] {
                unsafe {
                    asm!(
                        "2:",
                        concat!($mov, "  xmm0, [{src0}]"),
                        concat!($mov, "  xmm1, [{src1}]"),
                        "add   {src0}, 16",
                        "pavgb xmm0, xmm1",
                        "add   {src1}, 16",
                        concat!($mov, "  [{dst}], xmm0"),
                        "add   {dst}, {stride}",
                        "dec   {h}",
                        "jnz   2b",
                        src0   = inout(reg) src0.as_ptr() => _,
                        src1   = inout(reg) src1.as_ptr() => _,
                        dst    = inout(reg) dst.as_mut_ptr() => _,
                        stride = in(reg) stride,
                        h      = inout(reg) h => _,
                        out("xmm0") _,
                        out("xmm1") _,
                    );
                }
                return;
            }
            let weight0 = i32::from(wparams[0]);
            let offset0 = i32::from(wparams[1]);
            let weight1 = i32::from(wparams[2]);
            let offset1 = i32::from(wparams[3]);
            let wshift = i32::from(wparams[4]) + 1;
            let offset = (offset0 + offset1 + 1) >> 1;
            let bias = (1 << wshift) >> 1;

            unsafe {
                asm!(
                    "xorps      xmm0, xmm0",
                    "movd       xmm1, {weight0:e}",
                    "movd       xmm2, {weight1:e}",
                    "movd       xmm3, {offset:e}",
                    "movd       xmm4, {wshift:e}",
                    "movd       xmm5, {bias:e}",
                    "pshuflw    xmm1, xmm1, 0",
                    "pshuflw    xmm2, xmm2, 0",
                    "pshuflw    xmm3, xmm3, 0",
                    "pshuflw    xmm5, xmm5, 0",
                    "movlhps    xmm1, xmm1",
                    "movlhps    xmm2, xmm2",
                    "movlhps    xmm3, xmm3",
                    "movlhps    xmm5, xmm5",
                    "2:",
                    concat!($mov, " xmm6, [{src0}]"),
                    "add        {src0}, 16",
                    concat!($mov, " xmm7, [{src1}]"),
                    "add        {src1}, 16",
                    "punpcklbw  xmm6, xmm0",
                    "punpcklbw  xmm7, xmm0",
                    "pmullw     xmm6, xmm1",
                    "pmullw     xmm7, xmm2",
                    "paddw      xmm6, xmm5",
                    "paddw      xmm6, xmm7",
                    "psraw      xmm6, xmm4",
                    "paddw      xmm6, xmm3",
                    "movhlps    xmm7, xmm6",
                    "packuswb   xmm6, xmm7",
                    concat!($mov, " [{dst}], xmm6"),
                    "add        {dst}, {stride}",
                    "dec        {h}",
                    "jnz        2b",
                    h       = inout(reg) h => _,
                    src0    = inout(reg) src0.as_ptr() => _,
                    src1    = inout(reg) src1.as_ptr() => _,
                    dst     = inout(reg) dst.as_mut_ptr() => _,
                    stride  = in(reg) stride,
                    weight0 = in(reg) weight0,
                    weight1 = in(reg) weight1,
                    offset  = in(reg) offset,
                    wshift  = in(reg) wshift,
                    bias    = in(reg) bias,
                    out("xmm0") _,
                    out("xmm1") _,
                    out("xmm2") _,
                    out("xmm3") _,
                    out("xmm4") _,
                    out("xmm5") _,
                    out("xmm6") _,
                    out("xmm7") _,
                );
            }
        }
    }
}

put_block_weighted2!(put_block_weighted2_8, "movq");
put_block_weighted2!(put_block_weighted2_4, "movd");

pub fn put_block_weighted2_16(dst: &mut [u8], stride: usize, src0: &[u8], src1: &[u8], h: usize, wparams: [i8; 5]) {
    if wparams == [1, 0, 1, 0, 0] {
        unsafe {
            asm!(
                "2:",
                "movups xmm0, [{src0}]",
                "movups xmm1, [{src1}]",
                "add    {src0}, 16",
                "pavgb  xmm0, xmm1",
                "add    {src1}, 16",
                "movaps [{dst}], xmm0",
                "add    {dst}, {stride}",
                "dec    {h}",
                "jnz    2b",
                src0   = inout(reg) src0.as_ptr() => _,
                src1   = inout(reg) src1.as_ptr() => _,
                dst    = inout(reg) dst.as_mut_ptr() => _,
                stride = in(reg) stride,
                h      = inout(reg) h => _,
                out("xmm0") _,
                out("xmm1") _,
            );
        }
        return;
    }
    let weight0 = i32::from(wparams[0]);
    let offset0 = i32::from(wparams[1]);
    let weight1 = i32::from(wparams[2]);
    let offset1 = i32::from(wparams[3]);
    let wshift = i32::from(wparams[4]) + 1;
    let offset = (offset0 + offset1 + 1) >> 1;
    let bias = (1 << wshift) >> 1;

    unsafe {
        asm!(
            "xorps      xmm0, xmm0",
            "movd       xmm1, {weight0:e}",
            "movd       xmm2, {weight1:e}",
            "movd       xmm3, {offset:e}",
            "movd       xmm4, {wshift:e}",
            "movd       xmm5, {bias:e}",
            "pshuflw    xmm1, xmm1, 0",
            "pshuflw    xmm2, xmm2, 0",
            "pshuflw    xmm3, xmm3, 0",
            "pshuflw    xmm5, xmm5, 0",
            "movlhps    xmm1, xmm1",
            "movlhps    xmm2, xmm2",
            "movlhps    xmm3, xmm3",
            "movlhps    xmm5, xmm5",
            "2:",
            "movq       xmm6, [{src0}]",
            "movq       xmm7, [{src1}]",
            "punpcklbw  xmm6, xmm0",
            "punpcklbw  xmm7, xmm0",
            "pmullw     xmm6, xmm1",
            "pmullw     xmm7, xmm2",
            "paddw      xmm6, xmm5",
            "paddw      xmm6, xmm7",
            "psraw      xmm6, xmm4",
            "paddw      xmm6, xmm3",
            "movhlps    xmm7, xmm6",
            "packuswb   xmm6, xmm7",
            "movq       [{dst}], xmm6",
            "movq       xmm6, [{src0} + 8]",
            "add        {src0}, 16",
            "movq       xmm7, [{src1} + 8]",
            "add        {src1}, 16",
            "punpcklbw  xmm6, xmm0",
            "punpcklbw  xmm7, xmm0",
            "pmullw     xmm6, xmm1",
            "pmullw     xmm7, xmm2",
            "paddw      xmm6, xmm5",
            "paddw      xmm6, xmm7",
            "psraw      xmm6, xmm4",
            "paddw      xmm6, xmm3",
            "movhlps    xmm7, xmm6",
            "packuswb   xmm6, xmm7",
            "movq       [{dst} + 8], xmm6",
            "add        {dst}, {stride}",
            "dec        {h}",
            "jnz        2b",
            h       = inout(reg) h => _,
            src0    = inout(reg) src0.as_ptr() => _,
            src1    = inout(reg) src1.as_ptr() => _,
            dst     = inout(reg) dst.as_mut_ptr() => _,
            stride  = in(reg) stride,
            weight0 = in(reg) weight0,
            weight1 = in(reg) weight1,
            offset  = in(reg) offset,
            wshift  = in(reg) wshift,
            bias    = in(reg) bias,
            out("xmm0") _,
            out("xmm1") _,
            out("xmm2") _,
            out("xmm3") _,
            out("xmm4") _,
            out("xmm5") _,
            out("xmm6") _,
            out("xmm7") _,
        );
    }
}
