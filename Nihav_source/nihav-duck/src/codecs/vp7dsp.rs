use nihav_core::frame::*;

fn clip_u8(val: i16) -> u8 {
    val.max(0).min(255) as u8
}

const DCT_COEFFS: [i32; 16] = [
    23170,  23170,  23170,  23170,
    30274,  12540, -12540, -30274,
    23170, -23170, -23170,  23170,
    12540, -30274,  30274, -12540
];

pub fn idct4x4(coeffs: &mut [i16; 16]) {
    let mut tmp = [0i16; 16];
    for (src, dst) in coeffs.chunks(4).zip(tmp.chunks_mut(4)) {
        let s0 = i32::from(src[0]);
        let s1 = i32::from(src[1]);
        let s2 = i32::from(src[2]);
        let s3 = i32::from(src[3]);

        let t0 = (s0 + s2).wrapping_mul(23170);
        let t1 = (s0 - s2).wrapping_mul(23170);
        let t2 = s1.wrapping_mul(30274) + s3.wrapping_mul(12540);
        let t3 = s1.wrapping_mul(12540) - s3.wrapping_mul(30274);

        dst[0] = ((t0 + t2) >> 14) as i16;
        dst[1] = ((t1 + t3) >> 14) as i16;
        dst[2] = ((t1 - t3) >> 14) as i16;
        dst[3] = ((t0 - t2) >> 14) as i16;
    }
    for i in 0..4 {
        let s0 = i32::from(tmp[i + 4 * 0]);
        let s1 = i32::from(tmp[i + 4 * 1]);
        let s2 = i32::from(tmp[i + 4 * 2]);
        let s3 = i32::from(tmp[i + 4 * 3]);

        let t0 = (s0 + s2).wrapping_mul(23170) + 0x20000;
        let t1 = (s0 - s2).wrapping_mul(23170) + 0x20000;
        let t2 = s1.wrapping_mul(30274) + s3.wrapping_mul(12540);
        let t3 = s1.wrapping_mul(12540) - s3.wrapping_mul(30274);

        coeffs[i + 0 * 4] = ((t0 + t2) >> 18) as i16;
        coeffs[i + 1 * 4] = ((t1 + t3) >> 18) as i16;
        coeffs[i + 2 * 4] = ((t1 - t3) >> 18) as i16;
        coeffs[i + 3 * 4] = ((t0 - t2) >> 18) as i16;
    }
}

pub fn idct4x4_dc(coeffs: &mut [i16; 16]) {
    let dc = ((((i32::from(coeffs[0]) * DCT_COEFFS[0]) >> 14) * DCT_COEFFS[0] + 0x20000) >> 18) as i16;
    for el in coeffs.iter_mut() {
        *el = dc;
    }
}

fn delta(p1: i16, p0: i16, q0: i16, q1: i16) -> i16 {
    (p1 - q1) + 3 * (q0 - p0)
}

pub type LoopFilterFunc = fn(buf: &mut [u8], off: usize, step: usize, stride: usize, len: usize, thr: i16, thr_inner: i16, thr_hev: i16);

pub fn simple_loop_filter(buf: &mut [u8], mut off: usize, step: usize, stride: usize, len: usize, thr: i16, _thr_inner: i16, _thr_hev: i16) {
    for _ in 0..len {
        let p1 = i16::from(buf[off - step * 2]);
        let p0 = i16::from(buf[off - step * 1]);
        let q0 = i16::from(buf[off + step * 0]);
        let q1 = i16::from(buf[off + step * 1]);
        let dpq = p0 - q0;
        if dpq.abs() < thr {
            let diff = delta(p1, p0, q0, q1);
            let diffq0 = (diff.min(127) + 4) >> 3;
            let diffp0 = diffq0 - if (diff & 7) == 4 { 1 } else { 0 };
            buf[off - step * 1] = clip_u8(p0 + diffp0);
            buf[off + step * 0] = clip_u8(q0 - diffq0);
        }
        off += stride;
    }
}

fn normal_loop_filter(buf: &mut [u8], mut off: usize, step: usize, stride: usize, len: usize, thr: i16, thr_inner: i16, thr_hev: i16, edge: bool) {
    for _ in 0..len {
        let p0 = i16::from(buf[off - step * 1]);
        let q0 = i16::from(buf[off + step * 0]);
        let dpq = p0 - q0;
        if dpq.abs() <= thr {
            let p3 = i16::from(buf[off - step * 4]);
            let p2 = i16::from(buf[off - step * 3]);
            let p1 = i16::from(buf[off - step * 2]);
            let q1 = i16::from(buf[off + step * 1]);
            let q2 = i16::from(buf[off + step * 2]);
            let q3 = i16::from(buf[off + step * 3]);
            let dp2 = p3 - p2;
            let dp1 = p2 - p1;
            let dp0 = p1 - p0;
            let dq0 = q1 - q0;
            let dq1 = q2 - q1;
            let dq2 = q3 - q2;
            if (dp0.abs() <= thr_inner) && (dp1.abs() <= thr_inner) &&
               (dp2.abs() <= thr_inner) && (dq0.abs() <= thr_inner) &&
               (dq1.abs() <= thr_inner) && (dq2.abs() <= thr_inner) {
                let high_edge_variation = (dp0.abs() > thr_hev) || (dq0.abs() > thr_hev);
                if high_edge_variation {
                    let diff = delta(p1, p0, q0, q1);
                    let diffq0 = (diff.min(127) + 4) >> 3;
                    let diffp0 = diffq0 - if (diff & 7) == 4 { 1 } else { 0 };
                    buf[off - step * 1] = clip_u8(p0 + diffp0);
                    buf[off + step * 0] = clip_u8(q0 - diffq0);
                } else if edge {
                    let d = delta(p1, p0, q0, q1);
                    let diff0 = (d * 27 + 63) >> 7;
                    buf[off - step * 1] = clip_u8(p0 + diff0);
                    buf[off + step * 0] = clip_u8(q0 - diff0);
                    let diff1 = (d * 18 + 63) >> 7;
                    buf[off - step * 2] = clip_u8(p1 + diff1);
                    buf[off + step * 1] = clip_u8(q1 - diff1);
                    let diff2 = (d * 9 + 63) >> 7;
                    buf[off - step * 3] = clip_u8(p2 + diff2);
                    buf[off + step * 2] = clip_u8(q2 - diff2);
                } else {
                    let diff = 3 * (q0 - p0);
                    let diffq0 = (diff.min(127) + 4) >> 3;
                    let diffp0 = diffq0 - if (diff & 7) == 4 { 1 } else { 0 };
                    buf[off - step * 1] = clip_u8(p0 + diffp0);
                    buf[off + step * 0] = clip_u8(q0 - diffq0);
                    let diff2 = (diffq0 + 1) >> 1;
                    buf[off - step * 2] = clip_u8(p1 + diff2);
                    buf[off + step * 1] = clip_u8(q1 - diff2);
                }
            }
        }
        off += stride;
    }
}

pub fn normal_loop_filter_inner(buf: &mut [u8], off: usize, step: usize, stride: usize, len: usize, thr: i16, thr_inner: i16, thr_hev: i16) {
    normal_loop_filter(buf, off, step, stride, len, thr, thr_inner, thr_hev, false);
}

pub fn normal_loop_filter_edge(buf: &mut [u8], off: usize, step: usize, stride: usize, len: usize, thr: i16, thr_inner: i16, thr_hev: i16) {
    normal_loop_filter(buf, off, step, stride, len, thr, thr_inner, thr_hev, true);
}

pub fn fade_frame(srcfrm: NAVideoBufferRef<u8>, dstfrm: &mut NASimpleVideoFrame<u8>, alpha: u16, beta: u16) {
    let mut fade_lut = [0u8; 256];
    for (i, el) in fade_lut.iter_mut().enumerate() {
        let y = i as u16;
        *el = (y + ((y * beta) >> 8) + alpha).max(0).min(255) as u8;
    }

    let (w, h)  = srcfrm.get_dimensions(0);
    let (wa, ha) = ((w + 15) & !15, (h + 15) & !15);
    let soff    = srcfrm.get_offset(0);
    let sstride = srcfrm.get_stride(0);
    let sdata   = srcfrm.get_data();
    let src = &sdata[soff..];
    let dstride = dstfrm.stride[0];
    let dst = &mut dstfrm.data[dstfrm.offset[0]..];
    for (src, dst) in src.chunks(sstride).zip(dst.chunks_mut(dstride)).take(ha) {
        for (s, d) in src.iter().zip(dst.iter_mut()).take(wa) {
            *d = fade_lut[*s as usize];
        }
    }

    for plane in 1..3 {
        let (w, h)  = srcfrm.get_dimensions(plane);
        let (wa, ha) = ((w + 7) & !7, (h + 7) & !7);
        let soff    = srcfrm.get_offset(plane);
        let sstride = srcfrm.get_stride(plane);
        let sdata   = srcfrm.get_data();
        let src = &sdata[soff..];
        let dstride = dstfrm.stride[plane];
        let dst = &mut dstfrm.data[dstfrm.offset[plane]..];
        for (src, dst) in src.chunks(sstride).zip(dst.chunks_mut(dstride)).take(ha) {
            dst[..wa].copy_from_slice(&src[..wa]);
        }
    }
}
