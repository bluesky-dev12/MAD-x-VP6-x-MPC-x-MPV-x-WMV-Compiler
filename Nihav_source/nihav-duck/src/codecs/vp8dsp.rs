use nihav_core::frame::NAVideoBufferRef;
use nihav_codec_support::codecs::blockdsp::edge_emu;

fn clip_u8(val: i16) -> u8 {
    val.max(0).min(255) as u8
}

fn delta(p1: i16, p0: i16, q0: i16, q1: i16) -> i16 {
    ((p1 - q1).max(-128).min(127) + 3 * (q0 - p0)).max(-128).min(127)
}

pub type LoopFilterFunc = fn(buf: &mut [u8], off: usize, step: usize, stride: usize, len: usize, thr: i16, thr_inner: i16, thr_hev: i16);

pub fn simple_loop_filter(buf: &mut [u8], mut off: usize, step: usize, stride: usize, len: usize, thr: i16, _thr_inner: i16, _thr_hev: i16) {
    for _ in 0..len {
        let p1 = i16::from(buf[off - step * 2]);
        let p0 = i16::from(buf[off - step * 1]);
        let q0 = i16::from(buf[off + step * 0]);
        let q1 = i16::from(buf[off + step * 1]);
        let diff = (p0 - q0).abs() * 2 + ((p1 - q1).abs() >> 1);
        if diff <= thr {
            let diff = delta(p1, p0, q0, q1);
            let diffq0 = (diff + 4).min(127) >> 3;
            let diffp0 = (diff + 3).min(127) >> 3;
            buf[off - step * 1] = clip_u8(p0 + diffp0);
            buf[off + step * 0] = clip_u8(q0 - diffq0);
        }
        off += stride;
    }
}

fn normal_loop_filter(buf: &mut [u8], mut off: usize, step: usize, stride: usize, len: usize, thr: i16, thr_inner: i16, thr_hev: i16, edge: bool) {
    for _i in 0..len {
        let p1 = i16::from(buf[off - step * 2]);
        let p0 = i16::from(buf[off - step * 1]);
        let q0 = i16::from(buf[off + step * 0]);
        let q1 = i16::from(buf[off + step * 1]);
        let diff = (p0 - q0).abs() * 2 + ((p1 - q1).abs() >> 1);
        if diff <= thr {
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
                    let diffq0 = (diff + 4).min(127) >> 3;
                    let diffp0 = (diff + 3).min(127) >> 3;
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
                    let diff = (3 * (q0 - p0)).max(-128).min(127);
                    let diffq0 = (diff + 4).min(127) >> 3;
                    let diffp0 = (diff + 3).min(127) >> 3;
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

pub fn iwht4x4(coeffs: &mut [i16; 16]) {
    for i in 0..4 {
        let s0 = coeffs[i];
        let s1 = coeffs[i + 4];
        let s2 = coeffs[i + 8];
        let s3 = coeffs[i + 12];
        let a1 = s0 + s3;
        let b1 = s1 + s2;
        let c1 = s1 - s2;
        let d1 = s0 - s3;
        coeffs[i]       = a1 + b1;
        coeffs[i + 4]   = c1 + d1;
        coeffs[i + 8]   = a1 - b1;
        coeffs[i + 12]  = d1 - c1;
    }
    for row in coeffs.chunks_mut(4) {
        let a1 = row[0] + row[3];
        let b1 = row[1] + row[2];
        let c1 = row[1] - row[2];
        let d1 = row[0] - row[3];
        row[0] = (a1 + b1 + 3) >> 3;
        row[1] = (c1 + d1 + 3) >> 3;
        row[2] = (a1 - b1 + 3) >> 3;
        row[3] = (d1 - c1 + 3) >> 3;
    }
}

pub fn iwht4x4_dc(coeffs: &mut [i16; 16]) {
    let dc = (coeffs[0] + 3) >> 3;
    *coeffs = [dc; 16];
}

const COS_PI8_SQRT2_MINUS1: i32 = 20091;
const SIN_PI8_SQRT2: i32 = 35468;

macro_rules! idct4 {
    ($s0: expr, $s1: expr, $s2: expr, $s3: expr, $shift: expr) => {{
        let a1 = i32::from($s0) + i32::from($s2);
        let b1 = i32::from($s0) - i32::from($s2);
        let temp1 = (i32::from($s1) * SIN_PI8_SQRT2) >> 16;
        let temp2 = i32::from($s3) + ((i32::from($s3) * COS_PI8_SQRT2_MINUS1) >> 16);
        let c1 = temp1 - temp2;
        let temp1 = i32::from($s1) + ((i32::from($s1) * COS_PI8_SQRT2_MINUS1) >> 16);
        let temp2 = (i32::from($s3) * SIN_PI8_SQRT2) >> 16;
        let d1 = temp1 + temp2;

        let bias = (1 << $shift) >> 1;
        $s0 = ((a1 + d1 + bias) >> $shift) as i16;
        $s3 = ((a1 - d1 + bias) >> $shift) as i16;
        $s1 = ((b1 + c1 + bias) >> $shift) as i16;
        $s2 = ((b1 - c1 + bias) >> $shift) as i16;
    }}
}

pub fn idct4x4(coeffs: &mut [i16; 16]) {
    for i in 0..4 {
        idct4!(coeffs[i], coeffs[i + 4], coeffs[i + 8], coeffs[i + 12], 0);
    }
    for row in coeffs.chunks_mut(4) {
        idct4!(row[0], row[1], row[2], row[3], 3);
    }
}

pub fn idct4x4_dc(coeffs: &mut [i16; 16]) {
    let dc = (coeffs[0] + 4) >> 3;
    *coeffs = [dc; 16];
}
macro_rules! interpolate {
    ($src: expr, $off: expr, $step: expr, $mode: expr) => {{
        let s0 = i32::from($src[$off + 0 * $step]);
        let s1 = i32::from($src[$off + 1 * $step]);
        let a = (8 - $mode) as i32;
        let b = $mode as i32;
        ((a * s0 + b * s1 + 4) >> 3).max(0).min(255) as u8
    }}
}

const TMP_STRIDE: usize = 16;

fn mc_block_common(dst: &mut [u8], mut doff: usize, dstride: usize, src: &[u8], sstride: usize, size: usize, mx: usize, my: usize) {
    if (mx == 0) && (my == 0) {
        let dst = &mut dst[doff..];
        for (out, src) in dst.chunks_mut(dstride).take(size).zip(src.chunks(sstride)) {
            out[..size].copy_from_slice(&src[..size]);
        }
    } else if my == 0 {
        for src in src.chunks(sstride).take(size) {
            for x in 0..size {
                dst[doff + x] = interpolate!(src, x, 1, mx);
            }
            doff += dstride;
        }
    } else if mx == 0 {
        for y in 0..size {
            for x in 0..size {
                dst[doff + x] = interpolate!(src, x + y * sstride, sstride, my);
            }
            doff += dstride;
        }
    } else {
        let mut tmp = [0u8; TMP_STRIDE * (16 + 1)];
        for (y, dst) in tmp.chunks_mut(TMP_STRIDE).take(size + 1).enumerate() {
            for x in 0..size {
                dst[x] = interpolate!(src, x + y * sstride, 1, mx);
            }
        }
        for y in 0..size {
            for x in 0..size {
                dst[doff + x] = interpolate!(tmp, x + y * TMP_STRIDE, TMP_STRIDE, my);
            }
            doff += dstride;
        }
    }
}
fn mc_block(dst: &mut [u8], doff: usize, dstride: usize, xpos: usize, ypos: usize,
            mvx: i16, mvy: i16, reffrm: NAVideoBufferRef<u8>, plane: usize,
            mc_buf: &mut [u8], size: usize) {
    if (mvx == 0) && (mvy == 0) {
        let dst = &mut dst[doff..];
        let sstride = reffrm.get_stride(plane);
        let srcoff = reffrm.get_offset(plane) + xpos + ypos * sstride;
        let src = &reffrm.get_data();
        let src = &src[srcoff..];
        for (out, src) in dst.chunks_mut(dstride).take(size).zip(src.chunks(sstride)) {
            out[..size].copy_from_slice(&src[..size]);
        }
        return;
    }
    let (w, h) = reffrm.get_dimensions(plane);
    let wa = if plane == 0 { (w + 15) & !15 } else { (w + 7) & !7 } as isize;
    let ha = if plane == 0 { (h + 15) & !15 } else { (h + 7) & !7 } as isize;
    let bsize = (size as isize) + 1;
    let ref_x = (xpos as isize) + ((mvx >> 3) as isize);
    let ref_y = (ypos as isize) + ((mvy >> 3) as isize);

    let (src, sstride) = if (ref_x < 0) || (ref_x + bsize > wa) || (ref_y < 0) || (ref_y + bsize > ha) {
            edge_emu(&reffrm, ref_x, ref_y, bsize as usize, bsize as usize, mc_buf, 32, plane, 4);
            (mc_buf as &[u8], 32)
        } else {
            let off     = reffrm.get_offset(plane);
            let stride  = reffrm.get_stride(plane);
            let data    = reffrm.get_data();
            (&data[off + (ref_x as usize) + (ref_y as usize) * stride..], stride)
        };
    let mx = (mvx & 7) as usize;
    let my = (mvy & 7) as usize;
    mc_block_common(dst, doff, dstride, src, sstride, size, mx, my);
}
pub fn mc_block16x16_bilin(dst: &mut [u8], doff: usize, dstride: usize, xpos: usize, ypos: usize,
                           mvx: i16, mvy: i16, src: NAVideoBufferRef<u8>, plane: usize, mc_buf: &mut [u8]) {
    mc_block(dst, doff, dstride, xpos, ypos, mvx, mvy, src, plane, mc_buf, 16);
}
pub fn mc_block8x8_bilin(dst: &mut [u8], doff: usize, dstride: usize, xpos: usize, ypos: usize,
                         mvx: i16, mvy: i16, src: NAVideoBufferRef<u8>, plane: usize, mc_buf: &mut [u8]) {
    mc_block(dst, doff, dstride, xpos, ypos, mvx, mvy, src, plane, mc_buf, 8);
}
pub fn mc_block4x4_bilin(dst: &mut [u8], doff: usize, dstride: usize, xpos: usize, ypos: usize,
                         mvx: i16, mvy: i16, src: NAVideoBufferRef<u8>, plane: usize, mc_buf: &mut [u8]) {
    mc_block(dst, doff, dstride, xpos, ypos, mvx, mvy, src, plane, mc_buf, 4);
}
