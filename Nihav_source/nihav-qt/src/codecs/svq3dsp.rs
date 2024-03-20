use nihav_codec_support::codecs::blockdsp::*;

#[allow(dead_code)]
#[derive(Debug,Clone,Copy)]
pub enum PredType4x4 {
    Ver,
    Hor,
    DC,
    DiagDownLeft,
    DiagDownRight,
    VerRight,
    HorDown,
    VerLeft,
    HorUp,
    LeftDC,
    TopDC,
    DC128,
}

#[allow(dead_code)]
#[derive(Debug,Clone,Copy)]
pub enum PredType8x8 {
    DC,
    Hor,
    Ver,
    Plane,
    LeftDC,
    TopDC,
    DC128
}

pub const INTRA_PRED16: [PredType8x8; 4] = [
    PredType8x8::DC, PredType8x8::Hor, PredType8x8::Ver, PredType8x8::Plane
];
pub const INTRA_PRED4: [PredType4x4; 9] = [
    PredType4x4::Ver, PredType4x4::Hor, PredType4x4::DC,
    PredType4x4::DiagDownLeft, PredType4x4::DiagDownRight,
    PredType4x4::VerRight, PredType4x4::HorDown,
    PredType4x4::VerLeft, PredType4x4::HorUp
];


const SVQ3_QUANTS: [i32; 32] = [
     3881,  4351,  4890,  5481,   6154,   6914,   7761,   8718,
     9781, 10987, 12339, 13828,  15523,  17435,  19561,  21873,
    24552, 27656, 30847, 34870,  38807,  43747,  49103,  54683,
    61694, 68745, 77615, 89113, 100253, 109366, 126635, 141533
];

pub fn chroma_transform(blk: &mut [i16; 4]) {
    let t0 = blk[0] + blk[2];
    let t1 = blk[0] - blk[2];
    let t2 = blk[1] + blk[3];
    let t3 = blk[1] - blk[3];
    blk[0] = t0 + t2;
    blk[1] = t0 - t2;
    blk[2] = t1 + t3;
    blk[3] = t1 - t3;
}

pub fn idct_dc_coeffs(blk: &mut [i16; 16], q: u8) {
    let quant = SVQ3_QUANTS[q as usize];
    let mut tmp = [0i32; 16];
    for (src, dst) in blk.chunks(4).zip(tmp.chunks_mut(4)) {
        let s0 = i32::from(src[0]);
        let s1 = i32::from(src[1]);
        let s2 = i32::from(src[2]);
        let s3 = i32::from(src[3]);
        let t0 = 13 * (s0 + s2);
        let t1 = 13 * (s0 - s2);
        let t2 = 17 * s1 +  7 * s3;
        let t3 =  7 * s1 - 17 * s3;
        dst[0] = t0 + t2;
        dst[1] = t1 + t3;
        dst[2] = t1 - t3;
        dst[3] = t0 - t2;
    }
    for i in 0..4 {
        let s0 = tmp[i];
        let s1 = tmp[i + 4];
        let s2 = tmp[i + 4 * 2];
        let s3 = tmp[i + 4 * 3];
        let t0 = 13 * (s0 + s2);
        let t1 = 13 * (s0 - s2);
        let t2 = 17 * s1 +  7 * s3;
        let t3 =  7 * s1 - 17 * s3;
        blk[i]         = (((t0 + t2).wrapping_mul(quant) + (1 << 19)) >> 20) as i16;
        blk[i + 4]     = (((t1 + t3).wrapping_mul(quant) + (1 << 19)) >> 20) as i16;
        blk[i + 4 * 2] = (((t1 - t3).wrapping_mul(quant) + (1 << 19)) >> 20) as i16;
        blk[i + 4 * 3] = (((t0 - t2).wrapping_mul(quant) + (1 << 19)) >> 20) as i16;
    }
}

pub fn idct(blk: &mut [i16; 16], q: u8, chroma: bool) {
    let quant = SVQ3_QUANTS[q as usize];
    let mut tmp = [0i32; 16];
    let dc = 13 * 13 * if chroma { quant * i32::from(blk[0]) / 2 } else { i32::from(blk[0]) * 1538 };
    blk[0] = 0;
    for (src, dst) in blk.chunks(4).zip(tmp.chunks_mut(4)) {
        let s0 = i32::from(src[0]);
        let s1 = i32::from(src[1]);
        let s2 = i32::from(src[2]);
        let s3 = i32::from(src[3]);
        let t0 = 13 * (s0 + s2);
        let t1 = 13 * (s0 - s2);
        let t2 = 17 * s1 +  7 * s3;
        let t3 =  7 * s1 - 17 * s3;
        dst[0] = t0 + t2;
        dst[1] = t1 + t3;
        dst[2] = t1 - t3;
        dst[3] = t0 - t2;
    }
    for i in 0..4 {
        let s0 = tmp[i];
        let s1 = tmp[i + 4];
        let s2 = tmp[i + 4 * 2];
        let s3 = tmp[i + 4 * 3];
        let t0 = 13 * (s0 + s2);
        let t1 = 13 * (s0 - s2);
        let t2 = 17 * s1 +  7 * s3;
        let t3 =  7 * s1 - 17 * s3;
        blk[i]         = (((t0 + t2).wrapping_mul(quant) + dc + (1 << 19)) >> 20) as i16;
        blk[i + 4]     = (((t1 + t3).wrapping_mul(quant) + dc + (1 << 19)) >> 20) as i16;
        blk[i + 4 * 2] = (((t1 - t3).wrapping_mul(quant) + dc + (1 << 19)) >> 20) as i16;
        blk[i + 4 * 3] = (((t0 - t2).wrapping_mul(quant) + dc + (1 << 19)) >> 20) as i16;
    }
}

pub fn add_coeffs(dst: &mut [u8], offset: usize, stride: usize, coeffs: &[i16]) {
    let out = &mut dst[offset..][..stride * 3 + 4];
    for (line, src) in out.chunks_mut(stride).take(4).zip(coeffs.chunks(4)) {
        for (dst, src) in line.iter_mut().take(4).zip(src.iter()) {
            *dst = (i32::from(*dst) + i32::from(*src)).max(0).min(255) as u8;
        }
    }
}

pub fn avg(dst: &mut [u8], dstride: usize,
           src: &[u8], sstride: usize, bw: usize, bh: usize) {
   for (dline, sline) in dst.chunks_mut(dstride).zip(src.chunks(sstride)).take(bh) {
        for (dst, src) in dline.iter_mut().zip(sline.iter()).take(bw) {
            *dst = ((u16::from(*dst) + u16::from(*src) + 1) >> 1) as u8;
        }
    }
}

fn clip8(val: i16) -> u8 { val.max(0).min(255) as u8 }

fn ipred_dc128(buf: &mut [u8], mut idx: usize, stride: usize, bsize: usize) {
    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = 128; }
        idx += stride;
    }
}
fn ipred_ver(buf: &mut [u8], mut idx: usize, stride: usize, bsize: usize) {
    let oidx = idx - stride;
    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = buf[oidx + x]; }
        idx += stride;
    }
}
fn ipred_hor(buf: &mut [u8], mut idx: usize, stride: usize, bsize: usize) {
    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = buf[idx - 1]; }
        idx += stride;
    }
}
fn ipred_dc(buf: &mut [u8], mut idx: usize, stride: usize, bsize: usize, shift: u8) {
    let mut adc: u16 = 0;
    for i in 0..bsize { adc += u16::from(buf[idx - stride + i]); }
    for i in 0..bsize { adc += u16::from(buf[idx - 1 + i * stride]); }
    let dc = ((adc + (1 << (shift - 1))) >> shift) as u8;

    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = dc; }
        idx += stride;
    }
}
fn ipred_left_dc(buf: &mut [u8], mut idx: usize, stride: usize, bsize: usize, shift: u8) {
    let mut adc: u16 = 0;
    for i in 0..bsize { adc += u16::from(buf[idx - 1 + i * stride]); }
    let dc = ((adc + (1 << (shift - 1))) >> shift) as u8;

    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = dc; }
        idx += stride;
    }
}
fn ipred_top_dc(buf: &mut [u8], mut idx: usize, stride: usize, bsize: usize, shift: u8) {
    let mut adc: u16 = 0;
    for i in 0..bsize { adc += u16::from(buf[idx - stride + i]); }
    let dc = ((adc + (1 << (shift - 1))) >> shift) as u8;

    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = dc; }
        idx += stride;
    }
}

fn load_top(dst: &mut [u16], buf: &mut [u8], idx: usize, stride: usize, len: usize) {
    for i in 0..len { dst[i] = u16::from(buf[idx - stride + i]); }
}
fn load_left(dst: &mut [u16], buf: &mut [u8], idx: usize, stride: usize, len: usize) {
    for i in 0..len { dst[i] = u16::from(buf[idx - 1 + i * stride]); }
}

fn ipred_4x4_ver(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    ipred_ver(buf, idx, stride, 4);
}
fn ipred_4x4_hor(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    ipred_hor(buf, idx, stride, 4);
}
fn ipred_4x4_diag_down_left(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    let mut t: [u16; 8] = [0; 8];
    let mut l: [u16; 8] = [0; 8];
    load_top(&mut t, buf, idx, stride, 4);
    load_left(&mut l, buf, idx, stride, 4);
    let a = ((l[1] + t[1]) >> 1) as u8;
    let b = ((l[2] + t[2]) >> 1) as u8;
    let c = ((l[3] + t[3]) >> 1) as u8;

    let dst = &mut buf[idx..];
    dst[0] = a; dst[1] = b; dst[2] = c; dst[3] = c;
    let dst = &mut buf[idx + stride..];
    dst[0] = b; dst[1] = c; dst[2] = c; dst[3] = c;
    let dst = &mut buf[idx + stride * 2..];
    dst[0] = c; dst[1] = c; dst[2] = c; dst[3] = c;
    let dst = &mut buf[idx + stride * 3..];
    dst[0] = c; dst[1] = c; dst[2] = c; dst[3] = c;
}
fn ipred_4x4_diag_down_right(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    let mut t: [u16; 5] = [0; 5];
    let mut l: [u16; 5] = [0; 5];
    load_top(&mut t, buf, idx - 1, stride, 5);
    load_left(&mut l, buf, idx - stride, stride, 5);
    let dst = &mut buf[idx..];

    for j in 0..4 {
        for i in 0..j {
            dst[i + j * stride] = ((l[j - i - 1] + 2 * l[j - i] + l[j - i + 1] + 2) >> 2) as u8;
        }
        dst[j + j * stride] = ((l[1] + 2 * l[0] + t[1] + 2) >> 2) as u8;
        for i in (j+1)..4 {
            dst[i + j * stride] = ((t[i - j - 1] + 2 * t[i - j] + t[i - j + 1] + 2) >> 2) as u8;
        }
    }
}
fn ipred_4x4_ver_right(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    let mut t: [u16; 5] = [0; 5];
    let mut l: [u16; 5] = [0; 5];
    load_top(&mut t, buf, idx - 1, stride, 5);
    load_left(&mut l, buf, idx - stride, stride, 5);
    let dst = &mut buf[idx..];

    for j in 0..4 {
        for i in 0..4 {
            let zvr = ((2 * i) as i8) - (j as i8);
            let pix;
            if zvr >= 0 {
                if (zvr & 1) == 0 {
                    pix = (t[i - (j >> 1)] + t[i - (j >> 1) + 1] + 1) >> 1;
                } else {
                    pix = (t[i - (j >> 1) - 1] + 2 * t[i - (j >> 1)] + t[i - (j >> 1) + 1] + 2) >> 2;
                }
            } else {
                if zvr == -1 {
                    pix = (l[1] + 2 * l[0] + t[1] + 2) >> 2;
                } else {
                    pix = (l[j] + 2 * l[j - 1] + l[j - 2] + 2) >> 2;
                }
            }
            dst[i + j * stride] = pix as u8;
        }
    }
}
fn ipred_4x4_ver_left(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8]) {
    let mut t: [u16; 8] = [0; 8];
    load_top(&mut t, buf, idx, stride, 4);
    for i in 0..4 { t[i + 4] = u16::from(tr[i]); }
    let dst = &mut buf[idx..];

    dst[0 + 0 * stride] = ((t[0] + t[1] + 1) >> 1) as u8;
    let pix = ((t[1] + t[2] + 1) >> 1) as u8;
    dst[1 + 0 * stride] = pix;
    dst[0 + 2 * stride] = pix;
    let pix = ((t[2] + t[3] + 1) >> 1) as u8;
    dst[2 + 0 * stride] = pix;
    dst[1 + 2 * stride] = pix;
    let pix = ((t[3] + t[4] + 1) >> 1) as u8;
    dst[3 + 0 * stride] = pix;
    dst[2 + 2 * stride] = pix;
    dst[3 + 2 * stride] = ((t[4] + t[5] + 1) >> 1) as u8;
    dst[0 + 1 * stride] = ((t[0] + 2*t[1] + t[2] + 2) >> 2) as u8;
    let pix = ((t[1] + 2*t[2] + t[3] + 2) >> 2) as u8;
    dst[1 + 1 * stride] = pix;
    dst[0 + 3 * stride] = pix;
    let pix = ((t[2] + 2*t[3] + t[4] + 2) >> 2) as u8;
    dst[2 + 1 * stride] = pix;
    dst[1 + 3 * stride] = pix;
    let pix = ((t[3] + 2*t[4] + t[5] + 2) >> 2) as u8;
    dst[3 + 1 * stride] = pix;
    dst[2 + 3 * stride] = pix;
    dst[3 + 3 * stride] = ((t[4] + 2*t[5] + t[6] + 2) >> 2) as u8;
}
fn ipred_4x4_hor_down(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    let mut t: [u16; 5] = [0; 5];
    let mut l: [u16; 5] = [0; 5];
    load_top(&mut t, buf, idx - 1, stride, 5);
    load_left(&mut l, buf, idx - stride, stride, 5);
    let dst = &mut buf[idx..];

    for j in 0..4 {
        for i in 0..4 {
            let zhd = ((2 * j) as i8) - (i as i8);
            let pix;
            if zhd >= 0 {
                if (zhd & 1) == 0 {
                    pix = (l[j - (i >> 1)] + l[j - (i >> 1) + 1] + 1) >> 1;
                } else {
                    pix = (l[j - (i >> 1) - 1] + 2 * l[j - (i >> 1)] + l[j - (i >> 1) + 1] + 2) >> 2;
                }
            } else {
                if zhd == -1 {
                    pix = (l[1] + 2 * l[0] + t[1] + 2) >> 2;
                } else {
                    pix = (t[i - 2] + 2 * t[i - 1] + t[i] + 2) >> 2;
                }
            }
            dst[i + j * stride] = pix as u8;
        }
    }
}
fn ipred_4x4_hor_up(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    let mut l: [u16; 8] = [0; 8];
    load_left(&mut l, buf, idx, stride, 8);
    let dst = &mut buf[idx..];

    dst[0 + 0 * stride] = ((l[0] + l[1] + 1) >> 1) as u8;
    dst[1 + 0 * stride] = ((l[0] + 2*l[1] + l[2] + 2) >> 2) as u8;
    let pix = ((l[1] + l[2] + 1) >> 1) as u8;
    dst[2 + 0 * stride] = pix;
    dst[0 + 1 * stride] = pix;
    let pix = ((l[1] + 2*l[2] + l[3] + 2) >> 2) as u8;
    dst[3 + 0 * stride] = pix;
    dst[1 + 1 * stride] = pix;
    let pix = ((l[2] + l[3] + 1) >> 1) as u8;
    dst[2 + 1 * stride] = pix;
    dst[0 + 2 * stride] = pix;
    let pix = ((l[2] + 3*l[3] + 2) >> 2) as u8;
    dst[3 + 1 * stride] = pix;
    dst[1 + 2 * stride] = pix;
    dst[3 + 2 * stride] = l[3] as u8;
    dst[1 + 3 * stride] = l[3] as u8;
    dst[0 + 3 * stride] = l[3] as u8;
    dst[2 + 2 * stride] = l[3] as u8;
    dst[2 + 3 * stride] = l[3] as u8;
    dst[3 + 3 * stride] = l[3] as u8;
}
fn ipred_4x4_dc(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    ipred_dc(buf, idx, stride, 4, 3);
}
fn ipred_4x4_left_dc(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    ipred_left_dc(buf, idx, stride, 4, 2);
}
fn ipred_4x4_top_dc(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    ipred_top_dc(buf, idx, stride, 4, 2);
}
fn ipred_4x4_dc128(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    ipred_dc128(buf, idx, stride, 4);
}

fn ipred_8x8_ver(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_ver(buf, idx, stride, 8);
}
fn ipred_8x8_hor(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_hor(buf, idx, stride, 8);
}
fn ipred_8x8_dc(buf: &mut [u8], idx: usize, stride: usize) {
    let mut t: [u16; 8] = [0; 8];
    load_top(&mut t, buf, idx, stride, 8);
    let mut l: [u16; 8] = [0; 8];
    load_left(&mut l, buf, idx, stride, 8);

    let dc0 = ((t[0] + t[1] + t[2] + t[3] + l[0] + l[1] + l[2] + l[3] + 4) >> 3) as u8;
    let sum1 = t[4] + t[5] + t[6] + t[7];
    let dc1 = ((sum1 + 2) >> 2) as u8;
    let sum2 = l[4] + l[5] + l[6] + l[7];
    let dc2 = ((sum2 + 2) >> 2) as u8;
    let dc3 = ((sum1 + sum2 + 4) >> 3) as u8;

    let dst = &mut buf[idx..];
    for row in dst.chunks_mut(stride).take(4) {
        row[..4].copy_from_slice(&[dc0; 4]);
        row[4..8].copy_from_slice(&[dc1; 4]);
    }
    for row in dst.chunks_mut(stride).skip(4).take(4) {
        row[..4].copy_from_slice(&[dc2; 4]);
        row[4..8].copy_from_slice(&[dc3; 4]);
    }
}
fn ipred_8x8_left_dc(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_left_dc(buf, idx, stride, 8, 3);
}
fn ipred_8x8_top_dc(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_top_dc(buf, idx, stride, 8, 3);
}
fn ipred_8x8_dc128(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_dc128(buf, idx, stride, 8);
}
fn ipred_8x8_plane(_buf: &mut [u8], _idx: usize, _stride: usize) {
    unreachable!();
/*    let mut h: i16 = 0;
    let mut v: i16 = 0;
    let     idx0 = idx + 3 - stride;
    let mut idx1 = idx + 4 * stride - 1;
    let mut idx2 = idx + 2 * stride - 1;
    for i in 0..4 {
        let i1 = (i + 1) as i16;
        h += i1 * (i16::from(buf[idx0 + i + 1]) - i16::from(buf[idx0 - i - 1]));
        v += i1 * (i16::from(buf[idx1]) - i16::from(buf[idx2]));
        idx1 += stride;
        idx2 -= stride;
    }
    let b = (17 * h + 16) >> 5;
    let c = (17 * v + 16) >> 5;
    let mut a = 16 * (i16::from(buf[idx - 1 + 7 * stride]) + i16::from(buf[idx + 7 - stride])) - 3 * (b + c) + 16;
    for line in buf[idx..].chunks_mut(stride).take(8) {
        let mut acc = a;
        for el in line.iter_mut().take(8) {
            *el = clip8(acc >> 5);
            acc += b;
        }
        a += c;
    }*/
}

fn ipred_16x16_ver(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_ver(buf, idx, stride, 16);
}
fn ipred_16x16_hor(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_hor(buf, idx, stride, 16);
}
fn ipred_16x16_dc(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_dc(buf, idx, stride, 16, 5);
}
fn ipred_16x16_left_dc(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_left_dc(buf, idx, stride, 16, 4);
}
fn ipred_16x16_top_dc(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_top_dc(buf, idx, stride, 16, 4);
}
fn ipred_16x16_dc128(buf: &mut [u8], idx: usize, stride: usize) {
    ipred_dc128(buf, idx, stride, 16);
}
fn ipred_16x16_plane(buf: &mut [u8], mut idx: usize, stride: usize) {
    let     idx0 = idx + 7 - stride;
    let mut idx1 = idx + 8 * stride - 1;
    let mut idx2 = idx1 - 2 * stride;

    let mut h = i16::from(buf[idx0 + 1]) - i16::from(buf[idx0 - 1]);
    let mut v = i16::from(buf[idx1])     - i16::from(buf[idx2]);

    for k in 2..9 {
        idx1 += stride;
        idx2 -= stride;
        h += (k as i16) * (i16::from(buf[idx0 + k]) - i16::from(buf[idx0 - k]));
        v += (k as i16) * (i16::from(buf[idx1])     - i16::from(buf[idx2]));
    }
    h = 5 * (h / 4) / 16;
    v = 5 * (v / 4) / 16;
    std::mem::swap(&mut h, &mut v);

    let mut a = 16 * (i16::from(buf[idx - 1 + 15 * stride]) + i16::from(buf[idx + 15 - stride]) + 1) - 7 * (v + h);

    for _ in 0..16 {
        let mut b = a;
        a += v;

        for dst in buf[idx..].chunks_mut(4).take(4) {
            dst[0] = clip8((b      ) >> 5);
            dst[1] = clip8((b +   h) >> 5);
            dst[2] = clip8((b + 2*h) >> 5);
            dst[3] = clip8((b + 3*h) >> 5);
            b += h * 4;
        }
        idx += stride;
    }
}

pub type IPred4x4Func = fn(buf: &mut [u8], off: usize, stride: usize, tr: &[u8]);
pub type IPred8x8Func = fn(buf: &mut [u8], off: usize, stride: usize);

pub const IPRED_FUNCS4X4: [IPred4x4Func; 12] = [
    ipred_4x4_ver, ipred_4x4_hor, ipred_4x4_dc,
    ipred_4x4_diag_down_left, ipred_4x4_diag_down_right,
    ipred_4x4_ver_right, ipred_4x4_hor_down, ipred_4x4_ver_left, ipred_4x4_hor_up,
    ipred_4x4_left_dc, ipred_4x4_top_dc, ipred_4x4_dc128
];

pub const IPRED_FUNCS8X8: [IPred8x8Func; 7] = [
    ipred_8x8_dc, ipred_8x8_hor, ipred_8x8_ver, ipred_8x8_plane,
    ipred_8x8_left_dc, ipred_8x8_top_dc, ipred_8x8_dc128
];

pub const IPRED_FUNCS16X16: [IPred8x8Func; 7] = [
    ipred_16x16_dc, ipred_16x16_hor, ipred_16x16_ver, ipred_16x16_plane,
    ipred_16x16_left_dc, ipred_16x16_top_dc, ipred_16x16_dc128
];

fn tpel_interp00(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    for (dline, sline) in dst.chunks_mut(dstride).zip(src.chunks(sstride)).take(bh) {
        dline[..bw].copy_from_slice(&sline[..bw]);
    }
}

fn interp2(val: u32) -> u8 {
    (((val + 1) * 683) >> 11) as u8
}

fn interp4(val: u32) -> u8 {
    (((val + 6) * 2731) >> 15) as u8
}

fn tpel_interp01(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    for (dline, sline) in dst.chunks_mut(dstride).zip(src.chunks(sstride)).take(bh) {
        let mut last = u32::from(sline[0]);
        for (dst, src) in dline.iter_mut().take(bw).zip(sline[1..].iter()) {
            let new = u32::from(*src);
            *dst = interp2(last * 2 + new);
            last = new;
        }
    }
}

fn tpel_interp02(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    for (dline, sline) in dst.chunks_mut(dstride).zip(src.chunks(sstride)).take(bh) {
        let mut last = u32::from(sline[0]);
        for (dst, src) in dline.iter_mut().take(bw).zip(sline[1..].iter()) {
            let new = u32::from(*src);
            *dst = interp2(last + new * 2);
            last = new;
        }
    }
}

fn tpel_interp10(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    let src1 = &src[sstride..];
    for (dline, (sline0, sline1)) in dst.chunks_mut(dstride).zip(src.chunks(sstride).zip(src1.chunks(sstride))).take(bh) {
        for (dst, (s0, s1)) in dline.iter_mut().zip(sline0.iter().zip(sline1.iter())).take(bw) {
            *dst = interp2(u32::from(*s0) * 2 + u32::from(*s1));
        }
    }
}

fn tpel_interp11(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    let mut sidx0 = 0;
    let mut sidx1 = sstride;
    for dline in dst.chunks_mut(dstride).take(bh) {
        for (x, dst) in dline.iter_mut().take(bw).enumerate() {
            *dst = interp4(u32::from(src[sidx0 + x]) * 4 + u32::from(src[sidx0 + x + 1]) * 3 +
                           u32::from(src[sidx1 + x]) * 3 + u32::from(src[sidx1 + x + 1]) * 2);
        }
        sidx0 += sstride;
        sidx1 += sstride;
    }
}

fn tpel_interp12(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    let mut sidx0 = 0;
    let mut sidx1 = sstride;
    for dline in dst.chunks_mut(dstride).take(bh) {
        for (x, dst) in dline.iter_mut().take(bw).enumerate() {
            *dst = interp4(u32::from(src[sidx0 + x]) * 3 + u32::from(src[sidx0 + x + 1]) * 4 +
                           u32::from(src[sidx1 + x]) * 2 + u32::from(src[sidx1 + x + 1]) * 3);
        }
        sidx0 += sstride;
        sidx1 += sstride;
    }
}

fn tpel_interp20(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    let src1 = &src[sstride..];
    for (dline, (sline0, sline1)) in dst.chunks_mut(dstride).zip(src.chunks(sstride).zip(src1.chunks(sstride))).take(bh) {
        for (dst, (s0, s1)) in dline.iter_mut().zip(sline0.iter().zip(sline1.iter())).take(bw) {
            *dst = interp2(u32::from(*s0) + u32::from(*s1) * 2);
        }
    }
}

fn tpel_interp21(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    let mut sidx0 = 0;
    let mut sidx1 = sstride;
    for dline in dst.chunks_mut(dstride).take(bh) {
        for (x, dst) in dline.iter_mut().take(bw).enumerate() {
            *dst = interp4(u32::from(src[sidx0 + x]) * 3 + u32::from(src[sidx0 + x + 1]) * 2 +
                           u32::from(src[sidx1 + x]) * 4 + u32::from(src[sidx1 + x + 1]) * 3);
        }
        sidx0 += sstride;
        sidx1 += sstride;
    }
}

fn tpel_interp22(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    let mut sidx0 = 0;
    let mut sidx1 = sstride;
    for dline in dst.chunks_mut(dstride).take(bh) {
        for (x, dst) in dline.iter_mut().take(bw).enumerate() {
            *dst = interp4(u32::from(src[sidx0 + x]) * 2 + u32::from(src[sidx0 + x + 1]) * 3 +
                           u32::from(src[sidx1 + x]) * 3 + u32::from(src[sidx1 + x + 1]) * 4);
        }
        sidx0 += sstride;
        sidx1 += sstride;
    }
}

pub const THIRDPEL_INTERP_FUNCS: &[BlkInterpFunc] = &[
    tpel_interp00, tpel_interp01, tpel_interp02,
    tpel_interp10, tpel_interp11, tpel_interp12,
    tpel_interp20, tpel_interp21, tpel_interp22
];
