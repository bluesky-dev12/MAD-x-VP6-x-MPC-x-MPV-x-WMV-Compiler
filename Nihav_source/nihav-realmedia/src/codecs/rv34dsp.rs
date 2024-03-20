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
    DiagDownLeftNoDown,
    HorUpNoDown,
    VerLeftNoDown
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

type IPred4x4Func = fn(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8]);

pub struct RV34CommonDSP {
    pub ipred4x4:   [IPred4x4Func; 15],
    pub ipred8x8:   [fn(buf: &mut [u8], idx: usize, stride: usize); 7],
    pub ipred16x16: [fn(buf: &mut [u8], idx: usize, stride: usize); 7],
}

#[allow(clippy::erasing_op)]
fn row_transform(src: &[i16], dst: &mut [i32]) {
    for i in 0..4 {
        let z0 = 13 * ((src[i + 4*0] as i32) + (src[i + 4*2] as i32));
        let z1 = 13 * ((src[i + 4*0] as i32) - (src[i + 4*2] as i32));
        let z2 =  7 *  (src[i + 4*1] as i32) - 17 * (src[i + 4*3] as i32);
        let z3 = 17 *  (src[i + 4*1] as i32) +  7 * (src[i + 4*3] as i32);
        dst[4 * i + 0] = z0 + z3;
        dst[4 * i + 1] = z1 + z2;
        dst[4 * i + 2] = z1 - z2;
        dst[4 * i + 3] = z0 - z3;
    }
}

fn clip8(a: i16) -> u8 {
    if a < 0 { 0 }
    else if a > 255 { 255 }
    else { a as u8 }
}

#[inline(always)]
fn mclip8(a: i32) -> u8 {
    if (a as u32) > 255 { !(a >> 16) as u8 }
    else { a as u8 }
}

#[allow(clippy::erasing_op)]
impl RV34CommonDSP {
    pub fn new() -> Self {
        Self {
            ipred4x4:   IPRED_FUNCS4X4,
            ipred8x8:   IPRED_FUNCS8X8,
            ipred16x16: IPRED_FUNCS16X16,
        }
    }
    pub fn add_coeffs(&self, dst: &mut [u8], idx: usize, stride: usize, coeffs: &[i16]) {
        let out = &mut dst[idx..][..stride * 3 + 4];
        let mut sidx: usize = 0;
        for el in out.chunks_mut(stride).take(4) {
            assert!(el.len() >= 4);
            el[0] = mclip8((el[0] as i32) + (coeffs[0 + sidx] as i32));
            el[1] = mclip8((el[1] as i32) + (coeffs[1 + sidx] as i32));
            el[2] = mclip8((el[2] as i32) + (coeffs[2 + sidx] as i32));
            el[3] = mclip8((el[3] as i32) + (coeffs[3 + sidx] as i32));
            sidx += 4;
        }
    }
    pub fn transform(&self, coeffs: &mut [i16]) {
        let mut tmp: [i32; 16] = [0; 16];
        row_transform(coeffs, &mut tmp);
        for i in 0..4 {
            let z0 = 13*(tmp[4*0+i] +    tmp[4*2+i]) + 0x200;
            let z1 = 13*(tmp[4*0+i] -    tmp[4*2+i]) + 0x200;
            let z2 =  7* tmp[4*1+i] - 17*tmp[4*3+i];
            let z3 = 17* tmp[4*1+i] +  7*tmp[4*3+i];
            coeffs[i * 4 + 0] = ((z0 + z3) >> 10) as i16;
            coeffs[i * 4 + 1] = ((z1 + z2) >> 10) as i16;
            coeffs[i * 4 + 2] = ((z1 - z2) >> 10) as i16;
            coeffs[i * 4 + 3] = ((z0 - z3) >> 10) as i16;
        }
    }
    pub fn transform_dc(&self, coeffs: &mut [i16]) {
        let val = (((coeffs[0] as i32) * 13 * 13 + 0x200) >> 10) as i16;
        for i in 0..16 { coeffs[i] = val; }
    }
    pub fn transform16(&self, coeffs: &mut [i16]) {
        let mut tmp: [i32; 16] = [0; 16];
        row_transform(coeffs, &mut tmp);
        for i in 0..4 {
            let z0 = 39*(tmp[4*0+i] +    tmp[4*2+i]);
            let z1 = 39*(tmp[4*0+i] -    tmp[4*2+i]);
            let z2 = 21* tmp[4*1+i] - 51*tmp[4*3+i];
            let z3 = 51* tmp[4*1+i] + 21*tmp[4*3+i];

            coeffs[i * 4 + 0] = ((z0 + z3) >> 11) as i16;
            coeffs[i * 4 + 1] = ((z1 + z2) >> 11) as i16;
            coeffs[i * 4 + 2] = ((z1 - z2) >> 11) as i16;
            coeffs[i * 4 + 3] = ((z0 - z3) >> 11) as i16;
        }
    }
    pub fn transform16_dc(&self, coeffs: &mut [i16]) {
        let val = (((coeffs[0] as i32) * 13 * 13 * 3) >> 11) as i16;
        for i in 0..16 { coeffs[i] = val; }
    }
    pub fn weight(&self, dst: &mut [u8], mut didx: usize, dstride: usize,
              src: &[u8], mut sidx: usize, sstride: usize, ratio1: u32, ratio2: u32,
              size: usize) {
        for _ in 0..size {
            for x in 0..size {
                dst[didx + x] = (((((dst[didx + x] as u32) * ratio1) >> 9)
                                + (((src[sidx + x] as u32) * ratio2) >> 9) + 0x10) >> 5) as u8;
            }
            didx += dstride;
            sidx += sstride;
        }
    }
    pub fn avg(&self, dst: &mut [u8], mut didx: usize, dstride: usize,
               src: &[u8], mut sidx: usize, sstride: usize,
               size: usize) {
        for _ in 0..size {
            for x in 0..size {
                dst[didx + x] = (((dst[didx + x] as u16) + (src[sidx + x] as u16) + 1) >> 1) as u8;
            }
            didx += dstride;
            sidx += sstride;
        }
    }
}

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
    for i in 0..bsize { adc += buf[idx - stride + i] as u16; }
    for i in 0..bsize { adc += buf[idx - 1 + i * stride] as u16; }
    let dc = ((adc + (1 << (shift - 1))) >> shift) as u8;

    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = dc; }
        idx += stride;
    }
}
fn ipred_left_dc(buf: &mut [u8], mut idx: usize, stride: usize, bsize: usize, shift: u8) {
    let mut adc: u16 = 0;
    for i in 0..bsize { adc += buf[idx - 1 + i * stride] as u16; }
    let dc = ((adc + (1 << (shift - 1))) >> shift) as u8;

    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = dc; }
        idx += stride;
    }
}
fn ipred_top_dc(buf: &mut [u8], mut idx: usize, stride: usize, bsize: usize, shift: u8) {
    let mut adc: u16 = 0;
    for i in 0..bsize { adc += buf[idx - stride + i] as u16; }
    let dc = ((adc + (1 << (shift - 1))) >> shift) as u8;

    for _ in 0..bsize {
        for x in 0..bsize { buf[idx + x] = dc; }
        idx += stride;
    }
}

fn load_top(dst: &mut [u16], buf: &mut [u8], idx: usize, stride: usize, len: usize) {
    for i in 0..len { dst[i] = buf[idx - stride + i] as u16; }
}
fn load_left(dst: &mut [u16], buf: &mut [u8], idx: usize, stride: usize, len: usize) {
    for i in 0..len { dst[i] = buf[idx - 1 + i * stride] as u16; }
}

fn ipred_4x4_ver(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    ipred_ver(buf, idx, stride, 4);
}
fn ipred_4x4_hor(buf: &mut [u8], idx: usize, stride: usize, _tr: &[u8]) {
    ipred_hor(buf, idx, stride, 4);
}
fn ipred_4x4_diag_down_left(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8]) {
    let mut t: [u16; 8] = [0; 8];
    let mut l: [u16; 8] = [0; 8];
    load_top(&mut t, buf, idx, stride, 4);
    for i in 0..4 { t[i + 4] = tr[i] as u16; }
    load_left(&mut l, buf, idx, stride, 8);
    let dst = &mut buf[idx..];

    dst[0 + 0 * stride] = ((t[0] + t[2] + 2*t[1] + 2 + l[0] + l[2] + 2*l[1] + 2) >> 3) as u8;
    let pix = ((t[1] + t[3] + 2*t[2] + 2 + l[1] + l[3] + 2*l[2] + 2) >> 3) as u8;
    dst[1 + 0 * stride] = pix;
    dst[0 + 1 * stride] = pix;
    let pix = ((t[2] + t[4] + 2*t[3] + 2 + l[2] + l[4] + 2*l[3] + 2) >> 3) as u8;
    dst[2 + 0 * stride] = pix;
    dst[1 + 1 * stride] = pix;
    dst[0 + 2 * stride] = pix;
    let pix = ((t[3] + t[5] + 2*t[4] + 2 + l[3] + l[5] + 2*l[4] + 2) >> 3) as u8;
    dst[3 + 0 * stride] = pix;
    dst[2 + 1 * stride] = pix;
    dst[1 + 2 * stride] = pix;
    dst[0 + 3 * stride] = pix;
    let pix = ((t[4] + t[6] + 2*t[5] + 2 + l[4] + l[6] + 2*l[5] + 2) >> 3) as u8;
    dst[3 + 1 * stride] = pix;
    dst[2 + 2 * stride] = pix;
    dst[1 + 3 * stride] = pix;
    let pix = ((t[5] + t[7] + 2*t[6] + 2 + l[5] + l[7] + 2*l[6] + 2) >> 3) as u8;
    dst[3 + 2 * stride] = pix;
    dst[2 + 3 * stride] = pix;
    dst[3 + 3 * stride] = ((t[6] + t[7] + 1 + l[6] + l[7] + 1) >> 2) as u8;
}
fn ipred_4x4_diag_down_left_nodown(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8]) {
    let mut t: [u16; 8] = [0; 8];
    let mut l: [u16; 4] = [0; 4];
    load_top(&mut t, buf, idx, stride, 4);
    for i in 0..4 { t[i + 4] = tr[i] as u16; }
    load_left(&mut l, buf, idx, stride, 4);
    let dst = &mut buf[idx..];

    dst[0 + 0 * stride] = ((t[0] + t[2] + 2*t[1] + 2 + l[0] + l[2] + 2*l[1] + 2) >> 3) as u8;
    let pix = ((t[1] + t[3] + 2*t[2] + 2 + l[1] + l[3] + 2*l[2] + 2) >> 3) as u8;
    dst[1 + 0 * stride] = pix;
    dst[0 + 1 * stride] = pix;
    let pix = ((t[2] + t[4] + 2*t[3] + 2 + l[2] + 3*l[3] + 2) >> 3) as u8;
    dst[2 + 0 * stride] = pix;
    dst[1 + 1 * stride] = pix;
    dst[0 + 2 * stride] = pix;
    let pix = ((t[3] + t[5] + 2*t[4] + 2 + l[3]*4 + 2) >> 3) as u8;
    dst[3 + 0 * stride] = pix;
    dst[2 + 1 * stride] = pix;
    dst[1 + 2 * stride] = pix;
    dst[0 + 3 * stride] = pix;
    let pix = ((t[4] + t[6] + 2*t[5] + 2 + l[3]*4 + 2) >> 3) as u8;
    dst[3 + 1 * stride] = pix;
    dst[2 + 2 * stride] = pix;
    dst[1 + 3 * stride] = pix;
    let pix = ((t[5] + t[7] + 2*t[6] + 2 + l[3]*4 + 2) >> 3) as u8;
    dst[3 + 2 * stride] = pix;
    dst[2 + 3 * stride] = pix;
    dst[3 + 3 * stride] = ((t[6] + t[7] + 1 + 2*l[3] + 1) >> 2) as u8;
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
fn ipred_4x4_ver_left_common(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8], no_down: bool) {
    let mut t: [u16; 8] = [0; 8];
    let mut l: [u16; 5] = [0; 5];
    load_top(&mut t, buf, idx, stride, 4);
    for i in 0..4 { t[i + 4] = tr[i] as u16; }
    load_left(&mut l, buf, idx, stride, 4);
    l[4] = if no_down { l[3] } else { buf[idx - 1 + 4 * stride] as u16 };
    let dst = &mut buf[idx..];

    dst[0 + 0 * stride] = ((2*t[0] + 2*t[1] + l[1] + 2*l[2] + l[3] + 4) >> 3) as u8;
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
    dst[0 + 1 * stride] = ((t[0] + 2*t[1] + t[2] + l[2] + 2*l[3] + l[4] + 4) >> 3) as u8;
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

fn ipred_4x4_ver_left(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8]) {
    ipred_4x4_ver_left_common(buf, idx, stride, tr, false);
}
fn ipred_4x4_ver_left_nodown(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8]) {
    ipred_4x4_ver_left_common(buf, idx, stride, tr, true);
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
fn ipred_4x4_hor_up(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8]) {
    let mut t: [u16; 8] = [0; 8];
    let mut l: [u16; 8] = [0; 8];
    load_top(&mut t, buf, idx, stride, 4);
    for i in 0..4 { t[i + 4] = tr[i] as u16; }
    load_left(&mut l, buf, idx, stride, 8);
    let dst = &mut buf[idx..];

    dst[0 + 0 * stride] = ((t[1] + 2*t[2] + t[3] + 2*l[0] + 2*l[1] + 4) >> 3) as u8;
    dst[1 + 0 * stride] = ((t[2] + 2*t[3] + t[4] + l[0] + 2*l[1] + l[2] + 4) >> 3) as u8;
    let pix = ((t[3] + 2*t[4] + t[5] + 2*l[1] + 2*l[2] + 4) >> 3) as u8;
    dst[2 + 0 * stride] = pix;
    dst[0 + 1 * stride] = pix;
    let pix = ((t[4] + 2*t[5] + t[6] + l[1] + 2*l[2] + l[3] + 4) >> 3) as u8;
    dst[3 + 0 * stride] = pix;
    dst[1 + 1 * stride] = pix;
    let pix = ((t[5] + 2*t[6] + t[7] + 2*l[2] + 2*l[3] + 4) >> 3) as u8;
    dst[2 + 1 * stride] = pix;
    dst[0 + 2 * stride] = pix;
    let pix = ((t[6] + 3*t[7] + l[2] + 3*l[3] + 4) >> 3) as u8;
    dst[3 + 1 * stride] = pix;
    dst[1 + 2 * stride] = pix;
    let pix = ((l[3] + 2*l[4] + l[5] + 2) >> 2) as u8;
    dst[3 + 2 * stride] = pix;
    dst[1 + 3 * stride] = pix;
    let pix = ((t[6] + t[7] + l[3] + l[4] + 2) >> 2) as u8;
    dst[0 + 3 * stride] = pix;
    dst[2 + 2 * stride] = pix;
    dst[2 + 3 * stride] = ((l[4] + l[5] + 1) >> 1) as u8;
    dst[3 + 3 * stride] = ((l[4] + 2*l[5] + l[6] + 2) >> 2) as u8;
}
fn ipred_4x4_hor_up_nodown(buf: &mut [u8], idx: usize, stride: usize, tr: &[u8]) {
    let mut t: [u16; 8] = [0; 8];
    let mut l: [u16; 4] = [0; 4];
    load_top(&mut t, buf, idx, stride, 4);
    for i in 0..4 { t[i + 4] = tr[i] as u16; }
    load_left(&mut l, buf, idx, stride, 4);
    let dst = &mut buf[idx..];

    dst[0 + 0 * stride] = ((t[1] + 2*t[2] + t[3] + 2*l[0] + 2*l[1] + 4) >> 3) as u8;
    dst[1 + 0 * stride] = ((t[2] + 2*t[3] + t[4] + l[0] + 2*l[1] + l[2] + 4) >> 3) as u8;
    let pix = ((t[3] + 2*t[4] + t[5] + 2*l[1] + 2*l[2] + 4) >> 3) as u8;
    dst[2 + 0 * stride] = pix;
    dst[0 + 1 * stride] = pix;
    let pix = ((t[4] + 2*t[5] + t[6] + l[1] + 2*l[2] + l[3] + 4) >> 3) as u8;
    dst[3 + 0 * stride] = pix;
    dst[1 + 1 * stride] = pix;
    let pix = ((t[5] + 2*t[6] + t[7] + 2*l[2] + 2*l[3] + 4) >> 3) as u8;
    dst[2 + 1 * stride] = pix;
    dst[0 + 2 * stride] = pix;
    let pix = ((t[6] + 3*t[7] + l[2] + 3*l[3] + 4) >> 3) as u8;
    dst[3 + 1 * stride] = pix;
    dst[1 + 2 * stride] = pix;
    dst[3 + 2 * stride] = l[3] as u8;
    dst[1 + 3 * stride] = l[3] as u8;
    let pix = ((t[6] + t[7] + 2*l[3] + 2) >> 2) as u8;
    dst[0 + 3 * stride] = pix;
    dst[2 + 2 * stride] = pix;
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
    ipred_dc(buf, idx, stride, 8, 4);
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
    for i in 0..4 {
        let i1 = (i + 1) as i16;
        h += i1 * ((buf[idx + (4 + i)          - stride] as i16) - (buf[idx + (2 - i)          - stride] as i16));
        v += i1 * ((buf[idx + (4 + i) * stride - 1]      as i16) - (buf[idx + (2 - i) * stride - 1] as i16));
    }
    let a = 16 * ((buf[idx - 1 + 7 * stride] as i16) + (buf[idx + 7 - stride] as i16));
    let b = (17 * h + 16) >> 5;
    let c = (17 * v + 16) >> 5;
    for y in 0..8 {
        let j3 = (y as i16) - 3;
        for x in 0..8 {
            let i3 = (x as i16) - 3;
            buf[idx + x] = clip8((a + b * i3 + c * j3 + 16) >> 5);
        }
        idx += stride;
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
    let mut idx1 = idx + 8*stride - 1;
    let mut idx2 = idx1 - 2*stride;

    let mut h = (buf[idx0 + 1] as i16) - (buf[idx0 - 1] as i16);
    let mut v = (buf[idx1 + 0] as i16) - (buf[idx2 + 0] as i16);

    for k in 2..9 {
        idx1 += stride;
        idx2 -= stride;
        h += (k as i16) * ((buf[idx0 + k] as i16) - (buf[idx0 - k] as i16));
        v += (k as i16) * ((buf[idx1 + 0] as i16) - (buf[idx2 + 0] as i16));
    }
    h = (h + (h >> 2)) >> 4;
    v = (v + (v >> 2)) >> 4;

    let mut a = 16 * ((buf[idx1 + 0] as i16) + (buf[idx2 + 16] as i16) + 1) - 7 * (v + h);

    for _ in 0..16 {
        let mut b = a;
        a += v;

        for x in 0..4 {
            buf[idx + x * 4 + 0] = clip8((b      ) >> 5);
            buf[idx + x * 4 + 1] = clip8((b +   h) >> 5);
            buf[idx + x * 4 + 2] = clip8((b + 2*h) >> 5);
            buf[idx + x * 4 + 3] = clip8((b + 3*h) >> 5);
            b += h * 4;
        }
        idx += stride;
    }
}

const IPRED_FUNCS4X4: [IPred4x4Func; 15] = [
    ipred_4x4_ver, ipred_4x4_hor, ipred_4x4_dc,
    ipred_4x4_diag_down_left, ipred_4x4_diag_down_right,
    ipred_4x4_ver_right, ipred_4x4_hor_down, ipred_4x4_ver_left, ipred_4x4_hor_up,
    ipred_4x4_left_dc, ipred_4x4_top_dc, ipred_4x4_dc128,
    ipred_4x4_diag_down_left_nodown, ipred_4x4_hor_up_nodown, ipred_4x4_ver_left_nodown
];

const IPRED_FUNCS8X8: [fn(buf: &mut [u8], idx: usize, stride: usize); 7] = [
    ipred_8x8_dc, ipred_8x8_hor, ipred_8x8_ver, ipred_8x8_plane,
    ipred_8x8_left_dc, ipred_8x8_top_dc, ipred_8x8_dc128
];

const IPRED_FUNCS16X16: [fn(buf: &mut [u8], idx: usize, stride: usize); 7] = [
    ipred_16x16_dc, ipred_16x16_hor, ipred_16x16_ver, ipred_16x16_plane,
    ipred_16x16_left_dc, ipred_16x16_top_dc, ipred_16x16_dc128
];
