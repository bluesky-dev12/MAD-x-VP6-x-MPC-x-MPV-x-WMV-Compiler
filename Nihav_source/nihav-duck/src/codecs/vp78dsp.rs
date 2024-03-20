use nihav_core::frame::*;
use nihav_codec_support::codecs::blockdsp::edge_emu;

fn clip_u8(val: i16) -> u8 {
    val.max(0).min(255) as u8
}

pub struct IPredContext {
    pub left:       [u8; 16],
    pub has_left:   bool,
    pub top:        [u8; 16],
    pub has_top:    bool,
    pub tl:         u8,
}

impl IPredContext {
    pub fn fill(&mut self, src: &[u8], off: usize, stride: usize, tsize: usize, lsize: usize) {
        if self.has_top {
            for i in 0..tsize {
                self.top[i] = src[off - stride + i];
            }
            for i in tsize..16 {
                self.top[i] = 0x80;
            }
        } else {
            self.top = [0x80; 16];
        }
        if self.has_left {
            for i in 0..lsize {
                self.left[i] = src[off - 1 + i * stride];
            }
            for i in lsize..16 {
                self.left[i] = 0x80;
            }
        } else {
            self.left = [0x80; 16];
        }
        if self.has_top && self.has_left {
            self.tl = src[off - stride - 1];
        } else {
            self.tl = 0x80;
        }
    }
}

impl Default for IPredContext {
    fn default() -> Self {
        Self {
            left:       [0x80; 16],
            top:        [0x80; 16],
            tl:         0x80,
            has_left:   false,
            has_top:    false,
        }
    }
}

pub fn add_coeffs4x4(dst: &mut [u8], off: usize, stride: usize, coeffs: &[i16; 16]) {
    let dst = &mut dst[off..];
    for (out, src) in dst.chunks_mut(stride).zip(coeffs.chunks(4)) {
        for (oel, iel) in out.iter_mut().take(4).zip(src.iter()) {
            *oel = clip_u8(i16::from(*oel) + *iel);
        }
    }
}
pub fn add_coeffs16x1(dst: &mut [u8], off: usize, coeffs: &[i16; 16]) {
    let dst = &mut dst[off..];
    for (oel, iel) in dst.iter_mut().take(16).zip(coeffs.iter()) {
        *oel = clip_u8(i16::from(*oel) + *iel);
    }
}

pub trait IntraPred {
    const SIZE: usize;
    fn ipred_dc(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let dc;
        if !ipred.has_left && !ipred.has_top {
            dc = 0x80;
        } else {
            let mut dcsum = 0;
            let mut dcshift = match Self::SIZE {
                    16 => 3,
                    _  => 2,
                };
            if ipred.has_left {
                for el in ipred.left.iter().take(Self::SIZE) {
                    dcsum += u16::from(*el);
                }
                dcshift += 1;
            }
            if ipred.has_top {
                for el in ipred.top.iter().take(Self::SIZE) {
                    dcsum += u16::from(*el);
                }
                dcshift += 1;
            }
            dc = ((dcsum + (1 << (dcshift - 1))) >> dcshift) as u8;
        }
        for _ in 0..Self::SIZE {
            let out = &mut dst[off..][..Self::SIZE];
            for el in out.iter_mut() {
                *el = dc;
            }
            off += stride;
        }
    }
    fn ipred_v(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        for _ in 0..Self::SIZE {
            let out = &mut dst[off..][..Self::SIZE];
            out.copy_from_slice(&ipred.top[0..Self::SIZE]);
            off += stride;
        }
    }
    fn ipred_h(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        for leftel in ipred.left.iter().take(Self::SIZE) {
            let out = &mut dst[off..][..Self::SIZE];
            for el in out.iter_mut() {
                *el = *leftel;
            }
            off += stride;
        }
    }
    fn ipred_tm(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let tl = i16::from(ipred.tl);
        for m in 0..Self::SIZE {
            for n in 0..Self::SIZE {
                dst[off + n] = clip_u8(i16::from(ipred.left[m]) + i16::from(ipred.top[n]) - tl);
            }
            off += stride;
        }
    }
    fn ipred_const(dst: &mut [u8], off: usize, stride: usize, dc: u8) {
        for row in dst[off..].chunks_mut(stride).take(Self::SIZE) {
            for el in row[..Self::SIZE].iter_mut() {
                *el = dc;
            }
        }
    }
}

pub struct IPred16x16 {}
impl IntraPred for IPred16x16 { const SIZE: usize = 16; }

pub struct IPred8x8 {}
impl IntraPred for IPred8x8 { const SIZE: usize = 8; }

macro_rules! load_pred4 {
    (topleft; $ipred: expr) => {{
        let tl = u16::from($ipred.tl);
        let a0 = u16::from($ipred.top[0]);
        let l0 = u16::from($ipred.left[0]);
        ((l0 + tl * 2 + a0 + 2) >> 2) as u8
    }};
    (top; $ipred: expr) => {{
        let tl = u16::from($ipred.tl);
        let a0 = u16::from($ipred.top[0]);
        let a1 = u16::from($ipred.top[1]);
        let a2 = u16::from($ipred.top[2]);
        let a3 = u16::from($ipred.top[3]);
        let a4 = u16::from($ipred.top[4]);
        let p0 = ((tl + a0 * 2 + a1 + 2) >> 2) as u8;
        let p1 = ((a0 + a1 * 2 + a2 + 2) >> 2) as u8;
        let p2 = ((a1 + a2 * 2 + a3 + 2) >> 2) as u8;
        let p3 = ((a2 + a3 * 2 + a4 + 2) >> 2) as u8;
        (p0, p1, p2, p3)
    }};
    (top8; $ipred: expr) => {{
        let t3 = u16::from($ipred.top[3]);
        let t4 = u16::from($ipred.top[4]);
        let t5 = u16::from($ipred.top[5]);
        let t6 = u16::from($ipred.top[6]);
        let t7 = u16::from($ipred.top[7]);
        let p4 = ((t3 + t4 * 2 + t5 + 2) >> 2) as u8;
        let p5 = ((t4 + t5 * 2 + t6 + 2) >> 2) as u8;
        let p6 = ((t5 + t6 * 2 + t7 + 2) >> 2) as u8;
        let p7 = ((t6 + t7 * 2 + t7 + 2) >> 2) as u8;
        (p4, p5, p6, p7)
    }};
    (topavg; $ipred: expr) => {{
        let tl = u16::from($ipred.tl);
        let a0 = u16::from($ipred.top[0]);
        let a1 = u16::from($ipred.top[1]);
        let a2 = u16::from($ipred.top[2]);
        let a3 = u16::from($ipred.top[3]);
        let p0 = ((tl + a0 + 1) >> 1) as u8;
        let p1 = ((a0 + a1 + 1) >> 1) as u8;
        let p2 = ((a1 + a2 + 1) >> 1) as u8;
        let p3 = ((a2 + a3 + 1) >> 1) as u8;
        (p0, p1, p2, p3)
    }};
    (left; $ipred: expr) => {{
        let tl = u16::from($ipred.tl);
        let l0 = u16::from($ipred.left[0]);
        let l1 = u16::from($ipred.left[1]);
        let l2 = u16::from($ipred.left[2]);
        let l3 = u16::from($ipred.left[3]);
        let l4 = u16::from($ipred.left[4]);
        let p0 = ((tl + l0 * 2 + l1 + 2) >> 2) as u8;
        let p1 = ((l0 + l1 * 2 + l2 + 2) >> 2) as u8;
        let p2 = ((l1 + l2 * 2 + l3 + 2) >> 2) as u8;
        let p3 = ((l2 + l3 * 2 + l4 + 2) >> 2) as u8;
        (p0, p1, p2, p3)
    }};
    (left8; $ipred: expr) => {{
        let l3 = u16::from($ipred.left[3]);
        let l4 = u16::from($ipred.left[4]);
        let l5 = u16::from($ipred.left[5]);
        let l6 = u16::from($ipred.left[6]);
        let l7 = u16::from($ipred.left[7]);
        let p4 = ((l3 + l4 * 2 + l5 + 2) >> 2) as u8;
        let p5 = ((l4 + l5 * 2 + l6 + 2) >> 2) as u8;
        let p6 = ((l5 + l6 * 2 + l7 + 2) >> 2) as u8;
        let p7 = ((l6 + l7 * 2 + l7 + 2) >> 2) as u8;
        (p4, p5, p6, p7)
    }};
    (leftavg; $ipred: expr) => {{
        let tl = u16::from($ipred.tl);
        let l0 = u16::from($ipred.left[0]);
        let l1 = u16::from($ipred.left[1]);
        let l2 = u16::from($ipred.left[2]);
        let l3 = u16::from($ipred.left[3]);
        let p0 = ((tl + l0 + 1) >> 1) as u8;
        let p1 = ((l0 + l1 + 1) >> 1) as u8;
        let p2 = ((l1 + l2 + 1) >> 1) as u8;
        let p3 = ((l2 + l3 + 1) >> 1) as u8;
        (p0, p1, p2, p3)
    }};
}

pub struct IPred4x4 {}
impl IPred4x4 {
    pub fn ipred_dc(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let mut dcsum = 0;
        for el in ipred.left.iter().take(4) {
            dcsum += u16::from(*el);
        }
        for el in ipred.top.iter().take(4) {
            dcsum += u16::from(*el);
        }
        let dc = ((dcsum + (1 << 2)) >> 3) as u8;
        for _ in 0..4 {
            let out = &mut dst[off..][..4];
            for el in out.iter_mut() {
                *el = dc;
            }
            off += stride;
        }
    }
    pub fn ipred_tm(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let tl = i16::from(ipred.tl);
        for m in 0..4 {
            for n in 0..4 {
                dst[off + n] = clip_u8(i16::from(ipred.left[m]) + i16::from(ipred.top[n]) - tl);
            }
            off += stride;
        }
    }
    pub fn ipred_ve(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let (v0, v1, v2, v3) = load_pred4!(top; ipred);
        let vert_pred = [v0, v1, v2, v3];
        for _ in 0..4 {
            let out = &mut dst[off..][..4];
            out.copy_from_slice(&vert_pred);
            off += stride;
        }
    }
    pub fn ipred_he(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let (p0, p1, p2, _) = load_pred4!(left; ipred);
        let p3 = ((u16::from(ipred.left[2]) + u16::from(ipred.left[3]) * 3 + 2) >> 2) as u8;
        let hor_pred = [p0, p1, p2, p3];
        for m in 0..4 {
            for n in 0..4 {
                dst[off + n] = hor_pred[m];
            }
            off += stride;
        }
    }
    pub fn ipred_ld(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let (_,  p0, p1, p2) = load_pred4!(top;  ipred);
        let (p3, p4, p5, p6) = load_pred4!(top8; ipred);

        dst[off + 0] = p0; dst[off + 1] = p1; dst[off + 2] = p2; dst[off + 3] = p3;
        off += stride;
        dst[off + 0] = p1; dst[off + 1] = p2; dst[off + 2] = p3; dst[off + 3] = p4;
        off += stride;
        dst[off + 0] = p2; dst[off + 1] = p3; dst[off + 2] = p4; dst[off + 3] = p5;
        off += stride;
        dst[off + 0] = p3; dst[off + 1] = p4; dst[off + 2] = p5; dst[off + 3] = p6;
    }
    pub fn ipred_rd(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let tl              = load_pred4!(topleft;  ipred);
        let (l0, l1, l2, _) = load_pred4!(left;     ipred);
        let (t0, t1, t2, _) = load_pred4!(top;      ipred);

        dst[off + 0] = tl; dst[off + 1] = t0; dst[off + 2] = t1; dst[off + 3] = t2;
        off += stride;
        dst[off + 0] = l0; dst[off + 1] = tl; dst[off + 2] = t0; dst[off + 3] = t1;
        off += stride;
        dst[off + 0] = l1; dst[off + 1] = l0; dst[off + 2] = tl; dst[off + 3] = t0;
        off += stride;
        dst[off + 0] = l2; dst[off + 1] = l1; dst[off + 2] = l0; dst[off + 3] = tl;
    }
    pub fn ipred_vr(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let tl               = load_pred4!(topleft; ipred);
        let (l0, l1, _,  _)  = load_pred4!(left;    ipred);
        let (t0, t1, t2, _)  = load_pred4!(top;     ipred);
        let (m0, m1, m2, m3) = load_pred4!(topavg;  ipred);

        dst[off + 0] = m0; dst[off + 1] = m1; dst[off + 2] = m2; dst[off + 3] = m3;
        off += stride;
        dst[off + 0] = tl; dst[off + 1] = t0; dst[off + 2] = t1; dst[off + 3] = t2;
        off += stride;
        dst[off + 0] = l0; dst[off + 1] = m0; dst[off + 2] = m1; dst[off + 3] = m2;
        off += stride;
        dst[off + 0] = l1; dst[off + 1] = tl; dst[off + 2] = t0; dst[off + 3] = t1;
    }
    pub fn ipred_vl(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let (_,  t1, t2, t3) = load_pred4!(top;     ipred);
        let (t4, t5, t6, _)  = load_pred4!(top8;    ipred);
        let (_,  m1, m2, m3) = load_pred4!(topavg;  ipred);
        let m4 = ((u16::from(ipred.top[3]) + u16::from(ipred.top[4]) + 1) >> 1) as u8;

        dst[off + 0] = m1; dst[off + 1] = m2; dst[off + 2] = m3; dst[off + 3] = m4;
        off += stride;
        dst[off + 0] = t1; dst[off + 1] = t2; dst[off + 2] = t3; dst[off + 3] = t4;
        off += stride;
        dst[off + 0] = m2; dst[off + 1] = m3; dst[off + 2] = m4; dst[off + 3] = t5;
        off += stride;
        dst[off + 0] = t2; dst[off + 1] = t3; dst[off + 2] = t4; dst[off + 3] = t6;
    }
    pub fn ipred_hd(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let tl               = load_pred4!(topleft; ipred);
        let (l0, l1, l2, _)  = load_pred4!(left;    ipred);
        let (m0, m1, m2, m3) = load_pred4!(leftavg; ipred);
        let (t0, t1, _,  _)  = load_pred4!(top;     ipred);

        dst[off + 0] = m0; dst[off + 1] = tl; dst[off + 2] = t0; dst[off + 3] = t1;
        off += stride;
        dst[off + 0] = m1; dst[off + 1] = l0; dst[off + 2] = m0; dst[off + 3] = tl;
        off += stride;
        dst[off + 0] = m2; dst[off + 1] = l1; dst[off + 2] = m1; dst[off + 3] = l0;
        off += stride;
        dst[off + 0] = m3; dst[off + 1] = l2; dst[off + 2] = m2; dst[off + 3] = l1;
    }
    pub fn ipred_hu(dst: &mut [u8], mut off: usize, stride: usize, ipred: &IPredContext) {
        let (_, m1, m2, m3) = load_pred4!(leftavg; ipred);
        let (_, l1, l2, _)  = load_pred4!(left;    ipred);
        let l3 = ((u16::from(ipred.left[2]) + u16::from(ipred.left[3]) * 3 + 2) >> 2) as u8;
        let p3 = ipred.left[3];

        dst[off + 0] = m1; dst[off + 1] = l1; dst[off + 2] = m2; dst[off + 3] = l2;
        off += stride;
        dst[off + 0] = m2; dst[off + 1] = l2; dst[off + 2] = m3; dst[off + 3] = l3;
        off += stride;
        dst[off + 0] = m3; dst[off + 1] = l3; dst[off + 2] = p3; dst[off + 3] = p3;
        off += stride;
        dst[off + 0] = p3; dst[off + 1] = p3; dst[off + 2] = p3; dst[off + 3] = p3;
    }
}

const VP7_BICUBIC_FILTERS: [[i16; 6]; 8] = [
    [ 0,   0, 128,   0,   0, 0 ],
    [ 0,  -6, 123,  12,  -1, 0 ],
    [ 2, -11, 108,  36,  -8, 1 ],
    [ 0,  -9,  93,  50,  -6, 0 ],
    [ 3, -16,  77,  77, -16, 3 ],
    [ 0,  -6,  50,  93,  -9, 0 ],
    [ 1,  -8,  36, 108, -11, 2 ],
    [ 0,  -1,  12, 123,  -6, 0 ]
];

macro_rules! interpolate {
    ($src: expr, $off: expr, $step: expr, $mode: expr) => {{
        let s0 = i32::from($src[$off + 0 * $step]);
        let s1 = i32::from($src[$off + 1 * $step]);
        let s2 = i32::from($src[$off + 2 * $step]);
        let s3 = i32::from($src[$off + 3 * $step]);
        let s4 = i32::from($src[$off + 4 * $step]);
        let s5 = i32::from($src[$off + 5 * $step]);
        let filt = &VP7_BICUBIC_FILTERS[$mode];
        let src = [s0, s1, s2, s3, s4, s5];
        let mut val = 64;
        for (s, c) in src.iter().zip(filt.iter()) {
            val += s * i32::from(*c);
        }
        clip_u8((val >> 7) as i16)
    }}
}

const EDGE_PRE: usize = 2;
const EDGE_POST: usize = 4;
const TMP_STRIDE: usize = 16;

fn mc_block_common(dst: &mut [u8], mut doff: usize, dstride: usize, src: &[u8], sstride: usize, size: usize, mx: usize, my: usize) {
    if (mx == 0) && (my == 0) {
        let dst = &mut dst[doff..];
        let src = &src[EDGE_PRE + EDGE_PRE * sstride..];
        for (out, src) in dst.chunks_mut(dstride).take(size).zip(src.chunks(sstride)) {
            out[..size].copy_from_slice(&src[..size]);
        }
    } else if my == 0 {
        let src = &src[EDGE_PRE * sstride..];
        for src in src.chunks(sstride).take(size) {
            for x in 0..size {
                dst[doff + x] = interpolate!(src, x, 1, mx);
            }
            doff += dstride;
        }
    } else if mx == 0 {
        let src = &src[EDGE_PRE..];
        for y in 0..size {
            for x in 0..size {
                dst[doff + x] = interpolate!(src, x + y * sstride, sstride, my);
            }
            doff += dstride;
        }
    } else {
        let mut tmp = [0u8; TMP_STRIDE * (16 + EDGE_PRE + EDGE_POST)];
        for (y, dst) in tmp.chunks_mut(TMP_STRIDE).take(size + EDGE_PRE + EDGE_POST).enumerate() {
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
    let bsize = (size as isize) + (EDGE_PRE as isize) + (EDGE_POST as isize);
    let ref_x = (xpos as isize) + ((mvx >> 3) as isize) - (EDGE_PRE as isize);
    let ref_y = (ypos as isize) + ((mvy >> 3) as isize) - (EDGE_PRE as isize);

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
pub fn mc_block16x16(dst: &mut [u8], doff: usize, dstride: usize, xpos: usize, ypos: usize,
                     mvx: i16, mvy: i16, src: NAVideoBufferRef<u8>, plane: usize, mc_buf: &mut [u8]) {
    mc_block(dst, doff, dstride, xpos, ypos, mvx, mvy, src, plane, mc_buf, 16);
}
pub fn mc_block8x8(dst: &mut [u8], doff: usize, dstride: usize, xpos: usize, ypos: usize,
                   mvx: i16, mvy: i16, src: NAVideoBufferRef<u8>, plane: usize, mc_buf: &mut [u8]) {
    mc_block(dst, doff, dstride, xpos, ypos, mvx, mvy, src, plane, mc_buf, 8);
}
pub fn mc_block4x4(dst: &mut [u8], doff: usize, dstride: usize, xpos: usize, ypos: usize,
                   mvx: i16, mvy: i16, src: NAVideoBufferRef<u8>, plane: usize, mc_buf: &mut [u8]) {
    mc_block(dst, doff, dstride, xpos, ypos, mvx, mvy, src, plane, mc_buf, 4);
}
pub fn mc_block_special(dst: &mut [u8], doff: usize, dstride: usize, xpos: usize, ypos: usize,
                        mvx: i16, mvy: i16, reffrm: NAVideoBufferRef<u8>, plane: usize,
                        mc_buf: &mut [u8], size: usize, pitch_mode: u8) {
    const Y_MUL: [isize; 8] = [ 1, 0, 2, 4, 1,  1, 2,  2 ];
    const Y_OFF: [isize; 8] = [ 0, 4, 0, 0, 1, -1, 1, -1 ];
    const ILACE_CHROMA: [bool; 8] = [ false, false, true, true, false, false, true, true ]; // mode&2 != 0

    let pitch_mode = (pitch_mode & 7) as usize;
    let (xstep, ymul) = if plane == 0 {
            (Y_OFF[pitch_mode], Y_MUL[pitch_mode])
        } else {
            (0, if ILACE_CHROMA[pitch_mode] { 2 } else { 1 })
        };

    let (w, h) = reffrm.get_dimensions(plane);
    let wa = if plane == 0 { (w + 15) & !15 } else { (w + 7) & !7 } as isize;
    let ha = if plane == 0 { (h + 15) & !15 } else { (h + 7) & !7 } as isize;
    let mut start_x = (xpos as isize) + ((mvx >> 3) as isize) - (EDGE_PRE as isize);
    let mut end_x   = (xpos as isize) + ((mvx >> 3) as isize) + ((size + EDGE_POST) as isize);
    if xstep < 0 {
        start_x -= (size + EDGE_POST) as isize;
    } else if xstep > 0 {
        end_x += (size as isize) * xstep;
    }
    let mut start_y = (ypos as isize) + ((mvy >> 3) as isize) - (EDGE_PRE as isize) * ymul;
    let mut end_y   = (ypos as isize) + ((mvy >> 3) as isize) + ((size + EDGE_POST) as isize) * ymul;
    if ymul == 0 {
        start_y -= EDGE_PRE as isize;
        end_y   += (EDGE_POST + 1) as isize;
    }
    let off     = reffrm.get_offset(plane);
    let stride  = reffrm.get_stride(plane);
    let (src, sstride) = if (start_x >= 0) && (end_x <= wa) && (start_y >= 0) && (end_y <= ha) {
            let data    = reffrm.get_data();
            (&data[off + (start_x as usize) + (start_y as usize) * stride..],
             ((stride as isize) + xstep) as usize)
        } else {
            let add = (size + EDGE_PRE + EDGE_POST) * xstep.unsigned_abs();
            let bw = size + EDGE_PRE + EDGE_POST + add;
            let bh = (end_y - start_y) as usize;
            let bo = if xstep >= 0 { 0 } else { add };
            edge_emu(&reffrm, start_x + (bo as isize), start_y, bw, bh, mc_buf, 128, plane, 0);
            (&mc_buf[bo..], (128 + xstep) as usize)
        };
    let mx = (mvx & 7) as usize;
    let my = (mvy & 7) as usize;
    match ymul {
        0 => unimplemented!(),
        1 => mc_block_common(dst, doff, dstride, src, sstride, size, mx, my),
        2 => {
            let hsize = size / 2;
            for y in 0..2 {
                for x in 0..2 {
                    mc_block_common(dst, doff + x * hsize + y * hsize * dstride, dstride,
                                    &src[x * hsize + y * sstride..], sstride * 2, hsize, mx, my);
                }
            }
        },
        4 => {
            let qsize = size / 4;
            for y in 0..4 {
                for x in 0..4 {
                    mc_block_common(dst, doff + x * qsize + y * qsize * dstride, dstride,
                                    &src[x * qsize + y * sstride..], sstride * 4, qsize, mx, my);
                }
            }
        },
        _ => unreachable!(),
    };
}
