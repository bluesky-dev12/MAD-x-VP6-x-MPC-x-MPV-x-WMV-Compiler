use frame::NAVideoBuffer;
use codecs::MV;
use codecs::blockdsp::edge_emu;
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
}

trait HFilt {
    const HMODE: usize;
    fn filter_h(src: &[u8], idx: usize) -> u8 {
        match Self::HMODE {
            1 => filter!(01; src, idx, 1),
            2 => filter!(02; src, idx, 1),
            3 => filter!(03; src, idx, 1),
            _ => src[idx],
        }
    }
}
trait VFilt {
    const VMODE: usize;
    fn filter_v(src: &[u8], idx: usize, stride: usize) -> u8 {
        match Self::VMODE {
            1 => filter!(01; src, idx, stride),
            2 => filter!(02; src, idx, stride),
            3 => filter!(03; src, idx, stride),
            _ => src[idx],
        }
    }
}
trait MC: HFilt+VFilt {
    const SIZE: usize;
    fn mc(dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize) {
        if (Self::HMODE != 0) && (Self::VMODE != 0) {
            let mut buf: [u8; (16 + 5) * 16] = [0; (16 + 5) * 16];
            let mut bidx = 0;
            let bstride = Self::SIZE;
            sidx -= sstride * 2;
            for _ in 0..Self::SIZE+5 {
                for x in 0..Self::SIZE { buf[bidx + x] = Self::filter_h(src, sidx + x); }
                bidx += bstride;
                sidx += sstride;
            }
            bidx = bstride * 2;
            for _ in 0..Self::SIZE {
                for x in 0..Self::SIZE { dst[didx + x] = Self::filter_v(&buf, bidx + x, bstride); }
                didx += dstride;
                bidx += bstride;
            }
        } else if Self::HMODE != 0 {
            for _ in 0..Self::SIZE {
                for x in 0..Self::SIZE {
                    dst[didx + x] = Self::filter_h(src, sidx + x);
                }
                didx += dstride;
                sidx += sstride;
            }
        } else if Self::VMODE != 0 {
            for _ in 0..Self::SIZE {
                for x in 0..Self::SIZE {
                    dst[didx + x] = Self::filter_v(src, sidx + x, sstride);
                }
                didx += dstride;
                sidx += sstride;
            }
        } else {
            for _ in 0..Self::SIZE {
                for x in 0..Self::SIZE {
                    dst[didx + x] = src[sidx + x];
                }
                didx += dstride;
                sidx += sstride;
            }
        }
    }
}

macro_rules! mc {
    ($name: ident, $size: expr, $vf: expr, $hf: expr) => {
        struct $name;
        impl HFilt for $name { const HMODE: usize = $hf; }
        impl VFilt for $name { const VMODE: usize = $vf; }
        impl MC for $name { const SIZE: usize = $size; }
    };
}

mc!(MC00_16, 16, 0, 0);
mc!(MC01_16, 16, 0, 1);
mc!(MC02_16, 16, 0, 2);
mc!(MC03_16, 16, 0, 3);
mc!(MC10_16, 16, 1, 0);
mc!(MC11_16, 16, 1, 1);
mc!(MC12_16, 16, 1, 2);
mc!(MC13_16, 16, 1, 3);
mc!(MC20_16, 16, 2, 0);
mc!(MC21_16, 16, 2, 1);
mc!(MC22_16, 16, 2, 2);
mc!(MC23_16, 16, 2, 3);
mc!(MC30_16, 16, 3, 1);
mc!(MC31_16, 16, 3, 1);
mc!(MC32_16, 16, 3, 2);
mc!(MC33_16, 16, 3, 3);

mc!(MC00_8, 8, 0, 0);
mc!(MC01_8, 8, 0, 1);
mc!(MC02_8, 8, 0, 2);
mc!(MC03_8, 8, 0, 3);
mc!(MC10_8, 8, 1, 0);
mc!(MC11_8, 8, 1, 1);
mc!(MC12_8, 8, 1, 2);
mc!(MC13_8, 8, 1, 3);
mc!(MC20_8, 8, 2, 0);
mc!(MC21_8, 8, 2, 1);
mc!(MC22_8, 8, 2, 2);
mc!(MC23_8, 8, 2, 3);
mc!(MC30_8, 8, 3, 1);
mc!(MC31_8, 8, 3, 1);
mc!(MC32_8, 8, 3, 2);
mc!(MC33_8, 8, 3, 3);


const RV40_CHROMA_BIAS: [[u16; 4]; 4] = [
    [ 0, 4, 8, 4 ],
    [ 8, 7, 8, 7 ],
    [ 0, 8, 4, 8 ],
    [ 8, 7, 8, 7 ]
];

fn rv40_chroma_mc(dst: &mut [u8], mut didx: usize, dstride: usize, src: &[u8], mut sidx: usize, sstride: usize, size: usize, x: usize, y: usize) {
    if (x == 0) && (y == 0) {
        for _ in 0..size {
            for x in 0..size { dst[didx + x] = src[sidx + x]; }
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

pub struct RV40DSP {
    luma_mc: [[fn (&mut [u8], usize, usize, &[u8], usize, usize); 16]; 2],
}

impl RV40DSP {
    pub fn new() -> Self {
        RV40DSP {
            luma_mc: [
                    [ MC00_16::mc, MC01_16::mc, MC02_16::mc, MC03_16::mc,
                      MC10_16::mc, MC11_16::mc, MC12_16::mc, MC13_16::mc,
                      MC20_16::mc, MC21_16::mc, MC22_16::mc, MC23_16::mc,
                      MC30_16::mc, MC31_16::mc, MC32_16::mc, MC33_16::mc ],
                    [ MC00_8::mc, MC01_8::mc, MC02_8::mc, MC03_8::mc,
                      MC10_8::mc, MC11_8::mc, MC12_8::mc, MC13_8::mc,
                      MC20_8::mc, MC21_8::mc, MC22_8::mc, MC23_8::mc,
                      MC30_8::mc, MC31_8::mc, MC32_8::mc, MC33_8::mc ] ],
        }
    }
}

fn check_pos(x: usize, y: usize, size: usize, w: usize, h: usize, dx: i16, dy: i16, e0: isize, e1: isize, e2: isize, e3: isize) -> bool {
    let xn = (x as isize) + (dx as isize);
    let yn = (y as isize) + (dy as isize);

    (xn - e0 >= 0) && (xn + (size as isize) + e1 <= (w as isize)) && (yn - e2 >= 0) && (yn + (size as isize) + e3 <= (h as isize))
}

const RV40_EDGE1: [isize; 4] = [ 0, 2, 2, 2 ];
const RV40_EDGE2: [isize; 4] = [ 0, 3, 3, 3 ];

impl RV34DSP for RV40DSP {
    fn loop_filter(&self, _frame: &mut NAVideoBuffer<u8>, _mbinfo: &[RV34MBInfo], _mb_w: usize, _mb_h: usize) {
    }
    fn do_luma_mc(&self, frame: &mut NAVideoBuffer<u8>, prev_frame: &NAVideoBuffer<u8>, x: usize, y: usize, mv: MV, use16: bool, avg: bool) {
        let size: usize = if use16 { 16 } else { 8 };
        let dstride = frame.get_stride(0);
        let doffset = frame.get_offset(0) + (if !avg { x + y * dstride } else { 0 });
        let mut data = frame.get_data_mut();
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
            edge_emu(prev_frame, (x as isize) + (dx as isize) - 2, (y as isize) + (dy as isize) - 2, 16+5, 16+5, &mut ebuf, 32, 0);
            self.luma_mc[if use16 { 0 } else { 1 }][mode](dst, doffset, dstride, &ebuf, 32*2 + 2, 32);
        }
    }
    fn do_chroma_mc(&self, frame: &mut NAVideoBuffer<u8>, prev_frame: &NAVideoBuffer<u8>, x: usize, y: usize, comp: usize, mv: MV, use8: bool, avg: bool) {
        let size: usize = if use8 { 8 } else { 4 };
        let dstride = frame.get_stride(comp);
        let doffset = frame.get_offset(comp) + (if !avg { x + y * dstride } else { 0 });
        let mut data = frame.get_data_mut();
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

        if check_pos(x, y, size, w, h, dx, dy, 0, 0, 1, 1) {
            let sstride = prev_frame.get_stride(comp);
            let mut soffset = prev_frame.get_offset(comp) + x + y * sstride;
            let data = prev_frame.get_data();
            let src: &[u8] = data.as_slice();
            soffset = ((soffset as isize) + (dx as isize) + (dy as isize) * (sstride as isize)) as usize;
            rv40_chroma_mc(dst, doffset, dstride, src, soffset, sstride, size, cx, cy);
        } else {
            let mut ebuf: [u8; 16*10] = [0; 16*10];
            edge_emu(prev_frame, (x as isize) + (dx as isize), (y as isize) + (dy as isize), 8+1, 8+1, &mut ebuf, 16, comp);
            rv40_chroma_mc(dst, doffset, dstride, &ebuf, 0, 16, size, cx, cy);
        }
    }
}
