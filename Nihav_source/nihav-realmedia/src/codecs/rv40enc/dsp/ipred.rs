use super::super::types::{PredType8x8, PredType4x4};
use super::RefMBData;

#[derive(Default)]
pub struct IntraPred16x16 {
    pub top:    [u8; 17],
    pub left:   [u8; 17],
}

impl IntraPred16x16 {
    pub fn new() -> Self { Self::default() }
    #[allow(clippy::many_single_char_names)]
    pub fn apply16(&self, mode: PredType8x8, dst: &mut [u8], stride: usize) {
        match mode {
            PredType8x8::DC => {
                let sumt = self.top[1..].iter().fold(0u32, |acc, &x| acc + u32::from(x));
                let suml = self.left[1..].iter().fold(0u32, |acc, &x| acc + u32::from(x));
                let dc = ((sumt + suml + 16) >> 5) as u8;
                for line in dst.chunks_mut(stride).take(16) {
                    for dst in line[..16].iter_mut() {
                        *dst = dc;
                    }
                }
            },
            PredType8x8::Hor => {
                for (&left, line) in self.left[1..].iter().zip(dst.chunks_mut(stride)) {
                    for dst in line[..16].iter_mut() {
                        *dst = left;
                    }
                }
            },
            PredType8x8::Ver => {
                for line in dst.chunks_mut(stride).take(16) {
                    line[..16].copy_from_slice(&self.top[1..]);
                }
            },
            PredType8x8::Plane => {
                let top0 = &self.top[9..];
                let top1 = &self.top[..8];
                let h = top0.iter().zip(top1.iter().rev()).enumerate().fold(
                            0i32, |acc, (k, (&a, &b))| acc + ((k + 1) as i32) * (i32::from(a) - i32::from(b)));
                let left0 = &self.left[9..];
                let left1 = &self.left[..8];
                let v = left0.iter().zip(left1.iter().rev()).enumerate().fold(
                            0i32, |acc, (k, (&a, &b))| acc + ((k + 1) as i32) * (i32::from(a) - i32::from(b)));
                let b = (h + (h >> 2)) >> 4;
                let c = (v + (v >> 2)) >> 4;
                let mut a = 16 * (i32::from(self.left[16]) + i32::from(self.top[16])) + 16 - 7 * (b + c);

                for line in dst.chunks_mut(stride).take(16) {
                    let mut oval = a;
                    for el in line[..16].iter_mut() {
                        *el = (oval >> 5).max(0).min(255) as u8;
                        oval += b;
                    }
                    a += c;
                }
            },
            PredType8x8::LeftDC => {
                let dc = ((self.left[1..].iter().fold(0u32, |acc, &x| acc + u32::from(x)) + 8) >> 4) as u8;
                for line in dst.chunks_mut(stride).take(16) {
                    for dst in line[..16].iter_mut() {
                        *dst = dc;
                    }
                }
            },
            PredType8x8::TopDC => {
                let dc = ((self.top[1..].iter().fold(0u32, |acc, &x| acc + u32::from(x)) + 8) >> 4) as u8;
                for line in dst.chunks_mut(stride).take(16) {
                    for dst in line[..16].iter_mut() {
                        *dst = dc;
                    }
                }
            },
            PredType8x8::DC128 => {
                for line in dst.chunks_mut(stride).take(16) {
                    for dst in line[..16].iter_mut() {
                        *dst = 128;
                    }
                }
            },
        }
    }
    pub fn apply8(&self, mode: PredType8x8, dst: &mut [u8], stride: usize) {
        match mode {
            PredType8x8::DC | PredType8x8::Plane => {
                let sumt = self.top[1..9].iter().fold(0u32, |acc, &x| acc + u32::from(x));
                let suml = self.left[1..9].iter().fold(0u32, |acc, &x| acc + u32::from(x));
                let dc = ((sumt + suml + 8) >> 4) as u8;
                for line in dst.chunks_mut(stride).take(8) {
                    for dst in line[..8].iter_mut() {
                        *dst = dc;
                    }
                }
            },
            PredType8x8::Hor => {
                for (&left, line) in self.left[1..9].iter().zip(dst.chunks_mut(stride)) {
                    for dst in line[..8].iter_mut() {
                        *dst = left;
                    }
                }
            },
            PredType8x8::Ver => {
                for line in dst.chunks_mut(stride).take(8) {
                    line[..8].copy_from_slice(&self.top[1..9]);
                }
            },
            PredType8x8::LeftDC => {
                let dc = ((self.left[1..9].iter().fold(0u32, |acc, &x| acc + u32::from(x)) + 4) >> 3) as u8;
                for line in dst.chunks_mut(stride).take(8) {
                    for dst in line[..8].iter_mut() {
                        *dst = dc;
                    }
                }
            },
            PredType8x8::TopDC => {
                let dc = ((self.top[1..9].iter().fold(0u32, |acc, &x| acc + u32::from(x)) + 4) >> 3) as u8;
                for line in dst.chunks_mut(stride).take(8) {
                    for dst in line[..8].iter_mut() {
                        *dst = dc;
                    }
                }
            },
            PredType8x8::DC128 => {
                for line in dst.chunks_mut(stride).take(8) {
                    for dst in line[..8].iter_mut() {
                        *dst = 128;
                    }
                }
            },
        }
    }
}

#[derive(Default)]
pub struct Intra4Pred {
    pub top:  [u8; 8],
    pub left: [u8; 8],
    pub tl:   u8,
}

impl Intra4Pred {
    pub fn new() -> Self { Self::default() }
    fn load_left(&self) -> [u16; 8] {
        let mut ret = [0; 8];
        for (dst, &src) in ret.iter_mut().zip(self.left.iter()) {
            *dst = u16::from(src);
        }
        ret
    }
    fn load_top(&self) -> [u16; 8] {
        let mut ret = [0; 8];
        for (dst, &src) in ret.iter_mut().zip(self.top.iter()) {
            *dst = u16::from(src);
        }
        ret
    }
    fn load_left_and_top(&self) -> ([u16; 5], [u16; 5]) {
        let mut left = [0; 5];
        let mut top = [0; 5];
        left[0] = u16::from(self.tl);
        top[0] = u16::from(self.tl);
        for (dst, &src) in left[1..].iter_mut().zip(self.left.iter()) {
            *dst = u16::from(src);
        }
        for (dst, &src) in top[1..].iter_mut().zip(self.top.iter()) {
            *dst = u16::from(src);
        }
        (left, top)
    }
    #[allow(clippy::many_single_char_names)]
    pub fn apply(&self, ptype: PredType4x4, buf: &mut [u8], stride: usize) {
        match ptype {
            PredType4x4::DC => {
                let dc_l = self.left[..4].iter().fold(0u32, |acc, &x| acc + u32::from(x));
                let dc_t = self.top [..4].iter().fold(0u32, |acc, &x| acc + u32::from(x));
                let dc = ((dc_t + dc_l + 4) >> 3) as u8;
                for line in buf.chunks_mut(stride).take(4) {
                    for el in line[..4].iter_mut() {
                        *el = dc;
                    }
                }
            },
            PredType4x4::LeftDC => {
                let dc_l = self.left[..4].iter().fold(0u32, |acc, &x| acc + u32::from(x));
                let dc = ((dc_l + 2) >> 2) as u8;
                for line in buf.chunks_mut(stride).take(4) {
                    for el in line[..4].iter_mut() {
                        *el = dc;
                    }
                }
            },
            PredType4x4::TopDC => {
                let dc_t = self.top [..4].iter().fold(0u32, |acc, &x| acc + u32::from(x));
                let dc = ((dc_t + 2) >> 2) as u8;
                for line in buf.chunks_mut(stride).take(4) {
                    for el in line[..4].iter_mut() {
                        *el = dc;
                    }
                }
            },
            PredType4x4::DC128 => {
                for line in buf.chunks_mut(stride).take(4) {
                    for el in line[..4].iter_mut() {
                        *el = 128;
                    }
                }
            },
            PredType4x4::Ver => {
                for line in buf.chunks_mut(stride).take(4) {
                    line[..4].copy_from_slice(&self.top[..4]);
                }
            },
            PredType4x4::Hor => {
                for (&left, line) in self.left[..4].iter().zip(buf.chunks_mut(stride)) {
                    for dst in line[..4].iter_mut() {
                        *dst = left;
                    }
                }
            },
            PredType4x4::DiagDownLeft => {
                let l = self.load_left();
                let t = self.load_top();
                buf[0] = ((t[0] + t[2] + 2*t[1] + 2 + l[0] + l[2] + 2*l[1] + 2) >> 3) as u8;
                let pix = ((t[1] + t[3] + 2*t[2] + 2 + l[1] + l[3] + 2*l[2] + 2) >> 3) as u8;
                buf[1]      = pix;
                buf[stride] = pix;
                let pix = ((t[2] + t[4] + 2*t[3] + 2 + l[2] + l[4] + 2*l[3] + 2) >> 3) as u8;
                buf[2]          = pix;
                buf[1 + stride] = pix;
                buf[2 * stride] = pix;
                let pix = ((t[3] + t[5] + 2*t[4] + 2 + l[3] + l[5] + 2*l[4] + 2) >> 3) as u8;
                buf[3]              = pix;
                buf[2 +     stride] = pix;
                buf[1 + 2 * stride] = pix;
                buf[    3 * stride] = pix;
                let pix = ((t[4] + t[6] + 2*t[5] + 2 + l[4] + l[6] + 2*l[5] + 2) >> 3) as u8;
                buf[3 +     stride] = pix;
                buf[2 + 2 * stride] = pix;
                buf[1 + 3 * stride] = pix;
                let pix = ((t[5] + t[7] + 2*t[6] + 2 + l[5] + l[7] + 2*l[6] + 2) >> 3) as u8;
                buf[3 + 2 * stride] = pix;
                buf[2 + 3 * stride] = pix;
                buf[3 + 3 * stride] = ((t[6] + t[7] + 1 + l[6] + l[7] + 1) >> 2) as u8;
            },
            PredType4x4::DiagDownRight => {
                let (l, t) = self.load_left_and_top();
                for (j, line) in buf.chunks_mut(stride).take(4).enumerate() {
                    for i in 0..j {
                        line[i] = ((l[j - i - 1] + 2 * l[j - i] + l[j - i + 1] + 2) >> 2) as u8;
                    }
                    line[j] = ((l[1] + 2 * l[0] + t[1] + 2) >> 2) as u8;
                    for i in (j + 1)..4 {
                        line[i] = ((t[i - j - 1] + 2 * t[i - j] + t[i - j + 1] + 2) >> 2) as u8;
                    }
                }
            },
            PredType4x4::VerRight => {
                let (l, t) = self.load_left_and_top();
                for (j, line) in buf.chunks_mut(stride).take(4).enumerate() {
                    for (i, pix) in line[..4].iter_mut().enumerate() {
                        let zvr = ((2 * i) as i8) - (j as i8);
                        *pix = if zvr >= 0 {
                                if (zvr & 1) == 0 {
                                    (t[i - (j >> 1)] + t[i - (j >> 1) + 1] + 1) >> 1
                                } else {
                                    (t[i - (j >> 1) - 1] + 2 * t[i - (j >> 1)] + t[i - (j >> 1) + 1] + 2) >> 2
                                }
                            } else {
                                if zvr == -1 {
                                    (l[1] + 2 * l[0] + t[1] + 2) >> 2
                                } else {
                                    (l[j] + 2 * l[j - 1] + l[j - 2] + 2) >> 2
                                }
                            } as u8;
                    }
                }
            },
            PredType4x4::HorDown => {
                let (l, t) = self.load_left_and_top();
                for (j, line) in buf.chunks_mut(stride).take(4).enumerate() {
                    for (i, pix) in line[..4].iter_mut().enumerate() {
                        let zhd = ((2 * j) as i8) - (i as i8);
                        *pix = if zhd >= 0 {
                                if (zhd & 1) == 0 {
                                    (l[j - (i >> 1)] + l[j - (i >> 1) + 1] + 1) >> 1
                                } else {
                                    (l[j - (i >> 1) - 1] + 2 * l[j - (i >> 1)] + l[j - (i >> 1) + 1] + 2) >> 2
                                }
                            } else {
                                if zhd == -1 {
                                    (l[1] + 2 * l[0] + t[1] + 2) >> 2
                                } else {
                                    (t[i - 2] + 2 * t[i - 1] + t[i] + 2) >> 2
                                }
                            } as u8;
                    }
                }
            },
            PredType4x4::VerLeft => {
                let l = self.load_left();
                let t = self.load_top();
                buf[0] = ((2*t[0] + 2*t[1] + l[1] + 2*l[2] + l[3] + 4) >> 3) as u8;
                let pix = ((t[1] + t[2] + 1) >> 1) as u8;
                buf[1]          = pix;
                buf[2 * stride] = pix;
                let pix = ((t[2] + t[3] + 1) >> 1) as u8;
                buf[2]              = pix;
                buf[1 + 2 * stride] = pix;
                let pix = ((t[3] + t[4] + 1) >> 1) as u8;
                buf[3]              = pix;
                buf[2 + 2 * stride] = pix;
                buf[3 + 2 * stride] = ((t[4] + t[5] + 1) >> 1) as u8;
                buf[        stride] = ((t[0] + 2*t[1] + t[2] + l[2] + 2*l[3] + l[4] + 4) >> 3) as u8;
                let pix = ((t[1] + 2*t[2] + t[3] + 2) >> 2) as u8;
                buf[1 +     stride] = pix;
                buf[    3 * stride] = pix;
                let pix = ((t[2] + 2*t[3] + t[4] + 2) >> 2) as u8;
                buf[2 +     stride] = pix;
                buf[1 + 3 * stride] = pix;
                let pix = ((t[3] + 2*t[4] + t[5] + 2) >> 2) as u8;
                buf[3 +     stride] = pix;
                buf[2 + 3 * stride] = pix;
                buf[3 + 3 * stride] = ((t[4] + 2*t[5] + t[6] + 2) >> 2) as u8;
            },
            PredType4x4::HorUp => {
                let l = self.load_left();
                let t = self.load_top();
                buf[0] = ((t[1] + 2*t[2] + t[3] + 2*l[0] + 2*l[1] + 4) >> 3) as u8;
                buf[1] = ((t[2] + 2*t[3] + t[4] + l[0] + 2*l[1] + l[2] + 4) >> 3) as u8;
                let pix = ((t[3] + 2*t[4] + t[5] + 2*l[1] + 2*l[2] + 4) >> 3) as u8;
                buf[2]              = pix;
                buf[        stride] = pix;
                let pix = ((t[4] + 2*t[5] + t[6] + l[1] + 2*l[2] + l[3] + 4) >> 3) as u8;
                buf[3]              = pix;
                buf[1 +     stride] = pix;
                let pix = ((t[5] + 2*t[6] + t[7] + 2*l[2] + 2*l[3] + 4) >> 3) as u8;
                buf[2 +     stride] = pix;
                buf[0 + 2 * stride] = pix;
                let pix = ((t[6] + 3*t[7] + l[2] + 3*l[3] + 4) >> 3) as u8;
                buf[3 +     stride] = pix;
                buf[1 + 2 * stride] = pix;
                let pix = ((l[3] + 2*l[4] + l[5] + 2) >> 2) as u8;
                buf[3 + 2 * stride] = pix;
                buf[1 + 3 * stride] = pix;
                let pix = ((t[6] + t[7] + l[3] + l[4] + 2) >> 2) as u8;
                buf[0 + 3 * stride] = pix;
                buf[2 + 2 * stride] = pix;
                buf[2 + 3 * stride] = ((l[4] + l[5] + 1) >> 1) as u8;
                buf[3 + 3 * stride] = ((l[4] + 2*l[5] + l[6] + 2) >> 2) as u8;
            },
            PredType4x4::DiagDownLeftNoDown => {
                let l = self.load_left();
                let t = self.load_top();
                buf[0] = ((t[0] + t[2] + 2*t[1] + 2 + l[0] + l[2] + 2*l[1] + 2) >> 3) as u8;
                let pix = ((t[1] + t[3] + 2*t[2] + 2 + l[1] + l[3] + 2*l[2] + 2) >> 3) as u8;
                buf[1]              = pix;
                buf[0 +     stride] = pix;
                let pix = ((t[2] + t[4] + 2*t[3] + 2 + l[2] + 3*l[3] + 2) >> 3) as u8;
                buf[2]              = pix;
                buf[1 +     stride] = pix;
                buf[0 + 2 * stride] = pix;
                let pix = ((t[3] + t[5] + 2*t[4] + 2 + l[3]*4 + 2) >> 3) as u8;
                buf[3]              = pix;
                buf[2 +     stride] = pix;
                buf[1 + 2 * stride] = pix;
                buf[0 + 3 * stride] = pix;
                let pix = ((t[4] + t[6] + 2*t[5] + 2 + l[3]*4 + 2) >> 3) as u8;
                buf[3 +     stride] = pix;
                buf[2 + 2 * stride] = pix;
                buf[1 + 3 * stride] = pix;
                let pix = ((t[5] + t[7] + 2*t[6] + 2 + l[3]*4 + 2) >> 3) as u8;
                buf[3 + 2 * stride] = pix;
                buf[2 + 3 * stride] = pix;
                buf[3 + 3 * stride] = ((t[6] + t[7] + 1 + 2*l[3] + 1) >> 2) as u8;
            },
            PredType4x4::HorUpNoDown => {
                let l = self.load_left();
                let t = self.load_top();
                buf[0] = ((t[1] + 2*t[2] + t[3] + 2*l[0] + 2*l[1] + 4) >> 3) as u8;
                buf[1] = ((t[2] + 2*t[3] + t[4] + l[0] + 2*l[1] + l[2] + 4) >> 3) as u8;
                let pix = ((t[3] + 2*t[4] + t[5] + 2*l[1] + 2*l[2] + 4) >> 3) as u8;
                buf[2]              = pix;
                buf[        stride] = pix;
                let pix = ((t[4] + 2*t[5] + t[6] + l[1] + 2*l[2] + l[3] + 4) >> 3) as u8;
                buf[3]              = pix;
                buf[1 +     stride] = pix;
                let pix = ((t[5] + 2*t[6] + t[7] + 2*l[2] + 2*l[3] + 4) >> 3) as u8;
                buf[2 +     stride] = pix;
                buf[    2 * stride] = pix;
                let pix = ((t[6] + 3*t[7] + l[2] + 3*l[3] + 4) >> 3) as u8;
                buf[3 +     stride] = pix;
                buf[1 + 2 * stride] = pix;
                buf[3 + 2 * stride] = l[3] as u8;
                buf[1 + 3 * stride] = l[3] as u8;
                let pix = ((t[6] + t[7] + 2*l[3] + 2) >> 2) as u8;
                buf[0 + 3 * stride] = pix;
                buf[2 + 2 * stride] = pix;
                buf[2 + 3 * stride] = l[3] as u8;
                buf[3 + 3 * stride] = l[3] as u8;
            },
            PredType4x4::VerLeftNoDown => {
                let l = [u16::from(self.left[0]), u16::from(self.left[1]), u16::from(self.left[2]), u16::from(self.left[3]), u16::from(self.left[3])];
                let t = self.load_top();
                buf[0] = ((2*t[0] + 2*t[1] + l[1] + 2*l[2] + l[3] + 4) >> 3) as u8;
                let pix = ((t[1] + t[2] + 1) >> 1) as u8;
                buf[1]              = pix;
                buf[    2 * stride] = pix;
                let pix = ((t[2] + t[3] + 1) >> 1) as u8;
                buf[2]              = pix;
                buf[1 + 2 * stride] = pix;
                let pix = ((t[3] + t[4] + 1) >> 1) as u8;
                buf[3]              = pix;
                buf[2 + 2 * stride] = pix;
                buf[3 + 2 * stride] = ((t[4] + t[5] + 1) >> 1) as u8;
                buf[        stride] = ((t[0] + 2*t[1] + t[2] + l[2] + 2*l[3] + l[4] + 4) >> 3) as u8;
                let pix = ((t[1] + 2*t[2] + t[3] + 2) >> 2) as u8;
                buf[1 +     stride] = pix;
                buf[    3 * stride] = pix;
                let pix = ((t[2] + 2*t[3] + t[4] + 2) >> 2) as u8;
                buf[2 +     stride] = pix;
                buf[1 + 3 * stride] = pix;
                let pix = ((t[3] + 2*t[4] + t[5] + 2) >> 2) as u8;
                buf[3 +     stride] = pix;
                buf[2 + 3 * stride] = pix;
                buf[3 + 3 * stride] = ((t[4] + 2*t[5] + t[6] + 2) >> 2) as u8;
            },
        }
    }
}

pub struct BlockIntra4Pred {
    pub ipred_y:    Intra4Pred,
    pub ipred_u:    Intra4Pred,
    pub ipred_v:    Intra4Pred,
    pub top_y:      [u8; 21],
    pub top_u:      [u8; 13],
    pub top_v:      [u8; 13],
    pub left_y:     [u8; 16],
    pub left_u:     [u8; 8],
    pub left_v:     [u8; 8],
    pub has_l:      bool,
}

impl BlockIntra4Pred {
    pub fn new(src_y: &IntraPred16x16, src_u: &IntraPred16x16, src_v: &IntraPred16x16, tr_y: [u8; 4], tr_u: [u8; 4], tr_v: [u8; 4], has_l: bool) -> Self {
        let mut top_y = [0; 21];
        top_y[..17].copy_from_slice(&src_y.top);
        top_y[17..].copy_from_slice(&tr_y);
        let mut top_u = [0; 13];
        top_u[..9].copy_from_slice(&src_u.top[..9]);
        top_u[9..].copy_from_slice(&tr_u);
        let mut top_v = [0; 13];
        top_v[..9].copy_from_slice(&src_v.top[..9]);
        top_v[9..].copy_from_slice(&tr_v);
        let mut left_y = [0; 16];
        left_y.copy_from_slice(&src_y.left[1..]);
        let mut left_u = [0; 8];
        left_u.copy_from_slice(&src_u.left[1..9]);
        let mut left_v = [0; 8];
        left_v.copy_from_slice(&src_v.left[1..9]);
        Self {
            ipred_y:    Intra4Pred::new(),
            ipred_u:    Intra4Pred::new(),
            ipred_v:    Intra4Pred::new(),
            top_y, top_u, top_v, left_y, left_u, left_v,
            has_l,
        }
    }
    pub fn pred_block(&mut self, dst: &mut RefMBData, x: usize, y: usize, mode: PredType4x4) {
        let do_chroma = ((x & 1) == 0) && ((y & 1) == 0);
        if x == 0 {
            self.ipred_y.tl = if y == 0 { self.top_y[0] } else { self.left_y[y * 4 - 1] };
            if y != 3 {
                self.ipred_y.left.copy_from_slice(&self.left_y[y * 4..][..8]);
            } else {
                self.ipred_y.left[..4].copy_from_slice(&self.left_y[12..]);
            }
            if y == 0 {
                self.ipred_u.tl = self.top_u[0];
                self.ipred_v.tl = self.top_v[0];
                self.ipred_u.left.copy_from_slice(&self.left_u);
                self.ipred_v.left.copy_from_slice(&self.left_v);
            } else if y == 2 {
                self.ipred_u.tl = self.left_u[3];
                self.ipred_v.tl = self.left_v[3];
                self.ipred_u.left[..4].copy_from_slice(&self.left_u[4..]);
                self.ipred_v.left[..4].copy_from_slice(&self.left_v[4..]);
            }
        }
        self.ipred_y.top.copy_from_slice(&self.top_y[x * 4 + 1..][..8]);
        if do_chroma {
            if x == 0 {
                self.ipred_u.top.copy_from_slice(&self.top_u[1..9]);
                self.ipred_v.top.copy_from_slice(&self.top_v[1..9]);
            } else if x == 2 {
                self.ipred_u.top.copy_from_slice(&self.top_u[5..]);
                self.ipred_v.top.copy_from_slice(&self.top_v[5..]);
            }
        }

        self.ipred_y.apply(mode, &mut dst.y[x * 4 + y * 4 * 16..], 16);
        if do_chroma {
            let has_ld = if (x == 0) && (y == 0) { self.has_l } else { false };
            let off = x * 2 + y * 2 * 8;
            let cmode = match mode {
                    PredType4x4::DiagDownLeft if !has_ld => PredType4x4::DiagDownLeftNoDown,
                    PredType4x4::VerLeft if !has_ld => PredType4x4::VerLeftNoDown,
                    PredType4x4::HorUp if !has_ld => PredType4x4::HorUpNoDown,
                    _ => mode,
                };
            self.ipred_u.apply(cmode, &mut dst.u[off..], 8);
            self.ipred_v.apply(cmode, &mut dst.v[off..], 8);
        }
    }
    pub fn update_from(&mut self, src: &RefMBData, x: usize, y: usize) {
        let do_chroma = ((x & 1) == 0) && ((y & 1) == 0);
        let y_off = x * 4 + y * 4 * 16;
        let c_off = x * 2 + y * 2 * 8;

        if x != 3 {
            self.ipred_y.tl = self.ipred_y.top[3];
            for (left, src) in self.ipred_y.left[..4].iter_mut().zip(src.y[y_off + 3..].chunks(16)) {
                *left = src[0];
            }
        }
        if do_chroma && x != 2 {
            self.ipred_u.tl = self.ipred_u.top[3];
            self.ipred_v.tl = self.ipred_v.top[3];
            for (left, src) in self.ipred_u.left[..4].iter_mut().zip(src.u[c_off + 3..].chunks(8)) {
                *left = src[0];
            }
            for (left, src) in self.ipred_v.left[..4].iter_mut().zip(src.v[c_off + 3..].chunks(8)) {
                *left = src[0];
            }
        }
        if x == 0 {
            self.top_y[0] = self.left_y[x * 4 + 3];
            if y == 0 {
                self.top_u[0] = self.left_u[3];
                self.top_v[0] = self.left_v[3];
            }
        }
        self.top_y[x * 4 + 1..][..4].copy_from_slice(&src.y[y_off + 3 * 16..][..4]);
        if x == 3 {
            let (head, tail) = self.top_y.split_at_mut(17);
            for el in tail.iter_mut() {
                *el = head[16];
            }
        }
        if do_chroma && y != 2 {
            self.top_u[x * 2 + 1..][..4].copy_from_slice(&src.u[c_off + 3 * 8..][..4]);
            self.top_v[x * 2 + 1..][..4].copy_from_slice(&src.v[c_off + 3 * 8..][..4]);
            if x == 2 {
                for i in 9..13 {
                    self.top_u[i] = self.top_u[8];
                    self.top_v[i] = self.top_v[8];
                }
            }
        }
    }
}
