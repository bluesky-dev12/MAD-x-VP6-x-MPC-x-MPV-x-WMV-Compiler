use super::super::types::Block;
use super::clip8;

pub trait BlockOps {
    fn from_diff(&mut self, new: &[u8], old: &[u8], stride: usize);
    fn add_to(&self, dst: &mut [u8], stride: usize);
    fn quant_dcs(&mut self, q_dc: usize, q_ac: usize);
    fn quant(&mut self, q_dc: usize, q_ac: usize);
    fn dequant_dcs(&mut self, q_dc: usize, q_ac: usize);
    fn dequant(&mut self, q_dc: usize, q_ac: usize);
    fn transform_4x4(&mut self);
    fn transform_dcs(&mut self);
    fn itransform_4x4(&mut self);
    fn itransform_dcs(&mut self);
}

macro_rules! tx {
    ($a:expr, $b:expr, $c:expr, $d:expr, $o0:expr, $o1:expr, $o2:expr, $o3:expr) => {
        let t0 = $a + $d;
        let t1 = $a - $d;
        let t2 = $b + $c;
        let t3 = $b - $c;
        $o0 = 13 * (t0 + t2);
        $o2 = 13 * (t0 - t2);
        $o1 = 17 * t1 +  7 * t3;
        $o3 =  7 * t1 - 17 * t3;
    }
}

macro_rules! itx {
    ($a:expr, $b:expr, $c:expr, $d:expr, $bias:expr) => {
        let t0 = 13 * ($a + $c) + $bias;
        let t1 = 13 * ($a - $c) + $bias;
        let t2 =  7 * $b - 17 * $d;
        let t3 = 17 * $b +  7 * $d;
        $a = t0 + t3;
        $d = t0 - t3;
        $b = t1 + t2;
        $c = t1 - t2;
    }
}

impl BlockOps for Block {
    fn from_diff(&mut self, new: &[u8], old: &[u8], stride: usize) {
        for (dline, (oline, nline)) in self.coeffs.chunks_mut(4).zip(old.chunks(stride).zip(new.chunks(stride))) {
            for (dst, (&o, &n)) in dline.iter_mut().zip(oline.iter().zip(nline.iter())) {
                *dst = i16::from(n) - i16::from(o);
            }
        }
    }
    fn add_to(&self, dst: &mut [u8], stride: usize) {
        for (line, row) in dst.chunks_mut(stride).zip(self.coeffs.chunks(4)) {
            for (dst, &add) in line.iter_mut().zip(row.iter()) {
                *dst = clip8(i16::from(*dst) + add);
            }
        }
    }
    fn quant_dcs(&mut self, q_dc: usize, q_ac: usize) {
        let q_dc = i32::from(RV34_QUANT_TAB[q_dc]);
        let q_ac = i32::from(RV34_QUANT_TAB[q_ac]);
        for (i, el) in self.coeffs.iter_mut().enumerate() {
            if *el != 0 {
                let q = if matches!(i, 0 | 1 | 4) { q_dc } else { q_ac };
                *el = (i32::from(*el) * 16 / q).max(-511).min(511) as i16;
            }
        }
    }
    fn quant(&mut self, q_dc: usize, q_ac: usize) {
        let q_dc = RV34_QUANT_TAB[q_dc];
        let q_ac = RV34_QUANT_TAB[q_ac];
        if self.coeffs[0] != 0 {
            self.coeffs[0] = self.coeffs[0] * 16 / q_dc;
        }
        for el in self.coeffs.iter_mut().skip(1) {
            if *el != 0 {
                *el = *el * 16 / q_ac;
            }
        }
    }
    fn dequant_dcs(&mut self, q_dc: usize, q_ac: usize) {
        let q_dc = i32::from(RV34_QUANT_TAB[q_dc]);
        let q_ac = i32::from(RV34_QUANT_TAB[q_ac]);
        for (i, el) in self.coeffs.iter_mut().enumerate() {
            if *el != 0 {
                let q = if matches!(i, 0 | 1 | 4) { q_dc } else { q_ac };
                *el = ((i32::from(*el) * q + 8) >> 4) as i16;
            }
        }
    }
    fn dequant(&mut self, q_dc: usize, q_ac: usize) {
        let q_ac = i32::from(RV34_QUANT_TAB[q_ac]);
        if self.coeffs[0] != 0 {
            let q_dc = i32::from(RV34_QUANT_TAB[q_dc]);
            self.coeffs[0] = ((i32::from(self.coeffs[0]) * q_dc + 8) >> 4) as i16;
        }
        for el in self.coeffs.iter_mut().skip(1) {
            if *el != 0 {
                *el = ((i32::from(*el) * q_ac + 8) >> 4) as i16;
            }
        }
    }
    fn transform_4x4(&mut self) {
        let mut tmp = [0; 16];
        for (drow, srow) in tmp.chunks_mut(4).zip(self.coeffs.chunks(4)) {
            tx!(i32::from(srow[0]), i32::from(srow[1]), i32::from(srow[2]), i32::from(srow[3]),
                drow[0], drow[1], drow[2], drow[3]);
        }
        for i in 0..4 {
            tx!(tmp[i], tmp[i + 4], tmp[i + 8], tmp[i + 12],
                tmp[i], tmp[i + 4], tmp[i + 8], tmp[i + 12]);
        }
        for (dst, &src) in self.coeffs.iter_mut().zip(tmp.iter()) {
            *dst = ((src + 223) / 446) as i16;
        }
    }
    fn transform_dcs(&mut self) {
        let mut tmp = [0; 16];
        for (drow, srow) in tmp.chunks_mut(4).zip(self.coeffs.chunks(4)) {
            tx!(i32::from(srow[0]), i32::from(srow[1]), i32::from(srow[2]), i32::from(srow[3]),
                drow[0], drow[1], drow[2], drow[3]);
        }
        for i in 0..4 {
            tx!(tmp[i], tmp[i + 4], tmp[i + 8], tmp[i + 12],
                tmp[i], tmp[i + 4], tmp[i + 8], tmp[i + 12]);
        }
        for (dst, &src) in self.coeffs.iter_mut().zip(tmp.iter()) {
            *dst = ((src + 334) / 669) as i16;
        }
    }
    fn itransform_4x4(&mut self) {
        let mut tmp: [i32; 16] = [0; 16];
        for (dst, &src) in tmp.iter_mut().zip(self.coeffs.iter()) {
            *dst = i32::from(src);
        }
        for row in tmp.chunks_mut(4) {
            itx!(row[0], row[1], row[2], row[3], 0);
        }
        for i in 0..4 {
            itx!(tmp[i], tmp[i + 4], tmp[i + 2 * 4], tmp[i + 3 * 4], 0x200);
        }
        for (dst, &src) in self.coeffs.iter_mut().zip(tmp.iter()) {
            *dst = (src >> 10) as i16;
        }
    }
    fn itransform_dcs(&mut self) {
        let mut tmp: [i32; 16] = [0; 16];
        for (dst, &src) in tmp.iter_mut().zip(self.coeffs.iter()) {
            *dst = i32::from(src);
        }
        for row in tmp.chunks_mut(4) {
            itx!(row[0], row[1], row[2], row[3], 0);
        }
        for i in 0..4 {
            itx!(tmp[i], tmp[i + 4], tmp[i + 2 * 4], tmp[i + 3 * 4], 0);
        }
        for (dst, &src) in self.coeffs.iter_mut().zip(tmp.iter()) {
            *dst = ((src * 3) >> 11) as i16;
        }
    }
}

const RV34_QUANT_TAB: [i16; 32] = [
     60,   67,   76,   85,   96,  108,  121,  136,
    152,  171,  192,  216,  242,  272,  305,  341,
    383,  432,  481,  544,  606,  683,  767,  854,
    963, 1074, 1212, 1392, 1566, 1708, 1978, 2211
];
