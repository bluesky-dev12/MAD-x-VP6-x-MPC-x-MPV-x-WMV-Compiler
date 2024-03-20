use super::types::Block;

mod blk;
pub use blk::*;
mod ipred;
pub use ipred::*;
mod loopfilt;
pub use loopfilt::*;
mod mc;
pub use mc::*;

fn clip8(a: i16) -> u8 {
    if a < 0 { 0 }
    else if a > 255 { 255 }
    else { a as u8 }
}

pub struct RefMBData {
    pub y:  [u8; 16 * 16],
    pub u:  [u8; 8 * 8],
    pub v:  [u8; 8 * 8],
}

impl RefMBData {
    pub fn new() -> Self {
        Self {
            y:  [0; 16 * 16],
            u:  [0; 8 * 8],
            v:  [0; 8 * 8],
        }
    }
    pub fn copy_from(&mut self, other: &Self) {
        self.y.copy_from_slice(&other.y);
        self.u.copy_from_slice(&other.u);
        self.v.copy_from_slice(&other.v);
    }
    pub fn calc_coeffs(&self, new: &Self, coeffs: &mut [Block; 25], q_dc: usize, q_ac: usize, is16: bool) {
        let (blocks, dcs) = coeffs.split_at_mut(24);
        let mut dblocks = blocks.iter_mut();
        let dcs = &mut dcs[0];
        for (y, (dstripe, sstripe)) in self.y.chunks(16 * 4).zip(new.y.chunks(16 * 4)).enumerate() {
            for x in (0..16).step_by(4) {
                let dst = dblocks.next().unwrap();
                Self::diff_blk(&sstripe[x..], &dstripe[x..], 16, dst);
                dst.transform_4x4();
                if is16 {
                    dcs.coeffs[x / 4 + y * 4] = dst.coeffs[0];
                    dst.coeffs[0] = 0;
                }
                dst.quant(q_ac, q_ac);
            }
        }
        let (cq_dc, cq_ac) = chroma_quants(q_ac);
        for (dstripe, sstripe) in self.u.chunks(8 * 4).zip(new.u.chunks(8 * 4)) {
            for x in (0..8).step_by(4) {
                let dst = dblocks.next().unwrap();
                Self::diff_blk(&sstripe[x..], &dstripe[x..], 8, dst);
                dst.transform_4x4();
                dst.quant(cq_dc, cq_ac);
            }
        }
        for (dstripe, sstripe) in self.v.chunks(8 * 4).zip(new.v.chunks(8 * 4)) {
            for x in (0..8).step_by(4) {
                let dst = dblocks.next().unwrap();
                Self::diff_blk(&sstripe[x..], &dstripe[x..], 8, dst);
                dst.transform_4x4();
                dst.quant(cq_dc, cq_ac);
            }
        }
        if is16 {
            coeffs[24].transform_dcs();
            coeffs[24].quant_dcs(q_dc, q_ac);
        }
    }
    fn diff_blk(src: &[u8], new: &[u8], stride: usize, dst: &mut Block) {
        for (drow, (sline, nline)) in dst.coeffs.chunks_mut(4).zip(src.chunks(stride).zip(new.chunks(stride))) {
            for (dst, (&a, &b)) in drow.iter_mut().zip(sline.iter().zip(nline.iter())) {
                *dst = i16::from(a) - i16::from(b);
            }
        }
    }
    pub fn avg(&mut self, ref1: &Self, weight1: u32, ref2: &Self, weight2: u32) {
        for (dst, (&src1, &src2)) in self.y.iter_mut().zip(ref1.y.iter().zip(ref2.y.iter())) {
            *dst = weight(src1, weight1, src2, weight2);
        }
        for (dst, (&src1, &src2)) in self.u.iter_mut().zip(ref1.u.iter().zip(ref2.u.iter())) {
            *dst = weight(src1, weight1, src2, weight2);
        }
        for (dst, (&src1, &src2)) in self.v.iter_mut().zip(ref1.v.iter().zip(ref2.v.iter())) {
            *dst = weight(src1, weight1, src2, weight2);
        }
    }
}

fn weight(pix1: u8, weight1: u32, pix2: u8, weight2: u32) -> u8 {
    ((((u32::from(pix1) * weight1) >> 9) + ((u32::from(pix2) * weight2) >> 9) + 0x10) >> 5) as u8
}

pub fn chroma_quants(q: usize) -> (usize, usize) {
    (RV34_CHROMA_QUANT_DC[q].into(), RV34_CHROMA_QUANT_AC[q].into())
}

const RV34_CHROMA_QUANT_DC: [u8; 32] = [
     0,  0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,
    14, 15, 15, 16, 17, 18, 18, 19, 20, 20, 21, 21, 22, 22, 23, 23
];
const RV34_CHROMA_QUANT_AC: [u8; 32] = [
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    16, 17, 17, 18, 19, 20, 20, 21, 22, 22, 23, 23, 24, 24, 25, 25
];
