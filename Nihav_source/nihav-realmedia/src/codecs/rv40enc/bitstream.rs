use nihav_core::frame::FrameType;
use nihav_core::io::bitwriter::*;
use nihav_core::io::intcode::*;
use nihav_codec_support::codecs::MV;
use super::types::*;
use super::super::rv34codes::*;
use super::super::rv40data::*;

pub fn write_slice_header(bw: &mut BitWriter, ftype: FrameType, q: usize, set_idx: usize, deblock: bool, pts: u32) {
    bw.write0();
    match ftype {
        FrameType::I => bw.write(0, 2),
        FrameType::P => bw.write(2, 2),
        FrameType::B => bw.write(3, 2),
        _ => unreachable!(),
    };
    bw.write(q as u32, 5);
    bw.write(0, 2); // unknown
    bw.write(set_idx as u32, 2);
    bw.write(!deblock as u32, 1);
    bw.write(pts, 13);
}

pub fn write_slice_dimensions(bw: &mut BitWriter, width: usize, height: usize) {
    let wcode = match width {
            160 => 0,
            176 => 1,
            240 => 2,
            320 => 3,
            352 => 4,
            640 => 5,
            704 => 6,
            _   => 7,
        };
    bw.write(wcode, 3);
    if wcode == 7 {
        let mut w = width >> 2;
        while w >= 255 {
            bw.write(255, 8);
            w -= 255;
        }
        bw.write(w as u32, 8);
    }

    let hcode = match height {
            120 => 0,
            132 => 1,
            144 => 2,
            240 => 3,
            288 => 4,
            480 => 5,
            180 => 6,
            360 => 7,
            576 => 8,
            _   => 9,
        };
    if hcode < 6 {
        bw.write(hcode, 3);
    } else {
        bw.write(hcode + 6, 4);
        if hcode == 9 {
            let mut h = height >> 2;
            while h >= 255 {
                bw.write(255, 8);
                h -= 255;
            }
            bw.write(h as u32, 8);
        }
    }
}

pub fn write_slice_mb_idx(bw: &mut BitWriter, mb_idx: usize, num_mbs: usize) {
    let mba_bits = match num_mbs - 1 {
               0..=  47 =>  6,
              48..=  98 =>  7,
              99..= 395 =>  9,
             396..=1583 => 11,
            1584..=6335 => 13,
            6336..=9215 => 14,
            _ => unreachable!(),
        };
    bw.write(mb_idx as u32, mba_bits);
}

pub fn write_skip_count(bw: &mut BitWriter, skip_count: u32) {
    bw.write_code(UintCodeType::Gamma, skip_count);
}

fn write_mv(bw: &mut BitWriter, mv: MV) {
    let xcode = if mv.x > 0 { (mv.x - 1) * 2 + 1 } else { -mv.x * 2 } as u32;
    let ycode = if mv.y > 0 { (mv.y - 1) * 2 + 1 } else { -mv.y * 2 } as u32;

    bw.write_code(UintCodeType::Gamma, xcode);
    bw.write_code(UintCodeType::Gamma, ycode);
}

pub fn write_mb_header(bw: &mut BitWriter, ftype: FrameType, sstate: &SliceState, mbstate: &MBState) {
    let mb_idx = mbstate.get_mb_idx(sstate.mb_x, sstate.mb_y);
    let pred_mbt = mbstate.get_pred_mbtype(sstate, ftype == FrameType::B);

    let set_id = pred_mbt.to_code();

    if ftype != FrameType::I {
        let (codes, lens) = if ftype == FrameType::P {
                (&RV40_PTYPE_CODES[set_id][..], &RV40_PTYPE_BITS[set_id][..])
            } else {
                (&RV40_BTYPE_CODES[set_id][..], &RV40_BTYPE_BITS[set_id][..])
            };
        let idx = mbstate.mb_type[mb_idx].to_code();
        bw.write(codes[idx].into(), lens[idx]);
    }
    match mbstate.mb_type[mb_idx] {
        MBType::Intra16 => {
            if ftype == FrameType::I {
                bw.write1();
            }
            bw.write(mbstate.ipred[mbstate.get_blk4_idx(sstate.mb_x, sstate.mb_y)] as u32, 2);
        },
        MBType::Intra => {
            if ftype == FrameType::I {
                bw.write0();
                bw.write1(); //dquant
            }
            let ystart = if sstate.has_t { 0 } else { 1 };
            let mut blk4_idx = mbstate.get_blk4_idx(sstate.mb_x, sstate.mb_y);

            if !sstate.has_t {
                let mut code = 0;
                for &el in mbstate.ipred[blk4_idx..][..4].iter() {
                    code = code * 2 + if el == 0 { 0 } else { 1 };
                }
                bw.write(RV40_AIC_TOP_CODES[code].into(), RV40_AIC_TOP_BITS[code]);
                blk4_idx += mbstate.blk4_stride;
            }
            for y in ystart..4 {
                let mut x = 0;
                while x < 4 {
                    let (lctx, tctx, trctx) = mbstate.get_ipred4x4_ctx(sstate.mb_x, sstate.mb_y, x, y);
                    let mode = mbstate.ipred[blk4_idx + x];
                    let ctx_word = if x < 3 {
                            ((trctx & 0xF) as u16) + (((tctx & 0xF) as u16) << 4) + (((lctx & 0xF) as u16) << 8)
                        } else { 0xFFF };
                    if let Some(idx) = RV40_AIC_PATTERNS.iter().position(|&x| x == ctx_word) {
                        let mode1 = mbstate.ipred[blk4_idx + x + 1];
                        let code = mode * 9 + mode1;
                        bw.write(RV40_AIC_MODE2_CODES[idx][code as usize].into(),
                                 RV40_AIC_MODE2_BITS[idx][code as usize]);
                        x += 2;
                    } else if tctx != -1 && lctx != -1 {
                        let idx = (tctx + lctx * 10) as usize;
                        let code = mode as usize;
                        bw.write(RV40_AIC_MODE1_CODES[idx][code].into(),
                                 RV40_AIC_MODE1_BITS[idx][code]);
                        x += 1;
                    } else {
                        match lctx {
                            -1 if tctx < 2 => {
                                if mode == 0 {
                                    bw.write1();
                                } else {
assert_eq!(mode, 1);
                                    bw.write0();
                                }
                            },
                            0 | 2 => {
                                if mode == 0 {
                                    bw.write1();
                                } else {
assert_eq!(mode, 2);
                                    bw.write0();
                                }
                            },
                            _ => {
assert_eq!(mode, 0);
                            },
                        };
                        x += 1;
                    }
                }
                blk4_idx += mbstate.blk4_stride;
            }
        },
        MBType::P16x16 | MBType::P16x16Mix => {
            let diff_mv = mbstate.get_diff_mv(sstate, true, 0, 0);
            write_mv(bw, diff_mv);
        },
        MBType::P16x8 => {
            let diff_mv = mbstate.get_diff_mv(sstate, true, 0, 0);
            write_mv(bw, diff_mv);
            let diff_mv = mbstate.get_diff_mv(sstate, true, 0, 1);
            write_mv(bw, diff_mv);
        },
        MBType::P8x16 => {
            let diff_mv = mbstate.get_diff_mv(sstate, false, 0, 0);
            write_mv(bw, diff_mv);
            let diff_mv = mbstate.get_diff_mv(sstate, false, 1, 0);
            write_mv(bw, diff_mv);
        },
        MBType::P8x8 => {
            for i in 0..4 {
                let diff_mv = mbstate.get_diff_mv(sstate, false, i & 1, i >> 1);
                write_mv(bw, diff_mv);
            }
        },
        MBType::Forward => {
            let fwd_diff = mbstate.get_diff_mv_b(sstate, true);
            write_mv(bw, fwd_diff);
        },
        MBType::Backward => {
            let bwd_diff = mbstate.get_diff_mv_b(sstate, false);
            write_mv(bw, bwd_diff);
        },
        MBType::Bidir => {
            let fwd_diff = mbstate.get_diff_mv_b(sstate, true);
            let bwd_diff = mbstate.get_diff_mv_b(sstate, false);
            write_mv(bw, fwd_diff);
            write_mv(bw, bwd_diff);
        },
        MBType::Invalid => unreachable!(),
        _ => unimplemented!(),
    };
}

trait CodeWriter {
    fn write(&self, bw: &mut BitWriter, code: u16);
}

impl CodeWriter for RV34CodeReader {
    fn write(&self, bw: &mut BitWriter, to_write: u16) {
        for (&sym, (&code, &bits)) in self.syms.iter().zip(self.codes.iter().zip(self.lengths.iter())) {
            if sym == to_write {
                bw.write(code, bits);
                return;
            }
        }
unreachable!();
    }
}

impl CodeWriter for RV34CBPCodeReader {
    fn write(&self, bw: &mut BitWriter, to_write: u16) {
        for (&sym, (&code, &bits)) in self.syms.iter().zip(self.codes.iter().zip(self.lengths.iter())) {
            if u16::from(sym) == to_write {
                bw.write(code, bits);
                return;
            }
        }
unreachable!();
    }
}

struct CBPSet {
    cbp_pattern:    RV34CodeReader,
    cbp:            [RV34CBPCodeReader; 4]
}

impl CBPSet {
    fn new(intra: bool, set: usize, subset: usize) -> Self {
        if intra {
            let cbp_pat = RV34CodeReader::new(&RV34_INTRA_CBPPAT[set][subset]);
            let cbp0 = RV34CBPCodeReader::new(&RV34_INTRA_CBP[set][subset]);
            let cbp1 = RV34CBPCodeReader::new(&RV34_INTRA_CBP[set][subset + 1*2]);
            let cbp2 = RV34CBPCodeReader::new(&RV34_INTRA_CBP[set][subset + 2*2]);
            let cbp3 = RV34CBPCodeReader::new(&RV34_INTRA_CBP[set][subset + 3*2]);
            CBPSet { cbp_pattern: cbp_pat, cbp: [cbp0, cbp1, cbp2, cbp3] }
        } else {
            let cbp_pat = RV34CodeReader::new(&RV34_INTER_CBPPAT[set]);
            let cbp0 = RV34CBPCodeReader::new(&RV34_INTER_CBP[set][0]);
            let cbp1 = RV34CBPCodeReader::new(&RV34_INTER_CBP[set][1]);
            let cbp2 = RV34CBPCodeReader::new(&RV34_INTER_CBP[set][2]);
            let cbp3 = RV34CBPCodeReader::new(&RV34_INTER_CBP[set][3]);
            CBPSet { cbp_pattern: cbp_pat, cbp: [cbp0, cbp1, cbp2, cbp3] }
        }
    }
}

struct CoefSet {
    pat0:       Vec<RV34CodeReader>,
    pat1:       Vec<RV34CodeReader>,
    pat2:       Vec<RV34CodeReader>,
}

impl CoefSet {
    fn new(intra: bool, set: usize) -> Self {
        if intra {
            let first0 = RV34CodeReader::new(&RV34_INTRA_FIRSTPAT[set][0]);
            let first1 = RV34CodeReader::new(&RV34_INTRA_FIRSTPAT[set][1]);
            let first2 = RV34CodeReader::new(&RV34_INTRA_FIRSTPAT[set][2]);
            let first3 = RV34CodeReader::new(&RV34_INTRA_FIRSTPAT[set][3]);
            let firsts = vec![first0, first1, first2, first3];

            let second0 = RV34CodeReader::new(&RV34_INTRA_SECONDPAT[set][0]);
            let second1 = RV34CodeReader::new(&RV34_INTRA_SECONDPAT[set][1]);
            let seconds = vec![second0, second1];

            let third0 = RV34CodeReader::new(&RV34_INTRA_THIRDPAT[set][0]);
            let third1 = RV34CodeReader::new(&RV34_INTRA_THIRDPAT[set][1]);
            let thirds = vec![third0, third1];

            CoefSet { pat0: firsts, pat1: seconds, pat2: thirds }
        } else {
            let first0 = RV34CodeReader::new(&RV34_INTER_FIRSTPAT[set][0]);
            let first1 = RV34CodeReader::new(&RV34_INTER_FIRSTPAT[set][1]);
            let firsts = vec![first0, first1];

            let second0 = RV34CodeReader::new(&RV34_INTER_SECONDPAT[set][0]);
            let second1 = RV34CodeReader::new(&RV34_INTER_SECONDPAT[set][1]);
            let seconds = vec![second0, second1];

            let third0 = RV34CodeReader::new(&RV34_INTER_THIRDPAT[set][0]);
            let third1 = RV34CodeReader::new(&RV34_INTER_THIRDPAT[set][1]);
            let thirds = vec![third0, third1];

            CoefSet { pat0: firsts, pat1: seconds, pat2: thirds }
        }
    }
}

struct FullSet {
    cbp:        Vec<CBPSet>,
    cset:       CoefSet,
    coeffs:     RV34CodeReader,
}

impl FullSet {
    fn new(intra: bool, set: usize) -> Self {
        if intra {
            let cbp0 = CBPSet::new(intra, set, 0);
            let cbp1 = CBPSet::new(intra, set, 1);
            let cbp: Vec<CBPSet> = vec![cbp0, cbp1];
            let cset = CoefSet::new(intra, set);
            let coeffs = RV34CodeReader::new(&RV34_INTRA_COEFFS[set]);
            FullSet { cbp, cset, coeffs }
        } else {
            let cbp0 = CBPSet::new(intra, set, 0);
            let cbp: Vec<CBPSet> = vec![cbp0];
            let cset = CoefSet::new(intra, set);
            let coeffs = RV34CodeReader::new(&RV34_INTER_COEFFS[set]);
            FullSet { cbp, cset, coeffs }
        }
    }
    fn write_block(&self, bw: &mut BitWriter, blk: &Block, subset_idx: usize, luma: bool) {
        let sblk0 = [blk.coeffs[0], blk.coeffs[1], blk.coeffs[4], blk.coeffs[5]];
        let sblk1 = [blk.coeffs[2], blk.coeffs[3], blk.coeffs[6], blk.coeffs[7]];
        let sblk2 = [blk.coeffs[8], blk.coeffs[12], blk.coeffs[9], blk.coeffs[13]]; // sub-block 2 has different order
        let sblk3 = [blk.coeffs[10], blk.coeffs[11], blk.coeffs[14], blk.coeffs[15]];

        let idx0 = get_subblock_index(&sblk0);
        let idx1 = get_subblock_index(&sblk1);
        let idx2 = get_subblock_index(&sblk2);
        let idx3 = get_subblock_index(&sblk3);

        let mut cflags = idx0;
        cflags = (cflags << 1) | ((idx1 != 0) as u16);
        cflags = (cflags << 1) | ((idx2 != 0) as u16);
        cflags = (cflags << 1) | ((idx3 != 0) as u16);

        self.cset.pat0[subset_idx].write(bw, cflags);

        if matches!(idx0, 0 | 27 | 54 | 81) { // only first coefficient is set
            write_single_coeff(bw, &self.coeffs, sblk0[0], 3);
        } else {
            write_coeffs(bw, &self.coeffs, &sblk0);
        }
        if idx1 != 0 {
            self.cset.pat1[!luma as usize].write(bw, idx1);
            write_coeffs(bw, &self.coeffs, &sblk1);
        }
        if idx2 != 0 {
            self.cset.pat1[!luma as usize].write(bw, idx2);
            write_coeffs(bw, &self.coeffs, &sblk2);
        }
        if idx3 != 0 {
            self.cset.pat2[!luma as usize].write(bw, idx3);
            write_coeffs(bw, &self.coeffs, &sblk3);
        }
    }
}

fn write_coeffs(bw: &mut BitWriter, coeffs: &RV34CodeReader, blk: &[i16; 4]) {
    for (&val, &limit) in blk.iter().zip([3i16, 2, 2, 2].iter()) {
        write_single_coeff(bw, coeffs, val, limit);
    }
}

fn write_single_coeff(bw: &mut BitWriter, coeffs: &RV34CodeReader, val: i16, limit: i16) {
    if val != 0 {
        if val.abs() >= limit {
            let mut val = (val.abs() - limit) as u16;
            if val > 23 {
                val -= 22;
                let bits = (15 - val.leading_zeros()) as u16;
                coeffs.write(bw, bits + 23);
                bw.write(u32::from(val - (1 << bits)), bits as u8);
            } else {
                coeffs.write(bw, val);
            }
        }
        if val > 0 {
            bw.write0();
        } else {
            bw.write1();
        }
    }
}

pub struct CodeSets {
    super_idx:  usize,
    set_idx:    usize,
    intra:      bool,
    is16:       bool,
    is_p16:     bool,

    iset:       Vec<FullSet>,
    pset:       Vec<FullSet>,
}

impl CodeSets {
    pub fn new() -> Self {
        let mut iset: Vec<FullSet> = Vec::with_capacity(5);
        for set in 0..5 { iset.push(FullSet::new(true, set)); }
        let mut pset: Vec<FullSet> = Vec::with_capacity(7);
        for set in 0..7 { pset.push(FullSet::new(false, set)); }

        Self {
            iset, pset,
            super_idx:  0,
            set_idx:    0,
            intra:      false,
            is16:       false,
            is_p16:     false,
        }
    }
    pub fn init(&mut self, quant: usize, subset: usize) {
        let mut idx = quant;
        if (subset == 2) && (idx < 19) {
            idx += 10;
        } else if (subset != 0) && (idx < 26) {
            idx += 5;
        }
        if idx > 30 {
            idx = 30;
        }
        self.super_idx = idx;
    }
    pub fn set_params(&mut self, mbtype: &MacroblockType) {
        self.is_p16 = matches!(*mbtype, MacroblockType::InterMix(_));
        self.intra = mbtype.is_intra() || self.is_p16;
        self.is16 = mbtype.is_16();
        self.set_idx = if self.intra {
                RV34_SET_IDX_INTRA[self.super_idx]
            } else {
                RV34_SET_IDX_INTER[self.super_idx]
            };
    }
    fn write_cbp(&self, bw: &mut BitWriter, coded_pat: [bool; 24], cbp_code: &CBPSet) {
        let mut cbp_pat = 0u16;
        for i in 16..20 {
            cbp_pat = cbp_pat * 3 + (coded_pat[i] as u16) + (coded_pat[i + 4] as u16);
        }
        let mut nnz = 0usize;
        for blk4 in coded_pat[..16].chunks(4) {
            let cur_nz = blk4.contains(&true);
            if cur_nz {
                nnz += 1;
            }
            cbp_pat = cbp_pat * 2 + (cur_nz as u16);
        }
        nnz = nnz.saturating_sub(1);

        cbp_code.cbp_pattern.write(bw, cbp_pat);
        for blk4 in coded_pat[..16].chunks(4) {
            let pat = (blk4[3] as u16) * 32 + (blk4[2] as u16) * 16 + (blk4[1] as u16) * 2 + (blk4[0] as u16);
            if pat != 0 {
                cbp_code.cbp[nnz].write(bw, pat);
            }
        }
        for i in 16..20 {
            if coded_pat[i] ^ coded_pat[i + 4] {
                if coded_pat[i] {
                    bw.write1();
                } else {
                    bw.write0();
                }
            }
        }
    }
    pub fn write_coeffs(&mut self, bw: &mut BitWriter, coeffs: &[Block; 25]) {
        let mut fset = if self.intra { &self.iset[self.set_idx] } else { &self.pset[self.set_idx] };

        const CODED_ORDER: [usize; 24] = [0, 1, 4, 5, 2, 3, 6, 7, 8, 9, 12, 13, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23];
        let cbp_code = &fset.cbp[if self.is16 { 1 } else { 0 }];
        let mut coded_blk = [false; 24];
        let mut coded_pat = [false; 24];
        for (i, ((cpat, cblk), &seq)) in coded_pat.iter_mut().zip(coded_blk.iter_mut())
                .zip(CODED_ORDER.iter()).enumerate() {
            *cpat = !coeffs[seq].is_empty();
            *cblk = !coeffs[i].is_empty();
        }
        self.write_cbp(bw, coded_pat, cbp_code);

        if self.is16 {
            fset.write_block(bw, &coeffs[24], 3, true);
        }
        let (luma_set, chroma_set) = if self.intra {
                (if self.is16 { 2 } else { 1 }, if !self.is_p16 { 0 } else { 1 })
            } else {
                (0, 1)
            };
        let mut citer = coded_blk.iter();
        for blk in coeffs[..16].iter() {
            if let Some(true) = citer.next() {
                fset.write_block(bw, blk, luma_set, true);
            }
        }
        if self.is_p16 {
            self.set_idx = RV34_SET_IDX_INTER[self.super_idx];
            fset = &self.pset[self.set_idx];
        }
        for blk in coeffs[16..24].iter() {
            if let Some(true) = citer.next() {
                fset.write_block(bw, blk, chroma_set, false);
            }
        }
    }
}

fn get_subblock_index(blk: &[i16; 4]) -> u16 {
    let mut idx = blk[0].abs().min(3) as u16;
    idx = idx * 3 + (blk[1].abs().min(2) as u16);
    idx = idx * 3 + (blk[2].abs().min(2) as u16);
    idx = idx * 3 + (blk[3].abs().min(2) as u16);
    idx
}
