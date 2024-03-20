use std::mem;
use std::ptr;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::{MV, ZERO_MV, ZIGZAG};
use nihav_codec_support::codecs::blockdsp::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::io::intcode::*;
use super::vpcommon::*;

#[derive(Clone,Copy,Debug,PartialEq)]
enum SBState {
    Coded,
    Partial,
    Uncoded,
}

fn map_idx(idx: usize) -> u8 {
    idx as u8
}

struct VP30Codes {
    dc_cb:      [Codebook<u8>; 5],
    ac_i_cb:    [Codebook<u8>; 5],
    ac_p_cb:    [Codebook<u8>; 5],
    mbtype_cb:  Codebook<VPMBType>,
}

fn map_mbt(idx: usize) -> VPMBType {
    VP30_MBTYPE_SYMS[idx]
}

impl VP30Codes {
    fn new() -> Self {
        let dc_cb;
        let ac_i_cb;
        let ac_p_cb;
        let mut cr = TableCodebookDescReader::new(&VP30_MBTYPE_CODES, &VP30_MBTYPE_BITS, map_mbt);
        let mbtype_cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        unsafe {
            let mut udc_cb: mem::MaybeUninit::<[Codebook<u8>; 5]> = mem::MaybeUninit::uninit();
            let mut uac_i_cb: mem::MaybeUninit::<[Codebook<u8>; 5]> = mem::MaybeUninit::uninit();
            let mut uac_p_cb: mem::MaybeUninit::<[Codebook<u8>; 5]> = mem::MaybeUninit::uninit();
            for i in 0..5 {
                let mut cr = TableCodebookDescReader::new(&VP30_DC_CODES[i], &VP30_DC_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*udc_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP30_AC_INTRA_CODES[i], &VP30_AC_INTRA_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac_i_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP30_AC_INTER_CODES[i], &VP30_AC_INTER_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac_p_cb.as_mut_ptr())[i], cb);
            }
            dc_cb   = udc_cb.assume_init();
            ac_i_cb = uac_i_cb.assume_init();
            ac_p_cb = uac_p_cb.assume_init();
        }
        Self { dc_cb, ac_i_cb, ac_p_cb, mbtype_cb }
    }
}

struct VP31Codes {
    dc_cb:      [Codebook<u8>; 16],
    ac0_cb:     [Codebook<u8>; 16],
    ac1_cb:     [Codebook<u8>; 16],
    ac2_cb:     [Codebook<u8>; 16],
    ac3_cb:     [Codebook<u8>; 16],
}

impl VP31Codes {
    fn new() -> Self {
        let dc_cb;
        let ac0_cb;
        let ac1_cb;
        let ac2_cb;
        let ac3_cb;
        unsafe {
            let mut udc_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            let mut uac0_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            let mut uac1_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            let mut uac2_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            let mut uac3_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            for i in 0..16 {
                let mut cr = TableCodebookDescReader::new(&VP31_DC_CODES[i], &VP31_DC_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*udc_cb.as_mut_ptr())[i], cb);

                let mut cr = TableCodebookDescReader::new(&VP31_AC_CAT0_CODES[i], &VP31_AC_CAT0_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac0_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP31_AC_CAT1_CODES[i], &VP31_AC_CAT1_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac1_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP31_AC_CAT2_CODES[i], &VP31_AC_CAT2_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac2_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP31_AC_CAT3_CODES[i], &VP31_AC_CAT3_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac3_cb.as_mut_ptr())[i], cb);
            }
            dc_cb = udc_cb.assume_init();
            ac0_cb = uac0_cb.assume_init();
            ac1_cb = uac1_cb.assume_init();
            ac2_cb = uac2_cb.assume_init();
            ac3_cb = uac3_cb.assume_init();
        }
        Self { dc_cb, ac0_cb, ac1_cb, ac2_cb, ac3_cb }
    }
    fn new_vp4() -> VP31Codes {
        let dc_cb;
        let ac0_cb;
        let ac1_cb;
        let ac2_cb;
        let ac3_cb;
        unsafe {
            let mut udc_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            let mut uac0_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            let mut uac1_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            let mut uac2_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            let mut uac3_cb: mem::MaybeUninit::<[Codebook<u8>; 16]> = mem::MaybeUninit::uninit();
            for i in 0..16 {
                let mut cr = TableCodebookDescReader::new(&VP40_DC_CODES[i], &VP40_DC_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*udc_cb.as_mut_ptr())[i], cb);

                let mut cr = TableCodebookDescReader::new(&VP40_AC_CAT0_CODES[i], &VP40_AC_CAT0_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac0_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP40_AC_CAT1_CODES[i], &VP40_AC_CAT1_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac1_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP40_AC_CAT2_CODES[i], &VP40_AC_CAT2_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac2_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP40_AC_CAT3_CODES[i], &VP40_AC_CAT3_BITS[i], map_idx);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*uac3_cb.as_mut_ptr())[i], cb);
            }
            dc_cb = udc_cb.assume_init();
            ac0_cb = uac0_cb.assume_init();
            ac1_cb = uac1_cb.assume_init();
            ac2_cb = uac2_cb.assume_init();
            ac3_cb = uac3_cb.assume_init();
        }
        VP31Codes { dc_cb, ac0_cb, ac1_cb, ac2_cb, ac3_cb }
    }
}

struct VP40AuxCodes {
    mv_x_cb:    [Codebook<i8>; 7],
    mv_y_cb:    [Codebook<i8>; 7],
    mbpat_cb:   [Codebook<u8>; 2],
}

fn map_mv(idx: usize) -> i8 {
    (idx as i8) - 31
}

impl VP40AuxCodes {
    fn new() -> Self {
        let mv_x_cb;
        let mv_y_cb;
        unsafe {
            let mut umv_x_cb: mem::MaybeUninit::<[Codebook<i8>; 7]> = mem::MaybeUninit::uninit();
            let mut umv_y_cb: mem::MaybeUninit::<[Codebook<i8>; 7]> = mem::MaybeUninit::uninit();
            for i in 0..7 {
                let mut cr = TableCodebookDescReader::new(&VP40_MV_X_CODES[i], &VP40_MV_X_BITS[i], map_mv);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*umv_x_cb.as_mut_ptr())[i], cb);
                let mut cr = TableCodebookDescReader::new(&VP40_MV_Y_CODES[i], &VP40_MV_Y_BITS[i], map_mv);
                let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
                ptr::write(&mut (*umv_y_cb.as_mut_ptr())[i], cb);
            }
            mv_x_cb = umv_x_cb.assume_init();
            mv_y_cb = umv_y_cb.assume_init();
        }
        let mut cr0 = TableCodebookDescReader::new(&VP40_MBPAT_CODES[0], &VP40_MBPAT_BITS[0], map_idx);
        let mut cr1 = TableCodebookDescReader::new(&VP40_MBPAT_CODES[1], &VP40_MBPAT_BITS[1], map_idx);
        let mbpat_cb = [Codebook::new(&mut cr0, CodebookMode::MSB).unwrap(),
                        Codebook::new(&mut cr1, CodebookMode::MSB).unwrap()];
        Self { mv_x_cb, mv_y_cb, mbpat_cb }
    }
}

#[allow(clippy::large_enum_variant)]
enum Codes {
    None,
    VP30(VP30Codes),
    VP31(VP31Codes),
}

#[derive(Clone)]
struct Block {
    btype:      VPMBType,
    coeffs:     [i16; 64],
    has_ac:     bool,
    idx:        usize,
    mv:         MV,
    coded:      bool,
}

impl Block {
    fn new() -> Self {
        Self {
            btype:      VPMBType::Intra,
            coeffs:     [0; 64],
            has_ac:     false,
            idx:        0,
            mv:         ZERO_MV,
            coded:      false,
        }
    }
}

type ReadRunFunc = fn (&mut BitReader) -> DecoderResult<usize>;

const VP31_LONG_RUN_BASE: [usize; 7] = [ 1, 2, 4, 6, 10, 18, 34 ];
const VP31_LONG_RUN_BITS: [u8;    7] = [ 0, 1, 1, 2,  3,  4, 12 ];
fn read_long_run(br: &mut BitReader) -> DecoderResult<usize> {
    let pfx                                     = br.read_code(UintCodeType::LimitedUnary(6, 0))? as usize;
    if pfx == 0 { return Ok(1); }
    Ok(VP31_LONG_RUN_BASE[pfx] + (br.read(VP31_LONG_RUN_BITS[pfx])? as usize))
}

const VP31_SHORT_RUN_BASE: [usize; 6] = [ 1, 3, 5, 7, 11, 15 ];
const VP31_SHORT_RUN_BITS: [u8;    6] = [ 1, 1, 1, 2,  2,  4 ];
fn read_short_run(br: &mut BitReader) -> DecoderResult<usize> {
    let pfx                                     = br.read_code(UintCodeType::LimitedUnary(5, 0))? as usize;
    Ok(VP31_SHORT_RUN_BASE[pfx] + (br.read(VP31_SHORT_RUN_BITS[pfx])? as usize))
}

fn read_mb_run(br: &mut BitReader) -> DecoderResult<usize> {
    let pfx                                     = br.read_code(UintCodeType::LimitedUnary(10, 0))? as usize;
    if pfx == 10 { unimplemented!(); }
    if pfx < 2 {
        Ok(pfx + 1)
    } else {
        let base = (1 << (pfx - 1)) + 1;
        let bits = (pfx - 1) as u8;
        let add_bits                            = br.read(bits)? as usize;
        Ok(base + add_bits)
    }
}

struct BitRunDecoder {
    value:      bool,
    run:        usize,
    read_run:   ReadRunFunc,
}

impl BitRunDecoder {
    fn new(br: &mut BitReader, read_run: ReadRunFunc) -> DecoderResult<Self> {
        let value                               = !br.read_bool()?; // it will be flipped before run decoding
        Ok(Self { value, run: 0, read_run })
    }
    fn get_val(&mut self, br: &mut BitReader) -> DecoderResult<bool> {
        if self.run == 0 {
            self.value = !self.value;
            self.run = (self.read_run)(br)?;
        }
        self.run -= 1;
        Ok(self.value)
    }
}

const VP30_NE0_BITS: [u8; 5] = [ 2, 2, 3, 4, 8 ];
const VP30_NE0_BASE: [usize; 5] = [ 1, 5, 9, 17, 33 ];
fn vp30_read_ne_run0(br: &mut BitReader) -> DecoderResult<usize> {
    let len                                     = br.read_code(UintCodeType::LimitedUnary(4, 0))? as usize;
    Ok(VP30_NE0_BASE[len] + (br.read(VP30_NE0_BITS[len])? as usize))
}
fn vp30_read_ne_run1(br: &mut BitReader) -> DecoderResult<usize> {
    let len                                     = br.read_code(UintCodeType::LimitedUnary(6, 0))? as usize;
    if len == 0 {
        Ok((br.read(1)? as usize) + 1)
    } else if len < 6 {
        Ok(len + 2)
    } else {
        Ok((br.read(8)? as usize) + 8)
    }
}
fn vp30_read_coded_run0(br: &mut BitReader) -> DecoderResult<usize> {
    let len                                     = br.read_code(UintCodeType::LimitedUnary(5, 0))? as usize;
    Ok(len + 1)
}
/*
 0           - 1
 11          - 2
 1000        - 3
 1010        - 4
 10011       - 5
 10111       - 6
 10010       - 7 + get_bits(3)
 101100      - 15 + get_bits(5)
 1011010     - 47 + get_bits(8)
 1011011     - 303 + get_bits(16)
 */
const VP30_CRUN1_LUT: [u8; 32] = [
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x34, 0x34, 0x75, 0x55, 0x44, 0x44, 0x85, 0x65,
    0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22
];
fn vp30_read_coded_run1(br: &mut BitReader) -> DecoderResult<usize> {
    let idx                                     = br.peek(5) as usize;
    let sym = (VP30_CRUN1_LUT[idx] >> 4) as usize;
    let bits = VP30_CRUN1_LUT[idx] & 0xF;
                                                br.skip(u32::from(bits))?;
    if sym < 7 {
        Ok(sym)
    } else if sym == 7 {
        Ok(7 + (br.read(3)? as usize))
    } else {
        let len                                 = br.read_code(UintCodeType::Unary012)?;
        match len {
            0 => Ok(15 + (br.read(5)? as usize)),
            1 => Ok(47 + (br.read(8)? as usize)),
            _ => Ok(303 + (br.read(16)? as usize)),
        }
    }
}

struct VP34Decoder {
    info:       NACodecInfoRef,
    width:      usize,
    height:     usize,
    mb_w:       usize,
    mb_h:       usize,
    version:    u8,
    is_intra:   bool,
    update_gf:  bool,
    quant:      usize,
    shuf:       VPShuffler,
    codes:      Codes,
    aux_codes:  Option<VP40AuxCodes>,
    loop_str:   i16,
    mc_buf:     NAVideoBufferRef<u8>,

    blocks:     Vec<Block>,
    mb_coded:   Vec<bool>,
    mb_partial: Vec<bool>,
    y_blocks:   usize,
    y_sbs:      usize,
    qmat_y:     [i16; 64],
    qmat_c:     [i16; 64],
    qmat_y_p:   [i16; 64],
    qmat_c_p:   [i16; 64],

    eob_run:    usize,
    last_dc:    [i16; 3],

    blk_addr:   Vec<usize>,
    blk_sub:    Vec<u8>,
    sb_info:    Vec<SBState>,
    sb_blocks:  Vec<u8>,
    sb_mbs:     Vec<u8>,
    mb_blocks:  Vec<u8>,
}

fn vp30_read_mv_comp(br: &mut BitReader) -> DecoderResult<i16> {
    let mode                                    = br.read(2)?;
    if mode == 0 { return Ok(0); }
    let sign                                    = br.read_bool()?;
    let val = match mode - 1 {
            0 => 1,
            1 =>                                  2 + (br.read(2)? as i16),
            _ =>                                  br.read(5)? as i16,
        };
    if !sign {
        Ok(val)
    } else {
        Ok(-val)
    }
}

fn vp30_read_mv(br: &mut BitReader) -> DecoderResult<MV> {
    let x = vp30_read_mv_comp(br)?;
    let y = vp30_read_mv_comp(br)?;
    Ok(MV{ x, y })
}

fn read_mv_comp_packed(br: &mut BitReader) -> DecoderResult<i16> {
    let code                                    = br.read(3)?;
    match code {
        0 => Ok(0),
        1 => Ok(1),
        2 => Ok(-1),
        3 => if br.read_bool()? { Ok(-2) } else { Ok(2) },
        4 => if br.read_bool()? { Ok(-3) } else { Ok(3) },
        5 => {
            let val                             = (br.read(2)? as i16) + 4;
            if br.read_bool()? {
                Ok(-val)
            } else {
                Ok(val)
            }
        },
        6 => {
            let val                             = (br.read(3)? as i16) + 8;
            if br.read_bool()? {
                Ok(-val)
            } else {
                Ok(val)
            }
        },
        _ => {
            let val                             = (br.read(4)? as i16) + 16;
            if br.read_bool()? {
                Ok(-val)
            } else {
                Ok(val)
            }
        },
    }
}

fn read_mv_packed(br: &mut BitReader) -> DecoderResult<MV> {
    let x = read_mv_comp_packed(br)?;
    let y = read_mv_comp_packed(br)?;
    Ok(MV{ x, y })
}

fn read_mv_comp_raw(br: &mut BitReader) -> DecoderResult<i16> {
    let val                                     = br.read(5)? as i16;
    if br.read_bool()? {
        Ok(-val)
    } else {
        Ok(val)
    }
}

fn read_mv_raw(br: &mut BitReader) -> DecoderResult<MV> {
    let x = read_mv_comp_raw(br)?;
    let y = read_mv_comp_raw(br)?;
    Ok(MV{ x, y })
}

fn rescale_qmat(dst_qmat: &mut [i16; 64], base_qmat: &[i16; 64], dc_quant: i16, ac_quant: i16, minval: i16) {
    for (dst, src) in dst_qmat.iter_mut().zip(base_qmat.iter()) {
        *dst = (src.wrapping_mul(ac_quant) / 100).max(minval) << 2;
    }
    dst_qmat[0] = (base_qmat[0] * dc_quant / 100).max(minval * 2) << 2;
}

fn rescale_qmat_vp4(dst_qmat: &mut [i16; 64], base_qmat: &[i16; 64], dc_quant: i16, ac_quant: i16, is_intra: bool) {
    let (bias, minval) = if is_intra { (3, 4) } else { (6, 8) };
    for (dst, src) in dst_qmat.iter_mut().zip(base_qmat.iter()) {
        *dst = ((src - bias).wrapping_mul(ac_quant) / 100 + bias) << 2;
    }
    dst_qmat[0] = (base_qmat[0] * dc_quant / 100).max(minval) << 2;
}

fn expand_token(blk: &mut Block, br: &mut BitReader, eob_run: &mut usize, token: u8) -> DecoderResult<()> {
    match token {
        // EOBs
        0 | 1 | 2 => { *eob_run = (token as usize) + 1; },
        3 | 4 | 5 => {
            let bits = token - 1;
            *eob_run                            = (br.read(bits)? as usize) + (1 << bits);
        },
        6 => { *eob_run                         = br.read(12)? as usize; },
        // zero runs
        7 | 8 => {
            let bits = if token == 7 { 3 } else { 6 };
            let run                             = (br.read(bits)? as usize) + 1;
            blk.idx += run;
            validate!(blk.idx <= 64);
        },
        // single coefficients
        9 | 10 | 11 | 12 => {
            let val = (i16::from(token) - 7) >> 1;
            if (token & 1) == 1 {
                blk.coeffs[ZIGZAG[blk.idx]] = val;
            } else {
                blk.coeffs[ZIGZAG[blk.idx]] = -val;
            }
            blk.idx += 1;
        },
        13 | 14 | 15 | 16 => {
            let val = i16::from(token) - 10;
            if !br.read_bool()? {
                blk.coeffs[ZIGZAG[blk.idx]] = val;
            } else {
                blk.coeffs[ZIGZAG[blk.idx]] = -val;
            }
            blk.idx += 1;
        },
        17 | 18 | 19 | 20 | 21 | 22 => {
            let add_bits = if token == 22 { 9 } else { token - 16 };
            let sign                            = br.read_bool()?;
            let val                             = (br.read(add_bits)? as i16) + VP3_LITERAL_BASE[(token - 17) as usize];
            if !sign {
                blk.coeffs[ZIGZAG[blk.idx]] = val;
            } else {
                blk.coeffs[ZIGZAG[blk.idx]] = -val;
            }
            blk.idx += 1;
        }
        // zero run plus coefficient
        23 | 24 | 25 | 26 | 27 => {
            blk.idx += (token - 22) as usize;
            validate!(blk.idx < 64);
            if !br.read_bool()? {
                blk.coeffs[ZIGZAG[blk.idx]] = 1;
            } else {
                blk.coeffs[ZIGZAG[blk.idx]] = -1;
            }
            blk.idx += 1;
        },
        28 | 29 => {
            let run_bits = token - 26;
            if token == 28 {
                blk.idx += 6;
            } else {
                blk.idx += 10;
            }
            let sign                            = br.read_bool()?;
            blk.idx                            += br.read(run_bits)? as usize;
            validate!(blk.idx < 64);
            if !sign {
                blk.coeffs[ZIGZAG[blk.idx]] = 1;
            } else {
                blk.coeffs[ZIGZAG[blk.idx]] = -1;
            }
            blk.idx += 1;
        },
        30 => {
            blk.idx += 1;
            validate!(blk.idx < 64);
            let sign                            = br.read_bool()?;
            let val                             = (br.read(1)? as i16) + 2;
            if !sign {
                blk.coeffs[ZIGZAG[blk.idx]] = val;
            } else {
                blk.coeffs[ZIGZAG[blk.idx]] = -val;
            }
            blk.idx += 1;
        },
        _ => {
            let sign                            = br.read_bool()?;
            let val                             = (br.read(1)? as i16) + 2;
            blk.idx                            += (br.read(1)? as usize) + 2;
            validate!(blk.idx < 64);
            if !sign {
                blk.coeffs[ZIGZAG[blk.idx]] = val;
            } else {
                blk.coeffs[ZIGZAG[blk.idx]] = -val;
            }
            blk.idx += 1;
        },
    };
    if *eob_run > 0 {
        blk.idx = 64;
        *eob_run -= 1;
    } else if (token > 8) && (blk.idx > 1) {
        blk.has_ac = true;
    }
    Ok(())
}

macro_rules! fill_dc_pred {
    ($self: expr, $ref_id: expr, $pred: expr, $pp: expr, $bit: expr, $idx: expr) => {
        if $self.blocks[$idx].coded && $self.blocks[$idx].btype.get_ref_id() == $ref_id {
            $pred[$bit] = i32::from($self.blocks[$idx].coeffs[0]);
            $pp |= 1 << $bit;
        }
    };
}

fn vp31_loop_filter_v(frm: &mut NASimpleVideoFrame<u8>, x: usize, y: usize, plane: usize, loop_str: i16) {
    let off = frm.offset[plane] + x + y * frm.stride[plane];
    vp31_loop_filter(frm.data, off, 1, frm.stride[plane], 8, loop_str);
}

fn vp31_loop_filter_h(frm: &mut NASimpleVideoFrame<u8>, x: usize, y: usize, plane: usize, loop_str: i16) {
    let off = frm.offset[plane] + x + y * frm.stride[plane];
    vp31_loop_filter(frm.data, off, frm.stride[plane], 1, 8, loop_str);
}

fn vp3_mv_mode(mvx: i16, mvy: i16) -> usize {
    let mode = ((mvx & 1) + (mvy & 1) * 2) as usize;
    if (mode == 3) && (mvx ^ mvy < 0) {
        4
    } else {
        mode
    }
}

impl VP34Decoder {
    fn new(version: u8) -> Self {
        let vt = alloc_video_buffer(NAVideoInfo::new(24, 24, false, YUV420_FORMAT), 4).unwrap();
        let mc_buf = vt.get_vbuf().unwrap();
        Self {
            info:       NACodecInfoRef::default(),
            width:      0,
            height:     0,
            mb_w:       0,
            mb_h:       0,
            version,
            is_intra:   true,
            update_gf:  false,
            quant:      0,
            shuf:       VPShuffler::new(),
            codes:      Codes::None,
            aux_codes:  None,
            loop_str:   0,
            mc_buf,

            blocks:     Vec::new(),
            mb_coded:   Vec::new(),
            mb_partial: Vec::new(),
            y_blocks:   0,
            y_sbs:      0,

            qmat_y:     [0; 64],
            qmat_c:     [0; 64],
            qmat_y_p:   [0; 64],
            qmat_c_p:   [0; 64],

            eob_run:    0,
            last_dc:    [0; 3],

            blk_addr:   Vec::new(),
            blk_sub:    Vec::new(),
            sb_info:    Vec::new(),
            sb_blocks:  Vec::new(),
            sb_mbs:     Vec::new(),
            mb_blocks:  Vec::new(),
        }
    }
    fn parse_header(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        self.is_intra                           = !br.read_bool()?;
                                                  br.skip(1)?;
        self.quant                              = br.read(6)? as usize;
        if self.is_intra {
            if br.peek(8) != 0 {
                validate!(self.version == 3 || self.version == 30);
                let mb_w                        = br.read(8)? as usize;
                let mb_h                        = br.read(8)? as usize;
                validate!(mb_w == self.mb_w && mb_h == self.mb_h);
                if self.version == 3 {
                    self.version = 30;
                    self.codes   = Codes::VP30(VP30Codes::new());
                }
            } else {
                let version                     = br.read(13)?;
                let coding_type                 = br.read(1)?;
                validate!(coding_type == 0);
                                                  br.skip(2)?;
                if version == 1 {
                    validate!(self.version == 3 || self.version == 31);
                    if self.version == 3 {
                        self.version = 31;
                        self.codes   = Codes::VP31(VP31Codes::new());
                    }
                } else if version == 3 {
                    validate!(self.version == 4);
                    let mb_h                    = br.read(8)? as usize;
                    let mb_w                    = br.read(8)? as usize;
                    validate!(mb_w == self.mb_w && mb_h == self.mb_h);
                    let fact1                   = br.read(5)?;
                    let fact2                   = br.read(3)?;
                    validate!(fact1 == 1 && fact2 == 1);
                    let fact1                   = br.read(5)?;
                    let fact2                   = br.read(3)?;
                    validate!(fact1 == 1 && fact2 == 1);
                                                  br.skip(2)?;
                } else {
                    return Err(DecoderError::InvalidData);
                }
            }
        }
        self.loop_str = if self.version != 4 {
                VP31_LOOP_STRENGTH[self.quant]
            } else {
                VP40_LOOP_STRENGTH[self.quant]
            };
        Ok(())
    }
    fn vp30_unpack_sb_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut has_nonempty = false;
        {
            let mut bit                         = !br.read_bool()?;
            let mut run = 0;
            for sb in self.sb_info.iter_mut() {
                if run == 0 {
                    bit = !bit;
                    run = if bit { vp30_read_ne_run1(br)? } else { vp30_read_ne_run0(br)? };
                }
                *sb = if bit { has_nonempty = true; SBState::Partial } else { SBState::Uncoded };
                run -= 1;
            }
            validate!(run == 0);
        }
        if has_nonempty {
            for el in self.mb_coded.iter_mut() { *el = false; }
            let mut bit                         = !br.read_bool()?;
            let mut run = 0;
            let mut mbiter = self.mb_coded.iter_mut();
            for (sb, nmb) in self.sb_info.iter_mut().zip(self.sb_mbs.iter()) {
                let nmbs = *nmb as usize;
                if *sb == SBState::Partial {
                    for _ in 0..nmbs {
                        if run == 0 {
                            bit = !bit;
                            run = if bit { vp30_read_coded_run1(br)? } else { vp30_read_coded_run0(br)? };
                        }
                        run -= 1;
                        *mbiter.next().unwrap() = bit;
                    }
                } else {
                    for _ in 0..nmbs {
                        mbiter.next().unwrap();
                    }
                }
            }
            validate!(run == 0);
            let mut bit                         = !br.read_bool()?;
            let mut run = 0;
            let mut cur_blk = 0;
            for (coded, nblk) in self.mb_coded.iter().zip(self.mb_blocks.iter()) {
                let nblks = *nblk as usize;
                if *coded {
                    let mut cb = [false; 4];
                    for j in 0..nblks {
                        if run == 0 {
                            bit = !bit;
                            run = if bit { vp30_read_coded_run1(br)? } else { vp30_read_coded_run0(br)? };
                        }
                        run -= 1;
                        cb[j] = bit;
                    }
                    for j in 0..nblks {
                        let addr = self.blk_addr[cur_blk + j] >> 2;
                        self.blocks[addr].coded = cb[j];
                    }
                }
                cur_blk += nblks;
            }
            validate!(run == 0);
        }
        Ok(())
    }
    fn vp30_unpack_mb_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut cur_blk = 0;
        if let Codes::VP30(ref codes) = self.codes {
            for (sb, nblk) in self.sb_info.iter_mut().zip(self.sb_blocks.iter()).take(self.y_sbs) {
                let nblks = *nblk as usize;
                if *sb == SBState::Uncoded {
                    for _ in 0..nblks {
                        self.blocks[self.blk_addr[cur_blk] >> 2].btype = VPMBType::InterNoMV;
                        cur_blk += 1;
                    }
                } else {
                    for _ in 0..nblks/4 {
                        let mut coded = *sb == SBState::Coded;
                        if !coded {
                            for blk in 0..4 {
                                if self.blocks[self.blk_addr[cur_blk + blk] >> 2].coded {
                                    coded = true;
                                    break;
                                }
                            }
                        }
                        let mode = if !coded {
                                VPMBType::InterNoMV
                            } else {
                                                br.read_cb(&codes.mbtype_cb)?
                            };
                        for _ in 0..4 {
                            self.blocks[self.blk_addr[cur_blk] >> 2].btype = mode;
                            cur_blk += 1;
                        }
                    }
                }
            }
        } else {
            return Err(DecoderError::Bug);
        }
        // replicate types for chroma
        let mut off_y = 0;
        let mut off_u = self.y_blocks;
        let mut off_v = off_u + self.mb_w * self.mb_h;
        for _blk_y in 0..self.mb_h {
            for blk_x in 0..self.mb_w {
                let btype = self.blocks[off_y + blk_x * 2].btype;
                self.blocks[off_u + blk_x].btype = btype;
                self.blocks[off_v + blk_x].btype = btype;
            }
            off_y += self.mb_w * 2 * 2;
            off_u += self.mb_w;
            off_v += self.mb_w;
        }
        Ok(())
    }
    fn vp30_unpack_mv_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut last_mv = ZERO_MV;

        let mut cur_blk = 0;
        for _ in 0..self.y_blocks/4 {
            let baddr = self.blk_addr[cur_blk] >> 2;
            if self.blocks[baddr].btype == VPMBType::InterFourMV {
                let saddr = baddr.min(self.blk_addr[cur_blk + 1] >> 2).min(self.blk_addr[cur_blk + 2] >> 2).min(self.blk_addr[cur_blk + 3] >> 2);
                for i in 0..4 {
                    let blk = &mut self.blocks[saddr + (i & 1) + (i & 2) * self.mb_w];
                    if blk.coded {
                        blk.mv = vp30_read_mv(br)?;
                    }
                    cur_blk += 1;
                }
            } else {
                let cur_mv;
                match self.blocks[baddr].btype {
                    VPMBType::Intra | VPMBType::InterNoMV | VPMBType::GoldenNoMV => {
                        cur_mv = ZERO_MV;
                    },
                    VPMBType::InterMV => {
                        cur_mv = vp30_read_mv(br)?;
                        last_mv = cur_mv;
                    },
                    VPMBType::InterNearest => {
                        cur_mv = last_mv;
                    },
                    _ => { // GoldenMV
                        cur_mv = vp30_read_mv(br)?;
                    },
                };
                for _ in 0..4 {
                    self.blocks[self.blk_addr[cur_blk] >> 2].mv = cur_mv;
                    cur_blk += 1;
                }
            }
        }
        Ok(())
    }
    fn vp30_unpack_coeffs(&mut self, br: &mut BitReader, coef_no: usize, table: usize) -> DecoderResult<()> {
        if let Codes::VP30(ref codes) = self.codes {
            for blkaddr in self.blk_addr.iter() {
                let blk: &mut Block = &mut self.blocks[blkaddr >> 2];
                if !blk.coded || blk.idx != coef_no { continue; }
                if self.eob_run > 0 {
                    blk.idx = 64;
                    self.eob_run -= 1;
                    continue;
                }
                let cb = if coef_no == 0 {
                        &codes.dc_cb[table]
                    } else if blk.btype.is_intra() {
                        &codes.ac_i_cb[table]
                    } else {
                        &codes.ac_p_cb[table]
                    };
                let token                       = br.read_cb(cb)?;
                expand_token(blk, br, &mut self.eob_run, token)?;
            }
            Ok(())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn vp31_unpack_sb_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut has_uncoded = false;
        let mut has_partial = false;
        {
            let mut brun = BitRunDecoder::new(br, read_long_run)?;
            for sb in self.sb_info.iter_mut() {
                if brun.get_val(br)? {
                    *sb = SBState::Partial;
                    has_partial = true;
                } else {
                    *sb = SBState::Uncoded;
                    has_uncoded = true;
                }
            }
        }
        if has_uncoded {
            let mut brun = BitRunDecoder::new(br, read_long_run)?;
            let mut cur_blk = 0;
            for (sb, nblk) in self.sb_info.iter_mut().zip(self.sb_blocks.iter()) {
                let nblks = *nblk as usize;
                if *sb != SBState::Partial && brun.get_val(br)? {
                    *sb = SBState::Coded;
                    for _ in 0..nblks {
                        let blk_idx = self.blk_addr[cur_blk] >> 2;
                        self.blocks[blk_idx].coded = true;
                        cur_blk += 1;
                    }
                } else {
                    for _ in 0..nblks {
                        let blk_idx = self.blk_addr[cur_blk] >> 2;
                        self.blocks[blk_idx].coded = false;
                        cur_blk += 1;
                    }
                }
            }
        }
        if has_partial {
            let mut brun = BitRunDecoder::new(br, read_short_run)?;
            let mut cur_blk = 0;
            for (sb, nblk) in self.sb_info.iter_mut().zip(self.sb_blocks.iter()) {
                let nblks = *nblk as usize;
                if *sb == SBState::Partial {
                    for _ in 0..nblks {
                        let blk_idx = self.blk_addr[cur_blk] >> 2;
                        self.blocks[blk_idx].coded = brun.get_val(br)?;
                        cur_blk += 1;
                    }
                } else {
                    cur_blk += nblks;
                }
            }
        }
        Ok(())
    }
    fn vp31_unpack_mb_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut modes = [VPMBType::InterNoMV; 8];
        let alphabet                            = br.read(3)? as usize;
        let raw_modes = alphabet >= 7;
        if alphabet == 0 {
            for mode in VP31_DEFAULT_MB_MODES.iter() {
                modes[br.read(3)? as usize] = *mode;
            }
        } else if alphabet < 7 {
            modes.copy_from_slice(&VP31_MB_MODES[alphabet - 1]);
        }

        let mut cur_blk = 0;
        for (sb, nblk) in self.sb_info.iter_mut().zip(self.sb_blocks.iter()).take(self.y_sbs) {
            let nblks = *nblk as usize;
            if *sb == SBState::Uncoded {
                for _ in 0..nblks {
                    self.blocks[self.blk_addr[cur_blk] >> 2].btype = VPMBType::InterNoMV;
                    cur_blk += 1;
                }
            } else {
                for _ in 0..nblks/4 {
                    let mut coded = *sb == SBState::Coded;
                    if !coded {
                        for blk in 0..4 {
                            if self.blocks[self.blk_addr[cur_blk + blk] >> 2].coded {
                                coded = true;
                                break;
                            }
                        }
                    }
                    let mode = if !coded {
                            VPMBType::InterNoMV
                        } else if !raw_modes {
                            let code            = br.read_code(UintCodeType::LimitedUnary(7, 0))?;
                            modes[code as usize]
                        } else {
                                                VP31_DEFAULT_MB_MODES[br.read(3)? as usize]
                        };
                    for _ in 0..4 {
                        self.blocks[self.blk_addr[cur_blk] >> 2].btype = mode;
                        cur_blk += 1;
                    }
                }
            }
        }
        // replicate types for chroma
        let mut off_y = 0;
        let mut off_u = self.y_blocks;
        let mut off_v = off_u + self.mb_w * self.mb_h;
        for _blk_y in 0..self.mb_h {
            for blk_x in 0..self.mb_w {
                let btype = self.blocks[off_y + blk_x * 2].btype;
                self.blocks[off_u + blk_x].btype = btype;
                self.blocks[off_v + blk_x].btype = btype;
            }
            off_y += self.mb_w * 2 * 2;
            off_u += self.mb_w;
            off_v += self.mb_w;
        }
        Ok(())
    }
    fn vp31_unpack_mv_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut last_mv = ZERO_MV;
        let mut last2_mv = ZERO_MV;
        let read_mv                             = if br.read_bool()? { read_mv_raw } else { read_mv_packed };

        let mut cur_blk = 0;
        for _ in 0..self.y_blocks/4 {
            if self.blocks[self.blk_addr[cur_blk] >> 2].btype == VPMBType::InterFourMV {
                let a0 = self.blk_addr[cur_blk + 0] >> 2;
                let a1 = self.blk_addr[cur_blk + 1] >> 2;
                let a2 = self.blk_addr[cur_blk + 2] >> 2;
                let a3 = self.blk_addr[cur_blk + 3] >> 2;
                let first = a0.min(a1).min(a2).min(a3);
                let last  = a0.max(a1).max(a2).max(a3);
                self.blocks[first + 0].mv = (read_mv)(br)?;
                self.blocks[first + 1].mv = (read_mv)(br)?;
                self.blocks[last  - 1].mv = (read_mv)(br)?;
                self.blocks[last  + 0].mv = (read_mv)(br)?;
                last2_mv = last_mv;
                last_mv = self.blocks[last].mv;
                cur_blk += 4;
            } else {
                let cur_mv;
                match self.blocks[self.blk_addr[cur_blk] >> 2].btype {
                    VPMBType::Intra | VPMBType::InterNoMV | VPMBType::GoldenNoMV => {
                        cur_mv = ZERO_MV;
                    },
                    VPMBType::InterMV => {
                        cur_mv = (read_mv)(br)?;
                        last2_mv = last_mv;
                        last_mv = cur_mv;
                    },
                    VPMBType::InterNearest => {
                        cur_mv = last_mv;
                    },
                    VPMBType::InterNear => {
                        cur_mv = last2_mv;
                        std::mem::swap(&mut last_mv, &mut last2_mv);
                    },
                    _ => { // GoldenMV
                        cur_mv = (read_mv)(br)?;
                    },
                };
                for _ in 0..4 {
                    self.blocks[self.blk_addr[cur_blk] >> 2].mv = cur_mv;
                    cur_blk += 1;
                }
            }
        }
        Ok(())
    }
    fn vp31_unpack_coeffs(&mut self, br: &mut BitReader, coef_no: usize, table_y: usize, table_c: usize) -> DecoderResult<()> {
        if let Codes::VP31(ref codes) = self.codes {
            let cbs = if coef_no == 0 {
                    [&codes.dc_cb[table_y], &codes.dc_cb[table_c]]
                } else if coef_no < 6 {
                    [&codes.ac0_cb[table_y], &codes.ac0_cb[table_c]]
                } else if coef_no < 15 {
                    [&codes.ac1_cb[table_y], &codes.ac1_cb[table_c]]
                } else if coef_no < 28 {
                    [&codes.ac2_cb[table_y], &codes.ac2_cb[table_c]]
                } else {
                    [&codes.ac3_cb[table_y], &codes.ac3_cb[table_c]]
                };
            for blkaddr in self.blk_addr.iter() {
                let blk: &mut Block = &mut self.blocks[blkaddr >> 2];
                if !blk.coded || blk.idx != coef_no { continue; }
                if self.eob_run > 0 {
                    blk.idx = 64;
                    self.eob_run -= 1;
                    continue;
                }
                let cb = if (blkaddr & 3) == 0 { cbs[0] } else { cbs[1] };
                let token                       = br.read_cb(cb)?;
                expand_token(blk, br, &mut self.eob_run, token)?;
            }
            Ok(())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn vp40_unpack_sb_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut has_uncoded = false;

        if let Some(ref aux_codes) = self.aux_codes {
            for el in self.sb_info.iter_mut() {
                *el = SBState::Partial;
            }
            {
                let mut mbiter = self.mb_coded.iter_mut();
                let mut brun = BitRunDecoder::new(br, read_mb_run)?;
                for nmb in self.sb_mbs.iter() {
                    let nmbs = *nmb as usize;
                    for _ in 0..nmbs {
                        let coded               = brun.get_val(br)?;
                        *mbiter.next().unwrap() = coded;
                        has_uncoded |= !coded;
                    }
                }
            }
            if has_uncoded {
                let mut mbiter = self.mb_coded.iter().zip(self.mb_partial.iter_mut());
                let mut brun = BitRunDecoder::new(br, read_mb_run)?;
                for nmb in self.sb_mbs.iter() {
                    let nmbs = *nmb as usize;
                    for _ in 0..nmbs {
                        let (coded, partial) = mbiter.next().unwrap();
                        if *coded {
                            *partial = false;
                        } else {
                            *partial            = brun.get_val(br)?;
                        }
                    }
                }

                let mut bpmode = 0;
                let mut cur_blk = 0;
                for ((coded, partial), nblk) in self.mb_coded.iter().zip(self.mb_partial.iter()).zip(self.mb_blocks.iter()) {
                    let nblks = *nblk as usize;
                    if *coded {
                        for _ in 0..nblks {
                            let addr = self.blk_addr[cur_blk] >> 2;
                            self.blocks[addr].coded = true;
                            cur_blk += 1;
                        }
                    } else if !*partial {
                        for _ in 0..nblks {
                            let addr = self.blk_addr[cur_blk] >> 2;
                            self.blocks[addr].coded = false;
                            cur_blk += 1;
                        }
                    } else {
                        let mut pat             = br.read_cb(&aux_codes.mbpat_cb[bpmode])? + 1;
                        bpmode = VP40_BP_PREDICTOR[pat as usize] as usize;

                        let mut addrs = [0; 4];
                        for i in 0..nblks {
                            addrs[i] = self.blk_addr[cur_blk + i] >> 2;
                            for j in (0..i).rev() {
                                if addrs[j] > addrs[j + 1] {
                                    addrs.swap(j, j + 1);
                                }
                            }
                        }
                        for i in 0..nblks {
                            self.blocks[addrs[i]].coded = (pat & 8) != 0;
                            pat <<= 1;
                            cur_blk += 1;
                        }
                    }
                }
            }
            Ok(())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn vp40_unpack_mv_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut last_mv = ZERO_MV;
        let mut last2_mv = ZERO_MV;
        let mut last_mv_g = ZERO_MV;

        let mut cur_blk = 0;
        if let Some(ref codes) = self.aux_codes {
            for _ in 0..self.y_blocks/4 {
                if self.blocks[self.blk_addr[cur_blk] >> 2].btype == VPMBType::InterFourMV {
                    let x_cb = &codes.mv_x_cb[VP40_MV_LUT_INDEX[last_mv.x.unsigned_abs() as usize]];
                    let y_cb = &codes.mv_y_cb[VP40_MV_LUT_INDEX[last_mv.y.unsigned_abs() as usize]];
                    let x_sign = last_mv.x < 0;
                    let y_sign = last_mv.y < 0;
                    last2_mv = last_mv;
                    let saddr = (self.blk_addr[cur_blk] >> 2).min(self.blk_addr[cur_blk + 1] >> 2).min(self.blk_addr[cur_blk + 2] >> 2).min(self.blk_addr[cur_blk + 3] >> 2);
                    for i in 0..4 {
                        let blk = &mut self.blocks[saddr + (i & 1) + (i >> 1) * self.mb_w * 2];
                        blk.mv.x = i16::from(br.read_cb(x_cb)?);
                        if x_sign {
                            blk.mv.x = -blk.mv.x;
                        }
                        blk.mv.y = i16::from(br.read_cb(y_cb)?);
                        if y_sign {
                            blk.mv.y = -blk.mv.y;
                        }
                        last_mv = blk.mv;
                        cur_blk += 1;
                    }
                } else {
                    let cur_mv;
                    match self.blocks[self.blk_addr[cur_blk] >> 2].btype {
                        VPMBType::Intra | VPMBType::InterNoMV | VPMBType::GoldenNoMV => {
                            cur_mv = ZERO_MV;
                        },
                        VPMBType::InterMV => {
                            let x_cb = &codes.mv_x_cb[VP40_MV_LUT_INDEX[last_mv.x.unsigned_abs() as usize]];
                            let y_cb = &codes.mv_y_cb[VP40_MV_LUT_INDEX[last_mv.y.unsigned_abs() as usize]];
                            let x_sign = last_mv.x < 0;
                            let y_sign = last_mv.y < 0;
                            let x               = i16::from(br.read_cb(x_cb)?);
                            let y               = i16::from(br.read_cb(y_cb)?);
                            cur_mv = MV { x: if !x_sign { x } else { -x }, y: if !y_sign { y } else { -y } };
                            last2_mv = last_mv;
                            last_mv = cur_mv;
                        },
                        VPMBType::InterNearest => {
                            cur_mv = last_mv;
                        },
                        VPMBType::InterNear => {
                            cur_mv = last2_mv;
                            std::mem::swap(&mut last_mv, &mut last2_mv);
                        },
                        _ => { // GoldenMV
                            let x_cb = &codes.mv_x_cb[VP40_MV_LUT_INDEX[last_mv_g.x.unsigned_abs() as usize]];
                            let y_cb = &codes.mv_y_cb[VP40_MV_LUT_INDEX[last_mv_g.y.unsigned_abs() as usize]];
                            let x_sign = last_mv_g.x < 0;
                            let y_sign = last_mv_g.y < 0;
                            let x               = i16::from(br.read_cb(x_cb)?);
                            let y               = i16::from(br.read_cb(y_cb)?);
                            cur_mv = MV { x: if !x_sign { x } else { -x }, y: if !y_sign { y } else { -y } };
                            last_mv_g = cur_mv;
                        },
                    };
                    for _ in 0..4 {
                        self.blocks[self.blk_addr[cur_blk] >> 2].mv = cur_mv;
                        cur_blk += 1;
                    }
                }
            }
            Ok(())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn vp40_unpack_coeffs(&mut self, br: &mut BitReader, dc_table_y: usize, dc_table_c: usize, ac_table_y: usize, ac_table_c: usize) -> DecoderResult<()> {
        const VP40_PRED_MASKS: [usize; 16] = [ // top, bottom, left, right
                0b1010, 0b1010, 0b1000, 0b1011,
                0b1010, 0b1010, 0b0010, 0b1110,
                0b0010, 0b1010, 0b0010, 0b0110,
                0b0100, 0b0111, 0b1110, 0b1110
            ];
        self.last_dc = [0; 3];
        if let Codes::VP31(ref codes) = self.codes {
            let mut coef_eob: [usize; 64] = [0; 64];
            for (blkaddr, bsub) in self.blk_addr.iter().zip(self.blk_sub.iter()) {
                let blk: &mut Block = &mut self.blocks[blkaddr >> 2];
                if !blk.coded { continue; }
                let mut idx = blkaddr >> 2;
                let (bx, by) = if (blkaddr & 3) == 0 {
                        (idx % (self.mb_w * 2), idx / (self.mb_w * 2))
                    } else {
                        idx -= self.mb_w * self.mb_h * 4;
                        if idx >= self.mb_w * self.mb_h {
                            idx -= self.mb_w * self.mb_h;
                        }
                        (idx % self.mb_w, idx / self.mb_w)
                    };
                while blk.idx < 64 {
                    if coef_eob[blk.idx] > 0 {
                        coef_eob[blk.idx] -= 1;
                        blk.idx = 64;
                        continue;
                    }
                    let cbs = if blk.idx == 0 {
                            [&codes.dc_cb[dc_table_y], &codes.dc_cb[dc_table_c]]
                        } else if blk.idx < 6 {
                            [&codes.ac0_cb[ac_table_y], &codes.ac0_cb[ac_table_c]]
                        } else if blk.idx < 15 {
                            [&codes.ac1_cb[ac_table_y], &codes.ac1_cb[ac_table_c]]
                        } else if blk.idx < 28 {
                            [&codes.ac2_cb[ac_table_y], &codes.ac2_cb[ac_table_c]]
                        } else {
                            [&codes.ac3_cb[ac_table_y], &codes.ac3_cb[ac_table_c]]
                        };
                    let cb = if (blkaddr & 3) == 0 { cbs[0] } else { cbs[1] };
                    let token                   = br.read_cb(cb)?;
                    expand_token(blk, br, &mut coef_eob[blk.idx], token)?;
                    if blk.idx == 64 { break; }
                }
                let idx = blkaddr >> 2;
                let mask = VP40_PRED_MASKS[*bsub as usize];
                if (blkaddr & 3) == 0 {
                    let pred_dc = self.vp40_predict_dc(bx, by, self.mb_w * 2, self.mb_h * 2, idx, mask, true);
                    let blk: &mut Block = &mut self.blocks[blkaddr >> 2];
                    blk.coeffs[0] += pred_dc;
                    self.last_dc[blk.btype.get_ref_id() as usize] = blk.coeffs[0];
                } else {
                    let pred_dc = self.vp40_predict_dc(bx, by, self.mb_w, self.mb_h, idx, mask, false);
                    let blk: &mut Block = &mut self.blocks[blkaddr >> 2];
                    blk.coeffs[0] += pred_dc;
                    self.last_dc[blk.btype.get_ref_id() as usize] = blk.coeffs[0];
                }
            }
            Ok(())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn decode_vp30(&mut self, br: &mut BitReader, frm: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        for blk in self.blocks.iter_mut() {
            blk.coeffs = [0; 64];
            blk.idx = 0;
            blk.coded = false;
            blk.has_ac = false;
        }
        if self.is_intra {
            for sb in self.sb_info.iter_mut() { *sb = SBState::Coded; }
            for blk in self.blocks.iter_mut() {
                blk.btype = VPMBType::Intra;
                blk.coded = true;
            }
        } else {
            if self.shuf.get_last().is_none() || self.shuf.get_golden().is_none() {
                return Err(DecoderError::MissingReference);
            }
            self.vp30_unpack_sb_info(br)?;
            self.vp30_unpack_mb_info(br)?;
            self.vp30_unpack_mv_info(br)?;
        }
        let dc_quant = VP30_DC_SCALES[self.quant] * 10;
        let ac_quant = VP30_AC_SCALES[self.quant];

        self.update_gf = ac_quant <= 50;

        rescale_qmat(&mut self.qmat_y, VP3_QMAT_Y, dc_quant, ac_quant, 2);
        rescale_qmat(&mut self.qmat_c, VP3_QMAT_C, dc_quant, ac_quant, 2);
        rescale_qmat(&mut self.qmat_y_p, VP3_QMAT_INTER, dc_quant, ac_quant, 4);
        if self.quant == 10 {
            self.qmat_y[29] = 980;
            self.qmat_y[58] = 1636;
            self.qmat_y[59] = 1964;
        } else if self.quant == 31 {
            self.qmat_y[58] = 456;
        } else if self.quant == 44 {
            self.qmat_y[58] = 224;
        }
        self.qmat_c_p.copy_from_slice(&self.qmat_y_p);

        let table = if ac_quant <= 50 {
                0
            } else if ac_quant <= 150 {
                1
            } else if ac_quant <= 300 {
                2
            } else if ac_quant <= 600 {
                3
            } else {
                4
            };

        self.eob_run = 0;
        self.vp30_unpack_coeffs(br, 0, table)?;
        let mut last_dc_i = 0;
        let mut last_dc_p = 0;
        for blkaddr in self.blk_addr.iter() {
            let blk: &mut Block = &mut self.blocks[blkaddr >> 2];
            if !blk.coded { continue; }
            if blk.btype.is_intra() {
                blk.coeffs[0] += last_dc_i;
                last_dc_i = blk.coeffs[0];
            } else {
                blk.coeffs[0] += last_dc_p;
                last_dc_p = blk.coeffs[0];
            }
        }

        for coef_no in 1..64 {
            self.vp30_unpack_coeffs(br, coef_no, table)?;
        }

        if self.is_intra {
            self.output_blocks_intra(frm);
        } else {
            self.output_blocks_inter(frm);
        }

        Ok(())
    }
    fn decode_vp31(&mut self, br: &mut BitReader, frm: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        for blk in self.blocks.iter_mut() {
            blk.coeffs = [0; 64];
            blk.idx = 0;
            blk.coded = false;
            blk.has_ac = false;
        }
        if self.is_intra {
            for sb in self.sb_info.iter_mut() { *sb = SBState::Coded; }
            for blk in self.blocks.iter_mut() {
                blk.btype = VPMBType::Intra;
                blk.coded = true;
            }
        } else {
            if self.shuf.get_last().is_none() || self.shuf.get_golden().is_none() {
                return Err(DecoderError::MissingReference);
            }
            self.vp31_unpack_sb_info(br)?;
            self.vp31_unpack_mb_info(br)?;
            self.vp31_unpack_mv_info(br)?;
        }
        let dc_quant = VP31_DC_SCALES[self.quant];
        let ac_quant = VP31_AC_SCALES[self.quant];
        rescale_qmat(&mut self.qmat_y, VP3_QMAT_Y, dc_quant, ac_quant, 2);
        rescale_qmat(&mut self.qmat_c, VP3_QMAT_C, dc_quant, ac_quant, 2);
        rescale_qmat(&mut self.qmat_y_p, VP3_QMAT_INTER, dc_quant, ac_quant, 4);
        rescale_qmat(&mut self.qmat_c_p, VP3_QMAT_INTER, dc_quant, ac_quant, 4);

        self.eob_run = 0;
        let dc_table_y                          = br.read(4)? as usize;
        let dc_table_c                          = br.read(4)? as usize;
        self.vp31_unpack_coeffs(br, 0, dc_table_y, dc_table_c)?;
        self.restore_dcs();

        let ac_table_y                          = br.read(4)? as usize;
        let ac_table_c                          = br.read(4)? as usize;
        for coef_no in 1..64 {
            self.vp31_unpack_coeffs(br, coef_no, ac_table_y, ac_table_c)?;
        }

        if self.is_intra {
            self.output_blocks_intra(frm);
        } else {
            self.output_blocks_inter(frm);
        }
        if self.loop_str > 0 {
            self.vp31_loop_filter(frm);
        }

        Ok(())
    }
    fn decode_vp4(&mut self, br: &mut BitReader, frm: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        for blk in self.blocks.iter_mut() {
            blk.coeffs = [0; 64];
            blk.idx = 0;
            blk.coded = false;
            blk.has_ac = false;
        }
        if self.is_intra {
            for sb in self.sb_info.iter_mut() { *sb = SBState::Coded; }
            for blk in self.blocks.iter_mut() {
                blk.btype = VPMBType::Intra;
                blk.coded = true;
            }
        } else {
            if self.shuf.get_last().is_none() || self.shuf.get_golden().is_none() {
                return Err(DecoderError::MissingReference);
            }
            self.vp40_unpack_sb_info(br)?;
            self.vp31_unpack_mb_info(br)?;
            self.vp40_unpack_mv_info(br)?;
        }
        let dc_y_quant = VP40_DC_Y_SCALES[self.quant];
        let dc_c_quant = VP40_DC_C_SCALES[self.quant];
        let ac_quant = VP40_AC_SCALES[self.quant];
        rescale_qmat_vp4(&mut self.qmat_y,   VP40_QMAT, dc_y_quant, ac_quant, true);
        rescale_qmat_vp4(&mut self.qmat_y_p, VP40_QMAT, dc_c_quant, ac_quant, false);
        self.qmat_c.copy_from_slice(&self.qmat_y);
        self.qmat_c_p.copy_from_slice(&self.qmat_y_p);

        self.eob_run = 0;
        let dc_table_y                          = br.read(4)? as usize;
        let dc_table_c                          = br.read(4)? as usize;
        let ac_table_y                          = br.read(4)? as usize;
        let ac_table_c                          = br.read(4)? as usize;
        self.vp40_unpack_coeffs(br, dc_table_y, dc_table_c, ac_table_y, ac_table_c)?;

        if self.is_intra {
            self.output_blocks_intra(frm);
        } else {
            self.output_blocks_inter(frm);
        }

        Ok(())
    }
    fn vp31_predict_dc(&self, bx: usize, by: usize, bw: usize, blk_idx: usize) -> i16 {
        let mut preds = [0i32; 4];
        let mut pp: usize = 0;
        let ref_id = self.blocks[blk_idx].btype.get_ref_id();
        let is_right = bx == bw - 1;
        if bx > 0 {
            fill_dc_pred!(self, ref_id, preds, pp, 0, blk_idx - 1);
            if by > 0 {
                fill_dc_pred!(self, ref_id, preds, pp, 1, blk_idx - 1 - bw);
            }
        }
        if by > 0 {
            fill_dc_pred!(self, ref_id, preds, pp, 2, blk_idx - bw);
            if !is_right {
                fill_dc_pred!(self, ref_id, preds, pp, 3, blk_idx + 1 - bw);
            }
        }
        if pp == 0 { return self.last_dc[ref_id as usize]; }
        let mut pred = 0i32;
        for i in 0..4 {
            if (pp & (1 << i)) != 0 {
                pred += preds[i] * i32::from(VP31_DC_WEIGHTS[pp][i]);
            }
        }
        pred /= i32::from(VP31_DC_WEIGHTS[pp][4]);
        if (pp & 7) == 7 {
            if (pred - preds[2]).abs() > 128 { return preds[2] as i16; }
            if (pred - preds[0]).abs() > 128 { return preds[0] as i16; }
            if (pred - preds[1]).abs() > 128 { return preds[1] as i16; }
        }
        pred as i16
    }
    fn vp40_predict_dc(&self, bx: usize, by: usize, bw: usize, bh: usize, blk_idx: usize, mask: usize, is_luma: bool) -> i16 {
        let mut preds = [0i32; 4];
        let mut pp: usize = 0;
        let ref_id = self.blocks[blk_idx].btype.get_ref_id();
        if (by > 0)      && (((mask >> 3) & 1) == 1) { //top
            fill_dc_pred!(self, ref_id, preds, pp, 0, blk_idx - bw);
        }
        if (by < bh - 1) && (((mask >> 2) & 1) == 1) { //bottom
            fill_dc_pred!(self, ref_id, preds, pp, 1, blk_idx + bw);
        }
        if (bx > 0)      && (((mask >> 1) & 1) == 1) { //left
            fill_dc_pred!(self, ref_id, preds, pp, 2, blk_idx - 1);
        }
        if (is_luma && bx == 0 && (by >= 4)) && (((mask >> 1) & 1) == 1) { //left wrap
            fill_dc_pred!(self, ref_id, preds, pp, 2, blk_idx - 1 - 3 * bw);
        }
        if (bx < bw - 1) && (((mask >> 0) & 1) == 1) { //right
            fill_dc_pred!(self, ref_id, preds, pp, 3, blk_idx + 1);
        }
        if pp == 0 { return self.last_dc[ref_id as usize]; }
        let mut pred = 0i32;
        let mut npred = 0;
        for i in 0..4 {
            if (pp & (1 << i)) != 0 {
                pred += preds[i];
                npred += 1;
                if npred == 2 {
                    return (pred / 2) as i16;
                }
            }
        }
        self.last_dc[ref_id as usize]
    }
    fn restore_dcs(&mut self) {
        let blk_stride = self.mb_w * 2;
        let mut blk_idx = 0;
        self.last_dc = [0; 3];
        for by in 0..self.mb_h*2 {
            for bx in 0..self.mb_w*2 {
                if !self.blocks[blk_idx + bx].coded { continue; }
                let dc = self.vp31_predict_dc(bx, by, self.mb_w*2, blk_idx + bx);
                self.blocks[blk_idx + bx].coeffs[0] += dc;
                self.last_dc[self.blocks[blk_idx + bx].btype.get_ref_id() as usize] = self.blocks[blk_idx + bx].coeffs[0];
            }
            blk_idx += blk_stride;
        }
        let blk_stride = self.mb_w;
        for _plane in 1..3 {
            self.last_dc = [0; 3];
            for by in 0..self.mb_h {
                for bx in 0..self.mb_w {
                    if !self.blocks[blk_idx + bx].coded { continue; }
                    let dc = self.vp31_predict_dc(bx, by, self.mb_w, blk_idx + bx);
                    self.blocks[blk_idx + bx].coeffs[0] += dc;
                    self.last_dc[self.blocks[blk_idx + bx].btype.get_ref_id() as usize] = self.blocks[blk_idx + bx].coeffs[0];
                }
                blk_idx += blk_stride;
            }
        }
    }
    fn output_blocks_intra(&mut self, frm: &mut NASimpleVideoFrame<u8>) {
        let mut biter = self.blocks.iter_mut();
        for by in 0..self.mb_h*2 {
            for bx in 0..self.mb_w*2 {
                let mut blk = biter.next().unwrap();
                let qmat = &self.qmat_y;
                blk.coeffs[0] *= qmat[0];
                if blk.has_ac {
                    unquant(&mut blk.coeffs, qmat);
                    vp_put_block(&mut blk.coeffs, bx, by, 0, frm);
                } else {
                    vp_put_block_dc(&mut blk.coeffs, bx, by, 0, frm);
                }
            }
        }
        for plane in 1..3 {
            for by in 0..self.mb_h {
                for bx in 0..self.mb_w {
                    let mut blk = biter.next().unwrap();
                    let qmat = &self.qmat_c;
                    blk.coeffs[0] *= qmat[0];
                    if blk.has_ac {
                        unquant(&mut blk.coeffs, qmat);
                        vp_put_block(&mut blk.coeffs, bx, by, plane, frm);
                    } else {
                        vp_put_block_dc(&mut blk.coeffs, bx, by, plane, frm);
                    }
                }
            }
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn output_blocks_inter(&mut self, frm: &mut NASimpleVideoFrame<u8>) {
        let mut blk_idx = 0;
        let bstride = self.mb_w * 2;
        for by in (0..self.mb_h*2).step_by(2) {
            for bx in (0..self.mb_w*2).step_by(2) {
                if self.blocks[blk_idx + bx].btype != VPMBType::InterFourMV {
                    continue;
                }
                let mvs = [ self.blocks[blk_idx + bx].mv,
                            self.blocks[blk_idx + bx + 1].mv,
                            self.blocks[blk_idx + bx     + bstride].mv,
                            self.blocks[blk_idx + bx + 1 + bstride].mv ];
                let mut mv_sum = mvs[0] + mvs[1] + mvs[2] + mvs[3];
                mv_sum.x = (mv_sum.x + 2) >> 2;
                mv_sum.y = (mv_sum.y + 2) >> 2;

                let src = self.shuf.get_last().unwrap();
                for i in 0..4 {
                    let xoff = (i &  1) * 8;
                    let yoff = (i >> 1) * 8;

                    let mode = vp3_mv_mode(mvs[i].x, mvs[i].y);
                    if self.version != 4 {
                        copy_block(frm, src.clone(), 0, bx * 8 + xoff, by * 8 + yoff,
                                   mvs[i].x >> 1, mvs[i].y >> 1, 8, 8, 0, 1, mode, VP3_INTERP_FUNCS);
                    } else {
                        vp_copy_block(frm, src.clone(), 0, bx * 8 + xoff, by * 8 + yoff,
                                      mvs[i].x >> 1, mvs[i].y >> 1, 0, 1, self.loop_str,
                                      mode, VP3_INTERP_FUNCS, self.mc_buf.clone());
                    }
                }

                let mx = (mv_sum.x >> 1) | (mv_sum.x & 1);
                let my = (mv_sum.y >> 1) | (mv_sum.y & 1);
                let mode = vp3_mv_mode(mx, my);
                copy_block(frm, src.clone(), 1, bx * 4, by * 4, mx >> 1, my >> 1, 8, 8, 0, 1, mode, VP3_INTERP_FUNCS);
                copy_block(frm, src.clone(), 2, bx * 4, by * 4, mx >> 1, my >> 1, 8, 8, 0, 1, mode, VP3_INTERP_FUNCS);
            }
            blk_idx += bstride * 2;
        }

        let mut biter = self.blocks.iter_mut();
        for by in 0..self.mb_h*2 {
            for bx in 0..self.mb_w*2 {
                let mut blk = biter.next().unwrap();
                // do MC for whole macroblock
                if !blk.btype.is_intra() && (((bx | by) & 1) == 0) && (blk.btype != VPMBType::InterFourMV) {
                    let src = if blk.btype.get_ref_id() == 1 {
                            self.shuf.get_last().unwrap()
                        } else {
                            self.shuf.get_golden().unwrap()
                        };
                    let mode = vp3_mv_mode(blk.mv.x, blk.mv.y);
                    if self.version != 4 {
                        copy_block(frm, src.clone(), 0, bx * 8, by * 8,
                                   blk.mv.x >> 1, blk.mv.y >> 1, 16, 16, 0, 1, mode, VP3_INTERP_FUNCS);
                    } else {
                        vp_copy_block(frm, src.clone(), 0, bx * 8, by * 8,
                                      blk.mv.x >> 1, blk.mv.y >> 1, 0, 1, self.loop_str,
                                      mode, VP3_INTERP_FUNCS, self.mc_buf.clone());
                        vp_copy_block(frm, src.clone(), 0, bx * 8 + 8, by * 8,
                                      blk.mv.x >> 1, blk.mv.y >> 1, 0, 1, self.loop_str,
                                      mode, VP3_INTERP_FUNCS, self.mc_buf.clone());
                        vp_copy_block(frm, src.clone(), 0, bx * 8, by * 8 + 8,
                                      blk.mv.x >> 1, blk.mv.y >> 1, 0, 1, self.loop_str,
                                      mode, VP3_INTERP_FUNCS, self.mc_buf.clone());
                        vp_copy_block(frm, src.clone(), 0, bx * 8 + 8, by * 8 + 8,
                                      blk.mv.x >> 1, blk.mv.y >> 1, 0, 1, self.loop_str,
                                      mode, VP3_INTERP_FUNCS, self.mc_buf.clone());
                    }
                    let mx = (blk.mv.x >> 1) | (blk.mv.x & 1);
                    let my = (blk.mv.y >> 1) | (blk.mv.y & 1);
                    let mode = vp3_mv_mode(mx, my);
                    if self.version != 4 {
                        copy_block(frm, src.clone(), 1, bx * 4, by * 4,
                                   mx >> 1, my >> 1, 8, 8, 0, 1, mode, VP3_INTERP_FUNCS);
                        copy_block(frm, src.clone(), 2, bx * 4, by * 4,
                                   mx >> 1, my >> 1, 8, 8, 0, 1, mode, VP3_INTERP_FUNCS);
                    } else {
                        vp_copy_block(frm, src.clone(), 1, bx * 4, by * 4,
                                      mx >> 1, my >> 1, 0, 1, self.loop_str,
                                      mode, VP3_INTERP_FUNCS, self.mc_buf.clone());
                        vp_copy_block(frm, src.clone(), 2, bx * 4, by * 4,
                                      mx >> 1, my >> 1, 0, 1, self.loop_str,
                                      mode, VP3_INTERP_FUNCS, self.mc_buf.clone());
                    }
                }
                let qmat = if blk.btype.is_intra() { &self.qmat_y } else { &self.qmat_y_p };
                blk.coeffs[0] *= qmat[0];
                if blk.has_ac {
                    unquant(&mut blk.coeffs, qmat);
                }
                if !blk.coded {
                    copy_block(frm, self.shuf.get_last().unwrap(), 0, bx * 8, by * 8, 0, 0, 8, 8, 0, 1, 0, VP3_INTERP_FUNCS);
                } else if blk.btype.is_intra() {
                    if blk.has_ac {
                        vp_put_block(&mut blk.coeffs, bx, by, 0, frm);
                    } else {
                        vp_put_block_dc(&mut blk.coeffs, bx, by, 0, frm);
                    }
                } else {
                    if blk.has_ac {
                        vp_add_block(&mut blk.coeffs, bx, by, 0, frm);
                    } else {
                        vp_add_block_dc(&mut blk.coeffs, bx, by, 0, frm);
                    }
                }
            }
        }
        for plane in 1..3 {
            for by in 0..self.mb_h {
                for bx in 0..self.mb_w {
                    let mut blk = biter.next().unwrap();
                    let qmat = if blk.btype.is_intra() { &self.qmat_c } else { &self.qmat_c_p };
                    blk.coeffs[0] *= qmat[0];
                    if blk.has_ac {
                        unquant(&mut blk.coeffs, qmat);
                    }
                    if !blk.coded {
                        copy_block(frm, self.shuf.get_last().unwrap(), plane, bx * 8, by * 8, 0, 0, 8, 8, 0, 1, 0, VP3_INTERP_FUNCS);
                    } else if blk.btype.is_intra() {
                        if blk.has_ac {
                            vp_put_block(&mut blk.coeffs, bx, by, plane, frm);
                        } else {
                            vp_put_block_dc(&mut blk.coeffs, bx, by, plane, frm);
                        }
                    } else {
                        if blk.has_ac {
                            vp_add_block(&mut blk.coeffs, bx, by, plane, frm);
                        } else {
                            vp_add_block_dc(&mut blk.coeffs, bx, by, plane, frm);
                        }
                    }
                }
            }
        }
    }
    fn vp31_loop_filter(&mut self, frm: &mut NASimpleVideoFrame<u8>) {
        let mut blk_idx = 0;
        let blk_w = self.mb_w * 2;
        for by in 0..self.mb_h*2 {
            for bx in 0..blk_w {
                let blk = &self.blocks[blk_idx + bx];
                if (bx > 0) && blk.coded {
                    vp31_loop_filter_v(frm, bx * 8, by * 8, 0, self.loop_str);
                }
                if (by > 0) && blk.coded {
                    vp31_loop_filter_h(frm, bx * 8, by * 8, 0, self.loop_str);
                }
                if (bx < blk_w - 1) && !self.blocks[blk_idx + bx + 1].coded {
                    vp31_loop_filter_v(frm, bx * 8 + 8, by * 8, 0, self.loop_str);
                }
                if (by < self.mb_h * 2 - 1) && !self.blocks[blk_idx + bx + blk_w].coded {
                    vp31_loop_filter_h(frm, bx * 8, by * 8 + 8, 0, self.loop_str);
                }
            }
            blk_idx += blk_w;
        }
        let blk_w = self.mb_w;
        for plane in 1..3 {
            for by in 0..self.mb_h {
                for bx in 0..self.mb_w {
                    let blk = &self.blocks[blk_idx + bx];
                    if (bx > 0) && blk.coded {
                        vp31_loop_filter_v(frm, bx * 8, by * 8, plane, self.loop_str);
                    }
                    if (by > 0) && blk.coded {
                        vp31_loop_filter_h(frm, bx * 8, by * 8, plane, self.loop_str);
                    }
                    if (bx < blk_w - 1) && !self.blocks[blk_idx + bx + 1].coded {
                        vp31_loop_filter_v(frm, bx * 8 + 8, by * 8, plane, self.loop_str);
                    }
                    if (by < self.mb_h - 1) && !self.blocks[blk_idx + bx + blk_w].coded {
                        vp31_loop_filter_h(frm, bx * 8, by * 8 + 8, plane, self.loop_str);
                    }
                }
                blk_idx += blk_w;
            }
        }
    }
    fn generate_block_addr(&mut self) {
        let sb_w_y = (self.width         + 31) >> 5;
        let sb_h_y = (self.height        + 31) >> 5;
        let sb_w_c = ((self.width  >> 1) + 31) >> 5;
        let sb_h_c = ((self.height >> 1) + 31) >> 5;
        self.y_sbs = sb_w_y * sb_h_y;
        let tot_sb = sb_w_y * sb_h_y + 2 * sb_w_c * sb_h_c;
        let tot_mb = self.mb_w * self.mb_h * 2 + ((self.mb_w + 1) & !1) * ((self.mb_h + 1) & !1) * 2;
        let bw = self.width >> 3;
        let bh = self.height >> 3;
        let tot_blk = bw * bh * 3 / 2;
        self.sb_info.resize(tot_sb, SBState::Uncoded);
        self.sb_blocks = Vec::with_capacity(tot_sb);
        self.mb_blocks = Vec::with_capacity(tot_mb);
        self.sb_mbs = Vec::with_capacity(tot_sb);
        self.blk_addr = Vec::with_capacity(tot_blk);
        self.blk_sub = Vec::with_capacity(tot_blk);
        self.y_blocks = bw * bh;
        let mut base_idx = 0;
        for plane in 0..3 {
            let w = if plane > 0 { self.width  >> 1 } else { self.width };
            let h = if plane > 0 { self.height >> 1 } else { self.height };
            let sb_w = (w + 31) >> 5;
            let sb_h = (h + 31) >> 5;
            let blk_w = w >> 3;
            let blk_h = h >> 3;
            for y in 0..sb_h {
                for x in 0..sb_w {
                    let mut nmbs = 0;
                    for mb_no in 0..4 {
                        let bx = x * 4 + HILBERT_ORDER[mb_no * 4][0];
                        let by = y * 4 + HILBERT_ORDER[mb_no * 4][1];
                        if (bx >= blk_w) || (by >= blk_h) { continue; }
                        let mut nblocks = 0;
                        for blk_no in 0..4 {
                            let bx = x * 4 + HILBERT_ORDER[mb_no * 4 + blk_no][0];
                            let by = y * 4 + HILBERT_ORDER[mb_no * 4 + blk_no][1];
                            if (bx >= blk_w) || (by >= blk_h) { continue; }
                            nblocks += 1;
                        }
                        self.mb_blocks.push(nblocks);
                        nmbs += 1;
                    }
                    self.sb_mbs.push(nmbs);

                    let mut nblocks = 0;
                    for blk_no in 0..16 {
                        let bx = x * 4 + HILBERT_ORDER[blk_no][0];
                        let by = y * 4 + HILBERT_ORDER[blk_no][1];
                        if (bx >= blk_w) || (by >= blk_h) { continue; }
                        let idx = base_idx + bx + by * blk_w;
                        self.blk_addr.push(idx * 4 + plane);
                        self.blk_sub.push(blk_no as u8);
                        nblocks += 1;
                    }
                    self.sb_blocks.push(nblocks);
                }
            }
            base_idx += blk_w * blk_h;
        }
        self.blocks.resize(tot_blk, Block::new());
        self.mb_coded.resize(tot_mb, false);
        self.mb_partial.resize(tot_mb, false);
    }
}

impl NADecoder for VP34Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = YUV420_FORMAT;
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            validate!(((self.width | self.height) & 15) == 0);
            self.mb_w   = self.width  >> 4;
            self.mb_h   = self.height >> 4;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), true, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            supp.pool_u8.set_dec_bufs(3);
            supp.pool_u8.prealloc_video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), true, fmt), 4)?;

            self.generate_block_addr();
            if self.version == 4 {
                self.codes = Codes::VP31(VP31Codes::new_vp4());
                self.aux_codes = Some(VP40AuxCodes::new());
            }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 0);
        let mut br = BitReader::new(&src, BitReaderMode::BE);

        self.parse_header(&mut br)?;
        if self.is_intra {
            self.shuf.clear();
        } else {
            if !self.shuf.has_refs() {
                return Err(DecoderError::MissingReference);
            }
        }

        let ret = supp.pool_u8.get_free();
        if ret.is_none() {
            return Err(DecoderError::AllocError);
        }
        let mut buf = ret.unwrap();
        let mut dframe = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
        match self.version {
            30 => self.decode_vp30(&mut br, &mut dframe)?,
            31 => self.decode_vp31(&mut br, &mut dframe)?,
             4 => self.decode_vp4(&mut br, &mut dframe)?,
             _ => return Err(DecoderError::Bug),
        }

        if self.is_intra || self.update_gf {
            self.shuf.add_golden_frame(buf.clone());
        }
        self.shuf.add_frame(buf.clone());

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(self.is_intra);
        frm.set_frame_type(if self.is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.shuf.clear();
    }
}

impl NAOptionHandler for VP34Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_vp3() -> Box<dyn NADecoder + Send> {
    Box::new(VP34Decoder::new(3))
}

pub fn get_decoder_vp4() -> Box<dyn NADecoder + Send> {
    Box::new(VP34Decoder::new(4))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;

    #[test]
    fn test_vp30() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/VP3/vp30-logo.avi
        test_decoding("avi", "vp3", "assets/Duck/vp30-logo.avi", Some(23), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x51aba7df, 0x6e42534d, 0xef6c5b13, 0x26c38d1f]));
    }

    #[test]
    fn test_vp31() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

//        let file = "assets/Duck/vp31.avi";
//        let file = "assets/Duck/vp31_crash.avi";
//        let file = "assets/Duck/01-vp31-0500.avi";
//        test_file_decoding("avi", file, Some(3), true, false, None/*Some("vp31")*/, &dmx_reg, &dec_reg);
//panic!("end");
        // sample: https://samples.mplayerhq.hu/V-codecs/VP3/01-vp31-0500.avi
        test_decoding("avi", "vp3", "assets/Duck/01-vp31-0500.avi", Some(16), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x65112f7e, 0x2914f29b, 0x2908ed2f, 0xce5fc8c5]));
    }

    #[test]
    fn test_vp4() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/VP4/ot171_vp40.avi
        test_decoding("avi", "vp3", "assets/Duck/ot171_vp40.avi", Some(86), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xd41d8cd9, 0x8f00b204, 0xe9800998, 0xecf8427e]));
    }
}

const HILBERT_ORDER: [[usize; 2]; 16] = [
    [ 0, 0 ], [ 1, 0 ], [ 1, 1 ], [ 0, 1 ],
    [ 0, 2 ], [ 0, 3 ], [ 1, 3 ], [ 1, 2 ],
    [ 2, 2 ], [ 2, 3 ], [ 3, 3 ], [ 3, 2 ],
    [ 3, 1 ], [ 2, 1 ], [ 2, 0 ], [ 3, 0 ]
];

const VP31_LOOP_STRENGTH: [i16; 64] = [
    30, 25, 20, 20, 15, 15, 14, 14,
    13, 13, 12, 12, 11, 11, 10, 10,
     9,  9,  8,  8,  7,  7,  7,  7,
     6,  6,  6,  6,  5,  5,  5,  5,
     4,  4,  4,  4,  3,  3,  3,  3,
     2,  2,  2,  2,  2,  2,  2,  2,
     0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0
];

const VP31_DEFAULT_MB_MODES: [VPMBType; 8] = [
    VPMBType::InterNoMV,    VPMBType::Intra,        VPMBType::InterMV,      VPMBType::InterNearest,
    VPMBType::InterNear,    VPMBType::GoldenNoMV,   VPMBType::GoldenMV,     VPMBType::InterFourMV
];

const VP31_MB_MODES: [[VPMBType; 8]; 6] = [
  [
    VPMBType::InterNearest, VPMBType::InterNear,    VPMBType::InterMV,      VPMBType::InterNoMV,
    VPMBType::Intra,        VPMBType::GoldenNoMV,   VPMBType::GoldenMV,     VPMBType::InterFourMV
  ], [
    VPMBType::InterNearest, VPMBType::InterNear,    VPMBType::InterNoMV,    VPMBType::InterMV,
    VPMBType::Intra,        VPMBType::GoldenNoMV,   VPMBType::GoldenMV,     VPMBType::InterFourMV
  ], [
    VPMBType::InterNearest, VPMBType::InterMV,      VPMBType::InterNear,    VPMBType::InterNoMV,
    VPMBType::Intra,        VPMBType::GoldenNoMV,   VPMBType::GoldenMV,     VPMBType::InterFourMV
  ], [
    VPMBType::InterNearest, VPMBType::InterMV,      VPMBType::InterNoMV,    VPMBType::InterNear,
    VPMBType::Intra,        VPMBType::GoldenNoMV,   VPMBType::GoldenMV,     VPMBType::InterFourMV
  ], [
    VPMBType::InterNoMV,    VPMBType::InterNearest, VPMBType::InterNear,    VPMBType::InterMV,
    VPMBType::Intra,        VPMBType::GoldenNoMV,   VPMBType::GoldenMV,     VPMBType::InterFourMV
  ], [
    VPMBType::InterNoMV,    VPMBType::GoldenNoMV,   VPMBType::InterNearest, VPMBType::InterNear,
    VPMBType::InterMV,      VPMBType::Intra,        VPMBType::GoldenMV,     VPMBType::InterFourMV
  ]
];

const VP3_LITERAL_BASE: [i16; 6] = [ 7, 9, 13, 21, 37, 69 ];

const VP31_AC_SCALES: [i16; 64] = [
    500, 450, 400, 370, 340, 310, 285, 265,
    245, 225, 210, 195, 185, 180, 170, 160,
    150, 145, 135, 130, 125, 115, 110, 107,
    100,  96,  93,  89,  85,  82,  75,  74,
     70,  68,  64,  60,  57,  56,  52,  50,
     49,  45,  44,  43,  40,  38,  37,  35,
     33,  32,  30,  29,  28,  25,  24,  22,
     21,  19,  18,  17,  15,  13,  12,  10
];

const VP31_DC_SCALES: [i16; 64] = [
    220, 200, 190, 180, 170, 170, 160, 160,
    150, 150, 140, 140, 130, 130, 120, 120,
    110, 110, 100, 100,  90,  90,  90,  80,
     80,  80,  70,  70,  70,  60,  60,  60,
     60,  50,  50,  50,  50,  40,  40,  40,
     40,  40,  30,  30,  30,  30,  30,  30,
     30,  20,  20,  20,  20,  20,  20,  20,
     20,  10,  10,  10,  10,  10,  10,  10
];

const VP3_QMAT_Y: &[i16; 64] = &[
    16,  11,  10,  16,  24,  40,  51,  61,
    12,  12,  14,  19,  26,  58,  60,  55,
    14,  13,  16,  24,  40,  57,  69,  56,
    14,  17,  22,  29,  51,  87,  80,  62,
    18,  22,  37,  58,  68, 109, 103,  77,
    24,  35,  55,  64,  81, 104, 113,  92,
    49,  64,  78,  87, 103, 121, 120, 101,
    72,  92,  95,  98, 112, 100, 103,  99
];

const VP3_QMAT_C: &[i16; 64] = &[
    17, 18, 24, 47, 99, 99, 99, 99,
    18, 21, 26, 66, 99, 99, 99, 99,
    24, 26, 56, 99, 99, 99, 99, 99,
    47, 66, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99
];

const VP3_QMAT_INTER: &[i16; 64] = &[
    16,  16,  16,  20,  24,  28,  32,  40,
    16,  16,  20,  24,  28,  32,  40,  48,
    16,  20,  24,  28,  32,  40,  48,  64,
    20,  24,  28,  32,  40,  48,  64,  64,
    24,  28,  32,  40,  48,  64,  64,  64,
    28,  32,  40,  48,  64,  64,  64,  96,
    32,  40,  48,  64,  64,  64,  96, 128,
    40,  48,  64,  64,  64,  96, 128, 128
];

const VP31_DC_CODES: [[u16; 32]; 16] = [
  [
    0x002D, 0x0026, 0x0166, 0x004E, 0x02CE, 0x059E, 0x027D, 0x0008,
    0x04F9, 0x000F, 0x000E, 0x001B, 0x0006, 0x0008, 0x0005, 0x001A,
    0x0015, 0x0007, 0x000C, 0x0001, 0x0000, 0x0009, 0x0017, 0x0029,
    0x0028, 0x00B2, 0x04F8, 0x059F, 0x009E, 0x013F, 0x0012, 0x0058,
  ], [
    0x0010, 0x0047, 0x01FF, 0x008C, 0x03FC, 0x046A, 0x0469, 0x0022,
    0x11A1, 0x000E, 0x000D, 0x0004, 0x0005, 0x0009, 0x0006, 0x001E,
    0x0016, 0x0007, 0x000C, 0x0001, 0x0000, 0x000A, 0x0017, 0x007D,
    0x007E, 0x011B, 0x08D1, 0x03FD, 0x046B, 0x11A0, 0x007C, 0x00FE,
  ], [
    0x0016, 0x0020, 0x0086, 0x0087, 0x0367, 0x06CC, 0x06CB, 0x006E,
    0x366D, 0x000F, 0x000E, 0x0004, 0x0005, 0x000A, 0x0006, 0x001A,
    0x0011, 0x0007, 0x000C, 0x0001, 0x0000, 0x0009, 0x0017, 0x006F,
    0x006D, 0x0364, 0x0D9A, 0x06CA, 0x1B37, 0x366C, 0x0042, 0x00D8,
  ], [
    0x0000, 0x002D, 0x00F7, 0x0058, 0x0167, 0x02CB, 0x02CA, 0x000E,
    0x1661, 0x0003, 0x0002, 0x0008, 0x0009, 0x000D, 0x0002, 0x001F,
    0x0017, 0x0001, 0x000C, 0x000E, 0x000A, 0x0006, 0x0078, 0x000F,
    0x007A, 0x0164, 0x0599, 0x02CD, 0x0B31, 0x1660, 0x0079, 0x00F6,
  ], [
    0x0003, 0x003C, 0x000F, 0x007A, 0x001D, 0x0020, 0x0072, 0x0006,
    0x0399, 0x0004, 0x0005, 0x0005, 0x0006, 0x000E, 0x0004, 0x0000,
    0x0019, 0x0002, 0x000D, 0x0007, 0x001F, 0x0030, 0x0011, 0x0031,
    0x0005, 0x0021, 0x00E7, 0x0038, 0x01CD, 0x0398, 0x007B, 0x0009,
  ], [
    0x0009, 0x0002, 0x0074, 0x0007, 0x00EC, 0x00D1, 0x01A6, 0x0006,
    0x0D21, 0x0005, 0x0006, 0x0008, 0x0007, 0x000F, 0x0004, 0x0000,
    0x001C, 0x0002, 0x0005, 0x0003, 0x000C, 0x0035, 0x01A7, 0x001B,
    0x0077, 0x01A5, 0x0349, 0x00D0, 0x0691, 0x0D20, 0x0075, 0x00ED,
  ], [
    0x000A, 0x000C, 0x0012, 0x001B, 0x00B7, 0x016C, 0x0099, 0x005A,
    0x16D8, 0x0007, 0x0006, 0x0009, 0x0008, 0x0000, 0x0005, 0x0017,
    0x000E, 0x0002, 0x0003, 0x000F, 0x001A, 0x004D, 0x2DB3, 0x002C,
    0x0011, 0x02DA, 0x05B7, 0x0098, 0x0B6D, 0x2DB2, 0x0010, 0x0027,
  ], [
    0x000D, 0x000F, 0x001D, 0x0008, 0x0051, 0x0056, 0x00AF, 0x002A,
    0x148A, 0x0007, 0x0000, 0x0008, 0x0009, 0x000C, 0x0006, 0x0017,
    0x000B, 0x0016, 0x0015, 0x0009, 0x0050, 0x00AE, 0x2917, 0x001C,
    0x0014, 0x0290, 0x0523, 0x0149, 0x0A44, 0x2916, 0x0053, 0x00A5,
  ], [
    0x0001, 0x001D, 0x00F5, 0x00F4, 0x024D, 0x0499, 0x0498, 0x0001,
    0x0021, 0x0006, 0x0005, 0x0006, 0x0005, 0x0002, 0x0007, 0x0025,
    0x007B, 0x001C, 0x0020, 0x000D, 0x0048, 0x0092, 0x0127, 0x000E,
    0x0004, 0x0011, 0x000C, 0x003C, 0x000F, 0x0000, 0x001F, 0x0013,
  ], [
    0x0005, 0x003C, 0x0040, 0x000D, 0x0031, 0x0061, 0x0060, 0x0002,
    0x00F5, 0x0006, 0x0005, 0x0007, 0x0006, 0x0002, 0x0009, 0x0025,
    0x0007, 0x0021, 0x0024, 0x0010, 0x0041, 0x00F4, 0x0019, 0x000E,
    0x0003, 0x0011, 0x0011, 0x003F, 0x003E, 0x007B, 0x0000, 0x0013,
  ], [
    0x000A, 0x0007, 0x0001, 0x0009, 0x0131, 0x0261, 0x0260, 0x0015,
    0x0001, 0x0007, 0x0006, 0x0008, 0x0007, 0x0006, 0x0012, 0x002F,
    0x0014, 0x0027, 0x002D, 0x0016, 0x004D, 0x0099, 0x0000, 0x0004,
    0x0001, 0x0005, 0x0017, 0x002E, 0x002C, 0x0008, 0x0006, 0x0001,
  ], [
    0x0000, 0x000E, 0x0017, 0x002A, 0x0010, 0x00F9, 0x00F8, 0x001E,
    0x003F, 0x0007, 0x0006, 0x0009, 0x0008, 0x0006, 0x000F, 0x0005,
    0x0016, 0x0029, 0x002B, 0x0015, 0x0050, 0x0011, 0x007D, 0x0004,
    0x0017, 0x0006, 0x0014, 0x002C, 0x002D, 0x000E, 0x0009, 0x0051,
  ], [
    0x0002, 0x0018, 0x002F, 0x000D, 0x0053, 0x0295, 0x0294, 0x00A4,
    0x007C, 0x0000, 0x0007, 0x0009, 0x0008, 0x001B, 0x000C, 0x0028,
    0x006A, 0x001E, 0x001D, 0x0069, 0x00D7, 0x007D, 0x014B, 0x0019,
    0x0016, 0x002E, 0x001C, 0x002B, 0x002A, 0x0068, 0x003F, 0x00D6,
  ], [
    0x0002, 0x001B, 0x000C, 0x0018, 0x0029, 0x007F, 0x02F0, 0x0198,
    0x0179, 0x0000, 0x0007, 0x0009, 0x0008, 0x001A, 0x000D, 0x002A,
    0x0064, 0x001E, 0x0067, 0x005F, 0x00CD, 0x007E, 0x02F1, 0x0016,
    0x000E, 0x002E, 0x0065, 0x002B, 0x0028, 0x003E, 0x00BD, 0x0199,
  ], [
    0x0002, 0x0007, 0x0016, 0x0006, 0x0036, 0x005C, 0x015D, 0x015C,
    0x02BF, 0x0000, 0x0007, 0x0009, 0x0008, 0x0018, 0x0034, 0x002A,
    0x005E, 0x006A, 0x0064, 0x005D, 0x00CB, 0x00AD, 0x02BE, 0x0014,
    0x0033, 0x006E, 0x005F, 0x006F, 0x006B, 0x00CA, 0x00AC, 0x015E,
  ], [
    0x000F, 0x001D, 0x0018, 0x000B, 0x0019, 0x0029, 0x00D6, 0x0551,
    0x0AA1, 0x0001, 0x0000, 0x0009, 0x0008, 0x001B, 0x0038, 0x0028,
    0x0057, 0x006A, 0x0068, 0x0056, 0x00E5, 0x0155, 0x0AA0, 0x0073,
    0x0069, 0x00D7, 0x00AB, 0x00E4, 0x00A9, 0x0151, 0x0150, 0x02A9,
  ]
];

const VP31_DC_BITS: [[u8; 32]; 16] = [
  [
     6,  7,  9,  8, 10, 11, 11,  5, 12,  4,  4,  5,  4,  4,  4,  5,
     5,  4,  4,  3,  3,  4,  5,  6,  6,  8, 12, 11,  9, 10,  6,  7,
  ], [
     5,  7,  9,  8, 10, 11, 11,  6, 13,  4,  4,  4,  4,  4,  4,  5,
     5,  4,  4,  3,  3,  4,  5,  7,  7,  9, 12, 10, 11, 13,  7,  8,
  ], [
     5,  6,  8,  8, 10, 11, 11,  7, 14,  4,  4,  4,  4,  4,  4,  5,
     5,  4,  4,  3,  3,  4,  5,  7,  7, 10, 12, 11, 13, 14,  7,  8,
  ], [
     4,  6,  8,  7,  9, 10, 10,  6, 13,  3,  3,  4,  4,  4,  4,  5,
     5,  4,  4,  4,  4,  5,  7,  6,  7,  9, 11, 10, 12, 13,  7,  8,
  ], [
     4,  6,  7,  7,  8,  9, 10,  6, 13,  3,  3,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  5,  6,  8,  6,  6,  9, 11,  9, 12, 13,  7,  7,
  ], [
     4,  5,  7,  6,  8,  9, 10,  6, 13,  3,  3,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  5,  7, 10,  6,  7, 10, 11,  9, 12, 13,  7,  8,
  ], [
     4,  5,  6,  6,  8,  9,  9,  7, 13,  3,  3,  4,  4,  3,  4,  5,
     5,  4,  4,  5,  6,  8, 14,  6,  6, 10, 11,  9, 12, 14,  6,  7,
  ], [
     4,  5,  6,  5,  7,  8,  9,  7, 13,  3,  2,  4,  4,  4,  4,  5,
     5,  5,  5,  5,  7,  9, 14,  6,  6, 10, 11,  9, 12, 14,  7,  8,
  ], [
     4,  6,  8,  8, 10, 11, 11,  5,  6,  3,  3,  4,  4,  4,  5,  6,
     7,  6,  6,  6,  7,  8,  9,  4,  4,  5,  6,  6,  5,  5,  5,  5,
  ], [
     4,  6,  7,  7,  9, 10, 10,  5,  8,  3,  3,  4,  4,  4,  5,  6,
     6,  6,  6,  6,  7,  8,  8,  4,  4,  5,  6,  6,  6,  7,  4,  5,
  ], [
     4,  5,  6,  6,  9, 10, 10,  6,  7,  3,  3,  4,  4,  4,  5,  6,
     6,  6,  6,  6,  7,  8,  7,  4,  4,  5,  6,  6,  6,  6,  5,  5,
  ], [
     3,  5,  6,  6,  7, 10, 10,  7,  8,  3,  3,  4,  4,  4,  5,  5,
     6,  6,  6,  6,  7,  7,  9,  4,  5,  5,  6,  6,  6,  6,  6,  7,
  ], [
     3,  5,  6,  5,  7, 10, 10,  8,  8,  2,  3,  4,  4,  5,  5,  6,
     7,  6,  6,  7,  8,  8,  9,  5,  5,  6,  6,  6,  6,  7,  7,  8,
  ], [
     3,  5,  5,  5,  6,  8, 10,  9,  9,  2,  3,  4,  4,  5,  5,  6,
     7,  6,  7,  7,  8,  8, 10,  5,  5,  6,  7,  6,  6,  7,  8,  9,
  ], [
     3,  4,  5,  4,  6,  7,  9,  9, 10,  2,  3,  4,  4,  5,  6,  6,
     7,  7,  7,  7,  8,  8, 10,  5,  6,  7,  7,  7,  7,  8,  8,  9,
  ], [
     4,  5,  5,  4,  5,  6,  8, 11, 12,  2,  2,  4,  4,  5,  6,  6,
     7,  7,  7,  7,  8,  9, 12,  7,  7,  8,  8,  8,  8,  9,  9, 10,
  ]
];

const VP31_AC_CAT0_CODES: [[u16; 32]; 16] = [
  [
    0x0008, 0x0025, 0x017A, 0x02F7, 0x0BDB, 0x17B4, 0x2F6B, 0x001D,
    0x2F6A, 0x0008, 0x0007, 0x0001, 0x0002, 0x000A, 0x0006, 0x0000,
    0x001C, 0x0009, 0x000D, 0x000F, 0x000C, 0x0003, 0x000A, 0x0016,
    0x0013, 0x005D, 0x0024, 0x00BC, 0x005C, 0x05EC, 0x000B, 0x005F,
  ], [
    0x000F, 0x0010, 0x004B, 0x00C6, 0x031D, 0x0C71, 0x0C70, 0x0001,
    0x0C73, 0x0008, 0x0009, 0x0002, 0x0003, 0x000B, 0x0006, 0x0000,
    0x001C, 0x0005, 0x000D, 0x000F, 0x000A, 0x0019, 0x0013, 0x001D,
    0x0030, 0x0062, 0x0024, 0x004A, 0x018F, 0x0C72, 0x000E, 0x0011,
  ], [
    0x001B, 0x0003, 0x008D, 0x0040, 0x0239, 0x0471, 0x08E0, 0x0003,
    0x11C3, 0x000A, 0x0009, 0x0004, 0x0005, 0x000E, 0x0007, 0x0001,
    0x001E, 0x0006, 0x000C, 0x000B, 0x0002, 0x0000, 0x0041, 0x001F,
    0x0022, 0x0002, 0x008F, 0x008C, 0x011D, 0x11C2, 0x001A, 0x0021,
  ], [
    0x001F, 0x0003, 0x0003, 0x0043, 0x000B, 0x0015, 0x0051, 0x0003,
    0x0050, 0x000D, 0x000C, 0x0004, 0x0006, 0x000E, 0x000A, 0x0001,
    0x001E, 0x0005, 0x0009, 0x0007, 0x0011, 0x0002, 0x0004, 0x0002,
    0x002D, 0x0020, 0x0042, 0x0001, 0x0000, 0x0029, 0x0017, 0x002C,
  ], [
    0x0003, 0x001F, 0x003A, 0x005D, 0x0173, 0x02E4, 0x172D, 0x0004,
    0x172C, 0x000F, 0x000E, 0x0009, 0x0008, 0x000C, 0x000A, 0x0001,
    0x0016, 0x0002, 0x0005, 0x001A, 0x002F, 0x0038, 0x05CA, 0x0006,
    0x0037, 0x001E, 0x003B, 0x0039, 0x00B8, 0x0B97, 0x0000, 0x0036,
  ], [
    0x0006, 0x0037, 0x005D, 0x000C, 0x00B9, 0x02E3, 0x05C4, 0x0004,
    0x1715, 0x0000, 0x000F, 0x0008, 0x0007, 0x000C, 0x0009, 0x001D,
    0x0016, 0x001C, 0x001A, 0x000B, 0x005E, 0x0170, 0x1714, 0x000A,
    0x000A, 0x0036, 0x005F, 0x001B, 0x001A, 0x0B8B, 0x0002, 0x0007,
  ], [
    0x000C, 0x000B, 0x0079, 0x0022, 0x00F0, 0x0119, 0x0230, 0x001D,
    0x08C4, 0x0001, 0x0000, 0x000A, 0x0009, 0x000B, 0x0007, 0x001C,
    0x003D, 0x000D, 0x0008, 0x0015, 0x008D, 0x118B, 0x118A, 0x000D,
    0x0010, 0x0009, 0x0014, 0x0047, 0x00F1, 0x0463, 0x001F, 0x000C,
  ], [
    0x0000, 0x001A, 0x0033, 0x000C, 0x0046, 0x01E3, 0x03C5, 0x0017,
    0x1E21, 0x0002, 0x0001, 0x0009, 0x000A, 0x0007, 0x001B, 0x003D,
    0x001B, 0x0022, 0x0079, 0x00F0, 0x1E20, 0x1E23, 0x1E22, 0x000E,
    0x0016, 0x0018, 0x0032, 0x001A, 0x0047, 0x0789, 0x001F, 0x0010,
  ], [
    0x001D, 0x0061, 0x004E, 0x009E, 0x027C, 0x09F5, 0x09F4, 0x0003,
    0x0060, 0x0000, 0x000F, 0x000B, 0x000A, 0x0009, 0x0005, 0x000D,
    0x0031, 0x0008, 0x0038, 0x0012, 0x0026, 0x013F, 0x04FB, 0x000D,
    0x0002, 0x000C, 0x0039, 0x001C, 0x000F, 0x001D, 0x0008, 0x0019,
  ], [
    0x0007, 0x0019, 0x00AB, 0x00AA, 0x0119, 0x0461, 0x0460, 0x001B,
    0x0047, 0x0001, 0x0000, 0x000C, 0x000B, 0x0009, 0x0005, 0x000D,
    0x0035, 0x003D, 0x003C, 0x0018, 0x0022, 0x008D, 0x0231, 0x000E,
    0x001F, 0x0009, 0x002B, 0x0010, 0x0034, 0x0054, 0x0008, 0x0014,
  ], [
    0x000C, 0x0005, 0x0008, 0x005B, 0x004D, 0x0131, 0x0261, 0x001A,
    0x0012, 0x0000, 0x000F, 0x000A, 0x0009, 0x0006, 0x001B, 0x0006,
    0x001C, 0x002C, 0x0015, 0x005A, 0x0027, 0x0099, 0x0260, 0x000E,
    0x0004, 0x000F, 0x0007, 0x001D, 0x000B, 0x0014, 0x0008, 0x0017,
  ], [
    0x000F, 0x0013, 0x0075, 0x0024, 0x0095, 0x0251, 0x04A0, 0x0010,
    0x00C8, 0x0002, 0x0001, 0x0001, 0x0000, 0x001A, 0x0011, 0x002C,
    0x0065, 0x0074, 0x004B, 0x00C9, 0x0129, 0x0943, 0x0942, 0x0003,
    0x000A, 0x001C, 0x0018, 0x0033, 0x0017, 0x002D, 0x001B, 0x003B,
  ], [
    0x0003, 0x001A, 0x002D, 0x0038, 0x0028, 0x0395, 0x0E51, 0x0037,
    0x00E4, 0x0001, 0x0000, 0x001F, 0x001E, 0x0017, 0x003A, 0x0073,
    0x002A, 0x002B, 0x0029, 0x01CB, 0x0729, 0x1CA1, 0x1CA0, 0x0004,
    0x000A, 0x0004, 0x0018, 0x0036, 0x000B, 0x002C, 0x0019, 0x003B,
  ], [
    0x0004, 0x0004, 0x003F, 0x0017, 0x0075, 0x01F5, 0x07D1, 0x0017,
    0x01F6, 0x0001, 0x0000, 0x001B, 0x001A, 0x000A, 0x0032, 0x0074,
    0x00F8, 0x00F9, 0x01F7, 0x03E9, 0x0FA0, 0x1F43, 0x1F42, 0x0003,
    0x000A, 0x001E, 0x001C, 0x003B, 0x0018, 0x0016, 0x0016, 0x0033,
  ], [
    0x0004, 0x0007, 0x0018, 0x001E, 0x0036, 0x0031, 0x0177, 0x0077,
    0x0176, 0x0001, 0x0000, 0x001A, 0x0019, 0x003A, 0x0019, 0x005C,
    0x00BA, 0x0061, 0x00C1, 0x0180, 0x0302, 0x0607, 0x0606, 0x0002,
    0x000A, 0x001F, 0x001C, 0x0037, 0x0016, 0x0076, 0x000D, 0x002F,
  ], [
    0x0000, 0x000A, 0x001A, 0x000C, 0x001D, 0x0039, 0x0078, 0x005E,
    0x0393, 0x0002, 0x0001, 0x0016, 0x000F, 0x002E, 0x005F, 0x0073,
    0x00E5, 0x01C8, 0x0E4A, 0x1C97, 0x1C96, 0x0E49, 0x0E48, 0x0004,
    0x0006, 0x001F, 0x001B, 0x001D, 0x0038, 0x0038, 0x003D, 0x0079,
  ]
];

const VP31_AC_CAT0_BITS: [[u8; 32]; 16] = [
  [
     5,  7,  9, 10, 12, 13, 14,  5, 14,  4,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  4,  4,  5,  5,  6,  7,  7,  8,  7, 11,  5,  7,
  ], [
     5,  6,  8,  8, 10, 12, 12,  4, 12,  4,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  4,  5,  6,  5,  6,  7,  7,  8,  9, 12,  5,  6,
  ], [
     5,  6,  8,  7, 10, 11, 12,  4, 13,  4,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  4,  5,  7,  5,  6,  6,  8,  8,  9, 13,  5,  6,
  ], [
     5,  6,  7,  7,  9, 10, 12,  4, 12,  4,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  5,  6,  8,  4,  6,  6,  7,  7,  7, 11,  5,  6,
  ], [
     4,  6,  7,  7,  9, 10, 13,  4, 13,  4,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  5,  6,  7, 11,  4,  6,  6,  7,  7,  8, 12,  4,  6,
  ], [
     4,  6,  7,  6,  8, 10, 11,  4, 13,  3,  4,  4,  4,  4,  4,  5,
     5,  5,  5,  5,  7,  9, 13,  4,  5,  6,  7,  7,  7, 12,  4,  5,
  ], [
     4,  5,  7,  6,  8,  9, 10,  5, 12,  3,  3,  4,  4,  4,  4,  5,
     6,  5,  5,  6,  8, 13, 13,  4,  5,  5,  6,  7,  8, 11,  5,  5,
  ], [
     3,  5,  6,  5,  7,  9, 10,  5, 13,  3,  3,  4,  4,  4,  5,  6,
     6,  6,  7,  8, 13, 13, 13,  4,  5,  5,  6,  6,  7, 11,  5,  5,
  ], [
     5,  7,  8,  9, 11, 13, 13,  4,  7,  3,  4,  4,  4,  4,  4,  5,
     6,  5,  6,  6,  7, 10, 12,  4,  4,  5,  6,  6,  5,  6,  4,  5,
  ], [
     4,  6,  8,  8, 10, 12, 12,  5,  8,  3,  3,  4,  4,  4,  4,  5,
     6,  6,  6,  6,  7,  9, 11,  4,  5,  5,  6,  6,  6,  7,  4,  5,
  ], [
     4,  5,  6,  7,  9, 11, 12,  5,  7,  3,  4,  4,  4,  4,  5,  5,
     6,  6,  6,  7,  8, 10, 12,  4,  4,  5,  5,  6,  5,  6,  4,  5,
  ], [
     4,  5,  7,  6,  8, 10, 11,  5,  8,  3,  3,  4,  4,  5,  5,  6,
     7,  7,  7,  8,  9, 12, 12,  3,  4,  5,  5,  6,  5,  6,  5,  6,
  ], [
     3,  5,  6,  6,  7, 10, 12,  6,  8,  3,  3,  5,  5,  5,  6,  7,
     7,  7,  7,  9, 11, 13, 13,  3,  4,  4,  5,  6,  5,  6,  5,  6,
  ], [
     3,  4,  6,  5,  7,  9, 11,  6,  9,  3,  3,  5,  5,  5,  6,  7,
     8,  8,  9, 10, 12, 13, 13,  3,  4,  5,  5,  6,  5,  6,  5,  6,
  ], [
     3,  4,  5,  5,  6,  7,  9,  7,  9,  3,  3,  5,  5,  6,  6,  7,
     8,  8,  9, 10, 11, 12, 12,  3,  4,  5,  5,  6,  5,  7,  5,  6,
  ], [
     3,  4,  5,  4,  5,  6,  7,  7, 11,  3,  3,  5,  5,  6,  7,  8,
     9, 10, 13, 14, 14, 13, 13,  3,  4,  5,  5,  6,  6,  7,  6,  7,
  ]
];

const VP31_AC_CAT1_CODES: [[u16; 32]; 16] = [
  [
    0x000B, 0x002B, 0x0054, 0x01B7, 0x06D9, 0x0DB1, 0x0DB0, 0x0002,
    0x00AB, 0x0009, 0x000A, 0x0007, 0x0008, 0x000F, 0x000C, 0x0003,
    0x001D, 0x0004, 0x000B, 0x0006, 0x001A, 0x0003, 0x00AA, 0x0001,
    0x0000, 0x0014, 0x006C, 0x00DA, 0x0002, 0x036D, 0x001C, 0x0037,
  ], [
    0x001D, 0x0004, 0x00B6, 0x006A, 0x05B9, 0x16E1, 0x16E0, 0x0007,
    0x016F, 0x000C, 0x000D, 0x0009, 0x0008, 0x000F, 0x000A, 0x0003,
    0x0017, 0x0002, 0x0004, 0x001C, 0x002C, 0x006B, 0x0B71, 0x0005,
    0x0003, 0x001B, 0x005A, 0x0034, 0x0005, 0x02DD, 0x0000, 0x000C,
  ], [
    0x0003, 0x007F, 0x00A1, 0x00A0, 0x020C, 0x0834, 0x106B, 0x0007,
    0x0082, 0x000E, 0x000D, 0x000B, 0x000C, 0x0000, 0x0009, 0x0002,
    0x0011, 0x001E, 0x0015, 0x003E, 0x0040, 0x041B, 0x106A, 0x0006,
    0x000A, 0x0029, 0x007E, 0x0051, 0x0021, 0x0107, 0x0004, 0x000B,
  ], [
    0x0007, 0x001B, 0x00F6, 0x00E9, 0x03A1, 0x0740, 0x0E82, 0x001F,
    0x01EF, 0x0001, 0x0002, 0x000B, 0x000C, 0x000D, 0x0008, 0x001C,
    0x0003, 0x0012, 0x0002, 0x0075, 0x01D1, 0x1D07, 0x1D06, 0x000A,
    0x0013, 0x003B, 0x001A, 0x007A, 0x003C, 0x01EE, 0x0000, 0x000C,
  ], [
    0x000D, 0x003D, 0x0042, 0x0037, 0x00D9, 0x0362, 0x06C6, 0x001F,
    0x0086, 0x0001, 0x0002, 0x000C, 0x000B, 0x000A, 0x0001, 0x000F,
    0x0025, 0x003C, 0x001A, 0x0087, 0x01B0, 0x0D8F, 0x0D8E, 0x000E,
    0x0013, 0x000C, 0x0024, 0x0020, 0x0011, 0x006D, 0x0000, 0x000E,
  ], [
    0x0000, 0x0012, 0x0076, 0x0077, 0x014D, 0x0533, 0x14C9, 0x0013,
    0x00A5, 0x0002, 0x0003, 0x000B, 0x000C, 0x0008, 0x001A, 0x002B,
    0x0075, 0x0074, 0x00A7, 0x0298, 0x14C8, 0x14CB, 0x14CA, 0x000F,
    0x001C, 0x0007, 0x002A, 0x0028, 0x001B, 0x00A4, 0x0002, 0x0006,
  ], [
    0x0002, 0x001A, 0x002B, 0x003A, 0x00ED, 0x0283, 0x0A0A, 0x0004,
    0x00A1, 0x0004, 0x0003, 0x000B, 0x000C, 0x001F, 0x0006, 0x0077,
    0x00A3, 0x00A2, 0x0140, 0x1417, 0x1416, 0x0A09, 0x0A08, 0x0000,
    0x001E, 0x0007, 0x002A, 0x0029, 0x001C, 0x00EC, 0x001B, 0x0005,
  ], [
    0x0002, 0x0002, 0x0018, 0x001D, 0x0035, 0x00E4, 0x01CF, 0x001D,
    0x0072, 0x0004, 0x0005, 0x0006, 0x0007, 0x0006, 0x0073, 0x0038,
    0x01CE, 0x039B, 0x0398, 0x0733, 0x0732, 0x0735, 0x0734, 0x0000,
    0x001F, 0x001B, 0x0034, 0x000F, 0x001E, 0x00E5, 0x0019, 0x0038,
  ], [
    0x0016, 0x0050, 0x0172, 0x02E7, 0x1732, 0x2E67, 0x2E66, 0x0006,
    0x0051, 0x0001, 0x0000, 0x000D, 0x000C, 0x0009, 0x001C, 0x0009,
    0x001C, 0x001D, 0x005D, 0x00B8, 0x05CD, 0x1731, 0x1730, 0x000F,
    0x0005, 0x000F, 0x0008, 0x0029, 0x001D, 0x002F, 0x0008, 0x0015,
  ], [
    0x0009, 0x0021, 0x0040, 0x00AD, 0x02B0, 0x1589, 0x1588, 0x001C,
    0x005F, 0x0000, 0x000F, 0x000D, 0x000C, 0x0006, 0x0011, 0x002A,
    0x0057, 0x005E, 0x0041, 0x0159, 0x0563, 0x158B, 0x158A, 0x0001,
    0x0005, 0x0014, 0x003B, 0x002E, 0x0004, 0x003A, 0x0007, 0x0016,
  ], [
    0x000E, 0x0007, 0x0046, 0x0045, 0x0064, 0x032A, 0x0657, 0x0018,
    0x000D, 0x0000, 0x000F, 0x000A, 0x000B, 0x001A, 0x0036, 0x0047,
    0x0044, 0x0018, 0x0033, 0x00CB, 0x0656, 0x0329, 0x0328, 0x0002,
    0x0006, 0x0019, 0x000E, 0x0037, 0x0009, 0x000F, 0x0002, 0x0010,
  ], [
    0x0003, 0x0018, 0x0023, 0x0077, 0x0194, 0x1956, 0x32AF, 0x003A,
    0x0076, 0x0002, 0x0001, 0x001F, 0x001E, 0x0014, 0x0022, 0x0064,
    0x0197, 0x0196, 0x032B, 0x0654, 0x32AE, 0x1955, 0x1954, 0x0000,
    0x0009, 0x001C, 0x0015, 0x0010, 0x000D, 0x0017, 0x0016, 0x0033,
  ], [
    0x0005, 0x0006, 0x003E, 0x0010, 0x0048, 0x093F, 0x24FA, 0x0032,
    0x0067, 0x0002, 0x0001, 0x001B, 0x001E, 0x0034, 0x0066, 0x0092,
    0x0126, 0x024E, 0x049E, 0x49F7, 0x49F6, 0x24F9, 0x24F8, 0x0000,
    0x0007, 0x0018, 0x0011, 0x003F, 0x000E, 0x0013, 0x0035, 0x0025,
  ], [
    0x0005, 0x0008, 0x0012, 0x001C, 0x001C, 0x00EA, 0x1D75, 0x001E,
    0x0066, 0x0001, 0x0002, 0x001B, 0x001A, 0x001F, 0x003B, 0x0074,
    0x01D6, 0x03AF, 0x1D74, 0x1D77, 0x1D76, 0x0EB9, 0x0EB8, 0x000F,
    0x0006, 0x0013, 0x003B, 0x003A, 0x0000, 0x0018, 0x0032, 0x0067,
  ], [
    0x0004, 0x000A, 0x001B, 0x000C, 0x000D, 0x00E6, 0x0684, 0x0072,
    0x00E7, 0x0002, 0x0001, 0x0017, 0x0016, 0x0018, 0x00D1, 0x01A0,
    0x0686, 0x0D0F, 0x0D0A, 0x1A17, 0x1A16, 0x1A1D, 0x1A1C, 0x000F,
    0x001D, 0x000E, 0x0035, 0x0038, 0x0000, 0x000F, 0x0019, 0x0069,
  ], [
    0x0003, 0x000C, 0x001B, 0x0000, 0x0003, 0x002E, 0x0051, 0x00BC,
    0x0053, 0x0004, 0x0002, 0x0016, 0x0015, 0x0015, 0x0050, 0x00A4,
    0x0294, 0x052B, 0x052A, 0x052D, 0x052C, 0x052F, 0x052E, 0x000E,
    0x001A, 0x0004, 0x0028, 0x0029, 0x000F, 0x000B, 0x005F, 0x00BD,
  ]
];

const VP31_AC_CAT1_BITS: [[u8; 32]; 16] = [
  [
     5,  7,  8,  9, 11, 12, 12,  4,  9,  4,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  5,  6,  9,  4,  5,  6,  7,  8,  6, 10,  5,  6,
  ], [
     5,  6,  8,  8, 11, 13, 13,  4,  9,  4,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  5,  6,  8, 12,  4,  5,  6,  7,  7,  6, 10,  4,  5,
  ], [
     4,  7,  8,  8, 10, 12, 13,  4,  8,  4,  4,  4,  4,  3,  4,  4,
     5,  5,  5,  6,  7, 11, 13,  4,  5,  6,  7,  7,  6,  9,  4,  5,
  ], [
     4,  6,  8,  8, 10, 11, 12,  5,  9,  3,  3,  4,  4,  4,  4,  5,
     5,  5,  5,  7,  9, 13, 13,  4,  5,  6,  6,  7,  6,  9,  4,  5,
  ], [
     4,  6,  7,  7,  9, 11, 12,  5,  8,  3,  3,  4,  4,  4,  4,  5,
     6,  6,  6,  8, 10, 13, 13,  4,  5,  5,  6,  6,  5,  8,  4,  5,
  ], [
     3,  5,  7,  7,  9, 11, 13,  5,  8,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  8, 10, 13, 13, 13,  4,  5,  5,  6,  6,  5,  8,  4,  5,
  ], [
     3,  5,  6,  6,  8, 10, 12,  5,  8,  3,  3,  4,  4,  5,  5,  7,
     8,  8,  9, 13, 13, 12, 12,  3,  5,  5,  6,  6,  5,  8,  5,  5,
  ], [
     3,  4,  5,  5,  6,  8, 11,  7,  9,  3,  3,  4,  4,  5,  7,  8,
    11, 12, 12, 13, 13, 13, 13,  3,  5,  5,  6,  6,  5,  8,  5,  6,
  ], [
     5,  7,  9, 10, 13, 14, 14,  4,  7,  3,  3,  4,  4,  4,  5,  5,
     6,  6,  7,  8, 11, 13, 13,  4,  4,  5,  5,  6,  5,  6,  4,  5,
  ], [
     4,  6,  7,  8, 10, 13, 13,  5,  7,  3,  4,  4,  4,  4,  5,  6,
     7,  7,  7,  9, 11, 13, 13,  3,  4,  5,  6,  6,  4,  6,  4,  5,
  ], [
     4,  5,  7,  7,  9, 12, 13,  5,  6,  3,  4,  4,  4,  5,  6,  7,
     7,  7,  8, 10, 13, 12, 12,  3,  4,  5,  5,  6,  4,  5,  4,  5,
  ], [
     3,  5,  6,  7,  9, 13, 14,  6,  7,  3,  3,  5,  5,  5,  6,  7,
     9,  9, 10, 11, 14, 13, 13,  3,  4,  5,  5,  5,  4,  5,  5,  6,
  ], [
     3,  4,  6,  5,  7, 12, 14,  6,  7,  3,  3,  5,  5,  6,  7,  8,
     9, 10, 11, 15, 15, 14, 14,  3,  4,  5,  5,  6,  4,  5,  6,  6,
  ], [
     3,  4,  5,  5,  6,  9, 14,  6,  7,  3,  3,  5,  5,  6,  7,  8,
    10, 11, 14, 14, 14, 13, 13,  4,  4,  5,  6,  6,  3,  5,  6,  7,
  ], [
     3,  4,  5,  4,  5,  8, 11,  7,  8,  3,  3,  5,  5,  6,  8,  9,
    11, 12, 12, 13, 13, 13, 13,  4,  5,  5,  6,  6,  3,  5,  6,  7,
  ], [
     3,  4,  5,  3,  4,  6,  9,  8,  9,  3,  3,  5,  5,  7,  9, 10,
    12, 13, 13, 13, 13, 13, 13,  4,  5,  5,  6,  6,  4,  6,  7,  8,
  ]
];

const VP31_AC_CAT2_CODES: [[u16; 32]; 16] = [
  [
    0x0003, 0x0009, 0x00D0, 0x01A3, 0x0344, 0x0D14, 0x1A2B, 0x0004,
    0x0015, 0x0000, 0x000F, 0x000B, 0x000C, 0x000E, 0x0009, 0x001B,
    0x000A, 0x0014, 0x000D, 0x002A, 0x0014, 0x068B, 0x1A2A, 0x0008,
    0x000B, 0x002B, 0x000B, 0x0069, 0x0035, 0x0008, 0x0007, 0x000C,
  ], [
    0x000A, 0x003C, 0x0032, 0x0030, 0x00C5, 0x0621, 0x0620, 0x001F,
    0x0033, 0x0001, 0x0000, 0x000E, 0x000D, 0x000C, 0x0004, 0x000D,
    0x0026, 0x0027, 0x0014, 0x0063, 0x0189, 0x0623, 0x0622, 0x000B,
    0x0012, 0x003D, 0x0022, 0x0015, 0x000B, 0x0023, 0x0007, 0x0010,
  ], [
    0x000F, 0x000C, 0x0043, 0x0010, 0x0044, 0x0114, 0x0455, 0x0018,
    0x0023, 0x0001, 0x0000, 0x000E, 0x000D, 0x0009, 0x0019, 0x0009,
    0x0017, 0x0016, 0x0042, 0x008B, 0x0454, 0x0457, 0x0456, 0x000B,
    0x0015, 0x000A, 0x0029, 0x0020, 0x000D, 0x0028, 0x0007, 0x0011,
  ], [
    0x0001, 0x001A, 0x0029, 0x002A, 0x00A0, 0x0285, 0x1425, 0x0002,
    0x0000, 0x0002, 0x0003, 0x000C, 0x000B, 0x0008, 0x0012, 0x0001,
    0x0051, 0x0001, 0x0143, 0x0508, 0x1424, 0x1427, 0x1426, 0x000F,
    0x001C, 0x0003, 0x0037, 0x002B, 0x0013, 0x0036, 0x001D, 0x0001,
  ], [
    0x0004, 0x001F, 0x003D, 0x0006, 0x0016, 0x0053, 0x014A, 0x0034,
    0x002A, 0x0002, 0x0003, 0x000B, 0x000C, 0x001C, 0x0037, 0x0017,
    0x002B, 0x0028, 0x00A4, 0x052D, 0x052C, 0x052F, 0x052E, 0x0000,
    0x001D, 0x0007, 0x0004, 0x0035, 0x0014, 0x0036, 0x0015, 0x003C,
  ], [
    0x0004, 0x000A, 0x0007, 0x001D, 0x0009, 0x01F3, 0x07C7, 0x0008,
    0x01F0, 0x0003, 0x0002, 0x000D, 0x000C, 0x0017, 0x007D, 0x01F2,
    0x07C6, 0x07C5, 0x1F12, 0x3E27, 0x3E26, 0x1F11, 0x1F10, 0x0000,
    0x001E, 0x0006, 0x0039, 0x0038, 0x003F, 0x002C, 0x0005, 0x002D,
  ], [
    0x0002, 0x0007, 0x0018, 0x0003, 0x0005, 0x0035, 0x004F, 0x0012,
    0x04E5, 0x0005, 0x0004, 0x000D, 0x000E, 0x0033, 0x0026, 0x009D,
    0x04E4, 0x04E7, 0x04E6, 0x04E1, 0x04E0, 0x04E3, 0x04E2, 0x0000,
    0x001F, 0x000C, 0x003D, 0x003C, 0x0032, 0x0034, 0x001B, 0x0008,
  ], [
    0x0000, 0x0004, 0x001C, 0x000F, 0x0002, 0x0007, 0x0075, 0x00E8,
    0x1D2A, 0x0005, 0x0004, 0x000D, 0x000C, 0x0077, 0x0E96, 0x3A57,
    0x3A56, 0x3A5D, 0x3A5C, 0x3A5F, 0x3A5E, 0x1D29, 0x1D28, 0x0003,
    0x0006, 0x000A, 0x002C, 0x0017, 0x0076, 0x01D3, 0x03A4, 0x002D,
  ], [
    0x000A, 0x0024, 0x00BF, 0x0085, 0x0211, 0x0842, 0x1087, 0x0018,
    0x0020, 0x0001, 0x0002, 0x000E, 0x000D, 0x0007, 0x0013, 0x0025,
    0x005E, 0x0043, 0x00BE, 0x0109, 0x1086, 0x0841, 0x0840, 0x000F,
    0x0001, 0x0011, 0x0000, 0x002E, 0x0019, 0x0001, 0x0006, 0x0016,
  ], [
    0x0002, 0x000F, 0x006F, 0x0061, 0x0374, 0x1BA8, 0x3753, 0x0012,
    0x0036, 0x0000, 0x0001, 0x000A, 0x000B, 0x001A, 0x0031, 0x0060,
    0x00DC, 0x01BB, 0x06EB, 0x1BAB, 0x3752, 0x3755, 0x3754, 0x000E,
    0x0006, 0x0013, 0x000E, 0x003E, 0x0008, 0x001E, 0x0019, 0x003F,
  ], [
    0x0003, 0x001C, 0x0025, 0x0024, 0x01DA, 0x1DBD, 0x3B7C, 0x003C,
    0x003D, 0x0000, 0x0001, 0x000B, 0x000A, 0x000B, 0x0077, 0x00EC,
    0x03B6, 0x076E, 0x1DBF, 0x76FB, 0x76FA, 0x3B79, 0x3B78, 0x000D,
    0x001F, 0x0013, 0x000A, 0x0008, 0x000C, 0x0008, 0x0009, 0x003A,
  ], [
    0x0005, 0x0003, 0x0004, 0x0010, 0x008F, 0x0475, 0x11D1, 0x0079,
    0x0027, 0x0002, 0x0003, 0x0001, 0x0000, 0x0026, 0x0046, 0x011C,
    0x0477, 0x08ED, 0x11D0, 0x11D3, 0x11D2, 0x11D9, 0x11D8, 0x000D,
    0x001F, 0x0012, 0x0005, 0x003D, 0x000C, 0x000E, 0x0022, 0x0078,
  ], [
    0x0005, 0x000C, 0x001B, 0x0000, 0x0006, 0x03E2, 0x3E3D, 0x000F,
    0x0034, 0x0003, 0x0002, 0x001E, 0x001D, 0x007D, 0x01F0, 0x07C6,
    0x3E3C, 0x3E3F, 0x3E3E, 0x3E39, 0x3E38, 0x3E3B, 0x3E3A, 0x0008,
    0x001C, 0x0002, 0x003F, 0x0035, 0x0009, 0x0001, 0x000E, 0x00F9,
  ], [
    0x0004, 0x000B, 0x0001, 0x000A, 0x001E, 0x00E0, 0x0E1E, 0x0071,
    0x0039, 0x0007, 0x0006, 0x000D, 0x000C, 0x0020, 0x01C2, 0x1C3F,
    0x1C3E, 0x0E19, 0x0E18, 0x0E1B, 0x0E1A, 0x0E1D, 0x0E1C, 0x0000,
    0x0009, 0x001D, 0x001F, 0x0011, 0x0005, 0x0001, 0x0043, 0x0042,
  ], [
    0x0004, 0x000D, 0x0007, 0x0002, 0x0014, 0x016C, 0x16D1, 0x02DF,
    0x016E, 0x0000, 0x0007, 0x002C, 0x002B, 0x02DE, 0x16D0, 0x16D3,
    0x16D2, 0x2DB5, 0x2DB4, 0x2DB7, 0x2DB6, 0x16D9, 0x16D8, 0x000C,
    0x002A, 0x005A, 0x001B, 0x001A, 0x0017, 0x000C, 0x05B7, 0x05B5,
  ], [
    0x0002, 0x000F, 0x001C, 0x000C, 0x003B, 0x01AC, 0x1AD8, 0x35B3,
    0x35B2, 0x0001, 0x0000, 0x0069, 0x0068, 0x35BD, 0x35BC, 0x35BF,
    0x35BE, 0x35B9, 0x35B8, 0x35BB, 0x35BA, 0x35B5, 0x35B4, 0x01A9,
    0x01A8, 0x035A, 0x00D7, 0x00D5, 0x003A, 0x001B, 0x35B7, 0x35B6,
  ]
];

const VP31_AC_CAT2_BITS: [[u8; 32]; 16] = [
  [
     4,  6,  8,  9, 10, 12, 13,  4,  7,  3,  4,  4,  4,  4,  4,  5,
     5,  5,  5,  6,  7, 11, 13,  4,  5,  6,  6,  7,  6,  6,  4,  5,
  ], [
     4,  6,  7,  7,  9, 12, 12,  5,  7,  3,  3,  4,  4,  4,  4,  5,
     6,  6,  6,  8, 10, 12, 12,  4,  5,  6,  6,  6,  5,  6,  4,  5,
  ], [
     4,  5,  7,  6,  8, 10, 12,  5,  7,  3,  3,  4,  4,  4,  5,  5,
     6,  6,  7,  9, 12, 12, 12,  4,  5,  5,  6,  6,  5,  6,  4,  5,
  ], [
     3,  5,  6,  6,  8, 10, 13,  5,  7,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  9, 11, 13, 13, 13,  4,  5,  5,  6,  6,  5,  6,  5,  5,
  ], [
     3,  5,  6,  5,  7,  9, 11,  6,  8,  3,  3,  4,  4,  5,  6,  7,
     8,  8, 10, 13, 13, 13, 13,  3,  5,  5,  5,  6,  5,  6,  5,  6,
  ], [
     3,  4,  5,  5,  6,  9, 11,  6,  9,  3,  3,  4,  4,  5,  7,  9,
    11, 11, 13, 14, 14, 13, 13,  3,  5,  5,  6,  6,  6,  6,  5,  6,
  ], [
     3,  4,  5,  4,  5,  7,  9,  7, 13,  3,  3,  4,  4,  6,  8, 10,
    13, 13, 13, 13, 13, 13, 13,  3,  5,  5,  6,  6,  6,  7,  6,  6,
  ], [
     3,  4,  5,  4,  4,  5,  7,  8, 13,  3,  3,  4,  4,  7, 12, 14,
    14, 14, 14, 14, 14, 13, 13,  3,  5,  5,  7,  6,  7,  9, 10,  7,
  ], [
     4,  6,  8,  8, 10, 12, 13,  5,  6,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  8,  9, 13, 12, 12,  4,  4,  5,  5,  6,  5,  5,  4,  5,
  ], [
     3,  5,  7,  7, 10, 13, 14,  5,  6,  3,  3,  4,  4,  5,  6,  7,
     8,  9, 11, 13, 14, 14, 14,  4,  4,  5,  5,  6,  4,  5,  5,  6,
  ], [
     3,  5,  6,  6,  9, 13, 14,  6,  6,  3,  3,  4,  4,  5,  7,  8,
    10, 11, 13, 15, 15, 14, 14,  4,  5,  5,  5,  5,  4,  4,  5,  6,
  ], [
     3,  4,  5,  5,  8, 11, 13,  7,  6,  3,  3,  4,  4,  6,  7,  9,
    11, 12, 13, 13, 13, 13, 13,  4,  5,  5,  5,  6,  4,  4,  6,  7,
  ], [
     3,  4,  5,  4,  6, 10, 14,  7,  6,  3,  3,  5,  5,  7,  9, 11,
    14, 14, 14, 14, 14, 14, 14,  4,  5,  5,  6,  6,  4,  3,  7,  8,
  ], [
     3,  4,  4,  4,  6,  9, 13,  8,  7,  3,  3,  5,  5,  7, 10, 14,
    14, 13, 13, 13, 13, 13, 13,  4,  5,  6,  6,  6,  4,  3,  8,  8,
  ], [
     3,  4,  4,  3,  5,  9, 13, 10,  9,  2,  3,  6,  6, 10, 13, 13,
    13, 14, 14, 14, 14, 13, 13,  5,  6,  7,  6,  6,  5,  4, 11, 11,
  ], [
     2,  4,  5,  4,  6,  9, 13, 14, 14,  2,  2,  7,  7, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14,  9,  9, 10,  8,  8,  6,  5, 14, 14,
  ]
];

const VP31_AC_CAT3_CODES: [[u16; 32]; 16] = [
  [
    0x0000, 0x0010, 0x0072, 0x0071, 0x0154, 0x0AAB, 0x0AA8, 0x0014,
    0x0070, 0x0002, 0x0003, 0x000C, 0x000B, 0x0003, 0x0011, 0x0073,
    0x0054, 0x00AB, 0x02AB, 0x1553, 0x1552, 0x1555, 0x1554, 0x000D,
    0x001E, 0x0012, 0x003E, 0x002B, 0x0002, 0x003F, 0x001D, 0x0013,
  ], [
    0x0003, 0x001F, 0x0029, 0x003D, 0x000C, 0x0069, 0x0345, 0x0002,
    0x0028, 0x0002, 0x0001, 0x000E, 0x000C, 0x0015, 0x0007, 0x001B,
    0x006B, 0x006A, 0x0344, 0x0347, 0x0346, 0x01A1, 0x01A0, 0x000B,
    0x001A, 0x0012, 0x0000, 0x003C, 0x0008, 0x001B, 0x0013, 0x0001,
  ], [
    0x0004, 0x0004, 0x003F, 0x0014, 0x0056, 0x015C, 0x15D5, 0x003C,
    0x002A, 0x0000, 0x0001, 0x000E, 0x000D, 0x000C, 0x00AF, 0x02BB,
    0x15D4, 0x15D7, 0x15D6, 0x15D1, 0x15D0, 0x15D3, 0x15D2, 0x000B,
    0x0019, 0x000D, 0x003E, 0x0031, 0x0007, 0x0005, 0x003D, 0x0030,
  ], [
    0x0005, 0x0008, 0x001A, 0x0000, 0x0036, 0x0011, 0x0106, 0x000A,
    0x006E, 0x0002, 0x0003, 0x0003, 0x0002, 0x006F, 0x0021, 0x020F,
    0x020E, 0x0101, 0x0100, 0x0103, 0x0102, 0x0105, 0x0104, 0x000C,
    0x001E, 0x0003, 0x003E, 0x003F, 0x0009, 0x000E, 0x000B, 0x0009,
  ], [
    0x0002, 0x000E, 0x001E, 0x000C, 0x001F, 0x006E, 0x00AD, 0x00AF,
    0x0014, 0x0004, 0x0003, 0x001A, 0x0017, 0x002A, 0x0576, 0x0AEF,
    0x0AEE, 0x0571, 0x0570, 0x0573, 0x0572, 0x0575, 0x0574, 0x0003,
    0x0016, 0x0004, 0x0036, 0x000B, 0x000A, 0x0000, 0x006F, 0x00AC,
  ], [
    0x0004, 0x0005, 0x0003, 0x0001, 0x0004, 0x002F, 0x0526, 0x1495,
    0x00A6, 0x0007, 0x0006, 0x002D, 0x002C, 0x1494, 0x1497, 0x1496,
    0x1491, 0x1490, 0x1493, 0x1492, 0x293D, 0x293C, 0x293F, 0x0000,
    0x0028, 0x00A5, 0x0148, 0x00A7, 0x002E, 0x0015, 0x0A4E, 0x293E,
  ], [
    0x0004, 0x0005, 0x0003, 0x0001, 0x0004, 0x002F, 0x0526, 0x1495,
    0x00A6, 0x0007, 0x0006, 0x002D, 0x002C, 0x1494, 0x1497, 0x1496,
    0x1491, 0x1490, 0x1493, 0x1492, 0x293D, 0x293C, 0x293F, 0x0000,
    0x0028, 0x00A5, 0x0148, 0x00A7, 0x002E, 0x0015, 0x0A4E, 0x293E,
  ], [
    0x0004, 0x0005, 0x0003, 0x0001, 0x0004, 0x002F, 0x0526, 0x1495,
    0x00A6, 0x0007, 0x0006, 0x002D, 0x002C, 0x1494, 0x1497, 0x1496,
    0x1491, 0x1490, 0x1493, 0x1492, 0x293D, 0x293C, 0x293F, 0x0000,
    0x0028, 0x00A5, 0x0148, 0x00A7, 0x002E, 0x0015, 0x0A4E, 0x293E,
  ], [
    0x0003, 0x0011, 0x0020, 0x0074, 0x010D, 0x0863, 0x0860, 0x000A,
    0x0075, 0x0001, 0x0000, 0x000B, 0x000A, 0x0018, 0x0038, 0x0042,
    0x010F, 0x010E, 0x0219, 0x10C3, 0x10C2, 0x10C5, 0x10C4, 0x000F,
    0x0004, 0x0019, 0x000B, 0x0039, 0x0009, 0x001B, 0x001A, 0x003B,
  ], [
    0x0005, 0x0001, 0x003E, 0x0001, 0x00E2, 0x1C6F, 0x38D9, 0x0039,
    0x001F, 0x0002, 0x0001, 0x0009, 0x0008, 0x0000, 0x0070, 0x01C7,
    0x038C, 0x071A, 0x38D8, 0x38DB, 0x38DA, 0x38DD, 0x38DC, 0x000D,
    0x001D, 0x000E, 0x003F, 0x003C, 0x000C, 0x0006, 0x003D, 0x001E,
  ], [
    0x0006, 0x000B, 0x0011, 0x001E, 0x0074, 0x03AA, 0x1D5C, 0x0001,
    0x0021, 0x0001, 0x0002, 0x0007, 0x0006, 0x003E, 0x00EB, 0x01D4,
    0x0EAF, 0x3ABB, 0x3ABA, 0x1D59, 0x1D58, 0x1D5B, 0x1D5A, 0x000A,
    0x001C, 0x0001, 0x003F, 0x003B, 0x0001, 0x0009, 0x0020, 0x0000,
  ], [
    0x0004, 0x000A, 0x0017, 0x0004, 0x0016, 0x016A, 0x16B1, 0x0017,
    0x005B, 0x0006, 0x0007, 0x0001, 0x0000, 0x000A, 0x02D7, 0x0B5A,
    0x16B0, 0x16B3, 0x16B2, 0x2D6D, 0x2D6C, 0x2D6F, 0x2D6E, 0x0006,
    0x000A, 0x0004, 0x002C, 0x0017, 0x0003, 0x0007, 0x0016, 0x00B4,
  ], [
    0x0005, 0x000D, 0x0005, 0x0009, 0x0033, 0x0193, 0x192C, 0x0061,
    0x0031, 0x0000, 0x0007, 0x0010, 0x0011, 0x00C8, 0x192F, 0x325B,
    0x325A, 0x1929, 0x1928, 0x192B, 0x192A, 0x325D, 0x325C, 0x0018,
    0x001A, 0x001B, 0x0065, 0x0019, 0x0004, 0x0007, 0x0060, 0x0324,
  ], [
    0x0006, 0x0000, 0x0002, 0x000F, 0x0039, 0x01D9, 0x1D82, 0x0761,
    0x03BE, 0x0001, 0x0002, 0x000F, 0x000E, 0x0762, 0x3B07, 0x3B06,
    0x3B1D, 0x3B1C, 0x3B1F, 0x3B1E, 0x3B19, 0x3B18, 0x3B1B, 0x0038,
    0x01DE, 0x00ED, 0x03BF, 0x00EE, 0x003A, 0x0006, 0x0EC0, 0x3B1A,
  ], [
    0x0000, 0x0002, 0x000F, 0x0006, 0x001C, 0x01D0, 0x0E8C, 0x1D1B,
    0x1D1A, 0x0003, 0x0002, 0x00EA, 0x00E9, 0x0E89, 0x0E88, 0x0E8B,
    0x0E8A, 0x1D65, 0x1D64, 0x1D67, 0x1D66, 0x1D61, 0x1D60, 0x03AD,
    0x1D63, 0x1D62, 0x1D1D, 0x1D1C, 0x003B, 0x01D7, 0x1D1F, 0x1D1E,
  ], [
    0x0002, 0x000F, 0x001C, 0x000C, 0x003B, 0x01AC, 0x1AD8, 0x35B3,
    0x35B2, 0x0001, 0x0000, 0x0069, 0x0068, 0x35BD, 0x35BC, 0x35BF,
    0x35BE, 0x35B9, 0x35B8, 0x35BB, 0x35BA, 0x35B5, 0x35B4, 0x01A9,
    0x01A8, 0x035A, 0x00D7, 0x00D5, 0x003A, 0x001B, 0x35B7, 0x35B6,
  ]
];

const VP31_AC_CAT3_BITS: [[u8; 32]; 16] = [
  [
     3,  5,  7,  7,  9, 12, 12,  5,  7,  3,  3,  4,  4,  4,  5,  7,
     7,  8, 10, 13, 13, 13, 13,  4,  5,  5,  6,  6,  4,  6,  5,  5,
  ], [
     3,  5,  6,  6,  7, 10, 13,  5,  6,  3,  3,  4,  4,  5,  6,  8,
    10, 10, 13, 13, 13, 12, 12,  4,  5,  5,  5,  6,  4,  5,  5,  5,
  ], [
     3,  4,  6,  5,  7,  9, 13,  6,  6,  3,  3,  4,  4,  5,  8, 10,
    13, 13, 13, 13, 13, 13, 13,  4,  5,  5,  6,  6,  4,  4,  6,  6,
  ], [
     3,  4,  5,  4,  6,  8, 12,  7,  7,  3,  3,  4,  4,  7,  9, 13,
    13, 12, 12, 12, 12, 12, 12,  4,  5,  5,  6,  6,  4,  4,  7,  7,
  ], [
     3,  4,  5,  4,  5,  7, 10, 10,  7,  3,  3,  5,  5,  8, 13, 14,
    14, 13, 13, 13, 13, 13, 13,  4,  5,  5,  6,  6,  4,  3,  7, 10,
  ], [
     3,  4,  3,  3,  4,  6, 11, 13,  8,  3,  3,  6,  6, 13, 13, 13,
    13, 13, 13, 13, 14, 14, 14,  3,  6,  8,  9,  8,  6,  5, 12, 14,
  ], [
     3,  4,  3,  3,  4,  6, 11, 13,  8,  3,  3,  6,  6, 13, 13, 13,
    13, 13, 13, 13, 14, 14, 14,  3,  6,  8,  9,  8,  6,  5, 12, 14,
  ], [
     3,  4,  3,  3,  4,  6, 11, 13,  8,  3,  3,  6,  6, 13, 13, 13,
    13, 13, 13, 13, 14, 14, 14,  3,  6,  8,  9,  8,  6,  5, 12, 14,
  ], [
     3,  5,  6,  7,  9, 12, 12,  5,  7,  3,  3,  4,  4,  5,  6,  7,
     9,  9, 10, 13, 13, 13, 13,  4,  4,  5,  5,  6,  4,  5,  5,  6,
  ], [
     3,  4,  6,  5,  8, 13, 14,  6,  6,  3,  3,  4,  4,  5,  7,  9,
    10, 11, 14, 14, 14, 14, 14,  4,  5,  5,  6,  6,  4,  4,  6,  6,
  ], [
     3,  4,  5,  5,  7, 10, 13,  6,  6,  3,  3,  4,  4,  6,  8,  9,
    12, 14, 14, 13, 13, 13, 13,  4,  5,  5,  6,  6,  4,  4,  6,  6,
  ], [
     3,  4,  5,  4,  6,  9, 13,  7,  7,  3,  3,  4,  4,  6, 10, 12,
    13, 13, 13, 14, 14, 14, 14,  4,  5,  5,  6,  6,  4,  4,  7,  8,
  ], [
     3,  4,  4,  4,  6,  9, 13,  8,  7,  2,  3,  5,  5,  8, 13, 14,
    14, 13, 13, 13, 13, 14, 14,  5,  6,  6,  7,  6,  4,  4,  8, 10,
  ], [
     3,  3,  4,  4,  6,  9, 13, 11, 10,  2,  2,  6,  6, 11, 14, 14,
    14, 14, 14, 14, 14, 14, 14,  6,  9,  8, 10,  8,  6,  5, 12, 14,
  ], [
     2,  3,  5,  4,  6, 10, 13, 14, 14,  2,  2,  9,  9, 13, 13, 13,
    13, 14, 14, 14, 14, 14, 14, 11, 14, 14, 14, 14,  7, 10, 14, 14,
  ], [
     2,  4,  5,  4,  6,  9, 13, 14, 14,  2,  2,  7,  7, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14,  9,  9, 10,  8,  8,  6,  5, 14, 14,
  ]
];

const VP31_DC_WEIGHTS: [[i16; 5]; 16] = [
    [  0,   0,  0,  0,   0 ],
    [  1,   0,  0,  0,   1 ],
    [  0,   1,  0,  0,   1 ],
    [  1,   0,  0,  0,   1 ],

    [  0,   0,  1,  0,   1 ],
    [  1,   0,  1,  0,   2 ],
    [  0,   0,  1,  0,   1 ],
    [ 29, -26, 29,  0,  32 ],

    [  0,   0,  0,  1,   1 ],
    [ 75,   0,  0, 53, 128 ],
    [  0,   1,  0,  1,   2 ],
    [ 75,   0,  0, 53, 128 ],

    [  0,   0,  1,  0,   1 ],
    [ 75,   0,  0, 53, 128 ],
    [  0,   3, 10,  3,  16 ],
    [ 29, -26, 29,  0,  32 ],
];

const VP30_DC_SCALES: [i16; 64] = [
    24, 20, 20, 20, 20, 20, 20, 20,
    19, 19, 19, 19, 18, 18, 18, 18,
    17, 17, 17, 17, 16, 16, 15, 15,
    14, 14, 13, 13, 12, 12, 11, 11,
    10, 10,  9,  9,  8,  8,  7,  7,
     6,  6,  6,  6,  5,  5,  5,  5,
     4,  4,  4,  4,  3,  3,  3,  3,
     2,  2,  2,  2,  1,  1,  1,  1
];

const VP30_AC_SCALES: [i16; 64] = [
    3000, 2500, 2000, 1500, 1200, 1000, 900, 800,
     750,  700,  650,  630,  600,  550, 500, 450,
     410,  380,  350,  320,  290,  260, 240, 220,
     200,  180,  165,  150,  140,  130, 120, 115,
     110,  100,   95,   90,   85,   80,  75,  70,
      67,   65,   63,   61,   57,   55,  53,  50,
      49,   46,   44,   42,   39,   36,  33,  30,
      27,   24,   21,   19,   17,   15,  12,  10
];

const VP30_DC_CODES: [[u16; 32]; 5] = [
  [
    0x0005, 0x002D, 0x0004, 0x0009, 0x0088, 0x0225, 0x0224, 0x0005,
    0x0011, 0x0007, 0x0006, 0x0009, 0x000A, 0x0007, 0x0017, 0x000C,
    0x002C, 0x0005, 0x0008, 0x0003, 0x0012, 0x0010, 0x0113, 0x0003,
    0x0010, 0x0000, 0x0013, 0x001A, 0x0023, 0x0045, 0x0001, 0x001B
  ], [
    0x000B, 0x0012, 0x0029, 0x0010, 0x000D, 0x00A2, 0x0020, 0x0009,
    0x0050, 0x0007, 0x0006, 0x0006, 0x0005, 0x0002, 0x0008, 0x0027,
    0x0005, 0x0022, 0x0023, 0x0057, 0x00A3, 0x0011, 0x0021, 0x0007,
    0x0000, 0x0009, 0x002A, 0x0003, 0x0007, 0x0026, 0x000C, 0x0056
  ], [
    0x000D, 0x0018, 0x0009, 0x0017, 0x0033, 0x0056, 0x00F7, 0x00F1,
    0x007A, 0x0000, 0x0007, 0x0009, 0x0008, 0x0005, 0x000D, 0x002D,
    0x0010, 0x001D, 0x001C, 0x0057, 0x00CB, 0x00F6, 0x00F0, 0x0014,
    0x000C, 0x002C, 0x0011, 0x001F, 0x002A, 0x0064, 0x00CA, 0x0079
  ], [
    0x000F, 0x001A, 0x0013, 0x001B, 0x003B, 0x0072, 0x01D3, 0x0707,
    0x0E0D, 0x0001, 0x0000, 0x000C, 0x000B, 0x0008, 0x0012, 0x002A,
    0x0073, 0x0028, 0x0075, 0x0056, 0x0052, 0x01C0, 0x0E0C, 0x0071,
    0x0057, 0x00E1, 0x00A6, 0x00E8, 0x00A7, 0x03A5, 0x03A4, 0x0382
  ], [
    0x000F, 0x001B, 0x0014, 0x001D, 0x0010, 0x0073, 0x00E2, 0x023C,
    0x11C9, 0x0001, 0x0000, 0x000C, 0x000B, 0x0009, 0x0015, 0x0035,
    0x0072, 0x0034, 0x0022, 0x0070, 0x0046, 0x011D, 0x11C8, 0x01C7,
    0x01C6, 0x0238, 0x047E, 0x023E, 0x0473, 0x08E5, 0x023D, 0x047F
  ]
];
const VP30_DC_BITS: [[u8; 32]; 5] = [
  [
     4,  6,  6,  6,  8, 10, 10,  6,  7,  3,  3,  4,  4,  4,  5,  5,
     6,  5,  5,  5,  6,  7,  9,  4,  5,  5,  6,  6,  6,  7,  5,  6
  ], [
     4,  5,  6,  5,  6,  8,  9,  7,  7,  3,  3,  4,  4,  4,  5,  6,
     6,  6,  6,  7,  8,  8,  9,  4,  4,  5,  6,  5,  5,  6,  6,  7
  ], [
     4,  5,  5,  5,  6,  7,  9,  9,  8,  2,  3,  4,  4,  4,  5,  6,
     6,  6,  6,  7,  8,  9,  9,  5,  5,  6,  6,  6,  6,  7,  8,  8
  ], [
     4,  5,  5,  5,  6,  7,  9, 11, 12,  2,  2,  4,  4,  4,  5,  6,
     7,  6,  7,  7,  7,  9, 12,  7,  7,  8,  8,  8,  8, 10, 10, 10
  ], [
     4,  5,  5,  5,  5,  7,  8, 10, 13,  2,  2,  4,  4,  4,  5,  6,
     7,  6,  6,  7,  7,  9, 13,  9,  9, 10, 11, 10, 11, 12, 10, 11
  ]
];
const VP30_AC_INTRA_CODES: [[u16; 32]; 5] = [
  [
    0x0008, 0x0033, 0x0008, 0x004B, 0x0089, 0x0221, 0x0220, 0x001F,
    0x0045, 0x0000, 0x000E, 0x000B, 0x000A, 0x000D, 0x0006, 0x001E,
    0x000A, 0x0018, 0x0013, 0x0005, 0x0009, 0x0046, 0x0111, 0x0007,
    0x000B, 0x0032, 0x0010, 0x004A, 0x0024, 0x0047, 0x0003, 0x0009
  ], [
    0x000E, 0x000E, 0x007B, 0x001E, 0x007E, 0x03EF, 0x07DD, 0x0018,
    0x00FA, 0x0002, 0x0000, 0x000A, 0x0008, 0x000B, 0x0003, 0x0012,
    0x0033, 0x000C, 0x003C, 0x001A, 0x007F, 0x01F6, 0x07DC, 0x000D,
    0x0013, 0x0004, 0x001B, 0x007A, 0x0032, 0x007C, 0x001F, 0x0005
  ], [
    0x0000, 0x0018, 0x0034, 0x000C, 0x006A, 0x01F9, 0x07EA, 0x0016,
    0x0FD7, 0x0002, 0x0001, 0x000A, 0x0009, 0x0007, 0x001B, 0x003E,
    0x0020, 0x0021, 0x006B, 0x01FB, 0x03F4, 0x1FAD, 0x1FAC, 0x000E,
    0x0019, 0x0011, 0x002F, 0x007F, 0x002E, 0x01F8, 0x001E, 0x000D
  ], [
    0x000E, 0x0016, 0x002E, 0x0003, 0x006E, 0x008B, 0x0113, 0x0018,
    0x0221, 0x0001, 0x0002, 0x000A, 0x0009, 0x0007, 0x001A, 0x0002,
    0x001B, 0x0023, 0x006F, 0x008A, 0x0111, 0x0441, 0x0440, 0x000F,
    0x0019, 0x0010, 0x0036, 0x001A, 0x002F, 0x0112, 0x0000, 0x000C
  ], [
    0x000E, 0x000F, 0x001B, 0x0033, 0x005A, 0x00B6, 0x0008, 0x001A,
    0x004D, 0x0001, 0x0002, 0x000A, 0x0009, 0x0008, 0x001B, 0x0003,
    0x002C, 0x002E, 0x0005, 0x00B7, 0x0027, 0x0099, 0x0098, 0x000F,
    0x0018, 0x000E, 0x002F, 0x001A, 0x0032, 0x0012, 0x0000, 0x000C
  ]
];
const VP30_AC_INTRA_BITS: [[u8; 32]; 5] = [
  [
     4,  6,  6,  7,  9, 11, 11,  5,  8,  3,  4,  4,  4,  4,  4,  5,
     5,  5,  5,  5,  6,  8, 10,  4,  5,  6,  6,  7,  6,  8,  4,  5
  ], [
     4,  5,  7,  6,  8, 11, 12,  5,  9,  3,  3,  4,  4,  4,  4,  5,
     6,  5,  6,  6,  8, 10, 12,  4,  5,  5,  6,  7,  6,  8,  5,  5
  ], [
     3,  5,  6,  5,  7,  9, 11,  5, 12,  3,  3,  4,  4,  4,  5,  6,
     6,  6,  7,  9, 10, 13, 13,  4,  5,  5,  6,  7,  6,  9,  5,  5
  ], [
     4,  5,  6,  5,  7,  8,  9,  5, 10,  3,  3,  4,  4,  4,  5,  5,
     6,  6,  7,  8,  9, 11, 11,  4,  5,  5,  6,  6,  6,  9,  4,  5
  ], [
     4,  5,  6,  6,  7,  8,  7,  5, 10,  3,  3,  4,  4,  4,  5,  5,
     6,  6,  6,  8,  9, 11, 11,  4,  5,  5,  6,  6,  6,  8,  4,  5
  ]
];
const VP30_AC_INTER_CODES: [[u16; 32]; 5] = [
  [
    0x000D, 0x0038, 0x0061, 0x0060, 0x0393, 0x1C95, 0x1C94, 0x0014,
    0x0073, 0x0001, 0x0000, 0x000B, 0x0009, 0x001D, 0x000E, 0x0022,
    0x0046, 0x0047, 0x00E5, 0x01C8, 0x0724, 0x1C97, 0x1C96, 0x000F,
    0x0006, 0x0019, 0x000F, 0x0031, 0x0004, 0x0010, 0x0005, 0x0015
  ], [
    0x0004, 0x001B, 0x0030, 0x0034, 0x00D5, 0x06B3, 0x3595, 0x0031,
    0x001A, 0x0002, 0x0001, 0x001F, 0x001E, 0x000C, 0x001B, 0x00D7,
    0x00D4, 0x01AD, 0x0358, 0x0D64, 0x3594, 0x3597, 0x3596, 0x0000,
    0x000A, 0x001D, 0x0017, 0x0039, 0x0007, 0x0019, 0x0016, 0x0038
  ], [
    0x0005, 0x0009, 0x001A, 0x001E, 0x001F, 0x00E2, 0x038E, 0x0070,
    0x003B, 0x0001, 0x0000, 0x0019, 0x0018, 0x001E, 0x003A, 0x01C6,
    0x071E, 0x0E3E, 0x1C7E, 0x71FD, 0x71FC, 0x71FF, 0x71FE, 0x0002,
    0x0008, 0x001D, 0x001B, 0x0039, 0x001F, 0x000D, 0x000C, 0x001C
  ], [
    0x0003, 0x000B, 0x001C, 0x000D, 0x0004, 0x000A, 0x0076, 0x00E8,
    0x01DC, 0x0001, 0x0000, 0x0033, 0x0032, 0x00E9, 0x03BB, 0x0774,
    0x1DD5, 0x3BAD, 0x3BAC, 0x3BAF, 0x3BAE, 0x3BA9, 0x3BA8, 0x0004,
    0x000A, 0x001F, 0x001E, 0x0030, 0x000B, 0x0075, 0x0031, 0x00EF
  ], [
    0x0009, 0x001E, 0x000F, 0x000E, 0x000C, 0x0008, 0x0001, 0x00E3,
    0x00E2, 0x0002, 0x0000, 0x003A, 0x0039, 0x0070, 0x01DC, 0x0776,
    0x0775, 0x0EEF, 0x0EE8, 0x1DD3, 0x1DD2, 0x1DDD, 0x1DDC, 0x0005,
    0x001F, 0x001B, 0x0006, 0x006A, 0x0034, 0x0076, 0x006B, 0x00EF
  ]
];
const VP30_AC_INTER_BITS: [[u8; 32]; 5] = [
  [
     4,  6,  7,  7, 10, 13, 13,  5,  7,  3,  3,  4,  4,  5,  5,  6,
     7,  7,  8,  9, 11, 13, 13,  4,  4,  5,  5,  6,  4,  5,  4,  5
  ], [
     3,  5,  6,  6,  8, 11, 14,  6,  6,  3,  3,  5,  5,  5,  6,  8,
     8,  9, 10, 12, 14, 14, 14,  3,  4,  5,  5,  6,  4,  5,  5,  6
  ], [
     3,  4,  5,  5,  6,  8, 10,  7,  7,  3,  3,  5,  5,  6,  7,  9,
    11, 12, 13, 15, 15, 15, 15,  3,  4,  5,  5,  6,  5,  5,  5,  6
  ], [
     3,  4,  5,  4,  4,  5,  7,  8,  9,  3,  3,  6,  6,  8, 10, 11,
    13, 14, 14, 14, 14, 14, 14,  3,  4,  5,  5,  6,  5,  7,  6,  8
  ], [
     4,  5,  5,  4,  4,  4,  3,  9,  9,  3,  3,  7,  7,  8, 10, 12,
    12, 13, 13, 14, 14, 14, 14,  3,  5,  5,  4,  7,  6,  8,  7,  9
  ]
];
const VP30_MBTYPE_CODES: [u8; 7] = [ 0x00, 0x08, 0x0A, 0x03, 0x0B, 0x13, 0x12 ];
const VP30_MBTYPE_BITS: [u8; 7] = [ 1, 4, 4, 2, 4, 5, 5 ];
const VP30_MBTYPE_SYMS: [VPMBType; 7] = [
    VPMBType::InterNoMV,    VPMBType::Intra,        VPMBType::InterMV,      VPMBType::InterNearest,
    VPMBType::GoldenNoMV,   VPMBType::GoldenMV,     VPMBType::InterFourMV
];

const VP40_DC_CODES: [[u32; 32]; 16] = [
  [
    0x000C, 0x0070, 0x01CA, 0x01CB, 0x0391, 0x1C9B, 0x3935, 0x0071,
    0x3934, 0x000B, 0x000F, 0x0019, 0x0002, 0x0009, 0x0003, 0x001D,
    0x0018, 0x0007, 0x000D, 0x0002, 0x0000, 0x000A, 0x0008, 0x001A,
    0x0073, 0x006F, 0x0E4C, 0x0727, 0x0392, 0x0390, 0x0036, 0x006E
  ], [
    0x0011, 0x007A, 0x0083, 0x0040, 0x0105, 0x0413, 0x0410, 0x007B,
    0x0822, 0x000E, 0x0002, 0x0002, 0x0006, 0x000A, 0x0007, 0x001F,
    0x0017, 0x0009, 0x000D, 0x0000, 0x000C, 0x0003, 0x003C, 0x002C,
    0x0021, 0x0169, 0x0412, 0x02D0, 0x02D1, 0x0823, 0x005B, 0x00B5
  ], [
    0x0017, 0x0010, 0x00B6, 0x0022, 0x016A, 0x02D0, 0x0B48, 0x0077,
    0x1692, 0x0000, 0x0003, 0x0003, 0x0009, 0x000C, 0x0005, 0x0002,
    0x001C, 0x0008, 0x000D, 0x000F, 0x000A, 0x0009, 0x0023, 0x003A,
    0x002C, 0x016B, 0x05A5, 0x02D3, 0x02D1, 0x1693, 0x0076, 0x00B7
  ], [
    0x001E, 0x0013, 0x00FB, 0x007C, 0x0046, 0x07D6, 0x0FA9, 0x0012,
    0x1F50, 0x0001, 0x0004, 0x0005, 0x000A, 0x000E, 0x0007, 0x0000,
    0x0017, 0x0006, 0x000D, 0x000C, 0x0001, 0x002C, 0x008F, 0x003F,
    0x002D, 0x01F4, 0x07D5, 0x008E, 0x07D7, 0x1F51, 0x0010, 0x0022
  ], [
    0x0001, 0x002B, 0x0012, 0x0055, 0x0027, 0x03B0, 0x0762, 0x0077,
    0x0261, 0x0002, 0x0006, 0x0007, 0x000B, 0x000F, 0x0008, 0x0000,
    0x001C, 0x0003, 0x0009, 0x0006, 0x0014, 0x0054, 0x0131, 0x0005,
    0x003A, 0x01D9, 0x0099, 0x004D, 0x0763, 0x0260, 0x0008, 0x00ED
  ], [
    0x0004, 0x0033, 0x0060, 0x0065, 0x00C2, 0x030D, 0x0619, 0x0064,
    0x1862, 0x0004, 0x0007, 0x000A, 0x000B, 0x000D, 0x0006, 0x0000,
    0x000F, 0x0003, 0x0005, 0x0002, 0x0002, 0x0077, 0x0C30, 0x0003,
    0x0031, 0x0187, 0x01D9, 0x00ED, 0x01D8, 0x1863, 0x001C, 0x003A
  ], [
    0x0008, 0x000A, 0x006A, 0x0016, 0x001E, 0x034E, 0x069F, 0x0068,
    0x0D28, 0x0005, 0x0007, 0x0007, 0x000C, 0x0000, 0x0006, 0x001B,
    0x0012, 0x0002, 0x0004, 0x0013, 0x000E, 0x034B, 0x1A53, 0x0006,
    0x0017, 0x01A6, 0x069E, 0x01A4, 0x0695, 0x1A52, 0x006B, 0x001F
  ], [
    0x000E, 0x000F, 0x0017, 0x0025, 0x009F, 0x0138, 0x024B, 0x0093,
    0x092A, 0x0005, 0x0000, 0x0008, 0x000D, 0x000F, 0x0006, 0x0004,
    0x000E, 0x0019, 0x0018, 0x000A, 0x009E, 0x0494, 0x1256, 0x0026,
    0x0016, 0x0124, 0x04E5, 0x0273, 0x04E4, 0x1257, 0x0048, 0x009D
  ], [
    0x0004, 0x002C, 0x0050, 0x001E, 0x0071, 0x00E1, 0x00E0, 0x001D,
    0x0006, 0x0007, 0x0006, 0x0007, 0x0005, 0x0006, 0x0015, 0x0000,
    0x0029, 0x0002, 0x0006, 0x0001, 0x0023, 0x001F, 0x0039, 0x0009,
    0x0002, 0x0010, 0x0007, 0x002D, 0x002F, 0x002E, 0x0022, 0x0051
  ], [
    0x0008, 0x002F, 0x0051, 0x0050, 0x02ED, 0x05D9, 0x05D8, 0x00BA,
    0x005C, 0x0007, 0x0006, 0x0009, 0x0006, 0x0007, 0x0016, 0x0005,
    0x002B, 0x0006, 0x000A, 0x0001, 0x000F, 0x001D, 0x0177, 0x0004,
    0x0001, 0x0004, 0x0001, 0x002A, 0x000B, 0x0029, 0x0000, 0x001C
  ], [
    0x000A, 0x003C, 0x0074, 0x004E, 0x026D, 0x04D9, 0x04D8, 0x009A,
    0x004C, 0x0000, 0x0006, 0x0008, 0x0007, 0x0006, 0x0016, 0x0008,
    0x002E, 0x000A, 0x000B, 0x003D, 0x0024, 0x00EB, 0x0137, 0x001F,
    0x001C, 0x003B, 0x0012, 0x0025, 0x002F, 0x0013, 0x004F, 0x00EA
  ], [
    0x000A, 0x000A, 0x0003, 0x0016, 0x0009, 0x0021, 0x0020, 0x00B3,
    0x0058, 0x0007, 0x0006, 0x0007, 0x0006, 0x0004, 0x0013, 0x0002,
    0x0025, 0x0000, 0x0003, 0x002D, 0x005D, 0x00B2, 0x0011, 0x0008,
    0x0002, 0x0006, 0x0017, 0x002F, 0x0007, 0x0024, 0x005C, 0x0005
  ], [
    0x000B, 0x0013, 0x001F, 0x0031, 0x0021, 0x0295, 0x0528, 0x00A4,
    0x003C, 0x0000, 0x0007, 0x0006, 0x0005, 0x001B, 0x0012, 0x0032,
    0x001D, 0x002B, 0x0030, 0x001C, 0x003D, 0x014B, 0x0529, 0x0008,
    0x001A, 0x0033, 0x0011, 0x002A, 0x0009, 0x0028, 0x0053, 0x0020
  ], [
    0x000E, 0x0015, 0x0029, 0x003F, 0x004D, 0x02F1, 0x05E0, 0x0092,
    0x0048, 0x0000, 0x0006, 0x0006, 0x0005, 0x0004, 0x000F, 0x002E,
    0x001D, 0x0028, 0x0027, 0x005F, 0x00BD, 0x0179, 0x05E1, 0x0008,
    0x001E, 0x002D, 0x001C, 0x002C, 0x003E, 0x0025, 0x004C, 0x0093
  ], [
    0x000C, 0x0017, 0x0035, 0x0013, 0x0021, 0x00AD, 0x06F1, 0x01BD,
    0x00D9, 0x0000, 0x0007, 0x0007, 0x0006, 0x0004, 0x0011, 0x002A,
    0x006E, 0x0025, 0x0024, 0x0057, 0x00D8, 0x0379, 0x06F0, 0x0005,
    0x0016, 0x0029, 0x006D, 0x0028, 0x0034, 0x0020, 0x00DF, 0x00AC
  ], [
    0x0000, 0x001A, 0x0006, 0x0019, 0x0030, 0x005A, 0x018A, 0x02DD,
    0x018B, 0x0001, 0x0007, 0x000A, 0x0009, 0x0002, 0x0010, 0x002E,
    0x006E, 0x002C, 0x000E, 0x005E, 0x00C4, 0x05B9, 0x05B8, 0x0011,
    0x0036, 0x005F, 0x001E, 0x0063, 0x006F, 0x001F, 0x00B6, 0x016F
  ]
];
const VP40_DC_BITS: [[u8; 32]; 16] = [
  [
    5,  7,  9,  9, 10, 13, 14,  7, 14,  4,  4,  5,  4,  4,  4,  5,
    5,  4,  4,  3,  3,  4,  4,  6,  7,  8, 12, 11, 10, 10,  7,  8
  ], [
    5,  7,  8,  7,  9, 11, 11,  7, 12,  4,  3,  4,  4,  4,  4,  5,
    5,  4,  4,  3,  4,  4,  6,  6,  6,  9, 11, 10, 10, 12,  7,  8
  ], [
    5,  6,  8,  7,  9, 10, 12,  7, 13,  3,  3,  4,  4,  4,  4,  4,
    5,  4,  4,  4,  4,  5,  7,  6,  6,  9, 11, 10, 10, 13,  7,  8
  ], [
    5,  6,  8,  7,  8, 11, 12,  6, 13,  3,  3,  4,  4,  4,  4,  4,
    5,  4,  4,  4,  4,  6,  9,  6,  6,  9, 11,  9, 11, 13,  6,  7
  ], [
    4,  6,  7,  7,  8, 10, 11,  7, 12,  3,  3,  4,  4,  4,  4,  4,
    5,  4,  4,  4,  5,  7, 11,  5,  6,  9, 10,  9, 11, 12,  6,  8
  ], [
    4,  6,  7,  7,  8, 10, 11,  7, 13,  3,  3,  4,  4,  4,  4,  4,
    5,  4,  4,  4,  5,  8, 12,  5,  6,  9, 10,  9, 10, 13,  6,  7
  ], [
    4,  5,  7,  6,  7, 10, 11,  7, 12,  3,  3,  4,  4,  3,  4,  5,
    5,  4,  4,  5,  6, 10, 13,  5,  6,  9, 11,  9, 11, 13,  7,  7
  ], [
    4,  5,  6,  6,  8,  9, 10,  8, 12,  3,  2,  4,  4,  4,  4,  4,
    5,  5,  5,  5,  8, 11, 13,  6,  6,  9, 11, 10, 11, 13,  7,  8
  ], [
    4,  6,  7,  7,  9, 10, 10,  7,  6,  3,  3,  4,  4,  4,  5,  5,
    6,  5,  5,  5,  6,  7,  8,  4,  4,  5,  6,  6,  6,  6,  6,  7
  ], [
    4,  6,  7,  7, 10, 11, 11,  8,  7,  3,  3,  4,  4,  4,  5,  5,
    6,  5,  5,  5,  6,  7,  9,  4,  4,  5,  6,  6,  5,  6,  6,  7
  ], [
    4,  6,  7,  7, 10, 11, 11,  8,  7,  2,  3,  4,  4,  4,  5,  5,
    6,  5,  5,  6,  6,  8,  9,  5,  5,  6,  6,  6,  6,  6,  7,  8
  ], [
    4,  5,  6,  6,  8, 10, 10,  8,  7,  3,  3,  4,  4,  4,  5,  5,
    6,  5,  5,  6,  7,  8,  9,  4,  4,  5,  6,  6,  5,  6,  7,  7
  ], [
    4,  5,  6,  6,  7, 10, 11,  8,  7,  2,  3,  4,  4,  5,  5,  6,
    6,  6,  6,  6,  7,  9, 11,  4,  5,  6,  6,  6,  5,  6,  7,  7
  ], [
    4,  5,  6,  6,  7, 10, 11,  8,  7,  2,  3,  4,  4,  4,  5,  6,
    6,  6,  6,  7,  8,  9, 11,  4,  5,  6,  6,  6,  6,  6,  7,  8
  ], [
    4,  5,  6,  5,  6,  8, 11,  9,  8,  2,  3,  4,  4,  4,  5,  6,
    7,  6,  6,  7,  8, 10, 11,  4,  5,  6,  7,  6,  6,  6,  8,  8
  ], [
    3,  5,  5,  5,  6,  7,  9, 10,  9,  2,  3,  4,  4,  4,  5,  6,
    7,  6,  6,  7,  8, 11, 11,  5,  6,  7,  7,  7,  7,  7,  8,  9
  ]
];
const VP40_AC_CAT0_CODES: [[u32; 32]; 16] = [
  [
    0x0006, 0x001E, 0x01CC, 0x01CE, 0x0734, 0x1CD5, 0x1CD4, 0x0018,
    0x0E6B, 0x0000, 0x000F, 0x0006, 0x0007, 0x000D, 0x0008, 0x0002,
    0x0019, 0x0005, 0x000B, 0x000A, 0x001D, 0x0027, 0x01CF, 0x0004,
    0x0038, 0x000E, 0x004C, 0x001F, 0x004D, 0x039B, 0x0012, 0x0072
  ], [
    0x0009, 0x004B, 0x0090, 0x0091, 0x0745, 0x1D11, 0x1D10, 0x0019,
    0x0E89, 0x0000, 0x000F, 0x0008, 0x0007, 0x000D, 0x000B, 0x0002,
    0x001C, 0x0003, 0x000A, 0x0005, 0x0018, 0x0010, 0x01D0, 0x0006,
    0x003B, 0x0011, 0x004A, 0x0049, 0x00E9, 0x03A3, 0x0013, 0x0075
  ], [
    0x0019, 0x0074, 0x001D, 0x00EA, 0x0073, 0x01CA, 0x0396, 0x001C,
    0x00E4, 0x0002, 0x0001, 0x0007, 0x0008, 0x000D, 0x0009, 0x001F,
    0x0018, 0x0000, 0x0006, 0x001E, 0x003B, 0x00EB, 0x0397, 0x000A,
    0x0002, 0x002C, 0x005B, 0x005A, 0x000F, 0x0038, 0x0017, 0x0006
  ], [
    0x001E, 0x006F, 0x00AE, 0x00AF, 0x0187, 0x061B, 0x0C35, 0x001A,
    0x030C, 0x0002, 0x0001, 0x0007, 0x0008, 0x000E, 0x0009, 0x001F,
    0x0014, 0x0000, 0x0001, 0x0019, 0x002A, 0x0060, 0x0C34, 0x000B,
    0x000D, 0x0036, 0x006E, 0x0056, 0x0031, 0x00C2, 0x0018, 0x0019
  ], [
    0x0001, 0x002C, 0x0005, 0x0015, 0x0008, 0x0097, 0x012D, 0x0017,
    0x004A, 0x0003, 0x0002, 0x0009, 0x000A, 0x000E, 0x0008, 0x001F,
    0x0007, 0x001E, 0x001B, 0x0004, 0x005A, 0x0024, 0x012C, 0x000C,
    0x0006, 0x0000, 0x0003, 0x005B, 0x0014, 0x0013, 0x001A, 0x000B
  ], [
    0x0004, 0x0000, 0x0017, 0x0063, 0x018B, 0x0310, 0x0C44, 0x0019,
    0x0623, 0x0004, 0x0003, 0x000A, 0x000B, 0x000D, 0x0003, 0x001C,
    0x0003, 0x000A, 0x0004, 0x0003, 0x018A, 0x188B, 0x188A, 0x000F,
    0x000B, 0x0002, 0x000A, 0x0002, 0x0016, 0x0189, 0x001D, 0x0030
  ], [
    0x000D, 0x0003, 0x0077, 0x000D, 0x0082, 0x020D, 0x0830, 0x0019,
    0x0419, 0x0003, 0x0002, 0x000A, 0x0009, 0x000B, 0x0002, 0x0011,
    0x0039, 0x0002, 0x0021, 0x0040, 0x1063, 0x20C5, 0x20C4, 0x000F,
    0x0018, 0x0007, 0x0038, 0x000C, 0x0076, 0x0107, 0x0000, 0x003A
  ], [
    0x000F, 0x001C, 0x0036, 0x0008, 0x0061, 0x0091, 0x0243, 0x0009,
    0x0120, 0x0005, 0x0003, 0x0008, 0x0005, 0x0001, 0x0013, 0x0031,
    0x0076, 0x0060, 0x0093, 0x0909, 0x0908, 0x090B, 0x090A, 0x0001,
    0x001A, 0x0019, 0x003A, 0x0025, 0x0077, 0x0092, 0x0000, 0x0037
  ], [
    0x001F, 0x0079, 0x00F1, 0x00F0, 0x011B, 0x0469, 0x0468, 0x003B,
    0x0022, 0x0005, 0x0004, 0x0007, 0x0005, 0x0006, 0x001C, 0x0001,
    0x0035, 0x003D, 0x003A, 0x0010, 0x0047, 0x008C, 0x0235, 0x0001,
    0x0001, 0x0019, 0x0000, 0x0030, 0x0009, 0x0031, 0x001B, 0x0034
  ], [
    0x0003, 0x001B, 0x00F3, 0x00FD, 0x03C9, 0x0F20, 0x1E42, 0x003D,
    0x00FC, 0x0006, 0x0004, 0x0002, 0x0000, 0x0001, 0x0017, 0x003E,
    0x001A, 0x0039, 0x002B, 0x0078, 0x01E5, 0x0791, 0x1E43, 0x0002,
    0x0007, 0x001D, 0x000C, 0x0038, 0x0014, 0x007F, 0x0016, 0x002A
  ], [
    0x0007, 0x0039, 0x0051, 0x0078, 0x03CB, 0x0F29, 0x1E51, 0x003D,
    0x00F3, 0x0006, 0x0004, 0x0002, 0x0000, 0x0001, 0x0017, 0x003E,
    0x007F, 0x002B, 0x007E, 0x0050, 0x01E4, 0x0795, 0x1E50, 0x0002,
    0x0006, 0x001D, 0x0006, 0x0038, 0x0007, 0x0029, 0x0016, 0x002A
  ], [
    0x0008, 0x003B, 0x001D, 0x0072, 0x01CC, 0x0734, 0x1CD5, 0x003A,
    0x001C, 0x0006, 0x0005, 0x0002, 0x0001, 0x0000, 0x0012, 0x003E,
    0x007F, 0x001E, 0x007E, 0x00E7, 0x039B, 0x0E6B, 0x1CD4, 0x0002,
    0x0006, 0x001E, 0x000E, 0x0038, 0x0006, 0x000F, 0x0013, 0x001F
  ], [
    0x000D, 0x003F, 0x0073, 0x000C, 0x00E4, 0x072B, 0x0E54, 0x003A,
    0x001A, 0x0005, 0x0004, 0x0002, 0x0001, 0x0000, 0x0007, 0x0038,
    0x0076, 0x0077, 0x001B, 0x01CB, 0x0394, 0x1CAB, 0x1CAA, 0x0002,
    0x0006, 0x001E, 0x000E, 0x003E, 0x0019, 0x001F, 0x0018, 0x001E
  ], [
    0x000E, 0x0007, 0x000C, 0x001C, 0x00BD, 0x02F3, 0x0BC9, 0x001F,
    0x00BF, 0x0006, 0x0004, 0x0002, 0x0001, 0x001E, 0x0001, 0x000D,
    0x003A, 0x003B, 0x00BE, 0x0178, 0x05E5, 0x1791, 0x1790, 0x0002,
    0x0006, 0x001F, 0x0016, 0x0000, 0x0015, 0x002E, 0x0014, 0x001E
  ], [
    0x0000, 0x001B, 0x0031, 0x003A, 0x0060, 0x006F, 0x01B9, 0x000E,
    0x001A, 0x0005, 0x0003, 0x0002, 0x001F, 0x001A, 0x0039, 0x000C,
    0x00C3, 0x00C2, 0x0036, 0x00DD, 0x0370, 0x06E3, 0x06E2, 0x0002,
    0x0008, 0x001E, 0x0019, 0x003B, 0x0012, 0x000F, 0x0013, 0x0038
  ], [
    0x0002, 0x0000, 0x0003, 0x001C, 0x0032, 0x001C, 0x0199, 0x0004,
    0x00CD, 0x0004, 0x0003, 0x001B, 0x001A, 0x003D, 0x0067, 0x003B,
    0x0198, 0x0075, 0x00E9, 0x03A1, 0x03A0, 0x03A3, 0x03A2, 0x0005,
    0x0002, 0x001F, 0x001D, 0x003C, 0x0018, 0x000F, 0x0006, 0x0005
  ]
];
const VP40_AC_CAT0_BITS: [[u8; 32]; 16] = [
  [
     5,  7,  9,  9, 11, 13, 13,  5, 12,  3,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  5,  6,  9,  4,  6,  6,  7,  7,  7, 10,  5,  7
  ], [
     5,  7,  8,  8, 11, 13, 13,  5, 12,  3,  4,  4,  4,  4,  4,  4,
     5,  4,  4,  4,  5,  6,  9,  4,  6,  6,  7,  7,  8, 10,  5,  7
  ], [
     5,  7,  8,  8, 10, 12, 13,  5, 11,  3,  3,  4,  4,  4,  4,  5,
     5,  4,  4,  5,  6,  8, 13,  4,  5,  6,  7,  7,  7,  9,  5,  6
  ], [
     5,  7,  8,  8, 10, 12, 13,  5, 11,  3,  3,  4,  4,  4,  4,  5,
     5,  4,  4,  5,  6,  8, 13,  4,  5,  6,  7,  7,  7,  9,  5,  6
  ], [
     4,  6,  7,  7,  8, 12, 13,  5, 11,  3,  3,  4,  4,  4,  4,  5,
     5,  5,  5,  5,  7, 10, 13,  4,  5,  5,  6,  7,  7,  9,  5,  6
  ], [
     4,  5,  7,  7,  9, 10, 12,  5, 11,  3,  3,  4,  4,  4,  4,  5,
     5,  5,  5,  6,  9, 13, 13,  4,  5,  5,  6,  6,  7,  9,  5,  6
  ], [
     4,  5,  7,  6,  8, 10, 12,  5, 11,  3,  3,  4,  4,  4,  4,  5,
     6,  5,  6,  7, 13, 14, 14,  4,  5,  5,  6,  6,  7,  9,  4,  6
  ], [
     4,  5,  6,  5,  7,  8, 10,  5,  9,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  8, 12, 12, 12, 12,  3,  5,  5,  6,  6,  7,  8,  4,  6
  ], [
     5,  7,  8,  8, 10, 12, 12,  6,  7,  3,  3,  4,  4,  4,  5,  5,
     6,  6,  6,  6,  8,  9, 11,  3,  4,  5,  5,  6,  5,  6,  5,  6
  ], [
     4,  6,  8,  8, 10, 12, 13,  6,  8,  3,  3,  4,  4,  4,  5,  6,
     6,  6,  6,  7,  9, 11, 13,  3,  4,  5,  5,  6,  5,  7,  5,  6
  ], [
     4,  6,  7,  7, 10, 12, 13,  6,  8,  3,  3,  4,  4,  4,  5,  6,
     7,  6,  7,  7,  9, 11, 13,  3,  4,  5,  5,  6,  5,  6,  5,  6
  ], [
     4,  6,  7,  7,  9, 11, 13,  6,  7,  3,  3,  4,  4,  4,  5,  6,
     7,  6,  7,  8, 10, 12, 13,  3,  4,  5,  5,  6,  5,  6,  5,  6
  ], [
     4,  6,  7,  6,  8, 11, 12,  6,  7,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  7,  9, 10, 13, 13,  3,  4,  5,  5,  6,  5,  6,  5,  6
  ], [
     4,  5,  6,  6,  8, 10, 12,  6,  8,  3,  3,  4,  4,  5,  5,  6,
     7,  7,  8,  9, 11, 13, 13,  3,  4,  5,  5,  5,  5,  6,  5,  6
  ], [
     3,  5,  6,  6,  7,  9, 11,  6,  7,  3,  3,  4,  5,  5,  6,  6,
     8,  8,  8, 10, 12, 13, 13,  3,  4,  5,  5,  6,  5,  6,  5,  6
  ], [
     3,  4,  5,  5,  6,  7,  9,  6,  8,  3,  3,  5,  5,  6,  7,  8,
     9,  9, 10, 12, 12, 12, 12,  3,  4,  5,  5,  6,  5,  6,  5,  6
  ]
];
const VP40_AC_CAT1_CODES: [[u32; 32]; 16] = [
  [
    0x0004, 0x00F5, 0x0182, 0x060F, 0x1839, 0x1838, 0x183B, 0x0013,
    0x00C0, 0x0003, 0x0002, 0x000B, 0x000A, 0x000E, 0x0008, 0x0001,
    0x0012, 0x001F, 0x0000, 0x0006, 0x007B, 0x0306, 0x183A, 0x000D,
    0x0007, 0x0031, 0x000A, 0x0061, 0x003C, 0x00F4, 0x0019, 0x000B
  ], [
    0x000A, 0x001A, 0x01D8, 0x03B3, 0x0ECA, 0x1D96, 0x3B2F, 0x0014,
    0x0036, 0x0004, 0x0003, 0x000C, 0x000B, 0x0000, 0x0004, 0x001C,
    0x0005, 0x0015, 0x0007, 0x0017, 0x0037, 0x0764, 0x3B2E, 0x000F,
    0x001A, 0x003A, 0x000C, 0x0077, 0x0004, 0x00ED, 0x001B, 0x0016
  ], [
    0x001A, 0x002D, 0x0058, 0x01F4, 0x07D4, 0x1F55, 0x1F54, 0x0014,
    0x0059, 0x0004, 0x0003, 0x000B, 0x000C, 0x000E, 0x0004, 0x0015,
    0x0005, 0x0007, 0x0004, 0x007C, 0x03EB, 0x1F57, 0x1F56, 0x0000,
    0x001B, 0x003F, 0x000D, 0x000C, 0x000A, 0x00FB, 0x001E, 0x0017
  ], [
    0x0000, 0x0075, 0x004A, 0x0097, 0x025B, 0x0969, 0x0968, 0x000B,
    0x00E8, 0x0005, 0x0004, 0x0007, 0x000C, 0x000D, 0x0001, 0x000A,
    0x0039, 0x003B, 0x0018, 0x00E9, 0x012C, 0x096B, 0x096A, 0x0001,
    0x001F, 0x0008, 0x0019, 0x0013, 0x000D, 0x0024, 0x001E, 0x0038
  ], [
    0x0004, 0x0014, 0x006E, 0x0057, 0x0159, 0x0562, 0x0AC7, 0x000B,
    0x006F, 0x0006, 0x0005, 0x0008, 0x0009, 0x0007, 0x001E, 0x0002,
    0x0007, 0x0006, 0x002A, 0x00AD, 0x0AC6, 0x0561, 0x0560, 0x0001,
    0x001F, 0x000C, 0x0039, 0x001A, 0x0000, 0x0036, 0x001D, 0x0038
  ], [
    0x0007, 0x001B, 0x000E, 0x000D, 0x03E1, 0x1F06, 0x3E0F, 0x0002,
    0x00F9, 0x0005, 0x0006, 0x0008, 0x0009, 0x0004, 0x000C, 0x001A,
    0x000F, 0x000C, 0x01F1, 0x07C0, 0x3E0E, 0x1F05, 0x1F04, 0x0001,
    0x0000, 0x001C, 0x003F, 0x003D, 0x0005, 0x007D, 0x001D, 0x003C
  ], [
    0x000F, 0x000A, 0x0071, 0x0006, 0x01C2, 0x0702, 0x1C0E, 0x0002,
    0x000E, 0x0005, 0x0004, 0x0006, 0x0007, 0x001D, 0x0017, 0x000F,
    0x01C3, 0x01C1, 0x0380, 0x381F, 0x381E, 0x1C0D, 0x1C0C, 0x0001,
    0x0004, 0x0018, 0x0001, 0x0000, 0x000D, 0x0016, 0x0019, 0x0039
  ], [
    0x0002, 0x001E, 0x003B, 0x000D, 0x0061, 0x01FA, 0x1FB5, 0x0031,
    0x00FC, 0x0004, 0x0005, 0x0001, 0x0007, 0x003A, 0x0060, 0x03F7,
    0x07EC, 0x1FB7, 0x3F6C, 0x7EDB, 0x7EDA, 0x3F69, 0x3F68, 0x0001,
    0x0000, 0x0019, 0x003E, 0x0039, 0x000D, 0x0038, 0x000C, 0x007F
  ], [
    0x001E, 0x0070, 0x0127, 0x0126, 0x0492, 0x124D, 0x124C, 0x0001,
    0x007F, 0x0006, 0x0005, 0x0005, 0x0004, 0x0001, 0x0007, 0x0025,
    0x0071, 0x007E, 0x0048, 0x0125, 0x0248, 0x124F, 0x124E, 0x0003,
    0x0008, 0x001D, 0x0006, 0x003E, 0x0002, 0x0000, 0x0013, 0x0039
  ], [
    0x0001, 0x0001, 0x00E7, 0x0091, 0x0240, 0x120D, 0x120C, 0x003C,
    0x0000, 0x0006, 0x0005, 0x0005, 0x0004, 0x001F, 0x0004, 0x0025,
    0x0072, 0x0049, 0x00E6, 0x0121, 0x0482, 0x120F, 0x120E, 0x0003,
    0x0008, 0x001D, 0x0005, 0x003D, 0x0003, 0x0001, 0x0013, 0x0038
  ], [
    0x0004, 0x000F, 0x00F4, 0x005B, 0x02D3, 0x0B4A, 0x1697, 0x003C,
    0x000E, 0x0006, 0x0005, 0x0002, 0x0001, 0x001D, 0x0000, 0x007B,
    0x002C, 0x00F5, 0x00B5, 0x0168, 0x1696, 0x0B49, 0x0B48, 0x0003,
    0x0009, 0x001F, 0x000A, 0x0001, 0x0008, 0x0006, 0x001C, 0x0017
  ], [
    0x0008, 0x0039, 0x001A, 0x0003, 0x00DB, 0x06D6, 0x0DAF, 0x003C,
    0x000C, 0x0006, 0x0005, 0x0002, 0x0001, 0x001D, 0x003D, 0x0000,
    0x0002, 0x0037, 0x006C, 0x01B4, 0x0DAE, 0x06D5, 0x06D4, 0x0002,
    0x0007, 0x001F, 0x0007, 0x0001, 0x0009, 0x000D, 0x000C, 0x0038
  ], [
    0x000F, 0x0004, 0x002F, 0x002E, 0x0054, 0x0555, 0x0554, 0x0016,
    0x000E, 0x0006, 0x0005, 0x0001, 0x0000, 0x0009, 0x000B, 0x0014,
    0x0057, 0x0056, 0x00AB, 0x0557, 0x0556, 0x02A9, 0x02A8, 0x0003,
    0x0008, 0x0013, 0x000A, 0x0008, 0x000E, 0x0012, 0x0006, 0x000F
  ], [
    0x0001, 0x000E, 0x0006, 0x0004, 0x00DA, 0x0DBE, 0x1B7E, 0x0007,
    0x001A, 0x0005, 0x0004, 0x001C, 0x001B, 0x003A, 0x0037, 0x006C,
    0x01B6, 0x036E, 0x0DBD, 0x36FF, 0x36FE, 0x1B79, 0x1B78, 0x0002,
    0x000C, 0x0000, 0x000F, 0x000C, 0x000F, 0x001A, 0x003B, 0x0005
  ], [
    0x0005, 0x001E, 0x003A, 0x003E, 0x00FC, 0x0FD7, 0x3F55, 0x0077,
    0x0030, 0x0003, 0x0004, 0x001A, 0x0019, 0x007F, 0x01FB, 0x03F4,
    0x0FD6, 0x1FA9, 0x3F54, 0x3F57, 0x3F56, 0x3F51, 0x3F50, 0x0001,
    0x0004, 0x001C, 0x000B, 0x000A, 0x0000, 0x001B, 0x0031, 0x0076
  ], [
    0x0005, 0x000C, 0x001B, 0x0008, 0x0038, 0x0015, 0x00A3, 0x00E6,
    0x0004, 0x0001, 0x0002, 0x0012, 0x0003, 0x000B, 0x0029, 0x00A0,
    0x0142, 0x0287, 0x0286, 0x0289, 0x0288, 0x028B, 0x028A, 0x000F,
    0x001D, 0x0013, 0x0001, 0x0000, 0x0003, 0x001A, 0x0072, 0x00E7
  ]
];
const VP40_AC_CAT1_BITS: [[u8; 32]; 16] = [
  [
     5,  8,  9, 11, 13, 13, 13,  5,  8,  3,  3,  4,  4,  4,  4,  4,
     5,  5,  4,  5,  7, 10, 13,  4,  5,  6,  6,  7,  6,  8,  5,  6
  ], [
     5,  7,  9, 10, 12, 13, 14,  5,  8,  3,  3,  4,  4,  3,  4,  5,
     5,  5,  5,  6,  8, 11, 14,  4,  5,  6,  6,  7,  5,  8,  5,  6
  ], [
     5,  7,  8,  9, 11, 13, 13,  5,  8,  3,  3,  4,  4,  4,  4,  5,
     5,  5,  5,  7, 10, 13, 13,  3,  5,  6,  6,  6,  5,  8,  5,  6
  ], [
     4,  7,  8,  9, 11, 13, 13,  5,  8,  3,  3,  4,  4,  4,  4,  5,
     6,  6,  6,  8, 10, 13, 13,  3,  5,  5,  6,  6,  5,  7,  5,  6
  ], [
     4,  6,  8,  8, 10, 12, 13,  5,  8,  3,  3,  4,  4,  4,  5,  5,
     6,  6,  7,  9, 13, 12, 12,  3,  5,  5,  6,  6,  4,  7,  5,  6
  ], [
     4,  6,  7,  7, 10, 13, 14,  5,  8,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  9, 11, 14, 13, 13,  3,  4,  5,  6,  6,  4,  7,  5,  6
  ], [
     4,  5,  7,  6,  9, 11, 13,  5,  7,  3,  3,  4,  4,  5,  6,  7,
     9,  9, 10, 14, 14, 13, 13,  3,  4,  5,  5,  5,  4,  6,  5,  6
  ], [
     3,  5,  6,  5,  7,  9, 13,  6,  8,  3,  3,  4,  4,  6,  7, 10,
    11, 13, 14, 15, 15, 14, 14,  3,  4,  5,  6,  6,  4,  6,  5,  7
  ], [
     5,  7,  9,  9, 11, 13, 13,  5,  7,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  7,  9, 10, 13, 13,  3,  4,  5,  5,  6,  4,  5,  5,  6
  ], [
     4,  6,  8,  8, 10, 13, 13,  6,  6,  3,  3,  4,  4,  5,  5,  6,
     7,  7,  8,  9, 11, 13, 13,  3,  4,  5,  5,  6,  4,  5,  5,  6
  ], [
     4,  6,  8,  8, 11, 13, 14,  6,  6,  3,  3,  4,  4,  5,  5,  7,
     7,  8,  9, 10, 14, 13, 13,  3,  4,  5,  5,  5,  4,  5,  5,  6
  ], [
     4,  6,  7,  7, 10, 13, 14,  6,  6,  3,  3,  4,  4,  5,  6,  6,
     7,  8,  9, 11, 14, 13, 13,  3,  4,  5,  5,  5,  4,  5,  5,  6
  ], [
     4,  5,  7,  7,  9, 13, 13,  6,  6,  3,  3,  4,  4,  5,  6,  7,
     9,  9, 10, 13, 13, 12, 12,  3,  4,  5,  5,  5,  4,  5,  5,  6
  ], [
     3,  5,  6,  6,  9, 13, 14,  6,  6,  3,  3,  5,  5,  6,  7,  8,
    10, 11, 13, 15, 15, 14, 14,  3,  4,  4,  5,  5,  4,  5,  6,  6
  ], [
     3,  5,  6,  6,  8, 12, 14,  7,  6,  3,  3,  5,  5,  7,  9, 10,
    12, 13, 14, 14, 14, 14, 14,  3,  4,  5,  5,  5,  3,  5,  6,  7
  ], [
     3,  4,  5,  4,  6,  8, 11,  8,  6,  3,  3,  5,  5,  7,  9, 11,
    12, 13, 13, 13, 13, 13, 13,  4,  5,  5,  5,  5,  3,  5,  7,  8
  ]
];
const VP40_AC_CAT2_CODES: [[u32; 32]; 16] = [
  [
    0x0009, 0x0015, 0x0028, 0x0052, 0x029A, 0x0537, 0x0536, 0x000A,
    0x0054, 0x0004, 0x0003, 0x000C, 0x000B, 0x000D, 0x0003, 0x0014,
    0x003A, 0x0004, 0x0038, 0x0055, 0x00A7, 0x0299, 0x0298, 0x0000,
    0x001E, 0x0008, 0x002B, 0x000B, 0x000B, 0x003B, 0x001F, 0x0039
  ], [
    0x001D, 0x002F, 0x0002, 0x0007, 0x0019, 0x0035, 0x0034, 0x0009,
    0x002E, 0x0006, 0x0005, 0x0009, 0x0008, 0x0007, 0x001F, 0x0008,
    0x0018, 0x0019, 0x0001, 0x0000, 0x0018, 0x0037, 0x0036, 0x0001,
    0x0001, 0x000A, 0x0039, 0x0016, 0x000D, 0x0001, 0x001E, 0x0038
  ], [
    0x0001, 0x0071, 0x00E0, 0x01C3, 0x0708, 0x1C26, 0x384F, 0x0001,
    0x0031, 0x0006, 0x0005, 0x0009, 0x0008, 0x0005, 0x000F, 0x0039,
    0x0077, 0x0076, 0x0030, 0x0385, 0x384E, 0x1C25, 0x1C24, 0x0001,
    0x0004, 0x000D, 0x0000, 0x0019, 0x001F, 0x000E, 0x001E, 0x003A
  ], [
    0x0006, 0x000C, 0x00D6, 0x007B, 0x01E8, 0x07A4, 0x0F4B, 0x0036,
    0x006A, 0x0007, 0x0005, 0x0008, 0x0009, 0x0001, 0x0007, 0x000D,
    0x003C, 0x00D7, 0x00F5, 0x07A7, 0x0F4A, 0x0F4D, 0x0F4C, 0x0002,
    0x0002, 0x000E, 0x0037, 0x0034, 0x0000, 0x0019, 0x0018, 0x001F
  ], [
    0x000A, 0x0027, 0x00BF, 0x00BE, 0x0224, 0x225D, 0x225C, 0x0026,
    0x005E, 0x0007, 0x0006, 0x0006, 0x0007, 0x0016, 0x002E, 0x0045,
    0x0088, 0x0113, 0x044A, 0x225F, 0x225E, 0x112D, 0x112C, 0x0002,
    0x0002, 0x0012, 0x0003, 0x0002, 0x0003, 0x0000, 0x0010, 0x0023
  ], [
    0x000F, 0x0006, 0x0075, 0x0074, 0x000A, 0x00BF, 0x00B9, 0x0022,
    0x0003, 0x0005, 0x0006, 0x0001, 0x0002, 0x0007, 0x0000, 0x0004,
    0x0016, 0x005E, 0x00B8, 0x00BB, 0x00BA, 0x017D, 0x017C, 0x0002,
    0x0006, 0x001C, 0x0010, 0x003B, 0x0009, 0x0007, 0x0001, 0x0023
  ], [
    0x0001, 0x001C, 0x0036, 0x003B, 0x00EA, 0x075B, 0x1D65, 0x0019,
    0x0074, 0x0004, 0x0005, 0x0000, 0x0001, 0x0037, 0x01D7, 0x075A,
    0x1D64, 0x1D67, 0x1D66, 0x1D61, 0x1D60, 0x1D63, 0x1D62, 0x0002,
    0x001F, 0x001A, 0x000D, 0x003D, 0x000C, 0x0007, 0x003C, 0x0018
  ], [
    0x0002, 0x0001, 0x0014, 0x0000, 0x002F, 0x00BB, 0x02E4, 0x007D,
    0x00BA, 0x0003, 0x0004, 0x0016, 0x001A, 0x00B8, 0x172E, 0x2E5F,
    0x2E5E, 0x1729, 0x1728, 0x172B, 0x172A, 0x172D, 0x172C, 0x0001,
    0x001E, 0x0015, 0x001B, 0x003F, 0x000C, 0x000E, 0x007C, 0x0173
  ], [
    0x0003, 0x007B, 0x0058, 0x01EA, 0x1EB1, 0x1EB0, 0x1EB3, 0x0013,
    0x0012, 0x0005, 0x0006, 0x0002, 0x0001, 0x0013, 0x003C, 0x002D,
    0x00F4, 0x0059, 0x03D7, 0x0F5B, 0x1EB2, 0x1EB5, 0x1EB4, 0x0003,
    0x000E, 0x001F, 0x0012, 0x0008, 0x0008, 0x0000, 0x000A, 0x0017
  ], [
    0x0008, 0x003C, 0x00F5, 0x00F4, 0x1EF7, 0x3DE9, 0x3DE8, 0x001C,
    0x000D, 0x0005, 0x0006, 0x0001, 0x0000, 0x0007, 0x000C, 0x00F6,
    0x01EE, 0x03DF, 0x07BC, 0x3DEB, 0x3DEA, 0x3DED, 0x3DEC, 0x0002,
    0x0009, 0x001F, 0x000F, 0x0005, 0x000E, 0x0006, 0x0004, 0x001D
  ], [
    0x0009, 0x0039, 0x0019, 0x0018, 0x0706, 0x383D, 0x383C, 0x000D,
    0x000F, 0x0005, 0x0006, 0x0000, 0x001D, 0x0003, 0x0071, 0x00E1,
    0x01C0, 0x0382, 0x1C1D, 0x383F, 0x383E, 0x3839, 0x3838, 0x0002,
    0x0008, 0x0002, 0x000D, 0x000C, 0x000F, 0x0007, 0x0002, 0x000E
  ], [
    0x0000, 0x0006, 0x0035, 0x0034, 0x0777, 0x1DD4, 0x3BAB, 0x000E,
    0x000F, 0x0005, 0x0004, 0x001C, 0x0019, 0x003A, 0x00EF, 0x01DC,
    0x0776, 0x0774, 0x3BAA, 0x3BAD, 0x3BAC, 0x3BAF, 0x3BAE, 0x0002,
    0x0007, 0x0002, 0x0018, 0x000C, 0x000F, 0x000D, 0x001B, 0x0076
  ], [
    0x0002, 0x0011, 0x0006, 0x004F, 0x0130, 0x1319, 0x1318, 0x004E,
    0x0007, 0x0006, 0x0005, 0x0010, 0x000D, 0x0005, 0x0099, 0x0262,
    0x098E, 0x131B, 0x131A, 0x263D, 0x263C, 0x263F, 0x263E, 0x0001,
    0x0007, 0x0000, 0x0012, 0x000C, 0x000E, 0x000F, 0x0004, 0x004D
  ], [
    0x0003, 0x0000, 0x0002, 0x0037, 0x01B7, 0x0DB5, 0x36DD, 0x006C,
    0x0016, 0x0005, 0x0004, 0x0003, 0x000A, 0x002E, 0x036C, 0x0DB4,
    0x36DC, 0x36DF, 0x36DE, 0x36D9, 0x36D8, 0x36DB, 0x36DA, 0x000E,
    0x0004, 0x001A, 0x0019, 0x0018, 0x000F, 0x0001, 0x002F, 0x00DA
  ], [
    0x0006, 0x0006, 0x000F, 0x0000, 0x0075, 0x03B8, 0x1DCA, 0x0074,
    0x0076, 0x0004, 0x0005, 0x0003, 0x0002, 0x01DE, 0x0EE6, 0x3B97,
    0x3B96, 0x3B9D, 0x3B9C, 0x3B9F, 0x3B9E, 0x1DC9, 0x1DC8, 0x0005,
    0x001C, 0x0009, 0x000E, 0x0008, 0x000F, 0x0001, 0x01DF, 0x01DD
  ], [
    0x0004, 0x000B, 0x001D, 0x000C, 0x0014, 0x00E0, 0x3875, 0x0386,
    0x01C2, 0x0000, 0x0001, 0x0071, 0x0072, 0x1C3F, 0x3874, 0x3877,
    0x3876, 0x3871, 0x3870, 0x3873, 0x3872, 0x3879, 0x3878, 0x003C,
    0x0073, 0x002A, 0x003D, 0x002B, 0x001F, 0x000D, 0x1C3E, 0x1C3D
  ]
];
const VP40_AC_CAT2_BITS: [[u8; 32]; 16] = [
  [
     5,  7,  8,  9, 12, 13, 13,  5,  7,  3,  3,  4,  4,  4,  4,  5,
     6,  5,  6,  7, 10, 12, 12,  3,  5,  5,  6,  6,  5,  6,  5,  6
  ], [
     5,  7,  8,  9, 11, 12, 12,  5,  7,  3,  3,  4,  4,  4,  5,  5,
     6,  6,  6,  7, 11, 12, 12,  3,  4,  5,  6,  6,  5,  5,  5,  6
  ], [
     4,  7,  8,  9, 11, 13, 14,  5,  7,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  7, 10, 14, 13, 13,  3,  4,  5,  5,  6,  5,  5,  5,  6
  ], [
     4,  6,  8,  8, 10, 12, 13,  6,  7,  3,  3,  4,  4,  4,  5,  6,
     7,  8,  9, 12, 13, 13, 13,  3,  4,  5,  6,  6,  4,  5,  5,  6
  ], [
     4,  6,  8,  8, 10, 14, 14,  6,  7,  3,  3,  4,  4,  5,  6,  7,
     8,  9, 11, 14, 14, 13, 13,  3,  4,  5,  5,  5,  4,  4,  5,  6
  ], [
     4,  5,  7,  7,  9, 13, 13,  6,  7,  3,  3,  4,  4,  5,  6,  8,
    10, 12, 13, 13, 13, 14, 14,  3,  4,  5,  5,  6,  4,  4,  5,  6
  ], [
     3,  5,  6,  6,  8, 11, 13,  6,  7,  3,  3,  4,  4,  6,  9, 11,
    13, 13, 13, 13, 13, 13, 13,  3,  5,  5,  5,  6,  4,  4,  6,  6
  ], [
     3,  4,  5,  4,  6,  8, 10,  7,  8,  3,  3,  5,  5,  8, 13, 14,
    14, 13, 13, 13, 13, 13, 13,  3,  5,  5,  5,  6,  4,  4,  7,  9
  ], [
     4,  7,  8,  9, 13, 13, 13,  6,  6,  3,  3,  4,  4,  5,  6,  7,
     8,  8, 10, 12, 13, 13, 13,  3,  4,  5,  5,  5,  4,  4,  5,  6
  ], [
     4,  6,  8,  8, 13, 14, 14,  6,  6,  3,  3,  4,  4,  5,  6,  8,
     9, 10, 11, 14, 14, 14, 14,  3,  4,  5,  5,  5,  4,  4,  5,  6
  ], [
     4,  6,  7,  7, 11, 14, 14,  6,  6,  3,  3,  4,  5,  5,  7,  8,
     9, 10, 13, 14, 14, 14, 14,  3,  4,  4,  5,  5,  4,  4,  5,  6
  ], [
     3,  5,  7,  7, 11, 13, 14,  6,  6,  3,  3,  5,  5,  6,  8,  9,
    11, 11, 14, 14, 14, 14, 14,  3,  4,  4,  5,  5,  4,  4,  6,  7
  ], [
     3,  5,  6,  7,  9, 13, 13,  7,  6,  3,  3,  5,  5,  6,  8, 10,
    12, 13, 13, 14, 14, 14, 14,  3,  4,  4,  5,  5,  4,  4,  6,  7
  ], [
     3,  4,  5,  6,  9, 12, 14,  7,  6,  3,  3,  5,  5,  7, 10, 12,
    14, 14, 14, 14, 14, 14, 14,  4,  4,  5,  5,  5,  4,  3,  7,  8
  ], [
     3,  4,  5,  4,  7, 10, 13,  7,  7,  3,  3,  5,  5,  9, 12, 14,
    14, 14, 14, 14, 14, 13, 13,  4,  5,  5,  5,  5,  4,  3,  9,  9
  ], [
     3,  4,  5,  4,  5,  8, 14, 10,  9,  2,  2,  7,  7, 13, 14, 14,
    14, 14, 14, 14, 14, 14, 14,  6,  7,  6,  6,  6,  5,  4, 13, 13
  ]
];
const VP40_AC_CAT3_CODES: [[u32; 32]; 16] = [
  [
    0x0007, 0x000F, 0x00BB, 0x00BA, 0x05CF, 0x173A, 0x2E77, 0x0029,
    0x0172, 0x0007, 0x0006, 0x0009, 0x0008, 0x0001, 0x0005, 0x000D,
    0x001D, 0x001C, 0x00B8, 0x02E6, 0x2E76, 0x1739, 0x1738, 0x0002,
    0x0006, 0x0016, 0x0004, 0x0028, 0x0015, 0x000C, 0x0000, 0x002F
  ], [
    0x000B, 0x0002, 0x0054, 0x002F, 0x02AC, 0x156B, 0x1568, 0x0016,
    0x0154, 0x0007, 0x0006, 0x0004, 0x0003, 0x0013, 0x0028, 0x002E,
    0x0157, 0x0155, 0x055B, 0x2AD3, 0x2AD2, 0x2AD5, 0x2AD4, 0x0003,
    0x0008, 0x0000, 0x000A, 0x0003, 0x0002, 0x002B, 0x0012, 0x0029
  ], [
    0x000F, 0x0007, 0x0001, 0x0000, 0x01C4, 0x0703, 0x0E02, 0x0011,
    0x00E1, 0x0005, 0x0006, 0x0002, 0x0001, 0x0009, 0x0010, 0x00E3,
    0x01C5, 0x01C1, 0x0702, 0x1C07, 0x1C06, 0x0E01, 0x0E00, 0x0004,
    0x0007, 0x001D, 0x000D, 0x0001, 0x0005, 0x0006, 0x000C, 0x0039
  ], [
    0x0001, 0x001C, 0x0011, 0x0013, 0x0042, 0x0207, 0x0815, 0x0075,
    0x0041, 0x0005, 0x0006, 0x0000, 0x001F, 0x003B, 0x0074, 0x0043,
    0x0080, 0x0206, 0x0814, 0x0817, 0x0816, 0x0409, 0x0408, 0x0003,
    0x0009, 0x001E, 0x0011, 0x0003, 0x0005, 0x0010, 0x0002, 0x0012
  ], [
    0x0001, 0x001F, 0x0027, 0x0001, 0x004B, 0x0123, 0x0915, 0x0000,
    0x0049, 0x0005, 0x0006, 0x001D, 0x001C, 0x0013, 0x004A, 0x0090,
    0x0914, 0x0917, 0x0916, 0x0911, 0x0910, 0x0913, 0x0912, 0x0003,
    0x0005, 0x0001, 0x0012, 0x0008, 0x0008, 0x001E, 0x0026, 0x0001
  ], [
    0x0003, 0x0001, 0x003F, 0x000B, 0x004E, 0x0132, 0x099A, 0x004F,
    0x0098, 0x0006, 0x0005, 0x001D, 0x001C, 0x007C, 0x0267, 0x1331,
    0x1330, 0x1333, 0x1332, 0x266D, 0x266C, 0x266F, 0x266E, 0x0001,
    0x0004, 0x001E, 0x0012, 0x000A, 0x0008, 0x0000, 0x007D, 0x004D
  ], [
    0x0002, 0x0007, 0x0015, 0x0003, 0x0004, 0x00A7, 0x0536, 0x0028,
    0x029A, 0x0006, 0x0004, 0x001C, 0x0017, 0x00A4, 0x29BE, 0x537F,
    0x537E, 0x29B9, 0x29B8, 0x29BB, 0x29BA, 0x29BD, 0x29BC, 0x000F,
    0x0000, 0x0005, 0x0016, 0x001D, 0x0006, 0x0001, 0x00A5, 0x014C
  ], [
    0x0004, 0x0007, 0x001A, 0x000C, 0x0006, 0x0029, 0x01BD, 0x1BE3,
    0x1BE0, 0x0000, 0x0007, 0x006E, 0x01BC, 0x37C3, 0x37C2, 0x37CD,
    0x37CC, 0x37CF, 0x37CE, 0x37C9, 0x37C8, 0x37CB, 0x37CA, 0x0015,
    0x01BF, 0x037D, 0x0036, 0x0002, 0x000B, 0x0028, 0x37C5, 0x37C4
  ], [
    0x0001, 0x0009, 0x0003, 0x0002, 0x011F, 0x08E9, 0x08E8, 0x002D,
    0x0022, 0x0006, 0x0007, 0x0010, 0x0011, 0x0017, 0x002C, 0x0046,
    0x011E, 0x011C, 0x0477, 0x08EB, 0x08EA, 0x08ED, 0x08EC, 0x0003,
    0x000B, 0x0001, 0x0014, 0x000A, 0x0009, 0x0015, 0x0000, 0x0010
  ], [
    0x0001, 0x001D, 0x0022, 0x0013, 0x011E, 0x08FC, 0x23F5, 0x0023,
    0x0022, 0x0005, 0x0006, 0x0010, 0x000B, 0x0010, 0x008E, 0x023E,
    0x08FF, 0x11FD, 0x23F4, 0x23F7, 0x23F6, 0x23F9, 0x23F8, 0x0003,
    0x0009, 0x0000, 0x001C, 0x000A, 0x000F, 0x0001, 0x0012, 0x0046
  ], [
    0x0003, 0x001F, 0x003C, 0x003D, 0x0086, 0x0877, 0x10E8, 0x0041,
    0x0040, 0x0005, 0x0006, 0x0007, 0x0006, 0x0004, 0x010F, 0x021C,
    0x0875, 0x21D3, 0x21D2, 0x21D9, 0x21D8, 0x21DB, 0x21DA, 0x0002,
    0x0009, 0x0000, 0x0011, 0x0003, 0x000E, 0x0002, 0x0005, 0x0042
  ], [
    0x0004, 0x0001, 0x003D, 0x0009, 0x00F3, 0x0793, 0x1E45, 0x0000,
    0x0002, 0x0005, 0x0006, 0x0008, 0x0001, 0x0003, 0x01E5, 0x0792,
    0x1E44, 0x1E47, 0x1E46, 0x1E41, 0x1E40, 0x1E43, 0x1E42, 0x0001,
    0x0006, 0x001F, 0x000F, 0x000E, 0x000E, 0x0005, 0x0078, 0x0001
  ], [
    0x0004, 0x0005, 0x000E, 0x0017, 0x003E, 0x00F0, 0x0F1E, 0x007A,
    0x007F, 0x0006, 0x0007, 0x0005, 0x0004, 0x007B, 0x01E2, 0x1E3F,
    0x1E3E, 0x0F19, 0x0F18, 0x0F1B, 0x0F1A, 0x0F1D, 0x0F1C, 0x0000,
    0x0003, 0x0016, 0x0009, 0x0008, 0x000A, 0x0006, 0x007E, 0x0079
  ], [
    0x0005, 0x000C, 0x001A, 0x0004, 0x001A, 0x00DE, 0x0DF4, 0x00DD,
    0x006D, 0x0000, 0x0007, 0x0025, 0x0024, 0x00DC, 0x0DF7, 0x1BEB,
    0x1BEA, 0x0DF1, 0x0DF0, 0x0DF3, 0x0DF2, 0x1BED, 0x1BEC, 0x0008,
    0x0013, 0x000C, 0x0037, 0x0036, 0x0005, 0x0007, 0x006C, 0x01BF
  ], [
    0x0005, 0x000D, 0x001F, 0x000C, 0x003B, 0x0040, 0x041A, 0x0104,
    0x0107, 0x0001, 0x0000, 0x0024, 0x0021, 0x020B, 0x106E, 0x20DF,
    0x20DE, 0x1055, 0x1054, 0x1057, 0x1056, 0x106D, 0x106C, 0x0011,
    0x003A, 0x0025, 0x0038, 0x0039, 0x0013, 0x001E, 0x020C, 0x0414
  ], [
    0x0000, 0x0007, 0x000D, 0x0005, 0x0009, 0x0022, 0x0CD1, 0x0CD0,
    0x0CD3, 0x0003, 0x0002, 0x008D, 0x00CC, 0x066B, 0x0CD2, 0x19B5,
    0x19B4, 0x19B7, 0x19B6, 0x19B1, 0x19B0, 0x19B3, 0x19B2, 0x0047,
    0x008C, 0x0337, 0x0067, 0x0018, 0x0010, 0x0032, 0x0CD5, 0x0CD4
  ]
];
const VP40_AC_CAT3_BITS: [[u8; 32]; 16] = [
  [
     4,  6,  8,  8, 11, 13, 14,  6,  9,  3,  3,  4,  4,  4,  5,  6,
     7,  7,  8, 10, 14, 13, 13,  3,  4,  5,  5,  6,  5,  6,  4,  6
  ], [
     4,  5,  7,  7, 10, 13, 13,  6,  9,  3,  3,  4,  4,  5,  6,  7,
     9,  9, 11, 14, 14, 14, 14,  3,  4,  4,  5,  5,  4,  6,  5,  6
  ], [
     4,  5,  6,  6,  9, 11, 12,  6,  8,  3,  3,  4,  4,  5,  6,  8,
     9,  9, 11, 13, 13, 12, 12,  3,  4,  5,  5,  5,  4,  5,  5,  6
  ], [
     3,  5,  6,  6,  8, 11, 13,  7,  8,  3,  3,  4,  5,  6,  7,  8,
     9, 11, 13, 13, 13, 12, 12,  3,  4,  5,  5,  5,  4,  5,  5,  6
  ], [
     3,  5,  6,  5,  8, 10, 13,  6,  8,  3,  3,  5,  5,  6,  8,  9,
    13, 13, 13, 13, 13, 13, 13,  3,  4,  4,  5,  5,  4,  5,  6,  6
  ], [
     3,  4,  6,  5,  7,  9, 12,  7,  8,  3,  3,  5,  5,  7, 10, 13,
    13, 13, 13, 14, 14, 14, 14,  3,  4,  5,  5,  5,  4,  4,  7,  7
  ], [
     3,  4,  5,  4,  5,  8, 11,  6, 10,  3,  3,  5,  5,  8, 14, 15,
    15, 14, 14, 14, 14, 14, 14,  4,  4,  5,  5,  5,  4,  4,  8,  9
  ], [
     3,  4,  5,  4,  4,  6,  9, 13, 13,  2,  3,  7,  9, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14,  5,  9, 10,  6,  3,  4,  6, 14, 14
  ], [
     3,  5,  6,  6, 10, 13, 13,  7,  7,  3,  3,  5,  5,  6,  7,  8,
    10, 10, 12, 13, 13, 13, 13,  3,  4,  4,  5,  5,  4,  5,  5,  6
  ], [
     3,  5,  6,  6,  9, 12, 14,  7,  7,  3,  3,  5,  5,  6,  8, 10,
    12, 13, 14, 14, 14, 14, 14,  3,  4,  4,  5,  5,  4,  4,  6,  7
  ], [
     3,  5,  6,  6,  8, 12, 13,  7,  7,  3,  3,  5,  5,  6,  9, 10,
    12, 14, 14, 14, 14, 14, 14,  3,  4,  4,  5,  5,  4,  4,  6,  7
  ], [
     3,  4,  6,  5,  8, 11, 13,  7,  7,  3,  3,  5,  5,  7,  9, 11,
    13, 13, 13, 13, 13, 13, 13,  3,  4,  5,  5,  5,  4,  4,  7,  7
  ], [
     3,  4,  5,  5,  7,  9, 13,  8,  8,  3,  3,  5,  5,  8, 10, 14,
    14, 13, 13, 13, 13, 13, 13,  3,  4,  5,  5,  5,  4,  4,  8,  8
  ], [
     3,  4,  5,  4,  6,  9, 13,  9,  8,  2,  3,  6,  6,  9, 13, 14,
    14, 13, 13, 13, 13, 14, 14,  4,  5,  5,  6,  6,  4,  4,  8, 10
  ], [
     3,  4,  5,  4,  6,  7, 11,  9,  9,  2,  2,  6,  6, 10, 13, 14,
    14, 13, 13, 13, 13, 13, 13,  5,  6,  6,  6,  6,  5,  5, 10, 11
  ], [
     2,  4,  5,  4,  5,  7, 13, 13, 13,  2,  2,  9,  9, 12, 13, 14,
    14, 14, 14, 14, 14, 14, 14,  8,  9, 11,  8,  6,  6,  7, 13, 13
  ]
];

const VP40_QMAT: &[i16; 64] = &[
    16, 17, 18, 20, 22, 24, 26, 28,
    17, 18, 20, 22, 24, 26, 28, 32,
    18, 20, 22, 24, 26, 28, 32, 36,
    20, 22, 24, 26, 28, 32, 36, 40,
    22, 24, 26, 28, 32, 36, 40, 44,
    24, 26, 28, 32, 36, 40, 44, 48,
    26, 28, 32, 36, 40, 44, 48, 52,
    28, 32, 36, 40, 44, 48, 52, 56
];
const VP40_DC_Y_SCALES: [i16; 64] = [
    180, 180, 180, 180, 180, 180, 175, 170,
    165, 160, 157, 155, 152, 150, 147, 145,
    142, 140, 137, 135, 132, 130, 127, 125,
    122, 120, 117, 115, 112, 110, 107, 105,
    102, 100,  97,  95,  92,  90,  87,  85,
     82,  80,  77,  75,  72,  70,  67,  65,
     62,  60,  57,  55,  52,  50,  47,  45,
     42,  40,  37,  35,  32,  30,  27,  25
];
const VP40_DC_C_SCALES: [i16; 64] = [
    150, 150, 150, 150, 150, 150, 150, 150,
    150, 150, 150, 150, 150, 150, 147, 145,
    142, 140, 137, 135, 132, 130, 127, 125,
    122, 120, 117, 115, 112, 110, 107, 105,
    102, 100,  97,  95,  92,  90,  87,  85,
     82,  80,  77,  75,  72,  70,  67,  65,
     62,  60,  57,  55,  52,  50,  47,  45,
     42,  40,  37,  35,  32,  30,  27,  25
];
const VP40_AC_SCALES: [i16; 64] = [
    500, 475, 450, 430, 410, 390, 370, 350,
    330, 315, 300, 285, 270, 260, 250, 240,
    230, 220, 210, 200, 190, 185, 180, 170,
    160, 150, 143, 135, 128, 120, 113, 106,
    100,  94,  90,  85,  80,  75,  70,  66,
     62,  57,  52,  49,  45,  41,  38,  35,
     33,  30,  27,  24,  22,  20,  18,  16,
     14,  12,  10,   9,   7,   6,   4,   1
];

const VP40_MBPAT_CODES: [[u8; 14]; 2] = [
    [  0b000, 0b1111, 0b1001,  0b010, 0b1101, 0b01110, 0b1011,  0b001, 0b01111, 0b1000, 0b0110, 0b1110, 0b1100, 0b1010 ],
    [ 0b0111, 0b1010, 0b1001, 0b1100, 0b1000, 0b01101,  0b000, 0b1110, 0b01100, 0b1101,  0b001, 0b1011, 0b1111,  0b010 ]
];
const VP40_MBPAT_BITS: [[u8; 14]; 2] = [
    [ 3, 4, 4, 3, 4, 5, 4, 3, 5, 4, 4, 4, 4, 4 ],
    [ 4, 4, 4, 4, 4, 5, 3, 4, 5, 4, 3, 4, 4, 3 ]
];
const VP40_BP_PREDICTOR: [u8; 15] = [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1 ];
const VP40_MV_X_CODES: [[u32; 63]; 7] = [
  [
    0x06A, 0x11A, 0x18E, 0x237, 0x04A, 0x236, 0x07A, 0x0D6,
    0x07E, 0x1FD, 0x08C, 0x0D7, 0x087, 0x183, 0x03C, 0x061,
    0x047, 0x069, 0x040, 0x048, 0x049, 0x086, 0x013, 0x0D2,
    0x01C, 0x042, 0x025, 0x01B, 0x013, 0x005, 0x000, 0x007,
    0x005, 0x01B, 0x019, 0x019, 0x008, 0x045, 0x01D, 0x0C6,
    0x068, 0x090, 0x041, 0x04B, 0x031, 0x18F, 0x062, 0x03E,
    0x044, 0x068, 0x030, 0x182, 0x0C0, 0x1A7, 0x091, 0x092,
    0x07B, 0x0FF, 0x1A6, 0x1FC, 0x06A, 0x093, 0x06B
  ], [
    0x039, 0x259, 0x01B, 0x1D1, 0x137, 0x1D0, 0x01A, 0x1B5,
    0x01D, 0x4BC, 0x06C, 0x038, 0x071, 0x02D, 0x07D, 0x075,
    0x019, 0x0E9, 0x037, 0x015, 0x01E, 0x0DB, 0x04C, 0x070,
    0x00D, 0x00C, 0x027, 0x004, 0x002, 0x000, 0x005, 0x007,
    0x006, 0x002, 0x008, 0x024, 0x00C, 0x03B, 0x01E, 0x09A,
    0x00E, 0x069, 0x04A, 0x12D, 0x035, 0x0F9, 0x018, 0x07F,
    0x00F, 0x0F8, 0x07E, 0x25F, 0x068, 0x02C, 0x014, 0x258,
    0x136, 0x4BD, 0x12E, 0x1B4, 0x017, 0x039, 0x01F
  ], [
    0x029, 0x3CB, 0x1F5, 0x263, 0x1F4, 0x3DA, 0x050, 0x260,
    0x1EC, 0x3D3, 0x109, 0x3D2, 0x051, 0x792, 0x0F3, 0x09A,
    0x0F7, 0x132, 0x0C1, 0x1E8, 0x02A, 0x085, 0x061, 0x1F7,
    0x078, 0x0C7, 0x023, 0x07C, 0x012, 0x00B, 0x00E, 0x00D,
    0x000, 0x005, 0x003, 0x004, 0x019, 0x020, 0x03F, 0x043,
    0x062, 0x09F, 0x04E, 0x181, 0x02B, 0x137, 0x0F5, 0x089,
    0x0C6, 0x262, 0x088, 0x3C8, 0x1F6, 0x3CA, 0x09E, 0x261,
    0x136, 0x108, 0x133, 0x793, 0x180, 0x3DB, 0x045
  ], [
    0x001, 0x1C7, 0x067, 0x0B5, 0x066, 0x139, 0x099, 0x0B4,
    0x0C3, 0x130, 0x000, 0x131, 0x09E, 0x0B7, 0x02C, 0x001,
    0x028, 0x138, 0x04B, 0x031, 0x060, 0x091, 0x003, 0x09D,
    0x017, 0x04D, 0x031, 0x070, 0x007, 0x03A, 0x007, 0x002,
    0x00B, 0x001, 0x00F, 0x008, 0x00D, 0x004, 0x00A, 0x00D,
    0x019, 0x002, 0x03B, 0x04A, 0x015, 0x0C2, 0x018, 0x032,
    0x072, 0x1C6, 0x029, 0x1C5, 0x049, 0x121, 0x01B, 0x030,
    0x01A, 0x1C4, 0x09F, 0x0B6, 0x019, 0x120, 0x073
  ], [
    0x023, 0x1C8, 0x043, 0x110, 0x00C, 0x153, 0x022, 0x111,
    0x00F, 0x042, 0x023, 0x1C9, 0x02A, 0x01B, 0x073, 0x045,
    0x06E, 0x089, 0x06C, 0x01A, 0x06F, 0x0B6, 0x00B, 0x0E5,
    0x025, 0x020, 0x029, 0x04D, 0x002, 0x014, 0x01A, 0x017,
    0x01E, 0x027, 0x018, 0x028, 0x01F, 0x000, 0x006, 0x010,
    0x007, 0x00B, 0x003, 0x004, 0x01D, 0x02C, 0x019, 0x02B,
    0x009, 0x055, 0x038, 0x00E, 0x024, 0x0A8, 0x00A, 0x099,
    0x05A, 0x098, 0x06D, 0x152, 0x02B, 0x0B7, 0x001
  ], [
    0x03D, 0x0B1, 0x0DD, 0x1F6, 0x0C5, 0x188, 0x037, 0x03F,
    0x01E, 0x189, 0x00F, 0x03E, 0x06A, 0x1F7, 0x061, 0x079,
    0x018, 0x0B0, 0x00E, 0x0B3, 0x00C, 0x0DF, 0x006, 0x0DC,
    0x019, 0x0DE, 0x027, 0x00E, 0x01A, 0x063, 0x00F, 0x00E,
    0x014, 0x07C, 0x036, 0x06B, 0x03F, 0x060, 0x008, 0x074,
    0x009, 0x078, 0x012, 0x00D, 0x015, 0x02D, 0x002, 0x01C,
    0x005, 0x03B, 0x000, 0x034, 0x019, 0x026, 0x010, 0x075,
    0x002, 0x036, 0x023, 0x0B2, 0x022, 0x0FA, 0x017
  ], [
    0x015, 0x0DD, 0x03E, 0x16E, 0x04C, 0x012, 0x05D, 0x0B6,
    0x06F, 0x1F1, 0x069, 0x1F0, 0x01D, 0x16F, 0x002, 0x06B,
    0x00C, 0x0DC, 0x068, 0x09B, 0x07D, 0x09A, 0x00D, 0x013,
    0x008, 0x0F9, 0x02C, 0x012, 0x033, 0x04F, 0x00D, 0x005,
    0x012, 0x03F, 0x032, 0x013, 0x03B, 0x005, 0x02F, 0x05A,
    0x03F, 0x01C, 0x03A, 0x008, 0x036, 0x05C, 0x010, 0x000,
    0x00C, 0x04E, 0x003, 0x06A, 0x00E, 0x003, 0x014, 0x01E,
    0x01C, 0x00F, 0x018, 0x023, 0x01E, 0x022, 0x002
  ]
];
const VP40_MV_X_BITS: [[u8; 63]; 7] = [
  [
     7,  9,  9, 10,  8, 10,  8,  9,  8, 10,  8,  9,  8,  9,  7,  7,
     7,  8,  7,  8,  7,  8,  6,  8,  6,  7,  6,  6,  5,  4,  2,  3,
     3,  5,  5,  6,  5,  7,  6,  8,  7,  8,  7,  8,  7,  9,  7,  7,
     7,  8,  7,  9,  8,  9,  8,  9,  8,  9,  9, 10,  8,  9,  7
  ], [
     7, 10,  8, 10,  9, 10,  8, 10,  8, 11,  8,  9,  8,  9,  8,  8,
     7,  9,  7,  8,  7,  9,  7,  8,  6,  7,  6,  6,  4,  4,  3,  3,
     3,  3,  4,  6,  5,  7,  6,  8,  6,  8,  7,  9,  7,  9,  7,  8,
     7,  9,  8, 10,  8,  9,  8, 10,  9, 11,  9, 10,  8,  9,  7
  ], [
     7, 10,  9, 10,  9, 10,  8, 10,  9, 10,  9, 10,  8, 11,  8,  8,
     8,  9,  8,  9,  7,  8,  7,  9,  7,  8,  6,  7,  5,  5,  4,  4,
     2,  3,  3,  4,  5,  6,  6,  7,  7,  8,  7,  9,  7,  9,  8,  8,
     8, 10,  8, 10,  9, 10,  8, 10,  9,  9,  9, 11,  9, 10,  7
  ], [
     6,  9,  8,  9,  8,  9,  8,  9,  8,  9,  7,  9,  8,  9,  7,  7,
     7,  9,  7,  8,  7,  8,  6,  8,  6,  7,  6,  7,  5,  6,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  5,  5,  6,  6,  7,  6,  8,  6,  7,
     7,  9,  7,  9,  7,  9,  7,  8,  7,  9,  8,  9,  7,  9,  7
  ], [
     6,  9,  8,  9,  7,  9,  7,  9,  7,  8,  7,  9,  7,  8,  7,  7,
     7,  8,  7,  8,  7,  8,  6,  8,  6,  7,  6,  7,  5,  6,  5,  5,
     5,  6,  5,  6,  5,  5,  4,  5,  4,  5,  4,  5,  5,  6,  5,  6,
     5,  7,  6,  7,  6,  8,  6,  8,  7,  8,  7,  9,  7,  8,  5
  ], [
     6,  8,  8,  9,  8,  9,  7,  8,  7,  9,  7,  8,  7,  9,  7,  7,
     6,  8,  6,  8,  6,  8,  6,  8,  6,  8,  6,  7,  6,  7,  5,  5,
     5,  7,  6,  7,  6,  7,  5,  7,  5,  7,  5,  6,  5,  6,  4,  5,
     4,  6,  4,  6,  5,  6,  5,  7,  5,  7,  6,  8,  6,  8,  5
  ], [
     5,  8,  7,  9,  7,  8,  7,  8,  7,  9,  7,  9,  7,  9,  6,  7,
     6,  8,  7,  8,  7,  8,  6,  8,  6,  8,  6,  7,  6,  7,  5,  5,
     5,  7,  6,  7,  6,  6,  6,  7,  6,  7,  6,  7,  6,  7,  5,  5,
     5,  7,  5,  7,  5,  6,  5,  6,  5,  6,  5,  6,  5,  6,  3
  ]
];
const VP40_MV_Y_CODES: [[u32; 63]; 7] = [
  [
    0x052, 0x14C, 0x1FA, 0x124, 0x082, 0x29E, 0x08E, 0x24B,
    0x09C, 0x3F7, 0x086, 0x114, 0x083, 0x3A5, 0x0FA, 0x04F,
    0x0FB, 0x13B, 0x0FC, 0x172, 0x044, 0x173, 0x051, 0x087,
    0x05F, 0x0BA, 0x026, 0x05E, 0x016, 0x015, 0x006, 0x001,
    0x000, 0x01C, 0x01E, 0x075, 0x03B, 0x0FF, 0x025, 0x0BB,
    0x07C, 0x08B, 0x048, 0x171, 0x042, 0x14E, 0x046, 0x0FE,
    0x040, 0x13A, 0x093, 0x115, 0x08F, 0x3F6, 0x170, 0x29F,
    0x1D1, 0x24A, 0x1D3, 0x3A4, 0x1D0, 0x14D, 0x050
  ], [
    0x0DE, 0x223, 0x136, 0x7C5, 0x12F, 0x4A1, 0x3D7, 0x7AC,
    0x133, 0x7C4, 0x1B8, 0x222, 0x096, 0x251, 0x095, 0x1F0,
    0x0DA, 0x110, 0x09A, 0x360, 0x0DD, 0x12E, 0x048, 0x092,
    0x078, 0x098, 0x027, 0x045, 0x01A, 0x010, 0x005, 0x000,
    0x001, 0x00E, 0x00C, 0x023, 0x03F, 0x0F4, 0x07D, 0x089,
    0x07B, 0x1BE, 0x0F9, 0x3E3, 0x0F3, 0x127, 0x0DB, 0x1EA,
    0x0D9, 0x6E7, 0x1BF, 0x4A0, 0x1B1, 0x6E6, 0x137, 0x7AD,
    0x126, 0x6C2, 0x132, 0x6C3, 0x129, 0x372, 0x0F2
  ], [
    0x016, 0x09C, 0x13C, 0x09E, 0x12B, 0x0BA, 0x181, 0x317,
    0x084, 0x04E, 0x026, 0x316, 0x180, 0x05C, 0x0C1, 0x02F,
    0x010, 0x045, 0x012, 0x189, 0x024, 0x13D, 0x066, 0x023,
    0x067, 0x0C6, 0x024, 0x04B, 0x011, 0x032, 0x00D, 0x000,
    0x007, 0x005, 0x003, 0x003, 0x005, 0x020, 0x008, 0x025,
    0x026, 0x04F, 0x061, 0x02B, 0x04E, 0x18A, 0x043, 0x09F,
    0x014, 0x254, 0x094, 0x310, 0x085, 0x311, 0x02A, 0x0BB,
    0x18F, 0x255, 0x09D, 0x09F, 0x18E, 0x044, 0x026
  ], [
    0x061, 0x12A, 0x00D, 0x3BD, 0x089, 0x109, 0x18E, 0x210,
    0x1D3, 0x211, 0x088, 0x019, 0x085, 0x018, 0x0E8, 0x0CE,
    0x040, 0x119, 0x045, 0x1D2, 0x04B, 0x1DD, 0x062, 0x094,
    0x075, 0x00C, 0x027, 0x00D, 0x002, 0x026, 0x006, 0x01E,
    0x00D, 0x01F, 0x001, 0x00A, 0x002, 0x007, 0x00B, 0x000,
    0x01C, 0x076, 0x032, 0x007, 0x024, 0x0C0, 0x007, 0x041,
    0x002, 0x18F, 0x047, 0x1DC, 0x043, 0x12B, 0x0CF, 0x118,
    0x0C6, 0x3BC, 0x08D, 0x3BF, 0x0C1, 0x3BE, 0x066
  ], [
    0x007, 0x14D, 0x0A0, 0x09E, 0x0CF, 0x39C, 0x0A1, 0x39D,
    0x0AB, 0x1C5, 0x026, 0x14C, 0x025, 0x19C, 0x03F, 0x0E1,
    0x066, 0x1CF, 0x03E, 0x1C4, 0x072, 0x04E, 0x006, 0x0AA,
    0x01C, 0x0E6, 0x032, 0x051, 0x03B, 0x005, 0x01F, 0x018,
    0x002, 0x03A, 0x000, 0x036, 0x005, 0x008, 0x008, 0x016,
    0x009, 0x00D, 0x003, 0x02F, 0x01E, 0x02E, 0x01A, 0x02B,
    0x00C, 0x024, 0x01E, 0x0E0, 0x004, 0x0A7, 0x054, 0x1C7,
    0x052, 0x19D, 0x03A, 0x09F, 0x03B, 0x1C6, 0x037
  ], [
    0x02A, 0x039, 0x025, 0x115, 0x024, 0x1FA, 0x02F, 0x114,
    0x075, 0x038, 0x0FC, 0x036, 0x01E, 0x1FB, 0x07F, 0x068,
    0x016, 0x037, 0x01F, 0x05C, 0x013, 0x08B, 0x001, 0x0FB,
    0x021, 0x044, 0x02B, 0x06B, 0x03B, 0x00C, 0x01C, 0x019,
    0x001, 0x020, 0x016, 0x07C, 0x00C, 0x074, 0x00A, 0x01C,
    0x012, 0x069, 0x00F, 0x06A, 0x014, 0x011, 0x01E, 0x017,
    0x002, 0x031, 0x01B, 0x030, 0x00D, 0x000, 0x001, 0x01D,
    0x023, 0x01A, 0x01D, 0x05D, 0x010, 0x0FA, 0x013
  ], [
    0x012, 0x026, 0x041, 0x022, 0x01A, 0x0A9, 0x04C, 0x1B2,
    0x05C, 0x0A8, 0x058, 0x1B3, 0x040, 0x079, 0x00C, 0x055,
    0x01F, 0x0D8, 0x076, 0x023, 0x05F, 0x078, 0x00B, 0x01B,
    0x02D, 0x010, 0x037, 0x06D, 0x032, 0x00A, 0x01A, 0x01E,
    0x01F, 0x02B, 0x00D, 0x077, 0x031, 0x05D, 0x038, 0x027,
    0x00C, 0x0E9, 0x033, 0x05E, 0x030, 0x04D, 0x00A, 0x021,
    0x007, 0x03D, 0x039, 0x0E8, 0x00B, 0x059, 0x014, 0x027,
    0x011, 0x075, 0x00E, 0x009, 0x008, 0x012, 0x000
  ]
];
const VP40_MV_Y_BITS: [[u8; 63]; 7] = [
  [
     7,  9,  9,  9,  8, 10,  8, 10,  8, 10,  8,  9,  8, 10,  8,  7,
     8,  9,  8,  9,  7,  9,  7,  8,  7,  8,  6,  7,  5,  5,  3,  2,
     2,  5,  5,  7,  6,  8,  6,  8,  7,  8,  7,  9,  7,  9,  7,  8,
     7,  9,  8,  9,  8, 10,  9, 10,  9, 10,  9, 10,  9,  9,  7
  ], [
     8, 10,  9, 11,  9, 11, 10, 11,  9, 11,  9, 10,  8, 10,  8,  9,
     8,  9,  8, 10,  8,  9,  7,  8,  7,  8,  6,  7,  5,  5,  3,  2,
     2,  4,  4,  6,  6,  8,  7,  8,  7,  9,  8, 10,  8,  9,  8,  9,
     8, 11,  9, 11,  9, 11,  9, 11,  9, 11,  9, 11,  9, 10,  8
  ], [
     7,  9,  9, 10,  9, 10,  9, 10,  8,  9,  8, 10,  9,  9,  8,  8,
     7,  9,  7,  9,  7,  9,  7,  8,  7,  8,  6,  7,  5,  6,  4,  3,
     3,  3,  3,  4,  4,  6,  5,  7,  6,  8,  7,  8,  7,  9,  7,  8,
     7, 10,  8, 10,  8, 10,  8, 10,  9, 10,  9, 10,  9,  9,  7
  ], [
     7,  9,  8, 10,  8,  9,  9, 10,  9, 10,  8,  9,  8,  9,  8,  8,
     7,  9,  7,  9,  7,  9,  7,  8,  7,  7,  6,  7,  5,  6,  4,  5,
     4,  5,  3,  4,  3,  4,  4,  5,  5,  7,  6,  7,  6,  8,  6,  7,
     6,  9,  7,  9,  7,  9,  8,  9,  8, 10,  8, 10,  8, 10,  7
  ], [
     6,  9,  8,  9,  8, 10,  8, 10,  8,  9,  7,  9,  7,  9,  7,  8,
     7,  9,  7,  9,  7,  8,  6,  8,  6,  8,  6,  7,  6,  6,  5,  5,
     4,  6,  4,  6,  4,  5,  4,  5,  4,  5,  4,  6,  5,  6,  5,  6,
     5,  7,  6,  8,  6,  8,  7,  9,  7,  9,  7,  9,  7,  9,  6
  ], [
     6,  8,  7,  9,  7,  9,  7,  9,  7,  8,  8,  8,  7,  9,  7,  7,
     6,  8,  7,  8,  6,  8,  6,  8,  6,  7,  6,  7,  6,  6,  5,  5,
     4,  6,  5,  7,  5,  7,  5,  6,  5,  7,  5,  7,  5,  6,  5,  5,
     4,  6,  5,  6,  5,  6,  5,  7,  6,  7,  6,  8,  6,  8,  5
  ], [
     5,  7,  7,  8,  7,  8,  7,  9,  7,  8,  7,  9,  7,  8,  6,  7,
     6,  8,  7,  8,  7,  8,  6,  7,  6,  7,  6,  7,  6,  6,  5,  5,
     5,  6,  5,  7,  6,  7,  6,  7,  5,  8,  6,  7,  6,  7,  5,  6,
     5,  7,  6,  8,  5,  7,  5,  6,  5,  7,  5,  6,  5,  6,  3
  ]
];

const VP40_LOOP_STRENGTH: [i16; 64] = [
    30, 25, 20, 20, 15, 15, 14, 14,
    13, 13, 12, 12, 11, 11, 10, 10,
     9,  9,  8,  8,  7,  7,  7,  7,
     6,  6,  6,  6,  5,  5,  5,  5,
     4,  4,  4,  4,  3,  3,  3,  3,
     2,  2,  2,  2,  2,  2,  2,  2,
     2,  2,  2,  2,  2,  2,  2,  2,
     1,  1,  1,  1,  1,  1,  1,  1
];
const VP40_MV_LUT_INDEX: [usize; 32] = [
    0, 1, 2, 2, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6
];
