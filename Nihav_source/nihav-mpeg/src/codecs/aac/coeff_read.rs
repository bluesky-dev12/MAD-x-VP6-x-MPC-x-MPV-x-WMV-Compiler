use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::io::intcode::*;
use std::mem;
use std::ptr;
use super::{INTENSITY_SCALE_MIN, MAX_WINDOWS, MAX_SFBS, GASubbandInfo, ICSInfo};
use super::tables::CBRT_TAB;

pub const ZERO_HCB:         u8 = 0;
pub const FIRST_PAIR_HCB:   u8 = 5;
pub const ESC_HCB:          u8 = 11;
pub const RESERVED_HCB:     u8 = 12;
pub const NOISE_HCB:        u8 = 13;
pub const INTENSITY_HCB2:   u8 = 14;
pub const INTENSITY_HCB:    u8 = 15;

pub struct Codebooks {
    pub scale_cb:       Codebook<i8>,
    pub spec_cb:        [Codebook<u16>; 11],
}

fn scale_map(idx: usize) -> i8 { (idx as i8) - 60 }
fn cb_map(idx: usize) -> u16 { idx as u16 }

impl Codebooks {
    pub fn new() -> Self {
        let mut coderead = TableCodebookDescReader::new(AAC_SCF_CODEBOOK_CODES, AAC_SCF_CODEBOOK_BITS, scale_map);
        let scale_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let spec_cb = unsafe {
                let mut uspec_cb: mem::MaybeUninit::<[Codebook<u16>; 11]> = mem::MaybeUninit::uninit();
                for i in 0..AAC_SPEC_CODES.len() {
                    let mut coderead = TableCodebookDescReader::new(AAC_SPEC_CODES[i], AAC_SPEC_BITS[i], cb_map);
                    ptr::write(&mut (*uspec_cb.as_mut_ptr())[i], Codebook::new(&mut coderead, CodebookMode::MSB).unwrap());
                }
                uspec_cb.assume_init()
            };
        Self { scale_cb, spec_cb }
    }
}

pub fn get_scale(scale: u8) -> f32 {
    2.0f32.powf(0.25 * (f32::from(scale) - 100.0 - 56.0))
}
fn iquant(val: i16) -> f32 {
    let idx = val.unsigned_abs() as usize;
    if idx < CBRT_TAB.len() {
        if val < 0 {
            -CBRT_TAB[idx]
        } else {
             CBRT_TAB[idx]
        }
    } else if val < 0 {
        -((-f32::from(val)).powf(4.0 / 3.0))
    } else {
        f32::from(val).powf(4.0 / 3.0)
    }
}
pub fn requant(val: f32, scale: f32) -> f32 {
    if scale == 0.0 { return 0.0; }
    let bval = val / scale;
    if bval >= 0.0 {
        val.powf(3.0 / 4.0)
    } else {
        -((-val).powf(3.0 / 4.0))
    }
}
fn decode_quads(br: &mut BitReader, cb: &Codebook<u16>, unsigned: bool, scale: f32, dst: &mut [f32]) -> DecoderResult<()> {
    for out in dst.chunks_mut(4) {
        let cw                                          = br.read_cb(cb)? as usize;
        if unsigned {
            for i in 0..4 {
                let val = AAC_QUADS[cw][i];
                if val != 0 {
                    if br.read_bool()? {
                        out[i] = -CBRT_TAB[val as usize] * scale;
                    } else {
                        out[i] =  CBRT_TAB[val as usize] * scale;
                    }
                }
            }
        } else {
            for i in 0..4 {
                let val = AAC_QUADS[cw][i] - 1;
                if val < 0 {
                    out[i] = -CBRT_TAB[val.unsigned_abs() as usize] * scale;
                } else {
                    out[i] =  CBRT_TAB[val.unsigned_abs() as usize] * scale;
                }
            }
        }
    }
    Ok(())
}
fn decode_pairs(br: &mut BitReader, cb: &Codebook<u16>, unsigned: bool, escape: bool, modulo: u16, scale: f32, dst: &mut [f32]) -> DecoderResult<()> {
    for out in dst.chunks_mut(2) {
        let cw                                          = br.read_cb(cb)?;
        let mut x = (cw / modulo) as i16;
        let mut y = (cw % modulo) as i16;
        if unsigned {
            if x != 0 && br.read_bool()? {
                x = -x;
            }
            if y != 0 && br.read_bool()? {
                y = -y;
            }
        } else {
            x -= (modulo >> 1) as i16;
            y -= (modulo >> 1) as i16;
        }
        if escape {
            if (x == 16) || (x == -16) {
                x = read_escape(br, x > 0)?;
            }
            if (y == 16) || (y == -16) {
                y = read_escape(br, y > 0)?;
            }
            out[0] = iquant(x) * scale;
            out[1] = iquant(y) * scale;
        } else {
            out[0] = scale * if x < 0 { -CBRT_TAB[x.unsigned_abs() as usize] } else { CBRT_TAB[x as usize] };
            out[1] = scale * if y < 0 { -CBRT_TAB[y.unsigned_abs() as usize] } else { CBRT_TAB[y as usize] };
        }
    }
    Ok(())
}
fn read_escape(br: &mut BitReader, sign: bool) -> DecoderResult<i16> {
    let prefix                                          = br.read_code(UintCodeType::UnaryOnes)? as u8;
    validate!(prefix < 9);
    let bits                                            = br.read(prefix + 4)? as i16;
    let bits = bits | (1 << (prefix + 4));
    if sign {
        Ok(bits)
    } else {
        Ok(-bits)
    }
}

fn get_band_start(info: &ICSInfo, sbinfo: &GASubbandInfo, swb: usize) -> usize {
    if info.long_win {
        sbinfo.long_bands[swb]
    } else {
        sbinfo.short_bands[swb]
    }
}

const NOISE_SCALE_MIN:      i16 = -100;

pub fn decode_scale_factor_data(br: &mut BitReader, scales: &mut [[u8; MAX_SFBS]; MAX_WINDOWS], global_gain: u8, info: &ICSInfo, sfb_cb: &[[u8; MAX_SFBS]; MAX_WINDOWS], codebooks: &Codebooks) -> DecoderResult<()> {
    let mut noise_pcm_flag = true;
    let mut scf_normal = i16::from(global_gain);
    let mut scf_intensity = 0i16;
    let mut scf_noise  = 0i16;
    for g in 0..info.window_groups {
        for sfb in 0..info.max_sfb {
            if sfb_cb[g][sfb] != ZERO_HCB {
                if (sfb_cb[g][sfb] == INTENSITY_HCB) || (sfb_cb[g][sfb] == INTENSITY_HCB2) {
                    let diff                        = i16::from(br.read_cb(&codebooks.scale_cb)?);
                    scf_intensity += diff;
                    validate!((scf_intensity >= INTENSITY_SCALE_MIN) && (scf_intensity < INTENSITY_SCALE_MIN + 256));
                    scales[g][sfb] = (scf_intensity - INTENSITY_SCALE_MIN) as u8;
                } else if sfb_cb[g][sfb] == NOISE_HCB {
                    if noise_pcm_flag {
                        noise_pcm_flag = false;
                        scf_noise                   = (br.read(9)? as i16) - 256 + i16::from(global_gain) - 90;
                    } else {
                        scf_noise                  += i16::from(br.read_cb(&codebooks.scale_cb)?);
                    }
                    validate!((scf_noise >= NOISE_SCALE_MIN) && (scf_noise < NOISE_SCALE_MIN + 256));
                    scales[g][sfb] = (scf_noise - NOISE_SCALE_MIN) as u8;
                } else {
                    scf_normal                     += i16::from(br.read_cb(&codebooks.scale_cb)?);
                    validate!((scf_normal >= 0) && (scf_normal < 255));
                    scales[g][sfb] = scf_normal as u8;
                }
            }
        }
    }
    Ok(())
}

pub fn decode_spectrum(br: &mut BitReader, coeffs: &mut [f32; 1024], scales: &[[u8; MAX_SFBS]; MAX_WINDOWS], info: &ICSInfo, sbinfo: &GASubbandInfo, sfb_cb: &[[u8; MAX_SFBS]; MAX_WINDOWS], codebooks: &Codebooks) -> DecoderResult<()> {
    for g in 0..info.window_groups {
        let cur_w   = info.get_group_start(g);
        let next_w  = info.get_group_start(g + 1);
        for sfb in 0..info.max_sfb {
            let start = get_band_start(info, sbinfo, sfb);
            let end   = get_band_start(info, sbinfo, sfb + 1);
            let cb_idx = sfb_cb[g][sfb];
            for w in cur_w..next_w {
                let dst = &mut coeffs[start + w*128..end + w*128];
                match cb_idx {
                    ZERO_HCB => { /* zeroes */ },
                    NOISE_HCB => { /* noise */ },
                    INTENSITY_HCB | INTENSITY_HCB2 => { /* intensity */ },
                    _ => {
                            let unsigned = AAC_UNSIGNED_CODEBOOK[(cb_idx - 1) as usize];
                            let scale = get_scale(scales[g][sfb]);
                            let cb = &codebooks.spec_cb[(cb_idx - 1) as usize];
                            if cb_idx < FIRST_PAIR_HCB {
                                decode_quads(br, cb, unsigned, scale, dst)?;
                            } else {
                                decode_pairs(br, cb, unsigned, cb_idx == ESC_HCB,
                                             AAC_CODEBOOK_MODULO[(cb_idx - FIRST_PAIR_HCB) as usize], scale, dst)?;
                            }
                        },
                };
            }
        }
    }
    Ok(())
}

const AAC_SCF_CODEBOOK_BITS: &[u8] = &[
    18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 18, 19, 18, 17, 17, 16, 17, 16, 16, 16, 16, 15, 15,
    14, 14, 14, 14, 14, 14, 13, 13, 12, 12, 12, 11, 12, 11, 10, 10,
    10,  9,  9,  8,  8,  8,  7,  6,  6,  5,  4,  3,  1,  4,  4,  5,
     6,  6,  7,  7,  8,  8,  9,  9, 10, 10, 10, 11, 11, 11, 11, 12,
    12, 13, 13, 13, 14, 14, 16, 15, 16, 15, 18, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19, 19
];

const AAC_SCF_CODEBOOK_CODES: &[u32] = &[
    0x3FFE8, 0x3FFE6, 0x3FFE7, 0x3FFE5, 0x7FFF5, 0x7FFF1, 0x7FFED, 0x7FFF6,
    0x7FFEE, 0x7FFEF, 0x7FFF0, 0x7FFFC, 0x7FFFD, 0x7FFFF, 0x7FFFE, 0x7FFF7,
    0x7FFF8, 0x7FFFB, 0x7FFF9, 0x3FFE4, 0x7FFFA, 0x3FFE3, 0x1FFEF, 0x1FFF0,
    0x0FFF5, 0x1FFEE, 0x0FFF2, 0x0FFF3, 0x0FFF4, 0x0FFF1, 0x07FF6, 0x07FF7,
    0x03FF9, 0x03FF5, 0x03FF7, 0x03FF3, 0x03FF6, 0x03FF2, 0x01FF7, 0x01FF5,
    0x00FF9, 0x00FF7, 0x00FF6, 0x007F9, 0x00FF4, 0x007F8, 0x003F9, 0x003F7,
    0x003F5, 0x001F8, 0x001F7, 0x000FA, 0x000F8, 0x000F6, 0x00079, 0x0003A,
    0x00038, 0x0001A, 0x0000B, 0x00004, 0x00000, 0x0000A, 0x0000C, 0x0001B,
    0x00039, 0x0003B, 0x00078, 0x0007A, 0x000F7, 0x000F9, 0x001F6, 0x001F9,
    0x003F4, 0x003F6, 0x003F8, 0x007F5, 0x007F4, 0x007F6, 0x007F7, 0x00FF5,
    0x00FF8, 0x01FF4, 0x01FF6, 0x01FF8, 0x03FF8, 0x03FF4, 0x0FFF0, 0x07FF4,
    0x0FFF6, 0x07FF5, 0x3FFE2, 0x7FFD9, 0x7FFDA, 0x7FFDB, 0x7FFDC, 0x7FFDD,
    0x7FFDE, 0x7FFD8, 0x7FFD2, 0x7FFD3, 0x7FFD4, 0x7FFD5, 0x7FFD6, 0x7FFF2,
    0x7FFDF, 0x7FFE7, 0x7FFE8, 0x7FFE9, 0x7FFEA, 0x7FFEB, 0x7FFE6, 0x7FFE0,
    0x7FFE1, 0x7FFE2, 0x7FFE3, 0x7FFE4, 0x7FFE5, 0x7FFD7, 0x7FFEC, 0x7FFF4,
    0x7FFF3
];

const AAC_SPEC_CB1_BITS: &[u8] = &[
    11,  9, 11, 10,  7, 10, 11,  9, 11, 10,  7, 10,  7,  5,  7,  9,
     7, 10, 11,  9, 11,  9,  7,  9, 11,  9, 11,  9,  7,  9,  7,  5,
     7,  9,  7,  9,  7,  5,  7,  5,  1,  5,  7,  5,  7,  9,  7,  9,
     7,  5,  7,  9,  7,  9, 11,  9, 11,  9,  7,  9, 11,  9, 11, 10,
     7,  9,  7,  5,  7,  9,  7, 10, 11,  9, 11, 10,  7,  9, 11,  9,
    11
];
const AAC_SPEC_CB1_CODES: &[u16] = &[
    0x7f8, 0x1f1, 0x7fd, 0x3f5, 0x068, 0x3f0, 0x7f7, 0x1ec,
    0x7f5, 0x3f1, 0x072, 0x3f4, 0x074, 0x011, 0x076, 0x1eb,
    0x06c, 0x3f6, 0x7fc, 0x1e1, 0x7f1, 0x1f0, 0x061, 0x1f6,
    0x7f2, 0x1ea, 0x7fb, 0x1f2, 0x069, 0x1ed, 0x077, 0x017,
    0x06f, 0x1e6, 0x064, 0x1e5, 0x067, 0x015, 0x062, 0x012,
    0x000, 0x014, 0x065, 0x016, 0x06d, 0x1e9, 0x063, 0x1e4,
    0x06b, 0x013, 0x071, 0x1e3, 0x070, 0x1f3, 0x7fe, 0x1e7,
    0x7f3, 0x1ef, 0x060, 0x1ee, 0x7f0, 0x1e2, 0x7fa, 0x3f3,
    0x06a, 0x1e8, 0x075, 0x010, 0x073, 0x1f4, 0x06e, 0x3f7,
    0x7f6, 0x1e0, 0x7f9, 0x3f2, 0x066, 0x1f5, 0x7ff, 0x1f7,
    0x7f4
];
const AAC_SPEC_CB2_BITS: &[u8] = &[
    9, 7, 9, 8, 6, 8, 9, 8, 9, 8, 6, 7, 6, 5, 6, 7,
    6, 8, 9, 7, 8, 8, 6, 8, 9, 7, 9, 8, 6, 7, 6, 5,
    6, 7, 6, 8, 6, 5, 6, 5, 3, 5, 6, 5, 6, 8, 6, 7,
    6, 5, 6, 8, 6, 8, 9, 7, 9, 8, 6, 8, 8, 7, 9, 8,
    6, 7, 6, 4, 6, 8, 6, 7, 9, 7, 9, 7, 6, 8, 9, 7,
    9
];
const AAC_SPEC_CB2_CODES: &[u16] = &[
    0x1f3, 0x06f, 0x1fd, 0x0eb, 0x023, 0x0ea, 0x1f7, 0x0e8,
    0x1fa, 0x0f2, 0x02d, 0x070, 0x020, 0x006, 0x02b, 0x06e,
    0x028, 0x0e9, 0x1f9, 0x066, 0x0f8, 0x0e7, 0x01b, 0x0f1,
    0x1f4, 0x06b, 0x1f5, 0x0ec, 0x02a, 0x06c, 0x02c, 0x00a,
    0x027, 0x067, 0x01a, 0x0f5, 0x024, 0x008, 0x01f, 0x009,
    0x000, 0x007, 0x01d, 0x00b, 0x030, 0x0ef, 0x01c, 0x064,
    0x01e, 0x00c, 0x029, 0x0f3, 0x02f, 0x0f0, 0x1fc, 0x071,
    0x1f2, 0x0f4, 0x021, 0x0e6, 0x0f7, 0x068, 0x1f8, 0x0ee,
    0x022, 0x065, 0x031, 0x002, 0x026, 0x0ed, 0x025, 0x06a,
    0x1fb, 0x072, 0x1fe, 0x069, 0x02e, 0x0f6, 0x1ff, 0x06d,
    0x1f6
];
const AAC_SPEC_CB3_BITS: &[u8] = &[
     1,  4,  8,  4,  5,  8,  9,  9, 10,  4,  6,  9,  6,  6,  9,  9,
     9, 10,  9, 10, 13,  9,  9, 11, 11, 10, 12,  4,  6, 10,  6,  7,
    10, 10, 10, 12,  5,  7, 11,  6,  7, 10,  9,  9, 11,  9, 10, 13,
     8,  9, 12, 10, 11, 12,  8, 10, 15,  9, 11, 15, 13, 14, 16,  8,
    10, 14,  9, 10, 14, 12, 12, 15, 11, 12, 16, 10, 11, 15, 12, 12,
    15
];
const AAC_SPEC_CB3_CODES: &[u16] = &[
    0x0000, 0x0009, 0x00ef, 0x000b, 0x0019, 0x00f0, 0x01eb, 0x01e6,
    0x03f2, 0x000a, 0x0035, 0x01ef, 0x0034, 0x0037, 0x01e9, 0x01ed,
    0x01e7, 0x03f3, 0x01ee, 0x03ed, 0x1ffa, 0x01ec, 0x01f2, 0x07f9,
    0x07f8, 0x03f8, 0x0ff8, 0x0008, 0x0038, 0x03f6, 0x0036, 0x0075,
    0x03f1, 0x03eb, 0x03ec, 0x0ff4, 0x0018, 0x0076, 0x07f4, 0x0039,
    0x0074, 0x03ef, 0x01f3, 0x01f4, 0x07f6, 0x01e8, 0x03ea, 0x1ffc,
    0x00f2, 0x01f1, 0x0ffb, 0x03f5, 0x07f3, 0x0ffc, 0x00ee, 0x03f7,
    0x7ffe, 0x01f0, 0x07f5, 0x7ffd, 0x1ffb, 0x3ffa, 0xffff, 0x00f1,
    0x03f0, 0x3ffc, 0x01ea, 0x03ee, 0x3ffb, 0x0ff6, 0x0ffa, 0x7ffc,
    0x07f2, 0x0ff5, 0xfffe, 0x03f4, 0x07f7, 0x7ffb, 0x0ff7, 0x0ff9,
    0x7ffa
];
const AAC_SPEC_CB4_BITS: &[u8] = &[
     4,  5,  8,  5,  4,  8,  9,  8, 11,  5,  5,  8,  5,  4,  8,  8,
     7, 10,  9,  8, 11,  8,  8, 10, 11, 10, 11,  4,  5,  8,  4,  4,
     8,  8,  8, 10,  4,  4,  8,  4,  4,  7,  8,  7,  9,  8,  8, 10,
     7,  7,  9, 10,  9, 10,  8,  8, 11,  8,  7, 10, 11, 10, 12,  8,
     7, 10,  7,  7,  9, 10,  9, 11, 11, 10, 12, 10,  9, 11, 11, 10,
    11
];
const AAC_SPEC_CB4_CODES: &[u16] = &[
    0x007, 0x016, 0x0f6, 0x018, 0x008, 0x0ef, 0x1ef, 0x0f3,
    0x7f8, 0x019, 0x017, 0x0ed, 0x015, 0x001, 0x0e2, 0x0f0,
    0x070, 0x3f0, 0x1ee, 0x0f1, 0x7fa, 0x0ee, 0x0e4, 0x3f2,
    0x7f6, 0x3ef, 0x7fd, 0x005, 0x014, 0x0f2, 0x009, 0x004,
    0x0e5, 0x0f4, 0x0e8, 0x3f4, 0x006, 0x002, 0x0e7, 0x003,
    0x000, 0x06b, 0x0e3, 0x069, 0x1f3, 0x0eb, 0x0e6, 0x3f6,
    0x06e, 0x06a, 0x1f4, 0x3ec, 0x1f0, 0x3f9, 0x0f5, 0x0ec,
    0x7fb, 0x0ea, 0x06f, 0x3f7, 0x7f9, 0x3f3, 0xfff, 0x0e9,
    0x06d, 0x3f8, 0x06c, 0x068, 0x1f5, 0x3ee, 0x1f2, 0x7f4,
    0x7f7, 0x3f1, 0xffe, 0x3ed, 0x1f1, 0x7f5, 0x7fe, 0x3f5,
    0x7fc
];
const AAC_SPEC_CB5_BITS: &[u8] = &[
    13, 12, 11, 11, 10, 11, 11, 12, 13, 12, 11, 10,  9,  8,  9, 10,
    11, 12, 12, 10,  9,  8,  7,  8,  9, 10, 11, 11,  9,  8,  5,  4,
     5,  8,  9, 11, 10,  8,  7,  4,  1,  4,  7,  8, 11, 11,  9,  8,
     5,  4,  5,  8,  9, 11, 11, 10,  9,  8,  7,  8,  9, 10, 11, 12,
    11, 10,  9,  8,  9, 10, 11, 12, 13, 12, 12, 11, 10, 10, 11, 12,
    13
];
const AAC_SPEC_CB5_CODES: &[u16] = &[
    0x1fff, 0x0ff7, 0x07f4, 0x07e8, 0x03f1, 0x07ee, 0x07f9, 0x0ff8,
    0x1ffd, 0x0ffd, 0x07f1, 0x03e8, 0x01e8, 0x00f0, 0x01ec, 0x03ee,
    0x07f2, 0x0ffa, 0x0ff4, 0x03ef, 0x01f2, 0x00e8, 0x0070, 0x00ec,
    0x01f0, 0x03ea, 0x07f3, 0x07eb, 0x01eb, 0x00ea, 0x001a, 0x0008,
    0x0019, 0x00ee, 0x01ef, 0x07ed, 0x03f0, 0x00f2, 0x0073, 0x000b,
    0x0000, 0x000a, 0x0071, 0x00f3, 0x07e9, 0x07ef, 0x01ee, 0x00ef,
    0x0018, 0x0009, 0x001b, 0x00eb, 0x01e9, 0x07ec, 0x07f6, 0x03eb,
    0x01f3, 0x00ed, 0x0072, 0x00e9, 0x01f1, 0x03ed, 0x07f7, 0x0ff6,
    0x07f0, 0x03e9, 0x01ed, 0x00f1, 0x01ea, 0x03ec, 0x07f8, 0x0ff9,
    0x1ffc, 0x0ffc, 0x0ff5, 0x07ea, 0x03f3, 0x03f2, 0x07f5, 0x0ffb,
    0x1ffe
];
const AAC_SPEC_CB6_BITS: &[u8] = &[
    11, 10,  9,  9,  9,  9,  9, 10, 11, 10,  9,  8,  7,  7,  7,  8,
     9, 10,  9,  8,  6,  6,  6,  6,  6,  8,  9,  9,  7,  6,  4,  4,
     4,  6,  7,  9,  9,  7,  6,  4,  4,  4,  6,  7,  9,  9,  7,  6,
     4,  4,  4,  6,  7,  9,  9,  8,  6,  6,  6,  6,  6,  8,  9, 10,
     9,  8,  7,  7,  7,  7,  8, 10, 11, 10,  9,  9,  9,  9,  9, 10,
    11
];
const AAC_SPEC_CB6_CODES: &[u16] = &[
    0x7fe, 0x3fd, 0x1f1, 0x1eb, 0x1f4, 0x1ea, 0x1f0, 0x3fc,
    0x7fd, 0x3f6, 0x1e5, 0x0ea, 0x06c, 0x071, 0x068, 0x0f0,
    0x1e6, 0x3f7, 0x1f3, 0x0ef, 0x032, 0x027, 0x028, 0x026,
    0x031, 0x0eb, 0x1f7, 0x1e8, 0x06f, 0x02e, 0x008, 0x004,
    0x006, 0x029, 0x06b, 0x1ee, 0x1ef, 0x072, 0x02d, 0x002,
    0x000, 0x003, 0x02f, 0x073, 0x1fa, 0x1e7, 0x06e, 0x02b,
    0x007, 0x001, 0x005, 0x02c, 0x06d, 0x1ec, 0x1f9, 0x0ee,
    0x030, 0x024, 0x02a, 0x025, 0x033, 0x0ec, 0x1f2, 0x3f8,
    0x1e4, 0x0ed, 0x06a, 0x070, 0x069, 0x074, 0x0f1, 0x3fa,
    0x7ff, 0x3f9, 0x1f6, 0x1ed, 0x1f8, 0x1e9, 0x1f5, 0x3fb,
    0x7fc
];
const AAC_SPEC_CB7_BITS: &[u8] = &[
     1,  3,  6,  7,  8,  9, 10, 11,  3,  4,  6,  7,  8,  8,  9,  9,
     6,  6,  7,  8,  8,  9,  9, 10,  7,  7,  8,  8,  9,  9, 10, 10,
     8,  8,  9,  9, 10, 10, 10, 11,  9,  8,  9,  9, 10, 10, 11, 11,
    10,  9,  9, 10, 10, 11, 12, 12, 11, 10, 10, 10, 11, 11, 12, 12
];
const AAC_SPEC_CB7_CODES: &[u16] = &[
    0x000, 0x005, 0x037, 0x074, 0x0f2, 0x1eb, 0x3ed, 0x7f7,
    0x004, 0x00c, 0x035, 0x071, 0x0ec, 0x0ee, 0x1ee, 0x1f5,
    0x036, 0x034, 0x072, 0x0ea, 0x0f1, 0x1e9, 0x1f3, 0x3f5,
    0x073, 0x070, 0x0eb, 0x0f0, 0x1f1, 0x1f0, 0x3ec, 0x3fa,
    0x0f3, 0x0ed, 0x1e8, 0x1ef, 0x3ef, 0x3f1, 0x3f9, 0x7fb,
    0x1ed, 0x0ef, 0x1ea, 0x1f2, 0x3f3, 0x3f8, 0x7f9, 0x7fc,
    0x3ee, 0x1ec, 0x1f4, 0x3f4, 0x3f7, 0x7f8, 0xffd, 0xffe,
    0x7f6, 0x3f0, 0x3f2, 0x3f6, 0x7fa, 0x7fd, 0xffc, 0xfff
];
const AAC_SPEC_CB8_BITS: &[u8] = &[
     5,  4,  5,  6,  7,  8,  9, 10,  4,  3,  4,  5,  6,  7,  7,  8,
     5,  4,  4,  5,  6,  7,  7,  8,  6,  5,  5,  6,  6,  7,  8,  8,
     7,  6,  6,  6,  7,  7,  8,  9,  8,  7,  6,  7,  7,  8,  8, 10,
     9,  7,  7,  8,  8,  8,  9,  9, 10,  8,  8,  8,  9,  9,  9, 10
];
const AAC_SPEC_CB8_CODES: &[u16] = &[
    0x00e, 0x005, 0x010, 0x030, 0x06f, 0x0f1, 0x1fa, 0x3fe,
    0x003, 0x000, 0x004, 0x012, 0x02c, 0x06a, 0x075, 0x0f8,
    0x00f, 0x002, 0x006, 0x014, 0x02e, 0x069, 0x072, 0x0f5,
    0x02f, 0x011, 0x013, 0x02a, 0x032, 0x06c, 0x0ec, 0x0fa,
    0x071, 0x02b, 0x02d, 0x031, 0x06d, 0x070, 0x0f2, 0x1f9,
    0x0ef, 0x068, 0x033, 0x06b, 0x06e, 0x0ee, 0x0f9, 0x3fc,
    0x1f8, 0x074, 0x073, 0x0ed, 0x0f0, 0x0f6, 0x1f6, 0x1fd,
    0x3fd, 0x0f3, 0x0f4, 0x0f7, 0x1f7, 0x1fb, 0x1fc, 0x3ff
];
const AAC_SPEC_CB9_BITS: &[u8] = &[
     1,  3,  6,  8,  9, 10, 10, 11, 11, 12, 12, 13, 13,  3,  4,  6,
     7,  8,  8,  9, 10, 10, 10, 11, 12, 12,  6,  6,  7,  8,  8,  9,
    10, 10, 10, 11, 12, 12, 12,  8,  7,  8,  9,  9, 10, 10, 11, 11,
    11, 12, 12, 13,  9,  8,  9,  9, 10, 10, 11, 11, 11, 12, 12, 12,
    13, 10,  9,  9, 10, 11, 11, 11, 12, 11, 12, 12, 13, 13, 11,  9,
    10, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 11, 10, 10, 11, 11,
    12, 12, 13, 13, 13, 13, 13, 13, 11, 10, 10, 11, 11, 11, 12, 12,
    13, 13, 14, 13, 14, 11, 10, 11, 11, 12, 12, 12, 12, 13, 13, 14,
    14, 14, 12, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 12,
    11, 12, 12, 12, 13, 13, 13, 13, 14, 14, 15, 15, 13, 12, 12, 12,
    13, 13, 13, 13, 14, 14, 14, 14, 15
];
const AAC_SPEC_CB9_CODES: &[u16] = &[
    0x0000, 0x0005, 0x0037, 0x00e7, 0x01de, 0x03ce, 0x03d9, 0x07c8,
    0x07cd, 0x0fc8, 0x0fdd, 0x1fe4, 0x1fec, 0x0004, 0x000c, 0x0035,
    0x0072, 0x00ea, 0x00ed, 0x01e2, 0x03d1, 0x03d3, 0x03e0, 0x07d8,
    0x0fcf, 0x0fd5, 0x0036, 0x0034, 0x0071, 0x00e8, 0x00ec, 0x01e1,
    0x03cf, 0x03dd, 0x03db, 0x07d0, 0x0fc7, 0x0fd4, 0x0fe4, 0x00e6,
    0x0070, 0x00e9, 0x01dd, 0x01e3, 0x03d2, 0x03dc, 0x07cc, 0x07ca,
    0x07de, 0x0fd8, 0x0fea, 0x1fdb, 0x01df, 0x00eb, 0x01dc, 0x01e6,
    0x03d5, 0x03de, 0x07cb, 0x07dd, 0x07dc, 0x0fcd, 0x0fe2, 0x0fe7,
    0x1fe1, 0x03d0, 0x01e0, 0x01e4, 0x03d6, 0x07c5, 0x07d1, 0x07db,
    0x0fd2, 0x07e0, 0x0fd9, 0x0feb, 0x1fe3, 0x1fe9, 0x07c4, 0x01e5,
    0x03d7, 0x07c6, 0x07cf, 0x07da, 0x0fcb, 0x0fda, 0x0fe3, 0x0fe9,
    0x1fe6, 0x1ff3, 0x1ff7, 0x07d3, 0x03d8, 0x03e1, 0x07d4, 0x07d9,
    0x0fd3, 0x0fde, 0x1fdd, 0x1fd9, 0x1fe2, 0x1fea, 0x1ff1, 0x1ff6,
    0x07d2, 0x03d4, 0x03da, 0x07c7, 0x07d7, 0x07e2, 0x0fce, 0x0fdb,
    0x1fd8, 0x1fee, 0x3ff0, 0x1ff4, 0x3ff2, 0x07e1, 0x03df, 0x07c9,
    0x07d6, 0x0fca, 0x0fd0, 0x0fe5, 0x0fe6, 0x1feb, 0x1fef, 0x3ff3,
    0x3ff4, 0x3ff5, 0x0fe0, 0x07ce, 0x07d5, 0x0fc6, 0x0fd1, 0x0fe1,
    0x1fe0, 0x1fe8, 0x1ff0, 0x3ff1, 0x3ff8, 0x3ff6, 0x7ffc, 0x0fe8,
    0x07df, 0x0fc9, 0x0fd7, 0x0fdc, 0x1fdc, 0x1fdf, 0x1fed, 0x1ff5,
    0x3ff9, 0x3ffb, 0x7ffd, 0x7ffe, 0x1fe7, 0x0fcc, 0x0fd6, 0x0fdf,
    0x1fde, 0x1fda, 0x1fe5, 0x1ff2, 0x3ffa, 0x3ff7, 0x3ffc, 0x3ffd,
    0x7fff
];
const AAC_SPEC_CB10_BITS: &[u8] = &[
     6,  5,  6,  6,  7,  8,  9, 10, 10, 10, 11, 11, 12,  5,  4,  4,
     5,  6,  7,  7,  8,  8,  9, 10, 10, 11,  6,  4,  5,  5,  6,  6,
     7,  8,  8,  9,  9, 10, 10,  6,  5,  5,  5,  6,  7,  7,  8,  8,
     9,  9, 10, 10,  7,  6,  6,  6,  6,  7,  7,  8,  8,  9,  9, 10,
    10,  8,  7,  6,  7,  7,  7,  8,  8,  8,  9, 10, 10, 11,  9,  7,
     7,  7,  7,  8,  8,  9,  9,  9, 10, 10, 11,  9,  8,  8,  8,  8,
     8,  9,  9,  9, 10, 10, 11, 11,  9,  8,  8,  8,  8,  8,  9,  9,
    10, 10, 10, 11, 11, 10,  9,  9,  9,  9,  9,  9, 10, 10, 10, 11,
    11, 12, 10,  9,  9,  9,  9, 10, 10, 10, 10, 11, 11, 11, 12, 11,
    10,  9, 10, 10, 10, 10, 10, 11, 11, 11, 11, 12, 11, 10, 10, 10,
    10, 10, 10, 11, 11, 12, 12, 12, 12
];
const AAC_SPEC_CB10_CODES: &[u16] = &[
    0x022, 0x008, 0x01d, 0x026, 0x05f, 0x0d3, 0x1cf, 0x3d0,
    0x3d7, 0x3ed, 0x7f0, 0x7f6, 0xffd, 0x007, 0x000, 0x001,
    0x009, 0x020, 0x054, 0x060, 0x0d5, 0x0dc, 0x1d4, 0x3cd,
    0x3de, 0x7e7, 0x01c, 0x002, 0x006, 0x00c, 0x01e, 0x028,
    0x05b, 0x0cd, 0x0d9, 0x1ce, 0x1dc, 0x3d9, 0x3f1, 0x025,
    0x00b, 0x00a, 0x00d, 0x024, 0x057, 0x061, 0x0cc, 0x0dd,
    0x1cc, 0x1de, 0x3d3, 0x3e7, 0x05d, 0x021, 0x01f, 0x023,
    0x027, 0x059, 0x064, 0x0d8, 0x0df, 0x1d2, 0x1e2, 0x3dd,
    0x3ee, 0x0d1, 0x055, 0x029, 0x056, 0x058, 0x062, 0x0ce,
    0x0e0, 0x0e2, 0x1da, 0x3d4, 0x3e3, 0x7eb, 0x1c9, 0x05e,
    0x05a, 0x05c, 0x063, 0x0ca, 0x0da, 0x1c7, 0x1ca, 0x1e0,
    0x3db, 0x3e8, 0x7ec, 0x1e3, 0x0d2, 0x0cb, 0x0d0, 0x0d7,
    0x0db, 0x1c6, 0x1d5, 0x1d8, 0x3ca, 0x3da, 0x7ea, 0x7f1,
    0x1e1, 0x0d4, 0x0cf, 0x0d6, 0x0de, 0x0e1, 0x1d0, 0x1d6,
    0x3d1, 0x3d5, 0x3f2, 0x7ee, 0x7fb, 0x3e9, 0x1cd, 0x1c8,
    0x1cb, 0x1d1, 0x1d7, 0x1df, 0x3cf, 0x3e0, 0x3ef, 0x7e6,
    0x7f8, 0xffa, 0x3eb, 0x1dd, 0x1d3, 0x1d9, 0x1db, 0x3d2,
    0x3cc, 0x3dc, 0x3ea, 0x7ed, 0x7f3, 0x7f9, 0xff9, 0x7f2,
    0x3ce, 0x1e4, 0x3cb, 0x3d8, 0x3d6, 0x3e2, 0x3e5, 0x7e8,
    0x7f4, 0x7f5, 0x7f7, 0xffb, 0x7fa, 0x3ec, 0x3df, 0x3e1,
    0x3e4, 0x3e6, 0x3f0, 0x7e9, 0x7ef, 0xff8, 0xffe, 0xffc,
    0xfff
];
const AAC_SPEC_CB11_BITS: &[u8] = &[
     4,  5,  6,  7,  8,  8,  9, 10, 10, 10, 11, 11, 12, 11, 12, 12,
    10,  5,  4,  5,  6,  7,  7,  8,  8,  9,  9,  9, 10, 10, 10, 10,
    11,  8,  6,  5,  5,  6,  7,  7,  8,  8,  8,  9,  9,  9, 10, 10,
    10, 10,  8,  7,  6,  6,  6,  7,  7,  8,  8,  8,  9,  9,  9, 10,
    10, 10, 10,  8,  8,  7,  7,  7,  7,  8,  8,  8,  8,  9,  9,  9,
    10, 10, 10, 10,  8,  8,  7,  7,  7,  7,  8,  8,  8,  9,  9,  9,
     9, 10, 10, 10, 10,  8,  9,  8,  8,  8,  8,  8,  8,  8,  9,  9,
     9, 10, 10, 10, 10, 10,  8,  9,  8,  8,  8,  8,  8,  8,  9,  9,
     9, 10, 10, 10, 10, 10, 10,  8, 10,  9,  8,  8,  9,  9,  9,  9,
     9, 10, 10, 10, 10, 10, 10, 11,  8, 10,  9,  9,  9,  9,  9,  9,
     9, 10, 10, 10, 10, 10, 10, 11, 11,  8, 11,  9,  9,  9,  9,  9,
     9, 10, 10, 10, 10, 10, 11, 10, 11, 11,  8, 11, 10,  9,  9, 10,
     9, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11,  8, 11, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11,  9, 11, 10,  9,
     9, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11,  9, 11, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11,  9, 12,
    10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 12, 12,  9,
     9,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  9,
     5
];
const AAC_SPEC_CB11_CODES: &[u16] = &[
    0x000, 0x006, 0x019, 0x03d, 0x09c, 0x0c6, 0x1a7, 0x390,
    0x3c2, 0x3df, 0x7e6, 0x7f3, 0xffb, 0x7ec, 0xffa, 0xffe,
    0x38e, 0x005, 0x001, 0x008, 0x014, 0x037, 0x042, 0x092,
    0x0af, 0x191, 0x1a5, 0x1b5, 0x39e, 0x3c0, 0x3a2, 0x3cd,
    0x7d6, 0x0ae, 0x017, 0x007, 0x009, 0x018, 0x039, 0x040,
    0x08e, 0x0a3, 0x0b8, 0x199, 0x1ac, 0x1c1, 0x3b1, 0x396,
    0x3be, 0x3ca, 0x09d, 0x03c, 0x015, 0x016, 0x01a, 0x03b,
    0x044, 0x091, 0x0a5, 0x0be, 0x196, 0x1ae, 0x1b9, 0x3a1,
    0x391, 0x3a5, 0x3d5, 0x094, 0x09a, 0x036, 0x038, 0x03a,
    0x041, 0x08c, 0x09b, 0x0b0, 0x0c3, 0x19e, 0x1ab, 0x1bc,
    0x39f, 0x38f, 0x3a9, 0x3cf, 0x093, 0x0bf, 0x03e, 0x03f,
    0x043, 0x045, 0x09e, 0x0a7, 0x0b9, 0x194, 0x1a2, 0x1ba,
    0x1c3, 0x3a6, 0x3a7, 0x3bb, 0x3d4, 0x09f, 0x1a0, 0x08f,
    0x08d, 0x090, 0x098, 0x0a6, 0x0b6, 0x0c4, 0x19f, 0x1af,
    0x1bf, 0x399, 0x3bf, 0x3b4, 0x3c9, 0x3e7, 0x0a8, 0x1b6,
    0x0ab, 0x0a4, 0x0aa, 0x0b2, 0x0c2, 0x0c5, 0x198, 0x1a4,
    0x1b8, 0x38c, 0x3a4, 0x3c4, 0x3c6, 0x3dd, 0x3e8, 0x0ad,
    0x3af, 0x192, 0x0bd, 0x0bc, 0x18e, 0x197, 0x19a, 0x1a3,
    0x1b1, 0x38d, 0x398, 0x3b7, 0x3d3, 0x3d1, 0x3db, 0x7dd,
    0x0b4, 0x3de, 0x1a9, 0x19b, 0x19c, 0x1a1, 0x1aa, 0x1ad,
    0x1b3, 0x38b, 0x3b2, 0x3b8, 0x3ce, 0x3e1, 0x3e0, 0x7d2,
    0x7e5, 0x0b7, 0x7e3, 0x1bb, 0x1a8, 0x1a6, 0x1b0, 0x1b2,
    0x1b7, 0x39b, 0x39a, 0x3ba, 0x3b5, 0x3d6, 0x7d7, 0x3e4,
    0x7d8, 0x7ea, 0x0ba, 0x7e8, 0x3a0, 0x1bd, 0x1b4, 0x38a,
    0x1c4, 0x392, 0x3aa, 0x3b0, 0x3bc, 0x3d7, 0x7d4, 0x7dc,
    0x7db, 0x7d5, 0x7f0, 0x0c1, 0x7fb, 0x3c8, 0x3a3, 0x395,
    0x39d, 0x3ac, 0x3ae, 0x3c5, 0x3d8, 0x3e2, 0x3e6, 0x7e4,
    0x7e7, 0x7e0, 0x7e9, 0x7f7, 0x190, 0x7f2, 0x393, 0x1be,
    0x1c0, 0x394, 0x397, 0x3ad, 0x3c3, 0x3c1, 0x3d2, 0x7da,
    0x7d9, 0x7df, 0x7eb, 0x7f4, 0x7fa, 0x195, 0x7f8, 0x3bd,
    0x39c, 0x3ab, 0x3a8, 0x3b3, 0x3b9, 0x3d0, 0x3e3, 0x3e5,
    0x7e2, 0x7de, 0x7ed, 0x7f1, 0x7f9, 0x7fc, 0x193, 0xffd,
    0x3dc, 0x3b6, 0x3c7, 0x3cc, 0x3cb, 0x3d9, 0x3da, 0x7d3,
    0x7e1, 0x7ee, 0x7ef, 0x7f5, 0x7f6, 0xffc, 0xfff, 0x19d,
    0x1c2, 0x0b5, 0x0a1, 0x096, 0x097, 0x095, 0x099, 0x0a0,
    0x0a2, 0x0ac, 0x0a9, 0x0b1, 0x0b3, 0x0bb, 0x0c0, 0x18f,
    0x004
];

const AAC_SPEC_BITS: [&[u8]; 11] = [
    AAC_SPEC_CB1_BITS, AAC_SPEC_CB2_BITS, AAC_SPEC_CB3_BITS, AAC_SPEC_CB4_BITS,
    AAC_SPEC_CB5_BITS, AAC_SPEC_CB6_BITS, AAC_SPEC_CB7_BITS, AAC_SPEC_CB8_BITS,
    AAC_SPEC_CB9_BITS, AAC_SPEC_CB10_BITS, AAC_SPEC_CB11_BITS
];
const AAC_SPEC_CODES: [&[u16]; 11] = [
    AAC_SPEC_CB1_CODES, AAC_SPEC_CB2_CODES, AAC_SPEC_CB3_CODES, AAC_SPEC_CB4_CODES,
    AAC_SPEC_CB5_CODES, AAC_SPEC_CB6_CODES, AAC_SPEC_CB7_CODES, AAC_SPEC_CB8_CODES,
    AAC_SPEC_CB9_CODES, AAC_SPEC_CB10_CODES, AAC_SPEC_CB11_CODES
];
const AAC_UNSIGNED_CODEBOOK: [bool; 11] = [
    false, false, true, true, false, false, true, true, true, true, true
];
const AAC_CODEBOOK_MODULO: [u16; 7] = [
    9, 9, 8, 8, 13, 13, 17
];

const AAC_QUADS: [[i8; 4]; 81] = [
    [ 0, 0, 0, 0 ], [ 0, 0, 0, 1 ], [ 0, 0, 0, 2 ],
    [ 0, 0, 1, 0 ], [ 0, 0, 1, 1 ], [ 0, 0, 1, 2 ],
    [ 0, 0, 2, 0 ], [ 0, 0, 2, 1 ], [ 0, 0, 2, 2 ],
    [ 0, 1, 0, 0 ], [ 0, 1, 0, 1 ], [ 0, 1, 0, 2 ],
    [ 0, 1, 1, 0 ], [ 0, 1, 1, 1 ], [ 0, 1, 1, 2 ],
    [ 0, 1, 2, 0 ], [ 0, 1, 2, 1 ], [ 0, 1, 2, 2 ],
    [ 0, 2, 0, 0 ], [ 0, 2, 0, 1 ], [ 0, 2, 0, 2 ],
    [ 0, 2, 1, 0 ], [ 0, 2, 1, 1 ], [ 0, 2, 1, 2 ],
    [ 0, 2, 2, 0 ], [ 0, 2, 2, 1 ], [ 0, 2, 2, 2 ],
    [ 1, 0, 0, 0 ], [ 1, 0, 0, 1 ], [ 1, 0, 0, 2 ],
    [ 1, 0, 1, 0 ], [ 1, 0, 1, 1 ], [ 1, 0, 1, 2 ],
    [ 1, 0, 2, 0 ], [ 1, 0, 2, 1 ], [ 1, 0, 2, 2 ],
    [ 1, 1, 0, 0 ], [ 1, 1, 0, 1 ], [ 1, 1, 0, 2 ],
    [ 1, 1, 1, 0 ], [ 1, 1, 1, 1 ], [ 1, 1, 1, 2 ],
    [ 1, 1, 2, 0 ], [ 1, 1, 2, 1 ], [ 1, 1, 2, 2 ],
    [ 1, 2, 0, 0 ], [ 1, 2, 0, 1 ], [ 1, 2, 0, 2 ],
    [ 1, 2, 1, 0 ], [ 1, 2, 1, 1 ], [ 1, 2, 1, 2 ],
    [ 1, 2, 2, 0 ], [ 1, 2, 2, 1 ], [ 1, 2, 2, 2 ],
    [ 2, 0, 0, 0 ], [ 2, 0, 0, 1 ], [ 2, 0, 0, 2 ],
    [ 2, 0, 1, 0 ], [ 2, 0, 1, 1 ], [ 2, 0, 1, 2 ],
    [ 2, 0, 2, 0 ], [ 2, 0, 2, 1 ], [ 2, 0, 2, 2 ],
    [ 2, 1, 0, 0 ], [ 2, 1, 0, 1 ], [ 2, 1, 0, 2 ],
    [ 2, 1, 1, 0 ], [ 2, 1, 1, 1 ], [ 2, 1, 1, 2 ],
    [ 2, 1, 2, 0 ], [ 2, 1, 2, 1 ], [ 2, 1, 2, 2 ],
    [ 2, 2, 0, 0 ], [ 2, 2, 0, 1 ], [ 2, 2, 0, 2 ],
    [ 2, 2, 1, 0 ], [ 2, 2, 1, 1 ], [ 2, 2, 1, 2 ],
    [ 2, 2, 2, 0 ], [ 2, 2, 2, 1 ], [ 2, 2, 2, 2 ],
];

