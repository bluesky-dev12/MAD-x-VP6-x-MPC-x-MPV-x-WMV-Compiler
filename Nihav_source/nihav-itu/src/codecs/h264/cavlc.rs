use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::io::intcode::*;
use super::*;
use super::dsp::{CHROMA_DC_SCAN, ZIGZAG, ZIGZAG1};
use super::slice::SliceHeader;

fn map_i_type(idx: usize) -> MBType {
    if idx == 0 {
        MBType::Intra4x4
    } else if idx == 25 {
        MBType::PCM
    } else {
        let imode = ((idx - 1) & 3) as u8;
        let cbpc  = ((idx - 1) / 4) as u8;
        let (cbpy, cbpc) = if cbpc >= 3 { (0xF, cbpc - 3) } else { (0x0, cbpc) };
        MBType::Intra16x16(imode, cbpy, cbpc)
    }
}

const NUM_I_TYPES: usize = 26;

const P_TYPES: [MBType; 5] = [
    MBType::P16x16, MBType::P16x8, MBType::P8x16, MBType::P8x8, MBType::P8x8Ref0
];

const B_TYPES: [MBType; 23] = [
    MBType::Direct,
    MBType::B16x16(BMode::L0),
    MBType::B16x16(BMode::L1),
    MBType::B16x16(BMode::Bi),
    MBType::B16x8(BMode::L0, BMode::L0),
    MBType::B8x16(BMode::L0, BMode::L0),
    MBType::B16x8(BMode::L1, BMode::L1),
    MBType::B8x16(BMode::L1, BMode::L1),
    MBType::B16x8(BMode::L0, BMode::L1),
    MBType::B8x16(BMode::L0, BMode::L1),
    MBType::B16x8(BMode::L1, BMode::L0),
    MBType::B8x16(BMode::L1, BMode::L0),
    MBType::B16x8(BMode::L0, BMode::Bi),
    MBType::B8x16(BMode::L0, BMode::Bi),
    MBType::B16x8(BMode::L1, BMode::Bi),
    MBType::B8x16(BMode::L1, BMode::Bi),
    MBType::B16x8(BMode::Bi, BMode::L0),
    MBType::B8x16(BMode::Bi, BMode::L0),
    MBType::B16x8(BMode::Bi, BMode::L1),
    MBType::B8x16(BMode::Bi, BMode::L1),
    MBType::B16x8(BMode::Bi, BMode::Bi),
    MBType::B8x16(BMode::Bi, BMode::Bi),
    MBType::B8x8,
];

pub fn decode_mb_type_cavlc(br: &mut BitReader, slice_hdr: &SliceHeader) -> DecoderResult<MBType> {
    let mb_type_id                      = br.read_ue()? as usize;
    match slice_hdr.slice_type {
        SliceType::I => {
            validate!(mb_type_id < NUM_I_TYPES);
            Ok(map_i_type(mb_type_id))
        },
        SliceType::SI => {
            validate!(mb_type_id < NUM_I_TYPES + 1);
            if mb_type_id == 0 {
                Ok(MBType::Intra4x4) // special SI one
            } else {
                Ok(map_i_type(mb_type_id - 1))
            }
        },
        SliceType::P | SliceType::SP => {
            validate!(mb_type_id < NUM_I_TYPES + P_TYPES.len());
            if mb_type_id < P_TYPES.len() {
                Ok(P_TYPES[mb_type_id])
            } else {
                Ok(map_i_type(mb_type_id - P_TYPES.len()))
            }
        },
        SliceType::B => {
            validate!(mb_type_id < NUM_I_TYPES + B_TYPES.len());
            if mb_type_id < B_TYPES.len() {
                Ok(B_TYPES[mb_type_id])
            } else {
                Ok(map_i_type(mb_type_id - B_TYPES.len()))
            }
        },
    }
}

fn read_refs(br: &mut BitReader, dst: &mut [PicRef], num_refs: usize) -> DecoderResult<()> {
    if num_refs > 1 {
        for pic_ref in dst.iter_mut() {
            *pic_ref                                = PicRef::new(br.read_te(num_refs as u32 - 1)? as u8);
        }
    } else {
        for pic_ref in dst.iter_mut() {
            *pic_ref = ZERO_REF;
        }
    }
    Ok(())
}

fn read_mvs(br: &mut BitReader, mvs: &mut [MV]) -> DecoderResult<()> {
    for mv in mvs.iter_mut() {
        mv.x                                        = br.read_se()? as i16;
        mv.y                                        = br.read_se()? as i16;
    }
    Ok(())
}

#[allow(clippy::cognitive_complexity)]
pub fn decode_mb_pred_cavlc(br: &mut BitReader, slice_hdr: &SliceHeader, mb_type: MBType, sstate: &mut SliceState, mb_info: &mut CurrentMBInfo) -> DecoderResult<()> {
    mb_info.mb_type = mb_type;
    let num_l0 = slice_hdr.num_ref_idx_l0_active;
    let num_l1 = slice_hdr.num_ref_idx_l1_active;
    match mb_type {
        MBType::Intra4x4 => {
            for &(x, y) in I4X4_SCAN.iter() {
                let x = x as usize;
                let y = y as usize;
                let top_pred  = sstate.get_top_blk4(x + y * 4).ipred;
                let left_pred = sstate.get_left_blk4(x + y * 4).ipred;

                let top_idx = top_pred.into_pred_idx();
                let left_idx = left_pred.into_pred_idx();
                let pred_mode = top_idx.min(left_idx);
                let mut pred_mode = if pred_mode != -1 { pred_mode as u8 } else { 2 };
                if !br.read_bool()? {
                    let new_mode                    = br.read(3)? as u8;
                    pred_mode = if new_mode >= pred_mode {
                            new_mode + 1
                        } else { new_mode };
                }
                mb_info.ipred[x + y * 4] = pred_mode.into();
                sstate.get_cur_blk4(x + y * 4).ipred = pred_mode.into();
            }
            mb_info.chroma_ipred                    = br.read_ue_lim(3)? as u8;
        },
        MBType::Intra8x8 => {
            for part in 0..4 {
                let blk4 = (part & 1) * 2 + (part & 2) * 4;
                let top_pred  = sstate.get_top_blk4(blk4).ipred;
                let left_pred = sstate.get_left_blk4(blk4).ipred;

                let top_idx = top_pred.into_pred_idx();
                let left_idx = left_pred.into_pred_idx();
                let pred_mode = top_idx.min(left_idx);
                let mut pred_mode = if pred_mode != -1 { pred_mode as u8 } else { 2 };
                if !br.read_bool()? {
                    let new_mode                    = br.read(3)? as u8;
                    pred_mode = if new_mode >= pred_mode {
                            new_mode + 1
                        } else { new_mode };
                }
                mb_info.ipred[blk4]     = pred_mode.into();
                mb_info.ipred[blk4 + 1] = pred_mode.into();
                mb_info.ipred[blk4 + 4] = pred_mode.into();
                mb_info.ipred[blk4 + 5] = pred_mode.into();
                sstate.get_cur_blk4(blk4).ipred = pred_mode.into();
                sstate.get_cur_blk4(blk4 + 1).ipred = pred_mode.into();
                sstate.get_cur_blk4(blk4 + 4).ipred = pred_mode.into();
                sstate.get_cur_blk4(blk4 + 5).ipred = pred_mode.into();
            }
            mb_info.chroma_ipred                    = br.read_ue_lim(3)? as u8;
        },
        MBType::Intra16x16(_ipred, _, _) => {
            sstate.fill_ipred(IntraPredMode::DC);
            mb_info.chroma_ipred                    = br.read_ue_lim(3)? as u8;
        },
        MBType::P16x16 | MBType::P16x8 | MBType::P8x16 => {
            let nparts = mb_type.num_parts();
            read_refs(br, &mut mb_info.ref_l0[..nparts], num_l0)?;
            read_mvs(br, &mut mb_info.mv_l0[..nparts])?;
        },
        MBType::B16x16(mode) => {
            if mode != BMode::L1 {
                read_refs(br, &mut mb_info.ref_l0[..1], num_l0)?;
            }
            if mode != BMode::L0 {
                read_refs(br, &mut mb_info.ref_l1[..1], num_l1)?;
            }
            if mode != BMode::L1 {
                read_mvs(br, &mut mb_info.mv_l0[..1])?;
            }
            if mode != BMode::L0 {
                read_mvs(br, &mut mb_info.mv_l1[..1])?;
            }
        },
        MBType::B16x8(mode0, mode1) | MBType::B8x16(mode0, mode1) => {
            if num_l0 > 1 {
                if mode0 != BMode::L1 {
                    read_refs(br, &mut mb_info.ref_l0[0..1], num_l0)?;
                }
                if mode1 != BMode::L1 {
                    read_refs(br, &mut mb_info.ref_l0[1..2], num_l0)?;
                }
            }
            if num_l1 > 1 {
                if mode0 != BMode::L0 {
                    read_refs(br, &mut mb_info.ref_l1[0..1], num_l1)?;
                }
                if mode1 != BMode::L0 {
                    read_refs(br, &mut mb_info.ref_l1[1..2], num_l1)?;
                }
            }
            if mode0 != BMode::L1 {
                read_mvs(br, &mut mb_info.mv_l0[0..1])?;
            }
            if mode1 != BMode::L1 {
                read_mvs(br, &mut mb_info.mv_l0[1..2])?;
            }
            if mode0 != BMode::L0 {
                read_mvs(br, &mut mb_info.mv_l1[0..1])?;
            }
            if mode1 != BMode::L0 {
                read_mvs(br, &mut mb_info.mv_l1[1..2])?;
            }
        },
        MBType::P8x8 | MBType::P8x8Ref0 | MBType::B8x8 => {
            for sub_mb in mb_info.sub_mb_type.iter_mut() {
                *sub_mb = decode_sub_mb_type(br, mb_type != MBType::B8x8)?;
            }
            for (part, &sub_mb) in mb_info.sub_mb_type.iter().enumerate() {
                if num_l0 > 1 && mb_type != MBType::P8x8Ref0 && sub_mb != SubMBType::Direct8x8 && !sub_mb.is_l1() {
                    read_refs(br, &mut mb_info.ref_l0[part..][..1], num_l0)?;
                }
            }
            for (part, &sub_mb) in mb_info.sub_mb_type.iter().enumerate() {
                if num_l1 > 1 && sub_mb != SubMBType::Direct8x8 && !sub_mb.is_l0() {
                    read_refs(br, &mut mb_info.ref_l1[part..][..1], num_l1)?;
                }
            }
            for (part, &sub_mb) in mb_info.sub_mb_type.iter().enumerate() {
                if sub_mb != SubMBType::Direct8x8 && !sub_mb.is_l1() {
                    let num_subparts = sub_mb.num_parts();
                    read_mvs(br, &mut mb_info.mv_l0[part * 4..][..num_subparts])?;
                }
            }
            for (part, &sub_mb) in mb_info.sub_mb_type.iter().enumerate() {
                if sub_mb != SubMBType::Direct8x8 && !sub_mb.is_l0() {
                    let num_subparts = sub_mb.num_parts();
                    read_mvs(br, &mut mb_info.mv_l1[part * 4..][..num_subparts])?;
                }
            }
        },
        _ => {},
    };
    Ok(())
}

fn decode_sub_mb_type(br: &mut BitReader, is_p: bool) -> DecoderResult<SubMBType> {
    const SUB_MB_P_TYPES: [SubMBType; 4] = [
        SubMBType::P8x8, SubMBType::P8x4, SubMBType::P4x8, SubMBType::P4x4
    ];
    const SUB_MB_B_TYPES: [SubMBType; 13] = [
        SubMBType::Direct8x8,
        SubMBType::B8x8(BMode::L0), SubMBType::B8x8(BMode::L1), SubMBType::B8x8(BMode::Bi),
        SubMBType::B8x4(BMode::L0), SubMBType::B4x8(BMode::L0),
        SubMBType::B8x4(BMode::L1), SubMBType::B4x8(BMode::L1),
        SubMBType::B8x4(BMode::Bi), SubMBType::B4x8(BMode::Bi),
        SubMBType::B4x4(BMode::L0), SubMBType::B4x4(BMode::L1), SubMBType::B4x4(BMode::Bi),
    ];
    if is_p {
        let idx                                     = br.read_ue_lim(SUB_MB_P_TYPES.len() as u32 - 1)? as usize;
        Ok(SUB_MB_P_TYPES[idx])
    } else {
        let idx                                     = br.read_ue_lim(SUB_MB_B_TYPES.len() as u32 - 1)? as usize;
        Ok(SUB_MB_B_TYPES[idx])
    }
}

fn map_coeff_token(val: u8) -> (usize, usize) {
    const TRAILING_ONES: [u8; 6] = [ 0, 0, 1, 0, 1, 2 ];
    const TOTAL_COEFF: [u8; 6] = [0, 1, 1, 2, 2, 2];

    if val < 6 {
        (TRAILING_ONES[val as usize] as usize, TOTAL_COEFF[val as usize] as usize)
    } else {
        (((val - 6) & 3) as usize, ((val + 6) >> 2) as usize)
    }
}

fn decode_coeffs(br: &mut BitReader, coeffs: &mut [i16], scan: &[usize], cb: &Codebook<u8>, tables: &CAVLCTables) -> DecoderResult<u8> {
    let coeff_token                                 = br.read_cb(cb)?;
    let (trail_ones, total_coeff) = map_coeff_token(coeff_token);
    let mut level = [0i16; 16];
    let mut run = [0u8; 16];
    if total_coeff > 0 {
        let mut suffix_length = (total_coeff > 10 && trail_ones < 3) as u8;
        for i in 0..total_coeff {
            if i < trail_ones {
                if !br.read_bool()? {
                    level[i] = 1;
                } else {
                    level[i] = -1;
                }
            } else {
                let level_prefix                    = br.read_code(UintCodeType::UnaryZeroes)?;
                validate!(level_prefix <= 19);
                let mut level_code = level_prefix.min(15) << suffix_length;
                if suffix_length > 0 || level_prefix >= 14 {
                    let level_suffix_size = if level_prefix == 14 && suffix_length == 0 {
                            4
                        } else if level_prefix >= 15 {
                            (level_prefix - 3) as u8
                        } else {
                            suffix_length
                        };
                    let level_suffix                = br.read(level_suffix_size)?;
                    level_code += level_suffix;
                }
                if level_prefix >= 15 && suffix_length == 0 {
                    level_code += 15;
                }
                if level_prefix >= 16 {
                    level_code += (1 << (level_prefix - 3)) - 4096;
                }
                if i == trail_ones && trail_ones < 3 {
                    level_code += 2;
                }
                level[i] = if (level_code & 1) == 0 {
                        (level_code as i32 + 2) >> 1
                    } else {
                        -((level_code as i32 + 1) >> 1)
                    } as i16;
                if suffix_length == 0 {
                    suffix_length = 1;
                }
                if level[i].abs() > (3 << (suffix_length - 1)) && suffix_length < 6 {
                    suffix_length += 1;
                }
            }
        }
        let mut zeros_left = if total_coeff < coeffs.len() {
                let cb = if coeffs.len() > 4 {
                        &tables.total_zeros_cb[total_coeff - 1]
                    } else {
                        &tables.cdc_total_zeros_cb[total_coeff - 1]
                    };
                                                      br.read_cb(cb)?
            } else { 0 };
        for i in 0..total_coeff - 1 {
            if zeros_left > 0 {
                let run_before                      = br.read_cb(&tables.run_before_cb[(zeros_left - 1).min(6) as usize])?;
                run[i] = run_before;
                zeros_left -= run_before;
            }
        }
        run[total_coeff - 1] = zeros_left;
        let mut idx = 0;
        for i in (0..total_coeff).rev() {
            idx += run[i] as usize;
            coeffs[scan[idx]] = level[i];
            idx += 1;
        }
    }
    Ok(total_coeff as u8)
}

fn decode_block(br: &mut BitReader, coeffs: &mut [i16; 16], cb: &Codebook<u8>, tables: &CAVLCTables) -> DecoderResult<u8> {
    decode_coeffs(br, coeffs, &ZIGZAG, cb, tables)
}

fn decode_block_ac(br: &mut BitReader, coeffs: &mut [i16; 16], cb: &Codebook<u8>, tables: &CAVLCTables) -> DecoderResult<u8> {
    decode_coeffs(br, &mut coeffs[1..], &ZIGZAG1, cb, tables)
}

fn decode_chroma_dc(br: &mut BitReader, coeffs: &mut [i16; 4], cb: &Codebook<u8>, tables: &CAVLCTables) -> DecoderResult<u8> {
    decode_coeffs(br, coeffs, &CHROMA_DC_SCAN, cb, tables)
}

fn get_cb_idx(nc: u8) -> usize {
    match nc {
        0 | 1 => 0,
        2 | 3 => 1,
        4..=7 => 2,
        _     => 3,
    }
}

pub fn decode_residual_cavlc(br: &mut BitReader, sstate: &mut SliceState, mb_info: &mut CurrentMBInfo, tables: &CAVLCTables) -> DecoderResult<()> {
    if mb_info.mb_type.is_intra16x16() {
        let mut top_nc  = sstate.get_top_blk4(0).ncoded;
        let mut left_nc = sstate.get_left_blk4(0).ncoded;
        if !sstate.has_left {
            left_nc = top_nc;
        } else if !sstate.has_top {
            top_nc = left_nc;
        }
        let cb_idx = get_cb_idx((left_nc + top_nc + 1) >> 1);

        let nc = decode_block(br, &mut mb_info.coeffs[24], &tables.coeff_token_cb[cb_idx], tables)?;
        mb_info.coded[24] = nc != 0;
    }
    for blk8 in 0..4 {
        if (mb_info.cbpy & (1 << blk8)) != 0 {
            for blk4 in 0..4 {
                let bx =  (blk8 & 1) * 2 + (blk4 & 1);
                let by = ((blk8 & 2) * 2 + (blk4 & 2)) >> 1;
                let blk_no = bx + by * 4;

                let mut top_nc  = sstate.get_top_blk4(blk_no).ncoded;
                let mut left_nc = sstate.get_left_blk4(blk_no).ncoded;
                if bx == 0 && !sstate.has_left {
                    left_nc = top_nc;
                } else if by == 0 && !sstate.has_top {
                    top_nc = left_nc;
                }
                let cb_idx = get_cb_idx((left_nc + top_nc + 1) >> 1);

                let nc = if mb_info.mb_type.is_intra16x16() {
                        decode_block_ac(br, &mut mb_info.coeffs[blk_no], &tables.coeff_token_cb[cb_idx], tables)?
                    } else {
                        decode_block(br, &mut mb_info.coeffs[blk_no], &tables.coeff_token_cb[cb_idx], tables)?
                    };
                sstate.get_cur_blk4(blk_no).ncoded = nc;
                mb_info.coded[blk_no] = nc != 0;
            }
        }
    }
    if mb_info.transform_size_8x8 {
        for y in 0..2 {
            for x in 0..2 {
                let b0 = &mb_info.coeffs[x     + y * 8];
                let b1 = &mb_info.coeffs[x + 1 + y * 8];
                let b2 = &mb_info.coeffs[x + 4 + y * 8];
                let b3 = &mb_info.coeffs[x + 5 + y * 8];
                let dst = &mut mb_info.coeffs8x8[x + y * 2].coeffs;
                for (dst, (s0, s1)) in dst.chunks_mut(8).zip(b0.chunks(4).zip(b1.chunks(4))) {
                    let (d0, d1) = dst.split_at_mut(4);
                    d0.copy_from_slice(s0);
                    d1.copy_from_slice(s1);
                }
                for (dst, (s0, s1)) in dst.chunks_mut(8).skip(4).zip(b2.chunks(4).zip(b3.chunks(4))) {
                    let (d0, d1) = dst.split_at_mut(4);
                    d0.copy_from_slice(s0);
                    d1.copy_from_slice(s1);
                }
            }
        }
    }
    for chroma in 0..2 {
        if (mb_info.cbpc & 3) != 0 {
            decode_chroma_dc(br, &mut mb_info.chroma_dc[chroma], &tables.cdc_coeff_token_cb, tables)?;
        }
    }
    for chroma in 0..2 {
        if (mb_info.cbpc & 2) != 0 {
            for blk4 in 0..4 {
                let blk_no = 16 + chroma * 4 + blk4;
                let bx = blk4 & 1;
                let by = blk4 >> 1;

                let mut top_nc  = sstate.get_top_blk8(blk4).ncoded_c[chroma];
                let mut left_nc = sstate.get_left_blk8(blk4).ncoded_c[chroma];
                if bx == 0 && !sstate.has_left {
                    left_nc = top_nc;
                } else if by == 0 && !sstate.has_top {
                    top_nc = left_nc;
                }
                let cb_idx = get_cb_idx((left_nc + top_nc + 1) >> 1);

                let nc = decode_block_ac(br, &mut mb_info.coeffs[blk_no], &tables.coeff_token_cb[cb_idx], tables)?;
                sstate.get_cur_blk8(blk4).ncoded_c[chroma] = nc;
                mb_info.coded[blk_no] = nc != 0;
            }
        }
    }

    Ok(())
}

pub struct CAVLCTables {
    coeff_token_cb:     [Codebook<u8>; 4],
    cdc_coeff_token_cb: Codebook<u8>,
    total_zeros_cb:     [Codebook<u8>; 15],
    cdc_total_zeros_cb: [Codebook<u8>; 3],
    run_before_cb:      [Codebook<u8>; 7],
}

fn map_idx(idx: usize) -> u8 { idx as u8 }

macro_rules! create_cb {
    ($bits: expr, $lens: expr) => {{
        let mut reader = TableCodebookDescReader::new($bits, $lens, map_idx);
        Codebook::new(&mut reader, CodebookMode::MSB).unwrap()
    }}
}

impl CAVLCTables {
    pub fn new() -> Self {
        /*let mut reader = TableCodebookDescReader::new(&COEFF_TOKEN_BITS[0], &COEFF_TOKEN_LENS[0], map_idx);
        let coef_tok_cb0 = Codebook::new(&mut reader, CodebookMode::MSB).unwrap();
        let mut reader = TableCodebookDescReader::new(&COEFF_TOKEN_BITS[1], &COEFF_TOKEN_LENS[1], map_idx);
        let coef_tok_cb1 = Codebook::new(&mut reader, CodebookMode::MSB).unwrap();
        let mut reader = TableCodebookDescReader::new(&COEFF_TOKEN_BITS[2], &COEFF_TOKEN_LENS[2], map_idx);
        let coef_tok_cb2 = Codebook::new(&mut reader, CodebookMode::MSB).unwrap();
        let mut reader = TableCodebookDescReader::new(&COEFF_TOKEN_BITS[3], &COEFF_TOKEN_LENS[3], map_idx);
        let coef_tok_cb3 = Codebook::new(&mut reader, CodebookMode::MSB).unwrap();

        let mut reader = TableCodebookDescReader::new(&CHROMA_DC_COEFF_TOKEN_BITS, &CHROMA_DC_COEFF_TOKEN_LENS, map_idx);
        let cdc_coeff_token_cb = Codebook::new(&mut reader, CodebookMode::MSB).unwrap();*/

        let coef_tok_cb0 = create_cb!(&COEFF_TOKEN_BITS[0], &COEFF_TOKEN_LENS[0]);
        let coef_tok_cb1 = create_cb!(&COEFF_TOKEN_BITS[1], &COEFF_TOKEN_LENS[1]);
        let coef_tok_cb2 = create_cb!(&COEFF_TOKEN_BITS[2], &COEFF_TOKEN_LENS[2]);
        let coef_tok_cb3 = create_cb!(&COEFF_TOKEN_BITS[3], &COEFF_TOKEN_LENS[3]);

        let cdc_coeff_token_cb = create_cb!(&CHROMA_DC_COEFF_TOKEN_BITS, &CHROMA_DC_COEFF_TOKEN_LENS);

        let total_zeros0  = create_cb!(&TOTAL_ZERO_BITS[ 0], &TOTAL_ZERO_LENS[ 0]);
        let total_zeros1  = create_cb!(&TOTAL_ZERO_BITS[ 1], &TOTAL_ZERO_LENS[ 1]);
        let total_zeros2  = create_cb!(&TOTAL_ZERO_BITS[ 2], &TOTAL_ZERO_LENS[ 2]);
        let total_zeros3  = create_cb!(&TOTAL_ZERO_BITS[ 3], &TOTAL_ZERO_LENS[ 3]);
        let total_zeros4  = create_cb!(&TOTAL_ZERO_BITS[ 4], &TOTAL_ZERO_LENS[ 4]);
        let total_zeros5  = create_cb!(&TOTAL_ZERO_BITS[ 5], &TOTAL_ZERO_LENS[ 5]);
        let total_zeros6  = create_cb!(&TOTAL_ZERO_BITS[ 6], &TOTAL_ZERO_LENS[ 6]);
        let total_zeros7  = create_cb!(&TOTAL_ZERO_BITS[ 7], &TOTAL_ZERO_LENS[ 7]);
        let total_zeros8  = create_cb!(&TOTAL_ZERO_BITS[ 8], &TOTAL_ZERO_LENS[ 8]);
        let total_zeros9  = create_cb!(&TOTAL_ZERO_BITS[ 9], &TOTAL_ZERO_LENS[ 9]);
        let total_zeros10 = create_cb!(&TOTAL_ZERO_BITS[10], &TOTAL_ZERO_LENS[10]);
        let total_zeros11 = create_cb!(&TOTAL_ZERO_BITS[11], &TOTAL_ZERO_LENS[11]);
        let total_zeros12 = create_cb!(&TOTAL_ZERO_BITS[12], &TOTAL_ZERO_LENS[12]);
        let total_zeros13 = create_cb!(&TOTAL_ZERO_BITS[13], &TOTAL_ZERO_LENS[13]);
        let total_zeros14 = create_cb!(&TOTAL_ZERO_BITS[14], &TOTAL_ZERO_LENS[14]);

        let cdc_total_zeros_cb0 = create_cb!(&CHROMA_DC_TOTAL_ZERO_BITS[0], &CHROMA_DC_TOTAL_ZERO_LENS[0]);
        let cdc_total_zeros_cb1 = create_cb!(&CHROMA_DC_TOTAL_ZERO_BITS[1], &CHROMA_DC_TOTAL_ZERO_LENS[1]);
        let cdc_total_zeros_cb2 = create_cb!(&CHROMA_DC_TOTAL_ZERO_BITS[2], &CHROMA_DC_TOTAL_ZERO_LENS[2]);

        let run_before_cb0 = create_cb!(&RUN_BEFORE_BITS[0], &RUN_BEFORE_LENS[0]);
        let run_before_cb1 = create_cb!(&RUN_BEFORE_BITS[1], &RUN_BEFORE_LENS[1]);
        let run_before_cb2 = create_cb!(&RUN_BEFORE_BITS[2], &RUN_BEFORE_LENS[2]);
        let run_before_cb3 = create_cb!(&RUN_BEFORE_BITS[3], &RUN_BEFORE_LENS[3]);
        let run_before_cb4 = create_cb!(&RUN_BEFORE_BITS[4], &RUN_BEFORE_LENS[4]);
        let run_before_cb5 = create_cb!(&RUN_BEFORE_BITS[5], &RUN_BEFORE_LENS[5]);
        let run_before_cb6 = create_cb!(&RUN_BEFORE_BITS[6], &RUN_BEFORE_LENS[6]);

        Self {
            coeff_token_cb: [coef_tok_cb0, coef_tok_cb1, coef_tok_cb2, coef_tok_cb3],
            cdc_coeff_token_cb,
            total_zeros_cb: [total_zeros0,  total_zeros1,  total_zeros2,
                             total_zeros3,  total_zeros4,  total_zeros5,
                             total_zeros6,  total_zeros7,  total_zeros8,
                             total_zeros9,  total_zeros10, total_zeros11,
                             total_zeros12, total_zeros13, total_zeros14 ],
            cdc_total_zeros_cb: [cdc_total_zeros_cb0, cdc_total_zeros_cb1, cdc_total_zeros_cb2],
            run_before_cb:  [ run_before_cb0, run_before_cb1, run_before_cb2,
                              run_before_cb3, run_before_cb4, run_before_cb5,
                              run_before_cb6 ],
        }
    }
}

const COEFF_TOKEN_BITS: [[u16; 62]; 4] = [
  [
    0x01, 0x05, 0x01, 0x07, 0x04, 0x01, 0x07, 0x06,
    0x05, 0x03, 0x07, 0x06, 0x05, 0x03, 0x07, 0x06,
    0x05, 0x04, 0x0F, 0x06, 0x05, 0x04, 0x0B, 0x0E,
    0x05, 0x04, 0x08, 0x0A, 0x0D, 0x04, 0x0F, 0x0E,
    0x09, 0x04, 0x0B, 0x0A, 0x0D, 0x0C, 0x0F, 0x0E,
    0x09, 0x0C, 0x0B, 0x0A, 0x0D, 0x08, 0x0F, 0x01,
    0x09, 0x0C, 0x0B, 0x0E, 0x0D, 0x08, 0x07, 0x0A,
    0x09, 0x0C, 0x04, 0x06, 0x05, 0x08
  ], [
    0x03, 0x0B, 0x02, 0x07, 0x07, 0x03, 0x07, 0x0A,
    0x09, 0x05, 0x07, 0x06, 0x05, 0x04, 0x04, 0x06,
    0x05, 0x06, 0x07, 0x06, 0x05, 0x08, 0x0F, 0x06,
    0x05, 0x04, 0x0B, 0x0E, 0x0D, 0x04, 0x0F, 0x0A,
    0x09, 0x04, 0x0B, 0x0E, 0x0D, 0x0C, 0x08, 0x0A,
    0x09, 0x08, 0x0F, 0x0E, 0x0D, 0x0C, 0x0B, 0x0A,
    0x09, 0x0C, 0x07, 0x0B, 0x06, 0x08, 0x09, 0x08,
    0x0A, 0x01, 0x07, 0x06, 0x05, 0x04
  ], [
    0x0F, 0x0F, 0x0E, 0x0B, 0x0F, 0x0D, 0x08, 0x0C,
    0x0E, 0x0C, 0x0F, 0x0A, 0x0B, 0x0B, 0x0B, 0x08,
    0x09, 0x0A, 0x09, 0x0E, 0x0D, 0x09, 0x08, 0x0A,
    0x09, 0x08, 0x0F, 0x0E, 0x0D, 0x0D, 0x0B, 0x0E,
    0x0A, 0x0C, 0x0F, 0x0A, 0x0D, 0x0C, 0x0B, 0x0E,
    0x09, 0x0C, 0x08, 0x0A, 0x0D, 0x08, 0x0D, 0x07,
    0x09, 0x0C, 0x09, 0x0C, 0x0B, 0x0A, 0x05, 0x08,
    0x07, 0x06, 0x01, 0x04, 0x03, 0x02
  ], [
    0x03, 0x00, 0x01, 0x04, 0x05, 0x06, 0x08, 0x09,
    0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11,
    0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
    0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21,
    0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29,
    0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
    0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
    0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F
  ]
];
const COEFF_TOKEN_LENS: [[u8; 62]; 4] = [
  [
     1,  6,  2,  8,  6,  3,  9,  8,  7,  5, 10,  9,  8,  6, 11, 10,
     9,  7, 13, 11, 10,  8, 13, 13, 11,  9, 13, 13, 13, 10, 14, 14,
    13, 11, 14, 14, 14, 13, 15, 15, 14, 14, 15, 15, 15, 14, 16, 15,
    15, 15, 16, 16, 16, 15, 16, 16, 16, 16, 16, 16, 16, 16
  ], [
     2,  6,  2,  6,  5,  3,  7,  6,  6,  4,  8,  6,  6,  4,  8,  7,
     7,  5,  9,  8,  8,  6, 11,  9,  9,  6, 11, 11, 11,  7, 12, 11,
    11,  9, 12, 12, 12, 11, 12, 12, 12, 11, 13, 13, 13, 12, 13, 13,
    13, 13, 13, 14, 13, 13, 14, 14, 14, 13, 14, 14, 14, 14
  ], [
     4,  6,  4,  6,  5,  4,  6,  5,  5,  4,  7,  5,  5,  4,  7,  5,
     5,  4,  7,  6,  6,  4,  7,  6,  6,  4,  8,  7,  7,  5,  8,  8,
     7,  6,  9,  8,  8,  7,  9,  9,  8,  8,  9,  9,  9,  8, 10,  9,
     9,  9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
  ], [ 6; 62 ]
];

const CHROMA_DC_COEFF_TOKEN_BITS: [u8; 14] = [
    1, 7, 1, 4, 6, 1, 3, 3, 2, 5, 2, 3, 2, 0
];
const CHROMA_DC_COEFF_TOKEN_LENS: [u8; 14] = [
    2, 6, 1, 6, 6, 3, 6, 7, 7, 6, 6, 8, 8, 7
];

const TOTAL_ZERO_BITS: [[u8; 16]; 15] = [
    [ 1, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 1 ],
    [ 7, 6, 5, 4, 3, 5, 4, 3, 2, 3, 2, 3, 2, 1, 0, 0 ],
    [ 5, 7, 6, 5, 4, 3, 4, 3, 2, 3, 2, 1, 1, 0, 0, 0 ],
    [ 3, 7, 5, 4, 6, 5, 4, 3, 3, 2, 2, 1, 0, 0, 0, 0 ],
    [ 5, 4, 3, 7, 6, 5, 4, 3, 2, 1, 1, 0, 0, 0, 0, 0 ],
    [ 1, 1, 7, 6, 5, 4, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0 ],
    [ 1, 1, 5, 4, 3, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0 ],
    [ 1, 1, 1, 3, 3, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 1, 0, 1, 3, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 1, 0, 1, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 1, 1, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
];
const TOTAL_ZERO_LENS: [[u8; 16]; 15] = [
    [ 1, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 9 ],
    [ 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 6, 6, 6, 6, 0 ],
    [ 4, 3, 3, 3, 4, 4, 3, 3, 4, 5, 5, 6, 5, 6, 0, 0 ],
    [ 5, 3, 4, 4, 3, 3, 3, 4, 3, 4, 5, 5, 5, 0, 0, 0 ],
    [ 4, 4, 4, 3, 3, 3, 3, 3, 4, 5, 4, 5, 0, 0, 0, 0 ],
    [ 6, 5, 3, 3, 3, 3, 3, 3, 4, 3, 6, 0, 0, 0, 0, 0 ],
    [ 6, 5, 3, 3, 3, 2, 3, 4, 3, 6, 0, 0, 0, 0, 0, 0 ],
    [ 6, 4, 5, 3, 2, 2, 3, 3, 6, 0, 0, 0, 0, 0, 0, 0 ],
    [ 6, 6, 4, 2, 2, 3, 2, 5, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 5, 5, 3, 2, 2, 2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 4, 4, 3, 3, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 4, 4, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 3, 3, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
];

const CHROMA_DC_TOTAL_ZERO_BITS: [[u8; 4]; 3] = [
    [ 1, 1, 1, 0 ], [ 1, 1, 0, 0 ], [ 1, 0, 0, 0 ]
];
const CHROMA_DC_TOTAL_ZERO_LENS: [[u8; 4]; 3] = [
    [ 1, 2, 3, 3 ], [ 1, 2, 2, 0 ], [ 1, 1, 0, 0 ]
];

const RUN_BEFORE_BITS: [[u8; 15]; 7] = [
    [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 3, 2, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 3, 0, 1, 3, 2, 5, 4, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
];
const RUN_BEFORE_LENS: [[u8; 15]; 7] = [
    [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0 ],
    [ 1, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0 ],
    [ 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0 ],
    [ 2, 2, 2, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0 ],
    [ 2, 2, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0,  0,  0 ],
    [ 2, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0,  0,  0 ],
    [ 3, 3, 3, 3, 3, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]
];
