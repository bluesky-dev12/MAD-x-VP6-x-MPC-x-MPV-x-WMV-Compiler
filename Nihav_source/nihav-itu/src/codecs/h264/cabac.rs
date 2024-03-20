//use nihav_core::codecs::{DecoderResult, DecoderError};

use super::*;
use super::cabac_coder::*;
use super::dsp::{CHROMA_DC_SCAN, ZIGZAG, ZIGZAG1, ZIGZAG8X8};
use super::slice::SliceHeader;

pub fn cabac_decode_mbskip(cabac: &mut CABAC, sstate: &SliceState, slice_hdr: &SliceHeader) -> bool {
    let skip_idx = if slice_hdr.slice_type.is_p() { 11 } else { 24 };
    let mut mb_skip_ctx = 0;
    let left_mbt = sstate.get_left_mb().mb_type;
    let top_mbt  = sstate.get_top_mb().mb_type;
    if left_mbt != CompactMBType::None && !left_mbt.is_skip() {
        mb_skip_ctx += 1;
    }
    if top_mbt != CompactMBType::None && !top_mbt.is_skip() {
        mb_skip_ctx += 1;
    }
    if !slice_hdr.slice_type.is_intra() {
        cabac.decode_bit(skip_idx + mb_skip_ctx)
    } else {
        false
    }
}

fn decode_i_type(cabac: &mut CABAC, start: usize, ctx: usize) -> MBType {
    if !cabac.decode_bit(start + ctx) {
        MBType::Intra4x4
    } else if !cabac.decode_terminate() {
        let cbpy = if cabac.decode_bit(start + 3) { 0xF } else { 0x0 };
        let cbpc = cabac.decode_012(start + 4);
        let ipred = cabac.decode_bits(start + 6, start + 7, 2);

        MBType::Intra16x16(ipred, cbpy, cbpc)
    } else {
        MBType::PCM
    }
}

fn decode_i_type_inter(cabac: &mut CABAC, start: usize) -> MBType {
    if !cabac.decode_bit(start) {
        MBType::Intra4x4
    } else if !cabac.decode_terminate() {
        let cbpy = if cabac.decode_bit(start + 1) { 0xF } else { 0x0 };
        let cbpc = if !cabac.decode_bit(start + 2) {
                0
            } else if !cabac.decode_bit(start + 2) {
                1
            } else {
                2
            };
        let ipred = cabac.decode_bits(start + 3, start + 3, 2);

        MBType::Intra16x16(ipred, cbpy, cbpc)
    } else {
        MBType::PCM
    }
}

fn remap_si_mbtype(mbtype: MBType) -> MBType {
    match mbtype {
        MBType::Intra16x16(0, 0, 0) => MBType::Intra4x4,
        MBType::Intra16x16(imode, cbpy, cbpc) => {
            let idx = imode + if cbpy != 0 { 12 } else { 0 } + cbpc * 4 - 1;
            let nimode = idx & 3;
            let (ncbpy, ncbpc) = if (idx >> 2) >= 3 {
                    (0xF, (idx >> 2) - 3)
                } else {
                    (0x0, idx >> 2)
                };
            MBType::Intra16x16(nimode, ncbpy, ncbpc)
        },
        MBType::PCM => MBType::Intra16x16(3, 1, 2),
        _ => mbtype,
    }
}

pub fn cabac_decode_mb_type(cabac: &mut CABAC, slice_hdr: &SliceHeader, sstate: &SliceState) -> MBType {
    match slice_hdr.slice_type {
        SliceType::I | SliceType::SI => {
            let mut ctx = 0;
            if sstate.get_left_mb().mb_type.is_intra16orpcm() {
                ctx += 1;
            }
            if sstate.get_top_mb().mb_type.is_intra16orpcm() {
                ctx += 1;
            }
            let mbtype = decode_i_type(cabac, 3, ctx);
            if slice_hdr.slice_type == SliceType::I {
                mbtype
            } else {
                remap_si_mbtype(mbtype)
            }
        },
        SliceType::P | SliceType::SP => {
            if cabac.decode_bit(14) {
                decode_i_type_inter(cabac, 17)
            } else if !cabac.decode_bit(15) {
                if !cabac.decode_bit(16) {
                    MBType::P16x16
                } else {
                    MBType::P8x8
                }
            } else {
                if !cabac.decode_bit(17) {
                    MBType::P8x16
                } else {
                    MBType::P16x8
                }
            }
        },
        SliceType::B => {
            let mut ctx = 0;
            if !sstate.get_left_mb().mb_type.is_direct() {
                ctx += 1;
            }
            if !sstate.get_top_mb().mb_type.is_direct() {
                ctx += 1;
            }
            if !cabac.decode_bit(27 + ctx) {
                MBType::Direct
            } else if !cabac.decode_bit(30) {
                if !cabac.decode_bit(32) {
                    MBType::B16x16(BMode::L0)
                } else {
                    MBType::B16x16(BMode::L1)
                }
            } else {
                let idx = cabac.decode_bits(31, 32, 4);
                match idx {
                    0x0 => MBType::B16x16(BMode::Bi),
                    0x1 => MBType::B16x8(BMode::L0, BMode::L0),
                    0x2 => MBType::B8x16(BMode::L0, BMode::L0),
                    0x3 => MBType::B16x8(BMode::L1, BMode::L1),
                    0x4 => MBType::B8x16(BMode::L1, BMode::L1),
                    0x5 => MBType::B16x8(BMode::L0, BMode::L1),
                    0x6 => MBType::B8x16(BMode::L0, BMode::L1),
                    0x7 => MBType::B16x8(BMode::L1, BMode::L0),
                    0xE => MBType::B8x16(BMode::L1, BMode::L0),
                    0xF => MBType::B8x8,
                    0xD => decode_i_type_inter(cabac, 32),
                    _ => {
                        let idx = (idx - 8) * 2 + (cabac.decode_bit(32) as u8);
                        match idx {
                            0 => MBType::B16x8(BMode::L0, BMode::Bi),
                            1 => MBType::B8x16(BMode::L0, BMode::Bi),
                            2 => MBType::B16x8(BMode::L1, BMode::Bi),
                            3 => MBType::B8x16(BMode::L1, BMode::Bi),
                            4 => MBType::B16x8(BMode::Bi, BMode::L0),
                            5 => MBType::B8x16(BMode::Bi, BMode::L0),
                            6 => MBType::B16x8(BMode::Bi, BMode::L1),
                            7 => MBType::B8x16(BMode::Bi, BMode::L1),
                            8 => MBType::B16x8(BMode::Bi, BMode::Bi),
                            _ => MBType::B8x16(BMode::Bi, BMode::Bi),
                        }
                    },
                }
            }
        },
    }
}

fn decode_sub_mb_type_cabac(cabac: &mut CABAC, slice_hdr: &SliceHeader) -> SubMBType {
    match slice_hdr.slice_type {
        SliceType::P | SliceType::SP => {
            if cabac.decode_bit(21) {
                SubMBType::P8x8
            } else if !cabac.decode_bit(22) {
                SubMBType::P8x4
            } else if cabac.decode_bit(23) {
                SubMBType::P4x8
            } else {
                SubMBType::P4x4
            }
        },
        SliceType::B => {
            if !cabac.decode_bit(36) {
                SubMBType::Direct8x8
            } else if !cabac.decode_bit(37) {
                if !cabac.decode_bit(39) {
                    SubMBType::B8x8(BMode::L0)
                } else {
                    SubMBType::B8x8(BMode::L1)
                }
            } else {
                let idx = cabac.decode_bits(38, 39, 3);
                match idx {
                    0 => SubMBType::B8x8(BMode::Bi),
                    1 => SubMBType::B8x4(BMode::L0),
                    2 => SubMBType::B4x8(BMode::L0),
                    3 => SubMBType::B8x4(BMode::L1),
                    6 => SubMBType::B4x4(BMode::L1),
                    7 => SubMBType::B4x4(BMode::Bi),
                    _ => {
                        let idx = (idx - 4) * 2 + (cabac.decode_bit(39) as u8);
                        match idx {
                            0 => SubMBType::B4x8(BMode::L1),
                            1 => SubMBType::B8x4(BMode::Bi),
                            2 => SubMBType::B4x8(BMode::Bi),
                            _ => SubMBType::B4x4(BMode::L0),
                        }
                    },
                }
            }
        },
        _ => unreachable!(),
    }
}

fn decode_ref_idx(cabac: &mut CABAC, num_refs: usize, ctx: usize) -> PicRef {
    if num_refs == 1 {
        return ZERO_REF;
    }
    if !cabac.decode_bit(54 + ctx) {
        ZERO_REF
    } else if !cabac.decode_bit(54 + 4) {
        PicRef::new(1)
    } else {
        let mut idx = 2;
        while cabac.decode_bit(54 + 5) && idx < 32 {
            idx += 1;
        }
        if idx < num_refs {
            PicRef::new(idx as u8)
        } else {
            INVALID_REF
        }
    }
}

fn decode_mv_component(cabac: &mut CABAC, base: usize, ctx: usize) -> i16 {
    if !cabac.decode_bit(base + ctx) {
        0
    } else {
        let mut val = 1;
        while val < 9 && cabac.decode_bit(base + (2 + val).min(6)) {
            val += 1;
        }
        if val >= 9 {
            let mut pfx = 3;
            while pfx < 16 && cabac.decode_bypass() {
                val += 1 << pfx;
                pfx += 1;
            }
            val += cabac.decode_bypass_bits(pfx) as usize;
        }
        if val == 0 || !cabac.decode_bypass() {
            val as i16
        } else {
            -(val as i16)
        }
    }
}

fn decode_mv(cabac: &mut CABAC, ctx0: usize, ctx1: usize) -> MV {
    let x = decode_mv_component(cabac, 40, ctx0);
    let y = decode_mv_component(cabac, 47, ctx1);
    MV{ x, y }
}

#[allow(clippy::cognitive_complexity)]
pub fn decode_mb_pred_cabac(cabac: &mut CABAC, slice_hdr: &SliceHeader, mb_type: MBType, sstate: &mut SliceState, mb_info: &mut CurrentMBInfo) {
    mb_info.mb_type = mb_type;
    let num_l0 = slice_hdr.num_ref_idx_l0_active;
    let num_l1 = slice_hdr.num_ref_idx_l1_active;
    sstate.reset_mb_mv();
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

                if !cabac.decode_bit(68) {
                    let m0 = cabac.decode_bit(69) as u8;
                    let m1 = cabac.decode_bit(69) as u8;
                    let m2 = cabac.decode_bit(69) as u8;
                    let new_mode = (m2 << 2) | (m1 << 1) | m0;
                    pred_mode = if new_mode >= pred_mode {
                            new_mode + 1
                        } else { new_mode };
                }
                mb_info.ipred[x + y * 4] = pred_mode.into();
                sstate.get_cur_blk4(x + y * 4).ipred = pred_mode.into();
            }
            let mut ctx = 0;
            if sstate.get_left_mb().cmode != 0 {
                ctx += 1;
            }
            if sstate.get_top_mb().cmode != 0 {
                ctx += 1;
            }
            mb_info.chroma_ipred = if !cabac.decode_bit(64 + ctx) {
                    0
                } else if !cabac.decode_bit(67) {
                    1
                } else if !cabac.decode_bit(67) {
                    2
                } else {
                    3
                };
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
                if !cabac.decode_bit(68) {
                    let m0 = cabac.decode_bit(69) as u8;
                    let m1 = cabac.decode_bit(69) as u8;
                    let m2 = cabac.decode_bit(69) as u8;
                    let new_mode = (m2 << 2) | (m1 << 1) | m0;
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
            let mut ctx = 0;
            if sstate.get_left_mb().cmode != 0 {
                ctx += 1;
            }
            if sstate.get_top_mb().cmode != 0 {
                ctx += 1;
            }
            mb_info.chroma_ipred = if !cabac.decode_bit(64 + ctx) {
                    0
                } else if !cabac.decode_bit(67) {
                    1
                } else if !cabac.decode_bit(67) {
                    2
                } else {
                    3
                };
        },
        MBType::Intra16x16(_ipred, _, _) => {
            let mut ctx = 0;
            if sstate.get_left_mb().cmode != 0 {
                ctx += 1;
            }
            if sstate.get_top_mb().cmode != 0 {
                ctx += 1;
            }
            mb_info.chroma_ipred = if !cabac.decode_bit(64 + ctx) {
                    0
                } else if !cabac.decode_bit(67) {
                    1
                } else if !cabac.decode_bit(67) {
                    2
                } else {
                    3
                };
        },
        MBType::P16x16 | MBType::P16x8 | MBType::P8x16 => {
            let num_subparts = mb_type.num_parts();
            let (pw, ph) = mb_type.size();
            let mut xoff = 0;
            let mut yoff = 0;
            for i in 0..num_subparts {
                let ctx = sstate.get_mv_ref_ctx(xoff, yoff, 0);
                let ref_idx = decode_ref_idx(cabac, num_l0, ctx);
                mb_info.ref_l0[i] = ref_idx;
                sstate.fill_ref(xoff, yoff, pw, ph, 0, ref_idx);
                xoff += pw;
                if xoff == 16 {
                    xoff = 0;
                    yoff += ph;
                }
            }
            let mut xoff = 0;
            let mut yoff = 0;
            for i in 0..num_subparts {
                let (ctx0, ctx1) = sstate.get_mv_ctx(xoff, yoff, 0);
                let mv = decode_mv(cabac, ctx0, ctx1);
                mb_info.mv_l0[i] = mv;
                sstate.fill_mvd(xoff, yoff, pw, ph, 0, mv);
                xoff += pw;
                if xoff == 16 {
                    xoff = 0;
                    yoff += ph;
                }
            }
        },
        MBType::B16x16(mode) => {
            if mode != BMode::L1 {
                let ctx = sstate.get_mv_ref_ctx(0, 0, 0);
                let ref_idx = decode_ref_idx(cabac, num_l0, ctx);
                mb_info.ref_l0[0] = ref_idx;
                sstate.fill_ref(0, 0, 16, 16, 0, ref_idx);
            }
            if mode != BMode::L0 {
                let ctx = sstate.get_mv_ref_ctx(0, 0, 1);
                let ref_idx = decode_ref_idx(cabac, num_l1, ctx);
                mb_info.ref_l1[0] = ref_idx;
                sstate.fill_ref(0, 0, 16, 16, 1, ref_idx);
            }
            if mode != BMode::L1 {
                let (ctx0, ctx1) = sstate.get_mv_ctx(0, 0, 0);
                let mv = decode_mv(cabac, ctx0, ctx1);
                mb_info.mv_l0[0] = mv;
                sstate.fill_mvd(0, 0, 16, 16, 0, mv);
            }
            if mode != BMode::L0 {
                let (ctx0, ctx1) = sstate.get_mv_ctx(0, 0, 1);
                let mv = decode_mv(cabac, ctx0, ctx1);
                mb_info.mv_l1[0] = mv;
                sstate.fill_mvd(0, 0, 16, 16, 1, mv);
            }
        },
        MBType::B16x8(mode0, mode1) | MBType::B8x16(mode0, mode1) => {
            let (pw, ph) = mb_info.mb_type.size();
            let (px, py) = (pw & 8, ph & 8);
            if mode0 != BMode::L1 {
                let ctx = sstate.get_mv_ref_ctx(0, 0, 0);
                let ref_idx = decode_ref_idx(cabac, num_l0, ctx);
                mb_info.ref_l0[0] = ref_idx;
                sstate.fill_ref(0, 0, pw, ph, 0, ref_idx);
            }
            if mode1 != BMode::L1 {
                let ctx = sstate.get_mv_ref_ctx(pw & 8, ph & 8, 0);
                let ref_idx = decode_ref_idx(cabac, num_l0, ctx);
                mb_info.ref_l0[1] = ref_idx;
                sstate.fill_ref(px, py, pw, ph, 0, ref_idx);
            }
            if mode0 != BMode::L0 {
                let ctx = sstate.get_mv_ref_ctx(0, 0, 1);
                let ref_idx = decode_ref_idx(cabac, num_l1, ctx);
                mb_info.ref_l1[0] = ref_idx;
                sstate.fill_ref(0, 0, pw, ph, 1, ref_idx);
            }
            if mode1 != BMode::L0 {
                let ctx = sstate.get_mv_ref_ctx(pw & 8, ph & 8, 1);
                let ref_idx = decode_ref_idx(cabac, num_l1, ctx);
                mb_info.ref_l1[1] = ref_idx;
                sstate.fill_ref(px, py, pw, ph, 1, ref_idx);
            }
            if mode0 != BMode::L1 {
                let (ctx0, ctx1) = sstate.get_mv_ctx(0, 0, 0);
                let mv = decode_mv(cabac, ctx0, ctx1);
                mb_info.mv_l0[0] = mv;
                sstate.fill_mvd(0, 0, pw, ph, 0, mv);
            }
            if mode1 != BMode::L1 {
                let (ctx0, ctx1) = sstate.get_mv_ctx(pw & 8, ph & 8, 0);
                let mv = decode_mv(cabac, ctx0, ctx1);
                mb_info.mv_l0[1] = mv;
                sstate.fill_mvd(px, py, pw, ph, 0, mv);
            }
            if mode0 != BMode::L0 {
                let (ctx0, ctx1) = sstate.get_mv_ctx(0, 0, 1);
                let mv = decode_mv(cabac, ctx0, ctx1);
                mb_info.mv_l1[0] = mv;
                sstate.fill_mvd(0, 0, pw, ph, 1, mv);
            }
            if mode1 != BMode::L0 {
                let (ctx0, ctx1) = sstate.get_mv_ctx(pw & 8, ph & 8, 1);
                let mv = decode_mv(cabac, ctx0, ctx1);
                mb_info.mv_l1[1] = mv;
                sstate.fill_mvd(px, py, pw, ph, 1, mv);
            }
        },
        MBType::P8x8 | MBType::B8x8 => {
            for sub_type in mb_info.sub_mb_type.iter_mut() {
                *sub_type = decode_sub_mb_type_cabac(cabac, slice_hdr);
            }
            let num_l = [num_l0, num_l1];
            let dst_ref = [&mut mb_info.ref_l0, &mut mb_info.ref_l1];
            for ref_l in 0..2 {
                for spart in 0..4 {
                    let stype = mb_info.sub_mb_type[spart];
                    if stype != SubMBType::Direct8x8 && ((ref_l == 0 && !stype.is_l1()) || (ref_l == 1 && !stype.is_l0())) {
                        let ctx = sstate.get_mv_ref_ctx((spart & 1) * 8, (spart & 2) * 4, ref_l);
                        let ref_idx = decode_ref_idx(cabac, num_l[ref_l], ctx);
                        dst_ref[ref_l][spart] = ref_idx;
                        sstate.get_cur_blk8(spart).ref_idx[ref_l] = ref_idx;
                    }
                }
            }
            let dst_mv = [&mut mb_info.mv_l0, &mut mb_info.mv_l1];
            for ref_l in 0..2 {
                for spart in 0..4 {
                    let stype = mb_info.sub_mb_type[spart];
                    if stype == SubMBType::Direct8x8 || (ref_l == 0 && stype.is_l1()) || (ref_l == 1 && stype.is_l0()) {
                        continue;
                    }
                    let (pw, ph) = stype.size();
                    let mut xoff = (spart & 1) * 8;
                    let mut yoff = (spart & 2) * 4;
                    let num_sub = stype.num_parts();
                    let orig_x = xoff;
                    for i in 0..num_sub {
                        let (ctx0, ctx1) = sstate.get_mv_ctx(xoff, yoff, ref_l);
                        let mv = decode_mv(cabac, ctx0, ctx1);
                        dst_mv[ref_l][spart * 4 + i] = mv;
                        sstate.fill_mvd(xoff, yoff, pw, ph, ref_l, mv);
                        xoff += pw;
                        if xoff == orig_x + 8 {
                            xoff -= 8;
                            yoff += ph;
                        }
                    }
                }
            }
        },
        _ => {},
    };
}

pub fn decode_cbp_cabac(cabac: &mut CABAC, sstate: &SliceState) -> (u8, u8) {
    let mbt_a = sstate.get_left_mb().mb_type;
    let mbt_b = sstate.get_top_mb().mb_type;
    let left = if mbt_a == CompactMBType::None || mbt_a == CompactMBType::PCM {
            0x3F
        } else if !mbt_a.is_skip() {
            sstate.get_left_mb().cbp
        } else {
            0
        };
    let top = if mbt_b == CompactMBType::None || mbt_b == CompactMBType::PCM {
            0x3F
        } else if !mbt_b.is_skip() {
            sstate.get_top_mb().cbp
        } else {
            0
        };

    let cbp_ctx = if (left & 2) != 0 { 0 } else { 1 } + if (top & 4) != 0 { 0 } else { 2 };
    let mut cbpy = cabac.decode_bit(73 + cbp_ctx) as u8;
    let cbp_ctx = if cbpy != 0 { 0 } else { 1 } + if (top & 8) != 0 { 0 } else { 2 };
    cbpy |= (cabac.decode_bit(73 + cbp_ctx) as u8) << 1;
    let cbp_ctx = if (left & 8) != 0 { 0 } else { 1 } + if (cbpy & 1) != 0 { 0 } else { 2 };
    cbpy |= (cabac.decode_bit(73 + cbp_ctx) as u8) << 2;
    let cbp_ctx = if (cbpy & 4) != 0 { 0 } else { 1 } + if (cbpy & 2) != 0 { 0 } else { 2 };
    cbpy |= (cabac.decode_bit(73 + cbp_ctx) as u8) << 3;

    let left = if mbt_a == CompactMBType::PCM {
            0x2F
        } else if mbt_a == CompactMBType::None || !mbt_a.is_skip() {
            sstate.get_left_mb().cbp
        } else {
            0
        };
    let top = if mbt_b == CompactMBType::PCM {
            0x2F
        } else if mbt_b == CompactMBType::None || !mbt_b.is_skip() {
            sstate.get_top_mb().cbp
        } else {
            0
        };
    let cleft = left >> 4;
    let ctop  = top  >> 4;
    let cbp_ctx0 = if cleft != 0 { 1 } else { 0 } + if ctop != 0 { 2 } else { 0 };
    let cbp_ctx1 = if cleft == 2 { 1 } else { 0 } + if ctop == 2 { 2 } else { 0 };
    let cbpc = if !cabac.decode_bit(77 + cbp_ctx0) {
            0
        } else {
            cabac.decode_bit(81 + cbp_ctx1) as u8 + 1
        };

    (cbpy, cbpc)
}

pub fn decode_mb_qp_delta_cabac(cabac: &mut CABAC, ctx: usize) -> i32 {
    if !cabac.decode_bit(60 + ctx) {
        0
    } else if !cabac.decode_bit(62) {
        1
    } else {
        let mut val = 0;
        while val < 128 && cabac.decode_bit(63) {
            val += 1;
        }
        if (val & 1) != 0 {
            (val >> 1) + 2
        } else {
            -(val >> 1) - 1
        }
    }
}

fn decode_block(cabac: &mut CABAC, coeffs: &mut [i16], cat: usize, ctx_off: usize) -> bool {
    const CTX_BASE: [(usize, usize); 5] = [
        (0, 0), (15, 10), (29, 20), (44, 30), (47, 39)
    ];
    let (flag_off, coef_off) = CTX_BASE[cat];
    let scan: &[usize] = match coeffs.len() {
            4 => &CHROMA_DC_SCAN,
            15 => &ZIGZAG1,
            16 => &ZIGZAG,
            _ => unreachable!(),
        };

    let coded_block_flag = cabac.decode_bit(85 + ctx_off);
    let mut coded = [false; 16];
    if coded_block_flag {
        let mut last_idx = coeffs.len() - 1;
        for i in 0..coeffs.len() - 1 {
            coded[i] = cabac.decode_bit(105 + flag_off + i); // or 277 for interlaced
            if coded[i] {
                let last = cabac.decode_bit(166 + flag_off + i); // or 338 for interlaced
                if last {
                    last_idx = i;
                    break;
                }
            }
        }
        coded[last_idx] = true;
        let mut coef_ctx = 0;
        for i in (0..=last_idx).rev() {
            if coded[i] {
                let zero_ctx = if coef_ctx < 4 { coef_ctx + 1 } else { 0 };
                coeffs[scan[i]] = if !cabac.decode_bit(227 + coef_off + zero_ctx) {
                        if coef_ctx < 3 {
                            coef_ctx += 1;
                        }
                        1
                    } else {
                        let cur_ctx = 227 + coef_off + (coef_ctx + 2).max(5);
                        coef_ctx = (coef_ctx + 1).max(4).min(7);

                        let mut coef = 2;
                        while coef < 15 && cabac.decode_bit(cur_ctx) {
                            coef += 1;
                        }
                        if coef == 15 {
                            let mut pfx = 0;
                            while pfx < 15 && cabac.decode_bypass() {
                                pfx += 1;
                            }
                            let mut tail = 1;
                            for _ in 0..pfx {
                                tail = (tail << 1) + (cabac.decode_bypass() as i16);
                            }
                            coef + tail - 1
                        } else {
                            coef
                        }
                    };
                if cabac.decode_bypass() {
                    coeffs[scan[i]] = -coeffs[scan[i]];
                }
            }
        }
    }
    coded_block_flag
}

fn decode_block8x8(cabac: &mut CABAC, coeffs: &mut [i16; 64], _cat: usize) {
    const SIG_FLAG_MAP: [usize; 63] = [
         0,  1,  2,  3,  4,  5,  5,  4,  4,  3,  3,  4,  4,  4,  5,  5,
         4,  4,  4,  4,  3,  3,  6,  7,  7,  7,  8,  9, 10,  9,  8,  7,
         7,  6, 11, 12, 13, 11,  6,  7,  8,  9, 14, 10,  9,  8,  6, 11,
        12, 13, 11,  6,  9, 14, 10,  9, 11, 12, 13, 11, 14, 10, 12
    ];
    const LAST_SIG_FLAG_MAP: [usize; 63] = [
        0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4,
        5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8
    ];
    let (flag_off, coef_off) = (0, 0);
    let scan = &ZIGZAG8X8;

    let mut coded = [false; 64];
    let mut last_idx = coeffs.len() - 1;
    for i in 0..coeffs.len() - 1 {
        coded[i] = cabac.decode_bit(402 + flag_off + SIG_FLAG_MAP[i]);
        if coded[i] {
            let last = cabac.decode_bit(417 + flag_off + LAST_SIG_FLAG_MAP[i]);
            if last {
                last_idx = i;
                break;
            }
        }
    }
    coded[last_idx] = true;
    let mut coef_ctx = 0;
    for i in (0..=last_idx).rev() {
        if coded[i] {
            let zero_ctx = if coef_ctx < 4 { coef_ctx + 1 } else { 0 };
            coeffs[scan[i]] = if !cabac.decode_bit(426 + coef_off + zero_ctx) {
                    if coef_ctx < 3 {
                        coef_ctx += 1;
                    }
                    1
                } else {
                    let cur_ctx = 426 + coef_off + (coef_ctx + 2).max(5);
                    coef_ctx = (coef_ctx + 1).max(4).min(7);

                    let mut coef = 2;
                    while coef < 15 && cabac.decode_bit(cur_ctx) {
                        coef += 1;
                    }
                    if coef == 15 {
                        let mut pfx = 0;
                        while pfx < 15 && cabac.decode_bypass() {
                            pfx += 1;
                        }
                        let mut tail = 1;
                        for _ in 0..pfx {
                            tail = (tail << 1) + (cabac.decode_bypass() as i16);
                        }
                        coef + tail - 1
                    } else {
                        coef
                    }
                };
            if cabac.decode_bypass() {
                coeffs[scan[i]] = -coeffs[scan[i]];
            }
        }
    }
}

fn derive_ctx_off(sstate: &mut SliceState, cat: usize, blk_no: usize) -> usize {
    let mbt   = sstate.get_cur_mb().mb_type;
    let mut mbt_a = sstate.get_left_mb().mb_type;
    let mut mbt_b = sstate.get_top_mb().mb_type;
    let (trans_a, trans_b, mut cond_term_a, mut cond_term_b) = match cat {
            0 => {
                (mbt_a == CompactMBType::Intra16x16,
                 mbt_b == CompactMBType::Intra16x16,
                 (sstate.get_left_mb().coded_flags & 1) as usize,
                 (sstate.get_top_mb().coded_flags  & 1) as usize)
            },
            1 | 2 => {
                if (blk_no & 3) != 0 {
                    mbt_a = mbt;
                }
                if blk_no >= 4 {
                    mbt_b = mbt;
                }
                let nc_left = sstate.get_left_blk4(blk_no).ncoded;
                let nc_top  = sstate.get_top_blk4(blk_no).ncoded;
                (nc_left != 0,
                 nc_top != 0,
                 (nc_left != 0) as usize,
                 (nc_top != 0) as usize)
            },
            3 => {
                ((sstate.get_left_mb().cbp & 0x30) != 0,
                 (sstate.get_top_mb().cbp & 0x30) != 0,
                 ((sstate.get_left_mb().coded_flags & (1 << (blk_no + 1 + 16))) != 0) as usize,
                 ((sstate.get_top_mb().coded_flags & (1 << (blk_no + 1 + 16))) != 0) as usize)
            },
            4 => {
                let chroma = blk_no >> 2;
                if (blk_no & 1) != 0 {
                    mbt_a = mbt;
                }
                if (blk_no & 2) != 0 {
                    mbt_b = mbt;
                }
                ((blk_no & 1) != 0 || (sstate.get_left_mb().cbp & 0x20) != 0,
                 (blk_no & 2) != 0 || (sstate.get_top_mb().cbp & 0x20) != 0,
                 (sstate.get_left_blk8(blk_no & 3).ncoded_c[chroma] != 0) as usize,
                 (sstate.get_top_blk8(blk_no & 3).ncoded_c[chroma] != 0) as usize)
            },
            _ => unreachable!(),
        };
    /*let coded_no = match cat {
            0     => 0,
            1 | 2 => blk_no + 1,
            3     => 1 + 16 + blk_no,
            4     => 1 + 16 + 2 + blk_no,
            _ => unreachable!(),
        };*/

    if mbt_a == CompactMBType::None && mbt.is_inter() {
        cond_term_a = 0;
    }
    if !trans_a && mbt_a != CompactMBType::PCM {
        cond_term_a = 0;
    }
    /*if mbt.is_intra() && pps.constrained_intra_pred && mbt_a.is_inter() && slice_partitioning {
        cond_term_a = 0;
    }*/
    if (mbt_a == CompactMBType::PCM) || (mbt_a == CompactMBType::None && mbt.is_intra()) {
        cond_term_a = 1;
    }

    if mbt_b == CompactMBType::None && mbt.is_inter() {
        cond_term_b = 0;
    }
    if !trans_b && mbt_b != CompactMBType::PCM {
        cond_term_b = 0;
    }
    /*if mbt.is_intra() && pps.constrained_intra_pred && mbt_b.is_inter() && slice_partitioning {
        cond_term_b = 0;
    }*/
    if (mbt_b == CompactMBType::PCM) || (mbt_b == CompactMBType::None && mbt.is_intra()) {
        cond_term_b = 1;
    }

    cat * 4 + cond_term_b * 2 + cond_term_a
}

pub fn decode_residual_cabac(cabac: &mut CABAC, sstate: &mut SliceState, mb_info: &mut CurrentMBInfo) {
    sstate.get_cur_mb().mb_type = mb_info.mb_type.into();
    let mut coded_flags = 0;
    if mb_info.mb_type.is_intra16x16() {
        let off = derive_ctx_off(sstate, 0, 0);
        let coded = decode_block(cabac, &mut mb_info.coeffs[24], 0, off);
        mb_info.coded[24] = coded;
        if coded {
            coded_flags |= 1;
        }
    }
    if !mb_info.transform_size_8x8 {
        for blk8 in 0..4 {
            if (mb_info.cbpy & (1 << blk8)) != 0 {
                for blk4 in 0..4 {
                    let blk_no = (blk8 & 1) * 2 + (blk8 & 2) * 4 + (blk4 & 1) + (blk4 & 2) * 2;
                    let coded = if mb_info.mb_type.is_intra16x16() {
                            let off = derive_ctx_off(sstate, 1, blk_no);
                            decode_block(cabac, &mut mb_info.coeffs[blk_no][1..], 1, off)
                        } else {
                            let off = derive_ctx_off(sstate, 2, blk_no);
                            decode_block(cabac, &mut mb_info.coeffs[blk_no], 2, off)
                        };
                    sstate.get_cur_blk4(blk_no).ncoded = coded as u8;
                    mb_info.coded[blk_no] = coded;
                    if coded {
                        coded_flags |= 1 << (1 + blk_no);
                    }
                }
            }
        }
    } else {
        for blk8 in 0..4 {
            if (mb_info.cbpy & (1 << blk8)) != 0 {
                let blk4 = (blk8 & 1) * 2 + (blk8 & 2) * 4;
                decode_block8x8(cabac, &mut mb_info.coeffs8x8[blk8].coeffs, 5);
                coded_flags |= 0x33 << blk4;
                mb_info.coded[blk4]     = true;
                mb_info.coded[blk4 + 1] = true;
                mb_info.coded[blk4 + 4] = true;
                mb_info.coded[blk4 + 5] = true;
                sstate.get_cur_blk4(blk4).ncoded     = 1;
                sstate.get_cur_blk4(blk4 + 1).ncoded = 1;
                sstate.get_cur_blk4(blk4 + 4).ncoded = 1;
                sstate.get_cur_blk4(blk4 + 5).ncoded = 1;
            }
        }
    }
    for chroma in 0..2 {
        if (mb_info.cbpc & 3) != 0 {
            let off = derive_ctx_off(sstate, 3, chroma);
            let coded = decode_block(cabac, &mut mb_info.chroma_dc[chroma], 3, off);
            if coded {
                coded_flags |= 1 << (16 + 1 + chroma);
            }
        }
    }
    for chroma in 0..2 {
        if (mb_info.cbpc & 2) != 0 {
            for blk4 in 0..4 {
                let blk_no = 16 + chroma * 4 + blk4;
                let off = derive_ctx_off(sstate, 4, blk_no - 16);
                let coded = decode_block(cabac, &mut mb_info.coeffs[blk_no][1..], 4, off);
                sstate.get_cur_blk8(blk4).ncoded_c[chroma] = coded as u8;
                mb_info.coded[blk_no] = coded;
                if coded {
                    coded_flags |= 1 << (1 + 2 + blk_no);
                }
            }
        }
    }
    sstate.get_cur_mb().coded_flags = coded_flags;
}
