use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::frame::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::{CurrentMBInfo, I4X4_SCAN, Shareable};
use super::dispatch::{ThreadDispatcher, FrameDecodingStatus};
use super::dsp::*;
use super::pic_ref::SimplifiedSliceRefs;
use super::slice::{SliceHeader, WeightInfo, DEF_WEIGHT_INFO};
use super::types::*;

fn pred_intra(frm: &mut NASimpleVideoFrame<u8>, sstate: &SliceState, mb_info: &CurrentMBInfo) {
    let yoff = frm.offset[0] + sstate.mb_x * 16 + sstate.mb_y * 16 * frm.stride[0];
    match mb_info.mb_type {
        MBType::Intra16x16(imode, _, _) => {
            let id = if imode != 2 || (sstate.has_top && sstate.has_left) {
                    imode as usize
                } else if !sstate.has_top && !sstate.has_left {
                    IPRED8_DC128
                } else if !sstate.has_left {
                    IPRED8_DC_TOP
                } else {
                    IPRED8_DC_LEFT
                };
            IPRED_FUNCS16X16[id](&mut frm.data[yoff..], frm.stride[0], &sstate.top_line_y[sstate.mb_x * 16..], &sstate.left_y);
        },
        MBType::Intra8x8 => {
            let mut ictx = IPred8Context::new();
            for part in 0..4 {
                let x = (part & 1) * 2;
                let y = part & 2;
                let blk4 = x + y * 4;

                let cur_yoff = yoff + x * 4 + y * 4 * frm.stride[0];
                let has_top = y > 0 || sstate.has_top;
                let has_left = x > 0 || sstate.has_left;
                let imode = mb_info.ipred[blk4];
                let id = if imode != IntraPredMode::DC || (has_top && has_left) {
                        let im_id: u8 = imode.into();
                        im_id as usize
                    } else if !has_top && !has_left {
                        IPRED4_DC128
                    } else if !has_left {
                        IPRED4_DC_TOP
                    } else {
                        IPRED4_DC_LEFT
                    };
                let mb_idx = sstate.mb_x + sstate.mb_y * sstate.mb_w;
                let noright = (y == 2 || sstate.mb_x == sstate.mb_w - 1 || mb_idx < sstate.mb_start + sstate.mb_w) && (x == 2);
                let has_tl = (has_top && x > 0) || (has_left && y > 0) || (x == 0 && y == 0 && sstate.mb_x > 0 && mb_idx > sstate.mb_start + sstate.mb_w);
                if id != IPRED4_DC128 {
                    let top = if y == 0 {
                            &sstate.top_line_y[sstate.mb_x * 16 + x * 4..]
                        } else {
                            &frm.data[cur_yoff - frm.stride[0]..]
                        };
                    let mut left_buf = [0; 9];
                    let left = if x == 0 {
                            &sstate.left_y[y * 4..]
                        } else {
                            if has_tl {
                                if y == 0 {
                                    left_buf[0] = sstate.top_line_y[sstate.mb_x * 16 + x * 4 - 1];
                                } else {
                                    left_buf[0] = frm.data[cur_yoff - 1 - frm.stride[0]];
                                }
                            }
                            if has_left {
                                for (dst, src) in left_buf[1..].iter_mut().zip(frm.data[cur_yoff - 1..].chunks(frm.stride[0])) {
                                    *dst = src[0];
                                }
                            }
                            &left_buf
                        };
                    ictx.fill(top, left, has_top, has_top && !noright, has_left, has_tl);
                }
                IPRED_FUNCS8X8_LUMA[id](&mut frm.data[cur_yoff..], frm.stride[0], &ictx);
                if mb_info.coded[blk4] {
                    add_coeffs8(frm.data, cur_yoff, frm.stride[0], &mb_info.coeffs8x8[part].coeffs);
                }
            }
        },
        MBType::Intra4x4 => {
            for &(x,y) in I4X4_SCAN.iter() {
                let x = x as usize;
                let y = y as usize;
                let cur_yoff = yoff + x * 4 + y * 4 * frm.stride[0];
                let has_top = y > 0 || sstate.has_top;
                let has_left = x > 0 || sstate.has_left;
                let imode = mb_info.ipred[x + y * 4];
                let id = if imode != IntraPredMode::DC || (has_top && has_left) {
                        let im_id: u8 = imode.into();
                        im_id as usize
                    } else if !has_top && !has_left {
                        IPRED4_DC128
                    } else if !has_left {
                        IPRED4_DC_TOP
                    } else {
                        IPRED4_DC_LEFT
                    };
                let noright = (sstate.mb_x == sstate.mb_w - 1 || sstate.mb_x + sstate.mb_y * sstate.mb_w < sstate.mb_start + sstate.mb_w) && (x == 3);
                let tr: [u8; 4] = if y == 0 {
                        let tsrc = &sstate.top_line_y[sstate.mb_x * 16 + x * 4..];
                        if has_top && !noright {
                            [tsrc[4], tsrc[5], tsrc[6], tsrc[7]]
                        } else if has_top {
                            [tsrc[3]; 4]
                        } else {
                            [0; 4]
                        }
                    } else if (x & 1) == 0 || (x == 1 && y == 2) {
                        let i = cur_yoff - frm.stride[0];
                        [frm.data[i + 4], frm.data[i + 5], frm.data[i + 6], frm.data[i + 7]]
                    } else {
                        let i = cur_yoff - frm.stride[0];
                        [frm.data[i + 3], frm.data[i + 3], frm.data[i + 3], frm.data[i + 3]]
                    };
                let mut top = [128; 4];
                let mut left = [128; 9];
                if y == 0 {
                    if has_top {
                        top.copy_from_slice(&sstate.top_line_y[sstate.mb_x * 16 + x * 4..][..4]);
                    }
                } else {
                    top.copy_from_slice(&frm.data[cur_yoff - frm.stride[0]..][..4]);
                }
                if x == 0 {
                    if has_left {
                        for (dst, &src) in left.iter_mut().zip(sstate.left_y[y * 4..].iter()) {
                            *dst = src;
                        }
                    }
                } else {
                    if y == 0 {
                        if x == 0 {
                            left[0] = sstate.left_y[y * 4];
                        } else if has_top {
                            left[0] = sstate.top_line_y[sstate.mb_x * 16 + x * 4 - 1];
                        }
                    } else {
                        left[0] = frm.data[cur_yoff - frm.stride[0] - 1];
                    }
                    for (dst, row) in left[1..].iter_mut().zip(frm.data[cur_yoff - 1..].chunks(frm.stride[0])) {
                        *dst = row[0];
                    }
                }
                IPRED_FUNCS4X4[id](&mut frm.data[cur_yoff..], frm.stride[0], &top, &left, &tr);
                if mb_info.coded[x + y * 4] {
                    add_coeffs(frm.data, cur_yoff, frm.stride[0], &mb_info.coeffs[x + y * 4]);
                }
            }
        },
        _ => unreachable!(),
    };
    let id = if mb_info.chroma_ipred != 0 || (sstate.has_top && sstate.has_left) {
            mb_info.chroma_ipred as usize
        } else if !sstate.has_top && !sstate.has_left {
            IPRED8_DC128
        } else if !sstate.has_left {
            IPRED8_DC_TOP
        } else {
            IPRED8_DC_LEFT
        };
    for chroma in 1..3 {
        let off = frm.offset[chroma] + sstate.mb_x * 8 + sstate.mb_y * 8 * frm.stride[chroma];
        let top = &sstate.top_line_c[chroma - 1][sstate.mb_x * 8..];
        IPRED_FUNCS8X8_CHROMA[id](&mut frm.data[off..], frm.stride[chroma], top, &sstate.left_c[chroma - 1]);
    }
}

fn add_luma(frm: &mut NASimpleVideoFrame<u8>, sstate: &SliceState, mb_info: &CurrentMBInfo) {
    let mut yoff = frm.offset[0] + sstate.mb_x * 16 + sstate.mb_y * 16 * frm.stride[0];
    if !mb_info.transform_size_8x8 {
        for y in 0..4 {
            for x in 0..4 {
                if mb_info.coded[x + y * 4] {
                    add_coeffs(frm.data, yoff + x * 4, frm.stride[0], &mb_info.coeffs[x + y * 4]);
                }
            }
            yoff += frm.stride[0] * 4;
        }
    } else {
        for y in 0..2 {
            for x in 0..2 {
                if mb_info.coded[x * 2 + y * 2 * 4] {
                    add_coeffs8(frm.data, yoff + x * 8, frm.stride[0], &mb_info.coeffs8x8[x + y * 2].coeffs);
                }
            }
            yoff += frm.stride[0] * 8;
        }
    }
}

fn add_chroma(frm: &mut NASimpleVideoFrame<u8>, sstate: &SliceState, mb_info: &CurrentMBInfo) {
    for chroma in 1..3 {
        let mut off = frm.offset[chroma] + sstate.mb_x * 8 + sstate.mb_y * 8 * frm.stride[chroma];
        for y in 0..2 {
            for x in 0..2 {
                let blk_no = 16 + (chroma - 1) * 4 + x + y * 2;
                if mb_info.coded[blk_no] || mb_info.coeffs[blk_no][0] != 0 {
                    add_coeffs(frm.data, off + x * 4, frm.stride[chroma], &mb_info.coeffs[blk_no]);
                }
            }
            off += frm.stride[chroma] * 4;
        }
    }
}

fn do_p_mc(frm: &mut NASimpleVideoFrame<u8>, xpos: usize, ypos: usize, w: usize, h: usize, mv: MV, ref_pic: Option<&SimpleFrame>, weight: &WeightInfo, mc_dsp: &mut H264MC) {
    if let Some(buf) = ref_pic {
        if !weight.is_weighted() {
            mc_dsp.do_mc(frm, buf, xpos, ypos, w, h, mv);
        } else {
            let mut tmp = McBlock::new();
            mc_dsp.mc_blocks(&mut tmp, buf, xpos, ypos, w, h, mv);

            let yoff = frm.offset[0] + xpos + ypos * frm.stride[0];
            let yw = if weight.luma_weighted {
                    [weight.luma_weight, weight.luma_offset, weight.luma_shift as i8]
                } else {
                    [1, 0, 0]
                };
            let wmode = match w {
                    2 => 0,
                    4 => 1,
                    8 => 2,
                    _ => 3,
                };
            (mc_dsp.put_block_weighted[wmode])(&mut frm.data[yoff..], frm.stride[0], &tmp.y, h, yw);

            for chroma in 0..2 {
                let cstride = frm.stride[chroma + 1];
                let coff = frm.offset[chroma + 1] + xpos / 2 + ypos / 2 * cstride;
                let cw = if weight.chroma_weighted {
                        [weight.chroma_weight[chroma], weight.chroma_offset[chroma], weight.chroma_shift as i8]
                    } else {
                        [1, 0, 0]
                    };
                let csrc = if chroma == 0 { &tmp.u } else { &tmp.v };
                (mc_dsp.put_block_weighted[wmode - 1])(&mut frm.data[coff..], cstride, csrc, h / 2, cw);
            }
        }
    } else {
        mc_dsp.gray_block(frm, xpos, ypos, w, h);
    }
}

#[allow(clippy::match_like_matches_macro)]
fn do_b_mc(frm: &mut NASimpleVideoFrame<u8>, mode: BMode, xpos: usize, ypos: usize, w: usize, h: usize, mv0: MV, ref_pic0: Option<&SimpleFrame>, weight0: &WeightInfo, mv1: MV, ref_pic1: Option<&SimpleFrame>, weight1: &WeightInfo, mc_dsp: &mut H264MC) {
    let do_weight = match (mode, weight0.is_weighted(), weight1.is_weighted()) {
            (BMode::L0, true, _) => true,
            (BMode::L1, _, true) => true,
            (BMode::Bi, true, true) => true,
            _ => false,
        };
    if !do_weight {
        match mode {
            BMode::L0 => {
                if let Some(buf) = ref_pic0 {
                    mc_dsp.do_mc(frm, buf, xpos, ypos, w, h, mv0);
                } else {
                    mc_dsp.gray_block(frm, xpos, ypos, w, h);
                }
            },
            BMode::L1 => {
                if let Some(buf) = ref_pic1 {
                    mc_dsp.do_mc(frm, buf, xpos, ypos, w, h, mv1);
                } else {
                    mc_dsp.gray_block(frm, xpos, ypos, w, h);
                }
            },
            BMode::Bi => {
                match (ref_pic0, ref_pic1) {
                    (Some(buf0), Some(buf1)) => {
                        mc_dsp.do_mc(frm, buf0, xpos, ypos, w, h, mv0);
                        mc_dsp.do_mc_avg(frm, buf1, xpos, ypos, w, h, mv1);
                    },
                    (Some(buf0), None) => {
                        mc_dsp.do_mc(frm, buf0, xpos, ypos, w, h, mv0);
                    },
                    (None, Some(buf1)) => {
                        mc_dsp.do_mc(frm, buf1, xpos, ypos, w, h, mv1);
                    },
                    (None, None) => {
                        mc_dsp.gray_block(frm, xpos, ypos, w, h);
                    },
                };
            },
        };
    } else {
        let mut tmp0 = McBlock::new();
        let mut tmp1 = McBlock::new();
        match (mode, ref_pic0, ref_pic1) {
            (BMode::L0, Some(buf), _) | (BMode::L1, _, Some(buf)) => {
                let (mv, weight) = if mode == BMode::L0 { (mv0, weight0) } else { (mv1, weight1) };
                mc_dsp.mc_blocks(&mut tmp0, buf, xpos, ypos, w, h, mv);

                let yoff = frm.offset[0] + xpos + ypos * frm.stride[0];
                let yw = if weight.luma_weighted {
                        [weight.luma_weight, weight.luma_offset, weight.luma_shift as i8]
                    } else {
                        [1, 0, 0]
                    };
                let wmode = match w {
                        2 => 0,
                        4 => 1,
                        8 => 2,
                        _ => 3,
                    };
                (mc_dsp.put_block_weighted[wmode])(&mut frm.data[yoff..], frm.stride[0], &tmp0.y, h, yw);

                for chroma in 0..2 {
                    let cstride = frm.stride[chroma + 1];
                    let coff = frm.offset[chroma + 1] + xpos / 2 + ypos / 2 * cstride;
                    let cw = if weight.chroma_weighted {
                            [weight.chroma_weight[chroma], weight.chroma_offset[chroma], weight.chroma_shift as i8]
                        } else {
                            [1, 0, 0]
                        };
                    let csrc = if chroma == 0 { &tmp0.u } else { &tmp0.v };
                    (mc_dsp.put_block_weighted[wmode - 1])(&mut frm.data[coff..], cstride, csrc, h / 2, cw);
                }
            },
            (BMode::Bi, Some(buf0), Some(buf1)) => { // do both and avg
                mc_dsp.mc_blocks(&mut tmp0, buf0, xpos, ypos, w, h, mv0);
                mc_dsp.mc_blocks(&mut tmp1, buf1, xpos, ypos, w, h, mv1);

                let yoff = frm.offset[0] + xpos + ypos * frm.stride[0];
                let yw = match (weight0.luma_weighted, weight1.luma_weighted) {
                        (true, true) => [weight0.luma_weight, weight0.luma_offset, weight1.luma_weight, weight1.luma_offset, weight0.luma_shift as i8],
                        (true, false) => [weight0.luma_weight, weight0.luma_offset, 1 << weight0.luma_shift, 0, weight0.luma_shift as i8],
                        (false, true) => [1 << weight1.luma_shift, 0, weight1.luma_weight, weight1.luma_offset, weight1.luma_shift as i8],
                        (false, false) => [1, 0, 1, 0, 0],
                    };
                let wmode = match w {
                        2 => 0,
                        4 => 1,
                        8 => 2,
                        _ => 3,
                    };
                (mc_dsp.put_block_weighted2[wmode])(&mut frm.data[yoff..], frm.stride[0], &tmp0.y, &tmp1.y, h, yw);

                for chroma in 0..2 {
                    let cstride = frm.stride[chroma + 1];
                    let coff = frm.offset[chroma + 1] + xpos / 2 + ypos / 2 * cstride;
                    let cw0 = weight0.chroma_weight[chroma];
                    let co0 = weight0.chroma_offset[chroma];
                    let cw1 = weight1.chroma_weight[chroma];
                    let co1 = weight1.chroma_offset[chroma];
                    let cw = match (weight0.chroma_weighted, weight1.chroma_weighted) {
                            (true, true) => [cw0, co0, cw1, co1, weight0.luma_shift as i8],
                            (true, false) => [cw0, co0, 1 << weight0.luma_shift, 0, weight0.luma_shift as i8],
                            (false, true) => [1 << weight1.luma_shift, 0, cw1, co1, weight1.luma_shift as i8],
                            (false, false) => [1, 0, 1, 0, 0],
                        };
                    let csrc0 = if chroma == 0 { &tmp0.u } else { &tmp0.v };
                    let csrc1 = if chroma == 0 { &tmp1.u } else { &tmp1.v };
                    (mc_dsp.put_block_weighted2[wmode - 1])(&mut frm.data[coff..], cstride, csrc0, csrc1, h / 2, cw);
                }
            },
            _ => {
                mc_dsp.gray_block(frm, xpos, ypos, w, h);
            },
        };
    }
}

fn do_b_mc_4x4bi(frm: &mut NASimpleVideoFrame<u8>, xpos: usize, ypos: usize, mv: &[MV; 2], ref_pic0: Option<&SimpleFrame>, weight0: &WeightInfo, ref_pic1: Option<&SimpleFrame>, weight1: &WeightInfo, mc_dsp: &mut H264MC) {
    if !weight0.is_weighted() || !weight1.is_weighted() {
        match (ref_pic0, ref_pic1) {
            (Some(buf0), Some(buf1)) => {
                mc_dsp.do_mc(frm, buf0, xpos, ypos, 4, 4, mv[0]);
                mc_dsp.do_mc_avg(frm, buf1, xpos, ypos, 4, 4, mv[1]);
            },
            (Some(buf0), None) => {
                mc_dsp.do_mc(frm, buf0, xpos, ypos, 4, 4, mv[0]);
            },
            (None, Some(buf1)) => {
                mc_dsp.do_mc(frm, buf1, xpos, ypos, 4, 4, mv[1]);
            },
            (None, None) => {
                mc_dsp.gray_block(frm, xpos, ypos, 4, 4);
            },
        };
    } else {
        let mut tmp0 = McBlock::new();
        let mut tmp1 = McBlock::new();
        match (ref_pic0, ref_pic1) {
            (Some(buf0), Some(buf1)) => { // do both and avg
                mc_dsp.mc_blocks(&mut tmp0, buf0, xpos, ypos, 4, 4, mv[0]);
                mc_dsp.mc_blocks(&mut tmp1, buf1, xpos, ypos, 4, 4, mv[1]);

                let yoff = frm.offset[0] + xpos + ypos * frm.stride[0];
                let yw = match (weight0.luma_weighted, weight1.luma_weighted) {
                        (true, true) => [weight0.luma_weight, weight0.luma_offset, weight1.luma_weight, weight1.luma_offset, weight0.luma_shift as i8],
                        (true, false) => [weight0.luma_weight, weight0.luma_offset, 1 << weight0.luma_shift, 0, weight0.luma_shift as i8],
                        (false, true) => [1 << weight1.luma_shift, 0, weight1.luma_weight, weight1.luma_offset, weight1.luma_shift as i8],
                        (false, false) => [1, 0, 1, 0, 0],
                    };
                (mc_dsp.put_block_weighted2[1])(&mut frm.data[yoff..], frm.stride[0], &tmp0.y, &tmp1.y, 4, yw);

                for chroma in 0..2 {
                    let cstride = frm.stride[chroma + 1];
                    let coff = frm.offset[chroma + 1] + xpos / 2 + ypos / 2 * cstride;
                    let cw0 = weight0.chroma_weight[chroma];
                    let co0 = weight0.chroma_offset[chroma];
                    let cw1 = weight1.chroma_weight[chroma];
                    let co1 = weight1.chroma_offset[chroma];
                    let cw = match (weight0.chroma_weighted, weight1.chroma_weighted) {
                            (true, true) => [cw0, co0, cw1, co1, weight0.luma_shift as i8],
                            (true, false) => [cw0, co0, 1 << weight0.luma_shift, 0, weight0.luma_shift as i8],
                            (false, true) => [1 << weight1.luma_shift, 0, cw1, co1, weight1.luma_shift as i8],
                            (false, false) => [1, 0, 1, 0, 0],
                        };
                    let csrc0 = if chroma == 0 { &tmp0.u } else { &tmp0.v };
                    let csrc1 = if chroma == 0 { &tmp1.u } else { &tmp1.v };
                    (mc_dsp.put_block_weighted2[0])(&mut frm.data[coff..], cstride, csrc0, csrc1, 2, cw);
                }
            },
            _ => {
                mc_dsp.gray_block(frm, xpos, ypos, 4, 4);
            },
        };
    }
}

fn get_weights(slice_hdr: &SliceHeader, frame_refs: &SimplifiedSliceRefs, mode: BMode, weight_mode: u8, ref_l0: PicRef, ref_l1: PicRef) -> (WeightInfo, WeightInfo) {
    let idx_l0 = ref_l0.index();
    let idx_l1 = ref_l1.index();
    if mode != BMode::Bi || weight_mode != 2 {
        (slice_hdr.get_weight(0, idx_l0), slice_hdr.get_weight(1, idx_l1))
    } else if let (Some(Some(ref pic0)), Some(Some(ref pic1))) = (frame_refs.ref_list0.get(idx_l0), frame_refs.ref_list1.get(idx_l1)) {
        let r0_poc = pic0.full_id as u16;
        let r1_poc = pic1.full_id as u16;
        let cur_id = frame_refs.cur_id as u16;
        if (r0_poc == r1_poc) || pic0.long_term || pic1.long_term {
            return (DEF_WEIGHT_INFO, DEF_WEIGHT_INFO);
        }

        let td = (i32::from(r1_poc) - i32::from(r0_poc)).max(-128).min(127);
        let tx = (16384 + (td / 2).abs()) / td;
        let tb = (i32::from(cur_id) - i32::from(r0_poc)).max(-128).min(127);
        let scale = ((tb * tx + 32) >> 6).max(-1024).min(1023);
        if scale == 128 || (scale >> 2) < -64 || (scale >> 2) > 128 {
            return (DEF_WEIGHT_INFO, DEF_WEIGHT_INFO);
        }
        let w1 = (scale >> 2) as i8;
        let w0 = 64 - w1;

        let weight0 = WeightInfo {
            luma_weighted:      true,
            luma_weight:        w0,
            luma_offset:        0,
            luma_shift:         5,
            chroma_weighted:    true,
            chroma_weight:      [w0; 2],
            chroma_offset:      [0; 2],
            chroma_shift:       5,
        };
        let weight1 = WeightInfo {
            luma_weighted:      true,
            luma_weight:        w1,
            luma_offset:        0,
            luma_shift:         5,
            chroma_weighted:    true,
            chroma_weight:      [w1; 2],
            chroma_offset:      [0; 2],
            chroma_shift:       5,
        };

        (weight0, weight1)
    } else {
        (DEF_WEIGHT_INFO, DEF_WEIGHT_INFO)
    }
}

pub fn recon_mb(frm: &mut NASimpleVideoFrame<u8>, slice_hdr: &SliceHeader, mb_info: &CurrentMBInfo, sstate: &mut SliceState, frame_refs: &SimplifiedSliceRefs, mc_dsp: &mut H264MC, weight_mode: u8) {
    let xpos = sstate.mb_x * 16;
    let ypos = sstate.mb_y * 16;

    match mb_info.mb_type {
        MBType::Intra16x16(_, _, _) => {
            pred_intra(frm, sstate, mb_info);
        },
        MBType::Intra4x4 | MBType::Intra8x8 => {
            pred_intra(frm, sstate, mb_info);
        },
        MBType::PCM => {},
        MBType::PSkip => {
            let mv = sstate.get_cur_blk4(0).mv[0];
            let rpic = frame_refs.select_ref_pic(0, 0);
            let weight = &slice_hdr.get_weight(0, 0);
            do_p_mc(frm, xpos, ypos, 16, 16, mv, rpic, weight, mc_dsp);
        },
        MBType::P16x16 => {
            let mv = sstate.get_cur_blk4(0).mv[0];
            let rpic = frame_refs.select_ref_pic(0, mb_info.ref_l0[0].index());
            let weight = &slice_hdr.get_weight(0, mb_info.ref_l0[0].index());
            do_p_mc(frm, xpos, ypos, 16, 16, mv, rpic, weight, mc_dsp);
        },
        MBType::P16x8 | MBType::P8x16 => {
            let (bw, bh, bx, by) = if mb_info.mb_type == MBType::P16x8 {
                    (16, 8, 0, 8)
                } else {
                    (8, 16, 8, 0)
                };
            let mv = sstate.get_cur_blk4(0).mv[0];
            let rpic = frame_refs.select_ref_pic(0, mb_info.ref_l0[0].index());
            let weight = &slice_hdr.get_weight(0, mb_info.ref_l0[0].index());
            do_p_mc(frm, xpos, ypos, bw, bh, mv, rpic, weight, mc_dsp);
            let mv = sstate.get_cur_blk4(bx / 4 + by).mv[0];
            let rpic = frame_refs.select_ref_pic(0, mb_info.ref_l0[1].index());
            let weight = &slice_hdr.get_weight(0, mb_info.ref_l0[1].index());
            do_p_mc(frm, xpos + bx, ypos + by, bw, bh, mv, rpic, weight, mc_dsp);
        },
        MBType::P8x8 | MBType::P8x8Ref0 => {
            for part in 0..4 {
                let bx = (part & 1) * 8;
                let by = (part & 2) * 4;
                let mv = sstate.get_cur_blk4(bx / 4 + by).mv[0];
                let rpic = frame_refs.select_ref_pic(0, mb_info.ref_l0[part].index());
                let weight = &slice_hdr.get_weight(0, mb_info.ref_l0[part].index());

                match mb_info.sub_mb_type[part] {
                    SubMBType::P8x8 => {
                        do_p_mc(frm, xpos + bx, ypos + by, 8, 8, mv, rpic, weight, mc_dsp);
                    },
                    SubMBType::P8x4 => {
                        do_p_mc(frm, xpos + bx, ypos + by, 8, 4, mv, rpic, weight, mc_dsp);
                        let mv = sstate.get_cur_blk4(bx / 4 + by + 4).mv[0];
                        do_p_mc(frm, xpos + bx, ypos + by + 4, 8, 4, mv, rpic, weight, mc_dsp);
                    },
                    SubMBType::P4x8 => {
                        do_p_mc(frm, xpos + bx, ypos + by, 4, 8, mv, rpic, weight, mc_dsp);
                        let mv = sstate.get_cur_blk4(bx / 4 + by + 1).mv[0];
                        do_p_mc(frm, xpos + bx + 4, ypos + by, 4, 8, mv, rpic, weight, mc_dsp);
                    },
                    SubMBType::P4x4 => {
                        for sb_no in 0..4 {
                            let sxpos = xpos + bx + (sb_no & 1) * 4;
                            let sypos = ypos + by + (sb_no & 2) * 2;
                            let sblk_no = (bx / 4 + (sb_no & 1)) + ((by / 4) + (sb_no >> 1)) * 4;
                            let mv = sstate.get_cur_blk4(sblk_no).mv[0];
                            do_p_mc(frm, sxpos, sypos, 4, 4, mv, rpic, weight, mc_dsp);
                        }
                    },
                    _ => unreachable!(),
                };
            }
        },
        MBType::B16x16(mode) => {
            let mv0 = sstate.get_cur_blk4(0).mv[0];
            let rpic0 = frame_refs.select_ref_pic(0, mb_info.ref_l0[0].index());
            let mv1 = sstate.get_cur_blk4(0).mv[1];
            let rpic1 = frame_refs.select_ref_pic(1, mb_info.ref_l1[0].index());
            let (weight0, weight1) = get_weights(slice_hdr, frame_refs, mode, weight_mode, mb_info.ref_l0[0], mb_info.ref_l1[0]);
            do_b_mc(frm, mode, xpos, ypos, 16, 16, mv0, rpic0, &weight0, mv1, rpic1, &weight1, mc_dsp);
        },
        MBType::B16x8(mode0, mode1) | MBType::B8x16(mode0, mode1) => {
            let (pw, ph) = mb_info.mb_type.size();
            let (px, py) = (pw & 8, ph & 8);
            let modes = [mode0, mode1];
            let (mut bx, mut by) = (0, 0);
            for part in 0..2 {
                let blk = if part == 0 { 0 } else { (px / 4) + py };
                let mv0 = sstate.get_cur_blk4(blk).mv[0];
                let rpic0 = frame_refs.select_ref_pic(0, mb_info.ref_l0[part].index());
                let mv1 = sstate.get_cur_blk4(blk).mv[1];
                let rpic1 = frame_refs.select_ref_pic(1, mb_info.ref_l1[part].index());
                let (weight0, weight1) = get_weights(slice_hdr, frame_refs, modes[part], weight_mode, mb_info.ref_l0[part], mb_info.ref_l1[part]);
                do_b_mc(frm, modes[part], xpos + bx, ypos + by, pw, ph, mv0, rpic0, &weight0, mv1, rpic1, &weight1, mc_dsp);
                bx += px;
                by += py;
            }
        },
        MBType::Direct | MBType::BSkip => {
            let colo_mb_type = frame_refs.get_colocated_info(sstate.mb_x, sstate.mb_y).0.mb_type;
            let is_16x16 = colo_mb_type.is_16x16_ref();

            if is_16x16 {
                let mv = sstate.get_cur_blk4(0).mv;
                let ref_idx = sstate.get_cur_blk8(0).ref_idx;
                let rpic0 = frame_refs.select_ref_pic(0, ref_idx[0].index());
                let rpic1 = frame_refs.select_ref_pic(1, ref_idx[1].index());
                let (weight0, weight1) = get_weights(slice_hdr, frame_refs, BMode::Bi, weight_mode, ref_idx[0], ref_idx[1]);
                do_b_mc(frm, BMode::Bi, xpos, ypos, 16, 16, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
            } else {
                for blk4 in 0..16 {
                    let ref_idx = sstate.get_cur_blk8(blk4_to_blk8(blk4)).ref_idx;
                    let rpic0 = frame_refs.select_ref_pic(0, ref_idx[0].index());
                    let rpic1 = frame_refs.select_ref_pic(1, ref_idx[1].index());
                    let (weight0, weight1) = get_weights(slice_hdr, frame_refs, BMode::Bi, weight_mode, ref_idx[0], ref_idx[1]);
                    let mv = &sstate.get_cur_blk4(blk4).mv;
                    do_b_mc_4x4bi(frm, xpos + (blk4 & 3) * 4, ypos + (blk4 >> 2) * 4, mv, rpic0, &weight0, rpic1, &weight1, mc_dsp);
                }
            }
            sstate.apply_to_blk8(|blk8| { blk8.ref_idx[0].set_direct(); blk8.ref_idx[1].set_direct(); });
        },
        MBType::B8x8 => {
            for part in 0..4 {
                let ridx = sstate.get_cur_blk8(part).ref_idx;
                let rpic0 = frame_refs.select_ref_pic(0, ridx[0].index());
                let rpic1 = frame_refs.select_ref_pic(1, ridx[1].index());
                let subtype = mb_info.sub_mb_type[part];
                let blk8 = (part & 1) * 2 + (part & 2) * 4;
                let mut bx = (part & 1) * 8;
                let mut by = (part & 2) * 4;
                match subtype {
                    SubMBType::Direct8x8 => {
                        for blk in 0..4 {
                            let ref_idx = sstate.get_cur_blk8(bx / 8 + (by / 8) * 2).ref_idx;
                            let rpic0 = frame_refs.select_ref_pic(0, ref_idx[0].index());
                            let rpic1 = frame_refs.select_ref_pic(1, ref_idx[1].index());
                            let (weight0, weight1) = get_weights(slice_hdr, frame_refs, BMode::Bi, weight_mode, ref_idx[0], ref_idx[1]);
                            let mv = &sstate.get_cur_blk4(bx / 4 + (by / 4) * 4).mv;
                            do_b_mc_4x4bi(frm, xpos + bx, ypos + by, mv, rpic0, &weight0, rpic1, &weight1, mc_dsp);
                            bx += 4;
                            if blk == 1 {
                                bx -= 8;
                                by += 4;
                            }
                        }
                        sstate.get_cur_blk8(part).ref_idx[0].set_direct();
                        sstate.get_cur_blk8(part).ref_idx[1].set_direct();
                    },
                    SubMBType::B8x8(mode) => {
                        let mv = sstate.get_cur_blk4(blk8).mv;
                        let (weight0, weight1) = get_weights(slice_hdr, frame_refs, mode, weight_mode, ridx[0], ridx[1]);
                        do_b_mc(frm, mode, xpos + bx, ypos + by, 8, 8, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                    },
                    SubMBType::B8x4(mode) | SubMBType::B4x8(mode) => {
                        let (weight0, weight1) = get_weights(slice_hdr, frame_refs, mode, weight_mode, ridx[0], ridx[1]);
                        let (pw, ph) = subtype.size();
                        let mv = sstate.get_cur_blk4(blk8).mv;
                        do_b_mc(frm, mode, xpos + bx, ypos + by, pw, ph, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                        let addr2 = blk8 + (pw & 4) / 4 + (ph & 4);
                        let mv = sstate.get_cur_blk4(addr2).mv;
                        do_b_mc(frm, mode, xpos + bx + (pw & 4), ypos + by + (ph & 4), pw, ph, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                    },
                    SubMBType::B4x4(mode) => {
                        let (weight0, weight1) = get_weights(slice_hdr, frame_refs, mode, weight_mode, ridx[0], ridx[1]);
                        for i in 0..4 {
                            let addr2 = blk8 + (i & 1) + (i & 2) * 2;
                            let mv = sstate.get_cur_blk4(addr2).mv;
                            do_b_mc(frm, mode, xpos + bx, ypos + by, 4, 4, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                            bx += 4;
                            if i == 1 {
                                bx -= 8;
                                by += 4;
                            }
                        }
                    },
                    _ => unreachable!(),
                };
            }
        },
    };
    if !mb_info.mb_type.is_skip() {
        if mb_info.mb_type != MBType::Intra4x4 && mb_info.mb_type != MBType::Intra8x8 {
            add_luma(frm, sstate, mb_info);
        }
        add_chroma(frm, sstate, mb_info);
    }
}

pub fn wait_for_mb(disp: &Shareable<ThreadDispatcher>, sstate: &SliceState, xpos: usize, ypos: usize, mv: MV, ref_id: u32) -> DecoderResult<()> {
    let xpos = xpos as isize + ((mv.x >> 2) as isize) + 4;
    let ypos = ypos as isize + ((mv.y >> 2) as isize) + 4;
    let dst_mb_x = ((xpos.max(0) as usize) / 16).min(sstate.mb_w - 1);
    let dst_mb_y = ((ypos.max(0) as usize) / 16).min(sstate.mb_h - 1);
    let expected_mb = dst_mb_x + dst_mb_y * sstate.mb_w;
    loop {
        if let Ok(ds) = disp.read() {
            match ds.check_pos(ref_id, expected_mb) {
                FrameDecodingStatus::Ok => return Ok(()),
                FrameDecodingStatus::NotReady => {},
                _ => return Err(DecoderError::MissingReference),
            };
        }
        std::thread::yield_now();
    }
}

fn wait_b_mc(disp: &Shareable<ThreadDispatcher>, sstate: &SliceState, frame_refs: &SimplifiedSliceRefs, mv: [MV; 2], ref_idx: [PicRef; 2], xpos: usize, ypos: usize, w: usize, h: usize) -> DecoderResult<()> {
    if let Some(ref_id) = frame_refs.get_ref_id(0, ref_idx[0].index()) {
        wait_for_mb(disp, sstate, xpos + w, ypos + h, mv[0], ref_id)?;
    }
    if let Some(ref_id) = frame_refs.get_ref_id(1, ref_idx[1].index()) {
        wait_for_mb(disp, sstate, xpos + w, ypos + h, mv[1], ref_id)?;
    }
    Ok(())
}

pub fn recon_mb_mt(frm: &mut NASimpleVideoFrame<u8>, slice_hdr: &SliceHeader, mb_info: &CurrentMBInfo, sstate: &mut SliceState, frame_refs: &SimplifiedSliceRefs, mc_dsp: &mut H264MC, weight_mode: u8, disp: &Shareable<ThreadDispatcher>) -> DecoderResult<()> {
    let xpos = sstate.mb_x * 16;
    let ypos = sstate.mb_y * 16;

    match mb_info.mb_type {
        MBType::Intra16x16(_, _, _) => {
            pred_intra(frm, sstate, mb_info);
        },
        MBType::Intra4x4 | MBType::Intra8x8 => {
            pred_intra(frm, sstate, mb_info);
        },
        MBType::PCM => {},
        MBType::PSkip => {
            let mv = sstate.get_cur_blk4(0).mv[0];
            if let Some(ref_id) = frame_refs.get_ref_id(0, 0) {
                wait_for_mb(disp, sstate, xpos + 16, ypos + 16, mv, ref_id)?;
            }
            let rpic = frame_refs.select_ref_pic(0, 0);
            let weight = &slice_hdr.get_weight(0, 0);
            do_p_mc(frm, xpos, ypos, 16, 16, mv, rpic, weight, mc_dsp);
        },
        MBType::P16x16 => {
            let mv = sstate.get_cur_blk4(0).mv[0];
            if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[0].index()) {
                wait_for_mb(disp, sstate, xpos + 16, ypos + 16, mv, ref_id)?;
            }
            let rpic = frame_refs.select_ref_pic(0, mb_info.ref_l0[0].index());
            let weight = &slice_hdr.get_weight(0, mb_info.ref_l0[0].index());
            do_p_mc(frm, xpos, ypos, 16, 16, mv, rpic, weight, mc_dsp);
        },
        MBType::P16x8 | MBType::P8x16 => {
            let (bw, bh, bx, by) = if mb_info.mb_type == MBType::P16x8 {
                    (16, 8, 0, 8)
                } else {
                    (8, 16, 8, 0)
                };
            let mv = sstate.get_cur_blk4(0).mv[0];
            if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[0].index()) {
                wait_for_mb(disp, sstate, xpos + bw, ypos + bh, mv, ref_id)?;
            }
            let rpic = frame_refs.select_ref_pic(0, mb_info.ref_l0[0].index());
            let weight = &slice_hdr.get_weight(0, mb_info.ref_l0[0].index());
            do_p_mc(frm, xpos, ypos, bw, bh, mv, rpic, weight, mc_dsp);
            let mv = sstate.get_cur_blk4(bx / 4 + by).mv[0];
            if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[1].index()) {
                wait_for_mb(disp, sstate, xpos + 16, ypos + 16, mv, ref_id)?;
            }
            let rpic = frame_refs.select_ref_pic(0, mb_info.ref_l0[1].index());
            let weight = &slice_hdr.get_weight(0, mb_info.ref_l0[1].index());
            do_p_mc(frm, xpos + bx, ypos + by, bw, bh, mv, rpic, weight, mc_dsp);
        },
        MBType::P8x8 | MBType::P8x8Ref0 => {
            for part in 0..4 {
                let bx = (part & 1) * 8;
                let by = (part & 2) * 4;
                let mv = sstate.get_cur_blk4(bx / 4 + by).mv[0];
                let rpic = frame_refs.select_ref_pic(0, mb_info.ref_l0[part].index());
                let weight = &slice_hdr.get_weight(0, mb_info.ref_l0[part].index());

                match mb_info.sub_mb_type[part] {
                    SubMBType::P8x8 => {
                        if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[part].index()) {
                            wait_for_mb(disp, sstate, xpos + bx + 8, ypos + by + 8, mv, ref_id)?;
                        }
                        do_p_mc(frm, xpos + bx, ypos + by, 8, 8, mv, rpic, weight, mc_dsp);
                    },
                    SubMBType::P8x4 => {
                        if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[part].index()) {
                            wait_for_mb(disp, sstate, xpos + bx + 8, ypos + by + 4, mv, ref_id)?;
                        }
                        do_p_mc(frm, xpos + bx, ypos + by, 8, 4, mv, rpic, weight, mc_dsp);
                        let mv = sstate.get_cur_blk4(bx / 4 + by + 4).mv[0];
                        if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[part].index()) {
                            wait_for_mb(disp, sstate, xpos + bx + 8, ypos + by + 8, mv, ref_id)?;
                        }
                        do_p_mc(frm, xpos + bx, ypos + by + 4, 8, 4, mv, rpic, weight, mc_dsp);
                    },
                    SubMBType::P4x8 => {
                        if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[part].index()) {
                            wait_for_mb(disp, sstate, xpos + bx + 4, ypos + by + 8, mv, ref_id)?;
                        }
                        do_p_mc(frm, xpos + bx, ypos + by, 4, 8, mv, rpic, weight, mc_dsp);
                        let mv = sstate.get_cur_blk4(bx / 4 + by + 1).mv[0];
                        if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[part].index()) {
                            wait_for_mb(disp, sstate, xpos + bx + 8, ypos + by + 8, mv, ref_id)?;
                        }
                        do_p_mc(frm, xpos + bx + 4, ypos + by, 4, 8, mv, rpic, weight, mc_dsp);
                    },
                    SubMBType::P4x4 => {
                        for sb_no in 0..4 {
                            let sxpos = xpos + bx + (sb_no & 1) * 4;
                            let sypos = ypos + by + (sb_no & 2) * 2;
                            let sblk_no = (bx / 4 + (sb_no & 1)) + ((by / 4) + (sb_no >> 1)) * 4;
                            let mv = sstate.get_cur_blk4(sblk_no).mv[0];
                            if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[part].index()) {
                                wait_for_mb(disp, sstate, sxpos + 4, sypos + 4, mv, ref_id)?;
                            }
                            do_p_mc(frm, sxpos, sypos, 4, 4, mv, rpic, weight, mc_dsp);
                        }
                    },
                    _ => unreachable!(),
                };
            }
        },
        MBType::B16x16(mode) => {
            let mv0 = sstate.get_cur_blk4(0).mv[0];
            let rpic0 = frame_refs.select_ref_pic(0, mb_info.ref_l0[0].index());
            let mv1 = sstate.get_cur_blk4(0).mv[1];
            let rpic1 = frame_refs.select_ref_pic(1, mb_info.ref_l1[0].index());
            let (weight0, weight1) = get_weights(slice_hdr, frame_refs, mode, weight_mode, mb_info.ref_l0[0], mb_info.ref_l1[0]);
            wait_b_mc(disp, sstate, frame_refs, [mv0, mv1], [mb_info.ref_l0[0], mb_info.ref_l1[0]], xpos, ypos, 16, 16)?;
            do_b_mc(frm, mode, xpos, ypos, 16, 16, mv0, rpic0, &weight0, mv1, rpic1, &weight1, mc_dsp);
        },
        MBType::B16x8(mode0, mode1) | MBType::B8x16(mode0, mode1) => {
            let (pw, ph) = mb_info.mb_type.size();
            let (px, py) = (pw & 8, ph & 8);
            let modes = [mode0, mode1];
            let (mut bx, mut by) = (0, 0);
            for part in 0..2 {
                let blk = if part == 0 { 0 } else { (px / 4) + py };
                let mv0 = sstate.get_cur_blk4(blk).mv[0];
                let rpic0 = frame_refs.select_ref_pic(0, mb_info.ref_l0[part].index());
                let mv1 = sstate.get_cur_blk4(blk).mv[1];
                let rpic1 = frame_refs.select_ref_pic(1, mb_info.ref_l1[part].index());
                let (weight0, weight1) = get_weights(slice_hdr, frame_refs, modes[part], weight_mode, mb_info.ref_l0[part], mb_info.ref_l1[part]);
                wait_b_mc(disp, sstate, frame_refs, [mv0, mv1], [mb_info.ref_l0[part], mb_info.ref_l1[part]], xpos + bx, ypos + by, pw, ph)?;
                do_b_mc(frm, modes[part], xpos + bx, ypos + by, pw, ph, mv0, rpic0, &weight0, mv1, rpic1, &weight1, mc_dsp);
                bx += px;
                by += py;
            }
        },
        MBType::Direct | MBType::BSkip => {
            if let Some(ref_id) = frame_refs.get_ref_id(1, mb_info.ref_l1[0].index()) {
                wait_for_mb(disp, sstate, xpos, ypos, ZERO_MV, ref_id)?;
            }
            let colo_mb_type = frame_refs.get_colocated_info(sstate.mb_x, sstate.mb_y).0.mb_type;
            let is_16x16 = colo_mb_type.is_16x16_ref();

            if is_16x16 {
                let mv = sstate.get_cur_blk4(0).mv;
                let ref_idx = sstate.get_cur_blk8(0).ref_idx;
                let rpic0 = frame_refs.select_ref_pic(0, ref_idx[0].index());
                let rpic1 = frame_refs.select_ref_pic(1, ref_idx[1].index());
                if let Some(ref_id) = frame_refs.get_ref_id(0, mb_info.ref_l0[0].index()) {
                    wait_for_mb(disp, sstate, xpos + 16, ypos + 16, mv[0], ref_id)?;
                }
                if let Some(ref_id) = frame_refs.get_ref_id(1, mb_info.ref_l1[0].index()) {
                    wait_for_mb(disp, sstate, xpos + 16, ypos + 16, mv[1], ref_id)?;
                }
                let (weight0, weight1) = get_weights(slice_hdr, frame_refs, BMode::Bi, weight_mode, ref_idx[0], ref_idx[1]);
                wait_b_mc(disp, sstate, frame_refs, mv, ref_idx, xpos, ypos, 16, 16)?;
                do_b_mc(frm, BMode::Bi, xpos, ypos, 16, 16, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
            } else {
                for blk4 in 0..16 {
                    let mv = sstate.get_cur_blk4(blk4).mv;
                    let ref_idx = sstate.get_cur_blk8(blk4_to_blk8(blk4)).ref_idx;
                    if let Some(ref_id) = frame_refs.get_ref_id(0, ref_idx[0].index()) {
                        wait_for_mb(disp, sstate, xpos + 16, ypos + 16, mv[0], ref_id)?;
                    }
                    if let Some(ref_id) = frame_refs.get_ref_id(1, ref_idx[1].index()) {
                        wait_for_mb(disp, sstate, xpos + 16, ypos + 16, mv[1], ref_id)?;
                    }
                    let rpic0 = frame_refs.select_ref_pic(0, ref_idx[0].index());
                    let rpic1 = frame_refs.select_ref_pic(1, ref_idx[1].index());
                    let (weight0, weight1) = get_weights(slice_hdr, frame_refs, BMode::Bi, weight_mode, ref_idx[0], ref_idx[1]);
                    wait_b_mc(disp, sstate, frame_refs, mv, ref_idx, xpos + (blk4 & 3) * 4, ypos + (blk4 >> 2) * 4, 4, 4)?;
                    do_b_mc(frm, BMode::Bi, xpos + (blk4 & 3) * 4, ypos + (blk4 >> 2) * 4, 4, 4, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                }
            }
            sstate.apply_to_blk8(|blk8| { blk8.ref_idx[0].set_direct(); blk8.ref_idx[1].set_direct(); });
        },
        MBType::B8x8 => {
            for part in 0..4 {
                let ridx = sstate.get_cur_blk8(part).ref_idx;
                let rpic0 = frame_refs.select_ref_pic(0, ridx[0].index());
                let rpic1 = frame_refs.select_ref_pic(1, ridx[1].index());
                let subtype = mb_info.sub_mb_type[part];
                let blk8 = (part & 1) * 2 + (part & 2) * 4;
                let mut bx = (part & 1) * 8;
                let mut by = (part & 2) * 4;
                match subtype {
                    SubMBType::Direct8x8 => {
                        for blk in 0..4 {
                            let mv = sstate.get_cur_blk4(bx / 4 + (by / 4) * 4).mv;
                            let ref_idx = sstate.get_cur_blk8(bx / 8 + (by / 8) * 2).ref_idx;
                            let rpic0 = frame_refs.select_ref_pic(0, ref_idx[0].index());
                            let rpic1 = frame_refs.select_ref_pic(1, ref_idx[1].index());
                            let (weight0, weight1) = get_weights(slice_hdr, frame_refs, BMode::Bi, weight_mode, ref_idx[0], ref_idx[1]);
                            wait_b_mc(disp, sstate, frame_refs, mv, ref_idx, xpos + bx, ypos + by, 4, 4)?;
                            do_b_mc(frm, BMode::Bi, xpos + bx, ypos + by, 4, 4, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                            bx += 4;
                            if blk == 1 {
                                bx -= 8;
                                by += 4;
                            }
                        }
                        sstate.get_cur_blk8(part).ref_idx[0].set_direct();
                        sstate.get_cur_blk8(part).ref_idx[1].set_direct();
                    },
                    SubMBType::B8x8(mode) => {
                        let mv = sstate.get_cur_blk4(blk8).mv;
                        let (weight0, weight1) = get_weights(slice_hdr, frame_refs, mode, weight_mode, ridx[0], ridx[1]);
                        wait_b_mc(disp, sstate, frame_refs, mv, ridx, xpos + bx, ypos + by, 8, 8)?;
                        do_b_mc(frm, mode, xpos + bx, ypos + by, 8, 8, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                    },
                    SubMBType::B8x4(mode) | SubMBType::B4x8(mode) => {
                        let (weight0, weight1) = get_weights(slice_hdr, frame_refs, mode, weight_mode, ridx[0], ridx[1]);
                        let (pw, ph) = subtype.size();
                        let mv = sstate.get_cur_blk4(blk8).mv;
                        wait_b_mc(disp, sstate, frame_refs, mv, ridx, xpos + bx, ypos + by, pw, ph)?;
                        do_b_mc(frm, mode, xpos + bx, ypos + by, pw, ph, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                        let addr2 = blk8 + (pw & 4) / 4 + (ph & 4);
                        let mv = sstate.get_cur_blk4(addr2).mv;
                        wait_b_mc(disp, sstate, frame_refs, mv, ridx, xpos + bx + (pw & 4), ypos + by + (ph & 4), pw, ph)?;
                        do_b_mc(frm, mode, xpos + bx + (pw & 4), ypos + by + (ph & 4), pw, ph, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                    },
                    SubMBType::B4x4(mode) => {
                        let (weight0, weight1) = get_weights(slice_hdr, frame_refs, mode, weight_mode, ridx[0], ridx[1]);
                        for i in 0..4 {
                            let addr2 = blk8 + (i & 1) + (i & 2) * 2;
                            let mv = sstate.get_cur_blk4(addr2).mv;
                            wait_b_mc(disp, sstate, frame_refs, mv, ridx, xpos + bx, ypos + by, 4, 4)?;
                            do_b_mc(frm, mode, xpos + bx, ypos + by, 4, 4, mv[0], rpic0, &weight0, mv[1], rpic1, &weight1, mc_dsp);
                            bx += 4;
                            if i == 1 {
                                bx -= 8;
                                by += 4;
                            }
                        }
                    },
                    _ => unreachable!(),
                };
            }
        },
    };
    if !mb_info.mb_type.is_skip() {
        if mb_info.mb_type != MBType::Intra4x4 && mb_info.mb_type != MBType::Intra8x8 {
            add_luma(frm, sstate, mb_info);
        }
        add_chroma(frm, sstate, mb_info);
    }
    Ok(())
}
