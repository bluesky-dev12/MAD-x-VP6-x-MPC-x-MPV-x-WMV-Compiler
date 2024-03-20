use std::sync::Arc;

use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::frame::FrameType;
use nihav_core::io::bitreader::*;

use super::ReadUE;
use super::sets::*;

pub const MAX_FRAMES: usize = 32;

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum SliceType {
    I,
    P,
    B,
    SI,
    SP,
}

impl SliceType {
    pub fn is_intra(self) -> bool {
        matches!(self, SliceType::I | SliceType::SI)
    }
    pub fn is_p(self) -> bool {
        matches!(self, SliceType::P | SliceType::SP)
    }
    pub fn is_b(self) -> bool { self == SliceType::B }
    pub fn is_s(self) -> bool {
        matches!(self, SliceType::SI | SliceType::SP)
    }
    pub fn to_frame_type(self) -> FrameType {
        match self {
            SliceType::I | SliceType::SI => FrameType::I,
            SliceType::P | SliceType::SP => FrameType::P,
            SliceType::B                 => FrameType::B,
        }
    }
}

const SLICE_TYPES: [SliceType; 10] = [
    SliceType::P, SliceType::B, SliceType::I, SliceType::SP, SliceType::SI,
    SliceType::P, SliceType::B, SliceType::I, SliceType::SP, SliceType::SI,
];

#[derive(Clone,Copy,Default)]
pub struct WeightInfo {
    pub luma_weighted:                      bool,
    pub luma_weight:                        i8,
    pub luma_offset:                        i8,
    pub luma_shift:                         u8,
    pub chroma_weighted:                    bool,
    pub chroma_weight:                      [i8; 2],
    pub chroma_offset:                      [i8; 2],
    pub chroma_shift:                       u8,
}

impl WeightInfo {
    pub fn is_weighted(&self) -> bool {
        self.luma_weighted || self.chroma_weighted
    }
}

#[derive(Clone,Copy)]
pub struct ReorderingInfo {
    pub reordering_of_pic_nums_idc:         [u8; MAX_FRAMES],
    pub abs_diff_or_num:                    [u16; MAX_FRAMES],
    pub num_ops:                            usize,
}

#[derive(Clone,Copy)]
pub struct AdaptiveMarking {
    pub memory_management_control_op:       [u8; MAX_FRAMES],
    pub operation_arg:                      [u16; MAX_FRAMES],
    pub operation_arg2:                     [u16; MAX_FRAMES],
    pub num_ops:                            usize,
}

#[derive(Clone)]
pub struct SliceHeader {
    pub first_mb_in_slice:                  usize,
    pub slice_type:                         SliceType,
    pub same_slice_type:                    bool,
    pub pic_parameter_set_id:               u32,
    pub frame_num:                          u16,
    pub field_pic:                          bool,
    pub bottom_field:                       bool,
    pub idr_pic_id:                         u16,
    pub pic_order_cnt_lsb:                  u16,
    pub delta_pic_order_cnt_bottom:         i32,
    pub delta_pic_order_cnt:                [i32; 2],
    pub redundant_pic_cnt:                  u8,
    pub direct_spatial_mv_pred:             bool,
    pub num_ref_idx_active_override:        bool,
    pub num_ref_idx_l0_active:              usize,
    pub num_ref_idx_l1_active:              usize,
    pub ref_pic_list_reordering_l0:         bool,
    pub reordering_list_l0:                 ReorderingInfo,
    pub ref_pic_list_reordering_l1:         bool,
    pub reordering_list_l1:                 ReorderingInfo,
    pub luma_log2_weight_denom:             u8,
    pub chroma_log2_weight_denom:           u8,
    pub weights_l0:                         [WeightInfo; MAX_FRAMES],
    pub weights_l1:                         [WeightInfo; MAX_FRAMES],
    pub no_output_of_prior_pics:            bool,
    pub long_term_reference:                bool,
    pub adaptive_ref_pic_marking_mode:      bool,
    pub adaptive_ref_pic_marking:           AdaptiveMarking,
    pub cabac_init_idc:                     u8,
    pub slice_qp_delta:                     i32,
    pub slice_qp:                           u8,
    pub sp_for_switch:                      bool,
    pub slice_qs_delta:                     i32,
    pub slice_qs:                           u8,
    pub disable_deblocking_filter_idc:      u8,
    pub slice_alpha_c0_offset:              i8,
    pub slice_beta_offset:                  i8,
    pub slice_group_change_cycle:           u32,
}

pub const DEF_WEIGHT_INFO: WeightInfo = WeightInfo {
    luma_weighted:                      false,
    luma_weight:                        0,
    luma_offset:                        0,
    luma_shift:                         0,
    chroma_weighted:                    false,
    chroma_weight:                      [0; 2],
    chroma_offset:                      [0; 2],
    chroma_shift:                       0,
};

impl SliceHeader {
    pub fn get_weight(&self, list_id: u8, idx: usize) -> WeightInfo {
        if list_id == 0 {
            if idx < self.num_ref_idx_l0_active {
                self.weights_l0[idx]
            } else {
                DEF_WEIGHT_INFO
            }
        } else {
            if idx < self.num_ref_idx_l1_active {
                self.weights_l1[idx]
            } else {
                DEF_WEIGHT_INFO
            }
        }
    }
}

pub fn parse_slice_header_minimal(br: &mut BitReader) -> DecoderResult<(usize, SliceType)> {
    let first_mb_in_slice                           = br.read_ue()? as usize;
    let stype                                       = br.read_ue_lim(SLICE_TYPES.len() as u32 - 1)?;
    let slice_type = SLICE_TYPES[stype as usize];
    Ok((first_mb_in_slice, slice_type))
}

#[allow(clippy::cognitive_complexity)]
#[allow(clippy::manual_range_contains)]
pub fn parse_slice_header(br: &mut BitReader, sps_arr: &[Arc<SeqParameterSet>], pps_arr: &[Arc<PicParameterSet>], is_idr: bool, nal_ref_idc: u8) -> DecoderResult<SliceHeader> {
    let mut hdr: SliceHeader = unsafe { std::mem::zeroed() };

    hdr.first_mb_in_slice                           = br.read_ue()? as usize;
    let stype                                       = br.read_ue_lim(SLICE_TYPES.len() as u32 - 1)?;
    hdr.slice_type = SLICE_TYPES[stype as usize];
    hdr.same_slice_type = stype >= 5;
    hdr.pic_parameter_set_id                        = br.read_ue()?;

    let mut pps_ptr = None;
    for pps in pps_arr.iter() {
        if pps.pic_parameter_set_id == hdr.pic_parameter_set_id {
            pps_ptr = Some(pps);
            break;
        }
    }
    validate!(pps_ptr.is_some());
    let pps = pps_ptr.unwrap();
    let mut sps_ptr = None;
    for sps in sps_arr.iter() {
        if sps.seq_parameter_set_id == pps.seq_parameter_set_id {
            sps_ptr = Some(sps);
            break;
        }
    }
    validate!(sps_ptr.is_some());
    let sps = sps_ptr.unwrap();

    hdr.frame_num                                   = br.read(sps.log2_max_frame_num)? as u16;
    if !sps.frame_mbs_only {
        hdr.field_pic                               = br.read_bool()?;
        if hdr.field_pic {
            hdr.bottom_field                        = br.read_bool()?;
        }
    }

    if is_idr {
        hdr.idr_pic_id                              = br.read_ue_lim(65535)? as u16;
    }
    if sps.pic_order_cnt_type == 0 {
        hdr.pic_order_cnt_lsb                       = br.read(sps.log2_max_pic_order_cnt_lsb)? as u16;
        if pps.pic_order_present && !hdr.field_pic {
            hdr.delta_pic_order_cnt_bottom          = br.read_se()?;
        }
    } else if sps.pic_order_cnt_type == 1 && !sps.delta_pic_order_always_zero {
        hdr.delta_pic_order_cnt[0]                  = br.read_se()?;
        if pps.pic_order_present && !hdr.field_pic {
            hdr.delta_pic_order_cnt[1]              = br.read_se()?;
        }
    }
    if pps.redundant_pic_cnt_present {
        hdr.redundant_pic_cnt                       = br.read_ue_lim(127)? as u8;
    }
    if hdr.slice_type.is_b() {
        hdr.direct_spatial_mv_pred                  = br.read_bool()?;
    }
    if !hdr.slice_type.is_intra() {
        hdr.num_ref_idx_active_override             = br.read_bool()?;
        if hdr.num_ref_idx_active_override {
            hdr.num_ref_idx_l0_active               = (br.read_ue_lim(15)? + 1) as usize;
            if hdr.slice_type.is_b() {
                hdr.num_ref_idx_l1_active           = (br.read_ue_lim(15)? + 1) as usize;
            }
        } else {
            hdr.num_ref_idx_l0_active = pps.num_ref_idx_l0_active;
            if hdr.slice_type.is_b() {
                hdr.num_ref_idx_l1_active = pps.num_ref_idx_l1_active;
            }
        }
    }
    parse_ref_pic_list_reordering(&mut hdr, br)?;
    if (pps.weighted_pred && hdr.slice_type.is_p()) ||
        (pps.weighted_bipred_idc == 1 && hdr.slice_type.is_b()) {
        parse_pred_weight_table(&mut hdr, br)?;
    } else {
        for weight in hdr.weights_l0[..hdr.num_ref_idx_l0_active].iter_mut() {
            weight.luma_weighted = false;
            weight.chroma_weighted = false;
        }
        for weight in hdr.weights_l1[..hdr.num_ref_idx_l1_active].iter_mut() {
            weight.luma_weighted = false;
            weight.chroma_weighted = false;
        }
    }
    if nal_ref_idc != 0 {
        if is_idr {
            hdr.no_output_of_prior_pics             = br.read_bool()?;
            hdr.long_term_reference                 = br.read_bool()?;
        } else {
            hdr.adaptive_ref_pic_marking_mode       = br.read_bool()?;
            if hdr.adaptive_ref_pic_marking_mode {
                let mark_info = &mut hdr.adaptive_ref_pic_marking;
                loop {
                    let memory_management_control_op = br.read_ue_lim(6)? as u8;
                    if memory_management_control_op == 0 {
                        break;
                    }
                    if mark_info.num_ops >= mark_info.memory_management_control_op.len() {
                        return Err(DecoderError::NotImplemented);
                    }
                    mark_info.memory_management_control_op[mark_info.num_ops] = memory_management_control_op;
                    mark_info.operation_arg[mark_info.num_ops] = match memory_management_control_op {
                            1 | 3 => {
                                let difference_of_pic_nums = br.read_ue()? + 1;
                                difference_of_pic_nums as u16
                            },
                            2 => {
                                let long_term_pic_num = br.read_ue_lim(65535)?;
                                long_term_pic_num as u16
                            },
                            6 => {
                                let long_term_frame_idx = br.read_ue_lim(65536)?;
                                long_term_frame_idx as u16
                            },
                            4 => {
                                let max_long_term_frame_idx_plus1 = br.read_ue()?;
                                max_long_term_frame_idx_plus1 as u16
                            },
                            _ => 0,
                        };
                    mark_info.operation_arg2[mark_info.num_ops] = if memory_management_control_op == 3 {
                            let long_term_frame_idx = br.read_ue_lim(65536)?;
                            long_term_frame_idx as u16
                        } else {
                            0
                        };
                    mark_info.num_ops += 1;
                }
            }
        }
    }
    if pps.entropy_coding_mode && !hdr.slice_type.is_intra() {
        hdr.cabac_init_idc                          = br.read_ue_lim(2)? as u8;
    }
    hdr.slice_qp_delta                              = br.read_se()?;
    let qp = i32::from(pps.pic_init_qp) + hdr.slice_qp_delta;
    validate!(qp >= 0 && qp <= 51);
    hdr.slice_qp = qp as u8;
    if hdr.slice_type.is_s() {
        if hdr.slice_type == SliceType::SP {
            hdr.sp_for_switch                       = br.read_bool()?;
        }
        hdr.slice_qs_delta                          = br.read_se()?;
        let qs = i32::from(pps.pic_init_qs) + hdr.slice_qs_delta;
        validate!(qs >= 0 && qs <= 51);
        hdr.slice_qs = qs as u8;
    }
    if pps.deblocking_filter_control_present {
        hdr.disable_deblocking_filter_idc           = br.read_ue_lim(2)? as u8;
        if hdr.disable_deblocking_filter_idc != 1 {
            let val                                 = br.read_se()?;
            validate!(val >= -6 && val <= 6);
            hdr.slice_alpha_c0_offset = val as i8 * 2;
            let val                                 = br.read_se()?;
            validate!(val >= -6 && val <= 6);
            hdr.slice_beta_offset = val as i8 * 2;
        }
    }
    if pps.num_slice_groups > 1 && pps.slice_group_map_type >= 3 && pps.slice_group_map_type <= 5 {
        hdr.slice_group_change_cycle                = br.read_ue()?;
    }

    Ok(hdr)
}

fn parse_ref_pic_list_reordering(hdr: &mut SliceHeader, br: &mut BitReader) -> DecoderResult<()> {
    if !hdr.slice_type.is_intra() {
        hdr.ref_pic_list_reordering_l0              = br.read_bool()?;
        let reord_list = &mut hdr.reordering_list_l0;
        reord_list.num_ops = 0;
        if hdr.ref_pic_list_reordering_l0 {
            loop {
                let reordering_of_pic_nums_idc      = br.read_ue_lim(3)?;
                if reordering_of_pic_nums_idc == 3 {
                    break;
                }
                validate!(reord_list.num_ops < MAX_FRAMES);
                reord_list.reordering_of_pic_nums_idc[reord_list.num_ops] = reordering_of_pic_nums_idc as u8;
                if reordering_of_pic_nums_idc != 2 {
                    let abs_diff_pic_num            = br.read_ue()? + 1;
                    reord_list.abs_diff_or_num[reord_list.num_ops] = abs_diff_pic_num as u16;
                } else {
                    let long_term_pic_num           = br.read_ue()?;
                    reord_list.abs_diff_or_num[reord_list.num_ops] = long_term_pic_num as u16;
                }
                reord_list.num_ops += 1;
            }
            validate!(reord_list.num_ops > 0);
        }
    }
    if hdr.slice_type.is_b() {
        hdr.ref_pic_list_reordering_l1              = br.read_bool()?;
        let reord_list = &mut hdr.reordering_list_l1;
        reord_list.num_ops = 0;
        if hdr.ref_pic_list_reordering_l1 {
            loop {
                let reordering_of_pic_nums_idc      = br.read_ue_lim(3)?;
                if reordering_of_pic_nums_idc == 3 {
                    break;
                }
                validate!(reord_list.num_ops < MAX_FRAMES);
                reord_list.reordering_of_pic_nums_idc[reord_list.num_ops] = reordering_of_pic_nums_idc as u8;
                if reordering_of_pic_nums_idc != 2 {
                    let abs_diff_pic_num            = br.read_ue()? + 1;
                    reord_list.abs_diff_or_num[reord_list.num_ops] = abs_diff_pic_num as u16;
                } else {
                    let long_term_pic_num           = br.read_ue()?;
                    reord_list.abs_diff_or_num[reord_list.num_ops] = long_term_pic_num as u16;
                }
                reord_list.num_ops += 1;
            }
            validate!(reord_list.num_ops > 0);
        }
    }
    Ok(())
}

fn parse_pred_weight_table(hdr: &mut SliceHeader, br: &mut BitReader) -> DecoderResult<()> {
    hdr.luma_log2_weight_denom                      = br.read_ue_lim(7)? as u8;
    hdr.chroma_log2_weight_denom                    = br.read_ue_lim(7)? as u8;
    for weight in hdr.weights_l0[..hdr.num_ref_idx_l0_active].iter_mut() {
        weight.luma_weighted                        = br.read_bool()?;
        if weight.luma_weighted {
            let w                                   = br.read_se()?;
            validate!(w >= -128 && w <= 127);
            weight.luma_weight = w as i8;
            let offset                              = br.read_se()?;
            validate!(offset >= -128 && offset <= 127);
            weight.luma_offset = offset as i8;
        }
        weight.luma_shift = hdr.luma_log2_weight_denom;

        weight.chroma_weighted                      = br.read_bool()?;
        if weight.chroma_weighted {
            for i in 0..2 {
                let w                               = br.read_se()?;
                validate!(w >= -128 && w <= 127);
                weight.chroma_weight[i] = w as i8;
                let offset                          = br.read_se()?;
                validate!(offset >= -128 && offset <= 127);
                weight.chroma_offset[i] = offset as i8;
            }
        }
        weight.chroma_shift = hdr.chroma_log2_weight_denom;
    }
    for weight in hdr.weights_l1[..hdr.num_ref_idx_l1_active].iter_mut() {
        weight.luma_weighted                        = br.read_bool()?;
        if weight.luma_weighted {
            let w                                   = br.read_se()?;
            validate!(w >= -128 && w <= 127);
            weight.luma_weight = w as i8;
            let offset                              = br.read_se()?;
            validate!(offset >= -128 && offset <= 127);
            weight.luma_offset = offset as i8;
        }
        weight.luma_shift = hdr.luma_log2_weight_denom;

        weight.chroma_weighted                      = br.read_bool()?;
        if weight.chroma_weighted {
            for i in 0..2 {
                let w                               = br.read_se()?;
                validate!(w >= -128 && w <= 127);
                weight.chroma_weight[i] = w as i8;
                let offset                          = br.read_se()?;
                validate!(offset >= -128 && offset <= 127);
                weight.chroma_offset[i] = offset as i8;
            }
        }
        weight.chroma_shift = hdr.chroma_log2_weight_denom;
    }

    Ok(())
}
