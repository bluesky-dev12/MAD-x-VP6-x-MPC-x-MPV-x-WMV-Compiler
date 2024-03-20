use std::sync::Arc;

use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::io::bitreader::*;

use super::ReadUE;

#[derive(Clone)]
pub struct SeqParameterSet {
    pub profile_idc:                        u8,
    pub high_profile:                       bool,
    pub constraint_set0:                    bool,
    pub constraint_set1:                    bool,
    pub constraint_set2:                    bool,
    pub level_idc:                          u8,
    pub seq_parameter_set_id:               u32,
    pub chroma_format_idc:                  u8,
    pub separate_colour_plane:              bool,
    pub bit_depth_luma:                     u8,
    pub bit_depth_chroma:                   u8,
    pub qpprime_y_zero_transform_bypass:    bool,
    pub seq_scaling_matrix_present:         bool,
    pub scaling_list_4x4:                   [[u8; 16]; 6],
    pub scaling_list_8x8:                   [[u8; 64]; 6],
    pub log2_max_frame_num:                 u8,
    pub pic_order_cnt_type:                 u8,
    pub log2_max_pic_order_cnt_lsb:         u8,
    pub delta_pic_order_always_zero:        bool,
    pub offset_for_non_ref_pic:             i32,
    pub offset_for_top_to_bottom_field:     i32,
    pub num_ref_frames_in_pic_order_cnt_cycle:  usize,
    pub offset_for_ref_frame:               [i32; 256],
    pub num_ref_frames:                     usize,
    pub gaps_in_frame_num_value_allowed:    bool,
    pub pic_width_in_mbs:                   usize,
    pub pic_height_in_mbs:                  usize,
    pub frame_mbs_only:                     bool,
    pub mb_adaptive_frame_field:            bool,
    pub direct_8x8_inference:               bool,
    pub frame_cropping:                     bool,
    pub frame_crop_left_offset:             usize,
    pub frame_crop_right_offset:            usize,
    pub frame_crop_top_offset:              usize,
    pub frame_crop_bottom_offset:           usize,
    pub vui_parameters_present:             bool,
}

pub fn is_high_profile(profile: u8) -> bool {
    matches!(profile, 100 | 110 | 122 | 244 | 44 | 83 | 86 | 118 | 128 | 138 | 139 | 134 | 125)
}

#[allow(clippy::cognitive_complexity)]
pub fn parse_sps(src: &[u8]) -> DecoderResult<SeqParameterSet> {
    let mut br = BitReader::new(src, BitReaderMode::BE);
    let mut sps: SeqParameterSet = unsafe { std::mem::zeroed() };

    sps.profile_idc                                 = br.read(8)? as u8;
    sps.constraint_set0                             = br.read_bool()?;
    sps.constraint_set1                             = br.read_bool()?;
    sps.constraint_set2                             = br.read_bool()?;
    let reserved                                    = br.read(5)?;
    validate!(reserved == 0);
    sps.level_idc                                   = br.read(8)? as u8;
    sps.seq_parameter_set_id                        = br.read_ue()?;
    sps.high_profile = is_high_profile(sps.profile_idc);
    if sps.high_profile {
        sps.chroma_format_idc                       = br.read_ue_lim(3)? as u8;
        if sps.chroma_format_idc == 3 {
            sps.separate_colour_plane               = br.read_bool()?;
        }
        sps.bit_depth_luma                          = br.read_ue_lim(6)? as u8 + 8;
        sps.bit_depth_chroma                        = br.read_ue_lim(6)? as u8 + 8;
        sps.qpprime_y_zero_transform_bypass         = br.read_bool()?;
        sps.seq_scaling_matrix_present              = br.read_bool()?;
        if sps.seq_scaling_matrix_present {
            let mut slist_present = [false; 6];
            for (i, slist) in sps.scaling_list_4x4.iter_mut().enumerate() {
                slist_present[i]                    = br.read_bool()?;
                if slist_present[i] {
                    parse_scaling_list(&mut br, slist, i < 3)?;
                }
            }
            for i in 1..6 {
                if i == 3 {
                    continue;
                }
                if !slist_present[i] {
                    sps.scaling_list_4x4[i] = sps.scaling_list_4x4[i - 1];
                }
            }

            let mut slist_present = [false; 6];
            let num_8x8 = if sps.chroma_format_idc != 3 { 2 } else { 6 };
            for (i, slist) in sps.scaling_list_8x8.iter_mut().take(num_8x8).enumerate() {
                slist_present[i]                    = br.read_bool()?;
                if slist_present[i] {
                    parse_scaling_list(&mut br, slist, (i & 1) == 0)?;
                }
            }
            if num_8x8 > 2 {
                for i in 2..6 {
                    if !slist_present[i] {
                        sps.scaling_list_8x8[i] = sps.scaling_list_8x8[i - 2];
                    }
                }
            }
        } else {
            sps.scaling_list_4x4 = [[16; 16]; 6];
            sps.scaling_list_8x8 = [[16; 64]; 6];
        }
    } else {
        sps.chroma_format_idc = 1;
        sps.bit_depth_luma = 8;
        sps.bit_depth_chroma = 8;
    }
    sps.log2_max_frame_num                          = (br.read_ue_lim(12)? + 4) as u8;
    sps.pic_order_cnt_type                          = br.read_ue_lim(2)? as u8;
    match sps.pic_order_cnt_type {
        0 => {
            sps.log2_max_pic_order_cnt_lsb          = (br.read_ue_lim(12)? + 4) as u8;
        },
        1 => {
            sps.delta_pic_order_always_zero         = br.read_bool()?;
            sps.offset_for_non_ref_pic              = br.read_se()?;
            sps.offset_for_top_to_bottom_field      = br.read_se()?;
            sps.num_ref_frames_in_pic_order_cnt_cycle   = br.read_ue_lim(255)? as usize;
            for offset in sps.offset_for_ref_frame[..sps.num_ref_frames_in_pic_order_cnt_cycle].iter_mut() {
                *offset                             = br.read_se()?;
            }
        },
        _ => {},
    };
    sps.num_ref_frames                              = br.read_ue()? as usize;
    validate!(sps.num_ref_frames <= super::slice::MAX_FRAMES);
    sps.gaps_in_frame_num_value_allowed             = br.read_bool()?;
    sps.pic_width_in_mbs                            = (br.read_ue()? + 1) as usize;
    sps.pic_height_in_mbs                           = (br.read_ue()? + 1) as usize;
    validate!(sps.pic_width_in_mbs <= 1024 && sps.pic_height_in_mbs <= 1024);
    sps.frame_mbs_only                              = br.read_bool()?;
    if !sps.frame_mbs_only {
        sps.mb_adaptive_frame_field                 = br.read_bool()?;
    }
    sps.direct_8x8_inference                        = br.read_bool()?;
    sps.frame_cropping                              = br.read_bool()?;
    if sps.frame_cropping {
        sps.frame_crop_left_offset                  = br.read_ue()? as usize;
        sps.frame_crop_right_offset                 = br.read_ue()? as usize;
        sps.frame_crop_top_offset                   = br.read_ue()? as usize;
        sps.frame_crop_bottom_offset                = br.read_ue()? as usize;
        let l = sps.frame_crop_left_offset * 2;
        let r = sps.pic_width_in_mbs * 16 - sps.frame_crop_right_offset * 2;
        let t = sps.frame_crop_top_offset * 2;
        let d = sps.pic_height_in_mbs * 16 - sps.frame_crop_bottom_offset * 2;
        validate!(l < r && t < d);
    }
    sps.vui_parameters_present                      = br.read_bool()?;
    if sps.vui_parameters_present {
        // xxx: vui is ignored for now
        if br.read_bool()? {
            let idc = br.read(8)?;
            if idc == 255 {
                br.read(16)?;
                br.read(16)?;
            }
        }
        if br.read_bool()? {
            br.read_bool()?;
        }
        if br.read_bool()? {
            br.read(3)?;
            br.read_bool()?;
            if br.read_bool()? {
                br.read(8)?;
                br.read(8)?;
                br.read(8)?;
            }
        }
        if br.read_bool()? {
            br.read_ue()?;
            br.read_ue()?;
        }
        if br.read_bool()? {
            br.read(32)?;
            br.read(32)?;
            br.read_bool()?;
        }
        let nal_hrd_parameters_present = br.read_bool()?;
        if nal_hrd_parameters_present {
            skip_hrd_parameters(&mut br)?;
        }
        let vcl_hrd_parameters_present = br.read_bool()?;
        if vcl_hrd_parameters_present {
            skip_hrd_parameters(&mut br)?;
        }
        if nal_hrd_parameters_present || vcl_hrd_parameters_present {
            br.read_bool()?;
        }
        br.read_bool()?;
        if br.read_bool()? {
            br.read_bool()?;
            br.read_ue()?;
            br.read_ue()?;
            br.read_ue()?;
            br.read_ue()?;
            br.read_ue()?;
            br.read_ue()?;
        }
    }

    Ok(sps)
}

fn parse_scaling_list(br: &mut BitReader, slist: &mut[u8], is_intra: bool) -> DecoderResult<()> {
    const DEFAULT_INTRA_4X4: [u8; 16] = [
        6, 13, 13, 20, 20, 20, 28, 28, 28, 28, 32, 32, 32, 37, 37, 42
    ];
    const DEFAULT_INTER_4X4: [u8; 16] = [
        10, 14, 14, 20, 20, 20, 24, 24, 24, 24, 27, 27, 27, 30, 30, 34
    ];
    const DEFAULT_INTRA_8X8: [u8; 64] = [
         6, 10, 10, 13, 11, 13, 16, 16, 16, 16, 18, 18, 18, 18, 18, 23,
        23, 23, 23, 23, 23, 25, 25, 25, 25, 25, 25, 25, 27, 27, 27, 27,
        27, 27, 27, 27, 29, 29, 29, 29, 29, 29, 29, 31, 31, 31, 31, 31,
        31, 33, 33, 33, 33, 33, 36, 36, 36, 36, 38, 38, 38, 40, 40, 42
    ];
    const DEFAULT_INTER_8X8: [u8; 64] = [
         9, 13, 13, 15, 13, 15, 17, 17, 17, 17, 19, 19, 19, 19, 19, 21,
        21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 24, 24, 24, 24,
        24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 27, 27, 27, 27, 27,
        27, 28, 28, 28, 28, 28, 30, 30, 30, 30, 32, 32, 32, 33, 33, 35
    ];
    let mut last_scale = 8u8;
    let mut next_scale = 8u8;
    let mut use_default = false;
    for (j, elem) in slist.iter_mut().enumerate() {
        if next_scale != 0 {
            let delta                   = br.read_se()?;
            next_scale = last_scale.wrapping_add(delta as u8);
            if (j == 0) && (next_scale == 0) {
                use_default = true;
                break;
            }
        }
        *elem = if next_scale == 0 { last_scale } else { next_scale };
        last_scale = *elem;
    }
    if use_default {
        match (slist.len(), is_intra) {
            (16, true)  => slist.copy_from_slice(&DEFAULT_INTRA_4X4),
            (16, false) => slist.copy_from_slice(&DEFAULT_INTER_4X4),
            (64, true)  => slist.copy_from_slice(&DEFAULT_INTRA_8X8),
            (64, false) => slist.copy_from_slice(&DEFAULT_INTER_8X8),
            _ => unreachable!(),
        };
    }
    Ok(())
}

fn skip_hrd_parameters(br: &mut BitReader) -> DecoderResult<()> {
    let cpb_cnt = br.read_ue()? as usize + 1;
    br.read(4)?;
    br.read(4)?;
    for _ in 0..cpb_cnt {
        br.read_ue()?;
        br.read_ue()?;
        br.read_bool()?;
    }
    br.read(5)?;
    br.read(5)?;
    br.read(5)?;
    br.read(5)?;
    Ok(())
}

const MAX_SLICE_GROUPS: usize = 8;

#[derive(Clone)]
pub struct PicParameterSet {
    pub pic_parameter_set_id:               u32,
    pub seq_parameter_set_id:               u32,
    pub entropy_coding_mode:                bool,
    pub pic_order_present:                  bool,
    pub num_slice_groups:                   usize,
    pub slice_group_map_type:               u8,
    pub run_length:                         [u32; MAX_SLICE_GROUPS],
    pub top_left:                           [u32; MAX_SLICE_GROUPS],
    pub bottom_right:                       [u32; MAX_SLICE_GROUPS],
    pub slice_group_change_direction:       bool,
    pub slice_group_change_rate:            u32,
    pub pic_size_in_map_units:              u32,
    pub num_ref_idx_l0_active:              usize,
    pub num_ref_idx_l1_active:              usize,
    pub weighted_pred:                      bool,
    pub weighted_bipred_idc:                u8,
    pub pic_init_qp:                        u8,
    pub pic_init_qs:                        u8,
    pub chroma_qp_index_offset:             i8,
    pub deblocking_filter_control_present:  bool,
    pub constrained_intra_pred:             bool,
    pub redundant_pic_cnt_present:          bool,
    pub transform_8x8_mode:                 bool,
    pub pic_scaling_matrix_present:         bool,
    pub scaling_list_4x4:                   [[u8; 16]; 6],
    pub scaling_list_8x8:                   [[u8; 64]; 6],
    pub second_chroma_qp_index_offset:      i8,
}

pub fn parse_pps(src: &[u8], sps_arr: &[Arc<SeqParameterSet>], full_size: usize) -> DecoderResult<Arc<PicParameterSet>> {
    let mut br = BitReader::new(src, BitReaderMode::BE);
    let mut pps: PicParameterSet = unsafe { std::mem::zeroed() };

    pps.pic_parameter_set_id                        = br.read_ue()?;
    pps.seq_parameter_set_id                        = br.read_ue()?;
    let mut found = false;
    let mut cur_sps = None;
    for sps in sps_arr.iter() {
        if sps.seq_parameter_set_id == pps.seq_parameter_set_id {
            found = true;
            cur_sps = Some(sps);
            break;
        }
    }
    validate!(found);
    let sps = cur_sps.unwrap();
    pps.entropy_coding_mode                         = br.read_bool()?;
    pps.pic_order_present                           = br.read_bool()?;
    pps.num_slice_groups                            = (br.read_ue()? + 1) as usize;
    validate!(pps.num_slice_groups <= MAX_SLICE_GROUPS);
    if pps.num_slice_groups > 1 {
        let smtype                                  = br.read_ue()?;
        validate!(smtype <= 6);
        pps.slice_group_map_type = smtype as u8;
        match pps.slice_group_map_type {
            0 => {
                for elem in pps.run_length[..pps.num_slice_groups].iter_mut() {
                    *elem                           = br.read_ue()?;
                }
            },
            2 => {
                for i in 0..pps.num_slice_groups - 1 {
                    pps.top_left[i]                 = br.read_ue()?;
                    pps.bottom_right[i]             = br.read_ue()?;
                }
            },
            3 | 4 | 5 => {
                pps.slice_group_change_direction    = br.read_bool()?;
                pps.slice_group_change_rate         = br.read_ue()?;
            },
            6 => {
                pps.pic_size_in_map_units           = br.read_ue()? + 1;
                for _ in 0..pps.pic_size_in_map_units {
                    let _slice_group_id             = br.read_ue()?;
                }
            },
            _ => {},
        };
println!("slice mode!");
        return Err(DecoderError::NotImplemented);
    }
    pps.num_ref_idx_l0_active                       = (br.read_ue()? + 1) as usize;
    pps.num_ref_idx_l1_active                       = (br.read_ue()? + 1) as usize;
    pps.weighted_pred                               = br.read_bool()?;
    pps.weighted_bipred_idc                         = br.read(2)? as u8;
    let qp                                          = br.read_se()? + 26;
    validate!(qp > 0 && qp < 52);
    pps.pic_init_qp = qp as u8;
    let qs                                          = br.read_se()? + 26;
    validate!(qs > 0 && qs < 52);
    pps.pic_init_qs = qs as u8;
    let off                                         = br.read_se()?;
    validate!(off >= -12 && off <= 12);
    pps.chroma_qp_index_offset = off as i8;
    pps.deblocking_filter_control_present           = br.read_bool()?;
    pps.constrained_intra_pred                      = br.read_bool()?;
    pps.redundant_pic_cnt_present                   = br.read_bool()?;
    if br.tell() < full_size {
        pps.transform_8x8_mode                      = br.read_bool()?;
        pps.pic_scaling_matrix_present              = br.read_bool()?;
        if pps.pic_scaling_matrix_present {
            let mut slist_present = [false; 6];
            for (i, slist) in pps.scaling_list_4x4.iter_mut().enumerate() {
                slist_present[i]                    = br.read_bool()?;
                if slist_present[i] {
                    parse_scaling_list(&mut br, slist, i < 3)?;
                }
            }
            for i in 1..6 {
                if i == 3 {
                    continue;
                }
                if !slist_present[i] {
                    pps.scaling_list_4x4[i] = pps.scaling_list_4x4[i - 1];
                }
            }

            let mut slist_present = [false; 6];
            let num_8x8 = if !pps.transform_8x8_mode { 0 } else if sps.chroma_format_idc != 3 { 2 } else { 6 };
            for (i, slist) in pps.scaling_list_8x8.iter_mut().take(num_8x8).enumerate() {
                slist_present[i]                    = br.read_bool()?;
                if slist_present[i] {
                    parse_scaling_list(&mut br, slist, (i & 1) == 0)?;
                }
            }
            if num_8x8 > 2 {
                for i in 2..6 {
                    if !slist_present[i] {
                        pps.scaling_list_8x8[i] = pps.scaling_list_8x8[i - 2];
                    }
                }
            }
        } else {
            pps.scaling_list_4x4 = sps.scaling_list_4x4;
            pps.scaling_list_8x8 = sps.scaling_list_8x8;
        }
        let off                                     = br.read_se()?;
        validate!(off >= -12 && off <= 12);
        pps.second_chroma_qp_index_offset = off as i8;
    } else {
        pps.second_chroma_qp_index_offset = pps.chroma_qp_index_offset;
    }

    Ok(Arc::new(pps))
}
