use std::sync::Arc;

use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;

use super::*;

struct H264Decoder {
    info:       NACodecInfoRef,
    width:      usize,
    height:     usize,
    num_mbs:    usize,
    nal_len:    u8,
    sps:        Vec<Arc<SeqParameterSet>>,
    cur_sps:    usize,
    pps:        Vec<Arc<PicParameterSet>>,
    cur_pps:    usize,

    skip_mode:      FrameSkipMode,
    deblock_skip:   bool,

    is_mbaff:   bool,

    cavlc_cb:   CAVLCTables,

    sstate:     SliceState,

    cur_pic:    Option<PictureInfo>,
    cur_id:     u16,
    has_pic:    bool,
    frame_refs: FrameRefs,

    temporal_mv:    bool,
    deblock_mode:   u8,
    lf_alpha:       i8,
    lf_beta:        i8,
    is_s:           bool,

    ipcm_buf:   [u8; 256 + 64 + 64],

    mc_dsp:     H264MC,

    transform_8x8_mode: bool,
}

impl H264Decoder {
    fn new() -> Self {
        let avg_vi = NAVideoInfo { width: 32, height: 32, flipped: false, format: YUV420_FORMAT, bits: 12 };
        let avg_buf = alloc_video_buffer(avg_vi, 4).unwrap().get_vbuf().unwrap();
        H264Decoder{
            info:       NACodecInfoRef::default(),
            width:      0,
            height:     0,
            num_mbs:    0,
            nal_len:    0,
            sps:        Vec::with_capacity(1),
            cur_sps:    0,
            pps:        Vec::with_capacity(3),
            cur_pps:    0,

            skip_mode:      FrameSkipMode::default(),
            deblock_skip:   false,

            is_mbaff:   false,

            cavlc_cb:   CAVLCTables::new(),

            sstate:     SliceState::new(),
            cur_pic:    None,
            cur_id:     0,
            has_pic:    false,
            frame_refs: FrameRefs::new(),

            temporal_mv:        false,
            deblock_mode:       0,
            lf_alpha:           0,
            lf_beta:            0,
            is_s:               false,

            ipcm_buf:   [0; 256 + 64 + 64],

            mc_dsp:     H264MC::new(avg_buf),

            transform_8x8_mode: false,
        }
    }
    fn handle_nal(&mut self, src: &[u8], supp: &mut NADecoderSupport, skip_decoding: bool) -> DecoderResult<()> {
        validate!(!src.is_empty());
        validate!((src[0] & 0x80) == 0);
        let nal_ref_idc   = src[0] >> 5;
        let nal_unit_type = src[0] & 0x1F;

        let mut full_size = src.len() * 8;
        for &byte in src.iter().rev() {
            if byte == 0 {
                full_size -= 8;
            } else {
                full_size -= (byte.trailing_zeros() + 1) as usize;
                break;
            }
        }
        validate!(full_size > 0);
        match nal_unit_type {
             1 | 5 if !skip_decoding => {
                let is_idr = nal_unit_type == 5;
                let mut br = BitReader::new(&src[..(full_size + 7)/8], BitReaderMode::BE);
                                                    br.skip(8)?;

                let slice_hdr = parse_slice_header(&mut br, self.sps.as_slice(), self.pps.as_slice(), is_idr, nal_ref_idc)?;
                validate!(br.tell() < full_size);
                let full_id;
                if slice_hdr.first_mb_in_slice == 0 {
                    validate!(self.cur_pic.is_none());
                    for (i, pps) in self.pps.iter().enumerate() {
                        if pps.pic_parameter_set_id == slice_hdr.pic_parameter_set_id {
                            self.cur_pps = i;
                            break;
                        }
                    }
                    for (i, sps) in self.sps.iter().enumerate() {
                        if sps.seq_parameter_set_id == self.pps[self.cur_pps].seq_parameter_set_id {
                            self.cur_sps = i;
                            break;
                        }
                    }

                    full_id = self.frame_refs.calc_picture_num(&slice_hdr, is_idr, nal_ref_idc, &self.sps[self.cur_sps]);

                    let sps = &self.sps[self.cur_sps];
                    if sps.chroma_format_idc != 1 || sps.bit_depth_luma != 8 || sps.bit_depth_chroma != 8 {
println!(" chroma fmt {} bits {}/{}", sps.chroma_format_idc, sps.bit_depth_luma, sps.bit_depth_chroma);
                        return Err(DecoderError::NotImplemented);
                    }
                    //let pps = &self.pps[self.cur_pps];

                    if is_idr {
                        self.frame_refs.clear_refs();
                    }

                    self.width  = sps.pic_width_in_mbs  << 4;
                    self.height = sps.pic_height_in_mbs << 4;
                    self.num_mbs = sps.pic_width_in_mbs * sps.pic_height_in_mbs;
                    self.mc_dsp.set_dimensions(self.width, self.height);

                    self.is_mbaff = sps.mb_adaptive_frame_field && !slice_hdr.field_pic;
                    if self.is_mbaff {
println!("MBAFF");
                        return Err(DecoderError::NotImplemented);
                    }
                    if !sps.frame_mbs_only {
println!("PAFF?");
                        return Err(DecoderError::NotImplemented);
                    }

//if slice_hdr.slice_type.is_b() { return Ok(()); }
                    self.cur_id = full_id as u16;
                } else {
                    if let Some(ref mut pic) = self.cur_pic {
                        validate!(pic.cur_mb == slice_hdr.first_mb_in_slice);
                        let new_type = slice_hdr.slice_type.to_frame_type();
                        pic.pic_type = match (pic.pic_type, new_type) {
                                (FrameType::I, _) => new_type,
                                (_, FrameType::B) => FrameType::B,
                                _ => pic.pic_type,
                            };
                        full_id = pic.full_id;
                    } else {
                        return Ok(());//Err(DecoderError::InvalidData);
                    }
                    validate!(self.cur_pps < self.pps.len() && self.pps[self.cur_pps].pic_parameter_set_id == slice_hdr.pic_parameter_set_id);
                }

                let sps = &self.sps[self.cur_sps];
                let pps = &self.pps[self.cur_pps];

                self.temporal_mv = !slice_hdr.direct_spatial_mv_pred;
                self.is_s = slice_hdr.slice_type == SliceType::SI || slice_hdr.slice_type == SliceType::SP;
                self.deblock_mode = slice_hdr.disable_deblocking_filter_idc;
                self.lf_alpha = slice_hdr.slice_alpha_c0_offset;
                self.lf_beta  = slice_hdr.slice_beta_offset;

                self.frame_refs.select_refs(sps, &slice_hdr, full_id);

                if slice_hdr.adaptive_ref_pic_marking_mode {
                    self.frame_refs.apply_adaptive_marking(&slice_hdr.adaptive_ref_pic_marking, slice_hdr.frame_num, 1 << self.sps[self.cur_sps].log2_max_frame_num)?;
                }
                if slice_hdr.first_mb_in_slice == 0 {
                    let ret = supp.pool_u8.get_free();
                    if ret.is_none() {
                        return Err(DecoderError::AllocError);
                    }
                    let tmp_vinfo = NAVideoInfo::new(self.width, self.height, false, YUV420_FORMAT);
                    let mut buf = ret.unwrap();
                    if buf.get_info() != tmp_vinfo {
                        supp.pool_u8.reset();
                        supp.pool_u8.prealloc_video(tmp_vinfo, 4)?;
                        let ret = supp.pool_u8.get_free();
                        if ret.is_none() {
                            return Err(DecoderError::AllocError);
                        }
                        buf = ret.unwrap();
                    }
                    self.cur_pic = Some(PictureInfo {
                            id: slice_hdr.frame_num,
                            full_id,
                            user_id:    full_id,
                            time:       NATimeInfo::new(None, None, None, 0, 0),
                            pic_type: slice_hdr.slice_type.to_frame_type(),
                            buf,
                            cur_mb: 0,
                            is_ref: nal_ref_idc != 0,
                            is_idr,
                            long_term: get_long_term_id(is_idr, &slice_hdr),
                            mv_info: NABufferRef::new(FrameMV::new(sps.pic_width_in_mbs, sps.pic_height_in_mbs)),
                        });
                }

                self.transform_8x8_mode = pps.transform_8x8_mode;

                self.sstate.reset(sps.pic_width_in_mbs, sps.pic_height_in_mbs, slice_hdr.first_mb_in_slice);

                let mut dst_pic = if let Some(ref pic) = self.cur_pic {
                        pic.clone()
                    } else {
                        return Err(DecoderError::InvalidData);
                    };
                let mut dst_frm = NASimpleVideoFrame::from_video_buf(&mut dst_pic.buf).unwrap();
                let dst_mv_info = &mut dst_pic.mv_info;
                if !pps.entropy_coding_mode {
                    self.has_pic = self.decode_slice_cavlc(&mut br, &slice_hdr, full_size, &mut dst_frm, dst_mv_info)?;
                } else {
                    br.align();
                    let start = br.tell() / 8;
                    let csrc = &src[start..];
                    validate!(csrc.len() >= 2);
                    let mut cabac = CABAC::new(csrc, slice_hdr.slice_type, slice_hdr.slice_qp, slice_hdr.cabac_init_idc as usize)?;
                    self.has_pic = self.decode_slice_cabac(&mut cabac, &slice_hdr, &mut dst_frm, dst_mv_info)?;
                }
            },
             2 => { // slice data partition A
                //slice header
                //slice id = read_ue()
                //cat 2 slice data (all but MB layer residual)
                return Err(DecoderError::NotImplemented);
            },
             3 => { // slice data partition B
                //slice id = read_ue()
                //if pps.redundant_pic_cnt_present { redundant_pic_cnt = read_ue() }
                //cat 3 slice data (MB layer residual)
                return Err(DecoderError::NotImplemented);
            },
             4 => { // slice data partition C
                //slice id = read_ue()
                //if pps.redundant_pic_cnt_present { redundant_pic_cnt = read_ue() }
                //cat 4 slice data (MB layer residual)
                return Err(DecoderError::NotImplemented);
            },
             6 => {}, //SEI
             7 => {
                let sps = parse_sps(&src[1..])?;
                self.sps.push(Arc::new(sps));
            },
             8 => {
                validate!(full_size >= 8 + 16);
                let pps = parse_pps(&src[1..], &self.sps, full_size - 8)?;
                let mut found = false;
                for stored_pps in self.pps.iter_mut() {
                    if stored_pps.pic_parameter_set_id == pps.pic_parameter_set_id {
                        *stored_pps = Arc::clone(&pps);
                        found = true;
                        break;
                    }
                }
                if !found {
                    self.pps.push(pps);
                }
            },
             9 => { // access unit delimiter
            },
            10 => {}, //end of sequence
            11 => {}, //end of stream
            12 => {}, //filler
            _  => {},
        };

        Ok(())
    }
    fn pred_mv(sstate: &mut SliceState, frame_refs: &SimplifiedSliceRefs, mb_info: &mut CurrentMBInfo, cur_id: u16, temporal_mv: bool, direct_8x8: bool) {
        let mb_type = mb_info.mb_type;
        if !mb_type.is_4x4() {
            let (pw, ph) = mb_type.size();
            let mut xoff = 0;
            let mut yoff = 0;
            if mb_type == MBType::Direct || mb_type == MBType::BSkip {
                sstate.predict_direct_mb(frame_refs, temporal_mv, direct_8x8, cur_id);
            }
            for part in 0..mb_type.num_parts() {
                if !mb_type.is_l1(part) {
                    match mb_type {
                        MBType::PSkip => sstate.predict_pskip(),
                        MBType::BSkip | MBType::Direct => {
                        },
                        _ => {
                            sstate.predict(xoff, yoff, pw, ph, 0,
 mb_info.mv_l0[part], mb_info.ref_l0[part]);
                        },
                    };
                }
                if !mb_type.is_l0(part) && mb_type != MBType::BSkip && mb_type != MBType::Direct {
                    sstate.predict(xoff, yoff, pw, ph, 1, mb_info.mv_l1[part], mb_info.ref_l1[part]);
                }
                if pw != 16 {
                    xoff += pw;
                } else {
                    yoff += ph;
                }
            }
        } else {
            for part in 0..4 {
                let sub_type = mb_info.sub_mb_type[part];
                let mut xoff = (part & 1) * 8;
                let mut yoff = (part & 2) * 4;
                let orig_x = xoff;
                let (pw, ph) = sub_type.size();
                for subpart in 0..sub_type.num_parts() {
                    if sub_type != SubMBType::Direct8x8 {
                        if !sub_type.is_l1() {
                            sstate.predict(xoff, yoff, pw, ph, 0, mb_info.mv_l0[part * 4 + subpart], mb_info.ref_l0[part]);
                        }
                        if !sub_type.is_l0() {
                            sstate.predict(xoff, yoff, pw, ph, 1, mb_info.mv_l1[part * 4 + subpart], mb_info.ref_l1[part]);
                        }
                    } else {
                        for sblk in 0..4 {
                            sstate.predict_direct_sub(frame_refs, temporal_mv, direct_8x8, cur_id, (xoff / 4) + (sblk & 1) + (yoff / 4) * 4 + (sblk & 2) * 2);
                        }
                    }
                    xoff += pw;
                    if xoff == orig_x + 8 {
                        xoff -= 8;
                        yoff += ph;
                    }
                }
            }
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn handle_macroblock(&mut self, slice_hdr: &SliceHeader, mb_info: &mut CurrentMBInfo, slice_refs: &SimplifiedSliceRefs, frm: &mut NASimpleVideoFrame<u8>, mv_info: &mut FrameMV) {
        let pps = &self.pps[self.cur_pps];

        let qp_y = mb_info.qp_y;
        let qpr = ((qp_y as i8) + pps.chroma_qp_index_offset).max(0).min(51) as usize;
        let qp_u = CHROMA_QUANTS[qpr];
        let qpb = ((qp_y as i8) + pps.second_chroma_qp_index_offset).max(0).min(51) as usize;
        let qp_v = CHROMA_QUANTS[qpb];

        let tx_bypass = qp_y == 0 && self.sps[self.cur_sps].qpprime_y_zero_transform_bypass;

        self.sstate.get_cur_mb().mb_type = mb_info.mb_type.into();
        if mb_info.mb_type != MBType::PCM {
            self.sstate.get_cur_mb().qp_y = qp_y;
            self.sstate.get_cur_mb().qp_u = qp_u;
            self.sstate.get_cur_mb().qp_v = qp_v;
            self.sstate.get_cur_mb().transform_8x8 = mb_info.transform_size_8x8;
        }
        let has_dc = mb_info.mb_type.is_intra16x16() && mb_info.coded[24];
        if has_dc {
            idct_luma_dc(&mut mb_info.coeffs[24], qp_y);
            for i in 0..16 {
                mb_info.coeffs[i][0] = mb_info.coeffs[24][i];
            }
        }
        if !tx_bypass {
            if !mb_info.transform_size_8x8 {
                let quant_dc = !mb_info.mb_type.is_intra16x16();
                if quant_dc {
                    for (coded, coeffs) in mb_info.coded[..16].iter_mut().zip(mb_info.coeffs[..16].iter_mut()) {
                        if *coded {
                            idct(coeffs, qp_y);
                        } else if has_dc {
                            idct_dc(coeffs, qp_y, quant_dc);
                            *coded = true;
                        }
                    }
                } else {
                    for (coded, coeffs) in mb_info.coded[..16].iter_mut().zip(mb_info.coeffs[..16].iter_mut()) {
                        if *coded {
                            idct_skip_dc(coeffs, qp_y);
                        } else if has_dc {
                            idct_dc(coeffs, qp_y, quant_dc);
                            *coded = true;
                        }
                    }
                }
            } else {
                for i in 0..4 {
                    if mb_info.coded[(i & 1) * 2 + (i & 2) * 4] {
                        dequant8x8(&mut mb_info.coeffs8x8[i].coeffs, &pps.scaling_list_8x8[!mb_info.mb_type.is_intra() as usize]);
                        idct8x8(&mut mb_info.coeffs8x8[i].coeffs, qp_y);
                    }
                }
            }
        } else if !mb_info.transform_size_8x8 {
            for i in 0..16 {
                if !mb_info.coded[i] && has_dc {
                    mb_info.coded[i] = true;
                }
            }
        }
        for chroma in 0..2 {
            let qp_c = if chroma == 0 { qp_u } else { qp_v };
            if mb_info.cbpc != 0 {
                chroma_dc_transform(&mut mb_info.chroma_dc[chroma], qp_c);
            }
            for i in 0..4 {
                let blk_no = 16 + chroma * 4 + i;
                mb_info.coeffs[blk_no][0] = mb_info.chroma_dc[chroma][i];
                if mb_info.coded[blk_no] {
                    idct_skip_dc(&mut mb_info.coeffs[blk_no], qp_c);
                } else if mb_info.coeffs[blk_no][0] != 0 {
                    idct_dc(&mut mb_info.coeffs[blk_no], qp_c, false);
                    mb_info.coded[blk_no] = true;
                }
            }
        }
        if !pps.entropy_coding_mode || mb_info.mb_type.is_skip() || mb_info.mb_type.is_intra() {
            self.sstate.reset_mb_mv();
        }
        if !mb_info.mb_type.is_intra() {
            Self::pred_mv(&mut self.sstate, slice_refs, mb_info, self.cur_id, self.temporal_mv, self.sps[self.cur_sps].direct_8x8_inference);
        }
        if !pps.constrained_intra_pred && mb_info.mb_type != MBType::Intra4x4 && mb_info.mb_type != MBType::Intra8x8 {
            self.sstate.fill_ipred(IntraPredMode::DC);
        }

        let xpos = self.sstate.mb_x * 16;
        let ypos = self.sstate.mb_y * 16;

        if mb_info.mb_type != MBType::PCM {
            let weight_mode = if self.pps[self.cur_pps].weighted_pred && slice_hdr.slice_type.is_p() {
                    1
                } else if slice_hdr.slice_type.is_b() {
                    self.pps[self.cur_pps].weighted_bipred_idc
                } else {
                    0
                };
            recon_mb(frm, slice_hdr, mb_info, &mut self.sstate, slice_refs, &mut self.mc_dsp, weight_mode);
        } else {
            for (dline, src) in frm.data[frm.offset[0] + xpos + ypos * frm.stride[0]..].chunks_mut(frm.stride[0]).take(16).zip(self.ipcm_buf.chunks(16)) {
                dline[..16].copy_from_slice(src);
            }
            for (dline, src) in frm.data[frm.offset[1] + xpos/2 + ypos/2 * frm.stride[1]..].chunks_mut(frm.stride[1]).take(8).zip(self.ipcm_buf[256..].chunks(8)) {
                dline[..8].copy_from_slice(src);
            }
            for (dline, src) in frm.data[frm.offset[2] + xpos/2 + ypos/2 * frm.stride[2]..].chunks_mut(frm.stride[2]).take(8).zip(self.ipcm_buf[256 + 64..].chunks(8)) {
                dline[..8].copy_from_slice(src);
            }
        }
/*match mb_info.mb_type {
MBType::BSkip | MBType::Direct | MBType::B16x16(_) | MBType::B16x8(_, _) | MBType::B8x16(_, _) | MBType::B8x8 => {
 let dstride = frm.stride[0];
 let dst = &mut frm.data[frm.offset[0] + self.sstate.mb_x * 16 + self.sstate.mb_y * 16 * dstride..];
 for el in dst[..16].iter_mut() { *el = 255; }
 for row in dst.chunks_mut(dstride).skip(1).take(15) {
  row[0] = 255;
 }
},
_ => {},
};*/
        self.sstate.save_ipred_context(frm);

        let mb_pos = self.sstate.mb_x + self.sstate.mb_y * mv_info.mb_stride;
        let mut mb = FrameMBInfo::new();
        mb.mb_type = mb_info.mb_type.into();
        for blk4 in 0..16 {
            mb.mv[blk4] = self.sstate.get_cur_blk4(blk4).mv;
        }
        for blk8 in 0..4 {
            mb.ref_poc[blk8] = slice_refs.map_refs(self.sstate.get_cur_blk8(blk8).ref_idx);
            mb.ref_idx[blk8] = self.sstate.get_cur_blk8(blk8).ref_idx;
        }
        mv_info.mbs[mb_pos] = mb;

        if !self.deblock_skip && self.deblock_mode != 1 {
            self.sstate.fill_deblock(slice_refs, self.deblock_mode, self.is_s);
            loop_filter_mb(frm, &self.sstate, self.lf_alpha, self.lf_beta);
        }
        self.sstate.next_mb();
    }
    fn decode_slice_cavlc(&mut self, br: &mut BitReader, slice_hdr: &SliceHeader, full_size: usize, frm: &mut NASimpleVideoFrame<u8>, mv_info: &mut FrameMV) -> DecoderResult<bool> {
        const INTRA_CBP: [u8; 48] = [
            47, 31, 15,  0, 23, 27, 29, 30,  7, 11, 13, 14, 39, 43, 45, 46,
            16,  3,  5, 10, 12, 19, 21, 26, 28, 35, 37, 42, 44,  1,  2,  4,
             8, 17, 18, 20, 24,  6,  9, 22, 25, 32, 33, 34, 36, 40, 38, 41
        ];
        const INTER_CBP: [u8; 48] = [
             0, 16,  1,  2,  4,  8, 32,  3,  5, 10, 12, 15, 47,  7, 11, 13,
            14,  6,  9, 31, 35, 37, 42, 44, 33, 34, 36, 40, 39, 43, 45, 46,
            17, 18, 20, 24, 19, 21, 26, 28, 23, 27, 29, 30, 22, 25, 38, 41
        ];

        let mut mb_idx = slice_hdr.first_mb_in_slice;
        let mut mb_info = CurrentMBInfo { qp_y: slice_hdr.slice_qp, ..Default::default() };
        let skip_type = if slice_hdr.slice_type.is_p() { MBType::PSkip } else { MBType::BSkip };

        let slice_refs = self.frame_refs.cur_refs.clone();
        let sslice_refs = SimplifiedSliceRefs::new(&slice_refs);

        while br.tell() < full_size && mb_idx < self.num_mbs {
            mb_info.coded = [false; 25];
            mb_info.ref_l0 = [ZERO_REF; 4];
            mb_info.ref_l1 = [ZERO_REF; 4];
            mb_info.mv_l0 = [ZERO_MV; 16];
            mb_info.mv_l1 = [ZERO_MV; 16];
            mb_info.chroma_dc = [[0; 4]; 2];
            mb_info.cbpy = 0;
            mb_info.cbpc = 0;

            if !slice_hdr.slice_type.is_intra() {
                let mb_skip_run                     = br.read_ue()? as usize;
                validate!(mb_idx + mb_skip_run <= self.num_mbs);
                mb_info.mb_type = skip_type;
                for _ in 0..mb_skip_run {
                    self.handle_macroblock(slice_hdr, &mut mb_info, &sslice_refs, frm, mv_info);
                    mb_idx += 1;
                }
                if mb_idx == self.num_mbs || br.tell() >= full_size {
                    break;
                }
            }
            if br.tell() < full_size {
                if self.is_mbaff && ((mb_idx & 1) == 0) {
                    let _mb_field_decoding          = br.read_bool()?;
                }
                let mut mb_type = decode_mb_type_cavlc(br, slice_hdr)?;
                mb_info.mb_type = mb_type;
                mb_info.transform_size_8x8 = false;
                if mb_type == MBType::PCM {
                                                      br.align();
                    for pix in self.ipcm_buf[..256 + 64 + 64].iter_mut() {
                        *pix                        = br.read(8)? as u8;
                    }
                    self.sstate.fill_ncoded(16);
                } else {
                    if self.transform_8x8_mode && mb_type == MBType::Intra4x4 {
                        mb_info.transform_size_8x8  = br.read_bool()?;
                        if mb_info.transform_size_8x8 {
                            mb_type = MBType::Intra8x8;
                            mb_info.mb_type = MBType::Intra8x8;
                        }
                    }
                    decode_mb_pred_cavlc(br, slice_hdr, mb_type, &mut self.sstate, &mut mb_info)?;
                    let (cbpy, cbpc) = if let MBType::Intra16x16(_, cbpy, cbpc) = mb_type {
                            (cbpy, cbpc)
                        } else {
                            let cbp_id              = br.read_ue()? as usize;
                            validate!(cbp_id < INTRA_CBP.len());
                            let cbp = if mb_type == MBType::Intra4x4 || mb_type == MBType::Intra8x8 {
                                    INTRA_CBP[cbp_id]
                                } else {
                                    INTER_CBP[cbp_id]
                                };
                            if self.transform_8x8_mode && (cbp & 0xF) != 0 && mb_info.can_have_8x8_tx(self.sps[self.cur_sps].direct_8x8_inference) {
                                mb_info.transform_size_8x8 = br.read_bool()?;
                            }
                            ((cbp & 0xF), (cbp >> 4))
                        };
                    mb_info.cbpy = cbpy;
                    mb_info.cbpc = cbpc;
                    self.sstate.get_cur_mb().cbp = (cbpc << 4) | cbpy;
                    if cbpy != 0 || cbpc != 0 || mb_type.is_intra16x16() {
                        let mb_qp_delta             = br.read_se()?;
                        validate!(mb_qp_delta >= -26 && mb_qp_delta <= 25);
                        let new_qp = mb_qp_delta + i32::from(mb_info.qp_y);
                        mb_info.qp_y = if new_qp < 0 {
                                (new_qp + 52) as u8
                            } else if new_qp >= 52 {
                                (new_qp - 52) as u8
                            } else {
                                new_qp as u8
                            };
                        mb_info.coeffs = [[0; 16]; 25];
                        if self.transform_8x8_mode {
                            mb_info.clear_coeffs8x8();
                        }
                        mb_info.chroma_dc = [[0; 4]; 2];
                        decode_residual_cavlc(br, &mut self.sstate, &mut mb_info, &self.cavlc_cb)?;
                    }
                }
                self.handle_macroblock(slice_hdr, &mut mb_info, &sslice_refs, frm, mv_info);
            }
            mb_idx += 1;
        }
        if let Some(ref mut pic) = self.cur_pic {
            pic.cur_mb = mb_idx;
        }
        Ok(mb_idx == self.num_mbs)
    }
    fn decode_slice_cabac(&mut self, cabac: &mut CABAC, slice_hdr: &SliceHeader, frm: &mut NASimpleVideoFrame<u8>, mv_info: &mut FrameMV) -> DecoderResult<bool> {
        let mut mb_idx = slice_hdr.first_mb_in_slice;
        let mut prev_mb_skipped = false;
        let skip_type = if slice_hdr.slice_type.is_p() { MBType::PSkip } else { MBType::BSkip };
        let mut last_qp_diff = false;

        let mut mb_info = CurrentMBInfo { qp_y: slice_hdr.slice_qp, ..Default::default() };

        let slice_refs = self.frame_refs.cur_refs.clone();
        let sslice_refs = SimplifiedSliceRefs::new(&slice_refs);

        while mb_idx < self.num_mbs {
            mb_info.coded = [false; 25];
            mb_info.ref_l0 = [ZERO_REF; 4];
            mb_info.ref_l1 = [ZERO_REF; 4];
            mb_info.mv_l0 = [ZERO_MV; 16];
            mb_info.mv_l1 = [ZERO_MV; 16];
            mb_info.chroma_dc = [[0; 4]; 2];
            mb_info.cbpy = 0;
            mb_info.cbpc = 0;
            let mb_skip = cabac_decode_mbskip(cabac, &self.sstate, slice_hdr);
            if !mb_skip {
                if self.is_mbaff && (((mb_idx & 1) == 0) || (prev_mb_skipped && ((mb_idx & 1) == 1))) {
                    let _mb_field_decoding          = cabac.decode_bit(70);
                }
                let mut mb_type                     = cabac_decode_mb_type(cabac, slice_hdr, &self.sstate);
                mb_info.mb_type = mb_type;
                mb_info.transform_size_8x8 = false;
                if mb_type == MBType::PCM {
                    let ipcm_size = 256 + 64 + 64;
                    validate!(cabac.pos + ipcm_size <= cabac.src.len());
                    self.ipcm_buf[..ipcm_size].copy_from_slice(&cabac.src[cabac.pos..][..ipcm_size]);
                    cabac.pos += ipcm_size;
                    cabac.reinit()?;
                    last_qp_diff = false;
                } else {
                    if self.transform_8x8_mode && mb_type == MBType::Intra4x4 {
                        let mut ctx = 0;
                        if self.sstate.get_top_mb().transform_8x8 {
                            ctx += 1;
                        }
                        if self.sstate.get_left_mb().transform_8x8 {
                            ctx += 1;
                        }
                        mb_info.transform_size_8x8  = cabac.decode_bit(399 + ctx);
                        if mb_info.transform_size_8x8 {
                            mb_type = MBType::Intra8x8;
                            mb_info.mb_type = MBType::Intra8x8;
                        }
                    }
                    decode_mb_pred_cabac(cabac, slice_hdr, mb_type, &mut self.sstate, &mut mb_info);
                    let (cbpy, cbpc) = if let MBType::Intra16x16(_, cbpy, cbpc) = mb_type {
                            (cbpy, cbpc)
                        } else {
                            decode_cbp_cabac(cabac, &self.sstate)
                        };
                    if self.transform_8x8_mode && cbpy != 0 && mb_info.can_have_8x8_tx(self.sps[self.cur_sps].direct_8x8_inference) {
                        let mut ctx = 0;
                        if self.sstate.get_top_mb().transform_8x8 {
                            ctx += 1;
                        }
                        if self.sstate.get_left_mb().transform_8x8 {
                            ctx += 1;
                        }
                        mb_info.transform_size_8x8  = cabac.decode_bit(399 + ctx);
                    }
                    if mb_type.is_intra() {
                        self.sstate.get_cur_mb().cmode = mb_info.chroma_ipred;
                    }
                    mb_info.cbpy = cbpy;
                    mb_info.cbpc = cbpc;
                    self.sstate.get_cur_mb().cbp = (cbpc << 4) | cbpy;
                    if cbpy != 0 || cbpc != 0 || mb_type.is_intra16x16() {
                        let mb_qp_delta = decode_mb_qp_delta_cabac(cabac, last_qp_diff as usize);
                        validate!(mb_qp_delta >= -26 && mb_qp_delta <= 25);
                        last_qp_diff = mb_qp_delta != 0;
                        let new_qp = mb_qp_delta + i32::from(mb_info.qp_y);
                        mb_info.qp_y = if new_qp < 0 {
                                (new_qp + 52) as u8
                            } else if new_qp >= 52 {
                                (new_qp - 52) as u8
                            } else {
                                new_qp as u8
                            };
                        mb_info.coeffs = [[0; 16]; 25];
                        if self.transform_8x8_mode {
                            mb_info.clear_coeffs8x8();
                        }
                        mb_info.chroma_dc = [[0; 4]; 2];
                        decode_residual_cabac(cabac, &mut self.sstate, &mut mb_info);
                    } else {
                        last_qp_diff = false;
                    }
                }
            } else {
                mb_info.mb_type = skip_type;
                mb_info.transform_size_8x8 = false;
                last_qp_diff = false;
            }
            self.handle_macroblock(slice_hdr, &mut mb_info, &sslice_refs, frm, mv_info);
            prev_mb_skipped = mb_skip;
            if !(self.is_mbaff && ((mb_idx & 1) == 0)) && cabac.decode_terminate() {
                if let Some(ref mut pic) = self.cur_pic {
                    pic.cur_mb = mb_idx + 1;
                }
                return Ok(mb_idx + 1 == self.num_mbs);
            }
            mb_idx += 1;
        }
        Err(DecoderError::InvalidData)
    }
}

impl NADecoder for H264Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = YUV420_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            let edata = info.get_extradata().unwrap();
//print!("edata:"); for &el in edata.iter() { print!(" {:02X}", el); } println!();
            if edata.len() > 11 && &edata[0..4] == b"avcC" {
                let mut mr = MemoryReader::new_read(edata.as_slice());
                let mut br = ByteReader::new(&mut mr);
                let mut nal_buf = Vec::new();

                                          br.read_skip(4)?;
                let version             = br.read_byte()?;
                validate!(version == 1);
                let profile             = br.read_byte()?;
                let _compatibility      = br.read_byte()?;
                let _level              = br.read_byte()?;
                let b                   = br.read_byte()?;
                validate!((b & 0xFC) == 0xFC);
                self.nal_len            = (b & 3) + 1;
                let b                   = br.read_byte()?;
                validate!((b & 0xE0) == 0xE0);
                let num_sps = (b & 0x1F) as usize;
                for _ in 0..num_sps {
                    let len             = br.read_u16be()? as usize;
                    let offset = br.tell() as usize;
                    validate!((br.peek_byte()? & 0x1F) == 7);
                    let _size = unescape_nal(&edata[offset..][..len], &mut nal_buf);
                    self.handle_nal(&nal_buf, supp, true)?;
                                          br.read_skip(len)?;
                }
                let num_pps             = br.read_byte()? as usize;
                for _ in 0..num_pps {
                    let len             = br.read_u16be()? as usize;
                    let offset = br.tell() as usize;
                    validate!((br.peek_byte()? & 0x1F) == 8);
                    let _size = unescape_nal(&edata[offset..][..len], &mut nal_buf);
                    self.handle_nal(&nal_buf, supp, true)?;
                                          br.read_skip(len)?;
                }
                if br.left() > 0 {
                    match profile {
                        100 | 110 | 122 | 144 => {
                            let b       = br.read_byte()?;
                            validate!((b & 0xFC) == 0xFC);
                            // b & 3 -> chroma format
                            let b       = br.read_byte()?;
                            validate!((b & 0xF8) == 0xF8);
                            // b & 7 -> luma depth minus 8
                            let b       = br.read_byte()?;
                            validate!((b & 0xF8) == 0xF8);
                            // b & 7 -> chroma depth minus 8
                            let num_spsext  = br.read_byte()? as usize;
                            for _ in 0..num_spsext {
                                let len = br.read_u16be()? as usize;
                                // parse spsext
                                          br.read_skip(len)?;
                            }
                        },
                        _ => {},
                    };
                }
            } else {
                return Err(DecoderError::NotImplemented);
            }

            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();

            if (self.width == 0 || self.height == 0) && !self.sps.is_empty() {
                self.width  = self.sps[0].pic_width_in_mbs  * 16;
                self.height = self.sps[0].pic_height_in_mbs * 16;
            }

            let num_bufs = if !self.sps.is_empty() {
                    self.sps[0].num_ref_frames + 1
                } else {
                    3
                }.max(16 + 1);
            supp.pool_u8.set_dec_bufs(num_bufs);
            supp.pool_u8.prealloc_video(NAVideoInfo::new(self.width, self.height, false, fmt), 4)?;

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);
        let mut nal_buf = Vec::with_capacity(src.len());
        if self.nal_len > 0 {
            let mut skip_decoding = false;
            if self.skip_mode != FrameSkipMode::None {
                let mut pic_type = FrameType::I;
                let mut is_ref = false;
                while br.left() > 0 {
                    let size = match self.nal_len {
                            1 => br.read_byte()? as usize,
                            2 => br.read_u16be()? as usize,
                            3 => br.read_u24be()? as usize,
                            4 => br.read_u32be()? as usize,
                            _ => unreachable!(),
                        };
                    validate!(br.left() >= (size as i64));
                    let offset = br.tell() as usize;
                    let size = unescape_nal(&src[offset..][..size], &mut nal_buf);
                    validate!(size > 0);
                    let nal_ref_idc   = nal_buf[0] >> 5;
                    let nal_unit_type = nal_buf[0] & 0x1F;
                    if nal_unit_type == 1 || nal_unit_type == 5 {
                        let mut bitr = BitReader::new(&nal_buf[1..], BitReaderMode::BE);
                        let (first_mb, slice_type) = parse_slice_header_minimal(&mut bitr)?;
                        if first_mb == 0 && nal_ref_idc != 0 {
                            is_ref = true;
                        }
                        let new_type = slice_type.to_frame_type();
                        pic_type = match (pic_type, new_type) {
                                         (FrameType::I, _) => new_type,
                                         (_, FrameType::B) => FrameType::B,
                                         _ => pic_type,
                                     };
                    }
                    br.read_skip(size)?;
                }
                match self.skip_mode {
                    FrameSkipMode::IntraOnly => {
                        skip_decoding = pic_type != FrameType::I;
                    },
                    FrameSkipMode::KeyframesOnly => {
                        if !is_ref {
                            skip_decoding = true;
                        }
                    },
                    _ => {},
                };
                br.seek(SeekFrom::Start(0))?;
            }
            while br.left() > 0 {
                let size = match self.nal_len {
                        1 => br.read_byte()? as usize,
                        2 => br.read_u16be()? as usize,
                        3 => br.read_u24be()? as usize,
                        4 => br.read_u32be()? as usize,
                        _ => unreachable!(),
                    };
                validate!(br.left() >= (size as i64));
                let offset = br.tell() as usize;
                let _size = unescape_nal(&src[offset..][..size], &mut nal_buf);
                self.handle_nal(nal_buf.as_slice(), supp, skip_decoding)?;
                br.read_skip(size)?;
            }
        } else {
//todo NAL detection
            unimplemented!();
        }

        let (bufinfo, ftype, dts) = if self.has_pic && self.cur_pic.is_some() {
                let mut npic = None;
                std::mem::swap(&mut self.cur_pic, &mut npic);
                let cpic = npic.unwrap();
                let ret = (NABufferType::Video(cpic.buf.clone()), cpic.pic_type, Some(u64::from(cpic.full_id)));
                if cpic.is_ref {
                    self.frame_refs.add_short_term(cpic.clone(), self.sps[self.cur_sps].num_ref_frames);
                }
                if let Some(lt_idx) = cpic.long_term {
                    self.frame_refs.add_long_term(lt_idx, cpic);
                }
                ret
            } else {
                (NABufferType::None, FrameType::Skip, None)
            };

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(ftype == FrameType::I);
        if let (Some(mydts), None) = (dts, frm.get_dts()) {
            frm.set_dts(Some(mydts));
        }
        if let Some(dts) = dts {
            frm.set_id(dts as i64);
        }
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for H264Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { DECODER_OPTIONS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in DECODER_OPTIONS.iter() {
                if opt_def.check(option).is_ok() {
                    match (option.name, &option.value) {
                        (FRAME_SKIP_OPTION, NAValue::String(ref strval)) => {
                            if let Ok(smode) = FrameSkipMode::from_str(strval) {
                                self.skip_mode = smode;
                            }
                        },
                        (DEBLOCK_SKIP_OPTION, NAValue::Bool(val)) => {
                            self.deblock_skip = *val;
                        },
                        _ => {},
                    }
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            FRAME_SKIP_OPTION => Some(NAValue::String(self.skip_mode.to_string())),
            DEBLOCK_SKIP_OPTION => Some(NAValue::Bool(self.deblock_skip)),
            _ => None,
        }
    }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(H264Decoder::new())
}
