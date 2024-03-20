use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::codecs::{MV, ZIGZAG};
use super::vpcommon::*;
use super::vp56::*;
use super::vp6data::*;
use super::vp6dsp::*;

#[derive(Default)]
struct VP6BR {
    vpversion:      u8,
    profile:        u8,
    interlaced:     bool,
    do_pm:          bool,
    loop_mode:      u8,
    autosel_pm:     bool,
    var_thresh:     u16,
    mv_thresh:      u8,
    bicubic:        bool,
    filter_alpha:   usize,
}

impl VP6BR {
    fn new() -> Self {
        Self::default()
    }
}

impl VP56Parser for VP6BR {
    fn parse_header(&mut self, bc: &mut BoolCoder) -> DecoderResult<VP56Header> {
        let mut hdr = VP56Header::default();
// horrible hack to match VP6 header parsing
        let src = bc.src;
        let mut br = BitReader::new(src, BitReaderMode::BE);

        hdr.is_intra                            = !br.read_bool()?;
        hdr.is_golden = hdr.is_intra;
        hdr.quant                               = br.read(6)? as u8;
        hdr.multistream                         = br.read_bool()?;
        if hdr.is_intra {
            hdr.version                         = br.read(5)? as u8;
            validate!((hdr.version >= VERSION_VP60) && (hdr.version <= VERSION_VP62));
            hdr.profile                         = br.read(2)? as u8;
            validate!((hdr.profile == VP6_SIMPLE_PROFILE) || (hdr.profile == VP6_ADVANCED_PROFILE));
            hdr.interlaced                      = br.read_bool()?;
        } else {
            hdr.version = self.vpversion;
            hdr.profile = self.profile;
            hdr.interlaced = self.interlaced;
        }
        if hdr.multistream || (hdr.profile == VP6_SIMPLE_PROFILE) {
            hdr.offset                          = br.read(16)? as u16;
            validate!(hdr.offset > if hdr.is_intra { 6 } else { 2 });
            hdr.multistream = true;
        }
        let bytes = br.tell() >> 3;
        bc.skip_bytes(bytes);
        self.loop_mode = 0;
        if hdr.is_intra {
            hdr.mb_h                            = bc.read_bits(8) as u8;
            hdr.mb_w                            = bc.read_bits(8) as u8;
            hdr.disp_h                          = bc.read_bits(8) as u8;
            hdr.disp_w                          = bc.read_bits(8) as u8;
            validate!((hdr.mb_h > 0) && (hdr.mb_w > 0) && (hdr.disp_w > 0) && (hdr.disp_h > 0));
            validate!((hdr.disp_w <= hdr.mb_w) && (hdr.disp_h <= hdr.mb_h));
            hdr.scale                           = bc.read_bits(2) as u8;
        } else {
            hdr.is_golden                       = bc.read_bool();
            if hdr.profile == VP6_ADVANCED_PROFILE {
                self.loop_mode                  = bc.read_bool() as u8;
                if self.loop_mode != 0 {
                    self.loop_mode             += bc.read_bool() as u8;
                    validate!(self.loop_mode <= 1);
                }
                if hdr.version == VERSION_VP62 {
                    self.do_pm                  = bc.read_bool();
                }
            }
        }

        if (hdr.profile == VP6_ADVANCED_PROFILE) && (hdr.is_intra || self.do_pm) {
            self.autosel_pm                     = bc.read_bool();
            if self.autosel_pm {
                self.var_thresh                 = bc.read_bits(5) as u16;
                if hdr.version != VERSION_VP62 {
                    self.var_thresh <<= 5;
                }
                self.mv_thresh                  = bc.read_bits(3) as u8;
            } else {
                self.bicubic                    = bc.read_bool();
            }
            if hdr.version == VERSION_VP62 {
                self.filter_alpha               = bc.read_bits(4) as usize;
            } else {
                self.filter_alpha = 16;
            }
        }

        hdr.use_huffman                         = bc.read_bool();

        self.vpversion  = hdr.version;
        self.profile    = hdr.profile;
        self.interlaced = hdr.interlaced;
        Ok(hdr)
    }
    fn decode_mv(&self, bc: &mut BoolCoder, model: &VP56MVModel) -> i16 {
        let val = if !bc.read_prob(model.nz_prob) { // short vector
                vp_tree!(bc, model.tree_probs[0],
                         vp_tree!(bc, model.tree_probs[1],
                                  vp_tree!(bc, model.tree_probs[2], 0, 1),
                                  vp_tree!(bc, model.tree_probs[3], 2, 3)),
                         vp_tree!(bc, model.tree_probs[4],
                                  vp_tree!(bc, model.tree_probs[5], 4, 5),
                                  vp_tree!(bc, model.tree_probs[6], 6, 7)))
            } else {
                let mut raw = 0;
                for ord in LONG_VECTOR_ORDER.iter() {
                    raw                         |= (bc.read_prob(model.raw_probs[*ord]) as i16) << *ord;
                }
                if (raw & 0xF0) != 0 {
                    raw                         |= (bc.read_prob(model.raw_probs[3]) as i16) << 3;
                } else {
                    raw |= 1 << 3;
                }
                raw
            };
        if (val != 0) && bc.read_prob(model.sign_prob) {
            -val
        } else {
            val
        }
    }
    fn reset_models(&self, models: &mut VP56Models) {
        for (i, mdl) in models.mv_models.iter_mut().enumerate() {
            mdl.nz_prob         = NZ_PROBS[i];
            mdl.sign_prob       = 128;
            mdl.raw_probs.copy_from_slice(&RAW_PROBS[i]);
            mdl.tree_probs.copy_from_slice(&TREE_PROBS[i]);
        }
        models.vp6models.zero_run_probs.copy_from_slice(&ZERO_RUN_PROBS);
        reset_scan(&mut models.vp6models, self.interlaced);
    }
    fn decode_mv_models(&self, bc: &mut BoolCoder, models: &mut [VP56MVModel; 2]) -> DecoderResult<()> {
        for comp in 0..2 {
            if bc.read_prob(HAS_NZ_PROB[comp]) {
                models[comp].nz_prob            = bc.read_probability();
            }
            if bc.read_prob(HAS_SIGN_PROB[comp]) {
                models[comp].sign_prob          = bc.read_probability();
            }
        }
        for comp in 0..2 {
            for (i, prob) in HAS_TREE_PROB[comp].iter().enumerate() {
                if bc.read_prob(*prob) {
                    models[comp].tree_probs[i]  = bc.read_probability();
                }
            }
        }
        for comp in 0..2 {
            for (i, prob) in HAS_RAW_PROB[comp].iter().enumerate() {
                if bc.read_prob(*prob) {
                    models[comp].raw_probs[i]   = bc.read_probability();
                }
            }
        }
        Ok(())
    }
    fn decode_coeff_models(&self, bc: &mut BoolCoder, models: &mut VP56Models, is_intra: bool) -> DecoderResult<()> {
        let mut def_prob = [128u8; 11];
        for plane in 0..2 {
            for i in 0..11 {
                if bc.read_prob(HAS_COEF_PROBS[plane][i]) {
                    def_prob[i]                 = bc.read_probability();
                    models.coeff_models[plane].dc_value_probs[i] = def_prob[i];
                } else if is_intra {
                    models.coeff_models[plane].dc_value_probs[i] = def_prob[i];
                }
            }
        }

        if bc.read_bool() {
            for i in 1..64 {
                if bc.read_prob(HAS_SCAN_UPD_PROBS[i]) {
                    models.vp6models.scan_order[i]  = bc.read_bits(4) as usize;
                }
            }
            update_scan(&mut models.vp6models);
        } else {
            reset_scan(&mut models.vp6models, self.interlaced);
        }

        for comp in 0..2 {
            for i in 0..14 {
                if bc.read_prob(HAS_ZERO_RUN_PROBS[comp][i]) {
                    models.vp6models.zero_run_probs[comp][i] = bc.read_probability();
                }
            }
        }

        for ctype in 0..3 {
            for plane in 0..2 {
                for group in 0..6 {
                    for i in 0..11 {
                        if bc.read_prob(VP6_AC_PROBS[ctype][plane][group][i]) {
                            def_prob[i]         = bc.read_probability();
                            models.coeff_models[plane].ac_val_probs[ctype][group][i] = def_prob[i];
                        } else if is_intra {
                            models.coeff_models[plane].ac_val_probs[ctype][group][i] = def_prob[i];
                        }
                    }
                }
            }
        }
        for plane in 0..2 {
            let mdl = &mut models.coeff_models[plane];
            for i in 0..3 {
                for k in 0..5 {
                    mdl.dc_token_probs[0][i][k] = rescale_prob(mdl.dc_value_probs[k], &VP6_DC_WEIGHTS[k][i], 255);
                }
            }
        }
        Ok(())
    }
    fn decode_block(&self, bc: &mut BoolCoder, coeffs: &mut [i16; 64], model: &VP56CoeffModel, vp6model: &VP6Models, fstate: &mut FrameState) -> DecoderResult<()> {
        let left_ctx = fstate.coeff_cat[fstate.ctx_idx][0] as usize;
        let top_ctx = fstate.top_ctx as usize;
        let dc_mode = top_ctx + left_ctx;
        let token = decode_token_bc(bc, &model.dc_token_probs[0][dc_mode], model.dc_value_probs[5], true, true);
        let val = expand_token_bc(bc, &model.dc_value_probs, token, 6);
        coeffs[0] = val;
        fstate.last_idx[fstate.ctx_idx] = 0;

        let mut idx = 1;
        let mut last_val = val;
        while idx < 64 {
            let ac_band = VP6_IDX_TO_AC_BAND[idx];
            let ac_mode = last_val.abs().min(2) as usize;
            let has_nnz = (idx == 1) || (last_val != 0);
            let token = decode_token_bc(bc, &model.ac_val_probs[ac_mode][ac_band], model.ac_val_probs[ac_mode][ac_band][5], false, has_nnz);
            if token == 42 { break; }
            let val = expand_token_bc(bc, &model.ac_val_probs[ac_mode][ac_band], token, 6);
            coeffs[vp6model.zigzag[idx]] = val.wrapping_mul(fstate.ac_quant);
            idx += 1;
            last_val = val;
            if val == 0 {
                idx += decode_zero_run_bc(bc, &vp6model.zero_run_probs[if idx >= 7 { 1 } else { 0 }]);
                validate!(idx <= 64);
            }
        }
        fstate.coeff_cat[fstate.ctx_idx][0] = if coeffs[0] != 0 { 1 } else { 0 };
        fstate.top_ctx = fstate.coeff_cat[fstate.ctx_idx][0];
        fstate.last_idx[fstate.ctx_idx] = idx;
        Ok(())
    }
    fn decode_block_huff(&self, br: &mut BitReader, coeffs: &mut [i16; 64], vp6model: &VP6Models, model: &VP6HuffModels, fstate: &mut FrameState) -> DecoderResult<()> {
        let plane = if (fstate.plane == 0) || (fstate.plane == 3) { 0 } else { 1 };
        let mut last_val;

        if fstate.dc_zero_run[plane] == 0 {
            let (val, eob) = decode_token_huff(br, &model.dc_token_tree[plane])?;
            if eob {
                return Ok(());
            }
            last_val = val;
            coeffs[0] = val;
            if val == 0 {
                fstate.dc_zero_run[plane] = decode_eob_run_huff(br)?;
            }
        } else {
            last_val = 0;
            fstate.dc_zero_run[plane] -= 1;
        }

        if fstate.ac_zero_run[plane] > 0 {
            fstate.ac_zero_run[plane] -= 1;
            fstate.last_idx[fstate.ctx_idx] = 0;
            return Ok(());
        }

        let mut idx = 1;
        while idx < 64 {
            let ac_band = VP6_IDX_TO_AC_BAND[idx].min(3);
            let ac_mode = last_val.abs().min(2) as usize;
            let (val, eob) = decode_token_huff(br, &model.ac_token_tree[plane][ac_mode][ac_band])?;
            if eob {
                if idx == 1 {
                    fstate.ac_zero_run[plane] = decode_eob_run_huff(br)?;
                }
                break;
            }
            coeffs[vp6model.zigzag[idx]] = val.wrapping_mul(fstate.ac_quant);
            idx += 1;
            last_val = val;
            if val == 0 {
                idx += decode_zero_run_huff(br, &model.zero_run_tree[if idx >= 7 { 1 } else { 0 }])?;
                validate!(idx <= 64);
            }
        }

        fstate.last_idx[fstate.ctx_idx] = idx;

        Ok(())
    }
    fn mc_block(&self, dst: &mut NASimpleVideoFrame<u8>, mut mc_buf: NAVideoBufferRef<u8>, src: NAVideoBufferRef<u8>, plane: usize, x: usize, y: usize, mv: MV, loop_str: i16) {
        let is_luma = (plane != 1) && (plane != 2);
        let (sx, sy, mx, my, msx, msy) = if is_luma {
                (mv.x >> 2, mv.y >> 2, (mv.x & 3) << 1, (mv.y & 3) << 1, mv.x / 4, mv.y / 4)
            } else {
                (mv.x >> 3, mv.y >> 3, mv.x & 7, mv.y & 7, mv.x / 8, mv.y / 8)
            };
        let tmp_blk = mc_buf.get_data_mut().unwrap();
        get_block(tmp_blk, 16, src, plane, x, y, sx, sy);
        if (msx & 7) != 0 {
            let foff = (8 - (sx & 7)) as usize;
            let off = 2 + foff;
            vp31_loop_filter(tmp_blk, off, 1, 16, 12, loop_str);
        }
        if (msy & 7) != 0 {
            let foff = (8 - (sy & 7)) as usize;
            let off = (2 + foff) * 16;
            vp31_loop_filter(tmp_blk, off, 16, 1, 12, loop_str);
        }
        let copy_mode = (mx == 0) && (my == 0);
        let mut bicubic = !copy_mode && is_luma && self.bicubic;
        if is_luma && !copy_mode && (self.profile == VP6_ADVANCED_PROFILE) {
            if !self.autosel_pm {
                bicubic = true;
            } else {
                let mv_limit = 1 << (self.mv_thresh + 1);
                if (mv.x.abs() <= mv_limit) && (mv.y.abs() <= mv_limit) {
                    let mut var_off = 16 * 2 + 2;
                    if mv.x < 0 { var_off += 1; }
                    if mv.y < 0 { var_off += 16; }
                    let var = calc_variance(&tmp_blk[var_off..], 16);
                    if var >= self.var_thresh {
                        bicubic = true;
                    }
                }
            }
        }
        let dstride = dst.stride[plane];
        let dbuf = &mut dst.data[dst.offset[plane] + x + y * dstride..];
        if copy_mode {
            let src = &tmp_blk[2 * 16 + 2..];
            for (dline, sline) in dbuf.chunks_mut(dst.stride[plane]).zip(src.chunks(16)).take(8) {
                dline[..8].copy_from_slice(&sline[..8]);
            }
        } else if bicubic {
            let coeff_h = &VP6_BICUBIC_COEFFS[self.filter_alpha][mx as usize];
            let coeff_v = &VP6_BICUBIC_COEFFS[self.filter_alpha][my as usize];
            mc_bicubic(dbuf, dstride, tmp_blk, 16 * 2 + 2, 16, coeff_h, coeff_v);
        } else {
            mc_bilinear(dbuf, dstride, tmp_blk, 16 * 2 + 2, 16, mx as u16, my as u16);
        }
    }
}

fn update_scan(model: &mut VP6Models) {
    let mut idx = 1;
    for band in 0..16 {
        for i in 1..64 {
            if model.scan_order[i] == band {
                model.scan[idx] = i;
                idx += 1;
            }
        }
    }
    for i in 1..64 {
        model.zigzag[i] = ZIGZAG[model.scan[i]];
    }
}

fn reset_scan(model: &mut VP6Models, interlaced: bool) {
    if !interlaced {
        model.scan_order.copy_from_slice(&VP6_DEFAULT_SCAN_ORDER);
    } else {
        model.scan_order.copy_from_slice(&VP6_INTERLACED_SCAN_ORDER);
    }
    for i in 0..64 { model.scan[i] = i; }
    model.zigzag.copy_from_slice(&ZIGZAG);
}

fn decode_token_bc(bc: &mut BoolCoder, probs: &[u8], prob34: u8, is_dc: bool, has_nnz: bool) -> u8 {
    if has_nnz && !bc.read_prob(probs[0]) {
        if is_dc || bc.read_prob(probs[1]) {
            0
        } else {
            TOKEN_EOB
        }
    } else {
        vp_tree!(bc, probs[2],
                 1,
                 vp_tree!(bc, probs[3],
                          vp_tree!(bc, probs[4],
                                   2,
                                   vp_tree!(bc, prob34, 3, 4)),
                          TOKEN_LARGE))
    }
}

fn decode_zero_run_bc(bc: &mut BoolCoder, probs: &[u8; 14]) -> usize {
    let val = vp_tree!(bc, probs[0],
                    vp_tree!(bc, probs[1],
                        vp_tree!(bc, probs[2], 0, 1),
                        vp_tree!(bc, probs[3], 2, 3)),
                    vp_tree!(bc, probs[4],
                        vp_tree!(bc, probs[5],
                            vp_tree!(bc, probs[6], 4, 5),
                            vp_tree!(bc, probs[7], 6, 7)),
                        42));
    if val != 42 {
        val
    } else {
        let mut nval = 8;
        for i in 0..6 {
            nval                                += (bc.read_prob(probs[i + 8]) as usize) << i;
        }
        nval
    }
}

fn decode_token_huff(br: &mut BitReader, huff: &VP6Huff) -> DecoderResult<(i16, bool)> {
    let tok                                     = br.read_huff(huff)?;
    match tok {
        0   => Ok((0, false)),
        1 | 2 | 3 | 4 => {
            if !br.read_bool()? {
                Ok((i16::from(tok), false))
            } else {
                Ok((-i16::from(tok), false))
            }
        },
        5 | 6 | 7 | 8 | 9 | 10 => {
            let base = (tok - 5) as usize;
            let add_bits                        = br.read(VP6_COEF_ADD_BITS[base])? as i16;
            let val = VP56_COEF_BASE[base] + add_bits;
            if !br.read_bool()? {
                Ok((val, false))
            } else {
                Ok((-val, false))
            }
        },
        _   => Ok((0, true)),
    }
}

fn decode_eob_run_huff(br: &mut BitReader) -> DecoderResult<usize> {
    let val                                     = br.read(2)?;
    match val {
        0 => Ok(0),
        1 => Ok(1),
        2 => {
            let val                             = br.read(2)?;
            Ok((val as usize) + 2)
        },
        _ => {
            if br.read_bool()? {
                Ok((br.read(6)? as usize) + 10)
            } else {
                Ok((br.read(2)? as usize) + 6)
            }
        },
    }
}

fn decode_zero_run_huff(br: &mut BitReader, huff: &VP6Huff) -> DecoderResult<usize> {
    let val                                     = br.read_huff(huff)?;
    if val < 8 {
        Ok(val as usize)
    } else {
        Ok((br.read(6)? as usize) + 8)
    }
}

struct VP6Decoder {
    dec:        VP56Decoder,
    info:       NACodecInfoRef,
    br:         VP6BR,
    has_alpha:  bool,
    flipped:    bool,
}

impl VP6Decoder {
    fn new(flipped: bool, has_alpha: bool) -> Self {
        Self {
            dec:        VP56Decoder::new(6, has_alpha, flipped),
            info:       NACodecInfoRef::default(),
            br:         VP6BR::new(),
            has_alpha,
            flipped,
        }
    }
}

impl NADecoder for VP6Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = if !self.has_alpha {
                    YUV420_FORMAT
                } else {
                    VP_YUVA420_FORMAT
                };
            let mut width  = vinfo.get_width();
            let mut height = vinfo.get_height();
            if let (false, Some(edta)) = (self.flipped, info.get_extradata()) {
                if (width & 0xF) == 0 && (height & 0xF) == 0 {
                    width  -= usize::from(edta[0] >> 4);
                    height -= usize::from(edta[0] & 0xF);
                }
            }
            let myvinfo = NAVideoInfo::new(width, height, self.flipped, fmt);
            let myinfo = NACodecTypeInfo::Video(myvinfo);
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.dec.init(supp, myvinfo)?;
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        let (bufinfo, ftype) = self.dec.decode_frame(supp, src.as_slice(), &mut self.br)?;

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(ftype == FrameType::I);
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for VP6Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_vp6() -> Box<dyn NADecoder + Send> {
    Box::new(VP6Decoder::new(true, false))
}

pub fn get_decoder_vp6f() -> Box<dyn NADecoder + Send> {
    Box::new(VP6Decoder::new(false, false))
}

pub fn get_decoder_vp6_alpha() -> Box<dyn NADecoder + Send> {
    Box::new(VP6Decoder::new(false, true))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;

    #[test]
    fn test_vp6() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample from a private collection
        test_decoding("avi", "vp6", "assets/Duck/selection_720x576_300kBit_vp60i.avi", Some(16),
                      &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x042c3e96, 0x8a9b26a2, 0x4dcbaf66, 0x1b788d03]));
    }
    #[test]
    fn test_vp6_huff() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/VP6/vp6_crash.avi
        test_decoding("avi", "vp6", "assets/Duck/vp6_crash.avi", Some(4),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0xdcd70fa0, 0x0d075ce2, 0xc9e65077, 0xb003a92e],
                            [0x334abf96, 0x3a004c7a, 0x5781cd5c, 0x25c3ae5c],
                            [0x6164b851, 0x528cd8de, 0xecab7328, 0x4b49708a],
                            [0x11b048ac, 0xedb3e471, 0xd04e9399, 0x64e623e3],
                            [0x182871b1, 0x2146893a, 0x2912210e, 0x6dd592e8]]));
    }
    #[test]
    fn test_vp6_alpha() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample created by remuxing some VP6A in FLV
        test_decoding("avi", "vp6a", "assets/Duck/vp6a.avi", Some(25),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xaf903d79, 0x17ddb3c7, 0xf0a381e8, 0x26b36a7d],
                            [0xd3801a96, 0x1b5384ff, 0x422b228c, 0x9c4582c4],
                            [0x8ddb0dfe, 0x302eb1ed, 0x10e64e22, 0x5a5a62b9],
                            [0x79338328, 0x06113781, 0x8b116d18, 0xde56707e],
                            [0xdb58433b, 0x1de4ce67, 0x15fcbcee, 0x1df9de61]]));
    }
}
