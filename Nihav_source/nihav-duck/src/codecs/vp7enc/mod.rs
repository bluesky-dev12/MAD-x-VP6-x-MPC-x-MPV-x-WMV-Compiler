use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

mod blocks;
mod coder;
use coder::*;
mod frame_coder;
use frame_coder::*;
mod mb_coding;
mod models;
use models::*;
mod motion_est;
use motion_est::MVSearchMode;
mod rdo;
use rdo::*;

#[derive(PartialEq,Debug)]
enum EncodingState {
    Intra,
    Refinement,
    JustEncode,
}

#[allow(dead_code)]
struct VP7Encoder {
    stream:     Option<NAStreamRef>,
    pkt:        Option<NAPacket>,
    version:    u8,
    key_int:    u8,
    frmcount:   u8,
    width:      usize,
    height:     usize,
    mb_w:       usize,
    mb_h:       usize,

    metric:     RateDistMetric,
    fenc:       FrameEncoder,
    pmodels:    VP7Models,
    br_ctl:     BitRateControl,

    last_frame: NABufferType,
    gold_frame: NABufferType,
    last_gold:  bool,
    me_mode:    MVSearchMode,
    me_range:   i16,
    lf_level:   Option<u8>,

    mbt_depth:  usize,
    mb_weight:  Vec<usize>,
    mb_map:     Vec<usize>,
    mb_map2:    Vec<usize>,
    qframes:    Vec<NAFrame>,
    frm_pool:   NAVideoBufferPool<u8>,
    i_frame:    NAVideoBufferRef<u8>,
    enc_state:  EncodingState,
}

impl VP7Encoder {
    fn new() -> Self {
        let vt = alloc_video_buffer(NAVideoInfo::new(24, 24, false, YUV420_FORMAT), 4).unwrap();
        let mc_buf1 = vt.get_vbuf().unwrap();
        let vt = alloc_video_buffer(NAVideoInfo::new(24, 24, false, YUV420_FORMAT), 4).unwrap();
        let mc_buf2 = vt.get_vbuf().unwrap();
        let vt = alloc_video_buffer(NAVideoInfo::new(24, 24, false, YUV420_FORMAT), 4).unwrap();
        let i_frame = vt.get_vbuf().unwrap();
        Self {
            stream:     None,
            pkt:        None,
            version:    0,
            key_int:    10,
            frmcount:   0,
            width:      0,
            height:     0,
            mb_w:       0,
            mb_h:       0,

            metric:     RateDistMetric::new(),
            fenc:       FrameEncoder::new(mc_buf1, mc_buf2),
            pmodels:    VP7Models::new(),
            br_ctl:     BitRateControl::new(),

            last_frame: NABufferType::None,
            gold_frame: NABufferType::None,
            last_gold:  false,
            me_mode:    MVSearchMode::default(),
            me_range:   16,
            lf_level:   None,

            mbt_depth:  0,
            mb_weight:  Vec::new(),
            mb_map:     Vec::new(),
            mb_map2:    Vec::new(),
            qframes:    Vec::new(),
            frm_pool:   NAVideoBufferPool::new(0),
            i_frame,
            enc_state:  EncodingState::JustEncode,
        }
    }
    fn encode_frame(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        if let Some(ref vbuf) = buf.get_vbuf() {
            self.fenc.set_me_params(self.me_mode, self.me_range, self.version);
            self.fenc.load_frame(vbuf);

            let mut dbuf = Vec::with_capacity(4);
            let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
            let mut bw   = ByteWriter::new(&mut gw);

            let is_intra = self.frmcount == 0;
            let golden_frame = is_intra;

            self.br_ctl.set_key_interval(self.key_int);
            let cur_quant = self.br_ctl.get_frame_quant(is_intra);

            if let Some(level) = self.lf_level {
                self.fenc.loop_params.loop_filter_level = level;
            } else {
                self.fenc.loop_params.loop_filter_level = if cur_quant <= 16 { 0 } else { (cur_quant / 4) as u8 };
            }

            if is_intra {
                self.pmodels.reset();
                let mbt_frames = self.mbt_depth.min(self.key_int as usize);
                self.fenc.intra_blocks(cur_quant, &self.metric, &self.pmodels, if mbt_frames > 0 { Some(&self.mb_weight) } else { None });
            } else {
                let gold_ref = if !self.last_gold { &self.gold_frame } else { &NABufferType::None };
                self.fenc.inter_blocks(cur_quant, &self.metric, &self.pmodels, &self.last_frame, gold_ref);
            }

            let mut stats = VP7ModelsStat::new();
            let mut models = self.pmodels;
            self.fenc.generate_models(is_intra, &mut stats);
            stats.generate(&mut models, is_intra);

            bw.write_u24le(0)?; // frame tag
            if self.version == 0 {
                bw.write_byte(0)?; // unused
            }

            let start = bw.tell();

            let mut bc = BoolEncoder::new(&mut bw);
            if is_intra {
                bc.put_bits(self.width  as u32, 12)?;
                bc.put_bits(self.height as u32, 12)?;
                bc.put_bits(0, 2)?; // scale vertical
                bc.put_bits(0, 2)?; // scale horizontal
            }

            self.fenc.encode_features(&mut bc, cur_quant, &models)?;

            bc.put_bits(cur_quant as u32, 7)?; // y_ac_q
            bc.put_bool(false, 128)?; // y_dc_q
            bc.put_bool(false, 128)?; // y2_ac_q
            bc.put_bool(false, 128)?; // y2_dc_q
            bc.put_bool(false, 128)?; // uv_ac_q
            bc.put_bool(false, 128)?; // uv_dc_q

            if !is_intra {
                bc.put_bool(false, 128)?; // update golden frame
            }

            let has_fading = self.version == 0 || is_intra;
            if self.version != 0 {
                bc.put_bool(true, 128)?; // keep probabilities
                if !is_intra {
                    bc.put_bool(false, 128)?; // has fading feature
                }
            }
            if has_fading {
                bc.put_bool(false, 128)?; // fading
            }

            if self.version == 0 {
                bc.put_bool(self.fenc.loop_params.lf_simple, 128)?;
            }

            // scan
            bc.put_bool(false, 128)?;

            if self.version != 0 {
                bc.put_bool(self.fenc.loop_params.lf_simple, 128)?;
            }

            bc.put_bits(u32::from(self.fenc.loop_params.loop_filter_level), 6)?;
            bc.put_bits(u32::from(self.fenc.loop_params.loop_sharpness), 3)?;

            encode_dct_coef_prob_upd(&mut bc, &models.coef_probs, &self.pmodels.coef_probs)?;

            if !is_intra {
                bc.put_byte(models.prob_intra_pred)?;
                bc.put_byte(models.prob_last_pred)?;

                let ymode_differs = models.kf_ymode_prob != self.pmodels.kf_ymode_prob;
                bc.put_bool(ymode_differs, 128)?;
                if ymode_differs {
                    for &el in models.kf_ymode_prob.iter() {
                        bc.put_byte(el)?;
                    }
                }

                let uvmode_differs = models.kf_uvmode_prob != self.pmodels.kf_uvmode_prob;
                bc.put_bool(uvmode_differs, 128)?;
                if uvmode_differs {
                    for &el in models.kf_uvmode_prob.iter() {
                        bc.put_byte(el)?;
                    }
                }

                encode_mv_prob_upd(&mut bc, &models.mv_probs, &self.pmodels.mv_probs)?;
            }

            self.fenc.encode_mb_types(&mut bc, is_intra, &models)?;

            bc.flush()?;
            let end = bw.tell();

            let mut bc = BoolEncoder::new(&mut bw);
            self.fenc.encode_residues(&mut bc, &models)?;
            bc.flush()?;

            bw.seek(SeekFrom::Start(0))?;
            bw.write_u24le((((end - start) as u32) << 4) |
                           (u32::from(self.version) << 1) |
                           if is_intra { 0 } else { 1 })?;

            let cur_size = dbuf.len();

            self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, is_intra, dbuf));

            self.pmodels = models;

            if self.key_int > 0 {
                self.frmcount += 1;
            }
            if self.frmcount == self.key_int {
                self.frmcount = 0;
            }

            if let Some(ref mut vbuf) = self.last_frame.get_vbuf() {
                let mut frm = NASimpleVideoFrame::from_video_buf(vbuf).unwrap();
                self.fenc.reconstruct_frame(&mut frm, is_intra);
            }
            self.last_gold = golden_frame;
            if golden_frame {
                let mut dfrm = self.gold_frame.get_vbuf().unwrap();
                let src = self.last_frame.get_vbuf().unwrap();

                let dst = dfrm.get_data_mut().unwrap();
                dst.copy_from_slice(src.get_data());
            }

            self.br_ctl.update(cur_size);
            if self.br_ctl.has_bitrate() {
                let tgt_size = (self.br_ctl.get_target_size(is_intra) / 8) as usize;
                self.metric.adjust_br(cur_size, tgt_size);
            }

            Ok(())
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
}

impl NAEncoder for VP7Encoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, false, YUV420_FORMAT)),
                    ..Default::default() })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let outinfo = NAVideoInfo::new((vinfo.width + 15) & !15, (vinfo.height + 15) & !15, false, YUV420_FORMAT);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { 0 }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                if vinfo.format != YUV420_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if ((vinfo.width | vinfo.height) & 15) != 0 {
                    return Err(EncoderError::FormatError);
                }
                if (vinfo.width | vinfo.height) >= (1 << 12) {
                    return Err(EncoderError::FormatError);
                }

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, false, vinfo.format);
                let info = NACodecInfo::new("vp7", NACodecTypeInfo::Video(out_info), None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.last_frame = alloc_video_buffer(out_info, 4)?;
                self.gold_frame = alloc_video_buffer(out_info, 4)?;

                self.stream = Some(stream.clone());

                self.width  = vinfo.width;
                self.height = vinfo.height;
                self.mb_w = (vinfo.width  + 15) >> 4;
                self.mb_h = (vinfo.height + 15) >> 4;
                self.fenc.resize(self.mb_w, self.mb_h);

                self.br_ctl.set_params(encinfo.tb_num, encinfo.tb_den, encinfo.bitrate, self.key_int, self.mb_w * self.mb_h);

                self.frm_pool.reset();
                self.frm_pool.set_dec_bufs(self.mbt_depth + 1);
                self.frm_pool.prealloc_video(out_info, 4)?;
                self.i_frame = self.frm_pool.get_free().unwrap();
                self.mb_weight.resize(self.mb_w * self.mb_h, 0);
                self.mb_map.resize(self.mb_w * self.mb_h, 0);
                self.mb_map2.resize(self.mb_w * self.mb_h, 0);
                self.qframes.clear();
                self.enc_state = if self.mbt_depth.min(self.key_int as usize) > 0 {
                        EncodingState::Intra
                    } else {
                        EncodingState::JustEncode
                    };

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        if let Some(ref vbuf) = frm.get_buffer().get_vbuf() {
            let mbt_frames = self.mbt_depth.min(self.key_int as usize);
            if !self.qframes.is_empty() || (mbt_frames > 0 && self.enc_state != EncodingState::JustEncode) {
                if let Some(dbuf) = self.frm_pool.get_copy(vbuf) {
                    let newfrm = NAFrame::new(frm.ts, frm.frame_type, frm.key, frm.get_info(), NABufferType::Video(dbuf));
                    if self.enc_state == EncodingState::Intra {
                        for (i, el) in self.mb_map.iter_mut().enumerate() {
                            *el = i;
                        }
                        for el in self.mb_weight.iter_mut() {
                            *el = 1;
                        }
                        let frm = NASimpleVideoFrame::from_video_buf(&mut self.i_frame).unwrap();
                        let src = vbuf.get_data();
                        for plane in 0..3 {
                            let soff = vbuf.get_offset(plane);
                            let sstride = vbuf.get_stride(plane);
                            let copy_len = sstride.min(frm.stride[plane]);
                            for (dst, src) in frm.data[frm.offset[plane]..].chunks_mut(frm.stride[plane]).zip(src[soff..].chunks(sstride)).take(frm.height[plane]) {
                                dst[..copy_len].copy_from_slice(&src[..copy_len]);
                            }
                        }
                        self.enc_state = EncodingState::Refinement;
                    } else {
                        self.fenc.set_me_params(self.me_mode, self.me_range, self.version);
                        self.fenc.load_frame(vbuf);
                        self.fenc.mb_tree_search(self.i_frame.clone(), &self.mb_map, &mut self.mb_map2, &mut self.mb_weight);
                        std::mem::swap(&mut self.mb_map, &mut self.mb_map2);
                    }
                    self.qframes.push(newfrm);
                    Ok(())
                } else {
                    self.enc_state = EncodingState::JustEncode;
                    self.encode_frame(frm)
                }
            } else {
                self.encode_frame(frm)
            }
        } else {
            Err(EncoderError::FormatError)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        let mbt_frames = self.mbt_depth.min(self.key_int as usize);
        if self.qframes.len() >= mbt_frames {
            self.enc_state = EncodingState::JustEncode;
        }
        if self.pkt.is_none() && !self.qframes.is_empty() && self.enc_state == EncodingState::JustEncode {
            let frm = self.qframes.remove(0);
            self.encode_frame(&frm)?;
            if self.qframes.is_empty() && self.mbt_depth > 0 && self.frmcount == 0 {
                self.enc_state = EncodingState::Intra;
            }
        }
        let mut npkt = None;
        std::mem::swap(&mut self.pkt, &mut npkt);
        Ok(npkt)
    }
    fn flush(&mut self) -> EncoderResult<()> {
        self.frmcount = 0;
        self.enc_state = EncodingState::JustEncode;
        Ok(())
    }
}

const VERSION_OPTION: &str = "version";
const LF_LEVEL_OPTION: &str = "lf_level";
const LF_SHARP_OPTION: &str = "lf_sharpness";
const LF_SIMPLE_OPTION: &str = "lf_simple";
const QUANT_OPTION: &str = "quant";
const MV_SEARCH_OPTION: &str = "mv_mode";
const MV_RANGE_OPTION: &str = "mv_range";
const MBTREE_DEPTH: &str = "mbtree_depth";

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: KEYFRAME_OPTION, description: KEYFRAME_OPTION_DESC,
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
    NAOptionDefinition {
        name: VERSION_OPTION, description: "internal codec version",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(1)) },
    NAOptionDefinition {
        name: LF_LEVEL_OPTION, description: "loop filter level (-1 = automatic)",
        opt_type: NAOptionDefinitionType::Int(Some(-1), Some(63)) },
    NAOptionDefinition {
        name: LF_SHARP_OPTION, description: "loop filter sharpness",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(7)) },
    NAOptionDefinition {
        name: LF_SIMPLE_OPTION, description: "use simple loop filter",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: QUANT_OPTION, description: "force fixed quantiser for encoding",
        opt_type: NAOptionDefinitionType::Int(Some(-1), Some(127)) },
    NAOptionDefinition {
        name: MV_SEARCH_OPTION, description: "motion search mode",
        opt_type: NAOptionDefinitionType::String(Some(&["sea", "dia", "hex", "epzs"])) },
    NAOptionDefinition {
        name: MV_RANGE_OPTION, description: "motion search range (in pixels)",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(30)) },
    NAOptionDefinition {
        name: MBTREE_DEPTH, description: "number of frames in MB tree analysis buffer",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
];

impl NAOptionHandler for VP7Encoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        KEYFRAME_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.key_int = intval as u8;
                            }
                        },
                        VERSION_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.version = intval as u8;
                            }
                        },
                        LF_LEVEL_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.lf_level = if intval < 0 { None } else { Some(intval as u8) };
                            }
                        },
                        LF_SHARP_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.fenc.loop_params.loop_sharpness = intval as u8;
                            }
                        },
                        LF_SIMPLE_OPTION => {
                            if let NAValue::Bool(flag) = option.value {
                                self.fenc.loop_params.lf_simple = flag;
                            }
                        },
                        QUANT_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.br_ctl.set_quant(if intval < 0 { None } else { Some(intval as usize) });
                            }
                        },
                        MV_SEARCH_OPTION => {
                            if let NAValue::String(ref string) = option.value {
                                if let Ok(mv_mode) = string.parse::<MVSearchMode>() {
                                    self.me_mode = mv_mode;
                                }
                            }
                        },
                        MV_RANGE_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.me_range = intval as i16;
                            }
                        },
                        MBTREE_DEPTH => {
                            if let NAValue::Int(intval) = option.value {
                                self.mbt_depth = intval as usize;
                            }
                        },
                        _ => {},
                    };
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            KEYFRAME_OPTION => Some(NAValue::Int(i64::from(self.key_int))),
            VERSION_OPTION => Some(NAValue::Int(i64::from(self.version))),
            QUANT_OPTION => if let Some(q) = self.br_ctl.get_quant() {
                    Some(NAValue::Int(q as i64))
                } else {
                    Some(NAValue::Int(-1))
                },
            LF_LEVEL_OPTION => if let Some(lev) = self.lf_level {
                    Some(NAValue::Int(i64::from(lev)))
                } else {
                    Some(NAValue::Int(-1))
                },
            LF_SHARP_OPTION => Some(NAValue::Int(i64::from(self.fenc.loop_params.loop_sharpness))),
            LF_SIMPLE_OPTION => Some(NAValue::Bool(self.fenc.loop_params.lf_simple)),
            MV_SEARCH_OPTION => Some(NAValue::String(self.me_mode.to_string())),
            MV_RANGE_OPTION => Some(NAValue::Int(i64::from(self.me_range))),
            MBTREE_DEPTH => Some(NAValue::Int(self.mbt_depth as i64)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(VP7Encoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_commonfmt::*;
    use nihav_codec_support::test::enc_video::*;

    fn encode_test(out_name: &'static str, enc_options: &[NAOption], hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        duck_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/VP4/ot171_vp40.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Duck/ot171_vp40.avi",
                stream_type:    StreamType::Video,
                limit:          Some(9),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "vp7",
                out_name,
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  YUV420_FORMAT,
                flipped: false,
                bits:    12,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 50000,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          hash);
    }
    #[test]
    fn test_vp7_encoder() {
        let enc_options = &[
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(42) },
            ];
        encode_test("vp7-q42.avi", enc_options, &[0xa5079e5b, 0x33dd8a63, 0xfc189e21, 0xee08332b]);
    }
    #[test]
    fn test_vp7_encoder_noloop() {
        let enc_options = &[
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(42) },
                NAOption { name: super::LF_LEVEL_OPTION, value: NAValue::Int(0) },
            ];
        encode_test("vp7-noloop.avi", enc_options, &[0xc7d41732, 0x09b03059, 0x8550921c, 0xa99d4c29]);
    }
    #[test]
    fn test_vp7_encoder_mbtree() {
        let enc_options = &[
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(24) },
                NAOption { name: super::MBTREE_DEPTH, value: NAValue::Int(10) },
            ];
        encode_test("vp7-mbt.avi", enc_options, &[0xd0d90d31, 0x0253275d, 0xbe502d3c, 0xacf2b6e7]);
    }
    #[test]
    fn test_vp7_encoder_ratectl() {
        let enc_options = &[
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(-1) },
            ];
        encode_test("vp7-br.avi", enc_options, &[0x47dcd4da, 0x04b06feb, 0x386163c1, 0x54899da3]);
    }
}
