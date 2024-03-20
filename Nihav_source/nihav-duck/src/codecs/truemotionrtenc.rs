use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitwriter::*;

const TMRT_DELTA_TAB: [&[i16]; 3] = [
    &[ 5, -7, 36, -36 ],
    &[ 2, -3, 8, -8, 18, -18, 36, -36 ],
    &[ 1, -1, 2, -3, 8, -8, 18, -18, 36, -36, 54, -54, 96, -96, 144, -144 ]
];

const FIRST_NODE: u8 = 255;
const ERR_MAX: u32 = std::u32::MAX;

#[derive(Clone, Copy, Default)]
struct TrellisNode {
    err:        u32,
    hpred:      i16,
    idx:        u8,
}

struct TMRTEncoder {
    stream:     Option<NAStreamRef>,
    pkt:        Option<NAPacket>,
    dbits:      u8,
    hscale:     bool,
    do_trellis: bool,
    top_line:   Vec<u8>,
    trellis:    Vec<TrellisNode>,
    indices:    Vec<u8>,
}

fn find_delta(cur_delta: i16, delta_tab: &[i16]) -> (i16, usize) {
    let mut idx = 0;
    let mut best_diff = 512;
    let mut ndelta = delta_tab[0];
    for (i, &delta) in delta_tab.iter().enumerate() {
        let ddiff = (delta - cur_delta).abs();
        if ddiff < best_diff {
            idx = i;
            best_diff = ddiff;
            ndelta = delta;
        }
    }
    (ndelta, idx)
}

impl TMRTEncoder {
    fn new() -> Self {
        Self {
            stream:     None,
            pkt:        None,
            dbits:      4,
            hscale:     false,
            do_trellis: false,
            top_line:   Vec::new(),
            trellis:    Vec::new(),
            indices:    Vec::new(),
        }
    }
    fn encode_plane(&mut self, bw: &mut BitWriter, in_frm: &NAVideoBuffer<u8>, plane_no: usize) -> EncoderResult<()> {
        let (width, height) = in_frm.get_dimensions(plane_no);

        let stride = in_frm.get_stride(plane_no);
        let offset = in_frm.get_offset(plane_no);
        let src = in_frm.get_data();

        let delta_tab = TMRT_DELTA_TAB[(self.dbits - 2) as usize];
        self.top_line.clear();
        self.top_line.resize(width, if plane_no == 0 { 0 } else { 0x80 });

        let step = if self.hscale { 2 } else { 1 };
        for line in src[offset..].chunks(stride).take(height) {
            let mut hor_pred = 0;
            for (&cur, pred) in line[..width].iter().zip(self.top_line.iter_mut()).step_by(step) {
                let cur = i16::from(cur);
                let cur_delta = cur - i16::from(*pred) - hor_pred;

                let (ndelta, idx) = find_delta(cur_delta, delta_tab);

                bw.write(idx as u32, self.dbits);
                hor_pred += ndelta;
                *pred = (i16::from(*pred) + hor_pred).max(0).min(255) as u8;
            }
        }

        Ok(())
    }

    fn encode_plane_trellis(&mut self, bw: &mut BitWriter, in_frm: &NAVideoBuffer<u8>, plane_no: usize) -> EncoderResult<()> {
        let (width, height) = in_frm.get_dimensions(plane_no);

        let stride = in_frm.get_stride(plane_no);
        let offset = in_frm.get_offset(plane_no);
        let src = in_frm.get_data();

        let delta_tab = TMRT_DELTA_TAB[(self.dbits - 2) as usize];
        self.top_line.clear();
        self.top_line.resize(width, if plane_no == 0 { 0 } else { 0x80 });

        let trellis_size = delta_tab.len();
        self.trellis.resize(trellis_size * (width + 1), TrellisNode::default());
        self.indices.resize(width, 0);
        for node in self.trellis[..trellis_size].iter_mut() {
            node.idx = FIRST_NODE;
        }

        let step = if self.hscale { 2 } else { 1 };
        for line in src[offset..].chunks(stride).take(height) {
            let mut tsplit = trellis_size;
            for (&cur, &pred) in line[..width].iter().zip(self.top_line.iter()).step_by(step) {
                let (tprev, tcur) = self.trellis.split_at_mut(tsplit);
                let hist = &tprev[tprev.len() - trellis_size..];

                let pix_val = i32::from(cur);
                let top_val = i16::from(pred);
                for (dst, &delta) in tcur.iter_mut().zip(delta_tab.iter()) {
                    dst.err = ERR_MAX;
                    for (idx, src) in hist.iter().enumerate() {
                        if src.err == ERR_MAX {
                            continue;
                        }
                        let nval = i32::from((top_val + src.hpred + delta).max(0).min(255));
                        let new_err = src.err + (((nval - pix_val) * (nval - pix_val)) as u32);
                        if new_err < dst.err {
                            dst.err = new_err;
                            dst.idx = idx as u8;
                            dst.hpred = src.hpred + delta;
                        }
                    }
                }

                tsplit += trellis_size;
            }

            tsplit -= trellis_size;
            let mut best_idx = 0;
            let mut best_err = self.trellis[tsplit].err;

            for (idx, node) in self.trellis[tsplit..].iter().take(trellis_size).enumerate() {
                if node.err < best_err {
                    best_idx = idx as u8;
                    best_err = node.err;
                }
            }

            let mut cur_idx = best_idx;
            for dst in self.indices[..width / step].iter_mut().rev() {
                *dst = cur_idx;
                cur_idx = self.trellis[tsplit + (cur_idx as usize)].idx;
                tsplit -= trellis_size;
            }

            let mut hor_pred = 0;
            for (pred, &idx) in self.top_line.iter_mut().step_by(step).zip(self.indices.iter()) {
                bw.write(u32::from(idx), self.dbits);
                hor_pred += delta_tab[usize::from(idx)];
                *pred = (i16::from(*pred) + hor_pred).max(0).min(255) as u8;
            }
        }

        Ok(())
    }
}

impl NAEncoder for TMRTEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                        format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, true, YUV410_FORMAT)),
                        ..Default::default()
                    })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let pix_fmt = YUV410_FORMAT;
                let outinfo = NAVideoInfo::new((vinfo.width + 3) & !3, (vinfo.height + 3) & !3, false, pix_fmt);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_PARAMCHANGE }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                if vinfo.format != YUV410_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if ((vinfo.width | vinfo.height) & 3) != 0 {
                    return Err(EncoderError::FormatError);
                }
                if (vinfo.width | vinfo.height) >= (1 << 10) {
                    return Err(EncoderError::FormatError);
                }

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, false, vinfo.format);
                let info = NACodecInfo::new("truemotionrt", NACodecTypeInfo::Video(out_info), None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        if let Some(ref vbuf) = buf.get_vbuf() {
            let mut dbuf = Vec::with_capacity(4);
            let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
            let mut bw   = ByteWriter::new(&mut gw);

            bw.write_byte(0)?; // header size
            bw.write_byte(17)?;
            bw.write_byte(self.dbits)?;
            bw.write_byte(5)?;
            bw.write_byte(self.hscale as u8)?;
            bw.write_byte(0)?; // pad
            let (width, height) = vbuf.get_dimensions(0);
            bw.write_u16le(height as u16)?;
            bw.write_u16le(width as u16)?;
            bw.write_u16le(0)?; // pad
            bw.write_u32le(0)?; // full frame_size
            bw.write_u16le(0)?; // pad

            let hdr_size = bw.tell() as usize;

            bw.write_u32le(0)?; // data size

            let mut bw = BitWriter::new(dbuf, BitWriterMode::LE);
            if !self.do_trellis {
                for plane in 0..3 {
                    self.encode_plane(&mut bw, vbuf, plane)?;
                }
            } else {
                for plane in 0..3 {
                    self.encode_plane_trellis(&mut bw, vbuf, plane)?;
                }
            }
            dbuf = bw.end();

            let frame_size = dbuf.len() as u32;
            write_u32le(&mut dbuf[12..], frame_size)?;
            write_u32le(&mut dbuf[hdr_size..], frame_size - (hdr_size as u32) - 4)?;

            dbuf[0] = ((hdr_size | 0x80) as u8).rotate_right(3);
            for i in (1..hdr_size).rev() {
                dbuf[i] ^= dbuf[i + 1];
            }

            self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, true, dbuf));
            Ok(())
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        let mut npkt = None;
        std::mem::swap(&mut self.pkt, &mut npkt);
        Ok(npkt)
    }
    fn flush(&mut self) -> EncoderResult<()> {
        Ok(())
    }
}

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: "bits", description: "Bits per delta",
        opt_type: NAOptionDefinitionType::Int(Some(2), Some(4)) },
    NAOptionDefinition {
        name: "hscale", description: "Horizontal scaling mode",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: "trellis", description: "Trellis search for optimal deltas",
        opt_type: NAOptionDefinitionType::Bool },
];

impl NAOptionHandler for TMRTEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        "bits" => {
                            if let NAValue::Int(val) = option.value {
                                self.dbits = val as u8;
                            }
                        },
                        "hscale" => {
                            if let NAValue::Bool(val) = option.value {
                                self.hscale = val;
                            }
                        },
                        "trellis" => {
                            if let NAValue::Bool(val) = option.value {
                                self.do_trellis = val;
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
            "bits" => Some(NAValue::Int(i64::from(self.dbits))),
            "hscale" => Some(NAValue::Bool(self.hscale)),
            "trellis" => Some(NAValue::Bool(self.do_trellis)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(TMRTEncoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_commonfmt::*;
    use nihav_codec_support::test::enc_video::*;

    fn encode_test(name: &'static str, enc_options: &[NAOption], hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        duck_register_all_encoders(&mut enc_reg);

        // sample from private collection
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Duck/tr20_low.avi",
                stream_type:    StreamType::Video,
                limit:          Some(1),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "truemotionrt",
                out_name:       name,
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  YUV410_FORMAT,
                flipped: false,
                bits:    9,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options, hash);
    }
    #[test]
    fn test_truemotionrt_encoder_2bit() {
        let enc_options = &[
                NAOption { name: "bits", value: NAValue::Int(2) },
            ];
        encode_test("tmrt-2bit.avi", enc_options, &[0x2c2a5ae3, 0xde1646e4, 0xf76bb219, 0xd09602fa]);
    }
    #[test]
    fn test_truemotionrt_encoder_3bit() {
        let enc_options = &[
                NAOption { name: "bits", value: NAValue::Int(3) },
            ];
        encode_test("tmrt-3bit.avi", enc_options, &[0x36cf8f48, 0x3e8ff2ce, 0x6f3822cf, 0xf7fbf19d]);
    }
    #[test]
    fn test_truemotionrt_encoder_4bit() {
        let enc_options = &[
                NAOption { name: "bits", value: NAValue::Int(4) },
            ];
        encode_test("tmrt-4bit.avi", enc_options, &[0xa5a7fbe3, 0x7bac0b2b, 0x2af6f97f, 0xa65cd1fc]);
    }
    #[test]
    fn test_truemotionrt_encoder_hscale() {
        let enc_options = &[
                NAOption { name: "bits", value: NAValue::Int(3) },
                NAOption { name: "hscale", value: NAValue::Bool(true) },
            ];
        encode_test("tmrt-hscale.avi", enc_options, &[0xc17afa21, 0x5bdf49c9, 0x57997840, 0xfc2f17b6]);
    }
    #[test]
    fn test_truemotionrt_encoder_trellis() {
        let enc_options = &[
                NAOption { name: "bits", value: NAValue::Int(3) },
                NAOption { name: "hscale", value: NAValue::Bool(true) },
                NAOption { name: "trellis", value: NAValue::Bool(true) },
            ];
        encode_test("tmrt-trellis.avi", enc_options, &[0x3586b450, 0x6ea1ed31, 0xe14c0c7d, 0x0886bc4f]);
    }
}
