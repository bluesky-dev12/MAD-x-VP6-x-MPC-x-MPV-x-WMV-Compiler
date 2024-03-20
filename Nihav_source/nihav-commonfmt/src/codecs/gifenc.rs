use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitwriter::*;

const GRAY_FORMAT: NAPixelFormaton = NAPixelFormaton {
        model: ColorModel::YUV(YUVSubmodel::YUVJ),
        components: 1,
        comp_info: [Some(NAPixelChromaton{h_ss: 0, v_ss: 0, packed: false, depth: 8, shift: 0, comp_offs: 0, next_elem: 1}), None, None, None, None],
        elem_size: 1,
        be: true,
        alpha: false,
        palette: false,
    };

#[derive(Clone,Copy,Default,PartialEq)]
enum CompressionLevel {
    None,
    Fast,
    #[default]
    Best
}

impl std::string::ToString for CompressionLevel {
    fn to_string(&self) -> String {
        match *self {
            CompressionLevel::None => "none".to_string(),
            CompressionLevel::Fast => "fast".to_string(),
            CompressionLevel::Best => "best".to_string(),
        }
    }
}

const NO_CODE: u16 = 0;

struct LZWDictionary {
    cur_size:   usize,
    bit_len:    u8,
    clear_code: u16,
    end_code:   u16,
    orig_len:   u8,
    trie:       Vec<[u16; 257]>,
}

impl LZWDictionary {
    fn new() -> Self {
        Self {
            trie:       Vec::with_capacity(4096),
            cur_size:   0,
            bit_len:    0,
            clear_code: 0,
            end_code:   0,
            orig_len:   0,
        }
    }
    fn init(&mut self, bits: u8) {
        self.cur_size   = (1 << bits) + 2;
        self.bit_len    = bits + 1;
        self.clear_code = 1 << bits;
        self.end_code   = self.clear_code + 1;
        self.orig_len   = self.bit_len;

        self.trie.clear();
        for _ in 0..self.cur_size {
            self.trie.push([NO_CODE; 257]);
        }
        for (idx, nodes) in self.trie.iter_mut().enumerate() {
            nodes[256] = idx as u16;
        }
    }
    fn find(&self, src: &[u8]) -> (u16, usize, usize) {
        let mut idx = usize::from(src[0]);
        let mut last_len = 0;
        for (pos, &next) in src.iter().enumerate().skip(1) {
            let next = usize::from(next);
            if self.trie[idx][next] != NO_CODE {
                idx = usize::from(self.trie[idx][next]);
            } else {
                return (self.trie[idx][256], pos, idx);
            }
            last_len = pos;
        }
        (self.trie[idx][256], last_len + 1, idx)
    }
    fn add(&mut self, lastidx: usize, next: u8) {
        if self.cur_size >= (1 << 12) {
            return;
        }
        let next = usize::from(next);
        if self.trie[lastidx][next] == NO_CODE {
            let newnode = self.trie.len();
            self.trie.push([NO_CODE; 257]);
            self.trie[newnode][256] = self.cur_size as u16;
            self.trie[lastidx][next] = newnode as u16;
        }
        if (self.cur_size & (self.cur_size - 1)) == 0 && self.bit_len < 12 {
            self.bit_len += 1;
        }
        self.cur_size += 1;
    }
    fn reset(&mut self) {
        self.bit_len  = self.orig_len;
        self.cur_size = usize::from(self.end_code) + 1;
        self.trie.truncate(self.cur_size);
        for nodes in self.trie.iter_mut() {
            for el in nodes[..256].iter_mut() {
                *el = NO_CODE;
            }
        }
    }
}

struct LZWEncoder {
    dict:       LZWDictionary,
    level:      CompressionLevel,
    tmp:        Vec<u8>,
}

impl LZWEncoder {
    fn new() -> Self {
        Self {
            dict:       LZWDictionary::new(),
            level:      CompressionLevel::default(),
            tmp:        Vec::new(),
        }
    }
    fn compress(&mut self, writer: &mut ByteWriter, src: &[u8]) -> EncoderResult<()> {
        let clr_bits: u8 = if self.level != CompressionLevel::None {
                let maxclr = u16::from(src.iter().fold(0u8, |acc, &a| acc.max(a))) + 1;
                let mut bits = 2;
                while (1 << bits) < maxclr {
                    bits += 1;
                }
                bits
            } else { 8 };

        self.dict.init(clr_bits);

        self.tmp.clear();
        let mut tbuf = Vec::new();
        std::mem::swap(&mut tbuf, &mut self.tmp);
        let mut bw = BitWriter::new(tbuf, BitWriterMode::LE);

        bw.write(u32::from(self.dict.clear_code), self.dict.bit_len);

        match self.level {
            CompressionLevel::None => {
                let sym_limit = 1 << (clr_bits + 1);
                for &b in src.iter() {
                    if self.dict.cur_size >= sym_limit {
                        bw.write(u32::from(self.dict.clear_code), self.dict.bit_len);
                        self.dict.reset();
                    }
                    bw.write(u32::from(b), self.dict.bit_len);
                    self.dict.add(usize::from(b), 0);
                }
            },
            CompressionLevel::Fast => {
                let mut pos = 0;
                while pos < src.len() {
                    let (idx, len, trieidx) = self.dict.find(&src[pos..]);
                    bw.write(u32::from(idx), self.dict.bit_len);
                    pos += len;
                    if pos < src.len() {
                        self.dict.add(trieidx, src[pos]);
                    }
                    if self.dict.cur_size == 4096 {
                        bw.write(u32::from(self.dict.clear_code), self.dict.bit_len);
                        self.dict.reset();
                    }
                }
            },
            CompressionLevel::Best => {
                let mut pos = 0;
                let mut hist = [0; 16];
                let mut avg = 0;
                let mut avg1 = 0;
                let mut hpos = 0;
                while pos < src.len() {
                    let (idx, len, trieidx) = self.dict.find(&src[pos..]);
                    bw.write(u32::from(idx), self.dict.bit_len);
                    pos += len;
                    if pos >= src.len() {
                        break;
                    }
                    self.dict.add(trieidx, src[pos]);

                    avg1 -= hist[(hpos + 1) & 0xF];
                    avg1 += len;
                    if self.dict.cur_size == 4096 && (avg1 < avg - avg / 8) {
                        bw.write(u32::from(self.dict.clear_code), self.dict.bit_len);
                        self.dict.reset();
                    }
                    avg = avg1;
                    hpos = (hpos + 1) & 0xF;
                    hist[hpos] = len;
                }
            },
        };

        bw.write(u32::from(self.dict.end_code), self.dict.bit_len);
        tbuf = bw.end();
        std::mem::swap(&mut tbuf, &mut self.tmp);

        writer.write_byte(clr_bits)?;
        for chunk in self.tmp.chunks(255) {
            writer.write_byte(chunk.len() as u8)?;
            writer.write_buf(chunk)?;
        }
        writer.write_byte(0x00)?; // data end marker
        Ok(())
    }
}

struct GIFEncoder {
    stream:     Option<NAStreamRef>,
    cur_frm:    Vec<u8>,
    prev_frm:   Vec<u8>,
    tmp_buf:    Vec<u8>,
    pal:        [u8; 768],
    pkt:        Option<NAPacket>,
    first:      bool,
    width:      usize,
    height:     usize,
    grayscale:  bool,
    lzw:        LZWEncoder,
    p_trans:    bool,
    tr_idx:     Option<u8>,
}

impl GIFEncoder {
    fn new() -> Self {
        Self {
            stream:     None,
            pkt:        None,
            cur_frm:    Vec::new(),
            prev_frm:   Vec::new(),
            pal:        [0; 768],
            tmp_buf:    Vec::new(),
            first:      true,
            width:      0,
            height:     0,
            grayscale:  false,
            lzw:        LZWEncoder::new(),
            p_trans:    false,
            tr_idx:     None,
        }
    }
    fn write_dummy_frame(&mut self, bw: &mut ByteWriter) -> EncoderResult<()> {
        let mut pix = [self.cur_frm[0]];
        if let (true, Some(tr_idx)) = (self.p_trans, self.tr_idx) {
            if tr_idx < pix[0] {
                pix[0] = tr_idx;
            }
        }

        // 1x1 image descriptor
        bw.write_buf(&[0x2C, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00])?;
        self.lzw.compress(bw, &pix)?;
        Ok(())
    }
}

impl NAEncoder for GIFEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, true, YUV420_FORMAT)),
                    ..Default::default()
                })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let format = if vinfo.format == GRAY_FORMAT { GRAY_FORMAT } else { PAL8_FORMAT };
                let outinfo = NAVideoInfo::new(vinfo.width, vinfo.height, false, format);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_SKIPFRAME }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                if vinfo.width > 65535 || vinfo.height > 65535 {
                    return Err(EncoderError::FormatError);
                }
                if vinfo.format != PAL8_FORMAT && vinfo.format != GRAY_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                self.width  = vinfo.width;
                self.height = vinfo.height;
                self.grayscale = vinfo.format == GRAY_FORMAT;

                let edata = self.tr_idx.map(|val| vec![val]);

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, false, PAL8_FORMAT);
                let info = NACodecInfo::new("gif", NACodecTypeInfo::Video(out_info), edata);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());

                self.cur_frm  = vec![0; vinfo.width * vinfo.height];
                self.prev_frm = vec![0; vinfo.width * vinfo.height];
                self.tmp_buf.clear();
                self.tmp_buf.reserve(vinfo.width * vinfo.height);

                self.first = true;

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let mut dbuf = Vec::with_capacity(4);
        let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
        let mut bw   = ByteWriter::new(&mut gw);

        self.tmp_buf.clear();

        match frm.get_buffer() {
            NABufferType::Video(ref buf) => {
                let src = buf.get_data();
                let stride = buf.get_stride(0);
                let src = &src[buf.get_offset(0)..];

                for (dline, sline) in self.cur_frm.chunks_exact_mut(self.width)
                        .zip(src.chunks_exact(stride)) {
                    dline.copy_from_slice(&sline[..self.width]);
                }

                let cur_pal = &src[buf.get_offset(1)..][..768];
                if self.first {
                    if !self.grayscale {
                        self.pal.copy_from_slice(cur_pal);
                    } else {
                        for (i, pal) in self.pal.chunks_exact_mut(3).enumerate() {
                            pal[0] = i as u8;
                            pal[1] = i as u8;
                            pal[2] = i as u8;
                        }
                    }
                }

                let mut pal_changed = false;
                if !self.first && !self.grayscale {
                    let mut used = [false; 256];
                    for &b in self.cur_frm.iter() {
                        used[usize::from(b)] = true;
                    }
                    for (&used, (pal1, pal2)) in used.iter()
                            .zip(self.pal.chunks_exact(3).zip(cur_pal.chunks_exact(3))) {
                        if used && (pal1 != pal2) {
                            pal_changed = true;
                            break;
                        }
                    }
                }

                if self.first {
                    bw.write_byte(0x2C)?; // image descriptor
                    bw.write_u16le(0)?; // left
                    bw.write_u16le(0)?; // top
                    bw.write_u16le(self.width as u16)?;
                    bw.write_u16le(self.height as u16)?;
                    bw.write_byte(0)?; // flags
                    self.lzw.compress(&mut bw, &self.cur_frm)?;
                } else {
                    let mut top = 0;
                    for (y, (line1, line2)) in self.cur_frm.chunks_exact(self.width)
                                .zip(self.prev_frm.chunks_exact(self.width)).enumerate() {
                        if line1 == line2 {
                            top = y;
                        } else {
                            break;
                        }
                    }
                    if top != self.height - 1 {
                        let mut bot = self.height;
                        for (y, (line1, line2)) in self.cur_frm.chunks_exact(self.width)
                                    .zip(self.prev_frm.chunks_exact(self.width)).enumerate().rev() {
                            if line1 == line2 {
                                bot = y + 1;
                            } else {
                                break;
                            }
                        }
                        let mut left  = self.width - 1;
                        let mut right = 0;
                        for (line1, line2) in self.cur_frm.chunks_exact(self.width)
                                    .zip(self.prev_frm.chunks_exact(self.width))
                                    .skip(top).take(bot - top) {
                            if left > 0 {
                                let mut cur_l = 0;
                                for (x, (&p1, &p2)) in line1.iter().zip(line2.iter()).enumerate() {
                                    if p1 == p2 {
                                        cur_l = x + 1;
                                    } else {
                                        break;
                                    }
                                }
                                left = left.min(cur_l);
                            }
                            if right < self.width {
                                let mut cur_r = self.width;
                                for (x, (&p1, &p2)) in line1.iter().zip(line2.iter())
                                        .enumerate().rev() {
                                    if p1 == p2 {
                                        cur_r = x + 1;
                                    } else {
                                        break;
                                    }
                                }
                                right = right.max(cur_r);
                            }
                        }
                        self.tmp_buf.clear();
                        let use_transparency = self.p_trans && self.tr_idx.is_some();
                        let full_frame = right == 0 && top == 0 && left == self.width && bot == self.height;

                        let pic = match (use_transparency, full_frame) {
                                (true, _) => {
                                    let tr_idx = self.tr_idx.unwrap_or(0);
                                    for (cline, pline) in self.cur_frm.chunks_exact(self.width)
                                            .zip(self.prev_frm.chunks_exact(self.width))
                                            .skip(top).take(bot - top) {
                                        for (&cpix, &ppix) in cline[left..right].iter()
                                                .zip(pline[left..right].iter()) {
                                            self.tmp_buf.push(if cpix == ppix { tr_idx } else { cpix });
                                        }
                                    }
                                    &self.tmp_buf
                                },
                                (false, true) => {
                                    &self.cur_frm
                                },
                                (false, false) => {
                                    for line in self.cur_frm.chunks_exact(self.width)
                                            .skip(top).take(bot - top) {
                                        self.tmp_buf.extend_from_slice(&line[left..right]);
                                    }
                                    &self.tmp_buf
                                },
                            };

                        bw.write_byte(0x2C)?; // image descriptor
                        bw.write_u16le(left as u16)?;
                        bw.write_u16le(top as u16)?;
                        bw.write_u16le((right - left) as u16)?;
                        bw.write_u16le((bot - top) as u16)?;
                        if !pal_changed {
                            bw.write_byte(0)?; // flags
                        } else {
                            let maxclr = pic.iter().fold(0u8, |acc, &a| acc.max(a));
                            let clr_bits = if maxclr > 128 {
                                    8
                                } else {
                                    let mut bits = 1;
                                    while (1 << bits) < maxclr {
                                        bits += 1;
                                    }
                                    bits
                                };
                            bw.write_byte(0x80 | (clr_bits - 1))?;
                            bw.write_buf(&cur_pal[..(3 << clr_bits)])?;
                        }
                        self.lzw.compress(&mut bw, pic)?;
                    } else {
                        self.write_dummy_frame(&mut bw)?;
                    }
                }
            },
            NABufferType::None if !self.first => {
                self.write_dummy_frame(&mut bw)?;
            },
            _ => return Err(EncoderError::InvalidParameters),
        };

        self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, self.first, dbuf));
        self.first = false;

        if !self.grayscale {
            if let NABufferType::Video(ref buf) = frm.get_buffer() {
                let paloff = buf.get_offset(1);
                let data = buf.get_data();
                let mut pal = [0; 1024];
                let srcpal = &data[paloff..][..768];
                for (dclr, sclr) in pal.chunks_exact_mut(4).zip(srcpal.chunks_exact(3)) {
                    dclr[..3].copy_from_slice(sclr);
                }
                if let Some(ref mut pkt) = &mut self.pkt {
                    pkt.side_data.push(NASideData::Palette(true, Arc::new(pal)));
                }
            }
        } else {
            let mut pal = [0; 1024];
            for (i, quad) in pal.chunks_exact_mut(4).enumerate() {
                quad[0] = i as u8;
                quad[1] = i as u8;
                quad[2] = i as u8;
            }
            if let Some(ref mut pkt) = &mut self.pkt {
                pkt.side_data.push(NASideData::Palette(true, Arc::new(pal)));
            }
        }

        std::mem::swap(&mut self.cur_frm, &mut self.prev_frm);
        Ok(())
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
        name: "compr", description: "Compression level",
        opt_type: NAOptionDefinitionType::String(Some(&["none", "fast", "best"])) },
    NAOptionDefinition {
        name: "inter_transparent", description: "Code changed regions with transparency",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: "transparent_idx", description: "Palette index to use for transparency (on inter frames too if requested)",
        opt_type: NAOptionDefinitionType::Int(Some(-1), Some(255)) },
];

impl NAOptionHandler for GIFEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        "compr" => {
                            if let NAValue::String(ref strval) = option.value {
                                match strval.as_str() {
                                    "none" => self.lzw.level = CompressionLevel::None,
                                    "fast" => self.lzw.level = CompressionLevel::Fast,
                                    "best" => self.lzw.level = CompressionLevel::Best,
                                    _ => {},
                                };
                            }
                        },
                        "inter_transparent" => {
                            if let NAValue::Bool(bval) = option.value {
                                self.p_trans = bval;
                            }
                        },
                        "transparent_idx" => {
                            if let NAValue::Int(ival) = option.value {
                                self.tr_idx = if ival >= 0 { Some(ival as u8) } else { None };
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
            "compr" => Some(NAValue::String(self.lzw.level.to_string())),
            "inter_transparent" => Some(NAValue::Bool(self.p_trans)),
            "transparent_idx" => Some(NAValue::Int(self.tr_idx.map_or(-1i64, i64::from))),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(GIFEncoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_codec_support::test::enc_video::*;

    // sample: https://samples.mplayerhq.hu/V-codecs/Uncompressed/8bpp.avi
    fn test_gif_encoder_single(out_name: &'static str, enc_options: &[NAOption], hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        generic_register_all_encoders(&mut enc_reg);

        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Misc/8bpp.avi",
                stream_type:    StreamType::Video,
                limit:          Some(0),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "gif",
                enc_name:       "gif",
                out_name,
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  PAL8_FORMAT,
                flipped: false,
                bits:    8,
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
    // sample: https://samples.mplayerhq.hu/image-samples/GIF/3D.gif
    fn test_gif_anim(out_name: &'static str, enc_options: &[NAOption], hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        generic_register_all_encoders(&mut enc_reg);

        let dec_config = DecoderTestParams {
                demuxer:        "gif",
                in_name:        "assets/Misc/3D.gif",
                stream_type:    StreamType::Video,
                limit:          None,
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "gif",
                enc_name:       "gif",
                out_name,
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  PAL8_FORMAT,
                flipped: false,
                bits:    8,
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
    fn test_gif_single_none() {
        let enc_options = &[
                NAOption { name: "compr", value: NAValue::String("none".to_string()) },
            ];
        test_gif_encoder_single("none.gif", enc_options, &[0x32900cff, 0xef979bb0, 0x2d0355e8, 0x424bddee]);
    }
    #[test]
    fn test_gif_single_fast() {
        let enc_options = &[
                NAOption { name: "compr", value: NAValue::String("fast".to_string()) },
            ];
        test_gif_encoder_single("fast.gif", enc_options, &[0x9644f682, 0x497593cd, 0xdabb483d, 0x8fce63f4]);
    }
    #[test]
    fn test_gif_single_best() {
        let enc_options = &[
                NAOption { name: "compr", value: NAValue::String("best".to_string()) },
            ];
        test_gif_encoder_single("best.gif", enc_options, &[0x9644f682, 0x497593cd, 0xdabb483d, 0x8fce63f4]);
    }
    #[test]
    fn test_gif_anim_opaque() {
        let enc_options = &[
                NAOption { name: "compr", value: NAValue::String("fast".to_string()) },
            ];
        test_gif_anim("anim-opaque.gif", enc_options, &[0x58489e31, 0x1721d75e, 0xaebf93f2, 0x3fea9c6e]);
    }
    #[test]
    fn test_gif_anim_transparent() {
        let enc_options = &[
                NAOption { name: "compr", value: NAValue::String("fast".to_string()) },
                NAOption { name: "inter_transparent", value: NAValue::Bool(true) },
                NAOption { name: "transparent_idx", value: NAValue::Int(0x7F) },
            ];
        test_gif_anim("anim-transp.gif", enc_options, &[0x62df6232, 0x0c334457, 0x73738404, 0xa8829dcc]);
    }
}
