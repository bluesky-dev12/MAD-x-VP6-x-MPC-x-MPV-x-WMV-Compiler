use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use std::str::FromStr;

const ADAPT_TABLE: [i32; 16] = [
    230, 230, 230, 230, 307, 409, 512, 614,
    768, 614, 512, 409, 307, 230, 230, 230
];
const ADAPT_COEFFS: [[i32; 2]; 7] = [
    [ 256, 0 ], [ 512, -256 ], [ 0, 0 ], [ 192, 64 ],
    [ 240, 0 ], [ 460, -208 ], [ 392, -232 ]
];

#[derive(Default)]
struct Predictor {
    sample1:    i32,
    sample2:    i32,
    delta:      i32,
    coef1:      i32,
    coef2:      i32,
}

impl Predictor {
    fn expand_nibble(&mut self, nibble: u8) -> i16 {
        let mul = if (nibble & 8) == 0 { i32::from(nibble) } else { i32::from(nibble) - 16 };
        let pred = self.calc_pred() + self.delta.wrapping_mul(mul);
        self.update(pred.max(-0x8000).min(0x7FFF));
        self.delta = (ADAPT_TABLE[nibble as usize].wrapping_mul(self.delta) >> 8).max(16);
        self.sample1 as i16
    }
    fn calc_pred(&self) -> i32 {
        self.sample1.wrapping_mul(self.coef1).wrapping_add(self.sample2.wrapping_mul(self.coef2)) >> 8
    }
    fn update(&mut self, new_samp: i32) {
        self.sample2 = self.sample1;
        self.sample1 = new_samp;
    }
}

#[cfg(feature="decoder_ms_adpcm")]
struct MSADPCMDecoder {
    ainfo:          NAAudioInfo,
    chmap:          NAChannelMap,
    adapt_coeffs:   Vec<[i32; 2]>,
    block_len:      usize,
    block_samps:    usize,
}

#[cfg(feature="decoder_ms_adpcm")]
impl MSADPCMDecoder {
    fn new() -> Self {
        Self {
            ainfo:          NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            chmap:          NAChannelMap::new(),
            adapt_coeffs:   Vec::with_capacity(7),
            block_len:      0,
            block_samps:    0,
        }
    }
}

#[cfg(feature="decoder_ms_adpcm")]
impl NADecoder for MSADPCMDecoder {
    #[allow(clippy::int_plus_one)]
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.block_len = ainfo.get_block_len();
            let channels = ainfo.get_channels() as usize;
            validate!(channels == 2 || channels == 1);
            validate!(self.block_len >= 7 * channels + 1);
            self.block_samps = (self.block_len / channels - 7) * 2 + 2;
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), channels as u8, SND_S16P_FORMAT, self.block_samps);
            self.chmap = NAChannelMap::from_str(if channels == 1 { "C" } else { "L,R" }).unwrap();
            self.adapt_coeffs.clear();
            if let Some(ref buf) = info.get_extradata() {
                validate!(buf.len() >= 6);
                validate!((buf.len() & 3) == 0);
                let mut mr = MemoryReader::new_read(buf.as_slice());
                let mut br = ByteReader::new(&mut mr);
                let _smth               = br.read_u16le()?;
                let ncoeffs             = br.read_u16le()? as usize;
                validate!(buf.len() == ncoeffs * 4 + 4);

                for _ in 0..ncoeffs {
                    let pair = [
                        i32::from(br.read_u16le()? as i16),
                        i32::from(br.read_u16le()? as i16)];
                    self.adapt_coeffs.push(pair);
                }
            } else {
                self.adapt_coeffs.extend_from_slice(&ADAPT_COEFFS);
            }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            let channels = self.chmap.num_channels();
            validate!(!pktbuf.is_empty() && (pktbuf.len() % self.block_len) == 0);
            let nblocks = pktbuf.len() / self.block_len;
            let nsamples = nblocks * self.block_samps;
            let abuf = alloc_audio_buffer(self.ainfo, nsamples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let mut off = [adata.get_offset(0), adata.get_offset(1)];
            let dst = adata.get_data_mut().unwrap();

            let mut pred = [Predictor::default(), Predictor::default()];

            for blk in pktbuf.chunks(self.block_len) {
                let mut mr = MemoryReader::new_read(blk);
                let mut br = ByteReader::new(&mut mr);
                for ch in 0..channels {
                    let coef_idx                = br.read_byte()? as usize;
                    validate!(coef_idx < self.adapt_coeffs.len());
                    pred[ch].coef1 = self.adapt_coeffs[coef_idx][0];
                    pred[ch].coef2 = self.adapt_coeffs[coef_idx][1];
                }
                for ch in 0..channels {
                    pred[ch].delta              = i32::from(br.read_u16le()?);
                }
                for ch in 0..channels {
                    let samp                    = br.read_u16le()? as i16;
                    pred[ch].sample1            = i32::from(samp);
                }
                for ch in 0..channels {
                    let samp                    = br.read_u16le()? as i16;
                    pred[ch].sample2            = i32::from(samp);
                }
                for ch in 0..channels {
                    dst[off[ch]]     = pred[ch].sample2 as i16;
                    dst[off[ch] + 1] = pred[ch].sample1 as i16;
                    off[ch] += 2;
                }
                if channels == 1 {
                    while br.left() > 0 {
                        let idx                 = br.read_byte()?;
                        dst[off[0]] = pred[0].expand_nibble(idx >> 4);
                        off[0] += 1;
                        dst[off[0]] = pred[0].expand_nibble(idx & 0xF);
                        off[0] += 1;
                    }
                } else {
                    while br.left() > 0 {
                        let idx                 = br.read_byte()?;
                        dst[off[0]] = pred[0].expand_nibble(idx >> 4);
                        off[0] += 1;
                        dst[off[1]] = pred[1].expand_nibble(idx & 0xF);
                        off[1] += 1;
                    }
                }
            }
            let mut frm = NAFrame::new_from_pkt(pkt, info.replace_info(NACodecTypeInfo::Audio(self.ainfo)), abuf);
            frm.set_duration(Some(nsamples as u64));
            frm.set_keyframe(false);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

#[cfg(feature="decoder_ms_adpcm")]
impl NAOptionHandler for MSADPCMDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

#[cfg(feature="decoder_ms_adpcm")]
pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(MSADPCMDecoder::new())
}

#[derive(Default)]
#[cfg(feature="encoder_ms_adpcm")]
struct MSADPCMEncoder {
    stream:     Option<NAStreamRef>,
    samples:    Vec<i16>,
    block_len:  usize,
    channels:   usize,
    flush:      bool,
    srate:      u32,
}

#[cfg(feature="encoder_ms_adpcm")]
const DEFAULT_BLOCK_LEN: usize = 256;

#[cfg(feature="encoder_ms_adpcm")]
impl MSADPCMEncoder {
    fn new() -> Self { Self::default() }
    fn encode_packet(&mut self) -> EncoderResult<NAPacket> {
        if self.samples.is_empty() {
            return Err(EncoderError::TryAgain);
        }
        let len = (self.samples.len() / self.channels).min(self.block_len);
        if len < self.block_len && !self.flush {
            return Err(EncoderError::TryAgain);
        }
        if len < 2 {
            self.flush = false;
            return Err(EncoderError::TryAgain);
        }

        let mut dbuf = vec![0u8; Self::calc_block_size(len, self.channels)];
        let mut mw = MemoryWriter::new_write(dbuf.as_mut_slice());
        let mut bw = ByteWriter::new(&mut mw);

        let mut best_idx = [0usize; 2];
        for ch in 0..self.channels {
            let mut best_dist = std::i64::MAX;
            for i in 0..ADAPT_COEFFS.len() {
                let dist = self.calc_dist(ch, i, len);
                if dist < best_dist {
                    best_dist = dist;
                    best_idx[ch] = i;
                }
            }
            bw.write_byte(best_idx[ch] as u8)?;
        }
        let mut dec = [Predictor::default(), Predictor::default()];
        for ch in 0..self.channels {
            dec[ch].sample1 = i32::from(self.samples[ch + self.channels]);
            dec[ch].sample2 = i32::from(self.samples[ch]);
            dec[ch].coef1   = ADAPT_COEFFS[best_idx[ch]][0];
            dec[ch].coef2   = ADAPT_COEFFS[best_idx[ch]][1];
            if len > 2 {
                let pred = dec[ch].calc_pred();
                dec[ch].delta = ((i32::from(self.samples[ch + self.channels * 2]) - pred).abs() / 4).max(16);
            } else {
                dec[ch].delta = 16;
            }
        }
        for ch in 0..self.channels {
            bw.write_u16le(dec[ch].delta as u16)?;
        }
        for ch in 0..self.channels {
            bw.write_u16le(dec[ch].sample1 as u16)?;
        }
        for ch in 0..self.channels {
            bw.write_u16le(dec[ch].sample2 as u16)?;
        }
        if self.channels == 1 {
            for samps in self.samples.chunks(2).skip(1).take(len/2 - 1) {
                let diff = i32::from(samps[0]) - dec[0].calc_pred();
                let nib0 = Self::calculate_mul(dec[0].delta, diff);
                dec[0].expand_nibble(nib0);
                let diff = i32::from(samps[1]) - dec[0].calc_pred();
                let nib1 = Self::calculate_mul(dec[0].delta, diff);
                dec[0].expand_nibble(nib1);
                bw.write_byte(nib0 * 16 + nib1)?;
            }
        } else {
            for samps in self.samples.chunks(2).skip(2).take(len - 2) {
                let diff = i32::from(samps[0]) - dec[0].calc_pred();
                let nib0 = Self::calculate_mul(dec[0].delta, diff);
                dec[0].expand_nibble(nib0);
                let diff = i32::from(samps[1]) - dec[1].calc_pred();
                let nib1 = Self::calculate_mul(dec[1].delta, diff);
                dec[1].expand_nibble(nib1);
                bw.write_byte(nib0 * 16 + nib1)?;
            }
        }
        self.samples.drain(..len * self.channels);
        let ts = NATimeInfo::new(None, None, Some(1), 1, self.srate);
        Ok(NAPacket::new(self.stream.clone().unwrap(), ts, true, dbuf))
    }
    fn calc_dist(&self, ch: usize, idx: usize, len: usize) -> i64 {
        let mut dist = 0;
        let mut dec = Predictor {
                sample2: i32::from(self.samples[ch]),
                sample1: i32::from(self.samples[ch + self.channels]),
                coef1:   ADAPT_COEFFS[idx][0],
                coef2:   ADAPT_COEFFS[idx][1],
                delta:   16,
            };
        if self.channels == 1 {
            for samp in self.samples.iter().skip(2).take(len - 2) {
                let pred = dec.calc_pred();
                dec.update(pred);
                let diff = i64::from(*samp) - i64::from(pred);
                dist += diff * diff;
            }
        } else {
            for samp in self.samples.chunks(2).skip(2).take(len - 2) {
                let pred = dec.calc_pred();
                dec.update(pred);
                let diff = i64::from(samp[ch]) - i64::from(pred);
                dist += diff * diff;
            }
        }
        dist
    }
    fn calculate_mul(delta: i32, diff: i32) -> u8 {
        ((diff / delta).max(-8).min(7) & 0xF) as u8
    }
    fn calc_block_size(nsamps: usize, channels: usize) -> usize {
        (nsamps - 2) * channels / 2 + 7 * channels
    }
}

#[cfg(feature="encoder_ms_adpcm")]
impl NAEncoder for MSADPCMEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Audio(NAAudioInfo::new(0, 1, SND_S16_FORMAT, DEFAULT_BLOCK_LEN)),
                    ..Default::default() })
            },
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(ainfo) => {
                let mut outinfo = ainfo;
                outinfo.channels = outinfo.channels.min(2);
                if outinfo.format != SND_S16P_FORMAT && outinfo.format != SND_S16_FORMAT {
                    outinfo.format = SND_S16_FORMAT;
                }
                if outinfo.block_len == 0 {
                    outinfo.block_len = DEFAULT_BLOCK_LEN;
                }
                if outinfo.block_len < 2 {
                    outinfo.block_len = 2;
                }
                if (outinfo.channels == 1) && ((outinfo.block_len & 1) == 1) {
                    outinfo.block_len += 1;
                }
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Audio(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_CBR }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(ainfo) => {
                if ainfo.format != SND_S16P_FORMAT && ainfo.format != SND_S16_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if ainfo.channels != 1 && ainfo.channels != 2 {
                    return Err(EncoderError::FormatError);
                }
                if ainfo.block_len < 2 || ((ainfo.block_len * (ainfo.channels as usize)) & 1) != 0 {
                    return Err(EncoderError::FormatError);
                }
                self.channels = ainfo.channels as usize;
                self.block_len = ainfo.block_len;

                let soniton = NASoniton::new(4, 0);
                let out_ainfo = NAAudioInfo::new(ainfo.sample_rate, ainfo.channels, soniton, Self::calc_block_size(self.block_len, self.channels));
                let info = NACodecInfo::new("ms-adpcm", NACodecTypeInfo::Audio(out_ainfo), None);
                let mut stream = NAStream::new(StreamType::Audio, stream_id, info, self.block_len as u32, ainfo.sample_rate, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());
                self.samples = Vec::with_capacity(self.block_len * self.channels);
                self.srate = ainfo.sample_rate;
                self.flush = false;

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        if let Some(ref abuf) = buf.get_abuf_i16() {
            let src = abuf.get_data();
            let len = abuf.get_length();
            let ch  = abuf.get_chmap().num_channels();
            if abuf.get_step() > 1 || ch == 1 {
                self.samples.extend(src.iter().take(len * ch));
            } else {
                let (src0, src1) = src.split_at(abuf.get_stride());
                self.samples.reserve(len * 2);
                for (s0, s1) in src0.iter().take(len).zip(src1.iter()) {
                    self.samples.push(*s0);
                    self.samples.push(*s1);
                }
            }
            Ok(())
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        if let Ok(pkt) = self.encode_packet() {
            Ok(Some(pkt))
        } else {
            Ok(None)
        }
    }
    fn flush(&mut self) -> EncoderResult<()> {
        self.flush = true;
        Ok(())
    }
}

#[cfg(feature="encoder_ms_adpcm")]
impl NAOptionHandler for MSADPCMEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

#[cfg(feature="encoder_ms_adpcm")]
pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(MSADPCMEncoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::dec_video::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;
    use nihav_commonfmt::*;
    #[cfg(feature="decoder_ms_adpcm")]
    #[test]
    fn test_ms_adpcm_decoder() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        ms_register_all_decoders(&mut dec_reg);

        test_decoding("avi", "ms-adpcm", "assets/MS/dance.avi", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xb1d6f12c, 0x86d2821b, 0x395f6827, 0xb6be93bf]));
    }
    #[cfg(feature="encoder_ms_adpcm")]
    #[test]
    fn test_ms_adpcm_encoder() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        ms_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        ms_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/RT21/320x240/laser05.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Indeo/laser05.avi",
                stream_type:    StreamType::Audio,
                limit:          None,
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "wav",
                enc_name:       "ms-adpcm",
                out_name:       "msadpcm.wav",
                mux_reg, enc_reg,
            };
        let dst_ainfo = NAAudioInfo {
                sample_rate:    0,
                channels:       0,
                format:         SND_S16_FORMAT,
                block_len:      128,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Audio(dst_ainfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        test_encoding_md5(&dec_config, &enc_config, enc_params, &[],
                          &[0x82259f45, 0xba7b984a, 0xc03c94e5, 0x00b4312b]);
    }
}
