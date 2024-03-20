use nihav_core::codecs::*;
use nihav_core::io::bitwriter::*;
use nihav_codec_support::codecs::imaadpcm::*;

#[derive(Default)]
struct ADPCMEncoder {
    stream:     Option<NAStreamRef>,
    samples:    Vec<i16>,
    flush:      bool,
    state:      [IMAState; 2],
    channels:   usize,
    srate:      u32,
    pos:        u64,
}

const BLOCK_LEN: usize = 4096;

impl ADPCMEncoder {
    fn new() -> Self { Self::default() }
    fn encode_packet(&mut self) -> EncoderResult<NAPacket> {
        if self.samples.is_empty() {
            return Err(EncoderError::TryAgain);
        }
        let cur_len = (self.samples.len() / self.channels).min(BLOCK_LEN);
        if cur_len < BLOCK_LEN && !self.flush {
            return Err(EncoderError::TryAgain);
        }

        let bsize = (2 + (16 + 6 + (cur_len - 1) * 4) * self.channels + 7) / 8;

        let mut bw = BitWriter::new(Vec::with_capacity(bsize), BitWriterMode::BE);

        bw.write(2, 2);

        for ch in 0..self.channels {
            self.state[ch].predictor = i32::from(self.samples[ch]);
            self.state[ch].step = 30;

            bw.write_s(i32::from(self.samples[ch]), 16);
            bw.write(self.state[ch].step as u32, 6);
        }

        let mut siter = self.samples[self.channels..].iter();
        for _ in 1..cur_len {
            for state in self.state[..self.channels].iter_mut() {
                let samp = *siter.next().unwrap();
                let nib = state.compress_sample(samp);
                state.expand_sample(nib);
                bw.write(u32::from(nib), 4);
            }
        }

        let data = bw.end();

        self.samples.drain(..cur_len * self.channels);
        let ts = NATimeInfo::new(Some(self.pos), None, Some(cur_len as u64), 1, self.srate);
        self.pos += cur_len as u64;
        Ok(NAPacket::new(self.stream.clone().unwrap(), ts, true, data))
    }
}

impl NAEncoder for ADPCMEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Audio(NAAudioInfo::new(0, 1, SND_S16_FORMAT, BLOCK_LEN)),
                    ..Default::default() })
            },
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(ainfo) => {
                let mut outinfo = ainfo;
                outinfo.channels = outinfo.channels.max(1).min(2);
                if outinfo.format != SND_S16P_FORMAT && outinfo.format != SND_S16_FORMAT {
                    outinfo.format = SND_S16_FORMAT;
                }
                outinfo.block_len = BLOCK_LEN;
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
                self.channels = ainfo.channels as usize;

                let soniton = NASoniton::new(4, 0);
                let out_ainfo = NAAudioInfo::new(ainfo.sample_rate, ainfo.channels, soniton, if self.channels == 1 { 2051 } else { 4101 });
                let info = NACodecInfo::new("flv-adpcm", NACodecTypeInfo::Audio(out_ainfo), None);
                let mut stream = NAStream::new(StreamType::Audio, stream_id, info, 1, ainfo.sample_rate, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());
                self.samples = Vec::with_capacity(BLOCK_LEN * self.channels);
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
                self.samples.extend_from_slice(&src[..len * ch]);
            } else {
                let astride = abuf.get_stride();
                self.samples.reserve(len * ch);
                for i in 0..len {
                    for ch in 0..self.channels {
                        self.samples.push(src[ch * astride + i]);
                    }
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

impl NAOptionHandler for ADPCMEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) {}
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(ADPCMEncoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_flv_adpcm_encoder() {

        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        flash_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        flash_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        flash_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/FLV/flash_flv_adpcm_testfiles/mono_11k.flv
        let dec_config = DecoderTestParams {
                demuxer:        "flv",
                in_name:        "assets/Flash/mono_11k.flv",
                stream_type:    StreamType::Audio,
                limit:          Some(3700),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "flv",
                enc_name:       "flv-adpcm",
                out_name:       "flv_adpcm.flv",
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
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, &[]);

        test_encoding_md5(&dec_config, &enc_config, enc_params, &[],
                          &[0x6114b7de, 0xb84c226f, 0x0caab8e5, 0x2c7df63f]);
    }
}
