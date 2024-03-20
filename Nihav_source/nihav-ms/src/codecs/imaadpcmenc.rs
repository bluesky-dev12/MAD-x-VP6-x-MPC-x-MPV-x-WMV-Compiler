use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::imaadpcm::*;

struct NibbleWriter<'a> {
    bw:     ByteWriter<'a>,
    val:    u8,
    first:  bool,
}

impl<'a> NibbleWriter<'a> {
    fn new(bw: ByteWriter<'a>) -> Self {
        Self { bw, val: 0, first: true }
    }
    fn write(&mut self, nib: u8) -> EncoderResult<()> {
        if self.first {
            self.val = nib;
            self.first = false;
        } else {
            self.val |= nib << 4;
            self.bw.write_byte(self.val)?;
            self.first = true;
        }
        Ok(())
    }
}

#[derive(Clone,Copy)]
struct TrellisNode {
    state:  IMAState,
    nib:    u8,
    error:  i64,
}

impl Default for TrellisNode {
    fn default() -> Self {
        TrellisNode {
            state:  IMAState::new(),
            nib:    0,
            error:  0,
        }
    }
}

#[derive(Default)]
struct IMAADPCMEncoder {
    stream:     Option<NAStreamRef>,
    samples:    Vec<i16>,
    block_len:  usize,
    block_size: usize,
    channels:   usize,
    flush:      bool,
    srate:      u32,
    trellis:    bool,
    nodes:      Vec<[TrellisNode; 16]>,
    first:      Vec<IMAState>,
    nibs:       Vec<Vec<u8>>,
}

const DEFAULT_BLOCK_LEN: usize = 256;

impl IMAADPCMEncoder {
    fn new() -> Self { Self::default() }
    fn encode_packet(&mut self) -> EncoderResult<NAPacket> {
        if self.samples.is_empty() {
            return Err(EncoderError::TryAgain);
        }
        let cur_len = (self.samples.len() / self.channels).min(self.block_len);
        if cur_len < self.block_len && !self.flush {
            return Err(EncoderError::TryAgain);
        }
        if cur_len < self.block_len {
            self.samples.resize(self.block_len * self.channels, 0);
        }

        let mut dbuf = vec![0u8; self.block_size];
        let mut mw = MemoryWriter::new_write(dbuf.as_mut_slice());
        let mut bw = ByteWriter::new(&mut mw);

        for ch in 0..self.channels {
            self.first[ch].predictor = i32::from(self.samples[ch]);
            self.first[ch].step = Self::calc_step(self.samples[ch], self.samples[ch + self.channels]) as usize;
        }

        if !self.trellis {
            for ch in 0..self.channels {
                bw.write_u16le(self.first[ch].predictor as u16)?;
                bw.write_byte(self.first[ch].step as u8)?;
                bw.write_byte(0)?;
            }
            let mut nw = NibbleWriter::new(bw);
            for samples in self.samples.chunks(self.channels).take(self.block_len).skip(1) {
                for (state, &samp) in self.first.iter_mut().zip(samples.iter()) {
                    let nib = state.compress_sample(samp);
                    state.expand_sample(nib);
                    nw.write(nib)?;
                }
            }
        } else {
            self.nodes.reserve(self.block_len);
            self.nibs.resize(self.channels, Vec::new());
            for nibs in self.nibs.iter_mut() {
                nibs.resize(self.block_len, 0);
            }
            let step = self.channels;
            let mut state = [TrellisNode::default(); 16];
            for ch in 0..self.channels {
                self.nodes.clear();
                for i in 0..16 {
                    let step = (((self.first[ch].step + i) as isize) - 8).max(0).min(IMA_MAX_STEP as isize) as usize;
                    state[i].state.predictor = self.first[ch].predictor;
                    state[i].state.step = step;
                    state[i].error = 0;
                    state[i].nib = step as u8;
                }
                self.nodes.push(state);
                let mut sidx = ch + step;
                for _i in 1..self.block_len {
                    let sample = self.samples[sidx];

                    for (nnib, cstate) in state.iter_mut().enumerate() {
                        for nib in 0..16 {
                            let pnode = &self.nodes[self.nodes.len() - 1][nib];
                            let mut ima = pnode.state;
                            let nsamp = ima.expand_sample(nnib as u8);
                            let diff = i64::from(i32::from(sample) - i32::from(nsamp));
                            let error = pnode.error + diff * diff;
                            if (nib == 0) || error < cstate.error {
                                cstate.state = ima;
                                cstate.nib = nib as u8;
                                cstate.error = error;
                            }
                        }
                    }
                    self.nodes.push(state);

                    sidx += step;
                }

                let mut idx = 0;
                let mut best_err = self.nodes[self.nodes.len() - 1][0].error;
                for (i, node) in self.nodes[self.nodes.len() - 1].iter().enumerate() {
                    if node.error < best_err {
                        best_err = node.error;
                        idx = i;
                    }
                }
                let mut dst = self.nibs[ch].iter_mut().rev();
                while let Some(nodes) = self.nodes.pop() {
                    *dst.next().unwrap() = idx as u8;
                    idx = nodes[idx].nib as usize;
                }
                self.nibs[ch][0] = idx as u8;
            }
            for ch in 0..self.channels {
                bw.write_u16le(self.first[ch].predictor as u16)?;
                bw.write_byte(self.nibs[ch][0])?;
                bw.write_byte(0)?;
            }
            let mut nw = NibbleWriter::new(bw);
            for i in 1..self.block_len {
                for ch in 0..self.channels {
                    nw.write(self.nibs[ch][i])?;
                }
            }
        }

        self.samples.drain(..self.block_len * self.channels);
        let ts = NATimeInfo::new(None, None, Some(1), 1, self.srate);
        Ok(NAPacket::new(self.stream.clone().unwrap(), ts, true, dbuf))
    }
    fn calc_block_size(nsamps: usize, channels: usize) -> usize {
        ((nsamps - 1) * channels + 1) / 2 + 4 * channels
    }
    fn calc_step(samp1: i16, samp2: i16) -> u8 {
        let diff = (i32::from(samp1) - i32::from(samp2)).abs();
        for (i, &step) in IMA_STEP_TABLE.iter().enumerate() {
            if step >= diff {
                return i as u8;
            }
        }
        IMA_MAX_STEP
    }
}

impl NAEncoder for IMAADPCMEncoder {
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
                outinfo.channels = outinfo.channels.max(1);
                if outinfo.format != SND_S16P_FORMAT && outinfo.format != SND_S16_FORMAT {
                    outinfo.format = SND_S16_FORMAT;
                }
                if outinfo.block_len == 0 {
                    outinfo.block_len = DEFAULT_BLOCK_LEN;
                }
                if outinfo.block_len < 2 {
                    outinfo.block_len = 2;
                }
                if (outinfo.block_len & 7) != 0 {
                    outinfo.block_len = (outinfo.block_len & 7) + 7;
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
                if ainfo.block_len < 2 || ((ainfo.block_len * (ainfo.channels as usize)) & 7) != 0 {
                    return Err(EncoderError::FormatError);
                }
                self.channels = ainfo.channels as usize;
                self.block_len = ainfo.block_len;
                self.block_size = Self::calc_block_size(self.block_len, ainfo.channels as usize);
                self.first = Vec::with_capacity(self.channels);
                for _ in 0..self.channels {
                    self.first.push(IMAState::new());
                }

                let soniton = NASoniton::new(4, 0);
                let out_ainfo = NAAudioInfo::new(ainfo.sample_rate, ainfo.channels, soniton, Self::calc_block_size(self.block_len, self.channels));
                let info = NACodecInfo::new("ima-adpcm-ms", NACodecTypeInfo::Audio(out_ainfo), None);
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

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: "trellis", description: "Use trellis search",
        opt_type: NAOptionDefinitionType::Bool },
];

impl NAOptionHandler for IMAADPCMEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        "trellis" => {
                            if let NAValue::Bool(val) = option.value {
                                self.trellis = val;
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
            "trellis" => Some(NAValue::Bool(self.trellis)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(IMAADPCMEncoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;
    use nihav_commonfmt::*;

    fn test_ima_adpcm_ms_encoder(name: &'static str, trellis: bool, hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        ms_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        ms_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/VP4/ot171_vp40.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Duck/ot171_vp40.avi",
                stream_type:    StreamType::Audio,
                limit:          None,
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "wav",
                enc_name:       "ima-adpcm-ms",
                out_name:       name,
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
        let enc_options = &[
                NAOption{name: "trellis", value: NAValue::Bool(trellis)},
            ];
//        test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);

        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          hash);
    }
    #[test]
    fn test_ima_adpcm_ms_encoder_notrellis() {
        test_ima_adpcm_ms_encoder("msimaadpcm-notr.wav", false,
            &[0x59909f10, 0xf0420dd2, 0xcfee7ef5, 0x7623caa3]);
    }
    #[test]
    fn test_ima_adpcm_ms_encoder_trellis() {
        test_ima_adpcm_ms_encoder("msimaadpcm-tr.wav", true,
            &[0x99e373e2, 0x80439b4b, 0xcb4f0b78, 0xeb1e9a51]);
    }
}
