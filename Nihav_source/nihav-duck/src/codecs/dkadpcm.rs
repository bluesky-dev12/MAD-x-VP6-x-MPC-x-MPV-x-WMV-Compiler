use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::imaadpcm::*;
use std::str::FromStr;

struct DuckADPCMDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    is_dk3:     bool,
    ch_state:   [IMAState; 2],
    block_len:  usize,
}

impl DuckADPCMDecoder {
    fn new(is_dk3: bool) -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            chmap:      NAChannelMap::new(),
            is_dk3,
            ch_state:   [IMAState::new(), IMAState::new()],
            block_len:  0,
        }
    }
}

impl NADecoder for DuckADPCMDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            validate!(ainfo.get_block_len() > 16);
            self.block_len = ainfo.get_block_len();
            let channels = ainfo.get_channels();
            validate!(channels == 2 || (!self.is_dk3 && channels == 1));
            let len = if self.is_dk3 {
                    ((self.block_len - 16) * 2 / 3) * 2
                } else {
                    (self.block_len - 4 * (channels as usize)) * 2 / (channels as usize)
                };
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), channels, SND_S16P_FORMAT, len);
            self.chmap = NAChannelMap::from_str(if channels == 1 { "C" } else { "L,R" }).unwrap();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            validate!(pktbuf.len() > (if self.is_dk3 { 16 } else { 4 * self.chmap.num_channels() }));
            let nblocks = pktbuf.len() / self.block_len;
            let out_block_len = self.ainfo.get_block_len();
            let duration = out_block_len * nblocks;
            let abuf = alloc_audio_buffer(self.ainfo, duration, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let mut off0 = adata.get_offset(0);
            let mut off1 = adata.get_offset(1);
            let dst = adata.get_data_mut().unwrap();

            for blk in pktbuf.chunks_exact(self.block_len) {
                let mut mr = MemoryReader::new_read(blk);
                let mut br = ByteReader::new(&mut mr);
                if self.is_dk3 {
                    let _typeid                 = br.read_byte()?;
                    let _version                = br.read_byte()?;
                    let _srate                  = br.read_u32le()?;
                    let samples                 = br.read_u32le()? as usize;
                    let sumpred                 = br.read_u16le()? as i16;
                    let diffpred                = br.read_u16le()? as i16;
                    let sumstep                 = br.read_byte()?;
                    let diffstep                = br.read_byte()?;
                    validate!(sumstep <= IMA_MAX_STEP && diffstep <= IMA_MAX_STEP);
                    validate!(samples <= out_block_len);
                    self.ch_state[0].reset(sumpred,  sumstep);
                    self.ch_state[1].reset(diffpred, diffstep);
                    let mut last_nib = 0;
                    let mut diff_val: i32 = i32::from(diffpred);
                    for x in (0..out_block_len).step_by(2) {
                        let nib0;
                        let nib1;
                        let nib2;
                        if (x & 2) == 0 {
                            let b0              = br.read_byte()?;
                            let b1              = br.read_byte()?;
                            nib0 = b0 & 0xF;
                            nib1 = b0 >> 4;
                            nib2 = b1 & 0xF;
                            last_nib = b1 >> 4;
                        } else {
                            let b0              = br.read_byte()?;
                            nib0 = last_nib;
                            nib1 = b0 & 0xF;
                            nib2 = b0 >> 4;
                        }
                        let sum0 = i32::from(self.ch_state[0].expand_sample(nib0));
                        let diff = i32::from(self.ch_state[1].expand_sample(nib1));
                        let sum1 = i32::from(self.ch_state[0].expand_sample(nib2));
                        diff_val = (diff_val + diff) >> 1;
                        dst[off0 + x + 0] = (sum0 + diff_val) as i16;
                        dst[off1 + x + 0] = (sum0 - diff_val) as i16;
                        diff_val = (diff_val + diff) >> 1;
                        dst[off0 + x + 1] = (sum1 + diff_val) as i16;
                        dst[off1 + x + 1] = (sum1 - diff_val) as i16;
                        diff_val = diff;
                    }
                } else {
                    let nchannels = self.chmap.num_channels();
                    for ch in 0..nchannels {
                        let pred                = br.read_u16le()? as i16;
                        let step                = br.read_byte()?;
                                                  br.read_skip(1)?;
                        validate!(step <= IMA_MAX_STEP);
                        self.ch_state[ch].reset(pred, step);
                    }
                    if nchannels == 2 {
                        for x in 0..out_block_len {
                            let b               = br.read_byte()?;
                            dst[off0 + x] = self.ch_state[0].expand_sample(b >> 4);
                            dst[off1 + x] = self.ch_state[1].expand_sample(b & 0xF);
                        }
                    } else {
                        for x in (0..out_block_len).step_by(2) {
                            let b               = br.read_byte()?;
                            dst[off0 + x + 0] = self.ch_state[0].expand_sample(b >> 4);
                            dst[off0 + x + 1] = self.ch_state[0].expand_sample(b & 0xF);
                        }
                    }
                }
                off0 += out_block_len;
                off1 += out_block_len;
            }
            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
            frm.set_duration(Some(duration as u64));
            frm.set_keyframe(false);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for DuckADPCMDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_dk3() -> Box<dyn NADecoder + Send> {
    Box::new(DuckADPCMDecoder::new(true))
}

pub fn get_decoder_dk4() -> Box<dyn NADecoder + Send> {
    Box::new(DuckADPCMDecoder::new(false))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_dk3() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        //let file = "assets/Duck/AVI-DUCK-dk3.duk";
        //test_decode_audio("avi", file, Some(100), None/*Some("dk3")*/, &dmx_reg, &dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/DUCK/AVI-DUCK-dk3.duk
        test_decoding("avi", "adpcm-dk3", "assets/Duck/AVI-DUCK-dk3.duk", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xa48fae0a, 0xa536b27f, 0x169ecc19, 0x8436fade]));
    }
    #[test]
    fn test_dk4() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

//        let file = "assets/Duck/virtuafighter2-opening1.avi";
//        test_decode_audio("avi", file, Some(100), None/*Some("dk4")*/, &dmx_reg, &dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/DUCK/virtuafighter2-opening1.avi
        test_decoding("avi", "adpcm-dk4", "assets/Duck/virtuafighter2-opening1.avi", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x04e40d15, 0xf65b3427, 0x1dd5181f, 0xf321b56f]));
    }
}
