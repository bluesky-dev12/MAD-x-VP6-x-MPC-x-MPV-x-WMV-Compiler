use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::imaadpcm::*;
use std::str::FromStr;

struct IMAADPCMDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    ch_state:   [IMAState; 2],
    block_len:  usize,
}

impl IMAADPCMDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            chmap:      NAChannelMap::new(),
            ch_state:   [IMAState::new(), IMAState::new()],
            block_len:  0,
        }
    }
}

impl NADecoder for IMAADPCMDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.block_len = ainfo.get_block_len();
            let channels = ainfo.get_channels() as usize;
            validate!(channels == 2 || channels == 1);
            validate!(self.block_len >= 8 * channels);
            validate!((self.block_len & 3) == 0);
            let len = (self.block_len / channels - 4) * 2 + 1;
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), channels as u8, SND_S16P_FORMAT, len);
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
            let channels = self.chmap.num_channels();
            validate!(pktbuf.len() >= 8 * channels);
            let nsamples = (pktbuf.len() / channels - 4) * 2 + 1;
            let abuf = alloc_audio_buffer(self.ainfo, nsamples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let mut off = [adata.get_offset(0), adata.get_offset(1)];
            let dst = adata.get_data_mut().unwrap();

            let mut mr = MemoryReader::new_read(pktbuf.as_slice());
            let mut br = ByteReader::new(&mut mr);
            for ch in 0..channels {
                let pred                        = br.read_u16le()? as i16;
                let step                        = br.read_byte()?;
                                                  br.read_skip(1)?;
                validate!(step <= IMA_MAX_STEP);
                self.ch_state[ch].reset(pred, step);
                dst[off[ch]] = pred;
                off[ch] += 1;
            }
            while br.left() > 0 {
                for ch in 0..channels {
                    let mut cw                  = br.read_u32le()?;
                    for _ in 0..8 {
                        dst[off[ch]] = self.ch_state[ch].expand_sample((cw & 0xF) as u8);
                        off[ch] += 1;
                        cw >>= 4;
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

impl NAOptionHandler for IMAADPCMDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(IMAADPCMDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::ms_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_ima_adpcm_ms() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        ms_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/ima-adpcm/ima-adpcm-stutter/IMAG0006.AVI
        test_decoding("avi", "ima-adpcm-ms", "assets/MS/IMAG0006.AVI", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x0cdc640f, 0xb00df235, 0x1ec4a280, 0x065b5e9e]));
    }
}
