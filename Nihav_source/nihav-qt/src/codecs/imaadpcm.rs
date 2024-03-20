use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::imaadpcm::*;
use std::str::FromStr;

const PACKET_LEN: usize = 34;
const PACKET_SAMPLES: usize = 64;

struct IMAADPCMDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    ch_state:   [IMAState; 2],
}

impl IMAADPCMDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, PACKET_SAMPLES),
            chmap:      NAChannelMap::new(),
            ch_state:   [IMAState::new(), IMAState::new()],
        }
    }
}

impl NADecoder for IMAADPCMDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            let channels = ainfo.get_channels() as usize;
            validate!(channels == 2 || channels == 1);
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), channels as u8, SND_S16P_FORMAT, PACKET_SAMPLES);
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
            validate!(pktbuf.len() % (PACKET_LEN * channels) == 0);
            let nblocks = pktbuf.len() / channels / PACKET_LEN;
            let nsamples = nblocks * PACKET_SAMPLES;
            let abuf = alloc_audio_buffer(self.ainfo, nsamples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let mut off = [adata.get_offset(0), adata.get_offset(1)];
            let dst = adata.get_data_mut().unwrap();

            for data in pktbuf.chunks(PACKET_LEN * channels) {
                for ch in 0..channels {
                    let mut mr = MemoryReader::new_read(&data[PACKET_LEN * ch..][..PACKET_LEN]);
                    let mut br = ByteReader::new(&mut mr);

                    let init                    = br.read_u16be()?;
                    let pred = (init as i16) & !0x7F;
                    let step = (init & 0x7F) as u8;
                    validate!(step <= IMA_MAX_STEP);
                    self.ch_state[ch].reset(pred, step);

                    for i in (0..PACKET_SAMPLES).step_by(2) {
                        let byte                = br.read_byte()?;
                        dst[off[ch] + i]     = self.ch_state[ch].expand_sample(byte & 0xF);
                        dst[off[ch] + i + 1] = self.ch_state[ch].expand_sample(byte >> 4);
                    }
                    off[ch] += PACKET_SAMPLES;
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
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_ima_adpcm_qt() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/ima-adpcm/shuffle-ima41.mov
        test_decoding("mov", "ima-adpcm-qt", "assets/QT/shuffle-ima41.mov", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xba2ad472, 0xd6aee026, 0xb915dd7d, 0xac51314c]));
    }
}
