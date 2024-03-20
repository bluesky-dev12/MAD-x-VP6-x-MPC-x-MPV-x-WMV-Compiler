use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::codecs::imaadpcm::*;
use std::str::FromStr;

const STEPS: [&[i8]; 4] = [
    &[ -1, 2],
    &[ -1, -1, 2, 4],
    &[ -1, -1, -1, -1, 2, 4, 6, 8],
    &[ -1, -1, -1, -1, -1, -1, -1, -1, 1, 2, 4, 6, 8, 10, 13, 16 ]
];

#[derive(Clone,Copy)]
struct State {
    predictor:  i32,
    step:       usize,
    smask:      u8,
    steps:      &'static [i8],
}

impl State {
    fn new() -> Self {
        Self {
            predictor:  0,
            step:       0,
            smask:      0,
            steps:      STEPS[2],
        }
    }
    fn expand_sample(&mut self, nibble: u8) -> i16 {
        let istep = (self.step as isize) + isize::from(self.steps[(nibble & !self.smask) as usize]);
        let sign = (nibble & self.smask) != 0;
        let diff = (i32::from(2 * (nibble & !self.smask) + 1) * IMA_STEP_TABLE[self.step]) >> 3;
        let sample = if !sign { self.predictor + diff } else { self.predictor - diff };
        self.predictor = sample.max(i32::from(std::i16::MIN)).min(i32::from(std::i16::MAX));
        self.step = istep.max(0).min(IMA_MAX_STEP as isize) as usize;
        self.predictor as i16
    }
}

const BLOCK_LEN: usize = 4096;

struct ADPCMDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    ch_state:   [State; 2],
}

impl ADPCMDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, BLOCK_LEN),
            chmap:      NAChannelMap::new(),
            ch_state:   [State::new(), State::new()],
        }
    }
}

impl NADecoder for ADPCMDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            let channels = ainfo.get_channels() as usize;
            validate!(channels == 2 || channels == 1);
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), channels as u8, SND_S16P_FORMAT, 0);
            self.chmap = NAChannelMap::from_str(if channels == 1 { "C" } else { "L,R" }).unwrap();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let src = pkt.get_buffer();
            let channels = self.chmap.num_channels();
            let mut br = BitReader::new(&src, BitReaderMode::BE);
            let step_size                       = br.read(2)? as usize + 2;

            let pkt_size = (16 + 6 + (BLOCK_LEN - 1) * step_size) * channels;
            let num_pkts = ((br.left() as usize) / pkt_size).max(1);
            let nsamples = num_pkts * BLOCK_LEN;
            let mut abuf = alloc_audio_buffer(self.ainfo, nsamples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let mut off = [adata.get_offset(0), adata.get_offset(1)];
            let dst = adata.get_data_mut().unwrap();

            let mut tot_samples = 0;
            for _pkt in 0..num_pkts {
                for (ch, state) in self.ch_state[..channels].iter_mut().enumerate() {
                    state.predictor             = br.read_s(16)?;
                    state.step                  = br.read(6)? as usize;
                    state.steps = STEPS[step_size - 2];
                    state.smask = 1 << (step_size - 1);
                    dst[off[ch]] = state.predictor as i16;
                    off[ch] += 1;
                }
                tot_samples += 1;
                let cur_samples = ((br.left() as usize) / step_size / channels).min(BLOCK_LEN - 1);
                for _ in 0..cur_samples {
                    for (ch, state) in self.ch_state[..channels].iter_mut().enumerate() {
                        let idx                 = br.read(step_size as u8)? as u8;
                        dst[off[ch]] = state.expand_sample(idx);
                        off[ch] += 1;
                    }
                }
                tot_samples += cur_samples;
            }
            abuf.truncate_audio(tot_samples);

            let mut frm = NAFrame::new_from_pkt(pkt, info.replace_info(NACodecTypeInfo::Audio(self.ainfo)), abuf);
            frm.set_duration(Some(tot_samples as u64));
            frm.set_keyframe(true);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for ADPCMDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(ADPCMDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::flash_register_all_decoders;
    use crate::flash_register_all_demuxers;
    #[test]
    fn test_flv_adpcm_mono() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        flash_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/FLV/flash_flv_adpcm_testfiles/mono_11k.flv
        test_decoding("flv", "flv-adpcm", "assets/Flash/mono_11k.flv", Some(3000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x4cf30e71, 0x4360c85b, 0x21c52863, 0x1782160e]));
    }
    #[test]
    fn test_flv_adpcm_stereo() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        flash_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/FLV/flash_flv_adpcm_testfiles/stereo_44k.flv
        test_decoding("flv", "flv-adpcm", "assets/Flash/stereo_44k.flv", Some(3000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xae108d38, 0xb36236f8, 0x2bc18d31, 0xac600424]));
    }
}
