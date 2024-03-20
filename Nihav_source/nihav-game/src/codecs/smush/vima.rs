use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::codecs::imaadpcm::*;
use std::str::FromStr;

const VIMA_STEPS: [&[i8]; 4] = [
    /*&[ -1, 4, -1, 4 ],
    &[ -1, -1, 2, 6, -1, -1, 2, 6 ],*/
    &[ -1, -1, -1, -1, 1, 2, 4, 6, -1, -1, -1, -1, 1, 2, 4, 6 ],
    &[ -1, -1, -1, -1, -1, -1, -1, -1, 1,  1,  1,  2,  2,  4,  5,  6,
       -1, -1, -1, -1, -1, -1, -1, -1, 1,  1,  1,  2,  2,  4,  5,  6 ],
    &[ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        1,  1,  1,  1,  1,  2,  2,  2,  2,  4,  4,  4,  5,  5,  6,  6,
       -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        1,  1,  1,  1,  1,  2,  2,  2,  2,  4,  4,  4,  5,  5,  6,  6 ],
    &[ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
       -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,
        2,  2,  4,  4,  4,  4,  4,  4,  5,  5,  5,  5,  6,  6,  6,  6,
       -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
       -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,
        2,  2,  4,  4,  4,  4,  4,  4,  5,  5,  5,  5,  6,  6,  6,  6 ]
];

const STEP_TO_BITS: [u8; 89] = [
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
];

struct VIMADecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
}

impl VIMADecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            chmap:      NAChannelMap::new(),
        }
    }
}

impl NADecoder for VIMADecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            let channels = ainfo.get_channels();
            validate!(channels == 1 || channels == 2);
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), channels, SND_S16P_FORMAT, 1);
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
            validate!(src.len() > 4);

            let mut br = BitReader::new(&src, BitReaderMode::BE);
            let mut samples             = br.read(32)? as usize;
            if samples == 0xFFFFFFFF {
                                          br.skip(32)?;
                samples                 = br.read(32)? as usize;
            }

            let mut steps = [0; 2];
            steps[0]                    = br.read(8)? as usize;
            let stereo = (steps[0] & 0x80) != 0;
            validate!(!stereo || (self.chmap.num_channels() == 2));
            if stereo {
                steps[0] ^= 0xFF;
            }
            validate!(steps[0] <= (IMA_MAX_STEP as usize));

            let mut predictor = [0; 2];
            predictor[0]                = br.read_s(16)?;
            if stereo {
                steps[1]                = br.read(8)? as usize;
                validate!(steps[1] <= (IMA_MAX_STEP as usize));
                predictor[1]            = br.read_s(16)?;
            }

            let abuf = alloc_audio_buffer(self.ainfo, samples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let offset = adata.get_offset(1);
            let adata = adata.get_data_mut().unwrap();
            let (l, r) = adata.split_at_mut(offset);

            for (ch_no, ch) in [l, r].iter_mut().take(if stereo { 2 } else { 1 }).enumerate() {
                let mut step = steps[ch_no];
                let mut sample = predictor[ch_no];

                for dst in ch.iter_mut().take(samples) {
                    let bits = STEP_TO_BITS[step];
                    let mask = 1 << (bits - 1);

                    let idx             = br.read(bits)? as u8;

                    sample = if (idx & !mask) != (mask - 1) {
                            let sign = (idx & mask) != 0;
                            let aidx = idx & !mask;

                            let mut diff = (i32::from(2 * aidx + 1) * IMA_STEP_TABLE[step]) >> (bits - 1);
                            if sign {
                                diff = -diff;
                            }

                            (sample + diff).max(-32768).min(32767)
                        } else {
                                          br.read_s(16)?
                        };
                    step = ((step as i8) + VIMA_STEPS[(bits - 4) as usize][idx as usize]).max(0).min(IMA_MAX_STEP as i8) as usize;
                    *dst = sample as i16;
                }
            }
            if !stereo && self.chmap.num_channels() == 2 {
                let (l, r) = adata.split_at_mut(offset);
                r[..samples].copy_from_slice(l);
            }

            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
            frm.set_duration(Some(samples as u64));
            frm.set_keyframe(true);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for VIMADecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_vima() -> Box<dyn NADecoder + Send> {
    Box::new(VIMADecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    #[test]
    fn test_smush_vima() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // samples from Grim Fandango
        test_decoding("smush", "smush-vima", "assets/Game/smush/lol.snm", Some(75000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xddd5dce1, 0xd5dc353c, 0xba176be8, 0x5afade63]));
        test_decoding("smush", "smush-vima", "assets/Game/smush/ac_bu.snm", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x97a548e7, 0xb22d082b, 0x14c4110b, 0x9723891f]));
        test_decoding("smush-mcmp", "smush-vima", "assets/Game/smush/1104 - Lupe.IMC", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x78389e65, 0xd99458a9, 0x6c62904e, 0xcaf732ba]));
    }
}

