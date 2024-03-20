use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::dsp::qmf::QMF;
use std::str::FromStr;
use std::sync::Arc;

const CODEC_SAMPLES: usize = 1152;

struct LHDecoder {
    ainfo:      NAAudioInfo,
    info:       Arc<NACodecInfo>,
    chmap:      NAChannelMap,

    bitalloc:   [[u8; 32]; 3],
    scf_select: [u8; 32],
    scales:     [[u8; 32]; 3],
    samples:    [[f32; 32]; 36],

    bitpos:     u32,

    qmf:        QMF,
}

impl LHDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(22050, 1, SND_F32P_FORMAT, CODEC_SAMPLES),
            info:       NACodecInfo::new_dummy(),
            chmap:      NAChannelMap::new(),

            bitalloc:   [[0; 32]; 3],
            scf_select: [0; 32],
            scales:     [[0; 32]; 3],
            samples:    [[0.0; 32]; 36],

            bitpos:     0,

            qmf:        QMF::new(),
        }
    }
    fn unpack_bitalloc(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        for i in 0..3 {
            for sb in 0..32 {
                self.bitalloc[i][sb] = br.read(BITALLOC_INFO[sb])? as u8;
            }
        }
        Ok(())
    }
    fn unpack_scales(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        for sb in 0..32 {
            if (self.bitalloc[0][sb] | self.bitalloc[1][sb] | self.bitalloc[2][sb]) != 0 {
                self.scf_select[sb] = br.read(2)? as u8;
            } else {
                self.scf_select[sb] = 0;
            }
        }

        self.scales = [[0; 32]; 3];
        for sb in 0..32 {
            let ba0 = self.bitalloc[0][sb];
            let ba1 = self.bitalloc[1][sb];
            let ba2 = self.bitalloc[2][sb];
            if (ba0 | ba1 | ba2) == 0 {
                continue;
            }
            match self.scf_select[sb] {
                0 => {
                    for j in 0..3 {
                        if self.bitalloc[j][sb] != 0 {
                            self.scales[j][sb] = br.read(6)? as u8;
                        }
                    }
                },
                1 => {
                    if (ba0 | ba1) != 0 {
                        let scale = br.read(6)? as u8;
                        self.scales[0][sb] = scale;
                        self.scales[1][sb] = scale;
                    }
                    if ba2 != 0 {
                        self.scales[2][sb] = br.read(6)? as u8;
                    }
                },
                2 => {
                    let scale = br.read(6)? as u8;
                    self.scales[0][sb] = scale;
                    self.scales[1][sb] = scale;
                    self.scales[2][sb] = scale;
                },
                _ => {
                    if ba0 != 0 {
                        self.scales[0][sb] = br.read(6)? as u8;
                    }
                    if (ba1 | ba2) != 0 {
                        let scale = br.read(6)? as u8;
                        self.scales[1][sb] = scale;
                        self.scales[2][sb] = scale;
                    }
                },
            };
        }
        Ok(())
    }
    #[allow(clippy::identity_op)]
    fn unpack_samples(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        for grp in 0..3 {
            for gr in 0..4 {
                for sb in 0..32 {
                    let set = (grp * 4 + gr) * 3;
                    if self.bitalloc[grp][sb] == 0 {
                        self.samples[set + 0][sb] = 0.0;
                        self.samples[set + 1][sb] = 0.0;
                        self.samples[set + 2][sb] = 0.0;
                        continue;
                    }
                    let idx = sb * 4 + (self.bitalloc[grp][sb] as usize);
                    let bits = GROUP_BITS[idx];
                    let sf = SCALEFACTORS[self.scales[grp][sb] as usize];
                    if GROUP_INFO[idx] == 1 {
                        let radix = (1 << bits) - 1;
                        let val0 = br.read(bits)? as usize;
                        let val1 = br.read(bits)? as usize;
                        let val2 = br.read(bits)? as usize;
                        self.samples[set + 0][sb] = Self::dequant(val0, idx, radix) * sf;
                        self.samples[set + 1][sb] = Self::dequant(val1, idx, radix) * sf;
                        self.samples[set + 2][sb] = Self::dequant(val2, idx, radix) * sf;
                    } else {
                        let radix = GROUP_RADIX[idx] as usize;
                        let val = br.read(bits)? as usize;
                        let val0 = val % radix;
                        let val1 = (val / radix) % radix;
                        let val2 = val / radix / radix;
                        self.samples[set + 0][sb] = Self::dequant(val0, idx, radix) * sf;
                        self.samples[set + 1][sb] = Self::dequant(val1, idx, radix) * sf;
                        self.samples[set + 2][sb] = Self::dequant(val2, idx, radix) * sf;
                    }
                }
            }
        }
        Ok(())
    }
    fn dequant(val: usize, idx: usize, radix: usize) -> f32 {
        let qval = match radix {
                3  => QUANTS3[val],
                5  => QUANTS5[val],
                7  => QUANTS7[val],
                15 => QUANTS15[val],
                63 => QUANTS63[val],
                _  => unreachable!(),
            };
        let bias_idx = QUANT_BIAS_MAP[idx] as usize;
        (qval + QUANT_BIAS[bias_idx]) / QUANT_RANGE[bias_idx]
    }
}

impl NADecoder for LHDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), 1, SND_F32P_FORMAT, CODEC_SAMPLES);
            self.info = info.replace_info(NACodecTypeInfo::Audio(self.ainfo));
            self.chmap = NAChannelMap::from_str("C").unwrap();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();

            let mut daudio = Vec::with_capacity(CODEC_SAMPLES);

            let mut br = BitReader::new(pktbuf.as_slice(), BitReaderMode::BE);
            br.skip(self.bitpos)?;

            while br.left() >= 8 {
                self.unpack_bitalloc(&mut br)?;
                self.unpack_scales(&mut br)?;
                self.unpack_samples(&mut br)?;

                let mut samp_buf = [0.0f32; 32];
                for set in 0..36 {
                    self.qmf.synth(&self.samples[set], &mut samp_buf);
                    daudio.extend_from_slice(&samp_buf);
                }
            }

            self.bitpos = (br.tell() as u32) & 7;

            let abuf = alloc_audio_buffer(self.ainfo, daudio.len(), self.chmap.clone())?;
            let mut adata = abuf.get_abuf_f32().unwrap();
            let buf = adata.get_data_mut().unwrap();
            buf[..daudio.len()].copy_from_slice(daudio.as_slice());

            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
            frm.set_duration(Some(CODEC_SAMPLES as u64));
            frm.set_keyframe(true);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn flush(&mut self) {
        self.qmf = QMF::new();
        self.bitpos = 0;
    }
}

impl NAOptionHandler for LHDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(LHDecoder::new())
}

const BITALLOC_INFO: [u8; 32] = [
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
];

const GROUP_BITS: [u8; 128] = [
    0, 3, 4, 6, 0, 3, 4, 6, 0, 3, 4, 6, 0, 3, 4, 6,
    0, 3, 4, 6, 0, 3, 4, 6, 0, 5, 7, 4, 0, 5, 7, 4,
    0, 5, 7, 4, 0, 5, 7, 4, 0, 5, 7, 4, 0, 5, 7, 4,
    0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0,
    0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0,
    0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0,
    0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0,
    0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0
];
const GROUP_INFO: [u8; 128] = [
    0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1,
    0, 1, 1, 1, 0, 1, 1, 1, 0, 3, 3, 1, 0, 3, 3, 1,
    0, 3, 3, 1, 0, 3, 3, 1, 0, 3, 3, 1, 0, 3, 3, 1,
    0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0,
    0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0,
    0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0,
    0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0,
    0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0
];
const GROUP_RADIX: [u8; 128] = [
    0,  7, 15, 63,  0,  7, 15, 63,  0,  7, 15, 63,  0,  7, 15, 63,
    0,  7, 15, 63,  0,  7, 15, 63,  0,  3,  5, 15,  0,  3,  5, 15,
    0,  3,  5, 15,  0,  3,  5, 15,  0,  3,  5, 15,  0,  3,  5, 15,
    0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,
    0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,
    0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,
    0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,
    0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0,  0,  3,  0,  0
];

const QUANT_BIAS_MAP: [u8; 128] = [
    0, 2, 4, 6, 0, 2, 4, 6, 0, 2, 4, 6, 0, 2, 4, 6,
    0, 2, 4, 6, 0, 2, 4, 6, 0, 0, 1, 4, 0, 0, 1, 4,
    0, 0, 1, 4, 0, 0, 1, 4, 0, 0, 1, 4, 0, 0, 1, 4,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
];
const QUANT_BIAS: [f32; 17] = [
    0.5, 0.5, 0.25, 0.5, 0.125, 0.0625, 0.03125, 0.015625,
    0.0078125, 0.00390625, 0.001953125, 0.0009765625, 0.00048828125,
    0.00024414062, 0.00012207031, 0.000061035164, 0.000030517582
];
const QUANT_RANGE: [f32; 17] = [
    0.75, 0.625, 0.875, 0.5625, 0.9375, 0.96875, 0.984375,
    0.9921875, 0.99609375, 0.99804688, 0.99902344, 0.99951172,
    0.99975586, 0.99987793, 0.99993896, 0.99996948, 0.99998474
];
const SCALEFACTORS: [f32; 64] = [
    2.0, 1.587401, 1.2599211, 1.0, 0.79370052, 0.62996054,
    0.5, 0.39685026, 0.31498027, 0.25, 0.19842513, 0.15749013,
    0.125, 0.099212565, 0.078745067, 0.0625, 0.049606282, 0.039372534,
    0.03125, 0.024803141, 0.019686267, 0.015625, 0.012401571, 0.0098431334,
    0.0078125, 0.0062007853, 0.0049215667, 0.00390625, 0.0031003926, 0.0024607833,
    0.001953125, 0.0015501963, 0.0012303917, 0.0009765625, 0.00077509816, 0.00061519584,
    0.00048828125, 0.00038754908, 0.00030759792, 0.00024414062, 0.00019377454, 0.00015379896,
    0.00012207031, 0.00009688727, 0.00007689948, 0.000061035156,
    0.000048443635, 0.00003844974, 0.000030517578, 0.000024221818,
    0.00001922487, 0.000015258789, 0.000012110909, 0.000009612435,
    0.0000076293945, 0.0000060554544, 0.0000048062175, 0.0000038146973,
    0.0000030277272, 0.0000024031087, 0.0000019073486, 0.0000015138636,
    0.0000012015544, 9.9999997e-21
];

const QUANTS3: [f32; 4] = [ -1.0, -0.5, 0.0, 0.5 ];
const QUANTS5: [f32; 6] = [ -1.0, -0.75, -0.5, -0.25, 0.0, 0.25 ];
const QUANTS7: [f32; 8] = [ -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75 ];
const QUANTS15: [f32; 16] = [
    -1.0, -0.875, -0.75, -0.625, -0.5, -0.375, -0.25, -0.125,
     0.0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875 ];
const QUANTS63: [f32; 64] = [
    -1.0,  -0.96875, -0.9375, -0.90625, -0.875, -0.84375, -0.8125, -0.78125,
    -0.75, -0.71875, -0.6875, -0.65625, -0.625, -0.59375, -0.5625, -0.53125,
    -0.5,  -0.46875, -0.4375, -0.40625, -0.375, -0.34375, -0.3125, -0.28125,
    -0.25, -0.21875, -0.1875, -0.15625, -0.125, -0.09375, -0.0625, -0.03125,
     0.0,   0.03125,  0.0625,  0.09375,  0.125,  0.15625,  0.1875,  0.21875,
     0.25,  0.28125,  0.3125,  0.34375,  0.375,  0.40625,  0.4375,  0.46875,
     0.5,   0.53125,  0.5625,  0.59375,  0.625,  0.65625,  0.6875,  0.71875,
     0.75,  0.78125,  0.8125,  0.84375,  0.875,  0.90625,  0.9375,  0.96875 ];

