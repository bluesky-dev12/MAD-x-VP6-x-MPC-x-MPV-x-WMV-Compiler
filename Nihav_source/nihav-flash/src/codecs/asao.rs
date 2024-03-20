use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::dsp::mdct::*;
use nihav_codec_support::dsp::window::*;

struct Random { state: u32 }

impl Random {
    fn new(seed: u32) -> Self { Self { state: seed } }
    fn next(&mut self) -> u32 {
        self.state ^= self.state << 13;
        self.state ^= self.state >> 17;
        self.state ^= self.state <<  5;
        self.state
    }
}

const NUM_BANDS: usize = 23;
const BLOCK_LEN: usize = 256;
const PACKED_BLK_LEN: usize = 64;
const CODED_LEN: usize = 124;

struct ASAODecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    window:     [f32; 128],
    imdct:      IMDCT,
    rng:        Random,

    scales:     [f32; 128],
    iscales:    [i32; 128],
    bits:       [i8; 128],
    coeffs:     [f32; 128],
    prev:       [f32; 128],
    tmp:        [f32; 128],
}

const HEADER_BITS:  u32 = 116;
const SUBPART_BITS: i32 = (((PACKED_BLK_LEN as u32) * 8 - HEADER_BITS) / 2) as i32;
const MAX_CBITS: i32 = 6;
const BASE_OFF: i32 = 4228;
const BASE_SHIFT: i8 = 19;

trait SignedShift {
    type Output;
    fn sshift(self, shift: i8) -> Self::Output;
}

impl SignedShift for i32 {
    type Output = Self;
    fn sshift(self, shift: i8) -> Self::Output {
        if shift >= 0 {
            self << shift
        } else {
            self >> -shift
        }
    }
}

fn norm(val: &mut i32) -> i8 {
    if *val == 0 {
        31
    } else {
        let shift = val.abs().leading_zeros() - 1;
        *val <<= shift;
        shift as i8
    }
}

fn sum_bits(src: &[i32; 128], shift: i8, off: i32) -> i32 {
    let mut sum = 0;
    for &el in src[..CODED_LEN].iter() {
        let val = (((el - off) >> (shift - 1)) + 1) >> 1;
        sum += val.max(0).min(MAX_CBITS);
    }
    sum
}

fn bitalloc(bits: &mut [i8; 128], scales: &[i32; 128]) {
    let mut max = scales[..CODED_LEN].iter().fold(scales[0], |v, &x| v.max(x));
    let mut shift = norm(&mut max) - 16;

    let mut tmp = [0; 128];
    let mut sum = 0;
    for i in 0..CODED_LEN {
        tmp[i] = (scales[i].sshift(shift) * 3) >> 2;
        sum += tmp[i];
    }

    shift += 11;
    let ref_shift = shift;

    sum -= SUBPART_BITS << shift;
    shift += norm(&mut sum);
    shift = ref_shift - (BASE_SHIFT + shift - 31);
    let mut offset = ((BASE_OFF * (sum >> 16)) >> 15).sshift(shift);

    let mut bitsum = sum_bits(&tmp, ref_shift, offset);

    if bitsum != SUBPART_BITS {
        let mut off2 = bitsum - SUBPART_BITS;
        shift = 0;
        while off2.abs() <= 0x3FFF {
            off2 <<= 1;
            shift += 1;
        }

        shift = ref_shift - (BASE_SHIFT + shift - 15);
        off2 = ((BASE_OFF * off2) >> 15).sshift(shift);

        let mut last_off = 0;
        let mut last_bitsum = 0;
        let mut iter = 1;
        while iter < 20 {
            last_off = offset;
            offset += off2;
            last_bitsum = bitsum;
            bitsum = sum_bits(&tmp, ref_shift, offset);
            if (bitsum - SUBPART_BITS) * (last_bitsum - SUBPART_BITS) <= 0 {
                break;
            }
            iter += 1;
        }

        let (mut big_off, mut small_off, mut big_sum, mut small_sum) = if bitsum > SUBPART_BITS {
                (offset, last_off, bitsum, last_bitsum)
            } else {
                (last_off, offset, last_bitsum, bitsum)
            };

        while bitsum != SUBPART_BITS && iter < 20 {
            let off = (big_off + small_off) >> 1;
            bitsum = sum_bits(&tmp, ref_shift, off);
            if bitsum > SUBPART_BITS {
                big_off = off;
                big_sum = bitsum;
            } else {
                small_off = off;
                small_sum = bitsum;
            }
            iter += 1;
        }

        if (big_sum - SUBPART_BITS).abs() >= (small_sum - SUBPART_BITS).abs() {
            offset = small_off;
            bitsum = small_sum;
        } else {
            offset = big_off;
            bitsum = big_sum;
        }
    }

    for (bits, &val) in bits.iter_mut().zip(tmp.iter()).take(CODED_LEN) {
        *bits = ((((val - offset) >> (ref_shift - 1)) + 1) >> 1).max(0).min(MAX_CBITS) as i8;
    }
    if bitsum > SUBPART_BITS {
        let mut sum = 0;
        let mut i = 0;
        while sum < SUBPART_BITS {
            sum += i32::from(bits[i]);
            i += 1;
        }
        bits[i - 1] -= (sum - SUBPART_BITS) as i8;
        while i < CODED_LEN {
            bits[i] = 0;
            i += 1;
        }
    }
}

fn overlap_add(dst: &mut [f32], src: &[f32; 128], prev: &[f32; 128], window: &[f32; 128]) {
    for i in 0..64 {
        let p = prev[64 + i];
        let s = src [63 - i];
        let w0 = window[i];
        let w1 = window[127 - i];
        dst[i]       = p * w1 - s * w0;
        dst[127 - i] = p * w0 + s * w1;
    }
}

impl ASAODecoder {
    fn new() -> Self {
        let mut window = [0.0; 128];
        generate_window(WindowType::Sine, 1.0/1024.0, 128, true, &mut window);

        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_F32P_FORMAT, BLOCK_LEN),
            chmap:      NAChannelMap::new(),
            window,
            imdct:      IMDCT::new(256, true),
            rng:        Random::new(42),

            scales:     [0.0; 128],
            iscales:    [0; 128],
            bits:       [0; 128],
            coeffs:     [0.0; 128],
            prev:       [0.0; 128],
            tmp:        [0.0; 128],
        }
    }
    fn decode_block(&mut self, br: &mut BitReader, dst: &mut [f32]) -> DecoderResult<()> {
        let mut scale = i32::from(SCALE[br.read(6)? as usize]);
        let mut sc_iter = self.scales.iter_mut();
        let mut isc_iter = self.iscales.iter_mut();
        for (band, &band_size) in BAND_SIZES.iter().enumerate() {
            if band > 0 {
                scale += i32::from(SCALE_DIFF[br.read(5)? as usize]);
            }
            let scf = -(2.0f32.powf((scale as f32) / 2048.0));
            for _ in 0..band_size {
                *sc_iter.next().unwrap() = scf;
                *isc_iter.next().unwrap() = scale;
            }
        }

        bitalloc(&mut self.bits, &self.iscales);

        let mut coeffs = &mut self.coeffs;
        let mut prev   = &mut self.prev;
        for (i, out) in dst.chunks_exact_mut(BLOCK_LEN / 2).take(2).enumerate() {
            br.seek(HEADER_BITS + (SUBPART_BITS as u32) * (i as u32))?;
            for j in 0..CODED_LEN {
                if self.bits[j] <= 0 {
                    self.tmp[j] = std::f32::consts::FRAC_1_SQRT_2;
                    if (self.rng.next() & 1) != 0 {
                        self.tmp[j] = -self.tmp[j];
                    }
                } else {
                    let val = br.read(self.bits[j] as u8)? as usize;
                    self.tmp[j] = QUANT_VALS[self.bits[j] as usize][val];
                }
                self.tmp[j] *= self.scales[j];
            }
            for j in CODED_LEN..128 {
                self.tmp[j] = 0.0;
            }
            self.imdct.imdct_half(&self.tmp, coeffs);
            overlap_add(out, coeffs, prev, &self.window);
            std::mem::swap(&mut coeffs, &mut prev);
        }

        Ok(())
    }
}


impl NADecoder for ASAODecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), 1, SND_F32P_FORMAT, BLOCK_LEN);
            self.chmap = NAChannelMap::new();
            self.chmap.add_channel(NAChannelType::C);
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let src = pkt.get_buffer();

            validate!((src.len() % PACKED_BLK_LEN) == 0);
            let npkts = src.len() / PACKED_BLK_LEN;
            let nsamples = npkts * BLOCK_LEN;
            let abuf = alloc_audio_buffer(self.ainfo, nsamples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_f32().unwrap();
            let dst = adata.get_data_mut().unwrap();

            for (src, dst) in src.chunks_exact(PACKED_BLK_LEN).zip(dst.chunks_mut(BLOCK_LEN)) {
                let mut br = BitReader::new(src, BitReaderMode::LE);
                self.decode_block(&mut br, dst)?;
            }

            let mut frm = NAFrame::new_from_pkt(pkt, info.replace_info(NACodecTypeInfo::Audio(self.ainfo)), abuf);
            frm.set_duration(Some(nsamples as u64));
            frm.set_keyframe(true);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for ASAODecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(ASAODecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::flash_register_all_decoders;
    use crate::flash_register_all_demuxers;
    #[test]
    fn test_asao() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        flash_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/Nelly_Moser/nellymoser-in-flv.flv
        test_decode_audio("flv", "assets/Flash/nellymoser-in-flv.flv", Some(3000), None/*Some("asao")*/, &dmx_reg, &dec_reg);
    }
}

#[allow(clippy::excessive_precision)]
const QUANT_VALS: [&[f32]; 7] = [
  &[  0.0000000000 ],
  &[ -0.8472560048,  0.7224709988 ],
  &[ -1.5247479677, -0.4531480074,  0.3753609955,  1.4717899561 ],
  &[ -1.9822579622, -1.1929379702, -0.5829370022, -0.0693780035,
      0.3909569979,  0.9069200158,  1.4862740040,  2.2215409279 ],
  &[ -2.3887870312, -1.8067539930, -1.4105420113, -1.0773609877,
     -0.7995010018, -0.5558109879, -0.3334020078, -0.1324490011,
      0.0568020009,  0.2548770010,  0.4773550034,  0.7386850119,
      1.0443060398,  1.3954459429,  1.8098750114,  2.3918759823 ],
  &[ -2.3893830776, -1.9884680510, -1.7514040470, -1.5643119812,
     -1.3922129869, -1.2164649963, -1.0469499826, -0.8905100226,
     -0.7645580173, -0.6454579830, -0.5259280205, -0.4059549868,
     -0.3029719889, -0.2096900046, -0.1239869967, -0.0479229987,
      0.0257730000,  0.1001340002,  0.1737180054,  0.2585540116,
      0.3522900045,  0.4569880068,  0.5767750144,  0.7003160119,
      0.8425520062,  1.0093879700,  1.1821349859,  1.3534560204,
      1.5320819616,  1.7332619429,  1.9722349644,  2.3978140354 ],
  &[ -2.5756309032, -2.0573320389, -1.8984919786, -1.7727810144,
     -1.6662600040, -1.5742180347, -1.4993319511, -1.4316639900,
     -1.3652280569, -1.3000990152, -1.2280930281, -1.1588579416,
     -1.0921250582, -1.0135740042, -0.9202849865, -0.8287050128,
     -0.7374889851, -0.6447759867, -0.5590940118, -0.4857139885,
     -0.4110319912, -0.3459700048, -0.2851159871, -0.2341620028,
     -0.1870580018, -0.1442500055, -0.1107169986, -0.0739680007,
     -0.0365610011, -0.0073290002,  0.0203610007,  0.0479039997,
      0.0751969963,  0.0980999991,  0.1220389977,  0.1458999962,
      0.1694349945,  0.1970459968,  0.2252430022,  0.2556869984,
      0.2870100141,  0.3197099864,  0.3525829911,  0.3889069855,
      0.4334920049,  0.4769459963,  0.5204820037,  0.5644530058,
      0.6122040153,  0.6685929894,  0.7341650128,  0.8032159805,
      0.8784040213,  0.9566209912,  1.0397069454,  1.1293770075,
      1.2211159468,  1.3080279827,  1.4024800062,  1.5056819916,
      1.6227730513,  1.7724959850,  1.9430880547,  2.2903931141 ]
];

const SCALE_DIFF: [i16; 32] = [
    -11725, -9420, -7910, -6801, -5948, -5233, -4599, -4039,
     -3507, -3030, -2596, -2170, -1774, -1383, -1016,  -660,
      -329,    -1,   337,   696,  1085,  1512,  1962,  2433,
      2968,  3569,  4314,  5279,  6622,  8154, 10076, 12975
];

const BAND_SIZES: [usize; NUM_BANDS] = [
    2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 6, 6, 7, 8, 9, 10, 12, 14, 15
];

const SCALE: [u16; 64] = [
     3134,  5342,  6870,  7792,  8569,  9185,  9744, 10191,
    10631, 11061, 11434, 11770, 12116, 12513, 12925, 13300,
    13674, 14027, 14352, 14716, 15117, 15477, 15824, 16157,
    16513, 16804, 17090, 17401, 17679, 17948, 18238, 18520,
    18764, 19078, 19381, 19640, 19921, 20205, 20500, 20813,
    21162, 21465, 21794, 22137, 22453, 22756, 23067, 23350,
    23636, 23926, 24227, 24521, 24819, 25107, 25414, 25730,
    26120, 26497, 26895, 27344, 27877, 28463, 29426, 31355
];
