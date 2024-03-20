use nihav_core::formats::SND_F32P_FORMAT;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_codec_support::dsp::mdct::IMDCT;
use nihav_codec_support::dsp::window::{generate_window, WindowType};
use std::str::FromStr;

const SAMPLES: usize = 320;
const NUM_REGIONS: usize = 14;
const REGION_SIZE: usize = 20;
const NUM_DIFF_CB: usize = NUM_REGIONS - 1;

struct PRNG {
    dw: [u16; 4],
}

impl PRNG {
    fn new() -> Self { Self{ dw: [1; 4] } }
    fn get_dw(&mut self) -> u16 {
        let mut ret = self.dw[0].wrapping_add(self.dw[3]);
        if (ret & 0x8000) != 0 {
            ret = ret.wrapping_add(1);
        }

        self.dw[0] = self.dw[1];
        self.dw[1] = self.dw[2];
        self.dw[2] = self.dw[3];
        self.dw[3] = ret;

        ret
    }
}

struct SirenDecoder {
    chmap:              NAChannelMap,
    ainfo:              NAAudioInfo,
    info:               NACodecInfoRef,
    diff_cb:            Vec<Codebook<i8>>,
    vec_cb:             [Codebook<u16>; 7],
    imdct:              IMDCT,

    coeffs:             [f32; SAMPLES],
    delay:              [f32; SAMPLES],
    synth_buf:          [f32; SAMPLES * 2],
    last_good_frame:    [f32; SAMPLES],
    window:             [f32; SAMPLES],
    quant_tab:          [f32; 64],

    pow_index:          [i32; NUM_REGIONS],
    region_quant:       [f32; NUM_REGIONS],
    power_cat:          [usize; NUM_REGIONS],
    cat_balance:        [usize; 16],

    rng:                PRNG,
}

fn map_idx_diff(idx: usize) -> i8 { (idx as i8) - 12 }
fn map_idx0(idx: usize) -> u16 { VEC_CB0_SYMS[idx] }
fn map_idx1(idx: usize) -> u16 { VEC_CB1_SYMS[idx] }
fn map_idx2(idx: usize) -> u16 { VEC_CB2_SYMS[idx] }
fn map_idx3(idx: usize) -> u16 { VEC_CB3_SYMS[idx] }
fn map_idx4(idx: usize) -> u16 { VEC_CB4_SYMS[idx] }
fn map_idx5(idx: usize) -> u16 { VEC_CB5_SYMS[idx] }
fn map_idx6(idx: usize) -> u16 { VEC_CB6_SYMS[idx] }
impl SirenDecoder {
    fn new() -> Self {
        let mut diff_cb = Vec::with_capacity(NUM_DIFF_CB);
        for i in 0..NUM_DIFF_CB {
            let mut cr = TableCodebookDescReader::new(&DIFF_CODE_BITS[i], &DIFF_CODE_LENS[i], map_idx_diff);
            let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
            diff_cb.push(cb);
        }
        let mut cr = TableCodebookDescReader::new(&VEC_CB0_BITS, &VEC_CB0_LENS, map_idx0);
        let vec_cb0 = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        let mut cr = TableCodebookDescReader::new(&VEC_CB1_BITS, &VEC_CB1_LENS, map_idx1);
        let vec_cb1 = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        let mut cr = TableCodebookDescReader::new(&VEC_CB2_BITS, &VEC_CB2_LENS, map_idx2);
        let vec_cb2 = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        let mut cr = TableCodebookDescReader::new(&VEC_CB3_BITS, &VEC_CB3_LENS, map_idx3);
        let vec_cb3 = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        let mut cr = TableCodebookDescReader::new(&VEC_CB4_BITS, &VEC_CB4_LENS, map_idx4);
        let vec_cb4 = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        let mut cr = TableCodebookDescReader::new(&VEC_CB5_BITS, &VEC_CB5_LENS, map_idx5);
        let vec_cb5 = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        let mut cr = TableCodebookDescReader::new(&VEC_CB6_BITS, &VEC_CB6_LENS, map_idx6);
        let vec_cb6 = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        let vec_cb = [vec_cb0, vec_cb1, vec_cb2, vec_cb3, vec_cb4, vec_cb5, vec_cb6];

        let mut window = [0.0f32; SAMPLES];
        generate_window(WindowType::Sine, 1.0 / 320.0 / 64.0, SAMPLES, true, &mut window);

        let mut quant_tab = [0.0; 64];
        for i in 0..64 {
            quant_tab[i] = 2.0f32.powf((((i as i32) - 24) as f32) / 2.0);
        }

        SirenDecoder {
            chmap:              NAChannelMap::from_str("C").unwrap(),
            ainfo:              NAAudioInfo::new(16000, 1, SND_F32P_FORMAT, SAMPLES),
            info:               NACodecInfo::new_dummy(),
            diff_cb, vec_cb,

            coeffs:             [0.0; SAMPLES],
            delay:              [0.0; SAMPLES],
            synth_buf:          [0.0; SAMPLES * 2],
            last_good_frame:    [0.0; SAMPLES],
            imdct:              IMDCT::new(SAMPLES * 2, false),
            window, quant_tab,

            pow_index:          [0; NUM_REGIONS],
            region_quant:       [0.0; NUM_REGIONS],
            power_cat:          [0; NUM_REGIONS],
            cat_balance:        [0; 16],

            rng:                PRNG::new(),
        }
    }
    fn decode_envelope(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        self.pow_index[0]               = (br.read(5)? as i32) - 7;

        for i in 1..NUM_REGIONS {
            let diff                    = br.read_cb(&self.diff_cb[i - 1])?;
            self.pow_index[i] = (self.pow_index[i - 1] + i32::from(diff)).max(-24).min(39);
        }
        for i in 0..NUM_REGIONS {
            self.region_quant[i]        = self.quant_tab[(self.pow_index[i] + 24) as usize];
        }

        Ok(())
    }
    fn allocate_bits(&mut self, tot_bits: u32, rate_ctl: usize) -> DecoderResult<()> {
        const CATEGORY_BITS: [u32; 8] = [ 52, 47, 43, 37, 29, 22, 16, 0 ];
        const MAX_RC: usize = 15;

        let mut offset = -32;
        let mut delta = 32;
        while delta > 0 {
            let mut bitpool = 0;
            for reg in 0..NUM_REGIONS {
                let cat = ((delta + offset - self.pow_index[reg]) >> 1).max(0).min(7) as usize;
                //self.power_cat[reg] = cat;
                bitpool += CATEGORY_BITS[cat];
            }
            if bitpool >= tot_bits - 32 {
                offset += delta;
            }
            delta >>= 1;
        }

        let mut bitpool = 0;
        let mut max_rate_cat = [0; NUM_REGIONS];
        let mut min_rate_cat = [0; NUM_REGIONS];
        for reg in 0..NUM_REGIONS {
            let cat = ((offset - self.pow_index[reg]) >> 1).max(0).min(7) as usize;
            max_rate_cat[reg] = cat;
            min_rate_cat[reg] = cat;
            self.power_cat[reg] = cat;
            bitpool += CATEGORY_BITS[cat];
        }

        let mut min_bitpool = bitpool;
        let mut max_bitpool = bitpool;
        let mut min_offset = MAX_RC + 1;
        let mut max_offset = MAX_RC + 1;
        let mut temp_cat = [0; 64];
        for _ in 0..MAX_RC {
            if min_bitpool + max_bitpool > tot_bits * 2 {
                let mut max_value = -99;
                let mut min_idx = 0;
                for reg in (0..NUM_REGIONS).rev() {
                    if min_rate_cat[reg] >= 7 { continue; }
                    let val = offset - self.pow_index[reg] - 2 * (min_rate_cat[reg] as i32);
                    if max_value < val {
                        max_value = val;
                        min_idx = reg;
                    }
                }
                validate!(max_value != -99);
                temp_cat[min_offset] = min_idx;
                min_offset += 1;
                min_bitpool = min_bitpool + CATEGORY_BITS[min_rate_cat[min_idx] + 1] - CATEGORY_BITS[min_rate_cat[min_idx]];
                min_rate_cat[min_idx] += 1;
            } else {
                let mut min_value = 99;
                let mut max_idx = 0;
                for reg in 0..NUM_REGIONS {
                    if max_rate_cat[reg] == 0 { continue; }
                    let val = offset - self.pow_index[reg] - 2 * (max_rate_cat[reg] as i32);
                    if min_value > val {
                        min_value = val;
                        max_idx = reg;
                    }
                }
                validate!(min_value != 99);
                max_offset -= 1;
                temp_cat[max_offset] = max_idx;
                max_bitpool = max_bitpool + CATEGORY_BITS[max_rate_cat[max_idx] - 1] - CATEGORY_BITS[max_rate_cat[max_idx]];
                max_rate_cat[max_idx] -= 1;
            }
        }

        self.power_cat = max_rate_cat;

        self.cat_balance[..MAX_RC].copy_from_slice(&temp_cat[max_offset..][..MAX_RC]);

        for cat in self.cat_balance.iter().take(rate_ctl) {
            self.power_cat[*cat] += 1;
        }
        Ok(())
    }
    fn decode_coefficients(&mut self, br: &mut BitReader) -> DecoderResult<bool> {
        const NUM_VECTORS: [u8; 8] = [ 10, 10, 10, 5, 5, 4, 4, 20 ];
        const VECTOR_SIZE: [u8; 8] = [ 2, 2, 2, 4, 4, 5, 5, 1 ];
        const INDEX_BITS:  [u8; 8] = [ 4, 4, 3, 3, 2, 2, 1, 0 ];

        let mut error = false;
        self.coeffs = [0.0; SAMPLES];
        for (reg, coeffs) in self.coeffs.chunks_exact_mut(REGION_SIZE).take(NUM_REGIONS).enumerate() {
            let mut cat = self.power_cat[reg];
            if cat < 7 {
                let cb = &self.vec_cb[cat];
                let num_vecs = NUM_VECTORS[cat] as usize;
                let vec_len  = VECTOR_SIZE[cat] as usize;

                'vec_loop: for i in 0..num_vecs {
                    let ret             = br.read_cb(cb);
                    if ret.is_err() {
                        error = true;
                        break 'vec_loop;
                    }
                    let mut idx = ret.ok().unwrap_or(0) as usize;
                    let mask = (1 << INDEX_BITS[cat]) - 1;
                    for j in 0..vec_len {
                        let coef = QUANT_LEVELS[cat][idx & mask] * self.region_quant[reg];
                        idx >>= INDEX_BITS[cat];
                        if coef != 0.0 {
                            if br.left() == 0 {
                                break 'vec_loop;
                            }
                            coeffs[i * vec_len + j] = if br.read_bool()? { -coef } else { coef };
                        }
                    }
                }
                if error {
                    cat = 7;
                    for i in reg..NUM_REGIONS {
                        self.power_cat[i] = 7;
                    }
                }
            }
            let noise_val = match cat {
                    5 => {
                        let mut nz_count = 0;
                        for el in coeffs.iter() {
                            if *el != 0.0 {
                                if *el > 2.0 * self.region_quant[reg] {
                                    nz_count += 4;
                                }
                                nz_count += 1;
                            }
                        }
                        if nz_count < REGION_SIZE {
                            self.region_quant[reg] * CAT5_NOISE_FACTOR[nz_count]
                        } else {
                            0.0
                        }
                    },
                    6 => {
                        let mut nz_count = 0;
                        for el in coeffs.iter() {
                            if *el != 0.0 {
                                nz_count += 1;
                            }
                        }
                        self.region_quant[reg] * CAT6_NOISE_FACTOR[nz_count]
                    },
                    7 => {
                        self.region_quant[reg] * 0.70711
                    },
                    _ => 0.0,
                };
            if cat >= 5 {
                let mut dw1 = self.rng.get_dw();
                let mut dw2 = self.rng.get_dw();
                for pair in coeffs.chunks_exact_mut(2) {
                    if (cat == 7) || pair[0] == 0.0 {
                        pair[0] = if (dw1 & 1) != 0 { noise_val } else { -noise_val };
                    }
                    if (cat == 7) || pair[1] == 0.0 {
                        pair[1] = if (dw2 & 1) != 0 { noise_val } else { -noise_val };
                    }
                    dw1 >>= 1;
                    dw2 >>= 1;
                }
            }
        }
        Ok(error)
    }
}

impl NADecoder for SirenDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(_ainfo) = info.get_properties() {
            self.info = info.replace_info(NACodecTypeInfo::Audio(self.ainfo));
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        validate!(info.get_properties().is_audio());
        let src = pkt.get_buffer();

        let mut br = BitReader::new(src.as_slice(), BitReaderMode::BE);
        self.decode_envelope(&mut br)?;
        let rate_ctl = br.read(4)? as usize;
        self.allocate_bits(br.left() as u32, rate_ctl)?;
        self.decode_coefficients(&mut br)?;

        let mut bad_frame = false;
        while br.left() > 0 {
            bad_frame                   |= !br.read_bool().ok().unwrap_or(true);
        }
        for el in self.pow_index.iter() {
            if *el > 33 {
                bad_frame = true;
            }
        }

        if bad_frame {
            self.coeffs.copy_from_slice(&self.last_good_frame);
            self.last_good_frame = [0.0; SAMPLES];
        } else {
            self.last_good_frame.copy_from_slice(&self.coeffs);
        }

        let abuf = alloc_audio_buffer(self.ainfo, SAMPLES, self.chmap.clone())?;
        let mut adata = abuf.get_abuf_f32().unwrap();
        let asamples = adata.get_data_mut().unwrap();

        self.imdct.imdct_half(&self.coeffs, &mut self.synth_buf);
        for i in (0..SAMPLES/2).step_by(2) {
            let idx0 = i;
            let idx1 = SAMPLES / 2 - 1 - i;
            let idx2 = SAMPLES / 2     + i;
            let idx3 = SAMPLES     - 1 - i;
            // samples are actually in reverse order
            let c3 = self.synth_buf[idx0];
            let c2 = self.synth_buf[idx1];
            let c1 = self.synth_buf[idx2];
            let c0 = self.synth_buf[idx3];

            let d0 = self.delay[idx0];
            let d1 = self.delay[idx1];

            let w0 = self.window[idx0];
            let w1 = self.window[idx1];
            let w2 = self.window[idx2];
            let w3 = self.window[idx3];

            asamples[idx3] = c1 * w3 - d0 * w0;
            asamples[idx2] = c0 * w2 - d1 * w1;
            asamples[idx1] = d1 * w2 + c0 * w1;
            asamples[idx0] = d0 * w3 + c1 * w0;

            self.delay[idx0] = c2;
            self.delay[idx1] = c3;
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.delay           = [0.0; SAMPLES];
        self.last_good_frame = [0.0; SAMPLES];
    }
}

impl NAOptionHandler for SirenDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(SirenDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::vivo_register_all_decoders;
    use crate::vivo_register_all_demuxers;
    #[test]
    fn test_siren() {
        let mut dmx_reg = RegisteredDemuxers::new();
        vivo_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        vivo_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/vivo/vivo2/favmovie.viv
        let file = "assets/Misc/favmovie.viv";
        //let file = "assets/Misc/gr_al.viv";
        test_decode_audio("vivo", file, None, None/*Some("siren")*/, &dmx_reg, &dec_reg);
    }
}

const DIFF_CODE_BITS: [[u16; 24]; NUM_DIFF_CB] = [
  [
    0x0008, 0x0026, 0x0012, 0x000A, 0x0007, 0x0006, 0x0003, 0x0002,
    0x0000, 0x0001, 0x0007, 0x0006, 0x0005, 0x0004, 0x000B, 0x004E,
    0x009E, 0x013E, 0x04FE, 0x04FF, 0x09F8, 0x09F9, 0x09FA, 0x09FB
  ], [
    0x0024, 0x0008, 0x0003, 0x0005, 0x0000, 0x0001, 0x0007, 0x0006,
    0x0004, 0x0003, 0x0002, 0x0005, 0x0003, 0x0004, 0x0005, 0x0013,
    0x004A, 0x0096, 0x012E, 0x04BD, 0x04BE, 0x04BF, 0x0978, 0x0979
  ], [
    0x0A16, 0x0284, 0x00A0, 0x0029, 0x0005, 0x000B, 0x0007, 0x0005,
    0x0004, 0x0001, 0x0000, 0x0006, 0x0004, 0x0007, 0x0003, 0x0006,
    0x0004, 0x0015, 0x0051, 0x0143, 0x050A, 0x142F, 0x285C, 0x285D
  ], [
    0x0B7C, 0x016E, 0x00B5, 0x00B4, 0x002F, 0x002E, 0x001B, 0x000A,
    0x0008, 0x0005, 0x0001, 0x0000, 0x0003, 0x0007, 0x0004, 0x0009,
    0x000C, 0x001A, 0x002C, 0x00B6, 0x02DE, 0x0B7D, 0x0B7E, 0x0B7F
  ], [
    0x0F8E, 0x1F1F, 0x03E2, 0x00F9, 0x003F, 0x001A, 0x0013, 0x0012,
    0x000E, 0x0008, 0x0006, 0x0001, 0x0000, 0x0002, 0x0005, 0x0007,
    0x000C, 0x001E, 0x001B, 0x007D, 0x01F0, 0x07C6, 0x3E3C, 0x3E3D
  ], [
    0x0CB6, 0x065A, 0x0197, 0x00CE, 0x00CA, 0x0064, 0x001E, 0x000E,
    0x0003, 0x0005, 0x0003, 0x0000, 0x0002, 0x0004, 0x0002, 0x000D,
    0x0018, 0x001F, 0x0066, 0x00CF, 0x032C, 0x196F, 0x32DC, 0x32DD
  ], [
    0x0456, 0x08A8, 0x0457, 0x008B, 0x0023, 0x0009, 0x0003, 0x0014,
    0x000B, 0x0004, 0x0002, 0x0001, 0x0003, 0x0003, 0x0001, 0x0000,
    0x0015, 0x0005, 0x0010, 0x0044, 0x0114, 0x08A9, 0x08AA, 0x08AB
  ], [
    0x03F5, 0x03F6, 0x007F, 0x003E, 0x001D, 0x0006, 0x0004, 0x0010,
    0x0000, 0x0001, 0x0003, 0x0002, 0x0003, 0x0001, 0x0005, 0x0009,
    0x0011, 0x0005, 0x001C, 0x001E, 0x00FC, 0x03F7, 0x07E8, 0x07E9
  ], [
    0x017D, 0x017C, 0x0174, 0x00BF, 0x005E, 0x002C, 0x0010, 0x000A,
    0x0007, 0x0003, 0x0001, 0x0000, 0x0002, 0x0006, 0x0009, 0x0011,
    0x002D, 0x005C, 0x00BB, 0x02EA, 0x05D6, 0x0BAF, 0x175C, 0x175D
  ], [
    0x0BDC, 0x02F6, 0x00BC, 0x002D, 0x002B, 0x000A, 0x0004, 0x0003,
    0x0006, 0x0004, 0x0002, 0x0000, 0x0003, 0x0007, 0x000B, 0x0014,
    0x002A, 0x002C, 0x002E, 0x005F, 0x017A, 0x0BDD, 0x0BDE, 0x0BDF
  ], [
    0x02EF, 0x005C, 0x002D, 0x0014, 0x001A, 0x0004, 0x000C, 0x0007,
    0x0004, 0x0000, 0x0004, 0x0001, 0x0003, 0x0005, 0x0005, 0x0003,
    0x001B, 0x0015, 0x002C, 0x002F, 0x00BA, 0x0176, 0x05DC, 0x05DD
  ], [
    0xB204, 0x1641, 0x0B21, 0x0591, 0x0165, 0x002D, 0x0017, 0x0006,
    0x000A, 0x0007, 0x0002, 0x0002, 0x0003, 0x0000, 0x0004, 0x0006,
    0x0007, 0x0058, 0x00B3, 0x02C9, 0x2C80, 0xB205, 0xB206, 0xB207
  ], [
    0x09CF, 0x1398, 0x139A, 0x1399, 0x0138, 0x004F, 0x0026, 0x0024,
    0x001E, 0x000E, 0x0006, 0x0000, 0x0002, 0x0001, 0x0003, 0x0005,
    0x0008, 0x001F, 0x0025, 0x009D, 0x0272, 0x139B, 0x139C, 0x139D
  ]
];
const DIFF_CODE_LENS: [[u8; 24]; 13/*27*/] = [
 [  4,  6,  5,  5,  4,  4,  4,  4,  4,  4,  3,  3,  3,  4,  5,  7,  8,  9, 11, 11, 12, 12, 12, 12 ],
 [ 10,  8,  6,  5,  5,  4,  3,  3,  3,  3,  3,  3,  4,  5,  7,  9, 11, 12, 13, 15, 15, 15, 16, 16 ],
 [ 12, 10,  8,  6,  5,  4,  4,  4,  4,  4,  4,  3,  3,  3,  4,  4,  5,  5,  7,  9, 11, 13, 14, 14 ],
 [ 13, 10,  9,  9,  7,  7,  5,  5,  4,  3,  3,  3,  3,  3,  4,  4,  4,  5,  7,  9, 11, 13, 13, 13 ],
 [ 12, 13, 10,  8,  6,  6,  5,  5,  4,  4,  3,  3,  3,  3,  3,  4,  5,  5,  6,  7,  9, 11, 14, 14 ],
 [ 12, 11,  9,  8,  8,  7,  5,  4,  4,  3,  3,  3,  3,  3,  4,  4,  5,  5,  7,  8, 10, 13, 14, 14 ],
 [ 15, 16, 15, 12, 10,  8,  6,  5,  4,  3,  3,  3,  2,  3,  4,  5,  5,  7,  9, 11, 13, 16, 16, 16 ],
 [ 14, 14, 11, 10,  9,  7,  7,  5,  5,  4,  3,  3,  2,  3,  3,  4,  5,  7,  9,  9, 12, 14, 15, 15 ],
 [  9,  9,  9,  8,  7,  6,  5,  4,  3,  3,  3,  3,  3,  3,  4,  5,  6,  7,  8, 10, 11, 12, 13, 13 ],
 [ 14, 12, 10,  8,  6,  6,  5,  4,  3,  3,  3,  3,  3,  3,  4,  5,  6,  8,  8,  9, 11, 14, 14, 14 ],
 [ 13, 10,  9,  8,  6,  6,  5,  4,  4,  4,  3,  3,  2,  3,  4,  5,  6,  8,  9,  9, 11, 12, 14, 14 ],
 [ 16, 13, 12, 11,  9,  6,  5,  5,  4,  4,  4,  3,  2,  3,  3,  4,  5,  7,  8, 10, 14, 16, 16, 16 ],
 [ 13, 14, 14, 14, 10,  8,  7,  7,  5,  4,  3,  3,  2,  3,  3,  4,  5,  5,  7,  9, 11, 14, 14, 14 ]
];

const VEC_CB0_BITS: [u16; 181] = [
    0x0000, 0x0001, 0x0001, 0x0010, 0x0044, 0x0114, 0x0115, 0x008B,
    0x0023, 0x0009, 0x0005, 0x0018, 0x0019, 0x000D, 0x0007, 0x0001,
    0x0010, 0x0044, 0x0114, 0x0115, 0x022C, 0x045A, 0x045B, 0x0117,
    0x0046, 0x0047, 0x0009, 0x0014, 0x0015, 0x0016, 0x002E, 0x005E,
    0x005F, 0x000C, 0x001A, 0x0036, 0x0037, 0x000E, 0x001E, 0x003E,
    0x00FC, 0x00FD, 0x01FC, 0x01FD, 0x01FE, 0x01FF, 0x0008, 0x0090,
    0x0091, 0x0049, 0x004A, 0x004B, 0x004C, 0x009A, 0x0136, 0x09B8,
    0x1372, 0x1373, 0x1374, 0x1375, 0x1376, 0x1377, 0x026F, 0x004E,
    0x004F, 0x000A, 0x0016, 0x0017, 0x0018, 0x0032, 0x0033, 0x00D0,
    0x01A2, 0x0346, 0x0347, 0x0069, 0x00D4, 0x00D5, 0x01AC, 0x06B4,
    0x06B5, 0x06B6, 0x1ADC, 0x1ADD, 0x0D6F, 0x00D7, 0x0036, 0x0037,
    0x000E, 0x0078, 0x0079, 0x003D, 0x003E, 0x003F, 0x0002, 0x0003,
    0x0020, 0x0420, 0x0421, 0x0422, 0x0423, 0x0212, 0x0426, 0x0427,
    0x0085, 0x010C, 0x010D, 0x010E, 0x010F, 0x0044, 0x0114, 0x022A,
    0x022B, 0x0116, 0x08B8, 0x08B9, 0x045D, 0x1178, 0x1179, 0x08BD,
    0x045F, 0x008C, 0x008D, 0x0047, 0x0012, 0x0098, 0x0099, 0x0134,
    0x0135, 0x0136, 0x0137, 0x004E, 0x0278, 0x0279, 0x013D, 0x013E,
    0x09F8, 0x27E4, 0x27E5, 0x13F3, 0x04FD, 0x027F, 0x000A, 0x0016,
    0x005C, 0x00BA, 0x00BB, 0x005E, 0x02F8, 0x02F9, 0x017D, 0x00BF,
    0x0018, 0x0032, 0x0033, 0x0034, 0x0350, 0x0351, 0x0352, 0x0353,
    0x00D5, 0x006B, 0x006C, 0x00DA, 0x00DB, 0x006E, 0x01BC, 0x01BD,
    0x00DF, 0x001C, 0x003A, 0x03B0, 0x0762, 0x0763, 0x01D9, 0x01DA,
    0x01DB, 0x00EE, 0x00EF, 0x000F, 0x0001
];
const VEC_CB0_LENS: [u8; 181] = [
     9,  9,  8, 11, 13, 15, 15, 14, 12, 10,  9, 11, 11, 10,  9,  6,
     9, 11, 13, 13, 14, 15, 15, 13, 11, 11,  8,  9,  9,  9, 10, 11,
    11,  8,  9, 10, 10,  8,  9, 10, 12, 12, 13, 13, 13, 13,  7, 11,
    11, 10, 10, 10, 10, 11, 12, 15, 16, 16, 16, 16, 16, 16, 13, 10,
    10,  7,  8,  8,  8,  9,  9, 11, 12, 13, 13, 10, 11, 11, 12, 14,
    14, 14, 16, 16, 15, 11,  9,  9,  7, 10, 10,  9,  9,  9,  4,  4,
     7, 12, 12, 12, 12, 11, 12, 12,  9, 10, 10, 10, 10,  8, 10, 11,
    11, 10, 13, 13, 12, 14, 14, 13, 12,  9,  9,  8,  6,  9,  9, 10,
    10, 10, 10,  8, 11, 11, 10, 10, 13, 15, 15, 14, 12, 11,  5,  6,
     8,  9,  9,  8, 11, 11, 10,  9,  6,  7,  7,  7, 11, 11, 11, 11,
     9,  8,  8,  9,  9,  8, 10, 10,  9,  6,  7, 11, 12, 12, 10, 10,
    10,  9,  9,  5,  1
];
const VEC_CB0_SYMS: [u16; 181] = [
     37,   51,    5,  102,   76,  139,  155,  169,  151,   41,   82,  103,
    118,  100,    8,   32,  113,  134,  211,  182,  213,  214,  124,  183,
     28,   29,   96,   52,   24,   67,  146,  193,  104,   35,  144,  176,
    115,   21,  129,   70,  210,   45,  123,  197,   92,   61,   49,   74,
    164,   56,   11,  177,   42,  192,  195,  109,  185,  156,  140,  216,
    171,  201,  212,   27,   57,   19,   81,   50,    6,  145,   38,   12,
     60,  122,   77,  131,  119,   59,  166,  153,  199,   93,  125,  141,
    215,   13,   98,   53,   64,  101,   85,    9,   25,  114,   16,    1,
      4,  137,  106,  150,  121,  149,   91,  167,   83,  116,  117,   71,
    178,  112,  162,  179,   44,  147,  152,  198,  181,  154,  170,  138,
    107,   10,   39,   97,   18,   40,   54,   86,   72,   88,   43,   22,
    209,   90,  163,  133,  108,  186,  200,  184,  196,  165,   17,   33,
      7,   26,   68,   36,  135,  208,   87,   99,   48,   20,   80,   34,
    105,  180,   75,  194,  161,   66,   23,  160,   84,  128,   58,  148,
     55,    3,   65,  120,  136,  168,  132,   73,   89,   69,  130,    2,    0
];

const VEC_CB1_BITS: [u16; 94] = [
    0x0000, 0x0010, 0x0044, 0x0045, 0x0023, 0x0009, 0x0005, 0x000C,
    0x000D, 0x000E, 0x001E, 0x001F, 0x0001, 0x0002, 0x000C, 0x0034,
    0x01A8, 0x0352, 0x0353, 0x00D5, 0x006B, 0x0036, 0x006E, 0x006F,
    0x01C0, 0x01C1, 0x00E1, 0x0071, 0x0072, 0x0073, 0x01D0, 0x03A2,
    0x03A3, 0x00E9, 0x0075, 0x003B, 0x0078, 0x00F2, 0x01E6, 0x01E7,
    0x003D, 0x001F, 0x0020, 0x0021, 0x0088, 0x0112, 0x0113, 0x0045,
    0x0023, 0x0024, 0x0094, 0x0095, 0x004B, 0x004C, 0x009A, 0x0136,
    0x04DC, 0x4DD0, 0x4DD1, 0x26E9, 0x1375, 0x09BB, 0x026F, 0x0027,
    0x000A, 0x000B, 0x0018, 0x0064, 0x00CA, 0x0196, 0x0197, 0x0066,
    0x0067, 0x001A, 0x001B, 0x000E, 0x0078, 0x0790, 0x0F22, 0x1E46,
    0x1E47, 0x03C9, 0x01E5, 0x00F3, 0x007A, 0x00F6, 0x03DC, 0x07BA,
    0x1EEC, 0x1EED, 0x0F77, 0x01EF, 0x001F, 0x0001
];
const VEC_CB1_LENS: [u8; 94] = [
     5,  9, 11, 11, 10,  8,  7,  8,  8,  8,  9,  9,  4,  4,  6,  8,
    11, 12, 12, 10,  9,  8,  9,  9, 11, 11, 10,  9,  9,  9, 11, 12,
    12, 10,  9,  8,  9, 10, 11, 11,  8,  7,  7,  7,  9, 10, 10,  8,
     7,  7,  9,  9,  8,  8,  9, 10, 12, 16, 16, 15, 14, 13, 11,  7,
     5,  5,  6,  8,  9, 10, 10,  8,  8,  6,  6,  5,  8, 12, 13, 14,
    14, 11, 10,  9,  8,  9, 11, 12, 14, 14, 13, 10,  6,  1
];
const VEC_CB1_SYMS: [u16; 94] = [
     17,   23,   86,  131,   85,   80,   49,   21,    5,   36,   83,  112,
      1,   16,   33,   51,   71,   87,   88,   70,   39,   81,   53,  113,
    145,  117,   55,   68,   98,    7,   41,  118,   57,  100,   84,   66,
     99,  115,  116,  132,   52,   64,   20,   65,   54,   25,  128,   37,
      4,   35,    8,   69,   22,    6,   24,  144,  147,  120,  105,  150,
    135,  148,  102,   50,    2,   32,    3,   82,  114,    9,   56,   97,
     67,   19,   48,   18,   96,  103,   73,  104,  149,  146,  130,   40,
     38,  129,   72,  133,  119,   89,  134,  101,   34,    0
];

const VEC_CB2_BITS: [u16; 48] = [
    0x0000, 0x0008, 0x0024, 0x004A, 0x004B, 0x0013, 0x000A, 0x000B,
    0x0018, 0x0019, 0x0034, 0x0035, 0x006C, 0x00DA, 0x01B6, 0x01B7,
    0x0037, 0x001C, 0x001D, 0x001E, 0x003E, 0x003F, 0x0002, 0x0018,
    0x0019, 0x000D, 0x0007, 0x0008, 0x0009, 0x000A, 0x0160, 0x0161,
    0x0162, 0x0163, 0x0059, 0x00B4, 0x2D40, 0x5A82, 0x5A83, 0x16A1,
    0x0B51, 0x05A9, 0x02D5, 0x016B, 0x005B, 0x0017, 0x0003, 0x0001
];
const VEC_CB2_LENS: [u8; 48] = [
     4,  7,  9, 10, 10,  8,  7,  7,  8,  8,  9,  9, 10, 11, 12, 12,
     9,  8,  8,  8,  9,  9,  4,  7,  7,  6,  5,  5,  5,  5, 10, 10,
    10, 10,  8,  9, 15, 16, 16, 14, 13, 12, 11, 10,  8,  6,  3,  1
];
const VEC_CB2_SYMS: [u16; 48] = [
     8,   25,   13,   42,   14,   12,    3,   24,   33,    4,   28,   41,
    36,   22,   44,   30,   35,   32,   27,   20,   40,   21,    9,   19,
    26,   18,    2,   10,   17,   16,   49,   29,   43,   50,   34,    6,
    45,   53,   46,   52,   38,   51,   37,   48,    5,   11,    1,    0
];

const VEC_CB3_BITS: [u16; 520] = [
    0x0000, 0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086,
    0x0087, 0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E,
    0x008F, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096,
    0x0097, 0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E,
    0x013E, 0x013F, 0x0005, 0x0003, 0x0002, 0x000C, 0x001A, 0x001B,
    0x0038, 0x0039, 0x003A, 0x003B, 0x000F, 0x0008, 0x0090, 0x0091,
    0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 0x0098, 0x0099,
    0x009A, 0x009B, 0x0027, 0x0005, 0x000C, 0x000D, 0x000E, 0x001E,
    0x001F, 0x0040, 0x0041, 0x0042, 0x0860, 0x0861, 0x0862, 0x0863,
    0x0864, 0x0865, 0x0866, 0x0867, 0x0868, 0x0869, 0x086A, 0x086B,
    0x086C, 0x086D, 0x086E, 0x086F, 0x0870, 0x0871, 0x0872, 0x0873,
    0x0874, 0x0875, 0x0876, 0x0877, 0x0878, 0x0879, 0x087A, 0x087B,
    0x087C, 0x087D, 0x087E, 0x087F, 0x0880, 0x0881, 0x0882, 0x0883,
    0x0884, 0x0885, 0x0886, 0x0887, 0x0888, 0x0889, 0x088A, 0x088B,
    0x088C, 0x088D, 0x088E, 0x088F, 0x0890, 0x0891, 0x0892, 0x0893,
    0x0894, 0x0895, 0x0896, 0x0897, 0x0898, 0x0899, 0x089A, 0x089B,
    0x089C, 0x089D, 0x089E, 0x089F, 0x08A0, 0x08A1, 0x08A2, 0x08A3,
    0x08A4, 0x08A5, 0x08A6, 0x08A7, 0x08A8, 0x08A9, 0x08AA, 0x08AB,
    0x08AC, 0x08AD, 0x08AE, 0x08AF, 0x08B0, 0x08B1, 0x08B2, 0x08B3,
    0x08B4, 0x08B5, 0x08B6, 0x08B7, 0x08B8, 0x08B9, 0x08BA, 0x08BB,
    0x08BC, 0x08BD, 0x08BE, 0x08BF, 0x08C0, 0x08C1, 0x08C2, 0x08C3,
    0x08C4, 0x08C5, 0x08C6, 0x08C7, 0x08C8, 0x08C9, 0x08CA, 0x08CB,
    0x08CC, 0x08CD, 0x08CE, 0x08CF, 0x08D0, 0x08D1, 0x08D2, 0x08D3,
    0x08D4, 0x08D5, 0x08D6, 0x08D7, 0x08D8, 0x08D9, 0x08DA, 0x08DB,
    0x08DC, 0x08DD, 0x08DE, 0x08DF, 0x08E0, 0x08E1, 0x08E2, 0x08E3,
    0x08E4, 0x08E5, 0x08E6, 0x08E7, 0x08E8, 0x08E9, 0x08EA, 0x08EB,
    0x08EC, 0x08ED, 0x08EE, 0x08EF, 0x08F0, 0x08F1, 0x08F2, 0x08F3,
    0x08F4, 0x08F5, 0x08F6, 0x08F7, 0x08F8, 0x08F9, 0x08FA, 0x08FB,
    0x08FC, 0x08FD, 0x08FE, 0x08FF, 0x0009, 0x0A00, 0x0A01, 0x0A02,
    0x0A03, 0x0A04, 0x0A05, 0x0A06, 0x0A07, 0x0A08, 0x0A09, 0x0A0A,
    0x0A0B, 0x0A0C, 0x0A0D, 0x0A0E, 0x0A0F, 0x0A10, 0x0A11, 0x0A12,
    0x0A13, 0x0A14, 0x0A15, 0x0A16, 0x0A17, 0x0A18, 0x0A19, 0x0A1A,
    0x0A1B, 0x0A1C, 0x0A1D, 0x0A1E, 0x0A1F, 0x0A20, 0x0A21, 0x0A22,
    0x0A23, 0x0A24, 0x0A25, 0x0A26, 0x0A27, 0x0A28, 0x0A29, 0x0A2A,
    0x0A2B, 0x0A2C, 0x0A2D, 0x0A2E, 0x0A2F, 0x00A3, 0x0029, 0x002A,
    0x0056, 0x0057, 0x000B, 0x0003, 0x0004, 0x0005, 0x0030, 0x0062,
    0x18C0, 0x18C1, 0x18C2, 0x18C3, 0x18C4, 0x18C5, 0x18C6, 0x18C7,
    0x0319, 0x018D, 0x00C7, 0x0032, 0x0198, 0x0199, 0x019A, 0x019B,
    0x0067, 0x001A, 0x0360, 0x0361, 0x0362, 0x0363, 0x0364, 0x0365,
    0x0366, 0x0367, 0x0368, 0x0369, 0x036A, 0x036B, 0x00DB, 0x006E,
    0x006F, 0x001C, 0x00E8, 0x00E9, 0x0075, 0x0076, 0x0077, 0x0780,
    0x0781, 0x0782, 0x0783, 0x0784, 0x0785, 0x0786, 0x0787, 0x0788,
    0x0789, 0x078A, 0x078B, 0x078C, 0x078D, 0x078E, 0x078F, 0x0079,
    0x003D, 0x007C, 0x07D0, 0x07D1, 0x07D2, 0x07D3, 0x01F5, 0x00FB,
    0x00FC, 0x00FD, 0x00FE, 0x01FE, 0x01FF, 0x0004, 0x0005, 0x000C,
    0x0068, 0x00D2, 0x00D3, 0x0035, 0x0036, 0x0037, 0x0038, 0x00E4,
    0x00E5, 0x0073, 0x003A, 0x0076, 0x01DC, 0x01DD, 0x00EF, 0x000F,
    0x0008, 0x0009, 0x0050, 0x0144, 0x0145, 0x0146, 0x028E, 0x051E,
    0x051F, 0x0052, 0x0530, 0x0531, 0x0532, 0x0533, 0x0534, 0x0535,
    0x0536, 0x0537, 0x014E, 0x0A78, 0x0A79, 0x0A7A, 0x0A7B, 0x0A7C,
    0x0A7D, 0x0A7E, 0x0A7F, 0x0015, 0x0016, 0x0017, 0x0006, 0x0070,
    0x00E2, 0x0E30, 0x0E31, 0x0E32, 0x0E33, 0x038D, 0x01C7, 0x0039,
    0x01D0, 0x03A2, 0x0E8C, 0x0E8D, 0x0747, 0x00E9, 0x0075, 0x003B,
    0x001E, 0x007C, 0x00FA, 0x03EC, 0x03ED, 0x03EE, 0x07DE, 0x07DF,
    0x07E0, 0x07E1, 0x07E2, 0x07E3, 0x01F9, 0x00FD, 0x007F, 0x0008,
    0x0090, 0x0091, 0x0092, 0x0093, 0x0128, 0x0129, 0x012A, 0x0958,
    0x9590, 0x9591, 0x4AC9, 0x2565, 0x12B3, 0x04AD, 0x0257, 0x004B,
    0x0098, 0x0264, 0x0265, 0x0266, 0x04CE, 0x04CF, 0x004D, 0x009C,
    0x013A, 0x0276, 0x0277, 0x004F, 0x00A0, 0x0284, 0x050A, 0x050B,
    0x050C, 0x0A1A, 0x0A1B, 0x0A1C, 0x0A1D, 0x0A1E, 0x0A1F, 0x00A2,
    0x0A30, 0x0A31, 0x0A32, 0x0A33, 0x028D, 0x028E, 0x1478, 0x1479,
    0x147A, 0x147B, 0x147C, 0x147D, 0x147E, 0x147F, 0x0029, 0x0015,
    0x0058, 0x0059, 0x00B4, 0x16A0, 0x16A1, 0x16A2, 0x16A3, 0x16A4,
    0x16A5, 0x16A6, 0x2D4E, 0x2D4F, 0x02D5, 0x016B, 0x02D8, 0x02D9,
    0x016D, 0x016E, 0x05BC, 0x05BD, 0x02DF, 0x002E, 0x002F, 0x0003
];
const VEC_CB3_LENS: [u8; 520] = [
     8, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    16, 16, 10,  9,  8, 10, 11, 11, 12, 12, 12, 12, 10,  9, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 11,  8,  9,  9,  9, 10,
    10, 11, 11, 11, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16,  8, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 12, 10, 10, 11, 11,  8,  6,  6,  6,  9, 10,
    16, 16, 16, 16, 16, 16, 16, 16, 13, 12, 11,  9, 12, 12, 12, 12,
    10,  8, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 11, 10,
    10,  8, 11, 11, 10, 10, 10, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 10,  9, 10, 14, 14, 14, 14, 12, 11,
    11, 11, 11, 12, 12,  5,  5,  6,  9, 10, 10,  8,  8,  8,  8, 10,
    10,  9,  8,  9, 11, 11, 10,  6,  5,  5,  8, 10, 10, 10, 11, 12,
    12,  8, 12, 12, 12, 12, 12, 12, 12, 12, 10, 13, 13, 13, 13, 13,
    13, 13, 13,  6,  6,  6,  4,  8,  9, 13, 13, 13, 13, 11, 10,  7,
    10, 11, 13, 13, 12,  9,  8,  7,  6,  8,  9, 11, 11, 11, 12, 12,
    12, 12, 12, 12, 10,  9,  8,  4,  8,  8,  8,  8,  9,  9,  9, 12,
    16, 16, 15, 14, 13, 11, 10,  7,  8, 10, 10, 10, 11, 11,  7,  8,
     9, 10, 10,  7,  8, 10, 11, 11, 11, 12, 12, 12, 12, 12, 12,  8,
    12, 12, 12, 12, 10, 10, 13, 13, 13, 13, 13, 13, 13, 13,  6,  5,
     7,  7,  8, 13, 13, 13, 13, 13, 13, 13, 14, 14, 10,  9, 10, 10,
     9,  9, 11, 11, 10,  6,  6,  2
];
const VEC_CB3_SYMS: [u16; 520] = [
      74, 1120, 1682, 1745, 1801,  274, 1050,  730,  546, 1058, 1729, 1539,
    2240, 1547, 1163, 1730, 2064, 1624,  723, 1235, 1115,   35, 2248, 1304,
     644,  196, 1625, 1036, 1548, 1185,  652, 1044, 2136, 1753, 1664,  129,
     640,   83,  665,  130, 1225, 1232,  602,   76,  587, 1161, 1049,  545,
     265, 1218, 2113,  202,   98,  131,  643, 1288,  524,  785,  720, 1096,
     704,  528,  657,  601,   67,  537,   33,  579, 1746, 2258, 1248,  786,
    1298, 1810, 2265, 2313, 1761, 1562, 2074, 2264, 1569, 2257,  225, 2138,
    1056, 2081, 2249, 1690, 2202,  736, 2186, 1242, 1754,  282,  794, 1306,
    1792, 1561, 2201, 1570, 2073,  281, 1122, 1634, 2178,  674, 1186, 1698,
     226,  738, 1250, 1240,  289,  800, 1738, 2051, 1632, 1121,  793, 1603,
    2115, 1633, 2250,  737, 1667, 2179,  195,  707, 1219, 1731, 2243,  259,
     771, 1283, 1795, 2304, 2144, 2305, 1249, 2059, 2192, 2072, 2200, 1611,
    2123, 1689,  288, 1696, 1675, 2187,  203,  792, 1227, 1739, 2251,  267,
     779, 1291, 1803, 1802, 2256, 2242, 1555, 2067, 1560,  258, 1688, 1619,
    2131,  770, 2145, 2114, 1683, 2195,  211, 1057, 2065, 1747,  275,  787,
    1299, 1817, 1697, 1051, 1563, 2075, 1794, 2050,  801, 1627, 2139, 1800,
     667, 1179, 1691,  219,  731, 1243, 1755,  283,  795, 2066,  547, 1059,
    1571, 1184,  611, 1123, 1635,  163,  675, 1187,  227,  739, 1568, 1760,
    1028, 1540, 2052, 2193, 1752, 1092, 1604, 2116, 1793, 2177, 1156, 1668,
     649, 1618,  708, 1220, 1732,  260,  772, 1808, 1816,  280, 2312, 2060,
    1305, 1241, 1297, 1612, 2124,  140, 2122, 1164, 1676,  204,  716, 1228,
    1740,  268,  780, 2241, 1809, 2194, 1556, 2137, 2058, 1108, 1620,  148,
     660, 1172, 1684,  212,  724, 1236,   28,  540, 1052, 1564,   92,  604,
    1116,  666, 1105, 1153, 1673, 1162,  522,  584,  513,   73,  712,  650,
    1628,  156,  668, 1180,  220,  732,   36,  548,  672,  777,   32,  656,
    1233, 1113, 1602, 1665,  523,  586,  609, 1538,  728, 1112, 1289,  257,
    1280, 1234,  216,  610, 1091, 2121, 1026, 1224, 1608, 1600,   96,  536,
     713, 1104,   26, 1296, 1282, 2130, 1626,  218,  162, 1744, 1155, 2129,
     715, 1043, 1281,  147, 1171,  224,   99, 1169,  144, 1672,  132, 1100,
    2185,  596,  264,  768,  664, 1168, 1106,  154,  651,   64,    8,  585,
     594,  201,   90, 1097,  593,   11, 1089, 2048, 1098,   89,    3,   75,
     208,   91, 1090,   65,  576,    9,  137,  642,  658,  515,   20, 1736,
    1728, 1025,  194,  784,  722,  538,   34, 1099,  769, 2176, 1544,  608,
    1666, 1546,  217,  160, 2184,  544, 1027,  577,  521,  520,    1, 1032,
     145, 2049, 1680,  659,  273,   97, 2112,   16,  138, 1217,  580,  588,
     729,    4,  514,   81,    2,  592,  192, 1042,  531,  595, 2120, 1610,
     714, 1170,  139, 1737, 1034,   88,  578,  512,   18, 1536, 1033,   66,
      24,  200,  530,   84,  100,  612, 2080,  266, 1177,  209, 1601,   80,
    1160,  146,  600,  152, 1035,  776,   17,   82,   25, 1609,  153,  648,
     529, 1216, 1545,   27,   68,  272, 1552,  161, 1176, 1048, 1617,  641,
     210, 1114, 1553, 1616, 1040,  705, 2057,  706, 2128, 1674, 1226, 1554,
    1178, 1107,   10,   72,  136,  128, 1152, 1681,  539,  603,  155,  516,
     673,  532,  778, 1290, 1537,   12,  721,  256, 1041,   19, 1154, 2056,
     193, 1024, 1088,    0
];

const VEC_CB4_BITS: [u16; 209] = [
    0x0000, 0x0004, 0x000A, 0x0016, 0x002E, 0x0178, 0x0179, 0x00BD,
    0x05F0, 0x05F1, 0x05F2, 0x05F3, 0x05F4, 0x05F5, 0x05F6, 0x05F7,
    0x05F8, 0x05F9, 0x05FA, 0x05FB, 0x05FC, 0x05FD, 0x05FE, 0x05FF,
    0x0018, 0x0019, 0x000D, 0x001C, 0x001D, 0x000F, 0x0001, 0x0002,
    0x0018, 0x0064, 0x0328, 0x0CA4, 0x0CA5, 0x0CA6, 0x0CA7, 0x0CA8,
    0x0CA9, 0x0655, 0x032B, 0x00CB, 0x0066, 0x0067, 0x000D, 0x000E,
    0x000F, 0x0001, 0x0010, 0x0440, 0x1104, 0x220A, 0x220B, 0x0883,
    0x0221, 0x0111, 0x0089, 0x0045, 0x0118, 0x0119, 0x0468, 0x08D2,
    0x08D3, 0x08D4, 0x08D5, 0x046B, 0x011B, 0x008E, 0x023C, 0x047A,
    0x08F6, 0x23DC, 0x23DD, 0x23DE, 0x23DF, 0x011F, 0x0009, 0x000A,
    0x0058, 0x02C8, 0x0592, 0x164C, 0x164D, 0x164E, 0x164F, 0x0B28,
    0x0B29, 0x0595, 0x0596, 0x0597, 0x02CC, 0x059A, 0x0B36, 0x0B37,
    0x0B38, 0x2CE4, 0x59CA, 0x59CB, 0x1673, 0x059D, 0x02CF, 0x00B4,
    0x00B5, 0x005B, 0x0017, 0x0018, 0x0019, 0x001A, 0x0036, 0x01B8,
    0x06E4, 0x1B94, 0x1B95, 0x0DCB, 0x0373, 0x06E8, 0x06E9, 0x0375,
    0x01BB, 0x006F, 0x001C, 0x0074, 0x00EA, 0x01D6, 0x0EB8, 0x1D72,
    0x75CC, 0x75CD, 0x75CE, 0x75CF, 0x75D0, 0x75D1, 0x75D2, 0x75D3,
    0x75D4, 0x75D5, 0x75D6, 0x75D7, 0x75D8, 0x75D9, 0x75DA, 0x75DB,
    0x75DC, 0x75DD, 0x75DE, 0x75DF, 0x75E0, 0x75E1, 0x75E2, 0x75E3,
    0x75E4, 0x75E5, 0x75E6, 0x75E7, 0x75E8, 0x75E9, 0x75EA, 0x75EB,
    0x75EC, 0x75ED, 0x75EE, 0x75EF, 0x75F0, 0x75F1, 0x75F2, 0x75F3,
    0x75F4, 0x75F5, 0x75F6, 0x75F7, 0x75F8, 0x75F9, 0x75FA, 0x75FB,
    0x75FC, 0x75FD, 0x75FE, 0x75FF, 0x00EC, 0x01DA, 0x76C0, 0x76C1,
    0x76C2, 0x76C3, 0x76C4, 0x76C5, 0x76C6, 0x76C7, 0x76C8, 0x76C9,
    0x76CA, 0x76CB, 0x76CC, 0x76CD, 0x76CE, 0x76CF, 0x76D0, 0x76D1,
    0x76D2, 0x76D3, 0x76D4, 0x76D5, 0x3B6B, 0x0EDB, 0x03B7, 0x0077, 0x000F
];
const VEC_CB4_LENS: [u8; 209] = [
     5,  7,  8,  9, 10, 13, 13, 12, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,  9,  9,  8,  9,  9,  8,  4,  4,
     7,  9, 12, 14, 14, 14, 14, 14, 14, 13, 12, 10,  9,  9,  6,  6,
     6,  2,  5, 11, 13, 14, 14, 12, 10,  9,  8,  7,  9,  9, 11, 12,
    12, 12, 12, 11,  9,  8, 10, 11, 12, 14, 14, 14, 14,  9,  4,  4,
     7, 10, 11, 13, 13, 13, 13, 12, 12, 11, 11, 11, 10, 11, 12, 12,
    12, 14, 15, 15, 13, 11, 10,  8,  8,  7,  5,  5,  5,  5,  6,  9,
    11, 13, 13, 12, 10, 11, 11, 10,  9,  7,  5,  7,  8,  9, 12, 13,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15,  8,  9, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 14, 12, 10,  7,  4
];
const VEC_CB4_SYMS: [u16; 209] = [
     80,  128,   96,  132,  160,  134,   92,   23,  180,  228,  232,  214,
    225,  166,   54,  138,  121,  131,  147,   99,  141,   75,   27,   91,
     37,   66,   24,  129,  145,   36,    1,   64,    6,   73,   34,   57,
     49,   42,  106,   14,   56,  130,   98,    7,   33,   97,   69,   81,
     85,    0,   20,   12,   76,   11,   44,  212,   40,   70,   22,  144,
     86,  149,  165,  196,  102,  137,  146,   52,   72,  148,  192,   28,
     19,  120,   30,  224,  169,   82,    4,   16,    8,  208,   48,   93,
    168,  193,   53,  152,   67,  161,  112,   29,   41,   74,   77,  150,
     71,   87,  114,  178,  213,  116,  164,   18,  101,   32,   65,   21,
     84,   17,    2,    3,  153,  113,   83,  117,  105,   38,   90,   13,
     10,  100,   68,    9,   89,  133,  197,  209,  181,   61,  245,  198,
    205,  125,  156,  220,  233,  172,  216,  230,  109,  118,  182,  217,
    176,  173,  202,  204,  108,  154,  218,  184,  185,  170,   58,  122,
    124,   78,  142,  201,   94,  158,   46,  110,  157,  200,  240,  195,
    244,   60,  210,  211,   35,  241,  163,   51,  115,  221,  229,  135,
     88,   26,  199,  177,   45,  151,  215,   39,  103,  167,   55,  119,
    140,  162,  139,  226,   50,  155,   43,  107,   15,   79,   31,   95,
    194,  136,  104,   25,    5
];

const VEC_CB5_BITS: [u16; 192] = [
    0x0000, 0x0004, 0x0005, 0x0060, 0x0184, 0x030A, 0x0616, 0x0617,
    0x030C, 0x061A, 0x061B, 0x0187, 0x0031, 0x0019, 0x001A, 0x001B,
    0x000E, 0x003C, 0x003D, 0x003E, 0x007E, 0x01FC, 0x03FA, 0x1FD8,
    0x1FD9, 0x0FED, 0x0FEE, 0x0FEF, 0x00FF, 0x0010, 0x0044, 0x008A,
    0x008B, 0x0023, 0x0009, 0x000A, 0x000B, 0x0060, 0x1840, 0x1841,
    0x1842, 0x1843, 0x0C22, 0x0C23, 0x0612, 0x184C, 0x309A, 0x309B,
    0x0C27, 0x030A, 0x0616, 0x0617, 0x0186, 0x1870, 0x1871, 0x0C39,
    0x0C3A, 0x0C3B, 0x061E, 0x0C3E, 0x0C3F, 0x0031, 0x0019, 0x001A,
    0x0036, 0x00DC, 0x0DD0, 0x0DD1, 0x06E9, 0x0375, 0x01BB, 0x01BC,
    0x0DE8, 0x37A4, 0x6F4A, 0x6F4B, 0x1BD3, 0x06F5, 0x06F6, 0x1BDC,
    0x1BDD, 0x0DEF, 0x0DF0, 0x0DF1, 0x6F90, 0x6F91, 0x6F92, 0x6F93,
    0x6F94, 0x6F95, 0x6F96, 0x6F97, 0x6F98, 0x6F99, 0x6F9A, 0x6F9B,
    0x6F9C, 0x6F9D, 0x6F9E, 0x6F9F, 0x6FA0, 0x6FA1, 0x6FA2, 0x6FA3,
    0x6FA4, 0x6FA5, 0x6FA6, 0x6FA7, 0x6FA8, 0x6FA9, 0x6FAA, 0x6FAB,
    0x6FAC, 0x6FAD, 0x6FAE, 0x6FAF, 0x6FB0, 0x6FB1, 0x6FB2, 0x6FB3,
    0x6FB4, 0x6FB5, 0x6FB6, 0x6FB7, 0x6FB8, 0x6FB9, 0x6FBA, 0x6FBB,
    0x6FBC, 0x6FBD, 0x6FBE, 0x6FBF, 0x6FC0, 0x6FC1, 0x6FC2, 0x6FC3,
    0x6FC4, 0x6FC5, 0x6FC6, 0x6FC7, 0x6FC8, 0x6FC9, 0x6FCA, 0x6FCB,
    0x6FCC, 0x6FCD, 0x6FCE, 0x6FCF, 0x6FD0, 0x6FD1, 0x6FD2, 0x6FD3,
    0x6FD4, 0x6FD5, 0x6FD6, 0x6FD7, 0x6FD8, 0x6FD9, 0x6FDA, 0x6FDB,
    0x6FDC, 0x6FDD, 0x37EF, 0x0DFC, 0x0DFD, 0x0DFE, 0x37FC, 0x37FD,
    0x37FE, 0x37FF, 0x001C, 0x003A, 0x0076, 0x0077, 0x00F0, 0x00F1,
    0x03C8, 0x0792, 0x1E4C, 0x1E4D, 0x1E4E, 0x1E4F, 0x1E50, 0x1E51,
    0x1E52, 0x1E53, 0x0F2A, 0x0F2B, 0x03CB, 0x00F3, 0x003D, 0x001F
];
const VEC_CB5_LENS: [u8; 192] = [
     2,  4,  4,  8, 10, 11, 12, 12, 11, 12, 12, 10,  7,  6,  6,  6,
     5,  7,  7,  7,  8, 10, 11, 14, 14, 13, 13, 13,  9,  5,  7,  8,
     8,  6,  4,  4,  4,  7, 13, 13, 13, 13, 12, 12, 11, 13, 14, 14,
    12, 10, 11, 11,  9, 13, 13, 12, 12, 12, 11, 12, 12,  6,  5,  5,
     6,  8, 12, 12, 11, 10,  9,  9, 12, 14, 15, 15, 13, 11, 11, 13,
    13, 12, 12, 12, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 14, 12, 12, 12, 14, 14, 14, 14,  5,  6,  7,  7,  8,  8,
    10, 11, 13, 13, 13, 13, 13, 13, 13, 13, 12, 12, 10,  8,  6,  5
];
const VEC_CB5_SYMS: [u16; 192] = [
      0,    1,  256,  277,   24,   25,  262,   88,  100,  132,   70,  384,
     81,   17,  272,   68,    5,  321,   69,  276,  337,  144,   72,   10,
    389,  289,  532,  517,   32,  320,  324,  325,    2,  260,    4,   64,
     16,  273,  352,  265,  264,  329,   97,  148,   66,   86,  533,  149,
     37,   36,  400,   22,  128,  280,  326,  516,  101,  292,  528,  577,
    592,   21,   20,   80,  336,  512,  133,  580,   33,   96,  576,    9,
    274,  593,  597,  581,  356,  513,  288,  338,  278,  328,  388,   73,
    405,  661,  344,  660,  549,  152,  417,  613,  165,  421,  392,  296,
    521,  645,  641,  585,  137,  393,  608,  416,  537,  656,  609,  601,
    153,  409,  420,  297,  105,  361,  360,  408,  514,  164,   40,  578,
    130,  386,  548,  544,  530,  584,  545,  594,  146,  402,  600,  290,
     98,  354,  644,  136,  518,  657,  520,  582,  134,  390,  104,  536,
    534,  161,  342,  598,  150,  406,   38,  294,  102,  358,  612,  266,
     74,  330,   26,  282,   90,  346,   41,  293,  322,   82,   34,  640,
    401,  160,  257,   84,  340,   85,  341,    6,  258,  129,  353,  529,
    404,  357,  281,   89,  345,  596,  385,  145,   18,    8,  261,   65
];

const VEC_CB6_BITS: [u16; 32] = [
    0x0000, 0x0004, 0x0005, 0x0003, 0x0001, 0x0002, 0x0003, 0x0001,
    0x0002, 0x0003, 0x0004, 0x0005, 0x0018, 0x00C8, 0x0192, 0x0326,
    0x0327, 0x0065, 0x0066, 0x0067, 0x0068, 0x0069, 0x01A8, 0x01A9,
    0x00D5, 0x006B, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F, 0x0001
];
const VEC_CB6_LENS: [u8; 32] = [
     7,  9,  9,  8,  6,  6,  6,  4,  4,  4,  4,  4,  6,  9, 10, 11,
    11,  8,  8,  8,  8,  8, 10, 10,  9,  8,  6,  6,  6,  6,  6,  1
];
const VEC_CB6_SYMS: [u16; 32] = [
     9,   19,   22,    7,    3,   24,    6,    1,   16,    2,    8,    4,
    12,   21,   27,   29,   31,   28,   14,   13,   11,   26,   30,   23,
    15,   25,    5,   10,   20,   17,   18,    0
];

const QUANT_LEVELS: [[f32; 14]; 7] = [
    [ 0.0, 0.392, 0.761, 1.120, 1.477, 1.832, 2.183, 2.541, 2.893, 3.245, 3.598, 3.942, 4.288, 4.724 ],
    [ 0.0, 0.544, 1.060, 1.563, 2.068, 2.571, 3.072, 3.562, 4.070, 4.620, 0.0,   0.0,   0.0,   0.0 ],
    [ 0.0, 0.746, 1.464, 2.180, 2.882, 3.584, 4.316, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0 ],
    [ 0.0, 1.006, 2.000, 2.993, 3.985, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0 ],
    [ 0.0, 1.321, 2.703, 3.983, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0 ],
    [ 0.0, 1.657, 3.491, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0 ],
    [ 0.0, 1.964, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0 ]
];

const CAT5_NOISE_FACTOR: [f32; REGION_SIZE + 1] = [
    0.70711, 0.6179,  0.5005,  0.3220,  0.17678, 0.17678, 0.17678, 0.17678, 0.17678, 0.17678,
    0.17678, 0.17678, 0.17678, 0.17678, 0.17678, 0.17678, 0.17678, 0.17678, 0.17678, 0.17678, 0.0
];
const CAT6_NOISE_FACTOR: [f32; REGION_SIZE + 1] = [
    0.70711, 0.5686, 0.3563, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
    0.25,    0.25,   0.25,   0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.0
];
