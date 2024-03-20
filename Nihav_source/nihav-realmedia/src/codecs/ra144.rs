use nihav_core::formats::*;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;

const NBLOCKS: usize = 4;
const BLOCKSIZE: usize = 40;
const LPC_ORDER: usize = 10;
const BUFFERSIZE: usize = 146;
const FRAME_SIZE: usize = 20;

struct RA144Decoder {
    chmap:  NAChannelMap,
    ainfo:  NAAudioInfo,
    info:   NACodecInfoRef,

    old_energy:     u16,
    lpc_data:       [[i32; LPC_ORDER]; 2],
    frame_no:       usize,
    lpc_refl_rms:   [u32; 2],
    audio:          [i16; BLOCKSIZE + LPC_ORDER],
    adapt_cb:       [i16; BUFFERSIZE + 2],
}

impl RA144Decoder {
    fn new() -> Self {
        RA144Decoder {
            chmap:      NAChannelMap::new(),
            ainfo:      NAAudioInfo::new(0, 1, SND_S16_FORMAT, NBLOCKS * BLOCKSIZE),
            info:       NACodecInfo::new_dummy(),

            old_energy:     0,
            lpc_data:       [[0; 10]; 2],
            frame_no:       0,
            lpc_refl_rms:   [0; 2],
            audio:          [0; BLOCKSIZE + LPC_ORDER],
            adapt_cb:       [0; BUFFERSIZE + 2],
        }
    }

    fn evaluate_coeffs(&mut self, lpc_refl: &[i32; LPC_ORDER]) {
        let mut tmp: [i32; LPC_ORDER] = [0; LPC_ORDER];

        for i in 0..LPC_ORDER {
            let src;
            let dst;
            if (i & 1) == 0 {
                src = &self.lpc_data[self.frame_no];
                dst = &mut tmp;
            } else {
                src = &tmp;
                dst = &mut self.lpc_data[self.frame_no];
            }
            dst[i] = lpc_refl[i] << 4;

            for j in 0..i {
                dst[j] = ((lpc_refl[i] * src[i - j - 1]) >> 12) + src[j];
            }
        }
        for i in 0..LPC_ORDER {
            self.lpc_data[self.frame_no][i] >>= 4;
        }
    }
    fn interpolate(&mut self, coeffs: &mut [i16; LPC_ORDER], scale1: i32, copyold: bool, energy: u16) -> u32 {
        let scale2 = (NBLOCKS as i32) - scale1;
        {
            let src1 = &self.lpc_data[self.frame_no];
            let src2 = &self.lpc_data[self.frame_no ^ 1];
            for i in 0..LPC_ORDER {
                coeffs[i] = ((scale1 * src1[i] + scale2 * src2[i]) >> 2) as i16;
            }
        }
        if let Some(rms) = eval_reflection(coeffs) {
            rescale_rms(rms, energy)
        } else {
            let src = if copyold { &self.lpc_data[self.frame_no ^ 1] } else { &self.lpc_data[self.frame_no] };
            for i in 0..LPC_ORDER {
                coeffs[i] = src[i] as i16;
            }
            rescale_rms(self.lpc_refl_rms[if copyold { 1 } else { 0 }], energy)
        }
    }
    fn process_subblock(&mut self, br: &mut BitReader, lpc_coeffs: &[i16; LPC_ORDER], rms: u32) -> DecoderResult<()> {
        let cba_idx = br.read(7)? as usize;
        let gain    = br.read(8)? as usize;
        let cb1_idx = br.read(7)? as usize;
        let cb2_idx = br.read(7)? as usize;

        let mut tmp_a: [i16; BLOCKSIZE] = [0; BLOCKSIZE];
        let mut m: [u32; 3] = [0; 3];
        if cba_idx != 0 {
            let len = cba_idx + BLOCKSIZE/2 - 1;
            let soff = BUFFERSIZE - len;
            for i in 0..len.min(BLOCKSIZE) {
                tmp_a[i] = self.adapt_cb[i + soff];
            }
            for i in len..BLOCKSIZE {
                tmp_a[i] = self.adapt_cb[i + soff - len];
            }
            m[0] = (calc_irms(&tmp_a) * rms) >> 12;
        } else {
            m[0] = 0;
        }
        m[1] = ((RA144_CODEBOOK1[cb1_idx] as u32) * rms) >> 8;
        m[2] = ((RA144_CODEBOOK2[cb2_idx] as u32) * rms) >> 8;
        for i in 0..3 {
            m[i] = (m[i] * (RA144_GAIN_VAL_TAB[gain][i] as u32)) >> RA144_GAIN_EXP_TAB[gain];
        }

        for i in 0..(BUFFERSIZE - BLOCKSIZE) {
            self.adapt_cb[i] = self.adapt_cb[i + BLOCKSIZE];
        }

        let doff = BUFFERSIZE - BLOCKSIZE;
        if m[0] != 0 {
            for i in 0..BLOCKSIZE {
                let sum =   (m[0] as i32) * (tmp_a[i] as i32)
                          + (m[1] as i32) * (RA144_VECTORS1[cb1_idx][i] as i32)
                          + (m[2] as i32) * (RA144_VECTORS2[cb2_idx][i] as i32);
                self.adapt_cb[i + doff] = (sum >> 12) as i16;
            }
        } else {
            for i in 0..BLOCKSIZE {
                let sum =   (m[1] as i32) * (RA144_VECTORS1[cb1_idx][i] as i32)
                          + (m[2] as i32) * (RA144_VECTORS2[cb2_idx][i] as i32);
                self.adapt_cb[i + doff] = (sum >> 12) as i16;
            }
        }

        for i in 0..LPC_ORDER {
            self.audio[i] = self.audio[i + BLOCKSIZE];
        }
        if celp_synth_filter(&mut self.audio, &self.adapt_cb, lpc_coeffs) {
            self.audio = [0; BLOCKSIZE + LPC_ORDER];
        }
        Ok(())
    }
}

const CHMAP_MONO: [NAChannelType; 1] = [NAChannelType::C];

fn celp_synth_filter(dst: &mut [i16], src: &[i16], filt: &[i16; LPC_ORDER]) -> bool { //1,0,0xfff
    for i in 0..BLOCKSIZE {
        let mut sum = -0xFFF;
        for j in 0..LPC_ORDER {
            sum += (filt[j] as i32) * (dst[LPC_ORDER + i - j - 1] as i32);
        }
        let sum1 = (-sum >> 12) + (src[BUFFERSIZE - BLOCKSIZE + i] as i32);
        if ((sum1 >> 16) != 0) && ((sum1 >> 16) != -1) { // overflow
            return true;
        }
        dst[LPC_ORDER + i] = sum1 as i16;
    }
    false
}

fn fixp_sqrt(mut val: u32) -> u32 {
    let mut shift = 2;
    while val >= (1 << 12) {
        val >>= 2;
        shift += 1;
    }
    (SQRT_TABLE[val as usize] as u32) << shift
}

fn calc_rms(coeffs: &[i32; LPC_ORDER]) -> u32 {
    let mut res = 1 << 16;
    let mut shift = 10;

    for i in 0..LPC_ORDER {
        res = ((((1 << 24) - ((coeffs[i] * coeffs[i]) as u32)) >> 12) * res) >> 12;
        if res == 0 { return 0; }
        while res < (1 << 14) {
            res <<= 2;
            shift += 1;
        }
    }
    fixp_sqrt(res) >> shift
}

fn calc_irms(coeffs: &[i16; BLOCKSIZE]) -> u32 {
    let mut sum = 0;
    for i in 0..BLOCKSIZE {
        sum += ((coeffs[i] as i32) * (coeffs[i] as i32)) as u32;
    }
    if sum != 0 {
        (1 << 29) / (fixp_sqrt(sum) >> 8)
    } else {
        0
    }
}

fn rescale_rms(rms: u32, energy: u16) -> u32 {
    (rms * (energy as u32)) >> 10
}

fn eval_reflection(coeffs: &[i16; LPC_ORDER]) -> Option<u32> {
    let mut tmp1: [i32; LPC_ORDER] = [0; LPC_ORDER];
    let mut tmp2: [i32; LPC_ORDER] = [0; LPC_ORDER];
    let mut tmp3: [i32; LPC_ORDER] = [0; LPC_ORDER];

    for i in 0..LPC_ORDER {
        tmp2[i] = coeffs[i] as i32;
    }
    tmp3[LPC_ORDER - 1] = tmp2[LPC_ORDER - 1];

    if (tmp2[LPC_ORDER - 1] as u32).wrapping_add(0x1000) > 0x1FFF {
        return None;
    }

    for i in (0..(LPC_ORDER - 1)).rev() {
        let src;
        let dst;
        if (i & 1) == 0 {
            src = &tmp2;
            dst = &mut tmp1;
        } else {
            src = &tmp1;
            dst = &mut tmp2;
        }
        let a = (1 << 12) - ((src[i + 1] * src[i + 1]) >> 12);
        let scale = if a != 0 { (1 << 24) / a } else { 1 << 24 };
        for j in 0..=i {
            let result = (src[j] - ((tmp3[i + 1] * src[i - j]) >> 12)).checked_mul(scale);
            if let Some(val) = result {
                dst[j] = val >> 12;
            } else {
                return None;
            }
        }
        if (dst[i] as u32).wrapping_add(0x1000) > 0x1FFF {
            return None;
        }
        tmp3[i] = dst[i];
    }

    Some(calc_rms(&tmp3))
}

fn clip_out(sample: i16) -> i16 {
    sample.max(-16384 >> 2).min(16383 >> 2)
}

impl NADecoder for RA144Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.chmap.add_channels(&CHMAP_MONO);
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(),
                                          1,
                                          SND_S16_FORMAT, NBLOCKS * BLOCKSIZE);
            self.info = info.replace_info(NACodecTypeInfo::Audio(self.ainfo));
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        validate!(info.get_properties().is_audio());
        let pktbuf = pkt.get_buffer();
        let nframes = pktbuf.len() / FRAME_SIZE;

        let duration = NBLOCKS * BLOCKSIZE * nframes;

        let abuf = alloc_audio_buffer(self.ainfo, duration, self.chmap.clone())?;
        let mut adata = abuf.get_abuf_i16().unwrap();
        let dst = adata.get_data_mut().unwrap();

        for (input, output) in pktbuf.chunks(FRAME_SIZE).zip(dst.chunks_mut(NBLOCKS * BLOCKSIZE)) {
            let mut br = BitReader::new(input, BitReaderMode::BE);

            let mut lpc_refl: [i32; LPC_ORDER] = [0; LPC_ORDER];
            for i in 0..LPC_ORDER {
                lpc_refl[i] = RA144_LPC_REFL_CODEBOOK[i][br.read(RA144_LPC_SIZES[i])? as usize] as i32;
            }

            self.evaluate_coeffs(&lpc_refl);
            self.lpc_refl_rms[0] = calc_rms(&lpc_refl);

            let energy = RA144_ENERGY_TAB[br.read(5)? as usize];

            let mut block_coeffs: [[i16; LPC_ORDER]; NBLOCKS] = [[0; LPC_ORDER]; NBLOCKS];
            let mut refl_rms: [u32; NBLOCKS] = [0; NBLOCKS];

            let old_energy = self.old_energy;
            let interp_energy = (fixp_sqrt((energy as u32) * (old_energy as u32)) >> 12) as u16;
            refl_rms[0] = self.interpolate(&mut block_coeffs[0], 1, true, old_energy);
            refl_rms[1] = self.interpolate(&mut block_coeffs[1], 2, energy <= old_energy, interp_energy);
            refl_rms[2] = self.interpolate(&mut block_coeffs[2], 3, false, energy);
            refl_rms[3] = rescale_rms(self.lpc_refl_rms[0], energy);
            for i in 0..LPC_ORDER { block_coeffs[3][i] = self.lpc_data[self.frame_no][i] as i16; }

            for (i, block) in output.chunks_mut(BLOCKSIZE).enumerate() {

                self.process_subblock(&mut br, &block_coeffs[i], refl_rms[i])?;
                for j in 0..BLOCKSIZE {
                    block[j] = clip_out(self.audio[j + LPC_ORDER]) << 2;
                }
            }

            self.old_energy = energy;
            self.lpc_refl_rms[1] = self.lpc_refl_rms[0];
            self.frame_no ^= 1;
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for RA144Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RA144Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::realmedia_register_all_decoders;
    use crate::realmedia_register_all_demuxers;
    #[test]
    fn test_ra144() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/real/AC-14_4/ra3_in_rm_file.rm
        let file = "assets/RV/ra3_in_rm_file.rm";
        test_decode_audio("realmedia", file, Some(5000), None/*Some("ra14.4")*/, &dmx_reg, &dec_reg);
    }
}

const RA144_LPC_SIZES: [u8; LPC_ORDER] = [ 6, 5, 5, 4, 4, 3, 3, 3, 3, 2 ];
const RA144_LPC_REFL_CODEBOOK: [&[i16]; LPC_ORDER] = [
    LPC_REFL_CB0, LPC_REFL_CB1, LPC_REFL_CB2, LPC_REFL_CB3, LPC_REFL_CB4,
    LPC_REFL_CB5, LPC_REFL_CB6, LPC_REFL_CB7, LPC_REFL_CB8, LPC_REFL_CB9,
];
const LPC_REFL_CB0: &[i16; 64] = &[
    -4041, -4018, -3998, -3977, -3954, -3930, -3906, -3879,
    -3852, -3825, -3795, -3764, -3731, -3699, -3666, -3631,
    -3594, -3555, -3513, -3468, -3420, -3372, -3321, -3268,
    -3212, -3153, -3090, -3021, -2944, -2863, -2772, -2676,
    -2565, -2445, -2328, -2202, -2072, -1941, -1808, -1660,
    -1508, -1348, -1185,  -994,  -798,  -600,  -374,  -110,
      152,   447,   720,   982,  1229,  1456,  1682,  1916,
     2130,  2353,  2595,  2853,  3118,  3363,  3588,  3814
];
const LPC_REFL_CB1: &[i16; 32] = &[
    -3091, -2386, -1871, -1425, -1021,  -649,  -316,   -20,
      267,   544,   810,  1065,  1305,  1534,  1756,  1970,
     2171,  2359,  2536,  2700,  2854,  2996,  3133,  3263,
     3386,  3499,  3603,  3701,  3789,  3870,  3947,  4020
];
const LPC_REFL_CB2: &[i16; 32] = &[
    -3525, -3295, -3081, -2890, -2696, -2511, -2328, -2149,
    -1979, -1817, -1658, -1498, -1341, -1188, -1032,  -876,
     -721,  -561,  -394,  -228,   -54,   119,   296,   484,
      683,   895,  1123,  1373,  1651,  1965,  2360,  2854
];
const LPC_REFL_CB3: &[i16; 16] = &[
    -1845, -1057,  -522,   -77,   301,   647,   975,  1285,
     1582,  1873,  2163,  2452,  2735,  3017,  3299,  3569
];
const LPC_REFL_CB4: &[i16; 16] = &[
    -2691, -2187, -1788, -1435, -1118,  -837,  -571,  -316,
      -59,   201,   470,   759,  1077,  1457,  1908,  2495
];
const LPC_REFL_CB5: &[i16; 8] = &[
    -1372,  -474,   133,   632,  1100,  1571,  2075,  2672
];
const LPC_REFL_CB6: &[i16; 8] = &[
    -2389, -1787, -1231,  -717,  -239,   234,   770,  1474
];
const LPC_REFL_CB7: &[i16; 8] = &[
    -1569,  -864,  -296,   200,   670,  1151,  1709,  2385
];
const LPC_REFL_CB8: &[i16; 8] = &[
    -2200, -1608, -1062,  -569,  -120,   338,   863,  1621
];
const LPC_REFL_CB9: &[i16; 4] = &[
     -617,   190,   802,  1483
];

const RA144_ENERGY_TAB: [u16; 32] = [
        0,    16,    20,    25,    32,    41,    51,    65,
       81,   103,   129,   163,   205,   259,   326,   410,
      516,   650,   819,  1031,  1298,  1634,  2057,  2590,
     3261,  4105,  5168,  6507,  8192, 10313, 12983, 16345
];

const SQRT_TABLE: [u16; 4096] = [
0x0000,0x0400,0x05a8,0x06ed,0x0800,0x08f1,0x09cc,0x0a95,
0x0b50,0x0c00,0x0ca6,0x0d44,0x0ddb,0x0e6c,0x0ef7,0x0f7d,
0x1000,0x107e,0x10f8,0x116f,0x11e3,0x1254,0x12c2,0x132e,
0x1398,0x1400,0x1465,0x14c8,0x152a,0x158a,0x15e8,0x1645,
0x16a0,0x16fa,0x1752,0x17aa,0x1800,0x1854,0x18a8,0x18fa,
0x194c,0x199c,0x19ec,0x1a3a,0x1a88,0x1ad5,0x1b21,0x1b6c,
0x1bb6,0x1c00,0x1c48,0x1c90,0x1cd8,0x1d1e,0x1d64,0x1daa,
0x1dee,0x1e33,0x1e76,0x1eb9,0x1efb,0x1f3d,0x1f7e,0x1fbf,
0x2000,0x203f,0x207f,0x20bd,0x20fc,0x2139,0x2177,0x21b4,
0x21f0,0x222d,0x2268,0x22a4,0x22df,0x2319,0x2353,0x238d,
0x23c6,0x2400,0x2438,0x2471,0x24a9,0x24e0,0x2518,0x254f,
0x2585,0x25bc,0x25f2,0x2628,0x265d,0x2693,0x26c8,0x26fc,
0x2731,0x2765,0x2799,0x27cc,0x2800,0x2833,0x2865,0x2898,
0x28ca,0x28fc,0x292e,0x2960,0x2991,0x29c2,0x29f3,0x2a24,
0x2a54,0x2a85,0x2ab5,0x2ae5,0x2b14,0x2b44,0x2b73,0x2ba2,
0x2bd1,0x2c00,0x2c2e,0x2c5c,0x2c8a,0x2cb8,0x2ce6,0x2d13,
0x2d41,0x2d6e,0x2d9b,0x2dc8,0x2df4,0x2e21,0x2e4d,0x2e79,
0x2ea5,0x2ed1,0x2efd,0x2f28,0x2f54,0x2f7f,0x2faa,0x2fd5,
0x3000,0x302a,0x3055,0x307f,0x30a9,0x30d3,0x30fd,0x3127,
0x3150,0x317a,0x31a3,0x31cc,0x31f5,0x321e,0x3247,0x3270,
0x3298,0x32c1,0x32e9,0x3311,0x3339,0x3361,0x3389,0x33b0,
0x33d8,0x3400,0x3427,0x344e,0x3475,0x349c,0x34c3,0x34ea,
0x3510,0x3537,0x355d,0x3584,0x35aa,0x35d0,0x35f6,0x361c,
0x3642,0x3667,0x368d,0x36b2,0x36d8,0x36fd,0x3722,0x3747,
0x376c,0x3791,0x37b6,0x37db,0x3800,0x3824,0x3848,0x386d,
0x3891,0x38b5,0x38d9,0x38fd,0x3921,0x3945,0x3969,0x398c,
0x39b0,0x39d3,0x39f7,0x3a1a,0x3a3d,0x3a60,0x3a83,0x3aa6,
0x3ac9,0x3aec,0x3b0f,0x3b31,0x3b54,0x3b76,0x3b99,0x3bbb,
0x3bdd,0x3c00,0x3c22,0x3c44,0x3c66,0x3c87,0x3ca9,0x3ccb,
0x3ced,0x3d0e,0x3d30,0x3d51,0x3d72,0x3d94,0x3db5,0x3dd6,
0x3df7,0x3e18,0x3e39,0x3e5a,0x3e7b,0x3e9c,0x3ebc,0x3edd,
0x3efd,0x3f1e,0x3f3e,0x3f5f,0x3f7f,0x3f9f,0x3fbf,0x3fdf,
0x4000,0x401f,0x403f,0x405f,0x407f,0x409f,0x40be,0x40de,
0x40fe,0x411d,0x413c,0x415c,0x417b,0x419a,0x41ba,0x41d9,
0x41f8,0x4217,0x4236,0x4255,0x4273,0x4292,0x42b1,0x42d0,
0x42ee,0x430d,0x432b,0x434a,0x4368,0x4387,0x43a5,0x43c3,
0x43e1,0x4400,0x441e,0x443c,0x445a,0x4478,0x4495,0x44b3,
0x44d1,0x44ef,0x450c,0x452a,0x4548,0x4565,0x4583,0x45a0,
0x45be,0x45db,0x45f8,0x4615,0x4633,0x4650,0x466d,0x468a,
0x46a7,0x46c4,0x46e1,0x46fe,0x471b,0x4737,0x4754,0x4771,
0x478d,0x47aa,0x47c7,0x47e3,0x4800,0x481c,0x4838,0x4855,
0x4871,0x488d,0x48a9,0x48c6,0x48e2,0x48fe,0x491a,0x4936,
0x4952,0x496e,0x498a,0x49a5,0x49c1,0x49dd,0x49f9,0x4a14,
0x4a30,0x4a4b,0x4a67,0x4a83,0x4a9e,0x4ab9,0x4ad5,0x4af0,
0x4b0b,0x4b27,0x4b42,0x4b5d,0x4b78,0x4b93,0x4bae,0x4bca,
0x4be5,0x4c00,0x4c1a,0x4c35,0x4c50,0x4c6b,0x4c86,0x4ca1,
0x4cbb,0x4cd6,0x4cf1,0x4d0b,0x4d26,0x4d40,0x4d5b,0x4d75,
0x4d90,0x4daa,0x4dc4,0x4ddf,0x4df9,0x4e13,0x4e2d,0x4e48,
0x4e62,0x4e7c,0x4e96,0x4eb0,0x4eca,0x4ee4,0x4efe,0x4f18,
0x4f32,0x4f4c,0x4f65,0x4f7f,0x4f99,0x4fb3,0x4fcc,0x4fe6,
0x5000,0x5019,0x5033,0x504c,0x5066,0x507f,0x5099,0x50b2,
0x50cb,0x50e5,0x50fe,0x5117,0x5130,0x514a,0x5163,0x517c,
0x5195,0x51ae,0x51c7,0x51e0,0x51f9,0x5212,0x522b,0x5244,
0x525d,0x5276,0x528f,0x52a7,0x52c0,0x52d9,0x52f2,0x530a,
0x5323,0x533c,0x5354,0x536d,0x5385,0x539e,0x53b6,0x53cf,
0x53e7,0x5400,0x5418,0x5430,0x5449,0x5461,0x5479,0x5491,
0x54a9,0x54c2,0x54da,0x54f2,0x550a,0x5522,0x553a,0x5552,
0x556a,0x5582,0x559a,0x55b2,0x55ca,0x55e2,0x55fa,0x5611,
0x5629,0x5641,0x5659,0x5670,0x5688,0x56a0,0x56b7,0x56cf,
0x56e6,0x56fe,0x5716,0x572d,0x5745,0x575c,0x5773,0x578b,
0x57a2,0x57ba,0x57d1,0x57e8,0x5800,0x5817,0x582e,0x5845,
0x585c,0x5874,0x588b,0x58a2,0x58b9,0x58d0,0x58e7,0x58fe,
0x5915,0x592c,0x5943,0x595a,0x5971,0x5988,0x599f,0x59b5,
0x59cc,0x59e3,0x59fa,0x5a11,0x5a27,0x5a3e,0x5a55,0x5a6b,
0x5a82,0x5a99,0x5aaf,0x5ac6,0x5adc,0x5af3,0x5b09,0x5b20,
0x5b36,0x5b4d,0x5b63,0x5b7a,0x5b90,0x5ba6,0x5bbd,0x5bd3,
0x5be9,0x5c00,0x5c16,0x5c2c,0x5c42,0x5c58,0x5c6f,0x5c85,
0x5c9b,0x5cb1,0x5cc7,0x5cdd,0x5cf3,0x5d09,0x5d1f,0x5d35,
0x5d4b,0x5d61,0x5d77,0x5d8d,0x5da3,0x5db9,0x5dce,0x5de4,
0x5dfa,0x5e10,0x5e26,0x5e3b,0x5e51,0x5e67,0x5e7c,0x5e92,
0x5ea8,0x5ebd,0x5ed3,0x5ee9,0x5efe,0x5f14,0x5f29,0x5f3f,
0x5f54,0x5f6a,0x5f7f,0x5f95,0x5faa,0x5fbf,0x5fd5,0x5fea,
0x6000,0x6015,0x602a,0x603f,0x6055,0x606a,0x607f,0x6094,
0x60aa,0x60bf,0x60d4,0x60e9,0x60fe,0x6113,0x6128,0x613d,
0x6152,0x6168,0x617d,0x6192,0x61a7,0x61bb,0x61d0,0x61e5,
0x61fa,0x620f,0x6224,0x6239,0x624e,0x6263,0x6277,0x628c,
0x62a1,0x62b6,0x62ca,0x62df,0x62f4,0x6309,0x631d,0x6332,
0x6347,0x635b,0x6370,0x6384,0x6399,0x63ad,0x63c2,0x63d7,
0x63eb,0x6400,0x6414,0x6428,0x643d,0x6451,0x6466,0x647a,
0x648e,0x64a3,0x64b7,0x64cb,0x64e0,0x64f4,0x6508,0x651d,
0x6531,0x6545,0x6559,0x656e,0x6582,0x6596,0x65aa,0x65be,
0x65d2,0x65e6,0x65fa,0x660f,0x6623,0x6637,0x664b,0x665f,
0x6673,0x6687,0x669b,0x66af,0x66c3,0x66d6,0x66ea,0x66fe,
0x6712,0x6726,0x673a,0x674e,0x6761,0x6775,0x6789,0x679d,
0x67b1,0x67c4,0x67d8,0x67ec,0x6800,0x6813,0x6827,0x683b,
0x684e,0x6862,0x6875,0x6889,0x689d,0x68b0,0x68c4,0x68d7,
0x68eb,0x68fe,0x6912,0x6925,0x6939,0x694c,0x6960,0x6973,
0x6986,0x699a,0x69ad,0x69c1,0x69d4,0x69e7,0x69fb,0x6a0e,
0x6a21,0x6a35,0x6a48,0x6a5b,0x6a6e,0x6a82,0x6a95,0x6aa8,
0x6abb,0x6ace,0x6ae2,0x6af5,0x6b08,0x6b1b,0x6b2e,0x6b41,
0x6b54,0x6b67,0x6b7a,0x6b8d,0x6ba1,0x6bb4,0x6bc7,0x6bda,
0x6bed,0x6c00,0x6c12,0x6c25,0x6c38,0x6c4b,0x6c5e,0x6c71,
0x6c84,0x6c97,0x6caa,0x6cbc,0x6ccf,0x6ce2,0x6cf5,0x6d08,
0x6d1a,0x6d2d,0x6d40,0x6d53,0x6d65,0x6d78,0x6d8b,0x6d9e,
0x6db0,0x6dc3,0x6dd6,0x6de8,0x6dfb,0x6e0d,0x6e20,0x6e33,
0x6e45,0x6e58,0x6e6a,0x6e7d,0x6e8f,0x6ea2,0x6eb4,0x6ec7,
0x6ed9,0x6eec,0x6efe,0x6f11,0x6f23,0x6f36,0x6f48,0x6f5a,
0x6f6d,0x6f7f,0x6f92,0x6fa4,0x6fb6,0x6fc9,0x6fdb,0x6fed,
0x7000,0x7012,0x7024,0x7036,0x7049,0x705b,0x706d,0x707f,
0x7091,0x70a4,0x70b6,0x70c8,0x70da,0x70ec,0x70fe,0x7110,
0x7123,0x7135,0x7147,0x7159,0x716b,0x717d,0x718f,0x71a1,
0x71b3,0x71c5,0x71d7,0x71e9,0x71fb,0x720d,0x721f,0x7231,
0x7243,0x7255,0x7267,0x7279,0x728a,0x729c,0x72ae,0x72c0,
0x72d2,0x72e4,0x72f5,0x7307,0x7319,0x732b,0x733d,0x734e,
0x7360,0x7372,0x7384,0x7395,0x73a7,0x73b9,0x73ca,0x73dc,
0x73ee,0x7400,0x7411,0x7423,0x7434,0x7446,0x7458,0x7469,
0x747b,0x748c,0x749e,0x74b0,0x74c1,0x74d3,0x74e4,0x74f6,
0x7507,0x7519,0x752a,0x753c,0x754d,0x755f,0x7570,0x7581,
0x7593,0x75a4,0x75b6,0x75c7,0x75d8,0x75ea,0x75fb,0x760d,
0x761e,0x762f,0x7641,0x7652,0x7663,0x7674,0x7686,0x7697,
0x76a8,0x76ba,0x76cb,0x76dc,0x76ed,0x76fe,0x7710,0x7721,
0x7732,0x7743,0x7754,0x7766,0x7777,0x7788,0x7799,0x77aa,
0x77bb,0x77cc,0x77dd,0x77ee,0x7800,0x7811,0x7822,0x7833,
0x7844,0x7855,0x7866,0x7877,0x7888,0x7899,0x78aa,0x78bb,
0x78cc,0x78dd,0x78ee,0x78fe,0x790f,0x7920,0x7931,0x7942,
0x7953,0x7964,0x7975,0x7986,0x7996,0x79a7,0x79b8,0x79c9,
0x79da,0x79eb,0x79fb,0x7a0c,0x7a1d,0x7a2e,0x7a3e,0x7a4f,
0x7a60,0x7a71,0x7a81,0x7a92,0x7aa3,0x7ab3,0x7ac4,0x7ad5,
0x7ae5,0x7af6,0x7b07,0x7b17,0x7b28,0x7b39,0x7b49,0x7b5a,
0x7b6b,0x7b7b,0x7b8c,0x7b9c,0x7bad,0x7bbd,0x7bce,0x7bde,
0x7bef,0x7c00,0x7c10,0x7c21,0x7c31,0x7c41,0x7c52,0x7c62,
0x7c73,0x7c83,0x7c94,0x7ca4,0x7cb5,0x7cc5,0x7cd5,0x7ce6,
0x7cf6,0x7d07,0x7d17,0x7d27,0x7d38,0x7d48,0x7d58,0x7d69,
0x7d79,0x7d89,0x7d9a,0x7daa,0x7dba,0x7dcb,0x7ddb,0x7deb,
0x7dfb,0x7e0c,0x7e1c,0x7e2c,0x7e3c,0x7e4d,0x7e5d,0x7e6d,
0x7e7d,0x7e8d,0x7e9e,0x7eae,0x7ebe,0x7ece,0x7ede,0x7eee,
0x7efe,0x7f0f,0x7f1f,0x7f2f,0x7f3f,0x7f4f,0x7f5f,0x7f6f,
0x7f7f,0x7f8f,0x7f9f,0x7faf,0x7fbf,0x7fcf,0x7fdf,0x7fef,
0x8000,0x800f,0x801f,0x802f,0x803f,0x804f,0x805f,0x806f,
0x807f,0x808f,0x809f,0x80af,0x80bf,0x80cf,0x80df,0x80ef,
0x80ff,0x810e,0x811e,0x812e,0x813e,0x814e,0x815e,0x816d,
0x817d,0x818d,0x819d,0x81ad,0x81bc,0x81cc,0x81dc,0x81ec,
0x81fc,0x820b,0x821b,0x822b,0x823b,0x824a,0x825a,0x826a,
0x8279,0x8289,0x8299,0x82a8,0x82b8,0x82c8,0x82d7,0x82e7,
0x82f7,0x8306,0x8316,0x8326,0x8335,0x8345,0x8354,0x8364,
0x8374,0x8383,0x8393,0x83a2,0x83b2,0x83c1,0x83d1,0x83e0,
0x83f0,0x8400,0x840f,0x841f,0x842e,0x843e,0x844d,0x845c,
0x846c,0x847b,0x848b,0x849a,0x84aa,0x84b9,0x84c9,0x84d8,
0x84e7,0x84f7,0x8506,0x8516,0x8525,0x8534,0x8544,0x8553,
0x8562,0x8572,0x8581,0x8591,0x85a0,0x85af,0x85be,0x85ce,
0x85dd,0x85ec,0x85fc,0x860b,0x861a,0x862a,0x8639,0x8648,
0x8657,0x8667,0x8676,0x8685,0x8694,0x86a3,0x86b3,0x86c2,
0x86d1,0x86e0,0x86ef,0x86ff,0x870e,0x871d,0x872c,0x873b,
0x874a,0x8759,0x8769,0x8778,0x8787,0x8796,0x87a5,0x87b4,
0x87c3,0x87d2,0x87e1,0x87f0,0x8800,0x880f,0x881e,0x882d,
0x883c,0x884b,0x885a,0x8869,0x8878,0x8887,0x8896,0x88a5,
0x88b4,0x88c3,0x88d2,0x88e1,0x88f0,0x88ff,0x890e,0x891c,
0x892b,0x893a,0x8949,0x8958,0x8967,0x8976,0x8985,0x8994,
0x89a3,0x89b2,0x89c0,0x89cf,0x89de,0x89ed,0x89fc,0x8a0b,
0x8a19,0x8a28,0x8a37,0x8a46,0x8a55,0x8a64,0x8a72,0x8a81,
0x8a90,0x8a9f,0x8aad,0x8abc,0x8acb,0x8ada,0x8ae8,0x8af7,
0x8b06,0x8b15,0x8b23,0x8b32,0x8b41,0x8b50,0x8b5e,0x8b6d,
0x8b7c,0x8b8a,0x8b99,0x8ba8,0x8bb6,0x8bc5,0x8bd4,0x8be2,
0x8bf1,0x8c00,0x8c0e,0x8c1d,0x8c2b,0x8c3a,0x8c49,0x8c57,
0x8c66,0x8c74,0x8c83,0x8c91,0x8ca0,0x8caf,0x8cbd,0x8ccc,
0x8cda,0x8ce9,0x8cf7,0x8d06,0x8d14,0x8d23,0x8d31,0x8d40,
0x8d4e,0x8d5d,0x8d6b,0x8d7a,0x8d88,0x8d97,0x8da5,0x8db4,
0x8dc2,0x8dd1,0x8ddf,0x8ded,0x8dfc,0x8e0a,0x8e19,0x8e27,
0x8e36,0x8e44,0x8e52,0x8e61,0x8e6f,0x8e7d,0x8e8c,0x8e9a,
0x8ea9,0x8eb7,0x8ec5,0x8ed4,0x8ee2,0x8ef0,0x8eff,0x8f0d,
0x8f1b,0x8f2a,0x8f38,0x8f46,0x8f54,0x8f63,0x8f71,0x8f7f,
0x8f8e,0x8f9c,0x8faa,0x8fb8,0x8fc7,0x8fd5,0x8fe3,0x8ff1,
0x9000,0x900e,0x901c,0x902a,0x9038,0x9047,0x9055,0x9063,
0x9071,0x907f,0x908d,0x909c,0x90aa,0x90b8,0x90c6,0x90d4,
0x90e2,0x90f0,0x90ff,0x910d,0x911b,0x9129,0x9137,0x9145,
0x9153,0x9161,0x916f,0x917e,0x918c,0x919a,0x91a8,0x91b6,
0x91c4,0x91d2,0x91e0,0x91ee,0x91fc,0x920a,0x9218,0x9226,
0x9234,0x9242,0x9250,0x925e,0x926c,0x927a,0x9288,0x9296,
0x92a4,0x92b2,0x92c0,0x92ce,0x92dc,0x92ea,0x92f8,0x9306,
0x9314,0x9321,0x932f,0x933d,0x934b,0x9359,0x9367,0x9375,
0x9383,0x9391,0x939f,0x93ac,0x93ba,0x93c8,0x93d6,0x93e4,
0x93f2,0x9400,0x940d,0x941b,0x9429,0x9437,0x9445,0x9452,
0x9460,0x946e,0x947c,0x948a,0x9497,0x94a5,0x94b3,0x94c1,
0x94cf,0x94dc,0x94ea,0x94f8,0x9506,0x9513,0x9521,0x952f,
0x953c,0x954a,0x9558,0x9566,0x9573,0x9581,0x958f,0x959c,
0x95aa,0x95b8,0x95c5,0x95d3,0x95e1,0x95ee,0x95fc,0x960a,
0x9617,0x9625,0x9633,0x9640,0x964e,0x965c,0x9669,0x9677,
0x9684,0x9692,0x96a0,0x96ad,0x96bb,0x96c8,0x96d6,0x96e4,
0x96f1,0x96ff,0x970c,0x971a,0x9727,0x9735,0x9742,0x9750,
0x975d,0x976b,0x9779,0x9786,0x9794,0x97a1,0x97af,0x97bc,
0x97ca,0x97d7,0x97e5,0x97f2,0x9800,0x980d,0x981a,0x9828,
0x9835,0x9843,0x9850,0x985e,0x986b,0x9879,0x9886,0x9893,
0x98a1,0x98ae,0x98bc,0x98c9,0x98d6,0x98e4,0x98f1,0x98ff,
0x990c,0x9919,0x9927,0x9934,0x9942,0x994f,0x995c,0x996a,
0x9977,0x9984,0x9992,0x999f,0x99ac,0x99ba,0x99c7,0x99d4,
0x99e2,0x99ef,0x99fc,0x9a09,0x9a17,0x9a24,0x9a31,0x9a3f,
0x9a4c,0x9a59,0x9a66,0x9a74,0x9a81,0x9a8e,0x9a9b,0x9aa9,
0x9ab6,0x9ac3,0x9ad0,0x9ade,0x9aeb,0x9af8,0x9b05,0x9b12,
0x9b20,0x9b2d,0x9b3a,0x9b47,0x9b54,0x9b62,0x9b6f,0x9b7c,
0x9b89,0x9b96,0x9ba3,0x9bb1,0x9bbe,0x9bcb,0x9bd8,0x9be5,
0x9bf2,0x9c00,0x9c0d,0x9c1a,0x9c27,0x9c34,0x9c41,0x9c4e,
0x9c5b,0x9c68,0x9c75,0x9c83,0x9c90,0x9c9d,0x9caa,0x9cb7,
0x9cc4,0x9cd1,0x9cde,0x9ceb,0x9cf8,0x9d05,0x9d12,0x9d1f,
0x9d2c,0x9d39,0x9d46,0x9d53,0x9d60,0x9d6d,0x9d7a,0x9d87,
0x9d94,0x9da1,0x9dae,0x9dbb,0x9dc8,0x9dd5,0x9de2,0x9def,
0x9dfc,0x9e09,0x9e16,0x9e23,0x9e30,0x9e3d,0x9e4a,0x9e57,
0x9e64,0x9e71,0x9e7e,0x9e8b,0x9e98,0x9ea4,0x9eb1,0x9ebe,
0x9ecb,0x9ed8,0x9ee5,0x9ef2,0x9eff,0x9f0c,0x9f18,0x9f25,
0x9f32,0x9f3f,0x9f4c,0x9f59,0x9f66,0x9f72,0x9f7f,0x9f8c,
0x9f99,0x9fa6,0x9fb3,0x9fbf,0x9fcc,0x9fd9,0x9fe6,0x9ff3,
0xa000,0xa00c,0xa019,0xa026,0xa033,0xa03f,0xa04c,0xa059,
0xa066,0xa073,0xa07f,0xa08c,0xa099,0xa0a6,0xa0b2,0xa0bf,
0xa0cc,0xa0d9,0xa0e5,0xa0f2,0xa0ff,0xa10b,0xa118,0xa125,
0xa132,0xa13e,0xa14b,0xa158,0xa164,0xa171,0xa17e,0xa18a,
0xa197,0xa1a4,0xa1b0,0xa1bd,0xa1ca,0xa1d6,0xa1e3,0xa1f0,
0xa1fc,0xa209,0xa216,0xa222,0xa22f,0xa23c,0xa248,0xa255,
0xa261,0xa26e,0xa27b,0xa287,0xa294,0xa2a0,0xa2ad,0xa2ba,
0xa2c6,0xa2d3,0xa2df,0xa2ec,0xa2f8,0xa305,0xa312,0xa31e,
0xa32b,0xa337,0xa344,0xa350,0xa35d,0xa369,0xa376,0xa382,
0xa38f,0xa39b,0xa3a8,0xa3b5,0xa3c1,0xa3ce,0xa3da,0xa3e7,
0xa3f3,0xa400,0xa40c,0xa418,0xa425,0xa431,0xa43e,0xa44a,
0xa457,0xa463,0xa470,0xa47c,0xa489,0xa495,0xa4a2,0xa4ae,
0xa4ba,0xa4c7,0xa4d3,0xa4e0,0xa4ec,0xa4f9,0xa505,0xa511,
0xa51e,0xa52a,0xa537,0xa543,0xa54f,0xa55c,0xa568,0xa574,
0xa581,0xa58d,0xa59a,0xa5a6,0xa5b2,0xa5bf,0xa5cb,0xa5d7,
0xa5e4,0xa5f0,0xa5fc,0xa609,0xa615,0xa621,0xa62e,0xa63a,
0xa646,0xa653,0xa65f,0xa66b,0xa678,0xa684,0xa690,0xa69d,
0xa6a9,0xa6b5,0xa6c1,0xa6ce,0xa6da,0xa6e6,0xa6f2,0xa6ff,
0xa70b,0xa717,0xa724,0xa730,0xa73c,0xa748,0xa754,0xa761,
0xa76d,0xa779,0xa785,0xa792,0xa79e,0xa7aa,0xa7b6,0xa7c3,
0xa7cf,0xa7db,0xa7e7,0xa7f3,0xa800,0xa80c,0xa818,0xa824,
0xa830,0xa83c,0xa849,0xa855,0xa861,0xa86d,0xa879,0xa885,
0xa892,0xa89e,0xa8aa,0xa8b6,0xa8c2,0xa8ce,0xa8da,0xa8e6,
0xa8f3,0xa8ff,0xa90b,0xa917,0xa923,0xa92f,0xa93b,0xa947,
0xa953,0xa960,0xa96c,0xa978,0xa984,0xa990,0xa99c,0xa9a8,
0xa9b4,0xa9c0,0xa9cc,0xa9d8,0xa9e4,0xa9f0,0xa9fc,0xaa09,
0xaa15,0xaa21,0xaa2d,0xaa39,0xaa45,0xaa51,0xaa5d,0xaa69,
0xaa75,0xaa81,0xaa8d,0xaa99,0xaaa5,0xaab1,0xaabd,0xaac9,
0xaad5,0xaae1,0xaaed,0xaaf9,0xab05,0xab11,0xab1d,0xab29,
0xab35,0xab41,0xab4d,0xab58,0xab64,0xab70,0xab7c,0xab88,
0xab94,0xaba0,0xabac,0xabb8,0xabc4,0xabd0,0xabdc,0xabe8,
0xabf4,0xac00,0xac0b,0xac17,0xac23,0xac2f,0xac3b,0xac47,
0xac53,0xac5f,0xac6b,0xac76,0xac82,0xac8e,0xac9a,0xaca6,
0xacb2,0xacbe,0xacc9,0xacd5,0xace1,0xaced,0xacf9,0xad05,
0xad11,0xad1c,0xad28,0xad34,0xad40,0xad4c,0xad57,0xad63,
0xad6f,0xad7b,0xad87,0xad92,0xad9e,0xadaa,0xadb6,0xadc2,
0xadcd,0xadd9,0xade5,0xadf1,0xadfd,0xae08,0xae14,0xae20,
0xae2c,0xae37,0xae43,0xae4f,0xae5b,0xae66,0xae72,0xae7e,
0xae8a,0xae95,0xaea1,0xaead,0xaeb8,0xaec4,0xaed0,0xaedc,
0xaee7,0xaef3,0xaeff,0xaf0a,0xaf16,0xaf22,0xaf2e,0xaf39,
0xaf45,0xaf51,0xaf5c,0xaf68,0xaf74,0xaf7f,0xaf8b,0xaf97,
0xafa2,0xafae,0xafba,0xafc5,0xafd1,0xafdd,0xafe8,0xaff4,
0xb000,0xb00b,0xb017,0xb022,0xb02e,0xb03a,0xb045,0xb051,
0xb05c,0xb068,0xb074,0xb07f,0xb08b,0xb097,0xb0a2,0xb0ae,
0xb0b9,0xb0c5,0xb0d0,0xb0dc,0xb0e8,0xb0f3,0xb0ff,0xb10a,
0xb116,0xb121,0xb12d,0xb139,0xb144,0xb150,0xb15b,0xb167,
0xb172,0xb17e,0xb189,0xb195,0xb1a0,0xb1ac,0xb1b8,0xb1c3,
0xb1cf,0xb1da,0xb1e6,0xb1f1,0xb1fd,0xb208,0xb214,0xb21f,
0xb22b,0xb236,0xb242,0xb24d,0xb259,0xb264,0xb270,0xb27b,
0xb286,0xb292,0xb29d,0xb2a9,0xb2b4,0xb2c0,0xb2cb,0xb2d7,
0xb2e2,0xb2ee,0xb2f9,0xb305,0xb310,0xb31b,0xb327,0xb332,
0xb33e,0xb349,0xb355,0xb360,0xb36b,0xb377,0xb382,0xb38e,
0xb399,0xb3a4,0xb3b0,0xb3bb,0xb3c7,0xb3d2,0xb3dd,0xb3e9,
0xb3f4,0xb400,0xb40b,0xb416,0xb422,0xb42d,0xb438,0xb444,
0xb44f,0xb45a,0xb466,0xb471,0xb47c,0xb488,0xb493,0xb49f,
0xb4aa,0xb4b5,0xb4c1,0xb4cc,0xb4d7,0xb4e2,0xb4ee,0xb4f9,
0xb504,0xb510,0xb51b,0xb526,0xb532,0xb53d,0xb548,0xb554,
0xb55f,0xb56a,0xb575,0xb581,0xb58c,0xb597,0xb5a3,0xb5ae,
0xb5b9,0xb5c4,0xb5d0,0xb5db,0xb5e6,0xb5f1,0xb5fd,0xb608,
0xb613,0xb61e,0xb62a,0xb635,0xb640,0xb64b,0xb657,0xb662,
0xb66d,0xb678,0xb684,0xb68f,0xb69a,0xb6a5,0xb6b0,0xb6bc,
0xb6c7,0xb6d2,0xb6dd,0xb6e8,0xb6f4,0xb6ff,0xb70a,0xb715,
0xb720,0xb72c,0xb737,0xb742,0xb74d,0xb758,0xb763,0xb76f,
0xb77a,0xb785,0xb790,0xb79b,0xb7a6,0xb7b2,0xb7bd,0xb7c8,
0xb7d3,0xb7de,0xb7e9,0xb7f4,0xb800,0xb80b,0xb816,0xb821,
0xb82c,0xb837,0xb842,0xb84d,0xb858,0xb864,0xb86f,0xb87a,
0xb885,0xb890,0xb89b,0xb8a6,0xb8b1,0xb8bc,0xb8c7,0xb8d3,
0xb8de,0xb8e9,0xb8f4,0xb8ff,0xb90a,0xb915,0xb920,0xb92b,
0xb936,0xb941,0xb94c,0xb957,0xb962,0xb96d,0xb978,0xb983,
0xb98f,0xb99a,0xb9a5,0xb9b0,0xb9bb,0xb9c6,0xb9d1,0xb9dc,
0xb9e7,0xb9f2,0xb9fd,0xba08,0xba13,0xba1e,0xba29,0xba34,
0xba3f,0xba4a,0xba55,0xba60,0xba6b,0xba76,0xba81,0xba8c,
0xba97,0xbaa2,0xbaad,0xbab8,0xbac3,0xbace,0xbad8,0xbae3,
0xbaee,0xbaf9,0xbb04,0xbb0f,0xbb1a,0xbb25,0xbb30,0xbb3b,
0xbb46,0xbb51,0xbb5c,0xbb67,0xbb72,0xbb7d,0xbb88,0xbb92,
0xbb9d,0xbba8,0xbbb3,0xbbbe,0xbbc9,0xbbd4,0xbbdf,0xbbea,
0xbbf5,0xbc00,0xbc0a,0xbc15,0xbc20,0xbc2b,0xbc36,0xbc41,
0xbc4c,0xbc57,0xbc61,0xbc6c,0xbc77,0xbc82,0xbc8d,0xbc98,
0xbca3,0xbcad,0xbcb8,0xbcc3,0xbcce,0xbcd9,0xbce4,0xbcef,
0xbcf9,0xbd04,0xbd0f,0xbd1a,0xbd25,0xbd30,0xbd3a,0xbd45,
0xbd50,0xbd5b,0xbd66,0xbd70,0xbd7b,0xbd86,0xbd91,0xbd9c,
0xbda6,0xbdb1,0xbdbc,0xbdc7,0xbdd2,0xbddc,0xbde7,0xbdf2,
0xbdfd,0xbe08,0xbe12,0xbe1d,0xbe28,0xbe33,0xbe3d,0xbe48,
0xbe53,0xbe5e,0xbe68,0xbe73,0xbe7e,0xbe89,0xbe93,0xbe9e,
0xbea9,0xbeb4,0xbebe,0xbec9,0xbed4,0xbedf,0xbee9,0xbef4,
0xbeff,0xbf0a,0xbf14,0xbf1f,0xbf2a,0xbf34,0xbf3f,0xbf4a,
0xbf55,0xbf5f,0xbf6a,0xbf75,0xbf7f,0xbf8a,0xbf95,0xbf9f,
0xbfaa,0xbfb5,0xbfbf,0xbfca,0xbfd5,0xbfdf,0xbfea,0xbff5,
0xc000,0xc00a,0xc015,0xc01f,0xc02a,0xc035,0xc03f,0xc04a,
0xc055,0xc05f,0xc06a,0xc075,0xc07f,0xc08a,0xc095,0xc09f,
0xc0aa,0xc0b5,0xc0bf,0xc0ca,0xc0d4,0xc0df,0xc0ea,0xc0f4,
0xc0ff,0xc109,0xc114,0xc11f,0xc129,0xc134,0xc13e,0xc149,
0xc154,0xc15e,0xc169,0xc173,0xc17e,0xc189,0xc193,0xc19e,
0xc1a8,0xc1b3,0xc1bd,0xc1c8,0xc1d3,0xc1dd,0xc1e8,0xc1f2,
0xc1fd,0xc207,0xc212,0xc21d,0xc227,0xc232,0xc23c,0xc247,
0xc251,0xc25c,0xc266,0xc271,0xc27b,0xc286,0xc290,0xc29b,
0xc2a5,0xc2b0,0xc2bb,0xc2c5,0xc2d0,0xc2da,0xc2e5,0xc2ef,
0xc2fa,0xc304,0xc30f,0xc319,0xc324,0xc32e,0xc339,0xc343,
0xc34e,0xc358,0xc363,0xc36d,0xc377,0xc382,0xc38c,0xc397,
0xc3a1,0xc3ac,0xc3b6,0xc3c1,0xc3cb,0xc3d6,0xc3e0,0xc3eb,
0xc3f5,0xc400,0xc40a,0xc414,0xc41f,0xc429,0xc434,0xc43e,
0xc449,0xc453,0xc45d,0xc468,0xc472,0xc47d,0xc487,0xc492,
0xc49c,0xc4a6,0xc4b1,0xc4bb,0xc4c6,0xc4d0,0xc4da,0xc4e5,
0xc4ef,0xc4fa,0xc504,0xc50e,0xc519,0xc523,0xc52e,0xc538,
0xc542,0xc54d,0xc557,0xc562,0xc56c,0xc576,0xc581,0xc58b,
0xc595,0xc5a0,0xc5aa,0xc5b4,0xc5bf,0xc5c9,0xc5d4,0xc5de,
0xc5e8,0xc5f3,0xc5fd,0xc607,0xc612,0xc61c,0xc626,0xc631,
0xc63b,0xc645,0xc650,0xc65a,0xc664,0xc66f,0xc679,0xc683,
0xc68e,0xc698,0xc6a2,0xc6ac,0xc6b7,0xc6c1,0xc6cb,0xc6d6,
0xc6e0,0xc6ea,0xc6f5,0xc6ff,0xc709,0xc713,0xc71e,0xc728,
0xc732,0xc73d,0xc747,0xc751,0xc75b,0xc766,0xc770,0xc77a,
0xc784,0xc78f,0xc799,0xc7a3,0xc7ae,0xc7b8,0xc7c2,0xc7cc,
0xc7d7,0xc7e1,0xc7eb,0xc7f5,0xc800,0xc80a,0xc814,0xc81e,
0xc828,0xc833,0xc83d,0xc847,0xc851,0xc85c,0xc866,0xc870,
0xc87a,0xc884,0xc88f,0xc899,0xc8a3,0xc8ad,0xc8b7,0xc8c2,
0xc8cc,0xc8d6,0xc8e0,0xc8ea,0xc8f5,0xc8ff,0xc909,0xc913,
0xc91d,0xc928,0xc932,0xc93c,0xc946,0xc950,0xc95a,0xc965,
0xc96f,0xc979,0xc983,0xc98d,0xc997,0xc9a2,0xc9ac,0xc9b6,
0xc9c0,0xc9ca,0xc9d4,0xc9df,0xc9e9,0xc9f3,0xc9fd,0xca07,
0xca11,0xca1b,0xca26,0xca30,0xca3a,0xca44,0xca4e,0xca58,
0xca62,0xca6c,0xca76,0xca81,0xca8b,0xca95,0xca9f,0xcaa9,
0xcab3,0xcabd,0xcac7,0xcad1,0xcadc,0xcae6,0xcaf0,0xcafa,
0xcb04,0xcb0e,0xcb18,0xcb22,0xcb2c,0xcb36,0xcb40,0xcb4a,
0xcb55,0xcb5f,0xcb69,0xcb73,0xcb7d,0xcb87,0xcb91,0xcb9b,
0xcba5,0xcbaf,0xcbb9,0xcbc3,0xcbcd,0xcbd7,0xcbe1,0xcbeb,
0xcbf5,0xcc00,0xcc0a,0xcc14,0xcc1e,0xcc28,0xcc32,0xcc3c,
0xcc46,0xcc50,0xcc5a,0xcc64,0xcc6e,0xcc78,0xcc82,0xcc8c,
0xcc96,0xcca0,0xccaa,0xccb4,0xccbe,0xccc8,0xccd2,0xccdc,
0xcce6,0xccf0,0xccfa,0xcd04,0xcd0e,0xcd18,0xcd22,0xcd2c,
0xcd36,0xcd40,0xcd4a,0xcd54,0xcd5e,0xcd68,0xcd72,0xcd7c,
0xcd86,0xcd90,0xcd99,0xcda3,0xcdad,0xcdb7,0xcdc1,0xcdcb,
0xcdd5,0xcddf,0xcde9,0xcdf3,0xcdfd,0xce07,0xce11,0xce1b,
0xce25,0xce2f,0xce39,0xce43,0xce4c,0xce56,0xce60,0xce6a,
0xce74,0xce7e,0xce88,0xce92,0xce9c,0xcea6,0xceb0,0xceba,
0xcec3,0xcecd,0xced7,0xcee1,0xceeb,0xcef5,0xceff,0xcf09,
0xcf13,0xcf1d,0xcf26,0xcf30,0xcf3a,0xcf44,0xcf4e,0xcf58,
0xcf62,0xcf6c,0xcf75,0xcf7f,0xcf89,0xcf93,0xcf9d,0xcfa7,
0xcfb1,0xcfbb,0xcfc4,0xcfce,0xcfd8,0xcfe2,0xcfec,0xcff6,
0xd000,0xd009,0xd013,0xd01d,0xd027,0xd031,0xd03b,0xd044,
0xd04e,0xd058,0xd062,0xd06c,0xd076,0xd07f,0xd089,0xd093,
0xd09d,0xd0a7,0xd0b0,0xd0ba,0xd0c4,0xd0ce,0xd0d8,0xd0e1,
0xd0eb,0xd0f5,0xd0ff,0xd109,0xd112,0xd11c,0xd126,0xd130,
0xd13a,0xd143,0xd14d,0xd157,0xd161,0xd16b,0xd174,0xd17e,
0xd188,0xd192,0xd19b,0xd1a5,0xd1af,0xd1b9,0xd1c3,0xd1cc,
0xd1d6,0xd1e0,0xd1ea,0xd1f3,0xd1fd,0xd207,0xd211,0xd21a,
0xd224,0xd22e,0xd238,0xd241,0xd24b,0xd255,0xd25f,0xd268,
0xd272,0xd27c,0xd285,0xd28f,0xd299,0xd2a3,0xd2ac,0xd2b6,
0xd2c0,0xd2c9,0xd2d3,0xd2dd,0xd2e7,0xd2f0,0xd2fa,0xd304,
0xd30d,0xd317,0xd321,0xd32b,0xd334,0xd33e,0xd348,0xd351,
0xd35b,0xd365,0xd36e,0xd378,0xd382,0xd38b,0xd395,0xd39f,
0xd3a8,0xd3b2,0xd3bc,0xd3c6,0xd3cf,0xd3d9,0xd3e3,0xd3ec,
0xd3f6,0xd400,0xd409,0xd413,0xd41c,0xd426,0xd430,0xd439,
0xd443,0xd44d,0xd456,0xd460,0xd46a,0xd473,0xd47d,0xd487,
0xd490,0xd49a,0xd4a3,0xd4ad,0xd4b7,0xd4c0,0xd4ca,0xd4d4,
0xd4dd,0xd4e7,0xd4f0,0xd4fa,0xd504,0xd50d,0xd517,0xd521,
0xd52a,0xd534,0xd53d,0xd547,0xd551,0xd55a,0xd564,0xd56d,
0xd577,0xd581,0xd58a,0xd594,0xd59d,0xd5a7,0xd5b0,0xd5ba,
0xd5c4,0xd5cd,0xd5d7,0xd5e0,0xd5ea,0xd5f4,0xd5fd,0xd607,
0xd610,0xd61a,0xd623,0xd62d,0xd637,0xd640,0xd64a,0xd653,
0xd65d,0xd666,0xd670,0xd679,0xd683,0xd68c,0xd696,0xd6a0,
0xd6a9,0xd6b3,0xd6bc,0xd6c6,0xd6cf,0xd6d9,0xd6e2,0xd6ec,
0xd6f5,0xd6ff,0xd708,0xd712,0xd71b,0xd725,0xd72f,0xd738,
0xd742,0xd74b,0xd755,0xd75e,0xd768,0xd771,0xd77b,0xd784,
0xd78e,0xd797,0xd7a1,0xd7aa,0xd7b4,0xd7bd,0xd7c7,0xd7d0,
0xd7da,0xd7e3,0xd7ed,0xd7f6,0xd800,0xd809,0xd812,0xd81c,
0xd825,0xd82f,0xd838,0xd842,0xd84b,0xd855,0xd85e,0xd868,
0xd871,0xd87b,0xd884,0xd88e,0xd897,0xd8a0,0xd8aa,0xd8b3,
0xd8bd,0xd8c6,0xd8d0,0xd8d9,0xd8e3,0xd8ec,0xd8f5,0xd8ff,
0xd908,0xd912,0xd91b,0xd925,0xd92e,0xd938,0xd941,0xd94a,
0xd954,0xd95d,0xd967,0xd970,0xd979,0xd983,0xd98c,0xd996,
0xd99f,0xd9a9,0xd9b2,0xd9bb,0xd9c5,0xd9ce,0xd9d8,0xd9e1,
0xd9ea,0xd9f4,0xd9fd,0xda07,0xda10,0xda19,0xda23,0xda2c,
0xda35,0xda3f,0xda48,0xda52,0xda5b,0xda64,0xda6e,0xda77,
0xda81,0xda8a,0xda93,0xda9d,0xdaa6,0xdaaf,0xdab9,0xdac2,
0xdacb,0xdad5,0xdade,0xdae8,0xdaf1,0xdafa,0xdb04,0xdb0d,
0xdb16,0xdb20,0xdb29,0xdb32,0xdb3c,0xdb45,0xdb4e,0xdb58,
0xdb61,0xdb6a,0xdb74,0xdb7d,0xdb86,0xdb90,0xdb99,0xdba2,
0xdbac,0xdbb5,0xdbbe,0xdbc8,0xdbd1,0xdbda,0xdbe4,0xdbed,
0xdbf6,0xdc00,0xdc09,0xdc12,0xdc1b,0xdc25,0xdc2e,0xdc37,
0xdc41,0xdc4a,0xdc53,0xdc5d,0xdc66,0xdc6f,0xdc78,0xdc82,
0xdc8b,0xdc94,0xdc9e,0xdca7,0xdcb0,0xdcb9,0xdcc3,0xdccc,
0xdcd5,0xdcde,0xdce8,0xdcf1,0xdcfa,0xdd04,0xdd0d,0xdd16,
0xdd1f,0xdd29,0xdd32,0xdd3b,0xdd44,0xdd4e,0xdd57,0xdd60,
0xdd69,0xdd73,0xdd7c,0xdd85,0xdd8e,0xdd98,0xdda1,0xddaa,
0xddb3,0xddbd,0xddc6,0xddcf,0xddd8,0xdde2,0xddeb,0xddf4,
0xddfd,0xde06,0xde10,0xde19,0xde22,0xde2b,0xde35,0xde3e,
0xde47,0xde50,0xde59,0xde63,0xde6c,0xde75,0xde7e,0xde87,
0xde91,0xde9a,0xdea3,0xdeac,0xdeb5,0xdebf,0xdec8,0xded1,
0xdeda,0xdee3,0xdeed,0xdef6,0xdeff,0xdf08,0xdf11,0xdf1a,
0xdf24,0xdf2d,0xdf36,0xdf3f,0xdf48,0xdf52,0xdf5b,0xdf64,
0xdf6d,0xdf76,0xdf7f,0xdf89,0xdf92,0xdf9b,0xdfa4,0xdfad,
0xdfb6,0xdfbf,0xdfc9,0xdfd2,0xdfdb,0xdfe4,0xdfed,0xdff6,
0xe000,0xe009,0xe012,0xe01b,0xe024,0xe02d,0xe036,0xe03f,
0xe049,0xe052,0xe05b,0xe064,0xe06d,0xe076,0xe07f,0xe088,
0xe092,0xe09b,0xe0a4,0xe0ad,0xe0b6,0xe0bf,0xe0c8,0xe0d1,
0xe0db,0xe0e4,0xe0ed,0xe0f6,0xe0ff,0xe108,0xe111,0xe11a,
0xe123,0xe12c,0xe136,0xe13f,0xe148,0xe151,0xe15a,0xe163,
0xe16c,0xe175,0xe17e,0xe187,0xe190,0xe199,0xe1a3,0xe1ac,
0xe1b5,0xe1be,0xe1c7,0xe1d0,0xe1d9,0xe1e2,0xe1eb,0xe1f4,
0xe1fd,0xe206,0xe20f,0xe218,0xe221,0xe22b,0xe234,0xe23d,
0xe246,0xe24f,0xe258,0xe261,0xe26a,0xe273,0xe27c,0xe285,
0xe28e,0xe297,0xe2a0,0xe2a9,0xe2b2,0xe2bb,0xe2c4,0xe2cd,
0xe2d6,0xe2df,0xe2e8,0xe2f1,0xe2fa,0xe303,0xe30c,0xe315,
0xe31f,0xe328,0xe331,0xe33a,0xe343,0xe34c,0xe355,0xe35e,
0xe367,0xe370,0xe379,0xe382,0xe38b,0xe394,0xe39d,0xe3a6,
0xe3af,0xe3b8,0xe3c1,0xe3ca,0xe3d3,0xe3dc,0xe3e5,0xe3ee,
0xe3f7,0xe400,0xe408,0xe411,0xe41a,0xe423,0xe42c,0xe435,
0xe43e,0xe447,0xe450,0xe459,0xe462,0xe46b,0xe474,0xe47d,
0xe486,0xe48f,0xe498,0xe4a1,0xe4aa,0xe4b3,0xe4bc,0xe4c5,
0xe4ce,0xe4d7,0xe4e0,0xe4e9,0xe4f2,0xe4fa,0xe503,0xe50c,
0xe515,0xe51e,0xe527,0xe530,0xe539,0xe542,0xe54b,0xe554,
0xe55d,0xe566,0xe56f,0xe578,0xe580,0xe589,0xe592,0xe59b,
0xe5a4,0xe5ad,0xe5b6,0xe5bf,0xe5c8,0xe5d1,0xe5da,0xe5e3,
0xe5eb,0xe5f4,0xe5fd,0xe606,0xe60f,0xe618,0xe621,0xe62a,
0xe633,0xe63c,0xe644,0xe64d,0xe656,0xe65f,0xe668,0xe671,
0xe67a,0xe683,0xe68c,0xe694,0xe69d,0xe6a6,0xe6af,0xe6b8,
0xe6c1,0xe6ca,0xe6d3,0xe6db,0xe6e4,0xe6ed,0xe6f6,0xe6ff,
0xe708,0xe711,0xe71a,0xe722,0xe72b,0xe734,0xe73d,0xe746,
0xe74f,0xe758,0xe760,0xe769,0xe772,0xe77b,0xe784,0xe78d,
0xe795,0xe79e,0xe7a7,0xe7b0,0xe7b9,0xe7c2,0xe7cb,0xe7d3,
0xe7dc,0xe7e5,0xe7ee,0xe7f7,0xe800,0xe808,0xe811,0xe81a,
0xe823,0xe82c,0xe834,0xe83d,0xe846,0xe84f,0xe858,0xe861,
0xe869,0xe872,0xe87b,0xe884,0xe88d,0xe895,0xe89e,0xe8a7,
0xe8b0,0xe8b9,0xe8c1,0xe8ca,0xe8d3,0xe8dc,0xe8e5,0xe8ed,
0xe8f6,0xe8ff,0xe908,0xe911,0xe919,0xe922,0xe92b,0xe934,
0xe93c,0xe945,0xe94e,0xe957,0xe960,0xe968,0xe971,0xe97a,
0xe983,0xe98b,0xe994,0xe99d,0xe9a6,0xe9ae,0xe9b7,0xe9c0,
0xe9c9,0xe9d2,0xe9da,0xe9e3,0xe9ec,0xe9f5,0xe9fd,0xea06,
0xea0f,0xea18,0xea20,0xea29,0xea32,0xea3b,0xea43,0xea4c,
0xea55,0xea5e,0xea66,0xea6f,0xea78,0xea80,0xea89,0xea92,
0xea9b,0xeaa3,0xeaac,0xeab5,0xeabe,0xeac6,0xeacf,0xead8,
0xeae0,0xeae9,0xeaf2,0xeafb,0xeb03,0xeb0c,0xeb15,0xeb1d,
0xeb26,0xeb2f,0xeb38,0xeb40,0xeb49,0xeb52,0xeb5a,0xeb63,
0xeb6c,0xeb74,0xeb7d,0xeb86,0xeb8f,0xeb97,0xeba0,0xeba9,
0xebb1,0xebba,0xebc3,0xebcb,0xebd4,0xebdd,0xebe5,0xebee,
0xebf7,0xec00,0xec08,0xec11,0xec1a,0xec22,0xec2b,0xec34,
0xec3c,0xec45,0xec4e,0xec56,0xec5f,0xec68,0xec70,0xec79,
0xec82,0xec8a,0xec93,0xec9c,0xeca4,0xecad,0xecb5,0xecbe,
0xecc7,0xeccf,0xecd8,0xece1,0xece9,0xecf2,0xecfb,0xed03,
0xed0c,0xed15,0xed1d,0xed26,0xed2e,0xed37,0xed40,0xed48,
0xed51,0xed5a,0xed62,0xed6b,0xed74,0xed7c,0xed85,0xed8d,
0xed96,0xed9f,0xeda7,0xedb0,0xedb8,0xedc1,0xedca,0xedd2,
0xeddb,0xede4,0xedec,0xedf5,0xedfd,0xee06,0xee0f,0xee17,
0xee20,0xee28,0xee31,0xee3a,0xee42,0xee4b,0xee53,0xee5c,
0xee65,0xee6d,0xee76,0xee7e,0xee87,0xee8f,0xee98,0xeea1,
0xeea9,0xeeb2,0xeeba,0xeec3,0xeecc,0xeed4,0xeedd,0xeee5,
0xeeee,0xeef6,0xeeff,0xef08,0xef10,0xef19,0xef21,0xef2a,
0xef32,0xef3b,0xef43,0xef4c,0xef55,0xef5d,0xef66,0xef6e,
0xef77,0xef7f,0xef88,0xef90,0xef99,0xefa2,0xefaa,0xefb3,
0xefbb,0xefc4,0xefcc,0xefd5,0xefdd,0xefe6,0xefee,0xeff7,
0xf000,0xf008,0xf011,0xf019,0xf022,0xf02a,0xf033,0xf03b,
0xf044,0xf04c,0xf055,0xf05d,0xf066,0xf06e,0xf077,0xf07f,
0xf088,0xf090,0xf099,0xf0a1,0xf0aa,0xf0b2,0xf0bb,0xf0c3,
0xf0cc,0xf0d4,0xf0dd,0xf0e5,0xf0ee,0xf0f6,0xf0ff,0xf107,
0xf110,0xf118,0xf121,0xf129,0xf132,0xf13a,0xf143,0xf14b,
0xf154,0xf15c,0xf165,0xf16d,0xf176,0xf17e,0xf187,0xf18f,
0xf198,0xf1a0,0xf1a9,0xf1b1,0xf1ba,0xf1c2,0xf1cb,0xf1d3,
0xf1dc,0xf1e4,0xf1ec,0xf1f5,0xf1fd,0xf206,0xf20e,0xf217,
0xf21f,0xf228,0xf230,0xf239,0xf241,0xf24a,0xf252,0xf25a,
0xf263,0xf26b,0xf274,0xf27c,0xf285,0xf28d,0xf296,0xf29e,
0xf2a6,0xf2af,0xf2b7,0xf2c0,0xf2c8,0xf2d1,0xf2d9,0xf2e1,
0xf2ea,0xf2f2,0xf2fb,0xf303,0xf30c,0xf314,0xf31c,0xf325,
0xf32d,0xf336,0xf33e,0xf347,0xf34f,0xf357,0xf360,0xf368,
0xf371,0xf379,0xf381,0xf38a,0xf392,0xf39b,0xf3a3,0xf3ac,
0xf3b4,0xf3bc,0xf3c5,0xf3cd,0xf3d6,0xf3de,0xf3e6,0xf3ef,
0xf3f7,0xf400,0xf408,0xf410,0xf419,0xf421,0xf429,0xf432,
0xf43a,0xf443,0xf44b,0xf453,0xf45c,0xf464,0xf46d,0xf475,
0xf47d,0xf486,0xf48e,0xf496,0xf49f,0xf4a7,0xf4b0,0xf4b8,
0xf4c0,0xf4c9,0xf4d1,0xf4d9,0xf4e2,0xf4ea,0xf4f2,0xf4fb,
0xf503,0xf50c,0xf514,0xf51c,0xf525,0xf52d,0xf535,0xf53e,
0xf546,0xf54e,0xf557,0xf55f,0xf567,0xf570,0xf578,0xf580,
0xf589,0xf591,0xf599,0xf5a2,0xf5aa,0xf5b2,0xf5bb,0xf5c3,
0xf5cb,0xf5d4,0xf5dc,0xf5e4,0xf5ed,0xf5f5,0xf5fd,0xf606,
0xf60e,0xf616,0xf61f,0xf627,0xf62f,0xf638,0xf640,0xf648,
0xf651,0xf659,0xf661,0xf66a,0xf672,0xf67a,0xf682,0xf68b,
0xf693,0xf69b,0xf6a4,0xf6ac,0xf6b4,0xf6bd,0xf6c5,0xf6cd,
0xf6d6,0xf6de,0xf6e6,0xf6ee,0xf6f7,0xf6ff,0xf707,0xf710,
0xf718,0xf720,0xf728,0xf731,0xf739,0xf741,0xf74a,0xf752,
0xf75a,0xf762,0xf76b,0xf773,0xf77b,0xf784,0xf78c,0xf794,
0xf79c,0xf7a5,0xf7ad,0xf7b5,0xf7bd,0xf7c6,0xf7ce,0xf7d6,
0xf7de,0xf7e7,0xf7ef,0xf7f7,0xf800,0xf808,0xf810,0xf818,
0xf821,0xf829,0xf831,0xf839,0xf842,0xf84a,0xf852,0xf85a,
0xf863,0xf86b,0xf873,0xf87b,0xf883,0xf88c,0xf894,0xf89c,
0xf8a4,0xf8ad,0xf8b5,0xf8bd,0xf8c5,0xf8ce,0xf8d6,0xf8de,
0xf8e6,0xf8ef,0xf8f7,0xf8ff,0xf907,0xf90f,0xf918,0xf920,
0xf928,0xf930,0xf939,0xf941,0xf949,0xf951,0xf959,0xf962,
0xf96a,0xf972,0xf97a,0xf982,0xf98b,0xf993,0xf99b,0xf9a3,
0xf9ab,0xf9b4,0xf9bc,0xf9c4,0xf9cc,0xf9d4,0xf9dd,0xf9e5,
0xf9ed,0xf9f5,0xf9fd,0xfa06,0xfa0e,0xfa16,0xfa1e,0xfa26,
0xfa2f,0xfa37,0xfa3f,0xfa47,0xfa4f,0xfa58,0xfa60,0xfa68,
0xfa70,0xfa78,0xfa80,0xfa89,0xfa91,0xfa99,0xfaa1,0xfaa9,
0xfab1,0xfaba,0xfac2,0xfaca,0xfad2,0xfada,0xfae2,0xfaeb,
0xfaf3,0xfafb,0xfb03,0xfb0b,0xfb13,0xfb1c,0xfb24,0xfb2c,
0xfb34,0xfb3c,0xfb44,0xfb4c,0xfb55,0xfb5d,0xfb65,0xfb6d,
0xfb75,0xfb7d,0xfb85,0xfb8e,0xfb96,0xfb9e,0xfba6,0xfbae,
0xfbb6,0xfbbe,0xfbc7,0xfbcf,0xfbd7,0xfbdf,0xfbe7,0xfbef,
0xfbf7,0xfc00,0xfc08,0xfc10,0xfc18,0xfc20,0xfc28,0xfc30,
0xfc38,0xfc40,0xfc49,0xfc51,0xfc59,0xfc61,0xfc69,0xfc71,
0xfc79,0xfc81,0xfc8a,0xfc92,0xfc9a,0xfca2,0xfcaa,0xfcb2,
0xfcba,0xfcc2,0xfcca,0xfcd2,0xfcdb,0xfce3,0xfceb,0xfcf3,
0xfcfb,0xfd03,0xfd0b,0xfd13,0xfd1b,0xfd23,0xfd2c,0xfd34,
0xfd3c,0xfd44,0xfd4c,0xfd54,0xfd5c,0xfd64,0xfd6c,0xfd74,
0xfd7c,0xfd84,0xfd8d,0xfd95,0xfd9d,0xfda5,0xfdad,0xfdb5,
0xfdbd,0xfdc5,0xfdcd,0xfdd5,0xfddd,0xfde5,0xfded,0xfdf5,
0xfdfd,0xfe06,0xfe0e,0xfe16,0xfe1e,0xfe26,0xfe2e,0xfe36,
0xfe3e,0xfe46,0xfe4e,0xfe56,0xfe5e,0xfe66,0xfe6e,0xfe76,
0xfe7e,0xfe86,0xfe8e,0xfe97,0xfe9f,0xfea7,0xfeaf,0xfeb7,
0xfebf,0xfec7,0xfecf,0xfed7,0xfedf,0xfee7,0xfeef,0xfef7,
0xfeff,0xff07,0xff0f,0xff17,0xff1f,0xff27,0xff2f,0xff37,
0xff3f,0xff47,0xff4f,0xff57,0xff5f,0xff67,0xff6f,0xff77,
0xff7f,0xff87,0xff8f,0xff97,0xff9f,0xffa7,0xffaf,0xffb7,
0xffbf,0xffc7,0xffcf,0xffd7,0xffdf,0xffe7,0xffef,0xfff7
];

const RA144_CODEBOOK1: [u16; 128] = [
    19657, 18474, 18365, 17520, 21048, 18231, 18584, 16671,
    20363, 19069, 19409, 18430, 21844, 18753, 19613, 17411,
    20389, 21772, 20129, 21702, 20978, 20472, 19627, 19387,
    21477, 23134, 21841, 23919, 22089, 21519, 21134, 20852,
    19675, 17821, 19044, 17477, 19986, 16955, 18446, 16086,
    21138, 18899, 20952, 18929, 21452, 17833, 20104, 17159,
    19770, 20056, 20336, 20866, 19329, 18217, 18908, 18004,
    21556, 21948, 23079, 23889, 20922, 19544, 20984, 19781,
    19781, 20984, 19544, 20922, 23889, 23079, 21948, 21556,
    18004, 18908, 18217, 19329, 20866, 20336, 20056, 19770,
    17159, 20104, 17833, 21452, 18929, 20952, 18899, 21138,
    16086, 18446, 16955, 19986, 17477, 19044, 17821, 19675,
    20852, 21134, 21519, 22089, 23919, 21841, 23134, 21477,
    19387, 19627, 20472, 20978, 21702, 20129, 21772, 20389,
    17411, 19613, 18753, 21844, 18430, 19409, 19069, 20363,
    16671, 18584, 18231, 21048, 17520, 18365, 18474, 19657,
];
const RA144_VECTORS1: [[i8; BLOCKSIZE]; 128] = [
  [
     38,  -4,  15,  -4,  14, -13,  12, -11,  -2,  -6,
     -6, -11, -45, -16, -11, -13,  -7,   6, -12,   4,
    -20,   3, -16,  12,  -1,  12,  46,  24,   0,  33,
     -3,   9, -12, -12,  -8,  -7,  17,  -6,   0,  -2,
    ], [
     60, -16,   3, -22,  10, -32,   0, -28, -17, -18,
     -3, -25, -37, -23, -10,   3,   2,   3,   0,   3,
    -14,   0, -14,  -1,   0,   2,  32,   9,  -1,  25,
      7,  13,  -5,  13,   8,   1,   2,   8, -10,   6,
    ], [
     27, -12,  28,  -2,   6,  -7,  15,   9, -11,   1,
    -13, -11, -40,   4, -29, -14, -19,  -5, -23,  -8,
    -30, -13, -17,   0, -14,  12,  34,  20,  -2,  25,
      2, -16,  -4, -12,  15,  16,  29,   7,  24,  10,
    ], [
     49, -24,  16, -20,   2, -26,   2,  -7, -25, -10,
    -11, -25, -32,  -3, -27,   2,  -8,  -8, -11,  -9,
    -24, -17, -16, -14, -13,   2,  20,   5,  -4,  17,
     14, -12,   3,  13,  33,  25,  14,  23,  15,  19,
    ], [
     46,  -6,  21,   8,  -2, -16,  -5,  -8, -11,   4,
      8,  15, -24,   4,  -2, -26,  -3, -16, -16, -14,
     -9,  -2,  -1,   4,  19,   7,  36,  17,   9,  13,
      0,  31,  -5, -12,   7,  -8,  11, -15, -13,  -4,
    ], [
     68, -18,   9,  -9,  -6, -35, -18, -25, -26,  -7,
     10,   1, -16,  -3,  -1,  -9,   6, -19,  -4, -15,
     -4,  -6,   0,  -8,  20,  -2,  23,   2,   7,   5,
     12,  35,   1,  13,  24,   0,  -3,   0, -22,   4,
    ], [
     35, -14,  34,  10, -10, -10,  -1,  12, -20,  12,
      0,  15, -18,  24, -20, -27, -14, -28, -27, -27,
    -20, -19,  -2,  -8,   5,   7,  25,  13,   5,   5,
      6,   5,   2, -12,  31,  15,  23,  -1,  12,   8,
    ], [
     57, -26,  22,  -7, -14, -28, -14,  -3, -35,   0,
      3,   1, -11,  16, -18, -10,  -4, -31, -15, -28,
    -14, -23,  -1, -21,   7,  -2,  11,  -1,   3,  -1,
     18,   9,  10,  13,  49,  24,   8,  14,   2,  16,
    ], [
     25,  15,  22,  11,  18,   4,  15, -22,   8,  -2,
    -17,  -9, -48, -20, -30, -17, -16,  11,  -1,  16,
      2,  10,  -5,  26,  -2,  -4,  22,   0,   2,  10,
     -6,  13, -14,  10, -23,   0,  10,  -2,   1,   0,
    ], [
     47,   3,  11,  -6,  15, -13,   2, -38,  -6, -13,
    -15, -22, -40, -28, -28,   0,  -5,   8,  10,  15,
      7,   7,  -4,  13,  -1, -14,   9, -14,   0,   2,
      4,  18,  -7,  36,  -6,   8,  -3,  13,  -7,   8,
    ], [
     14,   7,  36,  13,  10,  10,  18,   0,   0,   5,
    -25,  -8, -43,   0, -48, -18, -27,   0, -12,   3,
     -7,  -6,  -7,  13, -15,  -5,  11,  -3,   0,   2,
      0, -12,  -6,  10,   0,  23,  22,  11,  26,  12,
    ], [
     36,  -5,  24,  -4,   7,  -7,   6, -17, -14,  -5,
    -22, -22, -35,  -8, -46,  -1, -17,  -3,   0,   2,
     -2, -10,  -5,   0, -14, -15,  -2, -18,  -2,  -4,
     11,  -7,   1,  36,  18,  32,   7,  27,  17,  20,
    ], [
     33,  13,  29,  24,   1,   1,  -2, -18,   0,   9,
     -3,  17, -27,   0, -21, -30, -12, -11,  -5,  -2,
     12,   4,   9,  19,  18,  -9,  13,  -6,  11,  -8,
     -2,  35,  -8,  10,  -7,  -1,   4, -11, -10,  -2,
    ], [
     55,   1,  17,   6,  -1, -16, -15, -35, -15,  -2,
      0,   4, -19,  -8, -20, -13,  -1, -14,   7,  -3,
     18,   0,  10,   5,  19, -19,   0, -21,   8, -16,
      9,  39,   0,  36,  10,   7,  -9,   4, -20,   5,
    ], [
     22,   5,  42,  26,  -6,   8,   1,   2,  -9,  17,
    -10,  18, -21,  19, -39, -31, -23, -23, -16, -15,
      2, -12,   7,   6,   5,  -9,   1, -10,   7, -16,
      4,   9,   0,  10,  17,  22,  16,   2,  14,   9,
    ], [
     44,  -6,  30,   8,  -9, -10, -11, -14, -23,   5,
     -8,   4, -14,  12, -37, -14, -12, -26,  -4, -16,
      8, -16,   9,  -7,   6, -19, -12, -25,   5, -24,
     15,  13,   8,  36,  34,  31,   1,  18,   4,  18,
    ], [
     -3,  -5,  -9,  -7,  15,  -1,   5,  13,   2,  12,
      5,   2, -21, -23,  -2, -16,   0,   5,  -6,  13,
    -23,   3, -32,  10, -15,   8,  44,  28,   9,  37,
     -2,  13,  -9, -15, -12, -27,  -7, -12,   0, -11,
    ], [
     18, -17, -21, -25,  11, -19,  -6,  -3, -11,   0,
      7, -11, -13, -31,  -1,   0,   9,   1,   5,  12,
    -18,   0, -31,  -2, -13,  -1,  30,  14,   7,  29,
      9,  18,  -1,  10,   4, -18, -22,   3, -10,  -2,
    ], [
    -13, -13,   3,  -5,   7,   4,   9,  34,  -5,  20,
     -2,   3, -16,  -3, -20, -17, -11,  -7, -17,   0,
    -34, -13, -33,  -2, -28,   8,  32,  24,   5,  29,
      3, -12,   0, -15,  11,  -3,   3,   2,  24,   1,
    ], [
      8, -25,  -8, -23,   3, -13,  -3,  17, -20,   8,
      0, -10,  -8, -11, -18,   0,  -1, -10,  -5,   0,
    -28, -17, -32, -15, -26,  -1,  19,   9,   3,  21,
     15,  -7,   6,   9,  29,   5, -10,  17,  15,   9,
    ], [
      4,  -6,  -3,   5,  -1,  -4, -11,  16,  -6,  23,
     19,  29,   0,  -3,   6, -30,   3, -17, -10,  -5,
    -13,  -2, -17,   3,   5,   3,  35,  21,  17,  17,
      2,  35,  -2, -15,   3, -28, -13, -21, -13, -13,
    ], [
     26, -19, -15, -12,  -5, -22, -24,   0, -21,  12,
     21,  15,   8, -11,   7, -12,  14, -20,   2,  -6,
     -7,  -6, -16,  -9,   6,  -5,  21,   7,  15,  10,
     13,  39,   5,  10,  20, -19, -28,  -5, -22,  -5,
    ], [
     -5, -15,   9,   7,  -9,   2,  -8,  37, -14,  31,
     11,  29,   5,  16, -11, -30,  -7, -29, -21, -18,
    -23, -19, -18,  -9,  -7,   3,  23,  17,  14,   9,
      8,   9,   6, -15,  27,  -4,  -2,  -6,  12,  -1,
    ], [
     16, -27,  -2, -10, -13, -16, -20,  20, -29,  20,
     14,  16,  13,   8,  -9, -13,   2, -33,  -9, -19,
    -17, -23, -17, -22,  -6,  -6,   9,   2,  12,   2,
     20,  13,  13,  10,  45,   4, -16,   8,   2,   7,
    ], [
    -16,  14,  -2,   8,  20,  17,   9,   2,  14,  16,
     -6,   5, -24, -28, -21, -20,  -8,   9,   4,  25,
     -1,  11, -22,  24, -15,  -8,  21,   5,  11,  14,
     -5,  18, -11,   7, -27, -20, -14,  -7,   1,  -9,
    ], [
      6,   2, -14,  -9,  16,  -1,  -3, -14,   0,   5,
     -3,  -8, -16, -36, -19,  -3,   1,   6,  17,  24,
      4,   7, -21,  11, -14, -18,   7,  -9,   9,   7,
      6,  22,  -3,  33, -10, -11, -28,   7,  -7,   0,
    ], [
    -26,   6,  11,  10,  12,  23,  12,  23,   5,  24,
    -13,   5, -19,  -8, -38, -21, -20,  -2,  -6,  12,
    -11,  -5, -23,  11, -29,  -9,   9,   0,   7,   6,
      1,  -7,  -2,   7,  -3,   3,  -2,   6,  27,   3,
    ], [
     -4,  -6,   0,  -7,   8,   4,   0,   6,  -9,  13,
    -11,  -7, -11, -15, -37,  -4,  -9,  -5,   5,  11,
     -5,  -9, -22,  -1, -27, -18,  -4, -14,   5,   0,
     12,  -3,   4,  32,  14,  12, -17,  22,  17,  11,
    ], [
     -8,  12,   3,  21,   3,  14,  -8,   5,   4,  28,
      7,  32,  -2,  -8, -12, -34,  -4, -12,   1,   6,
      9,   4,  -7,  17,   4, -13,  11,  -1,  19,  -4,
      0,  39,  -4,   7, -11, -21, -20, -16, -10, -11,
    ], [
     13,   0,  -8,   3,   0,  -4, -21, -11,  -9,  16,
     10,  18,   5, -16, -10, -16,   5, -15,  13,   5,
     15,   1,  -6,   4,   6, -23,  -2, -16,  17, -12,
     10,  44,   3,  33,   6, -12, -34,  -1, -20,  -3,
    ], [
    -18,   4,  17,  23,  -4,  20,  -4,  26,  -3,  36,
      0,  32,   2,  12, -29, -34, -16, -24, -10,  -6,
      0, -12,  -8,   4,  -8, -13,   0,  -6,  16, -12,
      5,  13,   3,   7,  13,   3,  -8,  -2,  14,   0,
    ], [
      3,  -7,   5,   5,  -8,   2, -17,   9, -18,  24,
      2,  19,  10,   4, -28, -17,  -5, -28,   2,  -7,
      4, -15,  -7,  -8,  -6, -23, -13, -21,  14, -20,
     17,  18,  11,  33,  30,  11, -23,  13,   5,   9,
    ], [
     60,  10,   7,  -1,   9,  -8,   6, -13,   2, -15,
     -1, -10, -13, -11,  15,   0,   6,   9,  -1,   0,
    -13,   1, -11,  -3, -13,  21,  13,  26,  -7,  31,
    -10,  -7, -16, -33, -31, -10,  22,  -8,   1,  -2,
    ], [
     82,  -1,  -4, -19,   6, -27,  -6, -29, -12, -26,
      1, -24,  -5, -18,  17,  17,  17,   6,  10,   0,
     -7,  -2,  -9, -16, -12,  11,   0,  11,  -9,  23,
      0,  -3,  -8,  -8, -13,  -1,   8,   7,  -7,   6,
    ], [
     49,   2,  21,   0,   1,  -2,   9,   8,  -6,  -6,
     -8, -10,  -8,   9,  -2,   0,  -4,  -2, -13, -12,
    -23, -15, -12, -16, -26,  21,   2,  21, -11,  23,
     -4, -33,  -7, -33,  -6,  13,  34,   5,  27,  10,
    ], [
     71, -10,   9, -17,  -1, -20,  -3,  -8, -21, -18,
     -6, -24,   0,   1,   0,  16,   6,  -5,   0, -13,
    -17, -19, -11, -29, -25,  11, -11,   6, -13,  15,
      7, -29,   0,  -8,  11,  22,  20,  21,  17,  18,
    ], [
     67,   8,  14,  11,  -7, -11, -11,  -9,  -7,  -3,
     13,  16,   8,   9,  24, -12,  10, -13,  -5, -17,
     -2,  -4,   3, -10,   6,  17,   4,  19,   0,  11,
     -6,  13,  -9, -33, -14, -10,  16, -17, -10,  -4,
    ], [
     90,  -3,   2,  -6, -10, -29, -24, -26, -21, -15,
     15,   2,  16,   1,  25,   4,  21, -16,   6, -18,
      3,  -8,   5, -24,   8,   7,  -9,   4,  -1,   3,
      5,  18,  -1,  -7,   2,  -1,   2,  -1, -19,   3,
    ], [
     57,   0,  27,  13, -14,  -5,  -7,  11, -15,   4,
      5,  16,  13,  29,   6, -13,   0, -25, -16, -31,
    -12, -22,   2, -23,  -6,  16,  -7,  14,  -2,   3,
      0, -12,   0, -33,   9,  13,  28,  -3,  14,   7,
    ], [
     79, -11,  15,  -4, -18, -23, -20,  -5, -30,  -7,
      7,   2,  21,  21,   8,   3,  10, -28,  -4, -31,
     -6, -25,   3, -37,  -4,   7, -20,   0,  -4,  -4,
     11,  -7,   6,  -8,  27,  22,  14,  12,   5,  16,
    ], [
     47,  30,  15,  14,  14,   9,   9, -23,  13, -10,
    -12,  -7, -16, -15,  -3,  -3,  -1,  14,   9,  12,
      9,   8,   0,  10, -14,   4,  -9,   2,  -5,   8,
    -13,  -3, -18, -10, -45,  -3,  16,  -4,   4,   0,
    ], [
     69,  17,   3,  -3,  10,  -8,  -3, -40,  -1, -21,
    -10, -21,  -8, -23,  -1,  13,   8,  11,  21,  11,
     15,   4,   0,  -2, -13,  -5, -23, -12,  -7,   0,
     -1,   0, -10,  14, -28,   5,   1,  11,  -5,   7,
    ], [
     36,  21,  28,  16,   6,  16,  12,  -2,   4,  -2,
    -20,  -7, -11,   4, -20,  -4, -12,   2,  -1,   0,
      0,  -8,  -2,  -2, -27,   4, -21,  -2,  -9,   0,
     -6, -29,  -9, -10, -21,  21,  28,  10,  29,  11,
    ], [
     58,   9,  16,  -1,   2,  -2,   0, -19, -10, -13,
    -17, -21,  -3,  -3, -19,  12,  -2,   0,  10,  -1,
      5, -12,   0, -15, -26,  -5, -34, -16, -11,  -7,
      4, -25,  -2,  14,  -3,  29,  13,  25,  20,  20,
    ], [
     55,  28,  21,  27,  -2,   7,  -8, -20,   4,   1,
      1,  18,   5,   4,   5, -16,   2,  -8,   5,  -5,
     19,   2,  14,   3,   6,   0, -18,  -4,   2, -11,
     -8,  18, -11, -10, -29,  -3,  10, -13,  -8,  -3,
    ], [
     77,  16,   9,   9,  -6, -11, -21, -37, -10, -10,
      4,   5,  13,  -3,   7,   0,  13, -11,  17,  -6,
     25,  -1,  15,  -9,   7,  -9, -32, -19,   0, -18,
      2,  22,  -3,  15, -12,   5,  -4,   2, -17,   5,
    ], [
     44,  20,  34,  29, -10,  13,  -4,   0,  -4,   9,
     -5,  19,  10,  24, -11, -17,  -8, -20,  -5, -19,
      9, -14,  12,  -9,  -6,   0, -30,  -9,   0, -19,
     -2,  -7,  -2, -10,  -5,  20,  21,   1,  17,   9,
    ], [
     66,   8,  23,  11, -14,  -5, -17, -16, -19,  -2,
     -3,   5,  18,  17, -10,   0,   1, -23,   6, -20,
     15, -18,  14, -22,  -5, -10, -44, -23,  -2, -26,
      9,  -3,   4,  14,  12,  29,   7,  16,   7,  18,
    ], [
     18,   9, -17,  -4,  11,   3,   0,  11,   7,   4,
     10,   3,  10, -18,  24,  -3,  14,   7,   4,  10,
    -16,   1, -27,  -4, -27,  17,  12,  30,   0,  35,
     -9,  -3, -12, -36, -35, -30,  -2, -13,   2, -11,
    ], [
     40,  -2, -29, -22,   7, -14, -12,  -5,  -7,  -7,
     12,  -9,  18, -26,  26,  14,  24,   4,  16,   9,
    -10,  -2, -26, -18, -26,   7,  -1,  15,  -1,  27,
      2,   0,  -4, -11, -17, -21, -16,   1,  -7,  -3,
    ], [
      8,   1,  -3,  -2,   3,  10,   3,  32,  -1,  12,
      2,   4,  15,   1,   7,  -3,   2,  -4,  -6,  -3,
    -26, -15, -29, -17, -40,  17,   0,  26,  -2,  27,
     -2, -29,  -4, -36, -10,  -6,   9,   0,  27,   0,
    ], [
     30, -11, -15, -20,   0,  -8,  -9,  15, -15,   0,
      5,  -9,  23,  -6,   8,  13,  13,  -7,   5,  -3,
    -20, -19, -27, -31, -39,   7, -13,  11,  -4,  19,
      8, -25,   3, -11,   7,   2,  -4,  16,  18,   9,
    ], [
     26,   7, -11,   8,  -5,   1, -17,  14,  -1,  15,
     24,  30,  32,   1,  33, -16,  18, -14,   0,  -8,
     -6,  -4, -12, -12,  -6,  13,   2,  23,   8,  15,
     -4,  17,  -5, -36, -18, -30,  -8, -22, -10, -14,
    ], [
     48,  -4, -23,  -9,  -9, -17, -30,  -2, -16,   3,
     26,  16,  40,  -6,  35,   1,  28, -17,  12,  -9,
      0,  -8, -11, -25,  -5,   3, -10,   8,   6,   7,
      6,  22,   1, -11,  -1, -21, -22,  -7, -19,  -5,
    ], [
     15,   0,   2,  10, -13,   7, -14,  35, -10,  23,
     16,  31,  37,  21,  16, -17,   6, -26, -10, -21,
    -16, -21, -13, -25, -19,  13,  -8,  19,   5,   7,
      1,  -8,   2, -36,   5,  -6,   3,  -8,  15,  -1,
    ], [
     37, -12,  -9,  -7, -17, -11, -26,  18, -25,  12,
     19,  17,  45,  14,  17,   0,  17, -30,   1, -22,
    -10, -25, -12, -38, -18,   3, -22,   4,   3,   0,
     13,  -3,  10, -11,  23,   2, -10,   7,   5,   7,
    ], [
      5,  29,  -9,  11,  15,  22,   3,   0,  18,   8,
     -1,   6,   7, -23,   6,  -6,   5,  12,  15,  21,
      5,   8, -17,   9, -28,   0, -11,   6,   2,  12,
    -11,   0, -14, -13, -49, -22,  -8,  -9,   4,  -9,
    ], [
     27,  16, -21,  -6,  12,   3,  -9, -16,   3,  -2,
      1,  -7,  15, -31,   7,  10,  16,   9,  27,  21,
     11,   5, -16,  -3, -26,  -9, -24,  -7,   0,   4,
      0,   4,  -6,  11, -32, -14, -23,   6,  -5,  -1,
    ], [
     -4,  20,   3,  13,   8,  28,   6,  21,  10,  16,
     -8,   7,  12,  -3, -11,  -7,  -5,   0,   4,   8,
     -4,  -8, -18,  -3, -41,   0, -22,   2,   0,   4,
     -5, -25,  -6, -14, -25,   1,   2,   4,  29,   2,
    ], [
     17,   8,  -8,  -4,   4,  10,  -6,   5,  -4,   5,
     -6,  -6,  20, -10,  -9,   9,   4,  -2,  16,   7,
      1, -12, -17, -16, -39,  -9, -36, -12,  -2,  -3,
      6, -21,   1,  11,  -7,  10, -11,  20,  20,  11,
    ], [
     13,  27,  -3,  24,  -1,  19, -14,   3,   9,  20,
     12,  33,  29,  -3,  15, -20,   9,  -9,  11,   3,
     16,   2,  -2,   2,  -7,  -3, -20,   0,  10,  -7,
     -7,  22,  -7, -13, -33, -23, -14, -18,  -7, -12,
    ], [
     35,  15, -15,   6,  -4,   1, -27, -12,  -5,   8,
     15,  19,  37, -11,  16,  -2,  20, -12,  23,   2,
     22,  -1,  -1, -11,  -5, -13, -34, -14,   8, -14,
      4,  26,   0,  11, -16, -14, -29,  -2, -17,  -3,
    ], [
      3,  19,   9,  26,  -8,  26, -10,  24,   0,  28,
      5,  33,  34,  17,  -2, -20,  -1, -22,   0, -10,
      6, -14,  -3, -10, -20,  -4, -32,  -4,   7, -15,
      0,  -3,   0, -13,  -9,   0,  -3,  -4,  17,   0,
    ], [
     25,   7,  -2,   8, -12,   7, -23,   8, -13,  16,
      7,  20,  42,   9,   0,  -3,   9, -25,  12, -10,
     12, -18,  -2, -24, -19, -13, -46, -19,   5, -22,
     10,   0,   8,  11,   8,   9, -17,  11,   7,   8,
    ], [
    -25,  -7,   2,  -8,  12,  -7,  23,  -8,  13, -16,
     -7, -20, -42,  -9,   0,   3,  -9,  25, -12,  10,
    -12,  18,   2,  24,  19,  13,  46,  19,  -5,  22,
    -10,   0,  -8, -11,  -8,  -9,  17, -11,  -7,  -8,
    ], [
     -3, -19,  -9, -26,   8, -26,  10, -24,   0, -28,
     -5, -33, -34, -17,   2,  20,   1,  22,   0,  10,
     -6,  14,   3,  10,  20,   4,  32,   4,  -7,  15,
      0,   3,   0,  13,   9,   0,   3,   4, -17,   0,
    ], [
    -35, -15,  15,  -6,   4,  -1,  27,  12,   5,  -8,
    -15, -19, -37,  11, -16,   2, -20,  12, -23,  -2,
    -22,   1,   1,  11,   5,  13,  34,  14,  -8,  14,
     -4, -26,   0, -11,  16,  14,  29,   2,  17,   3,
    ], [
    -13, -27,   3, -24,   1, -19,  14,  -3,  -9, -20,
    -12, -33, -29,   3, -15,  20,  -9,   9, -11,  -3,
    -16,  -2,   2,  -2,   7,   3,  20,   0, -10,   7,
      7, -22,   7,  13,  33,  23,  14,  18,   7,  12,
    ], [
    -17,  -8,   8,   4,  -4, -10,   6,  -5,   4,  -5,
      6,   6, -20,  10,   9,  -9,  -4,   2, -16,  -7,
     -1,  12,  17,  16,  39,   9,  36,  12,   2,   3,
     -6,  21,  -1, -11,   7, -10,  11, -20, -20, -11,
    ], [
      4, -20,  -3, -13,  -8, -28,  -6, -21, -10, -16,
      8,  -7, -12,   3,  11,   7,   5,   0,  -4,  -8,
      4,   8,  18,   3,  41,   0,  22,  -2,   0,  -4,
      5,  25,   6,  14,  25,  -1,  -2,  -4, -29,  -2,
    ], [
    -27, -16,  21,   6, -12,  -3,   9,  16,  -3,   2,
     -1,   7, -15,  31,  -7, -10, -16,  -9, -27, -21,
    -11,  -5,  16,   3,  26,   9,  24,   7,   0,  -4,
      0,  -4,   6, -11,  32,  14,  23,  -6,   5,   1,
    ], [
     -5, -29,   9, -11, -15, -22,  -3,   0, -18,  -8,
      1,  -6,  -7,  23,  -6,   6,  -5, -12, -15, -21,
     -5,  -8,  17,  -9,  28,   0,  11,  -6,  -2, -12,
     11,   0,  14,  13,  49,  22,   8,   9,  -4,   9,
    ], [
    -37,  12,   9,   7,  17,  11,  26, -18,  25, -12,
    -19, -17, -45, -14, -17,   0, -17,  30,  -1,  22,
     10,  25,  12,  38,  18,  -3,  22,  -4,  -3,   0,
    -13,   3, -10,  11, -23,  -2,  10,  -7,  -5,  -7,
    ], [
    -15,   0,  -2, -10,  13,  -7,  14, -35,  10, -23,
    -16, -31, -37, -21, -16,  17,  -6,  26,  10,  21,
     16,  21,  13,  25,  19, -13,   8, -19,  -5,  -7,
     -1,   8,  -2,  36,  -5,   6,  -3,   8, -15,   1,
    ], [
    -48,   4,  23,   9,   9,  17,  30,   2,  16,  -3,
    -26, -16, -40,   6, -35,  -1, -28,  17, -12,   9,
      0,   8,  11,  25,   5,  -3,  10,  -8,  -6,  -7,
     -6, -22,  -1,  11,   1,  21,  22,   7,  19,   5,
    ], [
    -26,  -7,  11,  -8,   5,  -1,  17, -14,   1, -15,
    -24, -30, -32,  -1, -33,  16, -18,  14,   0,   8,
      6,   4,  12,  12,   6, -13,  -2, -23,  -8, -15,
      4, -17,   5,  36,  18,  30,   8,  22,  10,  14,
    ], [
    -30,  11,  15,  20,   0,   8,   9, -15,  15,   0,
     -5,   9, -23,   6,  -8, -13, -13,   7,  -5,   3,
     20,  19,  27,  31,  39,  -7,  13, -11,   4, -19,
     -8,  25,  -3,  11,  -7,  -2,   4, -16, -18,  -9,
    ], [
     -8,  -1,   3,   2,  -3, -10,  -3, -32,   1, -12,
     -2,  -4, -15,  -1,  -7,   3,  -2,   4,   6,   3,
     26,  15,  29,  17,  40, -17,   0, -26,   2, -27,
      2,  29,   4,  36,  10,   6,  -9,   0, -27,   0,
    ], [
    -40,   2,  29,  22,  -7,  14,  12,   5,   7,   7,
    -12,   9, -18,  26, -26, -14, -24,  -4, -16,  -9,
     10,   2,  26,  18,  26,  -7,   1, -15,   1, -27,
     -2,   0,   4,  11,  17,  21,  16,  -1,   7,   3,
    ], [
    -18,  -9,  17,   4, -11,  -3,   0, -11,  -7,  -4,
    -10,  -3, -10,  18, -24,   3, -14,  -7,  -4, -10,
     16,  -1,  27,   4,  27, -17, -12, -30,   0, -35,
      9,   3,  12,  36,  35,  30,   2,  13,  -2,  11,
    ], [
    -66,  -8, -23, -11,  14,   5,  17,  16,  19,   2,
      3,  -5, -18, -17,  10,   0,  -1,  23,  -6,  20,
    -15,  18, -14,  22,   5,  10,  44,  23,   2,  26,
     -9,   3,  -4, -14, -12, -29,  -7, -16,  -7, -18,
    ], [
    -44, -20, -34, -29,  10, -13,   4,   0,   4,  -9,
      5, -19, -10, -24,  11,  17,   8,  20,   5,  19,
     -9,  14, -12,   9,   6,   0,  30,   9,   0,  19,
      2,   7,   2,  10,   5, -20, -21,  -1, -17,  -9,
    ], [
    -77, -16,  -9,  -9,   6,  11,  21,  37,  10,  10,
     -4,  -5, -13,   3,  -7,   0, -13,  11, -17,   6,
    -25,   1, -15,   9,  -7,   9,  32,  19,   0,  18,
     -2, -22,   3, -15,  12,  -5,   4,  -2,  17,  -5,
    ], [
    -55, -28, -21, -27,   2,  -7,   8,  20,  -4,  -1,
     -1, -18,  -5,  -4,  -5,  16,  -2,   8,  -5,   5,
    -19,  -2, -14,  -3,  -6,   0,  18,   4,  -2,  11,
      8, -18,  11,  10,  29,   3, -10,  13,   8,   3,
    ], [
    -58,  -9, -16,   1,  -2,   2,   0,  19,  10,  13,
     17,  21,   3,   3,  19, -12,   2,   0, -10,   1,
     -5,  12,   0,  15,  26,   5,  34,  16,  11,   7,
     -4,  25,   2, -14,   3, -29, -13, -25, -20, -20,
    ], [
    -36, -21, -28, -16,  -6, -16, -12,   2,  -4,   2,
     20,   7,  11,  -4,  20,   4,  12,  -2,   1,   0,
      0,   8,   2,   2,  27,  -4,  21,   2,   9,   0,
      6,  29,   9,  10,  21, -21, -28, -10, -29, -11,
    ], [
    -69, -17,  -3,   3, -10,   8,   3,  40,   1,  21,
     10,  21,   8,  23,   1, -13,  -8, -11, -21, -11,
    -15,  -4,   0,   2,  13,   5,  23,  12,   7,   0,
      1,   0,  10, -14,  28,  -5,  -1, -11,   5,  -7,
    ], [
    -47, -30, -15, -14, -14,  -9,  -9,  23, -13,  10,
     12,   7,  16,  15,   3,   3,   1, -14,  -9, -12,
     -9,  -8,   0, -10,  14,  -4,   9,  -2,   5,  -8,
     13,   3,  18,  10,  45,   3, -16,   4,  -4,   0,
    ], [
    -79,  11, -15,   4,  18,  23,  20,   5,  30,   7,
     -7,  -2, -21, -21,  -8,  -3, -10,  28,   4,  31,
      6,  25,  -3,  37,   4,  -7,  20,   0,   4,   4,
    -11,   7,  -6,   8, -27, -22, -14, -12,  -5, -16,
    ], [
    -57,   0, -27, -13,  14,   5,   7, -11,  15,  -4,
     -5, -16, -13, -29,  -6,  13,   0,  25,  16,  31,
     12,  22,  -2,  23,   6, -16,   7, -14,   2,  -3,
      0,  12,   0,  33,  -9, -13, -28,   3, -14,  -7,
    ], [
    -90,   3,  -2,   6,  10,  29,  24,  26,  21,  15,
    -15,  -2, -16,  -1, -25,  -4, -21,  16,  -6,  18,
     -3,   8,  -5,  24,  -8,  -7,   9,  -4,   1,  -3,
     -5, -18,   1,   7,  -2,   1,  -2,   1,  19,  -3,
    ], [
    -67,  -8, -14, -11,   7,  11,  11,   9,   7,   3,
    -13, -16,  -8,  -9, -24,  12, -10,  13,   5,  17,
      2,   4,  -3,  10,  -6, -17,  -4, -19,   0, -11,
      6, -13,   9,  33,  14,  10, -16,  17,  10,   4,
    ], [
    -71,  10,  -9,  17,   1,  20,   3,   8,  21,  18,
      6,  24,   0,  -1,   0, -16,  -6,   5,   0,  13,
     17,  19,  11,  29,  25, -11,  11,  -6,  13, -15,
     -7,  29,   0,   8, -11, -22, -20, -21, -17, -18,
    ], [
    -49,  -2, -21,   0,  -1,   2,  -9,  -8,   6,   6,
      8,  10,   8,  -9,   2,   0,   4,   2,  13,  12,
     23,  15,  12,  16,  26, -21,  -2, -21,  11, -23,
      4,  33,   7,  33,   6, -13, -34,  -5, -27, -10,
    ], [
    -82,   1,   4,  19,  -6,  27,   6,  29,  12,  26,
     -1,  24,   5,  18, -17, -17, -17,  -6, -10,   0,
      7,   2,   9,  16,  12, -11,   0, -11,   9, -23,
      0,   3,   8,   8,  13,   1,  -8,  -7,   7,  -6,
    ], [
    -60, -10,  -7,   1,  -9,   8,  -6,  13,  -2,  15,
      1,  10,  13,  11, -15,   0,  -6,  -9,   1,   0,
     13,  -1,  11,   3,  13, -21, -13, -26,   7, -31,
     10,   7,  16,  33,  31,  10, -22,   8,  -1,   2,
    ], [
     -3,   7,  -5,  -5,   8,  -2,  17,  -9,  18, -24,
     -2, -19, -10,  -4,  28,  17,   5,  28,  -2,   7,
     -4,  15,   7,   8,   6,  23,  13,  21, -14,  20,
    -17, -18, -11, -33, -30, -11,  23, -13,  -5,  -9,
    ], [
     18,  -4, -17, -23,   4, -20,   4, -26,   3, -36,
      0, -32,  -2, -12,  29,  34,  16,  24,  10,   6,
      0,  12,   8,  -4,   8,  13,   0,   6, -16,  12,
     -5, -13,  -3,  -7, -13,  -3,   8,   2, -14,   0,
    ], [
    -13,   0,   8,  -3,   0,   4,  21,  11,   9, -16,
    -10, -18,  -5,  16,  10,  16,  -5,  15, -13,  -5,
    -15,  -1,   6,  -4,  -6,  23,   2,  16, -17,  12,
    -10, -44,  -3, -33,  -6,  12,  34,   1,  20,   3,
    ], [
      8, -12,  -3, -21,  -3, -14,   8,  -5,  -4, -28,
     -7, -32,   2,   8,  12,  34,   4,  12,  -1,  -6,
     -9,  -4,   7, -17,  -4,  13, -11,   1, -19,   4,
      0, -39,   4,  -7,  11,  21,  20,  16,  10,  11,
    ], [
      4,   6,   0,   7,  -8,  -4,   0,  -6,   9, -13,
     11,   7,  11,  15,  37,   4,   9,   5,  -5, -11,
      5,   9,  22,   1,  27,  18,   4,  14,  -5,   0,
    -12,   3,  -4, -32, -14, -12,  17, -22, -17, -11,
    ], [
     26,  -6, -11, -10, -12, -23, -12, -23,  -5, -24,
     13,  -5,  19,   8,  38,  21,  20,   2,   6, -12,
     11,   5,  23, -11,  29,   9,  -9,   0,  -7,  -6,
     -1,   7,   2,  -7,   3,  -3,   2,  -6, -27,  -3,
    ], [
     -6,  -2,  14,   9, -16,   1,   3,  14,   0,  -5,
      3,   8,  16,  36,  19,   3,  -1,  -6, -17, -24,
     -4,  -7,  21, -11,  14,  18,  -7,   9,  -9,  -7,
     -6, -22,   3, -33,  10,  11,  28,  -7,   7,   0,
    ], [
     16, -14,   2,  -8, -20, -17,  -9,  -2, -14, -16,
      6,  -5,  24,  28,  21,  20,   8,  -9,  -4, -25,
      1, -11,  22, -24,  15,   8, -21,  -5, -11, -14,
      5, -18,  11,  -7,  27,  20,  14,   7,  -1,   9,
    ], [
    -16,  27,   2,  10,  13,  16,  20, -20,  29, -20,
    -14, -16, -13,  -8,   9,  13,  -2,  33,   9,  19,
     17,  23,  17,  22,   6,   6,  -9,  -2, -12,  -2,
    -20, -13, -13, -10, -45,  -4,  16,  -8,  -2,  -7,
    ], [
      5,  15,  -9,  -7,   9,  -2,   8, -37,  14, -31,
    -11, -29,  -5, -16,  11,  30,   7,  29,  21,  18,
     23,  19,  18,   9,   7,  -3, -23, -17, -14,  -9,
     -8,  -9,  -6,  15, -27,   4,   2,   6, -12,   1,
    ], [
    -26,  19,  15,  12,   5,  22,  24,   0,  21, -12,
    -21, -15,  -8,  11,  -7,  12, -14,  20,  -2,   6,
      7,   6,  16,   9,  -6,   5, -21,  -7, -15, -10,
    -13, -39,  -5, -10, -20,  19,  28,   5,  22,   5,
    ], [
     -4,   6,   3,  -5,   1,   4,  11, -16,   6, -23,
    -19, -29,   0,   3,  -6,  30,  -3,  17,  10,   5,
     13,   2,  17,  -3,  -5,  -3, -35, -21, -17, -17,
     -2, -35,   2,  15,  -3,  28,  13,  21,  13,  13,
    ], [
     -8,  25,   8,  23,  -3,  13,   3, -17,  20,  -8,
      0,  10,   8,  11,  18,   0,   1,  10,   5,   0,
     28,  17,  32,  15,  26,   1, -19,  -9,  -3, -21,
    -15,   7,  -6,  -9, -29,  -5,  10, -17, -15,  -9,
    ], [
     13,  13,  -3,   5,  -7,  -4,  -9, -34,   5, -20,
      2,  -3,  16,   3,  20,  17,  11,   7,  17,   0,
     34,  13,  33,   2,  28,  -8, -32, -24,  -5, -29,
     -3,  12,   0,  15, -11,   3,  -3,  -2, -24,  -1,
    ], [
    -18,  17,  21,  25, -11,  19,   6,   3,  11,   0,
     -7,  11,  13,  31,   1,   0,  -9,  -1,  -5, -12,
     18,   0,  31,   2,  13,   1, -30, -14,  -7, -29,
     -9, -18,   1, -10,  -4,  18,  22,  -3,  10,   2,
    ], [
      3,   5,   9,   7, -15,   1,  -5, -13,  -2, -12,
     -5,  -2,  21,  23,   2,  16,   0,  -5,   6, -13,
     23,  -3,  32, -10,  15,  -8, -44, -28,  -9, -37,
      2, -13,   9,  15,  12,  27,   7,  12,   0,  11,
    ], [
    -44,   6, -30,  -8,   9,  10,  11,  14,  23,  -5,
      8,  -4,  14, -12,  37,  14,  12,  26,   4,  16,
     -8,  16,  -9,   7,  -6,  19,  12,  25,  -5,  24,
    -15, -13,  -8, -36, -34, -31,  -1, -18,  -4, -18,
    ], [
    -22,  -5, -42, -26,   6,  -8,  -1,  -2,   9, -17,
     10, -18,  21, -19,  39,  31,  23,  23,  16,  15,
     -2,  12,  -7,  -6,  -5,   9,  -1,  10,  -7,  16,
     -4,  -9,   0, -10, -17, -22, -16,  -2, -14,  -9,
    ], [
    -55,  -1, -17,  -6,   1,  16,  15,  35,  15,   2,
      0,  -4,  19,   8,  20,  13,   1,  14,  -7,   3,
    -18,   0, -10,  -5, -19,  19,   0,  21,  -8,  16,
     -9, -39,   0, -36, -10,  -7,   9,  -4,  20,  -5,
    ], [
    -33, -13, -29, -24,  -1,  -1,   2,  18,   0,  -9,
      3, -17,  27,   0,  21,  30,  12,  11,   5,   2,
    -12,  -4,  -9, -19, -18,   9, -13,   6, -11,   8,
      2, -35,   8, -10,   7,   1,  -4,  11,  10,   2,
    ], [
    -36,   5, -24,   4,  -7,   7,  -6,  17,  14,   5,
     22,  22,  35,   8,  46,   1,  17,   3,   0,  -2,
      2,  10,   5,   0,  14,  15,   2,  18,   2,   4,
    -11,   7,  -1, -36, -18, -32,  -7, -27, -17, -20,
    ], [
    -14,  -7, -36, -13, -10, -10, -18,   0,   0,  -5,
     25,   8,  43,   0,  48,  18,  27,   0,  12,  -3,
      7,   6,   7, -13,  15,   5, -11,   3,   0,  -2,
      0,  12,   6, -10,   0, -23, -22, -11, -26, -12,
    ], [
    -47,  -3, -11,   6, -15,  13,  -2,  38,   6,  13,
     15,  22,  40,  28,  28,   0,   5,  -8, -10, -15,
     -7,  -7,   4, -13,   1,  14,  -9,  14,   0,  -2,
     -4, -18,   7, -36,   6,  -8,   3, -13,   7,  -8,
    ], [
    -25, -15, -22, -11, -18,  -4, -15,  22,  -8,   2,
     17,   9,  48,  20,  30,  17,  16, -11,   1, -16,
     -2, -10,   5, -26,   2,   4, -22,   0,  -2, -10,
      6, -13,  14, -10,  23,   0, -10,   2,  -1,   0,
    ], [
    -57,  26, -22,   7,  14,  28,  14,   3,  35,   0,
     -3,  -1,  11, -16,  18,  10,   4,  31,  15,  28,
     14,  23,   1,  21,  -7,   2, -11,   1,  -3,   1,
    -18,  -9, -10, -13, -49, -24,  -8, -14,  -2, -16,
    ], [
    -35,  14, -34, -10,  10,  10,   1, -12,  20, -12,
      0, -15,  18, -24,  20,  27,  14,  28,  27,  27,
     20,  19,   2,   8,  -5,  -7, -25, -13,  -5,  -5,
     -6,  -5,  -2,  12, -31, -15, -23,   1, -12,  -8,
    ], [
    -68,  18,  -9,   9,   6,  35,  18,  25,  26,   7,
    -10,  -1,  16,   3,   1,   9,  -6,  19,   4,  15,
      4,   6,   0,   8, -20,   2, -23,  -2,  -7,  -5,
    -12, -35,  -1, -13, -24,   0,   3,   0,  22,  -4,
    ], [
    -46,   6, -21,  -8,   2,  16,   5,   8,  11,  -4,
     -8, -15,  24,  -4,   2,  26,   3,  16,  16,  14,
      9,   2,   1,  -4, -19,  -7, -36, -17,  -9, -13,
      0, -31,   5,  12,  -7,   8, -11,  15,  13,   4,
    ], [
    -49,  24, -16,  20,  -2,  26,  -2,   7,  25,  10,
     11,  25,  32,   3,  27,  -2,   8,   8,  11,   9,
     24,  17,  16,  14,  13,  -2, -20,  -5,   4, -17,
    -14,  12,  -3, -13, -33, -25, -14, -23, -15, -19,
    ], [
    -27,  12, -28,   2,  -6,   7, -15,  -9,  11,  -1,
     13,  11,  40,  -4,  29,  14,  19,   5,  23,   8,
     30,  13,  17,   0,  14, -12, -34, -20,   2, -25,
     -2,  16,   4,  12, -15, -16, -29,  -7, -24, -10,
    ], [
    -60,  16,  -3,  22, -10,  32,   0,  28,  17,  18,
      3,  25,  37,  23,  10,  -3,  -2,  -3,   0,  -3,
     14,   0,  14,   1,   0,  -2, -32,  -9,   1, -25,
     -7, -13,   5, -13,  -8,  -1,  -2,  -8,  10,  -6,
    ], [
    -38,   4, -15,   4, -14,  13, -12,  11,   2,   6,
      6,  11,  45,  16,  11,  13,   7,  -6,  12,  -4,
     20,  -3,  16, -12,   1, -12, -46, -24,   0, -33,
      3,  -9,  12,  12,   8,   7, -17,   6,   0,   2
    ]
];
const RA144_CODEBOOK2: [u16; 128] = [
    12174, 13380, 13879, 13832, 13170, 13227, 13204, 12053,
    12410, 13988, 14348, 14631, 13100, 13415, 13224, 12268,
    11982, 13825, 13499, 14210, 13877, 14788, 13811, 13109,
    11449, 13275, 12833, 13717, 12728, 13696, 12759, 12405,
    10230, 12185, 11628, 13161, 11762, 13458, 12312, 12818,
    10443, 12773, 12011, 14020, 11818, 13825, 12453, 13226,
    10446, 13162, 11881, 14300, 12859, 16288, 13490, 15053,
    10155, 12820, 11519, 13973, 12041, 15081, 12635, 14198,
    14198, 12635, 15081, 12041, 13973, 11519, 12820, 10155,
    15053, 13490, 16288, 12859, 14300, 11881, 13162, 10446,
    13226, 12453, 13825, 11818, 14020, 12011, 12773, 10443,
    12818, 12312, 13458, 11762, 13161, 11628, 12185, 10230,
    12405, 12759, 13696, 12728, 13717, 12833, 13275, 11449,
    13109, 13811, 14788, 13877, 14210, 13499, 13825, 11982,
    12268, 13224, 13415, 13100, 14631, 14348, 13988, 12410,
    12053, 13204, 13227, 13170, 13832, 13879, 13380, 12174,
];
const RA144_VECTORS2: [[i8; BLOCKSIZE]; 128] = [
    [
     73, -32, -60, -15, -26,  59,   2, -33,  30, -10,
     -3, -17,   8,  30,  -1, -26,  -4, -22,  10,  16,
    -36,  -5, -11,  56,  37,   6, -10,  -5, -13,  -3,
      6,  -5,  11,   4, -19,  -5, -16,  41,  24,  13,
    ], [
      4, -11, -37,  23,  -5,  46,  -2, -29,  -5, -39,
    -21,  -9,   0,  49,  12,  -9, -16, -26,  22,  15,
    -45, -20,  -5,  40,  22,  17, -26,  31, -14,   2,
    -14,  10,  30,  20, -27,  -9, -39,  39,  18,   5,
    ], [
     34, -25, -48, -28, -11,  34,  -2, -41,   9,  -7,
    -17,  21,  20,  24, -17, -33,   0, -24,  10,  42,
      3,  -5,  10,  42,  11,   8,  -3,   3,  16,   9,
     22,  -2,   0, -33, -10,  18,   7,  58,  10,  28,
    ], [
    -34,  -4, -25,  10,   9,  21,  -7, -36, -26, -36,
    -35,  28,  12,  42,  -3, -16, -12, -28,  21,  42,
     -5, -21,  16,  26,  -4,  19, -19,  39,  15,  15,
      1,  13,  19, -17, -17,  14, -15,  55,   4,  19,
    ], [
     28, -20, -51, -14,  -6,   7,   0, -26,  27,  -4,
     18, -40,  -6,  16,  -1, -15,   0, -55,  -5, -16,
    -19,  14,  -3,  49,  14,   1, -22, -30, -12,   0,
     24,  15,   9, -17, -45, -29,   4,  28,  51,  35,
    ], [
    -40,   0, -28,  24,  14,  -5,  -4, -21,  -7, -33,
      0, -32, -15,  35,  12,   1, -11, -58,   5, -16,
    -28,   0,   1,  33,   0,  11, -39,   5, -14,   6,
      3,  31,  28,  -1, -53, -33, -19,  25,  46,  26,
    ], [
    -11, -14, -39, -27,   9, -17,  -4, -33,   6,   0,
      4,  -1,   5,  10, -17, -22,   5, -57,  -5,   9,
     20,  13,  18,  35, -11,   3, -16, -22,  17,  13,
     40,  19,  -1, -55, -35,  -5,  27,  44,  37,  49,
    ], [
    -80,   6, -16,  11,  30, -30,  -9, -28, -28, -29,
    -13,   6,  -2,  28,  -3,  -5,  -7, -60,   5,   9,
     11,  -1,  24,  19, -27,  13, -32,  13,  15,  19,
     19,  35,  17, -39, -43,  -9,   4,  42,  32,  41,
    ], [
     78, -21, -43,   4, -38,  17,  17,  -5,  55,  24,
    -15, -36,  14,   4,  24, -24,  12,   5,  17,  31,
    -54,  -5,  -2,  27,  43, -12,   2,   9,  -9, -15,
     22,  -3,  28,  21, -20,   3,  20,  28,   9,  -5,
    ], [
      9,  -1, -20,  43, -17,   3,  12,   0,  20,  -4,
    -33, -29,   6,  22,  38,  -7,   0,   1,  29,  30,
    -63, -21,   3,  11,  27,  -1, -14,  45, -10,  -9,
      1,  12,  47,  37, -28,   0,  -2,  26,   4, -13,
    ], [
     39, -14, -30,  -8, -22,  -8,  12, -12,  34,  27,
    -29,   2,  26,  -2,   8, -31,  16,   3,  17,  57,
    -14,  -6,  19,  13,  16, -10,   8,  17,  20,  -2,
     38,   0,  17, -16, -11,  27,  44,  45,  -4,   8,
    ], [
    -29,   5,  -7,  30,  -1, -21,   7,  -7,   0,   0,
    -47,   9,  18,  15,  22, -14,   4,   0,  28,  57,
    -23, -21,  25,  -2,   1,   0,  -7,  53,  19,   3,
     17,  15,  36,   0, -19,  24,  21,  43,  -9,   0,
    ], [
     33, -10, -34,   5, -17, -35,  15,   1,  53,  30,
      6, -59,   0, -10,  24, -13,  17, -27,   1,  -1,
    -37,  13,   4,  20,  20, -18, -10, -16,  -8, -11,
     39,  18,  26,   0, -46, -20,  41,  15,  37,  15,
    ], [
    -35,  10, -11,  44,   3, -48,  10,   6,  17,   2,
    -11, -51,  -8,   8,  38,   3,   4, -31,  12,  -2,
    -46,  -1,  10,   4,   5,  -7, -26,  19, -10,  -5,
     18,  34,  45,  15, -54, -24,  18,  13,  31,   7,
    ], [
     -5,  -3, -21,  -7,  -2, -60,  10,  -5,  32,  34,
     -7, -20,  11, -16,   8, -20,  21, -29,   1,  24,
      2,  13,  27,   6,  -5, -15,  -3,  -8,  21,   1,
     55,  21,  15, -38, -37,   3,  65,  32,  23,  30,
    ], [
    -74,  17,   0,  31,  18, -73,   5,   0,  -3,   5,
    -25, -12,   3,   1,  22,  -3,   9, -33,  12,  24,
     -6,  -2,  33,  -9, -21,  -5, -20,  27,  19,   7,
     34,  37,  34, -22, -44,   0,  41,  29,  17,  21,
    ], [
     76, -35, -31, -28, -49,  43, -40,   0,  29, -14,
      8,   5,  10,  18, -26, -46,   0,   7,   6,   3,
    -25,  -7,  -2,  40,  28,  14,  18,  -3, -27, -28,
     -8, -45, -13,  34, -13, -27, -15,  31,  12,   3,
    ], [
      7, -15,  -9,   9, -28,  29, -45,   5,  -6, -43,
     -9,  12,   2,  36, -12, -30, -11,   3,  17,   3,
    -34, -22,   3,  24,  12,  24,   2,  32, -28, -22,
    -29, -29,   5,  50, -21, -31, -38,  29,   7,  -5,
    ], [
     36, -29, -19, -41, -34,  18, -45,  -6,   8, -10,
     -5,  43,  23,  11, -42, -53,   5,   5,   6,  30,
     14,  -8,  20,  26,   1,  16,  25,   4,   3, -15,
      7, -41, -23,  -3,  -4,  -3,   8,  48,  -1,  17,
    ], [
    -32,  -8,   3,  -2, -13,   4, -50,  -1, -27, -39,
    -23,  51,  15,  30, -27, -37,  -7,   1,  17,  29,
      5, -23,  25,  10, -14,  26,   8,  41,   1,  -9,
    -13, -26,  -5,  12, -12,  -7, -14,  45,  -6,   9,
    ], [
     31, -24, -23, -27, -29,  -9, -43,   8,  26,  -7,
     30, -17,  -4,   3, -26, -35,   5, -24, -10, -28,
     -9,  12,   5,  33,   5,   8,   5, -29, -26, -24,
      9, -23, -14,  12, -39, -52,   5,  18,  39,  24,
    ], [
    -37,  -3,   0,  10,  -7, -22, -48,  12,  -8, -36,
     12,  -9, -12,  22, -12, -19,  -6, -28,   0, -29,
    -18,  -3,  11,  17, -10,  18, -10,   7, -27, -18,
    -11,  -7,   3,  28, -47, -55, -18,  15,  34,  16,
    ], [
     -8, -17, -10, -40, -13, -34, -47,   0,   5,  -4,
     16,  21,   8,  -2, -42, -43,  10, -26, -10,  -2,
     31,  11,  27,  19, -21,  10,  12, -20,   3, -11,
     25, -20, -25, -25, -29, -28,  28,  34,  25,  38,
    ], [
    -77,   2,  11,  -1,   7, -47, -52,   5, -29, -33,
     -1,  28,   0,  15, -28, -26,  -2, -30,   0,  -2,
     22,  -4,  33,   3, -36,  21,  -3,  15,   2,  -5,
      4,  -4,  -6,  -9, -37, -31,   5,  32,  20,  30,
    ], [
     81, -25, -14,  -8, -61,   0, -25,  28,  54,  20,
     -3, -14,  17,  -8,   0, -44,  16,  35,  13,  18,
    -43,  -7,   6,  11,  33,  -4,  30,  11, -22, -40,
      6, -43,   3,  50, -14, -18,  22,  18,  -1, -16,
    ], [
     12,  -4,   8,  29, -39, -12, -30,  33,  19,  -8,
    -21,  -6,   8,   9,  13, -28,   4,  31,  24,  18,
    -52, -23,  12,  -4,  18,   5,  14,  47, -24, -34,
    -14, -27,  22,  66, -22, -22,  -1,  16,  -6, -24,
    ], [
     41, -18,  -2, -21, -45, -24, -30,  21,  33,  24,
    -17,  24,  29, -15, -16, -51,  21,  33,  13,  45,
     -3,  -8,  28,  -2,   7,  -2,  37,  19,   7, -27,
     22, -39,  -7,  12,  -5,   5,  45,  35, -15,  -1,
    ], [
    -27,   1,  20,  17, -24, -38, -35,  26,  -1,  -4,
    -35,  32,  21,   3,  -2, -35,   8,  29,  24,  44,
    -12, -24,  34, -18,  -8,   7,  21,  55,   5, -21,
      2, -23,  11,  28, -13,   1,  22,  33, -21, -10,
    ], [
     36, -13,  -5,  -7, -40, -51, -28,  36,  52,  27,
     18, -36,   2, -22,   0, -33,  21,   2,  -3, -13,
    -26,  11,  14,   4,  10, -10,  18, -14, -22, -36,
     24, -21,   1,  28, -40, -42,  42,   5,  25,   5,
    ], [
    -32,   6,  17,  31, -19, -65, -33,  41,  16,  -1,
      0, -29,  -6,  -4,  13, -17,   9,  -1,   8, -14,
    -35,  -3,  19, -11,  -4,   0,   1,  21, -23, -30,
      3,  -5,  20,  44, -48, -46,  19,   3,  20,  -3,
    ], [
     -3,  -7,   6, -20, -25, -77, -32,  29,  31,  30,
      4,   2,  14, -29, -16, -40,  26,   0,  -3,  12,
     13,  10,  36,  -9, -15,  -8,  24,  -6,   7, -22,
     40, -17,  -8,  -9, -31, -18,  66,  22,  11,  19,
    ], [
    -72,  13,  29,  18,  -4, -90, -37,  34,  -4,   1,
    -13,   9,   6, -11,  -2, -24,  13,  -3,   7,  11,
      4,  -4,  42, -25, -31,   1,   8,  29,   6, -17,
     19,  -2,  10,   6, -38, -22,  42,  19,   6,  11,
    ], [
    116, -20, -68, -30, -28,  83,  28, -18,  32, -22,
    -13, -21,   5,  28,   5,  -7, -24,  -8, -22,  17,
    -23,  30, -25,  45,  15,  -9, -11, -18,  22, -10,
      4,  -2,  19, -12,  23,   3, -43,   2,  12,  -4,
    ], [
     47,   0, -45,   7,  -7,  69,  23, -13,  -2, -51,
    -32, -14,  -3,  47,  19,   8, -37, -11, -10,  16,
    -32,  15, -19,  29,   0,   1, -28,  18,  20,  -4,
    -16,  13,  38,   3,  15,   0, -66,   0,   7, -13,
    ], [
     77, -13, -56, -43, -13,  57,  23, -26,  11, -19,
    -27,  16,  17,  22, -10, -15, -19, -10, -22,  43,
     16,  30,  -2,  31, -11,  -6,  -5,  -9,  52,   2,
     20,   0,   8, -50,  33,  27, -19,  19,  -1,   9,
    ], [
      8,   6, -33,  -4,   7,  44,  18, -21, -23, -48,
    -46,  24,   9,  40,   3,   1, -32, -13, -11,  43,
      7,  14,   3,  15, -26,   3, -21,  26,  50,   8,
      0,  16,  27, -34,  25,  23, -43,  17,  -6,   1,
    ], [
     71,  -9, -59, -29,  -8,  30,  26, -11,  30, -16,
      8, -44,  -9,  14,   5,   2, -19, -40, -38, -15,
     -7,  50, -17,  38,  -7, -14, -24, -43,  22,  -6,
     22,  19,  17, -34,  -2, -20, -23, -10,  39,  16,
    ], [
      2,  11, -36,   9,  13,  17,  21,  -6,  -5, -45,
    -10, -36, -18,  33,  19,  19, -31, -44, -27, -15,
    -16,  34, -11,  22, -22,  -4, -40,  -7,  21,   0,
      1,  35,  36, -18, -10, -24, -46, -12,  34,   8,
    ], [
     32,  -2, -47, -42,   7,   5,  21, -18,   9, -12,
     -5,  -5,   2,   8, -10,  -4, -14, -42, -38,  10,
     33,  49,   5,  24, -33, -12, -17, -35,  52,   6,
     38,  22,   7, -72,   7,   3,   0,   6,  25,  30,
    ], [
    -36,  18, -24,  -3,  28,  -7,  16, -13, -26, -41,
    -24,   1,  -5,  26,   3,  12, -27, -46, -27,  10,
     24,  34,  10,   8, -49,  -2, -34,   0,  51,  12,
     17,  38,  25, -56,   0,   0, -22,   3,  20,  22,
    ], [
    121,  -9, -50, -10, -40,  40,  43,   9,  58,  12,
    -25, -41,  11,   2,  31,  -5,  -8,  19, -15,  32,
    -41,  30, -16,  16,  20, -28,   0,  -3,  26, -22,
     19,   0,  36,   4,  22,  12,  -6,  -9,  -1, -24,
    ], [
     52,  10, -27,  27, -18,  26,  38,  14,  23, -16,
    -44, -33,   3,  20,  45,  10, -20,  15,  -3,  31,
    -50,  14, -10,   0,   5, -17, -15,  32,  24, -16,
     -1,  15,  55,  20,  14,   8, -29, -12,  -7, -32,
    ], [
     82,  -3, -38, -23, -24,  15,  38,   2,  37,  15,
    -39,  -2,  23,  -4,  15, -12,  -3,  17, -15,  58,
     -1,  29,   6,   2,  -5, -26,   7,   4,  56,  -9,
     35,   3,  25, -33,  32,  36,  17,   7, -15,  -9,
    ], [
     13,  17, -15,  15,  -3,   1,  33,   7,   1, -12,
    -58,   5,  15,  13,  29,   3, -16,  13,  -4,  57,
    -10,  13,  11, -13, -21, -15,  -9,  40,  55,  -3,
     14,  19,  44, -17,  24,  32,  -5,   4, -21, -18,
    ], [
     76,   1, -41,  -9, -19, -12,  41,  17,  55,  18,
     -3, -63,  -3, -12,  30,   5,  -3, -12, -31,   0,
    -24,  49,  -8,   9,  -1, -33, -12, -29,  27, -18,
     37,  21,  34, -17,  -3, -11,  14, -23,  25,  -2,
    ], [
      7,  22, -18,  29,   1, -25,  36,  21,  20,  -9,
    -22, -56, -11,   6,  45,  21, -15, -16, -20,  -1,
    -33,  34,  -2,  -6, -17, -23, -28,   6,  25, -12,
     16,  37,  53,  -1, -11, -15,  -8, -25,  20, -11,
    ], [
     37,   8, -29, -22,  -4, -37,  36,   9,  34,  22,
    -17, -24,   8, -18,  15,  -2,   1, -14, -31,  25,
     15,  48,  13,  -4, -28, -31,  -5, -21,  57,  -4,
     53,  24,  23, -55,   6,  12,  37,  -6,  11,  11,
    ], [
    -31,  28,  -6,  16,  16, -50,  31,  14,   0,  -6,
    -36, -17,   0,   0,  29,  14, -11, -18, -20,  25,
      6,  33,  19, -20, -43, -21, -21,  14,  55,   0,
     32,  40,  42, -39,  -1,   8,  14,  -8,   6,   3,
    ], [
    119, -24, -39, -44, -51,  66, -14,  15,  31, -26,
     -1,   0,   7,  16, -19, -28, -19,  22, -26,   4,
    -13,  28, -16,  29,   5,  -1,  16, -16,   8, -35,
    -10, -42,  -4,  17,  29, -19, -42,  -7,   0, -15,
    ], [
     50,  -3, -16,  -5, -30,  53, -19,  20,  -3, -55,
    -19,   8,   0,  34,  -5, -11, -32,  18, -15,   4,
    -22,  13, -10,  13,  -9,   8,   0,  19,   7, -29,
    -31, -26,  13,  33,  21, -22, -65,  -9,  -4, -23,
    ], [
     79, -17, -27, -56, -36,  41, -19,   8,  10, -22,
    -15,  39,  20,   9, -35, -35, -15,  20, -26,  31,
     26,  27,   6,  15, -20,   0,  23,  -8,  38, -22,
      5, -38, -15, -20,  39,   4, -18,   9, -13,  -1,
    ], [
     10,   3,  -4, -18, -15,  27, -24,  13, -24, -51,
    -34,  47,  12,  28, -21, -19, -27,  16, -15,  30,
     17,  12,  12,   0, -36,  10,   7,  27,  37, -16,
    -15, -22,   3,  -4,  31,   1, -42,   7, -18,  -9,
    ], [
     74, -12, -30, -42, -30,  14, -16,  23,  29, -19,
     20, -21,  -7,   1, -19, -17, -14, -10, -43, -27,
      3,  48,  -8,  22, -16,  -7,   4, -42,   9, -31,
      6, -20,  -6,  -4,   3, -43, -22, -20,  28,   5,
    ], [
      5,   7,  -7,  -4,  -9,   0, -21,  28,  -6, -48,
      2, -14, -15,  20,  -5,   0, -27, -14, -32, -28,
     -5,  32,  -2,   6, -32,   3, -12,  -5,   8, -25,
    -14,  -4,  12,  11,  -4, -47, -45, -22,  22,  -2,
    ], [
     34,  -6, -18, -55, -15, -11, -21,  16,   8, -16,
      6,  16,   5,  -4, -35, -24, -10, -12, -43,  -1,
     43,  47,  14,   8, -43,  -5,  10, -34,  39, -18,
     22, -16, -17, -42,  13, -19,   1,  -3,  14,  20,
    ], [
    -34,  14,   4, -17,   5, -24, -26,  20, -27, -45,
    -12,  24,  -2,  13, -21,  -8, -22, -16, -32,  -2,
     34,  31,  20,  -7, -58,   5,  -5,   2,  38, -12,
      2,  -1,   1, -26,   5, -23, -21,  -6,   8,  11,
    ], [
    124, -13, -21, -23, -62,  23,   0,  43,  57,   8,
    -13, -18,  14, -10,   6, -26,  -3,  49, -19,  19,
    -31,  27,  -7,   0,  11, -20,  29,  -1,  12, -47,
      4, -39,  11,  34,  28,  -9,  -5, -19, -13, -34,
    ], [
     55,   6,   1,  14, -41,  10,  -4,  48,  22, -20,
    -31, -10,   5,   7,  20,  -9, -16,  45,  -8,  19,
    -40,  12,  -1, -15,  -4, -10,  12,  34,  11, -41,
    -16, -24,  30,  49,  20, -13, -28, -22, -18, -43,
    ], [
     84,  -6,  -9, -36, -47,  -1,  -4,  36,  36,  12,
    -27,  20,  26, -17,  -9, -33,   1,  47, -19,  46,
      9,  27,  15, -13, -15, -18,  35,   6,  42, -33,
     20, -36,   1,  -4,  38,  14,  18,  -2, -27, -20,
    ], [
     15,  13,  13,   1, -26, -14,  -9,  41,   1, -16,
    -46,  27,  18,   1,   4, -16, -11,  43,  -8,  45,
      0,  11,  21, -29, -30,  -8,  19,  42,  41, -28,
      0, -20,  20,  11,  30,  10,  -4,  -5, -32, -28,
    ], [
     79,  -2, -12, -22, -42, -28,  -1,  51,  54,  15,
      8, -41,   0, -24,   6, -15,   1,  17, -36, -12,
    -14,  47,   0,  -6, -11, -26,  16, -27,  13, -43,
     22, -18,  10,  12,   2, -34,  15, -33,  13, -13,
    ], [
     10,  18,  10,  15, -21, -41,  -6,  56,  19, -13,
     -9, -33,  -9,  -6,  20,   1, -11,  13, -24, -13,
    -23,  32,   6, -22, -26, -15,   0,   8,  12, -37,
      1,  -2,  28,  27,  -5, -37,  -7, -35,   8, -21,
    ], [
     39,   4,   0, -35, -27, -53,  -6,  44,  33,  18,
     -5,  -2,  11, -31,  -9, -22,   6,  15, -36,  13,
     25,  46,  23, -20, -37, -24,  23, -19,  43, -29,
     38, -14,   0, -26,  12, -10,  38, -16,   0,   0,
    ], [
    -29,  25,  22,   2,  -6, -67, -11,  49,  -1, -10,
    -24,   5,   3, -13,   4,  -5,  -6,  11, -25,  12,
     16,  31,  28, -36, -53, -13,   6,  16,  42, -24,
     17,   1,  18, -10,   4, -13,  15, -18,  -5,  -7,
    ], [
     29, -25, -22,  -2,   6,  67,  11, -49,   1,  10,
     24,  -5,  -3,  13,  -4,   5,   6, -11,  25, -12,
    -16, -31, -28,  36,  53,  13,  -6, -16, -42,  24,
    -17,  -1, -18,  10,  -4,  13, -15,  18,   5,   7,
    ], [
    -39,  -4,   0,  35,  27,  53,   6, -44, -33, -18,
      5,   2, -11,  31,   9,  22,  -6, -15,  36, -13,
    -25, -46, -23,  20,  37,  24, -23,  19, -43,  29,
    -38,  14,   0,  26, -12,  10, -38,  16,   0,   0,
    ], [
    -10, -18, -10, -15,  21,  41,   6, -56, -19,  13,
      9,  33,   9,   6, -20,  -1,  11, -13,  24,  13,
     23, -32,  -6,  22,  26,  15,   0,  -8, -12,  37,
     -1,   2, -28, -27,   5,  37,   7,  35,  -8,  21,
    ], [
    -79,   2,  12,  22,  42,  28,   1, -51, -54, -15,
     -8,  41,   0,  24,  -6,  15,  -1, -17,  36,  12,
     14, -47,   0,   6,  11,  26, -16,  27, -13,  43,
    -22,  18, -10, -12,  -2,  34, -15,  33, -13,  13,
    ], [
    -15, -13, -13,  -1,  26,  14,   9, -41,  -1,  16,
     46, -27, -18,  -1,  -4,  16,  11, -43,   8, -45,
      0, -11, -21,  29,  30,   8, -19, -42, -41,  28,
      0,  20, -20, -11, -30, -10,   4,   5,  32,  28,
    ], [
    -84,   6,   9,  36,  47,   1,   4, -36, -36, -12,
     27, -20, -26,  17,   9,  33,  -1, -47,  19, -46,
     -9, -27, -15,  13,  15,  18, -35,  -6, -42,  33,
    -20,  36,  -1,   4, -38, -14, -18,   2,  27,  20,
    ], [
    -55,  -6,  -1, -14,  41, -10,   4, -48, -22,  20,
     31,  10,  -5,  -7, -20,   9,  16, -45,   8, -19,
     40, -12,   1,  15,   4,  10, -12, -34, -11,  41,
     16,  24, -30, -49, -20,  13,  28,  22,  18,  43,
    ], [
   -124,  13,  21,  23,  62, -23,   0, -43, -57,  -8,
     13,  18, -14,  10,  -6,  26,   3, -49,  19, -19,
     31, -27,   7,   0, -11,  20, -29,   1, -12,  47,
     -4,  39, -11, -34, -28,   9,   5,  19,  13,  34,
    ], [
     34, -14,  -4,  17,  -5,  24,  26, -20,  27,  45,
     12, -24,   2, -13,  21,   8,  22,  16,  32,   2,
    -34, -31, -20,   7,  58,  -5,   5,  -2, -38,  12,
     -2,   1,  -1,  26,  -5,  23,  21,   6,  -8, -11,
    ], [
    -34,   6,  18,  55,  15,  11,  21, -16,  -8,  16,
     -6, -16,  -5,   4,  35,  24,  10,  12,  43,   1,
    -43, -47, -14,  -8,  43,   5, -10,  34, -39,  18,
    -22,  16,  17,  42, -13,  19,  -1,   3, -14, -20,
    ], [
     -5,  -7,   7,   4,   9,   0,  21, -28,   6,  48,
     -2,  14,  15, -20,   5,   0,  27,  14,  32,  28,
      5, -32,   2,  -6,  32,  -3,  12,   5,  -8,  25,
     14,   4, -12, -11,   4,  47,  45,  22, -22,   2,
    ], [
    -74,  12,  30,  42,  30, -14,  16, -23, -29,  19,
    -20,  21,   7,  -1,  19,  17,  14,  10,  43,  27,
     -3, -48,   8, -22,  16,   7,  -4,  42,  -9,  31,
     -6,  20,   6,   4,  -3,  43,  22,  20, -28,  -5,
    ], [
    -10,  -3,   4,  18,  15, -27,  24, -13,  24,  51,
     34, -47, -12, -28,  21,  19,  27, -16,  15, -30,
    -17, -12, -12,   0,  36, -10,  -7, -27, -37,  16,
     15,  22,  -3,   4, -31,  -1,  42,  -7,  18,   9,
    ], [
    -79,  17,  27,  56,  36, -41,  19,  -8, -10,  22,
     15, -39, -20,  -9,  35,  35,  15, -20,  26, -31,
    -26, -27,  -6, -15,  20,   0, -23,   8, -38,  22,
     -5,  38,  15,  20, -39,  -4,  18,  -9,  13,   1,
    ], [
    -50,   3,  16,   5,  30, -53,  19, -20,   3,  55,
     19,  -8,   0, -34,   5,  11,  32, -18,  15,  -4,
     22, -13,  10, -13,   9,  -8,   0, -19,  -7,  29,
     31,  26, -13, -33, -21,  22,  65,   9,   4,  23,
    ], [
   -119,  24,  39,  44,  51, -66,  14, -15, -31,  26,
      1,   0,  -7, -16,  19,  28,  19, -22,  26,  -4,
     13, -28,  16, -29,  -5,   1, -16,  16,  -8,  35,
     10,  42,   4, -17, -29,  19,  42,   7,   0,  15,
    ], [
     31, -28,   6, -16, -16,  50, -31, -14,   0,   6,
     36,  17,   0,   0, -29, -14,  11,  18,  20, -25,
     -6, -33, -19,  20,  43,  21,  21, -14, -55,   0,
    -32, -40, -42,  39,   1,  -8, -14,   8,  -6,  -3,
    ], [
    -37,  -8,  29,  22,   4,  37, -36,  -9, -34, -22,
     17,  24,  -8,  18, -15,   2,  -1,  14,  31, -25,
    -15, -48, -13,   4,  28,  31,   5,  21, -57,   4,
    -53, -24, -23,  55,  -6, -12, -37,   6, -11, -11,
    ], [
     -7, -22,  18, -29,  -1,  25, -36, -21, -20,   9,
     22,  56,  11,  -6, -45, -21,  15,  16,  20,   1,
     33, -34,   2,   6,  17,  23,  28,  -6, -25,  12,
    -16, -37, -53,   1,  11,  15,   8,  25, -20,  11,
    ], [
    -76,  -1,  41,   9,  19,  12, -41, -17, -55, -18,
      3,  63,   3,  12, -30,  -5,   3,  12,  31,   0,
     24, -49,   8,  -9,   1,  33,  12,  29, -27,  18,
    -37, -21, -34,  17,   3,  11, -14,  23, -25,   2,
    ], [
    -13, -17,  15, -15,   3,  -1, -33,  -7,  -1,  12,
     58,  -5, -15, -13, -29,  -3,  16, -13,   4, -57,
     10, -13, -11,  13,  21,  15,   9, -40, -55,   3,
    -14, -19, -44,  17, -24, -32,   5,  -4,  21,  18,
    ], [
    -82,   3,  38,  23,  24, -15, -38,  -2, -37, -15,
     39,   2, -23,   4, -15,  12,   3, -17,  15, -58,
      1, -29,  -6,  -2,   5,  26,  -7,  -4, -56,   9,
    -35,  -3, -25,  33, -32, -36, -17,  -7,  15,   9,
    ], [
    -52, -10,  27, -27,  18, -26, -38, -14, -23,  16,
     44,  33,  -3, -20, -45, -10,  20, -15,   3, -31,
     50, -14,  10,   0,  -5,  17,  15, -32, -24,  16,
      1, -15, -55, -20, -14,  -8,  29,  12,   7,  32,
    ], [
   -121,   9,  50,  10,  40, -40, -43,  -9, -58, -12,
     25,  41, -11,  -2, -31,   5,   8, -19,  15, -32,
     41, -30,  16, -16, -20,  28,   0,   3, -26,  22,
    -19,   0, -36,  -4, -22, -12,   6,   9,   1,  24,
    ], [
     36, -18,  24,   3, -28,   7, -16,  13,  26,  41,
     24,  -1,   5, -26,  -3, -12,  27,  46,  27, -10,
    -24, -34, -10,  -8,  49,   2,  34,   0, -51, -12,
    -17, -38, -25,  56,   0,   0,  22,  -3, -20, -22,
    ], [
    -32,   2,  47,  42,  -7,  -5, -21,  18,  -9,  12,
      5,   5,  -2,  -8,  10,   4,  14,  42,  38, -10,
    -33, -49,  -5, -24,  33,  12,  17,  35, -52,  -6,
    -38, -22,  -7,  72,  -7,  -3,   0,  -6, -25, -30,
    ], [
     -2, -11,  36,  -9, -13, -17, -21,   6,   5,  45,
     10,  36,  18, -33, -19, -19,  31,  44,  27,  15,
     16, -34,  11, -22,  22,   4,  40,   7, -21,   0,
     -1, -35, -36,  18,  10,  24,  46,  12, -34,  -8,
    ], [
    -71,   9,  59,  29,   8, -30, -26,  11, -30,  16,
     -8,  44,   9, -14,  -5,  -2,  19,  40,  38,  15,
      7, -50,  17, -38,   7,  14,  24,  43, -22,   6,
    -22, -19, -17,  34,   2,  20,  23,  10, -39, -16,
    ], [
     -8,  -6,  33,   4,  -7, -44, -18,  21,  23,  48,
     46, -24,  -9, -40,  -3,  -1,  32,  13,  11, -43,
     -7, -14,  -3, -15,  26,  -3,  21, -26, -50,  -8,
      0, -16, -27,  34, -25, -23,  43, -17,   6,  -1,
    ], [
    -77,  13,  56,  43,  13, -57, -23,  26, -11,  19,
     27, -16, -17, -22,  10,  15,  19,  10,  22, -43,
    -16, -30,   2, -31,  11,   6,   5,   9, -52,  -2,
    -20,   0,  -8,  50, -33, -27,  19, -19,   1,  -9,
    ], [
    -47,   0,  45,  -7,   7, -69, -23,  13,   2,  51,
     32,  14,   3, -47, -19,  -8,  37,  11,  10, -16,
     32, -15,  19, -29,   0,  -1,  28, -18, -20,   4,
     16, -13, -38,  -3, -15,   0,  66,   0,  -7,  13,
    ], [
   -116,  20,  68,  30,  28, -83, -28,  18, -32,  22,
     13,  21,  -5, -28,  -5,   7,  24,   8,  22, -17,
     23, -30,  25, -45, -15,   9,  11,  18, -22,  10,
     -4,   2, -19,  12, -23,  -3,  43,  -2, -12,   4,
    ], [
     72, -13, -29, -18,   4,  90,  37, -34,   4,  -1,
     13,  -9,  -6,  11,   2,  24, -13,   3,  -7, -11,
     -4,   4, -42,  25,  31,  -1,  -8, -29,  -6,  17,
    -19,   2, -10,  -6,  38,  22, -42, -19,  -6, -11,
    ], [
      3,   7,  -6,  20,  25,  77,  32, -29, -31, -30,
     -4,  -2, -14,  29,  16,  40, -26,   0,   3, -12,
    -13, -10, -36,   9,  15,   8, -24,   6,  -7,  22,
    -40,  17,   8,   9,  31,  18, -66, -22, -11, -19,
    ], [
     32,  -6, -17, -31,  19,  65,  33, -41, -16,   1,
      0,  29,   6,   4, -13,  17,  -9,   1,  -8,  14,
     35,   3, -19,  11,   4,   0,  -1, -21,  23,  30,
     -3,   5, -20, -44,  48,  46, -19,  -3, -20,   3,
    ], [
    -36,  13,   5,   7,  40,  51,  28, -36, -52, -27,
    -18,  36,  -2,  22,   0,  33, -21,  -2,   3,  13,
     26, -11, -14,  -4, -10,  10, -18,  14,  22,  36,
    -24,  21,  -1, -28,  40,  42, -42,  -5, -25,  -5,
    ], [
     27,  -1, -20, -17,  24,  38,  35, -26,   1,   4,
     35, -32, -21,  -3,   2,  35,  -8, -29, -24, -44,
     12,  24, -34,  18,   8,  -7, -21, -55,  -5,  21,
     -2,  23, -11, -28,  13,  -1, -22, -33,  21,  10,
    ], [
    -41,  18,   2,  21,  45,  24,  30, -21, -33, -24,
     17, -24, -29,  15,  16,  51, -21, -33, -13, -45,
      3,   8, -28,   2,  -7,   2, -37, -19,  -7,  27,
    -22,  39,   7, -12,   5,  -5, -45, -35,  15,   1,
    ], [
    -12,   4,  -8, -29,  39,  12,  30, -33, -19,   8,
     21,   6,  -8,  -9, -13,  28,  -4, -31, -24, -18,
     52,  23, -12,   4, -18,  -5, -14, -47,  24,  34,
     14,  27, -22, -66,  22,  22,   1, -16,   6,  24,
    ], [
    -81,  25,  14,   8,  61,   0,  25, -28, -54, -20,
      3,  14, -17,   8,   0,  44, -16, -35, -13, -18,
     43,   7,  -6, -11, -33,   4, -30, -11,  22,  40,
     -6,  43,  -3, -50,  14,  18, -22, -18,   1,  16,
    ], [
     77,  -2, -11,   1,  -7,  47,  52,  -5,  29,  33,
      1, -28,   0, -15,  28,  26,   2,  30,   0,   2,
    -22,   4, -33,  -3,  36, -21,   3, -15,  -2,   5,
     -4,   4,   6,   9,  37,  31,  -5, -32, -20, -30,
    ], [
      8,  17,  10,  40,  13,  34,  47,   0,  -5,   4,
    -16, -21,  -8,   2,  42,  43, -10,  26,  10,   2,
    -31, -11, -27, -19,  21, -10, -12,  20,  -3,  11,
    -25,  20,  25,  25,  29,  28, -28, -34, -25, -38,
    ], [
     37,   3,   0, -10,   7,  22,  48, -12,   8,  36,
    -12,   9,  12, -22,  12,  19,   6,  28,   0,  29,
     18,   3, -11, -17,  10, -18,  10,  -7,  27,  18,
     11,   7,  -3, -28,  47,  55,  18, -15, -34, -16,
    ], [
    -31,  24,  23,  27,  29,   9,  43,  -8, -26,   7,
    -30,  17,   4,  -3,  26,  35,  -5,  24,  10,  28,
      9, -12,  -5, -33,  -5,  -8,  -5,  29,  26,  24,
     -9,  23,  14, -12,  39,  52,  -5, -18, -39, -24,
    ], [
     32,   8,  -3,   2,  13,  -4,  50,   1,  27,  39,
     23, -51, -15, -30,  27,  37,   7,  -1, -17, -29,
     -5,  23, -25, -10,  14, -26,  -8, -41,  -1,   9,
     13,  26,   5, -12,  12,   7,  14, -45,   6,  -9,
    ], [
    -36,  29,  19,  41,  34, -18,  45,   6,  -8,  10,
      5, -43, -23, -11,  42,  53,  -5,  -5,  -6, -30,
    -14,   8, -20, -26,  -1, -16, -25,  -4,  -3,  15,
     -7,  41,  23,   3,   4,   3,  -8, -48,   1, -17,
    ], [
     -7,  15,   9,  -9,  28, -29,  45,  -5,   6,  43,
      9, -12,  -2, -36,  12,  30,  11,  -3, -17,  -3,
     34,  22,  -3, -24, -12, -24,  -2, -32,  28,  22,
     29,  29,  -5, -50,  21,  31,  38, -29,  -7,   5,
    ], [
    -76,  35,  31,  28,  49, -43,  40,   0, -29,  14,
     -8,  -5, -10, -18,  26,  46,   0,  -7,  -6,  -3,
     25,   7,   2, -40, -28, -14, -18,   3,  27,  28,
      8,  45,  13, -34,  13,  27,  15, -31, -12,  -3,
    ], [
     74, -17,   0, -31, -18,  73,  -5,   0,   3,  -5,
     25,  12,  -3,  -1, -22,   3,  -9,  33, -12, -24,
      6,   2, -33,   9,  21,   5,  20, -27, -19,  -7,
    -34, -37, -34,  22,  44,   0, -41, -29, -17, -21,
    ], [
      5,   3,  21,   7,   2,  60, -10,   5, -32, -34,
      7,  20, -11,  16,  -8,  20, -21,  29,  -1, -24,
     -2, -13, -27,  -6,   5,  15,   3,   8, -21,  -1,
    -55, -21, -15,  38,  37,  -3, -65, -32, -23, -30,
    ], [
     35, -10,  11, -44,  -3,  48, -10,  -6, -17,  -2,
     11,  51,   8,  -8, -38,  -3,  -4,  31, -12,   2,
     46,   1, -10,  -4,  -5,   7,  26, -19,  10,   5,
    -18, -34, -45, -15,  54,  24, -18, -13, -31,  -7,
    ], [
    -33,  10,  34,  -5,  17,  35, -15,  -1, -53, -30,
     -6,  59,   0,  10, -24,  13, -17,  27,  -1,   1,
     37, -13,  -4, -20, -20,  18,  10,  16,   8,  11,
    -39, -18, -26,   0,  46,  20, -41, -15, -37, -15,
    ], [
     29,  -5,   7, -30,   1,  21,  -7,   7,   0,   0,
     47,  -9, -18, -15, -22,  14,  -4,   0, -28, -57,
     23,  21, -25,   2,  -1,   0,   7, -53, -19,  -3,
    -17, -15, -36,   0,  19, -24, -21, -43,   9,   0,
    ], [
    -39,  14,  30,   8,  22,   8, -12,  12, -34, -27,
     29,  -2, -26,   2,  -8,  31, -16,  -3, -17, -57,
     14,   6, -19, -13, -16,  10,  -8, -17, -20,   2,
    -38,   0, -17,  16,  11, -27, -44, -45,   4,  -8,
    ], [
     -9,   1,  20, -43,  17,  -3, -12,   0, -20,   4,
     33,  29,  -6, -22, -38,   7,   0,  -1, -29, -30,
     63,  21,  -3, -11, -27,   1,  14, -45,  10,   9,
     -1, -12, -47, -37,  28,   0,   2, -26,  -4,  13,
    ], [
    -78,  21,  43,  -4,  38, -17, -17,   5, -55, -24,
     15,  36, -14,  -4, -24,  24, -12,  -5, -17, -31,
     54,   5,   2, -27, -43,  12,  -2,  -9,   9,  15,
    -22,   3, -28, -21,  20,  -3, -20, -28,  -9,   5,
    ], [
     80,  -6,  16, -11, -30,  30,   9,  28,  28,  29,
     13,  -6,   2, -28,   3,   5,   7,  60,  -5,  -9,
    -11,   1, -24, -19,  27, -13,  32, -13, -15, -19,
    -19, -35, -17,  39,  43,   9,  -4, -42, -32, -41,
    ], [
     11,  14,  39,  27,  -9,  17,   4,  33,  -6,   0,
     -4,   1,  -5, -10,  17,  22,  -5,  57,   5,  -9,
    -20, -13, -18, -35,  11,  -3,  16,  22, -17, -13,
    -40, -19,   1,  55,  35,   5, -27, -44, -37, -49,
    ], [
     40,   0,  28, -24, -14,   5,   4,  21,   7,  33,
      0,  32,  15, -35, -12,  -1,  11,  58,  -5,  16,
     28,   0,  -1, -33,   0, -11,  39,  -5,  14,  -6,
     -3, -31, -28,   1,  53,  33,  19, -25, -46, -26,
    ], [
    -28,  20,  51,  14,   6,  -7,   0,  26, -27,   4,
    -18,  40,   6, -16,   1,  15,   0,  55,   5,  16,
     19, -14,   3, -49, -14,  -1,  22,  30,  12,   0,
    -24, -15,  -9,  17,  45,  29,  -4, -28, -51, -35,
    ], [
     34,   4,  25, -10,  -9, -21,   7,  36,  26,  36,
     35, -28, -12, -42,   3,  16,  12,  28, -21, -42,
      5,  21, -16, -26,   4, -19,  19, -39, -15, -15,
     -1, -13, -19,  17,  17, -14,  15, -55,  -4, -19,
    ], [
    -34,  25,  48,  28,  11, -34,   2,  41,  -9,   7,
     17, -21, -20, -24,  17,  33,   0,  24, -10, -42,
     -3,   5, -10, -42, -11,  -8,   3,  -3, -16,  -9,
    -22,   2,   0,  33,  10, -18,  -7, -58, -10, -28,
    ], [
     -4,  11,  37, -23,   5, -46,   2,  29,   5,  39,
     21,   9,   0, -49, -12,   9,  16,  26, -22, -15,
     45,  20,   5, -40, -22, -17,  26, -31,  14,  -2,
     14, -10, -30, -20,  27,   9,  39, -39, -18,  -5,
    ], [
    -73,  32,  60,  15,  26, -59,  -2,  33, -30,  10,
      3,  17,  -8, -30,   1,  26,   4,  22, -10, -16,
     36,   5,  11, -56, -37,  -6,  10,   5,  13,   3,
     -6,   5, -11,  -4,  19,   5,  16, -41, -24, -13
    ]
];

const RA144_GAIN_VAL_TAB: [[u16; 3]; 256] = [
    [ 541, 956,  768], [ 877, 581,  568], [ 675,1574,  635], [1248,1464,  668],
    [1246, 839, 1394], [2560,1386,  991], [ 925, 687,  608], [2208, 797, 1144],
    [ 535, 832,  799], [ 762, 605, 1154], [ 832,1122, 1003], [1180, 687, 1176],
    [1292, 901,  732], [1656, 689,  896], [1750,1248,  848], [2284, 942, 1022],
    [ 824,1472,  643], [ 517, 765,  512], [ 562,1816, 1522], [ 694,1826, 2700],
    [ 704, 524,  672], [1442, 757, 2232], [ 884, 551, 1266], [2232,1007, 1692],
    [ 932, 746,  777], [1132, 822,  926], [1226, 771,  611], [2948,1342, 1008],
    [1302, 594, 1158], [1602, 636, 1128], [3408, 910, 1438], [1996, 614,  575],
    [ 665, 935,  628], [ 631,1192,  829], [ 644, 926, 1052], [ 879, 988, 1226],
    [ 941,2768, 2772], [ 565,1344, 2304], [ 547, 628,  740], [ 639, 532, 1074],
    [ 955,1208,  598], [1124,1160,  900], [1206, 899, 1242], [ 746, 533,  624],
    [1458,1028,  735], [1706,1102,  692], [1898,1018, 1004], [2176, 988,  735],
    [1578, 782, 1642], [ 897, 516,  754], [2068, 702, 1656], [2344, 818, 1526],
    [ 907, 652,  592], [1056, 652,  642], [2124,1416,  780], [2664,1250,  727],
    [1894, 727, 1108], [2196, 657,  981], [4840, 920, 1704], [4992,1238,  983],
    [2420, 909, 1094], [2760, 935, 1032], [2800, 612,  853], [3068, 832,  574],
    [ 523,1796,  923], [ 722,1916, 1382], [1226,1542,  928], [ 758, 757,  584],
    [ 512,1134,  577], [ 615,1276,  698], [ 574,2568, 2356], [ 993,2728, 3512],
    [ 539, 890,  913], [ 694, 928, 1088], [ 805, 600, 1360], [2160, 951, 3128],
    [ 816, 950,  590], [ 955, 847,  811], [1094, 883,  556], [1304, 888,  604],
    [ 863,1170,  855], [1023, 997, 1032], [ 932,1228, 1280], [ 627, 564,  573],
    [ 876, 900, 1448], [1030, 857, 1792], [1294, 953, 1758], [1612, 854, 1714],
    [1090,1166,  631], [1314,1202,  751], [1480, 905,  795], [1682,1016,  568],
    [1494,1178,  983], [ 878, 613,  526], [1728,1446,  779], [2136,1348,  774],
    [ 950, 649,  939], [1180, 703,  899], [1236, 527, 1158], [1450, 647,  972],
    [1282, 647,  707], [1460, 663,  644], [1614, 572,  578], [3516,1222,  821],
    [2668, 729, 1682], [3128, 585, 1502], [3208, 733,  976], [6800, 871, 1416],
    [3480, 743, 1408], [3764, 899, 1170], [3772, 632,  875], [4092, 732,  638],
    [3112, 753, 2620], [3372, 945, 1890], [3768, 969, 2288], [2016, 559,  854],
    [1736, 729,  787], [1940, 686,  547], [2140, 635,  674], [4480,1272,  828],
    [3976, 592, 1666], [4384, 621, 1388], [4400, 801,  955], [4656, 522,  646],
    [4848, 625, 1636], [4984, 591,  874], [5352, 535, 1001], [11216,938, 1184],
    [ 925,3280, 1476], [ 735,1580, 1088], [1150,1576,  674], [ 655, 783,  528],
    [ 527,2052, 1354], [ 782,1704, 1880], [ 578, 910, 1026], [ 692, 882, 1468],
    [ 586, 683,  715], [ 739, 609,  717], [ 778, 773,  697], [ 922, 785,  813],
    [ 766, 651,  984], [ 978, 596, 1030], [1070, 757, 1080], [1324, 687, 1178],
    [1108,2144,  979], [ 723, 982,  690], [ 936, 956,  527], [1180,1002,  547],
    [ 517,1306,  825], [ 832,1184,  974], [1024, 957,  903], [1262,1090,  906],
    [1028, 720,  649], [1192, 679,  694], [2468,1480,  979], [2844,1370,  877],
    [1310, 835,  848], [1508, 839,  698], [1742,1030,  769], [1910, 852,  573],
    [1280, 859, 1174], [1584, 863, 1108], [1686, 708, 1364], [1942, 768, 1104],
    [ 891, 536,  690], [1016, 560,  663], [2172, 870, 1348], [2404, 999, 1170],
    [1890, 966,  889], [2116, 912,  777], [2296,1020,  714], [4872,1844,  932],
    [2392, 778,  929], [2604, 772,  744], [2764, 957,  722], [5832,1532,  984],
    [2188, 519, 1264], [2332, 532,  922], [5064, 995, 2412], [2708, 571,  874],
    [2408, 545,  666], [5016,1084,  875], [5376, 983, 1196], [5536, 979,  730],
    [5344, 634, 1744], [5688, 706, 1348], [5912, 977, 1190], [6072, 905,  763],
    [6048, 582, 1526], [11968,1013,1816], [12864,937, 1900], [12560,1086, 998],
    [1998, 684, 1884], [2504, 633, 1992], [1252, 567,  835], [1478, 571,  973],
    [2620, 769, 1414], [2808, 952, 1142], [2908, 712, 1028], [2976, 686,  741],
    [1462, 552,  714], [3296, 991, 1452], [1590, 615,  544], [3480,1150,  824],
    [3212, 832,  923], [3276, 839,  531], [3548, 786,  852], [3732, 764,  570],
    [5728, 906, 2616], [6272, 804, 2252], [3096, 535,  876], [3228, 598,  649],
    [6536, 759, 1436], [6648, 993,  846], [6864, 567, 1210],[14016,1012, 1302],
    [3408, 548, 1098], [7160,1008, 1742], [7136,1000, 1182], [7480,1032,  836],
    [7448, 612, 1552], [7744, 614,  816], [8384, 777, 1438], [8784, 694,  786],
    [ 882,1508, 1068], [ 597, 837,  766], [1270, 954, 1408], [ 803, 550,  798],
    [1398,1308,  798], [1848,1534,  738], [ 970, 675,  608], [1264, 706,  684],
    [1716, 767, 1126], [2108, 765, 1404], [2236, 924, 1003], [2472,1048,  611],
    [ 999, 942,  963], [1094, 857,  935], [2936, 926, 1138], [1934, 746,  551],
    [3336, 633, 1762], [3764, 701, 1454], [1890, 564,  636], [4096,1126,  793],
    [3936, 556, 1140], [3936, 540,  740], [4216, 764,  874], [8480,1328, 1014],
    [2184, 515, 1042], [4432, 934, 1344], [4784, 945, 1112], [5016,1062,  733],
    [9216,1020, 2028], [9968, 924, 1188], [5424, 909, 1206], [6512, 744, 1086]
];
const RA144_GAIN_EXP_TAB: [u8; 256] = [
   15, 15, 15, 15, 15, 16, 14, 15, 14, 14, 14, 14, 14, 14, 14, 14,
   14, 13, 14, 14, 13, 14, 13, 14, 13, 13, 13, 14, 13, 13, 14, 13,
   13, 13, 13, 13, 14, 13, 12, 12, 13, 13, 13, 12, 13, 13, 13, 13,
   13, 12, 13, 13, 12, 12, 13, 13, 13, 13, 14, 14, 13, 13, 13, 13,
   13, 13, 13, 12, 12, 12, 13, 13, 12, 12, 12, 13, 12, 12, 12, 12,
   12, 12, 12, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 11, 12, 12,
   12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 14, 13, 13, 13, 13,
   13, 13, 13, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 14,
   13, 12, 12, 11, 12, 12, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
   12, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 11, 11, 11, 11,
   12, 12, 12, 12, 11, 11, 12, 12, 12, 12, 12, 13, 12, 12, 12, 13,
   12, 12, 13, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14,
   12, 12, 11, 11, 12, 12, 12, 12, 11, 12, 11, 12, 12, 12, 12, 12,
   13, 13, 12, 12, 13, 13, 13, 14, 12, 13, 13, 13, 13, 13, 13, 13,
   11, 10, 11, 10, 11, 11, 10, 10, 11, 11, 11, 11, 10,  9, 11, 10,
   12, 12, 11, 12, 12, 12, 12, 13, 11, 12, 12, 12, 13, 13, 12, 12
];
