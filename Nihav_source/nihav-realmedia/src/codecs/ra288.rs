use nihav_core::formats::*;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
const CHMAP_MONO: [NAChannelType; 1] = [NAChannelType::C];
const NBLOCKS: usize = 32;
const BLOCKSIZE: usize = 5;
const FRAME_SIZE: usize = 38;

const SP_LPC_ORDER: usize = 36;
const SP_START: usize = 70;
const GAIN_LPC_ORDER: usize = 10;
const GAIN_START: usize = 28;

struct RA288Decoder {
    chmap:  NAChannelMap,
    ainfo:  NAAudioInfo,
    info:   NACodecInfoRef,

    speech_lpc:     [f32; SP_LPC_ORDER],
    speech_hist:    [f32; 111],
    speech_rec:     [f32; SP_LPC_ORDER + 1],
    gain_lpc:       [f32; GAIN_LPC_ORDER],
    gain_hist:      [f32; GAIN_LPC_ORDER + GAIN_START],
    gain_rec:       [f32; GAIN_LPC_ORDER + 1],
}

const BF_ORDER: usize = SP_LPC_ORDER + 1;
const BF_WORK_SIZE: usize = SP_LPC_ORDER + 40 + 35;

fn backfilter(hist: &mut [f32], rec: &mut [f32], filt: &mut [f32], win: &[f32], bw_tab: &[f32], start: usize, non_rec: usize, move_size: usize) {
    let mut temp: [f32; BF_ORDER] = [0.0; BF_ORDER];
    let order = filt.len();

    let mut tmp1: [f32; BF_ORDER] = [0.0; BF_ORDER];
    let mut tmp2: [f32; BF_ORDER] = [0.0; BF_ORDER];
    let mut work: [f32; BF_WORK_SIZE] = [0.0; BF_WORK_SIZE];
    for i in 0..(order + start + non_rec) {
        work[i] = win[i] * hist[i];
    }
    for i in (0..=order).rev() {
        let src1 = &work[(order - i)..];
        let src2 = &work[order + start - i..];
        tmp1[i] = scalarprod(&work[order..],         src1, start);
        tmp2[i] = scalarprod(&work[order + start..], src2, non_rec);
    }

    for i in 0..=order {
        rec[i]  = rec[i] * 0.5625 + tmp1[i];
        temp[i] = rec[i]          + tmp2[i];
    }
    temp[0] *= 257.0 / 256.0;

    if compute_lpc(&temp, filt) {
        for (f, c) in filt.iter_mut().zip(bw_tab.iter()) {
            *f *= c;
        }
    }
    for i in 0..move_size {
        hist[i] = hist[i + start];
    }
}

fn scalarprod(src0: &[f32], src1: &[f32], len: usize) -> f32 {
    let mut sum: f32 = 0.0;
    for (a, b) in src0.iter().take(len).zip(src1.iter().take(len)) {
        sum += *a * *b;
    }
    sum
}

fn compute_lpc(autoc: &[f32], filt: &mut [f32]) -> bool {
    let order = filt.len();
    let mut err = autoc[0];
    if (err <= 0.0) || (autoc[order] == 0.0) {
        return false;
    }
    for i in 0..order {
        let mut r = -autoc[i + 1];
        for j in 0..i {
            r -= filt[j] * autoc[i - j];
        }
        r /= err;
        err *= 1.0 - r * r;
        filt[i] = r;

        for j in 0..((i + 1) >> 1) {
            let f = filt[j];
            let b = filt[i - j - 1];
            filt[j]         = f + r * b;
            filt[i - j - 1] = b + r * f;
        }

        if err < 0.0 { return false; }
    }
    true
}

fn celp_lp_synth_filter(dst: &mut [f32], start: usize, filt: &[f32], src: &[f32]) {
    for (i, el) in src.iter().enumerate() {
        dst[start + i] = *el;
        for (j, coeff) in filt.iter().enumerate() {
            dst[start + i] -= *coeff * dst[start + i - j - 1];
        }
    }
}

impl RA288Decoder {
    fn new() -> Self {
        RA288Decoder {
            chmap:      NAChannelMap::new(),
            ainfo:      NAAudioInfo::new(0, 1, SND_F32P_FORMAT, NBLOCKS * BLOCKSIZE),
            info:       NACodecInfo::new_dummy(),

            speech_lpc:     [0.0; SP_LPC_ORDER],
            speech_hist:    [0.0; 111],
            speech_rec:     [0.0; SP_LPC_ORDER + 1],
            gain_lpc:       [0.0; GAIN_LPC_ORDER],
            gain_hist:      [0.0; 38],
            gain_rec:       [0.0; GAIN_LPC_ORDER + 1],
        }
    }

    fn process_subblock(&mut self, gain: f32, cb_idx: usize) {
        for i in 0..SP_LPC_ORDER {
            self.speech_hist[SP_START + i] = self.speech_hist[75 + i];
        }
        let mut sum: f32 = 32.0;
        for i in 0..GAIN_LPC_ORDER {
            sum -= self.gain_hist[GAIN_START + GAIN_LPC_ORDER - 1 - i] * self.gain_lpc[i];
        }
        sum = sum.max(0.0).min(60.0);

        let scale = (sum * 0.1151292546497).exp() * gain * (1.0 / ((1 << 23) as f32));
        let mut buf: [f32; BLOCKSIZE] = [0.0; BLOCKSIZE];
        for i in 0..BLOCKSIZE {
            buf[i] = (RA288_CODEBOOK[cb_idx][i] as f32) * scale;
        }

        let mut sum: f32 = 0.0;
        for el in buf.iter() {
            sum += *el * *el * (((1 << 24) as f32) / 5.0);
        }
        sum = sum.max(1.0);
        for i in 0..(GAIN_LPC_ORDER - 1) {
            self.gain_hist[GAIN_START + i] = self.gain_hist[GAIN_START + i + 1];
        }
        self.gain_hist[GAIN_START + GAIN_LPC_ORDER - 1] = 10.0 * sum.log10() - 32.0;
        celp_lp_synth_filter(&mut self.speech_hist, SP_START + SP_LPC_ORDER, &self.speech_lpc, &buf);
    }
}

impl NADecoder for RA288Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.chmap.add_channels(&CHMAP_MONO);
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(),
                                          1,
                                          SND_F32P_FORMAT, NBLOCKS * BLOCKSIZE);
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
        let mut adata = abuf.get_abuf_f32().unwrap();
        let dst = adata.get_data_mut().unwrap();

        for (input, output) in pktbuf.chunks(FRAME_SIZE).zip(dst.chunks_mut(NBLOCKS * BLOCKSIZE)) {
            let mut br = BitReader::new(input, BitReaderMode::LE);

            for (i, block) in output.chunks_mut(BLOCKSIZE).enumerate() {
                let gain = RA288_GAIN_TAB[br.read(3)? as usize];
                let cb   = br.read((6 + (i & 1)) as u8)? as usize;

                self.process_subblock(gain, cb);

                for j in 0..BLOCKSIZE {
                    block[j] = self.speech_hist[SP_START + SP_LPC_ORDER + j];
                }
                if (i & 7) == 3 {
                    backfilter(&mut self.speech_hist, &mut self.speech_rec, &mut self.speech_lpc, RA288_SPEECH_WINDOW, RA288_SPEECH_BW_TAB, 40, 35, SP_START);
                    backfilter(&mut self.gain_hist, &mut self.gain_rec, &mut self.gain_lpc, RA288_GAIN_WINDOW, RA288_GAIN_BW_TAB, 8, 20, GAIN_START);
                }
            }
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for RA288Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RA288Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::realmedia_register_all_decoders;
    use crate::realmedia_register_all_demuxers;
    #[test]
    fn test_ra288() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/real/AC-28_8/A0000044.rm
        let file = "assets/RV/A0000044.rm";
        test_decode_audio("realmedia", file, Some(5000), None/*Some("ra28.8")*/, &dmx_reg, &dec_reg);
    }
}

const RA288_GAIN_TAB: [f32; 8] = [
     0.515625,  0.90234375,  1.57910156,  2.76342773,
    -0.515625, -0.90234375, -1.57910156, -2.76342773
];

const RA288_CODEBOOK: [[i16; 5]; 128] = [
    [   668, -2950, -1254, -1790, -2553], [ -5032, -4577, -1045,  2908,  3318],
    [ -2819, -2677,  -948, -2825, -4450], [ -6679,  -340,  1482, -1276,  1262],
    [  -562, -6757,  1281,   179, -1274], [ -2512, -7130, -4925,  6913,  2411],
    [ -2478,  -156,  4683, -3873,     0], [ -8208,  2140,  -478, -2785,   533],
    [  1889,  2759,  1381, -6955, -5913], [  5082, -2460, -5778,  1797,   568],
    [ -2208, -3309, -4523, -6236, -7505], [ -2719,  4358, -2988, -1149,  2664],
    [  1259,   995,  2711, -2464,-10390], [  1722, -7569, -2742,  2171, -2329],
    [  1032,   747,  -858, -7946,-12843], [  3106,  4856, -4193, -2541,  1035],
    [  1862,  -960, -6628,   410,  5882], [ -2493, -2628, -4000,   -60,  7202],
    [ -2672,  1446,  1536, -3831,  1233], [ -5302,  6912,  1589, -4187,  3665],
    [ -3456, -8170, -7709,  1384,  4698], [ -4699, -6209,-11176,  8104, 16830],
    [   930,  7004,  1269, -8977,  2567], [  4649, 11804,  3441, -5657,  1199],
    [  2542,  -183, -8859, -7976,  3230], [ -2872, -2011, -9713, -8385, 12983],
    [  3086,  2140, -3680, -9643, -2896], [ -7609,  6515, -2283, -2522,  6332],
    [ -3333, -5620, -9130,-11131,  5543], [  -407, -6721,-17466, -2889, 11568],
    [  3692,  6796,  -262,-10846, -1856], [  7275, 13404, -2989,-10595,  4936],
    [   244, -2219,  2656,  3776, -5412], [ -4043, -5934,  2131,   863, -2866],
    [ -3302,  1743, -2006,  -128, -2052], [ -6361,  3342, -1583,   -21,  1142],
    [ -3837, -1831,  6397,  2545, -2848], [ -9332, -6528,  5309,  1986, -2245],
    [ -4490,   748,  1935, -3027,  -493], [ -9255,  5366,  3193, -4493,  1784],
    [  4784,  -370,  1866,  1057, -1889], [  7342, -2690, -2577,   676,  -611],
    [  -502,  2235, -1850, -1777, -2049], [  1011,  3880, -2465,  2209,  -152],
    [  2592,  2829,  5588,  2839, -7306], [ -3049, -4918,  5955,  9201, -4447],
    [   697,  3908,  5798, -4451, -4644], [ -2121,  5444, -2570,   321, -1202],
    [  2846, -2086,  3532,   566,  -708], [ -4279,   950,  4980,  3749,   452],
    [ -2484,  3502,  1719,  -170,   238], [ -3435,   263,  2114, -2005,  2361],
    [ -7338, -1208,  9347, -1216, -4013], [-13498,  -439,  8028, -4232,   361],
    [ -3729,  5433,  2004, -4727, -1259], [ -3986,  7743,  8429, -3691,  -987],
    [  5198,  -423,  1150, -1281,   816], [  7409,  4109, -3949,  2690,    30],
    [  1246,  3055,   -35, -1370,  -246], [ -1489,  5635,  -678, -2627,  3170],
    [  4830, -4585,  2008, -1062,   799], [  -129,   717,  4594, 14937, 10706],
    [   417,  2759,  1850, -5057, -1153], [ -3887,  7361, -5768,  4285,   666],
    [  1443,  -938,    20, -2119, -1697], [ -3712, -3402, -2212,   110,  2136],
    [ -2952,    12, -1568, -3500, -1855], [ -1315, -1731,  1160,  -558,  1709],
    [    88, -4569,   194,  -454, -2957], [ -2839, -1666,  -273,  2084,  -155],
    [  -189, -2376,  1663, -1040, -2449], [ -2842, -1369,   636,  -248, -2677],
    [  1517,    79, -3013, -3669,  -973], [  1913, -2493, -5312,  -749,  1271],
    [ -2903, -3324, -3756, -3690, -1829], [ -2913, -1547, -2760, -1406,  1124],
    [  1844, -1834,   456,   706, -4272], [   467, -4256, -1909,  1521,  1134],
    [  -127,  -994,  -637, -1491, -6494], [   873, -2045, -3828, -2792,  -578],
    [  2311, -1817,  2632, -3052,  1968], [   641,  1194,  1893,  4107,  6342],
    [   -45,  1198,  2160, -1449,  2203], [ -2004,  1713,  3518,  2652,  4251],
    [  2936, -3968,  1280,   131, -1476], [  2827,     8, -1928,  2658,  3513],
    [  3199,  -816,  2687, -1741, -1407], [  2948,  4029,   394,  -253,  1298],
    [  4286,    51, -4507,   -32,  -659], [  3903,  5646, -5588, -2592,  5707],
    [  -606,  1234, -1607, -5187,   664], [  -525,  3620, -2192, -2527,  1707],
    [  4297, -3251, -2283,   812, -2264], [  5765,   528, -3287,  1352,  1672],
    [  2735,  1241, -1103, -3273, -3407], [  4033,  1648, -2965, -1174,  1444],
    [    74,   918,  1999,   915, -1026], [ -2496, -1605,  2034,  2950,   229],
    [ -2168,  2037,    15, -1264,  -208], [ -3552,  1530,   581,  1491,   962],
    [ -2613, -2338,  3621, -1488, -2185], [ -1747,    81,  5538,  1432, -2257],
    [ -1019,   867,   214, -2284, -1510], [ -1684,  2816,  -229,  2551, -1389],
    [  2707,   504,   479,  2783, -1009], [  2517, -1487, -1596,   621,  1929],
    [  -148,  2206, -4288,  1292, -1401], [  -527,  1243, -2731,  1909,  1280],
    [  2149, -1501,  3688,   610, -4591], [  3306, -3369,  1875,  3636, -1217],
    [  2574,  2513,  1449, -3074, -4979], [   814,  1826, -2497,  4234, -4077],
    [  1664,  -220,  3418,  1002,  1115], [   781,  1658,  3919,  6130,  3140],
    [  1148,  4065,  1516,   815,   199], [  1191,  2489,  2561,  2421,  2443],
    [   770, -5915,  5515,  -368, -3199], [  1190,  1047,  3742,  6927, -2089],
    [   292,  3099,  4308,  -758, -2455], [   523,  3921,  4044,  1386,    85],
    [  4367,  1006, -1252, -1466, -1383], [  3852,  1579,   -77,  2064,   868],
    [  5109,  2919,  -202,   359,  -509], [  3650,  3206,  2303,  1693,  1296],
    [  2905, -3907,   229, -1196, -2332], [  5977, -3585,   805,  3825, -3138],
    [  3746,  -606,    53,  -269, -3301], [   606,  2018, -1316,  4064,   398]
];

const RA288_SPEECH_WINDOW: &[f32] = &[
  0.576690972, 0.580838025, 0.585013986, 0.589219987, 0.59345597,  0.597723007,
  0.602020264, 0.606384277, 0.610748291, 0.615142822, 0.619598389, 0.624084473,
  0.628570557, 0.633117676, 0.637695313, 0.642272949, 0.646911621, 0.651580811,
  0.656280518, 0.66104126,  0.665802002, 0.670593262, 0.675445557, 0.680328369,
  0.685241699, 0.690185547, 0.695159912, 0.700164795, 0.705230713, 0.710327148,
  0.715454102, 0.720611572, 0.725830078, 0.731048584, 0.736328125, 0.741638184,
  0.747009277, 0.752380371, 0.7578125,   0.763305664, 0.768798828, 0.774353027,
  0.779937744, 0.785583496, 0.791229248, 0.796936035, 0.802703857, 0.808502197,
  0.814331055, 0.820220947, 0.826141357, 0.832092285, 0.838104248, 0.844146729,
  0.850250244, 0.856384277, 0.862548828, 0.868774414, 0.875061035, 0.881378174,
  0.88772583,  0.894134521, 0.900604248, 0.907104492, 0.913635254, 0.920227051,
  0.926879883, 0.933563232, 0.940307617, 0.94708252,  0.953918457, 0.96081543,
  0.96774292,  0.974731445, 0.981781006, 0.988861084, 0.994842529, 0.998565674,
  0.999969482, 0.99911499,  0.996002197, 0.990600586, 0.982910156, 0.973022461,
  0.960876465, 0.946533203, 0.930053711, 0.911437988, 0.89074707,  0.868041992,
  0.843322754, 0.816680908, 0.788208008, 0.757904053, 0.725891113, 0.692199707,
  0.656921387, 0.620178223, 0.582000732, 0.542480469, 0.501739502, 0.459838867,
  0.416900635, 0.373016357, 0.328277588, 0.282775879, 0.236663818, 0.189971924,
  0.142852783, 0.0954284668,0.0477600098
];
const RA288_SPEECH_BW_TAB: &[f32] = &[
  0.98828125,  0.976699829, 0.965254128, 0.953942537, 0.942763507, 0.931715488,
  0.920796931, 0.910006344, 0.899342179, 0.888803005, 0.878387332, 0.868093729,
  0.857920766, 0.847867012, 0.837931097, 0.828111589, 0.818407178, 0.808816493,
  0.799338162, 0.789970934, 0.780713439, 0.771564424, 0.762522638, 0.753586829,
  0.744755745, 0.736028135, 0.727402806, 0.718878567, 0.710454226, 0.702128589,
  0.693900526, 0.685768902, 0.677732527, 0.669790328, 0.66194123,  0.654184103
];

const RA288_GAIN_WINDOW: &[f32] = &[
  0.505699992, 0.524200022, 0.54339999,  0.563300014, 0.583953857, 0.60534668,
  0.627502441, 0.650482178, 0.674316406, 0.699005127, 0.724578857, 0.75112915,
  0.778625488, 0.807128906, 0.836669922, 0.86730957,  0.899078369, 0.932006836,
  0.961486816, 0.982757568, 0.995635986, 1.0,         0.995819092, 0.983154297,
  0.96206665,  0.932769775, 0.895507813, 0.850585938, 0.798400879, 0.739379883,
  0.674072266, 0.602996826, 0.526763916, 0.446014404, 0.361480713, 0.273834229,
  0.183868408, 0.0923461914
];
const RA288_GAIN_BW_TAB: &[f32] = &[
  0.90625,     0.821289063, 0.74432373,  0.674499512, 0.61126709,
  0.553955078, 0.50201416,  0.454956055, 0.41229248,  0.373657227
];
