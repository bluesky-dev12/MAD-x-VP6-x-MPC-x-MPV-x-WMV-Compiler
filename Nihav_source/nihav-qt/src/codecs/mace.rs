use nihav_core::codecs::*;
use std::str::FromStr;

const SND_U8P_FORMAT: NASoniton = NASoniton { bits: 8, be: false, packed: false, planar: true, float: false, signed: false };

#[derive(Clone,Copy,Default)]
struct ChannelPredictor {
    index:      usize,
    prev:       i16,
    pprev:      i16,
    scale:      i16,
    level:      i16,
}

fn clip(val: i32) -> i16 {
    if val > 0x7FFF { 0x7FFF }
    else if val < -0x8000 { -0x7FFF }
    else { val as i16 }
}

fn to_sample(val: i32) -> u8 {
    ((val >> 8) + 128).max(0).min(255) as u8
}

impl ChannelPredictor {
    fn get_quant(&mut self, idx: usize, middle: bool) -> i16 {
        let pred;
        let tab_idx = (self.index >> 4) & 0x7F;
        if !middle {
            if idx < 4 {
                pred = QUANT_TAB0[tab_idx][idx];
            } else {
                pred = -1 - QUANT_TAB0[tab_idx][8 - 1 - idx];
            }
            self.index -= self.index >> 5;
            self.index = ((self.index as isize) + (STEP_TAB0[idx] as isize)).max(0) as usize;
        } else {
            if idx < 2 {
                pred = QUANT_TAB1[tab_idx][idx];
            } else {
                pred = -1 - QUANT_TAB1[tab_idx][4 - 1 - idx];
            }
            self.index -= self.index >> 5;
            self.index = ((self.index as isize) + (STEP_TAB1[idx] as isize)).max(0) as usize;
        }

        pred
    }
    fn pred_mace3(&mut self, idx: usize, middle: bool) -> u8 {
        let pred = self.get_quant(idx, middle);
        let cur = clip(i32::from(pred) + i32::from(self.level));
        self.level = cur - (cur >> 3);
        to_sample(i32::from(cur))
    }
    fn pred_mace6(&mut self, idx: usize, middle: bool) -> (u8, u8) {
        let pred = self.get_quant(idx, middle);
        if (self.prev ^ pred) >= 0 {
            self.scale = self.scale.saturating_add(506);
        } else {
            self.scale = clip(i32::from(self.scale) - 314);
        }
        let mut cur = clip(i32::from(pred) + i32::from(self.level));

        self.level = ((i32::from(cur) * i32::from(self.scale)) >> 15) as i16;
        cur >>= 1;

        let diff = (i32::from(self.pprev) - i32::from(cur)) >> 2;
        let s0 = i32::from(self.pprev) + i32::from(self.prev) - diff;
        let s1 = i32::from(self.prev) + i32::from(cur) + diff;

        self.pprev = self.prev;
        self.prev  = cur;

        (to_sample(s0), to_sample(s1))
    }
}

struct MaceDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    ch_pred:    [ChannelPredictor; 2],
    is_mace6:   bool,
}

impl MaceDecoder {
    fn new(is_mace6: bool) -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_U8P_FORMAT, 1),
            chmap:      NAChannelMap::new(),
            ch_pred:    [ChannelPredictor::default(); 2],
            is_mace6,
        }
    }
}

impl NADecoder for MaceDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            let channels = ainfo.get_channels() as usize;
            validate!(channels == 2 || channels == 1);
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), channels as u8, SND_U8P_FORMAT, 1);
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
            let nsamples = pktbuf.len() * (if self.is_mace6 { 6 } else { 3 }) / channels;
            let abuf = alloc_audio_buffer(self.ainfo, nsamples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_u8().unwrap();
            let mut off = [adata.get_offset(0), adata.get_offset(1)];
            let dst = adata.get_data_mut().unwrap();

            if !self.is_mace6 {
                for src in pktbuf.chunks(channels) {
                    for ch in 0..channels {
                        let val = src[ch];
                        let idx0 = val & 7;
                        let idx1 = (val >> 3) & 3;
                        let idx2 = val >> 5;
                        dst[off[ch]]     = self.ch_pred[ch].pred_mace3(idx0 as usize, false);
                        dst[off[ch] + 1] = self.ch_pred[ch].pred_mace3(idx1 as usize, true);
                        dst[off[ch] + 2] = self.ch_pred[ch].pred_mace3(idx2 as usize, false);
                        off[ch] += 3;
                    }
                }
            } else {
                for src in pktbuf.chunks(channels) {
                    for ch in 0..channels {
                        let val = src[ch];
                        let idx0 = val >> 5;
                        let idx1 = (val >> 3) & 3;
                        let idx2 = val & 7;
                        let (s0, s1) = self.ch_pred[ch].pred_mace6(idx0 as usize, false);
                        dst[off[ch]]     = s0;
                        dst[off[ch] + 1] = s1;
                        let (s0, s1) = self.ch_pred[ch].pred_mace6(idx1 as usize, true);
                        dst[off[ch] + 2] = s0;
                        dst[off[ch] + 3] = s1;
                        let (s0, s1) = self.ch_pred[ch].pred_mace6(idx2 as usize, false);
                        dst[off[ch] + 4] = s0;
                        dst[off[ch] + 5] = s1;
                        off[ch] += 6;
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

impl NAOptionHandler for MaceDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_3() -> Box<dyn NADecoder + Send> {
    Box::new(MaceDecoder::new(false))
}

pub fn get_decoder_6() -> Box<dyn NADecoder + Send> {
    Box::new(MaceDecoder::new(true))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    // samples from https://samples.mplayerhq.hu/A-codecs/MACE/surge-samples/
    #[test]
    fn test_mace3() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "mace-3", "assets/QT/surge-1-8-MAC3.mov", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x2df88db3, 0xa6167019, 0x6d4c64e7, 0xc89da2a5]));
    }
    #[test]
    fn test_mace6() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "mace-6", "assets/QT/surge-1-8-MAC6.mov", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xc32857e2, 0xc1ea1ce8, 0x2d77dacf, 0xef504f1f]));
    }
}

const STEP_TAB0: [i16; 8] = [ -13, 8, 76, 222, 222, 76, 8, -13 ];
const STEP_TAB1: [i16; 4] = [ -18, 140, 140, -18 ];
const QUANT_TAB0: [[i16; 4]; 128] = [
    [    37,   116,   206,   330 ], [    39,   121,   216,   346 ],
    [    41,   127,   225,   361 ], [    42,   132,   235,   377 ],
    [    44,   137,   245,   392 ], [    46,   144,   256,   410 ],
    [    48,   150,   267,   428 ], [    51,   157,   280,   449 ],
    [    53,   165,   293,   470 ], [    55,   172,   306,   490 ],
    [    58,   179,   319,   511 ], [    60,   187,   333,   534 ],
    [    63,   195,   348,   557 ], [    66,   205,   364,   583 ],
    [    69,   214,   380,   609 ], [    72,   223,   396,   635 ],
    [    75,   233,   414,   663 ], [    79,   244,   433,   694 ],
    [    82,   254,   453,   725 ], [    86,   265,   472,   756 ],
    [    90,   278,   495,   792 ], [    94,   290,   516,   826 ],
    [    98,   303,   538,   862 ], [   102,   316,   562,   901 ],
    [   107,   331,   588,   942 ], [   112,   345,   614,   983 ],
    [   117,   361,   641,  1027 ], [   122,   377,   670,  1074 ],
    [   127,   394,   701,  1123 ], [   133,   411,   732,  1172 ],
    [   139,   430,   764,  1224 ], [   145,   449,   799,  1280 ],
    [   152,   469,   835,  1337 ], [   159,   490,   872,  1397 ],
    [   166,   512,   911,  1459 ], [   173,   535,   951,  1523 ],
    [   181,   558,   993,  1590 ], [   189,   584,  1038,  1663 ],
    [   197,   610,  1085,  1738 ], [   206,   637,  1133,  1815 ],
    [   215,   665,  1183,  1895 ], [   225,   695,  1237,  1980 ],
    [   235,   726,  1291,  2068 ], [   246,   759,  1349,  2161 ],
    [   257,   792,  1409,  2257 ], [   268,   828,  1472,  2357 ],
    [   280,   865,  1538,  2463 ], [   293,   903,  1606,  2572 ],
    [   306,   944,  1678,  2688 ], [   319,   986,  1753,  2807 ],
    [   334,  1030,  1832,  2933 ], [   349,  1076,  1914,  3065 ],
    [   364,  1124,  1999,  3202 ], [   380,  1174,  2088,  3344 ],
    [   398,  1227,  2182,  3494 ], [   415,  1281,  2278,  3649 ],
    [   434,  1339,  2380,  3811 ], [   453,  1398,  2486,  3982 ],
    [   473,  1461,  2598,  4160 ], [   495,  1526,  2714,  4346 ],
    [   517,  1594,  2835,  4540 ], [   540,  1665,  2961,  4741 ],
    [   564,  1740,  3093,  4953 ], [   589,  1818,  3232,  5175 ],
    [   615,  1898,  3375,  5405 ], [   643,  1984,  3527,  5647 ],
    [   671,  2072,  3683,  5898 ], [   701,  2164,  3848,  6161 ],
    [   733,  2261,  4020,  6438 ], [   766,  2362,  4199,  6724 ],
    [   800,  2467,  4386,  7024 ], [   836,  2578,  4583,  7339 ],
    [   873,  2692,  4786,  7664 ], [   912,  2813,  5001,  8008 ],
    [   952,  2938,  5223,  8364 ], [   995,  3070,  5457,  8739 ],
    [  1039,  3207,  5701,  9129 ], [  1086,  3350,  5956,  9537 ],
    [  1134,  3499,  6220,  9960 ], [  1185,  3655,  6497, 10404 ],
    [  1238,  3818,  6788, 10869 ], [  1293,  3989,  7091, 11355 ],
    [  1351,  4166,  7407, 11861 ], [  1411,  4352,  7738, 12390 ],
    [  1474,  4547,  8084, 12946 ], [  1540,  4750,  8444, 13522 ],
    [  1609,  4962,  8821, 14126 ], [  1680,  5183,  9215, 14756 ],
    [  1756,  5415,  9626, 15415 ], [  1834,  5657, 10057, 16104 ],
    [  1916,  5909, 10505, 16822 ], [  2001,  6173, 10975, 17574 ],
    [  2091,  6448, 11463, 18356 ], [  2184,  6736, 11974, 19175 ],
    [  2282,  7037, 12510, 20032 ], [  2383,  7351, 13068, 20926 ],
    [  2490,  7679, 13652, 21861 ], [  2601,  8021, 14260, 22834 ],
    [  2717,  8380, 14897, 23854 ], [  2838,  8753, 15561, 24918 ],
    [  2965,  9144, 16256, 26031 ], [  3097,  9553, 16982, 27193 ],
    [  3236,  9979, 17740, 28407 ], [  3380, 10424, 18532, 29675 ],
    [  3531, 10890, 19359, 31000 ], [  3688, 11375, 20222, 32382 ],
    [  3853, 11883, 21125, 32767 ], [  4025, 12414, 22069, 32767 ],
    [  4205, 12967, 23053, 32767 ], [  4392, 13546, 24082, 32767 ],
    [  4589, 14151, 25157, 32767 ], [  4793, 14783, 26280, 32767 ],
    [  5007, 15442, 27452, 32767 ], [  5231, 16132, 28678, 32767 ],
    [  5464, 16851, 29957, 32767 ], [  5708, 17603, 31294, 32767 ],
    [  5963, 18389, 32691, 32767 ], [  6229, 19210, 32767, 32767 ],
    [  6507, 20067, 32767, 32767 ], [  6797, 20963, 32767, 32767 ],
    [  7101, 21899, 32767, 32767 ], [  7418, 22876, 32767, 32767 ],
    [  7749, 23897, 32767, 32767 ], [  8095, 24964, 32767, 32767 ],
    [  8456, 26078, 32767, 32767 ], [  8833, 27242, 32767, 32767 ],
    [  9228, 28457, 32767, 32767 ], [  9639, 29727, 32767, 32767 ]
];
const QUANT_TAB1: [[i16; 2]; 128] = [
    [    64,   216 ], [    67,   226 ], [    70,   236 ], [    74,   246 ],
    [    77,   257 ], [    80,   268 ], [    84,   280 ], [    88,   294 ],
    [    92,   307 ], [    96,   321 ], [   100,   334 ], [   104,   350 ],
    [   109,   365 ], [   114,   382 ], [   119,   399 ], [   124,   416 ],
    [   130,   434 ], [   136,   454 ], [   142,   475 ], [   148,   495 ],
    [   155,   519 ], [   162,   541 ], [   169,   564 ], [   176,   590 ],
    [   185,   617 ], [   193,   644 ], [   201,   673 ], [   210,   703 ],
    [   220,   735 ], [   230,   767 ], [   240,   801 ], [   251,   838 ],
    [   262,   876 ], [   274,   914 ], [   286,   955 ], [   299,   997 ],
    [   312,  1041 ], [   326,  1089 ], [   341,  1138 ], [   356,  1188 ],
    [   372,  1241 ], [   388,  1297 ], [   406,  1354 ], [   424,  1415 ],
    [   443,  1478 ], [   462,  1544 ], [   483,  1613 ], [   505,  1684 ],
    [   527,  1760 ], [   551,  1838 ], [   576,  1921 ], [   601,  2007 ],
    [   628,  2097 ], [   656,  2190 ], [   686,  2288 ], [   716,  2389 ],
    [   748,  2496 ], [   781,  2607 ], [   816,  2724 ], [   853,  2846 ],
    [   891,  2973 ], [   930,  3104 ], [   972,  3243 ], [  1016,  3389 ],
    [  1061,  3539 ], [  1108,  3698 ], [  1158,  3862 ], [  1209,  4035 ],
    [  1264,  4216 ], [  1320,  4403 ], [  1379,  4599 ], [  1441,  4806 ],
    [  1505,  5019 ], [  1572,  5244 ], [  1642,  5477 ], [  1715,  5722 ],
    [  1792,  5978 ], [  1872,  6245 ], [  1955,  6522 ], [  2043,  6813 ],
    [  2134,  7118 ], [  2229,  7436 ], [  2329,  7767 ], [  2432,  8114 ],
    [  2541,  8477 ], [  2655,  8854 ], [  2773,  9250 ], [  2897,  9663 ],
    [  3026, 10094 ], [  3162, 10546 ], [  3303, 11016 ], [  3450, 11508 ],
    [  3604, 12020 ], [  3765, 12556 ], [  3933, 13118 ], [  4108, 13703 ],
    [  4292, 14315 ], [  4483, 14953 ], [  4683, 15621 ], [  4892, 16318 ],
    [  5111, 17046 ], [  5339, 17807 ], [  5577, 18602 ], [  5826, 19433 ],
    [  6086, 20300 ], [  6358, 21205 ], [  6642, 22152 ], [  6938, 23141 ],
    [  7248, 24173 ], [  7571, 25252 ], [  7909, 26380 ], [  8262, 27557 ],
    [  8631, 28786 ], [  9016, 30072 ], [  9419, 31413 ], [  9839, 32767 ],
    [ 10278, 32767 ], [ 10737, 32767 ], [ 11216, 32767 ], [ 11717, 32767 ],
    [ 12240, 32767 ], [ 12786, 32767 ], [ 13356, 32767 ], [ 13953, 32767 ],
    [ 14576, 32767 ], [ 15226, 32767 ], [ 15906, 32767 ], [ 16615, 32767 ]
];
