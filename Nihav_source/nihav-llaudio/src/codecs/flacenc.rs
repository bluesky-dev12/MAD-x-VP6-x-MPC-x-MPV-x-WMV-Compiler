use nihav_core::codecs::*;
use nihav_core::io::bitwriter::*;
use nihav_core::io::intcode::*;
use nihav_codec_support::dsp::lpc::*;

fn to_unsigned(val: i32) -> u32 {
    if val >= 0 {
        val as u32 * 2
    } else {
        -val as u32 * 2 - 1
    }
}

fn calc_header_crc(hdr: &mut [u8]) {
    let mut crc = 0u8;
    let len = hdr.len() - 1;
    for &byte in hdr[..len].iter() {
        crc = CRC8_TABLE[(crc ^ byte) as usize];
    }
    hdr[len] = crc;
}

fn calc_frame_crc(buf: &mut Vec<u8>) {
    let mut crc = 0;
    for &byte in buf.iter() {
        crc = (crc << 8) ^ CRC16_TABLE[(((crc >> 8) as u8) ^ byte) as usize];
    }
    buf.push((crc >> 8) as u8);
    buf.push(crc as u8);
}

#[derive(Clone,Copy,Debug,PartialEq)]
#[allow(dead_code)]
enum StereoMode {
    Normal,
    LeftSide,
    SideRight,
    MidSide,
}

fn find_stereo_mode(samp1: &[i32], samp2: &[i32]) -> StereoMode {
    let mut sums = [0; 3];
    for (&l, &r) in samp1.iter().zip(samp2.iter()) {
        sums[0] += l.abs() + r.abs();
        sums[1] += l.abs() + (l - r).abs();
        let b = l - r;
        let a = r + (b >> 1);
        sums[2] += a.abs() + b.abs();
    }
    let mut best_idx = 0;
    let mut best_val = sums[0];
    for (i, &val) in sums.iter().enumerate() {
        if val <= best_val {
            best_val = val;
            best_idx = i;
        }
    }
    match best_idx {
        0 => StereoMode::Normal,
        1 => StereoMode::LeftSide,
        _ => StereoMode::MidSide,
    }
}

fn apply_stereo(samp1: &mut [i32], samp2: &mut [i32], smode: StereoMode) {
    match smode {
        StereoMode::Normal => {},
        StereoMode::LeftSide => {
            for (l, r) in samp1.iter_mut().zip(samp2.iter_mut()) {
                *r = *l - *r;
            }
        },
        StereoMode::SideRight => {
            for (l, r) in samp1.iter_mut().zip(samp2.iter_mut()) {
                *l -= *r;
            }
        },
        StereoMode::MidSide => {
            for (l, r) in samp1.iter_mut().zip(samp2.iter_mut()) {
                let b = *l - *r;
                *l = *r + (b >> 1);
                *r = b;
            }
        },
    };
}

fn apply_fixed_filter(dst: &mut [u32], src: &[i32], order: i8) {
    match order {
        -1 => {
            let mut last = src[0];
            for (dst, &cur) in dst[1..].iter_mut().zip(src[1..].iter()) {
                *dst = to_unsigned(cur - last);
                last = cur;
            }
        },
        -2 => {
            let mut last0 = src[1];
            let mut last1 = src[0];
            for (dst, &cur) in dst[2..].iter_mut().zip(src[2..].iter()) {
                *dst = to_unsigned(cur - 2 * last0 + last1);
                last1 = last0;
                last0 = cur;
            }
        },
        -3 => {
            let mut last0 = src[2];
            let mut last1 = src[1];
            let mut last2 = src[0];
            for (dst, &cur) in dst[3..].iter_mut().zip(src[3..].iter()) {
                *dst = to_unsigned(cur - 3 * last0 + 3 * last1 - last2);
                last2 = last1;
                last1 = last0;
                last0 = cur;
            }
        },
        -4 => {
            let mut last0 = src[3];
            let mut last1 = src[2];
            let mut last2 = src[1];
            let mut last3 = src[0];
            for (dst, &cur) in dst[4..].iter_mut().zip(src[4..].iter()) {
                *dst = to_unsigned(cur - 4 * last0 + 6 * last1 - 4 * last2 + last3);
                last3 = last2;
                last2 = last1;
                last1 = last0;
                last0 = cur;
            }
        },
        _ => unreachable!(),
    };
}

fn apply_lpc(dst: &mut [u32], src: &[i32], filter: &[i32], shift: u8) {
    let order = filter.len();

    for (i, dst) in dst[order..src.len()].iter_mut().enumerate() {
        let sum = src[i..].iter().zip(filter.iter()).fold(0, |acc, (&c, &f)| acc + i64::from(c) * i64::from(f));
        *dst = to_unsigned(src[i + order] - ((sum >> shift) as i32));
    }
}

fn encode_residual(bw: &mut BitWriter, src: &[u32]) {
    let sum = src.iter().sum::<u32>() / (src.len() as u32);

    let k = (31 - sum.max(1).leading_zeros()) as u8;
    if k < 16 {
        bw.write(0, 2);
        bw.write(0, 4); // 1 partition
        bw.write(u32::from(k), 4);
    } else {
        bw.write(1, 2);
        bw.write(0, 4); // 1 partition
        bw.write(u32::from(k), 5);
    }
    for &samp in src.iter() {
        bw.write_code(UintCodeType::Rice(k), samp);
    }
}

#[derive(Default)]
struct FLACEncoder {
    stream:     Option<NAStreamRef>,
    channels:   usize,
    srate:      u32,
    block_len:  usize,
    order:      i8,
    samples:    Vec<Vec<i32>>,
    flush:      bool,
    cur_pos:    usize,
    tmp:        Vec<u32>,
    ffilter:    [f64; 32],
    ifilter:    [i32; 32],
}

#[allow(clippy::match_overlapping_arm)]
fn nsamples_code(nsamp: usize) -> u8 {
    match nsamp {
        192     => 1,
        576     => 2,
        1152    => 3,
        2304    => 4,
        4608    => 5,
        256     => 8,
        512     => 9,
        1024    => 10,
        2048    => 11,
        4096    => 12,
        8192    => 13,
        16384   => 14,
        32768   => 15,
        0..=255 => 6,
        _       => 7,
    }
}

fn encode_nsamples_esc(bw: &mut BitWriter, nsamp: usize, code: u8) {
    match code {
        6 => {
            bw.write((nsamp - 1) as u32, 8);
        },
        7 => {
            bw.write((nsamp - 1) as u32, 16);
        },
        _ => {},
    };
}

fn write_utf8(bw: &mut BitWriter, val: usize) {
    let val = val as u32;
    let lz = 32 - val.leading_zeros();
    match lz {
        0..=7  => {
            bw.write(val, 8);
        },
        8..=11 => {
            bw.write(0xC0 | (val >> 6),   8);
            bw.write(0x80 | (val & 0x3F), 8);
        },
        12..=16 => {
            bw.write(0xE0 | ( val >> 12),         8);
            bw.write(0x80 | ((val >>  6) & 0x3F), 8);
            bw.write(0x80 | ( val        & 0x3F), 8);
        },
        17..=21 => {
            bw.write(0xF0 | ( val >> 18),         8);
            bw.write(0x80 | ((val >> 12) & 0x3F), 8);
            bw.write(0x80 | ((val >>  6) & 0x3F), 8);
            bw.write(0x80 | ( val        & 0x3F), 8);
        },
        22..=26 => {
            bw.write(0xF8 | ( val >> 24),         8);
            bw.write(0x80 | ((val >> 18) & 0x3F), 8);
            bw.write(0x80 | ((val >> 12) & 0x3F), 8);
            bw.write(0x80 | ((val >>  6) & 0x3F), 8);
            bw.write(0x80 | ( val        & 0x3F), 8);
        },
        _ => {
            bw.write(0xFC | ( val >> 30),         8);
            bw.write(0x80 | ((val >> 24) & 0x3F), 8);
            bw.write(0x80 | ((val >> 18) & 0x3F), 8);
            bw.write(0x80 | ((val >> 12) & 0x3F), 8);
            bw.write(0x80 | ((val >>  6) & 0x3F), 8);
            bw.write(0x80 | ( val        & 0x3F), 8);
        },
    };
}

const DEFAULT_BLOCK_LEN: usize = 4096;

impl FLACEncoder {
    fn new() -> Self { Self::default() }
    fn encode_packet(&mut self) -> EncoderResult<NAPacket> {
        if self.samples[0].is_empty() || (!self.flush && self.samples[0].len() < self.block_len) {
            return Err(EncoderError::TryAgain);
        }

        let nsamples = self.samples[0].len().min(self.block_len);

        let mut bw = BitWriter::new(Vec::with_capacity(1024), BitWriterMode::BE);
        bw.write(0x3FFE, 14);
        bw.write0(); // reserved
        bw.write1(); // blocking strategy - variable
        let scode = nsamples_code(nsamples);
        bw.write(u32::from(scode), 4);
        bw.write(0x0, 4); // sample rate - read the stream info
        let smode = if self.channels == 2 {
                let (l, r) = self.samples.split_at_mut(1);
                let smode = find_stereo_mode(&l[0][..nsamples], &r[0][..nsamples]);
                apply_stereo(&mut l[0][..nsamples], &mut r[0][..nsamples], smode);
                smode
            } else {
                StereoMode::Normal
            };
        let chan_idx = match smode {
                StereoMode::Normal    => (self.channels as u32) - 1,
                StereoMode::LeftSide  => 8,
                StereoMode::SideRight => 9,
                StereoMode::MidSide   => 10,
            };
        bw.write(chan_idx, 4);
        bw.write(0x4, 3); // 16 bits per sample
        bw.write0(); // reserved
        write_utf8(&mut bw, self.cur_pos);
        encode_nsamples_esc(&mut bw, nsamples, scode);
        // optional bits per sample
        bw.write(0x00, 8); // header CRC placeholder
        let hdr_crc_pos = bw.tell() >> 3;
        for chan in 0..self.channels {
            let samp_bits = match (smode, chan) {
                    (StereoMode::LeftSide,  1) |
                    (StereoMode::SideRight, 0) |
                    (StereoMode::MidSide,   1) => 16 + 1,
                    _ => 16,
                };
            self.encode_channel(&mut bw, chan, nsamples, samp_bits);
        }

        let mut dbuf = bw.end();
        calc_header_crc(&mut dbuf[..hdr_crc_pos]);
        calc_frame_crc(&mut dbuf);
        let ts = NATimeInfo::new(Some(self.cur_pos as u64), None, Some(nsamples as u64), 1, self.srate);

        for samp in self.samples.iter_mut() {
            samp.drain(..nsamples);
        }
        self.cur_pos += nsamples;

        Ok(NAPacket::new(self.stream.clone().unwrap(), ts, true, dbuf))
    }
    fn encode_channel(&mut self, bw: &mut BitWriter, chan: usize, nsamples: usize, samp_bits: u8) {
        let samp = &self.samples[chan][..nsamples];

        let s0 = samp[0];
        let mut same = true;
        for &s in samp[1..].iter() {
            if s != s0 {
                same = false;
                break;
            }
        }

        bw.write0();
        if !same {
            match self.order {
                0 => {
                    bw.write(1, 6);
                    bw.write0(); // no wasted bits
                    for &el in samp.iter() {
                        bw.write_s(el, samp_bits);
                    }
                },
                -1 | -2 | -3 | -4 => {
                    let order = -self.order as usize;

                    apply_fixed_filter(&mut self.tmp, samp, self.order);

                    bw.write(8 | (order as u32), 6);
                    bw.write0(); // no wasted bits
                    for &el in samp[..order].iter() {
                        bw.write_s(el, samp_bits);
                    }
                    encode_residual(bw, &self.tmp[order..nsamples]);
                },
                _ => {
                    let order = self.order as usize;
                    calc_lpc_filter(samp, &mut self.ffilter[..order]);

                    let mut filter_prec = 12;
                    let scale = f64::from(1 << filter_prec);

                    let maxval = (self.ffilter[..order].iter().fold(0.0, |acc: f64, &el| acc.max(el.abs())) * scale) as i32;
                    let mut mask = 0;
                    for (dst, &src) in self.ifilter[..order].iter_mut().zip(&self.ffilter) {
                        *dst = (src * scale) as i32;
                        mask |= *dst;
                    }
                    let mut zbits = mask.trailing_zeros();
                    let mut filter_bits = 33 - maxval.leading_zeros() - zbits;
                    if filter_bits > 15 {
                        let sub = filter_bits - 15;
                        zbits += sub;
                        filter_bits = 15;
                    }
                    filter_prec -= zbits;
                    if zbits > 0 {
                        for el in self.ifilter[..order].iter_mut() {
                            *el >>= zbits;
                        }
                    }

                    apply_lpc(&mut self.tmp, samp, &self.ifilter[..order], filter_prec as u8);

                    bw.write(0x20 | ((order - 1) as u32), 6);
                    bw.write0(); // no wasted bits
                    for &el in samp[..order].iter() {
                        bw.write_s(el, samp_bits);
                    }
                    bw.write(filter_bits - 1, 4);
                    bw.write(filter_prec, 5);
                    for &coef in self.ifilter[..order].iter().rev() {
                        bw.write_s(coef, filter_bits as u8);
                    }
                    encode_residual(bw, &self.tmp[order..nsamples]);
                },
            };
        } else {
            bw.write(0, 6);
            bw.write0(); // no wasted bits
            bw.write(u32::from(s0 as u16), samp_bits);
        }
    }
}

impl NAEncoder for FLACEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Audio(NAAudioInfo::new(0, 1, SND_S16_FORMAT, DEFAULT_BLOCK_LEN)),
                    ..Default::default() })
            },
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(ainfo) => {
                let mut outinfo = ainfo;
                outinfo.channels = outinfo.channels.max(1).min(8);
                if outinfo.format != SND_S16P_FORMAT && outinfo.format != SND_S16_FORMAT {
                    outinfo.format = SND_S16P_FORMAT;
                }
                if outinfo.block_len == 0 {
                    outinfo.block_len = DEFAULT_BLOCK_LEN;
                }
                outinfo.block_len = outinfo.block_len.max(2).min(65535);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Audio(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { 0 }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(ainfo) => {
                if ainfo.format != SND_S16P_FORMAT && ainfo.format != SND_S16_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if ainfo.block_len == 0 {
                    self.block_len = DEFAULT_BLOCK_LEN;
                } else {
                    if ainfo.block_len < 2 || ainfo.block_len > 65535 {
                        return Err(EncoderError::FormatError);
                    }
                    self.block_len = ainfo.block_len;
                }
                if ainfo.channels == 0 || ainfo.channels > 8 {
                    return Err(EncoderError::FormatError);
                }
                self.channels = ainfo.channels as usize;

                let soniton = SND_S16P_FORMAT;
                let out_ainfo = NAAudioInfo::new(ainfo.sample_rate, ainfo.channels, soniton, self.block_len);
                let info = NACodecInfo::new("flac", NACodecTypeInfo::Audio(out_ainfo), None);
                let mut stream = NAStream::new(StreamType::Audio, stream_id, info, 1, ainfo.sample_rate, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());
                self.srate = ainfo.sample_rate;

                self.samples.clear();
                for _ in 0..self.channels {
                    self.samples.push(Vec::with_capacity(DEFAULT_BLOCK_LEN));
                }
                self.tmp.resize(self.block_len, 0);

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        if let Some(ref abuf) = buf.get_abuf_i16() {
            let src = abuf.get_data();
            let len = abuf.get_length();
            if abuf.get_step() == 1 {
                let astride = abuf.get_stride();
                let mut off = 0;
                for dst in self.samples.iter_mut() {
                    dst.reserve(len);
                    for &samp in src[off..][..len].iter() {
                        dst.push(i32::from(samp));
                    }
                    off += astride;
                }
            } else {
                for dst in self.samples.iter_mut() {
                    dst.reserve(len);
                }
                let mut src = src.iter();
                for _ in 0..len {
                    for dst in self.samples.iter_mut() {
                        dst.push(i32::from(*src.next().unwrap()));
                    }
                }
            }
            Ok(())
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        if let Ok(pkt) = self.encode_packet() {
            Ok(Some(pkt))
        } else {
            Ok(None)
        }
    }
    fn flush(&mut self) -> EncoderResult<()> {
        self.flush = true;
        Ok(())
    }
}

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: "order", description: "LPC order",
        opt_type: NAOptionDefinitionType::Int(Some(-4), Some(32)) },
    NAOptionDefinition {
        name: "block_size", description: "block size",
        opt_type: NAOptionDefinitionType::Int(Some(2), Some(65535)) },
];

impl NAOptionHandler for FLACEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        "order" => {
                            if let NAValue::Int(val) = option.value {
                                self.order = val as i8;
                            }
                        },
                        "block_size" => {
                            if let NAValue::Int(val) = option.value {
                                self.block_len = val as usize;
                                self.tmp.resize(self.block_len, 0);
                            }
                        },
                        _ => {},
                    };
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            "order" => Some(NAValue::Int(i64::from(self.order))),
            "block_size" => Some(NAValue::Int(self.block_len as i64)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(FLACEncoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_flac_encoder_verbatim() {
        let enc_options = &[
                NAOption{name: "order", value: NAValue::Int(0)},
            ];
        test_flac_encoder("uncompr.flac", enc_options,
                          &[0xcd9767ee, 0xf8a86d20, 0x317944aa, 0xd9044f5c]);
    }
    #[test]
    fn test_flac_encoder_fixed1() {
        let enc_options = &[
                NAOption{name: "order", value: NAValue::Int(-1)},
            ];
        test_flac_encoder("fixed1.flac", enc_options,
                          &[0xcf654f93, 0x1db0bb07, 0xbc0cd9c3, 0xf9d2cc4e]);
    }
    #[test]
    fn test_flac_encoder_fixed4() {
        let enc_options = &[
                NAOption{name: "order", value: NAValue::Int(-4)},
            ];
        test_flac_encoder("fixed1.flac", enc_options,
                          &[0xbc2fba7f, 0x6f9406ee, 0x98be1f67, 0xe2b86b2d]);
    }
    #[test]
    fn test_flac_encoder_lpc10() {
        let enc_options = &[
                NAOption{name: "order", value: NAValue::Int(10)},
            ];
        test_flac_encoder("lpc10.flac", enc_options,
                          &[0xb6736f2e, 0xb61dda7a, 0xeb4037db, 0x8ee4afb9]);
    }

    fn test_flac_encoder(name: &'static str, enc_options: &[NAOption], hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        llaudio_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        llaudio_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.flac
        let dec_config = DecoderTestParams {
                demuxer:        "flac",
                in_name:        "assets/LLaudio/luckynight.flac",
                stream_type:    StreamType::Audio,
                limit:          Some(10),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "flac",
                enc_name:       "flac",
                out_name:       name,
                mux_reg, enc_reg,
            };
        let dst_ainfo = NAAudioInfo {
                sample_rate:    0,
                channels:       0,
                format:         SND_S16_FORMAT,
                block_len:      0,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Audio(dst_ainfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
//        test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);

        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          hash);
    }
}

const CRC8_TABLE: [u8; 256] = [
    0x00, 0x07, 0x0E, 0x09, 0x1C, 0x1B, 0x12, 0x15,
    0x38, 0x3F, 0x36, 0x31, 0x24, 0x23, 0x2A, 0x2D,
    0x70, 0x77, 0x7E, 0x79, 0x6C, 0x6B, 0x62, 0x65,
    0x48, 0x4F, 0x46, 0x41, 0x54, 0x53, 0x5A, 0x5D,
    0xE0, 0xE7, 0xEE, 0xE9, 0xFC, 0xFB, 0xF2, 0xF5,
    0xD8, 0xDF, 0xD6, 0xD1, 0xC4, 0xC3, 0xCA, 0xCD,
    0x90, 0x97, 0x9E, 0x99, 0x8C, 0x8B, 0x82, 0x85,
    0xA8, 0xAF, 0xA6, 0xA1, 0xB4, 0xB3, 0xBA, 0xBD,
    0xC7, 0xC0, 0xC9, 0xCE, 0xDB, 0xDC, 0xD5, 0xD2,
    0xFF, 0xF8, 0xF1, 0xF6, 0xE3, 0xE4, 0xED, 0xEA,
    0xB7, 0xB0, 0xB9, 0xBE, 0xAB, 0xAC, 0xA5, 0xA2,
    0x8F, 0x88, 0x81, 0x86, 0x93, 0x94, 0x9D, 0x9A,
    0x27, 0x20, 0x29, 0x2E, 0x3B, 0x3C, 0x35, 0x32,
    0x1F, 0x18, 0x11, 0x16, 0x03, 0x04, 0x0D, 0x0A,
    0x57, 0x50, 0x59, 0x5E, 0x4B, 0x4C, 0x45, 0x42,
    0x6F, 0x68, 0x61, 0x66, 0x73, 0x74, 0x7D, 0x7A,
    0x89, 0x8E, 0x87, 0x80, 0x95, 0x92, 0x9B, 0x9C,
    0xB1, 0xB6, 0xBF, 0xB8, 0xAD, 0xAA, 0xA3, 0xA4,
    0xF9, 0xFE, 0xF7, 0xF0, 0xE5, 0xE2, 0xEB, 0xEC,
    0xC1, 0xC6, 0xCF, 0xC8, 0xDD, 0xDA, 0xD3, 0xD4,
    0x69, 0x6E, 0x67, 0x60, 0x75, 0x72, 0x7B, 0x7C,
    0x51, 0x56, 0x5F, 0x58, 0x4D, 0x4A, 0x43, 0x44,
    0x19, 0x1E, 0x17, 0x10, 0x05, 0x02, 0x0B, 0x0C,
    0x21, 0x26, 0x2F, 0x28, 0x3D, 0x3A, 0x33, 0x34,
    0x4E, 0x49, 0x40, 0x47, 0x52, 0x55, 0x5C, 0x5B,
    0x76, 0x71, 0x78, 0x7F, 0x6A, 0x6D, 0x64, 0x63,
    0x3E, 0x39, 0x30, 0x37, 0x22, 0x25, 0x2C, 0x2B,
    0x06, 0x01, 0x08, 0x0F, 0x1A, 0x1D, 0x14, 0x13,
    0xAE, 0xA9, 0xA0, 0xA7, 0xB2, 0xB5, 0xBC, 0xBB,
    0x96, 0x91, 0x98, 0x9F, 0x8A, 0x8D, 0x84, 0x83,
    0xDE, 0xD9, 0xD0, 0xD7, 0xC2, 0xC5, 0xCC, 0xCB,
    0xE6, 0xE1, 0xE8, 0xEF, 0xFA, 0xFD, 0xF4, 0xF3
];

const CRC16_TABLE: [u16; 256] = [
    0x0000, 0x8005, 0x800F, 0x000A, 0x801B, 0x001E, 0x0014, 0x8011,
    0x8033, 0x0036, 0x003C, 0x8039, 0x0028, 0x802D, 0x8027, 0x0022,
    0x8063, 0x0066, 0x006C, 0x8069, 0x0078, 0x807D, 0x8077, 0x0072,
    0x0050, 0x8055, 0x805F, 0x005A, 0x804B, 0x004E, 0x0044, 0x8041,
    0x80C3, 0x00C6, 0x00CC, 0x80C9, 0x00D8, 0x80DD, 0x80D7, 0x00D2,
    0x00F0, 0x80F5, 0x80FF, 0x00FA, 0x80EB, 0x00EE, 0x00E4, 0x80E1,
    0x00A0, 0x80A5, 0x80AF, 0x00AA, 0x80BB, 0x00BE, 0x00B4, 0x80B1,
    0x8093, 0x0096, 0x009C, 0x8099, 0x0088, 0x808D, 0x8087, 0x0082,
    0x8183, 0x0186, 0x018C, 0x8189, 0x0198, 0x819D, 0x8197, 0x0192,
    0x01B0, 0x81B5, 0x81BF, 0x01BA, 0x81AB, 0x01AE, 0x01A4, 0x81A1,
    0x01E0, 0x81E5, 0x81EF, 0x01EA, 0x81FB, 0x01FE, 0x01F4, 0x81F1,
    0x81D3, 0x01D6, 0x01DC, 0x81D9, 0x01C8, 0x81CD, 0x81C7, 0x01C2,
    0x0140, 0x8145, 0x814F, 0x014A, 0x815B, 0x015E, 0x0154, 0x8151,
    0x8173, 0x0176, 0x017C, 0x8179, 0x0168, 0x816D, 0x8167, 0x0162,
    0x8123, 0x0126, 0x012C, 0x8129, 0x0138, 0x813D, 0x8137, 0x0132,
    0x0110, 0x8115, 0x811F, 0x011A, 0x810B, 0x010E, 0x0104, 0x8101,
    0x8303, 0x0306, 0x030C, 0x8309, 0x0318, 0x831D, 0x8317, 0x0312,
    0x0330, 0x8335, 0x833F, 0x033A, 0x832B, 0x032E, 0x0324, 0x8321,
    0x0360, 0x8365, 0x836F, 0x036A, 0x837B, 0x037E, 0x0374, 0x8371,
    0x8353, 0x0356, 0x035C, 0x8359, 0x0348, 0x834D, 0x8347, 0x0342,
    0x03C0, 0x83C5, 0x83CF, 0x03CA, 0x83DB, 0x03DE, 0x03D4, 0x83D1,
    0x83F3, 0x03F6, 0x03FC, 0x83F9, 0x03E8, 0x83ED, 0x83E7, 0x03E2,
    0x83A3, 0x03A6, 0x03AC, 0x83A9, 0x03B8, 0x83BD, 0x83B7, 0x03B2,
    0x0390, 0x8395, 0x839F, 0x039A, 0x838B, 0x038E, 0x0384, 0x8381,
    0x0280, 0x8285, 0x828F, 0x028A, 0x829B, 0x029E, 0x0294, 0x8291,
    0x82B3, 0x02B6, 0x02BC, 0x82B9, 0x02A8, 0x82AD, 0x82A7, 0x02A2,
    0x82E3, 0x02E6, 0x02EC, 0x82E9, 0x02F8, 0x82FD, 0x82F7, 0x02F2,
    0x02D0, 0x82D5, 0x82DF, 0x02DA, 0x82CB, 0x02CE, 0x02C4, 0x82C1,
    0x8243, 0x0246, 0x024C, 0x8249, 0x0258, 0x825D, 0x8257, 0x0252,
    0x0270, 0x8275, 0x827F, 0x027A, 0x826B, 0x026E, 0x0264, 0x8261,
    0x0220, 0x8225, 0x822F, 0x022A, 0x823B, 0x023E, 0x0234, 0x8231,
    0x8213, 0x0216, 0x021C, 0x8219, 0x0208, 0x820D, 0x8207, 0x0202
];
