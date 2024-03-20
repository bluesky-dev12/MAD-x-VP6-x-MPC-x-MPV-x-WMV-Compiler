use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::intcode::*;

#[derive(Default)]
struct Filter {
    predictor:  i32,
    error:      i32,
    round:      i32,
    shift:      u8,
    qm:         [i32; 8],
    dx:         [i32; 8],
    dl:         [i32; 8],
}

impl Filter {
    fn reset(&mut self, bpp: u8) {
        const SHIFTS: [u8; 3] = [10, 9, 10];
        self.shift = SHIFTS[(bpp - 1) as usize];
        self.round = (1 << self.shift) >> 1;
        self.error = 0;
        self.qm = [0; 8];
        self.dx = [0; 8];
        self.dl = [0; 8];
        self.predictor = 0;
    }
    fn hybrid_filt(&mut self, delta: i32) -> i32 {
        if self.error < 0 {
            for (qm, dx) in self.qm.iter_mut().zip(self.dx.iter()) {
                *qm -= *dx;
            }
        } else if self.error > 0 {
            for (qm, dx) in self.qm.iter_mut().zip(self.dx.iter()) {
                *qm += *dx;
            }
        }

        let mut sum = self.round;
        for (dl, qm) in self.dl.iter().zip(self.qm.iter()) {
            sum = sum.wrapping_add(*dl * *qm);
        }
        self.error = delta;
        let val = (sum >> self.shift) + delta;

        for i in 0..4 {
            self.dx[i] = self.dx[i + 1];
            self.dl[i] = self.dl[i + 1];
        }
        self.dx[4] =  (self.dl[4] >> 30) | 1;
        self.dx[5] = ((self.dl[5] >> 30) | 2) & !1;
        self.dx[6] = ((self.dl[6] >> 30) | 2) & !1;
        self.dx[7] = ((self.dl[7] >> 30) | 4) & !3;
        self.dl[4] = -self.dl[5];
        self.dl[5] = -self.dl[6];
        self.dl[6] = val - self.dl[7];
        self.dl[7] = val;
        self.dl[5] += self.dl[6];
        self.dl[4] += self.dl[5];

        val
    }
    fn static_pred(&mut self, bpp: u8, mut val: i32) -> i32 {
        val += match bpp {
                0     => ((i64::from(self.predictor) * 15) >> 4) as i32,
                1 | 2 => ((i64::from(self.predictor) * 31) >> 5) as i32,
                _     => self.predictor,
            };
        self.predictor = val;
        val
    }
}

struct RiceDecoder {
    k:      u8,
    sum:    u32,
}

impl RiceDecoder {
    fn new() -> Self {
        let k = 10;
        Self {
            k, sum: RiceDecoder::limit(k)
        }
    }
    fn reset(&mut self) {
        self.k = 10;
        self.sum = RiceDecoder::limit(self.k);
    }
    fn limit(k: u8) -> u32 { 1 << (k + 4).min(31) }
    fn update(&mut self, val: u32) {
        self.sum -= self.sum >> 4;
        self.sum += val;
        if self.k > 0 && self.sum < Self::limit(self.k) {
            self.k -= 1;
        } else if self.sum > Self::limit(self.k + 1) {
            self.k += 1;
        }
    }
}

trait Output {
    fn set(&mut self, val: i32);
}

impl Output for i16 {
    fn set(&mut self, val: i32) { *self = val as i16; }
}
impl Output for i32 {
    fn set(&mut self, val: i32) { *self = val; }
}

struct ChannelDecoder {
    filt:       Filter,
    rice0:      RiceDecoder,
    rice1:      RiceDecoder,
    offset:     usize,
    sample:     i32,
}

impl ChannelDecoder {
    fn new() -> Self {
        Self {
            filt:       Filter::default(),
            rice0:      RiceDecoder::new(),
            rice1:      RiceDecoder::new(),
            offset:     0,
            sample:     0,
        }
    }
}

struct TTADecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    bpp:        u8,
    framelen:   u32,
    nsamples:   u32,
    ch_dec:     Vec<ChannelDecoder>,
}

impl TTADecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            chmap:      NAChannelMap::new(),
            bpp:        0,
            framelen:   0,
            nsamples:   0,
            ch_dec:     Vec::new(),
        }
    }
    fn decode_frame<T: Output>(&mut self, br: &mut BitReader, dst: &mut [T], stride: usize) -> DecoderResult<bool> {
        for (i, chdec) in self.ch_dec.iter_mut().enumerate() {
            chdec.offset = i * stride;
            chdec.rice0.reset();
            chdec.rice1.reset();
            chdec.filt.reset(self.bpp);
        }

        let channels = self.ch_dec.len();
        let tail_len = self.nsamples % self.framelen;

        for sample in 0..self.framelen {
            for chdec in self.ch_dec.iter_mut() {
                let pfx                 = br.read_code(UintCodeType::UnaryOnes)?;
                let (k, pfx, level1) = if pfx == 0 {
                        (chdec.rice0.k, 0, false)
                    } else {
                        (chdec.rice1.k, pfx - 1, true)
                    };
                let mut val             = (pfx << k) | br.read(k)?;
                if level1 {
                    chdec.rice1.update(val);
                    val += 1 << chdec.rice0.k;
                }
                chdec.rice0.update(val);
                let delta = if (val & 1) == 0 {
                        -((val >> 1) as i32)
                    } else {
                        ((val + 1) >> 1) as i32
                    };
                let hval = chdec.filt.hybrid_filt(delta);
                chdec.sample = chdec.filt.static_pred(self.bpp, hval);
            }
            if channels > 1 {
                self.ch_dec[channels - 1].sample += self.ch_dec[channels - 2].sample / 2;
                let mut last = self.ch_dec[channels - 1].sample;
                for chdec in self.ch_dec.iter_mut().rev().skip(1) {
                    chdec.sample = last - chdec.sample;
                    last = chdec.sample;
                }
            }
            for chdec in self.ch_dec.iter_mut() {
                dst[chdec.offset].set(chdec.sample);
                chdec.offset += 1;
            }
            if (tail_len > 0) && (sample == tail_len - 1) && (br.left() < 40) {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

fn check_crc(buf: &[u8]) -> bool {
    if buf.len() <= 4 {
        return false;
    }
    let mut crc = 0xFFFFFFFF;
    let ref_crc = read_u32le(&buf[buf.len() - 4..]).unwrap_or(0);
    for el in buf.iter().take(buf.len() - 4) {
        crc = CRC32_TAB[(crc as u8 ^ *el) as usize] ^ (crc >> 8);
    }
    crc == !ref_crc
}

impl NADecoder for TTADecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(_ainfo) = info.get_properties() {
            if let Some(buf) = info.get_extradata() {
                if !check_crc(&buf) {
                    return Err(DecoderError::ChecksumError);
                }
                let mut mr = MemoryReader::new_read(&buf);
                let mut br = ByteReader::new(&mut mr);
                let tag                 = br.read_tag()?;
                validate!(&tag == b"TTA1");
                let afmt                = br.read_u16le()?;
                if afmt != 1 {
                    return Err(DecoderError::NotImplemented);
                }
                let channels            = br.read_u16le()?;
                validate!(channels > 0 && channels < 256);
                let bpp                 = br.read_u16le()?;
                validate!(bpp > 0 && bpp <= 32);
                let srate               = br.read_u32le()?;
                validate!(srate > 256 && srate < 1048576);
                self.nsamples           = br.read_u32le()?;
                validate!(self.nsamples > 0);

                self.framelen = srate * 256 / 245;

                self.chmap = if channels == 1 {
                        NAChannelMap::from_str("C").unwrap()
                    } else if channels == 2 {
                        NAChannelMap::from_str("L,R").unwrap()
                    } else {
                        return Err(DecoderError::NotImplemented);
                    };
                let fmt = match bpp {
                         8 | 16 => SND_S16P_FORMAT,
                        24 | 32 => SND_S32P_FORMAT,
                        _ => return Err(DecoderError::NotImplemented),
                    };
                self.bpp = (bpp / 8) as u8;
                self.ch_dec = Vec::with_capacity(channels as usize);
                for _ in 0..channels {
                    self.ch_dec.push(ChannelDecoder::new());
                }

                self.ainfo = NAAudioInfo::new(srate, channels as u8, fmt, self.framelen as usize);
                Ok(())
            } else {
                Err(DecoderError::InvalidData)
            }
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            validate!(pktbuf.len() > 4);
            if !check_crc(&pktbuf) {
                return Err(DecoderError::ChecksumError);
            }

            let mut br = BitReader::new(&pktbuf, BitReaderMode::LE);

            let mut abuf = alloc_audio_buffer(self.ainfo, self.framelen as usize, self.chmap.clone())?;
            let duration = match abuf {
                    NABufferType::AudioI16(ref mut adata) => {
                        let stride = adata.get_stride();
                        let dst = adata.get_data_mut().unwrap();
                        let not_last = self.decode_frame(&mut br, dst, stride)?;
                        if not_last {
                            self.framelen
                        } else {
                            self.nsamples % self.framelen
                        }
                    },
                    NABufferType::AudioI32(ref mut adata) => {
                        let stride = adata.get_stride();
                        let dst = adata.get_data_mut().unwrap();
                        let not_last = self.decode_frame(&mut br, dst, stride)?;
                        if not_last {
                            self.framelen
                        } else {
                            self.nsamples % self.framelen
                        }
                    },
                    _ => unreachable!(),
                };
            abuf.truncate_audio(duration as usize);

            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
            frm.set_duration(Some(u64::from(duration)));
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for TTADecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(TTADecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::llaudio_register_all_decoders;
    use crate::llaudio_register_all_demuxers;
    #[test]
    fn test_tta() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.tta
        test_decoding("tta", "tta", "assets/LLaudio/luckynight.tta", Some(3), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xce0fe9c4, 0xa69eefda, 0xe182008c, 0xe941db3f]));
    }
}

const CRC32_TAB: [u32; 256] = [
    0x00000000, 0x77073096, 0xee0e612c, 0x990951ba,
    0x076dc419, 0x706af48f, 0xe963a535, 0x9e6495a3,
    0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
    0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,
    0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
    0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
    0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec,
    0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5,
    0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
    0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
    0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940,
    0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
    0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116,
    0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f,
    0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
    0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,
    0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a,
    0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
    0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818,
    0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
    0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
    0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457,
    0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c,
    0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
    0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
    0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb,
    0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
    0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,
    0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086,
    0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
    0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4,
    0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad,
    0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
    0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683,
    0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
    0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
    0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe,
    0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7,
    0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
    0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
    0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252,
    0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
    0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60,
    0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79,
    0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
    0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f,
    0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04,
    0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
    0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a,
    0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
    0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
    0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,
    0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e,
    0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
    0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
    0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45,
    0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
    0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db,
    0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0,
    0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
    0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6,
    0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf,
    0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
    0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
];
