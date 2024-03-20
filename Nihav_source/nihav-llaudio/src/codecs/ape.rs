use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;

use super::apepred::*;
use super::apereader::*;

struct APEDecoder {
    ainfo:          NAAudioInfo,
    chmap:          NAChannelMap,
    version:        u16,
    decode_mono:    fn(&mut Coder, &mut [i32]) -> DecoderResult<()>,
    decode_stereo:  fn(&mut Coder, &mut [i32], &mut [i32]) -> DecoderResult<()>,
    is_stereo:      bool,
    left:           Vec<i32>,
    right:          Vec<i32>,
    fmode:          FilterMode,
    data:           Vec<u8>,
    blocksperframe: usize,
}

impl APEDecoder {
    fn new() -> Self {
        Self {
            ainfo:          NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            chmap:          NAChannelMap::new(),
            version:        0,
            decode_mono:    decode_mono_dummy,
            decode_stereo:  decode_stereo_dummy,
            is_stereo:      false,
            left:           Vec::new(),
            right:          Vec::new(),
            fmode:          FilterMode::None,
            data:           Vec::new(),
            blocksperframe: 0,
        }
    }
}

impl NADecoder for APEDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(_ainfo) = info.get_properties() {
            if let Some(buf) = info.get_extradata() {
                let mut mr = MemoryReader::new_read(&buf);
                let mut br = ByteReader::new(&mut mr);
                let version             = br.read_u16le()?;
                let compression         = br.read_u16le()?;
                let _flags              = br.read_u16le()?;
                let channels            = br.read_byte()?;
                let bits                = br.read_byte()?;
                let srate               = br.read_u32le()?;
                let blocksperframe      = br.read_u32le()? as usize;

                validate!(channels > 0);
                validate!(bits > 0 && bits <= 32);
                validate!((compression % 1000) == 0 && compression > 0 && compression <= 5000);
                validate!(compression < 5000 || version >= 3930);
                if bits != 16 {
                    return Err(DecoderError::NotImplemented);
                }
                if version > 3990 {
                    return Err(DecoderError::NotImplemented);
                }

                self.version = version;
                self.blocksperframe = blocksperframe;
                self.is_stereo = channels == 2;
                self.left.resize(blocksperframe, 0);
                if self.is_stereo {
                    self.right.resize(blocksperframe, 0);
                }

                self.decode_mono = if version >= 3990 {
                        decode_mono_3990
                    } else if version >= 3910 {
                        decode_mono_3910
                    } else if version >= 3900 {
                        decode_mono_3900
                    } else if version >= 3890 {
                        decode_mono_3890
                    } else if version >= 3860 {
                        decode_mono_3860
                    } else {
                        decode_mono_0000
                    };
                self.decode_stereo = if version >= 3990 {
                        decode_stereo_3990
                    } else if version >= 3930 {
                        decode_stereo_3930
                    } else if version >= 3910 {
                        decode_stereo_3910
                    } else if version >= 3900 {
                        decode_stereo_3900
                    } else if version >= 3890 {
                        decode_stereo_3890
                    } else if version >= 3860 {
                        decode_stereo_3860
                    } else {
                        decode_stereo_0000
                    };
                self.fmode = FilterMode::new(version, compression);

                self.chmap = if channels == 1 {
                        NAChannelMap::from_str("C").unwrap()
                    } else if channels == 2 {
                        NAChannelMap::from_str("L,R").unwrap()
                    } else {
                        return Err(DecoderError::NotImplemented);
                    };
                self.ainfo = NAAudioInfo::new(srate, channels, SND_S16P_FORMAT, 4602);
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
            validate!(pktbuf.len() > 9);

            let nblocks                 = read_u32le(&pktbuf[0..])? as usize;
            validate!(nblocks > 0);
            let bits                    = u32::from(pktbuf[4]);
            validate!(bits < 32);
            self.data.clear();
            self.data.reserve((pktbuf.len() & !3) + 2);
            for word in pktbuf[8..].chunks_exact(4) {
                self.data.push(word[3]);
                self.data.push(word[2]);
                self.data.push(word[1]);
                self.data.push(word[0]);
            }
            if self.version < 3950 {
                self.data.push(0);
                self.data.push(0);
            }

            let (mut coder, ref_crc, fflags) = if self.version < 3900 {
                    let mut br = BitReader::new(&self.data, BitReaderMode::BE);
                                          br.skip(bits)?;
                    let mut crc         = br.read(32)?;
                    let fflags = if self.version >= 3830 && (crc & 0x80000000) != 0 {
                            crc ^= 0x80000000;
                                          br.read(32)?
                        } else {
                            0
                        };
                    (Coder::Rice(RiceCoder::new(br)), crc, fflags)
                } else {
                    let mut boff = (bits / 8) as usize;
                    let mut crc         = read_u32be(&self.data[boff..])?;
                    boff += 4;
                    let fflags = if (crc & 0x80000000) != 0 {
                            crc ^= 0x80000000;
                            let flg     = read_u32be(&self.data[boff..])?;
                            boff += 4;
                            flg
                        } else {
                            0
                        };
                    // it ignores first byte anyway
                    (Coder::Range(RangeCoder::new(&self.data[boff + 1..])), crc, fflags)
                };
            self.left.resize(nblocks, 0);
            if self.is_stereo {
                self.right.resize(nblocks, 0);
            }
            if (!self.is_stereo && (fflags & 1) == 0) || (self.is_stereo && (fflags & 3) != 3) {
                if !self.is_stereo || (fflags & 4) != 0 {
                    (self.decode_mono)(&mut coder, &mut self.left)?;
                    self.fmode.filter_mono(&mut self.left);

                    if (fflags & 4) != 0 {
                        self.right.copy_from_slice(&self.left);
                    }
                } else {
                    (self.decode_stereo)(&mut coder, &mut self.left, &mut self.right)?;
                    self.fmode.filter_stereo(&mut self.left, &mut self.right);
                }
            } else {
                for l in self.left.iter_mut() { *l = 0; }
                if self.is_stereo {
                    for r in self.right.iter_mut() { *r = 0; }
                }
            }

            if self.version >= 0x3990 || nblocks == self.blocksperframe {
                let mut crc = 0xFFFFFFFF;
                if !self.is_stereo {
                    for el in self.left.iter() {
                        let byte1 = *el as u8;
                        let byte0 = (*el >> 8) as u8;
                        crc = (crc >> 8) ^ CRC32_TAB[((crc as u8) ^ byte1) as usize];
                        crc = (crc >> 8) ^ CRC32_TAB[((crc as u8) ^ byte0) as usize];
                    }
                } else {
                    for (l, r) in self.left.iter().zip(self.right.iter()) {
                        let byte1 = *l as u8;
                        let byte0 = (*l >> 8) as u8;
                        crc = (crc >> 8) ^ CRC32_TAB[((crc as u8) ^ byte1) as usize];
                        crc = (crc >> 8) ^ CRC32_TAB[((crc as u8) ^ byte0) as usize];
                        let byte1 = *r as u8;
                        let byte0 = (*r >> 8) as u8;
                        crc = (crc >> 8) ^ CRC32_TAB[((crc as u8) ^ byte1) as usize];
                        crc = (crc >> 8) ^ CRC32_TAB[((crc as u8) ^ byte0) as usize];
                    }
                }
                crc = !crc;
                if self.version >= 3830 {
                    crc >>= 1;
                }
                if crc != ref_crc {
                    return Err(DecoderError::ChecksumError);
                }
            }

            let abuf = alloc_audio_buffer(self.ainfo, nblocks, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let off1 = adata.get_offset(1);
            let dst = adata.get_data_mut().unwrap();
            let (left, right) = dst.split_at_mut(off1);
            for (dst, src) in left.iter_mut().zip(self.left.iter()) {
                *dst = *src as i16;
            }
            if self.is_stereo {
                for (dst, src) in right.iter_mut().zip(self.right.iter()) {
                    *dst = *src as i16;
                }
            }

            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
            frm.set_duration(Some(nblocks as u64));
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for APEDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(APEDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::llaudio_register_all_decoders;
    use crate::llaudio_register_all_demuxers;
    // samples from Libav test suite
    #[test]
    fn test_ape_3990() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("ape", "ape", "assets/LLaudio/ape/luckynight.ape", Some(3), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x569e002b, 0xd93772a9, 0x1cfd81cd, 0xad81319a]));
    }
    #[test]
    fn test_ape_3940() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("ape", "ape", "assets/LLaudio/ape/luckynight-mac394b1-c4000.ape", Some(1), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xeb55ece6, 0xe5f22759, 0xd0696dd6, 0x84ae9a6c]));
    }
    #[test]
    fn test_ape_3920() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("ape", "ape", "assets/LLaudio/ape/luckynight-mac392b2-c4000.ape", Some(1), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xeb55ece6, 0xe5f22759, 0xd0696dd6, 0x84ae9a6c]));
    }
    #[test]
    fn test_ape_3910() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("ape", "ape", "assets/LLaudio/ape/luckynight-mac391b1-c4000.ape", Some(1), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xeb55ece6, 0xe5f22759, 0xd0696dd6, 0x84ae9a6c]));
    }
    #[test]
    fn test_ape_3890() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("ape", "ape", "assets/LLaudio/ape/luckynight-mac389b1-c4000.ape", Some(1), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xeb55ece6, 0xe5f22759, 0xd0696dd6, 0x84ae9a6c]));
    }
    #[test]
    fn test_ape_3880() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("ape", "ape", "assets/LLaudio/ape/luckynight-mac388-c4000.ape", Some(1), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xeb55ece6, 0xe5f22759, 0xd0696dd6, 0x84ae9a6c]));
    }
    #[test]
    fn test_ape_3800() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("ape", "ape", "assets/LLaudio/ape/luckynight-mac380-c4000.ape", Some(1), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xeb55ece6, 0xe5f22759, 0xd0696dd6, 0x84ae9a6c]));
    }
}

const CRC32_TAB: [u32; 256] = [
    0x00000000, 0x77073096, 0xEE0E612C, 0x990951BA, 0x076DC419, 0x706AF48F, 0xE963A535, 0x9E6495A3,
    0x0EDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988, 0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91,
    0x1DB71064, 0x6AB020F2, 0xF3B97148, 0x84BE41DE, 0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7,
    0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC, 0x14015C4F, 0x63066CD9, 0xFA0F3D63, 0x8D080DF5,
    0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172, 0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B,
    0x35B5A8FA, 0x42B2986C, 0xDBBBC9D6, 0xACBCF940, 0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59,
    0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116, 0x21B4F4B5, 0x56B3C423, 0xCFBA9599, 0xB8BDA50F,
    0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924, 0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D,
    0x76DC4190, 0x01DB7106, 0x98D220BC, 0xEFD5102A, 0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433,
    0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818, 0x7F6A0DBB, 0x086D3D2D, 0x91646C97, 0xE6635C01,
    0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E, 0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457,
    0x65B0D9C6, 0x12B7E950, 0x8BBEB8EA, 0xFCB9887C, 0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65,
    0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2, 0x4ADFA541, 0x3DD895D7, 0xA4D1C46D, 0xD3D6F4FB,
    0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0, 0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9,
    0x5005713C, 0x270241AA, 0xBE0B1010, 0xC90C2086, 0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F,
    0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4, 0x59B33D17, 0x2EB40D81, 0xB7BD5C3B, 0xC0BA6CAD,
    0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A, 0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683,
    0xE3630B12, 0x94643B84, 0x0D6D6A3E, 0x7A6A5AA8, 0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1,
    0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE, 0xF762575D, 0x806567CB, 0x196C3671, 0x6E6B06E7,
    0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC, 0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5,
    0xD6D6A3E8, 0xA1D1937E, 0x38D8C2C4, 0x4FDFF252, 0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B,
    0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60, 0xDF60EFC3, 0xA867DF55, 0x316E8EEF, 0x4669BE79,
    0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236, 0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F,
    0xC5BA3BBE, 0xB2BD0B28, 0x2BB45A92, 0x5CB36A04, 0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D,
    0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A, 0x9C0906A9, 0xEB0E363F, 0x72076785, 0x05005713,
    0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38, 0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21,
    0x86D3D2D4, 0xF1D4E242, 0x68DDB3F8, 0x1FDA836E, 0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777,
    0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C, 0x8F659EFF, 0xF862AE69, 0x616BFFD3, 0x166CCF45,
    0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2, 0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB,
    0xAED16A4A, 0xD9D65ADC, 0x40DF0B66, 0x37D83BF0, 0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9,
    0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6, 0xBAD03605, 0xCDD70693, 0x54DE5729, 0x23D967BF,
    0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94, 0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D,
];
