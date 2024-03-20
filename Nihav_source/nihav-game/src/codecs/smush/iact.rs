use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use std::str::FromStr;

struct IACTDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    bits:       u8,
    tot_size:   u32,
    old:        bool,
    queued:     Vec<u8>,
}

impl IACTDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16_FORMAT, 0),
            chmap:      NAChannelMap::new(),
            bits:       0,
            tot_size:   0,
            old:        false,
            queued:     Vec::new(),
        }
    }
}

impl NADecoder for IACTDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), ainfo.get_channels(), SND_S16_FORMAT, 1);
            self.chmap = NAChannelMap::from_str(if ainfo.get_channels() == 1 { "C" } else { "L,R" }).unwrap();
            self.bits  = ainfo.get_format().bits;
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let src = pkt.get_buffer();
            validate!(src.len() > 18);

            let code  = read_u16le(&src[0..])?;
            let flags = read_u16le(&src[2..])?;
            if code != 8 || flags != 0x2E {
                let mut frm = NAFrame::new_from_pkt(pkt, info, NABufferType::None);
                frm.set_duration(Some(0));
                frm.set_keyframe(true);
                return Ok(frm.into_ref());
            }
            let _left = read_u32le(&src[14..])?;
            let offset = if self.tot_size == 0 {
                    let mut mr = MemoryReader::new_read(&src[18..]);
                    let mut br = ByteReader::new(&mut mr);
                    let tag             = br.read_tag()?;
                    if &tag == b"iMUS" {
                        self.tot_size   = br.read_u32be()?;
                        validate!(self.tot_size != 0);
                        loop {
                            let tag     = br.read_tag()?;
                            let size    = br.read_u32be()?;
                            match &tag {
                                b"DATA" => {
                                    break;
                                },
                                _ =>      br.read_skip(size as usize)?,
                            };
                        }
                        br.tell() as usize
                    } else {
                        self.tot_size = 1;
                        self.old = true;
                        self.bits = 8;
                        0
                    }
                } else { 0 };

            let data = &src[offset + 18..];
            if self.old {
                self.queued.extend_from_slice(data);
            }
            let nsamples = (match self.bits {
                    _ if self.old => {
                        let mut mr = MemoryReader::new_read(&self.queued);
                        let mut br = ByteReader::new(&mut mr);
                        let mut nblocks = 0;
                        while br.left() > 0 {
                            let len     = br.read_u16be()? as usize;
                            if br.left() < (len as i64) {
                                break;
                            }
                            nblocks += 1;
                                          br.read_skip(len)?;
                        }
                        nblocks * 1024 * self.chmap.num_channels()
                    },
                     8 => data.len(),
                    12 => data.len() * 2 / 3,
                    16 => data.len() / 2,
                    _ => unimplemented!(),
                }) / self.chmap.num_channels();

            let abuf = alloc_audio_buffer(self.ainfo, nsamples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let adata = adata.get_data_mut().unwrap();
            match self.bits {
                _ if self.old => {
                    let mut mr = MemoryReader::new_read(&self.queued);
                    let mut br = ByteReader::new(&mut mr);
                    for dst in adata.chunks_exact_mut(1024 * 2) {
                        let len         = br.read_u16be()? as usize;
                        let end = br.tell() + (len as u64);
                        let b           = br.read_byte()?;
                        let scale1 = b >> 4;
                        let scale2 = b & 0xF;
                        for pair in dst.chunks_exact_mut(2) {
                            if br.left() < 2 {
                                break;
                            }
                            let b       = br.read_byte()? as i8;
                            if b != -0x80 {
                                pair[0] = i16::from(b) << scale1;
                            } else {
                                pair[0] = br.read_u16be()? as i16;
                            }
                            let b       = br.read_byte()? as i8;
                            if b != -0x80 {
                                pair[1] = i16::from(b) << scale2;
                            } else {
                                pair[1] = br.read_u16be()? as i16;
                            }
                        }
                        validate!(br.tell() <= end);
                                          br.seek(SeekFrom::Start(end))?;
                    }
                    let consumed = br.tell() as usize;
                    self.queued.drain(..consumed);
                },
                8 => {
                    for (dst, &src) in adata.iter_mut().zip(data.iter()) {
                        *dst = (u16::from(src) << 8) as i16;
                    }
                },
                12 => {
                    for (dst, src) in adata.chunks_exact_mut(2).zip(data.chunks_exact(3)) {
                        dst[0] = (((u16::from(src[1] << 4) << 8) | (u16::from(src[0]) << 4)) ^ 0x8000) as i16;
                        dst[1] = (((u16::from(src[1] & 0xF0) << 8) | (u16::from(src[2]) << 4)) ^ 0x8000) as i16;
                    }
                },
                16 => {
                    for (dst, src) in adata.iter_mut().zip(data.chunks_exact(2)) {
                        *dst = read_u16le(src)? as i16;
                    }
                },
                _ => unreachable!(),
            }

            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
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

impl NAOptionHandler for IACTDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_iact() -> Box<dyn NADecoder + Send> {
    Box::new(IACTDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    #[test]
    fn test_smush_iact_imuse() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from The Dig
        test_decoding("smush", "smush-iact", "assets/Game/smush/PIGOUT.SAN", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x7d731a75, 0x9869cb8f, 0xded5b893, 0xb507b17a]));
    }
    #[test]
    fn test_smush_iact_raw() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from Curse of Monkey Island
        test_decoding("smush", "smush-iact", "assets/Game/smush/ZAP010.SAN", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xecf88bc6, 0x5c89ce94, 0x8e40a22a, 0xfc4ba86c]));
    }
}

