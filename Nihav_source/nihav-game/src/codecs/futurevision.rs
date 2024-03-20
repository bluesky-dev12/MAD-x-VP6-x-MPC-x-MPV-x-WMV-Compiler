use nihav_core::frame::*;
use nihav_core::formats;
#[cfg(feature="decoder_fstaud")]
use nihav_core::formats::NAChannelMap;
use nihav_core::codecs::*;
#[cfg(feature="decoder_fstvid")]
use nihav_core::io::byteio::*;
#[cfg(feature="decoder_fstaud")]
use nihav_codec_support::codecs::imaadpcm::IMAState;

#[cfg(feature="decoder_fstvid")]
struct FutureVisionVideoDecoder {
    info:       NACodecInfoRef,
    pal:        [u8; 768],
    frame:      Vec<u8>,
    w:          usize,
    h:          usize,
}

#[cfg(feature="decoder_fstvid")]
struct Bits8<'a> {
    src:    &'a [u8],
    pos:    usize,
    buf:    u8,
    bit:    u8,
}

#[cfg(feature="decoder_fstvid")]
impl<'a> Bits8<'a> {
    fn new(src: &'a [u8]) -> Self { Bits8 { src, pos: 0, buf: 0, bit: 0 } }
    fn read_bit(&mut self) -> ByteIOResult<bool> {
        if self.bit == 0 {
            if self.pos < self.src.len() {
                self.buf = self.src[self.pos];
                self.pos += 1;
                self.bit = 8;
            } else {
                return Err(ByteIOError::ReadError);
            }
        }
        let bit = (self.buf & 0x80) != 0;
        self.buf <<= 1;
        self.bit -= 1;
        Ok(bit)
    }
}

#[cfg(feature="decoder_fstvid")]
impl FutureVisionVideoDecoder {
    fn new() -> Self {
        FutureVisionVideoDecoder {
            info:       NACodecInfoRef::default(),
            pal:        [0; 768],
            frame:      Vec::new(),
            w:          0,
            h:          0,
        }
    }

    fn output_frame(&mut self, bufinfo: &mut NABufferType, w: usize, h: usize) {
        let bufo = bufinfo.get_vbuf();
        let mut buf = bufo.unwrap();
        let paloff = buf.get_offset(1);
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        dst[paloff..][..768].copy_from_slice(&self.pal);
        for (dline, sline) in dst.chunks_mut(stride).zip(self.frame.chunks(w)).take(h) {
            dline[..w].copy_from_slice(sline);
        }
    }
}

#[cfg(feature="decoder_fstvid")]
impl NADecoder for FutureVisionVideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            validate!((w & 1) == 0 && (h & 1) == 0);
            let fmt = PAL8_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.w = w;
            self.h = h;

            self.frame.resize(w * h, 0);
            self.pal = [0; 768];
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() >= 4);

        let bitsize = read_u16le(&src)? as usize;
        let bsize = (bitsize + 8) >> 3;
        validate!(bsize + 2 <= src.len());

        let mut flags = Bits8::new(&src[2..][..bsize]);
        let mut mr = MemoryReader::new_read(&src[2 + bsize..]);
        let mut br = ByteReader::new(&mut mr);

        if (bsize + 2 != src.len()) && flags.read_bit()? {
            for dst in self.pal.iter_mut() {
                let b                   = br.read_byte()?;
                *dst = (b << 2) | (b >> 4);
            }
        }

        let mut is_intra = true;
        let stride = self.w;
        // for some reason last row should not be decoded
        for row4 in self.frame.chunks_mut(stride * 4).take(self.h / 4 - 1) {
            for x in (0..self.w).step_by(4) {
                if flags.read_bit()? {
                    if flags.read_bit()? {
                        let c0          = br.read_byte()?;
                        let c1          = br.read_byte()?;
                        let mut mask    = br.read_u16le()?;
                        for dst in row4[x..].chunks_mut(stride) {
                            for pix in dst.iter_mut().take(4) {
                                *pix = if (mask & 0x8000) != 0 { c1 } else { c0 };
                                mask <<= 1;
                            }
                        }
                    } else {
                        for dst in row4[x..].chunks_mut(stride) {
                                          br.read_buf(&mut dst[..4])?;
                        }
                    }
                } else {
                    is_intra = false;
                }
            }
        }

        let mut bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;

        self.output_frame(&mut bufinfo, self.w, self.h);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

#[cfg(feature="decoder_fstvid")]
impl NAOptionHandler for FutureVisionVideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

#[cfg(feature="decoder_fstvid")]
pub fn get_decoder_video() -> Box<dyn NADecoder + Send> {
    Box::new(FutureVisionVideoDecoder::new())
}

#[cfg(feature="decoder_fstaud")]
struct FutureVisionAudioDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    state:      IMAState,
    count:      usize,
}

#[cfg(feature="decoder_fstaud")]
impl FutureVisionAudioDecoder {
    fn new() -> Self {
        FutureVisionAudioDecoder {
            ainfo:  NAAudioInfo::new(0, 1, formats::SND_S16_FORMAT, 0),
            chmap:  NAChannelMap::from_ms_mapping(0x4), //single channel
            state:  IMAState::new(),
            count:  0,
        }
    }
}

#[cfg(feature="decoder_fstaud")]
impl NADecoder for FutureVisionAudioDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), 1, formats::SND_S16P_FORMAT, 1);
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            let samples = pktbuf.len() * 2;
            let abuf = alloc_audio_buffer(self.ainfo, samples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let buf = adata.get_data_mut().unwrap();
            for (dst, &val) in buf.chunks_exact_mut(2).zip(pktbuf.iter()) {
                dst[0] = self.state.expand_sample(val & 0xF);
                dst[1] = self.state.expand_sample(val >> 4);
                if self.count < 50 {
                    dst[0] = 0;
                    dst[1] = 0;
                }
                self.count += 2;
            }
            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
            frm.set_duration(Some(samples as u64));
            frm.set_keyframe(false);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

#[cfg(feature="decoder_fstaud")]
impl NAOptionHandler for FutureVisionAudioDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

#[cfg(feature="decoder_fstaud")]
pub fn get_decoder_audio() -> Box<dyn NADecoder + Send> {
    Box::new(FutureVisionAudioDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;

    // samples come from the Harvester game
    #[test]
    fn test_fst_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("fst", "fst-video", "assets/Game/alarm.fst", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x4028440a, 0xcb8aed5b, 0x2a9f1ead, 0x269169f5]));
    }
    #[test]
    fn test_fst_audio() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("fcmp", "fst-audio", "assets/Game/anxiety.cmp", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xa45b65b3, 0xe0654352, 0xf553e90b, 0x5dce0023]));
    }
}
