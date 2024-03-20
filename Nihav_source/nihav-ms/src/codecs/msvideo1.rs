use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::HAMShuffler;

const RGB555_FORMAT: NAPixelFormaton = NAPixelFormaton {
        model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
        comp_info: [
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift: 10, comp_offs: 0, next_elem: 2 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  5, comp_offs: 0, next_elem: 2 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  0, comp_offs: 0, next_elem: 2 }),
            None, None],
        elem_size: 2, be: false, alpha: false, palette: false };

#[derive(Default)]
struct Video1Decoder {
    info:       NACodecInfoRef,
    hams:       HAMShuffler<u8>,
    hams16:     HAMShuffler<u16>,
    width:      usize,
    height:     usize,
    is_16bit:   bool,
}

impl Video1Decoder {
    fn new() -> Self {
        Self::default()
    }
    fn decode_frame(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u8>) -> DecoderResult<FrameType> {
        let off = frm.offset[0];
        let stride = frm.stride[0];
        let mut skip_count = 0;
        let blk_w = (self.width  + 3) >> 2;
        let blk_h = (self.height + 3) >> 2;
        let mut cur_x = 0;
        let mut cur_y = 0;
        while cur_x < blk_w && cur_y < blk_h {
            let op                      = br.read_u16le()?;
            let advance;
            if op < 0x8000 {
                let mut clr = [0; 2];
                                          br.read_buf(&mut clr)?;
                let mut flags = !op as usize;
                let cur_off = off + cur_x * 4 + cur_y * 4 * stride;
                for j in 0..4 {
                    for i in 0..4 {
                        frm.data[cur_off + i + j * stride] = clr[flags & 1];
                        flags >>= 1;
                    }
                }
                advance = 1;
            } else if op < 0x8400 {
                let cur_off = off + cur_x * 4 + cur_y * 4 * stride;
                let clr = op as u8;
                for j in 0..4 {
                    for i in 0..4 {
                        frm.data[cur_off + i + j * stride] = clr;
                    }
                }
                advance = 1;
            } else if op < 0x8800 {
                advance = (op & 0x3FF) as usize;
                validate!(advance > 0);
                skip_count += advance;
            } else {
                let mut clr = [0; 8];
                                      br.read_buf(&mut clr)?;
                let mut flags = !op as usize;
                let cur_off = off + cur_x * 4 + cur_y * 4 * stride;
                for j in 0..4 {
                    for i in 0..4 {
                        frm.data[cur_off + i + j * stride] = clr[(i >> 1) * 2 + (j >> 1) * 4 + (flags & 1)];
                        flags >>= 1;
                    }
                }
                advance = 1;
            }
            cur_x += advance;
            while cur_x >= blk_w {
                cur_x -= blk_w;
                cur_y += 1;
            }
        }
        validate!(cur_x == 0 && cur_y == blk_h);
        if skip_count == 0 {
            Ok(FrameType::I)
        } else if skip_count < blk_w * blk_h {
            Ok(FrameType::P)
        } else {
            Ok(FrameType::Skip)
        }
    }
    fn decode_frame16(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u16>) -> DecoderResult<FrameType> {
        let off = frm.offset[0];
        let stride = frm.stride[0];
        let mut skip_count = 0;
        let blk_w = (self.width  + 3) >> 2;
        let blk_h = (self.height + 3) >> 2;
        let mut cur_x = 0;
        let mut cur_y = 0;
        while cur_x < blk_w && cur_y < blk_h {
            let op                      = br.read_u16le()?;
            let advance;
            if (op & 0x8000) == 0 {
                let mut clr = [0; 8];
                clr[0]                  = br.read_u16le()?;
                clr[1]                  = br.read_u16le()?;
                let mut flags = !op as usize;
                let cur_off = off + cur_x * 4 + cur_y * 4 * stride;
                if (clr[0] & 0x8000) == 0 {
                    for j in 0..4 {
                        for i in 0..4 {
                            frm.data[cur_off + i + j * stride] = clr[flags & 1];
                            flags >>= 1;
                        }
                    }
                } else {
                    clr[2]              = br.read_u16le()?;
                    clr[3]              = br.read_u16le()?;
                    clr[4]              = br.read_u16le()?;
                    clr[5]              = br.read_u16le()?;
                    clr[6]              = br.read_u16le()?;
                    clr[7]              = br.read_u16le()?;
                    for j in 0..4 {
                        for i in 0..4 {
                            frm.data[cur_off + i + j * stride] = clr[(i >> 1) * 2 + (j >> 1) * 4 + (flags & 1)];
                            flags >>= 1;
                        }
                    }
                }
                advance = 1;
            } else if (op & 0xFC00) == 0x8400 {
                advance = (op & 0x3FF) as usize;
                validate!(advance > 0);
                skip_count += advance;
            } else {
                let cur_off = off + cur_x * 4 + cur_y * 4 * stride;
                let clr = op & 0x7FFF;
                for j in 0..4 {
                    for i in 0..4 {
                        frm.data[cur_off + i + j * stride] = clr;
                    }
                }
                advance = 1;
            }
            cur_x += advance;
            while cur_x >= blk_w {
                cur_x -= blk_w;
                cur_y += 1;
            }
        }
        validate!((cur_x == 0 || cur_x == 1) && cur_y == blk_h);
        if skip_count == 0 {
            Ok(FrameType::I)
        } else if skip_count < blk_w * blk_h {
            Ok(FrameType::P)
        } else {
            Ok(FrameType::Skip)
        }
    }
}

impl NADecoder for Video1Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.is_16bit = !vinfo.get_format().palette;
            let fmt = if !self.is_16bit { PAL8_FORMAT } else { RGB555_FORMAT };
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, true, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    #[allow(clippy::identity_op)]
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() >= 2);
        let mut mr = MemoryReader::new_read(src.as_slice());
        let mut br = ByteReader::new(&mut mr);

        let buftype;
        let ftype;
        if !self.is_16bit {
            let bufret = self.hams.clone_ref();
            let mut buf;
            if let Some(bbuf) = bufret {
                buf = bbuf;
            } else {
                let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 2)?;
                buf = bufinfo.get_vbuf().unwrap();
                self.hams.add_frame(buf);
                buf = self.hams.get_output_frame().unwrap();
            }
            let mut frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
            ftype = self.decode_frame(&mut br, &mut frm)?;
            let paloff = frm.offset[1];
            let dpal = &mut frm.data[paloff..];
let mut found_pal = false;
            for sd in pkt.side_data.iter() {
                match *sd {
                    NASideData::Palette(_, ref pal) => {
                        for (dst, src) in dpal.chunks_mut(3).zip(pal.chunks(4)) {
                            dst[0] = src[0];
                            dst[1] = src[1];
                            dst[2] = src[2];
                        }
found_pal = true;
                        break;
                    },
                    _ => {},
                };
            }
if !found_pal {
    for i in 0..256 {
        dpal[i * 3 + 0] = i as u8;
        dpal[i * 3 + 1] = i as u8;
        dpal[i * 3 + 2] = i as u8;
    }
}
            buftype = NABufferType::Video(buf);
        } else {
            let bufret = self.hams16.clone_ref();
            let mut buf;
            if let Some(bbuf) = bufret {
                buf = bbuf;
            } else {
                let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 2)?;
                buf = bufinfo.get_vbuf16().unwrap();
                self.hams16.add_frame(buf);
                buf = self.hams16.get_output_frame().unwrap();
            }
            let mut frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
            ftype = self.decode_frame16(&mut br, &mut frm)?;
            buftype = NABufferType::Video16(buf);
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), buftype);
        frm.set_keyframe(ftype == FrameType::I);
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.hams.clear();
        self.hams16.clear();
    }
}

impl NAOptionHandler for Video1Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(Video1Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::ms_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_ms_video1_8bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        ms_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/avi/palette_change/toon.avi
        test_decoding("avi", "msvideo1", "assets/MS/toon.avi", Some(66), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x0c26ec42, 0xb75bfea7, 0x1e6342ae, 0xb14dcfa3]));
    }
    #[test]
    fn test_ms_video1_16bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        ms_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/CRAM/clock-cram16.avi
        test_decoding("avi", "msvideo1", "assets/MS/clock-cram16.avi", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x03381fa4, 0x5b294fec, 0xb97a7575, 0xa1a3ffe9]));
    }
}
