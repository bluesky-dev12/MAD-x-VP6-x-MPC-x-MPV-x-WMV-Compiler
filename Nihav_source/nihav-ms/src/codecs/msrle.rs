use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::HAMShuffler;

#[derive(Default)]
struct RleDecoder {
    info:       NACodecInfoRef,
    hams:       HAMShuffler<u8>,
    width:      usize,
    height:     usize,
    is_4bit:    bool,
}

impl RleDecoder {
    fn new() -> Self {
        Self::default()
    }
    fn decode_8bit(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u8>) -> DecoderResult<FrameType> {
        let mut has_skips = false;

        if (br.left() as usize) == self.width * self.height {
            for line in frm.data.chunks_mut(frm.stride[0]).take(self.height) {
                                        br.read_buf(&mut line[..self.width])?;
            }
            return Ok(FrameType::I);
        }
        let mut x = 0;
        let mut y = 0;
        let mut doff = 0;
        loop {
            let a                       = br.read_byte()?;
            let b                       = br.read_byte()?;
            if a > 0 {
                let len = a as usize;
                validate!(y < self.height);
                validate!(x + len <= self.width);
                for _ in 0..len {
                    frm.data[doff] = b;
                    doff += 1;
                }
                x += len;
            } else {
                match b {
                    0 => {
                        if x != self.width {
                            has_skips = true;
                        }
                        x = 0;
                        y += 1;
                    },
                    1 => {
                        if x != 0 || y != self.height {
                            has_skips = true;
                        }
                        break;
                    },
                    2 => {
                        let xoff        = br.read_byte()? as usize;
                        let yoff        = br.read_byte()? as usize;
                        validate!(x + xoff <= self.width);
                        validate!(y + yoff <= self.height);
                        x += xoff;
                        y += yoff;
                        has_skips = true;
                    },
                    _ => {
                        let len = b as usize;
                        validate!(y < self.height);
                        validate!(x + len <= self.width);
                        for _ in 0..len {
                            frm.data[doff] = br.read_byte()?;
                            doff += 1;
                        }
                        x += len;
                        if (len & 1) != 0 {
                                          br.read_byte()?; // padding
                        }
                    },
                };
                if b < 3 {
                    doff = x + y * frm.stride[0];
                }
            }
        }


        Ok(if has_skips { FrameType::P } else { FrameType::I })
    }
    fn decode_4bit(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u8>) -> DecoderResult<FrameType> {
        let mut has_skips = false;

        if (br.left() as usize) == (self.width + 1) / 2 * self.height {
            for line in frm.data.chunks_mut(frm.stride[0]).take(self.height) {
                for i in 0..self.width/2 {
                    let clr             = br.read_byte()?;
                    line[i * 2]     = clr >> 4;
                    line[i * 2 + 1] = clr & 0xF;
                }
                if (self.width & 1) == 1 {
                    let clr             = br.read_byte()?;
                    line[self.width - 1] = clr >> 4;
                }
            }
            return Ok(FrameType::I);
        }
        let mut x = 0;
        let mut y = 0;
        let mut doff = 0;
        loop {
            let a                       = br.read_byte()?;
            let b                       = br.read_byte()?;
            if a > 0 {
                let len = a as usize;
                validate!(y < self.height);
                validate!(x + len <= self.width);
                for _ in 0..len / 2 {
                    frm.data[doff] = b >> 4;
                    doff += 1;
                    frm.data[doff] = b & 0xF;
                    doff += 1;
                }
                if (len & 1) != 0 {
                    frm.data[doff] = b >> 4;
                    doff += 1;
                }
                x += len;
            } else {
                match b {
                    0 => {
                        if x != self.width {
                            has_skips = true;
                        }
                        x = 0;
                        y += 1;
                    },
                    1 => {
                        if x != 0 || y != self.height {
                            has_skips = true;
                        }
                        break;
                    },
                    2 => {
                        let xoff        = br.read_byte()? as usize;
                        let yoff        = br.read_byte()? as usize;
                        validate!(x + xoff <= self.width);
                        validate!(y + yoff <= self.height);
                        x += xoff;
                        y += yoff;
                        has_skips = true;
                    },
                    _ => {
                        let len = b as usize;
                        validate!(y < self.height);
                        validate!(x + len <= self.width);
                        for _ in 0..len / 2 {
                            let clr     = br.read_byte()?;
                            frm.data[doff] = clr >> 4;
                            doff += 1;
                            frm.data[doff] = clr & 0xF;
                            doff += 1;
                        }
                        if (len & 1) != 0 {
                            let clr     = br.read_byte()?;
                            frm.data[doff] = clr >> 4;
                            doff += 1;
                        }
                        x += len;
                        if ((len + 1) & 2) != 0 {
                                          br.read_byte()?; // padding
                        }
                    },
                };
                if b < 3 {
                    doff = x + y * frm.stride[0];
                }
            }
        }

        Ok(if has_skips { FrameType::P } else { FrameType::I })
    }
}

impl NADecoder for RleDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, true, PAL8_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.is_4bit = vinfo.bits == 4;

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() >= 2);
        let mut mr = MemoryReader::new_read(src.as_slice());
        let mut br = ByteReader::new(&mut mr);

        let ftype;
        let bufret = self.hams.clone_ref();
        let mut buf;
        if let Some(bbuf) = bufret {
            buf = bbuf;
        } else {
            let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
            buf = bufinfo.get_vbuf().unwrap();
            self.hams.add_frame(buf);
            buf = self.hams.get_output_frame().unwrap();
        }
        let mut frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
        if !self.is_4bit {
            ftype = self.decode_8bit(&mut br, &mut frm)?;
        } else {
            ftype = self.decode_4bit(&mut br, &mut frm)?;
        }
        let paloff = frm.offset[1];
        let dpal = &mut frm.data[paloff..];
        for sd in pkt.side_data.iter() {
            match *sd {
                NASideData::Palette(_, ref pal) => {
                    for (dst, src) in dpal.chunks_mut(3).zip(pal.chunks(4)) {
                        dst[0] = src[0];
                        dst[1] = src[1];
                        dst[2] = src[2];
                    }
                    break;
                },
                _ => {},
            };
        }
        let buftype = NABufferType::Video(buf);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), buftype);
        frm.set_keyframe(ftype == FrameType::I);
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.hams.clear();
    }
}

impl NAOptionHandler for RleDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RleDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::ms_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    // samples from https://samples.mplayerhq.hu/V-codecs/RLE
    #[test]
    fn test_ms_rle_8bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        ms_register_all_decoders(&mut dec_reg);

        test_decoding("avi", "msrle", "assets/MS/workcycl-64color.avi", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0xdd030d70, 0x0b6140ff, 0xdd01823e, 0x27c18f12],
                            [0xd592aa63, 0x97bafa10, 0x380787ed, 0x71da30c4],
                            [0x78596af1, 0x8c21fa09, 0xd914131b, 0x04e3e176],
                            [0xcfe091d7, 0xccd513d5, 0x5bd3caa3, 0xcc388e4a],
                            [0xbea60252, 0x1e493c7d, 0x9996b50c, 0x8f7a1c24],
                            [0xfab23365, 0xe8070819, 0xa2ce974e, 0x15dcabba],
                            [0xd1cdf200, 0x9c2e6d33, 0xbe3270d1, 0x1dede547]]));
    }
    #[test]
    fn test_ms_rle_8bit_raw() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        ms_register_all_decoders(&mut dec_reg);

        test_decoding("avi", "msrle", "assets/MS/suzie_appl_rle8.avi", Some(2), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0xe4d5ad8d, 0x50e8bd7e, 0x0d602ece, 0x3631a8b4],
                            [0x3301ba0e, 0x281c3e54, 0xbd961d44, 0xc352845b],
                            [0x3ed7881d, 0x86acadf7, 0x2922d8bc, 0x8ec9b17e]]));
    }
    #[test]
    fn test_ms_rle_4bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        ms_register_all_decoders(&mut dec_reg);

        test_decoding("avi", "msrle", "assets/MS/mplayer-msrle-4bit.avi", Some(2), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x9898c473, 0x80362654, 0xdb40513a, 0xb0c60fd9],
                            [0x281e1531, 0x368a71cb, 0x9dd02f8d, 0xdfe630d9],
                            [0x6faac693, 0x1856d0fd, 0x47933bb2, 0xb2fb02b0]]));
    }
    #[test]
    fn test_ms_rle_4bit_raw() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        ms_register_all_decoders(&mut dec_reg);

        test_decoding("avi", "msrle", "assets/MS/suzie_appl_rle4.avi", Some(2), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xb5d38296, 0xdae25407, 0x985973f0, 0xb1da9c94]));
    }
}
