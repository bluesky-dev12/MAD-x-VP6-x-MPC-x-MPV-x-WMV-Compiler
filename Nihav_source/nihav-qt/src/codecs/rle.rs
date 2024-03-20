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

pub const ARGB_FORMAT: NAPixelFormaton = NAPixelFormaton {
        model: ColorModel::RGB(RGBSubmodel::RGB), components: 4,
        comp_info: [
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 1, next_elem: 4 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 2, next_elem: 4 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 3, next_elem: 4 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 0, next_elem: 4 }),
            None ],
        elem_size: 4, be: false, alpha: true, palette: false };

#[derive(Default)]
struct RleDecoder {
    info:       NACodecInfoRef,
    hams:       HAMShuffler<u8>,
    hams16:     HAMShuffler<u16>,
    width:      usize,
    height:     usize,
    depth:      u8,
}

#[allow(clippy::needless_range_loop)]
fn unpack_mono(src: u16, dst: &mut [u8; 16]) {
    for i in 0..16 {
        dst[i] = ((src >> (15 - i)) & 1) as u8;
    }
}

#[allow(clippy::needless_range_loop)]
fn unpack_pixels(src: u32, dst: &mut [u8; 16], depth: u8) {
    let depth = depth as usize;
    let mask = (1 << depth) - 1;
    for i in 0..32 / depth {
        dst[i] = ((src >> (32 - depth - i * depth)) & mask) as u8;
    }
}

impl RleDecoder {
    fn new() -> Self {
        Self::default()
    }
    fn decode_mono(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u8>, start_line: usize, end_line: usize) -> DecoderResult<bool> {
        let mut has_skips = false;
        let mut pix_buf = [0u8; 16];
        let npixels = 16;

        let stride = frm.stride[0];
        let mut doff = frm.offset[0] + start_line * stride;
        let mut y = start_line;
        while y < end_line {
            let mut x = 0;
            while x < self.width {
                let skip_code               = br.read_byte()?;
                let rle_code                = br.read_byte()? as i8;
                if x == 0 {
                    validate!((skip_code & 0x80) != 0);
                }
                if rle_code == 0 {
                    return Ok(false);
                }
                if x != 0 && (skip_code & 0x80) != 0 {
                    x = 0;
                    y += 1;
                    validate!(y < end_line);
                    doff += stride;
                    has_skips = true;
                }
                let skip = (skip_code & 0x1F) as usize;
                if skip > 0 {
                    has_skips = true;
                }
                validate!(x + skip * npixels <= self.width);
                x += skip * npixels;
                if rle_code < 0 {
                    let len = (-i16::from(rle_code)) as usize;
                    validate!(x + len * npixels <= self.width);
                    let val                 = br.read_u16be()?;
                    unpack_mono(val, &mut pix_buf);
                    for _ in 0..len {
                        for el in pix_buf.iter() {
                            frm.data[doff + x] = *el;
                            x += 1;
                        }
                    }
                } else {
                    let len = rle_code as usize;
                    validate!(x + len * npixels <= self.width);
                    for _ in 0..len {
                        let val             = br.read_u16be()?;
                        unpack_mono(val, &mut pix_buf);
                        for el in pix_buf.iter() {
                            frm.data[doff + x] = *el;
                            x += 1;
                        }
                    }
                }
            }
            doff += stride;
            y += 1;
        }

        Ok(has_skips)
    }
    fn decode_pal(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u8>, start_line: usize, end_line: usize) -> DecoderResult<bool> {
        let mut has_skips = false;
        let mut pix_buf = [0u8; 16];
        let npixels = match self.depth {
                2 => 16,
                4 => 8,
                8 => 4,
                _ => return Err(DecoderError::InvalidData),
            };

        let stride = frm.stride[0];
        let mut doff = frm.offset[0] + start_line * stride;
        for _ in start_line..end_line {
            let mut x = 0;
            let mut skip_mode = true;
            while x < self.width {
                if skip_mode {
                    let skip_code           = br.read_byte()?;
                    if skip_code == 0 {
                        return Ok(true);
                    } else {
                        let skip = (skip_code - 1) as usize;
                        validate!(x + skip * npixels <= self.width);
                        x += skip * npixels;
                        has_skips = true;
                    }
                    skip_mode = false;
                } else {
                    let rle_code            = br.read_byte()? as i8;
                    if rle_code == 0 {
                        skip_mode = true;
                    } else if rle_code == -1 {
                        if x < self.width {
                            has_skips = true;
                        }
                        break;
                    } else if rle_code < 0 {
                        let len = (-i16::from(rle_code)) as usize;
                        validate!(x + len * npixels <= self.width);
                        let val             = br.read_u32be()?;
                        unpack_pixels(val, &mut pix_buf, self.depth);
                        for _ in 0..len {
                            for el in pix_buf.iter().take(npixels) {
                                frm.data[doff + x] = *el;
                                x += 1;
                            }
                        }
                    } else {
                        let len = rle_code as usize;
                        validate!(x + len * npixels <= self.width);
                        for _ in 0..len {
                            let val         = br.read_u32be()?;
                            unpack_pixels(val, &mut pix_buf, self.depth);
                            for el in pix_buf.iter().take(npixels) {
                                frm.data[doff + x] = *el;
                                x += 1;
                            }
                        }
                    }
                }
            }
            if x == self.width {
                let eol                 = br.read_byte()? as i8;
                validate!(eol == -1);
            }
            doff += stride;
        }

        Ok(has_skips)
    }
    fn decode_16bit(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u16>, start_line: usize, end_line: usize) -> DecoderResult<bool> {
        let mut has_skips = false;

        let stride = frm.stride[0];
        let mut doff = frm.offset[0] + start_line * stride;
        for _ in start_line..end_line {
            let mut x = 0;
            let mut skip_mode = true;
            while x < self.width {
                if skip_mode {
                    let skip_code           = br.read_byte()?;
                    if skip_code == 0 {
                        return Ok(true);
                    } else {
                        let skip = (skip_code - 1) as usize;
                        validate!(x + skip <= self.width);
                        x += skip;
                        has_skips = true;
                    }
                    skip_mode = false;
                } else {
                    let rle_code            = br.read_byte()? as i8;
                    if rle_code == 0 {
                        skip_mode = true;
                    } else if rle_code == -1 {
                        if x < self.width {
                            has_skips = true;
                        }
                        break;
                    } else if rle_code < 0 {
                        let len = (-i16::from(rle_code)) as usize;
                        validate!(x + len <= self.width);
                        let pix             = br.read_u16be()?;
                        for _ in 0..len {
                            frm.data[doff + x] = pix;
                            x += 1;
                        }
                    } else {
                        let len = rle_code as usize;
                        validate!(x + len <= self.width);
                        for _ in 0..len {
                            frm.data[doff + x] = br.read_u16be()?;
                            x += 1;
                        }
                    }
                }
            }
            if x == self.width {
                let eol                 = br.read_byte()? as i8;
                validate!(eol == -1);
            }
            doff += stride;
        }

        Ok(has_skips)
    }
    fn decode_24bit(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u8>, start_line: usize, end_line: usize) -> DecoderResult<bool> {
        let mut has_skips = false;
        let mut rgb = [0u8; 3];

        let stride = frm.stride[0];
        let mut doff = frm.offset[0] + start_line * stride;
        for _ in start_line..end_line {
            let mut x = 0;
            let mut skip_mode = true;
            while x < self.width {
                if skip_mode {
                    let skip_code           = br.read_byte()?;
                    if skip_code == 0 {
                        return Ok(true);
                    } else {
                        let skip = (skip_code - 1) as usize;
                        validate!(x + skip <= self.width);
                        x += skip;
                        has_skips = true;
                    }
                    skip_mode = false;
                } else {
                    let rle_code            = br.read_byte()? as i8;
                    if rle_code == 0 {
                        skip_mode = true;
                    } else if rle_code == -1 {
                        if x < self.width {
                            has_skips = true;
                        }
                        break;
                    } else if rle_code < 0 {
                        let len = (-i16::from(rle_code)) as usize;
                        validate!(x + len <= self.width);
                                              br.read_buf(&mut rgb)?;
                        for _ in 0..len {
                            frm.data[doff + x * 3]     = rgb[0];
                            frm.data[doff + x * 3 + 1] = rgb[1];
                            frm.data[doff + x * 3 + 2] = rgb[2];
                            x += 1;
                        }
                    } else {
                        let len = rle_code as usize;
                        validate!(x + len  <= self.width);
                                              br.read_buf(&mut frm.data[doff + x * 3..][..len * 3])?;
                        x += len;
                    }
                }
            }
            if x == self.width {
                let eol                 = br.read_byte()? as i8;
                validate!(eol == -1);
            }
            doff += stride;
        }

        Ok(has_skips)
    }
    fn decode_32bit(&self, br: &mut ByteReader, frm: &mut NASimpleVideoFrame<u8>, start_line: usize, end_line: usize) -> DecoderResult<bool> {
        let mut has_skips = false;
        let mut rgb = [0u8; 4];

        let stride = frm.stride[0];
        let mut doff = frm.offset[0] + start_line * stride;
        for _ in start_line..end_line {
            let mut x = 0;
            let mut skip_mode = true;
            while x < self.width {
                if skip_mode {
                    let skip_code           = br.read_byte()?;
                    if skip_code == 0 {
                        return Ok(true);
                    } else {
                        let skip = (skip_code - 1) as usize;
                        validate!(x + skip <= self.width);
                        x += skip;
                        has_skips = true;
                    }
                    skip_mode = false;
                } else {
                    let rle_code            = br.read_byte()? as i8;
                    if rle_code == 0 {
                        skip_mode = true;
                    } else if rle_code == -1 {
                        if x < self.width {
                            has_skips = true;
                        }
                        break;
                    } else if rle_code < 0 {
                        let len = (-i16::from(rle_code)) as usize;
                        validate!(x + len <= self.width);
                                              br.read_buf(&mut rgb)?;
                        for _ in 0..len {
                            frm.data[doff + x * 4]     = rgb[0];
                            frm.data[doff + x * 4 + 1] = rgb[1];
                            frm.data[doff + x * 4 + 2] = rgb[2];
                            frm.data[doff + x * 4 + 3] = rgb[3];
                            x += 1;
                        }
                    } else {
                        let len = rle_code as usize;
                        validate!(x + len  <= self.width);
                                              br.read_buf(&mut frm.data[doff + x * 4..][..len * 4])?;
                        x += len;
                    }
                }
            }
            if x == self.width {
                let eol                 = br.read_byte()? as i8;
                validate!(eol == -1);
            }
            doff += stride;
        }

        Ok(has_skips)
    }
}

impl NADecoder for RleDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            self.depth = vinfo.bits;
            let fmt = match self.depth {
                    1 | 2 | 4 | 8 => PAL8_FORMAT,
                    15 | 16 => RGB555_FORMAT,
                    24 => RGB24_FORMAT,
                    32 => ARGB_FORMAT,
                    _ => return Err(DecoderError::InvalidData),
                };
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

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

        let id                          = br.read_byte()?;
        validate!(id == 0x40 || id == 0x00);
        let length                      = br.read_u24be()? as usize;
        validate!(length == src.len());

        let flags                       = br.read_u16be()?;
        let (start_line, end_line) = if (flags & 0x8) != 0 {
                let start               = br.read_u16be()? as usize;
                                          br.read_skip(2)?;
                let h                   = br.read_u16be()? as usize;
                                          br.read_skip(2)?;
                validate!(start + h <= self.height);
                (start, start + h)
            } else {
                (0, self.height)
            };

        let mut has_skips;
        let buftype;
        if self.depth <= 8 {
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
            if self.depth != 1 {
                has_skips = self.decode_pal(&mut br, &mut frm, start_line, end_line)?;
            } else {
                has_skips = self.decode_mono(&mut br, &mut frm, start_line, end_line)?;
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
            buftype = NABufferType::Video(buf);
        } else if self.depth == 16 {
            let bufret = self.hams16.clone_ref();
            let mut buf;
            if let Some(bbuf) = bufret {
                buf = bbuf;
            } else {
                let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
                buf = bufinfo.get_vbuf16().unwrap();
                self.hams16.add_frame(buf);
                buf = self.hams16.get_output_frame().unwrap();
            }
            let mut frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
            has_skips = self.decode_16bit(&mut br, &mut frm, start_line, end_line)?;
            buftype = NABufferType::Video16(buf);
        } else if self.depth == 24 {
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
            has_skips = self.decode_24bit(&mut br, &mut frm, start_line, end_line)?;
            buftype = NABufferType::Video(buf);
        } else if self.depth == 32 {
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
            has_skips = self.decode_32bit(&mut br, &mut frm, start_line, end_line)?;
            buftype = NABufferType::Video(buf);
        } else {
            return Err(DecoderError::InvalidData);
        }
        if (start_line != 0) || (end_line != self.height) {
            has_skips = true;
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), buftype);
        frm.set_keyframe(!has_skips);
        frm.set_frame_type(if !has_skips { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.hams.clear();
        self.hams16.clear();
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
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    // samples from https://samples.mplayerhq.hu/V-codecs/QTRLE
    #[test]
    fn test_qt_rle_1bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

//test_file_decoding("mov", "assets/QT/Animation-Monochrome.mov", Some(6), true, false, Some("qtrle-mono"), &dmx_reg, &dec_reg);
        test_decoding("mov", "qt-rle", "assets/QT/Animation-Monochrome.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0xc5b6fd2b, 0x87aae95c, 0xc32d0aaa, 0x4ed21592],
                            [0xe13aa599, 0x2ec6421e, 0xfcfdcc81, 0x8c2ee2ef],
                            [0xd835547b, 0xe2ffd6c9, 0xe24135c1, 0x7d2bb64f],
                            [0x6176e812, 0xb637444c, 0x71b1cbe5, 0xfc98905f],
                            [0x53a6b414, 0xbbf266c0, 0x859c3d10, 0x179a2252],
                            [0x56bc8cdd, 0x6438ba4d, 0x056e79c1, 0xb851e767],
                            [0xc25b9fec, 0xfc93233a, 0x30301b6d, 0xb290f5d7]]));
    }
    #[test]
    fn test_qt_rle_4bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "qt-rle", "assets/QT/Animation-16Greys.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x280c82aa, 0xa8bd11a6, 0x5b5babda, 0xeff58dd9],
                            [0x5c732231, 0x81d20be3, 0x41249639, 0xab2a9841],
                            [0xb689268d, 0x26de8510, 0x506e0a06, 0xb0dcfc40],
                            [0x09dced38, 0xbd0f5482, 0x3803efc9, 0x84bcaac4],
                            [0x5c49f8cc, 0xc1e2bd17, 0x1c22b849, 0x8f09afd5],
                            [0xe9f7b847, 0x13a4e429, 0x1b8af943, 0x82766ed2],
                            [0x2833c41a, 0x53781343, 0xce083e07, 0xdc3b24e9]]));
    }
    #[test]
    fn test_qt_rle_8bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "qt-rle", "assets/QT/Animation-256Greys.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x3c439212, 0x419944a5, 0xb274d9f5, 0xf3c53b12],
                            [0xd2a51886, 0xfbd37c2e, 0x899806c6, 0x465a66e0],
                            [0xbd59029e, 0x9de8766c, 0xd681bf0c, 0xa7fe4dfe],
                            [0x6150d380, 0xe8d67e0b, 0x0beb8455, 0xfcfd899d],
                            [0xba0f6b50, 0xba253267, 0xb727c5d7, 0x10dbf0c7],
                            [0x4f93f1aa, 0x2f10aa86, 0xfa97b35f, 0x687065df],
                            [0x730270e2, 0x3a3e02f1, 0xb906f478, 0xdb42af87]]));
    }
    #[test]
    fn test_qt_rle_16bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "qt-rle", "assets/QT/Animation-Highcolour.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x8fa5361b, 0xd36d6def, 0x33b250e9, 0xab98a41c],
                            [0xdd253ce0, 0xbc5fd3a1, 0x51fe2394, 0x950f18e4],
                            [0xbf34ca8d, 0x1faa24ee, 0x8d70af09, 0x742a4056],
                            [0x9feca59b, 0xdca7dc1d, 0xb6fe14f1, 0xd88c8b67],
                            [0x2e1544b8, 0x89a7e788, 0x0efcde09, 0xec4e7995],
                            [0x1e28b2a8, 0xd53495ca, 0x405a9b6e, 0x59338cad],
                            [0xe90156a4, 0xb64bca4a, 0x5cdf6681, 0xc32945fb]]));
    }
    #[test]
    fn test_qt_rle_24bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "qt-rle", "assets/QT/Animation-Truecolour.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x84d00c89, 0xf92c7f81, 0x6c1bd9e7, 0x35dd8f57],
                            [0x5ff0a7c9, 0xb589a484, 0xecc230b3, 0xe4e53bd3],
                            [0xbb394f7b, 0x2591b6c6, 0x12b0070c, 0xfa0952fc],
                            [0x380c1756, 0x72be8b16, 0xa7bbff48, 0x106017d4],
                            [0x27d8cc7c, 0x43ee384d, 0x542b95b1, 0x0a3ee031],
                            [0xa29529de, 0xcb53d03b, 0x4f2b6ec5, 0x4ea5d738],
                            [0x181d471b, 0x90d389e7, 0xc627b263, 0xa9d42323]]));
    }
    #[test]
    fn test_qt_rle_32bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "qt-rle", "assets/QT/Jag-finder-renaming.mov", Some(10), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe82390cf, 0xfaceab34, 0xcc857b2f, 0xed47f125],
                            [0xe67d4be5, 0xe41ce60e, 0x4e6912c3, 0xc96dc521]]));
    }
}
