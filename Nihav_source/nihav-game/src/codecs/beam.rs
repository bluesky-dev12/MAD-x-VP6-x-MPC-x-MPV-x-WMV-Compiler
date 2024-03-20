use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

const VB_FLAG_GMC:      u16 = 0x0001;
const VB_FLAG_AUDIO:    u16 = 0x0004;
const VB_FLAG_VIDEO:    u16 = 0x0008;
const VB_FLAG_PALETTE:  u16 = 0x0010;

const RGB555_FORMAT: NAPixelFormaton = NAPixelFormaton { model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
                                        comp_info: [
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift: 10, comp_offs: 0, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  5, comp_offs: 1, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  0, comp_offs: 2, next_elem: 2 }),
                                            None, None],
                                        elem_size: 2, be: false, alpha: false, palette: false };

fn check_size(x: usize, row_no: usize, dx: i16, dy: i16, stride: usize, len: usize) -> Option<usize> {
    let start = (x as i32) + i32::from(dx) + ((row_no * 4) as i32 + i32::from(dy)) * (stride as i32);
    if start >= 0 {
        let start = start as usize;
        let end = start + 4 + stride * 3;

        if end <= len {
            Some(start)
        } else {
            None
        }
    } else {
        None
    }
}

fn decode_video8(br: &mut ByteReader, dst: &mut [u8], prev_frame: &[u8], width: usize, gmv: [i16; 2]) -> DecoderResult<FrameType> {
    let mut ftype = FrameType::I;

    let mut btc = 0;
    let mut btypes = 0;
    for (row_no, strip) in dst.chunks_mut(width * 4).enumerate() {
        for x in (0..width).step_by(4) {
            if btc == 0 {
                btypes                          = br.read_byte()?;
                btc = 4;
            }
            match btypes & 0xC0 {
                0xC0 => {
                    let t                       = br.read_byte()?;
                    let mut pattern = VB_PATTERNS[(t & 0x3F) as usize];
                    let op = t >> 6;
                    validate!(op != 3);
                    if op == 0 {
                        let mut clr = [0; 2];
                                              br.read_buf(&mut clr)?;
                        for dline in strip[x..].chunks_mut(width).take(4) {
                            for el in dline[..4].iter_mut() {
                                *el = clr[(pattern & 1) as usize];
                                pattern >>= 1;
                            }
                        }
                    } else {
                        if op == 2 {
                            pattern = !pattern;
                        }
                        let clr             = br.read_byte()?;

                        if let Some(start) = check_size(x, row_no, gmv[0], gmv[1], width, prev_frame.len()) {
                            for (dline, sline) in strip[x..].chunks_mut(width).zip(prev_frame[start..].chunks(width)).take(4) {
                                for (dst, &src) in dline[..4].iter_mut().zip(sline.iter()) {
                                    *dst = if (pattern & 1) != 0 { clr } else { src };
                                    pattern >>= 1;
                                }
                            }
                        } else {
                            return Err(DecoderError::InvalidData);
                        }

                        ftype = FrameType::P;
                    }
                },
                0x80 => {
                    let clr                 = br.read_byte()?;
                    for dline in strip[x..].chunks_mut(width).take(4) {
                        for el in dline[..4].iter_mut() {
                            *el = clr;
                        }
                    }
                },
                0x40 => {
                    let mv                  = br.read_byte()?;
                    if mv == 0 {
                        for dline in strip[x..].chunks_mut(width).take(4) {
                                          br.read_buf(&mut dline[..4])?;
                        }
                    } else {
                        let mvx = (((mv & 0xF) ^ 8) as i16) - 8;
                        let mvy = (((mv >>  4) ^ 8) as i16) - 8;
                        if let Some(start) = check_size(x, row_no, gmv[0] + mvx, gmv[1] + mvy, width, prev_frame.len()) {
                            for (dline, sline) in strip[x..].chunks_mut(width).zip(prev_frame[start..].chunks(width)).take(4) {
                                dline[..4].copy_from_slice(&sline[..4]);
                            }
                        } else {
                            return Err(DecoderError::InvalidData);
                        }
                        ftype = FrameType::P;
                    }
                },
                _ => {
                    if let Some(start) = check_size(x, row_no, gmv[0], gmv[1], width, prev_frame.len()) {
                        for (dline, sline) in strip[x..].chunks_mut(width).zip(prev_frame[start..].chunks(width)).take(4) {
                            dline[..4].copy_from_slice(&sline[..4]);
                        }
                    } else {
                        return Err(DecoderError::InvalidData);
                    }
                    ftype = FrameType::P;
                },
            }

            btypes <<= 2;
            btc     -= 1;
        }
    }

    Ok(ftype)
}

struct OldData {
    pal:            [u8; 768],
    prev_frame:     Vec<u8>,
    cur_frame:      Vec<u8>,
    width:          usize,
    flags:          u16,
}

impl OldData {
    fn new(w: usize, h: usize, flags: u16) -> Self {
        Self {
            pal:            [0; 768],
            prev_frame:     vec![0; w * h],
            cur_frame:      vec![0; w * h],
            width:          w,
            flags,
        }
    }
    fn decode(&mut self, br: &mut ByteReader, vbuf: &mut NAVideoBufferRef<u8>) -> DecoderResult<FrameType> {
        let asize                               = br.read_u16le()? as usize;
        let _smth                               = br.read_u16le()? as usize;
        let pal_size                            = br.read_u16le()? as usize;
        if pal_size > 0 {
            let pal_start                       = br.read_u16le()? as usize;
            validate!(pal_start + pal_size <= 256);
                                                  br.read_buf(&mut self.pal[pal_start * 3..][..pal_size * 3])?;
        }
                                                  br.read_skip(asize)?;

        let (gmv_x, gmv_y) = if (self.flags & 0x2000) != 0 {
                let mvx                         = br.read_u16le()? as i16;
                let mvy                         = br.read_u16le()? as i16;
                (mvx, mvy)
            } else {
                (0, 0)
            };

        std::mem::swap(&mut self.cur_frame, &mut self.prev_frame);

        let ftype = decode_video8(br, &mut self.cur_frame, &self.prev_frame, self.width, [gmv_x, gmv_y])?;

        let stride = vbuf.get_stride(0);
        let pal_off = vbuf.get_offset(1);
        let data = vbuf.get_data_mut().unwrap();

        for (dline, sline) in data.chunks_mut(stride).zip(self.cur_frame.chunks(self.width)) {
            dline[..self.width].copy_from_slice(sline);
        }
        data[pal_off..][..768].copy_from_slice(&self.pal);

        Ok(ftype)
    }
}

struct Data8 {
    pal:            [u8; 768],
    prev_frame:     Vec<u8>,
    cur_frame:      Vec<u8>,
    width:          usize,
}

impl Data8 {
    fn new(w: usize, h: usize) -> Self {
        Self {
            pal:            [0; 768],
            prev_frame:     vec![0; w * h],
            cur_frame:      vec![0; w * h],
            width:          w,
        }
    }
    fn decode(&mut self, br: &mut ByteReader, vbuf: &mut NAVideoBufferRef<u8>) -> DecoderResult<FrameType> {
        let flags                               = br.read_u16le()?;
        let (gmv_x, gmv_y) = if (flags & VB_FLAG_GMC) != 0 {
                let mvx                         = br.read_u16le()? as i16;
                let mvy                         = br.read_u16le()? as i16;
                (mvx, mvy)
            } else {
                (0, 0)
            };
        if (flags & VB_FLAG_AUDIO) != 0 {
            let asize                           = br.read_u32le()? as usize;
            validate!(asize >= 4 && asize <= (br.left() as usize));
                                                  br.read_skip(asize - 4)?;
        }
        let mut ftype = FrameType::Skip;
        if (flags & VB_FLAG_VIDEO) != 0 {
            std::mem::swap(&mut self.cur_frame, &mut self.prev_frame);

            let vsize                           = br.read_u32le()? as u64;
            validate!(vsize > 4);
            let end = br.tell() + vsize - 4;

            ftype = decode_video8(br, &mut self.cur_frame, &self.prev_frame, self.width, [gmv_x, gmv_y])?;

            validate!(br.tell() == end);
        }
        if (flags & VB_FLAG_PALETTE) != 0 {
            let pal_size                        = br.read_u32le()? as usize;
            validate!(pal_size > 6);
            validate!(br.left() as usize >= pal_size - 4);
            let start                           = br.read_byte()? as usize;
            let mut size                        = br.read_byte()? as usize;
            if size == 0 {
                size = 256;
            }
            validate!(start + size <= 256);
            validate!(size * 3 + 6 == pal_size);
                                                  br.read_buf(&mut self.pal[start * 3..][..size * 3])?;
        }

        let stride = vbuf.get_stride(0);
        let pal_off = vbuf.get_offset(1);
        let data = vbuf.get_data_mut().unwrap();

        for (dline, sline) in data.chunks_mut(stride).zip(self.cur_frame.chunks(self.width)) {
            dline[..self.width].copy_from_slice(sline);
        }
        data[pal_off..][..768].copy_from_slice(&self.pal);

        Ok(ftype)
    }
}

struct Data16 {
    prev_frame:     Vec<u16>,
    cur_frame:      Vec<u16>,
    pal:            [u16; 256],
    width:          usize,
}

impl Data16 {
    fn new(w: usize, h: usize) -> Self {
        Self {
            prev_frame:     vec![0; w * h],
            cur_frame:      vec![0; w * h],
            pal:            [0; 256],
            width:          w,
        }
    }
    fn decode(&mut self, br: &mut ByteReader, vbuf: &mut NAVideoBufferRef<u16>) -> DecoderResult<FrameType> {
        let flags                               = br.read_u16le()?;
        let (gmv_x, gmv_y) = if (flags & VB_FLAG_GMC) != 0 {
                let mvx                         = br.read_u16le()? as i16;
                let mvy                         = br.read_u16le()? as i16;
                (mvx, mvy)
            } else {
                (0, 0)
            };
        if (flags & VB_FLAG_AUDIO) != 0 {
            let asize                           = br.read_u32le()? as usize;
            validate!(asize <= (br.left() as usize));
                                                  br.read_skip(asize - 4)?;
        }
        let mut ftype = FrameType::Skip;
        if (flags & VB_FLAG_VIDEO) != 0 {
            let vsize                           = br.read_u32le()? as u64;
            validate!(vsize > 4);
            let end = br.tell() + vsize - 4;
            let has_pal = (flags & VB_FLAG_PALETTE) != 0;
            if has_pal {
                let cur_off = br.tell();
                                                  br.seek(SeekFrom::Current((vsize - 4) as i64))?;
                let psize                       = br.read_u32le()? as usize;
                validate!(psize > 4 && psize <= 0x204 && (psize & 1) == 0);
                for el in self.pal[..(psize - 4)/ 2].iter_mut() {
                    *el                         = br.read_u16le()?;
                }
                                                  br.seek(SeekFrom::Start(cur_off))?;
            }

            std::mem::swap(&mut self.cur_frame, &mut self.prev_frame);

            let mut btc = 0;
            let mut btypes = 0;
            ftype = FrameType::I;
            for (row_no, strip) in self.cur_frame.chunks_mut(self.width * 4).enumerate() {
                for x in (0..self.width).step_by(4) {
                    if btc == 0 {
                        btypes                  = br.read_byte()?;
                        btc = 4;
                    }
                    match btypes & 0xC0 {
                        0xC0 => {
                            let t               = br.read_byte()?;
                            let mut pattern = VB_PATTERNS[(t & 0x3F) as usize];
                            let op = t >> 6;
                            validate!(op != 3);
                            if op == 0 {
                                let mut clr = [0; 2];
                                if has_pal {
                                    clr[0]      = self.pal[br.read_byte()? as usize];
                                    clr[1]      = self.pal[br.read_byte()? as usize];
                                } else {
                                    clr[0]      = br.read_u16le()?;
                                    clr[1]      = br.read_u16le()?;
                                }
                                for dline in strip[x..].chunks_mut(self.width).take(4) {
                                    for el in dline[..4].iter_mut() {
                                        *el = clr[(pattern & 1) as usize];
                                        pattern >>= 1;
                                    }
                                }
                            } else {
                                if op == 2 {
                                    pattern = !pattern;
                                }
                                let clr = if has_pal {
                                                  self.pal[br.read_byte()? as usize]
                                    } else {
                                                  br.read_u16le()?
                                    };

                                if let Some(start) = check_size(x, row_no, gmv_x, gmv_y, self.width, self.prev_frame.len()) {
                                    for (dline, sline) in strip[x..].chunks_mut(self.width).zip(self.prev_frame[start..].chunks(self.width)).take(4) {
                                        for (dst, &src) in dline[..4].iter_mut().zip(sline.iter()) {
                                            *dst = if (pattern & 1) != 0 { clr } else { src };
                                            pattern >>= 1;
                                        }
                                    }
                                } else {
                                    return Err(DecoderError::InvalidData);
                                }
                                ftype = FrameType::P;
                            }
                        },
                        0x80 => {
                            let clr = if has_pal {
                                                  self.pal[br.read_byte()? as usize]
                                } else {
                                                  br.read_u16le()?
                                };
                            for dline in strip[x..].chunks_mut(self.width).take(4) {
                                for el in dline[..4].iter_mut() {
                                    *el = clr;
                                }
                            }
                        },
                        0x40 => {
                            let mv              = br.read_byte()?;
                            if mv == 0 {
                                if has_pal {
                                    for dline in strip[x..].chunks_mut(self.width).take(4) {
                                        for el in dline[..4].iter_mut() {
                                            *el = self.pal[br.read_byte()? as usize];
                                        }
                                    }
                                } else {
                                    for dline in strip[x..].chunks_mut(self.width).take(4) {
                                        for el in dline[..4].iter_mut() {
                                            *el = br.read_u16le()?;
                                        }
                                    }
                                }
                            } else {
                                let mvx = (((mv & 0xF) ^ 8) as i16) - 8;
                                let mvy = (((mv >>  4) ^ 8) as i16) - 8;
                                if let Some(start) = check_size(x, row_no, gmv_x + mvx, gmv_y + mvy, self.width, self.prev_frame.len()) {
                                    for (dline, sline) in strip[x..].chunks_mut(self.width).zip(self.prev_frame[start..].chunks(self.width)).take(4) {
                                        dline[..4].copy_from_slice(&sline[..4]);
                                    }
                                } else {
                                    return Err(DecoderError::InvalidData);
                                }
                                ftype = FrameType::P;
                            }
                        },
                        _ => {
                            if let Some(start) = check_size(x, row_no, gmv_x, gmv_y, self.width, self.prev_frame.len()) {
                                for (dline, sline) in strip[x..].chunks_mut(self.width).zip(self.prev_frame[start..].chunks(self.width)).take(4) {
                                    dline[..4].copy_from_slice(&sline[..4]);
                                }
                            } else {
                                return Err(DecoderError::InvalidData);
                            }
                            ftype = FrameType::P;
                        },
                    }

                    btypes <<= 2;
                    btc     -= 1;
                }
            }
            validate!(br.tell() == end);
        }
        if (flags & VB_FLAG_PALETTE) != 0 {
            let psize                           = br.read_u32le()? as usize;
                                                  br.read_skip(psize - 4)?;
        }

        let stride = vbuf.get_stride(0);
        let data = vbuf.get_data_mut().unwrap();

        for (dline, sline) in data.chunks_mut(stride).zip(self.cur_frame.chunks(self.width)) {
            dline[..self.width].copy_from_slice(sline);
        }

        Ok(ftype)
    }
}

enum FrameData {
    Old(OldData),
    Pal(Data8),
    New(Data16),
    None,
}

struct BeamVideoDecoder {
    info:       NACodecInfoRef,
    data:       FrameData,
    fcp:        bool,
}

impl BeamVideoDecoder {
    fn new(fcp: bool) -> Self {
        Self {
            info: NACodecInfoRef::default(),
            data: FrameData::None,
            fcp,
        }
    }
}

impl NADecoder for BeamVideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let width  = vinfo.get_width();
            let height = vinfo.get_height();
            validate!((width & 3) == 0 && (height & 3) == 0);
            let fmt = match vinfo.bits {
                    _ if self.fcp => {
                        let edata = info.get_extradata();
                        validate!(edata.is_some());
                        let edata = edata.unwrap();
                        validate!(edata.len() >= 2);
                        let flags = read_u16le(&edata)?;
                        self.data = FrameData::Old(OldData::new(width, height, flags));
                        PAL8_FORMAT
                    },
                    8 => {
                        self.data = FrameData::Pal(Data8::new(width, height));
                        PAL8_FORMAT
                    },
                    16 => {
                        self.data = FrameData::New(Data16::new(width, height));
                        RGB555_FORMAT
                    },
                    _ => return Err(DecoderError::InvalidData),
                };

            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 1);

        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);

        let mut bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;

        let ftype = match (&mut self.data, &mut bufinfo) {
            (FrameData::Old(ref mut data), NABufferType::Video(ref mut vbuf)) => data.decode(&mut br, vbuf)?,
            (FrameData::Pal(ref mut data), NABufferType::Video(ref mut vbuf)) => data.decode(&mut br, vbuf)?,
            (FrameData::New(ref mut data), NABufferType::Video16(ref mut vbuf)) => data.decode(&mut br, vbuf)?,
            _ => unreachable!(),
        };

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(ftype == FrameType::I);
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for BeamVideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_vbv() -> Box<dyn NADecoder + Send> {
    Box::new(BeamVideoDecoder::new(false))
}

pub fn get_decoder_fcp() -> Box<dyn NADecoder + Send> {
    Box::new(BeamVideoDecoder::new(true))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    #[test]
    fn test_beam_fcp() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from The Dame was Loaded
        test_decoding("siff", "beam-fcp", "assets/Game/siff/BEAM.FCP", Some(1000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0xa31342c0, 0x7afe5a76, 0x4111f17a, 0x30ec3a18],
                            [0xa31342c0, 0x7afe5a76, 0x4111f17a, 0x30ec3a18],
                            [0xa31342c0, 0x7afe5a76, 0x4111f17a, 0x30ec3a18],
                            [0xa31342c0, 0x7afe5a76, 0x4111f17a, 0x30ec3a18],
                            [0xa31342c0, 0x7afe5a76, 0x4111f17a, 0x30ec3a18],
                            [0x447870bf, 0x9bb22dab, 0x1f557dae, 0xc41575ad],
                            [0xeb3bd749, 0xc72811d3, 0x23d430b2, 0xc31dbd85],
                            [0x34bb16b4, 0x8e6066fe, 0xf3f6170a, 0xae4ec54e],
                            [0xee12f3ff, 0x4000ed43, 0x446336d7, 0x6cc344bf],
                            [0xfe6ba9e1, 0xcc97f503, 0xf6fc8f14, 0x40c334c0],
                            [0xb4229527, 0x8591ac0b, 0x1ba40ea8, 0x15aa5710]]));
    }
    #[test]
    fn test_beam_video_8bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from Lost Vikings 2
        test_decoding("siff", "beam-video", "assets/Game/siff/BEAM.VB", Some(1000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x23604e03, 0x5781ea8a, 0x6b28d338, 0xc32d52d6],
                            [0xba68d1af, 0xacf630b7, 0x3899073d, 0x07135315],
                            [0xb10cd159, 0xd787a566, 0x523123ca, 0x8a757f9e],
                            [0x7f4dfb53, 0x581884ab, 0x71de61c0, 0xfec72a11],
                            [0xc2a40282, 0x729548e1, 0xc63211bf, 0x586158fc],
                            [0xb8bf753c, 0x53686fb2, 0x7d307625, 0xdab70698],
                            [0xa2e90475, 0x076ef58a, 0xe2276941, 0x9c47ca14],
                            [0x80f785df, 0x19e015ab, 0x93beaf9b, 0xff1d0907],
                            [0x957a9a60, 0xc8a8ae09, 0xad5ecf6c, 0x06097f03],
                            [0xdfd331d4, 0x2f6ba92b, 0x5bfcb6f8, 0x85bab2b9],
                            [0xf37d42c1, 0x7f54d6bd, 0xc2dca688, 0x60d64e8c],
                            [0x97a0e18b, 0xce9ea101, 0x56eea57c, 0xeb48e8b3],
                            [0x7043048c, 0x373fdf9d, 0x1b27d5b3, 0x3e4e9c7a]]));
    }
    #[test]
    fn test_beam_video_16bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from Alien Earth
        test_decoding("siff", "beam-video", "assets/Game/siff/beamlogo.vbc", Some(500), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x90aeded9, 0x922cf894, 0x6ea78586, 0x35493724],
                            [0x90aeded9, 0x922cf894, 0x6ea78586, 0x35493724],
                            [0xa19daf5e, 0xa4858d72, 0x6700fc4d, 0x95c5e5dd],
                            [0x57d556af, 0x95c5c5f4, 0xe943f1e3, 0xb86941e1],
                            [0xa63351ae, 0x12ed21ee, 0x7246d1f4, 0xf3a5a79a],
                            [0x5930e388, 0x4891d37b, 0x619ea7ff, 0xc8dac16d],
                            [0x36eeaf75, 0x767e5e5e, 0x397e0100, 0xfa306811],
                            [0xb4722a6d, 0x61cc8483, 0xe4966f11, 0xa67cd0f4],
                            [0x62be7367, 0xcbeb77b2, 0xcb438480, 0xda39b47a],
                            [0x568b020d, 0x4992bc7f, 0xcbe20b5c, 0x697a35bc],
                            [0x35e5a6a8, 0x903c0d07, 0xbacf7734, 0xd1c54b6d],
                            [0xd3450588, 0xfae0a9ec, 0x72a8fce6, 0x7c57e3c1],
                            [0x90f0cc47, 0xd7e7d3ef, 0x46d81b4d, 0xf4495aa2]]));
    }
}

const VB_PATTERNS: [u16; 64] = [
    0x0660, 0xFF00, 0xCCCC, 0xF000, 0x8888, 0x000F, 0x1111, 0xFEC8,
    0x8CEF, 0x137F, 0xF731, 0xC800, 0x008C, 0x0013, 0x3100, 0xCC00,
    0x00CC, 0x0033, 0x3300, 0x0FF0, 0x6666, 0x00F0, 0x0F00, 0x2222,
    0x4444, 0xF600, 0x8CC8, 0x006F, 0x1331, 0x318C, 0xC813, 0x33CC,
    0x6600, 0x0CC0, 0x0066, 0x0330, 0xF900, 0xC88C, 0x009F, 0x3113,
    0x6000, 0x0880, 0x0006, 0x0110, 0xCC88, 0xFC00, 0x00CF, 0x88CC,
    0x003F, 0x1133, 0x3311, 0xF300, 0x6FF6, 0x0603, 0x08C6, 0x8C63,
    0xC631, 0x6310, 0xC060, 0x0136, 0x136C, 0x36C8, 0x6C80, 0x324C
];
