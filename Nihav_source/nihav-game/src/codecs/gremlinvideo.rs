use nihav_core::frame::*;
use nihav_core::formats;
use nihav_core::formats::{NAChannelType, NAChannelMap};
use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

struct GremlinVideoDecoder {
    info:       NACodecInfoRef,
    pal:        [u8; 768],
    frame:      Vec<u8>,
    frame16:    Vec<u16>,
    scale_v:    bool,
    scale_h:    bool,
    is_16bit:   bool,
}

struct Bits8 {
    queue: u8,
    fill:  u8,
}

struct Bits32 {
    queue: u32,
    fill:  u8,
}

const PREAMBLE_SIZE: usize = 4096;

impl Bits8 {
    fn new() -> Self { Bits8 { queue: 0, fill: 0 } }
    fn read_2bits(&mut self, br: &mut ByteReader) -> ByteIOResult<u8> {
        if self.fill == 0 {
            self.queue  = br.read_byte()?;
            self.fill  += 8;
        }
        let res = self.queue >> 6;
        self.queue <<= 2;
        self.fill   -= 2;
        Ok(res)
    }
}

impl Bits32 {
    fn new() -> Self { Bits32 { queue: 0, fill: 0 } }
    fn fill(&mut self, br: &mut ByteReader) -> ByteIOResult<()> {
        self.queue = br.read_u32le()?;
        self.fill  = 32;
        Ok(())
    }
    fn read_bits(&mut self, br: &mut ByteReader, nbits: u8) -> ByteIOResult<u32> {
        let res = self.queue & ((1 << nbits) - 1);
        self.queue >>= nbits;
        self.fill   -= nbits;
        if self.fill <= 16 {
            self.queue |= u32::from(br.read_u16le()?) << self.fill;
            self.fill  += 16;
        }
        Ok(res)
    }
}

impl GremlinVideoDecoder {
    fn new() -> Self {
        GremlinVideoDecoder {
            info: NACodecInfoRef::default(), pal: [0; 768],
            frame: Vec::new(), frame16: Vec::new(),
            scale_v: false, scale_h: false, is_16bit: false,
        }
    }

    fn lz_copy(&mut self, idx: usize, offset: isize, len: usize) -> DecoderResult<()> {
        if idx + len > self.frame.len() { return Err(DecoderError::InvalidData); }
        if offset == -1 {
            let c = self.frame[idx - 1];
            for i in 0..len { self.frame[idx + i] = c; }
        } else if offset < 0 {
            let start = idx - (-offset as usize);
            for i in 0..len { self.frame[idx + i] = self.frame[start + i]; }
        } else {
            if idx + (offset as usize) + len > self.frame.len() { return Err(DecoderError::InvalidData); }
            let start = idx + (offset as usize);
            for i in 0..len { self.frame[idx + i] = self.frame[start + i]; }
        }
        Ok(())
    }

    fn lz_copy16(&mut self, idx: usize, offset: isize, len: usize) -> DecoderResult<()> {
        if idx + len > self.frame16.len() { return Err(DecoderError::InvalidData); }
        if offset == -1 {
            let c = self.frame16[idx - 1];
            for i in 0..len { self.frame16[idx + i] = c; }
        } else if offset < 0 {
            let start = idx - (-offset as usize);
            for i in 0..len { self.frame16[idx + i] = self.frame16[start + i]; }
        } else {
            if idx + (offset as usize) + len > self.frame16.len() { return Err(DecoderError::InvalidData); }
            let start = idx + (offset as usize);
            for i in 0..len { self.frame16[idx + i] = self.frame16[start + i]; }
        }
        Ok(())
    }

    fn rescale(&mut self, w: usize, h: usize, scale_v: bool, scale_h: bool) {
        if (self.scale_v == scale_v) && (self.scale_h == scale_h) { return; }

        if self.scale_h && self.scale_v {
            for j in 0..h {
                let y = h - j - 1;
                for i in 0..w {
                    let x = w - i - 1;
                    self.frame[PREAMBLE_SIZE + x + y * w] = self.frame[PREAMBLE_SIZE + x/2 + (y/2) * (w/2)];
                }
            }
        } else if self.scale_h {
            for j in 0..h {
                let y = h - j - 1;
                for x in 0..w {
                    self.frame[PREAMBLE_SIZE + x + y * w] = self.frame[PREAMBLE_SIZE + x + (y/2) * w];
                }
            }
        } else if self.scale_v {
            for j in 0..h {
                let y = h - j - 1;
                for i in 0..w {
                    let x = w - i - 1;
                    self.frame[PREAMBLE_SIZE + x + y * w] = self.frame[PREAMBLE_SIZE + x/2 + y * (w/2)];
                }
            }
        }

        if scale_h && scale_v {
            for y in 0..h/2 {
                for x in 0..w/2 {
                    self.frame[PREAMBLE_SIZE + x + y * (w/2)] = self.frame[PREAMBLE_SIZE + x*2 + y*2 * w];
                }
            }
        } else if scale_h {
            for y in 0..h/2 {
                for x in 0..w {
                    self.frame[PREAMBLE_SIZE + x + y * w] = self.frame[PREAMBLE_SIZE + x + y*2 * w];
                }
            }
        } else if scale_v {
            for y in 0..h {
                for x in 0..w/2 {
                    self.frame[PREAMBLE_SIZE + x + y * w] = self.frame[PREAMBLE_SIZE + x*2 + y * w];
                }
            }
        }

        self.scale_v = scale_v;
        self.scale_h = scale_h;
    }

    fn rescale16(&mut self, w: usize, h: usize, scale_v: bool, scale_h: bool) {
        if (self.scale_v == scale_v) && (self.scale_h == scale_h) { return; }

        if self.scale_h && self.scale_v {
            for j in 0..h {
                let y = h - j - 1;
                for i in 0..w {
                    let x = w - i - 1;
                    self.frame16[PREAMBLE_SIZE + x + y * w] = self.frame16[PREAMBLE_SIZE + x/2 + (y/2) * (w/2)];
                }
            }
        } else if self.scale_h {
            for j in 0..h {
                let y = h - j - 1;
                for x in 0..w {
                    self.frame16[PREAMBLE_SIZE + x + y * w] = self.frame16[PREAMBLE_SIZE + x + (y/2) * w];
                }
            }
        } else if self.scale_v {
            for j in 0..h {
                let y = h - j - 1;
                for i in 0..w {
                    let x = w - i - 1;
                    self.frame16[PREAMBLE_SIZE + x + y * w] = self.frame16[PREAMBLE_SIZE + x/2 + y * (w/2)];
                }
            }
        }

        if scale_h && scale_v {
            for y in 0..h/2 {
                for x in 0..w/2 {
                    self.frame16[PREAMBLE_SIZE + x + y * (w/2)] = self.frame16[PREAMBLE_SIZE + x*2 + y*2 * w];
                }
            }
        } else if scale_h {
            for y in 0..h/2 {
                for x in 0..w {
                    self.frame16[PREAMBLE_SIZE + x + y * w] = self.frame16[PREAMBLE_SIZE + x + y*2 * w];
                }
            }
        } else if scale_v {
            for y in 0..h {
                for x in 0..w/2 {
                    self.frame16[PREAMBLE_SIZE + x + y * w] = self.frame16[PREAMBLE_SIZE + x*2 + y * w];
                }
            }
        }

        self.scale_v = scale_v;
        self.scale_h = scale_h;
    }

    fn output_frame(&mut self, bufinfo: &mut NABufferType, w: usize, h: usize) {
        let bufo = bufinfo.get_vbuf();
        let mut buf = bufo.unwrap();
        let paloff = buf.get_offset(1);
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();
        let mut sidx = PREAMBLE_SIZE;
        let mut didx = 0;

        dst[paloff..][..768].copy_from_slice(&self.pal);
        if !self.scale_v && !self.scale_h {
            for _ in 0..h {
                dst[didx..][..w].copy_from_slice(&self.frame[sidx..][..w]);
                sidx += w;
                didx += stride;
            }
        } else {
            for y in 0..h {
                if !self.scale_v {
                    dst[didx..][..w].copy_from_slice(&self.frame[sidx..][..w]);
                } else {
                    for x in 0..w { dst[didx + x] = self.frame[sidx + x/2]; }
                }
                if !self.scale_h || ((y & 1) == 1) {
                    sidx += if !self.scale_v { w } else { w/2 };
                }
                didx += stride;
            }
        }
    }

    fn output_frame16(&mut self, bufinfo: &mut NABufferType, w: usize, h: usize) {
        let bufo = bufinfo.get_vbuf16();
        let mut buf = bufo.unwrap();
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        if !self.scale_v && !self.scale_h {
            for (dline, sline) in dst.chunks_mut(stride).zip(self.frame16[PREAMBLE_SIZE..].chunks(w)).take(h) {
                dline[..w].copy_from_slice(sline);
            }
        } else if !self.scale_h {
            for (dline, sline) in dst.chunks_mut(stride).zip(self.frame16[PREAMBLE_SIZE..].chunks(w / 2)).take(h) {
                for (dst, &src) in dline.chunks_mut(2).zip(sline.iter()) {
                    dst[0] = src;
                    dst[1] = src;
                }
            }
        } else if !self.scale_v {
            for (dline, sline) in dst.chunks_mut(stride * 2).zip(self.frame16[PREAMBLE_SIZE..].chunks(w)).take(h) {
                dline[..w].copy_from_slice(sline);
                dline[stride..][..w].copy_from_slice(sline);
            }
        } else {
            for (dline, sline) in dst.chunks_mut(stride).zip(self.frame16[PREAMBLE_SIZE..].chunks(w / 2)).take(h) {
                for x in 0..w/2 {
                    dline[x * 2]              = sline[x];
                    dline[x * 2 + 1]          = sline[x];
                    dline[x * 2     + stride] = sline[x];
                    dline[x * 2 + 1 + stride] = sline[x];
                }
            }
        }
    }

    fn decode_method2(&mut self, br: &mut ByteReader) -> DecoderResult<()> {
        let mut bits = Bits8::new();

        let mut size = self.info.get_properties().get_video_info().unwrap().get_width() *
                        self.info.get_properties().get_video_info().unwrap().get_height();
        let mut idx = PREAMBLE_SIZE;
        if self.frame[8] != 0 {
            for c in 0..256 {
                for i in 0..16 { self.frame[c * 16 + i] = c as u8; }
            }
        }
        while size > 0 {
            let tag = bits.read_2bits(br)?;
            if tag == 0 {
                self.frame[idx] = br.read_byte()?;
                size -= 1;
                idx  += 1;
            } else if tag == 1 {
                let b = br.read_byte()?;
                let len = ((b & 0xF) as usize) + 3;
                let bot = (b >> 4) as isize;
                let off = ((br.read_byte()? as isize) << 4) + bot - 4096;
                validate!(len <= size);
                size -= len;
                self.lz_copy(idx, off, len)?;
                idx += len;
            } else if tag == 2 {
                let len = (br.read_byte()? as usize) + 2;
                validate!(len <= size);
                size -= len;
                idx += len;
            } else {
                break;
            }
        }
        Ok(())
    }

    fn decode_method2_16bit(&mut self, br: &mut ByteReader) -> DecoderResult<()> {
        let mut bits = Bits8::new();

        if self.frame16[8] != 64 {
            for (i, dst) in self.frame16[..PREAMBLE_SIZE].chunks_mut(4).enumerate() {
                let val = (i as u16) << 5;
                for el in dst.iter_mut() {
                    *el = val;
                }
            }
        }

        let mut size = self.info.get_properties().get_video_info().unwrap().get_width() *
                        self.info.get_properties().get_video_info().unwrap().get_height();
        let mut idx = PREAMBLE_SIZE;
        while size > 0 {
            let tag = bits.read_2bits(br)?;
            if tag == 0 {
                self.frame16[idx] = br.read_u16le()?;
                size -= 1;
                idx  += 1;
            } else if tag == 1 {
                let b = br.read_byte()?;
                let len = ((b & 0xF) as usize) + 3;
                let bot = (b >> 4) as isize;
                let off = ((br.read_byte()? as isize) << 4) + bot - 4096;
                validate!(len <= size);
                size -= len;
                self.lz_copy16(idx, off, len)?;
                idx += len;
            } else if tag == 2 {
                let len = (br.read_byte()? as usize) + 2;
                validate!(len <= size);
                size -= len;
                idx += len;
            } else {
                break;
            }
        }
        Ok(())
    }

    fn decode_method5(&mut self, br: &mut ByteReader, skip: usize) -> DecoderResult<()> {
        let mut bits = Bits8::new();

        let mut size = self.info.get_properties().get_video_info().unwrap().get_width() *
                        self.info.get_properties().get_video_info().unwrap().get_height();
        let mut idx = PREAMBLE_SIZE;
        validate!(size >= skip);
        size -= skip;
        idx  += skip;
        while size > 0 {
            let tag = bits.read_2bits(br)?;
            if tag == 0 {
                self.frame[idx] = br.read_byte()?;
                size -= 1;
                idx  += 1;
            } else if tag == 1 {
                let b = br.read_byte()?;
                let len = ((b & 0xF) as usize) + 3;
                let bot = (b >> 4) as isize;
                let off = ((br.read_byte()? as isize) << 4) + bot - 4096;
                validate!(len <= size);
                size -= len;
                self.lz_copy(idx, off, len)?;
                idx += len;
            } else if tag == 2 {
                let b = br.read_byte()?;
                if b == 0 { break; }
                let len: usize = (if b != 0xFF { b as usize } else { br.read_u16le()? as usize }) + 1;
                validate!(len <= size);
                size -= len;
                idx += len;
            } else {
                let b = br.read_byte()?;
                let len = ((b & 0x3) as usize) + 2;
                let off = -((b >> 2) as isize) - 1;
                validate!(len <= size);
                size -= len;
                self.lz_copy(idx, off, len)?;
                idx += len;
            }
        }
        Ok(())
    }

    #[allow(clippy::identity_op)]
    fn decode_method68(&mut self, br: &mut ByteReader,
                       skip: usize, use8: bool) -> DecoderResult<()> {
        let mut bits = Bits32::new();

        let mut size = self.info.get_properties().get_video_info().unwrap().get_width() *
                        self.info.get_properties().get_video_info().unwrap().get_height();
        let mut idx = PREAMBLE_SIZE;
        validate!(size >= skip);
        size -= skip;
        idx  += skip;
        bits.fill(br)?;
        while size > 0 {
            let tag = bits.read_bits(br, 2)?;
            if tag == 0 { //draw
                let b = bits.read_bits(br, 1)?;
                if b == 0 {
                    self.frame[idx] = br.read_byte()?;
                    size -= 1;
                    idx  += 1;
                } else {
                    let mut len: usize = 2;
                    let mut lbits = 0;
                    loop {
                        lbits += 1;
                        let val = bits.read_bits(br, lbits)?;
                        len += val as usize;
                        if val != ((1 << lbits) - 1) { break; }
                        validate!(lbits < 16);
                    }
                    validate!(len <= size);
                    for i in 0..len { self.frame[idx + i] = br.read_byte()?; }
                    size -= len;
                    idx  += len;
                }
            } else if tag == 1 { //skip
                let b = bits.read_bits(br, 1)?;
                let len: usize;
                if b == 0 {
                    len = (bits.read_bits(br, 4)? as usize) + 2;
                } else {
                    let bb = br.read_byte()?;
                    if (bb & 0x80) == 0 {
                        len = (bb as usize) + 18;
                    } else {
                        let top = ((bb & 0x7F) as usize) << 8;
                        len = top + (br.read_byte()? as usize) + 146;
                    }
                }
                validate!(len <= size);
                size -= len;
                idx  += len;
            } else if tag == 2 {
                let subtag = bits.read_bits(br, 2)? as usize;
                if subtag != 3 {
                    let top = (bits.read_bits(br, 4)? as usize) << 8;
                    let offs = top + (br.read_byte()? as usize);
                    if (subtag != 0) || (offs <= 0xF80) {
                        let len = subtag + 3;
                        self.lz_copy(idx, (offs as isize) - 4096, len)?;
                        idx += len;
                    } else {
                        if offs == 0xFFF { return Ok(()); }
                        let real_off = ((offs >> 4) & 0x7) + 1;
                        let len = ((offs & 0xF) + 2) * 2;
                        validate!(len <= size);
                        size -= len;
                        let c1 = self.frame[idx - real_off];
                        let c2 = self.frame[idx - real_off + 1];
                        for i in 0..len/2 {
                            self.frame[idx + i*2 + 0] = c1;
                            self.frame[idx + i*2 + 1] = c2;
                        }
                        idx += len;
                    }
                } else {
                    let b = br.read_byte()?;
                    let off = ((b & 0x7F) as usize) + 1;
                    let len = if (b & 0x80) == 0 { 2 } else { 3 };
                    validate!(len <= size);
                    size -= len;
                    self.lz_copy(idx, -(off as isize), len)?;
                    idx += len;
                }
            } else {
                let len: usize;
                let off: isize;
                if use8 {
                    let b = br.read_byte()?;
                    if (b & 0xC0) == 0xC0 {
                        len = ((b & 0x3F) as usize) + 8;
                        let q = bits.read_bits(br, 4)? as isize;
                        off = (q << 8) + (br.read_byte()? as isize) + 1;
                    } else {
                        let ofs1: isize;
                        if (b & 0x80) == 0 {
                            len = ((b >> 4) as usize) + 6;
                            ofs1 = (b & 0xF) as isize;
                        } else {
                            len = ((b & 0x3F) as usize) + 14;
                            ofs1 = bits.read_bits(br, 4)? as isize;
                        }
                        off = (ofs1 << 8) + (br.read_byte()? as isize) - 4096;
                    }
                } else {
                    let b = br.read_byte()?;
                    if (b >> 4) == 0xF {
                        len = (br.read_byte()? as usize) + 21;
                    } else {
                        len = ((b >> 4) as usize) + 6;
                    }
                    let ofs1 = (b & 0xF) as isize;
                    off = (ofs1 << 8) + (br.read_byte()? as isize) - 4096;
                }
                validate!(len <= size);
                size -= len;
                self.lz_copy(idx, off, len)?;
                idx += len;
            }
        }
        Ok(())
    }
}

const RGB555_FORMAT: NAPixelFormaton = NAPixelFormaton { model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
                                        comp_info: [
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift: 10, comp_offs: 0, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  5, comp_offs: 1, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  0, comp_offs: 2, next_elem: 2 }),
                                            None, None],
                                        elem_size: 2, be: false, alpha: false, palette: false };

impl NADecoder for GremlinVideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            self.is_16bit = !vinfo.get_format().is_paletted();
            let fmt = if !self.is_16bit { PAL8_FORMAT } else { RGB555_FORMAT };
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            if !self.is_16bit {
                self.frame.resize(PREAMBLE_SIZE + w * h, 0);
                for i in 0..2 {
                    for j in 0..256 {
                        for k in 0..8 {
                            self.frame[i * 2048 + j * 8 + k] = j as u8;
                        }
                    }
                }
                let edata = info.get_extradata().unwrap();
                validate!(edata.len() == 768);
                for c in 0..256 {
                    for i in 0..3 {
                        let cc = edata[c * 3 + i];
                        self.pal[c * 3 + i] = (cc << 2) | (cc >> 4);
                    }
                }
            } else {
                self.frame16.resize(PREAMBLE_SIZE + w * h, 0);
            }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);
        let flags = br.read_u32le()?;
        let w = self.info.get_properties().get_video_info().unwrap().get_width();
        let h = self.info.get_properties().get_video_info().unwrap().get_height();

        let cmethod = flags & 0xF;
        let is_intra = (flags & 0x40) != 0;
        let scale_v = (flags & 0x10) != 0;
        let scale_h = (flags & 0x20) != 0;

        if !self.is_16bit {
            self.rescale(w, h, scale_v, scale_h);
        } else {
            self.rescale16(w, h, scale_v, scale_h);
        }

        if self.is_16bit && (cmethod != 2) && (cmethod != 3) {
            return Err(DecoderError::NotImplemented);
        }
        if (cmethod == 0) || (cmethod == 1) {
            validate!(!self.is_16bit);
            for c in 0..256 {
                for i in 0..3 {
                    let b = br.read_byte()?;
                    self.pal[c * 3 + i] = (b << 2) | (b >> 4);
                }
            }
            if cmethod == 1 {
                for i in PREAMBLE_SIZE..self.frame.len() { self.frame[i] = 0x00; }
            }
            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::None);
            frm.set_keyframe(false);
            frm.set_frame_type(FrameType::Skip);
            return Ok(frm.into_ref())
        } else if cmethod == 3 {
            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::None);
            frm.set_keyframe(false);
            frm.set_frame_type(FrameType::Skip);
            return Ok(frm.into_ref())
        } else if cmethod == 2 {
            if !self.is_16bit {
                self.decode_method2(&mut br)?;
            } else {
                self.decode_method2_16bit(&mut br)?;
            }
        } else if cmethod == 5 {
            self.decode_method5(&mut br, (flags >> 8) as usize)?;
        } else if cmethod == 6 {
            self.decode_method68(&mut br, (flags >> 8) as usize, false)?;
        } else if cmethod == 8 {
            self.decode_method68(&mut br, (flags >> 8) as usize, true)?;
        } else {
            return Err(DecoderError::NotImplemented);
        }

        let mut bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;

        if !self.is_16bit {
            self.output_frame(&mut bufinfo, w, h);
        } else {
            self.output_frame16(&mut bufinfo, w, h);
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for GremlinVideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_video() -> Box<dyn NADecoder + Send> {
    Box::new(GremlinVideoDecoder::new())
}

struct GremlinAudioDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    delta_tab: [i16; 256],
    state0:     i16,
    state1:     i16,
}

impl GremlinAudioDecoder {
    fn new() -> Self {
        let mut delta_tab: [i16; 256] = [0; 256];
        let mut delta = 0;
        let mut code = 64;
        let mut step = 45;
        for i in 0..127 {
            delta += code >> 5;
            code  += step;
            step  += 2;
            delta_tab[i * 2 + 1] =  delta;
            delta_tab[i * 2 + 2] = -delta;
        }
        delta_tab[255] = 32767;//delta + (code >> 5);
        GremlinAudioDecoder {
            ainfo:  NAAudioInfo::new(0, 1, formats::SND_S16_FORMAT, 0),
            chmap:  NAChannelMap::new(),
            delta_tab,
            state0: 0,
            state1: 0,
        }
    }
}

const CHMAP_MONO: [NAChannelType; 1] = [NAChannelType::C];
const CHMAP_STEREO: [NAChannelType; 2] = [NAChannelType::L, NAChannelType::R];

fn get_default_chmap(nch: u8) -> NAChannelMap {
    let mut chmap = NAChannelMap::new();
    match nch {
        1 => chmap.add_channels(&CHMAP_MONO),
        2 => chmap.add_channels(&CHMAP_STEREO),
        _ => (),
    }
    chmap
}

impl NADecoder for GremlinAudioDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), ainfo.get_channels(), formats::SND_S16P_FORMAT, ainfo.get_block_len());
            self.chmap = get_default_chmap(ainfo.get_channels());
            if self.chmap.num_channels() == 0 { return Err(DecoderError::InvalidData); }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            let samples = pktbuf.len() / self.chmap.num_channels();
            let abuf = alloc_audio_buffer(self.ainfo, samples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let off1 = adata.get_offset(1);
            let buf = adata.get_data_mut().unwrap();
            if self.chmap.num_channels() == 2 {
                for (i, e) in pktbuf.chunks(2).enumerate() {
                    self.state0 = self.state0.wrapping_add(self.delta_tab[e[0] as usize]);
                    buf[i] = self.state0;
                    self.state1 = self.state1.wrapping_add(self.delta_tab[e[1] as usize]);
                    buf[off1 + i] = self.state1;
                }
            } else {
                for (i, e) in pktbuf.iter().enumerate() {
                    self.state0 += self.delta_tab[*e as usize];
                    buf[i] = self.state0;
                }
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

impl NAOptionHandler for GremlinAudioDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_audio() -> Box<dyn NADecoder + Send> {
    Box::new(GremlinAudioDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;

    // samples: intro1.gdv from Normality, SHELI_S.GDV from Jungle Strike
    #[test]
    fn test_gdv_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("gdv", "gdv-video", "assets/Game/intro1.gdv", Some(10), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x7ea302bf, 0xc3e210cf, 0x6e341376, 0x9e976056]));
    }
    #[test]
    fn test_gdv_video16() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("gdv", "gdv-video", "assets/Game/SHELI_S.GDV", Some(10), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x9acfdefd, 0x12687f8c, 0x9ef1cd56, 0x019240bb]));
    }
    #[test]
    fn test_gdv_audio() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("gdv", "gdv-audio", "assets/Game/intro1.gdv", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xd41d8cd9, 0x8f00b204, 0xe9800998, 0xecf8427e]));
    }
}
