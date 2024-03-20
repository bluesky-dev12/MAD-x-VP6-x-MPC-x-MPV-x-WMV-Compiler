use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

const DICT_SIZE: usize = 4096;
const MAX_BITS:   u8 = 12;
const INVALID_POS: usize = 65536;

struct BitReader<'a> {
    src:    &'a [u8],
    pos:    usize,
    left:   u8,
    bitbuf: u32,
    bits:   u8,
}

impl<'a> BitReader<'a> {
    fn new(src: &'a [u8]) -> Self {
        Self {
            src,
            pos:    0,
            left:   0,
            bitbuf: 0,
            bits:   0,
        }
    }
    fn read(&mut self, nbits: u8) -> DecoderResult<u32> {
        while self.bits < nbits {
            while self.left > 0 && self.bits <= 24 {
                self.bitbuf |= u32::from(self.src[self.pos]) << self.bits;
                self.bits += 8;
                self.pos  += 1;
                self.left -= 1;
            }
            if self.bits < nbits {
                if self.pos >= self.src.len() {
                    return Err(DecoderError::ShortData);
                }
                self.left = self.src[self.pos];
                self.pos += 1;
                validate!(self.left > 0);
                if self.pos + usize::from(self.left) > self.src.len() {
                    return Err(DecoderError::ShortData);
                }
            }
        }
        let ret = self.bitbuf & ((1 << nbits) - 1);
        self.bitbuf >>= nbits;
        self.bits    -= nbits;
        Ok(ret)
    }
}

struct LZWState {
    dict_sym:   [u8; DICT_SIZE],
    dict_prev:  [u16; DICT_SIZE],
    dict_pos:   usize,
    dict_lim:   usize,
    nsyms:      usize,
    idx_bits:   u8,
}

impl LZWState {
    fn new() -> Self {
        Self {
            dict_sym:   [0; DICT_SIZE],
            dict_prev:  [0; DICT_SIZE],
            dict_pos:   0,
            dict_lim:   0,
            idx_bits:   0,
            nsyms:      0,
        }
    }
    fn reset(&mut self, bits: u8) {
        self.nsyms    = (1 << bits) + 2;
        self.dict_pos = self.nsyms;
        self.dict_lim = 1 << (bits + 1);
        self.idx_bits = bits + 1;
    }
    fn add(&mut self, prev: usize, sym: u8) {
        if self.dict_pos < self.dict_lim {
            self.dict_sym [self.dict_pos] = sym;
            self.dict_prev[self.dict_pos] = prev as u16;
            self.dict_pos += 1;
        }
    }
    fn decode_idx(&self, dst: &mut [u8], pos: usize, idx: usize) -> DecoderResult<usize> {
        let mut tot_len = 1;
        let mut tidx = idx;
        while tidx >= self.nsyms {
            tidx = self.dict_prev[tidx] as usize;
            tot_len += 1;
        }
        validate!(pos + tot_len <= dst.len());

        let mut end = pos + tot_len - 1;
        let mut tidx = idx;
        while tidx >= self.nsyms {
            dst[end] = self.dict_sym[tidx];
            end -= 1;
            tidx = self.dict_prev[tidx] as usize;
        }
        dst[end] = tidx as u8;

        Ok(tot_len)
    }
    fn unpack(&mut self, src: &[u8], dst: &mut [u8]) -> DecoderResult<()> {
        validate!(src.len() >= 4);
        let mut br = BitReader::new(&src[1..]);

        let bits = src[0];
        validate!(bits > 0);
        let reset_sym = 1 << bits;
        let end_sym = reset_sym + 1;

        self.reset(bits);

        let mut pos = 0;
        let mut lastidx = INVALID_POS;
        loop {
            let idx         = br.read(self.idx_bits)? as usize;
            if idx == reset_sym {
                self.reset(bits);
                lastidx = INVALID_POS;
                continue;
            }
            if idx == end_sym {
                break;
            }
            validate!(idx <= self.dict_pos);
            if idx != self.dict_pos {
                let len = self.decode_idx(dst, pos, idx)?;
                if lastidx != INVALID_POS {
                    self.add(lastidx, dst[pos]);
                }
                pos += len;
            } else {
                validate!(lastidx != INVALID_POS);
                let len = self.decode_idx(dst, pos, lastidx)?;
                let lastsym = dst[pos];
                pos += len;
                validate!(pos < dst.len());
                dst[pos] = lastsym;
                pos += 1;
                self.add(lastidx, lastsym);
            }

            lastidx = idx;
            if self.dict_pos == self.dict_lim && self.idx_bits < MAX_BITS {
                self.dict_lim <<= 1;
                self.idx_bits += 1;
            }
        }
        validate!(pos == dst.len());
        validate!(br.pos + 2 == src.len());
        Ok(())
    }
}

struct GIFDecoder {
    info:       NACodecInfoRef,
    gpal:       [u8; 768],
    lpal:       [u8; 768],
    frame:      Vec<u8>,
    dbuf:       Vec<u8>,
    width:      usize,
    height:     usize,
    lzw:        LZWState,
    transp:     Option<u8>,
}

impl GIFDecoder {
    fn new() -> Self {
        Self {
            info:       NACodecInfoRef::default(),
            gpal:       [0; 768],
            lpal:       [0; 768],
            frame:      Vec::new(),
            dbuf:       Vec::new(),
            width:      0,
            height:     0,
            lzw:        LZWState::new(),
            transp:     None,
        }
    }
}

impl NADecoder for GIFDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.width;
            self.height = vinfo.height;
            self.transp = None;
            self.gpal   = [0; 768];
            if let Some(ref edata) = info.get_extradata() {
                validate!(edata.len() >= 3);
                if edata[1] != 0 {
                    self.transp = Some(edata[1]);
                }
                self.gpal[..edata.len() - 3].copy_from_slice(&edata[3..]);
            }
            self.frame  = vec![0; self.width * self.height];
            self.dbuf   = vec![0; self.width * self.height];
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, false, PAL8_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 0);

        for sd in pkt.side_data.iter() {
            if let NASideData::Palette(true, ref pal) = sd {
                for (dst, src) in self.gpal.chunks_mut(3).zip(pal.chunks(4)) {
                    dst[0] = src[0];
                    dst[1] = src[1];
                    dst[2] = src[2];
                }
                break;
            }
        }

        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);
        let tag                         = br.read_byte()?;
        validate!(tag == 0x2C);
        let left                        = usize::from(br.read_u16le()?);
        let top                         = usize::from(br.read_u16le()?);
        let width                       = usize::from(br.read_u16le()?);
        let height                      = usize::from(br.read_u16le()?);
        validate!(width > 0 && height > 0);
        validate!(left + width <= self.width && top + height <= self.height);
        let flags                       = br.read_byte()?;
        let local_pal = (flags & 0x80) != 0;
        if local_pal {
            let csize = 3 << ((flags & 7) + 1);
                                          br.read_buf(&mut self.lpal[..csize])?;
        }

        let start = br.tell() as usize;
        self.dbuf.resize(width * height, 0);
        self.lzw.unpack(&src[start..], &mut self.dbuf)?;

        if let Some(tpix) = self.transp {
            for (dline, sline) in self.frame.chunks_exact_mut(self.width).skip(top)
                    .zip(self.dbuf.chunks_exact(width)) {
                for (dst, &src) in dline[left..][..width].iter_mut().zip(sline.iter()) {
                    if src != tpix {
                        *dst = src;
                    }
                }
            }
        } else {
            for (dline, sline) in self.frame.chunks_exact_mut(self.width).skip(top)
                    .zip(self.dbuf.chunks_exact(width)) {
                dline[left..][..width].copy_from_slice(sline);
            }
        }

        let buf = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
        let mut vbuf = buf.get_vbuf().unwrap();
        let paloff = vbuf.get_offset(1);
        let stride = vbuf.get_stride(0);
        let data = vbuf.get_data_mut().unwrap();

        for (drow, srow) in data.chunks_exact_mut(stride).zip(self.frame.chunks_exact(self.width)) {
            drow[..self.width].copy_from_slice(srow);
        }
        data[paloff..][..768].copy_from_slice(if local_pal { &self.lpal } else { &self.gpal });

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), buf);
        let ftype = if pkt.keyframe { FrameType::I } else { FrameType::P };
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for GIFDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(GIFDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::*;

    // sample: https://samples.mplayerhq.hu/image-samples/GIF/3D.gif
    #[test]
    fn test_gif_decoder() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);

        test_decoding("gif", "gif", "assets/Misc/3D.gif",
                      Some(2), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x95e68f8f, 0xe899ac86, 0x0af66a0a, 0x34a4a00e],
                            [0xdf920e8c, 0xeb57c5f8, 0xd862507e, 0xd733fca3],
                            [0x75bee5cb, 0xefb2076c, 0xfce61f8a, 0x2d2b30df]]));
    }
}
