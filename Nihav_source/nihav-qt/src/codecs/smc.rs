use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::HAMShuffler;

struct SmcDecoder {
    info:       NACodecInfoRef,
    hams:       HAMShuffler<u8>,
    width:      usize,
    height:     usize,
    pairs:      [[u8; 2]; 256],
    quads:      [[u8; 4]; 256],
    octets:     [[u8; 8]; 256],
}

impl SmcDecoder {
    fn new() -> Self {
        Self {
            info:       NACodecInfoRef::default(),
            hams:       HAMShuffler::default(),
            width:      0,
            height:     0,
            pairs:      [[0; 2]; 256],
            quads:      [[0; 4]; 256],
            octets:     [[0; 8]; 256],
        }
    }
    fn put_block(dst: &mut [u8], dstride: usize, block: &[u8; 16]) {
        for (line, src) in dst.chunks_mut(dstride).take(4).zip(block.chunks(4)) {
            line[..4].copy_from_slice(src);
        }
    }
    fn put_blocks(&self, dst: &mut [u8], stride: usize, doff: &mut usize, x: &mut usize, len: usize, block: &[u8; 16]) {
        for _ in 0..len {
            Self::put_block(&mut dst[*doff + *x..], stride, block);
            *x += 4;
            if *x == self.width {
                *x = 0;
                *doff += stride * 4;
            }

        }
    }
}

impl NADecoder for SmcDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = (vinfo.get_width()  + 3) & !3;
            self.height = (vinfo.get_height() + 3) & !3;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, false, PAL8_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() >= 2);
        let mut mr = MemoryReader::new_read(src.as_slice());
        let mut br = ByteReader::new(&mut mr);

        let _flags                      = br.read_byte()?;
        let length                      = br.read_u24be()? as usize;
        validate!(length == src.len());

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
        let frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
        let stride = frm.stride[0];
        let mut doff = frm.offset[0];

        self.pairs      = [[0; 2]; 256];
        self.quads      = [[0; 4]; 256];
        self.octets     = [[0; 8]; 256];

        let mut x = 0;
        let nblocks = (self.width / 4) * (self.height / 4);
        let mut blockpos = 0;
        let mut pair_idx = 0;
        let mut quad_idx = 0;
        let mut oct_idx = 0;
        let mut has_skips = false;
        let mut block = [0u8; 16];
        let mut lblock = [0u8; 16];
        let mut llblock = [0u8; 16];
        while blockpos < nblocks {
            let opcode                  = br.read_byte()?;
            let ext_opcode = (opcode & 0x10) != 0;
            let len = if !ext_opcode || (opcode >= 0x80) {
                    ((opcode & 0xF) as usize) + 1
                } else {
                                          (br.read_byte()? as usize) + 1
                };
            validate!(blockpos + len <= nblocks);
            match opcode >> 5 {
                0 => {
                    has_skips = true;
                    for _ in 0..len {
                        x += 4;
                        if x == self.width {
                            x = 0;
                            doff += stride * 4;
                        }
                    }
                },
                1 => {
                    self.put_blocks(frm.data, stride, &mut doff, &mut x, len, &lblock);
                    llblock = lblock;
                },
                2 => {
                    validate!(blockpos + len * 2 <= nblocks);
                    for i in 0..len*2 {
                        if (i & 1) == 0 {
                            Self::put_block(&mut frm.data[doff + x..], stride, &llblock);
                        } else {
                            Self::put_block(&mut frm.data[doff + x..], stride, &lblock);
                        }
                        x += 4;
                        if x == self.width {
                            x = 0;
                            doff += stride * 4;
                        }
                    }
                    blockpos += len;
                },
                3 => {
                    let clr             = br.read_byte()?;
                    block = [clr; 16];
                    self.put_blocks(frm.data, stride, &mut doff, &mut x, len, &block);
                    if len > 1 {
                        llblock = block;
                    } else {
                        llblock = lblock;
                    }
                    lblock = block;
                },
                4 => {
                    let clr;
                    if !ext_opcode {
                                          br.read_buf(&mut self.pairs[pair_idx])?;
                        clr = self.pairs[pair_idx];
                        pair_idx = (pair_idx + 1) & 0xFF;
                    } else {
                        let idx         = br.read_byte()? as usize;
                        clr = self.pairs[idx];
                    }
                    for _ in 0..len {
                        let flags       = br.read_u16be()? as usize;
                        for i in 0..16 {
                            block[i] = clr[(flags >> (15 - i)) & 1];
                        }
                        Self::put_block(&mut frm.data[doff + x..], stride, &block);
                        x += 4;
                        if x == self.width {
                            x = 0;
                            doff += stride * 4;
                        }
                        llblock = lblock;
                        lblock = block;
                    }
                },
                5 => {
                    let clr;
                    if !ext_opcode {
                                          br.read_buf(&mut self.quads[quad_idx])?;
                        clr = self.quads[quad_idx];
                        quad_idx = (quad_idx + 1) & 0xFF;
                    } else {
                        let idx         = br.read_byte()? as usize;
                        clr = self.quads[idx];
                    }
                    for _ in 0..len {
                        let flags       = br.read_u32be()? as usize;
                        for i in 0..16 {
                            block[i] = clr[(flags >> (30 - i * 2)) & 3];
                        }
                        Self::put_block(&mut frm.data[doff + x..], stride, &block);
                        x += 4;
                        if x == self.width {
                            x = 0;
                            doff += stride * 4;
                        }
                        llblock = lblock;
                        lblock = block;
                    }
                },
                6 => {
                    let clr;
                    if !ext_opcode {
                                          br.read_buf(&mut self.octets[oct_idx])?;
                        clr = self.octets[oct_idx];
                        oct_idx = (oct_idx + 1) & 0xFF;
                    } else {
                        let idx         = br.read_byte()? as usize;
                        clr = self.octets[idx];
                    }
                    for _ in 0..len {
                        let flg0        = br.read_u16be()? as usize;
                        let flg1        = br.read_u16be()? as usize;
                        let flg2        = br.read_u16be()? as usize;

                        let line0 = flg0 >> 4;
                        let line1 = flg1 >> 4;
                        let line2 = flg2 >> 4;
                        let line3 = ((flg0 & 0xF) << 8) | ((flg1 & 0xF) << 4) | (flg2 & 0xF);
                        let flags = [line0, line1, line2, line3];
                        for j in 0..4 {
                            let flg = flags[j];
                            for i in 0..4 {
                                block[i + j * 4] = clr[(flg >> (9 - i * 3)) & 7];
                            }
                        }
                        Self::put_block(&mut frm.data[doff + x..], stride, &block);
                        x += 4;
                        if x == self.width {
                            x = 0;
                            doff += stride * 4;
                        }
                        llblock = lblock;
                        lblock = block;
                    }
                },
                _ => {
                    validate!((opcode & 0xF0) != 0xF0);
                    for _ in 0..len {
                                          br.read_buf(&mut block)?;
                        Self::put_block(&mut frm.data[doff + x..], stride, &block);
                        x += 4;
                        if x == self.width {
                            x = 0;
                            doff += stride * 4;
                        }
                        llblock = lblock;
                        lblock = block;
                    }
                },
            };
            blockpos += len;
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
        frm.set_keyframe(!has_skips);
        frm.set_frame_type(if !has_skips { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.hams.clear();
    }
}

impl NAOptionHandler for SmcDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(SmcDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    // samples from https://samples.mplayerhq.hu/V-codecs/SMC
    #[test]
    fn test_smc() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "qt-smc", "assets/QT/aletrek-smc.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x4ea351ee, 0x279beedd, 0x71738173, 0x0fba61e8],
                            [0x2c5ea61c, 0x1f539c55, 0x9234aa95, 0xd60be3ef],
                            [0x307955c9, 0x0bc6d4cb, 0x7bebef1e, 0x5f2f3aee],
                            [0x00db3e90, 0x9f57baef, 0x23e6c43b, 0xe9d6dc44],
                            [0x54f2e3eb, 0x6313a0df, 0xb4b52777, 0x10f020f0],
                            [0xdefb0b7d, 0xbdaa77c8, 0xb053d60c, 0xe836b9b0],
                            [0xb820f95b, 0x0ce11d8a, 0xcfd8f623, 0x2b3acb1d]]));
    }
    #[test]
    fn test_smc_gray() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        test_decoding("mov", "qt-smc", "assets/QT/aletrek-smc-gray.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x72e87d61, 0x85cde631, 0x780eea35, 0x7dfb8dfb],
                            [0xc41fa1a8, 0x1f038e2e, 0xd67189fd, 0x566e74a8],
                            [0xc2d8e57f, 0x501c5618, 0xdd6cd85a, 0x2c05f0b0],
                            [0x7d530e44, 0xf02646d6, 0xbab960d0, 0x39ea2344],
                            [0x46a2d3dc, 0x93684f3e, 0x88102523, 0x02d19236],
                            [0x78995cd8, 0x3fbcbba2, 0x692d6e19, 0x25334ed0],
                            [0x9632de1a, 0xccbe20a8, 0x4cc5fabe, 0x4dadddbe]]));
    }
}
