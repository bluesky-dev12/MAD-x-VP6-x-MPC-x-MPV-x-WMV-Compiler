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
struct RpzaDecoder {
    info:       NACodecInfoRef,
    hams:       HAMShuffler<u16>,
    width:      usize,
    height:     usize,
}

impl RpzaDecoder {
    fn new() -> Self {
        Self::default()
    }
    fn put_block(dst: &mut [u16], dstride: usize, block: &[u16; 16]) {
        for (line, src) in dst.chunks_mut(dstride).take(4).zip(block.chunks(4)) {
            line[..4].copy_from_slice(src);
        }
    }
}

fn div3(a: u16, b: u16) -> u16 {
    let r0 = (a >> 10) & 0x1F;
    let g0 = (a >>  5) & 0x1F;
    let b0 =  a        & 0x1F;
    let r1 = (b >> 10) & 0x1F;
    let g1 = (b >>  5) & 0x1F;
    let b1 =  b        & 0x1F;

    let r = (r0 * 21 + r1 * 11) / 32;
    let g = (g0 * 21 + g1 * 11) / 32;
    let b = (b0 * 21 + b1 * 11) / 32;

    (r << 10) | (g << 5) | b
}

impl NADecoder for RpzaDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = (vinfo.get_width()  + 3) & !3;
            self.height = (vinfo.get_height() + 3) & !3;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, false, RGB555_FORMAT));
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
        validate!(id == 0xE1);
        let length                      = br.read_u24be()? as usize;
        validate!(length == src.len());

        let bufret = self.hams.clone_ref();
        let mut buf;
        if let Some(bbuf) = bufret {
            buf = bbuf;
        } else {
            let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 2)?;
            buf = bufinfo.get_vbuf16().unwrap();
            self.hams.add_frame(buf);
            buf = self.hams.get_output_frame().unwrap();
        }
        let frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
        let stride = frm.stride[0];
        let mut doff = frm.offset[0];

        let mut x = 0;
        let nblocks = (self.width / 4) * (self.height / 4);
        let mut blockpos = 0;
        let mut has_skips = false;
        let mut block = [0u16; 16];
        while blockpos < nblocks {
            let opcode                  = br.read_byte()?;
            if (opcode & 0x80) == 0 {
                let b2                  = br.read_byte()?;
                let clr0 = u16::from(opcode) * 256 + u16::from(b2);
                if (br.peek_byte()? & 0x80) == 0 {
                    block[0] = clr0;
                    for el in block.iter_mut().skip(1) {
                        *el             = br.read_u16be()?;
                    }
                } else {
                    let clr3            = br.read_u16be()?;
                    let clr1 = div3(clr0, clr3);
                    let clr2 = div3(clr3, clr0);
                    let clr = [clr3, clr2, clr1, clr0];
                    let flags       = br.read_u32be()? as usize;
                    for i in 0..16 {
                        block[i] = clr[(flags >> (30 - i * 2)) & 3];
                    }
                }
                Self::put_block(&mut frm.data[doff + x..], stride, &block);
                x += 4;
                if x == self.width {
                    x = 0;
                    doff += stride * 4;
                }
                blockpos += 1;
                continue;
            }
            let len = ((opcode & 0x1F) as usize) + 1;
            validate!(blockpos + len <= nblocks);
            match opcode >> 5 {
                4 => {
                    has_skips = true;
                    for _ in 0..len {
                        x += 4;
                        if x == self.width {
                            x = 0;
                            doff += stride * 4;
                        }
                    }
                },
                5 => {
                    let clr             = br.read_u16be()?;
                    block = [clr; 16];
                    for _ in 0..len {
                        Self::put_block(&mut frm.data[doff + x..], stride, &block);
                        x += 4;
                        if x == self.width {
                            x = 0;
                            doff += stride * 4;
                        }
                    }
                },
                6 => {
                    let clr0            = br.read_u16be()?;
                    let clr3            = br.read_u16be()?;
                    let clr1 = div3(clr0, clr3);
                    let clr2 = div3(clr3, clr0);
                    let clr = [clr3, clr2, clr1, clr0];
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
                    }
                },
                _ => {
                    return Err(DecoderError::InvalidData);
                },
            };
            blockpos += len;
        }

        let buftype = NABufferType::Video16(buf);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), buftype);
        frm.set_keyframe(!has_skips);
        frm.set_frame_type(if !has_skips { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.hams.clear();
    }
}

impl NAOptionHandler for RpzaDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RpzaDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_rpza() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/RPZA/aletrek-rpza.mov
        test_decoding("mov", "apple-video", "assets/QT/aletrek-rpza.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0xe7cfc941, 0xa448841b, 0x75afc888, 0x94aa064b],
                            [0xadaaec50, 0xff7319ff, 0xa1f3f64a, 0xc40c2985],
                            [0xdfa5c4b9, 0xdac2d22b, 0x14f9f281, 0x7295eae7],
                            [0x3f3420b9, 0xbe48b885, 0x91b0fb51, 0xd71462ac],
                            [0xcaae1580, 0x16eecce3, 0x2ca0dd4b, 0x7f9c62e3],
                            [0xb51fc759, 0xe1cfc171, 0xda854767, 0x878f6e17],
                            [0x8d3abf6d, 0xeecec7a2, 0x5832e5d6, 0x86145fc9]]));
    }
}
