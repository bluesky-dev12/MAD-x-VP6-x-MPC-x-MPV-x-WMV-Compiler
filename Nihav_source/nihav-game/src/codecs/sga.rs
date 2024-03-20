use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

const RGB555_FORMAT: NAPixelFormaton = NAPixelFormaton { model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
                                        comp_info: [
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift: 10, comp_offs: 0, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  5, comp_offs: 1, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  0, comp_offs: 2, next_elem: 2 }),
                                            None, None],
                                        elem_size: 2, be: false, alpha: false, palette: false };

const OPCODE_SKIP: usize = 91;

struct DPVideoDecoder {
    info:       NACodecInfoRef,
    pal:        [u16; 256],
    subtype:    u8,
    frame:      Vec<u8>,
    width:      usize,
    height:     usize,
}

impl DPVideoDecoder {
    fn new() -> Self {
        Self {
            info:       NACodecInfoRef::default(),
            pal:        [0; 256],
            subtype:    0,
            frame:      Vec::new(),
            width:      0,
            height:     0,
        }
    }
    fn decode_81(&mut self, br: &mut ByteReader) -> DecoderResult<()> {
        let mut pat = [0; 8];
        let mut cur_clr = [0; 16];
        let mut off = 0;
        let mut rle_len = 0;
        let mut src_off = 0;
        let mut had_skips = false;
        for _y in (0..self.height).step_by(8) {
            for x in (0..self.width).step_by(8) {
                let off = off + x;
                if rle_len > 0 {
                    copy_block(&mut self.frame, self.width, src_off, off, if had_skips { 15 } else { 0 });
                    rle_len -= 1;
                    continue;
                }

                let mut op              = usize::from(br.read_byte()?);
                if (op & 0x80) != 0 {
                    rle_len = (op & 0x7F) + 1;
                    op                  = usize::from(br.read_byte()?);
                    src_off = off;
                    had_skips = false;
                }
                validate!(op < 0x80);
                match OPCODE_TYPE[op] {
                    4 => {
                        if op < OPCODE_SKIP {
                            let nclrs = usize::from(PATTERN_COLOURS[op]);
                            assert_eq!(nclrs, 2);
                            br.read_buf(&mut cur_clr[..nclrs])?;
                            let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                            let psrc = if pattern == 0 {
                                    br.read_buf(&mut pat)?;
                                    &pat
                                } else {
                                    &PATTERN_8X8[pattern]
                                };
                            paint_8x8(&mut self.frame[off..], self.width, psrc, &cur_clr);
                        } else {
                            had_skips = true;
                        }
                    },
                    2 => {
                        if op < OPCODE_SKIP {
                            let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                            let nclrs = usize::from(PATTERN_COLOURS[op]);
                            br.read_buf(&mut cur_clr[..nclrs])?;
                            let mask = br.read_u32le()?;
                            paint_8x4_old(&mut self.frame[off..], self.width, &PATTERN_8X4_OLD[pattern], &cur_clr, mask);
                        }
                        let op = usize::from(br.read_byte()?);
                        validate!(op < 0x80);
                        match OPCODE_TYPE[op] {
                            2 => {
                                if op < OPCODE_SKIP {
                                    let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                    let nclrs = usize::from(PATTERN_COLOURS[op]);
                                    br.read_buf(&mut cur_clr[..nclrs])?;
                                    let mask = br.read_u32le()?;
                                    paint_8x4_old(&mut self.frame[off + self.width * 4..], self.width, &PATTERN_8X4_OLD[pattern], &cur_clr, mask);
                                }
                            },
                            0 => {
                                if op < OPCODE_SKIP {
                                    let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                    let nclrs = usize::from(PATTERN_COLOURS[op]);
                                    br.read_buf(&mut cur_clr[..nclrs])?;
                                    let mask = br.read_u16le()?;
                                    paint_4x4_old(&mut self.frame[off + self.width * 4..], self.width, &PATTERN_4X4_OLD[pattern], &cur_clr, mask);
                                }
                                let op = usize::from(br.read_byte()?);
                                validate!(op < 0x80 && OPCODE_TYPE[op] == 0);
                                if op < OPCODE_SKIP {
                                    let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                    let nclrs = usize::from(PATTERN_COLOURS[op]);
                                    br.read_buf(&mut cur_clr[..nclrs])?;
                                    let mask = br.read_u16le()?;
                                    paint_4x4_old(&mut self.frame[off + self.width * 4 + 4..], self.width, &PATTERN_4X4_OLD[pattern], &cur_clr, mask);
                                }
                            },
                            _ => return Err(DecoderError::InvalidData),
                        };
                    },
                    _ => {
                        if op < OPCODE_SKIP {
                            let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                            let nclrs = usize::from(PATTERN_COLOURS[op]);
                            br.read_buf(&mut cur_clr[..nclrs])?;
                            let mask = br.read_u16le()?;
                            paint_4x4_old(&mut self.frame[off..], self.width, &PATTERN_4X4_OLD[pattern], &cur_clr, mask);
                        }
                        let op = usize::from(br.read_byte()?);
                        validate!(op < 0x80 && OPCODE_TYPE[op] == 0);
                        if op < OPCODE_SKIP {
                            let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                            let nclrs = usize::from(PATTERN_COLOURS[op]);
                            br.read_buf(&mut cur_clr[..nclrs])?;
                            let mask = br.read_u16le()?;
                            paint_4x4_old(&mut self.frame[off + 4..], self.width, &PATTERN_4X4_OLD[pattern], &cur_clr, mask);
                        }
                        let op = usize::from(br.read_byte()?);
                        validate!(op < 0x80);
                        match OPCODE_TYPE[op] {
                            2 => {
                                if op < OPCODE_SKIP {
                                    let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                    let nclrs = usize::from(PATTERN_COLOURS[op]);
                                    br.read_buf(&mut cur_clr[..nclrs])?;
                                    let mask = br.read_u32le()?;
                                    paint_8x4_old(&mut self.frame[off + self.width * 4..], self.width, &PATTERN_8X4_OLD[pattern], &cur_clr, mask);
                                }
                            },
                            0 => {
                                if op < OPCODE_SKIP {
                                    let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                    let nclrs = usize::from(PATTERN_COLOURS[op]);
                                    br.read_buf(&mut cur_clr[..nclrs])?;
                                    let mask = br.read_u16le()?;
                                    paint_4x4_old(&mut self.frame[off + self.width * 4..], self.width, &PATTERN_4X4_OLD[pattern], &cur_clr, mask);
                                }
                                let op = usize::from(br.read_byte()?);
                                validate!(op < 0x80 && OPCODE_TYPE[op] == 0);
                                if op < OPCODE_SKIP {
                                    let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                    let nclrs = usize::from(PATTERN_COLOURS[op]);
                                    br.read_buf(&mut cur_clr[..nclrs])?;
                                    let mask = br.read_u16le()?;
                                    paint_4x4_old(&mut self.frame[off + self.width * 4 + 4..], self.width, &PATTERN_4X4_OLD[pattern], &cur_clr, mask);
                                }
                            },
                            _ => return Err(DecoderError::InvalidData),
                        };
                    },
                };
            }
            off += self.width * 8;
        }
        Ok(())
    }
    fn decode_85(&mut self, clr_src: &[u8], op_src: &[u8]) -> DecoderResult<()> {
        let mut mr = MemoryReader::new_read(clr_src);
        let mut clrs = ByteReader::new(&mut mr);
        let mut mr = MemoryReader::new_read(op_src);
        let mut ops = ByteReader::new(&mut mr);

        let mut pat = [0; 8];
        let mut cur_clr = [0; 16];

        let mut off = 0;
        let mut rle_len = 0;
        let mut rle_mode = 0;
        let mut src_off = 0;
        for _y in (0..self.height).step_by(8) {
            for x in (0..self.width).step_by(8) {
                let off = off + x;
                if rle_len > 0 {
                    copy_block(&mut self.frame, self.width, src_off, off, rle_mode);
                    rle_len -= 1;
                    continue;
                }
                let op = usize::from(ops.read_byte()?);
                if op < 0x80 {
                    src_off = off;
                    match OPCODE_TYPE[op] {
                        4 => {
                            if op < OPCODE_SKIP {
                                let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                let psrc = if pattern == 0 {
                                        ops.read_buf(&mut pat)?;
                                        &pat
                                    } else {
                                        &PATTERN_8X8[pattern]
                                    };
                                let nclrs = usize::from(PATTERN_COLOURS[op]);
                                assert_eq!(nclrs, 2);
                                clrs.read_buf(&mut cur_clr[..nclrs])?;
                                if pattern == 0 {
                                    cur_clr.swap(0, 1);
                                }
                                paint_8x8(&mut self.frame[off..], self.width, psrc, &cur_clr);
                            }
                        },
                        2 => {
                            if op < OPCODE_SKIP {
                                let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                let nclrs = usize::from(PATTERN_COLOURS[op]);
                                clrs.read_buf(&mut cur_clr[..nclrs])?;
                                let mask = ops.read_u32le()?;
                                paint_8x4(&mut self.frame[off..], self.width, &PATTERN_8X4[pattern], &cur_clr, mask);
                            }
                            let op = usize::from(ops.read_byte()?);
                            validate!(op < 0x80);
                            match OPCODE_TYPE[op] {
                                2 => {
                                    if op < OPCODE_SKIP {
                                        let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                        let nclrs = usize::from(PATTERN_COLOURS[op]);
                                        clrs.read_buf(&mut cur_clr[..nclrs])?;
                                        let mask = ops.read_u32le()?;
                                        paint_8x4(&mut self.frame[off + self.width * 4..], self.width, &PATTERN_8X4[pattern], &cur_clr, mask);
                                    }
                                },
                                0 => {
                                    if op < OPCODE_SKIP {
                                        let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                        let nclrs = usize::from(PATTERN_COLOURS[op]);
                                        clrs.read_buf(&mut cur_clr[..nclrs])?;
                                        let mask = ops.read_u16le()?;
                                        paint_4x4(&mut self.frame[off + self.width * 4..], self.width, &PATTERN_4X4[pattern], &cur_clr, mask);
                                    }
                                    let op = usize::from(ops.read_byte()?);
                                    validate!(op < 0x80 && OPCODE_TYPE[op] == 0);
                                    if op < OPCODE_SKIP {
                                        let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                        let nclrs = usize::from(PATTERN_COLOURS[op]);
                                        clrs.read_buf(&mut cur_clr[..nclrs])?;
                                        let mask = ops.read_u16le()?;
                                        paint_4x4(&mut self.frame[off + self.width * 4 + 4..], self.width, &PATTERN_4X4[pattern], &cur_clr, mask);
                                    }
                                },
                                _ => return Err(DecoderError::InvalidData),
                            };
                        },
                        _ => {
                            if op < OPCODE_SKIP {
                                let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                let nclrs = usize::from(PATTERN_COLOURS[op]);
                                clrs.read_buf(&mut cur_clr[..nclrs])?;
                                let mask = ops.read_u16le()?;
                                paint_4x4(&mut self.frame[off..], self.width, &PATTERN_4X4[pattern], &cur_clr, mask);
                            }
                            let op = usize::from(ops.read_byte()?);
                            validate!(op < 0x80 && OPCODE_TYPE[op] == 0);
                            if op < OPCODE_SKIP {
                                let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                let nclrs = usize::from(PATTERN_COLOURS[op]);
                                clrs.read_buf(&mut cur_clr[..nclrs])?;
                                let mask = ops.read_u16le()?;
                                paint_4x4(&mut self.frame[off + 4..], self.width, &PATTERN_4X4[pattern], &cur_clr, mask);
                            }
                            let op = usize::from(ops.read_byte()?);
                            validate!(op < 0x80);
                            match OPCODE_TYPE[op] {
                                2 => {
                                    if op < OPCODE_SKIP {
                                        let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                        let nclrs = usize::from(PATTERN_COLOURS[op]);
                                        clrs.read_buf(&mut cur_clr[..nclrs])?;
                                        let mask = ops.read_u32le()?;
                                        paint_8x4(&mut self.frame[off + self.width * 4..], self.width, &PATTERN_8X4[pattern], &cur_clr, mask);
                                    }
                                },
                                0 => {
                                    if op < OPCODE_SKIP {
                                        let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                        let nclrs = usize::from(PATTERN_COLOURS[op]);
                                        clrs.read_buf(&mut cur_clr[..nclrs])?;
                                        let mask = ops.read_u16le()?;
                                        paint_4x4(&mut self.frame[off + self.width * 4..], self.width, &PATTERN_4X4[pattern], &cur_clr, mask);
                                    }
                                    let op = usize::from(ops.read_byte()?);
                                    validate!(op < 0x80 && OPCODE_TYPE[op] == 0);
                                    if op < OPCODE_SKIP {
                                        let pattern = usize::from(OPCODE_TO_PATTERN[op]);
                                        let nclrs = usize::from(PATTERN_COLOURS[op]);
                                        clrs.read_buf(&mut cur_clr[..nclrs])?;
                                        let mask = ops.read_u16le()?;
                                        paint_4x4(&mut self.frame[off + self.width * 4 + 4..], self.width, &PATTERN_4X4[pattern], &cur_clr, mask);
                                    }
                                },
                                _ => return Err(DecoderError::InvalidData),
                            };
                        },
                    };
                } else {
                    rle_mode = ((op >> 3) & 0xF) as u8;
                    rle_len = op & 7;
                    if rle_len == 7 {
                        rle_len = usize::from(ops.read_byte()?);
                    }
if rle_mode !=0 && rle_mode !=15 {
println!(" RLE len {} mode {}", rle_len, rle_mode);
}
                    copy_block(&mut self.frame, self.width, src_off, off, rle_mode);
                }
            }
            off += 8 * self.width;
        }
        Ok(())
    }
}

fn copy_block(frame: &mut [u8], stride: usize, mut src_off: usize, mut dst_off: usize, mode: u8) {
    match mode {
        0 => {
            for _ in 0..8 {
                for x in 0..8 {
                    frame[dst_off + x] = frame[src_off + x];
                }
                src_off += stride;
                dst_off += stride;
            }
        },
// 1 -> copy 4x4 from src+4 to dst+1, copy 8x4 from src+1 to dst+1
// 2 -> copy 4x4 from src to dst,     copy 8x4 from src+1 to dst+1
// 3 -> copy 8x4 from src+1 to dst+1
// 4 -> copy 8x4 from src to dst,     copy 4x4 from src+1 to dst+1
// 5 -> copy 4x4 from src+4 to dst+1, copy 4x4 from src+1 to dst+1
// 6 -> ??? copy 8x4 from src to dst, copy 4x4 from src+1 to dst+1
// 7 -> copy 4x4 from src+1 to dst+1
// 8 -> copy 8x4 from src to dst,     copy 4x4 from src+2 to dst+2
// 9 -> copy 4x4 from src+4 to dst+1, copy 4x4 from src+2 to dst+2
//10 -> copy 4x8 from src+2 to dst+2
//11 -> copy 4x4 from src+2 to dst+2
//12 -> copy 8x4 block
//13 -> copy 4x4 block
        14 | 15 => {}, // skip
        _ => unimplemented!(),
    };
}

fn paint_8x8(dst: &mut [u8], stride: usize, pat: &[u8; 8], clrs: &[u8; 16]) {
    for (line, &lpat) in dst.chunks_mut(stride).zip(pat.iter()) {
        let mut mask = usize::from(lpat);
        for dst in line[..8].iter_mut() {
            *dst = clrs[mask & 1];
            mask >>= 1;
        }
    }
}

fn paint_8x4_old(dst: &mut [u8], stride: usize, pat: &[u8; 8], clrs: &[u8; 16], mut mask: u32) {
    for (y, line) in dst.chunks_mut(stride).take(4).enumerate() {
        for (x, dst) in line[..8].iter_mut().enumerate() {
            *dst = clrs[usize::from(pat[x / 2 + (y / 2) * 4] + ((mask & 1) as u8))];
            mask >>= 1;
        }
    }
}

fn paint_8x4(dst: &mut [u8], stride: usize, pat: &[u8; 8], clrs: &[u8; 16], mut mask: u32) {
    for (y, line) in dst.chunks_mut(stride).take(4).enumerate() {
        for (x, dst) in line[..8].iter_mut().enumerate() {
            *dst = clrs[usize::from(pat[x / 2 + (y / 2) * 4] - ((mask & 1) as u8))];
            mask >>= 1;
        }
    }
}

fn paint_4x4_old(dst: &mut [u8], stride: usize, pat: &[u8; 8], clrs: &[u8; 16], mut mask: u16) {
    for (line, pat) in dst.chunks_mut(stride).zip(pat.chunks_exact(2)) {
        for (x, dst) in line[..4].iter_mut().enumerate() {
            *dst = clrs[usize::from(pat[x / 2] + ((mask & 1) as u8))];
            mask >>= 1;
        }
    }
}

fn paint_4x4(dst: &mut [u8], stride: usize, pat: &[u8; 8], clrs: &[u8; 16], mut mask: u16) {
    for (line, pat) in dst.chunks_mut(stride).zip(pat.chunks_exact(2)) {
        for (x, dst) in line[..4].iter_mut().enumerate() {
            *dst = clrs[usize::from(pat[x / 2] - ((mask & 1) as u8))];
            mask >>= 1;
        }
    }
}

impl NADecoder for DPVideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let width  = vinfo.get_width();
            let height = vinfo.get_height();
            validate!((width & 7) == 0 && (height & 7) == 0);
            if let Some(ref edata) = info.get_extradata() {
                validate!(!edata.is_empty());
                self.subtype = edata[0];
                validate!(self.subtype >= 0x80);
                if !matches!(self.subtype, 0x81 | 0x85 | 0x86 | 0x89 | 0x8A) {
                    return Err(DecoderError::NotImplemented);
                }
                match self.subtype {
                    0x81 | 0x8A => {
                    },
                    0x85 | 0x86 => {
                        validate!(edata.len() > 256 * 2);
                        for (dst, src) in self.pal.iter_mut().zip(edata[1..].chunks_exact(2)) {
                            *dst = read_u16be(src)?;
                        }
                    },
                    0x89 => {
                    },
                    _ => return Err(DecoderError::NotImplemented),
                };
                let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, RGB555_FORMAT));
                self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
                self.width  = vinfo.get_width();
                self.height = vinfo.get_height();
                self.frame.resize(self.width * self.height, 0);

                Ok(())
            } else {
                Err(DecoderError::InvalidData)
            }
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        match self.subtype {
            0x81 | 0x8A => {
                validate!(src.len() > 12);
                let mut mr = MemoryReader::new_read(&src);
                let mut br = ByteReader::new(&mut mr);
                let stype           = br.read_byte()?;
                validate!(stype == 0x81 || stype == 0x8A);
                                      br.read_byte()?;
                let size            = usize::from(br.read_u16le()?);
                if size + 4 < src.len() {
                    return Err(DecoderError::ShortData);
                }
                                      br.read_u32be()?; // timestamp
                                      br.read_byte()?;
                let pal_size        = usize::from(br.read_byte()?);
                let width           = usize::from(br.read_byte()?) * 8;
                let height          = usize::from(br.read_byte()?) * 8;
                validate!(width == self.width);
                validate!(height == self.height);

                for dst in self.pal.iter_mut().take(pal_size) {
                    *dst            = br.read_u16le()?;
                }

                self.decode_81(&mut br)?;
            },
            0x85 | 0x86 | 0x89 => {
                validate!(src.len() > 6);
                let mut mr = MemoryReader::new_read(&src);
                let mut br = ByteReader::new(&mut mr);

                let pal_offset      = usize::from(br.read_u16be()?);
                let pal_size        = usize::from(br.read_u16be()?);
                let offset          = usize::from(br.read_u16be()?);
                validate!(offset <= src.len());
                validate!(pal_offset + pal_size <= 256);
                validate!(pal_size * 2 <= src.len());

                for (dst, src) in self.pal[pal_offset..].iter_mut().zip(src[6..][..pal_size * 2].chunks_exact(2)) {
                    *dst = read_u16be(src)?;
                }

                self.decode_85(&src[6 + pal_size * 2..], &src[6 + offset..])?;
            },
            _ => unreachable!(),
        };

        let mut bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
        if let NABufferType::Video16(ref mut vbuf16) = bufinfo {
            let stride = vbuf16.get_stride(0);
            let dst = vbuf16.get_data_mut().unwrap();
            for (dline, sline) in dst.chunks_exact_mut(stride).zip(self.frame.chunks_exact(self.width)) {
                for (dst, &src) in dline.iter_mut().zip(sline.iter()) {
                    *dst = self.pal[usize::from(src)];
                }
            }
        } else {
            return Err(DecoderError::Bug);
        }

        let frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for DPVideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(DPVideoDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    #[test]
    fn test_sga_dec_81() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from Double Switch
        test_decoding("sga", "dp-sga", "assets/Game/sga/ALEXSTIL.AVC", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x8ae53caf, 0x9bd04a58, 0xf08f3ea9, 0x72b6fd05]));
    }
    #[test]
    fn test_sga_dec_85() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from Night Trap
        test_decoding("sga", "dp-sga", "assets/Game/sga/CRMOVIE", Some(32), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0xa408375d, 0x3fc131f5, 0xd9ce5172, 0x30043774],
                            [0x2c18a4b2, 0x5771e98b, 0x90373b23, 0xc38b820d],
                            [0x2112a384, 0x8af91c70, 0xa8ba0a10, 0x166b754e],
                            [0x8824d580, 0xaba31634, 0x005f0c5e, 0xe45ac6c5],
                            [0x3f5551ba, 0xff75d014, 0xc7d22554, 0x2567f49f],
                            [0xf6257fd0, 0x457f6ff2, 0x0f5975bb, 0x85457c46],
                            [0xb7302db3, 0x5d384875, 0x8bce4edf, 0x9b25b176]]));
    }
    #[test]
    fn test_sga_dec_86() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

//test_decode_images("sga", "assets/Game/sga/dplogo.dtv", "sga86", None, &dmx_reg, &dec_reg);
//panic!("end");

        // sample from Corpse Killer
        test_decoding("sga", "dp-sga", "assets/Game/sga/dplogo.dtv", Some(10), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x6dec0bbc, 0x9e81995b, 0x660a899b, 0xbc7954ca],
                            [0xce33d3e5, 0xcba3398d, 0x63a6ca73, 0xc3967861],
                            [0x55d3506a, 0x4b43c8e9, 0xac878ff2, 0xfd538d50],
                            [0x5c2abd0a, 0x0fc59df8, 0xfce6f84d, 0x577a6f0a],
                            [0x922e5500, 0xe16d523d, 0x44122da3, 0xf8d74675]]));
    }
}

const OPCODE_TYPE: [u8; 128] = [
    0x00, 0x04, 0x00, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x02, 0x00, 0x00, 0x00, 0x04, 0x04, 0x04, 0x04,
    0x02, 0x02, 0x00, 0x00, 0x00, 0x02, 0x00, 0x02,
    0x00, 0x00, 0x02, 0x02, 0x00, 0x02, 0x02, 0x02,
    0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
    0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x04, 0x04, 0x04, 0x04, 0x02, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
];
const OPCODE_TO_PATTERN: [u8; 96] = [
    0x00, 0x00, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05,
    0x00, 0x00, 0x01, 0x02, 0x06, 0x07, 0x08, 0x09,
    0x01, 0x02, 0x03, 0x04, 0x05, 0x03, 0x06, 0x04,
    0x07, 0x08, 0x05, 0x06, 0x09, 0x07, 0x08, 0x09,
    0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11,
    0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
    0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11,
    0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
    0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
    0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11,
    0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
    0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00
];
const PATTERN_COLOURS: [u8; 96] = [
    0x00, 0x02, 0x00, 0x02, 0x02, 0x02, 0x02, 0x02,
    0x02, 0x08, 0x08, 0x0a, 0x02, 0x02, 0x02, 0x02,
    0x04, 0x06, 0x06, 0x08, 0x08, 0x0a, 0x0a, 0x08,
    0x06, 0x04, 0x0a, 0x08, 0x02, 0x08, 0x08, 0x06,
    0x10, 0x0e, 0x0e, 0x0c, 0x0e, 0x0c, 0x0c, 0x0a,
    0x0e, 0x0c, 0x0c, 0x0a, 0x0c, 0x0a, 0x0a, 0x08,
    0x10, 0x0e, 0x0e, 0x0c, 0x0e, 0x0c, 0x0c, 0x0a,
    0x0e, 0x0c, 0x0c, 0x0a, 0x0c, 0x0a, 0x0a, 0x08,
    0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
    0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
    0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
    0x02, 0x02, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00
];
const PATTERN_4X4: [[u8; 8]; 26] = [
    [ 0x01, 0x03, 0x05, 0x03, 0x07, 0x07, 0x07, 0x07 ],
    [ 0x01, 0x03, 0x01, 0x05, 0x07, 0x07, 0x07, 0x07 ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x09, 0x09, 0x09 ],
    [ 0x01, 0x03, 0x01, 0x03, 0x05, 0x05, 0x05, 0x05 ],
    [ 0x01, 0x01, 0x01, 0x01, 0x03, 0x05, 0x07, 0x05 ],
    [ 0x01, 0x01, 0x01, 0x01, 0x03, 0x05, 0x03, 0x07 ],
    [ 0x01, 0x01, 0x01, 0x01, 0x03, 0x05, 0x07, 0x09 ],
    [ 0x01, 0x01, 0x01, 0x01, 0x03, 0x05, 0x03, 0x05 ],
    [ 0x01, 0x01, 0x01, 0x01, 0x03, 0x03, 0x03, 0x03 ],
    [ 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01 ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b, 0x0d, 0x0f ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b, 0x0d, 0x0b ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b, 0x09, 0x0d ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b, 0x09, 0x0b ],
    [ 0x01, 0x03, 0x05, 0x03, 0x07, 0x09, 0x0b, 0x0d ],
    [ 0x01, 0x03, 0x05, 0x03, 0x07, 0x09, 0x0b, 0x09 ],
    [ 0x01, 0x03, 0x05, 0x03, 0x07, 0x09, 0x07, 0x0b ],
    [ 0x01, 0x03, 0x05, 0x03, 0x07, 0x09, 0x07, 0x09 ],
    [ 0x01, 0x03, 0x01, 0x05, 0x07, 0x09, 0x0b, 0x0d ],
    [ 0x01, 0x03, 0x01, 0x05, 0x07, 0x09, 0x0b, 0x09 ],
    [ 0x01, 0x03, 0x01, 0x05, 0x07, 0x09, 0x07, 0x0b ],
    [ 0x01, 0x03, 0x01, 0x05, 0x07, 0x09, 0x07, 0x09 ],
    [ 0x01, 0x03, 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b ],
    [ 0x01, 0x03, 0x01, 0x03, 0x05, 0x07, 0x09, 0x07 ],
    [ 0x01, 0x03, 0x01, 0x03, 0x05, 0x07, 0x05, 0x09 ],
    [ 0x01, 0x03, 0x01, 0x03, 0x05, 0x07, 0x05, 0x07 ]
];
const PATTERN_8X4: [[u8; 8]; 26] = [
    [ 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01 ],
    [ 0x01, 0x01, 0x03, 0x03, 0x01, 0x01, 0x03, 0x03 ],
    [ 0x01, 0x01, 0x03, 0x03, 0x01, 0x01, 0x05, 0x05 ],
    [ 0x01, 0x03, 0x05, 0x05, 0x07, 0x09, 0x05, 0x05 ],
    [ 0x01, 0x03, 0x05, 0x05, 0x07, 0x07, 0x05, 0x05 ],
    [ 0x01, 0x01, 0x03, 0x05, 0x01, 0x01, 0x07, 0x09 ],
    [ 0x01, 0x01, 0x03, 0x05, 0x01, 0x01, 0x07, 0x07 ],
    [ 0x01, 0x01, 0x03, 0x03, 0x05, 0x07, 0x03, 0x03 ],
    [ 0x01, 0x01, 0x03, 0x03, 0x01, 0x01, 0x05, 0x07 ],
    [ 0x01, 0x01, 0x03, 0x03, 0x05, 0x05, 0x03, 0x03 ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b, 0x0d, 0x0f ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b, 0x0d, 0x0d ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x09, 0x0b, 0x0d ],
    [ 0x01, 0x03, 0x05, 0x07, 0x09, 0x09, 0x0b, 0x0b ],
    [ 0x01, 0x03, 0x05, 0x05, 0x07, 0x09, 0x0b, 0x0d ],
    [ 0x01, 0x03, 0x05, 0x05, 0x07, 0x09, 0x0b, 0x0b ],
    [ 0x01, 0x03, 0x05, 0x05, 0x07, 0x07, 0x09, 0x0b ],
    [ 0x01, 0x03, 0x05, 0x05, 0x07, 0x07, 0x09, 0x09 ],
    [ 0x01, 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b, 0x0d ],
    [ 0x01, 0x01, 0x03, 0x05, 0x07, 0x09, 0x0b, 0x0b ],
    [ 0x01, 0x01, 0x03, 0x05, 0x07, 0x07, 0x09, 0x0b ],
    [ 0x01, 0x01, 0x03, 0x05, 0x07, 0x07, 0x09, 0x09 ],
    [ 0x01, 0x01, 0x03, 0x03, 0x05, 0x07, 0x09, 0x0b ],
    [ 0x01, 0x01, 0x03, 0x03, 0x05, 0x07, 0x09, 0x09 ],
    [ 0x01, 0x01, 0x03, 0x03, 0x05, 0x05, 0x07, 0x09 ],
    [ 0x01, 0x01, 0x03, 0x03, 0x05, 0x05, 0x07, 0x07 ]
];
const PATTERN_8X8: [[u8; 8]; 26] = [
    [ 0; 8 ],
    [ 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa ],
    [ 0xff, 0xff, 0x55, 0xaa, 0x55, 0xaa, 0x00, 0x00 ],
    [ 0x5f, 0xaf, 0x57, 0xab, 0x15, 0x2a, 0x05, 0x0a ],
    [ 0x17, 0x2b, 0x17, 0x2b, 0x17, 0x2b, 0x17, 0x2b ],
    [ 0x05, 0x2a, 0x55, 0x2a, 0x55, 0xab, 0x57, 0xaf ],
    [ 0x00, 0x00, 0x55, 0xaa, 0x55, 0xaa, 0xff, 0xff ],
    [ 0x50, 0xa0, 0x54, 0xa8, 0xd5, 0xea, 0xf5, 0xfa ],
    [ 0xd4, 0xe8, 0xd4, 0xe8, 0xd4, 0xe8, 0xd4, 0xe8 ],
    [ 0xf5, 0xea, 0xd5, 0xaa, 0x54, 0xaa, 0x54, 0xa0 ],
    [ 0x55, 0xaa, 0x55, 0xaa, 0x00, 0x00, 0x00, 0x00 ],
    [ 0x55, 0x2a, 0x15, 0x0a, 0x05, 0x02, 0x01, 0x00 ],
    [ 0x05, 0x0a, 0x05, 0x0a, 0x05, 0x0a, 0x05, 0x0a ],
    [ 0x01, 0x00, 0x05, 0x02, 0x15, 0x0a, 0x55, 0x2a ],
    [ 0x00, 0x00, 0x00, 0x00, 0x55, 0xaa, 0x55, 0xaa ],
    [ 0x00, 0x80, 0x40, 0xa0, 0x50, 0xa8, 0x54, 0xaa ],
    [ 0x50, 0xa0, 0x50, 0xa0, 0x50, 0xa0, 0x50, 0xa0 ],
    [ 0x54, 0xaa, 0x50, 0xa8, 0x40, 0xa0, 0x00, 0x80 ],
    [ 0xff, 0xff, 0xff, 0xff, 0x55, 0xaa, 0x55, 0xaa ],
    [ 0x7f, 0xff, 0x5f, 0xbf, 0x57, 0xaf, 0x55, 0xab ],
    [ 0x5f, 0xaf, 0x5f, 0xaf, 0x5f, 0xaf, 0x5f, 0xaf ],
    [ 0x55, 0xab, 0x57, 0xaf, 0x5f, 0xbf, 0x7f, 0xff ],
    [ 0x55, 0xaa, 0x55, 0xaa, 0xff, 0xff, 0xff, 0xff ],
    [ 0xd5, 0xaa, 0xf5, 0xea, 0xfd, 0xfa, 0xff, 0xfe ],
    [ 0xf5, 0xfa, 0xf5, 0xfa, 0xf5, 0xfa, 0xf5, 0xfa ],
    [ 0xff, 0xfe, 0xfd, 0xfa, 0xf5, 0xea, 0xd5, 0xaa ]
];

const PATTERN_4X4_OLD: [[u8; 8]; 26] = [
    [ 0x00, 0x02, 0x04, 0x02, 0x06, 0x06, 0x06, 0x06 ],
    [ 0x00, 0x02, 0x00, 0x04, 0x06, 0x06, 0x06, 0x06 ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x08, 0x08, 0x08 ],
    [ 0x00, 0x02, 0x00, 0x02, 0x04, 0x04, 0x04, 0x04 ],
    [ 0x00, 0x00, 0x00, 0x00, 0x02, 0x04, 0x06, 0x04 ],
    [ 0x00, 0x00, 0x00, 0x00, 0x02, 0x04, 0x02, 0x06 ],
    [ 0x00, 0x00, 0x00, 0x00, 0x02, 0x04, 0x06, 0x08 ],
    [ 0x00, 0x00, 0x00, 0x00, 0x02, 0x04, 0x02, 0x04 ],
    [ 0x00, 0x00, 0x00, 0x00, 0x02, 0x02, 0x02, 0x02 ],
    [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c, 0x0e ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c, 0x0a ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x08, 0x0c ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x08, 0x0a ],
    [ 0x00, 0x02, 0x04, 0x02, 0x06, 0x08, 0x0a, 0x0c ],
    [ 0x00, 0x02, 0x04, 0x02, 0x06, 0x08, 0x0a, 0x08 ],
    [ 0x00, 0x02, 0x04, 0x02, 0x06, 0x08, 0x06, 0x0a ],
    [ 0x00, 0x02, 0x04, 0x02, 0x06, 0x08, 0x06, 0x08 ],
    [ 0x00, 0x02, 0x00, 0x04, 0x06, 0x08, 0x0a, 0x0c ],
    [ 0x00, 0x02, 0x00, 0x04, 0x06, 0x08, 0x0a, 0x08 ],
    [ 0x00, 0x02, 0x00, 0x04, 0x06, 0x08, 0x06, 0x0a ],
    [ 0x00, 0x02, 0x00, 0x04, 0x06, 0x08, 0x06, 0x08 ],
    [ 0x00, 0x02, 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a ],
    [ 0x00, 0x02, 0x00, 0x02, 0x04, 0x06, 0x08, 0x06 ],
    [ 0x00, 0x02, 0x00, 0x02, 0x04, 0x06, 0x04, 0x08 ],
    [ 0x00, 0x02, 0x00, 0x02, 0x04, 0x06, 0x04, 0x06 ]
];
const PATTERN_8X4_OLD: [[u8; 8]; 26] = [
    [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0x00, 0x00, 0x02, 0x02, 0x00, 0x00, 0x02, 0x02 ],
    [ 0x00, 0x00, 0x02, 0x02, 0x00, 0x00, 0x04, 0x04 ],
    [ 0x00, 0x02, 0x04, 0x04, 0x06, 0x08, 0x04, 0x04 ],
    [ 0x00, 0x02, 0x04, 0x04, 0x06, 0x06, 0x04, 0x04 ],
    [ 0x00, 0x00, 0x02, 0x04, 0x00, 0x00, 0x06, 0x08 ],
    [ 0x00, 0x00, 0x02, 0x04, 0x00, 0x00, 0x06, 0x06 ],
    [ 0x00, 0x00, 0x02, 0x02, 0x04, 0x06, 0x02, 0x02 ],
    [ 0x00, 0x00, 0x02, 0x02, 0x00, 0x00, 0x04, 0x06 ],
    [ 0x00, 0x00, 0x02, 0x02, 0x04, 0x04, 0x02, 0x02 ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c, 0x0e ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c, 0x0c ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x08, 0x0a, 0x0c ],
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x08, 0x0a, 0x0a ],
    [ 0x00, 0x02, 0x04, 0x04, 0x06, 0x08, 0x0a, 0x0c ],
    [ 0x00, 0x02, 0x04, 0x04, 0x06, 0x08, 0x0a, 0x0a ],
    [ 0x00, 0x02, 0x04, 0x04, 0x06, 0x06, 0x08, 0x0a ],
    [ 0x00, 0x02, 0x04, 0x04, 0x06, 0x06, 0x08, 0x08 ],
    [ 0x00, 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c ],
    [ 0x00, 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0a ],
    [ 0x00, 0x00, 0x02, 0x04, 0x06, 0x06, 0x08, 0x0a ],
    [ 0x00, 0x00, 0x02, 0x04, 0x06, 0x06, 0x08, 0x08 ],
    [ 0x00, 0x00, 0x02, 0x02, 0x04, 0x06, 0x08, 0x0a ],
    [ 0x00, 0x00, 0x02, 0x02, 0x04, 0x06, 0x08, 0x08 ],
    [ 0x00, 0x00, 0x02, 0x02, 0x04, 0x04, 0x06, 0x08 ],
    [ 0x00, 0x00, 0x02, 0x02, 0x04, 0x04, 0x06, 0x06 ]
];
