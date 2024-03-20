use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
//use std::str::FromStr;

struct FrameData {
    info:       NACodecInfoRef,
    width:      usize,
    height:     usize,
    frm0:       Vec<u16>,
    frm1:       Vec<u16>,
    frm2:       Vec<u16>,
}

impl FrameData {
    fn new() -> Self {
        Self {
            info:       NACodecInfoRef::default(),
            width:      0,
            height:     0,
            frm0:       Vec::new(),
            frm1:       Vec::new(),
            frm2:       Vec::new(),
        }
    }
    fn init(&mut self, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, false, RGB565_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            self.frm0.resize(self.width * self.height, 0);
            self.frm1.resize(self.width * self.height, 0);
            self.frm2.resize(self.width * self.height, 0);
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn get_frame(&mut self, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
        if let Some(ref mut vbuf) = bufinfo.get_vbuf16() {
            let stride = vbuf.get_stride(0);
            let data = vbuf.get_data_mut().unwrap();
            for (dst, src) in data.chunks_mut(stride).zip(self.frm0.chunks(self.width).take(self.height)) {
                dst[..self.width].copy_from_slice(src);
            }
        } else {
            return Err(DecoderError::Bug);
        }

        let is_intra = pkt.keyframe;
        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
}

fn decode_rle(br: &mut ByteReader, dst: &mut [u8]) -> DecoderResult<()> {
    let mut len = 0;
    let mut clr = 0;
    let mut run = false;

    for el in dst.iter_mut() {
        if len == 0 {
            let op                      = br.read_byte()?;
            run = (op & 1) != 0;
            if run {
                clr                     = br.read_byte()?;
            }
            len = ((op >> 1) + 1) as usize;
        }
        *el = if run { clr } else { br.read_byte()? };
        len -= 1;
    }
    validate!(len == 0);

    Ok(())
}

struct Smush2Decoder {
    glyphs4:    [[u8; 16]; 256],
    glyphs8:    [[u8; 64]; 256],
    pic:        FrameData,
    rle_buf:    Vec<u8>,
}

impl Smush2Decoder {
    fn new() -> Self {
        let mut glyphs4 = [[0; 16]; 256];
        let mut glyphs8 = [[0; 64]; 256];
        super::make_glyphs_47(&mut glyphs4, &mut glyphs8);
        Self {
            pic:        FrameData::new(),
            rle_buf:    Vec::new(),
            glyphs4, glyphs8,
        }
    }
}

struct BlockData<'a> {
    glyphs4:    &'a [[u8; 16]; 256],
    glyphs8:    &'a [[u8; 64]; 256],
    frm1:       &'a [u16],
    frm2:       &'a [u16],
    cb:         &'a [u16; 256],
    clr4:       [u16; 4],
    stride:     usize,
}

fn draw_glyph(dst: &mut [u16], stride: usize, bsize: usize, glyph: &[u8], clr2: [u16; 2]) {
    for (dst, src) in dst.chunks_mut(stride).zip(glyph.chunks_exact(bsize)) {
        for (el, &bit) in dst[..bsize].iter_mut().zip(src.iter()) {
            *el = clr2[bit as usize];
        }
    }
}

fn do_block2(br: &mut ByteReader, dst: &mut [u16], x: usize, y: usize, bsize: usize, bdata: &BlockData) -> DecoderResult<()> {
    let stride = bdata.stride;
    let op                              = br.read_byte()?;
    match op {
        0xFF if bsize > 2 => {
            let hsize = bsize / 2;
            do_block2(br, dst, x, y, hsize, bdata)?;
            do_block2(br, &mut dst[hsize..], x + hsize, y, bsize / 2, bdata)?;
            do_block2(br, &mut dst[hsize * stride..], x, y + hsize, hsize, bdata)?;
            do_block2(br, &mut dst[hsize * (stride + 1)..], x + hsize, y + hsize, bsize / 2, bdata)?;
        },
        0xFF => {
            dst[0]                      = br.read_u16le()?;
            dst[1]                      = br.read_u16le()?;
            dst[stride]                 = br.read_u16le()?;
            dst[stride + 1]             = br.read_u16le()?;
        },
        0xFE => {
            let pix                     = br.read_u16le()?;
            for dst in dst.chunks_mut(stride).take(bsize) {
                for el in dst[..bsize].iter_mut() {
                    *el = pix;
                }
            }
        },
        0xFD => {
            let idx                     = br.read_byte()? as usize;
            let pix = bdata.cb[idx];
            for dst in dst.chunks_mut(stride).take(bsize) {
                for el in dst[..bsize].iter_mut() {
                    *el = pix;
                }
            }
        },
        0xF9..=0xFC => {
            let pix = bdata.clr4[(op - 0xF9) as usize];
            for dst in dst.chunks_mut(stride).take(bsize) {
                for el in dst[..bsize].iter_mut() {
                    *el = pix;
                }
            }
        },
        0xF8 if bsize > 2 => {
            let idx                     = br.read_byte()? as usize;
            let mut clr2 = [0; 2];
            clr2[1]                     = br.read_u16le()?;
            clr2[0]                     = br.read_u16le()?;
            let glyph: &[u8] = if bsize == 8 { &bdata.glyphs8[idx] } else { &bdata.glyphs4[idx] };
            draw_glyph(dst, stride, bsize, glyph, clr2);
        },
        0xF8 => {
             dst[0]                     = br.read_u16le()?;
             dst[1]                     = br.read_u16le()?;
             dst[stride]                = br.read_u16le()?;
             dst[stride + 1]            = br.read_u16le()?;
        },
        0xF7 if bsize > 2 => {
            let idx                     = br.read_byte()? as usize;
            let mut clr2 = [0; 2];
            clr2[1]                     = bdata.cb[br.read_byte()? as usize];
            clr2[0]                     = bdata.cb[br.read_byte()? as usize];
            let glyph: &[u8] = if bsize == 8 { &bdata.glyphs8[idx] } else { &bdata.glyphs4[idx] };
            draw_glyph(dst, stride, bsize, glyph, clr2);
        },
        0xF7 => {
             dst[0]                     = bdata.cb[br.read_byte()? as usize];
             dst[1]                     = bdata.cb[br.read_byte()? as usize];
             dst[stride]                = bdata.cb[br.read_byte()? as usize];
             dst[stride + 1]            = bdata.cb[br.read_byte()? as usize];
        },
        0xF6 => {
            let off = x + y * stride;
            let src = &bdata.frm1[off..];
            for (dst, src) in dst.chunks_mut(stride).zip(src.chunks(stride)).take(bsize) {
                dst[..bsize].copy_from_slice(&src[..bsize]);
            }
        },
        0xF5 => {
            let off                     = br.read_u16le()? as i16 as isize;
            let mx = off % (stride as isize);
            let my = off / (stride as isize);
            let off = (x as isize) + mx + ((y as isize) + my) * (stride as isize);
            validate!(off >= 0);
            let src = &bdata.frm2[off as usize..];
            for (dst, src) in dst.chunks_mut(stride).zip(src.chunks(stride)).take(bsize) {
                let size = dst.len().min(src.len()).min(bsize);
                dst[..size].copy_from_slice(&src[..size]);
            }
        },
        _ => {
            let mx = C47_MV[op as usize][0] as isize;
            let my = C47_MV[op as usize][1] as isize;
            let off = (x as isize) + mx + ((y as isize) + my) * (stride as isize);
            let src = &bdata.frm2[off as usize..];
            for (dst, src) in dst.chunks_mut(stride).zip(src.chunks(stride)).take(bsize) {
                let size = dst.len().min(src.len()).min(bsize);
                dst[..size].copy_from_slice(&src[..size]);
            }
        },
    };
    Ok(())
}

impl NADecoder for Smush2Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        self.pic.init(info)
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 8);

        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);

        let mut reorder = 0;
        while br.left() > 0 {
            let tag                     = br.read_tag()?;
            let size                    = br.read_u32be()? as usize;
            let tend = br.tell() + (size as u64);
            validate!((size as i64) <= br.left());
            match &tag {
                b"Bl16" => {
                    validate!(size >= 8);
                                          br.read_skip(2)?;
                    let _x              = br.read_u16le()? as usize;
                    let _y              = br.read_u16le()? as usize;
                                          br.read_skip(2)?;
                    validate!(_x <= self.pic.width && _y <= self.pic.height);
                    if size > 8 {
                        let w           = br.read_u32le()? as usize;
                        let h           = br.read_u32le()? as usize;
                        validate!(w == self.pic.width && h == self.pic.height);
                        let seq         = br.read_u16le()?;
                        let compr       = br.read_byte()?;
                        reorder         = br.read_byte()?;
                                          br.read_skip(4)?;
                        let mut clr4 = [0; 4];
                        for el in clr4.iter_mut() {
                            *el         = br.read_u16le()?;
                        }
                        let bg_clr      = br.read_u16le()?;
                        let _fg_clr     = br.read_u16le()?;
                        let _unp_size   = br.read_u32le()?;
                        let mut cb = [0; 256];
                        for el in cb.iter_mut() {
                            *el         = br.read_u16le()?;
                        }
                        if size > 0x230 {
                                          br.read_skip(8)?;
                        }
                        validate!(br.tell() < tend);
                        let start = br.tell() as usize;
                                          br.seek(SeekFrom::Start(tend))?;
                        let mut mr = MemoryReader::new_read(&src[start..(tend as usize)]);
                        let mut br = ByteReader::new(&mut mr);

                        if seq == 0 {
                            for el in self.pic.frm1.iter_mut() {
                                *el = bg_clr;
                            }
                            for el in self.pic.frm2.iter_mut() {
                                *el = bg_clr;
                            }
                        }

                        match compr {
                            0 => {
                                for row in self.pic.frm0.chunks_mut(self.pic.width).take(h) {
                                    for el in row[..w].iter_mut() {
                                        *el = br.read_u16le()?;
                                    }
                                }
                            },
                            1 => { unimplemented!(); }, //decode half-res and interpolate
                            2 => {
                                let bdata = BlockData {
                                        glyphs4:    &self.glyphs4,
                                        glyphs8:    &self.glyphs8,
                                        frm1:       &self.pic.frm1,
                                        frm2:       &self.pic.frm2,
                                        stride:     self.pic.width,
                                        clr4,
                                        cb:         &cb,
                                    };
                                let dst = &mut self.pic.frm0;
                                let stride = self.pic.width;
                                for (row_no, row) in dst.chunks_mut(stride * 8).take((h + 7) / 8).enumerate() {
                                    for col in (0..w).step_by(8) {
                                        do_block2(&mut br, &mut row[col..], col, row_no * 8, 8, &bdata)?;
                                    }
                                }
                            },
                            3 => {
                                self.pic.frm0.copy_from_slice(&self.pic.frm2);
                            },
                            4 => {
                                self.pic.frm0.copy_from_slice(&self.pic.frm1);
                            },
                            5 => {
                                let size = w * h * 2;
                                self.rle_buf.resize(size, 0);
                                decode_rle(&mut br, &mut self.rle_buf)?;
                                for (drow, srow) in self.pic.frm0.chunks_mut(self.pic.width).zip(self.rle_buf.chunks(w * 2)) {
                                    for (dst, src) in drow.iter_mut().zip(srow.chunks_exact(2)) {
                                        *dst = read_u16le(src)?;
                                    }
                                }
                            },
                            6 => {
                                for row in self.pic.frm0.chunks_mut(self.pic.width).take(h) {
                                    for el in row[..w].iter_mut() {
                                        let idx = br.read_byte()? as usize;
                                        *el = cb[idx];
                                    }
                                }
                            },
                            7 => { unimplemented!(); }, //decode half-res using codebook indices and interpolate
                            8 => {
                                let size = w * h;
                                self.rle_buf.resize(size, 0);
                                decode_rle(&mut br, &mut self.rle_buf)?;
                                for (row, src) in self.pic.frm0.chunks_mut(self.pic.width).zip(self.rle_buf.chunks(w)) {
                                    for (el, &idx) in row.iter_mut().zip(src.iter()) {
                                        *el = cb[idx as usize];
                                    }
                                }
                            },
                            _ => return Err(DecoderError::NotImplemented),
                        };
                    }
                },
                _ =>                      br.read_skip(size)?,
            };
        }

        let ret = self.pic.get_frame(pkt);

        if reorder == 2 {
            std::mem::swap(&mut self.pic.frm1, &mut self.pic.frm2);
        }
        if reorder != 0 {
            std::mem::swap(&mut self.pic.frm0, &mut self.pic.frm2);
        }

        ret
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for Smush2Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder_video_v2() -> Box<dyn NADecoder + Send> {
    Box::new(Smush2Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    // sample from Grim Fandango
    #[test]
    fn test_smush_sanm() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from Grim Fandango
        test_decoding("smush", "smushv2", "assets/Game/smush/lol.snm", Some(4), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x408e4dc9, 0x4483d7d8, 0xc9fae314, 0x3bb45ec9],
                            [0x83548952, 0x0b4a6ccb, 0x42609794, 0x59d3c7d4],
                            [0x5349f6ca, 0x56361199, 0x7194439f, 0x90df21b8],
                            [0x0c359bab, 0xed69f862, 0x9c899813, 0x3f6aac2a],
                            [0x58870617, 0x97c5f3a6, 0x1b2c761c, 0x6ec1cd0e]]));
    }
}

const C47_MV: [[i8; 2]; 255] = [
    [  0,   0], [ -1, -43], [  6, -43], [ -9, -42], [ 13, -41],
    [-16, -40], [ 19, -39], [-23, -36], [ 26, -34], [ -2, -33],
    [  4, -33], [-29, -32], [ -9, -32], [ 11, -31], [-16, -29],
    [ 32, -29], [ 18, -28], [-34, -26], [-22, -25], [ -1, -25],
    [  3, -25], [ -7, -24], [  8, -24], [ 24, -23], [ 36, -23],
    [-12, -22], [ 13, -21], [-38, -20], [  0, -20], [-27, -19],
    [ -4, -19], [  4, -19], [-17, -18], [ -8, -17], [  8, -17],
    [ 18, -17], [ 28, -17], [ 39, -17], [-12, -15], [ 12, -15],
    [-21, -14], [ -1, -14], [  1, -14], [-41, -13], [ -5, -13],
    [  5, -13], [ 21, -13], [-31, -12], [-15, -11], [ -8, -11],
    [  8, -11], [ 15, -11], [ -2, -10], [  1, -10], [ 31, -10],
    [-23,  -9], [-11,  -9], [ -5,  -9], [  4,  -9], [ 11,  -9],
    [ 42,  -9], [  6,  -8], [ 24,  -8], [-18,  -7], [ -7,  -7],
    [ -3,  -7], [ -1,  -7], [  2,  -7], [ 18,  -7], [-43,  -6],
    [-13,  -6], [ -4,  -6], [  4,  -6], [  8,  -6], [-33,  -5],
    [ -9,  -5], [ -2,  -5], [  0,  -5], [  2,  -5], [  5,  -5],
    [ 13,  -5], [-25,  -4], [ -6,  -4], [ -3,  -4], [  3,  -4],
    [  9,  -4], [-19,  -3], [ -7,  -3], [ -4,  -3], [ -2,  -3],
    [ -1,  -3], [  0,  -3], [  1,  -3], [  2,  -3], [  4,  -3],
    [  6,  -3], [ 33,  -3], [-14,  -2], [-10,  -2], [ -5,  -2],
    [ -3,  -2], [ -2,  -2], [ -1,  -2], [  0,  -2], [  1,  -2],
    [  2,  -2], [  3,  -2], [  5,  -2], [  7,  -2], [ 14,  -2],
    [ 19,  -2], [ 25,  -2], [ 43,  -2], [ -7,  -1], [ -3,  -1],
    [ -2,  -1], [ -1,  -1], [  0,  -1], [  1,  -1], [  2,  -1],
    [  3,  -1], [ 10,  -1], [ -5,   0], [ -3,   0], [ -2,   0],
    [ -1,   0], [  1,   0], [  2,   0], [  3,   0], [  5,   0],
    [  7,   0], [-10,   1], [ -7,   1], [ -3,   1], [ -2,   1],
    [ -1,   1], [  0,   1], [  1,   1], [  2,   1], [  3,   1],
    [-43,   2], [-25,   2], [-19,   2], [-14,   2], [ -5,   2],
    [ -3,   2], [ -2,   2], [ -1,   2], [  0,   2], [  1,   2],
    [  2,   2], [  3,   2], [  5,   2], [  7,   2], [ 10,   2],
    [ 14,   2], [-33,   3], [ -6,   3], [ -4,   3], [ -2,   3],
    [ -1,   3], [  0,   3], [  1,   3], [  2,   3], [  4,   3],
    [ 19,   3], [ -9,   4], [ -3,   4], [  3,   4], [  7,   4],
    [ 25,   4], [-13,   5], [ -5,   5], [ -2,   5], [  0,   5],
    [  2,   5], [  5,   5], [  9,   5], [ 33,   5], [ -8,   6],
    [ -4,   6], [  4,   6], [ 13,   6], [ 43,   6], [-18,   7],
    [ -2,   7], [  0,   7], [  2,   7], [  7,   7], [ 18,   7],
    [-24,   8], [ -6,   8], [-42,   9], [-11,   9], [ -4,   9],
    [  5,   9], [ 11,   9], [ 23,   9], [-31,  10], [ -1,  10],
    [  2,  10], [-15,  11], [ -8,  11], [  8,  11], [ 15,  11],
    [ 31,  12], [-21,  13], [ -5,  13], [  5,  13], [ 41,  13],
    [ -1,  14], [  1,  14], [ 21,  14], [-12,  15], [ 12,  15],
    [-39,  17], [-28,  17], [-18,  17], [ -8,  17], [  8,  17],
    [ 17,  18], [ -4,  19], [  0,  19], [  4,  19], [ 27,  19],
    [ 38,  20], [-13,  21], [ 12,  22], [-36,  23], [-24,  23],
    [ -8,  24], [  7,  24], [ -3,  25], [  1,  25], [ 22,  25],
    [ 34,  26], [-18,  28], [-32,  29], [ 16,  29], [-11,  31],
    [  9,  32], [ 29,  32], [ -4,  33], [  2,  33], [-26,  34],
    [ 23,  36], [-19,  39], [ 16,  40], [-13,  41], [  9,  42],
    [ -6,  43], [  1,  43], [  0,   0], [  0,   0], [  0,   0],
];
