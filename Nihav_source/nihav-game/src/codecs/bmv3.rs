use nihav_core::formats;
use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use std::str::FromStr;

const BMV_INTRA:    u8 = 0x03;
const BMV_SCROLL:   u8 = 0x04;
const BMV_PAL:      u8 = 0x08;
const BMV_COMMAND:  u8 = 0x10;
const BMV_PRINT:    u8 = 0x80;

const BMV_MAX_WIDTH: usize = 640;
const BMV_MAX_HEIGHT: usize = 432;
const BMV_MAX_SIZE: usize = BMV_MAX_WIDTH * (BMV_MAX_HEIGHT + 1);

enum BMV3Mode {
    Normal,
    Copy,
    Pixfunc,
    After0,
    After1,
    After1C,
    After4,
    After5,
}

struct NibbleReader {
    nib:        u8,
    has_nib:    bool,
}

impl NibbleReader {
    fn new() -> Self {
        Self { nib: 0, has_nib: false }
    }
    fn get_nib(&mut self, br: &mut ByteReader) -> DecoderResult<u8> {
        if self.has_nib {
            self.has_nib = false;
            Ok(self.nib)
        } else {
            let b = br.read_byte()?;
            self.nib = b >> 4;
            self.has_nib = true;
            Ok(b & 0xF)
        }
    }
    fn get_length(&mut self, br: &mut ByteReader, mut len: usize, mut shift: u8) -> DecoderResult<usize> {
        loop {
            let nib = self.get_nib(br)? as usize;
            len |= nib << shift;
            shift += 2;
            if (nib & 0xC) != 0 {
                return Ok(len - 1);
            }
        }
    }
    fn push(&mut self, val: u8) {
        if self.has_nib {
            panic!("nibble already in cache");
        } else {
            self.nib = val;
            self.has_nib = true;
        }
    }
    fn reset(&mut self) {
        self.nib = 0;
        self.has_nib = false;
    }
}

struct BMV3VideoDecoder {
    info:       NACodecInfoRef,
    stride:     usize,
    height:     usize,
    frame:      Vec<u16>,
    prev_frame: Vec<u16>,
    pixels:     [u16; 256],
    pixbuf:     [[u16; 256]; 7],
    mode:       BMV3Mode,
    pos:        usize,
    end:        usize,
    nr:         NibbleReader,
    is_intra:   bool,
}

impl BMV3VideoDecoder {
    fn new() -> Self {
        let mut pixels = [0u16; 256];
        for (i, el) in pixels.iter_mut().enumerate() {
            *el = i as u16;
        }
        let mut pixbuf = [[0u16; 256]; 7];
        for i in 0..7 {
            for j in 0..256 {
                pixbuf[i][j] = ((i << 8) + j + 0xF8) as u16;
            }
        }

        Self {
            info:       NACodecInfoRef::default(),
            stride:     0,
            height:     0,
            frame:      vec![0; BMV_MAX_SIZE],
            prev_frame: vec![0; BMV_MAX_SIZE],
            pixels, pixbuf,
            mode:       BMV3Mode::Normal,
            pos:        0,
            end:        0,
            nr:         NibbleReader::new(),
            is_intra:   false,
        }
    }
    #[allow(clippy::identity_op)]
    fn decode_frame(&mut self, br: &mut ByteReader) -> DecoderResult<()> {
        let mut idx = 0;
        loop {
            let op                          = br.read_byte()?;
            let mut len;
            let skip;
            if op < 0x90 {
                let op2                     = br.read_u16le()?;
                skip = ((op2 >> 12) as usize) | ((op as usize) << 4);
                len = (op2 & 0xFFF) as usize;
            } else {
                len = ((op & 7) + 1) as usize;
                skip = ((op >> 3) - 0x11) as usize;
            }
            while (idx < 0xF8) && (len > 0) {
                self.pixels[idx]            = br.read_u16le()?;
                idx += 1;
                len -= 1;
            }
            while (idx < 0x7F8) && (len > 0) {
                let nidx = idx - 0xF8;
                self.pixbuf[nidx >> 8][nidx & 0xFF] = br.read_u16le()?;
                idx += 1;
                len -= 1;
            }
            validate!(len == 0);
            if skip == 0 { break; }
            idx += skip;
        }
        self.nr.reset();
        self.mode = BMV3Mode::Normal;
        while br.left() > 0 && self.pos < self.end {
            match self.mode {
                BMV3Mode::Normal => {
                    let op                  = br.read_byte()?;
                    self.decode_normal(br, op)?;
                },
                BMV3Mode::Copy   => {
                    let op                  = br.read_byte()?;
                    if (op & 1) == 0 {
                        self.decode_normal(br, op + 1)?;
                    } else {
                        self.decode_copy(br, op)?;
                    }
                },
                BMV3Mode::Pixfunc => {
                    let op                  = br.read_byte()?;
                    if (op & 1) == 0 {
                        self.decode_copy(br, op + 1)?;
                    } else {
                        self.decode_normal(br, op - 1)?;
                    }
                },
                BMV3Mode::After0 => {
                    let cur_op              = self.nr.get_nib(br)?;
                    if cur_op < 4 {
                        let op              = self.nr.get_nib(br)?;
                        match cur_op {
                            0 => self.decode_mode5c(br, op | 0x10)?,
                            1 => self.decode_mode4 (br, op | 0x10)?,
                            2 => self.decode_mode5c(br, op | 0x30)?,
                            _ => self.decode_mode4 (br, op | 0x30)?,
                        };
                    } else {
                        let len = (cur_op >> 1) - 1;
                        if (cur_op & 1) == 0 {
                            self.pixfunc(br, len as usize)?;
                        } else {
                            self.repeat(len as usize)?;
                        }
                    }
                }
                BMV3Mode::After1 => {
                    let cur_op              = self.nr.get_nib(br)?;
                    if cur_op < 4 {
                        let op              = self.nr.get_nib(br)?;
                        match cur_op {
                            0 => self.decode_mode4 (br, op | 0x10)?,
                            1 => self.decode_mode5c(br, op | 0x00)?,
                            2 => self.decode_mode4 (br, op | 0x30)?,
                            _ => self.decode_mode5c(br, op | 0x20)?,
                        };
                    } else {
                        let len = (cur_op >> 1) - 1;
                        if (cur_op & 1) == 0 {
                            self.repeat(len as usize)?;
                        } else {
                            self.copy(len as usize)?;
                        }
                    }
                },
                BMV3Mode::After1C => {
                    let cur_op              = self.nr.get_nib(br)?;
                    if cur_op < 4 {
                        let cur_op1         = self.nr.get_nib(br)?;
                        let m5_op = cur_op1 | (cur_op << 4);
                        self.decode_mode5c(br, m5_op)?;
                    } else if (cur_op & 1) == 0 {
                        let len = (cur_op >> 1) - 1;
                        self.copy(len as usize)?;
                    } else {
                        let len = (cur_op >> 1) - 1;
                        self.pixfunc(br, len as usize)?;
                    }
                },
                BMV3Mode::After4 => {
                    let cur_op0             = self.nr.get_nib(br)?;
                    let cur_op1             = self.nr.get_nib(br)?;
                    let cur_op = (cur_op0 << 4) | cur_op1;
                    if (cur_op & 0x10) == 0 {
                        self.decode_mode5c(br, cur_op | 0x10)?;
                    } else {
                        self.decode_mode4(br, cur_op)?;
                    }
                },
                BMV3Mode::After5 => {
                    let cur_op0             = self.nr.get_nib(br)?;
                    let cur_op1             = self.nr.get_nib(br)?;
                    let cur_op = (cur_op0 << 4) | cur_op1;
                    if (cur_op & 0x10) == 0 {
                        self.decode_mode4(br, cur_op | 0x10)?;
                    } else {
                        self.decode_mode5c(br, cur_op ^ 0x10)?;
                    }
                },
            };
            if self.pos >= self.end { break; }
        }
        Ok(())
    }

    fn copy(&mut self, len: usize) -> DecoderResult<()> {
        validate!(len <= self.end - self.pos);
        if self.is_intra {
            for _ in 0..len {
                self.frame[self.pos] = self.frame[self.pos - self.stride];
                self.pos += 1;
            }
        } else {
            for _ in 0..len {
                self.frame[self.pos] = self.prev_frame[self.pos];
                self.pos += 1;
            }
        }
        self.mode = BMV3Mode::Copy;
        Ok(())
    }
    fn pixfunc(&mut self, br: &mut ByteReader, len: usize) -> DecoderResult<()> {
        validate!(len <= self.end - self.pos);
        for _ in 0..len {
            let op                          = BMV_PIXFUNCS_MAP[br.read_byte()? as usize];
            let val;
            if op == 0xFF {
                val                         = br.read_u16le()?;
            } else if op >= 0xF8 {
                let tab_idx = (op - 0xF8) as usize;
                let sub_idx                 = br.read_byte()? as usize;
                val = self.pixbuf[tab_idx][sub_idx];
            } else {
                val = self.pixels[op as usize];
            }
            self.frame[self.pos] = val;
            self.pos += 1;
        }
        self.mode = BMV3Mode::Pixfunc;
        Ok(())
    }
    fn repeat(&mut self, len: usize) -> DecoderResult<()> {
        validate!(self.pos > 0);
        validate!(len <= self.end - self.pos);
        let pix = self.frame[self.pos - 1];
        for _ in 0..len {
            self.frame[self.pos] = pix;
            self.pos += 1;
        }
        self.mode = BMV3Mode::Normal;
        Ok(())
    }

    fn decode_normal(&mut self, br: &mut ByteReader, op: u8) -> DecoderResult<()> {
        if op < 0x40 {
            let mode = op & 1;
            let len = ((op >> 1) & 0x7) as usize;
            if len < 2 {
                let mut len = (op >> 3) as usize;
                if (op & 0xF) >= 2 {
                    len += 1;
                }
                len                         = self.nr.get_length(br, len, 3)?;
                if (op & 1) == 0 {
                    self.copy(len)?;
                    if self.nr.has_nib {
                        self.mode = BMV3Mode::After0;
                    }
                } else {
                    self.pixfunc(br, len)?;
                    if self.nr.has_nib {
                        self.mode = BMV3Mode::After1;
                    }
                }
            } else if mode == 0 {
                self.copy(len - 1)?;
                self.nr.push(op >> 4);
                self.mode = BMV3Mode::After4;
            } else {
                self.pixfunc(br, len - 1)?;
                self.nr.push(op >> 4);
                self.mode = BMV3Mode::After5;
            }
            return Ok(());
        }
        let x_op = (op >> 4) as usize;
        let y_op = ((op >> 1) & 7) as usize;
        let flag = (op & 1) as usize;
        if y_op == 0 || y_op == 1 {
            let len = x_op * 2 - 1 + y_op;
            if flag == 0 {
                self.copy(len)?;
            } else {
                self.pixfunc(br, len)?;
            }
        } else {
            let len1 = y_op - 1;
            let len2 = (x_op >> 1) - 1;
            if flag == 0 {
                self.copy(len1)?;
            } else {
                self.pixfunc(br, len1)?;
            }
            match (x_op & 1) * 2 + flag {
                0 => self.pixfunc(br, len2)?,
                1 => self.repeat(len2)?,
                2 => self.repeat(len2)?,
                _ => self.copy(len2)?,
            };
        }
        Ok(())
    }
    fn decode_copy(&mut self, br: &mut ByteReader, op: u8) -> DecoderResult<()> {
        if op < 0x40 {
            let len = ((op >> 1) & 0x7) as usize;
            if len < 2 {
                let mut len = (op >> 3) as usize;
                if (op & 0xF) >= 2 {
                    len += 1;
                }
                len                         = self.nr.get_length(br, len, 3)?;
                self.repeat(len)?;
                if self.nr.has_nib {
                    self.mode = BMV3Mode::After1C;
                }
            } else {
                self.repeat(len - 1)?;
                if br.left() == 0 { return Ok(()); }
                let op2                     = self.nr.get_nib(br)?;
                let cur_op = (op & 0xF0) | op2;
                self.decode_mode5c(br, cur_op)?;
            }
            return Ok(());
        }
        let x_op = (op >> 4) as usize;
        let y_op = ((op >> 1) & 7) as usize;
        if y_op == 0 || y_op == 1 {
            self.repeat(x_op * 2 - 1 + y_op)?;
        } else {
            self.repeat(y_op - 1)?;
            let len = (x_op >> 1) - 1;
            if (x_op & 1) == 0 {
                self.copy(len)?;
            } else {
                self.pixfunc(br, len)?;
            }
        }
        Ok(())
    }
    fn decode_mode4(&mut self, br: &mut ByteReader, op: u8) -> DecoderResult<()> {
        if (op & 0xF) < 4 {
            let mut len = ((op & 3) * 2) as usize;
            if (op & 0xF0) >= 0x20 {
                len += 1;
            }
            len                         = self.nr.get_length(br, len, 3)?;
            self.repeat(len)?;
            if self.nr.has_nib {
                self.mode = BMV3Mode::After1C;
            }
        } else {
            let len = ((op & 0xF) * 2 - 1 + (op >> 5)) as usize;
            self.repeat(len)?;
            self.mode = BMV3Mode::After1C;
        }
        Ok(())
    }
    fn decode_mode5c(&mut self, br: &mut ByteReader, op: u8) -> DecoderResult<()> {
        if (op & 0xF) < 4 {
            let mut len = ((op & 3) * 2) as usize;
            if (op & 0xF0) >= 0x20 {
                len += 1;
            }
            len                         = self.nr.get_length(br, len, 3)?;
            if (op & 0x10) == 0 {
                self.copy(len)?;
                if self.nr.has_nib {
                    self.mode = BMV3Mode::After0;
                }
            } else {
                self.pixfunc(br, len)?;
                if self.nr.has_nib {
                    self.mode = BMV3Mode::After1;
                }
            }
        } else {
            let len = ((op & 0xF) * 2 - 1 + (op >> 5)) as usize;
            if (op & 0x10) == 0 {
                self.copy(len)?;
                self.mode = BMV3Mode::After0;
            } else {
                self.pixfunc(br, len)?;
                self.mode = BMV3Mode::After1;
            }
        }
        Ok(())
    }
}

impl NADecoder for BMV3VideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, RGB565_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            self.stride = vinfo.get_width();
            self.height = vinfo.get_height();
            self.end    = self.stride * (self.height + 1);

            validate!((self.stride <= 640) && (self.height <= 432));

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
        let flags                               = br.read_byte()?;

        if (flags & BMV_COMMAND) != 0 {
            let size = if (flags & BMV_PRINT) != 0 { 8 } else { 10 };
            br.read_skip(size)?;
        }
        if (flags & BMV_PAL) != 0 {
            return Err(DecoderError::InvalidData);
        }
        let off;
        if ((flags & 1) == 0) && ((flags & BMV_SCROLL) != 0) {
            off                                 = br.read_u16le()? as usize;
        } else {
            off = 0;
        }
        self.pos = off + self.stride;
        self.is_intra = (flags & BMV_INTRA) == BMV_INTRA;

        let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;

        self.decode_frame(&mut br)?;

        {
            let mut buf = bufinfo.get_vbuf16().unwrap();
            let stride = buf.get_stride(0);
            let data = buf.get_data_mut().unwrap();
            let dst = data.as_mut_slice();

            let refbuf = &self.frame[self.stride..];
            for (dst, src) in dst.chunks_mut(stride).zip(refbuf.chunks(self.stride)).take(self.height) {
                let out = &mut dst[0..self.stride];
                out.copy_from_slice(src);
            }
        }
        std::mem::swap(&mut self.frame, &mut self.prev_frame);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(self.is_intra);
        frm.set_frame_type(if self.is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for BMV3VideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder_video() -> Box<dyn NADecoder + Send> {
    Box::new(BMV3VideoDecoder::new())
}

struct BMV3AudioDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    pred:       [i16; 2],
    nframes:    usize,
}

impl BMV3AudioDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, formats::SND_S16P_FORMAT, 0),
            chmap:      NAChannelMap::new(),
            pred:       [0; 2],
            nframes:    0,
        }
    }
}

#[allow(clippy::identity_op)]
fn decode_block(mode: u8, src: &[u8], dst: &mut [i16], mut pred: i16) -> i16 {
    let steps = &BMV_AUDIO_STEPS[mode as usize];
    let mut val2 = 0;
    for i in 0..10 {
        let val = (src[i * 2 + 0] as usize) + (src[i * 2 + 1] as usize) * 256;
        pred = pred.wrapping_add(steps[(val >> 10) & 0x1F]);
        dst[i * 3 + 0] = pred;
        pred = pred.wrapping_add(steps[(val >>  5) & 0x1F]);
        dst[i * 3 + 1] = pred;
        pred = pred.wrapping_add(steps[(val >>  0) & 0x1F]);
        dst[i * 3 + 2] = pred;
        val2 = (val2 << 1) | (val >> 15);
    }
    pred = pred.wrapping_add(steps[(val2 >> 5) & 0x1F]);
    dst[3 * 10 + 0] = pred;
    pred = pred.wrapping_add(steps[(val2 >> 0) & 0x1F]);
    dst[3 * 10 + 1] = pred;
    pred
}

impl NADecoder for BMV3AudioDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), ainfo.get_channels(), formats::SND_S16P_FORMAT, 32);
            self.chmap = NAChannelMap::from_str("L,R").unwrap();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            validate!(pktbuf.len() > 1);
            let samples = (pktbuf.len() / 41) * 32;
            let abuf = alloc_audio_buffer(self.ainfo, samples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let off1 = adata.get_offset(1);
            let dst = adata.get_data_mut().unwrap();
            let mut first = pktbuf[0] == 0;
            let psrc = &pktbuf[1..];
            for (n, src) in psrc.chunks_exact(41).enumerate() {
                let aoff0 = n * 32;
                let aoff1 = aoff0 + off1;
                if first {
                    let mode = src[40];
                    self.pred[0] = decode_block(mode >> 4, &src[0..], &mut dst[aoff0..], self.pred[0]);
                    self.pred[1] = decode_block(mode & 0xF, &src[20..], &mut dst[aoff1..], self.pred[1]);
                } else {
                    let mode = src[0];
                    self.pred[0] = decode_block(mode >> 4, &src[1..], &mut dst[aoff0..], self.pred[0]);
                    self.pred[1] = decode_block(mode & 0xF, &src[21..], &mut dst[aoff1..], self.pred[1]);
                }
                first = !first;
            }
            self.nframes += 1;
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

impl NAOptionHandler for BMV3AudioDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_audio() -> Box<dyn NADecoder + Send> {
    Box::new(BMV3AudioDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    // sample: https://samples.mplayerhq.hu/game-formats/bmv/DW3-Loffnote.bmv
    #[test]
    fn test_bmv_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("bmv3", "bmv3-video", "assets/Game/DW3-Loffnote.bmv", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xfa34b81b, 0xd0ab79e2, 0x78fb25cc, 0x98ae47ff]));
    }
    #[test]
    fn test_bmv_audio() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("bmv3", "bmv3-audio", "assets/Game/DW3-Loffnote.bmv", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xd8e743cc, 0x97604bd7, 0x8dbb89c6, 0xb34cad63]));
    }
}

const BMV_PIXFUNCS_MAP: [u8; 256] = [
    0x38, 0x78, 0xB8, 0xF9,
    0x39, 0x79, 0xB9, 0xFA,
    0x3A, 0x7A, 0xBA, 0xFB,
    0x3B, 0x7B, 0xBB, 0xFC,
    0x3C, 0x7C, 0xBC, 0xFD,
    0x3D, 0x7D, 0xBD, 0xFE,
    0x3E, 0x7E, 0xBE, 0xFF,
    0x3F, 0x7F, 0xBF, 0x00,
    0x40, 0x80, 0xC0, 0x01,
    0x41, 0x81, 0xC1, 0x02,
    0x42, 0x82, 0xC2, 0x03,
    0x43, 0x83, 0xC3, 0x04,
    0x44, 0x84, 0xC4, 0x05,
    0x45, 0x85, 0xC5, 0x06,
    0x46, 0x86, 0xC6, 0x07,
    0x47, 0x87, 0xC7, 0x08,
    0x48, 0x88, 0xC8, 0x09,
    0x49, 0x89, 0xC9, 0x0A,
    0x4A, 0x8A, 0xCA, 0x0B,
    0x4B, 0x8B, 0xCB, 0x0C,
    0x4C, 0x8C, 0xCC, 0x0D,
    0x4D, 0x8D, 0xCD, 0x0E,
    0x4E, 0x8E, 0xCE, 0x0F,
    0x4F, 0x8F, 0xCF, 0x10,
    0x50, 0x90, 0xD0, 0x11,
    0x51, 0x91, 0xD1, 0x12,
    0x52, 0x92, 0xD2, 0x13,
    0x53, 0x93, 0xD3, 0x14,
    0x54, 0x94, 0xD4, 0x15,
    0x55, 0x95, 0xD5, 0x16,
    0x56, 0x96, 0xD6, 0x17,
    0x57, 0x97, 0xD7, 0x18,
    0x58, 0x98, 0xD8, 0x19,
    0x59, 0x99, 0xD9, 0x1A,
    0x5A, 0x9A, 0xDA, 0x1B,
    0x5B, 0x9B, 0xDB, 0x1C,
    0x5C, 0x9C, 0xDC, 0x1D,
    0x5D, 0x9D, 0xDD, 0x1E,
    0x5E, 0x9E, 0xDE, 0x1F,
    0x5F, 0x9F, 0xDF, 0x20,
    0x60, 0xA0, 0xE0, 0x21,
    0x61, 0xA1, 0xE1, 0x22,
    0x62, 0xA2, 0xE2, 0x23,
    0x63, 0xA3, 0xE3, 0x24,
    0x64, 0xA4, 0xE4, 0x25,
    0x65, 0xA5, 0xE5, 0x26,
    0x66, 0xA6, 0xE6, 0x27,
    0x67, 0xA7, 0xE7, 0x28,
    0x68, 0xA8, 0xE8, 0x29,
    0x69, 0xA9, 0xE9, 0x2A,
    0x6A, 0xAA, 0xEA, 0x2B,
    0x6B, 0xAB, 0xEB, 0x2C,
    0x6C, 0xAC, 0xEC, 0x2D,
    0x6D, 0xAD, 0xED, 0x2E,
    0x6E, 0xAE, 0xEE, 0x2F,
    0x6F, 0xAF, 0xEF, 0x30,
    0x70, 0xB0, 0xF0, 0x31,
    0x71, 0xB1, 0xF1, 0x32,
    0x72, 0xB2, 0xF2, 0x33,
    0x73, 0xB3, 0xF3, 0x34,
    0x74, 0xB4, 0xF4, 0x35,
    0x75, 0xB5, 0xF5, 0x36,
    0x76, 0xB6, 0xF6, 0x37,
    0x77, 0xB7, 0xF7, 0xF8
];

const BMV_AUDIO_STEPS: [[i16; 32]; 16] = [
    [
         0x0000,  0x0400,  0x0800,  0x0C00,  0x1000,  0x1400,  0x1800,  0x1C00,
         0x2000,  0x2400,  0x2800,  0x2C00,  0x3000,  0x3400,  0x3800,  0x3C00,
        -0x4000, -0x3C00, -0x3800, -0x3400, -0x3000, -0x2C00, -0x2800, -0x2400,
        -0x2000, -0x1C00, -0x1800, -0x1400, -0x1000, -0x0C00, -0x0800, -0x0400
    ], [
         0x0000,  0x0200,  0x0400,  0x0600,  0x0800,  0x0A00,  0x0C00,  0x0E00,
         0x1000,  0x1200,  0x1400,  0x1600,  0x1800,  0x1A00,  0x1C00,  0x1E00,
        -0x2000, -0x1E00, -0x1C00, -0x1A00, -0x1800, -0x1600, -0x1400, -0x1200,
        -0x1000, -0x0E00, -0x0C00, -0x0A00, -0x0800, -0x0600, -0x0400, -0x0200
    ], [
         0x0000,  0x0100,  0x0200,  0x0300,  0x0400,  0x0500,  0x0600,  0x0700,
         0x0800,  0x0900,  0x0A00,  0x0B00,  0x0C00,  0x0D00,  0x0E00,  0x0F00,
        -0x1000, -0x0F00, -0x0E00, -0x0D00, -0x0C00, -0x0B00, -0x0A00, -0x0900,
        -0x0800, -0x0700, -0x0600, -0x0500, -0x0400, -0x0300, -0x0200, -0x0100
    ], [
         0x000,  0x080,  0x100,  0x180,  0x200,  0x280,  0x300,  0x380,
         0x400,  0x480,  0x500,  0x580,  0x600,  0x680,  0x700,  0x780,
        -0x800, -0x780, -0x700, -0x680, -0x600, -0x580, -0x500, -0x480,
        -0x400, -0x380, -0x300, -0x280, -0x200, -0x180, -0x100, -0x080
    ], [
         0x000,  0x048,  0x090,  0x0D8,  0x120,  0x168,  0x1B0,  0x1F8,
         0x240,  0x288,  0x2D0,  0x318,  0x360,  0x3A8,  0x3F0,  0x438,
        -0x480, -0x438, -0x3F0, -0x3A8, -0x360, -0x318, -0x2D0, -0x288,
        -0x240, -0x1F8, -0x1B0, -0x168, -0x120, -0x0D8, -0x090, -0x048
    ], [
         0x000,  0x030,  0x060,  0x090,  0x0C0,  0x0F0,  0x120,  0x150,
         0x180,  0x1B0,  0x1E0,  0x210,  0x240,  0x270,  0x2A0,  0x2D0,
        -0x300, -0x2D0, -0x2A0, -0x270, -0x240, -0x210, -0x1E0, -0x1B0,
        -0x180, -0x150, -0x120, -0x0F0, -0x0C0, -0x090, -0x060, -0x030
    ], [
         0x000,  0x020,  0x040,  0x060,  0x080,  0x0A0,  0x0C0,  0x0E0,
         0x100,  0x120,  0x140,  0x160,  0x180,  0x1A0,  0x1C0,  0x1E0,
        -0x200, -0x1E0, -0x1C0, -0x1A0, -0x180, -0x160, -0x140, -0x120,
        -0x100, -0x0E0, -0x0C0, -0x0A0, -0x080, -0x060, -0x040, -0x020
    ], [
         0x000,  0x016,  0x02C,  0x042,  0x058,  0x06E,  0x084,  0x09A,
         0x0B0,  0x0C6,  0x0DC,  0x0F2,  0x108,  0x11E,  0x134,  0x14A,
        -0x160, -0x14A, -0x134, -0x11E, -0x108, -0x0F2, -0x0DC, -0x0C6,
        -0x0B0, -0x09A, -0x084, -0x06E, -0x058, -0x042, -0x02C, -0x016
    ], [
         0x000,  0x010,  0x020,  0x030,  0x040,  0x050,  0x060,  0x070,
         0x080,  0x090,  0x0A0,  0x0B0,  0x0C0,  0x0D0,  0x0E0,  0x0F0,
        -0x100, -0x0F0, -0x0E0, -0x0D0, -0x0C0, -0x0B0, -0x0A0, -0x090,
        -0x080, -0x070, -0x060, -0x050, -0x040, -0x030, -0x020, -0x010
    ], [
         0x00,  0x0B,  0x16,  0x21,  0x2C,  0x37,  0x42,  0x4D,
         0x58,  0x63,  0x6E,  0x79,  0x84,  0x8F,  0x9A,  0xA5,
        -0xB0, -0xA5, -0x9A, -0x8F, -0x84, -0x79, -0x6E, -0x63,
        -0x58, -0x4D, -0x42, -0x37, -0x2C, -0x21, -0x16, -0x0B
    ], [
         0x00,  0x08,  0x10,  0x18,  0x20,  0x28,  0x30,  0x38,
         0x40,  0x48,  0x50,  0x58,  0x60,  0x68,  0x70,  0x78,
        -0x80, -0x78, -0x70, -0x68, -0x60, -0x58, -0x50, -0x48,
        -0x40, -0x38, -0x30, -0x28, -0x20, -0x18, -0x10, -0x08
    ], [
         0x00,  0x06,  0x0C,  0x12,  0x18,  0x1E,  0x24,  0x2A,
         0x30,  0x36,  0x3C,  0x42,  0x48,  0x4E,  0x54,  0x5A,
        -0x60, -0x5A, -0x54, -0x4E, -0x48, -0x42, -0x3C, -0x36,
        -0x30, -0x2A, -0x24, -0x1E, -0x18, -0x12, -0x0C, -0x06
    ], [
         0x00,  0x04,  0x08,  0x0C,  0x10,  0x14,  0x18,  0x1C,
         0x20,  0x24,  0x28,  0x2C,  0x30,  0x34,  0x38,  0x3C,
        -0x40, -0x3C, -0x38, -0x34, -0x30, -0x2C, -0x28, -0x24,
        -0x20, -0x1C, -0x18, -0x14, -0x10, -0x0C, -0x08, -0x04
    ], [
         0x00,  0x02,  0x05,  0x08,  0x0B,  0x0D,  0x10,  0x13,
         0x16,  0x18,  0x1B,  0x1E,  0x21,  0x23,  0x26,  0x29,
        -0x2C, -0x2A, -0x27, -0x24, -0x21, -0x1F, -0x1C, -0x19,
        -0x16, -0x14, -0x11, -0x0E, -0x0B, -0x09, -0x06, -0x03
    ], [
         0x00,  0x01,  0x03,  0x05,  0x07,  0x08,  0x0A,  0x0C,
         0x0E,  0x0F,  0x11,  0x13,  0x15,  0x16,  0x18,  0x1A,
        -0x1C, -0x1B, -0x19, -0x17, -0x15, -0x14, -0x12, -0x10,
        -0x0E, -0x0D, -0x0B, -0x09, -0x07, -0x06, -0x04, -0x02
    ], [
         0x00,  0x01,  0x02,  0x03,  0x04,  0x05,  0x06,  0x07,
         0x08,  0x09,  0x0A,  0x0B,  0x0C,  0x0D,  0x0E,  0x0F,
        -0x10, -0x0F, -0x0E, -0x0D, -0x0C, -0x0B, -0x0A, -0x09,
        -0x08, -0x07, -0x06, -0x05, -0x04, -0x03, -0x02, -0x01
    ]
];
