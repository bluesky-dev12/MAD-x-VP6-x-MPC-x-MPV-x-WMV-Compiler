use nihav_core::io::byteio::{ByteReader,MemoryReader};
use nihav_core::formats::YUV420_FORMAT;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::HAMShuffler;
use std::io::SeekFrom;

#[derive(Clone,Copy,PartialEq)]
enum DecodeMode {
    YUV,
    Gray,
    Palette,
    Unknown,
}

struct CinepakDecoder {
    info:   NACodecInfoRef,
    frmmgr: HAMShuffler<u8>,
    cb_v1:  Vec<[[u8; 6]; 256]>,
    cb_v4:  Vec<[[u8; 6]; 256]>,
    mode:   DecodeMode,
}

fn put_block(block: &[u8; 24], x: usize, y: usize, frm: &mut NASimpleVideoFrame<u8>) {
    let mut yoff = frm.offset[0] + x + y * frm.stride[0];
    for i in 0..4 {
        for j in 0..4 {
            frm.data[yoff + j] = block[j + i * 4];
        }
        yoff += frm.stride[0];
    }
    let mut uoff = frm.offset[1] + x / 2 + y / 2 * frm.stride[1];
    for i in 0..2 {
        for j in 0..2 {
            frm.data[uoff + j] = block[j + i * 2 + 16];
        }
        uoff += frm.stride[1];
    }
    let mut voff = frm.offset[2] + x / 2 + y / 2 * frm.stride[2];
    for i in 0..2 {
        for j in 0..2 {
            frm.data[voff + j] = block[j + i * 2 + 20];
        }
        voff += frm.stride[2];
    }
}

fn put_block_gray(block: &[u8; 24], x: usize, y: usize, frm: &mut NASimpleVideoFrame<u8>) {
    let mut yoff = frm.offset[0] + x + y * frm.stride[0];
    for i in 0..4 {
        for j in 0..4 {
            frm.data[yoff + j] = block[j + i * 4];
        }
        yoff += frm.stride[0];
    }
}

impl CinepakDecoder {
    fn new() -> Self {
        CinepakDecoder {
            info:   NACodecInfo::new_dummy(),
            frmmgr: HAMShuffler::new(),
            cb_v1:  Vec::with_capacity(1),
            cb_v4:  Vec::with_capacity(1),
            mode:   DecodeMode::Unknown,
        }
    }
    fn read_cb(br: &mut ByteReader, size: usize, cb: &mut [[u8; 6]; 256], is_yuv: bool) -> DecoderResult<()> {
        let cb_elem = if is_yuv { 6 } else { 4 };
        let cb_size = (size - 4) / cb_elem;
        validate!(size - 4 == cb_size * cb_elem);
        validate!(cb_size <= 256);
        for i in 0..cb_size {
                                          br.read_buf(&mut cb[i][..cb_elem])?;
            if !is_yuv {
                cb[i][4] = 0x80;
                cb[i][5] = 0x80;
            } else {
                cb[i][4] ^= 0x80;
                cb[i][5] ^= 0x80;
            }
        }
        Ok(())
    }
    fn read_cb_upd(br: &mut ByteReader, size: usize, cb: &mut [[u8; 6]; 256], is_yuv: bool) -> DecoderResult<()> {
        let cb_elem = if is_yuv { 6 } else { 4 };
        let end = br.tell() + (size as u64) - 4;
        for i in (0..256).step_by(32) {
            if br.tell() >= end {
                break;
            }
            let upd                     = br.read_u32be()?;
            for j in 0..32 {
                if ((upd >> (31 - j)) & 1) != 0 {
                                          br.read_buf(&mut cb[i + j][..cb_elem])?;
                    if !is_yuv {
                        cb[i + j][4] = 0x80;
                        cb[i + j][5] = 0x80;
                    } else {
                        cb[i + j][4] ^= 0x80;
                        cb[i + j][5] ^= 0x80;
                    }
                }
            }
        }
        validate!(br.tell() == end);
        Ok(())
    }
    fn decode_strip(&mut self, src: &[u8], sno: usize, is_intra: bool, is_intra_strip: bool, xoff: usize, yoff: usize, xend: usize, yend: usize, frm: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        let mut mr = MemoryReader::new_read(src);
        let mut br = ByteReader::new(&mut mr);
        let mut idx_pos = 0;
        let mut idx_size = 0;
        let mut v1_only = false;
        let mut intra_mode = true;
        while br.left() > 0 {
            let id                      = br.read_byte()?;
            if (id & 0xF0) == 0x20 && is_intra_strip {
                validate!((id & 1) == 0);
            }
            let size                    = br.read_u24be()? as usize;
            validate!(size >= 4 && (size - 4 <= (br.left() as usize)));
            match id {
                0x20 => Self::read_cb    (&mut br, size, &mut self.cb_v4[sno], true)?,
                0x21 => Self::read_cb_upd(&mut br, size, &mut self.cb_v4[sno], true)?,
                0x22 => Self::read_cb    (&mut br, size, &mut self.cb_v1[sno], true)?,
                0x23 => Self::read_cb_upd(&mut br, size, &mut self.cb_v1[sno], true)?,
                0x24 => Self::read_cb    (&mut br, size, &mut self.cb_v4[sno], false)?,
                0x25 => Self::read_cb_upd(&mut br, size, &mut self.cb_v4[sno], false)?,
                0x26 => Self::read_cb    (&mut br, size, &mut self.cb_v1[sno], false)?,
                0x27 => Self::read_cb_upd(&mut br, size, &mut self.cb_v1[sno], false)?,
                0x30 => { // intra indices
                    validate!(idx_pos == 0);
                    idx_pos = br.tell() as usize;
                    idx_size = size - 4;
                                          br.read_skip(idx_size)?;
                },
                0x31 => { // inter indices
                    validate!(!is_intra);
                    validate!(idx_pos == 0);
                    intra_mode = false;
                    idx_pos = br.tell() as usize;
                    idx_size = size - 4;
                                          br.read_skip(idx_size)?;
                },
                0x32 => { // V1-only blocks
                    validate!(idx_pos == 0);
                    idx_pos = br.tell() as usize;
                    idx_size = size - 4;
                    v1_only = true;
                                          br.read_skip(idx_size)?;
                },
                _ => return Err(DecoderError::InvalidData),
            };
        }
        validate!(idx_pos != 0);
        let mut mr = MemoryReader::new_read(&src[idx_pos..][..idx_size]);
        let mut br = ByteReader::new(&mut mr);

        let mut x = xoff;
        let mut y = yoff;
        let mut block = [0u8; 24];
        while br.left() > 0 {
            let mut flags = if !v1_only { br.read_u32be()? } else { 0x00000000 };
            let mut mask = 1 << 31;
            while mask > 0 {
                if !intra_mode {
                    let skip = (flags & mask) == 0;
                    mask >>= 1;
                    if skip {
                        x += 4;
                        if x >= xend {
                            x = xoff;
                            y += 4;
                            if y == yend {
                                return Ok(());
                            }
                        }
                        continue;
                    }
                    if mask == 0 {
                        flags           = br.read_u32be()?;
                        mask = 1 << 31;
                    }
                }
                if (flags & mask) == 0 {
                    let idx         = br.read_byte()? as usize;
                    let cb = &self.cb_v1[sno][idx];
                    block[ 0] = cb[0]; block[ 1] = cb[0]; block[ 2] = cb[1]; block[ 3] = cb[1];
                    block[ 4] = cb[0]; block[ 5] = cb[0]; block[ 6] = cb[1]; block[ 7] = cb[1];
                    block[ 8] = cb[2]; block[ 9] = cb[2]; block[10] = cb[3]; block[11] = cb[3];
                    block[12] = cb[2]; block[13] = cb[2]; block[14] = cb[3]; block[15] = cb[3];
                    block[16] = cb[4]; block[17] = cb[4];
                    block[18] = cb[4]; block[19] = cb[4];
                    block[20] = cb[5]; block[21] = cb[5];
                    block[22] = cb[5]; block[23] = cb[5];
                } else {
                    let idx0        = br.read_byte()? as usize;
                    let cb0 = &self.cb_v4[sno][idx0];
                    let idx1        = br.read_byte()? as usize;
                    let cb1 = &self.cb_v4[sno][idx1];
                    let idx2        = br.read_byte()? as usize;
                    let cb2 = &self.cb_v4[sno][idx2];
                    let idx3        = br.read_byte()? as usize;
                    let cb3 = &self.cb_v4[sno][idx3];
                    block[ 0] = cb0[0]; block[ 1] = cb0[1]; block[ 2] = cb1[0]; block[ 3] = cb1[1];
                    block[ 4] = cb0[2]; block[ 5] = cb0[3]; block[ 6] = cb1[2]; block[ 7] = cb1[3];
                    block[ 8] = cb2[0]; block[ 9] = cb2[1]; block[10] = cb3[0]; block[11] = cb3[1];
                    block[12] = cb2[2]; block[13] = cb2[3]; block[14] = cb3[2]; block[15] = cb3[3];
                    block[16] = cb0[4]; block[17] = cb1[4];
                    block[18] = cb2[4]; block[19] = cb3[4];
                    block[20] = cb0[5]; block[21] = cb1[5];
                    block[22] = cb2[5]; block[23] = cb3[5];
                }
                mask >>= 1;
                if self.mode == DecodeMode::YUV {
                    put_block(&block, x, y, frm);
                } else {
                    put_block_gray(&block, x, y, frm);
                }
                x += 4;
                if x >= xend {
                    x = xoff;
                    y += 4;
                    if y == yend {
                        return Ok(());
                    }
                }
            }
        }
        Ok(())
    }
}

impl NADecoder for CinepakDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(_vinfo) = info.get_properties() {
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, false, YUV420_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, None).into_ref();
            self.frmmgr.clear();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        if src.len() <= 10 { return Err(DecoderError::ShortData); }

        let mut mr = MemoryReader::new_read(src.as_slice());
        let mut br = ByteReader::new(&mut mr);

        let flags                       = br.read_byte()?;
        let size                        = br.read_u24be()? as usize;
        validate!(src.len() >= size);
        let width                       = br.read_u16be()? as usize;
        let height                      = br.read_u16be()? as usize;
        let nstrips                     = br.read_u16be()? as usize;

        let is_intra = (flags & 1) == 0;

        let mut mode = DecodeMode::Unknown;
                                          br.read_skip(1)?;
        let mut stripsize               = br.read_u24be()?;
                                          br.read_skip(8)?;
        while stripsize > 0 {
            let ctype                   = br.read_byte()?;
            let csize                   = br.read_u24be()?;
            match ctype {
                0x20 | 0x21 | 0x22 | 0x23 => {
                    mode = DecodeMode::YUV;
                    break;
                },
                0x24 | 0x25 | 0x26 | 0x27 => {
                    mode = DecodeMode::Gray;
                    break;
                },
                _ => {
                                          br.read_skip(csize as usize)?;
                    validate!(stripsize >= csize);
                    stripsize -= csize;
                },
            };
        }
        validate!(mode != DecodeMode::Unknown);
        br.seek(SeekFrom::Start(10))?;
        for sd in pkt.side_data.iter() {
            match *sd {
                NASideData::Palette(_, _) => {
                    mode = DecodeMode::Palette;
                    break;
                },
                _ => {},
            };
        }
        self.cb_v1.resize(nstrips, [[0; 6]; 256]);
        self.cb_v4.resize(nstrips, [[0; 6]; 256]);

        if let Some(ref vinfo) = self.info.get_properties().get_video_info() {
            if vinfo.width != width || vinfo.height != height || self.mode != mode {
                validate!(is_intra);
                let fmt = match mode {
                        DecodeMode::YUV     => YUV420_FORMAT,
                        DecodeMode::Gray    => NAPixelFormaton {
                                model: ColorModel::YUV(YUVSubmodel::YUVJ),
                                components: 1,
                                comp_info: [Some(NAPixelChromaton{h_ss: 0, v_ss: 0, packed: false, depth: 8, shift: 0, comp_offs: 0, next_elem: 1}), None, None, None, None],
                                elem_size: 1,
                                be: true,
                                alpha: false,
                                palette: false,
                            },
                        DecodeMode::Palette => PAL8_FORMAT,
                        _ => unreachable!(),
                    };
                let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(width, height, false, fmt));
                self.info = NACodecInfo::new_ref(self.info.get_name(), myinfo, None).into_ref();
                self.frmmgr.clear();
            }
        }
        let mut buf;
        if is_intra {
            let vinfo = self.info.get_properties().get_video_info().unwrap();
            let bufinfo = alloc_video_buffer(vinfo, 2)?;
            buf = bufinfo.get_vbuf().unwrap();
            self.mode = mode;
        } else {
            validate!(self.mode == mode);
            let bufret = self.frmmgr.clone_ref();
            if let Some(vbuf) = bufret {
                buf = vbuf;
            } else {
                return Err(DecoderError::MissingReference);
            }
        }
        if self.mode == DecodeMode::Palette {
            let paloff = buf.get_offset(1);
            let data = buf.get_data_mut().unwrap();
            let dpal = &mut data[paloff..];
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
        }
        let mut frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();

        let mut last_y = 0;
        for i in 0..nstrips {
            let flags                   = br.read_byte()?;
            validate!(flags == 0x10 || flags == 0x11);
            let is_intra_strip = (flags & 1) == 0;
            let size                    = br.read_u24be()? as usize;
            validate!(size > 12 && (size - 4) <= (br.left() as usize));
            let yoff                    = br.read_u16be()? as usize;
            let xoff                    = br.read_u16be()? as usize;
            if xoff != 0 || yoff != 0 {
                return Err(DecoderError::NotImplemented);
            }
            let yend                    = br.read_u16be()? as usize;
            let xend                    = br.read_u16be()? as usize;
            if i == 0 && is_intra && !is_intra_strip {
                return Err(DecoderError::InvalidData);
            }
            let start = br.tell() as usize;
            let end = start + size - 12;
            let strip_data = &src[start..end];
            if is_intra && i > 0 {
                self.cb_v1[i] = self.cb_v1[i - 1];
                self.cb_v4[i] = self.cb_v4[i - 1];
            }
            self.decode_strip(strip_data, i, is_intra, is_intra_strip, 0, last_y, xend, last_y + yend, &mut frm)?;
                                          br.read_skip(size - 12)?;
            last_y += yend;
        }

        self.frmmgr.add_frame(buf.clone());
        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.frmmgr.clear();
    }
}

impl NAOptionHandler for CinepakDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(CinepakDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::generic_register_all_decoders;
    use crate::generic_register_all_demuxers;
    #[test]
    fn test_cinepak() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/CVID/ot171.avi
        test_decoding("avi", "cinepak", "assets/Misc/ot171.avi", Some(10), &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5Frames(vec![
                        [0xd58326b0, 0xdbfc1dcc, 0x6d66a04c, 0x08a21bbb],
                        [0x9b2cb5c5, 0x69b5f261, 0xcaccaaaf, 0xff2a807d],
                        [0x55c322d5, 0xf76f81ce, 0x923ada8c, 0x4925a5c8],
                        [0x2d1a537a, 0x62233cb6, 0xc1d39c2f, 0xeec9ccf3],
                        [0xf3cc841d, 0x56603c01, 0x34f521cf, 0x61f8a0c9],
                        [0xd75c0802, 0x9e786186, 0xc7a05cdf, 0x52ddc59d],
                        [0xde19733b, 0x29633d17, 0x507e9f82, 0x94c09158],
                        [0x1ea11919, 0x133a282c, 0x8cee485c, 0x150cb3f4],
                        [0x55a6d8fb, 0x2ea287c0, 0x36b3083b, 0x954cfc64],
                        [0xfb8be1fb, 0x84ad10aa, 0xa00ee55c, 0x9e191e5b],
                        [0x9c090a08, 0x43071726, 0x26236b5a, 0x79595848]]));
    }
    #[test]
    fn test_cinepak_gray() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/CVID/grayscale/dday.mov
        test_decoding("mov", "cinepak", "assets/Misc/dday.mov", Some(10), &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5Frames(vec![
                        [0x2ab229bc, 0xb71308aa, 0x979511c6, 0xcef3ea92],
                        [0x94f227d5, 0xbaa646ef, 0xab78f751, 0x8e1f50da],
                        [0x555de93a, 0x625e77f0, 0x95611bae, 0xbd715e9d],
                        [0xb31b9ba7, 0xba6327f8, 0x5698954f, 0xc16fad2a],
                        [0xda86ffb6, 0x58deb79d, 0x59f62c5b, 0x1bd2a2c5],
                        [0x2f46c7eb, 0x8950ac76, 0xbc68c470, 0x12e3247a],
                        [0x77d73950, 0xf76b28b0, 0x3552bb52, 0x38900a51],
                        [0xf4f45bef, 0x91146af2, 0xdcf4d44e, 0x713bf36e],
                        [0x8e06d350, 0x787f245e, 0x32426903, 0xf35f7dd3],
                        [0x0e35ebc1, 0xfdb6c520, 0x2bf484dc, 0xcec78b63],
                        [0xb8411fa4, 0x3a35f646, 0x85e8e04a, 0xfff58785]]));
    }
    #[test]
    fn test_cinepak_pal() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/CVID/palette/catfight%20Tag%20team%20DT.mov
        test_decoding("mov", "cinepak", "assets/Misc/catfight Tag team DT.mov", Some(10), &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5Frames(vec![
                        [0x3f7ec8ea, 0x873a2bc6, 0xcc58336e, 0xe88c4ffd],
                        [0x9665feab, 0xc035fb92, 0x5e4b8718, 0xd1c68877],
                        [0x804f8838, 0x7f4b126e, 0x9efab284, 0xee62d451],
                        [0xbb1930dd, 0x62d4a5d1, 0xca34d891, 0x31236269],
                        [0xc23ec739, 0xbe683ffd, 0xecbc337b, 0x73a96b63],
                        [0xa2fa75f2, 0x1dd937a8, 0x44e2074e, 0x1ac24467],
                        [0x9ba0f1e5, 0xadbe5357, 0x4cfa785b, 0x16181d41],
                        [0xe126c340, 0x6ceaac41, 0x64992bff, 0x8d4bc3c4],
                        [0xba6b2510, 0xc40c2b85, 0x1c7d0199, 0x333d4860],
                        [0x293fe1c2, 0x9f358a7e, 0x4fef6450, 0x8477a4ff],
                        [0x4509095a, 0x65575fdd, 0x3a17ecc4, 0x37821bf9]]));
    }
}

