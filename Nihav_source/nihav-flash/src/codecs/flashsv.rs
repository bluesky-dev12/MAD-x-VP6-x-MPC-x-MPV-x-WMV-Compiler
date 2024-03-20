use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::compr::deflate::*;

#[derive(Default)]
struct FSVShuffler {
    lastframe: Option<NAVideoBufferRef<u8>>,
    keyframe:  Option<NAVideoBufferRef<u8>>,
}

#[allow(dead_code)]
impl FSVShuffler {
    fn new() -> Self { Self::default() }
    fn clear(&mut self) {
        self.keyframe  = None;
        self.lastframe = None;
    }
    fn add_frame(&mut self, buf: NAVideoBufferRef<u8>) {
        self.lastframe = Some(buf);
    }
    fn add_keyframe(&mut self, buf: NAVideoBufferRef<u8>) {
        self.keyframe = Some(buf);
    }
    fn clone_ref(&mut self) -> Option<NAVideoBufferRef<u8>> {
        if let Some(ref mut frm) = self.lastframe {
            let newfrm = frm.copy_buffer();
            *frm = newfrm.clone().into_ref();
            Some(newfrm.into_ref())
        } else {
            None
        }
    }
    fn has_last_frame(&self) -> bool { self.lastframe.is_some() }
    fn get_key_frame(&mut self) -> Option<NAVideoBufferRef<u8>> {
        self.keyframe.as_ref().cloned()
    }
    fn get_last_frame(&mut self) -> Option<NAVideoBufferRef<u8>> {
        self.lastframe.as_ref().cloned()
    }
}


struct FSVDecoder {
    info:       NACodecInfoRef,
    shuf:       FSVShuffler,
    w:          usize,
    h:          usize,
    block_w:    usize,
    block_h:    usize,
    ver1:       bool,
    has_pal:    bool,
    has_ifrm:   bool,
    tile:       Vec<u8>,
    cbuf:       [u8; 65536],
    pal:        [u8; 128 * 3],
    inflate:    Inflate,
    kdata:      Vec<u8>,
    bpos:       Vec<usize>,
    bsize:      Vec<usize>,
}

impl FSVDecoder {
    fn new(ver1: bool) -> Self {
        Self {
            info:       NACodecInfo::new_dummy(),
            shuf:       FSVShuffler::new(),
            w:          0,
            h:          0,
            block_w:    0,
            block_h:    0,
            ver1,
            has_pal:    false,
            has_ifrm:   false,
            tile:       Vec::new(),
            cbuf:       [0; 65536],
            pal:        DEFAULT_PAL,
            inflate:    Inflate::new(),
            kdata:      Vec::new(),
            bpos:       Vec::new(),
            bsize:      Vec::new(),
        }
    }
    fn decode_v1(&mut self, br: &mut ByteReader, data: &mut [u8], stride: usize) -> DecoderResult<bool> {
        let mut is_intra = true;
        for (yy, row) in data.chunks_mut(stride * self.block_h).enumerate() {
            let cur_h = (self.h - yy * self.block_h).min(self.block_h);
            for x in (0..self.w).step_by(self.block_w) {
                let cur_w = (self.w - x).min(self.block_w);

                let data_size           = br.read_u16be()? as usize;
                if data_size > 0 {
                                          br.read_buf(&mut self.cbuf[..data_size])?;
                    self.inflate = Inflate::new();
                    if self.inflate.decompress_block(&self.cbuf[..data_size], &mut self.tile[..cur_w * cur_h * 3]).is_err() {
                        return Err(DecoderError::InvalidData);
                    }
                    for (dst, src) in row[x * 3..].chunks_mut(stride).zip(self.tile.chunks(cur_w * 3)) {
                        dst[..cur_w * 3].copy_from_slice(src);
                    }
                } else {
                    is_intra = false;
                }
            }
        }
        Ok(is_intra)
    }
    fn decode_v2(&mut self, br: &mut ByteReader, data: &mut [u8], stride: usize, keyframe: bool) -> DecoderResult<bool> {
        let mut is_intra = !self.has_ifrm;
        let bstride = (self.w + self.block_w - 1) / self.block_w;
        for y in (0..self.h).step_by(self.block_h) {
            let cur_h = (self.h - y).min(self.block_h);
            for x in (0..self.w).step_by(self.block_w) {
                let cur_w = (self.w - x).min(self.block_w);

                let mut data_size       = br.read_u16be()? as usize;
                validate!(!keyframe || data_size > 0);
                if data_size == 0 {
                    is_intra = false;
                    continue;
                }
                let blk_start = br.tell();
                let flags               = br.read_byte()?;
                let depth = (flags >> 3) & 3;
                validate!(depth == 0 || depth == 2);
                let has_diff = (flags & 4) != 0;
                let cpriming = (flags & 2) != 0;
                let ppriming = (flags & 1) != 0;
                let (start, height) = if has_diff {
                        let start       = br.read_byte()? as usize;
                        let height      = br.read_byte()? as usize;
                        validate!(start + height <= cur_h);
                        (start, height)
                    } else {
                        (0, cur_h)
                    };
                if has_diff {
                    let ret = self.shuf.get_key_frame();
                    if ret.is_none() {
                        return Err(DecoderError::MissingReference);
                    }
                    let src = ret.unwrap();
                    let src = src.get_data();
                    for (dst, src) in data[x * 3 + y * stride..].chunks_mut(stride).take(cur_h).zip(src[x * 3 + y * stride..].chunks(stride)) {
                        dst[..cur_w * 3].copy_from_slice(&src[..cur_w * 3]);
                    }
                }
                if height != cur_h {
                    is_intra = false;
                }
                let ppos = if cpriming {
                        let xpos        = br.read_byte()? as usize;
                        let ypos        = br.read_byte()? as usize;
                        xpos + ypos * bstride
                    } else {
                        x / self.block_w + y / self.block_h * bstride
                    };
                data_size -= (br.tell() - blk_start) as usize;
                if keyframe {
                    self.bpos.push(br.tell() as usize);
                    self.bsize.push(data_size);
                }
                if data_size > 0 {
                                          br.read_buf(&mut self.cbuf[..data_size])?;
                    self.inflate = Inflate::new();
                    if cpriming || ppriming {
                        if self.bpos.is_empty() {
                            return Err(DecoderError::MissingReference);
                        }
                        let ret = self.inflate.decompress_block(&self.kdata[self.bpos[ppos]..][..self.bsize[ppos]], &mut self.tile);
                        if ret.is_err() {
                            return Err(DecoderError::InvalidData);
                        }
                        let ssize = ret.unwrap();
                        self.inflate = Inflate::new();
                        self.inflate.set_dict(&self.tile[..ssize]);
                    }
                    let ret = self.inflate.decompress_block(&self.cbuf[..data_size], &mut self.tile[..cur_w * height * 3]);
                    if ret.is_err() {
                        return Err(DecoderError::InvalidData);
                    }
                    let src_len = ret.unwrap();

                    let dst = &mut data[x * 3 + y * stride..];
                    match depth {
                        0 => {
                            validate!(src_len == cur_w * cur_h * 3);
                            for (dst, src) in dst.chunks_mut(stride).skip(start).take(height).zip(self.tile.chunks(cur_w * 3)) {
                                dst[..cur_w * 3].copy_from_slice(src);
                            }
                        },
                        2 => {
                            let mut mr = MemoryReader::new_read(&self.tile[..src_len]);
                            let mut br = ByteReader::new(&mut mr);
                            for line in dst.chunks_mut(stride).skip(start).take(height) {
                                for rgb in line.chunks_mut(3).take(cur_w) {
                                    let b           = br.read_byte()?;
                                    if (b & 0x80) == 0 {
                                        rgb.copy_from_slice(&self.pal[(b as usize) * 3..][..3]);
                                    } else {
                                        let c       = br.read_byte()?;
                                        let clr = (u16::from(b & 0x7F) << 8) | u16::from(c);
                                        let r = (clr >> 10) as u8;
                                        let g = ((clr >> 5) & 0x1F) as u8;
                                        let b = (clr & 0x1F) as u8;
                                        rgb[0] = (r << 3) | (r >> 2);
                                        rgb[1] = (g << 3) | (g >> 2);
                                        rgb[2] = (b << 3) | (b >> 2);
                                    }
                                }
                            }
                        },
                        _ => unreachable!(),
                    };
                } else {
                    is_intra = false;
                }
            }
        }
        if self.has_ifrm {
unimplemented!();
        }
        Ok(is_intra)
    }
}

impl NADecoder for FSVDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, true, BGR24_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        validate!(src.len() > 4);
        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);

        let hdr0                        = br.read_u16be()? as usize;
        let hdr1                        = br.read_u16be()? as usize;
        let w = hdr0 & 0xFFF;
        let h = hdr1 & 0xFFF;
        let blk_w = (hdr0 >> 12) * 16 + 16;
        let blk_h = (hdr1 >> 12) * 16 + 16;
        validate!(w != 0 && h != 0 && blk_w != 0 && blk_h != 0);

        if !self.ver1 {
            let flags                   = br.read_byte()?;
            self.has_pal  = (flags & 1) != 0;
            self.has_ifrm = (flags & 2) != 0;
            if self.has_pal {
                let pal_sz              = br.read_u16be()? as usize;
                                          br.read_buf(&mut self.cbuf[..pal_sz])?;
                self.inflate = Inflate::new();
                if self.inflate.decompress_block(&self.cbuf[..pal_sz], &mut self.pal).is_err() {
                    return Err(DecoderError::InvalidData);
                }
            }
            if pkt.keyframe {
                self.kdata.clear();
                self.kdata.extend_from_slice(&src);
                self.bpos.clear();
                self.bsize.clear();
            }
        }
        if self.w != w || self.h != h || self.block_w != blk_w || self.block_h != blk_h {
            self.flush();
            self.tile.resize(blk_w * blk_h * 3, 0);
            self.w = w;
            self.h = h;
            self.block_w = blk_w;
            self.block_h = blk_h;
        }

        let mut buf = if let Some(buffer) = self.shuf.clone_ref() {
                buffer
            } else {
                let vinfo = self.info.get_properties().get_video_info().unwrap();
                let bufinfo = alloc_video_buffer(vinfo, 0)?;
                bufinfo.get_vbuf().unwrap()
            };
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let is_intra = if self.ver1 {
                self.decode_v1(&mut br, data, stride)?
            } else {
                self.decode_v2(&mut br, data, stride, pkt.keyframe)?
            };

        if !is_intra && !self.shuf.has_last_frame() {
            return Err(DecoderError::MissingReference);
        }

        if pkt.is_keyframe() {
            self.shuf.add_keyframe(buf.clone());
        }
        self.shuf.add_frame(buf.clone());

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::VideoPacked(buf));
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.shuf.clear();
    }
}

impl NAOptionHandler for FSVDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(FSVDecoder::new(true))
}

pub fn get_decoder_v2() -> Box<dyn NADecoder + Send> {
    Box::new(FSVDecoder::new(false))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::flash_register_all_decoders;
    use crate::flash_register_all_demuxers;
    #[test]
    fn test_flashsv1() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        flash_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/FLV/flash_screen/screen.flv
        test_decoding("flv", "flashsv", "assets/Flash/screen.flv",
                      Some(3000), &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0xb45b899e, 0x417b17d5, 0x7bfe898b, 0x026b289f],
                            [0xb45b899e, 0x417b17d5, 0x7bfe898b, 0x026b289f],
                            [0xb45b899e, 0x417b17d5, 0x7bfe898b, 0x026b289f],
                            [0xb45b899e, 0x417b17d5, 0x7bfe898b, 0x026b289f],
                            [0xb45b899e, 0x417b17d5, 0x7bfe898b, 0x026b289f],
                            [0xb45b899e, 0x417b17d5, 0x7bfe898b, 0x026b289f],
                            [0xb45b899e, 0x417b17d5, 0x7bfe898b, 0x026b289f],
                            [0xb45b899e, 0x417b17d5, 0x7bfe898b, 0x026b289f],
                            [0xc04d4d1c, 0xbb1f4b4f, 0xe9f3d85e, 0xa40aff68],
                            [0x172e5bbe, 0xe44caba3, 0x6cb2a263, 0xcb79a89a]]));
    }
    #[test]
    fn test_flashsv2() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        flash_register_all_decoders(&mut dec_reg);

        // sample created from https://samples.mplayerhq.hu/FLV/flash_screen/screen.flv by recoding
        test_decoding("flv", "flashsv2", "assets/Flash/screen2.flv",
                      Some(4700), &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x55522afa, 0x9c7dd794, 0xdd67aa2e, 0x8b8c525e],
                            [0x55522afa, 0x9c7dd794, 0xdd67aa2e, 0x8b8c525e],
                            [0x55522afa, 0x9c7dd794, 0xdd67aa2e, 0x8b8c525e],
                            [0x55522afa, 0x9c7dd794, 0xdd67aa2e, 0x8b8c525e],
                            [0x55522afa, 0x9c7dd794, 0xdd67aa2e, 0x8b8c525e],
                            [0x55522afa, 0x9c7dd794, 0xdd67aa2e, 0x8b8c525e],
                            [0x55522afa, 0x9c7dd794, 0xdd67aa2e, 0x8b8c525e],
                            [0x55522afa, 0x9c7dd794, 0xdd67aa2e, 0x8b8c525e],
                            [0x9809efc2, 0xec5385aa, 0xb5eb9320, 0x4a47188e],
                            [0x40c77877, 0x58183722, 0x5700eb17, 0x27a00e33],
                            [0x802c2c6a, 0x3e08dd62, 0xa6c94df3, 0xc6318a6f],
                            [0x2aa70255, 0x652f0ca4, 0xe79817f9, 0x4f67e7ba],
                            [0x5cf34d91, 0xdfc54992, 0x4368180d, 0xfbe747d4],
                            [0x266d8bc4, 0x2b492ef4, 0xb42401a0, 0x23e530ec],
                            [0xa0e46b1c, 0x47d0620e, 0x0cbcb15b, 0x243e7f13]]));
    }
}

const DEFAULT_PAL: [u8; 128 * 3] = [
    0x00, 0x00, 0x00, 0x33, 0x33, 0x33, 0x66, 0x66, 0x66, 0x99, 0x99, 0x99,
    0xCC, 0xCC, 0xCC, 0xFF, 0xFF, 0xFF, 0x33, 0x00, 0x00, 0x66, 0x00, 0x00,
    0x99, 0x00, 0x00, 0xCC, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, 0x33, 0x00,
    0x00, 0x66, 0x00, 0x00, 0x99, 0x00, 0x00, 0xCC, 0x00, 0x00, 0xFF, 0x00,
    0x00, 0x00, 0x33, 0x00, 0x00, 0x66, 0x00, 0x00, 0x99, 0x00, 0x00, 0xCC,
    0x00, 0x00, 0xFF, 0x33, 0x33, 0x00, 0x66, 0x66, 0x00, 0x99, 0x99, 0x00,
    0xCC, 0xCC, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0x33, 0x33, 0x00, 0x66, 0x66,
    0x00, 0x99, 0x99, 0x00, 0xCC, 0xCC, 0x00, 0xFF, 0xFF, 0x33, 0x00, 0x33,
    0x66, 0x00, 0x66, 0x99, 0x00, 0x99, 0xCC, 0x00, 0xCC, 0xFF, 0x00, 0xFF,
    0xFF, 0xFF, 0x33, 0xFF, 0xFF, 0x66, 0xFF, 0xFF, 0x99, 0xFF, 0xFF, 0xCC,
    0xFF, 0x33, 0xFF, 0xFF, 0x66, 0xFF, 0xFF, 0x99, 0xFF, 0xFF, 0xCC, 0xFF,
    0x33, 0xFF, 0xFF, 0x66, 0xFF, 0xFF, 0x99, 0xFF, 0xFF, 0xCC, 0xFF, 0xFF,
    0xCC, 0xCC, 0x33, 0xCC, 0xCC, 0x66, 0xCC, 0xCC, 0x99, 0xCC, 0xCC, 0xFF,
    0xCC, 0x33, 0xCC, 0xCC, 0x66, 0xCC, 0xCC, 0x99, 0xCC, 0xCC, 0xFF, 0xCC,
    0x33, 0xCC, 0xCC, 0x66, 0xCC, 0xCC, 0x99, 0xCC, 0xCC, 0xFF, 0xCC, 0xCC,
    0x99, 0x99, 0x33, 0x99, 0x99, 0x66, 0x99, 0x99, 0xCC, 0x99, 0x99, 0xFF,
    0x99, 0x33, 0x99, 0x99, 0x66, 0x99, 0x99, 0xCC, 0x99, 0x99, 0xFF, 0x99,
    0x33, 0x99, 0x99, 0x66, 0x99, 0x99, 0xCC, 0x99, 0x99, 0xFF, 0x99, 0x99,
    0x66, 0x66, 0x33, 0x66, 0x66, 0x99, 0x66, 0x66, 0xCC, 0x66, 0x66, 0xFF,
    0x66, 0x33, 0x66, 0x66, 0x99, 0x66, 0x66, 0xCC, 0x66, 0x66, 0xFF, 0x66,
    0x33, 0x66, 0x66, 0x99, 0x66, 0x66, 0xCC, 0x66, 0x66, 0xFF, 0x66, 0x66,
    0x33, 0x33, 0x66, 0x33, 0x33, 0x99, 0x33, 0x33, 0xCC, 0x33, 0x33, 0xFF,
    0x33, 0x66, 0x33, 0x33, 0x99, 0x33, 0x33, 0xCC, 0x33, 0x33, 0xFF, 0x33,
    0x66, 0x33, 0x33, 0x99, 0x33, 0x33, 0xCC, 0x33, 0x33, 0xFF, 0x33, 0x33,
    0x00, 0x33, 0x66, 0x33, 0x66, 0x00, 0x66, 0x00, 0x33, 0x00, 0x66, 0x33,
    0x33, 0x00, 0x66, 0x66, 0x33, 0x00, 0x33, 0x66, 0x99, 0x66, 0x99, 0x33,
    0x99, 0x33, 0x66, 0x33, 0x99, 0x66, 0x66, 0x33, 0x99, 0x99, 0x66, 0x33,
    0x66, 0x99, 0xCC, 0x99, 0xCC, 0x66, 0xCC, 0x66, 0x99, 0x66, 0xCC, 0x99,
    0x99, 0x66, 0xCC, 0xCC, 0x99, 0x66, 0x99, 0xCC, 0xFF, 0xCC, 0xFF, 0x99,
    0xFF, 0x99, 0xCC, 0x99, 0xFF, 0xCC, 0xCC, 0x99, 0xFF, 0xFF, 0xCC, 0x99,
    0x11, 0x11, 0x11, 0x22, 0x22, 0x22, 0x44, 0x44, 0x44, 0x55, 0x55, 0x55,
    0xAA, 0xAA, 0xAA, 0xBB, 0xBB, 0xBB, 0xDD, 0xDD, 0xDD, 0xEE, 0xEE, 0xEE
];

const BGR24_FORMAT: NAPixelFormaton = NAPixelFormaton {
        model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
        comp_info: [
            Some(NAPixelChromaton{
                h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 2, next_elem: 3 }),
            Some(NAPixelChromaton{
                h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 1, next_elem: 3 }),
            Some(NAPixelChromaton{
                h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 0, next_elem: 3 }),
            None, None],
        elem_size: 3, be: false, alpha: false, palette: false
};
