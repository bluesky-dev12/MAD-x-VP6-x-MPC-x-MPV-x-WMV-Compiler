use nihav_core::codecs::*;
use nihav_core::compr::deflate::Inflate;

#[derive(Default)]
struct PicParams {
    width:  usize,
    height: usize,
    bw:     usize,
    bh:     usize,
    blk_w:  usize,
    blk_h:  usize,
    bpp:    usize,
    bits:   u8,
}

struct ZMBVDecoder {
    info:   NACodecInfoRef,
    comp:   u8,
    zbuf:   Vec<u8>,
    frm1:   Vec<u8>,
    frm2:   Vec<u8>,
    pparms: PicParams,
    pal:    [u8; 768],
    infl:   Inflate,
}

impl ZMBVDecoder {
    fn new() -> Self {
        Self {
            info:   NACodecInfo::new_dummy(),
            comp:   0,
            zbuf:   Vec::new(),
            frm1:   Vec::new(),
            frm2:   Vec::new(),
            pparms: PicParams::default(),
            pal:    [0; 768],
            infl:   Inflate::new(),
        }
    }
}

fn decode_intra(frm: &mut [u8], pal: &mut [u8; 768], pparms: &PicParams, src: &[u8]) -> DecoderResult<()> {
    let off = if pparms.bits == 8 {
            validate!(src.len() > 768);
            pal.copy_from_slice(&src[..768]);
            pal.len()
        } else { 0 };
    let src = &src[off..];
    let size = pparms.width * pparms.height * pparms.bpp;
    validate!(src.len() >= size);
    frm[..size].copy_from_slice(&src[..size]);
    Ok(())
}

fn decode_inter(frm: &mut [u8], prev: &[u8], dpal: bool, pal: &mut [u8; 768], pparms: &PicParams, src: &[u8]) -> DecoderResult<()> {
    let off = if pparms.bits == 8 && dpal {
            validate!(src.len() > 768);
            for (dst, &src) in pal.iter_mut().zip(src.iter()) {
                *dst ^= src;
            }
            pal.len()
        } else { 0 };
    let mv_len = (pparms.blk_w * pparms.blk_h * 2 + 3) & !3;
    validate!(src.len() >= off + mv_len);
    let mut mvs = src[off..][..mv_len].chunks_exact(2);
    let mut src = &src[off + mv_len..];

    let mut last_bw = pparms.width % pparms.bw;
    if last_bw == 0 {
        last_bw = pparms.bw;
    }
    let mut last_bh = pparms.height % pparms.bh;
    if last_bh == 0 {
        last_bh = pparms.bh;
    }

    let stride = pparms.width * pparms.bpp;
    let mut off = 0;
    let mut cur_h = pparms.bh;
    for y in (0..pparms.height).step_by(pparms.bh) {
        if y + pparms.bh >= pparms.height {
            cur_h = last_bh;
        }
        let mut cur_w = pparms.bw;
        let mut block_w = cur_w * pparms.bpp;
        for x in (0..pparms.width).step_by(pparms.bw) {
            if x + pparms.bw >= pparms.width {
                cur_w = last_bw;
                block_w = cur_w * pparms.bpp;
            }

            let mv = mvs.next().unwrap_or(&[0; 2]);
            let has_delta = (mv[0] & 1) != 0;
            let mv_x = (mv[0] as i8) >> 1;
            let mv_y = (mv[1] as i8) >> 1;

            let xoff = (x as isize) + (mv_x as isize);
            let yoff = (y as isize) + (mv_y as isize);
            if xoff >= 0 && (xoff as usize) + cur_w <= pparms.width && yoff >= 0 && (yoff as usize) + cur_h <= pparms.height {
                let src_off = (xoff as usize) * pparms.bpp + (yoff as usize) * stride;

                for (dline, sline) in frm[off..].chunks_mut(stride).zip(prev[src_off..].chunks(stride)).take(cur_h) {
                    dline[..block_w].copy_from_slice(&sline[..block_w]);
                }
            } else {
                let mut doff = off;
                let mut soff = xoff * (pparms.bpp as isize) + yoff * (stride as isize);
                for j in 0..cur_h {
                    let cy = yoff + (j as isize);
                    if cy >= 0 && (cy as usize) < pparms.height {
                        for i in 0..cur_w {
                            let cx = xoff + (i as isize);
                            if cx >= 0 && (cx as usize) < pparms.width {
                                for k in 0..pparms.bpp {
                                    frm[doff + i * pparms.bpp + k] = prev[(soff + ((i * pparms.bpp + k) as isize)) as usize]
                                }
                            } else {
                                for k in 0..pparms.bpp {
                                    frm[doff + i * pparms.bpp + k] = 0;
                                }
                            }
                        }
                    } else {
                        for p in frm[doff..][..block_w].iter_mut() {
                            *p = 0;
                        }
                    }
                    doff += stride;
                    soff += stride as isize;
                }
            }
            if has_delta {
                validate!(src.len() >= block_w * cur_h);
                for (dline, sline) in frm[off..].chunks_mut(stride).zip(src.chunks(block_w)).take(cur_h) {
                    for (dst, &src) in dline[..block_w].iter_mut().zip(sline.iter()) {
                        *dst ^= src;
                    }
                }
                src = &src[block_w * cur_h..];
            }

            off += pparms.bw * pparms.bpp;
        }
        off -= pparms.bw * pparms.blk_w * pparms.bpp;
        off += pparms.bh * stride;
    }

    Ok(())
}

const INTRA_FLAG: u8 = 0x01;
const DELTA_PAL:  u8 = 0x02;

const RGB555_FORMAT: NAPixelFormaton = NAPixelFormaton { model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
                                        comp_info: [
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift: 10, comp_offs: 0, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  5, comp_offs: 0, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  0, comp_offs: 0, next_elem: 2 }),
                                            None, None],
                                        elem_size: 2, be: false, alpha: false, palette: false };
const RGB24_0_FORMAT: NAPixelFormaton = NAPixelFormaton { model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
                                        comp_info: [
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 2, next_elem: 4 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 1, next_elem: 4 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 0, next_elem: 4 }),
                                            None, None],
                                        elem_size: 4, be: false, alpha: false, palette: false };

impl NADecoder for ZMBVDecoder {
    #[allow(clippy::or_fun_call)]
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.pparms.width  = vinfo.get_width();
            self.pparms.height = vinfo.get_height();
            self.zbuf   = vec![0; (self.pparms.width + 255) * (self.pparms.height + 64) * 4];
            self.frm1   = vec![0; self.pparms.width * self.pparms.height * 4];
            self.frm2   = vec![0; self.pparms.width * self.pparms.height * 4];
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        if src.is_empty() { return Err(DecoderError::ShortData); }

        let flags = src[0];
        let keyframe = (flags & INTRA_FLAG) != 0;

        if keyframe {
            validate!(src.len() > 7);
            let hi_ver      = src[1];
            let lo_ver      = src[2];
            let comp        = src[3];
            let fmt         = src[4];
            let bw          = src[5];
            let bh          = src[6];
            validate!(hi_ver == 0 && lo_ver == 1);
            validate!(comp == 0 || comp == 1);
            validate!(bw > 0 && bh > 0);
            self.comp = comp;
            self.pparms.bw = bw as usize;
            self.pparms.bh = bh as usize;
            let (bits, fmt) = match fmt {
                    0   => return Err(DecoderError::NotImplemented), //0,
                    1   => return Err(DecoderError::NotImplemented), //1,
                    2   => return Err(DecoderError::NotImplemented), //2,
                    3   => return Err(DecoderError::NotImplemented), //4,
                    4   => (8, PAL8_FORMAT),
                    5   => (15, RGB555_FORMAT),
                    6   => (16, RGB565_FORMAT),
                    7   => (24, RGB24_FORMAT),
                    8   => (32, RGB24_0_FORMAT),
                    _ => return Err(DecoderError::NotImplemented),
                };
            self.pparms.blk_w = (self.pparms.width + self.pparms.bw - 1) / self.pparms.bw;
            self.pparms.blk_h = (self.pparms.height + self.pparms.bh - 1) / self.pparms.bh;
            if self.pparms.bits != bits {
                self.pparms.bits = bits;
                self.pparms.bpp = ((bits + 7) / 8) as usize;
                let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.pparms.width, self.pparms.height, false, fmt));
                self.info = NACodecInfo::new_ref(self.info.get_name(), myinfo, self.info.get_extradata()).into_ref();
            }
            self.infl.reset();
        } else if self.pparms.bits == 0 {
            return Err(DecoderError::MissingReference);
        }

        let off = if keyframe { 7 } else { 1 };
        let src = match self.comp {
                0 => &src[off..],
                1 => {
                    let ret = self.infl.decompress_block(&src[off..], &mut self.zbuf);
                    if ret.is_err() {
                        return Err(DecoderError::InvalidData);
                    }
                    let len = ret.unwrap();
                    &self.zbuf[..len]
                },
                _ => unreachable!(),
            };
        if keyframe {
            decode_intra(&mut self.frm1, &mut self.pal, &self.pparms, src)?;
        } else {
            decode_inter(&mut self.frm1, &self.frm2, (flags & DELTA_PAL) != 0, &mut self.pal, &self.pparms, src)?;
        }

        let vinfo = self.info.get_properties().get_video_info().unwrap();
        let mut bufinfo = alloc_video_buffer(vinfo, 0)?;
        match (self.pparms.bits, &mut bufinfo) {
            (8, NABufferType::Video(ref mut buf)) => {
                let stride = buf.get_stride(0);
                let offset = buf.get_offset(0);
                let paloff = buf.get_offset(1);
                let data = buf.get_data_mut().unwrap();
                data[paloff..][..768].copy_from_slice(&self.pal);
                for (dline, sline) in data[offset..].chunks_mut(stride).zip(self.frm1.chunks_exact(self.pparms.width)).take(self.pparms.height) {
                    dline[..self.pparms.width].copy_from_slice(sline);
                }
            },
            (_, NABufferType::Video16(ref mut buf)) => {
                let stride = buf.get_stride(0);
                let offset = buf.get_offset(0);
                let data = buf.get_data_mut().unwrap();
                for (dline, sline) in data[offset..].chunks_mut(stride).zip(self.frm1.chunks_exact(self.pparms.width * 2)).take(self.pparms.height) {
                    for (dst, src) in dline[..self.pparms.width].iter_mut().zip(sline.chunks_exact(2)) {
                        *dst = u16::from(src[0]) | (u16::from(src[1]) << 8);
                    }
                }
            },
            (24, NABufferType::VideoPacked(ref mut buf)) => {
                let stride = buf.get_stride(0);
                let offset = buf.get_offset(0);
                let data = buf.get_data_mut().unwrap();
                for (dline, sline) in data[offset..].chunks_mut(stride).zip(self.frm1.chunks_exact(self.pparms.width * 3)).take(self.pparms.height) {
                    dline[..self.pparms.width * 3].copy_from_slice(sline);
                }
            },
            (32, NABufferType::VideoPacked(ref mut buf)) => {
                let stride = buf.get_stride(0);
                let offset = buf.get_offset(0);
                let data = buf.get_data_mut().unwrap();
                for (dline, sline) in data[offset..].chunks_mut(stride).zip(self.frm1.chunks_exact(self.pparms.width * 4)).take(self.pparms.height) {
                    dline[..self.pparms.width * 4].copy_from_slice(sline);
                }
            },
            _ => return Err(DecoderError::Bug),
        };

        std::mem::swap(&mut self.frm1, &mut self.frm2);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(keyframe);
        if keyframe {
            frm.set_frame_type(FrameType::I);
        } else {
            frm.set_frame_type(FrameType::P);
        }
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.pparms.bits = 0;
    }
}

impl NAOptionHandler for ZMBVDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(ZMBVDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::generic_register_all_decoders;
    use crate::generic_register_all_demuxers;
    // samples are from https://samples.mplayerhq.hu/V-codecs/ZMBV/
    #[test]
    fn test_zmbv_8() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        test_decoding("avi", "zmbv", "assets/Misc/td3_000.avi", Some(10),
                     &dmx_reg, &dec_reg, ExpectedTestResult::MD5([0x83c57ac3, 0xda325d18, 0x806bd3be, 0x4b108732]));
    }
    #[test]
    fn test_zmbv_15() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        test_decoding("avi", "zmbv", "assets/Misc/zmbv_15bit.avi", Some(20),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5([0x9c9d3544, 0x11b437b6, 0x97a47a98, 0xeafb8ec9]));
    }
    #[test]
    fn test_zmbv_16() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        test_decoding("avi", "zmbv", "assets/Misc/zmbv_16bit.avi", Some(20),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5([0x5c09564e, 0x000a07c7, 0xf0d8a0d4, 0xa4ef77e6]));
    }
    #[test]
    fn test_zmbv_32() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        test_decoding("avi", "zmbv", "assets/Misc/zmbv_32bit.avi", Some(20),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5([0x4ee7b80b, 0xdba2253c, 0x39721ddf, 0x46ed6d53]));
    }
}
