use nihav_core::codecs::*;
use nihav_core::compr::deflate::{Deflate, DeflateMode, DeflateWriter};
use nihav_core::io::byteio::*;

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

struct ZMBVEncoder {
    stream:     Option<NAStreamRef>,
    frm1:       Vec<u8>,
    frm2:       Vec<u8>,
    tmp_buf:    Vec<u8>,
    zbuf:       Vec<u8>,
    pal:        [u8; 768],
    pkt:        Option<NAPacket>,
    frmcount:   u8,
    key_int:    u8,
    tile_w:     usize,
    tile_h:     usize,
    cmode:      DeflateMode,
    compr:      Deflate,
    bpp:        u8,
    width:      usize,
    height:     usize,
    range:      usize,
    full_me:    bool,
    sent_pal:   bool,
}

fn buf_type_to_bpp(buf: &NABufferType) -> u8 {
    match buf {
        NABufferType::Video(ref vbuf) => {
            let vinfo = vbuf.get_info();
            if vinfo.get_format().is_paletted() {
                8
            } else {
                vinfo.get_format().get_total_depth()
            }
        },
        NABufferType::VideoPacked(ref vbuf) => {
            let vinfo = vbuf.get_info();
            vinfo.get_format().elem_size * 8
        },
        NABufferType::Video16(ref vbuf) => {
            let vinfo = vbuf.get_info();
            vinfo.get_format().get_total_depth()
        },
        _ => 0,
    }
}

fn copy_frame(buf: NABufferType, dst: &mut [u8], bpp: u8) -> EncoderResult<()> {
    match buf {
        NABufferType::Video(ref vbuf) => {
            if bpp != 8 {
                return Err(EncoderError::FormatError);
            }
            let off     = vbuf.get_offset(0);
            let stride  = vbuf.get_stride(0);
            let data    = vbuf.get_data();
            let w       = vbuf.get_info().get_width();
            let h       = vbuf.get_info().get_height();

            for (dline, sline) in dst.chunks_mut(w).zip(data[off..].chunks(stride)).take(h) {
                dline[..w].copy_from_slice(&sline[..w]);
            }
        },
        NABufferType::Video16(ref vbuf) => {
            let off     = vbuf.get_offset(0);
            let stride  = vbuf.get_stride(0);
            let data    = vbuf.get_data();
            let w       = vbuf.get_info().get_width();
            let h       = vbuf.get_info().get_height();

            for (dline, sline) in dst.chunks_mut(w * 2).zip(data[off..].chunks(stride)).take(h) {
                for (dst, &src) in dline[..w * 2].chunks_exact_mut(2).zip(sline.iter()) {
                    dst[0] = src as u8;
                    dst[1] = (src >> 8) as u8;
                }
            }
        },
        NABufferType::VideoPacked(ref vbuf) => {
            let off     = vbuf.get_offset(0);
            let stride  = vbuf.get_stride(0);
            let data    = vbuf.get_data();
            let w       = vbuf.get_info().get_width();
            let h       = vbuf.get_info().get_height();
            let w = w * (((bpp as usize) + 7) / 8);

            for (dline, sline) in dst.chunks_mut(w).zip(data[off..].chunks(stride)).take(h) {
                dline[..w].copy_from_slice(&sline[..w]);
            }
        },
        _ => return Err(EncoderError::FormatError),
    };
    Ok(())
}

fn to_signed(val: usize) -> isize {
    if (val & 1) == 0 {
        (val >> 1) as isize
    } else {
        -((val >> 1) as isize) - 1
    }
}

impl ZMBVEncoder {
    fn new() -> Self {
        Self {
            stream:     None,
            pkt:        None,
            frm1:       Vec::new(),
            frm2:       Vec::new(),
            pal:        [0; 768],
            tmp_buf:    Vec::new(),
            zbuf:       Vec::new(),
            frmcount:   0,
            key_int:    25,
            tile_w:     16,
            tile_h:     16,
            cmode:      DeflateMode::default(),
            compr:      Deflate::new(DeflateMode::default()),
            bpp:        0,
            width:      0,
            height:     0,
            range:      128,
            full_me:    false,
            sent_pal:   false,
        }
    }
    fn encode_intra(&mut self, bw: &mut ByteWriter, buf: NABufferType) -> EncoderResult<()> {
        let mut bpp = buf_type_to_bpp(&buf);

        if let NABufferType::None = buf {
            if self.bpp == 0 {
                return Err(EncoderError::FormatError);
            }
            self.frm1.copy_from_slice(&self.frm2);
            bpp = self.bpp;
        } else {
            if bpp == 0 {
                return Err(EncoderError::FormatError);
            }
            self.bpp = bpp;

            if let (NABufferType::Video(ref vbuf), true) = (&buf, bpp == 8) {
                let off = vbuf.get_offset(1);
                let data = vbuf.get_data();
                self.pal.copy_from_slice(&data[off..][..768]);
            }

            copy_frame(buf, &mut self.frm1, self.bpp)?;
        }

        bw.write_byte(1)?; // intra flag
        bw.write_byte(0)?; // high version
        bw.write_byte(1)?; // low version
        bw.write_byte(if self.cmode == DeflateMode::NoCompr { 0 } else { 1 })?;
        let fmt = match self.bpp {
                 8 => 4,
                15 => 5,
                16 => 6,
                24 => 7,
                32 => 8,
                 _ => unreachable!(),
            };
        bw.write_byte(fmt)?;
        bw.write_byte(self.tile_w as u8)?;
        bw.write_byte(self.tile_h as u8)?;

        let bm = ((bpp as usize) + 7) / 8;
        if self.cmode == DeflateMode::NoCompr {
            if bpp == 8 {
                bw.write_buf(&self.pal)?;
            }
            bw.write_buf(&self.frm1[..self.width * self.height * bm])?;
        } else {
            self.tmp_buf.clear();
            if bpp == 8 {
                self.tmp_buf.extend_from_slice(&self.pal);
            }
            self.tmp_buf.extend_from_slice(&self.frm1[..self.width * self.height * bm]);
            self.compr = Deflate::new(self.cmode);

            let mut db = Vec::new();
            std::mem::swap(&mut db, &mut self.zbuf);
            db.clear();
            let mut wr = DeflateWriter::new(db);
            self.compr.write_zlib_header(&mut wr);
            self.compr.compress(&self.tmp_buf, &mut wr);
            self.compr.compress_flush(&mut wr);
            let mut db = wr.end();
            std::mem::swap(&mut db, &mut self.zbuf);

            bw.write_buf(&self.zbuf)?;
        }

        Ok(())
    }
    fn encode_inter(&mut self, bw: &mut ByteWriter, buf: NABufferType) -> EncoderResult<()> {
        if let NABufferType::None = buf {
            self.frm1.copy_from_slice(&self.frm2);

            bw.write_byte(0)?;
            self.tmp_buf.clear();
            let tile_w = (self.width  + self.tile_w - 1) / self.tile_w;
            let tile_h = (self.height + self.tile_h - 1) / self.tile_h;
            let mv_size = (tile_w * tile_h * 2 + 3) & !3;
            for _ in 0..mv_size {
                self.tmp_buf.push(0);
            }
            if self.cmode == DeflateMode::NoCompr {
                bw.write_buf(&self.tmp_buf)?;
            } else {
                let mut db = Vec::new();

                std::mem::swap(&mut db, &mut self.zbuf);
                db.clear();
                let mut wr = DeflateWriter::new(db);
                self.compr.compress(&self.tmp_buf, &mut wr);
                self.compr.compress_flush(&mut wr);
                let mut db = wr.end();
                std::mem::swap(&mut db, &mut self.zbuf);

                bw.write_buf(&self.zbuf)?;
            }
            return Ok(());
        }
        let bpp = buf_type_to_bpp(&buf);
        if bpp == 0 || bpp != self.bpp {
            return Err(EncoderError::FormatError);
        }

        self.tmp_buf.clear();
        if let (NABufferType::Video(ref vbuf), true) = (&buf, bpp == 8) {
            let mut npal = [0; 768];
            let off = vbuf.get_offset(1);
            let data = vbuf.get_data();
            npal.copy_from_slice(&data[off..][..768]);
            let mut cmp = true;
            for (&a, &b) in self.pal.iter().zip(npal.iter()) {
                if a != b {
                    cmp = false;
                    break;
                }
            }
            if !cmp {
                for (&a, &b) in self.pal.iter().zip(npal.iter()) {
                    self.tmp_buf.push(a ^ b);
                }
                self.pal = npal;

                bw.write_byte(2)?;

                self.sent_pal = false;
            } else {
                bw.write_byte(0)?;
            }
        } else {
            bw.write_byte(0)?;
        }
        copy_frame(buf, &mut self.frm1, self.bpp)?;

        let tile_w = (self.width  + self.tile_w - 1) / self.tile_w;
        let tile_h = (self.height + self.tile_h - 1) / self.tile_h;
        let mut mv_start = self.tmp_buf.len();
        let mv_size = (tile_w * tile_h * 2 + 3) & !3;
        for _ in 0..mv_size {
            self.tmp_buf.push(0);
        }

        let bpp = ((self.bpp as usize) + 7) / 8;
        let stride = self.width * bpp;
        let mut off = 0;
        for y in (0..self.height).step_by(self.tile_h) {
            let cur_h = (self.height - y).min(self.tile_h);
            for x in (0..self.width).step_by(self.tile_w) {
                let cur_w = (self.width - x).min(self.tile_w);

                let (best_x, best_y, best_dist) = self.motion_search(&self.frm1[off..], x, y, cur_w, cur_h, bpp);
                let has_delta = best_dist != 0;
                self.tmp_buf[mv_start] = (best_x.wrapping_sub(x) << 1) as u8;
                if has_delta {
                    self.tmp_buf[mv_start] |= 1;
                }
                self.tmp_buf[mv_start + 1] = (best_y.wrapping_sub(y) << 1) as u8;
                mv_start += 2;
                if has_delta {
                    let rpos = best_x * bpp + best_y * stride;
                    for (line0, line1) in self.frm1[off..].chunks(stride).take(cur_h).zip(self.frm2[rpos..].chunks(stride)) {
                        for (&a, &b) in line0[..cur_w * bpp].iter().zip(line1[..cur_w * bpp].iter()) {
                            self.tmp_buf.push(a ^ b);
                        }
                    }
                }

                off += self.tile_w * bpp;
            }
            off -= tile_w * self.tile_w * bpp;
            off += stride * self.tile_h;
        }

        if self.cmode == DeflateMode::NoCompr {
            bw.write_buf(&self.tmp_buf)?;
        } else {
            let mut db = Vec::new();

            std::mem::swap(&mut db, &mut self.zbuf);
            db.clear();
            let mut wr = DeflateWriter::new(db);
            self.compr.compress(&self.tmp_buf, &mut wr);
            self.compr.compress_flush(&mut wr);
            let mut db = wr.end();
            std::mem::swap(&mut db, &mut self.zbuf);

            bw.write_buf(&self.zbuf)?;
        }

        Ok(())
    }
    fn calc_dist(&self, cur_frm: &[u8], xpos: usize, ypos: usize, cur_w: usize, cur_h: usize, bpp: usize) -> u32 {
        let stride = self.width * bpp;
        let mut diff = 0;
        let roff = xpos * bpp + ypos * stride;
        for (line0, line1) in cur_frm.chunks(stride).take(cur_h).zip(self.frm2[roff..].chunks(stride)) {
            for (&a, &b) in line0[..cur_w * bpp].iter().zip(line1[..cur_w * bpp].iter()) {
                diff += u32::from(a ^ b);
            }
        }
        diff
    }
    fn motion_search(&self, cur_frm: &[u8], x: usize, y: usize, cur_w: usize, cur_h: usize, bpp: usize) -> (usize, usize, u32) {
        let mut best_dist = self.calc_dist(cur_frm, x, y, cur_w, cur_h, bpp);
        if best_dist == 0 {
            return (x, y, 0);
        }
        let mut best_x = x;
        let mut best_y = y;

        if !self.full_me {
            let mut cur_range = self.range.min(64);

            while cur_range > 1 {
                let x1 = best_x.saturating_sub(cur_range);
                let x2 = (best_x + cur_range).min(self.width - cur_w);
                let y1 = best_y.saturating_sub(cur_range);
                let y2 = (best_y + cur_range).min(self.height - cur_h);
                let points = [(best_x,  y1),
                              (x2,      y1),
                              (x2,      best_y),
                              (x2,      y2),
                              (best_x,  y2),
                              (x1,      y2),
                              (x1,      best_y),
                              (x1,      y1)];

                for &(pt_x, pt_y) in points.iter() {
                    if ((x as isize) - (pt_x as isize)).abs() >= 64 {
                        continue;
                    }
                    if ((y as isize) - (pt_y as isize)).abs() >= 64 {
                        continue;
                    }
                    let dist = self.calc_dist(cur_frm, pt_x, pt_y, cur_w, cur_h, bpp);
                    if dist < best_dist {
                        best_dist = dist;
                        best_x = pt_x;
                        best_y = pt_y;
                    }
                }
                cur_range = (cur_range + 1) >> 1;
            }
        } else {
            for yoff in 0..self.range {
                let ypos = (y as isize) + to_signed(yoff);
                if ypos < 0 {
                    continue;
                }
                let ypos = ypos as usize;
                if ypos + cur_h > self.height {
                    break;
                }
                for xoff in 0..self.range {
                    let xpos = (x as isize) + to_signed(xoff);
                    if xpos < 0 {
                        continue;
                    }
                    let xpos = xpos as usize;
                    if xpos + cur_w > self.width {
                        break;
                    }

                    let diff = self.calc_dist(cur_frm, xpos, ypos, cur_w, cur_h, bpp);

                    if best_dist > diff {
                        best_dist = diff;
                        best_x = xpos;
                        best_y = ypos;
                        if diff == 0 {
                            return (best_x, best_y, 0);
                        }
                    }
                }
            }
        }
        (best_x, best_y, best_dist)
    }
}

impl NAEncoder for ZMBVEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, true, YUV420_FORMAT)),
                    ..Default::default()
                })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let depth = vinfo.format.get_total_depth();
                let pix_fmt = if vinfo.format.is_paletted() {
                        PAL8_FORMAT
                    } else if !vinfo.format.model.is_rgb() || depth > 16 {
                        RGB24_0_FORMAT
                    } else if depth < 16 {
                        RGB555_FORMAT
                    } else {
                        RGB565_FORMAT
                    };
                let outinfo = NAVideoInfo::new(vinfo.width, vinfo.height, false, pix_fmt);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_SKIPFRAME }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                self.width  = vinfo.width;
                self.height = vinfo.height;

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, false, vinfo.format);
                let info = NACodecInfo::new("zmbv", NACodecTypeInfo::Video(out_info), None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());

                self.frm1 = vec![0; vinfo.width * vinfo.height * 4];
                self.frm2 = vec![0; vinfo.width * vinfo.height * 4];

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        let mut dbuf = Vec::with_capacity(4);
        let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
        let mut bw   = ByteWriter::new(&mut gw);
        let is_intra = if self.frmcount == 0 {
                self.encode_intra(&mut bw, buf)?;
                true
            } else {
                self.encode_inter(&mut bw, buf)?;
                false
            };
        self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, is_intra, dbuf));
        if self.bpp == 8 && !self.sent_pal {
            if let NABufferType::Video(ref buf) = frm.get_buffer() {
                let paloff = buf.get_offset(1);
                let data = buf.get_data();
                let mut pal = [0; 1024];
                let srcpal = &data[paloff..][..768];
                for (dclr, sclr) in pal.chunks_exact_mut(4).zip(srcpal.chunks_exact(3)) {
                    dclr[..3].copy_from_slice(sclr);
                }
                if let Some(ref mut pkt) = &mut self.pkt {
                    pkt.side_data.push(NASideData::Palette(true, Arc::new(pal)));
                }
            }
            self.sent_pal = true;
        }
        self.frmcount += 1;
        if self.frmcount == self.key_int {
            self.frmcount = 0;
        }
        std::mem::swap(&mut self.frm1, &mut self.frm2);
        Ok(())
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        let mut npkt = None;
        std::mem::swap(&mut self.pkt, &mut npkt);
        Ok(npkt)
    }
    fn flush(&mut self) -> EncoderResult<()> {
        self.frmcount = 0;
        Ok(())
    }
}

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: KEYFRAME_OPTION, description: KEYFRAME_OPTION_DESC,
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
    NAOptionDefinition {
        name: "range", description: "Block search range (0-128)",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
    NAOptionDefinition {
        name: "full_me", description: "Brute force search",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: "tile_width", description: "Block width (1-255)",
        opt_type: NAOptionDefinitionType::Int(Some(1), Some(255)) },
    NAOptionDefinition {
        name: "tile_height", description: "Block width (1-255)",
        opt_type: NAOptionDefinitionType::Int(Some(1), Some(255)) },
    NAOptionDefinition {
        name: "compr_level", description: "Compression level",
        opt_type: DEFLATE_OPTION_VALUES },
];

impl NAOptionHandler for ZMBVEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        "compr_level" => {
                            if let NAValue::String(ref s) = option.value {
                                if let Ok(val) = s.parse::<DeflateMode>() {
                                    self.cmode = val;
                                }
                            }
                        },
                        KEYFRAME_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.key_int = intval as u8;
                            }
                        },
                        "range" => {
                            if let NAValue::Int(intval) = option.value {
                                self.range = intval as usize;
                            }
                        },
                        "full_me" => {
                            if let NAValue::Bool(bval) = option.value {
                                self.full_me = bval;
                            }
                        },
                        "tile_width" => {
                            if let NAValue::Int(intval) = option.value {
                                self.tile_w = intval as usize;
                            }
                        },
                        "tile_height" => {
                            if let NAValue::Int(intval) = option.value {
                                self.tile_h = intval as usize;
                            }
                        },
                        _ => {},
                    };
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            "compr_level" => Some(NAValue::String(self.cmode.to_string())),
            KEYFRAME_OPTION => Some(NAValue::Int(i64::from(self.key_int))),
            "range"       => Some(NAValue::Int(self.range as i64)),
            "full_me"     => Some(NAValue::Bool(self.full_me)),
            "tile_width"  => Some(NAValue::Int(self.tile_w as i64)),
            "tile_height" => Some(NAValue::Int(self.tile_h as i64)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(ZMBVEncoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_codec_support::test::enc_video::*;
    use super::{RGB555_FORMAT, RGB24_0_FORMAT};

    // samples are from https://samples.mplayerhq.hu/V-codecs/ZMBV/
    #[test]
    fn test_zmbv_encoder_8bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        generic_register_all_encoders(&mut enc_reg);

        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Misc/td3_000.avi",
                stream_type:    StreamType::Video,
                limit:          Some(20),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "zmbv",
                out_name:       "zmbv8.avi",
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  PAL8_FORMAT,
                flipped: false,
                bits:    8,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        let enc_options = &[
                NAOption { name: "range", value: NAValue::Int(16) },
                NAOption { name: "full_me", value: NAValue::Bool(true) },
            ];
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          &[0x18bd3754, 0x97007f81, 0xff2bcd07, 0x739c48dc]);
    }

    #[test]
    fn test_zmbv_encoder_15bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        generic_register_all_encoders(&mut enc_reg);

        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Misc/zmbv_15bit.avi",
                stream_type:    StreamType::Video,
                limit:          Some(4),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "zmbv",
                out_name:       "zmbv15.avi",
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  RGB555_FORMAT,
                flipped: false,
                bits:    8,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        let enc_options = &[
                NAOption { name: "range", value: NAValue::Int(16) },
                NAOption { name: "full_me", value: NAValue::Bool(true) },
            ];
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          &[0x00311257, 0xd26a0e9e, 0xfd4b003f, 0x7c962d7b]);
    }

    #[test]
    fn test_zmbv_encoder_16bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        generic_register_all_encoders(&mut enc_reg);

        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Misc/zmbv_16bit.avi",
                stream_type:    StreamType::Video,
                limit:          Some(4),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "zmbv",
                out_name:       "zmbv16.avi",
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  RGB565_FORMAT,
                flipped: false,
                bits:    8,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        let enc_options = &[
                NAOption { name: "range", value: NAValue::Int(16) },
                NAOption { name: "full_me", value: NAValue::Bool(true) },
            ];
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          &[0x4eea104f, 0x2ebe544b, 0x54deb0f9, 0xe5ca88f4]);
    }

    #[test]
    fn test_zmbv_encoder_32bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        generic_register_all_encoders(&mut enc_reg);

        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Misc/zmbv_32bit.avi",
                stream_type:    StreamType::Video,
                limit:          Some(4),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "zmbv",
                out_name:       "zmbv32.avi",
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  RGB24_0_FORMAT,
                flipped: false,
                bits:    8,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        let enc_options = &[
                NAOption { name: "range", value: NAValue::Int(16) },
                NAOption { name: "full_me", value: NAValue::Bool(true) },
            ];
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          &[0xffceb4bd, 0xb1beccd9, 0x4983e7f6, 0xf46e33ba]);
    }
}
