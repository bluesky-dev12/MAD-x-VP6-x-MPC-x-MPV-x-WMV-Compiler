use nihav_core::io::byteio::{ByteReader,MemoryReader};
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::codecs::*;

#[derive(Clone,Copy)]
struct BandPos {
    plane:  usize,
    x:      usize,
    y:      usize,
    w:      usize,
    h:      usize,
    level:  u8,
    ilevel: u8,
}

struct MWV1Decoder {
    info:   NACodecInfoRef,
    mode:   u32,
    flags:  u32,
    l_cb:   Codebook<u8>,
    h_cb:   Codebook<u8>,
    bands:  Vec<BandPos>,
    tmp:    Vec<f32>,
    plane:  [Vec<f32>; 3],
    stride: [usize; 3],
}

#[allow(clippy::too_many_arguments)]
fn decode_band(br: &mut BitReader, cb: &Codebook<u8>, dst: &mut [f32], w: usize, h: usize, stride: usize, scale: f32, round: bool, zero_skip: bool) -> DecoderResult<()> {
    let mut ival = 0;
    let mut zero_run = 0;

    for (y, row) in dst.chunks_mut(stride).take(h).enumerate() {
        for i in 0..w {
            let idx = if (y & 1) == 0 { i } else { w - 1 - i };
            if zero_run > 0 {
                if !zero_skip {
                    row[idx] = (ival as f32) * scale;
                }
                zero_run -= 1;
                continue;
            }
            let val                         = br.read_cb(cb)?;
            let diff = match val {
                    0 => {
                        let esc             = br.read(8)? as i16 - 0x80;
                        if esc < 0 {
                            esc - 0x7B
                        } else {
                            esc + 0x7C
                        }
                    },
                    1 => {
                        let esc             = br.read(12)? as i16 - 0x800;
                        if esc < 0 {
                            esc - 0xFB
                        } else {
                            esc + 0xFC
                        }
                    },
                    2 => {
                        zero_run            = br.read(4)? + 4;
                        if zero_skip {
                            continue;
                        }
                        0
                    },
                    3 => {
                        zero_run            = br.read(8)? + 20;
                        if zero_skip {
                            continue;
                        }
                        0
                    },
                    4 => {
                        zero_run            = br.read(12)? + 276;
                        if zero_skip {
                            continue;
                        }
                        0
                    },
                    0x80 => {
                        let esc             = (br.read(16)? ^ 0x8000) as i16;
                        if esc < 0 {
                            esc - 0x8FB
                        } else {
                            esc + 0x8FC
                        }
                    },
                    0xFC | 0xFD | 0xFE | 0xFF => {
                        zero_run = u32::from(val - 0xFC);
                        if zero_skip {
                            continue;
                        }
                        0
                    },
                    _ => {
                        i16::from(val) - 0x80
                    },
                };
            if !zero_skip {
                ival += diff;
                row[idx] = (ival as f32) * scale;
            } else {
                let bias = if !round { 0.0 } else if diff > 0 { 0.5 } else { -0.5 };
                row[idx] = ((diff as f32) + bias) * scale;
            }
        }
    }
    validate!(zero_run == 0);

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn decode_band2(br: &mut BitReader, cb: &Codebook<u8>, dst: &mut [f32], w: usize, h: usize, stride: usize, scale: f32, round: bool) -> DecoderResult<()> {
    const ZERO_RUN_BITS: [u8; 12] = [ 16, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 ];
    const ZERO_RUN_ADD: [u32; 12] = [
        0x4839, 0x839, 0x439, 0x239, 0x139, 0xB9, 0x79, 0x59, 0x49, 0x41, 0x3D, 0x3B
    ];

    let mut zero_run = 0;

    for (y, row) in dst.chunks_mut(stride).take(h).enumerate() {
        for i in 0..w {
            let idx = if (y & 1) == 0 { i } else { w - 1 - i };

            if zero_run > 0 {
                zero_run -= 1;
                continue;
            }
            let val                         = br.read_cb(cb)?;
            let ival = match val {
                    0x00..=0x40 => i16::from(val) - 32,
                    0x41..=0x7B => {
                        zero_run = u32::from(val) - 0x41;
                        continue;
                    },
                    0xEF..=0xFA => {
                        let idx = (val - 0xEF) as usize;
                        zero_run            = br.read(ZERO_RUN_BITS[idx])? + ZERO_RUN_ADD[idx];
                        continue;
                    },
                    0xFE => {
                        let esc             = br.read(14)? as i16 - 0x2000;
                        if esc < 0 {
                            esc - 0x220
                        } else {
                            esc + 0x221
                        }
                    },
                    0xFF => {
                        let esc             = br.read(10)? as i16 - 0x200;
                        if esc < 0 {
                            esc - 0x20
                        } else {
                            esc + 0x21
                        }
                    },
                    _ => return Err(DecoderError::InvalidData),
                };
            let bias = if !round { 0.0 } else if ival > 0 { 0.5 } else { -0.5 };
            row[idx] = ((ival as f32) + bias) * scale;
        }
    }
    validate!(zero_run == 0);

    Ok(())
}

fn combine_bands(lo: &[f32], hi: &[f32], dst: &mut [f32], step: usize, len: usize) {
    dst[0] = (lo[0] - lo[step]) / 128.0 + (hi[0] + lo[0]) / 16.0;
    dst[1] = (lo[step] - lo[0]) / 128.0 + (lo[0] - hi[0]) / 16.0;
    let mut didx  = 2;
    let mut loidx = 0;
    let mut hiidx = step;
    for _i in 0..(((len + 1) >> 1) - 2) {
        let tmp = (lo[loidx] - lo[loidx + 2 * step]) / 128.0;
        dst[didx]     = (lo[loidx + step] + hi[hiidx]) / 16.0 + tmp;
        dst[didx + 1] = (lo[loidx + step] - hi[hiidx]) / 16.0 - tmp;
        didx  += 2;
        loidx += step;
        hiidx += step;
    }
    if (len & 1) == 0 {
        let tmp = (lo[loidx] - lo[loidx + step]) / 128.0;
        dst[didx]     = (lo[loidx + step] + hi[hiidx]) / 16.0 + tmp;
        dst[didx + 1] = (lo[loidx + step] - hi[hiidx]) / 16.0 - tmp;
    } else {
        dst[didx] = lo[loidx + step] / 16.0;
    }
}

fn map_index(idx: usize) -> u8 { idx as u8 }

impl MWV1Decoder {
    fn new() -> Self {
        let mut cbr = TableCodebookDescReader::new(&L_CODE_BITS, &L_CODE_LENS, map_index);
        let l_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(&H_CODE_BITS, &H_CODE_LENS, map_index);
        let h_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();
        MWV1Decoder {
            info:   NACodecInfo::new_dummy(),
            l_cb, h_cb,
            mode:   0,
            flags:  0,
            bands:  Vec::new(),
            plane:  [Vec::new(), Vec::new(), Vec::new()],
            stride: [0; 3],
            tmp:    Vec::new(),
        }
    }
    fn reconstruct(&mut self, frm: &mut NASimpleVideoFrame<u8>) {
        for band in self.bands.iter() {
            if band.x == 0 || band.y == 0 {
                continue;
            }

            let stride = self.stride[band.plane] << band.ilevel;
            let dst_yy = [0, 1 << band.ilevel >> 1];
            for &dst_y in dst_yy.iter() {
                let dst = &mut self.plane[band.plane][dst_y * self.stride[band.plane]..];
                for row in dst.chunks_mut(stride).take(band.h) {
                    let (lo, hi) = row.split_at(band.x);
                    let size = band.x + band.w;
                    combine_bands(lo, hi, &mut self.tmp, 1, size);
                    row[..size].copy_from_slice(&self.tmp[..size]);
                }
            }
            let size = band.y + band.h;
            for x in 0..band.x + band.w {
                let col = &self.plane[band.plane][x..];
                combine_bands(col, &col[stride/2..], &mut self.tmp, stride, size);
                for y in 0..size {
                    self.plane[band.plane][x + y * (stride/2)] = self.tmp[y] * 128.0;
                }
            }
        }

        for plane in 0..3 {
            let dst = &mut frm.data[frm.offset[plane]..];
            for (drow, srow) in dst.chunks_mut(frm.stride[plane]).zip(self.plane[plane].chunks(self.stride[plane])) {
                for (dst, &src) in drow.iter_mut().zip(srow.iter()) {
                    *dst = (src + 128.0).max(0.0).min(255.0) as u8;
                }
            }
        }
    }
}

impl NADecoder for MWV1Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(_vinfo) = info.get_properties() {
            if let Some(edata) = info.get_extradata() {
                validate!(edata.len() >= 64);
                let mut mr = MemoryReader::new_read(edata.as_slice());
                let mut br = ByteReader::new(&mut mr);
                let len                 = br.read_u32be()? as usize;
                validate!(edata.len() == len);
                let _                   = br.read_u32be()?; //always 7?
                self.mode               = br.read_u32be()?; //pic_fmt
                let width               = br.read_u32be()? as usize;
                let height              = br.read_u32be()? as usize;
                let _smth               = br.read_u16be()?;
                let _bpp                = br.read_u16be()?;
                let _fmt                = br.read_u32le()?;
                let _unp_size           = br.read_u32be()? as usize;
                                          br.read_u32be()?; // always zero?
                                          br.read_u32be()?; // always zero?
                                          br.read_u32be()?; // always zero?
                                          br.read_u32be()?; // always zero?
                self.flags              = br.read_u32be()?; // 0x100 - interlaced?
                let mut levels = [[0; 2]; 3];
                for plane_lev in levels.iter_mut() {
                    for level in plane_lev.iter_mut() {
                        let lev         = br.read_u16be()?;
                        validate!(lev < 32);
                        *level = lev as u8;
                    }
                }
                validate!(width > 1 && height > 1);
                for plane_lev in levels.iter() {
                    if plane_lev[0] != plane_lev[1] {
                        return Err(DecoderError::NotImplemented);
                    }
                }
                let max_levels = levels[0][0].max(levels[1][0]).max(levels[2][0]);
                validate!(max_levels < 8);
                self.bands = Vec::with_capacity((max_levels as usize) * 9 + 1);
                self.tmp = vec![0.0; width.max(height)];
                let mut dim = [[width, height], [width >> 1, height >> 1], [width >> 1, height >> 1]];

                for level in (0..max_levels).rev() {
                    for plane in (0..3).rev() {
                        if level < levels[plane][0] {
                            let bp = BandPos {
                                    plane,
                                    level,
                                    ilevel: levels[plane][0] - level,
                                    x: (dim[plane][0] + 1) >> 1,
                                    y: (dim[plane][1] + 1) >> 1,
                                    w:  dim[plane][0]      >> 1,
                                    h:  dim[plane][1]      >> 1,
                                };
                            self.bands.push(bp);
                        }
                    }
                    for plane in (0..3).rev() {
                        if level < levels[plane][0] {
                            let bp = BandPos {
                                    plane,
                                    level,
                                    ilevel: levels[plane][0] - level,
                                    x: 0,
                                    y: (dim[plane][1] + 1) >> 1,
                                    w: (dim[plane][0] + 1) >> 1,
                                    h:  dim[plane][1]      >> 1,
                                };
                            self.bands.push(bp);
                        }
                    }
                    for plane in (0..3).rev() {
                        if level < levels[plane][0] {
                            let bp = BandPos {
                                    plane,
                                    level,
                                    ilevel: levels[plane][0] - level,
                                    x: (dim[plane][0] + 1) >> 1,
                                    y: 0,
                                    w:  dim[plane][0]      >> 1,
                                    h: (dim[plane][1] + 1) >> 1,
                                };
                            self.bands.push(bp);
                        }
                    }

                    if level != 0 {
                        for plane in 0..3 {
                            if level < levels[plane][0] {
                                dim[plane][0] = (dim[plane][0] + 1) >> 1;
                                dim[plane][1] = (dim[plane][1] + 1) >> 1;
                            }
                        }
                    }
                }
                for plane in (0..3).rev() {
                    let bp = BandPos {
                            plane,
                            level: 0,
                            ilevel: levels[plane][0],
                            x: 0,
                            y: 0,
                            w: (dim[plane][0] + 1) >> 1,
                            h: (dim[plane][1] + 1) >> 1,
                        };
                    self.bands.push(bp);
                }
                self.bands.reverse();

                self.plane = [vec![0.0; width * height], vec![0.0; width * height / 4], vec![0.0; width * height / 4]];
                self.stride = [width, width >> 1, width >> 1];

                let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(width, height, false, YUV420_FORMAT));
                self.info = NACodecInfo::new_ref(info.get_name(), myinfo, None).into_ref();
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
        if src.len() <= 26 { return Err(DecoderError::ShortData); }

        let mut br = BitReader::new(&src, BitReaderMode::BE);

        for plane in self.plane.iter_mut() {
            for el in plane.iter_mut() {
                *el = 0.0;
            }
        }

        let mut has_hdr = false;
        let mut band_iter = self.bands.iter_mut();

        while br.left() > 5 {
            let ff                      = br.read(32)?;
            validate!(ff == 0xFFFFFFFF);
            let tag_id                  = br.read(8)?;
            match tag_id {
                0xAA => {
                    validate!(!has_hdr);
                    let size            = br.read(32)?;
                    validate!(size >= 6);
                    let end = br.tell() + (size as usize) * 8 - 32;
                                          br.read(16)?;
                    if (self.flags & 0x100) == 0 {
                                          br.read(16)?;
                                          br.read(16)?;
                    }
                    if self.mode > 1 {
                                          br.read(8)?;
                    }
                    if self.mode > 4 {
                        let _size       = br.read(32)?;
                    }

                    validate!(br.tell() <= end);
                    let tail = end - br.tell();
                                          br.skip(tail as u32)?;
                    has_hdr = true;
                },
                0xAB => {
                    /*validate!(!has_hdr);
                    let size            = br.read(32)?;
                    validate!(size >= 6);
                                          br.read(16)?;
                    if self.levels == 2 || self.levels == 3 {
                                          br.read(16)?;
                                          br.read(16)?;
                    }
                    if components > 1 {
                                          br.read(8)?;
                    }
                                          br.skip((size - 6) * 8)?;
                    has_hdr = true;*/
                    return Err(DecoderError::NotImplemented);
                },
                0xD1 => {
                    let next_band = band_iter.next();
                    validate!(next_band.is_some());
                    let band = *next_band.unwrap();

                    let scale;
                    let band_mode;
                    let lscale = (1 << band.level) as f32;
                    if self.mode < 3 {
                        /*let _           = br.read(32)?;
                        let _           = br.read(32)?;
                        scale           = (br.read(32)? as f32) / 32768.0;*/
                        return Err(DecoderError::NotImplemented);
                    } else {
                        band_mode       = br.read(8)? as u8;
                        if band_mode == 0 {
                            scale = 0.0;
                        } else {
                            scale       = (br.read(32)? as f32) * lscale / 32768.0;
                        }
                    }

                    let dst_y = if band.y != 0 { 1 << band.ilevel >> 1 } else { 0 };

                    let dst = &mut self.plane[band.plane][band.x + dst_y * self.stride[band.plane]..];
                    let stride = self.stride[band.plane] << band.ilevel;
                    let round = (band_mode & 8) != 0;
                    match band_mode {
                        0 => {}, // empty band
                        1 | 9 => {
                            decode_band(&mut br, &self.l_cb, dst, band.w, band.h, stride, scale, round, true)?;
                        },
                        2 | 10 => {
                            decode_band2(&mut br, &self.h_cb, dst, band.w, band.h, stride, scale, round)?;
                        },
                        5 => {
                            decode_band(&mut br, &self.l_cb, dst, band.w, band.h, stride, scale, false, false)?;
                        },
                        _ => return Err(DecoderError::InvalidData),
                    };
                                          br.align();
                },
                0xD2 | 0xDA => {
                    let size            = br.read(32)?;
                    validate!(size >= 4);
                                          br.skip((size - 4) * 8)?;
                },
                0xD7 => {
                    let size            = br.read(32)?;
                    validate!(size == 13);
                                          br.read(16)?;
                    for _ in 0..3 {
                                          br.read(16)?;
                    }
                                          br.read(8)?;
                },
                0xDD => {
                    return Err(DecoderError::NotImplemented);
                },
                _ => {
                    return Err(DecoderError::NotImplemented);
                },
            };
            if tag_id == 0 { break; }
        }

        let vinfo = self.info.get_properties().get_video_info().unwrap();
        let bufinfo = alloc_video_buffer(vinfo, 2)?;
        let mut buf = bufinfo.get_vbuf().unwrap();
        let mut frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();

        self.reconstruct(&mut frm);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(true);
        frm.set_frame_type(FrameType::I);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for MWV1Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(MWV1Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::misc_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_mwv1() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        misc_register_all_decoders(&mut dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/MWV1/test.avi
        test_decoding("avi", "mwv1", "assets/Misc/mwv1.avi", Some(2), &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5Frames(vec![
                        [0x9f2e0e5b, 0xa767c1ae, 0x8c009dca, 0x7159d0cd],
                        [0xfc00be21, 0x899736d0, 0x6b18dd40, 0x5261af2c],
                        [0xf113305d, 0xffac919f, 0x0b64890e, 0x18c60420]]));
    }
}

const L_CODE_BITS: [u16; 256] = [
    0xFF6B, 0xFF76, 0x0014, 0x00F0, 0xFF77, 0xFF78, 0xFF79, 0xFF7A,
    0xFF7B, 0xFF7C, 0xFF7D, 0xFF7E, 0xFF7F, 0xFF80, 0xFF81, 0xFF82,
    0xFF83, 0xFF84, 0xFF85, 0xFF86, 0xFF87, 0xFF88, 0xFF89, 0xFF8A,
    0xFF8B, 0xFF8C, 0xFF8D, 0xFF8E, 0xFF8F, 0xFF90, 0xFF91, 0xFF92,
    0xFF93, 0xFF94, 0xFF95, 0xFF96, 0xFF97, 0xFF98, 0xFF99, 0xFF9A,
    0xFF9B, 0xFF9C, 0xFF9D, 0xFF9E, 0xFF9F, 0xFFA0, 0xFFA1, 0xFFA2,
    0xFFA3, 0xFFA4, 0xFFA5, 0xFFA6, 0xFFA7, 0xFFA8, 0xFFA9, 0xFFAA,
    0xFFAB, 0xFFAC, 0xFFAD, 0xFFAE, 0xFFAF, 0xFFB0, 0xFFB1, 0xFFB2,
    0xFFB3, 0xFFB4, 0xFF6C, 0xFFB5, 0xFFB6, 0xFF6D, 0xFFB7, 0xFF6E,
    0xFFB8, 0xFFB9, 0xFFBA, 0xFFBB, 0xFF6F, 0xFF5B, 0xFFBC, 0xFF4F,
    0xFFBD, 0xFF5C, 0xFFBE, 0xFFBF, 0xFF5D, 0xFF50, 0xFF5E, 0xFF70,
    0xFF5F, 0xFF60, 0xFF61, 0x1FDC, 0xFF51, 0x1FDD, 0x1FDE, 0x1FDF,
    0x1FE0, 0x1FE1, 0x1FE2, 0x1FE3, 0x0FE4, 0x0FE5, 0x07E8, 0x0FE6,
    0x0FE7, 0x07E9, 0x07EA, 0x07EB, 0x07EC, 0x07ED, 0x07EE, 0x03EA,
    0x03EB, 0x03EC, 0x01F0, 0x01F1, 0x01F2, 0x00F1, 0x00F2, 0x00F3,
    0x0074, 0x0075, 0x0034, 0x0035, 0x0015, 0x0016, 0x0008, 0x0000,
    0xFFC0, 0x0002, 0x0009, 0x0017, 0x0018, 0x0036, 0x0037, 0x0076,
    0x0077, 0x00F4, 0x00F5, 0x00F6, 0x00F7, 0x01F3, 0x01F4, 0x03ED,
    0x03EE, 0x03EF, 0x03F0, 0x03F1, 0x03F2, 0x07EF, 0x07F0, 0x07F1,
    0x03F3, 0x0FE8, 0x0FE9, 0x0FEA, 0x0FEB, 0x0FEC, 0x0FED, 0x1FE4,
    0x1FE5, 0x1FE6, 0x1FE7, 0xFF52, 0x1FE8, 0x3FD2, 0xFF53, 0x7FA6,
    0xFF62, 0xFF63, 0xFF54, 0xFF55, 0xFF64, 0xFF65, 0xFF71, 0xFF72,
    0xFF56, 0xFF57, 0xFF58, 0xFF66, 0xFF67, 0xFF68, 0xFF69, 0xFF73,
    0xFFC1, 0xFF74, 0xFFC2, 0xFF6A, 0xFF75, 0xFF59, 0xFF4E, 0xFF5A,
    0xFFC3, 0xFFC4, 0xFFC5, 0xFFC6, 0xFFC7, 0xFFC8, 0xFFC9, 0xFFCA,
    0xFFCB, 0xFFCC, 0xFFCD, 0xFFCE, 0xFFCF, 0xFFD0, 0xFFD1, 0xFFD2,
    0xFFD3, 0xFFD4, 0xFFD5, 0xFFD6, 0xFFD7, 0xFFD8, 0xFFD9, 0xFFDA,
    0xFFDB, 0xFFDC, 0xFFDD, 0xFFDE, 0xFFDF, 0xFFE0, 0xFFE1, 0xFFE2,
    0xFFE3, 0xFFE4, 0xFFE5, 0xFFE6, 0xFFE7, 0xFFE8, 0xFFE9, 0xFFEA,
    0xFFEB, 0xFFEC, 0xFFED, 0xFFEE, 0xFFEF, 0xFFF0, 0xFFF1, 0xFFF2,
    0xFFF3, 0xFFF4, 0xFFF5, 0xFFF6, 0xFFF7, 0xFFF8, 0xFFF9, 0xFFFA,
    0xFFFB, 0xFFFC, 0xFFFD, 0xFFFE, 0x0003, 0x0019, 0x0038, 0x0039
];
const L_CODE_LENS: [u8; 256] = [
    16, 16,  5,  8, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 13, 16, 13, 13, 13,
    13, 13, 13, 13, 12, 12, 11, 12, 12, 11, 11, 11, 11, 11, 11, 10,
    10, 10,  9,  9,  9,  8,  8,  8,  7,  7,  6,  6,  5,  5,  4,  2,
    16,  3,  4,  5,  5,  6,  6,  7,  7,  8,  8,  8,  8,  9,  9, 10,
    10, 10, 10, 10, 10, 11, 11, 11, 10, 12, 12, 12, 12, 12, 12, 13,
    13, 13, 13, 16, 13, 14, 16, 15, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,  3,  5,  6,  6
];
const H_CODE_BITS: [u16; 256] = [
    0xFFFF, 0xFFE5, 0xFFE6, 0xFFE7, 0xFFE8, 0xFFE9, 0xFFEA, 0xFFEB,
    0xFFEC, 0xFFED, 0xFFEE, 0xFFEF, 0xFFF0, 0xFFE0, 0xFFDB, 0xFFDC,
    0xFFDA, 0xFFD5, 0xFFD6, 0x1FF6, 0x1FF7, 0x0FEE, 0x0FEF, 0x07E0,
    0x07E1, 0x03E4, 0x01E4, 0x00E8, 0x006E, 0x0032, 0x0016, 0x0000,
    0x0000, 0x0001, 0x0017, 0x0033, 0x006F, 0x00E9, 0x01E5, 0x03E5,
    0x07E2, 0x07E3, 0x07E4, 0x0FF0, 0x1FF8, 0xFFD7, 0xFFD8, 0xFFD9,
    0xFFDD, 0xFFDE, 0xFFE1, 0xFFE2, 0xFFF1, 0xFFF2, 0xFFE3, 0xFFF3,
    0xFFF4, 0xFFF5, 0xFFF6, 0xFFF7, 0xFFF8, 0xFFF9, 0xFFFA, 0xFFFB,
    0xFFFC, 0x0004, 0x000A, 0x0018, 0x0034, 0x0035, 0x0036, 0x0070,
    0x0071, 0x0072, 0x0073, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE,
    0x00EF, 0x01E6, 0x01E7, 0x01E8, 0x01E9, 0x01EA, 0x01EB, 0x01EC,
    0x01ED, 0x01EE, 0x03E6, 0x03E7, 0x03E8, 0x03E9, 0x03EA, 0x03EB,
    0x03EC, 0x03ED, 0x03EE, 0x07E5, 0x07E6, 0x07E7, 0x07E8, 0x07E9,
    0x07EA, 0x07EB, 0x07EC, 0x07ED, 0x07EE, 0x07EF, 0x07F0, 0x07F1,
    0x07F2, 0x07F3, 0x07F4, 0x0FF1, 0x0FF2, 0x0FF3, 0x0FF4, 0x0FF5,
    0x0FF6, 0x0FF7, 0x0FF8, 0x0FF9, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0xFFDF,
    0x1FF9, 0x3FF4, 0x0FFA, 0x07F5, 0x00F0, 0x00F1, 0x01EF, 0x01F0,
    0x01F1, 0x03EF, 0x07F6, 0x0000, 0x0000, 0xFFD4, 0xFFFD, 0xFFFE
];
const H_CODE_LENS: [u8; 256] = [
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 13, 13, 12, 12, 11, 11, 10,  9,  8,  7,  6,  5,  2,
     0,  2,  5,  6,  7,  8,  9, 10, 11, 11, 11, 12, 13, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16,  3,  4,  5,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  8,  8,
     8,  9,  9,  9,  9,  9,  9,  9,  9,  9, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 16,
    13, 14, 12, 11,  8,  8,  9,  9,  9, 10, 11,  0,  0, 16, 16, 16
];
