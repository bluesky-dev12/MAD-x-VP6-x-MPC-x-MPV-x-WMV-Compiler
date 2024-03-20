use nihav_core::codecs::*;

struct RawDecoder {
    info:   NACodecInfoRef,
    pal:    [u8; 768],
}

impl RawDecoder {
    fn new() -> Self {
        Self {
            info:   NACodecInfo::new_dummy(),
            pal:    [0; 768],
        }
    }
}

impl NADecoder for RawDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(_vinfo) = info.get_properties() {
            self.info = info.clone();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        if let NACodecTypeInfo::Video(ref vinfo) = self.info.get_properties() {
            let src = pkt.get_buffer();
            let width  = vinfo.width;
            let height = vinfo.height;
            if !vinfo.format.model.is_rgb() {
                return Err(DecoderError::InvalidData);
            }
            let depth = if vinfo.format.is_paletted() { 8 } else { vinfo.format.get_total_depth() } as usize;
            let buf = match depth {
                    8 => {
                        for sd in pkt.side_data.iter() {
                            match *sd {
                                NASideData::Palette(_, ref pal) => {
                                    for (dst, src) in self.pal.chunks_mut(3).zip(pal.chunks(4)) {
                                        dst[0] = src[0];
                                        dst[1] = src[1];
                                        dst[2] = src[2];
                                    }
                                    break;
                                },
                                _ => {},
                            };
                        }

                        let sstride = (vinfo.width + 3) & !3;

                        let buf = alloc_video_buffer(*vinfo, 0)?;

                        let mut frm = buf.get_vbuf().unwrap();
                        let dstride = frm.get_stride(0);
                        let paloff  = frm.get_offset(1);
                        let dst     = frm.get_data_mut().unwrap();
                        for (drow, srow) in dst.chunks_mut(dstride).zip(src.chunks(sstride)).take(height) {
                            drow[..width].copy_from_slice(&srow[..width]);
                        }
                        dst[paloff..][..768].copy_from_slice(&self.pal);

                        buf
                    },
                    15 | 16 => {
                        let sstride = (vinfo.width * 2 + 3) & !3;

                        let buf = alloc_video_buffer(*vinfo, 0)?;

                        let mut frm = buf.get_vbuf16().unwrap();
                        let dstride = frm.get_stride(0);
                        let dst     = frm.get_data_mut().unwrap();

                        for (drow, srow) in dst.chunks_mut(dstride).zip(src.chunks(sstride)).take(height) {
                            for (dp, sp) in drow.iter_mut().zip(srow.chunks_exact(2)).take(width) {
                                *dp = u16::from(sp[0]) | (u16::from(sp[1]) << 8);
                            }
                        }

                        buf
                    },
                    24 | 32 => {
                        let ncomp = vinfo.format.components as usize;
                        let sstride = (width * (depth / 8) + 3) & !3;
                        let offs    = vec![0; ncomp];
                        let mut strides = vec![0; ncomp];
                        strides[0] = sstride;
                         NABufferType::VideoPacked(NAVideoBuffer::from_raw_parts(*vinfo, src, offs, strides).into_ref())
                    },
                    _ => return Err(DecoderError::NotImplemented),
                };

            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), buf);
            frm.set_keyframe(true);
            frm.set_frame_type(FrameType::I);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn flush(&mut self) {}
}


impl NAOptionHandler for RawDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RawDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::generic_register_all_decoders;
    use crate::generic_register_all_demuxers;
    #[test]
    fn test_rawvideo_ms_8() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/Uncompressed/8bpp.avi
        test_decoding("avi", "rawvideo-ms", "assets/Misc/8bpp.avi", Some(0), &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5([0xb6629439, 0x6ea482e9, 0x42c84d7c, 0x46c94431]));
    }
    #[test]
    fn test_rawvideo_ms_16() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/Uncompressed/16bpp.avi
        test_decoding("avi", "rawvideo-ms", "assets/Misc/16bpp.avi", Some(0), &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5([0xe80e16a1, 0x2d50659e, 0x413d24af, 0xea3bee05]));
    }
    #[test]
    fn test_rawvideo_ms_24() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        // sample:https://samples.mplayerhq.hu/V-codecs/Uncompressed/keve.avi
        test_decoding("avi", "rawvideo-ms", "assets/Misc/keve.avi", Some(0), &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5([0x9514ac1f, 0x4512cc62, 0x069485ba, 0x084a1e63]));
    }
    #[test]
    fn test_rawvideo_ms_32() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/Uncompressed/Logo-Uncompressed.zip
        test_decoding("avi", "rawvideo-ms", "assets/Misc/VRMLuncompressed.avi", Some(0), &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5([0xf4c9d468, 0x8f42c576, 0xc8eb522a, 0x75f654b1]));
    }
}
