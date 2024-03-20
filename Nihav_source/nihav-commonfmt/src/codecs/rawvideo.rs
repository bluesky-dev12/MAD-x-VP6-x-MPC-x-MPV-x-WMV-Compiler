use nihav_core::codecs::*;

struct RawDecoder {
    info:   NACodecInfoRef,
}

impl RawDecoder {
    fn new() -> Self {
        Self {
            info:   NACodecInfo::new_dummy(),
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
            let ncomp = vinfo.format.components as usize;
            let buf = if vinfo.format.is_unpacked() && vinfo.format.get_max_depth() == 8 {
                    let mut offs = vec![0; ncomp];
                    let mut strides = Vec::with_capacity(ncomp);
                    let mut sizes = Vec::with_capacity(ncomp);
                    for chr in vinfo.format.comp_info[..ncomp].iter() {
                        if let Some(chromaton) = chr {
                            let stride = chromaton.get_linesize(width);
                            let size = stride * chromaton.get_height(height);
                            sizes.push(size);
                            strides.push(stride);
                        } else {
                            return Err(DecoderError::InvalidData);
                        }
                    }
                    let mut off = 0;
                    for i in 0..ncomp {
                        for (cno, chr) in vinfo.format.comp_info[..ncomp].iter().enumerate() {
                            if let Some(chromaton) = chr {
                                let comp_off = chromaton.comp_offs as usize;
                                validate!(comp_off < ncomp);
                                if comp_off != i {
                                    continue;
                                }
                                offs[cno] = off;
                                off += sizes[i];
                            }
                        }
                    }
                    validate!(off == src.len());

                    NABufferType::Video(NAVideoBuffer::from_raw_parts(*vinfo, src, offs, strides).into_ref())
                } else {
                    let esize = vinfo.format.elem_size as usize;
                    let ychr = vinfo.format.get_chromaton(0).unwrap();
                    let ystep = if ychr.next_elem != 0 { ychr.next_elem as usize } else { esize };
                    let stride = (width * esize + ystep - 1) / ystep;
                    let offs    = vec![0];
                    let strides = vec![stride];
                    NABufferType::VideoPacked(NAVideoBuffer::from_raw_parts(*vinfo, src, offs, strides).into_ref())
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
    fn test_rawvideo() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        // sample: self-created with avconv
        test_decoding("yuv4mpeg", "rawvideo", "assets/Misc/test.y4m", None, &dmx_reg,
                     &dec_reg, ExpectedTestResult::MD5Frames(vec![
                                [0xd58326b0, 0xdbfc1dcc, 0x6d66a04c, 0x08a21bbb],
                                [0x9b2cb5c5, 0x69b5f261, 0xcaccaaaf, 0xff2a807d]]));
    }
}
