use nihav_core::muxers::*;

struct Y4MMuxer<'a> {
    bw:             &'a mut ByteWriter<'a>,
}

impl<'a> Y4MMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
        }
    }
}

fn get_format_name(fmt: NAPixelFormaton) -> MuxerResult<&'static str> {
    if fmt.model.is_yuv() && fmt.get_max_depth() == 8 {
        match fmt.components {
            1 => Ok("mono"),
            3 => {
                let uinfo = fmt.comp_info[1].unwrap();
                let vinfo = fmt.comp_info[2].unwrap();
                if uinfo.h_ss != vinfo.h_ss || uinfo.v_ss != vinfo.v_ss {
                    return Err(MuxerError::UnsupportedFormat);
                }
                match (uinfo.h_ss, vinfo.v_ss) {
                    (0, 0) => Ok("444"),
                    (1, 0) => Ok("422"),
                    (1, 1) => Ok("420"),
                    (2, 0) => Ok("411"),
                    _ => Err(MuxerError::UnsupportedFormat),
                }
            },
            _ => Err(MuxerError::UnsupportedFormat),
        }
    } else {
        Err(MuxerError::UnsupportedFormat)
    }
}

impl<'a> MuxCore<'a> for Y4MMuxer<'a> {
    fn create(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        if strmgr.get_num_streams() != 1 {
            return Err(MuxerError::InvalidArgument);
        }
        let vstr = strmgr.get_stream(0).unwrap();
        if vstr.get_media_type() != StreamType::Video {
            return Err(MuxerError::UnsupportedFormat);
        }
        let info = vstr.get_info();
        let vinfo = info.get_properties().get_video_info().unwrap();
        self.bw.write_buf(format!("YUV4MPEG2 W{} H{} F{}:{} C{}\n", vinfo.width, vinfo.height, vstr.tb_den, vstr.tb_num, get_format_name(vinfo.format)?).as_bytes())?;

        Ok(())
    }
    fn mux_frame(&mut self, _strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        if self.bw.tell() == 0 {
            return Err(MuxerError::NotCreated);
        }
        let src = pkt.get_buffer();
        self.bw.write_buf(b"FRAME\n")?;
        self.bw.write_buf(&src)?;
        Ok(())
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
        Ok(())
    }
}

impl<'a> NAOptionHandler for Y4MMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct Y4MMuxerCreator {}

impl MuxerCreator for Y4MMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(Y4MMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "yuv4mpeg" }
    fn get_capabilities(&self) -> MuxerCapabilities { MuxerCapabilities::SingleVideo("rawvideo") }
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_y4m_muxer() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        // sample: self-created with avconv
        let dec_config = DecoderTestParams {
                demuxer:        "yuv4mpeg",
                in_name:        "assets/Misc/test.y4m",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "yuv4mpeg",
                enc_name:   "",
                out_name:   "muxed.y4m",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "yuv4mpeg", &mux_reg,
                          [0x7586c1c5, 0x388209b8, 0xe08af8f8, 0x4b6b96c7]);
    }
}
