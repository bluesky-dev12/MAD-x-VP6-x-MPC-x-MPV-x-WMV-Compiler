use nihav_core::muxers::*;

struct EAMuxer<'a> {
    bw:             &'a mut ByteWriter<'a>,
    has_alpha:      bool,
    nframes:        u32,
    max_size:       [u32; 2],
}

impl<'a> EAMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
            has_alpha:      false,
            nframes:        0,
            max_size:       [0; 2],
        }
    }
}

impl<'a> MuxCore<'a> for EAMuxer<'a> {
    #[allow(clippy::unreadable_literal)]
    #[allow(clippy::cast_lossless)]
    fn create(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        if strmgr.get_num_streams() == 0 {
            return Err(MuxerError::InvalidArgument);
        }

        let mut nvideo = 0;
        for stream in strmgr.iter() {
            if stream.get_media_type() == StreamType::Video {
                if stream.get_info().get_name() != "vp6" {
                    return Err(MuxerError::UnsupportedFormat);
                }
                nvideo += 1;
            } else {
                return Err(MuxerError::UnsupportedFormat);
            }
        }
        if nvideo == 0 || nvideo > 2 {
            return Err(MuxerError::UnsupportedFormat);
        }
        self.has_alpha = nvideo == 2;

        if self.has_alpha {
            self.bw.write_buf(b"AVP6\x08\x00\x00\x00")?;
        }
        for (str_no, stream) in strmgr.iter().enumerate() {
            if let NACodecTypeInfo::Video(ref vinfo) = stream.get_info().get_properties() {
                let tag = if str_no == 0 { b"MVhd" } else { b"AVhd" };

                self.bw.write_buf(tag)?;
                self.bw.write_u32le(0x20)?;
                self.bw.write_buf(b"vp60")?;
                self.bw.write_u16le(vinfo.width as u16)?;
                self.bw.write_u16le(vinfo.height as u16)?;
                self.bw.write_u32le(0)?;
                self.bw.write_u32le(0)?;
                self.bw.write_u32le(stream.tb_den)?;
                self.bw.write_u32le(stream.tb_num)?;
            } else {
                unimplemented!();
            }
        }

        Ok(())
    }
    #[allow(clippy::collapsible_else_if)]
    fn mux_frame(&mut self, _strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        let stream = pkt.get_stream();
        let str_num = stream.get_num();
        if str_num > 2 {
            return Err(MuxerError::UnsupportedFormat);
        }

        let chunk_len = pkt.get_buffer().len() as u32;

        let tag = if pkt.is_keyframe() {
                if str_num == 0 { b"MV0K" } else { b"AV0K" }
            } else {
                if str_num == 0 { b"MV0F" } else { b"AV0F" }
            };
        self.max_size[str_num] = self.max_size[str_num].max(pkt.get_buffer().len() as u32);
        self.bw.write_buf(tag)?;
        self.bw.write_u32le(chunk_len + 8)?;
        self.bw.write_buf(&pkt.get_buffer())?;

        if str_num == 0 {
            self.nframes += 1;
        }

        Ok(())
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
        if !self.has_alpha {
            self.bw.seek(SeekFrom::Start(0x10))?;
            self.bw.write_u32le(self.nframes)?;
            self.bw.write_u32le(self.max_size[0])?;
        } else {
            self.bw.seek(SeekFrom::Start(0x18))?;
            self.bw.write_u32le(self.nframes)?;
            self.bw.write_u32le(self.max_size[0])?;
            self.bw.seek(SeekFrom::Start(0x38))?;
            self.bw.write_u32le(self.nframes)?;
            self.bw.write_u32le(self.max_size[1])?;
        }
        Ok(())
    }
}

impl<'a> NAOptionHandler for EAMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct EAMuxerCreator {}

impl MuxerCreator for EAMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(EAMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "ea" }
    fn get_capabilities(&self) -> MuxerCapabilities { MuxerCapabilities::OnlyVideo }
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_ea_muxer() {
        let mut dmx_reg = RegisteredDemuxers::new();
        nihav_commonfmt::generic_register_all_demuxers(&mut dmx_reg);
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Duck/vp6_crash.avi",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        game_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "ea",
                enc_name:   "",
                out_name:   "muxed.ea",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "ea", &mux_reg,
                          [0xc8c6484d, 0x863de1ae, 0x97a38a31, 0x59e2a7ef]);
    }
}
