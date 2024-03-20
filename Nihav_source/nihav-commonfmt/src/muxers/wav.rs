use nihav_core::muxers::*;
use nihav_registry::register::*;

struct WAVMuxer<'a> {
    bw:         &'a mut ByteWriter<'a>,
    data_pos:   u64,
}

impl<'a> WAVMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
            data_pos:   0,
        }
    }
}

fn patch_size(bw: &mut ByteWriter, pos: u64) -> MuxerResult<()> {
    let size = bw.tell() - pos;
    bw.seek(SeekFrom::Current(-((size + 4) as i64)))?;
    bw.write_u32le(size as u32)?;
    bw.seek(SeekFrom::End(0))?;
    Ok(())
}

impl<'a> MuxCore<'a> for WAVMuxer<'a> {
    fn create(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        if strmgr.get_num_streams() != 1 {
            return Err(MuxerError::InvalidArgument);
        }

        let stream = strmgr.get_stream(0).unwrap();

        if stream.get_info().get_properties().get_audio_info().is_none() {
            return Err(MuxerError::InvalidArgument);
        }
        let ainfo = stream.get_info().get_properties().get_audio_info().unwrap();

        let edata_len = if let Some(ref buf) = stream.get_info().get_extradata() { buf.len() } else { 0 };
        if edata_len >= (1 << 16) {
            return Err(MuxerError::UnsupportedFormat);
        }

        let twocc = find_wav_twocc(stream.get_info().get_name());
        if twocc.is_none() {
            return Err(MuxerError::UnsupportedFormat);
        }
        let twocc = if stream.get_info().get_name() == "pcm" {
                if !ainfo.format.float { 0x0001 } else { 0x0003 }
            } else {
                twocc.unwrap_or(0)
            };
        let avg_bytes_per_sec = if stream.get_info().get_name() == "pcm" {
                (u32::from(ainfo.channels) * ainfo.sample_rate * u32::from(ainfo.format.bits)) >> 3
            } else {
                0
            };

        self.bw.write_buf(b"RIFF\0\0\0\0WAVEfmt ")?;
        self.bw.write_u32le(if edata_len == 0 { 16 } else { 18 + edata_len } as u32)?;
        self.bw.write_u16le(twocc)?;
        self.bw.write_u16le(u16::from(ainfo.channels))?;
        self.bw.write_u32le(ainfo.sample_rate)?;
        self.bw.write_u32le(avg_bytes_per_sec)?;
        self.bw.write_u16le(ainfo.block_len as u16)?;
        self.bw.write_u16le(u16::from(ainfo.format.bits))?;
        if let Some(ref buf) = stream.get_info().get_extradata() {
            self.bw.write_u16le(edata_len as u16)?;
            self.bw.write_buf(buf.as_slice())?;
        }
        self.bw.write_buf(b"data\0\0\0\0")?;
        self.data_pos = self.bw.tell();

        Ok(())
    }
    fn mux_frame(&mut self, _strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        if self.data_pos == 0 {
            return Err(MuxerError::NotCreated);
        }

        let stream = pkt.get_stream();
        if stream.get_num() != 0 {
            return Err(MuxerError::UnsupportedFormat);
        }

        self.bw.write_buf(pkt.get_buffer().as_slice())?;
        Ok(())
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
        patch_size(self.bw, self.data_pos)?;
        patch_size(self.bw, 8)?;
        // todo patch avg_bytes_per_second if calculated
        // todo write fact value if calculated
        Ok(())
    }
}

impl<'a> NAOptionHandler for WAVMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct WAVMuxerCreator {}

impl MuxerCreator for WAVMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(WAVMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "wav" }
    fn get_capabilities(&self) -> MuxerCapabilities { MuxerCapabilities::SingleAudio("any") }
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_wav_muxer() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        // sample: https://samples.mplayerhq.hu/V-codecs/RT21/320x240/laser05.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Indeo/laser05.avi",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "wav",
                enc_name:   "",
                out_name:   "muxed.wav",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "wav", &mux_reg,
                          [0x1040ebe8, 0xe7a43e84, 0x49fbe234, 0xe870b6b3]);
    }
}
