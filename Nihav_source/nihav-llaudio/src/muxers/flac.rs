use nihav_core::muxers::*;

struct FLACMuxer<'a> {
    bw:             &'a mut ByteWriter<'a>,
    maxpkt:         usize,
    minpkt:         usize,
    duration:       u64,
    maxblk:         u16,
    minblk:         u16,
    bits:           u8,
}

impl<'a> FLACMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
            maxpkt: std::usize::MAX, minpkt: 0,
            maxblk: std::u16::MAX, minblk: 0,
            duration: 0,
            bits: 0,
        }
    }
}

impl<'a> MuxCore<'a> for FLACMuxer<'a> {
    fn create(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        if strmgr.get_num_streams() == 0 {
            return Err(MuxerError::InvalidArgument);
        }
        let stream = strmgr.get_stream(0).unwrap();
        if let NACodecTypeInfo::Audio(ref ainfo) = stream.get_info().get_properties() {
            self.bw.write_buf(b"fLaC")?;
            self.bw.write_byte(0x80)?; // last metadata block - streaminfo
            self.bw.write_u24be(34)?; // streaminfo size
            self.bw.write_u16be(2)?; // minimum block size
            self.bw.write_u16be(ainfo.block_len as u16)?;
            self.bw.write_u24be(0)?; // minimum frame size
            self.bw.write_u24be(0)?; // maximum frame size

            let bits = ainfo.format.bits - 1;
            self.bits = bits;
            self.bw.write_u24be(ainfo.sample_rate * 16 + u32::from(ainfo.channels - 1) * 2 + u32::from(bits >> 4))?;
            self.bw.write_byte(bits << 4)?;
            self.bw.write_u32be(0)?;//total samples low 32 bits
            self.bw.write_u64be(0)?;self.bw.write_u64be(0)?; //MD5

            Ok(())
        } else {
            Err(MuxerError::InvalidArgument)
        }
    }
    fn mux_frame(&mut self, _strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        let pktlen  = pkt.get_buffer().len();
        let slen    = pkt.ts.duration.unwrap_or(0);
        let samples = if slen != 1 { slen } else { u64::from(pkt.ts.tb_den) };

        self.maxpkt = self.maxpkt.max(pktlen);
        self.minpkt = self.minpkt.min(pktlen);
        self.maxblk = self.maxblk.max(samples as u16);
        self.minblk = self.minblk.min(samples as u16);
        self.duration += samples;

        self.bw.write_buf(&pkt.get_buffer())?;
        Ok(())
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
//todo: write MD5 somehow?
        self.bw.seek(SeekFrom::Start(8))?;
        self.bw.write_u16be(self.minblk)?;
        self.bw.write_u16be(self.maxblk)?;
        self.bw.write_u24be(self.minpkt as u32)?;
        self.bw.write_u24be(self.maxpkt as u32)?;
        self.bw.seek(SeekFrom::Current(3))?;
        self.bw.write_byte((self.bits << 4) | (((self.duration >> 32) as u8) & 0xF))?;
        self.bw.write_u32be(self.duration as u32)?;
        Ok(())
    }
}

impl<'a> NAOptionHandler for FLACMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct FLACMuxerCreator {}

impl MuxerCreator for FLACMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(FLACMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "flac" }
    fn get_capabilities(&self) -> MuxerCapabilities { MuxerCapabilities::SingleAudio("flac") }
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_flac_muxer() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.flac
        let dec_config = DecoderTestParams {
                demuxer:        "flac",
                in_name:        "assets/LLaudio/luckynight.flac",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        llaudio_register_all_muxers(&mut mux_reg);
/*        let enc_config = EncoderTestParams {
                muxer:      "flac",
                enc_name:   "",
                out_name:   "muxed.flac",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "flac", &mux_reg,
                          [0x77afb7c0, 0x84d2bd87, 0x6e028092, 0x7db7c72e]);
    }
}

