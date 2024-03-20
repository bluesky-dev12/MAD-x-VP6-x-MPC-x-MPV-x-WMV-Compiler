use nihav_core::muxers::*;

struct GIFMuxer<'a> {
    bw:             &'a mut ByteWriter<'a>,
    single:         bool,
    gif87:          bool,
    pal_written:    bool,
    nloops:         u16,
}

impl<'a> GIFMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
            single:         false,
            gif87:          false,
            pal_written:    false,
            nloops:         0,
        }
    }
    fn write_pal(&mut self, pal: &[u8; 1024]) -> MuxerResult<()> {
        let mut nclr = 256;
        for quad in pal.chunks_exact(4).rev() {
            if quad[0] == 0 && quad[1] == 0 && quad[2] == 0 {
                nclr -= 1;
            } else {
                break;
            }
        }
        let mut pal_bits = 1;
        while (1 << pal_bits) < nclr {
            pal_bits += 1;
        }
        self.bw.write_byte(0xF0 | (pal_bits - 1))?;
        self.bw.write_byte(0)?; // background colour index
        self.bw.write_byte(0)?; // aspect ratio
        for quad in pal.chunks_exact(4).take(1 << pal_bits) {
            self.bw.write_buf(&quad[..3])?;
        }
        Ok(())
    }
}

impl<'a> MuxCore<'a> for GIFMuxer<'a> {
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
        if vinfo.width > 65535 || vinfo.height > 65535 || !vinfo.format.palette {
            return Err(MuxerError::UnsupportedFormat);
        }

        if self.gif87 {
            self.single = true;
            self.bw.write_buf(b"GIF87a")?;
        } else {
            self.bw.write_buf(b"GIF89a")?;
        }
        self.bw.write_u16le(vinfo.width as u16)?;
        self.bw.write_u16le(vinfo.height as u16)?;

        Ok(())
    }
    fn mux_frame(&mut self, strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        if self.bw.tell() == 0 {
            return Err(MuxerError::NotCreated);
        }
        if !self.pal_written {
            let info = strmgr.get_stream(0).unwrap().get_info();
            let mut tr_idx = None;
            if let Some(ref edata) = info.get_extradata() {
                if edata.len() == 1 {
                    tr_idx = Some(edata[0]);
                } else if edata.len() >= 3 {
                    self.bw.write_buf(edata)?;
                    self.pal_written = true;
                }
            }
            if !self.pal_written {
                let mut pal_found = false;
                for sdata in pkt.side_data.iter() {
                    if let NASideData::Palette(_, ref pal) = sdata {
                        self.write_pal(pal,)?;
                        pal_found = true;
                        break;
                    }
                }
                if !pal_found {
                    return Err(MuxerError::InvalidArgument);
                }
            }
            self.pal_written = true;

            if !self.single {
                let vstr = strmgr.get_stream(0).unwrap();

                let delay = NATimeInfo::ts_to_time(1, 100, vstr.tb_num, vstr.tb_den) as u16;
                self.bw.write_byte(0x21)?; // graphic control
                self.bw.write_byte(0xF9)?; // graphic control extension
                self.bw.write_byte(4)?;    // block size
                self.bw.write_byte(if tr_idx.is_some() { 1 } else { 0 })?;    // flags
                self.bw.write_u16le(delay)?;
                self.bw.write_byte(tr_idx.unwrap_or(0))?; // transparent colour index
                self.bw.write_byte(0x00)?; // block terminator

                self.bw.write_byte(0x21)?; // graphic control
                self.bw.write_byte(0xFF)?; // application extension
                let app_id = b"NETSCAPE2.0";
                self.bw.write_byte(app_id.len() as u8)?;
                self.bw.write_buf(app_id)?;
                self.bw.write_byte(3)?; // application data block length
                self.bw.write_byte(0x01)?;
                self.bw.write_u16le(self.nloops)?;
                self.bw.write_byte(0x00)?; // block terminator
            }
        } else if self.single { // just one frame is expected
            return Err(MuxerError::InvalidArgument);
        }

        // buffer is supposed to have all the data starting from image descriptor
        let src = pkt.get_buffer();
        self.bw.write_buf(&src)?;
        Ok(())
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
        self.bw.write_byte(0x3B)?; // GIF terminator
        Ok(())
    }
}

const MUXER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: "gif87", description: "Create GIF87 image",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: "single", description: "Create single image",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: "loops", description: "Number of times to loop the animation",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(65535)) },
];

impl<'a> NAOptionHandler for GIFMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { MUXER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in MUXER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        "gif87" => {
                            if let NAValue::Bool(bval) = option.value {
                                self.gif87 = bval;
                            }
                        },
                        "single" => {
                            if let NAValue::Bool(bval) = option.value {
                                self.single = bval;
                            }
                        },
                        "loops" => {
                            if let NAValue::Int(ival) = option.value {
                                self.nloops = ival as u16;
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
            "gif87" => Some(NAValue::Bool(self.gif87)),
            "single" => Some(NAValue::Bool(self.single)),
            "loops" => Some(NAValue::Int(i64::from(self.nloops))),
            _ => None,
        }
    }
}

pub struct GIFMuxerCreator {}

impl MuxerCreator for GIFMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(GIFMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "gif" }
    fn get_capabilities(&self) -> MuxerCapabilities { MuxerCapabilities::SingleVideo("gif") }
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_gif_muxer() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        // sample: https://samples.mplayerhq.hu/image-samples/GIF/3D.gif
        let dec_config = DecoderTestParams {
                demuxer:        "gif",
                in_name:        "assets/Misc/3D.gif",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "gif",
                enc_name:   "",
                out_name:   "muxed.gif",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "gif", &mux_reg,
                          [0x7192b724, 0x2bc4fd05, 0xaa65f268, 0x3929e8bf]);
    }
}
