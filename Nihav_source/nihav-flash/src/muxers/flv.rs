use nihav_core::muxers::*;

const FLV_VCODECS: &[(&str, u8)] = &[
    ("flv263", 2),
    ("flashsv", 3),
    ("vp6f", 4),
    ("vp6a", 5),
    ("flashsv2", 6),
    ("h264", AVC_ID)
];

const NO_CODEC: u8 = 0;
const AVC_ID: u8 = 7;
const AAC_ID: u8 = 10;

fn find_audio_tag(cname: &'static str, rate: u32, channels: u8) -> MuxerResult<u8> {
    if channels != 1 && channels != 2 {
        return Err(MuxerError::InvalidArgument);
    }
    let tag = match cname {
            "flv-adpcm" => 1,
            "pcm" => 3,
            "mp3" => if rate != 8000 { 2 } else { return Ok(14); },
            "asao" => {
                if channels != 1 {
                    return Err(MuxerError::InvalidArgument);
                }
                match rate {
                    16000 => return Ok(4),
                    8000 => return Ok(5),
                    _ => 6,
                }
            },
            "alaw" => 7,
            "ulaw" => 8,
            "aac" => AAC_ID,
            "speex" => 11,
            _ => return Err(MuxerError::InvalidArgument),
        };
    match rate {
        5500 | 11025 | 22050 | 44100 => {},
        _ => return Err(MuxerError::InvalidArgument),
    };
    Ok(tag)
}

trait FLVPropertyWriter {
    fn write_property_num(&mut self, name: &str, val: f64) -> MuxerResult<()>;
    fn write_property_bool(&mut self, name: &str, val: bool) -> MuxerResult<()>;
}

impl<'a> FLVPropertyWriter for ByteWriter<'a> {
    fn write_property_num(&mut self, name: &str, val: f64) -> MuxerResult<()> {
        self.write_u16be(name.len() as u16)?;
        self.write_buf(name.as_bytes())?;
        self.write_byte(0)?;
        self.write_f64be(val)?;
        Ok(())
    }
    fn write_property_bool(&mut self, name: &str, val: bool) -> MuxerResult<()> {
        self.write_u16be(name.len() as u16)?;
        self.write_buf(name.as_bytes())?;
        self.write_byte(1)?;
        self.write_byte(val as u8)?;
        Ok(())
    }
}

macro_rules! write_packet {
    ($self: expr, $pkt_type: expr, $ts: expr, $code: block) => {
        let start = $self.bw.tell();
        $self.bw.write_byte($pkt_type)?;
        $self.bw.write_u24be(0)?;
        $self.bw.write_u24be($ts)?;
        $self.bw.write_byte(($ts >> 24) as u8)?;
        $self.bw.write_u24be(0)?;

        $code

        let end = $self.bw.tell();
        let size = end - start - 11;
        $self.bw.seek(SeekFrom::Start(start + 1))?;
        $self.bw.write_u24be(size as u32)?;
        $self.bw.seek(SeekFrom::Start(end))?;
        $self.bw.write_u32be((size + 11) as u32)?;
    }
}

struct FLVMuxer<'a> {
    bw:     &'a mut ByteWriter<'a>,
    atag:   u8,
    ahdr:   u8,
    vtag:   u8,
    vp6b:   u8,
    time:   u32,
    dpos:   u64,
}

impl<'a> FLVMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
            atag:   NO_CODEC,
            ahdr:   0,
            vtag:   NO_CODEC,
            vp6b:   0,
            time:   0,
            dpos:   0,
        }
    }
    fn write_metadata(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        write_packet!(self, 18, 0, {
            self.bw.write_buf(b"\x02\x00\x0AonMetaData\x08\x00\x00\x00\x00")?;
            for stream in strmgr.iter() {
                match stream.get_info().get_properties() {
                    NACodecTypeInfo::Video(ref vinfo) => {
                        self.bw.write_property_num("width", vinfo.width as f64)?;
                        self.bw.write_property_num("height", vinfo.height as f64)?;
                        self.bw.write_property_num("videocodecid", self.vtag as f64)?;
                    },
                    NACodecTypeInfo::Audio(ref ainfo) => {
                        self.bw.write_property_num("audiosamplerate", ainfo.sample_rate as f64)?;
                        self.bw.write_property_bool("stereo", ainfo.channels == 2)?;
                        self.bw.write_property_num("audiocodecid", self.atag as f64)?;
                    },
                    _ => {},
                };
            }
            self.bw.write_property_num("duration", 0.0)?;
            self.dpos = self.bw.tell() - 8;
            self.bw.write_u16be(0)?;
            self.bw.write_byte(9)?;
        });

        Ok(())
    }
}

impl<'a> MuxCore<'a> for FLVMuxer<'a> {
    fn create(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        if strmgr.get_num_streams() == 0 || strmgr.get_num_streams() > 2 {
            return Err(MuxerError::InvalidArgument);
        }

        let mut astream = None;
        let mut vstream = None;
        for stream in strmgr.iter() {
            let cname = stream.get_info().get_name();
            match stream.get_media_type() {
                StreamType::Video => {
                    vstream = Some(stream.clone());
                    if self.vtag != NO_CODEC {
                        return Err(MuxerError::InvalidArgument);
                    }
                    for &(name, tag) in FLV_VCODECS.iter() {
                        if name == cname {
                            self.vtag = tag;
                            break;
                        }
                    }
                    if self.vtag == NO_CODEC {
                        return Err(MuxerError::UnsupportedFormat);
                    }
                },
                StreamType::Audio => {
                    astream = Some(stream.clone());
                    if self.atag != NO_CODEC {
                        return Err(MuxerError::InvalidArgument);
                    }
                    if let Some(ainfo) = stream.get_info().get_properties().get_audio_info() {
                        self.atag = find_audio_tag(cname, ainfo.sample_rate, ainfo.channels)?;
                        self.ahdr = (self.atag << 4) |
                            (match ainfo.sample_rate {
                                5500 => 0,
                                11025 => 1,
                                22050 => 2,
                                _     => 3,
                            } << 2) |
                            if ainfo.format.bits == 8 { 0 } else { 2 } |
                            if ainfo.channels == 1 { 0 } else { 1 };
                    } else {
                        return Err(MuxerError::InvalidArgument);
                    }
                },
                _ => return Err(MuxerError::UnsupportedFormat),
            };
        }

        self.bw.write_buf(b"FLV\x01")?;
        let flags = 0x8 | if self.atag != NO_CODEC { 4 } else { 0 } | if self.vtag != NO_CODEC { 1 } else { 0 };
        self.bw.write_byte(flags)?;
        self.bw.write_u32be(9)?;
        self.bw.write_u32be(0)?;

        self.write_metadata(strmgr)?;

        if let (true, Some(ref stream)) = (self.vtag == 4 || self.vtag == 5, &vstream) {
            if let Some(edata) = stream.get_info().get_extradata() {
                if !edata.is_empty() {
                    self.vp6b = edata[0];
                }
            }
        }

        if let (true, Some(stream)) = (self.vtag == AVC_ID, vstream) {
            if let Some(edata) = stream.get_info().get_extradata() {
                validate!(edata.len() > 4);
                write_packet!(self, 9, 0, {
                    self.bw.write_byte(0x57)?;
                    self.bw.write_byte(0x00)?;
                    self.bw.write_u24be(0)?;
                    self.bw.write_buf(&edata[4..])?;
                });
            }
        }
        if let (true, Some(stream)) = (self.atag == AAC_ID, astream) {
            if let Some(edata) = stream.get_info().get_extradata() {
                write_packet!(self, 8, 0, {
                    self.bw.write_byte(self.ahdr)?;
                    self.bw.write_byte(0x00)?;
                    self.bw.write_buf(&edata)?;
                });
            }
        }

        Ok(())
    }
    fn mux_frame(&mut self, _strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        let stream = pkt.get_stream();
        let pts = pkt.get_pts().unwrap_or(0);
        let ms = NATimeInfo::ts_to_time(pts, 1000, pkt.ts.tb_num, pkt.ts.tb_den) as u32;
        self.time = self.time.max(ms);
        match stream.get_media_type() {
            StreamType::Video => {
                write_packet!(self, 9, ms, {
                    let hdr = self.vtag | if pkt.keyframe { 0x10 } else { 0x20 };
                    self.bw.write_byte(hdr)?;
                    match self.vtag {
                        4 | 5 => {
                            self.bw.write_byte(self.vp6b)?;
                        },
                        AVC_ID => {
                            self.bw.write_byte(1)?;
                            let cms = NATimeInfo::ts_to_time(pkt.get_pts().unwrap_or(pts), 1000, pkt.ts.tb_num, pkt.ts.tb_den) as u32;
                            let cts = cms.wrapping_sub(ms) << 8 >> 8;
                            self.bw.write_u24be(cts)?;
                        },
                        _ => {},
                    };
                    self.bw.write_buf(&pkt.get_buffer())?;
                });
            },
            StreamType::Audio => {
                write_packet!(self, 8, ms, {
                    self.bw.write_byte(self.ahdr)?;
                    if self.atag == AAC_ID {
                        self.bw.write_byte(1)?;
                    }
                    self.bw.write_buf(&pkt.get_buffer())?;
                });
            },
            _ => return Err(MuxerError::InvalidData),
        };
        Ok(())
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
        self.bw.seek(SeekFrom::Start(self.dpos))?;
        self.bw.write_f64be((self.time as f64) / 1000.0)?;
        Ok(())
    }
}

impl<'a> NAOptionHandler for FLVMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct FLVMuxerCreator {}

impl MuxerCreator for FLVMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(FLVMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "flv" }
    fn get_capabilities(&self) -> MuxerCapabilities { MuxerCapabilities::SingleVideoAndAudio("any", "any") }
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_flv_muxer() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        // sample: https://samples.mplayerhq.hu/A-codecs/Nelly_Moser/input.flv
        let dec_config = DecoderTestParams {
                demuxer:        "flv",
                in_name:        "assets/Flash/input.flv",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        flash_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "flv",
                enc_name:   "",
                out_name:   "muxed.flv",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "flv", &mux_reg,
                          [0xc777b605, 0x5777919d, 0x47996fe8, 0xf5e8d64f]);
    }
}
