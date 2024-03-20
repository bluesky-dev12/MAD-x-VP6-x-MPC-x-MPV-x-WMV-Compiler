use nihav_core::muxers::*;
mod audiostream;
use audiostream::*;
mod videostream;
use videostream::*;

trait RMWriterHelper {
    fn write_chunk(&mut self, id: &[u8], size: u32, version: u16) -> MuxerResult<()>;
    fn write_string(&mut self, data: &[u8]) -> MuxerResult<()>;
    fn patch_value(&mut self, val: u32, off: u64) -> MuxerResult<()>;
}

impl<'a> RMWriterHelper for ByteWriter<'a> {
    fn write_chunk(&mut self, id: &[u8], size: u32, version: u16) -> MuxerResult<()> {
        self.write_buf(id)?;
        self.write_u32be(size)?;
        self.write_u16be(version)?;
        Ok(())
    }
    fn write_string(&mut self, data: &[u8]) -> MuxerResult<()> {
        validate!(data.len() < 256);
        self.write_byte(data.len() as u8)?;
        self.write_buf(data)?;
        Ok(())
    }
    fn patch_value(&mut self, val: u32, off: u64) -> MuxerResult<()> {
        let cur_pos = self.tell();
        self.seek(SeekFrom::Start(off))?;
        self.write_u32be(val)?;
        self.seek(SeekFrom::Start(cur_pos))?;
        Ok(())
    }
}

pub trait RMStreamWriter {
    fn write_header(&mut self, bw: &mut ByteWriter, astream: &NAStream) -> MuxerResult<()>;
    fn queue_packet(&mut self, pkt: NAPacket, ms: u32) -> bool;
    fn get_packet(&mut self) -> Option<(Vec<u8>, u32, bool)>;
    fn flush(&mut self);
    fn finish(&mut self, bw: &mut ByteWriter) -> MuxerResult<()>;
    fn set_pkt_size(&mut self, pkt_size: usize);
}

#[derive(Clone,Copy)]
struct IndexEntry {
    time:       u32,
    pos:        u64,
    pkt_no:     u32,
}

struct RMStream {
    packetiser:     Box<dyn RMStreamWriter>,
    stream_id:      u16,
    mdpr_pos:       u64,
    npkts:          usize,
    data_size:      usize,
    max_pkt_size:   usize,
    time:           u32,
    cur_time:       u32,
    keyframe:       bool,
    audio:          bool,
    index:          Vec<IndexEntry>,
    debug:          bool,
}

impl RMStream {
    fn new(strno: usize, stream: &NAStream, pkt_size: usize) -> MuxerResult<Self> {
        let packetiser = match stream.get_media_type() {
                StreamType::Video => create_video_stream(stream, pkt_size)?,
                StreamType::Audio => create_audio_stream(stream)?,
                _ => Box::new(DummyStreamWriter{}),
            };
        Ok(Self{
            packetiser,
            stream_id:      strno as u16,
            mdpr_pos:       0,
            npkts:          0,
            data_size:      0,
            max_pkt_size:   0,
            time:           0,
            cur_time:       0,
            keyframe:       false,
            audio:          false,
            index:          Vec::new(),
            debug:          false,
        })
    }
    fn write_mdpr(&mut self, bw: &mut ByteWriter, strm: &NAStream) -> MuxerResult<()> {
        self.mdpr_pos = bw.tell();

        bw.write_chunk(b"MDPR", 0, 0)?;
        bw.write_u16be(self.stream_id)?;
        bw.write_u32be(0)?; //max br
        bw.write_u32be(0)?; //avg br
        bw.write_u32be(0)?; //max ps
        bw.write_u32be(0)?; //avg ps
        bw.write_u32be(0)?; //num packets
        bw.write_u32be(0)?; //duration
        bw.write_u32be(0)?; //preroll

        match strm.get_media_type() {
            StreamType::Video => {
                bw.write_string(b"The Video Stream")?;
                bw.write_string(b"video/x-pn-realvideo")?;
            },
            StreamType::Audio => {
                bw.write_string(b"The Audio Stream")?;
                bw.write_string(b"audio/x-pn-realaudio")?;
                self.audio = true;
            },
            _ => {
                bw.write_string(b"some other stream")?;
                bw.write_string(b"data")?;
            },
        };
        bw.write_u32be(0)?; //extradata size
        let edata_start = bw.tell();
        self.packetiser.write_header(bw, strm)?;
        let edata_end = bw.tell();
        bw.patch_value((edata_end - edata_start) as u32, edata_start - 4)?;

        patch_size(bw, self.mdpr_pos)?;

        Ok(())
    }
    fn write_packet(&mut self, bw: &mut ByteWriter, pkt: NAPacket, pkt_no: &mut u32) -> MuxerResult<()> {
        if let Some(pts) = pkt.get_pts() {
            let (tb_num, tb_den) = pkt.get_stream().get_timebase();
            let ms = NATimeInfo::ts_to_time(pts, 1000, tb_num, tb_den) as u32;
            self.time = self.time.max(ms);
            self.cur_time = ms;
        }
        self.keyframe = pkt.keyframe || self.audio;
        self.packetiser.queue_packet(pkt, self.cur_time);
        self.write_packets(bw, pkt_no)
    }
    fn write_packets(&mut self, bw: &mut ByteWriter, pkt_no: &mut u32) -> MuxerResult<()> {
        while let Some((data, ts, first)) = self.packetiser.get_packet() {
            validate!(data.len() < 65000);
            if self.keyframe && first {
                self.index.push(IndexEntry{ time: ts, pos: bw.tell(), pkt_no: *pkt_no });
            }
            let is_keyframe = self.keyframe && (!self.audio || first);
            if self.debug {
                println!(" writing packet for stream {} size {}{}", self.stream_id, data.len(), if is_keyframe { " kf" } else { "" });
            }
            bw.write_u16be(0)?; //version;
            bw.write_u16be((data.len() + 12) as u16)?;
            bw.write_u16be(self.stream_id)?;
            bw.write_u32be(ts)?;
            bw.write_byte(0)?; //packet group
            bw.write_byte(if is_keyframe { 0x2 } else { 0x0 })?;
            bw.write_buf(&data)?;

            self.npkts += 1;
            self.data_size += data.len();

            *pkt_no += 1;
        }
        Ok(())
    }
    fn finish(&mut self, bw: &mut ByteWriter, pkt_no: &mut u32) -> MuxerResult<()> {
        self.packetiser.flush();
        self.write_packets(bw, pkt_no)?;

        let pos = bw.tell();
        bw.seek(SeekFrom::Start(self.mdpr_pos + 12))?;
        bw.write_u32be(if self.time > 0 { (self.data_size * 1000 / (self.time as usize)) as u32 } else { 0 })?;
        bw.write_u32be(if self.time > 0 { (self.data_size * 1000 / (self.time as usize)) as u32 } else { 0 })?;
        bw.write_u32be(self.max_pkt_size as u32)?;
        bw.write_u32be(if self.npkts > 0 { (self.data_size / self.npkts) as u32 } else { 0 })?;
        bw.seek(SeekFrom::Current(8))?;
        bw.write_u32be(self.time)?;

        bw.seek(SeekFrom::Start(pos))?;
        Ok(())
    }
}

struct RMMuxer<'a> {
    bw:             &'a mut ByteWriter<'a>,
    streams:        Vec<RMStream>,
    data_pos:       u64,
    num_chunks:     u32,
    cur_packet:     u32,

    debug:          bool,
    vpkt_size:      usize,
}

impl<'a> RMMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
            streams:    Vec::new(),
            data_pos:   0,
            num_chunks: 0,
            cur_packet: 0,

            debug:      false,
            vpkt_size:  1400,
        }
    }
    fn write_index(&mut self) -> MuxerResult<()> {
        let mut indx_pos = 0x38;

        for stream in self.streams.iter() {
            let cur_pos = self.bw.tell();
            self.bw.patch_value(cur_pos as u32, indx_pos)?;
            indx_pos = cur_pos + 16;

            let idx_size = 10 + 10 + stream.index.len() * 14;
            self.bw.write_chunk(b"INDX", idx_size as u32, 0)?;
            self.bw.write_u32be(stream.index.len() as u32)?;
            self.bw.write_u16be(stream.stream_id)?;
            self.bw.write_u32be(0)?; // next index position
            for entry in stream.index.iter() {
                self.bw.write_u16be(0)?; // version
                self.bw.write_u32be(entry.time)?;
                self.bw.write_u32be(entry.pos as u32)?;
                self.bw.write_u32be(entry.pkt_no)?;
            }

            self.num_chunks += 1;
        }

        Ok(())
    }
    fn update_prop(&mut self) -> MuxerResult<()> {
        let mut data_size = 0;
        let mut npkts = 0;
        let mut max_pkt_size = 0;
        let mut time = 0;

        for stream in self.streams.iter() {
            data_size += stream.data_size;
            time = time.max(stream.time);
            npkts += stream.npkts;
            max_pkt_size = max_pkt_size.max(stream.max_pkt_size);
        }

        if npkts > 0 && time > 0 {
            let cur_pos = self.bw.tell();

            let bitrate = (data_size * 1000 / (time as usize)) as u32;
            self.bw.seek(SeekFrom::Start(28))?;
            self.bw.write_u32be(bitrate)?;
            self.bw.write_u32be(bitrate)?;
            self.bw.write_u32be(max_pkt_size as u32)?;
            self.bw.write_u32be((data_size / npkts) as u32)?;
            self.bw.write_u32be(npkts as u32)?;
            self.bw.write_u32be(time)?;

            self.bw.seek(SeekFrom::Start(cur_pos))?;
        }

        self.bw.patch_value(self.data_pos as u32, 0x3C)?;

        Ok(())
    }
}

fn patch_size(bw: &mut ByteWriter, pos: u64) -> MuxerResult<()> {
    let end = bw.tell();
    bw.patch_value((end - pos) as u32, pos + 4)
}

impl<'a> MuxCore<'a> for RMMuxer<'a> {
    fn create(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        if strmgr.get_num_streams() == 0 {
            return Err(MuxerError::InvalidArgument);
        }
        if strmgr.get_num_streams() > 100 {
            return Err(MuxerError::UnsupportedFormat);
        }

        self.bw.write_chunk(b".RMF", 18, 0)?;
        self.bw.write_u32be(0)?; // container version
        self.bw.write_u32be(0)?; // number of chunks

        self.num_chunks = 1;
        let prop_start = self.bw.tell();
        self.bw.write_chunk(b"PROP", 0, 0)?;
        self.bw.write_u32be(0)?; //max br
        self.bw.write_u32be(0)?; //avg br
        self.bw.write_u32be(0)?; //max ps
        self.bw.write_u32be(0)?; //avg ps
        self.bw.write_u32be(0)?; //num packets
        self.bw.write_u32be(0)?; //duration
        self.bw.write_u32be(0)?; //preroll
        self.bw.write_u32be(0)?; //index offset
        self.bw.write_u32be(0)?; //data offset
        self.bw.write_u16be(strmgr.get_num_streams() as u16)?;
        self.bw.write_u16be(0)?; // flags
        patch_size(self.bw, prop_start)?;

        self.streams.clear();
        for (strno, strm) in strmgr.iter().enumerate() {
            let mut swriter = RMStream::new(strno, &strm, self.vpkt_size)?;
            swriter.write_mdpr(self.bw, &strm)?;
            self.streams.push(swriter);
            self.num_chunks += 1;
        }

        self.data_pos = self.bw.tell();
        self.bw.write_chunk(b"DATA", 0, 0)?;
        self.bw.write_u32be(0)?; //num packets
        self.bw.write_u32be(0)?; //next data chunk
        self.num_chunks += 1;

        Ok(())
    }
    fn mux_frame(&mut self, _strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        if self.data_pos == 0 {
            return Err(MuxerError::NotCreated);
        }
        let stream = pkt.get_stream();
        let str_num = stream.get_num();
        if str_num > self.streams.len() {
            return Err(MuxerError::UnsupportedFormat);
        }
        self.streams[str_num].write_packet(self.bw, pkt, &mut self.cur_packet)?;

        Ok(())
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
        if self.data_pos == 0 {
            return Err(MuxerError::NotCreated);
        }
        let mut tot_npkts = 0;
        for stream in self.streams.iter_mut() {
            stream.finish(self.bw, &mut self.cur_packet)?;
            tot_npkts += stream.npkts;
        }

        let data_size = self.bw.tell() - self.data_pos;
        self.bw.patch_value(data_size as u32, self.data_pos + 4)?;
        self.bw.patch_value(tot_npkts as u32, self.data_pos + 10)?;

        self.write_index()?;
        self.update_prop()?;

        self.bw.patch_value(self.num_chunks, 14)?;
        Ok(())
    }
}

const DEBUG_OPTION: &str = "debug";
const VPKT_SIZE_OPTION: &str = "vpkt_size";

const MUXER_OPTIONS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: DEBUG_OPTION, description: "print some muxer statistics",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: VPKT_SIZE_OPTION, description: "video packet maximum size",
        opt_type: NAOptionDefinitionType::Int(Some(1024), Some(14800)) },
];

impl<'a> NAOptionHandler for RMMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { MUXER_OPTIONS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in MUXER_OPTIONS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        DEBUG_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.debug = val;
                                for stream in self.streams.iter_mut() {
                                    stream.debug = val;
                                }
                            }
                        },
                        VPKT_SIZE_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.vpkt_size = intval as usize;
                                for stream in self.streams.iter_mut() {
                                    stream.packetiser.set_pkt_size(self.vpkt_size);
                                }
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
            DEBUG_OPTION => Some(NAValue::Bool(self.debug)),
            VPKT_SIZE_OPTION => Some(NAValue::Int(self.vpkt_size as i64)),
            _ => None,
        }
    }
}

pub struct RealMediaMuxerCreator {}

impl MuxerCreator for RealMediaMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(RMMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "realmedia" }
    fn get_capabilities(&self) -> MuxerCapabilities { MuxerCapabilities::Universal }
}

struct RAMuxer<'a> {
    bw:             &'a mut ByteWriter<'a>,
    sw:             Option<Box<dyn RMStreamWriter>>,
}

impl<'a> RAMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
            sw:     None,
        }
    }
}

impl<'a> MuxCore<'a> for RAMuxer<'a> {
    fn create(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        if strmgr.get_num_streams() != 1 {
            return Err(MuxerError::InvalidArgument);
        }
        let astream = strmgr.get_stream(0).unwrap();
        if astream.get_media_type() != StreamType::Audio {
            return Err(MuxerError::InvalidArgument);
        }
        self.sw = Some(create_audio_stream(&astream)?);
        if let Some(ref mut sw) = self.sw {
            sw.write_header(self.bw, &astream)?;
        }
        Ok(())
    }
    fn mux_frame(&mut self, _strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        if let Some(ref mut sw) = self.sw {
            sw.queue_packet(pkt, 0);
            while let Some((data, _, _)) = sw.get_packet() {
                self.bw.write_buf(&data)?;
            }
            Ok(())
        } else {
            Err(MuxerError::NotCreated)
        }
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
        if let Some(ref mut sw) = self.sw {
            sw.finish(self.bw)?;
        }
        Ok(())
    }
}

impl<'a> NAOptionHandler for RAMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct RealAudioMuxerCreator {}

impl MuxerCreator for RealAudioMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(RAMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "realaudio" }
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
    fn test_rm_muxer() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        // sample from a private collection
        let dec_config = DecoderTestParams {
                demuxer:        "realmedia",
                in_name:        "assets/RV/rv30_weighted_mc.rm",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "realmedia",
                enc_name:   "",
                out_name:   "muxed.rm",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "realmedia", &mux_reg,
                          [0x9bc90ab0, 0x6b8c42f7, 0xaf81e8bf, 0x7c76ec57]);
    }

    #[test]
    fn test_ra_muxer_v3() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        //test sample: https://samples.mplayerhq.hu/real/RA/14_4/drummers.14.ra
        let dec_config = DecoderTestParams {
                demuxer:        "realaudio",
                in_name:        "assets/RV/drummers.14.ra",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "realaudio",
                enc_name:   "",
                out_name:   "v3.ra",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "realaudio", &mux_reg,
                          [0x8101a484, 0xf5d80805, 0x24577596, 0x9b27262f]);
    }
    #[test]
    fn test_ra_muxer_v4() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        //test sample: https://samples.mplayerhq.hu/real//RA/ra_with_comment_field/diemusik.ra
        let dec_config = DecoderTestParams {
                demuxer:        "realaudio",
                in_name:        "assets/RV/diemusik.ra",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "realaudio",
                enc_name:   "",
                out_name:   "v4.ra",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "realaudio", &mux_reg,
                          [0x33665ec3, 0x69b68ea2, 0x08d4b138, 0x318e305f]);
    }
    #[test]
    fn test_ra_muxer_sipro() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        //test sample: https://samples.mplayerhq.hu/real/AC-sipr/autahi-vox.rm
        let dec_config = DecoderTestParams {
                demuxer:        "realmedia",
                in_name:        "assets/RV/autahi-vox.rm",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "realaudio",
                enc_name:   "",
                out_name:   "v4-sipro.ra",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "realaudio", &mux_reg,
                          [0x08bd496d, 0x5f35d7ae, 0xe9c93c50, 0x9e803f76]);
    }
    #[test]
    fn test_ra_muxer_v5() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        //test sample: https://samples.mplayerhq.hu/real/AC-cook/cook_5.1/multichannel.rma
        let dec_config = DecoderTestParams {
                demuxer:        "realmedia",
                in_name:        "assets/RV/multichannel.rma",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "realaudio",
                enc_name:   "",
                out_name:   "v5.ra",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "realaudio", &mux_reg,
                          [0x52f42c49, 0x90ac79a7, 0x275a465f, 0x7a6f3659]);
    }
    #[test]
    fn test_rm_muxer_aac() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        //sample from a private collection
        let dec_config = DecoderTestParams {
                demuxer:        "realmedia",
                in_name:        "assets/RV/rv40_weighted_mc_2.rmvb",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "realmedia",
                enc_name:   "",
                out_name:   "aac.ram",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "realmedia", &mux_reg,
                          [0x8392bb8c, 0xeb8f4d04, 0x25262829, 0x63b2fda7]);
    }
    #[test]
    fn test_rm_muxer_ralf() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        //sample from a private collection
        let dec_config = DecoderTestParams {
                demuxer:        "realmedia",
                in_name:        "assets/RV/rv40_ralf.rmvb",
                limit:          None,
                stream_type:    StreamType::None,
                dmx_reg, dec_reg: RegisteredDecoders::new(),
            };
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        /*let enc_config = EncoderTestParams {
                muxer:      "realmedia",
                enc_name:   "",
                out_name:   "ralf.ram",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "realmedia", &mux_reg,
                          [0xe90893eb, 0x8634642c, 0xef679ac4, 0x2e89314a]);
    }
}
