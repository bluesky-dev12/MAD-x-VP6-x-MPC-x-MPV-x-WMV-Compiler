use nihav_core::muxers::*;
use nihav_registry::register::*;

#[derive(Clone,Copy)]
struct IdxEntry {
    stream:     u32,
    stype:      StreamType,
    key:        bool,
    pos:        u32,
    len:        u32,
}

#[derive(Clone,Copy)]
struct AVIStream {
    strh_pos:   u64,
    nframes:    u32,
    is_video:   bool,
    max_size:   u32,
    pal_change: bool,
}

struct AVIMuxer<'a> {
    bw:             &'a mut ByteWriter<'a>,
    index:          Vec<IdxEntry>,
    video_str:      Option<usize>,
    video_id:       u32,
    data_pos:       u64,
    stream_info:    Vec<AVIStream>,
    pal_pos:        Vec<u32>,
}

impl<'a> AVIMuxer<'a> {
    fn new(bw: &'a mut ByteWriter<'a>) -> Self {
        Self {
            bw,
            index:          Vec::new(),
            video_str:      None,
            video_id:       0,
            data_pos:       0,
            stream_info:    Vec::with_capacity(2),
            pal_pos:        Vec::with_capacity(2),
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

fn write_chunk_hdr(bw: &mut ByteWriter, stype: StreamType, str_no: u32) -> MuxerResult<()> {
    bw.write_byte(b'0' + ((str_no / 10) as u8))?;
    bw.write_byte(b'0' + ((str_no % 10) as u8))?;
    match stype {
        StreamType::Video => { bw.write_buf(b"dc")?; },
        StreamType::Audio => { bw.write_buf(b"wb")?; },
        StreamType::Subtitles => { bw.write_buf(b"tx")?; },
        _ => return Err(MuxerError::UnsupportedFormat),
    };
    Ok(())
}

impl<'a> MuxCore<'a> for AVIMuxer<'a> {
    #[allow(clippy::unreadable_literal)]
    #[allow(clippy::cast_lossless)]
    fn create(&mut self, strmgr: &StreamManager) -> MuxerResult<()> {
        if strmgr.get_num_streams() == 0 {
            return Err(MuxerError::InvalidArgument);
        }
        if strmgr.get_num_streams() > 99 {
            return Err(MuxerError::UnsupportedFormat);
        }
        for (str_no, strm) in strmgr.iter().enumerate() {
            if strm.get_media_type() == StreamType::Video {
                self.video_str = Some(str_no);
                self.video_id  = strm.id;
                break;
            }
        }
        let (vinfo, tb_num, tb_den) = if let Some(str_id) = self.video_str {
                let vstr = strmgr.get_stream(str_id).unwrap();
                (vstr.get_info(), vstr.tb_num, vstr.tb_den)
            } else {
                (NACodecInfo::new_dummy(), 0, 1)
            };
        let hdrl_pos = self.bw.tell() + 20;
        self.bw.write_buf(b"RIFF\0\0\0\0AVI LIST\0\0\0\0hdrlavih")?;
        self.bw.write_u32le(56)?; // avih size
        let ms_per_frame = NATimeInfo::ts_to_time(1, 1000000, tb_num, tb_den);
        self.bw.write_u32le(ms_per_frame as u32)?;
        self.bw.write_u32le(0)?; // max transfer rate
        self.bw.write_u32le(0)?; // padding granularity
        self.bw.write_u32le(0)?; // flags
        self.bw.write_u32le(0)?; // total frames
        self.bw.write_u32le(0)?; // initial frames
        self.bw.write_u32le(strmgr.get_num_streams() as u32)?;
        self.bw.write_u32le(0)?; // suggested buffer size
        if let NACodecTypeInfo::Video(ref vinfo) = vinfo.get_properties() {
            self.bw.write_u32le(vinfo.width as u32)?;
            self.bw.write_u32le(vinfo.height as u32)?;
        } else {
            self.bw.write_u32le(0)?;
            self.bw.write_u32le(0)?;
        }
        self.bw.write_u32le(0)?; // reserved
        self.bw.write_u32le(0)?; // reserved
        self.bw.write_u32le(0)?; // reserved
        self.bw.write_u32le(0)?; // reserved

        self.pal_pos.clear();
        self.pal_pos.resize(strmgr.get_num_streams(), 0);
        for (strno, strm) in strmgr.iter().enumerate() {
            let strl_pos = self.bw.tell() + 8;
            self.bw.write_buf(b"LIST\0\0\0\0strlstrh")?;
            self.bw.write_u32le(56)?; // strh size

            match strm.get_media_type() {
                StreamType::Video => {
                    self.bw.write_buf(b"vids")?;
                    let fcc = find_avi_fourcc(strm.get_info().get_name());
                    if fcc.is_none() {
                        return Err(MuxerError::UnsupportedFormat);
                    }
                    self.bw.write_buf(&fcc.unwrap_or([0; 4]))?;
                    let vinfo = strm.get_info().get_properties().get_video_info().unwrap();
                    if vinfo.width >= (1 << 16) || vinfo.height >= (1 << 16) {
                        return Err(MuxerError::UnsupportedFormat);
                    }
                },
                StreamType::Audio => {
                    self.bw.write_buf(b"auds")?;
                    self.bw.write_u32le(0)?;
                },
                StreamType::Subtitles => {
                    self.bw.write_buf(b"txts")?;
                    self.bw.write_u32le(0)?;
                },
                _ => return Err(MuxerError::UnsupportedFormat),
            };
            self.stream_info.push(AVIStream {
                    strh_pos:   self.bw.tell(),
                    is_video:   strm.get_media_type() == StreamType::Video,
                    nframes:    0,
                    max_size:   0,
                    pal_change: false,
                });

            self.bw.write_u32le(0)?; // flags
            self.bw.write_u16le(0)?; // priority
            self.bw.write_u16le(0)?; // language
            self.bw.write_u32le(0)?; // initial frames
            self.bw.write_u32le(strm.tb_num)?;
            self.bw.write_u32le(strm.tb_den)?;
            self.bw.write_u32le(0)?; // start
            self.bw.write_u32le(0)?; // length
            self.bw.write_u32le(0)?; // suggested buffer size
            self.bw.write_u32le(0)?; // quality
            self.bw.write_u32le(0)?; // sample_size
            self.bw.write_u16le(0)?; // x
            self.bw.write_u16le(0)?; // y
            self.bw.write_u16le(0)?; // w
            self.bw.write_u16le(0)?; // h

            self.bw.write_buf(b"strf")?;
            self.bw.write_u32le(0)?;
            let strf_pos = self.bw.tell();
            match strm.get_media_type() {
                StreamType::Video => {
                    let vinfo = strm.get_info().get_properties().get_video_info().unwrap();
                    let hdr_pos = self.bw.tell();
                    self.bw.write_u32le(0)?;
                    self.bw.write_u32le(vinfo.width as u32)?;
                    self.bw.write_u32le(vinfo.height as u32)?;
                    if !vinfo.format.palette {
                        self.bw.write_u16le(vinfo.format.components as u16)?;
                        self.bw.write_u16le(vinfo.format.get_total_depth() as u16)?;
                    } else {
                        self.bw.write_u16le(1)?;
                        self.bw.write_u16le(8)?;
                    }
                    let fcc = find_avi_fourcc(strm.get_info().get_name());
                    if fcc.is_none() {
                        return Err(MuxerError::UnsupportedFormat);
                    }
                    self.bw.write_buf(&fcc.unwrap_or([0; 4]))?;
                    self.bw.write_u32le(0)?; // image size
                    self.bw.write_u32le(0)?; // x dpi
                    self.bw.write_u32le(0)?; // y dpi
                    if vinfo.format.palette {
                        self.bw.write_u32le(256)?; // total colors
                        self.bw.write_u32le(0)?; // important colors
                        self.pal_pos[strno] = self.bw.tell() as u32;
                        for _ in 0..256 {
                            self.bw.write_u32le(0)?;
                        }
                    } else {
                        self.bw.write_u32le(0)?; // total colors
                        self.bw.write_u32le(0)?; // important colors
                    }
                    if let Some(ref edata) = strm.get_info().get_extradata() {
                        self.bw.write_buf(edata.as_slice())?;
                    }
                    let bisize = self.bw.tell() - hdr_pos;
                    self.bw.seek(SeekFrom::Current(-(bisize as i64)))?;
                    self.bw.write_u32le(bisize as u32)?;
                    self.bw.seek(SeekFrom::End(0))?;
                },
                StreamType::Audio => {
                    let ainfo = strm.get_info().get_properties().get_audio_info().unwrap();
                    let twocc = find_wav_twocc(strm.get_info().get_name());
                    if twocc.is_none() {
                        return Err(MuxerError::UnsupportedFormat);
                    }
                    self.bw.write_u16le(twocc.unwrap_or(0))?;
                    self.bw.write_u16le(ainfo.channels as u16)?;
                    self.bw.write_u32le(ainfo.sample_rate)?;
                    self.bw.write_u32le(0)?; // avg bytes per second
                    self.bw.write_u16le(ainfo.block_len as u16)?;
                    self.bw.write_u16le(ainfo.format.bits as u16)?;
                    if let Some(ref edata) = strm.get_info().get_extradata() {
                        self.bw.write_buf(edata.as_slice())?;
                    }
                },
                StreamType::Subtitles => {
                    if let Some(ref edata) = strm.get_info().get_extradata() {
                        self.bw.write_buf(edata.as_slice())?;
                    }
                },
                _ => unreachable!(),
            };
            patch_size(self.bw, strf_pos)?;
            patch_size(self.bw, strl_pos)?;
        }
        patch_size(self.bw, hdrl_pos)?;

        self.data_pos = self.bw.tell() + 8;
        self.bw.write_buf(b"LIST\0\0\0\0movi")?;

        Ok(())
    }
    fn mux_frame(&mut self, _strmgr: &StreamManager, pkt: NAPacket) -> MuxerResult<()> {
        if self.data_pos == 0 {
            return Err(MuxerError::NotCreated);
        }
        let stream = pkt.get_stream();
        let str_num = stream.get_num();
        if str_num > 99 || str_num >= self.stream_info.len() {
            return Err(MuxerError::UnsupportedFormat);
        }

        let chunk_len = pkt.get_buffer().len() as u32;

        if self.pal_pos[str_num] != 0 {
            for sdata in pkt.side_data.iter() {
                if let NASideData::Palette(_, ref pal) = sdata {
                    let cur_pos = self.bw.tell();
                    self.bw.seek(SeekFrom::Start(u64::from(self.pal_pos[str_num])))?;
                    for quad in pal.chunks(4) {
                        self.bw.write_byte(quad[2])?;
                        self.bw.write_byte(quad[1])?;
                        self.bw.write_byte(quad[0])?;
                        self.bw.write_byte(0)?;
                    }
                    self.bw.seek(SeekFrom::Start(cur_pos))?;
                    self.pal_pos[str_num] = 0;
                    break;
                }
            }
        } else {
            for sdata in pkt.side_data.iter() {
                if let NASideData::Palette(true, ref pal) = sdata {
                    //todo search for changed region
                    let start_clr = 0usize;
                    let end_clr = 256usize;
                    if start_clr < end_clr {
                        let chunk_len = ((end_clr - start_clr) as u32) * 4 + 4;
                        self.bw.write_byte(b'0' + ((str_num / 10) as u8))?;
                        self.bw.write_byte(b'0' + ((str_num % 10) as u8))?;
                        self.bw.write_buf(b"pc")?;
                        self.bw.write_u32le(chunk_len)?;
                        self.bw.write_byte(start_clr as u8)?;
                        self.bw.write_byte((end_clr - start_clr) as u8)?;
                        self.bw.write_u16le(0)?; //flags
                        self.bw.write_buf(&pal[start_clr * 4..end_clr * 4])?;
                        self.stream_info[str_num].pal_change = true;
                    }
                }
            }
        }

        self.stream_info[str_num].nframes += 1;
        self.stream_info[str_num].max_size = self.stream_info[str_num].max_size.max(chunk_len);
        self.index.push(IdxEntry {
                stream: str_num as u32,
                stype:  stream.get_media_type(),
                key:    pkt.keyframe,
                pos:    self.bw.tell() as u32,
                len:    chunk_len });
        write_chunk_hdr(self.bw, stream.get_media_type(), str_num as u32)?;
        self.bw.write_u32le(chunk_len)?;
        self.bw.write_buf(pkt.get_buffer().as_slice())?;
        if (self.bw.tell() & 1) != 0 {
            self.bw.write_byte(0)?;
        }
        Ok(())
    }
    fn flush(&mut self) -> MuxerResult<()> {
        Ok(())
    }
    fn end(&mut self) -> MuxerResult<()> {
        patch_size(self.bw, self.data_pos)?;
        if !self.index.is_empty() {
            self.bw.write_buf(b"idx1")?;
            self.bw.write_u32le((self.index.len() * 16) as u32)?;
            for item in self.index.iter() {
                write_chunk_hdr(self.bw, item.stype, item.stream)?;
                self.bw.write_u32le(if item.key { 0x10 } else { 0 })?;
                self.bw.write_u32le(item.pos)?;
                self.bw.write_u32le(item.len)?;
            }
        }
        patch_size(self.bw, 8)?;
        let mut max_frames = 0;
        let mut max_size = 0;
        for stri in self.stream_info.iter() {
            max_frames = max_frames.max(stri.nframes);
            max_size = max_size.max(stri.max_size);
            if stri.pal_change {
                self.bw.seek(SeekFrom::Start(stri.strh_pos))?;
                self.bw.write_u32le(0x00010000)?;
            }
            self.bw.seek(SeekFrom::Start(stri.strh_pos + 0x18))?;
            self.bw.write_u32le(if stri.is_video { stri.nframes } else { 0 })?;
            self.bw.write_u32le(stri.max_size)?;
        }
        self.bw.seek(SeekFrom::Start(0x30))?;
        self.bw.write_u32le(max_frames)?;
        self.bw.seek(SeekFrom::Current(8))?;
        self.bw.write_u32le(max_size)?;
        Ok(())
    }
}

impl<'a> NAOptionHandler for AVIMuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct AVIMuxerCreator {}

impl MuxerCreator for AVIMuxerCreator {
    fn new_muxer<'a>(&self, bw: &'a mut ByteWriter<'a>) -> Box<dyn MuxCore<'a> + 'a> {
        Box::new(AVIMuxer::new(bw))
    }
    fn get_name(&self) -> &'static str { "avi" }
    fn get_capabilities(&self) -> MuxerCapabilities { MuxerCapabilities::Universal }
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    #[test]
    fn test_avi_muxer() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        //test sample: https://samples.mplayerhq.hu/V-codecs/RT21/320x240/laser05.avi
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
                muxer:      "avi",
                enc_name:   "",
                out_name:   "muxed.avi",
                mux_reg, enc_reg: RegisteredEncoders::new(),
            };
        test_remuxing(&dec_config, &enc_config);*/
        test_remuxing_md5(&dec_config, "avi", &mux_reg,
                          [0xa0fb0e47, 0x412e24dd, 0x6b89711c, 0x276fb799]);
    }
}
