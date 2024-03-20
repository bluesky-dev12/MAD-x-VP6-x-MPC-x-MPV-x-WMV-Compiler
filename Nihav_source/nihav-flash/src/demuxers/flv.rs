use nihav_core::demuxers::*;
use nihav_core::io::bitreader::*;

const AVC_ID: u8 = 7;

struct FLVDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    vpkts:      Vec<NAPacket>,
    vtag:       Option<u8>,
    apkts:      Vec<NAPacket>,
    atag:       Option<u8>,
    vstream:    usize,
    astream:    usize,
    duration:   u64,
    width:      usize,
    height:     usize,
}

fn get_vcodec_name(tag: u8) -> DemuxerResult<&'static str> {
    match tag {
        2 => Ok("flv263"),
        3 => Ok("flashsv"),
        4 => Ok("vp6f"),
        5 => Ok("vp6a"),
        6 => Ok("flashsv2"),
        7 => Ok("h264"),
        _ => Err(DemuxerError::InvalidData),
    }
}

impl<'a> DemuxCore<'a> for FLVDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let mut tag = [0; 3];
                                          self.src.read_buf(&mut tag)?;
        validate!(&tag == b"FLV");
        let ver                         = self.src.read_byte()?;
        validate!(ver == 0 || ver == 1);
        let hdr                         = self.src.read_byte()?;
        validate!((hdr & 0xF2) == 0);
        let has_audio = (hdr & 4) != 0;
        let has_video = (hdr & 1) != 0;
        validate!(has_video || has_audio);
        let hdr_size                    = self.src.read_u32be()?;
        validate!(hdr_size >= 9);

        let first_prev_tag              = self.src.peek_u32be()?;
        validate!(first_prev_tag == 0);

        while (self.vtag.is_some() != has_video) || (self.atag.is_some() != has_audio) {
            self.parse_tag(strmgr)?;
            if self.apkts.len() > 100 || self.vpkts.len() > 100 {
                return Err(DemuxerError::InvalidData);
            }
        }

        seek_index.mode = SeekIndexMode::Automatic;

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        loop {
            if !self.vpkts.is_empty() && self.vpkts.len() >= self.apkts.len() {
                return Ok(self.vpkts.remove(0));
            }
            if !self.apkts.is_empty() {
                return Ok(self.apkts.remove(0));
            }
            self.parse_tag(strmgr)?;
        }
    }
    fn seek(&mut self, time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        let dst_ms = match time {
                NATimePoint::PTS(pts) => pts,
                NATimePoint::Milliseconds(ms) => ms,
                NATimePoint::None => return Err(DemuxerError::SeekError),
            };
        self.apkts.clear();
        self.vpkts.clear();
        let mut prev = None;
        loop {
            let ppos                    = self.src.read_u32be()?;
            let ret                     = self.src.read_byte();
            if let Err(ByteIOError::EOF) = ret {
                                          self.src.seek(SeekFrom::Current(-8 - i64::from(ppos)))?;
                continue;
            }
            let data_size               = self.src.read_u24be()?;
            let time                    = self.src.read_u24be()?;
            let ext_time                = self.src.read_byte()?;
            let _stream_id              = self.src.read_u24be()?;
            let ts = (u64::from(ext_time) << 32) | u64::from(time);
            if dst_ms == ts {
                                          self.src.seek(SeekFrom::Current(-15))?;
                return Ok(());
            }
            if let Some(p_ts) = prev {
                if dst_ms > p_ts && dst_ms < ts {
                                          self.src.seek(SeekFrom::Current(-19 - i64::from(ppos)))?;
                    return Ok(());
                }
            }
            prev = Some(ts);
            if dst_ms < ts {
                                          self.src.seek(SeekFrom::Current(-19 - i64::from(ppos)))?;
            } else {
                                          self.src.seek(SeekFrom::Current(i64::from(data_size)))?;
            }
        }
    }
    fn get_duration(&self) -> u64 { self.duration }
}

impl<'a> NAOptionHandler for FLVDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> FLVDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:        io,
            vpkts:      Vec::with_capacity(2),
            apkts:      Vec::with_capacity(2),
            vtag:       None,
            atag:       None,
            vstream:    0,
            astream:    0,
            duration:   0,
            width:      0,
            height:     0,
        }
    }
    fn parse_tag(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<()> {
        let _prev_tag_size              = self.src.read_u32be()?;
        let ret                         = self.src.read_byte();
        if let Err(ByteIOError::EOF) = ret {
            return Err(DemuxerError::EOF);
        }

        let tag = ret?;
        let mut data_size               = self.src.read_u24be()? as usize;
        let time                        = self.src.read_u24be()?;
        let ext_time                    = self.src.read_byte()?;
        let stream_id                   = self.src.read_u24be()?;
        validate!(stream_id == 0);
        if data_size == 0 {
            return Ok(());
        }
        let pkt_start = self.src.tell();
        match tag {
            8 => {
                let hdr                 = self.src.read_byte()?;
                if let Some(tag) = self.atag {
                    validate!(tag == (hdr >> 4));
                } else if data_size > 0 {
                    let cname = match hdr >> 4 {
                            0 | 3 => "pcm",
                            1 => "flv-adpcm",
                            2 | 14 => "mp3",
                            4..=6 => "asao",
                            7 => "alaw",
                            8 => "ulaw",
                            10 => "aac",
                            11 => "speex",
                            _ => return Err(DemuxerError::InvalidData),
                        };
                    let mut srate = match (hdr >> 2) & 0x3 {
                            0 => 5500,
                            1 => 11025,
                            2 => 22050,
                            _ => 44100,
                        };
                    let bits = if (hdr & 2) == 0 { 8 } else { 16 };
                    let mut channels = if (hdr & 1) == 0 { 1 } else { 2 };
                    let mut aac_edata = false;
                    match hdr >> 4 {
                         4 => { srate = 16000; channels = 1; },
                         5 => { srate = 8000; channels = 1; },
                        10 => { aac_edata = self.src.read_byte()? == 0; },
                        14 => srate = 8000,
                         _ => {},
                    };
                    let edata = if aac_edata {
                            let pkt_hdr_size = (self.src.tell() - pkt_start) as usize;
                            validate!(data_size >= pkt_hdr_size);
                            let mut data = vec![0; data_size - pkt_hdr_size];
                                              self.src.read_buf(&mut data)?;
                            Some(data)
                        } else {
                            None
                        };
                    let soniton = if bits == 16 { SND_S16P_FORMAT } else { SND_U8_FORMAT };
                    let ahdr = NAAudioInfo::new(srate, channels, soniton, 0);
                    let ci = NACodecTypeInfo::Audio(ahdr);
                    let ainfo = NACodecInfo::new(cname, ci, edata);
                    if let Some(id) = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, 1000, 0)) {
                        self.astream = id;
                    } else {
                        return Err(DemuxerError::MemoryError);
                    }
                    self.atag = Some(hdr >> 4);

                    if aac_edata {
                        return Ok(());
                    }
                }
                if (hdr >> 4) == 10 {
                    let pkt_type            = self.src.read_byte()?;
                    validate!(pkt_type == 1);
                }
                let pkt_hdr_size = (self.src.tell() - pkt_start) as usize;
                validate!(data_size >= pkt_hdr_size);
                data_size -= pkt_hdr_size;
                if data_size > 0 {
                    let stream = strmgr.get_stream(self.astream).unwrap();
                    let pts = (u64::from(ext_time) << 24) | u64::from(time);
                    let ts = stream.make_ts(Some(pts), None, None);
                    self.apkts.push(self.src.read_packet(stream, ts, true, data_size)?);
                }
            },
            9 => {
                let hdr                 = self.src.read_byte()?;
                let ftype = match hdr >> 4 {
                        1 => FrameType::I,
                        2 => FrameType::P,
                        3 => FrameType::P, // droppable
                        4 => FrameType::Other, // generated key frame
                        5 => FrameType::Other, // video info/command frame
                        _ => return Err(DemuxerError::InvalidData),
                    };
                let codec_tag = hdr & 0xF;
                if let Some(id) = self.vtag {
                    validate!(id == codec_tag);
                } else {
                    let cname = get_vcodec_name(codec_tag)?;
                    let is_avc = codec_tag == AVC_ID;
                    if is_avc {
                        let pkt_type            = self.src.read_byte()?;
                        validate!(pkt_type == 0);
                                                  self.src.read_u24be()?;
                    }
                    let mut edata = None;
                    let (width, height) = match codec_tag {
                            2 => {
                                let mut buf = [0; 9];
                                                  self.src.peek_buf(&mut buf)?;
                                let mut br = BitReader::new(&buf, BitReaderMode::BE);
                                                br.skip(30).unwrap_or(());
                                let sfmt      = br.read(3).unwrap_or(7);
                                match sfmt {
                                    0 => {
                                        let w           = br.read(8).unwrap_or(0) as usize;
                                        let h           = br.read(8).unwrap_or(0) as usize;
                                        (w, h)
                                    },
                                    1 => {
                                        let w           = br.read(16).unwrap_or(0) as usize;
                                        let h           = br.read(16).unwrap_or(0) as usize;
                                        (w, h)
                                    },
                                    2 => (352, 288),
                                    3 => (176, 144),
                                    4 => (128, 96),
                                    5 => (320, 240),
                                    6 => (160, 120),
                                    _ => (0, 0),
                                }
                            },
                            3 | 6 => {
                                let mut buf = [0; 4];
                                                  self.src.peek_buf(&mut buf)?;
                                let w = (read_u16be(&buf[0..])? & 0xFFF) as usize;
                                let h = (read_u16be(&buf[2..])? & 0xFFF) as usize;
                                (w, h)
                            },
                            4 => {
                                let mut buf = [0; 7];
                                                  self.src.peek_buf(&mut buf)?;
                                let off = if (buf[1] & 1) != 0 || (buf[2] & 6) == 0 { 5 } else { 3 };
                                validate!(buf[off] != 0 && buf[off + 1] != 0);
                                let w = usize::from(buf[off + 1]) * 16 - usize::from(buf[0] >> 4);
                                let h = usize::from(buf[off])     * 16 - usize::from(buf[0] & 0xF);

                                edata = Some(vec![buf[0]]);

                                (w, h)
                            },
                            5 => {
                                let mut buf = [0; 10];
                                                  self.src.peek_buf(&mut buf)?;
                                let off = if (buf[4] & 1) != 0 || (buf[5] & 6) == 0 { 8 } else { 6 };
                                validate!(buf[off] != 0 && buf[off + 1] != 0);
                                let w = usize::from(buf[off + 1]) * 16 - usize::from(buf[0] >> 4);
                                let h = usize::from(buf[off])     * 16 - usize::from(buf[0] & 0xF);

                                edata = Some(vec![buf[0]]);

                                (w, h)
                            },
                            7 => {
                                let pkt_hdr_size = (self.src.tell() - pkt_start) as usize;
                                validate!(data_size >= pkt_hdr_size);
                                data_size -= pkt_hdr_size;
                                let mut data = vec![0; data_size + 4];
                                data[..4].copy_from_slice(b"avcC");
                                                  self.src.read_buf(&mut data[4..])?;
                                edata = Some(data);
                                (self.width, self.height)
                            },
                            _ => unreachable!(),
                        };

                    let vhdr = NAVideoInfo::new(width, height, false, YUV420_FORMAT);
                    let vci = NACodecTypeInfo::Video(vhdr);
                    let vinfo = NACodecInfo::new(cname, vci, edata);
                    if let Some(id) = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 1000, 0)) {
                        self.vstream = id;
                    } else {
                        return Err(DemuxerError::MemoryError);
                    }
                    self.vtag = Some(codec_tag);
                    if is_avc {
                        return Ok(());
                    }
                }
                let mut cts = 0;
                match codec_tag {
                    4 | 5 => {
                                          self.src.read_skip(1)?;
                    },
                    7 => {
                        let pkt_type            = self.src.read_byte()?;
                        if pkt_type == 1 {
                            cts                 = ((self.src.read_u24be()? << 8) as i32) >> 8;
                        } else if pkt_type == 2 {
                            let pkt_hdr_size = (self.src.tell() - pkt_start) as usize;
                            validate!(data_size >= pkt_hdr_size);
                            data_size -= pkt_hdr_size;
                                                  self.src.read_skip(data_size)?;
                            return Ok(());
                        }
                    },
                    _ => {},
                };

                let pkt_hdr_size = (self.src.tell() - pkt_start) as usize;
                validate!(data_size >= pkt_hdr_size);
                data_size -= pkt_hdr_size;

                if data_size > 0 {
                    let stream = strmgr.get_stream(self.vstream).unwrap();
                    let pts = (u64::from(ext_time) << 24) | u64::from(time);
                    let dts = ((pts as i64) + i64::from(cts)).max(0) as u64;
                    let ts = stream.make_ts(Some(pts), Some(dts), None);
                    self.vpkts.push(self.src.read_packet(stream, ts, ftype == FrameType::I, data_size)?);
                }
            },
            18 => {
                let end = self.src.tell() + (data_size as u64);
                let ntype           = self.src.read_byte()?;
                validate!(ntype == 2);
                let nlen            = self.src.read_u16be()? as usize;
                validate!(nlen > 0);
                let mut name = vec![0; nlen];
                                      self.src.read_buf(&mut name)?;
                if &name == b"onMetaData" {
                    let otype               = self.src.read_byte()?;
                    validate!(otype == 8);
                    let _size               = self.src.read_u32be()?;
                    while self.src.tell() < end {
                        let nlen            = self.src.read_u16be()? as usize;
                        if nlen == 0 {
                            let emarker     = self.src.peek_byte()?;
                            if emarker == 9 {
                                              self.src.read_skip(1)?;
                                break;
                            }
                        }
                        let mut name = vec![0; nlen];
                                              self.src.read_buf(&mut name)?;
                        let vtype           = self.src.read_byte()?;
                        match vtype {
                            0 => {
                                let val     = self.src.read_f64be()?;
                                match name.as_slice() {
                                    b"duration" => self.duration = (val * 1000.0) as u64,
                                    b"width"    => self.width  = val as usize,
                                    b"height"   => self.height = val as usize,
                                    b"videocodecid" => {
                                        let codec_tag = val as u8;
                                        if self.vtag.is_none() && codec_tag != AVC_ID && self.width != 0 && self.height != 0 {
                                            let cname = get_vcodec_name(codec_tag)?;
                                            let edata = if cname.starts_with("vp6") {
                                                    let ebyte = ((16 - (self.width & 0xF)) & 0xF) * 16 + ((16 - (self.height & 0xF)) & 0xF);
                                                    Some(vec![ebyte as u8])
                                                } else { None };
                                            let vhdr = NAVideoInfo::new(self.width, self.height, false, YUV420_FORMAT);
                                            let vci = NACodecTypeInfo::Video(vhdr);
                                            let vinfo = NACodecInfo::new(cname, vci, edata);
                                            if let Some(id) = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 1000, 0)) {
                                                self.vstream = id;
                                            } else {
                                                return Err(DemuxerError::MemoryError);
                                            }
                                            self.vtag = Some(codec_tag);
                                        }
                                    },
                                    _ => {},
                                };
                            },
                            1 => {
                                let _val    = self.src.read_byte()?;
                            },
                            2 => {
                                let len     = self.src.read_u16be()? as usize;
                                let mut val = vec![0; len];
                                              self.src.read_buf(&mut val)?;
                            },
                            3 => {
                                break;//unimplemented!();
                            },
                            5 => {},
                            6 => {},
                            7 => {
                                              self.src.read_u16be()?;
                            },
                            8 => {
                                unimplemented!();
                            },
                            10 => {
                                unimplemented!();
                            },
                            11 => {
                                              self.src.read_f64be()?;
                                              self.src.read_u16be()?;
                            },
                            12 => {
                                let len     = self.src.read_u16be()? as usize;
                                let mut val = vec![0; len];
                                              self.src.read_buf(&mut val)?;
                            },
                            _ => break,
                        };
                    }
                }
                validate!(self.src.tell() <= end);
                let to_skip = (end - self.src.tell()) as usize;
                                          self.src.read_skip(to_skip)?;
            },
            _ => {
                                          self.src.read_skip(data_size)?;
            },
        };
        Ok(())
    }
}

pub struct FLVDemuxerCreator { }

impl DemuxerCreator for FLVDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(FLVDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "flv" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    // sample: https://samples.mplayerhq.hu/A-codecs/Nelly_Moser/input.flv
    #[test]
    fn test_flv_demux() {
        let mut file = File::open("assets/Flash/input.flv").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = FLVDemuxer::new(&mut br);
        let mut sm = StreamManager::new();
        let mut si = SeekIndex::new();
        dmx.open(&mut sm, &mut si).unwrap();

        loop {
            let pktres = dmx.get_frame(&mut sm);
            if let Err(e) = pktres {
                if e == DemuxerError::EOF { break; }
                panic!("error");
            }
            let pkt = pktres.unwrap();
            println!("Got {}", pkt);
        }
    }
    #[test]
    fn test_flv_demux_back() {
        let mut file = File::open("assets/Flash/input.flv").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = FLVDemuxer::new(&mut br);
        let mut sm = StreamManager::new();
        let mut si = SeekIndex::new();
        dmx.open(&mut sm, &mut si).unwrap();
        dmx.src.seek(SeekFrom::End(-4)).unwrap();
        dmx.seek(NATimePoint::Milliseconds(7500), &si).unwrap();

        loop {
            let pktres = dmx.get_frame(&mut sm);
            if let Err(e) = pktres {
                if e == DemuxerError::EOF { break; }
                panic!("error");
            }
            let pkt = pktres.unwrap();
            println!("Got {}", pkt);
        }
    }
}
