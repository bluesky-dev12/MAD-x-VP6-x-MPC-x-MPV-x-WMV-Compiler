use nihav_core::demuxers::*;

struct VivoDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    video_id:       usize,
    audio_id:       usize,
    video_buf:      Vec<u8>,
    vpts:           u64,
    apts:           u64,
    v_num:          u32,
    v_den:          u32,
    a_num:          u32,
    a_den:          u32,
    fps:            f32,
    width:          usize,
    height:         usize,
    duration:       u64,
    vname:          &'static str,
    aname:          &'static str,
}

fn read_size(br: &mut ByteReader) -> DemuxerResult<usize> {
    let mut ret = 0;
    loop {
        let c = br.read_byte()?;
        ret = (ret << 7) | ((c & 0x7F) as usize);
        if (c & 0x80) == 0 {
            break;
        }
    }
    Ok(ret)
}

impl<'a> DemuxCore<'a> for VivoDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, _seek_idx: &mut SeekIndex) -> DemuxerResult<()> {
        let mut hdr_data: Vec<u8> = Vec::with_capacity(256);
        loop {
            let hdr                     = self.src.peek_byte()?;
            if (hdr & 0xF0) != 0 { break; }
                                          self.src.read_skip(1)?;
            let hdr_len                 = read_size(self.src)?;
            hdr_data.resize(hdr_len, 0);
                                          self.src.read_buf(&mut hdr_data)?;
            self.parse_header_packet(&hdr_data)?;
        }

        validate!(self.vname != "none");
        if self.width == 0 && self.height == 0 {
            self.width  = 160;
            self.height = 120;
        }
        validate!(self.fps > 0.0 || (self.v_num > 0 && self.v_den > 0));

        if self.v_num == 0 {
            self.v_num = 1000;
            self.v_den = (self.fps * 1000.0) as u32;
        }

        let vhdr = NAVideoInfo::new(self.width, self.height, false, YUV420_FORMAT);
        let vinfo = NACodecInfo::new(self.vname, NACodecTypeInfo::Video(vhdr), None);
        let res = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, self.v_num, self.v_den, 0));
        validate!(res.is_some());
        self.video_id = res.unwrap();

        if self.aname == "none" && self.vname == "vivo1" {
            self.aname = "g723.1";
            self.a_num = 240;
            self.a_den = 8000;
        }
        if self.aname != "none" {
            let ahdr = NAAudioInfo::new(self.a_den, 1, SND_S16_FORMAT, self.a_num as usize);
            let ainfo = NACodecInfo::new(self.aname, NACodecTypeInfo::Audio(ahdr), None);
            let res = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, self.a_num, self.a_den, 0));
            validate!(res.is_some());
            self.audio_id = res.unwrap();
        }

        Ok(())
    }
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        let br = &mut self.src;

        loop {
            let ret                     = br.read_byte();
            let hdr1 = match ret {
                    Err(ByteIOError::EOF) => return Err(DemuxerError::EOF),
                    Err(error)            => return Err(error.into()),
                    Ok(val) => val,
                };
            let force_size = hdr1 == 0x82;
            let hdr = if force_size { br.read_byte()? } else { hdr1 };
            let (is_video, mut size) = match hdr >> 4 {
                    1 => { (true, 128) },
                    2 => { validate!(!force_size); (true, read_size(br)?) },
                    3 => { (false, 40) },
                    4 => { (false, 24) },
                    _ => return Err(DemuxerError::InvalidData),
                };
            if force_size {
                size                    = read_size(br)?;
            }
            if is_video {
                validate!(self.v_den != 0);
                let cur_size = self.video_buf.len();
                let new_size = cur_size + size;
                self.video_buf.resize(new_size, 0);
                                          br.read_buf(&mut self.video_buf[cur_size..])?;
                if (hdr >> 4) == 2 {
                    let mut buf = Vec::new();
                    std::mem::swap(&mut self.video_buf, &mut buf);
                    let strres = strmgr.get_stream(self.video_id);
                    validate!(strres.is_some());
                    let stream = strres.unwrap();
                    let ts = NATimeInfo::new(Some(self.vpts), None, None, self.v_num, self.v_den);
                    let pkt = NAPacket::new(stream, ts, self.vpts == 0, buf);
                    self.vpts += 1;
                    return Ok(pkt);
                }
            } else {
                validate!(self.a_den != 0);
                let mut buf: Vec<u8> = vec![0; size];
                                          br.read_buf(&mut buf)?;
                let strres = strmgr.get_stream(self.audio_id);
                validate!(strres.is_some());
                let stream = strres.unwrap();
                let ts = NATimeInfo::new(Some(self.apts), None, None, self.a_num, self.a_den);
                let pkt = NAPacket::new(stream, ts, true, buf);
                self.apts += 1;
                return Ok(pkt);
            }
        }
    }
    fn seek(&mut self, _time: NATimePoint, _seek_idx: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { self.duration }
}

impl<'a> NAOptionHandler for VivoDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> VivoDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        VivoDemuxer {
            src:            io,
            video_id:       0,
            audio_id:       0,
            video_buf:      Vec::new(),
            vpts:           0,
            apts:           0,
            v_num:          0,
            v_den:          0,
            a_num:          0,
            a_den:          0,
            width:          0,
            height:         0,
            fps:            0.0,
            duration:       0,
            vname:          "none",
            aname:          "none",
        }
    }
    fn parse_header_packet(&mut self, pkt: &[u8]) -> DemuxerResult<()> {
        for entry in pkt.split(|ch| *ch == 0xD) {
            if entry.len() < 3 || !entry.contains(&b':') { continue; }
            let entry = if !entry.is_empty() && entry[0] == 0xA { &entry[1..] } else { entry };
            let mut split = entry.split(|ch| *ch == b':');
            let name  = split.next().unwrap();
            let value = split.next().unwrap();

            let valstr = String::from_utf8_lossy(value);
            match name {
                b"Version" => {
                    match value {
                        b"Vivo/0.90" => { self.vname = "vivo1"; },
                        b"Vivo/1.00" => { self.vname = "vivo1"; },
                        b"Vivo/2.00" => { self.vname = "vivo2"; },
                        _ => {
                            println!("Unknown codec name {}", valstr);
                            return Err(DemuxerError::InvalidData);
                        },
                    };
                },
                b"FPS" => {
                    self.fps = if let Ok(val) = valstr.parse() {
                            val
                        } else { return Err(DemuxerError::InvalidData); };
                    validate!(self.fps > 0.0 && self.fps < 1000.0);
                },
                b"Width" => {
                    self.width = if let Ok(val) = valstr.parse() {
                            val
                        } else { return Err(DemuxerError::InvalidData); };
                    validate!(self.width > 0 && self.width <= 1024);
                },
                b"Height" => {
                    self.height = if let Ok(val) = valstr.parse() {
                            val
                        } else { return Err(DemuxerError::InvalidData); };
                    validate!(self.height > 0 && self.height <= 768);
                },
                b"SamplingFrequency" => {
                    let samp_freq: u32 = if let Ok(val) = valstr.parse() {
                            val
                        } else { return Err(DemuxerError::InvalidData); };
                    validate!(samp_freq == 8000 || samp_freq == 16000);
                    if samp_freq == 8000 {
                        self.aname = "g723.1";
                        self.a_num = 240;
                        self.a_den = 8000;
                    } else if samp_freq == 16000 {
                        self.aname = "siren";
                        self.a_num = 320;
                        self.a_den = 16000;
                    } else {
                        return Err(DemuxerError::InvalidData);
                    };
                },
                b"Duration" => {
                    self.duration = if let Ok(val) = valstr.parse() {
                            val
                        } else { 0 };
                },
/*                b"TimeUnitNumerator" => {
                    self.v_num = if let Ok(val) = valstr.parse() {
                            val
                        } else { return Err(DemuxerError::InvalidData); };
                    validate!(self.v_num > 0);
                    println!(" video codec tb_num {}", self.v_num);
                },
                b"TimeUnitDenominator" => {
                    self.v_den = if let Ok(val) = valstr.parse() {
                            val
                        } else { return Err(DemuxerError::InvalidData); };
                    validate!(self.v_den > 0);
                    println!(" video codec tb_den {}", self.v_den);
                },*/
                _ => { },
            };
        }
        Ok(())
    }
}

pub struct VivoDemuxerCreator { }

impl DemuxerCreator for VivoDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(VivoDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "vivo" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_vivo_demux() {
//        let mut file = File::open("assets/Misc/greetings.viv").unwrap();
        // sample: https://samples.mplayerhq.hu/vivo/vivo2/favmovie.viv
        let mut file = File::open("assets/Misc/favmovie.viv").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = VivoDemuxer::new(&mut br);
        let mut sm = StreamManager::new();
        let mut si = SeekIndex::new();
        dmx.open(&mut sm, &mut si).unwrap();
        loop {
            let pktres = dmx.get_frame(&mut sm);
            if let Err(e) = pktres {
                if (e as i32) == (DemuxerError::EOF as i32) { break; }
                panic!("error {:?}", e);
            }
            let pkt = pktres.unwrap();
            println!("Got {}", pkt);
        }
    }
}
