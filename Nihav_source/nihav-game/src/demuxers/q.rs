use nihav_core::frame::*;
use nihav_core::demuxers::*;

#[allow(dead_code)]
struct QDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    vpts:       u64,
    apts:       u64,
    bps:        usize,
    a_id:       Option<usize>,
    v_id:       Option<usize>,
    nframes:    usize,
    duration:   u64,
    side_data:  Vec<u8>,
}

impl<'a> DemuxCore<'a> for QDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let mut hdr = [0; 22];
                                          src.read_buf(&mut hdr)?;
        validate!(hdr[0] == 0x39);
        validate!(hdr[1] == 0x68);
        let version = hdr[2];
        validate!(version >= 3 && version <= 7);
        let mut width  = read_u16le(&hdr[4..])? as usize;
        let mut height = read_u16le(&hdr[6..])? as usize;
        if version > 3 {
            width  *= hdr[8] as usize;
            height *= hdr[9] as usize;
        }
        validate!(width > 0 && width <= 800);
        validate!(height > 0 && height <= 600);

        self.nframes = read_u16le(&hdr[10..])? as usize;
        validate!(self.nframes > 0);
        let fps = if hdr[16] == 0 { 5 } else { hdr[16] as u32 };
        self.duration = (self.nframes as u64) * 1000 / u64::from(fps);
        let asize = if version > 3 {
                                          src.read_u32le()?
            } else { 0 };

        let vhdr = NAVideoInfo::new(width, height, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("legend-q-video", vci, Some(hdr.to_vec()));
        self.v_id = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, fps, self.nframes as u64));
        if asize != 0 {
            let ntype                   = self.src.peek_byte()?;
            if ntype == 8 {
                let _                   = self.src.read_u16le()?;
                let size                = self.src.read_u32le()? as usize;
                validate!(size >= 44);
                let mut buf = vec![0; size];
                                          self.src.read_buf(&mut buf)?;
                let arate = read_u32le(&buf[24..])?;
                if arate > 0 {
                    let channels = buf[22];
                    let abits    = buf[34] as usize;
                    validate!(abits == 8 || abits == 16);
                    self.bps = (channels as usize) * abits / 8;
                    let bsize    = read_u16le(&buf[32..])? as usize;
                    let ahdr = NAAudioInfo::new(arate, channels, if abits == 16 { SND_S16_FORMAT } else { SND_U8_FORMAT }, bsize);
                    let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
                    self.a_id = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, arate, 2));
                }
            }
        }
        self.apts = 0;
        self.vpts = 0;
        self.side_data.clear();
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        loop {
            let ctype                   = self.src.read_u16le()?;
            let size                    = self.src.read_u32le()? as usize;
            match ctype {
                0xFFFF => return Err(DemuxerError::EOF),
                0 => {
                    if let Some(a_id) = self.a_id {
                        let stream = strmgr.get_stream(a_id).unwrap();
                        let ts = stream.make_ts(Some(self.apts), None, None);
                        self.apts += (size / self.bps) as u64;
                        return self.src.read_packet(stream, ts, true, size);
                    } else {
                                          self.src.read_skip(size)?;
                    }
                },
                1 => {
                    validate!(size <= 768);
                    let cur_len = self.side_data.len();
                    self.side_data.resize(cur_len + size + 6, 0);
                    self.side_data[cur_len] = ctype as u8;
                    write_u32le(&mut self.side_data[cur_len + 2..], size as u32)?;
                                          self.src.read_buf(&mut self.side_data[cur_len + 6..])?;
                },
                2 | 3 | 4 | 11 => {
                    validate!(self.v_id.is_some());
                    let stream = strmgr.get_stream(self.v_id.unwrap_or(0)).unwrap();
                    let ts = stream.make_ts(Some(self.vpts), None, None);
                    self.vpts += 1;

                    let cur_len = self.side_data.len();
                    self.side_data.resize(cur_len + size + 6, 0);
                    self.side_data[cur_len] = ctype as u8;
                    self.side_data[cur_len] = ctype as u8;
                    write_u32le(&mut self.side_data[cur_len + 2..], size as u32)?;
                    if let Err(err)       = self.src.read_buf(&mut self.side_data[cur_len + 6..]) {
                        self.side_data.truncate(cur_len);
                        return Err(err.into());
                    }
                    let mut buf = Vec::new();
                    std::mem::swap(&mut buf, &mut self.side_data);
                    return Ok(NAPacket::new(stream, ts, self.vpts == 1, buf));
                },
                5 => {
                    validate!(size <= 256);
                    let cur_len = self.side_data.len();
                    self.side_data.resize(cur_len + size + 6, 0);
                    self.side_data[cur_len] = ctype as u8;
                    write_u32le(&mut self.side_data[cur_len + 2..], size as u32)?;
                                          self.src.read_buf(&mut self.side_data[cur_len + 6..])?;
                },
                6 | 7 => {
                    self.side_data.push(ctype as u8);
                    self.side_data.push(0);
                    self.side_data.push(0);
                    self.side_data.push(0);
                    self.side_data.push(0);
                    self.side_data.push(0);
                },
                8 => return Err(DemuxerError::InvalidData), //should be handled before main loop
                9 => { // first part of interlaced frame
                    let cur_len = self.side_data.len();
                    self.side_data.resize(cur_len + size + 6, 0);
                    self.side_data[cur_len] = ctype as u8;
                    write_u32le(&mut self.side_data[cur_len + 2..], size as u32)?;
                                          self.src.read_buf(&mut self.side_data[cur_len + 6..])?;
                },
                _ => {
                                          self.src.read_skip(size)?;
                },
            };
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { self.duration }
}
impl<'a> NAOptionHandler for QDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}
impl<'a> QDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        QDemuxer {
            src:        io,
            vpts:       0,
            apts:       0,
            bps:        0,
            a_id:       None,
            v_id:       None,
            nframes:    0,
            duration:   0,
            side_data:  Vec::with_capacity(256 + 6),
        }
    }
}

pub struct QDemuxerCreator { }

impl DemuxerCreator for QDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(QDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "legend-q" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    // samples from Death Gate, Mission Critical and Shannara games
    #[test]
    fn test_q_demux_v3() {
        let mut file = File::open("assets/Game/dgate101.q").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = QDemuxer::new(&mut br);
        let mut sm = StreamManager::new();
        let mut si = SeekIndex::new();
        dmx.open(&mut sm, &mut si).unwrap();
        loop {
            let pktres = dmx.get_frame(&mut sm);
            if let Err(e) = pktres {
                if (e as i32) == (DemuxerError::EOF as i32) { break; }
                panic!("error");
            }
            let pkt = pktres.unwrap();
            println!("Got {}", pkt);
        }
    }

    #[test]
    fn test_q_demux_v4() {
        let mut file = File::open("assets/Game/1425A5.Q").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = QDemuxer::new(&mut br);
        let mut sm = StreamManager::new();
        let mut si = SeekIndex::new();
        dmx.open(&mut sm, &mut si).unwrap();
        loop {
            let pktres = dmx.get_frame(&mut sm);
            if let Err(e) = pktres {
                if (e as i32) == (DemuxerError::EOF as i32) { break; }
                panic!("error");
            }
            let pkt = pktres.unwrap();
            println!("Got {}", pkt);
        }
    }

    #[test]
    fn test_q_demux_v5() {
        let mut file = File::open("assets/Game/mc703.q").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = QDemuxer::new(&mut br);
        let mut sm = StreamManager::new();
        let mut si = SeekIndex::new();
        dmx.open(&mut sm, &mut si).unwrap();
        loop {
            let pktres = dmx.get_frame(&mut sm);
            if let Err(e) = pktres {
                if (e as i32) == (DemuxerError::EOF as i32) { break; }
                panic!("error");
            }
            let pkt = pktres.unwrap();
            println!("Got {}", pkt);
        }
    }
}
