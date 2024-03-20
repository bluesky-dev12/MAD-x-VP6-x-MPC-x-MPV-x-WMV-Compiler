use nihav_core::frame::*;
use nihav_core::demuxers::*;

struct ArxelCinemaDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    cur_frame:  usize,
    astreams:   usize,
    offsets:    Vec<u32>,
    vpts:       u64,
    tb_num:     u32,
    tb_den:     u32,
    is_ci2:     bool,
    tdata:      Vec<u8>,
}

impl<'a> DemuxCore<'a> for ArxelCinemaDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let magic                       = src.read_tag()?;
        validate!(&magic == b"CNM ");
        let magic                       = src.read_tag()?;
        validate!(&magic == b"UNR\x00");
        let _duration                   = src.read_u32le()?;
        let tb_den                      = src.read_u32le()?;
                                          src.read_byte()?;
        let width                       = src.read_u32le()? as usize;
        let height                      = src.read_u32le()? as usize;
        validate!(width != 0 && height != 0);
                                          src.read_u16le()?;
        self.astreams                   = src.read_byte()? as usize;
        validate!(self.astreams < 4);
        if self.astreams > 1 {
            return Err(DemuxerError::NotImplemented);
        }
        let nframes                     = src.read_u32le()? as usize;
                                          src.read_u32le()?; //nframes again?
        let tab_size                    = src.read_u32le()? as usize;
                                          src.read_skip(0x98)?;

        let vhdr = NAVideoInfo::new(width, height, true, RGB24_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        // use tab_size mismatch as the version marker
        let vinfo = NACodecInfo::new("arxel-video", vci, Some(vec![(tab_size != nframes * 8) as u8]));
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 100, tb_den, nframes as u64)).is_none() {
            return Err(DemuxerError::MemoryError);
        }
        if let Some(stream) = strmgr.get_stream_by_id(0) {
            let (tb_num, tb_den) = stream.get_timebase();
            self.tb_num = tb_num;
            self.tb_den = tb_den;
        }

        for trk in 0..(self.astreams as u32) {
            let channels                = src.read_byte()? + 1;
            let abits                   = src.read_byte()?;
            let arate                   = src.read_u32le()?;
                                          src.read_skip(10)?;
            validate!(channels > 0);
            validate!(abits >= 8);
            validate!(arate > 0);

            let ahdr = NAAudioInfo::new(arate, channels, if abits > 8 { SND_S16_FORMAT } else { SND_U8_FORMAT }, 2);
            let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
            if strmgr.add_stream(NAStream::new(StreamType::Audio, 1 + trk, ainfo, 1, arate, 0)).is_none() {
                return Err(DemuxerError::MemoryError);
            }
        }

        if tab_size == nframes * 8 {
            let tab_size = tab_size / 4;
            self.offsets = Vec::with_capacity(tab_size);
            for _ in 0..tab_size {
                let offset                  = src.read_u32le()?;
                self.offsets.push(offset);
            }
        } else {
            validate!(nframes > 0);
            let off0                    = src.read_u32le()?;
            let off1                    = src.read_u32le()?;
            if off0 == 0 && off1 == 0 {
                self.is_ci2 = true;
                                          src.read_skip((nframes - 1) * 8)?;
            } else {
                return Err(DemuxerError::InvalidData);
            }
        }
        self.cur_frame = 0;
        self.vpts = 0;
        Ok(())
    }

    #[allow(unused_variables)]
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        loop {
            let stream_id;
            if !self.is_ci2 {
                if self.cur_frame >= self.offsets.len() { return Err(DemuxerError::EOF); }
                let pos = u64::from(self.offsets[self.cur_frame]);
                stream_id = (self.cur_frame % (self.astreams.max(1) + 1)) as u32;
                self.cur_frame += 1;
                if pos == 0 {
                    continue;
                }
                                          self.src.seek(SeekFrom::Start(pos))?;
            } else {
                stream_id = 1;
            }
            let ftype = match self.src.read_byte() {
                    Ok(b) => b,
                    Err(ByteIOError::EOF) if self.is_ci2 => return Err(DemuxerError::EOF),
                    _ => return Err(DemuxerError::IOError),
                };
            match ftype {
                0x41 | 0x42 | 0x5A => {
                    let size            = self.src.read_u32le()? as usize;
                    if size == 0 {
                        continue;
                    }
                    if let Some(stream) = strmgr.get_stream_by_id(stream_id) {
                        let ts = stream.make_ts(None, None, None);
                        return self.src.read_packet(stream, ts, true, size);
                    } else {
                                          self.src.read_skip(size)?;
                    }
                },
                0x53 if !self.is_ci2 => {
                    let size            = self.src.peek_u32le()? as usize;
                    if let Some(stream) = strmgr.get_stream_by_id(0) {
                        let ts = stream.make_ts(Some(self.vpts), None, None);
                        self.vpts += 1;
                        return self.src.read_packet(stream, ts, true, size + 0x2F);
                    } else {
                        return Err(DemuxerError::MemoryError);
                    }
                },
                0x53 | 0x55 => {
                    let size            = self.src.peek_u32le()? as usize;
                    let mut data = Vec::new();
                    std::mem::swap(&mut self.tdata, &mut data);
                    data.push(ftype);
                    let head_size = data.len();
                    data.resize(head_size + size + 0x2F, 0);
                                          self.src.read_buf(&mut data[head_size..])?;
                    if let Some(stream) = strmgr.get_stream_by_id(0) {
                        let ts = stream.make_ts(Some(self.vpts), None, None);
                        self.vpts += 1;
                        return Ok(NAPacket::new(stream, ts, ftype == 0x55, data));
                    } else {
                        return Err(DemuxerError::MemoryError);
                    }
                },
                0x54 => {
                    validate!(self.is_ci2);
                    let size            = self.src.peek_u32le()? as usize;
                    validate!(self.tdata.is_empty());
                    self.tdata.resize(size + 9, 0);
                    self.tdata[0] = 0x54;
                                          self.src.read_buf(&mut self.tdata[1..])?;
                },
                _ => continue,
            };
        }
    }

    fn seek(&mut self, time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        if self.is_ci2 {
            return Err(DemuxerError::NotPossible);
        }
        match time {
            NATimePoint::PTS(pts) => self.seek_to_frame(pts),
            NATimePoint::Milliseconds(ms) => {
                if self.tb_num == 0 || self.tb_den == 0 {
                    return Err(DemuxerError::NotPossible);
                }
                let pts = NATimeInfo::time_to_ts(ms, 1000, self.tb_num, self.tb_den);
                self.seek_to_frame(pts)
            },
            _ => Err(DemuxerError::NotPossible)
        }
    }
    fn get_duration(&self) -> u64 { 0 }
}
impl<'a> NAOptionHandler for ArxelCinemaDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}
impl<'a> ArxelCinemaDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        ArxelCinemaDemuxer {
            src:        io,
            cur_frame:  0,
            astreams:   0,
            offsets:    Vec::new(),
            vpts:       0,
            tb_num:     0,
            tb_den:     0,
            is_ci2:     false,
            tdata:      Vec::new(),
        }
    }
    fn seek_to_frame(&mut self, pts: u64) -> DemuxerResult<()> {
        let nframe = (pts as usize) * (1 + self.astreams.max(1));
        if nframe < self.offsets.len() {
            self.cur_frame = nframe;
            Ok(())
        } else {
            Err(DemuxerError::SeekError)
        }
    }
}

pub struct CNMDemuxerCreator { }

impl DemuxerCreator for CNMDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(ArxelCinemaDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "arxel-cnm" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    // samples from the Ring game
    #[test]
    fn test_cnm_demux() {
        let mut file = File::open("assets/Game/logo.cnm").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = ArxelCinemaDemuxer::new(&mut br);
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
    fn test_cnm_noaud_demux() {
        let mut file = File::open("assets/Game/tr_as_ro_d.cnm").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = ArxelCinemaDemuxer::new(&mut br);
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
    // sample from Faust: The Seven Games of the Soul game
    #[test]
    fn test_ci2_demux() {
        let mut file = File::open("assets/Game/logo.CI2").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = ArxelCinemaDemuxer::new(&mut br);
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
