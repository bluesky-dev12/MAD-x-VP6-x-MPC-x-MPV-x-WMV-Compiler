use nihav_core::frame::*;
use nihav_core::demuxers::*;

#[allow(dead_code)]
struct FutureVisionVideoDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    cur_frame:  usize,
    apos:       u64,
    vsize:      Vec<usize>,
    asize:      Vec<usize>,
    a_id:       Option<usize>,
    v_id:       Option<usize>,
    vframe:     bool,
}

impl<'a> DemuxCore<'a> for FutureVisionVideoDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let magic                       = src.read_tag()?;
        validate!(&magic == b"2TSF");
        let width                       = src.read_u32le()? as usize;
        let height                      = src.read_u32le()? as usize;
        validate!(width != 0 && height != 0);
        let _flags                      = src.read_u32le()?;
        let nframes                     = src.read_u32le()? as usize;
        let fps                         = src.read_u32le()?;
        let arate                       = src.read_u32le()?;
        let abits                       = src.read_u32le()?;

        let vhdr = NAVideoInfo::new(width, height, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("fst-video", vci, None);
        self.v_id = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, fps, nframes as u64));
        if arate != 0 {
            validate!(abits == 8 || abits == 16);
            let ahdr = NAAudioInfo::new(arate, 1, if abits == 16 { SND_S16_FORMAT } else { SND_U8_FORMAT }, 2);
            let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
            self.a_id = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, arate, 2));
        }
        self.vsize = Vec::with_capacity(nframes);
        self.asize = Vec::with_capacity(nframes);
        for _ in 0..nframes {
            let vsize                   = src.read_u32le()? as usize;
            let asize                   = src.read_u16le()? as usize;
            self.vsize.push(vsize);
            self.asize.push(asize);
        }
        self.vframe = true;
        self.cur_frame = 0;
        self.apos = 0;
        Ok(())
    }

    #[allow(unused_variables)]
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.cur_frame >= self.vsize.len() { return Err(DemuxerError::EOF); }
        let (id, size, pts) = if self.vframe {
                self.vframe = self.a_id.is_none();

                (self.v_id.unwrap_or(0), self.vsize[self.cur_frame], self.cur_frame as u64)
            } else {
                self.vframe = true;
                let apos = self.apos;
                self.apos += (self.asize[self.cur_frame] as u64) * 2;

                (self.a_id.unwrap_or(0), self.asize[self.cur_frame], apos)
            };

        if self.vframe {
            self.cur_frame += 1;
        }

        let stream = strmgr.get_stream(id).unwrap();
        let ts = stream.make_ts(Some(pts), None, None);
        self.src.read_packet(stream, ts, true, size)
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}
impl<'a> NAOptionHandler for FutureVisionVideoDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}
impl<'a> FutureVisionVideoDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        FutureVisionVideoDemuxer {
            src:        io,
            cur_frame:  0,
            apos:       0,
            vsize:      Vec::new(),
            asize:      Vec::new(),
            a_id:       None,
            v_id:       None,
            vframe:     false,
        }
    }
}

pub struct FSTDemuxerCreator { }

impl DemuxerCreator for FSTDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(FutureVisionVideoDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "fst" }
}

#[allow(dead_code)]
struct FutureVisionAudioDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    a_id:       usize,
    end:        u64,
    arate:      u32,
}

impl<'a> DemuxCore<'a> for FutureVisionAudioDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let magic                       = src.read_tag()?;
        validate!(&magic == b"FCMP");
        let size                        = u64::from(src.read_u32le()?);
        let arate                       = src.read_u32le()?;
        validate!(arate != 0);
        let abits                       = src.read_u16le()?;
        validate!(abits == 8 || abits == 16);

        let ahdr = NAAudioInfo::new(arate, 1, if abits == 16 { SND_S16_FORMAT } else { SND_U8_FORMAT }, 2);
        let ainfo = NACodecInfo::new("fst-audio", NACodecTypeInfo::Audio(ahdr), None);
        self.a_id = strmgr.add_stream(NAStream::new(StreamType::Audio, 0, ainfo, 1, arate, 2)).unwrap();
        self.end = self.src.tell() + size;
        self.arate = arate;
        Ok(())
    }

    #[allow(unused_variables)]
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.src.tell() >= self.end { return Err(DemuxerError::EOF); }
        let size = (self.end - self.src.tell()).min(0x2000) as usize;
        let pts = (self.src.tell() - 14) * 2;

        let stream = strmgr.get_stream(self.a_id).unwrap();
        let ts = stream.make_ts(Some(pts), None, None);
        self.src.read_packet(stream, ts, true, size)
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { (self.end - 14) * 2000 / u64::from(self.arate) }
}
impl<'a> NAOptionHandler for FutureVisionAudioDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}
impl<'a> FutureVisionAudioDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        FutureVisionAudioDemuxer {
            src:        io,
            a_id:       0,
            end:        0,
            arate:      0,
        }
    }
}

pub struct FCMPDemuxerCreator { }

impl DemuxerCreator for FCMPDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(FutureVisionAudioDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "fcmp" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    // samples from the Harvester game
    #[test]
    fn test_fst_demux() {
        let mut file = File::open("assets/Game/c007.fst").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = FutureVisionVideoDemuxer::new(&mut br);
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
    fn test_fcmp_demux() {
        let mut file = File::open("assets/Game/anxiety.cmp").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = FutureVisionAudioDemuxer::new(&mut br);
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
