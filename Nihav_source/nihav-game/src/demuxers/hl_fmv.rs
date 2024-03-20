use nihav_core::frame::*;
use nihav_core::demuxers::*;

#[allow(dead_code)]
struct HighlanderFMVDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    vpts:       u64,
    apts:       u64,
}

impl<'a> DemuxCore<'a> for HighlanderFMVDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let tag                         = src.read_tag()?;
        validate!(&tag == b"FMV*");
        let size                        = src.read_u32le()?;
        validate!(size == 0);

        let vhdr = NAVideoInfo::new(320, 240, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("hl-fmv-video", vci, None);
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 2, 25, 0)).is_none() {
            return Err(DemuxerError::MemoryError);
        }
        let ahdr = NAAudioInfo::new(22050, 1, SND_U8_FORMAT, 1);
        let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
        if strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, 22050, 0)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        self.apts = 0;
        self.vpts = 0;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        let tag                         = self.src.read_tag()?;
        let size                        = self.src.read_u32le()? as usize;
        match &tag {
            b"AUD1" => {
                let stream = strmgr.get_stream_by_id(1).unwrap();
                let ts = stream.make_ts(Some(self.apts), None, None);
                self.apts += size as u64;
                self.src.read_packet(stream, ts, true, size)
            },
            b"VID3" => {
                let stream = strmgr.get_stream_by_id(0).unwrap();
                let ts = stream.make_ts(Some(self.vpts), None, None);
                self.vpts += 1;
                self.src.read_packet(stream, ts, true, size)
            },
            b"END*" => Err(DemuxerError::EOF),
            _ => Err(DemuxerError::InvalidData),
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}
impl<'a> NAOptionHandler for HighlanderFMVDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}
impl<'a> HighlanderFMVDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        HighlanderFMVDemuxer {
            src:        io,
            vpts:       0,
            apts:       0,
        }
    }
}

pub struct HighlanderFMVDemuxerCreator { }

impl DemuxerCreator for HighlanderFMVDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(HighlanderFMVDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "hl-fmv" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    // sample extracted from Highlander: The Last of the MacLeods unpublished game
    #[test]
    fn test_highlander_fmv_demux() {
        let mut file = File::open("assets/Game/0010.fmv").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = HighlanderFMVDemuxer::new(&mut br);
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
