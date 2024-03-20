use nihav_core::demuxers::*;

struct IVFDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    nframes:        u32,
    frameno:        u32,
}

impl<'a> IVFDemuxer<'a> {
    fn new(src: &'a mut ByteReader<'a>) -> Self {
        IVFDemuxer {
            src,
            nframes:    0,
            frameno:    0,
        }
    }
}

impl<'a> DemuxCore<'a> for IVFDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let tag                         = self.src.read_tag()?;
        validate!(&tag == b"DKIF");
        let ver                         = self.src.read_u16le()?;
        validate!(ver == 0);
        let hdr_len                     = self.src.read_u16le()? as usize;
        validate!(hdr_len >= 32);
        let fcc                         = self.src.read_tag()?;
        let codec_name = match &fcc {
                b"VP80" => "vp8",
                _       => "unknown",
            };
        let width                       = self.src.read_u16le()? as usize;
        let height                      = self.src.read_u16le()? as usize;
        validate!(width > 0 && height > 0);
        let tb_den                      = self.src.read_u32le()?;
        let tb_num                      = self.src.read_u32le()?;
        self.nframes                    = self.src.read_u32le()?;
        self.frameno = 0;

        self.src.seek(SeekFrom::Start(hdr_len as u64))?;

        let vci = NACodecTypeInfo::Video(NAVideoInfo::new(width, height, false, YUV420_FORMAT));
        let vinfo = NACodecInfo::new(codec_name, vci, None);
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, tb_num, tb_den, u64::from(self.nframes))).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        while self.frameno < self.nframes {
            let fsize                   = self.src.read_u32le()? as usize;
            let tstamp                  = self.src.read_u64le()?;
            self.frameno += 1;

            if fsize == 0 {
                continue;
            }

            if let Some(stream) = strmgr.get_stream(0) {
                let ts = stream.make_ts(Some(tstamp), None, None);
                return self.src.read_packet(stream, ts, false, fsize);
            } else {
                return Err(DemuxerError::InvalidData);
            }
        }
        Err(DemuxerError::EOF)
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for IVFDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct IVFDemuxerCreator { }

impl DemuxerCreator for IVFDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(IVFDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "dkivf" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_ivf_demux() {
        // sample is from the official VP8 test bitstream set
        let mut file = File::open("assets/Duck/VP8/vp80-00-comprehensive-001.ivf").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = IVFDemuxer::new(&mut br);
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
}
