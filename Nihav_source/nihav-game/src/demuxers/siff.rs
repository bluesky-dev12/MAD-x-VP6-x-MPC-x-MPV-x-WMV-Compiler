use nihav_core::frame::*;
use nihav_core::demuxers::*;

const DEFAULT_FCP_DELAY: u64 = 100;
const DEFAULT_VBV_DELAY: u64 = 80;

#[derive(Clone,Copy,Debug,PartialEq)]
enum SIFFType {
    None,
    FCP,
    VBV,
    Sound,
}

struct SIFFDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    subtype:    SIFFType,
    size:       u32,
    ablock:     usize,
    nframes:    usize,
    cframe:     usize,
    vpts:       u64,
    abuf:       Vec<u8>,
    apts:       u64,
    ver:        u8,
}

impl<'a> SIFFDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:        io,
            subtype:    SIFFType::None,
            size:       0,
            ablock:     0,
            nframes:    0,
            cframe:     0,
            vpts:       0,
            abuf:       Vec::new(),
            apts:       0,
            ver:        0,
        }
    }

    fn parse_fcp_header(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<()> {
        let tag                         = self.src.read_tag()?;
        validate!(&tag == b"FCHD");
        let hdr_size                    = self.src.read_u32be()? as usize;
        validate!(hdr_size >= 16);
        let mut flags = vec![0; 2];
                                          self.src.read_buf(&mut flags)?;
        let width                       = self.src.read_u16le()? as usize;
        let height                      = self.src.read_u16le()? as usize;
        validate!(width > 0 && height > 0);
        self.nframes                    = self.src.read_u16le()? as usize;
                                          self.src.read_skip(8)?;
                                          self.src.read_skip(hdr_size - 16)?;

        let vhdr = NAVideoInfo::new(width, height, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("beam-fcp", vci, Some(flags));
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 1000, self.nframes as u64 * DEFAULT_FCP_DELAY)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        self.ablock = 1;
        let srate = 22050;
        let ahdr = NAAudioInfo::new(srate, 1, SND_U8_FORMAT, 1);
        let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
        if strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, srate, 0)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        self.vpts = 0;
        self.apts = 0;
        self.cframe = 0;

        Ok(())
    }
    fn get_fcp_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.cframe >= self.nframes {
            return Err(DemuxerError::EOF);
        }
        let size                        = self.src.read_u16le()? as usize;
        validate!(size > 8);
        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(Some(self.vpts), None, None);
        let kframe = self.vpts == 0;
        self.cframe += 1;
        let pkt                         = self.src.read_packet(stream, ts, kframe, size - 2)?;
        let buf = pkt.get_buffer();

        let mut mr = MemoryReader::new_read(buf.as_slice());
        let mut br = ByteReader::new(&mut mr);
        let asize                       = br.read_u16le()? as usize;
        let duration                    = br.read_u16le()? as u64;
        validate!(asize < buf.len());
        if asize > 0 {
            let nclrs                   = br.read_u16le()? as usize;
            if nclrs > 0 {
                                          br.read_skip(nclrs * 3 + 2)?;
            }
            self.abuf.resize(asize, 0);
                                          br.read_buf(&mut self.abuf)?;
        }

        if duration > 0 {
            self.vpts += duration;
        } else {
            self.vpts += DEFAULT_FCP_DELAY;
        }

        Ok(pkt)
    }

    fn parse_vbv_header(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<()> {
        let tag                         = self.src.read_tag()?;
        validate!(&tag == b"VBHD");
        let hdr_size                    = self.src.read_u32be()? as usize;
        validate!(hdr_size >= 32);
        let version                     = self.src.read_u16le()?;
        validate!(version == 1 || version == 2);
        self.ver = version as u8;
        let width                       = self.src.read_u16le()? as usize;
        let height                      = self.src.read_u16le()? as usize;
        validate!(width > 0 && height > 0);
                                          self.src.read_skip(4)?;
        self.nframes                    = self.src.read_u16le()? as usize;
        let flags                       = self.src.read_u16le()?;
        let bits = flags as u8;
        let channels = if (flags & 0x100) != 0 { 2 } else { 1 };
        validate!(bits == 0 || bits >= 8);
        let srate                       = self.src.read_u16le()? as u32;
        self.ablock = (bits as usize) * (channels as usize) / 8;
                                          self.src.read_skip(16)?;
                                          self.src.read_skip(hdr_size - 32)?;

        let mut vhdr = NAVideoInfo::new(width, height, false, if version == 1 { PAL8_FORMAT } else { RGB565_FORMAT });
        vhdr.bits = version as u8 * 8;
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("beam-video", vci, None);
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 1000, self.nframes as u64 * DEFAULT_VBV_DELAY)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        if srate > 0 && bits > 0 {
            let ahdr = NAAudioInfo::new(srate, channels, if bits == 8 { SND_U8_FORMAT } else { SND_S16_FORMAT }, self.ablock);
            let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
            if strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, srate, 0)).is_none() {
                return Err(DemuxerError::MemoryError);
            }
        }

        self.vpts = 0;
        self.apts = 0;
        self.cframe = 0;

        Ok(())
    }
    fn get_vbv_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.cframe >= self.nframes {
            return Err(DemuxerError::EOF);
        }
        let size                        = self.src.read_u32le()? as usize;
        validate!(size > 6);
        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(Some(self.vpts), None, None);
        let kframe = self.vpts == 0;
        self.cframe += 1;
        let pkt                         = self.src.read_packet(stream, ts, kframe, size - 4)?;
        let buf = pkt.get_buffer();

        let mut mr = MemoryReader::new_read(buf.as_slice());
        let mut br = ByteReader::new(&mut mr);
        let flags                       = br.read_u16le()?;
        if (flags & 0x01) != 0 {
                                          br.read_skip(4)?;
        }
        if (flags & 0x04) != 0 {
            let asize                   = br.read_u32le()? as usize;
            validate!((asize > 4) && asize < (buf.len() - (br.tell() as usize)));
            self.abuf.resize(asize - 4, 0);
                                          br.read_buf(&mut self.abuf)?;
        }
        if (flags & 0x08) != 0 {
            let vsize                   = br.read_u32le()? as usize;
            validate!(vsize > 4);
                                          br.read_skip(vsize - 4)?;
        }
        if (flags & 0x10) != 0 {
            let psize                   = br.read_u32le()? as usize;
            validate!(psize > 4);
                                          br.read_skip(psize - 4)?;
        }
        let delay = if (flags & 0x20) != 0 {
                                          br.read_u16le()? as u64
            } else { 0 };
        self.vpts += if delay > 0 { delay } else { DEFAULT_VBV_DELAY };

        Ok(pkt)
    }

    fn parse_snd_header(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<()> {
        let tag                         = self.src.read_tag()?;
        validate!(&tag == b"SHDR");
        let hdr_size                    = self.src.read_u32be()? as usize;
        validate!(hdr_size >= 8);
        let duration                    = self.src.read_u32le()? as u64;
        let srate                       = self.src.read_u16le()? as u32;
        let flags                       = self.src.read_u16le()?;
        let bits = flags as u8;
        validate!(bits >= 8);
        let channels = if (flags & 0x100) != 0 { 2 } else { 1 };
        self.ablock = (bits as usize) * (channels as usize);
                                          self.src.read_skip(hdr_size - 8)?;

        let fmt = match bits {
                8 => SND_U8_FORMAT,
               16 => SND_S16_FORMAT,
               12 => NASoniton::new(12, SONITON_FLAG_PACKED | SONITON_FLAG_SIGNED),
                _ => return Err(DemuxerError::NotImplemented),
            };
        let ahdr = NAAudioInfo::new(srate, channels, fmt, self.ablock);
        let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
        if strmgr.add_stream(NAStream::new(StreamType::Audio, 0, ainfo, 1, srate, duration)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        Ok(())
    }
    fn get_snd_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.size == 0 {
            return Err(DemuxerError::EOF);
        }
        let cur_size = self.size.min(1024 * (self.ablock as u32));

        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(None, None, None);
        let pkt                         = self.src.read_packet(stream, ts, true, cur_size as usize)?;
        self.size -= cur_size;

        Ok(pkt)
    }
}

impl<'a> DemuxCore<'a> for SIFFDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let magic                       = self.src.read_tag()?;
        validate!(&magic == b"SIFF");
        self.size                       = self.src.read_u32be()?;
        let tag                         = self.src.read_tag()?;
        self.subtype = match &tag {
                b"FCPK" => SIFFType::FCP,
                b"VBV1" => SIFFType::VBV,
                b"SOUN" => SIFFType::Sound,
                _ => return Err(DemuxerError::NotImplemented),
            };

        match self.subtype {
            SIFFType::FCP   => self.parse_fcp_header(strmgr)?,
            SIFFType::VBV   => self.parse_vbv_header(strmgr)?,
            SIFFType::Sound => self.parse_snd_header(strmgr)?,
            _ => unreachable!(),
        };

        let tag                         = self.src.read_tag()?;
        validate!(&tag == b"BODY");
        let body_size                   = self.src.read_u32be()?;
        validate!(self.src.tell() + u64::from(body_size) <= u64::from(self.size) + 8);
        self.size = body_size;

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if !self.abuf.is_empty() {
            let mut buf = Vec::new();
            std::mem::swap(&mut buf, &mut self.abuf);

            if let Some(stream) = strmgr.get_stream(1) {
                let ts = stream.make_ts(Some(self.apts), None, None);
                self.apts += (buf.len() / self.ablock) as u64;
                return Ok(NAPacket::new(stream, ts, true, buf));
            }
        }
        match self.subtype {
            SIFFType::FCP   => self.get_fcp_frame(strmgr),
            SIFFType::VBV   => self.get_vbv_frame(strmgr),
            SIFFType::Sound => self.get_snd_frame(strmgr),
            _ => unreachable!(),
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for SIFFDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct SIFFDemuxerCreator { }

impl DemuxerCreator for SIFFDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(SIFFDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "siff" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    fn test_siff_demux(name: &str) {
        let mut file = File::open(name).unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = SIFFDemuxer::new(&mut br);
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
    fn test_siff_demux_fcp() {
        // sample from The Dame was Loaded game
        test_siff_demux("assets/Game/siff/BEAM.FCP");
    }
    #[test]
    fn test_siff_demux_anim_8bit() {
        // sample from Lost Vikings 2 game
        test_siff_demux("assets/Game/siff/BEAM.VB");
    }
    #[test]
    fn test_siff_demux_anim_16bit() {
        // sample from Alien Earth game
        test_siff_demux("assets/Game/siff/beamlogo.vbc");
    }
    #[test]
    fn test_siff_demux_snd() {
        // sample from The Dame was Loaded game
        test_siff_demux("assets/Game/siff/01AFIRST.SON");
        // sample from Lost Vikings 2 game
        test_siff_demux("assets/Game/siff/01THEME.SON");
    }
}
