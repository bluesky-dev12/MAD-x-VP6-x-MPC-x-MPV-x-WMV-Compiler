use nihav_core::frame::*;
use nihav_core::demuxers::*;

const RGB555_FORMAT: NAPixelFormaton = NAPixelFormaton { model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
                                        comp_info: [
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift: 10, comp_offs: 0, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  5, comp_offs: 1, next_elem: 2 }),
                                            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  0, comp_offs: 2, next_elem: 2 }),
                                            None, None],
                                        elem_size: 2, be: false, alpha: false, palette: false };
struct SGADemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    subtype:        u8,
    apts:           u64,
    abuf:           Vec<u8>,
    abuf2:          Vec<u8>,
    asize:          usize,
    no_ts_in_f9:    bool,
    ntsc:           bool,
}

impl<'a> SGADemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:            io,
            subtype:        0,
            apts:           0,
            abuf:           Vec::new(),
            abuf2:          Vec::new(),
            asize:          0,
            no_ts_in_f9:    false,
            ntsc:           false,
        }
    }
}

fn parse_smpte_time(src: &[u8], ntsc: bool) -> DemuxerResult<u64> {
    validate!(src.len() >= 4);
    let hours                           = src[0];
    let minutes                         = src[1];
    validate!(minutes < 60);
    let seconds                         = src[2];
    validate!(seconds < 60);
    let frame                           = src[3];
    if ntsc {
        validate!(frame < 60);
    } else {
        validate!(frame < 30);
    }

    let tot_min = u64::from(hours) * 60 + u64::from(minutes);
    let tot_sec = tot_min * 60 + u64::from(seconds);
    Ok(tot_sec * if ntsc { 60 } else { 30 } + u64::from(frame))
}

fn get_smpte_time(src: &mut ByteReader, ntsc: bool) -> DemuxerResult<u64> {
    let mut buf = [0; 4];
                                          src.read_buf(&mut buf)?;
    parse_smpte_time(&buf, ntsc)
}

impl<'a> DemuxCore<'a> for SGADemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let mut subtype                     = self.src.read_byte()?;
        match subtype {
            0xF1 => {
                                              self.src.read_skip(3)?;
                subtype                     = self.src.read_byte()?;
            },
            0xF4 => {
                                              self.src.read_skip(1)?;
                let csize                   = self.src.read_u16be()?;
                                              self.src.read_skip(usize::from(csize))?;
                subtype                     = self.src.read_byte()?;
            },
            0xF9 => {
                                              self.src.read_skip(3)?;
                if (self.src.peek_byte()? & 0x80) == 0 {
                                              self.src.read_skip(4)?;
                } else {
                    self.no_ts_in_f9 = true;
                }
                subtype                     = self.src.read_byte()?;
            },
            _ => {},
        };
        validate!(subtype >= 0x80);
        if !matches!(subtype, 0x81 | 0x85 | 0x86 | 0x89 | 0x8A) {
            return Err(DemuxerError::NotImplemented);
        }
        self.subtype = subtype;
        match subtype {
            0x81 | 0x8A => {
                                                  self.src.read_skip(9)?;
                let tile_w                      = self.src.read_byte()?;
                let tile_h                      = self.src.read_byte()?;
                validate!(tile_w > 0 && tile_h > 0);
                                                  self.src.seek(SeekFrom::Start(0))?;
                let vhdr = NAVideoInfo::new(usize::from(tile_w) * 8, usize::from(tile_h) * 8, false, RGB555_FORMAT);
                let vci = NACodecTypeInfo::Video(vhdr);
                let vinfo = NACodecInfo::new("dp-sga", vci, Some(vec![subtype]));
                if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 30, 0)).is_none() {
                    return Err(DemuxerError::MemoryError);
                }
            },
            0x85 | 0x86 => {
                let mut edata = vec![0; 0x201];
                edata[0] = subtype;
                                                  self.src.read_byte()?;
                let tile_w                      = self.src.read_byte()?;
                let tile_h                      = self.src.read_byte()?;
                validate!(tile_w > 0 && tile_h > 0);
                                                  self.src.read_skip(8)?;
                                                  self.src.read_buf(&mut edata[1..])?;
                if self.subtype == 0x85 {
                    self.asize                  = usize::from(self.src.read_u16be()?);
                }

                let vhdr = NAVideoInfo::new(usize::from(tile_w) * 8, usize::from(tile_h) * 8, false, RGB555_FORMAT);
                let vci = NACodecTypeInfo::Video(vhdr);
                let vinfo = NACodecInfo::new("dp-sga", vci, Some(edata));
                if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 30, 0)).is_none() {
                    return Err(DemuxerError::MemoryError);
                }

                let srate = 16000;
                let ahdr = NAAudioInfo::new(srate, 1, SND_U8_FORMAT, 1);
                let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
                if strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, srate, 0)).is_none() {
                    return Err(DemuxerError::MemoryError);
                }
            },
            0x89 => {
                                                  self.src.seek(SeekFrom::Start(0))?;
                let vhdr = NAVideoInfo::new(256, 160, false, RGB555_FORMAT);
                let vci = NACodecTypeInfo::Video(vhdr);
                let vinfo = NACodecInfo::new("dp-sga", vci, Some(vec![subtype]));
                if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 30, 0)).is_none() {
                    return Err(DemuxerError::MemoryError);
                }

                let srate = 22050;
                let ahdr = NAAudioInfo::new(srate, 1, SND_U8_FORMAT, 1);
                let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
                if strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, srate, 0)).is_none() {
                    return Err(DemuxerError::MemoryError);
                }
                let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
                if strmgr.add_stream(NAStream::new(StreamType::Audio, 2, ainfo, 1, srate, 0)).is_none() {
                    return Err(DemuxerError::MemoryError);
                }
            },
            _ => unreachable!(),
        };

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if !self.abuf.is_empty() {
            let mut buf = Vec::new();
            std::mem::swap(&mut buf, &mut self.abuf);

            if let Some(stream) = strmgr.get_stream(1) {
                let ts = stream.make_ts(Some(self.apts), None, None);
                self.apts += buf.len() as u64;
                return Ok(NAPacket::new(stream, ts, true, buf));
            }
        }
        if !self.abuf2.is_empty() {
            let mut buf = Vec::new();
            std::mem::swap(&mut buf, &mut self.abuf2);

            if let Some(stream) = strmgr.get_stream(2) {
                let ts = stream.make_ts(Some(self.apts), None, None);
                self.apts += buf.len() as u64;
                return Ok(NAPacket::new(stream, ts, true, buf));
            }
        }
        match self.subtype {
            0x81 | 0x8A => {
                let mut hdr = [0; 4];
                loop {
                    match                 self.src.read_buf(&mut hdr) {
                        Ok(_) => {},
                        Err(ByteIOError::ReadError) |
                        Err(ByteIOError::EOF) => return Err(DemuxerError::EOF),
                        Err(err) => return Err(err.into()),
                    };
                    let chunk_size = usize::from(read_u16le(&hdr[2..])?);
                    validate!(chunk_size > 8);
                    match hdr[0] {
                        0x81 | 0x8A => {
                            let mut buf = vec![0; chunk_size + 4];
                            buf[..4].copy_from_slice(&hdr);
                                          self.src.read_buf(&mut buf[4..])?;
                            let ts = parse_smpte_time(&buf[4..], self.ntsc)?;
                            let stream = strmgr.get_stream(0).unwrap();
                            let ts = NATimeInfo::new(Some(ts), None, None, stream.tb_num, stream.tb_den);
                            return Ok(NAPacket::new(stream, ts, false, buf));
                        },
                        0xF1 => {},
                        0xF9 => {
                            if !self.no_ts_in_f9 {
                                          self.src.read_skip(4)?;
                            }
                        },
                        _ =>              self.src.read_skip(chunk_size)?,
                    };
                    if (self.src.tell() & 1) != 0 {
                                          self.src.read_skip(1)?;
                    }
                }
            },
            0x85 => {
                let ts = match get_smpte_time(self.src, self.ntsc) {
                        Ok(val) => val,
                        Err(DemuxerError::IOError) => return Err(DemuxerError::EOF),
                        Err(err) => return Err(err),
                    };
                let pal_size            = self.src.read_u16be()?;
                let asize               = usize::from(self.src.read_u16be()?);
                validate!(asize >= self.asize);
                let full_size           = usize::from(self.src.read_u16be()?);
                validate!(full_size >= asize);
                let pal_off             = self.src.read_u16be()?;
                let vsize               = full_size - self.asize;
                let offset              = (asize - self.asize) as u16;
                let mut buf = vec![0; vsize + 6];
                if asize > 0 {
                    self.abuf.resize(self.asize - 1, 0);
                                          self.src.read_buf(&mut self.abuf)?;
                                          self.src.read_byte()?;
                }
                write_u16be(&mut buf,      pal_off)?;
                write_u16be(&mut buf[2..], pal_size)?;
                write_u16be(&mut buf[4..], offset)?;
                                          self.src.read_buf(&mut buf[6..])?;

                let stream = strmgr.get_stream(0).unwrap();
                let ts = NATimeInfo::new(Some(ts), None, None, stream.tb_num, stream.tb_den);
                Ok(NAPacket::new(stream, ts, false, buf))
            },
            0x86 => {
                let ts = match get_smpte_time(self.src, self.ntsc) {
                        Ok(val) => val,
                        Err(DemuxerError::IOError) => return Err(DemuxerError::EOF),
                        Err(err) => return Err(err),
                    };
                let asize               = usize::from(self.src.read_u16be()?);
                let vsize               = usize::from(self.src.read_u16be()?);
                let pal_off             = self.src.read_u16be()?;
                let pal_size            = self.src.read_u16be()?;
                let offset              = self.src.read_u16be()?;
                let mut buf = vec![0; vsize + 6];
                if asize > 0 {
                    self.abuf.resize(asize, 0);
                                          self.src.read_buf(&mut self.abuf)?;
                }
                write_u16be(&mut buf,      pal_off)?;
                write_u16be(&mut buf[2..], pal_size)?;
                write_u16be(&mut buf[4..], offset)?;
                                          self.src.read_buf(&mut buf[6..])?;

                let stream = strmgr.get_stream(0).unwrap();
                let ts = NATimeInfo::new(Some(ts), None, None, stream.tb_num, stream.tb_den);
                Ok(NAPacket::new(stream, ts, false, buf))
            },
            0x89 => {
                let mut hdr = [0; 4];
                loop {
                    match                 self.src.read_buf(&mut hdr) {
                        Ok(_) => {},
                        Err(ByteIOError::ReadError) |
                        Err(ByteIOError::EOF) => return Err(DemuxerError::EOF),
                        Err(err) => return Err(err.into()),
                    };
                    let chunk_size = usize::from(read_u16be(&hdr[2..])?);
                    validate!((hdr[0] & 0x80) != 0);
                    validate!(chunk_size > 8);
                    let end = self.src.tell() + (chunk_size as u64);
                    match hdr[0] {
                        0x89 => {
                            let ts = get_smpte_time(self.src, self.ntsc)?;
                            let asize       = usize::from(self.src.read_u16be()?);
                            let vsize       = usize::from(self.src.read_u16be()?);
                            validate!((asize & 0x7FFF) + vsize + 16 <= chunk_size);
                            let pal_size    = self.src.read_u16be()?;
                            let offset      = self.src.read_u16be()?;
                            validate!(usize::from(offset) <= vsize);
                            let mut buf = vec![0; vsize + 6];
                            if asize > 0 {
                                if (asize & 0x8000) == 0 {
                                    self.abuf.resize(asize, 0);
                                              self.src.read_buf(&mut self.abuf)?;
                                    self.abuf2.resize(asize, 0);
                                    self.abuf2.copy_from_slice(&self.abuf);
                                } else {
                                    let asize = asize & 0x7FFF;
                                    validate!((asize & 1) == 0);
                                    self.abuf.resize(asize / 2, 0);
                                    self.abuf2.resize(asize / 2, 0);
                                              self.src.read_buf(&mut self.abuf)?;
                                              self.src.read_buf(&mut self.abuf2)?;
                                }
                            }
                            write_u16be(&mut buf,      1/*pal_off*/)?;
                            write_u16be(&mut buf[2..], pal_size)?;
                            write_u16be(&mut buf[4..], offset)?;
                                              self.src.read_buf(&mut buf[6..])?;
                            validate!(self.src.tell() <= end);
                                              self.src.seek(SeekFrom::Start(end))?;

                            let stream = strmgr.get_stream(0).unwrap();
                            let ts = NATimeInfo::new(Some(ts), None, None, stream.tb_num, stream.tb_den);
                            return Ok(NAPacket::new(stream, ts, false, buf));
                        },
                        _ =>              self.src.read_skip(chunk_size)?,
                    };
                }
            },
            _ => unreachable!(),
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}

const DEMUXER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: "ntsc", description: "timestamps are 60fps instead of 30fps",
        opt_type: NAOptionDefinitionType::Bool },
];

impl<'a> NAOptionHandler for SGADemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { DEMUXER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in DEMUXER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    if let ("name", NAValue::Bool(ref bval)) = (option.name, &option.value) {
                        self.ntsc = *bval;
                    }
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            "ntsc" => Some(NAValue::Bool(self.ntsc)),
            _ => None,
        }
    }
}

pub struct SGADemuxerCreator { }

impl DemuxerCreator for SGADemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(SGADemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "sga" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    fn test_sga_demux(name: &str) {
        let mut file = File::open(name).unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = SGADemuxer::new(&mut br);
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
    fn test_sga_demux_81() {
        // samples from Double Switch game
        test_sga_demux("assets/Game/sga/ALEXSTIL.AVC");
        test_sga_demux("assets/Game/sga/DPLOGO.AVC");
    }
    #[test]
    fn test_sga_demux_85() {
        // sample from Night Trap game
        test_sga_demux("assets/Game/sga/CRMOVIE");
    }
    #[test]
    fn test_sga_demux_86() {
        // sample from Corpse Killer game
        test_sga_demux("assets/Game/sga/dplogo.dtv");
    }
}
