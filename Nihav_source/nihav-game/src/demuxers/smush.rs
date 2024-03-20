use nihav_core::frame::*;
use nihav_core::demuxers::*;

struct SmushDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    old:        bool,
    size:       u64,

    nframes:    usize,
    chunks:     Vec<u8>,

    keyframe:   bool,
    cur_frame:  usize,
    frme_end:   u64,
    asize:      u64,
}

fn parse_iact(br: &mut ByteReader, end: u64, arate: &mut u32, abits: &mut u8, chans: &mut u8, mcmp: bool) -> DemuxerResult<()> {
    if !mcmp {
                                          br.read_skip(14)?;
    }
    let tag                             = br.read_tag()?;
    if &tag != b"iMUS" {
        if mcmp {
            return Err(DemuxerError::InvalidData);
        }
        *arate = 22050;
        *abits = 16;
        *chans = 2;
        return Ok(());
    }
                                          br.read_skip(4)?;
    while br.tell() < end {
        let tag                         = br.read_tag()?;
        let size                        = u64::from(br.read_u32be()?);
        match &tag {
            b"MAP " => {
                let cend = br.tell() + size;
                while br.tell() < cend {
                    let tag             = br.read_tag()?;
                    let size            = u64::from(br.read_u32be()?);
                    match &tag {
                        b"FRMT" => {
                            validate!(size == 20);
                                          br.read_u32be()?;
                                          br.read_u32be()?;
                            let bits    = br.read_u32be()?;
                            validate!(bits > 0 && bits <= 16);
                            *abits = bits as u8;
                            *arate      = br.read_u32be()?;
                            let c       = br.read_u32be()?;
                            validate!(c == 1 || c == 2);
                            *chans = c as u8;
                            return Ok(());
                        },
                        _ =>              br.read_skip(size as usize)?,
                    };
                }
            },
            b"DATA" => return Err(DemuxerError::InvalidData),
            _  =>                         br.read_skip(size as usize)?,
        };
    }
    Err(DemuxerError::InvalidData)
}

impl<'a> SmushDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        SmushDemuxer {
            src:        io,

            old:        false,
            size:       0,

            nframes:    0,
            chunks:     Vec::new(),

            keyframe:   false,
            cur_frame:  0,
            frme_end:   0,
            asize:      0,
        }
    }
    fn parse_anim_header(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<()> {
        let src = &mut self.src;

        let tag                         = src.read_tag()?;
        validate!(&tag == b"AHDR");
        let size                        = u64::from(src.read_u32be()?);
        validate!(size >= 768 + 6);
        let end = src.tell() + size;
        validate!(end < self.size);
        let version                     = src.read_u16le()?;
        validate!(version < 3);
        self.nframes                    = src.read_u16le()? as usize;
        validate!(self.nframes != 0);
                                          src.read_skip(2)?; //max FRME size
        let mut edata = vec![0; 768 + 1];
        edata[0] = version as u8;
                                          src.read_buf(&mut edata[1..][..768])?;
                                          src.read_skip(size as usize - 768 - 6)?;

        let start = src.tell();
        let mut size = 0;
        while size == 0 {
            let tag                     = src.read_tag()?;
            validate!(&tag == b"FRME");
            size                        = u64::from(src.read_u32be()?);
        }

        let end = src.tell() + size;
        validate!(end <= self.size + 8); // some NUTs feature slightly incorrect total size

        let mut width = 0;
        let mut height = 0;
        let mut aname = "";
        let mut arate = 0;
        let mut abits = 0;
        let mut chans = 0;

        while src.tell() < end {
            let tag                     = src.read_tag()?;
            let size                    = u64::from(src.read_u32be()?);

            let tend = src.tell() + size;
            validate!(tend <= end);
            match &tag {
                b"FOBJ" => {
                    validate!(size >= 10);
                    let _codec          = src.read_u16le()?;
                    let x               = src.read_u16le()? as i16;
                    let y               = src.read_u16le()? as i16;
                    if x == 0 && y == 0 && width == 0 && height == 0 {
                        width           = src.read_u16le()? as usize;
                        height          = src.read_u16le()? as usize;
                    } else {
                        let w           = src.read_u16le()? as usize;
                        let h           = src.read_u16le()? as usize;
                        if x == 0 && y == 0 && w >= width && h >= height {
                            width  = w;
                            height = h;
                        }
                    }
                                          src.read_skip((size - 10) as usize)?;
                },
                b"IACT" => {
                    validate!(size > 8);
                    let end = src.tell() + size;
                    let opcode          = src.read_u16le()?;
                    let flags           = src.read_u16le()?;
                    if (opcode == 8) && (flags == 0x2E) {
                        if parse_iact(src, end, &mut arate, &mut abits, &mut chans, false).is_ok() {
                            aname = "smush-iact";
                        }
                        validate!(src.tell() <= end);
                    }
                                          src.seek(SeekFrom::Start(end))?;
                },
                b"PSAD" => {
                    aname = "pcm";
                    arate = 11025;
                    abits = 8;
                    chans = 2;
                                          src.read_skip(size as usize)?;
                },
                _ => {                    src.read_skip(size as usize)?; },
            };
            if (src.tell() & 1) != 0 {
                if let Ok(0) = src.peek_byte() {
                                          src.read_skip(1)?;
                }
            }
        }
        // hack
        width  = width.max(320);
        height = height.max(200);
                                          src.seek(SeekFrom::Start(start))?;
        self.frme_end = start;

        let vhdr = NAVideoInfo::new(width, height, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("smushv1", vci, Some(edata));
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 15, self.nframes as u64)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        if !aname.is_empty() {
            validate!(arate > 0);
            let mut fmt = SND_S16_FORMAT;
            match aname {
                "pcm" => { fmt = SND_U8_FORMAT; },
                "smush-iact" => { fmt.bits = abits; fmt.packed = true; },
                _ => {},
            };
            let ahdr = NAAudioInfo::new(arate, chans, fmt, 0);
            let ainfo = NACodecInfo::new(aname, NACodecTypeInfo::Audio(ahdr), None);
            if strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, arate, 0)).is_none() {
                return Err(DemuxerError::MemoryError);
            }
        }

        Ok(())
    }
    fn parse_sanm_header(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<()> {
        let src = &mut self.src;

        let tag                         = src.read_tag()?;
        validate!(&tag == b"SHDR");
        let size                        = u64::from(src.read_u32be()?);
        validate!(src.tell() + size <= self.size);
        validate!(size >= 0x426);

        let maj_ver                     = src.read_byte()?;
        let min_ver                     = src.read_byte()?;
        if maj_ver != 1 || min_ver != 0 {
            return Err(DemuxerError::NotImplemented);
        }
        self.nframes                    = src.read_u16le()? as usize;
        let _xoff                       = src.read_u16le()? as usize;
        let _yoff                       = src.read_u16le()? as usize;
        let width                       = src.read_u16le()? as usize;
        let height                      = src.read_u16le()? as usize;
        let _imgtype                    = src.read_u16le()?;
        let frame_delay                 = src.read_u32le()?;
        let _max_frame_size             = src.read_u32le()?;
                                          src.read_skip(1024)?; // palette
                                          src.read_skip((size as usize) - 0x416)?;

        let tag                         = src.read_tag()?;
        validate!(&tag == b"FLHD");
        let size                        = u64::from(src.read_u32be()?);
        let end = src.tell() + size;

        let mut arate = 0;
        let mut chans = 0;
        let mut alen  = 0;
        while src.tell() < end {
            let tag                     = src.read_tag()?;
            if src.tell() == end { break; }
            let size                    = src.read_u32be()?;
            match &tag {
                b"Wave" => {
                    validate!(size >= 8);
                    arate               = src.read_u32le()?;
                    let cc              = src.read_u32le()?;
                    validate!(cc == 1 || cc == 2);
                    chans = cc as u8;
                    if size >= 12 {
                        alen            = u64::from(src.read_u32le()? / cc / 2);
                                          src.read_skip((size as usize) - 12)?;
                    }
                },
                _ =>                      src.read_skip(size as usize)?,
            };
        }
        validate!(src.tell() == end);

        let vhdr = NAVideoInfo::new(width, height, false, RGB565_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("smushv2", vci, None);
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, frame_delay, 1000000, self.nframes as u64)).is_none() {
            return Err(DemuxerError::MemoryError);
        }
        if arate != 0 {
            let ahdr = NAAudioInfo::new(arate, chans, SND_S16P_FORMAT, 0);
            let ainfo = NACodecInfo::new("smush-vima", NACodecTypeInfo::Audio(ahdr), None);
            if strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, arate, alen)).is_none() {
                return Err(DemuxerError::MemoryError);
            }
        }

        Ok(())
    }

    fn queue_chunk(&mut self, tag: [u8; 4], size: usize) -> DemuxerResult<()> {
        self.chunks.extend_from_slice(&tag);
        let start = self.chunks.len();
        let nlen = start + size + 4;
        self.chunks.resize(nlen, 0);
        write_u32be(&mut self.chunks[start..], size as u32).unwrap();
                                          self.src.read_buf(&mut self.chunks[start + 4..])?;
        Ok(())
    }
    fn get_frame_anim(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        loop {
            if self.src.tell() >= self.frme_end {
                if !self.chunks.is_empty() {
                    let mut buf = Vec::new();
                    std::mem::swap(&mut self.chunks, &mut buf);
                    let stream = strmgr.get_stream(0).unwrap();
                    let ts = stream.make_ts(Some(self.cur_frame as u64 - 1), None, None);
                    return Ok(NAPacket::new(stream, ts, false, buf));
                }
                if self.cur_frame == self.nframes {
                    return Err(DemuxerError::EOF);
                }
                let tag                         = self.src.read_tag()?;
                validate!(&tag == b"FRME");
                let size                        = u64::from(self.src.read_u32be()?);
                self.frme_end = self.src.tell() + size;

                self.chunks.clear();
                self.cur_frame += 1;
                if size == 0 {
                    continue;
                }
            }
            let tag                     = self.src.read_tag()?;
            let size                    = u64::from(self.src.read_u32be()?);
            let tend = self.src.tell() + size;
            validate!(tend <= self.frme_end);
            match &tag {
                b"STOR" | b"FTCH" | b"NPAL" | b"XPAL" | b"FOBJ" => {
                    self.queue_chunk(tag, size as usize)?;
                },
                b"IACT" => {
                    validate!(size >= 4);
                    let opcode          = self.src.read_u16le()?;
                    let flags           = self.src.read_u16le()?;
                    if (opcode == 8) && (flags == 0x2E) {
                        if let Some(stream) = strmgr.get_stream(1) {
                            let ts = stream.make_ts(None, None, None);

                            let mut buf = vec![0; size as usize];
                            write_u16le(&mut buf[0..2], opcode).unwrap();
                            write_u16le(&mut buf[2..4], flags).unwrap();
                                          self.src.read_buf(&mut buf[4..])?;

                            if (self.src.tell() & 1) == 1 {
                                if let Ok(0) = self.src.peek_byte() {
                                          self.src.read_skip(1)?;
                                }
                            }
                            return Ok(NAPacket::new(stream, ts, true, buf));
                        }
                    }
                                          self.src.read_skip((size as usize) - 4)?;
                },
                b"PSAD" => {
                    if size > 0x30 {
                                          self.src.read_skip(0x30)?;
                        if let Some(stream) = strmgr.get_stream(1) {
                            let audio_size = size - 0x30;
                            let ts = stream.make_ts(Some(self.asize), None, None);
                            let pkt = self.src.read_packet(stream, ts, true, audio_size as usize)?;
                            self.asize += audio_size;
                            if (self.src.tell() & 1) == 1 {
                                if let Ok(0) = self.src.peek_byte() {
                                          self.src.read_skip(1)?;
                                }
                            }
                            return Ok(pkt);
                        } else {
                                          self.src.read_skip((size - 0x30) as usize)?;
                        }
                    } else {
                                          self.src.read_skip(size as usize)?;
                    }
                },
                _ => {
                                          self.src.read_skip(size as usize)?;
                },
            };
            if (self.src.tell() & 1) == 1 {
                if let Ok(0) = self.src.peek_byte() {
                                          self.src.read_skip(1)?;
                }
            }
        }
    }
    fn get_frame_sanm(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        loop {
            if self.src.tell() >= self.frme_end {
                if !self.chunks.is_empty() {
                    let mut buf = Vec::new();
                    std::mem::swap(&mut self.chunks, &mut buf);
                    let stream = strmgr.get_stream(0).unwrap();
                    let ts = stream.make_ts(Some(self.cur_frame as u64 - 1), None, None);
                    return Ok(NAPacket::new(stream, ts, self.keyframe, buf));
                }
                if self.cur_frame == self.nframes {
                    return Err(DemuxerError::EOF);
                }
                let tag                         = self.src.read_tag()?;
                let size                        = u64::from(self.src.read_u32be()?);
                self.frme_end = self.src.tell() + size;
                match &tag {
                    b"FLHD" => { self.keyframe = true; },
                    b"FRME" => { self.keyframe = false; },
                    _       => {
                                                  self.src.read_skip(size as usize)?;
                        continue;
                    },
                };

                self.chunks.clear();
                self.cur_frame += 1;
                if size == 0 {
                    continue;
                }
            }
            let tag                     = self.src.read_tag()?;
            if self.src.tell() >= self.frme_end { // happens after some Wave tags
                continue;
            }
            let size                    = u64::from(self.src.read_u32be()?);
            let tend = self.src.tell() + size;
            validate!(tend <= self.frme_end);
            match &tag {
                b"Bl16" => {
                    self.queue_chunk(tag, size as usize)?;
                },
                b"Wave" => {
                    if let Some(stream) = strmgr.get_stream(1) {
                        let mut buf = [0; 12];
                        let mut nsamples = 0;
                        if size >= 12 {
                                          self.src.peek_buf(&mut buf)?;
                            nsamples = read_u32be(&buf[0..])?;
                            if nsamples == 0xFFFFFFFF {
                                nsamples = read_u32be(&buf[8..])?;
                            }
                        }

                        let mut ts = stream.make_ts(None, None, None);
                        if nsamples != 0 {
                            ts.pts = Some(self.asize);
                            self.asize += u64::from(nsamples);
                        }
                        let pkt = self.src.read_packet(stream, ts, true, size as usize)?;
                        return Ok(pkt);
                    } else {
                                          self.src.read_skip(size as usize)?;
                    }
                },
                _ => {
//println!("unknown tag {}{}{}{} size {:X} @ {:X}", tag[0] as char, tag[1] as char, tag[2] as char, tag[3] as char, size, self.src.tell() - 8);
                                          self.src.read_skip(size as usize)?;
                },
            };
        }
    }
}

impl<'a> DemuxCore<'a> for SmushDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let magic                       = self.src.read_tag()?;
        match &magic {
            b"ANIM" => {
                self.old = true;
            },
            b"SANM" => {
                self.old = false;
            },
            _ => return Err(DemuxerError::InvalidData),
        };
        self.size                       = u64::from(self.src.read_u32be()?);
        if self.old {
            self.parse_anim_header(strmgr)?;
        } else {
            self.parse_sanm_header(strmgr)?;
        }

        self.cur_frame = 0;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.cur_frame > self.nframes { return Err(DemuxerError::EOF); }
        if self.old {
            self.get_frame_anim(strmgr)
        } else {
            self.get_frame_sanm(strmgr)
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for SmushDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct SmushDemuxerCreator { }

impl DemuxerCreator for SmushDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(SmushDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "smush" }
}


struct MCMPDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    cur_frame:  usize,

    offsets:    Vec<u64>,
    sizes:      Vec<u32>,
    samples:    Vec<u32>,
    pts:        Vec<u64>,
}

impl<'a> MCMPDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        MCMPDemuxer {
            src:        io,
            cur_frame:  0,

            offsets:    Vec::new(),
            sizes:      Vec::new(),
            samples:    Vec::new(),
            pts:        Vec::new(),
        }
    }
}

impl<'a> DemuxCore<'a> for MCMPDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let magic                       = self.src.read_tag()?;
        validate!(&magic == b"MCMP");
        let nframes                     = self.src.read_u16be()? as usize;
        validate!(nframes > 1);
        let cmp                         = self.src.read_byte()?;
        let size1                       = self.src.read_u32be()?;
        let hdr_size                    = self.src.read_u32be()?;
        validate!(cmp == 0 && size1 == hdr_size);

        let size = nframes - 1;
        self.offsets = Vec::with_capacity(size);
        self.sizes   = Vec::with_capacity(size);
        self.samples = Vec::with_capacity(size);
        self.pts     = Vec::with_capacity(size);

        let mut start = 0;
        let mut pts = 0;
        for _ in 1..nframes {
            let compr                   = self.src.read_byte()?;
            if compr != 1 {
                return Err(DemuxerError::NotImplemented);
            }
            let samples                 = self.src.read_u32be()? / 2;
            let size                    = self.src.read_u32be()?;
            self.offsets.push(start);
            self.sizes.push(size);
            self.samples.push(samples);
            self.pts.push(pts);

            start += u64::from(size);
            pts += u64::from(samples);
        }

        let codecs_desc_size            = self.src.read_u16be()? as usize;
        // todo check it's NULL+VIMA
                                          self.src.read_skip(codecs_desc_size)?;
        let data_start = self.src.tell() + u64::from(hdr_size);
        let mut arate = 0;
        let mut abits = 0;
        let mut chans = 0;
        if let Ok([b'R', b'I', b'F', b'F']) = self.src.peek_tag() {
            validate!(hdr_size >= 44);
                                          self.src.read_skip(22)?;
            let c                       = self.src.read_u16le()?;
            validate!(c == 1 || c == 2);
            chans = c as u8;
            arate                       = self.src.read_u32le()?;
            validate!(arate > 0);
        } else {
            parse_iact(self.src, data_start, &mut arate, &mut abits, &mut chans, true)?;
        }
        if chans == 2 {
            for (samp, pts) in self.samples.iter_mut().zip(self.pts.iter_mut()) {
                validate!((*samp & 1) == 0);
                *samp >>= 1;
                *pts  >>= 1;
            }
            pts >>= 1;
        }

        let ahdr = NAAudioInfo::new(arate, chans, SND_S16_FORMAT, 0);
        let ainfo = NACodecInfo::new("smush-vima", NACodecTypeInfo::Audio(ahdr), None);
        if strmgr.add_stream(NAStream::new(StreamType::Audio, 0, ainfo, 1, arate, pts)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        seek_index.mode = SeekIndexMode::Present;
        seek_index.add_stream(0);
        let index = seek_index.get_stream_index(0).unwrap();
        for (i, (off, &pts)) in self.offsets.iter_mut().zip(self.pts.iter()).enumerate() {
            *off += data_start;
            index.add_entry(SeekEntry { time: pts * 1000 / u64::from(arate), pts, pos: i as u64 });
        }
        index.filled = true;

        self.cur_frame = 0;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        let idx = self.cur_frame;
        if idx >= self.offsets.len() { return Err(DemuxerError::EOF); }
                                          self.src.seek(SeekFrom::Start(self.offsets[idx]))?;
        let mut buf = vec![0; self.sizes[idx] as usize + 4];
        write_u32be(&mut buf, self.samples[idx])?;
                                          self.src.read_buf(&mut buf[4..])?;

        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(Some(self.pts[idx]), None, None);

        self.cur_frame += 1;

        Ok(NAPacket::new(stream, ts, true, buf))
    }

    fn seek(&mut self, time: NATimePoint, seek_index: &SeekIndex) -> DemuxerResult<()> {
        if let Some(ret) = seek_index.find_pos(time) {
            self.cur_frame = ret.pos as usize;
            Ok(())
        } else {
            Err(DemuxerError::SeekError)
        }
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for MCMPDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct MCMPDemuxerCreator { }

impl DemuxerCreator for MCMPDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(MCMPDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "smush-mcmp" }
}


#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_smush_demux_anim_v1() {
        // sample from Rebel Assault game
        let mut file = File::open("assets/Game/smush/c1block.anm").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = SmushDemuxer::new(&mut br);
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
    fn test_smush_demux_anim_v2() {
        // sample from The Dig
        let mut file = File::open("assets/Game/smush/PIGOUT.SAN").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = SmushDemuxer::new(&mut br);
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
    fn test_smush_demux_sanm() {
        // sample from Grim Fandango
        let mut file = File::open("assets/Game/smush/lol.snm").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = SmushDemuxer::new(&mut br);
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
    fn test_mcmp_demux_imus() {
        // sample from Grim Fandango
        let mut file = File::open("assets/Game/smush/1104 - Lupe.IMC").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = MCMPDemuxer::new(&mut br);
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
    fn test_mcmp_demux_wav() {
        // sample from Grim Fandango
        let mut file = File::open("assets/Game/smush/breadpor.WAV").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = MCMPDemuxer::new(&mut br);
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
