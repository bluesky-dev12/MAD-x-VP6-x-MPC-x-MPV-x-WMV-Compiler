use nihav_core::frame::*;
use nihav_core::demuxers::*;

struct BMVDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    vid_id:     usize,
    aud_id:     usize,
    vpos:       u64,
    apos:       u64,
    pkt_buf:    Vec<NAPacket>,
}

impl<'a> DemuxCore<'a> for BMVDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let vhdr = NAVideoInfo::new(640, 429, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("bmv-video", vci, None);
        self.vid_id = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 12, 0)).unwrap();

        let ahdr = NAAudioInfo::new(22050, 2, SND_S16_FORMAT, 1);
        let ainfo = NACodecInfo::new("bmv-audio", NACodecTypeInfo::Audio(ahdr), None);
        self.aud_id = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, 22050, 0)).unwrap();

        self.vpos       = 0;
        self.apos       = 0;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if !self.pkt_buf.is_empty() {
            return Ok(self.pkt_buf.pop().unwrap());
        }

        loop {
            let ctype                   = self.src.read_byte()?;
            if ctype == 0 { // NOP chunk
                continue;
            }
            if ctype == 1 { return Err(DemuxerError::EOF); }
            let size                    = self.src.read_u24le()? as usize;
            validate!(size > 0);
            let asize;
            if (ctype & 0x20) != 0 {
                let nblocks             = self.src.peek_byte()?;
                asize = (nblocks as usize) * 65 + 1;
                validate!(asize < size);
                let stream = strmgr.get_stream(self.aud_id).unwrap();
                let ts = stream.make_ts(Some(self.apos), None, None);
                let apkt = self.src.read_packet(stream, ts, false, asize)?;
                self.apos += u64::from(nblocks) * 32;
                self.pkt_buf.push(apkt);
            } else {
                asize = 0;
            }
            let mut buf: Vec<u8> = vec![0; size - asize + 1];
            buf[0] = ctype;
            self.src.read_buf(&mut buf[1..])?;

            let stream = strmgr.get_stream(self.vid_id).unwrap();
            let ts = stream.make_ts(Some(self.vpos), None, None);
            let pkt = NAPacket::new(stream, ts, (ctype & 3) == 3, buf);

            self.vpos += 1;
            return Ok(pkt);
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for BMVDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> BMVDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:        io,
            vid_id:     0,
            aud_id:     0,
            vpos:       0,
            apos:       0,
            pkt_buf:    Vec::with_capacity(1),
        }
    }
}

pub struct BMVDemuxerCreator { }

impl DemuxerCreator for BMVDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(BMVDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "bmv" }
}

struct BMV3Demuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    vid_id:     usize,
    aud_id:     usize,
    vpos:       u64,
    apos:       u64,
    asize:      usize,
    ablob:      usize,
    pkt_buf:    Vec<NAPacket>,
}

impl<'a> DemuxCore<'a> for BMV3Demuxer<'a> {
    #[allow(unused_variables)]
    #[allow(clippy::cast_lossless)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let mut magic = [0u8; 4];
                                            src.read_buf(&mut magic)?;
        validate!(&magic[0..] == b"BMVi");
        let size                            = src.read_u32le()?;
        validate!(size == 24);
        let _slot_size                      = src.read_u32le()? as usize;
        let nframes                         = src.read_u32le()? as usize;
        let _prefetch_slots                 = src.read_u16le()?;
        let _cache_size                     = src.read_u16le()?;
        let fps                             = src.read_u16le()? as u32;
        let audio_size                      = src.read_u16le()? as usize;
        let audio_blob_size                 = src.read_u16le()? as usize;
        let _audio_id                       = src.read_byte()?;
        let _video_id                       = src.read_byte()?;
        let width                           = src.read_u16le()? as usize;
        let height                          = src.read_u16le()? as usize;
        validate!(nframes > 0);
        validate!((width > 0) && (width <= 640));
        validate!((height > 0) && (height <= 432));
        validate!((audio_size > audio_blob_size) && (audio_blob_size > 0) && (audio_size % audio_blob_size == 0));
        let mut dta = [0u8; 4];
                                            src.read_buf(&mut dta)?;
        validate!(&dta[0..] == b"DATA");
        let data_size                       = src.read_u32le()? as usize;
        validate!(data_size > 0);
        self.asize = audio_size;
        self.ablob = audio_blob_size;

        let vhdr = NAVideoInfo::new(width, height, false, RGB565_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("bmv3-video", vci, None);
        self.vid_id = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 256, fps, nframes as u64)).unwrap();

        let ahdr = NAAudioInfo::new(22050, 2, SND_S16_FORMAT, audio_blob_size);
        let ainfo = NACodecInfo::new("bmv3-audio", NACodecTypeInfo::Audio(ahdr), None);
        self.aud_id = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, 22050, 0)).unwrap();

        self.vpos       = 0;
        self.apos       = 0;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if !self.pkt_buf.is_empty() {
            return Ok(self.pkt_buf.pop().unwrap());
        }

        loop {
            let ctype                       = self.src.read_byte()?;
            if ctype == 0 { // NOP chunk
                continue;
            }
            if ctype == 1 { return Err(DemuxerError::EOF); }
            let size                    = self.src.read_u24le()? as usize;
            if size == 0 { continue; }
            let asize;
            if (ctype & 0x20) != 0 {
                if (ctype & 0x40) != 0 {
                    asize = self.asize - self.ablob;
                } else {
                    asize = self.asize;
                }
                validate!(asize <= size);
                let mut buf: Vec<u8> = vec![0; asize + 1];
                buf[0] = (self.src.tell() & 1) as u8;
                self.src.read_buf(&mut buf[1..])?;

                let stream = strmgr.get_stream(self.aud_id).unwrap();
                let ts = stream.make_ts(Some(self.apos), None, None);
                let apkt = NAPacket::new(stream, ts, false, buf);

                self.apos += (asize as u64) / 41 * 32;
                self.pkt_buf.push(apkt);
            } else {
                asize = 0;
            }
            if size == asize {
                if !self.pkt_buf.is_empty() {
                    return Ok(self.pkt_buf.pop().unwrap());
                } else {
                    continue;
                }
            }
            let mut buf: Vec<u8> = vec![0; size - asize + 1];
            buf[0] = ctype;
            self.src.read_buf(&mut buf[1..])?;

            let stream = strmgr.get_stream(self.vid_id).unwrap();
            let ts = stream.make_ts(Some(self.vpos), None, None);
            let pkt = NAPacket::new(stream, ts, (ctype & 3) == 3, buf);

            self.vpos += 1;
            return Ok(pkt);
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for BMV3Demuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> BMV3Demuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:        io,
            vid_id:     0,
            aud_id:     0,
            vpos:       0,
            apos:       0,
            asize:      0,
            ablob:      0,
            pkt_buf:    Vec::with_capacity(1),
        }
    }
}

pub struct BMV3DemuxerCreator { }

impl DemuxerCreator for BMV3DemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(BMV3Demuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "bmv3" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    // samples from https://samples.mplayerhq.hu/game-formats/bmv
    #[test]
    fn test_bmv_demux() {
        let mut file = File::open("assets/Game/DW2-MOUSE.BMV").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = BMVDemuxer::new(&mut br);
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
    fn test_bmv3_demux() {
        let mut file = File::open("assets/Game/DW3-Loffnote.bmv").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = BMV3Demuxer::new(&mut br);
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
