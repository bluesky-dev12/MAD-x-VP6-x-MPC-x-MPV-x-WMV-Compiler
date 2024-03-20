use nihav_core::frame::*;
use nihav_core::demuxers::*;

struct TTADemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    cur_frame:      u32,
    nframes:        u32,
    nsamples:       u32,
    offtab:         Vec<u64>,
    sizetab:        Vec<u32>,
    framelen:       u32,
    duration:       u64,
}

impl<'a> TTADemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:            io,
            cur_frame:      0,
            nframes:        0,
            nsamples:       0,
            offtab:         Vec::new(),
            sizetab:        Vec::new(),
            framelen:       0,
            duration:       0,
        }
    }
}

impl<'a> DemuxCore<'a> for TTADemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {

        let mut hdr = vec![0; 22];
                                          self.src.read_buf(&mut hdr)?;

        validate!(&hdr[..4] == b"TTA1");
        let _afmt                       = read_u16le(&hdr[4..])?;
        let channels                    = read_u16le(&hdr[6..])?;
        validate!(channels > 0 && channels < 256);
        let bpp                         = read_u16le(&hdr[8..])?;
        validate!(bpp > 0 && bpp <= 32);
        let srate                       = read_u32le(&hdr[10..])?;
        validate!(srate > 256 && srate < 1048576);
        self.nsamples                   = read_u32le(&hdr[14..])?;
        validate!(self.nsamples > 0);
        let _crc                        = read_u32le(&hdr[18..])?;

        self.framelen = srate * 256 / 245;

        self.nframes = (self.nsamples + self.framelen - 1) / self.framelen;
        self.duration = u64::from(self.nsamples) * 1000 / u64::from(srate);

        seek_index.mode = SeekIndexMode::Present;
        let mut off = u64::from(self.nframes) * 4 + 4 + 22;
        let mut cur_time = 0;
        for pts in 0..self.nframes {
            let fsize                   = self.src.read_u32le()?;
            self.sizetab.push(fsize);
            self.offtab.push(off);
            let time = u64::from(cur_time) * 1000 / u64::from(srate);
            seek_index.add_entry(0, SeekEntry { time, pts: u64::from(pts), pos: off });
            off += u64::from(fsize);
            cur_time += self.framelen;
        }

        let ahdr = NAAudioInfo::new(srate, channels as u8, SND_S16P_FORMAT, 1);
        let ainfo = NACodecInfo::new("tta", NACodecTypeInfo::Audio(ahdr), Some(hdr));
        strmgr.add_stream(NAStream::new(StreamType::Audio, 0, ainfo, self.framelen, srate, u64::from(self.nframes))).unwrap();

        self.cur_frame = 0;

        Ok(())
    }
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.cur_frame >= self.nframes {
            return Err(DemuxerError::EOF);
        }

        let size = self.sizetab[self.cur_frame as usize] as usize;
        let off = self.offtab[self.cur_frame as usize];
        self.src.seek(SeekFrom::Start(off))?;

        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(Some(self.cur_frame.into()), None, None);
        let pkt = self.src.read_packet(stream, ts, true, size)?;

        self.cur_frame += 1;

        Ok(pkt)
    }
    fn seek(&mut self, time: NATimePoint, seek_index: &SeekIndex) -> DemuxerResult<()> {
        let ret = seek_index.find_pos(time);
        if ret.is_none() {
            return Err(DemuxerError::SeekError);
        }
        let seek_info = ret.unwrap();
        self.cur_frame = seek_info.pts as u32;

        Ok(())
    }
    fn get_duration(&self) -> u64 { self.duration }
}

impl<'a> NAOptionHandler for TTADemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct TTADemuxerCreator { }

impl DemuxerCreator for TTADemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(TTADemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "tta" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_tta_demux() {
        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.tta
        let mut file = File::open("assets/LLaudio/luckynight.tta").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = TTADemuxer::new(&mut br);
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
