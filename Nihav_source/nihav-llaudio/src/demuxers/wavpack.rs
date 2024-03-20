use nihav_core::frame::*;
use nihav_core::demuxers::*;

const SAMPLE_RATES: [u32; 15] = [
     6000,  8000,  9600, 11025, 12000, 16000,  22050, 24000,
    32000, 44100, 48000, 64000, 88200, 96000, 192000
];
const WV_FLAG_MONO: u32         = 1 <<  2;
//const WV_FLAG_HYBRID: u32       = 1 <<  3;
//const WV_FLAG_JSTEREO: u32      = 1 <<  4;
//const WV_FLAG_CH_DECORR: u32    = 1 <<  5;
//const WV_FLAG_HYB_NOISE_SHAPING: u32 = 1 <<  6;
const WV_FLAG_FLOATS: u32       = 1 <<  7;
//const WV_FLAG_EXT_INTEGERS: u32 = 1 <<  8;
//const WV_FLAG_HYB_BITRATE: u32  = 1 <<  9;
//const WV_FLAG_HYB_BALANCED_NOISE: u32 = 1 << 10;
const WV_FLAG_START_BLOCK: u32  = 1 << 11;
const WV_FLAG_END_BLOCK: u32    = 1 << 12;
//const WV_FLAG_HAS_CRC: u32      = 1 << 28;
const WV_FLAG_FALSE_STEREO: u32 = 1 << 30;
//const WV_FLAG_DSD_AUDIO: u32    = 1 << 31;

const WV_STREAM_FLAGS: u32 = 0x8000008B;

#[derive(Clone,Copy,Default)]
struct WVHeader {
    size:           usize,
    ver:            u16,
    tot_samples:    u64,
    block_index:    u64,
    block_samples:  u32,
    flags:          u32,
    crc:            u32,
}

const WV_HEADER_SIZE: usize = 32;

impl WVHeader {
    #[allow(clippy::field_reassign_with_default)]
    fn parse(src: &[u8]) -> DemuxerResult<Self> {
        let mut mr = MemoryReader::new_read(src);
        let mut br = ByteReader::new(&mut mr);
        let tag                         = br.read_tag()?;
        validate!(&tag == b"wvpk");
        let mut hdr = Self::default();
        hdr.size                        = br.read_u32le()? as usize;
        validate!(hdr.size >= 24);
        hdr.ver                         = br.read_u16le()?;
        validate!(hdr.ver >= 0x402 || hdr.ver <= 0x410);
        let top_idx                     = br.read_byte()?;
        let top_samps                   = br.read_byte()?;
        hdr.tot_samples                 = u64::from(br.read_u32le()?) | (u64::from(top_samps) << 32);
        hdr.block_index                 = u64::from(br.read_u32le()?) | (u64::from(top_idx) << 32);
        hdr.block_samples               = br.read_u32le()?;
        hdr.flags                       = br.read_u32le()?;
        hdr.crc                         = br.read_u32le()?;
        Ok(hdr)
    }
    fn stream_eq(&self, rval: &Self) -> bool {
        self.ver == rval.ver &&
        (self.flags & WV_STREAM_FLAGS) == (rval.flags & WV_STREAM_FLAGS)
    }
    fn block_eq(&self, rval: &Self) -> bool {
        self.stream_eq(rval) && self.tot_samples == rval.tot_samples &&
        self.block_index == rval.block_index &&
        self.block_samples == rval.block_samples
    }
    fn is_start_block(&self) -> bool {
        (self.flags & WV_FLAG_START_BLOCK) != 0
    }
    fn is_end_block(&self) -> bool {
        (self.flags & WV_FLAG_END_BLOCK) != 0
    }
    fn get_num_channels(&self) -> u8 {
        if (self.flags & WV_FLAG_MONO) != 0 && (self.flags & WV_FLAG_FALSE_STEREO) == 0 { 1 } else { 2 }
    }
    fn get_sample_rate(&self) -> u32 {
        let idx = ((self.flags >> 23) & 0xF) as usize;
        if idx != 15 {
            SAMPLE_RATES[idx]
        } else {
            0
        }
    }
    fn get_size(&self) -> usize {
        self.size - (WV_HEADER_SIZE - 8)
    }
}

#[derive(Clone,Copy,Default)]
struct FrameSeekInfo {
    off:            u64,
    samplepos:      u64,
}

struct WavPackDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    samplepos:      u64,
    nsamples:       u64,
    first_blocks:   Option<(WVHeader, Vec<u8>)>,
    srate:          u32,
    known_frames:   Vec<FrameSeekInfo>,
}

impl<'a> WavPackDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:            io,
            samplepos:      0,
            nsamples:       0,
            first_blocks:   None,
            srate:          0,
            known_frames:   Vec::new(),
        }
    }
    fn read_blocks(&mut self) -> DemuxerResult<(WVHeader, Vec<u8>)> {
        let mut hdrbuf = [0u8; WV_HEADER_SIZE];
        let mut buf: Vec<u8> = Vec::new();
        let mut first = true;
        let mut refhdr = WVHeader::default();
        loop {
                                        self.src.read_buf(&mut hdrbuf)?;
            let hdr = WVHeader::parse(&hdrbuf)?;
            if first {
                validate!(hdr.is_start_block());
                refhdr = hdr;
                first = false;
            } else {
                validate!(refhdr.block_eq(&hdr));
            }
            buf.extend_from_slice(&hdrbuf);
            let pos = buf.len();
            buf.resize(pos + hdr.get_size(), 0);
                                        self.src.read_buf(&mut buf[pos..])?;

            if hdr.is_end_block() {
                break;
            }
        }
        Ok((refhdr, buf))
    }
}

impl<'a> DemuxCore<'a> for WavPackDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {

        let (hdr, buf) = self.read_blocks()?;

        let srate = hdr.get_sample_rate();
        validate!(srate != 0);
        let channels = if !hdr.is_end_block() {
                let mut ch_count = 0;
                let mut off = 0;
                loop {
                    let hdr = WVHeader::parse(&buf[off..]).unwrap();
                    off += WV_HEADER_SIZE + hdr.get_size();
                    ch_count += hdr.get_num_channels();
                    if hdr.is_end_block() {
                        break;
                    }
                }
                ch_count
            } else {
                hdr.get_num_channels()
            };

        self.nsamples = hdr.tot_samples;

        let mut fmt = SND_S16P_FORMAT;
        if (hdr.flags & WV_FLAG_FLOATS) != 0 {
            fmt.float = true;
        } else {
            fmt.bits = (((hdr.flags & 3) + 1) * 8) as u8;
        }

        let ahdr = NAAudioInfo::new(srate, channels, SND_S16P_FORMAT, 1);
        let ainfo = NACodecInfo::new("wavpack", NACodecTypeInfo::Audio(ahdr), Some(buf.clone()));
        strmgr.add_stream(NAStream::new(StreamType::Audio, 0, ainfo, 1, srate, hdr.tot_samples)).unwrap();
        seek_index.mode = SeekIndexMode::Automatic;
        self.srate = srate;
        self.known_frames = Vec::with_capacity(((self.nsamples + u64::from(srate) - 1) / u64::from(srate)) as usize);
        self.known_frames.push(FrameSeekInfo { off: 0, samplepos: hdr.block_index });

        self.first_blocks = Some((hdr, buf));
        Ok(())
    }
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.first_blocks.is_some() {
            let mut fb = None;
            std::mem::swap(&mut fb, &mut self.first_blocks);
            let (refhdr, buf) = fb.unwrap();
            let stream = strmgr.get_stream(0).unwrap();
            let ts = stream.make_ts(Some(self.samplepos), None, None);
            let pkt = NAPacket::new(stream, ts, true, buf);

            self.samplepos += u64::from(refhdr.block_samples);

            return Ok(pkt);
        }
        if self.samplepos == self.nsamples {
            return Err(DemuxerError::EOF);
        }
        let cur_off = self.src.tell();
        let cur_spos = self.samplepos;
        let (refhdr, buf) = self.read_blocks()?;

        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(Some(self.samplepos), None, None);
        let pkt = NAPacket::new(stream, ts, true, buf);

        self.samplepos += u64::from(refhdr.block_samples);
        if self.known_frames.last().unwrap_or(&FrameSeekInfo::default()).samplepos < cur_spos {
            self.known_frames.push(FrameSeekInfo{off: cur_off, samplepos: cur_spos });
        }

        Ok(pkt)
    }
    fn seek(&mut self, time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        self.first_blocks = None;
        if let NATimePoint::Milliseconds(ms) = time {
            let samppos = ms * u64::from(self.srate) / 1000;
            if self.known_frames.last().unwrap_or(&FrameSeekInfo::default()).samplepos >= samppos {
                for point in self.known_frames.iter().rev() {
                    if point.samplepos <= samppos {
                        self.src.seek(SeekFrom::Start(point.off))?;
                        self.samplepos = point.samplepos;
                        return Ok(());
                    }
                }
            } else {
                let mut hdrbuf = [0u8; WV_HEADER_SIZE];
                let lastoff = self.known_frames.last().unwrap_or(&FrameSeekInfo::default()).off;
                self.src.seek(SeekFrom::Start(lastoff))?;
                loop {
                                          self.src.peek_buf(&mut hdrbuf)?;
                    let hdr = WVHeader::parse(&hdrbuf)?;
                    if hdr.is_start_block() {
                        self.known_frames.push(FrameSeekInfo{off: self.src.tell(), samplepos: hdr.block_index });
                        if hdr.block_index <= samppos && hdr.block_index + u64::from(hdr.block_samples) > samppos {
                            self.samplepos = hdr.block_index;
                            return Ok(());
                        }
                        if hdr.block_index > samppos {
                            break;
                        }
                    }
                                          self.src.read_skip(WV_HEADER_SIZE + hdr.get_size())?
                }
            }
            Err(DemuxerError::SeekError)
        } else {
            Err(DemuxerError::NotPossible)
        }
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for WavPackDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub struct WavPackDemuxerCreator { }

impl DemuxerCreator for WavPackDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(WavPackDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "wavpack" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_wavpack_demux() {
        // sample from the official WavPack test samples set
        let mut file = File::open("assets/LLaudio/wv/false_stereo.wv").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = WavPackDemuxer::new(&mut br);
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
