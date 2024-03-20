use nihav_core::frame::*;
use nihav_core::demuxers::*;

struct FLACDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    data_start:     u64,
    tot_samples:    u64,
    cur_samples:    u64,
    blk_samples:    u16,
    min_samples:    u16,
    min_size:       usize,
    max_size:       usize,
    srate:          u32,
    build_index:    bool,
}

impl<'a> FLACDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:            io,
            data_start:     0,
            tot_samples:    0,
            cur_samples:    0,
            blk_samples:    0,
            min_samples:    0,
            min_size:       0,
            max_size:       0,
            srate:          0,
            build_index:    false,
        }
    }
}

impl<'a> RawDemuxCore<'a> for FLACDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let tag                         = self.src.read_tag()?;
        validate!(&tag == b"fLaC");
        let mut streaminfo: Vec<u8> = Vec::new();
        let mut srate = 0u32;
        let mut channels = 0u8;
        loop {
            let id1                     = self.src.read_byte()?;
            let len                     = self.src.read_u24be()? as usize;
            let id = id1 & 0x7F;

            match id {
                0x00 => {
                    validate!(len >= 34);
                    streaminfo = vec![0u8; len];
                                          self.src.read_buf(&mut streaminfo)?;
                    let min_bs          = read_u16be(&streaminfo[0..])?;
                    let max_bs          = read_u16be(&streaminfo[2..])?;
                    if min_bs == max_bs {
                        self.blk_samples = max_bs;
                    }
                    self.min_samples = min_bs;
                    self.min_size       = read_u24be(&streaminfo[4..])? as usize;
                    self.max_size       = read_u24be(&streaminfo[7..])? as usize;
                    let word            = read_u24be(&streaminfo[10..])?;
                    srate = word >> 4;
                    channels = (((word >> 1) & 7) + 1) as u8;
                    self.tot_samples    = (u64::from(streaminfo[13] & 0xF) << 32) | u64::from(read_u32be(&streaminfo[14..])?);
                },
                0x03 => {
                    validate!((len % 18) == 0);
                    seek_index.mode = SeekIndexMode::Present;
                    for _ in 0..len / 18 {
                        let sample      = self.src.read_u64be()?;
                        let offset      = self.src.read_u64be()?;
                        let _nsamps     = self.src.read_u16be()?;
                        let time = sample * 1000 / u64::from(srate.max(1000));
                        seek_index.add_entry(0, SeekEntry { time, pts: sample, pos: offset });
                    }
                },
                _ =>                      self.src.read_skip(len)?,
            };

            if (id1 & 0x80) != 0 {
                break;
            }
        }
        if seek_index.mode != SeekIndexMode::Present {
            seek_index.mode = SeekIndexMode::Automatic;
            self.build_index = true;
        } else {
            self.build_index = false;
        }
        self.data_start = self.src.tell();
        validate!(srate != 0);
        self.srate = srate;

        let base = if self.blk_samples != 0 { u32::from(self.blk_samples) } else { 1 };
        let ahdr = NAAudioInfo::new(srate, channels, SND_S16P_FORMAT, base as usize);
        let ainfo = NACodecInfo::new("flac", NACodecTypeInfo::Audio(ahdr), Some(streaminfo));
        strmgr.add_stream(NAStream::new(StreamType::Audio, 0, ainfo, base, srate, 0)).unwrap();

        Ok(())
    }
    fn get_data(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NARawData> {
        let stream = strmgr.get_stream(0).unwrap();
        let mut buf = vec![0; 8192];
        let size = self.src.read_buf_some(&mut buf)?;
        buf.truncate(size);
        Ok(NARawData::new(stream, buf))
    }
    fn seek(&mut self, time: NATimePoint, seek_index: &SeekIndex) -> DemuxerResult<()> {
        if seek_index.mode == SeekIndexMode::Present {
            let ret = seek_index.find_pos(time);
            if ret.is_none() {
                return Err(DemuxerError::SeekError);
            }
            let seek_info = ret.unwrap();
            self.cur_samples = seek_info.pts;
            self.src.seek(SeekFrom::Start(self.data_start + seek_info.pos))?;
            Ok(())
        } else {
            Err(DemuxerError::NotPossible)
        }
    }
    fn get_duration(&self) -> u64 { self.tot_samples * 1000 / u64::from(self.srate) }
}

impl<'a> NAOptionHandler for FLACDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct FLACDemuxerCreator { }

impl RawDemuxerCreator for FLACDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn RawDemuxCore<'a> + 'a> {
        Box::new(FLACDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "flac" }
    fn check_format(&self, br: &mut ByteReader) -> bool {
        if br.seek(SeekFrom::Start(0)).is_err() {
            return false;
        }
        matches!(br.read_tag(), Ok([b'f', b'L', b'a', b'C']))
    }
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use super::*;
    use crate::llaudio_register_all_packetisers;
    use std::fs::File;

    #[test]
    fn test_flac_raw_demux() {
        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.flac
        let mut file = File::open("assets/LLaudio/luckynight.flac").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = FLACDemuxer::new(&mut br);
        let mut sm = StreamManager::new();
        let mut si = SeekIndex::new();
        dmx.open(&mut sm, &mut si).unwrap();
        let stream = sm.get_stream(0).unwrap();
        let mut pkt_reg = RegisteredPacketisers::new();
        llaudio_register_all_packetisers(&mut pkt_reg);
        let creator = pkt_reg.find_packetiser("flac").unwrap();
        let mut pkts = (creator)();
        let mut tot_size = 0;
        while let Ok(pkt) = dmx.get_data(&mut sm) {
            tot_size += pkt.get_buffer().len();
            pkts.add_data(&pkt.get_buffer());
        }
        let mut tot_size2 = 0;
        let mut npkts = 0;
        while let Ok(Some(pkt)) = pkts.get_packet(stream.clone()) {
            tot_size2 += pkt.get_buffer().len();
            npkts += 1;
        }
        assert_eq!(npkts, 579);
        assert_eq!(tot_size, tot_size2);
    }
}
