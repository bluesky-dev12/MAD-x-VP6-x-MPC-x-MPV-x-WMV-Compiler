use nihav_core::demuxers::*;
use nihav_registry::register;
use nihav_core::demuxers::DemuxerError::*;

macro_rules! mktag {
    ($a:expr, $b:expr, $c:expr, $d:expr) => {
        (u32::from($a) << 24) | (u32::from($b) << 16) | (u32::from($c) << 8) | u32::from($d)
    };
    ($arr:expr) => {
        (u32::from($arr[0]) << 24) | (u32::from($arr[1]) << 16) | (u32::from($arr[2]) << 8) | u32::from($arr[3])
    };
}

struct WAVDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    data_pos:       u64,
    data_end:       u64,
    srate:          u32,
    block_size:     usize,
    is_pcm:         bool,
    avg_bytes:      u32,
    duration:       u64,

    force_tb_num:   u32,
    force_tb_den:   u32,
}

impl<'a> DemuxCore<'a> for WAVDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let riff                        = self.src.read_u32be()?;
        let riff_size                   = self.src.read_u32le()? as usize;
        let riff_end = self.src.tell() + if riff_size > 0 { riff_size as u64 } else { u64::from(std::u32::MAX) };
        let wave                        = self.src.read_u32be()?;
        validate!(riff == mktag!(b"RIFF"));
        validate!(wave == mktag!(b"WAVE"));

        seek_index.mode = SeekIndexMode::Automatic;

        let mut fmt_parsed = false;
        let mut duration = 0;
        while self.src.tell() < riff_end {
            let ctype                   = self.src.read_tag()?;
            let csize                   = self.src.read_u32le()? as usize;
            match &ctype {
                b"fmt " => {
                    validate!(!fmt_parsed);
                    self.parse_fmt(strmgr, csize)?;
                    fmt_parsed = true;
                },
                b"fact" => {
                    validate!(csize == 4);
                    duration            = self.src.read_u32le()? as usize;
                },
                b"data" => {
                    validate!(fmt_parsed);
                    self.data_pos = self.src.tell();
                    self.data_end = self.data_pos + (csize as u64);

                    if duration != 0 {
                        self.duration = (duration as u64) * 1000 / u64::from(self.srate);
                    } else if self.avg_bytes > 0 {
                        self.duration = (self.data_end - self.data_pos) * 1000 / u64::from(self.avg_bytes);
                    } else {
                        self.duration = 0;
                    }

                    return Ok(());
                },
                _ => {
                                          self.src.read_skip(csize)?;
                },
            };
        }
        Err(DemuxerError::InvalidData)
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.src.tell() >= self.data_end {
            return Err(DemuxerError::EOF);
        }
        let strm = strmgr.get_stream(0);
        if strm.is_none() { return Err(InvalidData); }
        let stream = strm.unwrap();
        let pts = if self.avg_bytes != 0 {
                let pos = self.src.tell() - self.data_pos;
                Some(pos * u64::from(self.srate) / u64::from(self.avg_bytes))
            } else {
                None
            };
        let ts = NATimeInfo::new(pts, None, None, 1, self.srate);
        if self.is_pcm {
            let bsize = if self.force_tb_num != 0 && self.force_tb_den != 0 {
                    let nbsize = u64::from(self.avg_bytes) * u64::from(self.force_tb_num) / u64::from(self.force_tb_den);
                    let mut nbsize = nbsize as usize + self.block_size - 1;
                    nbsize /= self.block_size;
                    nbsize *= self.block_size;
                    nbsize
                } else {
                    let mut bsize = self.block_size;
                    while bsize < 256 {
                        bsize <<= 1;
                    }
                    bsize
                };
            let mut buf = vec![0; bsize];
            let mut tot_size = 0;
            while let Ok(psize)         = self.src.read_buf_some(&mut buf[tot_size..]) {
                tot_size += psize;
                if tot_size == buf.len() {
                    break;
                }
            }
            buf.truncate(tot_size);
            Ok(NAPacket::new(stream, ts, true, buf))
        } else {
            self.src.read_packet(stream, ts, true, self.block_size)
        }
    }

    fn seek(&mut self, time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        if self.block_size != 0 && self.avg_bytes != 0 {
            let seek_off = match time {
                    NATimePoint::Milliseconds(ms) => {
                        let seek_dst = u64::from(self.avg_bytes) * ms / 1000;
                        seek_dst / (self.block_size as u64) * (self.block_size as u64)
                    },
                    NATimePoint::PTS(pts) => (self.block_size as u64) * pts,
                    NATimePoint::None => return Ok(()),
                };
            self.src.seek(SeekFrom::Start(self.data_pos + seek_off))?;
            Ok(())
        } else {
            Err(DemuxerError::NotImplemented)
        }
    }

    fn get_duration(&self) -> u64 { self.duration }
}

const WAV_OPTIONS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: "force_tb_num", description: "force timebase numerator for PCM blocks",
        opt_type: NAOptionDefinitionType::Int(Some(1), None) },
    NAOptionDefinition {
        name: "force_tb_den", description: "force timebase denominator for PCM blocks",
        opt_type: NAOptionDefinitionType::Int(Some(1), None) },
];

impl<'a> NAOptionHandler for WAVDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { WAV_OPTIONS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            match (option.name, &option.value) {
                ("force_tb_num", NAValue::Int(ref ival)) => {
                    self.force_tb_num = *ival as u32;
                },
                ("force_tb_den", NAValue::Int(ref ival)) => {
                    self.force_tb_den = *ival as u32;
                },
                _ => {},
            };
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            "force_tb_num" => Some(NAValue::Int(i64::from(self.force_tb_num))),
            "force_tb_den" => Some(NAValue::Int(i64::from(self.force_tb_den))),
            _ => None,
        }
    }
}

impl<'a> WAVDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        WAVDemuxer {
            src:        io,
            data_pos:   0,
            data_end:   0,
            srate:      0,
            block_size: 0,
            is_pcm:     false,
            avg_bytes:  0,
            duration:   0,
            force_tb_num:   0,
            force_tb_den:   0,
        }
    }
    fn parse_fmt(&mut self, strmgr: &mut StreamManager, csize: usize) -> DemuxerResult<()> {
        validate!(csize >= 14);
        let format_tag                  = self.src.read_u16le()?;
        let channels                    = self.src.read_u16le()?;
        validate!(channels < 256);
        let samples_per_sec             = self.src.read_u32le()?;
        let avg_bytes_per_sec           = self.src.read_u32le()?;
        let block_align                 = self.src.read_u16le()? as usize;
        if block_align == 0 {
            return Err(DemuxerError::NotImplemented);
        }
        let bits_per_sample             = if csize >= 16 { self.src.read_u16le()? } else { 8 };
        validate!(channels < 256);

        let edata = if csize > 16 {
                validate!(csize >= 18);
                let cb_size             = self.src.read_u16le()? as usize;
                let mut buf = vec![0; cb_size];
                                          self.src.read_buf(buf.as_mut_slice())?;
                Some(buf)
            } else {
                None
            };

        let cname = register::find_codec_from_wav_twocc(format_tag).unwrap_or("unknown");
        let soniton = if cname == "pcm" {
                if format_tag != 0x0003 {
                    if bits_per_sample == 8 {
                        NASoniton::new(8, 0)
                    } else {
                        NASoniton::new(bits_per_sample as u8, SONITON_FLAG_SIGNED)
                    }
                } else {
                    NASoniton::new(bits_per_sample as u8, SONITON_FLAG_FLOAT)
                }
            } else {
                NASoniton::new(bits_per_sample as u8, SONITON_FLAG_SIGNED)
            };
        let ahdr = NAAudioInfo::new(samples_per_sec, channels as u8, soniton, block_align);
        let ainfo = NACodecInfo::new(cname, NACodecTypeInfo::Audio(ahdr), edata);
        let res = strmgr.add_stream(NAStream::new(StreamType::Audio, 0, ainfo, 1, samples_per_sec, 0));
        if res.is_none() { return Err(MemoryError); }

        self.srate = samples_per_sec;
        self.block_size = block_align;
        self.avg_bytes = avg_bytes_per_sec;
        self.is_pcm = cname == "pcm";
        if self.is_pcm && self.avg_bytes == 0 {
            self.avg_bytes = self.block_size as u32 * self.srate;
        }

        Ok(())
    }
}

pub struct WAVDemuxerCreator { }

impl DemuxerCreator for WAVDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(WAVDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "wav" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_wav_demux() {
        // sample: https://samples.mplayerhq.hu/A-codecs/msadpcm-stereo/scatter.wav
        let mut file = File::open("assets/MS/scatter.wav").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = WAVDemuxer::new(&mut br);
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
