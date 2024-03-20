use nihav_core::frame::*;
use nihav_core::demuxers::*;

#[derive(Clone,Copy)]
struct Frame {
    off:            u32,
    size:           u32,
    bits_off:       u8,
}

struct APEDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    cur_frame:      usize,
    frames:         Vec<Frame>,
    normal_blocks:  u32,
    last_blocks:    u32,
    truncated:      bool,
    duration:       u64,
}

impl<'a> APEDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:            io,
            cur_frame:      0,
            frames:         Vec::new(),
            normal_blocks:  0,
            last_blocks:    0,
            truncated:      false,
            duration:       0,
        }
    }
}

impl<'a> DemuxCore<'a> for APEDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let tag                         = src.read_tag()?;
        validate!(&tag == b"MAC ");
        let version                     = src.read_u16le()?;
        validate!((3800..=3990).contains(&version));

        let seektab_len;
        let _wavtail_len;
        let compr_type;
        let flags;
        let blocksperframe;
        let finalblocks;
        let nframes;
        let bits;
        let channels;
        let srate;
        if version >= 3980 {
                                          src.read_skip(2)?;
            let descriptor_len          = src.read_u32le()? as usize;
            let header_len              = src.read_u32le()? as usize;
            validate!(header_len == 24);
            seektab_len                 = src.read_u32le()? as usize;
            let _wavheader_len          = src.read_u32le()? as usize;
            let _audio_len              = src.read_u64le()?;
            _wavtail_len                = src.read_u32le()? as usize;
                                          src.read_skip(16)?; // unpacked data MD5
            if descriptor_len > 52 {
                                          src.read_skip(descriptor_len - 52)?;
            }

            compr_type                  = src.read_u16le()?;
            flags                       = src.read_u16le()?;
            blocksperframe              = src.read_u32le()?;
            finalblocks                 = src.read_u32le()?;
            nframes                     = src.read_u32le()? as usize;
            bits                        = src.read_u16le()?;
            channels                    = src.read_u16le()?;
            srate                       = src.read_u32le()?;
        } else {
            compr_type                  = src.read_u16le()?;
            flags                       = src.read_u16le()?;
            channels                    = src.read_u16le()?;
            srate                       = src.read_u32le()?;

            let wavheader_len           = src.read_u32le()? as usize;
            _wavtail_len                = src.read_u32le()? as usize;
            nframes                     = src.read_u32le()? as usize;
            finalblocks                 = src.read_u32le()?;
            if (flags & 0x04) != 0 {
                                          src.read_u32le()?; // peak level
            }
            if (flags & 0x10) != 0 {
                seektab_len             = src.read_u32le()? as usize * 4;
            } else {
                seektab_len = nframes * 4;
            }

            if (flags & 0x01) != 0 {
                bits = 8;
            } else if (flags & 0x08) != 0 {
                bits = 24;
            } else {
                bits = 16;
            }

            blocksperframe = 9216 * if version >= 3950 {
                    32
                } else if (version >= 3900) || ((version >= 3800) && (compr_type >= 4000)) {
                    8
                } else {
                    1
                };

            if (flags & 0x20) == 0 {
                                          src.read_skip(wavheader_len)?;
            }
        }
        validate!(srate > 0);
        validate!(channels > 0 && channels < 256);
        validate!(bits > 0 && bits <= 32);
        validate!(nframes > 0 && nframes < (1 << 28));
        validate!(seektab_len >= nframes * 4);

        self.frames = Vec::with_capacity(nframes);
        self.normal_blocks = blocksperframe;
        self.last_blocks   = finalblocks;
        self.duration = (((nframes - 1) as u64) * u64::from(blocksperframe) + u64::from(finalblocks)) * 1000 / u64::from(srate);

        seek_index.mode = SeekIndexMode::Present;
        let first_off                   = src.peek_u32le()?;
        validate!(u64::from(first_off) >= src.tell() + ((nframes * 4) as u64));
        let mut last_off = first_off - 1;
        for i in 0..nframes {
            let off                     = src.read_u32le()?;
            validate!(off > last_off);
            let diff = (off - first_off) & 3;
            self.frames.push(Frame {
                    off:        off - diff,
                    size:       0,
                    bits_off:   (diff as u8) * 8,
                });

            last_off = off;

            let time = (i as u64) * u64::from(blocksperframe) * 1000 / u64::from(srate);
            seek_index.add_entry(0, SeekEntry { time, pts: i as u64, pos: i as u64 });
        }
        if version < 3810 {
            for frame in self.frames.iter_mut() {
                let bits                = src.read_byte()?;
                validate!(bits < 32);
                frame.bits_off += bits;
            }
        }
                                          src.seek(SeekFrom::End(0))?;
        let fsize = src.tell();
        validate!(fsize > u64::from(self.frames[0].off));
        self.truncated = u64::from(self.frames[self.frames.len() - 1].off) >= fsize;
        if self.truncated {
            let mut valid_frames = self.frames.len();
            for frame in self.frames.iter_mut().rev() {
                if u64::from(frame.off) >= fsize {
                    valid_frames -= 1;
                }
            }
            self.frames.truncate(valid_frames);
            validate!(!self.frames.is_empty());
            self.truncated = true;
        }
        let mut last_off = fsize as u32;
        for frame in self.frames.iter_mut().rev() {
            frame.size = last_off - frame.off;
            last_off = frame.off + (if frame.bits_off > 0 { 4 } else { 0 });
        }

        let mut hdr = vec![0u8; 16];
        write_u16le(&mut hdr[0..], version)?;
        write_u16le(&mut hdr[2..], compr_type)?;
        write_u16le(&mut hdr[4..], flags)?;
        hdr[6] = channels as u8;
        hdr[7] = bits as u8;
        write_u32le(&mut hdr[8..], srate)?;
        write_u32le(&mut hdr[12..], blocksperframe)?;

        let ahdr = NAAudioInfo::new(srate, channels as u8, SND_S16P_FORMAT, 1);
        let ainfo = NACodecInfo::new("ape", NACodecTypeInfo::Audio(ahdr), Some(hdr));
        strmgr.add_stream(NAStream::new(StreamType::Audio, 0, ainfo, blocksperframe, srate, nframes as u64)).unwrap();

        self.cur_frame = 0;

        Ok(())
    }
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.cur_frame >= self.frames.len() {
            return Err(DemuxerError::EOF);
        }

        let size = self.frames[self.cur_frame].size as usize;
        let off  = self.frames[self.cur_frame].off;
        let bits = self.frames[self.cur_frame].bits_off;
        let nblocks = if (self.cur_frame < self.frames.len() - 1) || self.truncated { self.normal_blocks } else { self.last_blocks };

                                          self.src.seek(SeekFrom::Start(off.into()))?;

        let mut buf = vec![0u8; size + 8];
        write_u32le(&mut buf[0..], nblocks)?;
        buf[4] = bits;
                                          self.src.read_buf(&mut buf[8..])?;

        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(Some(self.cur_frame as u64), None, None);
        let pkt = NAPacket::new(stream, ts, true, buf);

        self.cur_frame += 1;

        Ok(pkt)
    }
    fn seek(&mut self, time: NATimePoint, seek_index: &SeekIndex) -> DemuxerResult<()> {
        let ret = seek_index.find_pos(time);
        if ret.is_none() {
            return Err(DemuxerError::SeekError);
        }
        let seek_info = ret.unwrap();
        self.cur_frame = seek_info.pts as usize;
        if self.cur_frame >= self.frames.len() {
            return Err(DemuxerError::SeekError);
        }

        Ok(())
    }
    fn get_duration(&self) -> u64 { self.duration }
}

impl<'a> NAOptionHandler for APEDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct APEDemuxerCreator { }

impl DemuxerCreator for APEDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(APEDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "ape" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_ape_demux() {
        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.ape
        let mut file = File::open("assets/LLaudio/ape/luckynight.ape").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = APEDemuxer::new(&mut br);
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
