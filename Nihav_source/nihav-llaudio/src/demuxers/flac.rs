use nihav_core::frame::*;
use nihav_core::demuxers::*;

#[derive(Clone,Copy,Default)]
struct FrameSeekInfo {
    off:            u64,
    size:           u64,
    samplepos:      u64,
    sampleend:      u64,
}

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
    known_frames:   Vec<FrameSeekInfo>,
    build_index:    bool,
    frame_hdr:      u32,
}

fn common_header_word(mut val: u32) -> u32 {
    val &= !0x1F000; // blocking strategy and block size
    let ch_map = (val >> 4) & 0xF;
    if matches!(ch_map, 0x1 | 0x8 | 0x9 | 0xA) { // stereo coding modes
        val &= !0xF0;
        val |= 0x10;
    }
    val
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
            known_frames:   Vec::new(),
            build_index:    false,
            frame_hdr:      0,
        }
    }
    fn read_frame(&mut self) -> DemuxerResult<(Vec<u8>, u64, u64)> {
        if self.src.is_eof() || (self.tot_samples != 0 && self.cur_samples == self.tot_samples) { return Err(DemuxerError::EOF); }
        let mut buf = Vec::with_capacity(self.min_size);
        let mut crc = 0;
        let frame_start = self.src.tell();
        for _ in 0..5 {
            let byte                    = self.src.read_byte()?;
            buf.push(byte);
            crc = update_crc16(crc, byte);
        }
        if self.frame_hdr == 0 {
            self.frame_hdr = read_u32be(&buf).unwrap_or(0);
        }
        let mut ref_crc                 = self.src.read_u16be()?;
        loop {
            let byte                    = self.src.read_byte()?;
            let old_byte = (ref_crc >> 8) as u8;
            buf.push(old_byte);
            ref_crc = (ref_crc << 8) | u16::from(byte);
            crc = update_crc16(crc, old_byte);
            if buf.len() + 4 >= self.min_size && crc == ref_crc {
                let ret                 = self.src.peek_u32be();
                if ret.is_err() || (common_header_word(ret.unwrap_or(0)) == common_header_word(self.frame_hdr)) {
                    buf.push((ref_crc >> 8) as u8);
                    buf.push(ref_crc as u8);
                    break;
                }
            }
            if (self.max_size > 0) && (buf.len() > self.max_size) {
                return Err(DemuxerError::InvalidData);
            }
            if buf.len() > (1 << 23) {
                return Err(DemuxerError::InvalidData);
            }
        }

        let (duration, pts) = if self.blk_samples != 0 {
                validate!((buf[1] & 1) == 0);
                let blkno = u64::from(read_utf8(&buf[4..])?);
                self.cur_samples = blkno * u64::from(self.blk_samples);
                (u64::from(self.blk_samples), blkno)
            } else {
                let mut idx = 5;
                while idx < buf.len() && (buf[idx] & 0x80) != 0 {
                    idx += 1;
                }

                let bsz_id = buf[2] >> 4;
                let blksamps = match bsz_id {
                        0 => return Err(DemuxerError::InvalidData),
                        1 => 192,
                        2..=5 => 576 << (bsz_id - 2),
                        6 => {
                            validate!(idx < buf.len());
                            u64::from(buf[idx]) + 1
                        },
                        7 => {
                            validate!(idx + 2 <= buf.len());
                            u64::from(buf[idx]) * 256 + u64::from(buf[idx + 1]) + 1
                        },
                        _ => 256 << (bsz_id - 8),
                    };
                let pts = u64::from(read_utf8(&buf[4..])?);

                validate!(idx < buf.len());

                (blksamps, pts)
            };

        let spos = if self.blk_samples != 0 { pts * u64::from(self.blk_samples) } else { pts };
        if self.build_index && (self.known_frames.is_empty() || self.known_frames.last().unwrap_or(&FrameSeekInfo::default()).samplepos < spos) {
            let sampleend = spos + duration;
            self.known_frames.push(FrameSeekInfo{off: frame_start, size: buf.len() as u64, samplepos: spos, sampleend });
        }

        self.cur_samples += duration;

        Ok((buf, pts, duration))
    }
}

fn update_crc16(crc: u16, byte: u8) -> u16 {
    (crc << 8) ^ CRC16_TABLE[(((crc >> 8) as u8) ^ byte) as usize]
}

fn read_utf8(src: &[u8]) -> DemuxerResult<u32> {
    if (src[0] & 0x80) == 0 {
        return Ok(u32::from(src[0]));
    }
    let len = (!src[0]).leading_zeros() as usize;
    validate!(len != 1 && len <= 5 && src.len() >= len);
    let mut val = u32::from(src[0] & 0x1F);
    for byte in src.iter().take(len).skip(1) {
        validate!((*byte & 0xC0) == 0x80);
        val = (val << 6) | u32::from(*byte & 0x3F);
    }
    Ok(val)
}

impl<'a> DemuxCore<'a> for FLACDemuxer<'a> {
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
            let min_size = if self.min_samples != 0 { self.min_samples } else { 2048 };
            let nframes = self.tot_samples as usize / (min_size as usize);
            self.known_frames = Vec::with_capacity(nframes.max(1));
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
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        let (buf, pts, duration) = self.read_frame()?;

        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(Some(pts), None, Some(duration));
        let pkt = NAPacket::new(stream, ts, true, buf);

        Ok(pkt)
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
        } else if let NATimePoint::Milliseconds(ms) = time {
            let samppos = NATimeInfo::time_to_ts(ms, 1000, 1, self.srate);
            if self.known_frames.last().unwrap_or(&FrameSeekInfo::default()).sampleend >= samppos {
                for point in self.known_frames.iter().rev() {
                    if point.samplepos <= samppos {
                        self.src.seek(SeekFrom::Start(point.off))?;
                        self.cur_samples = point.samplepos;
                        return Ok(());
                    }
                }
            } else {
                let startinfo = FrameSeekInfo { off: self.data_start, size: 0, samplepos: 0, sampleend: 0 };
                let lentry = self.known_frames.last().unwrap_or(&startinfo);

                self.src.seek(SeekFrom::Start(lentry.off + lentry.size))?;
                self.cur_samples = lentry.sampleend;
                loop {
                    let frame_start = self.src.tell();
                    let ret = self.read_frame();
                    if ret.is_err() {
                        return Err(DemuxerError::SeekError);
                    }
                    let (_, pts, duration) = ret.unwrap();
                    self.cur_samples = pts;
                    if self.blk_samples != 0 {
                        self.cur_samples *= u64::from(self.blk_samples);
                    }
                    if self.cur_samples <= samppos && self.cur_samples + duration >= samppos {
                        self.src.seek(SeekFrom::Start(frame_start))?;
                        return Ok(());
                    }
                }
            }
            Err(DemuxerError::SeekError)
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

impl DemuxerCreator for FLACDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(FLACDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "flac" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_flac_demux() {
        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.flac
        let mut file = File::open("assets/LLaudio/luckynight.flac").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = FLACDemuxer::new(&mut br);
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

const CRC16_TABLE: [u16; 256] = [
    0x0000, 0x8005, 0x800F, 0x000A, 0x801B, 0x001E, 0x0014, 0x8011,
    0x8033, 0x0036, 0x003C, 0x8039, 0x0028, 0x802D, 0x8027, 0x0022,
    0x8063, 0x0066, 0x006C, 0x8069, 0x0078, 0x807D, 0x8077, 0x0072,
    0x0050, 0x8055, 0x805F, 0x005A, 0x804B, 0x004E, 0x0044, 0x8041,
    0x80C3, 0x00C6, 0x00CC, 0x80C9, 0x00D8, 0x80DD, 0x80D7, 0x00D2,
    0x00F0, 0x80F5, 0x80FF, 0x00FA, 0x80EB, 0x00EE, 0x00E4, 0x80E1,
    0x00A0, 0x80A5, 0x80AF, 0x00AA, 0x80BB, 0x00BE, 0x00B4, 0x80B1,
    0x8093, 0x0096, 0x009C, 0x8099, 0x0088, 0x808D, 0x8087, 0x0082,
    0x8183, 0x0186, 0x018C, 0x8189, 0x0198, 0x819D, 0x8197, 0x0192,
    0x01B0, 0x81B5, 0x81BF, 0x01BA, 0x81AB, 0x01AE, 0x01A4, 0x81A1,
    0x01E0, 0x81E5, 0x81EF, 0x01EA, 0x81FB, 0x01FE, 0x01F4, 0x81F1,
    0x81D3, 0x01D6, 0x01DC, 0x81D9, 0x01C8, 0x81CD, 0x81C7, 0x01C2,
    0x0140, 0x8145, 0x814F, 0x014A, 0x815B, 0x015E, 0x0154, 0x8151,
    0x8173, 0x0176, 0x017C, 0x8179, 0x0168, 0x816D, 0x8167, 0x0162,
    0x8123, 0x0126, 0x012C, 0x8129, 0x0138, 0x813D, 0x8137, 0x0132,
    0x0110, 0x8115, 0x811F, 0x011A, 0x810B, 0x010E, 0x0104, 0x8101,
    0x8303, 0x0306, 0x030C, 0x8309, 0x0318, 0x831D, 0x8317, 0x0312,
    0x0330, 0x8335, 0x833F, 0x033A, 0x832B, 0x032E, 0x0324, 0x8321,
    0x0360, 0x8365, 0x836F, 0x036A, 0x837B, 0x037E, 0x0374, 0x8371,
    0x8353, 0x0356, 0x035C, 0x8359, 0x0348, 0x834D, 0x8347, 0x0342,
    0x03C0, 0x83C5, 0x83CF, 0x03CA, 0x83DB, 0x03DE, 0x03D4, 0x83D1,
    0x83F3, 0x03F6, 0x03FC, 0x83F9, 0x03E8, 0x83ED, 0x83E7, 0x03E2,
    0x83A3, 0x03A6, 0x03AC, 0x83A9, 0x03B8, 0x83BD, 0x83B7, 0x03B2,
    0x0390, 0x8395, 0x839F, 0x039A, 0x838B, 0x038E, 0x0384, 0x8381,
    0x0280, 0x8285, 0x828F, 0x028A, 0x829B, 0x029E, 0x0294, 0x8291,
    0x82B3, 0x02B6, 0x02BC, 0x82B9, 0x02A8, 0x82AD, 0x82A7, 0x02A2,
    0x82E3, 0x02E6, 0x02EC, 0x82E9, 0x02F8, 0x82FD, 0x82F7, 0x02F2,
    0x02D0, 0x82D5, 0x82DF, 0x02DA, 0x82CB, 0x02CE, 0x02C4, 0x82C1,
    0x8243, 0x0246, 0x024C, 0x8249, 0x0258, 0x825D, 0x8257, 0x0252,
    0x0270, 0x8275, 0x827F, 0x027A, 0x826B, 0x026E, 0x0264, 0x8261,
    0x0220, 0x8225, 0x822F, 0x022A, 0x823B, 0x023E, 0x0234, 0x8231,
    0x8213, 0x0216, 0x021C, 0x8219, 0x0208, 0x820D, 0x8207, 0x0202
];
