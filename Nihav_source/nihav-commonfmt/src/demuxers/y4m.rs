use nihav_core::demuxers::*;
use std::str::FromStr;

struct Y4MDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    width:      usize,
    height:     usize,
    frame_size: usize,
    hdr_size:   u64,
    fps_num:    u32,
    fps_den:    u32,
    frameno:    u64,
}

impl<'a> DemuxCore<'a> for Y4MDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let format = self.parse_header()?;
        seek_index.mode = SeekIndexMode::Automatic;

        let vhdr = NAVideoInfo::new(self.width, self.height, false, format);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("rawvideo", vci, None);
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, self.fps_num, self.fps_den, 0)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        let mut marker = [0u8; 6];
        let res                         = self.src.read_buf(&mut marker);
        match res {
            Err(ByteIOError::EOF) => return Err(DemuxerError::EOF),
            Err(err) => return Err(err.into()),
            _ => {},
        };
        validate!(&marker == b"FRAME\n");
        let stream = strmgr.get_stream(0).unwrap();
        let ts = stream.make_ts(Some(self.frameno), None, None);
        let pkt = self.src.read_packet(stream, ts, true, self.frame_size)?;
        self.frameno += 1;
        Ok(pkt)
    }

    fn seek(&mut self, time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        let new_fno = match time {
                NATimePoint::PTS(pts) => {
                    pts
                },
                NATimePoint::Milliseconds(ms) => {
                    if (self.fps_num == 0) || (self.fps_den == 0) {
                        return Err(DemuxerError::SeekError);
                    }
                    NATimeInfo::time_to_ts(ms, 1000, self.fps_num, self.fps_den)
                },
                NATimePoint::None => return Err(DemuxerError::SeekError),
            };
        let pos = self.hdr_size + new_fno * ((self.frame_size + 6) as u64);
        self.src.seek(SeekFrom::Start(pos))?;
        self.frameno = new_fno;

        Ok(())
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for Y4MDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> Y4MDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:        io,
            width:      0,
            height:     0,
            frame_size: 0,
            fps_num:    0,
            fps_den:    0,
            hdr_size:   0,
            frameno:    0,
        }
    }
    fn parse_header(&mut self) -> DemuxerResult<NAPixelFormaton> {
        let mut format = YUV420_FORMAT;

        let mut magic = [0u8; 10];
                                    self.src.read_buf(&mut magic)?;
        validate!(&magic == b"YUV4MPEG2 ");
        while let Ok((last, tok)) = read_token(self.src) {
            let (id, val) = tok.split_at(1);
            validate!(id.len() == 1);
            match id.bytes().next().unwrap() {
                b'W' => {
                    if let Ok(w) = val.parse::<usize>() {
                        self.width = w;
                    }
                },
                b'H' => {
                    if let Ok(h) = val.parse::<usize>() {
                        self.height = h;
                    }
                },
                b'F' => {
                    if let Ok(fden) = val.parse::<u32>() {
                        self.fps_num = 1;
                        self.fps_den = fden;
                    } else {
                        let vals: Vec<&str> = val.split(':').collect();
                        if vals.len() == 2 {
                            if let Ok(fnum) = vals[1].parse::<u32>() {
                                self.fps_num = fnum;
                            }
                            if let Ok(fden) = vals[0].parse::<u32>() {
                                self.fps_den = fden;
                            }
                        }
                    }
                },
                b'C' => {
                    let fmt_str = val.as_bytes();
                    validate!(fmt_str.len() >= 3);
                    let mut pix_name: [u8; 7] = *b"yuv000p";
                    validate!(fmt_str[0] == b'4');
                    pix_name[3..6].copy_from_slice(&fmt_str[..3]);

                    if let Ok(fmt_name) = std::str::from_utf8(&pix_name) {
                        if let Ok(val) = NAPixelFormaton::from_str(fmt_name) {
                            format = val;
                        }
                    }
                    if format.model.is_yuv() {
                        format.model = ColorModel::YUV(YUVSubmodel::YCbCr);
                        if fmt_str.len() > 3 {
                            let (_, tail) = fmt_str.split_at(3);
                            if tail == b"jpeg" {
                                format.model = ColorModel::YUV(YUVSubmodel::YUVJ);
                            }
                        }
                    }
                },
                _ => {},
            };

            if last {
                break;
            }
        }
        validate!(self.width > 0 && self.height > 0);
        self.frame_size = 0;
        for chr in format.comp_info.iter().flatten() {
            self.frame_size += chr.get_data_size(self.width, self.height);
        }
        validate!(self.frame_size > 0);

        Ok(format)
    }
}

fn read_token(src: &mut ByteReader) -> DemuxerResult<(bool, String)> {
    let mut string = String::new();
    let ws;
    loop {
        let b                       = src.read_byte()?;
        match b {
            b' ' | b'\n' => { ws = b; break; },
            0..=0x7F => string.push(b as char),
            _ => return Err(DemuxerError::InvalidData),
        }
    }

    Ok((ws == b'\n', string))
}

pub struct Y4MDemuxerCreator { }

impl DemuxerCreator for Y4MDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(Y4MDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "yuv4mpeg" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_y4m_demux() {
        // sample: self-created with avconv
        let mut file = File::open("assets/Misc/test.y4m").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = Y4MDemuxer::new(&mut br);
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
