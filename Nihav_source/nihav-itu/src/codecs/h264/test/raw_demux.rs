use nihav_core::frame::*;
use nihav_core::io::byteio::*;
use nihav_core::demuxers::*;
use nihav_core::formats::YUV420_FORMAT;

struct RawH264Demuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    cur_frame:      u64,
    frame_buf:      Vec<u8>,
}

fn read_nal(src: &mut ByteReader, dst: &mut Vec<u8>) -> DemuxerResult<()> {
    dst.clear();
    loop {
        let b                           = src.read_byte()?;
        if b == 0 {
            let b2                      = src.read_byte()?;
            if b2 == 0 {
                let b3                  = src.read_byte()?;
                if b3 == 0 {
                    while src.read_byte()? != 1 { }
                    break;
                } else if b3 == 1 {
                    break;
                } else {
                    dst.push(b);
                    dst.push(b2);
                    dst.push(b3);
                }
            } else {
                dst.push(b);
                dst.push(b2);
            }
        } else {
            dst.push(b);
        }
    }
    Ok(())
}

fn put_nal(dst: &mut Vec<u8>, src: &[u8]) {
    let len = src.len();
    dst.push((len >> 24) as u8);
    dst.push((len >> 16) as u8);
    dst.push((len >>  8) as u8);
    dst.push( len        as u8);
    dst.extend_from_slice(src);
}

impl<'a> DemuxCore<'a> for RawH264Demuxer<'a> {
    #[allow(clippy::unreadable_literal)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_idx: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        while src.read_byte()? != 1 { }

        let mut edata: Vec<u8> = Vec::with_capacity(64);
        let mut num_sps = 0;
        let mut sps_buf = Vec::new();
        let mut num_pps = 0;
        let mut pps_buf = Vec::new();
        let mut profile = 0;
        let mut level = 0;

        let mut nal_buf = Vec::with_capacity(65536);
        loop {
            read_nal(src, &mut nal_buf)?;
            if !nal_buf.is_empty() {
                let nal_type = nal_buf[0] & 0x1F;
                match nal_type {
                    7 => {
                        if nal_buf.len() < 4 {
                            return Err(DemuxerError::InvalidData);
                        }
                        profile = nal_buf[1];
                        level   = nal_buf[3];
                        sps_buf.push((nal_buf.len() >> 8) as u8);
                        sps_buf.push(nal_buf.len() as u8);
                        sps_buf.extend_from_slice(&nal_buf);
                        num_sps += 1;
                    },
                    8 => {
                        pps_buf.push((nal_buf.len() >> 8) as u8);
                        pps_buf.push(nal_buf.len() as u8);
                        pps_buf.extend_from_slice(&nal_buf);
                        num_pps += 1;
                    },
                    1 | 5 => {
                        self.frame_buf = nal_buf;
                        break;
                    },
                    _ => {},
                };
            }
        }
        if num_sps == 0 || num_pps == 0 {
            return Err(DemuxerError::InvalidData);
        }
        edata.extend_from_slice(b"avcC");
        edata.push(1);
        edata.push(profile);
        edata.push(0);
        edata.push(level);
        edata.push(0xFF);
        edata.push(0xE0 | num_sps);
        edata.extend_from_slice(&sps_buf);
        edata.push(num_pps);
        edata.extend_from_slice(&pps_buf);

        let width = 16;
        let height = 16;

        let vhdr = NAVideoInfo::new(width, height, false, YUV420_FORMAT);
        let vinfo = NACodecInfo::new("h264", NACodecTypeInfo::Video(vhdr), Some(edata));
        if let None = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 25, 0)) {
            return Err(DemuxerError::InvalidData);
        }
        self.cur_frame = 0;

        Ok(())
    }
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        let ts = NATimeInfo::new(Some(self.cur_frame), None, None, 1, 25);
        let mut buf: Vec<u8> = Vec::with_capacity(65536);
        if !self.frame_buf.is_empty() {
            put_nal(&mut buf, &self.frame_buf);
            self.frame_buf.clear();
        }
        let strres = strmgr.get_stream(0);
        if strres.is_none() {
            return Err(DemuxerError::InvalidData);
        }
        let stream = strres.unwrap();

        let mut keyframe = false;

        let mut nal_buf = Vec::with_capacity(65536);
        let mut is_eof = false;
        while !is_eof {
            match read_nal(&mut self.src, &mut nal_buf) {
                Ok(()) => {},
                Err(DemuxerError::IOError) => { is_eof = true; },
                Err(err) => return Err(err),
            };
            if !nal_buf.is_empty() {
                let nal_type = nal_buf[0] & 0x1F;
                keyframe = nal_type == 5;
                match nal_type {
                    1 | 5 => {
                        let first_slice = (nal_buf[1] & 0x80) != 0;
                        if first_slice && !buf.is_empty() {
                            self.frame_buf.extend_from_slice(&nal_buf);
                            break;
                        } else {
                            put_nal(&mut buf, &nal_buf);
                        }
                    },
                    _ => {
                        //println!("non-slice NAL {} @ {:X}", nal_type, self.src.tell());
                        put_nal(&mut buf, &nal_buf);
                    },
                };
            }
        }

        if is_eof && buf.is_empty() {
            return Err(DemuxerError::EOF);
        }

        let pkt = NAPacket::new(stream, ts, keyframe, buf);

        self.cur_frame += 1;

        Ok(pkt)
    }
    fn seek(&mut self, _time: NATimePoint, _seek_idx: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotImplemented)
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for RawH264Demuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> RawH264Demuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        RawH264Demuxer {
            src:            io,
            cur_frame:      0,
            frame_buf:      Vec::new(),
        }
    }
}

pub struct RawH264DemuxerCreator { }

impl DemuxerCreator for RawH264DemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(RawH264Demuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "rawh264" }
}

