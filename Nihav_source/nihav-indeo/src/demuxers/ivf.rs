use nihav_core::demuxers::*;

struct IVFDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    nframes:        u32,
    vframe:         u32,
    aframe:         u32,
    size:           u64,
    vframes:        Vec<Vec<u8>>,
    vsizes:         Vec<u32>,
    aframes:        Vec<Vec<u8>>,
    do_v:           bool,

    passes:         u8,
}

impl<'a> IVFDemuxer<'a> {
    fn new(src: &'a mut ByteReader<'a>) -> Self {
        IVFDemuxer {
            src,
            nframes:    0,
            vframe:     0,
            aframe:     0,
            size:       0,
            vframes:    Vec::new(),
            aframes:    Vec::new(),
            vsizes:     Vec::new(),
            do_v:       false,

            passes:     0,
        }
    }
}

const IVF_GUID_0: [u8; 16] = [0x50, 0xEF, 0x81, 0x19, 0xB3, 0xBD, 0xD0, 0x11, 0xA3, 0xE5, 0x00, 0xA0, 0xC9, 0x24, 0x44, 0x36];
const IVF_GUID_1: [u8; 16] = [0x50, 0xEF, 0x81, 0x19, 0xB3, 0xBD, 0xD0, 0x11, 0xA3, 0xE5, 0x00, 0xA0, 0xC9, 0x24, 0x44, 0x37];


impl<'a> DemuxCore<'a> for IVFDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let mut guid = [0; 16];
                                          self.src.read_buf(&mut guid)?;
        let version = match guid {
            IVF_GUID_0 => 0,
            IVF_GUID_1 => 1,
            _ => return Err(DemuxerError::InvalidData),
        };
        let flags                       = self.src.read_u32le()?;
        // file header - 0x9C bytes
        let aframes                     = self.src.read_u32le()? as usize;
                                          self.src.read_skip(12)?;
        self.size                       = u64::from(self.src.read_u32le()?);
                                          self.src.read_skip(136)?;
        // video stream header - 0x8C bytes
        let tag                         = self.src.read_tag()?;
        validate!(&tag == b"vids");
                                          self.src.read_skip(16)?;
        let tb_num                      = self.src.read_u32le()?;
        let tb_den                      = self.src.read_u32le()?;
                                          self.src.read_skip(4)?;
        self.nframes                    = self.src.read_u32le()?;
                                          self.src.read_skip(104)?;

        let (atb_num, atb_den, aduration) = if (flags & 1) != 0 {
            // audio stream header - 0x8C bytes
                let tag                 = self.src.read_tag()?;
                validate!(&tag == b"auds");
                                          self.src.read_skip(16)?;
                let tb_num              = self.src.read_u32le()?;
                let tb_den              = self.src.read_u32le()?;
                                          self.src.read_skip(4)?;
                let duration            = self.src.read_u32le()?;
                                          self.src.read_skip(104)?;
                (tb_num, tb_den, duration)
            } else { (0, 0, 0) };

        let vhdr_size                   = self.src.read_u32le()? as usize;
        validate!(vhdr_size >= 40);
        let bmpi_size                   = self.src.read_u32le()? as usize;
        validate!(bmpi_size == vhdr_size);
        let width                       = self.src.read_u32le()? as usize;
        let height                      = self.src.read_u32le()? as i32;
        let planes                      = self.src.read_u16le()?;
        let bitcount                    = self.src.read_u16le()?;
        let fcc                         = self.src.read_tag()?;
                                          self.src.read_skip(20)?;

        let mut vhdr = NAVideoInfo::new(width, height.unsigned_abs() as usize, height < 0, YUV420_FORMAT);
        vhdr.bits = (planes as u8) * (bitcount as u8);
        let cname = match &fcc {
                b"IV31" | b"IV32" => "indeo3",
                b"IV41" => "indeo4",
                b"IV50" => "indeo5s",
                _ => "unknown",
            };
        let edata = if vhdr_size > 40 {
                let mut buf = vec![0; vhdr_size - 40];
                                          self.src.read_buf(&mut buf)?;
                Some(buf)
            } else {
                None
            };
        let vinfo = NACodecInfo::new(cname, NACodecTypeInfo::Video(vhdr), edata);
        let res = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, tb_num, tb_den, u64::from(self.nframes)));
        if res.is_none() { return Err(DemuxerError::MemoryError); }

        if (flags & 1) != 0 {
            let ahdr_size               = self.src.read_u32le()? as usize;
            validate!(ahdr_size >= 16);
            let w_format_tag            = self.src.read_u16le()?;
            let channels                = self.src.read_u16le()?;
            let samplespersec           = self.src.read_u32le()?;
            let _avgbytespersec         = self.src.read_u32le()?;
            let block_align             = self.src.read_u16le()?;
            let bits_per_sample         = self.src.read_u16le()?;

            let signed = bits_per_sample > 8;
            let soniton = NASoniton::new(bits_per_sample as u8, if signed { SONITON_FLAG_SIGNED } else { 0 });
            let ahdr = NAAudioInfo::new(samplespersec, channels as u8, soniton, block_align as usize);
            let edata = if ahdr_size > 16 {
                    let edata_size      = self.src.read_u16le()? as usize;
                    validate!(edata_size + 18 == ahdr_size);
                    if edata_size > 0 {
                        let mut buf = vec![0; edata_size];
                                          self.src.read_buf(&mut buf)?;
                        Some(buf)
                    } else {
                        None
                    }
                } else {
                    None
                };

            let cname = match w_format_tag {
                    0x401 => "iac",
                    0x402 => "imc",
                    _ =>     "unknown",
                };

            let ainfo = NACodecInfo::new(cname, NACodecTypeInfo::Audio(ahdr), edata);
            let res = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, atb_num, atb_den, u64::from(aduration)));
            if res.is_none() { return Err(DemuxerError::MemoryError); }
        }

        // video frame table
        self.vsizes.reserve(self.nframes as usize);
        for _ in 0..self.nframes {
            let size                    = self.src.read_u32le()?;
            self.vsizes.push(size);
        }

        if version == 1 {
                                          self.src.read_skip(128)?;
        }

        let comment_len                 = self.src.read_u32le()? as usize;
                                          self.src.read_skip(comment_len)?;

        self.vframe = 0;
        self.aframe = 0;

        self.vframes = Vec::with_capacity(self.nframes as usize);
        self.aframes = Vec::with_capacity(aframes);
        for _ in 0..self.nframes {
            self.vframes.push(Vec::new());
        }
        for _ in 0..aframes {
            self.aframes.push(Vec::new());
        }

        let mut last_ts = 1 << 31;
        let mut pass = 0;
        while self.src.tell() < self.size {
            let flg                     = self.src.read_u32le()?;
            let fsize                   = self.src.read_u32le()? as usize;

            let tstamp = (flg >> 1) as usize;

            if (flg & 1) != 0 {
                if last_ts > tstamp {
                    pass += 1;
                    if self.passes != 0 && pass > self.passes {
                        break;
                    }
                }
                last_ts = tstamp;
            }

            let dst = if (flg & 1) != 0 { &mut self.vframes[tstamp] } else { &mut self.aframes[tstamp] };
            let cur_size = dst.len();
            dst.resize(cur_size + fsize, 0);
                                          self.src.read_buf(&mut dst[cur_size..])?;
        }

        // remove provisionary code for drop frames if real data is present
        for frm in self.vframes.iter_mut() {
            if frm.len() > 2 && frm[0] == 0x9F && frm[1] == 0x00 {
                frm.remove(0);
                frm.remove(0);
            }
        }

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        let has_next = if self.do_v { self.vframe < self.nframes } else { self.aframe < self.nframes };
        if has_next {
            let (stream_id, tstamp, buf) = if self.do_v {
                    self.vframe += 1;
                    (0, self.vframe - 1, self.vframes[self.vframe as usize - 1].clone())
                } else {
                    self.aframe += 1;
                    (1, self.aframe - 1, self.aframes[self.aframe as usize - 1].clone())
                };
            if !self.do_v || (self.aframe as usize) < self.aframes.len() {
                self.do_v = !self.do_v;
            }

            if let Some(stream) = strmgr.get_stream(stream_id) {
                let ts = stream.make_ts(Some(tstamp as u64), None, None);
                return Ok(NAPacket::new_from_refbuf(stream, ts, false, NABufferRef::new(buf)));
            } else {
                return Err(DemuxerError::InvalidData);
            }
        }
        Err(DemuxerError::EOF)
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}

const PASSES: &str = "passes";

const DEMUXER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: PASSES, description: "Number of passes to assemble data (0 = all)",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
];

impl<'a> NAOptionHandler for IVFDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { DEMUXER_OPTS }
    #[allow(clippy::single_match)]
    fn set_options(&mut self, options: &[NAOption]) {
       for option in options.iter() {
            for opt_def in DEMUXER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        PASSES => {
                            if let NAValue::Int(intval) = option.value {
                                self.passes = intval as u8;
                            }
                        },
                        _ => {},
                    }
                }
            }
        }
    }
    #[allow(clippy::single_match)]
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            PASSES => Some(NAValue::Int(i64::from(self.passes))),
            _ => None,
        }
    }
}

pub struct IVFDemuxerCreator { }

impl DemuxerCreator for IVFDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(IVFDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "ivf" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_ivf_demux() {
        // sample is a trailer for Heart of Darkness game
        let mut file = File::open("assets/Indeo/TRAILERIIE.IVF").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = IVFDemuxer::new(&mut br);
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
