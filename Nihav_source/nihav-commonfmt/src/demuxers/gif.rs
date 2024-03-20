use nihav_core::demuxers::*;

struct GIFDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    frameno:    u64,
    is_87:      bool,
    pal:        Arc<[u8; 1024]>,
}

impl<'a> GIFDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:        io,
            frameno:    0,
            is_87:      false,
            pal:        Arc::new([0; 1024]),
        }
    }
    fn skip_blocks(&mut self) -> DemuxerResult<()> {
        loop {
            let size                    = self.src.read_byte()?;
            if size == 0 {
                break;
            }
                                          self.src.read_skip(usize::from(size))?;
        }
        Ok(())
    }
}

impl<'a> DemuxCore<'a> for GIFDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let mut magic = [0; 6];
                                          self.src.read_buf(&mut magic)?;
        validate!(&magic == b"GIF87a" || &magic == b"GIF89a");
        self.is_87 = &magic == b"GIF87a";

        let width                       = usize::from(self.src.read_u16le()?);
        let height                      = usize::from(self.src.read_u16le()?);
        validate!(width > 0 && height > 0);
        let flags                       = self.src.read_byte()?;
        let edata_size = 1 + 2 + if (flags & 0x80) != 0 { 3 << ((flags & 7) + 1) } else { 0 };
        let mut edata = vec![0; edata_size];
        edata[0] = flags;
                                          self.src.read_buf(&mut edata[1..])?;
        if (flags & 0x80) != 0 {
            let mut npal = [0; 1024];
            for (dpal, spal) in npal.chunks_exact_mut(4).zip(edata[3..].chunks_exact(3)) {
                dpal[..3].copy_from_slice(spal);
            }
            self.pal = Arc::new(npal);
        }
        let mut delay = 0;
        loop {
            match self.src.peek_byte() {
                Ok(0x2C) => break,
                Ok(_) => {},
                Err(_err) => return Err(DemuxerError::IOError),
            };
            let tag                     = self.src.read_byte()?;
            match tag {
                0x21 => {
                    validate!(!self.is_87);
                    let subtype         = self.src.read_byte()?;
                    match subtype {
                        0xF9 => {
                            let bsize   = self.src.read_byte()?;
                            validate!(bsize == 4);
                            let _flags  = self.src.read_byte()?;
                            delay       = self.src.read_u16le()?;
                            let _clr    = self.src.read_byte()?;
                            self.skip_blocks()?;
                        },
                        0xFF => {
                            let bsize   = self.src.read_byte()?;
                            validate!(bsize == 11);
                            let mut app_id = [0; 11];
                                          self.src.read_buf(&mut app_id)?;
                            if &app_id == b"NETSCAPE2.0" {
                                let bsize   = self.src.read_byte()?;
                                validate!(bsize == 3);
                                let b       = self.src.read_byte()?;
                                validate!(b == 1);
                                let _nloops = self.src.read_u16le()?;
                            }
                            self.skip_blocks()?;
                        },
                        _ => {
                            self.skip_blocks()?;
                        },
                    };
                },
                0x2C => unreachable!(),
                _ => return Err(DemuxerError::NotImplemented),
            };
        }

        let vhdr = NAVideoInfo::new(width, height, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("gif", vci, Some(edata));
        if strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, u32::from(delay.max(1)), 100, 0)).is_none() {
            return Err(DemuxerError::MemoryError);
        }

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        loop {
            match self.src.read_byte()? {
                0x2C => {
                    let mut data = vec![0; 10];
                    data[0] = 0x2C;
                                          self.src.read_buf(&mut data[1..])?;
                    if (data[9] & 0x80) != 0 {
                        let cmap_size = 3 << ((data[9] & 7) + 1);
                        data.resize(10 + cmap_size, 0);
                                          self.src.read_buf(&mut data[10..])?;
                    }
                    let lzw_bits        = self.src.read_byte()?;
                    data.push(lzw_bits);
                    let mut tbuf = [0; 255];
                    loop {
                        let bsize       = usize::from(self.src.read_byte()?);
                        data.push(bsize as u8);
                        if bsize == 0 {
                            break;
                        }
                                          self.src.read_buf(&mut tbuf[..bsize])?;
                        data.extend_from_slice(&tbuf[..bsize]);
                    }

                    let stream = strmgr.get_stream(0).unwrap();
                    let ts = stream.make_ts(Some(self.frameno), None, None);
                    let mut pkt = NAPacket::new(stream, ts, self.frameno == 0, data);
                    pkt.add_side_data(NASideData::Palette(false, self.pal.clone()));
                    self.frameno += 1;
                    return Ok(pkt);
                },
                0x21 => {
                                          self.src.read_byte()?;
                    self.skip_blocks()?;
                },
                0x3B => return Err(DemuxerError::EOF),
                _ => unimplemented!(),
            };
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for GIFDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub struct GIFDemuxerCreator { }

impl DemuxerCreator for GIFDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(GIFDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "gif" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_gif_demux() {
        // sample: https://samples.mplayerhq.hu/image-samples/GIF/3D.gif
        let mut file = File::open("assets/Misc/3D.gif").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = GIFDemuxer::new(&mut br);
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
