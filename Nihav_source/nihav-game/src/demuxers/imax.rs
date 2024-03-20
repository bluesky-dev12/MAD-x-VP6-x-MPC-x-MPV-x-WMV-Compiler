use nihav_core::frame::*;
use nihav_core::demuxers::*;
use std::sync::Arc;

#[allow(dead_code)]
struct IMAXDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    cur_frame:  u64,
    apos:       u64,
    a_id:       usize,
    v_id:       usize,
    pal:        Arc<[u8; 1024]>,
    pal_change: bool,
}

impl<'a> DemuxCore<'a> for IMAXDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let magic                       = src.read_tag()?;
        validate!(&magic == b"IMAX");
        let nframes                     = u64::from(src.read_u32le()?);
        let fps                         = u32::from(src.read_u16le()?);
        let magic2                      = src.read_u16le()?;
        validate!(magic2 == 0x102);
        let _zero                       = src.read_u16le()?;
        let _max_vframe_size            = src.read_u32le()?;
        let _buffering_size             = src.read_u32le()?;

        let vhdr = NAVideoInfo::new(320, 160, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new("fable-imax", vci, None);
        self.v_id = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, fps, nframes)).unwrap();
        let ahdr = NAAudioInfo::new(22050, 1, SND_U8_FORMAT, 2);
        let ainfo = NACodecInfo::new("pcm", NACodecTypeInfo::Audio(ahdr), None);
        self.a_id = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, 22050, 2)).unwrap();
        self.cur_frame = 0;
        self.apos = 0;
        Ok(())
    }

    #[allow(unused_variables)]
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        loop {
            let fsize                   = self.src.read_u32le()? as usize;
            let ftype                   = self.src.read_u32le()?;

            match ftype {
                0xAA97 => {
                    let stream = strmgr.get_stream(self.v_id).unwrap();
                    let ts = stream.make_ts(Some(self.cur_frame), None, None);
                    self.cur_frame += 1;
                    let mut pkt = self.src.read_packet(stream, ts, true, fsize)?;
                    pkt.add_side_data(NASideData::Palette(self.pal_change, self.pal.clone()));
                    self.pal_change = false;
                    return Ok(pkt);
                },
                0xAA98 => {
                    validate!(fsize == 768);
                    let mut pal = [0u8; 1024];
                    for chunk in pal.chunks_mut(4) {
                        let r           = self.src.read_byte()?;
                        let g           = self.src.read_byte()?;
                        let b           = self.src.read_byte()?;
                        chunk[0] = (r << 2) | (r >> 4);
                        chunk[1] = (g << 2) | (g >> 4);
                        chunk[2] = (b << 2) | (b >> 4);
                    }
                    self.pal = Arc::new(pal);
                    self.pal_change = true;
                },
                0xAA99 => {
                    let stream = strmgr.get_stream(self.a_id).unwrap();
                    let ts = stream.make_ts(Some(self.apos), None, None);
                    self.apos += fsize as u64;
                    return self.src.read_packet(stream, ts, true, fsize);
                },
                0xAAFF => return Err(DemuxerError::EOF),
                _ => return Err(DemuxerError::InvalidData),
            }
        }
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }
    fn get_duration(&self) -> u64 { 0 }
}
impl<'a> NAOptionHandler for IMAXDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}
impl<'a> IMAXDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        IMAXDemuxer {
            src:        io,
            cur_frame:  0,
            apos:       0,
            a_id:       0,
            v_id:       0,
            pal:        Arc::new([0; 1024]),
            pal_change: false,
        }
    }
}

pub struct IMAXDemuxerCreator { }

impl DemuxerCreator for IMAXDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(IMAXDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "fable-imax" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_imax_demux() {
        // sample from Fable game
        let mut file = File::open("assets/Game/present.imx").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = IMAXDemuxer::new(&mut br);
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
