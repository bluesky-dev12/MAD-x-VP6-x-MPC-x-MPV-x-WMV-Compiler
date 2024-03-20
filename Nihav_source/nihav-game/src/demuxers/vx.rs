use nihav_core::frame::*;
use nihav_core::demuxers::*;
use std::io::SeekFrom;

const AUDIO_EXTRADATA_LEN: usize = 3124;

struct VXDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    vid_id:     usize,
    aud_id:     usize,
    video_pos:  u64,
    num_vid:    usize,
    vno:        u64,
    num_vfrm:   u64,
    num_aud:    usize,
    ano:        u64,
    num_afrm:   u64,
}

impl<'a> DemuxCore<'a> for VXDemuxer<'a> {
    #[allow(unused_variables)]
    #[allow(clippy::cast_lossless)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let mut magic = [0u8; 4];
                                          src.read_buf(&mut magic)?;
        validate!(&magic == b"VXDS");
        let nframes                     = src.read_u32le()? as usize;
        let width                       = src.read_u32le()? as usize;
        let height                      = src.read_u32le()? as usize;
        let _unk                        = src.read_u32le()? as usize;
        let fps                         = src.read_u32le()?;
        validate!(fps > 0 && fps < 256);
        let srate                       = src.read_u32le()?;
        let num_audio_tracks            = src.read_u32le()? as usize;
        let _max_frame_size             = src.read_u32le()? as usize;
        let audio_off                   = src.read_u32le()? as u64;
        validate!(audio_off == 0 || audio_off >= 0x30);
        let vinfo_off                   = src.read_u32le()? as u64;
        validate!(vinfo_off == 0 || vinfo_off >= 0x30);
        let num_vstreams                = src.read_u32le()? as usize;
        if num_vstreams != 1 || num_audio_tracks > 1 {
            return Err(DemuxerError::NotImplemented);
        }

        let vhdr = NAVideoInfo::new(width, height, false, YUV420_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let edata = [fps as u8].to_vec();
        let vinfo = NACodecInfo::new("vxvideo", vci, Some(edata));
        self.vid_id = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, fps, nframes as u64)).unwrap();

        if num_audio_tracks != 0 {
            validate!(audio_off + ((num_audio_tracks * AUDIO_EXTRADATA_LEN) as u64) == vinfo_off);
            src.seek(SeekFrom::Start(audio_off))?;
            let mut edata = vec![0u8; AUDIO_EXTRADATA_LEN];
                                          src.read_buf(edata.as_mut_slice())?;
            let ahdr = NAAudioInfo::new(srate, 1, SND_S16P_FORMAT, 1);
            let ainfo = NACodecInfo::new("vxaudio", NACodecTypeInfo::Audio(ahdr), Some(edata));
            self.aud_id = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, srate, 0)).unwrap();
            self.num_afrm = nframes as u64;
            self.ano = 0;
            self.num_aud = num_audio_tracks;
        }

        if num_vstreams > 0 {
            src.seek(SeekFrom::Start(vinfo_off))?;
            for _ in 0..num_vstreams {
                let _smth               = src.read_u32le()?;
                let offset              = src.read_u32le()? as u64;
                self.video_pos = offset;
            }
        }

        self.num_vid  = num_vstreams;
        self.vno = 0;
        self.num_vfrm = nframes as u64;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.vno >= self.num_vfrm { return Err(DemuxerError::EOF); }
        self.src.seek(SeekFrom::Start(self.video_pos))?;
        let stream = strmgr.get_stream(self.vid_id);
        if stream.is_none() { return Err(DemuxerError::InvalidData); }
        let stream = stream.unwrap();
        let ts = stream.make_ts(Some(self.vno), None, None);
        let size                    = self.src.read_u16le()? as usize;
        validate!(size > 2);
        let _num_achunks            = self.src.read_u16le()?;
        let fsize = size - 2;
        let pkt                     = self.src.read_packet(stream, ts, false, fsize)?;
        self.video_pos = self.src.tell();
        self.vno += 1;
        Ok(pkt)
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }

    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for VXDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> VXDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            vid_id:     0,
            aud_id:     0,
            video_pos:  0,
            num_vid:    0,
            num_aud:    0,
            vno:        0,
            ano:        0,
            num_vfrm:   0,
            num_afrm:   0,
            src:        io,
        }
    }
}

pub struct VXDemuxerCreator { }

impl DemuxerCreator for VXDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(VXDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "vx" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_vx_demux() {
        // sample from some game
        let mut file = File::open("assets/Game/bioware.vx").unwrap();
        //let mut file = File::open("assets/Game/BS_01_Intro.vx").unwrap();
        //let mut file = File::open("assets/Game/sega.vx").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = VXDemuxer::new(&mut br);
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
