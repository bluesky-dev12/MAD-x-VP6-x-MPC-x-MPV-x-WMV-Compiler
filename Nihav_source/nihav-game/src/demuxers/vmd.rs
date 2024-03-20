use nihav_core::frame::*;
use nihav_core::demuxers::*;
use std::io::SeekFrom;

const OLD_HEADER_SIZE: usize = 0x330;
const NEW_HEADER_SIZE: usize = 0x34;
const HEADER1_SIZE: usize = 28;
const HEADER2_OFF: usize = HEADER1_SIZE + 768;
const FRAME_HDR_SIZE: usize = 10;

const CHTYPE_VIDEO: u8 = 0x02;
const CHTYPE_AUDIO: u8 = 0x01;

#[derive(Clone,Copy)]
struct FrameRec {
    chtype:     u8,
    size:       u32,
    off:        u32,
    hdr:        [u8; FRAME_HDR_SIZE],
    ts:         u32,
}

struct VMDDemuxer<'a> {
    src:        &'a mut ByteReader<'a>,
    vid_id:     usize,
    aud_id:     usize,
    fno:        usize,
    is_indeo:   bool,
    is_lhaud:   bool,
    frames:     Vec<FrameRec>,
}

impl<'a> DemuxCore<'a> for VMDDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        let src = &mut self.src;

        let mut header: [u8; OLD_HEADER_SIZE] = [0; OLD_HEADER_SIZE];
                                                src.read_buf(&mut header[..HEADER1_SIZE])?;
        let hdr_size = read_u16le(&header)? as usize + 2;
        validate!(hdr_size == OLD_HEADER_SIZE || hdr_size == NEW_HEADER_SIZE);
        if hdr_size == OLD_HEADER_SIZE {
                                                src.read_buf(&mut header[HEADER1_SIZE..][..768])?;
        }
                                                src.read_buf(&mut header[HEADER2_OFF..])?;

        let mut width  = read_u16le(&header[12..])? as usize;
        let mut height = read_u16le(&header[14..])? as usize;
        self.is_indeo = &header[24..27] == b"iv3";
        if self.is_indeo && width > 320 {
            width  >>= 1;
            height >>= 1;
        }

        let nframes = read_u16le(&header[6..])? as usize;
        let fpb     = read_u16le(&header[18..])? as usize;
        validate!(nframes > 0 && fpb > 0);

        let mut edata: Vec<u8> = Vec::with_capacity(OLD_HEADER_SIZE);
        edata.extend_from_slice(&header);
        let vhdr = NAVideoInfo::new(width, height, false, PAL8_FORMAT);
        let vci = NACodecTypeInfo::Video(vhdr);
        let vinfo = NACodecInfo::new(if !self.is_indeo { "vmd-video" } else { "indeo3" }, vci, Some(edata));
        self.vid_id = strmgr.add_stream(NAStream::new(StreamType::Video, 0, vinfo, 1, 12, nframes as u64)).unwrap();

        let is_ext_audio = (hdr_size & 0xF) == 4;
        let ext_audio_id = if is_ext_audio {
                                                src.read_u16le()?
            } else { 0 };
        if is_ext_audio {
            validate!(ext_audio_id >= 3 && ext_audio_id <= 6);
            self.is_lhaud = true;
        }
        let srate = u32::from(read_u16le(&header[804..])?);
        let block_size;
        if srate > 0 {
            let bsize = read_u16le(&header[806..])? as usize;
            let channels = if (header[811] & 0x8F) != 0 { 2 } else { 1 };
            let is16bit;
            if (bsize & 0x8000) != 0 {
                is16bit = true;
                block_size = 0x10000 - bsize;
            } else {
                is16bit = false;
                block_size = bsize;
            }

            let mut aedata: Vec<u8> = Vec::with_capacity(2);
            aedata.extend_from_slice(&header[810..][..2]);
            let ahdr = NAAudioInfo::new(srate, channels, if is16bit { SND_S16P_FORMAT } else { SND_U8_FORMAT }, block_size);
            let ac_name = if !is_ext_audio {
                    "vmd-audio"
                } else {
                    match ext_audio_id {
                        3 => "lhst15f8",
                        4 => "lhst500f22",
                        5 => "lhst250f11",
                        6 => "lhst48",
                        _ => "unknown",
                    }
                };
            let ainfo = NACodecInfo::new(ac_name, NACodecTypeInfo::Audio(ahdr), Some(aedata));
            self.aud_id = strmgr.add_stream(NAStream::new(StreamType::Audio, 1, ainfo, 1, srate, 0)).unwrap();
        } else {
            block_size = 0;
        }

        let adelay  = u32::from(read_u16le(&header[808..])?);
        let idx_off = u64::from(read_u32le(&header[812..])?);
                                                src.seek(SeekFrom::Start(idx_off))?;
        let mut offs: Vec<u32> = Vec::with_capacity(nframes);
        for i in 0..nframes {
            let _flags                          = src.read_u16le()?;
            let off                             = src.read_u32le()?;
            offs.push(off);
        }
        self.frames.reserve(nframes * fpb);
        let mut ats = adelay;
        for i in 0..nframes {
            let mut off = offs[i];
            for _ in 0..fpb {
                let chtype                      = src.read_byte()?;
                                                  src.read_skip(1)?;
                let mut size                    = src.read_u32le()?;
                let mut hdr: [u8; FRAME_HDR_SIZE] = [0; FRAME_HDR_SIZE];
                                                  src.read_buf(&mut hdr)?;
                if (i == 0) && (chtype == CHTYPE_AUDIO) && (size > 4) && ((size as usize) < block_size/2) {
                    size += 0x10000;
                }
                if (chtype == CHTYPE_VIDEO || chtype == CHTYPE_AUDIO) && (size > 0) {
                    let ts = if (i == 0) || (chtype != CHTYPE_AUDIO) {
                            i as u32
                        } else {
                            ats
                        };
                    self.frames.push(FrameRec { chtype, size, hdr, off, ts });
                }
                if i > 0 && chtype == CHTYPE_AUDIO {
                    ats += 1;
                }
                if chtype != 0 {
                    validate!(off.checked_add(size).is_some());
                    off += size;
                }
            }
        }

        self.fno = 0;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.fno >= self.frames.len() { return Err(DemuxerError::EOF); }
        let cur_frame = &self.frames[self.fno];
//println!("fno {} -> type {} size {} @ {:X} ts {}", self.fno, cur_frame.chtype, cur_frame.size, cur_frame.off, cur_frame.ts);
        let next_pos = u64::from(cur_frame.off);
        if self.src.tell() != next_pos {
            self.src.seek(SeekFrom::Start(next_pos))?;
        }

        let is_video = cur_frame.chtype == CHTYPE_VIDEO;
        let mut buf: Vec<u8> = Vec::with_capacity(FRAME_HDR_SIZE + (cur_frame.size as usize));
        if !((is_video && self.is_indeo) || (!is_video && self.is_lhaud)) {
            buf.extend_from_slice(&cur_frame.hdr);
            buf.resize(FRAME_HDR_SIZE + (cur_frame.size as usize), 0);
            self.src.read_buf(&mut buf[FRAME_HDR_SIZE..])?;
        } else {
            buf.resize(cur_frame.size as usize, 0);
            self.src.read_buf(&mut buf)?;
        }

        self.fno += 1;

        let str_id = if is_video { self.vid_id } else { self.aud_id };
        let stream = strmgr.get_stream(str_id).unwrap();
        let ts = stream.make_ts(Some(u64::from(cur_frame.ts)), None, None);
        let pkt = NAPacket::new(stream, ts, false, buf);

        Ok(pkt)
    }

    fn seek(&mut self, _time: NATimePoint, _seek_index: &SeekIndex) -> DemuxerResult<()> {
        Err(DemuxerError::NotPossible)
    }

    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for VMDDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> VMDDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self {
            src:        io,
            vid_id:     0,
            aud_id:     0,
            fno:        0,
            is_indeo:   false,
            is_lhaud:   false,
            frames:     Vec::new(),
        }
    }
}

pub struct VMDDemuxerCreator { }

impl DemuxerCreator for VMDDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(VMDDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "vmd" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_vmd_demux() {
        // sample: https://samples.mplayerhq.hu/game-formats/sierra-vmd/Lighthouse/128.vmd
        let mut file = File::open("assets/Game/128.vmd").unwrap();
        //let mut file = File::open("assets/Game/1491.VMD").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = VMDDemuxer::new(&mut br);
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
