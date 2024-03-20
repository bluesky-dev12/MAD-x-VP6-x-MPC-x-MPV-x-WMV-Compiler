use nihav_core::demuxers::*;
use nihav_registry::register;
use nihav_core::demuxers::DemuxerError::*;
use std::str::FromStr;

macro_rules! mktag {
    ($a:expr, $b:expr, $c:expr, $d:expr) => ({
        (($a as u32) << 24) | (($b as u32) << 16) | (($c as u32) << 8) | ($d as u32)
    });
    ($arr:expr) => ({
        (($arr[0] as u32) << 24) | (($arr[1] as u32) << 16) | (($arr[2] as u32) << 8) | ($arr[3] as u32)
    });
}

struct StreamState {
    strm_no:    u8,
    got_strf:   bool,
    strm_type:  Option<StreamType>,
}

impl StreamState {
    fn new() -> Self {
        StreamState { strm_no: 0, got_strf: true, strm_type: None }
    }
    fn reset(&mut self) {
        self.strm_type = None;
        self.got_strf  = true;
        self.strm_no  += 1;
    }
    fn valid_state(&self) -> bool {
        match self.strm_type {
            None => self.got_strf,
            _    => false,
        }
    }
}

struct PalInfo {
    pal:        Arc<[u8; 1024]>,
    changed:    bool,
    stream_no:  usize,
}

#[derive(Clone,Copy,Default)]
struct RIFFSegment {
    pos:        u64,
    size:       usize,
    movi_pos:   u64,
    movi_size:  usize,
}

impl RIFFSegment {
    fn contains(&self, pos: u64) -> bool {
        pos >= self.movi_pos && pos < self.movi_pos + (self.movi_size as u64)
    }
    fn get_end(&self) -> u64 {
        self.pos + (self.size as u64)
    }
}

#[allow(dead_code)]
struct AVIDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    cur_frame:      Vec<u64>,
    num_streams:    u8,
    size:           usize,
    movi_size:      usize,
    movi_pos:       u64,
    movi_orig:      usize,
    sstate:         StreamState,
    tb_num:         u32,
    tb_den:         u32,
    strm_duration:  u32,
    key_offs:       Vec<u64>,
    pal:            Vec<PalInfo>,
    odml:           bool,
    odml_idx:       Vec<u64>,
    odml_riff:      Vec<RIFFSegment>,
}

#[derive(Debug,Clone,Copy,PartialEq)]
enum RIFFTag {
    Chunk(u32),
    List(u32,u32),
}

struct RIFFParser {
    tag:   RIFFTag,
    parse: fn(&mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize>,
}

impl<'a> DemuxCore<'a> for AVIDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        self.read_header(strmgr, seek_index)?;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.movi_size == 0 {
            if !self.odml {
                return Err(EOF);
            }
            self.try_next_odml_chunk()?;
        }
        let mut tag: [u8; 4] = [0; 4];
        loop {
            if (self.src.tell() & 1) == 1 {
                self.src.read_skip(1)?;
                self.movi_size -= 1;
                if self.movi_size == 0 {
                    if !self.odml {
                        return Err(EOF);
                    }
                    self.try_next_odml_chunk()?;
                }
            }
            let is_keyframe = self.key_offs.binary_search(&self.src.tell()).is_ok();
            self.src.read_buf(&mut tag)?;
            let size = self.src.read_u32le()? as usize;
            if mktag!(tag) == mktag!(b"JUNK") {
                self.movi_size -= size + 8;
                self.src.read_skip(size)?;
                if self.movi_size == 0 {
                    if !self.odml {
                        return Err(EOF);
                    }
                    self.try_next_odml_chunk()?;
                }
                continue;
            }
            if mktag!(tag) == mktag!(b"LIST") {
                self.movi_size -= 12;
                self.src.read_skip(4)?;
                if self.movi_size == 0 {
                    if !self.odml {
                        return Err(EOF);
                    }
                    self.try_next_odml_chunk()?;
                }
                continue;
            }
            if (tag[0] == b'i' && tag[1] == b'x') || (&tag == b"idx1") {
                let idx_pos = self.src.tell() - 8;
                if !self.odml {
                    return Err(EOF);
                }
                self.src.read_skip(size)?;
                if idx_pos > self.movi_pos {
                    self.try_next_odml_chunk()?;
                } else {
                    self.movi_pos = self.src.tell();
                }
                continue;
            }
            if tag[0] < b'0' || tag[0] > b'9' || tag[1] < b'0' || tag[1] > b'9' {
                return Err(InvalidData);
            }
            let stream_no = (tag[0] - b'0') * 10 + (tag[1] - b'0');
            if tag[2] == b'p' && tag[3] == b'c' {
                self.parse_palette_change(stream_no as usize, size)?;
                self.movi_size -= size;
                if self.movi_size == 0 {
                    if !self.odml {
                        return Err(EOF);
                    }
                    self.try_next_odml_chunk()?;
                }
                continue;
            }
            let stream = strmgr.get_stream(stream_no as usize);
            if stream.is_none() {
                self.src.read_skip(size)?;
                self.movi_size -= size + 8;
                continue;
            }
            let stream = stream.unwrap();
            if size == 0 {
                self.movi_size -= 8;
                if self.movi_size == 0 {
                    if !self.odml {
                        return Err(EOF);
                    }
                    self.try_next_odml_chunk()?;
                }
                continue;
            }
            let (tb_num, _) = stream.get_timebase();
            let mut ts = stream.make_ts(Some(self.cur_frame[stream_no as usize]), None, None);
            if stream.get_media_type() == StreamType::Audio && tb_num == 1 && stream.get_info().get_name() == "pcm" {
                ts.pts = None;
            }
            let mut pkt = self.src.read_packet(stream, ts, is_keyframe, size)?;
            for pe in self.pal.iter_mut() {
                if pe.stream_no == (stream_no as usize) {
                    pkt.add_side_data(NASideData::Palette(pe.changed, pe.pal.clone()));
                    pe.changed = false;
                    break;
                }
            }
            self.cur_frame[stream_no as usize] += 1;
            self.movi_size -= size + 8;

            return Ok(pkt);
        }
    }

    fn seek(&mut self, time: NATimePoint, seek_index: &SeekIndex) -> DemuxerResult<()> {
        let ret = seek_index.find_pos(time);
        if ret.is_none() {
            return Err(DemuxerError::SeekError);
        }
        let seek_info = ret.unwrap();

        if self.odml && ((seek_info.pos < self.movi_pos) || (seek_info.pos > self.movi_pos + (self.movi_orig as u64))) {
            let mut found = false;
            for riff_seg in self.odml_riff.iter() {
                if riff_seg.contains(seek_info.pos) {
                    found = true;
                    self.movi_pos = riff_seg.movi_pos;
                    self.movi_orig = riff_seg.movi_size;
                    self.movi_size = riff_seg.movi_size;
                    break;
                }
            }
            if !found {
                let riff_seg = self.odml_riff.last().unwrap();
                self.src.seek(SeekFrom::Start(riff_seg.get_end()))?;
                loop {
                    let ret = self.try_next_odml_chunk();
                    if ret.is_err() {
                        return Err(DemuxerError::SeekError);
                    }
                    let riff_seg = self.odml_riff.last().unwrap();
                    if riff_seg.contains(seek_info.pos) {
                        self.movi_pos = riff_seg.movi_pos;
                        self.movi_orig = riff_seg.movi_size;
                        self.movi_size = riff_seg.movi_size;
                        break;
                    }
                    self.src.seek(SeekFrom::Start(riff_seg.get_end()))?;
                }
            }
        }
        if seek_info.pos < self.movi_pos { return Err(DemuxerError::SeekError); }
        let skip_size = (seek_info.pos - self.movi_pos) as usize;
        if skip_size > self.movi_orig { return Err(DemuxerError::SeekError); }
        self.movi_size = self.movi_orig - skip_size;

        self.cur_frame[seek_info.str_id as usize] = seek_info.pts;
        self.src.seek(SeekFrom::Start(seek_info.pos))?;

        Ok(())
    }
    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for AVIDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> AVIDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        AVIDemuxer {
            cur_frame: Vec::new(),
            num_streams: 0,
            src: io,
            size: 0,
            movi_size: 0,
            movi_pos:  0,
            movi_orig: 0,
            sstate: StreamState::new(),
            tb_num: 0,
            tb_den: 0,
            strm_duration: 0,
            key_offs: Vec::new(),
            pal: Vec::new(),
            odml: false,
            odml_idx: Vec::new(),
            odml_riff: Vec::with_capacity(1),
        }
    }

    fn parse_chunk(&mut self, strmgr: &mut StreamManager, end_tag: RIFFTag, csize: usize, depth: u16) -> DemuxerResult<(usize, bool)> {
        if csize < 8 { return Err(InvalidData); }
        if depth > 42 { return Err(InvalidData); }

        let tag  = self.src.read_u32be()?;
        let size = self.src.read_u32le()? as usize;
        if size > csize { return Err(InvalidData); }
        if RIFFTag::Chunk(tag) == end_tag {
            return Ok((size, true));
        }
        let is_list = is_list_tag(tag);
        let ltag = if is_list { self.src.read_u32be()? } else { 0 };
        if RIFFTag::List(tag, ltag) == end_tag {
            return Ok((size, true));
        }

        for chunk in CHUNKS.iter() {
            if RIFFTag::Chunk(tag) == chunk.tag {
                let psize = (chunk.parse)(self, strmgr, size)?;
                if psize != size { return Err(InvalidData); }
                if (psize & 1) == 1 { self.src.read_skip(1)?; }
                return Ok((size + 8, false));
            }
            if RIFFTag::List(tag, ltag) == chunk.tag {
                let mut rest_size = size - 4;
                let psize = (chunk.parse)(self, strmgr, rest_size)?;
                if psize > rest_size { return Err(InvalidData); }
                rest_size -= psize;
                while rest_size > 0 {
                    let (psize, _) = self.parse_chunk(strmgr, end_tag, rest_size, depth+1)?;
                    if psize > rest_size { return Err(InvalidData); }
                    rest_size -= psize;
                    if ((psize & 1) == 1) && (rest_size > 0) {
                        rest_size -= 1;
                    }
                }

                return Ok((size + 8, false));
            }
        }
        if !is_list {
            self.src.read_skip(size)?;
        } else {
            if size < 4 { return Err(InvalidData); }
            self.src.read_skip(size - 4)?;
        }
        if (size & 1) == 1 { self.src.read_skip(1)?; }
        Ok((size + 8, false))
    }

    fn read_header(&mut self, strmgr: &mut StreamManager, seek_idx: &mut SeekIndex) -> DemuxerResult<()> {
        let riff_tag = self.src.read_u32be()?;
        let size     = self.src.read_u32le()? as usize;
        self.odml_riff.push(RIFFSegment { pos: self.src.tell() - 8, size: size + 8, movi_pos: 0, movi_size: 0});
        let avi_tag  = self.src.read_u32be()?;
        let mut matches = false;
        for rt in RIFF_TAGS.iter() {
            if rt[0] == riff_tag && rt[1] == avi_tag {
                matches = true;
                break;
            }
        }
        if !matches {
            return Err(InvalidData);
        }
        self.size = size;
        let mut rest_size = size;
        loop {
            let (csz, end) = self.parse_chunk(strmgr, RIFFTag::List(mktag!(b"LIST"), mktag!(b"movi")), rest_size,0)?;
            if end {
                self.movi_size = csz - 4;
                self.movi_orig = self.movi_size;
                self.movi_pos = self.src.tell();

                self.odml_riff[0].movi_pos  = self.movi_pos;
                self.odml_riff[0].movi_size = self.movi_size;
                break;
            }
            rest_size -= csz;
        }
        if !seek_idx.skip_index {
            if !self.odml {
                self.src.read_skip(self.movi_size)?;
                while rest_size > 0 {
                    let ret = self.parse_chunk(strmgr, RIFFTag::Chunk(mktag!(b"idx1")), rest_size,0);
                    if ret.is_err() { break; }
                    let (csz, end) = ret.unwrap();
                    if end {
                        let _res = parse_idx1(self.src, strmgr, seek_idx, csz, self.movi_pos, &mut self.key_offs);
                        break;
                    }
                    rest_size -= csz;
                }
            } else {
                let mut start = 0;
                let mut last_strm_no = 255;
                for &offset in self.odml_idx.iter() {
                    if self.src.seek(SeekFrom::Start(offset)).is_err() {
                        break;
                    }
                    let ret = self.src.read_tag();
                    if ret.is_err() { break; }
                    let tag = ret.unwrap();
                    let ret = self.src.read_u32le();
                    if ret.is_err() { break; }
                    let size = ret.unwrap() as usize;
                    if &tag[..2] != b"ix" || tag[2] < b'0' || tag[2] > b'9' || tag[3] < b'0' || tag[3] > b'9'{
                        break;
                    }
                    let stream_no = ((tag[2] - b'0') * 10 + (tag[3] - b'0')) as usize;

                    if last_strm_no != stream_no {
                        start = 0;
                        last_strm_no = stream_no;
                    }
                    let ret = parse_odml_ix(self.src, strmgr, seek_idx, stream_no, size, start);
                    if let Ok(new_start) = ret {
                        start = new_start;
                    } else {
                        break;
                    }
                }
            }
        }
        if self.movi_pos != 0 {
            self.src.seek(SeekFrom::Start(self.movi_pos))?;
        } else {
            return Err(InvalidData);
        }
        if !self.sstate.valid_state() || self.sstate.strm_no != self.num_streams {
            return Err(InvalidData);
        }
        Ok(())
    }

    fn read_extradata(&mut self, size: usize) -> DemuxerResult<Option<Vec<u8>>> {
        if size == 0 { return Ok(None); }
        let mut edvec: Vec<u8> = vec![0; size];
        self.src.read_buf(&mut edvec)?;
        Ok(Some(edvec))
    }

    fn parse_palette_change(&mut self, stream_no: usize, size: usize) -> DemuxerResult<()> {
        for pe in self.pal.iter_mut() {
            if pe.stream_no == stream_no {
                let start_clr           = self.src.read_byte()? as usize;
                let len                 = self.src.read_byte()? as usize;
                let len = if len == 0 { 256 } else { len };
                let _flags              = self.src.read_u16le()?;
                validate!(start_clr + len <= 256);
                validate!(len * 4 + 4 == size);
                let mut newpal = *pe.pal;
                for i in start_clr..(start_clr + len) {
                    newpal[i * 4]       = self.src.read_byte()?;
                    newpal[i * 4 + 1]   = self.src.read_byte()?;
                    newpal[i * 4 + 2]   = self.src.read_byte()?;
                    newpal[i * 4 + 3]   = 0;
                                          self.src.read_byte()?; // flags
                }
                pe.pal = Arc::new(newpal);
                pe.changed = true;
                return Ok(());
            }
        }
        self.src.read_skip(size)?;
        Ok(())
    }
    fn try_next_odml_chunk(&mut self) -> DemuxerResult<()> {
        let last_seg = self.odml_riff.last().unwrap();
        if self.src.tell() < last_seg.get_end() {
            for riff_seg in self.odml_riff.iter() {
                if riff_seg.pos >= self.src.tell() {
                    self.src.seek(SeekFrom::Start(riff_seg.movi_pos))?;
                    self.movi_pos = riff_seg.movi_pos;
                    self.movi_size = riff_seg.movi_size;
                    return Ok(());
                }
            }
        }
        self.src.seek(SeekFrom::Start(last_seg.get_end()))?;

        let riff_pos = self.src.tell();
        let ret = self.src.read_tag();
        if let Ok([b'R', b'I', b'F', b'F']) = ret {
        } else {
            return Err(DemuxerError::EOF);
        }
        let riff_size = self.src.read_u32le()? as usize;
        let tag = self.src.read_tag()?;
        validate!(&tag == b"AVIX");
        let tag = self.src.read_tag()?;
        validate!(&tag == b"LIST");
        let list_size = self.src.read_u32le()? as usize;
        validate!(list_size >= 4);
        let tag = self.src.read_tag()?;
        validate!(&tag == b"movi");
        self.odml_riff.push(RIFFSegment{ pos: riff_pos, size: riff_size + 8, movi_pos: riff_pos + 24, movi_size: list_size - 4});
        self.movi_pos = riff_pos + 24;
        self.movi_size = list_size - 4;
        self.movi_orig = self.movi_size;

        Ok(())
    }
}

const RIFF_TAGS: &[[u32; 2]] = &[
    [ mktag!(b"RIFF"), mktag!(b"AVI ") ],
    [ mktag!(b"RIFF"), mktag!(b"AVIX") ],
    [ mktag!(b"ON2 "), mktag!(b"ON2f") ],
];

const CHUNKS: &[RIFFParser] = &[
    RIFFParser { tag: RIFFTag::List(mktag!(b"LIST"), mktag!(b"hdrl")), parse: parse_hdrl },
    RIFFParser { tag: RIFFTag::List(mktag!(b"LIST"), mktag!(b"strl")), parse: parse_strl },
    RIFFParser { tag: RIFFTag::Chunk(mktag!(b"avih")), parse: parse_avih },
    RIFFParser { tag: RIFFTag::Chunk(mktag!(b"ON2h")), parse: parse_avih },
    RIFFParser { tag: RIFFTag::Chunk(mktag!(b"strf")), parse: parse_strf },
    RIFFParser { tag: RIFFTag::Chunk(mktag!(b"strh")), parse: parse_strh },
    RIFFParser { tag: RIFFTag::Chunk(mktag!(b"indx")), parse: parse_indx },
    RIFFParser { tag: RIFFTag::Chunk(mktag!(b"JUNK")), parse: parse_junk },
    RIFFParser { tag: RIFFTag::List(mktag!(b"LIST"), mktag!(b"odml")), parse: parse_odml },
];

fn is_list_tag(tag: u32) -> bool {
    for chunk in CHUNKS.iter() {
        if let RIFFTag::List(ltag, _) = chunk.tag {
            if tag == ltag {
                return true;
            }
        }
    }
    false
}

#[allow(unused_variables)]
fn parse_hdrl(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    Ok(0)
}

#[allow(unused_variables)]
fn parse_strl(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    Ok(0)
}

fn parse_odml(dmx: &mut AVIDemuxer, _strmgr: &mut StreamManager, _size: usize) -> DemuxerResult<usize> {
    dmx.odml = true;
    Ok(0)
}

#[allow(unused_variables)]
fn parse_strh(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    if size < 0x38 { return Err(InvalidData); }
    let tag = dmx.src.read_u32be()?; //stream type
    let fcc = dmx.src.read_u32be()?; //handler(fourcc)
    dmx.src.read_u32le()?; //flags
    dmx.src.read_skip(2)?; //priority
    dmx.src.read_skip(2)?; //language
    dmx.src.read_skip(4)?; //initial frames
    dmx.tb_num = dmx.src.read_u32le()?; //scale
    dmx.tb_den = dmx.src.read_u32le()?; //rate
    dmx.src.read_skip(4)?; //start
    dmx.strm_duration = dmx.src.read_u32le()?;
    dmx.src.read_skip(4)?; //buf size
    dmx.src.read_skip(4)?; //quality
    dmx.src.read_skip(4)?; //sample size
    let a = dmx.src.read_u16le()?;
    let b = dmx.src.read_u16le()?;
    let c = dmx.src.read_u16le()?;
    let d = dmx.src.read_u16le()?;

    dmx.src.read_skip(size - 0x38)?;

    if !dmx.sstate.valid_state() || dmx.sstate.strm_no >= dmx.num_streams {
        return Err(InvalidData);
    }
    if tag == mktag!(b"vids") {
        dmx.sstate.strm_type = Some(StreamType::Video);
    } else if tag == mktag!(b"auds") {
        dmx.sstate.strm_type = Some(StreamType::Audio);
    } else {
        dmx.sstate.strm_type = Some(StreamType::Data);
    }
    dmx.sstate.got_strf = false;

    Ok(size)
}

fn parse_strf(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    if dmx.sstate.strm_type.is_none() { return Err(InvalidData); }
    match dmx.sstate.strm_type.unwrap() {
        StreamType::Video    => parse_strf_vids(dmx, strmgr, size),
        StreamType::Audio    => parse_strf_auds(dmx, strmgr, size),
        _                    => parse_strf_xxxx(dmx, strmgr, size),
    }
}

#[allow(unused_variables)]
fn parse_strf_vids(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    if size < 40 { return Err(InvalidData); }
    let bi_size         = dmx.src.read_u32le()?;
    if (bi_size as usize) < 40 { return Err(InvalidData); }
    let width           = dmx.src.read_u32le()?;
    let height          = dmx.src.read_u32le()? as i32;
    let planes          = dmx.src.read_u16le()?;
    let bitcount        = dmx.src.read_u16le()?;
    let mut compression: [u8; 4] = [0; 4];
                          dmx.src.read_buf(&mut compression)?;
    let img_size        = dmx.src.read_u32le()?;
    let xdpi            = dmx.src.read_u32le()?;
    let ydpi            = dmx.src.read_u32le()?;
    let colors          = dmx.src.read_u32le()?;
    validate!(colors <= 256);
    let imp_colors      = dmx.src.read_u32le()?;

    let flip = height < 0;
    let format = if bitcount > 8 { RGB24_FORMAT } else { PAL8_FORMAT };
    let mut vhdr = NAVideoInfo::new(width as usize, if flip { -height as usize } else { height as usize}, flip, format);
    vhdr.bits = (planes as u8) * (bitcount as u8);
    let cname = if find_raw_rgb_fmt(&compression, planes, bitcount, flip, &mut vhdr) {
            "rawvideo-ms"
        } else if find_raw_yuv_fmt(&compression, &mut vhdr) {
            "rawvideo"
        } else {
            match register::find_codec_from_avi_fourcc(&compression) {
                None => "unknown",
                Some(name) => name,
            }
        };
    let vci = NACodecTypeInfo::Video(vhdr);
    let edata = dmx.read_extradata(size - 40)?;
    if colors > 0 {
        if let Some(ref buf) = edata {
            let mut pal = [0u8; 1024];
            for (dpal, spal) in pal.chunks_mut(4).take(colors as usize).zip(buf.chunks(4)) {
                dpal[0] = spal[2];
                dpal[1] = spal[1];
                dpal[2] = spal[0];
                dpal[3] = 0;
            }
            let pal = PalInfo { pal: Arc::new(pal), changed: true, stream_no: strmgr.get_num_streams() };
            dmx.pal.push(pal);
        }
    }
    let vinfo = NACodecInfo::new(cname, vci, edata);
    let res = strmgr.add_stream(NAStream::new(StreamType::Video, u32::from(dmx.sstate.strm_no), vinfo, dmx.tb_num, dmx.tb_den, u64::from(dmx.strm_duration)));
    if res.is_none() { return Err(MemoryError); }
    dmx.sstate.reset();
    Ok(size)
}

fn find_raw_rgb_fmt(compr: &[u8; 4], planes: u16, bitcount: u16, flip: bool, vhdr: &mut NAVideoInfo) -> bool {
    match compr {
        &[0, 0, 0, 0] | b"DIB " => {
            if planes != 1 {
                return false;
            }
            let fmt_name = match bitcount {
                     8 => "pal8",
                    16 => "bgr555",
                    24 => "bgr24",
                    32 => "bgra24",
                    _ => return false,
                };
            if let Ok(fmt) = NAPixelFormaton::from_str(fmt_name) {
                vhdr.format = fmt;
                vhdr.flipped = !flip;
                true
            } else {
                false
            }
        },
        _ => false,
    }
}

fn find_raw_yuv_fmt(compr: &[u8; 4], vhdr: &mut NAVideoInfo) -> bool {
    let (fmt_name, swapuv) = match compr {
            b"UYVY" | b"UYNY" | b"UYNV" | b"2vuy" => ("uyvy", false),
            b"I420" | b"IYUV" => ("yuv420p", false),
            b"YV12" => ("yuv420p", true),
            b"YVU9" => ("yuv410p", true),
            _ => return false,
        };
    if let Ok(fmt) = NAPixelFormaton::from_str(fmt_name) {
        vhdr.format = fmt;
        if swapuv {
            vhdr.format.comp_info.swap(1, 2);
        }
        true
    } else {
        false
    }
}

#[allow(unused_variables)]
fn parse_strf_auds(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    if size < 16 { return Err(InvalidData); }
    let w_format_tag        = dmx.src.read_u16le()?;
    let channels            = dmx.src.read_u16le()?;
    let samplespersec       = dmx.src.read_u32le()?;
    let avgbytespersec      = dmx.src.read_u32le()?;
    let block_align         = dmx.src.read_u16le()?;
    let bits_per_sample     = dmx.src.read_u16le()?;

    let signed = bits_per_sample > 8;
    let soniton = NASoniton::new(bits_per_sample as u8, if signed { SONITON_FLAG_SIGNED } else { 0 });
    let ahdr = NAAudioInfo::new(samplespersec, channels as u8, soniton, block_align as usize);
    let edata = if size > 16 {
            let edata_size  = dmx.src.read_u16le()? as usize;
            validate!(edata_size + 18 <= size);
            dmx.read_extradata(size - 18)?
        } else {
            None
        };
    let cname = match register::find_codec_from_wav_twocc(w_format_tag) {
                    None => "unknown",
                    Some(name) => name,
                };
    let ainfo = NACodecInfo::new(cname, NACodecTypeInfo::Audio(ahdr), edata);
    let res = strmgr.add_stream(NAStream::new(StreamType::Audio, u32::from(dmx.sstate.strm_no), ainfo, dmx.tb_num, dmx.tb_den, u64::from(dmx.strm_duration)));
    if res.is_none() { return Err(MemoryError); }
    dmx.sstate.reset();
    Ok(size)
}

fn parse_strf_xxxx(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    let edata = dmx.read_extradata(size)?;
    let info = NACodecInfo::new("unknown", NACodecTypeInfo::None, edata);
    let res = strmgr.add_stream(NAStream::new(StreamType::Data, u32::from(dmx.sstate.strm_no), info, dmx.tb_num, dmx.tb_den, u64::from(dmx.strm_duration)));
    if res.is_none() { return Err(MemoryError); }
    dmx.sstate.reset();
    Ok(size)
}

#[allow(unused_variables)]
fn parse_avih(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    if size < 0x38 { return Err(InvalidData); }
    let timebase = dmx.src.read_u32le()?; //microsec per frame
    dmx.src.read_skip(4)?; //max frame size
    dmx.src.read_skip(4)?; //padding
    dmx.src.read_u32le()?; //flags
    let frames = dmx.src.read_u32le()?; //frames
    dmx.src.read_skip(4)?; //initial frames
    let streams = dmx.src.read_u32le()?; //streams
    if streams > 100 { return Err(InvalidData); }
    dmx.num_streams = streams as u8;

    dmx.src.read_skip(4)?; //buf size
    let width = dmx.src.read_u32le()?; //width
    let height = dmx.src.read_u32le()? as i32; //height
    dmx.src.read_skip(16)?; //reserved

    dmx.cur_frame.resize(streams as usize, 0);
    dmx.src.read_skip(size - 0x38)?;
    Ok(size)
}

fn parse_indx(dmx: &mut AVIDemuxer, _strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    dmx.odml = true;
    validate!(size >= 24);
    let entry_size = dmx.src.read_u16le()? as usize;
    if entry_size != 4 {
        dmx.src.read_skip(size - 2)?;
        return Ok(size);
    }
    let sub_type = dmx.src.read_byte()?;
    let idx_type = dmx.src.read_byte()?;
    validate!(sub_type == 0 && idx_type == 0);
    let entries = dmx.src.read_u32le()? as usize;
    validate!(size >= 24 + entries * 4 * entry_size);
    dmx.src.read_tag()?; //chunk id
    dmx.src.read_skip(12)?; // reserved
    for _ in 0..entries {
        let offset = dmx.src.read_u64le()?;
        let _idx_len = dmx.src.read_u32le()?;
        let _nframes = dmx.src.read_u32le()?;
        dmx.odml_idx.push(offset);
    }
    dmx.src.read_skip(size - 24 - entries * 4 * entry_size)?;
    Ok(size)
}

#[allow(unused_variables)]
fn parse_junk(dmx: &mut AVIDemuxer, strmgr: &mut StreamManager, size: usize) -> DemuxerResult<usize> {
    dmx.src.read_skip(size)?;
    Ok(size)
}

#[allow(clippy::verbose_bit_mask)]
fn parse_idx1(src: &mut ByteReader, strmgr: &mut StreamManager, seek_idx: &mut SeekIndex, size: usize, movi_pos: u64, key_offs: &mut Vec<u64>) -> DemuxerResult<usize> {
    validate!((size & 15) == 0);
    let mut tag = [0u8; 4];
    let num_entries = size >> 4;
    let mut counter = [0u64; 100];
    let mut add_offset = 0;
    let mut set_offset = false;
    for _ in 0..num_entries {
                              src.read_buf(&mut tag)?;
        let flags           = src.read_u32le()?;
        let mut offset      = src.read_u32le()? as u64;
        let _length         = src.read_u32le()?;

        if !set_offset && offset < movi_pos {
            add_offset = movi_pos - offset;
        }
        set_offset = true;

        offset += add_offset;

        if tag[0] < b'0' || tag[0] > b'9' || tag[1] < b'0' || tag[1] > b'9' {
            continue;
        }
        let stream_no = ((tag[0] - b'0') * 10 + (tag[1] - b'0')) as usize;

        if (flags & 0x10) != 0 {
            if let Some(stream) = strmgr.get_stream(stream_no) {
                if stream.get_media_type() == StreamType::Video {
                    let (tb_num, tb_den) = stream.get_timebase();
                    let pts = counter[stream_no];
                    let time = NATimeInfo::ts_to_time(pts, 1000, tb_num, tb_den);
                    validate!(offset >= movi_pos);
                    seek_idx.add_entry(stream_no as u32, SeekEntry { time, pts, pos: offset });
                }
                key_offs.push(offset);
            }
        }
        counter[stream_no] += 1;
    }
    key_offs.sort_unstable();
    Ok(size)
}

fn parse_odml_ix(src: &mut ByteReader, strmgr: &mut StreamManager, seek_idx: &mut SeekIndex, stream_no: usize, size: usize, start: u64) -> DemuxerResult<u64> {
    validate!(size >= 24);
    let entry_size = src.read_u16le()? as usize;
    if entry_size != 2 {
        return Err(DemuxerError::NotImplemented);
    }
    let sub_type = src.read_byte()?;
    let idx_type = src.read_byte()?;
    validate!(sub_type == 0 && idx_type == 1);
    let entries = src.read_u32le()? as usize;
    validate!(size >= 24 + entries * 4 * entry_size);
    src.read_tag()?; //chunk id
    let base_offset = src.read_u64le()?;
    src.read_u32le()?; //reserved
    if let Some(stream) = strmgr.get_stream(stream_no) {
        if stream.get_media_type() == StreamType::Video {
            let (tb_num, tb_den) = stream.get_timebase();

            for i in 0..entries {
                let offset = src.read_u32le()?;
                validate!(offset >= 8);
                let _size  = src.read_u32le()?;

                let pts = start + (i as u64);
                let time = NATimeInfo::ts_to_time(pts, 1000, tb_num, tb_den);
                seek_idx.add_entry(stream_no as u32, SeekEntry { time, pts, pos: base_offset + u64::from(offset - 8) });
            }

            Ok(start + (entries as u64))
        } else {
            Ok(0)
        }
    } else {
        Ok(0)
    }
}

pub struct AVIDemuxerCreator { }

impl DemuxerCreator for AVIDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(AVIDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "avi" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_avi_demux() {
        //test sample: https://samples.mplayerhq.hu/V-codecs/RT21/320x240/laser05.avi
        let mut file = File::open("assets/Indeo/laser05.avi").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = AVIDemuxer::new(&mut br);
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
