use super::*;
use nihav_core::demuxers::DemuxerError::*;
use std::io::SeekFrom;
use std::mem;
use std::fmt;

macro_rules! mktag {
    ($a:expr, $b:expr, $c:expr, $d:expr) => ({
        (($a as u32) << 24) | (($b as u32) << 16) | (($c as u32) << 8) | ($d as u32)
    });
    ($arr:expr) => ({
        (($arr[0] as u32) << 24) | (($arr[1] as u32) << 16) | (($arr[2] as u32) << 8) | ($arr[3] as u32)
    });
}

trait ReadSize {
    fn read_size(&mut self, ver: u16) -> ByteIOResult<u64>;
}

impl<'a> ReadSize for ByteReader<'a> {
    fn read_size(&mut self, ver: u16) -> ByteIOResult<u64> {
        match ver {
            0 => Ok(u64::from(self.read_u32be()?)),
            2 => self.read_u64be(),
            _ => unreachable!(),
        }
    }
}

const RM_SIPRO_BLOCK_SIZES: [usize; 4] = [ 29, 19, 37, 20 ];
const RM_SIPRO_SWAPS:   [[u8; 2]; 38] = [
    [  0, 63 ], [  1, 22 ], [  2, 44 ], [  3, 90 ],
    [  5, 81 ], [  7, 31 ], [  8, 86 ], [  9, 58 ],
    [ 10, 36 ], [ 12, 68 ], [ 13, 39 ], [ 14, 73 ],
    [ 15, 53 ], [ 16, 69 ], [ 17, 57 ], [ 19, 88 ],
    [ 20, 34 ], [ 21, 71 ], [ 24, 46 ], [ 25, 94 ],
    [ 26, 54 ], [ 28, 75 ], [ 29, 50 ], [ 32, 70 ],
    [ 33, 92 ], [ 35, 74 ], [ 38, 85 ], [ 40, 56 ],
    [ 42, 87 ], [ 43, 65 ], [ 45, 59 ], [ 48, 79 ],
    [ 49, 93 ], [ 51, 89 ], [ 55, 95 ], [ 61, 76 ],
    [ 67, 83 ], [ 77, 80 ]
];

struct RMVideoStream {
    frame:      Vec<u8>,
    hdr_size:   usize,
    frame_size: usize,
    frame_pos:  usize,
}

impl RMVideoStream {
    fn new() -> Self {
        RMVideoStream {
            frame:      Vec::new(),
            hdr_size:   0,
            frame_size: 0,
            frame_pos:  0,
        }
    }
    fn flush(&mut self) {
        self.frame.clear();
        self.frame_size = 0;
        self.frame_pos  = 0;
    }
    fn start_slice(&mut self, num_slices: usize, frame_size: usize, data: &[u8]) {
        self.hdr_size = num_slices * 8 + 1;
        self.frame.resize(frame_size + self.hdr_size, 0);
        self.frame[0] = (num_slices - 1) as u8;
        self.frame_pos = 0;
        self.add_slice(1, data).unwrap();
    }
    fn add_slice(&mut self, slice_no: usize, data: &[u8]) -> DemuxerResult<()> {
        validate!(self.hdr_size + self.frame_pos + data.len() <= self.frame.len());
        self.write_slice_info(slice_no);
        let dslice = &mut self.frame[self.hdr_size + self.frame_pos..][..data.len()];
        dslice.copy_from_slice(data);
        self.frame_pos += data.len();
        Ok(())
    }
    fn write_slice_info(&mut self, slice_no: usize) {
        let off = 1 + (slice_no - 1) * 8;
        self.frame[off + 0] = 0;
        self.frame[off + 1] = 0;
        self.frame[off + 2] = 0;
        self.frame[off + 3] = 1;
        self.frame[off + 4] = (self.frame_pos >> 24) as u8;
        self.frame[off + 5] = (self.frame_pos >> 16) as u8;
        self.frame[off + 6] = (self.frame_pos >>  8) as u8;
        self.frame[off + 7] = (self.frame_pos >>  0) as u8;
    }
    fn get_frame_data(&mut self) -> Vec<u8> {
        let mut v: Vec<u8> = Vec::new();
        mem::swap(&mut v, &mut self.frame);
        self.flush();
        v
    }
}

#[derive(Clone,Copy,PartialEq)]
enum Deinterleaver {
    None,
    RA28_8,
    Generic,
    Sipro,
    VBR,
}

struct RMAudioStream {
    deint:      Deinterleaver,
    iinfo:      Option<InterleaveInfo>,
    buf:        Vec<u8>,
    sub_packet: usize,
}

const RM_ILEAVE_INT0: u32 = mktag!(b"Int0");
const RM_ILEAVE_INT4: u32 = mktag!(b"Int4");
const RM_ILEAVE_GENR: u32 = mktag!(b"genr");
const RM_ILEAVE_SIPR: u32 = mktag!(b"sipr");
const RM_ILEAVE_VBRF: u32 = mktag!(b"vbrf");
const RM_ILEAVE_VBRS: u32 = mktag!(b"vbrs");

impl RMAudioStream {
    fn new(iinfo: Option<InterleaveInfo>) -> Self {
        let deint;
        let buf: Vec<u8>;
        if let Some(info) = iinfo {
            deint = match info.id {
                    RM_ILEAVE_INT0 => Deinterleaver::None,
                    RM_ILEAVE_INT4 => Deinterleaver::RA28_8,
                    RM_ILEAVE_GENR => Deinterleaver::Generic,
                    RM_ILEAVE_SIPR => Deinterleaver::Sipro,
                    RM_ILEAVE_VBRF => Deinterleaver::VBR,
                    RM_ILEAVE_VBRS => Deinterleaver::VBR,
                    _ => {println!("unknown deint {:X}", info.id); Deinterleaver::None },
                };
            match deint {
                Deinterleaver::None     => { buf = Vec::new(); },
                Deinterleaver::RA28_8  |
                Deinterleaver::Generic |
                Deinterleaver::Sipro    => {
                        let bsize = (info.frame_size as usize) * (info.factor as usize);
                        buf = vec![0; bsize];
                    },
                Deinterleaver::VBR      => {
                        buf = Vec::new();
                    },
            };
        } else {
            deint = Deinterleaver::None;
            buf = Vec::new();
        }
        RMAudioStream { deint, iinfo, buf, sub_packet: 0 }
    }
    fn read_apackets(&mut self, queued_packets: &mut Vec<NAPacket>, src: &mut ByteReader, stream: NAStreamRef, ts: u32, keyframe: bool, payload_size: usize) -> DemuxerResult<NAPacket> {
        let ts = stream.make_ts(Some(ts as u64), None, None);

        if keyframe {
            self.sub_packet = 0;
        }
        match self.deint {
            Deinterleaver::None     => { return src.read_packet(stream, ts, keyframe, payload_size); },
            Deinterleaver::RA28_8   => {
                    let iinfo = self.iinfo.unwrap();
                    let factor   = iinfo.factor as usize;
                    let halffact = factor >> 1;
                    let fsize    = iinfo.frame_size as usize;
                    let bsize    = iinfo.block_size as usize;
                    let ppos     = self.sub_packet;
                    for sb in 0..halffact {
                        let dst = &mut self.buf[sb * 2 * fsize + ppos * bsize..][..bsize];
                        src.read_buf(dst)?;
                    }
                    self.sub_packet += 1;
                    if self.sub_packet == factor {
                        self.sub_packet = 0;
                        return Ok(NAPacket::new(stream, ts, true, self.buf.clone()));
                    } else {
                        return Err(DemuxerError::TryAgain);
                    }
                },
            Deinterleaver::Generic  => {
                    let iinfo = self.iinfo.unwrap();
                    let factor   = iinfo.factor as usize;
                    let fsize    = iinfo.frame_size as usize;
                    let bsize    = iinfo.block_size as usize;
                    let factor2  = fsize / bsize;
                    let ppos     = self.sub_packet;

                    for sb in 0..factor2 {
                        let sb_pos = factor * sb + ((factor + 1) >> 1) * (ppos & 1) + (ppos >> 1);
                        let dst = &mut self.buf[bsize * sb_pos..][..bsize];
                        src.read_buf(dst)?;
                    }
                },
            Deinterleaver::Sipro    => {
                    let iinfo = self.iinfo.unwrap();
                    let fsize    = iinfo.frame_size as usize;
                    let ppos     = self.sub_packet;

                    let dst = &mut self.buf[fsize * ppos..][..fsize];
                    src.read_buf(dst)?;
                },
            Deinterleaver::VBR      => {
                    validate!(payload_size >= 5);
                    let hdrsizesize         = src.read_u16be()?;
                    let num_entries = (hdrsizesize / 16) as usize;
                    validate!(payload_size >= num_entries * 3 + 2);
                    let mut sizes: Vec<usize> = Vec::with_capacity(num_entries);
                    let mut tot_size = 0;
                    for _ in 0..num_entries {
                        let sz              = src.read_u16be()? as usize;
                        tot_size += sz;
                        sizes.push(sz);
                    }
                    validate!(tot_size + num_entries * 2 + 2 == payload_size);
                    let pkt_ts = stream.make_ts(None, None, None);
                    let mut first = true;
                    for size in sizes.iter() {
                        let cur_ts = if first { ts } else { pkt_ts };
                        first = false;
                        let pkt = src.read_packet(stream.clone(), cur_ts, true, *size)?;
                        queued_packets.push(pkt);
                    }
                    queued_packets.reverse();
                    let pkt0 = queued_packets.pop().unwrap();
                    return Ok(pkt0);
                },
        };

        let iinfo = self.iinfo.unwrap();
        let factor   = iinfo.factor as usize;
        let fsize    = if iinfo.block_size != 0 { iinfo.block_size } else { iinfo.frame_size } as usize;

        self.sub_packet += 1;
        if self.sub_packet < factor {
            return Err(DemuxerError::TryAgain);
        }

        self.sub_packet = 0;

        if self.deint == Deinterleaver::Sipro {
            sipro_restore(&mut self.buf, factor, iinfo.frame_size as usize);
        }

        let mut frames_iter = self.buf.chunks(fsize);
        let pkt0 = frames_iter.next().unwrap();

        let pkt_ts = stream.make_ts(None, None, None);
        for pkts in frames_iter {
            let pkt = NAPacket::new(stream.clone(), pkt_ts, true, pkts.to_vec());
            queued_packets.push(pkt);
        }
        queued_packets.reverse();
        Ok(NAPacket::new(stream, ts, true, pkt0.to_vec()))
    }
}

fn sipro_restore(buf: &mut [u8], factor: usize, fsize: usize) {
    let stride = factor * fsize * 2 / 96;
    for i in 0..38 {
        let mut sidx = (RM_SIPRO_SWAPS[i][0] as usize) * stride;
        let mut didx = (RM_SIPRO_SWAPS[i][1] as usize) * stride;
        for _ in 0..stride {
            let in0 = buf[sidx >> 1];
            let in1 = buf[didx >> 1];
            let nib0 = (in0 >> ((sidx & 1) * 4)) & 0xF;
            let nib1 = (in1 >> ((didx & 1) * 4)) & 0xF;

            buf[didx >> 1] = (nib0 << (4 * (didx & 1))) | (in1 & (0xF << (4 * (!didx & 1))));
            buf[sidx >> 1] = (nib1 << (4 * (sidx & 1))) | (in0 & (0xF << (4 * (!sidx & 1))));

            sidx += 1;
            didx += 1;
        }
    }
}

struct SubstreamInfo {
    id:         u32,
    map:        Vec<u16>,
    str_ids:    Vec<u32>,
}

struct MLTIMapper {
    sub_info:   Vec<SubstreamInfo>,
    sstr_id:    u32,
}

impl MLTIMapper {
    fn new() -> Self {
        MLTIMapper {
            sub_info:   Vec::new(),
            sstr_id:    0x10000,
        }
    }
    fn add_stream(&mut self, stream_no: u32) {
        let ssinfo = SubstreamInfo { id: stream_no, map: Vec::new(), str_ids: Vec::new() };
        self.sub_info.push(ssinfo);
    }
    fn get_substream_no(&self) -> u32 {
        self.sstr_id - 1
    }
    fn find_idx(&self, stream_no: u32) -> Option<usize> {
        self.sub_info.iter().position(|x| x.id == stream_no)
    }
    fn add_map_rule(&mut self, map_ss: u16) {
        let idx = self.sub_info.len() - 1;
        self.sub_info[idx].map.push(map_ss);
    }
    fn add_substream(&mut self) {
        let idx = self.sub_info.len() - 1;
        self.sub_info[idx].str_ids.push(self.sstr_id);
        self.sstr_id += 1;
    }
    fn is_mlti_stream(&self, stream_no: u32) -> bool {
        self.find_idx(stream_no).is_some()
    }
    fn find_substream(&self, stream_no: u32, grp: u16) -> Option<u32> {
        if let Some(idx) = self.find_idx(stream_no) {
            if (grp as usize) < self.sub_info[idx].map.len() {
                let sub_id = self.sub_info[idx].map[grp as usize] as usize;
                if sub_id < self.sub_info[idx].str_ids.len() {
                    return Some(self.sub_info[idx].str_ids[sub_id]);
                }
            }
        }
        None
    }
}

enum RMStreamType {
    Audio(RMAudioStream),
    Video(RMVideoStream),
    Logical,
    Unknown,
}

struct CommonStreamData {
    streams:        Vec<RMStreamType>,
    str_ids:        Vec<u32>,
    mlti_mapper:    MLTIMapper,
}

impl CommonStreamData {
    fn new() -> Self {
        CommonStreamData {
            streams:        Vec::new(),
            str_ids:        Vec::new(),
            mlti_mapper:    MLTIMapper::new(),
        }
    }
    fn get_stream_id(&self, str_no: u32, pkt_grp: u16) -> u32 {
        if !self.mlti_mapper.is_mlti_stream(str_no) {
            str_no
        } else {
            self.mlti_mapper.find_substream(str_no, pkt_grp).unwrap()
        }
    }
    fn find_stream(&self, stream_id: u32) -> Option<usize> {
        self.str_ids.iter().position(|x| *x == stream_id)
    }
}

struct RealMediaDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    data_pos:       u64,
    data_end:       u64,
    next_data:      u64,
    data_ver:       u16,
    num_packets:    u32,
    cur_packet:     u32,

    str_data:       CommonStreamData,

    data_chunks:    Vec<(u64, u32, u16)>,
    cur_data_chunk: usize,

    queued_pkts:    Vec<NAPacket>,
    slice_buf:      Vec<u8>,
}

fn find_codec_name(registry: &[(&[u8;4], &'static str)], fcc: u32) -> &'static str {
    for &(fourcc, name) in registry {
        if mktag!(fourcc) == fcc { return name; }
    }
    "unknown"
}

fn read_14or30(src: &mut ByteReader) -> DemuxerResult<(bool, u32)> {
    let tmp = src.read_u16be()?;
    let flag = (tmp & 0x8000) != 0;
    if (tmp & 0x4000) == 0x4000 {
        Ok((flag, ((tmp & 0x3FFF) as u32)))
    } else {
        let val = ((tmp as u32) << 16) | (src.read_u16be()? as u32);
        Ok((flag, val & 0x3FFFFFFF))
    }
}

fn read_video_buf(src: &mut ByteReader, stream: NAStreamRef, ts: u32, keyframe: bool, frame_size: usize) -> DemuxerResult<NAPacket> {
    let size = frame_size + 9;
    let mut vec: Vec<u8> = vec![0; size];
    //v[0] = 0; // 1 slice
    vec[4] = 1;
    src.read_buf(&mut vec[9..])?;

    let ts = stream.make_ts(Some(ts as u64), None, None);
    Ok(NAPacket::new(stream, ts, keyframe, vec))
}

fn read_multiple_frame(src: &mut ByteReader, stream: NAStreamRef, keyframe: bool, skip_mtype: bool) -> DemuxerResult<NAPacket> {
    if !skip_mtype {
        let mtype       = src.read_byte()?;
        validate!(mtype == 0xC0);
    }
    let (_, frame_size) = read_14or30(src)?;
    let (_, timestamp)  = read_14or30(src)?;
    let _seq_no         = src.read_byte()?;

    read_video_buf(src, stream, timestamp, keyframe, frame_size as usize)
}

struct RMDemuxCommon {}

impl RMDemuxCommon {
    fn parse_stream_info(str_data: &mut CommonStreamData, strmgr: &mut StreamManager, stream_no: u32, edata: &[u8], duration: u32) -> DemuxerResult<bool> {
        let mut is_mlti = false;
        let mut mr = MemoryReader::new_read(edata);
        let mut src = ByteReader::new(&mut mr);
        let tag  = src.read_u32be()?;
        let tag2 = src.peek_u32be()?;
        if tag == mktag!('.', 'r', 'a', 0xFD) {
            Self::parse_audio_stream(strmgr, &mut str_data.streams, stream_no, &mut src, edata, duration)?;
        } else if ((tag2 == mktag!('V', 'I', 'D', 'O')) || (tag2 == mktag!('I', 'M', 'A', 'G'))) && ((tag as usize) <= edata.len()) {
            Self::parse_video_stream(strmgr, &mut str_data.streams, stream_no, &mut src, edata, tag2, duration)?;
        } else if tag == mktag!(b"LSD:") {
            let extradata = Some(edata.to_owned());

            src.read_skip(4)?; //version
            let channels    = src.read_u16be()?;
            let samp_size   = src.read_u16be()?;
            let sample_rate = src.read_u32be()?;

            let soniton = NASoniton::new(samp_size as u8, SONITON_FLAG_SIGNED);
            let ahdr = NAAudioInfo::new(sample_rate, channels as u8, soniton, 1);
            let nainfo = NACodecInfo::new("ralf", NACodecTypeInfo::Audio(ahdr), extradata);
            let res = strmgr.add_stream(NAStream::new(StreamType::Audio, stream_no, nainfo, 1, 1000, u64::from(duration)));
            if res.is_none() { return Err(MemoryError); }
            let astr = RMAudioStream::new(None);
            str_data.streams.push(RMStreamType::Audio(astr));
        } else if tag == mktag!(b"MLTI") {
            is_mlti = true;
            let num_rules       = src.read_u16be()? as usize;
            let mut max_sub = 0;
            str_data.mlti_mapper.add_stream(stream_no);
            for _ in 0..num_rules {
                let substr      = src.read_u16be()?;
                max_sub = max_sub.max(substr);
                str_data.mlti_mapper.add_map_rule(substr);
            }
            let num_substreams  = src.read_u16be()? as usize;
            validate!(num_substreams > (max_sub as usize));
            for _ in 0..num_substreams {
                let hdr_size    = src.read_u32be()? as usize;
                validate!(hdr_size > 8);
                let pos = src.tell() as usize;
                src.read_skip(hdr_size)?;
                str_data.mlti_mapper.add_substream();
                {
                    let hdrsrc = &edata[pos..][..hdr_size];
                    let mut mr = MemoryReader::new_read(hdrsrc);
                    let mut hsrc = ByteReader::new(&mut mr);

                    let tag  = hsrc.read_u32be()?;
                    let tag2 = hsrc.peek_u32be()?;
                    let stream_no = str_data.mlti_mapper.get_substream_no();
//todo check that all substreams are of the same type");
                    if tag == mktag!('.', 'r', 'a', 0xFD) {
                        Self::parse_audio_stream(strmgr, &mut str_data.streams, stream_no, &mut hsrc, hdrsrc, duration)?;
                    } else if (tag2 == mktag!('V', 'I', 'D', 'O')) && ((tag as usize) <= hdr_size) {
                        Self::parse_video_stream(strmgr, &mut str_data.streams, stream_no, &mut hsrc, hdrsrc, tag2, duration)?;
                    } else {
println!("unknown MLTI substream {:08X} / {:08X}", tag, tag2);
                        return Err(DemuxerError::InvalidData);
                    }
                    str_data.str_ids.push(stream_no);
                }
            }
        } else {
            str_data.streams.push(RMStreamType::Logical);
        }
        Ok(is_mlti)
    }
    fn parse_audio_stream(strmgr: &mut StreamManager, streams: &mut Vec<RMStreamType>, stream_no: u32, src: &mut ByteReader, edata_: &[u8], duration: u32) -> DemuxerResult<()> {
        let ver         = src.read_u16be()?;
        let ainfo = match ver {
            3 => {
                    parse_aformat3(src)?
                },
            4 => {
                    parse_aformat4(src)?
                },
            5 => {
                    parse_aformat5(src)?
                },
            _ => {
                    println!("unknown version {}", ver);
                    return Err(DemuxerError::InvalidData);
                },
        };
        let cname = find_codec_name(RM_AUDIO_CODEC_REGISTER, ainfo.fcc);
        let blk_size = if ainfo.fcc != mktag!(b"sipr") {
                if let Some(ref iinfo) = ainfo.ileave_info {
                    iinfo.block_size as usize
                } else {
                    ainfo.granularity as usize
                }
            } else {
                validate!(ainfo.flavor <= 3);
                RM_SIPRO_BLOCK_SIZES[ainfo.flavor as usize]
            };
        let srate = ainfo.sample_rate;
        let soniton = NASoniton::new(ainfo.sample_size as u8, SONITON_FLAG_SIGNED);
        let ahdr = NAAudioInfo::new(srate, ainfo.channels as u8, soniton, blk_size);
        let extradata = if ainfo.edata_size == 0 {
                None
            } else {
                let eslice = &edata_[(src.tell() as usize)..];
                Some(eslice.to_vec())
            };
        let duration = if duration == 0 { ainfo.get_duration(1000) } else { u64::from(duration) };
        let nainfo = NACodecInfo::new(cname, NACodecTypeInfo::Audio(ahdr), extradata);
        let res = strmgr.add_stream(NAStream::new(StreamType::Audio, stream_no, nainfo, 1, 1000, duration));
        if res.is_none() { return Err(MemoryError); }

        let astr = RMAudioStream::new(ainfo.ileave_info);
        streams.push(RMStreamType::Audio(astr));
        Ok(())
    }
#[allow(unused_variables)]
    fn parse_video_stream(strmgr: &mut StreamManager, streams: &mut Vec<RMStreamType>, stream_no: u32, src: &mut ByteReader, edata_: &[u8], tag2: u32, duration: u32) -> DemuxerResult<()> {
        src.read_skip(4)?;
        let fcc         = src.read_u32be()?;
        let width       = src.read_u16be()? as usize;
        let height      = src.read_u16be()? as usize;
        let bpp         = src.read_u16be()?;
        let pad_w       = src.read_u16be()?;
        let pad_h       = src.read_u16be()?;
        let _fps;
        if tag2 == mktag!('V', 'I', 'D', 'O') {
            _fps        = src.read_u32be()?;
        } else {
            _fps = 0x10000;
        }
        let extradata: Option<Vec<u8>>;
        if src.left() > 0 {
            let eslice = &edata_[(src.tell() as usize)..];
            extradata = Some(eslice.to_vec());
        } else {
            extradata = None;
        }
        let cname = find_codec_name(RM_VIDEO_CODEC_REGISTER, fcc);

        let vhdr = NAVideoInfo::new(width, height, false, RGB24_FORMAT);
        let vinfo = NACodecInfo::new(cname, NACodecTypeInfo::Video(vhdr), extradata);
        let res = strmgr.add_stream(NAStream::new(StreamType::Video, stream_no, vinfo, 1, 1000, u64::from(duration)));
        if res.is_none() { return Err(DemuxerError::MemoryError); }

        let vstr = RMVideoStream::new();
        streams.push(RMStreamType::Video(vstr));
        Ok(())
    }
#[allow(unused_variables)]
    #[allow(clippy::question_mark)]
    fn parse_packet_payload(src: &mut ByteReader, rmstream: &mut RMStreamType, stream: NAStreamRef, slice_buf: &mut Vec<u8>, queued_pkts: &mut Vec<NAPacket>, keyframe: bool, ts: u32, payload_size: usize) -> DemuxerResult<NAPacket> {
        match rmstream {
            RMStreamType::Video(ref mut vstr) => {

                    let pos = src.tell();
                    let b0          = src.read_byte()?;
                    match b0 >> 6 {
                        0 => { // partial frame
                                let b1  = src.read_byte()?;
                                let hdr1 = ((b0 as u16) << 8) | (b1 as u16);
                                let num_pkts = ((hdr1 >> 7) & 0x7F) as usize;
                                let packet_num = hdr1 & 0x7F;
                                let (_, frame_size) = read_14or30(src)?;
                                let (_, off)        = read_14or30(src)?;
                                let seq_no = src.read_byte()?;
                                let hdr_skip = (src.tell() - pos) as usize;

                                let slice_size = payload_size - hdr_skip;
                                slice_buf.resize(slice_size, 0);
                                src.read_buf(slice_buf.as_mut_slice())?;
                                if packet_num == 1 {
                                    vstr.start_slice(num_pkts, frame_size as usize, slice_buf.as_slice());
                                } else {
                                    vstr.add_slice(packet_num as usize, slice_buf.as_slice())?;
                                }
                                if (packet_num as usize) < num_pkts {
                                    return Err(DemuxerError::TryAgain);
                                }
                                //todo: check if full frame is received
                                let ts = stream.make_ts(Some(ts as u64), None, None);
                                let pkt = NAPacket::new(stream, ts, keyframe, vstr.get_frame_data());
                                Ok(pkt)
                            },
                        1 => { // whole frame
                                let seq_no = src.read_byte()?;
                                read_video_buf(src, stream, ts, keyframe, payload_size - 2)
                            },
                        2 => { // last partial frame
                                let b1  = src.read_byte()?;
                                let hdr1 = ((b0 as u16) << 8) | (b1 as u16);
                                let num_pkts = ((hdr1 >> 7) & 0x7F) as usize;
                                let packet_num = hdr1 & 0x7F;
                                let (_, frame_size) = read_14or30(src)?;
                                let (_, tail_size)  = read_14or30(src)?;
                                let seq_no = src.read_byte()?;
                                slice_buf.resize(tail_size as usize, 0);
                                src.read_buf(slice_buf.as_mut_slice())?;
                                if packet_num == 1 && frame_size == tail_size {
                                    vstr.start_slice(num_pkts, frame_size as usize, slice_buf.as_slice());
                                } else {
                                    vstr.add_slice(packet_num as usize, slice_buf.as_slice())?;
                                }

                                while src.tell() < pos + (payload_size as u64) {
                                    let res = read_multiple_frame(src, stream.clone(), false, false);
                                    if res.is_err() { break; }
                                    queued_pkts.push(res.unwrap());
                                }
                                queued_pkts.reverse();
                                let ts = stream.make_ts(Some(ts as u64), None, None);
                                let pkt = NAPacket::new(stream, ts, keyframe, vstr.get_frame_data());
                                Ok(pkt)
                        },
                    _ => { // multiple frames
                                let res = read_multiple_frame(src, stream.clone(), keyframe, true);
                                if res.is_err() { return res; }
                                while src.tell() < pos + (payload_size as u64) {
                                    let res = read_multiple_frame(src, stream.clone(), false, false);
                                    if res.is_err() { break; }
                                    queued_pkts.push(res.unwrap());
                                }
                                queued_pkts.reverse();
                                res
                            },
                    }
                },
            RMStreamType::Audio(ref mut astr) => {
                    astr.read_apackets(queued_pkts, src, stream, ts, keyframe, payload_size)
                },
            _ => {
                      src.read_skip(payload_size)?;
                    Err(DemuxerError::InvalidData)
                },
        }
    }
}

impl<'a> DemuxCore<'a> for RealMediaDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_idx: &mut SeekIndex) -> DemuxerResult<()> {
        self.read_header(strmgr, seek_idx)?;
        Ok(())
    }

#[allow(unused_variables)]
    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if !self.queued_pkts.is_empty() {
            let pkt = self.queued_pkts.pop().unwrap();
            return Ok(pkt);
        }
        loop {
            if (self.cur_packet >= self.num_packets) || (self.src.tell() >= self.data_end) {
                self.cur_data_chunk += 1;
                if self.cur_data_chunk < self.data_chunks.len() {
                    let (pos, _, _) = self.data_chunks[self.cur_data_chunk];
                    self.src.seek(SeekFrom::Start(pos))?;
                    let res = read_chunk(self.src);
                    if let Ok((id, size, ver)) = res {
                        self.data_pos = self.src.tell();
                        self.data_ver = ver;
                        self.data_end = self.data_pos + (size as u64);
                        if self.parse_data_start().is_ok() {
                            continue;
                        }
                    }
                }
                return Err(DemuxerError::EOF);
            }

            let pkt_start = self.src.tell();
            let ver             = self.src.read_u16be()?;
            validate!(ver <= 1);
            let len             = self.src.read_u16be()? as usize;
            let str_no          = self.src.read_u16be()? as u32;
            let ts              = self.src.read_u32be()?;
            let pkt_grp;
            let flags;
            if ver == 0 {
                pkt_grp         = self.src.read_byte()? as u16;
                flags           = self.src.read_byte()?;
            } else {
                //asm_rule        = self.src.read_u16be()?;
                //asm_flags       = self.src.read_byte()?;
                self.src.read_skip(2)?;
                pkt_grp = 0;
                self.src.read_skip(1)?;
                flags = 0;
            }
            let hdr_size = self.src.tell() - pkt_start;
//println!("packet @{:X} size {} for {} ts {} grp {} flags {:X}", pkt_start, len, str_no, ts, pkt_grp, flags);
            self.cur_packet += 1;

            let payload_size = len - (hdr_size as usize);

            let stream_id = self.str_data.get_stream_id(str_no, pkt_grp);
            let sr = self.str_data.find_stream(stream_id);
            if sr.is_none() {
                self.src.read_skip(payload_size)?;
                return Err(DemuxerError::InvalidData);
            }
            let str_id = sr.unwrap();

            let streamres = strmgr.get_stream_by_id(stream_id);
            if streamres.is_none() {
                self.src.read_skip(payload_size)?;
                continue;
            }
            let stream = streamres.unwrap();
            if strmgr.is_ignored_id(stream_id) {
                self.src.read_skip(payload_size)?;
                continue;
            }
            //todo skip unwanted packet
            let keyframe = (flags & KEYFRAME_FLAG) != 0;

            let ret = RMDemuxCommon::parse_packet_payload(self.src, &mut self.str_data.streams[str_id], stream, &mut self.slice_buf, &mut self.queued_pkts, keyframe, ts, payload_size);
            if let Err(DemuxerError::TryAgain) = ret {
                continue;
            } else {
                return ret;
            }
        }
    }

    #[allow(unused_variables)]
    fn seek(&mut self, time: NATimePoint, seek_idx: &SeekIndex) -> DemuxerResult<()> {
        self.queued_pkts.clear();
        let ret = seek_idx.find_pos(time);
        if ret.is_none() {
            return Err(DemuxerError::SeekError);
        }
        let ret = ret.unwrap();
        let seek_pos = ret.pos;
        for (pos, size, ver) in self.data_chunks.iter() {
            if seek_pos < *pos { continue; }
            let end = *pos + (*size as u64);
            if seek_pos < end {
                self.cur_packet = 0;
                self.data_pos = seek_pos;
                self.data_ver = *ver;
                self.data_end = end;
                self.src.seek(SeekFrom::Start(seek_pos))?;
                return Ok(());
            }
        }
        Err(DemuxerError::SeekError)
    }

    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for RealMediaDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

fn read_chunk(src: &mut ByteReader) -> DemuxerResult<(u32, u32, u16)> {
    let id      = src.read_u32be()?;
if id == 0 { return Ok((0, 0, 0)); }
    let size    = src.read_u32be()?;
if size == 0 {
    let ver     = src.read_u16be()?;
    validate!(ver <= 2);
    return Ok((id, 0x0FFFFFFF, ver));
}
    validate!(size >= 10);
    let ver     = src.read_u16be()?;
    validate!(ver <= 2);
    Ok((id, size, ver))
}

#[derive(Clone,Copy,Debug)]
struct InterleaveInfo {
    id:         u32,
    factor:     u16,
    block_size: u16,
    frame_size: u16,
}

#[derive(Clone,Copy,Debug)]
#[allow(dead_code)]
struct RealAudioInfo {
    fcc:                u32,
    sample_rate:        u32,
    sample_size:        u16,
    flavor:             u16,
    channels:           u16,
    channel_mask:       u32,
    granularity:        u32,
    bytes_per_minute:   u32,
    total_bytes:        u32,
    edata_size:         u32,
    ileave_info:        Option<InterleaveInfo>
}

impl RealAudioInfo {
    fn get_duration(&self, base: u32) -> u64 {
        if self.bytes_per_minute != 0 {
            u64::from(self.total_bytes) * 60 * u64::from(base) / u64::from(self.bytes_per_minute)
        } else {
            0
        }
    }
}

fn skip_ra_metadata(src: &mut ByteReader) -> DemuxerResult<()> {
    let title_len           = src.read_byte()? as usize;
    src.read_skip(title_len)?;
    let author_len          = src.read_byte()? as usize;
    src.read_skip(author_len)?;
    let copywrong_len       = src.read_byte()? as usize;
    src.read_skip(copywrong_len)?;
    let comment_len         = src.read_byte()? as usize;
    src.read_skip(comment_len)?;
    Ok(())
}

#[allow(unused_variables)]
fn parse_aformat3(src: &mut ByteReader) -> DemuxerResult<RealAudioInfo> {
    let start = src.tell();
    let header_len          = src.read_u16be()?;
    validate!(header_len >= 24);
    let flavor              = src.read_u16be()?;
    let granularity         = src.read_u32be()?;
    let bytes_per_minute    = src.read_u32be()?;
    let total_bytes         = src.read_u32be()?;

    skip_ra_metadata(src)?;

    // the original RealAudio has no such fields
    let fcc = if src.tell() != start + u64::from(header_len) + 2 {
        let _can_copy           = src.read_byte()?;
        let fcc_len             = src.read_byte()?;
        validate!(fcc_len == 0 || fcc_len == 4);
                                  src.read_u32be()?
    } else {
        read_u32be(b"lpcJ")?
    };

    let end = src.tell();
    validate!(end - start <= (header_len as u64) + 2);

    Ok(RealAudioInfo {
        fcc, flavor,
        sample_rate: 8000, sample_size: 16, channels: 1, channel_mask: 0,
        granularity, bytes_per_minute,
        total_bytes, edata_size: 0,
        ileave_info: None,
    })
}

#[allow(unused_variables)]
fn parse_aformat4(src: &mut ByteReader) -> DemuxerResult<RealAudioInfo> {
    let start = src.tell();
    src.read_skip(2)?; // zeroes
    let id                  = src.read_u32be()?;
    validate!(id == mktag!(b".ra4"));
    let data_size           = src.read_u32be()?;
    let _ver4               = src.read_u16be()?; // should be 4
    let header_size         = src.read_u32be()?;
    let flavor              = src.read_u16be()?;
    let granularity         = src.read_u32be()?;
    let total_bytes         = src.read_u32be()?;
    let bytes_per_minute    = src.read_u32be()?;
    let _bytes_per_minute2  = src.read_u32be()?;
    let ileave_factor       = src.read_u16be()?;
    let ileave_block_size   = src.read_u16be()?;
    let _user_data          = src.read_u16be()?;
    let sample_rate         = src.read_u32be()?;
    let sample_size         = src.read_u32be()?;
    let channels            = src.read_u16be()?;
    let interleaver_id_len  = src.read_byte()?;
    validate!(interleaver_id_len == 4);
    let interleaver_id      = src.read_u32be()?;
    let fcc_len             = src.read_byte()?;
    validate!(fcc_len == 4);
    let fcc                 = src.read_u32be()?;
    let is_interleaved      = src.read_byte()?;
    let _can_copy           = src.read_byte()?;
    let _stream_type        = src.read_byte()?;

    skip_ra_metadata(src)?;

    let end = src.tell();
    validate!(end - start <= (header_size as u64) + 10);

    let ileave_info = if is_interleaved != 0 {
            Some(InterleaveInfo {
                    id: interleaver_id, factor: ileave_factor, block_size: granularity as u16,
                    frame_size: ileave_block_size,
                })
        } else {
            None
        };

    Ok(RealAudioInfo {
        fcc, flavor,
        sample_rate, sample_size: sample_size as u16, channels, channel_mask: 0,
        granularity, bytes_per_minute,
        total_bytes: total_bytes & 0xFFFFFF, edata_size: 0,
        ileave_info,
    })
}

#[allow(unused_variables)]
fn parse_aformat5(src: &mut ByteReader) -> DemuxerResult<RealAudioInfo> {
    let start = src.tell();
    src.read_skip(2)?; // zeroes
    let id                  = src.read_u32be()?;
    validate!((id == mktag!(b".ra5")) || (id == mktag!(b".ra4")));
    let data_size           = src.read_u32be()?;
    let _ver5               = src.read_u16be()?; // should be 5
    let header_size         = src.read_u32be()?;
    let flavor              = src.read_u16be()?;
    let granularity         = src.read_u32be()?;
    let total_bytes         = src.read_u32be()?;
    let bytes_per_minute    = src.read_u32be()?;
    let _bytes_per_minute2  = src.read_u32be()?;
    let ileave_factor       = src.read_u16be()?;
    let frame_size          = src.read_u16be()?;
    let ileave_block_size   = src.read_u16be()?;
    let user_data           = src.read_u32be()?;
    let _sample_rate1       = src.read_u16be()?;
    let sample_rate         = src.read_u32be()?;
    let sample_size         = src.read_u32be()?;
    let channels            = src.read_u16be()?;
    let interleaver_id      = src.read_u32be()?;
    let fcc                 = src.read_u32be()?;
    let is_interleaved      = src.read_byte()?;
    let _can_copy           = src.read_byte()?;
    let _stream_type        = src.read_byte()?;
    let has_ileave_pattern  = src.read_byte()?;
    if has_ileave_pattern != 0 {
unimplemented!("ra5 interleave pattern");
    }
    let mut edata_size          = src.read_u32be()?;
    let end = src.tell();
    if id == mktag!(b".ra5") {
        validate!(end - start <= (header_size as u64) + 10);
//    src.read_skip(((end - start) as usize) - (header_size as usize) - 10)?;
    } else {
        validate!(end - start <= (header_size as u64) + 15);
    }

    let ileave_info = if is_interleaved != 0 {
            Some(InterleaveInfo {
                    id: interleaver_id, factor: ileave_factor, block_size: ileave_block_size, frame_size,
                })
        } else {
            None
        };
    if (fcc == mktag!(b"raac")) || (fcc == mktag!(b"racp")) {
        validate!(edata_size > 1);
        edata_size -= 1;
        src.read_skip(1)?;
    }

    Ok(RealAudioInfo {
        fcc, flavor,
        sample_rate, sample_size: sample_size as u16, channels, channel_mask: 0,
        granularity, bytes_per_minute,
        total_bytes: total_bytes & 0xFFFFFF, edata_size,
        ileave_info,
    })
}

const RMVB_HDR_SIZE:  u32 = 18;
const RMVB_PROP_SIZE: u32 = 50;
const KEYFRAME_FLAG: u8 = 0x02;

impl<'a> RealMediaDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        RealMediaDemuxer {
            src:            io,
            data_pos:       0,
            data_end:       0,
            next_data:      0,
            data_ver:       0,
            num_packets:    0,
            cur_packet:     0,
            str_data:       CommonStreamData::new(),
            data_chunks:    Vec::new(),
            cur_data_chunk: 0,
            queued_pkts:    Vec::new(),
            slice_buf:      Vec::new(),
        }
    }
#[allow(unused_variables)]
    fn read_header(&mut self, strmgr: &mut StreamManager, seek_idx: &mut SeekIndex) -> DemuxerResult<()> {
        let (id, size, ver) = read_chunk(self.src)?;
        validate!((id == mktag!(b".RMF")) || (id == mktag!(b".RMP")));
        validate!(size >= RMVB_HDR_SIZE);
        let fver    = self.src.read_u32be()?;
        validate!(fver <= 1);
        let num_hdr = self.src.read_u32be()? as usize;
        validate!(num_hdr >= 1);
        if size > RMVB_HDR_SIZE {
            self.src.read_skip((size - RMVB_HDR_SIZE) as usize)?;
        }

        let (id, size, ver) = read_chunk(self.src)?;
        let prop_size = if ver == 0 { RMVB_PROP_SIZE } else { RMVB_PROP_SIZE + 4 };
        validate!(size >= prop_size);
        validate!((ver == 0) || (ver == 2));
        let maxbr       = self.src.read_u32be()?;
        let avgbr       = self.src.read_u32be()?;
        let maxps       = self.src.read_u32be()?;
        let avgps       = self.src.read_u32be()?;
        let num_pkt     = self.src.read_u32be()? as usize;
        let duration    = self.src.read_u32be()?;
        let preroll     = self.src.read_u32be()?;
        let idx_off     = self.src.read_size(ver)?;
        let data_off    = self.src.read_u32be()?;
        let num_streams = self.src.read_u16be()? as usize;
        let flags       = self.src.read_u16be()?;
        if size > prop_size {
            self.src.read_skip((size - prop_size) as usize)?;
        }

        for _ in 0..num_hdr {
            if self.src.is_eof() {
                //warn maybe?
                break;
            }
            let res = self.parse_chunk(strmgr, seek_idx);
            match res {
                Ok(last) => { if last { break; } },
                Err(DemuxerError::IOError) => { break; },
                Err(etype) => {
                        if self.data_chunks.is_empty() { // data is not found, report error
                            return Err(etype);
                        }
                    },
            };
        }
        validate!(!self.data_chunks.is_empty());
        self.cur_data_chunk = 0;
        let (pos, size, ver) = self.data_chunks[self.cur_data_chunk];
        self.data_pos = pos;
        self.data_ver = ver;
        self.data_end = pos + (size as u64);
        self.src.seek(SeekFrom::Start(self.data_pos + 10))?;
        self.parse_data_start()?;
        Ok(())
    }
    fn parse_data_start(&mut self) -> DemuxerResult<()> {
        let num_packets     = self.src.read_u32be()?;
        if self.data_ver == 2 {
                              self.src.read_skip(12)?; // zeroes?
        }
        let next_data_hdr   = self.src.read_u32be()?;
        self.num_packets = if num_packets > 0 { num_packets } else { 0xFFFFFF };
        self.cur_packet  = 0;
        self.next_data   = next_data_hdr as u64;
        Ok(())
    }
    fn parse_chunk(&mut self, strmgr: &mut StreamManager, seek_idx: &mut SeekIndex) -> DemuxerResult<bool> {
        let (id, size, ver) = read_chunk(self.src)?;
        let end_pos = self.src.tell() - 10 + (size as u64);

        validate!((ver == 0) || (ver == 2));
             if id == mktag!(b"CONT") { self.parse_content_desc()?; }
        else if id == mktag!(b"MDPR") { self.parse_mdpr(strmgr)?; }
        else if id == mktag!(b"DATA") {
            self.data_chunks.push((self.src.tell() - 10, size, ver));
        }
        else if id == mktag!(b"INDX") {
            if !seek_idx.skip_index {
                self.parse_index(seek_idx, (size as usize) - 10, ver)?;
            }
        }
        else if id == 0               { return Ok(true); }
        else                          { println!("unknown chunk type {:08X}", id); }

        let cpos = self.src.tell();
        if cpos < end_pos {
            self.src.read_skip((end_pos - cpos) as usize)?;
        }
        Ok(false)
    }
#[allow(unused_variables)]
    fn parse_content_desc(&mut self) -> DemuxerResult<()> {
        let title_len       = self.src.read_u16be()? as usize;
        self.src.read_skip(title_len)?;
        let author_len      = self.src.read_u16be()? as usize;
        self.src.read_skip(author_len)?;
        let copywrong_len   = self.src.read_u16be()? as usize;
        self.src.read_skip(copywrong_len)?;
        let comment_len     = self.src.read_u16be()? as usize;
        self.src.read_skip(comment_len)?;
        Ok(())
    }
#[allow(unused_variables)]
    fn parse_mdpr(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<()> {
        let stream_no       = self.src.read_u16be()? as u32;
//todo check stream_no for duplicates
        let maxbr           = self.src.read_u32be()?;
        let avgbr           = self.src.read_u32be()?;
        let maxps           = self.src.read_u32be()?;
        let avgps           = self.src.read_u32be()?;
        let start           = self.src.read_u32be()?;
        let preroll         = self.src.read_u32be()?;
        let duration        = self.src.read_u32be()?;
        let sname_size      = self.src.read_byte()? as usize;
        let sname           = read_string_size(self.src, sname_size)?;
        let mime_size       = self.src.read_byte()? as usize;
        let mime            = read_string_size(self.src, mime_size)?;
        let edata_size      = self.src.read_u32be()? as usize;
        let edata: Option<Vec<u8>> = if edata_size == 0 { None } else {
            let mut edvec: Vec<u8> = vec![0; edata_size];
            self.src.read_buf(&mut edvec)?;
            Some(edvec)
        };
        let mut is_mlti = false;
        if edata_size > 8 {
            if let Some(edata_) = edata {
                is_mlti = RMDemuxCommon::parse_stream_info(&mut self.str_data, strmgr, stream_no, &edata_, duration)?;
            }
        } else {
            self.str_data.streams.push(RMStreamType::Unknown);
        }
        if !is_mlti {
            self.str_data.str_ids.push(stream_no);
        }

        Ok(())
    }
    fn parse_index(&mut self, seek_idx: &mut SeekIndex, chunk_size: usize, ver: u16) -> DemuxerResult<()> {
        if ver != 0 && ver != 2 { return Ok(()); }
        let num_entries     = self.src.read_u32be()? as usize;
        let str_id          = self.src.read_u16be()? as u32;
        let _next_idx       = self.src.read_size(ver)?;
        if ver == 0 {
            validate!(chunk_size == num_entries * 14 + 10);
        } else {
            validate!(chunk_size == num_entries * 18 + 14);
        }
        if num_entries == 0 { return Ok(()); }

        seek_idx.add_stream(str_id);
        let idx = seek_idx.get_stream_index(str_id).unwrap();
        for _ in 0..num_entries {
            let iver        = self.src.read_u16be()?;
            validate!(iver == ver);
            let ts          = self.src.read_u32be()? as u64;
            let pos         = self.src.read_size(ver)?;
            let _pkt_no     = self.src.read_u32be()?;
            idx.add_entry(SeekEntry { time: ts, pts: 0, pos });
        }
        idx.filled = true;
        seek_idx.mode = SeekIndexMode::Present;
        Ok(())
    }
}

fn read_string_size(src: &mut ByteReader, size: usize) -> DemuxerResult<String> {
    let mut vec: Vec<u8> = Vec::new();
    for _ in 0..size {
        let c = src.read_byte()?;
        vec.push(c);
    }
    if let Ok(res) = String::from_utf8(vec) {
        Ok(res)
    } else {
        Ok(String::new())
    }
}

struct RealAudioDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    stream:         Option<RMAudioStream>,
    data_start:     u64,
    data_end:       u64,
    blk_size:       usize,
    queued_pkts:    Vec<NAPacket>,
}

impl<'a> DemuxCore<'a> for RealAudioDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, _seek_idx: &mut SeekIndex) -> DemuxerResult<()> {
        let magic                                       = self.src.read_u32be()?;
        validate!(magic == mktag!(b".ra\xFD"));
        let ver         = self.src.read_u16be()?;
        let ainfo = match ver {
            3 => {
                    parse_aformat3(self.src)?
                },
            4 => {
                    parse_aformat4(self.src)?
                },
            5 => {
                    parse_aformat5(self.src)?
                },
            _ => {
                    println!("unknown version {}", ver);
                    return Err(DemuxerError::InvalidData);
                },
        };
        let cname = find_codec_name(RM_AUDIO_CODEC_REGISTER, ainfo.fcc);
        let blk_size = if ainfo.fcc != mktag!(b"sipr") {
                ainfo.granularity as usize
            } else {
                validate!(ainfo.flavor <= 3);
                RM_SIPRO_BLOCK_SIZES[ainfo.flavor as usize]
            };
        let srate = ainfo.sample_rate;
        let soniton = NASoniton::new(ainfo.sample_size as u8, SONITON_FLAG_SIGNED);
        let ahdr = NAAudioInfo::new(srate, ainfo.channels as u8, soniton, blk_size);
        let extradata = if ainfo.edata_size == 0 {
                None
            } else {
                let mut dta: Vec<u8> = vec![0; ainfo.edata_size as usize];
                self.src.read_buf(dta.as_mut_slice())?;
                Some(dta)
            };
        let nainfo = NACodecInfo::new(cname, NACodecTypeInfo::Audio(ahdr), extradata);
        let res = strmgr.add_stream(NAStream::new(StreamType::Audio, 0, nainfo, 1, srate, ainfo.get_duration(ainfo.sample_rate)));
        if res.is_none() { return Err(MemoryError); }

        let astr = RMAudioStream::new(ainfo.ileave_info);
        self.data_start = self.src.tell();
        self.data_end   = if ainfo.total_bytes > 0 { self.src.tell() + (ainfo.total_bytes as u64) } else { 0 };
        self.blk_size = blk_size;
        self.stream = Some(astr);

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if !self.queued_pkts.is_empty() {
            let mut pkt = self.queued_pkts.pop().unwrap();
            pkt.ts.pts = None;
            return Ok(pkt);
        }
        if (self.data_end != 0) && (self.src.tell() >= self.data_end) {
            return Err(DemuxerError::EOF);
        }
        let streamres = strmgr.get_stream_by_id(0);
        let stream = streamres.unwrap();
        if let Some(ref mut astr) = self.stream {
            loop {
                let mut ret = astr.read_apackets(&mut self.queued_pkts, self.src, stream.clone(), 0, false, self.blk_size);
                if let Err(DemuxerError::TryAgain) = ret {
                    continue;
                }
                if let Ok(ref mut pkt) = ret {
                    pkt.ts.pts = None;
                }
                return ret;
            }
        }
        Err(DemuxerError::NoSuchInput)
    }

    #[allow(unused_variables)]
    fn seek(&mut self, time: NATimePoint, seek_idx: &SeekIndex) -> DemuxerResult<()> {
        Err(NotImplemented)
    }

    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for RealAudioDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> RealAudioDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        RealAudioDemuxer {
            src:            io,
            data_start:     0,
            data_end:       0,
            blk_size:       0,
            stream:         None,
            queued_pkts:    Vec::new(),
        }
    }
}

enum IVRRecord {
    Invalid(u8),
    StreamProperties(usize),
    Packet { ts: u32, strm: u32, flags: u32, len: usize, checksum: u32 },
    IntValue(Vec<u8>, u32),
    BinaryData(Vec<u8>, Vec<u8>),
    StringData(Vec<u8>, Vec<u8>),
    HeaderEnd,
    DataStart,
    DataEnd,
}

impl IVRRecord {
    fn read_string(src: &mut ByteReader) -> DemuxerResult<Vec<u8>> {
        let len                     = src.read_u32be()? as usize;
        let mut val = vec![0; len];
        src.read_buf(val.as_mut_slice())?;
        Ok(val)
    }

    fn read(src: &mut ByteReader) -> DemuxerResult<Self> {
        let code = src.read_byte()?;
        match code {
            1 => {
                    let val     = src.read_u32be()? as usize;
                    Ok(IVRRecord::StreamProperties(val))
                },
            2 => {
                    let ts      = src.read_u32be()?;
                    let strm    = src.read_u16be()? as u32;
                    let flags   = src.read_u32be()?;
                    let len     = src.read_u32be()? as usize;
                    let chk     = src.read_u32be()?;
                    validate!((len > 0) && (len < (1 << 24)));
                    Ok(IVRRecord::Packet { ts, strm, flags, len, checksum: chk })
                },
            3 => {
                    let name = Self::read_string(src)?;
                    let len  = src.read_u32be()?;
                    validate!(len == 4);
                    let val  = src.read_u32be()?;
                    Ok(IVRRecord::IntValue(name, val))
                },
            4 => {
                    let name = Self::read_string(src)?;
                    let len  = src.read_u32be()? as usize;
                    let mut val = vec![0; len];
                    src.read_buf(val.as_mut_slice())?;
                    Ok(IVRRecord::BinaryData(name, val))
                },
            5 => {
                    let name = Self::read_string(src)?;
                    let val  = Self::read_string(src)?;
                    Ok(IVRRecord::StringData(name, val))
                },
            6 => Ok(IVRRecord::HeaderEnd),
            7 => {
                    src.read_skip(8)?; // always zero?
                    Ok(IVRRecord::DataEnd)
                },
            8 => {
                    src.read_skip(8)?; // always zero?
                    Ok(IVRRecord::DataStart)
                },
            _ => Ok(IVRRecord::Invalid(code)),
        }
    }
    fn is_data_start(&self) -> bool {
        matches!(*self, IVRRecord::DataStart)
    }
}

impl fmt::Display for IVRRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IVRRecord::Invalid(typ) => write!(f, "Invalid({:02X})", typ),
            IVRRecord::StreamProperties(num) =>
                write!(f, "({} stream properties)", num),
            IVRRecord::Packet { ts, strm, flags, len, checksum } =>
                write!(f, "paket({}, {}, {:X}, {}, {})", ts, strm, flags, len, checksum),
            IVRRecord::IntValue(ref name, val) =>
                write!(f, "({} = {})", String::from_utf8_lossy(name), val),
            IVRRecord::BinaryData(ref name, ref val) =>
                write!(f, "({} = {} bytes)", String::from_utf8_lossy(name), val.len()),
            IVRRecord::StringData(ref name, ref val) =>
                write!(f, "({} = {})", String::from_utf8_lossy(name), String::from_utf8_lossy(val)),
            IVRRecord::HeaderEnd => write!(f, "header end"),
            IVRRecord::DataEnd   => write!(f, "data end"),
            IVRRecord::DataStart => write!(f, "data start"),
        }
    }
}

struct RecordDemuxer {
    start_pos:      u64,
    cur_pos:        u64,
    start_str:      u32,
    remap_ids:      Vec<u32>,
}

impl RecordDemuxer {
    fn new(pos: u64, start_str: u32) -> Self {
        RecordDemuxer {
            start_pos:      pos,
            cur_pos:        pos,
            start_str,
            remap_ids:      Vec::new(),
        }
    }
    fn parse_header(&mut self, src: &mut ByteReader, strmgr: &mut StreamManager, str_data: &mut CommonStreamData) -> DemuxerResult<()> {
        src.seek(SeekFrom::Start(self.cur_pos))?;
        let magic           = src.read_u32be()?;
        validate!(magic == mktag!(b".REC"));
        let _smth           = src.read_byte()?;
        let num_entries     = src.read_u32be()? as usize;
        for _ in 0..num_entries {
            let _rec = IVRRecord::read(src)?;
        }
        let mut has_seek_table = false;
        let mut cur_str_no = 0;
        loop {
            let rec = IVRRecord::read(src)?;
            match rec {
                IVRRecord::HeaderEnd => { break; },
                IVRRecord::StreamProperties(num) => {
                        let stream_no = cur_str_no + self.start_str;
                        cur_str_no += 1;
                        let mut parsed = false;
                        let mut real_stream_no = 0;
                        let mut duration = 0;
                        for _ in 0..num {
                            let rec = IVRRecord::read(src)?;
                            match rec {
                                IVRRecord::IntValue(ref name, val)   => {
                                        if name == b"StreamNumber\0" {
                                            real_stream_no = val;
                                        }
                                        if name == b"Duration\0" {
                                            duration = val;
                                        }
                                    },
                                IVRRecord::BinaryData(ref name, ref val)   => {
                                        if name == b"OpaqueData\0" {
                                            validate!(!parsed);
                                            let is_mlti = RMDemuxCommon::parse_stream_info(str_data, strmgr, stream_no, val, duration)?;
                                            if !is_mlti {
                                                str_data.str_ids.push(stream_no);
                                            }
                                            parsed = true;
                                        }
                                    },
                                IVRRecord::StringData(ref name, ref val)   => {
                                        if (name == b"SeekType\0") && (val != b"None\0") {
                                            has_seek_table = true;
                                        }
                                    },
                                _ => { return Err(DemuxerError::InvalidData); }
                            };
                        }
                        if !parsed {
                            str_data.streams.push(RMStreamType::Unknown);
                            str_data.str_ids.push(stream_no);
                        }
                        self.remap_ids.push(real_stream_no);
                    },
                _ => {println!(" unexpected {}", rec); return Err(DemuxerError::InvalidData); }
            };
        }
        let off0        = src.read_u32be()? as u64;
        let _off1       = src.read_u32be()?;
        let _off2       = src.read_u32be()?;
        validate!(off0 + self.start_pos == src.tell());
        if has_seek_table {
            src.read_skip(4)?;
            let data_off    = src.read_u32be()? as u64;
            let pos = src.tell();
            validate!(data_off + self.start_pos > pos);
            src.read_skip((data_off + self.start_pos - pos) as usize)?;
            let rec = IVRRecord::read(src)?;
            validate!(rec.is_data_start());
        } else {
            let ntype = src.peek_byte()?;
            validate!((ntype == 2) || (ntype == 7)); // packet or data end, no start
        }

        self.cur_pos = src.tell();

        Ok(())
    }
    fn get_packet(&mut self, src: &mut ByteReader, str_data: &mut CommonStreamData, strmgr: &StreamManager, queued_pkts: &mut Vec<NAPacket>, slice_buf: &mut Vec<u8>) -> DemuxerResult<NAPacket> {
        src.seek(SeekFrom::Start(self.cur_pos))?;
        loop {
            let rec = IVRRecord::read(src)?;
            match rec {
                IVRRecord::Packet { ts, strm, flags, len, .. } => {
                        let payload_size = len;
                        let sr = self.remap_ids.iter().position(|x| *x == strm);
                        validate!(sr.is_some());
                        let str_no = self.start_str + (sr.unwrap() as u32);
                        let pkt_grp = ((flags >> 8) & 0xFF) as u16;
                        let stream_id = str_data.get_stream_id(str_no, pkt_grp);
                        let sr = str_data.find_stream(stream_id);
                        if sr.is_none() {
                            src.read_skip(payload_size)?;
                            return Err(DemuxerError::InvalidData);
                        }
                        let str_id = sr.unwrap();

                        let streamres = strmgr.get_stream_by_id(stream_id);
                        if streamres.is_none() {
                            src.read_skip(payload_size)?;
                            continue;
                        }
                        let stream = streamres.unwrap();
                        if strmgr.is_ignored_id(stream_id) {
                            src.read_skip(payload_size)?;
                            continue;
                        }
                        let keyframe = false;
                        let ret = RMDemuxCommon::parse_packet_payload(src, &mut str_data.streams[str_id], stream, slice_buf, queued_pkts, keyframe, ts, payload_size);
                        if let Err(DemuxerError::TryAgain) = ret {
                            continue;
                        } else {
                            self.cur_pos = src.tell();
                            return ret;
                        }
                    },
                IVRRecord::DataEnd      => return Err(DemuxerError::EOF),
                _                       => return Err(DemuxerError::InvalidData),
            }
        }
    }
}

struct RealIVRDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    recs:           Vec<RecordDemuxer>,
    cur_rec:        usize,
    queued_pkts:    Vec<NAPacket>,
    slice_buf:      Vec<u8>,
    str_data:       CommonStreamData,
}

impl<'a> DemuxCore<'a> for RealIVRDemuxer<'a> {
    #[allow(unused_variables)]
    fn open(&mut self, strmgr: &mut StreamManager, seek_idx: &mut SeekIndex) -> DemuxerResult<()> {
        let magic                                       = self.src.peek_u32be()?;
        if magic == mktag!(b".REC") {
            let mut rec = RecordDemuxer::new(0, 0);
            rec.parse_header(self.src, strmgr, &mut self.str_data)?;
            self.recs.push(rec);
        } else if magic == mktag!(b".R1M") {
            self.src.read_skip(4)?; // magic
            self.src.read_skip(3)?; // always 0, 1, 1 ?
            let _name = IVRRecord::read_string(self.src)?;
            self.src.read_skip(1)?; // always 0?
            let len1 = self.src.read_u32be()? as u64;
            let off1 = self.src.read_u64be()?;
            let cpos = self.src.tell();
            validate!(off1 == len1 + cpos - 8);
            self.src.read_skip((off1 - cpos) as usize)?;
            loop {
                let typ = self.src.read_byte()?;
                match typ {
                    1 => {
                            let len = self.src.read_u32be()?;
                            self.src.read_skip(len as usize)?;
                        },
                    2 => {
                            let len = self.src.read_u32be()? as u64;
                            let pos = self.src.tell();
                            if len > 0x20 {
                                let num_streams = self.str_data.streams.len() as u32;
                                let mut rec = RecordDemuxer::new(pos + 12, num_streams);
                                rec.parse_header(self.src, strmgr, &mut self.str_data)?;
                                self.recs.push(rec);
                            }
                            self.src.seek(SeekFrom::Start(pos + len))?;
                        },
                    b'R' => {
                            let mut buf: [u8; 2] = [0; 2];
                            self.src.peek_buf(&mut buf)?;
                            if (buf[0] == b'J') && (buf[1] == b'M') { // RJMx markers at the end of file
                                break;
                            } else {
                                return Err(DemuxerError::InvalidData);
                            }
                        },
                    _ => { return Err(DemuxerError::InvalidData); },
                };
            }
        } else {
            return Err(DemuxerError::InvalidData);
        }

        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if !self.queued_pkts.is_empty() {
            let pkt = self.queued_pkts.pop().unwrap();
            return Ok(pkt);
        }
        loop {
            if self.cur_rec >= self.recs.len() { return Err(DemuxerError::EOF); }
            let res = self.recs[self.cur_rec].get_packet(self.src, &mut self.str_data, strmgr, &mut self.queued_pkts, &mut self.slice_buf);
            if let Err(DemuxerError::EOF) = res {
                self.cur_rec += 1;
            } else {
                return res;
            }
        }
    }

    #[allow(unused_variables)]
    fn seek(&mut self, time: NATimePoint, seek_idx: &SeekIndex) -> DemuxerResult<()> {
        Err(NotImplemented)
    }

    fn get_duration(&self) -> u64 { 0 }
}

impl<'a> NAOptionHandler for RealIVRDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

impl<'a> RealIVRDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        RealIVRDemuxer {
            src:            io,
            recs:           Vec::new(),
            cur_rec:        0,
            queued_pkts:    Vec::new(),
            slice_buf:      Vec::new(),
            str_data:       CommonStreamData::new(),
        }
    }
}

static RM_VIDEO_CODEC_REGISTER: &[(&[u8;4], &str)] = &[
    (b"RV10", "realvideo1"),
    (b"RV20", "realvideo2"),
    (b"RVTR", "realvideo2"),
    (b"RV30", "realvideo3"),
    (b"RV40", "realvideo4"),
    (b"RV60", "realvideo6"),
    (b"CLV1", "clearvideo_rm"),
];

static RM_AUDIO_CODEC_REGISTER: &[(&[u8;4], &str)] = &[
    (b"lpcJ", "ra14.4"),
    (b"28_8", "ra28.8"),
    (b"cook", "cook"),
    (b"dnet", "ac3"),
    (b"sipr", "sipro"),
    (b"atrc", "atrac3"),
    (b"LSD:", "ralf"),
    (b"raac", "aac"),
    (b"racp", "aac"),
];

pub struct RealMediaDemuxerCreator { }

impl DemuxerCreator for RealMediaDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(RealMediaDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "realmedia" }
}

pub struct RealAudioDemuxerCreator { }

impl DemuxerCreator for RealAudioDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(RealAudioDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "realaudio" }
}

pub struct RealIVRDemuxerCreator { }

impl DemuxerCreator for RealIVRDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(RealIVRDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "real_ivr" }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_rm_demux() {
        // sample from a private collection
        let mut file =
            File::open("assets/RV/rv10_dnet_640x352_realvideo_encoder_4.0.rm").unwrap();
//            File::open("assets/RV/rv20_cook_640x352_realproducer_plus_8.51.rm").unwrap();
//            File::open("assets/RV/rv20_svt_atrc_640x352_realproducer_plus_8.51.rm").unwrap();
//            File::open("assets/RV/rv30_atrc_384x208_realproducer_plus_8.51.rm").unwrap();
//            File::open("assets/RV/rv30_chroma_drift.rm").unwrap();
//            File::open("assets/RV/rv30_weighted_mc.rm").unwrap();
//            File::open("assets/RV/rv40_weighted_mc.rmvb").unwrap();
//            File::open("assets/RV/rv40_weighted_mc_2.rmvb").unwrap();
//            File::open("assets/RV/clv1_sipr_384x208_realvideo_encoder_4.0.rm").unwrap();
//            File::open("assets/RV/luckynight.rmvb").unwrap();
//            File::open("assets/RV/rv40_ralf.rmvb").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = RealMediaDemuxer::new(&mut br);
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
//panic!("the end");
    }
    #[test]
    fn test_ra_demux() {
        // sample: https://samples.mplayerhq.hu/real//RA/ra_with_comment_field/diemusik.ra
        let mut file =
//            File::open("assets/RV/welcome288.ra").unwrap();
            File::open("assets/RV/diemusik.ra").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = RealAudioDemuxer::new(&mut br);
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
    #[test]
    fn test_ivr_demux() {
        // sample: https://samples.mplayerhq.hu/real/ivr/Opener_rm_hi.ivr
        let mut file =
            File::open("assets/RV/Opener_rm_hi.ivr").unwrap();
//            File::open("assets/RV/SherwinWilliamsCommercial.ivr").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = RealIVRDemuxer::new(&mut br);
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
