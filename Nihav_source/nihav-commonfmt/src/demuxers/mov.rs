use nihav_core::demuxers::*;
use nihav_registry::register::*;
use nihav_core::compr::deflate::*;

macro_rules! mktag {
    ($a:expr, $b:expr, $c:expr, $d:expr) => ({
        (($a as u32) << 24) | (($b as u32) << 16) | (($c as u32) << 8) | ($d as u32)
    });
    ($arr:expr) => ({
        (($arr[0] as u32) << 24) | (($arr[1] as u32) << 16) | (($arr[2] as u32) << 8) | ($arr[3] as u32)
    });
}

trait Skip64 {
    fn skip64(&mut self, size: u64) -> ByteIOResult<()>;
}

impl<'a> Skip64 for ByteReader<'a> {
    fn skip64(&mut self, size: u64) -> ByteIOResult<()> {
        if (size as usize as u64) != size {
            self.seek(SeekFrom::Current(size as i64))?;
        } else {
            self.read_skip(size as usize)?;
        }
        Ok(())
    }
}

fn read_chunk_header(br: &mut ByteReader) -> DemuxerResult<(u32, u64)> {
    let size            = br.read_u32be()?;
    let ctype           = br.read_u32be()?;
    if size == 0 {
        Ok((ctype, br.left() as u64))
    } else if size == 1 {
        let size64      = br.read_u64be()?;
        validate!(size64 >= 16);
        Ok((ctype, size64 - 16))
    } else {
        validate!(size >= 8);
        Ok((ctype, (size as u64) - 8))
    }
}

fn read_palette(br: &mut ByteReader, size: u64, pal: &mut [u8; 1024]) -> DemuxerResult<u64> {
    let _seed           = br.read_u32be()?;
    let flags           = br.read_u16be()?;
    let palsize         = (br.read_u16be()? as usize) + 1;
    validate!(palsize <= 256);
    validate!(flags == 0 || flags == 0x4000 || flags == 0x8000);
    validate!((palsize as u64) * 8 + 8 <= size);
    for i in 0..palsize {
        let a           = br.read_u16be()?;
        let r           = br.read_u16be()?;
        let g           = br.read_u16be()?;
        let b           = br.read_u16be()?;
        pal[i * 4]     = (r >> 8) as u8;
        pal[i * 4 + 1] = (g >> 8) as u8;
        pal[i * 4 + 2] = (b >> 8) as u8;
        if flags == 0x8000 {
            pal[i * 4 + 3] = (a >> 8) as u8;
        }
    }
    if flags == 0x4000 {
        br.read_skip(8)?;
    }
    Ok(size)
}

struct RootChunkHandler {
    ctype:  u32,
    parse:  fn(dmx: &mut MOVDemuxer, strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64>,
}

struct TrackChunkHandler {
    ctype:  u32,
    parse:  fn(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64>,
}

const IGNORED_CHUNKS: &[u32] = &[
    mktag!(b"free"), mktag!(b"skip"), mktag!(b"udta"), mktag!(b"wide")
];

const ROOT_CHUNK_HANDLERS: &[RootChunkHandler] = &[
    RootChunkHandler { ctype: mktag!(b"ftyp"), parse: read_ftyp },
    RootChunkHandler { ctype: mktag!(b"mdat"), parse: read_mdat },
    RootChunkHandler { ctype: mktag!(b"moov"), parse: read_moov },
    RootChunkHandler { ctype: mktag!(b"moof"), parse: read_moof },
    RootChunkHandler { ctype: mktag!(b"sidx"), parse: read_sidx },
];

fn print_cname(ctype: u32, size: u64, off: u64, depth: u8) {
    for _ in 0..depth { print!("    "); }
    let tag = [(ctype >> 24) as u8, (ctype >> 16) as u8, (ctype >> 8) as u8, ctype as u8];
    let mut printable = true;
    for ch in tag.iter() {
        if !(0x20..=0x7F).contains(ch) {
            printable = false;
            break;
        }
    }
    if printable {
        print!(" '{}{}{}{}'", tag[0] as char, tag[1] as char, tag[2] as char, tag[3] as char);
    } else {
        print!(" {:08X}", ctype);
    }
    println!(" size {} @ {:X}", size, off);
}

macro_rules! read_chunk_list {
    (root; $name: expr, $fname: ident, $handlers: ident) => {
        fn $fname(&mut self, strmgr: &mut StreamManager, size: u64) -> DemuxerResult<()> {
            self.depth += 1;
            validate!(self.depth < 32);
            let list_end = self.src.tell() + size;
            while self.src.tell() < list_end {
                let ret = read_chunk_header(self.src);
                if ret.is_err() { break; }
                let (ctype, size) = ret.unwrap();
                if self.print_chunks {
                    print_cname(ctype, size, self.src.tell(), self.depth as u8);
                }
                if self.src.tell() + size > list_end {
                    break;
                }
                if IGNORED_CHUNKS.contains(&ctype) {
                    self.src.skip64(size)?;
                    continue;
                }
                let handler = $handlers.iter().find(|x| x.ctype == ctype);
                let read_size;
                if let Some(ref handler) = handler {
                    read_size = (handler.parse)(self, strmgr, size)?;
                } else {
                    println!("skipping unknown chunk {:08X} size {}", ctype, size);
                    read_size = 0;
                }
                validate!(read_size <= size);
                self.src.skip64(size - read_size)?;
            }
            self.depth -= 1;
            validate!(self.src.tell() == list_end);
            Ok(())
        }
    };
    (track; $name: expr, $fname: ident, $handlers: ident) => {
        fn $fname(&mut self, br: &mut ByteReader, size: u64) -> DemuxerResult<()> {
            self.depth += 1;
            validate!(self.depth < 32);
            let list_end = br.tell() + size;
            while br.tell() < list_end {
                let ret = read_chunk_header(br);
                if ret.is_err() { break; }
                let (ctype, size) = ret.unwrap();
                if self.print_chunks {
                    print_cname(ctype, size, br.tell(), self.depth + 1);
                }
                if br.tell() + size > list_end {
                    break;
                }
                if IGNORED_CHUNKS.contains(&ctype) {
                    br.skip64(size)?;
                    continue;
                }
                let handler = $handlers.iter().find(|x| x.ctype == ctype);
                let read_size;
                if let Some(ref handler) = handler {
                    read_size = (handler.parse)(self, br, size)?;
                } else {
                    read_size = 0;
                }
                validate!(read_size <= size);
                br.skip64(size - read_size)?;
            }
            self.depth -= 1;
            validate!(br.tell() == list_end);
            Ok(())
        }
    }
}

fn skip_chunk(_track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    br.skip64(size)?;
    Ok(size)
}

fn read_ftyp(dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    dmx.src.skip64(size)?;
    Ok(size)
}

fn read_mdat(dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    dmx.mdat_pos  = dmx.src.tell();
    dmx.mdat_size = size;
    dmx.src.skip64(size)?;
    Ok(size)
}

fn read_sidx(_dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, _size: u64) -> DemuxerResult<u64> {
    Ok(0)
}

fn read_moov(dmx: &mut MOVDemuxer, strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    dmx.read_moov(strmgr, size)?;
    Ok(size)
}

const MOOV_CHUNK_HANDLERS: &[RootChunkHandler] = &[
    RootChunkHandler { ctype: mktag!(b"mvhd"), parse: read_mvhd },
    RootChunkHandler { ctype: mktag!(b"cmov"), parse: read_cmov },
    RootChunkHandler { ctype: mktag!(b"ctab"), parse: read_ctab },
    RootChunkHandler { ctype: mktag!(b"trak"), parse: read_trak },
    RootChunkHandler { ctype: mktag!(b"meta"), parse: read_meta },
    RootChunkHandler { ctype: mktag!(b"mvex"), parse: read_mvex },
    RootChunkHandler { ctype: mktag!(b"iods"), parse: skip_chunk_mov },
];

fn read_mvhd(dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    const KNOWN_MVHD_SIZE: u64 = 100;
    let br = &mut dmx.src;
    validate!(size >= KNOWN_MVHD_SIZE);
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
    let _ctime              = br.read_u32be()?;
    let _mtime              = br.read_u32be()?;
    let tscale              = br.read_u32be()?;
    let duration            = br.read_u32be()?;
    let _pref_rate          = br.read_u32be()?;
    let _pref_volume        = br.read_u16be()?;
                              br.read_skip(10)?;
                              br.read_skip(36)?; // matrix
    let _preview_time       = br.read_u32be()?;
    let _preview_duration   = br.read_u32be()?;
    let _poster_time        = br.read_u32be()?;
    let _sel_time           = br.read_u32be()?;
    let _sel_duration       = br.read_u32be()?;
    let _cur_time           = br.read_u32be()?;
    let _next_track_id      = br.read_u32be()?;
    dmx.duration = duration;
    dmx.tb_den = tscale;

    Ok(KNOWN_MVHD_SIZE)
}

fn read_cmov(dmx: &mut MOVDemuxer, strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    let br = &mut dmx.src;
    validate!(size > 24);
    let dcom_size           = br.read_u32be()?;
    let dcom_tag            = br.read_tag()?;
    let compr_type          = br.read_tag()?;
    validate!(&dcom_tag == b"dcom" && dcom_size == 12);
    if &compr_type != b"zlib" {
        return Err(DemuxerError::NotImplemented);
    }
    let cmvd_size           = u64::from(br.read_u32be()?);
    let cmvd_tag            = br.read_tag()?;
    validate!(&cmvd_tag == b"cmvd" && cmvd_size > 14 && cmvd_size == size - 12);
    let comp_size = (cmvd_size - 12) as usize;
    let uncomp_size         = br.read_u32be()? as usize;
    validate!(uncomp_size > 8);
    let mut sbuf = vec![0; comp_size];
    let mut dbuf = vec![0; uncomp_size];
                              br.read_buf(sbuf.as_mut_slice())?;
    let ret = Inflate::uncompress(sbuf.as_slice(), dbuf.as_mut_slice());
    if ret.is_err() {
        return Err(DemuxerError::InvalidData);
    }
    let len = ret.unwrap();
    validate!(len == uncomp_size);
    let mut mr = MemoryReader::new_read(dbuf.as_slice());
    let mut br = ByteReader::new(&mut mr);
    let (ctype, csize) = read_chunk_header(&mut br)?;
    validate!(ctype == mktag!(b"moov"));
    let mut ddmx = MOVDemuxer::new(&mut br);
    ddmx.print_chunks = dmx.print_chunks;
    ddmx.read_moov(strmgr, csize)?;
    std::mem::swap(&mut dmx.tracks, &mut ddmx.tracks);
    dmx.duration = ddmx.duration;
    dmx.tb_den = ddmx.tb_den;
    std::mem::swap(&mut dmx.pal, &mut ddmx.pal);

    Ok(size)
}

fn read_ctab(dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    let mut pal = [0; 1024];
    let size = read_palette(dmx.src, size, &mut pal)?;
    dmx.pal = Some(Arc::new(pal));
    Ok(size)
}

fn read_meta(dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    dmx.src.skip64(size)?;
    Ok(size)
}

fn read_mvex(_dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, _size: u64) -> DemuxerResult<u64> {
    Ok(0)
}

fn skip_chunk_mov(_dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, _size: u64) -> DemuxerResult<u64> {
    Ok(0)
}

fn read_trak(dmx: &mut MOVDemuxer, strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    let mut track = Track::new(dmx.cur_track as u32, dmx.tb_den);
    track.print_chunks = dmx.print_chunks;
    track.read_trak(dmx.src, size)?;
    validate!(track.tkhd_found && track.stsd_found);
    validate!(strmgr.get_stream_by_id(track.track_id).is_none());
    dmx.cur_track += 1;
    dmx.tracks.push(track);
    Ok(size)
}

fn read_moof(dmx: &mut MOVDemuxer, strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    dmx.moof_off = dmx.src.tell() - 8;
    dmx.read_moof(strmgr, size)?;
    Ok(size)
}

const MOOF_CHUNK_HANDLERS: &[RootChunkHandler] = &[
    RootChunkHandler { ctype: mktag!(b"mfhd"), parse: read_mfhd },
    RootChunkHandler { ctype: mktag!(b"traf"), parse: read_traf },
    RootChunkHandler { ctype: mktag!(b"meta"), parse: read_meta },
];

fn read_mfhd(dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    const KNOWN_MFHD_SIZE: u64 = 8;
    validate!(size >= KNOWN_MFHD_SIZE);
    let version             = dmx.src.read_byte()?;
    validate!(version == 0);
    let flags               = dmx.src.read_u24be()?;
    validate!(flags == 0);
    let _seq_no             = dmx.src.read_u32be()?;

    Ok(KNOWN_MFHD_SIZE)
}

fn read_traf(dmx: &mut MOVDemuxer, _strmgr: &mut StreamManager, size: u64) -> DemuxerResult<u64> {
    let mut buf = [0u8; 16];
                              dmx.src.peek_buf(&mut buf)?;
    validate!(&buf[4..8] == b"tfhd");
    let track_id = read_u32be(&buf[12..16])?;
    let mut track = None;
    for trk in dmx.tracks.iter_mut() {
        if trk.track_id == track_id {
            track = Some(trk);
            break;
        }
    }
    if let Some(track) = track {
        track.moof_off = dmx.moof_off;
        track.read_traf(dmx.src, size)?;
        Ok(size)
    } else {
        Ok(0)
    }
}

const TRAK_CHUNK_HANDLERS: &[TrackChunkHandler] = &[
    TrackChunkHandler { ctype: mktag!(b"clip"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"matt"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"edts"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"tref"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"load"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"imap"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"tkhd"), parse: read_tkhd },
    TrackChunkHandler { ctype: mktag!(b"mdia"), parse: read_mdia },
];

fn read_tkhd(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    const KNOWN_TKHD_SIZE: u64 = 84;
    validate!(size >= KNOWN_TKHD_SIZE);
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
    let _ctime              = br.read_u32be()?;
    let _mtime              = br.read_u32be()?;
    let track_id            = br.read_u32be()?;
                              br.read_skip(4)?;
    let duration            = br.read_u32be()?;
                              br.read_skip(8)?;
    let _layer              = br.read_u16be()?;
    let _alt_group          = br.read_u16be()?;
    let _volume             = br.read_u16be()?;
                              br.read_skip(2)?;
                              br.read_skip(36)?; // matrix
    let width               = br.read_u32be()? as usize;
    let height              = br.read_u32be()? as usize;
    track.width  = width  >> 16;
    track.height = height >> 16;
    track.track_id = track_id;
    track.duration = duration;

    track.tkhd_found = true;
    Ok(KNOWN_TKHD_SIZE)
}

fn read_mdia(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    track.stream_type = StreamType::None;
    track.read_mdia(br, size)?;
    Ok(size)
}

const MDIA_CHUNK_HANDLERS: &[TrackChunkHandler] = &[
    TrackChunkHandler { ctype: mktag!(b"mdhd"), parse: read_mdhd },
    TrackChunkHandler { ctype: mktag!(b"hdlr"), parse: read_hdlr },
    TrackChunkHandler { ctype: mktag!(b"minf"), parse: read_minf },
];

fn read_mdhd(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    const KNOWN_MDHD_SIZE: u64 = 24;
    validate!(size >= KNOWN_MDHD_SIZE);
    let version             = br.read_byte()?;
    validate!(version == 0);
    let flags               = br.read_u24be()?;
    validate!(flags == 0);
    let _ctime              = br.read_u32be()?;
    let _mtime              = br.read_u32be()?;
    track.tb_den            = br.read_u32be()?;
    validate!(track.tb_den != 0);
    track.duration          = br.read_u32be()?;
    let _language           = br.read_u16be()?;
    let _quality            = br.read_u16be()?;

    Ok(KNOWN_MDHD_SIZE)
}

fn read_hdlr(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    const KNOWN_HDLR_SIZE: u64 = 24;
    validate!(size >= KNOWN_HDLR_SIZE);
    let version             = br.read_byte()?;
    validate!(version == 0);
    let flags               = br.read_u24be()?;
    validate!(flags == 0);
    let comp_type           = br.read_u32be()?;
    let comp_subtype        = br.read_u32be()?;
    let _comp_manufacturer  = br.read_u32be()?;
    let _comp_flags         = br.read_u32be()?;
    let _comp_flags_mask    = br.read_u32be()?;

    if comp_type == mktag!(b"mhlr") || comp_type == 0 {
        if comp_subtype == mktag!(b"vide") {
            track.stream_type = StreamType::Video;
        } else if comp_subtype == mktag!(b"soun") {
            track.stream_type = StreamType::Audio;
        } else {
            track.stream_type = StreamType::Data;
        }
    } else if comp_type == mktag!(b"dhlr") {
        track.stream_type = StreamType::Data;
    } else {
        println!("Unknown stream type");
        track.stream_type = StreamType::Data;
    }

    Ok(KNOWN_HDLR_SIZE)
}

fn read_minf(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    track.read_minf(br, size)?;
    Ok(size)
}

const MINF_CHUNK_HANDLERS: &[TrackChunkHandler] = &[
    TrackChunkHandler { ctype: mktag!(b"hdlr"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"dinf"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"vmhd"), parse: read_vmhd },
    TrackChunkHandler { ctype: mktag!(b"smhd"), parse: read_smhd },
    TrackChunkHandler { ctype: mktag!(b"gmhd"), parse: read_gmhd },
    TrackChunkHandler { ctype: mktag!(b"gmin"), parse: read_gmin },
    TrackChunkHandler { ctype: mktag!(b"stbl"), parse: read_stbl },
];

fn read_vmhd(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    const KNOWN_VMHD_SIZE: u64 = 12;
    validate!(track.stream_type == StreamType::Video);
    validate!(size >= KNOWN_VMHD_SIZE);
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
                              br.read_skip(2)?; // graphics mode
                              br.read_skip(6)?; // opcolor
    Ok(KNOWN_VMHD_SIZE)
}

fn read_smhd(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    const KNOWN_SMHD_SIZE: u64 = 8;
    validate!(track.stream_type == StreamType::Audio);
    validate!(size >= KNOWN_SMHD_SIZE);
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
                              br.read_skip(2)?; // balance
                              br.read_skip(2)?;
    Ok(KNOWN_SMHD_SIZE)
}

fn read_gmhd(track: &mut Track, _br: &mut ByteReader, _size: u64) -> DemuxerResult<u64> {
    validate!(track.stream_type == StreamType::Data);
    Ok(0)
}

fn read_gmin(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    validate!(track.stream_type == StreamType::Data);
    const KNOWN_GMIN_SIZE: u64 = 16;
    validate!(size >= KNOWN_GMIN_SIZE);
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
                              br.read_skip(2)?; // graphics mode
                              br.read_skip(6)?; // opcolor
                              br.read_skip(2)?; // balance
                              br.read_skip(2)?;
    Ok(KNOWN_GMIN_SIZE)
}

fn read_stbl(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    track.read_stbl(br, size)?;
    Ok(size)
}

const STBL_CHUNK_HANDLERS: &[TrackChunkHandler] = &[
    TrackChunkHandler { ctype: mktag!(b"stsd"), parse: read_stsd },
    TrackChunkHandler { ctype: mktag!(b"stts"), parse: read_stts },
    TrackChunkHandler { ctype: mktag!(b"stss"), parse: read_stss },
    TrackChunkHandler { ctype: mktag!(b"stsc"), parse: read_stsc },
    TrackChunkHandler { ctype: mktag!(b"stsz"), parse: read_stsz },
    TrackChunkHandler { ctype: mktag!(b"stco"), parse: read_stco },
    TrackChunkHandler { ctype: mktag!(b"stsh"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"ctts"), parse: read_ctts },
];

fn parse_audio_edata(br: &mut ByteReader, start_pos: u64, size: u64) -> DemuxerResult<Option<Vec<u8>>> {
    let read_part = br.tell() - start_pos;
    if read_part + 8 < size {
        let mut buf = [0; 8];
                              br.peek_buf(&mut buf)?;
        if &buf[4..8] != b"wave" {
            let mut buf = vec![0; (size - read_part) as usize];
                              br.read_buf(&mut buf)?;
            return Ok(Some(buf));
        }

        let csize           = br.read_u32be()? as u64;
        let ctag            = br.read_u32be()?;
        validate!(read_part + csize <= size);
        validate!(ctag == mktag!(b"wave"));
        if csize == 8 {
            return Ok(None);
        }
        let mut buf = [0; 8];
                              br.peek_buf(&mut buf)?;
        if &buf[4..8] == b"frma" {
                              br.read_skip(12)?;
            if csize > 20 {
                let mut buf = vec![0; (csize - 20) as usize];
                              br.read_buf(&mut buf)?;
                Ok(Some(buf))
            } else {
                Ok(None)
            }
        } else if csize > 8 {
            let mut buf = vec![0; (csize as usize) - 8];
                              br.read_buf(&mut buf)?;
            Ok(Some(buf))
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

#[allow(clippy::neg_cmp_op_on_partial_ord)]
fn read_stsd(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    const KNOWN_STSD_SIZE: u64 = 24;
    validate!(size >= KNOWN_STSD_SIZE);
    let start_pos = br.tell();
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
    let entries             = br.read_u32be()?;
    validate!(entries > 0);
    let esize               = u64::from(br.read_u32be()?);
    validate!(esize + 8 <= size);
    let mut fcc = [0u8; 4];
                              br.read_buf(&mut fcc)?;
                              br.read_skip(6)?;
    let _data_ref           = br.read_u16be()?;

    track.fcc = fcc;

    let codec_info;
    match track.stream_type {
        StreamType::Video => {
            let _ver            = br.read_u16be()?;
            let _revision       = br.read_u16le()?;
            let _vendor         = br.read_u32be()?;
            let _temp_quality   = br.read_u32be()?;
            let _spat_quality   = br.read_u32be()?;
            let width           = br.read_u16be()? as usize;
            let height          = br.read_u16be()? as usize;
            let _hor_res        = br.read_u32be()?;
            let _vert_res       = br.read_u32be()?;
            let data_size       = br.read_u32be()?;
            validate!(data_size == 0);
            let _frame_count    = br.read_u16be()? as usize;
            let _cname_len      = br.read_byte()? as usize;
                                  br.read_skip(31)?; // actual compressor name
            let depth           = br.read_u16be()?;
            let ctable_id       = br.read_u16be()?;
            let grayscale = depth > 0x20 || depth == 1;
            let depth = if grayscale { depth & 0x1F } else { depth };
            if ctable_id == 0 {
                let max_pal_size = start_pos + size - br.tell();
                if depth <= 8 {
                    let mut pal = [0; 1024];
                    read_palette(br, max_pal_size, &mut pal)?;
                    track.pal = Some(Arc::new(pal));
                } else {
                                  br.read_skip(max_pal_size as usize)?;
                }
            } else if (depth <= 8) && !grayscale {
                match depth & 0x1F {
                    2 => {
                        let mut pal = [0; 1024];
                        pal[..4 * 4].copy_from_slice(&MOV_DEFAULT_PAL_2BIT);
                        track.pal = Some(Arc::new(pal));
                    },
                    4 => {
                        let mut pal = [0; 1024];
                        pal[..16 * 4].copy_from_slice(&MOV_DEFAULT_PAL_4BIT);
                        track.pal = Some(Arc::new(pal));
                    },
                    8 => {
                        track.pal = Some(Arc::new(MOV_DEFAULT_PAL_8BIT));
                    },
                    _ => {},
                };
            } else if grayscale && ctable_id != 0xFFFF {
                let mut pal = [0; 1024];
                let cdepth = depth & 0x1F;
                let size = 1 << cdepth;
                for i in 0..size {
                    let mut clr = ((size - 1 - i) as u8) << (8 - cdepth);
                    let mut off = 8 - cdepth;
                    while off >= cdepth {
                        clr |= clr >> (8 - off);
                        off -= cdepth;
                    }
                    if off > 0 {
                        clr |= clr >> (8 - off);
                    }
                    pal[i * 4]     = clr;
                    pal[i * 4 + 1] = clr;
                    pal[i * 4 + 2] = clr;
                }
                track.pal = Some(Arc::new(pal));
            }
// todo other atoms, put as extradata
            let cname = if let Some(name) = find_codec_from_mov_video_fourcc(&fcc) {
                    name
                } else if let Some(name) = find_codec_from_avi_fourcc(&fcc) {
                    name
                } else {
                    "unknown"
                };
            let format = if depth > 8 { RGB24_FORMAT } else { PAL8_FORMAT };
            let mut vhdr = NAVideoInfo::new(width, height, false, format);
            vhdr.bits = depth as u8;
            //skip various common atoms
            while br.tell() - start_pos + 4 < size {
                let mut buf = [0u8; 8];
                br.peek_buf(&mut buf)?;
                let tsize = read_u32be(&buf).unwrap() as usize;
                let tag = &buf[4..8];
                validate!(tsize >= 8);
                match tag {
                    b"pasp" | b"clap" => {
                        br.read_skip(tsize)?;
                    },
                    _ => break,
                };
            }
            let edata = if br.tell() - start_pos + 4 < size {
                    let edata_size  = br.read_u32be()? as usize;
                    validate!(edata_size >= 4);
                    let mut buf = vec![0; edata_size - 4];
                                  br.read_buf(buf.as_mut_slice())?;
                    Some(buf)
                } else {
                    None
                };
            codec_info = NACodecInfo::new(cname, NACodecTypeInfo::Video(vhdr), edata);
        },
        StreamType::Audio => {
            let sver            = br.read_u16be()?;
            let _revision       = br.read_u16le()?;
            let _vendor         = br.read_u32be()?;
            let mut nchannels   = br.read_u16be()?;
            if sver != 2 {
                validate!(nchannels <= 64);
            }
            let sample_size     = br.read_u16be()?;
            validate!(sample_size <= 128);
            let _compr_id       = br.read_u16be()?;
            let packet_size     = br.read_u16be()? as usize;
            validate!(packet_size == 0);
            let mut sample_rate = br.read_u32be()? >> 16;
            if sver != 2 {
                validate!(sample_rate > 0);
            }
            let cname = if let Some(name) = find_codec_from_mov_audio_fourcc(&fcc) {
                    name
                } else if let (true, Some(name)) = ((fcc[0] == b'm' && fcc[1] == b's'),  find_codec_from_wav_twocc(u16::from(fcc[2]) * 256 + u16::from(fcc[3]))) {
                    name
                } else {
                    "unknown"
                };
            let mut soniton = NASoniton::new(sample_size as u8, SONITON_FLAG_SIGNED | SONITON_FLAG_BE);
            if &fcc == b"raw " && sample_size == 8 {
                soniton.signed = false;
            }
            let block_align = 1;
            match sver {
                1 => {
                    let samples_per_packet  = br.read_u32be()?;
                    let _bytes_per_packet   = br.read_u32be()?;
                    let bytes_per_frame     = br.read_u32be()?;
                    let _bytes_per_sample   = br.read_u32be()?;
                    track.bsize = bytes_per_frame as usize;
                    track.frame_samples = samples_per_packet as usize;
                    track.tb_num = samples_per_packet;
                },
                2 => {
                                              br.read_u32be()?; // some size
                    let srate               = br.read_f64be()?;
                    validate!(srate > 1.0);
                    sample_rate = srate as u32;
                    let channels            = br.read_u32be()?;
                    validate!(channels > 0 && channels < 255);
                    nchannels = channels as u16;
                                              br.read_u32be()?; // always 0x7F000000
                    let _bits_per_csample   = br.read_u32be()?;
                    let _codec_flags        = br.read_u32be()?;
                    let bytes_per_frame     = br.read_u32be()?;
                    let samples_per_packet  = br.read_u32be()?;
                    track.bsize = bytes_per_frame as usize;
                    track.frame_samples = samples_per_packet as usize;
                    track.tb_num = samples_per_packet;
                },
                _ => {
                    track.bsize = (sample_size / 8) as usize;
                },
            };
            if track.tb_den <= 1 {
                track.tb_den = sample_rate;
            }
            track.raw_audio = matches!(&fcc,
                    b"NONE" | b"raw " | b"twos" | b"sowt" |
                    b"in24" | b"in32" | b"fl32" | b"fl64" |
                    b"ima4" | b"ms\x00\x02" | b"ms\x00\x21" |
                    b"alaw" | b"ulaw" |
                    b"MAC3" | b"MAC6");
            let ahdr = NAAudioInfo::new(sample_rate, nchannels as u8, soniton, block_align);
            let edata = parse_audio_edata(br, start_pos, size)?;
            codec_info = NACodecInfo::new(cname, NACodecTypeInfo::Audio(ahdr), edata);
            track.channels  = nchannels as usize;
            track.bits      = sample_size as usize;
        },
        StreamType::None => {
            return Err(DemuxerError::InvalidData);
        },
        _ => {
//todo put it all into extradata
            let edata = None;
            codec_info = NACodecInfo::new("unknown", NACodecTypeInfo::None, edata);
        },
    };
    let read_size = br.tell() - start_pos;
    validate!(read_size <= size);
    track.stream = Some(NAStream::new(track.stream_type, track.track_no, codec_info, track.tb_num, track.tb_den, u64::from(track.duration)));
    track.stsd_found = true;
    Ok(read_size)
}

fn read_stts(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    validate!(size >= 8);
    let start_pos = br.tell();
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
    let entries             = br.read_u32be()? as usize;
    validate!(entries as u64 <= (size - 8) / 8);
    if entries == 0 {
    } else if entries == 1 {
        let _count          = br.read_u32be()?;
        let tb_num          = br.read_u32be()?;
        validate!(tb_num != 0);
        track.rescale(tb_num);
    } else {
        track.time_to_sample.clear();
        track.time_to_sample.reserve(entries);
        for _ in 0..entries {
            let count       = br.read_u32be()?;
            let mult        = br.read_u32be()?;
            track.time_to_sample.push((count, mult));
        }
    }
    let read_size = br.tell() - start_pos;
    validate!(read_size <= size);
    Ok(read_size)
}

fn read_stss(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
    let entries             = br.read_u32be()? as usize;
    validate!(entries < ((std::u32::MAX >> 2) - 8) as usize);
    validate!((entries * 4 + 8) as u64 == size);
    track.keyframes = Vec::with_capacity(entries);
    let mut last_sample_no = 0;
    for _ in 0..entries {
        let sample_no       = br.read_u32be()?;
        validate!(sample_no > last_sample_no);
        track.keyframes.push(sample_no);
        last_sample_no = sample_no;
    }
    Ok(size)
}

fn read_stsc(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
    let entries             = br.read_u32be()? as usize;
    validate!(entries < ((std::u32::MAX / 12) - 8) as usize);
    validate!((entries * 12 + 8) as u64 == size);
    track.sample_map = Vec::with_capacity(entries);
    let mut last_sample_no = 0;
    for _i in 0..entries {
        let sample_no       = br.read_u32be()?;
        validate!(sample_no > last_sample_no);
        let nsamples        = br.read_u32be()?;
        let _sample_desc    = br.read_u32be()?;
        track.sample_map.push((sample_no, nsamples));
        last_sample_no = sample_no;
    }
    Ok(size)
}

fn read_stsz(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
    let sample_size         = br.read_u32be()?;
    if sample_size != 0 {
        track.sample_size = sample_size;
        if track.sample_size != 1 || track.bsize == 0 {
            track.bsize = sample_size as usize;
        }
        Ok(8)
    } else {
        let entries             = br.read_u32be()? as usize;
        validate!((entries * 4 + 12) as u64 == size);
        track.chunk_sizes = Vec::with_capacity(entries);
        for _ in 0..entries {
            let sample_size     = br.read_u32be()?;
            track.chunk_sizes.push(sample_size);
        }
        Ok(size)
    }
}

fn read_stco(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    let version             = br.read_byte()?;
    validate!(version == 0);
    let _flags              = br.read_u24be()?;
    let entries             = br.read_u32be()? as usize;
    validate!((entries * 4 + 8) as u64 == size);
    track.chunk_offsets = Vec::with_capacity(entries);
    for _i in 0..entries {
        let sample_offset   = br.read_u32be()?;
        track.chunk_offsets.push(u64::from(sample_offset));
    }
    Ok(size)
}

fn read_ctts(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    validate!(size >= 8);
    let version             = br.read_byte()?;
    let _flags              = br.read_u24be()?;
    if version > 1 {
        return Err(DemuxerError::NotImplemented);
    }
    let entries             = br.read_u32be()? as usize;
    track.ctts_version = version;
    track.ctts_map.resize(entries);
    match version {
        0 | 1 => {
            validate!(size == (entries as u64) * 8 + 8);
            for _ in 0..entries {
                let samp_count  = br.read_u32be()?;
                let samp_offset = br.read_u32be()?;
                track.ctts_map.add(samp_count, samp_offset / track.tb_div);
            }
        },
        _ => unreachable!(),
    };
    track.ctts_map.reset();

    Ok(size)
}

const TRAF_CHUNK_HANDLERS: &[TrackChunkHandler] = &[
    TrackChunkHandler { ctype: mktag!(b"tfhd"), parse: read_tfhd },
    TrackChunkHandler { ctype: mktag!(b"trun"), parse: read_trun },
    TrackChunkHandler { ctype: mktag!(b"sbgp"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"sgpd"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"subs"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"saiz"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"saio"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"tfdt"), parse: skip_chunk },
    TrackChunkHandler { ctype: mktag!(b"meta"), parse: skip_chunk },
];

fn read_tfhd(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    validate!(size >= 8);
    let start = br.tell();
    let _version            = br.read_byte()?;
    let flags               = br.read_u24be()?;
    let _track_id           = br.read_u32be()?;
    if (flags & 0x000001) != 0 {
        let base_offset     = br.read_u64be()?;
        track.moof_off = base_offset;
    }
    if (flags & 0x000002) != 0 {
        let _sample_description_index = br.read_u32be()?;
    }
    if (flags & 0x000008) != 0 {
        let default_sample_duration = br.read_u32be()?;
        if track.tb_div == 1 {
            track.rescale(default_sample_duration);
        }
    }
    if (flags & 0x000010) != 0 {
        let _default_sample_size = br.read_u32be()?;
    }
    if (flags & 0x000020) != 0 {
        let _default_sample_flags = br.read_u32be()?;
    }
    if (flags & 0x010000) != 0 {
    }
    /*if (flags & 0x020000) != 0 { // base offset is moof start
    }*/
    Ok(br.tell() - start)
}

fn read_trun(track: &mut Track, br: &mut ByteReader, size: u64) -> DemuxerResult<u64> {
    validate!(size >= 8);
    let version             = br.read_byte()?;
    let flags               = br.read_u24be()?;
    let data_off_present        = (flags & 0x000001) != 0;
    let first_sample_flags      = (flags & 0x000004) != 0;
    let sample_duration_present = (flags & 0x000100) != 0;
    let sample_size_present     = (flags & 0x000200) != 0;
    let sample_flags_present    = (flags & 0x000400) != 0;
    let sample_ct_off_present   = (flags & 0x000800) != 0;

    let sample_count            = br.read_u32be()? as usize;

    let mut hdr_size = 8;
    let mut arr_size = 0;
    if data_off_present {
        hdr_size += 4;
    }
    if first_sample_flags {
        hdr_size += 4;
    }
    if sample_duration_present {
        arr_size += 4;
    }
    if sample_size_present {
        arr_size += 4;
    }
    if sample_flags_present {
        arr_size += 4;
    }
    if sample_ct_off_present {
        arr_size += 4;
    }
    validate!(size == hdr_size + arr_size * (sample_count as u64));

    let mut data_off = if data_off_present {
            let off             = br.read_u32be()? as i32;
            let new_off = (track.moof_off as i64) + i64::from(off);
            validate!(new_off > 0);
            new_off as u64
        } else {
            track.moof_off
        };
    if first_sample_flags {
        let _flags              = br.read_u32be()?;
    }

    if sample_size_present {
        track.chunk_sizes.reserve(sample_count);
        track.chunk_offsets.reserve(sample_count);
    }

    if sample_ct_off_present {
        if track.ctts_version != version {
            track.ctts_version = version;
        }
        track.ctts_map.reserve(sample_count);
    }

    if track.chunk_offsets.len() < (std::u32::MAX as usize) {
        track.keyframes.push((track.chunk_offsets.len() + 1) as u32);
    }
    for _ in 0..sample_count {
        if sample_duration_present {
            let _duration       = br.read_u32be()?;
        }
        if sample_size_present {
            let ssize           = br.read_u32be()?;
            track.chunk_sizes.push(ssize);
            track.chunk_offsets.push(data_off);
            data_off += u64::from(ssize);
        }
        if sample_flags_present {
            let _flags          = br.read_u32be()?;
        }
        if sample_ct_off_present {
            let samp_offset     = br.read_u32be()?;
            if version == 0 {
                track.ctts_map.add(1, samp_offset / track.tb_div);
            } else {
                track.ctts_map.add(1, ((samp_offset as i32) / (track.tb_div as i32)) as u32);
            }
        }
    }

    Ok(size)
}

struct MOVDemuxer<'a> {
    src:            &'a mut ByteReader<'a>,
    depth:          usize,
    mdat_pos:       u64,
    mdat_size:      u64,
    tracks:         Vec<Track>,
    cur_track:      usize,
    tb_den:         u32,
    duration:       u32,
    pal:            Option<Arc<[u8; 1024]>>,

    moof_off:       u64,

    print_chunks:   bool,

    macbinary:      bool,
}

struct Track {
    track_id:       u32,
    track_str_id:   usize,
    track_no:       u32,
    tb_num:         u32,
    tb_den:         u32,
    tb_div:         u32,
    raw_audio:      bool,
    raw_apos:       u64,
    duration:       u32,
    depth:          u8,
    tkhd_found:     bool,
    stsd_found:     bool,
    stream_type:    StreamType,
    width:          usize,
    height:         usize,
    channels:       usize,
    bits:           usize,
    bsize:          usize,
    fcc:            [u8; 4],
    keyframes:      Vec<u32>,
    chunk_sizes:    Vec<u32>,
    chunk_offsets:  Vec<u64>,
    time_to_sample: Vec<(u32, u32)>,
    sample_map:     Vec<(u32, u32)>,
    sample_size:    u32,
    frame_samples:  usize,
    ctts_map:       RLESearcher<u32>,
    ctts_version:   u8,
    stream:         Option<NAStream>,
    cur_chunk:      usize,
    cur_sample:     usize,
    cur_ts:         Option<u64>,
    samples_left:   usize,
    last_offset:    u64,
    pal:            Option<Arc<[u8; 1024]>>,
    timesearch:     TimeSearcher,

    moof_off:       u64,

    print_chunks:   bool,
}

#[derive(Default)]
struct TimeSearcher {
    idx:        usize,
    base:       u64,
    sbase:      u32,
    cur_len:    u32,
    cur_mul:    u32,
}

impl TimeSearcher {
    fn new() -> Self { Self::default() }
    fn reset(&mut self) {
        *self = Self::default();
    }
    fn map_time(&mut self, sample: u32, tts: &[(u32, u32)]) -> u64 {
        if tts.is_empty() {
            u64::from(sample)
        } else if sample >= self.sbase {
            let mut sample = sample - self.sbase;
            if self.idx == 0 {
                let (cur_len, cur_mul) = tts[0];
                self.cur_len = cur_len;
                self.cur_mul = cur_mul;
                self.idx += 1;
            }
            while self.idx < tts.len() && sample > self.cur_len {
                sample -= self.cur_len;
                self.sbase += self.cur_len;
                self.base += u64::from(self.cur_len) * u64::from(self.cur_mul);
                self.cur_len = tts[self.idx].0;
                self.cur_mul = tts[self.idx].1;
                self.idx += 1;
            }
            self.base + u64::from(sample) * u64::from(self.cur_mul)
        } else {
            self.reset();
            self.map_time(sample, tts)
        }
    }
}

#[derive(Default)]
struct RLESearcher<T> {
    array:      Vec<(u32, T)>,
    idx:        usize,
    start:      u64,
    next:       u64,
}

impl<T:Default+Copy> RLESearcher<T> {
    fn new() -> Self { Self::default() }
    fn resize(&mut self, size: usize) {
        self.array.clear();
        self.array.reserve(size);
    }
    fn reserve(&mut self, size: usize) {
        self.array.reserve(size);
    }
    fn add(&mut self, len: u32, val: T) {
        self.array.push((len, val));
    }
    fn reset(&mut self) {
        self.start = 0;
        if !self.array.is_empty() {
            self.idx = 0;
            self.next = u64::from(self.array[0].0);
        } else {
            self.idx = self.array.len();
            self.next = 0;
        }
    }
    fn map(&mut self, sample: u64) -> Option<T> {
        if sample < self.start {
            self.reset();
        }
        if self.idx < self.array.len() {
            if sample < self.next {
                Some(self.array[self.idx].1)
            } else {
                while (self.idx < self.array.len()) && (sample >= self.next) {
                    self.start = self.next;
                    self.idx += 1;
                    if self.idx < self.array.len() {
                        self.next += u64::from(self.array[self.idx].0);
                    }
                }
                if self.idx < self.array.len() {
                    Some(self.array[self.idx].1)
                } else {
                    None
                }
            }
        } else {
            None
        }
    }
}

impl Track {
    fn new(track_no: u32, tb_den: u32) -> Self {
        Self {
            tkhd_found:     false,
            stsd_found:     false,
            track_id:       0,
            track_str_id:   0,
            track_no,
            tb_num: 1,
            tb_den,
            tb_div:         1,
            raw_audio:      false,
            raw_apos:       0,
            duration:       0,
            stream_type:    StreamType::None,
            width:          0,
            height:         0,
            channels:       0,
            bits:           0,
            bsize:          0,
            fcc:            [0; 4],
            keyframes:      Vec::new(),
            chunk_sizes:    Vec::new(),
            chunk_offsets:  Vec::new(),
            time_to_sample: Vec::new(),
            sample_map:     Vec::new(),
            sample_size:    0,
            frame_samples:  0,
            ctts_map:       RLESearcher::new(),
            ctts_version:   0,
            stream:         None,
            depth:          0,
            cur_chunk:      0,
            cur_sample:     0,
            cur_ts:         None,
            samples_left:   0,
            last_offset:    0,
            pal:            None,
            timesearch:     TimeSearcher::new(),

            moof_off:       0,

            print_chunks:   false,
        }
    }
    read_chunk_list!(track; "trak", read_trak, TRAK_CHUNK_HANDLERS);
    read_chunk_list!(track; "mdia", read_mdia, MDIA_CHUNK_HANDLERS);
    read_chunk_list!(track; "minf", read_minf, MINF_CHUNK_HANDLERS);
    read_chunk_list!(track; "stbl", read_stbl, STBL_CHUNK_HANDLERS);
    read_chunk_list!(track; "traf", read_traf, TRAF_CHUNK_HANDLERS);
    fn rescale(&mut self, tb_num: u32) {
        self.tb_div = tb_num;
        if let Some(ref mut stream) = self.stream {
            let tb_den = stream.tb_den;
            let (tb_num, tb_den) = reduce_timebase(tb_num * stream.tb_num, tb_den);
            stream.duration /= u64::from(self.tb_div);
            stream.tb_num = tb_num;
            stream.tb_den = tb_den;
            self.tb_num = tb_num;
            self.tb_den = tb_den;
            self.duration /= self.tb_div;
        }
    }
    fn fill_seek_index(&self, seek_index: &mut SeekIndex) {
        if !self.keyframes.is_empty() {
            seek_index.mode = SeekIndexMode::Present;
        }
        let mut tsearch = TimeSearcher::new();
        for kf_time in self.keyframes.iter() {
            let pts = tsearch.map_time(*kf_time - 1, &self.time_to_sample);
            let time = NATimeInfo::ts_to_time(pts, 1000, self.tb_num, self.tb_den);
            seek_index.add_entry(self.track_no, SeekEntry { time, pts: u64::from(*kf_time - 1), pos: 0 });
        }
    }
    fn calculate_chunk_size(&self, nsamp: usize) -> usize {
        if nsamp == 0 {
            self.bsize
        } else {
            match &self.fcc {
                b"NONE" | b"raw " | b"twos" | b"sowt" => {
                    (nsamp * self.bits * self.channels + 7) >> 3
                },
                b"ima4" => {
                    let nblocks = (nsamp + 63) >> 6;
                    nblocks * 34 * self.channels
                },
                b"MAC3" => {
                    (nsamp + 5) / 6 * 2 * self.channels
                },
                b"MAC6" => {
                    (nsamp + 5) / 6 * self.channels
                },
                b"in24" => nsamp * 3 * self.channels,
                b"in32" | b"fl32" => nsamp * 4 * self.channels,
                b"fl64" => nsamp * 8 * self.channels,
                b"ulaw" | b"alaw" => nsamp,
                b"ms\x00\x02" => { //MS ADPCM
                    ((nsamp - 1) / 2 + 7) * self.channels
                },
                b"ms\x00\x21" => { //IMA ADPCM
                    (nsamp / 2 + 4) * self.channels
                },
                _ => self.bsize,
            }
        }
    }
    fn get_next_chunk(&mut self) -> Option<(NATimeInfo, u64, usize)> {
        let pts_val = self.timesearch.map_time(self.cur_sample as u32, &self.time_to_sample);
        let dts = if let Some(dts_corr) = self.ctts_map.map(self.cur_sample as u64) {
                let dts = match self.ctts_version {
                        0 => pts_val.wrapping_add(u64::from(dts_corr)),
                        1 => pts_val.wrapping_add(i64::from(dts_corr as i32) as u64),
                        _ => unimplemented!(),
                    };
                if (dts as i64) < 0 {
                    None
                } else {
                    Some(dts)
                }
            } else {
                Some(pts_val)
            };
        let mut pts = NATimeInfo::new(Some(pts_val), dts, None, self.tb_num, self.tb_den);
        if self.chunk_offsets.len() == self.chunk_sizes.len() { // simple one-to-one mapping
            if self.cur_sample >= self.chunk_sizes.len() {
                return None;
            }
            let offset = self.chunk_offsets[self.cur_sample];
            let size   = self.chunk_sizes[self.cur_sample] as usize;
            self.cur_sample += 1;
            Some((pts, offset, size))
        } else {
            if self.samples_left == 0 {
                if self.cur_chunk >= self.chunk_offsets.len() {
                    return None;
                }
                for (idx, samples) in self.sample_map.iter() {
                    if *idx as usize <= self.cur_chunk + 1 {
                        self.samples_left = *samples as usize;
                    } else {
                        break;
                    }
                }
                self.last_offset = self.chunk_offsets[self.cur_chunk];
                self.cur_chunk += 1;
            }
            let offset = self.last_offset;
            let size = self.get_size(self.cur_sample);
            self.last_offset += size as u64;
            if self.stream_type == StreamType::Video {
                self.samples_left -= 1;
            } else if self.frame_samples != 0 && self.bsize != 0 {
                let nblocks = size / self.bsize;
                if self.raw_audio {
                    pts.pts = Some(self.raw_apos);
                    pts.duration = Some(nblocks as u64);
                    self.raw_apos += nblocks as u64;
                }
                if nblocks > 0 {
                    let consumed = (nblocks * self.frame_samples).min(self.samples_left);
                    self.samples_left -= consumed;
                } else {
                    self.samples_left = 0;
                }
            } else if !self.raw_audio {
                self.samples_left -= 1;
            } else {
                const BLOCK_SAMPLES: usize = 1024 * 6; // should be multiple of 64 and 6 to fit both IMA ADPCM and MACE 6:1 blocks
                let max_size = self.calculate_chunk_size(BLOCK_SAMPLES);
                let cur_size = self.calculate_chunk_size(self.samples_left);
                let add_off = (size - cur_size) as u64;
                let dsize = cur_size.min(max_size);
                if self.samples_left >= BLOCK_SAMPLES {
                    self.cur_sample += BLOCK_SAMPLES;
                    self.samples_left -= BLOCK_SAMPLES;
                    self.last_offset -= size as u64;
                } else {
                    self.cur_sample += self.samples_left;
                    self.samples_left = 0;
                }
                return Some((pts, offset + add_off, dsize));
            }
            self.cur_sample += 1;
            Some((pts, offset, size))
        }
    }
    fn get_size(&self, sample_no: usize) -> usize {
        if !self.chunk_sizes.is_empty() {
            self.chunk_sizes[sample_no] as usize
        } else if !self.sample_map.is_empty() {
            let mut nsamp = 0;
            for (idx, samples) in self.sample_map.iter() {
                if *idx as usize <= self.cur_chunk {
                    nsamp = *samples;
                } else {
                    break;
                }
            }
            self.calculate_chunk_size(nsamp as usize)
        } else {
            self.bsize
        }
    }
    #[allow(clippy::collapsible_if)]
    #[allow(clippy::collapsible_else_if)]
    fn seek(&mut self, pts: u64, tpoint: NATimePoint) -> DemuxerResult<u64> {
        self.cur_sample = pts as usize;
        self.samples_left = 0;
        self.cur_ts = None;
        if self.stream_type == StreamType::Audio {
            if let NATimePoint::Milliseconds(ms) = tpoint {
                let exp_pts = NATimeInfo::time_to_ts(ms, 1000, self.tb_num, self.tb_den);
                if self.raw_audio {
                    if self.frame_samples != 0 {
                        self.raw_apos = exp_pts / (self.frame_samples as u64);
                        let mut apos = 0;
                        self.cur_sample = 0;
                        self.cur_chunk = 0;
                        let mut cmap = self.sample_map.iter();
                        let mut cur_samps = 0;
                        let (mut next_idx, mut next_samples) = cmap.next().unwrap();
                        loop {
                            if self.cur_chunk + 1 == next_idx as usize {
                                self.samples_left = cur_samps;
                                cur_samps = next_samples as usize;
                                if let Some((new_idx, new_samples)) = cmap.next() {
                                    next_idx = *new_idx;
                                    next_samples = *new_samples;
                                }
                            }
                            self.raw_apos = apos;
                            apos += (cur_samps / self.frame_samples) as u64;
                            if apos > exp_pts {
                                if cur_samps == self.frame_samples || apos > exp_pts + 1 {
                                    if self.cur_chunk >= self.chunk_offsets.len() {
                                        return Err(DemuxerError::SeekError);
                                    }
                                    self.last_offset = self.chunk_offsets[self.cur_chunk];
                                    break;
                                }
                            }
                            self.cur_chunk += 1;
                        }
                        self.samples_left = cur_samps;
                        self.cur_chunk += 1;
                    } else {
                        self.raw_apos = exp_pts;
                        self.cur_sample = exp_pts as usize;
                        let mut csamp = 0;
                        self.cur_chunk = 0;
                        let mut cmap = self.sample_map.iter();
                        let mut cur_samps = 0;
                        let (mut next_idx, mut next_samples) = cmap.next().unwrap();
                        loop {
                            if self.cur_chunk + 1 == next_idx as usize {
                                self.samples_left = cur_samps;
                                cur_samps = next_samples as usize;
                                if let Some((new_idx, new_samples)) = cmap.next() {
                                    next_idx = *new_idx;
                                    next_samples = *new_samples;
                                }
                            }
                            csamp += cur_samps;
                            if csamp > self.cur_sample {
                                if self.cur_chunk >= self.chunk_offsets.len() {
                                    return Err(DemuxerError::SeekError);
                                }
                                self.last_offset = self.chunk_offsets[self.cur_chunk];
                                break;
                            }
                            self.cur_chunk += 1;
                        }
                        self.samples_left = csamp - self.cur_sample;
                        self.cur_chunk += 1;
                    }
                } else if self.chunk_offsets.len() == self.chunk_sizes.len() {
                    self.cur_chunk = self.cur_sample;
                } else {
                    if !self.time_to_sample.is_empty() {
                        let mut remaining = exp_pts;
                        let mut abs_csamp = 0;
                        for &(count, scount) in self.time_to_sample.iter() {
                            let count = u64::from(count);
                            let scount = u64::from(scount);
                            let nblk = remaining / scount;
                            if nblk < count {
                                abs_csamp += nblk;
                                break;
                            }
                            remaining -= count * scount;
                            abs_csamp += count;
                        }
                        self.cur_sample = abs_csamp as usize;
                    } else {
                        self.cur_sample = exp_pts as usize;
                    }
                    let tgt_sample = self.cur_sample;
                    let mut csamp = 0;
                    self.cur_chunk = 0;
                    let mut cmap = self.sample_map.iter();
                    let mut cur_samps = 0;
                    let (mut next_idx, mut next_samples) = cmap.next().unwrap();
                    loop {
                        if self.cur_chunk + 1 == next_idx as usize {
                            self.samples_left = cur_samps;
                            cur_samps = next_samples as usize;
                            if let Some((new_idx, new_samples)) = cmap.next() {
                                next_idx = *new_idx;
                                next_samples = *new_samples;
                            }
                        }
                        csamp += cur_samps;
                        if csamp > self.cur_sample {
                            if self.cur_chunk >= self.chunk_offsets.len() {
                                self.cur_sample = csamp - cur_samps;
                                self.samples_left = 0;
                                self.cur_sample = csamp;
                                return Err(DemuxerError::SeekError);
                            }
                            self.last_offset = self.chunk_offsets[self.cur_chunk];
                            break;
                        }
                        self.cur_chunk += 1;
                    }
                    self.cur_sample = csamp - cur_samps;
                    self.samples_left = cur_samps;
                    self.last_offset = self.chunk_offsets[self.cur_chunk];
                    self.cur_chunk += 1;

                    // try to refine sample position
                    if self.chunk_sizes.len() > self.chunk_offsets.len() {
                        for i in self.cur_sample..tgt_sample {
                            self.cur_sample   += 1;
                            self.samples_left -= 1;
                            self.last_offset  += u64::from(self.chunk_sizes[i]);
                        }
                    }
                }
            } else {
                self.cur_chunk = self.cur_sample;
            }
        } else if self.chunk_offsets.len() != self.chunk_sizes.len() && !self.sample_map.is_empty() {
            let mut csamp = 0;
            self.cur_chunk = 0;
            let mut cmap = self.sample_map.iter();
            let mut cur_samps = 0;
            let (mut next_idx, mut next_samples) = cmap.next().unwrap();
            loop {
                if self.cur_chunk + 1 == next_idx as usize {
                    self.samples_left = cur_samps;
                    cur_samps = next_samples as usize;
                    if let Some((new_idx, new_samples)) = cmap.next() {
                        next_idx = *new_idx;
                        next_samples = *new_samples;
                    }
                }
                csamp += cur_samps;
                if csamp >= self.cur_sample {
                    if self.cur_chunk >= self.chunk_offsets.len() {
                        return Err(DemuxerError::SeekError);
                    }
                    self.last_offset = self.chunk_offsets[self.cur_chunk];
                    break;
                }
                self.cur_chunk += 1;
            }
            csamp -= cur_samps;
            for sample_no in csamp..self.cur_sample {
                self.last_offset += self.get_size(sample_no) as u64;
            }
            self.samples_left = csamp + cur_samps - self.cur_sample;
            self.cur_chunk += 1;
        }
        let cur_pts = self.timesearch.map_time(self.cur_sample as u32, &self.time_to_sample);
        let cur_time = NATimeInfo::ts_to_time(cur_pts, 1000, self.tb_num, self.tb_den);
        Ok(cur_time)
    }
}

fn process_packet(src: &mut ByteReader, strmgr: &StreamManager, track: &mut Track, pts: NATimeInfo, offset: u64, size: usize, first: bool) -> DemuxerResult<NAPacket> {
    if let Some(cpts) = pts.get_pts() {
        let ts = NATimeInfo::ts_to_time(cpts, 1000, pts.tb_num, pts.tb_den);
        track.cur_ts = Some(ts);
    } else {
        track.cur_ts = None;
    }
    let stream = strmgr.get_stream(track.track_str_id);
    if stream.is_none() { return Err(DemuxerError::InvalidData); }
    let stream = stream.unwrap();
    src.seek(SeekFrom::Start(offset))?;
    let mut pkt = src.read_packet(stream, pts, false, size)?;
    if let Some(ref pal) = track.pal {
        let side_data = NASideData::Palette(first, pal.clone());
        pkt.add_side_data(side_data);
    }
    Ok(pkt)
}

impl<'a> DemuxCore<'a> for MOVDemuxer<'a> {
    fn open(&mut self, strmgr: &mut StreamManager, seek_index: &mut SeekIndex) -> DemuxerResult<()> {
        if !self.macbinary {
            self.read_root(strmgr)?;
        } else {
            let ver                     = self.src.read_byte()?;
            validate!(ver == 0);
                                          self.src.read_skip(64)?;
            let tag                     = self.src.read_tag()?;
            validate!(&tag == b"MooV");
                                          self.src.read_skip(14)?;
            let data_length             = self.src.read_u32be()?;
            validate!(data_length > 8);
            let rsrc_length             = self.src.read_u32be()?;
            validate!(rsrc_length > 0);
                                          self.src.read_skip(31)?;
            let ver                     = self.src.read_byte()?;
            validate!(ver == 0x81);
            let ver                     = self.src.read_byte()?;
            validate!(ver == 0x81);
            //xxx: maybe check header CRC

            let rsrc_start = 0x80 + ((data_length + 0x7F) & !0x7F);
                                          self.src.seek(SeekFrom::Start(rsrc_start.into()))?;
            let rsrc_off                = self.src.read_u32be()?;
            let rsrc_map_off            = self.src.read_u32be()?;
            let rsrc_size               = self.src.read_u32be()?;
            let _rsrc_map_size          = self.src.read_u32be()?;
            validate!(rsrc_off >= 0x10);
            validate!(rsrc_map_off >= rsrc_off + rsrc_size);
                                          self.src.seek(SeekFrom::Current(i64::from(rsrc_off - 16)))?;
            // I'm too lazy to parse resource map, so let's just iterate over resources for movie header
            let end_pos = u64::from(rsrc_start + rsrc_off + rsrc_size);
            let mut peek_buf = [0u8; 8];
            while self.src.tell() < end_pos {
                let cur_size            = self.src.read_u32be()?;
                validate!(self.src.tell() + u64::from(cur_size) <= end_pos);
                if cur_size > 8 {
                    let rsize           = self.src.peek_u32be()?;
                    if rsize == cur_size {
                                          self.src.peek_buf(&mut peek_buf)?;
                        if &peek_buf[4..] == b"moov" {
                                          self.src.read_skip(8)?;
                            self.read_moov(strmgr, rsize.into())?;
                            self.mdat_pos = 8;
                            break;
                        }
                    }
                }
                                          self.src.read_skip(cur_size as usize)?;
            }
        }
        validate!(self.mdat_pos > 0);
        validate!(!self.tracks.is_empty());
        for track in self.tracks.iter_mut() {
            let mut strm = None;
            std::mem::swap(&mut track.stream, &mut strm);
            if let Some(stream) = strm {
                let str_id = strmgr.add_stream(stream).unwrap();
                track.track_str_id = str_id;
            }
        }
        if self.macbinary {
            // patch data offsets
            for track in self.tracks.iter_mut() {
                for offset in track.chunk_offsets.iter_mut() {
                    *offset += 0x80;
                }
            }
        }
        for track in self.tracks.iter() {
            track.fill_seek_index(seek_index);
        }
        self.src.seek(SeekFrom::Start(self.mdat_pos))?;
        self.cur_track = 0;
        Ok(())
    }

    fn get_frame(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<NAPacket> {
        if self.tracks.is_empty() {
            return Err(DemuxerError::EOF);
        }
        let mut has_all_time = true;
        let mut min_ts = std::u64::MAX;
        for trk in self.tracks.iter() {
            if let Some(ts) = trk.cur_ts {
                min_ts = min_ts.min(ts);
            } else {
                has_all_time = false;
                break;
            }
        }
        if has_all_time {
            for (trk_no, track) in self.tracks.iter_mut().enumerate() {
                if let Some(ts) = track.cur_ts {
                    if ts == min_ts {
                        let first = track.cur_sample == 0;
                        if let Some((pts, offset, size)) = track.get_next_chunk() {
                            self.cur_track = trk_no + 1;
                            return process_packet(self.src, strmgr, track, pts, offset, size, first);
                        }
                    }
                }
            }
        }

        for _ in 0..self.tracks.len() {
            if self.cur_track >= self.tracks.len() {
                self.cur_track = 0;
            }
            let track = &mut self.tracks[self.cur_track];
            self.cur_track += 1;
            let first = track.cur_sample == 0;
            if let Some((pts, offset, size)) = track.get_next_chunk() {
                return process_packet(self.src, strmgr, track, pts, offset, size, first);
            }
        }
        Err(DemuxerError::EOF)
    }

    fn seek(&mut self, time: NATimePoint, seek_index: &SeekIndex) -> DemuxerResult<()> {
        let ret = seek_index.find_pos(time);
        if ret.is_none() {
            if let NATimePoint::Milliseconds(_) = time {
                let mut aonly = true;
                for track in self.tracks.iter() {
                    if track.stream_type != StreamType::Audio {
                        aonly = false;
                        break;
                    }
                }
                if aonly {
                    for track in self.tracks.iter_mut() {
                        track.seek(0, time)?;
                    }
                    return Ok(());
                }
            }
            return Err(DemuxerError::SeekError);
        }
        let seek_info = ret.unwrap();
        let tbn = self.tracks[seek_info.str_id as usize].tb_num;
        let tbd = self.tracks[seek_info.str_id as usize].tb_den;
        let mut vpts = None;
        let mut apts = None;
        for track in self.tracks.iter_mut() {
            let cur_pts = if track.track_id == seek_info.str_id {
                    seek_info.pts
                } else {
                    seek_info.pts * u64::from(tbn) * u64::from(track.tb_den) / (u64::from(tbd) * u64::from(track.tb_num))
                };
            let actual_time = track.seek(cur_pts, time)?;
            match track.stream_type {
                StreamType::Video => vpts = Some(actual_time),
                StreamType::Audio => apts = Some(actual_time),
                _ => {},
            };
        }
        /* For audio+video stream case when the post-seek actual times differ
           by more than half a second try to seek audio to a closer position
           to video.
        */
        if let (true, Some(vtime), Some(atime)) = (self.tracks.len() == 2, vpts, apts) {
            if vtime.max(atime) - vtime.min(atime) > 500 && atime != 0 {
                for track in self.tracks.iter_mut() {
                    if track.stream_type == StreamType::Audio {
                        let new_pts = NATimeInfo::time_to_ts(vtime, 1000, track.tb_num, track.tb_den);
                        track.seek(new_pts, NATimePoint::Milliseconds(vtime))?;
                    }
                }
            }
        }

        Ok(())
    }
    fn get_duration(&self) -> u64 {
        if self.tb_den != 0 {
            u64::from(self.duration) * 1000 / u64::from(self.tb_den)
        } else {
            0
        }
    }
}

const PRINT_CHUNKS: &str = "print_chunks";

const DEMUXER_OPTIONS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name:           PRINT_CHUNKS,
        description:    "Print parsed file structure",
        opt_type:       NAOptionDefinitionType::Bool },
];

impl<'a> NAOptionHandler for MOVDemuxer<'a> {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { DEMUXER_OPTIONS }
    #[allow(clippy::single_match)]
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in DEMUXER_OPTIONS.iter() {
                if opt_def.check(option).is_ok() {
                    match (option.name, &option.value) {
                        (PRINT_CHUNKS, NAValue::Bool(val)) => {
                            self.print_chunks = *val;
                        },
                        _ => {},
                    }
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            PRINT_CHUNKS    => Some(NAValue::Bool(self.print_chunks)),
            _ => None,
        }
    }
}

impl<'a> MOVDemuxer<'a> {
    fn new(io: &'a mut ByteReader<'a>) -> Self {
        Self::new_common(io, false)
    }
    fn new_macbinary(io: &'a mut ByteReader<'a>) -> Self {
        Self::new_common(io, true)
    }
    fn new_common(io: &'a mut ByteReader<'a>, macbinary: bool) -> Self {
        MOVDemuxer {
            src:            io,
            depth:          0,
            mdat_pos:       0,
            mdat_size:      0,
            tracks:         Vec::with_capacity(2),
            cur_track:      0,
            tb_den:         0,
            duration:       0,
            pal:            None,

            moof_off:       0,

            print_chunks:   false,

            macbinary,
        }
    }
    fn read_root(&mut self, strmgr: &mut StreamManager) -> DemuxerResult<()> {
        self.depth = 0;
        while self.src.left() != 0 {
            let ret = read_chunk_header(self.src);
            if ret.is_err() { break; }
            let (ctype, size) = ret.unwrap();
            if self.print_chunks {
                print_cname(ctype, size, self.src.tell(), 0);
            }
            if IGNORED_CHUNKS.contains(&ctype) {
                self.src.skip64(size)?;
                continue;
            }
            let handler = ROOT_CHUNK_HANDLERS.iter().find(|x| x.ctype == ctype);
            let read_size;
            if let Some(handler) = handler {
                read_size = (handler.parse)(self, strmgr, size)?;
            } else {
                println!("skipping unknown chunk {:08X} size {}", ctype, size);
                read_size = 0;
            }
            validate!(read_size <= size);
            self.src.skip64(size - read_size)?;
        }
//todo check if all needed chunks are found
        Ok(())
    }
    read_chunk_list!(root; "moov", read_moov, MOOV_CHUNK_HANDLERS);
    read_chunk_list!(root; "moof", read_moof, MOOF_CHUNK_HANDLERS);
}

pub struct MOVDemuxerCreator { }

impl DemuxerCreator for MOVDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(MOVDemuxer::new(br))
    }
    fn get_name(&self) -> &'static str { "mov" }
}

pub struct MacBinaryMOVDemuxerCreator { }

impl DemuxerCreator for MacBinaryMOVDemuxerCreator {
    fn new_demuxer<'a>(&self, br: &'a mut ByteReader<'a>) -> Box<dyn DemuxCore<'a> + 'a> {
        Box::new(MOVDemuxer::new_macbinary(br))
    }
    fn get_name(&self) -> &'static str { "mov-macbin" }
}

const MOV_DEFAULT_PAL_2BIT: [u8; 4 * 4] = [
    0x93, 0x65, 0x5E, 0x00,
    0xFF, 0xFF, 0xFF, 0x00,
    0xDF, 0xD0, 0xAB, 0x00,
    0x00, 0x00, 0x00, 0x00
];
const MOV_DEFAULT_PAL_4BIT: [u8; 16 * 4] = [
    0xFF, 0xFB, 0xFF, 0x00,
    0xEF, 0xD9, 0xBB, 0x00,
    0xE8, 0xC9, 0xB1, 0x00,
    0x93, 0x65, 0x5E, 0x00,
    0xFC, 0xDE, 0xE8, 0x00,
    0x9D, 0x88, 0x91, 0x00,
    0xFF, 0xFF, 0xFF, 0x00,
    0xFF, 0xFF, 0xFF, 0x00,
    0xFF, 0xFF, 0xFF, 0x00,
    0x47, 0x48, 0x37, 0x00,
    0x7A, 0x5E, 0x55, 0x00,
    0xDF, 0xD0, 0xAB, 0x00,
    0xFF, 0xFB, 0xF9, 0x00,
    0xE8, 0xCA, 0xC5, 0x00,
    0x8A, 0x7C, 0x77, 0x00,
    0x00, 0x00, 0x00, 0x00
];
const MOV_DEFAULT_PAL_8BIT: [u8; 256 * 4] = [
    0xFF, 0xFF, 0xFF, 0x00,
    0xFF, 0xFF, 0xCC, 0x00,
    0xFF, 0xFF, 0x99, 0x00,
    0xFF, 0xFF, 0x66, 0x00,
    0xFF, 0xFF, 0x33, 0x00,
    0xFF, 0xFF, 0x00, 0x00,
    0xFF, 0xCC, 0xFF, 0x00,
    0xFF, 0xCC, 0xCC, 0x00,
    0xFF, 0xCC, 0x99, 0x00,
    0xFF, 0xCC, 0x66, 0x00,
    0xFF, 0xCC, 0x33, 0x00,
    0xFF, 0xCC, 0x00, 0x00,
    0xFF, 0x99, 0xFF, 0x00,
    0xFF, 0x99, 0xCC, 0x00,
    0xFF, 0x99, 0x99, 0x00,
    0xFF, 0x99, 0x66, 0x00,
    0xFF, 0x99, 0x33, 0x00,
    0xFF, 0x99, 0x00, 0x00,
    0xFF, 0x66, 0xFF, 0x00,
    0xFF, 0x66, 0xCC, 0x00,
    0xFF, 0x66, 0x99, 0x00,
    0xFF, 0x66, 0x66, 0x00,
    0xFF, 0x66, 0x33, 0x00,
    0xFF, 0x66, 0x00, 0x00,
    0xFF, 0x33, 0xFF, 0x00,
    0xFF, 0x33, 0xCC, 0x00,
    0xFF, 0x33, 0x99, 0x00,
    0xFF, 0x33, 0x66, 0x00,
    0xFF, 0x33, 0x33, 0x00,
    0xFF, 0x33, 0x00, 0x00,
    0xFF, 0x00, 0xFF, 0x00,
    0xFF, 0x00, 0xCC, 0x00,
    0xFF, 0x00, 0x99, 0x00,
    0xFF, 0x00, 0x66, 0x00,
    0xFF, 0x00, 0x33, 0x00,
    0xFF, 0x00, 0x00, 0x00,
    0xCC, 0xFF, 0xFF, 0x00,
    0xCC, 0xFF, 0xCC, 0x00,
    0xCC, 0xFF, 0x99, 0x00,
    0xCC, 0xFF, 0x66, 0x00,
    0xCC, 0xFF, 0x33, 0x00,
    0xCC, 0xFF, 0x00, 0x00,
    0xCC, 0xCC, 0xFF, 0x00,
    0xCC, 0xCC, 0xCC, 0x00,
    0xCC, 0xCC, 0x99, 0x00,
    0xCC, 0xCC, 0x66, 0x00,
    0xCC, 0xCC, 0x33, 0x00,
    0xCC, 0xCC, 0x00, 0x00,
    0xCC, 0x99, 0xFF, 0x00,
    0xCC, 0x99, 0xCC, 0x00,
    0xCC, 0x99, 0x99, 0x00,
    0xCC, 0x99, 0x66, 0x00,
    0xCC, 0x99, 0x33, 0x00,
    0xCC, 0x99, 0x00, 0x00,
    0xCC, 0x66, 0xFF, 0x00,
    0xCC, 0x66, 0xCC, 0x00,
    0xCC, 0x66, 0x99, 0x00,
    0xCC, 0x66, 0x66, 0x00,
    0xCC, 0x66, 0x33, 0x00,
    0xCC, 0x66, 0x00, 0x00,
    0xCC, 0x33, 0xFF, 0x00,
    0xCC, 0x33, 0xCC, 0x00,
    0xCC, 0x33, 0x99, 0x00,
    0xCC, 0x33, 0x66, 0x00,
    0xCC, 0x33, 0x33, 0x00,
    0xCC, 0x33, 0x00, 0x00,
    0xCC, 0x00, 0xFF, 0x00,
    0xCC, 0x00, 0xCC, 0x00,
    0xCC, 0x00, 0x99, 0x00,
    0xCC, 0x00, 0x66, 0x00,
    0xCC, 0x00, 0x33, 0x00,
    0xCC, 0x00, 0x00, 0x00,
    0x99, 0xFF, 0xFF, 0x00,
    0x99, 0xFF, 0xCC, 0x00,
    0x99, 0xFF, 0x99, 0x00,
    0x99, 0xFF, 0x66, 0x00,
    0x99, 0xFF, 0x33, 0x00,
    0x99, 0xFF, 0x00, 0x00,
    0x99, 0xCC, 0xFF, 0x00,
    0x99, 0xCC, 0xCC, 0x00,
    0x99, 0xCC, 0x99, 0x00,
    0x99, 0xCC, 0x66, 0x00,
    0x99, 0xCC, 0x33, 0x00,
    0x99, 0xCC, 0x00, 0x00,
    0x99, 0x99, 0xFF, 0x00,
    0x99, 0x99, 0xCC, 0x00,
    0x99, 0x99, 0x99, 0x00,
    0x99, 0x99, 0x66, 0x00,
    0x99, 0x99, 0x33, 0x00,
    0x99, 0x99, 0x00, 0x00,
    0x99, 0x66, 0xFF, 0x00,
    0x99, 0x66, 0xCC, 0x00,
    0x99, 0x66, 0x99, 0x00,
    0x99, 0x66, 0x66, 0x00,
    0x99, 0x66, 0x33, 0x00,
    0x99, 0x66, 0x00, 0x00,
    0x99, 0x33, 0xFF, 0x00,
    0x99, 0x33, 0xCC, 0x00,
    0x99, 0x33, 0x99, 0x00,
    0x99, 0x33, 0x66, 0x00,
    0x99, 0x33, 0x33, 0x00,
    0x99, 0x33, 0x00, 0x00,
    0x99, 0x00, 0xFF, 0x00,
    0x99, 0x00, 0xCC, 0x00,
    0x99, 0x00, 0x99, 0x00,
    0x99, 0x00, 0x66, 0x00,
    0x99, 0x00, 0x33, 0x00,
    0x99, 0x00, 0x00, 0x00,
    0x66, 0xFF, 0xFF, 0x00,
    0x66, 0xFF, 0xCC, 0x00,
    0x66, 0xFF, 0x99, 0x00,
    0x66, 0xFF, 0x66, 0x00,
    0x66, 0xFF, 0x33, 0x00,
    0x66, 0xFF, 0x00, 0x00,
    0x66, 0xCC, 0xFF, 0x00,
    0x66, 0xCC, 0xCC, 0x00,
    0x66, 0xCC, 0x99, 0x00,
    0x66, 0xCC, 0x66, 0x00,
    0x66, 0xCC, 0x33, 0x00,
    0x66, 0xCC, 0x00, 0x00,
    0x66, 0x99, 0xFF, 0x00,
    0x66, 0x99, 0xCC, 0x00,
    0x66, 0x99, 0x99, 0x00,
    0x66, 0x99, 0x66, 0x00,
    0x66, 0x99, 0x33, 0x00,
    0x66, 0x99, 0x00, 0x00,
    0x66, 0x66, 0xFF, 0x00,
    0x66, 0x66, 0xCC, 0x00,
    0x66, 0x66, 0x99, 0x00,
    0x66, 0x66, 0x66, 0x00,
    0x66, 0x66, 0x33, 0x00,
    0x66, 0x66, 0x00, 0x00,
    0x66, 0x33, 0xFF, 0x00,
    0x66, 0x33, 0xCC, 0x00,
    0x66, 0x33, 0x99, 0x00,
    0x66, 0x33, 0x66, 0x00,
    0x66, 0x33, 0x33, 0x00,
    0x66, 0x33, 0x00, 0x00,
    0x66, 0x00, 0xFF, 0x00,
    0x66, 0x00, 0xCC, 0x00,
    0x66, 0x00, 0x99, 0x00,
    0x66, 0x00, 0x66, 0x00,
    0x66, 0x00, 0x33, 0x00,
    0x66, 0x00, 0x00, 0x00,
    0x33, 0xFF, 0xFF, 0x00,
    0x33, 0xFF, 0xCC, 0x00,
    0x33, 0xFF, 0x99, 0x00,
    0x33, 0xFF, 0x66, 0x00,
    0x33, 0xFF, 0x33, 0x00,
    0x33, 0xFF, 0x00, 0x00,
    0x33, 0xCC, 0xFF, 0x00,
    0x33, 0xCC, 0xCC, 0x00,
    0x33, 0xCC, 0x99, 0x00,
    0x33, 0xCC, 0x66, 0x00,
    0x33, 0xCC, 0x33, 0x00,
    0x33, 0xCC, 0x00, 0x00,
    0x33, 0x99, 0xFF, 0x00,
    0x33, 0x99, 0xCC, 0x00,
    0x33, 0x99, 0x99, 0x00,
    0x33, 0x99, 0x66, 0x00,
    0x33, 0x99, 0x33, 0x00,
    0x33, 0x99, 0x00, 0x00,
    0x33, 0x66, 0xFF, 0x00,
    0x33, 0x66, 0xCC, 0x00,
    0x33, 0x66, 0x99, 0x00,
    0x33, 0x66, 0x66, 0x00,
    0x33, 0x66, 0x33, 0x00,
    0x33, 0x66, 0x00, 0x00,
    0x33, 0x33, 0xFF, 0x00,
    0x33, 0x33, 0xCC, 0x00,
    0x33, 0x33, 0x99, 0x00,
    0x33, 0x33, 0x66, 0x00,
    0x33, 0x33, 0x33, 0x00,
    0x33, 0x33, 0x00, 0x00,
    0x33, 0x00, 0xFF, 0x00,
    0x33, 0x00, 0xCC, 0x00,
    0x33, 0x00, 0x99, 0x00,
    0x33, 0x00, 0x66, 0x00,
    0x33, 0x00, 0x33, 0x00,
    0x33, 0x00, 0x00, 0x00,
    0x00, 0xFF, 0xFF, 0x00,
    0x00, 0xFF, 0xCC, 0x00,
    0x00, 0xFF, 0x99, 0x00,
    0x00, 0xFF, 0x66, 0x00,
    0x00, 0xFF, 0x33, 0x00,
    0x00, 0xFF, 0x00, 0x00,
    0x00, 0xCC, 0xFF, 0x00,
    0x00, 0xCC, 0xCC, 0x00,
    0x00, 0xCC, 0x99, 0x00,
    0x00, 0xCC, 0x66, 0x00,
    0x00, 0xCC, 0x33, 0x00,
    0x00, 0xCC, 0x00, 0x00,
    0x00, 0x99, 0xFF, 0x00,
    0x00, 0x99, 0xCC, 0x00,
    0x00, 0x99, 0x99, 0x00,
    0x00, 0x99, 0x66, 0x00,
    0x00, 0x99, 0x33, 0x00,
    0x00, 0x99, 0x00, 0x00,
    0x00, 0x66, 0xFF, 0x00,
    0x00, 0x66, 0xCC, 0x00,
    0x00, 0x66, 0x99, 0x00,
    0x00, 0x66, 0x66, 0x00,
    0x00, 0x66, 0x33, 0x00,
    0x00, 0x66, 0x00, 0x00,
    0x00, 0x33, 0xFF, 0x00,
    0x00, 0x33, 0xCC, 0x00,
    0x00, 0x33, 0x99, 0x00,
    0x00, 0x33, 0x66, 0x00,
    0x00, 0x33, 0x33, 0x00,
    0x00, 0x33, 0x00, 0x00,
    0x00, 0x00, 0xFF, 0x00,
    0x00, 0x00, 0xCC, 0x00,
    0x00, 0x00, 0x99, 0x00,
    0x00, 0x00, 0x66, 0x00,
    0x00, 0x00, 0x33, 0x00,
    0xEE, 0x00, 0x00, 0x00,
    0xDD, 0x00, 0x00, 0x00,
    0xBB, 0x00, 0x00, 0x00,
    0xAA, 0x00, 0x00, 0x00,
    0x88, 0x00, 0x00, 0x00,
    0x77, 0x00, 0x00, 0x00,
    0x55, 0x00, 0x00, 0x00,
    0x44, 0x00, 0x00, 0x00,
    0x22, 0x00, 0x00, 0x00,
    0x11, 0x00, 0x00, 0x00,
    0x00, 0xEE, 0x00, 0x00,
    0x00, 0xDD, 0x00, 0x00,
    0x00, 0xBB, 0x00, 0x00,
    0x00, 0xAA, 0x00, 0x00,
    0x00, 0x88, 0x00, 0x00,
    0x00, 0x77, 0x00, 0x00,
    0x00, 0x55, 0x00, 0x00,
    0x00, 0x44, 0x00, 0x00,
    0x00, 0x22, 0x00, 0x00,
    0x00, 0x11, 0x00, 0x00,
    0x00, 0x00, 0xEE, 0x00,
    0x00, 0x00, 0xDD, 0x00,
    0x00, 0x00, 0xBB, 0x00,
    0x00, 0x00, 0xAA, 0x00,
    0x00, 0x00, 0x88, 0x00,
    0x00, 0x00, 0x77, 0x00,
    0x00, 0x00, 0x55, 0x00,
    0x00, 0x00, 0x44, 0x00,
    0x00, 0x00, 0x22, 0x00,
    0x00, 0x00, 0x11, 0x00,
    0xEE, 0xEE, 0xEE, 0x00,
    0xDD, 0xDD, 0xDD, 0x00,
    0xBB, 0xBB, 0xBB, 0x00,
    0xAA, 0xAA, 0xAA, 0x00,
    0x88, 0x88, 0x88, 0x00,
    0x77, 0x77, 0x77, 0x00,
    0x55, 0x55, 0x55, 0x00,
    0x44, 0x44, 0x44, 0x00,
    0x22, 0x22, 0x22, 0x00,
    0x11, 0x11, 0x11, 0x00,
    0x00, 0x00, 0x00, 0x00
];

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn test_mov_demux() {
        // sample: https://samples.mplayerhq.hu/V-codecs/IV32/cubes.mov
        let mut file = File::open("assets/Indeo/cubes.mov").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = MOVDemuxer::new(&mut br);
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
    fn test_dash_demux() {
        // sample: a stream downloaded with youtube-dl
        let mut file = File::open("assets/ITU/dash.m4a").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = MOVDemuxer::new(&mut br);
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
    fn test_macbinary_demux() {
        // sample from King's Quest VI Macintosh edition
        let mut file = File::open("assets/QT/Halfdome.bin").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = MOVDemuxer::new_macbinary(&mut br);
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
