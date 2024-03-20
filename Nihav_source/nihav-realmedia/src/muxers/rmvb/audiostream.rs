use nihav_core::frame::*;
use nihav_core::muxers::*;
use super::RMStreamWriter;

// fourcc, codec name, interleaver, version
static AUDIO_CODEC_REGISTRY: &[(&[u8;4], &str, &[u8;4], u8)] = &[
    (b"lpcJ", "ra14.4", b"Int0", 3),
    (b"28_8", "ra28.8", b"Int4", 4),
    (b"cook", "cook",   b"genr", 5),
    (b"dnet", "ac3",    b"Int0", 4),
    (b"sipr", "sipro",  b"sipr", 4),
    (b"atrc", "atrac3", b"genr", 5),
    (b"LSD:", "ralf",   b"Int0", 6),
    (b"raac", "aac",    b"vbrs", 5),
    (b"racp", "aac",    b"vbrf", 5),
];

struct InterleaveParams {
    sample_rate:    u32,
    channels:       u8,
    block_size:     usize,
    factor:         usize,
    frames_per_blk: usize,
}

trait Interleaver {
    fn get_flavor(&self) -> usize;
    fn get_block_size(&self) -> usize;
    fn get_factor(&self) -> usize;
    fn get_frames_per_block(&self) -> usize;
    fn is_empty(&self) -> bool;
    fn add_packet(&mut self, src: &[u8]) -> bool;
    fn get_packet(&mut self) -> Option<(Vec<u8>, bool)>;
    fn flush(&mut self);

    fn get_frame_size(&self) -> usize { self.get_block_size() / self.get_frames_per_block() }
}

struct NoInterleaver {
    frame_size: usize,
    pkt:        Option<Vec<u8>>,
}
impl Interleaver for NoInterleaver {
    fn get_flavor(&self) -> usize { 0 }
    fn get_block_size(&self) -> usize { self.frame_size }
    fn get_factor(&self) -> usize { 1 }
    fn get_frames_per_block(&self) -> usize { 1 }
    fn is_empty(&self) -> bool { self.pkt.is_none() }
    fn add_packet(&mut self, src: &[u8]) -> bool {
        if self.pkt.is_none() {
            self.pkt = Some(src.to_vec());
            true
        } else {
            false
        }
    }
    fn get_packet(&mut self) -> Option<(Vec<u8>, bool)> {
        let mut ret = None;
        std::mem::swap(&mut self.pkt, &mut ret);
        ret.map(|pkt| (pkt, true))
    }
    fn flush(&mut self) {}
}

struct Int4Interleaver {
    flavor:     usize,
    factor:     usize,
    frame_size: usize,
    block_size: usize,
    fpb:        usize,
    cur_frame:  usize,
    rd_block:   usize,
    buf:        Vec<u8>,
    map:        Vec<usize>,
}
impl Int4Interleaver {
    fn new(frame_size: usize) -> MuxerResult<Self> {
        let params = RA_28_8_INTERLEAVE_PARAMS;
        for (flavor, entry) in params.iter().enumerate() {
            if entry.block_size / entry.frames_per_blk == frame_size {
                let full_size = entry.frames_per_blk * entry.factor;
                let mut map = vec![0; full_size];
                for i in 0..full_size {
                    let fval = i * entry.factor;
                    let mapped = (fval % full_size) + fval / full_size;
                    map[mapped] = i;
                }

                return Ok(Self {
                        flavor,
                        frame_size,
                        map,
                        factor:     entry.factor,
                        block_size: entry.block_size,
                        fpb:        entry.frames_per_blk,
                        cur_frame:  0,
                        rd_block:   0,
                        buf:        vec![0; entry.block_size * entry.factor],
                    });
            }
        }
        Err(MuxerError::UnsupportedFormat)
    }
}
impl Interleaver for Int4Interleaver {
    fn get_flavor(&self) -> usize { self.flavor }
    fn get_block_size(&self) -> usize { self.block_size }
    fn get_factor(&self) -> usize { self.factor }
    fn get_frames_per_block(&self) -> usize { self.fpb }
    fn is_empty(&self) -> bool { self.cur_frame == 0 }
    fn add_packet(&mut self, src: &[u8]) -> bool {
        if self.cur_frame == self.factor * self.fpb {
            return false;
        }
        let pos = self.map[self.cur_frame];
        self.buf[pos * self.frame_size..][..self.frame_size].copy_from_slice(src);
        self.cur_frame += 1;
        true
    }
    fn get_packet(&mut self) -> Option<(Vec<u8>, bool)> {
        if self.cur_frame == self.factor * self.fpb {
            let first = self.rd_block == 0;
            let src = &self.buf[self.rd_block * self.block_size..][..self.block_size];
            self.rd_block += 1;
            if self.rd_block == self.factor {
                self.rd_block = 0;
                self.cur_frame = 0;
            }
            Some((src.to_vec(), first))
        } else {
            None
        }
    }
    fn flush(&mut self) {
        if self.cur_frame != 0 {
            self.cur_frame = self.factor * self.fpb;
        }
    }
}

struct GenericInterleaver {
    flavor:     usize,
    factor:     usize,
    frame_size: usize,
    block_size: usize,
    fpb:        usize,
    cur_frame:  usize,
    rd_block:   usize,
    buf:        Vec<u8>,
    map:        Vec<usize>,
}
impl GenericInterleaver {
    fn new(ainfo: NAAudioInfo, frame_size: usize, fcc: [u8; 4]) -> MuxerResult<Self> {
        let params = match &fcc {
                b"atrc" => ATRAC_INTERLEAVE_PARAMS,
                b"cook" => COOK_INTERLEAVE_PARAMS,
                b"sipr" => SIPRO_INTERLEAVE_PARAMS,
                _ => return Err(MuxerError::UnsupportedFormat),
            };
        for (flavor, entry) in params.iter().enumerate() {
            if entry.sample_rate != 0 && entry.sample_rate != ainfo.sample_rate {
                continue;
            }
            if entry.channels != 0 && entry.channels != ainfo.channels {
                continue;
            }
            if entry.block_size / entry.frames_per_blk == frame_size {
                let full_size = entry.frames_per_blk * entry.factor;
                let mut map = vec![0; full_size];

                let mut frm = 0;
                let mut blk = 0;
                let mut even = true;
                for dst in map.iter_mut() {
                    let mapped = blk * entry.frames_per_blk + frm;
                    blk += 2;
                    if blk >= entry.factor {
                        if even {
                            blk = 1;
                        } else {
                            blk = 0;
                            frm += 1;
                        }
                        even = !even;
                    }
                    *dst = mapped;
                }

                return Ok(Self {
                        flavor,
                        frame_size,
                        map,
                        factor:     entry.factor,
                        block_size: entry.block_size,
                        fpb:        entry.frames_per_blk,
                        cur_frame:  0,
                        rd_block:   0,
                        buf:        vec![0; entry.block_size * entry.factor],
                    });
            }
        }
        Err(MuxerError::UnsupportedFormat)
    }
}
impl Interleaver for GenericInterleaver {
    fn get_flavor(&self) -> usize { self.flavor }
    fn get_block_size(&self) -> usize { self.block_size }
    fn get_factor(&self) -> usize { self.factor }
    fn get_frames_per_block(&self) -> usize { self.fpb }
    fn is_empty(&self) -> bool { self.cur_frame == 0 }
    fn add_packet(&mut self, src: &[u8]) -> bool {
        if self.cur_frame == self.factor * self.fpb {
            return false;
        }
        let pos = self.map[self.cur_frame];
        self.buf[pos * self.frame_size..][..self.frame_size].copy_from_slice(src);
        self.cur_frame += 1;
        true
    }
    fn get_packet(&mut self) -> Option<(Vec<u8>, bool)> {
        if self.cur_frame == self.factor * self.fpb {
            let first = self.rd_block == 0;
            let src = &self.buf[self.rd_block * self.block_size..][..self.block_size];
            self.rd_block += 1;
            if self.rd_block == self.factor {
                self.rd_block = 0;
                self.cur_frame = 0;
            }
            Some((src.to_vec(), first))
        } else {
            None
        }
    }
    fn flush(&mut self) {
        if self.cur_frame != 0 {
            self.cur_frame = self.factor * self.fpb;
        }
    }
}

struct SiproInterleaver {
    block_size: usize,
    factor:     usize,
    flavor:     usize,
    buf:        Vec<u8>,
    wr_pos:     usize,
    rd_pos:     usize,
}
impl SiproInterleaver {
    fn new(mut frame_size: usize) -> MuxerResult<Self> {
        if frame_size == 0 {
            return Err(MuxerError::UnsupportedFormat);
        }
        while frame_size < 200 {
            frame_size <<= 1;
        }
        for (flavor, entry) in SIPRO_INTERLEAVE_PARAMS.iter().enumerate() {
            if entry.block_size == frame_size {
                return Ok(Self {
                    block_size: entry.block_size,
                    factor:     entry.factor,
                    flavor,
                    buf:        vec![0; entry.block_size * entry.factor],
                    wr_pos:     0,
                    rd_pos:     0,
                });
            }
        }
        Err(MuxerError::UnsupportedFormat)
    }
}
impl Interleaver for SiproInterleaver {
    fn get_flavor(&self) -> usize { self.flavor }
    fn get_block_size(&self) -> usize { self.block_size }
    fn get_factor(&self) -> usize { self.factor }
    fn get_frames_per_block(&self) -> usize { 1 }
    fn is_empty(&self) -> bool { self.wr_pos == 0 }
    fn add_packet(&mut self, src: &[u8]) -> bool {
        if self.wr_pos < self.factor {
            self.buf[self.wr_pos * self.block_size..][..self.block_size].copy_from_slice(src);
            self.wr_pos += 1;
            true
        } else {
            false
        }
    }
    fn get_packet(&mut self) -> Option<(Vec<u8>, bool)> {
        if self.wr_pos == self.factor {
            let first = self.rd_pos == 0;
            if self.rd_pos == 0 {
                sipro_scramble(&mut self.buf, self.factor, self.block_size);
            }
            let ret = self.buf[self.rd_pos * self.block_size..][..self.block_size].to_vec();
            self.rd_pos += 1;
            if self.rd_pos == self.factor {
                self.rd_pos = 0;
                self.wr_pos = 0;
            }
            Some((ret, first))
        } else {
            None
        }
    }
    fn flush(&mut self) {
        if self.wr_pos != self.factor {
            self.wr_pos = self.factor;
        }
    }
}

fn sipro_scramble(buf: &mut [u8], factor: usize, fsize: usize) {
    let stride = factor * fsize * 2 / 96;
    for &swap_pair in SIPRO_SWAPS.iter() {
        let mut sidx = usize::from(swap_pair[0]) * stride;
        let mut didx = usize::from(swap_pair[1]) * stride;
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

struct AACInterleaver {
    frame_size: usize,
    data:       Vec<u8>,
    sizes:      Vec<usize>,
    full:       bool,
}
impl AACInterleaver {
    fn new(frame_size: usize) -> Self {
        Self {
            frame_size,
            data:   Vec::with_capacity(frame_size * 3 / 2),
            sizes:  Vec::with_capacity((frame_size / 128).max(8)),
            full:   false,
        }
    }
}
impl Interleaver for AACInterleaver {
    fn get_flavor(&self) -> usize { 0 }
    fn get_block_size(&self) -> usize { self.frame_size }
    fn get_factor(&self) -> usize { 1 }
    fn get_frames_per_block(&self) -> usize { 1 }
    fn is_empty(&self) -> bool { self.data.is_empty() }
    fn add_packet(&mut self, src: &[u8]) -> bool {
        if !self.full {
            self.data.extend_from_slice(src);
            self.sizes.push(src.len());
            if self.data.len() >= self.frame_size {
                self.full = true;
            }
            true
        } else {
            false
        }
    }
    fn get_packet(&mut self) -> Option<(Vec<u8>, bool)> {
        if self.full {
            let mut dst = Vec::with_capacity(self.frame_size);
            let mut gw = GrowableMemoryWriter::new_write(&mut dst);
            let mut bw = ByteWriter::new(&mut gw);
            bw.write_u16be((self.sizes.len() * 16) as u16).unwrap();
            for &pkt_size in self.sizes.iter() {
                bw.write_u16be(pkt_size as u16).unwrap();
            }
            bw.write_buf(&self.data).unwrap();

            self.data.clear();
            self.sizes.clear();
            self.full = false;

            Some((dst, true))
        } else {
            None
        }
    }
    fn flush(&mut self) {
        if !self.sizes.is_empty() {
            self.full = true;
        }
    }
}

struct InterleaveInfo {
    fcc:            [u8; 4],
    il_fcc:         [u8; 4],
    block_size:     usize,
    frame_size:     usize,
    factor:         usize,
    flavor:         usize,
}

struct AudioStreamWriter {
    fcc:            [u8; 4],
    il_fcc:         [u8; 4],
    is_raw:         bool,
    version:        u8,
    header_pos:     u64,
    interleave:     Box<dyn Interleaver>,
    data_size:      usize,
    first_time:     u32,
    last_time:      u32,
    size_in:        usize,
    size_out:       usize,
}

impl RMStreamWriter for AudioStreamWriter {
    fn write_header(&mut self, bw: &mut ByteWriter, astream: &NAStream) -> MuxerResult<()> {
        self.header_pos = bw.tell();
        if self.version < 6 {
            bw.write_buf(b".ra\xFD")?;
            bw.write_u16be(self.version.into())?;
        }

        let il_info = InterleaveInfo {
                fcc:        self.fcc,
                il_fcc:     self.il_fcc,
                block_size: self.interleave.get_block_size(),
                frame_size: self.interleave.get_frame_size(),
                factor:     self.interleave.get_factor(),
                flavor:     self.interleave.get_flavor(),
            };

        match self.version {
            3 => write_aformat3(bw, astream, &il_info)?,
            4 => write_aformat4(bw, astream, &il_info)?,
            5 => write_aformat5(bw, astream, &il_info)?,
            6 => write_lsd(bw, astream)?,
            _ => unreachable!(),
        }
        Ok(())
    }
    fn queue_packet(&mut self, pkt: NAPacket, ms: u32) -> bool {
        let src = pkt.get_buffer();
        self.data_size += src.len();
        let frame_size = self.interleave.get_frame_size();
        self.last_time = ms;
        if self.interleave.is_empty() {
            self.first_time = ms;
            self.size_in = 0;
            self.size_out = 0;
        } else {
            self.size_in += src.len() / frame_size;
        }
        if !self.is_raw {
            let mut ret = false;
            for frame in src.chunks(frame_size) {
                ret = self.interleave.add_packet(frame);
            }
            ret
        } else {
            self.interleave.add_packet(&src)
        }
    }
    fn get_packet(&mut self) -> Option<(Vec<u8>, u32, bool)> {
        if let Some((pkt, first)) = self.interleave.get_packet() {
            let time_add = if self.last_time > self.first_time && self.size_in > 0 {
                    let size = pkt.len();
                    let time_add = (self.size_out * ((self.last_time - self.first_time) as usize) / self.size_in) as u32;
                    self.size_out += size / self.interleave.get_frame_size();
                    time_add
                } else {
                    0
                };
            Some((pkt, self.first_time + time_add, first))
        } else {
            None
        }
    }
    fn flush(&mut self) { self.interleave.flush() }
    fn finish(&mut self, bw: &mut ByteWriter) -> MuxerResult<()> {
        let cur_pos = bw.tell();
        match self.version {
            3 => {
                bw.seek(SeekFrom::Start(self.header_pos + 18))?;
                bw.write_u32be(self.data_size as u32)?;
            },
            4 | 5 => {
                bw.seek(SeekFrom::Start(self.header_pos + 12))?;
                bw.write_u32be(self.data_size/*+header_size*/ as u32)?;
                bw.seek(SeekFrom::Current(12))?;
                bw.write_u32be(self.data_size as u32)?;
            },
            6 => unimplemented!(),
            _ => unreachable!(),
        };
        bw.seek(SeekFrom::Start(cur_pos))?;
        Ok(())
    }
    fn set_pkt_size(&mut self, _pkt_size: usize) {}
}

fn write_audio_metadata(bw: &mut ByteWriter) -> MuxerResult<()> {
    bw.write_byte(0)?; // title_string_length
    bw.write_byte(0)?; // author_string_length
    bw.write_byte(0)?; // copyright_string_length
    bw.write_byte(0)?; // user_string_length
    Ok(())
}

fn write_aformat3(bw: &mut ByteWriter, _stream: &NAStream, il_info: &InterleaveInfo) -> MuxerResult<()> {
    let start = bw.tell();
    bw.write_u16be(0)?; // header_bytes
    bw.write_u16be(il_info.flavor as u16)?;
    bw.write_u32be(il_info.frame_size as u32)?; // granularity
    bw.write_u32be(0)?; // bytes per minute
    bw.write_u32be(0)?; // total bytes
    write_audio_metadata(bw)?;
    bw.write_byte(0)?; //can_copy
    bw.write_byte(4)?; // FCC length
    bw.write_buf(&il_info.fcc)?;
    let end = bw.tell();
    bw.seek(SeekFrom::Start(start))?;
    bw.write_u16be((end - start - 2) as u16)?;
    bw.seek(SeekFrom::Start(end))?;

    Ok(())
}

fn write_aformat4(bw: &mut ByteWriter, stream: &NAStream, il_info: &InterleaveInfo) -> MuxerResult<()> {
    let info = stream.get_info().get_properties().get_audio_info().unwrap();

    bw.write_u16be(0)?;
    let start = bw.tell();
    bw.write_buf(b".ra4")?;
    bw.write_u32be(0)?; // data size
    bw.write_u16be(4)?; // version
    bw.write_u16be(0)?; // revision
    bw.write_u16be(0)?; // header_bytes
    bw.write_u16be(il_info.flavor as u16)?;
    bw.write_u32be(il_info.frame_size as u32)?; // granularity
    bw.write_u32be(0)?; // total bytes
    bw.write_u32be(0)?; // bytes_per_minute
    bw.write_u32be(0)?; // bytes_per_minute2
    bw.write_u16be(il_info.factor as u16)?;
    bw.write_u16be(il_info.block_size as u16)?;
    bw.write_u16be(0)?; // user data
    bw.write_u32be(info.sample_rate)?; // sample rate
    bw.write_u32be(info.format.bits.into())?; // sample size
    bw.write_u16be(info.channels.into())?; // num channels
    bw.write_byte(4)?; // ileave ID len
    bw.write_buf(&il_info.il_fcc)?;
    bw.write_byte(4)?; // codec ID len
    bw.write_buf(&il_info.fcc)?;
    bw.write_byte(if &il_info.il_fcc == b"Int0" { 0 } else { 1 })?;
    bw.write_byte(7)?; // can_copy
    bw.write_byte(0)?; // stream_type
    write_audio_metadata(bw)?;
    let end = bw.tell();
    bw.seek(SeekFrom::Start(start + 12))?;
    bw.write_u16be((end - start - 4) as u16)?;
    bw.seek(SeekFrom::Start(end))?;

    Ok(())
}

fn write_aformat5(bw: &mut ByteWriter, stream: &NAStream, il_info: &InterleaveInfo) -> MuxerResult<()> {
    let info = stream.get_info().get_properties().get_audio_info().unwrap();

    bw.write_u16be(0)?;
    let start = bw.tell();
    bw.write_buf(b".ra5")?;
    bw.write_u32be(0)?; // data size
    bw.write_u16be(5)?; // version
    bw.write_u16be(0)?; // revision
    bw.write_u16be(0)?; // header_bytes
    bw.write_u16be(il_info.flavor as u16)?;
    bw.write_u32be(il_info.block_size as u32)?; // granularity
    bw.write_u32be(0)?; // total bytes
    bw.write_u32be(0)?; // bytes_per_minute
    bw.write_u32be(0)?; // bytes_per_minute2
    bw.write_u16be(il_info.factor as u16)?;
    bw.write_u16be(il_info.block_size as u16)?;
    bw.write_u16be(il_info.frame_size as u16)?;
    bw.write_u16be(0)?; // user data
    bw.write_u32be(info.sample_rate)?; // sample rate
    bw.write_u32be(info.sample_rate)?; // actual sample rate
    bw.write_u32be(info.format.bits.into())?; // sample size
    bw.write_u16be(info.channels.into())?; // num channels
    bw.write_buf(&il_info.il_fcc)?;
    bw.write_buf(&il_info.fcc)?;
    bw.write_byte(if &il_info.il_fcc == b"Int0" { 0 } else { 1 })?;
    bw.write_byte(7)?; // can_copy
    bw.write_byte(0)?; // stream_type
    bw.write_byte(0)?; // has_interleave_pattern
    if let Some(edata) = stream.get_info().get_extradata() {
        if !matches!(&il_info.fcc, b"raac" | b"racp") {
            bw.write_u32be(edata.len() as u32)?;
            bw.write_buf(&edata)?;
        } else {
            bw.write_u32be((edata.len() + 1) as u32)?;
            bw.write_byte(2)?;
            bw.write_buf(&edata)?;
        }
    } else {
        bw.write_u32be(0)?;
    }
    let end = bw.tell();
    bw.seek(SeekFrom::Start(start + 12))?;
    bw.write_u16be((end - start - 4) as u16)?;
    bw.seek(SeekFrom::Start(end))?;

    Ok(())
}

fn write_lsd(bw: &mut ByteWriter, stream: &NAStream) -> MuxerResult<()> {
    if let Some(edata) = stream.get_info().get_extradata() {
        bw.write_buf(&edata)?;
    }
    Ok(())
}

fn create_interleaver(id: [u8; 4], fcc: [u8; 4], ainfo: NAAudioInfo) -> MuxerResult<Box<dyn Interleaver>> {
    let frame_size = ainfo.block_len;
    match &id {
        b"Int0" => Ok(Box::new(NoInterleaver{ frame_size, pkt: None })),
        b"Int4" => Ok(Box::new(Int4Interleaver::new(frame_size)?)),
        b"sipr" => Ok(Box::new(SiproInterleaver::new(frame_size)?)),
        b"genr" => Ok(Box::new(GenericInterleaver::new(ainfo, frame_size, fcc)?)),
        b"vbrs" | b"vbrf" => Ok(Box::new(AACInterleaver::new(1024))),
        _ => unimplemented!(),
    }
}

pub fn create_audio_stream(stream: &NAStream) -> MuxerResult<Box<dyn RMStreamWriter>> {
    let info = stream.get_info();
    let cname = info.get_name();
    let mut fourcc = [0u8; 4];
    let mut ileave = [0u8; 4];
    let mut version = 0;
    for &(fcc, name, ileaver, cversion) in AUDIO_CODEC_REGISTRY.iter() {
        if name == cname {
            fourcc = *fcc;
            ileave = *ileaver;
            version = cversion;
            break;
        }
    }
    if version > 0 {
        let ainfo = info.get_properties().get_audio_info().unwrap();
        Ok(Box::new(AudioStreamWriter {
            fcc:        fourcc,
            il_fcc:     ileave,
            is_raw:     &ileave == b"Int0",
            version,
            interleave: create_interleaver(ileave, fourcc, ainfo)?,
            header_pos: 0,
            data_size:  0,
            first_time: 0,
            last_time:  0,
            size_in:    0,
            size_out:   0,
        }))
    } else {
        Err(MuxerError::UnsupportedFormat)
    }
}

const SIPRO_SWAPS:   [[u8; 2]; 38] = [
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

macro_rules! int_params {
    ($rate:expr, $ch:expr, $bsize:expr, $factor:expr, $frm_per_blk:expr) => {
        InterleaveParams {
            sample_rate:    $rate,
            channels:       $ch,
            block_size:     $bsize,
            factor:         $factor,
            frames_per_blk: $frm_per_blk,
        }
    }
}

const RA_28_8_INTERLEAVE_PARAMS: &[InterleaveParams] = &[
    int_params!(0, 1, 228, 12, 6)
];
const SIPRO_INTERLEAVE_PARAMS: &[InterleaveParams] = &[ //2,1,3,0
    int_params!( 8000, 1, 232, 6, 16),
    int_params!( 8000, 1, 304, 6, 16),
    int_params!( 8000, 1, 296, 6, 16),
    int_params!(16000, 1, 320, 6, 16),
];
const ATRAC_INTERLEAVE_PARAMS: &[InterleaveParams] = &[
    int_params!(0, 0,  768, 20, 4),
    int_params!(0, 0, 1088, 20, 4),
    int_params!(0, 0,  912, 30, 3),
    int_params!(0, 0, 1152, 30, 3),
    int_params!(0, 0, 1272, 30, 3),
    int_params!(0, 0, 1024, 30, 2),
    int_params!(0, 0,  768, 10, 1),
    int_params!(0, 0, 1024, 10, 1),
];
const COOK_INTERLEAVE_PARAMS: &[InterleaveParams] = &[
    int_params!( 8000, 1,  288,  8,  9),
    int_params!(11025, 1,  352,  8, 11),
    int_params!(22050, 1,  564,  8, 12),
    int_params!(22050, 1,  600,  9, 10),
    int_params!(44100, 1,  651, 14,  7),
    int_params!(44100, 1,  640, 15,  5),
    int_params!(44100, 1,  744, 20,  4),
    int_params!(22050, 1,  558, 16,  6),
    int_params!( 8000, 1,  288,  6, 12),
    int_params!(11025, 2,  580, 10, 10),
    int_params!(22050, 2,  564, 14,  6),
    int_params!(22050, 2,  640, 16,  5),
    int_params!(44100, 2,  744, 20,  4),
    int_params!(44100, 2,  834, 30,  3),
    int_params!(44100, 1,  644, 20,  4),
    int_params!(44100, 1,  600,  9, 10),
    int_params!(44100, 1,  651, 14,  7),
    int_params!(22050, 2,  528, 10, 10),
    int_params!(22050, 2,  600, 10, 10),
    int_params!(22050, 2,  600, 10, 10),
    int_params!(22050, 2,  465, 16,  5),
    int_params!(44100, 2,  465, 16,  5),
    int_params!(44100, 2,  640, 16,  5),
    int_params!(44100, 2,  640, 16,  5),
    int_params!(44100, 2,  930, 16,  5),
    int_params!(44100, 2, 1400, 16,  5),
    int_params!(11025, 2,  376,  8, 11),
    int_params!(44100, 2,  930, 16,  5),
    int_params!(44100, 2, 1400, 16,  5),
    int_params!(22050, 2,  640, 16,  5),
    int_params!(44100, 5, 1395, 16,  5),
    int_params!(44100, 6, 1143, 10,  3),
    int_params!(44100, 6, 1064, 10,  2),
    int_params!(44100, 6,  778,  1,  1),
];
