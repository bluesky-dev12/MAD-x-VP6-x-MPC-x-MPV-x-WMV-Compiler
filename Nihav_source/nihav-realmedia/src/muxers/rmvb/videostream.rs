use std::collections::VecDeque;
use nihav_core::frame::*;
use nihav_core::muxers::*;
use super::RMStreamWriter;

static VIDEO_CODEC_REGISTRY: &[(&[u8;4], &str)] = &[
    (b"RV10", "realvideo1"),
    (b"RV20", "realvideo2"),
    (b"RVTR", "realvideo2"),
    (b"RV30", "realvideo3"),
    (b"RV40", "realvideo4"),
    (b"RV60", "realvideo6"),
    (b"CLV1", "clearvideo_rm"),
];

pub struct DummyStreamWriter {}
impl RMStreamWriter for DummyStreamWriter {
    fn write_header(&mut self, _bw: &mut ByteWriter, _stream: &NAStream) -> MuxerResult<()> {
        Ok(())
    }
    fn queue_packet(&mut self, _pkt: NAPacket, _ms: u32) -> bool {
        true
    }
    fn get_packet(&mut self) -> Option<(Vec<u8>, u32, bool)> {
        None
    }
    fn flush(&mut self) { }
    fn finish(&mut self, _bw: &mut ByteWriter) -> MuxerResult<()> {
        Ok(())
    }
    fn set_pkt_size(&mut self, _pkt_size: usize) {}
}

#[derive(Clone,Copy)]
enum VideoDataType {
    Frame,
    Slice{pkt_no: u8, npkt: u8, full_size: u32, offset: u32},
}

impl VideoDataType {
    fn is_frame(self) -> bool { matches!(self, VideoDataType::Frame) }
}

fn val_to_size(val: u32) -> usize { if val < (1 << 14) { 2 } else { 4 } }

#[derive(Clone)]
struct VideoData {
    vtype:      VideoDataType,
    pts:        u32,
    seq_no:     u8,
    data:       Vec<u8>,
}

impl VideoData {
    fn get_pkt_len(&self) -> usize {
        let plen = self.data.len();
        let ts_size = val_to_size(self.pts);
        match self.vtype {
            VideoDataType::Frame => plen + val_to_size(plen as u32) + ts_size + 2,
            VideoDataType::Slice{pkt_no: _, npkt: _, full_size, offset} => plen + val_to_size(full_size) + val_to_size(offset) + 3,
        }
    }
}

struct VideoStreamWriter {
    fcc:        [u8; 4],
    seq_no:     u8,
    time:       u32,
    mi_time:    u32,
    pkt_size:   usize,
    queue:      VecDeque<VideoData>,
    flush:      bool,
}

impl RMStreamWriter for VideoStreamWriter {
    fn write_header(&mut self, bw: &mut ByteWriter, vstream: &NAStream) -> MuxerResult<()> {
        let info = vstream.get_info().get_properties().get_video_info().unwrap();
        let start = bw.tell();

        bw.write_u32be(0)?; // header size
        bw.write_buf(b"VIDO")?;
        bw.write_buf(&self.fcc)?;
        bw.write_u16be(info.width as u16)?;
        bw.write_u16be(info.height as u16)?;
        bw.write_u16be(12)?; // bpp
        bw.write_u16be(0)?; // aligned width
        bw.write_u16be(0)?; // aligned height
        let (tb_num, tb_den) = vstream.get_timebase();
        if tb_num != 0 && tb_den != 0 {
            bw.write_u16be((tb_den / tb_num) as u16)?;
            let mut fps_frac = tb_den % tb_num;
            let mut div = tb_num;
            while div >= 0x10000 {
                fps_frac >>= 1;
                div      >>= 1;
            }
            fps_frac = (fps_frac << 16) / div;
            bw.write_u16le(fps_frac as u16)?;
        } else {
            bw.write_u16be(0)?;
            bw.write_u16be(0)?;
        }

        if let Some(edata) = vstream.get_info().get_extradata() {
            bw.write_buf(&edata)?;
        }
        let end = bw.tell();
        bw.seek(SeekFrom::Start(start))?;
        bw.write_u32be((end - start) as u32)?;
        bw.seek(SeekFrom::Start(end))?;
        Ok(())
    }
    fn queue_packet(&mut self, pkt: NAPacket, ms: u32) -> bool {
        let tot_size = self.queue.iter().fold(0usize, |acc, q| acc + q.get_pkt_len());
        if tot_size > self.pkt_size {
            return false;
        }

        self.time = ms;
        if ms > 0 {
            self.mi_time = ms.max(self.mi_time + 1);
        }

        let src = pkt.get_buffer();
        let nslices = usize::from(src[0]) + 1;
        let hdr_size = nslices * 8 + 1;

        if nslices == 1 {
            self.queue.push_back(VideoData {
                    vtype:      VideoDataType::Frame,
                    pts:        self.mi_time,
                    seq_no:     self.seq_no,
                    data:       src[9..].to_vec(),
                });
        } else if src.len() > hdr_size {
            let mut slice_sizes = [0; 256];
            let mut slice_offs  = [0; 256];

            for (el, src) in slice_offs.iter_mut().zip(src[1..].chunks_exact(8)) {
                *el = read_u32be(&src[4..]).unwrap() as usize;
            }
            for (dst, offs) in slice_sizes[..nslices - 1].iter_mut().zip(slice_offs.windows(2)) {
                *dst = offs[1] - offs[0];
            }
            slice_sizes[nslices - 1] = src.len() - hdr_size - slice_offs[nslices - 1];

            let src = &src[hdr_size..];
            let full_size = src.len() as u32;
            let npkt = nslices as u8;
            for (pkt_no, (&offset, &size)) in slice_offs.iter().zip(slice_sizes.iter()).take(nslices).enumerate() {
                let vtype = VideoDataType::Slice{pkt_no: (pkt_no + 1) as u8, npkt, full_size, offset: offset as u32};
                self.queue.push_back(VideoData {
                        vtype,
                        pts:        self.mi_time,
                        seq_no:     self.seq_no,
                        data:       src[offset..][..size].to_vec(),
                    });
            }
        }

        self.seq_no = self.seq_no.wrapping_add(1);

        true
    }
    fn get_packet(&mut self) -> Option<(Vec<u8>, u32, bool)> {
        if self.queue.is_empty() {
            return None;
        }
        let tot_size = self.queue.iter().fold(0usize, |acc, q| acc + q.get_pkt_len());
        if tot_size < self.pkt_size && !self.flush {
            return None;
        }
        let mut pkt_buf = Vec::new();

        let first = self.queue.pop_front().unwrap();
        let is_first = match first.vtype {
                VideoDataType::Frame => true,
                VideoDataType::Slice{pkt_no, npkt: _, full_size: _, offset: _} => pkt_no == 1,
            };
        if self.queue.is_empty() || (first.get_pkt_len() + self.queue[0].get_pkt_len() + 4 > self.pkt_size) {
            match first.vtype {
                VideoDataType::Frame => {
                    pkt_buf.push(0x40); // 0x1 = whole frame
                    pkt_buf.push(first.seq_no);
                    pkt_buf.extend_from_slice(&first.data);
                },
                VideoDataType::Slice{pkt_no, npkt, full_size: _, offset: _} => {
                    let id = if pkt_no == npkt { 2 } else { 0 };
                    write_slice(&mut pkt_buf, id, &first);
                },
            };
        } else {
            let second = &self.queue[0];
            match (first.vtype.is_frame(), second.vtype.is_frame()) {
                (true, true) => {
                    write_multiple_frame(&mut pkt_buf, &first);
                    while !self.queue.is_empty() && self.queue[0].vtype.is_frame() && (pkt_buf.len() + self.queue[0].get_pkt_len() < self.pkt_size) {
                        let frm = self.queue.pop_front().unwrap();
                        write_multiple_frame(&mut pkt_buf, &frm);
                    }
                },
                (true, false) => {
                    pkt_buf.push(0x40); // 0x1 = whole frame
                    pkt_buf.push(first.seq_no);
                    pkt_buf.extend_from_slice(&first.data);
                },
                (false, true) => {
                    write_slice(&mut pkt_buf, 2, &first);
                    while !self.queue.is_empty() && self.queue[0].vtype.is_frame() && (pkt_buf.len() + self.queue[0].get_pkt_len() < self.pkt_size) {
                        let frm = self.queue.pop_front().unwrap();
                        write_multiple_frame(&mut pkt_buf, &frm);
                    }
                },
                (false, false) => {
                    if let VideoDataType::Slice{pkt_no, npkt, full_size: _, offset: _} = first.vtype {
                       let id = if pkt_no == npkt { 2 } else { 0 };
                       write_slice(&mut pkt_buf, id, &first);
                    } else {
                        unreachable!()
                    }
                },
            };
        }
        Some((pkt_buf, first.pts, is_first))
    }
    fn flush(&mut self) {
        self.flush = true;
    }
    fn finish(&mut self, _bw: &mut ByteWriter) -> MuxerResult<()> {
        Ok(())
    }
    fn set_pkt_size(&mut self, pkt_size: usize) {
        self.pkt_size = pkt_size;
    }
}

fn write_16_or_32(dst: &mut Vec<u8>, val: u32) {
    if val < (1 << 14) {
        dst.push((1 << 6) | ((val >> 8) as u8));
        dst.push(val as u8);
    } else {
        dst.push((val >> 24) as u8);
        dst.push((val >> 16) as u8);
        dst.push((val >>  8) as u8);
        dst.push( val        as u8);
    }
}

fn write_multiple_frame(dst: &mut Vec<u8>, frm: &VideoData) {
    dst.push(0xC0); // 0x3 = multiple frame
    write_16_or_32(dst, frm.data.len() as u32);
    write_16_or_32(dst, frm.pts);
    dst.push(frm.seq_no);
    dst.extend_from_slice(&frm.data);
}

fn write_slice(dst: &mut Vec<u8>, id: u8, src: &VideoData) {
    if let VideoDataType::Slice{pkt_no, npkt, full_size, offset} = src.vtype {
        dst.push((id << 6) | (npkt >> 1));
        dst.push((npkt << 7) | pkt_no);
        write_16_or_32(dst, full_size);
        if id == 0 {
            write_16_or_32(dst, offset);
        } else {
            write_16_or_32(dst, src.data.len() as u32);
        }
        dst.push(src.seq_no);
        dst.extend_from_slice(&src.data);
    } else {
        unreachable!()
    }
}

pub fn create_video_stream(stream: &NAStream, pkt_size: usize) -> MuxerResult<Box<dyn RMStreamWriter>> {
    let info = stream.get_info();
    let cname = info.get_name();

    for &(fcc, name) in VIDEO_CODEC_REGISTRY.iter() {
        if name == cname {
            return Ok(Box::new(VideoStreamWriter {
                    fcc:        *fcc,
                    seq_no:     0,
                    time:       0,
                    mi_time:    0,
                    pkt_size,
                    queue:      VecDeque::new(),
                    flush:      false,
                }));
        }
    }
    Err(MuxerError::UnsupportedFormat)
}
