use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::codecs::HAMShuffler;

const HEADER_SIZE: usize = 0x2F;

const MV_2BIT: [(i8, i8); 4] = [(-1, 0), (-1, -1), (1, -1), (0, -2)];
const MV_4BIT: [(i8, i8); 16] = [
    (-2, -3), ( 2, -3), (-1, -4), ( 1, -4),
    (-1, -2), ( 1, -2), ( 0, -3), ( 0, -4),
    (-2,  0), (-2, -1), ( 2, -1), (-2, -2),
    ( 2, -2), (-1, -3), ( 1, -3), ( 0, -5)
];

const BPP: usize = 4;

struct ArxelVideoDecoder {
    info:       NACodecInfoRef,
    tiles:      [u8; 65536],
    version:    u8,
    idx_buf:    Vec<usize>,
    contexts:   [[usize; 16]; 4096],
    ctx_pos:    [usize; 4096],
    hams:       HAMShuffler<u8>,
}

impl ArxelVideoDecoder {
    fn new() -> Self {
        Self {
            info:   NACodecInfoRef::default(),
            tiles:  [0; 65536],
            version:    0,
            idx_buf:    Vec::new(),
            contexts:   [[0; 16]; 4096],
            ctx_pos:    [0; 4096],
            hams:       HAMShuffler::new(),
        }
    }
    fn decode_v1(&mut self, src: &[u8]) -> DecoderResult<(NABufferType, bool)> {
        let mut mr = MemoryReader::new_read(src);
        let mut br = ByteReader::new(&mut mr);

        let size                            = br.read_u32le()? as usize;
        validate!(src.len() >= size + HEADER_SIZE);
        let part2_off                       = br.read_u32le()? as u64;
        validate!(part2_off > 0 && part2_off < (size as u64));
        let num_tiles                       = br.read_u16le()? as usize;
        validate!(num_tiles > 0 && num_tiles < 4096);
        let tile_size                       = br.read_u16le()? as usize;
        let width                           = br.read_u32le()? as usize;
        let height                          = br.read_u32le()? as usize;

        let vinfo = self.info.get_properties().get_video_info().unwrap();
        validate!(width == vinfo.get_width());
        validate!(height == vinfo.get_height());

                                              br.seek(SeekFrom::Start(part2_off + (HEADER_SIZE as u64)))?;
        let tile_w = if tile_size == 2 { 2 } else { 4 };
        let tsize = tile_w * BPP;

        match tile_size {
            2 | 4 => {
                                              br.read_buf(&mut self.tiles[..tsize])?;
                let off = br.tell() as usize;
                let mut bir = BitReader::new(&src[off..], BitReaderMode::BE);
                for tile in 1..num_tiles {
                    for i in 0..tsize {
                        self.tiles[tile * tsize + i] = self.tiles[tile * tsize + i - tsize];
                    }
                    let bits                = bir.read(3)? as u8 + 1;
                    validate!(bits < 8);
                    for el in self.tiles[tile * tsize..][..tsize].iter_mut() {
                        let mut delta       = bir.read(bits)? as i16;
                        if delta != 0 && bir.read_bool()? {
                            delta = -delta;
                        }
                        *el = (i16::from(*el) + delta) as u8;
                    }
                }
            },
            _ => {
                validate!(tile_size == num_tiles * tsize);
                                              br.read_buf(&mut self.tiles[..tile_size])?;
            },
        };

        let bufinfo = alloc_video_buffer(vinfo, 0)?;
        let bufo = bufinfo.get_vbuf();
        let mut buf = bufo.unwrap();
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        let mut br = BitReader::new(&src[HEADER_SIZE..], BitReaderMode::BE);
        let idx_bits = if num_tiles < 0x400 { 10 } else if num_tiles < 0x800 { 11 } else { 12 };
        for y in (0..height).step_by(2) {
            for x in (0..width).step_by(tile_w) {
                let dst_pos = x * BPP + y * stride;
                if !br.read_bool()? {
                    let idx             = br.read(idx_bits)? as usize;
                    validate!(idx < num_tiles);
                    dst[dst_pos..][..tsize].copy_from_slice(&self.tiles[idx * tsize..][..tsize]);
                } else {
                    let (mv_x, mv_y) = if br.read_bool()? {
                            (0, -1)
                        } else if br.read_bool()? {
                            MV_2BIT[br.read(2)? as usize]
                        } else {
                            MV_4BIT[br.read(4)? as usize]
                        };

                    let isrc = (dst_pos as isize) + isize::from(mv_x) * (tsize as isize) + isize::from(mv_y) * ((stride * 2) as isize);
                    validate!(isrc >= 0);
                    let src_pos = isrc as usize;
                    validate!(src_pos + tsize <= dst.len());
                    let (src, dst) = dst.split_at_mut(dst_pos);
                    dst[..tsize].copy_from_slice(&src[src_pos..][..tsize]);
                }
            }
            // double lines
            let lines = &mut dst[y * stride..];
            let (src, dst) = lines.split_at_mut(stride);
            dst[..stride].copy_from_slice(src);
        }
        Ok((bufinfo, true))
    }
    fn add_to_context(&mut self, prev: usize, cur: usize) {
        self.contexts[prev][self.ctx_pos[prev]] = cur;
        self.ctx_pos[prev] += 1;
        if self.ctx_pos[prev] == 16 {
            self.ctx_pos[prev] = 0;
        }
    }
    fn decode_v2(&mut self, src: &[u8]) -> DecoderResult<(NABufferType, bool)> {
        let mut mr = MemoryReader::new_read(src);
        let mut br = ByteReader::new(&mut mr);

        let mut has_tiles = false;
        let mut is_55 = false;
        loop {
            let ftype                       = br.read_byte()?;
            match ftype {
                0x54 => {
                    let size                = br.read_u32le()? as usize;
                    let num_tiles           = br.read_u16le()? as usize;
                    let tile_size           = br.read_u16le()? as usize;

                    let tile_w = if tile_size == 2 { 2 } else { 4 };
                    let tsize = tile_w * BPP;

                    match tile_size {
                        2 | 4 => {
                            validate!(size >= tsize);
                                                          br.read_buf(&mut self.tiles[..tsize])?;
                            let off = br.tell() as usize;
                            let mut bir = BitReader::new(&src[off..][..size - tsize], BitReaderMode::LE);
                            for tile in 1..num_tiles {
                                let (prev_tiles, cur_tile) = self.tiles.split_at_mut(tile * tsize);
                                cur_tile[..16].copy_from_slice(&prev_tiles[prev_tiles.len() - 16..]);
                                for comp in 0..BPP {
                                    let bits            = bir.read(3)? as u8;
                                    if bits == 0 {
                                        continue;
                                    }
                                    for i in 0..tile_size {
                                        let el = &mut cur_tile[i * BPP + comp];
                                        *el = match bits {
                                                7 => {
                                                    bir.read(8)? as u8
                                                },
                                                _ => {
                                                    let mut delta       = bir.read(bits)? as i16;
                                                    if delta != 0 && bir.read_bool()? {
                                                        delta = -delta;
                                                    }
                                                    (i16::from(*el) + delta) as u8
                                                },
                                            };
                                    }
                                }
                            }
                                              br.read_skip(size - tsize)?;
                            has_tiles = true;
                        },
                        _ => {
                            unimplemented!();
                        },
                    };
                },
                0x53 => break,
                0x55 => {
                    is_55 = true;
                    break;
                },
                _ => return Err(DecoderError::InvalidData),
            };
        }

        let size                            = br.read_u32le()? as usize;
        validate!(size + HEADER_SIZE <= (br.left() as usize) + 4);
        let part2_off                       = br.read_u32le()?;
        validate!(part2_off as usize == size);
        let num_tiles                       = br.read_u16le()? as usize;
        validate!((0..4096).contains(&num_tiles));
        let tile_size                       = br.read_u16le()? as usize;
        let width                           = br.read_u32le()? as usize;
        let height                          = br.read_u32le()? as usize;
                                              br.read_skip(0x1B)?;

        let vinfo = self.info.get_properties().get_video_info().unwrap();
        validate!(width == vinfo.get_width());
        validate!(height == vinfo.get_height());
        let is_intra = is_55 && has_tiles;

        let mut vbuf = if is_intra {
                let binfo = alloc_video_buffer(vinfo, 0)?;
                let vbuf = binfo.get_vbuf().unwrap();
                self.hams.add_frame(vbuf);
                self.hams.get_output_frame().unwrap()
            } else {
                if let Some(buf) = self.hams.clone_ref() {
                    buf
                } else {
                    return Err(DecoderError::MissingReference);
                }
            };
        let stride = vbuf.get_stride(0);
        let data = vbuf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        let tile_w = if tile_size == 2 { 2 } else { 4 };
        let tsize = tile_w * BPP;
        let mut idx_bits = 0;
        let mut v = num_tiles;
        while v > 0 {
            idx_bits += 1;
            v >>= 1;
        }
        let start = br.tell() as usize;
        let mut br = BitReader::new(&src[start..], BitReaderMode::LE);
        let mut ypos = 0;
        let mut last_seen = [0usize.wrapping_sub(1); 4096];
        let mut cand_list = Vec::with_capacity(4);
        let istride = width / tile_w;
        self.idx_buf.resize(istride * height, 0);
        self.contexts = [[0; 16]; 4096];
        self.ctx_pos  = [0; 4096];

        for y in 0..height {
            for x8 in (0..istride).step_by(8) {
                let pos = ypos + x8;
                if br.read_bool()? {
                    validate!(y > 0);
                    for x in 0..8 {
                        self.idx_buf[pos + x] = self.idx_buf[pos + x - istride];
                    }
                } else {
                    for x in 0..8 {
                        if br.read_bool()? {
                            validate!(y > 0);
                            self.idx_buf[pos + x] = self.idx_buf[pos + x - istride];
                        } else {
                            let mode = br.read(2)?;
                            match mode {
                                0 => {
                                    let idx = br.read(idx_bits)? as usize;
                                    self.idx_buf[pos + x] = idx;
                                    if y > 0 {
                                        self.add_to_context(self.idx_buf[pos + x - istride], idx);
                                    }
                                },
                                1 => {
                                    cand_list.clear();
                                    let cur_pos = pos + x;
                                    if y > 0 {
                                        last_seen[self.idx_buf[cur_pos - istride]] = cur_pos;
                                    }
                                    if x8 + x > 0 {
                                        let src_idx = cur_pos - 1;
                                        if last_seen[self.idx_buf[src_idx]] != cur_pos {
                                            cand_list.push(self.idx_buf[src_idx]);
                                            last_seen[self.idx_buf[src_idx]] = cur_pos;
                                        }
                                    }
                                    if (y > 0) && (x8 + x > 0) {
                                        let src_idx = cur_pos - 1 - istride;
                                        if last_seen[self.idx_buf[src_idx]] != cur_pos {
                                            cand_list.push(self.idx_buf[src_idx]);
                                            last_seen[self.idx_buf[src_idx]] = cur_pos;
                                        }
                                    }
                                    if (y > 0) && (x8 + x + 1 < istride) {
                                        let src_idx = cur_pos + 1 - istride;
                                        if last_seen[self.idx_buf[src_idx]] != cur_pos {
                                            cand_list.push(self.idx_buf[src_idx]);
                                            last_seen[self.idx_buf[src_idx]] = cur_pos;
                                        }
                                    }
                                    if y > 1 {
                                        let src_idx = cur_pos - 2 * istride;
                                        if last_seen[self.idx_buf[src_idx]] != cur_pos {
                                            cand_list.push(self.idx_buf[src_idx]);
                                            last_seen[self.idx_buf[src_idx]] = cur_pos;
                                        }
                                    }

                                    validate!(!cand_list.is_empty());
                                    self.idx_buf[cur_pos] = match cand_list.len() {
                                            1 => cand_list[0],
                                            2 => cand_list[br.read(1)? as usize],
                                            _ => {
                                                let idx = br.read(2)? as usize;
                                                validate!(idx < cand_list.len());
                                                cand_list[idx]
                                            },
                                        };
                                    if y > 0 {
                                        self.add_to_context(self.idx_buf[cur_pos - istride], self.idx_buf[cur_pos]);
                                    }
                                },
                                2 => {
                                    validate!(y > 0);
                                    let top_idx = self.idx_buf[pos + x - istride];
                                    let delta = br.read(4)? as usize + 1;
                                    self.idx_buf[pos + x] = if !br.read_bool()? {
                                            validate!(top_idx + delta < num_tiles);
                                            top_idx + delta
                                        } else {
                                            validate!(top_idx >= delta);
                                            top_idx - delta
                                        };
                                    if y > 0 {
                                        self.add_to_context(self.idx_buf[pos + x - istride], self.idx_buf[pos + x]);
                                    }
                                },
                                _ => {
                                    validate!(y > 0);
                                    let idx = br.read(4)? as usize;
                                    self.idx_buf[pos + x] = self.contexts[self.idx_buf[pos + x - istride]][idx];
                                },
                            }
                        }
                    }
                }
            }
            ypos += istride;
        }

        for (dline, sline) in dst.chunks_mut(stride).take(height).zip(self.idx_buf.chunks_exact(istride)) {
            for (dst, &idx) in dline.chunks_exact_mut(tsize).zip(sline.iter()) {
                if idx != 0 || is_intra {
                    dst.copy_from_slice(&self.tiles[idx * tsize..][..tsize]);
                }
            }
        }

        Ok((NABufferType::Video(vbuf), is_intra))
    }
}

const RGBA_FORMAT: NAPixelFormaton = NAPixelFormaton {
        model: ColorModel::RGB(RGBSubmodel::RGB), components: 4,
        comp_info: [
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 2, next_elem: 4 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 1, next_elem: 4 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 0, next_elem: 4 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 8, shift: 0, comp_offs: 3, next_elem: 4 }),
            None ],
        elem_size: 4, be: false, alpha: true, palette: false };

impl NADecoder for ArxelVideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), true, RGBA_FORMAT));
            if let Some(edata) = info.get_extradata() {
                validate!(!edata.is_empty());
                if edata[0] > 1 {
                    return Err(DecoderError::NotImplemented);
                }
                self.version = edata[0];
            }
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > HEADER_SIZE);

        let (bufinfo, is_intra) = match self.version {
                0 => self.decode_v1(&src)?,
                1 => self.decode_v2(&src)?,
                _ => return Err(DecoderError::NotImplemented),
            };

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.hams.clear();
    }
}

impl NAOptionHandler for ArxelVideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(ArxelVideoDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    // sample from the Ring game
    #[test]
    fn test_arxel_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("arxel-cnm", "arxel-video", "assets/Game/logo.cnm", Some(10), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x9b1fc970, 0x1fe86e2c, 0x44dd9255, 0x3920c49b]));
    }
    // sample from Faust: The Seven Games of the Soul game
    #[test]
    fn test_arxel_video_v2() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("arxel-cnm", "arxel-video", "assets/Game/logo.CI2", Some(10), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x3bf66a39, 0x6627f529, 0x4ed19e8e, 0xc0693aae]));
    }
}
