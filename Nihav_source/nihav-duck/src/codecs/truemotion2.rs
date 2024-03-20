use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;

#[repr(u8)]
enum TM2StreamType {
    CHigh = 0,
    CLow,
    LHigh,
    LLow,
    Update,
    Motion,
    BlockType,
    Num
}

#[repr(u8)]
#[derive(Debug,Clone,Copy)]
enum TM2BlockType {
    HiRes,
    MedRes,
    LowRes,
    NullRes,
    Update,
    Still,
    Motion
}

const TM2_BLOCK_TYPES: [TM2BlockType; 7] = [
    TM2BlockType::HiRes,  TM2BlockType::MedRes, TM2BlockType::LowRes, TM2BlockType::NullRes,
    TM2BlockType::Update, TM2BlockType::Still,  TM2BlockType::Motion
];

trait ReadLenEsc {
    fn read_len_esc(&mut self) -> DecoderResult<usize>;
}

const TM2_ESCAPE: usize = 0x80000000;

impl<'a> ReadLenEsc for ByteReader<'a> {
    fn read_len_esc(&mut self) -> DecoderResult<usize> {
        let len                                 = self.read_u32le()? as usize;
        if len == TM2_ESCAPE {
            let len2                            = self.read_u32le()? as usize;
            Ok(len2)
        } else {
            Ok(len)
        }
    }
}

struct HuffDef {
    val_bits:   u8,
    max_bits:   u8,
    nelems:     usize,
}

impl HuffDef {
    fn read(&mut self, br: &mut BitReader, codes: &mut Vec<FullCodebookDesc<u8>>, prefix: u32, len: u8) -> DecoderResult<()> {
        validate!(len <= self.max_bits);
        if !br.read_bool()? {
            validate!(codes.len() < self.nelems);
            let sym                             = br.read(self.val_bits)? as u8;
            codes.push(FullCodebookDesc { code: prefix, bits: len, sym });
        } else {
            self.read(br, codes, (prefix << 1) | 0, len + 1)?;
            self.read(br, codes, (prefix << 1) | 1, len + 1)?;
        }
        Ok(())
    }
}

struct HuffTree {
    cb:         Option<Codebook<u8>>,
    sym0:       u8,
}

impl HuffTree {
    fn new() -> Self {
        Self { cb: None, sym0: 0 }
    }
}

const TM2_MAX_DELTAS: usize = 64;

struct TM2Stream {
    tokens:     Vec<u8>,
    deltas:     [i32; TM2_MAX_DELTAS],
    pos:        usize,
}

impl Default for TM2Stream {
    fn default() -> Self {
        Self {
            tokens:     Vec::new(),
            deltas:     [0; TM2_MAX_DELTAS],
            pos:        0,
        }
    }
}

impl TM2Stream {
    fn read_header(&mut self, src: &[u8], br: &mut ByteReader) -> DecoderResult<()> {
        self.tokens.clear();
        self.pos = 0;

        let len                                 = br.read_u32le()? as usize;
        let endpos = br.tell() + (len as u64) * 4;
        if len == 0 {
            return Ok(());
        }
        let ntoks                               = br.read_u32le()? as usize;
        validate!(ntoks < (1 << 24));
        if (ntoks & 1) != 0 {
            let dlen                            = br.read_len_esc()?;
            if (dlen as i32) > 0 {
                let rest_size = (endpos - br.tell()) as usize;
                let skip_size = self.read_deltas(&src[br.tell() as usize..][..rest_size])?;
                validate!(skip_size == dlen * 4);
                                                  br.read_skip(skip_size)?;
            }
        }
        let _len                                = br.read_len_esc()?;
        let _algo                               = br.read_u32le()?;

        let mut htree = HuffTree::new();
        let rest_size = (endpos - br.tell()) as usize;
        let skip_size = self.read_huff_tree(&src[br.tell() as usize..][..rest_size], &mut htree)?;
                                                  br.read_skip(skip_size)?;

        let len                                 = br.read_u32le()? as usize;
        validate!(br.tell() + (len as u64) * 4 <= endpos);
        if len > 0 {
            self.tokens.reserve(ntoks >> 1);
            let rest_size = (endpos - br.tell()) as usize;
            let skip_size = self.read_tokens(&src[br.tell() as usize..][..rest_size], &htree, ntoks >> 1)?;
                                                  br.read_skip(skip_size)?;
        } else {
            self.tokens.resize(ntoks >> 1, htree.sym0);
        }


        let pos = br.tell();
        validate!(pos <= endpos);
        let toskip = endpos - pos;
                                                  br.read_skip(toskip as usize)?;

        Ok(())
    }
    fn read_deltas(&mut self, src: &[u8]) -> DecoderResult<usize> {
        let mut br = BitReader::new(src, BitReaderMode::LE32MSB);
        let coded_deltas                        = br.read(9)? as usize;
        let bits                                = br.read(5)? as u8;
        validate!((coded_deltas <= TM2_MAX_DELTAS) && (bits > 0));
        let mask = 1 << (bits - 1);
        let bias = 1 << bits;
        self.deltas = [0; TM2_MAX_DELTAS];
        for i in 0..coded_deltas {
            let val                             = br.read(bits)?;
            if (val & mask) != 0 {
                self.deltas[i] = (val as i32) - bias;
            } else {
                self.deltas[i] = val as i32;
            }
        }

        Ok(((br.tell() + 31) >> 5) << 2)
    }
    fn read_huff_tree(&mut self, src: &[u8], htree: &mut HuffTree) -> DecoderResult<usize> {
        let mut br = BitReader::new(src, BitReaderMode::LE32MSB);

        let val_bits                            = br.read(5)? as u8;
        let max_bits                            = br.read(5)? as u8;
        let min_bits                            = br.read(5)? as u8;
        let nelems                              = br.read(17)? as usize;
        validate!(val_bits > 0 && val_bits <= 6);
        validate!(nelems > 0);
        validate!((max_bits < 25) && (min_bits <= max_bits));

        let mut codes: Vec<FullCodebookDesc<u8>> = Vec::with_capacity(nelems);
        let mut hdef = HuffDef { val_bits, max_bits, nelems };
        hdef.read(&mut br, &mut codes, 0, 0)?;
        htree.sym0 = codes[0].sym;
        if nelems > 1 {
            let mut cr = FullCodebookDescReader::new(codes);
            htree.cb = Some(Codebook::new(&mut cr, CodebookMode::MSB)?);
        }

        Ok(((br.tell() + 31) >> 5) << 2)
    }
    fn read_tokens(&mut self, src: &[u8], htree: &HuffTree, ntoks: usize) -> DecoderResult<usize> {
        let mut br = BitReader::new(src, BitReaderMode::LE32MSB);

        if let Some(ref cb) = htree.cb {
            for _ in 0..ntoks {
                let tok                         = br.read_cb(cb)?;
                self.tokens.push(tok);
            }
        }

        Ok(((br.tell() + 31) >> 5) << 2)
    }

    fn get_block_type(&mut self) -> DecoderResult<u8> {
        validate!(self.pos < self.tokens.len());
        let res = self.tokens[self.pos];
        self.pos += 1;
        Ok(res)
    }
    fn get_token(&mut self) -> DecoderResult<i32> {
        validate!(self.pos < self.tokens.len());
        let idx = self.tokens[self.pos] as usize;
        validate!(idx < TM2_MAX_DELTAS);
        self.pos += 1;
        Ok(self.deltas[idx])
    }
}

#[derive(Default)]
struct DeltaState {
    dy: [i32; 4],
    dc: [[i32; 2]; 2],
}

#[allow(clippy::erasing_op)]
impl DeltaState {
    fn apply_y(&mut self, dst: &mut [u8], mut yoff: usize, ystride: usize, ydeltas: &[i32; 16], last: &mut [i32]) {
        for y in 0..4 {
            let mut d = self.dy[y];
            for x in 0..4 {
                d += ydeltas[x + y * 4];
                last[x] += d;
                dst[yoff + x] = last[x].max(0).min(255) as u8;
            }
            self.dy[y] = d;
            yoff += ystride;
        }
    }
    fn apply_c(&mut self, dst: &mut [i16], mut coff: usize, cstride: usize, cdeltas: &[i32; 4], idx: usize, last: &mut [i32]) {
        for y in 0..2 {
            let mut d = self.dc[idx][y];
            for x in 0..2 {
                d += cdeltas[x + y * 2];
                last[x] += d;
                dst[coff + x] = last[x] as i16;
            }
            self.dc[idx][y] = d;
            coff += cstride;
        }
    }
    fn interpolate_y_low(&mut self, last: &mut [i32]) {
        let dsum = self.dy[0] + self.dy[1] + self.dy[2] + self.dy[3];
        last[1] = (last[0] - dsum + last[2]) >> 1;
        last[3] = (last[2] + last[4]) >> 1;

        let t0 = self.dy[0] + self.dy[1];
        let t1 = self.dy[2] + self.dy[3];
        self.dy[0] = t0 >> 1;
        self.dy[1] = t0 - (t0 >> 1);
        self.dy[2] = t1 >> 1;
        self.dy[3] = t1 - (t1 >> 1);
    }
    fn interpolate_y_null(&mut self, last: &mut [i32]) {
        let dsum = self.dy[0] + self.dy[1] + self.dy[2] + self.dy[3];
        let left = last[0] - dsum;
        let right = last[4];
        let diff = right - left;
        last[1] = left + (diff >> 2);
        last[2] = left + (diff >> 1);
        last[3] = right - (diff >> 2);

        let mut sum = left;
        self.dy[0] = (left + (dsum >> 2)) - sum;
        sum += self.dy[0];
        self.dy[1] = (left + (dsum >> 1)) - sum;
        sum += self.dy[1];
        self.dy[2] = (left + dsum - (dsum >> 2)) - sum;
        sum += self.dy[2];
        self.dy[3] = (left + dsum) - sum;
    }
    fn interpolate_c(&mut self, idx: usize, last: &mut [i32]) {
        let dsum = self.dc[idx][0] + self.dc[idx][1];
        let l = (last[0] + last[2] - dsum) >> 1;
        self.dc[idx][0] = dsum >> 1;
        self.dc[idx][1] = dsum - (dsum >> 1);
        last[1] = l;
    }
    fn recalc_y(&mut self, dst: &[u8], yoff: usize, ystride: usize, last: &mut [i32]) {
        let src = &dst[yoff+3..];
        self.dy[0] = i32::from(src[ystride * 0]) - last[3];
        self.dy[1] = i32::from(src[ystride * 1]) - i32::from(src[ystride * 0]);
        self.dy[2] = i32::from(src[ystride * 2]) - i32::from(src[ystride * 1]);
        self.dy[3] = i32::from(src[ystride * 3]) - i32::from(src[ystride * 2]);
        let src = &dst[yoff + 3 * ystride..];
        for x in 0..4 {
            last[x] = i32::from(src[x]);
        }
    }
    fn recalc_c(&mut self, dst: &[i16], coff: usize, cstride: usize, idx: usize, last: &mut [i32]) {
        self.dc[idx][0] = i32::from(dst[coff + 1]) - last[1];
        self.dc[idx][1] = i32::from(dst[coff + 1 + cstride]) - i32::from(dst[coff + 1]);
        last[0] = i32::from(dst[coff + cstride + 0]);
        last[1] = i32::from(dst[coff + cstride + 1]);
    }
}

#[derive(Default)]
struct TM2Frame {
    ydata:      Vec<u8>,
    udata:      Vec<i16>,
    vdata:      Vec<i16>,
    ystride:    usize,
    cstride:    usize,
}

impl TM2Frame {
    fn alloc(width: usize, height: usize) -> Self {
        let ystride = (width + 3) & !3;
        let ysize = ystride * ((height + 3) & !3);
        let cstride = ystride >> 1;
        let csize = cstride * (((height + 3) & !3) >> 1);
        Self { ydata: vec![0; ysize], udata: vec![0; csize], vdata: vec![0; csize], ystride, cstride }
    }
}

#[derive(Default)]
struct TM2Decoder {
    info:       NACodecInfoRef,
    streams:    [TM2Stream; TM2StreamType::Num as usize],
    width:      usize,
    height:     usize,
    cur_frame:  TM2Frame,
    prev_frame: TM2Frame,
}

impl TM2Decoder {
    fn new() -> Self { Self::default() }
    #[allow(clippy::manual_memcpy)]
    fn decode_blocks(&mut self) -> DecoderResult<bool> {
        let ydst = &mut self.cur_frame.ydata;
        let udst = &mut self.cur_frame.udata;
        let vdst = &mut self.cur_frame.vdata;
        let ystride = self.cur_frame.ystride;
        let cstride = self.cur_frame.cstride;
        let mut offs: [usize; 2] = [0; 2];
        let mut is_intra = true;

        let bw = self.width >> 2;
        let bh = self.height >> 2;
        validate!(self.streams[TM2StreamType::BlockType as usize].tokens.len() == bw * bh);

        let mut ydeltas: [i32; 16] = [0; 16];
        let mut cdeltas: [[i32; 4]; 2] = [[0; 4]; 2];
        let mut lasty: Vec<i32> = vec![0; self.width + 1];
        let mut lastu: Vec<i32> = vec![0; self.width/2 + 1];
        let mut lastv: Vec<i32> = vec![0; self.width/2 + 1];
        for by in 0..bh {
            let mut dstate = DeltaState::default();
            for bx in 0..bw {
                let bidx = self.streams[TM2StreamType::BlockType as usize].get_block_type()? as usize;
                validate!(bidx < TM2_BLOCK_TYPES.len());
                let btype = TM2_BLOCK_TYPES[bidx];
                match btype {
                    TM2BlockType::HiRes => {
                        for i in 0..4 {
                            cdeltas[0][i] = self.streams[TM2StreamType::CHigh as usize].get_token()?;
                            cdeltas[1][i] = self.streams[TM2StreamType::CHigh as usize].get_token()?;
                        }
                        dstate.apply_c(udst, offs[1] + bx * 2, cstride, &cdeltas[0], 0, &mut lastu[bx*2+1..]);
                        dstate.apply_c(vdst, offs[1] + bx * 2, cstride, &cdeltas[1], 1, &mut lastv[bx*2+1..]);
                        for i in 0..4*4 {
                            ydeltas[i] = self.streams[TM2StreamType::LHigh as usize].get_token()?;
                        }
                        dstate.apply_y(ydst, offs[0] + bx * 4, ystride, &ydeltas, &mut lasty[bx*4+1..]);
                    },
                    TM2BlockType::MedRes => {
                        cdeltas = [[0; 4]; 2];
                        cdeltas[0][0] = self.streams[TM2StreamType::CLow as usize].get_token()?;
                        cdeltas[1][0] = self.streams[TM2StreamType::CLow as usize].get_token()?;
                        dstate.interpolate_c(0, &mut lastu[bx*2..]);
                        dstate.apply_c(udst, offs[1] + bx * 2, cstride, &cdeltas[0], 0, &mut lastu[bx*2+1..]);
                        dstate.interpolate_c(1, &mut lastv[bx*2..]);
                        dstate.apply_c(vdst, offs[1] + bx * 2, cstride, &cdeltas[1], 1, &mut lastv[bx*2+1..]);
                        for i in 0..4*4 {
                            ydeltas[i] = self.streams[TM2StreamType::LHigh as usize].get_token()?;
                        }
                        dstate.apply_y(ydst, offs[0] + bx * 4, ystride, &ydeltas, &mut lasty[bx*4+1..]);
                    },
                    TM2BlockType::LowRes => {
                        cdeltas = [[0; 4]; 2];
                        cdeltas[0][0] = self.streams[TM2StreamType::CLow as usize].get_token()?;
                        cdeltas[1][0] = self.streams[TM2StreamType::CLow as usize].get_token()?;
                        dstate.interpolate_c(0, &mut lastu[bx*2..]);
                        dstate.apply_c(udst, offs[1] + bx * 2, cstride, &cdeltas[0], 0, &mut lastu[bx*2+1..]);
                        dstate.interpolate_c(1, &mut lastv[bx*2..]);
                        dstate.apply_c(vdst, offs[1] + bx * 2, cstride, &cdeltas[1], 1, &mut lastv[bx*2+1..]);
                        ydeltas = [0; 16];
                        ydeltas[ 0] = self.streams[TM2StreamType::LLow as usize].get_token()?;
                        ydeltas[ 2] = self.streams[TM2StreamType::LLow as usize].get_token()?;
                        ydeltas[ 8] = self.streams[TM2StreamType::LLow as usize].get_token()?;
                        ydeltas[10] = self.streams[TM2StreamType::LLow as usize].get_token()?;
                        dstate.interpolate_y_low(&mut lasty[bx*4..]);
                        dstate.apply_y(ydst, offs[0] + bx * 4, ystride, &ydeltas, &mut lasty[bx*4+1..]);
                    },
                    TM2BlockType::NullRes => {
                        cdeltas = [[0; 4]; 2];
                        dstate.interpolate_c(0, &mut lastu[bx*2..]);
                        dstate.apply_c(udst, offs[1] + bx * 2, cstride, &cdeltas[0], 0, &mut lastu[bx*2+1..]);
                        dstate.interpolate_c(1, &mut lastv[bx*2..]);
                        dstate.apply_c(vdst, offs[1] + bx * 2, cstride, &cdeltas[1], 1, &mut lastv[bx*2+1..]);
                        ydeltas = [0; 16];
                        dstate.interpolate_y_null(&mut lasty[bx*4..]);
                        dstate.apply_y(ydst, offs[0] + bx * 4, ystride, &ydeltas, &mut lasty[bx*4+1..]);
                    },
                    TM2BlockType::Update => {
                        is_intra = false;

                        let mut coff = offs[1] + bx * 2;
                        let usrc = &self.prev_frame.udata;
                        let vsrc = &self.prev_frame.vdata;
                        for _ in 0..2 {
                            for x in 0..2 {
                                let du = self.streams[TM2StreamType::Update as usize].get_token()?;
                                let dv = self.streams[TM2StreamType::Update as usize].get_token()?;
                                udst[coff + x] = usrc[coff + x] + (du as i16);
                                vdst[coff + x] = vsrc[coff + x] + (dv as i16);
                            }
                            coff += cstride;
                        }
                        dstate.recalc_c(udst, offs[1] + bx * 2, cstride, 0, &mut lastu[bx*2+1..]);
                        dstate.recalc_c(vdst, offs[1] + bx * 2, cstride, 1, &mut lastv[bx*2+1..]);
                        let mut yoff = offs[0] + bx * 4;
                        let ysrc = &self.prev_frame.ydata;
                        for _ in 0..4 {
                            for x in 0..4 {
                                let dy = self.streams[TM2StreamType::Update as usize].get_token()?;
                                ydst[yoff + x] = (i32::from(ysrc[yoff + x]) + dy) as u8;
                            }
                            yoff += ystride;
                        }
                        dstate.recalc_y(ydst, offs[0] + bx * 4, ystride, &mut lasty[bx*4+1..]);
                    },
                    TM2BlockType::Still => {
                        is_intra = false;

                        let mut coff = offs[1] + bx * 2;
                        let usrc = &self.prev_frame.udata;
                        let vsrc = &self.prev_frame.vdata;
                        for _ in 0..2 {
                            for x in 0..2 {
                                udst[coff + x] = usrc[coff + x];
                                vdst[coff + x] = vsrc[coff + x];
                            }
                            coff += cstride;
                        }
                        dstate.recalc_c(udst, offs[1] + bx * 2, cstride, 0, &mut lastu[bx*2+1..]);
                        dstate.recalc_c(vdst, offs[1] + bx * 2, cstride, 1, &mut lastv[bx*2+1..]);
                        let mut yoff = offs[0] + bx * 4;
                        let ysrc = &self.prev_frame.ydata;
                        for _ in 0..4 {
                            for x in 0..4 {
                                ydst[yoff + x] = ysrc[yoff + x];
                            }
                            yoff += ystride;
                        }
                        dstate.recalc_y(ydst, offs[0] + bx * 4, ystride, &mut lasty[bx*4+1..]);
                    },
                    TM2BlockType::Motion => {
                        is_intra = false;

                        let mx = self.streams[TM2StreamType::Motion as usize].get_token()?;
                        let my = self.streams[TM2StreamType::Motion as usize].get_token()?;
                        let xpos = (((bx as i32) * 4) + mx).max(0).min((self.width  - 4) as i32) as usize;
                        let ypos = (((by as i32) * 4) + my).max(0).min((self.height - 4) as i32) as usize;
                        let mut coff = offs[1] + bx * 2;
                        let mut csoff = (xpos >> 1) + (ypos >> 1) * cstride;
                        let usrc = &self.prev_frame.udata;
                        let vsrc = &self.prev_frame.vdata;
                        for _ in 0..2 {
                            for x in 0..2 {
                                udst[coff + x] = usrc[csoff + x];
                                vdst[coff + x] = vsrc[csoff + x];
                            }
                            coff  += cstride;
                            csoff += cstride;
                        }
                        dstate.recalc_c(udst, offs[1] + bx * 2, cstride, 0, &mut lastu[bx*2+1..]);
                        dstate.recalc_c(vdst, offs[1] + bx * 2, cstride, 1, &mut lastv[bx*2+1..]);
                        let mut yoff = offs[0] + bx * 4;
                        let mut ysoff = xpos + ypos * ystride;
                        let ysrc = &self.prev_frame.ydata;
                        for _ in 0..4 {
                            for x in 0..4 {
                                ydst[yoff + x] = ysrc[ysoff + x];
                            }
                            yoff  += ystride;
                            ysoff += ystride;
                        }
                        dstate.recalc_y(ydst, offs[0] + bx * 4, ystride, &mut lasty[bx*4+1..]);
                    },
                };
            }
            offs[0] += ystride * 4;
            offs[1] += cstride * 2;
        }

        Ok(is_intra)
    }
    fn output_frame(&mut self, buf: &mut NAVideoBuffer<u8>) {
        let fmt = buf.get_info().get_format();
        let offs = [fmt.get_chromaton(0).unwrap().get_offset() as usize,
                    fmt.get_chromaton(1).unwrap().get_offset() as usize,
                    fmt.get_chromaton(2).unwrap().get_offset() as usize];
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        let mut off = 0;
        let mut ysrc = 0;
        let mut csrc = 0;
        for y in 0..self.height {
            let out = &mut dst[off..];
            for (x, pic) in out.chunks_exact_mut(3).take(self.width).enumerate() {
                let y = i16::from(self.cur_frame.ydata[ysrc + x]);
                let u = self.cur_frame.udata[csrc + (x >> 1)];
                let v = self.cur_frame.vdata[csrc + (x >> 1)];
                pic[offs[0]] = (y + u).max(0).min(255) as u8;
                pic[offs[1]] = y.max(0).min(255) as u8;
                pic[offs[2]] = (y + v).max(0).min(255) as u8;
            }
            off += stride;
            ysrc += self.cur_frame.ystride;
            if (y & 1) != 0 {
                csrc += self.cur_frame.cstride;
            }
        }
    }
}

impl NADecoder for TM2Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, YUV410_FORMAT));
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            self.cur_frame  = TM2Frame::alloc(self.width, self.height);
            self.prev_frame = TM2Frame::alloc(self.width, self.height);
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() >= 40 + (TM2StreamType::Num as usize) * 4 + 4);
        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);

        let magic                               = br.read_u32be()?;
        validate!(magic == 0x100 || magic == 0x101);
                                                  br.read_skip(36)?;
        for stream in self.streams.iter_mut() {
            stream.read_header(&src, &mut br)?;
        }

        let myinfo = NAVideoInfo::new(self.width, self.height, false, RGB24_FORMAT);
        let bufinfo = alloc_video_buffer(myinfo, 2)?;
        let mut buf = bufinfo.get_vbuf().unwrap();

        let is_intra = self.decode_blocks()?;
        self.output_frame(&mut buf);
        std::mem::swap(&mut self.cur_frame, &mut self.prev_frame);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for TM2Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(TM2Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_tm2() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/TM20/tm20.avi
        test_decoding("avi", "truemotion2", "assets/Duck/tm20.avi", Some(16),
                        &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x8c336eb4, 0x10d0d934, 0x52392306, 0xc0bc6dd3],
                            [0xf168ddc2, 0x502fef17, 0xf7a5d0a2, 0xc0bf2d26],
                            [0xf33e02fa, 0x3931b691, 0xb29a0754, 0x07c0f8fa],
                            [0x2dd81034, 0x1c9f7616, 0x64eed48a, 0x3aa09cf0],
                            [0x55d18cd9, 0x0a3fd971, 0xf28fd5af, 0x9d9c3e3d],
                            [0xbf9cbbd8, 0x7b44c122, 0x1c7b1904, 0x77cc87aa],
                            [0xa6f6e79d, 0xc463a5bc, 0x5df9460c, 0xfce2e352],
                            [0x2ad22b2d, 0xd4ceaff8, 0xa4adb974, 0x37888a8d],
                            [0x4d45575f, 0x7e5b7670, 0x40cb7438, 0x9872d422],
                            [0xb35bc12b, 0x026c77c6, 0x93163784, 0xb37630b7],
                            [0xb1ec5059, 0x1fe26596, 0x4ac8d214, 0xdaf1b895],
                            [0x69dd5a5f, 0xe14a16fa, 0xa0653092, 0xb04e0739],
                            [0x979d8fe1, 0xbef29f89, 0xefae5f86, 0xa1ceb7d2],
                            [0xc6dc80d7, 0x80153c6b, 0x76d770c0, 0x8fd7cce7],
                            [0x8da96394, 0x1bd68024, 0x5feddfba, 0xd2b00660],
                            [0x53ff97c3, 0xc021a9b3, 0xabdddc10, 0xc99ab86f],
                            [0x66c877c4, 0xd8358048, 0xbca593db, 0xc6ecc4d1]]));
    }
}
