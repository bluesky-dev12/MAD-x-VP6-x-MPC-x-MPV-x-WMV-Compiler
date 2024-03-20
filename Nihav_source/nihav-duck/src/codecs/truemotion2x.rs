use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

#[derive(Default)]
struct Decryptor {
    key:    [u8; 4],
}

impl Decryptor {
    fn decrypt(&mut self, buf: &mut [u8]) {
        let mut pos: u8 = 0;
        for el in buf.iter_mut() {
            *el ^= self.key[(pos & 3) as usize];
            pos = pos.wrapping_add(1);
        }
    }
    fn set_state(&mut self, mut key: u32) {
        for _ in 0..3 {
            let bit31 = (key >> 31) & 1;
            let bit21 = (key >> 21) & 1;
            let bit01 = (key >>  1) & 1;
            let nbit0 = !key & 1;
            key = (key << 1) | (bit31 ^ bit21 ^ bit01 ^ nbit0);
        }
        for i in 0..4 {
            self.key[i] = (key >> (8 * (i ^ 3))) as u8;
        }
    }
}

struct Deltas {
    tabs:       [[i16; 256]; 2],
    codebook:   [[u8; 8]; 256],
    num_elems:  [usize; 256],
    num_vq:     usize,
    vq_idx:     usize,
    vq_pos:     usize,
    vq_esc:     u8,
}

impl Deltas {
    fn reset(&mut self, br: &mut ByteReader) -> DecoderResult<()> {
        let b                               = br.read_byte()? as usize;
        self.vq_idx = b;
        self.vq_pos = 0;
        Ok(())
    }
    fn get_val(&mut self, br: &mut ByteReader) -> DecoderResult<u8> {
        if self.vq_idx > self.codebook.len() { return Err(DecoderError::ShortData); }
        let ret = self.codebook[self.vq_idx][self.vq_pos];
        self.vq_pos += 1;
        if self.vq_pos == self.num_elems[self.vq_idx] {
            if br.left() > 0 {
                self.reset(br)?;
            } else {
                self.vq_idx = self.codebook.len() + 1;
            }
        }
        Ok(ret)
    }
    fn remap(val: u16) -> i16 {
        let hval = (val >> 1) as i16;
        if (val & 1) == 0 {
            hval
        } else {
            -1 - hval
        }
    }
    fn get_int(&mut self, br: &mut ByteReader) -> DecoderResult<i16> {
        let b = self.get_val(br)?;
        if b != self.vq_esc - 1 {
            return Ok(Self::remap(u16::from(b)));
        }
        let mut run = 0;
        let mut val;
        let mut pow = u16::from(self.vq_esc);
        loop {
            let b = self.get_val(br)?;
            run += 1;
            if b != self.vq_esc - 1 {
                val = u16::from(b) * pow;
                break;
            }
            pow *= u16::from(self.vq_esc);
        }

        for _ in 0..run {
            pow /= u16::from(self.vq_esc);
            let b = u16::from(self.get_val(br)?);
            val += pow * b;
        }
        Ok(Self::remap(val))
    }
    fn get_dy(&mut self, br: &mut ByteReader) -> DecoderResult<i16> {
        let b = self.get_val(br)?;
        Ok(self.tabs[1][b as usize])
    }
    fn get_dc(&mut self, br: &mut ByteReader) -> DecoderResult<i16> {
        let b = self.get_val(br)?;
        Ok(self.tabs[0][b as usize])
    }
}

impl Default for Deltas {
    fn default() -> Self {
        Self {
            tabs:       [[0; 256]; 2],
            codebook:   [[0; 8]; 256],
            num_elems:  [0; 256],
            num_vq:     0,
            vq_idx:     0,
            vq_pos:     0,
            vq_esc:     0,
        }
    }
}

#[derive(Clone,Copy, Default)]
struct BlkInfo {
    btype:      u8,
    mode:       u8,
    mv_x:       i16,
    mv_y:       i16,
}

const NUM_CPARAMS: usize = 25;
const CPARAM_NONE: u8 = 42;
const CPARAM_MISSING: u8 = 42 * 2;
const CPARAM_MV: u8 = 42 * 3;

macro_rules! apply_delta {
    ($buf:expr, $off:expr, $stride:expr, $hpred: expr, $delta:expr) => {
        $hpred = $hpred.wrapping_add($delta);
        $buf[$off] = $buf[$off - $stride].wrapping_add($hpred);
    };
}
macro_rules! copy_line {
    ($buf:expr, $off:expr, $stride:expr) => {
        for i in 0..8 {
            $buf[$off + i] = $buf[$off + i - $stride];
        }
    };
}

#[derive(Default)]
struct Frame {
    ydata:      Vec<i16>,
    udata:      Vec<i16>,
    vdata:      Vec<i16>,
    stride:     usize,
}

impl Frame {
    fn resize(&mut self, w: usize, h: usize) {
        self.stride = w;
        self.ydata.resize(self.stride * (h + 1), 0x80);
        self.udata.resize(self.stride * (h + 1), 0x80);
        self.vdata.resize(self.stride * (h + 1), 0x80);
    }
}

#[derive(Default)]
struct TM2XDecoder {
    info:       NACodecInfoRef,
    width:      usize,
    height:     usize,
    dec_buf:    Vec<u8>,
    version:    u8,
    is_intra:   bool,
    deltas:     Deltas,
    blk_info:   Vec<BlkInfo>,
    tile_size:  usize,
    cparams:    [[u8; 8]; NUM_CPARAMS],
    cur_frame:  Frame,
    ref_frame:  Frame,
}

impl TM2XDecoder {
    fn new() -> Self { Self::default() }
    fn output_frame(&mut self, buf: &mut NAVideoBuffer<u8>) {
        let mut offs = [ buf.get_offset(0), buf.get_offset(1), buf.get_offset(2) ];
        let strides = [ buf.get_stride(0), buf.get_stride(1), buf.get_stride(2) ];
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        let mut pos = self.cur_frame.stride;
        for _y in 0..self.height {
            for x in 0..self.width {
                let y = self.cur_frame.ydata[pos + x];
                let u = self.cur_frame.udata[pos + x];
                let v = self.cur_frame.vdata[pos + x];
                dst[offs[0] + x] = y.max(0).min(255) as u8;
                dst[offs[1] + x] = u.max(0).min(255) as u8;
                dst[offs[2] + x] = v.max(0).min(255) as u8;
            }
            for c in 0..3 {
                offs[c] += strides[c];
            }
            pos += self.cur_frame.stride;
        }
    }
    fn parse_init(&mut self, version: u8) -> DecoderResult<()> {
        self.version = version;

        let mut mr = MemoryReader::new_read(&self.dec_buf);
        let mut br = ByteReader::new(&mut mr);
        if version > 4 {
            let _smth                   = br.read_u32be()?;
        }
        let height                      = br.read_u16be()? as usize;
        let width                       = br.read_u16be()? as usize;
        validate!(width == self.width && height == self.height);
        if version > 4 {
            let _smth                   = br.read_u32be()?;
        }
        let _smth                       = br.read_byte()?;
        let _nfuncs                     = br.read_byte()? as usize;
        let _smth                       = br.read_u16be()? as usize;
        let has_mv                      = br.read_byte()?;
        self.is_intra = has_mv == 0;
        if version >= 4 {
            let _flags                  = br.read_u16be()?;
            let id_len                  = br.read_byte()? as usize;
                                          br.read_skip(id_len)?;
            let _smth1                  = br.read_byte()?;
            let len                     = br.read_byte()? as usize;
                                          br.read_skip(len)?;
            let _smth                   = br.read_u32be()?;
        }

        Ok(())
    }
    fn parse_tabs(&mut self) -> DecoderResult<()> {
        let mut mr = MemoryReader::new_read(&self.dec_buf);
        let mut br = ByteReader::new(&mut mr);

        let idx                         = br.read_byte()? as usize;
        validate!(idx < self.deltas.tabs.len());
        let len                         = br.read_byte()? as usize;
        validate!(((len * 2) as i64) == br.left());
        for i in 0..len {
            self.deltas.tabs[idx][i]    = br.read_u16be()? as i16;
        }

        Ok(())
    }
    fn parse_cb_desc(&mut self, version: u8) -> DecoderResult<()> {
        let mut mr = MemoryReader::new_read(&self.dec_buf);
        let mut br = ByteReader::new(&mut mr);

        if version == 0x0A {
            self.deltas.vq_esc          = br.read_byte()?;
            validate!(self.deltas.vq_esc > 1);
            let _tag                    = br.read_u16be()?;
        }
        let len                         = br.read_u16be()? as usize;
        validate!(len + 3 == (br.left() as usize));
        let num_entries                 = br.read_u16be()?;
        validate!(num_entries == 256);
        let max_elems                   = br.read_byte()?;
        validate!(max_elems > 0 && max_elems <= 8);
        let mut idx = 0;
        while br.left() > 0 {
            validate!(idx < self.deltas.codebook.len());
            let num_elems               = br.read_byte()? as usize;
            validate!(num_elems <= 8);
            self.deltas.num_elems[idx] = num_elems;
            for i in 0..num_elems {
                self.deltas.codebook[idx][i]    = br.read_byte()?;
            }
            idx += 1;
        }
        validate!(idx == 256);
        self.deltas.num_vq = idx;
        Ok(())
    }

    #[allow(clippy::int_plus_one)]
    #[allow(clippy::manual_memcpy)]
    #[allow(clippy::cognitive_complexity)]
    fn decode_frame(&mut self, src: &[u8]) -> DecoderResult<()> {
        let mut mr = MemoryReader::new_read(src);
        let mut br = ByteReader::new(&mut mr);

        self.deltas.reset(&mut br)?;
        let bw = self.width / 8;
        let bh = self.height / 8;
        let ntiles = (bw + self.tile_size - 1) / self.tile_size;
        let mut pos = self.cur_frame.stride;
        let ydata = &mut self.cur_frame.ydata;
        let udata = &mut self.cur_frame.udata;
        let vdata = &mut self.cur_frame.vdata;
        let stride = self.cur_frame.stride;
        for by in 0..bh {
            for tile in 0..ntiles {
                let xpos = tile * self.tile_size;
                let len = self.tile_size.min(bw - xpos);
                let mut last_x = 0;
                let mut last_y = 0;
                for el in self.blk_info.iter_mut().take(len) {
                    let t1                      = self.deltas.get_val(&mut br)?;
                    let t2                      = self.deltas.get_val(&mut br)?;
                    validate!((t1 as usize) < NUM_CPARAMS);
                    validate!(self.cparams[t1 as usize][0] != CPARAM_MISSING);
                    el.btype = t1;
                    el.mode  = t2;
                    if t2 > 0 {
                        el.mv_x                 = self.deltas.get_int(&mut br)?;
                        el.mv_y                 = self.deltas.get_int(&mut br)?;
                        last_x = el.mv_x;
                        last_y = el.mv_y;
                    } else {
                        el.mv_x = last_x;
                        el.mv_y = last_y;
                    }
                }
                for line in 0..8 {
                    let mut ypred = 0i16;
                    let mut upred = 0i16;
                    let mut vpred = 0i16;
                    for x in 0..len {
                        let bx = xpos + x;
                        let op = self.cparams[self.blk_info[x].btype as usize][line];
                        let cur_off = pos + bx * 8 + line * stride;
                        match op {
                            0 => { // y4|y4
                                for i in 0..8 {
                                    let delta = self.deltas.get_dy(&mut br)?;
                                    apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                }
                                copy_line!(udata, cur_off, stride);
                                copy_line!(vdata, cur_off, stride);
                                upred = 0;
                                vpred = 0;
                            },
                            1 => { // y2|y2
                                for i in 0..8 {
                                    if (i & 1) == 0 {
                                        let delta = self.deltas.get_dy(&mut br)?;
                                        apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                    } else {
                                        ydata[cur_off + i] = ydata[cur_off + i - 1];
                                    }
                                }
                                copy_line!(udata, cur_off, stride);
                                copy_line!(vdata, cur_off, stride);
                                upred = 0;
                                vpred = 0;
                            },
                            2 => { // y1|y1
                                for i in 0..8 {
                                    if (i & 3) == 0 {
                                        let delta = self.deltas.get_dy(&mut br)?;
                                        apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                    } else {
                                        ydata[cur_off + i] = ydata[cur_off + i - 1];
                                    }
                                }
                                copy_line!(udata, cur_off, stride);
                                copy_line!(vdata, cur_off, stride);
                                upred = 0;
                                vpred = 0;
                            },
                            3 => { // y1|0
                                let delta = self.deltas.get_dy(&mut br)?;
                                apply_delta!(ydata, cur_off, stride, ypred, delta);
                                for i in 1..8 {
                                    ydata[cur_off + i] = ydata[cur_off];
                                }
                                copy_line!(udata, cur_off, stride);
                                copy_line!(vdata, cur_off, stride);
                                upred = 0;
                                vpred = 0;
                            },
                            4 => { // c2y2c2y2|c2y2c2y2
                                for i in (0..8).step_by(2) {
                                    let delta = self.deltas.get_dc(&mut br)?;
                                    apply_delta!(udata, cur_off + i + 0, stride, upred, delta);
                                    udata[cur_off + i + 1] = udata[cur_off + i];
                                    let delta = self.deltas.get_dc(&mut br)?;
                                    apply_delta!(vdata, cur_off + i + 0, stride, vpred, delta);
                                    vdata[cur_off + i + 1] = vdata[cur_off + i];
                                    let delta = self.deltas.get_dy(&mut br)?;
                                    apply_delta!(ydata, cur_off + i + 0, stride, ypred, delta);
                                    let delta = self.deltas.get_dy(&mut br)?;
                                    apply_delta!(ydata, cur_off + i + 1, stride, ypred, delta);
                                }
                            },
                            5 => { // c2y1|c2y1
                                for i in 0..8 {
                                    if (i & 3) == 0 {
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(udata, cur_off + i, stride, upred, delta);
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(vdata, cur_off + i, stride, vpred, delta);
                                        let delta = self.deltas.get_dy(&mut br)?;
                                        apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                    } else {
                                        udata[cur_off + i] = udata[cur_off + i - 1];
                                        vdata[cur_off + i] = vdata[cur_off + i - 1];
                                        ydata[cur_off + i] = ydata[cur_off + i - 1];
                                    }
                                }
                            },
                            6 | 7 => unreachable!(),
                            8 => { // c2y4|c2y4
                                for i in 0..8 {
                                    if (i & 3) == 0 {
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(udata, cur_off + i, stride, upred, delta);
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(vdata, cur_off + i, stride, vpred, delta);
                                    } else {
                                        udata[cur_off + i] = udata[cur_off + i - 1];
                                        vdata[cur_off + i] = vdata[cur_off + i - 1];
                                    }
                                    let delta = self.deltas.get_dy(&mut br)?;
                                    apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                }
                            },
                            9 => { // c2y2|c2y2
                                for i in 0..8 {
                                    if (i & 3) == 0 {
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(udata, cur_off + i, stride, upred, delta);
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(vdata, cur_off + i, stride, vpred, delta);
                                    } else {
                                        udata[cur_off + i] = udata[cur_off + i - 1];
                                        vdata[cur_off + i] = vdata[cur_off + i - 1];
                                    }
                                    if (i & 1) == 0 {
                                        let delta = self.deltas.get_dy(&mut br)?;
                                        apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                    } else {
                                        ydata[cur_off + i] = ydata[cur_off + i - 1];
                                    }
                                }
                            },
                            10 => { // c2y1|c2y1
                                for i in 0..8 {
                                    if (i & 3) == 0 {
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(udata, cur_off + i, stride, upred, delta);
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(vdata, cur_off + i, stride, vpred, delta);
                                        let delta = self.deltas.get_dy(&mut br)?;
                                        apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                    } else {
                                        udata[cur_off + i] = udata[cur_off + i - 1];
                                        vdata[cur_off + i] = vdata[cur_off + i - 1];
                                        ydata[cur_off + i] = ydata[cur_off + i - 1];
                                    }
                                }
                            },
                            11 => unreachable!(),
                            12 => { // c2y8
                                for i in 0..8 {
                                    if i == 0 {
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(udata, cur_off + i, stride, upred, delta);
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(vdata, cur_off + i, stride, vpred, delta);
                                    } else {
                                        udata[cur_off + i] = udata[cur_off + i - 1];
                                        vdata[cur_off + i] = vdata[cur_off + i - 1];
                                    }
                                    let delta = self.deltas.get_dy(&mut br)?;
                                    apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                }
                            },
                            13 => { // c2y4
                                for i in 0..8 {
                                    if i == 0 {
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(udata, cur_off + i, stride, upred, delta);
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(vdata, cur_off + i, stride, vpred, delta);
                                    } else {
                                        udata[cur_off + i] = udata[cur_off + i - 1];
                                        vdata[cur_off + i] = vdata[cur_off + i - 1];
                                    }
                                    if (i & 1) == 0 {
                                        let delta = self.deltas.get_dy(&mut br)?;
                                        apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                    } else {
                                        ydata[cur_off + i] = ydata[cur_off + i - 1];
                                    }
                                }
                            },
                            14 => { // c2y2
                                for i in 0..8 {
                                    if i == 0 {
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(udata, cur_off + i, stride, upred, delta);
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(vdata, cur_off + i, stride, vpred, delta);
                                    } else {
                                        udata[cur_off + i] = udata[cur_off + i - 1];
                                        vdata[cur_off + i] = vdata[cur_off + i - 1];
                                    }
                                    if (i & 3) == 0 {
                                        let delta = self.deltas.get_dy(&mut br)?;
                                        apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                    } else {
                                        ydata[cur_off + i] = ydata[cur_off + i - 1];
                                    }
                                }
                            },
                            15 => { // c2y1
                                for i in 0..8 {
                                    if i == 0 {
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(udata, cur_off + i, stride, upred, delta);
                                        let delta = self.deltas.get_dc(&mut br)?;
                                        apply_delta!(vdata, cur_off + i, stride, vpred, delta);
                                        let delta = self.deltas.get_dy(&mut br)?;
                                        apply_delta!(ydata, cur_off + i, stride, ypred, delta);
                                    } else {
                                        udata[cur_off + i] = udata[cur_off + i - 1];
                                        vdata[cur_off + i] = vdata[cur_off + i - 1];
                                        ydata[cur_off + i] = ydata[cur_off + i - 1];
                                    }
                                }
                            },
                            CPARAM_NONE => {
                                copy_line!(ydata, cur_off, stride);
                                copy_line!(udata, cur_off, stride);
                                copy_line!(vdata, cur_off, stride);
                                ypred = 0;
                                upred = 0;
                                vpred = 0;
                            },
                            CPARAM_MV => {
                                let src_x = (bx as i16) * 8 + self.blk_info[x].mv_x;
                                let src_y = ((by * 8 + line) as i16) + self.blk_info[x].mv_y;
                                validate!(src_x >= 0 && (src_x as usize) + 8 <= self.width);
                                validate!(src_y >= 0 && (src_y as usize) + 1 <= self.height);
                                let ref_off = (src_x as usize) + ((src_y + 1) as usize) * stride;
                                for i in 0..8 {
                                    ydata[cur_off + i] = self.ref_frame.ydata[ref_off + i];
                                    udata[cur_off + i] = self.ref_frame.udata[ref_off + i];
                                    vdata[cur_off + i] = self.ref_frame.vdata[ref_off + i];
                                }
                                ypred = ydata[cur_off + 7] - ydata[cur_off + 7 - stride];
                                upred = udata[cur_off + 7] - udata[cur_off + 7 - stride];
                                vpred = vdata[cur_off + 7] - vdata[cur_off + 7 - stride];
                            },
                            _ => unreachable!(),
                        }
                    }
                }
            }
            pos += 8 * stride;
        }

        Ok(())
    }
}

impl NADecoder for TM2XDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = NAPixelFormaton::new(ColorModel::YUV(YUVSubmodel::YUVJ),
                                           Some(NAPixelChromaton::new(0, 0, false, 8, 0, 0, 1)),
                                           Some(NAPixelChromaton::new(0, 0, false, 8, 0, 1, 1)),
                                           Some(NAPixelChromaton::new(0, 0, false, 8, 0, 2, 1)),
                                           None, None, 0, 3);
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, fmt));
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            self.cur_frame.resize(self.width, self.height);
            self.ref_frame.resize(self.width, self.height);
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 8);
        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);
        let mut dec = Decryptor::default();

        let mut initialised = false;
        let mut got_key = false;
        let mut data_size = 0;
        self.cparams = [[CPARAM_MISSING; 8]; NUM_CPARAMS];
        self.is_intra = false;
        while br.left() >= 8 {
            let magic                           = br.read_u24be()?;
            let ctype                           = br.read_byte()?;
            let size                            = br.read_u32be()? as usize;
            validate!(magic == 0xA00001 && br.left() >= (size as i64));
            if ctype == 0x06 {
                validate!(size >= 4);
                let key                     = br.read_u32be()?;
                validate!((key as usize) == size - 4);
                dec.set_state(key);
                data_size = size - 4;
                br.read_skip(size - 4)?;
                got_key = true;
                continue;
            }
            self.dec_buf.resize(size, 0);
                                                  br.read_buf(&mut self.dec_buf)?;
            if ctype != 0x0A {
                dec.decrypt(&mut self.dec_buf);
            }
            match ctype {
                0x08 | 0x0C | 0x10 | 0x11 | 0x15 | 0x16 => {
                    if ctype < 0x15 { // old versions are not encrypted, roll back and zero key
                        dec.decrypt(&mut self.dec_buf);
                        dec.set_state(0);
                        got_key = true;
                    }
                    validate!(got_key && !initialised);
                    match ctype {
                        0x08 => self.parse_init(0)?,
                        0x0C => self.parse_init(1)?,
                        0x10 => self.parse_init(2)?,
                        0x11 => self.parse_init(3)?,
                        0x15 => self.parse_init(4)?,
                        0x16 => self.parse_init(5)?,
                        _ => unreachable!(),
                    };
                    initialised = true;
                },
                0x09 => {
                    validate!(initialised);
                    validate!(self.dec_buf.len() == 3);
                    validate!(self.dec_buf[0] == 8);
                    validate!(self.dec_buf[1] > 0);
                    validate!(self.dec_buf[2] == 1);
                    self.tile_size = self.dec_buf[1] as usize;
                    self.blk_info.resize(self.tile_size, BlkInfo::default());
                },
                0x0B => {
                    validate!(initialised);
                    validate!(self.dec_buf.len() == 4);
                    let idx = self.dec_buf[3] as usize;
                    validate!(idx < NUM_CPARAMS);
                    validate!((self.dec_buf[0] as usize) < TM2X_CODING_PARAMS.len());
                    if self.dec_buf[0] != 0 {
                        let tab = &TM2X_CODING_PARAMS[self.dec_buf[0] as usize];
                        let m0 = tab[0] as usize;
                        let m1 = tab[1] as usize;
                        let m2 = tab[2] as usize;
                        let m3 = tab[3] as usize;
                        let full_mode = (m2 * 4 + m0) as u8;
                        let lores_mode = m0 as u8;
                        for i in 0..8 {
                            if (i % m1) == 0 && (i % m3) == 0 {
                                self.cparams[idx][i] = full_mode;
                            } else if (i % m1) == 0 {
                                self.cparams[idx][i] = lores_mode;
                            } else {
                                self.cparams[idx][i] = CPARAM_NONE;
                            }
                        }
                    } else {
                        for i in 0..8 { self.cparams[idx][i] = CPARAM_MV; }
                    }
                },
                0x02 => {
                    validate!(initialised);
                    self.parse_tabs()?;
                },
                0x0A => {
                    validate!(initialised);
                    self.parse_cb_desc(0xA)?;
                },
                _ => { unimplemented!(); },
            };
        }
        if !self.is_intra {
            std::mem::swap(&mut self.cur_frame, &mut self.ref_frame);
        }
        self.decode_frame(&src[12..][..data_size])?;

        let myinfo = self.info.get_properties().get_video_info().unwrap();
        let bufinfo = alloc_video_buffer(myinfo, 2)?;
        let mut buf = bufinfo.get_vbuf().unwrap();

        self.output_frame(&mut buf);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(self.is_intra);
        frm.set_frame_type(if self.is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for TM2XDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(TM2XDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_tm2x() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/TM2x.avi
        test_decoding("avi", "truemotion2x", "assets/Duck/TM2x.avi", None,
                        &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x2854e7f3, 0x41e54fd3, 0xd9a16302, 0x580321b2],
                            [0x8e736f59, 0x57c58dc0, 0xe15bad3a, 0xf96e2c5b],
                            [0x12700e7c, 0xfa2d06e0, 0x05b758ba, 0xe79aabb6],
                            [0x29f935f2, 0x2ad0d6d1, 0x2fde19a2, 0x5aa3823b],
                            [0x7f1a787d, 0x77f3ab61, 0x0e584c66, 0x9d1842ea],
                            [0xb5607334, 0xdb149056, 0xe0e3ddb5, 0x19f8e254],
                            [0xb054e4d0, 0x1d241d4f, 0x75fdfe95, 0x9dde8024]]));
    }
}

const TM2X_CODING_PARAMS: [[u8; 4]; 25] = [
    [ 0, 0, 0, 0 ], [ 0, 1, 1, 1 ], [ 0, 1, 1, 2 ], [ 0, 1, 2, 4 ], [ 1, 1, 2, 4 ],
    [ 0, 2, 2, 4 ], [ 1, 2, 2, 4 ], [ 2, 2, 2, 4 ], [ 1, 4, 2, 4 ], [ 2, 4, 2, 4 ],
    [ 2, 8, 3, 8 ], [ 3, 4, 3, 8 ], [ 3, 8, 3, 8 ], [ 0, 1, 1, 4 ], [ 0, 1, 2, 2 ],
    [ 0, 2, 1, 4 ], [ 1, 1, 2, 2 ], [ 1, 4, 2, 8 ], [ 2, 2, 3, 4 ], [ 2, 4, 3, 8 ],
    [ 0, 1, 3, 8 ], [ 1, 2, 3, 8 ], [ 2, 4, 2, 4 ], [ 2, 4, 3, 8 ], [ 3, 8, 3, 8 ]
];
