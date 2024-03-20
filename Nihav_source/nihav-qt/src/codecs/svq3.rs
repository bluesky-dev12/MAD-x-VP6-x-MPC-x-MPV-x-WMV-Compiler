use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::intcode::*;
use nihav_codec_support::codecs::*;
use nihav_codec_support::codecs::blockdsp::*;
use nihav_codec_support::data::GenericCache;

use super::svq3dsp::*;

struct SVQ3Header {
    ftype:      FrameType,
    ts:         u8,
    quant:      u8,
    dquant:     bool,
}

#[derive(Clone,Copy,Debug,PartialEq)]
enum MCMode {
    Pixel,
    Halfpel,
    Thirdpel
}

#[derive(Default)]
struct SState {
    q:          u8,
    has_left:   bool,
    has_top:    bool,
    has_tl:     bool,
    has_tr:     bool,
    trb:        u8,
    trd:        u8,
}

struct IntraModeState {
    cache:      GenericCache<i8>,
    i16_pred:   i8,
}

impl IntraModeState {
    fn new(mb_w: usize) -> Self {
        let stride = 1 + mb_w * 4 + 1;
        IntraModeState { cache: GenericCache::new(4, stride, -1), i16_pred: 0 }
    }
    fn reset(&mut self) { self.cache.reset(); }
    fn update(&mut self) { self.cache.update_row(); }
    fn get_pos(&self, xpos: usize) -> usize {
        self.cache.stride + 1 + xpos * 4
    }
    fn set_mb_x(&mut self, mb_x: usize) {
        self.cache.xpos = self.get_pos(mb_x);
    }
    fn fill_block(&mut self, val: i8) {
        let mut pos = self.cache.xpos;
        for _ in 0..4 {
            for j in 0..4 {
                self.cache.data[pos + j] = val;
            }
            pos += self.cache.stride;
        }
    }
    fn get_pred16_type(&self, has_top: bool, has_left: bool) -> PredType8x8 {
        if !has_top && !has_left { return PredType8x8::DC128; }
        let mut im = INTRA_PRED16[self.i16_pred as usize];
        if !has_top {
            im = match im {
                    PredType8x8::Plane | PredType8x8::Ver => PredType8x8::Hor,
                    PredType8x8::DC => PredType8x8::LeftDC,
                    _   => im,
                 };
        } else if !has_left {
            im = match im {
                    PredType8x8::Plane | PredType8x8::Hor => PredType8x8::Ver,
                    PredType8x8::DC => PredType8x8::TopDC,
                    _   => im,
                 };
        }
        im
    }
    fn get_pred8_type(&self, has_top: bool, has_left: bool) -> PredType8x8 {
        if !has_top && !has_left { return PredType8x8::DC128; }
        let mut im = PredType8x8::DC;
        if !has_top {
            im = match im {
                    PredType8x8::Plane | PredType8x8::Ver => PredType8x8::Hor,
                    PredType8x8::DC => PredType8x8::LeftDC,
                    _   => im,
                 };
        } else if !has_left {
            im = match im {
                    PredType8x8::Plane | PredType8x8::Hor => PredType8x8::Ver,
                    PredType8x8::DC => PredType8x8::TopDC,
                    _   => im,
                 };
        }
        im
    }
    fn get_pred4_type(&self, x: usize, y: usize, has_top: bool, has_left: bool) -> PredType4x4 {
        let no_up = !has_top && (y == 0);
        let no_left = !has_left && (x == 0);
        if no_up && no_left { return PredType4x4::DC128; }

        let mut im = INTRA_PRED4[self.cache.data[self.cache.xpos + x + y * self.cache.stride] as usize];

        if no_up {
            im = match im {
                    PredType4x4::Ver => PredType4x4::Hor,
                    PredType4x4::DC  => PredType4x4::LeftDC,
                    _                => im,
                 };
        } else if no_left {
            im = match im {
                    PredType4x4::Hor            => PredType4x4::Ver,
                    PredType4x4::DC             => PredType4x4::TopDC,
                    _                           => im,
                 };
        }
        im
    }
}

struct MVInfo {
    mv_b:   Vec<MV>,
    mv_f:   Vec<MV>,
    w:      usize,
    h:      usize,
    has_b:  Vec<bool>,
    has_f:  Vec<bool>,
}

impl MVInfo {
    fn new() -> Self {
        Self { mv_b: Vec::new(), mv_f: Vec::new(), w: 0, h: 0, has_b: Vec::new(), has_f: Vec::new() }
    }
    fn resize(&mut self, mb_w: usize, mb_h: usize) {
        self.w = mb_w * 4;
        self.h = mb_h * 4;
        self.reset();
    }
    fn reset(&mut self) {
        let size = self.w * self.h;
        self.mv_f.clear();
        self.mv_f.resize(size, ZERO_MV);
        self.mv_b.clear();
        self.mv_b.resize(size, ZERO_MV);
        self.has_f.clear();
        self.has_f.resize(size >> 4, false);
        self.has_b.clear();
        self.has_b.resize(size >> 4, false);
    }
    fn fill(&mut self, mb_x: usize, mb_y: usize, fwd: bool, mv: MV) {
        let idx = mb_x * 4 + mb_y * 4 * self.w;
        let dst = if fwd { &mut self.mv_f[idx..] } else { &mut self.mv_b[idx..] };
        for row in dst.chunks_mut(self.w).take(4) {
            row[0] = mv;
            row[1] = mv;
            row[2] = mv;
            row[3] = mv;
        }
    }
    fn fill_part(&mut self, x: usize, y: usize, fwd: bool, bw: usize, bh: usize, mv: MV) {
        let idx = x + y * self.w;
        let dst = if fwd { &mut self.mv_f[idx..] } else { &mut self.mv_b[idx..] };
        for row in dst.chunks_mut(self.w).take(bh) {
            for el in row.iter_mut().take(bw) {
                *el = mv;
            }
        }
    }
    fn get_mv_by_idx(&self, idx: usize, fwd: bool) -> MV {
        if fwd { self.mv_f[idx] } else { self.mv_b[idx] }
    }
    fn pred_mv(&self, idx: usize, bw: usize, fwd: bool, has_top: bool, has_left: bool, has_tr: bool, has_tl: bool) -> MV {
        if !has_top && !has_left { return ZERO_MV; }
        let left_mv = if has_left { self.get_mv_by_idx(idx - 1, fwd) } else { ZERO_MV };
        let top_mv  = if has_top  { self.get_mv_by_idx(idx - self.w, fwd) } else { left_mv };
        let tr_mv;
        if has_tr {
            tr_mv = self.get_mv_by_idx(idx - self.w + bw, fwd);
        } else if has_tl {
            tr_mv = self.get_mv_by_idx(idx - self.w - 1, fwd);
        } else {
            tr_mv = left_mv;
        }
        MV::pred(left_mv, top_mv, tr_mv)
    }
    fn pred_mv_part(&self, x: usize, y: usize, bw: usize, fwd: bool, has_top: bool, has_left: bool, has_tr: bool, has_tl: bool) -> MV {
        self.pred_mv(x + y * self.w, bw, fwd, has_top, has_left, has_tr, has_tl)
    }
    fn get_mv(&self, x: usize, y: usize, fwd: bool) -> MV {
        let idx = x + y * self.w;
        if fwd { self.mv_f[idx] }
        else   { self.mv_b[idx] }
    }
}

struct SVQ3Decoder {
    info:       NACodecInfoRef,
    width:      usize,
    height:     usize,
    ipbs:       IPBShuffler,
    avg_buf:    NAVideoBufferRef<u8>,
    imode:      IntraModeState,
    mvi:        MVInfo,
    ref_mvi:    MVInfo,
    mbtypes:    Vec<u8>,
    ebuf:       [u8; 32 * 18],

    coeffs:     [[i16; 16]; 25],
    coded:      [bool; 24],
    dc_only:    [bool; 24],

    use_hpel:   bool,
    use_tpel:   bool,
    no_bframes: bool,
    protected:  bool,
    slice_buf:  Vec<u8>,
    mb_x:       usize,
    mb_y:       usize,
    mb_w:       usize,
    mb_h:       usize,
    ts_fwd:     u8,
    ts_bwd:     u8,
    pts_base:   u64,
    ts_base:    u8,
}

const ZIGZAG4: &[usize; 16] = &[
    0,  1,  4,  8,
    5,  2,  3,  6,
    9, 12, 13, 10,
    7, 11, 14, 15
];
const ALT_SCAN: &[usize; 16] = &[
     0,  1,  2,  6,
    10,  3,  7, 11,
     4,  8,  5,  9,
    12, 13, 14, 15
];

const SVQ3_RUNLEVEL: [(usize, i16); 16] = [
    ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 0, 2 ), ( 3, 1 ), ( 4, 1 ), ( 5, 1 ),
    ( 0, 3 ), ( 1, 2 ), ( 2, 2 ), ( 6, 1 ), ( 7, 1 ), ( 8, 1 ), ( 9, 1 ), ( 0, 4 )
];
const SVQ3_RUNLEVEL_ALT: [(usize, i16); 16] = [
    ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( 0, 2 ), ( 2, 1 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ),
    ( 3, 1 ), ( 4, 1 ), ( 1, 2 ), ( 1, 3 ), ( 0, 6 ), ( 0, 7 ), ( 0, 8 ), ( 0, 9 )
];
const RUN_ADD: [i16; 16] = [ 4, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0 ];
const RUN_ADD_ALT: [i16; 8] = [ 8, 2, 0, 0, 0, -1, -1, -1 ];

fn decode_alt_slice(br: &mut BitReader, blk: &mut [i16; 16], mut idx: usize, end: usize) -> DecoderResult<bool> {
    let mut coded = false;
    while idx < end {
        let val                         = br.read_code(UintCodeType::Gamma)?;
        if val == 0 { break; }
        let sign = (val & 1) == 0;
        let val = (val + 1) >> 1;
        let (run, level) = if (val as usize) < SVQ3_RUNLEVEL.len() {
                SVQ3_RUNLEVEL_ALT[val as usize]
            } else {
                let run = (val & 0x7) as usize;
                (run, ((val >> 3) as i16) + RUN_ADD_ALT[run.min(RUN_ADD_ALT.len() - 1)])
            };
        idx += run;
        validate!(idx < end);
        blk[ALT_SCAN[idx]] = if sign { -level } else { level };
        coded = true;
        idx += 1;
    }
    if idx == end {
        let val                         = br.read_code(UintCodeType::Gamma)?;
        validate!(val == 0);
    }
    Ok(coded)
}

fn decode_block(br: &mut BitReader, blk: &mut [i16; 16], start: usize, alt: bool) -> DecoderResult<bool> {
    let mut coded = false;
    if !alt {
        let mut idx = start;
        while idx < blk.len() {
            let val                     = br.read_code(UintCodeType::Gamma)?;
            if val == 0 { break; }
            let sign = (val & 1) == 0;
            let val = (val + 1) >> 1;
            let (run, level) = if (val as usize) < SVQ3_RUNLEVEL.len() {
                    SVQ3_RUNLEVEL[val as usize]
                } else {
                    let run = (val & 0xF) as usize;
                    (run, ((val >> 4) as i16) + RUN_ADD[run.min(RUN_ADD.len() - 1)])
                };
            idx += run;
            validate!(idx < blk.len());
            blk[ZIGZAG4[idx]] = if sign { -level } else { level };
            coded = true;
            idx += 1;
        }
        if idx == blk.len() {
            let val                     = br.read_code(UintCodeType::Gamma)?;
            validate!(val == 0);
        }
    } else {
        coded  = decode_alt_slice(br, blk, start,  8)?;
        coded |= decode_alt_slice(br, blk,     8, 16)?;
    }
    Ok(coded)
}

fn decode_chroma_dc(br: &mut BitReader) -> DecoderResult<[i16; 4]> {
    let mut idx = 0;
    let mut blk = [0i16; 4];
    while idx < blk.len() {
        let val                         = br.read_code(UintCodeType::Gamma)?;
        if val == 0 { break; }
        let sign = (val & 1) == 0;
        let val = (val + 1) >> 1;
        let (run, level) = if val < 3 {
                (0, val as i16)
            } else if val == 3 {
                (1, 1)
            } else {
                ((val & 3) as usize, (((val + 9) >> 2) - (val & 3)) as i16)
            };
        idx += run;
        validate!(idx < blk.len());
        blk[idx] = if sign { -level } else { level };
        idx += 1;
    }
    if idx == blk.len() {
        let val                         = br.read_code(UintCodeType::Gamma)?;
        validate!(val == 0);
    }
    Ok(blk)
}

fn read_mv(br: &mut BitReader) -> DecoderResult<MV> {
    let y                               = br.read_code_signed(IntCodeType::Gamma)? as i16;
    let x                               = br.read_code_signed(IntCodeType::Gamma)? as i16;
    Ok(MV{ x, y })
}

fn div6(val: i16) -> i16 {
    (((((i32::from(val) + (6 << 16)) as u32) / 6) as i32) - (1 << 16)) as i16
}

fn scale_mv(mv: MV, trb: u8, trd: u8) -> (MV, MV) {
    let trb = i32::from(trb);
    let trd = i32::from(trd);
    let fx = (i32::from(mv.x * 2) * trb / trd + 1) >> 1;
    let fy = (i32::from(mv.y * 2) * trb / trd + 1) >> 1;
    let bx = (i32::from(mv.x * 2) * (trb - trd) / trd + 1) >> 1;
    let by = (i32::from(mv.y * 2) * (trb - trd) / trd + 1) >> 1;
    (MV { x: fx as i16, y: fy as i16 }, MV { x: bx as i16, y: by as i16 })
}

fn add_mv(pred_mv: MV, dmv: MV, mc_mode: MCMode) -> (MV, usize, &'static [BlkInterpFunc]) {
    match mc_mode {
        MCMode::Pixel => {
            let x = div6(pred_mv.x + 3);
            let y = div6(pred_mv.y + 3);
            let mut mv = MV{ x, y } + dmv;
            mv.x *= 6;
            mv.y *= 6;
            (mv, 0, HALFPEL_INTERP_FUNCS)
        },
        MCMode::Halfpel => {
            let x = div6(pred_mv.x * 2 + 2);
            let y = div6(pred_mv.y * 2 + 2);
            let mut mv = MV{ x, y } + dmv;
            let mode = ((mv.x & 1) + (mv.y & 1) * 2) as usize;
            mv.x *= 3;
            mv.y *= 3;
            (mv, mode, HALFPEL_INTERP_FUNCS)
        },
        MCMode::Thirdpel => {
            let x = (pred_mv.x + 1) >> 1;
            let y = (pred_mv.y + 1) >> 1;
            let mut mv = MV{ x, y } + dmv;
            let mut mx = mv.x % 3;
            if mx < 0 { mx += 3; }
            let mut my = mv.y % 3;
            if my < 0 { my += 3; }
            let mode = (mx + my * 3) as usize;
            mv.x <<= 1;
            mv.y <<= 1;
            (mv, mode, THIRDPEL_INTERP_FUNCS)
       },
    }
}

fn copy_block(dst: &mut NASimpleVideoFrame<u8>, src: NAVideoBufferRef<u8>, ebuf: &mut [u8; 32 * 18], comp: usize,
              dx: usize, dy: usize, mv_x: i16, mv_y: i16, bw: usize, bh: usize,
              preborder: usize, postborder: usize,
              mode: usize, interp: &[BlkInterpFunc])
{
    let pre  = if mode != 0 { preborder  as isize } else { 0 };
    let post = if mode != 0 { postborder as isize } else { 0 };
    let (w, h) = src.get_dimensions(comp);
    let sx = (dx as isize) + (mv_x as isize);
    let sy = (dy as isize) + (mv_y as isize);

    if (sx - pre < 0) || (sx + (bw as isize) + post > (w as isize)) ||
       (sy - pre < 0) || (sy + (bh as isize) + post > (h as isize)) {
        let ebuf_stride: usize = 32;

        let dstride = dst.stride[comp];
        let doff    = dst.offset[comp];
        let edge = (pre + post) as usize;
        edge_emu(&src, sx - pre, sy - pre, bw + edge, bh + edge,
                 ebuf, ebuf_stride, comp, 4);
        (interp[mode])(&mut dst.data[doff + dx + dy * dstride..], dstride,
                       ebuf, ebuf_stride, bw, bh);
    } else {
        let sstride = src.get_stride(comp);
        let soff    = src.get_offset(comp);
        let sdta    = src.get_data();
        let sbuf: &[u8] = sdta.as_slice();
        let dstride = dst.stride[comp];
        let doff    = dst.offset[comp];
        let saddr = soff + ((sx - pre) as usize) + ((sy - pre) as usize) * sstride;
        (interp[mode])(&mut dst.data[doff + dx + dy * dstride..], dstride,
                       &sbuf[saddr..], sstride, bw, bh);
    }
}

fn mc_part(dframe: &mut NASimpleVideoFrame<u8>, src: NAVideoBufferRef<u8>, ebuf: &mut [u8; 32 * 18], xoff: usize, yoff: usize, bw: usize, bh: usize, mv: MV, mode: usize, ifuncs: &[BlkInterpFunc]) {
    let mx = div6(mv.x);
    let my = div6(mv.y);
    let cmx = (mx + if mx < 0 { 1 } else { 0 }) >> 1;
    let cmy = (my + if my < 0 { 1 } else { 0 }) >> 1;
    let post = if mode != 0 { 1 } else { 0 };

    copy_block(dframe, src.clone(), ebuf, 0, xoff,     yoff,     mx,  my,  bw * 4, bh * 4, 0, post, mode, ifuncs);
    copy_block(dframe, src.clone(), ebuf, 1, xoff / 2, yoff / 2, cmx, cmy, bw * 2, bh * 2, 0, post, mode, ifuncs);
    copy_block(dframe, src,         ebuf, 2, xoff / 2, yoff / 2, cmx, cmy, bw * 2, bh * 2, 0, post, mode, ifuncs);
}

impl SVQ3Decoder {
    fn new() -> Self {
        let tmp_vinfo = NAVideoInfo::new(16, 16, false, YUV420_FORMAT);
        let vt = alloc_video_buffer(tmp_vinfo, 4).unwrap();
        let vb = vt.get_vbuf();
        let avg_buf = vb.unwrap();

        Self {
            info:       NACodecInfoRef::default(),
            width:      0,
            height:     0,
            ipbs:       IPBShuffler::new(),
            avg_buf,
            imode:      IntraModeState::new(0),
            mvi:        MVInfo::new(),
            ref_mvi:    MVInfo::new(),
            mbtypes:    Vec::new(),
            ebuf:       [0; 32 * 18],

            coeffs:     [[0; 16]; 25],
            coded:      [false; 24],
            dc_only:    [false; 24],

            use_hpel:   false,
            use_tpel:   false,
            no_bframes: false,
            protected:  false,
            slice_buf:  Vec::new(),
            mb_x:       0,
            mb_y:       0,
            mb_w:       0,
            mb_h:       0,
            ts_fwd:     0,
            ts_bwd:     0,
            pts_base:   0,
            ts_base:    0,
        }
    }
    fn parse_sequence_header(&mut self, src: &[u8]) -> DecoderResult<()> {
        let mut br = BitReader::new(src, BitReaderMode::BE);
        let fcode                       = br.read(3)? as usize;
        let (w, h) = if fcode < FRAME_SIZES.len() {
                FRAME_SIZES[fcode]
            } else {
                let w                   = br.read(12)? as usize;
                let h                   = br.read(12)? as usize;
                validate!(w >= 16 && h >= 16);
                (w, h)
            };
        self.width  = w;
        self.height = h;
        self.use_hpel                   = br.read_bool()?;
        self.use_tpel                   = br.read_bool()?;
                                          br.skip(1)?;
                                          br.skip(1)?;
                                          br.skip(1)?;
                                          br.skip(1)?;
        self.no_bframes                 = br.read_bool()?;
                                          br.skip(1)?;
        while br.read_bool()? {
                                          br.skip(8)?;
        }
        self.protected                  = br.read_bool()?;
//println!(" seq: {}x{} hpel {} tpel {} nob {}", w, h, self.use_hpel, self.use_tpel, self.no_bframes);
        if self.protected {
unimplemented!();
        }
        Ok(())
    }
    fn prepare_slice_buffer(&mut self, src: &[u8]) -> DecoderResult<usize> {
        let llen = ((src[0] >> 5) & 3) as usize;
        validate!(llen != 0);
        validate!(src.len() > llen);
        let length = match llen {
                1 => src[1] as usize,
                2 => (src[1] as usize) * 256 + (src[2] as usize),
                3 => ((src[1] as usize) << 16) + ((src[2] as usize) << 8) + (src[3] as usize),
                _ => unreachable!(),
            };
        let slice_len = length + llen + 1;
        validate!(src.len() >= slice_len);
        self.slice_buf.clear();
        if llen > 1 {
            self.slice_buf.extend_from_slice(&src[slice_len - llen + 1..][..llen - 1]);
        }
        self.slice_buf.extend_from_slice(&src[llen + 1..][..slice_len - llen - 1]);
        // todo unscramble
        Ok(slice_len)
    }
    fn parse_slice_header(&self, br: &mut BitReader, slice_mode: u8) -> DecoderResult<SVQ3Header> {
        let ftype_id                    = br.read_code(UintCodeType::Gamma)? as usize;
        validate!(ftype_id < FRAME_TYPES.len());
        let ftype = FRAME_TYPES[ftype_id];
        if self.no_bframes {
            validate!(ftype != FrameType::B);
        }

        if slice_mode == 1 {
                                          br.skip(1)?;
        } else {
            let mbs = self.mb_w * self.mb_h;
            let mb_bits = if mbs < 64 { 6 } else { 32 - (mbs - 1).leading_zeros() } as u8;
            let _offset                 = br.read(mb_bits)?;
//println!("slice offset {}", _offset);
        }
        let ts                          = br.read(8)? as u8;
        let quant                       = br.read(5)? as u8;
        let dquant                      = br.read_bool()?;
                                          br.skip(1)?;
        if self.protected {
                                          br.skip(1)?;
        }
                                          br.skip(1)?;
                                          br.skip(2)?;
        while br.read_bool()? {
                                          br.skip(8)?;
        }

        Ok(SVQ3Header { ftype, ts, quant, dquant })
    }
    fn decode_intra_block(&mut self, br: &mut BitReader, mb_type: usize, sstate: &mut SState, hdr: &SVQ3Header) -> DecoderResult<()> {
        const INTRA_CBP: [u8; 48] = [
            47, 31, 15,  0, 23, 27, 29, 30,  7, 11, 13, 14, 39, 43, 45, 46,
            16,  3,  5, 10, 12, 19, 21, 26, 28, 35, 37, 42, 44,  1,  2,  4,
            8,  17, 18, 20, 24,  6,  9, 22, 25, 32, 33, 34, 36, 40, 38, 41
        ];
        let is_4x4 = mb_type == 8 || mb_type == 33;
        let cbp;
        if !is_4x4 {
            let angle = SVQ3_INTRA_ANGLE[(mb_type - 9) & 3];
                cbp   = SVQ3_INTRA_CBP[(mb_type - 9) / 4];
            self.imode.i16_pred = angle;
            self.imode.fill_block(2);
        } else if mb_type == 8 {
            for i in 0..8 {
                let idx                 = br.read_code(UintCodeType::Gamma)? as usize;
                validate!(idx < SVQ3_INTRA4_PAIRS.len());
                let mut iidx = self.imode.cache.xpos;
                if (i & 1) != 0 { iidx += self.imode.cache.stride; }
                if (i & 2) != 0 { iidx += 2; }
                if (i & 4) != 0 { iidx += self.imode.cache.stride * 2; }

                let t0 = self.imode.cache.data[iidx     - self.imode.cache.stride];
                let t1 = self.imode.cache.data[iidx + 1 - self.imode.cache.stride];
                let l0 = self.imode.cache.data[iidx - 1];

                let p = SVQ3_INTRA4_PAIRS[idx];
                self.imode.cache.data[iidx] = SVQ3_INTRA4_CTX_PRED[(t0 + 1) as usize][(l0 + 1) as usize][p[0] as usize];
                let l1 = self.imode.cache.data[iidx];
                self.imode.cache.data[iidx + 1] = SVQ3_INTRA4_CTX_PRED[(t1 + 1) as usize][(l1 + 1) as usize][p[1] as usize];
                validate!(self.imode.cache.data[iidx] != -1);
                validate!(self.imode.cache.data[iidx + 1] != -1);
            }
            let idx                     = br.read_code(UintCodeType::Gamma)? as usize;
            validate!(idx < INTRA_CBP.len());
            cbp = INTRA_CBP[idx];
            self.imode.i16_pred = 0;
        } else {
            self.imode.fill_block(2);
            cbp = 0;
        }

        if !is_4x4 || (hdr.dquant && hdr.ftype != FrameType::I && cbp != 0) {
            let dq                      = br.read_code_signed(IntCodeType::Gamma)?;
            let new_q = i32::from(sstate.q) + dq;
            validate!((0..32).contains(&new_q));
            sstate.q = new_q as u8;
        }
        if !is_4x4 {
            decode_block(br, &mut self.coeffs[24], 0, false)?;
            idct_dc_coeffs(&mut self.coeffs[24], sstate.q);
        }
        let start = if is_4x4 { 0 } else { 1 };
        let alt = sstate.q < 24 && is_4x4;
        for sb in 0..4 {
            if ((cbp >> sb) & 1) == 0 { continue; }
            for b in 0..4 {
                let blk_idx = if is_4x4 {
                        (sb & 1) * 2 + (sb >> 1) * 8 + (b & 1) + (b >> 1) * 4
                    } else {
                        sb * 4 + b
                    };
                self.coded[blk_idx] = decode_block(br, &mut self.coeffs[blk_idx], start, alt)?;
                if is_4x4 && self.coded[blk_idx] {
                    idct_dc_coeffs(&mut self.coeffs[blk_idx], sstate.q);
                }
            }
        }
        if !is_4x4 {
            for blk_idx in 0..16 {
                self.coeffs[blk_idx][0] = self.coeffs[24][blk_idx];
                self.dc_only[blk_idx] = self.coeffs[blk_idx][0] != 0;
                idct(&mut self.coeffs[blk_idx], sstate.q, false);
            }
        }
        if (cbp & 0x30) != 0 {
            let mut u_dc = decode_chroma_dc(br)?;
            let mut v_dc = decode_chroma_dc(br)?;
            chroma_transform(&mut u_dc);
            chroma_transform(&mut v_dc);

            let cdcs = [u_dc, v_dc];
            if (cbp & 0x20) != 0 {
                for comp in 0..2 {
                    for i in 0..4 {
                        let blk_idx = 16 + comp * 4 + i;
                        self.coded[blk_idx] = decode_block(br, &mut self.coeffs[blk_idx], 1, false)?;
                    }
                }
            }
            for comp in 0..2 {
                for i in 0..4 {
                    let blk_idx = 16 + comp * 4 + i;
                    self.coeffs[blk_idx][0] = cdcs[comp][i];
                    self.dc_only[blk_idx] = cdcs[comp][i] != 0;
                    idct(&mut self.coeffs[blk_idx], SVQ3_CHROMA_QUANT[sstate.q as usize], true);
                }
            }
        }

        Ok(())
    }
    fn do_mc_p(&mut self, br: &mut BitReader, mb_type: usize, sstate: &mut SState, dframe: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        if mb_type == 0 {
            self.mvi.fill(self.mb_x, self.mb_y, true, ZERO_MV);
            if let Some(ref_frm) = self.ipbs.get_lastref() {
                mc_part(dframe, ref_frm, &mut self.ebuf, self.mb_x * 16, self.mb_y * 16, 4, 4, ZERO_MV, 0, HALFPEL_INTERP_FUNCS);
            }
            return Ok(());
        }
        let mc_mode = if self.use_tpel && br.read_bool()? != self.use_hpel {
                MCMode::Thirdpel
            } else if self.use_hpel && br.read_bool()? != self.use_tpel {
                MCMode::Halfpel
            } else {
                MCMode::Pixel
            };
        let (bw, bh) = SVQ3_PART_SIZES[mb_type];
        let bw = (bw >> 2) as usize;
        let bh = (bh >> 2) as usize;

        let mut avail = [false; 6 * 5];
        avail[0] = sstate.has_tl;
        if sstate.has_top {
            avail[1] = true;
            avail[2] = true;
            avail[3] = true;
            avail[4] = true;
        }
        avail[5] = sstate.has_tr;
        if sstate.has_left {
            avail[6 * 1] = true;
            avail[6 * 2] = true;
            avail[6 * 3] = true;
            avail[6 * 4] = true;
        }

        let dir = true; //forward
        for y in (0..16).step_by(bh * 4) {
            for x in (0..16).step_by(bw * 4) {
                let mv_x = x >> 2;
                let mv_y = y >> 2;
                let xpos = self.mb_x * 16 + x;
                let ypos = self.mb_y * 16 + y;

                let avail_idx = mv_x + 1 + (mv_y + 1) * 6;
                let mut pred_mv = self.mvi.pred_mv_part(self.mb_x * 4 + mv_x, self.mb_y * 4 + mv_y, bw, dir, avail[avail_idx - 6], avail[avail_idx - 1], avail[avail_idx - 6 + bw], avail[avail_idx - 6 - 1]);
                pred_mv.x = pred_mv.x.max(-6 * (xpos as i16)).min(6 * ((((self.width  + 15) & !15) - xpos - bw * 4) as i16));
                pred_mv.y = pred_mv.y.max(-6 * (ypos as i16)).min(6 * ((((self.height + 15) & !15) - ypos - bh * 4) as i16));

                for j in 0..bh {
                    for i in 0..bw {
                        avail[avail_idx + i + j * 6] = true;
                    }
                }

                let dmv = read_mv(br)?;
                let (mv, mode, ifuncs) = add_mv(pred_mv, dmv, mc_mode);
                self.mvi.fill_part(self.mb_x * 4 + mv_x, self.mb_y * 4 + mv_y, dir, bw, bh, mv);
                if let Some(ref_frm) = self.ipbs.get_lastref() {
                    mc_part(dframe, ref_frm, &mut self.ebuf, xpos, ypos, bw, bh, mv, mode, ifuncs);
                }
            }
        }
        Ok(())
    }
    fn do_mc_direct(&mut self, sstate: &mut SState, dframe: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        let mut ref_mbt = self.mbtypes[self.mb_x + self.mb_y * self.mb_w];
        if ref_mbt >= 8 {
            ref_mbt = 0;
        }
        let (bw, bh) = SVQ3_PART_SIZES[ref_mbt as usize];
        let bw = (bw >> 2) as usize;
        let bh = (bh >> 2) as usize;
        let mut aframe = NASimpleVideoFrame::from_video_buf(&mut self.avg_buf).unwrap();
        if let (Some(fwd_ref), Some(bwd_ref)) = (self.ipbs.get_b_fwdref(), self.ipbs.get_b_bwdref()) {
            for y in (0..16).step_by(bh * 4) {
                for x in (0..16).step_by(bw * 4) {
                    let mv = self.ref_mvi.get_mv(self.mb_x * 4 + x / 4, self.mb_y * 4 + y / 4, true);
                    let (fwd_pred, bwd_pred) = scale_mv(mv, sstate.trb, sstate.trd);
                    let (fmv, fmode, _) = add_mv(fwd_pred, ZERO_MV, MCMode::Halfpel);
                    let (bmv, bmode, _) = add_mv(bwd_pred, ZERO_MV, MCMode::Halfpel);
                    self.mvi.fill_part(self.mb_x * 4 + x / 4, self.mb_y * 4 + y / 4, true,  bw, bh, fmv);
                    self.mvi.fill_part(self.mb_x * 4 + x / 4, self.mb_y * 4 + y / 4, false, bw, bh, bmv);

                    let xoff = self.mb_x * 16 + x;
                    let yoff = self.mb_y * 16 + y;
                    mc_part(dframe, fwd_ref.clone(), &mut self.ebuf, xoff, yoff, bw, bh, fmv, fmode, HALFPEL_INTERP_FUNCS);

                    let amv = MV { x: bmv.x + (xoff as i16) * 6, y: bmv.y + (yoff as i16) * 6 };
                    mc_part(&mut aframe, bwd_ref.clone(), &mut self.ebuf, 0, 0, bw, bh, amv, bmode, HALFPEL_INTERP_FUNCS);

                    let dstride = dframe.stride[0];
                    let dst = &mut dframe.data[dframe.offset[0] + xoff + yoff * dstride..];
                    let src = &aframe.data;
                    let sstride = aframe.stride[0];
                    avg(dst, dstride, src, sstride, bw * 4, bh * 4);

                    let dstride = dframe.stride[1];
                    let dst = &mut dframe.data[dframe.offset[1] + xoff / 2 + yoff / 2 * dstride..];
                    let sstride = aframe.stride[1];
                    avg(dst, dstride, &src[aframe.offset[1]..], sstride, bw * 2, bh * 2);

                    let dstride = dframe.stride[2];
                    let dst = &mut dframe.data[dframe.offset[2] + xoff / 2 + yoff / 2 * dstride..];
                    let sstride = aframe.stride[2];
                    avg(dst, dstride, &src[aframe.offset[2]..], sstride, bw * 2, bh * 2);
                }
            }
        }
        Ok(())
    }
    fn do_mc_b(&mut self, br: &mut BitReader, mb_type: usize, sstate: &mut SState, dframe: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        let mc_mode = if self.use_tpel && br.read_bool()? != self.use_hpel {
                MCMode::Thirdpel
            } else if self.use_hpel && br.read_bool()? != self.use_tpel {
                MCMode::Halfpel
            } else {
                MCMode::Pixel
            };
        let fwd_pred = self.mvi.pred_mv_part(self.mb_x * 4, self.mb_y * 4, 4, true,  sstate.has_top, sstate.has_left, sstate.has_tr, sstate.has_tl);
        let bwd_pred = self.mvi.pred_mv_part(self.mb_x * 4, self.mb_y * 4, 4, false, sstate.has_top, sstate.has_left, sstate.has_tr, sstate.has_tl);
        let (fdmv, bdmv) = match mb_type {
                1 => {
                    let dmv             = read_mv(br)?;
                    (dmv, ZERO_MV)
                },
                2 => {
                    let dmv             = read_mv(br)?;
                    (ZERO_MV, dmv)
                },
                3 => {
                    let fdmv            = read_mv(br)?;
                    let bdmv            = read_mv(br)?;
                    (fdmv, bdmv)
                },
                _ => unreachable!(),
            };
        let (fmv, fmode, ifuncs) = add_mv(fwd_pred, fdmv, mc_mode);
        let (bmv, bmode, _)      = add_mv(bwd_pred, bdmv, mc_mode);
        let has_fwd = mb_type != 2;
        let has_bwd = mb_type != 1;
        if has_fwd {
            self.mvi.fill(self.mb_x, self.mb_y, true, fmv);
        }
        if has_bwd {
            self.mvi.fill(self.mb_x, self.mb_y, false, bmv);
        }
        let refframe = if has_fwd { self.ipbs.get_b_fwdref() } else { self.ipbs.get_b_bwdref() };
        if let Some(ref_buf) = refframe {
            let (mv, mode) = if has_fwd { (fmv, fmode) } else { (bmv, bmode) };
            mc_part(dframe, ref_buf, &mut self.ebuf, self.mb_x * 16, self.mb_y * 16, 4, 4, mv, mode, ifuncs);
        }
        if let (Some(bwd_ref), true, true) = (self.ipbs.get_b_bwdref(), has_fwd, has_bwd) {
            let mut aframe = NASimpleVideoFrame::from_video_buf(&mut self.avg_buf).unwrap();
            let amv = MV { x: bmv.x + (self.mb_x as i16) * 16 * 6, y: bmv.y + (self.mb_y as i16) * 16 * 6 };
            mc_part(&mut aframe, bwd_ref, &mut self.ebuf, 0, 0, 4, 4, amv, bmode, ifuncs);

            let dstride = dframe.stride[0];
            let dst = &mut dframe.data[dframe.offset[0] + self.mb_x * 16 + self.mb_y * 16 * dstride..];
            let src = self.avg_buf.get_data();
            let sstride = self.avg_buf.get_stride(0);
            avg(dst, dstride, src, sstride, 16, 16);

            let dstride = dframe.stride[1];
            let dst = &mut dframe.data[dframe.offset[1] + self.mb_x * 8 + self.mb_y * 8 * dstride..];
            let sstride = self.avg_buf.get_stride(1);
            avg(dst, dstride, &src[self.avg_buf.get_offset(1)..], sstride, 8, 8);

            let dstride = dframe.stride[2];
            let dst = &mut dframe.data[dframe.offset[2] + self.mb_x * 8 + self.mb_y * 8 * dstride..];
            let sstride = self.avg_buf.get_stride(2);
            avg(dst, dstride, &src[self.avg_buf.get_offset(2)..], sstride, 8, 8);
        }
        Ok(())
    }
    fn decode_inter_block(&mut self, br: &mut BitReader, mb_type: usize, sstate: &mut SState, hdr: &SVQ3Header, dframe: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        const INTER_CBP: [u8; 48] = [
             0, 16,  1,  2,  4,  8, 32,  3,  5, 10, 12, 15, 47,  7, 11, 13,
            14,  6,  9, 31, 35, 37, 42, 44, 33, 34, 36, 40, 39, 43, 45, 46,
            17, 18, 20, 24, 19, 21, 26, 28, 23, 27, 29, 30, 22, 25, 38, 41
        ];
        if hdr.ftype == FrameType::P {
            self.do_mc_p(br, mb_type, sstate, dframe)?;
            if mb_type == 0 {
                return Ok(());
            }
        } else if mb_type != 0 {
            self.do_mc_b(br, mb_type, sstate, dframe)?;
        } else {
            self.do_mc_direct(sstate, dframe)?;
        }

        let idx                     = br.read_code(UintCodeType::Gamma)? as usize;
        validate!(idx < INTER_CBP.len());
        let cbp = INTER_CBP[idx];

        if hdr.dquant && cbp != 0 {
            let dq                      = br.read_code_signed(IntCodeType::Gamma)?;
            let new_q = i32::from(sstate.q) + dq;
            validate!((0..32).contains(&new_q));
            sstate.q = new_q as u8;
        }
        for sb in 0..4 {
            if ((cbp >> sb) & 1) == 0 { continue; }
            for b in 0..4 {
                let blk_idx = (sb & 1) * 2 + (sb >> 1) * 8 + (b & 1) + (b >> 1) * 4;
                self.coded[blk_idx] = decode_block(br, &mut self.coeffs[blk_idx], 0, false)?;
                if self.coded[blk_idx] {
                    idct_dc_coeffs(&mut self.coeffs[blk_idx], sstate.q);
                }
            }
        }
        if (cbp & 0x30) != 0 {
            let mut u_dc = decode_chroma_dc(br)?;
            let mut v_dc = decode_chroma_dc(br)?;
            chroma_transform(&mut u_dc);
            chroma_transform(&mut v_dc);

            let cdcs = [u_dc, v_dc];
            if (cbp & 0x20) != 0 {
                for comp in 0..2 {
                    for i in 0..4 {
                        let blk_idx = 16 + comp * 4 + i;
                        self.coded[blk_idx] = decode_block(br, &mut self.coeffs[blk_idx], 1, false)?;
                    }
                }
            }
            for comp in 0..2 {
                for i in 0..4 {
                    let blk_idx = 16 + comp * 4 + i;
                    self.coeffs[blk_idx][0] = cdcs[comp][i];
                    self.dc_only[blk_idx] = cdcs[comp][i] != 0;
                    idct(&mut self.coeffs[blk_idx], SVQ3_CHROMA_QUANT[sstate.q as usize], true);
                }
            }
        }

        Ok(())
    }
    fn put_intra4x4(&self, dframe: &mut NASimpleVideoFrame<u8>, sstate: &SState) {
        let dst = &mut dframe.data[0..];
        let stride = dframe.stride[0];
        let mut doff = dframe.offset[0] + self.mb_x * 16 + self.mb_y * 16 * stride;
        for y in 0..4 {
            for x in 0..4 {
                let im = self.imode.get_pred4_type(x, y, sstate.has_top, sstate.has_left);
                let noright = (self.mb_x == self.mb_w - 1) && (x == 3);
                let has_top = sstate.has_top || (y > 0);
                let topright: [u8; 4] = if (noright && sstate.has_top && y == 0) || (x == 3 && y > 0) {
                        let i = doff + x * 4 - stride;
                        [dst[i + 3], dst[i + 3], dst[i + 3], dst[i + 3]]
                    } else if has_top {
                        let i = doff + x * 4 - stride;
                        [dst[i + 4], dst[i + 5], dst[i + 6], dst[i + 7]]
                    } else {
                        [0; 4]
                    };
                IPRED_FUNCS4X4[im as usize](dst, doff + x * 4, stride, &topright);
                let blk_idx = x + y * 4;
                if self.dc_only[blk_idx] || self.coded[blk_idx] {
                    add_coeffs(dst, doff + x * 4, stride, &self.coeffs[blk_idx]);
                }
            }
            doff += stride * 4;
        }
        let im8 = self.imode.get_pred8_type(sstate.has_top, sstate.has_left);
        for comp in 1..3 {
            let stride = dframe.stride[comp];
            let mut doff = dframe.offset[comp] + self.mb_x * 8 + self.mb_y * 8 * stride;
            IPRED_FUNCS8X8[im8 as usize](dst, doff, stride);
            for y in 0..2 {
                for x in 0..2 {
                    let blk_idx = 16 + (comp - 1) * 4 + x + y * 2;
                    if self.dc_only[blk_idx] || self.coded[blk_idx] {
                        add_coeffs(dst, doff + x * 4, stride, &self.coeffs[blk_idx]);
                    }
                }
                doff += stride * 4;
            }
        }
    }
    fn put_residue(&self, dframe: &mut NASimpleVideoFrame<u8>) {
        let dst = &mut dframe.data[0..];
        let stride = dframe.stride[0];
        let mut doff = dframe.offset[0] + self.mb_x * 16 + self.mb_y * 16 * stride;
        for y in 0..4 {
            for x in 0..4 {
                let blk_idx = x + y * 4;
                if self.dc_only[blk_idx] || self.coded[blk_idx] {
                    add_coeffs(dst, doff + x * 4, stride, &self.coeffs[blk_idx]);
                }
            }
            doff += stride * 4;
        }
        for comp in 1..3 {
            let stride = dframe.stride[comp];
            let mut doff = dframe.offset[comp] + self.mb_x * 8 + self.mb_y * 8 * stride;
            for y in 0..2 {
                for x in 0..2 {
                    let blk_idx = 16 + (comp - 1) * 4 + x + y * 2;
                    if self.dc_only[blk_idx] || self.coded[blk_idx] {
                        add_coeffs(dst, doff + x * 4, stride, &self.coeffs[blk_idx]);
                    }
                }
                doff += stride * 4;
            }
        }
    }
    fn decode_slice(&mut self, br: &mut BitReader, hdr: &SVQ3Header, dframe: &mut NASimpleVideoFrame<u8>) -> DecoderResult<()> {
        let mut mb_idx = self.mb_x + self.mb_y * self.mb_w;
        let mbs = self.mb_w * self.mb_h;
        let mut sstate = SState::default();
        sstate.q = hdr.quant;
        if hdr.ftype == FrameType::B {
            sstate.trd = self.ts_bwd.wrapping_sub(self.ts_fwd).max(1);
            sstate.trb = hdr.ts.wrapping_sub(self.ts_fwd).max(1);
        }
        let start_idx = mb_idx;
        self.imode.reset();
        while mb_idx < mbs {
            sstate.has_top = mb_idx - start_idx >= self.mb_w;
            if sstate.has_top {
                sstate.has_tl = sstate.has_left && mb_idx - start_idx + 1 > self.mb_w;
                sstate.has_tr = self.mb_x + 1 < self.mb_w && mb_idx - start_idx >= self.mb_w - 1;
            } else {
                sstate.has_tl = false;
                sstate.has_tr = false;
            }

            if br.left() < 8 {
                if (br.tell() & 7) == 0 && br.peek(8) == 0 {
                    return Ok(());
                }
            }
            let mut mb_type             = br.read_code(UintCodeType::Gamma)? as usize;
            if hdr.ftype == FrameType::I {
                mb_type += 8;
            }
            if hdr.ftype == FrameType::B && mb_type >= 4 {
                mb_type += 4;
            }
            validate!(mb_type <= 33);
            self.imode.set_mb_x(self.mb_x);
            self.coeffs = [[0; 16]; 25];
            self.coded = [false; 24];
            self.dc_only = [false; 24];
            if hdr.ftype != FrameType::B {
                self.mbtypes[mb_idx] = mb_type as u8;
            }
            if mb_type < 8 {
                validate!(hdr.ftype != FrameType::I);
                self.imode.fill_block(2);
                self.decode_inter_block(br, mb_type, &mut sstate, hdr, dframe)?;
                self.put_residue(dframe);
            } else {
                self.decode_intra_block(br, mb_type, &mut sstate, hdr)?;
                let is_4x4 = mb_type == 8 || mb_type == 33;
                if is_4x4 {
                    self.put_intra4x4(dframe, &sstate);
                } else {
                    let im16 = self.imode.get_pred16_type(sstate.has_top, sstate.has_left);
                    let stride = dframe.stride[0];
                    let doff = dframe.offset[0] + self.mb_x * 16 + self.mb_y * 16 * stride;
                    IPRED_FUNCS16X16[im16 as usize](dframe.data, doff, stride);
                    let im8 = self.imode.get_pred8_type(sstate.has_top, sstate.has_left);
                    for comp in 1..3 {
                        let stride = dframe.stride[comp];
                        let doff = dframe.offset[comp] + self.mb_x * 8 + self.mb_y * 8 * stride;
                        IPRED_FUNCS8X8[im8 as usize](dframe.data, doff, stride);
                    }
                    self.put_residue(dframe);
                }
                self.mvi.fill(self.mb_x, self.mb_y, true,  ZERO_MV);
                self.mvi.fill(self.mb_x, self.mb_y, false, ZERO_MV);
            }
            sstate.has_left = true;
            self.mb_x += 1;
            if self.mb_x == self.mb_w {
                self.mb_x = 0;
                self.mb_y += 1;
                sstate.has_left = false;
                self.imode.update();
            }
            mb_idx += 1;
        }
        Ok(())
    }
}

const FRAME_SIZES: [(usize, usize); 7] = [
    (160, 120), (128,  96), (176, 144), (352, 288),
    (704, 576), (240, 180), (320, 240)
];
const FRAME_TYPES: [FrameType; 3] = [ FrameType::P, FrameType::B, FrameType::I ];

impl NADecoder for SVQ3Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            if let Some(ref edata) = info.get_extradata() {
                let mut start = edata.len();
                for i in 0..edata.len() - 9 {
                    if &edata[i..][..4] == b"SEQH" {
                        let size = read_u32be(&edata[i + 4..])? as usize;
                        validate!(i + 8 + size <= edata.len());
                        start = i + 8;
                        break;
                    }
                }
                if start < edata.len() {
                    self.parse_sequence_header(&edata[start..])?;
                } else {
                    return Err(DecoderError::InvalidData);
                }
            } else {
                return Err(DecoderError::InvalidData);
            }
            let myinfo = NAVideoInfo::new(self.width, self.height, false, YUV420_FORMAT);
            self.info = NACodecInfo::new_ref(info.get_name(), NACodecTypeInfo::Video(myinfo), info.get_extradata()).into_ref();
            supp.pool_u8.set_dec_bufs(3);
            supp.pool_u8.prealloc_video(myinfo, 4)?;

            self.mb_w = (self.width  + 15) >> 4;
            self.mb_h = (self.height + 15) >> 4;
            self.imode = IntraModeState::new(self.mb_w);

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 0);

        if src.len() == 1 {
            validate!(src[0] == 0xFF);
            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::None);
            frm.set_keyframe(false);
            frm.set_frame_type(FrameType::Skip);
            return Ok(frm.into_ref());
        }

        let slice_mode = src[0] & 0x9F;
        validate!(slice_mode == 1 || slice_mode == 2);
        let mut slice_len = self.prepare_slice_buffer(src.as_slice())?;

        let mut br = BitReader::new(&self.slice_buf, BitReaderMode::BE);
        let frame_hdr = self.parse_slice_header(&mut br, slice_mode)?;

        let ret = supp.pool_u8.get_free();
        if ret.is_none() {
            return Err(DecoderError::AllocError);
        }

        let mut buf = ret.unwrap();
        let mut dframe = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();

        match frame_hdr.ftype {
            FrameType::I => {
            },
            FrameType::P => {
                if self.ipbs.get_lastref().is_none() {
                    return Err(DecoderError::MissingReference);
                }
            },
            FrameType::B => {
                if self.ipbs.get_b_fwdref().is_none() || self.ipbs.get_b_bwdref().is_none() {
                    return Err(DecoderError::MissingReference);
                }
            },

            _ => unreachable!(),
        }

        self.mb_x = 0;
        self.mb_y = 0;

        self.mvi.resize(self.mb_w, self.mb_h);
        if frame_hdr.ftype != FrameType::B {
            self.mbtypes.resize(self.mb_w * self.mb_h, 0);
        }

        let mut slice_prepared = true;
        let mut off = 0;
        while off < src.len() {
            if src[off] == 0xFF { break; }

            let slice_mode = src[off] & 0x9F;
            validate!(slice_mode == 1 || slice_mode == 2);
            if !slice_prepared {
                slice_len = self.prepare_slice_buffer(&src[off..])?;
            }
            let mut sbuf = Vec::new();
            std::mem::swap(&mut sbuf, &mut self.slice_buf);
            let mut br = BitReader::new(sbuf.as_slice(), BitReaderMode::BE);
            let ret = self.parse_slice_header(&mut br, slice_mode);
            if let Err(err) = ret {
                std::mem::swap(&mut sbuf, &mut self.slice_buf);
                return Err(err);
            }
            let hdr = ret.unwrap();
            if hdr.ftype != frame_hdr.ftype || hdr.ts != frame_hdr.ts {
                std::mem::swap(&mut sbuf, &mut self.slice_buf);
                return Err(DecoderError::InvalidData);
            }
            let ret = self.decode_slice(&mut br, &hdr, &mut dframe);
            std::mem::swap(&mut sbuf, &mut self.slice_buf);
            ret?;
            slice_prepared = false;
            off += slice_len;
        }

        validate!(self.mb_x == 0 && self.mb_y == self.mb_h);

        if frame_hdr.ftype != FrameType::B {
            self.ipbs.add_frame(buf.clone());
            std::mem::swap(&mut self.mvi, &mut self.ref_mvi);
            self.ts_fwd = self.ts_bwd;
            self.ts_bwd = frame_hdr.ts;

            self.pts_base = pkt.get_pts().unwrap_or(0);
            self.ts_base = frame_hdr.ts;
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(frame_hdr.ftype == FrameType::I);
        frm.set_frame_type(frame_hdr.ftype);
        if !self.no_bframes && frm.get_pts().is_some() {
            let pts = self.pts_base - u64::from(self.ts_base.wrapping_sub(frame_hdr.ts));
            frm.set_pts(Some(pts));
        }
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.ipbs.clear();
        self.ts_fwd = 0;
        self.ts_bwd = 0;
    }
}

impl NAOptionHandler for SVQ3Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(SVQ3Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_svq3() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

//let file = "assets/QT/cristinreel.mov";
//let file = "assets/QT/broken_sword_Large.mov";
//test_file_decoding("mov", file, Some(264), true, false, Some("svq3"), &dmx_reg, &dec_reg);
//panic!("end");
        // sample: https://samples.mplayerhq.hu/V-codecs/SVQ3/broken_sword_Large.mov
        test_decoding("mov", "sorenson-video3", "assets/QT/broken_sword_Large.mov", Some(40), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                        [0x16924d18, 0xccc5a0b4, 0xc2bb9412, 0x93d41f10],
                        [0x84cccf62, 0x0762a61c, 0xe0b1d369, 0x211f066e],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0x8f9e157e, 0xb61f5864, 0x49cc29a7, 0xa2b648a4],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xb7a27005, 0xd22f8f4d, 0x8414d8e2, 0x84be8fda],
                        [0xad96c999, 0x89bfe564, 0x476f918a, 0xf89bb023],
                        [0x1f40cce7, 0xcccc68b3, 0x2a0b28b1, 0x3210675c],
                        [0x3165e832, 0xcb7dad5b, 0x295983fa, 0x270acdcd],
                        [0x54b88d2a, 0x97c5ad60, 0x9cca1823, 0x458566e6],
                        [0xbdd02a56, 0x7ee24530, 0x32262d19, 0x2f3c8237],
                        [0x4e898806, 0x85fb7504, 0x19da4747, 0x00c55a0e],
                        [0x5899e1f5, 0x667fbc86, 0xcbeeff49, 0x6ac996b9],
                        [0x1b640dd6, 0xa999c0f6, 0x57cb9bed, 0xf0265669],
                        [0x3cee4540, 0x35ce897b, 0x9db825aa, 0xd6204c2c],
                        [0x459bfa2f, 0x451555e6, 0x08681f32, 0xf56cdc05],
                        [0x78018c23, 0x333f1892, 0xabab4889, 0x3e3cf020],
                        [0x2a24d296, 0xc572a5fe, 0x0af6a85a, 0x5721bfc4],
                        [0xdf354969, 0xfbf01155, 0xa1e6d53a, 0x49334823],
                        [0x5e493eb2, 0xc92258b8, 0xcec5e684, 0x92bd0f3c],
                        [0x5bf8ea79, 0xb363c077, 0x05c461a3, 0xa065da2c]]));
    }
}

const SVQ3_INTRA_ANGLE: [i8; 4] = [ 0, 2, 1, 3 ];
const SVQ3_INTRA_CBP: [u8; 6] = [ 0x00, 0x10, 0x20, 0x0F, 0x1F, 0x2F ];

const SVQ3_INTRA4_PAIRS: [[u8; 2]; 25] = [
    [ 0, 0 ],
    [ 1, 0 ], [ 0, 1 ],
    [ 0, 2 ], [ 1, 1 ], [ 2, 0 ],
    [ 3, 0 ], [ 2, 1 ], [ 1, 2 ], [ 0, 3 ],
    [ 0, 4 ], [ 1, 3 ], [ 2, 2 ], [ 3, 1 ], [ 4, 0 ],
    [ 4, 1 ], [ 3, 2 ], [ 2, 3 ], [ 1, 4 ],
    [ 2, 4 ], [ 3, 3 ], [ 4, 2 ],
    [ 4, 3 ], [ 3, 4 ],
    [ 4, 4 ]
];

const SVQ3_INTRA4_CTX_PRED: [[[i8; 5]; 6]; 6] = [
  [
    [ 2, -1, -1, -1, -1 ], [ 2, 1, -1, -1, -1 ], [ 1, 2, -1, -1, -1 ],
    [ 2,  1, -1, -1, -1 ], [ 1, 2, -1, -1, -1 ], [ 1, 2, -1, -1, -1 ]
  ], [
    [ 0,  2, -1, -1, -1 ], [ 0, 2,  1,  4,  3 ], [ 0, 1,  2,  4,  3 ],
    [ 0,  2,  1,  4,  3 ], [ 2, 0,  1,  3,  4 ], [ 0, 4,  2,  1,  3 ]
  ], [
    [ 2,  0, -1, -1, -1 ], [ 2, 1,  0,  4,  3 ], [ 1, 2,  4,  0,  3 ],
    [ 2,  1,  0,  4,  3 ], [ 2, 1,  4,  3,  0 ], [ 1, 2,  4,  0,  3 ]
  ], [
    [ 2,  0, -1, -1, -1 ], [ 2, 0,  1,  4,  3 ], [ 1, 2,  0,  4,  3 ],
    [ 2,  1,  0,  4,  3 ], [ 2, 1,  3,  4,  0 ], [ 2, 4,  1,  0,  3 ]
  ], [
    [ 0,  2, -1, -1, -1 ], [ 0, 2,  1,  3,  4 ], [ 1, 2,  3,  0,  4 ],
    [ 2,  0,  1,  3,  4 ], [ 2, 1,  3,  0,  4 ], [ 2, 0,  4,  3,  1 ]
  ], [
    [ 0,  2, -1, -1, -1 ], [ 0, 2,  4,  1,  3 ], [ 1, 4,  2,  0,  3 ],
    [ 4,  2,  0,  1,  3 ], [ 2, 0,  1,  4,  3 ], [ 4, 2,  1,  0,  3 ]
  ]
];

const SVQ3_PART_SIZES: [(u8, u8); 8] = [
    (16, 16), (16, 16), (8, 16), (16, 8), (8, 8), (4, 8), (8, 4), (4, 4)
];

const SVQ3_CHROMA_QUANT: [u8; 32] = [
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    16, 17, 17, 18, 19, 20, 20, 21, 22, 22, 23, 23, 24, 24, 25, 25
];
