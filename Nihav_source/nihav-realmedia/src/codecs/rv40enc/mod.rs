use std::collections::VecDeque;
use std::str::FromStr;

use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitwriter::*;

mod bitstream;
use bitstream::*;

mod dsp;
use dsp::loop_filter_frame;

mod estimator;
use estimator::*;

mod mb_coding;
use mb_coding::*;

mod motion_est;
use motion_est::*;

mod ratectl;
use ratectl::*;
pub use ratectl::RateDistMetric;

mod types;
pub use types::*;

const DEBUG_BIT_FRAMENO:        u8 = 0;
const DEBUG_BIT_SLICE_SIZE:     u8 = 1;
const DEBUG_BIT_PSNR:           u8 = 2;
const DEBUG_BIT_RATECTL:        u8 = 3;
const DEBUG_FLAG_BITS: &[(&str, u8)] = &[
    ("frameno", DEBUG_BIT_FRAMENO),
    ("slicesize", DEBUG_BIT_SLICE_SIZE),
    ("psnr", DEBUG_BIT_PSNR),
    ("rc", DEBUG_BIT_RATECTL),
];

#[derive(Clone,Copy,Default)]
struct DebugFlags {
    flags:  u32,
}

impl DebugFlags {
    fn new() -> Self { Self::default() }
    fn is_set(self, bit: u8) -> bool { (self.flags & (1 << bit)) != 0 }
    fn parse(&mut self, args: &str) {
        self.flags = 0;
        for arg in args.split('+') {
            for &(name, bit) in DEBUG_FLAG_BITS.iter() {
                if name == arg {
                    self.flags += 1 << bit;
                }
            }
        }
    }
}

impl std::fmt::Display for DebugFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut flags = String::new();
        let mut first = true;
        for &(name, bit) in DEBUG_FLAG_BITS.iter() {
            if self.is_set(bit) {
                if !first {
                    flags.push('+');
                }
                flags.push_str(name);
                first = false;
            }
        }
        write!(f, "{}", flags)
    }
}

struct StaticFrameOrder {
    groups:     Vec<(FrameType, usize)>,
    start:      bool,
    cur_grp:    usize,
    cur_frm:    usize,
}

impl StaticFrameOrder {
    /*fn new() -> Self {
        Self {
            groups:     vec![(FrameType::I, 0)],
            start:      true,
            cur_grp:    0,
            cur_frm:    0,
        }
    }*/
    fn get_max_grp_len(&self) -> usize {
        let mut g_len = 1;
        for &(_, num_b) in self.groups.iter() {
            g_len = g_len.max(1 + num_b);
        }
        g_len
    }
    fn peek_next_frame(&self) -> (FrameType, usize) {
        if !self.start {
            let grp = &self.groups[self.cur_grp];
            if self.cur_frm == 0 {
                (grp.0, grp.1)
            } else {
                (FrameType::B, 0)
            }
        } else {
            (FrameType::I, 0)
        }
    }
    fn next_frame(&mut self) -> FrameType {
        if !self.start {
            let grp = &self.groups[self.cur_grp];
            let frm_type = if self.cur_frm == 0 {
                    grp.0
                } else {
                    FrameType::B
                };
            self.cur_frm += 1;
            if self.cur_frm > grp.1 {
                self.cur_frm = 0;
                self.cur_grp += 1;
                if self.cur_grp >= self.groups.len() {
                    self.cur_grp = 0;
                }
            }
            frm_type
        } else {
            self.start = false;
            self.cur_grp = if self.groups.len() > 1 { 1 } else { 0 };
            self.cur_frm = 0;
            FrameType::I
        }
    }
}

impl std::fmt::Display for StaticFrameOrder {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut seq = String::with_capacity(self.groups.len() * 2);
        for &(ftype, brun) in self.groups.iter() {
            seq.push(match ftype {
                    FrameType::I => 'I',
                    _ => 'P',
                });
            for _ in 0..brun {
                seq.push('B');
            }
        }
        write!(f, "{}", seq)
    }
}

struct DynamicFrameOrder {
    cur_ft:     FrameType,
    next_ft:    FrameType,
    p_count:    usize,
}

const NUM_GOP_KF: usize = 8;

impl DynamicFrameOrder {
    fn new() -> Self {
        Self {
            cur_ft:     FrameType::I,
            next_ft:    FrameType::Other,
            p_count:    0,
        }
    }
    fn peek_next_frame(&self) -> (FrameType, usize) {
        (self.cur_ft, if self.cur_ft == FrameType::Other || self.next_ft == FrameType::B { 1 } else { 0 })
    }
    fn next_frame(&mut self) -> FrameType {
        if self.cur_ft == FrameType::P {
            self.p_count += 1;
            if self.p_count >= NUM_GOP_KF {
                self.cur_ft = FrameType::I;
                self.p_count = 0;
            }
        }
        let next = self.cur_ft;
        self.cur_ft = self.next_ft;
        self.next_ft = if self.cur_ft != FrameType::B { FrameType::Other } else { FrameType::P };
        next
    }
    fn update(&mut self, ftype: FrameType) {
        if self.cur_ft == FrameType::Other {
            self.cur_ft = ftype;
            if self.cur_ft == FrameType::B {
                self.cur_ft = FrameType::P;
                self.next_ft = FrameType::B;
            } else {
                self.next_ft = FrameType::Other;
            }
        }
    }
}

impl std::fmt::Display for DynamicFrameOrder {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "dynamic")
    }
}

enum FrameOrder {
    Static(StaticFrameOrder),
    Dynamic(DynamicFrameOrder),
}

impl FrameOrder {
    fn new() -> Self {
        FrameOrder::Dynamic(DynamicFrameOrder::new())
    }
    fn get_max_grp_len(&self) -> usize {
        match self {
            FrameOrder::Static(ref order) => order.get_max_grp_len(),
            FrameOrder::Dynamic(ref _order) => 2,
        }
    }
    fn peek_next_frame(&self) -> (FrameType, usize) {
        match self {
            FrameOrder::Static(ref order) => order.peek_next_frame(),
            FrameOrder::Dynamic(ref order) => order.peek_next_frame(),
        }
    }
    fn next_frame(&mut self) -> FrameType {
        match self {
            FrameOrder::Static(ref mut order) => order.next_frame(),
            FrameOrder::Dynamic(ref mut order) => order.next_frame(),
        }
    }
    fn update(&mut self, ftype: FrameType) {
        if let FrameOrder::Dynamic(ref mut order) = self {
            order.update(ftype);
        }
    }
    fn is_dynamic(&self) -> bool { matches!(self, FrameOrder::Dynamic(_)) }
}

impl std::fmt::Display for FrameOrder {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            FrameOrder::Static(ref order) => order.fmt(f),
            FrameOrder::Dynamic(ref order) => order.fmt(f),
        }
    }
}

#[derive(Clone,Copy,Debug)]
enum ParseError {
    TooShort,
    TooLong,
    InvalidValue,
    InvalidCombination,
}

impl FromStr for FrameOrder {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "dynamic" {
            return Ok(FrameOrder::Dynamic(DynamicFrameOrder::new()));
        }
        let mut ftypes = Vec::new();
        for ch in s.bytes() {
            match ch {
                b'I' | b'i' => ftypes.push(FrameType::I),
                b'P' | b'p' => ftypes.push(FrameType::P),
                b'B' | b'b' => ftypes.push(FrameType::B),
                b' ' | b',' => {},
                _ => return Err(ParseError::InvalidValue),
            };
            if ftypes.len() > 16 {
                return Err(ParseError::TooLong);
            }
        }
        if ftypes.is_empty() {
            return Err(ParseError::TooShort);
        }
        if ftypes[0] != FrameType::I {
            return Err(ParseError::InvalidCombination);
        }

        let mut groups = Vec::new();
        let mut cur_ftype = ftypes[0];
        let mut cur_run = 0;
        for &ft in ftypes[1..].iter() {
            match ft {
                FrameType::I | FrameType::P => {
                    groups.push((cur_ftype, cur_run));
                    cur_ftype = ft;
                    cur_run = 0;
                },
                _ => {
                    cur_run += 1;
                    if cur_run > 4 {
                        return Err(ParseError::InvalidCombination);
                    }
                },
            };
        }
        groups.push((cur_ftype, cur_run));

        Ok(FrameOrder::Static(StaticFrameOrder{
            groups,
            start:      true,
            cur_grp:    0,
            cur_frm:    0,
        }))
    }
}

struct RV40Encoder {
    stream:     Option<NAStreamRef>,
    vinfo:      NAVideoInfo,
    width:      usize,
    height:     usize,
    mb_w:       usize,
    mb_h:       usize,

    slice_bits: u32,
    deblock:    bool,
    force_set:  Option<usize>,

    fce:        FrameComplexityEstimate,
    mbd:        MacroblockDecider,
    order:      FrameOrder,
    brc:        BitRateControl,
    rdm:        RateDistMetric,
    be:         BitsEstimator,
    me:         MotionEstimator,
    cset:       CodeSets,

    sstate:     SliceState,
    mbstate:    MBState,
    mbs:        VecDeque<Macroblock>,
    dblk:       Vec<DeblockInfo>,

    qframes:    Vec<NAFrame>,
    frm_pool:   NAVideoBufferPool<u8>,
    ref_p:      NAVideoBufferRef<u8>,
    ref_n:      NAVideoBufferRef<u8>,
    pkts:       VecDeque<NAPacket>,

    p_pts:      u64,
    n_pts:      u64,
    last_k_ts:  u64,
    last_b_ts:  u64,

    needs_alloc:    bool,
    max_grp_bufs:   usize,

    debug_log:  DebugFlags,

    refine_b:   bool,
    i4_in_b:    bool,
}

impl RV40Encoder {
    fn new() -> Self {
        let vinfo = NAVideoInfo::new(24, 24, false, YUV420_FORMAT);
        let vt = alloc_video_buffer(vinfo, 4).unwrap();
        let ref_p = vt.get_vbuf().unwrap();
        let vt = alloc_video_buffer(vinfo, 4).unwrap();
        let ref_n = vt.get_vbuf().unwrap();
        Self {
            stream:     None,
            vinfo,
            width:      0,
            height:     0,
            mb_w:       0,
            mb_h:       0,

            slice_bits: 10000,
            deblock:    true,
            force_set:  None,

            fce:        FrameComplexityEstimate::new(),
            mbd:        MacroblockDecider::new(),
            order:      FrameOrder::new(),
            brc:        BitRateControl::new(),
            rdm:        RateDistMetric::new(),
            be:         BitsEstimator::new(),
            me:         MotionEstimator::new(),
            cset:       CodeSets::new(),

            sstate:     SliceState::new(),
            mbstate:    MBState::new(),
            mbs:        VecDeque::new(),
            dblk:       Vec::new(),

            qframes:    Vec::new(),
            frm_pool:   NAVideoBufferPool::new(0),
            pkts:       VecDeque::new(),
            ref_p, ref_n,

            p_pts:      0,
            n_pts:      0,
            last_k_ts:  0,
            last_b_ts:  0,

            needs_alloc:    true,
            max_grp_bufs:   0,

            debug_log:  DebugFlags::new(),

            refine_b:   false,
            i4_in_b:    false,
        }
    }
    fn encode_frame(&mut self, frm: NAFrame, frameno: usize) -> EncoderResult<NAPacket> {
        let ftype = self.order.next_frame();
        let buf = frm.get_buffer();

        let tinfo = frm.get_time_information();
        let pts = NATimeInfo::ts_to_time(tinfo.pts.unwrap_or(0), 1000, tinfo.tb_num, tinfo.tb_den);
        let fpts = (pts & 0x1FFF) as u32;

        let ts_diff = if ftype == FrameType::B {
                pts.saturating_sub(self.last_k_ts.min(self.last_b_ts)) as u32
            } else {
                let diff = pts.saturating_sub(self.last_k_ts) as u32;
                diff / ((frameno + 1) as u32)
            };

        if self.debug_log.is_set(DEBUG_BIT_FRAMENO) {
            println!("encode frame type {} pts {}", ftype, pts);
        }
        let is_ref_frame = matches!(ftype, FrameType::I | FrameType::P);

        let tr_d = (self.n_pts - self.p_pts) as u32;
        let tr_b = (pts - self.p_pts) as u32;
        if !is_ref_frame {
            self.mbd.set_b_distance(tr_b, tr_d);
        }

        let mut rvbuf = if let Some(nfrm) = self.frm_pool.get_free() {
                nfrm
            } else {
                return Err(EncoderError::AllocError);
            };
        let mut recon_frm = NASimpleVideoFrame::from_video_buf(&mut rvbuf).unwrap();

        self.be.set_frame_type(ftype);
        if let Some(ref vbuf) = buf.get_vbuf() {
            let src = vbuf.get_data();

            if self.brc.rate_ctl_in_use() || self.order.is_dynamic() {
                self.fce.set_current(vbuf);
            }

            let complexity = if self.brc.rate_ctl_in_use() {
                    self.fce.get_complexity(ftype)
                } else { 0 };

            self.mbd.q = self.brc.get_quant(ftype, complexity);
            self.brc.init_metric(ftype, &mut self.rdm);
            self.be.set_quant(self.mbd.q);
            if self.debug_log.is_set(DEBUG_BIT_RATECTL) {
                println!(" expected frame size {}", self.brc.get_target_size(ftype));
                println!(" target quantiser {} lambda {} thresholds {} / {}", self.brc.get_last_quant(ftype), self.rdm.lambda, self.rdm.good_enough, self.rdm.p_split_thr);
            }

            let mut nslices = 0;
            let mut dvec = Vec::new();
            let mut mb_idx = 0;
            let mut slice_starts = Vec::new();
            let num_mbs = self.mb_w * self.mb_h;
            while mb_idx < num_mbs {
                slice_starts.push(dvec.len());
                let mut bw = BitWriter::new(dvec, BitWriterMode::BE);
                let slice_start_mb = mb_idx;

                self.mbstate.reset();

                let mut est_bits = 0;
                while est_bits < self.slice_bits && mb_idx < num_mbs {
                    let mb_x = mb_idx % self.mb_w;
                    let mb_y = mb_idx / self.mb_w;
                    self.sstate.has_t  = mb_idx >= slice_start_mb + self.mb_w;
                    self.sstate.has_l  = (mb_idx > slice_start_mb) && (mb_x > 0);
                    self.sstate.has_tl = (mb_idx > slice_start_mb + self.mb_w) && (mb_x > 0);
                    self.sstate.has_tr = (mb_idx >= slice_start_mb + self.mb_w - 1) && (mb_x + 1 < self.mb_w);
                    self.sstate.mb_x = mb_x;
                    self.sstate.mb_y = mb_y;

                    let offsets = [
                            vbuf.get_offset(0) + mb_x * 16 + mb_y * 16 * vbuf.get_stride(0),
                            vbuf.get_offset(1) + mb_x *  8 + mb_y *  8 * vbuf.get_stride(1),
                            vbuf.get_offset(2) + mb_x *  8 + mb_y *  8 * vbuf.get_stride(2),
                        ];
                    let strides = [vbuf.get_stride(0), vbuf.get_stride(1), vbuf.get_stride(2)];
                    self.mbd.load_mb(src, offsets, strides, &self.sstate);

                    self.be.set_pred_mb_type(self.mbstate.get_pred_mbtype(&self.sstate, ftype == FrameType::B));
                    if ftype == FrameType::B {
                        self.mbd.try_b_coding(&self.ref_p, &self.ref_n, &mut self.be, &mut self.me, &self.rdm, &self.mbstate, self.refine_b);
                    }
                    if ftype == FrameType::P {
                        self.mbd.try_p_coding(&self.ref_n, &mut self.be, &mut self.me, &self.rdm, &self.mbstate);
                    }
                    self.mbd.try_intra_16_pred(&mut self.be, &self.rdm);
                    if ftype != FrameType::B || self.i4_in_b {
                        self.mbd.try_intra_4x4_pred(&mut self.be, &self.rdm, &mut self.mbstate);
                    }

                    let mb = self.mbd.get_macroblock();
                    est_bits += self.mbd.get_est_bits();
                    self.mbd.recon_mb(&mut recon_frm);
                    self.mbstate.update(&mb.mb_type, mb_x, mb_y);

                    if self.deblock {
                        self.dblk[mb_idx].q = self.mbd.q as u8;
                        if ftype == FrameType::I {
                            self.dblk[mb_idx].is_strong = true;
                            self.dblk[mb_idx].cbp_y     = 0xFFFF;
                            self.dblk[mb_idx].cbp_c     = 0xFF;
                        } else {
                            self.dblk[mb_idx].is_strong = mb.mb_type.is_intra() || mb.mb_type.is_16();
                            let mut cbp = 0u16;
                            let mut mask = 1;
                            for blk in mb.coeffs[..16].iter() {
                                if !blk.is_empty() {
                                    cbp |= mask;
                                }
                                mask <<= 1;
                            }
                            self.dblk[mb_idx].cbp_y = cbp;
                            let mut cbp = 0u8;
                            let mut mask = 1;
                            for blk in mb.coeffs[16..24].iter() {
                                if !blk.is_empty() {
                                    cbp |= mask;
                                }
                                mask <<= 1;
                            }
                            self.dblk[mb_idx].cbp_c = cbp;
                        }
                        self.mbstate.fill_deblock(&mut self.dblk[mb_idx], &self.sstate);
                    }

                    self.mbs.push_back(mb);

                    mb_idx += 1;
                }

                let set_idx = if let Some(idx) = self.force_set {
                        idx
                    } else {
                        let mut hist = [0usize; 17];
                        for mb in self.mbs.iter() {
                            let blocks = if mb.mb_type.is_16() { &mb.coeffs } else { &mb.coeffs[..24] };

                            for blk in blocks.iter() {
                                let nz = blk.count_nz();
                                for el in hist[nz..].iter_mut() {
                                    *el += 1;
                                }
                            }
                        }
                        BitsEstimator::decide_set(&hist)
                    };

                let start_bits = bw.tell();
                write_slice_header(&mut bw, ftype, self.mbd.q, set_idx, self.deblock, fpts);
                if ftype == FrameType::I {
                    write_slice_dimensions(&mut bw, self.width, self.height);
                } else {
                    bw.write1(); // keep dimensions flag
                }
                write_slice_mb_idx(&mut bw, slice_start_mb, num_mbs);

                mb_idx = slice_start_mb;
                let mut skip_count = 0;
                self.cset.init(self.mbd.q, set_idx);
                while let Some(mb) = self.mbs.pop_front() {
                    if bw.tell() > start_bits + (self.slice_bits as usize) {
                        break;
                    }
                    let mb_x = mb_idx % self.mb_w;
                    let mb_y = mb_idx / self.mb_w;
                    self.sstate.has_t  = mb_idx >= slice_start_mb + self.mb_w;
                    self.sstate.has_l  = (mb_idx > slice_start_mb) && (mb_x > 0);
                    self.sstate.has_tl = (mb_idx > slice_start_mb + self.mb_w) && (mb_x > 0);
                    self.sstate.has_tr = (mb_idx >= slice_start_mb + self.mb_w - 1) && (mb_x + 1 < self.mb_w);
                    self.sstate.mb_x = mb_x;
                    self.sstate.mb_y = mb_y;
                    if mb.mb_type.is_skip() {
                        skip_count += 1;
                    } else {
                        if skip_count > 0 {
                            write_skip_count(&mut bw, skip_count);
                            skip_count = 0;
                        } else if ftype != FrameType::I {
                            bw.write1(); // zero skip count
                        }

                        write_mb_header(&mut bw, ftype, &self.sstate, &self.mbstate);
                        self.cset.set_params(&mb.mb_type);
                        self.cset.write_coeffs(&mut bw, &mb.coeffs);
                    }
                    mb_idx += 1;
                }
                self.mbs.clear();
                if skip_count > 0 {
                    write_skip_count(&mut bw, skip_count);
                }
                while (bw.tell() & 7) != 0 {
                    bw.write0();
                }
                if self.debug_log.is_set(DEBUG_BIT_SLICE_SIZE) {
                    println!("  slice {}..{} wrote {} bits / estimated {} bits", slice_start_mb, mb_idx, bw.tell(), est_bits);
                }
                dvec = bw.end();
                nslices += 1;
            }
            for _ in 0..(nslices * 8 + 1) {
                dvec.insert(0, 0);
            }
            dvec[0] = (nslices - 1) as u8;
            for (i, &off) in slice_starts.iter().enumerate() {
                dvec[i * 8 + 4] = 1;
                write_u32be(&mut dvec[i * 8 + 5..], off as u32)?;
            }
            if self.debug_log.is_set(DEBUG_BIT_RATECTL) {
                println!(" got frame size {}", dvec.len());
            }

            if is_ref_frame && self.deblock {
                loop_filter_frame(&mut recon_frm, &self.dblk, self.mb_w, self.mb_h);
            }

            if self.debug_log.is_set(DEBUG_BIT_PSNR) {
                let psnr = calc_psnr(vbuf, &rvbuf);
                println!("  encoded frame PSNR {} size {}", psnr, dvec.len());
            }

            if is_ref_frame {
                std::mem::swap(&mut self.ref_p, &mut self.ref_n);
                self.ref_n = rvbuf;

                self.p_pts = self.n_pts;
                self.n_pts = pts;

                self.mbstate.swap_mvs();
            }

            if is_ref_frame {
                if self.last_k_ts > self.last_b_ts {
                    self.last_b_ts = self.last_k_ts;
                }
                self.last_k_ts = pts;
                self.fce.update_ref();
            } else {
                self.last_b_ts = pts;
            }

            self.brc.update_stats(ftype, dvec.len(), ts_diff);

            Ok(NAPacket::new(self.stream.clone().unwrap(), frm.ts, ftype == FrameType::I, dvec))
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
}

fn calc_psnr(pic1: &NAVideoBuffer<u8>, pic2: &NAVideoBuffer<u8>) -> f64 {
    let data1 = pic1.get_data();
    let data2 = pic2.get_data();
    let mut sum = 0u64;
    let mut size = 0;
    for comp in 0..3 {
        let (w, h) = pic1.get_dimensions(comp);
        size += w * h;
        for (line1, line2) in data1[pic1.get_offset(comp)..].chunks(pic1.get_stride(comp)).zip(
                data2[pic2.get_offset(comp)..].chunks(pic2.get_stride(comp))).take(h) {
            for (&pix1, &pix2) in line1[..w].iter().zip(line2.iter()) {
                let diff = (i32::from(pix1) - i32::from(pix2)).unsigned_abs();
                sum += u64::from(diff * diff);
            }
        }
    }
    if size > 0 {
        48.13080360867910341240 - 10.0 * ((sum as f64) / (size as f64)).log10()
    } else {
        std::f64::INFINITY
    }
}

impl NAEncoder for RV40Encoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, false, YUV420_FORMAT)),
                    ..Default::default() })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let outinfo = NAVideoInfo::new((vinfo.width + 15) & !15, (vinfo.height + 15) & !15, false, YUV420_FORMAT);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { 0 }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                if vinfo.format != YUV420_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if ((vinfo.width | vinfo.height) & 15) != 0 {
                    return Err(EncoderError::FormatError);
                }
                if (vinfo.width | vinfo.height) >= (1 << 12) {
                    return Err(EncoderError::FormatError);
                }

                // 32-bit flags (VBR, bframes, slices, something else) and 32-bit version
                let edata = vec![0x01, 0x08, 0x10, 0x20, 0x40, 0x00, 0x80, 0x00];
                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, false, vinfo.format);
                let info = NACodecInfo::new("realvideo4", NACodecTypeInfo::Video(out_info), Some(edata));
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());

                self.width  = vinfo.width;
                self.height = vinfo.height;
                self.mb_w = (vinfo.width  + 15) >> 4;
                self.mb_h = (vinfo.height + 15) >> 4;

                if self.mb_w * self.mb_h > 9216 {
                    return Err(EncoderError::FormatError);
                }

                if (1..=100u8).contains(&encinfo.quality) {
                    self.brc.set_force_quality(Some(encinfo.quality));
                } else {
                    self.brc.set_force_quality(None);
                }
                self.brc.set_bitrate(encinfo.bitrate);

                self.vinfo = out_info;
                let max_frames = self.order.get_max_grp_len();
                self.frm_pool.set_dec_bufs(max_frames + 3);
                self.max_grp_bufs = max_frames;
                self.needs_alloc = true;

                self.fce.resize(self.width, self.height);
                self.mbstate.resize(self.mb_w, self.mb_h);
                self.mbd.resize(self.mb_w);
                self.dblk.resize(self.mb_w * self.mb_h, DeblockInfo::default());

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        if self.needs_alloc {
            self.frm_pool.prealloc_video(self.vinfo, 4)?;
            self.ref_n = self.frm_pool.get_free().unwrap();
            self.ref_p = self.frm_pool.get_free().unwrap();
            self.needs_alloc = false;
        }
        if let Some(ref vbuf) = frm.get_buffer().get_vbuf() {
            if let Some(dbuf) = self.frm_pool.get_copy(vbuf) {
                let newfrm = NAFrame::new(frm.ts, frm.frame_type, frm.key, frm.get_info(), NABufferType::Video(dbuf));
                self.qframes.push(newfrm);

                loop {
                    let (mut ftype, mut frame_pos) = self.order.peek_next_frame();
                    if frame_pos >= self.qframes.len() {
                        break;
                    }

                    if ftype == FrameType::Other {
                        if self.qframes.len() < 2 {
                            return Err(EncoderError::Bug);
                        }
                        if let (Some(ref frm1), Some(ref frm2)) = (self.qframes[0].get_buffer().get_vbuf(), self.qframes[1].get_buffer().get_vbuf()) {
                            let is_b = self.fce.decide_b_frame(frm1, frm2);
                            ftype = if is_b {
                                    frame_pos = 1;
                                    FrameType::B
                                } else {
                                    frame_pos = 0;
                                    FrameType::P
                                };
                        } else {
                            return Err(EncoderError::Bug);
                        }
                        self.order.update(ftype);
                    }

                    let frm = self.qframes.remove(frame_pos);
                    let pkt = self.encode_frame(frm, frame_pos)?;
                    self.pkts.push_back(pkt);
                }
                Ok(())
            } else {
                Err(EncoderError::AllocError)
            }
        } else {
            Err(EncoderError::FormatError)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        Ok(self.pkts.pop_front())
    }
    fn flush(&mut self) -> EncoderResult<()> {
        Ok(())
    }
}

const DEBUG_LOG_OPTION: &str = "debug";
const SLICE_SIZE_OPTION: &str = "slice_size";
const FRAME_ORDER_OPTION: &str = "frame_order";
const DEBLOCK_OPTION: &str = "loop_filt";
const QUANT_OPTION: &str = "quant";
const QUALITY_OPTION: &str = "quality";
const SET_OPTION: &str = "coding_set";
const SEARCH_MODE_OPTION: &str = "me_mode";
const SEARCH_RANGE_OPTION: &str = "me_range";
const SEARCH_THR_OPTION: &str = "me_thr";
const B_REFINE_OPTION: &str = "refine_b";
const I4_IN_B_OPTION: &str = "i4_in_b";
const B_OFFSET_OPTION: &str = "b_offset";

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: DEBUG_LOG_OPTION, description: "debug flags",
        opt_type: NAOptionDefinitionType::String(None) },
    NAOptionDefinition {
        name: SLICE_SIZE_OPTION, description: "soft slice size limit in bits",
        opt_type: NAOptionDefinitionType::Int(Some(4096), Some(100000)) },
    NAOptionDefinition {
        name: FRAME_ORDER_OPTION, description: "frame order (e.g. IBBPBB)",
        opt_type: NAOptionDefinitionType::String(None) },
    NAOptionDefinition {
        name: DEBLOCK_OPTION, description: "in-loop filter",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: QUANT_OPTION, description: "force quantiser (-1 = none)",
        opt_type: NAOptionDefinitionType::Int(Some(-1), Some(31)) },
    NAOptionDefinition {
        name: QUALITY_OPTION, description: "force quality (-1 = none)",
        opt_type: NAOptionDefinitionType::Int(Some(-1), Some(100)) },
    NAOptionDefinition {
        name: SET_OPTION, description: "force coding set (-1 = none)",
        opt_type: NAOptionDefinitionType::Int(Some(-1), Some(3)) },
    NAOptionDefinition {
        name: SEARCH_MODE_OPTION, description: "motion search mode",
        opt_type: NAOptionDefinitionType::String(Some(MVSearchMode::get_possible_modes())) },
    NAOptionDefinition {
        name: SEARCH_RANGE_OPTION, description: "motion search range",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(256)) },
    NAOptionDefinition {
        name: SEARCH_THR_OPTION, description: "motion search cut-off threshold",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(1048576)) },
    NAOptionDefinition {
        name: B_REFINE_OPTION, description: "better ME for B-frames",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: I4_IN_B_OPTION, description: "allow intra 4x4 coding in B-frames",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: B_OFFSET_OPTION, description: "B-frame quantiser offset",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(16)) },
];

impl NAOptionHandler for RV40Encoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        DEBUG_LOG_OPTION => {
                            if let NAValue::String(ref strval) = option.value {
                                self.debug_log.parse(strval);
                            }
                        },
                        SLICE_SIZE_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.slice_bits = intval as u32;
                            }
                        },
                        FRAME_ORDER_OPTION => {
                            if let NAValue::String(ref strval) = option.value {
                                if let Ok(norder) = strval.parse::<FrameOrder>() {
                                    self.order = norder;
                                    let max_frames = self.order.get_max_grp_len();
                                    if max_frames > self.max_grp_bufs {
                                        self.frm_pool.set_dec_bufs(max_frames + 3);
                                        self.needs_alloc = true;
                                        self.max_grp_bufs = max_frames;
                                    }
                                } else {
                                    println!("Invalid order sequence");
                                }
                            }
                        },
                        DEBLOCK_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.deblock = val;
                            }
                        },
                        QUANT_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                if val != -1 {
                                    self.brc.set_force_quant(Some(val as usize));
                                } else {
                                    self.brc.set_force_quant(None);
                                }
                            }
                        },
                        QUALITY_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                if val != -1 {
                                    self.brc.set_force_quality(Some(val as u8));
                                } else {
                                    self.brc.set_force_quality(None);
                                }
                            }
                        },
                        SET_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.force_set = if val != -1 { Some(val as usize) } else { None };
                            }
                        },
                        SEARCH_MODE_OPTION => {
                            if let NAValue::String(ref strval) = option.value {
                                if let Ok(mmode) = strval.parse::<MVSearchMode>() {
                                    self.me.set_mode(mmode);
                                } else {
                                    println!("Invalid mode");
                                }
                            }
                        },
                        SEARCH_RANGE_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.me.range = val as i16;
                            }
                        },
                        SEARCH_THR_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.me.thresh = val as u32;
                            }
                        },
                        B_REFINE_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.refine_b = val;
                            }
                        },
                        I4_IN_B_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.i4_in_b = val;
                            }
                        },
                        B_OFFSET_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.brc.b_offset = val as usize;
                            }
                        },
                        _ => {},
                    };
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            DEBUG_LOG_OPTION => Some(NAValue::String(self.debug_log.to_string())),
            SLICE_SIZE_OPTION => Some(NAValue::Int(self.slice_bits as i64)),
            FRAME_ORDER_OPTION => Some(NAValue::String(self.order.to_string())),
            DEBLOCK_OPTION => Some(NAValue::Bool(self.deblock)),
            QUANT_OPTION => Some(NAValue::Int(self.brc.get_force_quant().into())),
            QUALITY_OPTION => Some(NAValue::Int(self.brc.get_force_quality().into())),
            SET_OPTION => Some(NAValue::Int(if let Some(set) = self.force_set { set as i64 } else { -1 })),
            SEARCH_MODE_OPTION => Some(NAValue::String(self.me.get_mode().to_string())),
            SEARCH_THR_OPTION => Some(NAValue::Int(self.me.thresh.into())),
            SEARCH_RANGE_OPTION => Some(NAValue::Int(self.me.range.into())),
            B_REFINE_OPTION => Some(NAValue::Bool(self.refine_b)),
            I4_IN_B_OPTION => Some(NAValue::Bool(self.i4_in_b)),
            B_OFFSET_OPTION => Some(NAValue::Int(self.brc.b_offset as i64)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(RV40Encoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_codec_support::test::enc_video::*;
    use nihav_commonfmt::*;

    #[allow(unused_variables)]
    fn encode_test(out_name: &'static str, enc_options: &[NAOption], limit: Option<u64>, hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        realmedia_register_all_encoders(&mut enc_reg);

        // sample from private collection
        let dec_config = DecoderTestParams {
                demuxer:        "yuv4mpeg",
                in_name:        "assets/day3b.y4m",
                stream_type:    StreamType::Video,
                limit,
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "realmedia",
                enc_name:       "realvideo4",
                out_name,
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  YUV420_FORMAT,
                flipped: false,
                bits:    12,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 300000,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          hash);
    }
    #[test]
    fn test_rv40_encoder_simple() {
        let enc_options = &[
                NAOption { name: super::FRAME_ORDER_OPTION, value: NAValue::String("I".to_owned()) },
                NAOption { name: super::DEBLOCK_OPTION, value: NAValue::Bool(false) },
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(17) },
                NAOption { name: super::SEARCH_MODE_OPTION, value: NAValue::String("diamond".to_owned()) },
            ];
        encode_test("rv40simple.rmvb", enc_options, Some(10), &[0x03b0d743, 0x36c20dbb, 0x18fa1c9e, 0x4b2b7324]);
    }
    #[test]
    fn test_rv40_encoder_ipb() {
        let enc_options = &[
                NAOption { name: super::FRAME_ORDER_OPTION, value: NAValue::String("IBPB".to_owned()) },
                NAOption { name: super::DEBLOCK_OPTION, value: NAValue::Bool(true) },
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(17) },
                NAOption { name: super::SEARCH_MODE_OPTION, value: NAValue::String("hexagon".to_owned()) },
            ];
        encode_test("rv40ipb.rmvb", enc_options, Some(8), &[0xc382ab0b, 0xbcfbb02a, 0xf12a064f, 0xe6a5c2c3]);
    }
    #[test]
    fn test_rv40_encoder_advanced() {
        let enc_options = &[
                NAOption { name: super::FRAME_ORDER_OPTION, value: NAValue::String("dynamic".to_owned()) },
                NAOption { name: super::DEBLOCK_OPTION, value: NAValue::Bool(true) },
                NAOption { name: super::SEARCH_MODE_OPTION, value: NAValue::String("umh".to_owned()) },
            ];
        encode_test("rv40adv.rmvb", enc_options, Some(8), &[0xc4395f49, 0x0536d5f0, 0x32406834, 0xb7b634be]);
    }
}
