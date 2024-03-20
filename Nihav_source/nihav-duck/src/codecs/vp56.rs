use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::vpcommon::*;

pub const TOKEN_LARGE: u8 = 5;
pub const TOKEN_EOB: u8 = 42;

#[derive(Clone,Copy,Debug,Default)]
#[allow(dead_code)]
pub struct VP56Header {
    pub is_intra:       bool,
    pub is_golden:      bool,
    pub quant:          u8,
    pub multistream:    bool,
    pub use_huffman:    bool,
    pub version:        u8,
    pub profile:        u8,
    pub interlaced:     bool,
    pub offset:         u16,
    pub mb_w:           u8,
    pub mb_h:           u8,
    pub disp_w:         u8,
    pub disp_h:         u8,
    pub scale:          u8,
}

#[derive(Clone,Copy,Default)]
pub struct VP56MVModel {
    pub nz_prob:        u8,
    pub sign_prob:      u8,
    pub raw_probs:      [u8; 8],
    pub tree_probs:     [u8; 7],
}

#[derive(Clone,Copy,Default)]
pub struct VP56MBTypeModel {
    pub probs:          [u8; 10],
}

#[derive(Clone,Copy,Default)]
pub struct VP56CoeffModel {
    pub dc_token_probs: [[[u8; 5]; 6]; 6],
    pub dc_value_probs: [u8; 11],
    pub ac_ctype_probs: [[[[u8; 5]; 5]; 6]; 3],
    pub ac_type_probs:  [[[[u8; 5]; 6]; 3]; 3],
    pub ac_val_probs:   [[[u8; 11]; 6]; 3],
}

pub struct VP6Models {
    pub scan_order:         [usize; 64],
    pub scan:               [usize; 64],
    pub zigzag:             [usize; 64],
    pub zero_run_probs:     [[u8; 14]; 2],
}

const MAX_HUFF_ELEMS: usize = 12;
#[derive(Clone,Copy,Default)]
pub struct VP6Huff {
    pub codes:  [u16; MAX_HUFF_ELEMS],
    pub bits:   [u8; MAX_HUFF_ELEMS],
}

#[derive(Clone,Copy,Default)]
struct Node {
    weight:     u16,
    sym:        i8,
    ch0:        usize,
    ch1:        usize,
}

fn prob2weight(a: u8, b: u8) -> u8 {
    let w = ((u16::from(a) * u16::from(b)) >> 8) as u8;
    if w == 0 {
        1
    } else {
        w
    }
}

impl VP6Huff {
    fn build_codes(&mut self, probs: &[u8; 11]) {
        let mut weights = [0u8; 12];

        weights[11] = prob2weight( probs[0],  probs[ 1]);
        weights[ 0] = prob2weight( probs[0], !probs[ 1]);
        weights[ 1] = prob2weight(!probs[0],  probs[ 2]);
        let lvroot  = prob2weight(!probs[0], !probs[ 2]);
        let tworoot = prob2weight( lvroot,    probs[ 3]);
        let hlroot  = prob2weight( lvroot,   !probs[ 3]);
        weights[ 2] = prob2weight( tworoot,   probs[ 4]);
        let root34  = prob2weight( tworoot,  !probs[ 4]);
        weights[ 3] = prob2weight( root34,    probs[ 5]);
        weights[ 4] = prob2weight( root34,   !probs[ 5]);
        let c1root  = prob2weight( hlroot,    probs[ 6]);
        let c34root = prob2weight( hlroot,   !probs[ 6]);
        weights[ 5] = prob2weight( c1root,    probs[ 7]);
        weights[ 6] = prob2weight( c1root,   !probs[ 7]);
        let c3root  = prob2weight( c34root,   probs[ 8]);
        let c4root  = prob2weight( c34root,  !probs[ 8]);
        weights[ 7] = prob2weight( c3root,    probs[ 9]);
        weights[ 8] = prob2weight( c3root,   !probs[ 9]);
        weights[ 9] = prob2weight( c4root,    probs[10]);
        weights[10] = prob2weight( c4root,   !probs[10]);

        self.build(&weights);
    }
    fn build_codes_zero_run(&mut self, probs: &[u8; 14]) {
        let mut weights = [0u8; 9];

        let root   = prob2weight( probs[0],  probs[1]);
        weights[0] = prob2weight( root,      probs[2]);
        weights[1] = prob2weight( root,     !probs[2]);

        let root   = prob2weight( probs[0], !probs[1]);
        weights[2] = prob2weight( root,      probs[3]);
        weights[3] = prob2weight( root,     !probs[3]);

        let root   = prob2weight(!probs[0],  probs[4]);
        weights[8] = prob2weight(!probs[0], !probs[4]);
        let root1  = prob2weight( root,      probs[5]);
        let root2  = prob2weight( root,     !probs[5]);
        weights[4] = prob2weight( root1,     probs[6]);
        weights[5] = prob2weight( root1,    !probs[6]);
        weights[6] = prob2weight( root2,     probs[7]);
        weights[7] = prob2weight( root2,    !probs[7]);

        self.build(&weights);
    }
    fn build(&mut self, weights: &[u8]) {
        let mut nodes = [Node::default(); MAX_HUFF_ELEMS * 2];
        let mut nlen = 0;

        for w in weights.iter().rev() {
            let weight = u16::from(*w);
            let mut pos = nlen;
            for i in 0..nlen {
                if nodes[i].weight > weight {
                    pos = i;
                    break;
                }
            }
            for j in (pos..nlen).rev() {
                nodes[j + 1] = nodes[j];
            }
            nodes[pos].weight = weight;
            nodes[pos].sym    = (weights.len() - nlen - 1) as i8;
            nlen += 1;
        }

        let mut low = 0;
        for _ in 0..nlen-1 {
            let nnode = Node {
                    weight: nodes[low + 0].weight + nodes[low + 1].weight,
                    sym:    -1,
                    ch0:    low + 0,
                    ch1:    low + 1,
                };
            low += 2;
            let mut pos = low;
            while (pos < nlen) && (nodes[pos].weight < nnode.weight) {
                pos += 1;
            }
            for j in (pos..nlen).rev() {
                nodes[j + 1] = nodes[j];
            }
            nodes[pos] = nnode;
            nlen += 1;
        }
        self.get_codes(&nodes, nlen - 1, 0, 0);
        for i in nlen..self.codes.len() {
            self.codes[i]   = self.codes[0];
            self.bits[i]    = self.bits[0];
        }
    }
    fn get_codes(&mut self, nodes: &[Node], pos: usize, code: u16, len: u8) {
        if nodes[pos].sym >= 0 {
            self.codes[nodes[pos].sym as usize] = code;
            self.bits [nodes[pos].sym as usize] = len;
        } else {
            self.get_codes(nodes, nodes[pos].ch0, (code << 1) | 0, len + 1);
            self.get_codes(nodes, nodes[pos].ch1, (code << 1) | 1, len + 1);
        }
    }
}

pub trait ReadHuff {
    fn read_huff(&mut self, huff: &VP6Huff) -> DecoderResult<u8>;
}

impl<'a> ReadHuff for BitReader<'a> {
    fn read_huff(&mut self, huff: &VP6Huff) -> DecoderResult<u8> {
        let peekval                             = self.peek(16);
        for (i, (code, bit)) in huff.codes.iter().zip(huff.bits.iter()).enumerate() {
            if (peekval >> (16 - *bit)) == u32::from(*code) {
                self.skip(u32::from(*bit))?;
                return Ok(i as u8);
            }
        }
        Err(DecoderError::InvalidData)
    }
}

#[derive(Clone,Copy,Default)]
pub struct VP6HuffModels {
    pub dc_token_tree:      [VP6Huff; 2],
    pub ac_token_tree:      [[[VP6Huff; 6]; 3]; 2],
    pub zero_run_tree:      [VP6Huff; 2],
}

impl VP6Models {
    fn new() -> Self {
        Self {
            scan_order:         [0; 64],
            scan:               [0; 64],
            zigzag:             [0; 64],
            zero_run_probs:     [[0; 14]; 2],
        }
    }
}

pub struct VP56Models {
    pub mv_models:          [VP56MVModel; 2],
    pub mbtype_models:      [[VP56MBTypeModel; 10]; 3],
    pub coeff_models:       [VP56CoeffModel; 2],
    pub prob_xmitted:       [[u8; 20]; 3],
    pub vp6models:          VP6Models,
    pub vp6huff:            VP6HuffModels,
}

impl VP56Models {
    fn new() -> Self {
        Self {
            mv_models:      [VP56MVModel::default(); 2],
            mbtype_models:  [[VP56MBTypeModel::default(); 10]; 3],
            coeff_models:   [VP56CoeffModel::default(); 2],
            prob_xmitted:   [[0; 20]; 3],
            vp6models:      VP6Models::new(),
            vp6huff:        VP6HuffModels::default(),
        }
    }
}

pub trait VP56Parser {
    fn parse_header(&mut self, bc: &mut BoolCoder) -> DecoderResult<VP56Header>;
    fn reset_models(&self, models: &mut VP56Models);
    fn decode_mv(&self, bc: &mut BoolCoder, model: &VP56MVModel) -> i16;
    fn decode_mv_models(&self, bc: &mut BoolCoder, models: &mut [VP56MVModel; 2]) -> DecoderResult<()>;
    fn decode_coeff_models(&self, bc: &mut BoolCoder, models: &mut VP56Models, is_intra: bool) -> DecoderResult<()>;
    fn decode_block(&self, bc: &mut BoolCoder, coeffs: &mut [i16; 64], model: &VP56CoeffModel, vp6model: &VP6Models, fstate: &mut FrameState) -> DecoderResult<()>;
    fn decode_block_huff(&self, br: &mut BitReader, coeffs: &mut [i16; 64], vp6model: &VP6Models, model: &VP6HuffModels, fstate: &mut FrameState) -> DecoderResult<()>;
    fn mc_block(&self, dst: &mut NASimpleVideoFrame<u8>, mc_buf: NAVideoBufferRef<u8>, src: NAVideoBufferRef<u8>, plane: usize, x: usize, y: usize, mv: MV, loop_thr: i16);
}

enum CoeffReader<'a> {
    None,
    Bool(BoolCoder<'a>),
    Huff(BitReader<'a>),
}

#[derive(Clone,Copy,Default)]
struct MBInfo {
    mb_type:    VPMBType,
    mv:         MV,
}

pub struct FrameState {
    pub mb_x:           usize,
    pub mb_y:           usize,
    pub plane:          usize,
    pub coeff_cat:      [[u8; 64]; 4],
    pub last_idx:       [usize; 4],
    pub top_ctx:        u8,
    pub ctx_idx:        usize,
    pub dc_quant:       i16,
    pub ac_quant:       i16,
    pub dc_zero_run:    [usize; 2],
    pub ac_zero_run:    [usize; 2],
}

impl FrameState {
    fn new() -> Self {
        Self {
            mb_x:           0,
            mb_y:           0,
            plane:          0,
            coeff_cat:      [[0; 64]; 4],
            last_idx:       [0; 4],
            top_ctx:        0,
            ctx_idx:        0,
            dc_quant:       0,
            ac_quant:       0,
            dc_zero_run:    [0; 2],
            ac_zero_run:    [0; 2],
        }
    }
}

#[derive(Default)]
pub struct VP56DCPred {
    dc_y:       Vec<i16>,
    dc_u:       Vec<i16>,
    dc_v:       Vec<i16>,
    ldc_y:      [i16; 2],
    ldc_u:      i16,
    ldc_v:      i16,
    ref_y:      Vec<u8>,
    ref_c:      Vec<u8>,
    ref_left:   u8,
    y_idx:      usize,
    c_idx:      usize,
}

const INVALID_REF: u8 = 42;

impl VP56DCPred {
    fn new() -> Self { Self::default() }
    fn resize(&mut self, mb_w: usize) {
        self.dc_y.resize(mb_w * 2 + 2, 0);
        self.dc_u.resize(mb_w     + 2, 0);
        self.dc_v.resize(mb_w     + 2, 0);
        self.ref_y.resize(mb_w * 2 + 2, INVALID_REF);
        self.ref_c.resize(mb_w     + 2, INVALID_REF);
        self.ref_c[0] = 0;
    }
    fn reset(&mut self) {
        self.update_row();
        for el in self.ref_y.iter_mut().skip(1) { *el = INVALID_REF; }
        for el in self.ref_c.iter_mut().skip(1) { *el = INVALID_REF; }
    }
    fn update_row(&mut self) {
        self.y_idx = 1;
        self.c_idx = 1;
        self.ldc_y = [0; 2];
        self.ldc_u = 0;
        self.ldc_v = 0;
        self.ref_left = INVALID_REF;
    }
    fn next_mb(&mut self) {
        self.y_idx += 2;
        self.c_idx += 1;
    }
}

pub struct VP56Decoder {
    version:    u8,
    has_alpha:  bool,
    flip:       bool,
    shuf:       VPShuffler,
    width:      usize,
    height:     usize,
    mb_w:       usize,
    mb_h:       usize,
    models:     VP56Models,
    amodels:    VP56Models,
    coeffs:     [[i16; 64]; 6],
    last_mbt:   VPMBType,

    loop_thr:   i16,
    ilace_prob: u8,
    ilace_mb:   bool,

    mb_info:    Vec<MBInfo>,
    fstate:     FrameState,
    dc_pred:    VP56DCPred,
    last_dc:    [[i16; 4]; 3],
    top_ctx:    [Vec<u8>; 4],

    mc_buf:     NAVideoBufferRef<u8>,
}

fn rescale_mb_mode_prob(prob: u32, total: u32) -> u8 {
    (255 * prob / (1 + total)) as u8
}

fn map_mb_type(mbtype: VPMBType) -> usize {
    match mbtype {
        VPMBType::InterNoMV     => 0,
        VPMBType::Intra         => 1,
        VPMBType::InterMV       => 2,
        VPMBType::InterNearest  => 3,
        VPMBType::InterNear     => 4,
        VPMBType::GoldenNoMV    => 5,
        VPMBType::GoldenMV      => 6,
        VPMBType::InterFourMV   => 7,
        VPMBType::GoldenNearest => 8,
        VPMBType::GoldenNear    => 9,
    }
}

pub fn expand_token_bc(bc: &mut BoolCoder, val_probs: &[u8; 11], token: u8, version: u8) -> i16 {
    let mut sign = false;
    let level;
    if token < TOKEN_LARGE {
        if token != 0 {
            sign                                = bc.read_bool();
        }
        level = i16::from(token);
    } else {
        let cat: usize = vp_tree!(bc, val_probs[6],
                                  vp_tree!(bc, val_probs[7], 0, 1),
                                  vp_tree!(bc, val_probs[8],
                                           vp_tree!(bc, val_probs[9],  2, 3),
                                           vp_tree!(bc, val_probs[10], 4, 5)));
        if version == 5 {
            sign                                = bc.read_bool();
        }
        let mut add = 0i16;
        let add_probs = &VP56_COEF_ADD_PROBS[cat];
        for prob in add_probs.iter() {
            if *prob == 128 { break; }
            add                                 = (add << 1) | (bc.read_prob(*prob) as i16);
        }
        if version != 5 {
            sign                                = bc.read_bool();
        }
        level = VP56_COEF_BASE[cat] + add;
    }
    if !sign {
        level
    } else {
        -level
    }
}

impl VP56Decoder {
    pub fn new(version: u8, has_alpha: bool, flip: bool) -> Self {
        let vt = alloc_video_buffer(NAVideoInfo::new(24, 24, false, VP_YUVA420_FORMAT), 4).unwrap();
        let mc_buf = vt.get_vbuf().unwrap();
        Self {
            version, has_alpha, flip,
            shuf:       VPShuffler::new(),
            width:      0,
            height:     0,
            mb_w:       0,
            mb_h:       0,
            models:     VP56Models::new(),
            amodels:    VP56Models::new(),
            coeffs:     [[0; 64]; 6],
            last_mbt:   VPMBType::InterNoMV,

            loop_thr:   0,
            ilace_prob: 0,
            ilace_mb:   false,

            mb_info:    Vec::new(),
            fstate:     FrameState::new(),
            dc_pred:    VP56DCPred::new(),
            last_dc:    [[0; 4]; 3],
            top_ctx:    [Vec::new(), Vec::new(), Vec::new(), Vec::new()],

            mc_buf,
        }
    }
    fn set_dimensions(&mut self, width: usize, height: usize) {
        self.width  = width;
        self.height = height;
        self.mb_w   = (self.width  + 15) >> 4;
        self.mb_h   = (self.height + 15) >> 4;
        self.mb_info.resize(self.mb_w * self.mb_h, MBInfo::default());
        self.top_ctx = [vec![0; self.mb_w * 2], vec![0; self.mb_w], vec![0; self.mb_w], vec![0; self.mb_w * 2]];
    }
    pub fn init(&mut self, supp: &mut NADecoderSupport, vinfo: NAVideoInfo) -> DecoderResult<()> {
        supp.pool_u8.set_dec_bufs(3 + if vinfo.get_format().has_alpha() { 1 } else { 0 });
        supp.pool_u8.prealloc_video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, vinfo.get_format()), 4)?;
        self.set_dimensions(vinfo.get_width(), vinfo.get_height());
        self.dc_pred.resize(self.mb_w);
        Ok(())
    }
    pub fn flush(&mut self) {
        self.shuf.clear();
    }
    pub fn decode_frame(&mut self, supp: &mut NADecoderSupport, src: &[u8], br: &mut dyn VP56Parser) -> DecoderResult<(NABufferType, FrameType)> {
        let aoffset;
        let mut bc;
        if self.has_alpha {
            validate!(src.len() >= 7);
            aoffset = ((src[0] as usize) << 16) | ((src[1] as usize) << 8) | (src[2] as usize);
            validate!((aoffset > 0) && (aoffset < src.len() - 3));
            bc = BoolCoder::new(&src[3..])?;
        } else {
            validate!(src.len() >= 4);
            aoffset = src.len();
            bc = BoolCoder::new(src)?;
        }
        let hdr = br.parse_header(&mut bc)?;
        validate!((hdr.offset as usize) < aoffset); //XXX: take alpha 3 byte offset into account?

        if hdr.mb_w != 0 && (usize::from(hdr.mb_w) != self.mb_w || usize::from(hdr.mb_h) != self.mb_h) {
            self.set_dimensions((hdr.mb_w as usize) * 16, (hdr.mb_h as usize) * 16);
        }
        let fmt = if !self.has_alpha {
                YUV420_FORMAT
            } else {
                VP_YUVA420_FORMAT
                };
        let vinfo = NAVideoInfo::new(self.width, self.height, self.flip, fmt);
        let ret = supp.pool_u8.get_free();
        if ret.is_none() {
            return Err(DecoderError::AllocError);
        }
        let mut buf = ret.unwrap();
        if buf.get_info() != vinfo {
            self.shuf.clear();
            supp.pool_u8.reset();
            supp.pool_u8.prealloc_video(vinfo, 4)?;
            let ret = supp.pool_u8.get_free();
            if ret.is_none() {
                return Err(DecoderError::AllocError);
            }
            buf = ret.unwrap();
        }
        let mut dframe = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();

        if hdr.is_intra {
            self.shuf.clear();
        } else {
            if !self.shuf.has_refs() {
                return Err(DecoderError::MissingReference);
            }
        }

        let psrc = &src[if self.has_alpha { 3 } else { 0 }..aoffset];
        self.decode_planes(br, &mut dframe, &mut bc, &hdr, psrc, false)?;

        if self.has_alpha {
            let asrc = &src[aoffset + 3..];
            let mut bc = BoolCoder::new(asrc)?;
            let ahdr = br.parse_header(&mut bc)?;
            validate!(ahdr.mb_w == hdr.mb_w && ahdr.mb_h == hdr.mb_h);
            std::mem::swap(&mut self.models, &mut self.amodels);
            let ret = self.decode_planes(br, &mut dframe, &mut bc, &ahdr, asrc, true);
            std::mem::swap(&mut self.models, &mut self.amodels);
            ret?;
            match (hdr.is_golden, ahdr.is_golden) {
                (true, true) => { self.shuf.add_golden_frame(buf.clone()); },
                (true, false) => {
                    let cur_golden = self.shuf.get_golden().unwrap();
                    let off    = cur_golden.get_offset(3);
                    let stride = cur_golden.get_stride(3);
                    let mut new_golden = supp.pool_u8.get_copy(&buf).unwrap();
                    let dst = new_golden.get_data_mut().unwrap();
                    let src = cur_golden.get_data();
                    dst[off..][..stride * self.mb_h * 16].copy_from_slice(&src[off..][..stride * self.mb_h * 16]);
                    self.shuf.add_golden_frame(new_golden);
                },
                (false, true) => {
                    let cur_golden = self.shuf.get_golden().unwrap();
                    let off    = cur_golden.get_offset(3);
                    let stride = cur_golden.get_stride(3);
                    let mut new_golden = supp.pool_u8.get_copy(&cur_golden).unwrap();
                    let dst = new_golden.get_data_mut().unwrap();
                    let src = buf.get_data();
                    dst[off..][..stride * self.mb_h * 16].copy_from_slice(&src[off..][..stride * self.mb_h * 16]);
                    self.shuf.add_golden_frame(new_golden);
                },
                _ => {},
            };
        }

        if hdr.is_golden && !self.has_alpha {
            self.shuf.add_golden_frame(buf.clone());
        }
        self.shuf.add_frame(buf.clone());

        Ok((NABufferType::Video(buf), if hdr.is_intra { FrameType::I } else { FrameType::P }))
    }
    fn decode_planes(&mut self, br: &mut dyn VP56Parser, dframe: &mut NASimpleVideoFrame<u8>, bc: &mut BoolCoder, hdr: &VP56Header, src: &[u8], alpha: bool) -> DecoderResult<()> {
        let mut cr;
        if hdr.multistream {
            let off = hdr.offset as usize;
            if !hdr.use_huffman {
                let bc2 = BoolCoder::new(&src[off..])?;
                cr = CoeffReader::Bool(bc2);
            } else {
                let br = BitReader::new(&src[off..], BitReaderMode::BE);
                cr = CoeffReader::Huff(br);
            }
        } else {
            cr = CoeffReader::None;
        }

        if hdr.is_intra {
            br.reset_models(&mut self.models);
            self.reset_mbtype_models();
        } else {
            self.decode_mode_prob_models(bc)?;
            br.decode_mv_models(bc, &mut self.models.mv_models)?;
        }
        br.decode_coeff_models(bc, &mut self.models, hdr.is_intra)?;
        if hdr.use_huffman {
            for i in 0..2 {
                self.models.vp6huff.dc_token_tree[i].build_codes(&self.models.coeff_models[i].dc_value_probs);
            }
            for i in 0..2 {
                for mode in 0..3 {
                    for band in 0..6 {
                        self.models.vp6huff.ac_token_tree[i][mode][band].build_codes(&self.models.coeff_models[i].ac_val_probs[mode][band]);
                    }
                }
            }
            for i in 0..2 {
                self.models.vp6huff.zero_run_tree[i].build_codes_zero_run(&self.models.vp6models.zero_run_probs[i]);
            }
        }

        if hdr.interlaced {
            self.ilace_prob                     = bc.read_bits(8) as u8;
        }

        self.fstate = FrameState::new();
        self.fstate.dc_quant = VP56_DC_QUANTS[hdr.quant as usize] * 4;
        self.fstate.ac_quant = VP56_AC_QUANTS[hdr.quant as usize] * 4;
        self.loop_thr = i16::from(VP56_FILTER_LIMITS[hdr.quant as usize]);

        self.last_mbt = VPMBType::InterNoMV;
        for vec in self.top_ctx.iter_mut() {
            for el in vec.iter_mut() {
                *el = 0;
            }
        }
        self.last_dc = [[0; 4]; 3];
        self.last_dc[0][1] = 0x80;
        self.last_dc[0][2] = 0x80;
        self.dc_pred.reset();

        self.ilace_mb = false;
        for mb_y in 0..self.mb_h {
            self.fstate.mb_y = mb_y;
            self.fstate.coeff_cat = [[0; 64]; 4];
            self.fstate.last_idx = [24; 4];
            for mb_x in 0..self.mb_w {
                self.fstate.mb_x = mb_x;
                self.decode_mb(dframe, bc, &mut cr, br, hdr, alpha)?;
                self.dc_pred.next_mb();
            }
            self.dc_pred.update_row();
        }
        Ok(())
    }
    fn reset_mbtype_models(&mut self) {
        const DEFAULT_XMITTED_PROBS: [[u8; 20]; 3] = [
            [ 42,  69, 2, 1, 7, 1, 42, 44, 22, 6, 3, 1, 2, 0, 5, 1, 1, 0, 0, 0 ],
            [  8, 229, 1, 1, 8, 0,  0,  0,  0, 0, 2, 1, 1, 0, 0, 0, 1, 1, 0, 0 ],
            [ 35, 122, 1, 1, 6, 1, 34, 46,  0, 0, 2, 1, 1, 0, 1, 0, 1, 1, 0, 0 ]
        ];
        self.models.prob_xmitted.copy_from_slice(&DEFAULT_XMITTED_PROBS);
    }
    fn decode_mode_prob_models(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        for ctx in 0..3 {
            if bc.read_prob(174) {
                let idx                         = bc.read_bits(4) as usize;
                for i in 0..20 {
                    self.models.prob_xmitted[ctx][i ^ 1] = VP56_MODE_VQ[ctx][idx][i];
                }
            }
            if bc.read_prob(254) {
                for set in 0..20 {
                    if bc.read_prob(205) {
                        let sign                = bc.read_bool();
                        let diff = vp_tree!(bc, 171,
                                        vp_tree!(bc, 83, 2, 1),
                                        vp_tree!(bc, 199,
                                            vp_tree!(bc, 140,
                                                vp_tree!(bc, 125,
                                                    vp_tree!(bc, 104, 6, 5),
                                                    4
                                                ),
                                                3
                                            ),
                                            bc.read_bits(7)
                                        )) * 4;
                        validate!(diff < 256);
                        let diff = diff as u8;
                        if !sign {
                            validate!(self.models.prob_xmitted[ctx][set ^ 1] <= 255 - diff);
                            self.models.prob_xmitted[ctx][set ^ 1] += diff;
                        } else {
                            validate!(self.models.prob_xmitted[ctx][set ^ 1] >= diff);
                            self.models.prob_xmitted[ctx][set ^ 1] -= diff;
                        }
                    }
                }
            }
        }
        for ctx in 0..3 {
            let prob_xmitted = &self.models.prob_xmitted[ctx];
            for mode in 0..10 {
                let mdl = &mut self.models.mbtype_models[ctx][mode];
                let mut cnt = [0u32; 10];
                let mut total = 0;
                for i in 0..10 {
                    if i == mode { continue; }
                    cnt[i] = 100 * u32::from(prob_xmitted[i * 2]);
                    total += cnt[i];
                }
                let sum = u32::from(prob_xmitted[mode * 2]) + u32::from(prob_xmitted[mode * 2 + 1]);
                mdl.probs[9] = 255 - rescale_mb_mode_prob(u32::from(prob_xmitted[mode * 2 + 1]), sum);

                let inter_mv0_weight = cnt[0] + cnt[2];
                let inter_mv1_weight = cnt[3] + cnt[4];
                let gold_mv0_weight = cnt[5] + cnt[6];
                let gold_mv1_weight = cnt[8] + cnt[9];
                let mix_weight = cnt[1] + cnt[7];
                mdl.probs[0] = 1 + rescale_mb_mode_prob(inter_mv0_weight + inter_mv1_weight, total);
                mdl.probs[1] = 1 + rescale_mb_mode_prob(inter_mv0_weight, inter_mv0_weight + inter_mv1_weight);
                mdl.probs[2] = 1 + rescale_mb_mode_prob(mix_weight, mix_weight + gold_mv0_weight + gold_mv1_weight);
                mdl.probs[3] = 1 + rescale_mb_mode_prob(cnt[0], inter_mv0_weight);
                mdl.probs[4] = 1 + rescale_mb_mode_prob(cnt[3], inter_mv1_weight);
                mdl.probs[5] = 1 + rescale_mb_mode_prob(cnt[1], mix_weight);
                mdl.probs[6] = 1 + rescale_mb_mode_prob(gold_mv0_weight, gold_mv0_weight + gold_mv1_weight);
                mdl.probs[7] = 1 + rescale_mb_mode_prob(cnt[5], gold_mv0_weight);
                mdl.probs[8] = 1 + rescale_mb_mode_prob(cnt[8], gold_mv1_weight);
            }
        }
        Ok(())
    }
    fn find_mv_pred(&self, ref_id: u8) -> (usize, MV, MV, MV) {
        const CAND_POS: [(i8, i8); 12] = [
            (-1,  0), ( 0, -1),
            (-1, -1), (-1,  1),
            (-2,  0), ( 0, -2),
            (-1, -2), (-2, -1),
            (-2,  1), (-1,  2),
            (-2, -2), (-2,  2)
        ];

        let mut nearest_mv = ZERO_MV;
        let mut near_mv = ZERO_MV;
        let mut pred_mv = ZERO_MV;
        let mut num_mv: usize = 0;

        for (i, (yoff, xoff)) in CAND_POS.iter().enumerate() {
            let cx = (self.fstate.mb_x as isize) + (*xoff as isize);
            let cy = (self.fstate.mb_y as isize) + (*yoff as isize);
            if (cx < 0) || (cy < 0) {
                continue;
            }
            let cx = cx as usize;
            let cy = cy as usize;
            if (cx >= self.mb_w) || (cy >= self.mb_h) {
                continue;
            }
            let mb_pos = cx + cy * self.mb_w;
            let mv = self.mb_info[mb_pos].mv;
            if (self.mb_info[mb_pos].mb_type.get_ref_id() != ref_id) || (mv == ZERO_MV) {
                continue;
            }
            if num_mv == 0 {
                nearest_mv = mv;
                num_mv += 1;
                if (self.version > 5) && (i < 2) {
                    pred_mv = mv;
                }
            } else if mv != nearest_mv {
                near_mv = mv;
                num_mv += 1;
                break;
            }
        }

        (num_mv, nearest_mv, near_mv, pred_mv)
    }
    fn decode_mv(&self, bc: &mut BoolCoder, br: &mut dyn VP56Parser) -> MV {
        let x = br.decode_mv(bc, &self.models.mv_models[0]);
        let y = br.decode_mv(bc, &self.models.mv_models[1]);
        MV{ x, y }
    }
    fn decode_mb_type(&mut self, bc: &mut BoolCoder, ctx: usize) -> DecoderResult<VPMBType> {
        let probs = &self.models.mbtype_models[ctx][map_mb_type(self.last_mbt)].probs;
        if !bc.read_prob(probs[9]) {
            self.last_mbt = vp_tree!(
                    bc, probs[0],
                    vp_tree!(bc, probs[1],
                        vp_tree!(bc, probs[3], VPMBType::InterNoMV, VPMBType::InterMV),
                        vp_tree!(bc, probs[4], VPMBType::InterNearest, VPMBType::InterNear)
                    ),
                    vp_tree!(bc, probs[2],
                        vp_tree!(bc, probs[5], VPMBType::Intra, VPMBType::InterFourMV),
                        vp_tree!(bc, probs[6],
                            vp_tree!(bc, probs[7], VPMBType::GoldenNoMV, VPMBType::GoldenMV),
                            vp_tree!(bc, probs[8], VPMBType::GoldenNearest, VPMBType::GoldenNear)
                        )
                    )
                );
        }
        Ok(self.last_mbt)
    }
    #[allow(clippy::cognitive_complexity)]
    fn decode_mb(&mut self, frm: &mut NASimpleVideoFrame<u8>, bc: &mut BoolCoder, cr: &mut CoeffReader, br: &mut dyn VP56Parser, hdr: &VP56Header, alpha: bool) -> DecoderResult<()> {
        const FOURMV_SUB_TYPE: [VPMBType; 4] = [ VPMBType::InterNoMV, VPMBType::InterMV, VPMBType::InterNearest, VPMBType::InterNear ];

        let mb_x = self.fstate.mb_x;
        let mb_y = self.fstate.mb_y;
        self.coeffs = [[0; 64]; 6];
        let mb_pos = mb_x + mb_y * self.mb_w;
        let mut four_mv = [ZERO_MV; 4];
        let mut four_mbt = [VPMBType::Intra; 4];

        if hdr.interlaced {
            let iprob = self.ilace_prob;
            let prob = if mb_x == 0 {
                    iprob
                } else if !self.ilace_mb {
                    iprob + (((256 - u16::from(iprob)) >> 1) as u8)
                } else {
                    iprob - (iprob >> 1)
                };
            self.ilace_mb                       = bc.read_prob(prob);
        }

        let (num_mv, nearest_mv, near_mv, pred_mv) = if hdr.is_intra {
                (0, ZERO_MV, ZERO_MV, ZERO_MV)
            } else { self.find_mv_pred(VP_REF_INTER) };
        let mb_type = if hdr.is_intra {
                VPMBType::Intra
            } else {
                self.decode_mb_type(bc, (num_mv + 1) % 3)?
            };
        self.mb_info[mb_pos].mb_type = mb_type;
        if mb_type.get_ref_id() != VP_REF_GOLDEN {
            match mb_type {
                VPMBType::Intra |
                VPMBType::InterNoMV         => {
                    self.mb_info[mb_pos].mv = ZERO_MV;
                },
                VPMBType::InterMV           => {
                    let diff_mv = self.decode_mv(bc, br);
                    self.mb_info[mb_pos].mv = pred_mv + diff_mv;
                },
                VPMBType::InterNearest      => {
                    self.mb_info[mb_pos].mv = nearest_mv;
                },
                VPMBType::InterNear         => {
                    self.mb_info[mb_pos].mv = near_mv;
                },
                VPMBType::InterFourMV       => {
                    for i in 0..4 {
                        four_mbt[i]         = FOURMV_SUB_TYPE[bc.read_bits(2) as usize];
                    }
                    for i in 0..4 {
                        match four_mbt[i] {
                            VPMBType::InterNoMV => {},
                            VPMBType::InterMV   => {
                                let diff_mv = self.decode_mv(bc, br);
                                four_mv[i] = pred_mv + diff_mv;
                            },
                            VPMBType::InterNearest => {
                                four_mv[i] = nearest_mv;
                            },
                            VPMBType::InterNear => {
                                four_mv[i] = near_mv;
                            },
                            _ => unreachable!(),
                        };
                    }
                    self.mb_info[mb_pos].mv = four_mv[3];
                },
                _ => unreachable!(),
            };
        } else {
            let (_num_mv, nearest_mv, near_mv, pred_mv) = self.find_mv_pred(VP_REF_GOLDEN);
            match mb_type {
                VPMBType::GoldenNoMV        => {
                    self.mb_info[mb_pos].mv = ZERO_MV;
                },
                VPMBType::GoldenMV          => {
                    let diff_mv = self.decode_mv(bc, br);
                    self.mb_info[mb_pos].mv = pred_mv + diff_mv;
                },
                VPMBType::GoldenNearest     => {
                    self.mb_info[mb_pos].mv = nearest_mv;
                },
                VPMBType::GoldenNear        => {
                    self.mb_info[mb_pos].mv = near_mv;
                },
                _ => unreachable!(),
            };
        }
        if !mb_type.is_intra() && (mb_type != VPMBType::InterFourMV) {
            self.do_mc(br, frm, mb_type, self.mb_info[mb_pos].mv, alpha);
        } else if mb_type == VPMBType::InterFourMV {
            self.do_fourmv(br, frm, &four_mv, alpha);
        }

        for blk_no in 0..4 {
            self.fstate.plane = if !alpha { 0 } else { 3 };
            self.fstate.ctx_idx = blk_no >> 1;
            self.fstate.top_ctx = self.top_ctx[self.fstate.plane][mb_x * 2 + (blk_no & 1)];
            match cr {
                CoeffReader::None              => {
                    br.decode_block(bc, &mut self.coeffs[blk_no], &self.models.coeff_models[0], &self.models.vp6models, &mut self.fstate)?;
                },
                CoeffReader::Bool(ref mut bcc) => {
                    br.decode_block(bcc, &mut self.coeffs[blk_no], &self.models.coeff_models[0], &self.models.vp6models, &mut self.fstate)?;
                },
                CoeffReader::Huff(ref mut brc) => {
                    br.decode_block_huff(brc, &mut self.coeffs[blk_no], &self.models.vp6models, &self.models.vp6huff, &mut self.fstate)?;
                },
            };
            self.top_ctx[self.fstate.plane][mb_x * 2 + (blk_no & 1)] = self.fstate.top_ctx;
            self.predict_dc(mb_type, mb_pos, blk_no, alpha);

            let bx = mb_x * 2 + (blk_no & 1);
            let by = mb_y * 2 + (blk_no >> 1);
            let has_ac = self.fstate.last_idx[self.fstate.ctx_idx] > 0;
            if mb_type.is_intra() {
                if !self.ilace_mb {
                    if has_ac {
                        vp_put_block(&mut self.coeffs[blk_no], bx, by, self.fstate.plane, frm);
                    } else {
                        vp_put_block_dc(&mut self.coeffs[blk_no], bx, by, self.fstate.plane, frm);
                    }
                } else {
                    vp_put_block_ilace(&mut self.coeffs[blk_no], bx, by, self.fstate.plane, frm);
                }
            } else {
                if !self.ilace_mb {
                    if has_ac {
                        vp_add_block(&mut self.coeffs[blk_no], bx, by, self.fstate.plane, frm);
                    } else {
                        vp_add_block_dc(&mut self.coeffs[blk_no], bx, by, self.fstate.plane, frm);
                    }
                } else {
                    vp_add_block_ilace(&mut self.coeffs[blk_no], bx, by, self.fstate.plane, frm);
                }
            }
        }
        for blk_no in 4..6 {
            self.fstate.plane = blk_no - 3;
            self.fstate.ctx_idx = blk_no - 2;
            self.fstate.top_ctx = self.top_ctx[self.fstate.plane][mb_x];
            match cr {
                CoeffReader::None              => {
                    br.decode_block(bc, &mut self.coeffs[blk_no], &self.models.coeff_models[1], &self.models.vp6models, &mut self.fstate)?;
                },
                CoeffReader::Bool(ref mut bcc) => {
                    br.decode_block(bcc, &mut self.coeffs[blk_no], &self.models.coeff_models[1], &self.models.vp6models, &mut self.fstate)?;
                },
                CoeffReader::Huff(ref mut brc) => {
                    br.decode_block_huff(brc, &mut self.coeffs[blk_no], &self.models.vp6models, &self.models.vp6huff, &mut self.fstate)?;
                },
            };
            self.top_ctx[self.fstate.plane][mb_x] = self.fstate.top_ctx;
            self.predict_dc(mb_type, mb_pos, blk_no, alpha);
            if !alpha {
                let has_ac = self.fstate.last_idx[self.fstate.ctx_idx] > 0;
                if mb_type.is_intra() {
                    if has_ac {
                        vp_put_block(&mut self.coeffs[blk_no], mb_x, mb_y, self.fstate.plane, frm);
                    } else {
                        vp_put_block_dc(&mut self.coeffs[blk_no], mb_x, mb_y, self.fstate.plane, frm);
                    }
                } else {
                    if has_ac {
                        vp_add_block(&mut self.coeffs[blk_no], mb_x, mb_y, self.fstate.plane, frm);
                    } else {
                        vp_add_block_dc(&mut self.coeffs[blk_no], mb_x, mb_y, self.fstate.plane, frm);
                    }
                }
            }
        }
        Ok(())
    }
    fn do_mc(&mut self, br: &dyn VP56Parser, frm: &mut NASimpleVideoFrame<u8>, mb_type: VPMBType, mv: MV, alpha: bool) {
        let x = self.fstate.mb_x * 16;
        let y = self.fstate.mb_y * 16;
        let plane = if !alpha { 0 } else { 3 };
        let src = if mb_type.get_ref_id() == VP_REF_INTER {
                self.shuf.get_last().unwrap()
            } else {
                self.shuf.get_golden().unwrap()
            };

        br.mc_block(frm, self.mc_buf.clone(), src.clone(), plane, x + 0, y + 0, mv, self.loop_thr);
        br.mc_block(frm, self.mc_buf.clone(), src.clone(), plane, x + 8, y + 0, mv, self.loop_thr);
        br.mc_block(frm, self.mc_buf.clone(), src.clone(), plane, x + 0, y + 8, mv, self.loop_thr);
        br.mc_block(frm, self.mc_buf.clone(), src.clone(), plane, x + 8, y + 8, mv, self.loop_thr);
        if !alpha {
            let x = self.fstate.mb_x * 8;
            let y = self.fstate.mb_y * 8;
            br.mc_block(frm, self.mc_buf.clone(), src.clone(), 1, x, y, mv, self.loop_thr);
            br.mc_block(frm, self.mc_buf.clone(), src,         2, x, y, mv, self.loop_thr);
        }
    }
    fn do_fourmv(&mut self, br: &dyn VP56Parser, frm: &mut NASimpleVideoFrame<u8>, mvs: &[MV; 4], alpha: bool) {
        let x = self.fstate.mb_x * 16;
        let y = self.fstate.mb_y * 16;
        let plane = if !alpha { 0 } else { 3 };
        let src = self.shuf.get_last().unwrap();
        for blk_no in 0..4 {
            br.mc_block(frm, self.mc_buf.clone(), src.clone(),
                        plane, x + (blk_no & 1) * 8, y + (blk_no & 2) * 4,
                        mvs[blk_no], self.loop_thr);
        }
        if !alpha {
            let x = self.fstate.mb_x * 8;
            let y = self.fstate.mb_y * 8;
            let sum = mvs[0] + mvs[1] + mvs[2] + mvs[3];
            let mv = MV { x: sum.x / 4, y: sum.y / 4 };
            br.mc_block(frm, self.mc_buf.clone(), src.clone(), 1, x, y, mv, self.loop_thr);
            br.mc_block(frm, self.mc_buf.clone(), src,         2, x, y, mv, self.loop_thr);
        }
    }
    fn predict_dc(&mut self, mb_type: VPMBType, _mb_pos: usize, blk_no: usize, _alpha: bool) {
        let is_luma = blk_no < 4;
        let (plane, dcs) = match blk_no {
                4 => (1, &mut self.dc_pred.dc_u),
                5 => (2, &mut self.dc_pred.dc_v),
                _ => (0, &mut self.dc_pred.dc_y),
             };
        let (dc_ref, dc_idx) = if is_luma {
                (&mut self.dc_pred.ref_y, self.dc_pred.y_idx + (blk_no & 1))
            } else {
                (&mut self.dc_pred.ref_c, self.dc_pred.c_idx)
            };
        let ref_id = mb_type.get_ref_id();
        let mut dc_pred = 0;
        let mut count = 0;
        let has_left_blk = is_luma && ((blk_no & 1) == 1);
        if has_left_blk || self.dc_pred.ref_left == ref_id {
            dc_pred += match blk_no {
                    0 | 1 => self.dc_pred.ldc_y[0],
                    2 | 3 => self.dc_pred.ldc_y[1],
                    4     => self.dc_pred.ldc_u,
                    _     => self.dc_pred.ldc_v,
                };
            count += 1;
        }
        if dc_ref[dc_idx] == ref_id {
            dc_pred += dcs[dc_idx];
            count += 1;
        }
        if self.version == 5 {
            if (count < 2) && (dc_ref[dc_idx - 1] == ref_id) {
                dc_pred += dcs[dc_idx - 1];
                count += 1;
            }
            if (count < 2) && (dc_ref[dc_idx + 1] == ref_id) {
                dc_pred += dcs[dc_idx + 1];
                count += 1;
            }
        }
        if count == 0 {
            dc_pred = self.last_dc[ref_id as usize][plane];
        } else if count == 2 {
            dc_pred /= 2;
        }
        self.coeffs[blk_no][0] += dc_pred;

        let dc = self.coeffs[blk_no][0];
        if blk_no != 4 { // update top block reference only for the second chroma component
            dc_ref[dc_idx] = ref_id;
        }
        match blk_no {
            0 | 1 => {
                self.dc_pred.ldc_y[0] = dc;
            },
            2 | 3 => {
                self.dc_pred.ldc_y[1] = dc;
            },
            4 => {
                self.dc_pred.ldc_u = dc;
            },
            _ => {
                self.dc_pred.ldc_v = dc;
                self.dc_pred.ref_left = ref_id;
            },
        };
        dcs[dc_idx] = dc;

        self.last_dc[ref_id as usize][plane] = dc;
        self.coeffs[blk_no][0] = self.coeffs[blk_no][0].wrapping_mul(self.fstate.dc_quant);
    }
}

const VP56_DC_QUANTS: [i16; 64] = [
    47, 47, 47, 47, 45, 43, 43, 43,
    43, 43, 42, 41, 41, 40, 40, 40,
    40, 35, 35, 35, 35, 33, 33, 33,
    33, 32, 32, 32, 27, 27, 26, 26,
    25, 25, 24, 24, 23, 23, 19, 19,
    19, 19, 18, 18, 17, 16, 16, 16,
    16, 16, 15, 11, 11, 11, 10, 10,
     9,  8,  7,  5,  3,  3,  2,  2
];
const VP56_AC_QUANTS: [i16; 64] = [
    94, 92, 90, 88, 86, 82, 78, 74,
    70, 66, 62, 58, 54, 53, 52, 51,
    50, 49, 48, 47, 46, 45, 44, 43,
    42, 40, 39, 37, 36, 35, 34, 33,
    32, 31, 30, 29, 28, 27, 26, 25,
    24, 23, 22, 21, 20, 19, 18, 17,
    16, 15, 14, 13, 12, 11, 10,  9,
     8,  7,  6,  5,  4,  3,  2,  1
];

const VP56_FILTER_LIMITS: [u8; 64] = [
    14, 14, 13, 13, 12, 12, 10, 10,
    10, 10,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  7,  7,  7,  7,
     7,  7,  6,  6,  6,  6,  6,  6,
     5,  5,  5,  5,  4,  4,  4,  4,
     4,  4,  4,  3,  3,  3,  3,  2
];

const VP56_MODE_VQ: [[[u8; 20]; 16]; 3] = [
  [
    [   9,  15,  32,  25,   7,  19,   9,  21,   1,  12,  14,  12,   3,  18,  14,  23,   3,  10,   0,   4 ],
    [  48,  39,   1,   2,  11,  27,  29,  44,   7,  27,   1,   4,   0,   3,   1,   6,   1,   2,   0,   0 ],
    [  21,  32,   1,   2,   4,  10,  32,  43,   6,  23,   2,   3,   1,  19,   1,   6,  12,  21,   0,   7 ],
    [  69,  83,   0,   0,   0,   2,  10,  29,   3,  12,   0,   1,   0,   3,   0,   3,   2,   2,   0,   0 ],
    [  11,  20,   1,   4,  18,  36,  43,  48,  13,  35,   0,   2,   0,   5,   3,  12,   1,   2,   0,   0 ],
    [  70,  44,   0,   1,   2,  10,  37,  46,   8,  26,   0,   2,   0,   2,   0,   2,   0,   1,   0,   0 ],
    [   8,  15,   0,   1,   8,  21,  74,  53,  22,  42,   0,   1,   0,   2,   0,   3,   1,   2,   0,   0 ],
    [ 141,  42,   0,   0,   1,   4,  11,  24,   1,  11,   0,   1,   0,   1,   0,   2,   0,   0,   0,   0 ],
    [   8,  19,   4,  10,  24,  45,  21,  37,   9,  29,   0,   3,   1,   7,  11,  25,   0,   2,   0,   1 ],
    [  46,  42,   0,   1,   2,  10,  54,  51,  10,  30,   0,   2,   0,   2,   0,   1,   0,   1,   0,   0 ],
    [  28,  32,   0,   0,   3,  10,  75,  51,  14,  33,   0,   1,   0,   2,   0,   1,   1,   2,   0,   0 ],
    [ 100,  46,   0,   1,   3,   9,  21,  37,   5,  20,   0,   1,   0,   2,   1,   2,   0,   1,   0,   0 ],
    [  27,  29,   0,   1,   9,  25,  53,  51,  12,  34,   0,   1,   0,   3,   1,   5,   0,   2,   0,   0 ],
    [  80,  38,   0,   0,   1,   4,  69,  33,   5,  16,   0,   1,   0,   1,   0,   0,   0,   1,   0,   0 ],
    [  16,  20,   0,   0,   2,   8, 104,  49,  15,  33,   0,   1,   0,   1,   0,   1,   1,   1,   0,   0 ],
    [ 194,  16,   0,   0,   1,   1,   1,   9,   1,   3,   0,   0,   0,   1,   0,   1,   0,   0,   0,   0 ],
  ], [
    [  41,  22,   1,   0,   1,  31,   0,   0,   0,   0,   0,   1,   1,   7,   0,   1,  98,  25,   4,  10 ],
    [ 123,  37,   6,   4,   1,  27,   0,   0,   0,   0,   5,   8,   1,   7,   0,   1,  12,  10,   0,   2 ],
    [  26,  14,  14,  12,   0,  24,   0,   0,   0,   0,  55,  17,   1,   9,   0,  36,   5,   7,   1,   3 ],
    [ 209,   5,   0,   0,   0,  27,   0,   0,   0,   0,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0 ],
    [   2,   5,   4,   5,   0, 121,   0,   0,   0,   0,   0,   3,   2,   4,   1,   4,   2,   2,   0,   1 ],
    [ 175,   5,   0,   1,   0,  48,   0,   0,   0,   0,   0,   2,   0,   1,   0,   2,   0,   1,   0,   0 ],
    [  83,   5,   2,   3,   0, 102,   0,   0,   0,   0,   1,   3,   0,   2,   0,   1,   0,   0,   0,   0 ],
    [ 233,   6,   0,   0,   0,   8,   0,   0,   0,   0,   0,   1,   0,   1,   0,   0,   0,   1,   0,   0 ],
    [  34,  16, 112,  21,   1,  28,   0,   0,   0,   0,   6,   8,   1,   7,   0,   3,   2,   5,   0,   2 ],
    [ 159,  35,   2,   2,   0,  25,   0,   0,   0,   0,   3,   6,   0,   5,   0,   1,   4,   4,   0,   1 ],
    [  75,  39,   5,   7,   2,  48,   0,   0,   0,   0,   3,  11,   2,  16,   1,   4,   7,  10,   0,   2 ],
    [ 212,  21,   0,   1,   0,   9,   0,   0,   0,   0,   1,   2,   0,   2,   0,   0,   2,   2,   0,   0 ],
    [   4,   2,   0,   0,   0, 172,   0,   0,   0,   0,   0,   1,   0,   2,   0,   0,   2,   0,   0,   0 ],
    [ 187,  22,   1,   1,   0,  17,   0,   0,   0,   0,   3,   6,   0,   4,   0,   1,   4,   4,   0,   1 ],
    [ 133,   6,   1,   2,   1,  70,   0,   0,   0,   0,   0,   2,   0,   4,   0,   3,   1,   1,   0,   0 ],
    [ 251,   1,   0,   0,   0,   2,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0 ],
  ], [
    [   2,   3,   2,   3,   0,   2,   0,   2,   0,   0,  11,   4,   1,   4,   0,   2,   3,   2,   0,   4 ],
    [  49,  46,   3,   4,   7,  31,  42,  41,   0,   0,   2,   6,   1,   7,   1,   4,   2,   4,   0,   1 ],
    [  26,  25,   1,   1,   2,  10,  67,  39,   0,   0,   1,   1,   0,  14,   0,   2,  31,  26,   1,   6 ],
    [ 103,  46,   1,   2,   2,  10,  33,  42,   0,   0,   1,   4,   0,   3,   0,   1,   1,   3,   0,   0 ],
    [  14,  31,   9,  13,  14,  54,  22,  29,   0,   0,   2,   6,   4,  18,   6,  13,   1,   5,   0,   1 ],
    [  85,  39,   0,   0,   1,   9,  69,  40,   0,   0,   0,   1,   0,   3,   0,   1,   2,   3,   0,   0 ],
    [  31,  28,   0,   0,   3,  14, 130,  34,   0,   0,   0,   1,   0,   3,   0,   1,   3,   3,   0,   1 ],
    [ 171,  25,   0,   0,   1,   5,  25,  21,   0,   0,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0 ],
    [  17,  21,  68,  29,   6,  15,  13,  22,   0,   0,   6,  12,   3,  14,   4,  10,   1,   7,   0,   3 ],
    [  51,  39,   0,   1,   2,  12,  91,  44,   0,   0,   0,   2,   0,   3,   0,   1,   2,   3,   0,   1 ],
    [  81,  25,   0,   0,   2,   9, 106,  26,   0,   0,   0,   1,   0,   1,   0,   1,   1,   1,   0,   0 ],
    [ 140,  37,   0,   1,   1,   8,  24,  33,   0,   0,   1,   2,   0,   2,   0,   1,   1,   2,   0,   0 ],
    [  14,  23,   1,   3,  11,  53,  90,  31,   0,   0,   0,   3,   1,   5,   2,   6,   1,   2,   0,   0 ],
    [ 123,  29,   0,   0,   1,   7,  57,  30,   0,   0,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0 ],
    [  13,  14,   0,   0,   4,  20, 175,  20,   0,   0,   0,   1,   0,   1,   0,   1,   1,   1,   0,   0 ],
    [ 202,  23,   0,   0,   1,   3,   2,   9,   0,   0,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0 ],
  ]
];

