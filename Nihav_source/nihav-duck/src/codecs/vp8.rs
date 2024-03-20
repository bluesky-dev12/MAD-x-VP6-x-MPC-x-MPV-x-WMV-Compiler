use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::vpcommon::*;
use super::vp78::*;
use super::vp78data::*;
use super::vp78dsp::*;
use super::vp8dsp::*;

#[derive(Clone,Copy,PartialEq,Debug,Default)]
enum VP8Ref {
    #[default]
    Intra,
    Last,
    Golden,
    AltRef,
}

#[derive(Default)]
pub struct VP8Shuffler {
    lastframe: Option<NAVideoBufferRef<u8>>,
    goldframe: Option<NAVideoBufferRef<u8>>,
    altframe:  Option<NAVideoBufferRef<u8>>,
}

impl VP8Shuffler {
    pub fn new() -> Self { Self::default() }
    pub fn clear(&mut self) {
        self.lastframe = None;
        self.goldframe = None;
        self.altframe = None;
    }
    pub fn add_frame(&mut self, buf: NAVideoBufferRef<u8>) {
        self.lastframe = Some(buf);
    }
    pub fn add_golden_frame(&mut self, buf: NAVideoBufferRef<u8>) {
        self.goldframe = Some(buf);
    }
    pub fn add_altref_frame(&mut self, buf: NAVideoBufferRef<u8>) {
        self.altframe = Some(buf);
    }
    pub fn get_last(&mut self) -> Option<NAVideoBufferRef<u8>> {
        self.lastframe.as_ref().cloned()
    }
    pub fn get_golden(&mut self) -> Option<NAVideoBufferRef<u8>> {
        self.goldframe.as_ref().cloned()
    }
    pub fn get_altref(&mut self) -> Option<NAVideoBufferRef<u8>> {
        self.altframe.as_ref().cloned()
    }
    pub fn has_refs(&self) -> bool {
        self.lastframe.is_some()
    }
}

struct SBParams<'a> {
    coef_probs: &'a [[[[u8; 11]; 3]; 8]; 4],
    qmat:       &'a [i16; 16],
}

pub const COEF_NE_TREE: &[VPTreeDef<DCTToken>] = &[
    VPTreeDef::Value(DCTToken::Zero),   VPTreeDef::Index(2),
    VPTreeDef::Value(DCTToken::One),    VPTreeDef::Index(4),
    VPTreeDef::Index(6),                VPTreeDef::Index(10),
    VPTreeDef::Value(DCTToken::Two),    VPTreeDef::Index(8),
    VPTreeDef::Value(DCTToken::Three),  VPTreeDef::Value(DCTToken::Four),
    VPTreeDef::Index(12),               VPTreeDef::Index(14),
    VPTreeDef::Value(DCTToken::Cat1),   VPTreeDef::Value(DCTToken::Cat2),
    VPTreeDef::Index(16),               VPTreeDef::Index(18),
    VPTreeDef::Value(DCTToken::Cat3),   VPTreeDef::Value(DCTToken::Cat4),
    VPTreeDef::Value(DCTToken::Cat5),   VPTreeDef::Value(DCTToken::Cat6)
];

fn decode_subblock(bc: &mut BoolCoder, coeffs: &mut [i16; 16], ctype: usize, pctx: u8, sbparams: &SBParams) -> u8 {
    const COEF_BANDS: [usize; 16] = [ 0, 1, 2, 3, 6, 4, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7 ];

    let mut has_nz = 0;
    let start = if ctype != 0 { 0 } else { 1 };
    *coeffs = [0; 16];
    let mut cval = pctx as usize;
    for idx in start..16 {
        let probs = &sbparams.coef_probs[ctype][COEF_BANDS[idx]][cval];
        let tok = if cval != 0 || idx == start {
                                          bc.read_tree(COEF_TREE, probs)
            } else {
                                          bc.read_tree(COEF_NE_TREE, &probs[1..])
            };
        if tok == DCTToken::EOB { break; }
        let level = expand_token(bc, tok);
        coeffs[DEFAULT_SCAN_ORDER[idx]] = level.wrapping_mul(sbparams.qmat[idx]);
        cval = level.abs().min(2) as usize;
        has_nz |= cval;
    }
    if has_nz > 0 { 1 } else { 0 }
}

#[derive(Clone,Copy,Default)]
struct MBInfo {
    mb_type:    VPMBType,
    ymode:      PredMode,
    uvmode:     PredMode,
    loop_str:   u8,
    inner_filt: bool,
    rframe:     VP8Ref,
}

#[derive(Clone,Copy,Default)]
struct Segment {
    quant:      i8,
    lf:         i8,
}

#[derive(Default)]
struct SavedProbs {
    kf_ymode_prob:      [u8; 4],
    kf_uvmode_prob:     [u8; 3],

    coef_probs:         [[[[u8; 11]; 3]; 8]; 4],
    mv_probs:           [[u8; 19]; 2],

    segment_probs:      [u8; 3],
}

#[derive(Default)]
struct DecoderState {
    lf_simple:          bool,
    loop_filter_level:  u8,
    loop_sharpness:     u8,

    is_intra:           bool,
    version:            u8,

    kf_ymode_prob:      [u8; 4],
    kf_uvmode_prob:     [u8; 3],

    prob_intra_pred:    u8,
    prob_last_pred:     u8,
    prob_gold_pred:     u8,
    sign_bias:          [bool; 2],

    coef_probs:         [[[[u8; 11]; 3]; 8]; 4],
    mv_probs:           [[u8; 19]; 2],

    segmentation:       bool,
    update_seg_map:     bool,
    force_quant:        Option<u8>,
    force_loop_str:     Option<u8>,
    segment_probs:      [u8; 3],
    seg:                [Segment; 4],
    seg_feature_mode:   bool,

    lf_delta:           bool,
    lf_frame_delta:     [i8; 4],
    lf_mode_delta:      [i8; 4],

    has_y2:             bool,

    ipred_ctx_y:        IPredContext,
    ipred_ctx_u:        IPredContext,
    ipred_ctx_v:        IPredContext,
}

impl DecoderState {
    fn reset(&mut self) {
        const VP8_DEFAULT_MV_PROBS: [[u8; 19]; 2] = [
          [ 162, 128, 225, 146, 172, 147, 214,  39, 156, 128, 129, 132,  75, 145, 178, 206, 239, 254, 254 ],
          [ 164, 128, 204, 170, 119, 235, 140, 230, 228, 128, 130, 130,  74, 148, 180, 203, 236, 254, 254 ]
        ];

        self.kf_ymode_prob.copy_from_slice(Y_MODE_TREE_PROBS);
        self.kf_uvmode_prob.copy_from_slice(UV_MODE_TREE_PROBS);
        self.coef_probs.copy_from_slice(&DEFAULT_DCT_PROBS);
        self.mv_probs.copy_from_slice(&VP8_DEFAULT_MV_PROBS);
        self.segment_probs = [255; 3];
        self.seg = [Segment::default(); 4];
    }
    fn restore(&mut self, dst: &SavedProbs) {
        self.kf_ymode_prob      = dst.kf_ymode_prob;
        self.kf_uvmode_prob     = dst.kf_uvmode_prob;
        self.coef_probs         = dst.coef_probs;
        self.mv_probs           = dst.mv_probs;
        self.segment_probs      = dst.segment_probs;
    }
    fn save(&self, dst: &mut SavedProbs) {
        dst.kf_ymode_prob       = self.kf_ymode_prob;
        dst.kf_uvmode_prob      = self.kf_uvmode_prob;
        dst.coef_probs          = self.coef_probs;
        dst.mv_probs            = self.mv_probs;
//        dst.segment_probs       = self.segment_probs;
    }
}

fn decode_mv_component(bc: &mut BoolCoder, probs: &[u8; 19]) -> i16 {
    const LONG_VECTOR_ORDER: [usize; 9] = [ 0, 1, 2, 9, 8, 7, 6, 5, 4 ];

    let val = if !bc.read_prob(probs[0]) {
                                     bc.read_tree(SMALL_MV_TREE, &probs[2..9])
        } else {
            let raw_probs = &probs[9..];
            let mut raw = 0;
            for ord in LONG_VECTOR_ORDER.iter() {
                raw                  |= (bc.read_prob(raw_probs[*ord]) as i16) << *ord;
            }
            if (raw & 0x3F0) != 0 {
                raw                  |= (bc.read_prob(raw_probs[3]) as i16) << 3;
            } else {
                raw |= 1 << 3;
            }
            raw
        };
    if (val == 0) || !bc.read_prob(probs[1]) {
        val
    } else {
        -val
    }
}

struct VP8Decoder {
    info:           NACodecInfoRef,

    shuf:           VP8Shuffler,
    width:          usize,
    height:         usize,
    mb_w:           usize,
    mb_h:           usize,
    mb_info:        Vec<MBInfo>,
    mvs:            Vec<MV>,
    mv_stride:      usize,

    ymodes:         Vec<PredMode>,
    ymode_stride:   usize,
    uvmodes:        Vec<PredMode>,
    uvmode_stride:  usize,

    dstate:         DecoderState,
    pcache:         PredCache,
    tmp_probs:      SavedProbs,

    coeffs:         [[i16; 16]; 25],
    qmat:           [[[i16; 16]; 3]; 5],

    mc_buf:         NAVideoBufferRef<u8>,

    seg_map:        Vec<u8>,
}

impl VP8Decoder {
    fn new() -> Self {
        let vt = alloc_video_buffer(NAVideoInfo::new(128, 128, false, YUV420_FORMAT), 4).unwrap();
        let mc_buf = vt.get_vbuf().unwrap();
        Self {
            info:           NACodecInfoRef::default(),

            shuf:           VP8Shuffler::new(),
            width:          0,
            height:         0,
            mb_w:           0,
            mb_h:           0,
            mb_info:        Vec::new(),
            mvs:            Vec::new(),
            mv_stride:      0,

            ymodes:         Vec::new(),
            ymode_stride:   0,
            uvmodes:        Vec::new(),
            uvmode_stride:  0,

            dstate:         DecoderState::default(),
            pcache:         PredCache::new(),
            tmp_probs:      SavedProbs::default(),

            coeffs:         [[0; 16]; 25],
            qmat:           [[[0; 16]; 3]; 5],

            mc_buf,

            seg_map:        Vec::new(),
        }
    }
    fn set_dimensions(&mut self, width: usize, height: usize) {
        if (width == self.width) && (height == self.height) {
            return;
        }
        self.width  = width;
        self.height = height;
        self.mb_w   = (self.width  + 15) >> 4;
        self.mb_h   = (self.height + 15) >> 4;
        self.mb_info.resize(self.mb_w * self.mb_h, MBInfo::default());
        self.mv_stride = self.mb_w * 4;
        self.mvs.resize(self.mv_stride * self.mb_h * 4, ZERO_MV);

        self.ymode_stride   = self.mb_w * 4;
        self.uvmode_stride  = self.mb_w;
        self.ymodes.resize(self.ymode_stride * self.mb_h * 4, PredMode::default());
        self.uvmodes.resize(self.uvmode_stride * self.mb_h, PredMode::default());

        self.pcache.resize(self.mb_w);

        self.seg_map.clear();
        self.seg_map.resize(self.mb_w * self.mb_h, 0);
    }
    fn update_segmentation(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        self.dstate.update_seg_map          = bc.read_bool();
        if bc.read_bool() {
            self.dstate.seg_feature_mode    = bc.read_bool();
            for seg in self.dstate.seg.iter_mut() {
                if bc.read_bool() {
                    let quant_upd_val       = bc.read_bits(7) as i8;
                    let quant_upd_sign      = bc.read_bool();
                    seg.quant = if !quant_upd_sign { quant_upd_val } else { -quant_upd_val };
                }
            }
            for seg in self.dstate.seg.iter_mut() {
                if bc.read_bool() {
                    let lf_upd_val          = bc.read_bits(6) as i8;
                    let lf_upd_sign         = bc.read_bool();
                    seg.lf = if !lf_upd_sign { lf_upd_val } else { -lf_upd_val };
                }
            }
        }
        if self.dstate.update_seg_map {
            self.tmp_probs.segment_probs = self.dstate.segment_probs;
            for prob in self.dstate.segment_probs.iter_mut() {
                if bc.read_bool() {
                    *prob                    = bc.read_byte();
                }
            }
        }
        Ok(())
    }
    fn mb_lf_adjustments(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        self.dstate.lf_delta                = bc.read_bool();
        if self.dstate.lf_delta {
            if bc.read_bool() {
                for frame_delta in self.dstate.lf_frame_delta.iter_mut() {
                    if bc.read_bool() {
                        let delta_magn      = bc.read_bits(6) as i8;
                        let delta_sign      = bc.read_bool();
                        *frame_delta = if !delta_sign { delta_magn } else { -delta_magn };
                    }
                }
                for mode_delta in self.dstate.lf_mode_delta.iter_mut() {
                    if bc.read_bool() {
                        let delta_magn      = bc.read_bits(6) as i8;
                        let delta_sign      = bc.read_bool();
                        *mode_delta = if !delta_sign { delta_magn } else { -delta_magn };
                    }
                }
            }
        }
        Ok(())
    }
    fn read_delta_quant(bc: &mut BoolCoder, y_ac_q: usize) -> DecoderResult<usize> {
        if bc.read_bool() {
            let delta                   = bc.read_bits(4) as usize;
            if bc.read_bool() {
                Ok(y_ac_q.saturating_sub(delta))
            } else {
                Ok((y_ac_q + delta).min(127))
            }
        } else {
            Ok(y_ac_q)
        }
    }
    fn quant_indices(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        let y_ac_q                      = bc.read_bits(7) as usize;
        let y_dc_q  = Self::read_delta_quant(bc, y_ac_q)?;
        let y2_dc_q = Self::read_delta_quant(bc, y_ac_q)?;
        let y2_ac_q = Self::read_delta_quant(bc, y_ac_q)?;
        let uv_dc_q = Self::read_delta_quant(bc, y_ac_q)?;
        let uv_ac_q = Self::read_delta_quant(bc, y_ac_q)?;
        self.set_qmat(y_dc_q, y_ac_q, y2_dc_q, y2_ac_q, uv_dc_q, uv_ac_q);

        Ok(())
    }
    fn read_dct_coef_prob_upd(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        for i in 0..4 {
            for j in 0..8 {
                for k in 0..3 {
                    for l in 0..11 {
                        if bc.read_prob(DCT_UPDATE_PROBS[i][j][k][l]) {
                            self.dstate.coef_probs[i][j][k][l]  = bc.read_byte();
                        }
                    }
                }
            }
        }
        Ok(())
    }
    fn read_mv_prob_upd(&mut self, bc: &mut BoolCoder) -> DecoderResult<()> {
        const MV_UPDATE_PROBS: [[u8; 19]; 2] = [
          [ 237, 246, 253, 253, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 250, 250, 252, 254, 254 ],
          [ 231, 243, 245, 253, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 251, 251, 254, 254, 254 ]
        ];
        for comp in 0..2 {
            for i in 0..19 {
                if bc.read_prob(MV_UPDATE_PROBS[comp][i]) {
                    self.dstate.mv_probs[comp][i]   = bc.read_probability();
                }
            }
        }
        Ok(())
    }
    fn decode_mb_features(&mut self, bc: &mut BoolCoder, mb_idx: usize) -> DecoderResult<()> {
        let segment_id                  = bc.read_tree(FEATURE_TREE, &self.dstate.segment_probs);
        self.seg_map[mb_idx] = segment_id as u8;

        Ok(())
    }
    fn set_cur_segment(&mut self, mb_idx: usize) {
        self.dstate.force_quant = Some(self.seg_map[mb_idx]);
        let seg_id = self.seg_map[mb_idx] as usize;
        let segment = &self.dstate.seg[seg_id];
        let loop_str = if self.dstate.seg_feature_mode {
                segment.lf as u8
            } else {
                (i16::from(self.dstate.loop_filter_level) + i16::from(segment.lf)).max(0).min(63) as u8
            };
        self.dstate.force_loop_str = Some(loop_str);
    }
    fn decode_residue(&mut self, bc: &mut BoolCoder, mb_x: usize) -> bool {
        let qmat_idx = if let Some(idx) = self.dstate.force_quant { (idx as usize) + 1 } else { 0 };
        let mut sbparams = SBParams {
                qmat:       &self.qmat[qmat_idx][2],
                coef_probs: &self.dstate.coef_probs,
            };
        let mut has_ac = [false; 25];
        let mut coded = false;
        let ytype;
        if self.dstate.has_y2 {
            let pred = &self.pcache.y2_pred;
            let pidx = pred.xpos + mb_x;
            let pctx = self.pcache.y2_pred_left + pred.data[pidx - pred.stride];

            let has_nz = decode_subblock(bc, &mut self.coeffs[24], 1, pctx, &sbparams);
            self.pcache.y2_pred.data[pidx] = has_nz;
            self.pcache.y2_pred_left = has_nz;
            has_ac[24] = has_nz > 0;
            coded |= has_ac[24] | (self.coeffs[24][0] != 0);

            ytype = 0;
        } else {
            let pred = &mut self.pcache.y2_pred;
            let pidx = pred.xpos + mb_x;
            pred.data[pidx] = pred.data[pidx - pred.stride];

            ytype = 3;
        }
        sbparams.qmat = &self.qmat[qmat_idx][0];
        for i in 0..16 {
            let bx = i & 3;
            let by = i >> 2;
            let pred = &self.pcache.y_pred;
            let pidx = pred.xpos + mb_x * 4 + bx + by * pred.stride;
            let pctx = self.pcache.y_pred_left[by] + pred.data[pidx - pred.stride];

            let has_nz = decode_subblock(bc, &mut self.coeffs[i], ytype, pctx, &sbparams);
            self.pcache.y_pred.data[pidx] = has_nz;
            self.pcache.y_pred_left[by]   = has_nz;
            has_ac[i] = has_nz > 0;
            coded |= has_ac[i] | (self.coeffs[i][0] != 0);
        }
        sbparams.qmat = &self.qmat[qmat_idx][1];
        for i in 16..20 {
            let bx = i & 1;
            let by = (i >> 1) & 1;
            let pred = &self.pcache.u_pred;
            let pidx = pred.xpos + mb_x * 2 + bx + by * pred.stride;
            let pctx = self.pcache.u_pred_left[by] + pred.data[pidx - pred.stride];

            let has_nz = decode_subblock(bc, &mut self.coeffs[i], 2, pctx, &sbparams);
            self.pcache.u_pred.data[pidx] = has_nz;
            self.pcache.u_pred_left[by]   = has_nz;
            has_ac[i] = has_nz > 0;
            coded |= has_ac[i] | (self.coeffs[i][0] != 0);
        }
        for i in 20..24 {
            let bx = i & 1;
            let by = (i >> 1) & 1;
            let pred = &self.pcache.v_pred;
            let pidx = pred.xpos + mb_x * 2 + bx + by * pred.stride;
            let pctx = self.pcache.v_pred_left[by] + pred.data[pidx - pred.stride];

            let has_nz = decode_subblock(bc, &mut self.coeffs[i], 2, pctx, &sbparams);
            self.pcache.v_pred.data[pidx] = has_nz;
            self.pcache.v_pred_left[by]   = has_nz;
            has_ac[i] = has_nz > 0;
            coded |= has_ac[i] | (self.coeffs[i][0] != 0);
        }

        if self.dstate.has_y2 {
            let y2block = &mut self.coeffs[24];
            if has_ac[24] {
                iwht4x4(y2block);
            } else if y2block[0] != 0 {
                iwht4x4_dc(y2block);
            }
            for i in 0..16 {
                self.coeffs[i][0] = self.coeffs[24][i];
            }
        }
        for i in 0..24 {
            if has_ac[i] {
                idct4x4(&mut self.coeffs[i]);
            } else if self.coeffs[i][0] != 0 {
                idct4x4_dc(&mut self.coeffs[i]);
            }
        }

        coded
    }

    fn set_single_qmat(qmat: &mut [[i16; 16]; 3], y_dc_q: usize, y_ac_q: usize, y2_dc_q: usize, y2_ac_q: usize, uv_dc_q: usize, uv_ac_q: usize) {
        qmat[0][0] = DC_QUANTS[y_dc_q];
        for i in 1..16 {
            qmat[0][i] = AC_QUANTS[y_ac_q];
        }
        qmat[1][0] = DC_QUANTS[uv_dc_q].min(132);
        for i in 1..16 {
            qmat[1][i] = AC_QUANTS[uv_ac_q];
        }
        qmat[2][0] = DC_QUANTS[y2_dc_q] * 2;
        for i in 1..16 {
            qmat[2][i] = (i32::from(AC_QUANTS[y2_ac_q]) * 155 / 100).max(8) as i16;
        }
    }
    fn set_qmat(&mut self, y_dc_q: usize, y_ac_q: usize, y2_dc_q: usize, y2_ac_q: usize, uv_dc_q: usize, uv_ac_q: usize) {
        Self::set_single_qmat(&mut self.qmat[0], y_dc_q, y_ac_q, y2_dc_q, y2_ac_q, uv_dc_q, uv_ac_q);
        if self.dstate.segmentation {
            for (qmat, seg) in self.qmat[1..].iter_mut().zip(self.dstate.seg.iter()) {
                let q = if self.dstate.seg_feature_mode {
                        seg.quant.max(0) as usize
                    } else {
                        ((y_ac_q as i16) + i16::from(seg.quant)).max(0).min(127) as usize
                    };
                Self::set_single_qmat(qmat, q, q, q, q, q, q);
            }
        }
    }
    fn fill_ymode(&mut self, mb_x: usize, mb_y: usize, ymode: PredMode) {
        let mut iidx = mb_x * 4 + mb_y * 4 * self.ymode_stride;
        for _ in 0..4 {
            for x in 0..4 {
                self.ymodes[iidx + x] = ymode;
            }
            iidx += self.ymode_stride;
        }
    }
    fn fill_mv(&mut self, mb_x: usize, mb_y: usize, mv: MV) {
        let mut iidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        for _ in 0..4 {
            for x in 0..4 {
                self.mvs[iidx + x] = mv;
            }
            iidx += self.mb_w * 4;
        }
    }
    fn get_frame_sign(&self, rframe: VP8Ref) -> bool {
        match rframe {
            VP8Ref::Golden => self.dstate.sign_bias[0],
            VP8Ref::AltRef => self.dstate.sign_bias[1],
            _ => false,
        }
    }
    fn find_mv_pred(&self, mb_x: usize, mb_y: usize, frm_sign: bool) -> ([u8; 4], MV, MV, MV) {
        const VP8_MV_PROBS: [[u8; 4]; 6] = [
            [   7,   1,   1, 143 ],
            [  14,  18,  14, 107 ],
            [ 135,  64,  57,  68 ],
            [  60,  56, 128,  65 ],
            [ 159, 134, 128,  34 ],
            [ 234, 188, 128,  28 ]
        ];

        const OFFS: [(u8, u8, u8); 3] = [(0, 1, 2), (1, 0, 2), (1, 1, 1)];
        let mut mvs = [ZERO_MV; 3];
        let mut mvc = [0; 3];
        let mut num_mv = 0;
        let mut split_w = 0;

        let mut nearest_mv = ZERO_MV;
        let mut near_mv = ZERO_MV;

        for &(x, y, weight) in OFFS.iter() {
            let mv = if (x == 0 || mb_x > 0) && (y == 0 || mb_y > 0) {
                    let x = usize::from(x);
                    let y = usize::from(y);
                    let mb_idx = mb_x - x + (mb_y - y) * self.mb_w;
                    if self.mb_info[mb_idx].mb_type.is_intra() {
                        continue;
                    }
                    if self.mb_info[mb_idx].mb_type == VPMBType::InterFourMV {
                        split_w += weight;
                    }
                    let rsign = self.get_frame_sign(self.mb_info[mb_idx].rframe);
                    let mut mv_idx = mb_x * 4 + mb_y * 4 * self.mv_stride;
                    if y == 0 { // left
                        mv_idx += self.mv_stride * 3 - 1;
                    } else if x == 0 { // top
                        mv_idx -= self.mv_stride;
                        mv_idx += 3;
                    } else {
                        mv_idx -= self.mv_stride + 1;
                    }
                    if rsign == frm_sign {
                        self.mvs[mv_idx]
                    } else {
                        -self.mvs[mv_idx]
                    }
                } else {
                    continue;
                };
            let mut found = false;
            for i in 0..num_mv {
                if mvs[i] == mv {
                    mvc[i] += weight;
                    found = true;
                    break;
                }
            }
            if !found {
                mvs[num_mv] = mv;
                mvc[num_mv] = weight;
                num_mv += 1;
            }
        }

        match num_mv {
            2 => {
                if mvc[0] < mvc[1] {
                    mvs.swap(0, 1);
                    mvc.swap(0, 1);
                }
            },
            3 => {
                if mvc[1] < mvc[2] {
                    mvs.swap(1, 2);
                    mvc.swap(1, 2);
                }
                if mvc[0] < mvc[1] {
                    mvs.swap(0, 1);
                    mvc.swap(0, 1);
                }
                if mvc[1] < mvc[2] {
                    mvs.swap(1, 2);
                    mvc.swap(1, 2);
                }
            },
            _ => {},
        };

        let mut best_mv = mvs[0];

        let mut ct = [0; 4];
        for (&mv, &count) in mvs[..num_mv].iter().zip(mvc.iter()) {
            if mv != ZERO_MV {
                if nearest_mv == ZERO_MV {
                    nearest_mv = mv;
                    if mvc[0] == count {
                        best_mv = mv;
                    }
                    ct[1] = count;
                } else {
                    near_mv = mv;
                    ct[2] = count;
                    break;
                }
            }
        }
        for (&mv, &count) in mvs[..num_mv].iter().zip(mvc.iter()) {
            if mv == ZERO_MV {
                ct[0] = count;
                break;
            }
        }
        ct[3] = split_w;
        let best_mv = self.clip_mv(best_mv, mb_x, mb_y);

        let mvprobs = [VP8_MV_PROBS[ct[0] as usize][0],
                       VP8_MV_PROBS[ct[1] as usize][1],
                       VP8_MV_PROBS[ct[2] as usize][2],
                       VP8_MV_PROBS[ct[3] as usize][3]];

        (mvprobs, self.clip_mv(nearest_mv, mb_x, mb_y), self.clip_mv(near_mv, mb_x, mb_y), best_mv)
    }
    fn clip_mv(&self, mv: MV, mb_x: usize, mb_y: usize) -> MV {
        let pos_x = (mb_x as i32) * 16 * 4;
        let pos_y = (mb_y as i32) * 16 * 4;
        let mv_x = (pos_x + i32::from(mv.x)).max(-16 * 4).min((self.mb_w as i32) * 16 * 4);
        let mv_y = (pos_y + i32::from(mv.y)).max(-16 * 4).min((self.mb_h as i32) * 16 * 4);
         MV {x: (mv_x - pos_x) as i16, y: (mv_y - pos_y) as i16 }
    }
    fn get_split_mv(&self, bc: &mut BoolCoder, mb_x: usize, mb_y: usize, bx: usize, by: usize, pred_mv: MV) -> MV {
        const SUB_MV_REF_PROBS: [[u8; 3]; 5] = [
            [ 147, 136, 18 ],
            [ 106, 145,  1 ],
            [ 179, 121,  1 ],
            [ 223,   1, 34 ],
            [ 208,   1,  1 ]
        ];

        let mvidx = mb_x * 4 + bx + (mb_y * 4 + by) * self.mv_stride;
        let left_mv = if (mb_x > 0) || (bx > 0) {
                self.mvs[mvidx - 1]
            } else {
                ZERO_MV
            };
        let top_mv = if (mb_y > 0) || (by > 0) {
                self.mvs[mvidx - self.mv_stride]
            } else {
                ZERO_MV
            };

        let idx = if left_mv == top_mv {
                if left_mv == ZERO_MV {
                    4
                } else {
                    3
                }
            } else if top_mv == ZERO_MV {
                2
            } else if left_mv == ZERO_MV {
                1
            } else {
                0
            };
        let mode                        = bc.read_tree(SUB_MV_REF_TREE, &SUB_MV_REF_PROBS[idx]);
        match mode {
            SubMVRef::Left => left_mv,
            SubMVRef::Above => top_mv,
            SubMVRef::Zero => ZERO_MV,
            SubMVRef::New => {
                let dmy = decode_mv_component(bc, &self.dstate.mv_probs[0]);
                let dmx = decode_mv_component(bc, &self.dstate.mv_probs[1]);
                pred_mv + MV{ x: dmx, y: dmy }
            },
        }
    }
    fn do_split_mv(&mut self, bc: &mut BoolCoder, mb_x: usize, mb_y: usize, pred_mv: MV) -> DecoderResult<()> {
        let split_mode                  = bc.read_tree(MV_SPLIT_MODE_TREE, &MV_SPLIT_MODE_PROBS);
        let mut mvidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        match split_mode {
            MVSplitMode::TopBottom => {
                let top_mv = self.get_split_mv(bc, mb_x, mb_y, 0, 0, pred_mv);
                for _ in 0..2 {
                    for x in 0..4 { self.mvs[mvidx + x] = top_mv; }
                    mvidx += self.mv_stride;
                }
                let bot_mv = self.get_split_mv(bc, mb_x, mb_y, 0, 2, pred_mv);
                for _ in 2..4 {
                    for x in 0..4 { self.mvs[mvidx + x] = bot_mv; }
                    mvidx += self.mv_stride;
                }
            },
            MVSplitMode::LeftRight => {
                let left_mv  = self.get_split_mv(bc, mb_x, mb_y, 0, 0, pred_mv);
                self.mvs[mvidx + 1] = left_mv;
                let right_mv = self.get_split_mv(bc, mb_x, mb_y, 2, 0, pred_mv);
                for _ in 0..4 {
                    self.mvs[mvidx + 0] = left_mv;
                    self.mvs[mvidx + 1] = left_mv;
                    self.mvs[mvidx + 2] = right_mv;
                    self.mvs[mvidx + 3] = right_mv;
                    mvidx += self.mv_stride;
                }
            },
            MVSplitMode::Quarters => {
                for y in (0..4).step_by(2) {
                    for x in (0..4).step_by(2) {
                        self.mvs[mvidx + x] = self.get_split_mv(bc, mb_x, mb_y, x, y, pred_mv);
                        self.mvs[mvidx + x + 1] = self.mvs[mvidx + x];
                    }
                    for x in 0..4 {
                        self.mvs[mvidx + x + self.mv_stride] = self.mvs[mvidx + x];
                    }
                    mvidx += self.mv_stride * 2;
                }
            },
            MVSplitMode::Sixteenths => {
                for y in 0..4 {
                    for x in 0..4 {
                        self.mvs[mvidx + x] = self.get_split_mv(bc, mb_x, mb_y, x, y, pred_mv);
                    }
                    mvidx += self.mv_stride;
                }
            },
        };
        Ok(())
    }

    fn add_residue(&self, dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize, do_luma: bool) {
        if do_luma {
            let ydst = &mut dframe.data[dframe.offset[0]..];
            let ystride = dframe.stride[0];
            let mut yoff = mb_x * 16 + mb_y * 16 * ystride;
            for y in 0..4 {
                for x in 0..4 {
                    add_coeffs4x4(ydst, yoff + x * 4, ystride, &self.coeffs[x + y * 4]);
                }
                yoff += 4 * ystride;
            }
        }
        let dst = &mut dframe.data[0..];
        let mut uoff = dframe.offset[1] + mb_x * 8 + mb_y * 8 * dframe.stride[1];
        let ustride = dframe.stride[1];
        let mut voff = dframe.offset[2] + mb_x * 8 + mb_y * 8 * dframe.stride[2];
        let vstride = dframe.stride[2];
        for y in 0..2 {
            for x in 0..2 {
                add_coeffs4x4(dst, uoff + x * 4, ustride, &self.coeffs[16 + x + y * 2]);
                add_coeffs4x4(dst, voff + x * 4, vstride, &self.coeffs[20 + x + y * 2]);
            }
            uoff += ustride * 4;
            voff += vstride * 4;
        }
    }
    fn recon_intra_mb(&mut self, dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize, mb_coeff_skip: bool) -> DecoderResult<()> {
        let mb_idx = mb_x + mb_y * self.mb_w;
        let has_top = mb_y > 0;
        let has_left = mb_x > 0;
        let ydst = &mut dframe.data[dframe.offset[0]..];
        let ystride = dframe.stride[0];
        let mut yoff = mb_x * 16 + mb_y * 16 * ystride;
        let ipred_ctx_y = &mut self.dstate.ipred_ctx_y;
        ipred_ctx_y.has_top  = has_top;
        ipred_ctx_y.has_left = has_left;
        let is_normal = self.mb_info[mb_idx].ymode != PredMode::BPred;
        if is_normal {
            ipred_ctx_y.fill(ydst, yoff, ystride, 16, 16);
            if !has_top && self.mb_info[mb_idx].ymode == PredMode::VPred {
                IPred16x16::ipred_const(ydst, yoff, ystride, 0x7F)
            } else if !has_left && self.mb_info[mb_idx].ymode == PredMode::HPred {
                IPred16x16::ipred_const(ydst, yoff, ystride, 0x81)
            } else {
                match self.mb_info[mb_idx].ymode {
                    PredMode::DCPred => IPred16x16::ipred_dc(ydst, yoff, ystride, ipred_ctx_y),
                    PredMode::HPred  => IPred16x16::ipred_h (ydst, yoff, ystride, ipred_ctx_y),
                    PredMode::VPred  => IPred16x16::ipred_v (ydst, yoff, ystride, ipred_ctx_y),
                    PredMode::TMPred => IPred16x16::ipred_tm(ydst, yoff, ystride, ipred_ctx_y),
                    _ => unreachable!(),
                };
            }
        } else {
            let mut iidx = mb_x * 4 + mb_y * 4 * self.ymode_stride;
            let mut tr_save = [0x7Fu8; 16];
            let tr_edge = if has_top { ydst[yoff - ystride + 15] } else { 0x7F };
            for y in 0..4 {
                for x in 0..4 {
                    ipred_ctx_y.has_left = has_left || x > 0;
                    let bmode = self.ymodes[iidx + x];
                    let cur_yoff = yoff + x * 4;
                    let has_tr = ipred_ctx_y.has_top && ((x < 3) || ((y == 0) && (mb_y < self.mb_w - 1)));
                    let has_dl = ipred_ctx_y.has_left && (x == 0) && (y < 3);
                    ipred_ctx_y.fill(ydst, cur_yoff, ystride,
                                     if has_tr { 8 } else { 4 },
                                     if has_dl { 8 } else { 4 });

                    if !has_top && y == 0 && (has_left || x > 0) && bmode != PredMode::TMPred {
                        ipred_ctx_y.top = [0x7F; 16];
                        ipred_ctx_y.tl = 0x7F;
                    }
                    if !has_left && x == 0 && (has_top || y > 0) && bmode != PredMode::TMPred {
                        ipred_ctx_y.left = [0x81; 16];
                        ipred_ctx_y.tl = 0x81;
                    }
                    if !has_left && !has_top && x == 0 && y == 0 && bmode != PredMode::DCPred {
                        ipred_ctx_y.top = [0x7F; 16];
                        ipred_ctx_y.left = [0x81; 16];
                        ipred_ctx_y.tl = 0x7F;
                    }

                    if !has_tr {
                        for i in 0..4 {
                            ipred_ctx_y.top[i + 4] = tr_save[x * 4 + i];
                        }
                    } else {
                        for i in 0..4 {
                            tr_save[x * 4 + i] = ipred_ctx_y.top[i + 4];
                        }
                    }
                    if (mb_x == self.mb_w - 1) && has_top && (x == 3) {
                        for i in 0..4 {
                            ipred_ctx_y.top[i + 4] = tr_edge;
                        }
                    }
                    match bmode {
                        PredMode::DCPred => IPred4x4::ipred_dc(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::TMPred => IPred4x4::ipred_tm(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::HPred  => IPred4x4::ipred_he(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::VPred  => IPred4x4::ipred_ve(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::LDPred => IPred4x4::ipred_ld(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::RDPred => IPred4x4::ipred_rd(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::VRPred => IPred4x4::ipred_vr(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::VLPred => IPred4x4::ipred_vl(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::HDPred => IPred4x4::ipred_hd(ydst, cur_yoff, ystride, ipred_ctx_y),
                        PredMode::HUPred => IPred4x4::ipred_hu(ydst, cur_yoff, ystride, ipred_ctx_y),
                        _ => unreachable!(),
                    };
                    if !mb_coeff_skip {
                        add_coeffs4x4(ydst, cur_yoff, ystride, &self.coeffs[x + y * 4]);
                    }
                }
                ipred_ctx_y.has_top = true;
                yoff += 4 * ystride;
                iidx += self.ymode_stride;
            }
        }
        let dst = &mut dframe.data[0..];
        let uoff = dframe.offset[1] + mb_x * 8 + mb_y * 8 * dframe.stride[1];
        let ustride = dframe.stride[1];
        let voff = dframe.offset[2] + mb_x * 8 + mb_y * 8 * dframe.stride[2];
        let vstride = dframe.stride[2];
        let ipred_ctx_u = &mut self.dstate.ipred_ctx_u;
        let ipred_ctx_v = &mut self.dstate.ipred_ctx_v;
        ipred_ctx_u.has_top  = has_top;
        ipred_ctx_v.has_top  = has_top;
        ipred_ctx_u.has_left = has_left;
        ipred_ctx_v.has_left = has_left;
        ipred_ctx_u.fill(dst, uoff, ustride, 8, 8);
        ipred_ctx_v.fill(dst, voff, vstride, 8, 8);

        if !has_top && self.mb_info[mb_idx].uvmode == PredMode::VPred {
            IPred8x8::ipred_const(dst, uoff, ustride, 0x7F);
            IPred8x8::ipred_const(dst, voff, vstride, 0x7F);
        } else if !has_left && self.mb_info[mb_idx].uvmode == PredMode::HPred {
            IPred8x8::ipred_const(dst, uoff, ustride, 0x81);
            IPred8x8::ipred_const(dst, voff, vstride, 0x81);
        } else {
            match self.mb_info[mb_idx].uvmode {
                PredMode::DCPred => {
                    IPred8x8::ipred_dc(dst, uoff, ustride, ipred_ctx_u);
                    IPred8x8::ipred_dc(dst, voff, vstride, ipred_ctx_v);
                },
                PredMode::HPred => {
                    IPred8x8::ipred_h(dst, uoff, ustride, ipred_ctx_u);
                    IPred8x8::ipred_h(dst, voff, vstride, ipred_ctx_v);
                },
                PredMode::VPred => {
                    IPred8x8::ipred_v(dst, uoff, ustride, ipred_ctx_u);
                    IPred8x8::ipred_v(dst, voff, vstride, ipred_ctx_v);
                },
                PredMode::TMPred => {
                    IPred8x8::ipred_tm(dst, uoff, ustride, ipred_ctx_u);
                    IPred8x8::ipred_tm(dst, voff, vstride, ipred_ctx_v);
                },
                _ => unreachable!(),
            };
        }
        if !mb_coeff_skip {
            self.add_residue(dframe, mb_x, mb_y, is_normal);
        }
        Ok(())
    }
    fn recon_inter_mb(&mut self, dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize, mb_coeff_skip: bool, rframe: VP8Ref) {
        let refframe = match rframe {
                VP8Ref::Last   => self.shuf.get_last(),
                VP8Ref::Golden => self.shuf.get_golden(),
                VP8Ref::AltRef => self.shuf.get_altref(),
                VP8Ref::Intra => unreachable!(),
            }.unwrap();
        let single_mv = self.mb_info[mb_x + mb_y * self.mb_w].mb_type != VPMBType::InterFourMV;
        let mut iidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        let mc_buf = self.mc_buf.get_data_mut().unwrap();

        let dst = &mut dframe.data[0..];
        let ystride = dframe.stride[0];
        let mut yoff = dframe.offset[0] + mb_x * 16 + mb_y * 16 * ystride;
        if single_mv {
            if self.dstate.version == 0 {
                mc_block16x16(dst, yoff, ystride, mb_x * 16, mb_y * 16,
                              self.mvs[iidx].x * 2, self.mvs[iidx].y * 2, refframe.clone(), 0, mc_buf);
            } else {
                mc_block16x16_bilin(dst, yoff, ystride, mb_x * 16, mb_y * 16,
                              self.mvs[iidx].x * 2, self.mvs[iidx].y * 2, refframe.clone(), 0, mc_buf);
            }
        } else {
            for y in 0..4 {
                for x in 0..4 {
                    if self.dstate.version == 0 {
                        mc_block4x4(dst, yoff + x * 4, ystride, mb_x * 16 + x * 4, mb_y * 16 + y * 4,
                                    self.mvs[iidx + x].x * 2, self.mvs[iidx + x].y * 2, refframe.clone(), 0, mc_buf);
                    } else {
                        mc_block4x4_bilin(dst, yoff + x * 4, ystride, mb_x * 16 + x * 4, mb_y * 16 + y * 4,
                                          self.mvs[iidx + x].x * 2, self.mvs[iidx + x].y * 2, refframe.clone(), 0, mc_buf);
                    }
                }
                yoff += 4 * ystride;
                iidx += self.mv_stride;
            }
        }

        let mut iidx = mb_x * 4 + mb_y * 4 * self.mv_stride;
        let mut uoff = dframe.offset[1] + mb_x * 8 + mb_y * 8 * dframe.stride[1];
        let ustride = dframe.stride[1];
        let mut voff = dframe.offset[2] + mb_x * 8 + mb_y * 8 * dframe.stride[2];
        let vstride = dframe.stride[2];
        if single_mv {
            let mut chroma_mv = self.mvs[iidx];

            if self.dstate.version == 0 {
                mc_block8x8(dst, uoff, ustride, mb_x * 8, mb_y * 8, chroma_mv.x, chroma_mv.y, refframe.clone(), 1, mc_buf);
                mc_block8x8(dst, voff, vstride, mb_x * 8, mb_y * 8, chroma_mv.x, chroma_mv.y, refframe,         2, mc_buf);
            } else {
                if self.dstate.version == 3 {
                    chroma_mv.x &= !7;
                    chroma_mv.y &= !7;
                }
                mc_block8x8_bilin(dst, uoff, ustride, mb_x * 8, mb_y * 8, chroma_mv.x, chroma_mv.y, refframe.clone(), 1, mc_buf);
                mc_block8x8_bilin(dst, voff, vstride, mb_x * 8, mb_y * 8, chroma_mv.x, chroma_mv.y, refframe,         2, mc_buf);
            }
        } else {
            for y in 0..2 {
                for x in 0..2 {
                    let mut chroma_mv = self.mvs[iidx + x * 2] + self.mvs[iidx + x * 2 + 1]
                                       + self.mvs[iidx + x * 2 + self.mv_stride]
                                       + self.mvs[iidx + x * 2 + self.mv_stride + 1];
                    if chroma_mv.x < 0 {
                        chroma_mv.x += 1;
                    } else {
                        chroma_mv.x += 2;
                    }
                    if chroma_mv.y < 0 {
                        chroma_mv.y += 1;
                    } else {
                        chroma_mv.y += 2;
                    }
                    chroma_mv.x >>= 2;
                    chroma_mv.y >>= 2;

                    if self.dstate.version == 3 {
                        chroma_mv.x &= !7;
                        chroma_mv.y &= !7;
                    }

                    if self.dstate.version == 0 {
                        mc_block4x4(dst, uoff + x * 4, ustride, mb_x * 8 + x * 4, mb_y * 8 + y * 4,
                                    chroma_mv.x, chroma_mv.y, refframe.clone(), 1, mc_buf);
                        mc_block4x4(dst, voff + x * 4, vstride, mb_x * 8 + x * 4, mb_y * 8 + y * 4,
                                    chroma_mv.x, chroma_mv.y, refframe.clone(), 2, mc_buf);
                    } else {
                        mc_block4x4_bilin(dst, uoff + x * 4, ustride, mb_x * 8 + x * 4, mb_y * 8 + y * 4,
                                          chroma_mv.x, chroma_mv.y, refframe.clone(), 1,  mc_buf);
                        mc_block4x4_bilin(dst, voff + x * 4, vstride, mb_x * 8 + x * 4, mb_y * 8 + y * 4,
                                          chroma_mv.x, chroma_mv.y, refframe.clone(), 2, mc_buf);
                    }
                }
                uoff += ustride * 4;
                voff += vstride * 4;
                iidx += 2 * self.mv_stride;
            }
        }
        if !mb_coeff_skip {
            self.add_residue(dframe, mb_x, mb_y, true);
        }
    }
    fn loop_filter_mb(&mut self, dframe: &mut NASimpleVideoFrame<u8>, mb_x: usize, mb_y: usize, loop_str: u8, filter_inner: bool) {
        const HIGH_EDGE_VAR_THR: [[u8; 64]; 2] = [
          [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
            1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
            3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
          ], [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
          ]];

        let inner_thr = if self.dstate.loop_sharpness == 0 {
                i16::from(loop_str)
            } else {
                let bound1 = i16::from(9 - self.dstate.loop_sharpness);
                let shift = (self.dstate.loop_sharpness + 3) >> 2;
                (i16::from(loop_str) >> shift).min(bound1).max(1)
            };
        let blk_thr  = i16::from(loop_str) * 2 + inner_thr;
        let edge_thr = blk_thr + 4;
        let hev_thr     = i16::from(HIGH_EDGE_VAR_THR[if self.dstate.is_intra { 1 } else { 0 }][loop_str as usize]);

        let ystride = dframe.stride[0];
        let ustride = dframe.stride[1];
        let vstride = dframe.stride[2];
        let ypos = dframe.offset[0] + mb_x * 16 + mb_y * 16 * ystride;
        let upos = dframe.offset[1] + mb_x *  8 + mb_y *  8 * ustride;
        let vpos = dframe.offset[2] + mb_x *  8 + mb_y *  8 * vstride;

        let (loop_edge, loop_inner) = if self.dstate.lf_simple {
                (simple_loop_filter as LoopFilterFunc, simple_loop_filter as LoopFilterFunc)
            } else {
                (normal_loop_filter_edge as LoopFilterFunc, normal_loop_filter_inner as LoopFilterFunc)
            };

        if mb_x > 0 {
            loop_edge(dframe.data, ypos, 1, ystride, 16, edge_thr, inner_thr, hev_thr);
            if !self.dstate.lf_simple {
                loop_edge(dframe.data, upos, 1, ustride,  8, edge_thr, inner_thr, hev_thr);
                loop_edge(dframe.data, vpos, 1, vstride,  8, edge_thr, inner_thr, hev_thr);
            }
        }
        if filter_inner {
            for x in 1..4 {
                loop_inner(dframe.data, ypos + x * 4, 1, ystride, 16, blk_thr, inner_thr, hev_thr);
            }
            if !self.dstate.lf_simple {
                loop_inner(dframe.data, upos + 4, 1, ustride, 8, blk_thr, inner_thr, hev_thr);
                loop_inner(dframe.data, vpos + 4, 1, vstride, 8, blk_thr, inner_thr, hev_thr);
            }
        }

        if mb_y > 0 {
            loop_edge(dframe.data, ypos, ystride, 1, 16, edge_thr, inner_thr, hev_thr);
            if !self.dstate.lf_simple {
                loop_edge(dframe.data, upos, ustride, 1,  8, edge_thr, inner_thr, hev_thr);
                loop_edge(dframe.data, vpos, vstride, 1,  8, edge_thr, inner_thr, hev_thr);
            }
        }
        if filter_inner {
            for y in 1..4 {
                loop_inner(dframe.data, ypos + y * 4 * ystride, ystride, 1, 16, blk_thr, inner_thr, hev_thr);
            }
            if !self.dstate.lf_simple {
                loop_inner(dframe.data, upos + 4 * ustride, ustride, 1, 8, blk_thr, inner_thr, hev_thr);
                loop_inner(dframe.data, vpos + 4 * vstride, vstride, 1, 8, blk_thr, inner_thr, hev_thr);
            }
        }
    }
}

impl NADecoder for VP8Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = YUV420_FORMAT;
            let myvinfo = NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, fmt);
            let myinfo = NACodecTypeInfo::Video(myvinfo);
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            supp.pool_u8.set_dec_bufs(5);
            supp.pool_u8.prealloc_video(NAVideoInfo::new(myvinfo.get_width(), myvinfo.get_height(), false, vinfo.get_format()), 4)?;
            self.set_dimensions(myvinfo.get_width(), myvinfo.get_height());
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 4);

        let frame_tag = read_u24le(src.as_slice())?;
        self.dstate.is_intra    = (frame_tag & 1) == 0;
        self.dstate.version     = ((frame_tag >> 1) & 7) as u8;
        validate!(self.dstate.version <= 3);
        let _show_frame          = ((frame_tag >> 4) & 1) != 0;
        let part1_off = if self.dstate.is_intra { 10 } else { 3 };
        let part2_off           = (frame_tag >> 5) as usize;
        validate!(src.len() >= part2_off && part2_off > part1_off);

        if self.dstate.is_intra {
            validate!(src.len() > 10);
            let marker                  = read_u24be(&src[3..6])?;
            validate!(marker == 0x9D012A);
            let width_                  = read_u16le(&src[6..8])?;
            let height_                 = read_u16le(&src[8..10])?;
            let width  = ((width_  + 1) & 0x3FFE) as usize;
            let height = ((height_ + 1) & 0x3FFE) as usize;
//            let hscale = width_  >> 14;
//            let vscale = height_ >> 14;

            validate!((width > 0) && (height > 0));
            self.set_dimensions(width, height);

            self.dstate.reset();
        } else {
            if !self.shuf.has_refs() {
                return Err(DecoderError::MissingReference);
            }
        }

        let mut bc = BoolCoder::new(&src[part1_off..][..part2_off])?;

        if self.dstate.is_intra {
            let _color_space            = bc.read_bool();
            let _clamping_type          = bc.read_bool();
        }

        self.dstate.segmentation        = bc.read_bool();
        if self.dstate.segmentation {
            self.update_segmentation(&mut bc)?;
        } else {
            self.dstate.update_seg_map = false;
            self.dstate.force_quant     = None;
            self.dstate.force_loop_str  = None;
        }

        self.dstate.lf_simple           = bc.read_bool();
        self.dstate.loop_filter_level   = bc.read_bits(6) as u8;
        self.dstate.loop_sharpness      = bc.read_bits(3) as u8;

        self.mb_lf_adjustments(&mut bc)?;

        let num_partitions              = 1 << bc.read_bits(2);

        self.quant_indices(&mut bc)?;

        let (keep_probs, update_last, update_gf, update_ar) = if self.dstate.is_intra {
                let refresh_entropy_probs   = bc.read_bool();
                (refresh_entropy_probs, true, 4, 4)
            } else {
                let refresh_golden_frame    = bc.read_bool();
                let refresh_alternate_frame = bc.read_bool();
                let copy_to_golden = if !refresh_golden_frame {
                                              bc.read_bits(2)
                    } else { 4 };
                validate!(copy_to_golden != 3);
                let copy_to_altref = if !refresh_alternate_frame {
                                              bc.read_bits(2)
                    } else { 4 };
                validate!(copy_to_altref != 3);
                self.dstate.sign_bias[0]    = bc.read_bool();
                self.dstate.sign_bias[1]    = bc.read_bool();
                let refresh_entropy_probs   = bc.read_bool();
                let refresh_last            = bc.read_bool();
                (refresh_entropy_probs, refresh_last, copy_to_golden, copy_to_altref)
            };

        if !keep_probs {
            self.dstate.save(&mut self.tmp_probs);
        }

        self.read_dct_coef_prob_upd(&mut bc)?;

        let mb_no_coeff_skip            = bc.read_bool();
        let prob_skip_false             = bc.read_byte();

        if !self.dstate.is_intra {
            self.dstate.prob_intra_pred             = bc.read_byte();
            self.dstate.prob_last_pred              = bc.read_byte();
            self.dstate.prob_gold_pred              = bc.read_byte();
            if bc.read_bool() {
                for i in 0..4 {
                    self.dstate.kf_ymode_prob[i]    = bc.read_byte();
                }
            }
            if bc.read_bool() {
                for i in 0..3 {
                    self.dstate.kf_uvmode_prob[i]   = bc.read_byte();
                }
            }
            self.read_mv_prob_upd(&mut bc)?;
        }

        let mut data_start = part1_off + part2_off + (num_partitions - 1) * 3;
        let mut part_offs = [0; 8];
        validate!(data_start <= src.len());
        let mut size = src.len() - data_start;
        for i in 0..num_partitions - 1 {
            let len = read_u24le(&src[part1_off + part2_off + i * 3..][..3])? as usize;
            validate!(size >= len);
            part_offs[i] = data_start;
            data_start  += len;
            size        -= len;
        }
        part_offs[num_partitions - 1] = data_start;
        for start in part_offs[num_partitions..].iter_mut() {
            *start = data_start;
        }
        let mut bc_src = Vec::new();
        for &off in part_offs.iter() {
            bc_src.push(BoolCoder::new(&src[off..]).unwrap());
        }

        let vinfo = NAVideoInfo::new(self.width, self.height, false, YUV420_FORMAT);
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

        let mut mb_idx = 0;
        self.pcache.reset();
        let mut rframe = VP8Ref::Last;
        let loop_filter = self.dstate.version != 3 && self.dstate.loop_filter_level > 0;
        for mb_y in 0..self.mb_h {
            let bc_main = &mut bc_src[mb_y & (num_partitions - 1)];
            for mb_x in 0..self.mb_w {
                if self.dstate.update_seg_map {
                    self.decode_mb_features(&mut bc, mb_idx)?;
                }
                if self.dstate.segmentation {
                    self.set_cur_segment(mb_idx);
                }
                let mb_coeff_skip = if mb_no_coeff_skip {
                                        bc.read_prob(prob_skip_false)
                    } else { false };
                self.dstate.has_y2 = true;
                if self.dstate.is_intra {
                    let ymode           = bc.read_tree(KF_Y_MODE_TREE, KF_Y_MODE_TREE_PROBS);
                    if ymode == PredMode::BPred {
                        self.dstate.has_y2 = false;
                        let mut iidx = mb_x * 4 + mb_y * 4 * self.ymode_stride;
                        for y in 0..4 {
                            for x in 0..4 {
                                let top_mode = if (y > 0) || (mb_y > 0) {
                                        self.ymodes[iidx + x - self.ymode_stride]
                                    } else {
                                        PredMode::DCPred
                                    };
                                let left_mode = if (x > 0) || (mb_x > 0) {
                                        self.ymodes[iidx + x - 1]
                                    } else {
                                        PredMode::DCPred
                                    };
                                let top_idx  = top_mode.to_b_index();
                                let left_idx = left_mode.to_b_index();
                                let bmode = bc.read_tree(B_MODE_TREE, &KF_B_MODE_TREE_PROBS[top_idx][left_idx]);
                                self.ymodes[iidx + x] = bmode;
                            }
                            iidx += self.ymode_stride;
                        }
                    } else {
                        self.fill_ymode(mb_x, mb_y, ymode);
                    }
                    let uvmode          = bc.read_tree(UV_MODE_TREE, KF_UV_MODE_TREE_PROBS);
                    self.mb_info[mb_idx].mb_type    = VPMBType::Intra;
                    self.mb_info[mb_idx].ymode      = ymode;
                    self.mb_info[mb_idx].uvmode     = uvmode;
                    self.mb_info[mb_idx].rframe     = VP8Ref::Intra;
                } else if !bc.read_prob(self.dstate.prob_intra_pred) {
                    let ymode           = bc.read_tree(Y_MODE_TREE, &self.dstate.kf_ymode_prob);
                    if ymode == PredMode::BPred {
                        self.dstate.has_y2 = false;
                        let mut iidx = mb_x * 4 + mb_y * 4 * self.ymode_stride;
                        for _y in 0..4 {
                            for x in 0..4 {
                                let bmode = bc.read_tree(B_MODE_TREE, B_MODE_TREE_PROBS);
                                self.ymodes[iidx + x] = bmode;
                            }
                            iidx += self.ymode_stride;
                        }
                    } else {
                        self.fill_ymode(mb_x, mb_y, PredMode::Inter);
                    }
                    let uvmode          = bc.read_tree(UV_MODE_TREE, &self.dstate.kf_uvmode_prob);
                    self.mb_info[mb_idx].mb_type    = VPMBType::Intra;
                    self.mb_info[mb_idx].ymode      = ymode;
                    self.mb_info[mb_idx].uvmode     = uvmode;
                    self.mb_info[mb_idx].rframe     = VP8Ref::Intra;
                    self.fill_mv(mb_x, mb_y, ZERO_MV);
                } else {
                    rframe = if !bc.read_prob(self.dstate.prob_last_pred) {
                            VP8Ref::Last
                        } else if !bc.read_prob(self.dstate.prob_gold_pred) {
                            VP8Ref::Golden
                        } else {
                            VP8Ref::AltRef
                        };

                    let frm_sign = self.get_frame_sign(rframe);
                    let (mvprobs, nearest_mv, near_mv, pred_mv) = self.find_mv_pred(mb_x, mb_y, frm_sign);
                    let mbtype          = bc.read_tree(MV_REF_TREE, &mvprobs);

                    match mbtype {
                        VPMBType::InterNearest => {
                            self.fill_mv(mb_x, mb_y, nearest_mv);
                        },
                        VPMBType::InterNear => {
                            self.fill_mv(mb_x, mb_y, near_mv);
                        },
                        VPMBType::InterNoMV => {
                            self.fill_mv(mb_x, mb_y, ZERO_MV);
                        },
                        VPMBType::InterMV => {
                            let dmy     = decode_mv_component(&mut bc, &self.dstate.mv_probs[0]);
                            let dmx     = decode_mv_component(&mut bc, &self.dstate.mv_probs[1]);
                            let new_mv = pred_mv + MV{ x: dmx, y: dmy };
                            self.fill_mv(mb_x, mb_y, new_mv);
                        },
                        VPMBType::InterFourMV => {
                            self.dstate.has_y2 = false;
                            self.do_split_mv(&mut bc, mb_x, mb_y, pred_mv)?;
                        },
                        _ => unreachable!(),
                    };

                    self.fill_ymode(mb_x, mb_y, PredMode::Inter);
                    self.mb_info[mb_idx].mb_type    = mbtype;
                    self.mb_info[mb_idx].ymode      = PredMode::Inter;
                    self.mb_info[mb_idx].uvmode     = PredMode::Inter;
                    self.mb_info[mb_idx].rframe     = rframe;
                }
                let has_coeffs = if !mb_coeff_skip {
                        self.decode_residue(bc_main, mb_x)
                    } else {
                        let y2_left = self.pcache.y2_pred_left;
                        let y2_top  = self.pcache.y2_pred.data[self.pcache.y2_pred.xpos + mb_x - self.pcache.y2_pred.stride];
                        self.pcache.reset_left();
                        if !self.dstate.has_y2 {
                            self.pcache.y2_pred_left = y2_left;
                            self.pcache.y2_pred.data[self.pcache.y2_pred.xpos + mb_x] = y2_top;
                        }
                        false
                    };
                match self.mb_info[mb_idx].mb_type {
                    VPMBType::Intra => {
                        self.recon_intra_mb(&mut dframe, mb_x, mb_y, mb_coeff_skip)?;
                    },
                    _ => {
                        self.recon_inter_mb(&mut dframe, mb_x, mb_y, mb_coeff_skip, rframe);
                    },
                }
                if loop_filter {
                    if let Some(loop_str) = self.dstate.force_loop_str {
                        self.mb_info[mb_idx].loop_str = loop_str;
                    } else {
                        self.mb_info[mb_idx].loop_str = self.dstate.loop_filter_level;
                    }
                    if self.dstate.lf_delta {
                        let mut loop_str = self.mb_info[mb_idx].loop_str as i8;
                        let idx = match self.mb_info[mb_idx].rframe {
                                VP8Ref::Intra   => 0,
                                VP8Ref::Last    => 1,
                                VP8Ref::Golden  => 2,
                                VP8Ref::AltRef  => 3,
                            };
                        loop_str += self.dstate.lf_frame_delta[idx];
                        let idx = match self.mb_info[mb_idx].mb_type {
                                VPMBType::Intra         => 0,
                                VPMBType::InterNoMV     => 1,
                                VPMBType::InterFourMV   => 3,
                                _                       => 2,
                            };
                        if self.mb_info[mb_idx].mb_type != VPMBType::Intra || self.mb_info[mb_idx].ymode == PredMode::BPred {
                            loop_str += self.dstate.lf_mode_delta[idx];
                        }
                        self.mb_info[mb_idx].loop_str = loop_str.max(0).min(63) as u8;
                    }
                    self.mb_info[mb_idx].inner_filt = has_coeffs || (self.mb_info[mb_idx].ymode == PredMode::BPred) || (self.mb_info[mb_idx].mb_type == VPMBType::InterFourMV);
                }
                mb_idx += 1;
            }
            self.pcache.update_row();
            self.pcache.reset_left();
        }
        if loop_filter {
            let mut mb_idx = 0;
            for mb_y in 0..self.mb_h {
                for mb_x in 0..self.mb_w {
                    let loop_str = self.mb_info[mb_idx].loop_str;
                    if loop_str > 0 {
                        self.loop_filter_mb(&mut dframe, mb_x, mb_y, loop_str, self.mb_info[mb_idx].inner_filt);
                    }
                    mb_idx += 1;
                }
            }
        }

        if !keep_probs {
            self.dstate.restore(&self.tmp_probs);
        }

        match update_ar {
            1 => {
                let last = self.shuf.get_last().unwrap();
                self.shuf.add_altref_frame(last);
            },
            2 => {
                let golden = self.shuf.get_golden().unwrap();
                self.shuf.add_altref_frame(golden);
            },
            _ => {},
        };
        match update_gf {
            4 => self.shuf.add_golden_frame(buf.clone()),
            1 => {
                let last = self.shuf.get_last().unwrap();
                self.shuf.add_golden_frame(last);
            },
            2 => {
                let altref = self.shuf.get_altref().unwrap();
                self.shuf.add_golden_frame(altref);
            },
            _ => {},
        };
        if update_ar == 4 {
            self.shuf.add_altref_frame(buf.clone());
        }
        if update_last {
            self.shuf.add_frame(buf.clone());
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(self.dstate.is_intra);
        frm.set_frame_type(if self.dstate.is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.shuf.clear();
    }
}

impl NAOptionHandler for VP8Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(VP8Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use crate::duck_register_all_demuxers;

    // all samples are from the official VP8 test bitstreams set
    fn test_vp8_core(name: &str, hash: [u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        duck_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        test_decoding("dkivf", "vp8", name, None, &dmx_reg,
                      &dec_reg, ExpectedTestResult::MD5(hash));
    }

    #[test]
    fn test_vp8_01() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-001.ivf",
                      [0xfad12607, 0x4e1bd536, 0x3d43b9d1, 0xcadddb71]);
    }
    #[test]
    fn test_vp8_02() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-002.ivf",
                      [0x182f03dd, 0x264ebac0, 0x4e24c7c9, 0x499d7cdb]);
    }
    #[test]
    fn test_vp8_03() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-003.ivf",
                      [0xe5fe668b, 0x03390002, 0x2c3eb0ba, 0x76a44bd1]);
    }
    #[test]
    fn test_vp8_04() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-004.ivf",
                      [0x95097ce9, 0x808c1d47, 0xe03f99c4, 0x8ad111ec]);
    }
    #[test]
    fn test_vp8_05() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-005.ivf",
                      [0x0f469e4f, 0xd1dea533, 0xe5580688, 0xb2d242ff]);
    }
    /*#[test]
    fn test_vp8_06() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-006.ivf",
                      [0;4]);
    }*/
    #[test]
    fn test_vp8_07() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-007.ivf",
                      [0x92526913, 0xd89b6a9b, 0x00f2d602, 0xdef08bce]);
    }
    #[test]
    fn test_vp8_08() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-008.ivf",
                      [0x1676d1eb, 0x19bd175e, 0xc5bb10f5, 0xd49f24f1]);
    }
    #[test]
    fn test_vp8_09() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-009.ivf",
                      [0x19201a2d, 0x535bd82f, 0x41c1a565, 0x8def5379]);
    }
    #[test]
    fn test_vp8_10() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-010.ivf",
                      [0x61d05919, 0xa9883d9f, 0x215eb3f2, 0xdb63eb13]);
    }
    #[test]
    fn test_vp8_11() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-011.ivf",
                      [0x1a0afe5e, 0x70512a03, 0x323a8f11, 0x76bcf022]);
    }
    #[test]
    fn test_vp8_12() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-012.ivf",
                      [0x4ea997c8, 0x0dc2087e, 0x6deec81f, 0x1ecf6668]);
    }
    #[test]
    fn test_vp8_13() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-013.ivf",
                      [0x93169305, 0xd3054327, 0xbe3cc074, 0xf0773a75]);
    }
    /*#[test]
    fn test_vp8_14() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-014.ivf",
                      [0;4]);
    }*/
    #[test]
    fn test_vp8_15() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-015.ivf",
                      [0x23b9cc58, 0x2e344726, 0xe76cda09, 0x2b416bcf]);
    }
    #[test]
    fn test_vp8_16() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-016.ivf",
                      [0x55e889d2, 0x2f99718c, 0xf6936d55, 0xf8ade12b]);
    }
    #[test]
    fn test_vp8_17() {
        test_vp8_core("assets/Duck/VP8/vp80-00-comprehensive-017.ivf",
                      [0x95a68ffb, 0x228d1d8c, 0x6ee54f16, 0xa10fb9eb]);
    }
}

const DC_QUANTS: [i16; 128] = [
      4,   5,   6,   7,   8,   9,  10,  10,
     11,  12,  13,  14,  15,  16,  17,  17,
     18,  19,  20,  20,  21,  21,  22,  22,
     23,  23,  24,  25,  25,  26,  27,  28,
     29,  30,  31,  32,  33,  34,  35,  36,
     37,  37,  38,  39,  40,  41,  42,  43,
     44,  45,  46,  46,  47,  48,  49,  50,
     51,  52,  53,  54,  55,  56,  57,  58,
     59,  60,  61,  62,  63,  64,  65,  66,
     67,  68,  69,  70,  71,  72,  73,  74,
     75,  76,  76,  77,  78,  79,  80,  81,
     82,  83,  84,  85,  86,  87,  88,  89,
     91,  93,  95,  96,  98, 100, 101, 102,
    104, 106, 108, 110, 112, 114, 116, 118,
    122, 124, 126, 128, 130, 132, 134, 136,
    138, 140, 143, 145, 148, 151, 154, 157
];

const AC_QUANTS: [i16; 128] = [
      4,   5,   6,   7,   8,   9,  10,  11,
     12,  13,  14,  15,  16,  17,  18,  19,
     20,  21,  22,  23,  24,  25,  26,  27,
     28,  29,  30,  31,  32,  33,  34,  35,
     36,  37,  38,  39,  40,  41,  42,  43,
     44,  45,  46,  47,  48,  49,  50,  51,
     52,  53,  54,  55,  56,  57,  58,  60,
     62,  64,  66,  68,  70,  72,  74,  76,
     78,  80,  82,  84,  86,  88,  90,  92,
     94,  96,  98, 100, 102, 104, 106, 108,
    110, 112, 114, 116, 119, 122, 125, 128,
    131, 134, 137, 140, 143, 146, 149, 152,
    155, 158, 161, 164, 167, 170, 173, 177,
    181, 185, 189, 193, 197, 201, 205, 209,
    213, 217, 221, 225, 229, 234, 239, 245,
    249, 254, 259, 264, 269, 274, 279, 284
];
