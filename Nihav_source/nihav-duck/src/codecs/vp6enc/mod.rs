use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use super::vp6data::*;
use super::vpcommon::*;

mod coder;
use coder::*;
mod dsp;
use dsp::MVSearchMode;
mod huff;
use huff::*;
mod mb;
use mb::*;
mod models;
use models::*;
mod ratectl;
use ratectl::*;
mod rdo;

const VERSION_VP61: u8 = VERSION_VP60 + 1;

enum VP6Writer<'a, 'b> {
    BoolCoder(BoolEncoder<'a, 'b>),
    Huffman(HuffEncoder<'a, 'b>),
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

    last_dc:    [[i16; 4]; 3],
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

        self.last_dc = [[0; 4]; 3];
        self.last_dc[0][1] = 0x80;
        self.last_dc[0][2] = 0x80;
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
    fn predict_dc(&mut self, mb_type: VPMBType, blk_no: usize, coeffs: &mut [i16; 64], fwd: bool) {
        let is_luma = blk_no < 4;
        let (plane, dcs) = match blk_no {
                4 => (1, &mut self.dc_u),
                5 => (2, &mut self.dc_v),
                _ => (0, &mut self.dc_y),
             };
        let (dc_ref, dc_idx) = if is_luma {
                (&mut self.ref_y, self.y_idx + (blk_no & 1))
            } else {
                (&mut self.ref_c, self.c_idx)
            };
        let ref_id = mb_type.get_ref_id();
        let mut dc_pred = 0;
        let mut count = 0;
        let has_left_blk = is_luma && ((blk_no & 1) == 1);
        if has_left_blk || self.ref_left == ref_id {
            dc_pred += match blk_no {
                    0 | 1 => self.ldc_y[0],
                    2 | 3 => self.ldc_y[1],
                    4     => self.ldc_u,
                    _     => self.ldc_v,
                };
            count += 1;
        }
        if dc_ref[dc_idx] == ref_id {
            dc_pred += dcs[dc_idx];
            count += 1;
        }
        if count == 0 {
            dc_pred = self.last_dc[ref_id as usize][plane];
        } else if count == 2 {
            dc_pred /= 2;
        }
        if !fwd {
            coeffs[0] += dc_pred;
        }

        let dc = coeffs[0];
        if blk_no != 4 { // update top block reference only for the second chroma component
            dc_ref[dc_idx] = ref_id;
        }
        match blk_no {
            0 | 1 => {
                self.ldc_y[0] = dc;
            },
            2 | 3 => {
                self.ldc_y[1] = dc;
            },
            4 => {
                self.ldc_u = dc;
            },
            _ => {
                self.ldc_v = dc;
                self.ref_left = ref_id;
            },
        };
        dcs[dc_idx] = dc;

        self.last_dc[ref_id as usize][plane] = dc;
        if fwd {
            coeffs[0] -= dc_pred;
        }
    }
}

trait ZeroBlock {
    fn no_dc(&self) -> bool;
    fn no_ac(&self) -> bool;
}

impl ZeroBlock for [i16; 64] {
    fn no_dc(&self) -> bool {
        self[0] == 0
    }
    fn no_ac(&self) -> bool {
        for &el in self[1..].iter() {
            if el != 0 {
                return false;
            }
        }
        true
    }
}

struct VP6Encoder {
    stream:     Option<NAStreamRef>,
    pkt:        Option<NAPacket>,
    key_int:    u8,
    frmcount:   u8,
    mb_w:       usize,
    mb_h:       usize,
    dc_pred:    VP56DCPred,
    top_ctx:    [Vec<bool>; 4],
    mc_buf:     NAVideoBufferRef<u8>,
    fenc:       FrameEncoder,
    ratectl:    RateControl,

    flipped:    bool,
    huffman:    bool,

    version:    u8,
    profile:    u8,

    models:     VP56Models,
    stats:      VP56ModelsStat,
    pmodels:    VP56Models,

    last_frame: NABufferType,
    gold_frame: NABufferType,
    last_gold:  bool,
    me_mode:    MVSearchMode,
    me_range:   i16,

    force_q:    Option<usize>,
    fast:       bool,
}

impl VP6Encoder {
    fn new(flipped: bool) -> Self {
        let vt = alloc_video_buffer(NAVideoInfo::new(24, 24, false, VP_YUVA420_FORMAT), 4).unwrap();
        let mc_buf = vt.get_vbuf().unwrap();
        Self {
            stream:     None,
            pkt:        None,
            key_int:    10,
            frmcount:   0,
            mb_w:       0,
            mb_h:       0,
            dc_pred:    VP56DCPred::new(),
            top_ctx:    [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            fenc:       FrameEncoder::new(),
            ratectl:    RateControl::new(),
            mc_buf,

            flipped,
            huffman:    false,

            version:    VERSION_VP60,
            profile:    VP6_SIMPLE_PROFILE,

            models:     VP56Models::new(),
            pmodels:    VP56Models::new(),
            stats:      VP56ModelsStat::new(),

            last_frame: NABufferType::None,
            gold_frame: NABufferType::None,
            last_gold:  false,
            me_mode:    MVSearchMode::default(),
            me_range:   16,

            force_q:    None,
            fast:       false,
        }
    }
    fn decide_encoding(&mut self) -> bool {
        false
    }
    fn estimate_blocks(&mut self, is_intra: bool) {
        for top_ctx in self.top_ctx.iter_mut() {
            for el in top_ctx.iter_mut() {
                *el = false;
            }
        }
        let mut last_mbt = VPMBType::InterNoMV;
        let mut mb_idx = 0;
        for _mb_y in 0..self.mb_h {
            let mut left_dc = [false; 4];
            for mb_x in 0..self.mb_w {
                let mb_type = self.fenc.mb_types[mb_idx];
                if !is_intra {
                    estimate_mb_type(mb_type, last_mbt, ((self.fenc.num_mv[mb_idx] + 1) % 3) as usize, &mut self.stats);
                    last_mbt = mb_type;
                    match mb_type {
                        VPMBType::InterMV | VPMBType::GoldenMV => {
                            estimate_mv(self.fenc.coded_mv[mb_idx][3], &mut self.stats);
                        },
                        VPMBType::InterFourMV => {
                            for (&sub_type, &mv) in self.fenc.fmv_sub[mb_idx].iter().zip(self.fenc.coded_mv[mb_idx].iter()) {
                                if sub_type == VPMBType::InterMV {
                                    estimate_mv(mv, &mut self.stats);
                                }
                            }
                        },
                        _ => {},
                    };
                }
                let mb = self.fenc.get_mb(mb_idx);
                for i in 0..4 {
                    let cur_idx = mb_x * 2 + (i & 1);
                    let mut dc_mode = 0;
                    if self.top_ctx[0][cur_idx] {
                        dc_mode += 1;
                    }
                    if left_dc[i >> 1] {
                        dc_mode += 1;
                    }
                    self.top_ctx[0][cur_idx] = mb.coeffs[i][0] != 0;
                    left_dc[i >> 1] = mb.coeffs[i][0] != 0;
                    estimate_block(&mb.coeffs[i], dc_mode, &mut self.stats.coeff_models[0], &mut self.stats.vp6models, &self.models.vp6models.zigzag);
                }

                let mut dc_mode = 0;
                if self.top_ctx[1][mb_x] {
                    dc_mode += 1;
                }
                if left_dc[2] {
                    dc_mode += 1;
                }
                self.top_ctx[1][mb_x] = mb.coeffs[4][0] != 0;
                left_dc[2] = mb.coeffs[4][0] != 0;
                estimate_block(&mb.coeffs[4], dc_mode, &mut self.stats.coeff_models[1], &mut self.stats.vp6models, &self.models.vp6models.zigzag);

                let mut dc_mode = 0;
                if self.top_ctx[2][mb_x] {
                    dc_mode += 1;
                }
                if left_dc[3] {
                    dc_mode += 1;
                }
                self.top_ctx[2][mb_x] = mb.coeffs[5][0] != 0;
                left_dc[3] = mb.coeffs[5][0] != 0;
                estimate_block(&mb.coeffs[5], dc_mode, &mut self.stats.coeff_models[1], &mut self.stats.vp6models, &self.models.vp6models.zigzag);

                mb_idx += 1;
            }
        }
    }
    fn prepare_huff_models(&mut self) {
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
    fn determine_coeff_runs_luma(&self, hstate: &mut HuffState, mb_pos: usize, blk: usize) {
        let mb = self.fenc.get_mb(mb_pos);

        if !hstate.dc_zr_coded[0] {
            if mb.coeffs[blk].no_dc() {
                hstate.dc_zero_run[0] = 0;
                let mut blk_no = (blk + 1) & 3;
                let mut mb_no = mb_pos + ((blk + 1) >> 2);
                let mut cmb = mb;
                let mut last_mb_no = mb_pos;
                while (hstate.dc_zero_run[0] < MAX_EOB_RUN) && (mb_no < self.mb_w * self.mb_h) {
                    if mb_no != last_mb_no {
                        cmb = self.fenc.get_mb(mb_no);
                        last_mb_no = mb_no;
                    }
                    if !cmb.coeffs[blk_no].no_dc() {
                        break;
                    }
                    hstate.dc_zero_run[0] += 1;
                    blk_no += 1;
                    if blk_no == 4 {
                        blk_no = 0;
                        mb_no += 1;
                    }
                }
            }
        }
        if !hstate.ac_zr_coded[0] {
            if mb.coeffs[blk].no_ac() {
                hstate.ac_zero_run[0] = 0;
                let mut blk_no = (blk + 1) & 3;
                let mut mb_no = mb_pos + ((blk + 1) >> 2);
                let mut cmb = mb;
                let mut last_mb_no = mb_pos;
                while (hstate.ac_zero_run[0] < MAX_EOB_RUN) && (mb_no < self.mb_w * self.mb_h) {
                    if mb_no != last_mb_no {
                        cmb = self.fenc.get_mb(mb_no);
                        last_mb_no = mb_no;
                    }
                    if !cmb.coeffs[blk_no].no_ac() {
                        break;
                    }
                    hstate.ac_zero_run[0] += 1;
                    blk_no += 1;
                    if blk_no == 4 {
                        blk_no = 0;
                        mb_no += 1;
                    }
                }
            }
        }
    }
    fn determine_coeff_runs_chroma(&self, hstate: &mut HuffState, mb_pos: usize, plane: usize) {
        let mb = self.fenc.get_mb(mb_pos);
        let blk = plane + 3;

        if !hstate.dc_zr_coded[1] {
            if mb.coeffs[blk].no_dc() {
                hstate.dc_zero_run[1] = 0;
                let mut blk_no = if blk == 4 { 5 } else { 4 };
                let mut mb_no = mb_pos + if blk == 4 { 0 } else { 1 };
                while (hstate.dc_zero_run[1] < MAX_EOB_RUN) && (mb_no < self.mb_w * self.mb_h) {
                    let mb = self.fenc.get_mb(mb_no);
                    if !mb.coeffs[blk_no].no_dc() {
                        break;
                    }
                    hstate.dc_zero_run[1] += 1;
                    blk_no += 1;
                    if blk_no == 6 {
                        blk_no = 4;
                        mb_no += 1;
                    }
                }
            }
        }
        if !hstate.ac_zr_coded[1] {
            if mb.coeffs[blk].no_ac() {
                hstate.ac_zero_run[1] = 0;
                let mut blk_no = if blk == 4 { 5 } else { 4 };
                let mut mb_no = mb_pos + if blk == 4 { 0 } else { 1 };
                while (hstate.ac_zero_run[1] < MAX_EOB_RUN) && (mb_no < self.mb_w * self.mb_h) {
                    let mb = self.fenc.get_mb(mb_no);
                    if !mb.coeffs[blk_no].no_ac() {
                        break;
                    }
                    hstate.ac_zero_run[1] += 1;
                    blk_no += 1;
                    if blk_no == 6 {
                        blk_no = 4;
                        mb_no += 1;
                    }
                }
            }
        }
    }
    fn encode_intra(&mut self, bw: &mut ByteWriter, quant: usize) -> EncoderResult<bool> {
        self.models.reset(false);
        self.models.reset_mbtype_models();
        self.stats.reset();

        self.pmodels.reset(false);
        self.pmodels.reset_mbtype_models();

        let multistream = self.huffman || self.version != VERSION_VP60;

        self.fenc.prepare_intra_blocks();
        self.fenc.apply_dc_prediction(&mut self.dc_pred);
        self.estimate_blocks(true);
        self.stats.generate(&mut self.models, true);

        // header
        bw.write_byte(((quant as u8) << 1) | (multistream as u8))?;
        bw.write_byte((self.version << 3) | (self.profile << 1))?;
        bw.write_u16be(0)?; // part 2 offset placeholder

        let mut bc = BoolEncoder::new(bw);

        bc.put_bits(self.mb_h as u32, 8)?;
        bc.put_bits(self.mb_w as u32, 8)?;
        bc.put_bits(self.mb_h as u32, 8)?; // display MB height
        bc.put_bits(self.mb_w as u32, 8)?; // display MB width
        bc.put_bits(0, 2)?; // scaline mode
        // todo other advanced profile bits
        bc.put_bits(self.huffman as u32, 1)?; // Huffman mode

        encode_coeff_models(&mut bc, &mut self.models, &self.pmodels, true, false)?;
        self.pmodels = self.models.clone();

        if multistream || (self.profile == VP6_SIMPLE_PROFILE) {
            bc.flush()?;

            // patch coefficient offset
            let offset = bw.tell();
            if offset >= 65535 {
                return Err(EncoderError::Bug);
            }
            bw.seek(SeekFrom::Start(2))?;
            bw.write_u16be(offset as u16)?;
            bw.seek(SeekFrom::End(0))?;

            bc = BoolEncoder::new(bw);
        }
        let writer = if !self.huffman {
                VP6Writer::BoolCoder(bc)
            } else {
                VP6Writer::Huffman(HuffEncoder::new(bw))
            };
        self.encode_coeffs(writer)?;
        Ok(true)
    }
    fn encode_inter(&mut self, bw: &mut ByteWriter, quant: usize) -> EncoderResult<bool> {
        self.stats.reset();

        let multistream = self.huffman || self.version != VERSION_VP60;
        let loop_filter = false;

        self.fenc.prepare_intra_blocks();
        self.fenc.prepare_inter_blocks(false);
        if !self.last_gold {
            self.fenc.prepare_inter_blocks(true);
        }
        let lambda = if self.force_q.is_some() { 1.0 } else { self.ratectl.lambda };
        self.fenc.select_inter_blocks(self.last_frame.get_vbuf().unwrap(), self.mc_buf.clone(), !self.last_gold, lambda);
        // todo implement forced intra
        let (_force_intra, golden_frame) = self.fenc.decide_frame_type();
        self.fenc.apply_dc_prediction(&mut self.dc_pred);
        self.fenc.predict_mvs();

        self.write_inter_frame(bw, quant, multistream, loop_filter, golden_frame)?;

        Ok(golden_frame)
    }
    fn write_inter_frame(&mut self, bw: &mut ByteWriter, quant: usize, multistream: bool, loop_filter: bool, golden_frame: bool) -> EncoderResult<()> {
        self.estimate_blocks(false);

        self.stats.generate(&mut self.models, false);

        // header
        bw.write_byte(0x80 | ((quant as u8) << 1) | (multistream as u8))?;
        bw.write_u16be(0)?; // part 2 offset placeholder

        let mut bc = BoolEncoder::new(bw);

        bc.put_bits(golden_frame as u32, 1)?; // refresh golden frame
        if self.profile == VP6_ADVANCED_PROFILE {
            bc.put_bits(loop_filter as u32, 1)?; // use loop filter
            if loop_filter {
                bc.put_bits(0, 1)?; // loop filter selector
            }
            if self.version == VERSION_VP62 {
                bc.put_bits(0, 1)?; // auto select PM
            }
        }
        // todo other advanced profile bits
        bc.put_bits(self.huffman as u32, 1)?;

        encode_mode_prob_models(&mut bc, &mut self.models, &self.pmodels, &self.stats.mbtype_models)?;
        encode_mv_models(&mut bc, &self.models.mv_models, &self.pmodels.mv_models)?;
        encode_coeff_models(&mut bc, &mut self.models, &self.pmodels, false, false)?;
        self.pmodels = self.models.clone();

        let mut last_mbt = VPMBType::InterNoMV;
        for mb_idx in 0..self.mb_w * self.mb_h {
            let mb_type = self.fenc.mb_types[mb_idx];
            encode_mb_type(&mut bc, self.fenc.mb_types[mb_idx], last_mbt, ((self.fenc.num_mv[mb_idx] + 1) % 3) as usize, &self.models)?;
            last_mbt = mb_type;
            match mb_type {
                VPMBType::InterMV | VPMBType::GoldenMV => {
                    encode_mv(&mut bc, self.fenc.coded_mv[mb_idx][3], &self.models)?;
                },
                VPMBType::InterFourMV => {
                    for &sub_type in self.fenc.fmv_sub[mb_idx].iter() {
                        let id = match sub_type {
                                VPMBType::InterNoMV     => 0,
                                VPMBType::InterMV       => 1,
                                VPMBType::InterNearest  => 2,
                                VPMBType::InterNear     => 3,
                                _ => unreachable!(),
                            };
                        bc.put_bits(id, 2)?;
                    }
                    for (&sub_type, &mv) in self.fenc.fmv_sub[mb_idx].iter().zip(self.fenc.coded_mv[mb_idx].iter()) {
                        if sub_type == VPMBType::InterMV {
                            encode_mv(&mut bc, mv, &self.models)?;
                        }
                    }
                },
                _ => {},
            };
        }

        if multistream || (self.profile == VP6_SIMPLE_PROFILE) {
            bc.flush()?;

            // patch coefficient offset
            let offset = bw.tell();
            if offset >= 65535 {
                return Err(EncoderError::Bug);
            }
            bw.seek(SeekFrom::Start(1))?;
            bw.write_u16be(offset as u16)?;
            bw.seek(SeekFrom::End(0))?;

            bc = BoolEncoder::new(bw);
        }
        let writer = if !self.huffman {
                VP6Writer::BoolCoder(bc)
            } else {
                VP6Writer::Huffman(HuffEncoder::new(bw))
            };
        self.encode_coeffs(writer)?;

        Ok(())
    }
    fn encode_inter_fast(&mut self, bw: &mut ByteWriter, quant: usize) -> EncoderResult<bool> {
        self.stats.reset();

        let multistream = self.huffman || self.version != VERSION_VP60;
        let loop_filter = false;

        let last_frm = self.last_frame.get_vbuf().unwrap();
        let gold_frm = if !self.last_gold {
                Some(self.gold_frame.get_vbuf().unwrap())
            } else {
                None
            };
        let lambda = if self.force_q.is_some() { 1.0 } else { self.ratectl.lambda };
        self.fenc.select_inter_blocks_fast(last_frm, gold_frm, self.mc_buf.clone(), lambda);
        let golden_frame = false;
        self.fenc.apply_dc_prediction(&mut self.dc_pred);
        self.fenc.predict_mvs();
        self.estimate_blocks(false);

        self.write_inter_frame(bw, quant, multistream, loop_filter, golden_frame)?;

        Ok(false)
    }
    fn encode_coeffs(&mut self, mut writer: VP6Writer) -> EncoderResult<()> {
        if self.huffman {
            self.prepare_huff_models();
        }

        let mut hstate = HuffState::new();
        for top_ctx in self.top_ctx.iter_mut() {
            for el in top_ctx.iter_mut() {
                *el = false;
            }
        }
        let mut mb_pos = 0;
        for _mb_y in 0..self.mb_h {
            let mut left_dc = [false; 4];
            for mb_x in 0..self.mb_w {
                let mb = self.fenc.get_mb(mb_pos);
                for i in 0..4 {
                    let cur_idx = mb_x * 2 + (i & 1);
                    let mut dc_mode = 0;
                    if self.top_ctx[0][cur_idx] {
                        dc_mode += 1;
                    }
                    if left_dc[i >> 1] {
                        dc_mode += 1;
                    }
                    self.top_ctx[0][cur_idx] = mb.coeffs[i][0] != 0;
                    left_dc[i >> 1] = mb.coeffs[i][0] != 0;
                    if self.huffman {
                        self.determine_coeff_runs_luma(&mut hstate, mb_pos, i);
                    }
                    match writer {
                        VP6Writer::BoolCoder(ref mut bc) => encode_block(bc, &mb.coeffs[i], dc_mode, &self.models.coeff_models[0], &self.models.vp6models)?,
                        VP6Writer::Huffman(ref mut huff) => encode_block_huff(huff, &self.models.vp6models.zigzag, &mb.coeffs[i], 0, &mut hstate, &self.models.vp6huff)?,
                    };
                }

                for plane in 1..3 {
                    let mut dc_mode = 0;
                    if self.top_ctx[plane][mb_x] {
                        dc_mode += 1;
                    }
                    if left_dc[plane + 1] {
                        dc_mode += 1;
                    }
                    self.top_ctx[plane][mb_x] = mb.coeffs[plane + 3][0] != 0;
                    left_dc[plane + 1] = mb.coeffs[plane + 3][0] != 0;
                    if self.huffman {
                        self.determine_coeff_runs_chroma(&mut hstate, mb_pos, plane);
                    }
                    match writer {
                        VP6Writer::BoolCoder(ref mut bc) => encode_block(bc, &mb.coeffs[plane + 3], dc_mode, &self.models.coeff_models[1], &self.models.vp6models)?,
                        VP6Writer::Huffman(ref mut huff) => encode_block_huff(huff, &self.models.vp6models.zigzag, &mb.coeffs[plane + 3], 1, &mut hstate, &self.models.vp6huff)?,
                    };
                }

                mb_pos += 1;
            }
        }

        match writer {
            VP6Writer::BoolCoder(bc) => bc.flush()?,
            VP6Writer::Huffman(huff) => huff.flush()?,
        };

        Ok(())
    }
}

impl NAEncoder for VP6Encoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, self.flipped, YUV420_FORMAT)),
                    ..Default::default()
                })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let outinfo = NAVideoInfo::new((vinfo.width + 3) & !3, (vinfo.height + 3) & !3, self.flipped, YUV420_FORMAT);
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

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, self.flipped, vinfo.format);
                let info = NACodecInfo::new(if self.flipped { "vp6" } else { "vp6f" }, NACodecTypeInfo::Video(out_info), None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.last_frame = alloc_video_buffer(out_info, 4)?;
                self.gold_frame = alloc_video_buffer(out_info, 4)?;

                self.stream = Some(stream.clone());

                self.mb_w = (vinfo.width  + 15) >> 4;
                self.mb_h = (vinfo.height + 15) >> 4;
                self.fenc.resize(self.mb_w, self.mb_h);
                self.ratectl.init(self.mb_w, self.mb_h, encinfo.bitrate, encinfo.tb_num, encinfo.tb_den);

                self.dc_pred.resize(self.mb_w);
                self.top_ctx = [vec![false; self.mb_w * 2], vec![false; self.mb_w], vec![false; self.mb_w], vec![false; self.mb_w * 2]];

                self.version = VERSION_VP60;
                self.profile = VP6_SIMPLE_PROFILE;

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        if let Some(ref vbuf) = buf.get_vbuf() {
            let mut dbuf = Vec::with_capacity(4);
            let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
            let mut bw   = ByteWriter::new(&mut gw);

            // todo integrate with rate control
            let is_intra = (self.frmcount == 0) || self.decide_encoding();
            let quant = if let Some(q) = self.force_q {
                    q
                } else {
                    self.ratectl.guess_quant(is_intra, self.huffman)
                };

            self.fenc.read_mbs(vbuf);
            self.fenc.set_quant(quant);
            self.fenc.me_mode = self.me_mode;
            self.fenc.me_range = self.me_range;
            let golden_frame = if is_intra {
                    self.encode_intra(&mut bw, quant)?
                } else if !self.fast {
                    self.fenc.estimate_mvs(self.last_frame.get_vbuf().unwrap(), self.mc_buf.clone(), false);
                    if !self.last_gold {
                        self.fenc.estimate_mvs(self.gold_frame.get_vbuf().unwrap(), self.mc_buf.clone(), true);
                    }
                    self.encode_inter(&mut bw, quant)?
                } else {
                    self.encode_inter_fast(&mut bw, quant)?
                };
            self.fenc.reconstruct_frame(&mut self.dc_pred, self.last_frame.get_vbuf().unwrap());
            self.last_gold = golden_frame;
            if golden_frame {
                let mut dfrm = self.gold_frame.get_vbuf().unwrap();
                let src = self.last_frame.get_vbuf().unwrap();

                let dst = dfrm.get_data_mut().unwrap();
                dst.copy_from_slice(src.get_data());
            }

            if self.force_q.is_none() {
                self.ratectl.update(dbuf.len() * 8);
            }

            self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, is_intra, dbuf));
            if self.key_int > 0 {
                self.frmcount += 1;
            }
            if self.frmcount == self.key_int {
                self.frmcount = 0;
            }
            Ok(())
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        let mut npkt = None;
        std::mem::swap(&mut self.pkt, &mut npkt);
        Ok(npkt)
    }
    fn flush(&mut self) -> EncoderResult<()> {
        self.frmcount = 0;
        Ok(())
    }
}

const HUFFMAN_OPTION: &str = "huffman";
const QUANT_OPTION: &str = "quant";
const VERSION_OPTION: &str = "version";
const MV_SEARCH_OPTION: &str = "mv_mode";
const MV_RANGE_OPTION: &str = "mv_range";
const FAST_OPTION: &str = "fast";

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: KEYFRAME_OPTION, description: KEYFRAME_OPTION_DESC,
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
    NAOptionDefinition {
        name: HUFFMAN_OPTION, description: "use Huffman encoding",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: VERSION_OPTION, description: "codec minor version",
        opt_type: NAOptionDefinitionType::String(Some(&["vp60", "vp61", "vp62"])) },
    NAOptionDefinition {
        name: QUANT_OPTION, description: "force fixed quantiser for encoding",
        opt_type: NAOptionDefinitionType::Int(Some(-1), Some(63)) },
    NAOptionDefinition {
        name: MV_SEARCH_OPTION, description: "motion search mode",
        opt_type: NAOptionDefinitionType::String(Some(&["full", "dia", "hex"])) },
    NAOptionDefinition {
        name: MV_RANGE_OPTION, description: "motion search range (in pixels)",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(30)) },
    NAOptionDefinition {
        name: FAST_OPTION, description: "faster (but worse) encoding",
        opt_type: NAOptionDefinitionType::Bool },
];

impl NAOptionHandler for VP6Encoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        KEYFRAME_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.key_int = intval as u8;
                            }
                        },
                        HUFFMAN_OPTION => {
                            if let NAValue::Bool(bval) = option.value {
                                self.huffman = bval;
                            }
                        },
                        VERSION_OPTION => {
                            if let NAValue::String(ref string) = option.value {
                                self.version = match string.as_str() {
                                        "vp60" => VERSION_VP60,
                                        "vp61" => VERSION_VP61,
                                        "vp62" => VERSION_VP62,
                                        _ => unreachable!(),
                                    };
                            }
                        },
                        QUANT_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.force_q = if intval < 0 { None } else { Some(intval as usize) };
                            }
                        },
                        MV_SEARCH_OPTION => {
                            if let NAValue::String(ref string) = option.value {
                                if let Ok(mv_mode) = string.parse::<MVSearchMode>() {
                                    self.me_mode = mv_mode;
                                }
                            }
                        },
                        MV_RANGE_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.me_range = intval as i16;
                            }
                        },
                        FAST_OPTION => {
                            if let NAValue::Bool(bval) = option.value {
                                self.fast = bval;
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
            KEYFRAME_OPTION => Some(NAValue::Int(i64::from(self.key_int))),
            HUFFMAN_OPTION => Some(NAValue::Bool(self.huffman)),
            VERSION_OPTION => {
                    let ver = match self.version {
                            VERSION_VP60 => "vp60",
                            VERSION_VP61 => "vp61",
                            VERSION_VP62 => "vp62",
                            _ => unreachable!(),
                        };
                    Some(NAValue::String(ver.to_string()))
                },
            QUANT_OPTION => if let Some(q) = self.force_q {
                    Some(NAValue::Int(q as i64))
                } else {
                    Some(NAValue::Int(-1))
                },
            MV_SEARCH_OPTION => Some(NAValue::String(self.me_mode.to_string())),
            MV_RANGE_OPTION => Some(NAValue::Int(i64::from(self.me_range))),
            FAST_OPTION => Some(NAValue::Bool(self.fast)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(VP6Encoder::new(true))
}

pub fn get_encoder_flv() -> Box<dyn NAEncoder + Send> {
    Box::new(VP6Encoder::new(false))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_commonfmt::*;
    use nihav_codec_support::test::enc_video::*;

    fn encode_test(out_name: &'static str, enc_options: &[NAOption], hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        duck_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/VP4/ot171_vp40.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Duck/ot171_vp40.avi",
                stream_type:    StreamType::Video,
                limit:          Some(1),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "vp6",
                out_name,
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  YUV420_FORMAT,
                flipped: true,
                bits:    12,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 25000,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          hash);
    }
    #[test]
    fn test_vp6_encoder_bc() {
        let enc_options = &[
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(42) },
            ];
        encode_test("vp6-bool.avi", enc_options, &[0xb57f49e5, 0x6b48accd, 0xc28fadb3, 0xc89a30d2]);
    }
    #[test]
    fn test_vp6_encoder_fast() {
        let enc_options = &[
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(42) },
                NAOption { name: super::FAST_OPTION, value: NAValue::Bool(true) },
            ];
        encode_test("vp6-fast.avi", enc_options, &[0xb8037ce1, 0xc00ade72, 0x3c0b73c2, 0xbfc4113d]);
    }
    #[test]
    fn test_vp6_encoder_rc() {
        let enc_options = &[
            ];
        encode_test("vp6-rc.avi", enc_options, &[0x790baca9, 0x663eafcf, 0x36d1bed8, 0xddf882de]);
    }
    #[test]
    fn test_vp6_encoder_huff() {
        let enc_options = &[
                NAOption { name: super::HUFFMAN_OPTION, value: NAValue::Bool(true) },
                NAOption { name: super::QUANT_OPTION, value: NAValue::Int(42) },
            ];
        encode_test("vp6-huff.avi", enc_options, &[0x6e9bb23d, 0xde296d92, 0x4c225bae, 0x3651e31f]);
    }
}
