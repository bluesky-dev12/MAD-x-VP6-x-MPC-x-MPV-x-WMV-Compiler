use nihav_core::formats::SND_S16_FORMAT;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use std::str::FromStr;

const SAMPLES:          usize = 240;
const SUBFRAMES:        usize = 4;
const SUBFRAME_LEN:     usize = SAMPLES / SUBFRAMES;
const LPC_ORDER:        usize = 10;
const MIN_PITCH_LAG:    usize = 18;
const MAX_PITCH:        usize = MIN_PITCH_LAG + 127;
const MAX_ERASED_FRAMES:u8    = 3;
const MAX_PULSES:       usize = 6;

#[derive(Clone,Copy,PartialEq)]
enum G7231FrameType {
    Active,
    SID,
    Untransmitted,
}

#[derive(Clone,Copy,Default)]
struct Subframe {
    pitch_lag:      usize,
    acb_lag:        usize,
    acb_gain:       usize,
    dirac_train:    bool,
    pulse_sign:     u8,
    grid_index:     usize,
    amp_index:      usize,
    pulse_pos:      usize,

    ppf_index:      usize,
    ppf_opt_gain:   i32,
    ppf_sc_gain:    i32,
}

impl Subframe {
    fn read_acb_params(&mut self, br: &mut BitReader, is_6300: bool) -> DecoderResult<()> {
        let mut val                     = br.read(12)? as usize;
        let acb_len;
        if is_6300 && (self.pitch_lag < SUBFRAME_LEN - 2) {
            self.dirac_train = (val >> 11) != 0;
            val &= 0x7FF;
            acb_len = 85;
        } else {
            self.dirac_train = false;
            acb_len = 170;
        }
        self.acb_gain  = val / 24;
        self.amp_index = val % 24;
        validate!(self.acb_gain < acb_len);

        Ok(())
    }
    fn gen_fcb_excitation(&self, dst: &mut [i16], is_6300: bool, max_pos: usize, npulses: usize) {
        for el in dst.iter_mut().take(SUBFRAME_LEN) {
            *el = 0;
        }
        let fcb_gain = FIXED_CB_GAIN[self.amp_index];
        if is_6300 {
            if self.pulse_pos >= max_pos {
                return;
            }
            let mut pulse = MAX_PULSES - npulses;
            let mut ppos = self.pulse_pos;
            for i in 0..SUBFRAME_LEN/2 {
                let ret = ppos.checked_sub(PULSE_POSITIONS[pulse][i] as usize);
                if let Some(new_ppos) = ret {
                    ppos = new_ppos;
                    continue;
                }
                pulse += 1;
                let sign = (self.pulse_sign & (1 << (MAX_PULSES - pulse))) != 0;
                dst[i * 2 + self.grid_index] = if !sign { fcb_gain } else { -fcb_gain };
                if pulse == MAX_PULSES {
                    break;
                }
            }
            if self.dirac_train {
                let mut orig = [0; SUBFRAME_LEN];
                orig.copy_from_slice(&dst[..SUBFRAME_LEN]);
                for i in (self.pitch_lag..SUBFRAME_LEN).step_by(self.pitch_lag) {
                    for j in 0..SUBFRAME_LEN - i {
                        dst[i + j] += orig[j];
                    }
                }
            }
        } else {
            let mut cb_pos  = self.pulse_pos;
            let mut cb_sign = self.pulse_sign;
            for i in (0..8).step_by(2) {
                let off = (cb_pos & 7) * 8 + i + self.grid_index;
                dst[off] = if (cb_sign & 1) != 0 { fcb_gain } else { -fcb_gain };
                cb_pos  >>= 3;
                cb_sign >>= 1;
            }

            let lag = (PITCH_CONTRIBUTION[self.acb_gain * 2] as isize + self.pitch_lag as isize + self.acb_lag as isize - 1) as usize;
            let beta = PITCH_CONTRIBUTION[self.acb_gain * 2 + 1];
            if lag < SUBFRAME_LEN - 2 {
                for i in lag..SUBFRAME_LEN {
                    dst[i] += ((beta * i32::from(dst[i - lag])) >> 15) as i16;
                }
            }
        }
    }
    fn gen_acb_excitation(&mut self, acb_vector: &mut [i16; SUBFRAME_LEN], excitation: &[i16], is_6300: bool) {
        let lag = self.pitch_lag + self.acb_lag - 1;

        let mut residual = [0; SUBFRAME_LEN + 5 - 1];
        residual[0] = excitation[MAX_PITCH - 2 - lag];
        residual[1] = excitation[MAX_PITCH - 1 - lag];
        for (i, dst) in residual.iter_mut().skip(2).enumerate() {
            *dst = excitation[MAX_PITCH - lag + i % lag];
        }

        let codebook = if is_6300 && self.pitch_lag < SUBFRAME_LEN - 2 {
                &ACB_GAIN85[self.acb_gain]
            } else {
                &ACB_GAIN170[self.acb_gain]
            };
        for i in 0..SUBFRAME_LEN {
            let sum = dot_product(&residual[i..], codebook, 5);
            acb_vector[i] = (sum.saturating_add(sum).saturating_add(1 << 15) >> 16) as i16;
        }
    }
    fn compute_ppf_coeffs(&mut self, src: &[i16], offset: usize, is_6300: bool) {
        self.ppf_index    = offset;
        self.ppf_opt_gain = 0;
        self.ppf_sc_gain  = 0x7FFF;

        let (fwd_lag, fwd_energy) = autocorr_max(src, offset, SUBFRAME_LEN, self.pitch_lag, true);
        let (bwd_lag, bwd_energy) = autocorr_max(src, offset, SUBFRAME_LEN, self.pitch_lag, false);
        if fwd_lag == 0 && bwd_lag == 0 {
            return;
        }

        let tgt_energy = dot_product(&src[offset..], &src[offset..], SUBFRAME_LEN);
        let fwd_res_energy = if fwd_lag != 0 {
                dot_product(&src[offset + fwd_lag..], &src[offset + fwd_lag..], SUBFRAME_LEN)
            } else { 0 };
        let bwd_res_energy = if bwd_lag != 0 {
                dot_product(&src[offset - bwd_lag..], &src[offset - bwd_lag..], SUBFRAME_LEN)
            } else { 0 };

        let max_energy = tgt_energy.max(fwd_energy).max(fwd_res_energy).max(bwd_energy).max(bwd_res_energy);
        let scale = norm_bits(max_energy, 31);
        let tgt_energy     = tgt_energy     << scale >> 16;
        let fwd_energy     = fwd_energy     << scale >> 16;
        let fwd_res_energy = fwd_res_energy << scale >> 16;
        let bwd_energy     = bwd_energy     << scale >> 16;
        let bwd_res_energy = bwd_res_energy << scale >> 16;

        let use_fwd = if fwd_lag != 0 && bwd_lag == 0 {
                true
            } else if fwd_lag == 0 {
                false
            } else {
                let tmp1 = bwd_res_energy * ((fwd_energy * fwd_energy + (1 << 14)) >> 15);
                let tmp2 = fwd_res_energy * ((bwd_energy * bwd_energy + (1 << 14)) >> 15);
                tmp1 >= tmp2
            };
        if use_fwd {
            self.compute_ppf_gains(offset + fwd_lag, is_6300, tgt_energy, fwd_energy, fwd_res_energy);
        } else {
            self.compute_ppf_gains(offset - bwd_lag, is_6300, tgt_energy, bwd_energy, bwd_res_energy);
        }
    }
    fn compute_ppf_gains(&mut self, offset: usize, is_6300: bool, tgt_energy: i32, corr_energy: i32, res_energy: i32) {
        self.ppf_index = offset;

        let tmp1 = (tgt_energy * res_energy) >> 1;
        let tmp2 = corr_energy * corr_energy * 2;
        if tmp1 >= tmp2 {
            return;
        }
        let gain_weight = if is_6300 { 0x1800 } else { 0x2000 };
        self.ppf_opt_gain = if corr_energy >= res_energy {
                gain_weight
            } else {
                ((corr_energy << 15) / res_energy * gain_weight) >> 15
            };

        let tmp1 = (tgt_energy << 15) + (corr_energy * self.ppf_opt_gain * 2);
        let tmp2 = ((self.ppf_opt_gain * self.ppf_opt_gain) >> 15) * res_energy;

        let residual = tmp1.saturating_add(tmp2).saturating_add(1 << 15) >> 16;

        if tgt_energy >= residual * 2 {
            self.ppf_sc_gain = square_root_i32(0x7FFF0000);
        } else {
            let val = (tgt_energy << 14) / residual;
            self.ppf_sc_gain = square_root_i32(val << 16);
        }

        self.ppf_opt_gain = i32::from(clip16((self.ppf_opt_gain * self.ppf_sc_gain) >> 15));
    }
}

fn square_root_i32(val: i32) -> i32 {
    let mut res = 0;
    let mut exp = 1 << 14;

    for _ in 0..14 {
        let res_exp = res + exp;
        if val >= res_exp * res_exp * 2 {
            res += exp;
        }
        exp >>= 1;
    }

    res
}

struct PRNG {
    seed:   i32,
}

const CNG_RND_SEED: i32 = 12345;
impl PRNG {
    fn new()     -> Self { Self { seed: 0 } }
    fn new_cng() -> Self { Self { seed: CNG_RND_SEED } }
    fn reset(&mut self) { self.seed = CNG_RND_SEED; }
    fn next(&mut self) -> i32 {
        self.seed = self.seed.wrapping_mul(521).wrapping_add(259);
        self.seed
    }
    fn next_range(&mut self, range: usize) -> usize {
        let val = (self.next() & 0x7FFF) as usize;
        (val * range) >> 15
    }
}

struct G7231Decoder {
    chmap:              NAChannelMap,
    ainfo:              NAAudioInfo,
    info:               NACodecInfoRef,

    is_6300:            bool,
    prev_lsp:           [i16; LPC_ORDER],
    sid_lsp:            [i16; LPC_ORDER],
    prev_ftype:         G7231FrameType,
    cur_ftype:          G7231FrameType,
    lsp_index:          [usize; 3],
    subframe:           [Subframe; SUBFRAMES],
    excitation:         [i16; MAX_PITCH + SAMPLES + 4],
    prev_excitation:    [i16; MAX_PITCH],
    lpc:                [[i16; LPC_ORDER]; SUBFRAMES],

    filt_mem:           [i16; LPC_ORDER],

    interp_index:       usize,
    interp_gain:        i16,
    sid_gain:           i32,
    cur_gain:           i32,

    fir_mem:            [i16; LPC_ORDER],
    iir_mem:            [i32; LPC_ORDER],
    reflection_coef:    i32,
    pf_gain:            i32,

    cng_rnd:            PRNG,
    rnd:                PRNG,
    erased_frames:      u8,

    synth_buf:          [i16; SAMPLES + LPC_ORDER + MAX_PITCH + 4],
}

macro_rules! weighted_sum {
    ($dst: expr, $src1: expr, $weight1: expr, $src2: expr, $weight2: expr, $shift: expr) => {
        let bias = 1 << ($shift - 1);
        for (dst, (src1, src2)) in $dst.iter_mut().zip($src1.iter().zip($src2.iter())) {
            let val = (i32::from(*src1) * $weight1 + i32::from(*src2) * $weight2 + bias) >> $shift;
            *dst = clip16(val);
        }
    }
}

impl G7231Decoder {
    fn new() -> Self {
        let prev_lsp = DC_LSP;
        let sid_lsp  = DC_LSP;
        G7231Decoder {
            chmap:              NAChannelMap::from_str("C").unwrap(),
            ainfo:              NAAudioInfo::new(8000, 1, SND_S16_FORMAT, SAMPLES),
            info:               NACodecInfo::new_dummy(),

            is_6300:            true,
            prev_lsp, sid_lsp,
            prev_ftype:         G7231FrameType::SID,
            cur_ftype:          G7231FrameType::Untransmitted,
            lsp_index:          [0; 3],
            subframe:           [Subframe::default(); SUBFRAMES],
            excitation:         [0; MAX_PITCH + SAMPLES + 4],
            prev_excitation:    [0; MAX_PITCH],
            lpc:                [[0; LPC_ORDER]; SUBFRAMES],

            filt_mem:           [0; LPC_ORDER],

            interp_index:       0,
            interp_gain:        0,
            sid_gain:           0,
            cur_gain:           0,

            fir_mem:            [0; LPC_ORDER],
            iir_mem:            [0; LPC_ORDER],
            reflection_coef:    0,
            pf_gain:            1 << 12,

            cng_rnd:            PRNG::new_cng(),
            rnd:                PRNG::new(),
            erased_frames:      0,

            synth_buf:          [0; SAMPLES + LPC_ORDER + MAX_PITCH + 4],
        }
    }
    fn unpack_frame(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        const FRAME_SIZE: [isize; 4] = [ 24, 20, 4, 1 ];
        let ftype                       = br.read(2)? as usize;
        validate!(br.left() + 2 >= FRAME_SIZE[ftype] * 8);
        if ftype == 3 {
            self.cur_ftype = G7231FrameType::Untransmitted;
            return Ok(());
        }

        for lsp_index in self.lsp_index.iter_mut().rev() {
            *lsp_index                  = br.read(8)? as usize;
        }

        if ftype == 2 {
            self.cur_ftype = G7231FrameType::Untransmitted;
            self.subframe[0].amp_index  = br.read(8)? as usize;
            return Ok(());
        }

        self.cur_ftype = G7231FrameType::Active;
        self.is_6300 = ftype == 0;

        for i in (0..4).step_by(2) {
            self.subframe[i].pitch_lag  = (br.read(7)? as usize) + MIN_PITCH_LAG;
            validate!(self.subframe[i].pitch_lag < MIN_PITCH_LAG + 124);
            self.subframe[i + 1].pitch_lag = self.subframe[i].pitch_lag;
            self.subframe[i].acb_lag = 1;
            self.subframe[i + 1].acb_lag = br.read(2)? as usize;
        }

        for subframe in self.subframe.iter_mut() {
            subframe.read_acb_params(br, self.is_6300)?;
        }
        for subframe in self.subframe.iter_mut() {
            subframe.grid_index         = br.read(1)? as usize;
        }
        if self.is_6300 {
                                          br.read(1)?;
            let mut ppos                = br.read(13)? as usize;
            self.subframe[0].pulse_pos = ppos / 810;
            ppos %= 810;
            self.subframe[1].pulse_pos = ppos / 90;
            ppos %= 90;
            self.subframe[2].pulse_pos = ppos / 9;
            self.subframe[3].pulse_pos = ppos % 9;

            for (i, subframe) in self.subframe.iter_mut().enumerate() {
                let bits = if (i & 1) == 0 { 16 } else { 14 };
                let val                 = br.read(bits)? as usize;
                subframe.pulse_pos = (subframe.pulse_pos << bits) | val;
            }
            for (i, subframe) in self.subframe.iter_mut().enumerate() {
                let bits = if (i & 1) == 0 { 6 } else { 5 };
                subframe.pulse_sign     = br.read(bits)? as u8;
            }
        } else {
            for subframe in self.subframe.iter_mut() {
                subframe.pulse_pos      = br.read(12)? as usize;
            }
            for subframe in self.subframe.iter_mut() {
                subframe.pulse_sign     = br.read(4)? as u8;
            }
        }

        Ok(())
    }
    fn synth_frame_active(&mut self, dst: &mut [i16], bad_frame: bool) {
        const FCB_MAX_POS: [usize; 4] = [ 593775, 142506, 593775, 142506 ];
        if !bad_frame {
            self.erased_frames = 0;
        } else {
            self.erased_frames = (self.erased_frames + 1).min(MAX_ERASED_FRAMES);
        }
        if bad_frame {
            self.lsp_index = [0; 3];
        }
        let mut cur_lsp = [0; LPC_ORDER];
        Self::inverse_quant(&self.prev_lsp, &mut cur_lsp, &self.lsp_index, bad_frame);
        Self::interpolate_lsp(&mut self.lpc, &cur_lsp, &self.prev_lsp);
        self.prev_lsp.copy_from_slice(&cur_lsp);
        self.excitation[..MAX_PITCH].copy_from_slice(&self.prev_excitation);
        if self.erased_frames == 0 {
            let mut acb_vector = [0; SUBFRAME_LEN];
            self.interp_gain = FIXED_CB_GAIN[(self.subframe[2].amp_index + self.subframe[3].amp_index) >> 1];
            let mut exc_start = MAX_PITCH;
            for (i, subframe) in self.subframe.iter_mut().enumerate() {
                subframe.gen_fcb_excitation(&mut self.excitation[exc_start..], self.is_6300, FCB_MAX_POS[i], if (i & 1) == 0 { 6 } else { 5 });
                subframe.gen_acb_excitation(&mut acb_vector, &self.excitation[SUBFRAME_LEN * i..], self.is_6300);
                for i in 0..SUBFRAME_LEN {
                    let val = self.excitation[exc_start + i];
                    self.excitation[exc_start + i] = val.saturating_add(val).saturating_add(acb_vector[i]);
                }
                exc_start += SUBFRAME_LEN;
            }
            self.compute_interp_index();
            let mut offset = MAX_PITCH;
            for subframe in self.subframe.iter_mut() {
                subframe.compute_ppf_coeffs(&self.synth_buf[LPC_ORDER..], offset, self.is_6300);
                offset += SUBFRAME_LEN;
            }
            for i in 0..SUBFRAMES {
                let src1 = &self.excitation[MAX_PITCH + i * SUBFRAME_LEN..][..SUBFRAME_LEN];
                let src2 = &self.excitation[self.subframe[i].ppf_index..][..SUBFRAME_LEN];
                let dst = &mut self.synth_buf[LPC_ORDER + i * SUBFRAME_LEN..][..SUBFRAME_LEN];
                weighted_sum!(dst, src1, self.subframe[i].ppf_sc_gain, src2, self.subframe[i].ppf_opt_gain, 15);
            }
            self.prev_excitation.copy_from_slice(&self.excitation[SAMPLES..][..MAX_PITCH]);
        } else {
            self.interp_gain = (self.interp_gain * 3 + 2) >> 2;
            if self.erased_frames == MAX_ERASED_FRAMES {
                for el in self.excitation.iter_mut() {
                    *el = 0;
                }
                for el in self.prev_excitation.iter_mut() {
                    *el = 0;
                }
                for el in dst.iter_mut() {
                    *el = 0;
                }
            } else {
                if self.interp_index != 0 {
                    for i in 0..self.interp_index {
                        let sample = (i32::from(self.excitation[MAX_PITCH + i]) * 3) >> 2;
                        self.synth_buf[LPC_ORDER + i] = sample as i16;
                    }
                    for i in self.interp_index..SAMPLES {
                        self.synth_buf[LPC_ORDER + i] = self.synth_buf[LPC_ORDER + i - self.interp_index];
                    }
                } else {
                    for i in 0..SAMPLES {
                        let sample = self.rnd.next().wrapping_mul(i32::from(self.interp_gain)) >> 15;
                        self.synth_buf[LPC_ORDER + i] = sample as i16;
                    }
                }
                self.prev_excitation.copy_from_slice(&self.synth_buf[LPC_ORDER + SAMPLES - MAX_PITCH..][..MAX_PITCH]);
            }
        }
        self.cng_rnd.reset();
    }
    fn amp_index_to_sid_gain(val: usize) -> i32 {
        if val < 0x10 {
            (val as i32) << 6
        } else if val < 0x20 {
            ((val as i32) - 8) << 7
        } else {
            ((val as i32) - 20) << 8
        }
    }
    fn estimate_sid_gain(cur_gain: i32, sid_gain: i32) -> i32 {
        const CNG_BSEG: [i32; 3] = [ 2048, 18432, 231233 ];
        let shift = 16 - cur_gain * 2;
        let t = if shift > 0 { sid_gain << shift } else { sid_gain >> -shift };
        let x = (t * 273) >> 16;
        if x >= CNG_BSEG[2] {
            return 63;
        }
        let (shift, seg) = if x >= CNG_BSEG[1] { (4, 3) } else { (3, if x >= CNG_BSEG[0] { 1 } else { 0 }) };
        let seg2 = seg.min(3);

        let mut val = 1 << shift;
        let mut val_add = val >> 1;
        for _ in 0..shift {
            let t = seg * 32 + (val << seg2);
            let t2 = t * t;
            if x >= t2 {
                val += val_add;
            } else {
                val -= val_add;
            }
            val_add >>= 1;
        }

        let t = seg * 32 + (val << seg2);
        let y = t * t - x;
        if y <= 0 {
            let t = seg * 32 + ((val + 1) << seg2);
            let t2 = t * t - x;
            let val = (seg2 - 1) * 16 + val;
            if t2 >= y {
                val + 1
            } else {
                val
            }
        } else {
            let t = seg * 32 + ((val - 1) << seg2);
            let t2 = t * t - x;
            let val = (seg2 - 1) * 16 + val;
            if t2 >= y {
                val - 1
            } else {
                val
            }
        }
    }
    fn synth_frame_other(&mut self) {
        if self.cur_ftype == G7231FrameType::SID {
            self.sid_gain = Self::amp_index_to_sid_gain(self.subframe[0].amp_index);
            Self::inverse_quant(&self.prev_lsp, &mut self.sid_lsp, &self.lsp_index, false);
        } else if self.prev_ftype == G7231FrameType::Active {
            self.sid_gain = Self::estimate_sid_gain(self.cur_gain, self.sid_gain);
        }
        if self.prev_ftype == G7231FrameType::Active {
            self.cur_gain = self.sid_gain;
        } else {
            self.cur_gain = (self.cur_gain * 7 + self.sid_gain) >> 3;
        }
        self.generate_noise();
        Self::interpolate_lsp(&mut self.lpc, &self.sid_lsp, &self.prev_lsp);
        self.prev_lsp = self.sid_lsp;
    }
    fn generate_noise(&mut self) {
        const ADAPTIVE_LAG: [usize; SUBFRAMES] = [1, 0, 1, 3];
        self.subframe[0].pitch_lag = self.cng_rnd.next_range(21) + 123;
        self.subframe[1].pitch_lag = self.subframe[0].pitch_lag;
        self.subframe[2].pitch_lag = self.cng_rnd.next_range(19) + 123;
        self.subframe[3].pitch_lag = self.subframe[2].pitch_lag;

        for (i, subframe) in self.subframe.iter_mut().enumerate() {
            subframe.acb_gain = self.cng_rnd.next_range(50) + 1;
            subframe.acb_lag  = ADAPTIVE_LAG[i];
        }

        let mut off = [0; 4];
        let mut signs = [[0; 11]; 2];
        for i in (0..4).step_by(2) {
            let t = self.cng_rnd.next_range(1 << 13);
            off[i]     = t & 1;
            off[i + 1] = ((t >> 1) & 1) + SUBFRAME_LEN;
            for j in 0..11 {
                signs[i/2][j] = ((((t >> (j + 2)) & 1) * 2 - 1) << 14) as i32;
            }
        }

        let mut pos = [0; 11 * SUBFRAMES];
        let mut pidx = 0;
        let mut tmp = [0; SUBFRAME_LEN / 2];
        for i in 0..SUBFRAMES {
            let npulses = if (i & 1) == 0 { 6 } else { 5 };
            for j in 0..npulses {
                let idx = self.cng_rnd.next_range(SUBFRAME_LEN / 2 - j);
                pos[pidx] = tmp[idx] * 2 + off[i];
                pidx += 1;
                tmp[idx] = tmp[SUBFRAME_LEN / 2 - 1 - j];
            }
        }

        self.synth_buf[LPC_ORDER..][..MAX_PITCH].copy_from_slice(&self.prev_excitation);
        let mut acb_vec = [0; SUBFRAME_LEN];
        let mut tmp = [0; SUBFRAME_LEN * 2];
        for i in (0..SUBFRAMES).step_by(2) {
            let buf = &mut self.synth_buf[LPC_ORDER + SUBFRAME_LEN * i..];
            self.subframe[i].gen_acb_excitation(&mut acb_vec, buf, self.is_6300);
            buf[..SUBFRAME_LEN].copy_from_slice(&acb_vec);
            self.subframe[i + 1].gen_acb_excitation(&mut acb_vec, &buf[SUBFRAME_LEN..], self.is_6300);
            buf[SUBFRAME_LEN..][..SUBFRAME_LEN].copy_from_slice(&acb_vec);

            let mut max = 0;
            for j in 0..SUBFRAME_LEN*2 {
                max |= i32::from(buf[j]).abs();
            }
            let shift = if max == 0 { 0 } else {
                    (-10 + (32 - max.min(0x7FFF).leading_zeros()) as i32).max(-2)
                };
            let mut sum = 0;
            if shift < 0 {
                for j in 0..SUBFRAME_LEN*2 {
                    let val = buf[j] << -shift;
                    sum += i64::from(val) * i64::from(val);
                    tmp[j] = val;
                }
            } else {
                for j in 0..SUBFRAME_LEN*2 {
                    let val = buf[j] >> shift;
                    sum += i64::from(val) * i64::from(val);
                    tmp[j] = val;
                }
            }

            let mut b0 = 0;
            for j in 0..11 {
                b0 += i32::from(tmp[pos[i / 2 * 11 + j]]) * signs[i / 2][j];
            }
            b0 = ((i64::from(b0) * 2 * 2979 + (1 << 29)) >> 30) as i32;

            let mut c = self.cur_gain * ((self.cur_gain * (SUBFRAME_LEN as i32)) >> 5);
            if shift * 2 + 3 >= 0 {
                c >>= shift * 2 + 3;
            } else {
                c <<= -(shift * 2 + 3);
            }
            c = ((i64::from(clip32(sum * 2) - c) * 2979) >> 15) as i32;

            let delta = b0 * b0 * 2 - c;
            let x = if delta <= 0 {
                    -b0
                } else {
                    let d0 = square_root_i32(delta);
                    let x0 = d0 - b0;
                    let t  = d0 + b0;
                    if t.abs() < x0.abs() {
                        -t
                    } else {
                        x0
                    }
                };
            let shift = shift + 1;
            let x = (if shift >= 0 { x << shift } else { x >> -shift }).min(10000).max(-10000);
            for j in 0..11 {
                let val = (x * signs[i / 2][j]) >> 15;
                buf[pos[i / 2 * 11 + j]] = buf[pos[i / 2 * 11 + j]].saturating_add(val as i16);
            }

            for j in 0..SUBFRAME_LEN*2 {
                buf[MAX_PITCH + j] = buf[j];
            }
        }
        self.prev_excitation.copy_from_slice(&self.synth_buf[LPC_ORDER + SAMPLES..][..MAX_PITCH]);
    }
    fn compute_interp_index(&mut self) {
        let pitch_lag = self.subframe[3].pitch_lag;

        self.cur_gain = Self::scale_vector(&mut self.synth_buf[LPC_ORDER..][..SAMPLES + MAX_PITCH], &self.excitation[..SAMPLES + MAX_PITCH]);

        let (pos, cre) = autocorr_max(&self.synth_buf[LPC_ORDER..], MAX_PITCH + SUBFRAME_LEN * 2, SUBFRAME_LEN * 2, pitch_lag, false);
        let corr_energy = cre.saturating_add(1 << 15) >> 16;

        let tgt_energy = dot_product(&self.synth_buf[LPC_ORDER + MAX_PITCH + SUBFRAME_LEN * 2..], &self.synth_buf[LPC_ORDER + MAX_PITCH + SUBFRAME_LEN * 2..], SUBFRAME_LEN * 2);
        self.sid_gain = tgt_energy.saturating_add(1 << 15) >> 16;

        if corr_energy <= 0 {
            self.interp_index = 0;
            return;
        }

        let best_energy = dot_product(&self.synth_buf[LPC_ORDER + MAX_PITCH + SUBFRAME_LEN * 2 - pos..], &self.synth_buf[LPC_ORDER + MAX_PITCH + SUBFRAME_LEN * 2 - pos..], SUBFRAME_LEN * 2);
        let best_energy = best_energy.saturating_add(1 << 15) >> 16;

        let tmp = (best_energy * self.sid_gain) >> 3;

        if tmp < corr_energy * corr_energy {
            self.interp_index = pos;
        } else {
            self.interp_index = 0;
        }
    }
    fn scale_vector(dst: &mut [i16], src: &[i16]) -> i32 {
        let mut max = 0;
        for el in src.iter() {
            max = max.max(i32::from(*el).abs());
        }
        max = max.min(0x7FFF);
        let shift = norm_bits(max, 15);
        for (dst, src) in dst.iter_mut().zip(src.iter()) {
            *dst = *src << shift >> 3;
        }
        i32::from(shift) - 3
    }
    fn inverse_quant(prev_lsp: &[i16; LPC_ORDER], lsp: &mut [i16; LPC_ORDER], lsp_index: &[usize; 3], bad_frame: bool) {
        let (min_dist, pred) = if !bad_frame { (0x100, 12288) } else { (0x200, 23552) };
        lsp[0] = LSP_CODEBOOK0[lsp_index[0]][0];
        lsp[1] = LSP_CODEBOOK0[lsp_index[0]][1];
        lsp[2] = LSP_CODEBOOK0[lsp_index[0]][2];
        lsp[3] = LSP_CODEBOOK1[lsp_index[1]][0];
        lsp[4] = LSP_CODEBOOK1[lsp_index[1]][1];
        lsp[5] = LSP_CODEBOOK1[lsp_index[1]][2];
        lsp[6] = LSP_CODEBOOK2[lsp_index[2]][0];
        lsp[7] = LSP_CODEBOOK2[lsp_index[2]][1];
        lsp[8] = LSP_CODEBOOK2[lsp_index[2]][2];
        lsp[9] = LSP_CODEBOOK2[lsp_index[2]][3];

        for i in 0..LPC_ORDER {
            let diff = ((i32::from(prev_lsp[i]) - i32::from(DC_LSP[i])) * pred + (1 << 14)) >> 15;
            lsp[i] = (i32::from(lsp[i]) + i32::from(DC_LSP[i]) + diff) as i16;
        }

        let mut stable = false;

        for _ in 0..LPC_ORDER {
            lsp[0] = lsp[0].max(0x100);
            lsp[LPC_ORDER - 1] = lsp[LPC_ORDER - 1].min(0x7E00);

            for i in 1..LPC_ORDER {
                let mut val = min_dist + lsp[i - 1] - lsp[i];
                if val > 0 {
                    val >>= 1;
                    lsp[i - 1] -= val;
                    lsp[i]     += val;
                }
            }
            stable = true;
            for i in 1..LPC_ORDER {
                let val = lsp[i - 1] + min_dist - lsp[i] - 4;
                if val > 0 {
                    stable = false;
                    break;
                }
            }
            if stable {
                break;
            }
        }
        if !stable {
            lsp.copy_from_slice(prev_lsp);
        }
    }
    fn interpolate_lsp(lpc: &mut [[i16; LPC_ORDER]; SUBFRAMES], lsp: &[i16; LPC_ORDER], prev_lsp: &[i16; LPC_ORDER]) {
        weighted_sum!(lpc[0], lsp, 0x1000, prev_lsp, 0x3000, 14);
        weighted_sum!(lpc[1], lsp, 0x2000, prev_lsp, 0x2000, 14);
        weighted_sum!(lpc[2], lsp, 0x3000, prev_lsp, 0x1000, 14);
        lpc[3].copy_from_slice(lsp);
        for clpc in lpc.iter_mut() {
            Self::lsp2lpc(clpc);
        }
    }
    fn lsp2lpc(lpc: &mut [i16; LPC_ORDER]) {
        let mut tmp1 = [0; LPC_ORDER/2 + 1];
        let mut tmp2 = [0; LPC_ORDER/2 + 1];

        for lpc in lpc.iter_mut() {
            let index  = ((*lpc >> 7) & 0x1FF) as usize;
            let offset = (i32::from(*lpc & 0x7F) << 8) + 0x80;
            let val1 = i32::from(COS_TAB[index]);
            let val2 = (i32::from(COS_TAB[index + 1]) - val1) * offset * 2;
            *lpc = -(((val1 << 17) + val2 * 2).saturating_add(1 << 15) >> 16) as i16;
        }

        tmp1[0] = 1 << 28;
        tmp1[1] = (i32::from(lpc[0]) << 14) + (i32::from(lpc[2]) << 14);
        tmp1[2] = i32::from(lpc[0]) * i32::from(lpc[2]) + (2 << 28);

        tmp2[0] = 1 << 28;
        tmp2[1] = (i32::from(lpc[1]) << 14) + (i32::from(lpc[3]) << 14);
        tmp2[2] = i32::from(lpc[1]) * i32::from(lpc[3]) + (2 << 28);

        for i in 2..LPC_ORDER/2 {
            tmp1[i + 1] = tmp1[i - 1] + mul16(tmp1[i], lpc[i * 2]);
            tmp2[i + 1] = tmp2[i - 1] + mul16(tmp2[i], lpc[i * 2 + 1]);

            for j in (2..=i).rev() {
                tmp1[j] = mul16(tmp1[j - 1], lpc[i * 2])     + (tmp1[j] >> 1) + (tmp1[j - 2] >> 1);
                tmp2[j] = mul16(tmp2[j - 1], lpc[i * 2 + 1]) + (tmp2[j] >> 1) + (tmp2[j - 2] >> 1);
            }

            tmp1[0] >>= 1;
            tmp2[0] >>= 1;
            tmp1[1] = (tmp1[1] + (i32::from(lpc[2 * i])     << 16 >> i)) >> 1;
            tmp2[1] = (tmp2[1] + (i32::from(lpc[2 * i + 1]) << 16 >> i)) >> 1;
        }

        for i in 0..LPC_ORDER/2 {
            let c0 = i64::from(tmp1[i + 1]) + i64::from(tmp1[i]);
            let c1 = i64::from(tmp2[i + 1]) - i64::from(tmp2[i]);
            lpc[i]                 = (clip32((c0 + c1) * 8 + (1 << 15)) >> 16) as i16;
            lpc[LPC_ORDER - i - 1] = (clip32((c0 - c1) * 8 + (1 << 15)) >> 16) as i16;
        }
    }
    fn do_lpc(buf: &mut [i16], offset: usize, lpc: &[i16; LPC_ORDER]) {
        for i in 0..SUBFRAME_LEN {
            let mut sum = 0i32;
            for j in 0..LPC_ORDER {
                sum = sum.wrapping_sub(i32::from(buf[offset + i - j - 1]) * i32::from(lpc[j]));
            }
            let pred = (sum + (1 << 12)) >> 12;
            buf[offset + i] = clip16((i32::from(buf[offset + i]) + pred) >> 1);
        }
    }
    fn formant_postfilter(&mut self, dst: &mut [i16]) {
        self.synth_buf[..LPC_ORDER].copy_from_slice(&self.fir_mem);
        let mut filter_data = [0; LPC_ORDER + SAMPLES];
        filter_data[..LPC_ORDER].copy_from_slice(&self.iir_mem);

        let mut filter_coef = [[0; LPC_ORDER]; 2];
        for i in 0..SUBFRAMES {
            for j in 0..LPC_ORDER {
                filter_coef[0][j] = (i32::from(-self.lpc[i][j]) * i32::from(POSTFILTER_COEFFS[0][j]) + (1 << 14)) >> 15;
                filter_coef[1][j] = (i32::from(-self.lpc[i][j]) * i32::from(POSTFILTER_COEFFS[1][j]) + (1 << 14)) >> 15;
            }
            Self::iir_filter(&filter_coef, &self.synth_buf, &mut filter_data, LPC_ORDER + i * SUBFRAME_LEN);
        }
        self.fir_mem.copy_from_slice(&self.synth_buf[SAMPLES..][..LPC_ORDER]);
        self.iir_mem.copy_from_slice(&filter_data[SAMPLES..]);
        let mut offset = 0;
        for _ in 0..SUBFRAMES {
            let scale = Self::scale_vector(&mut dst[offset..][..SUBFRAME_LEN], &self.synth_buf[offset + LPC_ORDER..][..SUBFRAME_LEN]);
            let ac1 = dot_product(&dst[offset..], &dst[offset + 1..], SUBFRAME_LEN - 1);
            let ac0 = dot_product(&dst[offset..], &dst[offset..], SUBFRAME_LEN);
            let tmp = if (ac0 >> 16) != 0 { (ac1 >> 2) / (ac0 >> 16) } else { 0 };
            self.reflection_coef = (3 * self.reflection_coef + tmp + 2) >> 2;
            let gain = (-self.reflection_coef >> 1) & !3;

            for i in 0..SUBFRAME_LEN {
                let val = (filter_data[offset + LPC_ORDER + i - 1] >> 16) * gain;
                dst[offset + i] = (filter_data[offset + LPC_ORDER + i].saturating_add(val).saturating_add(val) >> 16) as i16;
            }

            let shift = 2 * scale + 4;
            let energy = if shift < 0 { clip32(i64::from(ac0) << -shift) } else { ac0 >> shift };
            Self::gain_scale(&mut self.pf_gain, &mut dst[offset..], energy);

            offset += SUBFRAME_LEN;
        }
    }
    fn iir_filter(coef: &[[i32; LPC_ORDER]; 2], src: &[i16], dst: &mut [i32], offset: usize) {
        for i in 0..SUBFRAME_LEN {
            let mut sum = 0;
            for j in 0..LPC_ORDER {
                sum -= i64::from(coef[0][j]) * i64::from(src[offset - 1 + i - j]) -
                       i64::from(coef[1][j]) * i64::from(dst[offset - 1 + i - j] >> 16);
            }
            dst[offset + i] = clip32((i64::from(src[offset + i]) << 16) + (sum << 3) + (1 << 15));
        }
    }
    fn gain_scale(pf_gain: &mut i32, buf: &mut [i16], energy: i32) {
        let mut den = 0i32;
        for i in 0..SUBFRAME_LEN {
            let val = i32::from(buf[i] >> 2);
            den = den.saturating_add(val * val).saturating_add(val * val);
        }
        let mut num = energy;
        let gain;
        if (num != 0) && (den != 0) {
            let bits1 = norm_bits(num, 31);
            let bits2 = norm_bits(den, 31);
            num = num << bits1 >> 1;
            den <<= bits2;
            let shift = (5 + bits1 - bits2).max(0);

            gain = square_root_i32(((num >> 1) / (den >> 16)) << 16 >> shift);
        } else {
            gain = 1 << 12;
        }

        for i in 0..SUBFRAME_LEN {
            *pf_gain = (15 * *pf_gain + gain + (1 << 3)) >> 4;
            buf[i] = clip16((i32::from(buf[i]) * (*pf_gain + (*pf_gain >> 4)) + (1 << 10)) >> 11);
        }
    }
}

fn clip16(a: i32) -> i16 {
    a.min(i32::from(std::i16::MAX)).max(i32::from(std::i16::MIN)) as i16
}

fn clip32(a: i64) -> i32 {
    a.min(i64::from(std::i32::MAX)).max(i64::from(std::i32::MIN)) as i32
}

fn mul16(a: i32, b: i16) -> i32 {
    let b = i32::from(b);
    (a >> 16) * b * 2 + (((a & 0xFFFF) * b) >> 15)
}

fn norm_bits(val: i32, target: u8) -> u8 {
    if val == 0 {
        target
    } else {
        target - (32 - val.leading_zeros()) as u8
    }
}

fn autocorr_max(src: &[i16], offset: usize, length: usize, pitch_lag: usize, forward: bool) -> (usize, i32) {
    let pitch_lag = pitch_lag.min(MAX_PITCH - 3);
    let mut max_energy = 0;
    let mut lag = 0;

    if forward {
        let end = (pitch_lag + 3).min(SAMPLES + MAX_PITCH - offset - length);
        for i in pitch_lag-3..=end {
            let energy = dot_product(&src[offset..], &src[offset + i..], length);
            if max_energy < energy {
                max_energy = energy;
                lag = i;
            }
        }
    } else {
        for i in pitch_lag-3..=pitch_lag+3 {
            let energy = dot_product(&src[offset..], &src[offset - i..], length);
            if max_energy < energy {
                max_energy = energy;
                lag = i;
            }
        }
    }
    (lag, max_energy)
}

fn dot_product(src1: &[i16], src2: &[i16], length: usize) -> i32 {
    let mut sum = 0;
    for (a, b) in src1.iter().zip(src2.iter()).take(length) {
        sum += i64::from(*a) * i64::from(*b)
    }
    clip32(sum << 1)
}

impl NADecoder for G7231Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(_ainfo) = info.get_properties() {
            self.info = info.replace_info(NACodecTypeInfo::Audio(self.ainfo));
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        validate!(info.get_properties().is_audio());
        let src = pkt.get_buffer();

        let mut bad_frame = false;

        let mut br = BitReader::new(src.as_slice(), BitReaderMode::LE);
        if self.unpack_frame(&mut br).is_err() {
            bad_frame = true;
            self.cur_ftype = if self.prev_ftype == G7231FrameType::Active {
                    G7231FrameType::Active
                } else {
                    G7231FrameType::Untransmitted
                };
        }

        let abuf = alloc_audio_buffer(self.ainfo, SAMPLES, self.chmap.clone())?;
        let mut adata = abuf.get_abuf_i16().unwrap();
        let asamples = adata.get_data_mut().unwrap();
        if self.cur_ftype == G7231FrameType::Active {
            self.synth_frame_active(asamples, bad_frame);
        } else {
            self.synth_frame_other();
        }
        self.prev_ftype = self.cur_ftype;
        self.synth_buf[..LPC_ORDER].copy_from_slice(&self.filt_mem);
        for i in 0..SUBFRAMES {
            Self::do_lpc(&mut self.synth_buf, LPC_ORDER + i * SUBFRAME_LEN, &self.lpc[i]);
        }
        self.filt_mem.copy_from_slice(&self.synth_buf[SAMPLES..][..LPC_ORDER]);
        self.formant_postfilter(asamples);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for G7231Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(G7231Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::vivo_register_all_decoders;
    use crate::vivo_register_all_demuxers;
    #[test]
    fn test_g723_1() {
        let mut dmx_reg = RegisteredDemuxers::new();
        vivo_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        vivo_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/vivo/viv1/adalogo.viv
        let file = "assets/Misc/adalogo.viv";
        //let file = "assets/Misc/gr_al.viv";
        //test_decode_audio("vivo", file, Some(1500), None/*Some("g7231")*/, &dmx_reg, &dec_reg);
        test_decoding("vivo", "g723.1", file, None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x94e60e2f, 0x578493e7, 0xe7ab1f1c, 0xae155977]));
    }
}

const DC_LSP: [i16; LPC_ORDER] = [
    0x0c3b, 0x1271, 0x1e0a, 0x2a36, 0x3630,
    0x406f, 0x4d28, 0x56f4, 0x638c, 0x6c46
];

const LSP_CODEBOOK0: [[i16; 3]; 256] = [
    [    0,      0,      0], [ -270,  -1372,  -1032], [ -541,  -1650,  -1382],
    [ -723,  -2011,  -2213], [ -941,  -1122,  -1942], [ -780,  -1145,  -2454],
    [ -884,  -1309,  -1373], [-1051,  -1523,  -1766], [-1083,  -1622,  -2300],
    [ -777,  -1377,  -2147], [ -935,  -1467,  -2763], [ -802,  -1327,  -3471],
    [ -935,  -1959,  -3999], [ -240,    -89,    222], [ -661,   -257,   -160],
    [ -994,   -466,   -419], [ -188,   -164,   -278], [ -342,   -512,   -415],
    [ -607,   -511,   -797], [   16,     19,   -716], [  374,    425,   -972],
    [ -346,    245,   -282], [ -265,    506,   -754], [ -620,   -147,   1955],
    [ -742,   -860,   2597], [ -150,   -352,   2704], [  305,    880,   1954],
    [  123,    731,   2766], [ -348,    765,   3327], [  618,    221,   3258],
    [ -178,    -47,   4219], [  393,   1304,   3842], [  698,   1702,   4801],
    [   63,   -584,   1229], [ -215,   -732,   1704], [  172,   -335,   1909],
    [   -2,    216,   1797], [  353,    127,   2205], [-1208,    188,     11],
    [ -513,    -75,   -683], [ -973,    222,   -646], [ -616,   -843,   -388],
    [ -950,  -1113,   -359], [-1431,   -623,   -705], [-1398,  -1063,   -178],
    [  -45,   -461,     35], [   -9,   -657,   -216], [  127,  -1078,     95],
    [ -950,  -1156,    584], [-1480,  -1494,    449], [ -120,   -705,    516],
    [ -368,   -961,    727], [ -378,   -526,    973], [ -793,   -614,    676],
    [ -801,   -755,   1287], [-1476,   -340,   1636], [ -505,  -1254,   1543],
    [-1243,  -1622,   1532], [ -776,  -1477,   -655], [-1151,  -1296,   -823],
    [-1153,  -1672,  -1124], [-1291,  -2003,  -1702], [ -622,  -1283,     57],
    [ -471,  -1611,    509], [-1060,  -1570,   -139], [ -873,  -2156,   -536],
    [-1716,  -2021,   -364], [-2150,  -3218,  -1291], [-1248,  -1945,  -2904],
    [-1215,  -2633,  -2855], [  167,   -244,     84], [  349,   -412,   -217],
    [  -40,   -352,    632], [  227,   -529,    405], [   68,   -383,   -443],
    [  167,   -558,   -706], [ -275,   -854,    -14], [ -351,  -1089,   -449],
    [  341,    -72,   -289], [  603,   -106,   -474], [  322,   -219,   -649],
    [  179,   -317,   -998], [  450,   -291,   -996], [  555,    195,   -525],
    [  784,    272,   -831], [ -148,   -384,   -849], [   82,   -536,  -1357],
    [  238,   -172,  -1354], [  422,   -268,  -1841], [  297,   -737,  -2079],
    [ -111,   -801,   -598], [    1,   -668,   -984], [ -131,   -818,  -1299],
    [ -329,   -521,  -1310], [ -151,   -778,  -1834], [  -93,   -352,  -1746],
    [ -568,   -640,  -1821], [ -509,   -941,  -2183], [  464,   -815,  -1250],
    [   79,  -1133,  -1597], [ -184,  -1353,  -2123], [ -196,   -410,  -2427],
    [ -192,   -833,  -2810], [ -259,  -1382,  -3045], [ -217,      4,  -1166],
    [ -800,   -325,  -1219], [ -363,   -830,   -898], [ -661,  -1134,   -960],
    [ -386,   -980,  -1501], [ -627,  -1159,  -1722], [ -903,   -829,   -855],
    [ -685,   -829,  -1313], [-1065,   -959,  -1405], [  441,     25,   -847],
    [  655,    -27,  -1181], [ 1159,   -110,   -705], [  856,    253,  -1671],
    [  415,    404,     -1], [  322,    903,   -398], [  670,    499,   -292],
    [  803,    591,   -610], [ 1144,    591,   -814], [  717,    183,    393],
    [  857,    381,    106], [  609,     62,    -27], [  792,    198,   -325],
    [  735,    805,     88], [ 1142,    812,     78], [ 1028,    366,   -292],
    [ 1309,    743,   -237], [ 1615,    589,    -79], [ 1010,    639,   -243],
    [  999,    964,   -311], [ 1500,   1137,   -615], [  988,    357,    646],
    [ 1227,    667,    683], [ 1164,   1565,    894], [ 1392,   2015,    477],
    [ 1138,    533,    250], [ 1437,    896,    391], [ 1765,   1118,     99],
    [ 1112,   1090,    802], [ 1596,    846,   1134], [  937,   1161,    279],
    [ 1719,   1254,    683], [ 1338,   1086,     35], [ 1419,   1324,    428],
    [ 1428,   1524,     40], [ 2108,   1594,     89], [ 1015,    544,   1222],
    [ 1121,    925,   1263], [ 1030,   1318,   1485], [ 1295,    789,   1817],
    [ 1323,   1272,   1909], [ 1724,   1237,   1803], [ 1797,   1689,    858],
    [ 2149,   1367,   1301], [ 2302,   1867,    761], [ 2863,   2351,   1053],
    [   52,    163,    -76], [  230,    309,   -492], [  -71,    619,     39],
    [ -218,    856,    499], [ -654,    736,   -207], [ -535,   1259,    155],
    [ -480,   1476,    643], [  262,   1081,    102], [  309,   1592,   -182],
    [  627,   1629,    534], [  337,    643,    456], [  758,    670,    713],
    [  202,   1126,    658], [  612,   1131,    666], [  686,   1223,   1136],
    [ -131,    377,    525], [   42,    708,    907], [   87,   1488,   1035],
    [  432,   2117,    904], [  137,    981,   1332], [ -447,   1014,   1136],
    [ -839,   1793,   1246], [ -559,    297,    198], [ -850,    685,    446],
    [-1273,    632,    826], [ -401,   -544,    173], [ -753,   -793,    144],
    [ -436,     -9,    772], [ -115,   -243,   1310], [ -670,   -269,    374],
    [-1027,    -13,    639], [ -887,    -81,   1137], [-1277,   -455,    158],
    [-1411,   -720,    736], [  172,     88,    403], [  386,    255,    756],
    [ -500,    522,    910], [ -958,    659,   1388], [ -395,    301,   1344],
    [ -356,    768,   1813], [ -613,    841,   2419], [  445,   -122,    252],
    [  629,    -87,    723], [  283,   -253,    870], [  456,   -116,   1381],
    [  757,    180,   1059], [  532,    408,   1509], [  947,    288,   1806],
    [ 1325,    994,   2524], [  892,   1219,   3023], [ 1397,   1596,   3406],
    [ 1143,   1552,   2546], [ 1850,   1433,   2710], [  -10,    134,   1002],
    [  154,    499,   1323], [  508,    792,   1117], [  509,   1340,   1616],
    [  762,    862,   1608], [  787,    740,   2320], [  794,   1727,   1283],
    [  465,   2108,   1660], [ -120,   1451,   1613], [ -386,   2016,   2169],
    [  891,   1225,   2050], [  456,   1480,   2185], [ 1493,   1283,   1209],
    [ 1397,   1636,   1518], [ 1776,   1738,   1552], [ 1572,   1698,   2141],
    [ 1389,   2126,   1271], [ 1959,   2413,   1119], [ 1365,   2892,   1505],
    [ 2206,   1971,   1623], [ 2076,   1950,   2280], [ 1717,   2291,   1867],
    [ 2366,   2515,   1953], [ 2865,   2838,   2522], [ 2535,   3465,   2011],
    [ 3381,   4127,   2638], [  836,   2667,   2289], [ 1761,   2773,   2337],
    [ 1415,   3325,   2911], [ 2354,   3138,   3126], [ 2659,   4192,   4010],
    [ 1048,   1786,   1818], [ 1242,   2111,   2240], [ 1512,   2079,   2780],
    [ 1573,   2491,   3138], [ 2230,   2377,   2782], [  416,   1773,   2704],
    [  725,   2336,   3297], [ 1252,   2373,   3978], [ 2094,   2268,   3568],
    [ 2011,   2712,   4528], [ 1341,   3507,   3876], [ 1216,   3919,   4922],
    [ 1693,   4793,   6012]
];

const LSP_CODEBOOK1: [[i16; 3]; 256] = [
    [    0,      0,      0], [-2114,  -1302,     76], [-2652,  -1278,  -1368],
    [-2847,   -828,   -349], [-3812,  -2190,   -349], [-3946,   -364,   -449],
    [-2725,  -4492,  -3607], [-3495,  -4764,  -1744], [  -51,   -756,     84],
    [ -153,  -1191,    504], [  108,  -1418,   1167], [ -835,   -896,    390],
    [ -569,  -1702,     87], [-1151,  -1818,    933], [-1826,  -2547,    411],
    [-1842,  -1818,   1451], [-2438,  -1611,    781], [-2747,  -2477,   1311],
    [ -940,   1252,    477], [-1629,   1688,    602], [-1202,    617,    280],
    [-1737,    393,    580], [-1528,   1077,   1199], [-2165,   -161,   1408],
    [-2504,  -1087,   2371], [-3458,   -175,   1395], [-1397,    -98,   -843],
    [-2252,   -177,  -1149], [-1489,   -726,  -1283], [-1558,   -265,  -1744],
    [-1867,   -821,  -1897], [-2062,  -1516,  -2340], [-2595,  -1142,  -2861],
    [  170,     46,   -819], [ -193,   -204,  -1151], [  326,   -196,  -1532],
    [  780,    329,   -816], [  201,    369,  -1243], [  650,   -209,  -1060],
    [ 1144,    -15,  -1216], [ 1203,   -259,  -1867], [ -890,   -564,  -1430],
    [ -638,   -852,  -1921], [  177,   -739,  -1358], [ -261,   -526,  -1666],
    [  206,   -407,  -2255], [  338,   -526,   -822], [  421,  -1095,  -1009],
    [  765,   -607,  -1408], [  825,  -1295,  -2004], [  357,   -905,  -1815],
    [  -58,  -1248,  -1588], [ -596,  -1436,  -2046], [  -73,  -1159,  -2116],
    [ -115,  -1382,  -2581], [ -160,  -1723,  -1952], [   -6,  -2196,  -2954],
    [ -649,  -1705,  -2603], [ -617,  -1453,  -3282], [ -949,  -2019,  -3102],
    [ -812,   1544,   1937], [-1854,    574,   2000], [-1463,   1140,   2649],
    [-2683,   1748,   1452], [-2486,   2241,   2523], [  783,   1910,   1435],
    [  581,   2682,   1376], [  236,   2197,   1885], [ -453,   2943,   2057],
    [ -682,   2178,   2565], [-1342,   3201,   3328], [ -288,   -184,    262],
    [  121,   -149,   -183], [  758,   -412,    206], [ 1038,   -204,    853],
    [ 1577,   -457,    700], [  937,   -640,   -567], [ 1508,   -528,  -1024],
    [ -225,   -527,   -427], [ -564,  -1095,   -332], [ -742,   -353,   -186],
    [-1288,   -459,     84], [-1853,   -484,   -274], [-1554,   -731,    825],
    [-2425,   -234,    382], [-1722,    293,   -271], [-2515,    425,   -564],
    [-2599,    818,    464], [ -358,    118,   -375], [ -613,    198,   -874],
    [ -690,    683,   -324], [-1352,   1155,   -168], [-1093,    129,   -324],
    [-1184,    611,   -858], [  433,    386,   -372], [ -120,    486,   -634],
    [  234,    851,   -631], [  602,    128,     46], [ 1099,    410,    159],
    [  715,   -145,   -424], [ 1198,    -85,   -593], [ 1390,    367,   -358],
    [ 1683,    362,   -964], [ 1711,    622,     45], [ 2033,    833,   -383],
    [ 2890,    549,   -506], [    7,    401,     52], [   72,    811,    415],
    [  566,    668,     41], [  467,   1218,    130], [   68,    957,   -187],
    [  -25,   1649,   -103], [ -661,    260,    214], [ -925,    -94,    612],
    [ -321,   -422,    965], [ -788,   -672,   1783], [  400,   -673,    779],
    [  741,   -595,   1635], [ -161,    307,    657], [ -382,    836,    871],
    [ -814,    400,   1223], [  364,    606,   1247], [   57,     75,   1571],
    [  151,    471,   2287], [  -81,   1021,   1502], [  227,   1470,   1097],
    [  658,   1275,   1653], [  664,   1478,   2377], [  263,   -127,    444],
    [  264,     89,    969], [  794,    171,    576], [  821,    186,   1226],
    [  404,    462,    517], [  339,    918,    794], [ 1280,   1423,    196],
    [ 1453,   2019,    365], [ 1615,   1481,    672], [ 2394,   1708,    508],
    [  806,   1238,    573], [  713,   1158,   1078], [ 1285,   1436,   1232],
    [ 1790,   1188,   1141], [  765,    643,    864], [ 1032,    797,   1279],
    [  900,    563,   1827], [ 1514,    673,   2312], [ 1544,   1129,   3240],
    [ 1469,   1050,   1594], [ 1945,   1318,   1988], [ 2397,   2026,   2060],
    [ 3538,   2057,   2620], [ 1249,   -118,     74], [ 1727,    194,    421],
    [ 2078,    -50,   -463], [  970,    688,   -432], [ 1149,    952,   -110],
    [ 1254,   1275,   -651], [ 1386,    929,    401], [ 1960,   1167,    232],
    [  407,   -752,   -243], [  859,  -1118,    172], [ -227,   -860,   -992],
    [ -796,  -1175,  -1380], [    8,  -1282,   -388], [  353,  -1781,  -1037],
    [ -732,   -397,   -807], [ -853,    -28,  -1342], [-1229,  -1207,  -1959],
    [-1015,  -1125,  -2543], [-1452,  -1791,  -2725], [-1891,  -2416,  -3269],
    [ -918,  -1629,   -783], [ -580,  -2155,   -698], [-1097,  -2364,    -96],
    [-1387,  -1513,      7], [-1588,  -2076,   -664], [-1473,  -2740,   -784],
    [-2378,  -3149,    -56], [-2856,  -2092,   -169], [-3391,  -3708,    316],
    [-1176,   -890,   -614], [-1944,  -1061,   -800], [ -299,  -1517,  -1000],
    [ -640,  -1850,  -1526], [-1454,  -1536,  -1233], [-1890,  -1955,  -1756],
    [-1086,  -1921,  -2122], [ -750,  -2325,  -2260], [-1325,  -2413,  -2673],
    [-1114,  -2542,  -3459], [-1341,  -2901,  -3963], [-1160,  -2226,  -1393],
    [-1001,  -2772,  -1573], [-1594,  -2641,  -1978], [-1534,  -3046,  -2624],
    [-2224,  -2196,   -675], [-2807,  -3054,  -1102], [-2008,  -2840,  -1186],
    [-1980,  -3332,  -1695], [-1715,  -3562,   -505], [-2527,  -4000,  -1887],
    [-2333,  -2734,  -2296], [-3440,  -2401,  -3211], [-2008,  -3528,  -3337],
    [-2247,  -3291,  -4510], [ -475,    949,    155], [ -149,   1365,    545],
    [ -757,   1644,   1083], [ -217,   2053,   1353], [-1433,   2301,   1462],
    [  495,   1661,    529], [   10,   2037,    740], [ 2082,   1898,    978],
    [ 2831,   2294,    911], [  842,    793,    420], [ 1223,   1023,    863],
    [ 1237,    451,    780], [ 1744,    708,    822], [ 1533,    284,   1384],
    [ 2135,    609,   1538], [ 2305,    626,    540], [ 2368,   1187,    955],
    [ 2586,   1255,     -7], [ 3116,   1131,    726], [ 3431,   1730,    428],
    [ 2734,   1648,   1307], [ 2988,   1231,   2010], [ 3523,   2024,   1488],
    [ 1034,   1657,    871], [ 1206,   2163,   1036], [ 1807,   2372,   1233],
    [ 1808,   1769,   1493], [ 1573,   2332,   1779], [ 1216,   1609,   1866],
    [ 1480,   1898,   2513], [  465,   2708,   2776], [  771,   3638,   3338],
    [ 1869,   2599,   2623], [ 2825,   2745,   2468], [ 2638,   2439,   1585],
    [ 2094,   2970,   1308], [ 2022,   3057,   1999], [ 3428,   2912,   1816],
    [ 4536,   2974,   2129], [ 1046,   2563,   2086], [ 1363,   3562,   2318],
    [ 2511,   1891,   2984], [ 1866,   2306,   3986], [ 3272,   2924,   3682],
    [ 3146,   3564,   2272], [ 3592,   3968,   2822], [ 2431,   3369,   3069],
    [ 1931,   4709,   3090], [ 2629,   4220,   3986], [ 4639,   4056,   3664],
    [ 4035,   5334,   4912]
];

const LSP_CODEBOOK2: [[i16; 4]; 256] = [
    [    0,      0,      0,      0], [  601,    512,   -542,    334],
    [  428,   1087,   -484,   -132], [  652,    622,   -391,   -572],
    [  378,    799,    141,   -860], [ 1040,    409,    112,   -554],
    [ 1123,    670,    -75,   -847], [ 1421,    494,   -315,  -1095],
    [  787,   1001,    114,   -460], [  988,   1672,    216,   -681],
    [ 1007,   1241,   -132,  -1247], [ 1073,    399,    186,     -5],
    [ 1262,    193,   -694,   -129], [  325,    196,     51,   -641],
    [  861,    -59,    350,   -458], [ 1261,    567,    586,   -346],
    [ 1532,    885,    210,   -517], [ 2027,    937,    113,   -792],
    [ 1383,   1064,    334,     38], [ 1964,   1468,    459,    133],
    [ 2062,   1186,    -98,   -121], [ 2577,   1445,    506,   -373],
    [ 2310,   1682,     -2,   -960], [ 2876,   1939,    765,    138],
    [ 3581,   2360,    649,   -414], [  219,    176,   -398,   -309],
    [  434,    -78,   -435,   -880], [ -344,    301,    265,   -552],
    [ -915,    470,    657,   -380], [  419,   -432,   -163,   -453],
    [  351,   -953,      8,   -562], [  789,    -43,     20,   -958],
    [  302,   -594,   -352,  -1159], [ 1040,    108,   -668,   -924],
    [ 1333,    210,  -1217,  -1663], [  483,    589,   -350,  -1140],
    [ 1003,    824,   -802,  -1184], [  745,     58,   -589,  -1443],
    [  346,    247,   -915,  -1683], [  270,    796,   -720,  -2043],
    [ 1208,    722,   -222,   -193], [ 1486,   1180,   -412,   -672],
    [ 1722,    179,    -69,   -521], [ 2047,    860,   -666,  -1410],
    [ -146,    222,   -281,   -805], [ -189,     90,   -114,  -1307],
    [ -152,   1086,   -241,   -764], [ -439,    733,   -601,  -1302],
    [ -833,   -167,   -351,   -601], [ -856,   -422,   -411,  -1059],
    [ -747,   -355,   -582,  -1644], [ -837,    210,   -916,  -1144],
    [-1800,     32,   -878,  -1687], [  -48,    -23,  -1146,     52],
    [ -350,   -409,  -1656,   -364], [  265,   -728,   -858,   -577],
    [  458,   -247,  -1141,   -997], [  691,   -407,  -1988,  -1161],
    [  -66,   -104,   -705,  -1249], [ -431,    -93,  -1191,  -1844],
    [  203,   -732,  -1000,  -1693], [   10,   -832,  -1846,  -1819],
    [  493,   -128,  -1436,  -1768], [  488,   -311,  -1730,  -2540],
    [ -653,   -532,  -1150,  -1172], [-1086,   -289,  -1706,  -1533],
    [ -699,  -1205,  -1216,  -1766], [-1032,  -1481,  -2074,  -1523],
    [ -721,  -1220,  -2277,  -2600], [   12,   -539,  -1484,  -1131],
    [  -40,   -911,  -2106,   -441], [ -471,   -484,  -2267,  -1549],
    [ -141,   -988,  -3006,  -1721], [-1545,  -2102,   -583,    342],
    [-1383,  -2772,   -386,    -13], [-2118,  -2589,  -1205,     72],
    [-2147,  -3231,   -965,    390], [-2949,  -3300,   -621,    637],
    [-3907,  -4138,   -865,    803], [-1287,   -845,   -375,   -548],
    [-1416,  -1169,   -487,  -1277], [-1400,  -1690,  -1027,   -418],
    [-2018,  -1909,  -1188,  -1260], [-1418,  -2222,  -2029,   -128],
    [-2067,  -2998,  -2693,   -310], [ -950,  -1028,  -1538,    185],
    [-1616,   -915,  -2205,   -549], [   19,   -821,  -1145,    352],
    [  184,  -1175,  -1356,   -627], [ -547,  -1088,  -1661,   -911],
    [ -216,  -1502,  -2197,   -948], [ -795,  -1306,  -2374,   -451],
    [ -924,  -1889,  -2796,   -680], [ -600,  -1614,  -3609,   -885],
    [-2392,  -2528,    319,    303], [-2908,  -2095,   -310,    573],
    [-3460,  -2141,     49,   -113], [-2231,   -448,    675,   -146],
    [-2805,   -532,   1231,    479], [-2684,   -486,   -200,    611],
    [-3525,   -971,   -198,    704], [-3707,    173,    349,    254],
    [-4734,  -1447,    -34,    880], [  777,   -512,    114,    -10],
    [ 1250,    -66,    442,     -5], [  604,    613,    452,   -352],
    [ 1224,    777,    675,  -1014], [-1372,    -79,  -1208,   -238],
    [-2389,    -17,  -1157,   -818], [-1504,   -673,  -1133,  -1060],
    [-1984,   -799,  -2005,  -1973], [-2037,   -798,  -1068,   -105],
    [-3190,   -899,  -1817,   -194], [ -156,   -886,    394,   -318],
    [ -258,  -1283,    551,    202], [ -536,  -1729,    910,    331],
    [ -847,  -1109,    795,   -163], [-1171,  -1128,    715,    519],
    [-1080,  -1319,   1685,    668], [-1000,  -1921,     96,    211],
    [-1487,  -2148,    831,    174], [-1139,   -374,    414,     -4],
    [-1517,  -1383,    396,   -352], [-1012,    439,    -59,   -967],
    [-1812,    706,   -440,  -1030], [-1971,   -329,    -34,   -827],
    [-2472,  -1588,   -151,   -606], [-2161,    374,   -281,     76],
    [-3012,    231,    -15,   -690], [ 1104,    566,    721,    209],
    [ 1685,    564,    383,     98], [ 1898,    750,    792,    -97],
    [  556,    -64,    561,    -93], [  876,    162,    913,    -22],
    [  961,    675,   1296,    140], [  756,   -396,    851,    544],
    [  360,   -303,   1341,    396], [  878,    -22,   1464,    863],
    [ -309,   -273,    642,   -129], [ -686,    -82,    842,    454],
    [   -5,    -47,   1069,    998], [  -94,    967,   1277,    298],
    [ -489,    385,   1473,    746], [ -369,   -717,   1333,    242],
    [  281,   -993,   1726,    924], [  464,    601,   1575,   1376],
    [ -250,    206,   2339,   1175], [ -438,    377,   -597,   -285],
    [-1020,    787,   -790,   -287], [ -458,   -410,    215,    295],
    [ -589,   -860,   -121,    797], [-1175,    122,   -437,    466],
    [-1480,   -121,    367,    924], [  234,    323,    770,   -555],
    [  145,     30,    996,     26], [   66,    849,     93,   -145],
    [ -117,   1261,    474,   -399], [-1495,   1051,    218,   -506],
    [-1390,    694,    994,     88], [  616,      7,     78,    304],
    [ 1060,     52,    -62,    835], [  833,    454,    649,   1359],
    [ -770,    464,     47,     93], [ -574,   1199,    -39,    379],
    [  114,    -98,    488,    485], [  727,    244,    606,    696],
    [  -76,    455,    671,    546], [ -565,    -13,    145,    819],
    [ -376,    569,    448,   1128], [  218,    122,    265,   1167],
    [  230,    738,    932,   1003], [  138,    477,     36,    450],
    [  404,    787,    -73,   1000], [  497,   1259,    387,   1231],
    [   17,    207,    195,    -79], [  562,    358,     53,   -158],
    [  493,    387,    478,    189], [  678,    831,    640,    558],
    [ -197,    523,    613,     57], [  429,    894,    769,    111],
    [   67,   1174,    568,    511], [ 1242,    824,    251,    840],
    [ 1419,   1074,    864,    481], [  924,   1474,    669,    724],
    [ 1539,   1879,    654,   1590], [  445,    337,   1111,    541],
    [  472,   1421,   1264,   1094], [  794,    735,   1103,    668],
    [ 1055,    863,   1192,   1020], [  778,   1105,    806,   1798],
    [ 1052,   1527,   1587,   2151], [  881,   1552,   1265,    391],
    [  726,    872,   1812,    601], [ 1469,    280,   1008,    616],
    [ 1403,    577,   1803,   1244], [ 1650,   1314,   1148,   1072],
    [ 1297,   1669,   1911,   1026], [ 2093,   1044,   2115,   1189],
    [ 1644,   1961,   2587,   1512], [   25,   -315,     -9,   -106],
    [  290,   -339,    428,   -444], [  -68,   -783,    735,    772],
    [  245,   -555,    468,     47], [  334,   -895,    814,    146],
    [  235,    368,   -964,   -959], [ -203,    315,  -1566,  -1217],
    [  801,     17,   -276,   -354], [  894,   -495,   -789,   -635],
    [  716,    291,  -1189,   -357], [  560,   -260,   -733,     -2],
    [  679,   -508,  -1429,    211], [  -51,    -62,   -428,    557],
    [  322,   -638,   -211,    614], [ -878,  -1057,    -84,    -71],
    [ -388,  -1415,   -167,   -318], [ -754,  -1574,    214,   -539],
    [-1419,  -2004,    -92,   -787], [  -47,   -856,   -347,   -255],
    [   23,  -1211,   -173,    320], [ -658,   -487,   -893,    353],
    [ -783,  -1587,   -584,    507], [-1420,   -859,   -378,    441],
    [-2095,  -1491,   -137,    439], [ -321,  -1450,  -1288,    -12],
    [ -359,  -2113,   -553,     -8], [ -831,  -1918,  -1561,     32],
    [-1014,  -2487,  -1359,   -939], [ -475,   -311,   -169,   -236],
    [ -907,   -426,    276,   -611], [  -96,   -400,     50,   -710],
    [ -426,  -1022,    -10,   -985], [ -197,   -258,   -744,   -575],
    [ -611,   -930,   -771,   -394], [ -267,   -776,   -612,   -939],
    [ -256,  -1346,   -802,  -1122], [ -796,  -1570,   -825,   -754],
    [  712,    876,    141,    227], [  981,   1509,     85,    124],
    [ 1462,   1228,    979,    -39], [ 1734,    999,   1481,    440],
    [ 2293,   1116,    769,    440], [ 2504,   1480,   1241,    356],
    [ 2474,   1909,   1558,    810], [  917,   1134,    607,   -134],
    [  509,   1809,    781,   -123], [ 1712,   1506,    559,   -423],
    [ 2037,   2317,    726,   -155], [ 3031,   2676,   1203,    331],
    [ 3664,   3274,   1768,    531], [ 1610,   1839,    867,    183],
    [ 1774,   1972,   1538,     97], [ 1822,   2158,   1282,    659],
    [ 2222,   2758,   1818,    900], [ 3251,   2124,   1723,    996],
    [ 3633,   2336,   2408,   1453], [ 2923,   3517,   2567,   1318],
];

const COS_TAB: [i16; 513] = [
    16384,  16383,  16379,  16373,  16364,  16353,  16340,  16324,
    16305,  16284,  16261,  16235,  16207,  16176,  16143,  16107,
    16069,  16029,  15986,  15941,  15893,  15843,  15791,  15736,
    15679,  15619,  15557,  15493,  15426,  15357,  15286,  15213,
    15137,  15059,  14978,  14896,  14811,  14724,  14635,  14543,
    14449,  14354,  14256,  14155,  14053,  13949,  13842,  13733,
    13623,  13510,  13395,  13279,  13160,  13039,  12916,  12792,
    12665,  12537,  12406,  12274,  12140,  12004,  11866,  11727,
    11585,  11442,  11297,  11151,  11003,  10853,  10702,  10549,
    10394,  10238,  10080,   9921,   9760,   9598,   9434,   9269,
     9102,   8935,   8765,   8595,   8423,   8250,   8076,   7900,
     7723,   7545,   7366,   7186,   7005,   6823,   6639,   6455,
     6270,   6084,   5897,   5708,   5520,   5330,   5139,   4948,
     4756,   4563,   4370,   4176,   3981,   3786,   3590,   3393,
     3196,   2999,   2801,   2603,   2404,   2205,   2006,   1806,
     1606,   1406,   1205,   1005,    804,    603,    402,    201,
        0,   -201,   -402,   -603,   -804,  -1005,  -1205,  -1406,
    -1606,  -1806,  -2006,  -2205,  -2404,  -2603,  -2801,  -2999,
    -3196,  -3393,  -3590,  -3786,  -3981,  -4176,  -4370,  -4563,
    -4756,  -4948,  -5139,  -5330,  -5520,  -5708,  -5897,  -6084,
    -6270,  -6455,  -6639,  -6823,  -7005,  -7186,  -7366,  -7545,
    -7723,  -7900,  -8076,  -8250,  -8423,  -8595,  -8765,  -8935,
    -9102,  -9269,  -9434,  -9598,  -9760,  -9921, -10080, -10238,
   -10394, -10549, -10702, -10853, -11003, -11151, -11297, -11442,
   -11585, -11727, -11866, -12004, -12140, -12274, -12406, -12537,
   -12665, -12792, -12916, -13039, -13160, -13279, -13395, -13510,
   -13623, -13733, -13842, -13949, -14053, -14155, -14256, -14354,
   -14449, -14543, -14635, -14724, -14811, -14896, -14978, -15059,
   -15137, -15213, -15286, -15357, -15426, -15493, -15557, -15619,
   -15679, -15736, -15791, -15843, -15893, -15941, -15986, -16029,
   -16069, -16107, -16143, -16176, -16207, -16235, -16261, -16284,
   -16305, -16324, -16340, -16353, -16364, -16373, -16379, -16383,
   -16384, -16383, -16379, -16373, -16364, -16353, -16340, -16324,
   -16305, -16284, -16261, -16235, -16207, -16176, -16143, -16107,
   -16069, -16029, -15986, -15941, -15893, -15843, -15791, -15736,
   -15679, -15619, -15557, -15493, -15426, -15357, -15286, -15213,
   -15137, -15059, -14978, -14896, -14811, -14724, -14635, -14543,
   -14449, -14354, -14256, -14155, -14053, -13949, -13842, -13733,
   -13623, -13510, -13395, -13279, -13160, -13039, -12916, -12792,
   -12665, -12537, -12406, -12274, -12140, -12004, -11866, -11727,
   -11585, -11442, -11297, -11151, -11003, -10853, -10702, -10549,
   -10394, -10238, -10080,  -9921,  -9760,  -9598,  -9434,  -9269,
    -9102,  -8935,  -8765,  -8595,  -8423,  -8250,  -8076,  -7900,
    -7723,  -7545,  -7366,  -7186,  -7005,  -6823,  -6639,  -6455,
    -6270,  -6084,  -5897,  -5708,  -5520,  -5330,  -5139,  -4948,
    -4756,  -4563,  -4370,  -4176,  -3981,  -3786,  -3590,  -3393,
    -3196,  -2999,  -2801,  -2603,  -2404,  -2205,  -2006,  -1806,
    -1606,  -1406,  -1205,  -1005,   -804,   -603,   -402,   -201,
        0,    201,    402,    603,    804,   1005,   1205,   1406,
     1606,   1806,   2006,   2205,   2404,   2603,   2801,   2999,
     3196,   3393,   3590,   3786,   3981,   4176,   4370,   4563,
     4756,   4948,   5139,   5330,   5520,   5708,   5897,   6084,
     6270,   6455,   6639,   6823,   7005,   7186,   7366,   7545,
     7723,   7900,   8076,   8250,   8423,   8595,   8765,   8935,
     9102,   9269,   9434,   9598,   9760,   9921,  10080,  10238,
    10394,  10549,  10702,  10853,  11003,  11151,  11297,  11442,
    11585,  11727,  11866,  12004,  12140,  12274,  12406,  12537,
    12665,  12792,  12916,  13039,  13160,  13279,  13395,  13510,
    13623,  13733,  13842,  13949,  14053,  14155,  14256,  14354,
    14449,  14543,  14635,  14724,  14811,  14896,  14978,  15059,
    15137,  15213,  15286,  15357,  15426,  15493,  15557,  15619,
    15679,  15736,  15791,  15843,  15893,  15941,  15986,  16029,
    16069,  16107,  16143,  16176,  16207,  16235,  16261,  16284,
    16305,  16324,  16340,  16353,  16364,  16373,  16379,  16383,
    16384
];

const FIXED_CB_GAIN: [i16; 24] = [
      1,    2,    3,    4,    6,    9,   13,   18,
     26,   38,   55,   80,  115,  166,  240,  348,
    502,  726, 1050, 1517, 2193, 3170, 4582, 6623
];

const ACB_GAIN170: [[i16; 20]; 170] = [
  [
         0,      0,      0,      0,      0,      0,      0,      0,      0,      0,
         0,      0,      0,      0,      0,      0,      0,      0,      0,      0
  ], [
       776,    212,    715,    670,    809,    -36,     -2,    -31,    -27,    -39,
       -10,    -33,     -9,    -31,     -8,    -29,    -38,    -10,    -35,    -33
  ], [
      1296,   1316,   -168,   -320,   -815,   -102,   -105,     -1,     -6,    -40,
      -104,     13,     13,     25,     25,     -3,     64,     65,     -8,    -15
  ], [
      -589,    680,   2478,    308,   -596,    -21,    -28,   -375,     -5,    -21,
        24,     89,   -102,     11,    -12,    -46,    -21,     24,     90,     11
  ], [
      -735,   -487,     -5,   2948,    468,    -33,    -14,      0,   -530,    -13,
       -21,      0,      0,    132,     87,      0,     21,     13,      0,    -84
  ], [
      1042,   1730,   1068,    333,    626,    -66,   -182,    -69,     -6,    -23,
      -110,    -67,   -112,    -21,    -35,    -21,    -39,    -66,    -40,    -12
  ], [
       486,   -769,   4074,   2825,  -1107,    -14,    -36,  -1013,   -487,    -74,
        22,   -120,    191,    -83,    132,   -702,     32,    -52,    275,    191
  ], [
      1521,   -767,   -124,   4320,   1026,   -141,    -35,      0,  -1139,    -64,
        71,     11,     -5,   -401,    202,     32,    -95,     48,      7,   -270
  ], [
      2425,   1267,   3439,    -91,  -1166,   -359,    -98,   -722,      0,    -83,
      -187,   -509,   -266,     13,      7,     19,    172,     90,    244,     -6
  ], [
     -1251,    975,    173,   4039,   2005,    -95,    -58,     -1,   -996,   -245,
        74,     13,    -10,    308,   -240,    -42,    153,   -119,    -21,   -494
  ], [
      1820,    632,   1322,   2062,   1031,   -202,    -24,   -106,   -259,    -64,
       -70,   -146,    -51,   -229,    -79,   -166,   -114,    -39,    -83,   -129
  ], [
      -447,   4904,    244,   -315,  -2038,    -12,  -1467,     -3,     -6,   -253,
       134,      6,    -73,     -8,     94,      4,    -55,    610,     30,    -39
  ], [
      -208,  -1102,    463,   -448,   5653,     -2,    -74,    -13,    -12,  -1950,
       -14,      5,     31,     -5,    -30,     12,     71,    380,   -159,    154
  ], [
      4739,   2600,  -1864,    856,  -1554,  -1371,   -412,   -212,    -44,   -147,
      -752,    539,    295,   -247,   -135,     97,    449,    246,   -176,     81
  ], [
      1894,   3533,     35,    -26,   2145,   -219,   -762,      0,      0,   -280,
      -408,     -4,     -7,      3,      5,      0,   -248,   -462,     -4,      3
  ], [
     -2699,   1841,   4072,   2443,   1582,   -444,   -207,  -1012,   -364,   -152,
       303,    670,   -457,    402,   -274,   -607,    260,   -177,   -393,   -236
  ], [
      -844,   3358,   6106,  -1059,   -537,    -43,   -688,  -2275,    -68,    -17,
       173,    314,  -1251,    -54,    217,    395,    -27,    110,    200,    -34
  ], [
      1251,   1016,   3020,   2210,   1445,    -95,    -63,   -556,   -298,   -127,
       -77,   -230,   -187,   -168,   -137,   -407,   -110,    -89,   -266,   -194
  ], [
      2099,   2277,   4038,   3533,  -2870,   -269,   -316,   -995,   -762,   -503,
      -291,   -517,   -561,   -452,   -491,   -871,    367,    399,    707,    619
  ], [
       400,  -1114,   8516,   2422,  -1117,     -9,    -75,  -4426,   -358,    -76,
        27,   -208,    579,    -59,    164,  -1259,     27,    -75,    580,    165
  ], [
     -4398,  -2011,   3912,  -2407,   2258,  -1180,   -247,   -934,   -353,   -311,
      -540,   1050,    480,   -646,   -295,    575,    606,    277,   -539,    331
  ], [
      1767,  -1447,   4240,   6160,   -757,   -190,   -127,  -1097,  -2316,    -35,
       156,   -457,    374,   -664,    544,  -1594,     81,    -66,    195,    284
  ], [
      1594,  -1463,   1035,   6938,   1920,   -155,   -130,    -65,  -2938,   -225,
       142,   -100,     92,   -675,    619,   -438,   -186,    171,   -121,   -813
  ], [
      -562,   4716,   4085,   -591,   2421,    -19,  -1357,  -1018,    -21,   -357,
       162,    140,  -1175,    -20,    170,    147,     83,   -696,   -603,     87
  ], [
      1552,   8778,   -935,    354,  -1424,   -147,  -4703,    -53,     -7,   -123,
      -831,     88,    501,    -33,   -189,     20,    134,    763,    -81,     30
  ], [
      4831,  -4431,     41,  -1479,  -2976,  -1424,  -1198,      0,   -133,   -540,
      1306,    -12,     11,    436,   -400,      3,    877,   -804,      7,   -268
  ], [
      2090,   1192,   1006,   1645,   4853,   -266,    -86,    -61,   -165,  -1437,
      -152,   -128,    -73,   -210,   -119,   -101,   -619,   -353,   -298,   -487
  ], [
      2386,   5712,   1426,    -94,   1350,   -347,  -1991,   -124,      0,   -111,
      -832,   -207,   -497,     13,     32,      8,   -196,   -470,   -117,      7
  ], [
     -1349,   1091,   1659,   8891,    313,   -111,    -72,   -168,  -4825,     -5,
        89,    136,   -110,    732,   -592,   -900,     25,    -20,    -31,   -170
  ], [
      9980,    916,   -381,   -808,     88,  -6080,    -51,     -8,    -39,      0,
      -558,    232,     21,    492,     45,    -18,    -53,     -4,      2,      4
  ], [
      2338,  -1031,   -248,   3928,   6484,   -333,    -64,     -3,   -942,  -2566,
       147,     35,    -15,   -560,    247,     59,   -925,    408,     98,  -1555
  ], [
      6166,  -1240,   -337,   3672,  -1277,  -2320,    -93,     -6,   -823,    -99,
       466,    126,    -25,  -1382,    278,     75,    480,    -96,    -26,    286
  ], [
      4377,   -132,  -2588,   1701,   4865,  -1169,     -1,   -409,   -176,  -1444,
        35,    691,    -20,   -454,     13,    268,  -1299,     39,    768,   -505
  ], [
      2594,   3295,   3944,   1481,    682,   -410,   -662,   -949,   -133,    -28,
      -521,   -624,   -793,   -234,   -297,   -356,   -108,   -137,   -164,    -61
  ], [
      4151,    624,    815,   4485,   2229,  -1052,    -23,    -40,  -1228,   -303,
      -158,   -206,    -31,  -1136,   -170,   -223,   -565,    -84,   -111,   -610
  ], [
     -3575,   -361,   4924,   2791,   4698,   -780,     -7,  -1480,   -475,  -1347,
       -78,   1074,    108,    609,     61,   -839,   1025,    103,  -1412,   -800
  ], [
     -2518,   3791,   8623,    315,   2465,   -387,   -877,  -4538,     -6,   -370,
       582,   1325,  -1995,     48,    -73,   -166,    378,   -570,  -1297,    -47
  ], [
      -691,   2989,   9957,   -421,  -1142,    -29,   -545,  -6051,    -10,    -79,
       126,    420,  -1817,    -17,     76,    256,    -48,    208,    694,    -29
  ], [
     -1918,    104,  -3190,  -3410,  -4440,   -224,      0,   -621,   -709,  -1203,
        12,   -373,     20,   -399,     21,   -664,   -519,     28,   -864,   -924
  ], [
     -3359,  -1668,   1854,   6939,   1430,   -688,   -169,   -209,  -2939,   -124,
      -341,    380,    188,   1422,    706,   -785,    293,    145,   -161,   -606
  ], [
        42,   9706,   3164,   -952,    907,      0,  -5750,   -611,    -55,    -50,
       -25,     -8,  -1874,      2,    564,    183,     -2,   -537,   -175,     52
  ], [
      1607,    785,   2862,   4327,   3307,   -157,    -37,   -500,  -1143,   -667,
       -77,   -280,   -137,   -424,   -207,   -756,   -324,   -158,   -577,   -873
  ], [
      6801,   3416,   2227,   1682,  -3217,  -2823,   -712,   -302,   -172,   -631,
     -1418,   -924,   -464,   -698,   -350,   -228,   1335,    670,    437,    330
  ], [
      3459,   3898,    364,   7841,  -2640,   -730,   -927,     -8,  -3753,   -425,
      -823,    -76,    -86,  -1655,  -1865,   -174,    557,    628,     58,   1263
  ], [
     -5902,  -3458,  -2465,  -1886,   4334,  -2126,   -730,   -371,   -217,  -1146,
     -1245,   -888,   -520,   -679,   -398,   -283,   1561,    915,    652,    499
  ], [
     -3710,   1133,   7849,   3443,   -215,   -840,    -78,  -3760,   -723,     -2,
       256,   1777,   -543,    779,   -238,  -1649,    -48,     14,    103,     45
  ], [
      4132,   2828,      2,  -4212,  -4116,  -1042,   -488,      0,  -1083,  -1034,
      -713,      0,      0,   1062,    727,      0,   1038,    710,      0,  -1058
  ], [
      5875,   8496,  -1796,   1376,  -1786,  -2107,  -4406,   -197,   -115,   -194,
     -3047,    644,    931,   -493,   -713,    150,    640,    926,   -195,    150
  ], [
      3143,   3483,   3546,   -793,   4489,   -603,   -740,   -767,    -38,  -1230,
      -668,   -680,   -754,    152,    168,    171,   -861,   -954,   -971,    217
  ], [
      2845,   7965,   3695,  -5432,   3978,   -494,  -3873,   -833,  -1801,   -966,
     -1383,   -641,  -1796,    943,   2641,   1225,   -691,  -1934,   -897,   1319
  ], [
      1538,    150,   7139,   2049,   3097,   -144,     -1,  -3110,   -256,   -585,
       -14,   -670,    -65,   -192,    -18,   -892,   -290,    -28,  -1349,   -387
  ], [
       618,   7520,   4729,   -238,  -3373,    -23,  -3452,  -1365,     -3,   -694,
      -283,   -178,  -2170,      8,    109,     68,    127,   1548,    973,    -49
  ], [
      2965,  -3013,   7912,   7076,  -1997,   -536,   -554,  -3821,  -3056,   -243,
       545,  -1431,   1455,  -1280,   1301,  -3417,    361,   -367,    964,    862
  ], [
      2443,   -929,  -1113,   9677,   4138,   -364,    -52,    -75,  -5716,  -1045,
       138,    166,    -63,  -1443,    549,    657,   -617,    234,    281,  -2444
  ], [
      1966,   3309,  10085,  -3399,   2105,   -236,   -668,  -6207,   -705,   -270,
      -397,  -1210,  -2037,    408,    686,   2092,   -252,   -425,  -1295,    436
  ], [
      -112,  -1368,   8868,   4822,   2048,      0,   -114,  -4800,  -1419,   -256,
        -9,     61,    740,     33,    402,  -2610,     14,    171,  -1108,   -602
  ], [
     -2597,    438,  -1839,   6229,   7266,   -411,    -11,   -206,  -2368,  -3223,
        69,   -291,     49,    987,   -166,    699,   1152,   -194,    816,  -2763
  ], [
      3454,    553,   9127,   4946,  -5596,   -728,    -18,  -5084,  -1493,  -1911,
      -116,  -1924,   -308,  -1042,   -166,  -2755,   1179,    188,   3117,   1689
  ], [
      -532,   -663,  12262,   2495,  -1004,    -17,    -26,  -9177,   -380,    -61,
       -21,    398,    496,     81,    101,  -1867,    -32,    -40,    751,    152
  ], [
     -2100,   1317,  -1509,  11425,   2997,   -269,   -105,   -139,  -7967,   -548,
       168,   -193,    121,   1464,   -918,   1052,    384,   -240,    276,  -2090
  ], [
      1193,  -2697,  11259,   5373,   -763,    -86,   -444,  -7737,  -1762,    -35,
       196,   -819,   1853,   -391,    884,  -3692,     55,   -125,    525,    250
  ], [
      2405,   -471,  11079,    203,    782,   -353,    -13,  -7491,     -2,    -37,
        69,  -1626,    318,    -29,      5,   -137,   -114,     22,   -529,     -9
  ], [
     -1871,   5685,  11290,  -2662,   1353,   -213,  -1972,  -7780,   -432,   -111,
       649,   1289,  -3917,   -304,    923,   1834,    154,   -469,   -932,    220
  ], [
     -3768,   5927,  -3093,   5041,   5212,   -866,  -2144,   -584,  -1551,  -1658,
      1363,   -711,   1119,   1159,  -1824,    951,   1198,  -1885,    984,  -1603
  ], [
     -2546,   9502,   5969,  -2440,   1928,   -395,  -5511,  -2175,   -363,   -226,
      1477,    927,  -3462,   -379,   1415,    889,    299,  -1118,   -702,    287
  ], [
     -4963,   3568,   4592,   5508,   3451,  -1503,   -777,  -1287,  -1851,   -727,
      1080,   1391,  -1000,   1668,  -1199,  -1543,   1045,   -751,   -967,  -1160
  ], [
      1745,  -2586,   3983,  10899,  -1551,   -186,   -408,   -968,  -7250,   -146,
       275,   -424,    628,  -1161,   1720,  -2649,    165,   -244,    377,   1032
  ], [
       867,   -456,   -727,   3369,  11822,    -45,    -12,    -32,   -692,  -8531,
        24,     38,    -20,   -178,     93,    149,   -625,    329,    525,  -2431
  ], [
      7535,   2422,   1926,   1405,   1599,  -3466,   -358,   -226,   -120,   -156,
     -1114,   -886,   -284,   -646,   -207,   -165,   -735,   -236,   -188,   -137
  ], [
      1041,   -735,   -142,  13209,   1515,    -66,    -33,     -1, -10649,   -140,
        46,      9,     -6,   -839,    593,    114,    -96,     68,     13,  -1222
  ], [
      7950,   6745,  -1444,  -1008,   2721,  -3857,  -2777,   -127,    -62,   -452,
     -3273,    700,    594,    489,    415,    -88,  -1320,  -1120,    239,    167
  ], [
     -4754,  -1379,   4522,   -578,  -5733,  -1379,   -116,  -1248,    -20,  -2006,
      -400,   1312,    380,   -167,    -48,    159,  -1663,   -482,   1582,   -202
  ], [
      3220,   5978,   5923,   2430,  -2689,   -633,  -2181,  -2141,   -360,   -441,
     -1175,  -1164,  -2161,   -477,   -886,   -878,    528,    981,    972,    398
  ], [
       377,   1312,  13978,  -1470,    677,     -8,   -105, -11925,   -132,    -28,
       -30,   -321,  -1119,     33,    117,   1254,    -15,    -54,   -577,     60
  ], [
     -3435,   6770,    314,   -885,   5686,   -720,  -2797,     -6,    -47,  -1973,
      1419,     65,   -129,   -185,    366,     16,   1192,  -2349,   -109,    307
  ], [
      3171,   8774,  -2260,   2679,   3069,   -613,  -4699,   -312,   -438,   -575,
     -1698,    437,   1210,   -518,  -1435,    369,   -594,  -1643,    423,   -501
  ], [
      5557,   1509,   5407,   -125,  -7386,  -1884,   -139,  -1784,      0,  -3330,
      -511,  -1834,   -498,     42,     11,     41,   2505,    680,   2438,    -56
  ], [
     -2838,   2595,  13228,    271,   1793,   -491,   -411, -10680,     -4,   -196,
       449,   2291,  -2095,     47,    -42,   -219,    310,   -284,  -1447,    -29
  ], [
       664,   -278,  14966,    951,   -711,    -26,     -4, -13672,    -55,    -30,
        11,   -606,    253,    -38,     16,   -869,     28,    -12,    650,     41
  ], [
       808,   1770,   8658,   5863,  -1486,    -39,   -191,  -4576,  -2098,   -134,
       -87,   -427,   -935,   -289,   -633,  -3098,     73,    160,    785,    531
  ], [
      3063,   1539,   2000,   -542,   9576,   -572,   -144,   -244,    -17,  -5597,
      -287,   -374,   -188,    101,     51,     66,  -1790,   -900,  -1169,    317
  ], [
       514,  14083,   -323,    896,   -891,    -16, -12106,     -6,    -49,    -48,
      -442,     10,    277,    -28,   -770,     17,     27,    766,    -17,     48
  ], [
       892,    158,   5237,  11057,  -1603,    -48,     -1,  -1674,  -7462,   -156,
        -8,   -285,    -50,   -602,   -106,  -3534,     87,     15,    512,   1082
  ], [
     -1612,   2564,  -4296,  12526,   5710,   -158,   -401,  -1126,  -9576,  -1990,
       252,   -422,    672,   1232,  -1960,   3284,    561,   -893,   1497,  -4365
  ], [
      4889,  -6878,    612,   6109,   4753,  -1459,  -2887,    -22,  -2277,  -1379,
      2052,   -182,    257,  -1823,   2564,   -228,  -1418,   1995,   -177,  -1772
  ], [
      3053,   -506,   2403,   9625,   1322,   -569,    -15,   -352,  -5655,   -106,
        94,   -448,     74,  -1794,    297,  -1412,   -246,     40,   -194,   -777
  ], [
      -754,  12904,   4480,  -2113,   1471,    -34, -10163,  -1225,   -272,   -132,
       594,    206,  -3529,    -97,   1664,    577,     67,  -1159,   -402,    189
  ], [
      4255,   1476,   5055,   2393,   2912,  -1105,   -132,  -1559,   -349,   -517,
      -383,  -1313,   -455,   -621,   -215,   -738,   -756,   -262,   -898,   -425
  ], [
     -1371,    535,   1417,  14604,   -997,   -114,    -17,   -122, -13017,    -60,
        44,    118,    -46,   1222,   -477,  -1263,    -83,     32,     86,    888
  ], [
      5368,  -1744,   4083,  -1236,   3753,  -1758,   -185,  -1017,    -93,   -860,
       571,  -1338,    434,    405,   -131,    308,  -1229,    399,   -935,    283
  ], [
      1588,  -3097,  14415,   3699,  -1171,   -154,   -585, -12683,   -835,    -83,
       300,  -1397,   2725,   -358,    699,  -3255,    113,   -221,   1030,    264
  ], [
       212,   7989,   9471,  -3344,   2009,     -2,  -3895,  -5475,   -682,   -246,
      -103,   -123,  -4618,     43,   1630,   1933,    -26,   -979,  -1161,    410
  ], [
       856,   2294,   -627,   6930,   6929,    -44,   -321,    -24,  -2931,  -2930,
      -119,     32,     87,   -362,   -970,    265,   -362,   -970,    265,  -2931
  ], [
      2357,  -4187,   7162,   7683,   3371,   -339,  -1070,  -3131,  -3603,   -693,
       602,  -1030,   1830,  -1105,   1963,  -3359,   -485,    861,  -1474,  -1581
  ], [
       350,   4585,  14053,  -3819,   1218,     -7,  -1283, -12054,   -890,    -90,
       -97,   -300,  -3933,     81,   1068,   3275,    -26,   -341,  -1045,    284
  ], [
     -3248,   3531,    475,   2137,  11711,   -644,   -761,    -13,   -278,  -8372,
       700,     94,   -102,    423,   -460,    -62,   2322,  -2524,   -340,  -1528
  ], [
     -3017,   3852,   1725,   8440,   5257,   -555,   -905,   -181,  -4348,  -1686,
       709,    317,   -405,   1554,  -1984,   -889,    968,  -1236,   -553,  -2708
  ], [
      -909,   3196,  15512,  -2528,   1066,    -50,   -623, -14686,   -390,    -69,
       177,    861,  -3026,   -140,    493,   2393,     59,   -208,  -1009,    164
  ], [
       959,  -3370,   9617,   9545,  -1761,    -56,   -693,  -5645,  -5561,   -189,
       197,   -563,   1978,   -558,   1963,  -5603,    103,   -362,   1034,   1026
  ], [
      7575,  11796,  -4845,   3252,  -1703,  -3502,  -8493,  -1433,   -645,   -177,
     -5454,   2240,   3488,  -1503,  -2341,    961,    787,   1226,   -503,    338
  ], [
      6409,   1722,   1764,  -4191,   6015,  -2507,   -181,   -189,  -1072,  -2208,
      -673,   -690,   -185,   1639,    440,    451,  -2353,   -632,   -647,   1538
  ], [
     -2420,  12161,   5038,   1286,  -2098,   -357,  -9027,  -1549,   -100,   -268,
      1796,    744,  -3740,    190,   -954,   -395,   -310,   1557,    645,    164
  ], [
     -2232,  -1341,   7246,   9470,  -1977,   -304,   -109,  -3204,  -5474,   -238,
      -182,    987,    593,   1290,    775,  -4188,   -269,   -161,    874,   1143
  ], [
      1030,   7034,   4231,   1551,   3077,    -64,  -3019,  -1093,   -146,   -577,
      -442,   -266,  -1816,    -97,   -666,   -400,   -193,  -1321,   -794,   -291
  ], [
      5121,  11835,   -477,  -1749,   2298,  -1601,  -8549,    -13,   -186,   -322,
     -3699,    149,    344,    546,   1264,    -50,   -718,  -1660,     66,    245
  ], [
     -3328,   3827,   5921,   9976,  -1045,   -676,   -894,  -2140,  -6075,    -66,
       777,   1203,  -1383,   2027,  -2330,  -3605,   -212,    244,    377,    636
  ], [
      3813,   5718,  -4666,  -3412,   5674,   -887,  -1995,  -1329,   -710,  -1965,
     -1331,   1086,   1628,    794,   1191,   -972,  -1320,  -1980,   1616,   1181
  ], [
      1348,  -3672,  13154,   6938,  -1690,   -110,   -823, -10561,  -2938,   -174,
       302,  -1082,   2948,   -570,   1555,  -5570,    139,   -379,   1357,    716
  ], [
      2151,  -3586,   6949,  12131,  -1224,   -282,   -785,  -2947,  -8982,    -91,
       470,   -912,   1521,  -1592,   2655,  -5145,    160,   -268,    519,    906
  ], [
     -2889,   9647,  10276,  -2728,    995,   -509,  -5680,  -6445,   -454,    -60,
      1701,   1812,  -6051,   -481,   1606,   1711,    175,   -586,   -624,    165
  ], [
      6177,   2184,    555,   1985,   6589,  -2329,   -291,    -18,   -240,  -2650,
      -823,   -209,    -74,   -748,   -264,    -67,  -2484,   -878,   -223,   -798
  ], [
      -492,    391,  17166,   -681,    240,    -14,     -9, -17987,    -28,     -3,
        11,    515,   -410,    -20,     16,    713,      7,     -5,   -252,     10
  ], [
     12628,   5448,  -2630,   3011,  -2695,  -9733,  -1811,   -422,   -553,   -443,
     -4199,   2027,    874,  -2321,  -1001,    483,   2077,    896,   -432,    495
  ], [
     -3628,   -534,   3447,   7002,   6751,   -803,    -17,   -725,  -2992,  -2782,
      -118,    763,    112,   1550,    228,  -1473,   1495,    220,  -1420,  -2885
  ], [
     -5239,   5901,   8107,   3650,   4846,  -1675,  -2125,  -4012,   -813,  -1433,
      1887,   2592,  -2920,   1167,  -1315,  -1806,   1550,  -1745,  -2398,  -1080
  ], [
      6157,   6678,   4099,  -1074,   2348,  -2314,  -2722,  -1025,    -70,   -336,
     -2509,  -1540,  -1670,    403,    437,    268,   -882,   -957,   -587,    153
  ], [
      1079,  16099,    242,   -881,   1690,    -71, -15820,     -3,    -47,   -174,
     -1060,    -16,   -238,     58,    865,     13,   -111,  -1661,    -25,     90
  ], [
      -278,    227,  -1039,   1636,  16945,     -4,     -3,    -65,   -163, -17526,
         3,    -17,     14,     27,    -22,    103,    287,   -234,   1074,  -1693
  ], [
     15778,  -1454,    574,   -603,   -107, -15195,   -129,    -20,    -22,      0,
      1400,   -553,     51,    581,    -53,     21,    103,     -9,      3,     -3
  ], [
      2406,   -836,  13224,   7993,  -4266,   -353,    -42, -10673,  -3899,  -1111,
       122,  -1942,    674,  -1174,    407,  -6451,    626,   -217,   3443,   2081
  ], [
      3184,  14368,  -3336,   2255,  -1801,   -619, -12600,   -679,   -310,   -198,
     -2793,    648,   2926,   -438,  -1977,    459,    350,   1580,   -366,    247
  ], [
     -1698,  17076,   2504,   -539,   -646,   -176, -17798,   -382,    -17,    -25,
      1770,    259,  -2610,    -55,    561,     82,    -67,    673,     98,    -21
  ], [
      2375,   -797,  -2696,  14483,   5383,   -344,    -38,   -443, -12803,  -1769,
       115,    391,   -131,  -2100,    705,   2384,   -780,    262,    886,  -4759
  ], [
     -2691,   2554,  -4520,   9573,  10655,   -442,   -398,  -1247,  -5594,  -6930,
       419,   -742,    704,   1572,  -1492,   2641,   1750,  -1661,   2939,  -6226
  ], [
     -4332,  -4399,  -1657,   4880,   7375,  -1145,  -1181,   -167,  -1453,  -3319,
     -1163,   -438,   -444,   1290,   1310,    493,   1950,   1980,    745,  -2196
  ], [
     -3498,   7405,   9955,   2693,  -2971,   -746,  -3347,  -6049,   -442,   -538,
      1581,   2125,  -4499,    575,  -1217,  -1636,   -634,   1342,   1805,    488
  ], [
      6717,  -3792,   7739,   2798,   3489,  -2754,   -877,  -3655,   -477,   -743,
      1554,  -3173,   1791,  -1147,    647,  -1321,  -1430,    807,  -1648,   -595
  ], [
      5263,   9770,   3463,   1069,  -3971,  -1690,  -5826,   -732,    -69,   -962,
     -3138,  -1112,  -2065,   -343,   -637,   -226,   1275,   2368,    839,    259
  ], [
      1243,  -2634,  16772,   1871,    332,    -94,   -423, -17169,   -213,     -6,
       199,  -1273,   2696,   -142,    300,  -1915,    -25,     53,   -339,    -37
  ], [
      2691,   2836,   3105,   5711,   4817,   -442,   -491,   -588,  -1991,  -1416,
      -465,   -510,   -537,   -938,   -988,  -1082,   -791,   -834,   -913,  -1679
  ], [
      4366,   2944,   7210,   3627,   1161,  -1163,   -529,  -3172,   -803,    -82,
      -784,  -1921,  -1295,   -966,   -651,  -1596,   -309,   -208,   -511,   -257
  ], [
     13888,   3951,   -671,  -2305,   3354, -11773,   -953,    -27,   -324,   -686,
     -3349,    569,    161,   1954,    556,    -94,  -2843,   -809,    137,    472
  ], [
      7053,   5847,   2929,   8378,  -4794,  -3036,  -2086,   -523,  -4284,  -1403,
     -2517,  -1261,  -1045,  -3607,  -2990,  -1498,   2064,   1711,    857,   2451
  ], [
     -2191,  12838,   9182,  -3915,   1617,   -293, -10059,  -5146,   -935,   -159,
      1717,   1228,  -7195,   -523,   3068,   2194,    216,  -1267,   -906,    386
  ], [
     -4881,  13114,   5767,   -435,   4155,  -1454, -10498,  -2030,    -11,  -1054,
      3907,   1718,  -4616,   -129,    348,    153,   1238,  -3326,  -1462,    110
  ], [
      7843,  -1250,    210,   7106,  -5203,  -3754,    -95,     -2,  -3082,  -1652,
       598,   -100,     16,  -3402,    542,    -91,   2491,   -397,     66,   2257
  ], [
     -2463,   8168,  14551,  -3908,   1828,   -370,  -4072, -12923,   -932,   -204,
      1228,   2188,  -7254,   -587,   1948,   3471,    274,   -911,  -1623,    436
  ], [
     -1579,    347,   -272,  -2735,  16031,   -152,     -7,     -4,   -456, -15686,
        33,    -26,      5,   -263,     58,    -45,   1545,   -340,    266,   2676
  ], [
     -6327,   1328,   5093,  -5079,   7617,  -2443,   -107,  -1583,  -1574,  -3541,
       513,   1967,   -413,  -1961,    411,   1578,   2941,   -617,  -2367,   2361
  ], [
      3286,  -4509,  11306,  11025,  -2623,   -659,  -1241,  -7802,  -7419,   -420,
       904,  -2267,   3112,  -2211,   3034,  -7608,    526,   -722,   1810,   1765
  ], [
      5567,  17853,  -3754,   1166,   -519,  -1892, -19455,   -860,    -83,    -16,
     -6067,   1275,   4090,   -396,  -1271,    267,    176,    566,   -119,     37
  ], [
     -2136,   -424,  15292,   5108,  -1648,   -278,    -10, -14273,  -1593,   -165,
       -55,   1993,    396,    666,    132,  -4768,   -214,    -42,   1538,    514
  ], [
      2267,  -3297,   2549,  16563,   -791,   -313,   -663,   -396, -16745,    -38,
       456,   -352,    513,  -2291,   3333,  -2576,    109,   -159,    123,    799
  ], [
      3655,   1899,  -3364,   6279,  12510,   -815,   -220,   -690,  -2406,  -9552,
      -423,    750,    390,  -1400,   -728,   1289,  -2791,  -1450,   2568,  -4794
  ], [
      8052,   2285,  -6193,   5138,   6003,  -3957,   -318,  -2341,  -1611,  -2199,
     -1123,   3044,    864,  -2525,   -716,   1942,  -2950,   -837,   2269,  -1882
  ], [
      -386,  -2291,   7679,  15387,  -2723,     -9,   -320,  -3599, -14452,   -452,
       -54,    181,   1074,    362,   2152,  -7212,    -64,   -380,   1276,   2557
  ], [
      2777,  -1173,   3984,  13079,   2508,   -470,    -84,   -969, -10440,   -384,
       198,   -675,    285,  -2217,    936,  -3180,   -425,    179,   -610,  -2002
  ], [
     -1879,   1771,  -2684,  16705,   1833,   -215,   -191,   -439, -17032,   -205,
       203,   -308,    290,   1916,  -1805,   2736,    210,   -198,    300,  -1869
  ], [
      1052,   4495,  15519,   1467,  -4032,    -67,  -1233, -14700,   -131,   -992,
      -288,   -997,  -4257,    -94,   -402,  -1389,    259,   1106,   3819,    361
  ], [
      3010,   2544,   6969,   7559,   1996,   -553,   -395,  -2964,  -3487,   -243,
      -467,  -1280,  -1082,  -1388,  -1174,  -3215,   -366,   -310,   -849,   -921
  ], [
     -5209,  -1867,   8713,  10351,   1549,  -1656,   -212,  -4634,  -6540,   -146,
      -593,   2770,    993,   3291,   1180,  -5505,    492,    176,   -824,   -979
  ], [
     -4314,   8513,    913,   7547,  -2723,  -1135,  -4423,    -50,  -3476,   -452,
      2241,    240,   -474,   1987,  -3921,   -420,   -717,   1415,    151,   1254
  ], [
     12929,  -1219,   2448,   1757,   6303, -10204,    -90,   -365,   -188,  -2425,
       962,  -1932,    182,  -1386,    130,   -262,  -4974,    469,   -941,   -676
  ], [
      6465,   4132,   3167,   3160,   5697,  -2551,  -1042,   -612,   -609,  -1981,
     -1630,  -1249,   -798,  -1247,   -797,   -611,  -2248,  -1437,  -1101,  -1099
  ], [
     -3636,   4859,  18914,  -1335,    810,   -807,  -1441, -21836,   -108,    -40,
      1078,   4198,  -5609,   -296,    396,   1541,    179,   -240,   -936,     66
  ], [
      8844,   7864,    654,  -4063,  -5680,  -4774,  -3774,    -26,  -1007,  -1969,
     -4245,   -353,   -314,   2193,   1950,    162,   3066,   2726,    226,  -1408
  ], [
      1859,   2634,   9228,    996,   9464,   -211,   -423,  -5197,    -60,  -5467,
      -299,  -1047,  -1483,   -113,   -160,   -561,  -1074,  -1521,  -5330,   -575
  ], [
      2949,  12260,  10290,   -497,  -3943,   -530,  -9174,  -6463,    -15,   -949,
     -2206,  -1852,  -7700,     89,    372,    312,    709,   2950,   2476,   -119
  ], [
     -2903,   1552,  14867,   9970,   -496,   -514,   -147, -13491,  -6068,    -15,
       275,   2634,  -1408,   1766,   -944,  -9047,    -87,     47,    450,    302
  ], [
      3243,   8234,   7586,   3373,   2151,   -642,  -4138,  -3512,   -694,   -282,
     -1630,  -1501,  -3812,   -667,  -1695,  -1561,   -425,  -1081,   -996,   -442
  ], [
     -9631,     60,   3501,   5359,  10150,  -5662,      0,   -748,  -1752,  -6288,
        35,   2058,    -12,   3150,    -19,  -1145,   5967,    -37,  -2169,  -3320
  ], [
     -6874,  -2553,  -5446,  -2195,  -7841,  -2884,   -397,  -1810,   -294,  -3753,
     -1071,  -2285,   -848,   -921,   -342,   -729,  -3290,  -1221,  -2606,  -1050
  ], [
     -3413,  -1141,   4630,  13612,   7897,   -711,    -79,  -1308, -11310,  -3806,
      -237,    964,    322,   2836,    948,  -3847,   1645,    550,  -2231,  -6561
  ], [
      4410,  -5678,   8006,  -3992,   3811,  -1187,  -1968,  -3912,   -973,   -886,
      1528,  -2155,   2775,   1074,  -1383,   1951,  -1025,   1321,  -1862,    928
  ], [
      5659,  11535,   2203,   -452,   7169,  -1954,  -8121,   -296,    -12,  -3137,
     -3984,   -761,  -1551,    156,    318,     60,  -2476,  -5048,   -964,    197
  ], [
      2914,  -2914,   3485,  -3965,  13675,   -518,   -518,   -741,   -959, -11414,
       518,   -620,    620,    705,   -705,    843,  -2433,   2432,  -2909,   3310
  ], [
      7843,   1907,   1022,   8882,   7972,  -3755,   -222,    -63,  -4815,  -3879,
      -913,   -489,   -119,  -4252,  -1034,   -554,  -3816,   -928,   -497,  -4322
  ], [
     13807,   9531,   1436,   1612,   1779, -11636,  -5544,   -125,   -158,   -193,
     -8032,  -1210,   -835,  -1358,   -938,   -141,  -1499,  -1035,   -156,   -175
  ], [
     13620,  -5337,   5450,  -2263,   1723, -11322,  -1738,  -1813,   -312,   -181,
      4436,  -4531,   1775,   1881,   -737,    752,  -1432,    561,   -573,    238
  ], [
      5297,   8374,   8872,   7694,   6538,  -1712,  -4280,  -4804,  -3613,  -2609,
     -2707,  -2868,  -4534,  -2487,  -3932,  -4166,  -2113,  -3341,  -3540,  -3070
  ]
];
const ACB_GAIN85: [[i16; 20]; 85] = [
  [
         0,      0,      0,      0,      0,      0,      0,      0,      0,      0,
         0,      0,      0,      0,      0,      0,      0,      0,      0,      0
  ], [
       800,   1496,    167,   -256,   -338,    -39,   -136,     -1,     -4,     -6,
       -73,     -8,    -15,     12,     23,      2,     16,     30,      3,     -5
  ], [
      -462,   -686,    493,   2575,    311,    -13,    -28,    -14,   -404,     -5,
       -19,     13,     20,     72,    107,    -77,      8,     13,     -9,    -48
  ], [
      1483,    144,    784,    928,   1243,   -134,     -1,    -37,    -52,    -94,
       -13,    -71,     -6,    -84,     -8,    -44,   -112,    -10,    -59,    -70
  ], [
       -77,    275,   3522,   1056,  -1254,      0,     -4,   -757,    -68,    -95,
         1,     16,    -59,      4,    -17,   -227,     -5,     21,    269,     80
  ], [
      -125,    -40,   -264,    381,   5027,      0,      0,     -4,     -8,  -1542,
         0,     -2,      0,      2,      0,      6,     38,     12,     81,   -117
  ], [
       138,    332,   2215,   2574,   1339,     -1,     -6,   -299,   -404,   -109,
        -2,    -18,    -44,    -21,    -52,   -348,    -11,    -27,   -181,   -210
  ], [
      3685,   2883,   -887,    866,  -1639,   -828,   -507,    -48,    -45,   -164,
      -648,    199,    156,   -194,   -152,     46,    368,    288,    -88,     86
  ], [
      1396,   2146,   2235,    345,    942,   -118,   -281,   -305,     -7,    -54,
      -182,   -190,   -292,    -29,    -45,    -47,    -80,   -123,   -128,    -19
  ], [
        13,   4475,   3549,   -804,   -655,      0,  -1222,   -768,    -39,    -26,
        -3,     -2,   -969,      0,    219,    174,      0,    179,    141,    -32
  ], [
      -724,    254,    242,   6049,   2462,    -32,     -3,     -3,  -2233,   -370,
        11,     10,     -3,    267,    -94,    -89,    108,    -38,    -36,   -909
  ], [
       626,  -1713,   6121,   4561,  -1061,    -23,   -179,  -2287,  -1270,    -68,
        65,   -233,    640,   -174,    477,  -1704,     40,   -111,    396,    295
  ], [
      -350,   1391,   7985,    511,   -405,     -7,   -118,  -3892,    -15,    -10,
        29,    170,   -678,     10,    -43,   -249,     -8,     34,    197,     12
  ], [
      3144,   -529,    608,   2530,   3878,   -603,    -17,    -22,   -390,   -918,
       101,   -116,     19,   -485,     81,    -93,   -744,    125,   -144,   -599
  ], [
      2589,   -689,   3045,   5603,   -404,   -409,    -29,   -566,  -1916,    -10,
       108,   -481,    128,   -885,    235,  -1041,     63,    -17,     75,    138
  ], [
      3107,    513,   1374,  -3594,  -4922,   -589,    -16,   -115,   -788,  -1478,
       -97,   -260,    -43,    681,    112,    301,    933,    154,    413,  -1079
  ], [
      2468,   6010,   1107,   -390,   1961,   -372,  -2204,    -74,     -9,   -234,
      -905,   -166,   -406,     58,    143,     26,   -295,   -719,   -132,     46
  ], [
      4773,   2766,   2368,   4862,  -4044,  -1390,   -467,   -342,  -1443,   -998,
      -806,   -690,   -399,  -1416,   -821,   -702,   1178,    682,    584,   1200
  ], [
      1665,  -1879,   1443,   1701,   8562,   -169,   -215,   -127,   -176,  -4475,
       190,   -146,    165,   -172,    195,   -149,   -870,    982,   -754,   -889
  ], [
      2716,   9011,  -1007,    755,  -1785,   -450,  -4956,    -61,    -34,   -194,
     -1493,    167,    554,   -125,   -415,     46,    296,    982,   -109,     82
  ], [
     -2727,   7548,   1285,    938,   3420,   -453,  -3478,   -100,    -53,   -714,
      1256,    213,   -592,    156,   -432,    -73,    569,  -1576,   -268,   -196
  ], [
      3677,    882,   4050,   1202,   2323,   -825,    -47,  -1001,    -88,   -329,
      -198,   -909,   -218,   -269,    -64,   -297,   -521,   -125,   -574,   -170
  ], [
      2046,   -753,    122,  10102,    603,   -255,    -34,      0,  -6229,    -22,
        94,    -15,      5,  -1261,    464,    -75,    -75,     27,     -4,   -372
  ], [
       449,  -1815,  10690,   3870,   -527,    -12,   -201,  -6976,   -914,    -16,
        49,   -293,   1184,   -106,    428,  -2525,     14,    -58,    344,    124
  ], [
      -941,   2352,   5049,   3650,   2637,    -54,   -337,  -1556,   -813,   -424,
       135,    290,   -725,    209,   -524,  -1125,    151,   -378,   -812,   -587
  ], [
     -1879,    796,   3117,   9569,   -404,   -215,    -38,   -593,  -5589,     -9,
        91,    357,   -151,   1097,   -464,  -1821,    -46,     19,     76,    236
  ], [
     -1715,   2043,  -2096,   9946,   4001,   -179,   -254,   -268,  -6038,   -977,
       213,   -219,    261,   1041,  -1240,   1272,    418,   -498,    511,  -2429
  ], [
     -5772,   -618,  -3921,    284,  -3155,  -2033,    -23,   -938,     -4,   -607,
      -218,  -1381,   -148,    100,     10,     68,  -1111,   -119,   -755,     54
  ], [
       382,   4748,   8003,  -2064,   2198,     -8,  -1376,  -3909,   -260,   -294,
      -110,   -186,  -2319,     48,    598,   1008,    -51,   -637,  -1073,    277
  ], [
      -867,   3015,  11926,  -1675,    947,    -45,   -555,  -8681,   -171,    -54,
       159,    631,  -2195,    -88,    308,   1219,     50,   -174,   -690,     96
  ], [
     -4933,   -432,   6757,   3771,   1352,  -1485,    -11,  -2786,   -867,   -111,
      -130,   2034,    178,   1135,     99,  -1555,    407,     35,   -557,   -311
  ], [
       152,   9726,   4231,  -1928,   1490,     -1,  -5774,  -1092,   -226,   -135,
       -90,    -39,  -2511,     17,   1144,    498,    -13,   -884,   -384,    175
  ], [
      2512,    193,   9033,   5361,  -3148,   -385,     -2,  -4980,  -1754,   -605,
       -29,  -1385,   -106,   -822,    -63,  -2956,    482,     37,   1735,   1030
  ], [
      8464,   2844,     12,    549,   2132,  -4373,   -493,      0,    -18,   -277,
     -1469,     -6,     -2,   -284,    -95,      0,  -1101,   -370,     -1,    -71
  ], [
      2141,  -2602,   7166,   9046,  -1350,   -279,   -413,  -3134,  -4994,   -111,
       340,   -936,   1138,  -1182,   1436,  -3957,    176,   -214,    590,    745
  ], [
      -244,    278,  13307,   1227,   -161,     -3,     -4, -10808,    -91,     -1,
         4,    198,   -226,     18,    -20,   -997,     -2,      2,    131,     12
  ], [
     -1947,   8217,   6269,    917,  -2559,   -231,  -4121,  -2399,    -51,   -399,
       976,    745,  -3144,    108,   -460,   -350,   -304,   1283,    979,    143
  ], [
     -1810,   2061,  -2781,   6056,  10058,   -200,   -259,   -472,  -2238,  -6174,
       227,   -307,    349,    669,   -761,   1028,   1111,  -1265,   1707,  -3717
  ], [
      7827,   9161,  -3409,   2473,  -1510,  -3739,  -5122,   -709,   -373,   -139,
     -4376,   1628,   1906,  -1181,  -1382,    514,    721,    844,   -314,    228
  ], [
     -1430,   8313,   9541,  -2955,   1626,   -124,  -4218,  -5556,   -533,   -161,
       725,    832,  -4841,   -257,   1499,   1721,    142,   -825,   -947,    293
  ], [
      2819,  -4247,   5391,   8673,   2756,   -485,  -1101,  -1774,  -4591,   -463,
       730,   -927,   1397,  -1492,   2248,  -2854,   -474,    714,   -907,  -1459
  ], [
       141,  14552,    690,    257,   -112,     -1, -12926,    -29,     -4,      0,
      -125,     -5,   -613,     -2,   -228,    -10,      0,     99,      4,      1
  ], [
     11938,  -1859,   1806,   -962,   -884,  -8699,   -211,   -199,    -56,    -47,
      1355,  -1316,    205,    701,   -109,    106,    644,   -100,     97,    -51
  ], [
      3728,   1982,   2264,   4584,   3131,   -848,   -239,   -312,  -1282,   -598,
      -451,   -515,   -273,  -1043,   -554,   -633,   -712,   -378,   -432,   -876
  ], [
     -1181,    766,    720,  14303,   -216,    -85,    -35,    -31, -12486,     -2,
        55,     51,    -33,   1031,   -668,   -628,    -15,     10,      9,    189
  ], [
     -4385,   4826,  10112,   1569,   3388,  -1173,  -1421,  -6242,   -150,   -700,
      1291,   2706,  -2979,    420,   -462,   -969,    906,   -998,  -2091,   -324
  ], [
      -448,   1932,  15591,  -1842,    657,    -12,   -227, -14837,   -207,    -26,
        52,    427,  -1838,    -50,    217,   1753,     18,    -77,   -626,     74
  ], [
     -4141,   1844,   3962,   5517,   6220,  -1046,   -207,   -958,  -1858,  -2361,
       466,   1001,   -446,   1394,   -621,  -1334,   1572,   -700,  -1504,  -2094
  ], [
       729,  -2299,  14755,   3657,   -952,    -32,   -322, -13288,   -816,    -55,
       102,   -656,   2071,   -162,    513,  -3294,     42,   -133,    857,    212
  ], [
     -1385,   5801,  13339,  -3137,   1344,   -117,  -2054, -10861,   -600,   -110,
       490,   1127,  -4723,   -265,   1111,   2554,    113,   -476,  -1094,    257
  ], [
      4710,   9661,   1073,  -2467,   3274,  -1354,  -5697,    -70,   -371,   -654,
     -2777,   -308,   -633,    709,   1455,    161,   -941,  -1930,   -214,    493
  ], [
      1843,  -3624,  12422,   6898,  -1559,   -207,   -802,  -9419,  -2904,   -148,
       407,  -1397,   2748,   -775,   1526,  -5230,    175,   -344,   1182,    656
  ], [
      1433,   2394,   2507,   1380,   8780,   -125,   -349,   -383,   -116,  -4705,
      -209,   -219,   -366,   -120,   -201,   -211,   -768,  -1283,  -1343,   -740
  ], [
     -1712,  12915,   5883,  -2197,    991,   -179, -10181,  -2112,   -294,    -60,
      1350,    615,  -4638,   -229,   1732,    789,    103,   -781,   -356,    133
  ], [
     15072,   2158,  -1245,    910,   -496, -13865,   -284,    -94,    -50,    -15,
     -1986,   1145,    164,   -837,   -119,     69,    456,     65,    -37,     27
  ], [
      4655,   7319,   4916,    586,  -3381,  -1322,  -3270,  -1475,    -20,   -697,
     -2079,  -1396,  -2196,   -166,   -261,   -175,    960,   1510,   1014,    120
  ], [
      1191,  -2140,   5120,  13498,  -1418,    -86,   -279,  -1600, -11121,   -122,
       155,   -372,    669,   -981,   1763,  -4218,    103,   -185,    443,   1168
  ], [
     -1530,   -817,   8191,   9632,  -1452,   -143,    -40,  -4095,  -5663,   -128,
       -76,    765,    408,    900,    480,  -4815,   -135,    -72,    726,    854
  ], [
     -3236,    607,   1696,  -2106,  11485,   -639,    -22,   -175,   -270,  -8051,
       119,    335,    -62,   -416,     78,    218,   2268,   -425,  -1189,   1476
  ], [
      3203,  -1903,   -837,   9679,   7057,   -626,   -221,    -42,  -5718,  -3039,
       372,    163,    -97,  -1892,   1124,    494,  -1380,    819,    360,  -4169
  ], [
       213,   -655,  17015,    620,   -384,     -2,    -26, -17671,    -23,     -9,
         8,   -221,    681,     -8,     24,   -644,      5,    -15,    399,     14
  ], [
      5088,     35,  -3339,   3726,   8488,  -1580,      0,   -680,   -847,  -4397,
       -10,   1037,      7,  -1157,     -8,    759,  -2636,    -18,   1730,  -1930
  ], [
      -988,   1454,  -2688,  15039,   2682,    -59,   -129,   -441, -13805,   -439,
        87,   -162,    238,    907,  -1335,   2467,    161,   -238,    440,  -2462
  ], [
     -4865,  -2842,    -53,   5495,   6523,  -1445,   -493,      0,  -1843,  -2597,
      -844,    -16,     -9,   1632,    953,     18,   1937,   1131,     21,  -2188
  ], [
      3076,  15069,  -2914,   1810,   -971,   -577, -13860,   -518,   -200,    -57,
     -2829,    547,   2680,   -339,  -1665,    322,    182,    893,   -172,    107
  ], [
      1311,   5355,  11054,   2299,  -3654,   -105,  -1750,  -7458,   -322,   -814,
      -428,   -885,  -3613,   -184,   -751,  -1551,    292,   1194,   2465,    512
  ], [
      4035,   5619,   4618,   1815,   1912,   -994,  -1927,  -1301,   -201,   -223,
     -1384,  -1137,  -1583,   -447,   -622,   -511,   -471,   -656,   -539,   -211
  ], [
     -2131,   2754,  -4501,  12879,   7432,   -277,   -463,  -1236, -10124,  -3371,
       358,   -585,    756,   1675,  -2165,   3538,    967,  -1249,   2042,  -5842
  ], [
      5618,   -515,   3219,  -4149,   4857,  -1926,    -16,   -632,  -1050,  -1440,
       176,  -1104,    101,   1422,   -130,    815,  -1666,    152,   -954,   1230
  ], [
      1838,  -1709,   1139,  16867,    716,   -206,   -178,    -79, -17366,    -31,
       191,   -127,    118,  -1892,   1759,  -1173,    -80,     74,    -49,   -737
  ], [
      1978,  -3845,  10050,  11854,  -2492,   -238,   -902,  -6164,  -8576,   -379,
       464,  -1213,   2358,  -1431,   2782,  -7271,    301,   -585,   1529,   1803
  ], [
     -2600,  11246,  11289,  -3647,   1463,   -412,  -7720,  -7778,   -812,   -130,
      1784,   1791,  -7749,   -578,   2504,   2513,    232,  -1004,  -1008,    325
  ], [
      3442,    907,   2725,   8970,   3638,   -723,    -50,   -453,  -4911,   -808,
      -190,   -572,   -150,  -1884,   -496,  -1492,   -764,   -201,   -605,  -1992
  ], [
      -126,  17498,   3481,  -2003,   1090,      0, -18689,   -739,   -244,    -72,
       135,     26,  -3717,    -15,   2139,    425,      8,  -1165,   -231,    133
  ], [
     -1814,   1048,  -2164,   4070,  16272,   -200,    -67,   -285,  -1011, -16160,
       116,   -239,    138,    450,   -260,    537,   1801,  -1041,   2149,  -4042
  ], [
      9354,  12580,  -1883,    962,   -617,  -5341,  -9660,   -216,    -56,    -23,
     -7183,   1075,   1446,   -549,   -738,    110,    352,    474,    -71,     36
  ], [
      1708,   4199,   7387,   6335,   1003,   -178,  -1076,  -3330,  -2449,    -61,
      -437,   -770,  -1893,   -660,  -1623,  -2856,   -104,   -257,   -452,   -388
  ], [
     -2624,   5623,  17310,  -2353,    592,   -420,  -1930, -18288,   -338,    -21,
       900,   2772,  -5941,   -376,    807,   2486,     94,   -203,   -625,     85
  ], [
      1211,   -850,   1193,  -1926,  15992,    -89,    -44,    -86,   -226, -15609,
        62,    -88,     61,    142,   -100,    140,  -1182,    830,  -1165,   1880
  ], [
      3983,  -2054,  11506,    -19,   3622,   -968,   -257,  -8080,      0,   -801,
       499,  -2797,   1442,      4,     -2,     13,   -880,    454,  -2544,      4
  ], [
      -786,  -1354,  16092,   7246,  -1665,    -37,   -111, -15805,  -3205,   -169,
       -65,    772,   1330,    348,    599,  -7117,    -80,   -137,   1636,    736
  ], [
     -4316,   -511,   6674,  11665,   4633,  -1137,    -15,  -2719,  -8305,  -1310,
      -134,   1758,    208,   3073,    364,  -4752,   1220,    144,  -1887,  -3299
  ], [
      7912,   4557,   1937,   1885,   7037,  -3821,  -1267,   -229,   -216,  -3022,
     -2200,   -935,   -538,   -910,   -524,   -222,  -3398,  -1957,   -832,   -809
  ], [
      3434,   2967,   5867,   8196,   8766,   -720,   -537,  -2101,  -4100,  -4690,
      -622,  -1230,  -1062,  -1718,  -1484,  -2935,  -1837,  -1588,  -3139,  -4385
  ], [
      5881,   9176,   8119,   3934,   3355,  -2111,  -5139,  -4023,   -944,   -687,
     -3294,  -2914,  -4547,  -1412,  -2203,  -1949,  -1204,  -1879,  -1662,   -805
  ]
];

const PULSE_POSITIONS: [[u32; SUBFRAME_LEN/2]; MAX_PULSES] = [
  [
    118755, 98280, 80730, 65780, 53130, 42504, 33649, 26334,  20349, 15504,
     11628,  8568,  6188,  4368,  3003,  2002,  1287,   792,    462,   252,
       126,    56,    21,     6,     1,     0,     0,     0,      0,     0
  ], [
     23751, 20475, 17550, 14950, 12650, 10626,  8855,  7315,   5985,  4845,
      3876,  3060,  2380,  1820,  1365,  1001,   715,   495,    330,   210,
       126,    70,    35,    15,     5,     1,     0,     0,      0,     0
  ], [
      3654,  3276,  2925,  2600,  2300,  2024,  1771,  1540,   1330,  1140,
       969,   816,   680,   560,   455,   364,   286,   220,    165,   120,
        84,    56,    35,    20,    10,     4,     1,     0,      0,     0
  ], [
       406,   378,   351,   325,   300,   276,   253,   231,    210,   190,
       171,   153,   136,   120,   105,    91,    78,    66,     55,    45,
        36,    28,    21,    15,    10,     6,     3,     1,      0,     0
  ], [
        29,    28,    27,    26,    25,    24,    23,    22,     21,    20,
        19,    18,    17,    16,    15,    14,    13,    12,     11,    10,
         9,     8,     7,     6,     5,     4,     3,     2,      1,     0
  ], [
     1; SUBFRAME_LEN/2
  ]
];

const PITCH_CONTRIBUTION: [i32; 340] = [
    60,     0,  0,  2489, 60,     0,  0,  5217,  1,  6171,  0,  3953,  0, 10364,  1,  9357,
    -1,  8843,  1,  9396,  0,  5794, -1, 10816,  2, 11606, -2, 12072,  0,  8616,  1, 12170,
     0, 14440,  0,  7787, -1, 13721,  0, 18205,  0, 14471,  0, 15807,  1, 15275,  0, 13480,
    -1, 18375, -1,     0,  1, 11194, -1, 13010,  1, 18836, -2, 20354,  1, 16233, -1,     0,
    60,     0,  0, 12130,  0, 13385,  1, 17834,  1, 20875,  0, 21996,  1,     0,  1, 18277,
    -1, 21321,  1, 13738, -1, 19094, -1, 20387, -1,     0,  0, 21008, 60,     0, -2, 22807,
     0, 15900,  1,     0,  0, 17989, -1, 22259,  1, 24395,  1, 23138,  0, 23948,  1, 22997,
     2, 22604, -1, 25942,  0, 26246,  1, 25321,  0, 26423,  0, 24061,  0, 27247, 60,     0,
    -1, 25572,  1, 23918,  1, 25930,  2, 26408, -1, 19049,  1, 27357, -1, 24538, 60,     0,
    -1, 25093,  0, 28549,  1,     0,  0, 22793, -1, 25659,  0, 29377,  0, 30276,  0, 26198,
     1, 22521, -1, 28919,  0, 27384,  1, 30162, -1,     0,  0, 24237, -1, 30062,  0, 21763,
     1, 30917, 60,     0,  0, 31284,  0, 29433,  1, 26821,  1, 28655,  0, 31327,  2, 30799,
     1, 31389,  0, 32322,  1, 31760, -2, 31830,  0, 26936, -1, 31180,  1, 30875,  0, 27873,
    -1, 30429,  1, 31050,  0,     0,  0, 31912,  1, 31611,  0, 31565,  0, 25557,  0, 31357,
    60,     0,  1, 29536,  1, 28985, -1, 26984, -1, 31587,  2, 30836, -2, 31133,  0, 30243,
    -1, 30742, -1, 32090, 60,     0,  2, 30902, 60,     0,  0, 30027,  0, 29042, 60,     0,
     0, 31756,  0, 24553,  0, 25636, -2, 30501, 60,     0, -1, 29617,  0, 30649, 60,     0,
     0, 29274,  2, 30415,  0, 27480,  0, 31213, -1, 28147,  0, 30600,  1, 31652,  2, 29068,
    60,     0,  1, 28571,  1, 28730,  1, 31422,  0, 28257,  0, 24797, 60,     0,  0,     0,
    60,     0,  0, 22105,  0, 27852, 60,     0, 60,     0, -1, 24214,  0, 24642,  0, 23305,
    60,     0, 60,     0,  1, 22883,  0, 21601, 60,     0,  2, 25650, 60,     0, -2, 31253,
    -2, 25144,  0, 17998
];

const POSTFILTER_COEFFS: [[i16; LPC_ORDER]; 2] = [
    [ 21299, 13844,  8999,  5849, 3802, 2471, 1606, 1044,  679,  441 ],
    [ 24576, 18432, 13824, 10368, 7776, 5832, 4374, 3281, 2460, 1845 ]
];
