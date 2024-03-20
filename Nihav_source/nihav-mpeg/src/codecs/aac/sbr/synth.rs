use super::*;
use super::bs::FrameClass;
use super::dsp::{SBRAnalysis, SBRSynthesis};
use nihav_codec_support::dsp::fft::{FFTComplex, FFTC_ZERO};

const RANGE: f32 = 65536.0;

const SMOOTH_DELAY: usize = 4;

const NEW_BW: [[f32; 4]; 4] = [
    [ 0.0, 0.6,  0.9, 0.98 ],
    [ 0.6, 0.75, 0.9, 0.98 ],
    [ 0.0, 0.75, 0.9, 0.98 ],
    [ 0.0, 0.75, 0.9, 0.98 ]
];

#[derive(Clone, Copy)]
pub enum QuantMode {
    Single,
    Left,
    Right,
}

#[derive(Clone)]
pub struct SBRChannel {
        sbr_a:              SBRAnalysis,
        sbr_s:              SBRSynthesis,

        w:                  [[FFTComplex; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],
        x:                  [[FFTComplex; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],
        x_high:             [[FFTComplex; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],
        y:                  [[FFTComplex; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],
        prev_y:             [[FFTComplex; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],

        bw_array:           [f32; SBR_BANDS],
        old_bw_array:       [f32; SBR_BANDS],

    pub qmode:              QuantMode,
    pub fclass:             FrameClass,
    pub amp_res:            bool,
    pub num_env:            usize,
        prev_num_env:       usize,
    pub freq_res:           [bool; NUM_ENVELOPES],
    pub env_border:         [usize; NUM_ENVELOPES + 1],
    pub noise_env_border:   [usize; 3],
    pub pointer:            u8,
    pub num_noise:          usize,
    pub last_env_end:       usize,

    pub df_env:             [bool; NUM_ENVELOPES],
    pub df_noise:           [bool; 2],

    pub invf_mode:          [u8; NUM_PATCHES],
        old_invf_mode:      [u8; NUM_PATCHES],

    pub data_env:           [[i8; SBR_BANDS]; NUM_PATCHES],
    pub data_noise:         [[i8; SBR_BANDS]; NUM_PATCHES],
    pub data_env2:          [[i8; SBR_BANDS]; NUM_PATCHES],
    pub data_noise2:        [[i8; SBR_BANDS]; NUM_PATCHES],
    pub last_envelope:      [i8; SBR_BANDS],
    pub last_noise_env:     [i8; SBR_BANDS],
    pub last_freq_res:      bool,

    pub add_harmonic:       [bool; SBR_BANDS],
        prev_l_a:           i8,

        s_idx_mapped:       [[bool; SBR_BANDS]; NUM_ENVELOPES],
        prev_s_idx_mapped:  [bool; SBR_BANDS],
        index_sine:         usize,
        index_noise:        usize,
        g_temp:             [[f32; SBR_BANDS]; MAX_SLOTS * 2 + QMF_DELAY + SMOOTH_DELAY],
        q_temp:             [[f32; SBR_BANDS]; MAX_SLOTS * 2 + QMF_DELAY + SMOOTH_DELAY],
}

impl SBRChannel {
    pub fn new() -> Self {
        Self {
            sbr_a:              SBRAnalysis::new(),
            sbr_s:              SBRSynthesis::new(),

            w:                  [[FFTC_ZERO; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],
            x:                  [[FFTC_ZERO; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],
            x_high:             [[FFTC_ZERO; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],
            y:                  [[FFTC_ZERO; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],
            prev_y:             [[FFTC_ZERO; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2],

            bw_array:           [0.0; SBR_BANDS],
            old_bw_array:       [0.0; SBR_BANDS],

            qmode:              QuantMode::Single,
            fclass:             FrameClass::FixFix,
            amp_res:            false,
            num_env:            0,
            prev_num_env:       0,
            freq_res:           [false; NUM_ENVELOPES],
            env_border:         [0; NUM_ENVELOPES + 1],
            noise_env_border:   [0; 3],
            pointer:            0,
            num_noise:          0,
            last_env_end:       0,

            df_env:             [false; NUM_ENVELOPES],
            df_noise:           [false; 2],

            invf_mode:          [0; NUM_PATCHES],
            old_invf_mode:      [0; NUM_PATCHES],

            data_env:           [[0; SBR_BANDS]; NUM_PATCHES],
            data_noise:         [[0; SBR_BANDS]; NUM_PATCHES],
            data_env2:          [[0; SBR_BANDS]; NUM_PATCHES],
            data_noise2:        [[0; SBR_BANDS]; NUM_PATCHES],
            last_envelope:      [0; SBR_BANDS],
            last_noise_env:     [0; SBR_BANDS],
            last_freq_res:      false,

            add_harmonic:       [false; SBR_BANDS],
            prev_l_a:           -1,

            s_idx_mapped:       [[false; SBR_BANDS]; NUM_ENVELOPES],
            prev_s_idx_mapped:  [false; SBR_BANDS],
            index_sine:         0,
            index_noise:        0,
            g_temp:             [[0.0; SBR_BANDS]; MAX_SLOTS * 2 + QMF_DELAY + SMOOTH_DELAY],
            q_temp:             [[0.0; SBR_BANDS]; MAX_SLOTS * 2 + QMF_DELAY + SMOOTH_DELAY],
        }
    }

    pub fn reset(&mut self) {
        self.prev_y             = [[FFTC_ZERO; SBR_BANDS]; QMF_DELAY + MAX_SLOTS * 2];
        self.old_bw_array       = [0.0; SBR_BANDS];
        self.last_envelope      = [0; SBR_BANDS];
        self.last_noise_env     = [0; SBR_BANDS];
        self.last_freq_res      = false;
        self.last_env_end       = 0;
        self.prev_num_env       = 0;
        self.old_invf_mode      = [0; NUM_PATCHES];
        self.prev_s_idx_mapped  = [false; SBR_BANDS];
        self.index_sine         = 0;
        self.index_noise        = 0;
    }

    pub fn set_amp_res(&mut self, amp_res: bool) {
        if self.fclass != FrameClass::FixFix || self.num_env != 1 {
            self.amp_res = amp_res;
        } else {
            self.amp_res = false;
        }
    }

    pub fn analysis(&mut self, dsp: &mut SBRDSP, src: &[f32]) {
        for (src, dst) in src.chunks(32).zip(self.w[QMF_DELAY..].iter_mut()) {
            self.sbr_a.analysis(dsp, src, dst);
        }
    }
    pub fn synthesis(&mut self, dsp: &mut SBRDSP, dst: &mut [f32]) {
        for (src, dst) in self.x.iter().zip(dst.chunks_mut(64)) {
            self.sbr_s.synthesis(dsp, src, dst);
        }
    }
    pub fn bypass(&mut self) {
        self.x.copy_from_slice(&self.w);
    }
    pub fn hf_generate(&mut self, state: &SBRState) {
        for (x, w) in self.x.iter_mut().zip(self.w.iter()) {
            x[..state.k_x].copy_from_slice(&w[..state.k_x]);
            for el in x[state.k_x..].iter_mut() {
                *el = FFTC_ZERO;
            }
        }

        let mut phi = [[[FFTC_ZERO; SBR_BANDS]; 3]; 3];
        let mut a0 = [FFTC_ZERO; SBR_BANDS];
        let mut a1 = [FFTC_ZERO; SBR_BANDS];
        let k0 = state.f[0];
        for (i, phi) in phi.iter_mut().enumerate() {
            for (j, phi) in phi.iter_mut().enumerate().skip(1) {
                let src0 = &self.x[HF_ADJ - i..][..MAX_SLOTS * 2 + 6 - 1];
                let src1 = &self.x[HF_ADJ - j..][..MAX_SLOTS * 2 + 6 - 1];
                for (slot0, slot1) in src0.iter().zip(src1.iter()) {
                    for (k, phi) in phi.iter_mut().take(k0).enumerate() {
                        *phi += slot0[k] * !slot1[k];
                    }
                }
            }
        }
        for (k, (a0, a1)) in a0.iter_mut().zip(a1.iter_mut()).take(k0).enumerate() {
            let phi12 = phi[1][2][k];
            let d_k = phi[2][2][k].re * phi[1][1][k].re - phi12.sq_modulus() / (1.0 + 1.0e-6);

            if d_k != 0.0 {
                *a1 = (phi[0][1][k] * phi[1][2][k] - phi[0][2][k] * phi[1][1][k]).scale(1.0 / d_k);
            }
            if phi[1][1][k].re != 0.0 {
                *a0 = -(phi[0][1][k] + *a1 * !phi[1][2][k]).scale(1.0 / phi[1][1][k].re);
            }
            if a0.sq_modulus() >= 4.0*4.0 || a1.sq_modulus() >= 4.0*4.0 {
                *a0 = FFTC_ZERO;
                *a1 = FFTC_ZERO;
            }
        }

        for k in 0..state.num_noise_bands {
            let new_bw = NEW_BW[self.old_invf_mode[k] as usize][self.invf_mode[k] as usize];
            let old_bw = self.old_bw_array[k];
            let temp_bw = if new_bw < old_bw {
                    0.75    * new_bw + 0.25    * old_bw
                } else {
                    0.90625 * new_bw + 0.09375 * old_bw
                };
            self.bw_array[k] = if temp_bw >= 0.015625 {
                    temp_bw
                } else {
                    0.0
                };
        }

        for (l, x_high) in self.x_high.iter_mut().enumerate().skip(HF_ADJ).take(self.env_border[self.num_env] * 2).skip(self.env_border[0]) {
            *x_high = [FFTC_ZERO; SBR_BANDS];
            let mut dst_k = state.k_x;
            for (&patch_start, &patch_len) in state.patch_start_subband[..state.num_patches].iter().zip(state.patch_num_subbands.iter()) {
                for (k, dst) in x_high[dst_k..][..patch_len].iter_mut().enumerate() {
                    let p = patch_start + k;
                    let cur_x = self.x[l][p];
                    let prev_x = self.x[l - 1][p];
                    let pprev_x = self.x[l - 2][p];
                    let g_k = match state.f_noise[..state.num_noise_bands].binary_search(&k) {
                            Ok(idx) | Err(idx) => idx.min(state.num_noise_bands - 1),
                        };
                    let bw_val = self.bw_array[g_k];
                    *dst = cur_x + a0[p] * prev_x.scale(bw_val) + a1[p] * pprev_x.scale(bw_val * bw_val);
                }
                dst_k += patch_len;
            }
        }
    }
    pub fn hf_adjust(&mut self, state: &SBRState, hdr: &SBRHeader) {
        const LIM_GAIN: [f32; 4] = [0.70795, 1.0, 1.41254, 10000.0];
        const H_SMOOTH: [f32; 5] = [
            1.0 / 3.0, 0.30150283239582, 0.21816949906249, 0.11516383427084, 0.03183050093751
        ];
        const PHI: [FFTComplex; 4] = [
            FFTComplex{ re:  1.0, im:  0.0 },
            FFTComplex{ re:  0.0, im:  1.0 },
            FFTComplex{ re: -1.0, im:  0.0 },
            FFTComplex{ re:  0.0, im: -1.0 },
        ];

        let kx = state.k_x;
        let km = state.f[state.num_master];
        let envelope_start = self.env_border[0];
        let envelope_end = self.env_border[self.num_env];

        let high_start = state.f[..=state.num_master].binary_search(&state.k_x).unwrap();
        let f_high = &state.f[..=state.num_master][high_start..];
        let f_low = &state.f_low[..=state.num_env_bands[0]];

        let l_a = match (self.fclass, self.pointer) {
                (_, 0) => -1,
                (FrameClass::FixFix, _) => -1,
                (FrameClass::FixVar, _) |
                (FrameClass::VarVar, _) => (self.num_env as i8) + 1 - (self.pointer as i8),
                (FrameClass::VarFix, 1) => -1,
                (FrameClass::VarFix, _) => (self.pointer as i8) - 1,
            };

        self.s_idx_mapped = [[false; SBR_BANDS]; NUM_ENVELOPES];
        for (l, s_idx_mapped) in self.s_idx_mapped[..self.num_env].iter_mut().enumerate() {
            let mut start = f_high[0];
            for (i, &end) in f_high.iter().skip(1).enumerate() {
                if self.add_harmonic[i] {
                    let mid = (start + end) / 2;
                    if ((l as i8) >= l_a) || self.prev_s_idx_mapped[mid] {
                        s_idx_mapped[mid] = true;
                    }
                }
                start = end;
            }
        }
        self.prev_s_idx_mapped = self.s_idx_mapped[self.num_env - 1];

        let mut s_mapped = [[false; SBR_BANDS]; NUM_ENVELOPES];
        for ((s_mapped, s_idx_mapped), &freq_res) in s_mapped.iter_mut().zip(self.s_idx_mapped[..self.num_env].iter()).zip(self.freq_res.iter()) {
            let mut band_start = kx;
            if freq_res {
                for (&add_sine, &band_end) in self.add_harmonic.iter().zip(f_high[1..].iter()) {
                    for el in s_mapped[band_start..band_end].iter_mut() {
                        *el = add_sine;
                    }
                    band_start = band_end;
                }
            } else {
                for &band_end in f_low[1..].iter() {
                    let add_sine = s_idx_mapped[band_start..band_end].contains(&true);
                    for el in s_mapped[band_start..band_end].iter_mut() {
                        *el = add_sine;
                    }
                    band_start = band_end;
                }
            }
        }

        let mut e_orig_mapped = [[0.0; SBR_BANDS]; NUM_ENVELOPES];
        let (a, pan_offset) = if self.amp_res { (1.0, 12.0) } else { (0.5, 24.0) };
        match self.qmode {
            QuantMode::Single => {
                for (dst, (src, &freq_res)) in e_orig_mapped[..self.num_env].iter_mut().zip(self.data_env.iter().zip(self.freq_res.iter())) {
                    let bands = if freq_res { f_high } else { f_low };
                    let mut start = kx;
                    for (&val, &band_end) in src.iter().zip(bands.iter().skip(1)) {
                        let scale = 2.0f32.powf(6.0 + f32::from(val) * a);
                        for dst in dst[start..band_end].iter_mut() {
                            *dst = scale;
                        }
                        start = band_end;
                    }
                }
            },
            QuantMode::Left => {
                for (dst, ((e0, e1), &freq_res)) in e_orig_mapped[..self.num_env].iter_mut().zip(self.data_env.iter().zip(self.data_env2.iter()).zip(self.freq_res.iter())) {
                    let bands = if freq_res { f_high } else { f_low };
                    let mut start = kx;
                    for ((&e0, &e1), &band_end) in e0.iter().zip(e1.iter()).zip(bands.iter().skip(1)) {
                        let scale = 2.0f32.powf(6.0 + f32::from(e0) * a + 1.0) / (1.0 + 2.0f32.powf((pan_offset - f32::from(e1)) * a));
                        for dst in dst[start..band_end].iter_mut() {
                            *dst = scale;
                        }
                        start = band_end;
                    }
                }
            },
            QuantMode::Right => {
                for (dst, ((e0, e1), &freq_res)) in e_orig_mapped[..self.num_env].iter_mut().zip(self.data_env2.iter().zip(self.data_env.iter()).zip(self.freq_res.iter())) {
                    let bands = if freq_res { f_high } else { f_low };
                    let mut start = kx;
                    for ((&e0, &e1), &band_end) in e0.iter().zip(e1.iter()).zip(bands.iter().skip(1)) {
                        let scale = 2.0f32.powf(6.0 + f32::from(e0) * a + 1.0) / (1.0 + 2.0f32.powf((f32::from(e1) - pan_offset) * a));
                        for dst in dst[start..band_end].iter_mut() {
                            *dst = scale;
                        }
                        start = band_end;
                    }
                }
            }
        };
        let mut q_mapped = [[0.0; SBR_BANDS]; NUM_ENVELOPES];
        let mut start = self.env_border[0];
        let noise_env = self.noise_env_border;
        match self.qmode {
            QuantMode::Single => {
                for (env_no, &env_end) in self.env_border[1..=self.num_env].iter().enumerate() {
                    let env_end = env_end;
                    let mut noise_env_no = 0;
                    for nenv in 0..self.num_noise {
                        if (start >= noise_env[nenv]) && (env_end <= noise_env[nenv + 1]) {
                            noise_env_no = nenv;
                            break;
                        }
                    }
                    let mut band_start = state.f_noise[0];
                    for (noise_band, &band_end) in state.f_noise[1..=state.num_noise_bands].iter().enumerate() {
                        let scale = 2.0f32.powf(6.0 - f32::from(self.data_noise[noise_env_no][noise_band]));
                        for el in q_mapped[env_no][band_start..band_end].iter_mut() {
                            *el = scale;
                        }
                        band_start = band_end;
                    }
                    start = env_end;
                }
            },
            QuantMode::Left => {
                for (env_no, &env_end) in self.env_border[1..=self.num_env].iter().enumerate() {
                    let env_end = env_end;
                    let mut noise_env_no = 0;
                    for nenv in 0..self.num_noise {
                        if (start >= noise_env[nenv]) && (env_end <= noise_env[nenv + 1]) {
                            noise_env_no = nenv;
                            break;
                        }
                    }
                    let mut band_start = state.f_noise[0];
                    for (noise_band, &band_end) in state.f_noise[1..=state.num_noise_bands].iter().enumerate() {
                        let scale = 2.0f32.powf(6.0 - f32::from(self.data_noise[noise_env_no][noise_band]) + 1.0) / (1.0 + 2.0f32.powf(12.0 - f32::from(self.data_noise2[noise_env_no][noise_band])));
                        for el in q_mapped[env_no][band_start..band_end].iter_mut() {
                            *el = scale;
                        }
                        band_start = band_end;
                    }
                    start = env_end;
                }
            },
            QuantMode::Right => {
                for (env_no, &env_end) in self.env_border[1..=self.num_env].iter().enumerate() {
                    let env_end = env_end;
                    let mut noise_env_no = 0;
                    for nenv in 0..self.num_noise {
                        if (start >= noise_env[nenv]) && (env_end <= noise_env[nenv + 1]) {
                            noise_env_no = nenv;
                            break;
                        }
                    }
                    let mut band_start = state.f_noise[0];
                    for (noise_band, &band_end) in state.f_noise[1..=state.num_noise_bands].iter().enumerate() {
                        let scale = 2.0f32.powf(6.0 - f32::from(self.data_noise2[noise_env_no][noise_band]) + 1.0) / (1.0 + 2.0f32.powf(f32::from(self.data_noise[noise_env_no][noise_band]) - 12.0));
                        for el in q_mapped[env_no][band_start..band_end].iter_mut() {
                            *el = scale;
                        }
                        band_start = band_end;
                    }
                    start = env_end;
                }
            },
        };

        let mut start = self.env_border[0];
        let mut e_curr = [[0.0f32; SBR_BANDS]; NUM_ENVELOPES];
        for (e_curr, &env_end) in e_curr.iter_mut().zip(self.env_border[1..=self.num_env].iter()) {
            for slot in self.x_high[HF_ADJ..][(start * 2)..(env_end * 2)].iter() {
                for (dst, x) in e_curr[kx..km].iter_mut().zip(slot[kx..km].iter()) {
                    *dst += x.sq_modulus();
                }
            }
            for el in e_curr[kx..km].iter_mut() {
                *el *= RANGE * RANGE;
                *el /= ((env_end - start) * 2) as f32;
            }
            start = env_end;
        }

        const EPS: f32 = 1.0;
        const EPS0: f32 = 1.0e-12;
        let la_prev = if self.prev_l_a == (self.prev_num_env as i8) { 0 } else { -1 };
        let mut g_max_tmp;
        let mut g_boost_tmp;
        let mut g = [0.0; SBR_BANDS];
        let mut q_m = [0.0; SBR_BANDS];
        let mut s_m = [0.0; SBR_BANDS];
        let mut q_m_lim = [0.0; SBR_BANDS];
        let mut g_lim = [0.0; SBR_BANDS];
        let mut g_lim_boost = [[0.0; SBR_BANDS]; NUM_ENVELOPES];
        let mut q_m_lim_boost = [[0.0; SBR_BANDS]; NUM_ENVELOPES];
        let mut s_m_boost = [[0.0; SBR_BANDS]; NUM_ENVELOPES];
        for env in 0..self.num_env {
            let mut start = kx;
            g_max_tmp = [0.0; SBR_BANDS];
            g_boost_tmp = [0.0; SBR_BANDS];
            for (dst, &end) in g_max_tmp.iter_mut().zip(state.f_lim[1..=state.num_lim].iter()) {
                let mut e_o_sum = EPS0;
                let mut e_c_sum = EPS0;
                for k in start..end {
                    e_o_sum += e_orig_mapped[env][k];
                    e_c_sum += e_curr[env][k];
                }
                *dst = (e_o_sum / e_c_sum).sqrt() * LIM_GAIN[hdr.limiter_gains as usize];
                start = end;
            }
            for k in kx..km {
                let e_orig = e_orig_mapped[env][k];
                let q_orig = q_mapped[env][k];
                let e_curr = e_curr[env][k];

                q_m[k] = (e_orig * q_orig / (1.0 + q_orig)).sqrt();
                s_m[k] = if self.s_idx_mapped[env][k] { (e_orig / (1.0 + q_orig)).sqrt() } else { 0.0 };
                g[k] = if !s_mapped[env][k] {
                        let q_add = if (env as i8) != l_a && (env as i8) != la_prev { q_orig } else { 0.0 };
                        (e_orig / ((EPS + e_curr) * (1.0 + q_add))).sqrt()
                    } else {
                        (e_orig / (EPS + e_curr) * q_orig / (1.0 + q_orig)).sqrt()
                    };

                let mut lidx = 0;
                for i in 0..state.num_lim {
                    if (state.f_lim[i] <= k) && (k < state.f_lim[i + 1]) {
                        lidx = i;
                        break;
                    }
                }

                let g_max = g_max_tmp[lidx].min(1.0e5);
                q_m_lim[k] = q_m[k].min(q_m[k] * g_max / g[k]);
                g_lim[k] = g[k].min(g_max);
            }
            let mut start = kx;
            for (lim_no, dst) in g_boost_tmp[..state.num_lim].iter_mut().enumerate() {
                let end = state.f_lim[lim_no + 1];
                let mut nsum = EPS0;
                let mut dsum = EPS0;
                for k in start..end {
                    nsum += e_orig_mapped[env][k];
                    dsum += e_curr[env][k] * g_lim[k] * g_lim[k];
                    if s_m[k] != 0.0 || (env as i8) == l_a || (env as i8) == la_prev {
                        dsum += s_m[k] * s_m[k];
                    } else {
                        dsum += q_m_lim[k] * q_m_lim[k];
                    }
                }
                *dst = (nsum / dsum).sqrt();
                let g_boost = dst.min(1.584893192);
                for k in start..end {
                    g_lim_boost[env][k] = g_lim[k] * g_boost;
                    q_m_lim_boost[env][k] = q_m_lim[k] * g_boost;
                    s_m_boost[env][k] = s_m[k] * g_boost;
                }
                start = end;
            }
        }

        let mut env_map = [0; MAX_SLOTS * 2 + QMF_DELAY];
        let mut start = self.env_border[0];
        for (env, &env_end) in self.env_border[1..=self.num_env].iter().enumerate() {
            for l in (start * 2)..(env_end * 2) {
                env_map[l] = env;
            }
            start = env_end;
        }

        let (ghead, gcur) = self.g_temp.split_at_mut(SMOOTH_DELAY);
        let (qhead, qcur) = self.q_temp.split_at_mut(SMOOTH_DELAY);
        if self.last_env_end > 0 {
            ghead.copy_from_slice(&gcur[self.last_env_end - SMOOTH_DELAY..][..SMOOTH_DELAY]);
            qhead.copy_from_slice(&qcur[self.last_env_end - SMOOTH_DELAY..][..SMOOTH_DELAY]);
            let mut start = self.env_border[0];
            for (&env_end, (g_lim, q_lim)) in self.env_border[1..=self.num_env].iter().zip(g_lim_boost.iter().zip(q_m_lim_boost.iter())) {
                for slot in (start * 2)..(env_end * 2) {
                    gcur[slot] = *g_lim;
                    qcur[slot] = *q_lim;
                }
                start = env_end;
            }
        } else {
            for dst in ghead.iter_mut() {
                *dst = g_lim_boost[0];
            }
            for dst in qhead.iter_mut() {
                *dst = q_m_lim_boost[0];
            }
            let mut start = 0;
            for (&env_end, (g_lim, q_lim)) in self.env_border[1..=self.num_env].iter().zip(g_lim_boost.iter().zip(q_m_lim_boost.iter())) {
                for slot in (start * 2)..(env_end * 2) {
                    gcur[slot] = *g_lim;
                    qcur[slot] = *q_lim;
                }
                start = env_end;
            }
        }

        let mut g_filt = [[0.0; SBR_BANDS]; MAX_SLOTS * 2 + QMF_DELAY];
        let mut q_filt = [[0.0; SBR_BANDS]; MAX_SLOTS * 2 + QMF_DELAY];
        if !hdr.smoothing_mode {
            for slot in (envelope_start * 2)..(envelope_end * 2) {
                if (slot as i8) == (la_prev * 2) {
                    g_filt[slot].copy_from_slice(&self.g_temp[slot + SMOOTH_DELAY]);
                    q_filt[slot].copy_from_slice(&self.q_temp[slot + SMOOTH_DELAY]);
                    continue;
                }
                for k in kx..km {
                    let mut gsum = 0.0;
                    let mut qsum = 0.0;
                    for (i, &coef) in H_SMOOTH.iter().enumerate() {
                        gsum += self.g_temp[slot + SMOOTH_DELAY - i][k] * coef;
                        qsum += self.q_temp[slot + SMOOTH_DELAY - i][k] * coef;
                    }
                    g_filt[slot][k] = gsum;
                    q_filt[slot][k] = qsum;
                }
            }
        } else {
            g_filt.copy_from_slice(gcur);
            q_filt.copy_from_slice(qcur);
        }

        let index_noise = self.index_noise.wrapping_sub(self.env_border[0] * 2) & 511;
        for (slot, y) in self.y.iter_mut().skip(HF_ADJ).take(envelope_end * 2).skip(envelope_start * 2).enumerate() {
            for (k, y) in y.iter_mut().enumerate().skip(kx).take(km - kx) {
                *y = self.x_high[HF_ADJ + slot][k].scale(g_filt[slot][k]);

                let smb = s_m_boost[env_map[slot]][k] / RANGE;
                if smb != 0.0 {
                    let mut s = FFTComplex { re: smb, im: smb };
                    if (k & 1) != 0 {
                        s = !s;
                    }
                    y.re += s.re * PHI[self.index_sine].re;
                    y.im += s.im * PHI[self.index_sine].im;
                } else {
                    *y += SBR_NOISE_TABLE[(index_noise + slot * SBR_BANDS + k - kx + 1) & 511].scale(q_filt[slot][k] / RANGE);
                }
            }
            self.index_sine = (self.index_sine + 1) & 3;
        }
        self.index_noise = (index_noise + km - kx) & 511;

        let end = if self.last_env_end != 0 {
                (self.last_env_end - MAX_SLOTS) * 2
            } else {
                0
            };
        self.last_env_end = envelope_end;
        for (i, x) in self.x[..end].iter_mut().enumerate() {
            x[state.k_x..].copy_from_slice(&self.prev_y[HF_ADJ + MAX_SLOTS * 2 + i][state.k_x..]);
        }
        for (x, y) in self.x[end..].iter_mut().zip(self.y[HF_ADJ + end..].iter()) {
            x[state.k_x..].copy_from_slice(&y[state.k_x..]);
        }

        self.prev_l_a = l_a;
    }
    pub fn update_frame(&mut self) {
        let (start, tail) = self.w.split_at_mut(QMF_DELAY);
        start.copy_from_slice(&tail[tail.len() - QMF_DELAY..]);
        self.prev_y = self.y;
        self.old_invf_mode = self.invf_mode;
        self.old_bw_array = self.bw_array;

        self.prev_num_env = self.num_env;
    }
}

const SBR_NOISE_TABLE: [FFTComplex; 512] = [
    FFTComplex{ re: -0.99948153278296, im: -0.59483417516607 },
    FFTComplex{ re:  0.97113454393991, im: -0.67528515225647 },
    FFTComplex{ re:  0.14130051758487, im: -0.95090983575689 },
    FFTComplex{ re: -0.47005496701697, im: -0.37340549728647 },
    FFTComplex{ re:  0.80705063769351, im:  0.29653668284408 },
    FFTComplex{ re: -0.38981478896926, im:  0.89572605717087 },
    FFTComplex{ re: -0.01053049862020, im: -0.66959058036166 },
    FFTComplex{ re: -0.91266367957293, im: -0.11522938140034 },
    FFTComplex{ re:  0.54840422910309, im:  0.75221367176302 },
    FFTComplex{ re:  0.40009252867955, im: -0.98929400334421 },
    FFTComplex{ re: -0.99867974711855, im: -0.88147068645358 },
    FFTComplex{ re: -0.95531076805040, im:  0.90908757154593 },
    FFTComplex{ re: -0.45725933317144, im: -0.56716323646760 },
    FFTComplex{ re: -0.72929675029275, im: -0.98008272727324 },
    FFTComplex{ re:  0.75622801399036, im:  0.20950329995549 },
    FFTComplex{ re:  0.07069442601050, im: -0.78247898470706 },
    FFTComplex{ re:  0.74496252926055, im: -0.91169004445807 },
    FFTComplex{ re: -0.96440182703856, im: -0.94739918296622 },
    FFTComplex{ re:  0.30424629369539, im: -0.49438267012479 },
    FFTComplex{ re:  0.66565033746925, im:  0.64652935542491 },
    FFTComplex{ re:  0.91697008020594, im:  0.17514097332009 },
    FFTComplex{ re: -0.70774918760427, im:  0.52548653416543 },
    FFTComplex{ re: -0.70051415345560, im: -0.45340028808763 },
    FFTComplex{ re: -0.99496513054797, im: -0.90071908066973 },
    FFTComplex{ re:  0.98164490790123, im: -0.77463155528697 },
    FFTComplex{ re: -0.54671580548181, im: -0.02570928536004 },
    FFTComplex{ re: -0.01689629065389, im:  0.00287506445732 },
    FFTComplex{ re: -0.86110349531986, im:  0.42548583726477 },
    FFTComplex{ re: -0.98892980586032, im: -0.87881132267556 },
    FFTComplex{ re:  0.51756627678691, im:  0.66926784710139 },
    FFTComplex{ re: -0.99635026409640, im: -0.58107730574765 },
    FFTComplex{ re: -0.99969370862163, im:  0.98369989360250 },
    FFTComplex{ re:  0.55266258627194, im:  0.59449057465591 },
    FFTComplex{ re:  0.34581177741673, im:  0.94879421061866 },
    FFTComplex{ re:  0.62664209577999, im: -0.74402970906471 },
    FFTComplex{ re: -0.77149701404973, im: -0.33883658042801 },
    FFTComplex{ re: -0.91592244254432, im:  0.03687901376713 },
    FFTComplex{ re: -0.76285492357887, im: -0.91371867919124 },
    FFTComplex{ re:  0.79788337195331, im: -0.93180971199849 },
    FFTComplex{ re:  0.54473080610200, im: -0.11919206037186 },
    FFTComplex{ re: -0.85639281671058, im:  0.42429854760451 },
    FFTComplex{ re: -0.92882402971423, im:  0.27871809078609 },
    FFTComplex{ re: -0.11708371046774, im: -0.99800843444966 },
    FFTComplex{ re:  0.21356749817493, im: -0.90716295627033 },
    FFTComplex{ re: -0.76191692573909, im:  0.99768118356265 },
    FFTComplex{ re:  0.98111043100884, im: -0.95854459734407 },
    FFTComplex{ re: -0.85913269895572, im:  0.95766566168880 },
    FFTComplex{ re: -0.93307242253692, im:  0.49431757696466 },
    FFTComplex{ re:  0.30485754879632, im: -0.70540034357529 },
    FFTComplex{ re:  0.85289650925190, im:  0.46766131791044 },
    FFTComplex{ re:  0.91328082618125, im: -0.99839597361769 },
    FFTComplex{ re: -0.05890199924154, im:  0.70741827819497 },
    FFTComplex{ re:  0.28398686150148, im:  0.34633555702188 },
    FFTComplex{ re:  0.95258164539612, im: -0.54893416026939 },
    FFTComplex{ re: -0.78566324168507, im: -0.75568541079691 },
    FFTComplex{ re: -0.95789495447877, im: -0.20423194696966 },
    FFTComplex{ re:  0.82411158711197, im:  0.96654618432562 },
    FFTComplex{ re: -0.65185446735885, im: -0.88734990773289 },
    FFTComplex{ re: -0.93643603134666, im:  0.99870790442385 },
    FFTComplex{ re:  0.91427159529618, im: -0.98290505544444 },
    FFTComplex{ re: -0.70395684036886, im:  0.58796798221039 },
    FFTComplex{ re:  0.00563771969365, im:  0.61768196727244 },
    FFTComplex{ re:  0.89065051931895, im:  0.52783352697585 },
    FFTComplex{ re: -0.68683707712762, im:  0.80806944710339 },
    FFTComplex{ re:  0.72165342518718, im: -0.69259857349564 },
    FFTComplex{ re: -0.62928247730667, im:  0.13627037407335 },
    FFTComplex{ re:  0.29938434065514, im: -0.46051329682246 },
    FFTComplex{ re: -0.91781958879280, im: -0.74012716684186 },
    FFTComplex{ re:  0.99298717043688, im:  0.40816610075661 },
    FFTComplex{ re:  0.82368298622748, im: -0.74036047190173 },
    FFTComplex{ re: -0.98512833386833, im: -0.99972330709594 },
    FFTComplex{ re: -0.95915368242257, im: -0.99237800466040 },
    FFTComplex{ re: -0.21411126572790, im: -0.93424819052545 },
    FFTComplex{ re: -0.68821476106884, im: -0.26892306315457 },
    FFTComplex{ re:  0.91851997982317, im:  0.09358228901785 },
    FFTComplex{ re: -0.96062769559127, im:  0.36099095133739 },
    FFTComplex{ re:  0.51646184922287, im: -0.71373332873917 },
    FFTComplex{ re:  0.61130721139669, im:  0.46950141175917 },
    FFTComplex{ re:  0.47336129371299, im: -0.27333178296162 },
    FFTComplex{ re:  0.90998308703519, im:  0.96715662938132 },
    FFTComplex{ re:  0.44844799194357, im:  0.99211574628306 },
    FFTComplex{ re:  0.66614891079092, im:  0.96590176169121 },
    FFTComplex{ re:  0.74922239129237, im: -0.89879858826087 },
    FFTComplex{ re: -0.99571588506485, im:  0.52785521494349 },
    FFTComplex{ re:  0.97401082477563, im: -0.16855870075190 },
    FFTComplex{ re:  0.72683747733879, im: -0.48060774432251 },
    FFTComplex{ re:  0.95432193457128, im:  0.68849603408441 },
    FFTComplex{ re: -0.72962208425191, im: -0.76608443420917 },
    FFTComplex{ re: -0.85359479233537, im:  0.88738125901579 },
    FFTComplex{ re: -0.81412430338535, im: -0.97480768049637 },
    FFTComplex{ re: -0.87930772356786, im:  0.74748307690436 },
    FFTComplex{ re: -0.71573331064977, im: -0.98570608178923 },
    FFTComplex{ re:  0.83524300028228, im:  0.83702537075163 },
    FFTComplex{ re: -0.48086065601423, im: -0.98848504923531 },
    FFTComplex{ re:  0.97139128574778, im:  0.80093621198236 },
    FFTComplex{ re:  0.51992825347895, im:  0.80247631400510 },
    FFTComplex{ re: -0.00848591195325, im: -0.76670128000486 },
    FFTComplex{ re: -0.70294374303036, im:  0.55359910445577 },
    FFTComplex{ re: -0.95894428168140, im: -0.43265504344783 },
    FFTComplex{ re:  0.97079252950321, im:  0.09325857238682 },
    FFTComplex{ re: -0.92404293670797, im:  0.85507704027855 },
    FFTComplex{ re: -0.69506469500450, im:  0.98633412625459 },
    FFTComplex{ re:  0.26559203620024, im:  0.73314307966524 },
    FFTComplex{ re:  0.28038443336943, im:  0.14537913654427 },
    FFTComplex{ re: -0.74138124825523, im:  0.99310339807762 },
    FFTComplex{ re: -0.01752795995444, im: -0.82616635284178 },
    FFTComplex{ re: -0.55126773094930, im: -0.98898543862153 },
    FFTComplex{ re:  0.97960898850996, im: -0.94021446752851 },
    FFTComplex{ re: -0.99196309146936, im:  0.67019017358456 },
    FFTComplex{ re: -0.67684928085260, im:  0.12631491649378 },
    FFTComplex{ re:  0.09140039465500, im: -0.20537731453108 },
    FFTComplex{ re: -0.71658965751996, im: -0.97788200391224 },
    FFTComplex{ re:  0.81014640078925, im:  0.53722648362443 },
    FFTComplex{ re:  0.40616991671205, im: -0.26469008598449 },
    FFTComplex{ re: -0.67680188682972, im:  0.94502052337695 },
    FFTComplex{ re:  0.86849774348749, im: -0.18333598647899 },
    FFTComplex{ re: -0.99500381284851, im: -0.02634122068550 },
    FFTComplex{ re:  0.84329189340667, im:  0.10406957462213 },
    FFTComplex{ re: -0.09215968531446, im:  0.69540012101253 },
    FFTComplex{ re:  0.99956173327206, im: -0.12358542001404 },
    FFTComplex{ re: -0.79732779473535, im: -0.91582524736159 },
    FFTComplex{ re:  0.96349973642406, im:  0.96640458041000 },
    FFTComplex{ re: -0.79942778496547, im:  0.64323902822857 },
    FFTComplex{ re: -0.11566039853896, im:  0.28587846253726 },
    FFTComplex{ re: -0.39922954514662, im:  0.94129601616966 },
    FFTComplex{ re:  0.99089197565987, im: -0.92062625581587 },
    FFTComplex{ re:  0.28631285179909, im: -0.91035047143603 },
    FFTComplex{ re: -0.83302725605608, im: -0.67330410892084 },
    FFTComplex{ re:  0.95404443402072, im:  0.49162765398743 },
    FFTComplex{ re: -0.06449863579434, im:  0.03250560813135 },
    FFTComplex{ re: -0.99575054486311, im:  0.42389784469507 },
    FFTComplex{ re: -0.65501142790847, im:  0.82546114655624 },
    FFTComplex{ re: -0.81254441908887, im: -0.51627234660629 },
    FFTComplex{ re: -0.99646369485481, im:  0.84490533520752 },
    FFTComplex{ re:  0.00287840603348, im:  0.64768261158166 },
    FFTComplex{ re:  0.70176989408455, im: -0.20453028573322 },
    FFTComplex{ re:  0.96361882270190, im:  0.40706967140989 },
    FFTComplex{ re: -0.68883758192426, im:  0.91338958840772 },
    FFTComplex{ re: -0.34875585502238, im:  0.71472290693300 },
    FFTComplex{ re:  0.91980081243087, im:  0.66507455644919 },
    FFTComplex{ re: -0.99009048343881, im:  0.85868021604848 },
    FFTComplex{ re:  0.68865791458395, im:  0.55660316809678 },
    FFTComplex{ re: -0.99484402129368, im: -0.20052559254934 },
    FFTComplex{ re:  0.94214511408023, im: -0.99696425367461 },
    FFTComplex{ re: -0.67414626793544, im:  0.49548221180078 },
    FFTComplex{ re: -0.47339353684664, im: -0.85904328834047 },
    FFTComplex{ re:  0.14323651387360, im: -0.94145598222488 },
    FFTComplex{ re: -0.29268293575672, im:  0.05759224927952 },
    FFTComplex{ re:  0.43793861458754, im: -0.78904969892724 },
    FFTComplex{ re: -0.36345126374441, im:  0.64874435357162 },
    FFTComplex{ re: -0.08750604656825, im:  0.97686944362527 },
    FFTComplex{ re: -0.96495267812511, im: -0.53960305946511 },
    FFTComplex{ re:  0.55526940659947, im:  0.78891523734774 },
    FFTComplex{ re:  0.73538215752630, im:  0.96452072373404 },
    FFTComplex{ re: -0.30889773919437, im: -0.80664389776860 },
    FFTComplex{ re:  0.03574995626194, im: -0.97325616900959 },
    FFTComplex{ re:  0.98720684660488, im:  0.48409133691962 },
    FFTComplex{ re: -0.81689296271203, im: -0.90827703628298 },
    FFTComplex{ re:  0.67866860118215, im:  0.81284503870856 },
    FFTComplex{ re: -0.15808569732583, im:  0.85279555024382 },
    FFTComplex{ re:  0.80723395114371, im: -0.24717418514605 },
    FFTComplex{ re:  0.47788757329038, im: -0.46333147839295 },
    FFTComplex{ re:  0.96367554763201, im:  0.38486749303242 },
    FFTComplex{ re: -0.99143875716818, im: -0.24945277239809 },
    FFTComplex{ re:  0.83081876925833, im: -0.94780851414763 },
    FFTComplex{ re: -0.58753191905341, im:  0.01290772389163 },
    FFTComplex{ re:  0.95538108220960, im: -0.85557052096538 },
    FFTComplex{ re: -0.96490920476211, im: -0.64020970923102 },
    FFTComplex{ re: -0.97327101028521, im:  0.12378128133110 },
    FFTComplex{ re:  0.91400366022124, im:  0.57972471346930 },
    FFTComplex{ re: -0.99925837363824, im:  0.71084847864067 },
    FFTComplex{ re: -0.86875903507313, im: -0.20291699203564 },
    FFTComplex{ re: -0.26240034795124, im: -0.68264554369108 },
    FFTComplex{ re: -0.24664412953388, im: -0.87642273115183 },
    FFTComplex{ re:  0.02416275806869, im:  0.27192914288905 },
    FFTComplex{ re:  0.82068619590515, im: -0.85087787994476 },
    FFTComplex{ re:  0.88547373760759, im: -0.89636802901469 },
    FFTComplex{ re: -0.18173078152226, im: -0.26152145156800 },
    FFTComplex{ re:  0.09355476558534, im:  0.54845123045604 },
    FFTComplex{ re: -0.54668414224090, im:  0.95980774020221 },
    FFTComplex{ re:  0.37050990604091, im: -0.59910140383171 },
    FFTComplex{ re: -0.70373594262891, im:  0.91227665827081 },
    FFTComplex{ re: -0.34600785879594, im: -0.99441426144200 },
    FFTComplex{ re: -0.68774481731008, im: -0.30238837956299 },
    FFTComplex{ re: -0.26843291251234, im:  0.83115668004362 },
    FFTComplex{ re:  0.49072334613242, im: -0.45359708737775 },
    FFTComplex{ re:  0.38975993093975, im:  0.95515358099121 },
    FFTComplex{ re: -0.97757125224150, im:  0.05305894580606 },
    FFTComplex{ re: -0.17325552859616, im: -0.92770672250494 },
    FFTComplex{ re:  0.99948035025744, im:  0.58285545563426 },
    FFTComplex{ re: -0.64946246527458, im:  0.68645507104960 },
    FFTComplex{ re: -0.12016920576437, im: -0.57147322153312 },
    FFTComplex{ re: -0.58947456517751, im: -0.34847132454388 },
    FFTComplex{ re: -0.41815140454465, im:  0.16276422358861 },
    FFTComplex{ re:  0.99885650204884, im:  0.11136095490444 },
    FFTComplex{ re: -0.56649614128386, im: -0.90494866361587 },
    FFTComplex{ re:  0.94138021032330, im:  0.35281916733018 },
    FFTComplex{ re: -0.75725076534641, im:  0.53650549640587 },
    FFTComplex{ re:  0.20541973692630, im: -0.94435144369918 },
    FFTComplex{ re:  0.99980371023351, im:  0.79835913565599 },
    FFTComplex{ re:  0.29078277605775, im:  0.35393777921520 },
    FFTComplex{ re: -0.62858772103030, im:  0.38765693387102 },
    FFTComplex{ re:  0.43440904467688, im: -0.98546330463232 },
    FFTComplex{ re: -0.98298583762390, im:  0.21021524625209 },
    FFTComplex{ re:  0.19513029146934, im: -0.94239832251867 },
    FFTComplex{ re: -0.95476662400101, im:  0.98364554179143 },
    FFTComplex{ re:  0.93379635304810, im: -0.70881994583682 },
    FFTComplex{ re: -0.85235410573336, im: -0.08342347966410 },
    FFTComplex{ re: -0.86425093011245, im: -0.45795025029466 },
    FFTComplex{ re:  0.38879779059045, im:  0.97274429344593 },
    FFTComplex{ re:  0.92045124735495, im: -0.62433652524220 },
    FFTComplex{ re:  0.89162532251878, im:  0.54950955570563 },
    FFTComplex{ re: -0.36834336949252, im:  0.96458298020975 },
    FFTComplex{ re:  0.93891760988045, im: -0.89968353740388 },
    FFTComplex{ re:  0.99267657565094, im: -0.03757034316958 },
    FFTComplex{ re: -0.94063471614176, im:  0.41332338538963 },
    FFTComplex{ re:  0.99740224117019, im: -0.16830494996370 },
    FFTComplex{ re: -0.35899413170555, im: -0.46633226649613 },
    FFTComplex{ re:  0.05237237274947, im: -0.25640361602661 },
    FFTComplex{ re:  0.36703583957424, im: -0.38653265641875 },
    FFTComplex{ re:  0.91653180367913, im: -0.30587628726597 },
    FFTComplex{ re:  0.69000803499316, im:  0.90952171386132 },
    FFTComplex{ re: -0.38658751133527, im:  0.99501571208985 },
    FFTComplex{ re: -0.29250814029851, im:  0.37444994344615 },
    FFTComplex{ re: -0.60182204677608, im:  0.86779651036123 },
    FFTComplex{ re: -0.97418588163217, im:  0.96468523666475 },
    FFTComplex{ re:  0.88461574003963, im:  0.57508405276414 },
    FFTComplex{ re:  0.05198933055162, im:  0.21269661669964 },
    FFTComplex{ re: -0.53499621979720, im:  0.97241553731237 },
    FFTComplex{ re: -0.49429560226497, im:  0.98183865291903 },
    FFTComplex{ re: -0.98935142339139, im: -0.40249159006933 },
    FFTComplex{ re: -0.98081380091130, im: -0.72856895534041 },
    FFTComplex{ re: -0.27338148835532, im:  0.99950922447209 },
    FFTComplex{ re:  0.06310802338302, im: -0.54539587529618 },
    FFTComplex{ re: -0.20461677199539, im: -0.14209977628489 },
    FFTComplex{ re:  0.66223843141647, im:  0.72528579940326 },
    FFTComplex{ re: -0.84764345483665, im:  0.02372316801261 },
    FFTComplex{ re: -0.89039863483811, im:  0.88866581484602 },
    FFTComplex{ re:  0.95903308477986, im:  0.76744927173873 },
    FFTComplex{ re:  0.73504123909879, im: -0.03747203173192 },
    FFTComplex{ re: -0.31744434966056, im: -0.36834111883652 },
    FFTComplex{ re: -0.34110827591623, im:  0.40211222807691 },
    FFTComplex{ re:  0.47803883714199, im: -0.39423219786288 },
    FFTComplex{ re:  0.98299195879514, im:  0.01989791390047 },
    FFTComplex{ re: -0.30963073129751, im: -0.18076720599336 },
    FFTComplex{ re:  0.99992588229018, im: -0.26281872094289 },
    FFTComplex{ re: -0.93149731080767, im: -0.98313162570490 },
    FFTComplex{ re:  0.99923472302773, im: -0.80142993767554 },
    FFTComplex{ re: -0.26024169633417, im: -0.75999759855752 },
    FFTComplex{ re: -0.35712514743563, im:  0.19298963768574 },
    FFTComplex{ re: -0.99899084509530, im:  0.74645156992493 },
    FFTComplex{ re:  0.86557171579452, im:  0.55593866696299 },
    FFTComplex{ re:  0.33408042438752, im:  0.86185953874709 },
    FFTComplex{ re:  0.99010736374716, im:  0.04602397576623 },
    FFTComplex{ re: -0.66694269691195, im: -0.91643611810148 },
    FFTComplex{ re:  0.64016792079480, im:  0.15649530836856 },
    FFTComplex{ re:  0.99570534804836, im:  0.45844586038111 },
    FFTComplex{ re: -0.63431466947340, im:  0.21079116459234 },
    FFTComplex{ re: -0.07706847005931, im: -0.89581437101329 },
    FFTComplex{ re:  0.98590090577724, im:  0.88241721133981 },
    FFTComplex{ re:  0.80099335254678, im: -0.36851896710853 },
    FFTComplex{ re:  0.78368131392666, im:  0.45506999802597 },
    FFTComplex{ re:  0.08707806671691, im:  0.80938994918745 },
    FFTComplex{ re: -0.86811883080712, im:  0.39347308654705 },
    FFTComplex{ re: -0.39466529740375, im: -0.66809432114456 },
    FFTComplex{ re:  0.97875325649683, im: -0.72467840967746 },
    FFTComplex{ re: -0.95038560288864, im:  0.89563219587625 },
    FFTComplex{ re:  0.17005239424212, im:  0.54683053962658 },
    FFTComplex{ re: -0.76910792026848, im: -0.96226617549298 },
    FFTComplex{ re:  0.99743281016846, im:  0.42697157037567 },
    FFTComplex{ re:  0.95437383549973, im:  0.97002324109952 },
    FFTComplex{ re:  0.99578905365569, im: -0.54106826257356 },
    FFTComplex{ re:  0.28058259829990, im: -0.85361420634036 },
    FFTComplex{ re:  0.85256524470573, im: -0.64567607735589 },
    FFTComplex{ re: -0.50608540105128, im: -0.65846015480300 },
    FFTComplex{ re: -0.97210735183243, im: -0.23095213067791 },
    FFTComplex{ re:  0.95424048234441, im: -0.99240147091219 },
    FFTComplex{ re: -0.96926570524023, im:  0.73775654896574 },
    FFTComplex{ re:  0.30872163214726, im:  0.41514960556126 },
    FFTComplex{ re: -0.24523839572639, im:  0.63206633394807 },
    FFTComplex{ re: -0.33813265086024, im: -0.38661779441897 },
    FFTComplex{ re: -0.05826828420146, im: -0.06940774188029 },
    FFTComplex{ re: -0.22898461455054, im:  0.97054853316316 },
    FFTComplex{ re: -0.18509915019881, im:  0.47565762892084 },
    FFTComplex{ re: -0.10488238045009, im: -0.87769947402394 },
    FFTComplex{ re: -0.71886586182037, im:  0.78030982480538 },
    FFTComplex{ re:  0.99793873738654, im:  0.90041310491497 },
    FFTComplex{ re:  0.57563307626120, im: -0.91034337352097 },
    FFTComplex{ re:  0.28909646383717, im:  0.96307783970534 },
    FFTComplex{ re:  0.42188998312520, im:  0.48148651230437 },
    FFTComplex{ re:  0.93335049681047, im: -0.43537023883588 },
    FFTComplex{ re: -0.97087374418267, im:  0.86636445711364 },
    FFTComplex{ re:  0.36722871286923, im:  0.65291654172961 },
    FFTComplex{ re: -0.81093025665696, im:  0.08778370229363 },
    FFTComplex{ re: -0.26240603062237, im: -0.92774095379098 },
    FFTComplex{ re:  0.83996497984604, im:  0.55839849139647 },
    FFTComplex{ re: -0.99909615720225, im: -0.96024605713970 },
    FFTComplex{ re:  0.74649464155061, im:  0.12144893606462 },
    FFTComplex{ re: -0.74774595569805, im: -0.26898062008959 },
    FFTComplex{ re:  0.95781667469567, im: -0.79047927052628 },
    FFTComplex{ re:  0.95472308713099, im: -0.08588776019550 },
    FFTComplex{ re:  0.48708332746299, im:  0.99999041579432 },
    FFTComplex{ re:  0.46332038247497, im:  0.10964126185063 },
    FFTComplex{ re: -0.76497004940162, im:  0.89210929242238 },
    FFTComplex{ re:  0.57397389364339, im:  0.35289703373760 },
    FFTComplex{ re:  0.75374316974495, im:  0.96705214651335 },
    FFTComplex{ re: -0.59174397685714, im: -0.89405370422752 },
    FFTComplex{ re:  0.75087906691890, im: -0.29612672982396 },
    FFTComplex{ re: -0.98607857336230, im:  0.25034911730023 },
    FFTComplex{ re: -0.40761056640505, im: -0.90045573444695 },
    FFTComplex{ re:  0.66929266740477, im:  0.98629493401748 },
    FFTComplex{ re: -0.97463695257310, im: -0.00190223301301 },
    FFTComplex{ re:  0.90145509409859, im:  0.99781390365446 },
    FFTComplex{ re: -0.87259289048043, im:  0.99233587353666 },
    FFTComplex{ re: -0.91529461447692, im: -0.15698707534206 },
    FFTComplex{ re: -0.03305738840705, im: -0.37205262859764 },
    FFTComplex{ re:  0.07223051368337, im: -0.88805001733626 },
    FFTComplex{ re:  0.99498012188353, im:  0.97094358113387 },
    FFTComplex{ re: -0.74904939500519, im:  0.99985483641521 },
    FFTComplex{ re:  0.04585228574211, im:  0.99812337444082 },
    FFTComplex{ re: -0.89054954257993, im: -0.31791913188064 },
    FFTComplex{ re: -0.83782144651251, im:  0.97637632547466 },
    FFTComplex{ re:  0.33454804933804, im: -0.86231516800408 },
    FFTComplex{ re: -0.99707579362824, im:  0.93237990079441 },
    FFTComplex{ re: -0.22827527843994, im:  0.18874759397997 },
    FFTComplex{ re:  0.67248046289143, im: -0.03646211390569 },
    FFTComplex{ re: -0.05146538187944, im: -0.92599700120679 },
    FFTComplex{ re:  0.99947295749905, im:  0.93625229707912 },
    FFTComplex{ re:  0.66951124390363, im:  0.98905825623893 },
    FFTComplex{ re: -0.99602956559179, im: -0.44654715757688 },
    FFTComplex{ re:  0.82104905483590, im:  0.99540741724928 },
    FFTComplex{ re:  0.99186510988782, im:  0.72023001312947 },
    FFTComplex{ re: -0.65284592392918, im:  0.52186723253637 },
    FFTComplex{ re:  0.93885443798188, im: -0.74895312615259 },
    FFTComplex{ re:  0.96735248738388, im:  0.90891816978629 },
    FFTComplex{ re: -0.22225968841114, im:  0.57124029781228 },
    FFTComplex{ re: -0.44132783753414, im: -0.92688840659280 },
    FFTComplex{ re: -0.85694974219574, im:  0.88844532719844 },
    FFTComplex{ re:  0.91783042091762, im: -0.46356892383970 },
    FFTComplex{ re:  0.72556974415690, im: -0.99899555770747 },
    FFTComplex{ re: -0.99711581834508, im:  0.58211560180426 },
    FFTComplex{ re:  0.77638976371966, im:  0.94321834873819 },
    FFTComplex{ re:  0.07717324253925, im:  0.58638399856595 },
    FFTComplex{ re: -0.56049829194163, im:  0.82522301569036 },
    FFTComplex{ re:  0.98398893639988, im:  0.39467440420569 },
    FFTComplex{ re:  0.47546946844938, im:  0.68613044836811 },
    FFTComplex{ re:  0.65675089314631, im:  0.18331637134880 },
    FFTComplex{ re:  0.03273375457980, im: -0.74933109564108 },
    FFTComplex{ re: -0.38684144784738, im:  0.51337349030406 },
    FFTComplex{ re: -0.97346267944545, im: -0.96549364384098 },
    FFTComplex{ re: -0.53282156061942, im: -0.91423265091354 },
    FFTComplex{ re:  0.99817310731176, im:  0.61133572482148 },
    FFTComplex{ re: -0.50254500772635, im: -0.88829338134294 },
    FFTComplex{ re:  0.01995873238855, im:  0.85223515096765 },
    FFTComplex{ re:  0.99930381973804, im:  0.94578896296649 },
    FFTComplex{ re:  0.82907767600783, im: -0.06323442598128 },
    FFTComplex{ re: -0.58660709669728, im:  0.96840773806582 },
    FFTComplex{ re: -0.17573736667267, im: -0.48166920859485 },
    FFTComplex{ re:  0.83434292401346, im: -0.13023450646997 },
    FFTComplex{ re:  0.05946491307025, im:  0.20511047074866 },
    FFTComplex{ re:  0.81505484574602, im: -0.94685947861369 },
    FFTComplex{ re: -0.44976380954860, im:  0.40894572671545 },
    FFTComplex{ re: -0.89746474625671, im:  0.99846578838537 },
    FFTComplex{ re:  0.39677256130792, im: -0.74854668609359 },
    FFTComplex{ re: -0.07588948563079, im:  0.74096214084170 },
    FFTComplex{ re:  0.76343198951445, im:  0.41746629422634 },
    FFTComplex{ re: -0.74490104699626, im:  0.94725911744610 },
    FFTComplex{ re:  0.64880119792759, im:  0.41336660830571 },
    FFTComplex{ re:  0.62319537462542, im: -0.93098313552599 },
    FFTComplex{ re:  0.42215817594807, im: -0.07712787385208 },
    FFTComplex{ re:  0.02704554141885, im: -0.05417518053666 },
    FFTComplex{ re:  0.80001773566818, im:  0.91542195141039 },
    FFTComplex{ re: -0.79351832348816, im: -0.36208897989136 },
    FFTComplex{ re:  0.63872359151636, im:  0.08128252493444 },
    FFTComplex{ re:  0.52890520960295, im:  0.60048872455592 },
    FFTComplex{ re:  0.74238552914587, im:  0.04491915291044 },
    FFTComplex{ re:  0.99096131449250, im: -0.19451182854402 },
    FFTComplex{ re: -0.80412329643109, im: -0.88513818199457 },
    FFTComplex{ re: -0.64612616129736, im:  0.72198674804544 },
    FFTComplex{ re:  0.11657770663191, im: -0.83662833815041 },
    FFTComplex{ re: -0.95053182488101, im: -0.96939905138082 },
    FFTComplex{ re: -0.62228872928622, im:  0.82767262846661 },
    FFTComplex{ re:  0.03004475787316, im: -0.99738896333384 },
    FFTComplex{ re: -0.97987214341034, im:  0.36526129686425 },
    FFTComplex{ re: -0.99986980746200, im: -0.36021610299715 },
    FFTComplex{ re:  0.89110648599879, im: -0.97894250343044 },
    FFTComplex{ re:  0.10407960510582, im:  0.77357793811619 },
    FFTComplex{ re:  0.95964737821728, im: -0.35435818285502 },
    FFTComplex{ re:  0.50843233159162, im:  0.96107691266205 },
    FFTComplex{ re:  0.17006334670615, im: -0.76854025314829 },
    FFTComplex{ re:  0.25872675063360, im:  0.99893303933816 },
    FFTComplex{ re: -0.01115998681937, im:  0.98496019742444 },
    FFTComplex{ re: -0.79598702973261, im:  0.97138411318894 },
    FFTComplex{ re: -0.99264708948101, im: -0.99542822402536 },
    FFTComplex{ re: -0.99829663752818, im:  0.01877138824311 },
    FFTComplex{ re: -0.70801016548184, im:  0.33680685948117 },
    FFTComplex{ re: -0.70467057786826, im:  0.93272777501857 },
    FFTComplex{ re:  0.99846021905254, im: -0.98725746254433 },
    FFTComplex{ re: -0.63364968534650, im: -0.16473594423746 },
    FFTComplex{ re: -0.16258217500792, im: -0.95939125400802 },
    FFTComplex{ re: -0.43645594360633, im: -0.94805030113284 },
    FFTComplex{ re: -0.99848471702976, im:  0.96245166923809 },
    FFTComplex{ re: -0.16796458968998, im: -0.98987511890470 },
    FFTComplex{ re: -0.87979225745213, im: -0.71725725041680 },
    FFTComplex{ re:  0.44183099021786, im: -0.93568974498761 },
    FFTComplex{ re:  0.93310180125532, im: -0.99913308068246 },
    FFTComplex{ re: -0.93941931782002, im: -0.56409379640356 },
    FFTComplex{ re: -0.88590003188677, im:  0.47624600491382 },
    FFTComplex{ re:  0.99971463703691, im: -0.83889954253462 },
    FFTComplex{ re: -0.75376385639978, im:  0.00814643438625 },
    FFTComplex{ re:  0.93887685615875, im: -0.11284528204636 },
    FFTComplex{ re:  0.85126435782309, im:  0.52349251543547 },
    FFTComplex{ re:  0.39701421446381, im:  0.81779634174316 },
    FFTComplex{ re: -0.37024464187437, im: -0.87071656222959 },
    FFTComplex{ re: -0.36024828242896, im:  0.34655735648287 },
    FFTComplex{ re: -0.93388812549209, im: -0.84476541096429 },
    FFTComplex{ re: -0.65298804552119, im: -0.18439575450921 },
    FFTComplex{ re:  0.11960319006843, im:  0.99899346780168 },
    FFTComplex{ re:  0.94292565553160, im:  0.83163906518293 },
    FFTComplex{ re:  0.75081145286948, im: -0.35533223142265 },
    FFTComplex{ re:  0.56721979748394, im: -0.24076836414499 },
    FFTComplex{ re:  0.46857766746029, im: -0.30140233457198 },
    FFTComplex{ re:  0.97312313923635, im: -0.99548191630031 },
    FFTComplex{ re: -0.38299976567017, im:  0.98516909715427 },
    FFTComplex{ re:  0.41025800019463, im:  0.02116736935734 },
    FFTComplex{ re:  0.09638062008048, im:  0.04411984381457 },
    FFTComplex{ re: -0.85283249275397, im:  0.91475563922421 },
    FFTComplex{ re:  0.88866808958124, im: -0.99735267083226 },
    FFTComplex{ re: -0.48202429536989, im: -0.96805608884164 },
    FFTComplex{ re:  0.27572582416567, im:  0.58634753335832 },
    FFTComplex{ re: -0.65889129659168, im:  0.58835634138583 },
    FFTComplex{ re:  0.98838086953732, im:  0.99994349600236 },
    FFTComplex{ re: -0.20651349620689, im:  0.54593044066355 },
    FFTComplex{ re: -0.62126416356920, im: -0.59893681700392 },
    FFTComplex{ re:  0.20320105410437, im: -0.86879180355289 },
    FFTComplex{ re: -0.97790548600584, im:  0.96290806999242 },
    FFTComplex{ re:  0.11112534735126, im:  0.21484763313301 },
    FFTComplex{ re: -0.41368337314182, im:  0.28216837680365 },
    FFTComplex{ re:  0.24133038992960, im:  0.51294362630238 },
    FFTComplex{ re: -0.66393410674885, im: -0.08249679629081 },
    FFTComplex{ re: -0.53697829178752, im: -0.97649903936228 },
    FFTComplex{ re: -0.97224737889348, im:  0.22081333579837 },
    FFTComplex{ re:  0.87392477144549, im: -0.12796173740361 },
    FFTComplex{ re:  0.19050361015753, im:  0.01602615387195 },
    FFTComplex{ re: -0.46353441212724, im: -0.95249041539006 },
    FFTComplex{ re: -0.07064096339021, im: -0.94479803205886 },
    FFTComplex{ re: -0.92444085484466, im: -0.10457590187436 },
    FFTComplex{ re: -0.83822593578728, im: -0.01695043208885 },
    FFTComplex{ re:  0.75214681811150, im: -0.99955681042665 },
    FFTComplex{ re: -0.42102998829339, im:  0.99720941999394 },
    FFTComplex{ re: -0.72094786237696, im: -0.35008961934255 },
    FFTComplex{ re:  0.78843311019251, im:  0.52851398958271 },
    FFTComplex{ re:  0.97394027897442, im: -0.26695944086561 },
    FFTComplex{ re:  0.99206463477946, im: -0.57010120849429 },
    FFTComplex{ re:  0.76789609461795, im: -0.76519356730966 },
    FFTComplex{ re: -0.82002421836409, im: -0.73530179553767 },
    FFTComplex{ re:  0.81924990025724, im:  0.99698425250579 },
    FFTComplex{ re: -0.26719850873357, im:  0.68903369776193 },
    FFTComplex{ re: -0.43311260380975, im:  0.85321815947490 },
    FFTComplex{ re:  0.99194979673836, im:  0.91876249766422 },
    FFTComplex{ re: -0.80692001248487, im: -0.32627540663214 },
    FFTComplex{ re:  0.43080003649976, im: -0.21919095636638 },
    FFTComplex{ re:  0.67709491937357, im: -0.95478075822906 },
    FFTComplex{ re:  0.56151770568316, im: -0.70693811747778 },
    FFTComplex{ re:  0.10831862810749, im: -0.08628837174592 },
    FFTComplex{ re:  0.91229417540436, im: -0.65987351408410 },
    FFTComplex{ re: -0.48972893932274, im:  0.56289246362686 },
    FFTComplex{ re: -0.89033658689697, im: -0.71656563987082 },
    FFTComplex{ re:  0.65269447475094, im:  0.65916004833932 },
    FFTComplex{ re:  0.67439478141121, im: -0.81684380846796 },
    FFTComplex{ re: -0.47770832416973, im: -0.16789556203025 },
    FFTComplex{ re: -0.99715979260878, im: -0.93565784007648 },
    FFTComplex{ re: -0.90889593602546, im:  0.62034397054380 },
    FFTComplex{ re: -0.06618622548177, im: -0.23812217221359 },
    FFTComplex{ re:  0.99430266919728, im:  0.18812555317553 },
    FFTComplex{ re:  0.97686402381843, im: -0.28664534366620 },
    FFTComplex{ re:  0.94813650221268, im: -0.97506640027128 },
    FFTComplex{ re: -0.95434497492853, im: -0.79607978501983 },
    FFTComplex{ re: -0.49104783137150, im:  0.32895214359663 },
    FFTComplex{ re:  0.99881175120751, im:  0.88993983831354 },
    FFTComplex{ re:  0.50449166760303, im: -0.85995072408434 },
    FFTComplex{ re:  0.47162891065108, im: -0.18680204049569 },
    FFTComplex{ re: -0.62081581361840, im:  0.75000676218956 },
    FFTComplex{ re: -0.43867015250812, im:  0.99998069244322 },
    FFTComplex{ re:  0.98630563232075, im: -0.53578899600662 },
    FFTComplex{ re: -0.61510362277374, im: -0.89515019899997 },
    FFTComplex{ re: -0.03841517601843, im: -0.69888815681179 },
    FFTComplex{ re: -0.30102157304644, im: -0.07667808922205 },
    FFTComplex{ re:  0.41881284182683, im:  0.02188098922282 },
    FFTComplex{ re: -0.86135454941237, im:  0.98947480909359 },
    FFTComplex{ re:  0.67226861393788, im: -0.13494389011014 },
    FFTComplex{ re: -0.70737398842068, im: -0.76547349325992 },
    FFTComplex{ re:  0.94044946687963, im:  0.09026201157416 },
    FFTComplex{ re: -0.82386352534327, im:  0.08924768823676 },
    FFTComplex{ re: -0.32070666698656, im:  0.50143421908753 },
    FFTComplex{ re:  0.57593163224487, im: -0.98966422921509 },
    FFTComplex{ re: -0.36326018419965, im:  0.07440243123228 },
    FFTComplex{ re:  0.99979044674350, im: -0.14130287347405 },
    FFTComplex{ re: -0.92366023326932, im: -0.97979298068180 },
    FFTComplex{ re: -0.44607178518598, im: -0.54233252016394 },
    FFTComplex{ re:  0.44226800932956, im:  0.71326756742752 },
    FFTComplex{ re:  0.03671907158312, im:  0.63606389366675 },
    FFTComplex{ re:  0.52175424682195, im: -0.85396826735705 },
    FFTComplex{ re: -0.94701139690956, im: -0.01826348194255 },
    FFTComplex{ re: -0.98759606946049, im:  0.82288714303073 },
    FFTComplex{ re:  0.87434794743625, im:  0.89399495655433 },
    FFTComplex{ re: -0.93412041758744, im:  0.41374052024363 },
    FFTComplex{ re:  0.96063943315511, im:  0.93116709541280 },
    FFTComplex{ re:  0.97534253457837, im:  0.86150930812689 },
    FFTComplex{ re:  0.99642466504163, im:  0.70190043427512 },
    FFTComplex{ re: -0.94705089665984, im: -0.29580042814306 },
    FFTComplex{ re:  0.91599807087376, im: -0.98147830385781 },
];
