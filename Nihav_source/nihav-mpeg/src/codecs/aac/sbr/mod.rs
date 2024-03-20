use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::io::bitreader::*;

mod bs;
mod dsp;
mod synth;

pub use bs::{SBRCodebooks, sbr_read_sce, sbr_read_cpe};
pub use dsp::SBRDSP;
pub use synth::SBRChannel;

pub const NUM_ENVELOPES: usize = 8;
pub const NUM_PATCHES:   usize = 5;
pub const SBR_BANDS:     usize = 64;
pub const QMF_DELAY:     usize = 8;
pub const HF_ADJ:        usize = 2;
pub const MAX_SLOTS:     usize = 16;

#[derive(Clone, Copy, Debug)]
pub struct SBRHeader {
    pub amp_res:        bool,
        start_freq:     usize,
        stop_freq:      usize,
        xover_band:     usize,
        freq_scale:     u8,
        alter_scale:    bool,
        noise_bands:    u8,
        limiter_bands:  u8,
        limiter_gains:  u8,
        interpol_freq:  bool,
        smoothing_mode: bool,
}

impl SBRHeader {
    pub fn new() -> Self {
        let mut obj: Self = unsafe { std::mem::zeroed() };
        obj.reset();
        obj
    }
    fn reset(&mut self) {
        self.freq_scale     = 2;
        self.alter_scale    = true;
        self.noise_bands    = 2;
        self.limiter_bands  = 2;
        self.limiter_gains  = 2;
        self.interpol_freq  = true;
        self.smoothing_mode = true;
    }

    pub fn read(br: &mut BitReader) -> DecoderResult<Self> {
        let mut sbrh = Self::new();
        sbrh.reset();
        sbrh.amp_res            = br.read_bool()?;
        sbrh.start_freq         = br.read(4)? as usize;
        sbrh.stop_freq          = br.read(4)? as usize;
        sbrh.xover_band         = br.read(3)? as usize;
                                  br.skip(2)?;
        let header_extra_1      = br.read_bool()?;
        let header_extra_2      = br.read_bool()?;
        if header_extra_1 {
            sbrh.freq_scale     = br.read(2)? as u8;
            sbrh.alter_scale    = br.read_bool()?;
            sbrh.noise_bands    = br.read(2)? as u8;
        }
        if header_extra_2 {
            sbrh.limiter_bands  = br.read(2)? as u8;
            sbrh.limiter_gains  = br.read(2)? as u8;
            sbrh.interpol_freq  = br.read_bool()?;
            if !sbrh.interpol_freq {
                return Err(DecoderError::NotImplemented);
            }
            sbrh.smoothing_mode = br.read_bool()?;
        }
        Ok(sbrh)
    }

    pub fn differs_from(&self, rval: &Self) -> bool {
        self.start_freq  != rval.start_freq ||
        self.stop_freq   != rval.stop_freq ||
        self.xover_band  != rval.xover_band ||
        self.freq_scale  != rval.freq_scale ||
        self.alter_scale != rval.alter_scale ||
        self.noise_bands != rval.noise_bands
    }
}

#[derive(Clone)]
pub struct SBRState {
    num_env_bands:          [usize; 2],
    num_master:             usize,
    num_noise_bands:        usize,
    num_lim:                usize,
    k_x:                    usize,
    low_to_high_res:        [usize; SBR_BANDS],
    high_to_low_res:        [usize; SBR_BANDS],
    f:                      [usize; SBR_BANDS],
    f_low:                  [usize; SBR_BANDS],
    f_noise:                [usize; SBR_BANDS],
    f_lim:                  [usize; SBR_BANDS],
    patch_num_subbands:     [usize; SBR_BANDS],
    patch_start_subband:    [usize; SBR_BANDS],
    num_patches:            usize,
}

impl SBRState {
    pub fn new() -> Self {
        Self {
            num_env_bands:          [0; 2],
            num_master:             0,
            num_noise_bands:        0,
            num_lim:                0,
            k_x:                    0,
            low_to_high_res:        [0; SBR_BANDS],
            high_to_low_res:        [0; SBR_BANDS],
            f:                      [0; SBR_BANDS],
            f_low:                  [0; SBR_BANDS],
            f_noise:                [0; SBR_BANDS],
            f_lim:                  [0; SBR_BANDS],
            patch_num_subbands:     [0; SBR_BANDS],
            patch_start_subband:    [0; SBR_BANDS],
            num_patches:            0,
        }
    }
    pub fn init(&mut self, hdr: &SBRHeader, srate: u32) -> DecoderResult<()> {
        let offset_tab = match srate {
                    0..=16000 => &SBR_OFFSETS[0],
                16001..=22050 => &SBR_OFFSETS[1],
                22051..=24000 => &SBR_OFFSETS[2],
                24001..=32000 => &SBR_OFFSETS[3],
                32001..=64000 => &SBR_OFFSETS[4],
                            _ => &SBR_OFFSETS[5],
            };
        let smult = match srate {
                    0..=31999 => 3000,
                32000..=63999 => 4000,
                            _ => 5000,
            };
        let start_min = (128 * smult     + srate / 2) / srate;
        let stop_min  = (128 * smult * 2 + srate / 2) / srate;

        let k0 = ((start_min as i32) + i32::from(offset_tab[hdr.start_freq])).max(0) as usize;
        let k2 = (match hdr.stop_freq {
                14 => 2 * k0,
                15 => 3 * k0,
                 _ => {
                    let mut stop_dk = [0; 14];
                    generate_vdk(&mut stop_dk, stop_min as usize, SBR_BANDS, 13);
                    let dk_sum: usize = stop_dk[..hdr.stop_freq].iter().sum();
                    (stop_min as usize) + dk_sum
                },
            }).min(SBR_BANDS);

        let max_bands = match srate {
                    0..=32000 => 48,
                32001..=47999 => 35,
                            _ => 32,
            };
        validate!(k2 - k0 <= max_bands);

        self.num_master = calculate_master_frequencies(&mut self.f, k0, k2, hdr);
        let num_high = self.num_master - hdr.xover_band;
        let num_low = (num_high + 1) / 2; // INT(num_high / 2) + (num_high - 2 * INT(num_high / 2))

        self.num_env_bands = [num_low, num_high];

        let f_high = &self.f[hdr.xover_band..];
        let m = f_high[num_high] - f_high[0];
        let k_x = f_high[0];
        self.k_x = k_x;
        self.f_low = [0; SBR_BANDS];
        if (num_high & 1) == 0 {
            for k in 0..=num_low {
                self.f_low[k] = f_high[k * 2];
            }
        } else {
            self.f_low[0] = f_high[0];
            for k in 1..=num_low {
                self.f_low[k] = f_high[k * 2 - 1];
            }
        }

        let high_src = &f_high[..=num_high];
        let low_src = &self.f_low[..=num_low];
        for (dst, low) in self.high_to_low_res.iter_mut().zip(low_src.iter()) {
            if let Ok(idx) = high_src.binary_search(low) {
                *dst = idx;
            } else {
                return Err(DecoderError::Bug);
            }
        }
        for (dst, high) in self.low_to_high_res.iter_mut().zip(high_src.iter()) {
            *dst = match low_src.binary_search(high) {
                    Ok(idx) => idx,
                    Err(idx) => idx - 1,
                };
        }

        let num_q = (((hdr.noise_bands as f32) * ((k2 as f32) / (k_x as f32)).log2()).round() as usize).max(1);
        self.num_noise_bands = num_q;
        self.f_noise = [0; SBR_BANDS];
        let mut prev = 0;
        self.f_noise[0] = self.f_low[0];
        for k in 1..=num_q {
            let idx = prev + ((num_low - prev) / (num_q + 1 - k));
            self.f_noise[k] = self.f_low[idx];
            prev = idx;
        }

        let mut num_patches = 0;
        self.patch_num_subbands = [0; SBR_BANDS];
        self.patch_start_subband = [0; SBR_BANDS];
        let mut msb = k0;
        let mut usb = k_x;
        let goal_sb = ((2048000 + srate / 2) / srate) as usize;
        let last_band = k_x + m;
        let mut k = if goal_sb < last_band {
                let mut kk = 0;
                for i in 0..self.num_master {
                    if self.f[i] >= goal_sb {
                        break;
                    }
                    kk = i + 1;
                }
                kk
            } else {
                self.num_master
            };
        loop {
            let mut sb;
            let mut odd;
            let mut j = k;
            loop {
                sb = self.f[j];
                odd = (sb - 2 + k0) & 1;
                if sb <= k0 + msb - 1 - odd {
                    break;
                }
                j -= 1;
            }

            self.patch_num_subbands[num_patches] = sb.saturating_sub(usb);
            self.patch_start_subband[num_patches] = k0 - odd - self.patch_num_subbands[num_patches];

            if self.patch_num_subbands[num_patches] > 0 {
                usb = sb;
                msb = sb;
                num_patches += 1;
            } else {
                msb = k_x;
            }

            if self.f[k] < sb + 3 {
                k = self.num_master;
            }

            if sb == last_band {
                break;
            }
        }
        if (num_patches > 1) && (self.patch_num_subbands[num_patches - 1] < 3) {
            num_patches -= 1;
        }
        validate!(num_patches <= NUM_PATCHES);
        self.num_patches = num_patches;

        self.f_lim = [0; SBR_BANDS];
        let num_l = if hdr.limiter_bands == 0 {
                self.f_lim[0] = self.f_low[0];
                self.f_lim[1] = self.f_low[num_low];
                1
            } else {
                let lim_bands = match hdr.limiter_bands {
                        1 => 1.2f32,
                        2 => 2.0,
                        _ => 3.0,
                    };
                let mut patch_borders = [0; NUM_PATCHES + 1];
                patch_borders[0] = k_x;
                for k in 0..num_patches {
                    patch_borders[k + 1] = patch_borders[k] + self.patch_num_subbands[k];
                }
                self.f_lim = self.f_low;
                let f_lim_ptr = &mut self.f_lim[..num_low + num_patches];
                for &pborder in patch_borders[1..num_patches].iter() {
                    let mut i = 0;
                    for &el in f_lim_ptr.iter() {
                        if el > pborder {
                            break;
                        }
                        i += 1;
                    }
                    for j in (i..num_low + num_patches - 1).rev() {
                        f_lim_ptr[j + 1] = f_lim_ptr[j];
                    }
                    f_lim_ptr[i] = pborder;
                }
                let mut nr_lim = num_low + num_patches - 1;
                let mut k = 1;
                let pbord = &patch_borders[..=num_patches];
                while k <= nr_lim {
                    let n_octaves = ((self.f_lim[k] as f32) / (self.f_lim[k - 1] as f32)).log2();
                    if (n_octaves * lim_bands) < 0.49 {
                        if self.f_lim[k] == self.f_lim[k - 1] || !pbord.contains(&self.f_lim[k]) {
                            for l in k..nr_lim {
                                self.f_lim[l] = self.f_lim[l + 1];
                            }
                            nr_lim -= 1;
                        } else if !pbord.contains(&self.f_lim[k - 1]) {
                            for l in (k - 1)..nr_lim {
                                self.f_lim[l] = self.f_lim[l + 1];
                            }
                            nr_lim -= 1;
                        } else {
                            k += 1;
                        }
                    } else {
                        k += 1;
                    }
                }

                nr_lim
            };
        self.num_lim = num_l;

        Ok(())
    }
}

fn generate_vdk(v_dk: &mut [usize], k0: usize, k1: usize, num_bands: usize) {
    let mut last = k0;
    let k0 = k0 as f64;
    let factor = (k1 as f64) / k0;
    for k in 0..num_bands {
        let next = (k0 * factor.powf((k + 1) as f64 / (num_bands as f64))).round() as usize;
        let newval = next - last;
        last = next;
        let mut idx = k;
        for (j, &el) in v_dk[..k].iter().enumerate() {
            if newval < el {
                idx = j;
                break;
            }
        }
        for j in (idx..k).rev() {
            v_dk[j + 1] = v_dk[j];
        }
        v_dk[idx] = newval;
    }
}

fn calculate_master_frequencies(f: &mut [usize; SBR_BANDS], k0: usize, k2: usize, hdr: &SBRHeader) -> usize {
    if hdr.freq_scale == 0 {
        let (dk, num_bands) = if !hdr.alter_scale {
                (1, 2 * ((k2 - k0) / 2))
            } else {
                (2, 2 * ((k2 - k0 + 2) / (2 * 2)))
            };
        let k2_achieved = k0 + num_bands * dk;
        let mut k2_diff = (k2 as isize) - (k2_achieved as isize);
        let mut v_dk = [dk; SBR_BANDS];
        if k2_diff < 0 {
            let mut k = 0;
            while k2_diff != 0 {
                v_dk[k] -= 1;
                k += 1;
                k2_diff += 1;
            }
        } else if k2_diff > 0 {
            let mut k = num_bands - 1;
            while k2_diff != 0 {
                v_dk[k] += 1;
                k -= 1;
                k2_diff -= 1;
            }
        }
        f[0] = k0;
        for i in 0..num_bands {
            f[i + 1] = f[i] + v_dk[i];
        }

        num_bands
    } else {
        let bands = 14 - hdr.freq_scale * 2;
        let warp = if !hdr.alter_scale { 1.0f32 } else { 1.3f32 };
        let two_regions = (k2 as f32) / (k0 as f32) > 2.2449;
        let k1 = if two_regions { 2 * k0 } else { k2 };
        let num_bands0 = 2 * (((bands as f32) * ((k1 as f32) / (k0 as f32)).log2() / 2.0).round() as usize);
        let mut v_dk0 = [0; SBR_BANDS];
        generate_vdk(&mut v_dk0, k0, k1, num_bands0);
        let mut v_k0 = [0; SBR_BANDS];
        v_k0[0] = k0;
        for i in 0..num_bands0 {
            v_k0[i + 1] = v_k0[i] + v_dk0[i];
        }

        if two_regions {
            let num_bands1 = 2 * (((bands as f32) * ((k2 as f32) / (k1 as f32)).log2() / (2.0 * warp)).round() as usize);
            let mut v_dk1 = [0; SBR_BANDS];
            generate_vdk(&mut v_dk1, k1, k2, num_bands1);
            let max_vdk0 = v_dk0[num_bands0 - 1];
            if v_dk1[0] < max_vdk0 {
                let change = (max_vdk0 - v_dk1[0]).min((v_dk1[num_bands1 - 1] - v_dk1[0]) / 2);
                v_dk1[0] += change;
                v_dk1[num_bands1 - 1] -= change;
            }
            let mut v_k1 = [0; SBR_BANDS];
            v_k1[0] = k1;
            for i in 0..num_bands1 {
                v_k1[i + 1] = v_k1[i] + v_dk1[i];
            }
            f[..=num_bands0].copy_from_slice(&v_k0[..=num_bands0]);
            f[num_bands0 + 1..][..=num_bands1].copy_from_slice(&v_k1[1..][..=num_bands1]);
            num_bands0 + num_bands1
        } else {
            f[..=num_bands0].copy_from_slice(&v_k0[..=num_bands0]);
            num_bands0
        }
    }
}

const SBR_OFFSETS: [[i8; 16]; 6] = [
    [ -8, -7, -6, -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5,  6,  7 ], // 16kHz
    [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5,  6,  7,  9, 11, 13 ], // 22kHz
    [ -5, -3, -2, -1,  0,  1,  2,  3,  4,  5,  6,  7,  9, 11, 13, 16 ], // 24kHz
    [ -6, -4, -2, -1,  0,  1,  2,  3,  4,  5,  6,  7,  9, 11, 13, 16 ], // 32kHz
    [ -4, -2, -1,  0,  1,  2,  3,  4,  5,  6,  7,  9, 11, 13, 16, 20 ], // 44kHz
    [ -2, -1,  0,  1,  2,  3,  4,  5,  6,  7,  9, 11, 13, 16, 20, 24 ]  // 64kHz
];
