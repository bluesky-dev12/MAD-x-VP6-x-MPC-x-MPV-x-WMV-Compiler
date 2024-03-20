use std::mem;
use std::ptr;
use std::f32::consts;

use nihav_core::formats::*;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_codec_support::dsp::fft::*;
use nihav_codec_support::dsp::window::*;

const BANDS:      usize =  32;
const COEFFS:     usize = 256;
const BLOCK_SIZE: usize =  64;

struct IMDCTContext {
    pretwiddle1: [f32; COEFFS/2],
    pretwiddle2: [f32; COEFFS/2],
    posttwiddle: [FFTComplex; COEFFS/2],
    tmp:         [FFTComplex; COEFFS/2],
    fft:         FFT,
    window:      [f32; COEFFS],
}

struct IMCChannel {
    old_floor:  [f32; BANDS],
    new_floor:  [f32; BANDS],
    log_floor:  [f32; BANDS],
    log_floor2: [f32; BANDS],
    bit_est:    [f32; BANDS],
    mask_wght:  [f32; BANDS],
    adj_floor:  [f32; BANDS],
    cw:         [f32; COEFFS],
    last_im:    [f32; COEFFS/2],
}

struct BitAlloc {
    band_width:     [usize; BANDS],
    band_present:   [bool; BANDS],
    band_skip:      [bool; BANDS],
    band_bits:      [u8; BANDS],
    cw_len:         [u8; COEFFS],
    band_bitsum:    [usize; BANDS],
    skip_flag:      [bool; COEFFS],
    skip_flag_bits: [u8; BANDS],
    skips_per_band: [usize; BANDS],
    keep_flag:      [bool; BANDS],
    coeff:          [u8; COEFFS],
}

impl IMCChannel {
    fn new() -> Self {
        IMCChannel {
            old_floor:  [0.0; BANDS],
            new_floor:  [0.0; BANDS],
            log_floor:  [0.0; BANDS],
            log_floor2: [0.0; BANDS],
            bit_est:    [0.0; BANDS],
            mask_wght:  [0.0; BANDS],
            adj_floor:  [0.0; BANDS],
            cw:         [0.0; COEFFS],
            last_im:    [0.0; COEFFS/2],
        }
    }
    fn reset(&mut self) {
        for i in 0..self.old_floor.len() { self.old_floor[i] = 1.0; }
        for i in 0..self.cw.len()        { self.cw[i]        = 0.0; }
    }
}

const BITALLOC_LIMIT: f32     = -1.0e20;
const BITALLOC_TOP_LIMIT: f32 =  1.0e20;
impl BitAlloc {
    fn new() -> Self {
        BitAlloc {
            band_width:     [0; BANDS],
            band_present:   [false; BANDS],
            band_skip:      [false; BANDS],
            band_bits:      [0; BANDS],
            cw_len:         [0; COEFFS],
            band_bitsum:    [0; BANDS],
            skip_flag:      [false; COEFFS],
            skip_flag_bits: [0; BANDS],
            skips_per_band: [0; BANDS],
            keep_flag:      [false; BANDS],
            coeff:          [0; COEFFS],
        }
    }
    fn reset(&mut self) {
        for i in 0..BANDS {
            self.band_width[i]      = 0;
            self.band_present[i]    = false;
            self.band_skip[i]       = false;
            self.band_bits[i]       = 0;
            self.keep_flag[i]       = false;
            self.band_bitsum[i]     = 0;
            self.skips_per_band[i]  = 0;
            self.skip_flag_bits[i]  = 0;
        }
        for i in 0..COEFFS {
            self.cw_len[i]          = 0;
            self.skip_flag[i]       = false;
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn calculate_bit_allocation(&mut self, ch_data: &mut IMCChannel, bits: usize, fixed_head: bool, adj_idx: usize) -> DecoderResult<()> {

        let mut peak = 0.0;
        for coef in ch_data.new_floor.iter() { if *coef > peak { peak = *coef; } }
        peak *= 0.25;

        for band in 0..BANDS-1 {
            ch_data.bit_est[band] = ch_data.log_floor2[band] - ch_data.mask_wght[band].log2();
        }
        ch_data.bit_est[BANDS - 1] = BITALLOC_LIMIT;

        for band in 0..BANDS {
            let mut idx = 42;
            let band_w = IMC_BANDS[band + 1] - IMC_BANDS[band];
            if band_w   == self.band_width[band] { idx = 0; }
            if band_w   >  self.band_width[band] { idx = 1; }
            if band_w/2 >= self.band_width[band] { idx = 2; }
            validate!(idx <= 2);
            idx *= 2;
            if ch_data.new_floor[band] < peak { idx += 1; }
            ch_data.bit_est[band] += IMC_BITALLOC_ADJ[adj_idx][idx];
        }

        if fixed_head {
            for i in 0..4 {
                ch_data.bit_est[i] = BITALLOC_LIMIT;
            }
        }

        let start = if fixed_head { 4 } else { 0 };

        let mut a_width = 0;
        let mut pool = 0.0;
        for band in start..BANDS-1 {
            a_width += self.band_width[band];
            pool    += (self.band_width[band] as f32) * ch_data.bit_est[band];
        }
        validate!(a_width > 0);

        self.band_width[BANDS - 1] = 0;
        pool = (pool * 0.5 - (bits as f32)) / (a_width as f32);

        let free_bits = bits as i32;
        let mut cur_bits: i32 = 0;
        let mut flag = 1;
        let mut mmcount = 0;
        for i in 0..BANDS/2 {
            let diff = cur_bits - free_bits;
            if diff.abs() <= 8 { break; }

            cur_bits = 0;
            let mut acc = 0;
            for band in start..BANDS {
                let mut len = (ch_data.bit_est[band] * 0.5 - pool + 0.5) as i32;
                if len < 0 { len = 0; }
                if len > 6 { len = 6; }
                self.band_bits[band] = len as u8;
                cur_bits += (self.band_width[band] as i32) * len;
                if len > 0 {
                    acc += self.band_width[band] as i32;
                }
            }

            let mut lflag = flag;
            flag = 1;
            if free_bits < cur_bits { flag = -1; }
            if i == 0 { lflag = flag; }
            if flag != lflag {
                mmcount += 1;
            }
            pool += ((cur_bits - free_bits) as f32) / (((mmcount + 1) * acc) as f32);
        }

        for band in start..BANDS {
            for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                self.cw_len[i] = self.band_bits[band];
            }
        }

        if free_bits > cur_bits {
            let mut tmp: [f32; BANDS] = [BITALLOC_LIMIT; BANDS];
            for band in 0..BANDS {
                if self.band_bits[band] != 6 {
                    tmp[band] = f32::from(self.band_bits[band]) * -2.0 + ch_data.bit_est[band] - 0.415;
                }
            }
            let mut peak = 0.0;
            while (peak > BITALLOC_LIMIT) && (cur_bits < free_bits) {
                peak = BITALLOC_LIMIT;
                let mut idx: Option<usize> = None;
                for band in 0..BANDS {
                    if tmp[band] > peak {
                        peak = tmp[band];
                        idx = Some(band);
                    }
                }
                if let Some(band) = idx {
                    tmp[band] -= 2.0;
                    self.band_bits[band] += 1;
                    if self.band_bits[band] == 6 {
                        tmp[band] = BITALLOC_LIMIT;
                    }
                    for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                        self.cw_len[i] += 1;
                        cur_bits       += 1;
                        if cur_bits >= free_bits {
                            break;
                        }
                    }
                }
            }
        }

        if cur_bits > free_bits {
            let mut tmp: [f32; BANDS] = [BITALLOC_TOP_LIMIT; BANDS];
            for band in start..BANDS {
                if self.band_bits[band] != 0 {
                    tmp[band] = f32::from(self.band_bits[band]) * -2.0 + ch_data.bit_est[band] - 0.415 + 2.0;
                }
            }
            while free_bits < cur_bits {
                let mut low = BITALLOC_TOP_LIMIT;
                let mut idx = 0;
                for band in 0..BANDS {
                    if tmp[band] < low {
                        low = tmp[band];
                        idx = band;
                    }
                }
                tmp[idx] += 2.0;
                self.band_bits[idx] -= 1;
                if self.band_bits[idx] == 0 {
                    tmp[idx] = BITALLOC_TOP_LIMIT;
                }
                for i in IMC_BANDS[idx]..IMC_BANDS[idx + 1] {
                    if self.cw_len[i] > 0 {
                        self.cw_len[i] -= 1;
                        cur_bits       -= 1;
                        if cur_bits <= free_bits {
                            break;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn adjust_bit_allocation(&mut self, ch_data: &mut IMCChannel, free_bits: i32) {
        let mut tmp: [f32; BANDS] = [BITALLOC_LIMIT; BANDS];
        for band in 0..BANDS {
            if self.band_bits[band] != 6 {
                tmp[band] = f32::from(self.band_bits[band]) * -2.0 + ch_data.bit_est[band] - 0.415;
            }
        }
        let mut used_bits: i32 = 0;
        let mut peak = 0.0;
        while (peak > BITALLOC_LIMIT) && (used_bits < free_bits) {
            peak = BITALLOC_LIMIT;
            let mut idx: Option<usize> = None;
            for band in 0..BANDS {
                if tmp[band] > peak {
                    peak = tmp[band];
                    idx = Some(band);
                }
            }
            if let Some(band) = idx {
                tmp[band] -= 2.0;
                self.band_bits[band] += 1;
                if self.band_bits[band] == 6 {
                    tmp[band] = BITALLOC_LIMIT;
                }
                for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                    if self.cw_len[i] >= 6 { continue; }
                    self.cw_len[i] += 1;
                    used_bits      += 1;
                    if used_bits >= free_bits {
                        break;
                    }
                }
            }
        }
    }
}

struct LUTs {
    exp_lev:  [f32; 16],
    exp_10:   [f32; 32],
    sqrt_tab: [f32; 32],
}

impl LUTs {
    fn new() -> Self {
        let mut exp_lev: [f32; 16] = [0.0; 16];
        for lev in 0..16 {
            exp_lev[lev] = 10.0f32.powf(-(lev as f32) * 0.4375);
        }

        let mut exp_10: [f32; 32] = [0.0; 32];
        for i in 0..32 {
            exp_10[i] = 10.0f32.powf(((i as f32) - 16.0) * 0.25);
        }

        let mut sqrt_tab: [f32; 32] = [0.0; 32];
        for i in 0..32 {
            sqrt_tab[i] = (i as f32).sqrt();
        }

        LUTs { exp_lev, exp_10, sqrt_tab }
    }
}

struct IMCDecoder {
    is_imc: bool,

    chmap:  NAChannelMap,
    ainfo:  NAAudioInfo,
    info:   NACodecInfoRef,

    codes:  [[Codebook<u8>; 4]; 4],
    ch_data: [IMCChannel; 2],
    ba:     BitAlloc,
    imdct:  IMDCTContext,

    cycle1: [usize; BANDS],
    cycle2: [usize; BANDS],
    weights1: [f32; BANDS-1],
    weights2: [f32; BANDS-1],

    luts:   LUTs,
}

fn freq2bark(freq: f32) -> f32 {
    3.5 * ((freq / 7500.0) * (freq / 7500.0)).atan() + 13.0 * (freq * 0.00076).atan()
}

fn calc_maxcoef(coef: f32) -> (f32, f32) {
    let c1 = 20000.0 / 10.0f32.powf(coef * 0.057031251);
    (c1, c1.log2())
}

impl IMCDecoder {
    fn new(is_imc: bool) -> Self {
        let mut cycle1: [usize; BANDS] = [0; BANDS];
        let mut cycle2: [usize; BANDS] = [0; BANDS];
        let mut weights1: [f32; BANDS-1] = [0.0; BANDS-1];
        let mut weights2: [f32; BANDS-1] = [0.0; BANDS-1];
        if is_imc {
            cycle1.copy_from_slice(&IMC_CYCLE1);
            cycle2.copy_from_slice(&IMC_CYCLE2);
            weights1.copy_from_slice(&IMC_WEIGHTS1);
            weights2.copy_from_slice(&IMC_WEIGHTS2);
        }
        let codes = unsafe {
                let mut ucodes: mem::MaybeUninit::<[[Codebook<u8>; 4]; 4]> = mem::MaybeUninit::uninit();
                for i in 0..4 {
                    for j in 0..4 {
                        let mut cr = IMCCodeReader::new(i, j);
                        ptr::write(&mut (*ucodes.as_mut_ptr())[i][j], Codebook::new(&mut cr, CodebookMode::MSB).unwrap());
                    }
                }
                ucodes.assume_init()
            };
        IMCDecoder {
            is_imc,
            chmap:      NAChannelMap::new(),
            ainfo:      NAAudioInfo::new(0, 0, SND_F32P_FORMAT, 0),
            info:       NACodecInfo::new_dummy(),

            codes,
            ch_data:    [IMCChannel::new(), IMCChannel::new()],
            ba:         BitAlloc::new(),
            imdct:      IMDCTContext::new(),
            luts:       LUTs::new(),

            cycle1,
            cycle2,
            weights1,
            weights2,
        }
    }

    fn generate_iac_tables(&mut self, sample_rate: f32) {
        let scale = sample_rate / 256.0 / 2.0 * 0.5;
        let nyq_freq = sample_rate / 2.0;
        let mut last_bark = 0.0;
        let mut freq_max: [f32; BANDS] = [0.0; BANDS];
        let mut freq_mid: [f32; BANDS] = [0.0; BANDS];
        let mut freq_min: [f32; BANDS] = [0.0; BANDS];
        for band in 0..BANDS {
            let freq = ((IMC_BANDS[band] + IMC_BANDS[band + 1] - 1) as f32) * scale;
            let bark = freq2bark(freq);
            if band > 0 {
                let bark_diff = bark - last_bark;
                self.weights1[band - 1] = 10.0f32.powf(-1.0 * bark_diff);
                self.weights2[band - 1] = 10.0f32.powf(-2.7 * bark_diff);
            }
            last_bark = bark;
            freq_mid[band] = freq;

            let mut tmp_freq = freq;
            while tmp_freq < nyq_freq {
                tmp_freq += 0.5;
                if freq2bark(tmp_freq) > bark + 0.5 { break; }
            }
            freq_max[band] = tmp_freq;

            let mut tmp_freq = freq;
            while tmp_freq > 0.0 {
                tmp_freq -= 0.5;
                if freq2bark(tmp_freq) < bark - 0.5 { break; }
            }
            freq_min[band] = tmp_freq;
        }

        for band in 0..BANDS {
            let mut s_band = BANDS - 1;
            while s_band > 0 && freq_max[band] <= freq_mid[s_band] { s_band -= 1; }
            self.cycle1[band] = s_band + 1;
        }

        self.cycle2[0] = 0;
        for band in 1..BANDS {
            let mut s_band = 0;
            while s_band < BANDS-1 && freq_min[band] >= freq_mid[s_band] { s_band += 1; }
            self.cycle2[band] = s_band - 1;
        }
    }

    fn read_level_coeffs_raw(&mut self, br: &mut BitReader, ch: usize) -> DecoderResult<()> {
        let ch_data = &mut self.ch_data[ch];
        let maxc_pos = br.read(5)? as usize;
        let max_coef = br.read(7)? as u8;

        let (c1, c2) = calc_maxcoef(f32::from(max_coef));
        for i in 0..BANDS {
            if i != maxc_pos {
                let level = br.read(4)?;
                ch_data.new_floor[i] = c1 * self.luts.exp_lev[level as usize];
                ch_data.log_floor[i] = c2 - 1.4533435415 * (level as f32);
            } else {
                ch_data.new_floor[i] = c1;
                ch_data.log_floor[i] = c2;
            }
            self.ba.band_width[i]  = IMC_BANDS[i + 1] - IMC_BANDS[i];

            ch_data.log_floor2[i]  = ch_data.log_floor[i] * 2.0;
            ch_data.mask_wght[i]   = 1.0;
        }

        Ok(())
    }

    fn calculate_channel_values(&mut self, ch: usize) {
        let ch_data = &mut self.ch_data[ch];
        let mut tmp2: [f32; BANDS+1] = [0.0; BANDS+1];
        let mut tmp3: [f32; BANDS] = [0.0; BANDS];

        for band in 0..BANDS {
            ch_data.mask_wght[band] = 0.0;
            let val;
            if self.ba.band_width[band] > 0 {
                val = f64::from(ch_data.new_floor[band]).powi(2);
                ch_data.log_floor2[band] = 2.0 * ch_data.log_floor[band];
            } else {
                val = 0.0;
                ch_data.log_floor2[band] = -30000.0;
            }
            let tmp = val * (self.ba.band_width[band] as f64) * 0.01;
            if val <= 1.0e-30 { tmp3[band] = 0.0; }
            else { tmp3[band] = tmp as f32; }
        }

        for band in 0..BANDS {
            let next_band = self.cycle1[band];
            for band2 in band..next_band {
                ch_data.mask_wght[band2] += tmp3[band];
            }
            tmp2[next_band] += tmp3[band];
        }

        let mut accum = 0.0;
        for band in 1..BANDS {
            accum = (tmp2[band] + accum) * self.weights1[band - 1];
            ch_data.mask_wght[band] += accum;
        }

        let mut tmp2: [f32; BANDS] = [0.0; BANDS];
        tmp2[0] = tmp3[0];
        for band in 1..BANDS {
            let prev_band = self.cycle2[band];
            for band2 in prev_band+1..band {
                ch_data.mask_wght[band2] += tmp3[band];
            }
            tmp2[prev_band + 1] += tmp3[band];
        }

        let mut accum = 0.0;
        for i in 0..BANDS-1 {
            let band = BANDS - 2 - i;
            accum = (tmp2[band + 1] + accum) * self.weights2[band];
            ch_data.mask_wght[band] += accum;
        }
    }

    fn read_level_coeffs(&mut self, br: &mut BitReader, reset: bool, sel_idx: usize, ch: usize) -> DecoderResult<()> {
        let mut level: [i8; BANDS] = [0; BANDS];
        let start;
        if reset {
            start = 1;
            level[0] = br.read(7)? as i8;
        } else {
            start = 0;
        }
        for i in start..BANDS {
            level[i] = br.read_cb(&self.codes[sel_idx][IMC_CB_SELECTOR[sel_idx][i]])? as i8;
            if level[i] == 17 {
                level[i] += br.read(4)? as i8;
            }
            self.ba.keep_flag[i] = level[i] == 16;
        }
        if reset {
            let ch_data = &mut self.ch_data[ch];
            let (mut c1, mut c2) = calc_maxcoef(f32::from(level[0]));
            ch_data.new_floor[0] = c1;
            ch_data.log_floor[0] = c2;
            for i in 1..BANDS {
                if level[i] == 16 {
                    ch_data.new_floor[i] = 1.0;
                    ch_data.log_floor[i] = 0.0;
                } else {
                    let lval;
                    if level[i] < 17 {
                        lval = level[i] - 7;
                    } else if level[i] < 25 {
                        lval = level[i] - 32;
                    } else {
                        lval = level[i] - 16;
                    }
                    c1 *= self.luts.exp_10[(lval + 16) as usize];
                    c2 += 0.83048 * f32::from(lval);
                    ch_data.new_floor[i] = c1;
                    ch_data.log_floor[i] = c2;
                }
            }
        } else {
            let ch_data = &mut self.ch_data[ch];
            for i in 0..BANDS {
                if level[i] < 16 {
                    let lval = level[i] - 7;
                    ch_data.new_floor[i]  = self.luts.exp_10[(lval + 16) as usize] * ch_data.old_floor[i];
                    ch_data.log_floor[i] += f32::from(lval) * 0.83048;
                } else {
                    ch_data.new_floor[i] = ch_data.old_floor[i];
                }
            }
        }

        self.ba.band_width[0] = IMC_BANDS[1] - IMC_BANDS[0];
        for i in 1..BANDS {
            if level[i] != 16 {
                self.ba.band_width[i] = IMC_BANDS[i + 1] - IMC_BANDS[i];
            } else {
                self.ba.band_width[i] = 0;
            }
        }

        for i in 0..BANDS-1 {
            if self.ba.band_width[i] > 0 {
                self.ba.band_present[i] = br.read_bool()?;
            }
        }
        self.calculate_channel_values(ch);

        Ok(())
    }

    fn read_skip_flags(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let ba = &mut self.ba;
        for band in 0..BANDS {
            if !ba.band_present[band] || ba.band_width[band] == 0 { continue; }

            if !ba.band_skip[band] {
                ba.skip_flag_bits[band] = (IMC_BANDS[band + 1] - IMC_BANDS[band]) as u8;
                for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                    ba.skip_flag[i] = br.read_bool()?;
                    if ba.skip_flag[i] {
                        ba.skips_per_band[band] += 1;
                    }
                }
            } else {
                let mut i = IMC_BANDS[band];
                while i < IMC_BANDS[band + 1] - 1 {
                    if !br.read_bool()? {
                        ba.skip_flag_bits[band] += 1;
                        ba.skip_flag[i]          = true;
                        ba.skip_flag[i + 1]      = true;
                        ba.skips_per_band[band] += 2;
                    } else {
                        if br.read_bool()? {
                            ba.skip_flag_bits[band] += 2;
                            ba.skip_flag[i]          = false;
                            ba.skip_flag[i + 1]      = true;
                            ba.skips_per_band[band] += 1;
                        } else {
                            ba.skip_flag_bits[band] += 3;
                            if !br.read_bool()? {
                                ba.skip_flag[i]          = true;
                                ba.skips_per_band[band] += 1;
                            } else {
                                ba.skip_flag[i]          = false;
                            }
                            ba.skip_flag[i + 1]      = false;
                        }
                    }
                    i += 2;
                }
                if i != IMC_BANDS[band + 1] {
                    ba.skip_flag_bits[band] += 1;
                    ba.skip_flag[i] = br.read_bool()?;
                    if ba.skip_flag[i] {
                        ba.skips_per_band[band] += 1;
                    }
                }
            }
        }
        Ok(())
    }

    fn read_bitalloc_delta(&mut self, br: &mut BitReader, ch: usize) -> DecoderResult<()> {
        for band in 0..BANDS {
            self.ba.band_bitsum[band] = 0;
            self.ba.band_skip[band]   = false;
            for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                self.ba.band_bitsum[band] += self.ba.cw_len[i] as usize;
            }
            if self.ba.band_present[band] {
                let band_w = IMC_BANDS[band + 1] - IMC_BANDS[band];
                let bitsum = self.ba.band_bitsum[band];
                if (bitsum > 0) && (((band_w * 3) >> 1) > bitsum) {
                    self.ba.band_skip[band] = true;
                }
            }
        }

        self.read_skip_flags(br)?;

        let ch_data = &mut self.ch_data[ch];
        for band in 0..BANDS {
            ch_data.adj_floor[band] = ch_data.new_floor[band];
            let band_w = IMC_BANDS[band + 1] - IMC_BANDS[band];
            let nonskip = band_w - self.ba.skips_per_band[band];
            if self.ba.band_present[band] && nonskip > 0 {
                ch_data.adj_floor[band] *= self.luts.sqrt_tab[band_w] / self.luts.sqrt_tab[nonskip];
            }
        }

        let mut bits_freed: i32 = 0;
        for band in 0..BANDS {
            if !self.ba.band_present[band] { continue; }
            for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                if self.ba.skip_flag[i] {
                    bits_freed += i32::from(self.ba.cw_len[i]);
                    self.ba.cw_len[i] = 0;
                }
            }
            bits_freed -= i32::from(self.ba.skip_flag_bits[band]);
        }

        if bits_freed < 0 { return Err(DecoderError::Bug); }
        self.ba.adjust_bit_allocation(ch_data, bits_freed);

        Ok(())
    }

    fn read_coeffs(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        for band in 0..BANDS {
            if self.ba.band_bitsum[band] == 0 { continue; }
            if !self.ba.band_present[band] && (self.ba.band_width[band] == 0) { continue; }
            for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                let len = self.ba.cw_len[i];
                if len > 0 && (!self.ba.band_present[band] || !self.ba.skip_flag[i]) {
                    self.ba.coeff[i] = br.read(len)? as u8;
                    } else {
                    self.ba.coeff[i] = 0;
                }
            }
        }
        Ok(())
    }

    fn inv_quant(&mut self, ch: usize, raw_coeffs: bool) {
        let qidx: usize = if raw_coeffs { 1 } else { 0 };
        let ch_data = &mut self.ch_data[ch];
        for band in 0..BANDS {
            for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                ch_data.cw[i] = 0.0;
                let cw_len = self.ba.cw_len[i];
                if cw_len == 0 || self.ba.skip_flag[i] { continue; }

                let val = self.ba.coeff[i] as usize;
                let mid = 1 << (cw_len - 1);
                let max = (1 << cw_len) - 1;
                if cw_len >= 4 {
                    let quant = &IMC_QUANT_LARGE[qidx];
                    if val >= mid {
                        ch_data.cw[i] =  quant[val - 8]       * ch_data.adj_floor[band];
                    } else {
                        ch_data.cw[i] = -quant[max - val - 8] * ch_data.adj_floor[band];
                    }
                } else {
                    let idx = qidx + (if self.ba.band_present[band] { 2 } else { 0 });
                    let quant = &IMC_QUANT_SMALL[idx];
                    if val >= mid {
                        ch_data.cw[i] =  quant[val - 1]       * ch_data.adj_floor[band];
                    } else {
                        ch_data.cw[i] = -quant[max - val - 1] * ch_data.adj_floor[band];
                    }
                }
            }
        }
    }

    fn decode_block(&mut self, data: &[u8], ch: usize, dst: &mut [f32]) -> DecoderResult<()> {
        let mut br = BitReader::new(&data[BLOCK_SIZE*ch..][..BLOCK_SIZE], BitReaderMode::LE16MSB);
        let hdr = br.read(9)?;
        validate!((hdr & 0x18) == 0);

        let reset       = br.read_bool()?;
        let fixed_head  = br.read_bool()?;
        let raw_coeffs  = br.read_bool()?;
        let weight_idx  = br.read(1)? as usize;

        if reset {
            self.ch_data[ch].reset();
        }

        self.ba.reset();

        if raw_coeffs {
            self.read_level_coeffs_raw(&mut br, ch)?;
        } else {
            let cb_idx = (if reset { 2 } else { 0 }) + (if fixed_head { 1 } else { 0 });
            self.read_level_coeffs(&mut br, reset, cb_idx, ch)?;
        }

        self.ch_data[ch].old_floor.copy_from_slice(&self.ch_data[ch].new_floor);

        let mut bitcount: usize = 0;
        if fixed_head {
            bitcount += 15;
            self.ba.band_bits[0] = 5;
            for i in 0..3 {
                self.ba.cw_len[i] = 5;
            }
            for band in 1..4 {
                let bits: u8;
                if raw_coeffs || !self.ba.keep_flag[band]{
                    bits = 5;
                } else {
                    bits = 0;
                }
                self.ba.band_bits[band] = bits;
                for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                    self.ba.cw_len[i] = bits;
                    bitcount += bits as usize;
                }
            }
        }

        if !self.is_imc {
            if self.ba.band_width[BANDS - 1] != 0 {
                bitcount += 1;
            }
            bitcount += 16;
        } else {
            if self.ba.band_width[BANDS - 1] != 0 {
                bitcount += 1;
            }
        }

        validate!(br.tell() + bitcount < BLOCK_SIZE * 8);
        self.ba.calculate_bit_allocation(&mut self.ch_data[ch], 512 - bitcount - br.tell(), fixed_head, weight_idx)?;

        if !raw_coeffs {
            self.read_bitalloc_delta(&mut br, ch)?;
        }

        for band in 0..BANDS {
            self.ba.band_bitsum[band] = 0;
            for i in IMC_BANDS[band]..IMC_BANDS[band + 1] {
                if !self.ba.skip_flag[i] {
                    self.ba.band_bitsum[band] += self.ba.cw_len[i] as usize;
                }
            }
        }

        self.read_coeffs(&mut br)?;
        self.inv_quant(ch, raw_coeffs);
        self.imdct.imdct(&self.ch_data[ch].cw, dst, &mut self.ch_data[ch].last_im);

        Ok(())
    }
}

impl IMDCTContext {
    fn new() -> Self {
        let mut window: [f32; COEFFS] = [0.0; COEFFS];
        generate_window(WindowType::Sine, 1.0, COEFFS, true, &mut window);
        let mut pretwiddle1: [f32; COEFFS/2] = [0.0; COEFFS/2];
        let mut pretwiddle2: [f32; COEFFS/2] = [0.0; COEFFS/2];
        let mut posttwiddle: [FFTComplex; COEFFS/2] = [FFTC_ZERO; COEFFS/2];
        for i in 0..COEFFS/2 {
            let n = i as f32;
            let base = (n * 4.0 + 1.0) / 1024.0 * consts::PI;
            let r1 = base.sin();
            let r2 = base.cos();
            if (i & 1) == 0 {
                pretwiddle1[i] = -(r1 + r2) * consts::SQRT_2;
                pretwiddle2[i] =  (r1 - r2) * consts::SQRT_2;
            } else {
                pretwiddle1[i] =  (r1 + r2) * consts::SQRT_2;
                pretwiddle2[i] = -(r1 - r2) * consts::SQRT_2;
            }
            posttwiddle[i] = FFTComplex::exp(consts::PI / 256.0 * n).scale(1.0/32768.0);
        }
        IMDCTContext {
            pretwiddle1,
            pretwiddle2,
            posttwiddle,
            tmp:         [FFTC_ZERO; COEFFS/2],
            fft:         FFTBuilder::new_fft(COEFFS/2, false),
            window,
        }
    }
    fn imdct(&mut self, coeffs: &[f32; COEFFS], dst: &mut [f32], last_im: &mut [f32; COEFFS/2]) {
        for i in 0..COEFFS/2 {
            let in2 = coeffs[i * 2];
            let in1 = coeffs[COEFFS - 1 - i * 2];
            let c2  = self.pretwiddle1[i];
            let c1  = self.pretwiddle2[i];
            self.tmp[i].re = -(c2 * in1 + c1 * in2);
            self.tmp[i].im =   c1 * in1 - c2 * in2;
        }
        self.fft.do_ifft_inplace(&mut self.tmp);
        for i in 0..COEFFS/2 {
            let tmp = !(self.tmp[i] * self.posttwiddle[i]);
            let c1 = self.window[i * 2];
            let c2 = self.window[COEFFS - 1 - i * 2];
            let im = last_im[i];
            dst[i * 2]              = c2 * im + c1 * tmp.re;
            dst[COEFFS - 1 - i * 2] = c1 * im - c2 * tmp.re;
            last_im[i] = tmp.im;
        }
    }
}

const CHMAP_MONO: [NAChannelType; 1] = [NAChannelType::C];
const CHMAP_STEREO: [NAChannelType; 2] = [NAChannelType::L, NAChannelType::R];

impl NADecoder for IMCDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.chmap = NAChannelMap::new();
            match ainfo.get_channels() {
                1 => { self.chmap.add_channels(&CHMAP_MONO); },
                2 => { self.chmap.add_channels(&CHMAP_STEREO); },
                _ => { return Err(DecoderError::InvalidData); },
            };
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(),
                                          ainfo.get_channels(),
                                          SND_F32P_FORMAT, 0);
            self.info = info.replace_info(NACodecTypeInfo::Audio(self.ainfo));

            if !self.is_imc {
                self.generate_iac_tables(ainfo.get_sample_rate() as f32);
            }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        validate!(info.get_properties().is_audio());
        let pktbuf = pkt.get_buffer();

        let nblocks = pktbuf.len() / BLOCK_SIZE / (self.ainfo.get_channels() as usize);
        let duration = COEFFS * nblocks;

        let abuf = alloc_audio_buffer(self.ainfo, duration, self.chmap.clone())?;
        let mut adata = abuf.get_abuf_f32().unwrap();
        let dst = adata.get_data_mut().unwrap();

        let mut start: usize = 0;
        let channels = self.ainfo.get_channels() as usize;
        for chunk in pktbuf.chunks(BLOCK_SIZE * channels) {
            for ch in 0..channels {
                let off = abuf.get_offset(ch) + start;
                self.decode_block(chunk, ch, &mut dst[off..off+COEFFS])?;
            }
            if (channels == 2) && ((chunk[1] & 0x20) != 0) {
                let off1 = abuf.get_offset(0) + start;
                let off2 = abuf.get_offset(1) + start;
                for i in 0..COEFFS {
                    let l = dst[off1 + i];
                    let r = dst[off2 + i];
                    dst[off1 + i] = l + r;
                    dst[off2 + i] = l - r;
                }
            }
            start += COEFFS;
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for IMCDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_imc() -> Box<dyn NADecoder + Send> {
    Box::new(IMCDecoder::new(true))
}

pub fn get_decoder_iac() -> Box<dyn NADecoder + Send> {
    Box::new(IMCDecoder::new(false))
}

struct IMCCodeReader { sel1: usize, sel2: usize }

impl IMCCodeReader {
    fn new(sel1: usize, sel2: usize) -> Self { IMCCodeReader { sel1, sel2 } }
}

impl CodebookDescReader<u8> for IMCCodeReader {
    fn bits(&mut self, idx: usize) -> u8  { IMC_CODE_LENGTHS[self.sel1][self.sel2][idx] }
    fn code(&mut self, idx: usize) -> u32 { u32::from(IMC_CODE_CODES[self.sel1][self.sel2][idx]) }
    fn sym (&mut self, idx: usize) -> u8 { idx as u8 }
    fn len(&mut self) -> usize { IMC_CODE_LENGTHS[0][0].len() }
}

static IMC_BANDS: [usize; 33] = [
      0,   3,   6,   9,  12,  16,  20,  24,  29,  34,  40,  46,  53,  60,  68,  76,
     84,  93, 102, 111, 121, 131, 141, 151, 162, 173, 184, 195, 207, 219, 231, 243,
    256,
];

const IMC_QUANT_SMALL: &[[f32; 8]; 4] = &[
    [ 8.4431201e-1, 4.7358301e-1, 1.448354,  2.7073899e-1,
      7.4449003e-1, 1.241991,     1.845484,  0.0 ],
    [ 8.6876702e-1, 4.7659001e-1, 1.478224,  2.5672799e-1,
      7.55777e-1,   1.3229851,    2.03438,   0.0 ],
    [ 7.5891501e-1, 6.2272799e-1, 1.271322,  3.47904e-1,
      7.5317699e-1, 1.150767,     1.628476,  0.0 ],
    [ 7.65257e-1,   6.44647e-1,   1.263824,  3.4548101e-1,
      7.6384902e-1, 1.214466,     1.7638789, 0.0 ]
];

const IMC_QUANT_LARGE: &[[f32; 56]; 2] = &[
    [ 1.39236e-1,   3.50548e-1,   5.9547901e-1, 8.5772401e-1,
      1.121545,     1.3882281,    1.695882,     2.1270809,
      7.2221003e-2, 1.85177e-1,   2.9521701e-1, 4.12568e-1,
      5.4068601e-1, 6.7679501e-1, 8.1196898e-1, 9.4765198e-1,
      1.0779999,    1.203415,     1.337265,     1.481871,
      1.639982,     1.814766,     2.0701399,    2.449862,
      3.7533998e-2, 1.02722e-1,   1.6021401e-1, 2.16043e-1,
      2.7231601e-1, 3.3025399e-1, 3.9022601e-1, 4.52849e-1,
      5.1794899e-1, 5.8529502e-1, 6.53956e-1,   7.2312802e-1,
      7.9150802e-1, 8.5891002e-1, 9.28141e-1,   9.9706203e-1,
      1.062153,     1.12564,      1.189834,     1.256122,
      1.324469,     1.3955311,    1.468906,     1.545084,
      1.6264729,    1.711524,     1.802705,     1.91023,
      2.0533991,    2.22333,      2.4830019,    3.253329 ],
    [ 1.11654e-1,   3.54469e-1,   6.4232099e-1, 9.6128798e-1,
      1.295053,     1.61777,      1.989839,     2.51107,
      5.7721999e-2, 1.69879e-1,   2.97589e-1,   4.3858799e-1,
      5.9039903e-1, 7.4934798e-1, 9.1628098e-1, 1.087297,
      1.262751,     1.4288321,    1.6040879,    1.79067,
      2.000668,     2.2394669,    2.649332,     5.2760072,
      2.9722e-2,    8.7316997e-2, 1.4445201e-1, 2.04247e-1,
      2.6879501e-1, 3.3716801e-1, 4.08811e-1,   4.8306999e-1,
      5.6049401e-1, 6.3955498e-1, 7.2044599e-1, 8.0427998e-1,
      8.8933599e-1, 9.7537601e-1, 1.062461,     1.1510431,
      1.240236,     1.326715,     1.412513,     1.500502,
      1.591749,     1.686413,     1.785239,     1.891233,
      2.0051291,    2.127681,     2.2709141,    2.475826,
      2.7219379,    3.101985,     4.686213,     6.2287788 ]
];

static IMC_CYCLE1: [usize; BANDS] = [
     1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,
    17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 32,
];

static IMC_CYCLE2: [usize; BANDS] = [
     0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29
];

static IMC_WEIGHTS1: [f32; BANDS-1] = [
    0.119595,   0.123124,   0.129192,   9.97377e-2,
    8.1923e-2,  9.61153e-2, 8.77885e-2, 8.61174e-2,
    9.00882e-2, 9.91658e-2, 0.112991,   0.131126,
    0.152886,   0.177292,   0.221782,   0.244917,
    0.267386,   0.306816,   0.323046,   0.33729,
    0.366773,   0.392557,   0.398076,   0.403302,
    0.42451,    0.444777,   0.449188,   0.455445,
    0.477853,   0.500669,   0.510395
];

static IMC_WEIGHTS2: [f32; BANDS-1] = [
    3.23466e-3, 3.49886e-3, 3.98413e-3, 1.98116e-3,
    1.16465e-3, 1.79283e-3, 1.40372e-3, 1.33274e-3,
    1.50523e-3, 1.95064e-3, 2.77472e-3, 4.14725e-3,
    6.2776e-3,  9.36401e-3, 1.71397e-2, 2.24052e-2,
    2.83971e-2, 4.11689e-2, 4.73165e-2, 5.31631e-2,
    6.66614e-2, 8.00824e-2, 8.31588e-2, 8.61397e-2,
    9.89229e-2, 0.112197,   0.115227,   0.119613,
    0.136174,   0.15445,    0.162685
];

static IMC_BITALLOC_ADJ: [[f32; 7]; 2] = [
    [ 7.6, 4.4, 6.1, 2.3, 6.2, 1.8, 0.0 ],
    [ 3.6, 3.7, 5.1, 1.6, 1.5, 1.2, 0.0 ]
];

static IMC_CODE_LENGTHS: &[[[u8; 18]; 4]; 4] = &[
    [
        [ 16, 15, 13, 11,  8,  5,  3,  1,  2,  4,  6,  9, 10, 12, 14, 16,  7,  0 ],
        [ 10,  8,  7,  6,  4,  4,  3,  2,  2,  3,  4,  6,  7,  9, 11, 11,  7,  0 ],
        [ 15, 15, 14, 11,  8,  6,  4,  2,  1,  4,  5,  7,  9, 10, 12, 13,  4,  0 ],
        [ 13, 11, 10,  8,  6,  4,  2,  2,  2,  3,  5,  7,  9, 12, 15, 15, 14,  0 ],
    ], [
        [ 14, 12, 10,  8,  7,  4,  2,  2,  2,  3,  5,  7,  9, 11, 13, 14,  7,  0 ],
        [ 14, 13, 11,  8,  6,  4,  3,  2,  2,  3,  5,  7,  9, 10, 12, 14,  3,  0 ],
        [ 13, 12, 10,  7,  5,  4,  3,  2,  2,  3,  4,  6,  8,  9, 11, 13,  4,  0 ],
        [ 13, 12, 10,  7,  5,  4,  3,  2,  2,  3,  4,  6,  8,  9, 11, 13,  4,  0 ],
    ], [
        [ 16, 14, 12, 10,  8,  5,  3,  1,  2,  4,  7,  9, 11, 13, 15, 17,  6, 17 ],
        [ 15, 13, 11,  8,  6,  4,  2,  2,  2,  3,  5,  7, 10, 12, 14, 16,  9, 16 ],
        [ 14, 12, 11,  9,  8,  6,  3,  1,  2,  5,  7, 10, 13, 15, 16, 17,  4, 17 ],
        [ 16, 14, 12,  9,  7,  5,  2,  2,  2,  3,  4,  6,  8, 11, 13, 15, 10, 16 ],
    ], [
        [ 13, 11, 10,  8,  7,  5,  2,  2,  2,  4,  6,  9, 12, 14, 15, 16,  3, 16 ],
        [ 11, 11, 10,  9,  8,  7,  5,  4,  3,  3,  3,  3,  3,  3,  4,  5,  6,  5 ],
        [  9,  9,  7,  6,  5,  4,  3,  3,  2,  3,  4,  5,  4,  5,  5,  6,  8,  6 ],
        [ 13, 12, 10,  8,  5,  3,  3,  2,  2,  3,  4,  7,  9, 11, 14, 15,  6, 15 ]
    ]
];

static IMC_CODE_CODES: &[[[u16; 18]; 4]; 4] = &[
    [
        [ 0xCC32, 0x6618, 0x1987, 0x0660, 0x00CD, 0x0018, 0x0007, 0x0000, 0x0002,
          0x000D, 0x0032, 0x0199, 0x0331, 0x0CC2, 0x330D, 0xCC33, 0x0067, 0x0000 ],
        [ 0x02FE, 0x00BE, 0x005E, 0x002D, 0x000A, 0x0009, 0x0003, 0x0003, 0x0000,
          0x0002, 0x0008, 0x002C, 0x005D, 0x017E, 0x05FE, 0x05FF, 0x005C, 0x0000 ],
        [ 0x5169, 0x5168, 0x28B5, 0x0517, 0x00A3, 0x0029, 0x0008, 0x0003, 0x0000,
          0x0009, 0x0015, 0x0050, 0x0144, 0x028A, 0x0A2C, 0x145B, 0x000B, 0x0000 ],
        [ 0x1231, 0x048D, 0x0247, 0x0090, 0x0025, 0x0008, 0x0001, 0x0003, 0x0000,
          0x0005, 0x0013, 0x0049, 0x0122, 0x0919, 0x48C3, 0x48C2, 0x2460, 0x0000 ]
    ], [
        [ 0x2D1D, 0x0B46, 0x02D0, 0x00B5, 0x0059, 0x000A, 0x0003, 0x0001, 0x0000,
          0x0004, 0x0017, 0x005B, 0x0169, 0x05A2, 0x168F, 0x2D1C, 0x0058, 0x0000 ],
        [ 0x1800, 0x0C01, 0x0301, 0x0061, 0x0019, 0x0007, 0x0004, 0x0003, 0x0000,
          0x0005, 0x000D, 0x0031, 0x00C1, 0x0181, 0x0601, 0x1801, 0x0002, 0x0000 ],
        [ 0x1556, 0x0AAA, 0x02AB, 0x0054, 0x0014, 0x000B, 0x0002, 0x0003, 0x0000,
          0x0003, 0x0008, 0x002B, 0x00AB, 0x0154, 0x0554, 0x1557, 0x0009, 0x0000 ],
        [ 0x1556, 0x0AAA, 0x02AB, 0x0054, 0x0014, 0x000B, 0x0002, 0x0003, 0x0000,
          0x0003, 0x0008, 0x002B, 0x00AB, 0x0154, 0x0554, 0x1557, 0x0009, 0x0000 ]
    ], [
        [ 0x2993, 0x0A65, 0x0298, 0x00A7, 0x0028, 0x0004, 0x0000, 0x0001, 0x0001,
          0x0003, 0x0015, 0x0052, 0x014D, 0x0533, 0x14C8, 0x5324, 0x000B, 0x5325 ],
        [ 0x09B8, 0x026F, 0x009A, 0x0012, 0x0005, 0x0000, 0x0001, 0x0002, 0x0003,
          0x0001, 0x0003, 0x0008, 0x004C, 0x0136, 0x04DD, 0x1373, 0x0027, 0x1372 ],
        [ 0x0787, 0x01E0, 0x00F1, 0x003D, 0x001F, 0x0006, 0x0001, 0x0001, 0x0001,
          0x0002, 0x000E, 0x0079, 0x03C2, 0x0F0D, 0x1E19, 0x3C30, 0x0000, 0x3C31 ],
        [ 0x4B06, 0x12C0, 0x04B1, 0x0097, 0x0024, 0x0008, 0x0002, 0x0003, 0x0000,
          0x0003, 0x0005, 0x0013, 0x004A, 0x0259, 0x0961, 0x2582, 0x012D, 0x4B07 ]
    ], [
        [ 0x0A5A, 0x0297, 0x014A, 0x0053, 0x0028, 0x000B, 0x0003, 0x0000, 0x0002,
          0x0004, 0x0015, 0x00A4, 0x052C, 0x14B7, 0x296C, 0x52DB, 0x0003, 0x52DA ],
        [ 0x0193, 0x0192, 0x00C8, 0x0065, 0x0033, 0x0018, 0x0007, 0x0004, 0x0000,
          0x0004, 0x0005, 0x0007, 0x0006, 0x0003, 0x0005, 0x0005, 0x000D, 0x0004 ],
        [ 0x0012, 0x0013, 0x0005, 0x0003, 0x0000, 0x0003, 0x0005, 0x0004, 0x0003,
          0x0003, 0x0005, 0x0005, 0x0004, 0x0004, 0x0003, 0x0005, 0x0008, 0x0004 ],
        [ 0x0D66, 0x06B2, 0x01AD, 0x006A, 0x000C, 0x0005, 0x0004, 0x0000, 0x0003,
          0x0002, 0x0007, 0x0034, 0x00D7, 0x0358, 0x1ACF, 0x359C, 0x001B, 0x359D ]
    ]
];

const IMC_CB_SELECTOR: [[usize; BANDS]; 4] = [
    [ 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2 ],
    [ 0, 2, 0, 3, 2, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
    [ 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2 ],
    [ 0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
];

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::indeo_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_imc() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        indeo_register_all_decoders(&mut dec_reg);

//        let file = "assets/Indeo/neal73_saber.avi";
//        let file = "assets/Indeo/IMC/hvalen.avi";
        // sample from a private collection
        let file = "assets/Indeo/IMC/8khz.avi";
//        let file = "assets/Indeo/STsKlassFist-1a.avi";
//        let file = "assets/Indeo/IMC/Angel Bday.avi";
        test_decode_audio("avi", file, None, None/*Some("imc")*/, &dmx_reg, &dec_reg);
        //test_file_decoding("avi", file, None, false, true, None);
    }
}
