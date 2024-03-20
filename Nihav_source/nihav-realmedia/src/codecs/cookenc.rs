use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitwriter::*;
use nihav_codec_support::dsp::fft::*;
use super::cookdata::*;

/*
 TODO:
  - calculate gains?
*/
const MAX_FRAME_SIZE: usize = 1024;

const EPS: f32 = 1.0e-6;

fn get_block_size(srate: u32) -> usize {
    match srate {
        8000 | 11025 => 256,
        22050 => 512,
        _ => 1024,
    }
}

trait ClipCat {
    fn clip_cat(&self) -> usize;
}

impl ClipCat for i32 {
    fn clip_cat(&self) -> usize { ((*self).max(0) as usize).min(NUM_CATEGORIES - 1) }
}

fn bitalloc(samples: usize, bits: usize, vector_bits: u8, total_subbands: usize, qindex: &[i8], category: &mut [u8], cat_index: &mut [u8]) {
    let avail_bits = (if bits > samples { samples + ((bits - samples) * 5) / 8 } else { bits }) as i32;

    let mut bias: i32 = -32;
    for i in 0..6 {
        let mut sum = 0;
        for j in 0..total_subbands {
            let idx = ((32 >> i) + bias - (qindex[j] as i32)) / 2;
            sum += COOK_EXP_BITS[idx.clip_cat()];
        }
        if sum >= (avail_bits - 32) {
            bias += 32 >> i;
        }
    }

    let mut exp_index1: [usize; MAX_SUBBANDS * 2] = [0; MAX_SUBBANDS * 2];
    let mut exp_index2: [usize; MAX_SUBBANDS * 2] = [0; MAX_SUBBANDS * 2];
    let mut sum = 0;
    for i in 0..total_subbands {
        let idx = ((bias - (qindex[i] as i32)) / 2).clip_cat();
        sum += COOK_EXP_BITS[idx];
        exp_index1[i] = idx;
        exp_index2[i] = idx;
    }

    let mut tbias1 = sum;
    let mut tbias2 = sum;
    let mut tcat: [usize; 128*2] = [0; 128*2];
    let mut tcat_idx1 = 128;
    let mut tcat_idx2 = 128;
    for _ in 1..(1 << vector_bits) {
        if tbias1 + tbias2 > avail_bits * 2 {
            let mut max = -999999;
            let mut idx = total_subbands + 1;
            for j in 0..total_subbands {
                if exp_index1[j] >= (NUM_CATEGORIES - 1) { continue; }
                let t = -2 * (exp_index1[j] as i32) - (qindex[j] as i32) + bias;
                if t >= max {
                    max = t;
                    idx = j;
                }
            }
            if idx >= total_subbands { break; }
            tcat[tcat_idx1] = idx;
            tcat_idx1 += 1;
            tbias1 -= COOK_EXP_BITS[exp_index1[idx]] - COOK_EXP_BITS[exp_index1[idx] + 1];
            exp_index1[idx] += 1;
        } else {
            let mut min = 999999;
            let mut idx = total_subbands + 1;
            for j in 0..total_subbands {
                if exp_index2[j] == 0 { continue; }
                let t = -2 * (exp_index2[j] as i32) - (qindex[j] as i32) + bias;
                if t < min {
                    min = t;
                    idx = j;
                }
            }
            if idx >= total_subbands { break; }
            tcat_idx2 -= 1;
            tcat[tcat_idx2] = idx;
            tbias2 -= COOK_EXP_BITS[exp_index2[idx]] - COOK_EXP_BITS[exp_index2[idx] - 1];
            exp_index2[idx] -= 1;
        }
    }
    for i in 0..total_subbands {
        category[i] = exp_index2[i] as u8;
    }
    for el in cat_index.iter_mut() {
        *el = 255;
    }
    for (dst, &src) in cat_index.iter_mut().zip(tcat[tcat_idx2..tcat_idx1].iter()) {
        *dst = src as u8;
    }
}

fn map_coef(val: f32, centroids: &[f32]) -> usize {
    if val < centroids[1] * 0.5 {
        0
    } else {
        let len = centroids.len();
        if val < centroids[len - 1] {
            for (i, pair) in centroids.windows(2).enumerate().skip(1) {
                if val <= (pair[0] + pair[1]) * 0.5 {
                    return i;
                }
            }
        }
        len - 1
    }
}

fn couple_bands(left: &[f32], right: &[f32], dst: &mut [f32], cpl_scales: &[f32]) -> u8 {
    let nrg0 = left.iter().fold(0.0f32, |acc, &v| acc + v * v);
    let nrg1 = right.iter().fold(0.0f32, |acc, &v| acc + v * v);
    let last_idx = cpl_scales.len() - 3;
    match (nrg0 > EPS, nrg1 > EPS) {
        (true, true) => {
            let tgt_scale0 = (nrg0 / (nrg0 + nrg1)).sqrt();
            let tgt_scale1 = (nrg1 / (nrg0 + nrg1)).sqrt();

            let mut best_dist = 42.0;
            let mut best_idx = 0;
            for i in 0..=last_idx {
                let scale0 = cpl_scales[i];
                let scale1 = cpl_scales[cpl_scales.len() - 1 - i];
                let dist = (scale0 - tgt_scale0).abs() + (scale1 - tgt_scale1).abs();
                if dist < best_dist {
                    best_dist = dist;
                    best_idx  = i;
                }
            }
            let scale_l = cpl_scales[best_idx];
            let scale_r = cpl_scales[cpl_scales.len() - 1 - best_idx];

            if best_idx == 0 {
                dst.copy_from_slice(left);
            } else {
                for (dst, (&ll, &rr)) in dst.iter_mut().zip(left.iter().zip(right.iter())) {
                    *dst = (ll / scale_l + rr / scale_r) * 0.5;
                }
            }
            best_idx as u8
        },
        (false, true) => {
            dst.copy_from_slice(right);
            last_idx as u8
        },
        (true, false) => {
            dst.copy_from_slice(left);
            0
        },
        _ => {
            for (dst, (&ll, &rr)) in dst.iter_mut().zip(left.iter().zip(right.iter())) {
                *dst = (ll + rr) * std::f32::consts::FRAC_1_SQRT_2;
            }
            (cpl_scales.len() / 2) as u8
        }
    }
}

#[derive(Clone,Copy,Default)]
struct PackedCoeffs {
    cw:     [u16; 10],
    bits:   [u8;  10],
    signs:  [u16; 10],
    nnz:    [u8;  10],
    num:    usize,
    cat:    usize,
    nbits:  u16,
}

impl PackedCoeffs {
    fn pack(&mut self, coeffs: &[f32], cat: usize) -> u16 {
        self.cat = cat;
        self.nbits = 0;

        if cat >= COOK_NUM_VQ_GROUPS.len() {
            self.num = 0;
            return 0;
        }

        let group_size = COOK_VQ_GROUP_SIZE[cat];
        let multiplier = COOK_VQ_MULT[cat] as usize + 1;
        let (code_words, code_bits) = match cat {
            0 => (&COOK_VQ0_CODES[..], &COOK_VQ0_BITS[..]),
            1 => (&COOK_VQ1_CODES[..], &COOK_VQ1_BITS[..]),
            2 => (&COOK_VQ2_CODES[..], &COOK_VQ2_BITS[..]),
            3 => (&COOK_VQ3_CODES[..], &COOK_VQ3_BITS[..]),
            4 => (&COOK_VQ4_CODES[..], &COOK_VQ4_BITS[..]),
            5 => (&COOK_VQ5_CODES[..], &COOK_VQ5_BITS[..]),
            6 => (&COOK_VQ6_CODES[..], &COOK_VQ6_BITS[..]),
            _ => unreachable!(),
            };
        let centroids = &COOK_QUANT_CENTROID[cat][..multiplier];
        self.num = COOK_NUM_VQ_GROUPS[cat];

        for (group_no, group) in coeffs.chunks(group_size).enumerate() {
            let mut cw = 0;
            let mut cvals = [0; 5];
            let mut sarr  = [0; 5];
            for ((dval, sign), &el) in cvals.iter_mut().zip(sarr.iter_mut()).zip(group.iter()) {
                let cur_val = map_coef(el.abs(), centroids);
                *dval = cur_val;
                *sign = (el < 0.0) as u16;
                cw = cw * multiplier + cur_val;
            }
            while cw >= code_bits.len() || code_bits[cw] == 0 {
                let mut max_pos = 0;
                let mut max_val = cvals[0];
                for (i, &val) in cvals.iter().enumerate().skip(1) {
                    if val > max_val {
                        max_val = val;
                        max_pos = i;
                    }
                }
                cvals[max_pos] -= 1;
                cw = 0;
                for &dval in cvals.iter().take(group_size) {
                    cw = cw * multiplier + dval;
                }
            }
            let mut signs = 0;
            let mut nnz = 0u8;
            for (&sign, &val) in sarr.iter().zip(cvals.iter()) {
                if val != 0 {
                    signs = (signs << 1) | sign;
                    nnz += 1;
                }
            }

            self.cw   [group_no] = code_words[cw];
            self.bits [group_no] = code_bits[cw];
            self.signs[group_no] = signs;
            self.nnz  [group_no] = nnz;
            self.nbits += u16::from(code_bits[cw]) + u16::from(nnz);
        }
        self.nbits
    }
    fn write(&self, bw: &mut BitWriter, mut bits_left: u16) {
        for ((&cw, &bits), (&signs, &nnz)) in
                self.cw.iter().zip(self.bits.iter()).zip(
                    self.signs.iter().zip(self.nnz.iter())).take(self.num) {
            let cur_bits = u16::from(bits + nnz);
            if cur_bits > bits_left {
                break;
            }
            bits_left -= cur_bits;
            bw.write(cw.into(), bits);
            if nnz > 0 {
                bw.write(signs.into(), nnz);
            }
        }
    }
}

struct TempData {
    bands:  [PackedCoeffs; MAX_SUBBANDS * 2],
}

impl Default for TempData {
    fn default() -> Self {
        Self {
            bands:  [PackedCoeffs::default(); MAX_SUBBANDS * 2],
        }
    }
}

struct ChannelDataParams<'a> {
    size:           usize,
    frame_size:     usize,
    hpow_tab:       &'a [f32; 128],
    coupling:       &'a [u8],
    js_bits:        u8,
    js_start:       usize,
    vector_bits:    u8,
}

struct CookChannelPair {
    br_info:        &'static BitrateParams,
    delay:          [[f32; MAX_FRAME_SIZE]; 2],
}

fn calc_qindex(nrg: f32) -> i8 {
    let mut nrg0 = nrg * 0.05;
    if nrg0 <= 1.0 {
        nrg0 *= std::f32::consts::FRAC_1_SQRT_2;
    } else {
        nrg0 *= std::f32::consts::SQRT_2;
    }
    nrg0.log2().max(-31.0).min(47.0) as i8
}

impl CookChannelPair {
    fn new(br_info: &'static BitrateParams) -> Self {
        Self {
            br_info,
            delay:      [[0.0; MAX_FRAME_SIZE]; 2],
        }
    }
    fn encode_bands(params: ChannelDataParams, dbuf: Vec<u8>, coeffs: &mut [f32], total_bands: usize, tmp: &mut TempData) -> EncoderResult<Vec<u8>> {
        let output_start = dbuf.len();
        let mut bw = BitWriter::new(dbuf, BitWriterMode::BE);
        let data_end = bw.tell() + params.frame_size;

        let mut qindex = [0i8; MAX_SUBBANDS * 2];
        for (qscale, band) in qindex.iter_mut().zip(coeffs.chunks(BAND_SIZE)).take(total_bands) {
            let nrg = band.iter().fold(0.0f32, |acc, &v| acc + v * v);
            *qscale = calc_qindex(nrg);
        }
        qindex[0] = qindex[0].max(-6);

        bw.write0(); // no gains
        //todo gains

        if params.js_bits > 0 {
            let (cpl_cb_codes, cpl_cb_bits) = match params.js_bits {
                    2 => (&COOK_CPL_2BITS_CODES[..], &COOK_CPL_2BITS_BITS[..]),
                    3 => (&COOK_CPL_3BITS_CODES[..], &COOK_CPL_3BITS_BITS[..]),
                    4 => (&COOK_CPL_4BITS_CODES[..], &COOK_CPL_4BITS_BITS[..]),
                    5 => (&COOK_CPL_5BITS_CODES[..], &COOK_CPL_5BITS_BITS[..]),
                    6 => (&COOK_CPL_6BITS_CODES[..], &COOK_CPL_6BITS_BITS[..]),
                    _ => unreachable!(),
                };
            let mut bit_size = 0;
            let mut raw_bit_size = 0;
            for &el in params.coupling.iter() {
                bit_size += cpl_cb_bits[usize::from(el)];
                raw_bit_size += params.js_bits;
            }
            if bit_size < raw_bit_size {
                bw.write1();
                for &el in params.coupling.iter() {
                    let idx = usize::from(el);
                    bw.write(cpl_cb_codes[idx].into(), cpl_cb_bits[idx]);
                }
            } else {
                bw.write0();
                for &el in params.coupling.iter() {
                    bw.write(u32::from(el), params.js_bits);
                }
            }
        }

        let mut last_q = qindex[0];
        bw.write((qindex[0] + 6) as u32, 6);
        if params.js_bits == 0 {
            for (i, qscale) in qindex[..total_bands].iter_mut().enumerate().skip(1) {
                let cb_idx = (i - 1).min(12);
                let diff = (*qscale - last_q).max(-12).min(11);
                *qscale = last_q + diff;
                last_q = *qscale;

                let idx2 = (diff + 12) as usize;
                bw.write(COOK_QUANT_CODES[cb_idx][idx2].into(), COOK_QUANT_BITS[cb_idx][idx2]);
            }
        } else {
            for (i, qscale) in qindex[..total_bands].iter_mut().enumerate().skip(1) {
                let band_no = if i < params.js_start * 2 { i >> 1 } else { i - params.js_start };
                let cb_idx = band_no.saturating_sub(1).min(12);
                let diff = (*qscale - last_q).max(-12).min(11);
                *qscale = last_q + diff;
                last_q = *qscale;

                let idx2 = (diff + 12) as usize;
                bw.write(COOK_QUANT_CODES[cb_idx][idx2].into(), COOK_QUANT_BITS[cb_idx][idx2]);
            }
        }

        let mut category = [0; MAX_SUBBANDS * 2];
        let mut cat_index = [0; 127];
        let bits_avail = data_end - bw.tell() - usize::from(params.vector_bits);

        bitalloc(params.size, bits_avail, params.vector_bits, total_bands, &qindex, &mut category, &mut cat_index);

        let mut tot_bits = 0;
        for ((band, packed), (&qindex, &cat)) in
                coeffs.chunks_exact_mut(BAND_SIZE).zip(tmp.bands.iter_mut())
                    .zip(qindex.iter().zip(category.iter())).take(total_bands) {
            for coef in band.iter_mut() {
                *coef *= params.hpow_tab[(64 - qindex) as usize];
            }
            tot_bits += packed.pack(band, cat.into());
        }

        let mut bits_left = bits_avail as u16;
        let mut bands_corrected = 0;
        let max_corr_bands = (1 << params.vector_bits) - 1;
        for &index in cat_index.iter() {
            if bands_corrected >= max_corr_bands || tot_bits <= bits_left || index == 255 {
                break;
            }
            let index = usize::from(index);
            let pband = &mut tmp.bands[index];
            let band_coeffs = &coeffs[index * BAND_SIZE..][..BAND_SIZE];
            let new_cat = (pband.cat + 1).min(NUM_CATEGORIES - 1);
            tot_bits -= pband.nbits;
            pband.pack(band_coeffs, new_cat);
            tot_bits += pband.nbits;
            bands_corrected += 1;
        }

        bw.write(bands_corrected, params.vector_bits);
        for packed in tmp.bands.iter().take(total_bands) {
            packed.write(&mut bw, bits_left);
            bits_left = bits_left.saturating_sub(packed.nbits);
            if bits_left == 0 {
                break;
            }
        }

        pad(&mut bw, data_end);

        let mut dbuf = bw.end();
        for (i, el) in dbuf[output_start..].iter_mut().enumerate() {
            *el ^= COOK_XOR_KEY[i & 3];
        }

        Ok(dbuf)
    }
    fn encode_mono(&mut self, dbuf: Vec<u8>, dsp: &mut CookDSP, src: &[f32], ch_no: usize, tmp: &mut TempData) -> EncoderResult<Vec<u8>> {
        dsp.mdct(&mut self.delay[ch_no], src, true);
        let coeffs = &mut dsp.coeffs;

        let frame_size = (self.br_info.frame_bits / u32::from(self.br_info.channels)) as usize;
        let total_bands: usize = self.br_info.max_subbands.into();

        let params = ChannelDataParams {
                size:           dsp.size,
                hpow_tab:       &dsp.hpow_tab,
                coupling:       &[],
                js_bits:        0,
                js_start:       0,
                vector_bits:    5,
                frame_size,
            };
        Self::encode_bands(params, dbuf, coeffs, total_bands, tmp)
    }
    fn encode_jstereo(&mut self, dbuf: Vec<u8>, dsp: &mut CookDSP, l_ch: &[f32], r_ch: &[f32], tmp: &mut TempData) -> EncoderResult<Vec<u8>> {
        let frame_size = self.br_info.frame_bits as usize;
        let low_bands = usize::from(self.br_info.js_start);
        let total_bands = usize::from(self.br_info.max_subbands) + low_bands;

        dsp.mdct(&mut self.delay[0], l_ch, true);
        dsp.mdct(&mut self.delay[1], r_ch, false);

        let mut decouple = [0; 20];
        let cpl_scales = COOK_CPL_SCALES[usize::from(self.br_info.js_bits - 2)];
        let cpl_start_band = COOK_CPL_BAND[usize::from(self.br_info.js_start)] as usize;
        let cpl_end_band = COOK_CPL_BAND[usize::from(self.br_info.max_subbands) - 1] as usize;

        let coeffs = &mut dsp.coeffs;
        let coeffs2 = &mut dsp.coeffs2;
        let coupled = &mut dsp.coupled;
        for (dst, (src0, src1)) in coupled.chunks_exact_mut(BAND_SIZE * 2).zip(coeffs.chunks(BAND_SIZE).zip(coeffs2.chunks(BAND_SIZE))).take(low_bands) {
            let (dst0, dst1) = dst.split_at_mut(BAND_SIZE);
            dst0.copy_from_slice(src0);
            dst1.copy_from_slice(src1);
        }
        for el in coupled[total_bands * BAND_SIZE..].iter_mut() {
            *el = 0.0;
        }

        let mut band = low_bands;
        let mut start_band = band;
        let mut cpl_band = cpl_start_band;
        let mut last_cpl_band = cpl_band;
        let end_band = usize::from(self.br_info.max_subbands);
        while band < end_band {
            cpl_band = usize::from(COOK_CPL_BAND[band]);
            if cpl_band != last_cpl_band {
                let length = (band - start_band) * BAND_SIZE;
                decouple[last_cpl_band] = couple_bands(&coeffs[start_band * BAND_SIZE..][..length],
                                                  &coeffs2[start_band * BAND_SIZE..][..length],
                                                  &mut coupled[(start_band + low_bands) * BAND_SIZE..][..length],
                                                  cpl_scales);
                last_cpl_band = cpl_band;
                start_band = band;
            }
            band += 1;
        }
        if band != start_band {
            let length = (band - start_band) * BAND_SIZE;
            decouple[last_cpl_band] = couple_bands(&coeffs[start_band * BAND_SIZE..][..length],
                                                   &coeffs2[start_band * BAND_SIZE..][..length],
                                                   &mut coupled[(start_band + low_bands) * BAND_SIZE..][..length],
                                                   cpl_scales);
        }

        let vector_bits = match dsp.size {
                1024 => 7,
                 512 => 6,
                   _ => 5,
            };
        let params = ChannelDataParams {
                size:           dsp.size,
                hpow_tab:       &dsp.hpow_tab,
                coupling:       &decouple[cpl_start_band..=cpl_end_band],
                js_bits:        self.br_info.js_bits,
                js_start:       self.br_info.js_start.into(),
                frame_size,
                vector_bits,
            };

        Self::encode_bands(params, dbuf, coupled, total_bands, tmp)
    }
}

fn pad(bw: &mut BitWriter, data_end: usize) {
    while (bw.tell() & 7) != 0 {
        bw.write0();
    }
    while bw.tell() + 32 <= data_end {
        bw.write(0, 32);
    }
    while bw.tell() + 8 <= data_end {
        bw.write(0, 8);
    }
}
pub struct MDCT {
    table:      Vec<FFTComplex>,
    tmp:        Vec<FFTComplex>,
    fft:        FFT,
    size:       usize,
}

impl MDCT {
    pub fn new(size: usize) -> Self {
        let fft = FFTBuilder::new_fft(size / 4, false);
        let mut table = Vec::with_capacity(size / 4);
        let factor = std::f32::consts::PI * 2.0 / (size as f32);
        for i in 0..(size / 4) {
            let val = factor * ((i as f32) + 1.0 / 8.0);
            table.push(FFTComplex::exp(val));
        }
        Self {
            fft,
            tmp:    vec![FFTC_ZERO; size / 4],
            table,
            size,
        }
    }
    pub fn mdct(&mut self, src: &[f32], dst: &mut [f32]) {
        let size1_8 = self.size / 8;
        let size1_4 = self.size / 4;
        let size1_2 = self.size / 2;
        let size3_4 = size1_4 * 3;

        for i in 0..size1_8 {
            let a0 = FFTComplex{re: -src[2 * i + size3_4] - src[size3_4 - 1 - 2 * i],
                                im: -src[size1_4 + 2 * i] + src[size1_4 - 1 - 2 * i]};
            let t0 = !self.table[i];
            let a1 = FFTComplex{re: src[2 * i] - src[size1_2 - 1 - 2 * i],
                                im: -src[size1_2 + 2 * i] - src[self.size - 1 - 2 * i]};
            let t1 = !self.table[size1_8 + i];
            self.tmp[i] = a0 * t0;
            self.tmp[i + size1_8] = a1 * t1;
        }
        self.fft.do_fft_inplace(&mut self.tmp);

        for i in 0..size1_8 {
            let a0 = self.tmp[size1_8 - 1 - i] * !self.table[size1_8 - 1 - i].rotate();
            let a1 = self.tmp[size1_8     + i] * !self.table[size1_8     + i].rotate();
            dst[size1_4 - 2 - i * 2] = a0.im;
            dst[size1_4 - 1 - i * 2] = a1.re;
            dst[size1_4     + i * 2] = a1.im;
            dst[size1_4 + 1 + i * 2] = a0.re;
        }
    }
}


struct CookDSP {
    size:       usize,
    mdct:       MDCT,
    tmp:        [f32; MAX_FRAME_SIZE * 2],
    window:     [f32; MAX_FRAME_SIZE * 2],
    pow_tab:    [f32; 128],
    hpow_tab:   [f32; 128],
    gain_tab:   [f32; 23],

    coeffs:     [f32; MAX_FRAME_SIZE],
    coeffs2:    [f32; MAX_FRAME_SIZE],
    coupled:    [f32; MAX_FRAME_SIZE * 2],
}

impl CookDSP {
    fn init(&mut self, frame_size: usize) {
        if self.size == frame_size {
            return;
        }
        self.size = frame_size;
        self.mdct = MDCT::new(self.size * 2);

        let fsamples = self.size as f32;
        let factor = std::f32::consts::PI / (2.0 * fsamples);
        let scale = fsamples;
        for (k, dst) in self.window[..self.size * 2].iter_mut().enumerate() {
            *dst = (factor * ((k as f32) + 0.5)).sin() * scale;
        }

        for (dst, &pow_val) in self.gain_tab.iter_mut().zip(self.pow_tab[53..].iter()) {
            *dst = pow_val.powf(8.0 / fsamples);
        }
    }
    fn mdct(&mut self, delay: &mut [f32], src: &[f32], mono: bool) {
        let (w0, w1) = self.window[..self.size * 2].split_at(self.size);
        let (d0, d1) = self.tmp[..self.size * 2].split_at_mut(self.size);

        for (dst, (&src, &win)) in d1.iter_mut().zip(delay.iter().zip(w0.iter()).rev()) {
            *dst = src * win;
        }
        for (dst, (&src, &win)) in d0.iter_mut().zip(src.iter().zip(w1.iter()).rev()) {
            *dst = src * win;
        }
        self.mdct.mdct(&self.tmp, if mono { &mut self.coeffs } else { &mut self.coeffs2 });

        delay[..self.size].copy_from_slice(&src[..self.size]);
    }
}

impl Default for CookDSP {
    fn default() -> Self {
        let mut pow_tab: [f32; 128] = [0.0; 128];
        let mut hpow_tab: [f32; 128] = [0.0; 128];
        for i in 0..128 {
            pow_tab[i]  = 2.0f32.powf((i as f32) - 64.0);
            hpow_tab[i] = 2.0f32.powf(((i as f32) - 64.0) * 0.5);
        }
        Self {
            size:       0,
            mdct:       MDCT::new(64),
            tmp:        [0.0; MAX_FRAME_SIZE * 2],
            window:     [0.0; MAX_FRAME_SIZE * 2],
            gain_tab:   [0.0; 23],
            pow_tab, hpow_tab,

            coeffs:     [0.0; MAX_FRAME_SIZE],
            coeffs2:    [0.0; MAX_FRAME_SIZE],
            coupled:    [0.0; MAX_FRAME_SIZE * 2],
        }
    }
}

#[derive(Default)]
struct CookEncoder {
    stream:         Option<NAStreamRef>,
    flavour:        usize,
    force_flv:      bool,
    g8_only:        bool,

    samples:        Vec<Vec<f32>>,
    tgt_size:       usize,
    nframes:        usize,
    frm_per_blk:    usize,
    frm_size:       usize,

    rd_pos:         usize,
    wr_pos:         usize,
    apts:           u64,

    dsp:            CookDSP,
    pairs:          Vec<CookChannelPair>,
    tmp:            TempData,
}

impl CookEncoder {
    fn new() -> Self { Self::default() }
    fn encode_packet(&mut self) -> EncoderResult<NAPacket> {
        if self.rd_pos == self.nframes {
            let mut start = self.wr_pos * self.frm_size;
            let mut dbuf = Vec::with_capacity(self.tgt_size * self.frm_per_blk);
            for _ in 0..self.frm_per_blk {
                let mut channels = self.samples.iter();

                for ch_pair in self.pairs.iter_mut() {
                    if ch_pair.br_info.channels == 1 {
                        let ch = channels.next().unwrap();
                        dbuf = ch_pair.encode_mono(dbuf, &mut self.dsp, &ch[start..][..self.frm_size], 0, &mut self.tmp)?;
                    } else {
                        let l_ch = channels.next().unwrap();
                        let r_ch = channels.next().unwrap();
                        if ch_pair.br_info.js_bits == 0 {
                            dbuf = ch_pair.encode_mono(dbuf, &mut self.dsp, &l_ch[start..][..self.frm_size], 0, &mut self.tmp)?;
                            dbuf = ch_pair.encode_mono(dbuf, &mut self.dsp, &r_ch[start..][..self.frm_size], 1, &mut self.tmp)?;
                        } else {
                            dbuf = ch_pair.encode_jstereo(dbuf, &mut self.dsp, &l_ch[start..][..self.frm_size], &r_ch[start..][..self.frm_size], &mut self.tmp)?;
                        }
                    }
                }

                start += self.frm_size;
            }

            let first = self.wr_pos == 0;

            self.wr_pos += self.frm_per_blk;
            if self.wr_pos == self.nframes {
                self.wr_pos = 0;
                self.rd_pos = 0;
            }

            let stream = self.stream.clone().unwrap();
            let (tb_num, tb_den) = stream.get_timebase();
            let ts = NATimeInfo::new(Some(self.apts), None, Some(1), tb_num, tb_den);
            self.apts += self.frm_per_blk as u64;
            Ok(NAPacket::new(self.stream.clone().unwrap(), ts, first, dbuf))
        } else {
            Err(EncoderError::TryAgain)
        }
    }
}

impl NAEncoder for CookEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Audio(NAAudioInfo::new(0, 1, SND_F32P_FORMAT, 512)),
                    ..Default::default() })
            },
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(ainfo) => {
                if !self.force_flv {
                    let mut outinfo = ainfo;
                    outinfo.format = SND_F32P_FORMAT;
                    outinfo.sample_rate = match ainfo.sample_rate {
                                0..= 8000 => 8000,
                             8001..=11025 => 11025,
                            11026..=22050 => 22050,
                            _             => 44100,
                        };
                    if !matches!(outinfo.channels, 1 | 2) && (outinfo.sample_rate != 44100 || !matches!(outinfo.channels, 4 | 5)) {
                        outinfo.channels = 2;
                    }
                    outinfo.block_len = get_block_size(outinfo.sample_rate);
                    let mut ofmt = *encinfo;
                    ofmt.format = NACodecTypeInfo::Audio(outinfo);
                    Ok(ofmt)
                } else {
                    let flavour = &COOK_FLAVOURS[self.flavour];
                    let blk_len = match flavour.sample_rate {
                            44100 => 1024,
                            22050 => 512,
                                _ => 256,
                        };
                    let newinfo = NAAudioInfo::new(flavour.sample_rate, flavour.channels, SND_F32P_FORMAT, blk_len);
                    let ofmt = EncodeParameters {
                            format:     NACodecTypeInfo::Audio(newinfo),
                            tb_num:     blk_len as u32,
                            tb_den:     flavour.sample_rate,
                            bitrate:    u32::from(flavour.bitrate) * 1000,
                            flags:      ENC_MODE_CBR,
                            quality:    0,
                        };
                    Ok(ofmt)
                }
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_CBR }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(ainfo) => {
                if ainfo.format != SND_F32P_FORMAT || !matches!(ainfo.sample_rate, 8000 | 11025 | 22050 | 44100) {
                    return Err(EncoderError::FormatError);
                }
                let blk_size = get_block_size(ainfo.sample_rate);
                if ainfo.block_len != blk_size {
                    return Err(EncoderError::FormatError);
                }

                let bitrate = if encinfo.bitrate > 0 { Some(encinfo.bitrate / 1000) } else { None };

                let flavours = if self.g8_only { &COOK_FLAVOURS[..RM8_AUDIO] } else { COOK_FLAVOURS };
                let mut cand_id = None;
                for (i, flavour) in flavours.iter().enumerate().rev() {
                    if flavour.sample_rate == ainfo.sample_rate && flavour.channels == ainfo.channels {
                        if let Some(target) = bitrate {
                            if target <= flavour.bitrate.into() {
                                cand_id = Some(i);
                                if target == flavour.bitrate.into() {
                                    break;
                                }
                            }
                        } else {
                            cand_id = Some(i);
                            break;
                        }
                    }
                }

                if let Some(id) = cand_id {
                    self.flavour = id;
                } else {
                    return Err(EncoderError::FormatError);
                }

                let flavour = &COOK_FLAVOURS[self.flavour];
                self.nframes = flavour.frames_per_blk * flavour.factor;
                self.frm_per_blk = flavour.frames_per_blk;
                self.samples.clear();
                self.frm_size = blk_size;
                for _ in 0..flavour.channels {
                    self.samples.push(vec![0.0; self.nframes * self.frm_size]);
                }
                self.dsp.init(self.frm_size);

                self.pairs.clear();
                for br_info in flavour.br_infos() {
                    self.pairs.push(CookChannelPair::new(br_info));
                }

                self.tgt_size = 0;
                for br_info in flavour.br_infos() {
                    self.tgt_size += br_info.frame_bits as usize / 8;
                }

                let mut edata = Vec::new();
                let mut gw = GrowableMemoryWriter::new_write(&mut edata);
                let mut bw = ByteWriter::new(&mut gw);
                if flavour.channels <= 2 {
                    let br_info = &BITRATE_PARAMS[flavour.br_ids[0]];
                    let ch_mode = if br_info.channels == 1 {
                            1
                        } else if br_info.js_bits == 0 {
                            2
                        } else {
                            3
                        };
                    bw.write_u32be((1 << 24) | ch_mode)?;
                    bw.write_u16be((self.frm_size as u16) * u16::from(br_info.channels))?;
                    bw.write_u16be(br_info.max_subbands.into())?;
                    if ch_mode == 3 {
                        bw.write_u32be(0)?; // delay
                        bw.write_u16be(br_info.js_start.into())?;
                        bw.write_u16be(br_info.js_bits.into())?;
                    }
                } else {
                    let chmap: &[u32] = match flavour.channels {
                            1 => &[ 0x04 ], // C
                            2 => &[ 0x03 ], // L,R
                            5 => &[ 0x03, 0x04, 0x30 ], // L,R  C  Ls,Rs
                            6 => &[ 0x03, 0x04, 0x08, 0x30 ], // L,R  C  LFE  Ls,Rs
                            _ => unreachable!(),
                        };
                    for (br_info, &channel_map) in flavour.br_infos().zip(chmap.iter()) {
                        bw.write_u32be(2 << 24)?;
                        bw.write_u16be((self.frm_size as u16) * u16::from(br_info.channels))?;
                        bw.write_u16be(br_info.max_subbands.into())?;
                        bw.write_u32be(0)?; // delay
                        bw.write_u16be(br_info.js_start.into())?;
                        bw.write_u16be(br_info.js_bits.into())?;
                        bw.write_u32be(channel_map)?;
                    }
                }

                let soniton = NASoniton::new(16, 0);
                let out_ainfo = NAAudioInfo::new(ainfo.sample_rate, ainfo.channels, soniton, self.tgt_size);
                let info = NACodecInfo::new("cook", NACodecTypeInfo::Audio(out_ainfo), Some(edata));
                let mut stream = NAStream::new(StreamType::Audio, stream_id, info, blk_size as u32, ainfo.sample_rate, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());
                self.wr_pos = 0;
                self.rd_pos = 0;

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        if let Some(ref abuf) = frm.get_buffer().get_abuf_f32() {
            if self.rd_pos == self.nframes {
                return Err(EncoderError::TryAgain);
            }
            let stride = abuf.get_stride();
            let src = abuf.get_data();
            let start = self.rd_pos * self.frm_size;

            for (dst, src) in self.samples.iter_mut().zip(src.chunks(stride)) {
                dst[start..][..self.frm_size].copy_from_slice(&src[..self.frm_size]);
            }
            self.rd_pos += 1;

            Ok(())
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        if let Ok(pkt) = self.encode_packet() {
            Ok(Some(pkt))
        } else {
            Ok(None)
        }
    }
    fn flush(&mut self) -> EncoderResult<()> {
        if self.wr_pos != 0 {
            let start = self.wr_pos * self.frm_size;
            for channel in self.samples.iter_mut() {
                for el in channel[start..].iter_mut() {
                    *el = 0.0;
                }
            }
            self.wr_pos = self.nframes;
        }
        Ok(())
    }
}

const FLAVOUR_OPTION: &str = "flavor";
const G8_ONLY_OPTION: &str = "g8_only";

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: FLAVOUR_OPTION, description: "Codec-specific profile (0..32, -1 = auto)",
        opt_type: NAOptionDefinitionType::Int(Some(-1), Some((COOK_FLAVOURS.len() - 1) as i64)) },
    NAOptionDefinition {
        name: G8_ONLY_OPTION, description: "Force formats present in RealMedia G8",
        opt_type: NAOptionDefinitionType::Bool },
];

impl NAOptionHandler for CookEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        FLAVOUR_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                if val >= 0 {
                                    self.flavour = val as usize;
                                    self.force_flv = true;
                                } else {
                                    self.force_flv = false;
                                }
                            }
                        },
                        G8_ONLY_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.g8_only = val;
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
            FLAVOUR_OPTION => Some(NAValue::Int(self.flavour as i64)),
            G8_ONLY_OPTION => Some(NAValue::Bool(self.g8_only)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> { Box::new(CookEncoder::new()) }

struct FlavourInfo {
    sample_rate:    u32,
    channels:       u8,
    bitrate:        u16,
    frames_per_blk: usize,
    factor:         usize,
    br_ids:         [usize; 4],
}

impl FlavourInfo {
    fn br_infos(&self) -> BitrateParamsIterator {
        BitrateParamsIterator::new(self.br_ids, self.channels)
    }
}

struct BitrateParamsIterator {
    ids:    [usize; 4],
    cur:    usize,
    len:    usize,
}

impl BitrateParamsIterator {
    fn new(ids: [usize; 4], chans: u8) -> Self {
        Self {
            ids,
            cur: 0,
            len: usize::from((chans + 1) / 2)
        }
    }
}

impl Iterator for BitrateParamsIterator {
    type Item = &'static BitrateParams;
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.len {
            let ret = &BITRATE_PARAMS[self.ids[self.cur]];
            self.cur += 1;
            Some(ret)
        } else {
            None
        }
    }
}

macro_rules! flavour_desc {
    ($srate:expr, $ch:expr, $br:expr, $fpb:expr, $factor:expr, $br_ids:expr) => {
        FlavourInfo {
            sample_rate:    $srate,
            channels:       $ch,
            bitrate:        $br,
            frames_per_blk: $fpb,
            factor:         $factor,
            br_ids:         $br_ids,
        }
    }
}

const RM8_AUDIO: usize = 17; // newer variants that use joint-stereo coding and support multichannel
const COOK_FLAVOURS: &[FlavourInfo] = &[
    flavour_desc!( 8000, 1,   8,  9,  8, [ 0,  0,  0,  0]),
    flavour_desc!(11025, 1,  11, 11,  8, [ 1,  0,  0,  0]),
    flavour_desc!(22050, 1,  16, 12,  8, [ 2,  0,  0,  0]),
    flavour_desc!(22050, 1,  20, 10,  9, [ 3,  0,  0,  0]),
    flavour_desc!(44100, 1,  32,  7, 14, [ 4,  0,  0,  0]),
    flavour_desc!(44100, 1,  44,  5, 16, [ 5,  0,  0,  0]),
    flavour_desc!(44100, 1,  64,  4, 20, [ 6,  0,  0,  0]),
    flavour_desc!(22050, 1,  32,  6, 16, [ 7,  0,  0,  0]),
    flavour_desc!( 8000, 1,   6, 12,  6, [ 8,  0,  0,  0]),
    flavour_desc!(11025, 2,  19, 10, 10, [ 9,  0,  0,  0]),
    flavour_desc!(22050, 2,  32,  6, 14, [10,  0,  0,  0]),
    flavour_desc!(22050, 2,  44,  5, 16, [11,  0,  0,  0]),
    flavour_desc!(44100, 2,  64,  4, 20, [12,  0,  0,  0]),
    flavour_desc!(44100, 2,  95,  3, 30, [13,  0,  0,  0]),
    flavour_desc!(44100, 1,  64,  4, 20, [14,  0,  0,  0]),
    flavour_desc!(44100, 1,  20, 10,  9, [15,  0,  0,  0]),
    flavour_desc!(44100, 1,  32,  7, 14, [16,  0,  0,  0]),
    flavour_desc!(22050, 2,  16, 10, 10, [17,  0,  0,  0]),
    flavour_desc!(22050, 2,  20, 10, 10, [18,  0,  0,  0]),
    flavour_desc!(22050, 2,  20, 10, 10, [19,  0,  0,  0]),
    flavour_desc!(22050, 2,  32,  5, 16, [20,  0,  0,  0]),
    flavour_desc!(44100, 2,  32,  5, 16, [21,  0,  0,  0]),
    flavour_desc!(44100, 2,  44,  5, 16, [22,  0,  0,  0]),
    flavour_desc!(44100, 2,  44,  5, 16, [23,  0,  0,  0]),
    flavour_desc!(44100, 2,  64,  5, 16, [24,  0,  0,  0]),
    flavour_desc!(44100, 2,  96,  5, 16, [25,  0,  0,  0]),
    flavour_desc!(11025, 2,  12, 11,  8, [26,  0,  0,  0]),
    flavour_desc!(44100, 2,  64,  5, 16, [27,  0,  0,  0]),
    flavour_desc!(44100, 2,  96,  5, 16, [28,  0,  0,  0]),
    flavour_desc!(22050, 2,  44,  5, 16, [29,  0,  0,  0]),
    flavour_desc!(44100, 5,  96,  5, 16, [21, 16, 21,  0]),
    flavour_desc!(44100, 6, 131,  3, 10, [23,  5, 30, 21]),
    flavour_desc!(44100, 6, 183,  2, 10, [24,  6, 30, 23]),
    flavour_desc!(44100, 6, 268,  1,  1, [25,  6, 30, 25])
];

struct BitrateParams {
    channels:       u8,
    max_subbands:   u8,
    frame_bits:     u32,
    js_start:       u8,
    js_bits:        u8,
}

macro_rules! br_desc {
    ($ch:expr, $bits:expr, $subbands:expr, $js_start:expr, $js_bits:expr) => {
        BitrateParams {
            channels:       $ch,
            max_subbands:   $subbands,
            frame_bits:     $bits,
            js_start:       $js_start,
            js_bits:        $js_bits,
        }
    }
}

const BITRATE_PARAMS: &[BitrateParams] = &[
    br_desc!(1,  256, 12,  0, 0),
    br_desc!(1,  256, 12,  0, 0),
    br_desc!(1,  376, 18,  0, 0),
    br_desc!(1,  480, 23,  0, 0),
    br_desc!(1,  744, 37,  0, 0),
    br_desc!(1, 1024, 47,  0, 0),
    br_desc!(1, 1488, 47,  0, 0),
    br_desc!(1,  744, 24,  0, 0),

    br_desc!(1,  192,  9,  0, 0),
    br_desc!(2,  464, 11,  0, 0),
    br_desc!(2,  752, 18,  0, 0),
    br_desc!(2, 1024, 24,  0, 0),
    br_desc!(2, 1488, 37,  0, 0),
    br_desc!(2, 2224, 47,  0, 0),
    br_desc!(1, 1488, 47,  0, 0),
    br_desc!(1,  480, 47,  0, 0),

    br_desc!(1,  744, 47,  0, 0),
    br_desc!(2,  384, 16,  1, 3),
    br_desc!(2,  480, 20,  1, 3),
    br_desc!(2,  480, 23,  1, 3),
    br_desc!(2,  744, 24,  2, 4),
    br_desc!(2,  744, 32,  2, 4),
    br_desc!(2, 1024, 32,  5, 5),
    br_desc!(2, 1024, 37,  2, 4),

    br_desc!(2, 1488, 37,  6, 5),
    br_desc!(2, 2240, 37,  8, 5),
    br_desc!(2,  288,  9,  1, 2),
    br_desc!(2, 1488, 30, 17, 5),
    br_desc!(2, 2240, 34, 19, 5),
    br_desc!(2, 1024, 23, 17, 5),
    br_desc!(1,  256,  1,  0, 0)
];

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_codec_support::test::enc_video::*;
    use crate::*;

    fn test_encoder(name: &'static str, enc_options: &[NAOption], bitrate: u32, channels: u8, hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        realmedia_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        realmedia_register_all_encoders(&mut enc_reg);

        // sample from a private collection
        let dec_config = DecoderTestParams {
                demuxer:        "realmedia",
                in_name:        "assets/RV/rv30_weighted_mc.rm",
                stream_type:    StreamType::Audio,
                limit:          None,
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "realmedia",
                enc_name:       "cook",
                out_name:       name,
                mux_reg, enc_reg,
            };
        let dst_ainfo = NAAudioInfo {
                sample_rate:    0,
                channels,
                format:         SND_F32P_FORMAT,
                block_len:      1024,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Audio(dst_ainfo),
                quality: 0,
                bitrate,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);

        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options, hash);
    }
    #[test]
    fn test_cook_encoder_mono() {
        let enc_options = &[
        ];
        test_encoder("cook-mono.rma", enc_options, 0, 1,
            &[0xa07cffaa, 0x257e8ca1, 0x27f58960, 0xb48f64f4]);
    }
    #[test]
    fn test_cook_encoder_stereo() {
        let enc_options = &[
            NAOption { name: super::G8_ONLY_OPTION, value: NAValue::Bool(true) },
        ];
        test_encoder("cook-stereo.rma", enc_options, 0, 2,
            &[0xbc407879, 0x18c7b334, 0xc0299482, 0x89c6b953]);
    }
    #[test]
    fn test_cook_encoder_jstereo_32kbps() {
        let enc_options = &[
        ];
        test_encoder("cook-jstereo32.rma", enc_options, 32000, 2,
            &[0xe7526c4e, 0xda095156, 0x840a74f7, 0x0e9a1603]);
    }
    #[test]
    fn test_cook_encoder_jstereo_64kbps() {
        let enc_options = &[
        ];
        test_encoder("cook-jstereo64.rma", enc_options, 64000, 2,
            &[0x09688175, 0x9abe1aac, 0x3c3f15fb, 0x066b99f0]);
    }
    #[test]
    fn test_cook_encoder_jstereo_96kbps() {
        let enc_options = &[
        ];
        test_encoder("cook-jstereo96.rma", enc_options, 96000, 2,
            &[0x51829ff8, 0x00017191, 0x5bc95490, 0xd1d03faf]);
    }
}
