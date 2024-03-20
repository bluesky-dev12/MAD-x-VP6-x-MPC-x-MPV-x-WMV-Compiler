use nihav_core::codecs::*;
use nihav_core::io::codebook::*;
use nihav_codec_support::dsp::qmf::QMF;
use super::qdmcommon::*;
use super::qdm2::TONE_SCALES;

const MAX_BANDS: usize = 30;

const NOISE_TAB_LEN: usize = 3840;

struct NoiseGen {
    noise_tab:          [f32; NOISE_TAB_LEN],
    idx:                usize,
}

impl NoiseGen {
    fn new() -> Self {
        let mut noise_tab = [0.0; NOISE_TAB_LEN];
        let mut rnd = RNG::new();
        for el in noise_tab.iter_mut() {
            *el = rnd.next_float() * 1.3;
        }

        Self { noise_tab, idx: 0 }
    }
    fn next(&mut self, band: usize) -> f32 {
        let ret = self.noise_tab[self.idx];
        self.idx += 1;
        if self.idx >= NOISE_TAB_LEN {
            self.idx -= NOISE_TAB_LEN;
        }
        ret * SB_NOISE_ATTENUATION[band]
    }
}

struct Tables {
    noise_samples:  [f32; 128],
    mod3:           [[u8; 5]; 243],
    mod5:           [[u8; 3]; 125],
}

impl Tables {
    fn new() -> Self {
        let mut noise_samples = [0.0; 128];
        let mut rnd = RNG::new();
        for el in noise_samples.iter_mut() {
            *el = rnd.next_float();
        }

        let mut mod3 = [[0u8; 5]; 243];
        for (i, row) in mod3.iter_mut().enumerate() {
            let mut base = 81u8;
            let mut low = i as u8;
            for el in row.iter_mut() {
                *el = low / base;
                low %= base;
                base /= 3;
            }
        }

        let mut mod5 = [[0u8; 3]; 125];
        for (i, row) in mod5.iter_mut().enumerate() {
            let mut base = 25u8;
            let mut low = i as u8;
            for el in row.iter_mut() {
                *el = low / base;
                low %= base;
                base /= 5;
            }
        }

        Self {
            noise_samples, mod3, mod5,
        }
    }
}

struct Codebooks {
    level_cb:               Codebook<u8>,
    level_diff_cb:          Codebook<u8>,
    run_cb:                 Codebook<u8>,
    tone_idx_mid_cb:        Codebook<u8>,
    tone_idx_high1_cb:      Codebook<u8>,
    tone_idx_high2_cb:      Codebook<u8>,
    type30_codes_cb:        Codebook<u8>,
    type34_codes_cb:        Codebook<u8>,
}

fn map_idx(idx: usize) -> u8 { idx as u8 }

macro_rules! create_codebook {
    ($codes: expr, $bits: expr) => ({
        let mut cbr = TableCodebookDescReader::new($codes, $bits, map_idx);
        Codebook::new(&mut cbr, CodebookMode::LSB).unwrap()
    })
}

impl Codebooks {
    fn new() -> Self {
        let level_cb = create_codebook!(LEVEL_CODES, LEVEL_BITS);
        let level_diff_cb = create_codebook!(LEVEL_DIFF_CODES, LEVEL_DIFF_BITS);
        let run_cb = create_codebook!(RUN_CODES, RUN_BITS);

        let tone_idx_mid_cb = create_codebook!(TONE_IDX_MID_CODES, TONE_IDX_MID_BITS);
        let tone_idx_high1_cb = create_codebook!(TONE_IDX_HIGH1_CODES, TONE_IDX_HIGH1_BITS);
        let tone_idx_high2_cb = create_codebook!(TONE_IDX_HIGH2_CODES, TONE_IDX_HIGH2_BITS);

        let type30_codes_cb = create_codebook!(TYPE30_CODES, TYPE30_BITS);
        let type34_codes_cb = create_codebook!(TYPE34_CODES, TYPE34_BITS);
        Self {
            level_cb, level_diff_cb, run_cb,
            tone_idx_mid_cb, tone_idx_high1_cb, tone_idx_high2_cb,
            type30_codes_cb, type34_codes_cb,
        }
    }
}

pub struct QDM2QMF {
    qmf:        [QMF; 2],
    cbs:        Codebooks,
    tables:     Tables,
    noisegen:   NoiseGen,

    pub is_intra:       bool,

    num_bands:          usize,
    subsampling:        u8,
    cm_selector:        usize,
    coef_per_sb_sel:    usize,
    channels:           usize,

    grid_2_quant:       [[[i8; 8]; 10]; 2],
    grid_1_quant:       [[[[i8; 8]; 8]; 3]; 2],
    grid_3_quant:       [[i8; MAX_BANDS]; 2],
    tone_idx_mid:       [[[i8; 8]; MAX_BANDS]; 2],
    tone_idx_base:      [[[i8; 8]; MAX_BANDS]; 2],
    tone_idx:           [[[i8; 64]; MAX_BANDS]; 2],
    quant_weight:       [[[u8; 64]; MAX_BANDS]; 2],
    sb_samples:         [[[f32; 32]; 128]; 2],
    tone_scale:         [[[f32; 64]; MAX_BANDS]; 2],
}

impl QDM2QMF {
    pub fn new() -> Self {
        Self {
            qmf:        [QMF::new(), QMF::new()],
            cbs:        Codebooks::new(),
            tables:     Tables::new(),
            noisegen:   NoiseGen::new(),

            num_bands:          0,
            subsampling:        0,
            cm_selector:        0,
            coef_per_sb_sel:    0,
            channels:           0,
            is_intra:           false,

            grid_2_quant:       [[[0; 8]; 10]; 2],
            grid_1_quant:       [[[[0; 8]; 8]; 3]; 2],
            grid_3_quant:       [[0; MAX_BANDS]; 2],
            tone_idx_mid:       [[[0; 8]; MAX_BANDS]; 2],
            tone_idx_base:      [[[0; 8]; MAX_BANDS]; 2],
            tone_idx:           [[[0; 64]; MAX_BANDS]; 2],
            quant_weight:       [[[0; 64]; MAX_BANDS]; 2],
            sb_samples:         [[[0.0; 32]; 128]; 2],
            tone_scale:         [[[0.0; 64]; MAX_BANDS]; 2],
        }
    }
    pub fn set_ch_and_subsampling(&mut self, channels: usize, subsampling: u8, full_bitrate: u32) {
        self.channels    = channels;
        self.subsampling = subsampling;
        self.num_bands   = (8 << self.subsampling).min(MAX_BANDS);

        let br = match self.subsampling * 2 + (channels as u8) - 1 {
                0 => 40,
                1 => 48,
                2 => 56,
                3 => 72,
                4 => 80,
                5 => 100,
                _ => unreachable!(),
            };
        self.cm_selector = if br * 1000 < full_bitrate {
                1
            } else if br * 1440 < full_bitrate {
                2
            } else if br * 1760 < full_bitrate {
                3
            } else if br * 2240 < full_bitrate {
                4
            } else {
                0
            };
        self.coef_per_sb_sel = if full_bitrate <= 8000 { 0 } else if full_bitrate < 16000 { 1 } else { 2 };
    }
    fn average_grid_quants(&mut self) {
        let ncoef = (COEFFS_PER_SB_AVG[self.coef_per_sb_sel][self.subsampling as usize] as usize) + 1;
        for ch in 0..self.channels {
            for i in 0..ncoef {
                let mut sum = 0;
                for el in self.grid_2_quant[ch][i].iter() {
                    sum += i16::from(*el);
                }
                sum /= 8;
                if sum > 0 {
                    sum -= 1;
                }
                self.grid_2_quant[ch][i] = [sum as i8; 8];
            }
        }
    }
    fn read_array(br: &mut QdmBitReader, dst: &mut [i8; 8], codebooks: &Codebooks) -> DecoderResult<()> {
        if br.left() < 15 { return Ok(()); }
        let mut last                    = br.read_code(&codebooks.level_cb)? as i32;
        dst[0] = last as i8;
        let mut idx = 1;
        while idx < 8 {
            let len                     = (br.read_code(&codebooks.run_cb)? as usize) + 1;
            let diff                    = br.read_code(&codebooks.level_diff_cb)? as i32;
            let diff = to_signed(diff);
            let val = last + diff;
            validate!(len + idx <= 8);
            for i in 1..=len {
                dst[idx] = (last + (i as i32) * diff / (len as i32)) as i8;
                idx += 1;
            }
            last = val;
        }
        Ok(())
    }
    pub fn read_type_9(&mut self, br: &mut QdmBitReader) -> DecoderResult<()> {
        let nbands = (COEFFS_PER_SB_AVG[self.coef_per_sb_sel][self.subsampling as usize] as usize) + 1;
        for i in 1..nbands {
            for ch in 0..self.channels {
                Self::read_array(br, &mut self.grid_2_quant[ch][i], &self.cbs)?;
            }
        }
        for ch in 0..self.channels {
            self.grid_2_quant[ch][0] = [0; 8];
        }
        Ok(())
    }
    pub fn read_type_10(&mut self, br: &mut QdmBitReader) -> DecoderResult<()> {
        for ch in 0..self.channels {
            let _ret = Self::read_array(br, &mut self.grid_2_quant[ch][0], &self.cbs);
            if br.left() < 16 {
                self.grid_2_quant[ch][0] = [0; 8];
                break;
            }
        }

        let n = (self.subsampling as usize) + 1;
        for band in 0..n {
            for ch in 0..self.channels {
                for i in 0..8 {
                    if br.read_bool() {
                        for el in self.grid_1_quant[ch][band][i].iter_mut() {
                            *el         = br.read_code(&self.cbs.tone_idx_high1_cb)? as i8;
                        }
                    } else {
                        self.grid_1_quant[ch][band][i] = [0; 8];
                    }
                }
            }
        }
        for band in 0..self.num_bands - 4 {
            for ch in 0..self.channels {
                if br.left() < 16 { break; }
                self.grid_3_quant[ch][band] = br.read_code(&self.cbs.tone_idx_high2_cb)? as i8;
                if band > 19 {
                    self.grid_3_quant[ch][band] -= 16;
                } else {
                    self.tone_idx_mid[ch][band] = [-16; 8];
                }
            }
        }
        for band in 0..self.num_bands - 5 {
            for ch in 0..self.channels {
                for i in 0..8 {
                    if br.left() < 16 { break; }
                    self.tone_idx_mid[ch][band][i] = (br.read_code(&self.cbs.tone_idx_mid_cb)? as i8) - 32;
                }
            }
        }

        self.set_tone_scales(true);
        Ok(())
    }
    fn inc_quant_weight(&mut self, band: usize) -> bool {
        let rlen = 128 / self.channels;
        for ch in 0..self.channels {
            let mut idx = 0;
            while idx < rlen {
                if self.quant_weight[ch][band][idx] < 8 {
                    return false;
                }
                let (val, run) = match self.quant_weight[ch][band][idx] {
                        8  => (10, 16),
                        10 => (16, 1),
                        16 => (24, 5),
                        24 => (30, 3),
                        30 => (30, 1),
                        _  => (8, 1),
                    };
                let len = run.min(rlen - idx);
                let ref_val = self.quant_weight[ch][band][idx];
                for _ in 0..len {
                    if self.quant_weight[ch][band + idx / 64][idx % 64] > ref_val {
                        self.quant_weight[ch][band][idx] = val;
unimplemented!();
                    }
                    idx += 1;
                }
            }
        }
        true
    }
    fn set_tone_scales(&mut self, has_data: bool) {
        const LAST_COEFF: [usize; 3] = [ 4, 7, 10 ];

        let csel = self.coef_per_sb_sel;
        for ch in 0..self.channels {
            for band in 0..MAX_BANDS {
                for i in 0..8 {
                    let csb = COEFFS_PER_SB_DEQUANT[csel][band] as usize;
                    let mut q = i32::from(self.grid_2_quant[ch][csb][i]) * i32::from(DEQUANT[csel][csb][band]);
                    if csb < LAST_COEFF[csel] - 1 {
                        q += i32::from(self.grid_2_quant[ch][csb + 1][i]) * i32::from(DEQUANT[csel][csb + 1][band]);
                    }
                    if q < 0 {
                        q += 255;
                    }
                    self.tone_idx_base[ch][band][i] = (q / 256) as i8;
                }
            }
        }
        if !self.is_intra && !has_data {
            for band in 0..self.num_bands {
                for ch in 0..self.channels {
                    for i in 0..64 {
                        self.tone_idx[ch][band][i] = self.tone_idx[ch][band][i / 8];
                        if self.tone_idx[ch][band][i] >= 0 {
                            self.tone_scale[ch][band][i] = TONE_SCALES[0][(self.tone_idx[ch][band][i] & 0x3F) as usize];
                        } else {
                            self.tone_scale[ch][band][i] = 0.0;
                        }
                    }
                }
            }
        } else {
            for band in 0..self.num_bands {
                for ch in 0..self.channels {
                    for i in 0..64 {
                        let mut q = self.tone_idx_base[ch][band][i / 8];
                        if band >= 4 {
                            q = q.wrapping_sub(self.grid_1_quant[ch][(band / 8).min(2)][i / 8][i % 8]);
                            if band < 24 {
                                q = q.wrapping_sub(self.tone_idx_mid[ch][band - 4][i / 8]);
                            }
                            q = q.wrapping_sub(self.grid_3_quant[ch][band - 4]);
                        }
                        self.tone_idx[ch][band][i] = q;
                        if q > 0 || (self.is_intra && q == 0) {
                            self.tone_scale[ch][band][i] = TONE_SCALES[0][(q & 0x3F) as usize];
                        } else {
                            self.tone_scale[ch][band][i] = 0.0;
                        }
                    }
                }
            }
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn read_noise_band(&mut self, br: &mut QdmBitReader, ch: usize, band: usize, samples: &mut [f32; 10], signs: &[bool; 16], jstereo: bool) -> DecoderResult<()> {
        let mut type34_first = true;
        let mut type34_pred = 0.0;
        let mut type34_scale = 0.0;
        let zero_coding                 = br.read_bool();
        let mut idx = 0;
        while idx < 128 {
            let len;
            match self.quant_weight[ch][band][idx / 2] {
                8 => {
                    if br.left() >= 10 {
                        if zero_coding {
                            for i in 0..5 {
                                if idx + i * 2 >= 128 { break; }
                                samples[i * 2] = if br.read_bool() {
                                        let ix = (br.read(1) as usize) * 2;
                                        QUANT_1BIT[jstereo as usize][ix]
                                    } else { 0.0 };
                            }
                        } else {
                            let idx = br.read(8) as usize;
                            validate!(idx < self.tables.mod3.len());
                            for i in 0..5 {
                                let k = self.tables.mod3[idx][i] as usize;
                                samples[i * 2] = QUANT_1BIT[jstereo as usize][k];
                            }
                        }
                        for el in samples.chunks_mut(2) {
                            el[1] = self.noisegen.next(band);
                        }
                    } else {
                        for el in samples.iter_mut() {
                            *el = self.noisegen.next(band);
                        }
                    }
                    len = 10;
                },
                10 => {
                    if br.left() > 0 {
                        let mut scale   = if br.read_bool() { -0.81 } else { 0.81 };
                        scale -= self.tables.noise_samples[((band + 1) * (idx + 5 * ch + 1)) & 0x7F] * 9.0 / 40.0;
                        samples[0] = scale;
                    } else {
                        samples[0] = self.noisegen.next(band);
                    }
                    len = 1;
                },
                16 => {
                    if br.left() >= 10 {
                        if zero_coding {
                            for i in 0..5 {
                                if idx + i >= 128 { break; }
                                samples[i] = if br.read_bool() {
                                        let ix = (br.read(1) as usize) * 2;
                                        QUANT_1BIT[jstereo as usize][ix]
                                    } else { 0.0 };
                            }
                        } else {
                            let idx = br.read(8) as usize;
                            validate!(idx < self.tables.mod3.len());
                            for i in 0..5 {
                                let k = self.tables.mod3[idx][i] as usize;
                                samples[i] = QUANT_1BIT[jstereo as usize][k];
                            }
                        }
                    } else {
                        for el in samples.iter_mut().take(5) {
                            *el = self.noisegen.next(band);
                        }
                    }
                    len = 5;
                },
                24 => {
                    if br.left() >= 7 {
                        let idx         = br.read(7) as usize;
                        validate!(idx < self.tables.mod5.len());
                        for i in 0..3 {
                            let k = self.tables.mod5[idx][i] as usize;
                            samples[i] = ((k as f32) - 2.0) * 0.5;
                        }
                    } else {
                        for el in samples.iter_mut().take(3) {
                            *el = self.noisegen.next(band);
                        }
                    }
                    len = 3;
                },
                30 => {
                    if br.left() >= 4 {
                        let idx         = br.read_code(&self.cbs.type30_codes_cb).unwrap_or(99) as usize;
                        if idx < QUANT_TYPE30.len() - 1 {
                            samples[0] = QUANT_TYPE30[idx];
                        } else {
                            samples[0] = self.noisegen.next(band);
                        }
                    } else {
                        samples[0] = self.noisegen.next(band);
                    }
                    len = 1;
                },
                34 => {
                    if br.left() >= 7 {
                        if type34_first {
                            type34_first = false;
                            type34_scale = 1.0 / ((1 << br.read(2)) as f32);
                            type34_pred = ((br.read(5) as f32) - 16.0) / 15.0;
                            samples[0] = type34_pred;
                        } else {
                            let idx     = br.read_code(&self.cbs.type34_codes_cb).unwrap_or(99) as usize;
                            if idx < TYPE34_DIFF.len() - 1 {
                                samples[0] = type34_pred + TYPE34_DIFF[idx] * type34_scale;
                                type34_pred = samples[0];
                            } else {
                                samples[0] = self.noisegen.next(band);
                            }
                        }
                    } else {
                        samples[0] = self.noisegen.next(band);
                    }
                    len = 1;
                },
                _ => {
                    len = 1;
                },
            };
            let llen = len.min(128 - idx);
            if !jstereo {
                for samp in samples.iter().take(llen) {
                    self.sb_samples[ch][idx][band] = self.tone_scale[ch][band][idx / 2] * *samp;
                    idx += 1;
                }
            } else {
                for samp in samples.iter().take(llen) {
                    self.sb_samples[0][idx][band] = self.tone_scale[0][band][idx / 2] * *samp;
                    if self.channels == 2 {
                        let sample = if signs[idx / 8] { -*samp } else { *samp };
                        self.sb_samples[1][idx][band] = self.tone_scale[1][band][idx / 2] * sample;
                    }
                    idx += 1;
                }
            }
        }
        Ok(())
    }
    fn read_band_data(&mut self, br: &mut QdmBitReader, start: usize, end: usize) -> DecoderResult<()> {
        let mut samples = [0.0f32; 10];
        let mut signs = [false; 16];
        for band in start..end {
            let jstereo = if self.channels == 1 || band < 12 {
                    false
                } else if band >= 24 {
                    true
                } else {
                                        br.read_bool()
                };
            if jstereo {
                if br.left() >= 16 {
                    for el in signs.iter_mut() {
                        *el             = br.read_bool();
                    }
                }
                for i in 0..64 {
                    self.quant_weight[0][band][i] = self.quant_weight[0][band][i].max(self.quant_weight[1][band][i]);
                }
                if !self.inc_quant_weight(band) {
                    self.fill_noise(band);
                    continue;
                }
            }

            let band_chan = if jstereo { 1 } else { self.channels };
            for ch in 0..band_chan {
                self.read_noise_band(br, ch, band, &mut samples, &signs, jstereo)?;
            }
        }
        Ok(())
    }
    fn fill_noise(&mut self, band: usize) {
        for ch in 0..self.channels {
            for i in 0..128 {
                self.sb_samples[ch][i][band] = self.noisegen.next(band) * self.tone_scale[ch][band][i / 2];
            }
        }
    }
    pub fn read_type_11(&mut self, br: &mut QdmBitReader) -> DecoderResult<()> {
        if br.left() >= 32 {
            let c                       = br.read(13);
            if c > 3 {
                if self.is_intra {
                    for ch in 0..self.channels {
                        for band in 0..MAX_BANDS {
                            let sb = QUANT_WEIGHT[self.cm_selector][band];
                            self.quant_weight[ch][band] = [sb; 64];
                        }
                    }
                } else {
unimplemented!();
                }
            }
        }
        self.read_band_data(br, 0, 8)?;
        Ok(())
    }
    pub fn read_type_12(&mut self, br: &mut QdmBitReader) -> DecoderResult<()> {
        self.read_band_data(br, 8, self.num_bands)?;
        Ok(())
    }
    pub fn fill_default(&mut self, id: u8) {
        match id {
            10 => {
                self.set_tone_scales(false);
            },
            11 => {
                for band in 0..8 {
                    self.fill_noise(band);
                }
            },
            12 => {
                for band in 8..self.num_bands {
                    self.fill_noise(band);
                }
            },
            _ => {},
        };
    }
    pub fn new_frame(&mut self) {
        self.grid_1_quant = [[[[0; 8]; 8]; 3]; 2];
        self.grid_3_quant = [[0; MAX_BANDS]; 2];
        self.tone_idx_mid = [[[0; 8]; MAX_BANDS]; 2];
        self.average_grid_quants();
    }
    pub fn synth(&mut self, dst: &mut [f32], sf: usize, ch: usize) {
        let mut osamps = [0.0f32; 32 * 8];
        let ssamp = 4 >> self.subsampling;
        for (i, out) in osamps.chunks_mut(32).enumerate() {
            self.qmf[ch].synth(&self.sb_samples[ch][sf * 8 + i], out);
        }
        let scale = 1.0 / ((1 << self.subsampling) as f32);
        for (src, dst) in osamps.chunks(ssamp).zip(dst.iter_mut()) {
            *dst += src[0] * scale;
        }
    }
    pub fn flush(&mut self) {
        for qmf in self.qmf.iter_mut() {
            qmf.reset();
        }
    }
}

const LEVEL_CODES: &[u16; 24] = &[
    0x37C, 0x004, 0x03C, 0x04C, 0x03A, 0x02C, 0x01C, 0x01A,
    0x024, 0x014, 0x001, 0x002, 0x000, 0x003, 0x007, 0x005,
    0x006, 0x008, 0x009, 0x00A, 0x00C, 0x0FC, 0x07C, 0x17C
];
const LEVEL_BITS: &[u8; 24] = &[
    10,  6,  7,  7,  6,  6,  6,  6,  6,  5,  4,  4,  4,  3,  3,  3,
     3,  4,  4,  5,  7,  8,  9, 10
];

const LEVEL_DIFF_CODES: &[u16; 37] = &[
    0x1C57, 0x0004, 0x0000, 0x0001, 0x0003, 0x0002, 0x000F, 0x000E,
    0x0007, 0x0016, 0x0037, 0x0027, 0x0026, 0x0066, 0x0006, 0x0097,
    0x0046, 0x01C6, 0x0017, 0x0786, 0x0086, 0x0257, 0x00D7, 0x0357,
    0x00C6, 0x0386, 0x0186, 0x0000, 0x0157, 0x0C57, 0x0057, 0x0000,
    0x0B86, 0x0000, 0x1457, 0x0000, 0x0457
];
const LEVEL_DIFF_BITS: &[u8; 37] = &[
    13,  3,  3,  2,  3,  3,  4,  4,  6,  5,  6,  6,  7,  7,  8,  8,
     8,  9,  8, 11,  9, 10,  8, 10,  9, 12, 10,  0, 10, 13, 11,  0,
    12,  0, 13,  0, 13
];

const RUN_CODES: &[u8; 6] = &[ 0x1F, 0x00, 0x01, 0x03, 0x07, 0x0F ];
const RUN_BITS: &[u8; 6] = &[ 5, 1, 2, 3, 4, 5 ];

const TONE_IDX_HIGH1_CODES: &[u16; 20] = &[
    0x5714, 0x000C, 0x0002, 0x0001, 0x0000, 0x0004, 0x0034, 0x0054,
    0x0094, 0x0014, 0x0114, 0x0214, 0x0314, 0x0614, 0x0E14, 0x0F14,
    0x2714, 0x0714, 0x1714, 0x3714
];
const TONE_IDX_HIGH1_BITS: &[u8; 20] = &[
    15, 4, 2, 1, 3, 5, 6, 7, 8, 10, 10, 11, 11, 12, 12, 12, 14, 14, 15, 14
];

const TONE_IDX_HIGH2_CODES: &[u16; 24] = &[
    0x0664, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0064, 0x00E4,
    0x00A4, 0x0068, 0x0004, 0x0008, 0x0014, 0x0018, 0x0000, 0x0001,
    0x0002, 0x0003, 0x000C, 0x0028, 0x0024, 0x0164, 0x0000, 0x0264
];
const TONE_IDX_HIGH2_BITS: &[u8; 24] = &[
    11, 0, 0, 0, 0, 0, 10, 8, 8, 7, 6, 6, 5, 5, 4, 2, 2, 2, 4, 7, 8, 9, 0, 11
];

const TONE_IDX_MID_CODES: &[u16; 24] = &[
    0x0FEA, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x03EA, 0x00EA, 0x002A, 0x001A,
    0x0006, 0x0001, 0x0000, 0x0002, 0x000A, 0x006A, 0x01EA, 0x07EA
];
const TONE_IDX_MID_BITS: &[u8; 24] = &[
    12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 11,  9,  7,  5,
     3,  1,  2,  4,  6,  8, 10, 12
];

const TYPE30_CODES: &[u8; 9] = &[ 0x3C, 0x06, 0x00, 0x01, 0x03, 0x02, 0x04, 0x0C, 0x1C ];
const TYPE30_BITS: &[u8; 9] = &[ 6, 3, 3, 2, 2, 3, 4, 5, 6 ];

const TYPE34_CODES: &[u8; 10] = &[ 0x18, 0x00, 0x01, 0x04, 0x05, 0x07, 0x03, 0x02, 0x06, 0x08 ];
const TYPE34_BITS: &[u8; 10] = &[ 5, 4, 3, 3, 3, 3, 3, 3, 3, 5 ];

const SB_NOISE_ATTENUATION: [f32; 32] = [
    0.0, 0.0, 0.3, 0.4, 0.5, 0.7, 1.0, 1.0,
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
];

const QUANT_1BIT: [[f32; 3]; 2] = [[ -0.92, 0.0, 0.92 ], [ -0.89, 0.0, 0.89 ]];

const QUANT_TYPE30: [f32; 8] = [
    -1.0, -0.625, -0.291666656732559, 0.0, 0.25, 0.5, 0.75, 1.0
];

const TYPE34_DIFF: [f32; 10] = [
    -1.0, -0.60947573184967, -0.333333343267441, -0.138071194291115, 0.0,
    0.138071194291115, 0.333333343267441, 0.60947573184967, 1.0, 0.0
];

const DEQUANT: [[[u16; 30]; 10]; 3] = [
  [
    [ 256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 256, 256, 205, 154, 102, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 51, 102, 154, 205, 256, 238, 219, 201, 183, 165, 146, 128, 110, 91, 73, 55, 37, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 18, 37, 55, 73, 91, 110, 128, 146, 165, 183, 201, 219, 238, 256, 228, 199, 171, 142, 114, 85, 57, 28 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
  ], [
    [ 256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 256, 171, 85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 85, 171, 256, 171, 85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 85, 171, 256, 219, 183, 146, 110, 73, 37, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 37, 73, 110, 146, 183, 219, 256, 228, 199, 171, 142, 114, 85, 57, 28, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 28, 57, 85, 114, 142, 171, 199, 228, 256, 213, 171, 128, 85, 43 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
  ], [
    [ 256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 256, 256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 256, 171, 85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 85, 171, 256, 192, 128, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 128, 192, 256, 205, 154, 102, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 51, 102, 154, 205, 256, 213, 171, 128, 85, 43, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 43, 85, 128, 171, 213, 256, 213, 171, 128, 85, 43 ]
  ]
];

/*const TONE_LEVEL_IDX_OFFSET: [[i8; 4]; 30] = [
    [ -50, -50,  0, -50 ],
    [ -50, -50,  0, -50 ],
    [ -50,  -9,  0, -19 ],
    [ -16,  -6,  0, -12 ],
    [ -11,  -4,  0,  -8 ],
    [  -8,  -3,  0,  -6 ],
    [  -7,  -3,  0,  -5 ],
    [  -6,  -2,  0,  -4 ],
    [  -5,  -2,  0,  -3 ],
    [  -4,  -1,  0,  -3 ],
    [  -4,  -1,  0,  -2 ],
    [  -3,  -1,  0,  -2 ],
    [  -3,  -1,  0,  -2 ],
    [  -3,  -1,  0,  -2 ],
    [  -2,  -1,  0,  -1 ],
    [  -2,  -1,  0,  -1 ],
    [  -2,  -1,  0,  -1 ],
    [  -2,   0,  0,  -1 ],
    [  -2,   0,  0,  -1 ],
    [  -1,   0,  0,  -1 ],
    [  -1,   0,  0,  -1 ],
    [  -1,   0,  0,  -1 ],
    [  -1,   0,  0,  -1 ],
    [  -1,   0,  0,  -1 ],
    [  -1,   0,  0,  -1 ],
    [  -1,   0,  0,  -1 ],
    [  -1,   0,  0,   0 ],
    [  -1,   0,  0,   0 ],
    [  -1,   0,  0,   0 ],
    [  -1,   0,  0,   0 ]
];*/

const COEFFS_PER_SB_AVG: [[u8; 4]; 3] = [
    [ 2, 3, 3, 3 ],
    [ 4, 5, 6, 6 ],
    [ 5, 7, 9, 9 ]
];

/*const COEFFS_PER_SB_AVG: [[u8; 30]; 3] = [
  [ 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 ],
  [ 0, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 ],
  [ 0, 1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9 ]
];*/

const COEFFS_PER_SB_DEQUANT: [[u8; 30]; 3] = [
  [ 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3 ],
  [ 0, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6 ],
  [ 0, 1, 2, 3, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9 ]
];

const QUANT_WEIGHT: [[u8; 30]; 5] = [
  [
    34, 30, 24, 24, 16, 16, 16, 16, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
  ], [
    34, 30, 24, 24, 16, 16, 16, 16, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
  ], [
    34, 30, 30, 30, 24, 24, 16, 16, 16, 16, 16, 16, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
  ], [
    34, 34, 30, 30, 24, 24, 24, 24, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 10, 10, 10, 10, 10, 10, 10, 10
  ], [
    34, 34, 30, 30, 30, 30, 30, 30, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16
  ]
];
