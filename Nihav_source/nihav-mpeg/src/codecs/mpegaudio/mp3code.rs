use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;

use super::SAMPLES;
use super::mp3data::*;

const MP3_MAX_BANDS: usize = MP3_BANDS_SHORT * 3 + 4;
const IS_MODE: u8 = 1;
const MS_MODE: u8 = 2;

#[derive(Clone,Copy)]
struct Granule {
    part2_3_length:     usize,
    big_values:         usize,
    global_gain:        u8,
    scalefac_compress:  usize,
    blocksplit:         bool,
    block_type:         u8,
    switch_point:       bool,
    table_select:       [u8; 3],
    subblock_gain:      [u8; 3],
    region_address1:    u8,
    region_address2:    u8,
    preflag:            bool,
    scalefac_scale:     bool,
    count1table_select: bool,
    scalefac:           [u8; MP3_MAX_BANDS],
    istereo:            [u8; MP3_MAX_BANDS],

    lastcoded:          usize,
    zero_part:          usize,
}

impl Default for Granule {
    fn default() -> Self { unsafe { std::mem::MaybeUninit::zeroed().assume_init() } }
}

impl Granule {
    fn get_mpeg2_params(&self, bits: &mut [u8; 4], independent: bool) -> usize {
        if independent {
            match self.scalefac_compress {
                0..=399 => {
                    bits[0] = MP3_SCF_BITS5[(self.scalefac_compress >> 4) * 2];
                    bits[1] = MP3_SCF_BITS5[(self.scalefac_compress >> 4) * 2 + 1];
                    bits[2] = ((self.scalefac_compress >> 2) & 3) as u8;
                    bits[3] =  (self.scalefac_compress       & 3) as u8;
                    0
                },
                400..=499 => {
                    let idx = self.scalefac_compress - 400;
                    bits[0] = MP3_SCF_BITS5[(idx >> 2) * 2];
                    bits[1] = MP3_SCF_BITS5[(idx >> 2) * 2 + 1];
                    bits[2] = (idx & 3) as u8;
                    bits[3] = 0;
                    1
                },
                _ => {
                    let idx = self.scalefac_compress - 500;
                    bits[0] = MP3_SCF_BITS3[idx * 2];
                    bits[1] = MP3_SCF_BITS3[idx * 2 + 1];
                    bits[2] = 0;
                    bits[3] = 0;
                    2
                },
            }
        } else {
            bits[3] = 0;
            let idx = self.scalefac_compress >> 1;
            match idx {
                0..=179 => {
                    bits[0] = MP3_SCF_BITS6[idx * 3];
                    bits[1] = MP3_SCF_BITS6[idx * 3 + 1];
                    bits[2] = MP3_SCF_BITS6[idx * 3 + 2];
                    3
                },
                180..=243 => {
                    let val = (idx - 180) as u8;
                    bits[0] =  val >> 4;
                    bits[1] = (val >> 2) & 3;
                    bits[2] =  val       & 3;
                    4
                },
                _ => {
                    let idx = idx - 244;
                    bits[0] = MP3_SCF_BITS6[idx * 2];
                    bits[1] = MP3_SCF_BITS6[idx * 2 + 1];
                    bits[2] = 0;
                    5
                },
            }
        }
    }
}

struct MDCTContext {
    win36:      [f32; 36],
    win36f:     [f32; 36],
    win12:      [f32; 12],
    win12f:     [f32; 12],
    tmp:        [f32; 36],
}

impl MDCTContext {
    fn new() -> Self {
        let mut win36  = [0.0; 36];
        let mut win36f = [0.0; 36];
        let mut win12  = [0.0; 12];
        let mut win12f = [0.0; 12];

        for i in 0..36 {
            win36 [i] = ((i as f32 + 0.5) * std::f32::consts::PI / 36.0).sin();
            win36f[i] = if (i & 1) == 0 { win36[i] } else { -win36[i] };
        }
        for i in 0..12 {
            win12 [i] = ((i as f32 + 0.5) * std::f32::consts::PI / 12.0).sin();
            win12f[i] = if (i & 1) == 0 { win12[i] } else { -win12[i] };
        }

        Self {
            tmp: [0.0; 36],
            win36, win36f, win12, win12f
        }
    }
    fn mdct36(&mut self, src: &mut [f32], dst: &mut[f32], delay: &mut [f32], len: usize, block_type: u8) {
        let mut flip = false;
        for i in (0..len).step_by(18) {
            let (win36, win12) = if flip { (&self.win36f, &self.win12f) } else { (&self.win36, &self.win12) };
            dct36(&mut src[i..], &mut self.tmp);
            match block_type {
                0 | 2 => {
                    for j in 0..9 {
                        dst[i + j] = delay[i + j] - self.tmp[8 - j] * win36[j];
                        delay[i + j] = self.tmp[j + 9] * win36[j + 18];
                    }
                    for j in 9..18 {
                        dst[i + j] = delay[i + j] + self.tmp[j - 9] * win36[j];
                        delay[i + j] = self.tmp[26 - j] * win36[j + 18];
                    }
                },
                1 => {
                    for j in 0..9 {
                        dst[i + j] = delay[i + j] - self.tmp[8 - j] * win36[j];
                    }
                    for j in 9..18 {
                        dst[i + j] = delay[i + j] + self.tmp[j - 9] * win36[j];
                    }
                    delay[i..][..6].copy_from_slice(&self.tmp[9..][..6]);
                    if flip {
                        for j in (1..6).step_by(2) {
                            delay[i + j] = -delay[i + j];
                        }
                    }
                    for j in 6..9 {
                        delay[i + j] = self.tmp[j + 9] * win12[j];
                    }
                    for j in 9..12 {
                        delay[i + j] = self.tmp[26 - j] * win12[j];
                    }
                    for j in 12..18 {
                        delay[i + j] = 0.0;
                    }
                },
                _ => {
                    dst[i..][..6].copy_from_slice(&delay[i..][..6]);
                    for j in 6..9 {
                        dst[i + j] = delay[i + j] - self.tmp[8 - j] * win12[j - 6];
                    }
                    for j in 9..12 {
                        dst[i + j] = delay[i + j] + self.tmp[j - 9] * win12[j - 6];
                    }
                    if !flip {
                        for j in 12..18 {
                            dst[i + j] = delay[i + j] + self.tmp[j - 9];
                        }
                    } else {
                        for j in 12..18 {
                            dst[i + j] = delay[i + j] + if (j & 1) == 0 { self.tmp[j - 9] } else { -self.tmp[j - 9] };
                        }
                    }
                    for j in 0..9 {
                        delay[i + j] = self.tmp[j + 9] * win36[j + 18];
                    }
                    for j in 9..18 {
                        delay[i + j] = self.tmp[26 - j] * win36[j + 18];
                    }
                },
            };

            flip = !flip;
        }
    }
    fn mdct12(&mut self, src: &[f32], dst: &mut[f32], delay: &mut [f32], len: usize) {
        let mut flip = false;
        for i in (0..len).step_by(18) {
            let window = if flip { &self.win12f } else { &self.win12 };
            for j in 0..3 {
                let tmp = &mut self.tmp[j * 12..];
                dct12(&src[i + j..], tmp);
                for (el, &w) in tmp.iter_mut().zip(window.iter()) {
                    *el *= w;
                }
            }

            for j in 0..6 {
                dst[i + j] = delay[i + j];
                delay[i + j] = self.tmp[j + 18] + self.tmp[j + 18 + 6];
            }
            for j in 6..12 {
                dst[i + j] = delay[i + j] + self.tmp[j - 6];
                delay[i + j] = self.tmp[j + 18 + 6];
            }
            for j in 12..18 {
                dst[i + j] = delay[i + j] + self.tmp[j - 6] + self.tmp[j];
                delay[i + j] = 0.0;
            }

            flip = !flip;
        }
    }
}

pub struct MP3Data {
        cb:             MP3Codebooks,
        mdct:           MDCTContext,
        granule:        [[Granule; 2]; 2],
        scfsi:          [[bool; 4]; 2],
    pub main_data_end:  usize,
        is_mode:        [i8; SAMPLES],
    pub mpeg1:          bool,
    pub sf_idx:         usize,

        delay:          [[f32; SAMPLES / 2]; 2],
        tmp:            [f32; SAMPLES / 2],
}

impl MP3Data {
    pub fn new() -> Self {
        Self {
            cb:             MP3Codebooks::new(),
            mdct:           MDCTContext::new(),
            granule:        [[Granule::default(); 2]; 2],
            scfsi:          [[false; 4]; 2],
            main_data_end:  0,
            is_mode:        [0; SAMPLES],
            mpeg1:          false,
            sf_idx:         0,

            delay:          [[0.0; SAMPLES / 2]; 2],
            tmp:            [0.0; SAMPLES / 2],
        }
    }
    pub fn reset(&mut self) {
        for dly in self.delay.iter_mut() {
            for el in dly.iter_mut() {
                *el = 0.0;
            }
        }
    }
    fn calc_scale(gr: &Granule, sb: usize, ssb: usize, sblk_gain: u8) -> i8 {
        (i32::from(gr.global_gain) - 64 - 146
            - 8 * i32::from(sblk_gain)
            - if gr.scalefac_scale { 4 } else { 2 } * (i32::from(gr.scalefac[ssb])
                + if gr.preflag { i32::from(MP3_PREEMP_SCALES[sb]) } else { 0 })
        ).min(127).max(-124) as i8
    }
    fn read_mp3_coeffs(&mut self, br: &mut BitReader, end: usize, gr_no: usize, ch: usize, coeffs: &mut [f32]) -> DecoderResult<()> {
        let mut scales = [0; SAMPLES / 2];
        let gr = &mut self.granule[gr_no][ch];

        // calculate scales first
        if gr.block_type != 2 {
            let mut off = 0;
            let mut sb = 0;
            while off < SAMPLES / 2 {
                let end = MP3_SFB_LONG_OFFS[self.sf_idx][sb + 1];
                let scale = Self::calc_scale(gr, sb, sb, 0);
                for el in scales[off..end].iter_mut() {
                    *el = scale;
                }

                if ch == 1 {
                    let scf = gr.scalefac[sb.min(MP3_BANDS - 1)];
                    if scf != gr.istereo[sb] {
                        for el in self.is_mode[gr_no * SAMPLES/2..][off..end].iter_mut() {
                            *el = scf as i8;
                        }
                    }
                }

                sb += 1;
                off = end;
            }
        } else {
            let end_band = if self.mpeg1 { 8 } else { 6 };

            let mut off = 0;
            let mut sb = 0;
            if gr.switch_point {
                while sb < end_band {
                    let end = MP3_SFB_LONG_OFFS[self.sf_idx][sb + 1];
                    let scale = Self::calc_scale(gr, sb, sb, 0);
                    for el in scales[off..end].iter_mut() {
                        *el = scale;
                    }

                    if ch == 1 {
                        let scf = gr.scalefac[sb.min(MP3_BANDS - 1)];
                        if scf != gr.istereo[sb] {
                            for el in self.is_mode[gr_no * SAMPLES/2..][off..end].iter_mut() {
                                *el = scf as i8;
                            }
                        }
                    }
                    sb += 1;
                    off = end;
                }
            }
            let mut ssb = if gr.switch_point { 8 } else { 0 };
            let mut sb = if gr.switch_point { 3 } else { 0 };
            while sb <= MP3_BANDS_SHORT {
                let band_size = MP3_SFB_SHORT_SIZE[self.sf_idx][sb];
                for win in 0..3 {
                    let scale = Self::calc_scale(gr, sb, ssb, gr.subblock_gain[win]);
                    for el in scales[off..][..band_size].iter_mut() {
                        *el = scale;
                    }

                    if ch == 1 {
                        if sb == MP3_BANDS_SHORT {
                            gr.scalefac[ssb] = gr.scalefac[ssb - 3];
                            gr.istereo[ssb]  = gr.istereo[ssb - 3];
                        }
                        let scf = gr.scalefac[ssb];
                        if scf != gr.istereo[ssb] {
                            for el in self.is_mode[gr_no * SAMPLES/2 + off..][..band_size].iter_mut() {
                                *el = scf as i8;
                            }
                        }
                    }
                    off += band_size;
                    ssb += 1;
                }
                sb += 1;
            }
        }

        // prepare for coefficients decoding
        let region1_start = if gr.block_type != 2 {
                MP3_SFB_LONG_OFFS[self.sf_idx][gr.region_address1 as usize + 1]
            } else if gr.switch_point {
                36
            } else {
                MP3_SFB_SHORT_OFFS[self.sf_idx][3] * 3
            }.min(gr.big_values);
        let region2_start = MP3_SFB_LONG_OFFS[self.sf_idx][((gr.region_address1 + gr.region_address2 + 2) as usize).min(MP3_BANDS + 1)].min(gr.big_values);

        for el in coeffs[..SAMPLES/2].iter_mut() {
            *el = 0.0;
        }

        // read coefficients
        gr.lastcoded = 0;
        if let Some((cb, esc_bits)) = self.cb.get_cb(gr.table_select[0]) {
            let lc = read_region(br, end, coeffs, &scales, 0, region1_start, cb, esc_bits)?;
            gr.lastcoded = gr.lastcoded.max(lc);
        }
        if let Some((cb, esc_bits)) = self.cb.get_cb(gr.table_select[1]) {
            let lc = read_region(br, end, coeffs, &scales, region1_start, region2_start, cb, esc_bits)?;
            gr.lastcoded = gr.lastcoded.max(lc);
        }
        if let Some((cb, esc_bits)) = self.cb.get_cb(gr.table_select[2]) {
            let lc = read_region(br, end, coeffs, &scales, region2_start, gr.big_values, cb, esc_bits)?;
            gr.lastcoded = gr.lastcoded.max(lc);
        }
        let (lc, zp) = read_region_quad(br, end, coeffs, &scales, gr.big_values, if !gr.count1table_select { Some(&self.cb.quad_cb) } else { None })?;
        gr.lastcoded = gr.lastcoded.max(lc);
        gr.zero_part = if zp > 0 { zp } else { gr.lastcoded };

        Ok(())
    }
    pub fn read_mp3_side_data(&mut self, br: &mut BitReader, channels: usize) -> DecoderResult<()> {
        if self.mpeg1 {
            self.main_data_end          = br.read(9)? as usize;
            let _private_bits           = br.read(if channels == 1 { 5 } else { 3 })?;
            for scfsis in self.scfsi[..channels].iter_mut() {
                for scfsi in scfsis.iter_mut() {
                    *scfsi              = br.read_bool()?;
                }
            }
        } else {
            self.main_data_end          = br.read(8)? as usize;
            let _private_bits           = br.read(channels as u8)?;
        }
        let granules = if self.mpeg1 { 2 } else { 1 };
        for grans in self.granule[..granules].iter_mut() {
            for gr in grans[..channels].iter_mut() {
                gr.part2_3_length       = br.read(12)? as usize;
                gr.big_values           = (br.read(9)? as usize) * 2;
                gr.global_gain          = br.read(8)? as u8;
                gr.scalefac_compress    = br.read(if self.mpeg1 { 4 } else { 9 })? as usize;
                gr.blocksplit           = br.read_bool()?;
                if gr.blocksplit {
                    gr.block_type       = br.read(2)? as u8;
                    gr.switch_point     = br.read_bool()?;
                    for tsel in gr.table_select[..2].iter_mut() {
                        *tsel           = br.read(5)? as u8;
                        match *tsel {
                            4 | 14 => return Err(DecoderError::InvalidData),
                            _ => {},
                        };
                    }
                    for gain in gr.subblock_gain.iter_mut() {
                        *gain           = br.read(3)? as u8;
                    }
                    gr.region_address1 = 7;
                    gr.region_address2 = 13;
                } else {
                    gr.block_type = 0;
                    gr.switch_point = false;
                    for tsel in gr.table_select.iter_mut() {
                        *tsel           = br.read(5)? as u8;
                        match *tsel {
                            4 | 14 => return Err(DecoderError::InvalidData),
                            _ => {},
                        };
                    }
                    gr.region_address1  = br.read(4)? as u8;
                    gr.region_address2  = br.read(3)? as u8;
                }
                if self.mpeg1 {
                    gr.preflag          = br.read_bool()?;
                } else {
                    gr.preflag = false;
                }
                gr.scalefac_scale       = br.read_bool()?;
                gr.count1table_select   = br.read_bool()?
            }
        }
        Ok(())
    }
    pub fn decode_mpeg1_layer3(&mut self, br: &mut BitReader, coeffs: &mut [[f32; SAMPLES]; 2], channels: usize) -> DecoderResult<()> {
        let mut data_end = 0;
        for gr_no in 0..2 {
            for ch in 0..channels {
                data_end += self.granule[gr_no][ch].part2_3_length;

                if self.granule[gr_no][ch].block_type != 2 {
                    if gr_no != 0 {
                        self.granule[1][ch].scalefac = self.granule[0][ch].scalefac;
                    } else {
                        for scf in self.granule[gr_no][ch].scalefac.iter_mut() {
                            *scf = 0;
                        }
                    }

                    let gr = &mut self.granule[gr_no][ch];
                    let bits1 = MP3_SCALEFAC_BITS1[gr.scalefac_compress];
                    let bits2 = MP3_SCALEFAC_BITS2[gr.scalefac_compress];
                    for cb in 0..11 {
                        if !self.scfsi[ch][SCFSI_FROM_BAND[cb]] || (gr_no == 0) {
                            gr.scalefac[cb]     = br.read(bits1)? as u8;
                        }
                    }
                    for cb in 11..MP3_BANDS {
                        if !self.scfsi[ch][SCFSI_FROM_BAND[cb]] || (gr_no == 0) {
                            gr.scalefac[cb]     = br.read(bits2)? as u8;
                        }
                    }
                    for scf in gr.scalefac[MP3_BANDS..].iter_mut() {
                        *scf = 0;
                    }
                    for is in gr.istereo.iter_mut() {
                        *is = 7;
                    }
                } else {
                    let gr = &mut self.granule[gr_no][ch];
                    let bits1 = MP3_SCALEFAC_BITS1[gr.scalefac_compress];
                    let bits2 = MP3_SCALEFAC_BITS2[gr.scalefac_compress];
                    let pivot = if gr.blocksplit && gr.switch_point { 17 } else { 18 };

                    for scf in gr.scalefac[..pivot].iter_mut() {
                        *scf        = br.read(bits1)? as u8;
                    }
                    for scf in gr.scalefac[pivot..][..18].iter_mut() {
                        *scf        = br.read(bits2)? as u8;
                    }
                    for scf in gr.scalefac[pivot + 18..].iter_mut() {
                        *scf = 0;
                    }
                    for is in gr.istereo.iter_mut() {
                        *is = 7;
                    }
                }
                self.read_mp3_coeffs(br, data_end, gr_no, ch, &mut coeffs[ch][gr_no * SAMPLES/2..])?;
                validate!(br.tell() <= data_end);
                br.seek(data_end as u32)?;
            }
        }
        Ok(())
    }
    pub fn decode_mpeg2_layer3(&mut self, br: &mut BitReader, coeffs: &mut [[f32; SAMPLES]; 2], channels: usize, mode_ext: u8) -> DecoderResult<()> {
        let mut data_end = 0;
        for ch in 0..channels {
            let gr = &mut self.granule[0][ch];
            data_end += gr.part2_3_length;

            let mut bits = [0; 4];
            let idx = gr.get_mpeg2_params(&mut bits, (ch == 0) || ((mode_ext & IS_MODE) == 0));
            let idx2 = if gr.block_type != 2 { 0 } else if !gr.switch_point { 1 } else { 2 };

            gr.preflag = idx == 2;
            let ends = &MP3_SCF_ENDS[idx][idx2];

            for (scf, is) in gr.scalefac[0..ends[0]].iter_mut().zip(gr.istereo[0..ends[0]].iter_mut()) {
                *scf                    = br.read(bits[0])? as u8;
                *is = (1 << bits[0]) - 1;
            }
            for (scf, is) in gr.scalefac[ends[0]..ends[1]].iter_mut().zip(gr.istereo[ends[0]..ends[1]].iter_mut()) {
                *scf                    = br.read(bits[1])? as u8;
                *is = (1 << bits[1]) - 1;
            }
            for (scf, is) in gr.scalefac[ends[1]..ends[2]].iter_mut().zip(gr.istereo[ends[1]..ends[2]].iter_mut()) {
                *scf                    = br.read(bits[2])? as u8;
                *is = (1 << bits[2]) - 1;
            }
            for (scf, is) in gr.scalefac[ends[2]..ends[3]].iter_mut().zip(gr.istereo[ends[2]..ends[3]].iter_mut()) {
                *scf                    = br.read(bits[3])? as u8;
                *is = (1 << bits[3]) - 1;
            }
            self.read_mp3_coeffs(br, data_end, 0, ch, &mut coeffs[ch])?;
            validate!(br.tell() <= data_end);
            br.seek(data_end as u32)?;
        }
        Ok(())
    }
    pub fn synth(&mut self, coeffs: &mut [[f32; SAMPLES]; 2], output: &mut [[[f32; 32]; 36]; 2], mode: u8, mode_ext: u8) {
        let channels = if mode == 3 { 1 } else { 2 };
        let granules = if self.mpeg1 { 2 } else { 1 };

        let mut end_freq = [[0; 2]; 2];
        for gr_no in 0..granules {
            for ch in 0..channels {
                end_freq[gr_no][ch] = self.granule[gr_no][ch].lastcoded;
            }
        }

        let mut band_flags = [[0; MP3_MAX_BANDS + 3]; 2];
        let mut band_start = [[0; MP3_MAX_BANDS + 3]; 2];
        let mut band_end   = [[0; MP3_MAX_BANDS + 3]; 2];

        if mode == 1 { // joint stereo
            let mut bound_band = [0; 2];
            let mut num_bands = [MP3_BANDS + 1; 2];

            for (gr_no, grans) in self.granule[..granules].iter_mut().enumerate() {
                if grans[1].block_type != 2 {
                    for band in 0..=MP3_BANDS {
                        band_flags[gr_no][band] = mode_ext;
                        band_start[gr_no][band] = MP3_SFB_LONG_OFFS[self.sf_idx][band];
                        band_end  [gr_no][band] = MP3_SFB_LONG_OFFS[self.sf_idx][band + 1];
                        if (end_freq[gr_no][1] >= band_end[gr_no][band]) || (grans[1].scalefac[band.min(MP3_BANDS - 1)] == grans[1].istereo[band.min(MP3_BANDS - 1)]) {
                            band_flags[gr_no][band] &= !IS_MODE;
                        }
                        if band_start[gr_no][band] < end_freq[gr_no][1] {
                            bound_band[gr_no] = band;
                        }
                        if (band_flags[gr_no][band] & IS_MODE) == 0 {
                            for el in self.is_mode[gr_no * SAMPLES/2..][band_start[gr_no][band]..band_end[gr_no][band]].iter_mut() {
                                *el = -1;
                            }
                        }
                    }
                } else {
                    let switch_off = if grans[1].switch_point { 3 } else { 0 };
                    let mut start = 0;
                    let mut band = 0;
                    if grans[1].switch_point {
                        let long_bands = if self.mpeg1 { 8 } else { 6 };
                        for _ in 0..long_bands {
                            band_flags[gr_no][band] = mode_ext;
                            band_start[gr_no][band] = MP3_SFB_LONG_OFFS[self.sf_idx][band];
                            band_end  [gr_no][band] = MP3_SFB_LONG_OFFS[self.sf_idx][band + 1];
                            if end_freq[gr_no][1] >= band_end[gr_no][band] {
                                band_flags[gr_no][band] &= !IS_MODE;
                            }
                            start = band_end[gr_no][band];
                            band += 1;
                        }
                    }
                    for sb in switch_off..=MP3_BANDS_SHORT {
                        let band_size = MP3_SFB_SHORT_SIZE[self.sf_idx][sb];
                        for _win in 0..3 {
                            band_flags[gr_no][band] = mode_ext;
                            band_start[gr_no][band] = start;
                            band_end  [gr_no][band] = start + band_size;
                            if end_freq[gr_no][1] >= band_end[gr_no][band] {
                                band_flags[gr_no][band] &= !IS_MODE;
                            }
                            start += band_size;
                            band += 1;
                        }
                    }
                    num_bands[gr_no] = band;
                }
            }
            if (mode_ext & IS_MODE) != 0 {
                for (gr_no, grans) in self.granule[..granules].iter_mut().enumerate() {
                    let (coef0, coef1) = coeffs.split_at_mut(1);
                    let coef0 = &mut coef0[0][gr_no * SAMPLES/2..];
                    let coef1 = &mut coef1[0][gr_no * SAMPLES/2..];
                    let is_mode = &self.is_mode[gr_no * SAMPLES/2..];
                    let start = band_end[gr_no][bound_band[gr_no]];
                    let end = grans[0].zero_part.max(start);

                    if self.mpeg1 {
                        let coef0 = &mut coef0[start..end];
                        let coef1 = &mut coef1[start..end];
                        let is_mode = &self.is_mode[gr_no * SAMPLES/2..][start..end];
                        for ((l, r), &is) in coef0.iter_mut().zip(coef1.iter_mut()).zip(is_mode.iter()) {
                            if is >= 0 && is < 7 {
                                let t = *l * MP3_ISTEREO_COEFFS[is as usize];
                                *l -= t;
                                *r  = t;
                            }
                        }
                    } else {
                        let iscale = (grans[1].scalefac_compress & 1) as u8;
                        for band in 0..num_bands[gr_no] {
                            if (band_flags[gr_no][band] & IS_MODE) != 0 {
                                let start = band_start[gr_no][band];
                                let end   = band_end[gr_no][band];
                                apply_istereo(&mut coef0[start..end], &mut coef1[start..end], is_mode[start], iscale, (band_flags[gr_no][band] & MS_MODE) != 0);
                            }
                        }
                    }
                    end_freq[gr_no][1] = end_freq[gr_no][1].max(end);
                }
            }
            if (mode_ext & MS_MODE) != 0 {
                for (gr_no, grans) in self.granule[..granules].iter_mut().enumerate() {
                    let end = grans[0].zero_part.max(grans[1].zero_part);
                    let (coef0, coef1) = coeffs.split_at_mut(1);
                    let coef0 = &mut coef0[0][gr_no * SAMPLES/2..];
                    let coef1 = &mut coef1[0][gr_no * SAMPLES/2..];
                    for band in 0..num_bands[gr_no] {
                        if band_start[gr_no][band] >= end {
                            break;
                        }
                        if band_flags[gr_no][band] == MS_MODE {
                            let start = band_start[gr_no][band];
                            let end   = band_end[gr_no][band];
                            super::apply_ms(&mut coef0[start..end], &mut coef1[start..end]);
                        }
                    }
                    end_freq[gr_no][0] = end;
                    end_freq[gr_no][1] = end;
                }
            }
        }
        for (gr_no, grans) in self.granule[..granules].iter_mut().enumerate() {
            for (ch, gr) in grans[..channels].iter_mut().enumerate() {
                let src = &mut coeffs[ch][gr_no * SAMPLES/2..][..SAMPLES/2];
                if gr.block_type != 2 {
                    dealias(src, SAMPLES/2);
                    self.mdct.mdct36(src, &mut self.tmp, &mut self.delay[ch], end_freq[gr_no][ch], gr.block_type);
                } else {
                    let switch_off = if gr.switch_point { MP3_SFB_LONG_OFFS[self.sf_idx][if self.mpeg1 { 8 } else { 6 }] } else { 0 };
                    let mut band_buf = [0.0; 66 * 3];
                    let mut sb = if gr.switch_point { 3 } else { 0 };
                    let mut off = switch_off;

                    while sb <= MP3_BANDS_SHORT {
                        let band_size = MP3_SFB_SHORT_SIZE[self.sf_idx][sb];
                        for win in 0..3 {
                            for i in 0..band_size {
                                band_buf[win + i * 3] = src[off + win * band_size + i];
                            }
                        }
                        src[off..][..band_size * 3].copy_from_slice(&band_buf[..band_size * 3]);
                        off += band_size * 3;
                        sb += 1;
                    }
                    if gr.switch_point {
                        dealias(src, switch_off);
                        self.mdct.mdct36(src, &mut self.tmp, &mut self.delay[ch], switch_off, gr.block_type);
                    }
                    self.mdct.mdct12(&src[switch_off..], &mut self.tmp[switch_off..], &mut self.delay[ch][switch_off..], end_freq[gr_no][ch] - switch_off);
                }

                let dst = &mut output[ch][gr_no * 18..];
                let end = (end_freq[gr_no][ch] + 17) / 18;
                for i in 0..end {
                    for j in 0..18 {
                        dst[j][i] = self.tmp[i * 18 + j];
                    }
                }
                for i in end..32 {
                    for j in 0..18 {
                        dst[j][i] = self.delay[ch][i * 18 + j];
                    }
                    for el in self.delay[ch][i * 18..][..18].iter_mut() {
                        *el = 0.0;
                    }
                }
            }
        }
    }
}

fn mp3_unquant(val: u32, scale: i8) -> f32 {
    (val as f32) * (val as f32).cbrt() * 2.0f32.powf((scale as f32) * 0.25)
}

#[allow(clippy::too_many_arguments)]
fn read_region(br: &mut BitReader, br_end: usize, coeffs: &mut [f32], scales: &[i8; SAMPLES/2], start: usize, end: usize, cb: &Codebook<u8>, esc_bits: u8) -> DecoderResult<usize> {
    let mut lastcoded = 0;
    for (i, (cpair, scpair)) in coeffs[start..end].chunks_exact_mut(2).zip(scales[start..end].chunks_exact(2)).enumerate() {
        if br.tell() >= br_end { break; }
        let val                         = br.read_cb(cb)?;
        if val == 0 {
            continue;
        }
        let a = if (val >> 4) != 0xF || esc_bits == 0 {
                u32::from(val >> 4)
            } else {
                                          br.read(esc_bits)? + 15
            };
        if a != 0 {
            let a = mp3_unquant(a, scpair[0]);
            cpair[0] = if br.read_bool()? { -a } else { a };
        }
        let b = if (val & 0xF) != 0xF || esc_bits == 0 {
                u32::from(val & 0xF)
            } else {
                                          br.read(esc_bits)? + 15
            };
        if b != 0 {
            let b = mp3_unquant(b, scpair[1]);
            cpair[1] = if br.read_bool()? { -b } else { b };
        }
        lastcoded = start + (i + 1) * 2;
    }
    Ok(lastcoded)
}

fn read_region_quad(br: &mut BitReader, br_end: usize, coeffs: &mut [f32], scales: &[i8; SAMPLES/2], start: usize, cb: Option<&Codebook<u8>>) -> DecoderResult<(usize, usize)> {
    let mut lastcoded = 0;
    let mut zero_part = 0;
    if br.tell() >= br_end {
        return Ok((0, 0));
    }
    for (i, (cquad, scquad)) in coeffs[start..SAMPLES/2].chunks_exact_mut(4).zip(scales[start..].chunks_exact(4)).enumerate() {
        zero_part = start + i * 4 + 4;
        if br.tell() >= br_end || (br.tell() + 3 >= br_end && cb.is_none()) {
            break;
        }
        let val = if let Some(cbook) = cb { br.read_cb(cbook)? } else { (br.read(4)? as u8) ^ 0xF };
        if val == 0 {
            continue;
        }
        for j in 0..4 {
            if ((val >> (3 - j)) & 1) != 0 {
                cquad[j] = mp3_unquant(1, scquad[j]);
                if br.read_bool()? {
                    cquad[j] = -cquad[j];
                }
                lastcoded = start + i * 4 + j + 1;
            }
        }
    }
    Ok((lastcoded, zero_part))
}

const DCT12_0: f32 = -0.92387953251128675613;
const DCT12_1: f32 = -0.38268343236508977174;

const DCT12_2: f32 = -0.1305261922200516;
const DCT12_3: f32 = -0.6087614290087205;
const DCT12_4: f32 =  0.7933533402912348;
const DCT12_5: f32 =  0.9914448613738103;

fn dct12(src: &[f32], dst: &mut [f32]) {
    let t0 = src[0] - src[9] - src[12];
    let t1 = src[3] - src[6] - src[15];

    dst[ 4] = t0 * DCT12_1 - t1 * DCT12_0;
    dst[ 7] = t0 * DCT12_0 + t1 * DCT12_1;
    dst[ 1] = -dst[4];
    dst[10] =  dst[7];

    let t0 = src[3] * DCT12_1 - src[12] * DCT12_0;
    let t1 = src[3] * DCT12_0 + src[12] * DCT12_1;

    dst[ 3] =  src[0] * DCT12_2 + src[6] * DCT12_3 + src[9] * DCT12_4 + src[15] * DCT12_5 - t0;
    dst[ 5] =  src[0] * DCT12_3 - src[6] * DCT12_2 - src[9] * DCT12_5 + src[15] * DCT12_4 - t1;
    dst[ 6] = -src[0] * DCT12_4 + src[6] * DCT12_5 - src[9] * DCT12_2 + src[15] * DCT12_3 - t0;
    dst[ 8] = -src[0] * DCT12_5 - src[6] * DCT12_4 + src[9] * DCT12_3 + src[15] * DCT12_2 + t1;
    dst[ 2] = -dst[3];
    dst[ 0] = -dst[5];
    dst[11] =  dst[6];
    dst[ 9] =  dst[8];
}

const DCT36_PRESCALE: [f32; 18] = [
    0.99904822158185776240, 0.99144486137381041114, 0.97629600711993336597,
    0.95371695074822692114, 0.92387953251128675613, 0.88701083317822170105,
    0.84339144581288570127, 0.79335334029123516458, 0.73727733681012404139,
    0.67559020761566024435, 0.60876142900872063942, 0.53729960834682383185,
    0.46174861323503393057, 0.38268343236508977174, 0.30070579950427312163,
    0.21643961393810287977, 0.13052619222005159156, 0.04361938736533599979
];
const DCT36_TWIDDLE: [f32; 9] = [
    0.99619469809174553229, 0.96592582628906828675, 0.90630778703664996324,
    0.81915204428899178969, 0.70710678118654752440, 0.57357643635104609611,
    0.42261826174069943619, 0.25881904510252076236, 0.08715574274765817357
];
const SDCT2_TWIDDLE: [f32; 7] = [
    -2.0 * 0.98480775301220805936, -2.0 * 0.86602540378443864676,
    -2.0 * 0.76604444311897803520, -2.0 * 0.64278760968653932633,
    -2.0 * 0.34202014332566873305, -2.0 * 0.17364817766693034887,
     2.0 * 0.93969262078590838404
];

fn sdct_ii(buf: &mut [f32]) {
    let t0 = buf[ 6] + buf[10];
    let t1 = buf[ 6] - buf[10];
    let t2 = buf[12] + buf[ 4];
    let t3 = buf[12] - buf[ 4];
    let t4 = buf[16] + buf[ 0];
    let t5 = buf[16] - buf[ 0];

    let t6 =  t0 + t2;
    let t7 = (t0 - t2) * SDCT2_TWIDDLE[2];
    let t8 = (t1 + t3) * SDCT2_TWIDDLE[3];
    let t9 =  t1 - t3;
    let t0 = (t0 - t4) * SDCT2_TWIDDLE[5];
    let t1 = (t1 - t5) * SDCT2_TWIDDLE[0];
    let t2 = (t2 - t4) * SDCT2_TWIDDLE[6];
    let t3 = (t3 + t5) * SDCT2_TWIDDLE[4];

    let ta =  t6 + t4;
    let tb = (t9 + t5) * SDCT2_TWIDDLE[1];

    let tc =  buf[2] + buf[14];
    let td = (buf[2] - buf[14]) * SDCT2_TWIDDLE[1];

    let t6 = buf[8]       + tc;
    let t9 = buf[8] * 2.0 - tc;
    let te = t9 + t2;
    let tf = t9 - t2;
    let t9 = t9 + t0;

    buf[ 0] = ta + t6;
    buf[ 2] = t8 - td - t1;
    buf[ 4] = t7 - te;
    buf[ 6] = tb;
    buf[ 8] = tf - t0;
    buf[10] = td - t3 - t1;
    buf[12] = ta - t6 * 2.0;
    buf[14] = t8 + t3 + td;
    buf[16] = t9 + t7;
}

fn dct36(src: &mut [f32], dst: &mut [f32]) {
    for (el, &w) in src.iter_mut().zip(DCT36_PRESCALE.iter()) {
        *el *= w;
    }

    let mut tmp = [0.0; 18];
    for i in 0..9 {
        tmp[i * 2]     =  src[i] + src[17 - i];
        tmp[i * 2 + 1] = (src[i] - src[17 - i]) * DCT36_TWIDDLE[i] * 2.0;
    }
    sdct_ii(&mut tmp);
    sdct_ii(&mut tmp[1..]);

    for i in (3..18).step_by(2) {
        tmp[i] -= tmp[i - 2];
    }
    tmp[0] *= 0.5;
    for i in 1..18 {
        tmp[i] = tmp[i] * 0.5 - tmp[i - 1];
    }
    for i in 0..18 {
        dst[i] = -tmp[17 - i] * 2.0;
    }
}

const ALIAS_COEFFS_S: [f32; 8] = [
    0.85749292571254418689, 0.88174199731770518178,
    0.94962864910273289205, 0.98331459249179014599,
    0.99551781606758576429, 0.99916055817814750453,
    0.99989919524444704627, 0.99999315507028023572
];
const ALIAS_COEFFS_A: [f32; 8] = [
    -0.51449575542752651213, -0.47173196856497227225,
    -0.31337745420390185437, -0.18191319961098117700,
    -0.09457419252642064760, -0.04096558288530404768,
    -0.01419856857247114805, -0.00369997467376003687
];
fn dealias(buf: &mut [f32], len: usize) {
    for i in (18..len).step_by(18) {
        for (j, (&cs, &ca)) in ALIAS_COEFFS_S.iter().zip(ALIAS_COEFFS_A.iter()).enumerate() {
            let a = buf[i - j - 1];
            let b = buf[i + j];
            let c0 = a * cs - b * ca;
            let c1 = a * ca + b * cs;
            buf[i - j - 1] = c0;
            buf[i + j]     = c1;
        }
    }
}

fn apply_istereo(ch0: &mut [f32], ch1: &mut [f32], is_mode: i8, iscale: u8, ms_mode: bool) {
    match (is_mode, ms_mode) {
        (-1, true) => {
            for (c0, c1) in ch0.iter_mut().zip(ch1.iter_mut()) {
                let a = (*c0 + *c1) * std::f32::consts::FRAC_1_SQRT_2;
                let b = (*c0 - *c1) * std::f32::consts::FRAC_1_SQRT_2;
                *c0 = a;
                *c1 = b;
            }
        },
        (-1, false) => {},
        (0, _) => {
            ch1.copy_from_slice(ch0);
        },
        _ => {
            let scale = mp3_unquant(1, -((is_mode + 1) >> 1) << iscale);
            if (is_mode & 1) == 0 {
                for (&c0, c1) in ch0.iter().zip(ch1.iter_mut()) {
                    *c1 = c0 * scale;
                }
            } else {
                for (c0, c1) in ch0.iter_mut().zip(ch1.iter_mut()) {
                    *c1 = *c0;
                    *c0 *= scale;
                }
            }
        },
    };
}
