use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::io::bitreader::*;
use std::f32::consts;
use super::MAX_WINDOWS;

#[derive(Clone,Copy)]
pub struct LTPData {
}

impl LTPData {
    pub fn read(br: &mut BitReader) -> DecoderResult<Option<Self>> {
        let predictor_data_present                      = br.read_bool()?;
        if !predictor_data_present { return Ok(None); }
unimplemented!("predictor data");
/*
        if is_main {
            let predictor_reset                         = br.read_bool()?;
            if predictor_reset {
                let predictor_reset_group_number        = br.read(5)?;
            }
            for sfb in 0..max_sfb.min(PRED_SFB_MAX) {
                prediction_used[sfb]                    = br.read_bool()?;
            }
        } else {
            let ltp_data_present                        = br.read_bool()?;
            if ltp_data_present {
                //ltp data
            }
            if common_window {
                let ltp_data_present                    = br.read_bool()?;
                if ltp_data_present {
                    //ltp data
                }
            }
        }
        Ok(Some(Self { }))
*/
    }
}

#[derive(Clone,Copy)]
#[allow(dead_code)]
pub struct PulseData {
    pub number_pulse:       usize,
    pub pulse_start_sfb:    usize,
    pub pulse_offset:       [u8; 4],
    pub pulse_amp:          [u8; 4],
}

impl PulseData {
    pub fn read(br: &mut BitReader) -> DecoderResult<Option<Self>> {
        let pulse_data_present                          = br.read_bool()?;
        if !pulse_data_present { return Ok(None); }

        let number_pulse                                = (br.read(2)? as usize) + 1;
        let pulse_start_sfb                             = br.read(6)? as usize;
        let mut pulse_offset: [u8; 4] = [0; 4];
        let mut pulse_amp: [u8; 4] = [0; 4];
        for i in 0..number_pulse {
            pulse_offset[i]                             = br.read(5)? as u8;
            pulse_amp[i]                                = br.read(4)? as u8;
        }
        Ok(Some(Self{ number_pulse, pulse_start_sfb, pulse_offset, pulse_amp }))
    }
}

pub const TNS_MAX_ORDER: usize = 20;
const TNS_MAX_LONG_BANDS: [usize; 12] = [ 31, 31, 34, 40, 42, 51, 46, 46, 42, 42, 42, 39 ];
const TNS_MAX_SHORT_BANDS: [usize; 12] = [ 9, 9, 10, 14, 14, 14, 14, 14, 14, 14, 14, 14 ];

#[derive(Clone,Copy)]
pub struct TNSCoeffs {
    pub length:     usize,
    pub order:      usize,
    pub direction:  bool,
    pub compress:   bool,
    pub coef:       [f32; TNS_MAX_ORDER + 1],
}

impl TNSCoeffs {
    pub fn new() -> Self {
        Self {
            length: 0, order: 0, direction: false, compress: false, coef: [0.0; TNS_MAX_ORDER + 1],
        }
    }
    pub fn read(&mut self, br: &mut BitReader, long_win: bool, coef_res: bool, max_order: usize) -> DecoderResult<()> {
        self.length                                     = br.read(if long_win { 6 } else { 4 })? as usize;
        self.order                                      = br.read(if long_win { 5 } else { 3 })? as usize;
        validate!(self.order <= max_order);
        if self.order > 0 {
            self.direction                              = br.read_bool()?;
            self.compress                               = br.read_bool()?;
            let mut coef_bits = 3;
            if coef_res      { coef_bits += 1; }
            if self.compress { coef_bits -= 1; }
            let sign_mask = 1 << (coef_bits - 1);
            let neg_mask  = !(sign_mask * 2 - 1);

            let fac_base = if coef_res { 1 << 3 } else { 1 << 2 } as f32;
            let iqfac   = (fac_base - 0.5) / (consts::PI / 2.0);
            let iqfac_m = (fac_base + 0.5) / (consts::PI / 2.0);
            let mut tmp: [f32; TNS_MAX_ORDER] = [0.0; TNS_MAX_ORDER];
            for el in tmp.iter_mut().take(self.order) {
                let val                                 = br.read(coef_bits)? as i8;
                let c = f32::from(if (val & sign_mask) != 0 { val | neg_mask } else { val });
                *el = (if c >= 0.0 { c / iqfac } else { c / iqfac_m }).sin();
            }
            // convert to LPC coefficients
            let mut b: [f32; TNS_MAX_ORDER + 1] = [0.0; TNS_MAX_ORDER + 1];
            for m in 1..=self.order {
                for i in 1..m {
                    b[i] = self.coef[i - 1] + tmp[m - 1] * self.coef[m - i - 1];
                }
                for i in 1..m {
                    self.coef[i - 1] = b[i];
                }
                self.coef[m - 1] = tmp[m - 1];
            }
        }
        Ok(())
    }
}

#[derive(Clone,Copy)]
#[allow(dead_code)]
pub struct TNSData {
    pub n_filt: [usize; MAX_WINDOWS],
    coef_res:   [bool; MAX_WINDOWS],
    pub coeffs: [[TNSCoeffs; 4]; MAX_WINDOWS],
}

impl TNSData {
    pub fn read(br: &mut BitReader, long_win: bool, num_windows: usize, max_order: usize) -> DecoderResult<Option<Self>> {
        let tns_data_present                            = br.read_bool()?;
        if !tns_data_present { return Ok(None); }
        let mut n_filt: [usize; MAX_WINDOWS] = [0; MAX_WINDOWS];
        let mut coef_res: [bool; MAX_WINDOWS] = [false; MAX_WINDOWS];
        let mut coeffs: [[TNSCoeffs; 4]; MAX_WINDOWS] = [[TNSCoeffs::new(); 4]; MAX_WINDOWS];
        for w in 0..num_windows {
            n_filt[w]                                   = br.read(if long_win { 2 } else { 1 })? as usize;
            if n_filt[w] != 0 {
                coef_res[w]                             = br.read_bool()?;
            }
            for filt in 0..n_filt[w] {
                coeffs[w][filt].read(br, long_win, coef_res[w], max_order)?;
            }
        }
        Ok(Some(Self { n_filt, coef_res, coeffs }))
    }
    pub fn apply(&self, coeffs: &mut [f32; 1024], w: usize, f: usize, start: usize, end: usize) {
        let order = self.coeffs[w][f].order;
        let lpc = &self.coeffs[w][f].coef;
        let mut state = [0.0f32; 64];
        let mut sidx = 32;
        if !self.coeffs[w][f].direction {
            for m in start..end {
                for i in 0..order {
                    coeffs[m] -= state[(sidx + i) & 63] * lpc[i];
                }
                sidx = (sidx + 63) & 63;
                state[sidx] = coeffs[m];
            }
        } else {
            for m in (start..end).rev() {
                for i in 0..order {
                    coeffs[m] -= state[(sidx + i) & 63] * lpc[i];
                }
                sidx = (sidx + 63) & 63;
                state[sidx] = coeffs[m];
            }
        }
    }
    pub fn get_max_bands(long_win: bool, srate_idx: usize) -> usize {
        if long_win {
            TNS_MAX_LONG_BANDS[srate_idx]
        } else {
            TNS_MAX_SHORT_BANDS[srate_idx]
        }
    }
}

#[derive(Clone,Copy)]
#[allow(dead_code)]
pub struct GainControlData {
    max_band:       u8,
}

impl GainControlData {
    pub fn read(br: &mut BitReader) -> DecoderResult<Option<Self>> {
        let gain_control_data_present                   = br.read_bool()?;
        if !gain_control_data_present { return Ok(None); }
unimplemented!("gain control data");
/*        self.max_band                                   = br.read(2)? as u8;
        if window_sequence == ONLY_LONG_SEQUENCE {
            for bd in 0..max_band
...
        }
        Ok(Some(Self { }))*/
    }
}
