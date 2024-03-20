use nihav_core::io::byteio::*;
use nihav_core::codecs::EncoderResult;
use super::super::vpcommon::*;
use super::super::vp6data::*;
use super::models::{VP6HuffModels, VP6Huff};

#[derive(Default)]
pub struct HuffState {
    pub dc_zero_run:    [usize; 2],
    pub dc_zr_coded:    [bool; 2],
    pub ac_zero_run:    [usize; 2],
    pub ac_zr_coded:    [bool; 2],
}

impl HuffState {
    pub fn new() -> Self { Self::default() }
}

pub const MAX_EOB_RUN: usize = 63 + 10;

pub struct HuffEncoder<'a, 'b> {
    bw:         &'a mut ByteWriter<'b>,
    bitbuf:     u32,
    bits:       u8,
}

impl<'a, 'b> HuffEncoder<'a, 'b> {
    pub fn new(bw: &'a mut ByteWriter<'b>) -> Self {
        Self {
            bitbuf:     0,
            bits:       0,
            bw
        }
    }
    pub fn flush(mut self) -> EncoderResult<()> {
        while self.bits > 0 {
            self.bw.write_byte((self.bitbuf >> 24) as u8)?;
            self.bitbuf <<= 8;
            self.bits = self.bits.saturating_sub(8);
        }
        Ok(())
    }
    fn put_bits(&mut self, val: u16, bits: u8) -> EncoderResult<()> {
        self.bitbuf |= u32::from(val) << (32 - self.bits - bits);
        self.bits   += bits;
        while self.bits >= 8 {
            self.bw.write_byte((self.bitbuf >> 24) as u8)?;
            self.bitbuf <<= 8;
            self.bits    -= 8;
        }
        Ok(())
    }
    fn encode_eob(&mut self, mdl: &VP6Huff) -> EncoderResult<()> {
        self.put_bits(mdl.codes[11], mdl.bits[11])
    }
    fn encode_val(&mut self, val: i16, mdl: &VP6Huff) -> EncoderResult<()> {
        let idx = match val.abs() {
                      0 => 0,
                      1 => 1,
                      2 => 2,
                      3 => 3,
                      4 => 4,
                 5..= 6 => 5,
                 7..=10 => 6,
                11..=18 => 7,
                19..=34 => 8,
                35..=66 => 9,
                _ => 10,
            };
        self.put_bits(mdl.codes[idx], mdl.bits[idx])?;
        if idx >= 5 {
            self.put_bits((val.abs() - VP56_COEF_BASE[idx - 5]) as u16, VP6_COEF_ADD_BITS[idx - 5])?;
        }
        if idx > 0 {
            self.put_bits((val < 0) as u16, 1)?;
        }
        Ok(())
    }
    fn encode_eob_run(&mut self, val: usize) -> EncoderResult<()> {
        match val {
            0 => { self.put_bits(0, 2)?; },
            1 => { self.put_bits(1, 2)?; },
            2..=5 => {
                self.put_bits(2, 2)?;
                self.put_bits((val - 2) as u16, 2)?;
            },
            6..=9 => {
                self.put_bits(3, 2)?;
                self.put_bits(0, 1)?;
                self.put_bits((val - 6) as u16, 2)?;
            },
            _ => {
                self.put_bits(3, 2)?;
                self.put_bits(1, 1)?;
                self.put_bits((val - 10) as u16, 6)?;
            },
        };
        Ok(())
    }
    fn encode_zero_run(&mut self, val: usize, mdl: &VP6Huff) -> EncoderResult<()> {
        self.put_bits(mdl.codes[val.min(8)], mdl.bits[val.min(8)])?;
        if val >= 8 {
            self.put_bits((val - 8) as u16, 6)?;
        }
        Ok(())
    }
}

pub fn encode_block_huff(huff: &mut HuffEncoder, scan: &[usize; 64], coeffs: &[i16; 64], plane: usize, hstate: &mut HuffState, model: &VP6HuffModels) -> EncoderResult<()> {
    let mut last_idx = 64;
    for i in (0..64).rev() {
        if coeffs[scan[i]] != 0 {
            last_idx = i;
            break;
        }
    }

    if !hstate.dc_zr_coded[plane] {
        let mdl = &model.dc_token_tree[plane];
        huff.encode_val(coeffs[0], mdl)?;
        if coeffs[0] == 0 {
            huff.encode_eob_run(hstate.dc_zero_run[plane])?;
            hstate.dc_zr_coded[plane] = hstate.dc_zero_run[plane] > 0;
        }
    } else {
        hstate.dc_zero_run[plane] -= 1;
        if hstate.dc_zero_run[plane] == 0 {
            hstate.dc_zr_coded[plane] = false;
        }
    }
    if hstate.ac_zr_coded[plane] {
        hstate.ac_zero_run[plane] -= 1;
        if hstate.ac_zero_run[plane] == 0 {
            hstate.ac_zr_coded[plane] = false;
        }
        return Ok(());
    }

    let mut last_val = coeffs[0];

    if last_idx == 0 || last_idx == 64 {
        let ac_band = VP6_IDX_TO_AC_BAND[1].min(3);
        let ac_mode = last_val.abs().min(2) as usize;
        let mdl = &model.ac_token_tree[plane][ac_mode][ac_band];
        huff.encode_eob(mdl)?;
        huff.encode_eob_run(hstate.ac_zero_run[plane])?;
        hstate.ac_zr_coded[plane] = hstate.ac_zero_run[plane] > 0;
        return Ok(());
    }

    let mut idx = 1;
    while idx < 64 {
        let ac_band = VP6_IDX_TO_AC_BAND[idx].min(3);
        let ac_mode = last_val.abs().min(2) as usize;
        let mdl = &model.ac_token_tree[plane][ac_mode][ac_band];
        if idx > last_idx {
            huff.encode_eob(mdl)?;
            break;
        }
        let val = coeffs[scan[idx]];
        huff.encode_val(val, mdl)?;

        idx += 1;
        last_val = val;

        if val == 0 {
            let first_idx = idx;
            while idx < 64 && coeffs[scan[idx]] == 0 {
                idx += 1;
            }
            let zrun = idx - first_idx;
            huff.encode_zero_run(zrun, &model.zero_run_tree[if first_idx >= 7 { 1 } else { 0 }])?;
        }
    }

    Ok(())
}
