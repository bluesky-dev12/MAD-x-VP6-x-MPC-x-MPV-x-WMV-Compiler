use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;

use super::{MAX_SLOTS, NUM_ENVELOPES, SBR_BANDS, SBRState, SBRChannel};
use super::synth::QuantMode;

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum FrameClass {
    FixFix,
    FixVar,
    VarFix,
    VarVar,
}

impl FrameClass {
    fn read(br: &mut BitReader) -> DecoderResult<Self> {
        Ok(match br.read(2)? {
            0 => Self::FixFix,
            1 => Self::FixVar,
            2 => Self::VarFix,
            _ => Self::VarVar,
        })
    }
}

pub fn sbr_read_sce(br: &mut BitReader, orig_amp_res: bool, state: &SBRState, cbs: &SBRCodebooks, ch: &mut SBRChannel) -> DecoderResult<()> {
    ch.qmode = QuantMode::Single;
    if br.read_bool()? {
                                          br.skip(4)?;
    }
    read_grid(br, ch)?;
    read_dtdf(br, ch)?;
    read_invf(br, ch, state)?;
    ch.set_amp_res(orig_amp_res);
    read_envelope(br, ch, false, cbs, state)?;
    read_noise(br, ch, false, cbs, state)?;
    read_sinusoidal_coding(br, ch, state)?;
    read_extensions(br)?;

    Ok(())
}
pub fn sbr_read_cpe(br: &mut BitReader, orig_amp_res: bool, state: &SBRState, cbs: &SBRCodebooks, ch: &mut [SBRChannel; 2]) -> DecoderResult<()> {
    if br.read_bool()? {
                                          br.skip(4)?;
                                          br.skip(4)?;
    }
    let coupling                        = br.read_bool()?;
    if coupling {
        ch[0].qmode = QuantMode::Left;
        ch[1].qmode = QuantMode::Right;
        read_grid(br, &mut ch[0])?;
        ch[1].fclass            = ch[0].fclass;
        ch[1].num_env           = ch[0].num_env;
        ch[1].env_border        = ch[0].env_border;
        ch[1].freq_res          = ch[0].freq_res;
        ch[1].pointer           = ch[0].pointer;
        ch[1].num_noise         = ch[0].num_noise;
        ch[1].noise_env_border  = ch[0].noise_env_border;

        read_dtdf(br, &mut ch[0])?;
        read_dtdf(br, &mut ch[1])?;
        read_invf(br, &mut ch[0], state)?;
        ch[1].invf_mode = ch[0].invf_mode;

        ch[0].set_amp_res(orig_amp_res);
        read_envelope(br, &mut ch[0], false, cbs, state)?;
        read_noise(br, &mut ch[0], false, cbs, state)?;
        ch[1].set_amp_res(orig_amp_res);
        read_envelope(br, &mut ch[1], true, cbs, state)?;
        read_noise(br, &mut ch[1], true, cbs, state)?;

        ch[0].data_env2   = ch[1].data_env;
        ch[0].data_noise2 = ch[1].data_noise;
        ch[1].data_env2   = ch[0].data_env;
        ch[1].data_noise2 = ch[0].data_noise;
    } else {
        ch[0].qmode = QuantMode::Single;
        ch[1].qmode = QuantMode::Single;
        read_grid(br, &mut ch[0])?;
        read_grid(br, &mut ch[1])?;
        read_dtdf(br, &mut ch[0])?;
        read_dtdf(br, &mut ch[1])?;
        read_invf(br, &mut ch[0], state)?;
        read_invf(br, &mut ch[1], state)?;

        ch[0].set_amp_res(orig_amp_res);
        read_envelope(br, &mut ch[0], false, cbs, state)?;
        ch[1].set_amp_res(orig_amp_res);
        read_envelope(br, &mut ch[1], false, cbs, state)?;
        read_noise(br, &mut ch[0], false, cbs, state)?;
        read_noise(br, &mut ch[1], false, cbs, state)?;
    }
    read_sinusoidal_coding(br, &mut ch[0], state)?;
    read_sinusoidal_coding(br, &mut ch[1], state)?;
    read_extensions(br)?;

    Ok(())
}

fn read_grid(br: &mut BitReader, chan: &mut SBRChannel) -> DecoderResult<()> {
    chan.fclass = FrameClass::read(br)?;
    match chan.fclass {
        FrameClass::FixFix => {
            chan.num_env                = 1 << br.read(2)?;
            let freq_res                = br.read_bool()?;
            for el in chan.freq_res[..chan.num_env].iter_mut() {
                *el = freq_res;
            }
            chan.env_border[0] = 0;
            if chan.num_env > 1 {
                let delta = (MAX_SLOTS + chan.num_env / 2) / chan.num_env;
                for i in 1..chan.num_env {
                    chan.env_border[i] = chan.env_border[i - 1] + delta;
                }
            }
            chan.env_border[chan.num_env] = MAX_SLOTS;
        },
        FrameClass::FixVar => {
            let var_bord_1              = br.read(2)? as u8;
            chan.num_env                = br.read(2)? as usize + 1;
            let mut rel_bord_1 = [0u8; NUM_ENVELOPES];
            for el in rel_bord_1[..chan.num_env - 1].iter_mut() {
                *el                 = 2 * (br.read(2)? as u8) + 2;
            }
            let ptr_bits = 8 - (chan.num_env as u8).leading_zeros();
            chan.pointer        = br.read(ptr_bits as u8)? as u8;
            for el in chan.freq_res[..chan.num_env].iter_mut().rev() {
                *el                 = br.read_bool()?;
            }

            chan.env_border[0] = 0;
            chan.env_border[chan.num_env] = MAX_SLOTS + usize::from(var_bord_1);
            for (i, &delta) in (1..chan.num_env).rev().zip(rel_bord_1.iter()) {
                chan.env_border[i] = chan.env_border[i + 1] - usize::from(delta);
            }
        },
        FrameClass::VarFix => {
            let var_bord_0          = br.read(2)? as u8;
            chan.num_env        = br.read(2)? as usize + 1;
            let mut rel_bord_0 = [0u8; NUM_ENVELOPES];
            for el in rel_bord_0[..chan.num_env - 1].iter_mut() {
                *el                 = 2 * (br.read(2)? as u8) + 2;
            }
            let ptr_bits = 8 - (chan.num_env as u8).leading_zeros();
            chan.pointer        = br.read(ptr_bits as u8)? as u8;
            for el in chan.freq_res[..chan.num_env].iter_mut() {
                *el                 = br.read_bool()?;
            }

            chan.env_border[0] = usize::from(var_bord_0);
            for i in 0..chan.num_env - 1 {
                chan.env_border[i + 1] = chan.env_border[i] + usize::from(rel_bord_0[i]);
            }
            chan.env_border[chan.num_env] = MAX_SLOTS;
        },
        FrameClass::VarVar => {
            let var_bord_0          = br.read(2)? as u8;
            let var_bord_1          = br.read(2)? as u8;
            let num_rel_0           = br.read(2)? as usize;
            let num_rel_1           = br.read(2)? as usize;
            chan.num_env = num_rel_0 + num_rel_1 + 1;
            let mut rel_bord_0 = [0u8; NUM_ENVELOPES];
            let mut rel_bord_1 = [0u8; NUM_ENVELOPES];
            for el in rel_bord_0[..num_rel_0].iter_mut() {
                *el                 = 2 * (br.read(2)? as u8) + 2;
            }
            for el in rel_bord_1[..num_rel_1].iter_mut() {
                *el                 = 2 * (br.read(2)? as u8) + 2;
            }
            let ptr_bits = 8 - (chan.num_env as u8).leading_zeros();
            chan.pointer        = br.read(ptr_bits as u8)? as u8;
            for el in chan.freq_res[..chan.num_env].iter_mut() {
                *el                 = br.read_bool()?;
            }

            chan.env_border[0] = usize::from(var_bord_0);
            for i in 0..num_rel_0 {
                chan.env_border[i + 1] = chan.env_border[i] + usize::from(rel_bord_0[i]);
            }
            chan.env_border[chan.num_env] = MAX_SLOTS + usize::from(var_bord_1);
            for i in 0..num_rel_1 {
                chan.env_border[chan.num_env - 1 - i] = chan.env_border[chan.num_env - i] - usize::from(rel_bord_1[i]);
            }
        },
    };
    for i in 0..chan.num_env {
        validate!(chan.env_border[i] < chan.env_border[i + 1]);
    }

    if chan.num_env > 1 {
        chan.num_noise = 2;
        let mid = match (chan.fclass, chan.pointer) {
                (FrameClass::FixFix, _) => chan.num_env / 2,
                (FrameClass::VarFix, 0) => 1,
                (FrameClass::VarFix, 1) => chan.num_env - 1,
                (FrameClass::VarFix, _) => chan.pointer as usize - 1,
                (_, 0) | (_, 1)         => chan.num_env - 1,
                (_, _)                  => chan.num_env + 1 - (chan.pointer as usize),
            };
        chan.noise_env_border[0] = chan.env_border[0];
        chan.noise_env_border[1] = chan.env_border[mid];
        chan.noise_env_border[2] = chan.env_border[chan.num_env];
    } else {
        chan.num_noise = 1;
        chan.noise_env_border[0] = chan.env_border[0];
        chan.noise_env_border[1] = chan.env_border[1];
    }


    Ok(())
}
fn read_dtdf(br: &mut BitReader, chan: &mut SBRChannel) -> DecoderResult<()> {
    for el in chan.df_env[..chan.num_env].iter_mut() {
        *el                         = br.read_bool()?;
    }
    for el in chan.df_noise[..chan.num_noise].iter_mut() {
        *el                         = br.read_bool()?;
    }
    Ok(())
}
fn read_invf(br: &mut BitReader, chan: &mut SBRChannel, state: &SBRState) -> DecoderResult<()> {
    for el in chan.invf_mode[..state.num_noise_bands].iter_mut() {
        *el                         = br.read(2)? as u8;
    }
    Ok(())
}
fn read_envelope(br: &mut BitReader, chan: &mut SBRChannel, coupled: bool, cbs: &SBRCodebooks, state: &SBRState) -> DecoderResult<()> {
    let (f_cb, t_cb) = if coupled {
            if chan.amp_res {
                (&cbs.env_bal_3_0db_f_cb, &cbs.env_bal_3_0db_t_cb)
            } else {
                (&cbs.env_bal_1_5db_f_cb, &cbs.env_bal_1_5db_t_cb)
            }
        } else {
            if chan.amp_res {
                (&cbs.env_3_0db_f_cb, &cbs.env_3_0db_t_cb)
            } else {
                (&cbs.env_1_5db_f_cb, &cbs.env_1_5db_t_cb)
            }
        };
    let scale = if coupled { 2 } else { 1 };
    for (envelope, (&df_env, &freq_res)) in chan.data_env[..chan.num_env].iter_mut().zip(chan.df_env.iter().zip(chan.freq_res.iter())) {
        let num_env_bands = state.num_env_bands[freq_res as usize];
        if !df_env {
            if coupled {
                envelope[0]         = br.read(5 + (!chan.amp_res as u8))? as i8;
            } else {
                envelope[0]         = br.read(6 + (!chan.amp_res as u8))? as i8;
            }
            envelope[0] *= scale;
            let mut last = envelope[0];
            for band_env in envelope[1..num_env_bands].iter_mut() {
                let delta           = br.read_cb(f_cb)?;
                *band_env = last + delta * scale;
                last = *band_env
            }
        } else {
            for (i, band_env) in envelope[..num_env_bands].iter_mut().enumerate() {
                let delta           = br.read_cb(t_cb)?;
                let last = match (freq_res, chan.last_freq_res) {
                        (false, true) => chan.last_envelope[state.high_to_low_res[i]],
                        (true, false) => chan.last_envelope[state.low_to_high_res[i]],
                        _ => chan.last_envelope[i],
                    };
                *band_env = last + delta * scale;
            }
        }
        chan.last_envelope = *envelope;
        chan.last_freq_res = freq_res;
    }
    Ok(())
}
fn read_noise(br: &mut BitReader, chan: &mut SBRChannel, coupled: bool, cbs: &SBRCodebooks, state: &SBRState) -> DecoderResult<()> {
    let (f_cb, t_cb) = if coupled {
            (&cbs.env_bal_3_0db_f_cb, &cbs.noise_bal_3_0db_t_cb)
        } else {
            (&cbs.env_3_0db_f_cb, &cbs.noise_3_0db_t_cb)
        };
    let scale = if coupled { 2 } else { 1 };
    for (noise, &df_noise) in chan.data_noise[..chan.num_noise].iter_mut().zip(chan.df_noise.iter()) {
        if !df_noise {
            noise[0]                = (br.read(5)? as i8) * scale;
            let mut last = noise[0];
            for band_noise in noise[1..state.num_noise_bands].iter_mut() {
                let delta           = br.read_cb(f_cb)?;
                *band_noise = last + scale * delta;
                last = *band_noise;
            }
        } else {
            for (band_noise, &last) in noise[..state.num_noise_bands].iter_mut().zip(chan.last_noise_env.iter()) {
                let delta           = br.read_cb(t_cb)?;
                *band_noise = last + delta * scale;
            }
        }
        chan.last_noise_env = *noise;
    }
    Ok(())
}
fn read_sinusoidal_coding(br: &mut BitReader, chan: &mut SBRChannel, state: &SBRState) -> DecoderResult<()> {
    if !br.read_bool()? {
        chan.add_harmonic = [false; SBR_BANDS];
        return Ok(());
    }
    for el in chan.add_harmonic[..state.num_env_bands[1]].iter_mut() {
        *el                         = br.read_bool()?;
    }
    Ok(())
}
fn read_extensions(br: &mut BitReader) -> DecoderResult<()> {
    if br.read_bool()? {
        let mut size                = br.read(4)? as usize;
        if size == 15 {
            size                   += br.read(8)? as usize;
        }
        validate!(br.left() >= ((size * 8) as isize));
        let end = br.tell() + size * 8;
        while br.tell() + 7 < end {
            let _extension_id       = br.read(2)?;
            // todo parse PS?
        }
    }
    Ok(())
}

pub struct SBRCodebooks {
    env_bal_1_5db_f_cb:     Codebook<i8>,
    env_bal_1_5db_t_cb:     Codebook<i8>,
    env_bal_3_0db_f_cb:     Codebook<i8>,
    env_bal_3_0db_t_cb:     Codebook<i8>,
    env_1_5db_f_cb:         Codebook<i8>,
    env_1_5db_t_cb:         Codebook<i8>,
    env_3_0db_f_cb:         Codebook<i8>,
    env_3_0db_t_cb:         Codebook<i8>,
    noise_bal_3_0db_t_cb:   Codebook<i8>,
    noise_3_0db_t_cb:       Codebook<i8>,
}

fn map_idx12(idx: usize) -> i8 { idx as i8 - 12 }
fn map_idx24(idx: usize) -> i8 { idx as i8 - 24 }
fn map_idx31(idx: usize) -> i8 { idx as i8 - 31 }
fn map_idx60(idx: usize) -> i8 { idx as i8 - 60 }

impl SBRCodebooks {
    pub fn new() -> Self {
        let mut cbr = TableCodebookDescReader::new(ENV_BAL_1_5DB_F_BITS, ENV_BAL_1_5DB_F_LENS, map_idx24);
        let env_bal_1_5db_f_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(ENV_BAL_1_5DB_T_BITS, ENV_BAL_1_5DB_T_LENS, map_idx24);
        let env_bal_1_5db_t_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();

        let mut cbr = TableCodebookDescReader::new(ENV_1_5DB_F_BITS, ENV_1_5DB_F_LENS, map_idx60);
        let env_1_5db_f_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(ENV_1_5DB_T_BITS, ENV_1_5DB_T_LENS, map_idx60);
        let env_1_5db_t_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();

        let mut cbr = TableCodebookDescReader::new(ENV_BAL_3_0DB_F_BITS, ENV_BAL_3_0DB_F_LENS, map_idx12);
        let env_bal_3_0db_f_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(ENV_BAL_3_0DB_T_BITS, ENV_BAL_3_0DB_T_LENS, map_idx12);
        let env_bal_3_0db_t_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();

        let mut cbr = TableCodebookDescReader::new(ENV_3_0DB_F_BITS, ENV_3_0DB_F_LENS, map_idx31);
        let env_3_0db_f_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(ENV_3_0DB_T_BITS, ENV_3_0DB_T_LENS, map_idx31);
        let env_3_0db_t_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();

        let mut cbr = TableCodebookDescReader::new(NOISE_BAL_3_0DB_T_BITS, NOISE_BAL_3_0DB_T_LENS, map_idx12);
        let noise_bal_3_0db_t_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(NOISE_3_0DB_T_BITS, NOISE_3_0DB_T_LENS, map_idx31);
        let noise_3_0db_t_cb = Codebook::new(&mut cbr, CodebookMode::MSB).unwrap();

        Self {
            env_bal_1_5db_f_cb, env_bal_1_5db_t_cb,
            env_1_5db_f_cb,     env_1_5db_t_cb,
            env_bal_3_0db_f_cb, env_bal_3_0db_t_cb,
            env_3_0db_f_cb,     env_3_0db_t_cb,
            noise_3_0db_t_cb, noise_bal_3_0db_t_cb
        }
    }
}

const ENV_1_5DB_F_BITS: &[u32; 121] = &[
    0x7ffe7, 0x7ffe8, 0xfffd2, 0xfffd3, 0xfffd4, 0xfffd5, 0xfffd6, 0xfffd7,
    0xfffd8, 0x7ffda, 0xfffd9, 0xfffda, 0xfffdb, 0xfffdc, 0x7ffdb, 0xfffdd,
    0x7ffdc, 0x7ffdd, 0xfffde, 0x3ffe4, 0xfffdf, 0xfffe0, 0xfffe1, 0x7ffde,
    0xfffe2, 0xfffe3, 0xfffe4, 0x7ffdf, 0xfffe5, 0x7ffe0, 0x3ffe8, 0x7ffe1,
    0x3ffe0, 0x3ffe9, 0x1ffef, 0x3ffe5, 0x1ffec, 0x1ffed, 0x1ffee, 0x0fff4,
    0x0fff3, 0x0fff0, 0x07ff7, 0x07ff6, 0x03ffa, 0x01ffa, 0x01ff9, 0x00ffa,
    0x00ff8, 0x007f9, 0x003fb, 0x001fc, 0x001fa, 0x000fb, 0x0007c, 0x0003c,
    0x0001c, 0x0000c, 0x00005, 0x00001, 0x00000, 0x00004, 0x0000d, 0x0001d,
    0x0003d, 0x000fa, 0x000fc, 0x001fb, 0x003fa, 0x007f8, 0x007fa, 0x007fb,
    0x00ff9, 0x00ffb, 0x01ff8, 0x01ffb, 0x03ff8, 0x03ff9, 0x0fff1, 0x0fff2,
    0x1ffea, 0x1ffeb, 0x3ffe1, 0x3ffe2, 0x3ffea, 0x3ffe3, 0x3ffe6, 0x3ffe7,
    0x3ffeb, 0xfffe6, 0x7ffe2, 0xfffe7, 0xfffe8, 0xfffe9, 0xfffea, 0xfffeb,
    0xfffec, 0x7ffe3, 0xfffed, 0xfffee, 0xfffef, 0xffff0, 0x7ffe4, 0xffff1,
    0x3ffec, 0xffff2, 0xffff3, 0x7ffe5, 0x7ffe6, 0xffff4, 0xffff5, 0xffff6,
    0xffff7, 0xffff8, 0xffff9, 0xffffa, 0xffffb, 0xffffc, 0xffffd, 0xffffe,
    0xfffff
];
const ENV_1_5DB_F_LENS: &[u8; 121] = &[
    19, 19, 20, 20, 20, 20, 20, 20,
    20, 19, 20, 20, 20, 20, 19, 20,
    19, 19, 20, 18, 20, 20, 20, 19,
    20, 20, 20, 19, 20, 19, 18, 19,
    18, 18, 17, 18, 17, 17, 17, 16,
    16, 16, 15, 15, 14, 13, 13, 12,
    12, 11, 10,  9,  9,  8,  7,  6,
     5,  4,  3,  2,  2,  3,  4,  5,
     6,  8,  8,  9, 10, 11, 11, 11,
    12, 12, 13, 13, 14, 14, 16, 16,
    17, 17, 18, 18, 18, 18, 18, 18,
    18, 20, 19, 20, 20, 20, 20, 20,
    20, 19, 20, 20, 20, 20, 19, 20,
    18, 20, 20, 19, 19, 20, 20, 20,
    20, 20, 20, 20, 20, 20, 20, 20,
    20
];
const ENV_1_5DB_T_BITS: &[u32; 121] = &[
    0x3ffd6, 0x3ffd7, 0x3ffd8, 0x3ffd9, 0x3ffda, 0x3ffdb, 0x7ffb8, 0x7ffb9,
    0x7ffba, 0x7ffbb, 0x7ffbc, 0x7ffbd, 0x7ffbe, 0x7ffbf, 0x7ffc0, 0x7ffc1,
    0x7ffc2, 0x7ffc3, 0x7ffc4, 0x7ffc5, 0x7ffc6, 0x7ffc7, 0x7ffc8, 0x7ffc9,
    0x7ffca, 0x7ffcb, 0x7ffcc, 0x7ffcd, 0x7ffce, 0x7ffcf, 0x7ffd0, 0x7ffd1,
    0x7ffd2, 0x7ffd3, 0x1ffe6, 0x3ffd4, 0x0fff0, 0x1ffe9, 0x3ffd5, 0x1ffe7,
    0x0fff1, 0x0ffec, 0x0ffed, 0x0ffee, 0x07ff4, 0x03ff9, 0x03ff7, 0x01ffa,
    0x01ff9, 0x00ffb, 0x007fc, 0x003fc, 0x001fd, 0x000fd, 0x0007d, 0x0003d,
    0x0001d, 0x0000d, 0x00005, 0x00001, 0x00000, 0x00004, 0x0000c, 0x0001c,
    0x0003c, 0x0007c, 0x000fc, 0x001fc, 0x003fd, 0x00ffa, 0x01ff8, 0x03ff6,
    0x03ff8, 0x07ff5, 0x0ffef, 0x1ffe8, 0x0fff2, 0x7ffd4, 0x7ffd5, 0x7ffd6,
    0x7ffd7, 0x7ffd8, 0x7ffd9, 0x7ffda, 0x7ffdb, 0x7ffdc, 0x7ffdd, 0x7ffde,
    0x7ffdf, 0x7ffe0, 0x7ffe1, 0x7ffe2, 0x7ffe3, 0x7ffe4, 0x7ffe5, 0x7ffe6,
    0x7ffe7, 0x7ffe8, 0x7ffe9, 0x7ffea, 0x7ffeb, 0x7ffec, 0x7ffed, 0x7ffee,
    0x7ffef, 0x7fff0, 0x7fff1, 0x7fff2, 0x7fff3, 0x7fff4, 0x7fff5, 0x7fff6,
    0x7fff7, 0x7fff8, 0x7fff9, 0x7fffa, 0x7fffb, 0x7fffc, 0x7fffd, 0x7fffe,
    0x7ffff
];
const ENV_1_5DB_T_LENS: &[u8; 121] = &[
    18, 18, 18, 18, 18, 18, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 17, 18, 16, 17, 18, 17,
    16, 16, 16, 16, 15, 14, 14, 13,
    13, 12, 11, 10,  9,  8,  7,  6,
     5,  4,  3,  2,  2,  3,  4,  5,
     6,  7,  8,  9, 10, 12, 13, 14,
    14, 15, 16, 17, 16, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19
];

const ENV_BAL_1_5DB_F_BITS: &[u32; 49] = &[
    0x3ffe2, 0x3ffe3, 0x3ffe4, 0x3ffe5, 0x3ffe6, 0x3ffe7, 0x3ffe8, 0x3ffe9,
    0x3ffea, 0x3ffeb, 0x3ffec, 0x3ffed, 0x3ffee, 0x3ffef, 0x3fff0, 0x0fff7,
    0x1fff0, 0x03ffc, 0x007fe, 0x007fc, 0x000fe, 0x0007e, 0x0000e, 0x00002,
    0x00000, 0x00006, 0x0001e, 0x0003e, 0x001fe, 0x007fd, 0x00ffe, 0x07ffa,
    0x0fff6, 0x3fff1, 0x3fff2, 0x3fff3, 0x3fff4, 0x3fff5, 0x3fff6, 0x3fff7,
    0x3fff8, 0x3fff9, 0x3fffa, 0x3fffb, 0x3fffc, 0x3fffd, 0x3fffe, 0x7fffe,
    0x7ffff
];
const ENV_BAL_1_5DB_F_LENS: &[u8; 49] = &[
    18, 18, 18, 18, 18, 18, 18, 18,
    18, 18, 18, 18, 18, 18, 18, 16,
    17, 14, 11, 11,  8,  7,  4,  2,
     1,  3,  5,  6,  9, 11, 12, 15,
    16, 18, 18, 18, 18, 18, 18, 18,
    18, 18, 18, 18, 18, 18, 18, 19,
    19
];
const ENV_BAL_1_5DB_T_BITS: &[u32; 49] = &[
    0x0ffe4, 0x0ffe5, 0x0ffe6, 0x0ffe7, 0x0ffe8, 0x0ffe9, 0x0ffea, 0x0ffeb,
    0x0ffec, 0x0ffed, 0x0ffee, 0x0ffef, 0x0fff0, 0x0fff1, 0x0fff2, 0x0fff3,
    0x0fff4, 0x0ffe2, 0x00ffc, 0x007fc, 0x001fe, 0x0007e, 0x0001e, 0x00006,
    0x00000, 0x00002, 0x0000e, 0x0003e, 0x000fe, 0x007fd, 0x00ffd, 0x07ff0,
    0x0ffe3, 0x0fff5, 0x0fff6, 0x0fff7, 0x0fff8, 0x0fff9, 0x0fffa, 0x1fff6,
    0x1fff7, 0x1fff8, 0x1fff9, 0x1fffa, 0x1fffb, 0x1fffc, 0x1fffd, 0x1fffe,
    0x1ffff
];
const ENV_BAL_1_5DB_T_LENS: &[u8; 49] = &[
    16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 12, 11,  9,  7,  5,  3,
     1,  2,  4,  6,  8, 11, 12, 15,
    16, 16, 16, 16, 16, 16, 16, 17,
    17, 17, 17, 17, 17, 17, 17, 17,
    17
];

const ENV_3_0DB_F_BITS: &[u32; 63] = &[
    0xffff0, 0xffff1, 0xffff2, 0xffff3, 0xffff4, 0xffff5, 0xffff6, 0x3fff3,
    0x7fff5, 0x7ffee, 0x7ffef, 0x7fff6, 0x3fff4, 0x3fff2, 0xffff7, 0x7fff0,
    0x1fff5, 0x3fff0, 0x1fff4, 0x0fff7, 0x0fff6, 0x07ff8, 0x03ffb, 0x00ffd,
    0x007fd, 0x003fd, 0x001fd, 0x000fd, 0x0003e, 0x0000e, 0x00002, 0x00000,
    0x00006, 0x0001e, 0x000fc, 0x001fc, 0x003fc, 0x007fc, 0x00ffc, 0x01ffc,
    0x03ffa, 0x07ff9, 0x07ffa, 0x0fff8, 0x0fff9, 0x1fff6, 0x1fff7, 0x3fff5,
    0x3fff6, 0x3fff1, 0xffff8, 0x7fff1, 0x7fff2, 0x7fff3, 0xffff9, 0x7fff7,
    0x7fff4, 0xffffa, 0xffffb, 0xffffc, 0xffffd, 0xffffe, 0xfffff
];
const ENV_3_0DB_F_LENS: &[u8; 63] = &[
    20, 20, 20, 20, 20, 20, 20, 18,
    19, 19, 19, 19, 18, 18, 20, 19,
    17, 18, 17, 16, 16, 15, 14, 12,
    11, 10,  9,  8,  6,  4,  2,  1,
     3,  5,  8,  9, 10, 11, 12, 13,
    14, 15, 15, 16, 16, 17, 17, 18,
    18, 18, 20, 19, 19, 19, 20, 19,
    19, 20, 20, 20, 20, 20, 20
];
const ENV_3_0DB_T_BITS: &[u32; 63] = &[
    0x3ffed, 0x3ffee, 0x7ffde, 0x7ffdf, 0x7ffe0, 0x7ffe1, 0x7ffe2, 0x7ffe3,
    0x7ffe4, 0x7ffe5, 0x7ffe6, 0x7ffe7, 0x7ffe8, 0x7ffe9, 0x7ffea, 0x7ffeb,
    0x7ffec, 0x1fff4, 0x0fff7, 0x0fff9, 0x0fff8, 0x03ffb, 0x03ffa, 0x03ff8,
    0x01ffa, 0x00ffc, 0x007fc, 0x000fe, 0x0003e, 0x0000e, 0x00002, 0x00000,
    0x00006, 0x0001e, 0x0007e, 0x001fe, 0x007fd, 0x01ffb, 0x03ff9, 0x03ffc,
    0x07ffa, 0x0fff6, 0x1fff5, 0x3ffec, 0x7ffed, 0x7ffee, 0x7ffef, 0x7fff0,
    0x7fff1, 0x7fff2, 0x7fff3, 0x7fff4, 0x7fff5, 0x7fff6, 0x7fff7, 0x7fff8,
    0x7fff9, 0x7fffa, 0x7fffb, 0x7fffc, 0x7fffd, 0x7fffe, 0x7ffff
];
const ENV_3_0DB_T_LENS: &[u8; 63] = &[
    18, 18, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 17, 16, 16, 16, 14, 14, 14,
    13, 12, 11,  8,  6,  4,  2,  1,
     3,  5,  7,  9, 11, 13, 14, 14,
    15, 16, 17, 18, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19
];

const ENV_BAL_3_0DB_F_BITS: &[u16; 25] = &[
    0x1ff7, 0x1ff8, 0x1ff9, 0x1ffa, 0x1ffb, 0x3ff8, 0x3ff9, 0x07fc,
    0x00fe, 0x007e, 0x000e, 0x0002, 0x0000, 0x0006, 0x001e, 0x003e,
    0x01fe, 0x0ffa, 0x1ff6, 0x3ffa, 0x3ffb, 0x3ffc, 0x3ffd, 0x3ffe,
    0x3fff
];
const ENV_BAL_3_0DB_F_LENS: &[u8; 25] = &[
    13, 13, 13, 13, 13, 14, 14, 11,
     8,  7,  4,  2,  1,  3,  5,  6,
     9, 12, 13, 14, 14, 14, 14, 14,
    14
];
const ENV_BAL_3_0DB_T_BITS: &[u16; 25] = &[
    0x1ff2, 0x1ff3, 0x1ff4, 0x1ff5, 0x1ff6, 0x1ff7, 0x1ff8, 0x0ff8,
    0x00fe, 0x007e, 0x000e, 0x0006, 0x0000, 0x0002, 0x001e, 0x003e,
    0x01fe, 0x1ff9, 0x1ffa, 0x1ffb, 0x1ffc, 0x1ffd, 0x1ffe, 0x3ffe,
    0x3fff
];
const ENV_BAL_3_0DB_T_LENS: &[u8; 25] = &[
    13, 13, 13, 13, 13, 13, 13, 12,
     8,  7,  4,  3,  1,  2,  5,  6,
     9, 13, 13, 13, 13, 13, 13, 14,
    14
];

const NOISE_3_0DB_T_BITS: &[u16; 63] = &[
    0x1fce, 0x1fcf, 0x1fd0, 0x1fd1, 0x1fd2, 0x1fd3, 0x1fd4, 0x1fd5,
    0x1fd6, 0x1fd7, 0x1fd8, 0x1fd9, 0x1fda, 0x1fdb, 0x1fdc, 0x1fdd,
    0x1fde, 0x1fdf, 0x1fe0, 0x1fe1, 0x1fe2, 0x1fe3, 0x1fe4, 0x1fe5,
    0x1fe6, 0x1fe7, 0x07f2, 0x00fd, 0x003e, 0x000e, 0x0006, 0x0000,
    0x0002, 0x001e, 0x00fc, 0x03f8, 0x1fcc, 0x1fe8, 0x1fe9, 0x1fea,
    0x1feb, 0x1fec, 0x1fcd, 0x1fed, 0x1fee, 0x1fef, 0x1ff0, 0x1ff1,
    0x1ff2, 0x1ff3, 0x1ff4, 0x1ff5, 0x1ff6, 0x1ff7, 0x1ff8, 0x1ff9,
    0x1ffa, 0x1ffb, 0x1ffc, 0x1ffd, 0x1ffe, 0x3ffe, 0x3fff
];
const NOISE_3_0DB_T_LENS: &[u8; 63] = &[
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 11,  8,  6,  4,  3,  1,
     2,  5,  8, 10, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 14, 14
];
const NOISE_BAL_3_0DB_T_BITS: &[u8; 25] = &[
    0xec, 0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3,
    0xf4, 0xf5, 0x1c, 0x02, 0x00, 0x06, 0x3a, 0xf6,
    0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe,
    0xff
];
const NOISE_BAL_3_0DB_T_LENS: &[u8; 25] = &[
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 5, 2, 1, 3, 6, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8
];
