use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::io::bitreader::*;
use super::mp2data::*;

#[derive(Default)]
pub struct MP2Data {
    pub mpeg1:      bool,
    pub sf_idx:     usize,
    pub br_idx:     usize,
        alloc:      [[u8; 32]; 2],
        scfsi:      [[u8; 32]; 2],
        scale:      [[[u8; 3]; 32]; 2],
}

fn unquant(val: u8, levels: i16, scale: f32) -> f32 {
    (((i16::from(val) << 1) + 1 - levels) as f32) * scale / (levels as f32)
}

fn read3samples(br: &mut BitReader, qbits: i8, scale: f32, band: usize, dst: &mut [[f32; 32]]) -> DecoderResult<()> {
    if qbits > 0 {
        let bits = qbits as u8;
        let maxval = (1 << bits) - 1;
        dst[0][band]                    = unquant(br.read(bits)? as u8, maxval, scale);
        dst[1][band]                    = unquant(br.read(bits)? as u8, maxval, scale);
        dst[2][band]                    = unquant(br.read(bits)? as u8, maxval, scale);
    } else {
        let samplecode                  = br.read(-qbits as u8)? as usize;
        match qbits {
            -5 => {
                for i in 0..3 {
                    dst[i][band] = unquant(GROUP3[samplecode][i], 3, scale);
                }
            },
            -7 => {
                for i in 0..3 {
                    dst[i][band] = unquant(GROUP5[samplecode][i], 5, scale);
                }
            },
            -10 => {
                for i in 0..3 {
                    dst[i][band] = unquant(GROUP9[samplecode][i], 9, scale);
                }
            },
            _ => unreachable!(),
        };
    }
    Ok(())
}

fn read3samples_joint(br: &mut BitReader, qbits: i8, scale0: f32, scale1: f32, band: usize, gr: usize, dst: &mut [[[f32; 32]; 36]; 2]) -> DecoderResult<()> {
    if qbits > 0 {
        let bits = qbits as u8;
        let maxval = (1 << bits) - 1;
        for i in 0..3 {
            let val                     = br.read(bits)? as u8;
            dst[0][gr * 3 + i][band] = unquant(val, maxval, scale0);
            dst[1][gr * 3 + i][band] = unquant(val, maxval, scale1);
        }
    } else {
        let samplecode                  = br.read(-qbits as u8)? as usize;
        match qbits {
            -5 => {
                for i in 0..3 {
                    dst[0][gr * 3 + i][band] = unquant(GROUP3[samplecode][i], 3, scale0);
                    dst[1][gr * 3 + i][band] = unquant(GROUP3[samplecode][i], 3, scale1);
                }
            },
            -7 => {
                for i in 0..3 {
                    dst[0][gr * 3 + i][band] = unquant(GROUP5[samplecode][i], 5, scale0);
                    dst[1][gr * 3 + i][band] = unquant(GROUP5[samplecode][i], 5, scale1);
                }
            },
            -10 => {
                for i in 0..3 {
                    dst[0][gr * 3 + i][band] = unquant(GROUP9[samplecode][i], 9, scale0);
                    dst[1][gr * 3 + i][band] = unquant(GROUP9[samplecode][i], 9, scale1);
                }
            },
            _ => unreachable!(),
        };
    }
    Ok(())
}

impl MP2Data {
    pub fn new() -> Self { Self::default() }
    pub fn read_layer2(&mut self, br: &mut BitReader, channels: usize, out: &mut [[[f32; 32]; 36]; 2], mode: u8, mode_extension: u8) -> DecoderResult<()> {
        let (ba_bits, ba_quants) = if self.mpeg1 {
                if channels == 1 {
                    (LAYER2_BITS[self.sf_idx][self.br_idx], LAYER2_ALLOC[self.br_idx])
                } else {
                    let br_idx = HALF_BITRATE_IDX[self.br_idx] as usize;
                    (LAYER2_BITS[self.sf_idx][br_idx], LAYER2_ALLOC[br_idx])
                }
            } else {
                (BITS_B2LFE, ALLOC_B2LFE)
            };
        let mut sblimit = ba_bits.len() - 1;
        while sblimit > 0 && ba_bits[sblimit - 1] == 0 {
            sblimit -= 1;
        }
        if sblimit == 0 {
            return Err(DecoderError::Bug);
        }

        *out = [[[0.0; 32]; 36]; 2];
        if channels == 1 {
            for (bits, &b) in self.alloc[0][..sblimit].iter_mut().zip(ba_bits.iter()) {
                *bits                   = br.read(b)? as u8;
            }
            for (scf, &alloc) in self.scfsi[0][..sblimit].iter_mut().zip(self.alloc[0].iter()) {
                if alloc != 0 {
                    *scf                = br.read(2)? as u8;
                }
            }
            for (band, scales) in self.scale[0].iter_mut().take(sblimit).enumerate() {
                if self.alloc[0][band] == 0 {
                    continue;
                }
                scales[0]               = br.read(6)? as u8;
                match self.scfsi[0][band] {
                    0 => {
                        scales[1]       = br.read(6)? as u8;
                        scales[2]       = br.read(6)? as u8;
                    },
                    1 => {
                        scales[1] = scales[0];
                        scales[2]       = br.read(6)? as u8;
                    },
                    2 => {
                        scales[1] = scales[0];
                        scales[2] = scales[0];
                    },
                    _ => {
                        scales[1]       = br.read(6)? as u8;
                        scales[2] = scales[1];
                    },
                };
            }

            for gr in 0..12 {
                for band in 0..sblimit {
                    if self.alloc[0][band] == 0 {
                        continue;
                    }
                    let scale = QUANTS[self.scale[0][band][gr / 4] as usize];
                    let qbits = ba_quants[band][self.alloc[0][band] as usize];
                    read3samples(br, qbits, scale, band, &mut out[0][gr * 3..])?;
                }
            }
        } else {
            let mut bound = if mode == 1 {
                    (mode_extension as usize + 1) * 4
                } else {
                    sblimit
                };
            while bound > 0 && ba_bits[bound - 1] == 0 {
                bound -= 1;
            }

            for band in 0..sblimit {
                self.alloc[0][band]     = br.read(ba_bits[band])? as u8;
                if band < bound {
                    self.alloc[1][band] = br.read(ba_bits[band])? as u8;
                } else {
                    self.alloc[1][band] = self.alloc[0][band];
                }
            }
            for band in 0..sblimit {
                for ch in 0..2 {
                    if self.alloc[ch][band] != 0 {
                        self.scfsi[ch][band] = br.read(2)? as u8;
                    }
                }
            }
            for band in 0..sblimit {
                for ch in 0..2 {
                    if self.alloc[ch][band] == 0 {
                        continue;
                    }
                    let scales = &mut self.scale[ch][band];
                    scales[0]           = br.read(6)? as u8;
                    match self.scfsi[ch][band] {
                        0 => {
                            scales[1]   = br.read(6)? as u8;
                            scales[2]   = br.read(6)? as u8;
                        },
                        1 => {
                            scales[1] = scales[0];
                            scales[2]   = br.read(6)? as u8;
                        },
                        2 => {
                            scales[1] = scales[0];
                            scales[2] = scales[0];
                        },
                        _ => {
                            scales[1]   = br.read(6)? as u8;
                            scales[2] = scales[1];
                        },
                    };
                }
            }
            for gr in 0..12 {
                for band in 0..bound {
                    for ch in 0..2 {
                        if self.alloc[ch][band] == 0 {
                            continue;
                        }
                        let scale = QUANTS[self.scale[ch][band][gr / 4] as usize];
                        let qbits = ba_quants[band][self.alloc[ch][band] as usize];
                        read3samples(br, qbits, scale, band, &mut out[ch][gr * 3..])?;
                    }
                }
                for band in bound..sblimit {
                    if self.alloc[0][band] == 0 {
                        continue;
                    }
                    let scale0 = QUANTS[self.scale[0][band][gr / 4] as usize];
                    let scale1 = QUANTS[self.scale[1][band][gr / 4] as usize];
                    let qbits = ba_quants[band][self.alloc[0][band] as usize];
                    read3samples_joint(br, qbits, scale0, scale1, band, gr, out)?;
                }
            }
        }

        Ok(())
    }
    pub fn reset(&mut self) {
    }
}
