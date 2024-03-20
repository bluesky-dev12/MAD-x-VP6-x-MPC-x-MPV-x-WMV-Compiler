use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use super::truemotion1data::*;

struct MaskState<'a> {
    is_intra:   bool,
    pos:        usize,
    row_pos:   usize,
    row_size:   usize,
    mask:       u8,
    src:        &'a [u8],
}

impl<'a> MaskState<'a> {
    fn new(is_intra: bool, src: &'a [u8], row_size: usize) -> Self {
        Self { is_intra, src, pos: 0, row_pos: 0, row_size, mask: 0x01 }
    }
    fn get_next(&mut self) -> bool {
        if self.is_intra {
            true
        } else {
            let res = (self.src[self.pos] & self.mask) == 0;
            self.mask <<= 1;
            if self.mask == 0 {
                self.pos += 1;
                self.mask = 0x01;
            }
            res
        }
    }
    fn reset_row(&mut self) {
        self.pos = self.row_pos;
        self.mask = 0x01;
    }
    fn next_row(&mut self) {
        self.row_pos += self.row_size;
        self.reset_row();
    }
}

struct IndexState<'a> {
    src:        &'a [u8],
    pos:        usize,
    vec_idx:    usize,
    vec_subidx: usize,
}

impl<'a> IndexState<'a> {
    fn new(src: &'a [u8]) -> Self {
        Self { src, pos: 0, vec_idx: 0, vec_subidx: 0 }
    }
    fn get_next(&mut self) -> DecoderResult<()> {
        validate!(self.pos < self.src.len());
        self.vec_idx = self.src[self.pos] as usize;
        self.vec_subidx = 0;
        self.pos += 1;
        Ok(())
    }
    fn get_pred(&self, dtab: &[[u32; 4]; 256]) -> u32 { dtab[self.vec_idx][self.vec_subidx] }
    fn get_diff16(&mut self, dtab: &[[u32; 4]; 256]) -> DecoderResult<u32> {
        let pred1 = self.get_pred(dtab);
        let mut pred = pred1 >> 1;
        if (pred1 & 1) != 0 {
            self.get_next()?;
            if self.vec_idx == 0 {
                self.get_next()?;
                let pred2 = self.get_pred(dtab);
                pred = pred.wrapping_add((pred2 >> 1).wrapping_mul(5));
                if (pred2 & 1) != 0 {
                    self.get_next()?;
                } else {
                    self.vec_subidx += 1;
                }
            }
        } else {
            self.vec_subidx += 1;
        }
        Ok(pred)
    }
    fn get_diff16_noesc(&mut self, dtab: &[[u32; 4]; 256]) -> DecoderResult<u32> {
        let pred1 = self.get_pred(dtab);
        let pred = pred1 >> 1;
        if (pred1 & 1) != 0 {
            self.get_next()?;
        } else {
            self.vec_subidx += 1;
        }
        Ok(pred)
    }
    fn get_diff24(&mut self, dtab: &[[u32; 4]; 256], esctab: &[[u32; 4]; 256]) -> DecoderResult<u32> {
        let pred1 = self.get_pred(dtab);
        let mut pred = pred1 >> 1;
        if (pred1 & 1) != 0 {
            self.get_next()?;
            if self.vec_idx == 0 {
                self.get_next()?;
                let pred2 = self.get_pred(esctab);
                pred = pred.wrapping_add(pred2 >> 1);
                if (pred2 & 1) != 0 {
                    self.get_next()?;
                } else {
                    self.vec_subidx += 1;
                }
            }
        } else {
            self.vec_subidx += 1;
        }
        Ok(pred)
    }
}

struct DeltaTables {
    ydt:        [[u32; 4]; 256],
    cdt:        [[u32; 4]; 256],
    fat_ydt:    [[u32; 4]; 256],
    fat_cdt:    [[u32; 4]; 256],
    adt:        [[u32; 4]; 256],
}

impl Default for DeltaTables {
    fn default() -> Self {
        Self {
            ydt:        [[0; 4]; 256],
            cdt:        [[0; 4]; 256],
            fat_ydt:    [[0; 4]; 256],
            fat_cdt:    [[0; 4]; 256],
            adt:        [[0; 4]; 256],
        }
    }
}

#[derive(Default)]
struct FrameBuf {
    last16: Option<NAVideoBufferRef<u16>>,
    last24: Option<NAVideoBufferRef<u8>>,
}

impl FrameBuf {
    fn set16(&mut self, buf: NAVideoBufferRef<u16>) { self.last16 = Some(buf); }
    fn set24(&mut self, buf: NAVideoBufferRef<u8>)  { self.last24 = Some(buf); }
    fn get16(&mut self) -> Option<NAVideoBufferRef<u16>> {
        if let Some(ref mut frm) = self.last16 {
            let newfrm = frm.copy_buffer();
            *frm = newfrm.clone().into_ref();
            Some(newfrm.into_ref())
        } else {
            None
        }
    }
    fn get24(&mut self) -> Option<NAVideoBufferRef<u8>> {
        if let Some(ref mut frm) = self.last24 {
            let newfrm = frm.copy_buffer();
            *frm = newfrm.clone().into_ref();
            Some(newfrm.into_ref())
        } else {
            None
        }
    }
    fn reset(&mut self) {
        self.last16 = None;
        self.last24 = None;
    }
}

#[derive(Default)]
struct TM1Decoder {
    info:           NACodecInfoRef,
    last_delta_set: usize,
    last_table_idx: usize,
    delta_tables:   DeltaTables,
    blk_w:          usize,
    blk_h:          usize,
    vert_pred:      Vec<u32>,
    lastframe:      FrameBuf,
}

impl TM1Decoder {
    fn new() -> Self { Self::default() }
    fn set_delta_tables(&mut self, delta_set: usize, table_idx: usize, is_24bit: bool) {
        if (self.last_delta_set == delta_set) && (self.last_table_idx == table_idx) { return; }
        let ydt  = &DUCK_Y_DELTAS[delta_set];
        let yfdt = DUCK_Y_FAT_DELTAS[delta_set];
        let cdt  = &DUCK_C_DELTAS[delta_set];
        let cfdt = DUCK_C_FAT_DELTAS[delta_set];
        let vec  = DUCK_VECTABLES[table_idx - 1];

        let mut vec_iter = vec.iter();
        for i in 0..256 {
            let len = (*vec_iter.next().unwrap() as usize) >> 1;
            for j in 0..len {
                let pair = vec_iter.next().unwrap();
                let lo = (pair >> 4) as usize;
                let hi = (pair & 0xF) as usize;
                if !is_24bit {
                    let d_lo = ydt[lo] + (ydt[lo] << 5) + (ydt[lo] << 10);
                    let d_hi = ydt[hi] + (ydt[hi] << 5) + (ydt[hi] << 10);
                    self.delta_tables.ydt[i][j] = ((d_lo + (d_hi << 16)) << 1) as u32;
                    let d_c = cdt[hi] + (cdt[lo] << 10);
                    self.delta_tables.cdt[i][j] = ((d_c + (d_c << 16)) << 1) as u32;
                    let d_a = lo + hi * 5;
                    self.delta_tables.adt[i][j] = ((d_a << 16) << 1) as u32;
                } else {
                    self.delta_tables.ydt[i][j]     = ((ydt [lo] + (ydt [hi] << 8) + (ydt [hi] << 16)) << 1) as u32;
                    self.delta_tables.fat_ydt[i][j] = ((yfdt[lo] + (yfdt[hi] << 8) + (yfdt[hi] << 16)) << 1) as u32;
                    self.delta_tables.cdt[i][j]     = ((cdt [hi] + (cdt [lo] << 16)) << 1) as u32;
                    self.delta_tables.fat_cdt[i][j] = ((cfdt[hi] + (cfdt[lo] << 16)) << 1) as u32;
                }
            }
            self.delta_tables.ydt[i][len - 1] |= 1;
            self.delta_tables.cdt[i][len - 1] |= 1;
            self.delta_tables.adt[i][len - 1] |= 1;
            self.delta_tables.fat_ydt[i][len - 1] |= 1;
            self.delta_tables.fat_cdt[i][len - 1] |= 1;
        }

        self.last_delta_set = delta_set;
        self.last_table_idx = table_idx;
    }
    fn decode_16bit(&mut self, dst: &mut [u16], stride: usize, width: usize, height: usize, mask: &mut MaskState<'_>, index: &mut IndexState<'_>) -> DecoderResult<()> {
        let mut off = 0;
        index.get_next()?;
        for y in 0..height {
            let mut hor_pred: u32 = 0;
            for x in (0..width).step_by(4) {
                if mask.get_next() {
                    match y & 3 {
                        0 => {
                            let dc0 = index.get_diff16(&self.delta_tables.cdt)?;
                            let dy0 = index.get_diff16(&self.delta_tables.ydt)?;
                            hor_pred = hor_pred.wrapping_add(dc0);
                            hor_pred = hor_pred.wrapping_add(dy0);
                            let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 0]);
                            self.vert_pred[(x >> 1) + 0] = cur;
                            dst[off + x + 0] = cur as u16;
                            dst[off + x + 1] = (cur >> 16) as u16;
                            if self.blk_w == 2 {
                                let dc1 = index.get_diff16(&self.delta_tables.cdt)?;
                                hor_pred = hor_pred.wrapping_add(dc1);
                            }
                            let dy1 = index.get_diff16(&self.delta_tables.ydt)?;
                            hor_pred = hor_pred.wrapping_add(dy1);
                            let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 1]);
                            self.vert_pred[(x >> 1) + 1] = cur;
                            dst[off + x + 2] = cur as u16;
                            dst[off + x + 3] = (cur >> 16) as u16;
                        },
                        1 | 3 => {
                            let dy0 = index.get_diff16(&self.delta_tables.ydt)?;
                            hor_pred = hor_pred.wrapping_add(dy0);
                            let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 0]);
                            self.vert_pred[(x >> 1) + 0] = cur;
                            dst[off + x + 0] = cur as u16;
                            dst[off + x + 1] = (cur >> 16) as u16;
                            let dy1 = index.get_diff16(&self.delta_tables.ydt)?;
                            hor_pred = hor_pred.wrapping_add(dy1);
                            let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 1]);
                            self.vert_pred[(x >> 1) + 1] = cur;
                            dst[off + x + 2] = cur as u16;
                            dst[off + x + 3] = (cur >> 16) as u16;
                        },
                        2 => {
                            if self.blk_h == 2 {
                                let dc0 = index.get_diff16(&self.delta_tables.cdt)?;
                                let dy0 = index.get_diff16(&self.delta_tables.ydt)?;
                                hor_pred = hor_pred.wrapping_add(dc0);
                                hor_pred = hor_pred.wrapping_add(dy0);
                                let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 0]);
                                self.vert_pred[(x >> 1) + 0] = cur;
                                dst[off + x + 0] = cur as u16;
                                dst[off + x + 1] = (cur >> 16) as u16;
                                if self.blk_w == 2 {
                                    let dc1 = index.get_diff16(&self.delta_tables.cdt)?;
                                    hor_pred = hor_pred.wrapping_add(dc1);
                                }
                                let dy1 = index.get_diff16(&self.delta_tables.ydt)?;
                                hor_pred = hor_pred.wrapping_add(dy1);
                                let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 1]);
                                self.vert_pred[(x >> 1) + 1] = cur;
                                dst[off + x + 2] = cur as u16;
                                dst[off + x + 3] = (cur >> 16) as u16;
                            } else {
                                let dy0 = index.get_diff16(&self.delta_tables.ydt)?;
                                hor_pred = hor_pred.wrapping_add(dy0);
                                let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 0]);
                                self.vert_pred[(x >> 1) + 0] = cur;
                                dst[off + x + 0] = cur as u16;
                                dst[off + x + 1] = (cur >> 16) as u16;
                                let dy1 = index.get_diff16(&self.delta_tables.ydt)?;
                                hor_pred = hor_pred.wrapping_add(dy1);
                                let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 1]);
                                self.vert_pred[(x >> 1) + 1] = cur;
                                dst[off + x + 2] = cur as u16;
                                dst[off + x + 3] = (cur >> 16) as u16;
                            }
                        },
                        _ => unreachable!(),
                    };
                } else {
                    let cur = u32::from(dst[off + x + 0]) | (u32::from(dst[off + x + 1]) << 16);
                    self.vert_pred[(x >> 1) + 0] = cur;
                    let cur = u32::from(dst[off + x + 2]) | (u32::from(dst[off + x + 3]) << 16);
                    hor_pred = cur.wrapping_sub(self.vert_pred[(x >> 1) + 1]);
                    self.vert_pred[(x >> 1) + 1] = cur;
                }
            }
            if (y & 3) != 3 {
                mask.reset_row();
            } else {
                mask.next_row();
            }
            off += stride;
        }
        Ok(())
    }
    fn decode_sprite(&mut self, dst: &mut [u16], stride: usize, width: usize, height: usize, mask: &mut MaskState<'_>, index: &mut IndexState<'_>) -> DecoderResult<()> {
        let mut off = 0;
        let _ = index.get_next();
        for y in 0..height {
            let mut hor_pred: u32 = 0;
            for x in (0..width).step_by(4) {
                let is_tm = !mask.get_next();
                let is_sprite = !mask.get_next();
                if is_tm {
                    if (y & 3) == 0 {
                        let dc0 = index.get_diff16(&self.delta_tables.cdt)?;
                        hor_pred = hor_pred.wrapping_add(dc0);
                    }
                    let dy0 = index.get_diff16(&self.delta_tables.ydt)?;
                    hor_pred = hor_pred.wrapping_add(dy0);
                    let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 0]);
                    self.vert_pred[(x >> 1) + 0] = cur;
                    dst[off + x + 0] = cur as u16;
                    dst[off + x + 1] = (cur >> 16) as u16;
                    let dy1 = index.get_diff16(&self.delta_tables.ydt)?;
                    hor_pred = hor_pred.wrapping_add(dy1);
                    let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 1]);
                    self.vert_pred[(x >> 1) + 1] = cur;
                    dst[off + x + 2] = cur as u16;
                    dst[off + x + 3] = (cur >> 16) as u16;
                } else if is_sprite {
                    if (y & 3) == 0 {
                        let dc0 = index.get_diff16(&self.delta_tables.cdt)?;
                        hor_pred = hor_pred.wrapping_add(dc0);
                    }
                    let dy0 = index.get_diff16(&self.delta_tables.ydt)?;
                    hor_pred = hor_pred.wrapping_add(dy0);
                    let _da0 = index.get_diff16_noesc(&self.delta_tables.adt)?;
                    let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 0]);
                    self.vert_pred[(x >> 1) + 0] = cur;
                    dst[off + x + 0] = cur as u16;
                    dst[off + x + 1] = (cur >> 16) as u16;
                    let dy1 = index.get_diff16(&self.delta_tables.ydt)?;
                    hor_pred = hor_pred.wrapping_add(dy1);
                    let _da1 = index.get_diff16_noesc(&self.delta_tables.adt)?;
                    let cur = hor_pred.wrapping_add(self.vert_pred[(x >> 1) + 1]);
                    self.vert_pred[(x >> 1) + 1] = cur;
                    dst[off + x + 2] = cur as u16;
                    dst[off + x + 3] = (cur >> 16) as u16;
                } else {
                    hor_pred = 0;
                    dst[off + x + 0] = 0;
                    dst[off + x + 1] = 0;
                    dst[off + x + 2] = 0;
                    dst[off + x + 3] = 0;
                    self.vert_pred[(x >> 1) + 0] = 0;
                    self.vert_pred[(x >> 1) + 1] = 0;
                }
            }
            if (y & 3) != 3 {
                mask.reset_row();
            } else {
                mask.next_row();
            }
            off += stride;
        }
        Ok(())
    }
    fn decode_24bit(&mut self, dst: &mut [u8], stride: usize, width: usize, height: usize, mask: &mut MaskState<'_>, index: &mut IndexState<'_>) -> DecoderResult<()> {
        let mut off = 0;
        index.get_next()?;
        for y in 0..height {
            let mut hor_pred: u32 = 0;
            for x in (0..width).step_by(2) {
                if mask.get_next() {
                    match y & 3 {
                        0 => {
                            let dc0 = index.get_diff24(&self.delta_tables.cdt, &self.delta_tables.fat_cdt)?;
                            let dy0 = index.get_diff24(&self.delta_tables.ydt, &self.delta_tables.fat_ydt)?;
                            hor_pred = hor_pred.wrapping_add(dc0);
                            hor_pred = hor_pred.wrapping_add(dy0);
                            let cur = hor_pred.wrapping_add(self.vert_pred[x + 0]);
                            self.vert_pred[x + 0] = cur;
                            dst[off + x*4 + 0] = cur as u8;
                            dst[off + x*4 + 1] = (cur >> 8) as u8;
                            dst[off + x*4 + 2] = (cur >> 16) as u8;
                            dst[off + x*4 + 3] = 0;
                            if self.blk_w == 2 {
                                let dc1 = index.get_diff24(&self.delta_tables.cdt, &self.delta_tables.fat_cdt)?;
                                hor_pred = hor_pred.wrapping_add(dc1);
                            }
                            let dy1 = index.get_diff24(&self.delta_tables.ydt, &self.delta_tables.fat_ydt)?;
                            hor_pred = hor_pred.wrapping_add(dy1);
                            let cur = hor_pred.wrapping_add(self.vert_pred[x + 1]);
                            self.vert_pred[x + 1] = cur;
                            dst[off + x*4 + 4] = cur as u8;
                            dst[off + x*4 + 5] = (cur >> 8) as u8;
                            dst[off + x*4 + 6] = (cur >> 16) as u8;
                            dst[off + x*4 + 7] = 0;
                        },
                        1 | 3 => {
                            let dy0 = index.get_diff24(&self.delta_tables.ydt, &self.delta_tables.fat_ydt)?;
                            hor_pred = hor_pred.wrapping_add(dy0);
                            let cur = hor_pred.wrapping_add(self.vert_pred[x + 0]);
                            self.vert_pred[x + 0] = cur;
                            dst[off + x*4 + 0] = cur as u8;
                            dst[off + x*4 + 1] = (cur >> 8) as u8;
                            dst[off + x*4 + 2] = (cur >> 16) as u8;
                            dst[off + x*4 + 3] = 0;
                            let dy1 = index.get_diff24(&self.delta_tables.ydt, &self.delta_tables.fat_ydt)?;
                            hor_pred = hor_pred.wrapping_add(dy1);
                            let cur = hor_pred.wrapping_add(self.vert_pred[x + 1]);
                            self.vert_pred[x + 1] = cur;
                            dst[off + x*4 + 4] = cur as u8;
                            dst[off + x*4 + 5] = (cur >> 8) as u8;
                            dst[off + x*4 + 6] = (cur >> 16) as u8;
                            dst[off + x*4 + 7] = 0;
                        },
                        2 => {
                            if self.blk_h == 2 {
                                let dc0 = index.get_diff24(&self.delta_tables.cdt, &self.delta_tables.fat_cdt)?;
                                let dy0 = index.get_diff24(&self.delta_tables.ydt, &self.delta_tables.fat_ydt)?;
                                hor_pred = hor_pred.wrapping_add(dc0);
                                hor_pred = hor_pred.wrapping_add(dy0);
                                let cur = hor_pred.wrapping_add(self.vert_pred[x + 0]);
                                self.vert_pred[x + 0] = cur;
                                dst[off + x*4 + 0] = cur as u8;
                                dst[off + x*4 + 1] = (cur >> 8) as u8;
                                dst[off + x*4 + 2] = (cur >> 16) as u8;
                                dst[off + x*4 + 3] = 0;
                                if self.blk_w == 2 {
                                    let dc1 = index.get_diff24(&self.delta_tables.cdt, &self.delta_tables.fat_cdt)?;
                                    hor_pred = hor_pred.wrapping_add(dc1);
                                }
                                let dy1 = index.get_diff24(&self.delta_tables.ydt, &self.delta_tables.fat_ydt)?;
                                hor_pred = hor_pred.wrapping_add(dy1);
                                let cur = hor_pred.wrapping_add(self.vert_pred[x + 1]);
                                self.vert_pred[x + 1] = cur;
                                dst[off + x*4 + 4] = cur as u8;
                                dst[off + x*4 + 5] = (cur >> 8) as u8;
                                dst[off + x*4 + 6] = (cur >> 16) as u8;
                                dst[off + x*4 + 7] = 0;
                            } else {
                                let dy0 = index.get_diff24(&self.delta_tables.ydt, &self.delta_tables.fat_ydt)?;
                                hor_pred = hor_pred.wrapping_add(dy0);
                                let cur = hor_pred.wrapping_add(self.vert_pred[x + 0]);
                                self.vert_pred[x + 0] = cur;
                                dst[off + x*4 + 0] = cur as u8;
                                dst[off + x*4 + 1] = (cur >> 8) as u8;
                                dst[off + x*4 + 2] = (cur >> 16) as u8;
                                dst[off + x*4 + 3] = 0;
                                let dy1 = index.get_diff24(&self.delta_tables.ydt, &self.delta_tables.fat_ydt)?;
                                hor_pred = hor_pred.wrapping_add(dy1);
                                let cur = hor_pred.wrapping_add(self.vert_pred[x + 1]);
                                self.vert_pred[x + 1] = cur;
                                dst[off + x*4 + 4] = cur as u8;
                                dst[off + x*4 + 5] = (cur >> 8) as u8;
                                dst[off + x*4 + 6] = (cur >> 16) as u8;
                                dst[off + x*4 + 7] = 0;
                            }
                        },
                        _ => unreachable!(),
                    };
                } else {
                    let cur =   u32::from(dst[off + x*4 + 0])
                             | (u32::from(dst[off + x*4 + 1]) << 8)
                             | (u32::from(dst[off + x*4 + 2]) << 16)
                             | (u32::from(dst[off + x*4 + 3]) << 24);
                    self.vert_pred[x + 0] = cur;
                    let cur =   u32::from(dst[off + x*4 + 4])
                             | (u32::from(dst[off + x*4 + 5]) << 8)
                             | (u32::from(dst[off + x*4 + 6]) << 16)
                             | (u32::from(dst[off + x*4 + 7]) << 24);
                    hor_pred = cur.wrapping_sub(self.vert_pred[x + 1]);
                    self.vert_pred[x + 1] = cur;
                }
            }
            if (y & 3) != 3 {
                mask.reset_row();
            } else {
                mask.next_row();
            }
            off += stride;
        }
        Ok(())
    }
}

impl NADecoder for TM1Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, YUV410_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 10);
        let hdr_size = (src[0].rotate_left(3) & 0x7F) as usize;
        validate!(hdr_size >= 12 && hdr_size < src.len());
        let mut hdr: [u8; 127] = [0; 127];
        for i in 1..hdr_size {
            hdr[i - 1] = src[i] ^ src[i + 1];
        }
        let mut mr = MemoryReader::new_read(&hdr[0..hdr_size-1]);
        let mut br = ByteReader::new(&mut mr);

        let tm1type                             = br.read_byte()? as usize;
        let delta_set                           = br.read_byte()? as usize;
        let table_idx                           = br.read_byte()? as usize;
        let height                              = br.read_u16le()? as usize;
        let width                               = br.read_u16le()? as usize;
        let _frameno                            = br.read_u16le()? as usize;
        let version                             = br.read_byte()?;
        let meta_type                           = br.read_byte()?;
        validate!(width > 0 && height > 0);
        let is_intra;
        let mut is_sprite = false;
        let mut spr_xoff = 0;
        let mut spr_yoff = 0;
        let mut spr_width = 0;
        let mut spr_height = 0;
        if version >= 2 {
            validate!(meta_type <= 3);
            if meta_type >= 2 {
                let frameinfo                   = br.read_byte()?;
                let _control                    = br.read_byte()?;

                is_intra = ((frameinfo & 0x10) != 0) || ((frameinfo & 0x08) == 0);
            } else {
                is_intra = true;
            }
            if meta_type == 3 {
                spr_xoff                        = br.read_u16le()? as usize;
                spr_yoff                        = br.read_u16le()? as usize;
                spr_width                       = br.read_u16le()? as usize;
                spr_height                      = br.read_u16le()? as usize;
                is_sprite = true;
            }
        } else {
            is_intra = true;
        }
        validate!(tm1type < TM1_COMPR_TYPES.len());
        let cinfo = TM1_COMPR_TYPES[tm1type];
        if cinfo.is_none() {
//check for missing ref
            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::None);
            frm.set_keyframe(false);
            frm.set_frame_type(FrameType::Skip);
            return Ok(frm.into_ref());
        }
        let compr_info = cinfo.unwrap();
        let is_24bit = !is_sprite && compr_info.is_24bit;

        let vec_idx = if ((tm1type & 1) != 0) && (meta_type > 0) { 1 } else { table_idx };
        validate!((delta_set < DUCK_Y_DELTAS.len()) && (vec_idx > 0) && (vec_idx <= DUCK_VECTABLES.len()));
        self.set_delta_tables(delta_set, vec_idx, is_24bit);

        let out_width = if is_24bit { width >> 1 } else { width };
        let mask_row_size = if is_sprite {
                ((spr_width >> 2) + 3) >> 2
            } else if is_intra {
                0
            } else {
                ((width >> 2) + 7) >> 3
            };
        let mask_size = mask_row_size * (if is_sprite { spr_height >> 2 } else { height >> 2 });
        let mask_bits = &src[hdr_size..][..mask_size];
        let index_bytes = &src[hdr_size+mask_size..];
        validate!(src.len() >= hdr_size + mask_size);
        self.vert_pred.clear();
        self.vert_pred.resize(out_width, 0);

        if is_intra || is_sprite {
            let fmt = if is_24bit { BGR0_FORMAT } else { RGB555_FORMAT };
            let myinfo = NAVideoInfo::new(out_width, height, false, fmt);
            let bufinfo = alloc_video_buffer(myinfo, 2)?;
            self.lastframe.reset();
            if !is_24bit {
                self.lastframe.set16(bufinfo.get_vbuf16().unwrap());
            } else {
                self.lastframe.set24(bufinfo.get_vbuf().unwrap());
            }
        }

        self.blk_w = compr_info.block_w;
        self.blk_h = compr_info.block_h;
        let mut mask = MaskState::new(is_intra && !is_sprite, mask_bits, mask_row_size);
        let mut index = IndexState::new(index_bytes);
        let bufinfo;
        if !is_24bit {
            if let Some(mut buf) = self.lastframe.get16() {
                let stride = buf.get_stride(0);
                {
                    let data = buf.get_data_mut().unwrap();
                    if !is_sprite {
                        self.decode_16bit(data.as_mut_slice(), stride, out_width, height, &mut mask, &mut index)?;
                    } else {
                        validate!(spr_xoff + spr_width <= out_width);
                        validate!(spr_yoff + spr_height <= height);
                        for el in data.iter_mut() { *el = 0; }
                        let dst = &mut data[spr_xoff + spr_yoff * stride..];
                        self.decode_sprite(dst, stride, spr_width, spr_height, &mut mask, &mut index)?;
                    }
                }
                bufinfo = NABufferType::Video16(buf);
            } else {
                return Err(DecoderError::MissingReference);
            }
        } else if let Some(mut buf) = self.lastframe.get24() {
            let stride = buf.get_stride(0);
            {
                let data = buf.get_data_mut().unwrap();
                self.decode_24bit(data.as_mut_slice(), stride, out_width, height, &mut mask, &mut index)?;
            }
            bufinfo = NABufferType::VideoPacked(buf);
        } else {
            return Err(DecoderError::MissingReference);
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra || is_sprite);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.lastframe.reset();
    }
}

impl NAOptionHandler for TM1Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(TM1Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_tm1() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/DUCK/phant2-940.duk
        test_decoding("avi", "truemotion1", "assets/Duck/phant2-940.duk", Some(12), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                        [0x989e62b8, 0x5d85c23c, 0x1cffba6d, 0xe599f1c4],
                        [0xc4231321, 0x25561487, 0x9db11f57, 0x4faeb9a5],
                        [0x36e3a831, 0xdbd21f89, 0x0a446071, 0xf6d31ee7],
                        [0x0af640af, 0x64bc2bac, 0x0e95dd72, 0x9e55360b],
                        [0xbc9c5f8b, 0x6c06f2bc, 0x216f4129, 0x3a421337],
                        [0xd8ea7297, 0xce5f79fc, 0x46071f4c, 0xaed7fb7a],
                        [0x87617060, 0x72ce8df8, 0xde42eaa6, 0x804a6f45],
                        [0xfd8c45b3, 0xf424b683, 0xb4d6a9bd, 0xc622d0b9],
                        [0x6c233746, 0xba8ed68e, 0xc0ed0e85, 0xc99e1dc0],
                        [0x5842aac0, 0xd3d78242, 0x5da21218, 0xea1ed0ad],
                        [0xdea0db20, 0xe2ce3586, 0xf7386649, 0xecc374f9],
                        [0xb80ae9cb, 0x04eb938e, 0xd8a337ee, 0x0054b5ed],
                        [0xf8b80e1d, 0xd8eb3d6c, 0xa99b23ff, 0x562851a1]]));
        test_decoding("avi", "truemotion1", "assets/Duck/SPRITES.AVI", Some(2), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xb89a4275, 0xf9797f5f, 0xe53c1ccd, 0xfa163e02]));
        //let file = "assets/Duck/AVI-DUCK-dk3.duk";
        //let file = "assets/Duck/phant2-940.duk";
        //let file = "assets/Duck/bugsampler-m01-16bit.avi";
        //let file = "assets/Duck/sonic3dblast_intro.avi";
        //let file = "assets/Duck/BUTTONS.AVI";
        //let file = "assets/Duck/SPRITES.AVI";
        //let file = "assets/Duck/TRICORD.AVI";
        //test_file_decoding("avi", file, Some(42), true, false, None/*Some("tm1-")*/, &dmx_reg, &dec_reg);
    }
}
