use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

#[derive(Clone,Copy,Debug,PartialEq)]
enum TileMode {
    Start,
    Fill,
    ShortPattern(u8, u8),
    LongPattern(u8, u8),
    Run,
    Reuse,
    FB,
    MV,
    Forward(u16),
    Backward(u16),
    Skip,
}

struct QVideoDecoder {
    info:       NACodecInfoRef,
    pal:        [u8; 768],
    frame:      Vec<u8>,
    w:          usize,
    h:          usize,
    tile_w:     usize,
    tile_h:     usize,
    tile_off:   Vec<usize>,
    mode:       u8,
    patterns:   [u16; 128],
    version:    u8,
    f8_cache:   [[u8; 16]; 240],
}

macro_rules! copy_tile {
    ($self: expr, $doff: expr, $soff: expr) => {
        let mut doff = $doff;
        let mut soff = $soff;
        for _y in 0..$self.tile_h {
            for x in 0..$self.tile_w {
                $self.frame[doff + x] = $self.frame[soff + x];
            }
            doff += $self.w;
            soff += $self.w;
        }
    }
}

impl QVideoDecoder {
    fn new() -> Self {
        QVideoDecoder {
            info:       NACodecInfoRef::default(),
            pal:        [0; 768],
            frame:      Vec::new(),
            w:          0,
            h:          0,
            tile_off:   Vec::new(),
            mode:       0,
            tile_w:     0,
            tile_h:     0,
            patterns:   [0; 128],
            version:    0,
            f8_cache:   [[0; 16]; 240],
        }
    }

    fn decode_mode7_tile(dst: &mut [u8], stride: usize, br: &mut ByteReader) -> DecoderResult<()> {
        let op                          = br.peek_byte()?;
        if op < 0xF8 {
            for dline in dst.chunks_mut(stride).take(4) {
                                          br.read_buf(&mut dline[..4])?;
            }
        } else if op == 0xF8 || op == 0xFF {
                                          br.read_byte()?;
        } else {
                                          br.read_byte()?;
            let mut clr = [0; 8];
            let nclr = (op - 0xF6) as usize;
            if nclr <= 4 {
                let mut pattern         = br.read_u32le()?;
                                          br.read_buf(&mut clr[..nclr])?;
                for dline in dst.chunks_mut(stride).take(4) {
                    for el in dline[..4].iter_mut() {
                        *el = clr[(pattern & 3) as usize];
                        pattern >>= 2;
                    }
                }
            } else {
                let mut pattern         = br.read_u24le()?;
                let     pattern2        = br.read_u24le()?;
                                          br.read_buf(&mut clr[..nclr])?;
                for (y, dline) in dst.chunks_mut(stride).take(4).enumerate() {
                    for el in dline[..4].iter_mut() {
                        *el = clr[(pattern & 7) as usize];
                        pattern >>= 3;
                    }
                    if y == 1 {
                        pattern = pattern2;
                    }
                }
            }
        }
        Ok(())
    }

    fn decode_frame_v3(&mut self, br: &mut ByteReader, _ctype: u16) -> DecoderResult<()> {
        let mut titer = self.tile_off.iter().enumerate();
        let mut skip_mode = false;
        while let Some((tile_no, &tile_off)) = titer.next()  {
            let op                      = br.read_byte()?;
            if op < 0xF8 {
                let clr0 = op;
                let clr1                = br.read_byte()?;
                if clr0 == clr1 {
                    for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                        for el in dline[..self.tile_w].iter_mut() {
                            *el = clr0;
                        }
                    }
                } else {
                    let mut pattern     = br.read_u16le()?;
                    for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                        for el in dline[..self.tile_w].iter_mut() {
                            *el = if (pattern & 0x8000) == 0 { clr0 } else { clr1 };
                            pattern <<= 1;
                        }
                    }
                }
            } else {
                match op {
                    0xF8 => {
                        unimplemented!();
                    },
                    0xF9 => {
                        let run         = br.read_byte()? as usize;
                        validate!(run > 0);

                        validate!(tile_no > 0);
                        let mut tile_off = tile_off;
                        for i in 0..run {
                            if !skip_mode {
                                copy_tile!(self, tile_off, self.tile_off[tile_no - 1]);
                            }
                            if i + 1 < run {
                                let (_tno, &toff) = titer.next().unwrap();
                                tile_off = toff;
                            }
                        }
                    },
                    0xFA => {
                        let off         = br.read_u16le()? as usize;
                        validate!(tile_no + off < self.tile_off.len());
                        copy_tile!(self, tile_off, self.tile_off[tile_no + off]);
                    },
                    0xFB => {
                        let off         = br.read_u16le()? as usize;
                        validate!(off <= tile_no);
                        copy_tile!(self, tile_off, self.tile_off[tile_no - off]);
                    },
                    0xFC => {
                        const MV_PART: [i8; 16] = [ 0, 4, 8, 12, 16, 20, 24, 28, -32, -4, -8, -12, -16, -20, -24, -28 ];

                        let idx         = br.read_byte()? as usize;
                        let x = MV_PART[idx & 0xF] as isize;
                        let y = MV_PART[idx >>  4] as isize;
                        let src_off = (tile_off as isize) + x + y * (self.w as isize);
                        validate!(src_off >= 0);
                        validate!((src_off as usize) + self.tile_w + (self.tile_h - 1) * self.w <= self.w * self.h);

                        copy_tile!(self, tile_off, src_off as usize);
                    },
                    0xFD => {
                        let off         = (br.read_byte()? as usize) + 1;
                        validate!(tile_no + off < self.tile_off.len());
                        copy_tile!(self, tile_off, self.tile_off[tile_no + off]);
                    },
                    0xFE => {
                        let off         = (br.read_byte()? as usize) + 1;
                        validate!(off <= tile_no);
                        copy_tile!(self, tile_off, self.tile_off[tile_no - off]);
                    },
                    _ => {},
                };
            }
            skip_mode = op == 0xFF;
        }

        Ok(())
    }

    fn decode_frame_5(&mut self, br: &mut ByteReader, _ctype: u16) -> DecoderResult<()> {
        let mut titer = self.tile_off.iter().enumerate();
        let mut last_mode = TileMode::Start;

        while let Some((tile_no, &tile_off)) = titer.next()  {
            let op                      = br.read_byte()?;
            if op < 0xF8 {
                let clr0 = op;
                let clr1                = br.read_byte()?;
                if clr0 == clr1 {
                    for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                        for el in dline[..self.tile_w].iter_mut() {
                            *el = clr0;
                        }
                    }
                    last_mode = TileMode::Fill;
                } else {
                    let pat             = br.read_byte()?;
                    let mut pattern = if pat < 128 {
                            last_mode = TileMode::ShortPattern(clr0, clr1);
                            self.patterns[pat as usize]
                        } else {
                            last_mode = TileMode::LongPattern(clr0, clr1);
                            u16::from(pat) | (u16::from(br.read_byte()?) << 8)
                        };
                    for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                        for el in dline[..self.tile_w].iter_mut() {
                            *el = if (pattern & 0x8000) == 0 { clr0 } else { clr1 };
                            pattern <<= 1;
                        }
                    }
                }
            } else {
                match op {
                    0xF8 => {
                        unimplemented!();
                    },
                    0xF9 => {
                        let run         = br.read_byte()? as usize;
                        validate!(run > 0);

                        validate!(tile_no > 0);
                        validate!(last_mode != TileMode::Start);
                        let mut tile_no  = tile_no;
                        let mut tile_off = tile_off;
                        for i in 0..run {
                            let copy_off = match last_mode {
                                    TileMode::Forward(off) => {
                                        tile_no + (off as usize)
                                    },
                                    TileMode::Backward(off) => {
                                        validate!(tile_no >= (off as usize));
                                        tile_no - (off as usize)
                                    },
                                    TileMode::Skip => self.tile_off.len(),
                                    _ => tile_no - 1,
                                };
                            if copy_off < self.tile_off.len() {
                                copy_tile!(self, tile_off, self.tile_off[copy_off]);
                            }
                            if i + 1 < run {
                                let (tno, &toff) = titer.next().unwrap();
                                tile_no  = tno;
                                tile_off = toff;
                            }
                        }
                        last_mode = TileMode::Run;
                    },
                    0xFA => {
                        let rtile       = br.read_u16le()? as usize;
                        validate!(rtile < self.tile_off.len());
                        copy_tile!(self, tile_off, self.tile_off[rtile]);
                        last_mode = TileMode::Reuse;
                    },
                    0xFB => {
                        match self.mode {
                            6 => {
                                let run = br.read_byte()? as usize;
                                validate!(run >= 2);
                                let mut tile_no  = tile_no;
                                let mut tile_off = tile_off;
                                for i in 0..run {
                                    match last_mode {
                                        TileMode::Start => return Err(DecoderError::InvalidData),
                                        TileMode::Fill => {
                                            let clr = br.read_byte()?;
                                            for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                                                for el in dline[..self.tile_w].iter_mut() {
                                                    *el = clr;
                                                }
                                            }
                                        },
                                        TileMode::ShortPattern(clr0, clr1) => {
                                            let pat = br.read_byte()?;
                                            let mut pattern = if pat < 128 {
                                                    self.patterns[pat as usize]
                                                } else {
                                                    u16::from(pat) | (u16::from(br.read_byte()?) << 8)
                                                };
                                            for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                                                for el in dline[..self.tile_w].iter_mut() {
                                                    *el = if (pattern & 0x8000) == 0 { clr0 } else { clr1 };
                                                    pattern <<= 1;
                                                }
                                            }
                                        },
                                        TileMode::LongPattern(clr0, clr1) => {
                                            let mut pattern = br.read_u16le()?;
                                            for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                                                for el in dline[..self.tile_w].iter_mut() {
                                                    *el = if (pattern & 0x8000) == 0 { clr0 } else { clr1 };
                                                    pattern <<= 1;
                                                }
                                            }
                                        },
                                        TileMode::Reuse => {
                                            let rtile       = br.read_u16le()? as usize;
                                            validate!(rtile < self.tile_off.len());
                                            copy_tile!(self, tile_off, self.tile_off[rtile]);
                                        },
                                        TileMode::MV => {
                                            let idx         = br.read_byte()? as usize;
                                            let (x, y) = DEF_MVS[idx];
                                            let src_off = (tile_off as isize) + (x as isize) * 4 + (y as isize) * 4 * (self.w as isize);
                                            validate!(src_off >= 0);
                                            validate!((src_off as usize) + self.tile_w + (self.tile_h - 1) * self.w <= self.w * self.h);
                                            copy_tile!(self, tile_off, src_off as usize);
                                        },
                                        TileMode::Forward(_) => {
                                            let off         = (br.read_byte()? as usize) + 1;
                                            validate!(tile_no + off < self.tile_off.len());
                                            copy_tile!(self, tile_off, self.tile_off[tile_no + off]);
                                        },
                                        TileMode::Backward(_) => {
                                            let off         = (br.read_byte()? as usize) + 1;
                                            validate!(off <= tile_no);
                                            copy_tile!(self, tile_off, self.tile_off[tile_no - off]);
                                        },
                                        _ => unimplemented!(),
                                    };

                                    if i + 1 < run {
                                        let (tno, &toff) = titer.next().unwrap();
                                        tile_no  = tno;
                                        tile_off = toff;
                                    }
                                }
                            },
                            7 => {
                                validate!(self.tile_w == 4 && self.tile_h == 4);
                                let run = br.read_byte()? as usize;
                                validate!(run > 0);

                                let mut tile_off = tile_off;
                                for i in 0..run {
                                    Self::decode_mode7_tile(&mut self.frame[tile_off..], self.w, br)?;

                                    if i + 1 < run {
                                        let (_tno, &toff) = titer.next().unwrap();
                                        tile_off = toff;
                                    }
                                }
                            },
                            _ => {
                                unimplemented!();
                            },
                        };
                        last_mode = TileMode::FB;
                    },
                    0xFC => {
                        let idx         = br.read_byte()? as usize;
                        let (x, y) = DEF_MVS[idx];
                        let src_off = (tile_off as isize) + (x as isize) * 4 + (y as isize) * 4 * (self.w as isize);
                        validate!(src_off >= 0);
                        validate!((src_off as usize) + self.tile_w + (self.tile_h - 1) * self.w <= self.w * self.h);

                        copy_tile!(self, tile_off, src_off as usize);
                        last_mode = TileMode::MV;
                    },
                    0xFD => {
                        let off         = (br.read_byte()? as usize) + 1;
                        validate!(tile_no + off < self.tile_off.len());
                        copy_tile!(self, tile_off, self.tile_off[tile_no + off]);
                        last_mode = TileMode::Forward(off as u16);
                    },
                    0xFE => {
                        let off         = (br.read_byte()? as usize) + 1;
                        validate!(off <= tile_no);
                        copy_tile!(self, tile_off, self.tile_off[tile_no - off]);
                        last_mode = TileMode::Backward(off as u16);
                    },
                    _ => {
                        last_mode = TileMode::Skip;
                    },
                };
            }
        }

        Ok(())
    }
    fn decode_frame_7(&mut self, br: &mut ByteReader, _ctype: u16) -> DecoderResult<()> {
        let mut titer = self.tile_off.iter().enumerate();
        let mut last_mode = TileMode::Start;

        let mut f8_mode = false;
        let row_size = self.w / self.tile_w;
        let mut next_row = 0;
        let mut f8_data = [0; 16];
        let mut f8_pos = 0;

        while let Some((tile_no, &tile_off)) = titer.next()  {
            if tile_no == next_row {
                f8_mode = false;
                next_row += row_size;
            }
            while br.peek_byte()? == 0xF8 {
                                          br.read_byte()?;
                if f8_mode {
                    f8_mode = false;
                } else {
                    let idx             = br.read_byte()? as usize;
                    if idx < 0x10 {
                        validate!(f8_pos < self.f8_cache.len());
                                          br.peek_buf(&mut self.f8_cache[f8_pos])?;
                        if idx > 0 {
                                          br.read_skip(idx)?;
                        }
                        f8_data = self.f8_cache[f8_pos];
                    } else {
                        f8_data = self.f8_cache[idx - 0x10];
                        self.f8_cache[f8_pos] = f8_data;
                    }
                    f8_pos += 1;
                    f8_mode = true;
                }
            }

            let op                      = br.read_byte()?;
            if op < 0xF8 {
                let (clr0, clr1) = if !f8_mode {
                        if br.peek_byte()? < 0xF8 {
                            (op, br.read_byte()?)
                        } else {
                            (op, op)
                        }
                    } else {
                        (f8_data[(op & 0xF) as usize], f8_data[(op >> 4) as usize])
                    };
                if clr0 == clr1 && (!f8_mode || ((op & 0xF) == (op >> 4))) {
                    for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                        for el in dline[..self.tile_w].iter_mut() {
                            *el = clr0;
                        }
                    }
                    last_mode = TileMode::Fill;
                } else {
                    let pat             = br.read_byte()?;
                    let mut pattern = if pat < 128 {
                            last_mode = TileMode::ShortPattern(clr0, clr1);
                            self.patterns[pat as usize]
                        } else {
                            last_mode = TileMode::LongPattern(clr0, clr1);
                            u16::from(pat) | (u16::from(br.read_byte()?) << 8)
                        };
                    for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                        for el in dline[..self.tile_w].iter_mut() {
                            *el = if (pattern & 0x8000) == 0 { clr0 } else { clr1 };
                            pattern <<= 1;
                        }
                    }
                }
            } else {
                match op {
                    0xF8 => {
                        unreachable!();
                    },
                    0xF9 => {
                        let run         = br.read_byte()? as usize;
                        validate!(run > 0);

                        validate!(tile_no > 0);
                        validate!(last_mode != TileMode::Start);
                        let mut tile_no  = tile_no;
                        let mut tile_off = tile_off;
                        for i in 0..run {
                            let copy_off = match last_mode {
                                    TileMode::Forward(off) => {
                                        tile_no + (off as usize)
                                    },
                                    TileMode::Backward(off) => {
                                        validate!(tile_no >= (off as usize));
                                        tile_no - (off as usize)
                                    },
                                    TileMode::Skip => self.tile_off.len(),
                                    _ => tile_no - 1,
                                };
                            if copy_off < self.tile_off.len() {
                                copy_tile!(self, tile_off, self.tile_off[copy_off]);
                            }
                            if i + 1 < run {
                                let (tno, &toff) = titer.next().unwrap();
                                tile_no  = tno;
                                tile_off = toff;
                            }
                        }
                        last_mode = TileMode::Run;
                    },
                    0xFA => {
                        let rtile       = br.read_u16le()? as usize;
                        validate!(rtile < self.tile_off.len());
                        copy_tile!(self, tile_off, self.tile_off[rtile]);
                        last_mode = TileMode::Reuse;
                    },
                    0xFB => {
                        match self.mode {
                            6 => {
                                let run = br.read_byte()? as usize;
                                validate!(run >= 2);
                                let mut tile_no  = tile_no;
                                let mut tile_off = tile_off;
                                for i in 0..run {
                                    match last_mode {
                                        TileMode::Start => return Err(DecoderError::InvalidData),
                                        TileMode::Fill => {
                                            let clr = br.read_byte()?;
                                            for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                                                for el in dline[..self.tile_w].iter_mut() {
                                                    *el = clr;
                                                }
                                            }
                                        },
                                        TileMode::ShortPattern(clr0, clr1) => {
                                            let pat = br.read_byte()?;
                                            let mut pattern = if pat < 128 {
                                                    self.patterns[pat as usize]
                                                } else {
                                                    u16::from(pat) | (u16::from(br.read_byte()?) << 8)
                                                };
                                            for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                                                for el in dline[..self.tile_w].iter_mut() {
                                                    *el = if (pattern & 0x8000) == 0 { clr0 } else { clr1 };
                                                    pattern <<= 1;
                                                }
                                            }
                                        },
                                        TileMode::LongPattern(clr0, clr1) => {
                                            let mut pattern = br.read_u16le()?;
                                            for dline in self.frame[tile_off..].chunks_mut(self.w).take(self.tile_h) {
                                                for el in dline[..self.tile_w].iter_mut() {
                                                    *el = if (pattern & 0x8000) == 0 { clr0 } else { clr1 };
                                                    pattern <<= 1;
                                                }
                                            }
                                        },
                                        TileMode::Reuse => {
                                            let rtile       = br.read_u16le()? as usize;
                                            validate!(rtile < self.tile_off.len());
                                            copy_tile!(self, tile_off, self.tile_off[rtile]);
                                        },
                                        TileMode::MV => {
                                            let idx         = br.read_byte()? as usize;
                                            let (x, y) = DEF_MVS[idx];
                                            let src_off = (tile_off as isize) + (x as isize) * 4 + (y as isize) * 4 * (self.w as isize);
                                            validate!(src_off >= 0);
                                            validate!((src_off as usize) + self.tile_w + (self.tile_h - 1) * self.w <= self.w * self.h);
                                            copy_tile!(self, tile_off, src_off as usize);
                                        },
                                        TileMode::Forward(_) => {
                                            let off         = (br.read_byte()? as usize) + 1;
                                            validate!(tile_no + off < self.tile_off.len());
                                            copy_tile!(self, tile_off, self.tile_off[tile_no + off]);
                                        },
                                        TileMode::Backward(_) => {
                                            let off         = (br.read_byte()? as usize) + 1;
                                            validate!(off <= tile_no);
                                            copy_tile!(self, tile_off, self.tile_off[tile_no - off]);
                                        },
                                        _ => unimplemented!(),
                                    };

                                    if i + 1 < run {
                                        let (tno, &toff) = titer.next().unwrap();
                                        tile_no  = tno;
                                        tile_off = toff;
                                    }
                                }
                            },
                            7 => {
                                validate!(self.tile_w == 4 && self.tile_h == 4);
                                let run = br.read_byte()? as usize;
                                validate!(run > 0);

                                let mut tile_off = tile_off;
                                for i in 0..run {
                                    Self::decode_mode7_tile(&mut self.frame[tile_off..], self.w, br)?;

                                    if i + 1 < run {
                                        let (_tno, &toff) = titer.next().unwrap();
                                        tile_off = toff;
                                    }
                                }
                            },
                            _ => {
                                unimplemented!();
                            },
                        };
                        last_mode = TileMode::FB;
                    },
                    0xFC => {
                        let idx         = br.read_byte()? as usize;
                        let (x, y) = DEF_MVS[idx];
                        let src_off = (tile_off as isize) + (x as isize) * 4 + (y as isize) * 4 * (self.w as isize);
                        validate!(src_off >= 0);
                        validate!((src_off as usize) + self.tile_w + (self.tile_h - 1) * self.w <= self.w * self.h);

                        copy_tile!(self, tile_off, src_off as usize);
                        last_mode = TileMode::MV;
                    },
                    0xFD => {
                        let off         = (br.read_byte()? as usize) + 1;
                        validate!(tile_no + off < self.tile_off.len());
                        copy_tile!(self, tile_off, self.tile_off[tile_no + off]);
                        last_mode = TileMode::Forward(off as u16);
                    },
                    0xFE => {
                        let off         = (br.read_byte()? as usize) + 1;
                        validate!(off <= tile_no);
                        copy_tile!(self, tile_off, self.tile_off[tile_no - off]);
                        last_mode = TileMode::Backward(off as u16);
                    },
                    _ => {
                        last_mode = TileMode::Skip;
                    },
                };
            }
        }

        Ok(())
    }

    fn output_frame(&mut self, bufinfo: &mut NABufferType, w: usize, h: usize) {
        let bufo = bufinfo.get_vbuf();
        let mut buf = bufo.unwrap();
        let paloff = buf.get_offset(1);
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        dst[paloff..][..768].copy_from_slice(&self.pal);
        for (dline, sline) in dst.chunks_mut(stride).zip(self.frame.chunks(w)).take(h) {
            dline[..w].copy_from_slice(sline);
        }
    }
}

impl NADecoder for QVideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(_vinfo) = info.get_properties() {
            let mut w;
            let mut h;
            if let Some(buf) = info.get_extradata() {
                validate!(buf.len() >= 22);
                w = read_u16le(&buf[4..])? as usize;
                h = read_u16le(&buf[6..])? as usize;
                self.tile_w = buf[8] as usize;
                self.tile_h = buf[9] as usize;
                validate!(self.tile_w > 0 && self.tile_h > 0);
                if self.tile_w != 4 || self.tile_h != 4 {
                    return Err(DecoderError::NotImplemented);
                }
                self.version = buf[2];
                if self.version != 3{
                    w *= self.tile_w;
                    h *= self.tile_h;
                } else {
                    validate!((w % self.tile_w) == 0);
                    validate!((h % self.tile_h) == 0);
                }
            } else {
                return Err(DecoderError::InvalidData);
            }
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, false, PAL8_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.w = w;
            self.h = h;

            self.mode = match self.version {
                    4 => 6,
                    5 => 7,
                    6 => 7,
                    7 => 7,
                    _ => 0,
                };

            self.frame.resize(w * h, 0);
            self.pal = [0; 768];
            self.tile_off = Vec::with_capacity((w / self.tile_w) * (h / self.tile_h));
            let mut off = 0;
            for _y in (0..h).step_by(self.tile_h) {
                for x in (0..w).step_by(self.tile_w) {
                    self.tile_off.push(off + x);
                }
                off += w * self.tile_h;
            }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(!src.is_empty());

        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);

        while br.left() >= 6 {
            let ctype                   = br.read_u16le()?;
            let csize                   = br.read_u32le()? as usize;
            validate!(csize <= (br.left() as usize));
            match ctype {
                1 => {
                    validate!(csize <= 768);
                                          br.read_buf(&mut self.pal[..csize])?;
                    for el in self.pal[..csize].iter_mut() {
                        *el = (*el << 2) | (*el >> 4);
                    }
                },
                2 | 3 | 4 | 9 | 11 => {
                    if self.version == 5 {
                        self.mode = if ctype == 9 || ctype == 11 { 7 } else { 6 };
                    }
                    if self.version == 3 {
                        self.decode_frame_v3(&mut br, ctype)?;
                    } else if self.version < 6 {
                        self.decode_frame_5(&mut br, ctype)?;
                    } else {
                        self.mode = if ctype == 11 { 7 } else { 6 };
                        self.decode_frame_7(&mut br, ctype)?;
                    }
                },
                5 => {
                    validate!(csize <= 256 && (csize & 1) == 0);
                    for el in self.patterns[..csize/2].iter_mut() {
                        *el             = br.read_u16le()?;
                    }
                },
                6 | 7 => {
                    self.mode = ctype as u8;
                },
                _ => return Err(DecoderError::InvalidData),
            };
        }

        let mut bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;

        self.output_frame(&mut bufinfo, self.w, self.h);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_frame_type(if pkt.is_keyframe() { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for QVideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(QVideoDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;

    // samples from Callahan's Crosstime Saloon, Deathgate, Mission Critical and Shannara games
    #[test]
    fn test_q_video3() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("legend-q", "legend-q-video", "assets/Game/dgate101.q", Some(31), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x9cc0014c, 0xf6332802, 0xfabeb715, 0xdfaa11c0]));
    }
    #[test]
    fn test_q_video4() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("legend-q", "legend-q-video", "assets/Game/1925.Q", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xe1af971a, 0xfb509816, 0x9d60f5d6, 0xbcf48a3b]));
    }
    #[test]
    fn test_q_video5() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("legend-q", "legend-q-video", "assets/Game/mc703.q", Some(16), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xf65ea3ce, 0x3052b2bb, 0xb10f8f69, 0x530d60f9]));
    }
    #[test]
    fn test_q_video7() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("legend-q", "legend-q-video", "assets/Game/CCS003.Q", Some(16), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x4c0f0712, 0xc6c39f5b, 0x5bb6902f, 0x9119940e]));
    }
}

const DEF_MVS: [(i8, i8); 256] = [
    ( 0, 8), ( 1, 8), ( 2, 8), ( 3, 8), ( 4, 8), ( 5, 8), ( 6, 8), ( 7, 8),
    (-8, 8), (-1, 8), (-2, 8), (-3, 8), (-4, 8), (-5, 8), (-6, 8), (-7, 8),
    ( 0, 9), ( 1, 9), ( 2, 9), ( 3, 9), ( 4, 9), ( 5, 9), ( 6, 9), ( 7, 9),
    (-8, 9), (-1, 9), (-2, 9), (-3, 9), (-4, 9), (-5, 9), (-6, 9), (-7, 9),
    ( 0, 2), ( 1, 2), ( 2, 2), ( 3, 2), ( 4, 2), ( 5, 2), ( 6, 2), ( 7, 2),
    (-8, 2), (-1, 2), (-2, 2), (-3, 2), (-4, 2), (-5, 2), (-6, 2), (-7, 2),
    ( 0, 3), ( 1, 3), ( 2, 3), ( 3, 3), ( 4, 3), ( 5, 3), ( 6, 3), ( 7, 3),
    (-8, 3), (-1, 3), (-2, 3), (-3, 3), (-4, 3), (-5, 3), (-6, 3), (-7, 3),
    ( 0, 4), ( 1, 4), ( 2, 4), ( 3, 4), ( 4, 4), ( 5, 4), ( 6, 4), ( 7, 4),
    (-8, 4), (-1, 4), (-2, 4), (-3, 4), (-4, 4), (-5, 4), (-6, 4), (-7, 4),
    ( 0, 5), ( 1, 5), ( 2, 5), ( 3, 5), ( 4, 5), ( 5, 5), ( 6, 5), ( 7, 5),
    (-8, 5), (-1, 5), (-2, 5), (-3, 5), (-4, 5), (-5, 5), (-6, 5), (-7, 5),
    ( 0, 6), ( 1, 6), ( 2, 6), ( 3, 6), ( 4, 6), ( 5, 6), ( 6, 6), ( 7, 6),
    (-8, 6), (-1, 6), (-2, 6), (-3, 6), (-4, 6), (-5, 6), (-6, 6), (-7, 6),
    ( 0, 7), ( 1, 7), ( 2, 7), ( 3, 7), ( 4, 7), ( 5, 7), ( 6, 7), ( 7, 7),
    (-8, 7), (-1, 7), (-2, 7), (-3, 7), (-4, 7), (-5, 7), (-6, 7), (-7, 7),
    ( 0,-8), ( 1,-8), ( 2,-8), ( 3,-8), ( 4,-8), ( 5,-8), ( 6,-8), ( 7,-8),
    (-8,-8), (-1,-8), (-2,-8), (-3,-8), (-4,-8), (-5,-8), (-6,-8), (-7,-8),
    ( 0,-9), ( 1,-9), ( 2,-9), ( 3,-9), ( 4,-9), ( 5,-9), ( 6,-9), ( 7,-9),
    (-8,-9), (-1,-9), (-2,-9), (-3,-9), (-4,-9), (-5,-9), (-6,-9), (-7,-9),
    ( 0,-2), ( 1,-2), ( 2,-2), ( 3,-2), ( 4,-2), ( 5,-2), ( 6,-2), ( 7,-2),
    (-8,-2), (-1,-2), (-2,-2), (-3,-2), (-4,-2), (-5,-2), (-6,-2), (-7,-2),
    ( 0,-3), ( 1,-3), ( 2,-3), ( 3,-3), ( 4,-3), ( 5,-3), ( 6,-3), ( 7,-3),
    (-8,-3), (-1,-3), (-2,-3), (-3,-3), (-4,-3), (-5,-3), (-6,-3), (-7,-3),
    ( 0,-4), ( 1,-4), ( 2,-4), ( 3,-4), ( 4,-4), ( 5,-4), ( 6,-4), ( 7,-4),
    (-8,-4), (-1,-4), (-2,-4), (-3,-4), (-4,-4), (-5,-4), (-6,-4), (-7,-4),
    ( 0,-5), ( 1,-5), ( 2,-5), ( 3,-5), ( 4,-5), ( 5,-5), ( 6,-5), ( 7,-5),
    (-8,-5), (-1,-5), (-2,-5), (-3,-5), (-4,-5), (-5,-5), (-6,-5), (-7,-5),
    ( 0,-6), ( 1,-6), ( 2,-6), ( 3,-6), ( 4,-6), ( 5,-6), ( 6,-6), ( 7,-6),
    (-8,-6), (-1,-6), (-2,-6), (-3,-6), (-4,-6), (-5,-6), (-6,-6), (-7,-6),
    ( 0,-7), ( 1,-7), ( 2,-7), ( 3,-7), ( 4,-7), ( 5,-7), ( 6,-7), ( 7,-7),
    (-8,-7), (-1,-7), (-2,-7), (-3,-7), (-4,-7), (-5,-7), (-6,-7), (-7,-7)
];
