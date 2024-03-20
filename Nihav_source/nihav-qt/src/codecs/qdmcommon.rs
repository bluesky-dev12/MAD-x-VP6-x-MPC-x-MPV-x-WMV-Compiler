use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;

/// Bitstream reader.
#[derive(Debug,Clone)]
pub struct QdmBitReader<'a> {
    cache: u32,
    bits:  u8,
    pos:   usize,
    src:   &'a [u8],
}

#[allow(clippy::identity_op)]
#[allow(dead_code)]
impl<'a> QdmBitReader<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        Self{ cache: 0, pos: 0, bits: 0, src }
    }
    pub fn tell(&self) -> usize {
        self.pos * 8 - (self.bits as usize)
    }
    pub fn left(&self) -> isize {
        ((self.src.len() as isize) - (self.pos as isize)) * 8 + (self.bits as isize)
    }
    fn refill(&mut self) {
        while self.bits <= 24 {
            let byte = if self.pos < self.src.len() {
                    self.pos += 1;
                    self.src[self.pos - 1]
                } else {
                    self.pos += 1;
                    0
                };
            self.cache |= u32::from(byte) << self.bits;
            self.bits += 8;
        }
    }
    fn read_cache(&mut self, nbits: u8) -> u32 {
        ((1 << nbits) - 1) & self.cache
    }
    fn skip_cache(&mut self, nbits: u8) {
        self.cache >>= nbits;
        self.bits -= nbits;
    }
    fn reset_cache(&mut self) {
        self.bits = 0;
        self.cache = 0;
    }
    pub fn read(&mut self, nbits: u8) -> u32 {
        if nbits == 0 { return 0; }
        if nbits > 32 { return 0; }
        if self.bits < nbits {
            self.refill();
        }
        let res = self.read_cache(nbits);
        self.skip_cache(nbits);
        res
    }
    pub fn read_bool(&mut self) -> bool {
        if self.bits < 1 {
            self.refill();
        }
        let res = self.read_cache(1);
        self.skip_cache(1);
        res == 1
    }
    pub fn peek(&mut self, nbits: u8) -> u32 {
        if nbits > 32 { return 0 }
        if self.bits < nbits { self.refill(); }
        self.read_cache(nbits)
    }
    pub fn skip(&mut self, nbits: u32) {
        if u32::from(self.bits) >= nbits {
            self.skip_cache(nbits as u8);
            return;
        }
        let mut skip_bits = nbits - u32::from(self.bits);
        self.reset_cache();
        self.pos += ((skip_bits / 32) * 4) as usize;
        skip_bits &= 0x1F;
        self.refill();
        if skip_bits > 0 {
            self.skip_cache(skip_bits as u8);
        }
    }
}

impl<'a, S: Copy> CodebookReader<S> for QdmBitReader<'a> {
    #[allow(unused_variables)]
    fn read_cb(&mut self, cb: &Codebook<S>) -> CodebookResult<S> {
        let mut esc = true;
        let mut idx = 0;
        let mut lut_bits = cb.lut_bits;
        while esc {
            let lut_idx = (self.peek(lut_bits) as usize) + idx;
            if cb.table[lut_idx] == TABLE_FILL_VALUE { return Err(CodebookError::InvalidCode); }
            let bits = cb.table[lut_idx] & 0x7F;
            esc  = (cb.table[lut_idx] & 0x80) != 0;
            idx  = (cb.table[lut_idx] >> 8) as usize;
            let skip_bits = if esc { u32::from(lut_bits) } else { bits };
            self.skip(skip_bits);
            lut_bits = bits as u8;
        }
        Ok(cb.syms[idx])
    }
}


pub fn to_signed(val: i32) -> i32 {
    if (val & 1) != 0 {
        (val + 1) >> 1
    } else {
        -(val >> 1)
    }
}

pub trait QdmcCodeReader {
    fn read_code(&mut self, cb: &Codebook<u8>) -> DecoderResult<u32>;
    fn read_code_long(&mut self, cb: &Codebook<u8>) -> DecoderResult<u32>;
}

impl<'a> QdmcCodeReader for BitReader<'a> {
    fn read_code(&mut self, cb: &Codebook<u8>) -> DecoderResult<u32> {
        let idx                         = self.read_cb(cb)?;
        if idx > 0 {
            Ok(u32::from(idx - 1))
        } else {
            let len                     = (self.read(3)? as u8) + 1;
            let val                     = self.read(len)?;
            Ok(val)
        }
    }
    fn read_code_long(&mut self, cb: &Codebook<u8>) -> DecoderResult<u32> {
        let idx                         = self.read_code(cb)? as usize;
        validate!(idx < ESCAPE_PREFIX.len());
        let add                         = self.read((idx >> 2) as u8)?;
        Ok(ESCAPE_PREFIX[idx] + add)
    }
}

impl<'a> QdmcCodeReader for QdmBitReader<'a> {
    fn read_code(&mut self, cb: &Codebook<u8>) -> DecoderResult<u32> {
        let idx                         = self.read_cb(cb)?;
        if idx > 0 {
            Ok(u32::from(idx - 1))
        } else {
            let len                     = (self.read(3) as u8) + 1;
            let val                     = self.read(len);
            Ok(val)
        }
    }
    fn read_code_long(&mut self, cb: &Codebook<u8>) -> DecoderResult<u32> {
        let idx                         = self.read_code(cb)? as usize;
        validate!(idx < ESCAPE_PREFIX.len());
        let add                         = self.read((idx >> 2) as u8);
        Ok(ESCAPE_PREFIX[idx] + add)
    }
}

const ESCAPE_PREFIX: [u32; 65] = [
    0x00000, 0x00001, 0x00002, 0x00003, 0x00004, 0x00006, 0x00008, 0x0000A,
    0x0000C, 0x00010, 0x00014, 0x00018, 0x0001C, 0x00024, 0x0002C, 0x00034,
    0x0003C, 0x0004C, 0x0005C, 0x0006C, 0x0007C, 0x0009C, 0x000BC, 0x000DC,
    0x000FC, 0x0013C, 0x0017C, 0x001BC, 0x001FC, 0x0027C, 0x002FC, 0x0037C,
    0x003FC, 0x004FC, 0x005FC, 0x006FC, 0x007FC, 0x009FC, 0x00BFC, 0x00DFC,
    0x00FFC, 0x013FC, 0x017FC, 0x01BFC, 0x01FFC, 0x027FC, 0x02FFC, 0x037FC,
    0x03FFC, 0x04FFC, 0x05FFC, 0x06FFC, 0x07FFC, 0x09FFC, 0x0BFFC, 0x0DFFC,
    0x0FFFC, 0x13FFC, 0x17FFC, 0x1BFFC, 0x1FFFC, 0x27FFC, 0x2FFFC, 0x37FFC,
    0x3FFFC
];

pub struct RNG {
    pub seed:   u32,
}

impl RNG {
    pub fn new() -> Self { Self { seed: 0 } }
    pub fn next(&mut self) -> u32 {
        self.seed = self.seed.wrapping_mul(0x343FD).wrapping_add(0x269EC3);
        self.seed
    }
    pub fn next_float(&mut self) -> f32 {
        self.next();
        ((((self.seed >> 16) & 0x7FFF) as f32) - 16384.0) / 16384.0
    }
}

#[derive(Clone,Copy)]
pub struct Tone {
    pub ch:         u8,
    pub phase:      u8,
    pub offset:     u8,
    pub freq:       u16,
    pub amp_idx:    u8,
}

pub const MAX_TONES: usize = 8192;

