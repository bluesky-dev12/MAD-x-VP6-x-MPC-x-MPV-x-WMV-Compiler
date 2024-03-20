use nihav_core::io::byteio::*;
use nihav_core::codecs::{EncoderResult, EncoderError};
use super::models::*;

pub struct EncSeq {
    pub bit:    bool,
    pub idx:    u8,
}

pub struct TokenSeq<T: PartialEq> {
    pub val:    T,
    pub seq:    &'static [EncSeq],
}

#[macro_export]
macro_rules! bit_entry {
    (T; $idx:expr) => {EncSeq {bit: true,  idx: $idx }};
    (F; $idx:expr) => {EncSeq {bit: false, idx: $idx }};
}

#[macro_export]
macro_rules! bit_seq {
    ($val: expr; $( $bit:tt),* ; $( $idx:expr),* ) => {
        TokenSeq {
            val: $val,
            seq:
                &[
                $(
                    bit_entry!($bit; $idx),
                )*
                ]
        }
    };
}

pub struct BoolEncoder<'a, 'b> {
    bw:     &'a mut ByteWriter<'b>,
    val:    u32,
    range:  u32,
    bits:   u8,
    saved:  u8,
    run:    usize,
}

impl<'a, 'b> BoolEncoder<'a, 'b> {
    pub fn new(bw: &'a mut ByteWriter<'b>) -> Self {
        Self {
            bw,
            val:    0,
            range:  255,
            bits:   0,
            saved:  0,
            run:    0,
        }
    }
    pub fn put_bool(&mut self, bit: bool, prob: u8) -> EncoderResult<()> {
        let split = 1 + (((self.range - 1) * u32::from(prob)) >> 8);
        if bit {
            self.range -= split;
            self.val   += split;
        } else {
            self.range = split;
        }

        if self.range < 128 {
            self.renorm()?;
        }
        Ok(())
    }
    fn flush_run(&mut self, overflow: bool) -> EncoderResult<()> {
        if self.run > 0 {
            self.bw.write_byte(self.saved + (overflow as u8))?;
            if !overflow {
                for _ in 1..self.run {
                    self.bw.write_byte(0xFF)?;
                }
            } else {
                for _ in 1..self.run {
                    self.bw.write_byte(0)?;
                }
            }
            self.run = 0;
        }
        Ok(())
    }
    fn renorm(&mut self) -> EncoderResult<()> {
        let bits = (self.range.leading_zeros() & 7) as u8;
        self.range <<= bits;
        if self.bits + bits < 23 {
            self.bits += bits;
            self.val <<= bits;
        } else {
            for _ in 0..bits {
                if (self.bits == 23) && ((self.val >> 31) != 0) {
                    self.flush_run(true)?;
                }
                self.val <<= 1;
                self.bits += 1;
                if self.bits == 24 {
                    let tbyte = (self.val >> 24) as u8;
                    let nbyte = (self.val >> 16) as u8;
                    if tbyte < 0xFF {
                        self.flush_run(false)?;
                        if nbyte < 0xFE {
                            self.bw.write_byte(tbyte)?;
                        } else {
                            self.saved = tbyte;
                            self.run = 1;
                        }
                    } else {
                        self.run += 1;
                    }
                    self.val &= 0xFFFFFF;
                    self.bits -= 8;
                }
            }
        }
        Ok(())
    }
    pub fn flush(mut self) -> EncoderResult<()> {
        self.flush_run(false)?;
        self.val <<= 24 - self.bits;
        self.bw.write_u32be(self.val)?;
        Ok(())
    }

    pub fn put_bits(&mut self, val: u32, len: u8) -> EncoderResult<()> {
        let mut mask = 1 << (len - 1);
        while mask != 0 {
            self.put_bool((val & mask) != 0, 128)?;
            mask >>= 1;
        }
        Ok(())
    }
    pub fn put_probability(&mut self, prob: u8) -> EncoderResult<()> {
        self.put_bits(u32::from(prob >> 1), 7)
    }
    pub fn encode_probability(&mut self, new: u8, old: u8, prob: u8) -> EncoderResult<()> {
        self.put_bool(new != old, prob)?;
        if new != old {
            self.put_probability(new)?;
        }
        Ok(())
    }
    pub fn write_el<T: PartialEq>(&mut self, el: T, tree: &[TokenSeq<T>], probs: &[u8]) -> EncoderResult<()> {
        for entry in tree.iter() {
            if entry.val == el {
                for seq in entry.seq.iter() {
                    self.put_bool(seq.bit, probs[seq.idx as usize])?;
                }
                return Ok(());
            }
        }
        Err(EncoderError::Bug)
    }
}

pub struct Estimator {}

#[allow(dead_code)]
impl Estimator {
    pub fn new() -> Self { Self{} }
    pub fn write_el<T: PartialEq>(&self, el: T, tree: &[TokenSeq<T>], probs: &mut [ProbCounter]) {
        for entry in tree.iter() {
            if entry.val == el {
                for seq in entry.seq.iter() {
                    probs[seq.idx as usize].add(seq.bit);
                }
                return;
            }
        }
    }
    pub fn est_nits(bit: bool, prob: u8) -> u32 {
        if !bit {
            u32::from(PROB_BITS[prob as usize])
        } else {
            u32::from(PROB_BITS[256 - (prob as usize)])
        }
    }
    pub fn nits_to_bits(nits: u32) -> u32 { (nits + 7) >> 3 }
}
