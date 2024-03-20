use nihav_core::codecs::{DecoderError, DecoderResult};
use nihav_core::io::bitreader::*;
use nihav_core::io::intcode::*;

pub struct RiceParams {
    k:      u8,
    sum:    u32
}

impl RiceParams {
    pub fn new() -> Self {
        let k = 10;
        Self { k, sum: 1 << (k + 4) }
    }
    fn update(&mut self, val: u32) {
        let limit = if self.k > 0 { 1 << (self.k + 4) } else { 0 };
        self.sum -= (self.sum + 16) >> 5;
        self.sum += (val + 1) / 2;
        if self.sum < limit {
            self.k -= 1;
        } else if (self.sum >= (1 << (self.k + 5))) && (self.k < 27) {
            self.k += 1;
        }
    }
    fn update_old(&mut self, val: u32) {
        let limit = if self.k > 0 { 1 << (self.k + 4) } else { 0 };
        self.sum -= (self.sum + 8) >> 4;
        self.sum += val;
        if self.sum < limit {
            self.k -= 1;
        } else if (self.sum >= (1 << (self.k + 5))) && (self.k < 24) {
            self.k += 1;
        }
    }
}

pub struct RiceCoder<'a> {
    br:     BitReader<'a>,
    rice_x: RiceParams,
    rice_y: RiceParams,
}

impl<'a> RiceCoder<'a> {
    pub fn new(br: BitReader<'a>) -> Self {
        Self {
            br,
            rice_x: RiceParams::new(),
            rice_y: RiceParams::new(),
        }
    }
}

struct ARangeCoder<'a> {
    src:    &'a [u8],
    pos:    usize,
    low:    u32,
    range:  u32,
    help:   u32,
    buffer: u32,
    error:  bool,
}

pub struct RangeCoder<'a> {
    rc:     ARangeCoder<'a>,
    rice_x: RiceParams,
    rice_y: RiceParams,
}

impl<'a> RangeCoder<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        Self {
            rc:     ARangeCoder::new(src),
            rice_x: RiceParams::new(),
            rice_y: RiceParams::new(),
        }
    }
    fn had_errors(&self) -> bool { self.rc.error }
}

const RANGE_BITS: u8 = 32;
const TOP: u32       = 1 << (RANGE_BITS - 1);
const BOTTOM: u32    = TOP >> 8;
const START_BITS: u8 = ((RANGE_BITS - 2) & 7) + 1;

const MAX_MODEL_VAL: u32 = 63;

impl<'a> ARangeCoder<'a> {
    fn new(src: &'a [u8]) -> Self {
        let buffer = u32::from(src[0]);
        Self {
            src:    &src[1..],
            pos:    0,
            low:    buffer >> (8 - START_BITS),
            range:  1 << START_BITS,
            help:   0,
            buffer,
            error:  false,
        }
    }
    fn reset(&mut self) {
        if self.pos == 0 {
            self.error = true;
        } else {
            self.pos -= 1;
        }
        if self.pos < self.src.len() - 1 {
            self.buffer = u32::from(self.src[self.pos]);
            self.pos += 1;
        }
        self.low    = self.buffer >> (8 - START_BITS);
        self.range  = 1 << START_BITS;
        self.help   = 0;
    }
    fn normalise(&mut self) {
        while self.range <= BOTTOM {
            self.buffer <<= 8;
            if self.pos < self.src.len() {
                self.buffer |= u32::from(self.src[self.pos]);
                self.pos += 1;
            } else {
                self.error = true;
            }
            self.low   <<= 8;
            self.low    |= (self.buffer >> 1) & 0xFF;
            self.range <<= 8;
        }
    }
    fn get_freq(&mut self, freq: u32) -> u32 {
        self.normalise();
        self.help = self.range / freq;
        self.low / self.help
    }
    fn get_bits(&mut self, bits: u8) -> u32 {
        self.normalise();
        self.help = self.range >> bits;
        self.low / self.help
    }
    fn update(&mut self, interval: u32, low: u32) {
        self.low    -= self.help * low;
        self.range   = self.help * interval;
    }
    fn decode_freq(&mut self, freq: u32) -> u32 {
        let sym = self.get_freq(freq);
        self.update(1, sym);
        sym
    }
    fn decode_bits(&mut self, bits: u8) -> u32 {
        let sym = self.get_bits(bits);
        self.update(1, sym);
        sym
    }
    fn decode_symbol(&mut self, freqs: &[u32; 22]) -> u32 {
        let bits = self.get_bits(16);
        if bits > 65492 {
            let sym = bits + MAX_MODEL_VAL - 65535;
            self.update(1, bits);
            if bits > 65536 {
                self.error = true;
            }
            sym
        } else {
            let mut sym = 0usize;
            while freqs[sym + 1] <= bits {
                sym += 1;
            }
            self.update(freqs[sym + 1] - freqs[sym], freqs[sym]);
            sym as u32
        }
    }
}

pub enum Coder<'a> {
    Rice(RiceCoder<'a>),
    Range(RangeCoder<'a>),
}

fn to_signed(val: u32) -> i32 {
    if (val & 1) != 0 {
        (val >> 1) as i32 + 1
    } else {
        -((val >> 1) as i32)
    }
}

pub fn decode_mono_dummy(_c: &mut Coder, _l: &mut [i32]) -> DecoderResult<()> {
    unreachable!();
}
pub fn decode_stereo_dummy(_c: &mut Coder, _l: &mut [i32], _r: &mut [i32]) -> DecoderResult<()> {
    unreachable!();
}

fn new_k(val: u32) -> u8 {
    (32 - (val | 1).leading_zeros()) as u8
}
fn rice_limit(k: u8) -> u32 { if k > 0 { 1 << (k + 4) } else { 0 } }
fn decode_channel_0000(br: &mut BitReader, dst: &mut [i32]) -> DecoderResult<()> {
    let (part01, part2) = dst.split_at_mut(64);
    let (part0, part1) = part01.split_at_mut(5);
    let mut pos = 0;
    let mut last = [0u32; 64];
    let mut sum = 0;
    for el in part0.iter_mut() {
        let val                         = br.read_code(UintCodeType::Rice(10))?;
        sum += val;
        *el = to_signed(val);
        last[pos] = val;
        pos += 1;
    }
    if part1.is_empty() {
        return Ok(());
    }
    let mut k = new_k(sum / 10);
    let mut w = 12;
    for el in part1.iter_mut() {
        let val                         = br.read_code(UintCodeType::Rice(k))?;
        sum += val;
        *el = to_signed(val);
        k = new_k(sum / w);
        w += 2;
        last[pos] = val;
        pos += 1;
    }
    if part2.is_empty() {
        return Ok(());
    }
    let mut top_limit = rice_limit(k + 1) * 4;
    let mut bot_limit = rice_limit(k)     * 4;
    pos = 0;
    for el in part2.iter_mut() {
        let val                         = br.read_code(UintCodeType::Rice(k))?;
        *el = to_signed(val);

        sum = sum.wrapping_add(val.wrapping_sub(last[pos]));
        while sum < bot_limit {
            k -= 1;
            bot_limit = rice_limit(k) * 4;
            top_limit >>= 1;
        }
        while sum >= top_limit {
            k += 1;
            validate!(k < 24);
            bot_limit = rice_limit(k) * 4;
            top_limit <<= 1;
        }

        last[pos] = val;
        pos = (pos + 1) & 63;
    }

    Ok(())
}
pub fn decode_mono_0000(c: &mut Coder, l: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Rice(ref mut rr) = c {
        decode_channel_0000(&mut rr.br, l)
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_stereo_0000(c: &mut Coder, l: &mut [i32], r: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Rice(ref mut rr) = c {
        decode_channel_0000(&mut rr.br, l)?;
        decode_channel_0000(&mut rr.br, r)?;
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}

fn decode_value_3860(br: &mut BitReader, rice: &mut RiceParams) -> DecoderResult<i32> {
    let overflow                        = br.read_code(UintCodeType::UnaryZeroes)?;

    let val                             = (overflow << rice.k) | br.read(rice.k)?;
    rice.update_old(val);
    Ok(to_signed(val))
}
fn decode_value_3890(br: &mut BitReader, rice: &mut RiceParams) -> DecoderResult<i32> {
    let mut overflow                    = br.read_code(UintCodeType::UnaryZeroes)?;
    while overflow >= 16 {
        overflow -= 16;
        rice.k += 4;
    }

    let val                             = (overflow << rice.k) | br.read(rice.k)?;
    rice.update_old(val);
    Ok(to_signed(val))
}
pub fn decode_mono_3860(c: &mut Coder, l: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Rice(ref mut rr) = c {
        for el in l.iter_mut() {
            *el = decode_value_3860(&mut rr.br, &mut rr.rice_y)?;
        }
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_mono_3890(c: &mut Coder, l: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Rice(ref mut rr) = c {
        for el in l.iter_mut() {
            *el = decode_value_3860(&mut rr.br, &mut rr.rice_y)?;
        }
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_stereo_3860(c: &mut Coder, l: &mut [i32], r: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Rice(ref mut rr) = c {
        for el in l.iter_mut() {
            *el = decode_value_3860(&mut rr.br, &mut rr.rice_y)?;
        }
        for el in r.iter_mut() {
            *el = decode_value_3860(&mut rr.br, &mut rr.rice_x)?;
        }
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_stereo_3890(c: &mut Coder, l: &mut [i32], r: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Rice(ref mut rr) = c {
        for el in l.iter_mut() {
            *el = decode_value_3890(&mut rr.br, &mut rr.rice_y)?;
        }
        for el in r.iter_mut() {
            *el = decode_value_3890(&mut rr.br, &mut rr.rice_x)?;
        }
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}

const COUNTS_3900: &[u32; 22] = &[
        0, 14824, 28224, 39348, 47855, 53994, 58171, 60926,
    62682, 63786, 64463, 64878, 65126, 65276, 65365, 65419,
    65450, 65469, 65480, 65487, 65491, 65493
];
fn decode_value_3900(rc: &mut ARangeCoder, rice: &mut RiceParams) -> i32 {
    let mut overflow = rc.decode_symbol(COUNTS_3900);
    let k = if overflow == MAX_MODEL_VAL {
            overflow = 0;
            rc.decode_bits(5) as u8
        } else {
            rice.k.saturating_sub(1)
        };
    let base = rc.decode_bits(k);
    let val = base + (overflow << k);
    rice.update(val);
    to_signed(val)
}
fn decode_value_3910(rc: &mut ARangeCoder, rice: &mut RiceParams) -> i32 {
    let mut overflow = rc.decode_symbol(COUNTS_3900);
    let k = if overflow == MAX_MODEL_VAL {
            overflow = 0;
            rc.decode_bits(5) as u8
        } else {
            rice.k.saturating_sub(1)
        };
    let base = if k <= 16 {
            rc.decode_bits(k)
        } else if k <= 32 {
            let low = rc.decode_bits(16);
            let high = rc.decode_bits(k - 16);
            (high << 16) | low
        } else {
            rc.error = true;
            return 0;
        };
    let val = base + (overflow << k);
    rice.update(val);
    to_signed(val)
}
pub fn decode_mono_3900(c: &mut Coder, l: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Range(ref mut rc) = c {
        for el in l.iter_mut() {
            *el = decode_value_3900(&mut rc.rc, &mut rc.rice_y);
        }
        validate!(!rc.had_errors());
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_mono_3910(c: &mut Coder, l: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Range(ref mut rc) = c {
        for el in l.iter_mut() {
            *el = decode_value_3910(&mut rc.rc, &mut rc.rice_y);
        }
        validate!(!rc.had_errors());
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_stereo_3900(c: &mut Coder, l: &mut [i32], r: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Range(ref mut rc) = c {
        for el in l.iter_mut() {
            *el = decode_value_3900(&mut rc.rc, &mut rc.rice_y);
        }
        rc.rc.normalise();
        validate!(!rc.had_errors() && rc.rc.pos < rc.rc.src.len());
        rc.rc.reset();
        for el in r.iter_mut() {
            *el = decode_value_3900(&mut rc.rc, &mut rc.rice_x);
        }
        validate!(!rc.had_errors());
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_stereo_3910(c: &mut Coder, l: &mut [i32], r: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Range(ref mut rc) = c {
        for el in l.iter_mut() {
            *el = decode_value_3910(&mut rc.rc, &mut rc.rice_y);
        }
        rc.rc.normalise();
        validate!(!rc.had_errors() && rc.rc.pos < rc.rc.src.len());
        rc.rc.reset();
        for el in r.iter_mut() {
            *el = decode_value_3910(&mut rc.rc, &mut rc.rice_x);
        }
        validate!(!rc.had_errors());
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_stereo_3930(c: &mut Coder, l: &mut [i32], r: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Range(ref mut rc) = c {
        for (l, r) in l.iter_mut().zip(r.iter_mut()) {
            *l = decode_value_3910(&mut rc.rc, &mut rc.rice_y);
            *r = decode_value_3910(&mut rc.rc, &mut rc.rice_x);
        }
        validate!(!rc.had_errors());
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}

const COUNTS_3990: &[u32; 22] = &[
        0, 19578, 36160, 48417, 56323, 60899, 63265, 64435,
    64971, 65232, 65351, 65416, 65447, 65466, 65476, 65482,
    65485, 65488, 65490, 65491, 65492, 65493
];
fn decode_value_3990(rc: &mut ARangeCoder, rice: &mut RiceParams) -> i32 {
    let pivot = (rice.sum >> 5).max(1);
    let mut overflow = rc.decode_symbol(COUNTS_3990);
    if overflow == MAX_MODEL_VAL {
        overflow  = rc.decode_bits(16) << 16;
        overflow |= rc.decode_bits(16);
    }
    let base = if pivot < (1 << 16) {
            rc.decode_freq(pivot)
        } else {
            let shift = (16 - pivot.trailing_zeros()) as u8;
            let hi = rc.decode_freq((pivot >> shift) + 1);
            let lo = rc.decode_bits(shift);
            (hi << shift) | lo
        };
    let val = base + overflow * pivot;
    rice.update(val);
    to_signed(val)
}
pub fn decode_mono_3990(c: &mut Coder, l: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Range(ref mut rc) = c {
        for el in l.iter_mut() {
            *el = decode_value_3990(&mut rc.rc, &mut rc.rice_y);
        }
        validate!(!rc.had_errors());
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
pub fn decode_stereo_3990(c: &mut Coder, l: &mut [i32], r: &mut [i32]) -> DecoderResult<()> {
    if let Coder::Range(ref mut rc) = c {
        for (l, r) in l.iter_mut().zip(r.iter_mut()) {
            *l = decode_value_3990(&mut rc.rc, &mut rc.rice_y);
            *r = decode_value_3990(&mut rc.rc, &mut rc.rice_x);
        }
        validate!(!rc.had_errors());
        Ok(())
    } else {
        Err(DecoderError::Bug)
    }
}
