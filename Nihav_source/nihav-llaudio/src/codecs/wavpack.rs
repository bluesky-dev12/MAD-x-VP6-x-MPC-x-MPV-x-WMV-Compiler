use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::intcode::*;

const SAMPLE_RATES: [u32; 15] = [
     6000,  8000,  9600, 11025, 12000, 16000,  22050, 24000,
    32000, 44100, 48000, 64000, 88200, 96000, 192000
];
const WV_FLAG_MONO: u32         = 1 <<  2;
const WV_FLAG_HYBRID: u32       = 1 <<  3;
const WV_FLAG_JSTEREO: u32      = 1 <<  4;
//const WV_FLAG_CH_DECORR: u32    = 1 <<  5;
//const WV_FLAG_HYB_NOISE_SHAPING: u32 = 1 <<  6;
const WV_FLAG_FLOATS: u32       = 1 <<  7;
const WV_FLAG_EXT_INTEGERS: u32 = 1 <<  8;
const WV_FLAG_HYB_BITRATE: u32  = 1 <<  9;
//const WV_FLAG_HYB_BALANCED_NOISE: u32 = 1 << 10;
const WV_FLAG_START_BLOCK: u32  = 1 << 11;
const WV_FLAG_END_BLOCK: u32    = 1 << 12;
//const WV_FLAG_HAS_CRC: u32      = 1 << 28;
const WV_FLAG_FALSE_STEREO: u32 = 1 << 30;
const WV_FLAG_DSD_AUDIO: u32    = 1 << 31;

const WV_STREAM_FLAGS: u32 = 0x8000008B;

#[derive(Clone,Copy,Default)]
struct WVHeader {
    size:           usize,
    ver:            u16,
    tot_samples:    u64,
    block_index:    u64,
    block_samples:  u32,
    flags:          u32,
    crc:            u32,
}

const WV_HEADER_SIZE: usize = 32;

impl WVHeader {
    #[allow(clippy::field_reassign_with_default)]
    fn parse(src: &[u8]) -> DecoderResult<Self> {
        let mut mr = MemoryReader::new_read(src);
        let mut br = ByteReader::new(&mut mr);
        let tag                         = br.read_tag()?;
        validate!(&tag == b"wvpk");
        let mut hdr = Self::default();
        hdr.size                        = br.read_u32le()? as usize;
        validate!(hdr.size >= 24);
        hdr.ver                         = br.read_u16le()?;
        validate!(hdr.ver >= 0x402 || hdr.ver <= 0x410);
        let top_idx                     = br.read_byte()?;
        let top_samps                   = br.read_byte()?;
        hdr.tot_samples                 = u64::from(br.read_u32le()?) | (u64::from(top_samps) << 32);
        hdr.block_index                 = u64::from(br.read_u32le()?) | (u64::from(top_idx) << 32);
        hdr.block_samples               = br.read_u32le()?;
        hdr.flags                       = br.read_u32le()?;
        hdr.crc                         = br.read_u32le()?;
        Ok(hdr)
    }
    fn stream_eq(&self, rval: &Self) -> bool {
        self.ver == rval.ver &&
        (self.flags & WV_STREAM_FLAGS) == (rval.flags & WV_STREAM_FLAGS)
    }
    fn block_eq(&self, rval: &Self) -> bool {
        self.stream_eq(rval) && self.block_index == rval.block_index &&
        self.tot_samples == rval.tot_samples &&
        self.block_samples == rval.block_samples
    }
    fn is_start_block(&self) -> bool {
        (self.flags & WV_FLAG_START_BLOCK) != 0
    }
    fn is_end_block(&self) -> bool {
        (self.flags & WV_FLAG_END_BLOCK) != 0
    }
    fn get_num_channels(&self) -> u8 {
        if (self.flags & WV_FLAG_MONO) != 0 && (self.flags & WV_FLAG_FALSE_STEREO) == 0 { 1 } else { 2 }
    }
    fn get_sample_rate(&self) -> u32 {
        let idx = ((self.flags >> 23) & 0xF) as usize;
        if idx != 15 {
            SAMPLE_RATES[idx]
        } else {
            0
        }
    }
    fn get_size(&self) -> usize {
        self.size - (WV_HEADER_SIZE - 8)
    }
    fn get_bits(&self) -> u8 {
        (((self.flags & 3) + 1) * 8) as u8
    }
}

fn wv_log2lin(val: i32) -> i32 {
    let sign = val < 0;
    let aval = val.abs();
    let mant = 0x100 | i32::from(WV_EXP_TABLE[(aval & 0xFF) as usize]);
    let exp = aval >> 8;
    let aval = if exp >= 9 {
            mant << (exp - 9)
        } else {
            mant >> (9 - exp)
        };
    if !sign {
        aval
    } else {
        -aval
    }
}

fn wv_lin2log(val: u32) -> u32 {
    if val == 0 {
        0
    } else if val == 1 {
        0x100
    } else {
        let val = val + (val >> 9);
        let bits = 32 - val.leading_zeros();
        if bits < 9 {
            (bits << 8) + u32::from(WV_LOG_TABLE[((val << (9 - bits)) & 0xFF) as usize])
        } else {
            (bits << 8) + u32::from(WV_LOG_TABLE[((val >> (bits - 9)) & 0xFF) as usize])
        }
    }
}

#[derive(Clone,Copy,Default)]
struct Decorrelator {
    delta:      i32,
    value:      i32,
    weight_a:   i32,
    weight_b:   i32,
    samples_a:  [i32; 8],
    samples_b:  [i32; 8],
}

impl Decorrelator {
    fn decorrelate_mono(&mut self, l: i32, pos: usize) -> i32 {
        let mode = self.value;
        let (a, npos) = if mode > 8 {
                let a = if (mode & 1) != 0 {
                        2 * self.samples_a[0] - self.samples_a[1]
                    } else {
                        (3 * self.samples_a[0] - self.samples_a[1]) >> 1
                    };
                self.samples_a[1] = self.samples_a[0];
                (a, 0)
            } else {
                (self.samples_a[pos], (pos + mode as usize) & 7)
            };
        let l2 = l + ((i64::from(self.weight_a) * i64::from(a) + 512) >> 10) as i32;
        if (a != 0) && (l != 0) {
            self.weight_a -= ((((l ^ a) >> 30) & 2) - 1) * self.delta;
        }
        self.samples_a[npos] = l2;
        l2
    }
    fn decorrelate_stereo(&mut self, l: i32, r: i32, pos: usize) -> (i32, i32) {
        let mode = self.value;
        if mode > 0 {
            let (a, b, npos) = if mode > 8 {
                    let (a, b) = if (mode & 1) != 0 {
                            (2 * self.samples_a[0] - self.samples_a[1],
                             2 * self.samples_b[0] - self.samples_b[1])
                        } else {
                            ((3 * self.samples_a[0] - self.samples_a[1]) >> 1,
                             (3 * self.samples_b[0] - self.samples_b[1]) >> 1)
                        };
                    self.samples_a[1] = self.samples_a[0];
                    self.samples_b[1] = self.samples_b[0];
                    (a, b, 0)
                } else {
                    (self.samples_a[pos], self.samples_b[pos], (pos + mode as usize) & 7)
                };
            let l2 = l + ((i64::from(self.weight_a) * i64::from(a) + 512) >> 10) as i32;
            let r2 = r + ((i64::from(self.weight_b) * i64::from(b) + 512) >> 10) as i32;
            if (a != 0) && (l != 0) {
                self.weight_a -= ((((l ^ a) >> 30) & 2) - 1) * self.delta;
            }
            if (b != 0) && (r != 0) {
                self.weight_b -= ((((r ^ b) >> 30) & 2) - 1) * self.delta;
            }
            self.samples_a[npos] = l2;
            self.samples_b[npos] = r2;
            (l2, r2)
        } else if mode == -1 {
            let l2 = l + ((i64::from(self.weight_a) * i64::from(self.samples_a[0]) + 512) >> 10) as i32;
            let r2 = r + ((i64::from(self.weight_b) * i64::from(l2) + 512) >> 10) as i32;
            self.update_weight_a(self.samples_a[0], l);
            self.update_weight_b(l2, r);
            self.samples_a[0] = r2;
            (l2, r2)
        } else {
            let r2 = r + ((i64::from(self.weight_b) * i64::from(self.samples_b[0]) + 512) >> 10) as i32;
            self.update_weight_b(self.samples_b[0], r);
            let rr = if mode == -3 {
                    let nr = self.samples_a[0];
                    self.samples_a[0] = r2;
                    nr
                } else {
                    r2
                };
            let l2 = l + ((i64::from(self.weight_a) * i64::from(rr) + 512) >> 10) as i32;
            self.update_weight_a(rr, l);
            self.samples_b[0] = l2;
            (l2, r2)
        }
    }
    fn update_weight_a(&mut self, l: i32, r: i32) {
        if (l != 0) && (r != 0) {
            if (l ^ r) < 0 {
                self.weight_a = (self.weight_a - self.delta).max(-1024)
            } else {
                self.weight_a = (self.weight_a + self.delta).min(1024)
            }
        }
    }
    fn update_weight_b(&mut self, l: i32, r: i32) {
        if (l != 0) && (r != 0) {
            if (l ^ r) < 0 {
                self.weight_b = (self.weight_b - self.delta).max(-1024)
            } else {
                self.weight_b = (self.weight_b + self.delta).min(1024)
            }
        }
    }
}

#[derive(Default)]
struct DecState {
    median:     [[u32; 3]; 2],
    zero:       bool,
    one:        bool,
    num_zeroes: u32,
    ebits:      u8,
    shift:      u8,
    and:        i32,
    or:         i32,
    post_shift: u8,
    slow_level: [u32; 2],
    br_acc:     [u32; 2],
    br_delta:   [u32; 2],
    hyb_max:    i32,
    hyb_min:    i32,
    has_hbr:    bool,
    stereo:     bool,
    error_lim:  [u32; 2],
}

fn read_biased_code(br: &mut BitReader) -> DecoderResult<u32> {
    let val                             = br.read_code(UintCodeType::UnaryOnes)?;
    validate!(val < 25);
    if val < 2 {
        Ok(val)
    } else {
        let bits = val - 1;
        Ok((1 << bits) | br.read(bits as u8)?)
    }
}
fn read_tail(br: &mut BitReader, bits: u32) -> DecoderResult<u32> {
    if bits < 1 {
        Ok(0)
    } else {
        let p = 31 - bits.leading_zeros();
        let esc = (1 << (p + 1)) - (bits + 1);
        let val                         = br.read(p as u8)?;
        if val < esc {
            Ok(val)
        } else {
            Ok(val * 2 - esc + br.read(1)?)
        }
    }
}

impl DecState {
    fn read_sample(&mut self, br: &mut BitReader, channel: usize) -> DecoderResult<i32> {
        if (self.median[0][0] < 2) && (self.median[1][0] < 2) && !self.zero && !self.one {
            if self.num_zeroes > 0 {
                self.num_zeroes -= 1;
                if self.num_zeroes != 0 {
                    return Ok(0);
                }
            } else {
                self.num_zeroes = read_biased_code(br)?;
                if self.num_zeroes != 0 {
                    self.median = [[0; 3]; 2];
                    return Ok(0);
                }
            }
        }
        let val = if self.zero {
                self.zero = false;
                0
            } else {
                let mut val             = br.read_code(UintCodeType::UnaryOnes)?;
                validate!(val <= 16);
                if val == 16 {
                    val += read_biased_code(br)?;
                }
                let one = (val & 1) != 0;
                val >>= 1;
                if self.one {
                    val += 1;
                }
                self.one = one;
                self.zero = !one;
                val
            };

        let (base, add) = match val {
                0 => {
                    let add = self.get_median(channel, 0) - 1;
                    self.dec_median(channel, 0);
                    (0, add)
                },
                1 => {
                    let base = self.get_median(channel, 0);
                    let add  = self.get_median(channel, 1) - 1;
                    self.inc_median(channel, 0);
                    self.dec_median(channel, 1);
                    (base, add)
                },
                2 => {
                    let base = self.get_median(channel, 0) + self.get_median(channel, 1);
                    let add  = self.get_median(channel, 2) - 1;
                    self.inc_median(channel, 0);
                    self.inc_median(channel, 1);
                    self.dec_median(channel, 2);
                    (base, add)
                },
                _ => {
                    let base = self.get_median(channel, 0) + self.get_median(channel, 1) + self.get_median(channel, 2) * (val - 2);
                    let add  = self.get_median(channel, 2) - 1;
                    self.inc_median(channel, 0);
                    self.inc_median(channel, 1);
                    self.inc_median(channel, 2);
                    (base, add)
                },
            };
        let val = base + read_tail(br, add)?;

        if !br.read_bool()? {
            Ok(val as i32)
        } else {
            Ok(!val as i32)
        }
    }
    fn read_sample_hyb(&mut self, br: &mut BitReader, channel: usize) -> DecoderResult<i32> {
        if (self.median[0][0] < 2) && (self.median[1][0] < 2) && !self.zero && !self.one {
            if self.num_zeroes > 0 {
                self.num_zeroes -= 1;
                if self.num_zeroes != 0 {
                    self.decay_slev(channel);
                    return Ok(0);
                }
            } else {
                self.num_zeroes = read_biased_code(br)?;
                if self.num_zeroes != 0 {
                    self.median = [[0; 3]; 2];
                    self.decay_slev(channel);
                    return Ok(0);
                }
            }
        }
        let val = if self.zero {
                self.zero = false;
                0
            } else {
                let mut val             = br.read_code(UintCodeType::UnaryOnes)?;
                validate!(val <= 16);
                if val == 16 {
                    val += read_biased_code(br)?;
                }
                let one = (val & 1) != 0;
                val >>= 1;
                if self.one {
                    val += 1;
                }
                self.one = one;
                self.zero = !one;
                val
            };

        if channel == 0 {
            self.update_error_limit();
        }

        let (mut base, mut add) = match val {
                0 => {
                    let add = self.get_median(channel, 0) - 1;
                    self.dec_median(channel, 0);
                    (0, add)
                },
                1 => {
                    let base = self.get_median(channel, 0);
                    let add  = self.get_median(channel, 1) - 1;
                    self.inc_median(channel, 0);
                    self.dec_median(channel, 1);
                    (base, add)
                },
                2 => {
                    let base = self.get_median(channel, 0) + self.get_median(channel, 1);
                    let add  = self.get_median(channel, 2) - 1;
                    self.inc_median(channel, 0);
                    self.inc_median(channel, 1);
                    self.dec_median(channel, 2);
                    (base, add)
                },
                _ => {
                    let base = self.get_median(channel, 0) + self.get_median(channel, 1) + self.get_median(channel, 2) * (val - 2);
                    let add  = self.get_median(channel, 2) - 1;
                    self.inc_median(channel, 0);
                    self.inc_median(channel, 1);
                    self.inc_median(channel, 2);
                    (base, add)
                },
            };
        let val = if self.error_lim[channel] == 0 {
                base + read_tail(br, add)?
            } else {
                let mut mid = (base * 2 + add + 1) >> 1;
                while add > self.error_lim[channel] {
                    if br.read_bool()? {
                        add += base;
                        add -= mid;
                        base = mid;
                    } else {
                        add = mid - base - 1;
                    }
                    mid = (base * 2 + add + 1) >> 1;
                }
                mid
            };

        if self.has_hbr {
            self.decay_slev(channel);
            self.slow_level[channel] += wv_lin2log(val);
        }

        if !br.read_bool()? {
            Ok(val as i32)
        } else {
            Ok(!val as i32)
        }
    }
    fn decay_slev(&mut self, channel: usize) {
        self.slow_level[channel] -= (self.slow_level[channel] + 0x80) >> 8;
    }
    fn update_error_limit(&mut self) {
        let mut br = [0; 2];
        let mut sl = [0; 2];

        for i in 0..2 {
            self.br_acc[i] += self.br_delta[i];
            br[i] = self.br_acc[i] >> 16;
            sl[i] = (self.slow_level[i] + 0x80) >> 8;
        }

        if self.stereo && self.has_hbr {
            let balance = ((sl[1] as i32) - (sl[0] as i32) + (br[1] as i32) + 1) >> 1;
            if balance > (br[0] as i32) {
                br[1] = br[0] << 1;
                br[0] = 0;
            } else if -balance > (br[0] as i32) {
                br[0] <<= 1;
                br[1]   = 0;
            } else {
                br[1] = ((br[0] as i32) + balance) as u32;
                br[0] = ((br[0] as i32) - balance) as u32;
            }
        }
        for i in 0..2 {
            self.error_lim[i] = if self.has_hbr {
                    if sl[i] + 0x100 > br[i] {
                        wv_log2lin((sl[i] + 0x100 - br[i]) as i32) as u32
                    } else {
                        0
                    }
                } else {
                    wv_log2lin(br[i] as i32) as u32
                };
        }
    }
    fn get_median(&self, channel: usize, idx: usize) -> u32 {
        (self.median[channel][idx] >> 4) + 1
    }
    fn inc_median(&mut self, channel: usize, idx: usize) {
        self.median[channel][idx] += ((self.median[channel][idx] + (128 >> idx)) / (128 >> idx)) * 5;
    }
    fn dec_median(&mut self, channel: usize, idx: usize) {
        self.median[channel][idx] -= ((self.median[channel][idx] + (128 >> idx) - 2) / (128 >> idx)) * 2;
    }
    fn produce_sample_common(&self, ebr: &mut BitReader, mut samp: i32) -> i32 {
        if self.ebits > 0 {
            samp <<= self.ebits;
            if ebr.left() >= self.ebits as isize {
                samp |=                 ebr.read(self.ebits).unwrap_or(0) as i32;
            }
        }
        let bit = (samp & self.and) | self.or;
        ((samp + bit) << self.shift) - bit
    }
    fn produce_sample(&self, ebr: &mut BitReader, samp: i32) -> i32 {
        self.produce_sample_common(ebr, samp) << self.post_shift
    }
    fn produce_sample_hyb(&self, ebr: &mut BitReader, samp: i32) -> i32 {
        self.produce_sample_common(ebr, samp).max(self.hyb_min).min(self.hyb_max) << self.post_shift
    }
}

struct DecorrState {
    decorr:     [Decorrelator; 16],
    num_decorr: usize,
}

impl DecorrState {
    fn new() -> Self {
        Self {
            decorr:     [Decorrelator::default(); 16],
            num_decorr: 0,
        }
    }
    fn reset(&mut self) {
        self.decorr = [Decorrelator::default(); 16];
        self.num_decorr = 0;
    }
}

struct WavPackDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    header:     WVHeader,

    dstate:     DecorrState,
}

fn get_subblock(br: &mut ByteReader) -> DecoderResult<(u8, usize)> {
    let id1                             = br.read_byte()?;
    let id = id1 & 0x3F;
    let mut len = 2 * if (id1 & 0x80) == 0 {
                                          br.read_byte()? as usize
        } else {
                                          br.read_u24le()? as usize
        };
    if (id1 & 0x40) != 0 {
        validate!(len > 0);
        len -= 1;
    }
    Ok((id, len))
}

trait Output {
    fn set(&mut self, val: i32);
}

impl Output for i16 {
    fn set(&mut self, val: i32) { *self = val as i16; }
}
impl Output for i32 {
    fn set(&mut self, val: i32) { *self = val; }
}

fn unpack_mono<T: Output+Copy>(br: &mut BitReader, ebr: &mut BitReader, state: &mut DecState, decorr: &mut DecorrState, dst: &mut [T], len: usize) -> DecoderResult<u32> {
    let mut crc = 0xFFFFFFFFu32;

    for (i, dst) in dst[..len].iter_mut().enumerate() {
        let mut l = state.read_sample(br, 0)?;
        for decorr in decorr.decorr[..decorr.num_decorr].iter_mut() {
            l = decorr.decorrelate_mono(l, i & 7);
        }
        crc = crc.wrapping_mul(3).wrapping_add(l as u32);
        l = state.produce_sample(ebr, l);
        dst.set(l);
    }

    Ok(crc)
}

#[allow(clippy::too_many_arguments)]
fn unpack_stereo<T: Output+Copy>(br: &mut BitReader, ebr: &mut BitReader, state: &mut DecState, decorr: &mut DecorrState, dst: &mut [T], off0: usize, off1: usize, len: usize, is_joint: bool) -> DecoderResult<u32> {
    let mut crc = 0xFFFFFFFFu32;

    for i in 0..len {
        let mut l = state.read_sample(br, 0)?;
        let mut r = state.read_sample(br, 1)?;
        for decorr in decorr.decorr[..decorr.num_decorr].iter_mut() {
            let (pl, pr) = decorr.decorrelate_stereo(l, r, i & 7);
            l = pl;
            r = pr;
        }
        if is_joint {
            r -= l >> 1;
            l += r;
        }
        crc = crc.wrapping_mul(3).wrapping_add(l as u32).wrapping_mul(3).wrapping_add(r as u32);
        l = state.produce_sample(ebr, l);
        r = state.produce_sample(ebr, r);
        dst[off0 + i].set(l);
        dst[off1 + i].set(r);
    }

    Ok(crc)
}

fn unpack_mono_hyb<T: Output+Copy>(br: &mut BitReader, ebr: &mut BitReader, state: &mut DecState, decorr: &mut DecorrState, dst: &mut [T], len: usize) -> DecoderResult<u32> {
    let mut crc = 0xFFFFFFFFu32;

    for (i, dst) in dst[..len].iter_mut().enumerate() {
        let mut l = state.read_sample_hyb(br, 0)?;
        for decorr in decorr.decorr[..decorr.num_decorr].iter_mut() {
            l = decorr.decorrelate_mono(l, i & 7);
        }
        crc = crc.wrapping_mul(3).wrapping_add(l as u32);
        l = state.produce_sample_hyb(ebr, l);
        dst.set(l);
    }

    Ok(crc)
}

#[allow(clippy::too_many_arguments)]
fn unpack_stereo_hyb<T: Output+Copy>(br: &mut BitReader, ebr: &mut BitReader, state: &mut DecState, decorr: &mut DecorrState, dst: &mut [T], off0: usize, off1: usize, len: usize, is_joint: bool) -> DecoderResult<u32> {
    let mut crc = 0xFFFFFFFFu32;

    for i in 0..len {
        let mut l = state.read_sample_hyb(br, 0)?;
        let mut r = state.read_sample_hyb(br, 1)?;
        for decorr in decorr.decorr[..decorr.num_decorr].iter_mut() {
            let (pl, pr) = decorr.decorrelate_stereo(l, r, i & 7);
            l = pl;
            r = pr;
        }
        if is_joint {
            r -= l >> 1;
            l += r;
        }
        crc = crc.wrapping_mul(3).wrapping_add(l as u32).wrapping_mul(3).wrapping_add(r as u32);
        l = state.produce_sample_hyb(ebr, l);
        r = state.produce_sample_hyb(ebr, r);
        dst[off0 + i].set(l);
        dst[off1 + i].set(r);
    }

    Ok(crc)
}

impl WavPackDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            chmap:      NAChannelMap::new(),
            header:     WVHeader::default(),

            dstate:     DecorrState::new(),
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn decode_block(&mut self, hdr: &WVHeader, src: &[u8], start_ch: usize, abuf: &mut NABufferType) -> DecoderResult<()> {
        let mut mr = MemoryReader::new_read(src);
        let mut br = ByteReader::new(&mut mr);
        let mut has_terms = false;
        let mut has_weights = false;
        let mut has_samples = false;
        let mut has_hybrid = false;
        let is_mono = (hdr.flags & (WV_FLAG_MONO | WV_FLAG_FALSE_STEREO)) != 0;
        let cur_channels = if is_mono { 1 } else { 2 };
        let mut data_pos = 0;
        let mut data_len = 0;
        let mut ebits_pos = 0;
        let mut ebits_len = 0;
        let mut dec_state = DecState::default();
        self.dstate.reset();
        dec_state.post_shift = if (hdr.get_bits() & 8) != 0 { 8 } else { 0 };
        dec_state.post_shift += ((hdr.flags >> 13) & 0x1F) as u8;
        validate!(dec_state.post_shift < 32);
        dec_state.hyb_max = 0x7FFFFFFF >> (32 - hdr.get_bits());
        dec_state.hyb_min = -dec_state.hyb_max - 1;
        let is_hybrid = (hdr.flags & WV_FLAG_HYBRID) != 0;
        let has_hybrid_br = (hdr.flags & WV_FLAG_HYB_BITRATE) != 0;
        dec_state.has_hbr = has_hybrid_br;
        dec_state.stereo  = !is_mono;
        while br.left() > 0 {
            let (id, len) = get_subblock(&mut br)?;
            match id {
                0x02 => { //decorr terms
                    self.dstate.num_decorr = len;
                    validate!(self.dstate.num_decorr <= self.dstate.decorr.len());
                    for decorr in self.dstate.decorr[..self.dstate.num_decorr].iter_mut().rev() {
                        let val         = br.read_byte()?;
                        decorr.value = i32::from(val & 0x1F) - 5;
                        decorr.delta = i32::from(val >> 5);
                    }
                    has_terms = true;
                },
                0x03 => { //decorr weights
                    validate!(has_terms);
                    validate!(len <= self.dstate.num_decorr * cur_channels);
                    for decorr in self.dstate.decorr[..self.dstate.num_decorr].iter_mut().rev().take(len / cur_channels) {
                        let val         = br.read_byte()? as i8;
                        decorr.weight_a = i32::from(val) << 3;
                        if decorr.weight_a > 0 {
                            decorr.weight_a += (decorr.weight_a + 64) >> 7;
                        }
                        if !is_mono {
                            let val     = br.read_byte()? as i8;
                            decorr.weight_b = i32::from(val) << 3;
                            if decorr.weight_b > 0 {
                                decorr.weight_b += (decorr.weight_b + 64) >> 7;
                            }
                        }
                    }
                    has_weights = true;
                },
                0x04 => { //decorr samples
                    validate!(has_weights);
                    let end = br.tell() + (len as u64);
                    for decorr in self.dstate.decorr[..self.dstate.num_decorr].iter_mut().rev() {
                        if br.tell() == end {
                            break;
                        }
                        if decorr.value > 8 {
                            let a0      = br.read_u16le()? as i16;
                            let a1      = br.read_u16le()? as i16;
                            decorr.samples_a[0] = wv_log2lin(i32::from(a0));
                            decorr.samples_a[1] = wv_log2lin(i32::from(a1));
                            if !is_mono {
                                let b0  = br.read_u16le()? as i16;
                                let b1  = br.read_u16le()? as i16;
                                decorr.samples_b[0] = wv_log2lin(i32::from(b0));
                                decorr.samples_b[1] = wv_log2lin(i32::from(b1));
                            }
                        } else if decorr.value < 0 {
                            let a0      = br.read_u16le()? as i16;
                            let b0      = br.read_u16le()? as i16;
                            decorr.samples_a[0] = wv_log2lin(i32::from(a0));
                            decorr.samples_b[0] = wv_log2lin(i32::from(b0));
                        } else {
                            let len = decorr.value as usize;
                            for i in 0..len {
                                let a   = br.read_u16le()? as i16;
                                decorr.samples_a[i] = wv_log2lin(i32::from(a));
                                if !is_mono {
                                    let b = br.read_u16le()? as i16;
                                    decorr.samples_b[i] = wv_log2lin(i32::from(b));
                                }
                            }
                        }
                    }
                    has_samples = true;
                },
                0x05 => { //entropy vars
                    validate!(len == 6 * cur_channels);
                    for el in dec_state.median[0].iter_mut() {
                        *el             = wv_log2lin(i32::from(br.read_u16le()? as i16)) as u32;
                    }
                    if !is_mono {
                        for el in dec_state.median[1].iter_mut() {
                            *el         = wv_log2lin(i32::from(br.read_u16le()? as i16)) as u32;
                        }
                    }
                },
                0x06 => { //hybrid parameters
                    validate!(is_hybrid);
                    let end = br.tell() + (len as u64);
                    if has_hybrid_br {
                        for el in dec_state.slow_level.iter_mut().take(cur_channels) {
                            *el         = wv_log2lin(i32::from(br.read_u16le()? as i16)) as u32;
                        }
                    }
                    for el in dec_state.br_acc.iter_mut().take(cur_channels) {
                        *el             = u32::from(br.read_u16le()?) << 16;
                    }
                    if br.tell() < end {
                        for el in dec_state.br_delta.iter_mut().take(cur_channels) {
                            *el         = wv_log2lin(i32::from(br.read_u16le()? as i16)) as u32;
                        }
                    }
                    validate!(br.tell() == end);
                    has_hybrid = true;
                },
                0x08 => {
                    validate!((hdr.flags & WV_FLAG_FLOATS) != 0);
                    return Err(DecoderError::NotImplemented);
                },
                0x09 => {
                    validate!((hdr.flags & WV_FLAG_EXT_INTEGERS) != 0);
                    validate!(len == 4);
                    let ebits           = br.read_byte()?;
                    let mode1           = br.read_byte()?;
                    let mode2           = br.read_byte()?;
                    let mode3           = br.read_byte()?;
                    if ebits != 0 {
                        dec_state.ebits = ebits;
                    } else if mode1 != 0 {
                        dec_state.shift = mode1;
                    } else if mode2 != 0 {
                        dec_state.and = 1;
                        dec_state.or  = 1;
                        dec_state.shift = mode2;
                    } else if mode3 != 0 {
                        dec_state.and = 1;
                        dec_state.shift = mode3;
                    }
                    if is_hybrid && hdr.get_bits() == 32 && dec_state.post_shift < 8 && dec_state.shift > 8 {
                        dec_state.post_shift    += 8;
                        dec_state.shift         -= 8;
                        dec_state.hyb_min       >>= 8;
                        dec_state.hyb_max       >>= 8;
                    }
                },
                0x0A => { // normal stream
                    validate!(has_samples);
                    data_pos = br.tell() as usize;
                    data_len = len;
                    br.read_skip(len)?;
                },
                0x0C => {
                    ebits_pos = br.tell() as usize;
                    ebits_len = len;
                    br.read_skip(len)?;
                },
                0x0E => return Err(DecoderError::NotImplemented), // DSD
                _ => { br.read_skip(len)?; },
            };
            if (len & 1) != 0 {
                                        br.read_skip(1)?;
            }
        }
        validate!(data_pos > 0 && data_len > 0);
        if is_hybrid {
            validate!(has_hybrid);
        }

        let mut br = BitReader::new(&src[data_pos..][..data_len], BitReaderMode::LE);
        let mut ebr = BitReader::new(&src[ebits_pos..][..ebits_len], BitReaderMode::LE);
        if is_mono {
            let is_fstereo = (hdr.flags & WV_FLAG_FALSE_STEREO) != 0;
            match abuf {
                NABufferType::AudioI16(ref mut adata) => {
                    let off0 = adata.get_offset(start_ch);
                    let off1 = adata.get_offset(start_ch + 1);
                    let dst = adata.get_data_mut().unwrap();
                    let crc = if !is_hybrid {
                            unpack_mono(&mut br, &mut ebr, &mut dec_state, &mut self.dstate, &mut dst[off0..], hdr.block_samples as usize)?
                        } else {
                            unpack_mono_hyb(&mut br, &mut ebr, &mut dec_state, &mut
 self.dstate, &mut dst[off0..], hdr.block_samples as usize)?
                        };
                    if crc != hdr.crc {
                        return Err(DecoderError::ChecksumError);
                    }
                    if is_fstereo {
                        for i in 0..(hdr.block_samples as usize) {
                            dst[off1 + i] = dst[off0 + i];
                        }
                    }
                },
                NABufferType::AudioI32(ref mut adata) => {
                    let off0 = adata.get_offset(start_ch);
                    let off1 = adata.get_offset(start_ch + 1);
                    let dst = adata.get_data_mut().unwrap();
                    let crc = if !is_hybrid {
                            unpack_mono(&mut br, &mut ebr, &mut dec_state, &mut self.dstate, &mut dst[off0..], hdr.block_samples as usize)?
                        } else {
                            unpack_mono_hyb(&mut br, &mut ebr, &mut dec_state, &mut
 self.dstate, &mut dst[off0..], hdr.block_samples as usize)?
                        };
                    if crc != hdr.crc {
                        return Err(DecoderError::ChecksumError);
                    }
                    if is_fstereo {
                        for i in 0..(hdr.block_samples as usize) {
                            dst[off1 + i] = dst[off0 + i];
                        }
                    }
                },
                _ => unreachable!(),
            }
        } else {
            let is_joint = (hdr.flags & WV_FLAG_JSTEREO) != 0;
            match abuf {
                NABufferType::AudioI16(ref mut adata) => {
                    let off0 = adata.get_offset(start_ch);
                    let off1 = adata.get_offset(start_ch + 1);
                    let dst = adata.get_data_mut().unwrap();
                    let crc = if !is_hybrid {
                            unpack_stereo(&mut br, &mut ebr, &mut dec_state, &mut self.dstate, dst, off0, off1, hdr.block_samples as usize, is_joint)?
                        } else {
                            unpack_stereo_hyb(&mut br, &mut ebr, &mut dec_state, &mut self.dstate, dst, off0, off1, hdr.block_samples as usize, is_joint)?
                        };
                    if crc != hdr.crc {
                        return Err(DecoderError::ChecksumError);
                    }
                },
                NABufferType::AudioI32(ref mut adata) => {
                    let off0 = adata.get_offset(start_ch);
                    let off1 = adata.get_offset(start_ch + 1);
                    let dst = adata.get_data_mut().unwrap();
                    let crc = if !is_hybrid {
                            unpack_stereo(&mut br, &mut ebr, &mut dec_state, &mut self.dstate, dst, off0, off1, hdr.block_samples as usize, is_joint)?
                        } else {
                            unpack_stereo_hyb(&mut br, &mut ebr, &mut dec_state, &mut self.dstate, dst, off0, off1, hdr.block_samples as usize, is_joint)?
                        };
                    if crc != hdr.crc {
                        return Err(DecoderError::ChecksumError);
                    }
                },
                _ => unreachable!(),
            }
        }

        Ok(())
    }
}

impl NADecoder for WavPackDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(_ainfo) = info.get_properties() {
            if let Some(buf) = info.get_extradata() {
                let mut channels = 0;
                let mut decl_channels = 0;
                let mut channel_map = 0;
                let mut off = 0;
                while off < buf.len() {
                    let hdr = WVHeader::parse(&buf[off..])?;
                    if (hdr.flags & (WV_FLAG_FLOATS | WV_FLAG_DSD_AUDIO)) != 0 {
                        return Err(DecoderError::NotImplemented);
                    }
                    self.header = hdr;
                    off += WV_HEADER_SIZE;
                    let size = hdr.get_size();
                    let mut mr = MemoryReader::new_read(&buf[off..][..size]);
                    let mut br = ByteReader::new(&mut mr);
                    while br.left() > 0 {
                        let (id, len)   = get_subblock(&mut br)?;
                        match id {
                            0xD => {
                                validate!(len > 1);
                                decl_channels = br.read_byte()?;
                                if decl_channels == 0 {
                                    return Err(DecoderError::NotImplemented);
                                }
                                channel_map = match len {
                                        2 => u32::from(br.read_byte()?),
                                        3 => u32::from(br.read_u16le()?),
                                        4 => br.read_u24le()?,
                                        5 => br.read_u32le()?,
                                        _ => return Err(DecoderError::NotImplemented),
                                    };
                            },
                            _ =>          br.read_skip(len)?,
                        };
                        if (len & 1) != 0 {
                                          br.read_skip(1)?;
                        }
                    }
                    channels += hdr.get_num_channels();

                    if hdr.is_end_block() {
                        break;
                    }
                    off += size;
                }
                if decl_channels != 0 {
                    validate!(decl_channels == channels);
                }
                self.chmap = if channel_map != 0 {
                        NAChannelMap::from_ms_mapping(channel_map)
                    } else if channels == 1 {
                        NAChannelMap::from_str("C").unwrap()
                    } else if channels == 2 {
                        NAChannelMap::from_str("L,R").unwrap()
                    } else {
                        return Err(DecoderError::NotImplemented);
                    };
                let bsamps = if self.header.block_samples == 0 {
                        self.header.get_sample_rate()
                    } else {
                        self.header.block_samples
                    } as usize;

                let bits = self.header.get_bits();
                let fmt = if (self.header.flags & WV_FLAG_FLOATS) != 0 {
                        SND_F32P_FORMAT
                    } else if bits <= 16 {
                        SND_S16P_FORMAT
                    } else {
                        SND_S32P_FORMAT
                    };

                self.ainfo = NAAudioInfo::new(self.header.get_sample_rate(), channels, fmt, bsamps);
                Ok(())
            } else {
                Err(DecoderError::InvalidData)
            }
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            validate!(pktbuf.len() > WV_HEADER_SIZE);
            let refhdr = WVHeader::parse(&pktbuf)?;

            if refhdr.block_samples == 0 {
                let mut frm = NAFrame::new_from_pkt(pkt, info, NABufferType::None);
                frm.set_frame_type(FrameType::Skip);
                frm.set_keyframe(false);
                return Ok(frm.into_ref());
            }

            let mut abuf = alloc_audio_buffer(self.ainfo, refhdr.block_samples as usize, self.chmap.clone())?;
            let mut start_ch = 0;
            let mut roff = 0;
            let mut first = true;
            let mut refhdr = WVHeader::default();
            loop {
                let hdr = WVHeader::parse(&pktbuf[roff..])?;
                if first {
                    validate!(hdr.is_start_block());
                    validate!(self.header.stream_eq(&hdr));
                    refhdr = hdr;
                    first = false;
                } else {
                    validate!(refhdr.block_eq(&hdr));
                }
                roff += WV_HEADER_SIZE;
                let blk_size = hdr.get_size();
                self.decode_block(&hdr, &pktbuf[roff..][..blk_size], start_ch, &mut abuf)?;
                roff += blk_size;
                if hdr.is_end_block() {
                    break;
                }
                start_ch += hdr.get_num_channels() as usize;
            }
            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
            frm.set_duration(Some(u64::from(refhdr.block_samples)));
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for WavPackDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(WavPackDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::llaudio_register_all_decoders;
    use crate::llaudio_register_all_demuxers;
    // samples come from the official WavPack test samples set
    #[test]
    fn test_wavpack_8bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("wavpack", "wavpack", "assets/LLaudio/wv/8bit-partial.wv", Some(100000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x8157bf0f, 0xeb441905, 0xeb6b815d, 0x113480a8]));
    }
    #[test]
    fn test_wavpack_12bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("wavpack", "wavpack", "assets/LLaudio/wv/12bit-partial.wv", Some(100000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xe5faf18c, 0xbf2d3b12, 0x5b0b8f00, 0x162b805a]));
    }
    #[test]
    fn test_wavpack_16bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("wavpack", "wavpack", "assets/LLaudio/wv/16bit-partial.wv", Some(100000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xaf31b252, 0xdf8b282a, 0x2dc38947, 0xf64c68a1]));
    }
    #[test]
    fn test_wavpack_24bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("wavpack", "wavpack", "assets/LLaudio/wv/24bit-partial.wv", Some(100000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xf5649972, 0xfe757241, 0x383d5ded, 0x0176a75b]));
    }
    #[test]
    fn test_wavpack_hybrid() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("wavpack", "wavpack", "assets/LLaudio/wv/4.0_16-bit.wv", Some(100000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x9cfa469a, 0x54af50e1, 0xe45434d1, 0x1bf987e2]));
    }
    #[test]
    fn test_wavpack_hybrid_32bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("wavpack", "wavpack", "assets/LLaudio/wv/4.0_32-bit_int.wv", Some(100000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x21415549, 0xf48ddb55, 0xef5c4e7f, 0xa48d5ab9]));
    }
}

const WV_EXP_TABLE: [u8; 256] = [
    0x00, 0x01, 0x01, 0x02, 0x03, 0x03, 0x04, 0x05,
    0x06, 0x06, 0x07, 0x08, 0x08, 0x09, 0x0a, 0x0b,
    0x0b, 0x0c, 0x0d, 0x0e, 0x0e, 0x0f, 0x10, 0x10,
    0x11, 0x12, 0x13, 0x13, 0x14, 0x15, 0x16, 0x16,
    0x17, 0x18, 0x19, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
    0x1d, 0x1e, 0x1f, 0x20, 0x20, 0x21, 0x22, 0x23,
    0x24, 0x24, 0x25, 0x26, 0x27, 0x28, 0x28, 0x29,
    0x2a, 0x2b, 0x2c, 0x2c, 0x2d, 0x2e, 0x2f, 0x30,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x35, 0x36,
    0x37, 0x38, 0x39, 0x3a, 0x3a, 0x3b, 0x3c, 0x3d,
    0x3e, 0x3f, 0x40, 0x41, 0x41, 0x42, 0x43, 0x44,
    0x45, 0x46, 0x47, 0x48, 0x48, 0x49, 0x4a, 0x4b,
    0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x51, 0x52,
    0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a,
    0x5b, 0x5c, 0x5d, 0x5e, 0x5e, 0x5f, 0x60, 0x61,
    0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
    0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71,
    0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
    0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x80, 0x81,
    0x82, 0x83, 0x84, 0x85, 0x87, 0x88, 0x89, 0x8a,
    0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91, 0x92,
    0x93, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b,
    0x9c, 0x9d, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4,
    0xa5, 0xa6, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad,
    0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb6, 0xb7,
    0xb8, 0xb9, 0xba, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0,
    0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc8, 0xc9, 0xca,
    0xcb, 0xcd, 0xce, 0xcf, 0xd0, 0xd2, 0xd3, 0xd4,
    0xd6, 0xd7, 0xd8, 0xd9, 0xdb, 0xdc, 0xdd, 0xde,
    0xe0, 0xe1, 0xe2, 0xe4, 0xe5, 0xe6, 0xe8, 0xe9,
    0xea, 0xec, 0xed, 0xee, 0xf0, 0xf1, 0xf2, 0xf4,
    0xf5, 0xf6, 0xf8, 0xf9, 0xfa, 0xfc, 0xfd, 0xff
];
const WV_LOG_TABLE: [u8; 256] = [
    0x00, 0x01, 0x03, 0x04, 0x06, 0x07, 0x09, 0x0a,
    0x0b, 0x0d, 0x0e, 0x10, 0x11, 0x12, 0x14, 0x15,
    0x16, 0x18, 0x19, 0x1a, 0x1c, 0x1d, 0x1e, 0x20,
    0x21, 0x22, 0x24, 0x25, 0x26, 0x28, 0x29, 0x2a,
    0x2c, 0x2d, 0x2e, 0x2f, 0x31, 0x32, 0x33, 0x34,
    0x36, 0x37, 0x38, 0x39, 0x3b, 0x3c, 0x3d, 0x3e,
    0x3f, 0x41, 0x42, 0x43, 0x44, 0x45, 0x47, 0x48,
    0x49, 0x4a, 0x4b, 0x4d, 0x4e, 0x4f, 0x50, 0x51,
    0x52, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a,
    0x5c, 0x5d, 0x5e, 0x5f, 0x60, 0x61, 0x62, 0x63,
    0x64, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c,
    0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x74, 0x75,
    0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d,
    0x7e, 0x7f, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85,
    0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d,
    0x8e, 0x8f, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95,
    0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9b, 0x9c,
    0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4,
    0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xa9, 0xaa, 0xab,
    0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb2,
    0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xb9,
    0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc0,
    0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc6, 0xc7,
    0xc8, 0xc9, 0xca, 0xcb, 0xcb, 0xcc, 0xcd, 0xce,
    0xcf, 0xd0, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd4,
    0xd5, 0xd6, 0xd7, 0xd8, 0xd8, 0xd9, 0xda, 0xdb,
    0xdc, 0xdc, 0xdd, 0xde, 0xdf, 0xe0, 0xe0, 0xe1,
    0xe2, 0xe3, 0xe4, 0xe4, 0xe5, 0xe6, 0xe7, 0xe7,
    0xe8, 0xe9, 0xea, 0xea, 0xeb, 0xec, 0xed, 0xee,
    0xee, 0xef, 0xf0, 0xf1, 0xf1, 0xf2, 0xf3, 0xf4,
    0xf4, 0xf5, 0xf6, 0xf7, 0xf7, 0xf8, 0xf9, 0xf9,
    0xfa, 0xfb, 0xfc, 0xfc, 0xfd, 0xfe, 0xff, 0xff
];
