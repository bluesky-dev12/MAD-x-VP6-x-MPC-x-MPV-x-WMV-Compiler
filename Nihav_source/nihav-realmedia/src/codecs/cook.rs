use nihav_core::formats::*;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::dsp::mdct::IMDCT;
use nihav_core::io::bitreader::*;
use nihav_core::io::byteio::{ByteReader, MemoryReader};
use nihav_core::io::codebook::*;
use nihav_core::io::intcode::*;
use std::f32::consts;
use std::mem::swap;
use super::cookdata::*;

#[derive(Debug,Clone,Copy,PartialEq)]
enum Mode {
    Mono,
    Stereo,
    JointStereo,
}

impl Mode {
    fn get_channels(self) -> usize {
        match self {
            Mode::Mono  => 1,
            _           => 2,
        }
    }
}

struct CookBookReader {
    bits:  &'static [u8],
    codes: &'static [u16],
}
impl CodebookDescReader<u16> for CookBookReader {
    fn bits(&mut self, idx: usize) -> u8  { self.bits[idx] }
    fn code(&mut self, idx: usize) -> u32 { self.codes[idx] as u32 }
    fn sym (&mut self, idx: usize) -> u16 { idx as u16 }
    fn len(&mut self) -> usize { self.bits.len() }
}

struct Codebooks {
    cpl_cb:     [Codebook<u16>; 5],
    quant_cb:   Vec<Codebook<u16>>,
    vq_cb:      [Codebook<u16>; 7],
}

impl Codebooks {
    fn new() -> Self {
        let mut cpl0 = CookBookReader { codes: COOK_CPL_2BITS_CODES, bits: COOK_CPL_2BITS_BITS };
        let mut cpl1 = CookBookReader { codes: COOK_CPL_3BITS_CODES, bits: COOK_CPL_3BITS_BITS };
        let mut cpl2 = CookBookReader { codes: COOK_CPL_4BITS_CODES, bits: COOK_CPL_4BITS_BITS };
        let mut cpl3 = CookBookReader { codes: COOK_CPL_5BITS_CODES, bits: COOK_CPL_5BITS_BITS };
        let mut cpl4 = CookBookReader { codes: COOK_CPL_6BITS_CODES, bits: COOK_CPL_6BITS_BITS };
        let cpl_cb = [Codebook::new(&mut cpl0, CodebookMode::MSB).unwrap(),
                      Codebook::new(&mut cpl1, CodebookMode::MSB).unwrap(),
                      Codebook::new(&mut cpl2, CodebookMode::MSB).unwrap(),
                      Codebook::new(&mut cpl3, CodebookMode::MSB).unwrap(),
                      Codebook::new(&mut cpl4, CodebookMode::MSB).unwrap()];
        let mut quant_cb: Vec<Codebook<u16>> = Vec::with_capacity(COOK_QUANT_CODES.len());
        for i in 0..COOK_QUANT_CODES.len() {
            let mut quant = CookBookReader { codes: COOK_QUANT_CODES[i], bits: COOK_QUANT_BITS[i] };
            quant_cb.push(Codebook::new(&mut quant, CodebookMode::MSB).unwrap());
        }
        let mut vq0 = CookBookReader { codes: COOK_VQ0_CODES, bits: COOK_VQ0_BITS };
        let mut vq1 = CookBookReader { codes: COOK_VQ1_CODES, bits: COOK_VQ1_BITS };
        let mut vq2 = CookBookReader { codes: COOK_VQ2_CODES, bits: COOK_VQ2_BITS };
        let mut vq3 = CookBookReader { codes: COOK_VQ3_CODES, bits: COOK_VQ3_BITS };
        let mut vq4 = CookBookReader { codes: COOK_VQ4_CODES, bits: COOK_VQ4_BITS };
        let mut vq5 = CookBookReader { codes: COOK_VQ5_CODES, bits: COOK_VQ5_BITS };
        let mut vq6 = CookBookReader { codes: COOK_VQ6_CODES, bits: COOK_VQ6_BITS };
        let vq_cb = [Codebook::new(&mut vq0, CodebookMode::MSB).unwrap(),
                     Codebook::new(&mut vq1, CodebookMode::MSB).unwrap(),
                     Codebook::new(&mut vq2, CodebookMode::MSB).unwrap(),
                     Codebook::new(&mut vq3, CodebookMode::MSB).unwrap(),
                     Codebook::new(&mut vq4, CodebookMode::MSB).unwrap(),
                     Codebook::new(&mut vq5, CodebookMode::MSB).unwrap(),
                     Codebook::new(&mut vq6, CodebookMode::MSB).unwrap()];
        Codebooks {
            cpl_cb,
            quant_cb,
            vq_cb,
        }
    }
}

struct CookDSP {
    imdct:      IMDCT,
    window:     [f32; 1024],
    out:        [f32; 2048],
    size:       usize,
    pow_tab:    [f32; 128],
    hpow_tab:   [f32; 128],
    gain_tab:   [f32; 23],
}

impl CookDSP {
    fn new(samples: usize) -> Self {
        let fsamples = samples as f32;
        let mut window: [f32; 1024] = [0.0; 1024];
        let factor = consts::PI / (2.0 * fsamples);
        let scale = (2.0 / fsamples).sqrt() / 32768.0;
        for k in 0..samples {
            window[k] = (factor * ((k as f32) + 0.5)).sin() * scale;
        }
        let mut pow_tab: [f32; 128] = [0.0; 128];
        let mut hpow_tab: [f32; 128] = [0.0; 128];
        for i in 0..128 {
            pow_tab[i]  = 2.0f32.powf((i as f32) - 64.0);
            hpow_tab[i] = 2.0f32.powf(((i as f32) - 64.0) * 0.5);
        }
        let mut gain_tab: [f32; 23] = [0.0; 23];
        for i in 0..23 {
            gain_tab[i] = pow_tab[i + 53].powf(8.0 / fsamples);
        }
        let size = samples;
        CookDSP { imdct: IMDCT::new(samples*2, false), window, out: [0.0; 2048], size, pow_tab, hpow_tab, gain_tab }
    }
}

trait ClipCat {
    fn clip_cat(&self) -> usize;
}

impl ClipCat for i32 {
    fn clip_cat(&self) -> usize { ((*self).max(0) as usize).min(NUM_CATEGORIES - 1) }
}

const MAX_SAMPLES: usize = MAX_SUBBANDS * BAND_SIZE;
const MAX_PAIRS: usize = 5;

#[derive(Clone,Copy)]
struct CookChannelPair {
    start_ch:       usize,
    mode:           Mode,
    samples:        usize,
    subbands:       usize,
    js_start:       usize,
    js_bits:        u8,
    vector_bits:    u8,

    decouple:       [u8; BAND_SIZE],
    category:       [u8; MAX_SUBBANDS * 2],

    block:          [[f32; MAX_SAMPLES * 2]; 2],
    delay:          [[f32; MAX_SAMPLES]; 2],
    gains:          [[i32; 9]; 2],
    prev_gains:     [[i32; 9]; 2],
    qindex:         [i8; MAX_SUBBANDS * 2],
}

impl CookChannelPair {
    fn new() -> Self {
        CookChannelPair {
            start_ch:       0,
            mode:           Mode::Mono,
            samples:        0,
            subbands:       0,
            js_start:       0,
            js_bits:        0,
            vector_bits:    0,

            decouple:       [0; BAND_SIZE],
            category:       [0; MAX_SUBBANDS * 2],

            block:          [[0.0; MAX_SAMPLES * 2]; 2],
            delay:          [[0.0; MAX_SAMPLES]; 2],
            gains:          [[0; 9]; 2],
            prev_gains:     [[0; 9]; 2],
            qindex:         [0; MAX_SUBBANDS * 2],
        }
    }
    fn read_hdr_v1(&mut self, br: &mut ByteReader) -> DecoderResult<()> {
        let ver                                         = br.read_u32be()?;
        let micro_ver = ver & 0xFF;
        self.samples                                    = br.read_u16be()? as usize;
        validate!(self.samples > 0 && ((self.samples & (self.samples - 1)) == 0));
        self.subbands                                   = br.read_u16be()? as usize;
        validate!(self.subbands <= MAX_SUBBANDS);
        match micro_ver {
            1 => {
                    self.mode       = Mode::Mono;
                    self.js_start   = 0;
                    self.js_bits    = 0;
                },
            2 => {
                    self.mode       = Mode::Stereo;
                    self.js_start   = 0;
                    self.js_bits    = 0;
                },
            3 => {
                    self.mode       = Mode::JointStereo;
                    let _delay                          = br.read_u32be()?;
                    self.js_start                       = br.read_u16be()? as usize;
                    self.js_bits                        = br.read_u16be()? as u8;
                    validate!(self.js_start < MAX_SUBBANDS);
                    validate!((self.js_bits >= 2) && (self.js_bits <= 6));
                },
            _ => { return Err(DecoderError::InvalidData);}
        }
        Ok(())
    }
    fn read_hdr_v2(&mut self, br: &mut ByteReader) -> DecoderResult<u32> {
        let ver                                         = br.read_u32be()?;
        validate!((ver >> 24) == 2);
        self.samples                                    = br.read_u16be()? as usize;
        self.subbands                                   = br.read_u16be()? as usize;
        validate!(self.subbands <= MAX_SUBBANDS);
        let _delay                                      = br.read_u32be()?;
        self.js_start                                   = br.read_u16be()? as usize;
        validate!(self.js_start < MAX_SUBBANDS);
        let js_bits                                     = br.read_u16be()?;
        let chmap                                       = br.read_u32be()?;
        if chmap.count_ones() == 1 {
            self.js_bits    = 0;
            self.mode       = Mode::Mono;
        } else {
            validate!((js_bits >= 2) && (js_bits <= 6));
            self.js_bits    = js_bits as u8;
            self.mode       = Mode::JointStereo;
        }
        Ok(chmap)
    }
    fn bitalloc(&mut self, num_vectors: usize, bits: usize) {
        let avail_bits = (if bits > self.samples { self.samples + ((bits - self.samples) * 5) / 8 } else { bits }) as i32;
        let total_subbands = self.subbands + self.js_start;

        let mut bias: i32 = -32;
        for i in 0..6 {
            let mut sum = 0;
            for j in 0..total_subbands {
                let idx = ((32 >> i) + bias - (self.qindex[j] as i32)) / 2;
                sum += COOK_EXP_BITS[idx.clip_cat()];
            }
            if sum >= (avail_bits - 32) {
                bias += 32 >> i;
            }
        }

        let mut exp_index1: [usize; MAX_SUBBANDS * 2] = [0; MAX_SUBBANDS * 2];
        let mut exp_index2: [usize; MAX_SUBBANDS * 2] = [0; MAX_SUBBANDS * 2];
        let mut sum = 0;
        for i in 0..total_subbands {
            let idx = ((bias - (self.qindex[i] as i32)) / 2).clip_cat();
            sum += COOK_EXP_BITS[idx];
            exp_index1[i] = idx;
            exp_index2[i] = idx;
        }

        let mut tbias1 = sum;
        let mut tbias2 = sum;
        let mut tcat: [usize; 128*2] = [0; 128*2];
        let mut tcat_idx1 = 128;
        let mut tcat_idx2 = 128;
        for _ in 1..(1 << self.vector_bits) {
            if tbias1 + tbias2 > avail_bits * 2 {
                let mut max = -999999;
                let mut idx = total_subbands + 1;
                for j in 0..total_subbands {
                    if exp_index1[j] >= (NUM_CATEGORIES - 1) { continue; }
                    let t = -2 * (exp_index1[j] as i32) - (self.qindex[j] as i32) + bias;
                    if t >= max {
                        max = t;
                        idx = j;
                    }
                }
                if idx >= total_subbands { break; }
                tcat[tcat_idx1] = idx;
                tcat_idx1 += 1;
                tbias1 -= COOK_EXP_BITS[exp_index1[idx]] - COOK_EXP_BITS[exp_index1[idx] + 1];
                exp_index1[idx] += 1;
            } else {
                let mut min = 999999;
                let mut idx = total_subbands + 1;
                for j in 0..total_subbands {
                    if exp_index2[j] == 0 { continue; }
                    let t = -2 * (exp_index2[j] as i32) - (self.qindex[j] as i32) + bias;
                    if t < min {
                        min = t;
                        idx = j;
                    }
                }
                if idx >= total_subbands { break; }
                tcat_idx2 -= 1;
                tcat[tcat_idx2] = idx;
                tbias2 -= COOK_EXP_BITS[exp_index2[idx]] - COOK_EXP_BITS[exp_index2[idx] - 1];
                exp_index2[idx] -= 1;
            }
        }
        for i in 0..total_subbands {
            self.category[i] = exp_index2[i] as u8;
        }

        for _ in 0..num_vectors {
            let idx = tcat[tcat_idx2];
            tcat_idx2 += 1;
            self.category[idx] = (self.category[idx] + 1).min((NUM_CATEGORIES - 1) as u8);
        }
    }
    fn decode_channel_data(&mut self, dsp: &mut CookDSP, rnd: &mut RND, codebooks: &Codebooks, src: &[u8], buf: &mut [u8], channel: usize) -> DecoderResult<()> {
        // decrypt
        for (i, b) in src.iter().enumerate() {
            buf[i] = b ^ COOK_XOR_KEY[i & 3];
        }
        let mut br = BitReader::new(&buf[..src.len()], BitReaderMode::BE);

        let num_gains                                   = br.read_code(UintCodeType::UnaryOnes)? as usize;
        validate!(num_gains <= 8);

        swap(&mut self.gains[channel], &mut self.prev_gains[channel]);
        self.block[channel] = [0.0; MAX_SAMPLES * 2];

        // gains
        let mut ipos = 0;
        for _ in 0..num_gains {
            let idx                                     = br.read(3)? as usize;
            let val;
            if br.read_bool()? {
                val                                     = (br.read(4)? as i32) - 7;
            } else {
                val = -1;
            }
            validate!(idx >= ipos);
            while ipos <= idx {
                self.prev_gains[channel][ipos] = val;
                ipos += 1;
            }
        }
        while ipos <= 8 {
            self.prev_gains[channel][ipos] = 0;
            ipos += 1;
        }

        // coupling information
        if self.mode == Mode::JointStereo {
            let cstart = COOK_CPL_BAND[self.js_start] as usize;
            let cend   = COOK_CPL_BAND[self.subbands - 1] as usize;
            if br.read_bool()? {
                let cb = &codebooks.cpl_cb[(self.js_bits - 2) as usize];
                for i in cstart..=cend {
                    self.decouple[i]                    = br.read_cb(cb)? as u8;
                }
            } else {
                for i in cstart..=cend {
                    self.decouple[i]                    = br.read(self.js_bits)? as u8;
                }
            }
        }

        // envelope
        let tot_subbands = self.subbands + self.js_start;
        self.qindex[0]                                  = (br.read(6)? as i8) - 6;
        for i in 1..tot_subbands {
            let mut pos = i;
            if pos >= self.js_start * 2 {
                pos -= self.js_start;
            } else {
                pos >>= 1;
            }
            let ipos = ((pos as i8) - 1).max(0).min(12);
            let cb = &codebooks.quant_cb[ipos as usize];
            self.qindex[i]                              = (br.read_cb(cb)? as i8) + self.qindex[i - 1] - 12;
            validate!((self.qindex[i] >= -63) && (self.qindex[i] <= 63));
        }
        let num_vectors                                 = br.read(self.vector_bits)? as usize;
        self.bitalloc(num_vectors, br.left() as usize);

        // coefficients
        self.block[channel] = [0.0; MAX_SAMPLES * 2];
        let mut off = 0;
        for sb in 0..tot_subbands {
            let mut coef_index: [u8; BAND_SIZE] = [0; BAND_SIZE];
            let mut coef_sign:  [bool; BAND_SIZE] = [false; BAND_SIZE];
            let cat = self.category[sb] as usize;
            if (cat < NUM_CATEGORIES - 1) && br.left() > 0 {
                unpack_band(&mut br, codebooks, &mut coef_index, &mut coef_sign, cat)?;
            }
            for i in 0..BAND_SIZE {
                let val;
                if coef_index[i] == 0 {
                    let v = COOK_DITHER_TAB[cat];
                    val = if !rnd.get_sign() { v } else { -v };
                } else {
                    let v = COOK_QUANT_CENTROID[cat][coef_index[i] as usize];
                    val = if !coef_sign[i] { v } else { -v };
                }
                self.block[channel][off + i] = val * dsp.hpow_tab[(self.qindex[sb] + 64) as usize];
            }
            off += BAND_SIZE;
        }

        Ok(())
    }
    fn decode(&mut self, dsp: &mut CookDSP, rnd: &mut RND, codebooks: &Codebooks, src: &[u8], buf: &mut [u8], abuf: &mut NABufferType) -> DecoderResult<()> {
        if self.mode == Mode::Stereo {
            let mut schunk = src.chunks(src.len() / 2);
            self.decode_channel_data(dsp, rnd, codebooks, schunk.next().unwrap(), buf, 0)?;
            self.decode_channel_data(dsp, rnd, codebooks, schunk.next().unwrap(), buf, 1)?;
        } else {
            self.decode_channel_data(dsp, rnd, codebooks, src, buf, 0)?;
        }
        // uncouple joint stereo channels
        if self.mode == Mode::JointStereo {
            for i in 0..self.js_start {
                for j in 0..BAND_SIZE {
                    self.block[1][i * BAND_SIZE + j] = self.block[0][(i * 2 + 1) * BAND_SIZE + j];
                    self.block[0][i * BAND_SIZE + j] = self.block[0][(i * 2)     * BAND_SIZE + j];
                }
            }
            let scale_idx = (self.js_bits as usize) - 2;
            let scale_off = (1 << self.js_bits) as usize;
            for i in self.js_start..self.subbands {
                let idx = self.decouple[COOK_CPL_BAND[i] as usize] as usize;
                let doff = i * BAND_SIZE;
                let soff = (i + self.js_start) * BAND_SIZE;
                let m1 = COOK_CPL_SCALES[scale_idx][            1 + idx];
                let m2 = COOK_CPL_SCALES[scale_idx][scale_off - 1 - idx];
                for j in 0..BAND_SIZE {
                    self.block[0][doff + j] = self.block[0][soff + j] * m1;
                    self.block[1][doff + j] = self.block[0][soff + j] * m2;
                }
            }
            for i in (self.subbands * BAND_SIZE)..MAX_SAMPLES {
                self.block[0][i] = 0.0;
                self.block[1][i] = 0.0;
            }
            self.gains[1] = self.gains[0];
            self.prev_gains[1] = self.prev_gains[0];
        }
        for ch in 0..self.mode.get_channels() {
            let off = abuf.get_offset(ch + self.start_ch);
            let mut adata = abuf.get_abuf_f32().unwrap();
            let output = adata.get_data_mut().unwrap();
            let dst = &mut output[off..];

            dsp.imdct.imdct(&self.block[ch], &mut dsp.out);

            let prev_gain = dsp.pow_tab[(self.prev_gains[ch][0] + 64) as usize];
            let mut cur_gain = 0.0;
            let mut cur_gain2 = 0.0;
            let mut gain_idx = 0;
            let eighthmask = (self.samples >> 3) - 1;
            for (i, out) in dst.iter_mut().take(self.samples).enumerate() {
                *out = dsp.out[i + self.samples] * prev_gain * dsp.window[i]
                       - self.delay[ch][i] * dsp.window[self.samples - i - 1];
                if (i & eighthmask) == 0 {
                    if (self.gains[ch][gain_idx] == 0) && (self.gains[ch][gain_idx + 1] == 0) {
                        cur_gain  = 1.0;
                        cur_gain2 = 1.0;
                    } else {
                        cur_gain  = dsp.pow_tab[(self.gains[ch][gain_idx] + 64) as usize];
                        cur_gain2 = dsp.gain_tab[(self.gains[ch][gain_idx + 1] - self.gains[ch][gain_idx] + 11) as usize];
                    }
                    gain_idx += 1;
                }
                *out *= cur_gain;
                cur_gain *= cur_gain2;
            }
            self.delay[ch][..self.samples].copy_from_slice(&dsp.out[..self.samples]);
        }
        Ok(())
    }
}

fn unpack_band(br: &mut BitReader, codebooks: &Codebooks, coef_index: &mut [u8; BAND_SIZE], coef_sign: &mut [bool; BAND_SIZE], cat: usize) -> DecoderResult<()> {
    let cb = &codebooks.vq_cb[cat];
    let group_size = COOK_VQ_GROUP_SIZE[cat];
    let mult = COOK_VQ_MULT[cat] + 1;
    for i in 0..COOK_NUM_VQ_GROUPS[cat] {
        let ret                                         = br.read_cb(cb);
        let mut val;
        if let Ok(v) = ret {
            val = v as u32;
        } else {
            let left = br.left() as u32;
            br.skip(left)?;
            break;
        }
        let mut nnz = 0;
        for j in (0..group_size).rev() {
            let t = (val * COOK_VQ_INV_RADIX[cat]) >> 20;
            coef_index[i * group_size + j] = (val - t * mult) as u8;
            if coef_index[i * group_size + j] != 0 {
                nnz += 1;
            }
            val = t;
        }
        if (br.left() as usize) < nnz {
            let left = br.left() as u32;
            br.skip(left)?;
            break;
        }
        for j in 0..group_size {
            if coef_index[i * group_size + j] != 0 {
                coef_sign[i * group_size + j]           = br.read_bool()?;
            } else {
                coef_sign[i * group_size + j] = false;
            }
        }
    }
    Ok(())
}

struct RND {
    state:  u32,
}

impl RND {
    fn new() -> Self {
        Self { state: 0xC0DECC00 }
    }
    fn get_sign(&mut self) -> bool {
        self.state = (self.state & 0xFFFF).wrapping_mul(36969).wrapping_add(self.state >> 16);
        (self.state & 0x10000) != 0
    }
}

struct CookDecoder {
    info:       NACodecInfoRef,
    chmap:      NAChannelMap,
    src:        [u8; 65536],
    num_pairs:  usize,
    pairs:      [CookChannelPair; MAX_PAIRS],
    channels:   usize,
    samples:    usize,
    codebooks:  Codebooks,
    rnd:        RND,
    dsp:        CookDSP,
}

impl CookDecoder {
    fn new() -> Self {
        CookDecoder {
            info:       NACodecInfo::new_dummy(),
            chmap:      NAChannelMap::new(),
            src:        [0; 65536],
            num_pairs:  0,
            channels:   0,
            samples:    0,
            pairs:      [CookChannelPair::new(); MAX_PAIRS],
            codebooks:  Codebooks::new(),
            rnd:        RND::new(),
            dsp:        CookDSP::new(1024),
        }
    }
}

impl NADecoder for CookDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            let edata = info.get_extradata().unwrap();
            validate!(edata.len() >= 4);

            let mut mr = MemoryReader::new_read(&edata);
            let mut br = ByteReader::new(&mut mr);
            let ver                                     = br.peek_u32be()?;

            let maj_ver = ver >> 24;
            let mut chmap: u32 = 0;
            match maj_ver {
                1 => {
                        self.num_pairs  = 1;
                        self.pairs[0].read_hdr_v1(&mut br)?;
                        self.channels = self.pairs[0].mode.get_channels();
                        if ainfo.get_channels() == 1 { // forced mono
                            self.pairs[0].mode = Mode::Mono;
                            self.channels       = 1;
                            chmap = 0x4;
                        } else {
                            chmap = 0x3;
                        }
                    },
                2 => {
                        self.num_pairs  = (edata.len() - (br.tell() as usize)) / 20;
                        validate!(self.num_pairs <= MAX_PAIRS);
                        let mut start_ch = 0;
                        for i in 0..self.num_pairs {
                            let pair_chmap = self.pairs[i].read_hdr_v2(&mut br)?;
                            self.pairs[i].start_ch = start_ch;
                            validate!((chmap & pair_chmap) == 0);
                            chmap |= pair_chmap;
                            start_ch += self.pairs[i].mode.get_channels();
                        }
                        self.channels = start_ch;
                    },
                _ => { return Err(DecoderError::InvalidData); }
            };

            self.samples = self.pairs[0].samples / self.pairs[0].mode.get_channels();
            validate!((self.samples >= 16) && (self.samples <= 1024));
            if self.samples != self.dsp.size {
                self.dsp = CookDSP::new(self.samples);
            }
            self.chmap   = NAChannelMap::from_ms_mapping(chmap);

            for i in 1..self.num_pairs {
                validate!((self.pairs[i].samples / self.pairs[i].mode.get_channels()) == self.samples);
            }

            let vector_bits = match self.samples {
                    16 | 32 | 64 | 128 | 256 => 5,
                    512                      => 6,
                    1024                     => 7,
                    _                        => unreachable!(),
                };
            for pair in self.pairs.iter_mut() {
                match pair.mode {
                    Mode::Mono          => {
                            pair.vector_bits = 5;
                        },
                    Mode::Stereo        => {
                            pair.vector_bits = 5;
                            pair.samples >>= 1;
                        },
                    Mode::JointStereo   => {
                            pair.vector_bits = vector_bits;
                            pair.samples >>= 1;
                        },
                };
            }

            let ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), self.channels as u8,
                                         SND_F32P_FORMAT, self.samples);
            self.info = info.replace_info(NACodecTypeInfo::Audio(ainfo));

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        validate!(info.get_properties().is_audio());
        let pktbuf = pkt.get_buffer();
        validate!(pktbuf.len() > self.num_pairs * 2);

        let mut seg_size: [usize; MAX_PAIRS] = [0; MAX_PAIRS];
        let mut seg_start: [usize; MAX_PAIRS+1] = [0; MAX_PAIRS+1];

        let ainfo = self.info.get_properties().get_audio_info().unwrap();

        seg_size[0] = pktbuf.len() - (self.num_pairs - 1);
        for i in 1..self.num_pairs {
            seg_size[i] = (pktbuf[pktbuf.len() - self.num_pairs + i] as usize) * 2;
            validate!(seg_size[i] != 0);
            let ret = seg_size[0].checked_sub(seg_size[i]);
            if let Some(val) = ret {
                seg_size[0] = val;
            } else {
                return Err(DecoderError::InvalidData);
            }
        }
        validate!(seg_size[0] != 0);
        seg_start[0] = 0;
        for i in 0..self.num_pairs {
            seg_start[i + 1] = seg_start[i] + seg_size[i];
        }

        let mut abuf = alloc_audio_buffer(ainfo, self.samples, self.chmap.clone())?;

        for pair in 0..self.num_pairs {
            self.pairs[pair].decode(&mut self.dsp, &mut self.rnd, &self.codebooks, &pktbuf[seg_start[pair]..seg_start[pair + 1]], &mut self.src, &mut abuf)?;
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.replace_info(NACodecTypeInfo::Audio(ainfo)), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        for pair in self.pairs.iter_mut() {
            pair.delay = [[0.0; MAX_SAMPLES]; 2];
        }
    }
}

impl NAOptionHandler for CookDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(CookDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::realmedia_register_all_decoders;
    use crate::realmedia_register_all_demuxers;
    #[test]
    fn test_cook() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

//        let file = "assets/RV/rv30_weighted_mc.rm";
        // sample: https://samples.mplayerhq.hu/real/AC-cook/cook_5.1/multichannel.rma
        let file = "assets/RV/multichannel.rma";
        test_decode_audio("realmedia", file, Some(2000), None/*Some("cook")*/, &dmx_reg, &dec_reg);
    }
}
