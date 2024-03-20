use nihav_core::formats::*;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::byteio::*;
use nihav_core::io::codebook::*;
use nihav_codec_support::dsp::mdct::IMDCT;
use std::f32::consts;

#[derive(Clone,Copy,PartialEq)]
enum Mode {
    Mono,
    Stereo,
    JointStereo,
}

const ATRAC3_FRAME_SIZE: usize = 1024;

#[derive(Clone,Copy)]
struct Tone {
    pos:        usize,
    ncoeffs:    usize,
    coeffs:     [f32; 8],
}

struct GainData {
    ngains:     [usize; 4],
    gain_lev:   [[usize; 8]; 4],
    gain_loc:   [[usize; 8]; 4],
}

impl GainData {
    fn new() -> Self {
        Self {
            ngains:     [0; 4],
            gain_lev:   [[0; 8]; 4],
            gain_loc:   [[0; 8]; 4],
        }
    }
    fn read(&mut self, br: &mut BitReader, bands: usize) -> DecoderResult<()> {
        self.ngains = [0; 4];
        for band in 0..bands {
            self.ngains[band]                           = br.read(3)? as usize;
            for i in 0..self.ngains[band] {
                self.gain_lev[band][i]                  = br.read(4)? as usize;
                self.gain_loc[band][i]                  = br.read(5)? as usize;
            }
        }
        Ok(())
    }
}

struct Channel {
    data:       [f32; ATRAC3_FRAME_SIZE],
    delay:      [f32; ATRAC3_FRAME_SIZE],
    qmf_delay:  [f32; 64 * 3],

    bands:      usize,
    block_no:   usize,

    gain_data:  [GainData; 2],

    subbands:   usize,
    components: usize,
    ntones:     usize,
    tones:      [Tone; 64],
}

impl Channel {
    fn new() -> Self {
        Self {
            data:       [0.0; ATRAC3_FRAME_SIZE],
            delay:      [0.0; ATRAC3_FRAME_SIZE],
            qmf_delay:  [0.0; 64 * 3],

            bands:      0,
            block_no:   0,

            gain_data:  [GainData::new(), GainData::new()],

            subbands:   0,
            components: 0,
            ntones:     0,
            tones:      [Tone {pos: 0, ncoeffs: 0, coeffs: [0.0; 8]}; 64],
        }
    }
    fn decode_unit(&mut self, br: &mut BitReader, codebooks: &[Codebook<u8>; 7], scalefactors: &[f32; 64]) -> DecoderResult<()> {
        self.bands                                      = (br.read(2)? as usize) + 1;
        self.gain_data[self.block_no].read(br, self.bands)?;
        self.data = [0.0; ATRAC3_FRAME_SIZE];

        self.ntones = 0;
        self.components                                 = br.read(5)? as usize;
        if self.components > 0 {
            let selector                                = br.read(2)?;
            validate!(selector != 2);
            let mut mode0 = (selector & 1) != 0;

            for _ in 0..self.components {
                let mut flags: [bool; 4] = [false; 4];
                for band in 0..self.bands {
                    flags[band]                         = br.read_bool()?;
                }
                let nvals                               = br.read(3)? as usize;
                let quant                               = br.read(3)? as usize;
                validate!(quant > 1);
                if selector == 3 {
                    mode0                               = br.read_bool()?;
                }
                for j in 0..self.bands*4 {
                    if !flags[j >> 2] { continue; }
                    let count                           = br.read(3)? as usize;
                    validate!(self.ntones + count < 64);
                    for _ in 0..count {
                        let scale_idx                   = br.read(6)? as usize;
                        let tone = &mut self.tones[self.ntones];
                        tone.pos                        = (br.read(6)? as usize) + j * 64;
                        tone.ncoeffs                    = (ATRAC3_FRAME_SIZE - tone.pos).min(nvals + 1);
                        let scale = scalefactors[scale_idx] * ATRAC3_MAX_QUANT[quant];
                        read_coeffs(br, codebooks, &mut tone.coeffs[..tone.ncoeffs], quant, scale, mode0)?;
                        self.ntones += 1;
                    }
                }
            }
        }

        // spectrum
        let mut quants: [usize; 32] = [0; 32];
        let mut scf:    [usize; 32] = [0; 32];
        self.subbands                                   = (br.read(5)? as usize) + 1;
        let mode0                                       = br.read_bool()?;
        for i in 0..self.subbands {
            quants[i]                                   = br.read(3)? as usize;
        }
        for i in 0..self.subbands {
            if quants[i] == 0 { continue; }
            scf[i]                                      = br.read(6)? as usize;
        }

        self.data = [0.0; ATRAC3_FRAME_SIZE];
        for i in 0..self.subbands {
            if quants[i] == 0 { continue; }
            let start = ATRAC3_SUBBANDS[i];
            let end   = ATRAC3_SUBBANDS[i + 1];
            let scale = scalefactors[scf[i]] * ATRAC3_MAX_QUANT[quants[i]];
            read_coeffs(br, codebooks, &mut self.data[start..end], quants[i], scale, mode0)?;
        }

        for i in 0..self.ntones {
            let tone = &self.tones[i];
            for j in 0..tone.ncoeffs {
                self.data[tone.pos + j] += tone.coeffs[j];
            }
        }
        Ok(())
    }
    fn synth(&mut self, dsp: &mut DSP) {
        let mut flag = 0;
        for (band, (data, delay)) in self.data.chunks_mut(256).zip(self.delay.chunks_mut(256)).enumerate() {
            if (flag & 1) != 0 {
                for i in 0..128 {
                    let t0 = data[i];
                    let t1 = data[255 - i];
                    data[i]         = t1;
                    data[255 - i]   = t0;
                }
            }
            dsp.synth_band(data);
            dsp.apply_gains(data, delay, &mut self.gain_data, self.block_no, band);
            delay.copy_from_slice(&dsp.tmp[256..512]);
            flag ^= 1;
        }
        self.block_no ^= 1;
    }
    fn do_qmf(&mut self, dsp: &mut DSP, dst: &mut [f32]) {
        let mut qchunks = self.qmf_delay.chunks_mut(64);
        let qmf0 = qchunks.next().unwrap();
        let qmf1 = qchunks.next().unwrap();
        let qmf2 = qchunks.next().unwrap();
        {
            let mut tchunks = self.data.chunks_mut(512);
            let tmp0 = tchunks.next().unwrap();
            let tmp1 = tchunks.next().unwrap();
            dsp.do_qmf(tmp0, qmf0, false);
            dsp.do_qmf(tmp1, qmf1, true);
        }
        dsp.do_qmf_out(dst, &self.data, qmf2);
    }
}

const ATRAC3_DEFAULT_DELAY: usize = 2190;
const ATRAC3_WEIGHTING_DELAYS: [u8; 6] = [ 0, 7, 0, 7, 0, 7 ];

fn read_coeffs(br: &mut BitReader, cb: &[Codebook<u8>; 7], dst: &mut [f32], quant: usize, scale: f32, mode0: bool) -> DecoderResult<()> {
    if mode0 {
        read_coeffs_mode0(br, dst, quant, scale)
    } else if quant == 1 {
        read_coeffs_mode1(br, &cb[0], dst, scale)
    } else {
        read_coeffs_other(br, &cb[quant - 1], dst, scale)
    }
}

const ATRAC3_MODE0_CB: [f32; 4] = [ 0.0, 1.0, -2.0, -1.0 ];
const ATRAC3_MODE0_BITS: [u8; 8] = [ 0, 4, 3, 3, 4, 4, 5, 6 ];

fn read_coeffs_mode0(br: &mut BitReader, dst: &mut [f32], quant: usize, scale: f32) -> DecoderResult<()> {
    let bits = ATRAC3_MODE0_BITS[quant];
    if bits > 0 {
        for el in dst.iter_mut() {
            let val                                     = br.read_s(bits)? as f32;
            *el = val * scale;
        }
    } else {
        for out in dst.chunks_mut(2) {
            let val                                     = br.read(4)? as usize;
            out[0] = ATRAC3_MODE0_CB[val >> 2] * scale;
            out[1] = ATRAC3_MODE0_CB[val &  3] * scale;
        }
    }
    Ok(())
}

const ATRAC3_MODE1_CB: [f32; 9 * 2] = [
     0.0,  0.0,
     0.0,  1.0,
     0.0, -1.0,
     1.0,  0.0,
    -1.0,  0.0,
     1.0,  1.0,
     1.0, -1.0,
    -1.0,  1.0,
    -1.0, -1.0
];

fn read_coeffs_mode1(br: &mut BitReader, cb: &Codebook<u8>, dst: &mut [f32], scale: f32) -> DecoderResult<()> {
    for out in dst.chunks_mut(2) {
        let val                                         = br.read_cb(cb)? as usize;
        out[0] = ATRAC3_MODE1_CB[val * 2 + 0] * scale;
        out[1] = ATRAC3_MODE1_CB[val * 2 + 1] * scale;
    }
    Ok(())
}

fn read_coeffs_other(br: &mut BitReader, cb: &Codebook<u8>, dst: &mut [f32], scale: f32) -> DecoderResult<()> {
    for el in dst.iter_mut() {
        let val                                         = (br.read_cb(cb)? as i8) + 1;
        let sign = (val & 1) != 0;
        let coef = f32::from(if sign { -(val >> 1) } else { val >> 1 });
        *el = coef * scale;
    }
    Ok(())
}

struct DSP {
    imdct:          IMDCT,
    window:         [f32; 512],
    gain_tab:       [f32; 16],
    gain_tab2:      [f32; 32],
    tmp:            [f32; ATRAC3_FRAME_SIZE + 64],
}

#[allow(clippy::manual_memcpy)]
impl DSP {
    fn new() -> Self {
        let mut gain_tab: [f32; 16] = [0.0; 16];
        let mut gain_tab2: [f32; 32] = [0.0; 32];

        for i in 0..16 {
            gain_tab[i] = 2.0f32.powf(4.0 - (i as f32));
        }
        for i in 0..32 {
            gain_tab2[i] = 2.0f32.powf(((i as f32) - 15.0) * -0.125);
        }

        let mut tmpw: [f32; 256] = [0.0; 256];
        for i in 0..tmpw.len() {
            tmpw[i] = (((((i as f32) + 0.5) / 256.0 - 0.5) * consts::PI).sin() + 1.0) * 0.5;
        }

        let mut window: [f32; 512] = [0.0; 512];
        for i in 0..tmpw.len() {
            let w = tmpw[i] / (tmpw[i] * tmpw[i] + tmpw[255 - i] * tmpw[255 - i]);
            window[i] = w;
            window[512 - 1 - i] = w;
        }

        Self {
                imdct:  IMDCT::new(512, false),
                tmp:    [0.0; ATRAC3_FRAME_SIZE + 64],
                gain_tab, gain_tab2, window,
            }
    }
    fn synth_band(&mut self, src: &[f32]) {
        let dst = &mut self.tmp;
        self.imdct.imdct(src, dst);
        for i in 0..512 {
            dst[i] *= self.window[i];
        }
    }
    fn apply_gains(&mut self, dst: &mut [f32], delay: &[f32], gain_data: &mut [GainData; 2], block_no: usize, band: usize) {
        let prev_ngains = gain_data[block_no ^ 1].ngains[band];
        let gain1 = if gain_data[block_no].ngains[band] > 0 {
                            self.gain_tab[gain_data[block_no].gain_lev[band][0]]
                    } else { 1.0 };

        if prev_ngains == 0 {
            for i in 0..256 {
                dst[i] = self.tmp[i] * gain1 + delay[i];
            }
            return;
        }

        gain_data[block_no ^ 1].gain_lev[band][prev_ngains] = 4;
        gain_data[block_no ^ 1].gain_loc[band][prev_ngains] = 32;

        let mut off = 0;
        for i in 0..prev_ngains {
            let start = gain_data[block_no ^ 1].gain_loc[band][i] * 8;
            let end = start + 8;

            let mut gain2   = self.gain_tab [gain_data[block_no ^ 1].gain_lev[band][i]];
            let gain_inc    = self.gain_tab2[gain_data[block_no ^ 1].gain_lev[band][i + 1] + 15 -
                                             gain_data[block_no ^ 1].gain_lev[band][i]];

            while off < start {
                dst[off] = (self.tmp[off] * gain1 + delay[off]) * gain2;
                off += 1;
            }
            while off < end {
                dst[off] = (self.tmp[off] * gain1 + delay[off]) * gain2;
                gain2 *= gain_inc;
                off += 1;
            }
        }
        for i in off..256 {
            dst[i] = self.tmp[i] * gain1 + delay[i];
        }
    }
    fn qmf_prepare(&mut self, src: &[f32], delay: &[f32], size: usize, swap: bool) {
        for i in 0..46 {
            self.tmp[i] = delay[i];
        }
        let (s0, s1) = if !swap {
                (&src[0..], &src[size/2..])
            } else {
                (&src[size/2..], &src[0..])
            };
        for i in (0..size).step_by(4) {
            self.tmp[46 + i + 0] = s0[i / 2 + 0] + s1[i / 2 + 0];
            self.tmp[46 + i + 1] = s0[i / 2 + 0] - s1[i / 2 + 0];
            self.tmp[46 + i + 2] = s0[i / 2 + 1] + s1[i / 2 + 1];
            self.tmp[46 + i + 3] = s0[i / 2 + 1] - s1[i / 2 + 1];
        }
    }
    fn qmf_synth(&mut self, dst: &mut [f32], size: usize) {
        for i in (0..size).step_by(2) {
            let mut acc0 = 0.0;
            let mut acc1 = 0.0;

            for j in (0..ATRAC3_QMF_FILTER.len()).step_by(2) {
                acc0 += self.tmp[i + j + 0] * ATRAC3_QMF_FILTER[j + 0];
                acc1 += self.tmp[i + j + 1] * ATRAC3_QMF_FILTER[j + 1];
            }
            dst[i + 0] = acc1 * consts::SQRT_2 / 256.0;
            dst[i + 1] = acc0 * consts::SQRT_2 / 256.0;
        }
    }
    fn do_qmf(&mut self, dst: &mut [f32], delay: &mut [f32], swap: bool) {
        self.qmf_prepare(dst, delay, 512, swap);
        self.qmf_synth(dst, 512);
        for i in 0..46 {
            delay[i] = self.tmp[512 + i];
        }
    }
    fn do_qmf_out(&mut self, dst: &mut [f32], src: &[f32], delay: &mut [f32]) {
        self.qmf_prepare(src, delay, 1024, false);
        self.qmf_synth(dst, 1024);
        for i in 0..46 {
            delay[i] = self.tmp[1024 + i];
        }
    }
}

struct Atrac3Decoder {
    info:       NACodecInfoRef,
    channels:   usize,
    chmap:      NAChannelMap,
    samples:    usize,
    mode:       Mode,
    scrambled:  bool,

    codebooks:  [Codebook<u8>; 7],
    dsp:        DSP,
    ch_data:    [Channel; 2],
    scalefactors:   [f32; 64],

    mci_prev:   [usize; 4],
    mci_cur:    [usize; 4],
    mci_next:   [usize; 4],

    weighting_delay:    [u8; 6],

    pkt_buf:    Vec<u8>,
}

struct Atrac3CodebookReader {
    bits:  &'static [u8],
    codes: &'static [u8],
}
impl CodebookDescReader<u8> for Atrac3CodebookReader {
    fn bits(&mut self, idx: usize) -> u8  { self.bits[idx] }
    fn code(&mut self, idx: usize) -> u32 { u32::from(self.codes[idx]) }
    fn sym (&mut self, idx: usize) -> u8 { idx as u8 }
    fn len(&mut self) -> usize { self.bits.len() }
}

impl Atrac3Decoder {
    fn new() -> Self {
        let mut scalefactors: [f32; 64] = [0.0; 64];
        for i in 0..scalefactors.len() {
            scalefactors[i] = 2.0f32.powf(((i as f32) - 15.0) / 3.0);
        }

        let mut cb0 = Atrac3CodebookReader { codes: ATRAC3_HUFF_CODES[0], bits: ATRAC3_HUFF_BITS[0] };
        let mut cb1 = Atrac3CodebookReader { codes: ATRAC3_HUFF_CODES[1], bits: ATRAC3_HUFF_BITS[1] };
        let mut cb2 = Atrac3CodebookReader { codes: ATRAC3_HUFF_CODES[2], bits: ATRAC3_HUFF_BITS[2] };
        let mut cb3 = Atrac3CodebookReader { codes: ATRAC3_HUFF_CODES[3], bits: ATRAC3_HUFF_BITS[3] };
        let mut cb4 = Atrac3CodebookReader { codes: ATRAC3_HUFF_CODES[4], bits: ATRAC3_HUFF_BITS[4] };
        let mut cb5 = Atrac3CodebookReader { codes: ATRAC3_HUFF_CODES[5], bits: ATRAC3_HUFF_BITS[5] };
        let mut cb6 = Atrac3CodebookReader { codes: ATRAC3_HUFF_CODES[6], bits: ATRAC3_HUFF_BITS[6] };
        Self {
            info:       NACodecInfo::new_dummy(),
            chmap:      NAChannelMap::new(),
            channels:   0,
            samples:    0,
            mode:       Mode::Mono,
            scrambled:  false,

            dsp:        DSP::new(),
            ch_data:    [Channel::new(), Channel::new()],
            codebooks:  [Codebook::new(&mut cb0, CodebookMode::MSB).unwrap(),
                         Codebook::new(&mut cb1, CodebookMode::MSB).unwrap(),
                         Codebook::new(&mut cb2, CodebookMode::MSB).unwrap(),
                         Codebook::new(&mut cb3, CodebookMode::MSB).unwrap(),
                         Codebook::new(&mut cb4, CodebookMode::MSB).unwrap(),
                         Codebook::new(&mut cb5, CodebookMode::MSB).unwrap(),
                         Codebook::new(&mut cb6, CodebookMode::MSB).unwrap() ],

            mci_prev:   [3; 4],
            mci_cur:    [3; 4],
            mci_next:   [3; 4],

            weighting_delay: ATRAC3_WEIGHTING_DELAYS,
            pkt_buf:    Vec::with_capacity(65536),
            scalefactors,
        }
    }
    #[allow(clippy::identity_op)]
    fn rev_matrix(&mut self) {
        for i in 0..4 {
            let c0 = self.mci_prev[i];
            let c1 = self.mci_cur[i];
            let off;
            if c0 != c1 {
                let l0 = ATRAC3_MATRIX_COEFFS[c0 * 2 + 0];
                let r0 = ATRAC3_MATRIX_COEFFS[c1 * 2 + 0];
                let l1 = ATRAC3_MATRIX_COEFFS[c0 * 2 + 1];
                let r1 = ATRAC3_MATRIX_COEFFS[c1 * 2 + 1];
                for idx in 0..8 {
                    let t0 = self.ch_data[0].data[idx + i * 256];
                    let t1 = self.ch_data[1].data[idx + i * 256];
                    let n1 = t0 * interp(l0, r0, idx) + t1 * interp(l1, r1, idx);
                    self.ch_data[0].data[idx + i * 256] = n1;
                    self.ch_data[1].data[idx + i * 256] = t0 * 2.0 - n1;
                }
                off = i * 256 + 8;
            } else {
                off = i * 256;
            }
            match c1 {
                0 => {
                        for i in off..256 {
                            let t0 = self.ch_data[0].data[i];
                            let t1 = self.ch_data[1].data[i];
                            self.ch_data[0].data[i] =       t1  * 2.0;
                            self.ch_data[1].data[i] = (t0 - t1) * 2.0;
                        }
                    },
                1 => {
                        for i in off..256 {
                            let t0 = self.ch_data[0].data[i];
                            let t1 = self.ch_data[1].data[i];
                            self.ch_data[0].data[i] = (t0 + t1) *  2.0;
                            self.ch_data[1].data[i] =       t1  * -2.0;
                        }
                    },
                _ => {
                        for i in off..256 {
                            let t0 = self.ch_data[0].data[i];
                            let t1 = self.ch_data[1].data[i];
                            self.ch_data[0].data[i] = t0 + t1;
                            self.ch_data[1].data[i] = t0 - t1;
                        }
                    },
            };
        }
    }
    fn weigh_channels(&mut self) {
        if (self.weighting_delay[1] == 7) && (self.weighting_delay[3] == 7) { return; }
        let pw: [f32; 2];
        if self.weighting_delay[1] == 7 {
            pw = [1.0; 2];
        } else {
            let w = f32::from(self.weighting_delay[1]) / 7.0;
            let iw = (2.0 - w * w).sqrt();
            if self.weighting_delay[0] == 0 {
                pw = [w, iw];
            } else {
                pw = [iw, w];
            }
        }
        let cw: [f32; 2];
        if self.weighting_delay[3] == 7 {
            cw = [1.0; 2];
        } else {
            let w = f32::from(self.weighting_delay[3]) / 7.0;
            let iw = (2.0 - w * w).sqrt();
            if self.weighting_delay[2] == 0 {
                cw = [w, iw];
            } else {
                cw = [iw, w];
            }
        }

        for i in 0..4 {
            let off = i * 256;
            for j in 0..8 {
                self.ch_data[0].data[off + j] *= interp(pw[0], pw[1], j);
                self.ch_data[1].data[off + j] *= interp(pw[0], pw[1], j);
            }
            for j in 8..256 {
                self.ch_data[0].data[off + j] *= cw[0];
                self.ch_data[1].data[off + j] *= cw[1];
            }
        }
    }
}

fn interp(a: f32, b: f32, pos: usize) -> f32 {
    a + ((pos as f32) / 8.0) * (b - a)
}

impl NADecoder for Atrac3Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.info = info.clone();
            let edata = info.get_extradata().unwrap();
            validate!(edata.len() >= 4);

            let mut mr = MemoryReader::new_read(&edata);
            let mut br = ByteReader::new(&mut mr);

            match edata.len() {
                10 => {
                        let version                     = br.read_u32be()?;
                        validate!(version == 4);
                        self.samples                    = br.read_u16be()? as usize;
                        let delay                       = br.read_u16be()? as usize;
                        validate!(delay == ATRAC3_DEFAULT_DELAY);
                        let mode                        = br.read_u16be()?;
                        self.mode = match mode {
                                0 => Mode::Mono,
                                2 => Mode::Stereo,
                                0x12 => Mode::JointStereo,
                                _ => return Err(DecoderError::InvalidData),
                            };
                        self.channels = if self.mode == Mode::Mono { 1 } else { 2 };
                        self.scrambled = true;
                    },
                14 | 16 => {
                        if edata.len() == 16 { br.read_skip(2)?; }
                        self.samples                    = br.read_u32be()? as usize;
                        let mode                        = br.read_u16be()?;
                        self.mode = if mode != 0 { Mode::JointStereo } else { Mode::Stereo };
                        self.channels = 2;
                                                          br.read_skip(2)?;
                        let ffactor                     = br.read_u16be()? as usize;
                        validate!((ffactor > 0) && (ffactor <= 16));
                        // calculate block_align / channels / ffactor, validate it's 96/152/192
                    },
                _ => { return Err(DecoderError::InvalidData) }
            };
            validate!(self.samples == ATRAC3_FRAME_SIZE * self.channels);
            if self.mode == Mode::Mono {
                self.chmap.add_channel(NAChannelType::C);
            } else {
                self.chmap.add_channel(NAChannelType::L);
                self.chmap.add_channel(NAChannelType::R);
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
        validate!(pktbuf.len() > 5);

        let frame_size = pktbuf.len();
        self.pkt_buf.resize(frame_size, 0);
        if self.scrambled {
            for (i, s) in pktbuf.iter().enumerate() {
                self.pkt_buf[i] = s ^ ATRAC3_XOR_KEY[i & 3];
            }
        } else {
            let dst = &mut self.pkt_buf[0..frame_size];
            dst.copy_from_slice(pktbuf.as_slice());
        }

        {
            let mut br = BitReader::new(&self.pkt_buf[0..frame_size], BitReaderMode::BE);
            let id                                  = br.read(6)?;
            validate!(id == 0x28);
            self.ch_data[0].decode_unit(&mut br, &self.codebooks, &self.scalefactors)?;
            self.ch_data[0].synth(&mut self.dsp);
        }
        if self.channels == 2 {
            let off;
            if self.mode == Mode::JointStereo {
                for i in 0..frame_size / 2 {
                    let b0 = self.pkt_buf[i];
                    let b1 = self.pkt_buf[frame_size - i - 1];
                    self.pkt_buf[i]                     = b1;
                    self.pkt_buf[frame_size - i - 1]    = b0;
                }
                let mut i = 0;
                while (i < frame_size) && (self.pkt_buf[i] == 0xF8) { i += 1; }
                validate!(frame_size - i > 4);
                off = i;
            } else {
                off = frame_size / 2;
            }
            let mut br = BitReader::new(&self.pkt_buf[off..frame_size], BitReaderMode::BE);
            if self.mode == Mode::JointStereo {
                let id                                  = br.read(2)?;
                validate!(id == 0x3);
            } else {
                let id                                  = br.read(6)?;
                validate!(id == 0x28);
            }
            if self.mode == Mode::JointStereo {
                for i in 0..self.weighting_delay.len() - 2 {
                    self.weighting_delay[i] = self.weighting_delay[i + 2];
                }
                self.weighting_delay[4]                 = br.read(1)? as u8;
                self.weighting_delay[5]                 = br.read(3)? as u8;
                self.mci_prev = self.mci_cur;
                self.mci_cur  = self.mci_next;
                for i in 0..4 {
                    self.mci_next[i]                    = br.read(2)? as usize;
                }
            }
            self.ch_data[1].decode_unit(&mut br, &self.codebooks, &self.scalefactors)?;
            self.ch_data[1].synth(&mut self.dsp);
        }
        if self.mode == Mode::JointStereo {
            self.rev_matrix();
            self.weigh_channels();
        }

        let ainfo = self.info.get_properties().get_audio_info().unwrap();

        let abuf = alloc_audio_buffer(ainfo, ATRAC3_FRAME_SIZE, self.chmap.clone())?;
        let mut adata = abuf.get_abuf_f32().unwrap();
        let output = adata.get_data_mut().unwrap();

        for ch in 0..self.channels {
            let dpos = abuf.get_offset(ch);
            self.ch_data[ch].do_qmf(&mut self.dsp, &mut output[dpos..][..ATRAC3_FRAME_SIZE]);
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.replace_info(NACodecTypeInfo::Audio(ainfo)), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        for ch_data in self.ch_data.iter_mut() {
            ch_data.delay = [0.0; 1024];
            ch_data.qmf_delay = [0.0; 64 * 3];
        }
    }
}

impl NAOptionHandler for Atrac3Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(Atrac3Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::test_decode_audio;
    use crate::generic_register_all_decoders;
    use nihav_realmedia::realmedia_register_all_demuxers;
    #[test]
    fn test_atrac3() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);

        // samples from private collection
        let file = "assets/RV/rv30_atrc_384x208_realproducer_plus_8.51.rm";
//        let file = "assets/RV/rv20_svt_atrc_640x352_realproducer_plus_8.51.rm";
        test_decode_audio("realmedia", file, Some(12000), None/*Some("atrac3")*/, &dmx_reg, &dec_reg);
    }
}

const ATRAC3_XOR_KEY: [u8; 4] = [ 0x53, 0x7F, 0x61, 0x03 ];

const ATRAC3_HUFF_CODES: [&[u8]; 7] = [
    &[ 0x00, 0x04, 0x05, 0x0C, 0x0D, 0x1C, 0x1D, 0x1E, 0x1F ],
    &[ 0x00, 0x04, 0x05, 0x06, 0x07 ],
    &[ 0x00, 0x04, 0x05, 0x0C, 0x0D, 0x0E, 0x0F ],
    &[ 0x00, 0x04, 0x05, 0x0C, 0x0D, 0x1C, 0x1D, 0x1E, 0x1F ], // same as 0
    &[ 0x00, 0x02, 0x03, 0x08, 0x09, 0x0A, 0x0B, 0x1C, 0x1D, 0x3C, 0x3D, 0x3E, 0x3F, 0x0C, 0x0D ],
    &[ 0x00, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x14,
       0x15, 0x16, 0x17, 0x18, 0x19, 0x34, 0x35, 0x36,
       0x37, 0x38, 0x39, 0x3A, 0x3B, 0x78, 0x79, 0x7A,
       0x7B, 0x7C, 0x7D, 0x7E, 0x7F, 0x08, 0x09 ],
    &[ 0x00, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E,
       0x0F, 0x10, 0x11, 0x24, 0x25, 0x26, 0x27, 0x28,
       0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30,
       0x31, 0x32, 0x33, 0x68, 0x69, 0x6A, 0x6B, 0x6C,
       0x6D, 0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74,
       0x75, 0xEC, 0xED, 0xEE, 0xEF, 0xF0, 0xF1, 0xF2,
       0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA,
       0xFB, 0xFC, 0xFD, 0xFE, 0xFF, 0x02, 0x03 ]
];
const ATRAC3_HUFF_BITS: [&[u8]; 7] = [
    &[ 1, 3, 3, 4, 4, 5, 5, 5, 5 ],
    &[ 1, 3, 3, 3, 3 ],
    &[ 1, 3, 3, 4, 4, 4, 4 ],
    &[ 1, 3, 3, 4, 4, 5, 5, 5, 5 ],
    &[ 2, 3, 3, 4, 4, 4, 4, 5, 5, 6, 6, 6, 6, 4, 4 ],
    &[ 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 4, 4 ],
    &[ 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7,
       7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4 ],
];

const ATRAC3_MAX_QUANT: [f32; 8] = [ 0.0, 1.0/1.5, 1.0/2.5, 1.0/3.5, 1.0/4.5, 1.0/7.5, 1.0/15.5, 1.0/31.5 ];

const ATRAC3_SUBBANDS: [usize; 32 + 1] = [
      0,   8,  16,  24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160, 176,
    192, 224, 256, 288, 320, 352, 384, 416, 448, 480, 512, 576, 640, 704, 768, 896,
   1024
];

const ATRAC3_MATRIX_COEFFS: [f32; 8] = [ 0.0, 2.0, 2.0, 2.0, 0.0, 0.0, 1.0, 1.0 ];

const ATRAC3_QMF_FILTER: [f32; 48] = [
    -0.000029238139, -0.000184109580, -0.000112315138,  0.000602345390,
     0.000484503806, -0.001705877949, -0.001041114796,  0.004068033770,
     0.001566677820, -0.008430772461, -0.001512299757,  0.015680588782,
    -0.000122339843, -0.026883240789,  0.004925364163,  0.043472178280,
    -0.015603342094, -0.068180441856,  0.037618979812,  0.108652018011,
    -0.087192758918, -0.198768734932,  0.264158189297,  0.928483188152,
     0.928483188152,  0.264158189297, -0.198768734932, -0.087192758918,
     0.108652018011,  0.037618979812, -0.068180441856, -0.015603342094,
     0.043472178280,  0.004925364163, -0.026883240789, -0.000122339843,
     0.015680588782, -0.001512299757, -0.008430772461,  0.001566677820,
     0.004068033770, -0.001041114796, -0.001705877949,  0.000484503806,
     0.000602345390, -0.000112315138, -0.000184109580, -0.000029238139
];
