use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::byteio::read_u16le;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::io::intcode::*;
use nihav_codec_support::dsp::fft::*;
use nihav_codec_support::dsp::mdct::IMDCT;

use std::str::FromStr;

use super::on2avcdata::*;

const COEFFS: usize = 1024;
const MAX_BANDS: usize = 14 * 8;
const QTAB_SIZE: usize = 10000;
const SHORT_WIN_POINT0: usize = 512 - 64;
const SHORT_WIN_POINT1: usize = 512 + 64;

struct WinInfo {
    is_long:        bool,
    bands:          &'static [usize],
}

impl WinInfo {
    fn get_tot_bands(&self) -> usize {
        if self.is_long {
            self.bands.len()
        } else {
            self.bands.len() * 8
        }
    }
}

struct Codebooks {
    scale_cb:       Codebook<i8>,
    spec_cb:        [Codebook<u16>; 15],
}

fn scale_map(idx: usize) -> i8 { (idx as i8) - 60 }
fn cb_map1(idx: usize)  -> u16 { AVC_SPEC_CB1_SYMS[idx] }
fn cb_map2(idx: usize)  -> u16 { AVC_SPEC_CB2_SYMS[idx] }
fn cb_map3(idx: usize)  -> u16 { AVC_SPEC_CB3_SYMS[idx] }
fn cb_map4(idx: usize)  -> u16 { AVC_SPEC_CB4_SYMS[idx] }
fn cb_map5(idx: usize)  -> u16 { AVC_SPEC_CB5_SYMS[idx] }
fn cb_map6(idx: usize)  -> u16 { AVC_SPEC_CB6_SYMS[idx] }
fn cb_map7(idx: usize)  -> u16 { AVC_SPEC_CB7_SYMS[idx] }
fn cb_map8(idx: usize)  -> u16 { AVC_SPEC_CB8_SYMS[idx] }
fn cb_map9(idx: usize)  -> u16 { AVC_SPEC_CB9_SYMS[idx] }
fn cb_map10(idx: usize) -> u16 { AVC_SPEC_CB10_SYMS[idx] }
fn cb_map11(idx: usize) -> u16 { AVC_SPEC_CB11_SYMS[idx] }
fn cb_map12(idx: usize) -> u16 { AVC_SPEC_CB12_SYMS[idx] }
fn cb_map13(idx: usize) -> u16 { AVC_SPEC_CB13_SYMS[idx] }
fn cb_map14(idx: usize) -> u16 { AVC_SPEC_CB14_SYMS[idx] }
fn cb_map15(idx: usize) -> u16 { AVC_SPEC_CB15_SYMS[idx] }

impl Codebooks {
    fn new() -> Self {
        let mut coderead = TableCodebookDescReader::new(AVC_SCF_CODEBOOK_CODES, AVC_SCF_CODEBOOK_BITS, scale_map);
        let scale_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB1_CODES, AVC_SPEC_CB1_BITS, cb_map1);
        let cb1 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB2_CODES, AVC_SPEC_CB2_BITS, cb_map2);
        let cb2 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB3_CODES, AVC_SPEC_CB3_BITS, cb_map3);
        let cb3 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB4_CODES, AVC_SPEC_CB4_BITS, cb_map4);
        let cb4 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB5_CODES, AVC_SPEC_CB5_BITS, cb_map5);
        let cb5 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB6_CODES, AVC_SPEC_CB6_BITS, cb_map6);
        let cb6 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB7_CODES, AVC_SPEC_CB7_BITS, cb_map7);
        let cb7 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB8_CODES, AVC_SPEC_CB8_BITS, cb_map8);
        let cb8 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB9_CODES, AVC_SPEC_CB9_BITS, cb_map9);
        let cb9 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB10_CODES, AVC_SPEC_CB10_BITS, cb_map10);
        let cb10 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB11_CODES, AVC_SPEC_CB11_BITS, cb_map11);
        let cb11 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB12_CODES, AVC_SPEC_CB12_BITS, cb_map12);
        let cb12 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB13_CODES, AVC_SPEC_CB13_BITS, cb_map13);
        let cb13 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB14_CODES, AVC_SPEC_CB14_BITS, cb_map14);
        let cb14 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = TableCodebookDescReader::new(AVC_SPEC_CB15_CODES, AVC_SPEC_CB15_BITS, cb_map15);
        let cb15 = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let spec_cb = [cb1, cb2, cb3, cb4, cb5, cb6, cb7, cb8, cb9, cb10, cb11, cb12, cb13, cb14, cb15];
        Self { scale_cb, spec_cb }
    }
}

struct SynthDSP {
    rdft512:        RDFT,
    rdft256:        RDFT,
    irdft128:       RDFT,
    irdft64:        RDFT,
    synth_tmp:      [FFTComplex; 512],
    synth_out:      [FFTComplex; 512],
}

impl SynthDSP {
    fn new() -> Self {
        let rdft512  = RDFTBuilder::new_rdft(512, true, true);
        let rdft256  = RDFTBuilder::new_rdft(256, true, true);
        let irdft128 = RDFTBuilder::new_rdft(128, false, false);
        let irdft64  = RDFTBuilder::new_rdft(64,  false, false);
        Self {
            rdft512, rdft256, irdft128, irdft64,
            synth_tmp:  [FFTC_ZERO; 512],
            synth_out:  [FFTC_ZERO; 512],
        }
    }
}

struct AVCDecoder {
    chmap:          NAChannelMap,
    ainfo:          NAAudioInfo,
    info:           NACodecInfoRef,
    dsp:            SynthDSP,
    version:        i32,
    codebooks:      Codebooks,
    channels:       usize,
    is_40khz:       bool,
    win_long:       &'static [f32; 1024],

    prev_win:       u8,
    cur_win:        u8,
    winfo:          &'static WinInfo,
    windows:        usize,
    win_grp:        [bool; 8],
    ms_present:     bool,
    ms_info:        [bool; MAX_BANDS],
    cbs:            [u8; MAX_BANDS],
    scales:         [u8; MAX_BANDS],

    imdct_long:     IMDCT,
    imdct_mid:      IMDCT,
    imdct_short:    IMDCT,
    coeffs:         [[f32; COEFFS]; 2],
    delay:          [[f32; COEFFS]; 2],
    tmp:            [f32; COEFFS * 2],
    ew_buf:         [f32; 1152],
    use_generic:    bool,

    qtab:           [f32; QTAB_SIZE],
    scale_tab:      [f32; 128],
}

impl AVCDecoder {
    fn new(version: i32) -> Self {
        let mut qtab = [0.0f32; QTAB_SIZE];
        for (i, el) in qtab.iter_mut().enumerate() {
            *el = (i as f32) * (i as f32).sqrt();
        }
        let mut scale_tab = [0.0f32; 128];
        for (i, el) in scale_tab.iter_mut().enumerate() {
            let pow = 10.0f32.powf((i as f32) * 0.1);
            if i < 20 {
                *el = (pow * 16.0).ceil() / 32.0;
            } else {
                *el = (pow * 0.5).ceil();
            }
            *el /= 32768.0;
        }

        AVCDecoder {
            chmap:          NAChannelMap::new(),
            ainfo:          NAAudioInfo::new(0, 0, SND_F32P_FORMAT, 0),
            info:           NACodecInfo::new_dummy(),
            dsp:            SynthDSP::new(),
            version,
            codebooks:      Codebooks::new(),
            channels:       0,
            is_40khz:       false,
            win_long:       AVC_WIN_LONG_32K,

            prev_win:       0,
            cur_win:        0,
            winfo:          &AVC_WINFO_LONG,
            windows:        0,
            win_grp:        [false; 8],
            ms_present:     false,
            ms_info:        [false; MAX_BANDS],
            cbs:            [0; MAX_BANDS],
            scales:         [0; MAX_BANDS],

            imdct_long:     IMDCT::new(1024 * 2, true),
            imdct_mid:      IMDCT::new(512  * 2, true),
            imdct_short:    IMDCT::new(128  * 2, true),
            coeffs:         [[0.0; COEFFS]; 2],
            delay:          [[0.0; COEFFS]; 2],
            tmp:            [0.0; COEFFS * 2],
            ew_buf:         [0.0; 1152],
            use_generic:    true,

            qtab,
            scale_tab,
        }
    }
    fn decode_channel(&mut self, br: &mut BitReader, chno: usize) -> DecoderResult<()> {
        let coeffs = &mut self.coeffs[chno];

        decode_band_types(br, &mut self.cbs, self.winfo)?;
        // band scales
        let bands = self.winfo.bands.len();
        let mut cur_band = 0;
        let mut scale = 0;
        let mut first = true;
        for wg in 0..self.windows {
            if self.win_grp[wg] {
                for _ in 0..bands {
                    if self.cbs[cur_band] == 0 {
                        let mut all_zero = true;
                        let mut band2 = cur_band;
                        for wg2 in wg + 1..self.windows {
                            if self.win_grp[wg2] {
                                break;
                            }
                            band2 += bands;
                            if self.cbs[band2] != 0 {
                                all_zero = false;
                                break;
                            }
                        }
                        if all_zero {
                            self.scales[cur_band] = 0;
                            cur_band += 1;
                            continue;
                        }
                    }
                    if first {
                        scale                   = br.read(7)? as i16;
                        first = false;
                    } else {
                        scale                   += i16::from(br.read_cb(&self.codebooks.scale_cb)?);
                        validate!((0..128).contains(&scale));
                    }
                    self.scales[cur_band] = scale as u8;
                    cur_band += 1;
                }
            } else {
                for _ in 0..bands {
                    self.scales[cur_band] = self.scales[cur_band - bands];
                    cur_band += 1;
                }
            }
        }
        // coefficients
        let mut cur_band = 0;
        let mut idx = 0;
        *coeffs = [0.0; COEFFS];
        for _ in 0..self.windows {
            let mut band_start = 0;
            for band_end in self.winfo.bands.iter() {
                let band_size = *band_end - band_start;
                let cb_idx = self.cbs[cur_band];
                if cb_idx > 0 {
                    let cb = &self.codebooks.spec_cb[(cb_idx - 1) as usize];
                    let scale = self.scale_tab[self.scales[cur_band] as usize];
                    let dst = &mut coeffs[idx..][..band_size];
                    if cb_idx < 9 {
                        decode_quads(br, cb, &self.qtab, scale, dst)?;
                    } else {
                        decode_pairs(br, cb, &self.qtab, scale, dst, cb_idx == 15)?;
                    }
                }

                band_start = *band_end;
                idx += band_size;
                cur_band += 1;
            }
        }
        Ok(())
    }
    fn decode_frame(&mut self, br: &mut BitReader, adata: &mut NAAudioBuffer<f32>, offset: usize) -> DecoderResult<()> {
        let enh_flag                            = br.read_bool()?;
        validate!(!enh_flag);
        self.prev_win = self.cur_win;
        self.cur_win                            = br.read(3)? as u8;
        self.winfo = match self.cur_win {
                3 => &AVC_WINFO_SHORT,
                4 => if self.is_40khz { &AVC_WINFO_40K_MODE4 } else { &AVC_WINFO_44K_MODE4 },
                5 => if self.is_40khz { &AVC_WINFO_40K_MODE5 } else { &AVC_WINFO_44K_MODE5 },
                6 => if self.is_40khz { &AVC_WINFO_40K_MODE6 } else { &AVC_WINFO_44K_MODE6 },
                _ => &AVC_WINFO_LONG,
            };
        self.windows = if self.winfo.is_long { 1 } else { 8 };
        let bands = self.winfo.bands.len();
        self.win_grp[0] = true;
        for el in self.win_grp.iter_mut().skip(1).take(self.windows - 1) {
            *el                                 = !br.read_bool()?;
        }

        self.ms_present                         = br.read_bool()?;
        if self.ms_present {
            validate!(self.channels == 2);
            let mut cur_band = 0;
            for wg in self.win_grp.iter().take(self.windows) {
                if *wg {
                    for _ in 0..bands {
                        self.ms_info[cur_band]  = br.read_bool()?;
                        cur_band += 1;
                    }
                } else {
                    for _ in 0..bands {
                        self.ms_info[cur_band] = self.ms_info[cur_band - bands];
                        cur_band += 1;
                    }
                }
            }
        }
        for ch in 0..self.channels {
            self.decode_channel(br, ch)?;
        }

        if (self.channels == 2) && self.ms_present {
            let mut idx = 0;
            let mut cur_band = 0;
            for _ in 0..self.windows {
                let mut band_start = 0;
                for band_end in self.winfo.bands.iter() {
                    let band_size = *band_end - band_start;
                    if self.ms_info[cur_band] {
                        for i in 0..band_size {
                            let l = self.coeffs[0][idx + i];
                            let r = self.coeffs[1][idx + i];
                            self.coeffs[0][idx + i] = l + r;
                            self.coeffs[1][idx + i] = l - r;
                        }
                    }
                    cur_band += 1;
                    idx += band_size;
                    band_start = *band_end;
                }
            }
        }

        for ch in 0..self.channels {
            let off = adata.get_offset(ch) + offset;
            let output = adata.get_data_mut().unwrap();
            self.synth_channel(ch, &mut output[off..][..COEFFS]);
        }
        Ok(())
    }
    #[allow(clippy::cognitive_complexity)]
    fn synth_channel(&mut self, chno: usize, dst: &mut [f32]) {
        let coeffs = &mut self.coeffs[chno];
        let delay  = &mut self.delay[chno];

        match self.cur_win {
            0 | 1 => {
                self.imdct_long.imdct_half(coeffs, &mut self.tmp);
                overlap_half(dst, &self.tmp, delay, self.win_long);
                delay[0..COEFFS/2].copy_from_slice(&self.tmp[COEFFS/2..COEFFS]);
                for i in COEFFS/2..COEFFS {
                    delay[i] = delay[COEFFS - 1 - i];
                }
            },
            2 | 7 => {
                self.imdct_long.imdct_half(coeffs, &mut self.ew_buf);
            },
            3 => {
                for (ain, aout) in coeffs.chunks(128).zip(self.tmp.chunks_mut(256)) {
                    self.imdct_short.imdct(ain, aout);
                }
                self.ew_buf = [0.0; 1152];
                for i in 0..128 {
                    self.ew_buf[i] = self.tmp[i];
                }
                for w in 0..7 {
                    overlap(&mut self.ew_buf[(w + 1) * 128..][..128],
                            &self.tmp[(w + 1) * 256..][..128],
                            &self.tmp[w * 256 + 128..][..128],
                            AVC_WIN_SHORT);
                }
                for i in 0..128 {
                    self.ew_buf[1024 + i] = self.tmp[7 * 256 + 128 + i];
                }
                for i in 0..SHORT_WIN_POINT0 {
                    dst[i] = delay[i];
                }
                overlap(&mut dst[SHORT_WIN_POINT0..SHORT_WIN_POINT1],
                        &self.ew_buf[0..128],
                        &delay[SHORT_WIN_POINT0..SHORT_WIN_POINT1], AVC_WIN_SHORT);
                for i in SHORT_WIN_POINT1..COEFFS {
                    dst[i] = self.ew_buf[i - SHORT_WIN_POINT1 + 128];
                }
                for i in 0..SHORT_WIN_POINT1 {
                    delay[i] = self.ew_buf[SHORT_WIN_POINT1 + i];
                }
                for i in COEFFS/2..COEFFS {
                    delay[i] = delay[COEFFS - 1 - i];
                }
            },
            4 => {
                if !self.use_generic {
                    synth1024(&mut self.dsp, coeffs, &mut self.ew_buf, &mut self.tmp, self.is_40khz);
                } else {
                    synth_generic(coeffs, &mut self.ew_buf, &mut self.tmp, self.is_40khz, 1024);
                }
             },
            5 => {
                if !self.use_generic {
                    synth512(&mut self.dsp, coeffs, &mut self.ew_buf, &mut self.tmp, self.is_40khz);
                } else {
                    synth_generic(coeffs, &mut self.ew_buf, &mut self.tmp, self.is_40khz, 512);
                }
                self.imdct_mid.imdct_half(&coeffs[512..], &mut self.ew_buf[512..]);
            },
            6 => {
                self.imdct_mid.imdct_half(coeffs, &mut self.ew_buf);
                if !self.use_generic {
                    synth512(&mut self.dsp, &coeffs[512..], &mut self.ew_buf[512..], &mut self.tmp, self.is_40khz);
                } else {
                    synth_generic(&coeffs[512..], &mut self.ew_buf[512..], &mut self.tmp, self.is_40khz, 512);
                }
            },
            _ => unreachable!(),
        };
        if (self.cur_win == 2) || (self.cur_win >= 4) {
            for i in 0..SHORT_WIN_POINT0 {
                dst[i] = delay[i];
            }
            overlap_half(&mut dst[SHORT_WIN_POINT0..SHORT_WIN_POINT1],
                         &self.ew_buf[0..64],
                         &delay[SHORT_WIN_POINT0..SHORT_WIN_POINT1], AVC_WIN_SHORT);
            for i in SHORT_WIN_POINT1..COEFFS {
                dst[i] = self.ew_buf[i - SHORT_WIN_POINT1 + 64];
            }
            for i in 0..COEFFS/2 {
                delay[i] = self.ew_buf[COEFFS/2 + i];
            }
            for i in COEFFS/2..COEFFS {
                delay[i] = delay[COEFFS - 1 - i];
            }
        }
    }
}

fn overlap(dst: &mut [f32], src: &[f32], delay: &[f32], win: &[f32]) {
    for (i, ((out, s), d)) in dst.iter_mut().zip(src.iter()).zip(delay.iter()).enumerate() {
        *out = *s * win[i] + *d * win[win.len() - 1 - i];
    }
}

fn overlap_half(dst: &mut [f32], src: &[f32], delay: &[f32], win: &[f32]) {
    let n = win.len();
    for i in 0..n/2 {
        let ii = n - 1 - i;
        let c = src  [n/2 - 1 - i];
        let d = delay[i];
        let w0 = win[i];
        let w1 = win[ii];
        dst[i]      = d * w1 - c * w0;
        dst[ii]     = d * w0 + c * w1;
    }
}

#[derive(Clone,Copy)]
struct SynthParams {
    p0:     usize,
    p1:     usize,
    idx:    usize
}

macro_rules! synth_step0_template {
    ($name:ident, $tab: ident, $size: expr) => {
        fn $name(src: &[f32], dst: &mut [f32], step: usize, off: usize, sp: &SynthParams) {
            for i in 0..step {
                for j in 0..sp.p0 {
                    dst[i] += (f64::from(src[j]) * $tab[sp.idx][j][i]) as f32;
                }
            }
            for i in 0..step {
                for j in 0..sp.p1 {
                    dst[$size - step + i] += (f64::from(src[sp.p0 + off + j]) * $tab[sp.idx][sp.p0 + j][i]) as f32;
                }
            }
        }
    }
}

synth_step0_template!(synth_step0_10, AVC_SYNTH_TABS10, 10);
synth_step0_template!(synth_step0_20, AVC_SYNTH_TABS20, 20);
synth_step0_template!(synth_step0_40, AVC_SYNTH_TABS40, 40);
synth_step0_template!(synth_step0_84, AVC_SYNTH_TABS84, 84);

fn synth_step1(src: &[f32], dst: &mut [f32], size: usize, stride: usize, step: usize, off: usize, mut p0: usize, win: &[f64])
{
    let mut pos = step - 1;
    for _ in 0..off {
        let scale = f64::from(src[p0]);
        p0 += 1;
        pos &= size - 1;
        for i in 0..pos.min(step) {
            dst[pos - i] += (scale * win[i]) as f32;
        }
        pos += stride;
    }
}

fn zero_some(buf: &mut [FFTComplex], p0: usize, p1: usize)
{
    for el in buf.iter_mut().take(p0/2) {
        *el = FFTC_ZERO;
    }
    if (p0 & 1) != 0 {
        buf[p0 / 2].re = 0.0;
    }
    let len = buf.len();
    let dst = &mut buf[len - p1/2..];
    for el in dst.iter_mut() {
        *el = FFTC_ZERO;
    }
    if (p1 & 1) != 0 {
        buf[len - p1/2 - 1].im = 0.0;
    }
}

fn merge_bands(src: &[FFTComplex], dst: &mut [FFTComplex], size: usize) {
    let step = if size == 512 { 2 } else { 4 };
    let (s01, s23) = src.split_at(size / 2);
    let (src0, src1) = s01.split_at(size / 4);
    let (src2, src3) = s23.split_at(size / 4);
    let (t00, t01) = AVC_SYNTH_TAB0.split_at(512);
    let (t10, t11) = AVC_SYNTH_TAB1.split_at(512);
    let (t20, t21) = AVC_SYNTH_TAB2.split_at(512);
    let (t30, t31) = AVC_SYNTH_TAB3.split_at(512);
    let hsize = size / 2;
    let qsize = size / 4;
    let osize = size / 8;
    dst[0].re = src0[0].re * t00[0] + src1[0].re * t10[0] + src2[0].re * t20[0] + src3[0].re * t30[0];
    dst[0].im = src0[0].re * t00[1] + src1[0].re * t10[1] + src2[0].re * t20[1] + src3[0].re * t30[1];
    dst[qsize].re = src0[0].im * t00[256] + src1[0].im * t10[256] +
                    src2[0].im * t20[256] + src3[0].im * t30[256];
    dst[qsize].im = src0[0].im * t00[257] + src1[0].im * t10[257] +
                    src2[0].im * t20[257] + src3[0].im * t30[257];
    for i in 1..qsize {
        let s0 = src0[i];
        let s1 = src1[i];
        let s2 = src2[i];
        let s3 = src3[i];
        let t0 = s0 * FFTComplex { re: t00[i * step + 0], im: t00[i * step + 1] } +
                 s1 * FFTComplex { re: t10[i * step + 0], im: t10[i * step + 1] } +
                 s2 * FFTComplex { re: t20[i * step + 0], im: t20[i * step + 1] } +
                 s3 * FFTComplex { re: t30[i * step + 0], im: t30[i * step + 1] };
        let t1 = s0 * FFTComplex { re: t01[i * step + 0], im: t01[i * step + 1] } +
                 s1 * FFTComplex { re: t11[i * step + 0], im: t11[i * step + 1] } +
                 s2 * FFTComplex { re: t21[i * step + 0], im: t21[i * step + 1] } +
                 s3 * FFTComplex { re: t31[i * step + 0], im: t31[i * step + 1] };
        dst[i]          = t0;
        dst[qsize + i]  = t1;
    }

    dst[hsize].re = src0[0].im * t01[256] + src1[0].im * t11[256] +
                    src2[0].im * t21[256] + src3[0].im * t31[256];
    dst[hsize].im = src0[0].im * t01[257] + src1[0].im * t11[257] +
                    src2[0].im * t21[257] + src3[0].im * t31[257];
    dst[hsize + qsize].re = src0[0].re * t01[256] + src1[0].re * t11[256] +
                            src2[0].re * t21[256] + src3[0].re * t31[256];
    dst[hsize + qsize].im = src0[0].re * t01[257] + src1[0].re * t11[257] +
                            src2[0].re * t21[257] + src3[0].re * t31[257];
    for i in 1..osize {
        let s0 = !src0[qsize - i];
        let s1 = !src1[qsize - i];
        let s2 = !src2[qsize - i];
        let s3 = !src3[qsize - i];
        let t0 = s0 * FFTComplex { re: t00[256 + i * step + 0], im: t00[256 + i * step + 1] } +
                 s1 * FFTComplex { re: t10[256 + i * step + 0], im: t10[256 + i * step + 1] } +
                 s2 * FFTComplex { re: t20[256 + i * step + 0], im: t20[256 + i * step + 1] } +
                 s3 * FFTComplex { re: t30[256 + i * step + 0], im: t30[256 + i * step + 1] };
        let t1 = s0 * FFTComplex { re: t01[256 + i * step + 0], im: t01[256 + i * step + 1] } +
                 s1 * FFTComplex { re: t11[256 + i * step + 0], im: t11[256 + i * step + 1] } +
                 s2 * FFTComplex { re: t21[256 + i * step + 0], im: t21[256 + i * step + 1] } +
                 s3 * FFTComplex { re: t31[256 + i * step + 0], im: t31[256 + i * step + 1] };
        dst[hsize + i] = !t0;
        dst[hsize + qsize + i] = !t1;
    }
}

const SPARAMS10: [SynthParams; 2] = [
    SynthParams { p0: 1, p1: 3, idx: 0 },
    SynthParams { p0: 3, p1: 1, idx: 1 } ];
const SPARAMS20: [SynthParams; 2] = [
    SynthParams { p0: 5, p1: 4, idx: 0 },
    SynthParams { p0: 4, p1: 5, idx: 1 } ];
const SPARAMS40: [SynthParams; 2] = [
    SynthParams { p0: 11, p1:  8, idx: 0 },
    SynthParams { p0:  8, p1: 11, idx: 1 } ];
const SPARAMS84: [SynthParams; 4] = [
    SynthParams { p0: 16, p1: 4, idx: 0 },
    SynthParams { p0: 16, p1: 4, idx: 1 },
    SynthParams { p0: 13, p1: 7, idx: 2 },
    SynthParams { p0: 15, p1: 5, idx: 3 } ];

const MERGE_ORDER_44K: &[u8] = &[
    4, 4, 2, 2, 0, 0, 2, 0, 0, 2, 2, 0, 0, 2, 0, 0, 2, 0, 0,
    2, 0, 0, 4, 0, 0, 0, 0, 2, 0, 0, 0
];
const MERGE_ORDER_40K: &[u8] = &[
    4, 4, 2, 2, 0, 0, 2, 0, 0, 2, 2, 0, 0, 2, 0, 0, 2, 0, 0,
    2, 0, 0, 4, 2, 0, 0, 2, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0
];

fn synth_filter(src: &[f32], dst: &mut [f32], len: usize, step: usize, bands: usize, idx: usize)
{
    let off = (len - step) / bands + 1;
    let params = match step {
            10 => &SPARAMS10[idx],
            20 => &SPARAMS20[idx],
            40 => &SPARAMS40[idx],
            _  => &SPARAMS84[idx],
        };

    if step == 10 {
        synth_step0_10(src, dst, step, off, params);
        synth_step1(src, dst, len, bands, step, off, params.p0, &AVC_TAB10[idx]);
    } else if step == 20 {
        synth_step0_20(src, dst, step, off, params);
        synth_step1(src, dst, len, bands, step, off, params.p0, &AVC_TAB20[idx]);
    } else if step == 40 {
        synth_step0_40(src, dst, step, off, params);
        synth_step1(src, dst, len, bands, step, off, params.p0, &AVC_TAB40[idx]);
    } else {
        synth_step0_84(src, dst, step, off, params);
        synth_step1(src, dst, len, bands, step, off, params.p0, &AVC_TAB84[idx]);
    }
}

fn synth_recursive(dst: &mut [f32], tmp: &mut [f32], size: usize, order: &[u8], order_idx: &mut usize, dir: bool) {
    let bands = order[*order_idx] as usize;
    *order_idx += 1;
    if bands == 0 { return; }
    let sub_size = size / bands;
    let mut sub_dir = false;
    for (dst, tmp) in dst.chunks_mut(sub_size).take(bands).zip(tmp.chunks_mut(sub_size)) {
        synth_recursive(dst, tmp, sub_size, order, order_idx, sub_dir);
        sub_dir = !sub_dir;
    }
    for el in tmp.iter_mut().take(size) { *el = 0.0; }
    let step = if bands == 2 {
            if sub_size <= 20 {
                10
            } else if sub_size <= 40 {
                20
            } else {
                40
            }
        } else {
            84
        };
    for (i, src) in dst.chunks_mut(sub_size).take(bands).enumerate() {
        let idx = if !dir { i } else { bands - 1 - i };
        synth_filter(src, tmp, size, step, bands, idx);
    }
    dst[..size].copy_from_slice(&tmp[..size]);
}

fn synth_generic(src: &[f32], dst: &mut [f32], tmpbuf: &mut [f32; COEFFS * 2], is_40khz: bool, size: usize) {
    let order = if is_40khz { MERGE_ORDER_40K } else { MERGE_ORDER_44K };
    dst[..size].copy_from_slice(&src[..size]);
    let mut order_idx = 0;
    synth_recursive(dst, tmpbuf, size, order, &mut order_idx, false);
    for el in dst.iter_mut().take(COEFFS) { *el *= 0.125; }
}

fn synth1024(dsp: &mut SynthDSP, src: &[f32], dst: &mut [f32], tmpbuf: &mut [f32; COEFFS * 2], is_40khz: bool) {
    for el in tmpbuf.iter_mut() {
        *el = 0.0;
    }
    let (tmp, tmp2) = tmpbuf.split_at_mut(COEFFS);
    for el in dsp.synth_tmp.iter_mut() { *el = FFTC_ZERO; }
    for el in dsp.synth_out.iter_mut() { *el = FFTC_ZERO; }

    let (size, step, stride) = (32, 20, 2);
    let off = (size - step) / stride + 1;
    let mut sparam0 = SynthParams { p0: 5, p1: 4, idx: 0 };
    let mut sparam1 = SynthParams { p0: 4, p1: 5, idx: 1 };
    for (i, dst) in tmp.chunks_mut(size).take(8).enumerate() {
        let coeffs = &src[i * 32..];
        synth_step0_20(coeffs, dst, step, off, &sparam0);
        synth_step1(coeffs, dst, size, stride, step, off, sparam0.p0, &AVC_TAB20[sparam0.idx]);
        let coeffs = &src[i * 32 + 16..];
        synth_step0_20(coeffs, dst, step, off, &sparam1);
        synth_step1(coeffs, dst, size, stride, step, off, sparam0.p0, &AVC_TAB20[sparam1.idx]);
        std::mem::swap(&mut sparam0, &mut sparam1);
    }

    let (size, step, stride) = (64, 40, 2);
    let off = (size - step) / stride + 1;
    let mut sparam0 = SynthParams { p0: 11, p1:  8, idx: 0 };
    let mut sparam1 = SynthParams { p0:  8, p1: 11, idx: 1 };
    let nchunks = if !is_40khz { 8 } else { 12 };
    for (i, dst) in tmp2.chunks_mut(size).take(nchunks).enumerate() {
        let coeffs = if i < 4 { &tmp[i * 64..] } else { &src[i * 64 + 32..] };
        synth_step0_40(coeffs, dst, step, off, &sparam0);
        synth_step1(coeffs, dst, size, stride, step, off, sparam0.p0, &AVC_TAB40[sparam0.idx]);
        let coeffs = if i < 4 { &tmp[i * 64 + 32..] } else { &src[i * 64 + 32..] };
        synth_step0_40(coeffs, dst, step, off, &sparam1);
        synth_step1(coeffs, dst, size, stride, step, off, sparam0.p0, &AVC_TAB40[sparam1.idx]);
        std::mem::swap(&mut sparam0, &mut sparam1);
    }

    for el in tmp.iter_mut().take(128) {
        *el = 0.0;
    }
    let (size, step, stride) = (256, 84, 4);
    let off = (size - step) / stride + 1;
    for i in 0..4 {
        let coeffs = &tmp2[i * 64..];
        synth_step0_84(coeffs, tmp, step, off, &SPARAMS84[i]);
        synth_step1(coeffs, tmp, size, stride, step, off, SPARAMS84[i].p0, &AVC_TAB84[i]);
    }
    for i in 0..4 {
        let coeffs = if i < 2 && is_40khz { &tmp2[256 + i * 64..] } else { &src[256 + i * 64..] };
        let dst = &mut tmp[256..];
        synth_step0_84(coeffs, dst, step, off, &SPARAMS84[3 - i]);
        synth_step1(coeffs, dst, size, stride, step, off, SPARAMS84[3 - i].p0, &AVC_TAB84[3 - i]);
    }

    if !is_40khz {
        let (size, step, stride) = (256, 40, 2);
        let off = (size - step) / stride + 1;
        let sparams = [ SynthParams { p0: 11, p1:  8, idx: 0 },
                        SynthParams { p0:  8, p1: 11, idx: 1 } ];
        for i in 0..2 {
            let coeffs = &src[512 + i * 128..];
            let dst = &mut tmp[512..];
            synth_step0_40(coeffs, dst, step, off, &sparams[i]);
            synth_step1(coeffs, dst, size, stride, step, off, sparams[i].p0, &AVC_TAB40[i]);
        }
    } else {
        let (size, step, stride) = (256, 84, 4);
        let off = (size - step) / stride + 1;
        for i in 0..4 {
            let coeffs = &src[512 + i * 64..];
            let dst = &mut tmp[512..];
            synth_step0_84(coeffs, dst, step, off, &SPARAMS84[i]);
            synth_step1(coeffs, dst, size, stride, step, off, SPARAMS84[i].p0, &AVC_TAB84[i]);
        }
    }

    for (i, s) in tmp.chunks(2).take(768/2).enumerate() {
        dsp.synth_tmp[i] = FFTComplex { re: s[0], im: s[1] };
    }
    for i in (768..1024).step_by(2) {
        dsp.synth_tmp[i >> 1] = FFTComplex { re: src[i], im: src[i + 1] };
    }
    {
        let dst = &mut tmp[768..1024];
        dst.copy_from_slice(&src[768..1024]);
    }
    for (i, chunk) in dsp.synth_tmp.chunks_exact_mut(128).enumerate() {
        zero_some(chunk, SPARAMS84[i].p0, SPARAMS84[i].p1);
    }
    for chunk in dsp.synth_tmp.chunks_exact_mut(128) {
        dsp.irdft128.do_rdft_inplace(chunk);
        for el in chunk.iter_mut() { *el = el.scale(1.0/128.0f32); }
    }
    merge_bands(&dsp.synth_tmp, &mut dsp.synth_out, 512);
    dsp.rdft512.do_rdft_inplace(&mut dsp.synth_out);
    for (out, src) in dst.chunks_exact_mut(2).zip(dsp.synth_out.iter()) {
        out[0] = src.re * (1.0 / 1024.0f32.sqrt());
        out[1] = src.im * (1.0 / 1024.0f32.sqrt());
    }
    let (size, step, stride) = (1024, 84, 4);
    let off = (size - step) / stride + 1;
    for i in 0..4 {
        let src = &tmp[256 * i..];
        synth_step0_84(src, dst, step, off, &SPARAMS84[i]);
    }
}
fn synth512(dsp: &mut SynthDSP, src: &[f32], dst: &mut [f32], tmpbuf: &mut [f32; COEFFS * 2], is_40khz: bool) {
    for el in tmpbuf.iter_mut() {
        *el = 0.0;
    }
    let (tmp, tmp2) = tmpbuf.split_at_mut(COEFFS);

    let (size, step, stride) = (16, 10, 2);
    let off = (size - step) / stride + 1;
    let mut sparam0 = SynthParams { p0: 1, p1: 3, idx: 0 };
    let mut sparam1 = SynthParams { p0: 3, p1: 1, idx: 1 };
    let mut tab_ptr0: &[f64] = AVC_TAB10_0;
    let mut tab_ptr1: &[f64] = AVC_TAB10_1;
    for (i, dst) in tmp.chunks_mut(size).take(8).enumerate() {
        let coeffs = &src[i * 16..];
        synth_step0_10(coeffs, dst, step, off, &sparam0);
        synth_step1(coeffs, dst, size, stride, step, off, sparam0.p0, tab_ptr0);
        let coeffs = &src[i * 16 + 8..];
        synth_step0_10(coeffs, dst, step, off, &sparam1);
        synth_step1(coeffs, dst, size, stride, step, off, sparam0.p0, tab_ptr1);
        std::mem::swap(&mut sparam0, &mut sparam1);
        std::mem::swap(&mut tab_ptr0, &mut tab_ptr1);
    }

    let (size, step, stride) = (32, 20, 2);
    let off = (size - step) / stride + 1;
    let mut sparam0 = SynthParams { p0: 5, p1: 4, idx: 0 };
    let mut sparam1 = SynthParams { p0: 4, p1: 5, idx: 1 };
    let nchunks = if !is_40khz { 8 } else { 12 };
    for (i, dst) in tmp2.chunks_mut(size).take(nchunks).enumerate() {
        let coeffs = if i < 4 { &tmp[i * 32..] } else { &src[i * 32..] };
        synth_step0_20(coeffs, dst, step, off, &sparam0);
        synth_step1(coeffs, dst, size, stride, step, off, sparam0.p0, &AVC_TAB20[sparam0.idx]);
        let coeffs = if i < 4 { &tmp[i * 32 + 16..] } else { &src[i * 32 + 16..] };
        synth_step0_20(coeffs, dst, step, off, &sparam1);
        synth_step1(coeffs, dst, size, stride, step, off, sparam0.p0, &AVC_TAB20[sparam1.idx]);
        std::mem::swap(&mut sparam0, &mut sparam1);
    }

    for el in tmp.iter_mut().take(64) {
        *el = 0.0;
    }
    let (size, step, stride) = (128, 84, 4);
    let off = (size - step) / stride + 1;
    for i in 0..4 {
        let coeffs = &tmp2[i * 32..];
        synth_step0_84(coeffs, tmp, step, off, &SPARAMS84[i]);
        synth_step1(coeffs, tmp, size, stride, step, off, SPARAMS84[i].p0, &AVC_TAB84[i]);
    }
    for i in 0..4 {
        let coeffs = if i < 2 && is_40khz { &tmp2[128 + i * 32..] } else { &src[128 + i * 32..] };
        let dst = &mut tmp[128..];
        synth_step0_84(coeffs, dst, step, off, &SPARAMS84[3 - i]);
        synth_step1(coeffs, dst, size, stride, step, off, SPARAMS84[3 - i].p0, &AVC_TAB84[3 - i]);
    }

    if !is_40khz {
        let (size, step, stride) = (128, 40, 2);
        let off = (size - step) / stride + 1;
        let sparams = [ SynthParams { p0: 11, p1:  8, idx: 0 },
                        SynthParams { p0:  8, p1: 11, idx: 1 } ];
        for i in 0..2 {
            let coeffs = &src[256 + i * 64..];
            let dst = &mut tmp[256..];
            synth_step0_40(coeffs, dst, step, off, &sparams[i]);
            synth_step1(coeffs, dst, size, stride, step, off, sparams[i].p0, &AVC_TAB40[i]);
        }
    } else {
        let (size, step, stride) = (128, 84, 4);
        let off = (size - step) / stride + 1;
        for i in 0..4 {
            let coeffs = &src[256 + i * 32..];
            let dst = &mut tmp[256..];
            synth_step0_84(coeffs, dst, step, off, &SPARAMS84[i]);
            synth_step1(coeffs, dst, size, stride, step, off, SPARAMS84[i].p0, &AVC_TAB84[i]);
        }
    }

    for (i, s) in tmp.chunks(2).take(384/2).enumerate() {
        dsp.synth_tmp[i] = FFTComplex { re: s[0], im: s[1] };
    }
    for i in (384..512).step_by(2) {
        dsp.synth_tmp[i >> 1] = FFTComplex { re: src[i], im: src[i + 1] };
    }
    {
        let dst = &mut tmp[384..512];
        dst.copy_from_slice(&src[384..512]);
    }
    for (i, chunk) in dsp.synth_tmp.chunks_exact_mut(128).enumerate() {
        zero_some(chunk, SPARAMS84[i].p0, SPARAMS84[i].p1);
    }
    for chunk in dsp.synth_tmp.chunks_exact_mut(64) {
        dsp.irdft64.do_rdft_inplace(chunk);
    }
    merge_bands(&dsp.synth_tmp, &mut dsp.synth_out, 256);
    dsp.rdft256.do_rdft_inplace(&mut dsp.synth_out);
    for (out, src) in dst.chunks_exact_mut(2).zip(dsp.synth_out.iter()).take(256) {
        out[0] = src.re * (1.0 / 512.0f32 / 1024.0f32);
        out[1] = src.im * (1.0 / 512.0f32 / 1024.0f32);
    }
    let (size, step, stride) = (512, 84, 4);
    let off = (size - step) / stride + 1;
    for i in 0..4 {
        let src = &tmp[128 * i..];
        synth_step0_84(src, dst, step, off, &SPARAMS84[i]);
    }
}

fn decode_band_types(br: &mut BitReader, cbs: &mut [u8; MAX_BANDS], winfo: &WinInfo) -> DecoderResult<()> {
    let bits_per_sect = if winfo.is_long { 5 } else { 3 };
    let esc_val = (1 << bits_per_sect) - 1;
    let tot_bands = winfo.get_tot_bands();

    let mut cur_band = 0;
    while cur_band < tot_bands {
        let codebook                            = br.read(4)? as u8;
        let mut run = 1;
        loop {
            let run_add                         = br.read(bits_per_sect)? as usize;
            run += run_add;
            if run_add != esc_val { break; }
        }
        validate!(cur_band + run <= tot_bands);
        for _ in 0..run {
            cbs[cur_band] = codebook;
            cur_band += 1;
        }
    }
    Ok(())
}

fn dequant(val: i16, qtab: &[f32; QTAB_SIZE], scale: f32) -> f32 {
    if val >= 0 {
        qtab[val as usize] * scale
    } else {
        -qtab[val.unsigned_abs() as usize] * scale
    }
}

fn decode_quads(br: &mut BitReader, cb: &Codebook<u16>, qtab: &[f32; QTAB_SIZE], scale: f32, dst: &mut [f32]) -> DecoderResult<()> {
    for quad in dst.chunks_exact_mut(4) {
        let mut idx                             = br.read_cb(cb)? as i16;
        for el in quad.iter_mut() {
            *el = dequant(idx >> 12, qtab, scale);
            idx <<= 4;
        }
    }
    Ok(())
}

fn decode_esc_val(br: &mut BitReader, qtab: &[f32; QTAB_SIZE], scale: f32, sign: bool) -> DecoderResult<f32> {
    let pfx                                     = br.read_code(UintCodeType::LimitedOnes(24))? + 4;
    let add                                     = br.read(pfx as u8)? as usize;
    let val = (1 << pfx) + add;
    let fval = scale * if val < qtab.len() {
            qtab[val]
        } else {
            (val as f32) * (val as f32).sqrt()
        };
    if !sign {
        Ok(fval)
    } else {
        Ok(-fval)
    }
}

fn decode_pairs(br: &mut BitReader, cb: &Codebook<u16>, qtab: &[f32; QTAB_SIZE], scale: f32, dst: &mut [f32], is_esc: bool) -> DecoderResult<()> {
    for pair in dst.chunks_exact_mut(2) {
        let idx                                 = br.read_cb(cb)? as i16;
        let idx0 = idx >> 8;
        let idx1 = (idx << 8) >> 8;
        if !is_esc || (idx0.abs() < 16) {
            pair[0] = dequant(idx0, qtab, scale);
        } else {
            pair[0] = decode_esc_val(br, qtab, scale, idx0 < 0)?;
        }
        if !is_esc || (idx1.abs() < 16) {
            pair[1] = dequant(idx1, qtab, scale);
        } else {
            pair[1] = decode_esc_val(br, qtab, scale, idx1 < 0)?;
        }
    }
    Ok(())
}

impl NADecoder for AVCDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.chmap = match ainfo.get_channels() {
                    1 => { NAChannelMap::from_str("C").unwrap() },
                    2 => { NAChannelMap::from_str("L,R").unwrap() },
                    _ => { return Err(DecoderError::InvalidData); },
                };
            let srate = ainfo.get_sample_rate();
            self.ainfo = NAAudioInfo::new(srate, ainfo.get_channels(),
                                          SND_F32P_FORMAT, COEFFS);
            self.info = info.replace_info(NACodecTypeInfo::Audio(self.ainfo));
            self.channels = ainfo.get_channels() as usize;
            self.is_40khz = srate <= 40000;

            self.win_long = if (srate > 24000) && ((srate > 32000) || (self.channels == 2)) {
                    AVC_WIN_LONG_32K
                } else {
                    AVC_WIN_LONG_24K
                };

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        validate!(info.get_properties().is_audio());
        let src = pkt.get_buffer();

        let abuf;

        if self.version == 500 {
            abuf = alloc_audio_buffer(self.ainfo, COEFFS, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_f32().unwrap();
            let mut br = BitReader::new(src.as_slice(), BitReaderMode::BE);
            self.decode_frame(&mut br, &mut adata, 0)?;
        } else {
            let mut offsets: Vec<usize> = Vec::new();
            let mut sizes:  Vec<usize> = Vec::new();

            let mut cur_off = 0;
            while cur_off + 2 < src.len() {
                let sz = read_u16le(&src[cur_off..])? as usize;
                validate!(sz > 0);
                validate!(cur_off + sz + 2 <= src.len());
                offsets.push(cur_off + 2);
                sizes.push(sz);
                cur_off += sz + 2;
            }

            validate!(!sizes.is_empty());

            abuf = alloc_audio_buffer(self.ainfo, COEFFS * sizes.len(), self.chmap.clone())?;
            let mut adata = abuf.get_abuf_f32().unwrap();
            let mut aoffset = 0;
            for (o, s) in offsets.iter().zip(sizes.iter()) {
                let mut br = BitReader::new(&src[*o..][..*s], BitReaderMode::BE);
                self.decode_frame(&mut br, &mut adata, aoffset)?;
                aoffset += COEFFS;
            }
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for AVCDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_500() -> Box<dyn NADecoder + Send> {
    Box::new(AVCDecoder::new(500))
}

pub fn get_decoder_501() -> Box<dyn NADecoder + Send> {
    Box::new(AVCDecoder::new(501))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_avc() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        //let file = "assets/Duck/Cell-140.vp5";
        //let file = "assets/Duck/Chocolat-500.vp5";
        // sample: https://samples.mplayerhq.hu/V-codecs/VP7/potter-500.vp7
        let file = "assets/Duck/potter-500.vp7";
        test_decode_audio("avi", file, Some(1500), None/*Some("avc")*/, &dmx_reg, &dec_reg);
    }
}

const AVC_WINFO_LONG: WinInfo = WinInfo {
    is_long:    true,
    bands:      &[
              4,   8,  12,  16,  20,  24,  28,  32,
             36,  40,  48,  56,  64,  72,  80,  88,
             96, 108, 120, 132, 144, 156, 172, 188,
            204, 224, 244, 264, 288, 312, 340, 368,
            400, 432, 464, 496, 528, 560, 592, 624,
            656, 688, 720, 752, 784, 816, 848, 880, 1024
        ],
};

const AVC_WINFO_SHORT: WinInfo = WinInfo {
    is_long:    false,
    bands:      &[ 4, 8, 12, 16, 20, 24, 32, 40, 48, 56, 68, 80, 108, 128 ],
};

const AVC_WINFO_40K_MODE4: WinInfo = WinInfo {
    is_long:    true,
    bands:      &[
              8,  16,  24,  32,  40,  48,  56,  64,
             72,  80,  88,  96, 104, 112, 120, 128,
            144, 160, 176, 192, 208, 224, 240, 256,
            264, 272, 280, 288, 296, 304, 312, 320,
            328, 336, 344, 352, 360, 368, 376, 384,
            400, 416, 432, 448, 464, 480, 496, 512,
            520, 528, 536, 544, 552, 560, 568, 576,
            584, 592, 600, 608, 616, 624, 632, 640,
            648, 656, 664, 672, 680, 688, 696, 704,
            712, 720, 728, 736, 744, 752, 760, 768,
            800, 832, 864, 896, 928, 960, 992, 1024
        ],
};
const AVC_WINFO_40K_MODE5: WinInfo = WinInfo {
    is_long:    true,
    bands:      &[
              8,  16,  24,  32,  40,  48,  56,  64,
             80,  96, 112, 128, 136, 144, 152, 160,
            168, 176, 184, 192, 208, 224, 240, 256,
            264, 272, 280, 288, 296, 304, 312, 320,
            328, 336, 344, 352, 360, 368, 376, 384,
            416, 448, 480, 512, 516, 520, 524, 528,
            532, 536, 540, 548, 556, 568, 580, 592,
            608, 624, 640, 656, 672, 688, 704, 720,
            736, 752, 768, 784, 800, 816, 832, 848,
            864, 880, 896, 912, 928, 944, 1024
        ],
};
const AVC_WINFO_40K_MODE6: WinInfo = WinInfo {
    is_long:    true,
    bands:      &[
              4,   8,  12,  16,  20,  24,  28,  36,
             44,  56,  68,  80,  96, 112, 128, 144,
            160, 176, 192, 208, 224, 240, 256, 272,
            288, 304, 320, 336, 352, 368, 384, 400,
            416, 432, 512, 520, 528, 536, 544, 552,
            560, 568, 576, 592, 608, 624, 640, 648,
            656, 664, 672, 680, 688, 696, 704, 720,
            736, 752, 768, 776, 784, 792, 800, 808,
            816, 824, 832, 840, 848, 856, 864, 872,
            880, 888, 896, 928, 960, 992, 1024
        ],
};

const AVC_WINFO_44K_MODE4: WinInfo = WinInfo {
    is_long:    true,
    bands:      &[
              8,  16,  24,  32,  40,  48,  56,  64,
             72,  80,  88,  96, 104, 112, 120, 128,
            136, 144, 152, 160, 168, 176, 184, 192,
            200, 208, 216, 224, 232, 240, 248, 256,
            264, 272, 280, 288, 296, 304, 312, 320,
            328, 336, 344, 352, 360, 368, 376, 384,
            392, 400, 408, 416, 424, 432, 440, 448,
            456, 464, 472, 480, 488, 496, 504, 512,
            528, 544, 560, 576, 592, 608, 624, 640,
            672, 704, 736, 768, 832, 896, 960, 1024
        ],
};
const AVC_WINFO_44K_MODE5: WinInfo = WinInfo {
    is_long:    true,
    bands:      &[
              8,  16,  24,  32,  40,  48,  56,  64,
             72,  80,  88,  96, 104, 112, 120, 128,
            136, 144, 152, 160, 168, 176, 184, 192,
            200, 208, 216, 224, 232, 240, 248, 256,
            272, 288, 304, 320, 352, 384, 448, 512,
            516, 520, 524, 528, 532, 536, 540, 548,
            556, 568, 580, 592, 608, 624, 640, 656,
            672, 688, 704, 720, 736, 752, 768, 784,
            800, 816, 832, 848, 864, 880, 896, 912,
            928, 944, 1024
        ],
};
const AVC_WINFO_44K_MODE6: WinInfo = WinInfo {
    is_long:    true,
    bands:      &[
              4,   8,  12,  16,  20,  24,  28,  36,
             44,  56,  68,  80,  96, 112, 128, 144,
            160, 176, 192, 208, 224, 240, 256, 272,
            288, 304, 320, 336, 352, 368, 384, 400,
            416, 432, 512, 520, 528, 536, 544, 552,
            560, 568, 576, 584, 592, 600, 608, 616,
            624, 632, 640, 648, 656, 664, 672, 680,
            688, 696, 704, 712, 720, 728, 736, 744,
            752, 760, 768, 784, 800, 816, 832, 864,
            896, 960, 1024
        ],
};
