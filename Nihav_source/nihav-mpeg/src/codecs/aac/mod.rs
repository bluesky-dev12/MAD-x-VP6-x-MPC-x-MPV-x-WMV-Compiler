use nihav_core::formats::*;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::dsp::mdct::IMDCT;
use nihav_codec_support::dsp::window::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::byteio::*;
use std::str::FromStr;

mod coeff_read;
use coeff_read::*;
mod info;
use info::*;
mod sbr;
use sbr::{SBRHeader, SBRCodebooks, SBRState, SBRChannel, SBRDSP, sbr_read_sce, sbr_read_cpe};
#[allow(clippy::excessive_precision)]
mod tables;
use tables::*;
mod tools;
use tools::*;

const MAX_WINDOWS:  usize = 8;
const MAX_SFBS:     usize = 64;

#[derive(Clone,Copy)]
pub struct ICSInfo {
    window_sequence:        u8,
    prev_window_sequence:   u8,
    window_shape:           bool,
    prev_window_shape:      bool,
    scale_factor_grouping:  [bool; MAX_WINDOWS],
    group_start:            [usize; MAX_WINDOWS],
    window_groups:          usize,
    num_windows:            usize,
    max_sfb:                usize,
    predictor_data:         Option<LTPData>,
    long_win:               bool,
}

const ONLY_LONG_SEQUENCE:   u8 = 0;
const LONG_START_SEQUENCE:  u8 = 1;
const EIGHT_SHORT_SEQUENCE: u8 = 2;
const LONG_STOP_SEQUENCE:   u8 = 3;

impl ICSInfo {
    fn new() -> Self {
        Self {
            window_sequence:        0,
            prev_window_sequence:   0,
            window_shape:           false,
            prev_window_shape:      false,
            scale_factor_grouping:  [false; MAX_WINDOWS],
            group_start:            [0; MAX_WINDOWS],
            num_windows:            0,
            window_groups:          0,
            max_sfb:                0,
            predictor_data:         None,
            long_win:               true,
        }
    }
    fn decode_ics_info(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        self.prev_window_sequence = self.window_sequence;
        self.prev_window_shape    = self.window_shape;
        let ics_reserved_bit                            = br.read(1)?;
        validate!(ics_reserved_bit == 0);
        self.window_sequence                            = br.read(2)? as u8;
        /*match self.prev_window_sequence {
            ONLY_LONG_SEQUENCE | LONG_STOP_SEQUENCE => {
                    if (self.window_sequence != ONLY_LONG_SEQUENCE) &&
                       (self.window_sequence != LONG_START_SEQUENCE) {
                        println!("incorrect previous window");
                    }
                },
            LONG_START_SEQUENCE | EIGHT_SHORT_SEQUENCE => {
                    if (self.window_sequence != EIGHT_SHORT_SEQUENCE) &&
                       (self.window_sequence != LONG_STOP_SEQUENCE) {
                        println!("incorrect previous window");
                    }
                },
            _ => {},
        };*/
        self.window_shape                               = br.read_bool()?;
        self.window_groups = 1;
        if self.window_sequence == EIGHT_SHORT_SEQUENCE {
            self.long_win = false;
            self.num_windows = 8;
            self.max_sfb                                = br.read(4)? as usize;
            for i in 0..MAX_WINDOWS-1 {
                self.scale_factor_grouping[i]           = br.read_bool()?;
                if !self.scale_factor_grouping[i] {
                    self.group_start[self.window_groups] = i + 1;
                    self.window_groups += 1;
                }
            }
        } else {
            self.long_win = true;
            self.num_windows = 1;
            self.max_sfb                                = br.read(6)? as usize;
            self.predictor_data = LTPData::read(br)?;
        }
        Ok(())
    }
    fn get_group_start(&self, g: usize) -> usize {
        if g == 0 {
            0
        } else if g >= self.window_groups {
            if self.long_win { 1 } else { 8 }
        } else {
            self.group_start[g]
        }
    }
}

#[derive(Clone)]
struct ICS {
    global_gain:    u8,
    info:           ICSInfo,
    pulse_data:     Option<PulseData>,
    tns_data:       Option<TNSData>,
    gain_control:   Option<GainControlData>,
    sect_cb:        [[u8; MAX_SFBS]; MAX_WINDOWS],
    sect_len:       [[usize; MAX_SFBS]; MAX_WINDOWS],
    sfb_cb:         [[u8; MAX_SFBS]; MAX_WINDOWS],
    num_sec:        [usize; MAX_WINDOWS],
    scales:         [[u8; MAX_SFBS]; MAX_WINDOWS],
    sbinfo:         GASubbandInfo,
    coeffs:         [f32; 1024],
    delay:          [f32; 1024],
}

const INTENSITY_SCALE_MIN:  i16 = -155;
impl ICS {
    fn new(sbinfo: GASubbandInfo) -> Self {
        Self {
            global_gain:    0,
            info:           ICSInfo::new(),
            pulse_data:     None,
            tns_data:       None,
            gain_control:   None,
            sect_cb:        [[0; MAX_SFBS]; MAX_WINDOWS],
            sect_len:       [[0; MAX_SFBS]; MAX_WINDOWS],
            sfb_cb:         [[0; MAX_SFBS]; MAX_WINDOWS],
            scales:         [[0; MAX_SFBS]; MAX_WINDOWS],
            num_sec:        [0; MAX_WINDOWS],
            sbinfo,
            coeffs:         [0.0; 1024],
            delay:          [0.0; 1024],
        }
    }
    fn decode_section_data(&mut self, br: &mut BitReader, may_have_intensity: bool) -> DecoderResult<()> {
        let sect_bits = if self.info.long_win { 5 } else { 3 };
        let sect_esc_val = (1 << sect_bits) - 1;

        for g in 0..self.info.window_groups {
            let mut k = 0;
            let mut l = 0;
            while k < self.info.max_sfb {
                self.sect_cb[g][l]                      = br.read(4)? as u8;
                self.sect_len[g][l] = 0;
                validate!(self.sect_cb[g][l] != RESERVED_HCB);
                if ((self.sect_cb[g][l] == INTENSITY_HCB) || (self.sect_cb[g][l] == INTENSITY_HCB2)) && !may_have_intensity {
                    return Err(DecoderError::InvalidData);
                }
                loop {
                    let sect_len_incr                   = br.read(sect_bits)? as usize;
                    self.sect_len[g][l] += sect_len_incr;
                    if sect_len_incr < sect_esc_val { break; }
                }
                validate!(k + self.sect_len[g][l] <= self.info.max_sfb);
                for _ in 0..self.sect_len[g][l] {
                    self.sfb_cb[g][k] = self.sect_cb[g][l];
                    k += 1;
                }
                l += 1;
            }
            self.num_sec[g] = l;
        }
        Ok(())
    }
    fn is_intensity(&self, g: usize, sfb: usize) -> bool {
        (self.sfb_cb[g][sfb] == INTENSITY_HCB) || (self.sfb_cb[g][sfb] == INTENSITY_HCB2)
    }
    fn get_intensity_dir(&self, g: usize, sfb: usize) -> bool {
        self.sfb_cb[g][sfb] == INTENSITY_HCB
    }
    fn is_noise(&self, g: usize, sfb: usize) -> bool {
        self.sfb_cb[g][sfb] == NOISE_HCB
    }
    fn decode_scale_factor_data(&mut self, br: &mut BitReader, codebooks: &Codebooks) -> DecoderResult<()> {
        decode_scale_factor_data(br, &mut self.scales, self.global_gain, &self.info, &self.sfb_cb, codebooks)
    }
    fn get_band_start(&self, swb: usize) -> usize {
        if self.info.long_win {
            self.sbinfo.long_bands[swb]
        } else {
            self.sbinfo.short_bands[swb]
        }
    }
    fn get_num_bands(&self) -> usize {
        if self.info.long_win {
            self.sbinfo.long_bands.len() - 1
        } else {
            self.sbinfo.short_bands.len() - 1
        }
    }
    fn decode_spectrum(&mut self, br: &mut BitReader, codebooks: &Codebooks) -> DecoderResult<()> {
        self.coeffs = [0.0; 1024];
        decode_spectrum(br, &mut self.coeffs, &self.scales, &self.info, &self.sbinfo, &self.sfb_cb, codebooks)
    }
    fn iquant(val: f32) -> f32 {
        if val < 0.0 {
            -((-val).powf(4.0 / 3.0))
        } else {
            val.powf(4.0 / 3.0)
        }
    }
    fn place_pulses(&mut self) {
        if let Some(ref pdata) = self.pulse_data {
            if pdata.pulse_start_sfb >= self.sbinfo.long_bands.len() - 1 { return; }
            let mut k = self.get_band_start(pdata.pulse_start_sfb);
            let mut band = pdata.pulse_start_sfb;
            for pno in 0..pdata.number_pulse {
                k += pdata.pulse_offset[pno] as usize;
                if k >= 1024 { return; }
                while self.get_band_start(band + 1) <= k { band += 1; }
                let scale = get_scale(self.scales[0][band]);
                let mut base = self.coeffs[k];
                if base != 0.0 {
                    base = requant(self.coeffs[k], scale);
                }
                if base > 0.0 {
                    base += f32::from(pdata.pulse_amp[pno]);
                } else {
                    base -= f32::from(pdata.pulse_amp[pno]);
                }
                self.coeffs[k] = Self::iquant(base) * scale;
            }
        }
    }
    fn decode_ics(&mut self, br: &mut BitReader, codebooks: &Codebooks, m4atype: M4AType, common_window: bool, may_have_intensity: bool) -> DecoderResult<()> {
        self.global_gain                                = br.read(8)? as u8;
        if !common_window {
            self.info.decode_ics_info(br)?;
        }
        self.decode_section_data(br, may_have_intensity)?;
        self.decode_scale_factor_data(br, codebooks)?;
        self.pulse_data = PulseData::read(br)?;
        validate!(self.pulse_data.is_none() || self.info.long_win);
        let tns_max_order;
        if !self.info.long_win {
            tns_max_order = 7;
        } else if m4atype == M4AType::LC {
            tns_max_order = 12;
        } else {
            tns_max_order = TNS_MAX_ORDER;
        }
        self.tns_data = TNSData::read(br, self.info.long_win, self.info.num_windows, tns_max_order)?;
        if m4atype == M4AType::SSR {
            self.gain_control = GainControlData::read(br)?;
        } else {
            let gain_control_data_present               = br.read_bool()?;
            validate!(!gain_control_data_present);
        }
        self.decode_spectrum(br, codebooks)?;
        Ok(())
    }
    fn synth_channel(&mut self, dsp: &mut DSP, dst: &mut [f32], srate_idx: usize) {
        self.place_pulses();
        if let Some(ref tns_data) = self.tns_data {
            let tns_max_bands = TNSData::get_max_bands(self.info.long_win, srate_idx).min(self.info.max_sfb);
            for w in 0..self.info.num_windows {
                let mut bottom = self.get_num_bands();
                for f in 0..tns_data.n_filt[w] {
                    let top = bottom;
                    bottom = if top >= tns_data.coeffs[w][f].length { top - tns_data.coeffs[w][f].length } else { 0 };
                    let order = tns_data.coeffs[w][f].order;
                    if order == 0 { continue; }
                    let start = w * 128 + self.get_band_start(tns_max_bands.min(bottom));
                    let end   = w * 128 + self.get_band_start(tns_max_bands.min(top));
                    tns_data.apply(&mut self.coeffs, w, f, start, end);
                }
            }
        }
        dsp.synth(&self.coeffs, &mut self.delay, self.info.window_sequence, self.info.window_shape, self.info.prev_window_shape, dst);
    }
}

#[derive(Clone)]
struct ChannelPair {
    pair:               bool,
    channel:            usize,
    common_window:      bool,
    ms_mask_present:    u8,
    ms_used:            [[bool; MAX_SFBS]; MAX_WINDOWS],
    ics:                [ICS; 2],
    sbr_hdr:            SBRHeader,
    sbr_state:          SBRState,
    sbr_ch:             [SBRChannel; 2],
    do_sbr:             bool,
}

impl ChannelPair {
    fn new(pair: bool, channel: usize, sbinfo: GASubbandInfo) -> Self {
        Self {
            pair, channel,
            common_window:      false,
            ms_mask_present:    0,
            ms_used:            [[false; MAX_SFBS]; MAX_WINDOWS],
            ics:                [ICS::new(sbinfo), ICS::new(sbinfo)],
            sbr_hdr:            SBRHeader::new(),
            sbr_state:          SBRState::new(),
            sbr_ch:             [SBRChannel::new(), SBRChannel::new()],
            do_sbr:             false,
        }
    }
    fn decode_ga_sce(&mut self, br: &mut BitReader, codebooks: &Codebooks, m4atype: M4AType) -> DecoderResult<()> {
        self.ics[0].decode_ics(br, codebooks, m4atype, false, false)?;
        Ok(())
    }
    fn decode_ga_cpe(&mut self, br: &mut BitReader, codebooks: &Codebooks, m4atype: M4AType) -> DecoderResult<()> {
        let common_window                               = br.read_bool()?;
        self.common_window = common_window;
        if common_window {
            self.ics[0].info.decode_ics_info(br)?;
            self.ms_mask_present                        = br.read(2)? as u8;
            validate!(self.ms_mask_present != 3);
            if self.ms_mask_present == 1 {
                for g in 0..self.ics[0].info.window_groups {
                    for sfb in 0..self.ics[0].info.max_sfb {
                        self.ms_used[g][sfb]            = br.read_bool()?;
                    }
                }
            }
            self.ics[1].info = self.ics[0].info;
        }
        self.ics[0].decode_ics(br, codebooks, m4atype, common_window, true)?;
        self.ics[1].decode_ics(br, codebooks, m4atype, common_window, true)?;
        if common_window && self.ms_mask_present != 0 {
            let mut g = 0;
            for w in 0..self.ics[0].info.num_windows {
                if w > 0 && !self.ics[0].info.scale_factor_grouping[w - 1] {
                    g += 1;
                }
                for sfb in 0..self.ics[0].info.max_sfb {
                    let start = w * 128 + self.ics[0].get_band_start(sfb);
                    let end   = w * 128 + self.ics[0].get_band_start(sfb + 1);
                    if self.ics[1].is_intensity(g, sfb) {
                        let invert = (self.ms_mask_present == 1) && self.ms_used[g][sfb];
                        let dir = self.ics[1].get_intensity_dir(g, sfb) ^ invert;
                        let scale = 0.5f32.powf(0.25 * (f32::from(self.ics[1].scales[g][sfb]) + f32::from(INTENSITY_SCALE_MIN)));
                        if dir {
                            for i in start..end {
                                self.ics[1].coeffs[i] = scale * self.ics[0].coeffs[i];
                            }
                        } else {
                            for i in start..end {
                                self.ics[1].coeffs[i] = -scale * self.ics[0].coeffs[i];
                            }
                        }
                    } else if ((self.ms_mask_present == 2) || self.ms_used[g][sfb]) && !self.ics[0].is_noise(g, sfb) {
                        for i in start..end {
                            let tmp = self.ics[0].coeffs[i] - self.ics[1].coeffs[i];
                            self.ics[0].coeffs[i] += self.ics[1].coeffs[i];
                            self.ics[1].coeffs[i] = tmp;
                        }
                    }
                }
            }
        }
        Ok(())
    }
    fn decode_sbr(&mut self, buf: &[u8], has_crc: bool, cbs: &SBRCodebooks, srate: u32) -> DecoderResult<()> {
        let mut br = BitReader::new(buf, BitReaderMode::BE);
        if has_crc {
            let _bs_sbr_crc_bits        = br.read(10)?;
        }
        if br.read_bool()? {
            if let Ok(hdr) = SBRHeader::read(&mut br) {
                if self.sbr_hdr.differs_from(&hdr) {
                    self.do_sbr = self.sbr_state.init(&hdr, srate).is_ok();
                    self.sbr_ch[0].reset();
                    self.sbr_ch[1].reset();
                }
                self.sbr_hdr = hdr;
            } else {
                self.do_sbr = false;
            }
        }
        if self.do_sbr {
            if !self.pair {
                sbr_read_sce(&mut br, self.sbr_hdr.amp_res, &self.sbr_state, cbs, &mut self.sbr_ch[0])?;
            } else {
                sbr_read_cpe(&mut br, self.sbr_hdr.amp_res, &self.sbr_state, cbs, &mut self.sbr_ch)?;
            }
        }

        Ok(())
    }
    fn synth_audio(&mut self, dsp: &mut DSP, abuf: &mut NABufferType, srate_idx: usize, upsample: bool) {
        let mut adata = abuf.get_abuf_f32().unwrap();
        let output = adata.get_data_mut().unwrap();
        let off0 = abuf.get_offset(self.channel);
        let off1 = abuf.get_offset(self.channel + 1);
        if !upsample {
            self.ics[0].synth_channel(dsp, &mut output[off0..], srate_idx);
            if self.pair {
                self.ics[1].synth_channel(dsp, &mut output[off1..], srate_idx);
            }
        } else {
            let mut tmp = [0.0; 1024];
            let nchannels = if self.pair { 2 } else { 1 };
            for ch in 0..nchannels {
                let off = if ch == 0 { off0 } else { off1 };
                self.ics[ch].synth_channel(dsp, &mut tmp, srate_idx);
                self.sbr_ch[ch].analysis(&mut dsp.sbr_dsp, &tmp);
                if self.do_sbr {
                    self.sbr_ch[ch].hf_generate(&self.sbr_state);
                    self.sbr_ch[ch].hf_adjust(&self.sbr_state, &self.sbr_hdr);
                } else {
                    self.sbr_ch[ch].bypass();
                }
                self.sbr_ch[ch].synthesis(&mut dsp.sbr_dsp, &mut output[off..][..2048]);
                self.sbr_ch[ch].update_frame();
            }
        }
    }
}

struct DSP {
    kbd_long_win:   [f32; 1024],
    kbd_short_win:  [f32; 128],
    sine_long_win:  [f32; 1024],
    sine_short_win: [f32; 128],
    imdct_long:     IMDCT,
    imdct_short:    IMDCT,
    tmp:            [f32; 2048],
    ew_buf:         [f32; 1152],
    sbr_dsp:        SBRDSP,
}

const SHORT_WIN_POINT0: usize = 512 - 64;
const SHORT_WIN_POINT1: usize = 512 + 64;

impl DSP {
    fn new() -> Self {
        let mut kbd_long_win: [f32; 1024] = [0.0; 1024];
        let mut kbd_short_win: [f32; 128] = [0.0; 128];
        generate_window(WindowType::KaiserBessel(4.0), 1.0, 1024, true, &mut kbd_long_win);
        generate_window(WindowType::KaiserBessel(6.0), 1.0,  128, true, &mut kbd_short_win);
        let mut sine_long_win: [f32; 1024] = [0.0; 1024];
        let mut sine_short_win: [f32; 128] = [0.0; 128];
        generate_window(WindowType::Sine, 1.0, 1024, true, &mut sine_long_win);
        generate_window(WindowType::Sine, 1.0,  128, true, &mut sine_short_win);
        Self {
            kbd_long_win, kbd_short_win,
            sine_long_win, sine_short_win,
            imdct_long: IMDCT::new(1024 * 2, true),
            imdct_short: IMDCT::new(128 * 2, true),
            tmp: [0.0; 2048], ew_buf: [0.0; 1152],
            sbr_dsp: SBRDSP::new(),
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn synth(&mut self, coeffs: &[f32; 1024], delay: &mut [f32; 1024], seq: u8, window_shape: bool, prev_window_shape: bool, dst: &mut [f32]) {
        let long_win  = if window_shape { &self.kbd_long_win  } else { &self.sine_long_win };
        let short_win = if window_shape { &self.kbd_short_win } else { &self.sine_short_win };
        let left_long_win  = if prev_window_shape { &self.kbd_long_win  } else { &self.sine_long_win };
        let left_short_win = if prev_window_shape { &self.kbd_short_win } else { &self.sine_short_win };
        if seq != EIGHT_SHORT_SEQUENCE {
            self.imdct_long.imdct(coeffs, &mut self.tmp);
        } else {
            for (ain, aout) in coeffs.chunks(128).zip(self.tmp.chunks_mut(256)) {
                self.imdct_short.imdct(ain, aout);
            }
            self.ew_buf = [0.0; 1152];
            for (w, src) in self.tmp.chunks(256).enumerate() {
                if w > 0 {
                    for i in 0..128 {
                        self.ew_buf[w * 128 + i] += src[i] * short_win[i];
                    }
                } else { // to be left-windowed
                    for i in 0..128 {
                        self.ew_buf[i] = src[i];
                    }
                }
                for i in 0..128 {
                    self.ew_buf[w * 128 + i + 128] += src[i + 128] * short_win[127 - i];
                }
            }
        }
        if seq == ONLY_LONG_SEQUENCE { // should be the most common case
            for i in 0..1024 {
                dst[i] = delay[i] + self.tmp[i] * left_long_win[i];
                delay[i] = self.tmp[i + 1024] * long_win[1023 - i];
            }
            return;
        }
        // output new data
        match seq {
            ONLY_LONG_SEQUENCE | LONG_START_SEQUENCE => {
                    for i in 0..1024 {
                        dst[i] = self.tmp[i] * left_long_win[i] + delay[i];
                    }
                },
            EIGHT_SHORT_SEQUENCE => {
                    for i in 0..SHORT_WIN_POINT0 {
                        dst[i] = delay[i];
                    }
                    for i in SHORT_WIN_POINT0..SHORT_WIN_POINT1 {
                        let j = i - SHORT_WIN_POINT0;
                        dst[i] = delay[i] + self.ew_buf[j] * left_short_win[j];
                    }
                    for i in SHORT_WIN_POINT1..1024 {
                        let j = i - SHORT_WIN_POINT0;
                        dst[i] = self.ew_buf[j];
                    }
                },
            LONG_STOP_SEQUENCE => {
                    for i in 0..SHORT_WIN_POINT0 {
                        dst[i] = delay[i];
                    }
                    for i in SHORT_WIN_POINT0..SHORT_WIN_POINT1 {
                        dst[i] = delay[i] + self.tmp[i] * left_short_win[i - SHORT_WIN_POINT0];
                    }
                    for i in SHORT_WIN_POINT1..1024 {
                        dst[i] = self.tmp[i];
                    }
                },
            _ => unreachable!(""),
        };
        // save delay
        match seq {
            ONLY_LONG_SEQUENCE | LONG_STOP_SEQUENCE => {
                    for i in 0..1024 {
                        delay[i] = self.tmp[i + 1024] * long_win[1023 - i];
                    }
                },
            EIGHT_SHORT_SEQUENCE => {
                    for i in 0..SHORT_WIN_POINT1 { // last part is already windowed
                        delay[i] = self.ew_buf[i + 512+64];
                    }
                    for i in SHORT_WIN_POINT1..1024 {
                        delay[i] = 0.0;
                    }
                },
            LONG_START_SEQUENCE   => {
                    for i in 0..SHORT_WIN_POINT0 {
                        delay[i] = self.tmp[i + 1024];
                    }
                    for i in SHORT_WIN_POINT0..SHORT_WIN_POINT1 {
                        delay[i] = self.tmp[i + 1024] * short_win[127 - (i - SHORT_WIN_POINT0)];
                    }
                    for i in SHORT_WIN_POINT1..1024 {
                        delay[i] = 0.0;
                    }
                },
            _ => unreachable!(""),
        };
    }
}

struct AACDecoder {
    info:       NACodecInfoRef,
    chmap:      NAChannelMap,
    m4ainfo:    M4AInfo,
    pairs:      Vec<ChannelPair>,
    codebooks:  Codebooks,
    dsp:        DSP,
    sbinfo:     GASubbandInfo,
    sbr_cbs:    SBRCodebooks,
    upsample:   bool,
}

impl AACDecoder {
    fn new() -> Self {
        AACDecoder {
            info:       NACodecInfo::new_dummy(),
            chmap:      NAChannelMap::new(),
            m4ainfo:    M4AInfo::new(),
            pairs:      Vec::new(),
            codebooks:  Codebooks::new(),
            dsp:        DSP::new(),
            sbinfo:     AAC_SUBBAND_INFO[0],
            sbr_cbs:    SBRCodebooks::new(),
            upsample:   false,
        }
    }
    fn set_pair(&mut self, pair_no: usize, channel: usize, pair: bool) -> DecoderResult<()> {
        if self.pairs.len() <= pair_no {
            self.pairs.push(ChannelPair::new(pair, channel, self.sbinfo));
        } else {
            validate!(self.pairs[pair_no].channel == channel);
            validate!(self.pairs[pair_no].pair    == pair);
        }
        validate!(if pair { channel + 1 } else { channel } < self.m4ainfo.channels);
        Ok(())
    }
    fn decode_ga(&mut self, br: &mut BitReader, abuf: &mut NABufferType) -> DecoderResult<()> {
        let mut cur_pair = 0;
        let mut cur_ch   = 0;
        while br.left() > 3 {
            let id                                      = br.read(3)?;
            match id {
                0 => { // ID_SCE
                        let _tag                        = br.read(4)?;
                        self.set_pair(cur_pair, cur_ch, false)?;
                        self.pairs[cur_pair].decode_ga_sce(br, &self.codebooks, self.m4ainfo.otype)?;
                        cur_pair += 1;
                        cur_ch   += 1;
                    },
                1 => { // ID_CPE
                        let _tag                        = br.read(4)?;
                        self.set_pair(cur_pair, cur_ch, true)?;
                        self.pairs[cur_pair].decode_ga_cpe(br, &self.codebooks, self.m4ainfo.otype)?;
                        cur_pair += 1;
                        cur_ch   += 2;
                    },
                2 => { // ID_CCE
                        unimplemented!("coupling channel element");
                    },
                3 => { // ID_LFE
                        let _tag                        = br.read(4)?;
                        self.set_pair(cur_pair, cur_ch, false)?;
                        self.pairs[cur_pair].decode_ga_sce(br, &self.codebooks, self.m4ainfo.otype)?;
                        cur_pair += 1;
                        cur_ch   += 1;
                    },
                4 => { // ID_DSE
                        let _id                         = br.read(4)?;
                        let align                       = br.read_bool()?;
                        let mut count                   = br.read(8)?;
                        if count == 255 { count        += br.read(8)?; }
                        if align {                        br.align(); }
                                                          br.skip(count * 8)?; // no SBR payload or such
                    },
                5 => { // ID_PCE
                        skimp_through_program_config_element(br)?;
                    },
                6 => { // ID_FIL
                        let mut count                   = br.read(4)? as usize;
                        if count == 15 {
                            count                      += br.read(8)? as usize;
                            count -= 1;
                        }
                        if count > 0 {
                            let extension_type          = br.read(4)?;
                            match extension_type {
                                0xD | 0xE => { // SBR data without or with CRC
                                    let has_crc = extension_type == 0xE;

                                    let mut buf = [0; 256 + 16];
                                    for el in buf[..count - 1].iter_mut() {
                                        *el             = br.read(8)? as u8;
                                    }
                                    buf[count - 1]      = br.read(4)? as u8;
                                    if cur_pair > 0 {
                                        self.pairs[cur_pair - 1].decode_sbr(&buf[..count], has_crc, &self.sbr_cbs, self.m4ainfo.srate * 2)?;
                                    }
                                },
                                _ => {
                                    for _ in 0..count-1 {
                                                          br.skip(8)?;
                                    }
                                                          br.skip(4)?;
                                },
                            };
                        }
                    },
                7 => { // ID_TERM
                        break;
                    },
                _ => { unreachable!(); },
            };
        }
        let srate_idx = GASubbandInfo::find_idx(self.m4ainfo.srate);
        for pair in 0..cur_pair {
            self.pairs[pair].synth_audio(&mut self.dsp, abuf, srate_idx, self.upsample);
        }
        Ok(())
    }
}

impl NADecoder for AACDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let edata = info.get_extradata().unwrap();
            validate!(edata.len() >= 2);

            if (edata.len() > 12) && (&edata[4..8] == b"esds") {
                let mut mr = MemoryReader::new_read(edata.as_slice());
                let mut br = ByteReader::new(&mut mr);
                let esds_size           = br.read_u32be()? as usize;
                validate!(esds_size <= edata.len());
                                          br.read_skip(8)?;
                let mut info_start = 0;
                let mut info_size = 0;
                while br.tell() < (esds_size as u64) {
                    let tag             = br.read_byte()?;
                    let mut size = 0;
                    loop {
                        let b           = br.read_byte()?;
                        size = (size << 7) | u64::from(b & 0x7F);
                        validate!(br.tell() + size <= (esds_size as u64));
                        if (b & 0x80) == 0 {
                            break;
                        }
                    }
                    match tag {
                        3 => {
                                          br.read_u16be()?;
                            let flags   = br.read_byte()?;
                            if (flags & 0x80) != 0 {
                                          br.read_u16be()?;
                            }
                            if (flags & 0x40) != 0 {
                                let len = br.read_byte()?;
                                          br.read_skip(len as usize)?;
                            }
                            if (flags & 0x20) != 0 {
                                          br.read_u16be()?;
                            }
                        },
                        4 => {
                            let _otype  = br.read_byte()?;
                            let _stype  = br.read_byte()?;
                            let _flags  = br.read_u24be()?;
                            let _max_br = br.read_u32be()?;
                            let _min_br = br.read_u32be()?;
                        },
                        5 => {
                            info_start = br.tell() as usize;
                            info_size = size as usize;
                            break;
                        },
                        _ => br.read_skip(size as usize)?,
                    }
                }
                validate!(info_start > 0 && info_size > 0);
                self.m4ainfo.read(&edata[info_start..][..info_size])?;
            } else {
                self.m4ainfo.read(&edata)?;
            }

            //println!("{}", self.m4ainfo);
            if (self.m4ainfo.otype != M4AType::LC) || (self.m4ainfo.channels > 2) || (self.m4ainfo.samples != 1024) {
                return Err(DecoderError::NotImplemented);
            }
            self.sbinfo = GASubbandInfo::find(self.m4ainfo.srate);

            self.upsample = self.m4ainfo.srate < 32000;
            let (srate, samples) = if !self.upsample {
                    (self.m4ainfo.srate, self.m4ainfo.samples)
                } else {
                    (self.m4ainfo.srate * 2, self.m4ainfo.samples * 2)
                };

            let ainfo = NAAudioInfo::new(srate, self.m4ainfo.channels as u8,
                                         SND_F32P_FORMAT, samples);
            self.info = info.replace_info(NACodecTypeInfo::Audio(ainfo));

            if self.m4ainfo.channels >= DEFAULT_CHANNEL_MAP.len() {
                return Err(DecoderError::NotImplemented);
            }
            let chmap_str = DEFAULT_CHANNEL_MAP[self.m4ainfo.channels];
            if chmap_str.is_empty() { return Err(DecoderError::NotImplemented); }
            self.chmap = NAChannelMap::from_str(chmap_str).unwrap();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        validate!(info.get_properties().is_audio());
        let pktbuf = pkt.get_buffer();

        let ainfo = self.info.get_properties().get_audio_info().unwrap();
        let samples = if !self.upsample { self.m4ainfo.samples } else { self.m4ainfo.samples * 2 };
        let mut abuf = alloc_audio_buffer(ainfo, samples, self.chmap.clone())?;

        let mut br = BitReader::new(&pktbuf, BitReaderMode::BE);
        match self.m4ainfo.otype {
            M4AType::LC => {
                    self.decode_ga(&mut br, &mut abuf)?;
                },
            _ => { unimplemented!(""); }
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.replace_info(NACodecTypeInfo::Audio(ainfo)), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        for pair in self.pairs.iter_mut() {
            pair.ics[0].delay = [0.0; 1024];
            pair.ics[1].delay = [0.0; 1024];
        }
    }
}

impl NAOptionHandler for AACDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(AACDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::test_decode_audio;
    use crate::mpeg_register_all_decoders;
    use nihav_realmedia::realmedia_register_all_demuxers;
    #[test]
    fn test_aac() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        mpeg_register_all_decoders(&mut dec_reg);

//        let file = "assets/RV/rv40_weighted_mc.rmvb";
        let file = "assets/RV/rv40_weighted_mc_2.rmvb";
        test_decode_audio("realmedia", file, Some(12000), None/*Some("aac")*/, &dmx_reg, &dec_reg);
    }
    #[test]
    fn test_aac_sbr() {
        let mut dmx_reg = RegisteredDemuxers::new();
        nihav_commonfmt::generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        mpeg_register_all_decoders(&mut dec_reg);

        let file = "assets/MPEG/SBRtestStereoAot29Sig0.mp4";
        test_decode_audio("mov", file, Some(400), None/*Some("aacsbr")*/, &dmx_reg, &dec_reg);
    }
}

const DEFAULT_CHANNEL_MAP: [&str; 9] = [
    "",
    "C",
    "L,R",
    "C,L,R",
    "C,L,R,Cs",
    "C,L,R,Ls,Rs",
    "C,L,R,Ls,Rs,LFE",
    "",
    "C,L,R,Ls,Rs,Lss,Rss,LFE",
];
