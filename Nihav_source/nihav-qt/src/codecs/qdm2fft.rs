use nihav_core::codecs::*;
use nihav_core::io::codebook::*;
use nihav_codec_support::dsp::fft::*;
use super::qdmcommon::*;
use super::qdm2::TONE_SCALES;
use std::f32::consts::PI;

struct Codebooks {
    group_cb:           [Codebook<u8>; 5],
    level_exp_alt_cb:   Codebook<u8>,
    level_exp_cb:       Codebook<u8>,
    stereo_exp_cb:      Codebook<u8>,
    phase_diff_cb:      Codebook<u8>,
}

fn map_idx(idx: usize) -> u8 { idx as u8 }

macro_rules! create_codebook {
    ($codes: expr, $bits: expr) => ({
        let mut cbr = TableCodebookDescReader::new($codes, $bits, map_idx);
        Codebook::new(&mut cbr, CodebookMode::LSB).unwrap()
    })
}

impl Codebooks {
    fn new() -> Self {
        let cb0 = create_codebook!(FFT_GROUP_CODES[0], FFT_GROUP_BITS[0]);
        let cb1 = create_codebook!(FFT_GROUP_CODES[1], FFT_GROUP_BITS[1]);
        let cb2 = create_codebook!(FFT_GROUP_CODES[2], FFT_GROUP_BITS[2]);
        let cb3 = create_codebook!(FFT_GROUP_CODES[3], FFT_GROUP_BITS[3]);
        let cb4 = create_codebook!(FFT_GROUP_CODES[4], FFT_GROUP_BITS[4]);
        let group_cb = [cb0, cb1, cb2, cb3, cb4];
        let level_exp_alt_cb = create_codebook!(FFT_LEVEL_EXP_ALT_CODES, FFT_LEVEL_EXP_ALT_BITS);
        let level_exp_cb = create_codebook!(FFT_LEVEL_EXP_CODES, FFT_LEVEL_EXP_BITS);
        let stereo_exp_cb = create_codebook!(FFT_STEREO_EXP_CODES, FFT_STEREO_EXP_BITS);
        let phase_diff_cb = create_codebook!(FFT_STEREO_PHASE_CODES, FFT_STEREO_PHASE_BITS);
        Self {
            group_cb, level_exp_alt_cb, level_exp_cb,
            stereo_exp_cb, phase_diff_cb,
        }
    }
}

#[derive(Clone,Copy)]
struct OldTone {
    used:       bool,
    phase:      i32,
    phase_add:  i32,
    time:       u8,
    scale:      f32,
    cutoff:     usize,
    grp:        u8,
    offset:     u8,
    freq:       u16,
    ch:         u8,
}

struct Synth {
    rdft:               RDFT,
    sbuf:               [[FFTComplex; 256]; 2],
    delay:              [[f32; 256]; 2],
}

impl Synth {
    fn new() -> Self {
        Self {
            rdft:           RDFTBuilder::new_rdft(256, true, true),
            sbuf:           [[FFTC_ZERO; 256]; 2],
            delay:          [[0.0; 256]; 2],
        }
    }
    fn new_sf(&mut self) {
        self.sbuf = [[FFTC_ZERO; 256]; 2];
    }
    fn synth_old_tone(&mut self, tone: &mut OldTone) {
        let off = tone.offset as usize;
        let scale = tone.scale * TONE_ENVELOPE[tone.grp as usize][tone.time as usize];
        tone.phase += tone.phase_add;

        let val = FFTComplex::exp((tone.phase as f32) * PI / 256.0).scale(scale);
        let ch = tone.ch as usize;

        if tone.grp >= 3 || tone.cutoff >= 3 {
            self.sbuf[ch][off]     += val;
            self.sbuf[ch][off + 1] -= val;
        } else {
            let tab = &FFT_TONE_SAMPLES[tone.grp as usize][(tone.freq as usize) - (off << (4 - tone.grp))];
            let wave = [tab[3] - tab[0], -tab[4], 1.0 - tab[2] - tab[3],
                        tab[1] + tab[4] - 1.0, tab[0] - tab[1], tab[2]];
            let cidx = CUTOFF_INDICES[tone.cutoff];
            let coff0 = (off as isize) + (cidx[0] as isize);
            let coff1 = (off as isize) + (cidx[1] as isize);
            if coff0 >= 0 {
                let imw = if tone.cutoff == 0 { -wave[0] } else { wave[0] };
                self.sbuf[ch][coff0 as usize].re += val.re * wave[0];
                self.sbuf[ch][coff0 as usize].im += val.im * imw;
            }
            if coff1 >= 0 {
                let imw = if tone.cutoff <= 1 { -wave[1] } else { wave[1] };
                self.sbuf[ch][coff1 as usize].re += val.re * wave[1];
                self.sbuf[ch][coff1 as usize].im += val.im * imw;
            }
            for i in 0..4 {
                self.sbuf[ch][off + i] += val.scale(wave[i + 2]);
            }
        }
        tone.time += 1;
        if tone.time >= (1 << (5 - tone.grp)) - 1 {
            tone.used = false;
        }
    }
    fn synth_tone(&mut self, tone: &mut Tone, cutoff: usize, scale: f32, off: usize, grp: usize) -> Option<OldTone> {
        let mut old_tone = OldTone {
                used:       true,
                phase:      64 * i32::from(tone.phase) - (off as i32) * 256 - 128,
                phase_add:  (2 * i32::from(tone.freq) + 1) << (grp + 3),
                grp:        grp as u8,
                time:       0,
                scale,
                cutoff,
                offset:     off as u8,
                ch:         tone.ch,
                freq:       tone.freq,
            };
        self.synth_old_tone(&mut old_tone);
        if old_tone.used {
            Some(old_tone)
        } else {
            None
        }
    }
    fn synth_tone4(&mut self, tone: Tone, scale: f32) {
        let ch = tone.ch as usize;
        let val = FFTComplex::exp(f32::from(tone.phase) * PI * 0.25).scale(scale);
        let offset = tone.freq as usize;
        self.sbuf[ch][offset]     += val;
        self.sbuf[ch][offset + 1] -= val;
    }
    fn synth_rdft(&mut self, dst: &mut [f32], ch: usize, len: usize) {
        self.sbuf[ch][0].re *= 2.0;
        self.sbuf[ch][0].im  = 0.0;
        self.rdft.do_rdft_inplace(&mut self.sbuf[ch]);
        let scale = 1.0 / 2.0;
        for ((src, dly), dst) in self.sbuf[ch].iter().take(len / 2).zip(self.delay[ch].chunks(2)).zip(dst.chunks_mut(2)) {
            dst[0] += src.re * scale + dly[0];
            dst[1] += src.im * scale + dly[1];
        }
        for (src, dly) in self.sbuf[ch].iter().skip(len / 2).take(len / 2).zip(self.delay[ch].chunks_mut(2)) {
            dly[0] = src.re * scale;
            dly[1] = src.im * scale;
        }
    }
}

pub struct QDM2FFT {
    cbs:                Codebooks,
    synth:              Synth,
    packet_size:        usize,
    frame_size:         usize,
    channels:           usize,
    subsampling:        u8,
    freq_range:         usize,

    fft_levels:         [u8; 6],

    tones:              [Vec<Tone>; 5],
    tone_start:         [usize; 5],
    old_tones:          Vec<OldTone>,

    pub is_intra:       bool,
    frame_bits:         u8,
}

impl QDM2FFT {
    pub fn new() -> Self {
        Self {
            cbs:            Codebooks::new(),
            synth:          Synth::new(),
            packet_size:    0,
            frame_size:     0,
            channels:       0,
            subsampling:    0,
            freq_range:     0,

            fft_levels:     [0; 6],
            tones:          [Vec::with_capacity(MAX_TONES),
                             Vec::with_capacity(MAX_TONES),
                             Vec::with_capacity(MAX_TONES),
                             Vec::with_capacity(MAX_TONES),
                             Vec::with_capacity(MAX_TONES)],
            tone_start:     [0; 5],
            old_tones:      Vec::new(),

            is_intra:       false,
            frame_bits:     0,
        }
    }
    pub fn set_params(&mut self, channels: usize, packet_size: usize, subsampling: u8) {
        self.subsampling = subsampling;
        self.channels = channels;
        self.synth.rdft = RDFTBuilder::new_rdft(packet_size, false, true);
        self.packet_size = packet_size;
        self.frame_size = packet_size * 16;
        self.frame_bits = (31 - (self.frame_size.leading_zeros() & 31)) as u8;
        self.freq_range = 255 >> (2 - self.subsampling);
    }
    pub fn read_type_13(&mut self, br: &mut QdmBitReader) -> DecoderResult<()> {
        for el in self.fft_levels.iter_mut() {
            *el                 = br.read(6) as u8;
        }
        Ok(())
    }
    pub fn read_type_14(&mut self, br: &mut QdmBitReader) -> DecoderResult<()> {
        for el in self.fft_levels.iter_mut() {
            *el                 = br.read_code(&self.cbs.level_exp_cb)? as u8;
        }
        Ok(())
    }
    fn read_tones(&mut self, br: &mut QdmBitReader, group: usize, use_cb2: bool) -> DecoderResult<()> {
        let group_bits = 4 - group;
        let group_size = 1 << (self.frame_bits - (group as u8));
        let mut freq = 1;
        let mut pos2 = 0;
        let mut offset = 0;
        let level_cb = if use_cb2 { &self.cbs.level_exp_cb } else { &self.cbs.level_exp_alt_cb };
        while freq < self.frame_size {
            if self.is_intra {
                let mut diff = 0;
                while diff < 2 {
                    diff                = br.read_code_long(&self.cbs.group_cb[group_bits])?;
                    if diff >= 2 { break; }
                    freq = 1;
                    if diff == 0 {
                        pos2    += group_size;
                        offset  += 1 << group_bits;
                    } else {
                        pos2    += 8 * group_size;
                        offset  += 8 << group_bits;
                    }
                    if pos2 >= self.frame_size {
                        return Ok(());
                    }
                }
                freq += (diff - 2) as usize;
            } else {
                let diff                = br.read_code_long(&self.cbs.group_cb[group_bits])?;
                freq += diff as usize;
                while freq >= group_size - 1 {
                    freq   -= group_size - 2;
                    pos2   += group_size;
                    offset += 1 << group_bits;
                }
            }
            if pos2 >= self.frame_size {
                return Ok(());
            }

            let pos = freq >> group_bits;
            if pos > LEVEL_INDEX.len() - 1 {
                return Ok(());
            }
            let stereo_mode = if self.channels == 2 {
                                        br.read(2) as u8
                    } else { 0 };

            let mut amp                 = br.read_code(level_cb)? as i8;
            amp += self.fft_levels[LEVEL_INDEX[pos] as usize] as i8;
            if amp < 0 {
                amp = 0;
            }
            let phase                   = br.read(3) as u8;
            let (amp2, phase2) = if stereo_mode > 1 {
                    let amp_diff        = br.read_code(&self.cbs.stereo_exp_cb)? as i8;
                    let phase_diff      = br.read_code(&self.cbs.phase_diff_cb)? as i8;
                    let mut p2 = (phase as i8) - phase_diff;
                    if p2 < 0 { p2 += 8; }
                    (amp - amp_diff, p2 as u8)
                } else { (0, 0) };

            if pos <= self.freq_range {
                if self.tones[group].len() >= MAX_TONES { return Ok(()); }
                self.tones[group].push(Tone { freq: freq as u16, offset, phase, ch: stereo_mode & 1, amp_idx: amp as u8 });
                if stereo_mode > 1 {
                    if self.tones[group].len() >= MAX_TONES { return Ok(()); }
                    self.tones[group].push(Tone { freq: freq as u16, offset, phase: phase2, ch: !stereo_mode & 1, amp_idx: amp2 as u8 });
                }
            }

            freq += 1;
        }
        Ok(())
    }
    pub fn new_frame(&mut self) {
        for tones in self.tones.iter_mut() {
            tones.clear();
        }
        self.tone_start = [0; 5];
    }
    pub fn read_fft_packet(&mut self, id: u8, br: &mut QdmBitReader) -> DecoderResult<()> {
        match id {
            17..=23 => {
                let grp = i16::from(self.subsampling) + 4 - i16::from(id - 17);
                if (0..5).contains(&grp) {
                    self.read_tones(br, grp as usize, false)?;
                }
            },
            31 => {
                for grp in 0..5 {
                    self.read_tones(br, grp, false)?;
                }
            },
            33..=39 => {
                let grp = i16::from(self.subsampling) + 4 - i16::from(id - 33);
                if (0..5).contains(&grp) {
                    self.read_tones(br, grp as usize, true)?;
                }
            },
            46 => {
                for el in self.fft_levels.iter_mut() {
                    *el                 = br.read(6) as u8;
                }
                for grp in 0..5 {
                    self.read_tones(br, grp, true)?;
                }
            },
            _ => {},
        };
        Ok(())
    }
    pub fn generate_tones(&mut self, sf: usize) {
        self.synth.new_sf();
        self.old_tones.retain(|el| el.used);

        for otone in self.old_tones.iter_mut() {
            self.synth.synth_old_tone(otone);
        }

        let sf_idx = (sf + 0xE) & 0xF;
        let scales = if self.is_intra { &TONE_SCALES[0] } else { &TONE_SCALES[1] };
        for group in 0..4 {
            let group_bits = 4 - group;
            for tone in self.tones[group].iter_mut().skip(self.tone_start[group]) {
                if (tone.offset as usize) > sf_idx {
                    break;
                }

                let off = (tone.freq >> group_bits) as usize;
                if off < self.freq_range {
                    let cutoff = if off < 2 { off } else if off < 60 { 2 } else { 3 };
                    let scale = scales[(tone.amp_idx & 0x3F) as usize];
                    let otone = self.synth.synth_tone(tone, cutoff, scale, off, group);
                    if let Some(otone) = otone {
                        self.old_tones.push(otone);
                    }
                }

                self.tone_start[group] += 1;
            }
        }
        {
            let group = 4;
            for tone in self.tones[group].iter().skip(self.tone_start[group]) {
                if (tone.offset as usize) > sf_idx {
                    break;
                }
                let scale = scales[(tone.amp_idx & 0x3F) as usize];
                self.synth.synth_tone4(*tone, scale);
                self.tone_start[group] += 1;
            }
        }
    }
    pub fn synth(&mut self, dst: &mut [f32], ch: usize) {
        self.synth.synth_rdft(dst, ch, self.packet_size);
    }
    pub fn flush(&mut self) {
    }
}

const FFT_GROUP_CODES: [&[u16]; 5] = [
  &[
    0x038E, 0x0001, 0x0000, 0x0022, 0x000A, 0x0006, 0x0012, 0x0002,
    0x001E, 0x003E, 0x0056, 0x0016, 0x000E, 0x0032, 0x0072, 0x0042,
    0x008E, 0x004E, 0x00F2, 0x002E, 0x0036, 0x00C2, 0x018E
  ],
  &[
    0x07A4, 0x0001, 0x0020, 0x0012, 0x001C, 0x0008, 0x0006, 0x0010,
    0x0000, 0x0014, 0x0004, 0x0032, 0x0070, 0x000C, 0x0002, 0x003A,
    0x001A, 0x002C, 0x002A, 0x0022, 0x0024, 0x000A, 0x0064, 0x0030,
    0x0062, 0x00A4, 0x01A4, 0x03A4
  ],
  &[
    0x1760, 0x0001, 0x0000, 0x0082, 0x000C, 0x0006, 0x0003, 0x0007,
    0x0008, 0x0004, 0x0010, 0x0012, 0x0022, 0x001A, 0x0000, 0x0020,
    0x000A, 0x0040, 0x004A, 0x006A, 0x002A, 0x0042, 0x0002, 0x0060,
    0x00AA, 0x00E0, 0x00C2, 0x01C2, 0x0160, 0x0360, 0x0760, 0x0F60
  ],
  &[
    0x33EA, 0x0005, 0x0000, 0x000C, 0x0000, 0x0006, 0x0003, 0x0008,
    0x0002, 0x0001, 0x0004, 0x0007, 0x001A, 0x000F, 0x001C, 0x002C,
    0x000A, 0x001D, 0x002D, 0x002A, 0x000D, 0x004C, 0x008C, 0x006A,
    0x00CD, 0x004D, 0x00EA, 0x020C, 0x030C, 0x010C, 0x01EA, 0x07EA,
    0x0BEA, 0x03EA, 0x13EA
  ],
  &[
    0x5282, 0x0016, 0x0000, 0x0136, 0x0004, 0x0000, 0x0007, 0x000A,
    0x000E, 0x0003, 0x0001, 0x000D, 0x0006, 0x0009, 0x0012, 0x0005,
    0x0025, 0x0022, 0x0015, 0x0002, 0x0076, 0x0035, 0x0042, 0x00C2,
    0x0182, 0x00B6, 0x0036, 0x03C2, 0x0482, 0x01C2, 0x0682, 0x0882,
    0x0A82, 0x0082, 0x0282, 0x1282, 0x3282, 0x2282
  ]
];
const FFT_GROUP_BITS: [&[u8]; 5] = [
  &[
    10,  1,  2,  6,  4,  5,  6,  7,  6,  6,  7,  7,  8,  7,  8,  8,
     9,  7,  8,  6,  6,  8, 10
  ],
  &[
    11,  1,  6,  6,  5,  4,  3,  6,  6,  5,  6,  6,  7,  6,  6,  6,
     6,  6,  6,  7,  8,  6,  7,  7,  7,  9, 10, 11
  ],
  &[
    13,  2,  0,  8,  4,  3,  3,  3,  4,  4,  5,  5,  6,  5,  7,  7,
     7,  7,  7,  7,  8,  8,  8,  9,  8,  8,  9,  9, 10, 11, 13, 12
  ],
  &[
    14,  4,  0, 10,  4,  3,  3,  4,  4,  3,  4,  4,  5,  4,  5,  6,
     6,  5,  6,  7,  7,  7,  8,  8,  8,  8,  9, 10, 10, 10, 10, 11,
    12, 13, 14
  ],
  &[
    15,  6,  0,  9,  3,  3,  3,  4,  4,  3,  4,  4,  5,  4,  5,  6,
     6,  6,  6,  8,  7,  6,  8,  9,  9,  8,  9, 10, 11, 10, 11, 12,
    12, 12, 14, 15, 14, 14
  ]
];

const FFT_LEVEL_EXP_ALT_CODES: &[u16; 28] = &[
    0x1EC6, 0x0006, 0x00C2, 0x0142, 0x0242, 0x0246, 0x00C6, 0x0046,
    0x0042, 0x0146, 0x00A2, 0x0062, 0x0026, 0x0016, 0x000E, 0x0005,
    0x0004, 0x0003, 0x0000, 0x0001, 0x000A, 0x0012, 0x0002, 0x0022,
    0x01C6, 0x02C6, 0x06C6, 0x0EC6
];
const FFT_LEVEL_EXP_ALT_BITS: &[u8; 28] = &[
    13,  7,  8,  9, 10, 10, 10, 10, 10,  9,  8,  7,  6,  5,  4,  3,
     3,  2,  3,  3,  4,  5,  7,  8,  9, 11, 12, 13
];

const FFT_LEVEL_EXP_CODES: &[u16; 20] = &[
    0x0F24, 0x0001, 0x0002, 0x0000, 0x0006, 0x0005, 0x0007, 0x000C,
    0x000B, 0x0014, 0x0013, 0x0004, 0x0003, 0x0023, 0x0064, 0x00A4,
    0x0024, 0x0124, 0x0324, 0x0724
];
const FFT_LEVEL_EXP_BITS: &[u8; 20] = &[
    12, 3, 3, 3, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6, 7, 8, 9, 10, 11, 12
];

const FFT_STEREO_EXP_CODES: &[u8; 7] = &[ 0x3E, 0x01, 0x00, 0x02, 0x06, 0x0E, 0x1E ];
const FFT_STEREO_EXP_BITS: &[u8; 7] = &[ 6, 1, 2, 3, 4, 5, 6 ];

const FFT_STEREO_PHASE_CODES: &[u8; 9] = &[
    0x35, 0x02, 0x00, 0x01, 0x0D, 0x15, 0x05, 0x09, 0x03
];
const FFT_STEREO_PHASE_BITS: &[u8; 9] = &[ 6, 2, 2, 4, 4, 6, 5, 4, 2 ];

const CUTOFF_INDICES: [[i8; 2]; 4] = [ [1, 2], [-1, 0], [-1,-2], [0, 0] ];

const LEVEL_INDEX: [u8; 256] = [
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
];

const FFT_TONE_SAMPLES: [[[f32; 5]; 16]; 4] = [
  [
    [ 0.0100000000,-0.0037037037,-0.0020000000,-0.0069444444,-0.0018416207 ],
    [ 0.0416666667, 0.0000000000, 0.0000000000,-0.0208333333,-0.0123456791 ],
    [ 0.1250000000, 0.0558035709, 0.0330687836,-0.0164473690,-0.0097465888 ],
    [ 0.1562500000, 0.0625000000, 0.0370370370,-0.0062500000,-0.0037037037 ],
    [ 0.1996007860, 0.0781250000, 0.0462962948, 0.0022727272, 0.0013468013 ],
    [ 0.2000000000, 0.0625000000, 0.0370370373, 0.0208333333, 0.0074074073 ],
    [ 0.2127659619, 0.0555555556, 0.0329218097, 0.0208333333, 0.0123456791 ],
    [ 0.2173913121, 0.0473484844, 0.0280583613, 0.0347222239, 0.0205761325 ],
    [ 0.2173913121, 0.0347222239, 0.0205761325, 0.0473484844, 0.0280583613 ],
    [ 0.2127659619, 0.0208333333, 0.0123456791, 0.0555555556, 0.0329218097 ],
    [ 0.2000000000, 0.0208333333, 0.0074074073, 0.0625000000, 0.0370370370 ],
    [ 0.1996007860, 0.0022727272, 0.0013468013, 0.0781250000, 0.0462962948 ],
    [ 0.1562500000,-0.0062500000,-0.0037037037, 0.0625000000, 0.0370370370 ],
    [ 0.1250000000,-0.0164473690,-0.0097465888, 0.0558035709, 0.0330687836 ],
    [ 0.0416666667,-0.0208333333,-0.0123456791, 0.0000000000, 0.0000000000 ],
    [ 0.0100000000,-0.0069444444,-0.0018416207,-0.0037037037,-0.0020000000 ]
  ], [
    [ 0.0050000000,-0.0200000000, 0.0125000000,-0.3030303030, 0.0020000000 ],
    [ 0.1041666642, 0.0400000000,-0.0250000000, 0.0333333333,-0.0200000000 ],
    [ 0.1250000000, 0.0100000000, 0.0142857144,-0.0500000007,-0.0200000000 ],
    [ 0.1562500000,-0.0006250000,-0.00049382716,-0.000625000,-0.00049382716 ],
    [ 0.1562500000,-0.0006250000,-0.00049382716,-0.000625000,-0.00049382716 ],
    [ 0.1250000000,-0.0500000000,-0.0200000000, 0.0100000000, 0.0142857144 ],
    [ 0.1041666667, 0.0333333333,-0.0200000000, 0.0400000000,-0.0250000000 ],
    [ 0.0050000000,-0.3030303030, 0.0020000001,-0.0200000000, 0.0125000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ]
  ], [
    [ 0.1428571492, 0.1250000000,-0.0285714287,-0.0357142873, 0.0208333333 ],
    [ 0.1818181818, 0.0588235296, 0.0333333333, 0.0212765951, 0.0100000000 ],
    [ 0.1818181818, 0.0212765951, 0.0100000000, 0.0588235296, 0.0333333333 ],
    [ 0.1428571492,-0.0357142873, 0.0208333333, 0.1250000000,-0.0285714287 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ]
  ], [
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ],
    [ 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ]
  ]
];

const TONE_ENVELOPE: [[f32; 31]; 4] = [
  [
    0.009607375, 0.038060248, 0.084265202, 0.146446645,
    0.222214907, 0.308658302, 0.402454883, 0.500000060,
    0.597545207, 0.691341758, 0.777785182, 0.853553414,
    0.915734828, 0.961939812, 0.990392685, 1.00000000,
    0.990392625, 0.961939752, 0.915734768, 0.853553295,
    0.777785063, 0.691341639, 0.597545087, 0.500000000,
    0.402454853, 0.308658272, 0.222214878, 0.146446615,
    0.084265172, 0.038060218, 0.009607345
  ], [
    0.038060248, 0.146446645, 0.308658302, 0.500000060,
    0.691341758, 0.853553414, 0.961939812, 1.00000000,
    0.961939752, 0.853553295, 0.691341639, 0.500000000,
    0.308658272, 0.146446615, 0.038060218, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000
  ], [
    0.146446645, 0.500000060, 0.853553414, 1.00000000,
    0.853553295, 0.500000000, 0.146446615, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000
  ], [
    0.500000060, 1.00000000,  0.500000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000, 0.000000000,
    0.000000000, 0.000000000, 0.000000000
  ]
];
