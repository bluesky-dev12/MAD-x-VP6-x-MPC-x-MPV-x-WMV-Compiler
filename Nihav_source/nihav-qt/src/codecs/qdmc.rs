use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_codec_support::dsp::fft::*;
use super::qdmcommon::*;

const MAX_NOISE_BANDS: usize = 19;
const MAX_FRAME_SIZE: usize = 8192;

struct QdmcDecoder {
    ainfo:          NAAudioInfo,
    chmap:          NAChannelMap,
    noise_val_cb:   Codebook<u8>,
    noise_seg_cb:   Codebook<u8>,
    amp_cb:         Codebook<u8>,
    amp_diff_cb:    Codebook<u8>,
    freq_diff_cb:   Codebook<u8>,
    phase_diff_cb:  Codebook<u8>,
    sin_tab:        [f32; 512],
    tone_tab:       [[f32; 32]; 5],
    rng:            RNG,
    fft:            FFT,
    fft_buf:        [[FFTComplex; MAX_FRAME_SIZE * 2]; 2],
    noise_tab:      [[f32; 256]; MAX_NOISE_BANDS],
    tmp:            [f32; MAX_FRAME_SIZE],
    sbuf:           [FFTComplex; 512],
    delay:          [[f32; 512]; 2],

    noise:          [[[u8; 16]; MAX_NOISE_BANDS]; 2],
    tones:          [Vec<Tone>; 5],

    order:          u8,
    frame_bits:     u8,
    samples:        usize,
    frm_bytes:      usize,
    noise_cat:      usize,
    sf_len:         usize,
}

fn def_cb_map(idx: usize) -> u8 { idx as u8 }
fn noise_val_cb_map(idx: usize) -> u8 { NOISE_VAL_SYMS[idx] }
fn noise_seg_cb_map(idx: usize) -> u8 { NOISE_SEG_SYMS[idx] }

impl QdmcDecoder {
    fn new() -> Self {
        let mut cbr = TableCodebookDescReader::new(&NOISE_VAL_CODES, &NOISE_VAL_BITS, noise_val_cb_map);
        let noise_val_cb = Codebook::new(&mut cbr, CodebookMode::LSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(&NOISE_SEG_CODES, &NOISE_SEG_BITS, noise_seg_cb_map);
        let noise_seg_cb = Codebook::new(&mut cbr, CodebookMode::LSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(&FREQ_DIFF_CODES, &FREQ_DIFF_BITS, def_cb_map);
        let freq_diff_cb = Codebook::new(&mut cbr, CodebookMode::LSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(&AMP_CODES, &AMP_BITS, def_cb_map);
        let amp_cb = Codebook::new(&mut cbr, CodebookMode::LSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(&AMP_DIFF_CODES, &AMP_DIFF_BITS, def_cb_map);
        let amp_diff_cb = Codebook::new(&mut cbr, CodebookMode::LSB).unwrap();
        let mut cbr = TableCodebookDescReader::new(&PHASE_DIFF_CODES, &PHASE_DIFF_BITS, def_cb_map);
        let phase_diff_cb = Codebook::new(&mut cbr, CodebookMode::LSB).unwrap();

        let mut sin_tab = [0.0f32; 512];
        for (i, tab) in sin_tab.iter_mut().enumerate() {
            *tab = (2.0 * (i as f32) * std::f32::consts::PI / 512.0).sin();
        }

        let mut tone_tab = [[0.0; 32]; 5];
        for group in 0..5 {
            for i in 0..(1 << (5 - group)) {
                tone_tab[group][i] = sin_tab[((i + 1) << (group + 3)) & 0x1FF];
            }
        }

        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_F32P_FORMAT, 1),
            chmap:      NAChannelMap::new(),
            noise_val_cb, noise_seg_cb, freq_diff_cb, amp_cb, amp_diff_cb, phase_diff_cb,
            sin_tab, tone_tab,
            noise:      [[[0; 16]; MAX_NOISE_BANDS]; 2],
            tones:      [Vec::with_capacity(MAX_TONES),
                         Vec::with_capacity(MAX_TONES),
                         Vec::with_capacity(MAX_TONES),
                         Vec::with_capacity(MAX_TONES),
                         Vec::with_capacity(MAX_TONES)],
            rng:        RNG::new(),
            fft:        FFTBuilder::new_fft(256, false),
            fft_buf:    [[FFTC_ZERO; MAX_FRAME_SIZE * 2]; 2],
            noise_tab:  [[0.0; 256]; MAX_NOISE_BANDS],
            tmp:        [0.0; MAX_FRAME_SIZE],
            sbuf:       [FFTC_ZERO; 512],
            delay:      [[0.0; 512]; 2],

            order:      0,
            frame_bits: 0,
            samples:    0,
            frm_bytes:  0,
            noise_cat:  0,
            sf_len:     0,
        }
    }
    fn fill_noise_table(&mut self) {
        let noise_bands = NOISE_SUBBANDS[self.noise_cat];
        self.noise_tab = [[0.0; 256]; MAX_NOISE_BANDS];
        for band in 0..(noise_bands.len() - 2) {
            let prev = noise_bands[band];
            let cur  = noise_bands[band + 1];
            let next = noise_bands[band + 2];

            let noise = &mut self.noise_tab[band];
            for i in prev..cur {
                noise[i] = ((i - prev) as f32) / ((cur - prev) as f32);
            }
            for i in cur..next {
                noise[i] = ((next - i) as f32) / ((next - cur) as f32);
            }
        }
    }
    fn read_noise_data(&mut self, br: &mut BitReader, ch: usize) -> DecoderResult<()> {
        let noise_bands = NOISE_SUBBANDS[self.noise_cat];
        for band in 0..(noise_bands.len() - 2) {
            let val                     = br.read_code(&self.noise_val_cb)? as i32;
            let mut last = to_signed(val);
            validate!(last >= 0);
            self.noise[ch][band][0] = last as u8;
            let mut idx = 1;
            while idx < 16 {
                let len                 = (br.read_code_long(&self.noise_seg_cb)? as usize) + 1;
                let val                 = br.read_code(&self.noise_val_cb)? as i32;
                let val = to_signed(val) + last;
                validate!(val >= 0);
                validate!(len + idx <= 16);
                for i in 1..=len {
                    self.noise[ch][band][idx] = (last + (i as i32) * (val - last) / (len as i32) - 1) as u8;
                    idx += 1;
                }
                last = val;
            }
        }
        Ok(())
    }
    fn read_wave_data(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        for tone in self.tones.iter_mut() {
            tone.clear();
        }
        for group in 0..5 {
            let group_size = 1 << (self.frame_bits - group - 1);
            let group_bits = 4 - group;
            let mut freq = 1;
            let mut off = 0;
            let mut pos2 = 0;
            while freq < group_size {
                let diff                = br.read_code_long(&self.freq_diff_cb)?;
                freq += diff as usize;
                while freq >= group_size - 1 {
                    freq -= group_size - 2;
                    off  += 1 << group_bits;
                    pos2 += group_size;
                }
                if pos2 >= (1 << self.frame_bits) {
                    break;
                }
                let stereo_mode = if self.chmap.num_channels() > 1 { br.read(2)? as u8 } else { 0 };
                let amp                 = br.read_code(&self.amp_cb)? as u8;
                let phase               = br.read(3)? as u8;
                let (amp2, phase2) = if (stereo_mode & 2) != 0 {
                                         (br.read_code(&self.amp_diff_cb)? as u8,
                                          br.read_code(&self.phase_diff_cb)? as u8)
                    } else { (0, 0) };
                if (freq >> group_bits) + 1 < self.sf_len {
                    validate!(self.tones[group as usize].len() < MAX_TONES);
                    self.tones[group as usize].push(Tone {
                            offset: off, ch: stereo_mode & 1, phase,
                            freq: freq as u16, amp_idx: amp
                        });
                    if (stereo_mode & 2) != 0 {
                        validate!(self.tones[group as usize].len() < MAX_TONES);
                        let phase = phase.wrapping_sub(phase2) & 7;
                        let amp_idx = amp.wrapping_sub(amp2) & 0x3F;
                        self.tones[group as usize].push(Tone {
                                offset: off, ch: !stereo_mode & 1, phase,
                                freq: freq as u16, amp_idx
                            });
                    }
                }
                freq += 1;
            }
        }
        Ok(())
    }
    fn add_noise(&mut self, ch: usize, sf: usize) {
        let noise_bands = NOISE_SUBBANDS[self.noise_cat];
        self.tmp = [0.0; MAX_FRAME_SIZE];
        for band in 0..(noise_bands.len() - 2) {
            if noise_bands[band] >= self.sf_len {
                break;
            }
            let scale = SCALES[(self.noise[ch][band][sf >> 1] & 0x3F) as usize] / 32768.0;
            let start = noise_bands[band];
            let end = noise_bands[band + 2].min(self.sf_len);
            let linscale = &self.noise_tab[band];
            for i in start..end {
                self.tmp[i] += scale * linscale[i];
            }
        }

        for i in 2..self.sf_len - 1 {
            let im = -self.rng.next_float() * self.tmp[i];
            let re =  self.rng.next_float() * self.tmp[i];
            let noise = FFTComplex { re, im };
            self.fft_buf[ch][sf * self.sf_len + i]     += noise;
            self.fft_buf[ch][sf * self.sf_len + i + 1] -= noise;
        }
    }
    fn add_tones(&mut self, sf: usize, start_idx: &mut [usize; 5]) {
        for group in 0..5 {
            let group_bits = 4 - group;
            let group_size = (1 << (group_bits + 1)) - 1;
            for tone in self.tones[group].iter().skip(start_idx[group]) {
                if (tone.offset as usize) > sf {
                    break;
                }
                start_idx[group] += 1;

                let pos = (tone.freq >> group_bits) as usize;
                let scale = SCALES[(tone.amp_idx & 0x3F) as usize] / 32768.0;
                let mut phase_idx = ((tone.phase as usize) * 64).wrapping_sub((2 * pos + 1) * 128) & 0x1FF;
                for i in 0..group_size {
                    phase_idx = phase_idx.wrapping_add((2 * (tone.freq as usize) + 1) << (7 - group_bits));
                    let factor = scale * self.tone_tab[group][i];
                    let re =  factor * self.sin_tab[(phase_idx + 128) & 0x1FF];
                    let im = -factor * self.sin_tab[ phase_idx        & 0x1FF];
                    let val = FFTComplex { re, im };
                    let ch = tone.ch as usize;
                    self.fft_buf[ch][(sf + i) * self.sf_len + pos]     += val;
                    self.fft_buf[ch][(sf + i) * self.sf_len + pos + 1] -= val;
                }
            }
        }
    }
    fn synth_channel(&mut self, ch: usize, subframe: usize, dst: &mut [f32]) {
        let sf_len = self.sf_len;
        self.sbuf = [FFTC_ZERO; 512];
        self.sbuf[..sf_len].copy_from_slice(&self.fft_buf[ch][subframe * sf_len..][..sf_len]);
        self.fft.do_fft_inplace(&mut self.sbuf);
        dst[..sf_len].copy_from_slice(&self.delay[ch][..sf_len]);
        for (dst, src) in dst.iter_mut().take(sf_len).zip(self.sbuf.iter()) {
            *dst += src.re;
        }
        for (dst, src) in self.delay[ch].iter_mut().take(sf_len).zip(self.sbuf.iter().skip(sf_len)) {
            *dst = src.re;
        }
    }
}

impl NADecoder for QdmcDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            if let Some(edata) = info.get_extradata() {
                validate!(edata.len() >= 36);
                let mut mr = MemoryReader::new_read(edata.as_slice());
                let mut br = ByteReader::new(&mut mr);
                let size                = br.read_u32be()? as usize;
                validate!(size >= 36 && size <= edata.len());
                let tag                 = br.read_tag()?;
                validate!(&tag == b"QDCA");
                let ver                 = br.read_u32be()?;
                validate!(ver == 1);
                let channels            = br.read_u32be()? as usize;
                validate!(channels == 2 || channels == 1);
                let srate               = br.read_u32be()?;
                let full_bitrate        = br.read_u32be()?;
                let frame_len           = br.read_u32be()? as usize;
                let packet_size         = br.read_u32be()? as usize;
                validate!(packet_size > 0 && (packet_size & (packet_size - 1)) == 0);
                validate!(frame_len == packet_size * 32);
                let bytes_per_frame     = br.read_u32be()? as usize;
                validate!(bytes_per_frame > 6);

                self.order = (31 - (packet_size.leading_zeros() & 31)) as u8;
                self.frame_bits = self.order + 5;
                self.samples = frame_len;
                self.frm_bytes = bytes_per_frame;
                self.sf_len = packet_size;

                let srate = if ainfo.get_sample_rate() != 0 {
                        ainfo.get_sample_rate()
                    } else { srate };
                self.ainfo = NAAudioInfo::new(srate, channels as u8, SND_F32P_FORMAT, 1);
                self.chmap = NAChannelMap::from_str(if channels == 1 { "C" } else { "L,R" }).unwrap();
                let (mut bitrate, fbits) = if srate >= 32000 {
                        (28000, 13)
                    } else if srate >= 16000 {
                        (20000, 12)
                    } else {
                        (16000, 11)
                    };
                if channels == 2 {
                    bitrate += bitrate / 2;
                }
                let idx = ((full_bitrate * 3 + bitrate / 2) / bitrate) as usize;
                self.noise_cat = NOISE_BAND_SELECTOR[idx.min(NOISE_BAND_SELECTOR.len() - 1)];
                validate!(frame_len == (1 << fbits));

                self.fft_buf = [[FFTC_ZERO; MAX_FRAME_SIZE * 2]; 2];
                self.fft = FFTBuilder::new_fft(packet_size * 2, false);

                self.fill_noise_table();
            } else {
                return Err(DecoderError::InvalidData);
            }

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            validate!(pktbuf.len() == self.frm_bytes);
            validate!(&pktbuf[..3] == b"QMC" && pktbuf[3] == 1);
            let checksum = u16::from(pktbuf[4]) + u16::from(pktbuf[5]) * 256;
            let mut sum = 0xE2u16;
            for el in pktbuf.iter().skip(6) {
                sum = sum.wrapping_add(u16::from(*el));
            }
            validate!(sum == checksum);

            let channels = self.chmap.num_channels();
            let abuf = alloc_audio_buffer(self.ainfo, self.samples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_f32().unwrap();
            let mut off = [adata.get_offset(0), adata.get_offset(1)];
            let dst = adata.get_data_mut().unwrap();

            let mut br = BitReader::new(&pktbuf[6..], BitReaderMode::LE);
            for ch in 0..channels {
                self.read_noise_data(&mut br, ch)?;
            }
            self.read_wave_data(&mut br)?;

            let mut tone_start = [0; 5];
            for subframe in 0..32 {
                for ch in 0..channels {
                    self.add_noise(ch, subframe);
                }
                self.add_tones(subframe, &mut tone_start);
                for ch in 0..channels {
                    self.synth_channel(ch, subframe, &mut dst[off[ch]..]);
                    off[ch] += self.sf_len;
                }
            }
            for ch in 0..channels {
                let mut chunks = self.fft_buf[ch].chunks_mut(1 << self.frame_bits);
                let first = chunks.next().unwrap();
                let second = chunks.next().unwrap();
                first.copy_from_slice(second);
                for el in second.iter_mut() {
                    *el = FFTC_ZERO;
                }
            }

            let mut frm = NAFrame::new_from_pkt(pkt, info.replace_info(NACodecTypeInfo::Audio(self.ainfo)), abuf);
            frm.set_duration(Some(self.samples as u64));
            frm.set_keyframe(false);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
        self.fft_buf = [[FFTC_ZERO; MAX_FRAME_SIZE * 2]; 2];
        self.delay = [[0.0; 512]; 2];
    }
}

impl NAOptionHandler for QdmcDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(QdmcDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_qdmc() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/QDMC/rumcoke.mov
        test_decoding("mov", "qdesign-music", "assets/QT/rumcoke.mov", Some(32), &dmx_reg, &dec_reg,
                      ExpectedTestResult::Decodes);
    }
}

const NOISE_VAL_BITS: [u8; 27] = [
    12,  2,  3,  2,  3,  3,  5,  5,
     6,  7,  7,  9,  7, 10,  9, 11,
     9,  9,  9,  9,  9,  9,  9,  9,
    10, 10, 12
];
const NOISE_VAL_CODES: [u16; 27] = [
    0xC7A, 0x000, 0x001, 0x003, 0x005, 0x006, 0x012, 0x00A,
    0x022, 0x01A, 0x002, 0x0FA, 0x03A, 0x35A, 0x1C2, 0x07A,
    0x1FA, 0x17A, 0x0DA, 0x142, 0x0C2, 0x042, 0x1DA, 0x05A,
    0x15A, 0x27A, 0x47A
];
const NOISE_VAL_SYMS: [u8; 27] = [
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36
];

const NOISE_SEG_BITS: [u8; 12] = [ 10, 1, 2, 4, 4, 4, 6, 7, 9, 10, 8, 5 ];
const NOISE_SEG_CODES: [u16; 12] = [
    0x30B, 0x000, 0x001, 0x003, 0x007, 0x00F, 0x02B, 0x04B,
    0x00B, 0x10B, 0x08B, 0x01B
];
const NOISE_SEG_SYMS: [u8; 12] = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 17 ];

const FREQ_DIFF_BITS: [u8; 47] = [
    18,  2,  4,  4,  5,  4,  4,  5,  5,  4,  5,  5,  5,  5,  6,  6,
     6,  6,  6,  7,  7,  6,  7,  6,  6,  6,  7,  7,  7,  7,  7,  8,
     9,  9,  8,  9, 11, 11, 12, 12, 13, 12, 14, 15, 18, 16, 17
];
const FREQ_DIFF_CODES: [u32; 47] = [
    0x2AD46, 0x00001, 0x00000, 0x00003, 0x0000C, 0x0000A, 0x00007, 0x00018,
    0x00012, 0x0000E, 0x00004, 0x00016, 0x0000F, 0x0001C, 0x00008, 0x00022,
    0x00026, 0x00002, 0x0003B, 0x00034, 0x00074, 0x0001F, 0x00014, 0x0002B,
    0x0001B, 0x0003F, 0x00028, 0x00054, 0x00006, 0x0004B, 0x0000B, 0x00068,
    0x000E8, 0x00046, 0x000C6, 0x001E8, 0x00146, 0x00346, 0x00546, 0x00746,
    0x01D46, 0x00F46, 0x00D46, 0x06D46, 0x0AD46, 0x02D46, 0x1AD46
];

const AMP_BITS: [u8; 28] = [
    13,  7,  8,  9, 10, 10, 10, 10, 10,  9,  8,  7,  6,  5,  4,  3,
     3,  2,  3,  3,  4,  5,  7,  8,  9, 11, 12, 13
];
const AMP_CODES: [u16; 28] = [
    0x1EC6, 0x0006, 0x00C2, 0x0142, 0x0242, 0x0246, 0x00C6, 0x0046,
    0x0042, 0x0146, 0x00A2, 0x0062, 0x0026, 0x0016, 0x000E, 0x0005,
    0x0004, 0x0003, 0x0000, 0x0001, 0x000A, 0x0012, 0x0002, 0x0022,
    0x01C6, 0x02C6, 0x06C6, 0x0EC6
];

const AMP_DIFF_BITS: [u8; 9] = [ 8, 2, 1, 3, 4, 5, 6, 7, 8 ];
const AMP_DIFF_CODES: [u8; 9] = [
    0xFE, 0x00, 0x01, 0x02, 0x06, 0x0E, 0x1E, 0x3E, 0x7E
];

const PHASE_DIFF_BITS: [u8; 9] = [ 6, 2, 2, 4, 4, 6, 5, 4, 2 ];
const PHASE_DIFF_CODES: [u8; 9] = [
    0x35, 0x02, 0x00, 0x01, 0x0D, 0x15, 0x05, 0x09, 0x03
];

const NOISE_BAND_SELECTOR: [usize; 5] = [ 4, 3, 2, 1, 0 ];
const NOISE_SUBBANDS: [&[usize]; 5] = [
    &[ 0, 1, 2, 4, 6, 8, 12, 16, 24, 32, 48, 56, 64, 80, 96, 120, 144, 176, 208, 240, 256 ],
    &[ 0, 2, 4, 8, 16, 24, 32, 48, 56, 64, 80, 104, 128, 160, 208, 256 ],
    &[ 0, 2, 4, 8, 16, 32, 48, 64, 80, 112, 160, 208, 256 ],
    &[ 0, 4, 8, 16, 32, 48, 64, 96, 144, 208, 256 ],
    &[ 0, 4, 16, 32, 64, 256 ]
];

const SCALES: [f32; 64] = [
    1.1875, 1.6835938, 2.375, 3.3671875, 4.75, 6.734375, 9.5, 13.46875,
    19.0, 26.9375, 38.0, 53.875, 76.0, 107.75, 152.0, 215.5,
    304.0, 431.0, 608.0, 862.0, 1216.0, 1724.0, 2432.0, 3448.0,
    4864.0, 6896.0, 9728.0, 13792.0, 19456.0, 27584.0, 38912.0, 55168.0,
    77824.0, 110336.0, 155648.0, 220672.0, 311296.0, 441344.0, 622592.0, 882688.0,
    1245184.0, 1765376.0, 2490368.0, 3530752.0, 4980736.0, 7061504.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
];
