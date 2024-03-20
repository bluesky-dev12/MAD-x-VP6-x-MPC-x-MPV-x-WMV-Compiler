use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use super::qdmcommon::*;
use super::qdm2fft::*;
use super::qdm2qmf::*;

pub const MAX_FRAME_SIZE: usize = 8192;

const SOFTCLIP_THRESHOLD: usize = 27600;
const HARDCLIP_THRESHOLD: usize = 35716;
const SOFTCLIP_SIZE: usize = HARDCLIP_THRESHOLD - SOFTCLIP_THRESHOLD + 1;

#[derive(Clone,Copy)]
struct Packet {
    id:     u8,
    size:   usize,
    offset: usize,
}

impl Packet {
    fn read(br: &mut ByteReader) -> DecoderResult<Self> {
        let id                          = br.read_byte()?;
        let size                        = if (id & 0x80) == 0 { br.read_byte()? as usize } else { br.read_u16be()? as usize };
        validate!(size <= (br.left() as usize));
        Ok(Packet { id: id & 0x7F, size, offset: br.tell() as usize })
    }
}

struct Qdmc2Decoder {
    ainfo:              NAAudioInfo,
    chmap:              NAChannelMap,
    softclip:           [i16; SOFTCLIP_SIZE],
    qmf_part:           QDM2QMF,
    fft_part:           QDM2FFT,
    audio:              [[f32; MAX_FRAME_SIZE]; 2],

    order:              u8,
    frame_bits:         u8,
    samples:            usize,
    frm_bytes:          usize,
    sf_len:             usize,
    channels:           usize,

    subsampling:        u8,
    do_synth:           bool,
}

impl Qdmc2Decoder {
    fn new() -> Self {
        let mut softclip = [0; SOFTCLIP_SIZE];
        let delta = 1.0 / ((32767 - SOFTCLIP_THRESHOLD) as f32);
        let diff = f32::from((SOFTCLIP_THRESHOLD as i16) - 32767);
        for (i, el) in softclip.iter_mut().enumerate() {
            *el = (SOFTCLIP_THRESHOLD as i16) - ((((i as f32) * delta).sin() * diff) as i16);
        }

        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 1),
            chmap:      NAChannelMap::new(),
            audio:      [[0.0; MAX_FRAME_SIZE]; 2],
            softclip,
            qmf_part:   QDM2QMF::new(),
            fft_part:   QDM2FFT::new(),

            order:      0,
            frame_bits: 0,
            samples:    0,
            frm_bytes:  0,
            sf_len:     0,
            channels:   0,

            subsampling:        0,
            do_synth:           false,
        }
    }
}

impl NADecoder for Qdmc2Decoder {
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
                validate!(frame_len == packet_size * 16);
                let bytes_per_frame     = br.read_u32be()? as usize;
                validate!(bytes_per_frame > 6);

                self.order = (31 - (packet_size.leading_zeros() & 31)) as u8;
                validate!(self.order >= 6 && self.order <= 8);
                self.frame_bits = self.order + 4;
                self.samples = frame_len;
                self.frm_bytes = bytes_per_frame;
                self.sf_len = packet_size;
                self.channels = channels;

                let srate = if ainfo.get_sample_rate() != 0 {
                        ainfo.get_sample_rate()
                    } else { srate };
                self.ainfo = NAAudioInfo::new(srate, channels as u8, SND_S16P_FORMAT, 1);
                self.chmap = NAChannelMap::from_str(if channels == 1 { "C" } else { "L,R" }).unwrap();

                self.subsampling = self.order - 6;
                self.fft_part.set_params(channels, self.sf_len, self.subsampling);
                self.qmf_part.set_ch_and_subsampling(channels, self.subsampling, full_bitrate);
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

            let mut mr = MemoryReader::new_read(pktbuf.as_slice());
            let mut br = ByteReader::new(&mut mr);
            let hdr = Packet::read(&mut br)?;

            let (is_intra, has_checksum) = match hdr.id {
                    2 => (true, true),
                    3 => (true, false),
                    4 | 5 => (false, true),
                    6 | 7 => (false, false),
                    _ => return Err(DecoderError::InvalidData),
                };
            if has_checksum {
                let mut csum = u16::from(br.read_byte()?) * 0x101;
                csum = csum.wrapping_add(u16::from(br.read_byte()?) * 2);
                for byte in pktbuf.iter() {
                    csum = csum.wrapping_sub(u16::from(*byte));
                }
                validate!(csum == 0);
            }
            self.fft_part.is_intra = is_intra;
            self.qmf_part.is_intra = is_intra;
            self.qmf_part.new_frame();

            let mut tone_pkt_list = Vec::new();
            let mut fft_pkt_list = Vec::new();
            while br.left() > 1 {
                let hdr                 = Packet::read(&mut br)?;
                if hdr.id == 0 { break; }
                match hdr.id {
                    8 => return Err(DecoderError::NotImplemented),
                    9..=12 => {
                        tone_pkt_list.push(hdr);
                    },
                    13 => {
                        let src = &pktbuf[hdr.offset..][..hdr.size];
                        let mut br = QdmBitReader::new(src);
                        self.fft_part.read_type_13(&mut br)?;
                    },
                    14 => {
                        let src = &pktbuf[hdr.offset..][..hdr.size];
                        let mut br = QdmBitReader::new(src);
                        self.fft_part.read_type_14(&mut br)?;
                    },
                    15 => return Err(DecoderError::NotImplemented),
                    16..=23 | 31..=39 | 46..=47 => {
                        fft_pkt_list.push(hdr);
                    },
                    _ => {},
                };
                                          br.read_skip(hdr.size)?;
            }

            if !tone_pkt_list.is_empty() {
                let mut has_9 = false;
                let mut has_10 = false;
                let mut has_11 = false;
                let mut pkt_12 = Packet { id: 0, size: 0, offset: 0 };
                for hdr in tone_pkt_list.iter() {
                    let src = &pktbuf[hdr.offset..][..hdr.size];
                    let mut br = QdmBitReader::new(src);
                    match hdr.id {
                         9 => { self.qmf_part.read_type_9(&mut br)?;  has_9 = true; },
                        10 => { self.qmf_part.read_type_10(&mut br)?; has_10 = true; },
                        11 => { self.qmf_part.read_type_11(&mut br)?; has_11 = true; },
                        12 => { pkt_12 = *hdr; },
                        _ => unreachable!(),
                    };
                }
                if !has_10 {
                    self.qmf_part.fill_default(10);
                }
                if !has_11 {
                    self.qmf_part.fill_default(11);
                }
                if pkt_12.id == 12 && has_9 && has_10 {
                    let src = &pktbuf[pkt_12.offset..][..pkt_12.size];
                    let mut br = QdmBitReader::new(src);
                    self.qmf_part.read_type_12(&mut br)?;
                } else {
                    self.qmf_part.fill_default(12);
                }
                self.do_synth = true;
            }

            if tone_pkt_list.is_empty() && self.do_synth {
                self.qmf_part.fill_default(10);
                self.qmf_part.fill_default(11);
                self.qmf_part.fill_default(12);
            }

            let channels = self.chmap.num_channels();
            let abuf = alloc_audio_buffer(self.ainfo, self.samples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let off = [adata.get_offset(0), adata.get_offset(1)];
            let dst = adata.get_data_mut().unwrap();

            self.audio = [[0.0; MAX_FRAME_SIZE]; 2];
            for subframe in 0..16 {
                if subframe == 2 {
                    self.fft_part.new_frame();
                    for hdr in fft_pkt_list.iter() {
                        let src = &pktbuf[hdr.offset..][..hdr.size];
                        let mut br = QdmBitReader::new(src);
                        self.fft_part.read_fft_packet(hdr.id, &mut br)?;
                    }
                }
                self.fft_part.generate_tones(subframe);
                for ch in 0..channels {
                    let output = &mut self.audio[ch][subframe * self.sf_len..][..self.sf_len];
                    self.fft_part.synth(output, ch);
                    if self.do_synth {
                        self.qmf_part.synth(output, subframe, ch);
                    }
                }
            }
            let frame_len = self.sf_len * 16;
            for ch in 0..channels {
                for (src, dst) in self.audio[ch].iter().take(frame_len).zip(dst[off[ch]..].iter_mut()) {
                    let samp = *src as i32;
                    if samp > (HARDCLIP_THRESHOLD as i32) {
                        *dst = 0x7FFF;
                    } else if samp > (SOFTCLIP_THRESHOLD as i32) {
                        *dst = self.softclip[(samp as usize) - SOFTCLIP_THRESHOLD];
                    } else if samp > -(SOFTCLIP_THRESHOLD as i32) {
                        *dst = samp as i16;
                    } else if samp > -(HARDCLIP_THRESHOLD as i32) {
                        *dst = -self.softclip[(-samp as usize) - SOFTCLIP_THRESHOLD];
                    } else {
                        *dst = -0x8000;
                    }
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
        self.qmf_part.flush();
        self.fft_part.flush();
    }
}

impl NAOptionHandler for Qdmc2Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(Qdmc2Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_qdm2() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        //test_decode_audio("mov", "assets/QT/0-22050HzSweep8kb.mov", None, Some("qdm2"), &dmx_reg, &dec_reg);
        // sample: https://samples.mplayerhq.hu/A-codecs/QDM2/sweep/0-22050HzSweep10kb.mov
        test_decoding("mov", "qdesign-music2", "assets/QT/0-22050HzSweep10kb.mov", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::Decodes);
    }
}

pub const TONE_SCALES: [[f32; 64]; 2] = [
  [
    0.17677669, 0.42677650, 0.60355347, 0.85355347,
    1.20710683, 1.68359375, 2.37500000, 3.36718750,
    4.75000000, 6.73437500, 9.50000000, 13.4687500,
    19.0000000, 26.9375000, 38.0000000, 53.8750000,
    76.0000000, 107.750000, 152.000000, 215.500000,
    304.000000, 431.000000, 608.000000, 862.000000,
    1216.00000, 1724.00000, 2432.00000, 3448.00000,
    4864.00000, 6896.00000, 9728.00000, 13792.0000,
    19456.0000, 27584.0000, 38912.0000, 55168.0000,
    77824.0000, 110336.000, 155648.000, 220672.000,
    311296.000, 441344.000, 622592.000, 882688.000,
    1245184.00, 1765376.00, 2490368.00, 0.00000000,
    0.00000000, 0.00000000, 0.00000000, 0.00000000,
    0.00000000, 0.00000000, 0.00000000, 0.00000000,
    0.00000000, 0.00000000, 0.00000000, 0.00000000,
    0.00000000, 0.00000000, 0.00000000, 0.00000000
  ], [
    0.59375000, 0.84179688, 1.18750000, 1.68359375,
    2.37500000, 3.36718750, 4.75000000, 6.73437500,
    9.50000000, 13.4687500, 19.0000000, 26.9375000,
    38.0000000, 53.8750000, 76.0000000, 107.750000,
    152.000000, 215.500000, 304.000000, 431.000000,
    608.000000, 862.000000, 1216.00000, 1724.00000,
    2432.00000, 3448.00000, 4864.00000, 6896.00000,
    9728.00000, 13792.0000, 19456.0000, 27584.0000,
    38912.0000, 55168.0000, 77824.0000, 110336.000,
    155648.000, 220672.000, 311296.000, 441344.000,
    622592.000, 882688.000, 1245184.00, 1765376.00,
    2490368.00, 3530752.00, 0.00000000, 0.00000000,
    0.00000000, 0.00000000, 0.00000000, 0.00000000,
    0.00000000, 0.00000000, 0.00000000, 0.00000000,
    0.00000000, 0.00000000, 0.00000000, 0.00000000,
    0.00000000, 0.00000000, 0.00000000, 0.00000000
  ]
];
