use nihav_core::codecs::*;
use nihav_core::io::byteio::read_u32be;
use nihav_core::io::bitreader::*;
use nihav_codec_support::dsp::qmf::QMF;

mod mp2data;
mod mp2code;
use mp2code::*;
mod mp3data;
mod mp3code;
use mp3code::*;

const SAMPLES: usize = 1152;
const BYTEBUF_SIZE: usize = 2048;

#[allow(clippy::large_enum_variant)]
enum LayerData {
    MP1,
    MP2(MP2Data),
    MP3(MP3Data),
}

impl LayerData {
    fn layer_id(&self) -> u8 {
        match *self {
            LayerData::MP1    => 0,
            LayerData::MP2(_) => 1,
            LayerData::MP3(_) => 2,
        }
    }
    fn reset(&mut self) {
        match self {
            LayerData::MP1 => {},
            LayerData::MP2(ref mut data) => data.reset(),
            LayerData::MP3(ref mut data) => data.reset(),
        };
    }
}

struct MPADecoder {
    info:       NACodecInfoRef,
    smap:       NAChannelMap,
    mmap:       NAChannelMap,
    qmf:        [QMF; 2],
    srate:      u32,
    channels:   u8,
    sf_idx:     usize,

    bytebuf:    Vec<u8>,
    coeffs:     [[f32; SAMPLES]; 2],
    out:        [[[f32; 32]; 36]; 2],
    ctx:        LayerData,
}

impl MPADecoder {
    fn new(layer: u8) -> Self {
        let ctx = match layer {
                0 => LayerData::MP1,
                1 => LayerData::MP2(MP2Data::new()),
                2 => LayerData::MP3(MP3Data::new()),
                _ => unreachable!(),
            };
        Self {
            info:       NACodecInfo::new_dummy(),
            smap:       NAChannelMap::from_ms_mapping(3),
            mmap:       NAChannelMap::from_ms_mapping(4),
            qmf:        [QMF::new(), QMF::new()],
            srate:      0,
            channels:   0,
            sf_idx:     0,
            ctx,

            bytebuf:    Vec::with_capacity(BYTEBUF_SIZE),
            coeffs:     [[0.0; SAMPLES]; 2],
            out:        [[[0.0; 32]; 36]; 2],
        }
    }
    fn read_mp3_side_data(&mut self, br: &mut BitReader, src: &[u8], mono: bool) -> DecoderResult<()> {
        if let LayerData::MP3(ref mut ctx) = self.ctx {
            let channels = if mono { 1 } else { 2 };
            ctx.mpeg1 = self.sf_idx < 3;
            ctx.sf_idx = self.sf_idx;
            ctx.read_mp3_side_data(br, channels)?;
            let hdr_size = br.tell() / 8;
            let add_len = src.len() - hdr_size;
            if self.bytebuf.len() + add_len > BYTEBUF_SIZE {
                self.bytebuf.drain(..self.bytebuf.len() + add_len - BYTEBUF_SIZE);
            }
            let underrun = self.bytebuf.len() < ctx.main_data_end;
            let del_len = if !underrun { self.bytebuf.len() - ctx.main_data_end } else { 0 };
            self.bytebuf.extend_from_slice(&src[hdr_size..]);
            self.bytebuf.drain(..del_len);
            if underrun {
                return Err(DecoderError::MissingReference);
            }

            Ok(())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn decode_layer3(&mut self, channels: usize, mode_ext: u8) -> DecoderResult<bool> {
        if let LayerData::MP3(ref mut ctx) = self.ctx {
            let mut br = BitReader::new(&self.bytebuf, BitReaderMode::BE);
            if ctx.mpeg1 {
                ctx.decode_mpeg1_layer3(&mut br, &mut self.coeffs, channels)?;
            } else {
                ctx.decode_mpeg2_layer3(&mut br, &mut self.coeffs, channels, mode_ext)?;
            }
            let used_data = (br.tell() + 7) / 8;
            self.bytebuf.drain(..used_data);

            Ok(true)
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn synth_layer3(&mut self, mode: u8, mode_ext: u8) {
        if let LayerData::MP3(ref mut ctx) = self.ctx {
            ctx.synth(&mut self.coeffs, &mut self.out, mode, mode_ext);
        }
    }
}

fn apply_ms(ch0: &mut [f32], ch1: &mut [f32]) {
    for (l, r) in ch0.iter_mut().zip(ch1) {
        let ll = (*l + *r) * std::f32::consts::FRAC_1_SQRT_2;
        let rr = (*l - *r) * std::f32::consts::FRAC_1_SQRT_2;
        *l = ll;
        *r = rr;
    }
}

impl NADecoder for MPADecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(_ainfo) = info.get_properties() {
            self.info = info.clone();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let src = pkt.get_buffer();

            let mut br = BitReader::new(src.as_slice(), BitReaderMode::BE);

            let syncword                = br.read(11)?;
            validate!(syncword == 0x7FF);
            let id                      = br.read(2)?;
            validate!(id != 1);
            let layer                   = (br.read(2)? ^ 3) as u8;
            validate!(layer != 3);
            let protection              = br.read_bool()?;
            let bitrate_index           = br.read(4)? as usize;
            validate!(bitrate_index < 15);
            if bitrate_index == 0 {
                //todo freeform eventually
                unimplemented!();
            }
            let mut sf_idx              = br.read(2)? as usize;
            validate!(sf_idx != 3);
            let padding                 = br.read_bool()?;
            let _private                = br.read_bool()?;
            let mode                    = br.read(2)? as u8;
            let mode_extension          = br.read(2)? as u8;
            let _copyright              = br.read_bool()?;
            let _original               = br.read_bool()?;
            let _emphasis               = br.read(2)?;
            if !protection {
                let _crc_check          = br.read(16)?;
            }
            validate!(layer == self.ctx.layer_id());
            match id {
                0 => sf_idx += 6,
                2 => sf_idx += 3,
                _ => {},
            };
            let mpeg1 = id == 3;
            let srate = SAMPLING_RATE[sf_idx];
            if self.srate == 0 {
                self.srate = srate;
            }
            validate!(srate == self.srate);
            let channels = if mode == 3 { 1 } else { 2 };
            if self.channels == 0 {
                self.channels = channels;
            }
            if channels != self.channels {
                self.flush();
            }
            let bitrate = BITRATE[if mpeg1 { 0 } else { 1 }][layer as usize][bitrate_index];
            let frame_size = match layer {
                    0 => {
                        ((SAMPLES / 3 / 8 * 1000 * (bitrate as usize) / (srate as usize)) & !3) + if padding { 4 } else { 0 }
                    },
                    2 if !mpeg1 => {
                        SAMPLES / 2 / 8 * 1000 * (bitrate as usize) / (srate as usize) + if padding { 1 } else { 0 }
                    },
                    _ => {
                        SAMPLES / 8 * 1000 * (bitrate as usize) / (srate as usize) + if padding { 1 } else { 0 }
                    },
                };
            validate!(src.len() >= frame_size);
            self.sf_idx = sf_idx;

            let nsamples = if mpeg1 { SAMPLES } else { SAMPLES / 2 };

            let ainfo = NAAudioInfo::new(srate, channels, SND_F32P_FORMAT, nsamples);
            let chmap = if channels == 1 { self.mmap.clone() } else { self.smap.clone() };

            let mut abuf = alloc_audio_buffer(ainfo, nsamples, chmap)?;
            let mut adata = abuf.get_abuf_f32().unwrap();
            let off = if channels == 1 { adata.get_length() } else { adata.get_stride() };
            let buf = adata.get_data_mut().unwrap();
            let (ch0, ch1) = buf.split_at_mut(off);

            match layer {
                0 => unimplemented!(),
                1 => {
                    if let LayerData::MP2(ref mut ctx) = self.ctx {
                        ctx.mpeg1 = self.sf_idx < 3;
                        ctx.sf_idx = self.sf_idx;
                        ctx.br_idx = bitrate_index;
                        ctx.read_layer2(&mut br, channels as usize, &mut self.out, mode, mode_extension)?;
                    } else {
                        return Err(DecoderError::Bug);
                    }
                    for (dst, src) in ch0.chunks_exact_mut(32).zip(self.out[0].iter_mut()) {
                        self.qmf[0].synth(src, dst);
                    }
                    if channels == 2 {
                        for (dst, src) in ch1.chunks_mut(32).zip(self.out[1].iter_mut()) {
                            self.qmf[1].synth(src, dst);
                        }
                    }
                },
                _ => {
                    let ret = self.read_mp3_side_data(&mut br, &src[..frame_size], channels == 1);
                    match ret {
                        Err(DecoderError::MissingReference) => {
                            abuf.truncate_audio(0);
                            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
                            frm.set_duration(Some(0));
                            return Ok(frm.into_ref());
                        },
                        Err(err) => return Err(err),
                        Ok(()) => {},
                    };
                    let has_data = self.decode_layer3(channels as usize, mode_extension)?;
                    if !has_data {
                        let frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::None);
                        return Ok(frm.into_ref());
                    }
                    self.synth_layer3(mode, mode_extension);
                    for (dst, src) in ch0.chunks_exact_mut(32).zip(self.out[0].iter_mut()) {
                        self.qmf[0].synth(src, dst);
                    }
                    if channels == 2 {
                        for (dst, src) in ch1.chunks_mut(32).zip(self.out[1].iter_mut()) {
                            self.qmf[1].synth(src, dst);
                        }
                    }
                },
            };

            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
            frm.set_duration(Some(nsamples as u64));
            frm.set_keyframe(true);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::Bug)
        }
    }
    fn flush(&mut self) {
        for qmf in self.qmf.iter_mut() {
            *qmf = QMF::new();
        }
        self.bytebuf.clear();
        self.ctx.reset();
    }
}

impl NAOptionHandler for MPADecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_mp2() -> Box<dyn NADecoder + Send> {
    Box::new(MPADecoder::new(1))
}

pub fn get_decoder_mp3() -> Box<dyn NADecoder + Send> {
    Box::new(MPADecoder::new(2))
}

#[derive(Clone,Copy,Debug)]
struct MPAHeader {
    layer:      u8,
    srate:      u32,
    channels:   u8,
    frame_size: usize,
    nsamples:   usize,
}

impl PartialEq for MPAHeader {
    fn eq(&self, other: &Self) -> bool {
        self.layer == other.layer &&
        self.srate == other.srate &&
        self.channels == other.channels
    }
}

#[derive(Default)]
struct MPAPacketiser {
    buf:        Vec<u8>,
    hdr:        Option<MPAHeader>,
}

impl MPAPacketiser {
    fn new() -> Self { Self::default() }
    fn parse_header(&self, off: usize) -> DecoderResult<MPAHeader> {
        if self.buf.len() < off + 4 { return Err(DecoderError::ShortData); }

        let mut br = BitReader::new(&self.buf[off..], BitReaderMode::BE);

        let syncword                = br.read(11)?;
        if syncword != 0x7FF { return Err(DecoderError::InvalidData); }
        let id                      = br.read(2)?;
        if id == 1 { return Err(DecoderError::InvalidData); }
        let layer                   = (br.read(2)? ^ 3) as u8;
        if layer == 3 { return Err(DecoderError::InvalidData); }
        let _protection             = br.read_bool()?;
        let bitrate_index           = br.read(4)? as usize;
        if bitrate_index == 15 { return Err(DecoderError::InvalidData); }
        if bitrate_index == 0 {
            //todo freeform eventually
            unimplemented!();
        }
        let mut sf_idx              = br.read(2)? as usize;
        if sf_idx == 3 { return Err(DecoderError::InvalidData); }
        let padding                 = br.read_bool()?;
        let _private                = br.read_bool()?;
        let mode                    = br.read(2)? as u8;

        match id {
            0 => sf_idx += 6,
            2 => sf_idx += 3,
            _ => {},
        };
        let mpeg1 = id == 3;
        let srate = SAMPLING_RATE[sf_idx];
        let channels = if mode == 3 { 1 } else { 2 };
        let bitrate = BITRATE[if mpeg1 { 0 } else { 1 }][layer as usize][bitrate_index];
        let frame_size = match layer {
                0 => {
                    ((SAMPLES / 3 / 8 * 1000 * (bitrate as usize) / (srate as usize)) & !3) + if padding { 4 } else { 0 }
                },
                2 if !mpeg1 => {
                    SAMPLES / 2 / 8 * 1000 * (bitrate as usize) / (srate as usize) + if padding { 1 } else { 0 }
                },
                _ => {
                    SAMPLES / 8 * 1000 * (bitrate as usize) / (srate as usize) + if padding { 1 } else { 0 }
                },
            };
        let nsamples = if mpeg1 { SAMPLES } else { SAMPLES / 2 };

        Ok(MPAHeader{ layer, srate, channels, frame_size, nsamples })
    }
}

impl NAPacketiser for MPAPacketiser {
    fn add_data(&mut self, src: &[u8]) -> bool {
        self.buf.extend_from_slice(src);
        self.buf.len() < 4096
    }
    fn parse_stream(&mut self, id: u32) -> DecoderResult<NAStreamRef> {
        if self.hdr.is_none() {
            if self.buf.len() < 4 {
                return Err(DecoderError::ShortData);
            }
            let hdr = self.parse_header(0)?;
            self.hdr = Some(hdr);
        }
        let hdr = self.hdr.unwrap();
        let mut duration = 0;
        if hdr.layer == 2 { // check for Xing/LAME info
            let mpeg1 = hdr.srate >= 32000;
            let offset = match (mpeg1, hdr.channels) {
                    (true, 1)  => 24,
                    (true, _)  => 36,
                    (false, 1) => 13,
                    (false, _) => 21,
                };
            if self.buf.len() >= offset + 12 && (&self.buf[offset..][..4] == b"Xing" || &self.buf[offset..][..4] == b"Info") {
                let flags   = read_u32be(&self.buf[offset + 4..]).unwrap_or(0);
                if (flags & 1) != 0 {
                    duration = u64::from(read_u32be(&self.buf[offset + 8..]).unwrap_or(0));
                }
            } else if self.buf.len() >= offset + 18 && &self.buf[offset..][..6] == b"VBRI\x00\x01" {
                duration = u64::from(read_u32be(&self.buf[offset + 14..]).unwrap_or(0));
            }
        }
        let ainfo = NAAudioInfo::new(hdr.srate, hdr.channels, SND_F32P_FORMAT, hdr.nsamples);
        let cname = match hdr.layer {
                0 => "mp1",
                1 => "mp2",
                _ => "mp3",
            };
        let info = NACodecInfo::new(cname, NACodecTypeInfo::Audio(ainfo), None);
        Ok(NAStream::new(StreamType::Audio, id, info, hdr.nsamples as u32, hdr.srate, duration).into_ref())
    }
    fn skip_junk(&mut self) -> DecoderResult<usize> {
        if self.buf.len() <= 2 {
            return Ok(0);
        }
        let mut off = 0;
        let mut hdr = u16::from(self.buf[0]) * 256 + u16::from(self.buf[1]);
        let mut iter = self.buf[2..].iter();
        loop {
            if (hdr & 0xFFE0) != 0xFFE0 {
                let ret = self.parse_header(off);
                match ret {
                    Ok(hdr) => {
                        if self.hdr.is_none() {
                            self.hdr = Some(hdr);
                        }
                        if self.hdr.unwrap() != hdr { // header is valid but mismatches
                            self.buf.drain(..off + 1);
                            return Err(DecoderError::InvalidData);
                        }
                        break;
                    },
                    Err(err) => {
                        self.buf.drain(..off + 1);
                        return Err(err);
                    },
                };
            }
            off += 1;
            if let Some(&b) = iter.next() {
                hdr = (hdr << 8) | u16::from(b);
            } else {
                break;
            }
        }
        self.buf.drain(..off);
        Ok(off)
    }
    fn get_packet(&mut self, stream: NAStreamRef) -> DecoderResult<Option<NAPacket>> {
        if self.buf.len() < 4 {
            return Err(DecoderError::ShortData);
        }
        let hdr = self.parse_header(0)?;
        if self.hdr.is_none() {
            self.hdr = Some(hdr);
        }
        if self.hdr.unwrap() != hdr {
            return Err(DecoderError::InvalidData);
        }
        if hdr.frame_size <= self.buf.len() {
            let mut data = Vec::with_capacity(hdr.frame_size);
            data.extend_from_slice(&self.buf[..hdr.frame_size]);
            self.buf.drain(..hdr.frame_size);
            let ts = NATimeInfo::new(None, None, Some(1), hdr.nsamples as u32, hdr.srate);
            Ok(Some(NAPacket::new(stream, ts, true, data)))
        } else {
            Ok(None)
        }
    }
    fn reset(&mut self) {
        self.buf.clear();
    }
    fn bytes_left(&self) -> usize { self.buf.len() }
}

pub fn get_packetiser() -> Box<dyn NAPacketiser + Send> {
    Box::new(MPAPacketiser::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::test_decode_audio;
    use crate::mpeg_register_all_decoders;
    use nihav_flash::flash_register_all_demuxers;
    use std::io::Read;
    use nihav_core::codecs::NAPacketiser;

    #[test]
    fn test_mpeg1_layer3_mono() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        mpeg_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/FLV/flash_video_5/i_004.flv
        let file = "assets/Flash/i_004.flv";
        test_decode_audio("flv", file, Some(6000), None/*Some("mp3_1")*/, &dmx_reg, &dec_reg);
    }
    #[test]
    fn test_mpeg1_layer3_stereo() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        mpeg_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/FLV/venture_030_ivcp_001_8bit.flv
        let file = "assets/Flash/venture_030_ivcp_001_8bit.flv";
        test_decode_audio("flv", file, Some(7200), None/*Some("mp3_2")*/, &dmx_reg, &dec_reg);
    }
    #[test]
    fn test_mpeg2_layer3() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        mpeg_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/FLV/flash_with_alpha/lection2-2.flv
        let file = "assets/Flash/lection2-2.flv";
        test_decode_audio("flv", file, Some(6000), None/*Some("mp3_3")*/, &dmx_reg, &dec_reg);
    }
    #[test]
    fn test_mpa_packetiser() {
        let mut buf = [0; 16384];
        // sample from a private collection
        let mut file = std::fs::File::open("assets/MPEG/1.mp3").unwrap();

        let mut pkts = super::MPAPacketiser::new();
        file.read_exact(&mut buf).unwrap();
        pkts.add_data(&buf);
        let stream = pkts.parse_stream(0).unwrap();
        let mut frame_sizes = Vec::with_capacity(15);
        while let Some(pkt) = pkts.get_packet(stream.clone()).unwrap() {
            let frame_size = pkt.get_buffer().len();
            println!("pkt size {}", frame_size);
            frame_sizes.push(frame_size);
        }
        assert_eq!(&frame_sizes, &[1044, 1044, 1045, 1045, 1045, 1045, 1045, 1045, 1045, 1045, 1045, 1044, 1045, 1045, 1045]);
    }
}

const BITRATE: [[[u32; 15]; 3]; 2] = [
  [
    [ 0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448 ],
    [ 0, 32, 48, 56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384 ],
    [ 0, 32, 40, 48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320 ]
  ], [
    [ 0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256 ],
    [ 0,  8, 16, 24, 32, 40, 48,  56,  64,  80,  96, 112, 128, 144, 160 ],
    [ 0,  8, 16, 24, 32, 40, 48,  56,  64,  80,  96, 112, 128, 144, 160 ]
  ]
];

const SAMPLING_RATE: [u32; 9] = [ 44100, 48000, 32000, 22050, 24000, 16000, 11025, 12000, 8000 ];
