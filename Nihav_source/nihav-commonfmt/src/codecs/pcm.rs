use nihav_core::formats::*;
use nihav_core::codecs::*;
#[cfg(feature="encoder_pcm")]
use nihav_core::io::byteio::*;

#[derive(Clone,Copy,Debug,PartialEq)]
#[cfg(feature="decoder_pcm")]
enum PCMMode {
    Infinity,
    ALaw,
    MuLaw,
}

#[cfg(feature="decoder_pcm")]
struct PCMDecoder {
    chmap:  NAChannelMap,
    mode:   PCMMode,
}

#[cfg(feature="decoder_pcm")]
fn cvt_alaw(val: u8) -> i16 {
    let val = val ^ 0x55;
    let sign = (val & 0x80) != 0;
    let exp  = ((val >> 4) & 7) + 4;
    let mant = (val & 0xF) + if exp != 0 { 16 } else { 0 };
    let aval = i16::from(mant) << exp;
    if sign { aval } else { -aval }
}

#[cfg(feature="decoder_pcm")]
fn cvt_mulaw(val: u8) -> i16 {
    let val = !val;
    let sign = (val & 0x80) != 0;
    let exp  = ((val >> 4) & 7) + 3;
    let mant = (val & 0xF) | 0x10;
    let aval = (i16::from(mant) << exp) - 128 - 4;
    if !sign { aval } else { -aval }
}

#[cfg(feature="decoder_pcm")]
impl PCMDecoder {
    fn new(mode: PCMMode) -> Self {
        PCMDecoder { chmap: NAChannelMap::new(), mode }
    }
    fn decode_xlaw(&self, pkt: &NAPacket, duration: u64, srate: u32) -> DecoderResult<NAFrameRef> {
        let pktbuf = pkt.get_buffer();
        let channels = self.chmap.num_channels();
        let abuf = alloc_audio_buffer(NAAudioInfo::new(srate, channels as u8, SND_S16_FORMAT, 1), duration as usize, self.chmap.clone())?;
        let mut buf = abuf.get_abuf_i16().unwrap();
        let dst = buf.get_data_mut().unwrap();
        for (src, dst) in pktbuf.chunks(channels).zip(dst.chunks_mut(channels)) {
            for (isamp, dsamp) in src.iter().zip(dst.iter_mut()) {
                *dsamp = if self.mode == PCMMode::ALaw { cvt_alaw(*isamp) } else { cvt_mulaw(*isamp) };
            }
        }
        let info = pkt.get_stream().get_info();
        let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
        frm.set_duration(Some(duration));
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
}

#[cfg(feature="decoder_pcm")]
const CHMAP_MONO: [NAChannelType; 1] = [NAChannelType::C];
#[cfg(feature="decoder_pcm")]
const CHMAP_STEREO: [NAChannelType; 2] = [NAChannelType::L, NAChannelType::R];

#[cfg(feature="decoder_pcm")]
fn get_default_chmap(nch: u8) -> NAChannelMap {
    let mut chmap = NAChannelMap::new();
    match nch {
        1 => chmap.add_channels(&CHMAP_MONO),
        2 => chmap.add_channels(&CHMAP_STEREO),
        _ => (),
    }
    chmap
}

#[cfg(feature="decoder_pcm")]
fn get_duration(ainfo: &NAAudioInfo, duration: Option<u64>, data_size: usize) -> u64 {
    if let Some(dur) = duration {
        dur
    } else {
        let size_bits = (data_size as u64) * 8;
        let blk_size = u64::from(ainfo.get_channels()) * u64::from(ainfo.get_format().get_bits());
        size_bits / blk_size
    }
}

#[cfg(feature="decoder_pcm")]
impl NADecoder for PCMDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.chmap = get_default_chmap(ainfo.get_channels());
            if self.chmap.num_channels() == 0 { return Err(DecoderError::InvalidData); }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            let duration = get_duration(&ainfo, pkt.get_duration(), pkt.get_buffer().len());
            if self.mode != PCMMode::Infinity {
                return self.decode_xlaw(pkt, duration, ainfo.sample_rate);
            }
            let pktbuf = pkt.get_buffer();
            let abuf = NAAudioBuffer::new_from_buf(ainfo, pktbuf, self.chmap.clone());
            let mut frm = NAFrame::new_from_pkt(pkt, info, NABufferType::AudioPacked(abuf));
            frm.set_duration(Some(duration));
            frm.set_keyframe(true);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

#[cfg(feature="decoder_pcm")]
impl NAOptionHandler for PCMDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

#[cfg(feature="decoder_pcm")]
pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(PCMDecoder::new(PCMMode::Infinity))
}

#[cfg(feature="decoder_pcm")]
pub fn get_a_law_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(PCMDecoder::new(PCMMode::ALaw))
}

#[cfg(feature="decoder_pcm")]
pub fn get_mu_law_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(PCMDecoder::new(PCMMode::MuLaw))
}

#[cfg(feature="encoder_pcm")]
struct PCMEncoder {
    stream: Option<NAStreamRef>,
    pkt:    Option<NAPacket>,
}

#[cfg(feature="encoder_pcm")]
impl PCMEncoder {
    fn new() -> Self {
        PCMEncoder {
            stream:     None,
            pkt:        None,
        }
    }
}

#[allow(unused_macros)]
macro_rules! write_buffer {
    ($abuf: expr, $dvec: expr, $write_be: ident, $write_le: ident, $dtype: tt) => {
        let info = $abuf.get_info();
        let len  = $abuf.get_length();
        let data = $abuf.get_data();
        let channels = $abuf.get_chmap().num_channels();
        let stride = $abuf.get_stride();
        let step = $abuf.get_step();
        let is_be = info.format.be;

        $dvec = vec![0u8; len * channels * std::mem::size_of::<$dtype>()];
        let mut mw = MemoryWriter::new_write($dvec.as_mut_slice());
        let mut bw = ByteWriter::new(&mut mw);
        for off in 0..len {
            for j in 0..channels {
                if is_be {
                    bw.$write_be(data[off * step + j * stride] as $dtype).unwrap();
                } else {
                    bw.$write_le(data[off * step + j * stride] as $dtype).unwrap();
                }
            }
        }
    }
}

#[cfg(feature="encoder_pcm")]
impl NAEncoder for PCMEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Audio(NAAudioInfo::new(0, 0, SND_S16P_FORMAT, 0)),
                    ..Default::default()
                })
            },
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => {
                Ok(*encinfo)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_CBR }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => {
                let info = NACodecInfo::new("pcm", encinfo.format, None);
                let mut stream = NAStream::new(StreamType::Audio, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();
                self.stream = Some(stream.clone());
                Ok(stream)
            }
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        let mut dbuf;
        match buf {
            NABufferType::AudioU8(ref abuf) => {
                let stride = abuf.get_stride();
                if stride == 1 { // packed already
                    self.pkt = Some(NAPacket::new_from_refbuf(self.stream.clone().unwrap(), frm.ts, true, abuf.get_data_ref()));
                    return Ok(());
                }
                let len  = abuf.get_length();
                let data = abuf.get_data();
                let channels = abuf.get_chmap().num_channels();
                dbuf = Vec::with_capacity(len * channels);
                for off in 0..len {
                    for j in 0..channels {
                        dbuf.push(data[off + j * stride]);
                    }
                }
            },
            NABufferType::AudioI16(ref abuf) => {
                write_buffer!(abuf, dbuf, write_u16be, write_u16le, u16);
            },
            NABufferType::AudioI32(ref abuf) => {
                write_buffer!(abuf, dbuf, write_u32be, write_u32le, u32);
            },
            NABufferType::AudioF32(ref abuf) => {
                write_buffer!(abuf, dbuf, write_f32be, write_f32le, f32);
            },
            NABufferType::AudioPacked(ref abuf) => {
                self.pkt = Some(NAPacket::new_from_refbuf(self.stream.clone().unwrap(), frm.ts, true, abuf.get_data_ref()));
                return Ok(());
            },
            NABufferType::None => {
                self.pkt = None;
                return Ok(());
            },
            _ => return Err(EncoderError::FormatError),
        };
        self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, true, dbuf));
        Ok(())
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        let mut npkt = None;
        std::mem::swap(&mut self.pkt, &mut npkt);
        Ok(npkt)
    }
    fn flush(&mut self) -> EncoderResult<()> {
        Ok(())
    }
}

#[cfg(feature="encoder_pcm")]
impl NAOptionHandler for PCMEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

#[cfg(feature="encoder_pcm")]
pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(PCMEncoder::new())
}
