use nihav_core::formats;
use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use std::str::FromStr;

const FRAME_W: usize = 640;
const FRAME_H: usize = 429;

const BMV_INTRA:    u8 = 0x03;
const BMV_SCROLL:   u8 = 0x04;
const BMV_PAL:      u8 = 0x08;
const BMV_COMMAND:  u8 = 0x10;
const BMV_PRINT:    u8 = 0x80;

struct BMVReader<'a> {
    src:        &'a [u8],
    pos:        usize,
    fwd:        bool,
    nibble:     u8,
    saved:      bool,
}

impl<'a> BMVReader<'a> {
    fn new(src: &'a [u8], fwd: bool) -> Self {
        let pos = if fwd { 0 } else { src.len() - 1 };
        Self { src, pos, fwd, nibble: 0, saved: false }
    }
    fn advance(&mut self) {
        if self.fwd {
            if self.pos < self.src.len() - 1 { self.pos += 1; }
        } else {
            if self.pos > 0 { self.pos -= 1; }
        }
    }
    fn get_byte(&mut self) -> u8 {
        let ret = self.src[self.pos];
        self.advance();
        ret
    }
    fn get_nibble(&mut self) -> u8 {
        if self.saved {
            self.saved = false;
            self.nibble
        } else {
            let val = self.get_byte();
            self.saved = true;
            self.nibble = val >> 4;
            val & 0xF
        }
    }
}

struct BMVWriter<'a> {
    data:       &'a mut [u8],
    pos:        usize,
    fwd:        bool,
    off:        isize,
}

impl<'a> BMVWriter<'a> {
    fn new(data: &'a mut [u8], fwd: bool, off: isize) -> Self {
        let pos = if fwd { 0 } else { data.len() - 1 };
        Self { data, pos, fwd, off }
    }
    fn is_at_end(&self) -> bool {
        if self.fwd {
            self.pos == self.data.len() - 1
        } else {
            self.pos == 0
        }
    }
    fn advance(&mut self) {
        if self.fwd {
            if self.pos < self.data.len() - 1 { self.pos += 1; }
        } else {
            if self.pos > 0 { self.pos -= 1; }
        }
    }
    fn put_byte(&mut self, val: u8) {
        self.data[self.pos] = val;
        self.advance();
    }
    fn copy(&mut self, len: usize) {
        for _ in 0..len {
            let saddr = (self.pos as isize) + self.off;
            if saddr < 0 { continue; }
            self.data[self.pos] = self.data[saddr as usize];
            self.advance();
        }
    }
    fn repeat(&mut self, len: usize) {
        let last = if self.fwd { self.data[self.pos - 1] } else { self.data[self.pos + 1] };
        for _ in 0..len {
            self.put_byte(last);
        }
    }
}

struct BMVVideoDecoder {
    info:       NACodecInfoRef,
    pal:        [u8; 768],
    frame:      [u8; FRAME_W * FRAME_H],
}

impl BMVVideoDecoder {
    fn new() -> Self {
        Self {
            info: NACodecInfoRef::default(), pal: [0; 768], frame: [0; FRAME_W * FRAME_H],
        }
    }
    fn decode_frame(&mut self, src: &[u8], bufinfo: &mut NABufferType, line: i16) -> DecoderResult<()> {
        let bufo = bufinfo.get_vbuf();
        let mut buf = bufo.unwrap();
        let paloff = buf.get_offset(1);
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        let fwd = (line <= -640) || (line >= 0);

        let mut br = BMVReader::new(src, fwd);
        let mut bw = BMVWriter::new(&mut self.frame, fwd, line as isize);
        let mut mode = 0;
        while !bw.is_at_end() {
            let mut val = 0;
            let mut shift = 0;
            loop {
                let nib = br.get_nibble() as usize;
                if (nib & 0xC) != 0 {
                    val |= nib << shift;
                    break;
                }
                val |= nib << shift;
                shift += 2;
            }
            if (val & 1) != 0 {
                mode += 1;
            }
            mode += 1;
            if mode >= 4 {
                mode -= 3;
            }
            validate!(val >= 2);
            let len = (val >> 1) - 1;

            match mode {
                1 => bw.copy(len),
                2 => for _ in 0..len { bw.put_byte(br.get_byte()); },
                3 => bw.repeat(len),
                _ => unreachable!(),
            };
        }

        for y in 0..FRAME_H {
            for x in 0..FRAME_W {
                dst[y * stride + x] = self.frame[y * FRAME_W + x];
            }
        }

        let dpal = &mut dst[paloff..][..768];
        dpal.copy_from_slice(&self.pal[0..]);

        Ok(())
    }
}

impl NADecoder for BMVVideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(_vinfo) = info.get_properties() {
            let fmt = NAPixelFormaton::new(ColorModel::RGB(RGBSubmodel::RGB),
                                           Some(NAPixelChromaton::new(0, 0, true, 8, 0, 0, 3)),
                                           Some(NAPixelChromaton::new(0, 0, true, 8, 0, 1, 3)),
                                           Some(NAPixelChromaton::new(0, 0, true, 8, 0, 2, 3)),
                                           None, None,
                                           FORMATON_FLAG_PALETTE, 3);
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(FRAME_W, FRAME_H, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 1);

        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);
        let flags                               = br.read_byte()?;

        if (flags & BMV_COMMAND) != 0 {
            let size = if (flags & BMV_PRINT) != 0 { 8 } else { 10 };
            br.read_skip(size)?;
        }
        if (flags & BMV_PAL) != 0 {
            br.read_buf(&mut self.pal)?;
        }
        let line;
        if (flags & BMV_SCROLL) != 0 {
            line                                = br.read_u16le()? as i16;
        } else if (flags & BMV_INTRA) == BMV_INTRA {
            line = -640;
        } else {
            line = 0;
        }
        let pos = br.tell() as usize;

        let mut bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;

        self.decode_frame(&src[pos..], &mut bufinfo, line)?;

        let is_intra = (flags & BMV_INTRA) == BMV_INTRA;
        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for BMVVideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder_video() -> Box<dyn NADecoder + Send> {
    Box::new(BMVVideoDecoder::new())
}

struct BMVAudioDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
}

const BMV_AUD_SCALES: [i32; 16] = [ 16512, 8256, 4128, 2064, 1032, 516, 258, 192, 129, 88, 64, 56, 48, 40, 36, 32 ];

impl BMVAudioDecoder {
    fn new() -> Self {
        Self {
            ainfo:  NAAudioInfo::new(0, 1, formats::SND_S16P_FORMAT, 0),
            chmap:  NAChannelMap::new(),
        }
    }
}

fn scale_sample(samp: u8, scale: i32) -> i16 {
    let val = (i32::from(samp as i8) * scale) >> 5;
    if val < -32768 {
        -32768
    } else if val > 32767 {
        32767
    } else {
        val as i16
    }
}

impl NADecoder for BMVAudioDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), ainfo.get_channels(), formats::SND_S16P_FORMAT, 32);
            self.chmap = NAChannelMap::from_str("L,R").unwrap();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            validate!(pktbuf.len() > 1);
            let nblocks = pktbuf[0] as usize;
            validate!(pktbuf.len() == 1 + 65 * nblocks);
            let samples = nblocks * 32;
            let abuf = alloc_audio_buffer(self.ainfo, samples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let off1 = adata.get_offset(1);
            let dst = adata.get_data_mut().unwrap();
            let psrc = &pktbuf[1..];
            for (n, src) in psrc.chunks_exact(65).enumerate() {
                let code = src[0].rotate_right(1);
                let scale0 = BMV_AUD_SCALES[(code & 0xF) as usize];
                let scale1 = BMV_AUD_SCALES[(code >>  4) as usize];
                let aoff0 = n * 32;
                let aoff1 = aoff0 + off1;
                let data = &src[1..];
                for (i, samp) in data.chunks_exact(2).enumerate() {
                    dst[aoff0 + i] = scale_sample(samp[0], scale0);
                    dst[aoff1 + i] = scale_sample(samp[1], scale1);
                }
            }
            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
            frm.set_duration(Some(samples as u64));
            frm.set_keyframe(false);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for BMVAudioDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder_audio() -> Box<dyn NADecoder + Send> {
    Box::new(BMVAudioDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    // samples from: https://samples.mplayerhq.hu/game-formats/bmv/
    #[test]
    fn test_bmv_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("bmv", "bmv-video", "assets/Game/WILDCAT.BMV", Some(40), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x9e91bb16, 0xc1edafc9, 0x4ef3171f, 0x0f3f6181]));
    }
    #[test]
    fn test_bmv_audio() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("bmv", "bmv-audio", "assets/Game/PERFECT.BMV", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x90b9ace4, 0x5fc19938, 0x7f534560, 0x32589cdf]));
    }
}
