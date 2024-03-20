use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;

const DICT_SIZE: usize = 4096;
const START_BITS: u8 = 9;
const MAX_BITS:   u8 = 12;
const START_POS:   usize = 258;
const INVALID_POS: usize = 65536;

struct LZWState {
    dict_sym:   [u8; DICT_SIZE],
    dict_prev:  [u16; DICT_SIZE],
    dict_pos:   usize,
    dict_lim:   usize,
    idx_bits:   u8,
}

impl LZWState {
    fn new() -> Self {
        Self {
            dict_sym:   [0; DICT_SIZE],
            dict_prev:  [0; DICT_SIZE],
            dict_pos:   START_POS,
            dict_lim:   1 << START_BITS,
            idx_bits:   START_BITS,
        }
    }
    fn reset(&mut self) {
        self.dict_pos = START_POS;
        self.dict_lim = 1 << START_BITS;
        self.idx_bits = START_BITS;
    }
    fn add(&mut self, prev: usize, sym: u8) {
        if self.dict_pos < self.dict_lim {
            self.dict_sym [self.dict_pos] = sym;
            self.dict_prev[self.dict_pos] = prev as u16;
            self.dict_pos += 1;
        }
    }
    fn decode_idx(&self, dst: &mut [u8], pos: usize, idx: usize) -> DecoderResult<usize> {
        let mut tot_len = 1;
        let mut tidx = idx;
        while tidx > 256 {
            tidx = self.dict_prev[tidx] as usize;
            tot_len += 1;
        }
        validate!(pos + tot_len <= dst.len());

        let mut end = pos + tot_len - 1;
        let mut tidx = idx;
        while tidx > 256 {
            dst[end] = self.dict_sym[tidx];
            end -= 1;
            tidx = self.dict_prev[tidx] as usize;
        }
        dst[end] = tidx as u8;

        Ok(tot_len)
    }
    #[allow(clippy::comparison_chain)]
    fn decode(&mut self, br: &mut BitReader, dst: &mut [u8]) -> DecoderResult<()> {
        self.reset();

        let mut pos = 0;
        let mut lastidx = INVALID_POS;
        loop {
            let idx         = br.read(self.idx_bits)? as usize;
            if idx == 256 {
                self.reset();
                lastidx = INVALID_POS;
                continue;
            }
            if idx == 257 {
                break;
            }
            if idx < self.dict_pos {
                let len = self.decode_idx(dst, pos, idx)?;
                if lastidx != INVALID_POS {
                    self.add(lastidx, dst[pos]);
                }
                pos += len;
            } else if idx == self.dict_pos {
                validate!(lastidx != INVALID_POS);
                let len = self.decode_idx(dst, pos, lastidx)?;
                let lastsym = dst[pos];
                pos += len;
                validate!(pos < dst.len());
                dst[pos] = lastsym;
                pos += 1;
                self.add(lastidx, lastsym);
            } else {
                return Err(DecoderError::InvalidData);
            }
            lastidx = idx;
            if self.dict_pos == self.dict_lim && self.idx_bits < MAX_BITS {
                self.dict_lim <<= 1;
                self.idx_bits += 1;
            }
        }
        validate!(pos == dst.len());
        Ok(())
    }
}

struct IPMADecoder {
    info:       NACodecInfoRef,
    v1:         bool,
    pal:        [u8; 768],
    frame:      Vec<u8>,
    fframe:     Vec<u8>,
    dbuf:       Vec<u8>,
    width:      usize,
    height:     usize,
    first:      bool,
    lzw:        LZWState,
}

impl IPMADecoder {
    fn new(v1: bool) -> Self {
        Self {
            info:       NACodecInfoRef::default(),
            v1,
            pal:        [0; 768],
            frame:      Vec::new(),
            fframe:     Vec::new(),
            dbuf:       Vec::new(),
            width:      0,
            height:     0,
            first:      false,
            lzw:        LZWState::new(),
        }
    }
}

impl NADecoder for IPMADecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.width;
            self.height = vinfo.height;
            self.frame  = vec![0; self.width * self.height];
            self.fframe = vec![0; self.width * self.height];
            self.dbuf   = vec![0; self.width * self.height];
            self.pal    = [0; 768];
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, true, PAL8_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            self.first = self.v1;

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 0);

        for sd in pkt.side_data.iter() {
            if let NASideData::Palette(true, ref pal) = sd {
                for (dst, src) in self.pal.chunks_mut(3).zip(pal.chunks(4)) {
                    dst[0] = src[0];
                    dst[1] = src[1];
                    dst[2] = src[2];
                }
                break;
            }
        }

        let mut br = BitReader::new(&src, BitReaderMode::LE);
        self.lzw.decode(&mut br, &mut self.dbuf)?;
        if self.first {
            self.fframe.copy_from_slice(&self.dbuf);
            self.first = false;
        }
        for (dp, (&sp, &rp)) in self.frame.iter_mut().zip(self.dbuf.iter().zip(self.fframe.iter())) {
            match sp {
                0 => {},
                1 => *dp = rp,
                _ => *dp = sp,
            };
        }

        let buf = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
        let mut vbuf = buf.get_vbuf().unwrap();
        let paloff = vbuf.get_offset(1);
        let stride = vbuf.get_stride(0);
        let data = vbuf.get_data_mut().unwrap();

        for (drow, srow) in data.chunks_mut(stride).zip(self.frame.chunks(self.width)) {
            drow[..self.width].copy_from_slice(srow);
        }
        data[paloff..][..768].copy_from_slice(&self.pal);

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), buf);
        let ftype = if pkt.keyframe { FrameType::I } else { FrameType::P };
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for IPMADecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(IPMADecoder::new(true))
}

pub fn get_decoder_v2() -> Box<dyn NADecoder + Send> {
    Box::new(IPMADecoder::new(false))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    // samples from https://misc.daniel-marschall.de/spiele/blown_away/ipma_codec/
    #[test]
    fn test_ipma_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("avi", "ipma", "assets/Game/lripa07.avi",
                      Some(2), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x13a4e2e5, 0xeac494b1, 0xef107b9f, 0x1213bbc6],
                            [0x09e2684a, 0xe453ec76, 0xff1c3cee, 0xf38701c9],
                            [0xd39258b1, 0x2491b8d9, 0x37dd4415, 0x8ddb4669]]));
    }
    #[test]
    fn test_ip20_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("avi", "ipma2", "assets/Game/27mwza00.avi",
                      Some(2), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x8b000e62, 0x2e4e0344, 0x471e40ca, 0x7b9f83d4],
                            [0xc1429aa9, 0x3f2e856b, 0x80b27114, 0x25b5d453],
                            [0xfa24d5fa, 0x29bf2e17, 0x3cf5f550, 0x296e29b0]]));
    }
}
