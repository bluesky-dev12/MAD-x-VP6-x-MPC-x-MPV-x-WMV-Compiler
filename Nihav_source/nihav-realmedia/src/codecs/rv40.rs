use nihav_core::formats;
use nihav_core::frame::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::io::intcode::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::rv3040::*;
use super::rv40dsp::*;
use super::rv40data::*;

struct AICCodeReader8 {
    lengths: &'static [u8],
    codes:   &'static [u8],
}

impl CodebookDescReader<i8> for AICCodeReader8 {
    fn bits(&mut self, idx: usize) -> u8    { self.lengths[idx] }
    fn code(&mut self, idx: usize) -> u32   { self.codes[idx] as u32 }
    fn sym (&mut self, idx: usize) -> i8    { idx as i8 }
    fn len (&mut self)             -> usize { self.lengths.len() }
}

struct AICCodeReader16 {
    lengths: &'static [u8],
    codes:   &'static [u16],
}

impl CodebookDescReader<i8> for AICCodeReader16 {
    fn bits(&mut self, idx: usize) -> u8    { self.lengths[idx] }
    fn code(&mut self, idx: usize) -> u32   { self.codes[idx] as u32 }
    fn sym (&mut self, idx: usize) -> i8    { idx as i8 }
    fn len (&mut self)             -> usize { self.lengths.len() }
}

struct TypeCodeReader {
    lengths: &'static [u8],
    codes:   &'static [u8],
    syms:    &'static [MBType],
}

impl CodebookDescReader<MBType> for TypeCodeReader {
    fn bits(&mut self, idx: usize) -> u8    { self.lengths[idx] }
    fn code(&mut self, idx: usize) -> u32   { self.codes[idx] as u32 }
    fn sym (&mut self, idx: usize) -> MBType{ self.syms[idx] }
    fn len (&mut self)             -> usize { self.lengths.len() }
}

struct RealVideo40BR {
    width:          usize,
    height:         usize,
    aic_top_cb:     Codebook<i8>,
    aic_mode1_cb:   Vec<Codebook<i8>>,
    aic_mode2_cb:   Vec<Codebook<i8>>,
    ptype_cb:       Vec<Codebook<MBType>>,
    btype_cb:       Vec<Codebook<MBType>>,
    had_skip_run:   bool,
}

impl RealVideo40BR {
    fn new() -> Self {
        let mut coderead = AICCodeReader8{ lengths: &RV40_AIC_TOP_BITS, codes: &RV40_AIC_TOP_CODES };
        let aic_top_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();

        let mut aic_mode1_cb: Vec<Codebook<i8>> = Vec::with_capacity(RV40_AIC_MODE1_BITS.len());
        for i in 0..RV40_AIC_MODE1_BITS.len() {
            if (i % 10) != 9 {
                let mut coderead = AICCodeReader8{ lengths: &RV40_AIC_MODE1_BITS[i], codes: &RV40_AIC_MODE1_CODES[i] };
                let cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
                aic_mode1_cb.push(cb);
            } else {
                let mut coderead = AICCodeReader8{ lengths: &RV40_AIC_MODE1_BITS_DUMMY, codes: &RV40_AIC_MODE1_CODES_DUMMY };
                let cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
                aic_mode1_cb.push(cb);
            }
        }

        let mut aic_mode2_cb: Vec<Codebook<i8>> = Vec::with_capacity(RV40_AIC_MODE2_BITS.len());
        for i in 0..RV40_AIC_MODE2_BITS.len() {
            let mut coderead = AICCodeReader16{ lengths: &RV40_AIC_MODE2_BITS[i], codes: &RV40_AIC_MODE2_CODES[i] };
            let cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
            aic_mode2_cb.push(cb);
        }

        let mut ptype_cb: Vec<Codebook<MBType>> = Vec::with_capacity(RV40_PTYPE_BITS.len());
        for i in 0..RV40_PTYPE_BITS.len() {
            let mut coderead = TypeCodeReader{ lengths: &RV40_PTYPE_BITS[i], codes: &RV40_PTYPE_CODES[i], syms: RV40_PTYPE_SYMS };
            let cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
            ptype_cb.push(cb);
        }

        let mut btype_cb: Vec<Codebook<MBType>> = Vec::with_capacity(RV40_BTYPE_BITS.len());
        for i in 0..RV40_BTYPE_BITS.len() {
            let mut coderead = TypeCodeReader{ lengths: &RV40_BTYPE_BITS[i], codes: &RV40_BTYPE_CODES[i], syms: RV40_BTYPE_SYMS };
            let cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
            btype_cb.push(cb);
        }

        RealVideo40BR {
            width:          0,
            height:         0,
            aic_top_cb,
            aic_mode1_cb,
            aic_mode2_cb,
            ptype_cb,
            btype_cb,
            had_skip_run:   false,
        }
    }
    fn predict_b_mv_component(&self, sstate: &SState, mvi: &MVInfo, mbinfo: &[RV34MBInfo], mbtype: MBType, fwd: bool) -> MV {
        let mut pred_mvs: [MV; 3] = [ZERO_MV; 3];
        let mut mv_count: usize = 0;
        let mb_x = sstate.mb_x;
        let mb_y = sstate.mb_y;
        let mb_stride = sstate.mb_w;
        let mb_idx = mb_x + mb_y * mb_stride;

        if !mbtype.has_mv_dir(fwd) {
            return ZERO_MV;
        }

        if sstate.has_left && mbinfo[mb_idx - 1].mbtype.has_mv_dir(fwd) {
            pred_mvs[mv_count] = mvi.get_mv(mb_x - 1, mb_y, 0, 0, fwd);
            mv_count += 1;
        }
        if !sstate.has_top {
            return pred_mvs[0];
        }
        if mbinfo[mb_idx - mb_stride].mbtype.has_mv_dir(fwd) {
            pred_mvs[mv_count] = mvi.get_mv(mb_x, mb_y - 1, 0, 0, fwd);
            mv_count += 1;
        }
        if sstate.has_tr {
            if mbinfo[mb_idx - mb_stride + 1].mbtype.has_mv_dir(fwd) {
                pred_mvs[mv_count] = mvi.get_mv(mb_x + 1, mb_y - 1, 0, 0, fwd);
                mv_count += 1;
            }
        } else {
            if sstate.has_tl && mbinfo[mb_idx - mb_stride - 1].mbtype.has_mv_dir(fwd) {
                pred_mvs[mv_count] = mvi.get_mv(mb_x - 1, mb_y - 1, 0, 0, fwd);
                mv_count += 1;
            }
        }

        match mv_count {
            3 => MV::pred(pred_mvs[0], pred_mvs[1], pred_mvs[2]),
            2 => { let sum_mv = pred_mvs[0] + pred_mvs[1]; MV { x: sum_mv.x / 2, y: sum_mv.y / 2 } },
            1 => pred_mvs[0],
            _ => ZERO_MV,
        }
    }
}

fn get_dimension(br: &mut BitReader, tab: &'static [i16]) -> DecoderResult<usize> {
    let t = br.read(3)? as usize;
    if tab[t] > 0 { return Ok(tab[t] as usize); }
    if tab[t] < 0 {
        let idx = (-tab[t] as usize) + (br.read(1)? as usize);
        if tab[idx] != 0 { return Ok(tab[idx] as usize); }
    }
    let mut size: usize = 0;
    loop {
        let t = br.read(8)? as usize;
        size += t << 2;
        if t != 255 { break; }
    }
    Ok(size)
}

impl RV34BitstreamDecoder for RealVideo40BR {
    fn decode_slice_header(&mut self, br: &mut BitReader, old_w: usize, old_h: usize) -> DecoderResult<RV34SliceHeader> {
        if br.read(1)? != 0 { return Err(DecoderError::InvalidData); }
        let ft_idx              = br.read(2)?;
        let ftype = match ft_idx {
                0|1 => FrameType::I,
                2   => FrameType::P,
                _   => FrameType::B,
            };
        let q                   = br.read(5)? as u8;
        if br.read(2)? != 0 { return Err(DecoderError::InvalidData); }
        let set_idx             = br.read(2)? as usize;
        let deblock             = !br.read_bool()?;
        let pts                 = br.read(13)? as u16;
        let w;
        let h;
        if (ftype == FrameType::I) || !br.read_bool()? {
            w = get_dimension(br, &RV40_STANDARD_WIDTHS)?;
            h = get_dimension(br, &RV40_STANDARD_HEIGHTS)?;
        } else {
            w = old_w;
            h = old_h;
        }
        let start               = br.read(get_slice_start_offset_bits(w, h))? as usize;

        self.had_skip_run = false;

        Ok(RV34SliceHeader{ ftype, quant: q, deblock, pts, width: w, height: h, start, end: 0, set_idx })
    }
    fn decode_intra_pred(&mut self, br: &mut BitReader, types: &mut [i8], mut pos: usize, tstride: usize, has_top: bool) -> DecoderResult<()> {
        let start;
        if has_top {
            start = 0;
        } else {
            let code = br.read_cb(&self.aic_top_cb)?;
            types[pos + 0] = (code >> 2) & 2;
            types[pos + 1] = (code >> 1) & 2;
            types[pos + 2] = (code >> 0) & 2;
            types[pos + 3] = (code << 1) & 2;
            pos += tstride;
            start = 1;
        }
        for _ in start..4 {
            let mut x: usize = 0;
            while x < 4 {
                let tr = types[pos + x - tstride + 1];
                let t  = types[pos + x - tstride];
                let l  = types[pos + x - 1];
                let ctx = if x < 3 { ((tr & 0xF) as u16) + (((t as u16) & 0xF) << 4) + (((l as u16) & 0xF) << 8) } else { 0xFFF };
                let res = RV40_AIC_PATTERNS.iter().position(|&x| x == ctx);
                if let Some(idx) = res {
                    let code = br.read_cb(&self.aic_mode2_cb[idx])?;
                    types[pos + x + 0] = code / 9;
                    types[pos + x + 1] = code % 9;
                    x += 2;
                } else {
                    if (t != -1) && (l != -1) {
                        let idx = (t as usize) + (l as usize) * 10;
                        types[pos + x] = br.read_cb(&self.aic_mode1_cb[idx])?;
                    } else {
                        match l {
                            -1 if t < 2 => { types[pos + x] =  (br.read(1)? as i8) ^ 1; },
                             0 | 2      => { types[pos + x] = ((br.read(1)? as i8) ^ 1) << 1; },
                            _           => { types[pos + x] = 0; },
                        };
                    }
                    x += 1;
                }
            }
            pos += tstride;
        }
        Ok(())
    }
    fn decode_inter_mb_hdr(&mut self, br: &mut BitReader, ftype: FrameType, mbtype_ref: MBType) -> DecoderResult<MBInfo> {
        let skip_run = if self.had_skip_run { 0 } else { br.read_code(UintCodeType::Gamma)? as usize };
        if skip_run > 0 {
            self.had_skip_run = true;
            return Ok(MBInfo { mbtype: MBType::MBSkip, skip_run: skip_run - 1, dquant: false })
        }
        self.had_skip_run = false;
        let mut mbtype;
        let idx;
        if ftype == FrameType::P {
            idx = match mbtype_ref {
                    MBType::MBIntra     => 0,
                    MBType::MBIntra16   => 1,
                    MBType::MBP16x16    => 2,
                    MBType::MBP8x8      => 3,
                    MBType::MBP16x8     => 4,
                    MBType::MBP8x16     => 5,
                    MBType::MBP16x16Mix => 6,
                    _                   => unreachable!(),
                };
            mbtype = br.read_cb(&self.ptype_cb[idx])?;
        } else {
            idx = match mbtype_ref {
                    MBType::MBIntra     => 0,
                    MBType::MBIntra16   => 1,
                    MBType::MBForward   => 2,
                    MBType::MBBackward  => 3,
                    MBType::MBBidir     => 4,
                    MBType::MBDirect    => 5,
                    _                   => 0,
                };
            mbtype = br.read_cb(&self.btype_cb[idx])?;
        }
        let dquant = mbtype == MBType::Invalid;
        if dquant {
            mbtype = if ftype == FrameType::P { br.read_cb(&self.ptype_cb[idx])? }
                     else { br.read_cb(&self.btype_cb[idx])? };
        }
        Ok(MBInfo { mbtype, skip_run: 0, dquant })
    }
    fn predict_b_mv(&self, sstate: &SState, mvi: &MVInfo, mbtype: MBType, mvs: &[MV], mbinfo: &[RV34MBInfo]) -> (MV, MV) {
        let mut mv_f = self.predict_b_mv_component(sstate, mvi, mbinfo, mbtype, true);
        let mut mv_b = self.predict_b_mv_component(sstate, mvi, mbinfo, mbtype, false);

        match mbtype {
            MBType::MBForward   => { mv_f += mvs[0]; },
            MBType::MBBackward  => { mv_b += mvs[0]; },
            MBType::MBBidir     => {
                    mv_f += mvs[0];
                    mv_b += mvs[1];
                },
            _ => {},
        };

        (mv_f, mv_b)
    }
    fn quant_dc(&self, is_intra: bool, q: u8) -> u8 { RV40_QUANT_DC[if is_intra { 0 } else { 1 }][q as usize] }
}

struct RealVideo40Decoder {
    bd:         RealVideo40BR,
    info:       NACodecInfoRef,
    dec:        RV34Decoder,
}

impl RealVideo40Decoder {
    fn new() -> Self {
        RealVideo40Decoder{
            bd:         RealVideo40BR::new(),
            info:       NACodecInfoRef::default(),
            dec:        RV34Decoder::new(false, Box::new(RV40DSP::new())),
        }
    }
}

impl NADecoder for RealVideo40Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = formats::YUV420_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            let edata = info.get_extradata().unwrap();
            let src: &[u8] = &edata;

            if src.len() < 2 { return Err(DecoderError::InvalidData); }

            self.bd.width  = vinfo.get_width();
            self.bd.height = vinfo.get_height();

            supp.pool_u8.set_dec_bufs(3);
            supp.pool_u8.prealloc_video(NAVideoInfo::new(self.bd.width, self.bd.height, false, fmt), 4)?;

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        let (bufinfo, ftype, ts) = self.dec.parse_frame(supp, src.as_slice(), &mut self.bd)?;

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(ftype == FrameType::I);
        frm.set_frame_type(ftype);
        frm.set_pts(Some(ts));
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for RealVideo40Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RealVideo40Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::realmedia_register_all_decoders;
    use crate::realmedia_register_all_demuxers;
    // samples from a private collection
    #[test]
    fn test_rv40() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        test_decoding("realmedia", "realvideo4", "assets/RV/rv40_weighted_mc.rmvb", Some(1500),
                      &dmx_reg, &dec_reg,ExpectedTestResult::MD5Frames(vec![
                            [0x27cf336a, 0xc1686c50, 0x5304783d, 0x6e77ffa2],
                            [0x91f236c7, 0x3bda2d38, 0x961a0243, 0xda803cf1],
                            [0x4075d7e8, 0xbcd7f85b, 0x1c0dd34b, 0x405d0a5d],
                            [0x642498b7, 0xb57aa202, 0x69ea0d23, 0x1cc0794f],
                            [0x1c1a4df8, 0x7e3fbd7d, 0x7fdeb57f, 0xf5d65179],
                            [0x86a5dcdd, 0xd66caabf, 0xdfe1fc99, 0xb3443375],
                            [0x86846664, 0xbee4268d, 0xc1e017e6, 0xc9d984c8],
                            [0x0ecbe176, 0x81e5aca6, 0xb7bda49c, 0x34007e7b],
                            [0x48c8a90e, 0xed003b8a, 0xc9e7e9a6, 0x54b1eca8],
                            [0x540cbc0b, 0x6d7afaa8, 0xb0951c1f, 0xed22089e],
                            [0x73190f85, 0x9cd72603, 0x1063ca54, 0xd4f82c7f],
                            [0xef6206e8, 0x6affb292, 0xe12b7c9c, 0x37416240],
                            [0x59f61c91, 0x66b2a632, 0x46556395, 0x74fbc1de],
                            [0xd75635ca, 0x60d13826, 0xfa41d914, 0x9cfded0e],
                            [0x7a8c4396, 0x6f3eda39, 0x4238dbaf, 0xa9052803]]));
        test_decoding("realmedia", "realvideo4", "assets/RV/rv40_weighted_mc_2.rmvb", Some(2000),
                      &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x4224b9d6, 0x32e3ff63, 0x02df9e60, 0xfa0548ee]));
    }
}

const RV40_STANDARD_WIDTHS:  [i16;  8] = [ 160, 172, 240, 320, 352, 640, 704, 0 ];
const RV40_STANDARD_HEIGHTS: [i16; 12] = [ 120, 132, 144, 240, 288, 480, -8, -10, 180, 360, 576, 0 ];
