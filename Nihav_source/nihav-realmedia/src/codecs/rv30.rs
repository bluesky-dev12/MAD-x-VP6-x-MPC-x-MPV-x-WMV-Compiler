use nihav_core::formats;
use nihav_core::io::bitreader::*;
use nihav_core::io::intcode::*;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::rv3040::*;
use super::rv30dsp::*;

struct RealVideo30BR {
    rpr_bits:       u8,
    width:          usize,
    height:         usize,
    widths:         Vec<usize>,
    heights:        Vec<usize>,
}

impl RealVideo30BR {
    fn new() -> Self {
        RealVideo30BR {
            rpr_bits:       0,
            width:          0,
            height:         0,
            widths:         Vec::new(),
            heights:        Vec::new(),
        }
    }
}

impl RV34BitstreamDecoder for RealVideo30BR {
    fn decode_slice_header(&mut self, br: &mut BitReader, _old_w: usize, _old_h: usize) -> DecoderResult<RV34SliceHeader> {
        if br.read(3)? != 0 { return Err(DecoderError::InvalidData); }
        let ft_idx              = br.read(2)?;
        let ftype = match ft_idx {
                0|1 => FrameType::I,
                2   => FrameType::P,
                _   => FrameType::B,
            };
        if br.read(1)? != 0 { return Err(DecoderError::InvalidData); }
        let q                   = br.read(5)? as u8;
        let deblock             = !br.read_bool()?;
        let pts                 = br.read(13)? as u16;
        let rpr                 = br.read(self.rpr_bits)? as usize;
        let (w, h) = if rpr != 0 {
                validate!(rpr < self.widths.len());
                (self.widths[rpr], self.heights[rpr])
            } else {
                (self.width, self.height)
            };
        let start               = br.read(get_slice_start_offset_bits(w, h))? as usize;
                                  br.skip(1)?;

        Ok(RV34SliceHeader{ ftype, quant: q, deblock, pts, width: w, height: h, start, end: 0, set_idx: 0 })
    }
    fn decode_intra_pred(&mut self, br: &mut BitReader, types: &mut [i8], mut pos: usize, tstride: usize, _has_top: bool) -> DecoderResult<()> {
        for _ in 0..4 {
            for x in 0..2 {
                let code = br.read_code(UintCodeType::Gamma)? as usize;
                validate!(code < 81);
                for k in 0..2 {
                    let new  = RV30_ITYPE_MAP[code * 2 + k] as usize;
                    let top  = (types[pos + x * 2 + k - tstride] + 1) as usize;
                    let left = (types[pos + x * 2 + k - 1] + 1) as usize;
                    types[pos + x * 2 + k] = RV30_ITYPE[top * 90 + left * 9 + new];
                    validate!(types[pos + x * 2 + k] != 9);
                }
            }
            pos += tstride;
        }
        Ok(())
    }
    fn decode_inter_mb_hdr(&mut self, br: &mut BitReader, ftype: FrameType, _mbtype: MBType) -> DecoderResult<MBInfo> {
        let mut code = br.read_code(UintCodeType::Gamma)? as usize;
        let mut dq = false;
        validate!(code < 11);
        if code > 5 {
            code -= 6;
            dq = true;
        }
        let idx = if ftype == FrameType::P { 0 } else { 1 };
        Ok(MBInfo { mbtype: RV30_MB_TYPES[idx][code], skip_run: 0, dquant: dq })
    }
    fn predict_b_mv(&self, sstate: &SState, mvi: &MVInfo, mbtype: MBType, mvs: &[MV], _mbinfo: &[RV34MBInfo]) -> (MV, MV) {
        let mb_x = sstate.mb_x;
        let mb_y = sstate.mb_y;
        let mv_f;
        let mv_b;
        match mbtype {
            MBType::MBForward | MBType::MBBackward => {
                    let mv_pred = mvi.pred_mb_mv(mb_x, mb_y, true, sstate.has_top, sstate.has_left, sstate.has_tr, sstate.has_tl);
                    mv_f = mv_pred + mvs[0];
                    mv_b = mv_f;
                },
            _   => {
                    mv_f = ZERO_MV;
                    mv_b = ZERO_MV;
                },
        };
        (mv_f, mv_b)
    }
    fn quant_dc(&self, _is_intra: bool, q: u8) -> u8 { RV30_QUANT_DC[q as usize] }
}

struct RealVideo30Decoder {
    bd:         RealVideo30BR,
    info:       NACodecInfoRef,
    dec:        RV34Decoder,
}

impl RealVideo30Decoder {
    fn new() -> Self {
        RealVideo30Decoder{
            bd:         RealVideo30BR::new(),
            info:       NACodecInfoRef::default(),
            dec:        RV34Decoder::new(true, Box::new(RV30DSP::new())),
        }
    }
}

impl NADecoder for RealVideo30Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = formats::YUV420_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            let edata = info.get_extradata().unwrap();
            let src: &[u8] = &edata;

            if src.len() < 2 { return Err(DecoderError::InvalidData); }
            let num_rpr     = (src[1] & 7) as usize;
            if src.len() < num_rpr * 2 + 8 { return Err(DecoderError::ShortData); }
            self.bd.rpr_bits  = ((num_rpr >> 1) + 1) as u8;
            if self.bd.rpr_bits > 3 { self.bd.rpr_bits = 3; }
            for i in 0..=num_rpr {
                self.bd.widths.push ((src[6 + i * 2] as usize) << 2);
                self.bd.heights.push((src[7 + i * 2] as usize) << 2);
            }

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

        let (bufinfo, ftype, pts) = self.dec.parse_frame(supp, src.as_slice(), &mut self.bd)?;

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(ftype == FrameType::I);
        frm.set_pts(Some(pts));
        frm.set_frame_type(ftype);//if ftype == FrameType::B { FrameType::Skip } else { ftype } );
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for RealVideo30Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RealVideo30Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::realmedia_register_all_decoders;
    use crate::realmedia_register_all_demuxers;
    #[test]
    fn test_rv30() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        // sample from a private collection
        test_decoding("realmedia", "realvideo3", "assets/RV/rv30_weighted_mc.rm", Some(700),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x2a4d13bf, 0x2f21f3c9, 0xcbd601be, 0x61a6405c],
                            [0x17ea48c7, 0x68334ff5, 0x6fb9729b, 0x9a93ed12],
                            [0xce42a48c, 0x0b5b7f0d, 0x3f66c4a1, 0x261f08e2],
                            [0x91ca8f5b, 0x1f578a93, 0x44e533f2, 0x83beec8a],
                            [0x8cb256a7, 0xb3889afd, 0x28806114, 0x9bbd5287],
                            [0x694570e2, 0x4b2df948, 0xc7d2e36d, 0xa5eb66b2],
                            [0xb9b68059, 0x0d420917, 0x4e0f33d4, 0x8d3a6b0b],
                            [0xb9d6bfa6, 0x04442a8e, 0x6fafc34e, 0xb418a23e],
                            [0xb94e226d, 0xbf8a5fc5, 0x6d9a03c6, 0x4a0d1a50],
                            [0xa2e76d33, 0x1b6996e4, 0xb6a26052, 0x3f5f6145],
                            [0x3b509515, 0x4aa2f4f9, 0x12a0c73b, 0x5b9b20d1],
                            [0x976e0e06, 0xf6194e6f, 0xe0fefc31, 0xf7587bd3],
                            [0x7b38660e, 0xa46f4080, 0xa493f422, 0x36eaaa3b],
                            [0x6375934a, 0xf2a23087, 0x367f9738, 0xf2251e09],
                            [0x54bcefe7, 0xbbc91dc7, 0x0acec7d7, 0x95cf6d02]]));
        // sample: https://samples.mplayerhq.hu/real/VC-RV30/simpsons-clip.rm
        test_decoding("realmedia", "realvideo3", "assets/RV/simpsons-clip.rm", Some(1337),
                      &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x36604117, 0x415f95cc, 0xec38e776, 0x9818d3be]));
    }
}

const RV30_QUANT_DC: [u8; 32] = [
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 22, 22, 23, 23, 23, 24, 24, 25, 25
];

const RV30_MB_TYPES: [[MBType; 6]; 2] = [
    [ MBType::MBSkip, MBType::MBP16x16, MBType::MBP8x8,    MBType::Invalid,    MBType::MBIntra, MBType::MBIntra16 ],
    [ MBType::MBSkip, MBType::MBDirect, MBType::MBForward, MBType::MBBackward, MBType::MBIntra, MBType::MBIntra16 ],
];

const RV30_ITYPE_MAP: [i8; 9*9*2] = [
    0, 0, 0, 1, 1, 0, 1, 1, 0, 2, 2, 0, 0, 3, 3, 0, 1, 2,
    2, 1, 0, 4, 4, 0, 3, 1, 1, 3, 0, 5, 5, 0, 2, 2, 1, 4,
    4, 1, 0, 6, 3, 2, 1, 5, 2, 3, 5, 1, 6, 0, 0, 7, 4, 2,
    2, 4, 3, 3, 6, 1, 1, 6, 7, 0, 0, 8, 5, 2, 4, 3, 2, 5,
    3, 4, 1, 7, 4, 4, 7, 1, 8, 0, 6, 2, 3, 5, 5, 3, 2, 6,
    1, 8, 2, 7, 7, 2, 8, 1, 5, 4, 4, 5, 3, 6, 6, 3, 8, 2,
    4, 6, 5, 5, 6, 4, 2, 8, 7, 3, 3, 7, 6, 5, 5, 6, 7, 4,
    4, 7, 8, 3, 3, 8, 7, 5, 8, 4, 5, 7, 4, 8, 6, 6, 7, 6,
    5, 8, 8, 5, 6, 7, 8, 6, 7, 7, 6, 8, 8, 7, 7, 8, 8, 8,
];

const RV30_ITYPE: [i8; 10*10*9] = [
    0, 9, 9, 9, 9, 9, 9, 9, 9,
    0, 2, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9,
    2, 0, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9,

    0, 1, 9, 9, 9, 9, 9, 9, 9,
    0, 2, 1, 6, 4, 8, 5, 7, 3,
    1, 0, 2, 6, 5, 4, 3, 8, 7,
    2, 8, 0, 1, 7, 4, 3, 6, 5,
    2, 0, 1, 3, 8, 5, 4, 7, 6,
    2, 0, 1, 4, 6, 7, 8, 3, 5,
    0, 1, 5, 2, 6, 3, 8, 4, 7,
    0, 1, 6, 2, 4, 7, 5, 8, 3,
    2, 7, 0, 1, 4, 8, 6, 3, 5,
    2, 8, 0, 1, 7, 3, 4, 5, 6,

    1, 0, 9, 9, 9, 9, 9, 9, 9,
    1, 2, 5, 6, 3, 0, 4, 8, 7,
    1, 6, 2, 5, 3, 0, 4, 8, 7,
    2, 1, 7, 6, 8, 3, 5, 0, 4,
    1, 2, 5, 3, 6, 8, 4, 7, 0,
    1, 6, 2, 0, 4, 5, 8, 7, 3,
    1, 5, 2, 6, 3, 8, 4, 0, 7,
    1, 6, 0, 2, 4, 5, 7, 3, 8,
    2, 1, 7, 6, 0, 8, 5, 4, 3,
    1, 2, 7, 8, 3, 4, 5, 6, 0,

    9, 9, 9, 9, 9, 9, 9, 9, 9,
    0, 2, 1, 8, 7, 6, 5, 4, 3,
    1, 2, 0, 6, 5, 7, 4, 8, 3,
    2, 8, 7, 1, 0, 6, 4, 3, 5,
    2, 0, 8, 1, 3, 7, 5, 4, 6,
    2, 0, 4, 1, 7, 8, 6, 3, 5,
    2, 0, 1, 5, 8, 4, 6, 7, 3,
    2, 0, 6, 1, 4, 7, 8, 5, 3,
    2, 7, 8, 1, 0, 5, 4, 6, 3,
    2, 8, 7, 1, 0, 4, 3, 6, 5,

    9, 9, 9, 9, 9, 9, 9, 9, 9,
    0, 2, 1, 3, 5, 8, 6, 4, 7,
    1, 0, 2, 5, 3, 6, 4, 8, 7,
    2, 8, 1, 0, 3, 5, 7, 6, 4,
    3, 2, 5, 8, 1, 4, 6, 7, 0,
    4, 2, 0, 6, 1, 5, 8, 3, 7,
    5, 3, 1, 2, 8, 6, 4, 0, 7,
    1, 6, 0, 2, 4, 5, 8, 3, 7,
    2, 7, 0, 1, 5, 4, 8, 6, 3,
    2, 8, 3, 5, 1, 0, 7, 6, 4,

    9, 9, 9, 9, 9, 9, 9, 9, 9,
    2, 0, 6, 1, 4, 7, 5, 8, 3,
    1, 6, 2, 0, 4, 5, 3, 7, 8,
    2, 8, 7, 6, 4, 0, 1, 5, 3,
    4, 2, 1, 0, 6, 8, 3, 5, 7,
    4, 2, 6, 0, 1, 5, 7, 8, 3,
    1, 2, 5, 0, 6, 3, 4, 7, 8,
    6, 4, 0, 1, 2, 7, 5, 3, 8,
    2, 7, 4, 6, 0, 1, 8, 5, 3,
    2, 8, 7, 4, 6, 1, 3, 5, 0,

    9, 9, 9, 9, 9, 9, 9, 9, 9,
    5, 1, 2, 3, 6, 8, 0, 4, 7,
    1, 5, 6, 3, 2, 0, 4, 8, 7,
    2, 1, 5, 3, 6, 8, 7, 4, 0,
    5, 3, 1, 2, 6, 8, 4, 7, 0,
    1, 6, 2, 4, 5, 8, 0, 3, 7,
    5, 1, 3, 6, 2, 0, 8, 4, 7,
    1, 6, 5, 2, 0, 4, 3, 7, 8,
    2, 7, 1, 6, 5, 0, 8, 3, 4,
    2, 5, 1, 3, 6, 8, 4, 0, 7,

    9, 9, 9, 9, 9, 9, 9, 9, 9,
    1, 6, 2, 0, 5, 4, 3, 7, 8,
    1, 6, 5, 4, 2, 3, 0, 7, 8,
    2, 1, 6, 7, 4, 8, 5, 3, 0,
    2, 1, 6, 5, 8, 4, 3, 0, 7,
    6, 4, 1, 2, 0, 5, 7, 8, 3,
    1, 6, 5, 2, 3, 0, 4, 8, 7,
    6, 1, 4, 0, 2, 7, 5, 3, 8,
    2, 7, 4, 6, 1, 5, 0, 8, 3,
    2, 1, 6, 8, 4, 7, 3, 5, 0,

    9, 9, 9, 9, 9, 9, 9, 9, 9,
    2, 0, 4, 7, 6, 1, 8, 5, 3,
    6, 1, 2, 0, 4, 7, 5, 8, 3,
    2, 7, 8, 0, 1, 6, 4, 3, 5,
    2, 4, 0, 8, 3, 1, 7, 6, 5,
    4, 2, 7, 0, 6, 1, 8, 5, 3,
    2, 1, 0, 8, 5, 6, 7, 4, 3,
    2, 6, 4, 1, 7, 0, 5, 8, 3,
    2, 7, 4, 0, 8, 6, 1, 5, 3,
    2, 8, 7, 4, 1, 0, 3, 6, 5,

    9, 9, 9, 9, 9, 9, 9, 9, 9,
    2, 0, 8, 1, 3, 4, 6, 5, 7,
    1, 2, 0, 6, 8, 5, 7, 3, 4,
    2, 8, 7, 1, 0, 3, 6, 5, 4,
    8, 3, 2, 5, 1, 0, 4, 7, 6,
    2, 0, 4, 8, 5, 1, 7, 6, 3,
    2, 1, 0, 8, 5, 3, 6, 4, 7,
    2, 1, 6, 0, 8, 4, 5, 7, 3,
    2, 7, 8, 4, 0, 6, 1, 5, 3,
    2, 8, 3, 0, 7, 4, 1, 6, 5,
];
