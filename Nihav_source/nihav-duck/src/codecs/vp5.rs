use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::codecs::{MV, ZIGZAG};
use super::vpcommon::*;
use super::vp56::*;

struct VP5BR {}

impl VP5BR {
    fn new() -> Self {
        Self {}
    }
}

impl VP56Parser for VP5BR {
    fn parse_header(&mut self, bc: &mut BoolCoder) -> DecoderResult<VP56Header> {
        let mut hdr = VP56Header::default();
        hdr.is_intra                            = !bc.read_bool();
        hdr.is_golden = hdr.is_intra;
                                                  bc.read_bool();
        hdr.quant                               = bc.read_bits(6) as u8;
        if hdr.is_intra {
            hdr.version                         = bc.read_bits(13) as u8;
            validate!(hdr.version == 5);
            hdr.profile                         = bc.read_bits(2) as u8;
            hdr.interlaced                      = bc.read_bool();
            validate!(!hdr.interlaced);
            hdr.mb_h                            = bc.read_bits(8) as u8;
            hdr.mb_w                            = bc.read_bits(8) as u8;
            hdr.disp_h                          = bc.read_bits(8) as u8;
            hdr.disp_w                          = bc.read_bits(8) as u8;
            validate!((hdr.mb_h > 0) && (hdr.mb_w > 0) && (hdr.disp_w > 0) && (hdr.disp_h > 0));
            validate!((hdr.disp_w <= hdr.mb_w) && (hdr.disp_h <= hdr.mb_h));
            hdr.scale                           = bc.read_bits(2) as u8;
        }

        Ok(hdr)
    }
    fn decode_mv(&self, bc: &mut BoolCoder, model: &VP56MVModel) -> i16 {
        if bc.read_prob(model.nz_prob) {
            let sign                            = bc.read_prob(model.sign_prob);
            let b0                              = bc.read_prob(model.raw_probs[0]) as i16;
            let b1                              = bc.read_prob(model.raw_probs[1]) as i16;
            let top: i16 = vp_tree!(bc, model.tree_probs[0],
                                vp_tree!(bc, model.tree_probs[1],
                                    vp_tree!(bc, model.tree_probs[2], 0, 1),
                                    vp_tree!(bc, model.tree_probs[3], 2, 3)
                                ),
                                vp_tree!(bc, model.tree_probs[4],
                                    vp_tree!(bc, model.tree_probs[5], 4, 5),
                                    vp_tree!(bc, model.tree_probs[6], 6, 7)
                                )
                           );
            let val = (top << 2) | (b1 << 1) | b0;
            if !sign {
                val
            } else {
                -val
            }
        } else {
            0
        }
    }
    fn reset_models(&self, models: &mut VP56Models) {
        for mdl in models.mv_models.iter_mut() {
            mdl.nz_prob         = 128;
            mdl.sign_prob       = 128;
            mdl.raw_probs[0]    = 85;
            mdl.raw_probs[1]    = 128;
            mdl.tree_probs      = [128; 7];
        }
    }
    fn decode_mv_models(&self, bc: &mut BoolCoder, models: &mut [VP56MVModel; 2]) -> DecoderResult<()> {
        const HAS_NZ_PROB:   [u8; 2] = [ 243, 235 ];
        const HAS_SIGN_PROB: [u8; 2] = [ 220, 211 ];
        const HAS_RAW0_PROB: [u8; 2] = [ 251, 246 ];
        const HAS_RAW1_PROB: [u8; 2] = [ 253, 249 ];
        const HAS_TREE_PROB: [[u8; 7]; 2] = [
            [ 237, 232, 241, 245, 247, 251, 253 ],
            [ 234, 231, 248, 249, 252, 252, 254 ]
        ];
        for comp in 0..2 {
            if bc.read_prob(HAS_NZ_PROB[comp]) {
                models[comp].nz_prob            = bc.read_probability();
            }
            if bc.read_prob(HAS_SIGN_PROB[comp]) {
                models[comp].sign_prob          = bc.read_probability();
            }
            if bc.read_prob(HAS_RAW0_PROB[comp]) {
                models[comp].raw_probs[0]       = bc.read_probability();
            }
            if bc.read_prob(HAS_RAW1_PROB[comp]) {
                models[comp].raw_probs[1]       = bc.read_probability();
            }
        }
        for comp in 0..2 {
            for i in 0..7 {
                if bc.read_prob(HAS_TREE_PROB[comp][i]) {
                    models[comp].tree_probs[i]  = bc.read_probability();
                }
            }
        }
        Ok(())
    }
    fn decode_coeff_models(&self, bc: &mut BoolCoder, models: &mut VP56Models, is_intra: bool) -> DecoderResult<()> {
        const COEF_PROBS: [[u8; 11]; 2] = [
            [ 146, 197, 181, 207, 232, 243, 238, 251, 244, 250, 249 ],
            [ 179, 219, 214, 240, 250, 254, 244, 254, 254, 254, 254 ]
        ];

        let mut def_prob = [128u8; 11];
        for plane in 0..2 {
            for i in 0..11 {
                if bc.read_prob(COEF_PROBS[plane][i]) {
                    def_prob[i]                 = bc.read_probability();
                    models.coeff_models[plane].dc_value_probs[i] = def_prob[i];
                } else if is_intra {
                    models.coeff_models[plane].dc_value_probs[i] = def_prob[i];
                }
            }
        }
        for ctype in 0..3 {
            for plane in 0..2 {
                for group in 0..6 {
                    for i in 0..11 {
                        if bc.read_prob(VP5_AC_PROBS[ctype][plane][group][i]) {
                            def_prob[i]         = bc.read_probability();
                            models.coeff_models[plane].ac_val_probs[ctype][group][i] = def_prob[i];
                        } else if is_intra {
                            models.coeff_models[plane].ac_val_probs[ctype][group][i] = def_prob[i];
                        }
                    }
                }
            }
        }
        for plane in 0..2 {
            let mdl = &mut models.coeff_models[plane];
            for i in 0..6 {
                for j in 0..6 {
                    for k in 0..5 {
                        mdl.dc_token_probs[i][j][k] = rescale_prob(mdl.dc_value_probs[k], &VP5_DC_WEIGHTS[k][i][j], 254);
                    }
                }
            }
            for ctype in 0..3 {
                for group in 0..3 {
                    for i in 0..6 {
                        for j in 0..5 {
                            mdl.ac_type_probs[ctype][group][i][j] = rescale_prob(mdl.ac_val_probs[ctype][group][j], &VP5_AC_WEIGHTS[ctype][group][j][i], 254);
                        }
                    }
                }
            }
        }
        Ok(())
    }
    fn decode_block(&self, bc: &mut BoolCoder, coeffs: &mut [i16; 64], model: &VP56CoeffModel, _vp6model: &VP6Models, fstate: &mut FrameState) -> DecoderResult<()> {
        const COEF_GROUPS: [u8; 64] = [
            0, 0, 1, 1, 2, 1, 1, 2,
            2, 1, 1, 2, 2, 2, 1, 2,
            2, 2, 2, 2, 1, 1, 2, 2,
            3, 3, 4, 3, 4, 4, 4, 3,
            3, 3, 3, 3, 4, 3, 3, 3,
            4, 4, 4, 4, 4, 3, 3, 4,
            4, 4, 3, 4, 4, 4, 4, 4,
            4, 4, 5, 5, 5, 5, 5, 5
        ];

        let mut ctype = 1;
        let left_ctx = fstate.coeff_cat[fstate.ctx_idx][0] as usize;
        let top_ctx = fstate.top_ctx as usize;

        let mut val_probs: &[u8; 11] = &model.dc_value_probs;
        let mut tok_probs: &[u8] = &model.dc_token_probs[left_ctx][top_ctx];
        let mut idx = 0;
        loop {
            let token = vp_tree!(bc, tok_probs[0],
                            if ctype != 0 { vp_tree!(bc, tok_probs[1], break, 0) } else { 0 },
                            vp_tree!(bc, tok_probs[2],
                                1,
                                vp_tree!(bc, tok_probs[3],
                                    vp_tree!(bc, tok_probs[4], 2,
                                             vp_tree!(bc, val_probs[5], 3, 4)),
                                    TOKEN_LARGE)));
            let val = expand_token_bc(bc, val_probs, token, 5);
            ctype = token.min(2) as usize;
            if token < TOKEN_LARGE {
                fstate.coeff_cat[fstate.ctx_idx][idx] = token.min(3);
            } else {
                fstate.coeff_cat[fstate.ctx_idx][idx] = 4;
            }
            coeffs[ZIGZAG[idx]] = val;
            if idx > 0 {
                coeffs[ZIGZAG[idx]] *= fstate.ac_quant;
            }

            idx += 1;
            if idx >= 64 {
                break;
            }
            let group = COEF_GROUPS[idx] as usize;
            val_probs = &model.ac_val_probs[ctype][group];
            tok_probs = if group > 2 { val_probs
                        } else {
                            let ctx = fstate.coeff_cat[fstate.ctx_idx][idx] as usize;
                            &model.ac_type_probs[ctype][group][ctx]
                        };
        }
        let end = fstate.last_idx[fstate.ctx_idx].min(24);
        fstate.last_idx[fstate.ctx_idx] = idx;
        for i in idx..end {
            fstate.coeff_cat[fstate.ctx_idx][i] = 5;
        }
        fstate.top_ctx = fstate.coeff_cat[fstate.ctx_idx][0];

        Ok(())
    }
    fn decode_block_huff(&self, _br: &mut BitReader, _coeffs: &mut [i16; 64], _vp6model: &VP6Models, _model: &VP6HuffModels, _fstate: &mut FrameState) -> DecoderResult<()> {
        unreachable!();
    }
    fn mc_block(&self, dst: &mut NASimpleVideoFrame<u8>, mc_buf: NAVideoBufferRef<u8>, src: NAVideoBufferRef<u8>, plane: usize, x: usize, y: usize, mv: MV, loop_str: i16) {
        let (sx, sy, mx, my) = if (plane != 1) && (plane != 2) {
                (mv.x >> 1, mv.y >> 1, mv.x & 1, mv.y & 1)
            } else {
                (mv.x >> 2, mv.y >> 2, if (mv.x & 3) != 0 { 1 } else { 0 }, if (mv.y & 3) != 0 { 1 } else { 0 })
            };
        let mode1 = (mx as usize) + (my as usize) * 2;
        let mode = if (mode1 == 3) && (mv.x ^ mv.y < 0) {
                4
            } else {
                mode1
            };
        vp_copy_block(dst, src, plane, x, y, sx, sy, 0, 1, loop_str,
                      mode, VP3_INTERP_FUNCS, mc_buf);
    }
}

struct VP5Decoder {
    dec:        VP56Decoder,
    info:       NACodecInfoRef,
    br:         VP5BR,
}

impl VP5Decoder {
    fn new() -> Self {
        Self {
            dec:        VP56Decoder::new(5, false, true),
            info:       NACodecInfoRef::default(),
            br:         VP5BR::new(),
        }
    }
}

impl NADecoder for VP5Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let myvinfo = NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, YUV420_FORMAT);
            let myinfo = NACodecTypeInfo::Video(myvinfo);
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.dec.init(supp, myvinfo)?;
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        let (bufinfo, ftype) = self.dec.decode_frame(supp, src.as_slice(), &mut self.br)?;

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(ftype == FrameType::I);
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for VP5Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(VP5Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::duck_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;

    #[test]
    fn test_vp5() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);

        let file = "assets/Duck/Cell-140.vp5";
        //let file = "assets/Duck/Chocolat-500.vp5";
        // sample: https://samples.mplayerhq.hu/V-codecs/VP5/Cell-140.vp5
        test_decoding("avi", "vp5", file, Some(96), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x9ad78b0f, 0xed988ead, 0x88ed2ea9, 0xcdb75cdf]));
    }
}

const VP5_AC_PROBS: [[[[u8; 11]; 6]; 2]; 3] = [
  [
    [
      [ 227, 246, 230, 247, 244, 254, 254, 254, 254, 254, 254 ],
      [ 202, 254, 209, 231, 231, 249, 249, 253, 254, 254, 254 ],
      [ 206, 254, 225, 242, 241, 251, 253, 254, 254, 254, 254 ],
      [ 235, 254, 241, 253, 252, 254, 254, 254, 254, 254, 254 ],
      [ 234, 254, 248, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ]
    ], [
      [ 240, 254, 248, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 238, 254, 240, 253, 254, 254, 254, 254, 254, 254, 254 ],
      [ 244, 254, 251, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ]
    ]
  ], [
    [
      [ 206, 203, 227, 239, 247, 254, 253, 254, 254, 254, 254 ],
      [ 207, 199, 220, 236, 243, 252, 252, 254, 254, 254, 254 ],
      [ 212, 219, 230, 243, 244, 253, 252, 254, 254, 254, 254 ],
      [ 236, 237, 247, 252, 253, 254, 254, 254, 254, 254, 254 ],
      [ 240, 240, 248, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ]
    ], [
      [ 230, 233, 249, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 238, 238, 250, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 248, 251, 254, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ]
    ]
  ], [
    [
      [ 225, 239, 227, 231, 244, 253, 243, 254, 254, 253, 254 ],
      [ 232, 234, 224, 228, 242, 249, 242, 252, 251, 251, 254 ],
      [ 235, 249, 238, 240, 251, 254, 249, 254, 253, 253, 254 ],
      [ 249, 253, 251, 250, 254, 254, 254, 254, 254, 254, 254 ],
      [ 251, 250, 249, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ]
    ], [
      [ 243, 244, 250, 250, 254, 254, 254, 254, 254, 254, 254 ],
      [ 249, 248, 250, 253, 254, 254, 254, 254, 254, 254, 254 ],
      [ 253, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ],
      [ 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254 ]
    ]
  ]
];

const VP5_DC_WEIGHTS: [[[[i16; 2]; 6]; 6]; 5] = [
  [
    [ [154,  61], [141,  54], [ 90,  45], [ 54,  34], [ 54,  13], [128, 109] ],
    [ [136,  54], [148,  45], [ 92,  41], [ 54,  33], [ 51,  15], [ 87, 113] ],
    [ [ 87,  44], [ 97,  40], [ 67,  36], [ 46,  29], [ 41,  15], [ 64,  80] ],
    [ [ 59,  33], [ 61,  31], [ 51,  28], [ 44,  22], [ 33,  12], [ 49,  63] ],
    [ [ 69,  12], [ 59,  16], [ 46,  14], [ 31,  13], [ 26,   6], [ 92,  26] ],
    [ [128, 108], [ 77, 119], [ 54,  84], [ 26,  71], [ 87,  19], [ 95, 155] ]
  ], [
    [ [154,   4], [182,   0], [159,  -8], [128,  -5], [143,  -5], [187,  55] ],
    [ [182,   0], [228,  -3], [187,  -7], [174,  -9], [189, -11], [169,  79] ],
    [ [161,  -9], [192,  -8], [187,  -9], [169, -10], [136,  -9], [184,  40] ],
    [ [164, -11], [179, -10], [174, -10], [161, -10], [115,  -7], [197,  20] ],
    [ [195, -11], [195, -11], [146, -10], [110,  -6], [ 95,  -4], [195,  39] ],
    [ [182,  55], [172,  77], [177,  37], [169,  29], [172,  52], [ 92, 162] ]
  ], [
    [ [174,  80], [164,  80], [ 95,  80], [ 46,  66], [ 56,  24], [ 36, 193] ],
    [ [164,  80], [166,  77], [105,  76], [ 49,  68], [ 46,  31], [ 49, 186] ],
    [ [ 97,  78], [110,  74], [ 72,  72], [ 44,  60], [ 33,  30], [ 69, 131] ],
    [ [ 61,  61], [ 69,  63], [ 51,  57], [ 31,  48], [ 26,  27], [ 64,  89] ],
    [ [ 67,  23], [ 51,  32], [ 36,  33], [ 26,  28], [ 20,  12], [ 44,  68] ],
    [ [ 26, 197], [ 41, 189], [ 61, 129], [ 28, 103], [ 49,  52], [-12, 245] ]
  ], [
    [ [102, 141], [ 79, 166], [ 72, 162], [ 97, 125], [179,   4], [307,   0] ],
    [ [ 72, 168], [ 69, 175], [ 84, 160], [105, 127], [148,  34], [310,   0] ],
    [ [ 84, 151], [ 82, 161], [ 87, 153], [ 87, 135], [115,  51], [317,   0] ],
    [ [ 97, 125], [102, 131], [105, 125], [ 87, 122], [ 84,  64], [ 54, 184] ],
    [ [166,  18], [146,  43], [125,  51], [ 90,  64], [ 95,   7], [ 38, 154] ],
    [ [294,   0], [ 13, 225], [ 10, 225], [ 67, 168], [  0, 167], [161,  94] ]
  ], [
    [ [172,  76], [172,  75], [136,  80], [ 64,  98], [ 74,  67], [315,   0] ],
    [ [169,  76], [207,  56], [164,  66], [ 97,  80], [ 67,  72], [328,   0] ],
    [ [136,  80], [187,  53], [154,  62], [ 72,  85], [ -2, 105], [305,   0] ],
    [ [ 74,  91], [128,  64], [113,  64], [ 61,  77], [ 41,  75], [259,   0] ],
    [ [ 46,  84], [ 51,  81], [ 28,  89], [ 31,  78], [ 23,  77], [202,   0] ],
    [ [323,   0], [323,   0], [300,   0], [236,   0], [195,   0], [328,   0] ]
  ]
];

const VP5_AC_WEIGHTS: [[[[[i16; 2]; 6]; 5]; 3]; 3] = [
  [
    [
      [ [276,  0], [238,  0], [195,  0], [156,  0], [113,  0], [274,  0] ],
      [ [  0,  1], [  0,  1], [  0,  1], [  0,  1], [  0,  1], [  0,  1] ],
      [ [192, 59], [182, 50], [141, 48], [110, 40], [ 92, 19], [125,128] ],
      [ [169, 87], [169, 83], [184, 62], [220, 16], [184,  0], [264,  0] ],
      [ [212, 40], [212, 36], [169, 49], [174, 27], [  8,120], [182, 71] ]
    ], [
      [ [259, 10], [197, 19], [143, 22], [123, 16], [110,  8], [133, 88] ],
      [ [  0,  1], [256,  0], [  0,  1], [  0,  1], [  0,  1], [  0,  1] ],
      [ [207, 46], [187, 50], [ 97, 83], [ 23,100], [ 41, 56], [ 56,188] ],
      [ [166, 90], [146,108], [161, 88], [136, 95], [174,  0], [266,  0] ],
      [ [264,  7], [243, 18], [184, 43], [-14,154], [ 20,112], [ 20,199] ]
    ], [
      [ [230, 26], [197, 22], [159, 20], [146, 12], [136,  4], [ 54,162] ],
      [ [  0,  1], [  0,  1], [  0,  1], [  0,  1], [  0,  1], [  0,  1] ],
      [ [192, 59], [156, 72], [ 84,101], [ 49,101], [ 79, 47], [ 79,167] ],
      [ [138,115], [136,116], [166, 80], [238,  0], [195,  0], [261,  0] ],
      [ [225, 33], [205, 42], [159, 61], [ 79, 96], [ 92, 66], [ 28,195] ]
    ]
  ], [
    [
      [ [200, 37], [197, 18], [159, 13], [143,  7], [102,  5], [123,126] ],
      [ [197,  3], [220, -9], [210,-12], [187, -6], [151, -2], [174, 80] ],
      [ [200, 53], [187, 47], [159, 40], [118, 38], [100, 18], [141,111] ],
      [ [179, 78], [166, 86], [197, 50], [207, 27], [187,  0], [115,139] ],
      [ [218, 34], [220, 29], [174, 46], [128, 61], [ 54, 89], [187, 65] ]
    ], [
      [ [238, 14], [197, 18], [125, 26], [ 90, 25], [ 82, 13], [161, 86] ],
      [ [189,  1], [205, -2], [156, -4], [143, -4], [146, -4], [172, 72] ],
      [ [230, 31], [192, 45], [102, 76], [ 38, 85], [ 56, 41], [ 64,173] ],
      [ [166, 91], [141,111], [128,116], [118,109], [177,  0], [ 23,222] ],
      [ [253, 14], [236, 21], [174, 49], [ 33,118], [ 44, 93], [ 23,187] ]
    ], [
      [ [218, 28], [179, 28], [118, 35], [ 95, 30], [ 72, 24], [128,108] ],
      [ [187,  1], [174, -1], [125, -1], [110, -1], [108, -1], [202, 52] ],
      [ [197, 53], [146, 75], [ 46,118], [ 33,103], [ 64, 50], [118,126] ],
      [ [138,114], [128,122], [161, 86], [243, -6], [195,  0], [ 38,210] ],
      [ [215, 39], [179, 58], [ 97,101], [ 95, 85], [ 87, 70], [ 69,152] ]
    ]
  ], [
    [
      [ [236, 24], [205, 18], [172, 12], [154,  6], [125,  1], [169, 75] ],
      [ [187,  4], [230, -2], [228, -4], [236, -4], [241, -2], [192, 66] ],
      [ [200, 46], [187, 42], [159, 34], [136, 25], [105, 10], [179, 62] ],
      [ [207, 55], [192, 63], [192, 54], [195, 36], [177,  1], [143, 98] ],
      [ [225, 27], [207, 34], [200, 30], [131, 57], [ 97, 60], [197, 45] ]
    ], [
      [ [271,  8], [218, 13], [133, 19], [ 90, 19], [ 72,  7], [182, 51] ],
      [ [179,  1], [225, -1], [154, -2], [110, -1], [ 92,  0], [195, 41] ],
      [ [241, 26], [189, 40], [ 82, 64], [ 33, 60], [ 67, 17], [120, 94] ],
      [ [192, 68], [151, 94], [146, 90], [143, 72], [161,  0], [113,128] ],
      [ [256, 12], [218, 29], [166, 48], [ 44, 99], [ 31, 87], [148, 78] ]
    ], [
      [ [238, 20], [184, 22], [113, 27], [ 90, 22], [ 74,  9], [192, 37] ],
      [ [184,  0], [215, -1], [141, -1], [ 97,  0], [ 49,  0], [264, 13] ],
      [ [182, 51], [138, 61], [ 95, 63], [ 54, 59], [ 64, 25], [200, 45] ],
      [ [179, 75], [156, 87], [174, 65], [177, 44], [174,  0], [164, 85] ],
      [ [195, 45], [148, 65], [105, 79], [ 95, 72], [ 87, 60], [169, 63] ]
    ]
  ]
];
