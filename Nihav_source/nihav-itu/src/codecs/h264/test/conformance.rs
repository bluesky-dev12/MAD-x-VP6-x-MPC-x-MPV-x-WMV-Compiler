use nihav_core::codecs::RegisteredDecoders;
use nihav_core::demuxers::RegisteredDemuxers;
use nihav_codec_support::test::dec_video::*;
use nihav_commonfmt::generic_register_all_demuxers;
use crate::itu_register_all_decoders;

use super::raw_demux::RawH264DemuxerCreator;

const PREFIX: &str = "assets/ITU/h264-conformance/";

fn test_files(names: &[(&str, [u32; 4])]) {
    let mut dmx_reg = RegisteredDemuxers::new();
    dmx_reg.add_demuxer(&RawH264DemuxerCreator{});
    generic_register_all_demuxers(&mut dmx_reg);
    let mut dec_reg = RegisteredDecoders::new();
    itu_register_all_decoders(&mut dec_reg);

    for (name, hash) in names.iter() {
        let test_name = format!("{}{}", PREFIX, name);
        println!("Testing {}", test_name);
        test_decoding("rawh264", "h264", &test_name, None, &dmx_reg, &dec_reg, ExpectedTestResult::MD5(*hash));
    }
}

const GENERAL_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("NL1_Sony_D.jsv", [0xD4BB8D98, 0x0C1377EE, 0x45515763, 0xAE7989FD]),
    ("SVA_NL1_B.264", [0xB5626983, 0xAC087749, 0x7FFF9A4B, 0x10D2F1D4]),
    ("NL2_Sony_H.jsv", [0x48D8380C, 0xDB7EFF52, 0x116C1AAD, 0xDBC583F5]),
    ("SVA_NL2_E.264", [0xB47E932D, 0x43628801, 0x3B8453D9, 0xA1D0F60D]),
    ("BA1_Sony_D.jsv", [0x114D1CF9, 0x4A2FCAFF, 0xDA0CF1B4, 0x9964BF3D]),
    ("SVA_BA1_B.264", [0xDAB92AA2, 0x145AB44A, 0xBAB2BEB2, 0x868DD326]),
    ("BA2_Sony_F.jsv", [0x124D2830, 0x7B057028, 0x12A374CF, 0xC9AAD615]),
    ("SVA_BA2_D.264", [0x66130B14, 0x295574BF, 0x35B725A8, 0xEADED3AE]),
    ("BA_MW_D.264", [0x7D5D351A, 0xD0616402, 0x94BF43A4, 0x3150FBCA]),
    ("BANM_MW_D.264", [0xE637D38E, 0xD004DF35, 0x40218E3D, 0x84B43E42]),
    ("BA1_FT_C.264", [0x8598CFC0, 0x6EDE33D4, 0xF24D8552, 0x28E5C8BB]),
    ("NLMQ1_JVC_C.264", [0xFFCABB64, 0x192CED39, 0x90872B46, 0x70AF05EB]),
    ("NLMQ2_JVC_C.264", [0x90B70FBA, 0xA5CA679E, 0xC9BF5E01, 0x1DDBA8F9]),
    ("BAMQ1_JVC_C.264", [0x2F4F0B86, 0xC76F0356, 0x491CE56D, 0x6331E885]),
    ("BAMQ2_JVC_C.264", [0xE3F5D5B0, 0x774B5537, 0x0745F2D0, 0x4F009575]),
    ("SVA_Base_B.264", [0x97F3F1F5, 0xB4034C8A, 0xBC29EF43, 0xE752005C]),
    ("SVA_FM1_E.264", [0x079F354D, 0xAC1204EE, 0x1C31DCAE, 0xD421E99C]),
    ("BASQP1_Sony_C.jsv", [0x630F0900, 0x8D248A40, 0xC3E04F7E, 0x43351EC6]),
    /*"FM1_BT_B.h264",
    "FM2_SVA_C.264",
    "FM1_FT_E.264",*/ //special slice modes
    ("CI_MW_D.264", [0x0EB95292, 0xAD9FC21C, 0x89D93F8B, 0x049E451A]),
    ("SVA_CL1_E.264", [0x5723A151, 0x8DE9FADC, 0xA7499C5B, 0xA34DA7C4]),
    ("CI1_FT_B.264", [0x16F329D0, 0x196938FD, 0xB1AB2402, 0x5B208CFD]),
    ("CVFC1_Sony_C.jsv", [0x4A8F8461, 0xD42A83C5, 0x126C4E5E, 0x5B2060D6]),
    ("AUD_MW_E.264", [0xE96FE505, 0x4DE0329A, 0x8868D060, 0x03375CDB]),
    ("MIDR_MW_D.264", [0xD87BFF88, 0xB2C5B96C, 0xCB291EF6, 0x8A45BBC2]),
    ("NRF_MW_E.264", [0xA8635615, 0xB50C5A16, 0xDECC555A, 0x3C6C81C8]),
    ("MPS_MW_A.264", [0x88BB5A51, 0x3BD7F3CC, 0x8190C7C0, 0x3688AB22]),
    ("CVBS3_Sony_C.jsv", [0xC7794005, 0xC8DA6D8D, 0xEB435ECE, 0x07908055]),
    ("BA3_SVA_C.264", [0xBF1AE77A, 0x094AA1E7, 0x5316BE72, 0x5B46EF1B]),
    ("SL1_SVA_B.264", [0xA44D8BAF, 0x7290B175, 0xC59A308D, 0x5C0D1E4C]),
    ("NL3_SVA_E.264", [0x428B0604, 0xFF02E0A0, 0x0DA08577, 0xDA0EEB76]),
    ("cvmp_mot_frm0_full_B.26l", [0x9F43ED02, 0xC0C322E8, 0x9FCAB584, 0xC9B31EC4]),
    // no direct mention
    //"FM2_SVA_B.264", //special slice mode
];
#[test]
fn test_h264_general() {
    test_files(GENERAL_TEST_STREAMS);
}

const I_PCM_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CVPCMNL1_SVA_C.264", [0x5C1FD0F6, 0x8E875200, 0x711FEBF1, 0xD683E58F]),
    ("CVPCMNL2_SVA_C.264", [0xAF1F1DBE, 0x1DD6569C, 0xB02271F0, 0x53217D88]),
];
#[test]
fn test_h264_ipcm() {
    test_files(I_PCM_TEST_STREAMS);
}

const MMCO_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("MR1_BT_A.h264", [0x617BF915, 0x48E89440, 0xC899A917, 0xC73CF171]),
    ("MR2_TANDBERG_E.264", [0x69C17B20, 0xDF6E89E6, 0x82BD82F1, 0x93B6D282]),
    ("MR3_TANDBERG_B.264", [0xC8AAC175, 0xE5E73C68, 0x87EE02FF, 0x6DEA0F64]),
    ("MR4_TANDBERG_C.264", [0xA40042BC, 0xAB00C341, 0xA9651725, 0x46d31A2C]),
    ("MR5_TANDBERG_C.264", [0x999EAE2E, 0x016DB374, 0x708B00E4, 0x335AE723]),
    ("MR1_MW_A.264", [0xDD56DC8E, 0x403B18EC, 0x57EB5B3A, 0xD834FFDE]),
    ("MR2_MW_A.264", [0xE1E93E65, 0x96AF2EFD, 0x0E7D0FE5, 0x94D5BE85]),
    /*"MR6_BT_B.h264",
    "MR7_BT_B.h264",
    "MR8_BT_B.h264",*/ // interlaced coding
    ("HCBP1_HHI_A.264", [0x2AD73C01, 0x57EA7763, 0x0F0BDE82, 0x17E27DC2]),
    ("HCBP2_HHI_A.264", [0x93F3C560, 0x42519B03, 0x1A4F03B1, 0xFD2C3A84]),
];
#[test]
fn test_h264_mmco() {
    test_files(MMCO_TEST_STREAMS);
}

const WP_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CVWP5_TOSHIBA_E.264", [0x9663DA55, 0xE5EF516C, 0x8BF0CA0B, 0xCC0ABBB8]),
    ("CVWP1_TOSHIBA_E.264", [0xE8868CA5, 0xE934AD77, 0x9132CDB3, 0xC71BE000]),
    ("CVWP2_TOSHIBA_E.264", [0xD06C81B6, 0x31FD4381, 0xF5AFB345, 0x4AF650AC]),
    ("CVWP3_TOSHIBA_E.264", [0x909A1FC8, 0x63D649CC, 0x4F091CBB, 0x058ADA78]),
];
#[test]
fn test_h264_wp() {
    test_files(WP_TEST_STREAMS);
}

/*const FIELD_CODING_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "CVNLFI1_Sony_C.jsv",
    "CVNLFI2_Sony_H.jsv",
    "Sharp_MP_Field_1_B.jvt",
    "Sharp_MP_Field_2_B.jvt",
    "Sharp_MP_Field_3_B.jvt",
    "CVFI1_Sony_D.jsv",
    "CVFI2_Sony_H.jsv",
    "FI1_Sony_E.jsv",
    "CVFI1_SVA_C.264",
    "CVFI2_SVA_C.264",
    "cvmp_mot_fld0_full_B.26l",
    "CVMP_MOT_FLD_L30_B.26l",
];
#[test]
fn test_h264_field() {
    test_files(FIELD_CODING_TEST_STREAMS);
}*/

/*const FRAME_FIELD_CODING_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "Sharp_MP_PAFF_1r2.jvt",
    "CVPA1_TOSHIBA_B.264",
    "cvmp_mot_picaff0_full_B.26l",
];
#[test]
fn test_h264_frame_field() {
    test_files(FRAME_FIELD_CODING_TEST_STREAMS);
}*/

/*const MBAFF_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "CVMANL1_TOSHIBA_B.264",
    "CVMANL2_TOSHIBA_B.264",
    "CVMA1_Sony_D.jsv",
    "CVMA1_TOSHIBA_B.264",
    "CVMAQP2_Sony_G.jsv",
    "CVMAQP3_Sony_D.jsv",
    "CVMAPAQP3_Sony_E.jsv",
    "cvmp_mot_mbaff0_full_B.26l",
    "CVMP_MOT_FRM_L31_B.26l",
];
#[test]
fn test_h264_mbaff() {
    test_files(MBAFF_CODING_TEST_STREAMS);
}*/

/*const S_PICTURE_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "sp1_bt_a.h264",
    "sp2_bt_b.h264",
];
#[test]
fn test_h264_s_picture() {
    test_files(S_PICTURE_TEST_STREAMS);
}*/

const LONG_SEQUENCE_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("LS_SVA_D.264", [0x9C53BE4B, 0x1DEDCD45, 0x98D30293, 0xF01C7BFE]),
];
#[test]
fn test_h264_long_sequence() {
    test_files(LONG_SEQUENCE_TEST_STREAMS);
}

const SEI_VUI_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CVSE2_Sony_B.jsv", [0xDD660FB4, 0x07FEB42E, 0xCD3AF06B, 0x42FDA90D]),
    ("CVSE3_Sony_H.jsv", [0x14DBB021, 0x2CC75879, 0xAA2C6282, 0x14FD2FFE]),
    ("CVSEFDFT3_Sony_E.jsv", [0xABEDFA7A, 0xAAAADE32, 0xE5E5CF35, 0x9A0DE4EA]),
];
#[test]
fn test_h264_sei_vui() {
    test_files(SEI_VUI_TEST_STREAMS);
}

const CABAC_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CANL1_TOSHIBA_G.264", [0xAFA07274, 0x6B16BD96, 0xF3152B45, 0xE2F2881E]),
    ("CANL1_Sony_E.jsv", [0x27F1D5D3, 0x89E110FC, 0x320788BF, 0x78006DB0]),
    ("CANL2_Sony_E.jsv", [0x3A28438E, 0x3E0795DE, 0xAED795FC, 0xFEFBC833]),
    ("CANL3_Sony_C.jsv", [0xFE2DC3CB, 0xA055044C, 0x739911B0, 0xE6AA66BA]),
    ("CANL1_SVA_B.264", [0xB02DEFCB, 0x741C0E98, 0x2313C574, 0x9F2008ED]),
    ("CANL2_SVA_B.264", [0xB02DEFCB, 0x741C0E98, 0x2313C574, 0x9F2008ED]),
    ("CANL3_SVA_B.264", [0x04A6DE98, 0x4EF88D1B, 0x8C1B26FC, 0x8F33A425]),
    ("CANL4_SVA_B.264", [0x19cee0ac, 0xcfbebacc, 0x57aa4cf0, 0x3e4ef26d]),
    ("CABA1_Sony_D.jsv", [0x24B155A4, 0x00DC10D1, 0x1D45A3AA, 0xDF61AE25]),
    ("CABA2_Sony_E.jsv", [0x3731F0F1, 0xACE3AD91, 0x76093A7B, 0x46347CEA]),
    ("CABA3_Sony_C.jsv", [0x873A96BF, 0x9359056B, 0x3BF8D878, 0x469B0106]),
    ("CABA3_TOSHIBA_E.264", [0x13651D01, 0xC5B533E6, 0xB7AA132B, 0xAE7669ED]),
    ("CABA1_SVA_B.264", [0x2F5CABD5, 0xBB4954C0, 0x386CAFD8, 0xA9AA782A]),
    ("CABA2_SVA_B.264", [0x6D4277A7, 0xFC70ED1F, 0xBE3C5F10, 0xB0A70671]),
    ("CABA3_SVA_B.264", [0x0F6066DD, 0xFAED6801, 0x8B3FEE8C, 0xFE8A2E1D]),
    ("camp_mot_frm0_full.26l", [0x22D837CA, 0x60037CC6, 0xA73AF607, 0x969E5422]),
];
#[test]
fn test_h264_cabac() {
    test_files(CABAC_TEST_STREAMS);
}

const CABAC_INIT_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CABACI3_Sony_B.jsv", [0x5AFF6524, 0xAE5642C9, 0xE26D44CC, 0xE5BFBF02]),
];
#[test]
fn test_h264_cabac_init() {
    test_files(CABAC_INIT_TEST_STREAMS);
}

const CABAC_MB_QPTEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CAQP1_Sony_B.jsv", [0xCAC07EAA, 0xBD141764, 0xD64CF9DE, 0x0230A92E]),
    ("CACQP3_Sony_D.jsv", [0x9E3036F3, 0x79705C9C, 0x32E37D44, 0xF66E5B3A]),
];
#[test]
fn test_h264_cabac_mb_qp() {
    test_files(CABAC_MB_QPTEST_STREAMS);
}

const CABAC_SLICE_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CABAST3_Sony_E.jsv", [0xDDA7F376, 0xA040B262, 0x79F6C7FB, 0x04E078BA]),
    ("CABASTBR3_Sony_B.jsv", [0xE3A61D43, 0xCED165BC, 0xB9A745E0, 0x52F9A2A6]),
];
#[test]
fn test_h264_cabac_slice() {
    test_files(CABAC_SLICE_TEST_STREAMS);
}

const CABAC_I_PCM_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CAPCMNL1_Sand_E.264", [0xEE9968EE, 0xEFE935F0, 0x45C6B70B, 0xE51691EB]),
    ("CAPCM1_Sand_E.264", [0xCA073CA1, 0x06E70D5C, 0xD51F6748, 0x5846A5B1]),
    ("CAPM3_Sony_D.jsv", [0xEA5C66FB, 0xD81D8A80, 0xFC876482, 0xF6A2DEC7]),
];
#[test]
fn test_h264_cabac_ipcm() {
    test_files(CABAC_I_PCM_TEST_STREAMS);
}

const CABAC_MMCO_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    /*"MR9_BT_B.h264",*/ //MBAFF
    ("HCMP1_HHI_A.264", [0x18486B98, 0xA55E03D3, 0x8F57EF2B, 0x031FA660]),
];
#[test]
fn test_h264_cabac_mmco() {
    test_files(CABAC_MMCO_TEST_STREAMS);
}

const CABAC_WP_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CAWP1_TOSHIBA_E.264", [0x87946607, 0xD1D774C2, 0xDA8EC863, 0x2710C84A]),
    ("CAWP5_TOSHIBA_E.264", [0x9663DA55, 0xE5EF516C, 0x8BF0CA0B, 0xCC0ABBB8]),
];
#[test]
fn test_h264_cabac_wp() {
    test_files(CABAC_WP_TEST_STREAMS);
}

/*const CABAC_FIELD_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "CABREF3_Sand_D.264",
    "CAFI1_SVA_C.264",
    "camp_mot_fld0_full.26l",
];
#[test]
fn test_h264_cabac_field_() {
    test_files(CABAC_FIELD_TEST_STREAMS);
}*/

/*const CABAC_FIELD_FRAME_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "Sharp_MP_PAFF_2.jvt",
    "CAPA1_TOSHIBA_B.264",
    "camp_mot_picaff0_full.26l",
];
#[test]
fn test_h264_cabac_field_frame() {
    test_files(CABAC_FIELD_FRAMETEST_STREAMS);
}*/

/*const CABAC_MBAFF_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "CAMANL1_TOSHIBA_B.264",
    "CAMANL2_TOSHIBA_B.264",
    "CANLMA2_Sony_C.jsv",
    "CANLMA3_Sony_C.jsv",
    "CAMA1_Sony_C.jsv",
    "CAMA1_TOSHIBA_B.264",
    "CAMANL3_Sand_E.264",
    "CAMA3_Sand_E.264",
    "CAMASL3_Sony_B.jsv",
    "CAMACI3_Sony_C.jsv",
    "camp_mot_mbaff0_full.26l",
    "CAMP_MOT_MBAFF_L30.26l",
    "CAMP_MOT_MBAFF_L31.26l",
    "CAPAMA3_Sand_F.264",
    "cama1_vtc_c.avc",
    "cama2_vtc_b.avc",
    "cama3_vtc_b.avc",
];
#[test]
fn test_h264_cabac_mbaff() {
    test_files(CABAC_MBAFF_TEST_STREAMS);
}*/

/*const CABAC_CAVLC_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "CVCANLMA2_Sony_C.jsv",
];
#[test]
fn test_h264_cabac_cavlc() {
    test_files(CABAC_CAVLC_TEST_STREAMS);
}*/ // contains MBAFF

const CABAC_PRED_BW_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("src19td.IBP.264", [0x45C0D420, 0x524779D5, 0xF5D8F8D1, 0xFB02218B]),
];
#[test]
fn test_h264_cabac_pred_bw() {
    test_files(CABAC_PRED_BW_TEST_STREAMS);
}

const FREXT_420_8_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("FRext/FRExt1_Panasonic.avc", [0x2383599D, 0xA01FAE95, 0xCEE2C970, 0xD88CFA93]),
    ("FRext/FRExt3_Panasonic.avc", [0x3C85643C, 0xE98FD834, 0xA1DA2600, 0x0307F1AF]),
    ("FRext/HCAFR1_HHI.264", [0xB5B74CA7, 0xBA23DFA4, 0xEF89E066, 0x299EC511]),
    //("FRext/HCAFF1_HHI.264", [0;4]), //PAFF
    //("FRext/HCAMFF1_HHI.264", [0;4]), //MBAFF
    //("FRext/FRExt2_Panasonic.avc", [0;4]), //PAFF
    //("FRext/FRExt4_Panasonic.avc", [0;4]), //MBAFF
    ("FRext/HPCANL_BRCM_C.264", [0x8B11529D, 0x9FF96CB8, 0xACC7EDCD, 0x81EDB1E6]),
    ("FRext/HPCA_BRCM_C.264", [0xC38BCFE1, 0xC5A6A1BF, 0x54F08947, 0x23943FAE]),
    /*("FRext/HPCAFLNL_BRCM_C.264", [0;4]), //PAFF
    ("FRext/HPCAFL_BRCM_C.264", [0;4]),*/
    ("FRext/HCAFR2_HHI.264", [0x0CBD7BBF, 0xE29B3C53, 0x64CF3F10, 0x34B1FF5A]),
    ("FRext/HCAFR3_HHI.264", [0x5842BBA0, 0x6C01267B, 0xB093FD66, 0xD36CBA66]),
    ("FRext/HCAFR4_HHI.264", [0x6E80B189, 0xAAE83055, 0x6F51F4EE, 0xC3BEE5C8]),
    ("FRext/HPCADQ_BRCM_B.264", [0x976A176F, 0x89296F2B, 0x14F2141D, 0x74D684BB]),
    ("FRext/HPCALQ_BRCM_B.264", [0x976A176F, 0x89296F2B, 0x14F2141D, 0x74D684BB]),
    //("FRext/HPCAMAPALQ_BRCM_B.264", [0;4]), //MBAFF
    ("FRext/HPCV_BRCM_A.264", [0xEB0EBC5D, 0xA5996EE1, 0x5F6B290F, 0x4F372F21]),
    ("FRext/HPCVNL_BRCM_A.264", [0x441E6C4F, 0x861B69E6, 0xC007EEFB, 0x820D6494]),
    /*("FRext/HPCVFL_BRCM_A.264", [0;4]), //PAFF
    ("FRext/HPCVFLNL_BRCM_A.264", [0;4]),*/
    //("FRext/HPCVMOLQ_BRCM_B.264", [0;4]), //grayscale
    //("FRext/HPCAMOLQ_BRCM_B.264", [0;4]), //grayscale
    ("FRext/HPCAQ2LQ_BRCM_B.264", [0xEF0F86EC, 0x8C1C79BA, 0xFA5DFB57, 0x85C2C4FC]),
    ("FRext/Freh1_B.264", [0x852901B5, 0xC0F54669, 0xD32B9487, 0x40A2BAD9]),
    ("FRext/Freh2_B.264", [0x28432234, 0x311A2C63, 0x6FE630A8, 0x007AA51E]),
    ("FRext/freh3.264", [0x94A77E3E, 0xAE7391C7, 0x555071A9, 0xFD67FB69]),
    //("FRext/freh4.264", [0;4]), //PAFF
    //("FRext/freh5.264", [0;4]), //MBAFF
    //("FRext/freh6.264", [0;4]), //PAFF
    //("FRext/Freh7_B.264", [0;4]), //PAFF
    ("FRext/freh8.264", [0x300C9E08, 0xED69F361, 0xF46A3BF5, 0x79D37EF1]),
    ("FRext/freh9.264", [0x8B3C12A3, 0xC244D147, 0xD71A4F0E, 0x7109393D]),
    //("FRext/freh10.264", [0;4]), //PAFF
    //("FRext/freh11.264", [0;4]), //PAFF
    ("FRext/Freh12_B.264", [0x73DEE4EE, 0x216B40CB, 0xF7647E2B, 0xCD2BBBD3]),
    /*("FRext/FREXT01_JVC_D.264", [0;4]), //MBAFF
    ("FRext/FREXT02_JVC_C.264", [0;4]),*/
    ("FRext/FRExt_MMCO4_Sony_B.264", [0xD4048185, 0xA358D7E7, 0x95EA950D, 0x27C66788]),

    ("FRext/test8b43.264", [0x921C816C, 0x14170EAD, 0x03C19C5C, 0x9ED3C0A4]),
];
#[test]
fn test_h264_frext_420_8() {
    test_files(FREXT_420_8_TEST_STREAMS);
}

/*const FREXT_420_10I_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    "FRext/PPH10I1_Panasonic_A.264",
    "FRext/PPH10I2_Panasonic_A.264",
    "FRext/PPH10I3_Panasonic_A.264",
    "FRext/PPH10I4_Panasonic_A.264",
    "FRext/PPH10I5_Panasonic_A.264",
    "FRext/PPH10I6_Panasonic_A.264",
    "FRext/PPH10I7_Panasonic_A.264",
];
#[test]
fn test_h264_frext_420_10i() {
    test_files(FREXT_420_10I_TEST_STREAMS);
}*/
