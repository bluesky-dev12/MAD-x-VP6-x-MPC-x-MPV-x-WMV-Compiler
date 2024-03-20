use nihav_core::codecs::RegisteredMTDecoders;
use nihav_core::demuxers::RegisteredDemuxers;
use nihav_codec_support::test::dec_video::*;
use nihav_commonfmt::generic_register_all_demuxers;
use crate::itu_register_all_mt_decoders;

use super::raw_demux::RawH264DemuxerCreator;

const PREFIX: &str = "assets/ITU/h264-conformance/";

fn test_files(names: &[(&str, [u32; 4])]) {
    let mut dmx_reg = RegisteredDemuxers::new();
    dmx_reg.add_demuxer(&RawH264DemuxerCreator{});
    generic_register_all_demuxers(&mut dmx_reg);
    let mut dec_reg = RegisteredMTDecoders::new();
    itu_register_all_mt_decoders(&mut dec_reg);

    for (name, hash) in names.iter() {
        let test_name = format!("{}{}", PREFIX, name);
        println!("Testing {}", test_name);
        test_mt_decoding("rawh264", "h264", &test_name, None, &dmx_reg, &dec_reg, ExpectedTestResult::MD5(*hash));
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
    ("BA_MW_D.264", [0x7d5d351a, 0xd0616402, 0x94bf43a4, 0x3150fbca]),
    ("BANM_MW_D.264", [0xe637d38e, 0xd004df35, 0x40218e3d, 0x84b43e42]),
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
    ("CI_MW_D.264", [0x0eb95292, 0xad9fc21c, 0x89d93f8b, 0x049e451a]),
    ("SVA_CL1_E.264", [0x5723A151, 0x8DE9FADC, 0xA7499C5B, 0xA34DA7C4]),
    ("CI1_FT_B.264", [0x16F329D0, 0x196938FD, 0xB1AB2402, 0x5B208CFD]),
    ("CVFC1_Sony_C.jsv", [0x4A8F8461, 0xD42A83C5, 0x126C4E5E, 0x5B2060D6]),
    ("AUD_MW_E.264", [0xe96fe505, 0x4de0329a, 0x8868d060, 0x03375cdb]),
    ("MIDR_MW_D.264", [0xd87bff88, 0xb2c5b96c, 0xcb291ef6, 0x8a45bbc2]),
    ("NRF_MW_E.264", [0xa8635615, 0xb50c5a16, 0xdecc555a, 0x3c6c81c8]),
    ("MPS_MW_A.264", [0x88bb5a51, 0x3bd7f3cc, 0x8190c7c0, 0x3688ab22]),
    ("CVBS3_Sony_C.jsv", [0xe3c16329, 0x88100491, 0xe8431c3c, 0x88ed4096]),
    ("BA3_SVA_C.264", [0x7032210d, 0xc0fc4a59, 0x49a2c941, 0x6fde4c27]),
    ("SL1_SVA_B.264", [0xc9d2c518, 0xca433636, 0x77e70a17, 0x213c82a2]),
    ("NL3_SVA_E.264", [0x21fa010c, 0x3c4bbb63, 0x1d17c4aa, 0xecd95df1]),
    ("cvmp_mot_frm0_full_B.26l", [0xcb065db3, 0xa27b4a52, 0xb31f6839, 0xa3ec590a]),

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
//    ("MR4_TANDBERG_C.264", [0xA40042BC, 0xAB00C341, 0xA9651725, 0x46d31A2C]), // TODO later
//    ("MR5_TANDBERG_C.264", [0x999EAE2E, 0x016DB374, 0x708B00E4, 0x335AE723]), //weird self-reference, TODO later
    ("MR1_MW_A.264", [0xdd56dc8e, 0x403b18ec, 0x57eb5b3a, 0xd834ffde]),
    ("MR2_MW_A.264", [0xe1e93e65, 0x96af2efd, 0x0e7d0fe5, 0x94d5be85]),
    /*"MR6_BT_B.h264",
    "MR7_BT_B.h264",
    "MR8_BT_B.h264",*/ // interlaced coding
    ("HCBP1_HHI_A.264", [0x13022e79, 0x70d78f1d, 0xe4aaf1f7, 0xbd0e440b]),
    ("HCBP2_HHI_A.264", [0x6c689d15, 0x41f97dcc, 0x1a17f5bd, 0xb6569cf1]),
];
#[test]
fn test_h264_mmco() {
    test_files(MMCO_TEST_STREAMS);
}

const WP_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CVWP5_TOSHIBA_E.264", [0x9663DA55, 0xE5EF516C, 0x8BF0CA0B, 0xCC0ABBB8]),
    ("CVWP1_TOSHIBA_E.264", [0xE8868CA5, 0xE934AD77, 0x9132CDB3, 0xC71BE000]),
    /*("CVWP2_TOSHIBA_E.264", [0x4ef20436, 0x093acfa5, 0xba60f9cb, 0x9e9c86d2]),
    ("CVWP3_TOSHIBA_E.264", [0x157a9a52, 0x63054bca, 0x0754e34d, 0xed250695]),*/ // negative P-frame POCs
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
    ("CVSE3_Sony_H.jsv", [0xcec17e7e, 0xbe686bfc, 0xf234dece, 0x41f59179]),
    ("CVSEFDFT3_Sony_E.jsv", [0xF44E4059, 0xD056AA37, 0x96F384A1, 0x1C894821]),
];
#[test]
fn test_h264_sei_vui() {
    test_files(SEI_VUI_TEST_STREAMS);
}

const CABAC_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CANL1_TOSHIBA_G.264", [0xAFA07274, 0x6B16BD96, 0xF3152B45, 0xE2F2881E]),
    ("CANL1_Sony_E.jsv", [0x27F1D5D3, 0x89E110FC, 0x320788BF, 0x78006DB0]),
    ("CANL2_Sony_E.jsv", [0x3A28438E, 0x3E0795DE, 0xAED795FC, 0xFEFBC833]),
    ("CANL3_Sony_C.jsv", [0xD8CE9D2F, 0xAA54CE32, 0x94AD1553, 0xC440CDE9]),
    ("CANL1_SVA_B.264", [0xB02DEFCB, 0x741C0E98, 0x2313C574, 0x9F2008ED]),
    ("CANL2_SVA_B.264", [0xB02DEFCB, 0x741C0E98, 0x2313C574, 0x9F2008ED]),
    ("CANL3_SVA_B.264", [0x04A6DE98, 0x4EF88D1B, 0x8C1B26FC, 0x8F33A425]),
    ("CANL4_SVA_B.264", [0x8F50D54B, 0x809E3B13, 0xC4F25B83, 0xDAC9715E]),
    ("CABA1_Sony_D.jsv", [0x24B155A4, 0x00DC10D1, 0x1D45A3AA, 0xDF61AE25]),
    ("CABA2_Sony_E.jsv", [0x3731F0F1, 0xACE3AD91, 0x76093A7B, 0x46347CEA]),
    ("CABA3_Sony_C.jsv", [0x28C778FD, 0xCF189AFF, 0x70095DB5, 0x2572456B]),
    ("CABA3_TOSHIBA_E.264", [0x13651D01, 0xC5B533E6, 0xB7AA132B, 0xAE7669ED]),
    ("CABA1_SVA_B.264", [0x2F5CABD5, 0xBB4954C0, 0x386CAFD8, 0xA9AA782A]),
    ("CABA2_SVA_B.264", [0x6D4277A7, 0xFC70ED1F, 0xBE3C5F10, 0xB0A70671]),
    ("CABA3_SVA_B.264", [0xA671891F, 0xACE44E55, 0x5C7CAF55, 0x94677EA8]),
    ("camp_mot_frm0_full.26l", [0xA37697DB, 0x4DC220E5, 0x53E8BFCD, 0x3BA31463]),
];
#[test]
fn test_h264_cabac() {
    test_files(CABAC_TEST_STREAMS);
}

const CABAC_INIT_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CABACI3_Sony_B.jsv", [0xB63FC9B1, 0x4CC4102C, 0xB3C09A73, 0x88E636B2]),
];
#[test]
fn test_h264_cabac_init() {
    test_files(CABAC_INIT_TEST_STREAMS);
}

const CABAC_MB_QPTEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CAQP1_Sony_B.jsv", [0xCAC07EAA, 0xBD141764, 0xD64CF9DE, 0x0230A92E]),
    ("CACQP3_Sony_D.jsv", [0xDFC2C76E, 0x559E61C0, 0xE3E29220, 0x05DC805E]),
];
#[test]
fn test_h264_cabac_mb_qp() {
    test_files(CABAC_MB_QPTEST_STREAMS);
}

const CABAC_SLICE_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CABAST3_Sony_E.jsv", [0xB4797DBC, 0x3CD95E50, 0x2C04F2DE, 0x629C61BA]),
    ("CABASTBR3_Sony_B.jsv", [0xF8081465, 0xA02CF3C3, 0xC678671A, 0xC456D62C]),
];
#[test]
fn test_h264_cabac_slice() {
    test_files(CABAC_SLICE_TEST_STREAMS);
}

const CABAC_I_PCM_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("CAPCMNL1_Sand_E.264", [0xEE9968EE, 0xEFE935F0, 0x45C6B70B, 0xE51691EB]),
    ("CAPCM1_Sand_E.264", [0xCA073CA1, 0x06E70D5C, 0xD51F6748, 0x5846A5B1]),
    ("CAPM3_Sony_D.jsv", [0x9ECC3BF5, 0xFF7CAC9A, 0x068A5BA5, 0x7BC87CB7]),
];
#[test]
fn test_h264_cabac_ipcm() {
    test_files(CABAC_I_PCM_TEST_STREAMS);
}

const CABAC_MMCO_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    /*"MR9_BT_B.h264",*/ //MBAFF
    ("HCMP1_HHI_A.264", [0xF1550F70, 0x6762E865, 0x29FE9204, 0x7981C250]),
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
    ("src19td.IBP.264", [0xEE593F70, 0x57480500, 0xCE7D8768, 0xF1AA0E41]),
];
#[test]
fn test_h264_cabac_pred_bw() {
    test_files(CABAC_PRED_BW_TEST_STREAMS);
}

const FREXT_420_8_TEST_STREAMS: &[(&str, [u32; 4])] = &[
    ("FRext/FRExt1_Panasonic.avc", [0x224897db, 0xcb44b3a4, 0x09f779f1, 0x4ed4af76]),
    ("FRext/FRExt3_Panasonic.avc", [0xC6AB33FD, 0xCC18BC98, 0x7FBF8B2C, 0xD240036D]),
    ("FRext/HCAFR1_HHI.264", [0x662BB873, 0x3085753A, 0xC5E0D55E, 0x1D1A4A09]),
    //("FRext/HCAFF1_HHI.264", [0;4]), //PAFF
    //("FRext/HCAMFF1_HHI.264", [0;4]), //MBAFF
    //("FRext/FRExt2_Panasonic.avc", [0;4]), //PAFF
    //("FRext/FRExt4_Panasonic.avc", [0;4]), //MBAFF
    ("FRext/HPCANL_BRCM_C.264", [0xB1660F91, 0xE5047ADA, 0xAE5204A5, 0x309D57B8]),
    ("FRext/HPCA_BRCM_C.264", [0x7F14A1E8, 0x39AD8B19, 0xAE5B8E17, 0x6E3989A3]),

    /*("FRext/HPCAFLNL_BRCM_C.264", [0;4]), //PAFF
    ("FRext/HPCAFL_BRCM_C.264", [0;4]),*/
    ("FRext/HCAFR2_HHI.264", [0x63D67A2A, 0x105325E9, 0x20DB4882, 0x3BCA5E0B]),
    ("FRext/HCAFR3_HHI.264", [0xead8442e, 0xc7c92029, 0xb4308393, 0x04429e08]),
    ("FRext/HCAFR4_HHI.264", [0xe3c8636e, 0x4a39d44b, 0x37c008be, 0x055f023f]),
    ("FRext/HPCADQ_BRCM_B.264", [0xbc418315, 0x190b9fbc, 0xf26b2b67, 0x74ec9e0c]),
    ("FRext/HPCALQ_BRCM_B.264", [0xbc418315, 0x190b9fbc, 0xf26b2b67, 0x74ec9e0c]),
    //("FRext/HPCAMAPALQ_BRCM_B.264", [0;4]), //MBAFF
    ("FRext/HPCV_BRCM_A.264", [0x2c898d3b, 0xd5a0ce47, 0x59056977, 0x0efa615c]),
    ("FRext/HPCVNL_BRCM_A.264", [0x5c03fbee, 0x3197c054, 0xd9bb8998, 0xc7ad74c0]),
    /*("FRext/HPCVFL_BRCM_A.264", [0;4]), //PAFF
    ("FRext/HPCVFLNL_BRCM_A.264", [0;4]),*/
    //("FRext/HPCVMOLQ_BRCM_B.264", [0;4]), //grayscale
    //("FRext/HPCAMOLQ_BRCM_B.264", [0;4]), //grayscale
    ("FRext/HPCAQ2LQ_BRCM_B.264", [0x0548d695, 0x187a2dd9, 0x4019c881, 0xd50c37fe]),
    ("FRext/Freh1_B.264", [0xdcbbcad3, 0xe236a00b, 0xe1634ab4, 0x10e18346]),
    ("FRext/Freh2_B.264", [0x016d6d3f, 0xe4592072, 0x28352500, 0xd2997d1b]),
    ("FRext/freh3.264", [0x1ec34cc7, 0x284a8778, 0x1a6fa64b, 0x71788926]),
    //("FRext/freh4.264", [0;4]), //PAFF
    //("FRext/freh5.264", [0;4]), //MBAFF
    //("FRext/freh6.264", [0;4]), //PAFF
    //("FRext/Freh7_B.264", [0;4]), //PAFF
    ("FRext/freh8.264", [0x0be92564, 0x2ad3dbf6, 0xda89d9b6, 0xeebe66e3]),
    ("FRext/freh9.264", [0xec630029, 0x953c309d, 0xa8813a35, 0x027fae05]),
    //("FRext/freh10.264", [0;4]), //PAFF
    //("FRext/freh11.264", [0;4]), //PAFF
    ("FRext/Freh12_B.264", [0xa78649ab, 0x5d909a25, 0xf24e2ac6, 0xf6381467]),
    /*("FRext/FREXT01_JVC_D.264", [0;4]), //MBAFF
    ("FRext/FREXT02_JVC_C.264", [0;4]),*/
    ("FRext/FRExt_MMCO4_Sony_B.264", [0x47be1aa9, 0x61b2cc22, 0x83e55893, 0x696693b5]),

    ("FRext/test8b43.264", [0x921c816c, 0x14170ead, 0x03c19c5c, 0x9ed3c0a4]),
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
