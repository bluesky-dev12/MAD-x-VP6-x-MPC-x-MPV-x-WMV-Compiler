use nihav_core::codecs::*;

#[allow(unused_macros)]
#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(feature="decoder_arxel_vid")]
pub mod arxel_vid;
#[cfg(any(feature="decoder_beam_vbv",feature="decoder_beam_fcp"))]
pub mod beam;
#[cfg(feature="decoder_bmv")]
pub mod bmv;
#[cfg(feature="decoder_bmv3")]
pub mod bmv3;
#[cfg(any(feature="decoder_fstvid",feature="decoder_fstaud"))]
pub mod futurevision;
#[cfg(feature="decoder_gdvvid")]
pub mod gremlinvideo;
#[cfg(feature="decoder_hl_fmv")]
pub mod hl_fmv;
#[cfg(feature="decoder_imax")]
pub mod imax;
#[cfg(feature="decoder_ipma")]
pub mod ipma;
#[cfg(feature="decoder_lhst500f22")]
pub mod lhst500f22;
#[cfg(feature="decoder_midivid")]
pub mod midivid;
#[cfg(feature="decoder_midivid3")]
pub mod midivid3;
#[cfg(feature="decoder_q")]
pub mod q;
#[cfg(feature="decoder_sga")]
pub mod sga;
#[cfg(any(feature="decoder_smush_video", feature="decoder_smush_audio"))]
pub mod smush;
#[cfg(feature="decoder_vmd")]
pub mod vmd;
#[cfg(feature="decoder_vx")]
#[allow(clippy::erasing_op)]
#[allow(clippy::identity_op)]
pub mod vx;

const GAME_CODECS: &[DecoderInfo] = &[
#[cfg(feature="decoder_gdvvid")]
    DecoderInfo { name: "gdv-audio", get_decoder: gremlinvideo::get_decoder_audio },
#[cfg(feature="decoder_gdvvid")]
    DecoderInfo { name: "gdv-video", get_decoder: gremlinvideo::get_decoder_video },
#[cfg(feature="decoder_arxel_vid")]
    DecoderInfo { name: "arxel-video", get_decoder: arxel_vid::get_decoder },
#[cfg(feature="decoder_beam_fcp")]
    DecoderInfo { name: "beam-fcp", get_decoder: beam::get_decoder_fcp },
#[cfg(feature="decoder_beam_vbv")]
    DecoderInfo { name: "beam-video", get_decoder: beam::get_decoder_vbv },
#[cfg(feature="decoder_bmv")]
    DecoderInfo { name: "bmv-audio", get_decoder: bmv::get_decoder_audio },
#[cfg(feature="decoder_bmv")]
    DecoderInfo { name: "bmv-video", get_decoder: bmv::get_decoder_video },
#[cfg(feature="decoder_bmv3")]
    DecoderInfo { name: "bmv3-audio", get_decoder: bmv3::get_decoder_audio },
#[cfg(feature="decoder_bmv3")]
    DecoderInfo { name: "bmv3-video", get_decoder: bmv3::get_decoder_video },
#[cfg(feature="decoder_fstaud")]
    DecoderInfo { name: "fst-audio", get_decoder: futurevision::get_decoder_audio },
#[cfg(feature="decoder_fstvid")]
    DecoderInfo { name: "fst-video", get_decoder: futurevision::get_decoder_video },
#[cfg(feature="decoder_hl_fmv")]
    DecoderInfo { name: "hl-fmv-video", get_decoder: hl_fmv::get_decoder },
#[cfg(feature="decoder_imax")]
    DecoderInfo { name: "fable-imax", get_decoder: imax::get_decoder },
#[cfg(feature="decoder_ipma")]
    DecoderInfo { name: "ipma", get_decoder: ipma::get_decoder },
#[cfg(feature="decoder_ipma")]
    DecoderInfo { name: "ipma2", get_decoder: ipma::get_decoder_v2 },
#[cfg(feature="decoder_q")]
    DecoderInfo { name: "legend-q-video", get_decoder: q::get_decoder },
#[cfg(feature="decoder_sga")]
    DecoderInfo { name: "dp-sga", get_decoder: sga::get_decoder },
#[cfg(feature="decoder_smush_video")]
    DecoderInfo { name: "smushv1", get_decoder: smush::get_decoder_video_v1 },
#[cfg(feature="decoder_smush_video")]
    DecoderInfo { name: "smushv2", get_decoder: smush::get_decoder_video_v2 },
#[cfg(feature="decoder_smush_audio")]
    DecoderInfo { name: "smush-iact", get_decoder: smush::get_decoder_iact },
#[cfg(feature="decoder_smush_audio")]
    DecoderInfo { name: "smush-vima", get_decoder: smush::get_decoder_vima },
#[cfg(feature="decoder_vmd")]
    DecoderInfo { name: "vmd-audio", get_decoder: vmd::get_decoder_audio },
#[cfg(feature="decoder_vmd")]
    DecoderInfo { name: "vmd-video", get_decoder: vmd::get_decoder_video },
#[cfg(feature="decoder_lhst500f22")]
    DecoderInfo { name: "lhst500f22", get_decoder: lhst500f22::get_decoder },
#[cfg(feature="decoder_midivid")]
    DecoderInfo { name: "midivid", get_decoder: midivid::get_decoder_video },
#[cfg(feature="decoder_midivid3")]
    DecoderInfo { name: "midivid3", get_decoder: midivid3::get_decoder_video },
#[cfg(feature="decoder_vx")]
    DecoderInfo { name: "vxaudio", get_decoder: vx::get_decoder_audio },
#[cfg(feature="decoder_vx")]
    DecoderInfo { name: "vxvideo", get_decoder: vx::get_decoder_video },
];

/// Registers all available codecs provided by this crate.
pub fn game_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in GAME_CODECS.iter() {
        rd.add_decoder(*decoder);
    }
}
