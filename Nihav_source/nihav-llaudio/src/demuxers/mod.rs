use nihav_core::demuxers::*;

#[allow(unused_macros)]
#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DemuxerError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DemuxerError::InvalidData); } };
}

#[cfg(feature="demuxer_ape")]
mod ape;
#[cfg(feature="demuxer_flac")]
mod flac;
#[cfg(feature="demuxer_flac")]
mod flacraw;
#[cfg(feature="demuxer_tta")]
mod tta;
#[cfg(feature="demuxer_wavpack")]
mod wavpack;

const LL_AUDIO_DEMUXERS: &[&dyn DemuxerCreator] = &[
#[cfg(feature="demuxer_ape")]
    &ape::APEDemuxerCreator {},
#[cfg(feature="demuxer_flac")]
    &flac::FLACDemuxerCreator {},
#[cfg(feature="demuxer_tta")]
    &tta::TTADemuxerCreator {},
#[cfg(feature="demuxer_wavpack")]
    &wavpack::WavPackDemuxerCreator {},
];

/// Registers all available demuxers provided by this crate.
pub fn llaudio_register_all_demuxers(rd: &mut RegisteredDemuxers) {
    for demuxer in LL_AUDIO_DEMUXERS.iter() {
        rd.add_demuxer(*demuxer);
    }
}

const LL_RAW_AUDIO_DEMUXERS: &[&dyn RawDemuxerCreator] = &[
#[cfg(feature="demuxer_flac")]
    &flacraw::FLACDemuxerCreator {},
];

/// Registers all available raw stream demuxers provided by this crate.
pub fn llaudio_register_all_raw_demuxers(rd: &mut RegisteredRawDemuxers) {
    for demuxer in LL_RAW_AUDIO_DEMUXERS.iter() {
        rd.add_demuxer(*demuxer);
    }
}
