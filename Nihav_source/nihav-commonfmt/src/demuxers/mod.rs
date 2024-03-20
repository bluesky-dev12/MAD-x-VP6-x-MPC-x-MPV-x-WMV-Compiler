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

#[cfg(feature="demuxer_avi")]
#[allow(clippy::cast_lossless)]
mod avi;
#[cfg(feature="demuxer_gif")]
mod gif;
#[cfg(feature="demuxer_mov")]
#[allow(clippy::cast_lossless)]
mod mov;
#[cfg(feature="demuxer_wav")]
mod wav;
#[cfg(feature="demuxer_y4m")]
mod y4m;

const DEMUXERS: &[&dyn DemuxerCreator] = &[
#[cfg(feature="demuxer_avi")]
    &avi::AVIDemuxerCreator {},
#[cfg(feature="demuxer_gif")]
    &gif::GIFDemuxerCreator {},
#[cfg(feature="demuxer_mov")]
    &mov::MOVDemuxerCreator {},
#[cfg(feature="demuxer_mov")]
    &mov::MacBinaryMOVDemuxerCreator {},
#[cfg(feature="demuxer_wav")]
    &wav::WAVDemuxerCreator {},
#[cfg(feature="demuxer_y4m")]
    &y4m::Y4MDemuxerCreator {},
];

/// Registers all available demuxers provided by this crate.
pub fn generic_register_all_demuxers(rd: &mut RegisteredDemuxers) {
    for demuxer in DEMUXERS.iter() {
        rd.add_demuxer(*demuxer);
    }
}
