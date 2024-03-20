use nihav_core::demuxers::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DemuxerError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DemuxerError::InvalidData); } };
}

#[cfg(feature="demuxer_real")]
#[allow(clippy::identity_op)]
#[allow(clippy::needless_range_loop)]
mod realmedia;

const RM_DEMUXERS: &[&dyn DemuxerCreator] = &[
#[cfg(feature="demuxer_real")]
    &realmedia::RealMediaDemuxerCreator {},
#[cfg(feature="demuxer_real")]
    &realmedia::RealAudioDemuxerCreator {},
#[cfg(feature="demuxer_real")]
    &realmedia::RealIVRDemuxerCreator {},
];

/// Registers all available demuxers provided by this crate.
pub fn realmedia_register_all_demuxers(rd: &mut RegisteredDemuxers) {
    for demuxer in RM_DEMUXERS.iter() {
        rd.add_demuxer(*demuxer);
    }
}
