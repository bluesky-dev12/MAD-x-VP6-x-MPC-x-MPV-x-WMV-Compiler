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

#[cfg(feature="demuxer_flv")]
mod flv;

const DEMUXERS: &[&dyn DemuxerCreator] = &[
#[cfg(feature="demuxer_flv")]
    &flv::FLVDemuxerCreator {},
];

/// Registers all available demuxers provided by this crate.
pub fn flash_register_all_demuxers(rd: &mut RegisteredDemuxers) {
    for demuxer in DEMUXERS.iter() {
        rd.add_demuxer(*demuxer);
    }
}
