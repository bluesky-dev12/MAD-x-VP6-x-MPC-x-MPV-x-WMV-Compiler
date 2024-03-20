use nihav_core::muxers::*;

#[cfg(feature="muxer_flac")]
mod flac;

const MUXERS: &[&dyn MuxerCreator] = &[
#[cfg(feature="muxer_flac")]
    &flac::FLACMuxerCreator {},
];

pub fn llaudio_register_all_muxers(rm: &mut RegisteredMuxers) {
    for muxer in MUXERS.iter() {
        rm.add_muxer(*muxer);
    }
}
