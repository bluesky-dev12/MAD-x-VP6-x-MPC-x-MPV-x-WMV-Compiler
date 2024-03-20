use nihav_core::muxers::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(MuxerError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(MuxerError::InvalidData); } };
}

#[cfg(feature="muxer_real")]
mod rmvb;

const MUXERS: &[&dyn MuxerCreator] = &[
#[cfg(feature="muxer_real")]
    &rmvb::RealMediaMuxerCreator {},
#[cfg(feature="muxer_real")]
    &rmvb::RealAudioMuxerCreator {},
];

pub fn realmedia_register_all_muxers(rm: &mut RegisteredMuxers) {
    for muxer in MUXERS.iter() {
        rm.add_muxer(*muxer);
    }
}
