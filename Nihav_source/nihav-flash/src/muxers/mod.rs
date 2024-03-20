use nihav_core::muxers::*;

#[allow(unused_macros)]
#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(MuxerError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(MuxerError::InvalidData); } };
}

#[cfg(feature="muxer_flv")]
mod flv;

const MUXERS: &[&dyn MuxerCreator] = &[
#[cfg(feature="muxer_flv")]
    &flv::FLVMuxerCreator {},
];

pub fn flash_register_all_muxers(rm: &mut RegisteredMuxers) {
    for muxer in MUXERS.iter() {
        rm.add_muxer(*muxer);
    }
}
