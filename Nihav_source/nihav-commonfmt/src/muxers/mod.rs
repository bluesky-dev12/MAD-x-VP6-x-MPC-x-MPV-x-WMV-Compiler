use nihav_core::muxers::*;

#[cfg(feature="muxer_avi")]
mod avi;
#[cfg(feature="muxer_gif")]
mod gif;
#[cfg(feature="muxer_wav")]
mod wav;
#[cfg(feature="muxer_y4m")]
mod y4m;

const MUXERS: &[&dyn MuxerCreator] = &[
#[cfg(feature="muxer_avi")]
    &avi::AVIMuxerCreator {},
#[cfg(feature="muxer_gif")]
    &gif::GIFMuxerCreator {},
#[cfg(feature="muxer_wav")]
    &wav::WAVMuxerCreator {},
#[cfg(feature="muxer_y4m")]
    &y4m::Y4MMuxerCreator {},
];

pub fn generic_register_all_muxers(rm: &mut RegisteredMuxers) {
    for muxer in MUXERS.iter() {
        rm.add_muxer(*muxer);
    }
}
