use nihav_core::muxers::*;

#[cfg(feature="muxer_ea")]
mod ea;

const MUXERS: &[&dyn MuxerCreator] = &[
#[cfg(feature="muxer_ea")]
    &ea::EAMuxerCreator {},
];

pub fn game_register_all_muxers(rm: &mut RegisteredMuxers) {
    for muxer in MUXERS.iter() {
        rm.add_muxer(*muxer);
    }
}
