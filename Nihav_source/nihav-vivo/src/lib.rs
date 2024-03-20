//! Crate for providing support for VivoActive formats.
extern crate nihav_core;
extern crate nihav_codec_support;

#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::needless_late_init)]
mod codecs;
mod demuxers;

pub use crate::codecs::vivo_register_all_decoders;
pub use crate::demuxers::vivo_register_all_demuxers;
