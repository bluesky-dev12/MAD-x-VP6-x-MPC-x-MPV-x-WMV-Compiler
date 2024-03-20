//! Crate for providing support for Bink and Smacker formats.
extern crate nihav_core;
extern crate nihav_codec_support;

#[cfg(feature="decoders")]
#[allow(clippy::cast_lossless)]
#[allow(clippy::collapsible_if)]
#[allow(clippy::collapsible_else_if)]
#[allow(clippy::excessive_precision)]
#[allow(clippy::identity_op)]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::needless_late_init)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::unreadable_literal)]
#[allow(clippy::useless_let_if_seq)]
#[allow(clippy::upper_case_acronyms)]
mod codecs;
#[cfg(feature="decoders")]
pub use crate::codecs::rad_register_all_decoders;
#[cfg(feature="encoders")]
pub use crate::codecs::rad_register_all_encoders;

#[cfg(feature="demuxers")]
#[allow(clippy::comparison_chain)]
#[allow(clippy::cast_lossless)]
mod demuxers;
#[cfg(feature="demuxers")]
pub use crate::demuxers::rad_register_all_demuxers;

#[cfg(feature="muxers")]
mod muxers;
#[cfg(feature="muxers")]
pub use crate::muxers::rad_register_all_muxers;
