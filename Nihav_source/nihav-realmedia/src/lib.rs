//! Crate for providing support for RealMedia formats.
extern crate nihav_core;
extern crate nihav_codec_support;

#[cfg(any(feature="decoders", feature="encoders"))]
#[allow(clippy::cast_lossless)]
#[allow(clippy::collapsible_if)]
#[allow(clippy::collapsible_else_if)]
#[allow(clippy::comparison_chain)]
#[allow(clippy::excessive_precision)]
#[allow(clippy::identity_op)]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::single_match)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::unreadable_literal)]
#[allow(clippy::useless_let_if_seq)]
#[allow(clippy::needless_late_init)]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::manual_range_contains)]
#[allow(clippy::wrong_self_convention)]
mod codecs;
#[cfg(feature="decoders")]
pub use crate::codecs::realmedia_register_all_decoders;
#[cfg(feature="encoders")]
pub use crate::codecs::realmedia_register_all_encoders;

#[cfg(feature="demuxers")]
#[allow(clippy::cast_lossless)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::unreadable_literal)]
#[allow(clippy::useless_let_if_seq)]
#[allow(clippy::needless_late_init)]
#[allow(clippy::upper_case_acronyms)]
mod demuxers;
#[cfg(feature="demuxers")]
pub use crate::demuxers::realmedia_register_all_demuxers;

#[cfg(feature="muxers")]
mod muxers;
#[cfg(feature="muxers")]
pub use crate::muxers::realmedia_register_all_muxers;
