//! Crate for providing support for various common formats.
extern crate nihav_core;
extern crate nihav_codec_support;
extern crate nihav_registry;

#[cfg(any(feature="decoders", feature="encoders"))]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::single_match)]
#[allow(clippy::unreadable_literal)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::excessive_precision)]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::manual_range_contains)]
#[allow(clippy::collapsible_else_if)]
mod codecs;

#[cfg(feature="decoders")]
pub use crate::codecs::generic_register_all_decoders;
#[cfg(feature="encoders")]
pub use crate::codecs::generic_register_all_encoders;

#[cfg(feature="demuxers")]
mod demuxers;
#[cfg(feature="demuxers")]
pub use crate::demuxers::generic_register_all_demuxers;

#[cfg(feature="muxers")]
mod muxers;
#[cfg(feature="muxers")]
pub use crate::muxers::generic_register_all_muxers;

#[cfg(test)]
extern crate nihav_realmedia;
