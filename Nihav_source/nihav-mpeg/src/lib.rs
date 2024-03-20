//! Crate for providing support for various MPEG formats.
extern crate nihav_core;
extern crate nihav_codec_support;

#[cfg(feature="decoders")]
#[allow(clippy::collapsible_if)]
#[allow(clippy::collapsible_else_if)]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::needless_late_init)]
#[allow(clippy::manual_range_contains)]
#[allow(clippy::upper_case_acronyms)]
mod codecs;

#[cfg(feature="decoders")]
pub use crate::codecs::mpeg_register_all_decoders;
#[cfg(feature="decoders")]
pub use crate::codecs::mpeg_register_all_packetisers;
