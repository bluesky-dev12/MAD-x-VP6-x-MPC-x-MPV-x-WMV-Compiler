//! Crate for providing support for various QuickTime formats.
extern crate nihav_core;
extern crate nihav_codec_support;

#[allow(clippy::comparison_chain)]
#[allow(clippy::single_match)]
#[allow(clippy::field_reassign_with_default)]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::collapsible_else_if)]
mod codecs;
pub use crate::codecs::qt_register_all_decoders;
