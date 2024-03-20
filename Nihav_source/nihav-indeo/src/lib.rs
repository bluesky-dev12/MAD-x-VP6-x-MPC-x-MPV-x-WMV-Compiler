//! Crate for providing support for Intel multimedia formats.
extern crate nihav_core;
extern crate nihav_codec_support;

#[allow(clippy::collapsible_if)]
#[allow(clippy::collapsible_else_if)]
#[allow(clippy::identity_op)]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::needless_late_init)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::useless_let_if_seq)]
#[allow(clippy::verbose_bit_mask)]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::manual_range_contains)]
mod codecs;

pub use crate::codecs::indeo_register_all_decoders;
pub use crate::codecs::indeo_register_all_encoders;

mod demuxers;

pub use crate::demuxers::indeo_register_all_demuxers;

#[cfg(test)]
extern crate nihav_commonfmt;