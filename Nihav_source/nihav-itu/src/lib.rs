//! Crate for providing support for ITU codecs.
extern crate nihav_core;
extern crate nihav_codec_support;

#[allow(clippy::collapsible_if)]
#[allow(clippy::comparison_chain)]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::useless_let_if_seq)]
mod codecs;
pub use crate::codecs::itu_register_all_decoders;
pub use crate::codecs::itu_register_all_mt_decoders;

#[cfg(test)]
extern crate nihav_commonfmt;
