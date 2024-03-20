//! Crate for providing support for various game-related formats.
extern crate nihav_core;
extern crate nihav_codec_support;

#[allow(clippy::collapsible_if)]
#[allow(clippy::collapsible_else_if)]
#[allow(clippy::excessive_precision)]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::manual_range_contains)]
#[allow(clippy::unreadable_literal)]
#[allow(clippy::useless_let_if_seq)]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::needless_late_init)]
mod codecs;
pub use crate::codecs::game_register_all_decoders;
#[allow(clippy::collapsible_if)]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::unreadable_literal)]
#[allow(clippy::manual_range_contains)]
#[allow(clippy::upper_case_acronyms)]
mod demuxers;
pub use crate::demuxers::game_register_all_demuxers;

mod muxers;
pub use crate::muxers::game_register_all_muxers;
