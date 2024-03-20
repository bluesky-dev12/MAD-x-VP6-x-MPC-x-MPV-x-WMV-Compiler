//! Core functionality of NihAV intended to be used by both crates implementing format support and users.
#[cfg(feature="decoders")]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::cast_lossless)]
#[allow(clippy::identity_op)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::unreadable_literal)]
pub mod codecs;

#[cfg(feature="compr")]
#[allow(clippy::manual_memcpy)]
#[allow(clippy::needless_range_loop)]
pub mod compr;

#[cfg(feature="muxers")]
pub mod muxers;

#[cfg(feature="demuxers")]
pub mod demuxers;

#[allow(clippy::needless_range_loop)]
#[allow(clippy::too_many_arguments)]
pub mod formats;
pub mod frame;
#[allow(clippy::too_many_arguments)]
pub mod io;
pub mod options;
pub mod refs;
pub mod reorder;
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::collapsible_if)]
#[allow(clippy::many_single_char_names)]
#[allow(clippy::needless_range_loop)]
#[allow(clippy::trivially_copy_pass_by_ref)]
pub mod scale;
#[allow(clippy::unreadable_literal)]
pub mod soundcvt;
