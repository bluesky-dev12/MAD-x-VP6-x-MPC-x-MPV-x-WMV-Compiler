//! Crate for providing support for various Flash-related formats.
extern crate nihav_core;
extern crate nihav_codec_support;

#[cfg(any(feature="decoders", feature="encoders"))]
mod codecs;

#[cfg(feature="decoders")]
pub use crate::codecs::flash_register_all_decoders;

#[cfg(feature="demuxers")]
mod demuxers;
#[cfg(feature="demuxers")]
pub use crate::demuxers::flash_register_all_demuxers;

#[cfg(feature="encoders")]
pub use crate::codecs::flash_register_all_encoders;

#[cfg(feature="muxers")]
mod muxers;
#[cfg(feature="muxers")]
pub use crate::muxers::flash_register_all_muxers;
