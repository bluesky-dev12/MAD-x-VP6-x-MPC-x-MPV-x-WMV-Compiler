use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[allow(clippy::collapsible_else_if)]
#[allow(clippy::manual_range_contains)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::upper_case_acronyms)]
#[cfg(feature="decoder_h264")]
mod h264;

const ITU_CODECS: &[DecoderInfo] = &[
#[cfg(feature="decoder_h264")]
    DecoderInfo { name: "h264", get_decoder: h264::get_decoder },
];

/// Registers all available codecs provided by this crate.
pub fn itu_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in ITU_CODECS.iter() {
        rd.add_decoder(*decoder);
    }
}

const ITU_MT_CODECS: &[MTDecoderInfo] = &[
#[cfg(feature="decoder_h264")]
    MTDecoderInfo { name: "h264", get_decoder: h264::get_decoder_mt },
];

/// Registers all available multi-threaded decoders provided by this crate.
pub fn itu_register_all_mt_decoders(rd: &mut RegisteredMTDecoders) {
    for decoder in ITU_MT_CODECS.iter() {
        rd.add_decoder(*decoder);
    }
}
