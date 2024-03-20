use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(feature="decoder_mwv1")]
mod mwv1;

const DECODERS: &[DecoderInfo] = &[
#[cfg(feature="decoder_mwv1")]
    DecoderInfo { name: "mwv1", get_decoder: mwv1::get_decoder },
];

/// Registers all available codecs provided by this crate.
pub fn misc_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in DECODERS.iter() {
        rd.add_decoder(*decoder);
    }
}

