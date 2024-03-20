use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(feature="decoder_intel263")]
mod intel263;
#[cfg(feature="decoder_indeo2")]
mod indeo2;
#[cfg(feature="decoder_indeo3")]
mod indeo3;
#[cfg(any(feature="decoder_indeo3", feature="encoder_indeo3"))]
mod indeo3data;
#[cfg(feature="decoder_indeo4")]
mod indeo4;
#[cfg(feature="decoder_indeo5")]
mod indeo5;

#[cfg(any(feature="decoder_indeo4", feature="decoder_indeo5"))]
mod ivi;
#[cfg(any(feature="decoder_indeo4", feature="decoder_indeo5"))]
mod ivibr;
#[cfg(any(feature="decoder_indeo4", feature="decoder_indeo5"))]
#[allow(clippy::erasing_op)]
mod ividsp;

#[cfg(feature="decoder_imc")]
#[allow(clippy::excessive_precision)]
#[allow(clippy::unreadable_literal)]
mod imc;

const INDEO_CODECS: &[DecoderInfo] = &[
#[cfg(feature="decoder_indeo2")]
    DecoderInfo { name: "indeo2", get_decoder: indeo2::get_decoder },
#[cfg(feature="decoder_indeo3")]
    DecoderInfo { name: "indeo3", get_decoder: indeo3::get_decoder },
#[cfg(feature="decoder_indeo4")]
    DecoderInfo { name: "indeo4", get_decoder: indeo4::get_decoder },
#[cfg(feature="decoder_indeo5")]
    DecoderInfo { name: "indeo5", get_decoder: indeo5::get_decoder },
#[cfg(feature="decoder_indeo5")]
    DecoderInfo { name: "indeo5s", get_decoder: indeo5::get_decoder_scalable },
#[cfg(feature="decoder_intel263")]
    DecoderInfo { name: "intel263", get_decoder: intel263::get_decoder },

#[cfg(feature="decoder_imc")]
    DecoderInfo { name: "imc", get_decoder: imc::get_decoder_imc },
#[cfg(feature="decoder_imc")]
    DecoderInfo { name: "iac", get_decoder: imc::get_decoder_iac },
];

/// Registers all available codecs provided by this crate.
pub fn indeo_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in INDEO_CODECS.iter() {
        rd.add_decoder(*decoder);
    }
}

#[cfg(feature="encoder_indeo3")]
mod indeo3enc;

const INDEO_ENCODERS: &[EncoderInfo] = &[
#[cfg(feature="encoder_indeo3")]
    EncoderInfo { name: "indeo3", get_encoder: indeo3enc::get_encoder },
];

/// Registers all available encoders provided by this crate.
pub fn indeo_register_all_encoders(re: &mut RegisteredEncoders) {
    for encoder in INDEO_ENCODERS.iter() {
        re.add_encoder(*encoder);
    }
}

