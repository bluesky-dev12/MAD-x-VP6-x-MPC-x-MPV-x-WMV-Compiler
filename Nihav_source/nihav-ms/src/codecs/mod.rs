use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(feature="decoder_msrle")]
pub mod msrle;

#[cfg(feature="decoder_msvideo1")]
pub mod msvideo1;

#[cfg(feature="decoder_ima_adpcm_ms")]
pub mod imaadpcm;

#[cfg(feature="encoder_ima_adpcm_ms")]
pub mod imaadpcmenc;

#[cfg(any(feature="decoder_ms_adpcm", feature="encoder_ms_adpcm"))]
pub mod msadpcm;

const MS_CODECS: &[DecoderInfo] = &[
#[cfg(feature="decoder_msrle")]
    DecoderInfo { name: "msrle", get_decoder: msrle::get_decoder },
#[cfg(feature="decoder_msvideo1")]
    DecoderInfo { name: "msvideo1", get_decoder: msvideo1::get_decoder },
#[cfg(feature="decoder_ima_adpcm_ms")]
    DecoderInfo { name: "ima-adpcm-ms", get_decoder: imaadpcm::get_decoder },
#[cfg(feature="decoder_ms_adpcm")]
    DecoderInfo { name: "ms-adpcm", get_decoder: msadpcm::get_decoder },
];

/// Registers all available codecs provided by this crate.
pub fn ms_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in MS_CODECS.iter() {
        rd.add_decoder(*decoder);
    }
}

#[cfg(feature="encoder_msvideo1")]
#[allow(clippy::collapsible_else_if)]
pub mod msvideo1enc;

const MS_ENCODERS: &[EncoderInfo] = &[
#[cfg(feature="encoder_msvideo1")]
    EncoderInfo { name: "msvideo1", get_encoder: msvideo1enc::get_encoder },
#[cfg(feature="encoder_ms_adpcm")]
    EncoderInfo { name: "ms-adpcm", get_encoder: msadpcm::get_encoder },
#[cfg(feature="encoder_ima_adpcm_ms")]
    EncoderInfo { name: "ima-adpcm-ms", get_encoder: imaadpcmenc::get_encoder },
];

/// Registers all available encoders provided by this crate.
pub fn ms_register_all_encoders(re: &mut RegisteredEncoders) {
    for encoder in MS_ENCODERS.iter() {
        re.add_encoder(*encoder);
    }
}
