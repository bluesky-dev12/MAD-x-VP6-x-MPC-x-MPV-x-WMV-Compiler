use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(any(feature="decoder_realvideo3", feature="decoder_realvideo4"))]
mod rv3040;
#[cfg(any(feature="decoder_realvideo3", feature="decoder_realvideo4", feature="encoder_realvideo4"))]
#[allow(clippy::erasing_op)]
mod rv34codes;
#[cfg(any(feature="decoder_realvideo3", feature="decoder_realvideo4"))]
#[allow(clippy::erasing_op)]
#[allow(clippy::many_single_char_names)]
mod rv34dsp;

#[cfg(feature="decoder_realvideo1")]
pub mod rv10;
#[cfg(feature="decoder_realvideo2")]
pub mod rv20;
#[cfg(feature="decoder_realvideo3")]
pub mod rv30;
#[cfg(feature="decoder_realvideo3")]
#[allow(clippy::erasing_op)]
#[allow(clippy::many_single_char_names)]
#[allow(clippy::neg_multiply)]
pub mod rv30dsp;
#[cfg(feature="decoder_realvideo4")]
pub mod rv40;
#[cfg(any(feature="decoder_realvideo4", feature="encoder_realvideo4"))]
pub mod rv40data;
#[cfg(feature="decoder_realvideo4")]
#[allow(clippy::erasing_op)]
#[allow(clippy::many_single_char_names)]
pub mod rv40dsp;
#[cfg(feature="decoder_realvideo6")]
pub mod rv60;
#[cfg(feature="decoder_realvideo6")]
pub mod rv60codes;
#[cfg(feature="decoder_realvideo6")]
#[allow(clippy::erasing_op)]
#[allow(clippy::many_single_char_names)]
pub mod rv60dsp;

#[cfg(feature="decoder_realaudio144")]
#[allow(clippy::manual_memcpy)]
pub mod ra144;
#[cfg(feature="decoder_realaudio288")]
pub mod ra288;
#[cfg(feature="decoder_cook")]
pub mod cook;
#[cfg(any(feature="decoder_cook", feature="encoder_cook"))]
pub mod cookdata;
#[cfg(feature="decoder_ralf")]
pub mod ralf;

const RM_CODECS: &[DecoderInfo] = &[
#[cfg(feature="decoder_realvideo1")]
    DecoderInfo { name: "realvideo1", get_decoder: rv10::get_decoder },
#[cfg(feature="decoder_realvideo2")]
    DecoderInfo { name: "realvideo2", get_decoder: rv20::get_decoder },
#[cfg(feature="decoder_realvideo3")]
    DecoderInfo { name: "realvideo3", get_decoder: rv30::get_decoder },
#[cfg(feature="decoder_realvideo4")]
    DecoderInfo { name: "realvideo4", get_decoder: rv40::get_decoder },
#[cfg(feature="decoder_realvideo6")]
    DecoderInfo { name: "realvideo6", get_decoder: rv60::get_decoder },

#[cfg(feature="decoder_realaudio144")]
    DecoderInfo { name: "ra14.4", get_decoder: ra144::get_decoder },
#[cfg(feature="decoder_realaudio288")]
    DecoderInfo { name: "ra28.8", get_decoder: ra288::get_decoder },
#[cfg(feature="decoder_cook")]
    DecoderInfo { name: "cook", get_decoder: cook::get_decoder },
#[cfg(feature="decoder_ralf")]
    DecoderInfo { name: "ralf", get_decoder: ralf::get_decoder },
];

/// Registers all available codecs provided by this crate.
pub fn realmedia_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in RM_CODECS.iter() {
        rd.add_decoder(*decoder);
    }
}

#[cfg(feature="encoder_cook")]
mod cookenc;

#[cfg(feature="encoder_rv40")]
mod rv40enc;

#[cfg(feature="encoders")]
const ENCODERS: &[EncoderInfo] = &[
#[cfg(feature="encoder_cook")]
    EncoderInfo { name: "cook", get_encoder: cookenc::get_encoder },

#[cfg(feature="encoder_rv40")]
    EncoderInfo { name: "realvideo4", get_encoder: rv40enc::get_encoder },
];

/// Registers all available encoders provided by this crate.
#[cfg(feature="encoders")]
pub fn realmedia_register_all_encoders(re: &mut RegisteredEncoders) {
    for encoder in ENCODERS.iter() {
        re.add_encoder(*encoder);
    }
}
