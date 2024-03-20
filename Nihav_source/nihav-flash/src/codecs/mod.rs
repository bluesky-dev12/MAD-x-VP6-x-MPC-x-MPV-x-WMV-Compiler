use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(feature="decoder_flv263")]
mod flv263;
#[cfg(feature="decoder_flashsv")]
mod flashsv;

#[cfg(feature="decoder_flv_adpcm")]
mod adpcm;
#[cfg(feature="decoder_asao")]
mod asao;

#[cfg(feature="decoders")]
const DECODERS: &[DecoderInfo] = &[
#[cfg(feature="decoder_flv263")]
    DecoderInfo { name: "flv263", get_decoder: flv263::get_decoder },
#[cfg(feature="decoder_flashsv")]
    DecoderInfo { name: "flashsv", get_decoder: flashsv::get_decoder },
#[cfg(feature="decoder_flashsv")]
    DecoderInfo { name: "flashsv2", get_decoder: flashsv::get_decoder_v2 },

#[cfg(feature="decoder_flv_adpcm")]
    DecoderInfo { name: "flv-adpcm", get_decoder: adpcm::get_decoder },
#[cfg(feature="decoder_asao")]
    DecoderInfo { name: "asao", get_decoder: asao::get_decoder },
];

/// Registers all available codecs provided by this crate.
#[cfg(feature="decoders")]
pub fn flash_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in DECODERS.iter() {
        rd.add_decoder(*decoder);
    }
}

#[cfg(feature="encoder_flv_adpcm")]
mod adpcmenc;

#[cfg(feature="encoders")]
const ENCODERS: &[EncoderInfo] = &[
#[cfg(feature="encoder_flv_adpcm")]
    EncoderInfo { name: "flv-adpcm", get_encoder: adpcmenc::get_encoder },
];

/// Registers all available encoders provided by this crate.
#[cfg(feature="encoders")]
pub fn flash_register_all_encoders(re: &mut RegisteredEncoders) {
    for encoder in ENCODERS.iter() {
        re.add_encoder(*encoder);
    }
}
