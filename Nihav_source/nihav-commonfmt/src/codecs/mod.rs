use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(feature="decoder_cinepak")]
mod cinepak;
#[cfg(feature="decoder_clearvideo")]
mod clearvideo;
#[cfg(feature="decoder_gif")]
mod gif;
#[cfg(feature="decoder_jpeg")]
mod jpeg;
#[cfg(feature="decoder_rawvideo")]
mod rawvideo;
#[cfg(feature="decoder_rawvideo_ms")]
mod rawvideo_ms;
#[cfg(feature="decoder_zmbv")]
mod zmbv;

#[cfg(feature="decoder_atrac3")]
#[allow(clippy::identity_op)]
#[allow(clippy::useless_let_if_seq)]
mod atrac3;
#[cfg(any(feature="decoder_pcm",feature="encoder_pcm"))]
mod pcm;
#[cfg(feature="decoder_sipro")]
#[allow(clippy::collapsible_if)]
#[allow(clippy::collapsible_else_if)]
#[allow(clippy::identity_op)]
#[allow(clippy::manual_memcpy)]
mod sipro;
#[cfg(feature="decoder_ts102366")]
mod ts102366;

#[cfg(feature="decoders")]
const DECODERS: &[DecoderInfo] = &[
#[cfg(feature="decoder_cinepak")]
    DecoderInfo { name: "cinepak", get_decoder: cinepak::get_decoder },
#[cfg(feature="decoder_clearvideo")]
    DecoderInfo { name: "clearvideo", get_decoder: clearvideo::get_decoder },
#[cfg(feature="decoder_clearvideo")]
    DecoderInfo { name: "clearvideo_rm", get_decoder: clearvideo::get_decoder_rm },
#[cfg(feature="decoder_gif")]
    DecoderInfo { name: "gif", get_decoder: gif::get_decoder },
#[cfg(feature="decoder_jpeg")]
    DecoderInfo { name: "jpeg", get_decoder: jpeg::get_decoder },
#[cfg(feature="decoder_rawvideo")]
    DecoderInfo { name: "rawvideo", get_decoder: rawvideo::get_decoder },
#[cfg(feature="decoder_rawvideo_ms")]
    DecoderInfo { name: "rawvideo-ms", get_decoder: rawvideo_ms::get_decoder },
#[cfg(feature="decoder_zmbv")]
    DecoderInfo { name: "zmbv", get_decoder: zmbv::get_decoder },

#[cfg(feature="decoder_pcm")]
    DecoderInfo { name: "pcm", get_decoder: pcm::get_decoder },
#[cfg(feature="decoder_pcm")]
    DecoderInfo { name: "alaw", get_decoder: pcm::get_a_law_decoder },
#[cfg(feature="decoder_pcm")]
    DecoderInfo { name: "ulaw", get_decoder: pcm::get_mu_law_decoder },
#[cfg(feature="decoder_sipro")]
    DecoderInfo { name: "sipro", get_decoder: sipro::get_decoder },
#[cfg(feature="decoder_ts102366")]
    DecoderInfo { name: "ac3", get_decoder: ts102366::get_decoder },
#[cfg(feature="decoder_atrac3")]
    DecoderInfo { name: "atrac3", get_decoder: atrac3::get_decoder },
];

/// Registers all available codecs provided by this crate.
#[cfg(feature="decoders")]
pub fn generic_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in DECODERS.iter() {
        rd.add_decoder(*decoder);
    }
}

#[cfg(feature="encoder_cinepak")]
mod cinepakenc;
#[cfg(feature="encoder_gif")]
mod gifenc;
#[cfg(feature="encoder_rawvideo")]
mod rawvideoenc;
#[cfg(feature="encoder_zmbv")]
mod zmbvenc;

#[cfg(feature="encoders")]
const ENCODERS: &[EncoderInfo] = &[
#[cfg(feature="encoder_cinepak")]
    EncoderInfo { name: "cinepak", get_encoder: cinepakenc::get_encoder },
#[cfg(feature="encoder_gif")]
    EncoderInfo { name: "gif", get_encoder: gifenc::get_encoder },
#[cfg(feature="encoder_rawvideo")]
    EncoderInfo { name: "rawvideo", get_encoder: rawvideoenc::get_encoder },
#[cfg(feature="encoder_zmbv")]
    EncoderInfo { name: "zmbv", get_encoder: zmbvenc::get_encoder },

#[cfg(feature="encoder_pcm")]
    EncoderInfo { name: "pcm", get_encoder: pcm::get_encoder },
];

/// Registers all available encoders provided by this crate.
#[cfg(feature="encoders")]
pub fn generic_register_all_encoders(re: &mut RegisteredEncoders) {
    for encoder in ENCODERS.iter() {
        re.add_encoder(*encoder);
    }
}

