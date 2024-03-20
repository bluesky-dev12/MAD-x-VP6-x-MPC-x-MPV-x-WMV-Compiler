use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(feature="decoder_ape")]
pub mod ape;
#[cfg(feature="decoder_ape")]
mod apepred;
#[cfg(feature="decoder_ape")]
mod apereader;

#[cfg(feature="decoder_flac")]
pub mod flac;

#[cfg(feature="decoder_tta")]
pub mod tta;

#[cfg(feature="decoder_wavpack")]
pub mod wavpack;

const LL_AUDIO_CODECS: &[DecoderInfo] = &[
#[cfg(feature="decoder_ape")]
    DecoderInfo { name: "ape", get_decoder: ape::get_decoder },
#[cfg(feature="decoder_flac")]
    DecoderInfo { name: "flac", get_decoder: flac::get_decoder },
#[cfg(feature="decoder_tta")]
    DecoderInfo { name: "tta", get_decoder: tta::get_decoder },
#[cfg(feature="decoder_wavpack")]
    DecoderInfo { name: "wavpack", get_decoder: wavpack::get_decoder },
];

/// Registers all available codecs provided by this crate.
pub fn llaudio_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in LL_AUDIO_CODECS.iter() {
        rd.add_decoder(*decoder);
    }
}

const LL_PACKETISERS: &[PacketiserInfo] = &[
#[cfg(feature="decoder_flac")]
    PacketiserInfo { name: "flac", get_packetiser: flac::get_packetiser },
];

/// Registers all available packetisers provided by this crate.
pub fn llaudio_register_all_packetisers(rp: &mut RegisteredPacketisers) {
    for pkt in LL_PACKETISERS.iter() {
        rp.add_packetiser(*pkt);
    }
}

#[cfg(feature="encoder_flac")]
pub mod flacenc;

const LL_AUDIO_ENCODERS: &[EncoderInfo] = &[
#[cfg(feature="decoder_flac")]
    EncoderInfo { name: "flac", get_encoder: flacenc::get_encoder },
];

/// Registers all available encoders provided by this crate.
pub fn llaudio_register_all_encoders(re: &mut RegisteredEncoders) {
    for encoder in LL_AUDIO_ENCODERS.iter() {
        re.add_encoder(*encoder);
    }
}
