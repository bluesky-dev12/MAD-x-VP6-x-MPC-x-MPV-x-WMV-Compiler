use nihav_core::codecs::*;

#[cfg(debug_assertions)]
macro_rules! validate {
    ($a:expr) => { if !$a { println!("check failed at {}:{}", file!(), line!()); return Err(DecoderError::InvalidData); } };
}
#[cfg(not(debug_assertions))]
macro_rules! validate {
    ($a:expr) => { if !$a { return Err(DecoderError::InvalidData); } };
}

#[cfg(feature="decoder_aac")]
#[allow(clippy::comparison_chain)]
#[allow(clippy::excessive_precision)]
#[allow(clippy::manual_memcpy)]
#[allow(clippy::useless_let_if_seq)]
mod aac;
#[cfg(feature="decoder_mpa")]
#[allow(clippy::excessive_precision)]
mod mpegaudio;

const DECODERS: &[DecoderInfo] = &[
#[cfg(feature="decoder_aac")]
    DecoderInfo { name: "aac", get_decoder: aac::get_decoder },
#[cfg(feature="decoder_mpa")]
    DecoderInfo { name: "mp2", get_decoder: mpegaudio::get_decoder_mp2 },
#[cfg(feature="decoder_mpa")]
    DecoderInfo { name: "mp3", get_decoder: mpegaudio::get_decoder_mp3 },
];

/// Registers all available codecs provided by this crate.
pub fn mpeg_register_all_decoders(rd: &mut RegisteredDecoders) {
    for decoder in DECODERS.iter() {
        rd.add_decoder(*decoder);
    }
}

const PACKETISERS: &[PacketiserInfo] = &[
#[cfg(feature="decoder_mpa")]
    PacketiserInfo { name: "mpa", get_packetiser: mpegaudio::get_packetiser },
];

/// Registers all available packetisers provided by this crate.
pub fn mpeg_register_all_packetisers(rp: &mut RegisteredPacketisers) {
    for packetiser in PACKETISERS.iter() {
        rp.add_packetiser(*packetiser);
    }
}
