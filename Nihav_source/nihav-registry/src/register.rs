//! Global registry of codec information.
//!
//! This module contains codec information from technical level that allows user to retrieve information about codec type and features without creating and invoking a decoder for such codec.
use std::fmt;

/// Codec types.
#[derive(Debug,Clone,Copy,PartialEq)]
#[allow(dead_code)]
pub enum CodecType {
    /// Video codec.
    Video,
    /// Audio codec.
    Audio,
    /// Subtitle codec.
    Subtitles,
    /// Some special codec (e.g. some event stream or separate timecodes stream).
    Data,
    /// Dummy codec.
    None,
}

impl fmt::Display for CodecType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CodecType::Video => write!(f, "Video"),
            CodecType::Audio => write!(f, "Audio"),
            CodecType::Subtitles => write!(f, "Subtitles"),
            CodecType::Data => write!(f, "Data"),
            CodecType::None => write!(f, "-"),
        }
    }
}

/// Codec capability flag for intra-only codecs.
pub const CODEC_CAP_INTRAONLY:u32   = 0x0001;
/// Codec capability flag for lossless codecs.
pub const CODEC_CAP_LOSSLESS:u32    = 0x0002;
/// Codec capability flag for codecs with frame reordering.
pub const CODEC_CAP_REORDER:u32     = 0x0004;
/// Codec capability flag for codecs that can be both lossy and lossless.
pub const CODEC_CAP_HYBRID:u32      = 0x0008;
/// Codec capability flag for codecs with scalability features.
pub const CODEC_CAP_SCALABLE:u32    = 0x0010;
/// Codec capability flag for codecs with complex frame reordering.
pub const CODEC_CAP_COMPLEX_REORDER:u32 = 0x0020;

/// Codec description structure.
#[derive(Clone)]
pub struct CodecDescription {
    /// Short codec name.
    ///
    /// Short codec name is used inside NihAV as the unique identifier.
    pub name:  &'static str,
    /// Full codec name.
    pub fname: &'static str,
    /// Codec type.
    pub ctype: CodecType,
    /// Codec capabilities.
    pub caps:  u32,
}

impl CodecDescription {
    /// Returns short codec name.
    pub fn get_name(&self) -> &'static str { self.name }
    /// Returns full codec name.
    pub fn get_full_name(&self) -> &'static str { self.fname }
    /// Returns codec type.
    pub fn get_codec_type(&self) -> CodecType { self.ctype }
    /// Reports whether the codec has only intra frames or not.
    pub fn is_intraonly(&self) -> bool { (self.caps & CODEC_CAP_INTRAONLY) != 0 }
    /// Reports whether the codec is lossless.
    pub fn is_lossless(&self)  -> bool { (self.caps & CODEC_CAP_LOSSLESS)  != 0 }
    /// Reports whether the codec requires frame reordering.
    pub fn has_reorder(&self)  -> bool { (self.caps & CODEC_CAP_REORDER)   != 0 }
    /// Reports whether the codec can be either lossless or lossy.
    pub fn is_hybrid(&self)    -> bool { (self.caps & CODEC_CAP_HYBRID)    != 0 }
    /// Reports whether codec supports scalability.
    ///
    /// Scalability means that codec can be decoded in reduced resolution by design.
    pub fn is_scalable(&self)  -> bool { (self.caps & CODEC_CAP_SCALABLE)  != 0 }
}

impl fmt::Display for CodecDescription {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = self.fname.to_string();
        if self.caps != 0 {
            let mut capfmt = "".to_string();
            if (self.caps & CODEC_CAP_INTRAONLY) != 0 {
                capfmt = format!("{} Intra-only", capfmt);
            }
            if (self.caps & CODEC_CAP_LOSSLESS) != 0 {
                capfmt = format!("{} Lossless", capfmt);
            }
            if (self.caps & CODEC_CAP_REORDER) != 0 {
                capfmt = format!("{} Frame reorder", capfmt);
            }
            if (self.caps & CODEC_CAP_HYBRID) != 0 {
                capfmt = format!("{} Can be lossy and lossless", capfmt);
            }
            if (self.caps & CODEC_CAP_SCALABLE) != 0 {
                capfmt = format!("{} Scalable", capfmt);
            }
            out = format!("{} ({})", out, capfmt);
        }
        write!(f, "{}", out)
    }
}

macro_rules! desc {
    (video; $n:expr, $fn:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Video,
                          caps: 0 }
    });
    (video; $n:expr, $fn:expr, $c:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Video,
                          caps: $c }
    });
    (video-ll; $n:expr, $fn:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Video,
                          caps: CODEC_CAP_LOSSLESS | CODEC_CAP_INTRAONLY }
    });
    (video-llp; $n:expr, $fn:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Video,
                          caps: CODEC_CAP_LOSSLESS }
    });
    (video-im; $n:expr, $fn:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Video,
                          caps: CODEC_CAP_INTRAONLY }
    });
    (video-modern; $n:expr, $fn:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Video,
                          caps: CODEC_CAP_REORDER | CODEC_CAP_HYBRID }
    });
    (audio; $n:expr, $fn:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Audio,
                          caps: 0 }
    });
    (audio-ll; $n:expr, $fn:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Audio,
                          caps: CODEC_CAP_LOSSLESS | CODEC_CAP_INTRAONLY }
    });
    (audio-hyb; $n:expr, $fn:expr) => ({
        CodecDescription{ name: $n, fname: $fn, ctype: CodecType::Audio,
                          caps: CODEC_CAP_HYBRID }
    });
}

/// Returns codec description for the provided codec short name if it is found.
pub fn get_codec_description(name: &str) -> Option<&'static CodecDescription> {
    CODEC_REGISTER.iter().find(|&reg| reg.name == name)
}

static CODEC_REGISTER: &[CodecDescription] = &[
    desc!(audio-ll; "pcm", "PCM"),
    desc!(audio;    "alaw", "A-law PCM"),
    desc!(audio;    "ulaw", "mu-law PCM"),

    desc!(video-im; "indeo1", "Intel Raw IF09"),
    desc!(video-im; "indeo2", "Intel Indeo 2"),
    desc!(video;    "indeo3", "Intel Indeo 3"),
    desc!(video;    "indeo4", "Intel Indeo 4", CODEC_CAP_REORDER | CODEC_CAP_SCALABLE),
    desc!(video;    "indeo5", "Intel Indeo 5", CODEC_CAP_REORDER | CODEC_CAP_SCALABLE),
    desc!(video;    "indeo5s", "Intel Indeo 5 Scalable", CODEC_CAP_SCALABLE),
    desc!(video;    "intel263", "Intel I263", CODEC_CAP_REORDER),
    desc!(audio;    "iac",    "Intel Indeo audio"),
    desc!(audio;    "imc",    "Intel Music Coder"),

    desc!(video;    "realvideo1", "Real Video 1"),
    desc!(video;    "realvideo2", "Real Video 2", CODEC_CAP_REORDER),
    desc!(video;    "realvideo3", "Real Video 3", CODEC_CAP_REORDER),
    desc!(video;    "realvideo4", "Real Video 4", CODEC_CAP_REORDER),
    desc!(video;    "realvideo6", "Real Video 6", CODEC_CAP_REORDER),
    desc!(video;    "clearvideo", "ClearVideo"),
    desc!(video;    "clearvideo_rm", "ClearVideo"),
    desc!(audio;    "ra14.4",     "RealAudio 14.4"),
    desc!(audio;    "ra28.8",     "RealAudio 28.8"),
    desc!(audio;    "cook",       "RealAudio Cooker"),
    desc!(audio;    "ralf",       "RealAudio Lossless"),
    desc!(audio;    "aac",        "AAC"),
    desc!(audio;    "ac3",        "ETSI TS 102 366"),
    desc!(audio;    "atrac3",     "Sony Atrac3"),
    desc!(audio;    "sipro",      "Sipro Labs ADPCM"),


    desc!(video-ll; "rawvideo",   "Raw video data"),
    desc!(video-ll; "rawvideo-ms", "Raw video data"),

    desc!(video;    "cinepak",    "Cinepak"),

    desc!(video-llp; "zmbv",      "Zip Motion Blocks Video"),

    desc!(video;    "msvideo1",      "MS Video 1"),
    desc!(video;    "msrle",         "MS RLE"),
    desc!(audio;    "ms-adpcm",      "MS ADPCM"),
    desc!(audio;    "ima-adpcm-ms",  "IMA ADPCM (MS variant)"),

    desc!(video;    "qt-smc",               "Apple Graphics"),
    desc!(video;    "qt-rle",               "Apple Animation"),
    desc!(video;    "apple-video",          "Apple video"),
    desc!(video;    "sorenson-video",       "Sorenson Video"),
    desc!(video;    "sorenson-video3",      "Sorenson Video 3", CODEC_CAP_REORDER),
    desc!(audio-ll; "alac",                 "Apple Lossless Audio Codec"),
    desc!(audio;    "mace-3",               "MACE 3:1"),
    desc!(audio;    "mace-6",               "MACE 6:1"),
    desc!(audio;    "ima-adpcm-qt",         "IMA ADPCM (Apple variant)"),
    desc!(audio;    "qdesign-music",        "QDesign Music"),
    desc!(audio;    "qdesign-music2",       "QDesign Music v2"),
    desc!(audio;    "qualcomm-purevoice",   "Qualcomm PureVoice"),

    desc!(video;    "truemotion1",   "TrueMotion 1"),
    desc!(video-im; "truemotionrt",  "TrueMotion RT"),
    desc!(video;    "truemotion2",   "TrueMotion 2"),
    desc!(video;    "truemotion2x",  "TrueMotion 2X"),
    desc!(video;    "vp3",           "VP3"),
    desc!(video;    "vp4",           "VP4"),
    desc!(video;    "vp5",           "VP5"),
    desc!(video;    "vp6",           "VP6"),
    desc!(video;    "vp6f",          "VP6 (in Flash)"),
    desc!(video;    "vp6a",          "VP6 with alpha"),
    desc!(video;    "vp7",           "VP7"),
    desc!(video;    "vp8",           "VP8"),
    desc!(video;    "vp9",           "VP9"),
    desc!(audio;    "adpcm-dk3",     "Duck DK3 ADPCM"),
    desc!(audio;    "adpcm-dk4",     "Duck DK4 ADPCM"),
    desc!(audio;    "on2avc-500",    "On2 AVC"),
    desc!(audio;    "on2avc-501",    "On2 AVC"),

    desc!(video;     "flv263",       "Sorenson H.263"),
    desc!(video-llp; "flashsv",      "Flash Screen Video"),
    desc!(video-llp; "flashsv2",     "Flash Screen Video 2"),
    desc!(audio;     "asao",         "N*llym*s*r ASAO"),
    desc!(audio;     "flv-adpcm",    "Flash ADPCM"),

    desc!(audio;     "mp1",          "MPEG Audio Layer I"),
    desc!(audio;     "mp2",          "MPEG Audio Layer II"),
    desc!(audio;     "mp3",          "MPEG Audio Layer III"),
    desc!(audio;     "speex",        "Speex"),

    desc!(video;    "gdv-video",     "Gremlin Digital Video - video"),
    desc!(audio;    "gdv-audio",     "Gremlin Digital Video - audio"),
    desc!(video-im; "arxel-video",   "Arxel Tribe Video"),
    desc!(video;    "beam-fcp",      "Beam Software Animation"),
    desc!(video;    "beam-video",    "Beam Software Video"),
    desc!(video;    "bmv-video",     "BMV video"),
    desc!(audio;    "bmv-audio",     "BMV audio"),
    desc!(video;    "bmv3-video",    "DW Noir BMV video"),
    desc!(audio;    "bmv3-audio",    "DW Noir BMV audio"),
    desc!(video;    "dp-sga",        "Digital Pictures SGA video"),
    desc!(video;    "fable-imax",    "Fable IMAX video"),
    desc!(video;    "fst-video",     "FutureVision video"),
    desc!(audio;    "fst-audio",     "FutureVision audio"),
    desc!(video;    "hl-fmv-video",  "Highlander FMV video"),
    desc!(video-llp; "ipma",         "Imagination Pilots Matte Animation"),
    desc!(video-llp; "ipma2",        "Imagination Pilots Matte Animation v2"),
    desc!(video;    "legend-q-video", "Legend Entertainment Q video"),
    desc!(video;    "midivid",       "MidiVid"),
    desc!(video;    "midivid3",      "MidiVid 3"),
    desc!(video-ll; "midivid-ll",    "MidiVid Lossless"),
    desc!(video;    "smushv1",       "SMUSH Video paletted"),
    desc!(video;    "smushv2",       "SMUSH Video 16-bit"),
    desc!(video;    "smush-iact",    "SMUSH IACT Audio"),
    desc!(video;    "smush-vima",    "SMUSH VIMA Audio"),
    desc!(video;    "vmd-video",     "VMD video"),
    desc!(audio;    "vmd-audio",     "VMD audio"),
    desc!(video;    "vxvideo",       "Actimagine Vx"),
    desc!(audio;    "vxaudio",       "Actimagine Sx"),

    desc!(video;    "smacker-video", "Smacker video"),
    desc!(audio;    "smacker-audio", "Smacker audio"),
    desc!(video;    "bink-video",    "Bink video"),
    desc!(video;    "bink2-video",   "Bink2 video"),
    desc!(audio;    "bink-audio-dct",   "Bink audio (DCT)"),
    desc!(audio;    "bink-audio-rdft",  "Bink audio (RDFT)"),

    desc!(audio;    "lhst15f8",      "L&H StreamTalk 15kbps at 8 kHz"),
    desc!(audio;    "lhst250f11",    "L&H StreamTalk 25kbps at 11 kHz"),
    desc!(audio;    "lhst500f22",    "L&H StreamTalk 50kpbs at 22 kHz"),
    desc!(audio;    "lhst48",        "L&H StreamTalk CELP Codec 4.8kbps at 8 kHz"),

    desc!(video;    "vivo1",         "VivoActive Video 1.0"),
    desc!(video;    "vivo2",         "VivoActive Video 2.0", CODEC_CAP_REORDER),
    desc!(audio;    "g723.1",        "ITU G.723.1"),
    desc!(audio;    "siren",         "Polycom Siren"),

    desc!(audio-ll;  "ape",          "Monkey's Audio"),
    desc!(audio-ll;  "flac",         "Free Lossless Audio Codec"),
    desc!(audio-ll;  "tta",          "True Audio codec"),
    desc!(audio-hyb; "wavpack",      "WavPack"),

    desc!(video-ll; "gif",           "GIF"),
    desc!(video-im; "jpeg",          "JPEG"),
    desc!(video;    "h264",          "ITU H.264", CODEC_CAP_COMPLEX_REORDER | CODEC_CAP_HYBRID),

    desc!(video-im; "mwv1",          "Aware MotionWavelets"),
];

static AVI_VIDEO_CODEC_REGISTER: &[(&[u8;4], &str)] = &[
    (&[1, 0, 0, 0], "msrle"),
    (&[2, 0, 0, 0], "msrle"),

    (b"CRAM", "msvideo1"),
    (b"MSVC", "msvideo1"),
    (b"WHAM", "msvideo1"),

    (b"MJPG", "jpeg"),

    (b"IF09", "indeo1"),
    (b"RT21", "indeo2"),
    (b"IV31", "indeo3"),
    (b"IV32", "indeo3"),
    (b"IV41", "indeo4"),
    (b"IV50", "indeo5"),
    (b"I263", "intel263"),

    (b"UCOD", "clearvideo"),
    (b"cvid", "cinepak"),
    (b"ZMBV", "zmbv"),

    (b"Ipma", "ipma"),
    (b"Ip20", "ipma2"),

    (b"MVDV", "midivid"),
    (b"MV30", "midivid3"),
    (b"MVLZ", "midivid-ll"),

    (b"DUCK", "truemotion1"),
    (b"TR20", "truemotionrt"),
    (b"TM20", "truemotion2"),
    (b"TM2A", "truemotion2x"),
    (b"TM2X", "truemotion2x"),
    (b"VP30", "vp3"),
    (b"VP31", "vp3"),
    (b"VP40", "vp4"),
    (b"VP50", "vp5"),
    (b"VP60", "vp6"),
    (b"VP61", "vp6"),
    (b"VP62", "vp6"),
    (b"VP6A", "vp6a"),
    (b"VP70", "vp7"),

    (b"MWV1", "mwv1"),
];

static WAV_CODEC_REGISTER: &[(u16, &str)] = &[
    (0x0000, "unknown"),
    (0x0001, "pcm"),
    (0x0002, "ms-adpcm"),
    (0x0003, "pcm"),
    (0x0011, "ima-adpcm-ms"),
    (0x0061, "adpcm-dk4"),
    (0x0062, "adpcm-dk3"),
    (0x0401, "imc"),
    (0x0402, "iac"),
    (0x0500, "on2avc-500"),
    (0x0501, "on2avc-501"),
];

static MOV_VIDEO_CODEC_REGISTER: &[(&[u8;4], &str)] = &[
    (b"cvid", "cinepak"),
    (b"jpeg", "jpeg"),
    //(b"raw ", "raw"),
    //(b"Yuv2", "raw"),
    (b"smc ", "qt-smc"),
    (b"rle ", "qt-rle"),
    (b"rpza", "apple-video"),
    (b"kpcd", "kodak-photocd"),
    //(b"mpeg", "mpeg-video"),
    (b"mjpa", "mjpeg-a"),
    (b"mjpb", "mjpeg-b"),
    (b"svqi", "sorenson-video"),
    (b"SVQ1", "sorenson-video"),
    (b"svq3", "sorenson-video3"),
    (b"SVQ3", "sorenson-video3"),

    (b"IV31", "indeo3"),
    (b"IV32", "indeo3"),

    (b"UCOD", "clearvideo"),

    (b"VP30", "vp3"),
    (b"VP31", "vp3"),

    (b"avc1", "h264"),
];

static MOV_AUDIO_CODEC_REGISTER: &[(&[u8;4], &str)] = &[
    (b"NONE", "pcm"),
    (b"raw ", "pcm"),
    (b"twos", "pcm"),
    (b"sowt", "pcm"),
    (b"fl32", "pcm"),
    (b"fl64", "pcm"),
    (b"in24", "pcm"),
    (b"in32", "pcm"),
    (b"MAC3", "mace-3"),
    (b"MAC6", "mace-6"),
    (b"ima4", "ima-adpcm-qt"),
    (b"ulaw", "ulaw"),
    (b"alaw", "alaw"),
    (b"dvca", "dv-audio"),
    (b"QDMC", "qdesign-music"),
    (b"QDM2", "qdesign-music2"),
    (b"Qclp", "qualcomm-purevoice"),
    //(b".mp3", "mpeg-layer3"),

    (b"mp4a", "aac"),

    (b"alac", "alac"),
];

/// Returns video codec short name for provided FOURCC (used in AVI format).
pub fn find_codec_from_avi_fourcc(fcc: &[u8;4]) -> Option<&'static str> {
    for (fourcc, name) in AVI_VIDEO_CODEC_REGISTER.iter() {
        if *fourcc == fcc { return Some(name); }
    }
    None
}

/// Returns FOURCC (used in AVI format) for provided codec name.
pub fn find_avi_fourcc(codecname: &str) -> Option<[u8; 4]> {
    for (fourcc, name) in AVI_VIDEO_CODEC_REGISTER.iter() {
        if *name == codecname { return Some(**fourcc); }
    }
    None
}

/// Returns known audio codec short name for provided TWOCC (used in WAV and AVI format).
pub fn find_codec_from_wav_twocc(tcc: u16) -> Option<&'static str> {
    for (twocc, name) in WAV_CODEC_REGISTER.iter() {
        if *twocc == tcc { return Some(name); }
    }
    None
}

/// Returns TWOCC (used in WAV and AVI format for provided codec name.
pub fn find_wav_twocc(codecname: &str) -> Option<u16> {
    for (twocc, name) in WAV_CODEC_REGISTER.iter() {
        if *name == codecname { return Some(*twocc); }
    }
    None
}

/// Returns video codec short name for provided FOURCC (used in MOV format).
pub fn find_codec_from_mov_video_fourcc(fcc: &[u8;4]) -> Option<&'static str> {
    for (fourcc, name) in MOV_VIDEO_CODEC_REGISTER.iter() {
        if *fourcc == fcc { return Some(name); }
    }
    None
}

/// Returns known audio codec short name for provided FOURCC (used in MOV format).
pub fn find_codec_from_mov_audio_fourcc(fcc: &[u8;4]) -> Option<&'static str> {
    for (fourcc, name) in MOV_AUDIO_CODEC_REGISTER.iter() {
        if *fourcc == fcc { return Some(name); }
    }
    None
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_register() {
        let c1 = find_codec_from_avi_fourcc(b"IV41").unwrap();
        let c2 = find_codec_from_wav_twocc(0x401).unwrap();
        println!("found {} and {}", c1, c2);
        let cd1 = get_codec_description(c1).unwrap();
        let cd2 = get_codec_description(c2).unwrap();
        println!("got {} and {}", cd1, cd2);
    }
}
