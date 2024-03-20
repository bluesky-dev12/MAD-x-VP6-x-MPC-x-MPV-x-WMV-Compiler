use nihav_core::codecs::{DecoderResult, DecoderError};
use nihav_core::io::bitreader::*;
use std::fmt;

#[allow(non_camel_case_types)]
#[derive(Clone,Copy,PartialEq)]
pub enum M4AType {
    None,
    Main,
    LC,
    SSR,
    LTP,
    SBR,
    Scalable,
    TwinVQ,
    CELP,
    HVXC,
    TTSI,
    MainSynth,
    WavetableSynth,
    GeneralMIDI,
    Algorithmic,
    ER_AAC_LC,
    ER_AAC_LTP,
    ER_AAC_Scalable,
    ER_TwinVQ,
    ER_BSAC,
    ER_AAC_LD,
    ER_CELP,
    ER_HVXC,
    ER_HILN,
    ER_Parametric,
    SSC,
    PS,
    MPEGSurround,
    Layer1,
    Layer2,
    Layer3,
    DST,
    ALS,
    SLS,
    SLSNonCore,
    ER_AAC_ELD,
    SMRSimple,
    SMRMain,
    Reserved,
    Unknown,
}

const M4A_TYPES: &[M4AType] = &[
    M4AType::None,              M4AType::Main,      M4AType::LC,            M4AType::SSR,
    M4AType::LTP,               M4AType::SBR,       M4AType::Scalable,      M4AType::TwinVQ,
    M4AType::CELP,              M4AType::HVXC,      M4AType::Reserved,      M4AType::Reserved,
    M4AType::TTSI,              M4AType::MainSynth, M4AType::WavetableSynth, M4AType::GeneralMIDI,
    M4AType::Algorithmic,       M4AType::ER_AAC_LC, M4AType::Reserved,      M4AType::ER_AAC_LTP,
    M4AType::ER_AAC_Scalable,   M4AType::ER_TwinVQ, M4AType::ER_BSAC,       M4AType::ER_AAC_LD,
    M4AType::ER_CELP,           M4AType::ER_HVXC,   M4AType::ER_HILN,       M4AType::ER_Parametric,
    M4AType::SSC,               M4AType::PS,        M4AType::MPEGSurround,  M4AType::Reserved /*escape*/,
    M4AType::Layer1,            M4AType::Layer2,    M4AType::Layer3,        M4AType::DST,
    M4AType::ALS,               M4AType::SLS,       M4AType::SLSNonCore,    M4AType::ER_AAC_ELD,
    M4AType::SMRSimple,         M4AType::SMRMain,
];
const M4A_TYPE_NAMES: &[&str] = &[
    "None", "AAC Main", "AAC LC", "AAC SSR", "AAC LTP", "SBR", "AAC Scalable", "TwinVQ", "CELP", "HVXC",
    /*"(reserved10)", "(reserved11)", */ "TTSI",
    "Main synthetic", "Wavetable synthesis", "General MIDI", "Algorithmic Synthesis and Audio FX",
    "ER AAC LC", /*"(reserved18)",*/ "ER AAC LTP", "ER AAC Scalable", "ER TwinVQ", "ER BSAC", "ER AAC LD",
    "ER CELP", "ER HVXC", "ER HILN", "ER Parametric", "SSC", "PS", "MPEG Surround", /*"(escape)",*/
    "Layer-1", "Layer-2", "Layer-3", "DST", "ALS", "SLS", "SLS non-core", "ER AAC ELD", "SMR Simple", "SMR Main",
    "(reserved)", "(unknown)",
];

impl fmt::Display for M4AType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", M4A_TYPE_NAMES[*self as usize])
    }
}

const AAC_SAMPLE_RATES: [u32; 16] = [
    96000, 88200, 64000, 48000, 44100, 32000, 24000, 22050,
    16000, 12000, 11025,  8000,  7350, 0, 0, 0
];

const AAC_CHANNELS: [usize; 8] = [ 0, 1, 2, 3, 4, 5, 6, 8 ];

pub struct M4AInfo {
    pub otype:          M4AType,
    pub srate:          u32,
    pub channels:       usize,
    pub samples:        usize,
    pub sbr_ps_info:    Option<(u32, usize)>,
    pub sbr_present:    bool,
    pub ps_present:     bool,
}

impl M4AInfo {
    pub fn new() -> Self {
        Self {
            otype:          M4AType::None,
            srate:          0,
            channels:       0,
            samples:        0,
            sbr_ps_info:    Option::None,
            sbr_present:    false,
            ps_present:     false,
        }
    }
    fn read_object_type(br: &mut BitReader) -> DecoderResult<M4AType> {
        let otypeidx;
        if br.peek(5) == 31 {
                                                          br.skip(5)?;
            otypeidx                                    = (br.read(6)? as usize) + 32;
        } else {
            otypeidx                                    = br.read(5)? as usize;
        }
        if otypeidx >= M4A_TYPES.len() {
            Ok(M4AType::Unknown)
        } else {
            Ok(M4A_TYPES[otypeidx])
        }
    }
    fn read_sampling_frequency(br: &mut BitReader) -> DecoderResult<u32> {
        if br.peek(4) == 15 {
            let srate                                   = br.read(24)?;
            Ok(srate)
        } else {
            let srate_idx                               = br.read(4)? as usize;
            Ok(AAC_SAMPLE_RATES[srate_idx])
        }
    }
    fn read_channel_config(br: &mut BitReader) -> DecoderResult<usize> {
        let chidx                                       = br.read(4)? as usize;
        if chidx < AAC_CHANNELS.len() {
            Ok(AAC_CHANNELS[chidx])
        } else {
            Ok(chidx)
        }
    }
    pub fn read(&mut self, src: &[u8]) -> DecoderResult<()> {
        let mut br = BitReader::new(src, BitReaderMode::BE);
        self.otype = Self::read_object_type(&mut br)?;
        self.srate = Self::read_sampling_frequency(&mut br)?;
        validate!(self.srate > 0);
        self.channels = Self::read_channel_config(&mut br)?;

        if (self.otype == M4AType::SBR) || (self.otype == M4AType::PS) {
            let ext_srate = Self::read_sampling_frequency(&mut br)?;
            self.otype = Self::read_object_type(&mut br)?;
            let ext_chans;
            if self.otype == M4AType::ER_BSAC {
                ext_chans = Self::read_channel_config(&mut br)?;
            } else {
                ext_chans = 0;
            }
            self.sbr_ps_info = Some((ext_srate, ext_chans));
        }

        match self.otype {
            M4AType::Main | M4AType::LC | M4AType::SSR | M4AType::Scalable | M4AType::TwinVQ |
            M4AType::ER_AAC_LC | M4AType::ER_AAC_LTP | M4AType::ER_AAC_Scalable | M4AType::ER_TwinVQ |
            M4AType::ER_BSAC | M4AType::ER_AAC_LD => {
                // GASpecificConfig
                    let short_frame                     = br.read_bool()?;
                    self.samples = if short_frame { 960 } else { 1024 };
                    let depends_on_core                 = br.read_bool()?;
                    if depends_on_core {
                        let _delay                      = br.read(14)?;
                    }
                    let extension_flag                  = br.read_bool()?;
                    if self.channels == 0 {
                        let (channels, sf_code) = skimp_through_program_config_element(&mut br)?;
                        validate!(channels > 0);
                        self.channels = channels;
                        validate!(AAC_SAMPLE_RATES[sf_code] != 0);
                        self.srate = AAC_SAMPLE_RATES[sf_code];
                    }
                    if (self.otype == M4AType::Scalable) || (self.otype == M4AType::ER_AAC_Scalable) {
                        let _layer                      = br.read(3)?;
                    }
                    if extension_flag {
                        if self.otype == M4AType::ER_BSAC {
                            let _num_subframes          = br.read(5)? as usize;
                            let _layer_length           = br.read(11)?;
                        }
                        if (self.otype == M4AType::ER_AAC_LC) ||
                           (self.otype == M4AType::ER_AAC_LTP) ||
                           (self.otype == M4AType::ER_AAC_Scalable) ||
                           (self.otype == M4AType::ER_AAC_LD) {
                            let _section_data_resilience    = br.read_bool()?;
                            let _scalefactors_resilience    = br.read_bool()?;
                            let _spectral_data_resilience   = br.read_bool()?;
                        }
                        let extension_flag3             = br.read_bool()?;
                        if extension_flag3 {
                            unimplemented!("version3 extensions");
                        }
                    }
                },
            M4AType::CELP => { unimplemented!("CELP config"); },
            M4AType::HVXC => { unimplemented!("HVXC config"); },
            M4AType::TTSI => { unimplemented!("TTS config"); },
            M4AType::MainSynth | M4AType::WavetableSynth | M4AType::GeneralMIDI | M4AType::Algorithmic => { unimplemented!("structured audio config"); },
            M4AType::ER_CELP => { unimplemented!("ER CELP config"); },
            M4AType::ER_HVXC => { unimplemented!("ER HVXC config"); },
            M4AType::ER_HILN | M4AType::ER_Parametric => { unimplemented!("parametric config"); },
            M4AType::SSC => { unimplemented!("SSC config"); },
            M4AType::MPEGSurround => {
                                                        br.skip(1)?; // sacPayloadEmbedding
                    unimplemented!("MPEG Surround config");
                },
            M4AType::Layer1 | M4AType::Layer2 | M4AType::Layer3 => { unimplemented!("MPEG Layer 1/2/3 config"); },
            M4AType::DST => { unimplemented!("DST config"); },
            M4AType::ALS => {
                                                        br.skip(5)?; // fillBits
                    unimplemented!("ALS config");
                },
            M4AType::SLS | M4AType::SLSNonCore => { unimplemented!("SLS config"); },
            M4AType::ER_AAC_ELD => { unimplemented!("ELD config"); },
            M4AType::SMRSimple | M4AType::SMRMain => { unimplemented!("symbolic music config"); },
            _ => {},
        };
        match self.otype {
            M4AType::ER_AAC_LC | M4AType::ER_AAC_LTP | M4AType::ER_AAC_Scalable | M4AType::ER_TwinVQ |
            M4AType::ER_BSAC | M4AType::ER_AAC_LD | M4AType::ER_CELP | M4AType::ER_HVXC |
            M4AType::ER_HILN | M4AType::ER_Parametric | M4AType::ER_AAC_ELD => {
                    let ep_config                       = br.read(2)?;
                    if (ep_config == 2) || (ep_config == 3) {
                        unimplemented!("error protection config");
                    }
                    if ep_config == 3 {
                        let direct_mapping              = br.read_bool()?;
                        validate!(direct_mapping);
                    }
                },
            _ => {},
        };
        if self.sbr_ps_info.is_some() && (br.left() >= 16) {
            let sync                                    = br.read(11)?;
            if sync == 0x2B7 {
                let ext_otype = Self::read_object_type(&mut br)?;
                if ext_otype == M4AType::SBR {
                    self.sbr_present                    = br.read_bool()?;
                    if self.sbr_present {
                        let _ext_srate = Self::read_sampling_frequency(&mut br)?;
                        if br.left() >= 12 {
                            let sync                    = br.read(11)?;
                            if sync == 0x548 {
                                self.ps_present         = br.read_bool()?;
                            }
                        }
                    }
                }
                if ext_otype == M4AType::PS {
                    self.sbr_present                    = br.read_bool()?;
                    if self.sbr_present {
                        let _ext_srate = Self::read_sampling_frequency(&mut br)?;
                    }
                    let _ext_channels = br.read(4)?;
                }
            }
        }

        Ok(())
    }
}

pub fn skimp_through_program_config_element(br: &mut BitReader) -> DecoderResult<(usize, usize)> {
    let _id                             = br.read(4)?;
    let _object_type                    = br.read(2)?;
    let sampling_frequency_index        = br.read(4)? as usize;
    let num_front_channel_elements      = br.read(4)? as usize;
    let num_side_channel_elements       = br.read(4)? as usize;
    let num_back_channel_elements       = br.read(4)? as usize;
    let num_lfe_channel_elements        = br.read(2)? as usize;
    let num_assoc_data_elements         = br.read(3)? as usize;
    let num_valid_cc_elements           = br.read(4)? as usize;
    let mono_mixdown_present            = br.read_bool()?;
    if mono_mixdown_present {
        let _mono_mixdown_element_number = br.read(4)?;
    }
    let stereo_mixdown_present           = br.read_bool()?;
    if stereo_mixdown_present {
        let _stereo_mixdown_element_number = br.read(4)?;
    }
    let matrix_mixdown_idx_present      = br.read_bool()?;
    if matrix_mixdown_idx_present {
        let _matrix_mixdown_idx         = br.read(2)?;
        let _pseudo_surround_enable     = br.read_bool()?;
    }
    for _i in 0..num_front_channel_elements {
        let _front_element_is_cpe       = br.read_bool()?;
        let _front_element_tag_select   = br.read(4)?;
    }
    for _i in 0..num_side_channel_elements {
        let _side_element_is_cpe        = br.read_bool()?;
        let _side_element_tag_select    = br.read(4)?;
    }
    for _i in 0..num_back_channel_elements {
        let _back_element_is_cpe        = br.read_bool()?;
        let _back_element_tag_select    = br.read(4)?;
    }
    for _i in 0..num_lfe_channel_elements {
        let _lfe_element_tag_select     = br.read(4)?;
    }
    for _i in 0..num_assoc_data_elements {
        let _assoc_data_element_tag_select = br.read(4)?;
    }
    for _i in 0..num_valid_cc_elements {
        let _cc_element_is_ind_sw       = br.read_bool()?;
        let _valid_cc_element_tag_select = br.read(4)?;
    }
                                          br.align();
    let comment_field_bytes             = br.read(8)?;
                                          br.skip(comment_field_bytes * 8)?;

    Ok((num_front_channel_elements +
        num_side_channel_elements +
        num_back_channel_elements +
        num_lfe_channel_elements, sampling_frequency_index))
}

impl fmt::Display for M4AInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "MPEG 4 Audio {}, {} Hz, {} channels, {} samples per frame",
               self.otype, self.srate, self.channels, self.samples)
    }
}

