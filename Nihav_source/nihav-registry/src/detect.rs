//! Container format detection.
//!
//! Usually user does not know the container format of the opened file.
//! That is why format detection functionality is needed.
//! This module contains the set of rules to detect container not merely by file extension but also by its content if possible.
//!
//! # Examples
//!
//! ```no_run
//! use nihav_registry::detect::detect_format;
//! use std::fs::File;
//! use nihav_core::io::byteio::*;
//!
//! let name = "mediafile.ogv";
//! let mut file = File::open(name).unwrap();
//! let mut filereader = FileReader::new_read(&mut file);
//! let mut br = ByteReader::new(&mut filereader);
//! let result = detect_format(name, &mut br);
//! if let Some((name, score)) = result {
//!     println!("detected format {} with score {:?}", name, score);
//! }
//! ```
use std::io::SeekFrom;
use nihav_core::io::byteio::ByteReader;

/// Format detection score.
#[derive(Debug,Clone,Copy,PartialEq)]
pub enum DetectionScore {
    /// Format is not detected.
    No,
    /// Format matched by file extension.
    ExtensionMatches,
    /// Format matches by markers inside the file.
    MagicMatches,
}

impl DetectionScore {
    /// Checks whether current detection score is less than a value it is compared against.
    pub fn less(self, other: DetectionScore) -> bool {
        (self as i32) < (other as i32)
    }
}

#[allow(dead_code)]
enum Arg {
    Byte(u8),
    U16BE(u16),
    U16LE(u16),
    U24BE(u32),
    U24LE(u32),
    U32BE(u32),
    U32LE(u32),
    U64BE(u64),
    U64LE(u64),
}

impl Arg {
    fn val(&self) -> u64 {
        match *self {
            Arg::Byte(b) => { u64::from(b) }
            Arg::U16BE(v) => { u64::from(v) }
            Arg::U16LE(v) => { u64::from(v) }
            Arg::U24BE(v) => { u64::from(v) }
            Arg::U24LE(v) => { u64::from(v) }
            Arg::U32BE(v) => { u64::from(v) }
            Arg::U32LE(v) => { u64::from(v) }
            Arg::U64BE(v) => { v }
            Arg::U64LE(v) => { v }
        }
    }
    fn read_val(&self, src: &mut ByteReader) -> Option<u64> {
        match *self {
            Arg::Byte(_) => {
                let res = src.peek_byte();
                if res.is_err() { return None; }
                Some(u64::from(res.unwrap()))
            }
            Arg::U16BE(_) => {
                let res = src.peek_u16be();
                if res.is_err() { return None; }
                Some(u64::from(res.unwrap()))
            }
            Arg::U16LE(_) => {
                let res = src.peek_u16le();
                if res.is_err() { return None; }
                Some(u64::from(res.unwrap()))
            }
            Arg::U24BE(_) => {
                let res = src.peek_u24be();
                if res.is_err() { return None; }
                Some(u64::from(res.unwrap()))
            }
            Arg::U24LE(_) => {
                let res = src.peek_u24le();
                if res.is_err() { return None; }
                Some(u64::from(res.unwrap()))
            }
            Arg::U32BE(_) => {
                let res = src.peek_u32be();
                if res.is_err() { return None; }
                Some(u64::from(res.unwrap()))
            }
            Arg::U32LE(_) => {
                let res = src.peek_u32le();
                if res.is_err() { return None; }
                Some(u64::from(res.unwrap()))
            }
            Arg::U64BE(_) => {
                let res = src.peek_u64be();
                if res.is_err() { return None; }
                Some(res.unwrap())
            }
            Arg::U64LE(_) => {
                let res = src.peek_u64le();
                if res.is_err() { return None; }
                Some(res.unwrap())
            }
        }
    }
    fn eq(&self, src: &mut ByteReader) -> bool {
        if let Some(rval) = self.read_val(src) {
            rval == self.val()
        } else {
            false
        }
    }
    fn ge(&self, src: &mut ByteReader) -> bool {
        if let Some(rval) = self.read_val(src) {
            rval >= self.val()
        } else {
            false
        }
    }
    fn gt(&self, src: &mut ByteReader) -> bool {
        if let Some(rval) = self.read_val(src) {
            rval > self.val()
        } else {
            false
        }
    }
    fn le(&self, src: &mut ByteReader) -> bool {
        if let Some(rval) = self.read_val(src) {
            rval <= self.val()
        } else {
            false
        }
    }
    fn lt(&self, src: &mut ByteReader) -> bool {
        if let Some(rval) = self.read_val(src) {
            rval < self.val()
        } else {
            false
        }
    }
}

#[allow(dead_code)]
enum CC<'a> {
    Or(&'a CC<'a>, &'a CC<'a>),
    Eq(Arg),
    Str(&'static [u8]),
    In(Arg, Arg),
    Lt(Arg),
    Le(Arg),
    Gt(Arg),
    Ge(Arg),
}

impl<'a> CC<'a> {
    fn eval(&self, src: &mut ByteReader) -> bool {
        match *self {
            CC::Or(a, b)         => { a.eval(src) || b.eval(src) },
            CC::Eq(ref arg)      => { arg.eq(src) },
            CC::In(ref a, ref b) => { a.ge(src) && b.le(src) },
            CC::Lt(ref arg)      => { arg.lt(src) },
            CC::Le(ref arg)      => { arg.le(src) },
            CC::Gt(ref arg)      => { arg.gt(src) },
            CC::Ge(ref arg)      => { arg.ge(src) },
            CC::Str(strng) => {
                let mut val: Vec<u8> = vec![0; strng.len()];
                let res = src.peek_buf(val.as_mut_slice());
                if res.is_err() { return false; }
                val == strng
            }
        }
    }
}

struct CheckItem<'a> {
    offs: u32,
    cond: &'a CC<'a>,
}

#[allow(dead_code)]
struct DetectConditions<'a> {
    demux_name: &'static str,
    extensions: &'static str,
    conditions: &'a [CheckItem<'a>],
}

const DETECTORS: &[DetectConditions] = &[
    DetectConditions {
        demux_name: "avi",
        extensions: ".avi",
        conditions: &[CheckItem{offs: 0, cond: &CC::Or(&CC::Str(b"RIFF"), &CC::Str(b"ON2 ")) },
                      CheckItem{offs: 8, cond: &CC::Or(&CC::Or(&CC::Str(b"AVI LIST"),
                                                               &CC::Str(b"AVIXLIST")),
                                                               &CC::Str(b"ON2fLIST")) },
                     ]
    },
    DetectConditions {
        demux_name: "wav",
        extensions: ".wav",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"RIFF") },
                      CheckItem{offs: 8, cond: &CC::Str(b"WAVEfmt ") }
                     ]
    },
    DetectConditions {
        demux_name: "mov",
        extensions: ".mov",
        conditions: &[CheckItem{offs: 4, cond: &CC::Or(&CC::Or(&CC::Str(b"mdat"),
                                                               &CC::Str(b"moov")),
                                                               &CC::Str(b"ftyp")) }],
    },
    DetectConditions {
        demux_name: "gif",
        extensions: ".gif",
        conditions: &[CheckItem{offs: 0, cond: &CC::Or(&CC::Str(b"GIF87a"),
                                                       &CC::Str(b"GIF89a")) }],
    },
    DetectConditions {
        demux_name: "mov",
        extensions: ".mov",
        conditions: &[CheckItem{offs:  0, cond: &CC::Str(b"\x00\x00\x00\x08wide") },
                      CheckItem{offs: 12, cond: &CC::Or(&CC::Or(&CC::Str(b"mdat"),
                                                                &CC::Str(b"moov")),
                                                                &CC::Str(b"ftyp")) }],
    },
    DetectConditions {
        demux_name: "mov-macbin",
        extensions: ".mov,.bin",
        conditions: &[CheckItem{offs: 0, cond: &CC::Eq(Arg::Byte(0))},
                      CheckItem{offs: 0x41, cond: &CC::Str(b"MooV")},
                      CheckItem{offs: 0x7A, cond: &CC::Eq(Arg::Byte(0x81))},
                      CheckItem{offs: 0x7B, cond: &CC::Eq(Arg::Byte(0x81))},
                      CheckItem{offs: 0x84, cond: &CC::Str(b"mdat")}],
    },
    DetectConditions {
        demux_name: "yuv4mpeg",
        extensions: ".y4m",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"YUV4MPEG2 ") }],
    },
    DetectConditions {
        demux_name: "flv",
        extensions: ".flv",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"FLV") },
                      CheckItem{offs: 3, cond: &CC::Le(Arg::Byte(1)) }],
    },
    DetectConditions {
        demux_name: "ivf",
        extensions: ".ivf",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(&[0x50, 0xEF, 0x81, 0x19, 0xB3, 0xBD, 0xD0, 0x11, 0xA3, 0xE5, 0x00, 0xA0, 0xC9, 0x24, 0x44])},
                      CheckItem{offs: 15, cond: &CC::Or(&CC::Eq(Arg::Byte(0x36)), &CC::Eq(Arg::Byte(0x37)))}],
    },
    DetectConditions {
        demux_name: "dkivf",
        extensions: ".ivf",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"DKIF\x00\x00")},
                      CheckItem{offs: 6, cond: &CC::Ge(Arg::U16LE(32))}],
    },
    DetectConditions {
        demux_name: "arxel-cnm",
        extensions: ".cnm",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"CNM UNR\x00")}],
    },
    DetectConditions {
        demux_name: "fcmp",
        extensions: ".cmp",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"FCMP")}],
    },
    DetectConditions {
        demux_name: "fst",
        extensions: ".fst",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"2TSF")}],
    },
    DetectConditions {
        demux_name: "gdv",
        extensions: ".gdv",
        conditions: &[CheckItem{offs: 0, cond: &CC::Eq(Arg::U32LE(0x29111994))}],
    },
    DetectConditions {
        demux_name: "fable-imax",
        extensions: ".imx",
        conditions: &[CheckItem{offs:  0, cond: &CC::Str(b"IMAX") },
                      CheckItem{offs: 10, cond: &CC::Eq(Arg::U16LE(0x102)) }],
    },
    DetectConditions {
        demux_name: "hl-fmv",
        extensions: ".fmv",
        conditions: &[CheckItem{offs:  0, cond: &CC::Str(b"FMV*") },
                      CheckItem{offs:  4, cond: &CC::Eq(Arg::U32LE(0)) }],
    },
    DetectConditions {
        demux_name: "legend-q",
        extensions: ".q",
        conditions: &[CheckItem{offs: 0, cond: &CC::Eq(Arg::U16LE(0x6839))},
                      CheckItem{offs: 2, cond: &CC::In(Arg::Byte(3), Arg::Byte(7))}],
    },
    DetectConditions {
        demux_name: "siff",
        extensions: ".vb,.vbc,.fcp,.son",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"SIFF")},
                      CheckItem{offs: 4, cond: &CC::Or(
                                    &CC::Or(
                                        &CC::Str(b"VBV1VBHD"),
                                        &CC::Str(b"SOUNSHDR")),
                                    &CC::Str(b"FCPKFCHD"))}],
    },
    DetectConditions {
        demux_name: "smush",
        extensions: ".san",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"ANIM")},
                      CheckItem{offs: 8, cond: &CC::Str(b"AHDR")}],
    },
    DetectConditions {
        demux_name: "smush-mcmp",
        extensions: ".imc",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"MCMP")},
                      CheckItem{offs: 6, cond: &CC::Eq(Arg::Byte(0))},
                      CheckItem{offs: 7, cond: &CC::Eq(Arg::Byte(0))}],
    },
    DetectConditions {
        demux_name: "smush",
        extensions: ".snm",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"SANM")},
                      CheckItem{offs: 8, cond: &CC::Str(b"SHDR")}],
    },
    DetectConditions {
        demux_name: "realaudio",
        extensions: ".ra,.ram",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b".ra\xFD")}],
    },
    DetectConditions {
        demux_name: "realmedia",
        extensions: ".rm,.rmvb,.rma,.ra,.ram",
        conditions: &[CheckItem{offs: 0, cond: &CC::Or(&CC::Str(b".RMF"), &CC::Str(b".RMP")) },
                      CheckItem{offs: 4, cond: &CC::Ge(Arg::U32BE(10))}],
    },
    DetectConditions {
        demux_name: "real_ivr",
        extensions: ".ivr",
        conditions: &[CheckItem{offs: 0, cond: &CC::Or(&CC::Str(b".R1M"), &CC::Str(b".REC"))}],
    },
    DetectConditions {
        demux_name: "bink",
        extensions: ".bik,.bk2",
        conditions: &[CheckItem{offs: 0, cond: &CC::Or(&CC::In(Arg::U32BE(0x42494B62),     // BIKb
                                                               Arg::U32BE(0x42494B7B)),    // BIKz
                                                       &CC::In(Arg::U32BE(0x4B423261),     // KB2a
                                                               Arg::U32BE(0x4B42327B)))}], // KB2z
    },
    DetectConditions {
        demux_name: "smacker",
        extensions: ".smk",
        conditions: &[CheckItem{offs: 0, cond: &CC::Or(&CC::Str(b"SMK2"), &CC::Str(b"SMK4"))}],
    },
    DetectConditions {
        demux_name: "ape",
        extensions: ".ape",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"MAC ") },
                      CheckItem{offs: 4, cond: &CC::In(Arg::U16LE(3800), Arg::U16LE(3990))}],
    },
    DetectConditions {
        demux_name: "flac",
        extensions: ".flac",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"fLaC") }],
    },
    DetectConditions {
        demux_name: "tta",
        extensions: ".tta",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"TTA1") }],
    },
    DetectConditions {
        demux_name: "wavpack",
        extensions: ".wv",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"wvpk") },
                      CheckItem{offs: 8, cond: &CC::In(Arg::U16LE(0x402), Arg::U16LE(0x410))}],
    },
    DetectConditions {
        demux_name: "vivo",
        extensions: ".viv",
        conditions: &[CheckItem{offs: 0, cond: &CC::In(Arg::U16BE(1), Arg::U16BE(0xFF))},
                      CheckItem{offs: 2, cond: &CC::Str(b"\x0D\x0AVersion:Vivo/")}],
    },
    DetectConditions {
        demux_name: "vivo",
        extensions: ".viv",
        conditions: &[CheckItem{offs: 0, cond: &CC::In(Arg::U16BE(1), Arg::U16BE(0xFF))},
                      CheckItem{offs: 3, cond: &CC::Str(b"\x0D\x0AVersion:Vivo/")}],
    },
    DetectConditions {
        demux_name: "bmv",
        extensions: ".bmv",
        conditions: &[],
    },
    DetectConditions {
        demux_name: "bmv3",
        extensions: ".bmv",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"BMVi") },
                      CheckItem{offs: 32, cond: &CC::Str(b"DATA")}],
    },
    DetectConditions {
        demux_name: "sga",
        extensions: ".dtv,.avc",
        conditions: &[],
    },
    DetectConditions {
        demux_name: "vmd",
        extensions: ".vmd",
        conditions: &[],
    },
    DetectConditions {
        demux_name: "vx",
        extensions: ".vx",
        conditions: &[CheckItem{offs: 0, cond: &CC::Str(b"VXDS") }],
    },
];

/// Tries to detect container format.
///
/// This function tries to determine container format using both file extension and checking against container specific markers inside.
/// In case of success the function returns short container name and the detection score.
/// Result should have the highest detection score among tested.
pub fn detect_format(name: &str, src: &mut ByteReader) -> Option<(&'static str, DetectionScore)> {
    let mut result = None;
    let lname = name.to_lowercase();
    for detector in DETECTORS {
        let mut score = DetectionScore::No;
        if !name.is_empty() {
            for ext in detector.extensions.split(',') {
                if lname.ends_with(ext) {
                    score = DetectionScore::ExtensionMatches;
                    break;
                }
            }
        }
        let mut passed = !detector.conditions.is_empty();
        for ck in detector.conditions {
            let ret = src.seek(SeekFrom::Start(u64::from(ck.offs)));
            if ret.is_err() {
                passed = false;
                break;
            }
            if !ck.cond.eval(src) {
                passed = false;
                break;
            }
        }
        if passed {
            score = DetectionScore::MagicMatches;
        }
        if score == DetectionScore::MagicMatches {
            return Some((detector.demux_name, score));
        }
        if result.is_none() && score != DetectionScore::No {
            result = Some((detector.demux_name, score));
        } else if result.is_some() {
            let (_, oldsc) = result.unwrap();
            if oldsc.less(score) {
                result = Some((detector.demux_name, score));
            }
        }
    }
    result
}

/// Tries to detect container format for provided file name.
pub fn detect_format_by_name(name: &str) -> Option<&'static str> {
    if name.is_empty() {
        return None;
    }
    let lname = name.to_lowercase();
    for detector in DETECTORS {
        for ext in detector.extensions.split(',') {
            if lname.ends_with(ext) {
                return Some(detector.demux_name);
            }
        }
    }
    None
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;
    use nihav_core::io::byteio::*;

    #[test]
    fn test_avi_detect() {
        let name = "assets/Indeo/laser05.avi";
        let mut file = File::open(name).unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let (name, score) = detect_format(name, &mut br).unwrap();
        assert_eq!(name, "avi");
        assert_eq!(score, DetectionScore::MagicMatches);
    }

    #[test]
    fn test_gdv_detect() {
        let name = "assets/Game/intro1.gdv";
        let mut file = File::open(name).unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let (name, score) = detect_format(name, &mut br).unwrap();
        assert_eq!(name, "gdv");
        assert_eq!(score, DetectionScore::MagicMatches);
    }
}
