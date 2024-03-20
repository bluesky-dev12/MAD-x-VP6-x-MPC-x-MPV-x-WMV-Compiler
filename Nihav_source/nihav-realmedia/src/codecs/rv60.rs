use nihav_core::formats::YUV420_FORMAT;
use nihav_core::frame::*;
use nihav_core::codecs::{NADecoder, NADecoderSupport, DecoderError, DecoderResult, FrameSkipMode};
use nihav_core::options::*;
use nihav_codec_support::codecs::{MV, ZERO_MV, IPBShuffler};
use nihav_core::io::byteio::{MemoryReader,ByteReader};
use nihav_core::io::bitreader::{BitReader,BitReaderMode};
use nihav_core::io::intcode::*;
use std::str::FromStr;

use super::rv60codes::*;
use super::rv60dsp::*;

struct UniqueList<A> {
    list:       [A; 4],
    fill:       usize,
    max_size:   usize,
}

impl<A:Copy+Default+PartialEq> UniqueList<A> {
    fn new(max_size: usize) -> Self {
        Self { list: [A::default(); 4], fill: 0, max_size }
    }
    fn add(&mut self, cand: A) {
        if self.fill == self.max_size { return; }
        let mut unique = true;
        for el in self.list.iter().take(self.fill) {
            if *el == cand {
                unique = false;
                break;
            }
        }
        if unique {
            self.list[self.fill] = cand;
            self.fill += 1;
        }
    }
}

const RV60_FRAME_TYPES: [FrameType; 4] = [ FrameType::I, FrameType::P, FrameType::B, FrameType::Other ];
const MAX_IMODE: u8 = 34;

#[derive(Clone,Copy,Debug)]
#[allow(dead_code)]
struct FrameHeader {
    profile:            u8,
    ftype:              FrameType,
    qp:                 u8,
    osvquant:           u8,
    ts:                 u32,
    width:              usize,
    awidth:             usize,
    height:             usize,
    aheight:            usize,
    two_f_refs:         bool,
    qp_off_type:        u8,
    deblock:            bool,
    deblock_chroma:     bool,
}

const RV60_CUSTOM_MSG_LENS: [u32; 4] = [ 2, 4, 16, 32 ];
impl FrameHeader {
    fn read(br: &mut BitReader) -> DecoderResult<Self> {
        let marker                                      = br.read(2)?;
        validate!(marker == 3);
        let profile                                     = br.read(2)? as u8;
        validate!(profile == 0);
        let _someval                                    = br.read(4)?;
        let ftypeid                                     = br.read(2)? as usize;
        let ftype = RV60_FRAME_TYPES[ftypeid];
        let qp                                          = br.read(6)? as u8;
        let marker                                      = br.read(1)?;
        validate!(marker == 0);
        let toolset                                     = br.read(2)?;
        validate!(toolset == 0);
        let osvquant                                    = br.read(2)? as u8;
        let _some_flag                                  = br.read_bool()?;
        let _some_val                                   = br.read(2)?;
        let ts                                          = br.read(24)?;
        let width                                       = ((br.read(11)? as usize) + 1) * 4;
        let height                                      = ((br.read(11)? as usize) + 0) * 4;
        validate!(height > 0);
        let awidth  = (width  + 15) & !15;
        let aheight = (height + 15) & !15;
        let _some_flag                                  = br.read_bool()?;
        let two_f_refs;
        if ftype == FrameType::I {
//byte17 = 0
            two_f_refs = false;
        } else {
            let flag                                    = br.read_bool()?;
            if flag { // untested
                                                          br.skip(1)?;
                                                          br.skip(1)?;
                                                          br.skip(1)?;
            }
//byte17 = flag?
            two_f_refs                                  = br.read_bool()?;
        }
// if byte17 { dw40 = 2; dw3C = 2; } else { dw40 = 1; dw3C = 1; }
        let _some_val                                   = br.read_code(UintCodeType::Unary012)?; // luma_qp_diff?
        let chroma_qp_diff                              = br.read(1)?;
        validate!(chroma_qp_diff == 0);
        let qp_off_type                                 = br.read_code(UintCodeType::Unary012)? as u8;
        let deblock                                     = br.read_bool()?;
        let deblock_chroma                              = deblock && !br.read_bool()?;
        if br.read_bool()? {
            let custom_msg_hdr_len                      = br.read(2)? as usize;
            if custom_msg_hdr_len != 0 {
                for i in 0..custom_msg_hdr_len {
                                                          br.skip(RV60_CUSTOM_MSG_LENS[i] * 8)?;
                }
            }
        }

        Ok(FrameHeader {
                profile, ftype, qp, osvquant, ts, width, height, awidth, aheight,
                two_f_refs, qp_off_type, deblock, deblock_chroma,
            })
    }
    fn parse_slice_sizes(&self, br: &mut BitReader, sizes: &mut Vec<usize>) -> DecoderResult<()> {
        let nslices = self.get_height_cu();
        let nbits                                       = (br.read(5)? as u8) + 1;
        validate!(nbits < 32);
        let mut signs: Vec<bool> = Vec::with_capacity(nslices);
        for _ in 0..nslices {
            let sign                                    = br.read_bool()?;
            signs.push(sign);
        }
        validate!(signs[0]);
        sizes.clear();
        let mut sum = 0;
        let first_size                                  = br.read(nbits)? as usize;
        validate!(first_size > 0);
        sum += first_size;
        let mut lastsize = first_size;
        sizes.push(first_size);
        for i in 1..nslices {
            let diff                                    = br.read(nbits)? as isize;
            let size;
            if signs[i] {
                let sum = (lastsize as isize).checked_add(diff);
                validate!(sum.is_some());
                size = sum.unwrap() as usize;
            } else {
                let sum = (lastsize as isize).checked_sub(diff);
                validate!(sum.is_some());
                size = sum.unwrap() as usize;
            }
            sizes.push(size);
            sum += size;
            lastsize = size;
        }
        br.align();
if ((br.left() >> 3) as usize) != sum {
println!(" left {} / {}", br.left() >> 3, sum);
}
        validate!((br.left() >> 3) >= (sum as isize));
        Ok(())
    }
    fn read_line_qp_offset(&self, br: &mut BitReader) -> DecoderResult<i8> {
        match self.qp_off_type {
            0 => Ok(0),
            1 => {
                    let val                             = br.read_code(UintCodeType::Unary012)?;
                    if val != 2 {
                        Ok(val as i8)
                    } else {
                        Ok(-1)
                    }
                },
            _ => {
                    if br.read(1)? == 0 {
                        Ok(0)
                    } else {
                        let val                         = br.read(2)? as i8;
                        if (val & 2) == 0 {
                            Ok(val + 1)
                        } else {
                            Ok(-((val & 1) + 1))
                        }
                    }
                },
        }
    }
    fn get_width_cu(&self) -> usize {
        (self.width + 63) >> 6
    }
    fn get_height_cu(&self) -> usize {
        (self.height + 63) >> 6
    }
    fn has_top_block(&self, xpos: usize, ypos: usize, dx: usize, dy: usize, size: usize) -> bool {
        if (ypos + dy) == 0 { return false; }
        let xpos2 = xpos + dx;
        if (xpos2 + size) > self.awidth { return false; }
        true
    }
    fn has_top_right_block(&self, xpos: usize, ypos: usize, dx: usize, dy: usize, size: usize) -> bool {
        if (ypos + dy) == 0 { return false; }
        let xpos2 = xpos + dx;
        if (xpos2 + size * 2) > self.awidth { return false; }
        let cxpos = ((xpos + dx) & 63) >> RV60_BLOCK_LOG2[size];
        let cypos = ((ypos + dy) & 63) >> RV60_BLOCK_LOG2[size];
        ((cypos as u8) & RV60_AVAIL_MASK[cxpos]) == 0
    }
    fn has_left_block(&self, xpos: usize, ypos: usize, dx: usize, dy: usize, size: usize) -> bool {
        if (xpos + dx) == 0 { return false; }
        let ypos2 = ypos + dy;
        if (ypos2 + size) > self.aheight { return false; }
        true
    }
    fn has_left_down_block(&self, xpos: usize, ypos: usize, dx: usize, dy: usize, size: usize) -> bool {
        if (xpos + dx) == 0 { return false; }
        let ypos2 = ypos + dy;
        if (ypos2 + size * 2) > self.aheight { return false; }
        let cxpos = (!(xpos + dx) & 63) >> RV60_BLOCK_LOG2[size];
        let cypos = (!(ypos + dy) & 63) >> RV60_BLOCK_LOG2[size];
        ((cypos as u8) & RV60_AVAIL_MASK[cxpos]) >= 1
    }
}

const RV60_BLOCK_LOG2: [u8; 65] = [
    0,
    0, 1, 0, 2, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 4,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6
];
const RV60_AVAIL_MASK: [u8; 64] = [
    0, 1, 0, 3, 0, 1, 0, 7, 0, 1, 0, 3, 0, 1, 0, 0xF,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
];

#[derive(Clone,Copy,PartialEq,Debug,Default)]
enum CUType {
    #[default]
    Intra,
    InterMV,
    Skip,
    InterNoMV,
}

const RV60_CU_TYPES: [CUType; 4] = [ CUType::Intra, CUType::InterMV, CUType::Skip, CUType::InterNoMV ];

#[derive(Clone,Copy,PartialEq,Debug,Default)]
enum PUType {
    #[default]
    Full,
    N2Hor,
    N2Ver,
    Quarters,
    N4Hor,
    N34Hor,
    N4Ver,
    N34Ver,
}

const RV60_PU_TYPES: [PUType; 8] = [
    PUType::Full,   PUType::N2Hor,  PUType::N2Ver,  PUType::Quarters,
    PUType::N4Hor,  PUType::N34Hor, PUType::N4Ver,  PUType::N34Ver,
];

impl PUType {
    fn get_num_mvs(self) -> usize {
        match self {
            PUType::Full        => 1,
            PUType::Quarters    => 4,
            _                   => 2,
        }
    }
    fn get_mv_size(self, part_no: usize, size: usize) -> (usize, usize) {
        let mv_size = size >> 2;
        match self {
            PUType::Full        => (mv_size, mv_size),
            PUType::N2Hor       => (mv_size, mv_size >> 1),
            PUType::N2Ver       => (mv_size >> 1, mv_size),
            PUType::Quarters    => (mv_size >> 1, mv_size >> 1),
            PUType::N4Hor       => {
                    if part_no == 0 {
                        (mv_size,      mv_size  >> 2)
                    } else {
                        (mv_size, (3 * mv_size) >> 2)
                    }
                },
            PUType::N34Hor      => {
                    if part_no == 0 {
                        (mv_size, (3 * mv_size) >> 2)
                    } else {
                        (mv_size,      mv_size  >> 2)
                    }
                },
            PUType::N4Ver       => {
                    if part_no == 0 {
                        (     mv_size  >> 2, mv_size)
                    } else {
                        ((3 * mv_size) >> 2, mv_size)
                    }
                },
            PUType::N34Ver      => {
                    if part_no == 0 {
                        ((3 * mv_size) >> 2, mv_size)
                    } else {
                        (     mv_size  >> 2, mv_size)
                    }
                },
        }
    }
    fn has_hor_split(self) -> bool {
        matches!(self, PUType::N2Hor | PUType::N4Hor | PUType::N34Hor | PUType::Quarters)
    }
    fn has_ver_split(self) -> bool {
        matches!(self, PUType::N2Ver | PUType::N4Ver | PUType::N34Ver | PUType::Quarters)
    }
}

#[derive(Clone,Copy,Debug)]
enum IntraMode {
    Index(u8),
    Mode(u8),
    DC64,
    Plane64,
}

#[derive(Clone,Copy,PartialEq,Debug,Default)]
enum TransformType {
    #[default]
    None,
    T4X4,
    T8X8,
    T16X16,
}

#[derive(Clone,Copy,PartialEq,Debug)]
enum MVRef {
    None,
    Ref0,
    Ref1,
    BRef,
    Ref0AndBRef,
    Skip0,
    Skip1,
    Skip2,
    Skip3,
}

const SKIP_MV_REF: [MVRef; 4] = [ MVRef::Skip0,  MVRef::Skip1, MVRef::Skip2,  MVRef::Skip3 ];

impl MVRef {
    fn get_skip_mv_num(self) -> usize {
        match self {
            MVRef::Skip1    => 1,
            MVRef::Skip2    => 2,
            MVRef::Skip3    => 3,
            _               => 0,
        }
    }
    fn is_ref0(self) -> bool {
        matches!(self, MVRef::Ref0 | MVRef::Ref0AndBRef)
    }
    fn is_fwd(self) -> bool {
        matches!(self, MVRef::Ref0 | MVRef::Ref1 | MVRef::Ref0AndBRef)
    }
    fn is_bwd(self) -> bool {
        matches!(self, MVRef::BRef | MVRef::Ref0AndBRef)
    }
}

#[derive(Clone,Copy,PartialEq,Debug)]
struct MVInfo {
    f_mv:   MV,
    b_mv:   MV,
    mvref:  MVRef,
}

impl MVInfo {
    fn is_some(&self) -> bool { self.mvref != MVRef::None }
    fn matches_fwd(&self, mvref: MVRef) -> bool {
        (self.mvref == mvref) || (self.mvref.is_ref0() && mvref.is_ref0())
    }
    fn matches_bwd(&self, mvref: MVRef) -> bool {
        self.mvref.is_bwd() && mvref.is_bwd()
    }
    fn is_deblock_cand(&self, other: &MVInfo) -> bool {
        if self.mvref != other.mvref { return true; }
        let mut mvdiff = 0;
        if self.mvref.is_fwd() {
            let diff = self.f_mv - other.f_mv;
            mvdiff += diff.x.abs() + diff.y.abs();
        }
        if self.mvref.is_bwd() {
            let diff = self.b_mv - other.b_mv;
            mvdiff += diff.x.abs() + diff.y.abs();
        }
        mvdiff > 4
    }
}

impl Default for MVInfo {
    fn default() -> Self { Self { f_mv: ZERO_MV, b_mv: ZERO_MV, mvref: MVRef::None } }
}

#[derive(Clone,Copy,Debug)]
struct CBHeader {
    cu_type:        CUType,
    pu_type:        PUType,
    ttype:          TransformType,
    imode:          [IntraMode; 4],
    mv:             [MVInfo; 4],
}

impl CBHeader {
    fn read(br: &mut BitReader, ftype: FrameType, two_f_refs: bool, size: usize) -> DecoderResult<Self> {
        let cu_type;
        let pu_type;
        let mut imode: [IntraMode; 4] = [IntraMode::Index(0); 4];
        let mut mv: [MVInfo; 4] = [MVInfo::default(); 4];
        if ftype == FrameType::I {
            cu_type = CUType::Intra;
        } else {
            cu_type                                     = RV60_CU_TYPES[br.read(2)? as usize];
        }
        match cu_type {
            CUType::Intra   => {
                    if (size == 8) && br.read_bool()? {
                        pu_type = PUType::Quarters;
                    } else {
                        pu_type = PUType::Full;
                    }
                    if pu_type == PUType::Quarters {
                        for i in 0..4 {
                            imode[i] = CBHeader::read_intra_mode(br)?;
                        }
                    } else if size <= 32 {
                        imode[0] = CBHeader::read_intra_mode(br)?;
                    } else {
                        if !br.read_bool()? {
                            imode[0] = IntraMode::DC64;
                        } else {
                            imode[0] = IntraMode::Plane64;
                        }
                    }
                },
            CUType::InterMV => {
                    let bits = if size == 8 { 2 } else { 3 };
                    pu_type                             = RV60_PU_TYPES[br.read(bits)? as usize];
                    CBHeader::read_mv_data(br, ftype, two_f_refs, size, pu_type, &mut mv)?;
                },
            _               => {
                    pu_type = PUType::Full;
                    let skip_mv_no                     = br.read_code(UintCodeType::LimitedUnary(3, 0))?;
                    mv[0].mvref = SKIP_MV_REF[skip_mv_no as usize];
                },
        };
        let ttype;
        if cu_type == CUType::Skip {
            ttype = TransformType::None;
        } else if size >= 32 {
            ttype = TransformType::T16X16;
        } else if size == 16 {
            if (cu_type == CUType::Intra) || (pu_type == PUType::Full) {
                ttype = TransformType::T16X16;
            } else {
                ttype = TransformType::T4X4;
            }
        } else {
            if pu_type == PUType::Full {
                ttype = TransformType::T8X8;
            } else {
                ttype = TransformType::T4X4;
            }
        }
        Ok(Self {
                cu_type, pu_type, ttype, imode, mv,
            })
    }
    fn read_intra_mode(br: &mut BitReader) -> DecoderResult<IntraMode> {
        if br.read_bool()? {
            let idx                                     = br.read_code(UintCodeType::Unary012)? as u8;
            Ok(IntraMode::Index(idx))
        } else {
            let mode                                    = br.read(5)? as u8;
            Ok(IntraMode::Mode(mode))
        }
    }
    fn read_mv_data(br: &mut BitReader, ftype: FrameType, two_f_refs: bool, size: usize, pu_type: PUType, mv: &mut [MVInfo; 4]) -> DecoderResult<()> {
        let mv_count = pu_type.get_num_mvs();
        for i in 0..mv_count {
            mv[i] = CBHeader::read_mv_info(br, ftype, two_f_refs, size, pu_type)?;
        }
        Ok(())
    }
    fn read_mv_info(br: &mut BitReader, ftype: FrameType, two_f_refs: bool, size: usize, pu_type: PUType) -> DecoderResult<MVInfo> {
        let mut f_mv = ZERO_MV;
        let mut b_mv = ZERO_MV;
        let mvref;
        if ftype != FrameType::B {
            if two_f_refs && br.read_bool()? {
                mvref = MVRef::Ref1;
            } else {
                mvref = MVRef::Ref0;
            }
            f_mv = CBHeader::read_mv(br)?;
            Ok(MVInfo { f_mv, b_mv: ZERO_MV, mvref })
        } else {
            if ((size <= 8) && ((size != 8) || (pu_type != PUType::Full))) || br.read_bool()? {
                if !br.read_bool()? {
                    mvref   = MVRef::Ref0;
                    f_mv    = CBHeader::read_mv(br)?;
                } else {
                    mvref   = MVRef::BRef;
                    b_mv    = CBHeader::read_mv(br)?;
                }
            } else {
                mvref   = MVRef::Ref0AndBRef;
                f_mv    = CBHeader::read_mv(br)?;
                b_mv    = CBHeader::read_mv(br)?;
            }
            Ok(MVInfo { f_mv, b_mv, mvref })
        }
    }
    fn read_mv(br: &mut BitReader) -> DecoderResult<MV> {
        let x                                           = br.read_code_signed(IntCodeType::Gamma)? as i16;
        let y                                           = br.read_code_signed(IntCodeType::Gamma)? as i16;
        Ok(MV { x, y })
    }
}

#[derive(Clone,Copy,Default)]
struct PUInfo {
    cu_type:    CUType,
    ttype:      TransformType,
    pu_type:    PUType,
}

impl PUInfo {
    fn is_intra(self) -> bool { self.cu_type == CUType::Intra }
}

const RV60_CANDIDATE_INTRA_ANGLES: [u8; 6] = [ 0, 1, 10, 26, 18, 2 ];

#[derive(Clone,Copy,Default)]
struct BlockInfo {
    mv:         MVInfo,
    imode:      u8,
}

struct DeblockInfo {
    left_str:   Vec<u8>,
    top_str:    Vec<u8>,
    stride:     usize,
}

impl DeblockInfo {
    fn new() -> Self {
        Self { left_str: Vec::new(), top_str: Vec::new(), stride: 0 }
    }
    fn reinit(&mut self, w: usize, h: usize) {
        self.left_str.clear();
        self.top_str.clear();
        self.stride = w >> 2;
        let size = self.stride * (h >> 2);
        self.left_str.resize(size, 0);
        self.top_str.resize(size, 0);
    }
    fn set_strength(&mut self, xpos: usize, ypos: usize, size: usize, q: u8, strength: u8) {
        let pos = self.get_pos(xpos, ypos);
        let dsize = size >> 2;
        let dval = (q << 2) | strength;
        for x in 0..dsize {
            self.top_str[pos + x] = dval;
            self.top_str[pos + (dsize - 1) * self.stride + x] = dval;
        }
        for y in 0..dsize {
            self.left_str[pos + y * self.stride] = dval;
            self.left_str[pos + y * self.stride + dsize - 1] = dval;
        }
    }
    fn get_pos(&self, xpos: usize, ypos: usize) -> usize {
        (xpos >> 2) + (ypos >> 2) * self.stride
    }
    fn get_top_strength(&self, pos: usize) -> u8 {
        self.top_str[pos] & 3
    }
    fn get_left_strength(&self, pos: usize) -> u8 {
        self.left_str[pos] & 3
    }
    fn set_top_strength(&mut self, pos: usize, strength: u8) {
        self.top_str[pos] |= strength;
    }
    fn set_left_strength(&mut self, pos: usize, strength: u8) {
        self.left_str[pos] |= strength;
    }
}

struct RealVideo60Decoder {
    info:       NACodecInfoRef,
    cbs:        RV60Codebooks,
    ipbs:       IPBShuffler,
    dsp:        RV60DSP,
    ipred:      IntraPredContext,
    skip_mode:  FrameSkipMode,

    avg_buf:    NAVideoBufferRef<u8>,

    y_coeffs:   [i16; 16 * 16],
    u_coeffs:   [i16; 8 * 8],
    v_coeffs:   [i16; 8 * 8],
    qp:         u8,
    sel_qp:     u8,

    cu_splits:  Vec<bool>,
    coded_blk:  [bool; 64],
    dblk:       DeblockInfo,

    pu_info:    Vec<PUInfo>,
    pu_stride:  usize,
    pu_pos:     usize,

    blk_info:   Vec<BlockInfo>,
    blk_stride: usize,
    blk_pos:    usize,

    xpos:       usize,
    ypos:       usize,

    ts_scale:   u64,
    ref0_pts:   u64,
    ref1_pts:   u64,
    ref0_ts:    u64,
    ref1_ts:    u64,
}

impl RealVideo60Decoder {
    fn new() -> Self {
        let tmp_vinfo = NAVideoInfo::new(64, 64, false, YUV420_FORMAT);
        let vt = alloc_video_buffer(tmp_vinfo, 4).unwrap();
        let vb = vt.get_vbuf();
        let avg_buf = vb.unwrap();
        RealVideo60Decoder{
            info:       NACodecInfoRef::default(),
            cbs:        RV60Codebooks::init(),
            ipbs:       IPBShuffler::new(),
            ipred:      IntraPredContext::new(),
            skip_mode:  FrameSkipMode::default(),
            dsp:        RV60DSP::new(),
            avg_buf,
            y_coeffs:   [0; 16 * 16],
            u_coeffs:   [0; 8 * 8],
            v_coeffs:   [0; 8 * 8],
            qp:         0,
            sel_qp:     0,
            cu_splits:  Vec::with_capacity(24),
            coded_blk:  [false; 64],
            dblk:       DeblockInfo::new(),
            pu_info:    Vec::new(),
            pu_stride:  0,
            pu_pos:     0,
            blk_info:   Vec::new(),
            blk_stride: 0,
            blk_pos:    0,
            xpos:       0,
            ypos:       0,

            ts_scale:   1,
            ref0_pts:   0,
            ref1_pts:   0,
            ref0_ts:    0,
            ref1_ts:    0,
        }
    }
    fn decode_cu_line(&mut self, buf: &mut NASimpleVideoFrame<u8>, hdr: &FrameHeader, src: &[u8], cu_y: usize) -> DecoderResult<()> {
        let mut br = BitReader::new(src, BitReaderMode::BE);
        let cu_w = hdr.get_width_cu();
        for cu_x in 0..cu_w {
            let dqp = hdr.read_line_qp_offset(&mut br)?;
            let qps = (hdr.qp as i8) + dqp;
            validate!((0..32).contains(&qps));
            let qp = qps as u8;
            self.qp = qp;
            self.sel_qp = match hdr.osvquant {
                0 => qp,
                1 => {
                        if qp <= 25 {
                            qp + 5
                        } else {
                            qp
                        }
                    },
                _ => {
                        if qp <= 18 {
                            qp + 10
                        } else if qp <= 25 {
                            qp + 5
                        } else {
                            qp
                        }
                    },
            };

            self.cu_splits.clear();
            self.coded_blk = [false; 64];
            self.decode_cb_tree(buf, hdr, &mut br, cu_x << 6, cu_y << 6, 6)?;
            if hdr.deblock {
                self.cu_splits.reverse();
                self.deblock_cb_tree(buf, hdr, cu_x << 6, cu_y << 6, 6);
            }
        }
if br.left() >= 8 {
println!(" left {} bits", br.left());
}
        Ok(())
    }
    #[allow(clippy::cognitive_complexity)]
    fn decode_cb_tree(&mut self, buf: &mut NASimpleVideoFrame<u8>, hdr: &FrameHeader, br: &mut BitReader, xpos: usize, ypos: usize, log_size: u8) -> DecoderResult<()> {
        if (xpos >= hdr.awidth) || (ypos >= hdr.aheight) { return Ok(()); }

        let size = 1 << log_size;
        let split = (xpos + size > hdr.awidth) || (ypos + size > hdr.aheight) || (size > 8 && br.read_bool()?);
        self.cu_splits.push(split);
        if split {
            let hsize = size >> 1;
            self.decode_cb_tree(buf, hdr, br, xpos,         ypos,         log_size - 1)?;
            self.decode_cb_tree(buf, hdr, br, xpos + hsize, ypos,         log_size - 1)?;
            self.decode_cb_tree(buf, hdr, br, xpos,         ypos + hsize, log_size - 1)?;
            self.decode_cb_tree(buf, hdr, br, xpos + hsize, ypos + hsize, log_size - 1)?;
        } else {
            let cbh = CBHeader::read(br, hdr.ftype, hdr.two_f_refs, size)?;
            self.pu_pos = (xpos >> 3) + (ypos >> 3) * self.pu_stride;
            self.blk_pos = (xpos >> 2) + (ypos >> 2) * self.blk_stride;
            self.xpos = xpos;
            self.ypos = ypos;
            self.reconstruct_info(hdr, &cbh, size)?;

            let split_i4x4 = (cbh.cu_type == CUType::Intra) && (size == 8) && (cbh.pu_type == PUType::Quarters);
            match cbh.cu_type {
                CUType::Intra => {
                        let itype = self.blk_info[self.blk_pos].imode;
                        if !split_i4x4 {
                            let dstride = buf.stride[0];
                            let off = xpos + ypos * dstride;
                            let dst = &mut buf.data;
                            self.populate_ipred(hdr, dst, 0, dstride, 0, 0, size, true);
                            self.ipred.pred_angle(dst, off, dstride, size, itype as usize, true);
                        }
                        for comp in 1..3 {
                            let dstride = buf.stride[comp];
                            let soff = buf.offset[comp];
                            let off = soff + (xpos >> 1) + (ypos >> 1) * dstride;
                            let dst = &mut buf.data;
                            self.populate_ipred(hdr, dst, soff, dstride, 0, 0, size >> 1, false);
                            self.ipred.pred_angle(dst, off, dstride, size >> 1, itype as usize, false);
                        }
                    },
                _ => {
                        let mut mv_x = xpos >> 2;
                        let mut mv_y = ypos >> 2;
                        let mut mv_pos = mv_x + mv_y * self.blk_stride;
                        for part_no in 0..cbh.pu_type.get_num_mvs() {
                            let (mv_w, mv_h) = cbh.pu_type.get_mv_size(part_no, size);
                            let mv = self.blk_info[mv_pos].mv;
                            let bw = mv_w << 2;
                            let bh = mv_h << 2;
                            let bx = mv_x << 2;
                            let by = mv_y << 2;
                            match mv.mvref {
                                MVRef::Ref0 => {
                                        if hdr.ftype != FrameType::B {
                                            if let Some(ref prevbuf) = self.ipbs.get_lastref() {
                                                self.dsp.do_mc(buf, prevbuf, bx, by, bw, bh, mv.f_mv, false);
                                            }
                                        } else {
                                            if let Some(ref prevbuf) = self.ipbs.get_b_fwdref() {
                                                self.dsp.do_mc(buf, prevbuf, bx, by, bw, bh, mv.f_mv, false);
                                            }
                                        }
                                    },
                                MVRef::Ref1 => {
                                        if let Some(ref prevbuf) = self.ipbs.get_nextref() {
                                            self.dsp.do_mc(buf, prevbuf, bx, by, bw, bh, mv.f_mv, false);
                                        }
                                    },
                                MVRef::BRef => {
                                        validate!(hdr.ftype == FrameType::B);
                                        if let Some(ref prevbuf) = self.ipbs.get_b_bwdref() {
                                            self.dsp.do_mc(buf, prevbuf, bx, by, bw, bh, mv.b_mv, false);
                                        }
                                    },
                                MVRef::Ref0AndBRef => {
                                        validate!(hdr.ftype == FrameType::B);
                                        if let (Some(ref prevbuf), Some(ref nextbuf)) = (self.ipbs.get_b_fwdref(), self.ipbs.get_b_bwdref()) {
                                            self.dsp.do_mc(buf, prevbuf, bx, by, bw, bh, mv.f_mv, false);
                                            {
                                                let mut avg_buf = NASimpleVideoFrame::from_video_buf(&mut self.avg_buf).unwrap();
                                                self.dsp.do_mc(&mut avg_buf, nextbuf, bx, by, bw, bh, mv.b_mv, true);
                                            }
                                            self.dsp.do_avg(buf, &self.avg_buf, bx, by, bw, bh);
                                        }
                                    },
                                _ => unreachable!(),
                            };
                            if cbh.pu_type == PUType::Quarters {
                                if part_no != 1 {
                                    mv_pos += mv_w;
                                    mv_x   += mv_w;
                                } else {
                                    mv_pos += mv_h * self.blk_stride - mv_w;
                                    mv_x   -= mv_w;
                                    mv_y   += mv_h;
                                }
                            } else if cbh.pu_type.has_hor_split() {
                                mv_pos += mv_h * self.blk_stride;
                                mv_y   += mv_h;
                            } else if cbh.pu_type.has_ver_split() {
                                mv_pos += mv_w;
                                mv_x   += mv_w;
                            }
                        }
                    },
            };
            if cbh.ttype != TransformType::None {
                self.y_coeffs = [0; 16 * 16];
                self.u_coeffs = [0; 8 * 8];
                self.v_coeffs = [0; 8 * 8];
            }
            let is_intra = cbh.cu_type == CUType::Intra;
            let cb_pos = ((xpos & 63) >> 3) + ((ypos & 63) >> 3) * 8;
            match cbh.ttype {
                TransformType::T4X4 => {
                        let subset = if is_intra { 0 } else { 2 };
                        if size == 16 {
                            let cbp16;
                            if br.read_bool()? {
                                cbp16 = rv6_decode_cbp16(br, &self.cbs, subset, self.sel_qp)?;
                            } else {
                                cbp16 = 0;
                            }
                            if cbp16 != 0 {
                                rv6_decode_cu_4x4in16x16(br, &self.cbs, is_intra, self.qp, self.sel_qp, &mut self.y_coeffs, &mut self.u_coeffs, &mut self.v_coeffs, cbp16)?;
                                for y in 0..4 {
                                    for x in 0..4 {
                                        let i = x + y * 4;
                                        if ((cbp16 >> i) & 1) != 0 {
                                            self.dsp.transform4x4(&mut self.y_coeffs[i * 16..][..16]);
                                            let dstride = buf.stride[0];
                                            let off = xpos + x * 4 + (ypos + y * 4) * dstride;
                                            let dst = &mut buf.data;
                                            self.dsp.add_block(dst, off, dstride, &self.y_coeffs[i*16..][..16], 4);
                                            self.coded_blk[cb_pos + (y / 2) * 8 + (x / 2)] = true;
                                        }
                                    }
                                }
                                for y in 0..2 {
                                    for x in 0..2 {
                                        let i = x + y * 2;
                                        let xoff = (xpos >> 1) + x * 4;
                                        let yoff = (ypos >> 1) + y * 4;
                                        if ((cbp16 >> (16 + i)) & 1) != 0 {
                                            self.dsp.transform4x4(&mut self.u_coeffs[i * 16..][..16]);
                                            let dstride = buf.stride[1];
                                            let off = buf.offset[1] + xoff + yoff * dstride;
                                            let dst = &mut buf.data;
                                            self.dsp.add_block(dst, off, dstride, &self.u_coeffs[i * 16..][..16], 4);
                                            self.coded_blk[cb_pos + y * 8 + x] = true;
                                        }
                                        if ((cbp16 >> (20 + i)) & 1) != 0 {
                                            self.dsp.transform4x4(&mut self.v_coeffs[i * 16..][..16]);
                                            let dstride = buf.stride[2];
                                            let off = buf.offset[2] + xoff + yoff * dstride;
                                            let dst = &mut buf.data;
                                            self.dsp.add_block(dst, off, dstride, &self.v_coeffs[i * 16..][..16], 4);
                                            self.coded_blk[cb_pos + y * 8 + x] = true;
                                        }
                                    }
                                }
                            }
                        } else {
                            let cbp8 = rv6_decode_cbp8(br, &self.cbs, subset, self.sel_qp)?;
                            if cbp8 != 0 {
                                self.coded_blk[cb_pos] = true;
                                rv6_decode_cu_8x8(br, &self.cbs, is_intra, self.qp, self.sel_qp, &mut self.y_coeffs, &mut self.u_coeffs, &mut self.v_coeffs, cbp8, true)?;
                            }
                            for i in 0..4 {
                                let xoff = (i & 1) * 4;
                                let yoff = (i & 2) * 2;
                                if split_i4x4 {
                                    let dstride = buf.stride[0];
                                    let off = xpos + xoff + (ypos + yoff) * dstride;
                                    let dst = &mut buf.data;
                                    self.populate_ipred(hdr, dst, 0, dstride, xoff, yoff, 4, true);
                                    let itype = self.blk_info[self.blk_pos + (i & 1) + (i >> 1) * self.blk_stride].imode;
                                    self.ipred.pred_angle(dst, off, dstride, 4, itype as usize, true);
                                }
                                if ((cbp8 >> i) & 1) != 0 {
                                    let blk = &mut self.y_coeffs[i * 16..][..16];
                                    self.dsp.transform4x4(blk);
                                    let dstride = buf.stride[0];
                                    let soff = buf.offset[0];
                                    let off = soff + xpos + xoff + (ypos + yoff) * dstride;
                                    self.dsp.add_block(buf.data, off, dstride, blk, 4);
                                }
                            }
                            if ((cbp8 >> 4) & 1) != 0 {
                                self.dsp.transform4x4(&mut self.u_coeffs);
                                let dstride = buf.stride[1];
                                let soff = buf.offset[1];
                                let off = soff + (xpos >> 1) + (ypos >> 1) * dstride;
                                self.dsp.add_block(buf.data, off, dstride, &self.u_coeffs, 4);
                            }
                            if ((cbp8 >> 5) & 1) != 0 {
                                self.dsp.transform4x4(&mut self.v_coeffs);
                                let dstride = buf.stride[2];
                                let soff = buf.offset[2];
                                let off = soff + (xpos >> 1) + (ypos >> 1) * dstride;
                                self.dsp.add_block(buf.data, off, dstride, &self.v_coeffs, 4);
                            }
                        }
                    },
                TransformType::T8X8 => {
                        let subset = if is_intra { 1 } else { 3 };
                        let cbp8 = rv6_decode_cbp8(br, &self.cbs, subset, self.sel_qp)?;
                        if cbp8 != 0 {
                            self.coded_blk[cb_pos] = true;
                            rv6_decode_cu_8x8(br, &self.cbs, is_intra, self.qp, self.sel_qp, &mut self.y_coeffs, &mut self.u_coeffs, &mut self.v_coeffs, cbp8, false)?;
                            if (cbp8 & 0xF) != 0 {
                                self.dsp.transform8x8(&mut self.y_coeffs);
                                let dstride = buf.stride[0];
                                let off = xpos + ypos * dstride;
                                self.dsp.add_block(buf.data, off, dstride, &self.y_coeffs, 8);
                            }
                            if ((cbp8 >> 4) & 1) != 0 {
                                self.dsp.transform4x4(&mut self.u_coeffs);
                                let dstride = buf.stride[1];
                                let soff = buf.offset[1];
                                let off = soff + (xpos >> 1) + (ypos >> 1) * dstride;
                                self.dsp.add_block(buf.data, off, dstride, &self.u_coeffs, 4);
                            }
                            if ((cbp8 >> 5) & 1) != 0 {
                                self.dsp.transform4x4(&mut self.v_coeffs);
                                let dstride = buf.stride[2];
                                let soff = buf.offset[2];
                                let off = soff + (xpos >> 1) + (ypos >> 1) * dstride;
                                self.dsp.add_block(buf.data, off, dstride, &self.v_coeffs, 4);
                            }
                        }
                    },
                TransformType::T16X16 => {
                        let subset = if is_intra { 1 } else { 3 };
                        let num_clusters = size >> 4;
                        let cl_cbp                      = br.read((num_clusters * num_clusters) as u8)?;
                        for y in 0..num_clusters {
                            for x in 0..num_clusters {
                                if ((cl_cbp >> (x + y * num_clusters)) & 1) == 0 { continue; }
                                self.coded_blk[cb_pos + x * 2 + y * 2 * 8 + 0] = true;
                                self.coded_blk[cb_pos + x * 2 + y * 2 * 8 + 1] = true;
                                self.coded_blk[cb_pos + x * 2 + y * 2 * 8 + 8] = true;
                                self.coded_blk[cb_pos + x * 2 + y * 2 * 8 + 9] = true;
                                let super_cbp = rv6_decode_cbp16(br, &self.cbs, subset, self.sel_qp)?;
                                if super_cbp != 0 {
                                    self.y_coeffs = [0; 16 * 16];
                                    self.u_coeffs = [0; 8 * 8];
                                    self.v_coeffs = [0; 8 * 8];
                                    rv6_decode_cu_16x16(br, &self.cbs, is_intra, self.qp, self.sel_qp, &mut self.y_coeffs, &mut self.u_coeffs, &mut self.v_coeffs, super_cbp)?;
                                    if (super_cbp & 0xFFFF) != 0 {
                                        self.dsp.transform16x16(&mut self.y_coeffs);
                                        let dstride = buf.stride[0];
                                        let off = xpos + x * 16 + (ypos + y * 16) * dstride;
                                        self.dsp.add_block(buf.data, off, dstride, &self.y_coeffs, 16);
                                    }
                                    if ((super_cbp >> 16) & 0xF) != 0 {
                                        self.dsp.transform8x8(&mut self.u_coeffs);
                                        let dstride = buf.stride[1];
                                        let soff = buf.offset[1];
                                        let off = soff + (xpos >> 1) + x * 8 + ((ypos >> 1) + y * 8) * dstride;
                                        self.dsp.add_block(buf.data, off, dstride, &self.u_coeffs, 8);
                                    }
                                    if ((super_cbp >> 20) & 0xF) != 0 {
                                        self.dsp.transform8x8(&mut self.v_coeffs);
                                        let dstride = buf.stride[2];
                                        let soff = buf.offset[2];
                                        let off = soff + (xpos >> 1) + x * 8 + ((ypos >> 1) + y * 8) * dstride;
                                        self.dsp.add_block(buf.data, off, dstride, &self.v_coeffs, 8);
                                    }
                                }
                            }
                        }
                    },
                _ => {},
            };
        }
        Ok(())
    }
    fn reconstruct_info(&mut self, hdr: &FrameHeader, cbh: &CBHeader, size: usize) -> DecoderResult<()>{
        let mut pui = PUInfo::default();
        let pu_size = size >> 3;
        pui.cu_type = cbh.cu_type;
        pui.ttype   = cbh.ttype;
        pui.pu_type = cbh.pu_type;
        if (cbh.cu_type == CUType::Intra) && (cbh.pu_type == PUType::Quarters) { // very special case
            self.pu_info[self.pu_pos] = pui;
            for y in 0..2 {
                for x in 0..2 {
                    let imode = self.reconstruct_intra(hdr, cbh, 4, x + y * 2);
                    validate!(imode <= MAX_IMODE);
                    self.blk_info[self.blk_pos + x + y * self.blk_stride].imode = imode;
                    self.blk_info[self.blk_pos + x + y * self.blk_stride].mv = MVInfo::default();
                }
            }
            return Ok(());
        }
        match cbh.cu_type {
            CUType::Intra   => {
                    self.pu_info[self.pu_pos] = pui;
                    let imode = self.reconstruct_intra(hdr, cbh, size, 0);
                    validate!(imode <= MAX_IMODE);
                    for y in 0..(size >> 2) {
                        for x in 0..(size >> 2) {
                            self.blk_info[self.blk_pos + x + y * self.blk_stride].imode = imode;
                        }
                    }
                },
            CUType::InterMV => {
                    let mut mv_x = self.xpos >> 2;
                    let mut mv_y = self.ypos >> 2;
                    let mut mv_pos = self.blk_pos;
                    let pu_type = cbh.pu_type;
                    for part_no in 0..pu_type.get_num_mvs() {
                        let (mv_w, mv_h) = pu_type.get_mv_size(part_no, size);
                        let mv = self.predict_mv(hdr, mv_x, mv_y, mv_w, &cbh.mv[part_no]);
                        for y in 0..mv_h {
                            for x in 0..mv_w {
                                self.blk_info[mv_pos + x + y * self.blk_stride].mv = mv;
                            }
                        }
                        if pu_type == PUType::Quarters {
                            if part_no != 1 {
                                mv_pos += mv_w;
                                mv_x   += mv_w;
                            } else {
                                mv_pos += mv_h * self.blk_stride - mv_w;
                                mv_x   -= mv_w;
                                mv_y   += mv_h;
                            }
                        } else if pu_type.has_hor_split() {
                            mv_pos += mv_h * self.blk_stride;
                            mv_y   += mv_h;
                        } else if pu_type.has_ver_split() {
                            mv_pos += mv_w;
                            mv_x   += mv_w;
                        }
                    }
                },
            _               => {
                    let skip_idx = cbh.mv[0].mvref.get_skip_mv_num();
                    let mut skip_cand: UniqueList<MVInfo> = UniqueList::new(4);
                    self.fill_skip_cand(hdr, &mut skip_cand, size);
                    let mv = skip_cand.list[skip_idx];

                    let mv_size = size >> 2;
                    for y in 0..mv_size {
                        for x in 0..mv_size {
                            self.blk_info[self.blk_pos + x + y * self.blk_stride].mv = mv;
                        }
                    }
                },
        };
        for y in 0..pu_size {
            for x in 0..pu_size {
                self.pu_info[self.pu_pos + x + y * self.pu_stride] = pui;
            }
        }
        Ok(())
    }
    fn reconstruct_intra(&self, hdr: &FrameHeader, cbh: &CBHeader, size: usize, sub: usize) -> u8 {
        match cbh.imode[0] {
            IntraMode::DC64     => { return 1; },
            IntraMode::Plane64  => { return 0; },
            _ => {},
        };
        // form list of predictors
        let blk_pos = self.blk_pos + (sub & 1) + (sub >> 1) * self.blk_stride;
        let mut ipm_cand: UniqueList<u8> = UniqueList::new(3);
        if hdr.has_top_block(self.xpos, self.ypos, (sub & 1) * 4, 0, size) {
            let pu = &self.pu_info[self.pu_pos - self.pu_stride];
            if pu.is_intra() {
                ipm_cand.add(self.blk_info[self.blk_pos + (sub & 1) - self.blk_stride].imode);
            }
        }
        if hdr.has_left_block(self.xpos, self.ypos, 0, (sub & 2) * 2, size) {
            let pu = &self.pu_info[self.pu_pos - 1];
            if pu.is_intra() {
                ipm_cand.add(self.blk_info[blk_pos - 1 - (sub & 1)].imode);
            }
        }
        let tl_x = if (sub & 2) == 0 { self.xpos + (sub & 1) * 4 } else { self.xpos };
        let tl_y = self.ypos + (sub & 2) * 4;
        if (tl_x > 0) && (tl_y > 0) {
            let pu = match sub {
                    0 => &self.pu_info[self.pu_pos - self.pu_stride - 1],
                    1 => &self.pu_info[self.pu_pos - self.pu_stride],
                    2 => &self.pu_info[self.pu_pos - 1],
                    _ => &self.pu_info[self.pu_pos - 1],
                };
            if pu.is_intra() {
                if sub != 3 {
                    ipm_cand.add(self.blk_info[blk_pos - self.blk_stride - 1].imode);
                } else {
                    ipm_cand.add(self.blk_info[blk_pos - self.blk_stride - 2].imode);
                }
            }
        }
        for el in RV60_CANDIDATE_INTRA_ANGLES.iter() {
            ipm_cand.add(*el);
        }
        // actually decode prediction mode
        match cbh.imode[sub] {
            IntraMode::Index(idx) => {
                    ipm_cand.list[idx as usize]
                },
            IntraMode::Mode(mode) => {
                    let mut imode = mode;
                    let mut ipm_cs: [u8; 3] = [ipm_cand.list[0], ipm_cand.list[1], ipm_cand.list[2]];
                    ipm_cs.sort();
                    for ic in ipm_cs.iter() {
                        if imode >= *ic {
                            imode += 1;
                        }
                    }
                    imode
                },
            _ => unreachable!(),
        }
    }
    fn populate_ipred(&mut self, hdr: &FrameHeader, src: &[u8], soff: usize, stride: usize, xoff: usize, yoff: usize, size: usize, is_luma: bool) {
        let src_off = if is_luma {
                soff + self.xpos + xoff + (self.ypos + yoff) * stride
            } else {
                soff + (self.xpos >> 1) + (self.ypos >> 1) * stride
            };
        self.ipred = IntraPredContext::new();
        if (self.ypos + yoff) > 0 {
            self.ipred.has_t = true;
            for x in 0..size {
                self.ipred.t[x + 1] = src[src_off - stride + x];
            }
            if (is_luma && hdr.has_top_right_block(self.xpos, self.ypos, xoff, yoff, size)) ||
               (!is_luma && hdr.has_top_right_block(self.xpos, self.ypos, 0, 0, size << 1)) {
                self.ipred.has_tr = true;
                for x in size..size*2 {
                    self.ipred.t[x + 1] = src[src_off - stride + x];
                }
            } else {
                for i in 0..size {
                    self.ipred.t[size + i + 1] = self.ipred.t[size];
                }
            }
            if (self.xpos + xoff) > 0 {
                self.ipred.t[0] = src[src_off - stride - 1];
            }
        }
        if (self.xpos + xoff) > 0 {
            self.ipred.has_l = true;
            for y in 0..size {
                self.ipred.l[y + 1] = src[src_off - 1 + y * stride];
            }
            if (is_luma && hdr.has_left_down_block(self.xpos, self.ypos, xoff, yoff, size)) ||
               (!is_luma && hdr.has_left_down_block(self.xpos, self.ypos, 0, 0, size << 1)) {
                self.ipred.has_ld = true;
                for y in size..size*2 {
                    self.ipred.l[y + 1] = src[src_off - 1 + y * stride];
                }
            } else {
                for i in 0..size {
                    self.ipred.l[size + i + 1] = self.ipred.l[size];
                }
            }
            if (self.ypos + yoff) > 0 {
                self.ipred.l[0] = src[src_off - stride - 1];
            }
        }
    }
    fn predict_mv(&self, hdr: &FrameHeader, mv_x: usize, mv_y: usize, mv_w: usize, mvi: &MVInfo) -> MVInfo {
        let mv_pos = mv_x + mv_y * self.blk_stride;
        let f_mv: MV;
        let b_mv: MV;
        if mvi.mvref.is_fwd() {
            let mut mv_cand: [MV; 3] = [ZERO_MV; 3];
            let mut mv_cand_size: usize = 0;
            if mv_x > 0 {
                let ref_mv = &self.blk_info[mv_pos - 1].mv;
                if ref_mv.matches_fwd(mvi.mvref) {
                    mv_cand[mv_cand_size] = ref_mv.f_mv;
                    mv_cand_size += 1;
                }
            }
            if mv_y > 0 {
                let ref_mv = &self.blk_info[mv_pos - self.blk_stride].mv;
                if ref_mv.matches_fwd(mvi.mvref) {
                    mv_cand[mv_cand_size] = ref_mv.f_mv;
                    mv_cand_size += 1;
                }
            }
            if hdr.has_top_block(mv_x << 2, mv_y << 2, mv_w << 2, 0, 4) {
                let ref_mv = &self.blk_info[mv_pos - self.blk_stride + mv_w].mv;
                if ref_mv.matches_fwd(mvi.mvref) {
                    mv_cand[mv_cand_size] = ref_mv.f_mv;
                    mv_cand_size += 1;
                }
            }
            f_mv = match mv_cand_size {
                1 | 2 => {
                        let x = mv_cand[0].x + mv_cand[1].x + mv_cand[2].x;
                        let y = mv_cand[0].y + mv_cand[1].y + mv_cand[2].y;
                        if mv_cand_size == 1 {
                            MV { x, y }
                        } else {
                            MV { x: x >> 1, y: y >> 1 }
                        }
                    },
                3   => MV::pred(mv_cand[0], mv_cand[1], mv_cand[2]),
                _   => ZERO_MV,
            };
        } else {
            f_mv = ZERO_MV;
        }
        if mvi.mvref.is_bwd() {
            let mut mv_cand: [MV; 3] = [ZERO_MV; 3];
            let mut mv_cand_size: usize = 0;
            if mv_x > 0 {
                let ref_mv = &self.blk_info[mv_pos - 1].mv;
                if ref_mv.matches_bwd(mvi.mvref) {
                    mv_cand[mv_cand_size] = ref_mv.b_mv;
                    mv_cand_size += 1;
                }
            }
            if mv_y > 0 {
                let ref_mv = &self.blk_info[mv_pos - self.blk_stride].mv;
                if ref_mv.matches_bwd(mvi.mvref) {
                    mv_cand[mv_cand_size] = ref_mv.b_mv;
                    mv_cand_size += 1;
                }
            }
            if hdr.has_top_block(mv_x << 2, mv_y << 2, mv_w << 2, 0, 4) {
                let ref_mv = &self.blk_info[mv_pos - self.blk_stride + mv_w].mv;
                if ref_mv.matches_bwd(mvi.mvref) {
                    mv_cand[mv_cand_size] = ref_mv.b_mv;
                    mv_cand_size += 1;
                }
            }
            b_mv = match mv_cand_size {
                1 | 2 => {
                        let x = mv_cand[0].x + mv_cand[1].x + mv_cand[2].x;
                        let y = mv_cand[0].y + mv_cand[1].y + mv_cand[2].y;
                        if mv_cand_size == 1 {
                            MV { x, y }
                        } else {
                            MV { x: x >> 1, y: y >> 1 }
                        }
                    },
                3   => MV::pred(mv_cand[0], mv_cand[1], mv_cand[2]),
                _   => ZERO_MV,
            };
        } else {
            b_mv = ZERO_MV;
        }

        MVInfo { f_mv: mvi.f_mv + f_mv, b_mv: mvi.b_mv + b_mv, mvref: mvi.mvref }
    }
    fn fill_skip_cand(&mut self, hdr: &FrameHeader, skip_cand: &mut UniqueList<MVInfo>, size: usize) {
        let mv_size = size >> 2;

        if self.xpos > 0 {
            let mv = &self.blk_info[self.blk_pos - 1].mv;
            if mv.is_some() {
                skip_cand.add(*mv);
            }
        }
        if self.ypos > 0 {
            let mv = &self.blk_info[self.blk_pos - self.blk_stride].mv;
            if mv.is_some() {
                skip_cand.add(*mv);
            }
        }
        if hdr.has_top_right_block(self.xpos, self.ypos, 0, 0, size) {
            let mv = &self.blk_info[self.blk_pos - self.blk_stride + mv_size].mv;
            if mv.is_some() {
                skip_cand.add(*mv);
            }
        }
        if hdr.has_left_down_block(self.xpos, self.ypos, 0, 0, size) {
            let mv = &self.blk_info[self.blk_pos + self.blk_stride * mv_size - 1].mv;
            if mv.is_some() {
                skip_cand.add(*mv);
            }
        }
        if hdr.has_left_block(self.xpos, self.ypos, 0, 0, size) {
            let mv = &self.blk_info[self.blk_pos + self.blk_stride * (mv_size - 1) - 1].mv;
            if mv.is_some() {
                skip_cand.add(*mv);
            }
        }
        if hdr.has_top_block(self.xpos, self.ypos, 0, 0, size) {
            let mv = &self.blk_info[self.blk_pos - self.blk_stride + mv_size - 1].mv;
            if mv.is_some() {
                skip_cand.add(*mv);
            }
        }
        if (self.xpos > 0) && (self.ypos > 0) {
            let mv = &self.blk_info[self.blk_pos - self.blk_stride - 1].mv;
            if mv.is_some() {
                skip_cand.add(*mv);
            }
        }
        for i in skip_cand.fill..4 {
            skip_cand.list[i] = MVInfo { f_mv: ZERO_MV, b_mv: ZERO_MV, mvref: MVRef::Ref0 };
        }
    }
    fn calc_tile_size(&self, pu_pos: usize, cu_type: CUType, log_size: u8) -> u8 {
        match log_size {
            3 => 3,
            4 if (cu_type != CUType::Intra) && (self.pu_info[pu_pos].pu_type != PUType::Full) => 3,
            4 | 5 | 6 => 4,
            _ => unreachable!(),
        }
    }
    fn deblock_cb_tree(&mut self, buf: &mut NASimpleVideoFrame<u8>, hdr: &FrameHeader, xpos: usize, ypos: usize, log_size: u8) {
        if (xpos >= hdr.awidth) || (ypos >= hdr.aheight) { return; }
        let split = self.cu_splits.pop().unwrap();
        if split {
            let hsize = 1 << (log_size - 1);
            self.deblock_cb_tree(buf, hdr, xpos,         ypos,         log_size - 1);
            self.deblock_cb_tree(buf, hdr, xpos + hsize, ypos,         log_size - 1);
            self.deblock_cb_tree(buf, hdr, xpos,         ypos + hsize, log_size - 1);
            self.deblock_cb_tree(buf, hdr, xpos + hsize, ypos + hsize, log_size - 1);
        } else {
            let pu_pos = (xpos >> 3) + (ypos >> 3) * self.pu_stride;
            let cu_type = self.pu_info[pu_pos].cu_type;
            let tsize = self.calc_tile_size(pu_pos, cu_type, log_size);
            let ntiles = 1 << (log_size - tsize);
            let dparams = RV60DeblockParams {
                            deblock_chroma: hdr.deblock_chroma,
                            width:          hdr.awidth,
                            height:         hdr.aheight,
                            dblkstride:     self.dblk.stride,
                        };
            for ty in 0..ntiles {
                for tx in 0..ntiles {
                    let x = xpos + (tx << tsize);
                    let y = ypos + (ty << tsize);
                    let cb_pos = ((x & 63) >> 3) + ((y & 63) >> 3) * 8;
                    if cu_type == CUType::Intra {
                        self.dblk.set_strength(x, y, 1 << tsize, self.qp, 2);
                    } else if (cu_type != CUType::Skip) && self.coded_blk[cb_pos] {
                        self.dblk.set_strength(x, y, 1 << tsize, self.qp, 1);
                    } else {
                        self.dblk.set_strength(x, y, 1 << tsize, self.qp, 0);
                        self.derive_deblock_strength(x, y, 1 << (tsize - 2));
                    }
                    self.dsp.do_deblock(&dparams, buf, x, y, 1 << tsize,
                                        self.dblk.top_str.as_slice(),
                                        self.dblk.left_str.as_slice(),
                                        self.dblk.get_pos(x, y));
                }
            }
        }
    }
    fn derive_deblock_strength(&mut self, xpos: usize, ypos: usize, size4: usize) {
        let blk_pos = (xpos >> 2) + (ypos >> 2) * self.blk_stride;
        let mut dblk_pos = self.dblk.get_pos(xpos, ypos);
        if ypos > 0 {
            let top_blk_pos = blk_pos - self.blk_stride;
            for i in 0..size4 {
                if self.dblk.get_top_strength(dblk_pos - self.dblk.stride + i) == 0 {
                    if self.blk_info[blk_pos + i].mv.is_deblock_cand(&self.blk_info[top_blk_pos + i].mv) {
                        self.dblk.set_top_strength(dblk_pos + i, 1);
                    }
                }
            }
        }
        if xpos > 0 {
            for i in 0..size4 {
                if self.dblk.get_left_strength(dblk_pos - 1) == 0 {
                    if self.blk_info[blk_pos + i * self.blk_stride].mv.is_deblock_cand(&self.blk_info[blk_pos + i * self.blk_stride - 1].mv) {
                        self.dblk.set_left_strength(dblk_pos, 1);
                    }
                }
                dblk_pos += self.dblk.stride;
            }
        }
    }
}

impl NADecoder for RealVideo60Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = YUV420_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            let edata = info.get_extradata().unwrap();
            let src: &[u8] = &edata;

            if src.len() < 8 { return Err(DecoderError::InvalidData); }
            let mut mr = MemoryReader::new_read(src);
            let mut br = ByteReader::new(&mut mr);
            let _flags                                  = br.read_u32be()?;
            let version                                 = br.read_u32be()?;
            let _unk                                    = br.read_u16be()?;
            validate!((version >> 28) == 4);
            // then width and height again as 16be

            //self.bd.width  = vinfo.get_width();
            //self.bd.height = vinfo.get_height();
            //self.frmmgr.clear();

            supp.pool_u8.set_dec_bufs(3);
            supp.pool_u8.prealloc_video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, fmt), 6)?;

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        validate!(src.len() > 9);
        let hsize = (src[0] as usize) * 8 + 9;
        let mut br = BitReader::new(&src[hsize..], BitReaderMode::BE);
        let hdr = FrameHeader::read(&mut br)?;
        match self.skip_mode {
            FrameSkipMode::None => {},
            FrameSkipMode::KeyframesOnly => {
                if hdr.ftype == FrameType::B {
                    let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::None);
                    frm.set_frame_type(FrameType::Skip);
                    return Ok(frm.into_ref());
                }
            },
            FrameSkipMode::IntraOnly => {
                if hdr.ftype != FrameType::I {
                    let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::None);
                    frm.set_frame_type(FrameType::Skip);
                    return Ok(frm.into_ref());
                }
            },
        };

        let mut slices: Vec<usize> = Vec::new();
        hdr.parse_slice_sizes(&mut br, &mut slices)?;
        match hdr.ftype {
            FrameType::P => {
                if self.ipbs.get_lastref().is_none() {
                    return Err(DecoderError::MissingReference);
                }
            },
            FrameType::B => {
                if self.ipbs.get_lastref().is_none() {
                    return Err(DecoderError::MissingReference);
                }
                if self.ipbs.get_nextref().is_none() {
                    return Err(DecoderError::MissingReference);
                }
            },
            _ => {},
        };

        let tmp_vinfo = NAVideoInfo::new(hdr.width, hdr.height, false, YUV420_FORMAT);
        let ret = supp.pool_u8.get_free();
        if ret.is_none() {
            return Err(DecoderError::AllocError);
        }
        let mut buf = ret.unwrap();
        if buf.get_info() != tmp_vinfo {
            self.ipbs.clear();
            supp.pool_u8.reset();
            supp.pool_u8.prealloc_video(tmp_vinfo, 6)?;
            let ret = supp.pool_u8.get_free();
            if ret.is_none() {
                return Err(DecoderError::AllocError);
            }
            buf = ret.unwrap();
        }

        let cu_w = hdr.get_width_cu();
        let cu_h = hdr.get_height_cu();
        self.pu_stride = cu_w << 3;
        self.pu_info.resize(self.pu_stride * (cu_h << 3), PUInfo::default());
        self.blk_stride = cu_w << 4;
        self.blk_info.clear();
        self.blk_info.resize(self.blk_stride * (cu_h << 4), BlockInfo::default());
        if hdr.deblock {
            self.dblk.reinit(hdr.awidth, hdr.aheight);
        }
        let mut off = hsize + (br.tell() >> 3);
        let mut dframe = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
        for (cu_y, size) in slices.into_iter().enumerate() {
            self.decode_cu_line(&mut dframe, &hdr, &src[off..][..size], cu_y)?;
            off += size;
        }
        if (hdr.ftype == FrameType::I) || (hdr.ftype == FrameType::P) {
            self.ipbs.add_frame(buf.clone());
        }

        if hdr.ftype != FrameType::B {
            self.ref0_pts = self.ref1_pts;
            self.ref1_pts = pkt.get_pts().unwrap_or(0);
            self.ref0_ts = self.ref1_ts;
            self.ref1_ts = hdr.ts as u64;
            if (self.ref1_pts > self.ref0_pts) && (self.ref1_ts > self.ref0_ts) {
                self.ts_scale = (self.ref1_pts - self.ref0_pts) / (self.ref1_ts - self.ref0_ts);
            }
        }
        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(hdr.ftype == FrameType::I);
        if hdr.ftype == FrameType::B {
            let pts = self.ref0_pts + ((hdr.ts as u64) - self.ref0_ts) * self.ts_scale;
            frm.set_pts(Some(pts));
        }
        frm.set_frame_type(hdr.ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.ipbs.clear();
    }
}

const DECODER_OPTIONS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: FRAME_SKIP_OPTION, description: FRAME_SKIP_OPTION_DESC,
        opt_type: NAOptionDefinitionType::String(Some(&[
                FRAME_SKIP_OPTION_VAL_NONE,
                FRAME_SKIP_OPTION_VAL_KEYFRAME,
                FRAME_SKIP_OPTION_VAL_INTRA
            ])) },
];

impl NAOptionHandler for RealVideo60Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { DECODER_OPTIONS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in DECODER_OPTIONS.iter() {
                if opt_def.check(option).is_ok() {
                    match (option.name, &option.value) {
                        (FRAME_SKIP_OPTION, NAValue::String(ref strval)) => {
                            if let Ok(smode) = FrameSkipMode::from_str(strval) {
                                self.skip_mode = smode;
                            }
                        },
                        _ => {},
                    }
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            FRAME_SKIP_OPTION => Some(NAValue::String(self.skip_mode.to_string())),
            _ => None,
        }
    }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RealVideo60Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::realmedia_register_all_decoders;
    use crate::realmedia_register_all_demuxers;
    #[test]
    fn test_rv60() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        // sample from a private collection
        test_decoding("realmedia", "realvideo6", "assets/RV/RV60.rmhd", Some(1000), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x2b1f1807, 0x09edef33, 0x0e6c78c1, 0x3b3c8179],
                            [0x76743a3b, 0x7dd4f196, 0x0193fe5a, 0x4f78c7cb],
                            [0x2b1f1807, 0x09edef33, 0x0e6c78c1, 0x3b3c8179],
                            [0xfee70206, 0x626f3bea, 0x7677ad4b, 0x1228f3b6],
                            [0x7156cbc2, 0xf381bcb6, 0xe86531f2, 0xb311c3ea],
                            [0x1742b5a1, 0x66252580, 0x242753de, 0x5215d732],
                            [0xd357ebda, 0x6460dba6, 0xa93eb616, 0x63ee6d60],
                            [0x4cd72275, 0x28e1e439, 0xad17dfca, 0x3fd7253f],
                            [0xe389ce4f, 0x8f0891b3, 0x88639b23, 0x21ed114f],
                            [0x5b2b2f1b, 0x17a7518b, 0x53806e6a, 0x4538bb00],
                            [0xdca03c9a, 0x1a45d80c, 0x86141211, 0x79912ed4],
                            [0x0bf66bf4, 0x46385620, 0xc6fa4796, 0xd8e16d56],
                            [0x4671a7f0, 0x46f50649, 0x268df27b, 0x70b71ab3]]));
    }
    #[test]
    fn test_rv60_dqp() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        // sample provided by Peter Ross
        test_decoding("realmedia", "realvideo6", "assets/RV/qp-offset-type-2.rmhd", Some(500), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x3dc2f19e, 0x0f8c66bd, 0x8e81ceda, 0xa1bf8f58],
                            [0xbd9c0f89, 0x67b780b0, 0xa4afe443, 0x9f17221a],
                            [0xf3e0a7ba, 0xe620ace9, 0x03857219, 0x8c3bd1fb],
                            [0xc4eedc8c, 0x81d2dd0f, 0xa6443847, 0x09c8cec9],
                            [0x565fc952, 0x4d5dc166, 0xf64b7b0d, 0x1570de50],
                            [0x0e50786a, 0xaf058ff3, 0xa3f71eba, 0x370c197a],
                            [0x1b92667b, 0x9cab9e24, 0x1bf48cb2, 0x368db124],
                            [0xefcc0ab4, 0x6efceb20, 0xb2501ee8, 0xb449b7b6],
                            [0xbbc2ca23, 0x6a7a8da2, 0xeadc1ff7, 0x2ff0a7f3],
                            [0x6d14a2b4, 0x0d2642fb, 0x78fcad10, 0xba571ec1],
                            [0xbdf889fd, 0x5f15838a, 0x8fedd13f, 0xc26a2e50],
                            [0x886f03b6, 0xc46ba7c3, 0xae6aa971, 0x90cf94b6],
                            [0x951693e7, 0xa77f68f3, 0x765990c9, 0x4a4d57fa],
                            [0x3c25f4eb, 0x5c113c41, 0x4d73f498, 0xd7e210b0]]));
    }
}
