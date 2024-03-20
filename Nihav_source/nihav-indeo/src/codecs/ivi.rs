use super::ivibr::{IVICodebook,IVI_CB_ZERO,RVMap,IVI_ZERO_RVMAP,IVI_RVMAPS};

pub fn clip8(a: i16) -> u8 {
    if a < 0 { 0 }
    else if a > 255 { 255 }
    else { a as u8 }
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum IVIFrameType {
    Intra,
    Inter,
    Bidir,
    Intra1,
    InterDroppable,
    InterScal,
    NULL,
    NULL2,
}

impl IVIFrameType {
    pub fn is_intra(self) -> bool {
        (self == IVIFrameType::Intra) || (self == IVIFrameType::Intra1)
    }
    pub fn is_null(self) -> bool {
        (self == IVIFrameType::NULL) || (self == IVIFrameType::NULL2)
    }
    pub fn is_bidir(self) -> bool {
        self == IVIFrameType::Bidir
    }
}

#[derive(Clone,Copy)]
pub struct PictureHeader {
    pub ftype:          IVIFrameType,
    pub width:          usize,
    pub height:         usize,
    pub slice_w:        usize,
    pub slice_h:        usize,
    pub transparent:    bool,
    pub luma_bands:     usize,
    pub chroma_bands:   usize,
    pub in_q:           bool,
}

impl PictureHeader {
    pub fn new(ftype: IVIFrameType, width: usize, height: usize, slice_w: usize, slice_h: usize, transparent: bool, luma_bands: usize, chroma_bands: usize, in_q: bool) -> Self {
        PictureHeader {
            ftype,
            width, height, slice_w, slice_h,
            transparent,
            luma_bands, chroma_bands,
            in_q,
        }
    }
    pub fn new_null(ftype: IVIFrameType) -> Self {
        PictureHeader {
            ftype,
            width: 0, height: 0, slice_w: 0, slice_h: 0,
            transparent: false,
            luma_bands: 0, chroma_bands: 0,
            in_q: false,
        }
    }
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum TSize {
    T8x8,
    T4x4,
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum TDir {
    TwoD,
    Row,
    Col,
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum IVITransformType {
    Haar (TSize, TDir),
    Slant(TSize, TDir),
    DCT  (TSize, TDir),
    None (TSize),
}

pub type TrFunc   = fn (&mut [i32; 64]);
pub type TrFuncDC = fn (&mut [i32; 64], i32);

impl IVITransformType {
    pub fn is_8x8(self) -> bool {
        match self {
            IVITransformType::Haar (ref sz, _) => { *sz == TSize::T8x8 },
            IVITransformType::Slant(ref sz, _) => { *sz == TSize::T8x8 },
            IVITransformType::DCT  (ref sz, _) => { *sz == TSize::T8x8 },
            IVITransformType::None (ref sz)    => { *sz == TSize::T8x8 },
        }
    }
    pub fn is_2d(self) -> bool {
        match self {
            IVITransformType::Haar (_, ref dir) => { *dir == TDir::TwoD },
            IVITransformType::Slant(_, ref dir) => { *dir == TDir::TwoD },
            IVITransformType::DCT  (_, ref dir) => { *dir == TDir::TwoD },
            _                                   => { false },
        }
    }
}

#[allow(dead_code)]
#[derive(Clone)]
pub struct TxParams4x4 {
    pub quant_intra:    &'static [u16; 16],
    pub quant_inter:    &'static [u16; 16],
    pub scan:           &'static [usize; 16],
}

impl TxParams4x4 {
    pub fn new(quant_intra: &'static [u16; 16], quant_inter: &'static [u16; 16], scan: &'static [usize; 16]) -> Self {
        TxParams4x4 {
            quant_intra, quant_inter, scan,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone)]
pub struct TxParams8x8 {
    pub quant_intra:    &'static [u16; 64],
    pub quant_inter:    &'static [u16; 64],
    pub scan:           &'static [usize; 64],
}

impl TxParams8x8 {
    pub fn new(quant_intra: &'static [u16; 64], quant_inter: &'static [u16; 64], scan: &'static [usize; 64]) -> Self {
        TxParams8x8 {
            quant_intra, quant_inter, scan,
        }
    }
}

#[derive(Clone)]
pub enum TxType {
    Transform4(TxParams4x4),
    Transform8(TxParams8x8),
    None,
}

pub const CORR_MAP_SIZE: usize = 122;

#[derive(Clone)]
pub struct BandHeader {
    pub plane_no:   usize,
    pub band_no:    usize,
    pub empty:      bool,
    pub mb_size:    usize,
    pub blk_size:   usize,
    pub halfpel:    bool,
    pub inherit_mv: bool,
    pub has_qdelta: bool,
    pub inherit_qd: bool,
    pub quant:      u32,
    pub blk_cb:     IVICodebook,
    pub rvmap:      RVMap,
    pub tr:         IVITransformType,
    pub ttype:      TxType,
}

impl BandHeader {
    pub fn new(plane_no: usize, band_no: usize, mb_size: usize, blk_size: usize, halfpel: bool, inherit_mv: bool, has_qdelta: bool, inherit_qd: bool, quant: u32, rvmap_idx: usize, num_corr: usize, corr_map: [u8; CORR_MAP_SIZE], blk_cb: IVICodebook, tr: IVITransformType, ttype: TxType) -> Self {
        let mut rvmap = IVI_RVMAPS[rvmap_idx].clone();
        for i in 0..num_corr {
            let pos1 = corr_map[i * 2 + 0] as usize;
            let pos2 = corr_map[i * 2 + 1] as usize;
            rvmap.runtab.swap(pos1, pos2);
            rvmap.valtab.swap(pos1, pos2);
        }
        BandHeader {
            plane_no, band_no,
            empty: false, halfpel,
            inherit_mv,
            has_qdelta, inherit_qd, quant,
            mb_size, blk_size,
            rvmap, blk_cb,
            tr, ttype,
        }
    }
    pub fn new_empty(plane_no: usize, band_no: usize) -> Self {
        BandHeader {
            plane_no, band_no,
            empty: true, halfpel: true,
            inherit_mv: false, has_qdelta: false, inherit_qd: false, quant: 0,
            mb_size: 0, blk_size: 0,
            rvmap: IVI_ZERO_RVMAP, blk_cb: IVI_CB_ZERO,
            tr: IVITransformType::None(TSize::T8x8), ttype: TxType::None,
        }
    }
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum MBType {
    Intra,
    Inter,
    Backward,
    Bidir,
}

#[derive(Clone,Copy)]
pub struct MB {
    pub mtype:  MBType,
    pub pos_x:  usize,
    pub pos_y:  usize,
    pub mv_x:   i32,
    pub mv_y:   i32,
    pub mv2_x:  i32,
    pub mv2_y:  i32,
    pub qd:     i16,
    pub q:      u8,
    pub cbp:    u8,
}

impl MB {
    pub fn new(x: usize, y: usize) -> Self {
        MB {
            mtype: MBType::Intra,
            pos_x: x, pos_y: y,
            mv_x: 0, mv_y: 0,
            mv2_x: 0, mv2_y: 0,
            cbp: 0, q: 0, qd: 0,
        }
    }
}

pub struct IVITile {
    pub pos_x:  usize,
    pub pos_y:  usize,
    pub mb_w:   usize,
    pub mb_h:   usize,
    pub w:      usize,
    pub h:      usize,
    pub mb:     Vec<MB>,
}

impl IVITile {
    pub fn new(pos_x: usize, pos_y: usize, w: usize, h: usize) -> Self {
        IVITile {
            pos_x, pos_y, w, h,
            mb_w: 0, mb_h: 0, mb: Vec::new(),
        }
    }
}

pub const IVI_SCAN_8X8_VER: [usize; 64] = [
    0,  8, 16, 24, 32, 40, 48, 56,
    1,  9, 17, 25, 33, 41, 49, 57,
    2, 10, 18, 26, 34, 42, 50, 58,
    3, 11, 19, 27, 35, 43, 51, 59,
    4, 12, 20, 28, 36, 44, 52, 60,
    5, 13, 21, 29, 37, 45, 53, 61,
    6, 14, 22, 30, 38, 46, 54, 62,
    7, 15, 23, 31, 39, 47, 55, 63
];
pub const IVI_SCAN_8X8_HOR: [usize; 64] = [
     0,  1,  2,  3,  4,  5,  6,  7,
     8,  9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23,
    24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55,
    56, 57, 58, 59, 60, 61, 62, 63
];
pub const IVI_SCAN_4X4: [usize; 16] = [ 0, 1, 4, 8, 5, 2, 3, 6, 9, 12, 13, 10, 7, 11, 14, 15 ];
