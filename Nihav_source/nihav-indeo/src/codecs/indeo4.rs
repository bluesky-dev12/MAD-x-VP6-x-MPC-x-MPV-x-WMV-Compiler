use nihav_core::io::bitreader::*;
use nihav_core::formats;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::ZIGZAG;
use super::ivi::*;
use super::ivibr::*;

#[inline(always)]
fn mclip8(a: i32) -> u8 {
    if (a as u16) > 255 { !(a >> 16) as u8 }
    else { a as u8 }
}

struct Indeo4Parser {
    mb_cb:          IVICodebook,
    blk_cb:         IVICodebook,
}

fn calc_quant(glob_q: u32, qd: i16) -> u8 {
    let q = (glob_q as i16) + qd;
    if q < 0 {
        0
    } else if q > 31 {
        31
    } else {
        q as u8
    }
}

impl Indeo4Parser {
    fn new() -> Self {
        Indeo4Parser {
            mb_cb:      IVI_CB_ZERO,
            blk_cb:     IVI_CB_ZERO,
        }
    }
}

impl IndeoXParser for Indeo4Parser {
#[allow(unused_variables,unused_assignments)]
    fn decode_picture_header(&mut self, br: &mut BitReader) -> DecoderResult<PictureHeader> {
        let sync                = br.read(18)?;
        validate!(sync == 0x3FFF8);
        let ftype_idx           = br.read(3)?;
        validate!(ftype_idx < 7);
        let ftype               = INDEO4_FRAME_TYPE[ftype_idx as usize];
        let transparent         = br.read_bool()?;
        br.skip(1)?;
        let data_size;
        if br.read_bool()? {
            data_size = br.read(24)? as usize;
        } else {
            data_size = 0;
        }
        if ftype.is_null() {
            return Ok(PictureHeader::new_null(ftype));
        }
        if br.read_bool()? {
            br.skip(32)?; // key lock
        }
        let width;
        let height;
        let pic_size_idx        = br.read(3)?;
        if pic_size_idx < 7 {
            width  = INDEO4_PICTURE_SIZE_TAB[pic_size_idx as usize][0];
            height = INDEO4_PICTURE_SIZE_TAB[pic_size_idx as usize][1];
        } else {
            height              = br.read(16)? as usize;
            width               = br.read(16)? as usize;
            validate!((width  > 0) && ((width  & 3) == 0));
            validate!((height > 0) && ((height & 3) == 0));
        }

        let slice_w;
        let slice_h;
        if br.read_bool()? {
            let idx = br.read(4)? as usize;
            slice_h = if idx < 15 { INDEO4_SLICE_SIZE_TAB[idx] } else { height };
            let idx = br.read(4)? as usize;
            slice_w = if idx < 15 { INDEO4_SLICE_SIZE_TAB[idx] } else { width };
        } else {
            slice_w = width;
            slice_h = height;
        }
        let subsampling         = br.read(2)?;
        validate!(subsampling == 0);
        let sc_idx              = br.read(2)?;
        match sc_idx {
            3 => { },
            2 => { validate!(br.read(2*4)? == 0xFF); }
            _ => { return Err(DecoderError::InvalidData); }
        };
        let luma_bands = if sc_idx == 2 { 4 } else { 1 };
        let sc_idx              = br.read(2)?;
        match sc_idx {
            3 => { },
            2 => { validate!(br.read(2*4)? == 0xFF); }
            _ => { return Err(DecoderError::InvalidData); }
        };
        let chroma_bands = if sc_idx == 2 { 4 } else { 1 };
        let frame_no;
        if br.read_bool()? {
            frame_no = br.read(20)?;
        } else {
            frame_no = 0;
        }
        if br.read_bool()? {
            br.skip(8)?; // decTimeEst
        }
        let desc_coded          = br.read_bool()?;
        self.mb_cb              = br.read_ivi_codebook_desc(true,  desc_coded)?;
        let desc_coded          = br.read_bool()?;
        self.blk_cb             = br.read_ivi_codebook_desc(false, desc_coded)?;
        let rvmap               = if br.read_bool()? { br.read(3)? as usize } else { 8 };
        let in_imf              = br.read_bool()?;
        let in_q                = br.read_bool()?;
        let glob_q              = br.read(5)? as u8;
        if br.read_bool()? {
            br.skip(3)?;
        }
        let checksum            = if br.read_bool()? { br.read(16)? } else { 0 };
        if br.read_bool()? {
            br.skip(8)?; // pic hdr extension
        }
        if br.read_bool()? {
            println!("bad blocks bits!");
        }
        br.align();

        Ok(PictureHeader::new(ftype, width, height, slice_w, slice_h, transparent, luma_bands, chroma_bands, in_q))
    }

#[allow(unused_variables,unused_assignments)]
    fn decode_band_header(&mut self, br: &mut BitReader, pic_hdr: &PictureHeader, plane: usize, band: usize) -> DecoderResult<BandHeader> {
        let plane_no        = br.read(2)? as usize;
        let band_no         = br.read(4)? as usize;
        validate!(plane_no == plane);
        validate!(band_no  == band);
        if br.read_bool()? {
            br.align();
            return Ok(BandHeader::new_empty(plane_no, band_no));
        }
        let hdr_size;
        if br.read_bool()? {
            hdr_size        = br.read(16)? as usize;
        } else {
            hdr_size = 32;
        }
        let mv_mode         = br.read(2)?;
        validate!(mv_mode < 2);
        if br.read_bool()? {
            br.skip(16)?; //checksum
        }

        let scale           = br.read(2)?;
        validate!(scale != 3);
        let mb_size  = 16 >> scale;
        let blk_size =  8 >> (scale >> 1);
        let inherit_mv      = br.read_bool()?;
        let inherit_qd      = br.read_bool()?;
        let quant           = br.read(5)?;

        let tr: IVITransformType;
        let txtype: TxType;
        if !br.read_bool()? || pic_hdr.ftype == IVIFrameType::Intra {
            let tr_id       = br.read(5)?;
            validate!(tr_id < 18);
            let scan_idx    = br.read(4)? as usize;
            validate!(scan_idx != 15);
            let qmat_idx    = br.read(5)? as usize;

            tr = INDEO4_TRANSFORMS[tr_id as usize];
            if (scan_idx < 5) || (scan_idx >= 10) {
                validate!(tr.is_8x8());
                validate!(qmat_idx < 15);
                let scan = if scan_idx < 5 { INDEO4_SCANS_8X8[scan_idx] }
                           else            { INDEO4_SCANS_8X8[4] };
                let qidx = INDEO4_Q8X8_IDX[qmat_idx];
                let qintra = INDEO4_Q8_INTRA[qidx];
                let qinter = INDEO4_Q8_INTER[qidx];
                txtype = TxType::Transform8(TxParams8x8::new(qintra, qinter, scan));
            } else if scan_idx < 10 {
                validate!(!tr.is_8x8());
                validate!((15..22).contains(&qmat_idx));
                let scan = INDEO4_SCANS_4X4[scan_idx - 5];
                let qidx = INDEO4_Q4X4_IDX[qmat_idx - 15];
                let qintra = INDEO4_Q4_INTRA[qidx];
                let qinter = INDEO4_Q4_INTER[qidx];
                txtype = TxType::Transform4(TxParams4x4::new(qintra, qinter, scan));
            } else {
                unreachable!();
            }
        } else {
            tr = IVITransformType::None(TSize::T8x8);
            txtype = TxType::None;
        }

        let blk_cb;
        if br.read_bool()? {
            blk_cb = br.read_ivi_codebook_desc(false, true)?;
        } else {
            blk_cb = self.blk_cb;
        }
        let rvmap_idx;
        if br.read_bool()? {
            rvmap_idx = br.read(3)? as usize;
        } else {
            rvmap_idx = 8;
        }
        let num_corr;
        let mut corr_map: [u8; CORR_MAP_SIZE] = [0; CORR_MAP_SIZE];
        if br.read_bool()? {
            num_corr = br.read(8)? as usize;
            validate!(num_corr*2 <= CORR_MAP_SIZE);
            for i in 0..num_corr*2 {
                corr_map[i] = br.read(8)? as u8;
            }
        } else {
            num_corr = 0;
        }

        br.align();
        Ok(BandHeader::new(plane_no, band_no, mb_size, blk_size, mv_mode == 1, inherit_mv, false, inherit_qd, quant, rvmap_idx, num_corr, corr_map, blk_cb, tr, txtype))
    }

    fn decode_mb_info(&mut self, br: &mut BitReader, pic_hdr: &PictureHeader, band: &BandHeader, tile: &mut IVITile, ref_tile: Option<&IVITile>, mv_scale: u8) -> DecoderResult<()> {
        let mut mv_x = 0;
        let mut mv_y = 0;
        let mut mb_idx = 0;

        for mb_y in 0..tile.mb_h {
            for mb_x in 0..tile.mb_w {
                let mut mb = MB::new(tile.pos_x + mb_x * band.mb_size, tile.pos_y + mb_y * band.mb_size);
                if !br.read_bool()? {
                    if pic_hdr.ftype.is_intra() {
                        mb.mtype = MBType::Intra;
                    } else if band.inherit_mv {
                        if let Some(tileref) = ref_tile {
                            mb.mtype = tileref.mb[mb_idx].mtype;
                        } else {
                            return Err(DecoderError::MissingReference);
                        }
                    } else {
                        if !pic_hdr.ftype.is_bidir() {
                            mb.mtype = if br.read_bool()? { MBType::Inter } else { MBType::Intra };
                        } else {
                            mb.mtype = match br.read(2)? {
                                0 => { MBType::Intra },
                                1 => { MBType::Inter },
                                2 => { MBType::Backward },
                                _ => { MBType::Bidir },
                            };
                        }
                    }
                    if band.mb_size == band.blk_size {
                        mb.cbp = br.read(1)? as u8;
                    } else {
                        mb.cbp = br.read(4)? as u8;
                    }
                    if band.inherit_qd {
                        if let Some(tileref) = ref_tile {
                            mb.qd = tileref.mb[mb_idx].qd;
                            mb.q  = calc_quant(band.quant, mb.qd);
                        } else {
                            mb.q = band.quant as u8;
                        }
                    } else if (mb.cbp != 0) || ((band.plane_no == 0) && (band.band_no == 0) && pic_hdr.in_q) {
                        mb.qd = br.read_ivi_cb_s(&self.mb_cb)? as i16;
                        mb.q = calc_quant(band.quant, mb.qd);
                    } else {
                        mb.q = band.quant as u8;
                    }

                    if mb.mtype != MBType::Intra {
                        if band.inherit_mv {
                            if let Some(tileref) = ref_tile {
                                let mx = tileref.mb[mb_idx].mv_x;
                                let my = tileref.mb[mb_idx].mv_y;
                                if mv_scale == 0 {
                                    mb.mv_x = mx;
                                    mb.mv_y = my;
                                } else {
                                    mb.mv_x = scale_mv(mx, mv_scale);
                                    mb.mv_y = scale_mv(my, mv_scale);
                                }
                            }
                        } else {
                            mv_y += br.read_ivi_cb_s(&self.mb_cb)?;
                            mv_x += br.read_ivi_cb_s(&self.mb_cb)?;
                            mb.mv_x = mv_x;
                            mb.mv_y = mv_y;
                            if mb.mtype == MBType::Backward {
                                mb.mv_x = -mb.mv_x;
                                mb.mv_y = -mb.mv_y;
                            } else if mb.mtype == MBType::Bidir {
                                mv_y += br.read_ivi_cb_s(&self.mb_cb)?;
                                mv_x += br.read_ivi_cb_s(&self.mb_cb)?;
                                mb.mv2_x = -mv_x;
                                mb.mv2_y = -mv_y;
                            }
                        }
                    }
                } else {
                    validate!(!pic_hdr.ftype.is_intra());
                    mb.mtype = MBType::Inter;
                    mb.cbp   = 0;
                    mb.qd    = 0;
                    if (band.plane_no == 0) && (band.band_no == 0) && pic_hdr.in_q {
                        mb.qd = br.read_ivi_cb_s(&self.mb_cb)? as i16;
                        mb.q  = calc_quant(band.quant, mb.qd);
                    }
                    if band.inherit_mv {
                        if let Some(tileref) = ref_tile {
                            let mx = tileref.mb[mb_idx].mv_x;
                            let my = tileref.mb[mb_idx].mv_y;
                            if mv_scale == 0 {
                                mb.mv_x = mx;
                                mb.mv_y = my;
                            } else {
                                mb.mv_x = scale_mv(mx, mv_scale);
                                mb.mv_y = scale_mv(my, mv_scale);
                            }
                        }
                    }
                }
                tile.mb[mb_idx] = mb;
                mb_idx += 1;
            }
        }
        br.align();
        Ok(())
    }

    fn recombine_plane(&mut self, src: &[i16], sstride: usize, dst: &mut [u8], dstride: usize, w: usize, h: usize) {
/*        let mut idx0 = 0;
        let mut idx1 = w / 2;
        let mut idx2 = (h / 2) * sstride;
        let mut idx3 = idx2 + idx1;
        let mut oidx0 = 0;
        let mut oidx1 = dstride;

        for _ in 0..(h/2) {
            for x in 0..(w/2) {
                let p0 = src[idx0 + x];
                let p1 = src[idx1 + x];
                let p2 = src[idx2 + x];
                let p3 = src[idx3 + x];
                let s0 = p0 + p2;
                let d0 = p0 - p2;
                let s1 = p1 + p3;
                let d1 = p1 - p3;
                dst[oidx0 + x * 2 + 0] = clip8(((s0 + s1 + 2) >> 2) + 128);
                dst[oidx0 + x * 2 + 1] = clip8(((d0 + d1 + 2) >> 2) + 128);
                dst[oidx1 + x * 2 + 0] = clip8(((s0 - s1 + 2) >> 2) + 128);
                dst[oidx1 + x * 2 + 1] = clip8(((d0 - d1 + 2) >> 2) + 128);
            }
            idx0 += sstride;
            idx1 += sstride;
            idx2 += sstride;
            idx3 += sstride;
            oidx0 += dstride * 2;
            oidx1 += dstride * 2;
        }*/
        unsafe {
            let hw = (w / 2) as isize;
            let hh = (h / 2) as isize;
            let mut band0 = src.as_ptr();
            let mut band1 = band0.offset(hw);
            let mut band2 = band0.add((h / 2) * sstride);
            let mut band3 = band2.offset(hw);
            let mut dst0 = dst.as_mut_ptr();
            let mut dst1 = dst0.add(dstride);
            for _ in 0..hh {
                let mut b0_ptr = band0;
                let mut b1_ptr = band1;
                let mut b2_ptr = band2;
                let mut b3_ptr = band3;
                let mut d0_ptr = dst0;
                let mut d1_ptr = dst1;
                for _ in 0..hw {
                    let p0 = i32::from(*b0_ptr);
                    let p1 = i32::from(*b1_ptr);
                    let p2 = i32::from(*b2_ptr);
                    let p3 = i32::from(*b3_ptr);
                    let s0 = p0.wrapping_add(p2);
                    let s1 = p1.wrapping_add(p3);
                    let d0 = p0.wrapping_sub(p2);
                    let d1 = p1.wrapping_sub(p3);
                    let o0 = s0.wrapping_add(s1).wrapping_add(2);
                    let o1 = d0.wrapping_add(d1).wrapping_add(2);
                    let o2 = s0.wrapping_sub(s1).wrapping_add(2);
                    let o3 = d0.wrapping_sub(d1).wrapping_add(2);
                    *d0_ptr.offset(0) = mclip8((o0 >> 2).wrapping_add(128));
                    *d0_ptr.offset(1) = mclip8((o1 >> 2).wrapping_add(128));
                    *d1_ptr.offset(0) = mclip8((o2 >> 2).wrapping_add(128));
                    *d1_ptr.offset(1) = mclip8((o3 >> 2).wrapping_add(128));
                    b0_ptr = b0_ptr.offset(1);
                    b1_ptr = b1_ptr.offset(1);
                    b2_ptr = b2_ptr.offset(1);
                    b3_ptr = b3_ptr.offset(1);
                    d0_ptr = d0_ptr.offset(2);
                    d1_ptr = d1_ptr.offset(2);
                }
                band0 = band0.add(sstride);
                band1 = band1.add(sstride);
                band2 = band2.add(sstride);
                band3 = band3.add(sstride);
                dst0 = dst0.add(dstride * 2);
                dst1 = dst1.add(dstride * 2);
            }
        }
    }
}

struct Indeo4Decoder {
    info:   NACodecInfoRef,
    dec:    IVIDecoder,
}

impl Indeo4Decoder {
    fn new() -> Self {
        Indeo4Decoder {
            info:   NACodecInfo::new_dummy(),
            dec:    IVIDecoder::new(false),
        }
    }
}

impl NADecoder for Indeo4Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            let f = vinfo.is_flipped();
            let fmt = formats::YUV410_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, f, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        let mut br = BitReader::new(src.as_slice(), BitReaderMode::LE);

        let mut ip = Indeo4Parser::new();
        let bufinfo = self.dec.decode_frame(&mut ip, &mut br)?;
        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(self.dec.is_intra());
        frm.set_frame_type(self.dec.get_frame_type());
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for Indeo4Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

const INDEO4_PICTURE_SIZE_TAB: [[usize; 2]; 7] = [
    [640, 480], [320, 240], [160, 120], [704, 480], [352, 240], [352, 288], [176, 144]
];

const INDEO4_SLICE_SIZE_TAB: [usize; 15] = [
    32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 480
];

const INDEO4_FRAME_TYPE: [IVIFrameType; 7] = [
    IVIFrameType::Intra, IVIFrameType::Intra1, IVIFrameType::Inter, IVIFrameType::Bidir,
    IVIFrameType::InterDroppable, IVIFrameType::NULL, IVIFrameType::NULL2
];

const INDEO4_TRANSFORMS: [IVITransformType; 18] = [
    IVITransformType::Haar(TSize::T8x8, TDir::TwoD),
    IVITransformType::Haar(TSize::T8x8, TDir::Row),
    IVITransformType::Haar(TSize::T8x8, TDir::Col),
    IVITransformType::None(TSize::T8x8),
    IVITransformType::Slant(TSize::T8x8, TDir::TwoD),
    IVITransformType::Slant(TSize::T8x8, TDir::Row),
    IVITransformType::Slant(TSize::T8x8, TDir::Col),
    IVITransformType::DCT(TSize::T8x8, TDir::TwoD),
    IVITransformType::DCT(TSize::T8x8, TDir::Row),
    IVITransformType::DCT(TSize::T8x8, TDir::Col),
    IVITransformType::Haar(TSize::T4x4, TDir::TwoD),
    IVITransformType::Slant(TSize::T4x4, TDir::TwoD),
    IVITransformType::None(TSize::T4x4),
    IVITransformType::Haar(TSize::T4x4, TDir::Row),
    IVITransformType::Haar(TSize::T4x4, TDir::Col),
    IVITransformType::Slant(TSize::T4x4, TDir::Row),
    IVITransformType::Slant(TSize::T4x4, TDir::Col),
    IVITransformType::DCT(TSize::T4x4, TDir::TwoD),
];

const INDEO4_SCAN_8X8_ALT: [usize; 64] = [
     0,  8,  1,  9, 16, 24,  2,  3,
    17, 25, 10, 11, 32, 40, 48, 56,
     4,  5,  6,  7, 33, 41, 49, 57,
    18, 19, 26, 27, 12, 13, 14, 15,
    34, 35, 43, 42, 50, 51, 59, 58,
    20, 21, 22, 23, 31, 30, 29, 28,
    36, 37, 38, 39, 47, 46, 45, 44,
    52, 53, 54, 55, 63, 62, 61, 60
];
const INDEO4_SCAN_4X4_ALT: [usize; 16] = [ 0, 1, 4, 5, 8, 12, 2, 3, 9, 13, 6, 7, 10, 11, 14, 15 ];
const INDEO4_SCAN_4X4_VER: [usize; 16] = [ 0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15 ];
const INDEO4_SCAN_4X4_HOR: [usize; 16] = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ];

const INDEO4_SCANS_8X8: [&[usize; 64]; 5] = [
    &ZIGZAG, &INDEO4_SCAN_8X8_ALT, &IVI_SCAN_8X8_HOR, &IVI_SCAN_8X8_VER, &ZIGZAG
];
const INDEO4_SCANS_4X4: [&[usize; 16]; 5] = [
    &IVI_SCAN_4X4, &INDEO4_SCAN_4X4_ALT, &INDEO4_SCAN_4X4_VER, &INDEO4_SCAN_4X4_HOR, &IVI_SCAN_4X4
];

const INDEO4_Q8X8_IDX: [usize; 15] = [ 0, 1, 0, 2, 1, 3, 0, 4, 1, 5, 0, 1, 6, 7, 8 ];
const INDEO4_Q4X4_IDX: [usize; 7] = [ 0, 1, 2, 2, 3, 3, 4 ];

const INDEO4_QUANT8X8_INTRA: [[u16; 64]; 9] = [
  [
      43,  342,  385,  470,  555,  555,  598,  726,
     342,  342,  470,  513,  555,  598,  726,  769,
     385,  470,  555,  555,  598,  726,  726,  811,
     470,  470,  555,  555,  598,  726,  769,  854,
     470,  555,  555,  598,  683,  726,  854, 1025,
     555,  555,  598,  683,  726,  854, 1025, 1153,
     555,  555,  598,  726,  811,  982, 1195, 1451,
     555,  598,  726,  811,  982, 1195, 1451, 1793
  ], [
      86, 1195, 2390, 2390, 4865, 4865, 4865, 4865,
    1195, 1195, 2390, 2390, 4865, 4865, 4865, 4865,
    2390, 2390, 4865, 4865, 6827, 6827, 6827, 6827,
    2390, 2390, 4865, 4865, 6827, 6827, 6827, 6827,
    4865, 4865, 6827, 6827, 6827, 6827, 6827, 6827,
    4865, 4865, 6827, 6827, 6827, 6827, 6827, 6827,
    4865, 4865, 6827, 6827, 6827, 6827, 6827, 6827,
    4865, 4865, 6827, 6827, 6827, 6827, 6827, 6827
  ], [
     235, 1067, 1195, 1323, 1451, 1579, 1707, 1835,
     235, 1067, 1195, 1323, 1451, 1579, 1707, 1835,
     235, 1067, 1195, 1323, 1451, 1579, 1707, 1835,
     235, 1067, 1195, 1323, 1451, 1579, 1707, 1835,
     235, 1067, 1195, 1323, 1451, 1579, 1707, 1835,
     235, 1067, 1195, 1323, 1451, 1579, 1707, 1835,
     235, 1067, 1195, 1323, 1451, 1579, 1707, 1835,
     235, 1067, 1195, 1323, 1451, 1579, 1707, 1835
  ], [
    1707, 1707, 3414, 3414, 3414, 3414, 3414, 3414,
    1707, 1707, 3414, 3414, 3414, 3414, 3414, 3414,
    1707, 1707, 3414, 3414, 3414, 3414, 3414, 3414,
    1707, 1707, 3414, 3414, 3414, 3414, 3414, 3414,
    1707, 1707, 3414, 3414, 3414, 3414, 3414, 3414,
    1707, 1707, 3414, 3414, 3414, 3414, 3414, 3414,
    1707, 1707, 3414, 3414, 3414, 3414, 3414, 3414,
    1707, 1707, 3414, 3414, 3414, 3414, 3414, 3414
  ], [
     897,  897,  897,  897,  897,  897,  897,  897,
    1067, 1067, 1067, 1067, 1067, 1067, 1067, 1067,
    1238, 1238, 1238, 1238, 1238, 1238, 1238, 1238,
    1409, 1409, 1409, 1409, 1409, 1409, 1409, 1409,
    1579, 1579, 1579, 1579, 1579, 1579, 1579, 1579,
    1750, 1750, 1750, 1750, 1750, 1750, 1750, 1750,
    1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921,
    2091, 2091, 2091, 2091, 2091, 2091, 2091, 2091
  ], [
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    3414, 3414, 3414, 3414, 3414, 3414, 3414, 3414,
    3414, 3414, 3414, 3414, 3414, 3414, 3414, 3414,
    3414, 3414, 3414, 3414, 3414, 3414, 3414, 3414,
    3414, 3414, 3414, 3414, 3414, 3414, 3414, 3414,
    3414, 3414, 3414, 3414, 3414, 3414, 3414, 3414,
    3414, 3414, 3414, 3414, 3414, 3414, 3414, 3414
  ], [
    2390, 2390, 2390, 2390, 2390, 2390, 2390, 2390,
    2390, 2390, 2390, 2390, 2390, 2390, 2390, 2390,
    2390, 2390, 2390, 2390, 2390, 2390, 2390, 2390,
    2390, 2390, 2390, 2390, 2390, 2390, 2390, 2390,
    2390, 2390, 2390, 2390, 2390, 2390, 2390, 2390,
    2390, 2390, 2390, 2390, 2390, 2390, 2390, 2390,
    2390, 2390, 2390, 2390, 2390, 2390, 2390, 2390,
    2390, 2390, 2390, 2390, 2390, 2390, 2390, 2390
  ], [
      22,  171,  214,  257,  257,  299,  299,  342,
     171,  171,  257,  257,  299,  299,  342,  385,
     214,  257,  257,  299,  299,  342,  342,  385,
     257,  257,  257,  299,  299,  342,  385,  427,
     257,  257,  299,  299,  342,  385,  427,  513,
     257,  299,  299,  342,  385,  427,  513,  598,
     299,  299,  299,  385,  385,  470,  598,  726,
     299,  299,  385,  385,  470,  598,  726,  897
  ], [
      86,  598, 1195, 1195, 2390, 2390, 2390, 2390,
     598,  598, 1195, 1195, 2390, 2390, 2390, 2390,
    1195, 1195, 2390, 2390, 3414, 3414, 3414, 3414,
    1195, 1195, 2390, 2390, 3414, 3414, 3414, 3414,
    2390, 2390, 3414, 3414, 3414, 3414, 3414, 3414,
    2390, 2390, 3414, 3414, 3414, 3414, 3414, 3414,
    2390, 2390, 3414, 3414, 3414, 3414, 3414, 3414,
    2390, 2390, 3414, 3414, 3414, 3414, 3414, 3414
  ]
];
const INDEO4_QUANT8X8_INTER: [[u16; 64]; 9] = [
  [
     427,  427,  470,  427,  427,  427,  470,  470,
     427,  427,  470,  427,  427,  427,  470,  470,
     470,  470,  470,  470,  470,  470,  470,  470,
     427,  427,  470,  470,  427,  427,  470,  470,
     427,  427,  470,  427,  427,  427,  470,  470,
     427,  427,  470,  427,  427,  427,  470,  470,
     470,  470,  470,  470,  470,  470,  470,  470,
     470,  470,  470,  470,  470,  470,  470,  470
  ], [
    1707, 1707, 2433, 2433, 3414, 3414, 3414, 3414,
    1707, 1707, 2433, 2433, 3414, 3414, 3414, 3414,
    2433, 2433, 3414, 3414, 4822, 4822, 4822, 4822,
    2433, 2433, 3414, 3414, 4822, 4822, 4822, 4822,
    3414, 3414, 4822, 4822, 3414, 3414, 3414, 3414,
    3414, 3414, 4822, 4822, 3414, 3414, 3414, 3414,
    3414, 3414, 4822, 4822, 3414, 3414, 3414, 3414,
    3414, 3414, 4822, 4822, 3414, 3414, 3414, 3414
  ], [
    1195, 1195, 1281, 1238, 1195, 1195, 1281, 1281,
    1195, 1195, 1281, 1238, 1195, 1195, 1281, 1281,
    1195, 1195, 1281, 1238, 1195, 1195, 1281, 1281,
    1195, 1195, 1281, 1238, 1195, 1195, 1281, 1281,
    1195, 1195, 1281, 1238, 1195, 1195, 1281, 1281,
    1195, 1195, 1281, 1238, 1195, 1195, 1281, 1281,
    1195, 1195, 1281, 1238, 1195, 1195, 1281, 1281,
    1195, 1195, 1281, 1238, 1195, 1195, 1281, 1281
  ], [
    2433, 2433, 3414, 3414, 2433, 2433, 2433, 2433,
    2433, 2433, 3414, 3414, 2433, 2433, 2433, 2433,
    2433, 2433, 3414, 3414, 2433, 2433, 2433, 2433,
    2433, 2433, 3414, 3414, 2433, 2433, 2433, 2433,
    2433, 2433, 3414, 3414, 2433, 2433, 2433, 2433,
    2433, 2433, 3414, 3414, 2433, 2433, 2433, 2433,
    2433, 2433, 3414, 3414, 2433, 2433, 2433, 2433,
    2433, 2433, 3414, 3414, 2433, 2433, 2433, 2433
  ], [
    1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195,
    1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195,
    1281, 1281, 1281, 1281, 1281, 1281, 1281, 1281,
    1238, 1238, 1238, 1238, 1238, 1238, 1238, 1238,
    1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195,
    1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195,
    1281, 1281, 1281, 1281, 1281, 1281, 1281, 1281,
    1281, 1281, 1281, 1281, 1281, 1281, 1281, 1281
  ], [
    2433, 2433, 2433, 2433, 2433, 2433, 2433, 2433,
    2433, 2433, 2433, 2433, 2433, 2433, 2433, 2433,
    3414, 3414, 3414, 3414, 3414, 3414, 3414, 3414,
    3414, 3414, 3414, 3414, 3414, 3414, 3414, 3414,
    2433, 2433, 2433, 2433, 2433, 2433, 2433, 2433,
    2433, 2433, 2433, 2433, 2433, 2433, 2433, 2433,
    2433, 2433, 2433, 2433, 2433, 2433, 2433, 2433,
    2433, 2433, 2433, 2433, 2433, 2433, 2433, 2433
  ], [
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
    1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707
  ], [
      86,  171,  171,  214,  214,  214,  214,  257,
     171,  171,  214,  214,  214,  214,  257,  257,
     171,  214,  214,  214,  214,  257,  257,  257,
     214,  214,  214,  214,  257,  257,  257,  299,
     214,  214,  214,  257,  257,  257,  299,  299,
     214,  214,  257,  257,  257,  299,  299,  299,
     214,  257,  257,  257,  299,  299,  299,  342,
     257,  257,  257,  299,  299,  299,  342,  342
  ], [
     854,  854, 1195, 1195, 1707, 1707, 1707, 1707,
     854,  854, 1195, 1195, 1707, 1707, 1707, 1707,
    1195, 1195, 1707, 1707, 2390, 2390, 2390, 2390,
    1195, 1195, 1707, 1707, 2390, 2390, 2390, 2390,
    1707, 1707, 2390, 2390, 1707, 1707, 1707, 1707,
    1707, 1707, 2390, 2390, 1707, 1707, 1707, 1707,
    1707, 1707, 2390, 2390, 1707, 1707, 1707, 1707,
    1707, 1707, 2390, 2390, 1707, 1707, 1707, 1707
  ]
];
const INDEO4_QUANT4X4_INTRA: [[u16; 16]; 5] = [
  [
      22,  214,  257,  299,
     214,  257,  299,  342,
     257,  299,  342,  427,
     299,  342,  427,  513
  ], [
     129, 1025, 1451, 1451,
    1025, 1025, 1451, 1451,
    1451, 1451, 2049, 2049,
    1451, 1451, 2049, 2049
  ], [
      43,  171,  171,  171,
      43,  171,  171,  171,
      43,  171,  171,  171,
      43,  171,  171,  171
  ], [
      43,   43,   43,   43,
     171,  171,  171,  171,
     171,  171,  171,  171,
     171,  171,  171,  171
  ], [
      43,   43,   43,   43,
      43,   43,   43,   43,
      43,   43,   43,   43,
      43,   43,   43,   43
  ]
];
const INDEO4_QUANT4X4_INTER: [[u16; 16]; 5] = [
  [
     107,  214,  257,  299,
     214,  257,  299,  299,
     257,  299,  299,  342,
     299,  299,  342,  342
  ], [
     513, 1025, 1238, 1238,
    1025, 1025, 1238, 1238,
    1238, 1238, 1451, 1451,
    1238, 1238, 1451, 1451
  ], [
      43,  171,  171,  171,
      43,  171,  171,  171,
      43,  171,  171,  171,
      43,  171,  171,  171
  ], [
      43,   43,   43,   43,
     171,  171,  171,  171,
     171,  171,  171,  171,
     171,  171,  171,  171
  ], [
      43,   43,   43,   43,
      43,   43,   43,   43,
      43,   43,   43,   43,
      43,   43,   43,   43
  ]
];
const INDEO4_Q8_INTRA: [&[u16; 64]; 9] = [
    &INDEO4_QUANT8X8_INTRA[0], &INDEO4_QUANT8X8_INTRA[1], &INDEO4_QUANT8X8_INTRA[2],
    &INDEO4_QUANT8X8_INTRA[3], &INDEO4_QUANT8X8_INTRA[4], &INDEO4_QUANT8X8_INTRA[5],
    &INDEO4_QUANT8X8_INTRA[6], &INDEO4_QUANT8X8_INTRA[7], &INDEO4_QUANT8X8_INTRA[8],
];
const INDEO4_Q8_INTER: [&[u16; 64]; 9] = [
    &INDEO4_QUANT8X8_INTER[0], &INDEO4_QUANT8X8_INTER[1], &INDEO4_QUANT8X8_INTER[2],
    &INDEO4_QUANT8X8_INTER[3], &INDEO4_QUANT8X8_INTER[4], &INDEO4_QUANT8X8_INTER[5],
    &INDEO4_QUANT8X8_INTER[6], &INDEO4_QUANT8X8_INTER[7], &INDEO4_QUANT8X8_INTER[8],
];
const INDEO4_Q4_INTRA: [&[u16; 16]; 5] = [
    &INDEO4_QUANT4X4_INTRA[0], &INDEO4_QUANT4X4_INTRA[1], &INDEO4_QUANT4X4_INTRA[2],
    &INDEO4_QUANT4X4_INTRA[3], &INDEO4_QUANT4X4_INTRA[4]
];
const INDEO4_Q4_INTER: [&[u16; 16]; 5] = [
    &INDEO4_QUANT4X4_INTER[0], &INDEO4_QUANT4X4_INTER[1], &INDEO4_QUANT4X4_INTER[2],
    &INDEO4_QUANT4X4_INTER[3], &INDEO4_QUANT4X4_INTER[4]
];

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(Indeo4Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::indeo_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_indeo4() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        indeo_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/IV41/indeo4-avi/volcano.avi
        test_decoding("avi", "indeo4", "assets/Indeo/IV4/volcano.avi", Some(16),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x194f626b, 0x023fdfd0, 0x9809665a, 0xd68f6f47],
                            [0x194f626b, 0x023fdfd0, 0x9809665a, 0xd68f6f47],
                            [0x194f626b, 0x023fdfd0, 0x9809665a, 0xd68f6f47],
                            [0x194f626b, 0x023fdfd0, 0x9809665a, 0xd68f6f47],
                            [0x46c6719d, 0xe6415ac0, 0x3e4d9799, 0xd2f5747d],
                            [0xe0278b0f, 0x3e4763d5, 0x88033344, 0xc9c2e6de],
                            [0xd962be7f, 0xafc1ac64, 0x0647cdcc, 0xd06465c6],
                            [0xedef0e19, 0xec75eed2, 0x955a2ae2, 0xd6145b4c],
                            [0x89ec8d4b, 0x3d446d74, 0xbd3d681d, 0x2d219dca],
                            [0x89e81643, 0x77fb2f1b, 0x2aa0782f, 0xb1b9b7ef],
                            [0xea283aec, 0x94d7cdf9, 0x961bbb69, 0x2b38162a],
                            [0x1d1b315c, 0x6613c5fa, 0xeff36485, 0x5025fbf2],
                            [0x4145c6a8, 0xd8d513b1, 0x34a5d353, 0x07750cd5],
                            [0xace12feb, 0x468754f3, 0xa72327f5, 0x1a6f6350],
                            [0x4b04dc0e, 0x684533a7, 0x6a4e4b16, 0x0b8a5e68],
                            [0xa3eb64fc, 0x5e02a31b, 0x6b484eae, 0xbb6e6c49],
                            [0x7d4ef46e, 0x6761c447, 0x02e002f5, 0x02d0231c]]));
        // a sample from Civilization II
        test_decoding("avi", "indeo4", "assets/Indeo/IV4/HRLDVIK.AVI", Some(8),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x239b8b87, 0x6dbec08c, 0x82bae1f0, 0x868e00c2],
                            [0xe2298beb, 0x4e08866d, 0x00cb6201, 0x6b0a6df3],
                            [0x9d7f4cf0, 0xed33df12, 0x2677be16, 0xce7e99b0],
                            [0x0c8d7489, 0x2b3ac56e, 0x36d75559, 0x70550903],
                            [0xc32b4b78, 0x2fc81737, 0xe4d7722b, 0xbcbbb35e],
                            [0x20bfd5e8, 0x6cfad540, 0xfc6c6b6c, 0xa4f39a7d],
                            [0xc327428d, 0x4e817b56, 0x4376eba2, 0xebafd04a],
                            [0x6a53a6ec, 0x7477a471, 0xd55bc98e, 0x7498de0f],
                            [0x398eba3a, 0x3cf3cce1, 0x90211dfe, 0x82c906f0]]));
    }
}
