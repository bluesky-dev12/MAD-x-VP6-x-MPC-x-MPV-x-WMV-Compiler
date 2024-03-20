use nihav_core::io::bitreader::*;
use nihav_core::formats;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::ZIGZAG;
use super::ivi::*;
use super::ivibr::*;

fn calc_quant(glob_q: u32, qd: i16) -> usize {
    let qq = (glob_q as i16) + qd;
    if qq < 0 {
        0
    } else if qq > 23 {
        23
    } else {
        qq as usize
    }
}

struct Indeo5Parser {
    mb_cb:          IVICodebook,

    width:          usize,
    height:         usize,
    tile_w:         usize,
    tile_h:         usize,
    luma_bands:     usize,
    chroma_bands:   usize,

    is_hpel:        [bool; 5],
    mb_size:        [usize; 5],
    blk_size:       [usize; 5],
}

impl Indeo5Parser {
    fn new() -> Self {
        Indeo5Parser {
            mb_cb:      IVI_CB_ZERO,

            width:          0,
            height:         0,
            tile_w:         0,
            tile_h:         0,
            luma_bands:     0,
            chroma_bands:   0,

            is_hpel:    [false; 5],
            mb_size:    [0; 5],
            blk_size:   [0; 5],
        }
    }
}

fn skip_extension(br: &mut BitReader) -> DecoderResult<()> {
    loop {
        let len             = br.read(8)?;
        if len == 0 { break; }
        br.skip(len * 8)?;
    }
    Ok(())
}

impl IndeoXParser for Indeo5Parser {
#[allow(unused_variables)]
#[allow(unused_assignments)]
    fn decode_picture_header(&mut self, br: &mut BitReader) -> DecoderResult<PictureHeader> {
        let sync                = br.read(5)?;
        validate!(sync == 0x1F);
        let ftype_idx           = br.read(3)?;
        validate!(ftype_idx < 5);
        let ftype               = INDEO5_FRAME_TYPE[ftype_idx as usize];
        let fnum                = br.read(8)?;
        if ftype == IVIFrameType::Intra {
            let gop_flags       = br.read(8)?;
            let hdr_size;
            if (gop_flags & 0x01) != 0 {
                hdr_size        = br.read(16)?;
            } else {
                hdr_size = 0;
            }
            if (gop_flags & 0x20) != 0 {
                br.skip(32)?; // lock word
            }
            self.tile_w = 0;
            self.tile_h = 0;
            if (gop_flags & 0x40) != 0 {
                self.tile_w     = 64 << br.read(2)?;
                self.tile_h = self.tile_w;
            }
            validate!(self.tile_w < 256);
            self.luma_bands     = (br.read(2)? * 3 + 1) as usize;
            self.chroma_bands   = (br.read(1)? * 3 + 1) as usize;
            validate!((self.luma_bands == 4) || (self.luma_bands == 1));
            validate!(self.chroma_bands == 1);
            let pic_size_idx    = br.read(4)? as usize;
            let w;
            let h;
            if pic_size_idx < 15 {
                w = INDEO5_PICTURE_SIZE_TAB[pic_size_idx][0];
                h = INDEO5_PICTURE_SIZE_TAB[pic_size_idx][1];
            } else {
                h               = br.read(13)? as usize;
                w               = br.read(13)? as usize;
            }
            validate!((w != 0) && (h != 0));
            self.width  = w;
            self.height = h;

            validate!((gop_flags & 0x02) == 0);
            if self.tile_w == 0 {
                self.tile_w = w;
                self.tile_h = h;
            }
            for b in 0..self.luma_bands+self.chroma_bands {
                self.is_hpel[b]     = br.read_bool()?;
                let mb_scale        = br.read(1)?;
                self.blk_size[b]    = 8 >> br.read(1)?;
                self.mb_size[b]     = self.blk_size[b] << (1 - mb_scale);
                let ext_tr          = br.read_bool()?;
                validate!(!ext_tr);
                let end_marker      = br.read(2)?;
                validate!(end_marker == 0);
            }
            if (gop_flags & 0x08) != 0 {
                let align       = br.read(3)?;
                validate!(align == 0);
                if br.read_bool()? {
                    br.skip(24)?; // transparency color
                }
            }
            br.align();
            br.skip(23)?;
            if br.read_bool()? { // gop extension
                loop {
                    let v       = br.read(16)?;
                    if (v & 0x8000) == 0 { break; }
                }
            }
            br.align();
        }
        if ftype.is_null() {
            br.align();
            return Ok(PictureHeader::new_null(ftype));
        }
        let flags               = br.read(8)?;
        let size;
        if (flags & 0x01) != 0 {
            size                = br.read(24)?;
        } else {
            size = 0;
        }
        let checksum;
        if (flags & 0x10) != 0 {
            checksum            = br.read(16)?;
        } else {
            checksum = 0;
        }
        if (flags & 0x20) != 0 {
            skip_extension(br)?;
        }
        let in_q = (flags & 0x08) != 0;
        self.mb_cb              = br.read_ivi_codebook_desc(true, (flags & 0x40) != 0)?;
        br.skip(3)?;
        br.align();

        Ok(PictureHeader::new(ftype, self.width, self.height, self.tile_w, self.tile_h, false, self.luma_bands, self.chroma_bands, in_q))
    }

#[allow(unused_variables)]
    fn decode_band_header(&mut self, br: &mut BitReader, pic_hdr: &PictureHeader, plane_no: usize, band_no: usize) -> DecoderResult<BandHeader> {
        let band_flags      = br.read(8)?;

        if (band_flags & 0x01) != 0 {
            br.align();
            return Ok(BandHeader::new_empty(plane_no, band_no));
        }
        let inherit_mv = (band_flags & 0x02) != 0;
        let has_qdelta = (band_flags & 0x04) != 0;
        let inherit_qd = ((band_flags & 0x08) != 0) || !has_qdelta;
        let data_size: usize;
        if (band_flags & 0x80) != 0 {
            data_size       = br.read(24)? as usize;
            validate!(data_size >= 4);
        } else {
            data_size = 0;
        }
        validate!(data_size <= ((br.left() / 8) as usize) + 4);

        let num_corr: usize;
        let mut corr_map: [u8; CORR_MAP_SIZE] = [0; CORR_MAP_SIZE];
        if (band_flags & 0x10) != 0 {
            num_corr = br.read(8)? as usize;
            validate!(num_corr*2 <= CORR_MAP_SIZE);
            for i in 0..num_corr*2 {
                corr_map[i] = br.read(8)? as u8;
            }
        } else {
            num_corr = 0;
        }
        let rvmap_idx;
        if (band_flags & 0x40) != 0 {
            rvmap_idx       = br.read(3)? as usize;
        } else {
            rvmap_idx = 8;
        }
        let blk_cb = br.read_ivi_codebook_desc(false, (band_flags & 0x80) != 0)?;
        if br.read_bool()? {
            br.skip(16)?; // checksum
        }
        let band_q          = br.read(5)?;
        if (band_flags & 0x20) != 0 {
            skip_extension(br)?;
        }
        br.align();

        let tr;
        let txtype;
        let band_id = if plane_no == 0 { band_no } else { self.luma_bands };
        match plane_no {
            0 => {
                    let scan = INDEO5_SCAN8X8[band_no];
                    let qintra;
                    let qinter;
                    validate!(self.blk_size[band_id] == 8);
                    match band_no {
                        0 => {
                                tr = IVITransformType::Slant(TSize::T8x8, TDir::TwoD);
                                if self.luma_bands == 1 {
                                    qintra = INDEO5_Q8_INTRA[0];
                                    qinter = INDEO5_Q8_INTER[0];
                                } else {
                                    qintra = INDEO5_Q8_INTRA[1];
                                    qinter = INDEO5_Q8_INTER[1];
                                }
                            },
                        1 => {
                                tr = IVITransformType::Slant(TSize::T8x8, TDir::Row);
                                qintra = INDEO5_Q8_INTRA[2];
                                qinter = INDEO5_Q8_INTER[2];
                            },
                        2 => {
                                tr = IVITransformType::Slant(TSize::T8x8, TDir::Col);
                                qintra = INDEO5_Q8_INTRA[3];
                                qinter = INDEO5_Q8_INTER[3];
                            },
                        3 => {
                                tr = IVITransformType::None(TSize::T8x8);
                                qintra = INDEO5_Q8_INTRA[4];
                                qinter = INDEO5_Q8_INTER[4];
                            },
                        _ => { unreachable!(); }
                    };
                    txtype = TxType::Transform8(TxParams8x8::new(qintra, qinter, scan));
                },
            1 | 2 => {
                    validate!(self.blk_size[band_id] == 4);
                    tr = IVITransformType::Slant(TSize::T4x4, TDir::TwoD);
                    let scan = INDEO5_SCAN4X4;
                    let qintra = INDEO5_Q4_INTRA;
                    let qinter = INDEO5_Q4_INTER;
                    txtype = TxType::Transform4(TxParams4x4::new(qintra, qinter, scan));
                },
            _ => { unreachable!(); }
        };

        Ok(BandHeader::new(plane_no, band_no, self.mb_size[band_id], self.blk_size[band_id], self.is_hpel[band_id], inherit_mv, has_qdelta, inherit_qd, band_q, rvmap_idx, num_corr, corr_map, blk_cb, tr, txtype))
    }

    #[allow(clippy::cognitive_complexity)]
    fn decode_mb_info(&mut self, br: &mut BitReader, pic_hdr: &PictureHeader, band: &BandHeader, tile: &mut IVITile, ref_tile: Option<&IVITile>, mv_scale: u8) -> DecoderResult<()> {
        let mut mv_x = 0;
        let mut mv_y = 0;
        let band_id = if pic_hdr.luma_bands == 4 { band.band_no + 1 } else { 0 };
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
                        mb.mtype = if br.read_bool()? { MBType::Inter } else { MBType::Intra };
                    }
                    if band.mb_size == band.blk_size {
                        mb.cbp = br.read(1)? as u8;
                    } else {
                        mb.cbp = br.read(4)? as u8;
                    }
                    let q;
                    if band.has_qdelta {
                        if band.inherit_qd {
                            if let Some(tileref) = ref_tile {
                                mb.qd = tileref.mb[mb_idx].qd;
                                q = calc_quant(band.quant, mb.qd);
                            } else {
                                return Err(DecoderError::MissingReference);
                            }
                        } else if (mb.cbp != 0) || ((band.plane_no == 0) && (band.band_no == 0) && pic_hdr.in_q) {
                            mb.qd = br.read_ivi_cb_s(&self.mb_cb)? as i16;
                            q = calc_quant(band.quant, mb.qd);
                        } else {
                            q = band.quant as usize;
                        }
                    } else {
                        q = band.quant as usize;
                    }

                    if mb.mtype == MBType::Intra {
                        if band.blk_size == 8 {
                            mb.q = INDEO5_QSCALE8_INTRA[band_id][q];
                        } else {
                            mb.q = INDEO5_QSCALE4_INTRA[q];
                        }
                    } else {
                        if band.blk_size == 8 {
                            mb.q = INDEO5_QSCALE8_INTER[band_id][q];
                        } else {
                            mb.q = INDEO5_QSCALE4_INTER[q];
                        }
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
                        }
                    }
                } else {
                    validate!(!pic_hdr.ftype.is_intra());
                    mb.mtype = MBType::Inter;
                    mb.cbp   = 0;
                    mb.qd    = 0;
                    if (band.plane_no == 0) && (band.band_no == 0) && pic_hdr.in_q {
                        mb.qd = br.read_ivi_cb_s(&self.mb_cb)? as i16;
                        let q = calc_quant(band.quant, mb.qd);
                        if mb.mtype == MBType::Intra {
                            if band.blk_size == 8 {
                                mb.q = INDEO5_QSCALE8_INTRA[band_id][q];
                            } else {
                                mb.q = INDEO5_QSCALE4_INTRA[q];
                            }
                        } else {
                            if band.blk_size == 8 {
                                mb.q = INDEO5_QSCALE8_INTER[band_id][q];
                            } else {
                                mb.q = INDEO5_QSCALE4_INTER[q];
                            }
                        }
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
        let mut idx0 = 0;
        let mut idx1 = w / 2;
        let mut idx2 = (h / 2) * sstride;
        let mut idx3 = idx2 + idx1;
        let mut bidx1 = idx1;
        let mut bidx3 = idx3;
        let mut oidx0 = 0;
        let mut oidx1 = dstride;
        let filt_lo = |a: i16, b: i16| a + b;
        let filt_hi = |a: i16, b: i16, c: i16| a - b * 6 + c;

        for _ in 0..(h/2)-1 {
            let mut b0_1 = src[idx0];
            let mut b0_2 = src[idx0 + sstride];
            let mut b1_1 = src[bidx1];
            let mut b1_2 = src[idx1];
            let mut b1_3 = filt_hi(b1_1, b1_2, src[idx1 + sstride]);
            let mut b2_1;
            let mut b2_2 = src[idx2];
            let mut b2_3 = b2_2;
            let mut b2_4;
            let mut b2_5 = src[idx2 + sstride];
            let mut b2_6 = b2_5;
            let mut b3_1;
            let mut b3_2 = src[bidx3];
            let mut b3_3 = b3_2;
            let mut b3_4;
            let mut b3_5 = src[idx3];
            let mut b3_6 = b3_5;
            let mut b3_8 = filt_hi(b3_2, b3_5, src[idx3 + sstride]);
            let mut b3_9 = b3_8;
            let mut b3_7;

            for x in 0..(w/2)-1 {
                b2_1 = b2_2;
                b2_2 = b2_3;
                b2_4 = b2_5;
                b2_5 = b2_6;
                b3_1 = b3_2;
                b3_2 = b3_3;
                b3_4 = b3_5;
                b3_5 = b3_6;
                b3_7 = b3_8;
                b3_8 = b3_9;

                let tmp0 = b0_1;
                let tmp1 = b0_2;
                b0_1 = src[idx0 + x + 1];
                b0_2 = src[idx0 + x + 1 + sstride];
                let mut p0 =  tmp0                       << 4;
                let mut p1 = (tmp0 + b0_1)               << 3;
                let mut p2 = (tmp0 + tmp1)               << 3;
                let mut p3 = (tmp0 + tmp1 + b0_1 + b0_2) << 2;

                let tmp0 = b1_1;
                let tmp1 = b1_2;
                let tmp2 = filt_lo(tmp0, tmp1);
                let tmp3 = filt_hi(tmp0, tmp1, b1_3);
                b1_2 = src[ idx1 + x + 1];
                b1_1 = src[bidx1 + x + 1];
                b1_3 = filt_hi(b1_1, b1_2, src[idx1 + x + 1 + sstride]);
                p0 +=  tmp2                << 3;
                p1 += (tmp2 + b1_1 + b1_2) << 2;
                p2 +=  tmp3                << 2;
                p3 += (tmp3 + b1_3)        << 1;

                b2_3 = src[idx2 + x + 1];
                b2_6 = src[idx2 + x + 1 + sstride];
                let tmp0 = filt_lo(b2_1, b2_2);
                let tmp1 = filt_hi(b2_1, b2_2, b2_3);
                p0 +=  tmp0                              << 3;
                p1 +=  tmp1                              << 2;
                p2 += (tmp0 + filt_lo(b2_4, b2_5))       << 2;
                p3 += (tmp1 + filt_hi(b2_4, b2_5, b2_6)) << 1;

                b3_6 = src[idx3 + x + 1];
                b3_3 = src[bidx3 + x + 1];
                b3_9 = filt_hi(b3_3, b3_6, src[idx3 + x + 1 + sstride]);
                let tmp0 = b3_1 + b3_4;
                let tmp1 = b3_2 + b3_5;
                let tmp2 = b3_3 + b3_6;
                p0 += filt_lo(tmp0, tmp1)       << 2;
                p1 += filt_hi(tmp0, tmp1, tmp2) << 1;
                p2 += filt_lo(b3_7, b3_8)       << 1;
                p3 += filt_hi(b3_7, b3_8, b3_9) << 0;

                dst[oidx0 + x * 2 + 0] = clip8((p0 >> 6) + 128);
                dst[oidx0 + x * 2 + 1] = clip8((p1 >> 6) + 128);
                dst[oidx1 + x * 2 + 0] = clip8((p2 >> 6) + 128);
                dst[oidx1 + x * 2 + 1] = clip8((p3 >> 6) + 128);
            }
            bidx1 = idx1;
            bidx3 = idx3;
            idx0 += sstride;
            idx1 += sstride;
            idx2 += sstride;
            idx3 += sstride;
            oidx0 += dstride * 2;
            oidx1 += dstride * 2;
        }
    }
}

struct Indeo5Decoder {
    info:   NACodecInfoRef,
    dec:    IVIDecoder,
    ip:     Indeo5Parser,
}

impl Indeo5Decoder {
    fn new(scalable: bool) -> Self {
        Indeo5Decoder {
            info:   NACodecInfo::new_dummy(),
            dec:    IVIDecoder::new(scalable),
            ip:     Indeo5Parser::new(),
        }
    }
}

impl NADecoder for Indeo5Decoder {
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

        let bufinfo = self.dec.decode_frame(&mut self.ip, &mut br)?;
        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(self.dec.is_intra());
        frm.set_frame_type(self.dec.get_frame_type());
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for Indeo5Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

const INDEO5_PICTURE_SIZE_TAB: [[usize; 2]; 15] = [
    [640, 480], [320, 240], [160, 120], [704, 480], [352, 240], [352, 288], [176, 144],
    [240, 180], [640, 240], [704, 240], [80, 60], [88, 72], [0, 0], [0, 0], [0, 0]
];

const INDEO5_FRAME_TYPE: [IVIFrameType; 5] = [
    IVIFrameType::Intra, IVIFrameType::Inter, IVIFrameType::InterScal,
    IVIFrameType::InterDroppable, IVIFrameType::NULL,
];

const INDEO5_QUANT8X8_INTRA: [[u16; 64]; 5] = [
  [
    0x1a, 0x2e, 0x36, 0x42, 0x46, 0x4a, 0x4e, 0x5a,
    0x2e, 0x32, 0x3e, 0x42, 0x46, 0x4e, 0x56, 0x6a,
    0x36, 0x3e, 0x3e, 0x44, 0x4a, 0x54, 0x66, 0x72,
    0x42, 0x42, 0x44, 0x4a, 0x52, 0x62, 0x6c, 0x7a,
    0x46, 0x46, 0x4a, 0x52, 0x5e, 0x66, 0x72, 0x8e,
    0x4a, 0x4e, 0x54, 0x62, 0x66, 0x6e, 0x86, 0xa6,
    0x4e, 0x56, 0x66, 0x6c, 0x72, 0x86, 0x9a, 0xca,
    0x5a, 0x6a, 0x72, 0x7a, 0x8e, 0xa6, 0xca, 0xfe,
  ], [
    0x26, 0x3a, 0x3e, 0x46, 0x4a, 0x4e, 0x52, 0x5a,
    0x3a, 0x3e, 0x42, 0x46, 0x4a, 0x4e, 0x56, 0x5e,
    0x3e, 0x42, 0x46, 0x48, 0x4c, 0x52, 0x5a, 0x62,
    0x46, 0x46, 0x48, 0x4a, 0x4e, 0x56, 0x5e, 0x66,
    0x4a, 0x4a, 0x4c, 0x4e, 0x52, 0x5a, 0x62, 0x6a,
    0x4e, 0x4e, 0x52, 0x56, 0x5a, 0x5e, 0x66, 0x6e,
    0x52, 0x56, 0x5a, 0x5e, 0x62, 0x66, 0x6a, 0x72,
    0x5a, 0x5e, 0x62, 0x66, 0x6a, 0x6e, 0x72, 0x76,
  ], [
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
  ], [
    0x4e, 0x4e, 0x4e, 0x4e, 0x4e, 0x4e, 0x4e, 0x4e,
    0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
    0xf2, 0xf2, 0xf2, 0xf2, 0xf2, 0xf2, 0xf2, 0xf2,
    0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4,
    0xde, 0xde, 0xde, 0xde, 0xde, 0xde, 0xde, 0xde,
    0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2,
    0xd6, 0xd6, 0xd6, 0xd6, 0xd6, 0xd6, 0xd6, 0xd6,
    0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2,
  ], [
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
  ]
];
const INDEO5_QUANT8X8_INTER: [[u16; 64]; 5] = [
  [
    0x26, 0x3a, 0x3e, 0x46, 0x4a, 0x4e, 0x52, 0x5a,
    0x3a, 0x3e, 0x42, 0x46, 0x4a, 0x4e, 0x56, 0x5e,
    0x3e, 0x42, 0x46, 0x48, 0x4c, 0x52, 0x5a, 0x62,
    0x46, 0x46, 0x48, 0x4a, 0x4e, 0x56, 0x5e, 0x66,
    0x4a, 0x4a, 0x4c, 0x4e, 0x52, 0x5a, 0x62, 0x6a,
    0x4e, 0x4e, 0x52, 0x56, 0x5a, 0x5e, 0x66, 0x6e,
    0x52, 0x56, 0x5a, 0x5e, 0x62, 0x66, 0x6a, 0x72,
    0x5a, 0x5e, 0x62, 0x66, 0x6a, 0x6e, 0x72, 0x76,
  ], [
    0x26, 0x3a, 0x3e, 0x46, 0x4a, 0x4e, 0x52, 0x5a,
    0x3a, 0x3e, 0x42, 0x46, 0x4a, 0x4e, 0x56, 0x5e,
    0x3e, 0x42, 0x46, 0x48, 0x4c, 0x52, 0x5a, 0x62,
    0x46, 0x46, 0x48, 0x4a, 0x4e, 0x56, 0x5e, 0x66,
    0x4a, 0x4a, 0x4c, 0x4e, 0x52, 0x5a, 0x62, 0x6a,
    0x4e, 0x4e, 0x52, 0x56, 0x5a, 0x5e, 0x66, 0x6e,
    0x52, 0x56, 0x5a, 0x5e, 0x62, 0x66, 0x6a, 0x72,
    0x5a, 0x5e, 0x62, 0x66, 0x6a, 0x6e, 0x72, 0x76,
  ], [
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
    0x4e, 0xaa, 0xf2, 0xd4, 0xde, 0xc2, 0xd6, 0xc2,
  ], [
    0x4e, 0x4e, 0x4e, 0x4e, 0x4e, 0x4e, 0x4e, 0x4e,
    0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
    0xf2, 0xf2, 0xf2, 0xf2, 0xf2, 0xf2, 0xf2, 0xf2,
    0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4,
    0xde, 0xde, 0xde, 0xde, 0xde, 0xde, 0xde, 0xde,
    0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2,
    0xd6, 0xd6, 0xd6, 0xd6, 0xd6, 0xd6, 0xd6, 0xd6,
    0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2, 0xc2,
  ], [
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e, 0x5e,
  ]
];
const INDEO5_QUANT4X4_INTRA: [u16; 16] = [
    0x1e, 0x3e, 0x4a, 0x52,
    0x3e, 0x4a, 0x52, 0x5e,
    0x4a, 0x52, 0x5e, 0x7a,
    0x52, 0x5e, 0x7a, 0x92
];
const INDEO5_QUANT4X4_INTER: [u16; 16] = [
    0x1e, 0x3e, 0x4a, 0x52,
    0x3e, 0x4a, 0x52, 0x56,
    0x4a, 0x52, 0x56, 0x5e,
    0x52, 0x56, 0x5e, 0x66
];
const INDEO5_Q8_INTRA: [&[u16; 64]; 5] = [
    &INDEO5_QUANT8X8_INTRA[0], &INDEO5_QUANT8X8_INTRA[1], &INDEO5_QUANT8X8_INTRA[2],
    &INDEO5_QUANT8X8_INTRA[3], &INDEO5_QUANT8X8_INTRA[4],
];
const INDEO5_Q8_INTER: [&[u16; 64]; 5] = [
    &INDEO5_QUANT8X8_INTER[0], &INDEO5_QUANT8X8_INTER[1], &INDEO5_QUANT8X8_INTER[2],
    &INDEO5_QUANT8X8_INTER[3], &INDEO5_QUANT8X8_INTER[4],
];
const INDEO5_Q4_INTRA: &[u16; 16] = &INDEO5_QUANT4X4_INTRA;
const INDEO5_Q4_INTER: &[u16; 16] = &INDEO5_QUANT4X4_INTER;

const INDEO5_SCAN8X8: [&[usize; 64]; 4] = [
    &ZIGZAG, &IVI_SCAN_8X8_VER, &IVI_SCAN_8X8_HOR, &IVI_SCAN_8X8_HOR
];
const INDEO5_SCAN4X4: &[usize; 16] = &IVI_SCAN_4X4;

const INDEO5_QSCALE8_INTRA: [[u8; 24]; 5] = [
  [
    0x0b, 0x0e, 0x10, 0x12, 0x14, 0x16, 0x17, 0x18, 0x1a, 0x1c, 0x1e, 0x20,
    0x22, 0x24, 0x27, 0x28, 0x2a, 0x2d, 0x2f, 0x31, 0x34, 0x37, 0x39, 0x3c,
  ], [
    0x01, 0x10, 0x12, 0x14, 0x16, 0x18, 0x1b, 0x1e, 0x22, 0x25, 0x28, 0x2c,
    0x30, 0x34, 0x38, 0x3d, 0x42, 0x47, 0x4c, 0x52, 0x58, 0x5e, 0x65, 0x6c,
  ], [
    0x13, 0x22, 0x27, 0x2a, 0x2d, 0x33, 0x36, 0x3c, 0x41, 0x45, 0x49, 0x4e,
    0x53, 0x58, 0x5d, 0x63, 0x69, 0x6f, 0x75, 0x7c, 0x82, 0x88, 0x8e, 0x95,
  ], [
    0x13, 0x1f, 0x21, 0x24, 0x27, 0x29, 0x2d, 0x2f, 0x34, 0x37, 0x3a, 0x3d,
    0x40, 0x44, 0x48, 0x4c, 0x4f, 0x52, 0x56, 0x5a, 0x5e, 0x62, 0x66, 0x6b,
  ], [
    0x31, 0x42, 0x47, 0x47, 0x4d, 0x52, 0x58, 0x58, 0x5d, 0x63, 0x67, 0x6b,
    0x6f, 0x73, 0x78, 0x7c, 0x80, 0x84, 0x89, 0x8e, 0x93, 0x98, 0x9d, 0xa4,
  ]
];
const INDEO5_QSCALE8_INTER: [[u8; 24]; 5] = [
  [
    0x0b, 0x11, 0x13, 0x14, 0x15, 0x16, 0x18, 0x1a, 0x1b, 0x1d, 0x20, 0x22,
    0x23, 0x25, 0x28, 0x2a, 0x2e, 0x32, 0x35, 0x39, 0x3d, 0x41, 0x44, 0x4a,
  ], [
    0x07, 0x14, 0x16, 0x18, 0x1b, 0x1e, 0x22, 0x25, 0x29, 0x2d, 0x31, 0x35,
    0x3a, 0x3f, 0x44, 0x4a, 0x50, 0x56, 0x5c, 0x63, 0x6a, 0x71, 0x78, 0x7e,
  ], [
    0x15, 0x25, 0x28, 0x2d, 0x30, 0x34, 0x3a, 0x3d, 0x42, 0x48, 0x4c, 0x51,
    0x56, 0x5b, 0x60, 0x65, 0x6b, 0x70, 0x76, 0x7c, 0x82, 0x88, 0x8f, 0x97,
  ], [
    0x13, 0x1f, 0x20, 0x22, 0x25, 0x28, 0x2b, 0x2d, 0x30, 0x33, 0x36, 0x39,
    0x3c, 0x3f, 0x42, 0x45, 0x48, 0x4b, 0x4e, 0x52, 0x56, 0x5a, 0x5e, 0x62,
  ], [
    0x3c, 0x52, 0x58, 0x5d, 0x63, 0x68, 0x68, 0x6d, 0x73, 0x78, 0x7c, 0x80,
    0x84, 0x89, 0x8e, 0x93, 0x98, 0x9d, 0xa3, 0xa9, 0xad, 0xb1, 0xb5, 0xba
  ]
];
const INDEO5_QSCALE4_INTRA: [u8; 24] = [
    0x01, 0x0b, 0x0b, 0x0d, 0x0d, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x13, 0x14,
    0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20
];
const INDEO5_QSCALE4_INTER: [u8; 24] = [
    0x0b, 0x0d, 0x0d, 0x0e, 0x11, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23
];

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(Indeo5Decoder::new(false))
}

pub fn get_decoder_scalable() -> Box<dyn NADecoder + Send> {
    Box::new(Indeo5Decoder::new(true))
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::indeo_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_indeo5() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        indeo_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/IV50/sample.avi
        test_decoding("avi", "indeo5", "assets/Indeo/IV5/sample.avi", Some(100),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0xcb451428, 0x5c8b1519, 0x96a81eda, 0x0c9cccf0],
                            [0x7554e11f, 0x3b60d661, 0x3bcd14b5, 0x036dcc16],
                            [0x92a20e68, 0x83418e5c, 0x640b6786, 0x9c867622],
                            [0x0b2cde46, 0x67b68ac5, 0xc152edc6, 0xda591ac3],
                            [0x9034354b, 0x81c66b57, 0xcb671505, 0x0062a75f],
                            [0x38606276, 0x547b83df, 0x4a22f079, 0x2afb8362],
                            [0x0d3fe5df, 0xadc8b042, 0x4ee9cb0d, 0xf9b79b11],
                            [0x2b0b1ddb, 0x612a163a, 0x47b5afc8, 0xeb4c79a5],
                            [0x8eadd17e, 0xc4c8c932, 0x57508232, 0x643e1498],
                            [0x0dcc7da2, 0x7b059de2, 0x0ff46dd9, 0x04c11f77],
                            [0xe358728a, 0xe42fc515, 0x8f5cc35f, 0x02973436],
                            [0x66847a32, 0x884b41f0, 0x0c3062bc, 0xb58d923b],
                            [0x555b2711, 0xbcb54b64, 0x5a6141e5, 0xa7b5cc44],
                            [0x0f326348, 0x5d36cb21, 0xe7f8210d, 0x8a43c2c5],
                            [0x4e7b18a3, 0x42146bbb, 0x04c4cf2b, 0x78fe3c0d],
                            [0x220f0721, 0xe681444a, 0x974bf97b, 0x9b658cfe],
                            [0x2d5a5f22, 0x23bd1ed8, 0xed383633, 0x5905422d],
                            [0x982ae872, 0x17b8f765, 0x3d1735af, 0xa2872d9f],
                            [0x743a2819, 0x392d856e, 0xf2ee64ca, 0x63101b79],
                            [0x0c9e0aa7, 0x79414f6b, 0x162c19a6, 0x86d69b96],
                            [0x3eae05ad, 0xbff350bb, 0x97e8a205, 0xfa8e42b2],
                            [0x0ccae893, 0xdfcf9885, 0xef39d053, 0x0a655a73],
                            [0x75e98d11, 0x83fab601, 0xe836a6d2, 0xb2a7c7cb],
                            [0x1565b1ce, 0x8ab813c5, 0xb1658413, 0xeebaf068],
                            [0xe2bb47dd, 0xc3c277a7, 0x8c6bd4be, 0xbbca989d],
                            [0xea97912e, 0xcd224d66, 0x76fd31c2, 0x7a854abf],
                            [0x8eebb842, 0x0534b043, 0xf55a3b65, 0x3868b974],
                            [0x82c55a0b, 0x5d18c3c8, 0xbdc40fd1, 0x5d11f2e2],
                            [0xda46bff0, 0xedc1e115, 0x77cb8b8a, 0xf5fd6ddf],
                            [0x7c9c19e9, 0x4188e742, 0xd8c2cbaf, 0x0ecd1aaa],
                            [0x33145d5b, 0x0c8e2f61, 0x2ab4b213, 0xf1cf9ebe],
                            [0x64f561e1, 0xae2aa75b, 0xe4d56a57, 0xdcc54d10],
                            [0x25545f02, 0x8c271a6a, 0x6b676110, 0xa348628b],
                            [0x8c33d2d7, 0x7dfc25a0, 0xbdb68979, 0xac7ce8f1],
                            [0x8adc7126, 0x71c4d390, 0x4b47aeed, 0x444dbd2b],
                            [0xaab41fbc, 0x2c6de4d8, 0x83b61f31, 0xc68879b1],
                            [0x90961b3a, 0x9a1d4b51, 0xbf34d9d1, 0xe90137a7],
                            [0x2e988439, 0x2d7e67ec, 0x9bfd6405, 0xd851ee32],
                            [0x3b2b45e8, 0x76225018, 0xa2632614, 0x06234ca9],
                            [0xd52dc6af, 0x36c0d185, 0x90f4a9d2, 0x0b024990],
                            [0x5ed3ebb2, 0xb19b49a2, 0xd691c27a, 0x7d52941c],
                            [0xcb22479a, 0x3713b23d, 0x24abe31b, 0x45877b4e],
                            [0x143aa9f1, 0xe7709011, 0xa6ef993b, 0x8e4f33e2],
                            [0xc8b1ad16, 0x820b4f85, 0xe9d1482a, 0x806af90c],
                            [0x4f1eda2b, 0xa73eac8c, 0x8004e66b, 0xa0804e11],
                            [0x668ceb7b, 0xad020ed3, 0x90687534, 0x96eded7e],
                            [0x8bbd77d0, 0x29fc1ae4, 0xc41fb7cf, 0x31b08f08],
                            [0x02d07857, 0x4ed79f55, 0x89cfe082, 0x97167616],
                            [0xd99dbf57, 0x87ef7406, 0x59c9abdf, 0xf664be32],
                            [0x6d2bd834, 0x6b61d6e5, 0x3fd282ce, 0xbc877248],
                            [0xb4dbab3f, 0x7006dded, 0x39608927, 0x9773b83e],
                            [0x6e1f21e8, 0xff1a76c0, 0x197dd40b, 0x3cedee3d],
                            [0x551f64b8, 0x98161c0f, 0xb93103a7, 0xac12366e],
                            [0x6854311c, 0x6bc09f24, 0x226ec3e9, 0x91507815],
                            [0xb106d988, 0x6c9cda70, 0xebf972ce, 0x97b1efa8],
                            [0x91b8c27b, 0x257ff731, 0x1589d60d, 0xcb2b3de9],
                            [0x819d3677, 0xa6afca81, 0x3d2b4ba9, 0xba8cc70f],
                            [0xfb2c67fa, 0x75af7785, 0xde6133b0, 0x1bb59f2c],
                            [0xceb72cd4, 0x8c581ccb, 0x803233f2, 0xb88a174b],
                            [0xe2811f64, 0xa488d42c, 0x3a62a574, 0x918df881],
                            [0x95a66cf8, 0x0529a646, 0x3bc15b00, 0xfb0eedc9],
                            [0x1e90f3ce, 0xa8f81b7f, 0x13d5445c, 0xc969a914],
                            [0x25287783, 0x19869a3d, 0x93672c2a, 0x07a57f5e],
                            [0x44dfacc0, 0x528ae192, 0x141721a9, 0x85b2a3d9],
                            [0x45f13c20, 0x90b07bb5, 0x9a155a2f, 0x3933fb77],
                            [0x40531c62, 0x437c14b6, 0xc03c8d39, 0x35f5ae87],
                            [0x32e58909, 0xdb068147, 0xa967c6cf, 0x33d9416a],
                            [0xa8c44523, 0x12a89332, 0xddd09ccf, 0x7b04c0d2],
                            [0xae00b8b4, 0x589efb18, 0xf8c1dec1, 0xfd9d867f],
                            [0xe2fc1828, 0x5f7a3b28, 0x72de1c32, 0xc01e6ea6],
                            [0x8be4c525, 0xcde70d98, 0xd7f5f5a1, 0xf39faadf],
                            [0x9c744944, 0x00a491f4, 0x665707a9, 0xa93fad9d],
                            [0x036f2525, 0x827ddd36, 0x989c99db, 0x6b9455fc],
                            [0x52c6b5d2, 0xfd4e23f9, 0xb620b0da, 0x462d7566],
                            [0x00ba54e6, 0xfe9021ae, 0xa8c65c37, 0xda979c78],
                            [0x6e11dca0, 0x41887539, 0x2b9781af, 0x94e6b930],
                            [0x086be7db, 0xcce50438, 0x2d94bc75, 0x00f4ebfe],
                            [0xd2216f7c, 0x33321d48, 0x8ce0f144, 0x34c1fd4f],
                            [0x9083c0fc, 0x6cb46451, 0xd8202b2f, 0xae5db326],
                            [0xe894be71, 0x0e1b6c67, 0x70d5e81f, 0x2075d5ff],
                            [0x7f9f10f7, 0x0a8df81d, 0x406cb9cc, 0x524879b5],
                            [0x4c329904, 0x6f939b2c, 0xd49f8bba, 0xad54aee5],
                            [0x92d68b43, 0x2c631f71, 0x98329455, 0x91971f7f],
                            [0x1b06289e, 0x4879af78, 0xc66d7c82, 0xda19e863],
                            [0x399741da, 0xd362b007, 0x8f94e50c, 0xcdd23209],
                            [0xa2063194, 0xc67f368a, 0x83462433, 0xe719c2db],
                            [0x9da0bfd1, 0x8977f90e, 0x7f2f4172, 0xab5213ff],
                            [0x82823c8f, 0xd7cdd62e, 0xac3a59ba, 0x17ef0fb9],
                            [0xb3e51375, 0xfebb3ad3, 0x54ffdda4, 0xfbf44cf9],
                            [0x261f8183, 0x8328012b, 0x47d7e527, 0xc19cca15],
                            [0xef25bfc7, 0x79460e21, 0xeb65f2ef, 0x63731f9f],
                            [0xd14fc8f5, 0x209123ea, 0xf5f1cf21, 0x93f84381],
                            [0xf5375f67, 0x0846d9a1, 0x07aaf802, 0xe753fa08],
                            [0x65a56edd, 0x4ea28e39, 0x10d17a7b, 0xa9445d87],
                            [0xbeb8eb53, 0xd01b9c94, 0xd1d15e41, 0x639307cc],
                            [0xc2272546, 0xd1701452, 0x2fe76cbd, 0xe1dd7697],
                            [0xff6c09f6, 0x1d7804c2, 0x1ca3a2f8, 0x3fffb67b],
                            [0xfe48f3fd, 0x0046ec25, 0x1ed4401a, 0xd67f4ed6],
                            [0x46c3c72f, 0x8398b8c1, 0xe26aff08, 0x0dd1ecbb],
                            [0xc7f49f94, 0xca32d87c, 0x2e063788, 0x0229c6c0],
                            [0xa9ec4820, 0x576c7877, 0xc21211c3, 0x502af3d3]]));
    }
    #[test]
    fn test_indeo5_scalable() {
        let mut dmx_reg = RegisteredDemuxers::new();
        crate::indeo_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        indeo_register_all_decoders(&mut dec_reg);

        // sample is a trailer for Heart of Darkness game
        test_decoding("ivf", "indeo5s", "assets/Indeo/TRAILERIIE.IVF", Some(31),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0xd6c5b653, 0x082e25e6, 0x90c0c112, 0xfa7f46c8],
                            [0x75e96cdb, 0x198cdb95, 0xde228898, 0x3cf68cfc],
                            [0xb1bed823, 0xad632d3e, 0x2e509143, 0x5e59e911],
                            [0xe3e7e2f0, 0xb8c7d968, 0xcb0b9273, 0xb7e48207],
                            [0xc6ef6975, 0xe0cc0b05, 0x983f7888, 0x64c9b89f],
                            [0xda936fad, 0xa170c0f2, 0x889a3e0c, 0xfef1a626],
                            [0xa85ccb32, 0x2c6847c4, 0x3259f48d, 0xe7e1b47b],
                            [0xedbb8256, 0x1e889428, 0xbc268e66, 0xf41d15f1],
                            [0xd6869899, 0x5121410d, 0xb12dfc96, 0x7dc67a24],
                            [0x9a594172, 0x3d9269c7, 0x1a7277fd, 0xe597dd01],
                            [0x3af07da6, 0x1968872d, 0xf7fc190e, 0x5c61c184],
                            [0xd8eec91e, 0x1aa55dfe, 0x9703a2ce, 0x1ce30990],
                            [0xea4821ae, 0x44ab9f2f, 0xa882bccb, 0xcae50f58],
                            [0x6ddfb989, 0x1affb8ad, 0x7bb2d74a, 0xc28e1a1d],
                            [0x4c9aa98d, 0xb6b2ddd2, 0xfb533baf, 0xc2d90242],
                            [0x332c8e68, 0x47a942ea, 0x6ced7891, 0x7667ad97],
                            [0x940ad339, 0x448ea27c, 0x3b7d0328, 0x4a4cf19f],
                            [0x08a60746, 0x399949ef, 0xce81ef06, 0xbc1d7d6b],
                            [0x4b5e51d0, 0xe26d32f1, 0xb1872663, 0xa70c6e65],
                            [0x428fb122, 0xf3a55f40, 0xdc4316d7, 0xe2765f76],
                            [0xcce4fa35, 0xb47d9848, 0xcbe7fef4, 0x5285022b],
                            [0xde30af92, 0x28a04fe2, 0x317f6be8, 0xde5c161c],
                            [0xe1f00bf7, 0xab2d4e91, 0x9eb674e6, 0x3b863314],
                            [0xac944130, 0xa5d1171a, 0xe8a0b591, 0x09d7652d],
                            [0x17c17612, 0x7cd40f67, 0x7aec3009, 0x2405b862],
                            [0x1d88eb87, 0x44496fb8, 0x58665011, 0xc545f745],
                            [0x04c32cce, 0x38eca98f, 0xd6227880, 0xc7d0f2bf],
                            [0x76d9dcb8, 0x92a35e1a, 0x2b968e96, 0x2c362e4a],
                            [0xda4904e7, 0x2d9d0a74, 0x63932049, 0x7bf9f0de],
                            [0x4f18931c, 0x61b9046f, 0xf5eac763, 0x0c1826d5],
                            [0x823d8746, 0x17debe43, 0xe256fda4, 0xdd1a6832],
                            [0x0bb4b91d, 0xf66f1c19, 0x166ee91a, 0x69379e27]]));
    }
}
