use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

mod cell;
use cell::*;
mod mv;
use mv::*;
mod ratectl;
use ratectl::*;
mod tree;
pub use tree::{Indeo3Cell, Plane};
use tree::Indeo3PrimaryTree;

const OS_HEADER_SIZE: usize = 16;
const BITSTREAM_HEADER_SIZE: usize = 48;
const HDR_FIELD_2: u32 = 0;
const FRMH_TAG: u32 = ((b'F' as u32) << 24) | ((b'R' as u32) << 16)
                     | ((b'M' as u32) << 8) | (b'H' as u32);
const PLANE_OFFSETS: usize = 32;

const CB_SELECTORS: [u8; 16] = [
    0x02, 0x14, 0x26, 0x38, 0x4A, 0x5C, 0x6E, 0x7F,
    0x82, 0x94, 0xA6, 0xB8, 0xCA, 0xDC, 0xEE, 0xFF
];

const PLANE_ORDER: [usize; 3] = [1, 2, 0];

pub struct Indeo3Writer<'a> {
    dst:    &'a mut Vec<u8>,
    bitbuf: u8,
    bits:   u8,
    bitpos: Option<usize>,
}

impl<'a> Indeo3Writer<'a> {
    fn new(dst: &'a mut Vec<u8>) -> Self {
        Self {
            dst,
            bitbuf: 0,
            bits:   0,
            bitpos: None,
        }
    }
    pub fn put_byte(&mut self, b: u8) {
        self.dst.push(b);
    }
    pub fn put_2bits(&mut self, val: u8) {
        if self.bits == 0 {
            self.bitpos = Some(self.dst.len());
            self.dst.push(0);
        }
        self.bitbuf |= val << (6 - self.bits);
        self.bits += 2;
        if self.bits == 8 {
            let bpos = self.bitpos.unwrap_or(0);
            self.dst[bpos] = self.bitbuf;
            self.bitbuf = 0;
            self.bits   = 0;
            self.bitpos = None;
        }
    }
}

impl<'a> Drop for Indeo3Writer<'a> {
    fn drop(&mut self) {
        if self.bits != 0 {
            let bpos = self.bitpos.unwrap_or(0);
            self.dst[bpos] = self.bitbuf;
        }
    }
}

#[derive(Default)]
struct Indeo3Frame {
    plane:  [Plane; 3],
}

impl Indeo3Frame {
    fn new() -> Self { Self::default() }
    fn alloc(&mut self, width: usize, height: usize) {
        self.plane[0].alloc(width,     height,     40);
        self.plane[1].alloc(width / 4, height / 4, 10);
        self.plane[2].alloc(width / 4, height / 4, 10);
    }
    fn fill(&mut self, vbuf: &NAVideoBufferRef<u8>) {
        let data = vbuf.get_data();
        for (plane_no, plane) in self.plane.iter_mut().enumerate() {
            plane.fill(&data[vbuf.get_offset(plane_no)..], vbuf.get_stride(plane_no));
        }
    }
    fn clear_mvs(&mut self) {
        for plane in self.plane.iter_mut() {
            plane.clear_mvs();
        }
    }
}

struct Indeo3Encoder {
    stream:     Option<NAStreamRef>,
    pkt:        Option<NAPacket>,
    cframe:     Indeo3Frame,
    pframe:     Indeo3Frame,
    cenc:       CellEncoder,
    mv_est:     MotionEstimator,
    rc:         RateControl,
    frameno:    u32,
    buf_sel:    bool,
    width:      usize,
    height:     usize,

    debug_tree: bool,
    debug_frm:  bool,
    try_again:  bool,
}

impl Indeo3Encoder {
    fn new() -> Self {
        Self {
            stream:     None,
            pkt:        None,
            cframe:     Indeo3Frame::new(),
            pframe:     Indeo3Frame::new(),
            cenc:       CellEncoder::new(),
            mv_est:     MotionEstimator::new(),
            rc:         RateControl::new(),
            frameno:    0,
            buf_sel:    false,
            width:      0,
            height:     0,

            debug_tree: false,
            debug_frm:  false,
            try_again:  false,
        }
    }
    fn encode_planes(&mut self, dbuf: &mut Vec<u8>, trees: &[Box<Indeo3PrimaryTree>], is_intra: bool) -> EncoderResult<()> {
        for (&planeno, tree) in PLANE_ORDER.iter().zip(trees.iter()) {
            let offset = dbuf.len();
            let ref_plane = &self.pframe.plane[planeno];

            let mut mc_count = [0; 4];
            let mvs = &self.cframe.plane[planeno].mvs;
            write_u32le(&mut mc_count, mvs.len() as u32)?;
            dbuf.extend_from_slice(&mc_count);
            for &(mv, _) in mvs.iter() {
                dbuf.push(mv.y as u8);
                dbuf.push(mv.x as u8);
            }

            let mut iw = Indeo3Writer::new(dbuf);
            self.cframe.plane[planeno].encode_tree(&mut iw, tree, &mut self.cenc, ref_plane);
            drop(iw);
            while (dbuf.len() & 3) != 0 {
                dbuf.push(0);
            }

            let plane_off = PLANE_OFFSETS + 4 * if planeno > 0 { planeno ^ 3 } else { 0 };
            write_u32le(&mut dbuf[plane_off..], (offset - OS_HEADER_SIZE) as u32)?;
        }

        let mut checksum = 0;
        for plane in self.cframe.plane.iter() {
            checksum ^= plane.checksum();
        }
        write_u16le(&mut dbuf[26..], checksum * 2)?;

        let size = (dbuf.len() - OS_HEADER_SIZE) as u32;
        write_u32le(&mut dbuf[8..], self.frameno ^ HDR_FIELD_2 ^ FRMH_TAG ^ size)?;
        write_u32le(&mut dbuf[12..], size)?;
        write_u32le(&mut dbuf[20..], size * 8)?;

        if is_intra {
            dbuf.extend_from_slice(b"\x0d\x0aVer 3.99.00.00\x0d\x0a\x00");
            while (dbuf.len() & 3) != 0 {
                dbuf.push(0);
            }
        }

        Ok(())
    }
}

impl NAEncoder for Indeo3Encoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                        format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, true, YUV410_FORMAT)),
                        ..Default::default()
                    })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let pix_fmt = YUV410_FORMAT;
                let outinfo = NAVideoInfo::new((vinfo.width + 15) & !15, (vinfo.height + 15) & !15, false, pix_fmt);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_SKIPFRAME }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                if vinfo.format != YUV410_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if ((vinfo.width | vinfo.height) & 15) != 0 {
                    return Err(EncoderError::FormatError);
                }
                if (vinfo.width > 640) || (vinfo.height > 480) {
                    return Err(EncoderError::FormatError);
                }

                self.width  = vinfo.width;
                self.height = vinfo.height;

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, false, vinfo.format);
                let info = NACodecInfo::new("indeo3", NACodecTypeInfo::Video(out_info), None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());

                self.cframe.alloc(vinfo.width, vinfo.height);
                self.pframe.alloc(vinfo.width, vinfo.height);

                self.rc.set_bitrate(encinfo.bitrate, encinfo.tb_num, encinfo.tb_den);
                self.rc.set_quality(encinfo.quality);

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        if self.debug_tree || self.debug_frm {
            println!("frame {}:", self.frameno);
        }

        let mut skip_frame = frm.get_frame_type() == FrameType::Skip;
        if let NABufferType::None = buf {
            skip_frame = true;
        }
        if skip_frame {
            let mut dbuf = Vec::with_capacity(16);
            let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
            let mut bw   = ByteWriter::new(&mut gw);

            // OS header
            bw.write_u32le(self.frameno)?;
            bw.write_u32le(HDR_FIELD_2)?;
            bw.write_u32le(0)?; // check
            bw.write_u32le(0)?; // size

            // bitstream header
            bw.write_u16le(32)?; // version
            bw.write_u16le(0)?;
            bw.write_u32le(0)?; // data size in bits
            bw.write_byte(0)?; // cb offset
            bw.write_byte(14)?; // reserved
            bw.write_u16le(0)?; // checksum
            bw.write_u16le(self.height as u16)?;
            bw.write_u16le(self.width as u16)?;

            let size = (dbuf.len() - OS_HEADER_SIZE) as u32;
            write_u32le(&mut dbuf[8..], self.frameno ^ HDR_FIELD_2 ^ FRMH_TAG ^ size)?;
            write_u32le(&mut dbuf[12..], size)?;
            write_u32le(&mut dbuf[20..], size * 8)?;

            let fsize = dbuf.len() as u32;
            self.rc.advance(fsize);
            self.frameno += 1;

            self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, false, dbuf));
            return Ok(());
        }

        if let Some(ref vbuf) = buf.get_vbuf() {
            let mut dbuf = Vec::with_capacity(16);
            let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
            let mut bw   = ByteWriter::new(&mut gw);

            let (width, height) = vbuf.get_dimensions(0);
            if width != self.width || height != self.height {
                self.width  = width;
                self.height = height;
                self.cframe.alloc(width, height);
                self.pframe.alloc(width, height);
                self.rc.reset();
            }

            let (is_intra, quant) = self.rc.get_quant(self.frameno);
            self.cenc.quant = quant;

            if is_intra {
                self.buf_sel = false;
            } else {
                self.buf_sel = !self.buf_sel;
            }

            self.cframe.fill(vbuf);
            self.cframe.clear_mvs();

            // OS header
            bw.write_u32le(self.frameno)?;
            bw.write_u32le(HDR_FIELD_2)?;
            bw.write_u32le(0)?; // check
            bw.write_u32le(0)?; // size

            // bitstream header
            bw.write_u16le(32)?; // version
            let mut flags = 0;
            if is_intra {
                flags |= 0x5;
            } else {
                flags |= 1;
                if self.buf_sel {
                    flags |= 1 << 9;
                }
            }
            bw.write_u16le(flags)?;
            bw.write_u32le(0)?; // data size in bits
            bw.write_byte(0)?; // cb offset
            bw.write_byte(14)?; // reserved
            bw.write_u16le(0)?; // checksum
            bw.write_u16le(height as u16)?;
            bw.write_u16le(width as u16)?;
            for _ in 0..3 {
                bw.write_u32le(0)?; // plane data offset
            }
            bw.write_u32le(0)?; // reserved
            bw.write_buf(&CB_SELECTORS)?;

            let mut trees = Vec::with_capacity(PLANE_ORDER.len());

            // prepare plane data structure
            for &planeno in PLANE_ORDER.iter() {
                let ref_plane = &self.pframe.plane[planeno];
                let mut tree = self.cframe.plane[planeno].find_cells(is_intra, ref_plane, &self.mv_est);
                if self.debug_tree {
                    println!(" tree for plane {}:", planeno);
                    tree.print();
                }
                let mvs = &mut self.cframe.plane[planeno].mvs;
                if mvs.len() > 256 {
                    compact_mvs(mvs);
                    self.cframe.plane[planeno].prune_extra_mvs(&mut tree);
                }
                trees.push(tree);
            }

            self.encode_planes(&mut dbuf, &trees, is_intra)?;

            let cur_quant = self.cenc.quant.unwrap_or(42);
            if self.try_again && !is_intra && cur_quant < 8 {
                let expected_size = self.rc.get_expected_size();
                if expected_size > 0 {
                    let cur_size = dbuf.len() as u32;
                    // try re-encoding frame if possible
                    if cur_size > expected_size * 3 / 2 {
                        self.cframe.fill(vbuf);
                        let new_quant = if cur_quant < 7 {
                                cur_quant + 1
                            } else {
                                cur_quant - 1
                            };
                        self.cenc.quant = Some(new_quant);
                        dbuf.truncate(OS_HEADER_SIZE + BITSTREAM_HEADER_SIZE);
                        self.encode_planes(&mut dbuf, &trees, is_intra)?;
                    }
                }
            }

            if self.debug_frm {
                for plane in self.cframe.plane.iter() {
                    for (y, line) in plane.data.chunks(plane.width).enumerate() {
                        print!(" {:3}:", y);
                        for &el in line.iter() { print!(" {:02X}", el); }
                        println!();
                    }
                    println!();
                }
            }

            std::mem::swap(&mut self.cframe, &mut self.pframe);
            self.frameno += 1;

            let fsize = dbuf.len() as u32;
            self.rc.advance(fsize);

            self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, is_intra, dbuf));
            Ok(())
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        let mut npkt = None;
        std::mem::swap(&mut self.pkt, &mut npkt);
        Ok(npkt)
    }
    fn flush(&mut self) -> EncoderResult<()> {
        Ok(())
    }
}

const DEBUG_TREE_OPTION: &str = "debug_tree";
const DEBUG_FRAME_OPTION: &str = "debug_frame";
const MV_RANGE_OPTION: &str = "mv_range";
const MV_FLAT_OPTION: &str = "mv_flat_threshold";
const MV_THRESHOLD_OPTION: &str = "mv_threshold";
const CELL_I_THRESHOLD_OPTION: &str = "cell_i_threshold";
const CELL_P_THRESHOLD_OPTION: &str = "cell_p_threshold";
const DO_RLE_OPTION: &str = "rle";
const TRY_AGAIN_OPTION: &str = "try_recompress";

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: KEYFRAME_OPTION, description: KEYFRAME_OPTION_DESC,
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
    NAOptionDefinition {
        name: DEBUG_TREE_OPTION, description: "Print frame trees",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: DEBUG_FRAME_OPTION, description: "Print encoder-reconstructed frames",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: MV_RANGE_OPTION, description: "Motion search range",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(120)) },
    NAOptionDefinition {
        name: MV_FLAT_OPTION, description: "Threshold for coding cell as skipped one",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(1000)) },
    NAOptionDefinition {
        name: MV_THRESHOLD_OPTION, description: "Threshold for coding cell as inter",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(1000)) },
    NAOptionDefinition {
        name: CELL_I_THRESHOLD_OPTION, description: "Threshold for coding intra block as flat",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
    NAOptionDefinition {
        name: CELL_P_THRESHOLD_OPTION, description: "Threshold for coding inter cell in coarser mode",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
    NAOptionDefinition {
        name: DO_RLE_OPTION, description: "Perform zero run length compation",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: TRY_AGAIN_OPTION, description: "Try compressing the frame again for the better bitrate fit",
        opt_type: NAOptionDefinitionType::Bool },
];

impl NAOptionHandler for Indeo3Encoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        KEYFRAME_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.rc.set_key_int(val as u32);
                            }
                        },
                        DEBUG_TREE_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.debug_tree = val;
                            }
                        },
                        DEBUG_FRAME_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.debug_frm = val;
                            }
                        },
                        MV_RANGE_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.mv_est.mv_range = val as i8;
                            }
                        },
                        MV_FLAT_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.mv_est.flat_thr = val as u16;
                            }
                        },
                        MV_THRESHOLD_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.mv_est.mv_thr = val as u16;
                            }
                        },
                        CELL_I_THRESHOLD_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.cenc.flat_thr_i = val as u32;
                            }
                        },
                        CELL_P_THRESHOLD_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.cenc.flat_thr_p = val as u32;
                            }
                        },
                        DO_RLE_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.cenc.do_rle = val;
                            }
                        },
                        TRY_AGAIN_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.try_again = val;
                            }
                        },
                        _ => {},
                    };
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            KEYFRAME_OPTION => Some(NAValue::Int(i64::from(self.rc.get_key_int()))),
            DEBUG_TREE_OPTION => Some(NAValue::Bool(self.debug_tree)),
            DEBUG_FRAME_OPTION => Some(NAValue::Bool(self.debug_frm)),
            MV_RANGE_OPTION => Some(NAValue::Int(i64::from(self.mv_est.mv_range))),
            MV_FLAT_OPTION => Some(NAValue::Int(i64::from(self.mv_est.flat_thr))),
            MV_THRESHOLD_OPTION => Some(NAValue::Int(i64::from(self.mv_est.mv_thr))),
            CELL_I_THRESHOLD_OPTION => Some(NAValue::Int(i64::from(self.cenc.flat_thr_i))),
            CELL_P_THRESHOLD_OPTION => Some(NAValue::Int(i64::from(self.cenc.flat_thr_p))),
            DO_RLE_OPTION => Some(NAValue::Bool(self.cenc.do_rle)),
            TRY_AGAIN_OPTION => Some(NAValue::Bool(self.try_again)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(Indeo3Encoder::new())
}

#[cfg(test)]
mod test {
    use crate::*;
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use nihav_commonfmt::*;
    use nihav_codec_support::test::enc_video::*;

    #[allow(unused_variables)]
    fn encode_test(name: &'static str, enc_options: &[NAOption], limit: Option<u64>, hash: &[u32; 4]) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        indeo_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        indeo_register_all_encoders(&mut enc_reg);

        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Indeo/laser05.avi",
                stream_type:    StreamType::Video,
                limit,
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "indeo3",
                out_name:       name,
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  YUV410_FORMAT,
                flipped: false,
                bits:    9,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 25000 * 8,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options, hash);
    }
    #[test]
    fn test_indeo3_encoder1() {
        let enc_options = &[
                NAOption { name: super::TRY_AGAIN_OPTION, value: NAValue::Bool(true) },
            ];
        encode_test("indeo3.avi", enc_options, Some(4), &[0x17d742bc, 0x6f4c1200, 0x79422bac, 0xc46b5dd0]);
    }
    /*#[test]
    fn test_indeo3_roundtrip() {
        const YPATTERN: [u8; 16] = [32, 72, 40, 106, 80, 20, 33, 58, 77, 140, 121, 100, 83, 57, 30, 11];
        const CPATTERN: [u8; 4] = [0x80; 4];

        let dst_vinfo = NAVideoInfo {
                width:   16,
                height:  16,
                format:  YUV410_FORMAT,
                flipped: false,
                bits:    9,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };

        let mut ienc = super::get_encoder();
        ienc.init(0, enc_params).unwrap();
        let mut buffer = alloc_video_buffer(dst_vinfo, 2).unwrap();
        if let NABufferType::Video(ref mut buf) = buffer {
            let vbuf = NASimpleVideoFrame::from_video_buf(buf).unwrap();
            for i in 0..16 {
                vbuf.data[vbuf.offset[0] + i * vbuf.stride[0]..][..16].copy_from_slice(&YPATTERN);
            }
            for plane in 1..3 {
                for i in 0..4 {
                    vbuf.data[vbuf.offset[plane] + i * vbuf.stride[plane]..][..4].copy_from_slice(&CPATTERN);
                }
            }
        }
        let info = NACodecInfo::new("indeo3", NACodecTypeInfo::Video(dst_vinfo), None).into_ref();
        let frm = NAFrame::new(NATimeInfo::new(Some(0), None, None, 1, 12), FrameType::I, true, info.clone(), buffer);
        //ienc.set_options(&[NAOption{ name: super::DEBUG_FRAME_OPTION, value: NAValue::Bool(true) }]);
        ienc.encode(&frm).unwrap();
        let pkt = ienc.get_packet().unwrap().unwrap();
        println!(" pkt size {}", pkt.get_buffer().len());

        let mut dec_reg = RegisteredDecoders::new();
        indeo_register_all_decoders(&mut dec_reg);
        let decfunc = dec_reg.find_decoder("indeo3").unwrap();
        let mut dec = (decfunc)();
        let mut dsupp = Box::new(NADecoderSupport::new());
        dec.init(&mut dsupp, info).unwrap();
        dec.set_options(&[NAOption{ name: "checksum", value: NAValue::Bool(true) }]);
        let dst = dec.decode(&mut dsupp, &pkt).unwrap();
        if let NABufferType::Video(ref vbuf) = dst.get_buffer() {
            for plane in 0..3 {
                let size = if plane == 0 { 16 } else { 4 };
                let start = vbuf.get_offset(plane);
                for line in vbuf.get_data()[start..].chunks(vbuf.get_stride(plane)).take(size) {
                    print!("   ");
                    for &el in line[..size].iter() {
                        print!(" {:02X}", el >> 1);
                    }
                    println!();
                }
                if plane == 0 {
                    print!("ref");
                    for &el in YPATTERN.iter() { print!(" {:02X}", el >> 1); } println!();
                }
                println!();
            }
        }
        panic!("end");
    }*/
}
