use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::io::intcode::*;
use nihav_codec_support::codecs::{MV, ZERO_MV};
use nihav_codec_support::codecs::blockdsp::*;

use super::svq1data::*;

#[derive(Clone,Copy,Debug,PartialEq)]
enum SVQ1FrameType {
    I,
    P,
    Drop,
}

#[derive(Clone,Copy,Debug,PartialEq)]
enum BlockType {
    Intra,
    Skip,
    OneMV,
    FourMV,
}

#[derive(Clone,Copy,Debug,PartialEq)]
#[allow(clippy::enum_variant_names)]
enum BlockDiv {
    Div16x16,
    Div16x8,
    Div8x8,
    Div8x4,
    Div4x4,
    Div4x2,
}

impl BlockDiv {
    fn is_final(self) -> bool { self == BlockDiv::Div4x2 }
    fn split(self) -> (Self, usize, usize) {
        match self {
            BlockDiv::Div16x16 => (BlockDiv::Div16x8, 0, 8),
            BlockDiv::Div16x8  => (BlockDiv::Div8x8, 8, 0),
            BlockDiv::Div8x8   => (BlockDiv::Div8x4, 0, 4),
            BlockDiv::Div8x4   => (BlockDiv::Div4x4, 4, 0),
            BlockDiv::Div4x4   => (BlockDiv::Div4x2, 0, 2),
            BlockDiv::Div4x2   => unreachable!(),
        }
    }
    fn get_size(self) -> (usize, usize) {
        match self {
            BlockDiv::Div16x16 => (16, 16),
            BlockDiv::Div16x8  => (16,  8),
            BlockDiv::Div8x8   => ( 8,  8),
            BlockDiv::Div8x4   => ( 8,  4),
            BlockDiv::Div4x4   => ( 4,  4),
            BlockDiv::Div4x2   => ( 4,  2),
        }
    }
    fn get_level(self) -> usize {
        match self {
            BlockDiv::Div16x16 => 5,
            BlockDiv::Div16x8  => 4,
            BlockDiv::Div8x8   => 3,
            BlockDiv::Div8x4   => 2,
            BlockDiv::Div4x4   => 1,
            BlockDiv::Div4x2   => 0,
        }
    }
}

const BLOCK_TYPES: [BlockType; 4] = [ BlockType::Skip, BlockType::OneMV, BlockType::FourMV, BlockType::Intra ];

impl SVQ1FrameType {
    fn is_ref(self) -> bool {
        self != SVQ1FrameType::Drop
    }
    fn is_intra(self) -> bool {
        self == SVQ1FrameType::I
    }
    fn to_frame_type(self) -> FrameType {
        match self {
            SVQ1FrameType::I => FrameType::I,
            SVQ1FrameType::P => FrameType::P,
            SVQ1FrameType::Drop => FrameType::P,
        }
    }
    fn from_id(id: u32) -> DecoderResult<Self> {
        match id {
            0 => Ok(SVQ1FrameType::I),
            1 => Ok(SVQ1FrameType::P),
            2 => Ok(SVQ1FrameType::Drop),
            _ => Err(DecoderError::InvalidData),
        }
    }
}

struct SVQ1DescReader {
    table:  &'static [[u8; 2]],
    bias:   i16,
}

impl CodebookDescReader<i16> for SVQ1DescReader {
    fn bits(&mut self, idx: usize) -> u8 { self.table[idx][1] }
    fn code(&mut self, idx: usize) -> u32 { u32::from(self.table[idx][0]) }
    fn sym(&mut self, idx: usize) -> i16 { (idx as i16) + self.bias }
    fn len(&mut self) -> usize { self.table.len() }
}

struct SVQ1InterMeanDescReader {}

impl CodebookDescReader<i16> for SVQ1InterMeanDescReader {
    fn bits(&mut self, idx: usize) -> u8 { SVQ_INTER_MEAN_CODES[idx][1] as u8 }
    fn code(&mut self, idx: usize) -> u32 { u32::from(SVQ_INTER_MEAN_CODES[idx][0]) }
    fn sym(&mut self, idx: usize) -> i16 { (idx as i16) - 256 }
    fn len(&mut self) -> usize { SVQ_INTER_MEAN_CODES.len() }
}

struct SVQ1Decoder {
    info:       NACodecInfoRef,
    width:      usize,
    height:     usize,
    ref_frm:    Option<NAVideoBufferRef<u8>>,
    mvs:        Vec<MV>,
    intra_stages_cb:    Vec<Codebook<i16>>,
    inter_stages_cb:    Vec<Codebook<i16>>,
    intra_mean_cb:      Codebook<i16>,
    inter_mean_cb:      Codebook<i16>,
    mv_cb:              Codebook<i16>,

    div_list:           [(BlockDiv, usize); 64],
}

impl SVQ1Decoder {
    #[allow(clippy::needless_range_loop)]
    fn new() -> Self {
        let mut intra_stages_cb = Vec::with_capacity(6);
        for i in 0..6 {
            let mut cbd = SVQ1DescReader { table: &SVQ_INTRA_STAGE_CODES[i], bias: -1 };
            let cb = Codebook::new(&mut cbd, CodebookMode::MSB).unwrap();
            intra_stages_cb.push(cb);
        }
        let mut inter_stages_cb = Vec::with_capacity(6);
        for i in 0..6 {
            let mut cbd = SVQ1DescReader { table: &SVQ_INTER_STAGE_CODES[i], bias: -1 };
            let cb = Codebook::new(&mut cbd, CodebookMode::MSB).unwrap();
            inter_stages_cb.push(cb);
        }
        let mut cbd = SVQ1DescReader { table: &SVQ_INTRA_MEAN_CODES, bias: 0 };
        let intra_mean_cb = Codebook::new(&mut cbd, CodebookMode::MSB).unwrap();
        let mut cbd = SVQ1InterMeanDescReader {};
        let inter_mean_cb = Codebook::new(&mut cbd, CodebookMode::MSB).unwrap();
        let mut cbd = SVQ1DescReader { table: &SVQ_MV_CODES, bias: 0 };
        let mv_cb = Codebook::new(&mut cbd, CodebookMode::MSB).unwrap();
        Self {
            info:       NACodecInfoRef::default(),
            width:      0,
            height:     0,
            ref_frm:    None,
            mvs:        Vec::new(),
            intra_stages_cb, inter_stages_cb, intra_mean_cb, inter_mean_cb, mv_cb,
            div_list:   [(BlockDiv::Div16x16, 0); 64],
        }
    }
    fn read_mv(&self, br: &mut BitReader) -> DecoderResult<MV> {
        let mut x                       = br.read_cb(&self.mv_cb)?;
        if x > 0 && br.read_bool()? {
            x = -x;
        }
        let mut y                       = br.read_cb(&self.mv_cb)?;
        if y > 0 && br.read_bool()? {
            y = -y;
        }
        Ok(MV { x, y })
    }
    #[allow(clippy::too_many_arguments)]
    fn pred_mv(&self, x: usize, y: usize, w: usize, mv_idx: usize, mvstride: usize, blk_idx: usize, diff: MV) -> MV {
        let a_mv = if x > 0 || (blk_idx & 1) != 0 { self.mvs[mv_idx - 1] } else { ZERO_MV };
        let b_mv = if y > 0 || (blk_idx & 2) != 0 { self.mvs[mv_idx - mvstride]  } else { a_mv };
        let c_mv = match blk_idx {
                0 => if y > 0 && x + 16 < w { self.mvs[mv_idx + 2 - mvstride] } else { ZERO_MV },
                1 => if y > 0 && x + 16 < w { self.mvs[mv_idx + 1 - mvstride] } else { ZERO_MV },
                2 => self.mvs[mv_idx + 1 - mvstride],
                _ => self.mvs[mv_idx - 1 - mvstride],
            };
        let mut new_mv = diff + MV::pred(a_mv, b_mv, c_mv);
        if      new_mv.x >=  32 { new_mv.x -= 64; }
        else if new_mv.x <= -32 { new_mv.x += 64; }
        if      new_mv.y >=  32 { new_mv.y -= 64; }
        else if new_mv.y <= -32 { new_mv.y += 64; }
        new_mv
    }
    fn decode_intra_block(&mut self, br: &mut BitReader, dst: &mut [u8], dstride: usize) -> DecoderResult<()> {
        self.div_list[0] = (BlockDiv::Div16x16, 0);
        let mut idx = 0;
        let mut end = 1;
        while idx < end {
            let (div, off) = self.div_list[idx];
            if !div.is_final() && br.read_bool()? {
                let (ndiv, xoff, yoff) = div.split();
                self.div_list[end] = (ndiv, off);
                end += 1;
                self.div_list[end] = (ndiv, off + xoff + yoff * dstride);
                end += 1;
            } else {
                let level = div.get_level();
                let stages              = br.read_cb(&self.intra_stages_cb[level])?;
                if level > 3 {
                    validate!(stages <= 0);
                }
                let (w, h) = div.get_size();
                let fill = if stages < 0 { 0 } else { br.read_cb(&self.intra_mean_cb)? } as u8;
                for line in dst[off..].chunks_mut(dstride).take(h) {
                    for el in line.iter_mut().take(w) {
                        *el = fill;
                    }
                }
                if stages > 0 {
                    for stage in 0..(stages as usize) {
                        let idx         = br.read(4)? as usize;
                        let src: &[i8] = match div {
                                BlockDiv::Div8x8 => &SVQ_INTRA_CB_8X8[stage * 16 + idx],
                                BlockDiv::Div8x4 => &SVQ_INTRA_CB_8X4[stage * 16 + idx],
                                BlockDiv::Div4x4 => &SVQ_INTRA_CB_4X4[stage * 16 + idx],
                                BlockDiv::Div4x2 => &SVQ_INTRA_CB_4X2[stage * 16 + idx],
                                _ => unreachable!(),
                            };
                        for (line, src) in dst[off..].chunks_mut(dstride).zip(src.chunks(w)) {
                            for x in 0..w {
                                line[x] = (i16::from(line[x]) + i16::from(src[x])).max(0).min(255) as u8;
                            }
                        }
                    }
                }
            }
            idx += 1;
        }
        Ok(())
    }
    fn decode_inter_block(&mut self, br: &mut BitReader, dst: &mut [u8], dstride: usize) -> DecoderResult<()> {
        self.div_list[0] = (BlockDiv::Div16x16, 0);
        let mut idx = 0;
        let mut end = 1;
        while idx < end {
            let (div, off) = self.div_list[idx];
            if !div.is_final() && br.read_bool()? {
                let (ndiv, xoff, yoff) = div.split();
                self.div_list[end] = (ndiv, off);
                end += 1;
                self.div_list[end] = (ndiv, off + xoff + yoff * dstride);
                end += 1;
            } else {
                let level = div.get_level();
                let stages              = br.read_cb(&self.inter_stages_cb[level])?;
                if level > 3 {
                    validate!(stages <= 0);
                }
                let (w, h) = div.get_size();
                let fill = if stages < 0 { 0 } else { br.read_cb(&self.inter_mean_cb)? };
                if fill != 0 {
                    for line in dst[off..].chunks_mut(dstride).take(h) {
                        for el in line.iter_mut().take(w) {
                            *el = el.wrapping_add(fill as u8);
                        }
                    }
                }
                if stages > 0 {
                    for stage in 0..(stages as usize) {
                        let idx         = br.read(4)? as usize;
                        let src: &[i8] = match div {
                                BlockDiv::Div8x8 => &SVQ_INTER_CB_8X8[stage * 16 + idx],
                                BlockDiv::Div8x4 => &SVQ_INTER_CB_8X4[stage * 16 + idx],
                                BlockDiv::Div4x4 => &SVQ_INTER_CB_4X4[stage * 16 + idx],
                                BlockDiv::Div4x2 => &SVQ_INTER_CB_4X2[stage * 16 + idx],
                                _ => unreachable!(),
                            };
                        for (line, src) in dst[off..].chunks_mut(dstride).zip(src.chunks(w)) {
                            for x in 0..w {
                                line[x] = line[x].wrapping_add(src[x] as u8);
                            }
                        }
                    }
                }
            }
            idx += 1;
        }
        Ok(())
    }
    fn decode_plane(&mut self, br: &mut BitReader, dframe: &mut NASimpleVideoFrame<u8>, plane: usize, is_intra: bool) -> DecoderResult<()> {
        let (w, h) = if plane == 0 {
                ((self.width + 15) & !15, (self.height + 15) & !15)
            } else {
                ((self.width / 4 + 15) & !15, (self.height / 4 + 15) & !15)
            };
        let mvstride = w / 8;
        self.mvs.clear();
        self.mvs.resize(mvstride * (h / 8), ZERO_MV);
        let mut mv_idx = 0;

        let mut doff = dframe.offset[plane];
        let dstride = dframe.stride[plane];
        for y in (0..h).step_by(16) {
            for x in (0..w).step_by(16) {
                let block_type = if is_intra {
                        BlockType::Intra
                    } else {
                        let idx         = br.read_code(UintCodeType::LimitedZeroes(3))? as usize;
                        BLOCK_TYPES[idx]
                    };
                match block_type {
                    BlockType::Intra => {
                        self.decode_intra_block(br, &mut dframe.data[doff + x..], dstride)?;
                    },
                    BlockType::Skip => {
                        if let Some(ref rfrm) = self.ref_frm {
                            let sstride = rfrm.get_stride(plane);
                            let soff = rfrm.get_offset(plane) + y * sstride;
                            let src = &rfrm.get_data()[soff + x..];
                            let dst = &mut dframe.data[doff + x..];
                            for (dline, sline) in dst.chunks_mut(dstride).zip(src.chunks(sstride)).take(16) {
                                dline[..16].copy_from_slice(&sline[..16]);
                            }
                        }
                    },
                    BlockType::OneMV => {
                        let mv = self.read_mv(br)?;
                        let new_mv = self.pred_mv(x, y, w, mv_idx + x / 8, mvstride, 0, mv);
                        self.mvs[mv_idx + x / 8] = new_mv;
                        self.mvs[mv_idx + x / 8 + 1] = new_mv;
                        self.mvs[mv_idx + mvstride + x / 8] = new_mv;
                        self.mvs[mv_idx + mvstride + x / 8 + 1] = new_mv;
                        if let Some(ref rfrm) = self.ref_frm {
                            let mode = ((new_mv.x & 1) + (new_mv.y & 1) * 2) as usize;
                            copy_block(dframe, rfrm.clone(), plane, x, y, new_mv.x >> 1, new_mv.y >> 1, 16, 16, 0, 1, mode, HALFPEL_INTERP_FUNCS);
                        }
                        self.decode_inter_block(br, &mut dframe.data[doff + x..], dstride)?;
                    },
                    BlockType::FourMV => {
                        for i in 0..4 {
                            let mv = self.read_mv(br)?;
                            let cur_idx = mv_idx + x / 8 + (i & 1) + (i >> 1) * mvstride;
                            let new_mv = self.pred_mv(x, y, w, cur_idx, mvstride, i, mv);
                            self.mvs[cur_idx] = new_mv;
                            if let Some(ref rfrm) = self.ref_frm {
                                let mode = ((new_mv.x & 1) + (new_mv.y & 1) * 2) as usize;
                                copy_block(dframe, rfrm.clone(), plane, x + (i & 1) * 8, y + (i >> 1) * 8, new_mv.x >> 1, new_mv.y >> 1, 8, 8, 0, 1, mode, HALFPEL_INTERP_FUNCS);
                            }
                        }
                        self.decode_inter_block(br, &mut dframe.data[doff + x..], dstride)?;
                    },
                };
            }
            doff += dstride * 16;
            mv_idx += mvstride * 2;
        }
        Ok(())
    }
}

impl NADecoder for SVQ1Decoder {
    fn init(&mut self, supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, false, YUV410_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            supp.pool_u8.set_dec_bufs(2);
            supp.pool_u8.prealloc_video(NAVideoInfo::new(vinfo.get_width(), vinfo.get_height(), false, YUV410_FORMAT), 6)?;
            self.mvs = Vec::with_capacity((self.width + 15) / 4 * ((self.height + 15) / 4));

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    #[allow(clippy::collapsible_if)]
    fn decode(&mut self, supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() >= 2);
        let mut br = BitReader::new(&src, BitReaderMode::BE);

        let fcode                       = br.read(22)?;
        validate!((fcode & 0x60) != 0);
        let _pts                        = br.read(8)?;
        let ptype                       = br.read(2)?;
        let ftype = SVQ1FrameType::from_id(ptype)?;
        let mut frm_data = Vec::new();
        if fcode != 0x20 {
            frm_data.extend_from_slice(&src);
            for i in 0..4 {
                let a = frm_data[i * 4 + 4];
                let b = frm_data[i * 4 + 5];
                let c = frm_data[i * 4 + 6];
                let d = frm_data[i * 4 + 7];
                frm_data[i * 4 + 4] = c ^ frm_data[32 - i * 4];
                frm_data[i * 4 + 5] = d ^ frm_data[33 - i * 4];
                frm_data[i * 4 + 6] = a ^ frm_data[34 - i * 4];
                frm_data[i * 4 + 7] = b ^ frm_data[35 - i * 4];
            }
            br = BitReader::new(&frm_data, BitReaderMode::BE);
            br.skip(32)?;
        }
        if ftype.is_intra() {
            if fcode == 0x50 || fcode == 0x60 {
                let _checksum           = br.read(16)? as u16;
//                let crc = calc_crc(frm_data.as_slice(), 0);
//                validate!(crc == _checksum);
            }
            if fcode == 0x40 || fcode == 0x60 || fcode == 0x70 {
                let str_len             = br.read(8)? as usize;
                for _ in 0..str_len {
                                          br.skip(8)?;
                }
            }
                                          br.skip(2)?;
                                          br.skip(2)?;
                                          br.skip(1)?;
            let size_id                 = br.read(3)? as usize;
            let (w, h) = if size_id < FRAME_SIZES.len() {
                    FRAME_SIZES[size_id]
                } else {
                    let w               = br.read(12)? as usize;
                    let h               = br.read(12)? as usize;
                    validate!(w >= 16 && h >= 16);
                    (w, h)
                };
            if self.width != w || self.height != h {
                self.flush();
                self.width  = w;
                self.height = h;
                let vinfo = NAVideoInfo::new(self.width, self.height, false, YUV410_FORMAT);
                supp.pool_u8.reset();
                supp.pool_u8.prealloc_video(vinfo, 6)?;
                let nmb = ((w + 15) / 4) * ((h + 15) / 4);
                if self.mvs.capacity() < nmb {
                    let add = nmb - self.mvs.capacity();
                    self.mvs.reserve(add);
                }
            }
        } else {
            if self.ref_frm.is_none() {
                return Err(DecoderError::MissingReference);
            }
        }
        if br.read_bool()? {
            let _pkt_crc                = br.read_bool()?;
            let _comp_crc               = br.read_bool()?;
            let marker                  = br.read(2)?;
            validate!(marker == 0);
        }
        if br.read_bool()? {
                                          br.skip(1)?;
                                          br.skip(4)?;
                                          br.skip(1)?;
                                          br.skip(2)?;
            while br.read_bool()? { }
        }

        let ret = supp.pool_u8.get_free();
        if ret.is_none() {
            return Err(DecoderError::AllocError);
        }

        let mut buf = ret.unwrap();
        let mut dframe = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
        self.decode_plane(&mut br, &mut dframe, 0, ftype.is_intra())?;
        self.decode_plane(&mut br, &mut dframe, 1, ftype.is_intra())?;
        self.decode_plane(&mut br, &mut dframe, 2, ftype.is_intra())?;

        if ftype.is_ref() {
            self.ref_frm = Some(buf.clone());
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(ftype.is_intra());
        frm.set_frame_type(ftype.to_frame_type());
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.ref_frm = None;
    }
}

impl NAOptionHandler for SVQ1Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(SVQ1Decoder::new())
}


#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_svq1() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/ima-adpcm/adpcm-bug.mov
        test_decoding("mov", "sorenson-video", "assets/QT/adpcm-bug.mov", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x90c5eb74, 0xcb942d7d, 0x84c5e444, 0x7f1ba2c2],
                            [0x650ae6f7, 0x9a0a6ec2, 0x0d907064, 0xb4c37321],
                            [0xa04e865b, 0xdbd65920, 0x4703d7dd, 0x962707a1],
                            [0xe89c98bc, 0x356791bb, 0xfb6f7302, 0x2250ef05],
                            [0x282ef2a7, 0x235541b4, 0x55055d99, 0x1a8d0b29],
                            [0x79c60bab, 0xe5a11a50, 0x5f9e800b, 0x12bce70d],
                            [0xe9a08fb7, 0x3b482a8b, 0x50e1560e, 0xd6d70287]]));
    }
}

const FRAME_SIZES: [(usize, usize); 7] = [
    (160, 120), (128,  96), (176, 144), (352, 288),
    (704, 576), (240, 180), (320, 240)
];

/*const CRC_TABLE: [u16; 256] = [ //CCITT 16-bit CRC?
    0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50A5, 0x60C6, 0x70E7,
    0x8108, 0x9129, 0xA14A, 0xB16B, 0xC18C, 0xD1AD, 0xE1CE, 0xF1EF,
    0x1231, 0x0210, 0x3273, 0x2252, 0x52B5, 0x4294, 0x72F7, 0x62D6,
    0x9339, 0x8318, 0xB37B, 0xA35A, 0xD3BD, 0xC39C, 0xF3FF, 0xE3DE,
    0x2462, 0x3443, 0x0420, 0x1401, 0x64E6, 0x74C7, 0x44A4, 0x5485,
    0xA56A, 0xB54B, 0x8528, 0x9509, 0xE5EE, 0xF5CF, 0xC5AC, 0xD58D,
    0x3653, 0x2672, 0x1611, 0x0630, 0x76D7, 0x66F6, 0x5695, 0x46B4,
    0xB75B, 0xA77A, 0x9719, 0x8738, 0xF7DF, 0xE7FE, 0xD79D, 0xC7BC,
    0x48C4, 0x58E5, 0x6886, 0x78A7, 0x0840, 0x1861, 0x2802, 0x3823,
    0xC9CC, 0xD9ED, 0xE98E, 0xF9AF, 0x8948, 0x9969, 0xA90A, 0xB92B,
    0x5AF5, 0x4AD4, 0x7AB7, 0x6A96, 0x1A71, 0x0A50, 0x3A33, 0x2A12,
    0xDBFD, 0xCBDC, 0xFBBF, 0xEB9E, 0x9B79, 0x8B58, 0xBB3B, 0xAB1A,
    0x6CA6, 0x7C87, 0x4CE4, 0x5CC5, 0x2C22, 0x3C03, 0x0C60, 0x1C41,
    0xEDAE, 0xFD8F, 0xCDEC, 0xDDCD, 0xAD2A, 0xBD0B, 0x8D68, 0x9D49,
    0x7E97, 0x6EB6, 0x5ED5, 0x4EF4, 0x3E13, 0x2E32, 0x1E51, 0x0E70,
    0xFF9F, 0xEFBE, 0xDFDD, 0xCFFC, 0xBF1B, 0xAF3A, 0x9F59, 0x8F78,
    0x9188, 0x81A9, 0xB1CA, 0xA1EB, 0xD10C, 0xC12D, 0xF14E, 0xE16F,
    0x1080, 0x00A1, 0x30C2, 0x20E3, 0x5004, 0x4025, 0x7046, 0x6067,
    0x83B9, 0x9398, 0xA3FB, 0xB3DA, 0xC33D, 0xD31C, 0xE37F, 0xF35E,
    0x02B1, 0x1290, 0x22F3, 0x32D2, 0x4235, 0x5214, 0x6277, 0x7256,
    0xB5EA, 0xA5CB, 0x95A8, 0x8589, 0xF56E, 0xE54F, 0xD52C, 0xC50D,
    0x34E2, 0x24C3, 0x14A0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
    0xA7DB, 0xB7FA, 0x8799, 0x97B8, 0xE75F, 0xF77E, 0xC71D, 0xD73C,
    0x26D3, 0x36F2, 0x0691, 0x16B0, 0x6657, 0x7676, 0x4615, 0x5634,
    0xD94C, 0xC96D, 0xF90E, 0xE92F, 0x99C8, 0x89E9, 0xB98A, 0xA9AB,
    0x5844, 0x4865, 0x7806, 0x6827, 0x18C0, 0x08E1, 0x3882, 0x28A3,
    0xCB7D, 0xDB5C, 0xEB3F, 0xFB1E, 0x8BF9, 0x9BD8, 0xABBB, 0xBB9A,
    0x4A75, 0x5A54, 0x6A37, 0x7A16, 0x0AF1, 0x1AD0, 0x2AB3, 0x3A92,
    0xFD2E, 0xED0F, 0xDD6C, 0xCD4D, 0xBDAA, 0xAD8B, 0x9DE8, 0x8DC9,
    0x7C26, 0x6C07, 0x5C64, 0x4C45, 0x3CA2, 0x2C83, 0x1CE0, 0x0CC1,
    0xEF1F, 0xFF3E, 0xCF5D, 0xDF7C, 0xAF9B, 0xBFBA, 0x8FD9, 0x9FF8,
    0x6E17, 0x7E36, 0x4E55, 0x5E74, 0x2E93, 0x3EB2, 0x0ED1, 0x1EF0
];

fn calc_crc(src: &[u8], start: u16) -> u16 {
    let mut crc = start;
    for byte in src.iter() {
        crc = CRC_TABLE[(*byte ^ ((crc >> 8) as u8)) as usize] ^ (crc << 8);
    }
    crc
}*/
