use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::formats;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::{MV, ZIGZAG};
use nihav_codec_support::codecs::h263::*;
use nihav_codec_support::codecs::h263::code::H263BlockDSP;
use nihav_codec_support::codecs::h263::decoder::*;
use nihav_codec_support::codecs::h263::data::*;

#[allow(dead_code)]
struct Tables {
    intra_mcbpc_cb: Codebook<u8>,
    inter_mcbpc_cb: Codebook<u8>,
    cbpy_cb:        Codebook<u8>,
    rl_cb:          Codebook<H263RLSym>,
    aic_rl_cb:      Codebook<H263RLSym>,
    mv_cb:          Codebook<u8>,
    luma_dc_cb:     Codebook<u8>,
    chroma_dc_cb:   Codebook<u8>,
}

struct RealVideo10Decoder {
    info:    NACodecInfoRef,
    dec:     H263BaseDecoder,
    tables:  Tables,
    w:       usize,
    h:       usize,
    new_ver: bool,
    bdsp:    H263BlockDSP,
    mvmode:  MVMode,
}

struct RealVideo10BR<'a> {
    br:         BitReader<'a>,
    tables:     &'a Tables,
    num_slices: usize,
    slice_no:   usize,
    slice_off:  Vec<u32>,
    w:          usize,
    h:          usize,
    mb_w:       usize,
    mb_h:       usize,
    new_ver:    bool,
    dc_coded:   [bool; 3],
    last_dc:    [i16; 3],
    mvmode:     MVMode,
}

struct RV10SliceInfo {
    is_p:   bool,
    qscale: u8,
    mb_x:   usize,
    mb_y:   usize,
    mb_c:   usize,
}

impl RV10SliceInfo {
    fn new(is_p: bool, qscale: u8, mb_x: usize, mb_y: usize, mb_c: usize) -> Self {
        RV10SliceInfo { is_p, qscale, mb_x, mb_y, mb_c }
    }
}

impl<'a> RealVideo10BR<'a> {
    fn new(src: &'a [u8], tables: &'a Tables, width: usize, height: usize, new_ver: bool, mvmode: MVMode) -> Self {
        let nslices = (src[0] as usize) + 1;
        let mut slice_offs = Vec::with_capacity(nslices);
        {
            let offs = &src[1..][..nslices * 8];
            let mut br = BitReader::new(offs, BitReaderMode::BE);
            for _ in 0..nslices {
                br.skip(32).unwrap();
                let off = br.read(32).unwrap();
                slice_offs.push(off);
            }
        }
        let soff = nslices * 8 + 1;
        RealVideo10BR {
            br:         BitReader::new(&src[soff..], BitReaderMode::BE),
            tables,
            num_slices: nslices,
            slice_no:   0,
            slice_off:  slice_offs,
            w:          width,
            h:          height,
            mb_w:       (width  + 15) >> 4,
            mb_h:       (height + 15) >> 4,
            new_ver,
            dc_coded:   [false; 3],
            last_dc:    [0; 3],
            mvmode,
        }
    }

#[allow(unused_variables)]
    fn decode_block(&mut self, sstate: &SliceState, quant: u8, intra: bool, coded: bool, blk: &mut [i16; 64], plane_no: usize) -> DecoderResult<()> {
        let br = &mut self.br;
        let mut idx = 0;
        if intra {
            let mut dc;
            if !self.new_ver || !sstate.is_iframe {
                dc = br.read(8)? as i16;
                if dc == 255 { dc = 128; }
            } else {
                if self.dc_coded[plane_no] {
                    let diff;
                    let bits = br.peek(14);
                    let ret = if plane_no == 0 {
                            br.read_cb(&self.tables.luma_dc_cb)
                        } else {
                            br.read_cb(&self.tables.chroma_dc_cb)
                        };
                    if ret.is_err() {
                        println!("Illegal {} code {:X}", if plane_no==0{"luma"}else{"chroma"},bits);
                    }
                    let val = ret.unwrap() as i16;

                    if val != 0 {
                        diff = val - 128;
                    } else {
                        let code = br.read(2)?;
                        match code {
                            0x0 => { diff = ((br.read(7)? + 1) as i8) as i16; },
                            0x1 => { diff = (br.read(7)? as i16) - 128; },
                            0x2 => {
                                    if plane_no == 0 {
                                        if br.read_bool()? {
                                            diff = ((br.read(8)? + 1) as i8) as i16;
                                        } else {
                                            diff = (br.read(8)? as i8) as i16;
                                        }
                                    } else {
                                        br.skip(9)?;
                                        diff = 1;
                                    }
                                },
                            _   => {
                                    if plane_no == 0 {
                                        br.skip(4)?;
                                        diff = 1;
                                    } else {
                                        return Err(DecoderError::InvalidData);
                                    }
                                },
                        };
                    }
                    dc = (self.last_dc[plane_no] - diff) & 0xFF;
                    self.last_dc[plane_no] = dc;
                } else {
                    self.dc_coded[plane_no] = true;
                    dc = self.last_dc[plane_no];
                }
            }
            blk[0] = dc << 3;
            idx = 1;
        }
        if !coded { return Ok(()); }

        let rl_cb = &self.tables.rl_cb; // could be aic too
        let q_add = if quant == 0 { 0i16 } else { ((quant - 1) | 1) as i16 };
        let q = (quant * 2) as i16;
        while idx < 64 {
            let code = br.read_cb(rl_cb)?;
            let run;
            let mut level;
            let last;
            if !code.is_escape() {
                run   = code.get_run();
                level = code.get_level();
                last  = code.is_last();
                if br.read_bool()? { level = -level; }
                if level >= 0 {
                    level = (level * q) + q_add;
                } else {
                    level = (level * q) - q_add;
                }
            } else {
                last  = br.read_bool()?;
                run   = br.read(6)? as u8;
                level = br.read_s(8)? as i16;
                if level == -128 {
                    let low = br.read(5)? as i16;
                    let top = br.read_s(6)? as i16;
                    level = (top << 5) | low;
                }
                if level >= 0 {
                    level = (level * q) + q_add;
                } else {
                    level = (level * q) - q_add;
                }
                if level < -2048 { level = -2048; }
                if level >  2047 { level =  2047; }
            }
            idx += run;
            validate!(idx < 64);
            let oidx = ZIGZAG[idx as usize];
            blk[oidx] = level;
            idx += 1;
            if last { break; }
        }
        Ok(())
    }
}

fn decode_mv_component(br: &mut BitReader, mv_cb: &Codebook<u8>) -> DecoderResult<i16> {
    let code = br.read_cb(mv_cb)? as i16;
    if code == 0 { return Ok(0) }
    if !br.read_bool()? {
        Ok(code)
    } else {
        Ok(-code)
    }
}

fn decode_mv(br: &mut BitReader, mv_cb: &Codebook<u8>) -> DecoderResult<MV> {
    let xval = decode_mv_component(br, mv_cb)?;
    let yval = decode_mv_component(br, mv_cb)?;
    Ok(MV::new(xval, yval))
}

impl<'a> BlockDecoder for RealVideo10BR<'a> {

#[allow(unused_variables)]
    fn decode_pichdr(&mut self) -> DecoderResult<PicInfo> {
        self.slice_no = 0;
        let shdr = self.read_slice_header()?;
        validate!((shdr.mb_x == 0) && (shdr.mb_y == 0));

        let mb_end = shdr.mb_x + shdr.mb_y * self.mb_w + shdr.mb_c;

        let ftype = if !shdr.is_p { Type::I } else { Type::P };
        let picinfo = PicInfo::new(self.w, self.h, ftype, self.mvmode, false, false, shdr.qscale, 0, None, None);
        Ok(picinfo)
    }

    #[allow(unused_variables)]
    fn decode_slice_header(&mut self, info: &PicInfo) -> DecoderResult<SliceInfo> {
        let shdr = self.read_slice_header()?;
        self.slice_no += 1;
        let mb_end = shdr.mb_x + shdr.mb_y * self.mb_w + shdr.mb_c;
        let ret = SliceInfo::new(shdr.mb_x, shdr.mb_y, mb_end, shdr.qscale);

        Ok(ret)
    }

    fn decode_block_header(&mut self, info: &PicInfo, slice: &SliceInfo, _sstate: &SliceState) -> DecoderResult<BlockInfo> {
        let br = &mut self.br;
        let mut q = slice.get_quant();
        match info.get_mode() {
            Type::I => {
                    let mut cbpc = br.read_cb(&self.tables.intra_mcbpc_cb)?;
                    while cbpc == 8 { cbpc = br.read_cb(&self.tables.intra_mcbpc_cb)?; }
                    let cbpy = br.read_cb(&self.tables.cbpy_cb)?;
                    let cbp = (cbpy << 2) | (cbpc & 3);
                    let dquant = (cbpc & 4) != 0;
                    if dquant {
                        let idx = br.read(2)? as usize;
                        q = ((q as i16) + (H263_DQUANT_TAB[idx] as i16)) as u8;
                        validate!(q < 32);
                    }
                    Ok(BlockInfo::new(Type::I, cbp, q))
                },
            Type::P => {
                    if br.read_bool()? {
                        return Ok(BlockInfo::new(Type::Skip, 0, info.get_quant()));
                    }
                    let mut cbpc = br.read_cb(&self.tables.inter_mcbpc_cb)?;
                    while cbpc == 20 { cbpc = br.read_cb(&self.tables.inter_mcbpc_cb)?; }
                    let is_intra = (cbpc & 0x04) != 0;
                    let dquant   = (cbpc & 0x08) != 0;
                    let is_4x4   = (cbpc & 0x10) != 0;
                    if is_intra {
                        let cbpy = br.read_cb(&self.tables.cbpy_cb)?;
                        let cbp = (cbpy << 2) | (cbpc & 3);
                        if dquant {
                            let idx = br.read(2)? as usize;
                            q = ((q as i16) + (H263_DQUANT_TAB[idx] as i16)) as u8;
                            validate!(q < 32);
                        }
                        let binfo = BlockInfo::new(Type::I, cbp, q);
                        return Ok(binfo);
                    }

                    let mut cbpy = br.read_cb(&self.tables.cbpy_cb)?;
//                    if /* !aiv && */(cbpc & 3) != 3 {
                        cbpy ^= 0xF;
//                    }
                    let cbp = (cbpy << 2) | (cbpc & 3);
                    if dquant {
                        let idx = br.read(2)? as usize;
                        q = ((q as i16) + (H263_DQUANT_TAB[idx] as i16)) as u8;
                        validate!(q < 32);
                    }
                    let mut binfo = BlockInfo::new(Type::P, cbp, q);
                    if !is_4x4 {
                        let mvec: [MV; 1] = [decode_mv(br, &self.tables.mv_cb)?];
                        binfo.set_mv(&mvec);
                    } else {
                        let mvec: [MV; 4] = [
                                decode_mv(br, &self.tables.mv_cb)?,
                                decode_mv(br, &self.tables.mv_cb)?,
                                decode_mv(br, &self.tables.mv_cb)?,
                                decode_mv(br, &self.tables.mv_cb)?
                            ];
                        binfo.set_mv(&mvec);
                    }
                    Ok(binfo)
                },
            _ => { println!("wrong info mode"); Err(DecoderError::InvalidData) },
        }
    }

    #[allow(unused_variables)]
    fn decode_block_intra(&mut self, info: &BlockInfo, sstate: &SliceState, quant: u8, no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(sstate, quant, true, coded, blk, if no < 4 { 0 } else { no - 3 })
    }

    #[allow(unused_variables)]
    fn decode_block_inter(&mut self, info: &BlockInfo, sstate: &SliceState, quant: u8, no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(sstate, quant, false, coded, blk, if no < 4 { 0 } else { no - 3 })
    }

    fn is_slice_end(&mut self) -> bool { false }
}

impl<'a> RealVideo10BR<'a> {
    fn read_slice_header(&mut self) -> DecoderResult<RV10SliceInfo> {
        validate!(self.slice_no < self.num_slices);

        let br = &mut self.br;
        br.seek(self.slice_off[self.slice_no] * 8)?;

        let marker      = br.read(1)?;
        validate!(marker == 1);
        let is_p        = br.read_bool()?;
        let pb_frame    = br.read_bool()?;
        validate!(!pb_frame);
        let qscale      = br.read(5)? as u8;
        validate!(qscale > 0);

        if !is_p && self.new_ver {
            self.last_dc[0]  = br.read(8)? as i16;
            self.last_dc[1]  = br.read(8)? as i16;
            self.last_dc[2]  = br.read(8)? as i16;
        } else {
            self.last_dc[0] = 0;
            self.last_dc[1] = 0;
            self.last_dc[2] = 0;
        }
        self.dc_coded[0] = false;
        self.dc_coded[1] = false;
        self.dc_coded[2] = false;

        let mb_x;
        let mb_y;
        let mb_count;
        if (br.peek(12) == 0) || (self.slice_no > 0) {
            mb_x        = br.read(6)? as usize;
            mb_y        = br.read(6)? as usize;
            mb_count    = br.read(12)? as usize;
        } else {
            mb_x        = 0;
            mb_y        = 0;
            mb_count    = self.mb_w * self.mb_h;
        }
        br.skip(3)?;
        validate!(mb_x + mb_y * self.mb_w + mb_count <= self.mb_w * self.mb_h);

        Ok(RV10SliceInfo::new(is_p, qscale, mb_x, mb_y, mb_count))
    }
}

impl RealVideo10Decoder {
    fn new() -> Self {
        let mut coderead = H263ShortCodeReader::new(H263_INTRA_MCBPC);
        let intra_mcbpc_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263ShortCodeReader::new(H263_INTER_MCBPC);
        let inter_mcbpc_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263ShortCodeReader::new(H263_CBPY);
        let cbpy_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263RLCodeReader::new(H263_RL_CODES);
        let rl_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263RLCodeReader::new(H263_RL_CODES_AIC);
        let aic_rl_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263ShortCodeReader::new(H263_MV);
        let mv_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = CodeReader::new(RV10_LUMA_DC_CODES,  RV10_LUMA_DC_BITS);
        let luma_dc_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = CodeReader::new(RV10_CHROMA_DC_CODES,  RV10_CHROMA_DC_BITS);
        let chroma_dc_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();

        let tables = Tables {
            intra_mcbpc_cb,
            inter_mcbpc_cb,
            cbpy_cb,
            rl_cb,
            aic_rl_cb,
            mv_cb,
            luma_dc_cb,
            chroma_dc_cb,
        };

        RealVideo10Decoder{
            info:           NACodecInfoRef::default(),
            dec:            H263BaseDecoder::new_with_opts(0),
            tables,
            w:              0,
            h:              0,
            new_ver:        false,
            bdsp:           H263BlockDSP::new(),
            mvmode:         MVMode::Long,
        }
    }
}

impl NADecoder for RealVideo10Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            let fmt = formats::YUV420_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.w = w;
            self.h = h;

            let edata = info.get_extradata().unwrap();
            let src: &[u8] = &edata;
            let ver = ((src[4] as u32) << 12) | ((src[5] as u32) << 4) | ((src[6] as u32) >> 4);
            let maj_ver = ver >> 16;
            let mic_ver = ver & 0xFF;
            validate!(maj_ver == 1);
            self.new_ver = mic_ver > 0;
            if mic_ver == 2 {
                self.dec = H263BaseDecoder::new_with_opts(H263DEC_OPT_HAS_OBMC);
            }
            if (src[3] & 1) != 0 {
                self.mvmode = MVMode::UMV;
            }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        let mut ibr = RealVideo10BR::new(&src, &self.tables, self.w, self.h, self.new_ver, self.mvmode);

        let bufinfo = self.dec.parse_frame(&mut ibr, &self.bdsp)?;

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(self.dec.is_intra());
        frm.set_frame_type(if self.dec.is_intra() { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for RealVideo10Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RealVideo10Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::realmedia_register_all_decoders;
    use crate::realmedia_register_all_demuxers;
    #[test]
    fn test_rv10_old() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/real/VC-RV10/thankyou.rm
        test_decoding("realmedia", "realvideo1", "assets/RV/thankyou.rm",
                      Some(1000), &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x8bba459c, 0xe6e8e01c, 0x36f90595, 0xb268adee],
                            [0x32ca9567, 0xedc13b6f, 0xbee77cfd, 0xc7ebe24b],
                            [0xcf1865b6, 0xea7cf1b2, 0x30a5a622, 0xe5775b0d],
                            [0x8d984cfd, 0xcbc81a8d, 0x71d5b37a, 0x74115bba],
                            [0x7ec2a9e8, 0x291fc62a, 0x5fc62722, 0xf2072b87],
                            [0xa150585b, 0x9d608fe7, 0xb8d42676, 0x070103f7],
                            [0x8aadd96f, 0xa02e0627, 0xa89e104f, 0xf47d1227],
                            [0xa4416bb1, 0xc9ca7a61, 0xad43de90, 0x3e9ec5b7]]));
    }
    #[test]
    fn test_rv10_obmc() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        // sample from a private collection
        test_decoding("realmedia", "realvideo1",
                      "assets/RV/rv10_dnet_640x352_realvideo_encoder_4.0.rm",
                      Some(1000), &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x23599d2e, 0xf7212b24, 0x6b34b848, 0xbca84641],
                            [0x23599d2e, 0xf7212b24, 0x6b34b848, 0xbca84641],
                            [0x23599d2e, 0xf7212b24, 0x6b34b848, 0xbca84641],
                            [0x23599d2e, 0xf7212b24, 0x6b34b848, 0xbca84641],
                            [0x23599d2e, 0xf7212b24, 0x6b34b848, 0xbca84641],
                            [0x23599d2e, 0xf7212b24, 0x6b34b848, 0xbca84641],
                            [0x23599d2e, 0xf7212b24, 0x6b34b848, 0xbca84641],
                            [0x23599d2e, 0xf7212b24, 0x6b34b848, 0xbca84641],
                            [0x5cb179dc, 0x56ce9d07, 0x2439dd68, 0x80fec0bf],
                            [0x00de945d, 0xf44b71d3, 0x1dd93df9, 0x468bdcd5],
                            [0x7deb3aae, 0x8856c5da, 0x53011115, 0xed91377b],
                            [0x73afc311, 0xa61d36dc, 0x4e6ba0a3, 0x6dc64514],
                            [0xee35a8ce, 0x8edf5f32, 0x601d238f, 0xe5fa7ea7],
                            [0xe9aeaaa9, 0x876a221b, 0xe2d70923, 0x611849fd],
                            [0x0ff535cf, 0xf9e6ee1c, 0xed3a822c, 0x915056c0]]));
    }
}

pub struct CodeReader { codes: &'static [u16], bits: &'static [u8] }

impl CodeReader {
    pub fn new(codes: &'static [u16], bits: &'static [u8]) -> Self {
        CodeReader { codes, bits }
    }
}

impl CodebookDescReader<u8> for CodeReader {
    fn bits(&mut self, idx: usize) -> u8  { self.bits[idx] }
    fn code(&mut self, idx: usize) -> u32 { self.codes[idx] as u32 }
    fn sym (&mut self, idx: usize) -> u8 { idx as u8 }
    fn len(&mut self) -> usize { self.bits.len() }
}

const RV10_LUMA_DC_CODES: &[u16] = &[
    0x001f, 0x0f00, 0x0f01, 0x0f02, 0x0f03, 0x0f04, 0x0f05, 0x0f06,
    0x0f07, 0x0f08, 0x0f09, 0x0f0a, 0x0f0b, 0x0f0c, 0x0f0d, 0x0f0e,
    0x0f0f, 0x0f10, 0x0f11, 0x0f12, 0x0f13, 0x0f14, 0x0f15, 0x0f16,
    0x0f17, 0x0f18, 0x0f19, 0x0f1a, 0x0f1b, 0x0f1c, 0x0f1d, 0x0f1e,
    0x0f1f, 0x0f20, 0x0f21, 0x0f22, 0x0f23, 0x0f24, 0x0f25, 0x0f26,
    0x0f27, 0x0f28, 0x0f29, 0x0f2a, 0x0f2b, 0x0f2c, 0x0f2d, 0x0f2e,
    0x0f2f, 0x0f30, 0x0f31, 0x0f32, 0x0f33, 0x0f34, 0x0f35, 0x0f36,
    0x0f37, 0x0f38, 0x0f39, 0x0f3a, 0x0f3b, 0x0f3c, 0x0f3d, 0x0f3e,
    0x0f3f, 0x0380, 0x0381, 0x0382, 0x0383, 0x0384, 0x0385, 0x0386,
    0x0387, 0x0388, 0x0389, 0x038a, 0x038b, 0x038c, 0x038d, 0x038e,
    0x038f, 0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396,
    0x0397, 0x0398, 0x0399, 0x039a, 0x039b, 0x039c, 0x039d, 0x039e,
    0x039f, 0x00c0, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5, 0x00c6,
    0x00c7, 0x00c8, 0x00c9, 0x00ca, 0x00cb, 0x00cc, 0x00cd, 0x00ce,
    0x00cf, 0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056,
    0x0057, 0x0020, 0x0021, 0x0022, 0x0023, 0x000c, 0x000d, 0x0004,
    0x0000, 0x0005, 0x000e, 0x000f, 0x0024, 0x0025, 0x0026, 0x0027,
    0x0058, 0x0059, 0x005a, 0x005b, 0x005c, 0x005d, 0x005e, 0x005f,
    0x00d0, 0x00d1, 0x00d2, 0x00d3, 0x00d4, 0x00d5, 0x00d6, 0x00d7,
    0x00d8, 0x00d9, 0x00da, 0x00db, 0x00dc, 0x00dd, 0x00de, 0x00df,
    0x03a0, 0x03a1, 0x03a2, 0x03a3, 0x03a4, 0x03a5, 0x03a6, 0x03a7,
    0x03a8, 0x03a9, 0x03aa, 0x03ab, 0x03ac, 0x03ad, 0x03ae, 0x03af,
    0x03b0, 0x03b1, 0x03b2, 0x03b3, 0x03b4, 0x03b5, 0x03b6, 0x03b7,
    0x03b8, 0x03b9, 0x03ba, 0x03bb, 0x03bc, 0x03bd, 0x03be, 0x03bf,
    0x0f40, 0x0f41, 0x0f42, 0x0f43, 0x0f44, 0x0f45, 0x0f46, 0x0f47,
    0x0f48, 0x0f49, 0x0f4a, 0x0f4b, 0x0f4c, 0x0f4d, 0x0f4e, 0x0f4f,
    0x0f50, 0x0f51, 0x0f52, 0x0f53, 0x0f54, 0x0f55, 0x0f56, 0x0f57,
    0x0f58, 0x0f59, 0x0f5a, 0x0f5b, 0x0f5c, 0x0f5d, 0x0f5e, 0x0f5f,
    0x0f60, 0x0f61, 0x0f62, 0x0f63, 0x0f64, 0x0f65, 0x0f66, 0x0f67,
    0x0f68, 0x0f69, 0x0f6a, 0x0f6b, 0x0f6c, 0x0f6d, 0x0f6e, 0x0f6f,
    0x0f70, 0x0f71, 0x0f72, 0x0f73, 0x0f74, 0x0f75, 0x0f76, 0x0f77,
    0x0f78, 0x0f79, 0x0f7a, 0x0f7b, 0x0f7c, 0x0f7d, 0x0f7e, 0x0f7f,
];

const RV10_LUMA_DC_BITS: &[u8] = &[
     5, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10,
    10,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  8,  8,  8,  8,
     8,  7,  7,  7,  7,  7,  7,  7,
     7,  6,  6,  6,  6,  5,  5,  4,
     2,  4,  5,  5,  6,  6,  6,  6,
     7,  7,  7,  7,  7,  7,  7,  7,
     8,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  8,  8,  8,  8,
    10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
];

const RV10_CHROMA_DC_CODES: &[u16] = &[
    0x0000, 0x3f00, 0x3f01, 0x3f02, 0x3f03, 0x3f04, 0x3f05, 0x3f06,
    0x3f07, 0x3f08, 0x3f09, 0x3f0a, 0x3f0b, 0x3f0c, 0x3f0d, 0x3f0e,
    0x3f0f, 0x3f10, 0x3f11, 0x3f12, 0x3f13, 0x3f14, 0x3f15, 0x3f16,
    0x3f17, 0x3f18, 0x3f19, 0x3f1a, 0x3f1b, 0x3f1c, 0x3f1d, 0x3f1e,
    0x3f1f, 0x3f20, 0x3f21, 0x3f22, 0x3f23, 0x3f24, 0x3f25, 0x3f26,
    0x3f27, 0x3f28, 0x3f29, 0x3f2a, 0x3f2b, 0x3f2c, 0x3f2d, 0x3f2e,
    0x3f2f, 0x3f30, 0x3f31, 0x3f32, 0x3f33, 0x3f34, 0x3f35, 0x3f36,
    0x3f37, 0x3f38, 0x3f39, 0x3f3a, 0x3f3b, 0x3f3c, 0x3f3d, 0x3f3e,
    0x3f3f, 0x0f80, 0x0f81, 0x0f82, 0x0f83, 0x0f84, 0x0f85, 0x0f86,
    0x0f87, 0x0f88, 0x0f89, 0x0f8a, 0x0f8b, 0x0f8c, 0x0f8d, 0x0f8e,
    0x0f8f, 0x0f90, 0x0f91, 0x0f92, 0x0f93, 0x0f94, 0x0f95, 0x0f96,
    0x0f97, 0x0f98, 0x0f99, 0x0f9a, 0x0f9b, 0x0f9c, 0x0f9d, 0x0f9e,
    0x0f9f, 0x03c0, 0x03c1, 0x03c2, 0x03c3, 0x03c4, 0x03c5, 0x03c6,
    0x03c7, 0x03c8, 0x03c9, 0x03ca, 0x03cb, 0x03cc, 0x03cd, 0x03ce,
    0x03cf, 0x00e0, 0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6,
    0x00e7, 0x0030, 0x0031, 0x0032, 0x0033, 0x0008, 0x0009, 0x0002,
    0x0000, 0x0003, 0x000a, 0x000b, 0x0034, 0x0035, 0x0036, 0x0037,
    0x00e8, 0x00e9, 0x00ea, 0x00eb, 0x00ec, 0x00ed, 0x00ee, 0x00ef,
    0x03d0, 0x03d1, 0x03d2, 0x03d3, 0x03d4, 0x03d5, 0x03d6, 0x03d7,
    0x03d8, 0x03d9, 0x03da, 0x03db, 0x03dc, 0x03dd, 0x03de, 0x03df,
    0x0fa0, 0x0fa1, 0x0fa2, 0x0fa3, 0x0fa4, 0x0fa5, 0x0fa6, 0x0fa7,
    0x0fa8, 0x0fa9, 0x0faa, 0x0fab, 0x0fac, 0x0fad, 0x0fae, 0x0faf,
    0x0fb0, 0x0fb1, 0x0fb2, 0x0fb3, 0x0fb4, 0x0fb5, 0x0fb6, 0x0fb7,
    0x0fb8, 0x0fb9, 0x0fba, 0x0fbb, 0x0fbc, 0x0fbd, 0x0fbe, 0x0fbf,
    0x3f40, 0x3f41, 0x3f42, 0x3f43, 0x3f44, 0x3f45, 0x3f46, 0x3f47,
    0x3f48, 0x3f49, 0x3f4a, 0x3f4b, 0x3f4c, 0x3f4d, 0x3f4e, 0x3f4f,
    0x3f50, 0x3f51, 0x3f52, 0x3f53, 0x3f54, 0x3f55, 0x3f56, 0x3f57,
    0x3f58, 0x3f59, 0x3f5a, 0x3f5b, 0x3f5c, 0x3f5d, 0x3f5e, 0x3f5f,
    0x3f60, 0x3f61, 0x3f62, 0x3f63, 0x3f64, 0x3f65, 0x3f66, 0x3f67,
    0x3f68, 0x3f69, 0x3f6a, 0x3f6b, 0x3f6c, 0x3f6d, 0x3f6e, 0x3f6f,
    0x3f70, 0x3f71, 0x3f72, 0x3f73, 0x3f74, 0x3f75, 0x3f76, 0x3f77,
    0x3f78, 0x3f79, 0x3f7a, 0x3f7b, 0x3f7c, 0x3f7d, 0x3f7e, 0x3f7f,
];

const RV10_CHROMA_DC_BITS: &[u8] = &[
     0, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10,
    10,  8,  8,  8,  8,  8,  8,  8,
     8,  6,  6,  6,  6,  4,  4,  3,
     2,  3,  4,  4,  6,  6,  6,  6,
     8,  8,  8,  8,  8,  8,  8,  8,
    10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14,
];
