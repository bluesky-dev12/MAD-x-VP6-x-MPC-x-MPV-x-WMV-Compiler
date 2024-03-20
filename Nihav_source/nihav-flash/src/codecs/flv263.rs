use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::formats;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::{MV, ZIGZAG};
use nihav_codec_support::codecs::h263::*;
use nihav_codec_support::codecs::h263::code::*;
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
}

struct SparkDecoder {
    info:    NACodecInfoRef,
    dec:     H263BaseDecoder,
    tables:  Tables,
    w:       usize,
    h:       usize,
    bdsp:    H263BlockDSP,
}

struct SparkBR<'a> {
    br:         BitReader<'a>,
    tables:     &'a Tables,
    ver1:       bool,
    mb_w:       usize,
    mb_h:       usize,
}

fn decode_mv_component(br: &mut BitReader, mv_cb: &Codebook<u8>) -> DecoderResult<i16> {
    let code = i16::from(br.read_cb(mv_cb)?);
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

impl<'a> SparkBR<'a> {
    fn new(src: &'a [u8], tables: &'a Tables, w: usize, h: usize) -> Self {
        Self {
            br:     BitReader::new(src, BitReaderMode::BE),
            tables,
            ver1:   false,
            mb_w:   (w + 15) >> 4,
            mb_h:   (h + 15) >> 4,
        }
    }

    fn decode_block(&mut self, quant: u8, intra: bool, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        let br = &mut self.br;
        let mut idx = 0;
        if intra {
            let mut dc = br.read(8)?;
            if dc == 255 { dc = 128; }
            blk[0] = (dc as i16) << 3;
            idx = 1;
        }
        if !coded { return Ok(()); }

        let rl_cb = &self.tables.rl_cb; // could be aic too
        let q_add = if quant == 0 { 0i16 } else { i16::from((quant - 1) | 1) };
        let q = i16::from(quant * 2);
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
                if level > 0 {
                    level = (level * q) + q_add;
                } else {
                    level = (level * q) - q_add;
                }
            } else if !self.ver1 {
                last  = br.read_bool()?;
                run   = br.read(6)? as u8;
                level = br.read_s(8)? as i16;
                if level == -128 {
                    let low = br.read(5)? as i16;
                    let top = br.read_s(6)? as i16;
                    level = (top << 5) | low;
                }
                if level > 0 {
                    level = (level * q) + q_add;
                } else {
                    level = (level * q) - q_add;
                }
                if level < -2048 { level = -2048; }
                if level >  2047 { level =  2047; }
            } else {
                let fmt_bit = br.read_bool()?;
                last  = br.read_bool()?;
                run   = br.read(6)? as u8;
                level = br.read_s(if !fmt_bit { 7 } else { 11 })? as i16;
                validate!(level != 0);
                if !fmt_bit {
                    validate!(level != 64);
                } else {
                    validate!(level != 1024);
                }
                if level > 0 {
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


impl<'a> BlockDecoder for SparkBR<'a> {
    fn decode_pichdr(&mut self) -> DecoderResult<PicInfo> {
        let br = &mut self.br;
        let syncw                   = br.read(17)?;
        validate!(syncw == 1);
        let version                 = br.read(5)?;
        validate!(version == 0 || version == 1);
        self.ver1 = version == 1;
        let tr                      = (br.read(8)? << 8) as u16;
        let sfmt                    = br.read(3)?;
        validate!(sfmt != 0b111);
        let (w, h) = match sfmt {
                0 => {
                    let w           = br.read(8)? as usize;
                    let h           = br.read(8)? as usize;
                    validate!(w != 0 && h != 0);
                    (w, h)
                },
                1 => {
                    let w           = br.read(16)? as usize;
                    let h           = br.read(16)? as usize;
                    validate!(w != 0 && h != 0);
                    (w, h)
                },
                2 => (352, 288),
                3 => (176, 144),
                4 => (128, 96),
                5 => (320, 240),
                6 => (160, 120),
                _ => unreachable!(),
            };
        let pic_type                = br.read(2)?;
        let ftype = match pic_type {
                0 => Type::I,
                1 => Type::P,
                2 => Type::Special,
                _ => return Err(DecoderError::InvalidData),
            };
        let deblock                 = br.read_bool()?;
        let quant                   = br.read(5)?;
        while br.read_bool()? { // skip PEI
            br.read(8)?;
        }
        self.mb_w = (w + 15) >> 4;
        self.mb_h = (h + 15) >> 4;

        let plusinfo = if deblock { Some(PlusInfo::new(false, deblock, false, false)) } else { None };
        let picinfo = PicInfo::new(w, h, ftype, MVMode::Long, true, false, quant as u8, tr, None, plusinfo);
        Ok(picinfo)
    }
    fn decode_slice_header(&mut self, info: &PicInfo) -> DecoderResult<SliceInfo> {
        Ok(SliceInfo::new(0, 0, self.mb_w * self.mb_h, info.quant))
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
                        q = (i16::from(q) + i16::from(H263_DQUANT_TAB[idx])) as u8;
                    }
                    Ok(BlockInfo::new(Type::I, cbp, q))
                },
            Type::P | Type::Special => {
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
            _ => { Err(DecoderError::InvalidData) },
        }
    }
    fn decode_block_intra(&mut self, _info: &BlockInfo, _sstate: &SliceState, quant: u8, _no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(quant, true, coded, blk)
    }
    fn decode_block_inter(&mut self, _info: &BlockInfo, _sstate: &SliceState, quant: u8, _no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(quant, false, coded, blk)
    }
    fn is_slice_end(&mut self) -> bool { self.br.peek(16) == 0 }
}

impl SparkDecoder {
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
        let tables = Tables {
            intra_mcbpc_cb,
            inter_mcbpc_cb,
            cbpy_cb,
            rl_cb,
            aic_rl_cb,
            mv_cb,
        };

        Self {
            info:       NACodecInfo::new_dummy(),
            dec:        H263BaseDecoder::new(true),
            tables,
            bdsp:       H263BlockDSP::new(),
            w:          0,
            h:          0,
        }
    }
}

impl NADecoder for SparkDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            let fmt = formats::YUV420_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.w = w;
            self.h = h;

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        let mut ibr = SparkBR::new(&src, &self.tables, self.w, self.h);
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

impl NAOptionHandler for SparkDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(SparkDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::flash_register_all_decoders;
    use crate::flash_register_all_demuxers;
    #[test]
    fn test_flv263() {
        let mut dmx_reg = RegisteredDemuxers::new();
        flash_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        flash_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/Nelly_Moser/input.flv
        test_decoding("flv", "flv263", "assets/Flash/input.flv",
                      Some(1000), &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x9110718e, 0x794e22ad, 0x3324e552, 0xf58a0449],
                            [0x2f7cfcd1, 0x1d6bb63b, 0x37dcd87d, 0xb0247d9c],
                            [0x317e6355, 0xc632f2d5, 0x1d6ae472, 0x45cc1ba6],
                            [0x7d883ffc, 0xaa8e7c68, 0x8dec683b, 0x0e0dcdea],
                            [0x79d4cece, 0x98749753, 0xfedb0fb1, 0x5398f6a0],
                            [0x153f1558, 0xd98a700c, 0xdb166ebe, 0xc347fd61],
                            [0x0b31c6a9, 0xcb126876, 0xd7dd8626, 0x4a6fea9f]]));
    }
}
