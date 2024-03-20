use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::formats;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::{MV, ZIGZAG};
use nihav_codec_support::codecs::blockdsp;
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

struct VivoBlockDSP {
    dct_tab:        [[f64; 64]; 64],
}

fn gen_coef(i: usize, j: usize) -> f64 {
    if i == 0 {
        1.0 / 8.0f64.sqrt()
    } else {
        (((j as f64) + 0.5) * (i as f64) * std::f64::consts::PI / 8.0).cos() * 0.5
    }
}

impl VivoBlockDSP {
    fn new() -> Self {
        let mut dct_tab = [[0.0; 64]; 64];
        for i in 0..8 {
            for j in 0..8 {
                for k in 0..8 {
                    for l in 0..8 {
                        let c0 = gen_coef(i, k);
                        let c1 = gen_coef(j, l);
                        let c = c0 * c1 * 64.0;
                        dct_tab[i * 8 + j][k * 8 + l] = c;
                    }
                }
            }
        }
        Self {
            dct_tab
        }
    }
}

#[allow(clippy::erasing_op)]
#[allow(clippy::identity_op)]
fn deblock_hor(buf: &mut [u8], stride: usize, off: usize, clip_tab: &[i16; 64]) {
    for x in 0..8 {
        let p1 = i16::from(buf[off - 2 * stride + x]);
        let p0 = i16::from(buf[off - 1 * stride + x]);
        let q0 = i16::from(buf[off + 0 * stride + x]);
        let q1 = i16::from(buf[off + 1 * stride + x]);
        let diff = (3 * (p1 - q1) + 8 * (q0 - p0)) >> 4;
        if (diff != 0) && (diff > -32) && (diff < 32) {
            let delta = clip_tab[(diff + 32) as usize];
            buf[off - 1 * stride + x] = (p0 + delta).max(0).min(255) as u8;
            buf[off + 0 * stride + x] = (q0 - delta).max(0).min(255) as u8;
        }
    }
}

#[allow(clippy::identity_op)]
fn deblock_ver(buf: &mut [u8], stride: usize, off: usize, clip_tab: &[i16; 64]) {
    for y in 0..8 {
        let p1 = i16::from(buf[off - 2 + y * stride]);
        let p0 = i16::from(buf[off - 1 + y * stride]);
        let q0 = i16::from(buf[off + 0 + y * stride]);
        let q1 = i16::from(buf[off + 1 + y * stride]);
        let diff = (3 * (p1 - q1) + 8 * (q0 - p0)) >> 4;
        if (diff != 0) && (diff > -32) && (diff < 32) {
            let delta = clip_tab[(diff + 32) as usize];
            buf[off - 1 + y * stride] = (p0 + delta).max(0).min(255) as u8;
            buf[off     + y * stride] = (q0 - delta).max(0).min(255) as u8;
        }
    }
}

fn gen_clip_tab(clip_tab: &mut [i16; 64], q: u8) {
    let q = i16::from(q);
    *clip_tab = [0; 64];
    let lim = (q + 2) >> 1;
    for i in 0..lim {
        clip_tab[(32 - i) as usize] = -i;
        clip_tab[(32 + i) as usize] =  i;
    }
    for i in lim..q {
        let val = q - i;
        clip_tab[(32 - i) as usize] = -val;
        clip_tab[(32 + i) as usize] =  val;
    }
}

impl BlockDSP for VivoBlockDSP {
    fn idct(&self, blk: &mut [i16; 64]) {
        let mut tmp = [0i32; 64];
        for (i, &el) in blk.iter().enumerate() {
            if el != 0 {
                let cmat = self.dct_tab[i];
                for (dst, &src) in tmp.iter_mut().zip(cmat.iter()) {
                    *dst += (src * (el as f64)) as i32;
                }
            }
        }
        for (dst, &src) in blk.iter_mut().zip(tmp.iter()) {
            *dst = ((src + 0x20) >> 6) as i16;
        }
    }
    fn copy_blocks(&self, dst: &mut NAVideoBuffer<u8>, src: NAVideoBufferRef<u8>, xpos: usize, ypos: usize, mv: MV) {
        let mode = ((mv.x & 1) + (mv.y & 1) * 2) as usize;
        let cmode = (if (mv.x & 3) != 0 { 1 } else { 0 }) + (if (mv.y & 3) != 0 { 2 } else { 0 });

        let mut dst = NASimpleVideoFrame::from_video_buf(dst).unwrap();

        blockdsp::copy_block(&mut dst, src.clone(), 0, xpos, ypos, mv.x >> 1, mv.y >> 1, 16, 16, 0, 1, mode, H263_INTERP_FUNCS);
        blockdsp::copy_block(&mut dst, src.clone(), 1, xpos >> 1, ypos >> 1, mv.x >> 2, mv.y >> 2, 8, 8, 0, 1, cmode, H263_INTERP_FUNCS);
        blockdsp::copy_block(&mut dst, src,         2, xpos >> 1, ypos >> 1, mv.x >> 2, mv.y >> 2, 8, 8, 0, 1, cmode, H263_INTERP_FUNCS);
    }
    fn copy_blocks8x8(&self, dst: &mut NAVideoBuffer<u8>, src: NAVideoBufferRef<u8>, xpos: usize, ypos: usize, mvs: &[MV; 4]) {
        let mut dst = NASimpleVideoFrame::from_video_buf(dst).unwrap();

        for (i, mv) in mvs.iter().enumerate() {
            let xadd = (i & 1) * 8;
            let yadd = (i & 2) * 4;
            let mode = ((mv.x & 1) + (mv.y & 1) * 2) as usize;

            blockdsp::copy_block(&mut dst, src.clone(), 0, xpos + xadd, ypos + yadd, mv.x >> 1, mv.y >> 1, 8, 8, 0, 1, mode, H263_INTERP_FUNCS);
        }

        let sum_mv = mvs[0] + mvs[1] + mvs[2] + mvs[3];
        let cmx = (sum_mv.x >> 3) + H263_CHROMA_ROUND[(sum_mv.x & 0xF) as usize];
        let cmy = (sum_mv.y >> 3) + H263_CHROMA_ROUND[(sum_mv.y & 0xF) as usize];
        let mode = ((cmx & 1) + (cmy & 1) * 2) as usize;
        for plane in 1..3 {
            blockdsp::copy_block(&mut dst, src.clone(), plane, xpos >> 1, ypos >> 1, cmx >> 1, cmy >> 1, 8, 8, 0, 1, mode, H263_INTERP_FUNCS);
        }
    }
    fn avg_blocks(&self, dst: &mut NAVideoBuffer<u8>, src: NAVideoBufferRef<u8>, xpos: usize, ypos: usize, mv: MV) {
        let mode = ((mv.x & 1) + (mv.y & 1) * 2) as usize;
        let cmode = (if (mv.x & 3) != 0 { 1 } else { 0 }) + (if (mv.y & 3) != 0 { 2 } else { 0 });

        let mut dst = NASimpleVideoFrame::from_video_buf(dst).unwrap();

        blockdsp::copy_block(&mut dst, src.clone(), 0, xpos, ypos, mv.x >> 1, mv.y >> 1, 16, 16, 0, 1, mode, H263_INTERP_AVG_FUNCS);
        blockdsp::copy_block(&mut dst, src.clone(), 1, xpos >> 1, ypos >> 1, mv.x >> 2, mv.y >> 2, 8, 8, 0, 1, cmode, H263_INTERP_AVG_FUNCS);
        blockdsp::copy_block(&mut dst, src,         2, xpos >> 1, ypos >> 1, mv.x >> 2, mv.y >> 2, 8, 8, 0, 1, cmode, H263_INTERP_AVG_FUNCS);
    }
    fn avg_blocks8x8(&self, dst: &mut NAVideoBuffer<u8>, src: NAVideoBufferRef<u8>, xpos: usize, ypos: usize, mvs: &[MV; 4]) {
        let mut dst = NASimpleVideoFrame::from_video_buf(dst).unwrap();

        for (i, mv) in mvs.iter().enumerate() {
            let xadd = (i & 1) * 8;
            let yadd = (i & 2) * 4;
            let mode = ((mv.x & 1) + (mv.y & 1) * 2) as usize;

            blockdsp::copy_block(&mut dst, src.clone(), 0, xpos + xadd, ypos + yadd, mv.x >> 1, mv.y >> 1, 8, 8, 0, 1, mode, H263_INTERP_AVG_FUNCS);
        }

        let sum_mv = mvs[0] + mvs[1] + mvs[2] + mvs[3];
        let cmx = (sum_mv.x >> 3) + H263_CHROMA_ROUND[(sum_mv.x & 0xF) as usize];
        let cmy = (sum_mv.y >> 3) + H263_CHROMA_ROUND[(sum_mv.y & 0xF) as usize];
        let mode = ((cmx & 1) + (cmy & 1) * 2) as usize;
        for plane in 1..3 {
            blockdsp::copy_block(&mut dst, src.clone(), plane, xpos >> 1, ypos >> 1, cmx >> 1, cmy >> 1, 8, 8, 0, 1, mode, H263_INTERP_AVG_FUNCS);
        }
    }
    fn filter_row(&self, buf: &mut NAVideoBuffer<u8>, mb_y: usize, mb_w: usize, cbpi: &CBPInfo) {
        let ystride = buf.get_stride(0);
        let ustride = buf.get_stride(1);
        let vstride = buf.get_stride(2);
        let yoff = buf.get_offset(0) + mb_y * 16 * ystride;
        let uoff = buf.get_offset(1) + mb_y * 8 * ustride;
        let voff = buf.get_offset(2) + mb_y * 8 * vstride;
        let buf = buf.get_data_mut().unwrap();

        let mut clip_tab = [0i16; 64];
        let mut last_q = 0;
        let mut off = yoff;
        for mb_x in 0..mb_w {
            let q = cbpi.get_q(mb_w + mb_x);
            if q != last_q {
                gen_clip_tab(&mut clip_tab, q);
                last_q = q;
            }
            if mb_y != 0 {
                deblock_hor(buf, ystride, off, &clip_tab);
                deblock_hor(buf, ystride, off + 8, &clip_tab);
            }
            deblock_hor(buf, ystride, off + 8 * ystride, &clip_tab);
            deblock_hor(buf, ystride, off + 8 * ystride + 8, &clip_tab);
            off += 16;
        }
        let mut off = yoff;
        for mb_x in 0..mb_w {
            let q = cbpi.get_q(mb_w + mb_x);
            if q != last_q {
                gen_clip_tab(&mut clip_tab, q);
                last_q = q;
            }
            if mb_y != 0 {
                let qtop = cbpi.get_q(mb_x);
                if qtop != last_q {
                    gen_clip_tab(&mut clip_tab, qtop);
                    last_q = qtop;
                }
                if mb_x != 0 {
                    deblock_ver(buf, ystride, off - 8 * ystride, &clip_tab);
                }
                deblock_ver(buf, ystride, off - 8 * ystride + 8, &clip_tab);
            }
            if mb_x != 0 {
                deblock_ver(buf, ystride, off, &clip_tab);
                deblock_ver(buf, ystride, off + 8, &clip_tab);
            }
            off += 16;
        }
        if mb_y != 0 {
            for mb_x in 0..mb_w {
                let q = cbpi.get_q(mb_w + mb_x);
                if q != last_q {
                    gen_clip_tab(&mut clip_tab, q);
                    last_q = q;
                }
                deblock_hor(buf, ustride, uoff + mb_x * 8, &clip_tab);
                deblock_hor(buf, vstride, voff + mb_x * 8, &clip_tab);
            }
            let offu = uoff - 8 * ustride;
            let offv = voff - 8 * vstride;
            for mb_x in 1..mb_w {
                let qt = cbpi.get_q(mb_x);
                if qt != last_q {
                    gen_clip_tab(&mut clip_tab, qt);
                    last_q = qt;
                }
                deblock_ver(buf, ustride, offu + mb_x * 8, &clip_tab);
                deblock_ver(buf, vstride, offv + mb_x * 8, &clip_tab);
            }
        }
    }
}

struct VivoDecoder {
    info:       NACodecInfoRef,
    dec:        H263BaseDecoder,
    tables:     Tables,
    bdsp:       VivoBlockDSP,
    lastframe:  Option<NABufferType>,
    lastpts:    Option<u64>,
    width:      usize,
    height:     usize,
}

struct VivoBR<'a> {
    br:     BitReader<'a>,
    tables: &'a Tables,
    gob_no: usize,
    mb_w:   usize,
    is_pb:  bool,
    is_ipb: bool,
    ref_w:  usize,
    ref_h:  usize,
    aic:    bool,
}

fn check_marker(br: &mut BitReader) -> DecoderResult<()> {
    let mark = br.read(1)?;
    validate!(mark == 1);
    Ok(())
}

impl<'a> VivoBR<'a> {
    fn new(src: &'a [u8], tables: &'a Tables, ref_w: usize, ref_h: usize) -> Self {
        VivoBR {
            br:     BitReader::new(src, BitReaderMode::BE),
            tables,
            gob_no: 0,
            mb_w:   0,
            is_pb:  false,
            is_ipb: false,
            ref_w, ref_h,
            aic:    false,
        }
    }

    fn decode_block(&mut self, quant: u8, intra: bool, coded: bool, blk: &mut [i16; 64], _plane_no: usize, acpred: ACPredMode) -> DecoderResult<()> {
        let br = &mut self.br;
        let mut idx = 0;
        if !self.aic && intra {
            let mut dc = br.read(8)? as i16;
            if dc == 255 { dc = 128; }
            blk[0] = dc << 3;
            idx = 1;
        }
        if !coded { return Ok(()); }
        let scan = match acpred {
                    ACPredMode::Hor => H263_SCAN_V,
                    ACPredMode::Ver => H263_SCAN_H,
                    _               => &ZIGZAG,
                };

        let rl_cb = if self.aic && intra { &self.tables.aic_rl_cb } else { &self.tables.rl_cb };
        let q = i16::from(quant) * 2;
        let q_add = q >> 1;
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
                if !intra || !self.aic {
                    if level >= 0 {
                        level = (level * q) + q_add;
                    } else {
                        level = (level * q) - q_add;
                    }
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
                if !intra || !self.aic {
                    if level >= 0 {
                        level = (level * q) + q_add;
                    } else {
                        level = (level * q) - q_add;
                    }
                    if level < -2048 { level = -2048; }
                    if level >  2047 { level =  2047; }
                }
            }
            idx += run;
            validate!(idx < 64);
            let oidx = scan[idx as usize];
            blk[oidx] = level;
            idx += 1;
            if last { break; }
        }
        Ok(())
    }
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

fn decode_b_info(br: &mut BitReader, is_pb: bool, is_ipb: bool, is_intra: bool) -> DecoderResult<BBlockInfo> {
    if is_pb { // as improved pb
        if is_ipb {
            let pb_mv_add = if is_intra { 1 } else { 0 };
            if br.read_bool()?{
                if br.read_bool()? {
                    let pb_mv_count = 1 - (br.read(1)? as usize);
                    let cbpb = br.read(6)? as u8;
                    Ok(BBlockInfo::new(true, cbpb, pb_mv_count + pb_mv_add, pb_mv_count == 1))
                } else {
                    Ok(BBlockInfo::new(true, 0, 1 + pb_mv_add, true))
                }
            } else {
                Ok(BBlockInfo::new(true, 0, pb_mv_add, false))
            }
        } else {
            let mvdb = br.read_bool()?;
            let cbpb = if mvdb && br.read_bool()? { br.read(6)? as u8 } else { 0 };
            Ok(BBlockInfo::new(true, cbpb, if mvdb { 1 } else { 0 }, false))
        }
    } else {
        Ok(BBlockInfo::new(false, 0, 0, false))
    }
}

impl<'a> BlockDecoder for VivoBR<'a> {

#[allow(unused_variables)]
#[allow(clippy::unreadable_literal)]
    fn decode_pichdr(&mut self) -> DecoderResult<PicInfo> {
        let br = &mut self.br;
        let syncw = br.read(22)?;
        validate!(syncw == 0x000020);
        let tr = (br.read(8)? << 8) as u16;
        check_marker(br)?;
        let id = br.read(1)?;
        validate!(id == 0);
        br.read(1)?; // split screen indicator
        br.read(1)?; // document camera indicator
        br.read(1)?; // freeze picture release
        let mut sfmt = br.read(3)?;
        validate!(sfmt != 0b000);
        let is_intra = !br.read_bool()?;
        let umv = br.read_bool()?;
        br.read(1)?; // syntax arithmetic coding
        let apm = br.read_bool()?;
        self.is_pb = br.read_bool()?;
        let deblock;
        let pbplus;
        let aic;
        if sfmt == 0b110 {
            sfmt = br.read(3)?;
            validate!(sfmt != 0b000 && sfmt != 0b110);
            aic = br.read_bool()?;
            deblock = br.read_bool()?;
            br.read(4)?; // unknown flags
            pbplus = br.read_bool()?;
            br.read(1)?; // unknown flag
            let _interlaced = br.read_bool()?;
            br.read(2)?; // unknown flags
        } else {
            aic = false;
            deblock = false;
            pbplus = false;
        }
        self.is_ipb = pbplus;
        let (w, h) = match sfmt {
                0b001 => ( 64,  48),
                0b011 => ( 88,  72),
                0b010 => (176, 144),
                0b100 => (352, 288),
                0b101 => (704, 576),
                0b111 => {
                    validate!((self.ref_w != 0) && (self.ref_h != 0));
                    ((self.ref_w + 15) & !15, (self.ref_h + 15) & !15)
                },
                _ => return Err(DecoderError::InvalidData),
            };
        let quant = br.read(5)?;
        let cpm = br.read_bool()?;
        validate!(!cpm);

        let pbinfo;
        if self.is_pb {
            let trb = br.read(3)?;
            let dbquant = br.read(2)?;
            pbinfo = Some(PBInfo::new(trb as u8, dbquant as u8, pbplus));
        } else {
            pbinfo = None;
        }
        while br.read_bool()? { // skip PEI
            br.read(8)?;
        }
        self.gob_no = 0;
        self.mb_w = (w + 15) >> 4;
        self.aic = aic;

        let ftype = if is_intra { Type::I } else { Type::P };
        let plusinfo = Some(PlusInfo::new(aic, deblock, false, false));
        let mvmode = if umv { MVMode::UMV } else { MVMode::Old };
        let picinfo = PicInfo::new(w, h, ftype, mvmode, umv, apm, quant as u8, tr, pbinfo, plusinfo);
        Ok(picinfo)
    }

    #[allow(unused_variables)]
    fn decode_slice_header(&mut self, info: &PicInfo) -> DecoderResult<SliceInfo> {
        let br = &mut self.br;
        let gbsc = br.read(17)?;
        validate!(gbsc == 1);
        let gn = br.read(5)?;
        let gfid = br.read(2)?;
        let gquant = br.read(5)?;
        let ret = SliceInfo::new_gob(0, self.gob_no, gquant as u8);
        self.gob_no += 1;
        Ok(ret)
    }

    #[allow(unused_variables)]
    fn decode_block_header(&mut self, info: &PicInfo, slice: &SliceInfo, sstate: &SliceState) -> DecoderResult<BlockInfo> {
        let br = &mut self.br;
        let mut q = slice.get_quant();
        match info.get_mode() {
            Type::I => {
                    let mut cbpc = br.read_cb(&self.tables.intra_mcbpc_cb)?;
                    while cbpc == 8 { cbpc = br.read_cb(&self.tables.intra_mcbpc_cb)?; }
                    let mut acpred = ACPredMode::None;
                    if let Some(ref pi) = info.plusinfo {
                        if pi.aic {
                            let acpp = br.read_bool()?;
                            acpred = ACPredMode::DC;
                            if acpp {
                                acpred = if !br.read_bool()? { ACPredMode::Hor } else { ACPredMode::Ver };
                            }
                        }
                    }
                    let cbpy = br.read_cb(&self.tables.cbpy_cb)?;
                    let cbp = (cbpy << 2) | (cbpc & 3);
                    let dquant = (cbpc & 4) != 0;
                    if dquant {
                        let idx = br.read(2)? as usize;
                        q = (i16::from(q) + i16::from(H263_DQUANT_TAB[idx])) as u8;
                    }
                    let mut binfo = BlockInfo::new(Type::I, cbp, q);
                    binfo.set_acpred(acpred);
                    Ok(binfo)
                },
            Type::P => {
                    if br.read_bool()? { return Ok(BlockInfo::new(Type::Skip, 0, info.get_quant())); }
                    let mut cbpc = br.read_cb(&self.tables.inter_mcbpc_cb)?;
                    while cbpc == 20 { cbpc = br.read_cb(&self.tables.inter_mcbpc_cb)?; }
                    let is_intra = (cbpc & 0x04) != 0;
                    let dquant   = (cbpc & 0x08) != 0;
                    let is_4x4   = (cbpc & 0x10) != 0;
                    if is_intra {
                        let mut acpred = ACPredMode::None;
                        if let Some(ref pi) = info.plusinfo {
                            if pi.aic {
                                let acpp = br.read_bool()?;
                                acpred = ACPredMode::DC;
                                if acpp {
                                    acpred = if !br.read_bool()? { ACPredMode::Hor } else { ACPredMode::Ver };
                                }
                            }
                        }
                        let mut mvec: Vec<MV> = Vec::new();
                        let bbinfo = decode_b_info(br, self.is_pb, self.is_ipb, true)?;
                        let cbpy = br.read_cb(&self.tables.cbpy_cb)?;
                        let cbp = (cbpy << 2) | (cbpc & 3);
                        if dquant {
                            let idx = br.read(2)? as usize;
                            q = (i16::from(q) + i16::from(H263_DQUANT_TAB[idx])) as u8;
                        }
                        let mut binfo = BlockInfo::new(Type::I, cbp, q);
                        binfo.set_bpart(bbinfo);
                        binfo.set_acpred(acpred);
                        if self.is_pb {
                            for _ in 0..bbinfo.get_num_mv() {
                                mvec.push(decode_mv(br, &self.tables.mv_cb)?);
                            }
                            binfo.set_b_mv(mvec.as_slice());
                        }
                        return Ok(binfo);
                    }

                    let bbinfo = decode_b_info(br, self.is_pb, self.is_ipb, false)?;
                    let mut cbpy = br.read_cb(&self.tables.cbpy_cb)?;
//                    if /* !aiv && */(cbpc & 3) != 3 {
                        cbpy ^= 0xF;
//                    }
                    let cbp = (cbpy << 2) | (cbpc & 3);
                    if dquant {
                        let idx = br.read(2)? as usize;
                        q = (i16::from(q) + i16::from(H263_DQUANT_TAB[idx])) as u8;
                    }
                    let mut binfo = BlockInfo::new(Type::P, cbp, q);
                    binfo.set_bpart(bbinfo);
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
                    if self.is_pb {
                        let mut mvec: Vec<MV> = Vec::with_capacity(bbinfo.get_num_mv());
                        for _ in 0..bbinfo.get_num_mv() {
                            let mv = decode_mv(br, &self.tables.mv_cb)?;
                            mvec.push(mv);
                        }
                        binfo.set_b_mv(mvec.as_slice());
                    }
                    Ok(binfo)
                },
            _ => Err(DecoderError::InvalidData),
        }
    }

    #[allow(unused_variables)]
    fn decode_block_intra(&mut self, info: &BlockInfo, _sstate: &SliceState, quant: u8, no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(quant, true, coded, blk, if no < 4 { 0 } else { no - 3 }, info.get_acpred())
    }

    #[allow(unused_variables)]
    fn decode_block_inter(&mut self, info: &BlockInfo, _sstate: &SliceState, quant: u8, no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(quant, false, coded, blk, if no < 4 { 0 } else { no - 3 }, ACPredMode::None)
    }

    fn is_slice_end(&mut self) -> bool { self.br.peek(16) == 0 }
}

impl VivoDecoder {
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

        VivoDecoder{
            info:           NACodecInfo::new_dummy(),
            dec:            H263BaseDecoder::new_with_opts(H263DEC_OPT_SLICE_RESET | H263DEC_OPT_USES_GOB | H263DEC_OPT_PRED_QUANT),
            tables,
            bdsp:           VivoBlockDSP::new(),
            lastframe:      None,
            lastpts:        None,
            width:          0,
            height:         0,
        }
    }
}

impl NADecoder for VivoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            let fmt = formats::YUV420_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.width  = w;
            self.height = h;
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        if src.len() == 0 {
            let buftype;
            let ftype;
            if self.lastframe.is_none() {
                buftype = NABufferType::None;
                ftype = FrameType::Skip;
            } else {
                let mut buf = None;
                std::mem::swap(&mut self.lastframe, &mut buf);
                buftype = buf.unwrap();
                ftype = FrameType::B;
            }
            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), buftype);
            frm.set_keyframe(false);
            frm.set_frame_type(ftype);
            if self.lastpts.is_some() {
                frm.set_pts(self.lastpts);
                self.lastpts = None;
            }
            return Ok(frm.into_ref());
        }
        let mut ibr = VivoBR::new(&src, &self.tables, self.width, self.height);

        let bufinfo = self.dec.parse_frame(&mut ibr, &self.bdsp)?;

        let mut cur_pts = pkt.get_pts();
        if !self.dec.is_intra() {
            let bret = self.dec.get_bframe(&self.bdsp);
            if let Ok(b_buf) = bret {
                self.lastframe = Some(b_buf);
                self.lastpts = pkt.get_pts();
                if let Some(pts) = pkt.get_pts() {
                    cur_pts = Some(pts + 1);
                }
            }
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(self.dec.is_intra());
        frm.set_frame_type(if self.dec.is_intra() { FrameType::I } else { FrameType::P });
        frm.set_pts(cur_pts);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for VivoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(VivoDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::vivo_register_all_decoders;
    use crate::vivo_register_all_demuxers;
    // samples from https://samples.mplayerhq.hu/vivo/
    #[test]
    fn test_vivo1() {
        let mut dmx_reg = RegisteredDemuxers::new();
        vivo_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        vivo_register_all_decoders(&mut dec_reg);

        test_decoding("vivo", "vivo1", "assets/Misc/gr_al.viv", Some(16),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x59ea0837, 0xdf9b5ca3, 0x73cd45c3, 0xaaf2fbf1],
                            [0x5d21c5a0, 0xfb335c4f, 0xe30945e9, 0xec65d54f],
                            [0x46916c45, 0x406f1019, 0xba0105e0, 0xfccd0aab],
                            [0x11006812, 0xb8e8c959, 0x5f69950b, 0x3578db5e],
                            [0xd0b84c1e, 0xad327825, 0x4a98dbbe, 0x48edc280],
                            [0xe1be71a4, 0xc7649922, 0x949496a6, 0xed697d09],
                            [0x0ed9deea, 0x3e9b9784, 0x4836e38b, 0x363f8108],
                            [0x358f2581, 0x95e6b290, 0x35ffde28, 0x003fa23e],
                            [0x482385bd, 0x43bb7cee, 0xf1900758, 0x3b5f1255],
                            [0x91d7549f, 0x96976ef4, 0xb2920f71, 0x71c7a9a6],
                            [0x95736d4f, 0x56748245, 0x5733bd5d, 0x20eacf6d],
                            [0xb5045185, 0x55dbe063, 0xb1c96f24, 0xe5d78296],
                            [0xa88cadf4, 0xedc4a0aa, 0xbb359ed8, 0x1ea28916],
                            [0x4d9f9491, 0x13d29319, 0x57aef355, 0x7e0c9e0b],
                            [0xc3b52284, 0xabb74a4b, 0xf03c9503, 0x834d149e],
                            [0x35fc95d0, 0x0f583ef0, 0xb176f055, 0xb587839a],
                            [0xd5e3b443, 0x3b11b285, 0x78ec3098, 0xe2109aaa]]));
    }
    #[test]
    fn test_vivo2() {
        let mut dmx_reg = RegisteredDemuxers::new();
        vivo_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        vivo_register_all_decoders(&mut dec_reg);

        test_decoding("vivo", "vivo2", "assets/Misc/02-KimagureOrangeRoad.viv", Some(56),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0x73f2afc2, 0x1cc43762, 0xbcfa886d, 0x7e607dd1],
                            [0x45652ef5, 0x7c03046c, 0xcef60a62, 0x4a1e8f24],
                            [0x09f6557e, 0x852cf108, 0x13dbb6ff, 0xaa8667e9],
                            [0x3668d658, 0xfdc37a56, 0x267cb690, 0xa33924ee],
                            [0x0ad13447, 0x26315ca3, 0xfbf43f79, 0xc3951941],
                            [0x8c870462, 0x9135c664, 0x586accfa, 0xfe6d9113],
                            [0x26b08477, 0xcdef5073, 0x86899b3c, 0x0d911f1d],
                            [0x7db88b9c, 0x780729bb, 0xe3195f66, 0xb4f2e7e7],
                            [0x9008ed4e, 0x29b05b6c, 0x3f8a2c23, 0x6d396334],
                            [0xa255e91b, 0x324b764b, 0x265a08ea, 0x0a5a6ae7],
                            [0xf2110e23, 0x8fa53178, 0x675c32cb, 0x63ba9297],
                            [0xbe0bb384, 0xde08646e, 0x45d3530c, 0xd5cdf571],
                            [0xfa19949a, 0xb9e12931, 0xcaa98f06, 0xa284718a],
                            [0x9b1472e5, 0x2d18472c, 0xd5419a58, 0xff16d619],
                            [0xeecd4532, 0xa6301dcc, 0x6eed2b15, 0xa456cae0],
                            [0xb58b2bb8, 0x1c07bd9a, 0x1913e1c3, 0xa1f4fdc2],
                            [0x51b6bef2, 0xd155d675, 0xaba07c15, 0xcf0a673e],
                            [0x3c607ce6, 0x3a203449, 0xdda5b7a0, 0x9119b738],
                            [0x7e7bae10, 0x4beb488e, 0x33db98bb, 0x8b49311e],
                            [0xfff04316, 0xe29afa50, 0xa6f0c921, 0x5b0d29e2],
                            [0xe640595a, 0x98bb55a4, 0x50c0a1e5, 0xe4970206],
                            [0x63873ec7, 0x03a5fd08, 0xe3154f25, 0xb4ed64c2],
                            [0xfa792148, 0x7efebf2c, 0x88d91825, 0x9dce368d],
                            [0x5ed17368, 0x0e056059, 0x2c872282, 0xe5a98237],
                            [0xd7ed6bd6, 0x01414003, 0x5187d790, 0x0af5b029],
                            [0x2991a9c6, 0x8e2dda24, 0xfbf79bbb, 0xbcbfc672],
                            [0xf2d23c64, 0xeb7e6094, 0x70a54901, 0x6d9d0204],
                            [0x54997451, 0xb4edb3c0, 0x5a873e8f, 0xe89d7aa6],
                            [0x94325a5f, 0xe0feb4b3, 0x2b1860bc, 0xc6b6a1a4],
                            [0x8fd428de, 0x17f14f9f, 0x94c8d0da, 0xf9feda90],
                            [0xed90d455, 0x22745327, 0x0ac511f1, 0x2dc2c29e],
                            [0xc50ac6c1, 0xc9eaf8ec, 0x5d9dc7f3, 0x4e55fd09],
                            [0x51325c40, 0xa7c01917, 0xdc064cc6, 0x143badc1],
                            [0x3fc207ee, 0x2c4b0f91, 0x66555499, 0xdf6dc148],
                            [0xd6e7fa33, 0x68ba6ce9, 0x45cac834, 0x1d51b094],
                            [0x1ed541a8, 0x8dd0cc41, 0x149024d4, 0x13777a2e],
                            [0x7fcbd065, 0xb20e068f, 0x9a5e3ed5, 0x6c35dded],
                            [0x07ea66c1, 0xbfadea3b, 0x07bb6580, 0xf40d7d2f],
                            [0x2e0fe75e, 0xa03d3011, 0x539c097d, 0x95fb265c],
                            [0x2f177f09, 0x63b7172a, 0xb0fdd893, 0xcab516cf],
                            [0x7cc5a956, 0xcd1ecff1, 0xf927038c, 0xa0610b48],
                            [0xc0518934, 0xf5c38e4b, 0x7fa255cd, 0x9b054c5b],
                            [0x0551de82, 0x08253d6b, 0x9144ced8, 0xc5adda55],
                            [0xf45735b5, 0x3f2e2111, 0x54d1ff16, 0xe4866157],
                            [0x0aad101e, 0xd37001f4, 0xd5734080, 0xfa4cf449],
                            [0x65318bdd, 0x68809e67, 0x1e27946a, 0x2ca977e5],
                            [0x119cf864, 0x8834473e, 0x721a253e, 0x4e417f19],
                            [0x0d887145, 0xa42a05cc, 0x64a4388e, 0xa05de81d],
                            [0xc184105a, 0x71ac7a8e, 0xaab0dba5, 0x078a331e],
                            [0x078827ea, 0x27670e40, 0x92be9ebb, 0x0fc1f5a0],
                            [0x8c82ca58, 0xbae0a643, 0x845afc5b, 0x15052a84],
                            [0x92065c13, 0x69d3f6b7, 0x7270aab9, 0xb80921fd],
                            [0xa58536b5, 0x6c46d7e1, 0x5f07f282, 0x350900e9],
                            [0x13bf3f52, 0x71f8aa99, 0x97e0aaa5, 0xd6c1cff5],
                            [0xa760cf95, 0xeb33c24c, 0x8dd35c6a, 0x4d81c8b5],
                            [0x50f68b90, 0x41b1f1a0, 0x9847b3ec, 0x1cab5ccc],
                            [0xbd71de9e, 0xc097b4ac, 0x003ab1e5, 0x9394b5bd]]));
    }
}
