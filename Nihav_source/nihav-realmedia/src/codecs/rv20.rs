use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::formats;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::{MV, ZIGZAG};
use nihav_codec_support::codecs::blockdsp;
use nihav_codec_support::codecs::h263::*;
use nihav_codec_support::codecs::h263::code::{H263_INTERP_FUNCS, H263_INTERP_AVG_FUNCS, h263_filter_row};
use nihav_codec_support::codecs::h263::decoder::*;
use nihav_codec_support::codecs::h263::data::*;


#[allow(dead_code)]
struct Tables {
    intra_mcbpc_cb: Codebook<u8>,
    inter_mcbpc_cb: Codebook<u8>,
    mbtype_b_cb:    Codebook<u8>,
    cbpy_cb:        Codebook<u8>,
    cbpc_b_cb:      Codebook<u8>,
    rl_cb:          Codebook<H263RLSym>,
    aic_rl_cb:      Codebook<H263RLSym>,
    mv_cb:          Codebook<u8>,
}

#[derive(Clone,Copy)]
struct RPRInfo {
    present:    bool,
    bits:       u8,
    widths:     [usize; 8],
    heights:    [usize; 8],
}

struct RealVideo20Decoder {
    info:       NACodecInfoRef,
    dec:        H263BaseDecoder,
    tables:     Tables,
    w:          usize,
    h:          usize,
    minor_ver:  u8,
    rpr:        RPRInfo,
    bdsp:       Box<dyn BlockDSP + Send>,
    base_ts:    u64,
    last_ts:    u16,
    next_ts:    u16,
}

struct RealVideo20BR<'a> {
    br:         BitReader<'a>,
    tables:     &'a Tables,
    num_slices: usize,
    slice_no:   usize,
    slice_off:  Vec<u32>,
    w:          usize,
    h:          usize,
    mb_w:       usize,
    mb_h:       usize,
    mb_pos_bits: u8,
    minor_ver:  u8,
    rpr:        RPRInfo,
    pts:        u16,
}

struct RV20SliceInfo {
    ftype:  Type,
    seq:    u32,
    qscale: u8,
    mb_x:   usize,
    mb_y:   usize,
    mb_pos: usize,
    w:      usize,
    h:      usize,
    loop_filter: bool,
}

#[derive(Default)]
struct RV20BlockDSP {}

impl RV20SliceInfo {
    fn new(ftype: Type, seq: u32, qscale: u8, mb_x: usize, mb_y: usize, mb_pos: usize, w: usize, h: usize, loop_filter: bool) -> Self {
        RV20SliceInfo { ftype, seq, qscale, mb_x, mb_y, mb_pos, w, h, loop_filter }
    }
}

macro_rules! idct {
    ($src: expr, $sstep: expr, $dst: expr, $dstep: expr, $bias: expr, $shift: expr, $dtype: tt) => {
        let s0 = $src[0]          as i32;
        let s1 = $src[$sstep]     as i32;
        let s2 = $src[$sstep * 2] as i32;
        let s3 = $src[$sstep * 3] as i32;
        let s4 = $src[$sstep * 4] as i32;
        let s5 = $src[$sstep * 5] as i32;
        let s6 = $src[$sstep * 6] as i32;
        let s7 = $src[$sstep * 7] as i32;

        let t0 = (s0 + s4).wrapping_mul(1448);
        let t1 = (s0 - s4).wrapping_mul(1448);
        let t2 = s2.wrapping_mul(1892) + s6.wrapping_mul(784);
        let t3 = s2.wrapping_mul(784)  - s6.wrapping_mul(1892);
        let t4 = s1.wrapping_mul(2009) + s3.wrapping_mul(1703)
               + s5.wrapping_mul(1138) + s7.wrapping_mul(400);
        let t5 = s1.wrapping_mul(1703) - s3.wrapping_mul(400)
               - s5.wrapping_mul(2009) - s7.wrapping_mul(1138);
        let t6 = s1.wrapping_mul(1138) - s3.wrapping_mul(2009)
               + s5.wrapping_mul(400)  + s7.wrapping_mul(1703);
        let t7 = s1.wrapping_mul(400)  - s3.wrapping_mul(1138)
               + s5.wrapping_mul(1703) - s7.wrapping_mul(2009);

        let t8 = t0 + t2;
        let t9 = t0 - t2;
        let ta = t1 + t3;
        let tb = t1 - t3;

        $dst[0]          = ((t8 + t4 + $bias) >> $shift) as $dtype;
        $dst[$dstep]     = ((ta + t5 + $bias) >> $shift) as $dtype;
        $dst[$dstep * 2] = ((tb + t6 + $bias) >> $shift) as $dtype;
        $dst[$dstep * 3] = ((t9 + t7 + $bias) >> $shift) as $dtype;
        $dst[$dstep * 4] = ((t9 - t7 + $bias) >> $shift) as $dtype;
        $dst[$dstep * 5] = ((tb - t6 + $bias) >> $shift) as $dtype;
        $dst[$dstep * 6] = ((ta - t5 + $bias) >> $shift) as $dtype;
        $dst[$dstep * 7] = ((t8 - t4 + $bias) >> $shift) as $dtype;
    }
}

impl BlockDSP for RV20BlockDSP {
    fn idct(&self, blk: &mut [i16; 64]) {
        let mut tmp = [0i32; 64];
        for (dst, src) in tmp.chunks_mut(8).zip(blk.chunks(8)) {
            idct!(src, 1, dst, 1, 0, 4, i32);
        }
        for i in 0..8 {
            idct!(&tmp[i..], 8, &mut blk[i..], 8, 1 << 19, 20, i16);
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

        for i in 0..4 {
            let xadd = (i & 1) * 8;
            let yadd = (i & 2) * 4;
            let mode = ((mvs[i].x & 1) + (mvs[i].y & 1) * 2) as usize;

            blockdsp::copy_block(&mut dst, src.clone(), 0, xpos + xadd, ypos + yadd, mvs[i].x >> 1, mvs[i].y >> 1, 8, 8, 0, 1, mode, H263_INTERP_FUNCS);
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

        for i in 0..4 {
            let xadd = (i & 1) * 8;
            let yadd = (i & 2) * 4;
            let mode = ((mvs[i].x & 1) + (mvs[i].y & 1) * 2) as usize;

            blockdsp::copy_block(&mut dst, src.clone(), 0, xpos + xadd, ypos + yadd, mvs[i].x >> 1, mvs[i].y >> 1, 8, 8, 0, 1, mode, H263_INTERP_AVG_FUNCS);
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
        h263_filter_row(buf, mb_y, mb_w, cbpi)
    }
}

fn get_mb_pos_bits(mb_w: usize, mb_h: usize) -> u8 {
    let max_pos = mb_w * mb_h - 1;
    for i in 0..H263_MBB.len() {
        if max_pos <= H263_MBB[i].blocks {
            return H263_MBB[i].bits;
        }
    }
    0
}

impl<'a> RealVideo20BR<'a> {
    fn new(src: &'a [u8], tables: &'a Tables, width: usize, height: usize, minor_ver: u8, rpr: RPRInfo) -> Self {
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
        let mb_w = (width  + 15) >> 4;
        let mb_h = (height + 15) >> 4;
        RealVideo20BR {
            br:         BitReader::new(&src[soff..], BitReaderMode::BE),
            tables,
            num_slices: nslices,
            slice_no:   0,
            slice_off:  slice_offs,
            w:          width,
            h:          height,
            mb_w,
            mb_h,
            mb_pos_bits: get_mb_pos_bits(mb_w, mb_h),
            minor_ver,
            rpr,
            pts:        0,
        }
    }

#[allow(unused_variables)]
    fn decode_block(&mut self, sstate: &SliceState, quant: u8, intra: bool, coded: bool, blk: &mut [i16; 64], plane_no: usize, acpred: ACPredMode) -> DecoderResult<()> {
        let br = &mut self.br;
        let mut idx = 0;
        if !sstate.is_iframe && intra {
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

        let rl_cb = if sstate.is_iframe { &self.tables.aic_rl_cb } else { &self.tables.rl_cb };
        let q = if plane_no == 0 { (quant * 2) as i16 } else { (H263_CHROMA_QUANT[quant as usize] * 2) as i16 };
        let q_add = if q == 0 || sstate.is_iframe { 0i16 } else { ((q >> 1) - 1) | 1 };
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
            let oidx = scan[idx as usize];
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

fn read_dquant(br: &mut BitReader, q: u8) -> DecoderResult<u8> {
    if br.read_bool()? {
        Ok(H263_MODIFIED_QUANT[br.read(1)? as usize][q as usize])
    } else {
        Ok(br.read(5)? as u8)
    }
}

impl<'a> BlockDecoder for RealVideo20BR<'a> {

#[allow(unused_variables)]
    fn decode_pichdr(&mut self) -> DecoderResult<PicInfo> {
        self.slice_no = 0;
        let shdr = self.read_slice_header()?;
//        self.slice_no += 1;
        validate!((shdr.mb_x == 0) && (shdr.mb_y == 0));
/*        let mb_count;
        if self.slice_no < self.num_slices {
            let pos = self.br.tell();
            let shdr2 = self.read_slice_header()?;
            self.br.seek(pos as u32)?;
            mb_count = shdr2.mb_pos - shdr.mb_pos;
        } else {
            mb_count = self.mb_w * self.mb_h;
        }*/

        let plusinfo = Some(PlusInfo::new(shdr.ftype == Type::I, shdr.loop_filter, false, false));
        let picinfo = PicInfo::new(shdr.w, shdr.h, shdr.ftype, MVMode::Long, false, false, shdr.qscale, shdr.seq as u16, None, plusinfo);
        Ok(picinfo)
    }

    #[allow(unused_variables)]
    fn decode_slice_header(&mut self, info: &PicInfo) -> DecoderResult<SliceInfo> {
        let shdr = self.read_slice_header()?;
        self.slice_no += 1;
        let mb_count;
        if self.slice_no < self.num_slices {
            let pos = self.br.tell();
            let shdr2 = self.read_slice_header()?;
            validate!(shdr2.mb_pos > shdr.mb_pos);
            validate!(shdr2.w == shdr.w && shdr2.h == shdr.h);
            mb_count = shdr2.mb_pos - shdr.mb_pos;
            self.br.seek(pos as u32)?;
        } else {
            mb_count = self.mb_w * self.mb_h - shdr.mb_pos;
        }
        let ret = SliceInfo::new(shdr.mb_x, shdr.mb_y, shdr.mb_pos + mb_count, shdr.qscale);

        Ok(ret)
    }

    fn decode_block_header(&mut self, info: &PicInfo, _slice: &SliceInfo, sstate: &SliceState) -> DecoderResult<BlockInfo> {
        let br = &mut self.br;
        let mut q = sstate.quant;
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
                                acpred = if br.read_bool()? { ACPredMode::Hor } else { ACPredMode::Ver };
                            }
                        }
                    }
                    let cbpy = br.read_cb(&self.tables.cbpy_cb)?;
                    let cbp = (cbpy << 2) | (cbpc & 3);
                    let dquant = (cbpc & 4) != 0;
                    if dquant {
                        q = read_dquant(br, q)?;
                    }
                    let mut binfo = BlockInfo::new(Type::I, cbp, q);
                    binfo.set_acpred(acpred);
                    Ok(binfo)
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
                            q = read_dquant(br, q)?;
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
                        q = read_dquant(br, q)?;
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
            Type::B => { // B
                    let mut mbtype = br.read_cb(&self.tables.mbtype_b_cb)? as usize;
                    while mbtype == 14 { mbtype = br.read_cb(&self.tables.mbtype_b_cb)? as usize; }

                    let is_coded  = (H263_MBTYPE_B_CAPS[mbtype] & H263_MBB_CAP_CODED) != 0;
                    let is_intra  = (H263_MBTYPE_B_CAPS[mbtype] & H263_MBB_CAP_INTRA) != 0;
                    let dquant    = (H263_MBTYPE_B_CAPS[mbtype] & H263_MBB_CAP_DQUANT) != 0;
                    let is_fwd    = (H263_MBTYPE_B_CAPS[mbtype] & H263_MBB_CAP_FORWARD) != 0;
                    let is_bwd    = (H263_MBTYPE_B_CAPS[mbtype] & H263_MBB_CAP_BACKWARD) != 0;

                    let cbp = if is_coded {
                            let cbpc = br.read_cb(&self.tables.cbpc_b_cb)?;
                            let mut cbpy = br.read_cb(&self.tables.cbpy_cb)?;
                            if !is_intra { cbpy ^= 0xF; }
                            (cbpy << 2) | (cbpc & 3)
                        } else { 0 };

                    if dquant {
                        q = read_dquant(br, q)?;
                    }

                    if is_intra {
                        let binfo = BlockInfo::new(Type::I, cbp, q);
                        return Ok(binfo);
                    }

                    let mut binfo = BlockInfo::new(Type::B, cbp, q);
                    if is_fwd {
                        let mvec: [MV; 1] = [decode_mv(br, &self.tables.mv_cb)?];
                        binfo.set_mv(&mvec);
                    }
                    if is_bwd {
                        let mvec: [MV; 1] = [decode_mv(br, &self.tables.mv_cb)?];
                        binfo.set_b_mv(&mvec);
                    }
                    Ok(binfo)
                },
            _ => { unreachable!(); },
        }
    }

    fn decode_block_intra(&mut self, info: &BlockInfo, sstate: &SliceState, quant: u8, no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(sstate, quant, true, coded, blk, if no < 4 { 0 } else { no - 3 }, info.get_acpred())
    }

    #[allow(unused_variables)]
    fn decode_block_inter(&mut self, info: &BlockInfo, sstate: &SliceState, quant: u8, no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(sstate, quant, false, coded, blk, if no < 4 { 0 } else { no - 3 }, ACPredMode::None)
    }

    fn is_slice_end(&mut self) -> bool { false }
}

impl<'a> RealVideo20BR<'a> {
#[allow(unused_variables)]
    fn read_slice_header(&mut self) -> DecoderResult<RV20SliceInfo> {
        validate!(self.slice_no < self.num_slices);

        let br = &mut self.br;
        br.seek(self.slice_off[self.slice_no] * 8)?;

        let frm_type    = br.read(2)?;
        let ftype = match frm_type {
                0 | 1 => { Type::I },
                2     => { Type::P },
                _     => { Type::B },
            };

        let marker      = br.read(1)?;
        validate!(marker == 0);
        let qscale      = br.read(5)? as u8;
        validate!(qscale > 0);
        let loop_filter;
        if self.minor_ver >= 2 {
            loop_filter = br.read_bool()?;
        } else {
            loop_filter = false;
        }
        let seq = if self.minor_ver <= 1 {
                br.read(8)?  << 8
            } else {
                br.read(13)? << 3
            };
        self.pts = seq as u16;
        let w;
        let h;
        if self.rpr.present {
            let rpr = br.read(self.rpr.bits)? as usize;
            if rpr == 0 {
                w = self.w;
                h = self.h;
            } else {
                w = self.rpr.widths[rpr - 1];
                h = self.rpr.heights[rpr - 1];
                validate!((w != 0) && (h != 0));
            }
            self.mb_w = (w + 15) >> 4;
            self.mb_h = (h + 15) >> 4;
            self.mb_pos_bits = get_mb_pos_bits(self.mb_w, self.mb_h);
        } else {
            w = self.w;
            h = self.h;
        }

        let mb_pos = br.read(self.mb_pos_bits)? as usize;
        let mb_x = mb_pos % self.mb_w;
        let mb_y = mb_pos / self.mb_w;

        br.skip(1)?; // no rounding

        if (self.minor_ver <= 1) && (frm_type == 3) {
            br.skip(5)?;
        }

        Ok(RV20SliceInfo::new(ftype, seq, qscale, mb_x, mb_y, mb_pos, w, h, loop_filter))
    }
}

impl RealVideo20Decoder {
    fn new() -> Self {
        let mut coderead = H263ShortCodeReader::new(H263_INTRA_MCBPC);
        let intra_mcbpc_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263ShortCodeReader::new(H263_INTER_MCBPC);
        let inter_mcbpc_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263ShortCodeReader::new(H263_MBTYPE_B);
        let mbtype_b_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263ShortCodeReader::new(H263_CBPY);
        let cbpy_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263ShortCodeReader::new(H263_CBPC_B);
        let cbpc_b_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263RLCodeReader::new(H263_RL_CODES);
        let rl_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263RLCodeReader::new(H263_RL_CODES_AIC);
        let aic_rl_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();
        let mut coderead = H263ShortCodeReader::new(H263_MV);
        let mv_cb = Codebook::new(&mut coderead, CodebookMode::MSB).unwrap();

        let tables = Tables {
            intra_mcbpc_cb,
            inter_mcbpc_cb,
            mbtype_b_cb,
            cbpy_cb,
            cbpc_b_cb,
            rl_cb,
            aic_rl_cb,
            mv_cb,
        };

        RealVideo20Decoder{
            info:           NACodecInfoRef::default(),
            dec:            H263BaseDecoder::new_b_frames(false),
            tables,
            w:              0,
            h:              0,
            minor_ver:      0,
            rpr:            RPRInfo { present: false, bits: 0, widths: [0; 8], heights: [0; 8] },
            bdsp:           Box::<RV20BlockDSP>::default(),
            base_ts:        0,
            last_ts:        0,
            next_ts:        0,
        }
    }
}

impl NADecoder for RealVideo20Decoder {
#[allow(unused_variables)]
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
            let min_ver = (ver >> 8) & 0xFF;
            let mic_ver = ver & 0xFF;
            validate!(maj_ver == 2);
            self.minor_ver = min_ver as u8;
            let rprb = src[1] & 7;
            if rprb == 0 {
                self.rpr.present = false;
            } else {
                self.rpr.present = true;
                self.rpr.bits    = ((rprb >> 1) + 1).min(3);
                let num_dim = ((src.len() - 8) / 2).min(self.rpr.widths.len() - 1);
                for i in 0..num_dim {
                    self.rpr.widths [i] = (src[i * 2 + 8] as usize) * 4;
                    self.rpr.heights[i] = (src[i * 2 + 9] as usize) * 4;
                }
            }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        let mut ibr = RealVideo20BR::new(&src, &self.tables, self.w, self.h, self.minor_ver, self.rpr);

        let bufinfo = self.dec.parse_frame(&mut ibr, self.bdsp.as_ref())?;

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        let ftype = self.dec.get_frame_type();
        let pts = ibr.pts;
        if ftype != FrameType::B {
            self.last_ts = self.next_ts;
            self.next_ts = pts;
            if self.last_ts > self.next_ts {
                self.base_ts += 1 << 16;
            }
        }
        let ts_diff = self.next_ts.wrapping_sub(pts);
        let ts = self.base_ts + (self.next_ts as u64) - (ts_diff as u64);
        frm.set_keyframe(self.dec.is_intra());
        frm.set_frame_type(ftype);
        frm.set_pts(Some(ts >> 3));
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.dec.flush();
    }
}

impl NAOptionHandler for RealVideo20Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

struct MBB { blocks: usize, bits: u8 }
const H263_MBB: &[MBB; 7] = &[
    MBB{ blocks:    47, bits:  6 },
    MBB{ blocks:    98, bits:  7 },
    MBB{ blocks:   395, bits:  9 },
    MBB{ blocks:  1583, bits: 11 },
    MBB{ blocks:  6335, bits: 13 },
    MBB{ blocks:  9215, bits: 14 },
    MBB{ blocks: 65536, bits: 14 },
];

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(RealVideo20Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::realmedia_register_all_decoders;
    use crate::realmedia_register_all_demuxers;
    #[test]
    fn test_rv20() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        realmedia_register_all_decoders(&mut dec_reg);

        // sample from a private collection
        test_decoding("realmedia", "realvideo2",
                      "assets/RV/rv20_svt_atrc_640x352_realproducer_plus_8.51.rm",
                      Some(1000), &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x8ce88686, 0x03ca3bb9, 0x0d18347b, 0xccdb0bc5],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0x319d142d, 0x607a7c28, 0x526a2794, 0xa6e7864f],
                        [0xa2008d4c, 0xf4684b3a, 0xecd0526c, 0xf0742a77],
                        [0x5eef6cfd, 0x4eb49f19, 0x4d760b7a, 0x741ccd0c],
                        [0x0e0529df, 0xf1cc3f03, 0x03986b0d, 0xd2033c08],
                        [0xdb9aa091, 0xf2c6345c, 0xde9deae8, 0x71f51a67],
                        [0x22c978cf, 0x6887a9ba, 0xe74c9316, 0x8cbdd29b],
                        [0x7c83c784, 0x15b20881, 0x74798f24, 0x2096573c],
                        [0x84a68aed, 0x4cfcefb1, 0x78d1b66b, 0x21b1860a],
                        [0x78ed094a, 0x8df72434, 0x58bcd64d, 0x8d725dfd]]));
    }
}
