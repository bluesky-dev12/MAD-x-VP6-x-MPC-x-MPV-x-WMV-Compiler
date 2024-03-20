use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::formats;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_codec_support::codecs::{MV, ZIGZAG};
use nihav_codec_support::codecs::blockdsp;
use nihav_codec_support::codecs::h263::*;
use nihav_codec_support::codecs::h263::code::{H263_INTERP_FUNCS, H263_INTERP_AVG_FUNCS};
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

#[derive(Default)]
struct I263BlockDSP {}

struct Intel263Decoder {
    info:    NACodecInfoRef,
    dec:     H263BaseDecoder,
    tables:  Tables,
    bdsp:    I263BlockDSP,
    lastframe:  Option<NABufferType>,
    lastpts:    Option<u64>,
}

struct Intel263BR<'a> {
    br:     BitReader<'a>,
    tables: &'a Tables,
    gob_no: usize,
    mb_w:   usize,
    is_pb:  bool,
    is_ipb: bool,
}

const W1: i32 = 2841;
const W2: i32 = 2676;
const W3: i32 = 2408;
const W5: i32 = 1609;
const W6: i32 = 1108;
const W7: i32 =  565;
const W8: i32 =  181;

const ROW_SHIFT: u8 = 8;
const COL_SHIFT: u8 = 14;

#[allow(clippy::erasing_op)]
fn idct_row(row: &mut [i16]) {
    let in0 = ((i32::from(row[0])) << 11) + (1 << (ROW_SHIFT - 1));
    let in1 =  (i32::from(row[4])) << 11;
    let in2 =   i32::from(row[6]);
    let in3 =   i32::from(row[2]);
    let in4 =   i32::from(row[1]);
    let in5 =   i32::from(row[7]);
    let in6 =   i32::from(row[5]);
    let in7 =   i32::from(row[3]);

    let tmp = W7 * (in4 + in5);
    let a4 = tmp + (W1 - W7) * in4;
    let a5 = tmp - (W1 + W7) * in5;

    let tmp = W3 * (in6 + in7);
    let a6 = tmp - (W3 - W5) * in6;
    let a7 = tmp - (W3 + W5) * in7;

    let tmp = in0 + in1;

    let a0 = in0 - in1;
    let t1 = W6 * (in2 + in3);
    let a2 = t1 - (W2 + W6) * in2;
    let a3 = t1 + (W2 - W6) * in3;
    let b1 = a4 + a6;

    let b4 = a4 - a6;
    let t2 = a5 - a7;
    let b6 = a5 + a7;
    let b7 = tmp + a3;
    let b5 = tmp - a3;
    let b3 = a0 + a2;
    let b0 = a0 - a2;
    let b2 = (W8 * (b4 + t2) + 128) >> 8;
    let b4 = (W8 * (b4 - t2) + 128) >> 8;

    row[0] = ((b7 + b1) >> ROW_SHIFT) as i16;
    row[7] = ((b7 - b1) >> ROW_SHIFT) as i16;
    row[1] = ((b3 + b2) >> ROW_SHIFT) as i16;
    row[6] = ((b3 - b2) >> ROW_SHIFT) as i16;
    row[2] = ((b0 + b4) >> ROW_SHIFT) as i16;
    row[5] = ((b0 - b4) >> ROW_SHIFT) as i16;
    row[3] = ((b5 + b6) >> ROW_SHIFT) as i16;
    row[4] = ((b5 - b6) >> ROW_SHIFT) as i16;
}

#[allow(clippy::erasing_op)]
fn idct_col(blk: &mut [i16; 64], off: usize) {
    let in0 = ((i32::from(blk[off + 0*8])) << 8) + (1 << (COL_SHIFT - 1));
    let in1 =  (i32::from(blk[off + 4*8])) << 8;
    let in2 =   i32::from(blk[off + 6*8]);
    let in3 =   i32::from(blk[off + 2*8]);
    let in4 =   i32::from(blk[off + 1*8]);
    let in5 =   i32::from(blk[off + 7*8]);
    let in6 =   i32::from(blk[off + 5*8]);
    let in7 =   i32::from(blk[off + 3*8]);

    let tmp = W7 * (in4 + in5);
    let a4 = (tmp + (W1 - W7) * in4) >> 3;
    let a5 = (tmp - (W1 + W7) * in5) >> 3;

    let tmp = W3 * (in6 + in7);
    let a6 = (tmp - (W3 - W5) * in6) >> 3;
    let a7 = (tmp - (W3 + W5) * in7) >> 3;

    let tmp = in0 + in1;

    let a0 = in0 - in1;
    let t1 = W6 * (in2 + in3);
    let a2 = (t1 - (W2 + W6) * in2) >> 3;
    let a3 = (t1 + (W2 - W6) * in3) >> 3;
    let b1 = a4 + a6;

    let b4 = a4 - a6;
    let t2 = a5 - a7;
    let b6 = a5 + a7;
    let b7 = tmp + a3;
    let b5 = tmp - a3;
    let b3 = a0 + a2;
    let b0 = a0 - a2;
    let b2 = (W8 * (b4 + t2) + 128) >> 8;
    let b4 = (W8 * (b4 - t2) + 128) >> 8;

    blk[off + 0*8] = ((b7 + b1) >> COL_SHIFT) as i16;
    blk[off + 7*8] = ((b7 - b1) >> COL_SHIFT) as i16;
    blk[off + 1*8] = ((b3 + b2) >> COL_SHIFT) as i16;
    blk[off + 6*8] = ((b3 - b2) >> COL_SHIFT) as i16;
    blk[off + 2*8] = ((b0 + b4) >> COL_SHIFT) as i16;
    blk[off + 5*8] = ((b0 - b4) >> COL_SHIFT) as i16;
    blk[off + 3*8] = ((b5 + b6) >> COL_SHIFT) as i16;
    blk[off + 4*8] = ((b5 - b6) >> COL_SHIFT) as i16;
}

const FILTER_STRENGTH: [u8; 32] = [
    1,  1,  2,  2,  3,  3,  4,  4,  4,  5,  5,  5,  6,  6,  7,  7,
    7,  8,  8,  8,  9,  9,  9, 10, 10, 10, 11, 11, 11, 12, 12, 12
];

#[allow(clippy::erasing_op)]
fn deblock_hor(buf: &mut NAVideoBuffer<u8>, comp: usize, strength: u8, off: usize) {
    let stride = buf.get_stride(comp);
    let dptr = buf.get_data_mut().unwrap();
    let buf = dptr.as_mut_slice();
    for x in 0..8 {
        let a = i16::from(buf[off - 2 * stride + x]);
        let b = i16::from(buf[off - 1 * stride + x]);
        let c = i16::from(buf[off + 0 * stride + x]);
        let d = i16::from(buf[off + 1 * stride + x]);
        let diff = (3 * (a - d) + 8 * (c - b)) / 16;
        if (diff != 0) && (diff > -24) && (diff < 24) {
            let d1a = (diff.abs() - 2 * (diff.abs() - i16::from(strength)).max(0)).max(0);
            let d1  = if diff < 0 { -d1a } else { d1a };

            buf[off - 1 * stride + x] = (b + d1).max(0).min(255) as u8;
            buf[off + 0 * stride + x] = (c - d1).max(0).min(255) as u8;
        }
    }
}

fn deblock_ver(buf: &mut NAVideoBuffer<u8>, comp: usize, strength: u8, off: usize) {
    let stride = buf.get_stride(comp);
    let dptr = buf.get_data_mut().unwrap();
    let buf = dptr.as_mut_slice();
    for y in 0..8 {
        let a = i16::from(buf[off - 2 + y * stride]);
        let b = i16::from(buf[off - 1 + y * stride]);
        let c = i16::from(buf[off + 0 + y * stride]);
        let d = i16::from(buf[off + 1 + y * stride]);
        let diff = (3 * (a - d) + 8 * (c - b)) / 16;
        if (diff != 0) && (diff > -24) && (diff < 24) {
            let d1a = (diff.abs() - 2 * (diff.abs() - i16::from(strength)).max(0)).max(0);
            let d1  = if diff < 0 { -d1a } else { d1a };

            buf[off - 1 + y * stride] = (b + d1).max(0).min(255) as u8;
            buf[off     + y * stride] = (c - d1).max(0).min(255) as u8;
        }
    }
}

impl BlockDSP for I263BlockDSP {
    fn idct(&self, blk: &mut [i16; 64]) {
        for i in 0..8 { idct_row(&mut blk[i*8..(i+1)*8]); }
        for i in 0..8 { idct_col(blk, i); }
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
        let stride  = buf.get_stride(0);
        let mut off = buf.get_offset(0) + mb_y * 16 * stride;
        for mb_x in 0..mb_w {
            let coff = off;
            let coded0 = cbpi.is_coded(mb_x, 0);
            let coded1 = cbpi.is_coded(mb_x, 1);
            let q = cbpi.get_q(mb_w + mb_x);
            let strength = if q < 32 { FILTER_STRENGTH[q as usize] } else { 0 };
            if mb_y != 0 {
                if coded0 && cbpi.is_coded_top(mb_x, 0) { deblock_hor(buf, 0, strength, coff); }
                if coded1 && cbpi.is_coded_top(mb_x, 1) { deblock_hor(buf, 0, strength, coff + 8); }
            }
            let coff = off + 8 * stride;
            if cbpi.is_coded(mb_x, 2) && coded0 { deblock_hor(buf, 0, q, coff); }
            if cbpi.is_coded(mb_x, 3) && coded1 { deblock_hor(buf, 0, q, coff + 8); }
            off += 16;
        }
        let mut leftt = false;
        let mut leftc = false;
        let mut off = buf.get_offset(0) + mb_y * 16 * stride;
        for mb_x in 0..mb_w {
            let ctop0 = cbpi.is_coded_top(mb_x, 0);
            let ctop1 = cbpi.is_coded_top(mb_x, 0);
            let ccur0 = cbpi.is_coded(mb_x, 0);
            let ccur1 = cbpi.is_coded(mb_x, 1);
            let q = cbpi.get_q(mb_w + mb_x);
            let strength = if q < 32 { FILTER_STRENGTH[q as usize] } else { 0 };
            if mb_y != 0 {
                let coff = off - 8 * stride;
                let qtop = cbpi.get_q(mb_x);
                let strtop = if qtop < 32 { FILTER_STRENGTH[qtop as usize] } else { 0 };
                if leftt && ctop0 { deblock_ver(buf, 0, strtop, coff); }
                if ctop0 && ctop1 { deblock_ver(buf, 0, strtop, coff + 8); }
            }
            if leftc && ccur0 { deblock_ver(buf, 0, strength, off); }
            if ccur0 && ccur1 { deblock_ver(buf, 0, strength, off + 8); }
            leftt = ctop1;
            leftc = ccur1;
            off += 16;
        }
        let strideu = buf.get_stride(1);
        let stridev = buf.get_stride(2);
        let offu = buf.get_offset(1) + mb_y * 8 * strideu;
        let offv = buf.get_offset(2) + mb_y * 8 * stridev;
        if mb_y != 0 {
            for mb_x in 0..mb_w {
                let ctu = cbpi.is_coded_top(mb_x, 4);
                let ccu = cbpi.is_coded(mb_x, 4);
                let ctv = cbpi.is_coded_top(mb_x, 5);
                let ccv = cbpi.is_coded(mb_x, 5);
                let q = cbpi.get_q(mb_w + mb_x);
                let strength = if q < 32 { FILTER_STRENGTH[q as usize] } else { 0 };
                if ctu && ccu { deblock_hor(buf, 1, strength, offu + mb_x * 8); }
                if ctv && ccv { deblock_hor(buf, 2, strength, offv + mb_x * 8); }
            }
            let mut leftu = false;
            let mut leftv = false;
            let offu = buf.get_offset(1) + (mb_y - 1) * 8 * strideu;
            let offv = buf.get_offset(2) + (mb_y - 1) * 8 * stridev;
            for mb_x in 0..mb_w {
                let ctu = cbpi.is_coded_top(mb_x, 4);
                let ctv = cbpi.is_coded_top(mb_x, 5);
                let qt = cbpi.get_q(mb_x);
                let strt = if qt < 32 { FILTER_STRENGTH[qt as usize] } else { 0 };
                if leftu && ctu { deblock_ver(buf, 1, strt, offu + mb_x * 8); }
                if leftv && ctv { deblock_ver(buf, 2, strt, offv + mb_x * 8); }
                leftu = ctu;
                leftv = ctv;
            }
        }
    }
}

fn check_marker(br: &mut BitReader) -> DecoderResult<()> {
    let mark = br.read(1)?;
    validate!(mark == 1);
    Ok(())
}

impl<'a> Intel263BR<'a> {
    fn new(src: &'a [u8], tables: &'a Tables) -> Self {
        Intel263BR {
            br:     BitReader::new(src, BitReaderMode::BE),
            tables,
            gob_no: 0,
            mb_w:   0,
            is_pb:  false,
            is_ipb: false,
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
            } else {
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

impl<'a> BlockDecoder for Intel263BR<'a> {

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
        validate!((sfmt != 0b000) && (sfmt != 0b110));
        let is_intra = !br.read_bool()?;
        let umv = br.read_bool()?;
        br.read(1)?; // syntax arithmetic coding
        let apm = br.read_bool()?;
        self.is_pb = br.read_bool()?;
        let deblock;
        let pbplus;
        if sfmt == 0b111 {
            sfmt = br.read(3)?;
            validate!((sfmt != 0b000) && (sfmt != 0b111));
            br.read(2)?; // unknown flags
            deblock = br.read_bool()?;
            br.read(1)?; // unknown flag
            pbplus = br.read_bool()?;
            br.read(5)?; // unknown flags
            let marker = br.read(5)?;
            validate!(marker == 1);
        } else {
            deblock = false;
            pbplus = false;
        }
        self.is_ipb = pbplus;
        let w; let h;
        if sfmt == 0b110 {
            let par = br.read(4)?;
            w = ((br.read(9)? + 1) * 4) as usize;
            check_marker(br)?;
            h = ((br.read(9)? + 1) * 4) as usize;
            if par == 0b1111 {
                let pixw = br.read(8)?;
                let pixh = br.read(8)?;
                validate!((pixw != 0) && (pixh != 0));
            }
        } else {
            let (w_, h_) = H263_SIZES[sfmt as usize];
            w = w_;
            h = h_;
        }
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
//println!("frame {}x{} intra: {} q {} pb {} apm {} umv {} @{}", w, h, is_intra, quant, self.is_pb, apm, umv, br.tell());
        self.gob_no = 0;
        self.mb_w = (w + 15) >> 4;

        let ftype = if is_intra { Type::I } else { Type::P };
        let plusinfo = if deblock { Some(PlusInfo::new(false, deblock, false, false)) } else { None };
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
//println!("GOB gn {:X} id {} q {}", gn, gfid, gquant);
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
                    let cbpy = br.read_cb(&self.tables.cbpy_cb)?;
                    let cbp = (cbpy << 2) | (cbpc & 3);
                    let dquant = (cbpc & 4) != 0;
                    if dquant {
                        let idx = br.read(2)? as usize;
                        q = (i16::from(q) + i16::from(H263_DQUANT_TAB[idx])) as u8;
                    }
                    Ok(BlockInfo::new(Type::I, cbp, q))
                },
            Type::P => {
                    if br.read_bool()? { return Ok(BlockInfo::new(Type::Skip, 0, info.get_quant())); }
                    let mut cbpc = br.read_cb(&self.tables.inter_mcbpc_cb)?;
                    while cbpc == 20 { cbpc = br.read_cb(&self.tables.inter_mcbpc_cb)?; }
                    let is_intra = (cbpc & 0x04) != 0;
                    let dquant   = (cbpc & 0x08) != 0;
                    let is_4x4   = (cbpc & 0x10) != 0;
                    if is_intra {
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
            _ => { Err(DecoderError::InvalidData) },
        }
    }

    #[allow(unused_variables)]
    fn decode_block_intra(&mut self, info: &BlockInfo, sstate: &SliceState, quant: u8, no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(quant, true, coded, blk)
    }

    #[allow(unused_variables)]
    fn decode_block_inter(&mut self, info: &BlockInfo, sstate: &SliceState, quant: u8, no: usize, coded: bool, blk: &mut [i16; 64]) -> DecoderResult<()> {
        self.decode_block(quant, false, coded, blk)
    }

    fn is_slice_end(&mut self) -> bool { self.br.peek(16) == 0 }
}

impl Intel263Decoder {
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

        Intel263Decoder{
            info:           NACodecInfo::new_dummy(),
            dec:            H263BaseDecoder::new(true),
            tables,
            bdsp:           I263BlockDSP::default(),
            lastframe:      None,
            lastpts:        None,
        }
    }
}

impl NADecoder for Intel263Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            let fmt = formats::YUV420_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();

        if src.len() == 8 {
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
        let mut ibr = Intel263BR::new(&src, &self.tables);

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

impl NAOptionHandler for Intel263Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(Intel263Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::indeo_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_intel263() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        indeo_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/IMC/neal73_saber.avi
        test_decoding("avi", "intel263", "assets/Indeo/neal73_saber.avi", Some(16),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                        [0x698c4f70, 0xf727bfc1, 0x96e687e9, 0xc9e37073],
                        [0xd41d8cd9, 0x8f00b204, 0xe9800998, 0xecf8427e],
                        [0x95dfe457, 0xaaeeaca9, 0x9764c111, 0xdf055b1f],
                        [0xac1d708c, 0x8e34aa47, 0x240b8f0e, 0x797b052b],
                        [0x965fe621, 0xebb049da, 0x18345724, 0x748ea32f],
                        [0x126c7492, 0x54d7457f, 0x9968a723, 0x89629378],
                        [0x8c690125, 0x3de8da89, 0x6030b702, 0xbd3f09ab],
                        [0xa9d3f7c7, 0xdfa1795c, 0x7ed34e86, 0x58b7cc26],
                        [0xe500e50e, 0x2312197d, 0xb8e93f41, 0xe6890cd8],
                        [0x2e8d8f15, 0xaf1c84fe, 0x05fec093, 0x3c383abb],
                        [0x6a1def4b, 0xc3549acc, 0x9ed127be, 0x2872f751],
                        [0x36599508, 0xe169caf9, 0xcdf6af6b, 0x29d167b8],
                        [0xfe98869d, 0x2b16b94b, 0x97caaf72, 0xbf7cc0c1],
                        [0x9fbfaf0a, 0xfa4ce8fc, 0xdc038ab8, 0x649c1eaa],
                        [0x141749be, 0xfba7acd4, 0xd0372e02, 0x6b191bc5],
                        [0x99252b73, 0x2ce009d9, 0xf6753c1d, 0x31892a08],
                        [0xefe81436, 0x4ab365db, 0x57a0b058, 0x26a6ca02]]));
    }
}
