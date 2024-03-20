use nihav_core::frame::*;
use nihav_codec_support::codecs::MV;
use super::super::SimpleFrame;

macro_rules! module_selector {
    ($( ($cond:meta, $module:ident) ),*) => {
        module_selector!(list; r#false; $(($cond, $module)),*);
    };
    (list; $nocond:meta; ($ccar:meta, $carmod:ident), $(($condcdr:meta, $cdrmod:ident)),*) => {
        module_selector!(single; $nocond; $ccar; $carmod);
        module_selector!(list; any($nocond, $ccar); $(($condcdr, $cdrmod)),*);
    };
    (list; $nocond:meta; ($yescond:meta, $module:ident)) => {
        module_selector!(single; $nocond; $yescond; $module);
    };
    (list; $_:meta; ) => {};
    (single; $nocond:meta; $yescond:meta; $module:ident) => {
        #[cfg(all(not($nocond), $yescond))]
        mod $module;
        #[cfg(all(not($nocond), $yescond))]
        use $module::*;
    };
}

module_selector! (
    (all(feature = "simd", target_arch = "x86_64"), x86),
    (debug_assertions, debug),
    (not(debug_assertions), release)
);

type MCFunc = fn (dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, h: usize);

fn clip_u8(val: i16) -> u8 { val.max(0).min(255) as u8 }

trait RegisterSIMD {
    fn register_simd(&mut self);
}

#[repr(align(16))]
pub struct McBlock {
    pub y:  [u8; 16 * 16],
    pub u:  [u8; 16 * 16],
    pub v:  [u8; 16 * 16],
}

impl McBlock {
    pub fn new() -> Self {
        unsafe {
            let blk = std::mem::MaybeUninit::uninit();
            blk.assume_init()
        }
    }
}

#[allow(clippy::type_complexity)]
pub struct H264MC {
    avg_buf:    NAVideoBufferRef<u8>,
    pub put_block_weighted:     [fn (dst: &mut [u8], stride: usize, src: &[u8], h: usize, wparams: [i8; 3]); 4],
    pub put_block_weighted2:    [fn (dst: &mut [u8], stride: usize, src0: &[u8], src1: &[u8], h: usize, wparams: [i8; 5]); 4],
    pub chroma_interp:          [fn (dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, dx: u16, dy: u16, h: usize); 3],
    avg:        [fn (dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bh: usize); 4],

    width:      usize,
    height:     usize,
}

impl H264MC {
    pub fn new(avg_buf: NAVideoBufferRef<u8>) -> Self {
        let mut obj = Self {
            avg_buf,
            put_block_weighted:     [put_blk_w_2, put_blk_w_4, put_blk_w_8, put_blk_w_16],
            put_block_weighted2:    [put_blk_w2_2, put_blk_w2_4, put_blk_w2_8, put_blk_w2_16],
            chroma_interp:          [chroma_interp_2, chroma_interp_4, chroma_interp_8],
            avg:                    [avg_2, avg_4, avg_8, avg_16],
            width: 0, height: 0,
        };
        obj.register_simd();
        obj
    }
    pub fn set_dimensions(&mut self, width: usize, height: usize) {
        self.width  = width;
        self.height = height;
    }
    pub fn do_mc(&mut self, frm: &mut NASimpleVideoFrame<u8>, refpic: &SimpleFrame, xpos: usize, ypos: usize, w: usize, h: usize, mv: MV) {
        let mut ebuf = [0u8; 22 * 22];
        let mvx = mv.x >> 2;
        let mvy = mv.y >> 2;
        let mode = ((mv.x & 3) + (mv.y & 3) * 4) as usize;
        let pre  = if mode != 0 { 2isize } else { 0 };
        let post = if mode != 0 { 3isize } else { 0 };
        let (yw, yh) = (self.width, self.height);
        let src = refpic.data;
        let systride = refpic.stride[0];
        let src_x = (xpos as isize) + (mvx as isize);
        let src_y = (ypos as isize) + (mvy as isize);
        let (ysrc, ystride) = if (src_x - pre < 0) || (src_x + (w as isize) + post > (yw as isize)) || (src_y - pre < 0) || (src_y + (h as isize) + post > (yh as isize)) {
                let add = (pre + post) as usize;
                edge_emu_sf(refpic, src_x - pre, src_y - pre, yw, yh, w + add, h + add, &mut ebuf, 22, 0);
                (&ebuf[..], 22)
            } else {
                (&src[refpic.offset[0] + ((src_x - pre) as usize) + ((src_y - pre) as usize) * systride..], systride)
            };
        let wmode = match w {
                4 => 0,
                8 => 1,
                _ => 2,
            };
        (H264_LUMA_INTERP[wmode][mode])(&mut frm.data[frm.offset[0] + xpos + ypos * frm.stride[0]..], frm.stride[0], ysrc, ystride, h);

        let (cw, ch) = (self.width >> 1, self.height >> 1);
        let mvx = mv.x >> 3;
        let mvy = mv.y >> 3;
        let dx = (mv.x & 7) as u16;
        let dy = (mv.y & 7) as u16;
        let src_x = ((xpos >> 1) as isize) + (mvx as isize);
        let src_y = ((ypos >> 1) as isize) + (mvy as isize);
        let suoff = refpic.offset[1];
        let svoff = refpic.offset[2];
        let sustride = refpic.stride[1];
        let svstride = refpic.stride[2];
        let cbw = w / 2;
        let cbh = h / 2;
        let (csrc, cstride) = if (src_x < 0) || (src_x + (cbw as isize) + 1 > (cw as isize)) || (src_y < 0) || (src_y + (cbh as isize) + 1 > (ch as isize)) {
                let aw = (cw + 7) & !7;
                let ah = (ch + 7) & !7;
                edge_emu_sf(refpic, src_x, src_y, aw, ah, cbw+1, cbh+1, &mut ebuf,      18, 1);
                edge_emu_sf(refpic, src_x, src_y, aw, ah, cbw+1, cbh+1, &mut ebuf[9..], 18, 2);
                ([&ebuf, &ebuf[9..]], [18, 18])
            } else {
                ([&src[suoff + (src_x as usize) + (src_y as usize) * sustride..],
                 &src[svoff + (src_x as usize) + (src_y as usize) * svstride..]],
                 [sustride, svstride])
            };
        for chroma in 1..3 {
            let off = frm.offset[chroma] + xpos / 2 + (ypos / 2) * frm.stride[chroma];
            (self.chroma_interp[wmode])(&mut frm.data[off..], frm.stride[chroma], csrc[chroma - 1], cstride[chroma - 1], dx, dy, cbh);
        }
    }

    pub fn mc_blocks(&mut self, dst: &mut McBlock, refpic: &SimpleFrame, xpos: usize, ypos: usize, w: usize, h: usize, mv: MV) {
        let mode = ((mv.x & 3) + (mv.y & 3) * 4) as usize;

        let pre  = if mode != 0 { 2 } else { 0 };
        let post = if mode != 0 { 3 } else { 0 };
        let (width, height) = (self.width, self.height);
        let sx = (xpos as isize) + ((mv.x >> 2) as isize);
        let sy = (ypos as isize) + ((mv.y >> 2) as isize);

        const EBUF_STRIDE: usize = 32;
        let mut ebuf = [0u8; EBUF_STRIDE * (16 + 2 + 3)];

        let wmode = match w {
                4 => 0,
                8 => 1,
                _ => 2,
            };
        if (sx - pre < 0) || (sx + (w as isize) + post > (width as isize)) ||
           (sy - pre < 0) || (sy + (h as isize) + post > (height as isize)) {
            let edge = (pre + post) as usize;
            edge_emu_sf(refpic, sx - pre, sy - pre, width, height, w + edge, h + edge,
                     &mut ebuf, EBUF_STRIDE, 0);
            (H264_LUMA_INTERP[wmode][mode])(&mut dst.y, 16, &ebuf, EBUF_STRIDE, h);
        } else {
            let sstride = refpic.stride[0];
            let soff    = refpic.offset[0];
            let sbuf    = refpic.data;
            let saddr = soff + ((sx - pre) as usize) + ((sy - pre) as usize) * sstride;
            (H264_LUMA_INTERP[wmode][mode])(&mut dst.y, 16, &sbuf[saddr..], sstride, h);
        }

        let (cw, ch) = (self.width >> 1, self.height >> 1);
        let mvx = mv.x >> 3;
        let mvy = mv.y >> 3;
        let dx = (mv.x & 7) as u16;
        let dy = (mv.y & 7) as u16;
        let src_x = ((xpos >> 1) as isize) + (mvx as isize);
        let src_y = ((ypos >> 1) as isize) + (mvy as isize);
        let suoff = refpic.offset[1];
        let svoff = refpic.offset[2];
        let sustride = refpic.stride[1];
        let svstride = refpic.stride[2];
        let src = refpic.data;
        let cbw = w / 2;
        let cbh = h / 2;
        let (csrc, cstride) = if (src_x < 0) || (src_x + (cbw as isize) + 1 > (cw as isize)) || (src_y < 0) || (src_y + (cbh as isize) + 1 > (ch as isize)) {
                let aw = (cw + 7) & !7;
                let ah = (ch + 7) & !7;
                edge_emu_sf(refpic, src_x, src_y, aw, ah, cbw+1, cbh+1, &mut ebuf,      18, 1);
                edge_emu_sf(refpic, src_x, src_y, aw, ah, cbw+1, cbh+1, &mut ebuf[9..], 18, 2);
                ([&ebuf, &ebuf[9..]], [18, 18])
            } else {
                ([&src[suoff + (src_x as usize) + (src_y as usize) * sustride..],
                 &src[svoff + (src_x as usize) + (src_y as usize) * svstride..]],
                 [sustride, svstride])
            };
        (self.chroma_interp[wmode])(&mut dst.u, 16, csrc[0], cstride[0], dx, dy, cbh);
        (self.chroma_interp[wmode])(&mut dst.v, 16, csrc[1], cstride[1], dx, dy, cbh);
    }

    pub fn do_mc_avg(&mut self, frm: &mut NASimpleVideoFrame<u8>, refpic: &SimpleFrame, xpos: usize, ypos: usize, w: usize, h: usize, mv: MV) {
        let mut abuf = self.avg_buf.clone();
        let stride_y = abuf.get_stride(0);
        let stride_c = abuf.get_stride(1);
        let off_y = abuf.get_offset(0);
        let off_u = abuf.get_offset(1);
        let off_v = abuf.get_offset(2);
        let data = abuf.get_data_mut().unwrap();
        let mut afrm = NASimpleVideoFrame {
                width:      [64, 32, 32, 0],
                height:     [64, 32, 32, 0],
                flip:       false,
                stride:     [stride_y, stride_c, stride_c, 0],
                offset:     [off_y, off_u, off_v, 0],
                components: 3,
                data,
            };
        let amv = MV { x: mv.x + (xpos as i16) * 4, y: mv.y + (ypos as i16) * 4 };
        self.do_mc(&mut afrm, refpic, 0, 0, w, h, amv);
        let wsize = match w {
                2 => 0,
                4 => 1,
                8 => 2,
                _ => 3,
            };
        let src = self.avg_buf.get_data();
        for comp in 0..3 {
            let shift = if comp == 0 { 0 } else { 1 };
            let sstride = self.avg_buf.get_stride(comp);
            let soff = self.avg_buf.get_offset(comp);
            (self.avg[wsize - shift])(&mut frm.data[frm.offset[comp] + (xpos >> shift) + (ypos >> shift) * frm.stride[comp]..], frm.stride[comp], &src[soff..], sstride, h >> shift);
        }
    }

    pub fn gray_block(&mut self, frm: &mut NASimpleVideoFrame<u8>, x: usize, y: usize, w: usize, h: usize) {
        let yoff = frm.offset[0] + x + y * frm.stride[0];
        let coff = [frm.offset[1] + x / 2 + y / 2 * frm.stride[1],
                    frm.offset[2] + x / 2 + y / 2 * frm.stride[2]];
        for row in frm.data[yoff..].chunks_mut(frm.stride[0]).take(h) {
            for el in row[..w].iter_mut() {
                *el = 128;
            }
        }
        for chroma in 0..2 {
            for row in frm.data[coff[chroma]..].chunks_mut(frm.stride[chroma + 1]).take(h / 2) {
                for el in row[..w / 2].iter_mut() {
                    *el = 128;
                }
            }
        }
    }
}

fn edge_emu_sf(src: &SimpleFrame, xpos: isize, ypos: isize, w: usize, h: usize, bw: usize, bh: usize, dst: &mut [u8], dstride: usize, comp: usize) {
    let stride = src.stride[comp];
    let offs   = src.offset[comp];
    let framebuf = src.data;

    for y in 0..bh {
        let srcy;
        if (y as isize) + ypos < 0 { srcy = 0; }
        else if (y as isize) + ypos >= (h as isize) { srcy = h - 1; }
        else { srcy = ((y as isize) + ypos) as usize; }

        for x in 0..bw {
            let srcx;
            if (x as isize) + xpos < 0 { srcx = 0; }
            else if (x as isize) + xpos >= (w as isize) { srcx = w - 1; }
            else { srcx = ((x as isize) + xpos) as usize; }
            dst[x + y * dstride] = framebuf[offs + srcx + srcy * stride];
        }
    }
}

fn avg(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bw: usize, bh: usize) {
    for (dline, sline) in dst.chunks_mut(dstride).zip(src.chunks(sstride)).take(bh) {
        for (dst, src) in dline.iter_mut().zip(sline.iter()).take(bw) {
            *dst = ((u16::from(*dst) + u16::from(*src) + 1) >> 1) as u8;
        }
    }
}

fn avg_2(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bh: usize) {
    let _ = src[sstride + 1];
    let _ = dst[dstride + 1];
    dst[0]           = ((u16::from(dst[0])           + u16::from(src[0])           + 1) >> 1) as u8;
    dst[1]           = ((u16::from(dst[1])           + u16::from(src[1])           + 1) >> 1) as u8;
    dst[dstride]     = ((u16::from(dst[dstride])     + u16::from(src[sstride])     + 1) >> 1) as u8;
    dst[dstride + 1] = ((u16::from(dst[dstride + 1]) + u16::from(src[sstride + 1]) + 1) >> 1) as u8;
    if bh == 4 {
        let _ = src[sstride * 3 + 1];
        let _ = dst[dstride * 3 + 1];
        dst[dstride * 2]     = ((u16::from(dst[dstride * 2])     + u16::from(src[sstride * 2])     + 1) >> 1) as u8;
        dst[dstride * 2 + 1] = ((u16::from(dst[dstride * 2 + 1]) + u16::from(src[sstride * 2 + 1]) + 1) >> 1) as u8;
        dst[dstride * 3]     = ((u16::from(dst[dstride * 3])     + u16::from(src[sstride * 3])     + 1) >> 1) as u8;
        dst[dstride * 3 + 1] = ((u16::from(dst[dstride * 3 + 1]) + u16::from(src[sstride * 3 + 1]) + 1) >> 1) as u8;
    }
}
fn avg_4(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bh: usize) {
    avg(dst, dstride, src, sstride, 4, bh);
}
fn avg_8(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bh: usize) {
    avg(dst, dstride, src, sstride, 8, bh);
}
fn avg_16(dst: &mut [u8], dstride: usize, src: &[u8], sstride: usize, bh: usize) {
    avg(dst, dstride, src, sstride, 16, bh);
}

fn put_block_weighted(dst: &mut [u8], stride: usize, src: &[u8], w: usize, h: usize, wparams: [i8; 3]) {
    let weight = i16::from(wparams[0]);
    let offset = i16::from(wparams[1]);
    let wshift = wparams[2] as u8;
    let bias = (1 << wshift) >> 1;

    for (drow, srow) in dst.chunks_mut(stride).zip(src.chunks_exact(16)).take(h) {
        for (dst, &src) in drow[..w].iter_mut().zip(srow.iter()) {
            *dst = clip_u8(((i16::from(src) * weight + bias) >> wshift) + offset);
        }
    }
}

fn put_blk_w_2(dst: &mut [u8], stride: usize, src: &[u8], h: usize, wparams: [i8; 3]) {
    put_block_weighted(dst, stride, src, 2, h, wparams);
}
fn put_blk_w_4(dst: &mut [u8], stride: usize, src: &[u8], h: usize, wparams: [i8; 3]) {
    put_block_weighted(dst, stride, src, 4, h, wparams);
}
fn put_blk_w_8(dst: &mut [u8], stride: usize, src: &[u8], h: usize, wparams: [i8; 3]) {
    put_block_weighted(dst, stride, src, 8, h, wparams);
}
fn put_blk_w_16(dst: &mut [u8], stride: usize, src: &[u8], h: usize, wparams: [i8; 3]) {
    put_block_weighted(dst, stride, src, 16, h, wparams);
}

fn put_block_weighted2(dst: &mut [u8], stride: usize, src0: &[u8], src1: &[u8], w: usize, h: usize, wparams: [i8; 5]) {
    let weight0 = i16::from(wparams[0]);
    let offset0 = i16::from(wparams[1]);
    let weight1 = i16::from(wparams[2]);
    let offset1 = i16::from(wparams[3]);
    let wshift = (wparams[4] as u8) + 1;
    let offset = (offset0 + offset1 + 1) >> 1;
    let bias = (1 << wshift) >> 1;

    for (drow, (srow0, srow1)) in dst.chunks_mut(stride).zip(src0.chunks_exact(16).zip(src1.chunks_exact(16))).take(h) {
        for (dst, (&src0, &src1)) in drow[..w].iter_mut().zip(srow0.iter().zip(srow1.iter())) {
            *dst = clip_u8(((i16::from(src0) * weight0 + i16::from(src1) * weight1 + bias) >> wshift) + offset);
        }
    }
}

fn put_blk_w2_2(dst: &mut [u8], stride: usize, src0: &[u8], src1: &[u8], h: usize, wparams: [i8; 5]) {
    let weight0 = i16::from(wparams[0]);
    let offset0 = i16::from(wparams[1]);
    let weight1 = i16::from(wparams[2]);
    let offset1 = i16::from(wparams[3]);
    let wshift = (wparams[4] as u8) + 1;
    let offset = (offset0 + offset1 + 1) >> 1;
    let bias = (1 << wshift) >> 1;

    let _ = src0[16 + 1];
    let _ = src1[16 + 1];
    let _ = dst[stride + 1];
    dst[0]          = clip_u8(((i16::from(src0[ 0]) * weight0 + i16::from(src1[ 0]) * weight1 + bias) >> wshift) + offset);
    dst[1]          = clip_u8(((i16::from(src0[ 1]) * weight0 + i16::from(src1[ 1]) * weight1 + bias) >> wshift) + offset);
    dst[stride]     = clip_u8(((i16::from(src0[16]) * weight0 + i16::from(src1[16]) * weight1 + bias) >> wshift) + offset);
    dst[stride + 1] = clip_u8(((i16::from(src0[17]) * weight0 + i16::from(src1[17]) * weight1 + bias) >> wshift) + offset);
    if h == 4 {
        let _ = src0[16 * 3 + 1];
        let _ = src1[16 * 3 + 1];
        let _ = dst[stride * 3 + 1];
        dst[stride * 2]     = clip_u8(((i16::from(src0[32]) * weight0 + i16::from(src1[32]) * weight1 + bias) >> wshift) + offset);
        dst[stride * 2 + 1] = clip_u8(((i16::from(src0[33]) * weight0 + i16::from(src1[33]) * weight1 + bias) >> wshift) + offset);
        dst[stride * 3]     = clip_u8(((i16::from(src0[48]) * weight0 + i16::from(src1[48]) * weight1 + bias) >> wshift) + offset);
        dst[stride * 3 + 1] = clip_u8(((i16::from(src0[49]) * weight0 + i16::from(src1[49]) * weight1 + bias) >> wshift) + offset);
    }
}
fn put_blk_w2_4(dst: &mut [u8], stride: usize, src0: &[u8], src1: &[u8], h: usize, wparams: [i8; 5]) {
    put_block_weighted2(dst, stride, src0, src1, 4, h, wparams);
}
fn put_blk_w2_8(dst: &mut [u8], stride: usize, src0: &[u8], src1: &[u8], h: usize, wparams: [i8; 5]) {
    put_block_weighted2(dst, stride, src0, src1, 8, h, wparams);
}
fn put_blk_w2_16(dst: &mut [u8], stride: usize, src0: &[u8], src1: &[u8], h: usize, wparams: [i8; 5]) {
    put_block_weighted2(dst, stride, src0, src1, 16, h, wparams);
}
