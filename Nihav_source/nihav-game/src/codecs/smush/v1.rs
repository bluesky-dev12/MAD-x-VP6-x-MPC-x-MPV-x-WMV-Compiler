use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

struct Glyphs {
    data:   [[[u8; 16]; 256]; 2],
    glyph8: [[u8; 64]; 256],
    glyph8_init: bool,
}

impl Glyphs {
    fn new() -> Self {
        Self {
            data:   [[[0; 16]; 256]; 2],
            glyph8: [[0; 64]; 256],
            glyph8_init: false,
        }
    }
    fn make_glyphs_4(&mut self, mode: u8) {
        for i in (1..16).step_by(2) {
            let cy = (i as u8) + mode;
            for j in 0..16 {
                let dst = &mut self.data[0][(i / 2) * 16 + j];

                let cx = (j as u8) + mode;
                let avg = mode + (((i + j) >> 1) as u8);
                if avg == cx || avg == cy {
                    dst[ 0] = cx; dst[ 1] = cy; dst[ 2] = cx; dst[ 3] = cy;
                    dst[ 4] = cy; dst[ 5] = cx; dst[ 6] = cy; dst[ 7] = cy;
                    dst[ 8] = cx; dst[ 9] = cy; dst[10] = cx; dst[11] = cy;
                    dst[12] = cx; dst[13] = cx; dst[14] = cy; dst[15] = cx;
                } else {
                    let c0 = avg;
                    let c1 = ((u16::from(avg) + u16::from(cy)) >> 1) as u8;
                    let c2 = ((u16::from(avg) + u16::from(cx)) >> 1) as u8;
                    dst[ 0] = c0; dst[ 1] = c0; dst[ 2] = c1; dst[ 3] = cy;
                    dst[ 4] = c0; dst[ 5] = c0; dst[ 6] = c1; dst[ 7] = cy;
                    dst[ 8] = c2; dst[ 9] = c2; dst[10] = c0; dst[11] = c1;
                    dst[12] = cx; dst[13] = cx; dst[14] = c2; dst[15] = c0;
                }
            }
        }
        for i in (0..16).step_by(2) {
            let cy = (i as u8) + mode;
            for j in 0..16 {
                let dst = &mut self.data[0][128 + (i / 2) * 16 + j];

                let cx = (j as u8) + mode;
                let avg = mode + (((i + j) >> 1) as u8);
                if avg == cx || avg == cy {
                    dst[ 0] = cy; dst[ 1] = cy; dst[ 2] = cx; dst[ 3] = cy;
                    dst[ 4] = cy; dst[ 5] = cy; dst[ 6] = cy; dst[ 7] = cx;
                    dst[ 8] = cx; dst[ 9] = cy; dst[10] = cx; dst[11] = cx;
                    dst[12] = cy; dst[13] = cx; dst[14] = cy; dst[15] = cx;
                } else {
                    let c1 = ((u16::from(avg) + u16::from(cy)) >> 1) as u8;
                    let c2 = ((u16::from(avg) + u16::from(cx)) >> 1) as u8;
                    dst[ 0] = cy; dst[ 1] = cy; dst[ 2] = c1; dst[ 3] = cx;
                    dst[ 4] = cy; dst[ 5] = cy; dst[ 6] = c1; dst[ 7] = cx;
                    dst[ 8] = c1; dst[ 9] = c1; dst[10] = cx; dst[11] = c2;
                    dst[12] = cx; dst[13] = cx; dst[14] = c2; dst[15] = cx;
                }
            }
        }
    }
    fn make_glyphs_5(&mut self, mode: u8) {
        for i in 0..8 {
            let cy = (i as u8) + mode;
            for j in 0..8 {
                let dst = &mut self.data[0][i * 8 + j];

                let cx = (j as u8) + mode;
                let avg = mode + (((i + j) >> 1) as u8);
                let c0 = avg;
                let c1 = ((u16::from(avg) + u16::from(cy)) >> 1) as u8;
                let c2 = ((u16::from(avg) + u16::from(cx)) >> 1) as u8;

                dst[ 0] = c0; dst[ 1] = c0; dst[ 2] = c1; dst[ 3] = cy;
                dst[ 4] = c0; dst[ 5] = c0; dst[ 6] = c1; dst[ 7] = cy;
                dst[ 8] = c2; dst[ 9] = c2; dst[10] = c0; dst[11] = c1;
                dst[12] = cx; dst[13] = cx; dst[14] = c2; dst[15] = c0;
            }
        }
        for i in 0..8 {
            let cy = (i as u8) + mode;
            for j in 0..8 {
                let dst = &mut self.data[0][i * 8 + j + 64];

                let cx = (j as u8) + mode;
                let avg = mode + (((i + j) >> 1) as u8);
                let c0 = avg;
                let c2 = ((u16::from(avg) + u16::from(cx)) >> 1) as u8;

                dst[ 0] = cy; dst[ 1] = cy; dst[ 2] = cy; dst[ 3] = cy;
                dst[ 4] = c0; dst[ 5] = c0; dst[ 6] = c0; dst[ 7] = c0;
                dst[ 8] = c2; dst[ 9] = c2; dst[10] = c2; dst[11] = c2;
                dst[12] = cx; dst[13] = cx; dst[14] = cx; dst[15] = cx;
            }
        }
        for i in 0..8 {
            let cy = (i as u8) + mode;
            for j in 0..8 {
                let dst = &mut self.data[0][i * 8 + j + 128];

                let cx = (j as u8) + mode;
                let avg = mode + (((i + j) >> 1) as u8);
                let c0 = avg;
                let c1 = ((u16::from(avg) + u16::from(cy)) >> 1) as u8;
                let c2 = ((u16::from(avg) + u16::from(cx)) >> 1) as u8;

                dst[ 0] = cy; dst[ 1] = cy; dst[ 2] = c1; dst[ 3] = c0;
                dst[ 4] = cy; dst[ 5] = cy; dst[ 6] = c1; dst[ 7] = c0;
                dst[ 8] = c1; dst[ 9] = c1; dst[10] = c0; dst[11] = c2;
                dst[12] = c0; dst[13] = c0; dst[14] = c2; dst[15] = cx;
            }
        }
        for i in 0..8 {
            let cy = (i as u8) + mode;
            for j in 0..8 {
                let dst = &mut self.data[0][i * 8 + j + 192];

                let cx = (j as u8) + mode;
                let avg = mode + (((i + j) >> 1) as u8);
                let c0 = avg;
                let c2 = ((u16::from(avg) + u16::from(cx)) >> 1) as u8;

                dst[ 0] = cy; dst[ 1] = c0; dst[ 2] = c2; dst[ 3] = cx;
                dst[ 4] = cy; dst[ 5] = c0; dst[ 6] = c2; dst[ 7] = cx;
                dst[ 8] = cy; dst[ 9] = c0; dst[10] = c2; dst[11] = cx;
                dst[12] = cy; dst[13] = c0; dst[14] = c2; dst[15] = cx;
            }
        }
    }
    fn read_additional(&mut self, br: &mut ByteReader, add: u16) -> DecoderResult<()> {
        if add > 0 {
            validate!(add <= 256);
            let mut gbuf = [0; 8];
            for glyph in self.data[1].iter_mut().take(add as usize) {
                                          br.read_buf(&mut gbuf)?;
                for (pair, &b) in glyph.chunks_mut(2).zip(gbuf.iter()) {
                    pair[0] = b >> 4;
                    pair[1] = b & 0xF;
                }
            }
        }
        Ok(())
    }
    fn make_glyphs_47(&mut self) {
        super::make_glyphs_47(&mut self.data[0], &mut self.glyph8);
        self.glyph8_init = true;
    }
}

struct FrameData {
    info:       NACodecInfoRef,
    pal:        [u8; 768],
    fpal:       [u16; 768],
    pdelta:     [u16; 768],
    width:      usize,
    height:     usize,
    frm0:       Vec<u8>,
    frm1:       Vec<u8>,
    frm2:       Vec<u8>,
}

impl FrameData {
    fn new() -> Self {
        Self {
            info:       NACodecInfoRef::default(),
            pal:        [0; 768],
            fpal:       [0; 768],
            pdelta:     [0; 768],
            width:      0,
            height:     0,
            frm0:       Vec::new(),
            frm1:       Vec::new(),
            frm2:       Vec::new(),
        }
    }
    fn init(&mut self, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
self.width = (self.width + 7) & !7;
self.height = (self.height + 7) & !7;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, false, PAL8_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            if let Some(edata) = info.get_extradata() {
                validate!(edata.len() > 768);
                self.pal.copy_from_slice(&edata[1..][..768]);
            }

            self.frm0.resize(self.width * self.height, 0);
            self.frm1.resize(self.width * self.height, 0);
            self.frm2.resize(self.width * self.height, 0);
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    #[inline]
    fn set_pixel(&mut self, xoff: i16, yoff: i16, x: usize, y: usize, pix: u8) {
        let xpos = (xoff as isize) + (x as isize);
        if xpos < 0 { return; }
        let xpos = xpos as usize;
        let ypos = (yoff as isize) + (y as isize);
        if ypos < 0 { return; }
        let ypos = ypos as usize;
        if xpos < self.width && ypos < self.height {
            self.frm0[xpos + ypos * self.width] = pix;
        }
    }
    fn get_pixel(&mut self, xoff: i16, yoff: i16, x: usize, y: usize) -> u8 {
        let xpos = (xoff as isize) + (x as isize);
        if xpos < 0 { return 0; }
        let xpos = xpos as usize;
        let ypos = (yoff as isize) + (y as isize);
        if ypos < 0 { return 0; }
        let ypos = ypos as usize;
        if xpos < self.width && ypos < self.height {
            self.frm0[xpos + ypos * self.width]
        } else {
            0
        }
    }
    fn loop_filter(&mut self, _xoff: i16, _yoff: i16, _x: usize, _y: usize) {
/*        let xpos = (xoff as isize) + (x as isize);
        if xpos < 0 { return; }
        let xpos = xpos as usize;
        let ypos = (yoff as isize) + (y as isize);
        if ypos < 0 { return; }
        let ypos = ypos as usize;
        if xpos < self.width && ypos < self.height {
            let start = xpos + ypos * self.width;
            if xpos > 0 {
                for row in self.frm0[start - 1..].chunks_mut(self.width).take(4) {
                    let x0 = row[0];
                    let x1 = row[1];
                    row[1] = 0x80 | (x0.wrapping_add(x1) >> 1);
                }
            }
            if ypos > 0 {
                for i in 0..4 {
                    let y0 = self.frm0[start + i];
                    let y1 = &mut self.frm0[start + i + self.width];
                    *y1 = 0x80 | (y1.wrapping_add(y0) >> 1);
                }
            }
        }*/
    }
    fn get_frame(&mut self, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
        if let Some(ref mut vbuf) = bufinfo.get_vbuf() {
            let stride = vbuf.get_stride(0);
            let paloff = vbuf.get_offset(1);
            let data = vbuf.get_data_mut().unwrap();
            for (dst, src) in data.chunks_mut(stride).zip(self.frm0.chunks(self.width).take(self.height)) {
                dst[..self.width].copy_from_slice(src);
            }
            data[paloff..][..768].copy_from_slice(&self.pal);
        } else {
            return Err(DecoderError::Bug);
        }

        let is_intra = pkt.keyframe;
        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
}

fn do_mc(dst: &mut [u8], src: &[u8], stride: usize, xoff: isize, yoff: isize, w: usize, h: usize) {
    let mut pos = xoff + yoff * (stride as isize);
    for row in dst.chunks_mut(stride).take(4) {
        for i in 0..4 {
            row[i] = if pos >= 0 && (pos as usize) < w + (h - 1) * stride {
                    src[pos as usize]
                } else { 0 };
            pos += 1;
        }
        pos -= 4;
        pos += stride as isize;
    }
}

#[allow(clippy::too_many_arguments)]
fn do_block47(br: &mut ByteReader, dst: &mut [u8], frm1: &[u8], frm2: &[u8], x: usize, y: usize, stride: usize, bsize: usize, clr: &[u8; 6], glyphs: &Glyphs) -> DecoderResult<()> {
    let op                              = br.read_byte()?;
    match op {
        0xFF if bsize > 2 => {
            let hsize = bsize / 2;
            do_block47(br, dst, frm1, frm2, x, y, stride, hsize, clr, glyphs)?;
            do_block47(br, &mut dst[hsize..], frm1, frm2, x + hsize, y, stride, bsize / 2, clr, glyphs)?;
            do_block47(br, &mut dst[hsize * stride..], frm1, frm2, x, y + hsize, stride, hsize, clr, glyphs)?;
            do_block47(br, &mut dst[hsize * (stride + 1)..], frm1, frm2, x + hsize, y + hsize, stride, bsize / 2, clr, glyphs)?;
        },
        0xFF => {
                                          br.read_buf(&mut dst[..2])?;
                                          br.read_buf(&mut dst[stride..][..2])?;
        },
        0xFE => {
            let pix                     = br.read_byte()?;
            for dst in dst.chunks_mut(stride).take(bsize) {
                for el in dst[..bsize].iter_mut() {
                    *el = pix;
                }
            }
        },
        0xFD => {
            let idx                     = br.read_byte()? as usize;
            let mut clr = [0; 2];
            clr[1]                      = br.read_byte()?;
            clr[0]                      = br.read_byte()?;
            let mut glyph = if bsize == 8 { glyphs.glyph8[idx].iter() } else { glyphs.data[0][idx].iter() };

            for dst in dst.chunks_mut(stride).take(bsize) {
                for el in dst[..bsize].iter_mut() {
                    *el = clr[*glyph.next().unwrap_or(&0) as usize];
                }
            }
        },
        0xFC => {
            let off = x + y * stride;
            let src = &frm1[off..];
            for (dst, src) in dst.chunks_mut(stride).zip(src.chunks(stride)).take(bsize) {
                dst[..bsize].copy_from_slice(&src[..bsize]);
            }
        },
        0xF8..=0xFB => {
            let pix = clr[(op & 7) as usize];
            for dst in dst.chunks_mut(stride).take(bsize) {
                for el in dst[..bsize].iter_mut() {
                    *el = pix;
                }
            }
        },
        _ => {
            let mx = C47_MV[0][op as usize][0] as isize;
            let my = C47_MV[0][op as usize][1] as isize;
            let off = (x as isize) + mx + ((y as isize) + my) * (stride as isize);
            validate!(off >= 0);
            let src = &frm2[off as usize..];
            for (dst, src) in dst.chunks_mut(stride).zip(src.chunks(stride)).take(bsize) {
                dst[..bsize].copy_from_slice(&src[..bsize]);
            }
        },
    };
    Ok(())
}

macro_rules! c48_mv {
    (index; $dst: expr, $src: expr, $br: expr, $x: expr, $y: expr, $stride: expr, $size: expr, $mvsel: expr) => ({
        for yy in (0..8).step_by($size) {
            for xx in (0..8).step_by($size) {
                let idx = $br.read_byte()? as usize;
                validate!(idx < 255);
                let mx = C47_MV[$mvsel][idx][0] as isize;
                let my = C47_MV[$mvsel][idx][1] as isize;
                c48_mv!(common; &mut $dst[xx + yy * $stride..], $src, $x + xx, $y + yy, mx, my, $stride, $size)
            }
        }

    });
    (offset; $dst: expr, $src: expr, $br: expr, $x: expr, $y: expr, $w: expr, $stride: expr, $size: expr) => ({
        for yy in (0..8).step_by($size) {
            for xx in (0..8).step_by($size) {
                let offset              = $br.read_u16le()? as i16 as isize;
                let mx = offset % ($w as isize);
                let my = offset / ($w as isize);
                c48_mv!(common; &mut $dst[xx + yy * $stride..], $src, $x + xx, $y + yy, mx, my, $stride, $size)
            }
        }
    });
    (common; $dst: expr, $src: expr, $x: expr, $y: expr, $mx: expr, $my: expr, $stride: expr, $size: expr) => {{
        let srcpos = ($x as isize) + $mx + (($y as isize) + $my) * ($stride as isize);
        validate!(srcpos >= 0);
        for (dst, src) in $dst.chunks_mut($stride).zip($src[srcpos as usize..].chunks($stride)).take($size) {
            let size = dst.len().min(src.len()).min($size);
            dst[..size].copy_from_slice(&src[..size]);
        }
    }}
}

fn scale2x(block: &[u8; 16], dst: &mut [u8], stride: usize) {
    for (drow, src) in dst.chunks_mut(stride * 2).zip(block.chunks_exact(4)) {
        for row in drow.chunks_mut(stride) {
            for (dpair, &el) in row.chunks_exact_mut(2).zip(src.iter()) {
                dpair[0] = el;
                dpair[1] = el;
            }
        }
    }
}

struct Smush1Decoder {
    glyphs:     Glyphs,
    pic:        FrameData,
    version:    u8,
    prev_seq:   u16,
    reorder:    u8,
    filter:     [[u8; 256]; 256],
}

impl Smush1Decoder {
    fn new() -> Self {
        Self {
            glyphs:     Glyphs::new(),
            pic:        FrameData::new(),
            version:    0,
            prev_seq:   0,
            reorder:    0,
            filter:     [[0; 256]; 256],
        }
    }

    fn decode_rle(br: &mut ByteReader, dst: &mut [u8], w: usize, h: usize, stride: usize) -> DecoderResult<()> {
        let mut x = 0;
        let mut y = 0;
        let mut len = 0;
        let mut clr = 0;
        let mut run = false;
        while (x != 0) || (y != h) {
            if len == 0 {
                let op                  = br.read_byte()?;
                run = (op & 1) != 0;
                if run {
                    clr                 = br.read_byte()?;
                }
                len = ((op >> 1) + 1) as usize;
            }
            if run {
                dst[x + y * stride] = clr;
            } else {
                dst[x + y * stride]     = br.read_byte()?;
            }
            len -= 1;
            x += 1;
            if x == w {
                x = 0;
                y += 1;
            }
        }
        validate!(len == 0);

        Ok(())
    }

    fn decode_1(&mut self, br: &mut ByteReader, x: i16, y: i16, w: usize, h: usize, transparent: bool) -> DecoderResult<()> {
        for yy in 0..h {
            let len                     = u64::from(br.read_u16le()?);
            let end = br.tell() + len;
            let mut xx = 0;
            while (br.tell() < end) && (xx < w) {
                let op                  = br.read_byte()?;
                let len = ((op >> 1) + 1) as usize;
                if (op & 1) == 0 {
                    for _ in 0..len {
                        let clr         = br.read_byte()?;
                        if !transparent || clr != 0 {
                            self.pic.set_pixel(x, y, xx, yy, clr);
                        }
                        xx += 1;
                    }
                } else {
                    let clr             = br.read_byte()?;
                    if !transparent || clr != 0 {
                        for _ in 0..len {
                            self.pic.set_pixel(x, y, xx, yy, clr);
                            xx += 1;
                        }
                    } else {
                        xx += len;
                    }
                }
            }
            validate!(br.tell() == end && xx == w);
        }

        Ok(())
    }
    #[allow(clippy::verbose_bit_mask)]
    fn decode_2(&mut self, br: &mut ByteReader, x: i16, y: i16, _w: usize, _h: usize, len: usize) -> DecoderResult<()> {

        validate!((len & 3) == 0);
        let mut xpos = x;
        let mut ypos = y;
        for _ in 0..len/4 {
            let xoff                    = br.read_u16le()? as i16;
            let yoff                    = i16::from(br.read_byte()?);
            let pix                     = br.read_byte()?;

            xpos += xoff;
            ypos += yoff;
            self.pic.set_pixel(xpos, ypos, 0, 0, pix);
        }
        Ok(())
    }
    #[allow(clippy::too_many_arguments)]
    fn decode_4(&mut self, br: &mut ByteReader, x: i16, y: i16, w: usize, h: usize, mode: u8, add: u16) -> DecoderResult<()> {
        self.glyphs.make_glyphs_4(mode);
        self.glyphs.read_additional(br, add)?;

        for col in (0..w).step_by(4) {
            let mut mask = 0;
            let mut bits = 0;
            for row in (0..h).step_by(4) {
                let bit = if add > 0 {
                        if bits == 0 {
                            mask        = br.read_byte()?;
                            bits = 8;
                        }
                        let bit = (mask & 0x80) != 0;
                        mask <<= 1;
                        bits -= 1;
                        bit
                    } else {
                        false
                    };

                let tile_no             = br.read_byte()? as usize;
                if !bit && (tile_no == 0x80) {
                    continue;
                }
                let src = &self.glyphs.data[bit as usize][tile_no];
                for (y1, srow) in src.chunks(4).enumerate() {
                    for (x1, &pix) in srow.iter().enumerate() {
                        self.pic.set_pixel(x, y, col + x1, row + y1, pix);
                    }
                }
                if !bit {
                    self.pic.loop_filter(x, y, col, row);
                }
            }
        }

        Ok(())
    }
    #[allow(clippy::too_many_arguments)]
    fn decode_5(&mut self, br: &mut ByteReader, x: i16, y: i16, w: usize, h: usize, mode: u8, add: u16) -> DecoderResult<()> {
        self.glyphs.make_glyphs_5(mode);
        self.glyphs.read_additional(br, add)?;

        for col in (0..w).step_by(4) {
            let mut mask = 0;
            let mut bits = 0;
            for row in (0..h).step_by(4) {
                let bit = if add > 0 {
                        if bits == 0 {
                            mask        = br.read_byte()?;
                            bits = 8;
                        }
                        let bit = (mask & 0x80) != 0;
                        mask <<= 1;
                        bits -= 1;
                        bit
                    } else {
                        false
                    };

                let tile_no             = br.read_byte()? as usize;
                let src = &self.glyphs.data[bit as usize][tile_no];
                for (y1, srow) in src.chunks(4).enumerate() {
                    for (x1, &pix) in srow.iter().enumerate() {
                        self.pic.set_pixel(x, y, col + x1, row + y1, pix);
                    }
                }
                if !bit {
                    self.pic.loop_filter(x, y, col, row);
                }
            }
        }

        Ok(())
    }
    fn decode_21(&mut self, br: &mut ByteReader, x: i16, y: i16, w: usize, h: usize, size: usize) -> DecoderResult<()> {
        let end = br.tell() + (size as u64);
        for yy in 0..h {
            if br.tell() >= end { break; }
            let len                     = u64::from(br.read_u16le()?);
            let end = br.tell() + len;
            let mut xx = 0;
            let mut skip = true;
            while (br.tell() < end) && (xx <= w) {
                let len                 = br.read_u16le()? as usize;
                if !skip {
                    for _ in 0..=len {
                        let pix         = br.read_byte()?;
                        self.pic.set_pixel(x, y, xx, yy, pix);
                        xx += 1;
                    }
                } else {
                    for _ in 0..len {
                        self.pic.set_pixel(x, y, xx, yy, 0);
                        xx += 1;
                    }
                }
                skip = !skip;
            }
            validate!(br.tell() == end && xx == w + 1);
        }

        Ok(())
    }
    #[allow(clippy::too_many_arguments)]
    fn decode_23(&mut self, br: &mut ByteReader, x: i16, y: i16, w: usize, h: usize, bias: u8, add: u16, old: bool) -> DecoderResult<()> {
        let mut lut = [0; 256];
        if old {
            for (i, el) in lut.iter_mut().enumerate() {
                *el = (i as u8).wrapping_add(bias.wrapping_sub(0x30));
            }
        } else if add != 256 {
            for (i, el) in lut.iter_mut().enumerate() {
                *el = (i as u8).wrapping_add(add as u8);
            }
        } else {
                                          br.read_buf(&mut lut)?;
        }
        for yy in 0..h {
            let len                     = u64::from(br.read_u16le()?);
            let end = br.tell() + len;
            let mut xx = 0;
            let mut skip = true;
            while (br.tell() < end) && (xx <= w) {
                let len                 = br.read_byte()? as usize;
                if !skip {
                    for _ in 0..len {
                        let pix = self.pic.get_pixel(x, y, xx, yy);
                        self.pic.set_pixel(x, y, xx, yy, lut[pix as usize]);
                        xx += 1;
                    }
                } else {
                    xx += len;
                }
                skip = !skip;
            }
            validate!(br.tell() == end && xx == w + 1);
        }

        Ok(())
    }
    fn decode_37(&mut self, br: &mut ByteReader, x: i16, y: i16, mut w: usize, mut h: usize) -> DecoderResult<()> {
        let compr                       = br.read_byte()?;
        let mv_off                      = br.read_byte()? as usize;
        validate!(mv_off <= 2);
        let seq                         = br.read_u16le()?;
        let _unp_size                   = br.read_u32le()?;
        let _packed_size                = br.read_u32le()?;
        let flags                       = br.read_byte()?;
                                          br.read_skip(3)?;

        w = (w + 3) & !3;
        h = (h + 3) & !3;

        validate!(x >= 0 && y >= 0);
        let x = x as usize;
        let y = y as usize;
        validate!((x + w <= self.pic.width) && (y + h <= self.pic.height));

        if compr == 0 || compr == 2 {
            for el in self.pic.frm1.iter_mut() {
                *el = 0;
            }
            for el in self.pic.frm2.iter_mut() {
                *el = 0;
            }
        } else if ((seq & 1) != 0) || ((flags & 1) == 0) {
            std::mem::swap(&mut self.pic.frm0, &mut self.pic.frm2);
        }

        let stride = self.pic.width;
        let dst = &mut self.pic.frm0[x + y * stride..];
        let prv = &self.pic.frm2[x + y * stride..];
        match compr {
            0 => {
                for line in dst.chunks_mut(stride).take(h) {
                                          br.read_buf(&mut line[..w])?;
                }
            },
            1 => {
                let mut len = -1;
                let mut run = false;
                let mut code = 0;
                for (row_no, row) in dst.chunks_mut(stride * 4).take(h / 4).enumerate() {
                    for col in (0..w).step_by(4) {
                        let skip_code = if len < 0 {
                                let op = br.read_byte()?;
                                len = (op >> 1) as i8;
                                run = (op & 1) != 0;
                                false
                            } else {
                                run
                            };
                        if !skip_code {
                            code        = br.read_byte()?;
                            if code == 0xFF {
                                len -= 1;
                                for drow in row[col..].chunks_mut(stride) {
                                    for el in drow[..4].iter_mut() {
                                        if len < 0 {
                                            let op = br.read_byte()?;
                                            len = (op >> 1) as i8;
                                            run = (op & 1) != 0;
                                            if run {
                                                code = br.read_byte()?;
                                            }
                                        }
                                        if run {
                                            *el = code;
                                        } else {
                                            *el = br.read_byte()?;
                                        }
                                        len -= 1;
                                    }
                                }
                                continue;
                            }
                        }
                        let idx = code as usize;
                        let mx = C37_MV[mv_off][idx * 2] as isize;
                        let my = C37_MV[mv_off][idx * 2 + 1] as isize;
                        do_mc(&mut row[col..], &self.pic.frm2, stride,
                              (x as isize) + (col as isize) + mx,
                              (y as isize) + (row_no as isize) * 4 + my,
                              self.pic.width, self.pic.height);
                        len -= 1;
                    }
                }
            },
            2 => {
                Self::decode_rle(br, dst, w, h, stride)?;
            },
            3 | 4 => {
                let has_fills = (flags & 4) != 0;
                let has_skips = compr == 4;
                let mut skip_run = 0;
                for (row_no, row) in dst.chunks_mut(stride * 4).take(h / 4).enumerate() {
                    for col in (0..w).step_by(4) {
                        if skip_run > 0 {
                            for (drow, srow) in row[col..].chunks_mut(stride).zip(prv[col + row_no * 4 * stride..].chunks(stride)) {
                                drow[..4].copy_from_slice(&srow[..4]);
                            }
                            skip_run -= 1;
                            continue;
                        }
                        let opcode      = br.read_byte()?;
                        match opcode {
                            0xFF => {
                                for drow in row[col..].chunks_mut(stride) {
                                          br.read_buf(&mut drow[..4])?;
                                }
                            },
                            0xFE if has_fills => {
                                for drow in row[col..].chunks_mut(stride) {
                                    let clr = br.read_byte()?;
                                    for el in drow[..4].iter_mut() {
                                        *el = clr;
                                    }
                                }
                            },
                            0xFD if has_fills => {
                                let clr = br.read_byte()?;
                                for drow in row[col..].chunks_mut(stride) {
                                    for el in drow[..4].iter_mut() {
                                        *el = clr;
                                    }
                                }
                            },
                            0 if has_skips => {
                                skip_run = br.read_byte()?;
                                for (drow, srow) in row[col..].chunks_mut(stride).zip(prv[col + row_no * 4 * stride..].chunks(stride)) {
                                    drow[..4].copy_from_slice(&srow[..4]);
                                }
                            },
                            _ => {
                                let idx = opcode as usize;
                                let mx = C37_MV[mv_off][idx * 2] as isize;
                                let my = C37_MV[mv_off][idx * 2 + 1] as isize;
                                do_mc(&mut row[col..], &self.pic.frm2, stride,
                                      (x as isize) + (col as isize) + mx,
                                      (y as isize) + (row_no as isize) * 4 + my,
                                      self.pic.width, self.pic.height);
                            },
                        };
                    }
                }
            },
            _ => return Err(DecoderError::InvalidData),
        };

        Ok(())
    }
    fn decode_47(&mut self, br: &mut ByteReader, x: i16, y: i16, w: usize, h: usize) -> DecoderResult<()> {
        let seq                         = br.read_u16le()?;
        let compr                       = br.read_byte()?;
        let reorder                     = br.read_byte()?;
        let flags                       = br.read_byte()?;
                                          br.read_skip(3)?;
        let mut clr = [0; 6];
                                          br.read_buf(&mut clr)?;
        let _dec_size                   = br.read_u32le()?;
                                          br.read_skip(4)?;
                                          br.read_skip(4)?;

        if (flags & 1) != 0 {
            for i in 0..256 {
                for j in i..256 {
                    let val             = br.read_byte()?;
                    self.filter[i][j] = val;
                    self.filter[j][i] = val;
                }
            }
        }

        if compr == 2 && !self.glyphs.glyph8_init {
            self.glyphs.make_glyphs_47();
        }

        if seq == 0 {
            for el in self.pic.frm1.iter_mut() {
                *el = 0;
            }
            for el in self.pic.frm2.iter_mut() {
                *el = 0;
            }
        }

        validate!(x >= 0 && y >= 0);
        let x = x as usize;
        let y = y as usize;
        validate!((x + w <= self.pic.width) && (y + h <= self.pic.height));

        let stride = self.pic.width;
        let dst = &mut self.pic.frm0[x + y * stride..];
        match compr {
            0 => {
                for line in dst.chunks_mut(stride).take(h) {
                                          br.read_buf(&mut line[..w])?;
                }
            },
            1 => {
                for row in dst.chunks_mut(stride * 2).take((h + 1) / 2) {
                    for col in (0..w).step_by(2) {
                        let pix         = br.read_byte()?;
                        row[col]              = pix;
                        row[col + 1]          = pix;
                        row[col + stride]     = pix;
                        row[col + stride + 1] = pix;
                    }
                }
            },
            2 => {
                for (row_no, row) in dst.chunks_mut(stride * 8).take((h + 7) / 8).enumerate() {
                    for col in (0..w).step_by(8) {
                        do_block47(br, &mut row[col..], &self.pic.frm1, &self.pic.frm2, col, row_no * 8, stride, 8, &clr, &self.glyphs)?;
                    }
                }
            },
            3 => {
                self.pic.frm0.copy_from_slice(&self.pic.frm2);
            },
            4 => {
                self.pic.frm0.copy_from_slice(&self.pic.frm1);
            },
            5 => {
                Self::decode_rle(br, dst, w, h, stride)?;
            },
            _ => return Err(DecoderError::InvalidData),
        };

        self.reorder = if seq == 0 || seq == self.prev_seq + 1 { reorder } else { 0 };
        self.prev_seq = seq;

        Ok(())
    }
    fn decode_48(&mut self, br: &mut ByteReader, x: i16, y: i16, mut w: usize, mut h: usize) -> DecoderResult<()> {
        let compr                       = br.read_byte()?;
        let mvsel                       = br.read_byte()? as usize;
        validate!(mvsel < 2);
        let _seq                        = br.read_u16le()?;
        let _packed_size                = br.read_u32le()?;
        let _unpacked_size              = br.read_u32le()?;
        let flags                       = br.read_byte()?;
                                          br.read_skip(3)?;
        if (flags & 8) != 0 {
            for i in 0..256 {
                for j in i..256 {
                    let val             = br.read_byte()?;
                    self.filter[i][j] = val;
                    self.filter[j][i] = val;
                }
            }
        }

        w = (w + 7) & !7;
        h = (h + 7) & !7;
        std::mem::swap(&mut self.pic.frm0, &mut self.pic.frm2);

        validate!(x >= 0 && y >= 0);
        let x = x as usize;
        let y = y as usize;
        validate!((x + w <= self.pic.width) && (y + h <= self.pic.height));

        let stride = self.pic.width;
        let dst = &mut self.pic.frm0[x + y * stride..];
        match compr {
            0 => {
                for line in dst.chunks_mut(stride).take(h) {
                                          br.read_buf(&mut line[..w])?;
                }
            },
            2 => {
                Self::decode_rle(br, dst, w, h, stride)?;
            },
            3 => {
                let mut block = [0; 16];
                for (row_no, row) in dst.chunks_mut(stride * 8).take((h + 7) / 8).enumerate() {
                    for col in (0..w).step_by(8) {
                        let op          = br.read_byte()?;
                        match op {
                            0xFF => {
                                let val = br.read_byte()?;

                                // it should be interpolated which means reading top pixels...
                                for el in block.iter_mut() {
                                    *el = val;
                                }
                                scale2x(&block, &mut row[col..], stride);
                            },
                            0xFE => {
                                c48_mv!(offset; &mut row[col..], &self.pic.frm2, br, col, row_no * 8, w, stride, 8);
                            },
                            0xFD => {
                                block[ 5]     = br.read_byte()?;
                                block[ 7]     = br.read_byte()?;
                                block[13]     = br.read_byte()?;
                                block[15]     = br.read_byte()?;

                                // it should be interpolated which means reading top pixels...
                                block[ 0] = block[ 5];
                                block[ 1] = block[ 5];
                                block[ 4] = block[ 5];
                                block[ 2] = block[ 7];
                                block[ 3] = block[ 7];
                                block[ 6] = block[ 7];
                                block[ 8] = block[13];
                                block[ 9] = block[13];
                                block[12] = block[13];
                                block[10] = block[15];
                                block[11] = block[15];
                                block[14] = block[15];
                                scale2x(&block, &mut row[col..], stride);
                            },
                            0xFC => {
                                c48_mv!(index; &mut row[col..], &self.pic.frm2, br, col, row_no * 8, stride, 4, mvsel);
                            },
                            0xFB => {
                                c48_mv!(offset; &mut row[col..], &self.pic.frm2, br, col, row_no * 8, w, stride, 4);
                            },
                            0xFA => {
                                                br.read_buf(&mut block)?;
                                scale2x(&block, &mut row[col..], stride);
                            },
                            0xF9 => {
                                c48_mv!(index; &mut row[col..], &self.pic.frm2, br, col, row_no * 8, stride, 2, mvsel);
                            },
                            0xF8 => {
                                c48_mv!(offset; &mut row[col..], &self.pic.frm2, br, col, row_no * 8, w, stride, 2);
                            },
                            0xF7 => {
                                for line in row[col..].chunks_mut(stride) {
                                          br.read_buf(&mut line[..8])?;
                                }
                            },
                            _ => {
                                          br.seek(SeekFrom::Current(-1))?;
                                c48_mv!(index; &mut row[col..], &self.pic.frm2, br, col, row_no * 8, stride, 8, mvsel);
                            },
                        };
                    }
                }
            },
            5 => {
                for row in dst.chunks_mut(stride * 2).take((h + 1) / 2) {
                    let mut last        = br.read_byte()?;
                    row[0] = last;
                    for col in (1..w).step_by(2) {
                        let new         = br.read_byte()?;
                        row[col] = self.filter[last as usize][new as usize];
                        row[col + 1] = new;
                        last = new;
                    }
                }
                let mut off0 = 0;
                let mut off1 = stride;
                let mut off2 = stride * 2;
                for _ in (1..h).step_by(2) {
                    for i in 0..w {
                        dst[off1 + i] = self.filter[dst[off0 + i] as usize][dst[off2 + i] as usize];
                    }
                    off0 = off2;
                    off1 += stride * 2;
                    off2 += stride * 2;
                }
            },
            _ => return Err(DecoderError::InvalidData),
        };
        Ok(())
    }
}

impl NADecoder for Smush1Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let Some(edata) = info.get_extradata() {
            validate!(!edata.is_empty() && edata[0] <= 2);
            self.version = edata[0];
        }
        self.pic.init(info)
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 8);

        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);

        let mut store = false;
        while br.left() > 0 {
            let tag                     = br.read_tag()?;
            let size                    = br.read_u32be()? as usize;
            validate!((size as i64) <= br.left());
            match &tag {
                b"NPAL" => {
                    validate!((size % 3) == 0);
                                          br.read_buf(&mut self.pic.pal[..size])?;
                },
                b"XPAL" => {
                    let cmd             = br.read_u32be()?;
                    match cmd {
                        0 => {
                            validate!(size == 0x604);
                            for el in self.pic.pdelta.iter_mut() {
                                *el     = br.read_u16le()?;
                            }
                            for (dst, &src) in self.pic.fpal.iter_mut().zip(self.pic.pal.iter()) {
                                *dst = u16::from(src) << 7;
                            }
                        },
                        1 => {
                            validate!(size == 4 || size == 6);
                            for i in 0..768 {
                                self.pic.fpal[i] = self.pic.fpal[i].wrapping_add(self.pic.pdelta[i]);
                                self.pic.pal[i] = (self.pic.fpal[i] >> 7) as u8;
                            }
                                          br.read_skip(size - 4)?;
                        },
                        2 => {
                            validate!(size == 0x904);
                            for el in self.pic.pdelta.iter_mut() {
                                *el     = br.read_u16le()?;
                            }
                                          br.read_buf(&mut self.pic.pal)?;
                        },
                        _ => return Err(DecoderError::InvalidData),
                    };
                },
                b"FTCH" => {
                                          br.read_skip(size)?;
                    self.pic.frm0.copy_from_slice(&self.pic.frm1);
                },
                b"STOR" => {
                    store = true;
                                          br.read_skip(size)?;
                },
                b"FOBJ" => {
                    validate!(size >= 14);
                    let end = br.tell() + (size as u64);
                    let compression     = br.read_byte()?;
                    let cparam          = br.read_byte()?;
                    let x               = br.read_u16le()? as i16;
                    let y               = br.read_u16le()? as i16;
                    let w               = br.read_u16le()? as usize;
                    let h               = br.read_u16le()? as usize;
                    let _               = br.read_u16le()?;
                    let param2          = br.read_u16le()?;

                    match compression {
                        1 | 3 => self.decode_1(&mut br, x, y, w, h, (compression == 1) ^ (self.version != 1))?,
                        2  => self.decode_2(&mut br, x, y, w, h, size - 14)?,
                        4 | 33 => self.decode_4(&mut br, x, y, w, h, cparam, param2)?,
                        5 | 34 => self.decode_5(&mut br, x, y, w, h, cparam, param2)?,
                        21 | 44 => self.decode_21(&mut br, x, y, w, h, size - 14)?,
                        23 => self.decode_23(&mut br, x, y, w, h, cparam, param2, self.version == 1)?,
                        37 => {
                            let start = br.tell() as usize;
                            let end   = start + size - 14;
                            let mut mr = MemoryReader::new_read(&src[start..end]);
                            let mut br = ByteReader::new(&mut mr);
                            self.decode_37(&mut br, x, y, w, h)?;
                        },
                        47 => {
                            let start = br.tell() as usize;
                            let end   = start + size - 14;
                            let mut mr = MemoryReader::new_read(&src[start..end]);
                            let mut br = ByteReader::new(&mut mr);
                            self.decode_47(&mut br, x, y, w, h)?;
                        },
                        48 => {
                            let start = br.tell() as usize;
                            let end   = start + size - 14;
                            let mut mr = MemoryReader::new_read(&src[start..end]);
                            let mut br = ByteReader::new(&mut mr);
                            self.decode_48(&mut br, x, y, w, h)?;
                        },
                        _ => return Err(DecoderError::NotImplemented),
                    };
                    validate!(br.tell() <= end);
                    let tail = end - br.tell();
                                          br.read_skip(tail as usize)?;
                    if store {
                        self.pic.frm1.copy_from_slice(&self.pic.frm0);
                        store = false;
                    }
                },
                _ =>                      br.read_skip(size)?,
            };
        }

        let ret = self.pic.get_frame(pkt);

        if self.reorder == 2 {
            std::mem::swap(&mut self.pic.frm1, &mut self.pic.frm2);
        }
        if self.reorder != 0 {
            std::mem::swap(&mut self.pic.frm0, &mut self.pic.frm2);
        }
        self.reorder = 0;

        ret
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for Smush1Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder_video_v1() -> Box<dyn NADecoder + Send> {
    Box::new(Smush1Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    // samples from Rebel Assault
    #[test]
    fn test_smush_anim_v1() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("smush", "smushv1", "assets/Game/smush/c1block.anm", Some(42), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x39339398, 0x7ce83788, 0xaac917d4, 0xaef9d653]));
        test_decoding("smush", "smushv1", "assets/Game/smush/c1c3po.anm", Some(42), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x9c1c2422, 0x2121aa7a, 0xc06418bc, 0xd82d704b]));
        test_decoding("smush", "smushv1", "assets/Game/smush/o1option.anm", Some(4), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x21ea3ee9, 0x3d88bcee, 0x9b71a87a, 0xc5e0a006]));
    }
    #[test]
    fn test_smush_anim_v2() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from The Dig
        test_decoding("smush", "smushv1", "assets/Game/smush/PIGOUT.SAN", Some(42), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x74794559, 0x78a1e484, 0x379a1eec, 0x0609e0b2]));
        // sample from Full Throttle
        test_decoding("smush", "smushv1", "assets/Game/smush/FIRE.SAN", Some(16), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x442f73b9, 0x0b98d80f, 0xee2f0e19, 0xa555a33d]));
        // sample from Mortimer and the Riddles of the Medallion
        test_decoding("smush", "smushv1", "assets/Game/smush/FOREST1.SAN", Some(24), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xd5b71505, 0x0ffe79dd, 0xc274dbaf, 0x8b952271]));
        // sample from Curse of Monkey Island
        test_decoding("smush", "smushv1", "assets/Game/smush/ZAP010.SAN", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x369839f1, 0x2daab242, 0x23995d80, 0x501fbe09]));
        // sample from Jedi Knight: Mysteries of the Sith
        test_decoding("smush", "smushv1", "assets/Game/smush/S2L4ECS.SAN", Some(42), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x4525b5f3, 0x9fe5fb23, 0xf5f27980, 0x12589ce1]));
    }
}

const C37_MV: [[i8; 255 * 2]; 3] = [
  [
    0,   0,   1,   0,   2,   0,   3,   0,   5,   0,   8,   0,  13,   0,  21,
    0,  -1,   0,  -2,   0,  -3,   0,  -5,   0,  -8,   0, -13,   0, -17,   0,
  -21,   0,   0,   1,   1,   1,   2,   1,   3,   1,   5,   1,   8,   1,  13,
    1,  21,   1,  -1,   1,  -2,   1,  -3,   1,  -5,   1,  -8,   1, -13,   1,
  -17,   1, -21,   1,   0,   2,   1,   2,   2,   2,   3,   2,   5,   2,   8,
    2,  13,   2,  21,   2,  -1,   2,  -2,   2,  -3,   2,  -5,   2,  -8,   2,
  -13,   2, -17,   2, -21,   2,   0,   3,   1,   3,   2,   3,   3,   3,   5,
    3,   8,   3,  13,   3,  21,   3,  -1,   3,  -2,   3,  -3,   3,  -5,   3,
   -8,   3, -13,   3, -17,   3, -21,   3,   0,   5,   1,   5,   2,   5,   3,
    5,   5,   5,   8,   5,  13,   5,  21,   5,  -1,   5,  -2,   5,  -3,   5,
   -5,   5,  -8,   5, -13,   5, -17,   5, -21,   5,   0,   8,   1,   8,   2,
    8,   3,   8,   5,   8,   8,   8,  13,   8,  21,   8,  -1,   8,  -2,   8,
   -3,   8,  -5,   8,  -8,   8, -13,   8, -17,   8, -21,   8,   0,  13,   1,
   13,   2,  13,   3,  13,   5,  13,   8,  13,  13,  13,  21,  13,  -1,  13,
   -2,  13,  -3,  13,  -5,  13,  -8,  13, -13,  13, -17,  13, -21,  13,   0,
   21,   1,  21,   2,  21,   3,  21,   5,  21,   8,  21,  13,  21,  21,  21,
   -1,  21,  -2,  21,  -3,  21,  -5,  21,  -8,  21, -13,  21, -17,  21, -21,
   21,   0,  -1,   1,  -1,   2,  -1,   3,  -1,   5,  -1,   8,  -1,  13,  -1,
   21,  -1,  -1,  -1,  -2,  -1,  -3,  -1,  -5,  -1,  -8,  -1, -13,  -1, -17,
   -1, -21,  -1,   0,  -2,   1,  -2,   2,  -2,   3,  -2,   5,  -2,   8,  -2,
   13,  -2,  21,  -2,  -1,  -2,  -2,  -2,  -3,  -2,  -5,  -2,  -8,  -2, -13,
   -2, -17,  -2, -21,  -2,   0,  -3,   1,  -3,   2,  -3,   3,  -3,   5,  -3,
    8,  -3,  13,  -3,  21,  -3,  -1,  -3,  -2,  -3,  -3,  -3,  -5,  -3,  -8,
   -3, -13,  -3, -17,  -3, -21,  -3,   0,  -5,   1,  -5,   2,  -5,   3,  -5,
    5,  -5,   8,  -5,  13,  -5,  21,  -5,  -1,  -5,  -2,  -5,  -3,  -5,  -5,
   -5,  -8,  -5, -13,  -5, -17,  -5, -21,  -5,   0,  -8,   1,  -8,   2,  -8,
    3,  -8,   5,  -8,   8,  -8,  13,  -8,  21,  -8,  -1,  -8,  -2,  -8,  -3,
   -8,  -5,  -8,  -8,  -8, -13,  -8, -17,  -8, -21,  -8,   0, -13,   1, -13,
    2, -13,   3, -13,   5, -13,   8, -13,  13, -13,  21, -13,  -1, -13,  -2,
  -13,  -3, -13,  -5, -13,  -8, -13, -13, -13, -17, -13, -21, -13,   0, -17,
    1, -17,   2, -17,   3, -17,   5, -17,   8, -17,  13, -17,  21, -17,  -1,
  -17,  -2, -17,  -3, -17,  -5, -17,  -8, -17, -13, -17, -17, -17, -21, -17,
    0, -21,   1, -21,   2, -21,   3, -21,   5, -21,   8, -21,  13, -21,  21,
  -21,  -1, -21,  -2, -21,  -3, -21,  -5, -21,  -8, -21, -13, -21, -17, -21
  ], [
    0,   0,  -8, -29,   8, -29, -18, -25,  17, -25,   0, -23,  -6, -22,   6,
  -22, -13, -19,  12, -19,   0, -18,  25, -18, -25, -17,  -5, -17,   5, -17,
  -10, -15,  10, -15,   0, -14,  -4, -13,   4, -13,  19, -13, -19, -12,  -8,
  -11,  -2, -11,   0, -11,   2, -11,   8, -11, -15, -10,  -4, -10,   4, -10,
   15, -10,  -6,  -9,  -1,  -9,   1,  -9,   6,  -9, -29,  -8, -11,  -8,  -8,
   -8,  -3,  -8,   3,  -8,   8,  -8,  11,  -8,  29,  -8,  -5,  -7,  -2,  -7,
    0,  -7,   2,  -7,   5,  -7, -22,  -6,  -9,  -6,  -6,  -6,  -3,  -6,  -1,
   -6,   1,  -6,   3,  -6,   6,  -6,   9,  -6,  22,  -6, -17,  -5,  -7,  -5,
   -4,  -5,  -2,  -5,   0,  -5,   2,  -5,   4,  -5,   7,  -5,  17,  -5, -13,
   -4, -10,  -4,  -5,  -4,  -3,  -4,  -1,  -4,   0,  -4,   1,  -4,   3,  -4,
    5,  -4,  10,  -4,  13,  -4,  -8,  -3,  -6,  -3,  -4,  -3,  -3,  -3,  -2,
   -3,  -1,  -3,   0,  -3,   1,  -3,   2,  -3,   4,  -3,   6,  -3,   8,  -3,
  -11,  -2,  -7,  -2,  -5,  -2,  -3,  -2,  -2,  -2,  -1,  -2,   0,  -2,   1,
   -2,   2,  -2,   3,  -2,   5,  -2,   7,  -2,  11,  -2,  -9,  -1,  -6,  -1,
   -4,  -1,  -3,  -1,  -2,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,
   -1,   4,  -1,   6,  -1,   9,  -1, -31,   0, -23,   0, -18,   0, -14,   0,
  -11,   0,  -7,   0,  -5,   0,  -4,   0,  -3,   0,  -2,   0,  -1,   0,   0,
  -31,   1,   0,   2,   0,   3,   0,   4,   0,   5,   0,   7,   0,  11,   0,
   14,   0,  18,   0,  23,   0,  31,   0,  -9,   1,  -6,   1,  -4,   1,  -3,
    1,  -2,   1,  -1,   1,   0,   1,   1,   1,   2,   1,   3,   1,   4,   1,
    6,   1,   9,   1, -11,   2,  -7,   2,  -5,   2,  -3,   2,  -2,   2,  -1,
    2,   0,   2,   1,   2,   2,   2,   3,   2,   5,   2,   7,   2,  11,   2,
   -8,   3,  -6,   3,  -4,   3,  -2,   3,  -1,   3,   0,   3,   1,   3,   2,
    3,   3,   3,   4,   3,   6,   3,   8,   3, -13,   4, -10,   4,  -5,   4,
   -3,   4,  -1,   4,   0,   4,   1,   4,   3,   4,   5,   4,  10,   4,  13,
    4, -17,   5,  -7,   5,  -4,   5,  -2,   5,   0,   5,   2,   5,   4,   5,
    7,   5,  17,   5, -22,   6,  -9,   6,  -6,   6,  -3,   6,  -1,   6,   1,
    6,   3,   6,   6,   6,   9,   6,  22,   6,  -5,   7,  -2,   7,   0,   7,
    2,   7,   5,   7, -29,   8, -11,   8,  -8,   8,  -3,   8,   3,   8,   8,
    8,  11,   8,  29,   8,  -6,   9,  -1,   9,   1,   9,   6,   9, -15,  10,
   -4,  10,   4,  10,  15,  10,  -8,  11,  -2,  11,   0,  11,   2,  11,   8,
   11,  19,  12, -19,  13,  -4,  13,   4,  13,   0,  14, -10,  15,  10,  15,
   -5,  17,   5,  17,  25,  17, -25,  18,   0,  18, -12,  19,  13,  19,  -6,
   22,   6,  22,   0,  23, -17,  25,  18,  25,  -8,  29,   8,  29,   0,  31
  ], [
    0,   0,  -6, -22,   6, -22, -13, -19,  12, -19,   0, -18,  -5, -17,   5,
  -17, -10, -15,  10, -15,   0, -14,  -4, -13,   4, -13,  19, -13, -19, -12,
   -8, -11,  -2, -11,   0, -11,   2, -11,   8, -11, -15, -10,  -4, -10,   4,
  -10,  15, -10,  -6,  -9,  -1,  -9,   1,  -9,   6,  -9, -11,  -8,  -8,  -8,
   -3,  -8,   0,  -8,   3,  -8,   8,  -8,  11,  -8,  -5,  -7,  -2,  -7,   0,
   -7,   2,  -7,   5,  -7, -22,  -6,  -9,  -6,  -6,  -6,  -3,  -6,  -1,  -6,
    1,  -6,   3,  -6,   6,  -6,   9,  -6,  22,  -6, -17,  -5,  -7,  -5,  -4,
   -5,  -2,  -5,  -1,  -5,   0,  -5,   1,  -5,   2,  -5,   4,  -5,   7,  -5,
   17,  -5, -13,  -4, -10,  -4,  -5,  -4,  -3,  -4,  -2,  -4,  -1,  -4,   0,
   -4,   1,  -4,   2,  -4,   3,  -4,   5,  -4,  10,  -4,  13,  -4,  -8,  -3,
   -6,  -3,  -4,  -3,  -3,  -3,  -2,  -3,  -1,  -3,   0,  -3,   1,  -3,   2,
   -3,   3,  -3,   4,  -3,   6,  -3,   8,  -3, -11,  -2,  -7,  -2,  -5,  -2,
   -4,  -2,  -3,  -2,  -2,  -2,  -1,  -2,   0,  -2,   1,  -2,   2,  -2,   3,
   -2,   4,  -2,   5,  -2,   7,  -2,  11,  -2,  -9,  -1,  -6,  -1,  -5,  -1,
   -4,  -1,  -3,  -1,  -2,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,
   -1,   4,  -1,   5,  -1,   6,  -1,   9,  -1, -23,   0, -18,   0, -14,   0,
  -11,   0,  -7,   0,  -5,   0,  -4,   0,  -3,   0,  -2,   0,  -1,   0,   0,
  -23,   1,   0,   2,   0,   3,   0,   4,   0,   5,   0,   7,   0,  11,   0,
   14,   0,  18,   0,  23,   0,  -9,   1,  -6,   1,  -5,   1,  -4,   1,  -3,
    1,  -2,   1,  -1,   1,   0,   1,   1,   1,   2,   1,   3,   1,   4,   1,
    5,   1,   6,   1,   9,   1, -11,   2,  -7,   2,  -5,   2,  -4,   2,  -3,
    2,  -2,   2,  -1,   2,   0,   2,   1,   2,   2,   2,   3,   2,   4,   2,
    5,   2,   7,   2,  11,   2,  -8,   3,  -6,   3,  -4,   3,  -3,   3,  -2,
    3,  -1,   3,   0,   3,   1,   3,   2,   3,   3,   3,   4,   3,   6,   3,
    8,   3, -13,   4, -10,   4,  -5,   4,  -3,   4,  -2,   4,  -1,   4,   0,
    4,   1,   4,   2,   4,   3,   4,   5,   4,  10,   4,  13,   4, -17,   5,
   -7,   5,  -4,   5,  -2,   5,  -1,   5,   0,   5,   1,   5,   2,   5,   4,
    5,   7,   5,  17,   5, -22,   6,  -9,   6,  -6,   6,  -3,   6,  -1,   6,
    1,   6,   3,   6,   6,   6,   9,   6,  22,   6,  -5,   7,  -2,   7,   0,
    7,   2,   7,   5,   7, -11,   8,  -8,   8,  -3,   8,   0,   8,   3,   8,
    8,   8,  11,   8,  -6,   9,  -1,   9,   1,   9,   6,   9, -15,  10,  -4,
   10,   4,  10,  15,  10,  -8,  11,  -2,  11,   0,  11,   2,  11,   8,  11,
   19,  12, -19,  13,  -4,  13,   4,  13,   0,  14, -10,  15,  10,  15,  -5,
   17,   5,  17,   0,  18, -12,  19,  13,  19,  -6,  22,   6,  22,   0,  23
  ]
];

const C47_MV: [[[i8; 2]; 255]; 2] = [
  [
    [  0,   0], [ -1, -43], [  6, -43], [ -9, -42], [ 13, -41],
    [-16, -40], [ 19, -39], [-23, -36], [ 26, -34], [ -2, -33],
    [  4, -33], [-29, -32], [ -9, -32], [ 11, -31], [-16, -29],
    [ 32, -29], [ 18, -28], [-34, -26], [-22, -25], [ -1, -25],
    [  3, -25], [ -7, -24], [  8, -24], [ 24, -23], [ 36, -23],
    [-12, -22], [ 13, -21], [-38, -20], [  0, -20], [-27, -19],
    [ -4, -19], [  4, -19], [-17, -18], [ -8, -17], [  8, -17],
    [ 18, -17], [ 28, -17], [ 39, -17], [-12, -15], [ 12, -15],
    [-21, -14], [ -1, -14], [  1, -14], [-41, -13], [ -5, -13],
    [  5, -13], [ 21, -13], [-31, -12], [-15, -11], [ -8, -11],
    [  8, -11], [ 15, -11], [ -2, -10], [  1, -10], [ 31, -10],
    [-23,  -9], [-11,  -9], [ -5,  -9], [  4,  -9], [ 11,  -9],
    [ 42,  -9], [  6,  -8], [ 24,  -8], [-18,  -7], [ -7,  -7],
    [ -3,  -7], [ -1,  -7], [  2,  -7], [ 18,  -7], [-43,  -6],
    [-13,  -6], [ -4,  -6], [  4,  -6], [  8,  -6], [-33,  -5],
    [ -9,  -5], [ -2,  -5], [  0,  -5], [  2,  -5], [  5,  -5],
    [ 13,  -5], [-25,  -4], [ -6,  -4], [ -3,  -4], [  3,  -4],
    [  9,  -4], [-19,  -3], [ -7,  -3], [ -4,  -3], [ -2,  -3],
    [ -1,  -3], [  0,  -3], [  1,  -3], [  2,  -3], [  4,  -3],
    [  6,  -3], [ 33,  -3], [-14,  -2], [-10,  -2], [ -5,  -2],
    [ -3,  -2], [ -2,  -2], [ -1,  -2], [  0,  -2], [  1,  -2],
    [  2,  -2], [  3,  -2], [  5,  -2], [  7,  -2], [ 14,  -2],
    [ 19,  -2], [ 25,  -2], [ 43,  -2], [ -7,  -1], [ -3,  -1],
    [ -2,  -1], [ -1,  -1], [  0,  -1], [  1,  -1], [  2,  -1],
    [  3,  -1], [ 10,  -1], [ -5,   0], [ -3,   0], [ -2,   0],
    [ -1,   0], [  1,   0], [  2,   0], [  3,   0], [  5,   0],
    [  7,   0], [-10,   1], [ -7,   1], [ -3,   1], [ -2,   1],
    [ -1,   1], [  0,   1], [  1,   1], [  2,   1], [  3,   1],
    [-43,   2], [-25,   2], [-19,   2], [-14,   2], [ -5,   2],
    [ -3,   2], [ -2,   2], [ -1,   2], [  0,   2], [  1,   2],
    [  2,   2], [  3,   2], [  5,   2], [  7,   2], [ 10,   2],
    [ 14,   2], [-33,   3], [ -6,   3], [ -4,   3], [ -2,   3],
    [ -1,   3], [  0,   3], [  1,   3], [  2,   3], [  4,   3],
    [ 19,   3], [ -9,   4], [ -3,   4], [  3,   4], [  7,   4],
    [ 25,   4], [-13,   5], [ -5,   5], [ -2,   5], [  0,   5],
    [  2,   5], [  5,   5], [  9,   5], [ 33,   5], [ -8,   6],
    [ -4,   6], [  4,   6], [ 13,   6], [ 43,   6], [-18,   7],
    [ -2,   7], [  0,   7], [  2,   7], [  7,   7], [ 18,   7],
    [-24,   8], [ -6,   8], [-42,   9], [-11,   9], [ -4,   9],
    [  5,   9], [ 11,   9], [ 23,   9], [-31,  10], [ -1,  10],
    [  2,  10], [-15,  11], [ -8,  11], [  8,  11], [ 15,  11],
    [ 31,  12], [-21,  13], [ -5,  13], [  5,  13], [ 41,  13],
    [ -1,  14], [  1,  14], [ 21,  14], [-12,  15], [ 12,  15],
    [-39,  17], [-28,  17], [-18,  17], [ -8,  17], [  8,  17],
    [ 17,  18], [ -4,  19], [  0,  19], [  4,  19], [ 27,  19],
    [ 38,  20], [-13,  21], [ 12,  22], [-36,  23], [-24,  23],
    [ -8,  24], [  7,  24], [ -3,  25], [  1,  25], [ 22,  25],
    [ 34,  26], [-18,  28], [-32,  29], [ 16,  29], [-11,  31],
    [  9,  32], [ 29,  32], [ -4,  33], [  2,  33], [-26,  34],
    [ 23,  36], [-19,  39], [ 16,  40], [-13,  41], [  9,  42],
    [ -6,  43], [  1,  43], [  0,   0], [  0,   0], [  0,   0],
  ], [
    [  0,   0], [  1,   0], [  2,   0], [  3,   0], [  5,   0],
    [  8,   0], [ 13,   0], [ 21,   0], [ -1,   0], [ -2,   0],
    [ -3,   0], [ -5,   0], [ -8,   0], [-13,   0], [-17,   0],
    [-21,   0], [  0,   1], [  1,   1], [  2,   1], [  3,   1],
    [  5,   1], [  8,   1], [ 13,   1], [ 21,   1], [ -1,   1],
    [ -2,   1], [ -3,   1], [ -5,   1], [ -8,   1], [-13,   1],
    [-17,   1], [-21,   1], [  0,   2], [  1,   2], [  2,   2],
    [  3,   2], [  5,   2], [  8,   2], [ 13,   2], [ 21,   2],
    [ -1,   2], [ -2,   2], [ -3,   2], [ -5,   2], [ -8,   2],
    [-13,   2], [-17,   2], [-21,   2], [  0,   3], [  1,   3],
    [  2,   3], [  3,   3], [  5,   3], [  8,   3], [ 13,   3],
    [ 21,   3], [ -1,   3], [ -2,   3], [ -3,   3], [ -5,   3],
    [ -8,   3], [-13,   3], [-17,   3], [-21,   3], [  0,   5],
    [  1,   5], [  2,   5], [  3,   5], [  5,   5], [  8,   5],
    [ 13,   5], [ 21,   5], [ -1,   5], [ -2,   5], [ -3,   5],
    [ -5,   5], [ -8,   5], [-13,   5], [-17,   5], [-21,   5],
    [  0,   8], [  1,   8], [  2,   8], [  3,   8], [  5,   8],
    [  8,   8], [ 13,   8], [ 21,   8], [ -1,   8], [ -2,   8],
    [ -3,   8], [ -5,   8], [ -8,   8], [-13,   8], [-17,   8],
    [-21,   8], [  0,  13], [  1,  13], [  2,  13], [  3,  13],
    [  5,  13], [  8,  13], [ 13,  13], [ 21,  13], [ -1,  13],
    [ -2,  13], [ -3,  13], [ -5,  13], [ -8,  13], [-13,  13],
    [-17,  13], [-21,  13], [  0,  21], [  1,  21], [  2,  21],
    [  3,  21], [  5,  21], [  8,  21], [ 13,  21], [ 21,  21],
    [ -1,  21], [ -2,  21], [ -3,  21], [ -5,  21], [ -8,  21],
    [-13,  21], [-17,  21], [-21,  21], [  0,  -1], [  1,  -1],
    [  2,  -1], [  3,  -1], [  5,  -1], [  8,  -1], [ 13,  -1],
    [ 21,  -1], [ -1,  -1], [ -2,  -1], [ -3,  -1], [ -5,  -1],
    [ -8,  -1], [-13,  -1], [-17,  -1], [-21,  -1], [  0,  -2],
    [  1,  -2], [  2,  -2], [  3,  -2], [  5,  -2], [  8,  -2],
    [ 13,  -2], [ 21,  -2], [ -1,  -2], [ -2,  -2], [ -3,  -2],
    [ -5,  -2], [ -8,  -2], [-13,  -2], [-17,  -2], [-21,  -2],
    [  0,  -3], [  1,  -3], [  2,  -3], [  3,  -3], [  5,  -3],
    [  8,  -3], [ 13,  -3], [ 21,  -3], [ -1,  -3], [ -2,  -3],
    [ -3,  -3], [ -5,  -3], [ -8,  -3], [-13,  -3], [-17,  -3],
    [-21,  -3], [  0,  -5], [  1,  -5], [  2,  -5], [  3,  -5],
    [  5,  -5], [  8,  -5], [ 13,  -5], [ 21,  -5], [ -1,  -5],
    [ -2,  -5], [ -3,  -5], [ -5,  -5], [ -8,  -5], [-13,  -5],
    [-17,  -5], [-21,  -5], [  0,  -8], [  1,  -8], [  2,  -8],
    [  3,  -8], [  5,  -8], [  8,  -8], [ 13,  -8], [ 21,  -8],
    [ -1,  -8], [ -2,  -8], [ -3,  -8], [ -5,  -8], [ -8,  -8],
    [-13,  -8], [-17,  -8], [-21,  -8], [  0, -13], [  1, -13],
    [  2, -13], [  3, -13], [  5, -13], [  8, -13], [ 13, -13],
    [ 21, -13], [ -1, -13], [ -2, -13], [ -3, -13], [ -5, -13],
    [ -8, -13], [-13, -13], [-17, -13], [-21, -13], [  0, -17],
    [  1, -17], [  2, -17], [  3, -17], [  5, -17], [  8, -17],
    [ 13, -17], [ 21, -17], [ -1, -17], [ -2, -17], [ -3, -17],
    [ -5, -17], [ -8, -17], [-13, -17], [-17, -17], [-21, -17],
    [  0, -21], [  1, -21], [  2, -21], [  3, -21], [  5, -21],
    [  8, -21], [ 13, -21], [ 21, -21], [ -1, -21], [ -2, -21],
    [ -3, -21], [ -5, -21], [ -8, -21], [-13, -21], [-17, -21]
  ]
];
