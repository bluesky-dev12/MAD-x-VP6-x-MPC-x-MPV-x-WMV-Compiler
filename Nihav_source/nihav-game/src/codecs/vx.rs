use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_core::io::intcode::*;
use std::str::FromStr;

use std::ops::*;

#[derive(Clone,Copy,Default,Debug,PartialEq)]
struct MV {
    x: i8,
    y: i8,
}

impl Add for MV {
    type Output = MV;
    fn add(self, other: MV) -> MV { MV { x: self.x + other.x, y: self.y + other.y } }
}

impl AddAssign for MV {
    fn add_assign(&mut self, other: MV) { self.x += other.x; self.y += other.y; }
}

impl Sub for MV {
    type Output = MV;
    fn sub(self, other: MV) -> MV { MV { x: self.x - other.x, y: self.y - other.y } }
}

impl SubAssign for MV {
    fn sub_assign(&mut self, other: MV) { self.x -= other.x; self.y -= other.y; }
}

impl MV {
    fn pred(t: MV, tl: MV, l: MV) -> MV {
        let x0 = i16::from(t.x);
        let x1 = i16::from(tl.x);
        let x2 = i16::from(l.x);
        let y0 = i16::from(t.y);
        let y1 = i16::from(tl.y);
        let y2 = i16::from(l.y);
        let x = x0 + x1 + x2 - x0.min(x1).min(x2) - x0.max(x1).max(x2);
        let y = y0 + y1 + y2 - y0.min(y1).min(y2) - y0.max(y1).max(y2);
        MV{ x: x as i8, y: y as i8 }
    }
}

trait ReadCodes {
    fn read_gammap(&mut self) -> BitReaderResult<u32>;
    fn read_gammap_s(&mut self) -> BitReaderResult<i32>;
    fn read_unary(&mut self) -> BitReaderResult<u32>;
}

impl<'a> ReadCodes for BitReader<'a> {
    fn read_gammap(&mut self) -> BitReaderResult<u32> {
        Ok(self.read_code(UintCodeType::GammaP)? - 1)
    }
    fn read_gammap_s(&mut self) -> BitReaderResult<i32> {
        let val = self.read_code(UintCodeType::GammaP)?;
        if (val & 1) == 0 {
            Ok((val >> 1) as i32)
        } else {
            Ok((1 - (val as i32)) >> 1)
        }
    }
    fn read_unary(&mut self) -> BitReaderResult<u32> {
        self.read_code(UintCodeType::UnaryZeroes)
    }
}

const CUR_BUF: usize = 3;
const CHROMA_OFF: usize = 256 * 256;

macro_rules! mc {
    ($bufs: expr, $src_id: expr, $dst_id: expr, $mx: expr, $my: expr, $xpos: expr, $ypos: expr, $w: expr, $h: expr, $ydelta: expr, $udelta: expr, $vdelta: expr, $width: expr, $height: expr) => {
        if ($mx + ($xpos as isize) < 0) || ($mx + (($xpos + $w) as isize) > ($width as isize)) {
            return Err(DecoderError::InvalidData);
        }
        if ($my + ($ypos as isize) < 0) || ($my + (($ypos + $h) as isize) > ($height as isize)) {
            return Err(DecoderError::InvalidData);
        }

        let sx = (($xpos as isize) + $mx) as usize;
        let sy = (($ypos as isize) + $my) as usize;

        let mut soff = sx + sy * 256;
        let mut doff = $xpos + $ypos * 256;
        for _y in 0..$h {
            for x in 0..$w {
                $bufs[$dst_id][doff + x] = (i32::from($bufs[$src_id][soff + x]) + $ydelta).max(0).min(255) as u8;
            }
            soff += 256;
            doff += 256;
        }
        let mut soff = CHROMA_OFF + sx / 2 + (sy / 2) * 256;
        let mut doff = CHROMA_OFF + $xpos / 2 + $ypos / 2 * 256;
        for _y in 0..$h / 2 {
            for x in 0..$w / 2 {
                $bufs[$dst_id][doff + x] = (i32::from($bufs[$src_id][soff + x]) + $udelta).max(0).min(255) as u8;
                $bufs[$dst_id][doff + x + 128] = (i32::from($bufs[$src_id][soff + x + 128]) + $vdelta).max(0).min(255) as u8;
            }
            soff += 256;
            doff += 256;
        }
    }
}

fn pred_dc(buf: &mut [u8], mut pos: usize, x: usize, y: usize, w: usize, h: usize) {
    let dc = if x == 0 && y == 0 {
            128
        } else if y == 0 {
            let mut sum = 0;
            let hh = h as u16;
            for i in 0..h {
                sum += u16::from(buf[pos - 1 + i * 256]);
            }
            ((sum + hh / 2) / hh) as u8
        } else if x == 0 {
            let mut sum = 0;
            let ww = w as u16;
            for i in 0..w {
                sum += u16::from(buf[pos - 256 + i]);
            }
            ((sum + ww / 2) / ww) as u8
        } else {
            let mut sum = 0;
            let ww = w as u16;
            for i in 0..w {
                sum += u16::from(buf[pos - 256 + i]);
            }
            let wdc = (sum + ww / 2) / ww;

            let mut sum = 0;
            let hh = h as u16;
            for i in 0..h {
                sum += u16::from(buf[pos - 1 + i * 256]);
            }
            let hdc = (sum + hh / 2) / hh;

            ((wdc + hdc + 1) >> 1) as u8
        };
    for _ in 0..h {
        for x in 0..w {
            buf[pos + x] = dc;
        }
        pos += 256;
    }
}
fn pred_dc4x4(buf: &mut [u8], mut pos: usize, x: usize, y: usize) {
    if x == 0 && y == 0 {
        for _ in 0..4 {
            for x in 0..4 {
                buf[x] = 0x80;
            }
            pos += 256;
        }
        return;
    }
    let mut sum = 0;
    let mut shift = 1;
    if y != 0 {
        for i in 0..4 {
            sum += u16::from(buf[pos - 256 + i]);
        }
        sum += 2;
        shift += 1;
    }
    if x != 0 {
        for i in 0..4 {
            sum += u16::from(buf[pos + i * 256 - 1]);
        }
        sum += 2;
        shift += 1;
    }
    let dc = (sum >> shift) as u8;
    for _ in 0..4 {
        for x in 0..4 {
            buf[pos + x] = dc;
        }
        pos += 256;
    }
}

fn pred_hor(buf: &mut [u8], mut pos: usize, w: usize, h: usize) {
    for _ in 0..h {
        for x in 0..w {
            buf[pos + x] = buf[pos - 1];
        }
        pos += 256;
    }
}

fn pred_ver(buf: &mut [u8], mut pos: usize, w: usize, h: usize) {
    for _ in 0..h {
        for x in 0..w {
            buf[pos + x] = buf[pos + x - 256];
        }
        pos += 256;
    }
}

fn avg(a: u8, b: u8) -> u8 {
    ((u16::from(a) + u16::from(b) + 1) >> 1) as u8
}
fn avg_nr(a: u8, b: u8) -> u8 {
    ((u16::from(a) + u16::from(b)) >> 1) as u8
}
fn interp2(a: u8, b: u8, c: u8) -> u8 {
    ((u16::from(a) + 2 * u16::from(b) + u16::from(c) + 2) >> 2) as u8
}

fn pred_ddown_left(blk: &mut [u8], pos: usize) {
    let (t0, t1, t2, t3) = (blk[pos - 256], blk[pos - 256 + 1], blk[pos - 256 + 2], blk[pos - 256 + 3]);
    let (t4, t5, t6, t7) = (blk[pos - 256 + 4], blk[pos - 256 + 5], blk[pos - 256 + 6], blk[pos - 256 + 7]);

    blk[pos + 0 + 0 * 256] = interp2(t0, t1, t2);
    let pix = interp2(t1, t2, t3);
    blk[pos + 1 + 0 * 256] = pix;
    blk[pos + 0 + 1 * 256] = pix;
    let pix = interp2(t2, t3, t4);
    blk[pos + 2 + 0 * 256] = pix;
    blk[pos + 1 + 1 * 256] = pix;
    blk[pos + 0 + 2 * 256] = pix;
    let pix = interp2(t3, t4, t5);
    blk[pos + 3 + 0 * 256] = pix;
    blk[pos + 2 + 1 * 256] = pix;
    blk[pos + 1 + 2 * 256] = pix;
    blk[pos + 0 + 3 * 256] = pix;
    let pix = interp2(t4, t5, t6);
    blk[pos + 3 + 1 * 256] = pix;
    blk[pos + 2 + 2 * 256] = pix;
    blk[pos + 1 + 3 * 256] = pix;
    let pix = interp2(t5, t6, t7);
    blk[pos + 3 + 2 * 256] = pix;
    blk[pos + 2 + 3 * 256] = pix;
    blk[pos + 3 + 3 * 256] = interp2(t6, t7, t7);
}
fn pred_ddown_right(blk: &mut [u8], pos: usize) {
    let (l0, l1, l2, l3) = (blk[pos - 1], blk[pos + 256 - 1], blk[pos + 256 * 2 - 1], blk[pos + 256 * 3 - 1]);
    let (tl, t0, t1, t2) = (blk[pos - 256 - 1], blk[pos - 256], blk[pos - 256 + 1], blk[pos - 256 + 2]);
    let t3 = blk[pos - 256 + 3];

    blk[pos + 0 + 3 * 256] = interp2(l1, l2, l3);
    let pix = interp2(l0, l1, l2);
    blk[pos + 0 + 2 * 256] = pix;
    blk[pos + 1 + 3 * 256] = pix;
    let pix = interp2(tl, l0, l1);
    blk[pos + 0 + 1 * 256] = pix;
    blk[pos + 1 + 2 * 256] = pix;
    blk[pos + 2 + 3 * 256] = pix;
    let pix = interp2(l0, tl, t0);
    blk[pos + 0 + 0 * 256] = pix;
    blk[pos + 1 + 1 * 256] = pix;
    blk[pos + 2 + 2 * 256] = pix;
    blk[pos + 3 + 3 * 256] = pix;
    let pix = interp2(tl, t0, t1);
    blk[pos + 1 + 0 * 256] = pix;
    blk[pos + 2 + 1 * 256] = pix;
    blk[pos + 3 + 2 * 256] = pix;
    let pix = interp2(t0, t1, t2);
    blk[pos + 2 + 0 * 256] = pix;
    blk[pos + 3 + 1 * 256] = pix;
    blk[pos + 3 + 0 * 256] = interp2(t1, t2, t3);
}
fn pred_ver_right(blk: &mut [u8], pos: usize) {
    let (l0, l1, l2) = (blk[pos - 1], blk[pos + 256 - 1], blk[pos + 256 * 2 - 1]);
    let (tl, t0, t1, t2, t3) = (blk[pos - 256 - 1], blk[pos - 256], blk[pos - 256 + 1], blk[pos - 256 + 2], blk[pos - 256 + 3]);

    blk[pos + 0 + 3 * 256] = interp2(l0, l1, l2);
    blk[pos + 0 + 2 * 256] = interp2(tl, l0, l1);
    let pix = interp2(l0, tl, t0);
    blk[pos + 0 + 1 * 256] = pix;
    blk[pos + 1 + 3 * 256] = pix;
    let pix = avg(tl, t0);
    blk[pos + 0 + 0 * 256] = pix;
    blk[pos + 1 + 2 * 256] = pix;
    let pix = interp2(tl, t0, t1);
    blk[pos + 1 + 1 * 256] = pix;
    blk[pos + 2 + 3 * 256] = pix;
    let pix = avg(t0, t1);
    blk[pos + 1 + 0 * 256] = pix;
    blk[pos + 2 + 2 * 256] = pix;
    let pix = interp2(t0, t1, t2);
    blk[pos + 2 + 1 * 256] = pix;
    blk[pos + 3 + 3 * 256] = pix;
    let pix = avg(t1, t2);
    blk[pos + 2 + 0 * 256] = pix;
    blk[pos + 3 + 2 * 256] = pix;
    blk[pos + 3 + 1 * 256] = interp2(t1, t2, t3);
    blk[pos + 3 + 0 * 256] = avg(t2, t3);
}
fn pred_hor_down(blk: &mut [u8], pos: usize) {
    let (l0, l1, l2, l3) = (blk[pos - 1], blk[pos + 256 - 1], blk[pos + 256 * 2 - 1], blk[pos + 256 * 3 - 1]);
    let (tl, t0, t1, t2) = (blk[pos - 256 - 1], blk[pos - 256], blk[pos - 256 + 1], blk[pos - 256 + 2]);

    blk[pos + 0 + 3 * 256] = avg(l2, l3);
    blk[pos + 1 + 3 * 256] = interp2(l1, l2, l3);
    let pix = avg(l1, l2);
    blk[pos + 0 + 2 * 256] = pix;
    blk[pos + 2 + 3 * 256] = pix;
    let pix = interp2(l0, l1, l2);
    blk[pos + 1 + 2 * 256] = pix;
    blk[pos + 3 + 3 * 256] = pix;
    let pix = avg(l0, l1);
    blk[pos + 0 + 1 * 256] = pix;
    blk[pos + 2 + 2 * 256] = pix;
    let pix = interp2(tl, l0, l1);
    blk[pos + 1 + 1 * 256] = pix;
    blk[pos + 3 + 2 * 256] = pix;
    let pix = avg(tl, l0);
    blk[pos + 0 + 0 * 256] = pix;
    blk[pos + 2 + 1 * 256] = pix;
    let pix = interp2(l0, tl, t0);
    blk[pos + 1 + 0 * 256] = pix;
    blk[pos + 3 + 1 * 256] = pix;
    blk[pos + 2 + 0 * 256] = interp2(tl, t0, t1);
    blk[pos + 3 + 0 * 256] = interp2(t0, t1, t2);
}
fn pred_ver_left(blk: &mut [u8], pos: usize) {
    let (t0, t1, t2, t3) = (blk[pos - 256], blk[pos - 256 + 1], blk[pos - 256 + 2], blk[pos - 256 + 3]);
    let (t4, t5, t6) = (blk[pos - 256 + 4], blk[pos - 256 + 5], blk[pos - 256 + 6]);

    blk[pos + 3 + 3 * 256] = interp2(t4, t5, t6);
    blk[pos + 3 + 2 * 256] = avg(t4, t5);
    let pix = interp2(t3, t4, t5);
    blk[pos + 3 + 1 * 256] = pix;
    blk[pos + 2 + 3 * 256] = pix;
    let pix = avg(t3, t4);
    blk[pos + 3 + 0 * 256] = pix;
    blk[pos + 2 + 2 * 256] = pix;
    let pix = interp2(t2, t3, t4);
    blk[pos + 2 + 1 * 256] = pix;
    blk[pos + 1 + 3 * 256] = pix;
    let pix = avg(t2, t3);
    blk[pos + 2 + 0 * 256] = pix;
    blk[pos + 1 + 2 * 256] = pix;
    let pix = interp2(t1, t2, t3);
    blk[pos + 1 + 1 * 256] = pix;
    blk[pos + 0 + 3 * 256] = pix;
    let pix = avg(t1, t2);
    blk[pos + 1 + 0 * 256] = pix;
    blk[pos + 0 + 2 * 256] = pix;
    blk[pos + 0 + 1 * 256] = interp2(t0, t1, t2);
    blk[pos + 0 + 0 * 256] = avg(t0, t1);
}
fn pred_hor_up(blk: &mut [u8], pos: usize) {
    let (l0, l1, l2, l3) = (blk[pos - 1], blk[pos + 256 - 1], blk[pos + 256 * 2 - 1], blk[pos + 256 * 3 - 1]);

    blk[pos + 0 + 0 * 256] = avg(l0, l1);
    blk[pos + 1 + 0 * 256] = interp2(l0, l1, l2);
    let pix = avg(l1, l2);
    blk[pos + 2 + 0 * 256] = pix;
    blk[pos + 0 + 1 * 256] = pix;
    let pix = interp2(l1, l2, l3);
    blk[pos + 3 + 0 * 256] = pix;
    blk[pos + 1 + 1 * 256] = pix;
    let pix = avg(l2, l3);
    blk[pos + 2 + 1 * 256] = pix;
    blk[pos + 0 + 2 * 256] = pix;
    let pix = interp2(l2, l3, l3);
    blk[pos + 3 + 1 * 256] = pix;
    blk[pos + 1 + 2 * 256] = pix;
    blk[pos + 0 + 3 * 256] = l3;
    blk[pos + 2 + 2 * 256] = l3;
    blk[pos + 3 + 2 * 256] = l3;
    blk[pos + 1 + 3 * 256] = l3;
    blk[pos + 2 + 3 * 256] = l3;
    blk[pos + 3 + 3 * 256] = l3;
}

fn pred_plane(blk: &mut [u8], pos: usize, w: usize, h: usize) {
    if w == 1 && h == 1 {
        return;
    }
    if h == 1 {
        blk[pos + w / 2 - 1] = avg_nr(blk[pos - 1], blk[pos + w - 1]);
        if w > 2 {
            pred_plane(blk, pos,         w / 2, 1);
            pred_plane(blk, pos + w / 2, w / 2, 1);
        }
        return;
    }
    if w == 1 {
        blk[pos + (h / 2 - 1) * 256] = avg_nr(blk[pos - 256], blk[pos + (h - 1) * 256]);
        if h > 2 {
            pred_plane(blk, pos,                 1, h / 2);
            pred_plane(blk, pos + (h / 2) * 256, 1, h / 2);
        }
        return;
    }

    let is_even_block = ((w.trailing_zeros() + h.trailing_zeros()) & 1) == 0; // i.e. w*h = 256/64/16/4

    let hoff  = (h     - 1) * 256;
    let h2off = (h / 2 - 1) * 256;
    let woff  = w - 1;
    let w2off = w / 2 - 1;
    let dr = blk[pos + woff + hoff];
    let tr = blk[pos + woff - 256];
    let dl = blk[pos -    1 + hoff];

    let dm = avg_nr(dl, dr);
    blk[pos + w2off + hoff]  = dm;
    blk[pos + woff  + h2off] = avg_nr(tr, dr);
    let (val1, val2) = if is_even_block {
            (dm, blk[pos + w2off - 256])
        } else {
            (blk[pos + woff + h2off], blk[pos - 1 + h2off])
        };
    blk[pos + w2off + h2off] = avg_nr(val1, val2);

    let hw = w / 2;
    let hh = h / 2;
    pred_plane(blk, pos,                 hw, hh);
    pred_plane(blk, pos + hw,            hw, hh);
    pred_plane(blk, pos      + hh * 256, hw, hh);
    pred_plane(blk, pos + hw + hh * 256, hw, hh);
}
fn pred_plane_delta(blk: &mut [u8], pos: usize, w: usize, h: usize, delta: i32) {
    let tr = blk[pos + w - 1 - 256];
    let dl = blk[pos + 256 * (h - 1) - 1];
    let pred = avg(tr, dl).wrapping_add(delta as u8);
    blk[pos + 256 * (h - 1) + w - 1] = pred;
    pred_plane(blk, pos, w, h);
}

struct Codebooks {
    nc_cb:          [Codebook<u8>; 3],
    num_zero_cb:    [Codebook<u8>; 15],
    zero_run_cb:    [Codebook<u8>; 6],
}

fn map_idx(idx: usize) -> u8 { idx as u8 }
macro_rules! create_cb {
    ($bits_tab: expr, $lens_tab: expr) => {{
        let mut cbr = TableCodebookDescReader::new($bits_tab, $lens_tab, map_idx);
        Codebook::new(&mut cbr, CodebookMode::MSB).unwrap()
    }}
}

impl Codebooks {
    fn new() -> Self {
        let nc_cb0 = create_cb!(&NC_BITS[0], &NC_LENS[0]);
        let nc_cb1 = create_cb!(&NC_BITS[1], &NC_LENS[1]);
        let nc_cb2 = create_cb!(&NC_BITS[2], &NC_LENS[2]);

        let nz0  = create_cb!(&NUM_ZERO_BITS[ 0], &NUM_ZERO_LENS[ 0]);
        let nz1  = create_cb!(&NUM_ZERO_BITS[ 1], &NUM_ZERO_LENS[ 1]);
        let nz2  = create_cb!(&NUM_ZERO_BITS[ 2], &NUM_ZERO_LENS[ 2]);
        let nz3  = create_cb!(&NUM_ZERO_BITS[ 3], &NUM_ZERO_LENS[ 3]);
        let nz4  = create_cb!(&NUM_ZERO_BITS[ 4], &NUM_ZERO_LENS[ 4]);
        let nz5  = create_cb!(&NUM_ZERO_BITS[ 5], &NUM_ZERO_LENS[ 5]);
        let nz6  = create_cb!(&NUM_ZERO_BITS[ 6], &NUM_ZERO_LENS[ 6]);
        let nz7  = create_cb!(&NUM_ZERO_BITS[ 7], &NUM_ZERO_LENS[ 7]);
        let nz8  = create_cb!(&NUM_ZERO_BITS[ 8], &NUM_ZERO_LENS[ 8]);
        let nz9  = create_cb!(&NUM_ZERO_BITS[ 9], &NUM_ZERO_LENS[ 9]);
        let nz10 = create_cb!(&NUM_ZERO_BITS[10], &NUM_ZERO_LENS[10]);
        let nz11 = create_cb!(&NUM_ZERO_BITS[11], &NUM_ZERO_LENS[11]);
        let nz12 = create_cb!(&NUM_ZERO_BITS[12], &NUM_ZERO_LENS[12]);
        let nz13 = create_cb!(&NUM_ZERO_BITS[13], &NUM_ZERO_LENS[13]);
        let nz14 = create_cb!(&NUM_ZERO_BITS[14], &NUM_ZERO_LENS[14]);

        let zcb0 = create_cb!(&ZERO_RUN_BITS[0], &ZERO_RUN_LENS[0]);
        let zcb1 = create_cb!(&ZERO_RUN_BITS[1], &ZERO_RUN_LENS[1]);
        let zcb2 = create_cb!(&ZERO_RUN_BITS[2], &ZERO_RUN_LENS[2]);
        let zcb3 = create_cb!(&ZERO_RUN_BITS[3], &ZERO_RUN_LENS[3]);
        let zcb4 = create_cb!(&ZERO_RUN_BITS[4], &ZERO_RUN_LENS[4]);
        let zcb5 = create_cb!(&ZERO_RUN_BITS[5], &ZERO_RUN_LENS[5]);
        Self {
            nc_cb:          [nc_cb0, nc_cb1, nc_cb2],
            num_zero_cb:    [nz0, nz1, nz2, nz3, nz4, nz5, nz6, nz7, nz8, nz9, nz10, nz11, nz12, nz13, nz14],
            zero_run_cb:    [zcb0, zcb1, zcb2, zcb3, zcb4, zcb5],
        }
    }
}

macro_rules! transform {
    ($a: expr, $b: expr, $c: expr, $d: expr, $q0: expr, $q1: expr) => {
        let t0 = ($a + $c).wrapping_mul($q0);
        let t1 = ($a - $c).wrapping_mul($q0);
        let tb = $b.wrapping_mul($q1);
        let td = $d.wrapping_mul($q1);
        let t2 =  tb       + (td >> 1);
        let t3 = (tb >> 1) -  td;
        $a = t0 + t2;
        $b = t1 + t3;
        $c = t1 - t3;
        $d = t0 - t2;
    }
}

fn idct_add(qmat: &[i32; 8], blk: &mut [i32; 16], dst: &mut [u8]) {
    for i in 0..4 {
        transform!(blk[i + 0 * 4], blk[i + 1 * 4], blk[i + 2 * 4], blk[i + 3 * 4], qmat[i], qmat[i + 4]);
    }
    for (dline, row) in dst.chunks_mut(256).zip(blk.chunks_mut(4)) {
        transform!(row[0], row[1], row[2], row[3], 1, 1);
        for (out, coef) in dline.iter_mut().zip(row.iter_mut()) {
            *out = (i32::from(*out) + ((*coef + 0x20) >> 6)).max(0).min(255) as u8;
        }
    }
}

fn decode_coeffs(br: &mut BitReader, codebooks: &Codebooks, qmat: &[i32; 8], ctx: u8, dst: &mut [u8]) -> DecoderResult<u8> {
    const ZIGZAG: [usize; 16] = [
        0,  1,  4,  8,
        5,  2,  3,  6,
        9, 12, 13, 10,
        7, 11, 14, 15
    ];
    const MAX_LEVEL: [i32; 6] = [ 2, 5, 11, 23, 47, 0x8000 ];

    let (ncoeffs, nones) = if ctx < 8 {
            let sym                     = br.read_cb(&codebooks.nc_cb[NC_MAP[ctx as usize]])?;
            if sym == 0 {
                return Ok(0);
            }
            (sym >> 2, sym & 3)
        } else {
            let ncoeffs                 = (br.read(4)? + 1) as u8;
            let nones                   = br.read(2)? as u8;
            if ncoeffs < nones {
                return Ok(0);
            }
            (ncoeffs, nones)
        };
    let mut num_zero = if ncoeffs == 16 { 0 } else {
            br.read_cb(&codebooks.num_zero_cb[ncoeffs as usize - 1])?
        };
    validate!(ncoeffs + num_zero <= 16);
    let mut blk = [0i32; 16];
    let mut level = 0usize;
    let mut coef_left = ncoeffs;
    let mut ones_left = nones;
    let mut idx = ncoeffs + num_zero;
    while coef_left > 0 {
        let val = if ones_left > 0 {
                ones_left -= 1;
                if !br.read_bool()? { 1 } else { -1 }
            } else {
                let prefix              = br.read_unary()?;
                let val = if prefix < 15 {
                        (br.read(level as u8)? | (prefix << level)) as i32
                    } else {
                        (br.read(11)? + (15 << level)) as i32
                    };
                if val > MAX_LEVEL[level] {
                    level += 1;
                }
                if !br.read_bool()? {
                    val + 1
                } else {
                    -(val + 1)
                }
            };
        idx -= 1;
        blk[ZIGZAG[idx as usize]] = val;
        coef_left -= 1;
        if num_zero > 0 && coef_left > 0 {
            let run = if num_zero < 7 {
                    br.read_cb(&codebooks.zero_run_cb[num_zero as usize - 1])?
                } else {
                    if br.peek(3) != 0 {
                        7 - (br.read(3)? as u8)
                    } else {
                        (br.read_unary()? as u8) + 4
                    }
                };
            validate!(run <= num_zero);
            idx      -= run;
            num_zero -= run;
        }
    }
    idct_add(qmat, &mut blk, dst);
    Ok(ncoeffs)
}

const NCSTRIDE: usize = 64 + 1;

struct VXVideoDecoder {
    info:       NACodecInfoRef,
    width:      usize,
    height:     usize,
    buf:        [[u8; 256 * 392]; 4], // layout is luma with stride 256 and at 256*256 chroma lines with two components starting at positions 0 and 128 and stride 256
    refs:       [usize; 4],
    ipred4x4:   [u8; 25],
    y_ncoeffs:  [u8; NCSTRIDE * (256 / 4 + 1)],
    c_ncoeffs:  [u8; NCSTRIDE * (256 / 8 + 1)],
    mvs:        [MV; 16 * 16],
    pred_mv:    MV,
    cur_mv:     MV,
    qmat:       [i32; 8],
    codebooks:  Codebooks,
}

impl VXVideoDecoder {
    fn new() -> Self {
        Self {
            info:       NACodecInfoRef::default(),
            buf:        [[0x80; 256 * 392]; 4],
            width:      0,
            height:     0,
            refs:       [0, 1, 2, 3],
            ipred4x4:   [9; 25],
            y_ncoeffs:  [0; NCSTRIDE * (256 / 4 + 1)],
            c_ncoeffs:  [0; NCSTRIDE * (256 / 8 + 1)],
            mvs:        [MV::default(); 16 * 16],
            cur_mv:     MV::default(),
            pred_mv:    MV::default(),
            qmat:       [0; 8],
            codebooks:  Codebooks::new(),
        }
    }
    fn update_refs(&mut self) {
        for el in self.refs.iter_mut() {
            *el = (*el + 3) & 3;
        }
    }
    fn decode_frame(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        let mut mv_pos = 18;
        for ypos in (0..self.height).step_by(16) {
            for xpos in (0..self.width).step_by(16) {
                let left_mv = self.mvs[mv_pos - 1];
                let top_mv  = self.mvs[mv_pos - 17];
                let tl_mv   = self.mvs[mv_pos - 18];
                self.pred_mv = MV::pred(top_mv, tl_mv, left_mv);
                self.cur_mv  = MV::default();
                self.decode_block(br, xpos, ypos, 16, 16)?;
                self.mvs[mv_pos] = self.cur_mv;
                mv_pos += 1;
            }
            mv_pos -= self.width / 16;
            mv_pos += 18;
        }
        Ok(())
    }
    fn copy_block(&mut self, xpos: usize, ypos: usize, w: usize, h: usize, ref_buf: usize) -> DecoderResult<()> {
        let src = self.refs[ref_buf];
        let dst = self.refs[CUR_BUF];
        self.cur_mv = self.pred_mv;
        let mx = self.pred_mv.x as isize;
        let my = self.pred_mv.y as isize;
        mc!(self.buf, src, dst, mx, my, xpos, ypos, w, h, 0, 0, 0, self.width, self.height);
        Ok(())
    }
    fn do_mc(&mut self, br: &mut BitReader, xpos: usize, ypos: usize, w: usize, h: usize, ref_buf: usize) -> DecoderResult<()> {
        let src = self.refs[ref_buf];
        let dst = self.refs[CUR_BUF];
        let dx                          = br.read_gammap_s()? as i8;
        let dy                          = br.read_gammap_s()? as i8;
        self.cur_mv = self.pred_mv + MV { x: dx, y: dy };
        let mx = self.cur_mv.x as isize;
        let my = self.cur_mv.y as isize;
        mc!(self.buf, src, dst, mx, my, xpos, ypos, w, h, 0, 0, 0, self.width, self.height);

        Ok(())
    }
    fn do_mc_bias(&mut self, br: &mut BitReader, xpos: usize, ypos: usize, w: usize, h: usize) -> DecoderResult<()> {
        let src = self.refs[0];
        let dst = self.refs[CUR_BUF];
        let mx                          = br.read_gammap_s()? as isize;
        let my                          = br.read_gammap_s()? as isize;
        let ydelta                      = br.read_gammap_s()? * 2;
        let udelta                      = br.read_gammap_s()? * 2;
        let vdelta                      = br.read_gammap_s()? * 2;
        mc!(self.buf, src, dst, mx, my, xpos, ypos, w, h, ydelta, udelta, vdelta, self.width, self.height);

        Ok(())
    }
    fn pred_plane(&mut self, br: &mut BitReader, xpos: usize, ypos: usize, w: usize, h: usize) -> DecoderResult<()> {
        let ydelta                      = br.read_gammap_s()? * 2;
        let udelta                      = br.read_gammap_s()? * 2;
        let vdelta                      = br.read_gammap_s()? * 2;
        let yoff = xpos + ypos * 256;
        let coff = CHROMA_OFF + xpos / 2 + ypos / 2 * 256;
        pred_plane_delta(&mut self.buf[self.refs[CUR_BUF]], yoff,       w,     h,     ydelta);
        pred_plane_delta(&mut self.buf[self.refs[CUR_BUF]], coff,       w / 2, h / 2, udelta);
        pred_plane_delta(&mut self.buf[self.refs[CUR_BUF]], coff + 128, w / 2, h / 2, vdelta);
        Ok(())
    }
    fn intra_pred(&mut self, br: &mut BitReader, xpos: usize, ypos: usize, w: usize, h: usize) -> DecoderResult<()> {
        let ymode                       = br.read_gammap()? as usize;
        let cmode                       = br.read_gammap()? as usize;
        let yoff = xpos + ypos * 256;
        let coff = CHROMA_OFF + xpos / 2 + ypos / 2 * 256;
        let blk = &mut self.buf[self.refs[CUR_BUF]];
        match ymode {
            0 => pred_ver(blk, yoff, w, h),
            1 => pred_hor(blk, yoff, w, h),
            2 => pred_dc (blk, yoff, xpos, ypos, w, h),
            3 => pred_plane_delta(blk, yoff, w, h, 0),
            _ => return Err(DecoderError::InvalidData),
        };
        match cmode {
            0 => {
                pred_dc(blk, coff,       xpos / 2, ypos / 2, w / 2, h / 2);
                pred_dc(blk, coff + 128, xpos / 2, ypos / 2, w / 2, h / 2);
            },
            1 => {
                pred_hor(blk, coff,       w / 2, h / 2);
                pred_hor(blk, coff + 128, w / 2, h / 2);
            },
            2 => {
                pred_ver(blk, coff,       w / 2, h / 2);
                pred_ver(blk, coff + 128, w / 2, h / 2);
            },
            3 => {
                pred_plane_delta(blk, coff,       w / 2, h / 2, 0);
                pred_plane_delta(blk, coff + 128, w / 2, h / 2, 0);
            },
            _ => return Err(DecoderError::InvalidData),
        };
        Ok(())
    }
    fn intra_pred4x4(&mut self, br: &mut BitReader, xpos: usize, ypos: usize, w: usize, h: usize) -> DecoderResult<()> {
        let mut yoff = xpos + ypos * 256;
        let mut idx = 6;
        let blk = &mut self.buf[self.refs[CUR_BUF]];
        for y in (0..h).step_by(4) {
            for x in (0..w).step_by(4) {
                let mut mode = self.ipred4x4[idx - 5].min(self.ipred4x4[idx - 1]);
                if mode == 9 {
                    mode = 2;
                }
                if !br.read_bool()? {
                    let mode1           = br.read(3)? as u8;
                    mode = if mode1 >= mode { mode1 + 1 } else { mode1 };
                }
                match mode {
                    0 => pred_ver(blk, yoff + x, 4, 4),
                    1 => pred_hor(blk, yoff + x, 4, 4),
                    2 => pred_dc4x4(blk, yoff + x, xpos + x, ypos + y),
                    3 => pred_ddown_left (blk, yoff + x),
                    4 => pred_ddown_right(blk, yoff + x),
                    5 => pred_ver_right  (blk, yoff + x),
                    6 => pred_hor_down   (blk, yoff + x),
                    7 => pred_ver_left   (blk, yoff + x),
                    8 => pred_hor_up     (blk, yoff + x),
                    _ => unreachable!(),
                };
                self.ipred4x4[idx] = mode;
                idx += 1;
            }
            yoff += 256 * 4;
            idx -= w / 4;
            idx += 5;
        }
        let cmode                       = br.read_gammap()? as usize;
        let coff = CHROMA_OFF + xpos / 2 + ypos / 2 * 256;
        match cmode {
            0 => {
                pred_dc(blk, coff,       xpos / 2, ypos / 2, w / 2, h / 2);
                pred_dc(blk, coff + 128, xpos / 2, ypos / 2, w / 2, h / 2);
            },
            1 => {
                pred_hor(blk, coff,       w / 2, h / 2);
                pred_hor(blk, coff + 128, w / 2, h / 2);
            },
            2 => {
                pred_ver(blk, coff,       w / 2, h / 2);
                pred_ver(blk, coff + 128, w / 2, h / 2);
            },
            3 => {
                pred_plane_delta(blk, coff,       w / 2, h / 2, 0);
                pred_plane_delta(blk, coff + 128, w / 2, h / 2, 0);
            },
            _ => return Err(DecoderError::InvalidData),
        };
        Ok(())
    }
    fn decode_residue(&mut self, br: &mut BitReader, xpos: usize, ypos: usize, w: usize, h: usize) -> DecoderResult<()> {
        const CBP: [u8; 32] = [
            0x00, 0x08, 0x04, 0x02, 0x01, 0x1F, 0x0F, 0x0A,
            0x05, 0x0C, 0x03, 0x10, 0x0E, 0x0D, 0x0B, 0x07,
            0x09, 0x06, 0x1E, 0x1B, 0x1A, 0x1D, 0x17, 0x15,
            0x18, 0x12, 0x11, 0x1C, 0x14, 0x13, 0x16, 0x19
        ];

        let mut yoff = xpos + ypos * 256;
        let mut coff = CHROMA_OFF + xpos / 2 + ypos / 2 * 256;
        let blk = &mut self.buf[self.refs[CUR_BUF]];
        let mut yidx = (xpos / 4 + 1) + NCSTRIDE * (ypos / 4 + 1);
        let mut cidx = (xpos / 8 + 1) + NCSTRIDE * (ypos / 8 + 1);
        for _y in (0..h).step_by(8) {
            for x in (0..w).step_by(8) {
                let idx                 = br.read_gammap()? as usize;
                validate!(idx < CBP.len());
                let cbp = CBP[idx];
                for bno in 0..4 {
                    let cur_yidx = yidx + x / 4 + (bno & 1) + (bno / 2) * NCSTRIDE;
                    if (cbp & (1 << bno)) != 0 {
                        let ctx = avg(self.y_ncoeffs[cur_yidx - 1], self.y_ncoeffs[cur_yidx - NCSTRIDE]);
                        self.y_ncoeffs[cur_yidx] = decode_coeffs(br, &self.codebooks, &self.qmat, ctx, &mut blk[yoff + x + (bno & 1) * 4 + (bno / 2) * 4 * 256..])?;
                    } else {
                        self.y_ncoeffs[cur_yidx] = 0;
                    }
                }
                if (cbp & 0x10) != 0 {
                    let ctx = avg(self.c_ncoeffs[cidx + x / 8 - 1], self.c_ncoeffs[cidx + x / 8 - NCSTRIDE]);
                    let unc = decode_coeffs(br, &self.codebooks, &self.qmat, ctx, &mut blk[coff + x / 2..])?;
                    let vnc = decode_coeffs(br, &self.codebooks, &self.qmat, ctx, &mut blk[coff + 128 + x / 2..])?;
                    self.c_ncoeffs[cidx + x / 8] = avg(unc, vnc);
                } else {
                    self.c_ncoeffs[cidx + x / 8] = 0;
                }
            }
            yidx += NCSTRIDE * 2;
            cidx += NCSTRIDE;
            yoff += 8 * 256;
            coff += 4 * 256;
        }
        Ok(())
    }
    fn decode_block(&mut self, br: &mut BitReader, xpos: usize, ypos: usize, w: usize, h: usize) -> DecoderResult<()> {
        let mode                        = br.read_gammap()?;
        let min_dim = w.min(h);
        let large_block = min_dim >= 8;
        if mode >= 16 && !large_block {
            return Err(DecoderError::InvalidData);
        }
        match mode {
            0 if w > 2 => {
                let hw = w / 2;
                self.decode_block(br, xpos,      ypos, hw, h)?;
                self.decode_block(br, xpos + hw, ypos, hw, h)?;
            },
            1 => { self.copy_block(xpos, ypos, w, h, 0)?; },
            2 if h > 2 => {
                let hh = h / 2;
                self.decode_block(br, xpos, ypos,      w, hh)?;
                self.decode_block(br, xpos, ypos + hh, w, hh)?;
            },
            3 => { self.do_mc_bias(br, xpos, ypos, w, h)?; },
            4 | 5 | 6 => {
                let ref_id = (mode - 4) as usize;
                self.do_mc(br, xpos, ypos, w, h, ref_id)?;
            },
            7 => { self.pred_plane(br, xpos, ypos, w, h)?; },
            8 if large_block => {
                let hw = w / 2;
                self.decode_block(br, xpos,      ypos, hw, h)?;
                self.decode_block(br, xpos + hw, ypos, hw, h)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            9 => { self.copy_block(xpos, ypos, w, h, 1)?; },
            10 if large_block => {
                self.do_mc_bias(br, xpos, ypos, w, h)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            11 => {
                if min_dim >= 4 {
                    self.intra_pred(br, xpos, ypos, w, h)?;
                }
            },
            12 if large_block => {
                self.copy_block(xpos, ypos, w, h, 0)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            13 if large_block => {
                let hh = h / 2;
                self.decode_block(br, xpos, ypos,      w, hh)?;
                self.decode_block(br, xpos, ypos + hh, w, hh)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            14 => { self.copy_block(xpos, ypos, w, h, 2)?; },
            15 => {
                if min_dim >= 4 {
                    self.intra_pred4x4(br, xpos, ypos, w, h)?;
                }
            },
            16 | 17 | 18 => {
                let ref_id = (mode - 16) as usize;
                self.do_mc(br, xpos, ypos, w, h, ref_id)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            19 => {
                self.intra_pred4x4(br, xpos, ypos, w, h)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            20 => {
                self.copy_block(xpos, ypos, w, h, 1)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            21 => {
                self.copy_block(xpos, ypos, w, h, 2)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            22 => {
                self.intra_pred(br, xpos, ypos, w, h)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            23 => {
                self.pred_plane(br, xpos, ypos, w, h)?;
                self.decode_residue(br, xpos, ypos, w, h)?;
            },
            _ => return Err(DecoderError::InvalidData),
        };
        Ok(())
    }
}

const QUANTISERS: [u8; 52] = [
    10, 13, 10, 13, 13, 16, 13, 16, 11, 14, 11, 14, 14, 18, 14, 18,
    13, 16, 13, 16, 16, 20, 16, 20, 14, 18, 14, 18, 18, 23, 18, 23,
    16, 20, 16, 20, 20, 25, 20, 25, 18, 23, 18, 23, 23, 29, 23, 29,
    20, 14, 12, 10
];

impl NADecoder for VXVideoDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            validate!(self.width <= 256 && self.height <= 256);

            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, false, YUV420_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            if let Some(edata) = info.get_extradata() {
                validate!(edata.len() > 0);
                let fps = edata[0] as usize;
                validate!(fps <= 60);
                let base = &QUANTISERS[(fps % 6) * 8..][..8];
                let scale = fps / 6;
                for (dq, iq) in self.qmat.iter_mut().zip(base.iter()) {
                    *dq = i32::from(*iq) << scale;
                }
            } else {
                return Err(DecoderError::InvalidData);
            }

            for frm in self.buf.iter_mut() {
                let (ybuf, cbuf) = frm.split_at_mut(CHROMA_OFF);
                for el in ybuf.iter_mut() {
                    *el = 0;
                }
                for el in cbuf.iter_mut() {
                    *el = 0x80;
                }
            }

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 0);

        let mut br = BitReader::new(&src[0..], BitReaderMode::LE16MSB);

        self.y_ncoeffs = [0; NCSTRIDE * (256 / 4 + 1)];
        self.c_ncoeffs = [0; NCSTRIDE * (256 / 8 + 1)];
        self.mvs = [MV::default(); 16 * 16];
        self.decode_frame(&mut br)?;

        let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 4)?;
        let mut buf = bufinfo.get_vbuf().unwrap();
        let ystride = buf.get_stride(0);
        let ustride = buf.get_stride(1);
        let vstride = buf.get_stride(2);
        let yoff = buf.get_offset(0);
        let uoff = buf.get_offset(1);
        let voff = buf.get_offset(2);
        let data = buf.get_data_mut().unwrap();
        let cur = self.refs[CUR_BUF];
        for (sline, dline) in self.buf[cur][0..].chunks(256).take(self.height).zip(data[yoff..].chunks_mut(ystride)) {
            dline[..self.width].copy_from_slice(&sline[..self.width]);
        }
        for (sline, dline) in self.buf[cur][CHROMA_OFF..].chunks(256).take(self.height / 2).zip(data[uoff..].chunks_mut(ustride)) {
            dline[..self.width / 2].copy_from_slice(&sline[..self.width / 2]);
        }
        for (sline, dline) in self.buf[cur][CHROMA_OFF + 128..].chunks(256).take(self.height / 2).zip(data[voff..].chunks_mut(vstride)) {
            dline[..self.width / 2].copy_from_slice(&sline[..self.width / 2]);
        }
        let videobuf = NABufferType::Video(buf);
        self.update_refs();

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), videobuf);
        let is_intra = pkt.get_pts() == Some(0);
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for VXVideoDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder_video() -> Box<dyn NADecoder + Send> {
    Box::new(VXVideoDecoder::new())
}


struct AudioState {
    lpc0_idx:   usize,
    lpc1_idx:   usize,
    lpc2_idx:   usize,
    scale:      i32,
    frame_mode: usize,
    cur_filt:   [i32; 8],

    lpc0_cb:    [[i16; 8]; 64],
    lpc1_cb:    [[i16; 8]; 64],
    lpc2_cb:    [[i16; 8]; 64],
    decays:     [i32; 8],
    base_filt:  [i32; 8],
    base_scale: i32,
}

impl AudioState {
    fn new() -> Self {
        Self {
            lpc0_idx:   0,
            lpc1_idx:   0,
            lpc2_idx:   0,
            scale:      0,
            frame_mode: 0,
            cur_filt:   [0; 8],

            lpc0_cb:    [[0; 8]; 64],
            lpc1_cb:    [[0; 8]; 64],
            lpc2_cb:    [[0; 8]; 64],
            decays:     [0; 8],
            base_filt:  [0; 8],
            base_scale: 0,
        }
    }
    fn read_initial_params(&mut self, br: &mut ByteReader) -> DecoderResult<()> {
        for entry in self.lpc0_cb.iter_mut() {
            for el in entry.iter_mut() {
                *el                     = br.read_u16le()? as i16;
            }
        }
        for entry in self.lpc1_cb.iter_mut() {
            for el in entry.iter_mut() {
                *el                     = br.read_u16le()? as i16;
            }
        }
        for entry in self.lpc2_cb.iter_mut() {
            for el in entry.iter_mut() {
                *el                     = br.read_u16le()? as i16;
            }
        }
        for el in self.decays.iter_mut() {
            *el                         = i32::from(br.read_u16le()? as i16);
        }
        for el in self.base_filt.iter_mut() {
            *el                         = br.read_u32le()? as i32;
        }
        self.base_scale                 = br.read_u32le()? as i32;
        Ok(())
    }
    fn unpack_data(&mut self, br: &mut ByteReader, val: u16, dst: &mut [i32]) -> DecoderResult<()> {
        self.lpc0_idx = (val & 0x3F) as usize;
        self.scale = (self.decays[((val >> 6) & 7) as usize] * self.scale) >> 13;
        let val1                        = br.read_u16le()?;
        self.lpc1_idx = ((val1 >> 6) & 0x3F) as usize;
        self.lpc2_idx = (val1 & 0x3F) as usize;
        self.frame_mode = ((val1 >> 12) & 3) as usize;
        let mut idx = (val1 >> 14) as usize;
        if self.frame_mode == 0 {
            let mut tail = 0;
            for _ in 0..8 {
                let val                 = br.read_u16le()?;
                for i in 0..5 {
                    let add = i32::from((val >> (13 - i * 3)) & 7);
                    dst[idx] += self.scale * (add * 2 - 7);
                    idx += 3;
                }
                tail = tail * 2 + (val & 1);
            }
            let add = i32::from((tail >> 5) & 7);
            dst[idx] += self.scale * (add * 2 - 7);
            idx += 3;
            let add = i32::from((tail >> 2) & 7);
            dst[idx] += self.scale * (add * 2 - 7);
        } else {
            let (len, step) = match self.frame_mode {
                    1 => (5, 3),
                    2 => (4, 4),
                    3 => (3, 5),
                    _ => unreachable!(),
                };
            idx += 128;
            for _ in 0..len {
                let val                 = br.read_u16le()?;
                for i in 0..8 {
                    let add = i32::from((val >> (14 - i * 2)) & 3);
                    dst[idx] += self.scale * (add * 2 - 3);
                    idx += step;
                }
            }
        }
        Ok(())
    }
    fn update_intra(&mut self) {
        self.cur_filt = self.base_filt;
        for i in 0..8 {
            self.cur_filt[i] += i32::from(self.lpc0_cb[self.lpc0_idx][i]);
            self.cur_filt[i] += i32::from(self.lpc1_cb[self.lpc1_idx][i]);
            self.cur_filt[i] += i32::from(self.lpc2_cb[self.lpc2_idx][i]);
        }
    }
    fn update_inter(&mut self) {
        for i in 0..8 {
            self.cur_filt[i] += i32::from(self.lpc0_cb[self.lpc0_idx][i]);
            self.cur_filt[i] += i32::from(self.lpc1_cb[self.lpc1_idx][i]);
            self.cur_filt[i] += i32::from(self.lpc2_cb[self.lpc2_idx][i]);
        }
    }
}

fn apply_lpc(dst: &mut [i32], src: &[i32], hist: &mut [i32], filt: &[i32; 8]) {
    for (hidx, (out, src)) in dst.iter_mut().zip(src.iter()).enumerate() {
        let mut sum = *src << 14;
        for i in 0..8 {
            sum += hist[(hidx + i) & 7] * filt[i];
        }
        let samp = sum >> 14;
        *out = samp;
        hist[hidx & 7] = samp;
    }
}

struct VXAudioDecoder {
    ainfo:      NAAudioInfo,
    info:       Arc<NACodecInfo>,
    chmap:      NAChannelMap,
    buf:        [i32; 256 * 2],
    flip_buf:   bool,
    state:      AudioState,
    lpc_hist:   [i32; 8],
    lpc_filt:   [i32; 8],
    lpc_filt1:  [i32; 8],
}

impl VXAudioDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            info:       NACodecInfo::new_dummy(),
            chmap:      NAChannelMap::new(),
            buf:        [0; 256 * 2],
            flip_buf:   true,
            state:      AudioState::new(),
            lpc_hist:   [0; 8],
            lpc_filt:   [0; 8],
            lpc_filt1:  [0; 8],
        }
    }
    fn decode_inter(&mut self, br: &mut ByteReader, mode: u16, val: u16) -> DecoderResult<()> {
        let (mut cur_buf, mut prev_buf) = self.buf.split_at_mut(256);
        if self.flip_buf {
            std::mem::swap(&mut cur_buf, &mut prev_buf);
        }
        cur_buf[0..128].copy_from_slice(&prev_buf[128..]);
        if mode == 0x7E {
            for el in cur_buf[128..].iter_mut() {
                *el = 0;
            }
        } else {
            let src = &prev_buf[127 - (mode as usize)..];
            let dst = &mut cur_buf[128..];
            for i in 0..7 {
                dst[i] = (src[i] * ((i + 1) as i32)) >> 4;
            }
            for i in 7..121 {
                dst[i] = src[i] >> 1;
            }
            for i in 121..128 {
                dst[i] = (src[i] * ((128 - i) as i32)) >> 4;
            }
        }

        self.state.unpack_data(br, val, prev_buf )?;
        self.state.update_inter();

        let (cfilt, pfilt) = if !self.flip_buf {
                (&mut self.lpc_filt, &mut self.lpc_filt1)
            } else {
                (&mut self.lpc_filt1, &mut self.lpc_filt)
            };
        *cfilt = self.state.cur_filt;
        let mut f0 = [0; 8];
        let mut f1 = [0; 8];
        let mut f2 = [0; 8];
        for i in 0..8 {
            f1[i] = (pfilt[i] + cfilt[i]) >> 1;
            f0[i] = (pfilt[i] + f1   [i]) >> 1;
            f2[i] = (f1   [i] + cfilt[i]) >> 1;
        }
        apply_lpc(&mut cur_buf[ 0..][..32], &prev_buf[128..],      &mut self.lpc_hist, &f0);
        apply_lpc(&mut cur_buf[32..][..32], &prev_buf[128 + 32..], &mut self.lpc_hist, &f1);
        apply_lpc(&mut cur_buf[64..][..32], &prev_buf[128 + 64..], &mut self.lpc_hist, &f2);
        apply_lpc(&mut cur_buf[96..][..32], &prev_buf[128 + 96..], &mut self.lpc_hist, cfilt);
        Ok(())
    }
    fn decode_intra(&mut self, br: &mut ByteReader, val: u16) -> DecoderResult<()> {
        self.state.scale = self.state.base_scale;
        self.lpc_hist = [0; 8];
        self.flip_buf = true;

        let (mut cur_buf, mut prev_buf) = self.buf.split_at_mut(256);
        if self.flip_buf {
            std::mem::swap(&mut cur_buf, &mut prev_buf);
        }
        for el in cur_buf[128..].iter_mut() {
            *el = 0;
        }
        self.state.unpack_data(br, val, prev_buf)?;
        self.state.update_intra();

        self.lpc_filt = self.state.cur_filt;
        apply_lpc(&mut cur_buf[..128], &prev_buf[128..], &mut self.lpc_hist, &self.lpc_filt);
        Ok(())
    }
    fn output(&mut self, dst: &mut [i16]) {
        let src = if self.flip_buf { &self.buf[256..][..128] } else { &self.buf[..128] };
        for (src, dst) in src.iter().zip(dst.iter_mut()) {
            *dst = (*src).max(-0x8000).min(0x7FFF) as i16;
        }
        self.flip_buf = !self.flip_buf;
    }
}

impl NADecoder for VXAudioDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(ainfo) = info.get_properties() {
            if let Some(edata) = info.get_extradata() {
                validate!(edata.len() >= 3124);
                let mut mr = MemoryReader::new_read(edata.as_slice());
                let mut br = ByteReader::new(&mut mr);
                self.state.read_initial_params(&mut br)?;
            } else {
                return Err(DecoderError::InvalidData);
            }
            self.ainfo = NAAudioInfo::new(ainfo.get_sample_rate(), 1, SND_S16_FORMAT, 1);
            self.info = info.replace_info(NACodecTypeInfo::Audio(self.ainfo));
            self.chmap = NAChannelMap::from_str("C").unwrap();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        const SUBFRAME_LEN: [usize; 4] = [20, 14, 12, 10];

        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            validate!(pktbuf.len() >= 10);

            let mut mr = MemoryReader::new_read(&pktbuf);
            let mut br = ByteReader::new(&mut mr);
            let mut nblocks = 0;
            while br.left() > 4 {
                                          br.read_skip(2)?;
                let val                 = br.read_u16le()?;
                nblocks += 1;
                let sf_len = SUBFRAME_LEN[((val >> 12) & 3) as usize];
                if br.left() <= sf_len as i64 {
                    break;
                }
                                          br.read_skip(sf_len - 4)?;
            }

            let samples = 128 * nblocks;
            let abuf = alloc_audio_buffer(self.ainfo, samples, self.chmap.clone())?;
            let mut adata = abuf.get_abuf_i16().unwrap();
            let dst = adata.get_data_mut().unwrap();

            let mut mr = MemoryReader::new_read(&pktbuf);
            let mut br = ByteReader::new(&mut mr);

            let mut blk_no = 0usize;
            while br.left() > 0 {
                let val                 = br.read_u16le()?;
                let mode = val >> 9;
                if mode == 0x7F {
                    self.decode_intra(&mut br, val)?;
                } else {
                    self.decode_inter(&mut br, val & 0x1FF, mode)?;
                }
                self.output(&mut dst[blk_no * 128..]);
                blk_no += 1;
            }

            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), abuf);
            frm.set_duration(Some(samples as u64));
            frm.set_keyframe(true);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}


impl NAOptionHandler for VXAudioDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder_audio() -> Box<dyn NADecoder + Send> {
    Box::new(VXAudioDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    #[test]
    fn test_vx_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from some game
        test_decoding("vx", "vxvideo", "assets/Game/bioware.vx", Some(31), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                                [0x33de14fa, 0x00948eb7, 0x028141d7, 0x1d07abd6],
                                [0x77a96135, 0x0cc2d0b5, 0x45862c7c, 0xe27f5b10],
                                [0x3c3ed089, 0x3c643216, 0xe6aed381, 0x4d43c50f],
                                [0x09e86330, 0x37d3a766, 0xa8198ac5, 0x21fa089c],
                                [0xbcab34d7, 0xdffe234a, 0x6534709f, 0xc3050e32],
                                [0x2bab595d, 0x7a19937e, 0xccc97277, 0x91b32191],
                                [0xe082f77e, 0x6498fd0d, 0xa3828c0f, 0xb4f7a02a],
                                [0x8515f6e2, 0xc0cb8ab3, 0xf91a3483, 0xd54470fc],
                                [0x17b1b54a, 0x1b574323, 0xcdeec1ec, 0x7a00ae4e],
                                [0x773f1d47, 0x86188681, 0xb111bcb8, 0x80fe34e9],
                                [0xbc4a8e20, 0xca91d4b6, 0x7275a162, 0x9a73be7c],
                                [0x0d4992b5, 0xd12a985a, 0x929ebec2, 0x0653fbc7],
                                [0xc24691b7, 0xabd27a7a, 0xd62cbd73, 0xd72a49ea],
                                [0x9024f7f4, 0xbe707e73, 0x27b4b4a0, 0x33bb515e],
                                [0x31aee8fc, 0xd0a3fa6d, 0xea11ef6a, 0x53a5f031],
                                [0x4a83f326, 0xceff4329, 0x54fbe91e, 0xf98ee74b],
                                [0xe54b5450, 0x979f4b26, 0x910ee666, 0x05fd1996],
                                [0xe48d66d0, 0x0a69b963, 0x9917084e, 0xf4b0486d],
                                [0x624a8b32, 0x0d1ce036, 0x9de8ebf0, 0x472a77b9],
                                [0x5d307d48, 0x1168f3a9, 0xaa792fb2, 0x34430b20],
                                [0xf2d80474, 0xac6b0972, 0x500e569e, 0x3c8e7dde],
                                [0x481310b3, 0x70cdeb91, 0xed007972, 0x70cefff3],
                                [0x8b5b17ca, 0xca6f9a72, 0x0256908a, 0x4505cf85],
                                [0xb6222c1d, 0x7a9760cb, 0xb3276304, 0x2ff1595e],
                                [0xf98e3d89, 0xae957c83, 0xff849c05, 0x8ca54276],
                                [0xbcebda1c, 0x6f6e4ac6, 0x023e7f0f, 0x9578142f],
                                [0x399f5155, 0xd95b33e3, 0xf0b55af8, 0xe32db6b2],
                                [0x0c4d4347, 0x5f5061e4, 0xe2fa4690, 0xa340d294],
                                [0x6fcdddb5, 0xf101da80, 0x6f55ddd9, 0x0dfeead1],
                                [0xb9623043, 0x1dab8a93, 0x22fd5f7a, 0x2c2a6633],
                                [0xb3ac2652, 0xf474e49d, 0x7db51405, 0xcd1c13cc],
                                [0x6a901339, 0xda88b2be, 0x6d943e18, 0xda9b5926]]));
    }
}

const NC_MAP: [usize; 8] = [ 0, 0, 1, 1, 2, 2, 2, 2 ];
const NC_BITS: [[u16; 68]; 3] = [
  [
    0x0001, 0x0000, 0x0000, 0x0000, 0x0005, 0x0001, 0x0000, 0x0000,
    0x0007, 0x0004, 0x0001, 0x0000, 0x0007, 0x0006, 0x0005, 0x0003,
    0x0007, 0x0006, 0x0005, 0x0003, 0x0007, 0x0006, 0x0005, 0x0004,
    0x000F, 0x0006, 0x0005, 0x0004, 0x000B, 0x000E, 0x0005, 0x0004,
    0x0008, 0x000A, 0x000D, 0x0004, 0x000F, 0x000E, 0x0009, 0x0004,
    0x000B, 0x000A, 0x000D, 0x000C, 0x000F, 0x000E, 0x0009, 0x000C,
    0x000B, 0x000A, 0x000D, 0x0008, 0x000F, 0x0001, 0x0009, 0x000C,
    0x000B, 0x000E, 0x000D, 0x0008, 0x0007, 0x000A, 0x0009, 0x000C,
    0x0004, 0x0006, 0x0005, 0x0008
  ], [
    0x0003, 0x0000, 0x0000, 0x0000, 0x000B, 0x0002, 0x0000, 0x0000,
    0x0007, 0x0007, 0x0003, 0x0000, 0x0007, 0x000A, 0x0009, 0x0005,
    0x0007, 0x0006, 0x0005, 0x0004, 0x0004, 0x0006, 0x0005, 0x0006,
    0x0007, 0x0006, 0x0005, 0x0008, 0x000F, 0x0006, 0x0005, 0x0004,
    0x000B, 0x000E, 0x000D, 0x0004, 0x000F, 0x000A, 0x0009, 0x0004,
    0x000B, 0x000E, 0x000D, 0x000C, 0x0008, 0x000A, 0x0009, 0x0008,
    0x000F, 0x000E, 0x000D, 0x000C, 0x000B, 0x000A, 0x0009, 0x000C,
    0x0007, 0x000B, 0x0006, 0x0008, 0x0009, 0x0008, 0x000A, 0x0001,
    0x0007, 0x0006, 0x0005, 0x0004
  ], [
    0x000F, 0x0000, 0x0000, 0x0000, 0x000F, 0x000E, 0x0000, 0x0000,
    0x000B, 0x000F, 0x000D, 0x0000, 0x0008, 0x000C, 0x000E, 0x000C,
    0x000F, 0x000A, 0x000B, 0x000B, 0x000B, 0x0008, 0x0009, 0x000A,
    0x0009, 0x000E, 0x000D, 0x0009, 0x0008, 0x000A, 0x0009, 0x0008,
    0x000F, 0x000E, 0x000D, 0x000D, 0x000B, 0x000E, 0x000A, 0x000C,
    0x000F, 0x000A, 0x000D, 0x000C, 0x000B, 0x000E, 0x0009, 0x000C,
    0x0008, 0x000A, 0x000D, 0x0008, 0x000D, 0x0007, 0x0009, 0x000C,
    0x0009, 0x000C, 0x000B, 0x000A, 0x0005, 0x0008, 0x0007, 0x0006,
    0x0001, 0x0004, 0x0003, 0x0002
  ]
];
const NC_LENS: [[u8; 68]; 3] = [
  [
     1,  0,  0,  0,  6,  2,  0,  0,
     8,  6,  3,  0,  9,  8,  7,  5,
    10,  9,  8,  6, 11, 10,  9,  7,
    13, 11, 10,  8, 13, 13, 11,  9,
    13, 13, 13, 10, 14, 14, 13, 11,
    14, 14, 14, 13, 15, 15, 14, 14,
    15, 15, 15, 14, 16, 15, 15, 15,
    16, 16, 16, 15, 16, 16, 16, 16,
    16, 16, 16, 16
  ], [
     2,  0,  0,  0,  6,  2,  0,  0,
     6,  5,  3,  0,  7,  6,  6,  4,
     8,  6,  6,  4,  8,  7,  7,  5,
     9,  8,  8,  6, 11,  9,  9,  6,
    11, 11, 11,  7, 12, 11, 11,  9,
    12, 12, 12, 11, 12, 12, 12, 11,
    13, 13, 13, 12, 13, 13, 13, 13,
    13, 14, 13, 13, 14, 14, 14, 13,
    14, 14, 14, 14
  ], [
     4,  0,  0,  0,  6,  4,  0,  0,
     6,  5,  4,  0,  6,  5,  5,  4,
     7,  5,  5,  4,  7,  5,  5,  4,
     7,  6,  6,  4,  7,  6,  6,  4,
     8,  7,  7,  5,  8,  8,  7,  6,
     9,  8,  8,  7,  9,  9,  8,  8,
     9,  9,  9,  8, 10,  9,  9,  9,
    10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10
  ]
];

const NUM_ZERO_BITS: [[u8; 16]; 15] = [
  [
    0x01, 0x03, 0x02, 0x03, 0x02, 0x03, 0x02, 0x03, 0x02, 0x03, 0x02, 0x03, 0x02, 0x03, 0x02, 0x01
  ], [
    0x07, 0x06, 0x05, 0x04, 0x03, 0x05, 0x04, 0x03, 0x02, 0x03, 0x02, 0x03, 0x02, 0x01, 0x00, 0x00
  ], [
    0x05, 0x07, 0x06, 0x05, 0x04, 0x03, 0x04, 0x03, 0x02, 0x03, 0x02, 0x01, 0x01, 0x00, 0x00, 0x00
  ], [
    0x03, 0x07, 0x05, 0x04, 0x06, 0x05, 0x04, 0x03, 0x03, 0x02, 0x02, 0x01, 0x00, 0x00, 0x00, 0x00
  ], [
    0x05, 0x04, 0x03, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x01, 0x01, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x01, 0x01, 0x05, 0x04, 0x03, 0x03, 0x02, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x01, 0x01, 0x01, 0x03, 0x03, 0x02, 0x02, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x01, 0x00, 0x01, 0x03, 0x02, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x01, 0x00, 0x01, 0x03, 0x02, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x00, 0x01, 0x01, 0x02, 0x01, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x00, 0x01, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x00, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x00, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ], [
    0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ]
];
const NUM_ZERO_LENS: [[u8; 16]; 15] = [
    [ 1,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7,  8,  8,  9,  9,  9 ],
    [ 3,  3,  3,  3,  3,  4,  4,  4,  4,  5,  5,  6,  6,  6,  6,  0 ],
    [ 4,  3,  3,  3,  4,  4,  3,  3,  4,  5,  5,  6,  5,  6,  0,  0 ],
    [ 5,  3,  4,  4,  3,  3,  3,  4,  3,  4,  5,  5,  5,  0,  0,  0 ],
    [ 4,  4,  4,  3,  3,  3,  3,  3,  4,  5,  4,  5,  0,  0,  0,  0 ],
    [ 6,  5,  3,  3,  3,  3,  3,  3,  4,  3,  6,  0,  0,  0,  0,  0 ],
    [ 6,  5,  3,  3,  3,  2,  3,  4,  3,  6,  0,  0,  0,  0,  0,  0 ],
    [ 6,  4,  5,  3,  2,  2,  3,  3,  6,  0,  0,  0,  0,  0,  0,  0 ],
    [ 6,  6,  4,  2,  2,  3,  2,  5,  0,  0,  0,  0,  0,  0,  0,  0 ],
    [ 5,  5,  3,  2,  2,  2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0 ],
    [ 4,  4,  3,  3,  1,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ],
    [ 4,  4,  2,  1,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ],
    [ 3,  3,  1,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ],
    [ 2,  2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ],
    [ 1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ]
];

const ZERO_RUN_BITS: [[u8; 8]; 6] = [
    [ 1, 0, 0, 0, 0, 0, 0, 0 ],
    [ 1, 1, 0, 0, 0, 0, 0, 0 ],
    [ 3, 2, 1, 0, 0, 0, 0, 0 ],
    [ 3, 2, 1, 1, 0, 0, 0, 0 ],
    [ 3, 2, 3, 2, 1, 0, 0, 0 ],
    [ 3, 0, 1, 3, 2, 5, 4, 0 ]
];
const ZERO_RUN_LENS: [[u8; 8]; 6] = [
    [ 1, 1, 0, 0, 0, 0, 0, 0 ],
    [ 1, 2, 2, 0, 0, 0, 0, 0 ],
    [ 2, 2, 2, 2, 0, 0, 0, 0 ],
    [ 2, 2, 2, 3, 3, 0, 0, 0 ],
    [ 2, 2, 3, 3, 3, 3, 0, 0 ],
    [ 2, 3, 3, 3, 3, 3, 3, 0 ]
];

