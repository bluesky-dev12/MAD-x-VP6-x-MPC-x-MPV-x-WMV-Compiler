use super::CB_SELECTORS;
use super::mv::MV;
use super::super::indeo3data::*;
use super::{Indeo3Cell, Plane};

pub const MAX_CELL_SIZE: usize = 2400;
const DEFAULT_PIXEL: u8 = 0x40;
const INTRA_FLAT_THRESHOLD: u32 = 8;
const INTER_FLAT_THRESHOLD: u32 = 8;

struct IndexWriter<'a> {
    dst:    &'a mut [u8],
    pos:    usize,
    do_rle: bool,
}

const SKIP_CELL: u8 = 0xFD;
const ZERO_CELL: u8 = 0xFD;

impl<'a> IndexWriter<'a> {
    fn new(dst: &'a mut [u8], do_rle: bool) -> Self {
        Self {
            dst,
            pos:    0,
            do_rle,
        }
    }
    fn write_pair(&mut self, idx0: u8, idx1: u8, quad_radix: u8, esc: u8) {
        //xxx: in theory one in theory the other index just has to make output fit byte range
        if idx0 < quad_radix && idx1 < quad_radix {
            let quad = idx1 * quad_radix + idx0 + esc;
            self.dst[self.pos] = quad;
            self.pos += 1;
        } else {
            self.dst[self.pos] = idx0;
            self.pos += 1;
            self.dst[self.pos] = idx1;
            self.pos += 1;
        }
    }
    fn write_byte(&mut self, val: u8) {
        self.dst[self.pos] = val;
        self.pos += 1;
    }
    fn compact_cell(&mut self, esc_vals: [u8; 4]) {
        if !self.do_rle {
            return;
        }
        let tail = &self.dst[self.pos - 4..][..4];
        let mut count = 0;
        for (&a, &b) in tail.iter().zip(esc_vals.iter()).rev() {
            if a != b {
                break;
            }
            count += 1;
        }
        if count > 1 {
            self.pos -= count;
            self.dst[self.pos] = ZERO_CELL;
            self.pos += 1;
        }
    }
    fn compact_all_cells(&mut self) {
        if !self.do_rle {
            return;
        }
        if self.pos > 2 {
            let mut i = 0;
            while i + 2 < self.pos {
                if self.dst[i] == ZERO_CELL && self.dst[i + 1] == ZERO_CELL {
                    let mut last_idx = i;
                    for j in (i + 1)..self.pos {
                        if self.dst[j] != ZERO_CELL {
                            break;
                        }
                        last_idx = j;
                    }
                    let len = (last_idx - i + 1).min(31);
                    if len == 2 {
                        self.dst[i] = 0xFC;
                        move_tail(&mut self.dst[i + 1..self.pos], 1);
                        self.pos -= 1;
                    } else {
                        self.dst[i] = 0xFB;
                        self.dst[i + 1] = len as u8;
                        move_tail(&mut self.dst[i + 2..self.pos], len - 2);
                        self.pos -= len - 2;
                    }
                }
                i += 1;
            }
        }
    }
    fn end(self) -> usize {
        self.pos
    }
}

fn move_tail(buf: &mut [u8], off: usize) {
    let len = buf.len();
    for i in off..len {
        buf[i - off] = buf[i];
    }
}

#[derive(Default)]
struct CodebookSuggester {
    count:  [u16; 16],
}

const BINNING_FACTORS: [u8; 16] = [3, 7, 9, 12, 14, 16, 18, 40, 2, 3, 4, 5, 6, 7, 8, 9];

impl CodebookSuggester {
    fn new() -> Self { Self::default() }
    fn merge(cs1: &Self, cs2: &Self) -> Self {
        let mut count = [0; 16];
        for (dst, (&src1, &src2)) in count.iter_mut().zip(cs1.count.iter().zip(cs2.count.iter())) {
            *dst = src1 + src2;
        }
        Self { count }
    }
    fn add_delta(&mut self, delta: u8) {
        for (i, &fac) in BINNING_FACTORS.iter().enumerate() {
            let val = if i < 8 { delta + fac - 1 } else { delta };
            if (val % fac) == 0 {
                self.count[i] += 1;
            }
        }
    }
    fn add_line(&mut self, src: &[i8]) {
        for &delta in src.iter() {
            if delta == 0 {
                continue;
            }
            let delta = delta.unsigned_abs();
            self.add_delta(delta);
        }
    }
    fn add_line_half(&mut self, src: &[i8]) {
        for &delta in src.iter().step_by(2) {
            if delta == 0 {
                continue;
            }
            let delta = delta.unsigned_abs();
            self.add_delta(delta);
        }
    }
    fn get_best(&self) -> u8 {
        let mut idx = 0;
        for (i, &cnt) in self.count.iter().enumerate().skip(1) {
            if cnt > self.count[idx] {
                idx = i;
            }
        }
        idx as u8
    }
}

pub struct CellEncoder {
        buf:    [u8; MAX_CELL_SIZE + 160],
        rbuf:   [u8; MAX_CELL_SIZE + 160],
        deltas: [i8; MAX_CELL_SIZE],
        cell:   Indeo3Cell,
    pub out:    [u8; MAX_CELL_SIZE / 2 + 1],
    pub osize:  usize,

    pub flat_thr_i: u32,
    pub flat_thr_p: u32,
    pub do_rle:     bool,
    pub quant:      Option<u8>,
}

impl CellEncoder {
    pub fn new() -> Self {
        Self {
            buf:    [0; MAX_CELL_SIZE + 160],
            rbuf:   [0; MAX_CELL_SIZE + 160],
            deltas: [0; MAX_CELL_SIZE],
            cell:   Indeo3Cell::new(0, 0),
            out:    [0; MAX_CELL_SIZE / 2 + 1],
            osize:  0,

            flat_thr_i: INTRA_FLAT_THRESHOLD,
            flat_thr_p: INTER_FLAT_THRESHOLD,
            do_rle:     true,
            quant:      None,
        }
    }
    pub fn read_buffer(&mut self, plane: &Plane, cell: Indeo3Cell) {
        self.cell = cell;

        let src = &plane.data[cell.get_x() + cell.get_y() * plane.width..];
        let dst_w = cell.get_width();
        for (dline, sline) in self.buf.chunks_mut(dst_w).skip(1).zip(src.chunks(plane.width)).take(cell.get_height()) {
            dline.copy_from_slice(&sline[..dst_w]);
        }
        if cell.get_y() > 0 {
            self.buf[..dst_w].copy_from_slice(&plane.data[cell.get_x() + (cell.get_y() - 1) * plane.width..][..dst_w]);
        } else {
            for el in self.buf[..dst_w].iter_mut() {
                *el = DEFAULT_PIXEL;
            }
        }
    }
    pub fn read_mv_buffer(&mut self, plane: &Plane, cell: Indeo3Cell, mv: MV) {
        self.cell = cell;

        let xoff = (cell.get_x() as isize + isize::from(mv.x)) as usize;
        let yoff = (cell.get_y() as isize + isize::from(mv.y)) as usize;
        let src = &plane.data[xoff + yoff * plane.width..];
        let dst_w = cell.get_width();
        for (dline, sline) in self.rbuf.chunks_mut(dst_w).skip(1).zip(src.chunks(plane.width)).take(cell.get_height()) {
            dline.copy_from_slice(&sline[..dst_w]);
        }
    }
    pub fn null_mv(&mut self) {
        let stride = self.cell.get_width();
        self.buf[stride..].copy_from_slice(&self.rbuf[stride..]);
    }
    pub fn gen_diffs_intra(&mut self) {
        let stride = self.cell.get_width();
        let mut start = stride;
        for dline in self.deltas.chunks_mut(stride).take(self.cell.get_height()) {
            let (pprev, cur) = self.buf.split_at(start);
            let prev = &pprev[pprev.len() - stride..];

            for (dst, (&cur, &top)) in dline.iter_mut().zip(cur.iter().zip(prev.iter())) {
                *dst = (cur as i8) - (top as i8);
            }

            start += stride;
        }
    }
    pub fn gen_diffs_inter(&mut self) {
        let stride = self.cell.get_width();
        let prev_iter = self.rbuf.chunks(stride).skip(1);
        let cur_iter = self.buf.chunks(stride).skip(1);
        for (dline, (cur, prev)) in self.deltas.chunks_mut(stride).take(self.cell.get_height()).zip(cur_iter.zip(prev_iter)) {
            for (dst, (&cur, &prev)) in dline.iter_mut().zip(cur.iter().zip(prev.iter())) {
                *dst = (cur as i8) - (prev as i8);
            }
        }
    }
    pub fn put_buffer(&self, plane: &mut Plane) {
        let to_skip = if !self.cell.is_intra() || self.cell.get_y() == 0 { 1 } else { 0 };

        let dst = &mut plane.data[self.cell.get_x() + (self.cell.get_y() + to_skip - 1) * plane.width..];
        let src_w = self.cell.get_width();
        for (sline, dline) in self.buf.chunks(src_w).skip(to_skip).zip(dst.chunks_mut(plane.width)).take(self.cell.get_height() + 1 - to_skip) {
            dline[..src_w].copy_from_slice(sline);
        }
    }
    fn determine_mode(&self, intra: bool, mut mode_hint: u8) -> (u8, [u8; 2]) {
        if let Some(qmode) = self.quant {
            if intra {
                return (mode_hint, [qmode, qmode]);
            } else {
                let qmode = qmode & 7;
                return (mode_hint, [qmode, qmode]);
            }
        }

        let stride = self.cell.get_width();

        let mut cb_p = CodebookSuggester::new();
        let mut cb_s = CodebookSuggester::new();
        if !intra && (self.cell.get_height() & 7 == 0) {
            let mut vdiff = 0;
            let mut hdiff = 0;
            for line_pair in self.deltas.chunks(stride * 2).take(self.cell.get_height() / 2) {
                let (line1, line2) = line_pair.split_at(stride);
                for (&el1, &el2) in line1.iter().zip(line2.iter()) {
                    let diff = i32::from(el1) - i32::from(el2);
                    vdiff += (diff * diff) as u32;
                }
            }
            for line in self.deltas.chunks(stride).take(self.cell.get_height()) {
                for pair in line.chunks(2) {
                    let diff = i32::from(pair[1]) - i32::from(pair[0]);
                    hdiff += (diff * diff) as u32;
                }
            }
            vdiff /= (self.cell.get_width() * self.cell.get_height() / 2) as u32;
            hdiff /= (self.cell.get_width() * self.cell.get_height() / 2) as u32;

            mode_hint = match ((vdiff > self.flat_thr_p), (hdiff > self.flat_thr_p)) {
                    (false, false) if (self.cell.get_width() & 7) == 0 => 10,
                    (false, _)     => 11,
                    _              => 0,
                };
        }
        match mode_hint {
            0 => {
                for line_pair in self.deltas.chunks(stride * 2).take(self.cell.get_height() / 2) {
                    let (line1, line2) = line_pair.split_at(stride);
                    cb_p.add_line(line1);
                    cb_s.add_line(line2);
                }
            },
            3 => {
                for line_quad in self.deltas.chunks(stride * 4).take(self.cell.get_height() / 4) {
                    let (line01, line23) = line_quad.split_at(stride * 2);
                    let (_line0, line1) = line01.split_at(stride);
                    let (_line2, line3) = line23.split_at(stride);
                    cb_p.add_line(line1);
                    cb_s.add_line(line3);
                }
            },
            10 => {
                for line_quad in self.deltas.chunks(stride * 4).take(self.cell.get_height() / 4) {
                    let (line01, line23) = line_quad.split_at(stride * 2);
                    let (_line0, line1) = line01.split_at(stride);
                    let (_line2, line3) = line23.split_at(stride);
                    cb_p.add_line_half(line1);
                    cb_s.add_line_half(line3);
                }
            },
            11 => {
                for line_quad in self.deltas.chunks(stride * 4).take(self.cell.get_height() / 4) {
                    let (line01, line23) = line_quad.split_at(stride * 2);
                    let (_line0, line1) = line01.split_at(stride);
                    let (_line2, line3) = line23.split_at(stride);
                    cb_p.add_line(line1);
                    cb_s.add_line(line3);
                }
            },
            _ => unreachable!(),
        };
        let cb_f = CodebookSuggester::merge(&cb_p, &cb_s).get_best();
        let cb_p = cb_p.get_best();
        let mut cb_s = cb_s.get_best();

        let mut use_single = !intra || mode_hint == 10 || cb_p == cb_s;
        if !use_single {
            if cb_s == 0 { // we can adjust to the CB_SELECTORS here
                cb_s = (((cb_p & 7) + 1) * 2).min(15);
            }
            let ncb = (cb_p << 4) + cb_s;
            use_single = !CB_SELECTORS.contains(&ncb);
        }

        if use_single {
            if intra || cb_f < 8 { // we don't want requant happening in inter mode
                (mode_hint, [cb_f, cb_f])
            } else {
                (mode_hint, [0, 0])
            }
        } else {
            (mode_hint + 1, [cb_p, cb_s])
        }
    }
    pub fn compress_intra(&mut self, mode_hint: u8) {
        let (mode, vq_idx) = self.determine_mode(true, mode_hint);

        let cb_no1 = usize::from(vq_idx[1]);
        let cb_no2 = usize::from(vq_idx[0]);
        let cb1 = IVI3_DELTA_CBS[cb_no1];
        let cb2 = IVI3_DELTA_CBS[cb_no2];

        let mut requant_idx = None;
        if (mode == 1) || (mode == 4) {
            let aq_idx = (vq_idx[0] << 4) | vq_idx[1];
            let mut idx = 42;
            for (i, &el) in CB_SELECTORS.iter().enumerate() {
                if el == aq_idx {
                    idx = i;
                    break;
                }
            }
            self.out[0] = (mode << 4) | (idx as u8);
        } else {
            self.out[0] = (mode << 4) | (cb_no1 as u8);

            if (8..=15).contains(&cb_no1) {
                requant_idx = Some(cb_no1 - 8);
            }
        }
        if self.cell.get_y() == 0 || !matches!(mode, 0 | 3 | 10) {
            requant_idx = None;
        }

        let start = 1;
        let mut iwriter = IndexWriter::new(&mut self.out[start..], self.do_rle);

        let esc_val1 = (cb1.data.len() / 2) as u8;
        let esc_val2 = (cb2.data.len() / 2) as u8;

        let cbs = [cb1, cb2, cb1, cb2];
        let esc_vals = [esc_val1, esc_val2, esc_val1, esc_val2];

        let mut first_line = self.cell.get_y() == 0;
        let stride = self.cell.get_width();

        if let Some(ridx) = requant_idx {// && !first_line {
            requant(&mut self.buf[..stride], ridx);
        }

        let mut cell4 = [0; 20];
        match mode {
            0 | 1 | 2 => {
                for y in (0..self.cell.get_height()).step_by(4) {
                    for x in (0..self.cell.get_width()).step_by(4) {
                        Self::get_cell4(&self.buf, x, y, stride, &mut cell4);
                        // first check if the cell can be coded with zero predictor
                        let mut diff = 0;
                        let mut pivot = 4;
                        for _y in 0..4 {
                            let (top, cur) = cell4.split_at(pivot);
                            let top = &top[top.len() - 4..];
                            for (&tval, &cval) in top.iter().zip(cur.iter()) {
                                let cdiff = i32::from(tval) - i32::from(cval);
                                diff += cdiff * cdiff;
                            }
                            pivot += 4;
                        }
                        if (diff as u32) < self.flat_thr_i {
                            iwriter.write_byte(ZERO_CELL);
                            let (top, tail) = cell4.split_at_mut(4);
                            for dline in tail.chunks_mut(4) {
                                dline.copy_from_slice(top);
                            }
                            Self::put_cell4(&mut self.buf, x, y, stride, &cell4);
                            continue;
                        }

                        compress_intra_cell(&mut iwriter, &mut cell4, &cbs, esc_vals);
                        Self::put_cell4(&mut self.buf, x, y, stride, &cell4);
                    }
                }
            },
            3 | 4 => {
                for y in (0..self.cell.get_height()).step_by(8) {
                    for x in (0..self.cell.get_width()).step_by(4) {
                        Self::get_cell_mode3(&self.buf, x, y, stride, &mut cell4);
                        compress_intra_cell(&mut iwriter, &mut cell4, &cbs, esc_vals);
                        Self::put_cell_mode3(&mut self.buf, x, y, stride, &cell4, first_line);
                    }
                    first_line = false;
                }
            },
            10 => {
                for y in (0..self.cell.get_height()).step_by(8) {
                    for x in (0..self.cell.get_width()).step_by(8) {
                        Self::get_cell_mode10i(&self.buf, x, y, stride, &mut cell4);
                        compress_intra_cell(&mut iwriter, &mut cell4, &cbs, esc_vals);
                        Self::put_cell_mode10i(&mut self.buf, x, y, stride, &cell4, first_line);
                    }
                    first_line = false;
                }
            },
            _ => unreachable!(),
        };
        iwriter.compact_all_cells();

        self.osize = iwriter.end() + start;
    }
    pub fn compress_inter(&mut self) {
        let (mode, vq_idx) = self.determine_mode(false, 0);

        let cb_no1 = usize::from(vq_idx[1]);
        let cb_no2 = usize::from(vq_idx[0]);
        let cb1 = IVI3_DELTA_CBS[cb_no1];
        let cb2 = IVI3_DELTA_CBS[cb_no2];

        if (mode == 1) || (mode == 4) {
            let aq_idx = (vq_idx[0] << 4) | vq_idx[1];
            let mut idx = 42;
            for (i, &el) in CB_SELECTORS.iter().enumerate() {
                if el == aq_idx {
                    idx = i;
                    break;
                }
            }
            self.out[0] = (mode << 4) | (idx as u8);
        } else {
            self.out[0] = (mode << 4) | (cb_no1 as u8);
        }
        let start = 1;
        let mut iwriter = IndexWriter::new(&mut self.out[start..], self.do_rle);

        let esc_val1 = (cb1.data.len() / 2) as u8;
        let esc_val2 = (cb2.data.len() / 2) as u8;

        let cbs = [cb1, cb2, cb1, cb2];
        let esc_vals = [esc_val1, esc_val2, esc_val1, esc_val2];


        let stride = self.cell.get_width();
        let mut ccell4 = [0; 20];
        let mut pcell4 = [0; 20];
        match mode {
            0 | 1 | 2 => {
                for y in (0..self.cell.get_height()).step_by(4) {
                    for x in (0..self.cell.get_width()).step_by(4) {
                        Self::get_cell4(&self.buf,  x, y, stride, &mut ccell4);
                        Self::get_cell4(&self.rbuf, x, y, stride, &mut pcell4);
                        // first check if the cell can be coded with zero predictor
                        let mut diff = 0;
                        for (&pval, &cval) in pcell4[4..].iter().zip(ccell4[4..].iter()) {
                            let cdiff = i32::from(pval) - i32::from(cval);
                            diff += cdiff * cdiff;
                        }
                        if diff < 8 {
                            iwriter.write_byte(SKIP_CELL);
                            Self::put_cell4(&mut self.buf, x, y, stride, &pcell4);
                            continue;
                        }

                        compress_inter_cell(&mut iwriter, &mut ccell4, &pcell4, &cbs, esc_vals);
                        Self::put_cell4(&mut self.buf, x, y, stride, &ccell4);
                    }
                }
            },
            10 => {
                let mut offset = 0;
                let mut ref_cell = [0; 64];
                let mut avg_diff: [i16; 16];
                for _y in (0..self.cell.get_height()).step_by(8) {
                    for x in (0..self.cell.get_width()).step_by(8) {
                        for (dline, sline) in ref_cell.chunks_mut(8).zip(self.rbuf[offset + stride + x..].chunks(stride)) {
                            dline.copy_from_slice(&sline[..8]);
                        }
                        avg_diff = [0; 16];
                        for j in 0..8 {
                            for i in 0..8 {
                                avg_diff[i / 2 + (j / 2) * 4] += i16::from(self.deltas[offset + x + i + j * stride]);
                            }
                        }
                        for el in avg_diff.iter_mut() {
                            *el = (*el + 2) >> 2;
                        }
                        compress_inter_cell_mode10(&mut iwriter, &mut ref_cell, &avg_diff, &cbs, esc_vals);
                        for (sline, dline) in ref_cell.chunks(8).zip(self.buf[offset + stride + x..].chunks_mut(stride)) {
                            dline[..8].copy_from_slice(sline);
                        }
                    }
                    offset += stride * 8;
                }
            },
            11 => {
                let mut offset = 0;
                let mut ref_cell = [0; 32];
                let mut avg_diff: [i16; 16];
                for _y in (0..self.cell.get_height()).step_by(8) {
                    for x in (0..self.cell.get_width()).step_by(4) {
                        for (dline, sline) in ref_cell.chunks_mut(4).zip(self.rbuf[offset + stride + x..].chunks(stride)) {
                            dline.copy_from_slice(&sline[..4]);
                        }
                        avg_diff = [0; 16];
                        for j in 0..8 {
                            for i in 0..4 {
                                avg_diff[i + (j / 2) * 4] += i16::from(self.deltas[offset + x + i + j * stride]);
                            }
                        }
                        for el in avg_diff.iter_mut() {
                            *el = (*el + 1) >> 1;
                        }

                        compress_inter_cell_mode11(&mut iwriter, &mut ref_cell, &avg_diff, &cbs, esc_vals);
                        for (sline, dline) in ref_cell.chunks(4).zip(self.buf[offset + stride + x..].chunks_mut(stride)) {
                            dline[..4].copy_from_slice(sline);
                        }
                    }
                    offset += stride * 8;
                }
            },
            _ => unreachable!(),
        };
        iwriter.compact_all_cells();

        self.osize = iwriter.end() + start;
    }

    fn get_cell4(data: &[u8], x: usize, y: usize, stride: usize, cell: &mut [u8; 20]) {
        for (dst, src) in cell.chunks_mut(4).zip(data[x + y * stride..].chunks(stride)) {
            dst.copy_from_slice(&src[..4]);
        }
    }
    fn put_cell4(data: &mut [u8], x: usize, y: usize, stride: usize, cell: &[u8; 20]) {
        for (src, dst) in cell.chunks(4).zip(data[x + y * stride..].chunks_mut(stride)).skip(1) {
            dst[..4].copy_from_slice(src);
        }
    }
    fn get_cell_mode3(data: &[u8], x: usize, y: usize, stride: usize, cell: &mut [u8; 20]) {
        let src = &data[x + y * stride..];
        for (dline, slines) in cell.chunks_mut(4).zip(src.chunks(stride * 2)) {
            dline.copy_from_slice(&slines[..4]);
        }
    }
    fn put_cell_mode3(data: &mut [u8], x: usize, y: usize, stride: usize, cell: &[u8; 20], first_line: bool) {
        let dst = &mut data[x + y * stride..];
        let mut dst_idx = stride;
        for line in 0..4 {
            for x in 0..4 {
                let top = cell[line * 4 + x];
                let cur = cell[(line + 1) * 4 + x];
                dst[dst_idx + x]          = (top + cur) >> 1;
                dst[dst_idx + stride + x] = cur;
            }
            dst_idx += stride * 2;
        }
        if first_line {
            dst[stride..][..4].copy_from_slice(&cell[4..8]);
        }
    }
    fn get_cell_mode10i(data: &[u8], x: usize, y: usize, stride: usize, cell: &mut [u8; 20]) {
        let src = &data[x + y * stride..];
        for (dline, src_pair) in cell.chunks_mut(4).zip(src.chunks(stride * 2)) {
            for (dst, src) in dline.iter_mut().zip(src_pair.chunks(2)) {
                *dst = src[0];
            }
        }
    }
    fn put_cell_mode10i(data: &mut [u8], x: usize, y: usize, stride: usize, cell: &[u8; 20], first_line: bool) {
        let dst = &mut data[x + y * stride..];
        let mut dst_idx = stride;
        for line in 0..4 {
            for x in 0..8 {
                let top = dst[dst_idx - stride + x];
                let cur = cell[(line + 1) * 4 + x / 2];
                dst[dst_idx + x]            = (top + cur) >> 1;
                dst[dst_idx + stride + x]   = cur;
            }
            dst_idx += stride * 2;
        }
        if first_line {
            let (top, tail) = dst[stride..].split_at_mut(stride);
            top[..8].copy_from_slice(&tail[..8]);
        }
    }
}

fn requant(line: &mut [u8], rq_index: usize) {
    let tab = &REQUANT_TAB[rq_index];
    for el in line.iter_mut() {
        *el = tab[usize::from(*el)];
    }
}

fn compress_intra_cell(iwriter: &mut IndexWriter, cell4: &mut [u8; 20], cbs: &[&IviDeltaCB; 4], esc_vals: [u8; 4]) {
    let mut pivot = 4;
    for y in 0..4 {
        let cb = cbs[y];
        let esc_val = esc_vals[y];

        let (prev, cur) = cell4.split_at_mut(pivot);
        let prev = &prev[prev.len() - 4..];
        let cur = &mut cur[..4];
        let (idx0, idx1) = find_quad(cb.data, prev, cur);

        cur[0] = ((prev[0] as i8) + cb.data[usize::from(idx1) * 2]) as u8;
        cur[1] = ((prev[1] as i8) + cb.data[usize::from(idx1) * 2 + 1]) as u8;
        cur[2] = ((prev[2] as i8) + cb.data[usize::from(idx0) * 2]) as u8;
        cur[3] = ((prev[3] as i8) + cb.data[usize::from(idx0) * 2 + 1]) as u8;

        iwriter.write_pair(idx0, idx1, cb.quad_radix, esc_val);

        pivot += 4;
    }
    iwriter.compact_cell(esc_vals);
}

fn compress_inter_cell(iwriter: &mut IndexWriter, ccell4: &mut [u8; 20], pcell: &[u8; 20], cbs: &[&IviDeltaCB; 4], esc_vals: [u8; 4]) {
    for (y, (prev, cur)) in pcell[4..].chunks(4).zip(ccell4[4..].chunks_mut(4)).enumerate() {
        let cb = cbs[y];
        let esc_val = esc_vals[y];

        let (idx0, idx1) = find_quad(cb.data, prev, cur);

        cur[0] = ((prev[0] as i8) + cb.data[usize::from(idx1) * 2]) as u8;
        cur[1] = ((prev[1] as i8) + cb.data[usize::from(idx1) * 2 + 1]) as u8;
        cur[2] = ((prev[2] as i8) + cb.data[usize::from(idx0) * 2]) as u8;
        cur[3] = ((prev[3] as i8) + cb.data[usize::from(idx0) * 2 + 1]) as u8;

        iwriter.write_pair(idx0, idx1, cb.quad_radix, esc_val);
    }
    iwriter.compact_cell(esc_vals);
}

fn compress_inter_cell_mode10(iwriter: &mut IndexWriter, cell: &mut [u8; 64], diffs: &[i16; 16], cbs: &[&IviDeltaCB; 4], esc_vals: [u8; 4]) {
    for y in 0..4 {
        let cb = cbs[y];
        let esc_val = esc_vals[y];
        let mut indices = [0, 0];
        for pair_no in (0..4).step_by(2) {
            let src_idx = y * 8 * 2 + pair_no * 2;
            let src0 = [cell[src_idx], cell[src_idx + 1], cell[src_idx + 8], cell[src_idx + 9]];
            let src1 = [cell[src_idx + 2], cell[src_idx + 3], cell[src_idx + 10], cell[src_idx + 11]];

            let cur_diff = [diffs[y * 4 + pair_no] as i8, diffs[y * 4 + pair_no + 1] as i8];

            let mut best_idx = 0;
            let mut best_dist = pair_dist(&cur_diff, &[0, 0]);
            for (idx, cbpair) in cb.data.chunks(2).enumerate().skip(1) {
                let dist = pair_dist(&cur_diff, cbpair);
                if dist < best_dist {
                    let mut fits = true;
                    for &el in src0.iter() {
                        if !in_range(el as i8, cbpair[0]) {
                            fits = false;
                            break;
                        }
                    }
                    for &el in src1.iter() {
                        if !in_range(el as i8, cbpair[1]) {
                            fits = false;
                            break;
                        }
                    }
                    if fits {
                        best_dist = dist;
                        best_idx = idx;
                    }
                }
            }

            indices[pair_no / 2] = best_idx as u8;

            let cb_pair = &cb.data[best_idx * 2..];
            for row in cell[src_idx..].chunks_mut(8).take(2) {
                row[0] = ((row[0] as i8) + cb_pair[0]) as u8;
                row[1] = ((row[1] as i8) + cb_pair[0]) as u8;
                row[2] = ((row[2] as i8) + cb_pair[1]) as u8;
                row[3] = ((row[3] as i8) + cb_pair[1]) as u8;
            }
        }
        iwriter.write_pair(indices[1], indices[0], cb.quad_radix, esc_val);
    }
    iwriter.compact_cell(esc_vals);
}

fn compress_inter_cell_mode11(iwriter: &mut IndexWriter, cell: &mut [u8; 32], diffs: &[i16; 16], cbs: &[&IviDeltaCB; 4], esc_vals: [u8; 4]) {
    for y in 0..4 {
        let cb = cbs[y];
        let esc_val = esc_vals[y];
        let mut indices = [0, 0];
        for pair_no in (0..4).step_by(2) {
            let src_idx = y * 4 * 2 + pair_no;
            let src0 = [cell[src_idx], cell[src_idx + 4]];
            let src1 = [cell[src_idx + 1], cell[src_idx + 5]];

            let cur_diff = [diffs[y * 4 + pair_no] as i8, diffs[y * 4 + pair_no + 1] as i8];

            let mut best_idx = 0;
            let mut best_dist = pair_dist(&cur_diff, &[0, 0]);
            for (idx, cbpair) in cb.data.chunks(2).enumerate().skip(1) {
                let dist = pair_dist(&cur_diff, cbpair);
                if dist < best_dist {
                    let mut fits = true;
                    for &el in src0.iter() {
                        if !in_range(el as i8, cbpair[0]) {
                            fits = false;
                            break;
                        }
                    }
                    for &el in src1.iter() {
                        if !in_range(el as i8, cbpair[1]) {
                            fits = false;
                            break;
                        }
                    }
                    if fits {
                        best_dist = dist;
                        best_idx = idx;
                    }
                }
            }

            indices[pair_no / 2] = best_idx as u8;

            let cb_pair = &cb.data[best_idx * 2..];
            cell[src_idx]     = ((cell[src_idx]     as i8) + cb_pair[0]) as u8;
            cell[src_idx + 4] = ((cell[src_idx + 4] as i8) + cb_pair[0]) as u8;
            cell[src_idx + 1] = ((cell[src_idx + 1] as i8) + cb_pair[1]) as u8;
            cell[src_idx + 5] = ((cell[src_idx + 5] as i8) + cb_pair[1]) as u8;
        }
        iwriter.write_pair(indices[1], indices[0], cb.quad_radix, esc_val);
    }
    iwriter.compact_cell(esc_vals);
}

fn pair_dist(src: &[i8], pair: &[i8]) -> u32 {
    let d0 = (i32::from(src[0]) - i32::from(pair[0])).unsigned_abs();
    let d1 = (i32::from(src[1]) - i32::from(pair[1])).unsigned_abs();
    d0 * d0 + d1 * d1
}

fn in_range(base: i8, delta: i8) -> bool {
    if let Some(val) = base.checked_add(delta) {
        val >= 0
    } else {
        false
    }
}

fn find_pair(cb_data: &[i8], ppair: &[u8], cpair: &[u8]) -> u8 {
    let ppair = [ppair[0] as i8, ppair[1] as i8];
    let diff = [(cpair[0] as i8) - ppair[0], (cpair[1] as i8) - ppair[1]];
    // pair 0 is always zero;
    if diff == [0, 0] {
        return 0;
    }
    let mut best_idx = 0;
    let mut best_dist = pair_dist(&diff, &[0, 0]);
    for (idx, cbpair) in cb_data.chunks(2).enumerate().skip(1) {
        let dist = pair_dist(&diff, cbpair);
        if dist < best_dist && in_range(ppair[0], cbpair[0]) && in_range(ppair[1], cbpair[1]) {
            best_dist = dist;
            best_idx = idx;
        }
    }
    best_idx as u8
}

fn find_quad(cb_data: &[i8], prev: &[u8], cur: &[u8]) -> (u8, u8) {
    let (ppair1, ppair0) = prev.split_at(2);
    let (cpair1, cpair0) = cur.split_at(2);
    let idx1 = find_pair(cb_data, ppair1, cpair1);
    let idx0 = find_pair(cb_data, ppair0, cpair0);
    (idx0, idx1)
}

const REQUANT_TAB: [[u8; 128]; 8] = [
  [
    0x00, 0x02, 0x02, 0x04, 0x04, 0x06, 0x06, 0x08,
    0x08, 0x0a, 0x0a, 0x0c, 0x0c, 0x0e, 0x0e, 0x10,
    0x10, 0x12, 0x12, 0x14, 0x14, 0x16, 0x16, 0x18,
    0x18, 0x1a, 0x1a, 0x1c, 0x1c, 0x1e, 0x1e, 0x20,
    0x20, 0x22, 0x22, 0x24, 0x24, 0x26, 0x26, 0x28,
    0x28, 0x2a, 0x2a, 0x2c, 0x2c, 0x2e, 0x2e, 0x30,
    0x30, 0x32, 0x32, 0x34, 0x34, 0x36, 0x36, 0x38,
    0x38, 0x3a, 0x3a, 0x3c, 0x3c, 0x3e, 0x3e, 0x40,
    0x40, 0x42, 0x42, 0x44, 0x44, 0x46, 0x46, 0x48,
    0x48, 0x4a, 0x4a, 0x4c, 0x4c, 0x4e, 0x4e, 0x50,
    0x50, 0x52, 0x52, 0x54, 0x54, 0x56, 0x56, 0x58,
    0x58, 0x5a, 0x5a, 0x5c, 0x5c, 0x5e, 0x5e, 0x60,
    0x60, 0x62, 0x62, 0x64, 0x64, 0x66, 0x66, 0x68,
    0x68, 0x6a, 0x6a, 0x6c, 0x6c, 0x6e, 0x6e, 0x70,
    0x70, 0x72, 0x72, 0x74, 0x74, 0x76, 0x76, 0x78,
    0x78, 0x7a, 0x7a, 0x7c, 0x7c, 0x7e, 0x7e, 0x7e
  ], [
    0x01, 0x01, 0x04, 0x04, 0x04, 0x07, 0x07, 0x0a,
    0x0a, 0x0a, 0x0a, 0x0d, 0x0d, 0x0d, 0x10, 0x10,
    0x10, 0x13, 0x13, 0x13, 0x16, 0x16, 0x16, 0x19,
    0x19, 0x19, 0x1c, 0x1c, 0x1c, 0x1f, 0x1f, 0x1f,
    0x22, 0x22, 0x22, 0x25, 0x25, 0x25, 0x28, 0x28,
    0x28, 0x2b, 0x2b, 0x2b, 0x2e, 0x2e, 0x2e, 0x31,
    0x31, 0x31, 0x34, 0x34, 0x34, 0x37, 0x37, 0x37,
    0x3a, 0x3a, 0x3a, 0x3d, 0x3d, 0x3d, 0x40, 0x40,
    0x40, 0x43, 0x43, 0x43, 0x46, 0x46, 0x46, 0x49,
    0x49, 0x49, 0x4c, 0x4c, 0x4c, 0x4f, 0x4f, 0x4f,
    0x52, 0x52, 0x52, 0x55, 0x55, 0x55, 0x58, 0x58,
    0x58, 0x5b, 0x5b, 0x5b, 0x5e, 0x5e, 0x5e, 0x61,
    0x61, 0x61, 0x64, 0x64, 0x64, 0x67, 0x67, 0x67,
    0x6a, 0x6a, 0x6a, 0x6d, 0x6d, 0x6d, 0x70, 0x70,
    0x70, 0x73, 0x73, 0x73, 0x76, 0x76, 0x76, 0x76,
    0x76, 0x79, 0x7c, 0x7c, 0x7c, 0x7f, 0x7f, 0x7f
  ], [
    0x00, 0x00, 0x04, 0x04, 0x04, 0x04, 0x08, 0x08,
    0x08, 0x08, 0x0c, 0x0c, 0x0c, 0x0c, 0x10, 0x10,
    0x10, 0x10, 0x14, 0x14, 0x14, 0x14, 0x18, 0x18,
    0x18, 0x18, 0x1c, 0x1c, 0x1c, 0x1c, 0x20, 0x20,
    0x20, 0x20, 0x24, 0x24, 0x24, 0x24, 0x28, 0x28,
    0x28, 0x28, 0x2c, 0x2c, 0x2c, 0x2c, 0x30, 0x30,
    0x30, 0x30, 0x34, 0x34, 0x34, 0x34, 0x38, 0x38,
    0x38, 0x38, 0x3c, 0x3c, 0x3c, 0x3c, 0x40, 0x40,
    0x40, 0x40, 0x44, 0x44, 0x44, 0x44, 0x48, 0x48,
    0x48, 0x48, 0x4c, 0x4c, 0x4c, 0x4c, 0x50, 0x50,
    0x50, 0x50, 0x54, 0x54, 0x54, 0x54, 0x58, 0x58,
    0x58, 0x58, 0x5c, 0x5c, 0x5c, 0x5c, 0x60, 0x60,
    0x60, 0x60, 0x64, 0x64, 0x64, 0x64, 0x68, 0x68,
    0x68, 0x68, 0x6c, 0x6c, 0x6c, 0x6c, 0x70, 0x70,
    0x70, 0x70, 0x74, 0x74, 0x74, 0x74, 0x78, 0x78,
    0x78, 0x78, 0x7c, 0x7c, 0x7c, 0x7c, 0x7c, 0x7c
  ], [
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x09, 0x09, 0x09, 0x09, 0x09, 0x0e, 0x0e, 0x0e,
    0x0e, 0x0e, 0x13, 0x13, 0x13, 0x13, 0x13, 0x18,
    0x18, 0x18, 0x18, 0x18, 0x1d, 0x1d, 0x1d, 0x1d,
    0x1d, 0x22, 0x22, 0x22, 0x22, 0x22, 0x27, 0x27,
    0x27, 0x27, 0x27, 0x2c, 0x2c, 0x2c, 0x2c, 0x2c,
    0x31, 0x31, 0x31, 0x31, 0x31, 0x36, 0x36, 0x36,
    0x36, 0x36, 0x3b, 0x3b, 0x3b, 0x3b, 0x3b, 0x40,
    0x40, 0x40, 0x40, 0x40, 0x45, 0x45, 0x45, 0x45,
    0x45, 0x4a, 0x4a, 0x4a, 0x4a, 0x4a, 0x4f, 0x4f,
    0x4f, 0x4f, 0x4f, 0x54, 0x54, 0x54, 0x54, 0x54,
    0x59, 0x59, 0x59, 0x59, 0x59, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x63, 0x63, 0x63, 0x63, 0x63, 0x68,
    0x68, 0x68, 0x68, 0x68, 0x6d, 0x6d, 0x6d, 0x6d,
    0x6d, 0x72, 0x72, 0x72, 0x72, 0x72, 0x77, 0x77,
    0x77, 0x77, 0x77, 0x7c, 0x7c, 0x7c, 0x7c, 0x7c
  ], [
    0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
    0x0a, 0x0a, 0x0a, 0x0a, 0x0a, 0x0a, 0x0a, 0x10,
    0x10, 0x10, 0x10, 0x10, 0x10, 0x16, 0x16, 0x16,
    0x16, 0x16, 0x16, 0x1c, 0x1c, 0x1c, 0x1c, 0x1c,
    0x1c, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x28,
    0x28, 0x28, 0x28, 0x28, 0x28, 0x2e, 0x2e, 0x2e,
    0x2e, 0x2e, 0x2e, 0x34, 0x34, 0x34, 0x34, 0x34,
    0x34, 0x3a, 0x3a, 0x3a, 0x3a, 0x3a, 0x3a, 0x40,
    0x40, 0x40, 0x40, 0x40, 0x40, 0x46, 0x46, 0x46,
    0x46, 0x46, 0x46, 0x4c, 0x4c, 0x4c, 0x4c, 0x4c,
    0x4c, 0x52, 0x52, 0x52, 0x52, 0x52, 0x52, 0x58,
    0x58, 0x58, 0x58, 0x58, 0x58, 0x5e, 0x5e, 0x5e,
    0x5e, 0x5e, 0x5e, 0x64, 0x64, 0x64, 0x64, 0x64,
    0x64, 0x6a, 0x6a, 0x6a, 0x6a, 0x6a, 0x6a, 0x70,
    0x70, 0x70, 0x70, 0x70, 0x70, 0x76, 0x76, 0x76,
    0x76, 0x76, 0x76, 0x7c, 0x7c, 0x7c, 0x7c, 0x7c
  ], [
    0x01, 0x01, 0x01, 0x01, 0x08, 0x08, 0x08, 0x08,
    0x08, 0x08, 0x08, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f,
    0x0f, 0x0f, 0x16, 0x16, 0x16, 0x16, 0x16, 0x16,
    0x16, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d,
    0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x2b,
    0x2b, 0x2b, 0x2b, 0x2b, 0x2b, 0x2b, 0x32, 0x32,
    0x32, 0x32, 0x32, 0x32, 0x32, 0x39, 0x39, 0x39,
    0x39, 0x39, 0x39, 0x39, 0x40, 0x40, 0x40, 0x40,
    0x40, 0x40, 0x40, 0x47, 0x47, 0x47, 0x47, 0x47,
    0x47, 0x47, 0x4e, 0x4e, 0x4e, 0x4e, 0x4e, 0x4e,
    0x4e, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
    0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x63,
    0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x6a, 0x6a,
    0x6a, 0x6a, 0x6a, 0x6a, 0x6a, 0x71, 0x71, 0x71,
    0x71, 0x71, 0x71, 0x71, 0x78, 0x78, 0x78, 0x78,
    0x78, 0x78, 0x78, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f
  ], [
    0x00, 0x00, 0x00, 0x00, 0x08, 0x08, 0x08, 0x08,
    0x08, 0x08, 0x08, 0x08, 0x10, 0x10, 0x10, 0x10,
    0x10, 0x10, 0x10, 0x10, 0x18, 0x18, 0x18, 0x18,
    0x18, 0x18, 0x18, 0x18, 0x20, 0x20, 0x20, 0x20,
    0x20, 0x20, 0x20, 0x20, 0x28, 0x28, 0x28, 0x28,
    0x28, 0x28, 0x28, 0x28, 0x30, 0x30, 0x30, 0x30,
    0x30, 0x30, 0x30, 0x30, 0x38, 0x38, 0x38, 0x38,
    0x38, 0x38, 0x38, 0x38, 0x40, 0x40, 0x40, 0x40,
    0x40, 0x40, 0x40, 0x40, 0x48, 0x48, 0x48, 0x48,
    0x48, 0x48, 0x48, 0x48, 0x50, 0x50, 0x50, 0x50,
    0x50, 0x50, 0x50, 0x50, 0x58, 0x58, 0x58, 0x58,
    0x58, 0x58, 0x58, 0x58, 0x60, 0x60, 0x60, 0x60,
    0x60, 0x60, 0x60, 0x60, 0x68, 0x68, 0x68, 0x68,
    0x68, 0x68, 0x68, 0x68, 0x70, 0x70, 0x70, 0x70,
    0x70, 0x70, 0x70, 0x70, 0x78, 0x78, 0x78, 0x78,
    0x78, 0x78, 0x78, 0x78, 0x78, 0x78, 0x78, 0x78
  ], [
    0x01, 0x01, 0x01, 0x01, 0x01, 0x0a, 0x0a, 0x0a,
    0x0a, 0x0a, 0x0a, 0x0a, 0x0a, 0x0a, 0x13, 0x13,
    0x13, 0x13, 0x13, 0x13, 0x13, 0x13, 0x13, 0x1c,
    0x1c, 0x1c, 0x1c, 0x1c, 0x1c, 0x1c, 0x1c, 0x1c,
    0x25, 0x25, 0x25, 0x25, 0x25, 0x25, 0x25, 0x25,
    0x25, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
    0x2e, 0x2e, 0x37, 0x37, 0x37, 0x37, 0x37, 0x37,
    0x37, 0x37, 0x37, 0x40, 0x40, 0x40, 0x40, 0x40,
    0x40, 0x40, 0x40, 0x40, 0x49, 0x49, 0x49, 0x49,
    0x49, 0x49, 0x49, 0x49, 0x49, 0x52, 0x52, 0x52,
    0x52, 0x52, 0x52, 0x52, 0x52, 0x52, 0x5b, 0x5b,
    0x5b, 0x5b, 0x5b, 0x5b, 0x5b, 0x5b, 0x5b, 0x64,
    0x64, 0x64, 0x64, 0x64, 0x64, 0x64, 0x64, 0x64,
    0x6d, 0x6d, 0x6d, 0x6d, 0x6d, 0x6d, 0x6d, 0x6d,
    0x6d, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76,
    0x76, 0x76, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f
  ]
];
