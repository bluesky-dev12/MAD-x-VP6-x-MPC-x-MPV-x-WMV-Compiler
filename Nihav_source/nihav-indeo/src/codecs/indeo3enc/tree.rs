use super::Indeo3Writer;
use super::mv::*;
use super::cell::{CellEncoder, MAX_CELL_SIZE};
use std::ops::DerefMut;

pub enum Indeo3PrimaryTree {
    VSplit(Box<Indeo3PrimaryTree>, Box<Indeo3PrimaryTree>),
    HSplit(Box<Indeo3PrimaryTree>, Box<Indeo3PrimaryTree>),
    RelFill(MV, Box<Indeo3SecondaryTree>),
    AbsFill(Box<Indeo3SecondaryTree>),
}

impl Indeo3PrimaryTree {
    pub fn print(&self) {
        println!("Plane tree:");
        self.print1(1);
    }
    fn print1(&self, depth: u8) {
        for _ in 0..depth {
            print!("    ");
        }
        match self {
            Indeo3PrimaryTree::VSplit(t1, t2) => {
                println!("vertical split");
                t1.print1(depth + 1);
                t2.print1(depth + 1);
            },
            Indeo3PrimaryTree::HSplit(t1, t2) => {
                println!("horizontal split");
                t1.print1(depth + 1);
                t2.print1(depth + 1);
            },
            Indeo3PrimaryTree::RelFill(mv, sec) => {
                println!("relative fill {},{}", mv.x, mv.y);
                sec.print1(depth + 1);
            },
            Indeo3PrimaryTree::AbsFill(sec) => {
                println!("absolute fill");
                sec.print1(depth + 1);
            }
        }
    }
}

pub enum Indeo3SecondaryTree {
    VSplit(Box<Indeo3SecondaryTree>, Box<Indeo3SecondaryTree>),
    HSplit(Box<Indeo3SecondaryTree>, Box<Indeo3SecondaryTree>),
    VQData(u8),
    VQNull(u8),
}

impl Indeo3SecondaryTree {
    fn print1(&self, depth: u8) {
        for _ in 0..depth {
            print!("    ");
        }
        match self {
            Indeo3SecondaryTree::VSplit(t1, t2) => {
                println!("vertical split");
                t1.print1(depth + 1);
                t2.print1(depth + 1);
            },
            Indeo3SecondaryTree::HSplit(t1, t2) => {
                println!("horizontal split");
                t1.print1(depth + 1);
                t2.print1(depth + 1);
            },
            Indeo3SecondaryTree::VQData(mode) => {
                println!("VQ data ({})", mode);
            },
            Indeo3SecondaryTree::VQNull(mode) => {
                println!("VQ Null ({})", mode);
            }
        }
    }
}

const THRESHOLD: u32 = 64;

#[derive(Clone, Copy)]
pub struct Indeo3Cell {
    x:      u8,
    y:      u8,
    w:      u8,
    h:      u8,
    intra:  bool,
}

impl Indeo3Cell {
    pub fn new(width: usize, height: usize) -> Self {
        Self {
            x:      0,
            y:      0,
            w:      (width / 4) as u8,
            h:      (height / 4) as u8,
            intra:  false,
        }
    }

    pub fn get_x(&self) -> usize { usize::from(self.x) * 4 }
    pub fn get_y(&self) -> usize { usize::from(self.y) * 4 }
    pub fn get_width(&self) -> usize { usize::from(self.w) * 4 }
    pub fn get_height(&self) -> usize { usize::from(self.h) * 4 }
    pub fn is_intra(&self) -> bool { self.intra }

    fn split_h(&self) -> (Self, Self) {
        let h1 = if self.h > 2 { ((self.h + 2) >> 2) << 1 } else { 1 };
        let h2 = self.h - h1;
        let mut cell1 = *self;
        cell1.h  = h1;
        let mut cell2 = *self;
        cell2.y += h1;
        cell2.h  = h2;
        (cell1, cell2)
    }
    fn split_v(&self, stripw: u8) -> (Self, Self) {
        let w1 = if self.w > stripw {
                if self.w > stripw * 2 { stripw * 2 } else { stripw }
            } else {
                if self.w > 2 { ((self.w + 2) >> 2) << 1 } else { 1 }
            };
        let w2 = self.w - w1;
        let mut cell1 = *self;
        cell1.w  = w1;
        let mut cell2 = *self;
        cell2.x += w1;
        cell2.w  = w2;
        (cell1, cell2)
    }
}

impl std::fmt::Display for Indeo3Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[{}x{} @ {},{}{}]", self.get_width(), self.get_height(), self.get_x(), self.get_y(), if self.intra { " intra" } else { "" })
    }
}

#[derive(Default)]
pub struct Plane {
    pub data:   Vec<u8>,
    pub width:  usize,
    pub height: usize,
    pub stripw: u8,
    pub mvs:    Vec<(MV, u16)>,
}

fn ssd(a: u8, b: u8) -> u32 {
    let diff = i32::from(a) - i32::from(b);
    (diff * diff) as u32
}

impl Plane {
    pub fn alloc(&mut self, width: usize, height: usize, stripw: u8) {
        self.data.resize(width * height, 0);
        self.width  = width;
        self.height = height;
        self.stripw = stripw;
        if self.mvs.capacity() < 256 {
            self.mvs.reserve(256);
        }
    }
    pub fn fill(&mut self, src: &[u8], stride: usize) {
        for (dline, sline) in self.data.chunks_mut(self.width).zip(src.chunks(stride)) {
            for (dst, &src) in dline.iter_mut().zip(sline.iter()) {
                *dst = src >> 1;
            }
        }
    }
    pub fn clear_mvs(&mut self){
        self.mvs.clear();
    }
    pub fn checksum(&self) -> u16 {
        let xors = self.data.chunks(2).fold([0u8; 2], |acc, pair| [acc[0] ^ pair[0], acc[1] ^ pair[1]]);
        u16::from(xors[0]) | (u16::from(xors[1]) * 256)
    }
    pub fn find_cells(&mut self, is_intra: bool, pplane: &Plane, mv_est: &MotionEstimator) -> Box<Indeo3PrimaryTree> {
        let cell = Indeo3Cell::new(self.width, self.height);
        self.split_pri(cell, pplane, mv_est, is_intra)
    }
    fn split_pri(&mut self, mut cell: Indeo3Cell, pplane: &Plane, mv_est: &MotionEstimator, is_intra: bool) -> Box<Indeo3PrimaryTree> {
        let width  = cell.get_width();
        let height = cell.get_height();
        if width * height > MAX_CELL_SIZE {
            let (hsplit, vsplit) = if width != height {
                    (width > (self.stripw as usize) * 4 || width > height, height > width)
                } else {
                    let (hdiff, vdiff) = self.calculate_diffs(cell);
                    (vdiff > THRESHOLD && vdiff > hdiff,
                     hdiff > THRESHOLD && hdiff > vdiff)
                };
            match (hsplit, vsplit) {
                (true, _) => {
                    let (cell1, cell2) = cell.split_v(self.stripw);
                    let tree1 = self.split_pri(cell1, pplane, mv_est, is_intra);
                    let tree2 = self.split_pri(cell2, pplane, mv_est, is_intra);
                    Box::new(Indeo3PrimaryTree::VSplit(tree1, tree2))
                },
                (_, true) => {
                    let (cell1, cell2) = cell.split_h();
                    let tree1 = self.split_pri(cell1, pplane, mv_est, is_intra);
                    let tree2 = self.split_pri(cell2, pplane, mv_est, is_intra);
                    Box::new(Indeo3PrimaryTree::HSplit(tree1, tree2))
                },
                (false, false) => {
                    let sec = self.split_sec(cell);
                    Box::new(Indeo3PrimaryTree::AbsFill(sec))
                },
            }
        } else {
            if !is_intra {
                if let Some((mv, flat)) = mv_est.mv_search(self, pplane, cell) {
                    return self.add_mv_tree(mv, flat, cell);
                }

                // try splitting once to see if it improves situation
                if width >= 16 && height >= 16 {
                    let vsplit = width > height;
                    let (mut cell1, mut cell2) = if vsplit {
                            cell.split_v(self.stripw)
                        } else {
                            cell.split_h()
                        };
                    let search1 = mv_est.mv_search(self, pplane, cell1);
                    let search2 = mv_est.mv_search(self, pplane, cell2);
                    if search1.is_some() || search2.is_some() {
                        let tree1 = if let Some((mv, flat)) = search1 {
                                self.add_mv_tree(mv, flat, cell1)
                            } else {
                                cell1.intra = true;
                                let sec = self.split_sec(cell1);
                                Box::new(Indeo3PrimaryTree::AbsFill(sec))
                            };
                        let tree2 = if let Some((mv, flat)) = search2 {
                                self.add_mv_tree(mv, flat, cell2)
                            } else {
                                cell2.intra = true;
                                let sec = self.split_sec(cell2);
                                Box::new(Indeo3PrimaryTree::AbsFill(sec))
                            };
                        return if vsplit {
                                Box::new(Indeo3PrimaryTree::VSplit(tree1, tree2))
                            } else {
                                Box::new(Indeo3PrimaryTree::HSplit(tree1, tree2))
                            }
                    }
                }
            }
            cell.intra = true;
            let sec = self.split_sec(cell);
            Box::new(Indeo3PrimaryTree::AbsFill(sec))
        }
    }
    fn add_mv_tree(&mut self, mv: MV, flat: bool, cell: Indeo3Cell) -> Box<Indeo3PrimaryTree> {
        let sec = if flat {
                Box::new(Indeo3SecondaryTree::VQNull(0))
            } else {
                Box::new(Indeo3SecondaryTree::VQData(0))
            };

        let mut found = false;
        for (ref cmv, ref mut count) in self.mvs.iter_mut() {
            if cmv == &mv {
                *count += u16::from(cell.w) * u16::from(cell.h);
                found = true;
                break;
            }
        }
        if !found {
            self.mvs.push((mv, 1));
        }

        Box::new(Indeo3PrimaryTree::RelFill(mv, sec))
    }
    fn split_sec(&mut self, cell: Indeo3Cell) -> Box<Indeo3SecondaryTree> {
        let (hdiff, vdiff) = self.calculate_diffs(cell);
        if hdiff == 0 && vdiff == 0 {
            if !cell.intra {
                return Box::new(Indeo3SecondaryTree::VQNull(0));
            } else {
                return Box::new(Indeo3SecondaryTree::VQData(0));
            }
        }
        if cell.get_width() > 16 && cell.get_height() > 16 {
            let hsplit = vdiff > THRESHOLD && vdiff > hdiff * 2;
            let vsplit = hdiff > THRESHOLD && hdiff > vdiff * 2;
            match (vsplit, hsplit) {
                (true, _) => {
                    let (cell1, cell2) = cell.split_v(self.stripw);
                    let tree1 = self.split_sec(cell1);
                    let tree2 = self.split_sec(cell2);
                    Box::new(Indeo3SecondaryTree::VSplit(tree1, tree2))
                },
                (_, true) => {
                    let (cell1, cell2) = cell.split_h();
                    let tree1 = self.split_sec(cell1);
                    let tree2 = self.split_sec(cell2);
                    Box::new(Indeo3SecondaryTree::HSplit(tree1, tree2))
                },
                _ => {
                    Box::new(Indeo3SecondaryTree::VQData(0))
                },
            }
        } else {
            let is_w8 = (cell.get_width()  & 7) == 0;
            let is_h8 = (cell.get_height() & 7) == 0;
            let mode = match (hdiff > THRESHOLD, vdiff > THRESHOLD) {
                    (false, false) if is_w8 && is_h8 => 10,
                    (_, true)      if is_h8          => 3,
                    _                                => 0,
                };
            Box::new(Indeo3SecondaryTree::VQData(mode))
        }
    }
    fn calculate_diffs(&self, cell: Indeo3Cell) -> (u32, u32) {
        let offset = cell.get_x() + cell.get_y() * self.width;
        let mut w = cell.get_width();
        if cell.get_x() + w == self.width { w -= 1; }
        let mut h = cell.get_height();
        if cell.get_y() + h == self.height { h -= 1; }

        let mut vdiff = 0;
        let mut hdiff = 0;
        let src0 = &self.data[offset..];
        let src1 = &self.data[offset + self.width..];
        for (line0, line1) in src0.chunks(self.width).zip(src1.chunks(self.width)).take(h) {
            for ((&cur, &right), &bottom) in line0.iter().zip(line0[1..].iter()).zip(line1.iter()).take(w) {
                hdiff += ssd(cur, right);
                vdiff += ssd(cur, bottom);
            }
        }
        let area = (w * h) as u32;
        (hdiff * 16 / area, vdiff * 16 / area)
    }
    pub fn prune_extra_mvs(&mut self, tree: &mut Box<Indeo3PrimaryTree>) {
        let cell = Indeo3Cell::new(self.width, self.height);
        self.prune_pri(cell, tree)
    }
    fn prune_pri(&mut self, cell: Indeo3Cell, tree: &mut Box<Indeo3PrimaryTree>) {
        match tree.deref_mut() {
            Indeo3PrimaryTree::HSplit(ref mut tree1, ref mut tree2) => {
                let (cell1, cell2) = cell.split_h();
                self.prune_pri(cell1, tree1);
                self.prune_pri(cell2, tree2);
            },
            Indeo3PrimaryTree::VSplit(ref mut tree1, ref mut tree2) => {
                let (cell1, cell2) = cell.split_v(self.stripw);
                self.prune_pri(cell1, tree1);
                self.prune_pri(cell2, tree2);
            },
            Indeo3PrimaryTree::AbsFill(_) => {},
            Indeo3PrimaryTree::RelFill(ref mv, ref _sec) => {
                if find_mv(*mv, &self.mvs).is_none() {
                    let sec = self.split_sec(cell);
                    *tree = Box::new(Indeo3PrimaryTree::AbsFill(sec));
                }
            },
        }
    }
    pub fn encode_tree(&mut self, iw: &mut Indeo3Writer, tree: &Indeo3PrimaryTree, cenc: &mut CellEncoder, refp: &Plane) {
        let cell = Indeo3Cell::new(self.width, self.height);
        self.encode_pri(iw, cell, tree, cenc, refp);
    }
    fn encode_pri(&mut self, iw: &mut Indeo3Writer, mut cell: Indeo3Cell, tree: &Indeo3PrimaryTree, cenc: &mut CellEncoder, refp: &Plane) {
        match tree {
            Indeo3PrimaryTree::HSplit(t1, t2) => {
                iw.put_2bits(0);
                let (cell1, cell2) = cell.split_h();
                self.encode_pri(iw, cell1, t1, cenc, refp);
                self.encode_pri(iw, cell2, t2, cenc, refp);
            },
            Indeo3PrimaryTree::VSplit(t1, t2) => {
                iw.put_2bits(1);
                let (cell1, cell2) = cell.split_v(self.stripw);
                self.encode_pri(iw, cell1, t1, cenc, refp);
                self.encode_pri(iw, cell2, t2, cenc, refp);
            },
            Indeo3PrimaryTree::AbsFill(sec) => {
                iw.put_2bits(2);
                cell.intra = true;
                self.encode_sec(iw, cell, sec, cenc);
            }
            Indeo3PrimaryTree::RelFill(mv, sec) => {
                if let Some(mv_idx) = find_mv(*mv, &self.mvs) {
                    iw.put_2bits(3);
                    iw.put_byte(mv_idx);
                    cell.intra = false;
                    let real_mv = self.mvs[usize::from(mv_idx)].0;
                    self.encode_sec_inter(iw, cell, sec, cenc, real_mv, refp);
                } else {
                    iw.put_2bits(2);
                    cell.intra = true;
                    self.encode_sec(iw, cell, sec, cenc);
                }
            },
        }
    }
    fn encode_sec(&mut self, iw: &mut Indeo3Writer, cell: Indeo3Cell, tree: &Indeo3SecondaryTree, cenc: &mut CellEncoder) {
        match tree {
            Indeo3SecondaryTree::HSplit(t1, t2) => {
                iw.put_2bits(0);
                let (cell1, cell2) = cell.split_h();
                self.encode_sec(iw, cell1, t1, cenc);
                self.encode_sec(iw, cell2, t2, cenc);
            },
            Indeo3SecondaryTree::VSplit(t1, t2) => {
                iw.put_2bits(1);
                let (cell1, cell2) = cell.split_v(self.stripw);
                self.encode_sec(iw, cell1, t1, cenc);
                self.encode_sec(iw, cell2, t2, cenc);
            },
            Indeo3SecondaryTree::VQNull(mode) => {
                iw.put_2bits(2);
                iw.put_2bits(*mode);
            },
            Indeo3SecondaryTree::VQData(mode) => {
                iw.put_2bits(3);
                self.encode_cell_data_intra(iw, cell, cenc, *mode);
            },
        }
    }
    fn encode_sec_inter(&mut self, iw: &mut Indeo3Writer, cell: Indeo3Cell, tree: &Indeo3SecondaryTree, cenc: &mut CellEncoder, mv: MV, refp: &Plane) {
        match tree {
            Indeo3SecondaryTree::HSplit(_t1, _t2) => {
                unimplemented!();
            },
            Indeo3SecondaryTree::VSplit(_t1, _t2) => {
                unimplemented!();
            },
            Indeo3SecondaryTree::VQNull(mode) => {
                iw.put_2bits(2);
                iw.put_2bits(*mode);
                cenc.read_mv_buffer(refp, cell, mv);
                cenc.null_mv();
                cenc.put_buffer(self);
            },
            Indeo3SecondaryTree::VQData(_mode) => {
                iw.put_2bits(3);
                self.encode_cell_data_inter(iw, cell, cenc, mv, refp);
            },
        }
    }
    fn encode_cell_data_intra(&mut self, iw: &mut Indeo3Writer, cell: Indeo3Cell, cenc: &mut CellEncoder, mode: u8) {
        cenc.read_buffer(self, cell);
        cenc.gen_diffs_intra();
        cenc.compress_intra(mode);
        cenc.put_buffer(self);
        for &b in cenc.out[..cenc.osize].iter() {
            iw.put_byte(b);
        }
    }
    fn encode_cell_data_inter(&mut self, iw: &mut Indeo3Writer, cell: Indeo3Cell, cenc: &mut CellEncoder, mv: MV, refp: &Plane) {
        cenc.read_buffer(self, cell);
        cenc.read_mv_buffer(refp, cell, mv);
        cenc.gen_diffs_inter();
        cenc.compress_inter();
        cenc.put_buffer(self);
        for &b in cenc.out[..cenc.osize].iter() {
            iw.put_byte(b);
        }
    }
}
