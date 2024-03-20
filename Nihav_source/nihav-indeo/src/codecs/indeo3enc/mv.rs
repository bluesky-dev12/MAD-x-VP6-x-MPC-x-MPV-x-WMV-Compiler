use super::{Indeo3Cell, Plane};

const MV_THRESHOLD: u16 = 64;
const FLAT_THRESHOLD: u16 = 8;

const DIA_LARGE: [(i8, i8); 5] = [(0, 0), (2, 0), (0, 2), (-2, 0), (0, -2)];
const DIA_SMALL: [(i8, i8); 5] = [(0, 0), (1, 0), (0, 1), (-1, 0), (0, -1)];
const SEARCH_RANGE: i8 = 16;

#[derive(Clone, Copy, PartialEq)]
pub struct MV {
    pub x: i8,
    pub y: i8
}

pub struct MotionEstimator {
    pub mv_range:   i8,
    pub flat_thr:   u16,
    pub mv_thr:     u16,
}

impl MotionEstimator {
    pub fn new() -> Self {
        Self {
            mv_range:       SEARCH_RANGE,
            flat_thr:       FLAT_THRESHOLD,
            mv_thr:         MV_THRESHOLD,
        }
    }
    pub fn mv_search(&self, cur: &Plane, prev: &Plane, cell: Indeo3Cell) -> Option<(MV, bool)> {
        let plane_w = prev.width  as isize;
        let plane_h = prev.height as isize;
        let cell_w = cell.get_width()  as isize;
        let cell_h = cell.get_height() as isize;
        let start_x = cell.get_x() as isize;
        let start_y = cell.get_y() as isize;

        let check_mv = |mv: MV| {
                if mv.x.abs() < SEARCH_RANGE && mv.y.abs() < SEARCH_RANGE {
                    let new_x = start_x + isize::from(mv.x);
                    let new_y = start_y + isize::from(mv.y);
                    new_x >= 0 && new_x + cell_w <= plane_w && new_y >= 0 && new_y + cell_h <= plane_h
                } else {
                    false
                }
            };

        let area = (cell.get_width() * cell.get_height()) as u32;
        let flat_thr = u32::from(self.flat_thr) * area;

        let mut best_mv = MV{ x: 0, y: 0 };
        let mut best_score = calc_mv_score(cur, prev, cell, best_mv);

        if best_score < flat_thr {
            return Some((best_mv, true));
        }

        let mut found_better = true;
        while found_better {
            found_better = false;
            for step in DIA_LARGE.iter() {
                let new_mv = MV{ x: best_mv.x + step.0, y: best_mv.y + step.1 };
                if !check_mv(new_mv) {
                    continue;
                }
                let score = calc_mv_score(cur, prev, cell, new_mv);
                if score < best_score {
                    best_mv = new_mv;
                    best_score = score;
                    found_better = true;
                    if best_score < flat_thr {
                        return Some((best_mv, true));
                    }
                }
            }
        }
        for step in DIA_SMALL.iter() {
            let new_mv = MV{ x: best_mv.x + step.0, y: best_mv.y + step.1 };
            if !check_mv(new_mv) {
                continue;
            }
            let score = calc_mv_score(cur, prev, cell, new_mv);
            if score < best_score {
                best_mv = new_mv;
                best_score = score;
                if best_score < flat_thr {
                    break;
                }
            }
        }
        let score = (best_score / area) as u16;
        if score < self.mv_thr {
            Some((best_mv, false))
        } else {
            None
        }
    }
}

fn calc_cell_diff(src1: &[u8], src2: &[u8], stride: usize, width: usize, height: usize) -> u32 {
    let mut score = 0;
    for (line1, line2) in src1.chunks(stride).zip(src2.chunks(stride)).take(height) {
        for (&a, &b) in line1.iter().zip(line2.iter()).take(width) {
            let diff = if a >= b { u32::from(a - b) } else { u32::from(b - a) };
            score += diff * diff;
        }
    }
    score
}

fn calc_mv_score(cur: &Plane, prev: &Plane, cell: Indeo3Cell, mv: MV) -> u32 {
    let xoff = (cell.get_x() as isize + isize::from(mv.x)) as usize;
    let yoff = (cell.get_y() as isize + isize::from(mv.y)) as usize;

    let cur_ptr = &cur.data[cell.get_x() + (cell.get_y() + 1) * cur.width..];
    let ref_ptr = &prev.data[xoff + (yoff + 1) * prev.width..];

    calc_cell_diff(cur_ptr, ref_ptr, cur.width, cell.get_width(), cell.get_height())
}

pub fn compact_mvs(mvs: &mut Vec<(MV, u16)>) {
    mvs.sort_by(|a, b| b.1.cmp(&a.1));
    mvs.truncate(256);
}

pub fn find_mv(mv: MV, mvs: &[(MV, u16)]) -> Option<u8> {
    for (i, &(cand_mv, _)) in mvs.iter().enumerate() {
        if cand_mv == mv {
            return Some(i as u8);
        }
    }
    None
}
