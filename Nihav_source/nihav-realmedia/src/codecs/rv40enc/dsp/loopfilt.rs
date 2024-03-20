use nihav_core::frame::NASimpleVideoFrame;
use super::super::types::DeblockInfo;
use super::clip8;

const Y_TOP_ROW_MASK:   u32 = 0x000F;
const Y_BOT_ROW_MASK:   u32 = 0xF000;
const Y_LEFT_COL_MASK:  u32 = 0x1111;
const Y_RIGHT_COL_MASK: u32 = 0x8888;
const C_TOP_ROW_MASK:   u8 = 0x3;
const C_BOT_ROW_MASK:   u8 = 0xC;
const C_LEFT_COL_MASK:  u8 = 0x5;
const C_RIGHT_COL_MASK: u8 = 0xA;

macro_rules! test_bit {
    ($pat: expr, $x: expr) => ( (($pat >> $x) & 1) != 0 )
}

pub fn loop_filter_frame(dst: &mut NASimpleVideoFrame<u8>, dblk: &[DeblockInfo], mb_w: usize, mb_h: usize) {
    let small_frame = dst.width[0] * dst.height[0] <= 176 * 144;

    let mut mb_pos = 0;
    for mb_y in 0..mb_h {
        let is_last_row = mb_y == mb_h - 1;
        let mut left_q: usize = 0;
        let mut left_cbp_y = 0;
        let mut left_cbp_c = 0;
        let mut left_dbk_y = 0;

        for mb_x in 0..mb_w {
            let q = usize::from(dblk[mb_pos].q);
            let alpha = RV40_ALPHA_TAB[q];
            let beta  = RV40_BETA_TAB[q];
            let beta_y = if small_frame { beta * 4 } else { beta * 3 };
            let beta_c = beta * 3;

            let is_strong = dblk[mb_pos].is_strong;
            let top_is_strong = mb_y > 0 && dblk[mb_pos - mb_w].is_strong;
            let left_is_strong = mb_x > 0 && dblk[mb_pos - 1].is_strong;
            let bot_is_strong = !is_last_row && dblk[mb_pos + mb_w].is_strong;

            let cur_dbk_y = dblk[mb_pos].deblock_y;
            let cur_cbp_y = if is_strong { 0xFFFF } else { u32::from(dblk[mb_pos].cbp_y) };

            let (top_cbp_y, top_dbk_y) = if mb_y > 0 {
                    (if top_is_strong { 0xFFFF } else { u32::from(dblk[mb_pos - mb_w].cbp_y) }, dblk[mb_pos - mb_w].deblock_y)
                } else {
                    (0, 0)
                };
            let bot_dbk_y = if !is_last_row {
                    dblk[mb_pos + mb_w].deblock_y
                } else {
                    0
                };

            let y_to_deblock = (cur_dbk_y as u32) | ((bot_dbk_y as u32) << 16);
            let mut y_h_deblock = y_to_deblock | ((cur_cbp_y << 4) & !Y_TOP_ROW_MASK) | ((top_cbp_y & Y_BOT_ROW_MASK) >> 12);
            let mut y_v_deblock = y_to_deblock | ((cur_cbp_y << 1) & !Y_LEFT_COL_MASK) | ((left_cbp_y & Y_RIGHT_COL_MASK) >> 3);

            if mb_x == 0 {
                y_v_deblock &= !Y_LEFT_COL_MASK;
            }
            if mb_y == 0 {
                y_h_deblock &= !Y_TOP_ROW_MASK;
            }
            if is_last_row || is_strong || bot_is_strong {
                y_h_deblock &= !(Y_TOP_ROW_MASK << 16);
            }

            for y in 0..4 {
                let yoff = dst.offset[0] + mb_x * 16 + (mb_y * 16 + y * 4) * dst.stride[0];
                for x in 0..4 {
                    let bpos = x + y * 4;
                    let ver_strong = (x == 0) && (mb_x > 0) && (is_strong || left_is_strong);

                    let cur_strength: usize;
                    if is_strong {
                        cur_strength = 2;
                    } else if test_bit!(cur_dbk_y, bpos) {
                        cur_strength = 1;
                    } else {
                        cur_strength = 0;
                    }

                    let left_strength: usize;
                    if x > 0 {
                        if is_strong {
                            left_strength = 2;
                        } else if test_bit!(cur_dbk_y, bpos - 1) {
                            left_strength = 1;
                        } else {
                            left_strength = 0;
                        }
                    } else if mb_x > 0 {
                        if left_is_strong {
                            left_strength = 2;
                        } else if test_bit!(left_dbk_y, bpos + 3) {
                            left_strength = 1;
                        } else {
                            left_strength = 0;
                        }
                    } else {
                        left_strength = 0;
                    }

                    let bot_strength: usize;
                    if y < 3 {
                        if is_strong {
                            bot_strength = 2;
                        } else if test_bit!(cur_dbk_y, bpos + 4) {
                            bot_strength = 1;
                        } else {
                            bot_strength = 0;
                        }
                    } else if !is_last_row {
                        if dblk[mb_pos + mb_w].is_strong {
                            bot_strength = 2;
                        } else if test_bit!(bot_dbk_y, x) {
                            bot_strength = 1;
                        } else {
                            bot_strength = 0;
                        }
                    } else {
                        bot_strength = 0;
                    }

                    let top_strength: usize;
                    if y > 0 {
                        if is_strong {
                            top_strength = 2;
                        } else if test_bit!(cur_dbk_y, bpos - 4) {
                            top_strength = 1;
                        } else {
                            top_strength = 0;
                        }
                    } else if mb_y > 0 {
                        if top_is_strong {
                            top_strength = 2;
                        } else if test_bit!(top_dbk_y, bpos + 12) {
                            top_strength = 1;
                        } else {
                            top_strength = 0;
                        }
                    } else {
                        top_strength = 0;
                    }

                    let l_q = if x > 0 { q } else { left_q };
                    let top_q = if mb_y > 0 { usize::from(dblk[mb_pos - mb_w].q) } else { 0 };

                    let lim_cur     = RV40_FILTER_CLIP_TBL [cur_strength][q];
                    let lim_top     = RV40_FILTER_CLIP_TBL [top_strength][top_q];
                    let lim_left    = RV40_FILTER_CLIP_TBL[left_strength][l_q];
                    let lim_bottom  = RV40_FILTER_CLIP_TBL [bot_strength][q];

                    let dmode = if y > 0 { x + y * 4 } else { x * 4 };

                    if test_bit!(y_h_deblock, bpos + 4) {
                        rv40_loop_filter4_h(dst.data, yoff + 4 * dst.stride[0] + x * 4, dst.stride[0],
                                            dmode, lim_cur, lim_bottom, alpha, beta, beta_y, false, false);
                    }
                    if test_bit!(y_v_deblock, bpos) && !ver_strong {
                        rv40_loop_filter4_v(dst.data, yoff + x * 4, dst.stride[0],
                                            dmode, lim_left, lim_cur, alpha, beta, beta_y, false, false);
                    }
                    if (y == 0) && test_bit!(y_h_deblock, bpos) && (is_strong || top_is_strong) {
                        rv40_loop_filter4_h(dst.data, yoff + x * 4, dst.stride[0],
                                            dmode, lim_top, lim_cur, alpha, beta, beta_y, false, true);
                    }
                    if test_bit!(y_v_deblock, bpos) && ver_strong {
                        rv40_loop_filter4_v(dst.data, yoff + x * 4, dst.stride[0],
                                            dmode, lim_left, lim_cur, alpha, beta, beta_y, false, true);
                    }
                }
            }

            let cur_cbp_c = dblk[mb_pos].cbp_c;
            let top_cbp_c = if mb_y > 0 {
                    if top_is_strong { 0xFF } else { dblk[mb_pos - mb_w].cbp_c }
                } else {
                    0
                };
            let bot_cbp_c = if !is_last_row {
                    dblk[mb_pos + mb_w].cbp_c
                } else {
                    0
                };
            for comp in 1..3 {
                let cshift = (comp - 1) * 4;
                let c_cur_cbp  = (cur_cbp_c  >> cshift) & 0xF;
                let c_top_cbp  = (top_cbp_c  >> cshift) & 0xF;
                let c_left_cbp = (left_cbp_c >> cshift) & 0xF;
                let c_bot_cbp  = (bot_cbp_c  >> cshift) & 0xF;

                let c_deblock = c_cur_cbp | (c_bot_cbp << 4);
                let mut c_v_deblock = c_deblock | ((c_cur_cbp << 1) & !C_LEFT_COL_MASK) | ((c_left_cbp & C_RIGHT_COL_MASK) >> 1);
                let mut c_h_deblock = c_deblock | ((c_cur_cbp & C_TOP_ROW_MASK) << 2) | ((c_top_cbp & C_BOT_ROW_MASK) >> 2);
                if mb_x == 0 {
                    c_v_deblock &= !C_LEFT_COL_MASK;
                }
                if mb_y == 0 {
                    c_h_deblock &= !C_TOP_ROW_MASK;
                }
                if is_last_row || is_strong || bot_is_strong {
                    c_h_deblock &= !(C_TOP_ROW_MASK << 4);
                }

                for y in 0..2 {
                    let coff = dst.offset[comp] + mb_x * 8 + (mb_y * 8 + y * 4) * dst.stride[comp];
                    for x in 0..2 {
                        let bpos = x + y * 2;

                        let ver_strong = (x == 0) && (is_strong || left_is_strong);

                        let cur_strength: usize;
                        if is_strong {
                            cur_strength = 2;
                        } else if test_bit!(c_cur_cbp, bpos) {
                            cur_strength = 1;
                        } else {
                            cur_strength = 0;
                        }

                        let left_strength: usize;
                        if x > 0 {
                            if is_strong {
                                left_strength = 2;
                            } else if test_bit!(c_cur_cbp, bpos - 1) {
                                left_strength = 1;
                            } else {
                                left_strength = 0;
                            }
                        } else if mb_x > 0 {
                            if left_is_strong {
                                left_strength = 2;
                            } else if test_bit!(c_left_cbp, bpos + 1) {
                                left_strength = 1;
                            } else {
                                left_strength = 0;
                            }
                        } else {
                            left_strength = 0;
                        }

                        let bot_strength: usize;
                        if y != 3 {
                            if is_strong {
                                bot_strength = 2;
                            } else if test_bit!(c_cur_cbp, bpos + 2) {
                                bot_strength = 1;
                            } else {
                                bot_strength = 0;
                            }
                        } else if !is_last_row {
                            if dblk[mb_pos + mb_w].is_strong {
                                bot_strength = 2;
                            } else if test_bit!(c_bot_cbp, x) {
                                bot_strength = 1;
                            } else {
                                bot_strength = 0;
                            }
                        } else {
                            bot_strength = 0;
                        }

                        let top_strength: usize;
                        if y > 0 {
                            if is_strong {
                                top_strength = 2;
                            } else if test_bit!(c_cur_cbp, bpos - 2) {
                                top_strength = 1;
                            } else {
                                top_strength = 0;
                            }
                        } else if mb_y > 0 {
                            if top_is_strong {
                                top_strength = 2;
                            } else if test_bit!(c_top_cbp, bpos + 2) {
                                top_strength = 1;
                            } else {
                                top_strength = 0;
                            }
                        } else {
                            top_strength = 0;
                        }

                        let l_q = if x > 0 { q } else { left_q };
                        let top_q = if mb_y > 0 { usize::from(dblk[mb_pos - mb_w].q) } else { 0 };

                        let lim_cur     = RV40_FILTER_CLIP_TBL [cur_strength][q];
                        let lim_top     = RV40_FILTER_CLIP_TBL [top_strength][top_q];
                        let lim_left    = RV40_FILTER_CLIP_TBL[left_strength][l_q];
                        let lim_bottom  = RV40_FILTER_CLIP_TBL [bot_strength][q];

                        if test_bit!(c_h_deblock, bpos + 2) {
                            rv40_loop_filter4_h(dst.data, coff + 4 * dst.stride[comp] + x * 4, dst.stride[comp],
                                                x * 8, lim_cur, lim_bottom, alpha, beta, beta_c, true, false);
                        }
                        if test_bit!(c_v_deblock, bpos) && !ver_strong {
                            rv40_loop_filter4_v(dst.data, coff + x * 4, dst.stride[comp],
                                                y * 8, lim_left, lim_cur, alpha, beta, beta_c, true, false);
                        }
                        if (y == 0) && test_bit!(c_h_deblock, bpos) && (is_strong || top_is_strong) {
                            rv40_loop_filter4_h(dst.data, coff + x * 4, dst.stride[comp],
                                                x * 8, lim_top, lim_cur, alpha, beta, beta_c, true, true);
                        }
                        if test_bit!(c_v_deblock, bpos) && ver_strong {
                            rv40_loop_filter4_v(dst.data, coff + x * 4, dst.stride[comp],
                                                y * 8, lim_left, lim_cur, alpha, beta, beta_c, true, true);
                        }
                    }
                }
            }

            left_q = q;
            left_dbk_y = cur_dbk_y;
            left_cbp_y = cur_cbp_y;
            left_cbp_c = cur_cbp_c;

            mb_pos += 1;
        }
    }
}

macro_rules! el {
    ($src: ident, $o: expr) => ($src[$o] as i16);
}

fn clip_symm(a: i16, lim: i16) -> i16 {
    if a < -lim {
        -lim
    } else if a > lim {
        lim
    } else {
        a
    }
}

fn rv40_weak_loop_filter4(pix: &mut [u8], mut off: usize, step: usize, stride: usize,
                          filter_p1: bool, filter_q1: bool, alpha: i16, beta: i16,
                          lim_p0q0: i16, lim_p1: i16, lim_q1: i16) {
    for _ in 0..4 {
        let p0 = el!(pix, off -   step);
        let q0 = el!(pix, off);

        let t = q0 - p0;
        if t == 0 {
            off += stride;
            continue;
        }

        let u = (alpha * t.wrapping_abs()) >> 7;
        if u > (if filter_p1 && filter_q1 { 2 } else { 3 }) {
            off += stride;
            continue;
        }

        let p2 = el!(pix, off - 3*step);
        let p1 = el!(pix, off - 2*step);
        let q1 = el!(pix, off +   step);
        let q2 = el!(pix, off + 2*step);

        let strength;
        if filter_p1 && filter_q1 {
            strength = (t << 2) + (p1 - q1);
        } else {
            strength = t << 2;
        }

        let diff = clip_symm((strength + 4) >> 3, lim_p0q0);
        pix[off - step] = clip8(p0 + diff);
        pix[off       ] = clip8(q0 - diff);

        if filter_p1 && ((p1 - p2).wrapping_abs() <= beta) {
            let p1_diff = ((p1 - p0) + (p1 - p2) - diff) >> 1;
            pix[off - 2*step] = clip8(p1 - clip_symm(p1_diff, lim_p1));
        }

        if filter_q1 && ((q1 - q2).wrapping_abs() <= beta) {
            let q1_diff = ((q1 - q0) + (q1 - q2) + diff) >> 1;
            pix[off + step] = clip8(q1 - clip_symm(q1_diff, lim_q1));
        }

        off += stride;
    }
}

fn rv40_weak_loop_filter4_h(pix: &mut [u8], off: usize, stride: usize,
                            filter_p1: bool, filter_q1: bool, alpha: i16, beta: i16,
                            lim_p0q0: i16, lim_p1: i16, lim_q1: i16) {
    rv40_weak_loop_filter4(pix, off, stride, 1, filter_p1, filter_q1, alpha, beta, lim_p0q0, lim_p1, lim_q1);
}
#[allow(clippy::eq_op)]
fn rv40_weak_loop_filter4_v(pix: &mut [u8], off: usize, stride: usize,
                            filter_p1: bool, filter_q1: bool, alpha: i16, beta: i16,
                            lim_p0q0: i16, lim_p1: i16, lim_q1: i16) {
    let src = &mut pix[off - 3..][..stride * 3 + 3 + 3];
    for ch in src.chunks_mut(stride).take(4) {
        assert!(ch.len() >= 3 + 3);
        let p0 = el!(ch, 3 - 1);
        let q0 = el!(ch, 3);

        let t = q0 - p0;
        if t == 0 {
            continue;
        }

        let u = (alpha * t.wrapping_abs()) >> 7;
        if u > (if filter_p1 && filter_q1 { 2 } else { 3 }) {
            continue;
        }

        let p2 = el!(ch, 3 - 3);
        let p1 = el!(ch, 3 - 2);
        let q1 = el!(ch, 3 + 1);
        let q2 = el!(ch, 3 + 2);

        let strength;
        if filter_p1 && filter_q1 {
            strength = (t << 2) + (p1 - q1);
        } else {
            strength = t << 2;
        }

        let diff = clip_symm((strength + 4) >> 3, lim_p0q0);
        ch[3 - 1] = clip8(p0 + diff);
        ch[3    ] = clip8(q0 - diff);

        if filter_p1 && ((p1 - p2).wrapping_abs() <= beta) {
            let p1_diff = ((p1 - p0) + (p1 - p2) - diff) >> 1;
            ch[3 - 2] = clip8(p1 - clip_symm(p1_diff, lim_p1));
        }

        if filter_q1 && ((q1 - q2).wrapping_abs() <= beta) {
            let q1_diff = ((q1 - q0) + (q1 - q2) + diff) >> 1;
            ch[3 + 1] = clip8(q1 - clip_symm(q1_diff, lim_q1));
        }
    }
}

#[allow(clippy::many_single_char_names)]
fn sfilter(a: i16, b: i16, c: i16, d: i16, e: i16, dither: i16, clip: bool, lims: i16) -> i16 {
    let val = (25 * (a + e) + 26 * (b + c + d) + dither) >> 7;
    if clip {
        if val < c - lims {
            c - lims
        } else if val > c + lims {
            c + lims
        } else {
            val
        }
    } else {
        val
    }
}

fn rv40_strong_loop_filter4(pix: &mut [u8], mut off: usize, step: usize, stride: usize,
                            alpha: i16, lims: i16, dmode: usize, chroma: bool) {
    for i in 0..4 {
        let p0 = el!(pix, off -   step);
        let q0 = el!(pix, off);

        let t = q0 - p0;
        if t == 0 {
            off += stride;
            continue;
        }

        let fmode = (alpha * t.wrapping_abs()) >> 7;
        if fmode > 1 {
            off += stride;
            continue;
        }

        let p3 = el!(pix, off - 4*step);
        let p2 = el!(pix, off - 3*step);
        let p1 = el!(pix, off - 2*step);
        let q1 = el!(pix, off +   step);
        let q2 = el!(pix, off + 2*step);
        let q3 = el!(pix, off + 3*step);

        let np0 = sfilter(p2, p1, p0, q0, q1,     RV40_DITHER_L[dmode + i], fmode != 0, lims);
        let nq0 = sfilter(    p1, p0, q0, q1, q2, RV40_DITHER_R[dmode + i], fmode != 0, lims);

        let np1 = sfilter(p3, p2, p1, np0, q0,              RV40_DITHER_L[dmode + i], fmode != 0, lims);
        let nq1 = sfilter(             p0, nq0, q1, q2, q3, RV40_DITHER_R[dmode + i], fmode != 0, lims);

        pix[off - 2*step] = np1 as u8;
        pix[off -   step] = np0 as u8;
        pix[off]          = nq0 as u8;
        pix[off +   step] = nq1 as u8;

        if !chroma {
            let np2 = sfilter(np0, np1, p2, p3, p2, 64, false, 0);
            let nq2 = sfilter(nq0, nq1, q2, q3, q2, 64, false, 0);
            pix[off - 3*step] = np2 as u8;
            pix[off + 2*step] = nq2 as u8;
        }

        off += stride;
    }
}

fn rv40_loop_strength(pix: &[u8], off: usize, step: usize, stride: usize,
                      beta: i16, beta2: i16, edge: bool) -> (bool, bool, bool) {
    let mut sum_p1p0 = 0;
    let mut sum_q1q0 = 0;

    let mut off1 = off;
    for _ in 0..4 {
        sum_p1p0 += el!(pix, off1 - 2 * step) - el!(pix, off1 - step);
        sum_q1q0 += el!(pix, off1 +     step) - el!(pix, off1);
        off1 += stride;
    }

    let filter_p1 = sum_p1p0.wrapping_abs() < beta * 4;
    let filter_q1 = sum_q1q0.wrapping_abs() < beta * 4;

    if (!filter_p1 || !filter_q1) || !edge {
        return (false, filter_p1, filter_q1);
    }

    let mut sum_p1p2 = 0;
    let mut sum_q1q2 = 0;

    let mut off1 = off;
    for _ in 0..4 {
        sum_p1p2 += el!(pix, off1 - 2 * step) - el!(pix, off1 - 3 * step);
        sum_q1q2 += el!(pix, off1 +     step) - el!(pix, off1 + 2 * step);
        off1 += stride;
    }

    let strong = (sum_p1p2.wrapping_abs() < beta2) && (sum_q1q2.wrapping_abs() < beta2);

    (strong, filter_p1, filter_q1)
}

fn rv40_loop_strength_h(pix: &[u8], off: usize, stride: usize,
                        beta: i16, beta2: i16, edge: bool) -> (bool, bool, bool) {
    rv40_loop_strength(pix, off, stride, 1, beta, beta2, edge)
}

#[allow(clippy::eq_op)]
fn rv40_loop_strength_v(pix: &[u8], off: usize, stride: usize,
                        beta: i16, beta2: i16, edge: bool) -> (bool, bool, bool) {
    let src = &pix[off - 3..][..stride * 3 + 3 + 3];
    let mut sum_p1p0 = 0;
    let mut sum_q1q0 = 0;

    for ch in src.chunks(stride).take(4) {
        assert!(ch.len() >= 3 + 3);
        sum_p1p0 += el!(ch, 3 - 2) - el!(ch, 3 - 1);
        sum_q1q0 += el!(ch, 3 + 1) - el!(ch, 3);
    }

    let filter_p1 = sum_p1p0.wrapping_abs() < beta * 4;
    let filter_q1 = sum_q1q0.wrapping_abs() < beta * 4;

    if (!filter_p1 || !filter_q1) || !edge {
        return (false, filter_p1, filter_q1);
    }

    let mut sum_p1p2 = 0;
    let mut sum_q1q2 = 0;

    for ch in src.chunks(stride).take(4) {
        assert!(ch.len() >= 3 + 3);
        sum_p1p2 += el!(ch, 3 - 2) - el!(ch, 3 - 3);
        sum_q1q2 += el!(ch, 3 + 1) - el!(ch, 3 + 2);
    }

    let strong = (sum_p1p2.wrapping_abs() < beta2) && (sum_q1q2.wrapping_abs() < beta2);

    (strong, filter_p1, filter_q1)
}

fn rv40_loop_filter4_h(pix: &mut [u8], off: usize, stride: usize,
                     dmode: usize, lim_p1: i16, lim_q1: i16, alpha: i16, beta: i16, beta2: i16,
                     chroma: bool, edge: bool) {
    let (strong, filter_p1, filter_q1) = rv40_loop_strength_h(pix, off, stride, beta, beta2, edge);
    let lims = (filter_p1 as i16) + (filter_q1 as i16) + ((lim_p1 + lim_q1) >> 1) + 1;

    if strong {
        rv40_strong_loop_filter4(pix, off, stride, 1, alpha, lims, dmode, chroma);
    } else if filter_p1 && filter_q1 {
        rv40_weak_loop_filter4_h(pix, off, stride, true, true, alpha, beta,
                                 lims, lim_p1, lim_q1);
    } else if filter_p1 || filter_q1 {
        rv40_weak_loop_filter4_h(pix, off, stride, filter_p1, filter_q1, alpha, beta,
                                 lims >> 1, lim_p1 >> 1, lim_q1 >> 1);
    }
}

fn rv40_loop_filter4_v(pix: &mut [u8], off: usize, stride: usize,
                     dmode: usize, lim_p1: i16, lim_q1: i16, alpha: i16, beta: i16, beta2: i16,
                     chroma: bool, edge: bool) {
    let (strong, filter_p1, filter_q1) = rv40_loop_strength_v(pix, off, stride, beta, beta2, edge);
    let lims = (filter_p1 as i16) + (filter_q1 as i16) + ((lim_p1 + lim_q1) >> 1) + 1;

    if strong {
        rv40_strong_loop_filter4(pix, off, 1, stride, alpha, lims, dmode, chroma);
    } else if filter_p1 && filter_q1 {
        rv40_weak_loop_filter4_v(pix, off, stride, true, true, alpha, beta,
                                 lims, lim_p1, lim_q1);
    } else if filter_p1 || filter_q1 {
        rv40_weak_loop_filter4_v(pix, off, stride, filter_p1, filter_q1, alpha, beta,
                                 lims >> 1, lim_p1 >> 1, lim_q1 >> 1);
    }
}

const RV40_DITHER_L: [i16; 16] = [
    0x40, 0x50, 0x20, 0x60, 0x30, 0x50, 0x40, 0x30,
    0x50, 0x40, 0x50, 0x30, 0x60, 0x20, 0x50, 0x40
];
const RV40_DITHER_R: [i16; 16] = [
    0x40, 0x30, 0x60, 0x20, 0x50, 0x30, 0x30, 0x40,
    0x40, 0x40, 0x50, 0x30, 0x20, 0x60, 0x30, 0x40
];

const RV40_ALPHA_TAB: [i16; 32] = [
    128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 122,  96,  75,  59,  47,  37,
     29,  23,  18,  15,  13,  11,  10,   9,
      8,   7,   6,   5,   4,   3,   2,   1
];

const RV40_BETA_TAB: [i16; 32] = [
     0,  0,  0,  0,  0,  0,  0,  0,  3,  3,  3,  4,  4,  4,  6,  6,
     6,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 13, 14, 15, 16, 17
];

const RV40_FILTER_CLIP_TBL: [[i16; 32]; 3] = [
  [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ], [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 5, 5
  ], [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
    1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 7, 8, 9
  ]
];

