use nihav_core::frame::NASimpleVideoFrame;
use super::types::SliceState;
use super::dsp::*;

const ALPHA: [i16; 52] = [
      0,   0,   0,   0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,
      4,   4,   5,   6,  7,  8,  9, 10, 12, 13,  15,  17,  20,  22,  25,  28,
     32,  36,  40,  45, 50, 56, 63, 71, 80, 90, 100, 113, 127, 144, 162, 182,
    203, 226, 255, 255
];
const BETA: [i16; 52] = [
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     2,  2,  2,  3,  3,  3,  3,  4,  4,  4,  6,  6,  7,  7,  8,  8,
     9,  9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16,
    17, 17, 18, 18
];

const TC0: [[u8; 3]; 52] = [
    [ 0,  0,  0], [ 0,  0,  0], [ 0,  0,  0], [ 0,  0,  0],
    [ 0,  0,  0], [ 0,  0,  0], [ 0,  0,  0], [ 0,  0,  0],
    [ 0,  0,  0], [ 0,  0,  0], [ 0,  0,  0], [ 0,  0,  0],
    [ 0,  0,  0], [ 0,  0,  0], [ 0,  0,  0], [ 0,  0,  0],
    [ 0,  0,  0], [ 0,  0,  1], [ 0,  0,  1], [ 0,  0,  1],
    [ 0,  0,  1], [ 0,  1,  1], [ 0,  1,  1], [ 1,  1,  1],
    [ 1,  1,  1], [ 1,  1,  1], [ 1,  1,  1], [ 1,  1,  2],
    [ 1,  1,  2], [ 1,  1,  2], [ 1,  1,  2], [ 1,  2,  3],
    [ 1,  2,  3], [ 2,  2,  3], [ 2,  2,  4], [ 2,  3,  4],
    [ 2,  3,  4], [ 3,  3,  5], [ 3,  4,  6], [ 3,  4,  6],
    [ 4,  5,  7], [ 4,  5,  8], [ 4,  6,  9], [ 5,  7, 10],
    [ 6,  8, 11], [ 6,  8, 13], [ 7, 10, 14], [ 8, 11, 16],
    [ 9, 12, 18], [10, 13, 20], [11, 15, 23], [13, 17, 25]
];

fn get_lf_idx(qp0: u8, qp1: u8, off: i8) -> usize {
    (i16::from((qp0 + qp1 + 1) >> 1) + i16::from(off)).max(0).min(51) as usize
}

macro_rules! filter_edge_func {
    ($funcname: ident, $edgefilter: ident, $normfilter: ident) => {
        fn $funcname(dst: &mut [u8], off: usize, stride: usize, dmode: u8, quants: [u8; 2], alpha_off: i8, beta_off: i8) {
            let q = quants[0];
            let qleft = quants[1];
            if dmode != 0 {
                let index_a = get_lf_idx(q, qleft, alpha_off);
                let alpha = ALPHA[index_a];
                let beta = BETA[get_lf_idx(q, qleft, beta_off)];
                if dmode == 4 {
                    $edgefilter(dst, off, stride, alpha, beta);
                } else {
                    let tc0 = i16::from(TC0[index_a][(dmode - 1) as usize]);
                    $normfilter(dst, off, stride, alpha, beta, tc0);
                }
            }
        }
    }
}

filter_edge_func!(filter_edge_y_v, loop_filter_lumaedge_v, loop_filter_lumanormal_v);
filter_edge_func!(filter_edge_y_h, loop_filter_lumaedge_h, loop_filter_lumanormal_h);
filter_edge_func!(filter_edge_c_v, loop_filter_chromaedge_v, loop_filter_chromanormal_v);
filter_edge_func!(filter_edge_c_h, loop_filter_chromaedge_h, loop_filter_chromanormal_h);

pub fn loop_filter_mb(frm: &mut NASimpleVideoFrame<u8>, sstate: &SliceState, alpha_off: i8, beta_off: i8) {
    let yoff = frm.offset[0] + sstate.mb_x * 16 + sstate.mb_y * 16 * frm.stride[0];
    let uoff = frm.offset[1] + sstate.mb_x *  8 + sstate.mb_y *  8 * frm.stride[1];
    let voff = frm.offset[2] + sstate.mb_x *  8 + sstate.mb_y *  8 * frm.stride[2];
    let mb_idx = sstate.mb.xpos + sstate.mb_x;

    let lqy = sstate.mb.data[mb_idx - 1].qp_y;
    let lqu = sstate.mb.data[mb_idx - 1].qp_u;
    let lqv = sstate.mb.data[mb_idx - 1].qp_v;
    let qy = sstate.mb.data[mb_idx].qp_y;
    let qu = sstate.mb.data[mb_idx].qp_u;
    let qv = sstate.mb.data[mb_idx].qp_v;

    for (y, dmodes) in sstate.deblock.chunks(4).enumerate() {
        filter_edge_y_v(frm.data, yoff + y * 4 * frm.stride[0], frm.stride[0], dmodes[0] & 0xF, [qy, lqy], alpha_off, beta_off);
        for x in 1..4 {
            filter_edge_y_v(frm.data, yoff + x * 4 + y * 4 * frm.stride[0], frm.stride[0], dmodes[x] & 0xF, [qy, qy], alpha_off, beta_off);
        }
        filter_edge_c_v(frm.data, uoff + y * 2 * frm.stride[1], frm.stride[1], dmodes[0] & 0xF, [qu, lqu], alpha_off, beta_off);
        filter_edge_c_v(frm.data, uoff + y * 2 * frm.stride[1] + 4, frm.stride[1], dmodes[2] & 0xF, [qu, qu], alpha_off, beta_off);
        filter_edge_c_v(frm.data, voff + y * 2 * frm.stride[2], frm.stride[2], dmodes[0] & 0xF, [qv, lqv], alpha_off, beta_off);
        filter_edge_c_v(frm.data, voff + y * 2 * frm.stride[2] + 4, frm.stride[2], dmodes[2] & 0xF, [qv, qv], alpha_off, beta_off);
    }

    let tqy = sstate.mb.data[mb_idx - sstate.mb.stride].qp_y;
    let tqu = sstate.mb.data[mb_idx - sstate.mb.stride].qp_u;
    let tqv = sstate.mb.data[mb_idx - sstate.mb.stride].qp_v;

    let dmodes = &sstate.deblock;
    for x in 0..4 {
        filter_edge_y_h(frm.data, yoff + x * 4, frm.stride[0], dmodes[x] >> 4, [qy, tqy], alpha_off, beta_off);
    }
    for x in 0..4 {
        filter_edge_c_h(frm.data, uoff + x * 2, frm.stride[1], dmodes[x] >> 4, [qu, tqu], alpha_off, beta_off);
        filter_edge_c_h(frm.data, voff + x * 2, frm.stride[2], dmodes[x] >> 4, [qv, tqv], alpha_off, beta_off);
    }

    for (y, dmodes) in sstate.deblock.chunks(4).enumerate().skip(1) {
        for x in 0..4 {
            filter_edge_y_h(frm.data, yoff + x * 4 + y * 4 * frm.stride[0], frm.stride[0], dmodes[x] >> 4, [qy, qy], alpha_off, beta_off);
        }
    }

    let dmodes = &sstate.deblock[4 * 2..];
    for x in 0..4 {
        filter_edge_c_h(frm.data, uoff + x * 2 + frm.stride[1] * 4, frm.stride[1], dmodes[x] >> 4, [qu, qu], alpha_off, beta_off);
        filter_edge_c_h(frm.data, voff + x * 2 + frm.stride[2] * 4, frm.stride[2], dmodes[x] >> 4, [qv, qv], alpha_off, beta_off);
    }
}
