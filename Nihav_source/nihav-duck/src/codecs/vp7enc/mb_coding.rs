use nihav_codec_support::codecs::{MV, ZERO_MV};
use super::super::vp78::{PredMode, MVSplitMode, SubMVRef};
use super::super::vp78dsp::*;
use super::blocks::*;
use super::coder::*;
use super::models::*;
use super::motion_est::*;
use super::rdo::*;

pub struct IntraModePredCtx<'a> {
    pub metric:     &'a RateDistMetric,
    pub models:     &'a VP7Models,
    pub q:          usize,
    pub tr:         [u8; 4],
    pub ipred_y:    IPredContext,
    pub ipred_u:    IPredContext,
    pub ipred_v:    IPredContext,
    pub pctx:       BlockPCtx,
}

struct UniqueList<A> {
    list:       [A; 4],
    fill:       usize,
}

impl<A:Copy+Default+PartialEq> UniqueList<A> {
    fn new() -> Self {
        Self { list: [A::default(); 4], fill: 0 }
    }
    fn add(&mut self, cand: A) {
        if self.fill == self.list.len() { return; }
        let mut unique = true;
        for el in self.list.iter().take(self.fill) {
            if *el == cand {
                unique = false;
                break;
            }
        }
        if unique {
            self.list[self.fill] = cand;
            self.fill += 1;
        }
    }
    fn get_list(&self) -> &[A] { &self.list[..self.fill] }
}

pub fn try_i4x4_pred(mut src: LumaIterator, modes: &mut [PredMode; 16], res: &mut Residue, new: &mut [u8; 256], pctx: &IntraModePredCtx, ref_best_dist: u32) -> u32 {
    const PRED4X4: [PredMode; 10] = [
        PredMode::DCPred, PredMode::HPred,  PredMode::VPred,  PredMode::TMPred,
        PredMode::LDPred, PredMode::RDPred, PredMode::VRPred, PredMode::VLPred,
        PredMode::HUPred, PredMode::HDPred
    ];

    let mut ipred4 = IPredContext::default();
    let mut top = [0x80; 21];
    let mut diff = [0i16; 16];
    let mut yblk = [0u8; 16];
    top[0] = pctx.ipred_y.tl;
    top[1..][..16].copy_from_slice(&pctx.ipred_y.top);
    top[17..].copy_from_slice(&pctx.tr);

    let mut tot_dist = 0;
    let mut nz_top  = pctx.pctx.nz_y_top;
    let mut nz_left = pctx.pctx.nz_y_left;
    for y in 0..4 {
        let (l1, l2) = ipred4.left.split_at_mut(16 - y * 4);
        l1.copy_from_slice(&pctx.ipred_y.left[y * 4..]);
        for el in l2.iter_mut() { *el = 0x80; }

        ipred4.tl = if y == 0 { top[0] } else { pctx.ipred_y.left[y * 4 - 1] };
        for x in 0..4 {
            let tsrc = &top[x * 4 + 1..];
            let (t1, t2) = ipred4.top.split_at_mut(tsrc.len().min(16));
            for (dst, &src) in t1.iter_mut().zip(tsrc.iter()) { *dst = src; }
            for el in t2.iter_mut() { *el = 0x80; }

            let mut best_mode = PredMode::DCPred;
            let mut best_dist = MAX_DIST;
            let mut best_has_nz = false;

            let srcblk = src.next().unwrap();
            for &mode in PRED4X4.iter() {
                yblk.ipred4(4, mode, &ipred4);
                let mode_nits = b_mode_nits(mode);
                let blkctx = (nz_top[x] as u8) + (nz_left[y] as u8);
                let (dist1, has_nz) = pctx.metric.block_dist(&srcblk, &yblk, pctx.q, 3, blkctx, &pctx.models.coef_probs[3]);
                let dist = dist1 + pctx.metric.calc_metric(0, mode_nits);
                if dist < best_dist {
                    best_mode = mode;
                    best_dist = dist;
                    best_has_nz = has_nz;
                    if dist <= SMALL_DIST {
                        break;
                    }
                }
            }
            nz_top[x]  = best_has_nz;
            nz_left[y] = best_has_nz;
            modes[x + y * 4] = best_mode;
            tot_dist += best_dist;
            if tot_dist >= ref_best_dist {
                return MAX_DIST;
            }

            yblk.ipred4(4, modes[x + y * 4], &ipred4);
            get_block_difference(&mut diff, &srcblk, &yblk);
            res.luma[x + y * 4] = diff;
            diff.fdct();
            diff.requant_y(pctx.q);
            diff.idct();

            let nblk = &mut new[x * 4 + y * 4 * 16..];
            for (dst, (src, res)) in nblk.chunks_mut(16).zip(yblk.chunks(4).zip(diff.chunks(4))) {
                for (del, (&sel, &rel)) in dst.iter_mut().zip(src.iter().zip(res.iter())) {
                    *del = (i16::from(sel) + rel).max(0).min(255) as u8;
                }
            }

            ipred4.tl = top[x * 4 + 4];
            top[x * 4 + 1..][..4].copy_from_slice(&nblk[16 * 3..][..4]);
            for (dst, src) in ipred4.left[..4].iter_mut().zip(nblk.chunks(16)) {
                *dst = src[3];
            }
        }
    }
    tot_dist
}

fn try_intra16_pred(sblk: &SrcBlock, newblk: &mut SrcBlock, res: &mut Residue, imctx: &IntraModePredCtx, ymode: PredMode) -> u32 {
    newblk.fill_ipred_luma(ymode, &imctx.ipred_y);

    for (dst, (src1, src2)) in res.luma.iter_mut().zip(sblk.luma_blocks().zip(newblk.luma_blocks())) {
        get_block_difference(dst, &src1, &src2);
    }

    let mut nits = 0;

    res.fdct_luma();
    res.fdct_dc_block();
    res.quant_luma(imctx.q);
    nits += estimate_subblock_nits(&res.dcs, 1, imctx.pctx.nz_y2, &imctx.models.coef_probs[1]);
    let mut nz_top  = imctx.pctx.nz_y_top;
    let mut nz_left = imctx.pctx.nz_y_left;
    for (y, row) in res.luma.chunks(4).enumerate() {
        for (x, blk) in row.iter().enumerate() {
            let has_nz = blk.has_nz();
            let pctx = (nz_top[x] as u8) + (nz_left[y] as u8);
            nits += estimate_subblock_nits(blk, 0, pctx, &imctx.models.coef_probs[0]);
            nz_top[x] = has_nz;
            nz_left[y] = has_nz;
        }
    }
    res.dequant_luma();
    res.idct_luma();

    let mut dist = 0;
    for (diff, (src1, src2)) in res.luma.iter().zip(sblk.luma_blocks().zip(newblk.luma_blocks())) {
        dist += get_difference_dist(&src1, &src2, diff);
    }

    imctx.metric.calc_metric(dist, nits)
}

pub fn select_intra_mode(sblk: &SrcBlock, newblk: &mut SrcBlock, res: &mut Residue, imctx: &IntraModePredCtx, ref_best_dist: u32, mb_type: MBType) -> MBType {
    const PRED16X16: [PredMode; 4] = [PredMode::DCPred, PredMode::HPred, PredMode::VPred, PredMode::TMPred];

    let mut best_ymode = PredMode::DCPred;
    let mut y_best_dist = MAX_DIST;
    let mut use_i4 = false;
    let mut i4_modes = [PredMode::DCPred; 16];
    if !sblk.is_flat() {
        for &ymode in PRED16X16.iter() {
            let dist = try_intra16_pred(sblk, newblk, res, imctx, ymode);

            if dist < y_best_dist {
                best_ymode = ymode;
                y_best_dist = dist;
                if dist <= SMALL_DIST {
                    break;
                }
            }
        }

        if y_best_dist >= ref_best_dist {
            return mb_type;
        }

        if y_best_dist > SMALL_DIST {
            res.reset();
            let dist4 = try_i4x4_pred(sblk.luma_blocks(), &mut i4_modes, res, &mut newblk.luma, imctx, y_best_dist);
            use_i4 = dist4 < y_best_dist;
            y_best_dist = y_best_dist.min(dist4);
        }
    } else if ref_best_dist != MAX_DIST { // we can skip that for intra-only case
        y_best_dist = try_intra16_pred(sblk, newblk, res, imctx, PredMode::DCPred);
        if y_best_dist >= ref_best_dist {
            return mb_type;
        }
    }

    let mut best_cmode = PredMode::DCPred;
    let mut c_best_dist = MAX_DIST;
    for &cmode in PRED16X16.iter() {
        newblk.fill_ipred_chroma(cmode, &imctx.ipred_u, &imctx.ipred_v);
        let mut dist = 0;
        'csearch: for chroma in 0..2 {
            let mut nz_top  = imctx.pctx.nz_c_top[chroma];
            let mut nz_left = imctx.pctx.nz_c_left[chroma];
            for (idx, (sblk, nblk)) in sblk.chroma_blocks(chroma).zip(newblk.chroma_blocks(chroma)).enumerate() {
                let pctx = (nz_top[idx & 1] as u8) + (nz_left[idx >> 1] as u8);
                let (dist1, has_nz) = imctx.metric.block_dist(&sblk, &nblk, imctx.q, 2, pctx, &imctx.models.coef_probs[2]);
                dist += dist1;
                nz_top[idx & 1]   = has_nz;
                nz_left[idx >> 1] = has_nz;
                if dist >= c_best_dist {
                    break 'csearch;
                }
            }
        }
        if dist < c_best_dist {
            best_cmode = cmode;
            c_best_dist = dist;
        }
    }
    let tot_dist = y_best_dist.saturating_add(c_best_dist);
    if (ref_best_dist == MAX_DIST) || (tot_dist < ref_best_dist) {
        if !use_i4 {
            MBType::Intra(best_ymode, best_cmode)
        } else {
            MBType::Intra4x4(i4_modes, [0; 16], best_cmode)
        }
    } else {
        mb_type
    }
}

pub fn calc_inter_mb_dist(sblk: &SrcBlock, newblk: &SrcBlock, res: &mut Residue, imctx: &IntraModePredCtx, pdc: i16) -> u32 {
    res.set_luma_from_diff(&sblk.luma, &newblk.luma);
    res.set_chroma_from_diff(&sblk.chroma, &newblk.chroma);
    res.fdct();
    res.fdct_dc_block();
    requant_y2_dc(&mut res.dcs[0], imctx.q);
    res.dcs[0] -= pdc;
    res.quant(imctx.q);
    let mut nits = estimate_subblock_nits(&res.dcs, 1, imctx.pctx.nz_y2, &imctx.models.coef_probs[1]);
    let mut nz_top  = imctx.pctx.nz_y_top;
    let mut nz_left = imctx.pctx.nz_y_left;
    for (y, row) in res.luma.chunks(4).enumerate() {
        for (x, blk) in row.iter().enumerate() {
            let has_nz = blk.has_nz();
            let pctx = (nz_top[x] as u8) + (nz_left[y] as u8);
            nits += estimate_subblock_nits(blk, 0, pctx, &imctx.models.coef_probs[0]);
            nz_top[x] = has_nz;
            nz_left[y] = has_nz;
        }
    }
    for (c_idx, chroma) in res.chroma.iter().enumerate() {
        let mut nz_top  = imctx.pctx.nz_c_top[c_idx];
        let mut nz_left = imctx.pctx.nz_c_left[c_idx];
        for (idx, blk) in chroma.iter().enumerate() {
            let pctx = (nz_top[idx & 1] as u8) + (nz_left[idx >> 1] as u8);
            let has_nz = blk.has_nz();
            nits += estimate_subblock_nits(blk, 2, pctx, &imctx.models.coef_probs[2]);
            nz_top[idx & 1]   = has_nz;
            nz_left[idx >> 1] = has_nz;
        }
    }
    res.dequant();
    res.idct();
    let mut dist = 0;
    for (diff, (src, new)) in res.luma.iter().zip(sblk.luma_blocks().zip(newblk.luma_blocks())) {
        dist += get_difference_dist(&src, &new, diff);
    }
    for chroma in 0..2 {
        for (diff, (src, new)) in res.chroma[chroma].iter().zip(sblk.chroma_blocks(chroma).zip(newblk.chroma_blocks(chroma))) {
            dist += get_difference_dist(&src, &new, diff);
        }
    }

    res.reset();
    imctx.metric.calc_metric(dist, nits)
}

#[allow(clippy::too_many_arguments)]
pub fn try_inter_split(sblk: &SrcBlock, newblk: &mut SrcBlock, res: &mut Residue, mvprobs: [u8; 4], nearest_mv: MV, near_mv: MV, pred_mv: MV, last: bool, mb_x: usize, mb_y: usize, mv_search: &mut Box<dyn MVSearch + Send>, mv_est: &mut MVEstimator, pctx: &mut PredContext, imctx: &IntraModePredCtx, inter_dist: u32) -> Option<(MBType, u32)> {
    let mv_stride = pctx.mv_stride;
    let mut blk8 = [0; 64];
    let mut mvs8 = [ZERO_MV; 4];
    let mut split_cand = [false; 4];
    let mut mv_dist = [0; 4];

    let mb_mv = pctx.mvs[mb_x * 4 + mb_y * 4 * mv_stride];
    for (quarter, dst_mv) in mvs8.iter_mut().enumerate() {
        let xoff = mb_x * 16 + (quarter & 1) * 8;
        let yoff = mb_y * 16 + (quarter & 2) * 4;

        let off = (quarter & 1) * 8 + (quarter >> 1) * 8 * 16;
        for (src, dst) in sblk.luma[off..].chunks(16).zip(blk8.chunks_mut(8)) {
            dst.copy_from_slice(&src[..8]);
        }

        let mut mvs = UniqueList::new();
        mvs.add(ZERO_MV);
        mvs.add(mb_mv);
        let mv_idx = xoff / 4 + (yoff / 4) * mv_stride;
        if xoff > 0 {
            mvs.add(pctx.mvs[mv_idx - 1]);
        }
        if mv_idx >= mv_stride {
            mvs.add(pctx.mvs[mv_idx - mv_stride]);
        }
        mvs.add(near_mv);
        mvs.add(nearest_mv);
        let (mv, dist) = mv_search.search_blk8(mv_est, &blk8, xoff, yoff, mvs.get_list());
        *dst_mv = mv;
        split_cand[quarter] = dist > LARGE_BLK8_DIST;
        mv_dist[quarter] = dist;
    }
    if mvs8[0] == mvs8[1] && mvs8[0] == mvs8[2] && mvs8[0] == mvs8[3] {
        // single MV per MB
        return None;
    }
    let mv_idx = mb_x * 4 + mb_y * 4 * mv_stride;
    for (dst, src) in pctx.mvs[mv_idx..].chunks_mut(2 * mv_stride).zip(mvs8.chunks(2)) {
        dst[0] = src[0];
        dst[1] = src[0];
        dst[2] = src[1];
        dst[3] = src[1];
        dst[mv_stride    ] = src[0];
        dst[mv_stride + 1] = src[0];
        dst[mv_stride + 2] = src[1];
        dst[mv_stride + 3] = src[1];
    }
    recon_split_mb(newblk, mb_x, mb_y, &pctx.mvs, mv_stride, mv_est);

    let mut tot_dist = calc_inter_mb_dist(sblk, newblk, res, imctx, pctx.get_y2_dc_pred(last));

    let mut split_mode = MVSplitMode::Quarters;
    let mut sub_refs = [SubMVRef::Zero; 16];
    let mut sub_mvs = [ZERO_MV; 16];

    let mut mv_nits = 0;
    if mvs8[0] == mvs8[1] && mvs8[2] == mvs8[3] {
        split_mode = MVSplitMode::TopBottom;
        sub_mvs[0] = mvs8[0] - pred_mv;
        sub_mvs[1] = mvs8[2] - pred_mv;

        let mv_idx = mb_x * 4 + mb_y * 4 * mv_stride;
        let left_mv = if mb_x > 0 { pctx.mvs[mv_idx - 1] } else { ZERO_MV };
        let top_mv = if mb_y > 0 { pctx.mvs[mv_idx - mv_stride] } else { ZERO_MV };
        let (ref0, nits0) = sub_mv_nits(mvs8[0], left_mv, top_mv, pred_mv, imctx.models);
        let left_mv = if mb_x > 0 { pctx.mvs[mv_idx + 2 * mv_stride - 1] } else { ZERO_MV };
        let (ref1, nits1) = sub_mv_nits(mvs8[2], left_mv, mvs8[0], pred_mv, imctx.models);
        sub_refs[0] = ref0;
        sub_refs[1] = ref1;
        mv_nits += nits0 + nits1;
    } else if mvs8[0] == mvs8[2] && mvs8[1] == mvs8[3] {
        split_mode = MVSplitMode::LeftRight;
        sub_mvs[0] = mvs8[0] - pred_mv;
        sub_mvs[1] = mvs8[1] - pred_mv;

        let mv_idx = mb_x * 4 + mb_y * 4 * mv_stride;
        let left_mv = if mb_x > 0 { pctx.mvs[mv_idx - 1] } else { ZERO_MV };
        let top_mv = if mb_y > 0 { pctx.mvs[mv_idx - mv_stride] } else { ZERO_MV };
        let (ref0, nits0) = sub_mv_nits(mvs8[0], left_mv, top_mv, pred_mv, imctx.models);
        let top_mv = if mb_y > 0 { pctx.mvs[mv_idx - mv_stride + 2] } else { ZERO_MV };
        let (ref1, nits1) = sub_mv_nits(mvs8[1], mvs8[0], top_mv, pred_mv, imctx.models);
        sub_refs[0] = ref0;
        sub_refs[1] = ref1;
        mv_nits += nits0 + nits1;
    } else {
        for (quarter, &mv) in mvs8.iter().enumerate() {
            let xoff = mb_x * 16 + (quarter & 1) * 8;
            let yoff = mb_y * 16 + (quarter & 2) * 4;
            let mv_idx = xoff / 4 + (yoff / 4) * mv_stride;
            let left_mv = if xoff > 0 { pctx.mvs[mv_idx - 1] } else { ZERO_MV };
            let top_mv = if yoff > 0 { pctx.mvs[mv_idx - mv_stride] } else { ZERO_MV };
            let (cur_sub_ref, nits) = sub_mv_nits(mv, left_mv, top_mv, pred_mv, imctx.models);
            sub_refs[quarter] = cur_sub_ref;
            sub_mvs[quarter] = mv - pred_mv;
            mv_nits += nits;

            pctx.mvs[mv_idx] = mv;
            pctx.mvs[mv_idx + 1] = mv;
            pctx.mvs[mv_idx + mv_stride] = mv;
            pctx.mvs[mv_idx + mv_stride + 1] = mv;
        }
    }
    mv_nits += sub_mv_mode_nits(split_mode);
    tot_dist += imctx.metric.calc_metric(0, mv_nits);
    if tot_dist < inter_dist {
        if tot_dist > SMALL_DIST && (split_cand[0] || split_cand[1] || split_cand[2] || split_cand[3]) {
            let mut blk4 = [0; 16];
            let mut has_splits = false;
            for (quarter, &mv_dist) in mv_dist.iter().enumerate() {
                if !split_cand[quarter] {
                    continue;
                }
                let xoff = mb_x * 16 + (quarter & 1) * 8;
                let yoff = mb_y * 16 + (quarter & 2) * 4;
                let mut dist_sum = 0;
                let mut smv = [ZERO_MV; 4];
                for (subq, smv) in smv.iter_mut().enumerate() {
                    let off = (quarter & 1) * 8 + (subq & 1) * 4 + ((quarter >> 1) * 8 + (subq >> 1) * 4) * 16;
                    for (dst, src) in blk4.chunks_mut(4).zip(sblk.luma[off..].chunks(16)) {
                        dst.copy_from_slice(&src[..4]);
                    }

                    let mut mvs = UniqueList::new();
                    mvs.add(ZERO_MV);
                    mvs.add(mvs8[quarter]);
                    mvs.add(mb_mv);
                    let mv_idx = xoff / 4 + (subq & 1) + ((yoff / 4) + (subq >> 1)) * mv_stride;
                    if xoff > 0 || (subq & 1) != 0 {
                        mvs.add(pctx.mvs[mv_idx - 1]);
                    }
                    if mv_idx >= mv_stride {
                        mvs.add(pctx.mvs[mv_idx - mv_stride]);
                    }
                    let (mv, dist) = mv_search.search_blk4(mv_est, &blk4, xoff, yoff, mvs.get_list());
                    *smv = mv;
                    dist_sum += dist;
                }
                if dist_sum < mv_dist / 2 {
                    for (subq, &smv) in smv.iter().enumerate() {
                        let mv_idx = xoff / 4 + (subq & 1) + ((yoff / 4) + (subq >> 1)) * mv_stride;
                        pctx.mvs[mv_idx] = smv;
                    }
                    has_splits = true;
                }
            }
            if has_splits {
                recon_split_mb(newblk, mb_x, mb_y, &pctx.mvs, mv_stride, mv_est);
                let mut split16_dist = calc_inter_mb_dist(sblk, newblk, res, imctx, pctx.get_y2_dc_pred(last));
                if split16_dist < tot_dist {
                    let mut mv_nits = sub_mv_mode_nits(MVSplitMode::Sixteenths);
                    let mut mv_idx = mb_x * 4 + mb_y * 4 * mv_stride;
                    let mut sub_refs2 = [SubMVRef::Zero; 16];
                    let mut sub_mvs2 = [ZERO_MV; 16];
                    for y in 0..4 {
                        for x in 0..4 {
                            let left_mv = if x > 0 || mb_x > 0 { pctx.mvs[mv_idx + x - 1] } else { ZERO_MV };
                            let top_mv = if mv_idx + x >= mv_stride { pctx.mvs[mv_idx + x - mv_stride] } else { ZERO_MV };
                            let cur_mv = pctx.mvs[mv_idx + x];
                            sub_mvs2[x + y * 4] = cur_mv - pred_mv;
                            let (cur_sub_ref, nits) = sub_mv_nits(cur_mv, left_mv, top_mv, pred_mv, imctx.models);
                            sub_refs2[x + y * 4] = cur_sub_ref;
                            mv_nits += nits;
                        }
                        mv_idx += mv_stride;
                    }
                    split16_dist += imctx.metric.calc_metric(0, mv_nits);
                    if split16_dist < tot_dist {
                        let mb_t = MBType::InterSplitMV(last, mvprobs, MVSplitMode::Sixteenths, sub_refs2, sub_mvs2);
                        return Some((mb_t, split16_dist));
                    }
                }
            }
        }
        let mv_idx = mb_x * 4 + mb_y * 4 * mv_stride;
        for (dst, src) in pctx.mvs[mv_idx..].chunks_mut(2 * mv_stride).zip(mvs8.chunks(2)) {
            dst[0] = src[0];
            dst[1] = src[0];
            dst[2] = src[1];
            dst[3] = src[1];
            dst[mv_stride    ] = src[0];
            dst[mv_stride + 1] = src[0];
            dst[mv_stride + 2] = src[1];
            dst[mv_stride + 3] = src[1];
        }
        Some((MBType::InterSplitMV(last, mvprobs, split_mode, sub_refs, sub_mvs), tot_dist))
    } else {
        None
    }
}

fn get_chroma_mv(mut mv: MV) -> MV {
    if mv.x < 0 {
        mv.x += 1;
    } else {
        mv.x += 2;
    }
    if mv.y < 0 {
        mv.y += 1;
    } else {
        mv.y += 2;
    }
    mv.x >>= 2;
    mv.y >>= 2;
    mv
}

pub fn recon_split_mb(newblk: &mut SrcBlock, mb_x: usize, mb_y: usize, mvs: &[MV], mv_stride: usize, mv_est: &mut MVEstimator) {
    let mut mv_idx = mb_x * 4 + mb_y * 4 * mv_stride;
    let mut sum_mv = [ZERO_MV; 2];
    let mut blk4 = [0; 16];
    for (y, strip) in newblk.luma.chunks_mut(16 * 4).enumerate() {
        if (y & 1) == 0 {
            sum_mv = [ZERO_MV; 2];
        }
        for x in 0..4 {
            let mv = mvs[mv_idx + x];
            sum_mv[x / 2] += mv;
            mv_est.get_blk4(&mut blk4, 0, mb_x * 16 + x * 4, mb_y * 16 + y * 4, mv);
            for (dst, src) in strip[x * 4..].chunks_mut(16).zip(blk4.chunks(4)) {
                dst[..4].copy_from_slice(src);
            }
        }
        if (y & 1) == 1 {
            let cmv = [get_chroma_mv(sum_mv[0]), get_chroma_mv(sum_mv[1])];
            for chroma in 0..2 {
                for (x, &mv) in cmv.iter().enumerate() {
                    mv_est.get_blk4(&mut blk4, chroma + 1, mb_x * 8 + x * 4, mb_y * 8 + (y & 2) * 2, mv);
                    for (dst, src) in newblk.chroma[chroma][x * 4 + (y & 2) * 2 * 8..].chunks_mut(8).zip(blk4.chunks(4)) {
                        dst[..4].copy_from_slice(src);
                    }
                }
            }
        }
        mv_idx += mv_stride;
    }
}
