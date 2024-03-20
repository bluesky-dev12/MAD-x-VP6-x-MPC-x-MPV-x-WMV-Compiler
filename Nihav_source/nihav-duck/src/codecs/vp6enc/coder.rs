use nihav_core::codecs::{EncoderResult, EncoderError};
use nihav_codec_support::codecs::MV;
use crate::codecs::vpenc::coder::*;
use crate::codecs::vpenc::models::*;
use super::super::vpcommon::*;
use super::super::vp6data::*;
use super::models::*;

pub use crate::codecs::vpenc::coder::{BoolEncoder, Estimator};

pub const MODE_TREE: &[TokenSeq<VPMBType>] = &[
    bit_seq!(VPMBType::Intra;         T, F, F; 0, 2, 5),
    bit_seq!(VPMBType::InterFourMV;   T, F, T; 0, 2, 5),
    bit_seq!(VPMBType::InterNoMV;     F, F, F; 0, 1, 3),
    bit_seq!(VPMBType::InterMV;       F, F, T; 0, 1, 3),
    bit_seq!(VPMBType::InterNearest;  F, T, F; 0, 1, 4),
    bit_seq!(VPMBType::InterNear;     F, T, T; 0, 1, 4),
    bit_seq!(VPMBType::GoldenNoMV;    T, T, F, F; 0, 2, 6, 7),
    bit_seq!(VPMBType::GoldenMV;      T, T, F, T; 0, 2, 6, 7),
    bit_seq!(VPMBType::GoldenNearest; T, T, T, F; 0, 2, 6, 8),
    bit_seq!(VPMBType::GoldenNear;    T, T, T, T; 0, 2, 6, 8),
];

const MODE_TREE_DIFF: &[TokenSeq<u8>] = &[
    bit_seq!(1; F, T; 0, 1),
    bit_seq!(2; F, F; 0, 1),
    bit_seq!(3; T, F, T; 0, 2, 3),
    bit_seq!(4; T, F, F, T; 0, 2, 3, 4),
    bit_seq!(5; T, F, F, F, T; 0, 2, 3, 4, 5),
    bit_seq!(6; T, F, F, F, F; 0, 2, 3, 4, 5),
    bit_seq!(7; T, T; 0, 2),
];

const MODE_TREE_DIFF_PROBS: &[u8; 6] = &[171, 83, 199, 140, 125, 104];

const SHORT_MV_TREE: &[TokenSeq<u8>] = &[
    bit_seq!(0; F, F, F; 0, 1, 2),
    bit_seq!(1; F, F, T; 0, 1, 2),
    bit_seq!(2; F, T, F; 0, 1, 3),
    bit_seq!(3; F, T, T; 0, 1, 3),
    bit_seq!(4; T, F, F; 0, 4, 5),
    bit_seq!(5; T, F, T; 0, 4, 5),
    bit_seq!(6; T, T, F; 0, 4, 6),
    bit_seq!(7; T, T, T; 0, 4, 6),
];

const EOB: i8 = 42;

const DC_TREE: &[TokenSeq<i8>] = &[
    bit_seq!(  0; F; 0),
    bit_seq!(  1; T, F; 0, 2),
    bit_seq!(  2; T, T, F, F; 0, 2, 3, 4),
    bit_seq!(  3; T, T, F, T, F; 0, 2, 3, 4, 5),
    bit_seq!(  4; T, T, F, T, T; 0, 2, 3, 4, 5),
    bit_seq!( -1; T, T, T, F, F; 0, 2, 3, 6, 7),
    bit_seq!( -2; T, T, T, F, T; 0, 2, 3, 6, 7),
    bit_seq!( -3; T, T, T, T, F, F; 0, 2, 3, 6, 8, 9),
    bit_seq!( -4; T, T, T, T, F, T; 0, 2, 3, 6, 8, 9),
    bit_seq!( -5; T, T, T, T, T, F; 0, 2, 3, 6, 8, 10),
    bit_seq!( -6; T, T, T, T, T, T; 0, 2, 3, 6, 8, 10),
];

const NZ_COEF_TREE: &[TokenSeq<i8>] = &[
    bit_seq!(  1; F; 2),
    bit_seq!(  2; T, F, F; 2, 3, 4),
    bit_seq!(  3; T, F, T, F; 2, 3, 4, 5),
    bit_seq!(  4; T, F, T, T; 2, 3, 4, 5),
    bit_seq!( -1; T, T, F, F; 2, 3, 6, 7),
    bit_seq!( -2; T, T, F, T; 2, 3, 6, 7),
    bit_seq!( -3; T, T, T, F, F; 2, 3, 6, 8, 9),
    bit_seq!( -4; T, T, T, F, T; 2, 3, 6, 8, 9),
    bit_seq!( -5; T, T, T, T, F; 2, 3, 6, 8, 10),
    bit_seq!( -6; T, T, T, T, T; 2, 3, 6, 8, 10),
];

const COEF_TREE: &[TokenSeq<i8>] = &[
    bit_seq!(  0; F, T; 0, 1),
    bit_seq!(EOB; F, F; 0, 1),
    bit_seq!(  1; T, F; 0, 2),
    bit_seq!(  2; T, T, F, F; 0, 2, 3, 4),
    bit_seq!(  3; T, T, F, T, F; 0, 2, 3, 4, 5),
    bit_seq!(  4; T, T, F, T, T; 0, 2, 3, 4, 5),
    bit_seq!( -1; T, T, T, F, F; 0, 2, 3, 6, 7),
    bit_seq!( -2; T, T, T, F, T; 0, 2, 3, 6, 7),
    bit_seq!( -3; T, T, T, T, F, F; 0, 2, 3, 6, 8, 9),
    bit_seq!( -4; T, T, T, T, F, T; 0, 2, 3, 6, 8, 9),
    bit_seq!( -5; T, T, T, T, T, F; 0, 2, 3, 6, 8, 10),
    bit_seq!( -6; T, T, T, T, T, T; 0, 2, 3, 6, 8, 10),
];

fn coef_to_cat(coef: i16) -> i8 {
    match coef.abs() {
        0 ..=4  => coef.abs() as i8,
        5 ..=6  => -1,
        7 ..=10 => -2,
        11..=18 => -3,
        19..=34 => -4,
        35..=66 => -5,
        _       => -6,
    }
}

const ZERO_RUN_TREE: &[TokenSeq<u8>] = &[
    bit_seq!(1; F, F, F; 0, 1, 2),
    bit_seq!(2; F, F, T; 0, 1, 2),
    bit_seq!(3; F, T, F; 0, 1, 3),
    bit_seq!(4; F, T, T; 0, 1, 3),
    bit_seq!(5; T, F, F, F; 0, 4, 5, 6),
    bit_seq!(6; T, F, F, T; 0, 4, 5, 6),
    bit_seq!(7; T, F, T, F; 0, 4, 5, 7),
    bit_seq!(8; T, F, T, T; 0, 4, 5, 7),
    bit_seq!(9; T, T; 0, 4),
];

pub trait EncoderTrait {
    fn write_cat(&mut self, cat: i8, tree: &[TokenSeq<i8>], tok_probs: &[u8], val_probs: &[u8; 11]) -> EncoderResult<()>;
    fn write_large_coef(&mut self, val: i16, cat: usize) -> EncoderResult<()>;
    fn write_dc(&mut self, val: i16, tok_probs: &[u8; 5], val_probs: &[u8; 11]) -> EncoderResult<()>;
    fn write_ac(&mut self, val: i16, tree: &[TokenSeq<i8>], probs: &[u8; 11]) -> EncoderResult<()>;
    fn write_zero_run(&mut self, val: usize, probs: &[u8; 14]) -> EncoderResult<()>;
}

impl<'a, 'b> EncoderTrait for BoolEncoder<'a, 'b> {
    fn write_cat(&mut self, cat: i8, tree: &[TokenSeq<i8>], tok_probs: &[u8], val_probs: &[u8; 11]) -> EncoderResult<()> {
        for entry in tree.iter() {
            if entry.val == cat {
                for seq in entry.seq.iter() {
                    let prob = if seq.idx < 5 {
                            tok_probs[seq.idx as usize]
                        } else {
                            val_probs[seq.idx as usize]
                        };
                    self.put_bool(seq.bit, prob)?;
                }
                return Ok(());
            }
        }
        Err(EncoderError::Bug)
    }
    fn write_large_coef(&mut self, val: i16, cat: usize) -> EncoderResult<()> {
        let base = VP56_COEF_BASE[cat];
        let mut probs = VP56_COEF_ADD_PROBS[cat].iter();
        let add = val.abs() - base;
        let mut mask = 1 << (VP6_COEF_ADD_BITS[cat] - 1);
        while mask != 0 {
            self.put_bool((add & mask) != 0, *probs.next().unwrap())?;
            mask >>= 1;
        }
        self.put_bool(val < 0, 128)?;

        Ok(())
    }
    fn write_dc(&mut self, val: i16, tok_probs: &[u8; 5], val_probs: &[u8; 11]) -> EncoderResult<()> {
        let cat = coef_to_cat(val);
        self.write_cat(cat, DC_TREE, tok_probs, val_probs)?;
        if cat < 0 {
            self.write_large_coef(val, (-cat - 1) as usize)?;
        } else if val != 0 {
            self.put_bool(val < 0, 128)?;
        }
        Ok(())
    }
    fn write_ac(&mut self, val: i16, tree: &[TokenSeq<i8>], probs: &[u8; 11]) -> EncoderResult<()> {
        let cat = coef_to_cat(val);
        self.write_cat(cat, tree, probs, probs)?;
        if cat < 0 {
            self.write_large_coef(val, (-cat - 1) as usize)?;
        } else if val != 0 {
            self.put_bool(val < 0, 128)?;
        }
        Ok(())
    }
    fn write_zero_run(&mut self, val: usize, probs: &[u8; 14]) -> EncoderResult<()> {
        self.write_el(val.min(9) as u8, ZERO_RUN_TREE, probs)?;
        if val >= 9 {
            let add = val - 9;
            for i in 0..6 {
                self.put_bool(((add >> i) & 1) != 0, probs[i + 8])?;
            }
        }
        Ok(())
    }
}

fn rescale_mb_mode_prob(prob: u32, total: u32) -> u8 {
    (255 * prob / (1 + total)) as u8
}

fn calc_mb_model_probs(prob_xmitted: &[u8; 20], mbtype_models: &mut [VP56MBTypeModel; 10]) {
    for mode in 0..10 {
        let mdl = &mut mbtype_models[mode];
        let mut cnt = [0u32; 10];
        let mut total = 0;
        for i in 0..10 {
            if i == mode { continue; }
            cnt[i] = 100 * u32::from(prob_xmitted[i * 2]);
            total += cnt[i];
        }
        let sum = u32::from(prob_xmitted[mode * 2]) + u32::from(prob_xmitted[mode * 2 + 1]);
        mdl.probs[9] = 255 - rescale_mb_mode_prob(u32::from(prob_xmitted[mode * 2 + 1]), sum);

        let inter_mv0_weight = cnt[0] + cnt[2];
        let inter_mv1_weight = cnt[3] + cnt[4];
        let gold_mv0_weight = cnt[5] + cnt[6];
        let gold_mv1_weight = cnt[8] + cnt[9];
        let mix_weight = cnt[1] + cnt[7];
        mdl.probs[0] = 1 + rescale_mb_mode_prob(inter_mv0_weight + inter_mv1_weight, total);
        mdl.probs[1] = 1 + rescale_mb_mode_prob(inter_mv0_weight, inter_mv0_weight + inter_mv1_weight);
        mdl.probs[2] = 1 + rescale_mb_mode_prob(mix_weight, mix_weight + gold_mv0_weight + gold_mv1_weight);
        mdl.probs[3] = 1 + rescale_mb_mode_prob(cnt[0], inter_mv0_weight);
        mdl.probs[4] = 1 + rescale_mb_mode_prob(cnt[3], inter_mv1_weight);
        mdl.probs[5] = 1 + rescale_mb_mode_prob(cnt[1], mix_weight);
        mdl.probs[6] = 1 + rescale_mb_mode_prob(gold_mv0_weight, gold_mv0_weight + gold_mv1_weight);
        mdl.probs[7] = 1 + rescale_mb_mode_prob(cnt[5], gold_mv0_weight);
        mdl.probs[8] = 1 + rescale_mb_mode_prob(cnt[8], gold_mv1_weight);
    }
}

fn calc_mbtype_bits(prob_xmitted: &[u8; 20], stats: &[[usize; 10]; 10], mdl: &mut [VP56MBTypeModel; 10]) -> u32 {
    const MB_TYPES: [VPMBType; 10] = [
        VPMBType::InterNoMV,
        VPMBType::Intra,
        VPMBType::InterMV,
        VPMBType::InterNearest,
        VPMBType::InterNear,
        VPMBType::GoldenNoMV,
        VPMBType::GoldenMV,
        VPMBType::InterFourMV,
        VPMBType::GoldenNearest,
        VPMBType::GoldenNear
    ];

    calc_mb_model_probs(prob_xmitted, mdl);
    let mut nits = 0;
    for (last, (srow, mdl)) in stats.iter().zip(mdl.iter()).enumerate() {
        for (cur, &ccount) in srow.iter().enumerate() {
            let ccount = ccount as u32;
            nits += Estimator::est_nits(cur == last, mdl.probs[9]) * ccount;
            if cur != last {
                for entry in MODE_TREE.iter() {
                    if entry.val == MB_TYPES[cur] {
                        for seq in entry.seq.iter() {
                            nits += Estimator::est_nits(seq.bit, mdl.probs[seq.idx as usize]) * ccount;
                        }
                        break;
                    }
                }
            }
        }
    }

    Estimator::nits_to_bits(nits)
}

fn find_model_vq(prob_xmitted: &[u8; 20], vq: &[[u8; 20]; 16]) -> usize {
    let mut best_idx = 0;
    let mut best_dist = i16::MAX;

    for (idx, row) in vq.iter().enumerate() {
        let mut dist = 0;
        for i in 0..20 {
            let a = prob_xmitted[i ^ 1];
            let b = row[i];
            dist += (i16::from(a) - i16::from(b)).abs();
        }
        if dist == 0 {
            return idx;
        }
        if dist < best_dist {
            best_dist = dist;
            best_idx  = idx;
        }
    }

    best_idx
}

// todo per-delta decision, incremental updates and such
fn deltas_bits(probs: &[u8; 20], base: &[u8; 20], stats: &[[usize; 10]; 10], tmp: &mut [VP56MBTypeModel; 10], deltas: &mut [i16; 20]) -> u32 {
    const DELTA_PROBS: [u8; 8] = [
        PROB_BITS[205],
        PROB_BITS[256 - 205] + PROB_BITS[171] + PROB_BITS[256 - 83] + PROB_BITS[128],
        PROB_BITS[256 - 205] + PROB_BITS[171] + PROB_BITS[83] + PROB_BITS[128],
        PROB_BITS[256 - 205] + PROB_BITS[256 - 171] + PROB_BITS[199] + PROB_BITS[256 - 140] + PROB_BITS[128],
        PROB_BITS[256 - 205] + PROB_BITS[256 - 171] + PROB_BITS[199] + PROB_BITS[140] + PROB_BITS[256 - 125] + PROB_BITS[128],
        PROB_BITS[256 - 205] + PROB_BITS[256 - 171] + PROB_BITS[199] + PROB_BITS[140] + PROB_BITS[125] + PROB_BITS[256 - 104] + PROB_BITS[128],
        PROB_BITS[256 - 205] + PROB_BITS[256 - 171] + PROB_BITS[199] + PROB_BITS[140] + PROB_BITS[125] + PROB_BITS[104] + PROB_BITS[128],
        PROB_BITS[256 - 205] + PROB_BITS[256 - 171] + PROB_BITS[256 - 199] + 8 * PROB_BITS[128],
    ];

    let mut nits = 0;
    let mut tprobs = [0u8; 20];

    for i in 0..20 {
        let old = i16::from(base[i]);
        let new = i16::from(probs[i]);
        let mut diff = (new - old) & !3;
        if old + diff > 255 {
            diff -= 4;
        } else if old + diff < 0 || (old + diff == 0 && new != 0) {
            diff += 4;
        }
        tprobs[i] = (old + diff) as u8;
        deltas[i] = diff;
        nits += u32::from(DELTA_PROBS[(diff.abs() >> 2).min(7) as usize]);
    }

    Estimator::nits_to_bits(nits) + calc_mbtype_bits(&tprobs, stats, tmp) + 5
}

pub fn encode_mode_prob_models(bc: &mut BoolEncoder, models: &mut VP56Models, pmodels: &VP56Models, stats: &[[[usize; 10]; 10]; 3]) -> EncoderResult<()> {
    let mut tmp = [VP56MBTypeModel::default(); 10];
    let mut tprob = [0; 20];
    for ctx in 0..3 {
        let mut models_changed = models.prob_xmitted[ctx] != pmodels.prob_xmitted[ctx];
        if models_changed {
            let old_bits = calc_mbtype_bits(&pmodels.prob_xmitted[ctx], &stats[ctx], &mut tmp);
            let new_bits = calc_mbtype_bits(&models.prob_xmitted[ctx], &stats[ctx], &mut tmp) + 4;
            if new_bits < old_bits {
                let idx = find_model_vq(&models.prob_xmitted[ctx], &VP56_MODE_VQ[ctx]);
                for i in 0..20 {
                    tprob[i ^ 1] = VP56_MODE_VQ[ctx][idx][i];
                }
                let vq_bits = calc_mbtype_bits(&tprob, &stats[ctx], &mut tmp) + 4;
                if vq_bits < old_bits {
                    bc.put_bool(true, 174)?;
                    bc.put_bits(idx as u32, 4)?;
                    let mut diffs_present = tprob != models.prob_xmitted[ctx];
                    let mut deltas = [0; 20];
                    let delta_cost = deltas_bits(&models.prob_xmitted[ctx], &tprob, &stats[ctx], &mut tmp, &mut deltas);
                    if delta_cost + 1 >= new_bits {
                        diffs_present = false;
                    }
                    if diffs_present {
                        bc.put_bool(true, 254)?;
                        for i in 0..20 {
                            let diff = deltas[i ^ 1] >> 2;
                            bc.put_bool(diff != 0, 205)?;
                            if diff != 0 {
                                let d0 = diff.abs().min(7) as u8;
                                bc.put_bool(diff < 0, 128)?;
                                bc.write_el(d0, MODE_TREE_DIFF, MODE_TREE_DIFF_PROBS)?;
                                if d0 == 7 {
                                    bc.put_bits(u32::from(diff.unsigned_abs()), 7)?;
                                }
                                tprob[i ^ 1] = (i16::from(tprob[i ^ 1]) + deltas[i ^ 1]) as u8;
                            }
                        }
                    }
                    if !diffs_present {
                        bc.put_bool(false, 254)?;
                    }
                } else {
                    models_changed = false;
                }
            } else {
                models_changed = false;
            }
        }
        if !models_changed {
            bc.put_bool(false, 174)?;
            bc.put_bool(false, 254)?;
            models.prob_xmitted[ctx] = pmodels.prob_xmitted[ctx];
        } else {
            models.prob_xmitted[ctx] = tprob;
        }
    }
    for ctx in 0..3 {
        let prob_xmitted = &models.prob_xmitted[ctx];
        calc_mb_model_probs(prob_xmitted, &mut models.mbtype_models[ctx]);
    }
    Ok(())
}

pub fn encode_mv_models(bc: &mut BoolEncoder, models: &[VP56MVModel; 2], pmodels: &[VP56MVModel; 2]) -> EncoderResult<()> {
    for (i, (mdl, pmdl)) in models.iter().zip(pmodels.iter()).enumerate() {
        bc.encode_probability(mdl.nz_prob, pmdl.nz_prob, HAS_NZ_PROB[i])?;
        bc.encode_probability(mdl.sign_prob, pmdl.sign_prob, HAS_SIGN_PROB[i])?;
    }
    for (i, (mdl, pmdl)) in models.iter().zip(pmodels.iter()).enumerate() {
        for (&coded_prob, (&prob, &pprob)) in HAS_TREE_PROB[i].iter().zip(mdl.tree_probs.iter().zip(pmdl.tree_probs.iter())) {
            bc.encode_probability(prob, pprob, coded_prob)?;
        }
    }
    for (i, (mdl, pmdl)) in models.iter().zip(pmodels.iter()).enumerate() {
        for (&coded_prob, (&prob, &pprob)) in HAS_RAW_PROB[i].iter().zip(mdl.raw_probs.iter().zip(pmdl.raw_probs.iter())) {
            bc.encode_probability(prob, pprob, coded_prob)?;
        }
    }
    Ok(())
}

pub fn encode_coeff_models(bc: &mut BoolEncoder, models: &mut VP56Models, pmodels: &VP56Models, is_intra: bool, interlaced: bool) -> EncoderResult<()> {
    let mut def_prob = [128u8; 11];
    for plane in 0..2 {
        for i in 0..11 {
            let pprob = pmodels.coeff_models[plane].dc_value_probs[i];
            let prob = models.coeff_models[plane].dc_value_probs[i];
            let changed = (is_intra && prob != def_prob[i]) || (!is_intra && prob != pprob);
            bc.put_bool(changed, HAS_COEF_PROBS[plane][i])?;
            if changed {
                bc.put_probability(prob)?;
                def_prob[i] = prob;
            }
        }
    }

    bc.put_bool(false, 128)?;
    reset_scan(&mut models.vp6models, interlaced);
    /* for scan
        for i in 1..64 {
            if bc.read_prob(HAS_SCAN_UPD_PROBS[i]) {
                models.vp6models.scan_order[i]  = bc.read_bits(4) as usize;
            }
        }
        update_scan(&mut models.vp6models);
    */

    for comp in 0..2 {
        for i in 0..14 {
            bc.encode_probability(models.vp6models.zero_run_probs[comp][i], pmodels.vp6models.zero_run_probs[comp][i], HAS_ZERO_RUN_PROBS[comp][i])?;
        }
    }

    for ctype in 0..3 {
        for plane in 0..2 {
            for group in 0..6 {
                for i in 0..11 {
                    let pprob = pmodels.coeff_models[plane].ac_val_probs[ctype][group][i];
                    let prob = models.coeff_models[plane].ac_val_probs[ctype][group][i];
                    let changed = (is_intra && prob != def_prob[i]) || (!is_intra && prob != pprob);
                    bc.put_bool(changed, VP6_AC_PROBS[ctype][plane][group][i])?;
                    if changed {
                        bc.put_probability(prob)?;
                        def_prob[i] = prob;
                    }
                }
            }
        }
    }

    for plane in 0..2 {
        let mdl = &mut models.coeff_models[plane];
        for i in 0..3 {
            for k in 0..5 {
                mdl.dc_token_probs[0][i][k] = rescale_prob(mdl.dc_value_probs[k], &VP6_DC_WEIGHTS[k][i], 255);
            }
        }
    }
    Ok(())
}

pub fn encode_block(bc: &mut BoolEncoder, blk: &[i16; 64], dc_mode: usize, model: &VP56CoeffModel, vp6model: &VP6Models) -> EncoderResult<()> {
    let mut last = 64;
    for i in (0..64).rev() {
        if blk[vp6model.zigzag[i]] != 0 {
            last = i;
            break;
        }
    }
    if last < 64 {
        bc.write_dc(blk[0], &model.dc_token_probs[0][dc_mode], &model.dc_value_probs)?;
        let mut idx = 1;
        let mut last_idx = 0;
        let mut last_val = blk[0];
        while idx <= last {
            let val = blk[vp6model.zigzag[idx]];
            let has_nnz = (idx == 1) || (last_val != 0);
            if (val != 0) || has_nnz {
                if last_val == 0 && idx != 1 {
                    let zrun = idx - last_idx;
                    bc.write_zero_run(zrun, &vp6model.zero_run_probs[if last_idx + 1 >= 7 { 1 } else { 0 }])?;
                }
                let ac_band = VP6_IDX_TO_AC_BAND[idx];
                let ac_mode = last_val.abs().min(2) as usize;
                let tree = if has_nnz { COEF_TREE } else { NZ_COEF_TREE };
                bc.write_ac(val, tree, &model.ac_val_probs[ac_mode][ac_band])?;
                last_val = val;
                last_idx = idx;
            }
            idx += 1;
        }
        if idx < 64 {
            let ac_band = VP6_IDX_TO_AC_BAND[idx];
            let ac_mode = last_val.abs().min(2) as usize;
            bc.write_el(EOB, COEF_TREE, &model.ac_val_probs[ac_mode][ac_band])?;
        }
    } else {
        bc.write_cat(0, DC_TREE, &model.dc_token_probs[0][dc_mode], &model.dc_value_probs)?;
        let ac_band = VP6_IDX_TO_AC_BAND[1];
        bc.write_el(EOB, COEF_TREE, &model.ac_val_probs[0][ac_band])?;
    }
    Ok(())
}

fn map_mb_type(mbtype: VPMBType) -> usize {
    match mbtype {
        VPMBType::InterNoMV     => 0,
        VPMBType::Intra         => 1,
        VPMBType::InterMV       => 2,
        VPMBType::InterNearest  => 3,
        VPMBType::InterNear     => 4,
        VPMBType::GoldenNoMV    => 5,
        VPMBType::GoldenMV      => 6,
        VPMBType::InterFourMV   => 7,
        VPMBType::GoldenNearest => 8,
        VPMBType::GoldenNear    => 9,
    }
}

pub fn encode_mb_type(bc: &mut BoolEncoder, mb_type: VPMBType, last_mb_type: VPMBType, ctx: usize, model: &VP56Models) -> EncoderResult<()> {
    let probs = &model.mbtype_models[ctx][map_mb_type(last_mb_type)].probs;
    bc.put_bool(mb_type == last_mb_type, probs[9])?;
    if mb_type != last_mb_type {
        bc.write_el(mb_type, MODE_TREE, probs)?;
    }
    Ok(())
}

fn encode_mv_component(bc: &mut BoolEncoder, mv: i16, model: &VP56MVModel) -> EncoderResult<()> {
    let aval = mv.abs();
    bc.put_bool(aval >= 8, model.nz_prob)?;
    if aval < 8 {
        bc.write_el(aval as u8, SHORT_MV_TREE, &model.tree_probs)?;
    } else {
        for &ord in LONG_VECTOR_ORDER.iter() {
            bc.put_bool(((aval >> ord) & 1) != 0, model.raw_probs[ord])?;
        }
        if (aval & 0xF0) != 0 {
            bc.put_bool((aval & (1 << 3)) != 0, model.raw_probs[3])?;
        }
    }
    if aval != 0 {
        bc.put_bool(mv < 0, model.sign_prob)?;
    }
    Ok(())
}

pub fn encode_mv(bc: &mut BoolEncoder, mv: MV, model: &VP56Models) -> EncoderResult<()> {
    encode_mv_component(bc, mv.x, &model.mv_models[0])?;
    encode_mv_component(bc, mv.y, &model.mv_models[1])?;
    Ok(())
}

pub trait VP6EstimatorTrait {
    fn write_cat(&self, cat: i8, tree: &[TokenSeq<i8>], probs: &mut [ProbCounter; 11]);
    fn write_dc(&self, val: i16, probs: &mut [ProbCounter; 11]);
    fn write_ac(&self, val: i16, tree: &[TokenSeq<i8>], probs: &mut [ProbCounter; 11]);
    fn write_zero_run(&self, val: usize, probs: &mut [ProbCounter; 14]);
}

impl VP6EstimatorTrait for Estimator {
    fn write_cat(&self, cat: i8, tree: &[TokenSeq<i8>], probs: &mut [ProbCounter; 11]) {
        for entry in tree.iter() {
            if entry.val == cat {
                for seq in entry.seq.iter() {
                    probs[seq.idx as usize].add(seq.bit);
                }
                return;
            }
        }
    }
    fn write_dc(&self, val: i16, probs: &mut [ProbCounter; 11]) {
        self.write_cat(coef_to_cat(val), DC_TREE, probs);
    }
    fn write_ac(&self, val: i16, tree: &[TokenSeq<i8>], probs: &mut [ProbCounter; 11]) {
        self.write_cat(coef_to_cat(val), tree, probs);
    }
    fn write_zero_run(&self, val: usize, probs: &mut [ProbCounter; 14]) {
        self.write_el(val.min(9) as u8, ZERO_RUN_TREE, probs);
        if val >= 9 {
            let add = val - 9;
            for i in 0..6 {
                probs[i + 8].add(((add >> i) & 1) != 0);
            }
        }
    }
}

pub fn estimate_block(blk: &[i16; 64], _dc_mode: usize, model: &mut VP56CoeffModelStat, vp6model: &mut VP6ModelsStat, scan: &[usize; 64]) {
    let bc = Estimator::new();

    let mut last = 64;
    for i in (0..64).rev() {
        if blk[scan[i]] != 0 {
            last = i;
            break;
        }
    }
    if last < 64 {
        bc.write_dc(blk[0], &mut model.dc_value_probs);
        let mut idx = 1;
        let mut last_idx = 0;
        let mut last_val = blk[0];
        while idx <= last {
            let val = blk[scan[idx]];
            let has_nnz = (idx == 1) || (last_val != 0);
            if (val != 0) || has_nnz {
                if last_val == 0 && idx != 1 {
                    let zrun = idx - last_idx;
                    bc.write_zero_run(zrun, &mut vp6model.zero_run_probs[if last_idx + 1 >= 7 { 1 } else { 0 }]);
                }
                let ac_band = VP6_IDX_TO_AC_BAND[idx];
                let ac_mode = last_val.abs().min(2) as usize;
                let tree = if has_nnz { COEF_TREE } else { NZ_COEF_TREE };
                bc.write_ac(val, tree, &mut model.ac_val_probs[ac_mode][ac_band]);
                last_val = val;
                last_idx = idx;
            }
            idx += 1;
        }
        if idx < 64 {
            let ac_band = VP6_IDX_TO_AC_BAND[idx];
            let ac_mode = last_val.abs().min(2) as usize;
            bc.write_el(EOB, COEF_TREE, &mut model.ac_val_probs[ac_mode][ac_band]);
        }
    } else {
        bc.write_cat(0, DC_TREE, &mut model.dc_value_probs);
        let ac_band = VP6_IDX_TO_AC_BAND[1];
        bc.write_el(EOB, COEF_TREE, &mut model.ac_val_probs[0][ac_band]);
    }
}

pub fn estimate_mb_type(mb_type: VPMBType, last_mb_type: VPMBType, ctx: usize, model: &mut VP56ModelsStat) {
    model.mbtype_models[ctx][map_mb_type(last_mb_type)][map_mb_type(mb_type)] += 1;
}

fn estimate_mv_component(mv: i16, model: &mut VP56MVModelStat) {
    let bc = Estimator::new();
    let aval = mv.abs();
    model.nz_prob.add(aval >= 8);
    if aval < 8 {
        bc.write_el(aval as u8, SHORT_MV_TREE, &mut model.tree_probs);
    } else {
        for &ord in LONG_VECTOR_ORDER.iter() {
            model.raw_probs[ord].add(((aval >> ord) & 1) != 0);
        }
        if (aval & 0xF0) != 0 {
            model.raw_probs[3].add((aval & (1 << 3)) != 0);
        }
    }
    if aval != 0 {
        model.sign_prob.add(mv < 0);
    }
}

pub fn estimate_mv(mv: MV, model: &mut VP56ModelsStat) {
    estimate_mv_component(mv.x, &mut model.mv_models[0]);
    estimate_mv_component(mv.y, &mut model.mv_models[1]);
}

const VP56_MODE_VQ: [[[u8; 20]; 16]; 3] = [
  [
    [   9,  15,  32,  25,   7,  19,   9,  21,   1,  12,  14,  12,   3,  18,  14,  23,   3,  10,   0,   4 ],
    [  48,  39,   1,   2,  11,  27,  29,  44,   7,  27,   1,   4,   0,   3,   1,   6,   1,   2,   0,   0 ],
    [  21,  32,   1,   2,   4,  10,  32,  43,   6,  23,   2,   3,   1,  19,   1,   6,  12,  21,   0,   7 ],
    [  69,  83,   0,   0,   0,   2,  10,  29,   3,  12,   0,   1,   0,   3,   0,   3,   2,   2,   0,   0 ],
    [  11,  20,   1,   4,  18,  36,  43,  48,  13,  35,   0,   2,   0,   5,   3,  12,   1,   2,   0,   0 ],
    [  70,  44,   0,   1,   2,  10,  37,  46,   8,  26,   0,   2,   0,   2,   0,   2,   0,   1,   0,   0 ],
    [   8,  15,   0,   1,   8,  21,  74,  53,  22,  42,   0,   1,   0,   2,   0,   3,   1,   2,   0,   0 ],
    [ 141,  42,   0,   0,   1,   4,  11,  24,   1,  11,   0,   1,   0,   1,   0,   2,   0,   0,   0,   0 ],
    [   8,  19,   4,  10,  24,  45,  21,  37,   9,  29,   0,   3,   1,   7,  11,  25,   0,   2,   0,   1 ],
    [  46,  42,   0,   1,   2,  10,  54,  51,  10,  30,   0,   2,   0,   2,   0,   1,   0,   1,   0,   0 ],
    [  28,  32,   0,   0,   3,  10,  75,  51,  14,  33,   0,   1,   0,   2,   0,   1,   1,   2,   0,   0 ],
    [ 100,  46,   0,   1,   3,   9,  21,  37,   5,  20,   0,   1,   0,   2,   1,   2,   0,   1,   0,   0 ],
    [  27,  29,   0,   1,   9,  25,  53,  51,  12,  34,   0,   1,   0,   3,   1,   5,   0,   2,   0,   0 ],
    [  80,  38,   0,   0,   1,   4,  69,  33,   5,  16,   0,   1,   0,   1,   0,   0,   0,   1,   0,   0 ],
    [  16,  20,   0,   0,   2,   8, 104,  49,  15,  33,   0,   1,   0,   1,   0,   1,   1,   1,   0,   0 ],
    [ 194,  16,   0,   0,   1,   1,   1,   9,   1,   3,   0,   0,   0,   1,   0,   1,   0,   0,   0,   0 ],
  ], [
    [  41,  22,   1,   0,   1,  31,   0,   0,   0,   0,   0,   1,   1,   7,   0,   1,  98,  25,   4,  10 ],
    [ 123,  37,   6,   4,   1,  27,   0,   0,   0,   0,   5,   8,   1,   7,   0,   1,  12,  10,   0,   2 ],
    [  26,  14,  14,  12,   0,  24,   0,   0,   0,   0,  55,  17,   1,   9,   0,  36,   5,   7,   1,   3 ],
    [ 209,   5,   0,   0,   0,  27,   0,   0,   0,   0,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0 ],
    [   2,   5,   4,   5,   0, 121,   0,   0,   0,   0,   0,   3,   2,   4,   1,   4,   2,   2,   0,   1 ],
    [ 175,   5,   0,   1,   0,  48,   0,   0,   0,   0,   0,   2,   0,   1,   0,   2,   0,   1,   0,   0 ],
    [  83,   5,   2,   3,   0, 102,   0,   0,   0,   0,   1,   3,   0,   2,   0,   1,   0,   0,   0,   0 ],
    [ 233,   6,   0,   0,   0,   8,   0,   0,   0,   0,   0,   1,   0,   1,   0,   0,   0,   1,   0,   0 ],
    [  34,  16, 112,  21,   1,  28,   0,   0,   0,   0,   6,   8,   1,   7,   0,   3,   2,   5,   0,   2 ],
    [ 159,  35,   2,   2,   0,  25,   0,   0,   0,   0,   3,   6,   0,   5,   0,   1,   4,   4,   0,   1 ],
    [  75,  39,   5,   7,   2,  48,   0,   0,   0,   0,   3,  11,   2,  16,   1,   4,   7,  10,   0,   2 ],
    [ 212,  21,   0,   1,   0,   9,   0,   0,   0,   0,   1,   2,   0,   2,   0,   0,   2,   2,   0,   0 ],
    [   4,   2,   0,   0,   0, 172,   0,   0,   0,   0,   0,   1,   0,   2,   0,   0,   2,   0,   0,   0 ],
    [ 187,  22,   1,   1,   0,  17,   0,   0,   0,   0,   3,   6,   0,   4,   0,   1,   4,   4,   0,   1 ],
    [ 133,   6,   1,   2,   1,  70,   0,   0,   0,   0,   0,   2,   0,   4,   0,   3,   1,   1,   0,   0 ],
    [ 251,   1,   0,   0,   0,   2,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0 ],
  ], [
    [   2,   3,   2,   3,   0,   2,   0,   2,   0,   0,  11,   4,   1,   4,   0,   2,   3,   2,   0,   4 ],
    [  49,  46,   3,   4,   7,  31,  42,  41,   0,   0,   2,   6,   1,   7,   1,   4,   2,   4,   0,   1 ],
    [  26,  25,   1,   1,   2,  10,  67,  39,   0,   0,   1,   1,   0,  14,   0,   2,  31,  26,   1,   6 ],
    [ 103,  46,   1,   2,   2,  10,  33,  42,   0,   0,   1,   4,   0,   3,   0,   1,   1,   3,   0,   0 ],
    [  14,  31,   9,  13,  14,  54,  22,  29,   0,   0,   2,   6,   4,  18,   6,  13,   1,   5,   0,   1 ],
    [  85,  39,   0,   0,   1,   9,  69,  40,   0,   0,   0,   1,   0,   3,   0,   1,   2,   3,   0,   0 ],
    [  31,  28,   0,   0,   3,  14, 130,  34,   0,   0,   0,   1,   0,   3,   0,   1,   3,   3,   0,   1 ],
    [ 171,  25,   0,   0,   1,   5,  25,  21,   0,   0,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0 ],
    [  17,  21,  68,  29,   6,  15,  13,  22,   0,   0,   6,  12,   3,  14,   4,  10,   1,   7,   0,   3 ],
    [  51,  39,   0,   1,   2,  12,  91,  44,   0,   0,   0,   2,   0,   3,   0,   1,   2,   3,   0,   1 ],
    [  81,  25,   0,   0,   2,   9, 106,  26,   0,   0,   0,   1,   0,   1,   0,   1,   1,   1,   0,   0 ],
    [ 140,  37,   0,   1,   1,   8,  24,  33,   0,   0,   1,   2,   0,   2,   0,   1,   1,   2,   0,   0 ],
    [  14,  23,   1,   3,  11,  53,  90,  31,   0,   0,   0,   3,   1,   5,   2,   6,   1,   2,   0,   0 ],
    [ 123,  29,   0,   0,   1,   7,  57,  30,   0,   0,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0 ],
    [  13,  14,   0,   0,   4,  20, 175,  20,   0,   0,   0,   1,   0,   1,   0,   1,   1,   1,   0,   0 ],
    [ 202,  23,   0,   0,   1,   3,   2,   9,   0,   0,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0 ],
  ]
];
