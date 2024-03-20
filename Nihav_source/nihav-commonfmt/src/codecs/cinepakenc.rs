use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::vq::*;

#[derive(Default,Clone,Copy,PartialEq,Debug)]
struct YUVCode {
    y:  [u8; 4],
    u:  u8,
    v:  u8,
}
impl VQElement for YUVCode {
    fn dist(&self, rval: Self) -> u32 {
        let mut ysum = 0;
        for (y0, y1) in self.y.iter().zip(rval.y.iter()) {
            let yd = i32::from(*y0) - i32::from(*y1);
            ysum += yd * yd;
        }
        let ud = i32::from(self.u) - i32::from(rval.u);
        let vd = i32::from(self.v) - i32::from(rval.v);
        (ysum + ud * ud + vd * vd) as u32
    }
    fn min_cw() -> Self { YUVCode { y: [0; 4], u: 0, v: 0 } }
    fn max_cw() -> Self { YUVCode { y: [255; 4], u: 255, v: 255 } }
    fn min(&self, rval: Self) -> Self {
        let mut ycode = YUVCode::default();
        for i in 0..4 {
            ycode.y[i] = self.y[i].min(rval.y[i]);
        }
        ycode.u = self.u.min(rval.u);
        ycode.v = self.v.min(rval.v);
        ycode
    }
    fn max(&self, rval: Self) -> Self {
        let mut ycode = YUVCode::default();
        for i in 0..4 {
            ycode.y[i] = self.y[i].max(rval.y[i]);
        }
        ycode.u = self.u.max(rval.u);
        ycode.v = self.v.max(rval.v);
        ycode
    }
    fn num_components() -> usize { 6 }
    fn sort_by_component(arr: &mut [Self], component: usize) {
        let mut counts = [0; 256];
        for entry in arr.iter() {
            let idx = match component {
                    0 | 1 | 2 | 3 => entry.y[component],
                    4 => entry.u,
                    _ => entry.v,
                } as usize;
            counts[idx] += 1;
        }
        let mut offs = [0; 256];
        for i in 0..255 {
            offs[i + 1] = offs[i] + counts[i];
        }
        let mut dst = vec![YUVCode::default(); arr.len()];
        for entry in arr.iter() {
            let idx = match component {
                    0 | 1 | 2 | 3 => entry.y[component],
                    4 => entry.u,
                    _ => entry.v,
                } as usize;
            dst[offs[idx]] = *entry;
            offs[idx] += 1;
        }
        arr.copy_from_slice(dst.as_slice());
    }
    fn max_dist_component(min: &Self, max: &Self) -> usize {
        let mut comp = 0;
        let mut diff = 0;
        for i in 0..4 {
            let d = u32::from(max.y[i]) - u32::from(min.y[i]);
            if d > diff {
                diff = d;
                comp = i;
            }
        }
        let ud = u32::from(max.u) - u32::from(min.u);
        if ud > diff {
            diff = ud;
            comp = 4;
        }
        let vd = u32::from(max.v) - u32::from(min.v);
        if vd > diff {
            comp = 5;
        }
        comp
    }
}

#[derive(Default)]
struct YUVCodeSum {
    ysum:   [u64; 4],
    usum:   u64,
    vsum:   u64,
    count:  u64,
}

impl VQElementSum<YUVCode> for YUVCodeSum {
    fn zero() -> Self { Self::default() }
    fn add(&mut self, rval: YUVCode, count: u64) {
        for i in 0..4 {
            self.ysum[i] += u64::from(rval.y[i]) * count;
        }
        self.usum += u64::from(rval.u) * count;
        self.vsum += u64::from(rval.v) * count;
        self.count += count;
    }
    fn get_centroid(&self) -> YUVCode {
        if self.count != 0 {
            let mut ycode = YUVCode::default();
            for i in 0..4 {
                ycode.y[i] = ((self.ysum[i] + self.count / 2) / self.count) as u8;
            }
            ycode.u = ((self.usum + self.count / 2) / self.count) as u8;
            ycode.v = ((self.vsum + self.count / 2) / self.count) as u8;
            ycode
        } else {
            YUVCode::default()
        }
    }
}

struct RNG {
    seed: u32,
}

impl RNG {
    fn new() -> Self { Self { seed: 0x12345678 } }
    fn next(&mut self) -> u8 {
        let mut x = self.seed;
        x ^= x.wrapping_shl(13);
        x ^= x >> 17;
        self.seed = x;
        (self.seed >> 24) as u8
    }
    fn fill_entry(&mut self, entry: &mut YUVCode) {
        for y in entry.y.iter_mut() {
            *y = self.next();
        }
        entry.u = self.next();
        entry.v = self.next();
    }
}

const GRAY_FORMAT: NAPixelFormaton = NAPixelFormaton {
        model: ColorModel::YUV(YUVSubmodel::YUVJ),
        components: 1,
        comp_info: [Some(NAPixelChromaton{h_ss: 0, v_ss: 0, packed: false, depth: 8, shift: 0, comp_offs: 0, next_elem: 1}), None, None, None, None],
        elem_size: 1,
        be: true,
        alpha: false,
        palette: false,
    };

struct MaskWriter {
    masks:  Vec<u32>,
    mask:   u32,
    pos:    u8,
}

impl MaskWriter {
    fn new() -> Self {
        Self {
            masks:  Vec::new(),
            mask:   0,
            pos:    0,
        }
    }
    fn reset(&mut self) {
        self.masks.clear();
        self.mask = 0;
        self.pos = 0;
    }
    fn put_v1(&mut self) {
        self.mask <<= 1;
        self.pos += 1;
        if self.pos == 32 {
            self.flush();
        }
    }
    fn put_v4(&mut self) {
        self.mask <<= 1;
        self.mask  |= 1;
        self.pos += 1;
        if self.pos == 32 {
            self.flush();
        }
    }
    fn put_inter(&mut self, skip: bool) {
        self.mask <<= 1;
        self.mask  |= !skip as u32;
        self.pos += 1;
        if self.pos == 32 {
            self.flush();
        }
    }
    fn flush(&mut self) {
        self.masks.push(self.mask);
        self.mask = 0;
        self.pos = 0;
    }
    fn end(&mut self) {
        if self.pos == 0 { return; }
        while self.pos < 32 {
            self.mask <<= 1;
            self.pos += 1;
        }
        self.flush();
    }
}

#[derive(Clone,Copy,PartialEq)]
enum QuantMode {
    ELBG,
    Fast,
    MedianCut,
}

impl std::string::ToString for QuantMode {
    fn to_string(&self) -> String {
        match *self {
            QuantMode::ELBG => "elbg".to_string(),
            QuantMode::Fast => "fast".to_string(),
            QuantMode::MedianCut => "mediancut".to_string(),
        }
    }
}

#[derive(Clone,Copy,PartialEq)]
enum CodingMode {
    Skip,
    V1,
    V4
}

struct CinepakEncoder {
    stream:     Option<NAStreamRef>,
    lastfrm:    Option<NAVideoBufferRef<u8>>,
    pkt:        Option<NAPacket>,
    frmcount:   u8,
    key_int:    u8,
    qmode:      QuantMode,
    quality:    u8,
    refine:     bool,
    nstrips:    usize,
    force_v1:   bool,
    cur_strip:  usize,
    v1_entries: Vec<YUVCode>,
    v4_entries: Vec<YUVCode>,
    v1_cb:      Vec<[YUVCode; 256]>,
    v4_cb:      Vec<[YUVCode; 256]>,
    v1_cur_cb:  Vec<[YUVCode; 256]>,
    v4_cur_cb:  Vec<[YUVCode; 256]>,
    v1_len:     usize,
    v4_len:     usize,
    v1_idx:     Vec<u8>,
    v4_idx:     Vec<u8>,
    grayscale:  bool,
    rng:        RNG,
    masks:      MaskWriter,
    skip_dist:  Vec<u32>,
    fst_bins:   [Vec<YUVCode>; 4],
    v1_cand:    Vec<YUVCode>,
    v4_cand:    Vec<YUVCode>,
    cmode:      Vec<CodingMode>,
}

fn avg4(a: u8, b: u8, c: u8, d: u8) -> u8 {
    ((u16::from(a) + u16::from(b) + u16::from(c) + u16::from(d) + 3) >> 2) as u8
}

fn variance(a: u8, mean: u8) -> u32 {
    if a >= mean {
        u32::from(a - mean) * u32::from(a - mean)
    } else {
        u32::from(mean - a) * u32::from(mean - a)
    }
}

fn patch_size(bw: &mut ByteWriter, pos: u64) -> EncoderResult<()> {
    let size = bw.tell() - pos;
    bw.seek(SeekFrom::Current(-((size + 3) as i64)))?;
    bw.write_u24be((size + 4) as u32)?;
    bw.seek(SeekFrom::End(0))?;
    Ok(())
}

fn elbg_quant(entries: &[YUVCode], codebook: &mut [YUVCode]) -> usize {
    let cb_len = quantise_median_cut::<YUVCode, YUVCodeSum>(entries, codebook);
    if cb_len < codebook.len() {
        cb_len
    } else {
        let mut elbg: ELBG<YUVCode, YUVCodeSum> = ELBG::new(codebook);
        elbg.quantise(entries, codebook)
    }
}

fn quant_fast(bins: &mut [Vec<YUVCode>; 4], entries: &[YUVCode], codebook: &mut [YUVCode]) -> usize {
    for bin in bins.iter_mut() {
        bin.clear();
    }
    for &entry in entries.iter() {
        let y_avg = avg4(entry.y[0], entry.y[1], entry.y[2], entry.y[3]);
        let dist = entry.y.iter().fold(0u32, |acc, &x| acc + variance(x, y_avg));
        let ilog = if dist == 0 { 0 } else { 32 - dist.leading_zeros() };
        let bin = match ilog {
                0..=3 => &mut bins[0],
                4..=7 => &mut bins[1],
                8..=11 => &mut bins[2],
                _ => &mut bins[3],
            };
        bin.push(entry);
    }
    let mut free_cw = codebook.len();
    let mut entries_left = entries.len();
    let mut offset = 0;
    for bin in bins.iter() {
        if bin.is_empty() {
            continue;
        }
        if free_cw == 0 || entries_left == 0 {
            break;
        }
        let target = (free_cw * bin.len() + entries_left - 1) / entries_left;
        let cur_len = elbg_quant(bin, &mut codebook[offset..][..target]);
        offset += cur_len;
        free_cw -= cur_len;
        entries_left -= bin.len();
    }
    offset
}

impl CinepakEncoder {
    fn new() -> Self {
        Self {
            stream:     None,
            pkt:        None,
            lastfrm:    None,
            frmcount:   0,
            qmode:      QuantMode::Fast,
            key_int:    25,
            quality:    0,
            refine:     false,
            nstrips:    2,
            force_v1:   false,
            cur_strip:  0,
            v1_entries: Vec::new(),
            v4_entries: Vec::new(),
            v1_cb:      Vec::with_capacity(2),
            v4_cb:      Vec::with_capacity(2),
            v1_cur_cb:  Vec::with_capacity(2),
            v4_cur_cb:  Vec::with_capacity(2),
            v1_len:     0,
            v4_len:     0,
            grayscale:  false,
            rng:        RNG::new(),
            v1_idx:     Vec::new(),
            v4_idx:     Vec::new(),
            masks:      MaskWriter::new(),
            skip_dist:  Vec::new(),
            fst_bins:   [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            v1_cand:    Vec::new(),
            v4_cand:    Vec::new(),
            cmode:      Vec::new(),
        }
    }
    fn read_strip(&mut self, in_frm: &NAVideoBuffer<u8>, start: usize, end: usize) {
        let ystride  = in_frm.get_stride(0);
        let mut yoff = in_frm.get_offset(0) + start * ystride;
        let ustride  = in_frm.get_stride(1);
        let mut uoff = in_frm.get_offset(1) + start / 2 * ustride;
        let vstride  = in_frm.get_stride(2);
        let mut voff = in_frm.get_offset(2) + start / 2 * vstride;
        let (width, _) = in_frm.get_dimensions(0);
        let data = in_frm.get_data();
        self.v1_entries.clear();
        self.v4_entries.clear();
        for _ in (start..end).step_by(4) {
            for x in (0..width).step_by(4) {
                let mut yblk = [0; 16];
                let mut ublk = [128; 4];
                let mut vblk = [128; 4];
                for j in 0..4 {
                    for i in 0..4 {
                        yblk[i + j * 4] = data[yoff + x + i + j * ystride];
                    }
                }
                if !self.grayscale {
                    for j in 0..2 {
                        for i in 0..2 {
                            ublk[i + j * 2] = data[uoff + x / 2 + i + j * ustride];
                            vblk[i + j * 2] = data[voff + x / 2 + i + j * vstride];
                        }
                    }
                }
                self.v1_entries.push(YUVCode {
                        y: [avg4(yblk[ 0], yblk[ 1], yblk[ 4], yblk[ 5]),
                            avg4(yblk[ 2], yblk[ 3], yblk[ 6], yblk[ 7]),
                            avg4(yblk[ 8], yblk[ 9], yblk[12], yblk[13]),
                            avg4(yblk[10], yblk[11], yblk[14], yblk[15])],
                        u: avg4(ublk[0], ublk[1], ublk[2], ublk[3]),
                        v: avg4(vblk[0], vblk[1], vblk[2], vblk[3]),
                    });
                for i in 0..4 {
                    let yidx = (i & 1) * 2 + (i & 2) * 4;
                    self.v4_entries.push(YUVCode {
                            y: [ yblk[yidx], yblk[yidx + 1], yblk[yidx + 4], yblk[yidx + 5] ],
                            u: ublk[i],
                            v: vblk[i],
                        });
                }
            }
            yoff += ystride * 4;
            uoff += ustride * 2;
            voff += vstride * 2;
        }
    }
    fn find_nearest(codebook: &[YUVCode], code: YUVCode) -> (u8, u32) {
        let mut min_dist = std::u32::MAX;
        let mut idx = 0;
        for (i, cw) in codebook.iter().enumerate() {
            let dist = cw.dist(code);
            if dist < min_dist {
                min_dist = dist;
                idx = i;
                if dist == 0 {
                    break;
                }
            }
        }
        (idx as u8, min_dist)
    }
    fn can_update_cb(new_cb: &[YUVCode], old_cb: &[YUVCode], cb_size: usize) -> bool {
        let mut skip_count = 0;
        for (new, old) in new_cb.iter().zip(old_cb.iter()) {
            if new == old {
                skip_count += 1;
            }
        }
        let full_size = cb_size * new_cb.len();
        let upd_size = cb_size * (new_cb.len() - skip_count) + (new_cb.len() + 31) / 32 * 4;
        upd_size < full_size
    }
    fn write_cb(bw: &mut ByteWriter, mut id: u8, new_cb: &[YUVCode], old_cb: &[YUVCode], grayscale: bool, update: bool, num_elem: usize) -> EncoderResult<()> {
        if grayscale {
            id |= 4;
        }
        if update {
            id |= 1;
        }
        bw.write_byte(id)?;
        bw.write_u24be(0)?;
        let chunk_pos = bw.tell();
        if !update {
            for entry in new_cb.iter().take(num_elem) {
                bw.write_buf(&entry.y)?;
                if !grayscale {
                    bw.write_byte(entry.u ^ 0x80)?;
                    bw.write_byte(entry.v ^ 0x80)?;
                }
            }
        } else {
            let mut end = num_elem;
            for (i, (ncw, ocw)) in new_cb.iter().zip(old_cb.iter()).enumerate().take(num_elem).rev() {
                if ncw == ocw {
                    end = i;
                } else {
                    break;
                }
            }
            for i in (0..end).step_by(32) {
                let mut mask = 0;
                for j in 0..32 {
                    mask <<= 1;
                    if new_cb[i + j] != old_cb[i + j] {
                        mask |= 1;
                    }
                }
                bw.write_u32be(mask)?;
                for j in 0..32 {
                    if new_cb[i + j] == old_cb[i + j] { continue; }
                    bw.write_buf(&new_cb[i + j].y)?;
                    if !grayscale {
                        bw.write_byte(new_cb[i + j].u ^ 0x80)?;
                        bw.write_byte(new_cb[i + j].v ^ 0x80)?;
                    }
                }
            }
        }
        patch_size(bw, chunk_pos)?;
        Ok(())
    }
    fn render_stripe(&mut self, intra: bool, start: usize, end: usize) {
        if let Some(ref mut dst_frm) = self.lastfrm {
            let ystride  = dst_frm.get_stride(0);
            let mut yoff = dst_frm.get_offset(0) + start * ystride;
            let ustride  = dst_frm.get_stride(1);
            let mut uoff = dst_frm.get_offset(1) + start / 2 * ustride;
            let vstride  = dst_frm.get_stride(2);
            let mut voff = dst_frm.get_offset(2) + start / 2 * vstride;
            let (width, _) = dst_frm.get_dimensions(0);
            let data = dst_frm.get_data_mut().unwrap();
            let mut miter = self.masks.masks.iter();
            let mut v1_iter = self.v1_idx.iter();
            let mut v4_iter = self.v4_idx.iter();
            let mut cur_mask = 0;
            let mut cur_bit = 0;
            for _ in (start..end).step_by(4) {
                for x in (0..width).step_by(4) {
                    if cur_bit == 0 {
                        if !intra || !self.v1_idx.is_empty() {
                            cur_mask = *miter.next().unwrap();
                        } else {
                            cur_mask = 0xFFFFFFFF;
                        }
                        cur_bit = 1 << 31;
                    }
                    if !intra {
                        if (cur_mask & cur_bit) == 0 {
                            cur_bit >>= 1;
                            continue;
                        }
                        cur_bit >>= 1;
                        if cur_bit == 0 {
                            cur_mask = *miter.next().unwrap();
                            cur_bit = 1 << 31;
                        }
                    }
                    if (cur_mask & cur_bit) == 0 {
                        let idx = *v1_iter.next().unwrap() as usize;
                        let cb = &self.v1_cur_cb[self.cur_strip][idx];

                        let mut coff = yoff + x;
                        data[coff]     = cb.y[0]; data[coff + 1] = cb.y[0];
                        data[coff + 2] = cb.y[1]; data[coff + 3] = cb.y[1];
                        coff += ystride;
                        data[coff]     = cb.y[0]; data[coff + 1] = cb.y[0];
                        data[coff + 2] = cb.y[1]; data[coff + 3] = cb.y[1];
                        coff += ystride;
                        data[coff]     = cb.y[2]; data[coff + 1] = cb.y[2];
                        data[coff + 2] = cb.y[3]; data[coff + 3] = cb.y[3];
                        coff += ystride;
                        data[coff]     = cb.y[2]; data[coff + 1] = cb.y[2];
                        data[coff + 2] = cb.y[3]; data[coff + 3] = cb.y[3];

                        if !self.grayscale {
                            let mut coff = uoff + x / 2;
                            data[coff] = cb.u; data[coff + 1] = cb.u;
                            coff += ustride;
                            data[coff] = cb.u; data[coff + 1] = cb.u;

                            let mut coff = voff + x / 2;
                            data[coff] = cb.v; data[coff + 1] = cb.v;
                            coff += vstride;
                            data[coff] = cb.v; data[coff + 1] = cb.v;
                        }
                    } else {
                        let idx0 = *v4_iter.next().unwrap() as usize;
                        let cb0 = &self.v4_cur_cb[self.cur_strip][idx0];
                        let idx1 = *v4_iter.next().unwrap() as usize;
                        let cb1 = &self.v4_cur_cb[self.cur_strip][idx1];
                        let idx2 = *v4_iter.next().unwrap() as usize;
                        let cb2 = &self.v4_cur_cb[self.cur_strip][idx2];
                        let idx3 = *v4_iter.next().unwrap() as usize;
                        let cb3 = &self.v4_cur_cb[self.cur_strip][idx3];

                        let mut coff = yoff + x;
                        data[coff]     = cb0.y[0]; data[coff + 1] = cb0.y[1];
                        data[coff + 2] = cb1.y[0]; data[coff + 3] = cb1.y[1];
                        coff += ystride;
                        data[coff]     = cb0.y[2]; data[coff + 1] = cb0.y[3];
                        data[coff + 2] = cb1.y[2]; data[coff + 3] = cb1.y[3];
                        coff += ystride;
                        data[coff]     = cb2.y[0]; data[coff + 1] = cb2.y[1];
                        data[coff + 2] = cb3.y[0]; data[coff + 3] = cb3.y[1];
                        coff += ystride;
                        data[coff]     = cb2.y[2]; data[coff + 1] = cb2.y[3];
                        data[coff + 2] = cb3.y[2]; data[coff + 3] = cb3.y[3];

                        if !self.grayscale {
                            let mut coff = uoff + x / 2;
                            data[coff] = cb0.u; data[coff + 1] = cb1.u;
                            coff += ustride;
                            data[coff] = cb2.u; data[coff + 1] = cb3.u;

                            let mut coff = voff + x / 2;
                            data[coff] = cb0.v; data[coff + 1] = cb1.v;
                            coff += vstride;
                            data[coff] = cb2.v; data[coff + 1] = cb3.v;
                        }
                    }
                    cur_bit >>= 1;
                }
                yoff += ystride * 4;
                uoff += ustride * 2;
                voff += vstride * 2;
            }
        } else {
            unreachable!();
        }
    }
    fn calc_skip_dist(&mut self, in_frm: &NAVideoBuffer<u8>, start: usize, end: usize) {
        self.skip_dist.clear();
        if let Some(ref ref_frm) = self.lastfrm {
            let rystride  = ref_frm.get_stride(0);
            let mut ryoff = ref_frm.get_offset(0) + start * rystride;
            let rustride  = ref_frm.get_stride(1);
            let mut ruoff = ref_frm.get_offset(1) + start / 2 * rustride;
            let rvstride  = ref_frm.get_stride(2);
            let mut rvoff = ref_frm.get_offset(2) + start / 2 * rvstride;
            let (width, _) = ref_frm.get_dimensions(0);
            let rdata = ref_frm.get_data();

            let iystride  = in_frm.get_stride(0);
            let mut iyoff = in_frm.get_offset(0) + start * iystride;
            let iustride  = in_frm.get_stride(1);
            let mut iuoff = in_frm.get_offset(1) + start / 2 * iustride;
            let ivstride  = in_frm.get_stride(2);
            let mut ivoff = in_frm.get_offset(2) + start / 2 * ivstride;
            let idata = in_frm.get_data();

            for _ in (start..end).step_by(4) {
                for x in (0..width).step_by(4) {
                    let mut dist = 0;
                    let mut roff = ryoff + x;
                    let mut ioff = iyoff + x;
                    for _ in 0..4 {
                        for i in 0..4 {
                            let d = i32::from(rdata[roff + i]) - i32::from(idata[ioff + i]);
                            dist += d * d;
                        }
                        roff += rystride;
                        ioff += iystride;
                    }
                    if !self.grayscale {
                        let mut roff = ruoff + x / 2;
                        let mut ioff = iuoff + x / 2;
                        let ud = i32::from(rdata[roff]) - i32::from(idata[ioff]);
                        dist += ud * ud;
                        let ud = i32::from(rdata[roff + 1]) - i32::from(idata[ioff + 1]);
                        dist += ud * ud;
                        roff += rustride; ioff += iustride;
                        let ud = i32::from(rdata[roff]) - i32::from(idata[ioff]);
                        dist += ud * ud;
                        let ud = i32::from(rdata[roff + 1]) - i32::from(idata[ioff + 1]);
                        dist += ud * ud;

                        let mut roff = rvoff + x / 2;
                        let mut ioff = ivoff + x / 2;
                        let vd = i32::from(rdata[roff]) - i32::from(idata[ioff]);
                        dist += vd * vd;
                        let vd = i32::from(rdata[roff + 1]) - i32::from(idata[ioff + 1]);
                        dist += vd * vd;
                        roff += rvstride; ioff += ivstride;
                        let vd = i32::from(rdata[roff]) - i32::from(idata[ioff]);
                        dist += vd * vd;
                        let vd = i32::from(rdata[roff + 1]) - i32::from(idata[ioff + 1]);
                        dist += vd * vd;
                    }
                    self.skip_dist.push(dist as u32);
                }

                iyoff += iystride * 4;
                iuoff += iustride * 2;
                ivoff += ivstride * 2;
                ryoff += rystride * 4;
                ruoff += rustride * 2;
                rvoff += rvstride * 2;
            }
        } else {
            unreachable!();
        }
    }
    fn quant_vectors(&mut self) {
        match self.qmode {
            QuantMode::ELBG => {
                self.v1_len = elbg_quant(&self.v1_entries, &mut self.v1_cur_cb[self.cur_strip]);
                self.v4_len = if !self.force_v1 {
                        elbg_quant(&self.v4_entries, &mut self.v4_cur_cb[self.cur_strip])
                    } else {
                        0
                    };
            },
            QuantMode::Fast => {
                for bin in self.fst_bins.iter_mut() {
                    bin.clear();
                }
                self.v1_len = quant_fast(&mut self.fst_bins, &self.v1_entries, &mut self.v1_cur_cb[self.cur_strip]);
                self.v4_len = if !self.force_v1 {
                        quant_fast(&mut self.fst_bins, &self.v4_entries, &mut self.v4_cur_cb[self.cur_strip])
                    } else {
                        0
                    };
            },
            QuantMode::MedianCut => {
                self.v1_len = quantise_median_cut::<YUVCode, YUVCodeSum>(&self.v1_entries, &mut self.v1_cur_cb[self.cur_strip]);
                if !self.force_v1 {
                    self.v4_len = quantise_median_cut::<YUVCode, YUVCodeSum>(&self.v4_entries, &mut self.v4_cur_cb[self.cur_strip]);
                } else {
                    self.v4_len = 0;
                }
            },
        };

        for e in self.v1_cur_cb[self.cur_strip].iter_mut().skip(self.v1_len) { *e = YUVCode::default(); }
        for e in self.v4_cur_cb[self.cur_strip].iter_mut().skip(self.v4_len) { *e = YUVCode::default(); }
    }
    fn refine_vectors(&mut self) {
        match self.qmode {
            QuantMode::ELBG => {
                self.v1_len = if !self.v1_cand.is_empty() {
                        elbg_quant(&self.v1_cand, &mut self.v1_cur_cb[self.cur_strip])
                    } else {
                        0
                    };
                self.v4_len = if !self.force_v1 && !self.v4_cand.is_empty() {
                        elbg_quant(&self.v4_cand, &mut self.v4_cur_cb[self.cur_strip])
                    } else {
                        0
                    };
            },
            QuantMode::Fast => {
                for bin in self.fst_bins.iter_mut() {
                    bin.clear();
                }
                self.v1_len = if !self.v1_cand.is_empty() {
                        quant_fast(&mut self.fst_bins, &self.v1_cand, &mut self.v1_cur_cb[self.cur_strip])
                    } else {
                        0
                    };
                self.v4_len = if !self.force_v1 && !self.v4_cand.is_empty() {
                        quant_fast(&mut self.fst_bins, &self.v4_cand, &mut self.v4_cur_cb[self.cur_strip])
                    } else {
                        0
                    };
            },
            QuantMode::MedianCut => {
                self.v1_len = if !self.v1_cand.is_empty() {
                        quantise_median_cut::<YUVCode, YUVCodeSum>(&self.v1_cand, &mut self.v1_cur_cb[self.cur_strip])
                    } else {
                        0
                    };
                if !self.force_v1 && !self.v4_cand.is_empty() {
                    self.v4_len = quantise_median_cut::<YUVCode, YUVCodeSum>(&self.v4_cand, &mut self.v4_cur_cb[self.cur_strip]);
                } else {
                    self.v4_len = 0;
                }
            },
        };

        for e in self.v1_cur_cb[self.cur_strip].iter_mut().skip(self.v1_len) { *e = YUVCode::default(); }
        for e in self.v4_cur_cb[self.cur_strip].iter_mut().skip(self.v4_len) { *e = YUVCode::default(); }
    }
    fn encode_intra(&mut self, bw: &mut ByteWriter, in_frm: &NAVideoBuffer<u8>) -> EncoderResult<bool> {
        let (width, height) = in_frm.get_dimensions(0);
        let mut strip_h = (height / self.nstrips + 3) & !3;
        if strip_h == 0 {
            self.nstrips = 1;
            strip_h = height;
        }
        let mut start_line = 0;
        let mut end_line = strip_h;

        bw.write_byte(0)?; // intra flag
        bw.write_u24be(0)?; // frame size
        let frame_data_pos = bw.tell();
        bw.write_u16be(width as u16)?;
        bw.write_u16be(height as u16)?;
        bw.write_u16be(self.nstrips as u16)?;

        self.cur_strip = 0;
        for entry in self.v1_cb[self.cur_strip].iter_mut() {
            self.rng.fill_entry(entry);
        }
        for entry in self.v4_cb[self.cur_strip].iter_mut() {
            self.rng.fill_entry(entry);
        }
        while start_line < height {
            self.read_strip(in_frm, start_line, end_line);

            if self.cur_strip > 0 {
                self.v1_cb[self.cur_strip] = self.v1_cb[self.cur_strip - 1];
                self.v4_cb[self.cur_strip] = self.v4_cb[self.cur_strip - 1];
            }
            self.quant_vectors();
            if self.grayscale {
                for cw in self.v1_cur_cb[self.cur_strip].iter_mut() {
                    cw.u = 128;
                    cw.v = 128;
                }
                for cw in self.v4_cur_cb[self.cur_strip].iter_mut() {
                    cw.u = 128;
                    cw.v = 128;
                }
            }

            self.v1_idx.clear();
            self.v4_idx.clear();
            self.masks.reset();

            self.cmode.clear();
            self.v1_cand.clear();
            self.v4_cand.clear();
            for (v1_entry, v4_entries) in self.v1_entries.iter().zip(self.v4_entries.chunks(4)) {
                let (v1_idx, v1_dist) = Self::find_nearest(&self.v1_cur_cb[self.cur_strip][..self.v1_len], *v1_entry);
                if v1_dist == 0 || self.force_v1 {
                    if !self.refine {
                        self.masks.put_v1();
                        self.v1_idx.push(v1_idx);
                    } else {
                        self.cmode.push(CodingMode::V1);
                        self.v1_cand.push(*v1_entry);
                    }
                    continue;
                }
                let (v40_idx, v40_dist) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[0]);
                let (v41_idx, v41_dist) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[1]);
                let (v42_idx, v42_dist) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[2]);
                let (v43_idx, v43_dist) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[3]);
                if v40_dist + v41_dist + v42_dist + v43_dist > v1_dist {
                    if !self.refine {
                        self.masks.put_v4();
                        self.v4_idx.push(v40_idx);
                        self.v4_idx.push(v41_idx);
                        self.v4_idx.push(v42_idx);
                        self.v4_idx.push(v43_idx);
                    } else {
                        self.cmode.push(CodingMode::V4);
                        self.v4_cand.extend_from_slice(v4_entries);
                    }
                } else {
                    if !self.refine {
                        self.masks.put_v1();
                        self.v1_idx.push(v1_idx);
                    } else {
                        self.cmode.push(CodingMode::V1);
                        self.v1_cand.push(*v1_entry);
                    }
                }
            }
            if self.refine {
                self.refine_vectors();
                let mut v1_src = self.v1_cand.iter();
                let mut v4_src = self.v4_cand.chunks_exact(4);
                for &cmode in self.cmode.iter() {
                    match cmode {
                        CodingMode::Skip => unreachable!(),
                        CodingMode::V1 => {
                            let v1_entry = v1_src.next().unwrap();
                            let (v1_idx, _) = Self::find_nearest(&self.v1_cur_cb[self.cur_strip][..self.v1_len], *v1_entry);
                            self.masks.put_v1();
                            self.v1_idx.push(v1_idx);
                        },
                        CodingMode::V4 => {
                            let v4_entries = v4_src.next().unwrap();
                            let (v40_idx, _) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[0]);
                            let (v41_idx, _) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[1]);
                            let (v42_idx, _) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[2]);
                            let (v43_idx, _) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[3]);

                            self.masks.put_v4();
                            self.v4_idx.push(v40_idx);
                            self.v4_idx.push(v41_idx);
                            self.v4_idx.push(v42_idx);
                            self.v4_idx.push(v43_idx);
                        },
                    };
                }
            }
            self.masks.end();

            let mut is_intra_strip = start_line == 0;
            let (upd_v1, upd_v4) = if !is_intra_strip {
                    let cb_size = if self.grayscale { 4 } else { 6 };
                    (Self::can_update_cb(&self.v1_cur_cb[self.cur_strip][..self.v1_len], &self.v1_cb[self.cur_strip][..self.v1_len], cb_size),
                     Self::can_update_cb(&self.v4_cur_cb[self.cur_strip][..self.v4_len], &self.v4_cb[self.cur_strip][..self.v4_len], cb_size))
                } else {
                    (false, false)
                };
            if !is_intra_strip && !upd_v1 && !upd_v4 {
                is_intra_strip = true;
            }
            bw.write_byte(if is_intra_strip { 0x10 } else { 0x11 })?;
            bw.write_u24be(0)?; // strip size
            let strip_data_pos = bw.tell();
            bw.write_u16be(0)?; // yoff
            bw.write_u16be(0)?; // xoff
            bw.write_u16be((end_line - start_line) as u16)?;
            bw.write_u16be(width as u16)?;

            Self::write_cb(bw, 0x20, &self.v4_cur_cb[self.cur_strip], &self.v4_cb[self.cur_strip], self.grayscale, upd_v4, self.v4_len)?;
            Self::write_cb(bw, 0x22, &self.v1_cur_cb[self.cur_strip], &self.v1_cb[self.cur_strip], self.grayscale, upd_v1, self.v1_len)?;

            self.render_stripe(true, start_line, end_line);

            if self.v4_idx.is_empty() {
                bw.write_byte(0x32)?;
                bw.write_u24be((self.v1_idx.len() + 4) as u32)?;
                bw.write_buf(self.v1_idx.as_slice())?;
            } else {
                bw.write_byte(0x30)?;
                bw.write_u24be(0)?;
                let chunk_pos = bw.tell();
                let mut v1_pos = 0;
                let mut v4_pos = 0;
                for _ in 0..32 {
                    self.v1_idx.push(0);
                    self.v4_idx.push(0);
                    self.v4_idx.push(0);
                    self.v4_idx.push(0);
                    self.v4_idx.push(0);
                }
                for mask in self.masks.masks.iter() {
                    bw.write_u32be(*mask)?;
                    for j in (0..32).rev() {
                        if (mask & (1 << j)) == 0 {
                            bw.write_byte(self.v1_idx[v1_pos])?;
                            v1_pos += 1;
                        } else {
                            bw.write_byte(self.v4_idx[v4_pos])?;
                            bw.write_byte(self.v4_idx[v4_pos + 1])?;
                            bw.write_byte(self.v4_idx[v4_pos + 2])?;
                            bw.write_byte(self.v4_idx[v4_pos + 3])?;
                            v4_pos += 4;
                        }
                    }
                }
                patch_size(bw, chunk_pos)?;
            }

            patch_size(bw, strip_data_pos)?;

            self.v1_cb[self.cur_strip].copy_from_slice(&self.v1_cur_cb[self.cur_strip]);
            self.v4_cb[self.cur_strip].copy_from_slice(&self.v4_cur_cb[self.cur_strip]);
            start_line = end_line;
            end_line = (end_line + strip_h).min(height);

            self.cur_strip += 1;
        }
        patch_size(bw, frame_data_pos)?;
        Ok(true)
    }
    fn encode_inter(&mut self, bw: &mut ByteWriter, in_frm: &NAVideoBuffer<u8>) -> EncoderResult<bool> {
        let (width, height) = in_frm.get_dimensions(0);
        let mut strip_h = (height / self.nstrips + 3) & !3;
        if strip_h == 0 {
            self.nstrips = 1;
            strip_h = height;
        }
        let mut start_line = 0;
        let mut end_line = strip_h;

        bw.write_byte(1)?; // intra flag
        bw.write_u24be(0)?; // frame size
        let frame_data_pos = bw.tell();
        bw.write_u16be(width as u16)?;
        bw.write_u16be(height as u16)?;
        bw.write_u16be(self.nstrips as u16)?;

        self.cur_strip = 0;
        while start_line < height {
            self.read_strip(in_frm, start_line, end_line);
            self.calc_skip_dist(in_frm, start_line, end_line);

            self.quant_vectors();
            if self.grayscale {
                for cw in self.v1_cur_cb[self.cur_strip].iter_mut() {
                    cw.u = 128;
                    cw.v = 128;
                }
                for cw in self.v4_cur_cb[self.cur_strip].iter_mut() {
                    cw.u = 128;
                    cw.v = 128;
                }
            }

            self.v1_idx.clear();
            self.v4_idx.clear();
            self.masks.reset();

            self.cmode.clear();
            self.v1_cand.clear();
            self.v4_cand.clear();

            let mut skip_iter = self.skip_dist.iter();
            for (v1_entry, v4_entries) in self.v1_entries.iter().zip(self.v4_entries.chunks(4)) {
                let skip_dist = *skip_iter.next().unwrap();
                if skip_dist == 0 {
                    if !self.refine {
                        self.masks.put_inter(true);
                    } else {
                        self.cmode.push(CodingMode::Skip);
                    }
                    continue;
                }
                let (v1_idx, v1_dist) = Self::find_nearest(&self.v1_cur_cb[self.cur_strip][..self.v1_len], *v1_entry);
                if skip_dist < v1_dist {
                    if !self.refine {
                        self.masks.put_inter(true);
                    } else {
                        self.cmode.push(CodingMode::Skip);
                    }
                    continue;
                } else if !self.refine {
                    self.masks.put_inter(false);
                }
                if v1_dist == 0 || self.force_v1 {
                    if !self.refine {
                        self.masks.put_v1();
                        self.v1_idx.push(v1_idx);
                    } else {
                        self.cmode.push(CodingMode::V1);
                        self.v1_cand.push(*v1_entry);
                    }
                    continue;
                }
                let (v40_idx, v40_dist) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[0]);
                let (v41_idx, v41_dist) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[1]);
                let (v42_idx, v42_dist) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[2]);
                let (v43_idx, v43_dist) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[3]);
                if v40_dist + v41_dist + v42_dist + v43_dist > v1_dist {
                    if !self.refine {
                        self.masks.put_v4();
                        self.v4_idx.push(v40_idx);
                        self.v4_idx.push(v41_idx);
                        self.v4_idx.push(v42_idx);
                        self.v4_idx.push(v43_idx);
                    } else {
                        self.cmode.push(CodingMode::V4);
                        self.v4_cand.extend_from_slice(v4_entries);
                    }
                } else {
                    if !self.refine {
                        self.masks.put_v1();
                        self.v1_idx.push(v1_idx);
                    } else {
                        self.cmode.push(CodingMode::V1);
                        self.v1_cand.push(*v1_entry);
                    }
                }
            }
            if self.refine {
                self.refine_vectors();
                let mut v1_src = self.v1_cand.iter();
                let mut v4_src = self.v4_cand.chunks_exact(4);
                for &cmode in self.cmode.iter() {
                    match cmode {
                        CodingMode::Skip => {
                            self.masks.put_inter(true);
                        },
                        CodingMode::V1 => {
                            let v1_entry = v1_src.next().unwrap();
                            let (v1_idx, _) = Self::find_nearest(&self.v1_cur_cb[self.cur_strip][..self.v1_len], *v1_entry);
                            self.masks.put_inter(false);
                            self.masks.put_v1();
                            self.v1_idx.push(v1_idx);
                        },
                        CodingMode::V4 => {
                            let v4_entries = v4_src.next().unwrap();
                            let (v40_idx, _) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[0]);
                            let (v41_idx, _) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[1]);
                            let (v42_idx, _) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[2]);
                            let (v43_idx, _) = Self::find_nearest(&self.v4_cur_cb[self.cur_strip][..self.v4_len], v4_entries[3]);

                            self.masks.put_inter(false);
                            self.masks.put_v4();
                            self.v4_idx.push(v40_idx);
                            self.v4_idx.push(v41_idx);
                            self.v4_idx.push(v42_idx);
                            self.v4_idx.push(v43_idx);
                        },
                    };
                }
            }
            self.masks.end();

            let (upd_v1, upd_v4) = {
                    let cb_size = if self.grayscale { 4 } else { 6 };
                    (Self::can_update_cb(&self.v1_cur_cb[self.cur_strip][..self.v1_len], &self.v1_cb[self.cur_strip][..self.v1_len], cb_size),
                     Self::can_update_cb(&self.v4_cur_cb[self.cur_strip][..self.v4_len], &self.v4_cb[self.cur_strip][..self.v4_len], cb_size))
                };
            bw.write_byte(0x11)?;
            bw.write_u24be(0)?; // strip size
            let strip_data_pos = bw.tell();
            bw.write_u16be(0)?; // yoff
            bw.write_u16be(0)?; // xoff
            bw.write_u16be((end_line - start_line) as u16)?;
            bw.write_u16be(width as u16)?;

            Self::write_cb(bw, 0x20, &self.v4_cur_cb[self.cur_strip], &self.v4_cb[self.cur_strip], self.grayscale, upd_v4, self.v4_len)?;
            Self::write_cb(bw, 0x22, &self.v1_cur_cb[self.cur_strip], &self.v1_cb[self.cur_strip], self.grayscale, upd_v1, self.v1_len)?;

            self.render_stripe(false, start_line, end_line);

            bw.write_byte(0x31)?;
            bw.write_u24be(0)?;
            let chunk_pos = bw.tell();
            let mut v1_pos = 0;
            let mut v4_pos = 0;
            for _ in 0..32 {
                self.v1_idx.push(0);
                self.v4_idx.push(0);
                self.v4_idx.push(0);
                self.v4_idx.push(0);
                self.v4_idx.push(0);
            }
            let mut skip = true;
            for mask in self.masks.masks.iter() {
                bw.write_u32be(*mask)?;
                if *mask == 0 && skip { continue; }
                let mut bit = 1 << 31;
                while bit > 0 {
                    if skip {
                        skip = (mask & bit) == 0;
                        bit >>= 1;
                    } else {
                        if (mask & bit) == 0 {
                            bw.write_byte(self.v1_idx[v1_pos])?;
                            v1_pos += 1;
                        } else {
                            bw.write_byte(self.v4_idx[v4_pos])?;
                            bw.write_byte(self.v4_idx[v4_pos + 1])?;
                            bw.write_byte(self.v4_idx[v4_pos + 2])?;
                            bw.write_byte(self.v4_idx[v4_pos + 3])?;
                            v4_pos += 4;
                        }
                        bit >>= 1;
                        skip = true;
                    }
                }
            }
            patch_size(bw, chunk_pos)?;

            patch_size(bw, strip_data_pos)?;

            self.v1_cb[self.cur_strip].copy_from_slice(&self.v1_cur_cb[self.cur_strip]);
            self.v4_cb[self.cur_strip].copy_from_slice(&self.v4_cur_cb[self.cur_strip]);
            start_line = end_line;
            end_line = (end_line + strip_h).min(height);

            self.cur_strip += 1;
        }
        patch_size(bw, frame_data_pos)?;
        Ok(true)
    }
}

impl NAEncoder for CinepakEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                        format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, true, YUV420_FORMAT)),
                        ..Default::default()
                    })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let pix_fmt = if vinfo.format == GRAY_FORMAT { GRAY_FORMAT } else { YUV420_FORMAT };
                let outinfo = NAVideoInfo::new((vinfo.width + 3) & !3, (vinfo.height + 3) & !3, false, pix_fmt);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { 0 }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                if vinfo.format != YUV420_FORMAT && vinfo.format != GRAY_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if ((vinfo.width | vinfo.height) & 3) != 0 {
                    return Err(EncoderError::FormatError);
                }
                if (vinfo.width | vinfo.height) >= (1 << 16) {
                    return Err(EncoderError::FormatError);
                }

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, false, vinfo.format);
                let info = NACodecInfo::new("cinepak", NACodecTypeInfo::Video(out_info), None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());
                self.quality = encinfo.quality;
                self.grayscale = vinfo.format != YUV420_FORMAT;
                let num_blocks = vinfo.width / 2 * vinfo.height / 2;
                self.v1_entries = Vec::with_capacity(num_blocks);
                self.v4_entries = Vec::with_capacity(num_blocks * 4);
                self.v1_idx = Vec::with_capacity(num_blocks);
                self.v4_idx = Vec::with_capacity(num_blocks * 4);
                self.skip_dist = Vec::with_capacity(vinfo.width / 4 * vinfo.height / 4);

                let buf = alloc_video_buffer(out_info, 2)?;
                self.lastfrm = Some(buf.get_vbuf().unwrap());

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        if let Some(ref vbuf) = buf.get_vbuf() {
            if self.nstrips == 0 {
                let (w, h) = vbuf.get_dimensions(0);
                self.nstrips = ((((w * h) >> 4) + 1200) / 2400).max(1).min(3);
                let strip_h = ((h + self.nstrips - 1) / self.nstrips + 3) & !3;
                self.nstrips = (h + strip_h - 1) / strip_h;
            }
            let cur_strips = self.v1_cb.len();
            if cur_strips != self.nstrips {
                self.frmcount = 0;
            }
            if cur_strips < self.nstrips {
                for _ in cur_strips..self.nstrips {
                    self.v1_cb.push([YUVCode::default(); 256]);
                    self.v4_cb.push([YUVCode::default(); 256]);
                    self.v1_cur_cb.push([YUVCode::default(); 256]);
                    self.v4_cur_cb.push([YUVCode::default(); 256]);
                }
            }

            let mut dbuf = Vec::with_capacity(4);
            let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
            let mut bw   = ByteWriter::new(&mut gw);
            let is_intra = if self.frmcount == 0 {
                    self.encode_intra(&mut bw, vbuf)?
                } else {
                    self.encode_inter(&mut bw, vbuf)?
                };
            self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, is_intra, dbuf));
            self.frmcount += 1;
            if self.frmcount == self.key_int {
                self.frmcount = 0;
            }
            Ok(())
        } else {
            Err(EncoderError::InvalidParameters)
        }
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        let mut npkt = None;
        std::mem::swap(&mut self.pkt, &mut npkt);
        Ok(npkt)
    }
    fn flush(&mut self) -> EncoderResult<()> {
        self.frmcount = 0;
        Ok(())
    }
}

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: KEYFRAME_OPTION, description: KEYFRAME_OPTION_DESC,
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(128)) },
    NAOptionDefinition {
        name: "nstrips", description: "Number of strips per frame (0 - automatic)",
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(16)) },
    NAOptionDefinition {
        name: "quant_mode", description: "Quantisation mode",
        opt_type: NAOptionDefinitionType::String(Some(&["elbg", "fast", "mediancut"])) },
    NAOptionDefinition {
        name: "force_v1", description: "Force coarse (V1-only) mode",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: "refine", description: "Try to improve coded picture",
        opt_type: NAOptionDefinitionType::Bool },
];

impl NAOptionHandler for CinepakEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        KEYFRAME_OPTION => {
                            if let NAValue::Int(intval) = option.value {
                                self.key_int = intval as u8;
                            }
                        },
                        "nstrips" => {
                            if let NAValue::Int(intval) = option.value {
                                self.nstrips = intval as usize;
                            }
                        },
                        "quant_mode" => {
                            if let NAValue::String(ref strval) = option.value {
                                match strval.as_str() {
                                    "elbg"      => self.qmode = QuantMode::ELBG,
                                    "fast"      => self.qmode = QuantMode::Fast,
                                    "mediancut" => self.qmode = QuantMode::MedianCut,
                                    _ => {},
                                };
                            }
                        },
                        "force_v1" => {
                            if let NAValue::Bool(val) = option.value {
                                self.force_v1 = val;
                            }
                        },
                        "refine" => {
                            if let NAValue::Bool(val) = option.value {
                                self.refine = val;
                            }
                        },
                        _ => {},
                    };
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            KEYFRAME_OPTION => Some(NAValue::Int(i64::from(self.key_int))),
            "nstrips" => Some(NAValue::Int(self.nstrips as i64)),
            "quant_mode" => Some(NAValue::String(self.qmode.to_string())),
            "force_v1" => Some(NAValue::Bool(self.force_v1)),
            "refine" => Some(NAValue::Bool(self.refine)),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(CinepakEncoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_codec_support::test::enc_video::*;

    #[test]
    fn test_cinepak_encoder() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        generic_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/UCOD/TalkingHead_352x288.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Misc/TalkingHead_352x288.avi",
                stream_type:    StreamType::Video,
                limit:          Some(2),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "cinepak",
                out_name:       "cinepak.avi",
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  YUV420_FORMAT,
                flipped: true,
                bits:    12,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        let enc_options = &[
                NAOption { name: "quant_mode", value: NAValue::String("mediancut".to_string()) },
            ];
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options,
                          &[0x1d4690c8, 0x3b15b4b3, 0xc2df3c7b, 0x1a25b159]);
    }
}
