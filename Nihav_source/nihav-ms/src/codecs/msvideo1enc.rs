use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

type UnpackedPixel = [u16; 4];

fn map_quality_pal(quality: u8) -> (u32, u32) {
    if quality == 0 {
        (0, 0)
    } else {
        let skip_threshold = (10 - (u32::from(quality) / 10).min(10)) * (8 << 6);
        let fill_threshold = (10 - (u32::from(quality) / 10).min(10)) * (16 << 6);
        (skip_threshold, fill_threshold)
    }
}

fn map_quality_15bit(quality: u8) -> (u32, u32) {
    if quality == 0 {
        (0, 0)
    } else {
        let skip_threshold = (10 - (u32::from(quality) / 10).min(10)) * 8;
        let fill_threshold = (10 - (u32::from(quality) / 10).min(10)) * 16;
        (skip_threshold, fill_threshold)
    }
}

trait PixelOps {
    fn unpack(&self) -> UnpackedPixel;
    fn dist<T: PixelOps>(&self, val: T) -> u32 {
        dist_core(self.unpack(), &val.unpack())
    }
}

impl PixelOps for u16 {
    fn unpack(&self) -> UnpackedPixel {
        let val = *self;
        let r = (val >> 10) & 0x1F;
        let g = (val >>  5) & 0x1F;
        let b =  val        & 0x1F;
        [r, g, b, rgb2y(r, g, b)]
    }
}

fn dist_core(val: UnpackedPixel, other: &UnpackedPixel) -> u32 {
    let sum = val.iter().zip(other.iter()).take(3).fold(0i32,
            |acc, (&a, &b)| {
                let diff = i32::from(a) - i32::from(b);
                acc + diff * diff
            });
    sum as u32
}

fn find_nearest(pix: UnpackedPixel, pal: &[UnpackedPixel; 256]) -> usize {
    let mut bestidx = 0;
    let mut bestdist = std::u32::MAX;

    for (idx, entry) in pal.iter().enumerate() {
        let dist = dist_core(pix, entry);
        if dist == 0 {
            return idx;
        }
        if bestdist > dist {
            bestdist = dist;
            bestidx  = idx;
        }
    }
    bestidx
}

struct LocalSearch {
    pal:        [UnpackedPixel; 256],
    db:         Vec<Vec<UnpackedPixel>>,
}

impl LocalSearch {
    fn quant(key: UnpackedPixel) -> usize {
        (((key[0] >> 3) as usize) << 10) |
        (((key[1] >> 3) as usize) << 5) |
         ((key[2] >> 3) as usize)
    }
    fn new() -> Self {
        let mut db = Vec::with_capacity(1 << 15);
        for _ in 0..(1 << 15) {
            db.push(Vec::new());
        }
        Self {
            pal: [UnpackedPixel::default(); 256],
            db
        }
    }
    fn recalculate(&mut self, pal: &[UnpackedPixel; 256]) {
        self.pal = *pal;
        for vec in self.db.iter_mut() {
            vec.clear();
        }
        for (i, palentry) in pal.iter().enumerate() {
            let r0 = (palentry[0] >> 3) as usize;
            let g0 = (palentry[1] >> 3) as usize;
            let b0 = (palentry[2] >> 3) as usize;
            for r in r0.saturating_sub(1)..=(r0 + 1).min(31) {
                for g in g0.saturating_sub(1)..=(g0 + 1).min(31) {
                    for b in b0.saturating_sub(1)..=(b0 + 1).min(31) {
                        let idx = (r << 10) | (g << 5) | b;
                        self.db[idx].push([palentry[0], palentry[1], palentry[2], i as u16]);
                    }
                }
            }
        }
    }
    fn search(&self, pix: UnpackedPixel) -> usize {
        let idx = Self::quant(pix);
        let mut best_dist = std::u32::MAX;
        let mut best_idx = 0;
        let mut count = 0;
        for clr in self.db[idx].iter() {
            let dist = dist_core(pix, clr);
            count += 1;
            if best_dist > dist {
                best_dist = dist;
                best_idx = clr[3] as usize;
                if dist == 0 { break; }
            }
        }
        if count > 0 {
            best_idx
        } else {
            find_nearest(pix, &self.pal)
        }
    }
}

fn rgb2y(r: u16, g: u16, b: u16) -> u16 {
    (r * 77 + g * 150 + b * 29) >> 8
}

fn pack_rgb555(val: UnpackedPixel) -> u16 {
    (val[0] << 10) | (val[1] << 5) | val[2]
}

#[derive(Default)]
struct PixelAverage {
    sum:    UnpackedPixel,
    count:  u16,
}

impl PixelAverage {
    fn new() -> Self { Self::default() }
    fn add(&mut self, val: &UnpackedPixel) {
        for (dst, &src) in self.sum.iter_mut().zip(val.iter()) {
            *dst += src;
        }
        self.count += 1;
    }
    fn get_avg(&self) -> UnpackedPixel {
        if self.count > 0 {
            let mut ret = self.sum;
            for el in ret.iter_mut() {
                *el /= self.count;
            }
            ret
        } else {
            [0; 4]
        }
    }
}

macro_rules! quant_template {
    ($name:ident, $N:expr) => {
        fn $name(pix: &[UnpackedPixel; $N]) -> ([UnpackedPixel; 2], u16, u32) {
            let mut avg = PixelAverage::new();
            let mut maxv = [0; 4];
            let mut minv = [255; 4];
            for src in pix.iter() {
                avg.add(src);
                for ((maxv, minv), &comp) in maxv.iter_mut().zip(minv.iter_mut()).zip(src.iter()) {
                    *maxv = (*maxv).max(comp);
                    *minv = (*minv).min(comp);
                }
            }
            let avg = avg.get_avg();

            let mut best_axis = 3;
            let mut best_dist = maxv[3] - minv[3];
            for (comp_no, (&minval, &maxval)) in minv.iter().zip(maxv.iter()).enumerate().take(3) {
                if maxval - minval > best_dist {
                    best_axis = comp_no;
                    best_dist = maxval - minval;
                }
            }
            if best_dist == 0 {
                let mut dist = 0;
                for el in pix.iter() {
                    dist += dist_core(avg, el);
                }
                return ([avg; 2], 0, dist);
            }

            let mut avg1 = PixelAverage::new();
            let mut avg2 = PixelAverage::new();
            let mut mask = 0;
            let mut mask_bit = 1;
            for clr in pix.iter() {
                if clr[best_axis] > avg[best_axis] {
                    avg2.add(clr);
                } else {
                    avg1.add(clr);
                    mask |= mask_bit;
                }
                mask_bit <<= 1;
            }

            let clr0 = avg1.get_avg();
            let clr1 = avg2.get_avg();
            let mut dist = 0;
            for clr in pix.iter() {
                let dist0 = dist_core(clr0, clr);
                let dist1 = dist_core(clr1, clr);
                dist += dist0.min(dist1);
            }
            ([clr0, clr1], mask, dist)
        }
    }
}

quant_template!(quant2_16pix, 16);
quant_template!(quant2_4pix, 4);

#[derive(Default)]
struct BlockState {
    fill_dist:  u32,
    fill_val:   UnpackedPixel,
    clr2_dist:  u32,
    clr2_flags: u16,
    clr2:       [UnpackedPixel; 2],
    clr8_dist:  u32,
    clr8_flags: u16,
    clr8:       [[UnpackedPixel; 2]; 4],
    pal_mode:   bool,
}

impl BlockState {
    fn new_pal() -> Self { Self { pal_mode: true, ..Default::default() } }
    fn set_fill_val(&mut self, val: UnpackedPixel) {
        self.fill_val = val;
        if !self.pal_mode {
            self.fill_val[0] &= !1;
        }
    }
    fn calc_clrs(buf: &[UnpackedPixel; 16]) -> (Option<UnpackedPixel>, Option<UnpackedPixel>) {
        let     clr0 = buf[0];
        let mut clr1 = clr0;
        let mut single = true;
        for &pix in buf[1..].iter() {
            if pix != clr0 {
                if single {
                    clr1 = pix;
                    single = false;
                } else if pix != clr1 {
                    return (None, None);
                }
            }
        }
        if !single {
            (Some(clr0), Some(clr1))
        } else {
            (Some(clr0), None)
        }
    }
    fn calc_stats(&mut self, buf: &[UnpackedPixel; 16]) {
        let mut filled = false;
        let mut two_clr = false;
        match Self::calc_clrs(buf) {
            (Some(clr0), Some(clr1)) => {
                self.clr2[0] = clr0;
                self.clr2[1] = clr1;
                two_clr = true;
            },
            (Some(clr0), None) => {
                self.clr2[0] = clr0;
                self.clr2[1] = clr0;
                self.set_fill_val(buf[0]);
                filled = true;
                two_clr = true;
            },
            _ => {},
        };
        self.fill_dist = 0;
        if !filled {
            let mut avg = PixelAverage::new();
            for pix in buf.iter() {
                avg.add(pix);
            }
            self.set_fill_val(avg.get_avg());
            for pix in buf.iter() {
                self.fill_dist += dist_core(self.fill_val, pix);
            }
        }
        if self.fill_dist == 0 {
            self.clr2_dist = std::u32::MAX;
            self.clr8_dist = std::u32::MAX;
            return;
        }

        self.clr2_flags = 0u16;
        if two_clr {
            let mut mask = 1;
            self.clr2_dist = 0;
            for &pix in buf.iter() {
                if pix == self.clr2[0] {
                    self.clr2_flags |= mask;
                } else {
                }
                mask <<= 1;
            }
            if (self.clr2_flags & 0x8000) != 0 {
                self.clr2_flags = !self.clr2_flags;
                self.clr2.swap(0, 1);
            }
        } else {
            let (clrs, mask, dist) = quant2_16pix(buf);
            self.clr2 = clrs;
            self.clr2_flags = mask;
            self.clr2_dist = dist;
            if (self.clr2_flags & 0x8000) != 0 {
                self.clr2_flags = !self.clr2_flags;
                self.clr2.swap(0, 1);
            }
        }
        if self.clr2_dist == 0 {
            self.clr8_dist = std::u32::MAX;
            return;
        }

        self.clr8 = [[UnpackedPixel::default(); 2]; 4];
        self.clr8_flags = 0;
        self.clr8_dist = 0;
        for i in 0..4 {
            let off = (i & 1) * 2 + (i & 2) * 4;
            let src2 = [buf[off], buf[off + 1], buf[off + 4], buf[off + 5]];
            let (clrs, mask, dist) = quant2_4pix(&src2);
            self.clr8[i] = clrs;
            let lo_bits = mask & 0x3;
            let hi_bits = (mask & 0xC) << 2;
            self.clr8_flags |= (hi_bits | lo_bits) << ((i & 1) * 2 + (i & 2) * 4);
            self.clr8_dist += dist;
        }
    }
}

struct BlockPainterPal<'a> {
    ls:    &'a LocalSearch,
}
impl<'a> BlockPainterPal<'a> {
    fn new(ls: &'a LocalSearch) -> Self { Self{ ls } }
    fn find_index(&self, pix: UnpackedPixel) -> u8 { self.ls.search(pix) as u8 }
    fn put_fill(&self, bstate: &BlockState, dst: &mut [u8], dstride: usize) -> u8 {
        let fill_val = self.find_index(bstate.fill_val);
        for line in dst.chunks_mut(dstride) {
            for i in 0..4 {
                line[i] = fill_val;
            }
        }
        fill_val
    }
    fn put_clr2(&self, bstate: &BlockState, dst: &mut [u8], dstride: usize) -> [u8; 2] {
        let clr2 = [self.find_index(bstate.clr2[0]), self.find_index(bstate.clr2[1])];
        for j in 0..4 {
            for i in 0..4 {
                if (bstate.clr2_flags & (1 << (i + j * 4))) == 0 {
                    dst[i + j * dstride] = clr2[0];
                } else {
                    dst[i + j * dstride] = clr2[1];
                }
            }
        }
        clr2
    }
    fn put_clr8(&self, bstate: &BlockState, dst: &mut [u8], dstride: usize) -> [[u8; 4]; 4] {
        let mut clr8 = [[0; 4]; 4];
        for (dst, src) in clr8.iter_mut().zip(bstate.clr8.iter()) {
            for (dst, &src) in dst.iter_mut().zip(src.iter()) {
                *dst = self.find_index(src);
            }
        }
        let mut clr8_flags = bstate.clr8_flags;
        let swap = (clr8_flags & 0x8000) == 0;
        if swap {
            clr8_flags ^= 0xFF00;
        }
        if clr8_flags < 0x9000 {
            clr8_flags |= 0x1000;
        }
        if swap {
            clr8_flags ^= 0xFF00;
        }
        for (j, line) in dst.chunks_mut(dstride).take(4).enumerate() {
            for (i, el) in line.iter_mut().take(4).enumerate() {
                let blk_no = (i >> 1) + (j & 2);
                *el = clr8[blk_no][(!clr8_flags & 1) as usize];
                clr8_flags >>= 1;
            }
        }
        clr8
    }
}

struct BlockWriterPal {}
impl BlockWriterPal {
    fn write_fill(bw: &mut ByteWriter, fill_val: u8) -> EncoderResult<()> {
        bw.write_byte(fill_val)?;
        bw.write_byte(0x80)?;
        Ok(())
    }
    fn write_clr2(bw: &mut ByteWriter, clr2_flags: u16, clr2: [u8; 2]) -> EncoderResult<()> {
        bw.write_u16le(clr2_flags)?;
        bw.write_byte(clr2[0])?;
        bw.write_byte(clr2[1])?;
        Ok(())
    }
    fn write_clr8(bw: &mut ByteWriter, mut clr8_flags: u16, mut clr8: [[u8; 4]; 4]) -> EncoderResult<()> {
        if (clr8_flags & 0x8000) == 0 {
            clr8_flags ^= 0xFF00;
            clr8[2].swap(0, 1);
            clr8[3].swap(0, 1);
        }
        if clr8_flags < 0x9000 {
            clr8_flags |= 0x1000;
        }

        bw.write_u16le(clr8_flags)?;
        bw.write_byte(clr8[0][0])?;
        bw.write_byte(clr8[0][1])?;
        bw.write_byte(clr8[1][0])?;
        bw.write_byte(clr8[1][1])?;
        bw.write_byte(clr8[2][0])?;
        bw.write_byte(clr8[2][1])?;
        bw.write_byte(clr8[3][0])?;
        bw.write_byte(clr8[3][1])?;
        Ok(())
    }
}

struct BlockPainter15 {}
impl BlockPainter15 {
    fn new() -> Self { Self{} }
    fn put_fill(&self, bstate: &BlockState, dst: &mut [u16], dstride: usize) -> u16 {
        let fill_val = pack_rgb555(bstate.fill_val);
        for line in dst.chunks_mut(dstride) {
            for i in 0..4 {
                line[i] = fill_val;
            }
        }
        fill_val
    }
    fn put_clr2(&self, bstate: &BlockState, dst: &mut [u16], dstride: usize) -> [u16; 2] {
        let clr2 = [pack_rgb555(bstate.clr2[0]), pack_rgb555(bstate.clr2[1])];
        for j in 0..4 {
            for i in 0..4 {
                if (bstate.clr2_flags & (1 << (i + j * 4))) == 0 {
                    dst[i + j * dstride] = clr2[0];
                } else {
                    dst[i + j * dstride] = clr2[1];
                }
            }
        }
        clr2
    }
    fn put_clr8(&self, bstate: &BlockState, dst: &mut [u16], dstride: usize) -> [[u16; 4]; 4] {
        let mut clr8 = [[0; 4]; 4];
        for (dst, src) in clr8.iter_mut().zip(bstate.clr8.iter()) {
            for (dst, &src) in dst.iter_mut().zip(src.iter()) {
                *dst = pack_rgb555(src);
            }
        }
        let mut clr8_flags = bstate.clr8_flags;
        for (j, line) in dst.chunks_mut(dstride).take(4).enumerate() {
            for (i, el) in line.iter_mut().take(4).enumerate() {
                let blk_no = (i >> 1) + (j & 2);
                *el = clr8[blk_no][(!clr8_flags & 1) as usize];
                clr8_flags >>= 1;
            }
        }
        clr8
    }
}

struct BlockWriter15 {}
impl BlockWriter15 {
    fn write_fill(bw: &mut ByteWriter, fill_val: u16) -> EncoderResult<()> {
        bw.write_u16le(fill_val | 0x8000)?;
        Ok(())
    }
    fn write_clr2(bw: &mut ByteWriter, clr2_flags: u16, clr2: [u16; 2]) -> EncoderResult<()> {
        bw.write_u16le(clr2_flags)?;
        bw.write_u16le(clr2[0])?;
        bw.write_u16le(clr2[1])?;
        Ok(())
    }
    fn write_clr8(bw: &mut ByteWriter, mut clr8_flags: u16, mut clr8: [[u16; 4]; 4]) -> EncoderResult<()> {
        if (clr8_flags & 0x8000) != 0 {
            clr8_flags ^= 0xFF00;
            clr8[2].swap(0, 1);
            clr8[3].swap(0, 1);
        }
        bw.write_u16le(clr8_flags)?;
        bw.write_u16le(clr8[0][0] | 0x8000)?;
        bw.write_u16le(clr8[0][1])?;
        bw.write_u16le(clr8[1][0])?;
        bw.write_u16le(clr8[1][1])?;
        bw.write_u16le(clr8[2][0])?;
        bw.write_u16le(clr8[2][1])?;
        bw.write_u16le(clr8[3][0])?;
        bw.write_u16le(clr8[3][1])?;
        Ok(())
    }
}

struct MSVideo1Encoder {
    stream:     Option<NAStreamRef>,
    pkt:        Option<NAPacket>,
    pool8:      NAVideoBufferPool<u8>,
    pool15:     NAVideoBufferPool<u16>,
    lastfrm8:   Option<NAVideoBufferRef<u8>>,
    lastfrm15:  Option<NAVideoBufferRef<u16>>,
    quality:    u8,
    frmcount:   u8,
    key_int:    u8,

    pal_mode:   bool,
    pal:        [UnpackedPixel; 256],
    ls:         LocalSearch,
}

impl MSVideo1Encoder {
    fn new() -> Self {
        Self {
            stream:     None,
            pkt:        None,
            pool8:      NAVideoBufferPool::new(2),
            pool15:     NAVideoBufferPool::new(2),
            lastfrm8:   None,
            lastfrm15:  None,
            quality:    0,
            frmcount:   0,
            key_int:    25,

            pal_mode:   false,
            pal:        [UnpackedPixel::default(); 256],
            ls:         LocalSearch::new(),
        }
    }
    fn get_block(src: &[u16], sstride: usize, buf: &mut [UnpackedPixel; 16]) {
        for (line, dst) in src.chunks(sstride).zip(buf.chunks_mut(4)) {
            for (dst, src) in dst.iter_mut().zip(line.iter()) {
                *dst = src.unpack();
            }
        }
    }
    fn get_block8(src: &[u8], sstride: usize, buf: &mut [UnpackedPixel; 16], pal: &[UnpackedPixel; 256]) {
        for (line, dst) in src.chunks(sstride).zip(buf.chunks_mut(4)) {
            for (dst, src) in dst.iter_mut().zip(line.iter()) {
                *dst = pal[usize::from(*src)];
            }
        }
    }
    fn write_skips(bw: &mut ByteWriter, skips: usize) -> EncoderResult<()> {
        bw.write_u16le((skips as u16) | 0x8400)?;
        Ok(())
    }
    fn encode_inter_rgb555(bw: &mut ByteWriter, cur_frm: &mut NAVideoBuffer<u16>, in_frm: &NAVideoBuffer<u16>, prev_frm: &NAVideoBuffer<u16>, quality: u8) -> EncoderResult<bool> {
        let (skip_threshold, fill_threshold) = map_quality_15bit(quality);
        let mut is_intra = true;
        let src = in_frm.get_data();
        let sstride = in_frm.get_stride(0);
        let soff = in_frm.get_offset(0);
        let (w, h) = in_frm.get_dimensions(0);
        let rsrc = prev_frm.get_data();
        let rstride = prev_frm.get_stride(0);
        let roff = prev_frm.get_offset(0);
        let dstride = cur_frm.get_stride(0);
        let doff = cur_frm.get_offset(0);
        let dst = cur_frm.get_data_mut().unwrap();
        let mut skip_run = 0;
        let bpainter = BlockPainter15::new();
        for ((sstrip, rstrip), dstrip) in src[soff..].chunks(sstride * 4).take(h / 4).zip(rsrc[roff..].chunks(rstride * 4)).zip(dst[doff..].chunks_mut(dstride * 4)) {
            for x in (0..w).step_by(4) {
                let mut buf = [UnpackedPixel::default(); 16];
                let mut refbuf = [UnpackedPixel::default(); 16];
                Self::get_block(&sstrip[x..], sstride, &mut buf);
                Self::get_block(&rstrip[x..], rstride, &mut refbuf);

                let mut skip_dist = 0;
                for (pix, rpix) in buf.iter().zip(refbuf.iter()) {
                    skip_dist += dist_core(*rpix, pix);
                }
                if skip_dist <= skip_threshold {
                    skip_run += 1;
                    is_intra = false;
                    for (dst, src) in dstrip[x..].chunks_mut(dstride).zip(rstrip[x..].chunks(rstride)).take(4) {
                        dst[..4].copy_from_slice(&src[..4]);
                    }
                    if skip_run == 1023 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                    continue;
                }

                let mut bstate = BlockState::default();
                bstate.calc_stats(&buf);

                let dst = &mut dstrip[x..];
                if skip_dist <= bstate.fill_dist && skip_dist * 2 <= bstate.clr2_dist {
                    skip_run += 1;
                    is_intra = false;
                    for (dst, src) in dst.chunks_mut(dstride).zip(rstrip[x..].chunks(rstride)).take(4) {
                        dst[..4].copy_from_slice(&src[..4]);
                    }
                    if skip_run == 1023 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                } else if bstate.fill_dist <= fill_threshold ||
                          bstate.fill_dist <= bstate.clr2_dist {
                    let fill_val = bpainter.put_fill(&bstate, dst, dstride);
                    if skip_run != 0 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                    BlockWriter15::write_fill(bw, fill_val)?;
                } else if bstate.clr8_dist < bstate.clr2_dist {
                    let clr8 = bpainter.put_clr8(&bstate, dst, dstride);
                    if skip_run != 0 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                    BlockWriter15::write_clr8(bw, bstate.clr8_flags, clr8)?;
                } else {
                    let clr2 = bpainter.put_clr2(&bstate, dst, dstride);
                    if skip_run != 0 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                    BlockWriter15::write_clr2(bw, bstate.clr2_flags, clr2)?;
                }
            }
        }
        if skip_run != 0 {
            Self::write_skips(bw, skip_run)?;
        }
        if is_intra {
            bw.write_u16le(0)?;
        } //xxx: something for inter?
        Ok(is_intra)
    }
    fn encode_intra_rgb555(bw: &mut ByteWriter, cur_frm: &mut NAVideoBuffer<u16>, in_frm: &NAVideoBuffer<u16>, quality: u8) -> EncoderResult<bool> {
        let (_, fill_threshold) = map_quality_15bit(quality);
        let src = in_frm.get_data();
        let sstride = in_frm.get_stride(0);
        let soff = in_frm.get_offset(0);
        let (w, h) = in_frm.get_dimensions(0);
        let dstride = cur_frm.get_stride(0);
        let doff = cur_frm.get_offset(0);
        let dst = cur_frm.get_data_mut().unwrap();
        let bpainter = BlockPainter15::new();
        for (sstrip, dstrip) in src[soff..].chunks(sstride * 4).take(h / 4).zip(dst[doff..].chunks_mut(dstride * 4)) {
            for x in (0..w).step_by(4) {
                let mut buf = [UnpackedPixel::default(); 16];
                Self::get_block(&sstrip[x..], sstride, &mut buf);
                let mut bstate = BlockState::default();
                bstate.calc_stats(&buf);

                let dst = &mut dstrip[x..];
                if bstate.fill_dist <= fill_threshold ||
                   bstate.fill_dist <= bstate.clr2_dist {
                    let fill_val = bpainter.put_fill(&bstate, dst, dstride);
                    BlockWriter15::write_fill(bw, fill_val)?;
                } else if bstate.clr8_dist < bstate.clr2_dist {
                    let clr8 = bpainter.put_clr8(&bstate, dst, dstride);
                    BlockWriter15::write_clr8(bw, bstate.clr8_flags, clr8)?;
                } else {
                    let clr2 = bpainter.put_clr2(&bstate, dst, dstride);
                    BlockWriter15::write_clr2(bw, bstate.clr2_flags, clr2)?;
                }
            }
        }
        bw.write_u16le(0)?;
        Ok(true)
    }
    fn encode_inter_pal(bw: &mut ByteWriter, cur_frm: &mut NAVideoBuffer<u8>, in_frm: &NAVideoBuffer<u8>, prev_frm: &NAVideoBuffer<u8>, quality: u8, pal: &[UnpackedPixel; 256], ls: &LocalSearch) -> EncoderResult<bool> {
        let (skip_threshold, fill_threshold) = map_quality_pal(quality);
        let mut is_intra = true;
        let src = in_frm.get_data();
        let sstride = in_frm.get_stride(0);
        let soff = in_frm.get_offset(0);
        let (w, h) = in_frm.get_dimensions(0);
        let rsrc = prev_frm.get_data();
        let rstride = prev_frm.get_stride(0);
        let roff = prev_frm.get_offset(0);
        let dstride = cur_frm.get_stride(0);
        let doff = cur_frm.get_offset(0);
        let dst = cur_frm.get_data_mut().unwrap();
        let mut skip_run = 0;
        let bpainter = BlockPainterPal::new(ls);
        for ((sstrip, rstrip), dstrip) in src[soff..].chunks(sstride * 4).take(h / 4).zip(rsrc[roff..].chunks(rstride * 4)).zip(dst[doff..].chunks_mut(dstride * 4)) {
            for x in (0..w).step_by(4) {
                let mut buf = [UnpackedPixel::default(); 16];
                let mut refbuf = [UnpackedPixel::default(); 16];
                Self::get_block8(&sstrip[x..], sstride, &mut buf, pal);
                Self::get_block8(&rstrip[x..], rstride, &mut refbuf, pal);

                let mut skip_dist = 0;
                for (pix, rpix) in buf.iter().zip(refbuf.iter()) {
                    skip_dist += dist_core(*rpix, pix);
                }
                if skip_dist <= skip_threshold {
                    skip_run += 1;
                    is_intra = false;
                    for (dst, src) in dstrip[x..].chunks_mut(dstride).zip(rstrip[x..].chunks(rstride)).take(4) {
                        dst[..4].copy_from_slice(&src[..4]);
                    }
                    if skip_run == 1023 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                    continue;
                }

                let mut bstate = BlockState::new_pal();
                bstate.calc_stats(&buf);

                let dst = &mut dstrip[x..];
                if skip_dist <= bstate.fill_dist && skip_dist * 2 <= bstate.clr2_dist {
                    skip_run += 1;
                    is_intra = false;
                    for (dst, src) in dst.chunks_mut(dstride).zip(rstrip[x..].chunks(rstride)).take(4) {
                        dst[..4].copy_from_slice(&src[..4]);
                    }
                    if skip_run == 1023 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                } else if bstate.fill_dist <= fill_threshold ||
                          bstate.fill_dist <= bstate.clr2_dist {
                    let fill_val = bpainter.put_fill(&bstate, dst, dstride);
                    if skip_run != 0 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                    BlockWriterPal::write_fill(bw, fill_val)?;
                } else if bstate.clr8_dist < bstate.clr2_dist {
                    let clr8 = bpainter.put_clr8(&bstate, dst, dstride);
                    if skip_run != 0 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                    BlockWriterPal::write_clr8(bw, bstate.clr8_flags, clr8)?;
                } else {
                    let clr2 = bpainter.put_clr2(&bstate, dst, dstride);
                    if skip_run != 0 {
                        Self::write_skips(bw, skip_run)?;
                        skip_run = 0;
                    }
                    BlockWriterPal::write_clr2(bw, bstate.clr2_flags, clr2)?;
                }
            }
        }
        if skip_run != 0 {
            Self::write_skips(bw, skip_run)?;
        }
        if is_intra {
            bw.write_u16le(0)?;
        } //xxx: something for inter?
        Ok(is_intra)
    }
    fn encode_intra_pal(bw: &mut ByteWriter, cur_frm: &mut NAVideoBuffer<u8>, in_frm: &NAVideoBuffer<u8>, quality: u8, pal: &[UnpackedPixel; 256], ls: &LocalSearch) -> EncoderResult<bool> {
        let (_, fill_threshold) = map_quality_pal(quality);
        let src = in_frm.get_data();
        let sstride = in_frm.get_stride(0);
        let soff = in_frm.get_offset(0);
        let (w, h) = in_frm.get_dimensions(0);
        let dstride = cur_frm.get_stride(0);
        let doff = cur_frm.get_offset(0);
        let dst = cur_frm.get_data_mut().unwrap();
        let bpainter = BlockPainterPal::new(ls);
        for (sstrip, dstrip) in src[soff..].chunks(sstride * 4).take(h / 4).zip(dst[doff..].chunks_mut(dstride * 4)) {
            for x in (0..w).step_by(4) {
                let mut buf = [UnpackedPixel::default(); 16];
                Self::get_block8(&sstrip[x..], sstride, &mut buf, pal);
                let mut bstate = BlockState::new_pal();
                bstate.calc_stats(&buf);

                let dst = &mut dstrip[x..];
                if bstate.fill_dist <= fill_threshold ||
                   bstate.fill_dist <= bstate.clr2_dist {
                    let fill_val = bpainter.put_fill(&bstate, dst, dstride);
                    BlockWriterPal::write_fill(bw, fill_val)?;
                } else if bstate.clr8_dist < bstate.clr2_dist {
                    let clr8 = bpainter.put_clr8(&bstate, dst, dstride);
                    BlockWriterPal::write_clr8(bw, bstate.clr8_flags, clr8)?;
                } else {
                    let clr2 = bpainter.put_clr2(&bstate, dst, dstride);
                    BlockWriterPal::write_clr2(bw, bstate.clr2_flags, clr2)?;
                }
            }
        }
        bw.write_u16le(0)?;
        Ok(true)
    }
}

const RGB555_FORMAT: NAPixelFormaton = NAPixelFormaton {
        model: ColorModel::RGB(RGBSubmodel::RGB), components: 3,
        comp_info: [
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift: 10, comp_offs: 0, next_elem: 2 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  5, comp_offs: 0, next_elem: 2 }),
            Some(NAPixelChromaton{ h_ss: 0, v_ss: 0, packed: true, depth: 5, shift:  0, comp_offs: 0, next_elem: 2 }),
            None, None],
        elem_size: 2, be: false, alpha: false, palette: false };

impl NAEncoder for MSVideo1Encoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, true, RGB555_FORMAT)),
                    ..Default::default() })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                let oformat = if vinfo.format == PAL8_FORMAT { PAL8_FORMAT } else { RGB555_FORMAT };
                let outinfo = NAVideoInfo::new((vinfo.width + 3) & !3, (vinfo.height + 3) & !3, true, oformat);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_SKIPFRAME }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                if vinfo.format != RGB555_FORMAT && vinfo.format != PAL8_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if ((vinfo.width | vinfo.height) & 3) != 0 {
                    return Err(EncoderError::FormatError);
                }

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, true, vinfo.format);
                let info = NACodecInfo::new("msvideo1", NACodecTypeInfo::Video(out_info), None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();
                self.pal_mode = vinfo.format.is_paletted();

                if !self.pal_mode {
                    if self.pool15.prealloc_video(out_info, 2).is_err() {
                        return Err(EncoderError::AllocError);
                    }
                } else {
                    if self.pool8.prealloc_video(out_info, 2).is_err() {
                        return Err(EncoderError::AllocError);
                    }
                }

                self.stream = Some(stream.clone());
                self.quality = encinfo.quality;

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        if frm.frame_type == FrameType::Skip {
            if let Some(ref stream) = self.stream {
                let mut dbuf = Vec::with_capacity(4);
                let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
                let mut bw   = ByteWriter::new(&mut gw);

                let vinfo = stream.get_info().get_properties().get_video_info().unwrap();
                let mut nskips = ((vinfo.get_width() + 3) / 4) * ((vinfo.get_height() + 3) / 4);
                while nskips >= 1023 {
                    Self::write_skips(&mut bw, 1023)?;
                    nskips -= 1023;
                }
                if nskips > 0 {
                    Self::write_skips(&mut bw, nskips)?;
                }
                self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, false, dbuf));
                self.frmcount += 1;
                if self.frmcount == self.key_int {
                    self.frmcount = 0;
                }
                return Ok(());
            } else {
                return Err(EncoderError::Bug);
            }
        }
        if let Some(ref vbuf) = buf.get_vbuf16() {
            if self.pal_mode {
                return Err(EncoderError::InvalidParameters);
            }
            let mut cur_frm = self.pool15.get_free().unwrap();
            let mut dbuf = Vec::with_capacity(4);
            let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
            let mut bw   = ByteWriter::new(&mut gw);
            if self.frmcount == 0 {
                self.lastfrm15 = None;
            }
            let is_intra = if let Some(ref prev_buf) = self.lastfrm15 {
                    Self::encode_inter_rgb555(&mut bw, &mut cur_frm, vbuf, prev_buf, self.quality)?
                } else {
                    Self::encode_intra_rgb555(&mut bw, &mut cur_frm, vbuf, self.quality)?
                };
            self.lastfrm15 = Some(cur_frm);
            self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, is_intra, dbuf));
            self.frmcount += 1;
            if self.frmcount == self.key_int {
                self.frmcount = 0;
            }
            Ok(())
        } else if let Some(ref vbuf) = buf.get_vbuf() {
            if !self.pal_mode {
                return Err(EncoderError::InvalidParameters);
            }
            let src = vbuf.get_data();
            let pal = &src[vbuf.get_offset(1)..];
            let mut pal_changed = false;
            for (cur_pal, new_pal) in self.pal.iter_mut().zip(pal.chunks_exact(3)) {
                let (cur_clr, luma) = cur_pal.split_at_mut(3);
                let new_clr = [u16::from(new_pal[0]), u16::from(new_pal[1]), u16::from(new_pal[2])];
                if cur_clr != new_clr {
                    pal_changed = true;
                    cur_clr.copy_from_slice(&new_clr);
                    luma[0] = rgb2y(cur_clr[0], cur_clr[1], cur_clr[2]);
                }
            }

            if pal_changed {
                self.ls.recalculate(&self.pal);
            }

            let mut cur_frm = self.pool8.get_free().unwrap();
            let mut dbuf = Vec::with_capacity(4);
            let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
            let mut bw   = ByteWriter::new(&mut gw);
            if self.frmcount == 0 {
                self.lastfrm8 = None;
            }
            let is_intra = if let Some(ref prev_buf) = self.lastfrm8 {
                    Self::encode_inter_pal(&mut bw, &mut cur_frm, vbuf, prev_buf, self.quality, &self.pal, &self.ls)?
                } else {
                    Self::encode_intra_pal(&mut bw, &mut cur_frm, vbuf, self.quality, &self.pal, &self.ls)?
                };
            self.lastfrm8 = Some(cur_frm);
            let mut pkt = NAPacket::new(self.stream.clone().unwrap(), frm.ts, is_intra, dbuf);
            if pal_changed {
                let mut epal = [0; 1024];
                for (dst, src) in epal.chunks_mut(4).zip(self.pal.iter()) {
                    dst[0] = src[0] as u8;
                    dst[1] = src[1] as u8;
                    dst[2] = src[2] as u8;
                }
                pkt.add_side_data(NASideData::Palette(true, Arc::new(epal)));
            }
            self.pkt = Some(pkt);
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
];

impl NAOptionHandler for MSVideo1Encoder {
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
                        _ => {},
                    };
                }
            }
        }
    }
    fn query_option_value(&self, name: &str) -> Option<NAValue> {
        match name {
            KEYFRAME_OPTION => Some(NAValue::Int(i64::from(self.key_int))),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(MSVideo1Encoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_commonfmt::*;
    use nihav_codec_support::test::enc_video::*;
    use super::RGB555_FORMAT;

    #[test]
    fn test_ms_video1_encoder_pal() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        ms_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        ms_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/RLE/mplayer-msrle-4bit.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/MS/mplayer-msrle-4bit.avi",
                stream_type:    StreamType::Video,
                limit:          Some(3),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "msvideo1",
                out_name:       "msvideo1pal.avi",
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  PAL8_FORMAT,
                flipped: true,
                bits:    8,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, &[]);
        test_encoding_md5(&dec_config, &enc_config, enc_params, &[],
                          &[0x5afaf853, 0xd53ba9dd, 0x630f53e7, 0x41b33a36]);
    }

    #[test]
    fn test_ms_video1_encoder_rgb555() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);
        ms_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        ms_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/UCOD/TalkingHead_352x288.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Misc/TalkingHead_352x288.avi",
                stream_type:    StreamType::Video,
                limit:          Some(3),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "msvideo1",
                out_name:       "msvideo1.avi",
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  RGB555_FORMAT,
                flipped: true,
                bits:    16,
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 80,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, &[]);
        test_encoding_md5(&dec_config, &enc_config, enc_params, &[],
                          &[0xb3175a7b, 0x4a6cb45e, 0x526f3f5d, 0xaa1574cc]);
    }
}
