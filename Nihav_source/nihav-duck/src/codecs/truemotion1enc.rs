use std::ops::*;
use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use super::truemotion1data::*;

#[derive(Clone,Copy,PartialEq,Default)]
enum BlockMode {
    FourByFour,
    #[default]
    TwoByTwo,
    FourByTwo,
    TwoByFour,
}

impl FromStr for BlockMode {
    type Err = ();
    fn from_str(s: &str) -> Result<BlockMode, Self::Err> {
        match s {
            "4x4" => Ok(BlockMode::FourByFour),
            "2x4" => Ok(BlockMode::TwoByFour),
            "4x2" => Ok(BlockMode::FourByTwo),
            "2x2" => Ok(BlockMode::TwoByTwo),
            _ => Err(()),
        }
    }
}

impl ToString for BlockMode {
    fn to_string(&self) -> String {
        match *self {
            BlockMode::FourByFour => "4x4".to_string(),
            BlockMode::FourByTwo  => "4x2".to_string(),
            BlockMode::TwoByFour  => "2x4".to_string(),
            BlockMode::TwoByTwo   => "2x2".to_string(),
        }
    }
}

#[derive(Clone,Copy,PartialEq,Default)]
enum OptionMode {
    Off,
    On,
    #[default]
    Auto,
}

impl FromStr for OptionMode {
    type Err = ();
    fn from_str(s: &str) -> Result<OptionMode, Self::Err> {
        match s {
            "off"  => Ok(OptionMode::Off),
            "on"   => Ok(OptionMode::On),
            "auto" => Ok(OptionMode::Auto),
            _ => Err(()),
        }
    }
}

impl ToString for OptionMode {
    fn to_string(&self) -> String {
        match *self {
            OptionMode::Off  => "off".to_string(),
            OptionMode::On   => "on".to_string(),
            OptionMode::Auto => "auto".to_string(),
        }
    }
}

#[derive(Default,Clone,Copy,PartialEq)]
struct WorkPixel ([i16; 3]);

impl WorkPixel {
    fn decorr(self) -> Self {
        WorkPixel([self.0[0] - self.0[1], self.0[1], self.0[2] - self.0[1]])
    }
    fn recon(self) -> Self {
        WorkPixel([self.0[0] + self.0[1], self.0[1], self.0[2] + self.0[1]])
    }
    fn check16(self) -> bool {
        let full = self.recon();
        for comp in full.0.iter() {
            if !(0i16..32i16).contains(comp) {
                return false;
            }
        }
        true
    }
    fn clear_chroma(&mut self) {
        self.0[0] = 0;
        self.0[2] = 0;
    }
    fn avg(mut self, other: Self) -> Self {
        self += other;
        for el in self.0.iter_mut() {
            *el = (*el + 1) >> 1;
        }
        self
    }
}

impl std::fmt::Display for WorkPixel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "({:3},{:3},{:3})", self.0[0], self.0[1], self.0[2])
    }
}

impl Add for WorkPixel {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        WorkPixel([self.0[0] + rhs.0[0], self.0[1] + rhs.0[1], self.0[2] + rhs.0[2]])
    }
}
impl AddAssign for WorkPixel {
    fn add_assign(&mut self, rhs: Self) {
        self.0[0] += rhs.0[0];
        self.0[1] += rhs.0[1];
        self.0[2] += rhs.0[2];
    }
}
impl Sub for WorkPixel {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        WorkPixel([self.0[0] - rhs.0[0], self.0[1] - rhs.0[1], self.0[2] - rhs.0[2]])
    }
}
impl SubAssign for WorkPixel {
    fn sub_assign(&mut self, rhs: Self) {
        self.0[0] -= rhs.0[0];
        self.0[1] -= rhs.0[1];
        self.0[2] -= rhs.0[2];
    }
}

fn load16(frm: &mut [WorkPixel], width: usize, vbuf: &NAVideoBuffer<u16>) {
    let stride = vbuf.get_stride(0);
    let src = vbuf.get_data();

    for (dline, sline) in frm.chunks_exact_mut(width).zip(
                src[vbuf.get_offset(0)..].chunks_exact(stride)) {
        for (dst, &src) in dline.iter_mut().zip(sline.iter()) {
            let el = (src & 0x7FFF) as i16;
            *dst = WorkPixel([el >> 10, (el >> 5) & 0x1F, el & 0x1F]).decorr();
        }
    }
}

fn load24(frm: &mut [WorkPixel], width: usize, vbuf: &NAVideoBuffer<u8>) {
    let stride = vbuf.get_stride(0);
    let src = vbuf.get_data();

    for (dline, sline) in frm.chunks_exact_mut(width).zip(
                src[vbuf.get_offset(0)..].chunks_exact(stride)) {
        for (dst, src) in dline.iter_mut().zip(sline.chunks_exact(8)) {
            let pix1 = WorkPixel([i16::from(src[0]), i16::from(src[1]), i16::from(src[2])]);
            let pix2 = WorkPixel([i16::from(src[4]), i16::from(src[5]), i16::from(src[6])]);
            *dst = pix1.avg(pix2);
        }
    }
}

const MAX_DIFF: i32 = std::i32::MAX;

fn luma15_delta(pix: &mut WorkPixel, pred: WorkPixel, tgt: WorkPixel, deltas: &[i32; 8], fat_deltas: &[i32; 8]) {
    let mut tpix = *pix;
    let tgt_val = i32::from(tgt.0[1]);
    let base_val = i32::from(pred.0[1]);
    let mut best_delta = 0;
    let mut best_diff = MAX_DIFF;
    if i32::from(pix.0[1].abs()) <= deltas[deltas.len() - 2] {
        for &delta in deltas.iter() {
            let diff = (base_val + delta - tgt_val) * (base_val + delta - tgt_val);
            if diff < best_diff {
                tpix.0[1] = delta as i16;
                if (tpix + pred).check16() {
                    best_diff = diff;
                    best_delta = delta as i16;
                    if diff == 0 {
                        break;
                    }
                }
            }
        }
    }
    if best_diff == MAX_DIFF {
        for &f_delta in fat_deltas.iter().skip(1) {
            for &delta1 in deltas.iter() {
                let delta = delta1 + f_delta;
                let diff = (base_val + delta - tgt_val) * (base_val + delta - tgt_val);
                if diff < best_diff {
                    tpix.0[1] = delta as i16;
                    if (tpix + pred).check16() {
                        best_diff = diff;
                        best_delta = delta as i16;
                        if diff == 0 {
                            break;
                        }
                    }
                }
            }
        }
    }
    pix.0[1] = best_delta;
}

fn chroma15_deltas(pix: &mut [WorkPixel; 2], pred: &[WorkPixel; 2], tgt: &[WorkPixel], component: usize, deltas: &[i32; 8], fat_deltas: &[i32; 8]) {
    let mut tpix = *pix;
    let pred_val = [i32::from(pred[0].0[component]), i32::from(pred[1].0[component])];
    let tgt_val  = [i32::from(tgt[0].0[component]),  i32::from(tgt[1].0[component])];

    let mut best_delta = 0;
    let mut best_diff = MAX_DIFF;

    let delta_thr = deltas[deltas.len() - 2];
    if i32::from(pix[0].0[component].abs()) <= delta_thr &&
       i32::from(pix[1].0[component].abs()) <= delta_thr {
        for &delta in deltas.iter() {
            let diffs = [pred_val[0] + delta - tgt_val[0], pred_val[1] + delta - tgt_val[1]];
            let diff = diffs[0] * diffs[0] + diffs[1] * diffs[1];
            if diff < best_diff {
                tpix[0].0[component] = delta as i16;
                tpix[1].0[component] = delta as i16;
                if (tpix[0] + pred[0]).check16() && (tpix[1] + pred[1]).check16() {
                    best_diff = diff;
                    best_delta = delta as i16;
                    if diff == 0 {
                        break;
                    }
                }
            }
        }
    }
    if best_diff == MAX_DIFF {
        for &f_delta in fat_deltas.iter().skip(1) {
            for &delta1 in deltas.iter() {
                let delta = delta1 + f_delta;
                let diffs = [pred_val[0] + delta - tgt_val[0], pred_val[1] + delta - tgt_val[1]];
                let diff = diffs[0] * diffs[0] + diffs[1] * diffs[1];
                if diff < best_diff {
                    tpix[0].0[component] = delta as i16;
                    tpix[1].0[component] = delta as i16;
                    if (tpix[0] + pred[0]).check16() && (tpix[1] + pred[1]).check16() {
                        best_diff = diff;
                        best_delta = delta as i16;
                        if diff == 0 {
                            break;
                        }
                    }
                }
            }
        }
    }
    pix[0].0[component] = best_delta;
    pix[1].0[component] = best_delta;
}

#[derive(Default)]
struct MaskWriter {
    data:       Vec<u8>,
    buf:        u8,
    pos:        u8,
}

impl MaskWriter {
    fn new() -> Self { Self::default() }
    fn reset(&mut self) {
        self.data.clear();
        self.buf = 0;
        self.pos = 0;
    }
    fn write_bit(&mut self, val: bool) {
        if val {
            self.buf |= 1 << self.pos;
        }
        self.pos += 1;
        if self.pos == 8 {
            self.data.push(self.buf);
            self.buf = 0;
            self.pos = 0;
        }
    }
    fn flush(&mut self) {
        if self.pos != 0 {
            self.data.push(self.buf);
        }
    }
}

struct IndexWriter {
    dst:    Vec<u8>,
    in_seq: [u8; 4],
    in_len: usize,
    cand:   u8,
    table:  &'static [u8],
}

enum SearchResult {
    Delta(u8),
    Escape(u8, u8),
}

impl IndexWriter {
    fn new() -> Self {
        Self {
            dst:    Vec::new(),
            in_seq: [0; 4],
            in_len: 0,
            cand:   0,
            table:  DUCK_VECTABLES[0],
        }
    }
    fn reset(&mut self, idx: usize) {
        self.dst.clear();
        self.in_len = 0;
        self.table = DUCK_VECTABLES[idx];
    }
    fn flush(&mut self) {
        if self.in_len > 0 {
            let idx = self.find(self.in_len).unwrap();
            assert_eq!(idx, self.cand);
            self.dst.push(self.cand);
            self.in_len = 0;
        }
    }
    fn find(&self, len: usize) -> Option<u8> {
        let mut cur_idx = 0u8;
        let src = self.table;
        let mut cb_pos = 0;
        while cb_pos < src.len() {
            let entry_len = usize::from(src[cb_pos]) / 2;
            cb_pos += 1;
            if entry_len < len {
                break;
            }
            if entry_len == len && src[cb_pos..][..len] == self.in_seq[..len] {
                return Some(cur_idx.max(1));
            }
            cb_pos += entry_len;
            cur_idx += 1;
        }
        None
    }
    fn find_delta(dval: i16, deltas: &[i32; 8], fat_deltas: &[i32; 8]) -> SearchResult {
        if let Some(pos) = deltas.iter().position(|&x| x == i32::from(dval)) {
            SearchResult::Delta(pos as u8)
        } else {
            for (i, &delta1) in deltas.iter().enumerate() {
                for (j, &f_delta) in fat_deltas.iter().enumerate().skip(1) {
                    let delta = delta1 + f_delta;
                    if delta == i32::from(dval) {
                        return SearchResult::Escape(i as u8, j as u8);
                    }
                }
            }
            unreachable!()
        }
    }
    fn add_deltas(&mut self, d0: i16, d1: i16, deltas: &[i32; 8], fat_deltas: &[i32; 8]) {
        let idx0 = Self::find_delta(d0, deltas, fat_deltas);
        let idx1 = Self::find_delta(d1, deltas, fat_deltas);
        match (idx0, idx1) {
            (SearchResult::Delta(i0), SearchResult::Delta(i1)) => {
                self.add(i0, i1);
            },
            (SearchResult::Delta(i0), SearchResult::Escape(i10, i11)) => {
                self.add(i0, i10);
                self.add_byte(0);
                self.add(0, i11);
            },
            (SearchResult::Escape(i00, i01), SearchResult::Delta(i1)) => {
                self.add(i00, i1);
                self.add_byte(0);
                self.add(i01, 0);
            },
            (SearchResult::Escape(i00, i01), SearchResult::Escape(i10, i11)) => {
                self.add(i00, i10);
                self.add_byte(0);
                self.add(i01, i11);
            },
        }
    }
    fn add(&mut self, idx0: u8, idx1: u8) {
        let pair = idx0 * 16 + idx1;
        self.in_seq[self.in_len] = pair;
        self.in_len += 1;
        match self.find(self.in_len) {
            None => {
                self.dst.push(self.cand);
                self.in_seq[0] = self.in_seq[self.in_len - 1];
                self.in_len = 1;
                self.cand = self.find(self.in_len).unwrap();
            },
            Some(idx) if self.in_len == 4 => {
                self.dst.push(idx);
                self.in_len = 0;
            },
            Some(idx) => {
                self.cand = idx;
            }
        }
    }
    fn add_byte(&mut self, val: u8) {
        self.flush();
        self.dst.push(val);
    }
}

struct TM1Encoder {
    stream:     Option<NAStreamRef>,
    pkt:        Option<NAPacket>,
    top_line:   Vec<WorkPixel>,
    skip_map:   Vec<bool>,
    frame:      Vec<WorkPixel>,
    prev_frame: Vec<WorkPixel>,
    width:      usize,
    height:     usize,
    is16:       bool,
    mask:       MaskWriter,
    idx_wr:     IndexWriter,
    tm1type:    usize,
    block_mode: BlockMode,
    delta_set:  usize,
    table_idx:  usize,
    delta_mode: OptionMode,
    table_mode: OptionMode,
    frameno:    usize,
    key_int:    usize,
}

impl TM1Encoder {
    fn new() -> Self {
        Self {
            stream:     None,
            pkt:        None,
            top_line:   Vec::new(),
            skip_map:   Vec::new(),
            frame:      Vec::new(),
            prev_frame: Vec::new(),
            width:      0,
            height:     0,
            is16:       false,
            mask:       MaskWriter::new(),
            idx_wr:     IndexWriter::new(),
            tm1type:    0,
            block_mode: BlockMode::default(),
            delta_set:  0,
            table_idx:  0,
            delta_mode: OptionMode::Off,
            table_mode: OptionMode::Off,
            frameno:    0,
            key_int:    10,
        }
    }
    fn encode_16(&mut self, is_intra: bool) -> EncoderResult<()> {
        let (blk_w, blk_h) = if let Some(ref info) = TM1_COMPR_TYPES[self.tm1type] {
                (info.block_w, info.block_h)
            } else {
                unreachable!();
            };
        self.mask.reset();
        self.idx_wr.reset(self.table_idx);
        for el in self.top_line.iter_mut() {
            *el = WorkPixel::default();
        }

        let y_deltas  = &DUCK_Y_DELTAS  [self.delta_set];
        let yf_deltas = &DUCK_Y_DELTAS_5[self.delta_set];
        let c_deltas  = &DUCK_C_DELTAS  [self.delta_set];
        let cf_deltas = &DUCK_C_DELTAS_5[self.delta_set];

        let mut has_c = [[false; 2]; 4];
        has_c[0][0] = true;
        if blk_w == 2 {
            has_c[0][1] = true;
        }
        if blk_h == 2 {
            has_c[2] = has_c[0];
        }

        if !is_intra {
            self.skip_map.clear();
            for (stripe, pstripe) in self.frame.chunks_exact(self.width * 4).zip(
                    self.prev_frame.chunks_exact(self.width * 4)) {
                for x in (0..self.width).step_by(4) {
                    let mut skip = true;
                    'cmp_loop: for (line, pline) in stripe[x..].chunks(self.width).zip(
                            pstripe[x..].chunks(self.width)) {
                        for (&a, &b) in line[..4].iter().zip(pline[..4].iter()) {
                            if a != b {
                                skip = false;
                                break 'cmp_loop;
                            }
                        }
                    }
                    self.mask.write_bit(skip);
                    self.skip_map.push(skip);
                }
            }
        }

        let mut skip_blk_addr = 0;
        for (y, line) in self.frame.chunks_exact_mut(self.width).enumerate() {
            let mut hpred = [WorkPixel::default(); 2];

            for (x, (pair, vpred)) in line.chunks_exact_mut(2).zip(
                    self.top_line.chunks_exact_mut(2)).enumerate() {
                if !is_intra && self.skip_map[skip_blk_addr + x / 2] {
                    pair[0] = self.prev_frame[x * 2     + y * self.width];
                    pair[1] = self.prev_frame[x * 2 + 1 + y * self.width];
                    hpred[0] = pair[0] - vpred[0];
                    hpred[1] = pair[1] - vpred[1];
                    vpred.copy_from_slice(pair);
                    continue;
                }
                let pval = [vpred[0] + hpred[0], vpred[1] + hpred[1]];
                let mut deltas = [pair[0] - pval[0], pair[1] - pval[1]];
                if has_c[y & 3][x & 1] {
                    chroma15_deltas(&mut deltas, &pval, pair, 0, c_deltas, cf_deltas);
                    chroma15_deltas(&mut deltas, &pval, pair, 2, c_deltas, cf_deltas);
                    self.idx_wr.add_deltas(deltas[0].0[0], deltas[0].0[2], c_deltas, cf_deltas);
                } else {
                    deltas[0].clear_chroma();
                    deltas[1].clear_chroma();
                }
                luma15_delta(&mut deltas[0], pval[0], pair[0], y_deltas, yf_deltas);
                luma15_delta(&mut deltas[1], pval[1], pair[1], y_deltas, yf_deltas);
                self.idx_wr.add_deltas(deltas[0].0[1], deltas[1].0[1], y_deltas, yf_deltas);

                hpred[0] += deltas[0];
                hpred[1] += deltas[1];
                vpred[0] += hpred[0];
                vpred[1] += hpred[1];
                pair[0] = vpred[0];
                pair[1] = vpred[1];
            }

            if (y & 3) == 3 {
                skip_blk_addr += self.width / 4;
            }
        }

        Ok(())
    }
}

impl NAEncoder for TM1Encoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                        format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, true, BGR0_FORMAT)),
                        ..Default::default()
                    })
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                /*let in_fmt = vinfo.format;
                let pix_fmt = if in_fmt.model.is_rgb() && in_fmt.get_total_depth() <= 16 {
                        RGB555_FORMAT
                    } else {
                        BGR0_FORMAT
                    };*/
                let pix_fmt = RGB555_FORMAT;
                let outinfo = NAVideoInfo::new((vinfo.width + 3) & !3, (vinfo.height + 3) & !3, false, pix_fmt);
                let mut ofmt = *encinfo;
                ofmt.format = NACodecTypeInfo::Video(outinfo);
                Ok(ofmt)
            }
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_PARAMCHANGE }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(vinfo) => {
                if vinfo.format != BGR0_FORMAT && vinfo.format != RGB555_FORMAT {
                    return Err(EncoderError::FormatError);
                }
                if vinfo.format == BGR0_FORMAT {
                    return Err(EncoderError::NotImplemented);
                }
                if ((vinfo.width | vinfo.height) & 3) != 0 {
                    return Err(EncoderError::FormatError);
                }
                if (vinfo.width | vinfo.height) >= (1 << 10) {
                    return Err(EncoderError::FormatError);
                }

                let out_info = NAVideoInfo::new(vinfo.width, vinfo.height, false, vinfo.format);
                let info = NACodecInfo::new("truemotion1", NACodecTypeInfo::Video(out_info), None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();

                self.stream = Some(stream.clone());

                Ok(stream)
            },
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();

        let mut meta_type = 2;
        let mut is_intra = meta_type == 1 || self.frameno == 0;

        if let Some(vinfo) = buf.get_video_info() {
            if vinfo.width != self.width || vinfo.height != self.height {
                self.top_line.resize(vinfo.width, WorkPixel::default());
                self.skip_map.resize((vinfo.width / 4) * (vinfo.height / 4), false);
                self.frame.resize(vinfo.width * vinfo.height, WorkPixel::default());
                self.prev_frame.resize(vinfo.width * vinfo.height, WorkPixel::default());
                self.width  = vinfo.width;
                self.height = vinfo.height;
                is_intra = true;
                if meta_type == 1 && self.width < 213 && self.height >= 176 {
                    // switch to a newer version in order to avoid being an interpolated frame
                    meta_type = 2;
                }
            }
        } else {
            return Err(EncoderError::InvalidParameters);
        }

        let old_is16 = self.is16;
        match buf {
            NABufferType::Video(ref vbuf) | NABufferType::VideoPacked(ref vbuf) => {
                self.is16 = false;
                load24(&mut self.frame, self.width / 2, vbuf); // 24-bit video is half-size
            },
            NABufferType::Video16(ref vbuf16) => {
                self.is16 = true;
                load16(&mut self.frame, self.width, vbuf16);
            },
            _ => return Err(EncoderError::InvalidParameters),
        };
        if old_is16 != self.is16 {
            is_intra = true;
        }

        let mut dbuf = Vec::with_capacity(4);
        let mut gw   = GrowableMemoryWriter::new_write(&mut dbuf);
        let mut bw   = ByteWriter::new(&mut gw);

        self.tm1type = match (self.is16, self.block_mode) {
                (true,  BlockMode::FourByFour) => 2,
                (true,  BlockMode::FourByTwo)  => 4,
                (true,  BlockMode::TwoByFour)  => 6,
                (true,  BlockMode::TwoByTwo)   => 8,
                (false, BlockMode::FourByFour) => 10,
                (false, BlockMode::FourByTwo)  => 12,
                (false, BlockMode::TwoByFour)  => 14,
                (false, BlockMode::TwoByTwo)   => 16,
            };
        self.delta_set = if self.is16 { 0 } else { 3 };
        if self.delta_mode == OptionMode::On && self.delta_set < 3 {
            self.delta_set += 1;
        }
        if (self.tm1type & 1) != 0 && meta_type != 0 {
            self.table_idx = 0;
        } else {
            self.table_idx = if self.is16 {
                    0
                } else {
                    match self.table_mode {
                        OptionMode::Off => 1,
                        OptionMode::On => 2,
                        OptionMode::Auto => self.delta_set.saturating_sub(1),
                    }
                };
        }

        bw.write_byte(0)?; // header size
        bw.write_byte(self.tm1type as u8)?;
        bw.write_byte(self.delta_set as u8)?;
        bw.write_byte(self.table_idx as u8 + 1)?;
        bw.write_u16le(self.height as u16)?;
        bw.write_u16le(self.width as u16)?;
        bw.write_u16le(0)?; // checksum
        bw.write_byte(2)?; // version
        bw.write_byte(meta_type)?;
        if meta_type == 2 {
            let flags = if is_intra { 0x10 } else { 0x08 };
            bw.write_byte(flags)?;
            bw.write_byte(1 << 4)?; // control - PC flavour
        }

        if self.is16 {
            self.encode_16(is_intra)?;
        } else {
            unimplemented!();
        }
        std::mem::swap(&mut self.frame, &mut self.prev_frame);
        self.frameno += 1;
        if self.frameno >= self.key_int {
            self.frameno = 0;
        }

        let hdr_size = bw.tell() as usize;

        if !is_intra {
            self.mask.flush();
            dbuf.extend_from_slice(&self.mask.data);
        }
        self.idx_wr.add_byte(1); // TM1 need a non-zero code at the end to make sure it's not an escape
        dbuf.extend_from_slice(&self.idx_wr.dst);

        dbuf[0] = ((hdr_size | 0x80) as u8).rotate_right(3);
        for i in (1..hdr_size).rev() {
            dbuf[i] ^= dbuf[i + 1];
        }

        self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, is_intra, dbuf));
        Ok(())
    }
    fn get_packet(&mut self) -> EncoderResult<Option<NAPacket>> {
        let mut npkt = None;
        std::mem::swap(&mut self.pkt, &mut npkt);
        Ok(npkt)
    }
    fn flush(&mut self) -> EncoderResult<()> {
        Ok(())
    }
}

const ENCODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: KEYFRAME_OPTION, description: KEYFRAME_OPTION_DESC,
        opt_type: NAOptionDefinitionType::Int(Some(0), Some(64)) },
    NAOptionDefinition {
        name: "delta_mode", description: "Alternative delta mode",
        opt_type: NAOptionDefinitionType::String(Some(&["off", "on", "auto"])) },
    NAOptionDefinition {
        name: "table_mode", description: "Alternative table mode",
        opt_type: NAOptionDefinitionType::String(Some(&["off", "on", "auto"])) },
    NAOptionDefinition {
        name: "block_mode", description: "Block mode",
        opt_type: NAOptionDefinitionType::String(Some(&["2x2", "2x4", "4x2", "4x4"])) },
];

impl NAOptionHandler for TM1Encoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { ENCODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in ENCODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        KEYFRAME_OPTION => {
                            if let NAValue::Int(val) = option.value {
                                self.key_int = val as usize;
                            }
                        },
                        "delta_mode" => {
                            if let NAValue::String(ref val) = option.value {
                                self.delta_mode = val.parse::<OptionMode>().unwrap();
                            }
                        },
                        "table_mode" => {
                            if let NAValue::String(ref val) = option.value {
                                self.table_mode = val.parse::<OptionMode>().unwrap();
                            }
                        },
                        "block_mode" => {
                            if let NAValue::String(ref val) = option.value {
                                self.block_mode = val.parse::<BlockMode>().unwrap();
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
            KEYFRAME_OPTION => Some(NAValue::Int(self.key_int as i64)),
            "delta_mode" => Some(NAValue::String(self.delta_mode.to_string())),
            "table_mode" => Some(NAValue::String(self.table_mode.to_string())),
            "block_mode" => Some(NAValue::String(self.block_mode.to_string())),
            _ => None,
        }
    }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(TM1Encoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_core::muxers::*;
    use crate::*;
    use nihav_commonfmt::*;
    use nihav_codec_support::test::enc_video::*;
    use super::super::truemotion1data::{RGB555_FORMAT, BGR0_FORMAT};

    #[allow(unused_variables)]
    fn encode_test(name: &'static str, enc_options: &[NAOption], hash: &[u32; 4], is16: bool) {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        duck_register_all_decoders(&mut dec_reg);
        let mut mux_reg = RegisteredMuxers::new();
        generic_register_all_muxers(&mut mux_reg);
        let mut enc_reg = RegisteredEncoders::new();
        duck_register_all_encoders(&mut enc_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/TM20/tm20.avi
        let dec_config = DecoderTestParams {
                demuxer:        "avi",
                in_name:        "assets/Duck/tm20.avi",
                stream_type:    StreamType::Video,
                limit:          Some(2),
                dmx_reg, dec_reg,
            };
        let enc_config = EncoderTestParams {
                muxer:          "avi",
                enc_name:       "truemotion1",
                out_name:       name,
                mux_reg, enc_reg,
            };
        let dst_vinfo = NAVideoInfo {
                width:   0,
                height:  0,
                format:  if is16 { RGB555_FORMAT } else { BGR0_FORMAT },
                flipped: false,
                bits:    if is16 { 16 } else { 24 },
            };
        let enc_params = EncodeParameters {
                format:  NACodecTypeInfo::Video(dst_vinfo),
                quality: 0,
                bitrate: 0,
                tb_num:  0,
                tb_den:  0,
                flags:   0,
            };
        //test_encoding_to_file(&dec_config, &enc_config, enc_params, enc_options);
        test_encoding_md5(&dec_config, &enc_config, enc_params, enc_options, hash);
    }
    #[test]
    fn test_truemotion1_encoder_rgb15_4x4() {
        let enc_options = &[
                NAOption { name: "key_int", value: NAValue::Int(8) },
                NAOption { name: "block_mode", value: NAValue::String("4x4".to_string()) },
            ];
        encode_test("tm1-15.avi", enc_options, &[0x6b8a5d15, 0xc9c9a391, 0x588c95c5, 0x5568d3b3], true);
    }
    #[test]
    fn test_truemotion1_encoder_rgb15_4x2() {
        let enc_options = &[
                NAOption { name: "key_int", value: NAValue::Int(8) },
                NAOption { name: "block_mode", value: NAValue::String("4x2".to_string()) },
            ];
        encode_test("tm1-15.avi", enc_options, &[0x999c2ffd, 0xd637f7a3, 0x4ebc070a, 0xef6fca4b], true);
    }
    #[test]
    fn test_truemotion1_encoder_rgb15_2x4() {
        let enc_options = &[
                NAOption { name: "key_int", value: NAValue::Int(8) },
                NAOption { name: "block_mode", value: NAValue::String("2x4".to_string()) },
            ];
        encode_test("tm1-15.avi", enc_options, &[0xfe62d1e6, 0xbdd8f28b, 0xf7fd810e, 0x0a9142f1], true);
    }
    #[test]
    fn test_truemotion1_encoder_rgb15_2x2() {
        let enc_options = &[
                NAOption { name: "key_int", value: NAValue::Int(8) },
                NAOption { name: "block_mode", value: NAValue::String("2x2".to_string()) },
            ];
        encode_test("tm1-15.avi", enc_options, &[0x3b445eb8, 0xac7cab31, 0x4c2ce978, 0x9b698658], true);
    }
    /*#[test]
    fn test_truemotion1_encoder_rgb24() {
        let enc_options = &[
                NAOption { name: "key_int", value: NAValue::Int(0) },
            ];
        encode_test("tm1-24.avi", enc_options, &[0x36cf8f48, 0x3e8ff2ce, 0x6f3822cf, 0xf7fbf19d], false);
panic!("end");
    }*/
}

// fat deltas for 15-bit mode
#[allow(clippy::neg_multiply)]
const DUCK_Y_DELTAS_5: [[i32; 8]; 4] = [
    [ 0,     -1 * 5,  1 * 5,  -3 * 5,  3 * 5,  -6 * 5,  6 * 5,  -6 * 5 ],
    [ 0,     -1 * 5,  2 * 5,  -3 * 5,  4 * 5,  -6 * 5,  6 * 5,  -6 * 5 ],
    [ 2 * 5, -3 * 5, 10 * 5, -10 * 5, 23 * 5, -23 * 5, 47 * 5, -47 * 5 ],
    [ 0,     -2 * 5,  2 * 5,  -8 * 5,  8 * 5, -18 * 5, 18 * 5, -40 * 5 ]
];
#[allow(clippy::neg_multiply)]
const DUCK_C_DELTAS_5: [[i32; 8]; 4] = [
    [ 0, -1 * 5, 1 * 5,  -2 * 5,  3 * 5,  -4 * 5,  5 * 5,  -4 * 5 ],
    [ 0, -1 * 5, 1 * 5,  -2 * 5,  3 * 5,  -4 * 5,  5 * 5,  -4 * 5 ],
    [ 0, -4 * 5, 3 * 5, -16 * 5, 20 * 5, -32 * 5, 36 * 5, -32 * 5 ],
    [ 0, -2 * 5, 2 * 5,  -8 * 5,  8 * 5, -18 * 5, 18 * 5, -40 * 5 ]
];
