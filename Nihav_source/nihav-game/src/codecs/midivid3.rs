use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::codebook::*;
use nihav_codec_support::codecs::IPShuffler;

const COEFFS_PER_BLOCK: [usize; 4] = [0, 1, 4, 64];

struct BlockTypeReader<'a> {
    src:    &'a [u8],
    pos:    usize,
    bpos:   usize,
}

impl<'a> BlockTypeReader<'a> {
    fn read_block_type(&mut self) -> DecoderResult<usize> {
        if self.pos >= self.src.len() {
            return Err(DecoderError::ShortData);
        }
        let btype = (self.src[self.pos] >> self.bpos) & 3;
        self.bpos += 2;
        if self.bpos == 8 {
            self.bpos = 0;
            self.pos += 1;
        }
        Ok(btype as usize)
    }
}

struct MaskReader<'a> {
    src:    &'a [u8],
    pos:    usize,
    bpos:   usize,
}

impl<'a> MaskReader<'a> {
    fn read_type(&mut self) -> DecoderResult<(bool, bool)> {
        if self.pos >= self.src.len() {
            return Err(DecoderError::ShortData);
        }
        let is_intra    = ((self.src[self.pos] >> self.bpos) & 1) == 0;
        let has_residue = ((self.src[self.pos] >> (self.bpos + 4)) & 1) == 0;
        self.bpos += 1;
        if self.bpos == 4 {
            self.bpos = 0;
            self.pos += 1;
        }
        Ok((is_intra, has_residue))
    }
}

struct Midivid3Decoder {
    info:           NACodecInfoRef,
    shuf:           IPShuffler,
    cb:             Codebook<u32>,
    width:          usize,
    height:         usize,
    num_mbs:        usize,
    coeffs:         [i16; 65536],
    mvs:            Vec<i16>,
    qmat_y_intra:   [i16; 64],
    qmat_c_intra:   [i16; 64],
    qmat_y_inter:   [i16; 64],
    qmat_c_inter:   [i16; 64],
}

impl Midivid3Decoder {
    fn new() -> Self {
        let mut cr = ShortCodebookDescReader::new(vec![
                    ShortCodebookDesc { code: 0b00,         bits: 2 },
                    ShortCodebookDesc { code: 0b01,         bits: 2 },
                    ShortCodebookDesc { code: 0b100,        bits: 3 },
                    ShortCodebookDesc { code: 0b101,        bits: 3 },
                    ShortCodebookDesc { code: 0b110,        bits: 3 },
                    ShortCodebookDesc { code: 0b1110,       bits: 4 },
                    ShortCodebookDesc { code: 0b11110,      bits: 5 },
                    ShortCodebookDesc { code: 0b111110,     bits: 6 },
                    ShortCodebookDesc { code: 0b1111110,    bits: 7 },
                    ShortCodebookDesc { code: 0b11111110,   bits: 8 },
                    ShortCodebookDesc { code: 0b111111110,  bits: 9 },
                    ShortCodebookDesc { code: 0b111111111,  bits: 9 }
                ]);
        let cb = Codebook::new(&mut cr, CodebookMode::MSB).unwrap();
        Midivid3Decoder {
            info:           NACodecInfoRef::default(),
            shuf:           IPShuffler::default(),
            cb,
            width: 0, height: 0, num_mbs: 0,
            coeffs:         [0; 65536],
            mvs:            Vec::new(),
            qmat_y_intra:   [0; 64],
            qmat_c_intra:   [0; 64],
            qmat_y_inter:   [0; 64],
            qmat_c_inter:   [0; 64],
        }
    }
    fn decode_intra(&mut self, frm: &mut NASimpleVideoFrame<u8>, bt_reader: &mut BlockTypeReader, br: &mut BitReader) -> DecoderResult<()> {
        let mut ydst = frm.offset[0];
        let mut udst = frm.offset[1];
        let mut vdst = frm.offset[2];
        for _y in (0..self.height).step_by(16) {
            let mut ydc = 0;
            let mut udc = 0;
            let mut vdc = 0;
            let ncoeffs = br.read(16)? as usize;
            decode_values(br, &mut self.coeffs[..ncoeffs], &self.cb)?;
            let mut cpos = 0;
            for x in (0..self.width).step_by(16) {
                for blk in 0..4 {
                    let btype = bt_reader.read_block_type()?;
                    let nc = COEFFS_PER_BLOCK[btype];
                    validate!(cpos + nc <= ncoeffs);
                    let cur_off = x + (blk & 1) * 8 + (blk & 2) * 4 * frm.stride[0];
                    if btype != 0 {
                        self.coeffs[cpos] += ydc;
                        ydc = self.coeffs[cpos];
                    }
                    decode_block_intra(&mut frm.data[ydst + cur_off..], frm.stride[0], btype, &self.coeffs[cpos..], &self.qmat_y_intra);
                    cpos += nc;
                }
                let btype = bt_reader.read_block_type()?;
                let nc = COEFFS_PER_BLOCK[btype];
                validate!(cpos + nc <= ncoeffs);
                if btype != 0 {
                    self.coeffs[cpos] += udc;
                    udc = self.coeffs[cpos];
                }
                decode_block_intra(&mut frm.data[udst + x / 2..], frm.stride[1], btype, &self.coeffs[cpos..], &self.qmat_c_intra);
                cpos += nc;
                let btype = bt_reader.read_block_type()?;
                let nc = COEFFS_PER_BLOCK[btype];
                validate!(cpos + nc <= ncoeffs);
                if btype != 0 {
                    self.coeffs[cpos] += vdc;
                    vdc = self.coeffs[cpos];
                }
                decode_block_intra(&mut frm.data[vdst + x / 2..], frm.stride[2], btype, &self.coeffs[cpos..], &self.qmat_c_intra);
                cpos += nc;
            }
            ydst += frm.stride[0] * 16;
            udst += frm.stride[1] * 8;
            vdst += frm.stride[2] * 8;
        }
        Ok(())
    }
    fn decode_inter(&mut self, frm: &mut NASimpleVideoFrame<u8>, mask_reader: &mut MaskReader, bt_reader: &mut BlockTypeReader, br: &mut BitReader, num_mvs: usize) -> DecoderResult<()> {
        let ref_frm = self.shuf.get_ref();
        if ref_frm.is_none() {
            return Err(DecoderError::MissingReference);
        }
        let ref_frm = ref_frm.unwrap();

        decode_values(br, &mut self.mvs[..num_mvs * 2], &self.cb)?;

        let mut ydst = frm.offset[0];
        let mut udst = frm.offset[1];
        let mut vdst = frm.offset[2];
        let mut cur_mv = 0;
        for y in (0..self.height).step_by(16) {
            let mut ydc = 0;
            let mut udc = 0;
            let mut vdc = 0;
            let ncoeffs = br.read(16)? as usize;
            let pred_mbs = br.read(8)? as usize;
            decode_values(br, &mut self.coeffs[..ncoeffs], &self.cb)?;
            let mut cpos = 0;
            let mut cur_mb = 0;
            for x in (0..self.width).step_by(16) {
                let (is_intra, has_residue) = mask_reader.read_type()?;
                if is_intra {
                    for blk in 0..4 {
                        let btype = bt_reader.read_block_type()?;
                        let nc = COEFFS_PER_BLOCK[btype];
                        validate!(cpos + nc <= ncoeffs);
                        if (btype != 0) && (cur_mb < pred_mbs) {
                            self.coeffs[cpos] += ydc;
                            ydc = self.coeffs[cpos];
                        }
                        let cur_off = x + (blk & 1) * 8 + (blk & 2) * 4 * frm.stride[0];
                        decode_block_intra(&mut frm.data[ydst + cur_off..], frm.stride[0], btype, &self.coeffs[cpos..], &self.qmat_y_intra);
                        cpos += nc;
                    }
                    let btype = bt_reader.read_block_type()?;
                    let nc = COEFFS_PER_BLOCK[btype];
                    validate!(cpos + nc <= ncoeffs);
                    if (btype != 0) && (cur_mb < pred_mbs) {
                        self.coeffs[cpos] += udc;
                        udc = self.coeffs[cpos];
                    }
                    decode_block_intra(&mut frm.data[udst + x / 2..], frm.stride[1], btype, &self.coeffs[cpos..], &self.qmat_c_intra);
                    cpos += nc;
                    let btype = bt_reader.read_block_type()?;
                    let nc = COEFFS_PER_BLOCK[btype];
                    validate!(cpos + nc <= ncoeffs);
                    if (btype != 0) && (cur_mb < pred_mbs) {
                        self.coeffs[cpos] += vdc;
                        vdc = self.coeffs[cpos];
                    }
                    decode_block_intra(&mut frm.data[vdst + x / 2..], frm.stride[2], btype, &self.coeffs[cpos..], &self.qmat_c_intra);
                    cpos += nc;
                } else {
                    validate!(cur_mv < num_mvs);
                    let mv_x = self.mvs[cur_mv * 2];
                    let mv_y = self.mvs[cur_mv * 2 + 1];
                    cur_mv += 1;
                    let src_x = (x as isize) + (mv_x as isize);
                    let src_y = (y as isize) + (mv_y as isize);
                    validate!(src_x >= 0 && src_x + 16 <= self.width as isize);
                    validate!(src_y >= 0 && src_y + 16 <= self.height as isize);
                    let sstride = ref_frm.get_stride(0);
                    let src = &ref_frm.get_data()[ref_frm.get_offset(0) + (src_x as usize) + (src_y as usize) * sstride..];
                    let dst = &mut frm.data[ydst + x..];
                    for (dst, src) in dst.chunks_mut(frm.stride[0]).zip(src.chunks(sstride)).take(16) {
                        dst[..16].copy_from_slice(&src[..16]);
                    }
                    let xoff = (src_x as usize) >> 1;
                    let yoff = (src_y as usize) >> 1;
                    for plane in 1..3 {
                        let sstride = ref_frm.get_stride(plane);
                        let src = &ref_frm.get_data()[ref_frm.get_offset(plane) + xoff + yoff * sstride..];
                        let dst = &mut frm.data[if plane == 1 { udst } else { vdst } + x / 2..];
                        for (dst, src) in dst.chunks_mut(frm.stride[plane]).zip(src.chunks(sstride)).take(8) {
                            dst[..8].copy_from_slice(&src[..8]);
                        }
                    }
                    if has_residue {
                        for blk in 0..4 {
                            let btype = bt_reader.read_block_type()?;
                            let nc = COEFFS_PER_BLOCK[btype];
                            validate!(cpos + nc <= ncoeffs);
                            if (btype != 0) && (cur_mb < pred_mbs) {
                                self.coeffs[cpos] += ydc;
                                ydc = self.coeffs[cpos];
                            }
                            let cur_off = x + (blk & 1) * 8 + (blk & 2) * 4 * frm.stride[0];
                            decode_block_inter(&mut frm.data[ydst + cur_off..], frm.stride[0], btype, &self.coeffs[cpos..], &self.qmat_y_inter);
                            cpos += nc;
                        }
                        let btype = bt_reader.read_block_type()?;
                        let nc = COEFFS_PER_BLOCK[btype];
                        validate!(cpos + nc <= ncoeffs);
                        if (btype != 0) && (cur_mb < pred_mbs) {
                            self.coeffs[cpos] += udc;
                            udc = self.coeffs[cpos];
                        }
                        decode_block_inter(&mut frm.data[udst + x / 2..], frm.stride[1], btype, &self.coeffs[cpos..], &self.qmat_c_inter);
                        cpos += nc;
                        let btype = bt_reader.read_block_type()?;
                        let nc = COEFFS_PER_BLOCK[btype];
                        validate!(cpos + nc <= ncoeffs);
                        if (btype != 0) && (cur_mb < pred_mbs) {
                            self.coeffs[cpos] += vdc;
                            vdc = self.coeffs[cpos];
                        }
                        decode_block_inter(&mut frm.data[vdst + x / 2..], frm.stride[2], btype, &self.coeffs[cpos..], &self.qmat_c_inter);
                        cpos += nc;
                    }
                }
                if is_intra || has_residue {
                    cur_mb += 1;
                }
            }
            ydst += frm.stride[0] * 16;
            udst += frm.stride[1] * 8;
            vdst += frm.stride[2] * 8;
        }
        Ok(())
    }
}

fn decode_values(br: &mut BitReader, dst: &mut [i16], cb: &Codebook<u32>) -> DecoderResult<()> {
    let mut zero_run = 0usize;
    for el in dst.iter_mut() {
        if zero_run > 0 {
            *el = 0;
            zero_run -= 1;
        } else {
            let val = br.read_cb(cb)? as u8;
            if val == 0 {
                zero_run = if br.read_bool()? {
                        br.read(6)? as usize + 8
                    } else {
                        br.read(3)? as usize
                    };
                *el = 0;
            } else {
                let sign = br.read_bool()?;
                let abits = br.read(val - 1)? as i16;
                *el = (1 << (val - 1)) + abits;
                if sign {
                    *el = -*el;
                }
            }
        }
    }
    Ok(())
}

fn dequant(val: i16, q: i16) -> i32 {
    i32::from(val) * i32::from(q)
}

fn scale_coef(val: i32, scale: i16) -> i32 {
    (val * i32::from(scale)) >> 8
}

macro_rules! idct_1d {
    ($c0: expr, $c1: expr, $c2: expr, $c3: expr, $c4: expr, $c5: expr, $c6: expr, $c7: expr) => {
        let t0 = $c0 + $c4;
        let t1 = $c0 - $c4;
        let t2 = $c2 + $c6;
        let t3 = scale_coef($c2 - $c6, 362) - t2;
        let t4 = t0 + t2;
        let t5 = t0 - t2;
        let t6 = t1 + t3;
        let t7 = t1 - t3;
        let t8 = $c5 + $c3;
        let t9 = $c5 - $c3;
        let ta = $c1 + $c7;
        let tb = $c1 - $c7;
        let tc = t8 + ta;
        let td = scale_coef(tb + t9, 473);
        let te = scale_coef(t9, -669) - tc + td;
        let tf = scale_coef(ta - t8, 362) - te;
        let t10 = scale_coef(tb, 277) - td + tf;
        $c0 = t4 + tc;
        $c1 = t6 + te;
        $c2 = t7 + tf;
        $c3 = t5 - t10;
        $c4 = t5 + t10;
        $c5 = t7 - tf;
        $c6 = t6 - te;
        $c7 = t4 - tc;
    }
}

#[allow(clippy::erasing_op)]
#[allow(clippy::identity_op)]
fn idct(blk: &mut [i32; 64]) {
    for i in 0..8 {
        idct_1d!(blk[i + 0 * 8], blk[i + 1 * 8], blk[i + 2 * 8], blk[i + 3 * 8],
                 blk[i + 4 * 8], blk[i + 5 * 8], blk[i + 6 * 8], blk[i + 7 * 8]);
    }
    for row in blk.chunks_mut(8) {
        idct_1d!(row[0], row[1], row[2], row[3], row[4], row[5], row[6], row[7]);
    }
    for el in blk.iter_mut() {
        *el >>= 5;
    }
}

fn decode_block_intra(dst: &mut [u8], stride: usize, btype: usize, coeffs: &[i16], qmat: &[i16; 64]) {
    match btype {
        0 | 1 => {
            let fill_val = if btype == 0 { 0x80 } else {
                    ((dequant(coeffs[0], qmat[0]) >> 5) + 128).max(0).min(255) as u8
                };
            for line in dst.chunks_mut(stride).take(8) {
                for el in line.iter_mut().take(8) {
                    *el = fill_val;
                }
            }
        },
        2 => {
            let mut blk = [0i32; 64];
            blk[0] = dequant(coeffs[0], qmat[0]);
            blk[1] = dequant(coeffs[1], qmat[1]);
            blk[8] = dequant(coeffs[2], qmat[8]);
            blk[9] = dequant(coeffs[3], qmat[9]);
            idct(&mut blk);
            for (line, row) in dst.chunks_mut(stride).zip(blk.chunks(8)).take(8) {
                for (dst, coef) in line.iter_mut().zip(row.iter()).take(8) {
                    *dst = (*coef + 128).max(0).min(255) as u8;
                }
            }
        },
        _ => {
            let mut blk = [0i32; 64];
            for i in 0..64 {
                blk[SCAN[i]] = dequant(coeffs[i], qmat[SCAN[i]]);
            }
            idct(&mut blk);
            for (line, row) in dst.chunks_mut(stride).zip(blk.chunks(8)).take(8) {
                for (dst, coef) in line.iter_mut().zip(row.iter()).take(8) {
                    *dst = (*coef + 128).max(0).min(255) as u8;
                }
            }
        },
    };
}

fn decode_block_inter(dst: &mut [u8], stride: usize, btype: usize, coeffs: &[i16], qmat: &[i16; 64]) {
    match btype {
        0 => {}
        1 => {
            let dc = dequant(coeffs[0], qmat[0]) >> 5;
            for line in dst.chunks_mut(stride).take(8) {
                for el in line.iter_mut().take(8) {
                    *el = (i32::from(*el) + dc).max(0).min(255) as u8;
                }
            }
        },
        2 => {
            let mut blk = [0i32; 64];
            blk[0] = dequant(coeffs[0], qmat[0]);
            blk[1] = dequant(coeffs[1], qmat[1]);
            blk[8] = dequant(coeffs[2], qmat[8]);
            blk[9] = dequant(coeffs[3], qmat[9]);
            idct(&mut blk);
            for (line, row) in dst.chunks_mut(stride).zip(blk.chunks(8)).take(8) {
                for (dst, coef) in line.iter_mut().zip(row.iter()).take(8) {
                    *dst = (i32::from(*dst) + *coef).max(0).min(255) as u8;
                }
            }
        },
        _ => {
            let mut blk = [0i32; 64];
            for i in 0..64 {
                blk[SCAN[i]] = dequant(coeffs[i], qmat[SCAN[i]]);
            }
            idct(&mut blk);
            for (line, row) in dst.chunks_mut(stride).zip(blk.chunks(8)).take(8) {
                for (dst, coef) in line.iter_mut().zip(row.iter()).take(8) {
                    *dst = (i32::from(*dst) + *coef).max(0).min(255) as u8;
                }
            }
        },
    };
}

fn init_quant(qmat: &mut [i16; 64], base_qmat: &[u8; 64], quant: u8) {
    let q = if quant < 50 {
            5000 / i32::from(quant.max(1))
        } else {
            i32::from((100 - quant.min(100)) * 2)
        };
    for (inq, (outq, scale)) in base_qmat.iter().zip(qmat.iter_mut().zip(QUANT_MATRIX.iter())) {
        let val = ((i32::from(*inq) * q + 50) / 100).max(1).min(0x7FFF);
        *outq = ((val * i32::from(*scale) + 0x800) >> 12) as i16;
    }
}

impl NADecoder for Midivid3Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, true, YUV420_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.num_mbs = ((self.width + 15) >> 4) * ((self.height + 15) >> 4);
            self.mvs.resize(self.num_mbs * 2, 0);

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 8);

        let i_quant     = src[0];
        let p_quant     = i_quant.wrapping_add(src[1]);
        let inter_flag  = read_u16le(&src[2..])?;
        validate!(inter_flag < 2);
        let is_intra = inter_flag == 0;

        init_quant(&mut self.qmat_y_intra, &QUANT_MAT_LUMA,   i_quant);
        init_quant(&mut self.qmat_c_intra, &QUANT_MAT_CHROMA, i_quant);
        init_quant(&mut self.qmat_y_inter, &QUANT_MAT_LUMA,   p_quant);
        init_quant(&mut self.qmat_c_inter, &QUANT_MAT_CHROMA, p_quant);

        let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 4)?;
        let mut buf = bufinfo.get_vbuf().unwrap();

        let mut frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
        let bt_size = read_u16le(&src[4..])? as usize;
        if is_intra {
            validate!(bt_size + 6 <= src.len());

            let bt_buf = if is_intra { &src[6..][..bt_size] } else { &src[8..][..bt_size] };
            let mut bt_reader = BlockTypeReader { src: bt_buf, pos: 0, bpos: 0 };
            let mut br = BitReader::new(&src[6 + bt_size..], BitReaderMode::BE);

            self.decode_intra(&mut frm, &mut bt_reader, &mut br)?;
        } else {
            let num_mvs = read_u16le(&src[6..])? as usize;
            let mask_len = (self.num_mbs + 3) >> 2;
            let data_start = 8 + bt_size + mask_len;
            validate!(data_start <= src.len());
            validate!(num_mvs <= self.num_mbs);

            let bt_buf = &src[8 + mask_len..][..bt_size];
            let mask = &src[8..][..mask_len];
            let mut mask_reader = MaskReader { src: mask, pos: 0, bpos: 0 };
            let mut bt_reader = BlockTypeReader { src: bt_buf, pos: 0, bpos: 0 };
            let mut br = BitReader::new(&src[data_start..], BitReaderMode::BE);

            self.decode_inter(&mut frm, &mut mask_reader, &mut bt_reader, &mut br, num_mvs)?;
        }

        self.shuf.add_frame(buf.clone());

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.shuf.clear();
    }
}

impl NAOptionHandler for Midivid3Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder_video() -> Box<dyn NADecoder + Send> {
    Box::new(Midivid3Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_midivid3_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/mv30.avi
        test_decoding("avi", "midivid3", "assets/Game/mv30.avi", Some(16), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                            [0x0f4f2377, 0xe017458f, 0xebf6d59d, 0x238a3e64],
                            [0xdca83224, 0xbd2fd721, 0x954804c6, 0xaa38b1f3],
                            [0x35104196, 0xf96eacb5, 0x5d34910a, 0xb6dfde27],
                            [0x6106ddf6, 0xda75f5bf, 0x1eaeef52, 0xffbfbdb8],
                            [0xdd429c46, 0x4bc67009, 0xdc14d6e2, 0x816f7e4a],
                            [0xa58a19fa, 0x663edfc1, 0x12a89fe7, 0x7ffd8484],
                            [0xf4f2f0c5, 0x5479661c, 0x22839c5f, 0x8ff45232],
                            [0xb70cb7d9, 0x9c514449, 0xabb85bb4, 0x5b20d9d7],
                            [0xd65c33cc, 0xba03ff85, 0x615b3171, 0xfd16334f],
                            [0x13c5ffc3, 0x1e279df1, 0x48393c15, 0x394bfabe],
                            [0x169cb91a, 0x2a8f8a63, 0x995d53c2, 0x1123d5e9],
                            [0x26a8bc57, 0x3b8ba658, 0xb960ef85, 0xf4dbd720],
                            [0x13a405e9, 0x3fae3101, 0xee1c29f4, 0x3bf69d94],
                            [0x3d7b1b6b, 0x9026d717, 0x97e1c6c1, 0x4c6877a7],
                            [0x8f2f4b9a, 0x6afa65f3, 0x9b0e0173, 0x56725a4a],
                            [0x4b140176, 0x10ee87d5, 0x899e86fe, 0xb30fc404],
                            [0xb82892a1, 0x2eda670c, 0x7a130bb4, 0x810fc089]]));
    }
}

const QUANT_MAT_LUMA: [u8; 64] = [
    12, 12, 15, 19, 25, 34, 40, 48,
    12, 12, 18, 22, 27, 44, 47, 46,
    17, 18, 21, 26, 35, 46, 52, 47,
    18, 20, 24, 28, 40, 61, 59, 51,
    20, 24, 32, 43, 50, 72, 72, 63,
    25, 31, 42, 48, 58, 72, 81, 75,
    38, 46, 54, 61, 71, 84, 88, 85,
    50, 61, 65, 68, 79, 78, 86, 91
];

const QUANT_MAT_CHROMA: [u8; 64] = [
    12, 16, 24, 47, 99, 99, 99, 99,
    16, 21, 26, 66, 99, 99, 99, 99,
    24, 26, 56, 99, 99, 99, 99, 99,
    47, 66, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99
];

const QUANT_MATRIX: [i16; 64] = [
    16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
    22725, 31521, 29692, 26722, 22725, 17855, 12299,  6270,
    21407, 29692, 27969, 25172, 21407, 16819, 11585,  5906,
    19266, 26722, 25172, 22654, 19266, 15137, 10426,  5315,
    16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
    12873, 17855, 16819, 15137, 12873, 10114,  6967,  3552,
     8867, 12299, 11585, 10426,  8867,  6967,  4799,  2446,
     4520,  6270,  5906,  5315,  4520,  3552,  2446,  1247
];

const SCAN: [usize; 64] = [
     0,  1,  8,  9, 16,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63
];
