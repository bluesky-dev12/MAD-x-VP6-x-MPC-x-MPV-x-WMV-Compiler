use nihav_core::formats;
use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use std::io::SeekFrom;
use super::indeo3data::*;

const DEFAULT_PIXEL: u8 = 0x40;
const STRIP_WIDTH:   u8 = 160;
const FRMH_TAG: u32 = ((b'F' as u32) << 24) | ((b'R' as u32) << 16)
                     | ((b'M' as u32) << 8) | (b'H' as u32);

const FLAG_8BIT:     u16 = 1 << 1;
const FLAG_KEYFRAME: u16 = 1 << 2;
const FLAG_BUFSEL:   u16 = 1 << 9;

const MAX_DEPTH: u8 = 20;

const H_SPLIT: u8 = 0;
const V_SPLIT: u8 = 1;
const ABS_FILL: u8 = 2;
const REL_FILL: u8 = 3;
const VQ_NULL: u8 = 2;
const VQ_DATA: u8 = 3;

type RequantTab = [[u8; 128]; 8];

trait AddDelta {
    fn add_delta(&mut self, delta: i8) -> DecoderResult<()>;
}

impl AddDelta for u8 {
    fn add_delta(&mut self, delta: i8) -> DecoderResult<()> {
        *self = self.wrapping_add(delta as u8);
        validate!((*self & 0x80) == 0);
        Ok(())
    }
}

#[derive(Clone, Copy)]
struct MV {
    x: i8,
    y: i8
}

struct Header {
    vq_offset:  u8,
    alt_quant:  [u8; 16],
    mvs:        [MV; 256],
    num_mvs:    usize,
    data_start: [u64; 3],
    data_end:   [u64; 3],
    is_intra:   bool,
}

impl Header {
    fn new() -> Self {
        Self {
            vq_offset:  0,
            alt_quant:  [0; 16],
            mvs:        [MV { x: 0, y: 0 }; 256],
            num_mvs:    0,
            data_start: [0; 3],
            data_end:   [0; 3],
            is_intra:   false,
        }
    }
}

struct DataReader<'a, 'b> {
    br:         &'a mut ByteReader<'b>,
    bpos:       u8,
    bitbuf:     u8,
}

impl<'a, 'b> DataReader<'a, 'b> {
    fn new(br: &'a mut ByteReader<'b>) -> Self {
        Self {
            br,
            bpos:       0,
            bitbuf:     0,
        }
    }
    fn read_2bits(&mut self) -> DecoderResult<u8> {
        if self.bpos == 0 {
            self.bitbuf     = self.br.read_byte()?;
            self.bpos       = 8;
        }
        self.bpos -= 2;
        let bits = (self.bitbuf >> self.bpos) & 3;
        Ok(bits)
    }
    fn read_byte(&mut self) -> DecoderResult<u8> {
        Ok(self.br.read_byte()?)
    }
}

#[derive(Debug, PartialEq)]
enum Corrector {
    Zero,
    Skip,
    Quad([i8; 4]),
    Fill(u8),
    ZeroBlock,
    SkipBlock,
}

impl Corrector {
    fn is_whole_block(&self) -> bool { matches!(*self, Corrector::Fill(_) | Corrector::ZeroBlock | Corrector::SkipBlock) }
}

struct QuadDecoder<'a, 'b, 'c> {
    br:         &'a mut DataReader<'b, 'c>,
    cb:         [&'static IviDeltaCB; 4],
    cb_idx:     [usize; 4],
    mode:       u8,

    lines_run:  u8,
    block_run:  u8,
    skip_flag:  bool,
    next_lit:   bool,
    fill:       Option<u8>,
}

impl<'a, 'b, 'c> QuadDecoder<'a, 'b, 'c> {
    fn new(br: &'a mut DataReader<'b, 'c>, mode: u8, cb_index: [usize; 2]) -> Self {
        Self {
            br, mode,
            lines_run:  0,
            block_run:  0,
            skip_flag:  false,
            next_lit:   false,
            fill:       None,
            cb:         [IVI3_DELTA_CBS[cb_index[0]], IVI3_DELTA_CBS[cb_index[1]],
                         IVI3_DELTA_CBS[cb_index[0]], IVI3_DELTA_CBS[cb_index[1]]],
            cb_idx:     [cb_index[0], cb_index[1], cb_index[0], cb_index[1]],
        }
    }
    fn get_skip_corr(&self) -> Corrector {
        if !self.skip_flag {
            Corrector::Zero
        } else {
            Corrector::Skip
        }
    }
    fn read(&mut self, line: u8) -> DecoderResult<Corrector> {
        if let Some(fill) = self.fill {
            self.fill = None;
            return Ok(Corrector::Fill(fill));
        }
        if self.lines_run > 0 {
            self.lines_run -= 1;
            return Ok(self.get_skip_corr());
        }
        if self.block_run > 0 {
            self.block_run -= 1;
            let corr = if !self.skip_flag {
                    Corrector::ZeroBlock
                } else {
                    Corrector::SkipBlock
                };
            return Ok(corr);
        }
        let mut b = self.br.read_byte()?;
        if self.next_lit {
            self.next_lit = false;
            if b >= 0xF8 {
                b = (self.cb[usize::from(line)].data.len() / 2) as u8;
            }
        }

        match b {
            0..=0xF7 => {
                let cb = self.cb[usize::from(line)];

                let esc_val = (cb.data.len() / 2) as u8;
                let (idx0, idx1) = if b < esc_val {
                        let idx2 = self.br.read_byte()?;
                        validate!(idx2 < esc_val);
                        (b, idx2)
                    } else if self.cb_idx[usize::from(line)] < 16 {
                        ((b - esc_val) % cb.quad_radix, (b - esc_val) / cb.quad_radix)
                    } else {
                        ((b - esc_val) / cb.quad_radix, (b - esc_val) % cb.quad_radix)
                    };
                let idx0 = usize::from(idx0);
                let idx1 = usize::from(idx1);
                Ok(Corrector::Quad([cb.data[idx1 * 2], cb.data[idx1 * 2 + 1],
                                    cb.data[idx0 * 2], cb.data[idx0 * 2 + 1]]))
            },
            0xF8 => {
                validate!(line == 0);
                let fillval = self.br.read_byte()?;
                if (fillval & 0x80) != 0 {
                    self.fill = Some(fillval & 0x7F);
                }
                Ok(Corrector::Fill(fillval & 0x7F))
            },
            0xF9 => {
                validate!(line == 0);
                self.skip_flag = true;
                self.block_run = 1;
                Ok(Corrector::SkipBlock)
            },
            0xFA => {
                validate!(self.mode != 3 && self.mode != 10);
                Ok(Corrector::SkipBlock)
            },
            0xFB => {
                let b = self.br.read_byte()?;
                validate!((b & 0x1F) > 0);
                validate!(b < 0x40);
                self.skip_flag = (b & 0x20) != 0;
                self.block_run = (b & 0x1F) - 1;
                if line > 0 {
                    self.lines_run = 3 - line;
                    Ok(self.get_skip_corr())
                } else {
                    let corr = if !self.skip_flag {
                            Corrector::ZeroBlock
                        } else {
                            Corrector::SkipBlock
                        };
                    Ok(corr)
                }
            },
            0xFC => {
                self.block_run = 1;
                self.skip_flag = false;
                if line > 0 {
                    self.lines_run = 3 - line;
                    Ok(Corrector::Zero)
                } else {
                    Ok(Corrector::ZeroBlock)
                }
            },
            0xFD => {
                self.lines_run = 3 - line;
                self.skip_flag = false;
                Ok(Corrector::Zero)
            },
            0xFE => {
                validate!(line < 2);
                self.lines_run = 2 - line;
                self.skip_flag = false;
                Ok(Corrector::Zero)
            },
            0xFF => {
                validate!(line == 0);
                self.lines_run = 1;
                self.skip_flag = false;
                self.next_lit  = true;
                Ok(Corrector::Zero)
            },
        }
    }
}

#[derive(Clone, Copy)]
struct Indeo3Cell {
    x:      u8,
    y:      u8,
    width:  u8,
    height: u8,
    mv:     Option<MV>,
    depth:  u8,
}

impl Indeo3Cell {
    fn new(width: usize, height: usize) -> Self {
        Self {
            x:      0,
            y:      0,
            width:  (width / 4) as u8,
            height: (height / 4) as u8,
            mv:     None,
            depth:  0,
        }
    }

    fn get_x(&self) -> usize { usize::from(self.x) * 4 }
    fn get_y(&self) -> usize { usize::from(self.y) * 4 }
    fn get_width(&self) -> usize { usize::from(self.width) * 4 }
    fn get_height(&self) -> usize { usize::from(self.height) * 4 }
    fn is_intra(&self) -> bool { self.mv.is_none() }

    fn split_h(&self) -> (Self, Self) {
        let h1 = if self.height > 2 { ((self.height + 2) >> 2) << 1 } else { 1 };
        let h2 = self.height - h1;
        let mut cell1 = *self;
        cell1.height = h1;
        cell1.depth += 1;
        let mut cell2 = *self;
        cell2.y     += h1;
        cell2.height = h2;
        cell2.depth += 1;
        (cell1, cell2)
    }
    fn split_v(&self, stripw: u8) -> (Self, Self) {
        let w1 = if self.width > stripw {
                if self.width > stripw * 2 { stripw * 2 } else { stripw }
            } else {
                if self.width > 2 { ((self.width + 2) >> 2) << 1 } else { 1 }
            };
        let w2 = self.width - w1;
        let mut cell1 = *self;
        cell1.width  = w1;
        cell1.depth += 1;
        let mut cell2 = *self;
        cell2.x     += w1;
        cell2.width  = w2;
        cell2.depth += 1;
        (cell1, cell2)
    }
}

impl std::fmt::Display for Indeo3Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let spec = if let Some(mv) = self.mv {
                format!("mv {},{}", mv.x, mv.y)
            } else {
                "intra".to_owned()
            };
        write!(f, "[{}x{} @ {},{} {}]", self.get_width(), self.get_height(), self.get_x(), self.get_y(), spec)
    }
}

#[derive(Default)]
struct Plane {
    width:      usize,
    height:     usize,
    data:       Vec<u8>,
    stripw:     u8,
}

impl Plane {
    fn new(stripw: u8) -> Self {
        Self {
            stripw: stripw / 4,
            ..Default::default()
        }
    }
    fn alloc(&mut self, width: usize, height: usize) {
        self.width  = width;
        self.height = height;
        self.data.resize(width * (height + 1), 0);
        for el in self.data[..width].iter_mut() {
            *el = DEFAULT_PIXEL;
        }
    }
    fn reset(&mut self) {
        for el in self.data[self.width..].iter_mut() {
            *el = 0;
        }
    }
    fn output_plane(&self, dst: &mut [u8], stride: usize, is_8bit: bool) {
        for (dline, sline) in dst.chunks_mut(stride).zip(self.data.chunks(self.width).skip(1)) {
            if !is_8bit {
                for (dst, &src) in dline.iter_mut().zip(sline.iter()) {
                    *dst = src << 1;
                }
            } else {
                let size = dline.len();
                dline.copy_from_slice(&sline[..size]);
            }
        }
    }
    fn checksum(&self) -> u16 {
        let mut checksum = [0; 2];
        for pair in self.data.chunks(2) {
            checksum[0] ^= pair[0];
            checksum[1] ^= pair[1];
        }
        read_u16le(&checksum).unwrap_or(0)
    }
    fn decode_data(&mut self, br: &mut DataReader, ref_plane: &mut Self, header: &Header, requant_tab: &RequantTab) -> DecoderResult<()> {
        let cell = Indeo3Cell::new(self.width, self.height);
        self.decode_mv_tree(br, ref_plane, cell, header, requant_tab)?;
        Ok(())
    }
    fn decode_mv_tree(&mut self, br: &mut DataReader, ref_plane: &mut Self, cell: Indeo3Cell, header: &Header, requant_tab: &RequantTab) -> DecoderResult<()> {
        validate!(cell.depth < MAX_DEPTH);
        match br.read_2bits()? {
            H_SPLIT => {
                let (cell1, cell2) = cell.split_h();
                self.decode_mv_tree(br, ref_plane, cell1, header, requant_tab)?;
                self.decode_mv_tree(br, ref_plane, cell2, header, requant_tab)?;
            },
            V_SPLIT => {
                let (cell1, cell2) = cell.split_v(self.stripw);
                self.decode_mv_tree(br, ref_plane, cell1, header, requant_tab)?;
                self.decode_mv_tree(br, ref_plane, cell2, header, requant_tab)?;
            },
            ABS_FILL => {
                self.decode_vq_tree(br, ref_plane, cell, header, requant_tab)?;
            },
            REL_FILL => {
                validate!(!header.is_intra);
                let mv_idx = usize::from(br.read_byte()?);
                validate!(mv_idx < header.num_mvs);
                let mut sec_cell = cell;
                sec_cell.mv = Some(header.mvs[mv_idx]);
                self.decode_vq_tree(br, ref_plane, sec_cell, header, requant_tab)?;
            },
            _ => unreachable!(),
        }
        Ok(())
    }
    fn decode_vq_tree(&mut self, br: &mut DataReader, ref_plane: &mut Self, cell: Indeo3Cell, header: &Header, requant_tab: &RequantTab) -> DecoderResult<()> {
        validate!(cell.depth < MAX_DEPTH);
        match br.read_2bits()? {
            H_SPLIT => {
                let (cell1, cell2) = cell.split_h();
                self.decode_vq_tree(br, ref_plane, cell1, header, requant_tab)?;
                self.decode_vq_tree(br, ref_plane, cell2, header, requant_tab)?;
            },
            V_SPLIT => {
                let (cell1, cell2) = cell.split_v(self.stripw);
                self.decode_vq_tree(br, ref_plane, cell1, header, requant_tab)?;
                self.decode_vq_tree(br, ref_plane, cell2, header, requant_tab)?;
            },
            VQ_NULL => {
                let code = br.read_2bits()?;
                match code {
                    0 => {
                        self.copy_cell(ref_plane, cell)?;
                    },
                    1 => return Err(DecoderError::NotImplemented), // skip cell
                    _ => return Err(DecoderError::InvalidData),
                };
            },
            VQ_DATA => {
                let code = br.read_byte()?;
                let mode = code >> 4;
                let vq_index = code & 0xF;
                let cb_index = if matches!(mode, 1 | 4 | 12) {
                        let aq = header.alt_quant[usize::from(vq_index)];
                        [aq & 0xF, aq >> 4]
                    } else {
                        [vq_index; 2]
                    };
                let cb_index = [usize::from(cb_index[0] + header.vq_offset), usize::from(cb_index[1] + header.vq_offset)];
                validate!(cb_index[0] < IVI3_DELTA_CBS.len());
                validate!(cb_index[1] < IVI3_DELTA_CBS.len());
                if cell.get_y() > 0 && matches!(mode, 0 | 3 | 10) && (8..=15).contains(&cb_index[0]) {
                    let src = if cell.is_intra() {
                            &mut self.data[cell.get_x() + cell.get_y() * self.width..]
                        } else {
                            &mut ref_plane.data[cell.get_x() + (cell.get_y() + 1) * ref_plane.width..]
                        };
                    let cur_requant_tab = &requant_tab[cb_index[0] - 8];
                    for el in src.iter_mut().take(cell.get_width()) {
                        *el = cur_requant_tab[usize::from(*el)];
                    }
                }

                let qmode = if mode != 10 || cell.is_intra() { mode } else { 20 };
                let mut quad_decoder = QuadDecoder::new(br, qmode, cb_index);
                if !cell.is_intra() {
                    self.copy_cell(ref_plane, cell)?;
                }
                match qmode {
                    0 | 1 => {
                        self.process_cell_0_1(&mut quad_decoder, cell)?;
                    },
                    3 | 4 => {
                        validate!(cell.is_intra());
                        validate!((cell.get_height() & 7) == 0);
                        self.process_cell_3_4(&mut quad_decoder, cell)?;
                    },
                    10 => {
                        validate!((cell.get_width() & 7) == 0);
                        validate!((cell.get_height() & 7) == 0);
                        self.process_cell_10_intra(&mut quad_decoder, cell)?;
                    },
                    20 => {
                        validate!((cell.get_width() & 7) == 0);
                        validate!((cell.get_height() & 7) == 0);
                        self.process_cell_10_inter(&mut quad_decoder, cell)?;
                    },
                    11 => {
                        validate!(!cell.is_intra());
                        validate!((cell.get_height() & 7) == 0);
                        self.process_cell_11(&mut quad_decoder, cell)?;
                    },
                    2 | 12 => { return Err(DecoderError::NotImplemented)},
                    _ => return Err(DecoderError::InvalidData),
                };
                if matches!(qmode, 3 | 4 | 10) && cell.get_y() == 0 {
                    let line_pair = &mut self.data[cell.get_x() + (cell.get_y() + 1) * self.width..];
                    let (line0, line1) = line_pair.split_at_mut(self.width);
                    line0[..cell.get_width()].copy_from_slice(&line1[..cell.get_width()]);
                }
            },
            _ => unreachable!(),
        }
        Ok(())
    }
    fn process_cell_0_1(&mut self, qd: &mut QuadDecoder, cell: Indeo3Cell) -> DecoderResult<()> {
        let stride = self.width;
        let mut offset = cell.get_x() + (cell.get_y() + 1) * stride;
        let cell_w = cell.get_width();
        for _y in (0..cell.get_height()).step_by(4) {
'block0:
            for x in (0..cell_w).step_by(4) {
                let top = &self.data[offset - stride + x..];
                let mut top = [top[0], top[1], top[2], top[3]];
                for line in 0..4 {
                    let corr = qd.read(line as u8)?;
                    if !cell.is_intra() && !corr.is_whole_block() {
                        let src = &self.data[offset + line * stride + x..];
                        top.copy_from_slice(&src[..4]);
                    }
                    match corr {
                        Corrector::SkipBlock => continue 'block0,
                        Corrector::Fill(fill) => {
                            for line in self.data[offset + x..].chunks_mut(stride).take(4) {
                                for el in line[..4].iter_mut() {
                                    *el = fill;
                                }
                            }
                        },
                        Corrector::ZeroBlock if cell.is_intra() => {
                            let (head, cur) = self.data.split_at_mut(offset + x);
                            let prev = &head[head.len() - stride..];
                            for dline in cur.chunks_mut(stride).take(4) {
                                dline[..4].copy_from_slice(&prev[..4]);
                            }
                            continue 'block0;
                        },
                        Corrector::ZeroBlock => continue 'block0,
                        Corrector::Quad(quad) => {
                            for (el, &corr) in top.iter_mut().zip(quad.iter()) {
                                el.add_delta(corr)?;
                            }
                        },
                        Corrector::Zero => {},
                        Corrector::Skip if cell.is_intra() => unimplemented!(),
                        Corrector::Skip => {},
                    };
                    let wback = match corr {
                        Corrector::Zero if cell.is_intra() => true,
                        Corrector::Quad(_) => true,
                        _ => false,
                    };
                    if wback {
                        self.data[offset + x + line * stride..][..4].copy_from_slice(&top);
                    }
                }
            }
            offset += self.width * 4;
        }
        Ok(())
    }
    fn process_cell_3_4(&mut self, qd: &mut QuadDecoder, cell: Indeo3Cell) -> DecoderResult<()> {
        let stride = self.width;
        let mut offset = cell.get_x() + (cell.get_y() + 1) * stride;
        let cell_w = cell.get_width();
        for _y in (0..cell.get_height()).step_by(8) {
'block3:
            for x in (0..cell_w).step_by(4) {
                let top = &self.data[offset - stride + x..][..4];
                let mut top = [top[0], top[1], top[2], top[3]];
                for line in 0..4 {
                    let corr = qd.read(line as u8)?;
                    match corr {
                        Corrector::SkipBlock => continue 'block3,
                        Corrector::Fill(fill) => {
                            for line in self.data[offset + x..].chunks_mut(stride).take(8) {
                                for el in line[..4].iter_mut() {
                                    *el = fill;
                                }
                            }
                        },
                        Corrector::ZeroBlock => {
                            for dline in self.data[offset + x..].chunks_mut(stride).take(8) {
                                dline[..4].copy_from_slice(&top);
                            }
                            continue 'block3;
                        },
                        Corrector::Quad(quad) => {
                            for (el, &corr) in top.iter_mut().zip(quad.iter()) {
                                el.add_delta(corr)?;
                            }
                        },
                        Corrector::Zero => {},
                        Corrector::Skip => unimplemented!(),
                    };

                    if corr != Corrector::Skip {
                        let dst = &mut self.data[offset + x + (line * 2 + 1) * stride..][..4];
                        dst.copy_from_slice(&top);
                    }
                }

                let mut top_off = offset + x - stride;
                for _line in 0..4 {
                    for pos in 0..4 {
                        self.data[top_off + stride + pos] = (self.data[top_off + pos] + self.data[top_off + stride * 2 + pos]) >> 1;
                    }
                    top_off += stride * 2;
                }
            }
            offset += self.width * 8;
        }
        Ok(())
    }
    fn process_cell_10_intra(&mut self, qd: &mut QuadDecoder, cell: Indeo3Cell) -> DecoderResult<()> {
        let stride = self.width;
        let mut offset = cell.get_x() + (cell.get_y() + 1) * stride;
        let cell_w = cell.get_width();
        for _y in (0..cell.get_height()).step_by(8) {
'block10i:
            for x in (0..cell_w).step_by(8) {
                let top = &self.data[offset - stride + x..][..8];
                let mut top = [top[0], top[2], top[4], top[6]];
                for line in 0..4 {
                    let corr = qd.read(line as u8)?;
                    match corr {
                        Corrector::SkipBlock => continue 'block10i,
                        Corrector::Fill(fill) => {
                            for line in self.data[offset + x..].chunks_mut(stride).take(8) {
                                for el in line[..8].iter_mut() {
                                    *el = fill;
                                }
                            }
                        },
                        Corrector::ZeroBlock => {
                            let line_pair = &mut self.data[offset - stride + x..];
                            let (top_line, dst_line) = line_pair.split_at_mut(stride);
                            for (i, (dst, &top_s)) in dst_line.iter_mut()
                                    .zip(top_line.iter()).take(8).enumerate() {
                                *dst = (top_s + top[i >> 1]) >> 1;
                            }
                            for dline in self.data[offset + x..].chunks_mut(stride).take(8).skip(1) {
                                for (dst, &src) in dline.chunks_mut(2).zip(top.iter()) {
                                    dst[0] = src;
                                    dst[1] = src;
                                }
                            }
                            continue 'block10i;
                        },
                        Corrector::Quad(quad) => {
                            for (el, &corr) in top.iter_mut().zip(quad.iter()) {
                                el.add_delta(corr)?;
                            }
                        },
                        Corrector::Zero => {},
                        Corrector::Skip => unimplemented!(),
                    };

                    if corr != Corrector::Skip {
                        for (dst, &prev) in self.data[offset + x + (line * 2 + 1) * stride..].chunks_mut(2).zip(top.iter()).take(4) {
                            dst[0] = prev;
                            dst[1] = prev;
                        }
                    }
                }

                let mut top_off = offset + x - stride;
                for _line in 0..4 {
                    for pos in 0..8 {
                        self.data[top_off + stride + pos] = (self.data[top_off + pos] + self.data[top_off + stride * 2 + pos]) >> 1;
                    }
                    top_off += stride * 2;
                }
            }
            offset += self.width * 8;
        }
        Ok(())
    }
    fn process_cell_10_inter(&mut self, qd: &mut QuadDecoder, cell: Indeo3Cell) -> DecoderResult<()> {
        let stride = self.width;
        let mut offset = cell.get_x() + (cell.get_y() + 1) * stride;
        let cell_w = cell.get_width();
        for _y in (0..cell.get_height()).step_by(8) {
'block10p:
            for x in (0..cell_w).step_by(8) {
                for line in 0..4 {
                    let corr = qd.read(line as u8)?;
                    match corr {
                        Corrector::SkipBlock | Corrector::ZeroBlock => continue 'block10p,
                        Corrector::Fill(fill) => {
                            for line in self.data[offset + x..].chunks_mut(stride).take(8) {
                                for el in line[..8].iter_mut() {
                                    *el = fill;
                                }
                            }
                        },
                        Corrector::Quad(quad) => {
                            let strip = &mut self.data[offset + x + line * 2 * stride..];
                            for (xoff, &corr) in quad.iter().enumerate() {
                                strip[xoff * 2].add_delta(corr)?;
                                strip[xoff * 2 + 1].add_delta(corr)?;
                                strip[xoff * 2 + stride].add_delta(corr)?;
                                strip[xoff * 2 + stride + 1].add_delta(corr)?;
                            }
                        },
                        _ => {},
                    };
                }
            }
            offset += self.width * 8;
        }
        Ok(())
    }
    fn process_cell_11(&mut self, qd: &mut QuadDecoder, cell: Indeo3Cell) -> DecoderResult<()> {
        let stride = self.width;
        let mut offset = cell.get_x() + (cell.get_y() + 1) * stride;
        let cell_w = cell.get_width();
        for _y in (0..cell.get_height()).step_by(8) {
'block10p:
            for x in (0..cell_w).step_by(4) {
                for line in 0..4 {
                    let corr = qd.read(line as u8)?;
                    match corr {
                        Corrector::SkipBlock | Corrector::ZeroBlock => continue 'block10p,
                        Corrector::Fill(fill) => {
                            for line in self.data[offset + x..].chunks_mut(stride).take(8) {
                                for el in line[..4].iter_mut() {
                                    *el = fill;
                                }
                            }
                        },
                        Corrector::Quad(quad) => {
                            let strip = &mut self.data[offset + x + line * 2 * stride..];
                            for (xoff, &corr) in quad.iter().enumerate() {
                                strip[xoff].add_delta(corr)?;
                                strip[xoff + stride].add_delta(corr)?;
                            }
                        },
                        _ => {},
                    };
                }
            }
            offset += self.width * 8;
        }
        Ok(())
    }
    fn copy_cell(&mut self, ref_plane: &Self, cell: Indeo3Cell) -> DecoderResult<()> {
        if let Some(mv) = cell.mv {
            let xpos = (cell.get_x() as isize) + isize::from(mv.x);
            validate!(xpos >= 0);
            let xpos = xpos as usize;
            validate!(xpos + cell.get_width() <= self.width);
            let ypos = (cell.get_y() as isize) + isize::from(mv.y);
            validate!(ypos >= 0);
            let ypos = ypos as usize;
            validate!(ypos + cell.get_height() <= self.height);
            let src = &ref_plane.data[xpos + (ypos + 1) * ref_plane.width..];
            let dst = &mut self.data[cell.get_x() + (cell.get_y() + 1) * self.width..];

            let width  = cell.get_width();
            let height = cell.get_height();
            for (dline, sline) in dst.chunks_mut(ref_plane.width).zip(src.chunks(self.width)).take(height) {
                dline[..width].copy_from_slice(&sline[..width]);
            }
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
}

struct IV3Frame {
    plane:      [Plane; 3],
}

impl IV3Frame {
    fn new() -> Self {
        Self {
            plane: [
                Plane::new(STRIP_WIDTH),
                Plane::new(STRIP_WIDTH >> 2),
                Plane::new(STRIP_WIDTH >> 2),
            ],
        }
    }
    fn alloc(&mut self, width: usize, height: usize) {
        self.plane[0].alloc( width,            height);
        let chroma_w = ((width  + 15) & !15) >> 2;
        let chroma_h = ((height + 15) & !15) >> 2;
        self.plane[1].alloc(chroma_w, chroma_h);
        self.plane[2].alloc(chroma_w, chroma_h);
    }
    fn reset(&mut self) {
        for plane in self.plane.iter_mut() {
            plane.reset()
        }
    }
    fn decode_planes(&mut self, ref_frame: &mut IV3Frame, br: &mut ByteReader, header: &mut Header, requant_tab: &RequantTab) -> DecoderResult<()> {
        let data_start = header.data_start;
        let data_end   = header.data_end;
        for ((cur_plane, ref_plane), (&start, &end)) in self.plane.iter_mut()
                .zip(ref_frame.plane.iter_mut())
                .zip(data_start.iter().zip(data_end.iter())) {
            br.seek(SeekFrom::Start(start))?;
            let num_mvs = br.read_u32le()? as usize;
            if header.is_intra {
                validate!(num_mvs == 0);
            } else {
                validate!(num_mvs <= header.mvs.len());
            }
            header.num_mvs = num_mvs;
            for mv in header.mvs.iter_mut().take(num_mvs) {
                mv.y = br.read_byte()? as i8;
                mv.x = br.read_byte()? as i8;
            }
            let mut reader = DataReader::new(br);
            cur_plane.decode_data(&mut reader, ref_plane, header, requant_tab)?;
            validate!(br.tell() <= end);
        }
        Ok(())
    }
    fn output_frame(&self, dst: &mut NAVideoBuffer<u8>, is_8bit: bool) {
        let dfrm = NASimpleVideoFrame::from_video_buf(dst).unwrap();
        for (plane_no, plane) in self.plane.iter().enumerate() {
            plane.output_plane(&mut dfrm.data[dfrm.offset[plane_no]..], dfrm.stride[plane_no], is_8bit);
        }
    }
    fn checksum(&self, is_8bit: bool) -> u16 {
        let mut checksum = 0;
        for plane in self.plane.iter() {
            checksum ^= plane.checksum();
        }
        if !is_8bit {
            checksum <<= 1;
        }
        checksum
    }
}

struct Indeo3Decoder {
    info:       NACodecInfoRef,
    width:      u16,
    height:     u16,
    header:     Header,
    frame0:     IV3Frame,
    frame1:     IV3Frame,
    requant_tab: RequantTab,
    do_crc:     bool,
    ign_size:   bool,
}

impl Indeo3Decoder {
    fn new() -> Self {
        const REQUANT_OFF: [i32; 8] = [ 0, 1, 0, 4, 4, 1, 0, 1 ];

        let dummy_info = NACodecInfo::new_dummy();

        let mut requant_tab = [[0u8; 128]; 8];
        for i in 0..8 {
            let step = (i as i32) + 2;
            let start = if (i == 3) || (i == 4) { -3 } else { step / 2 };
            let mut last = 0;
            for j in 0..128 {
                requant_tab[i][j] = (((j as i32) + start) / step * step + REQUANT_OFF[i]) as u8;
                if requant_tab[i][j] < 128 {
                    last = requant_tab[i][j];
                } else {
                    requant_tab[i][j] = last;
                }
            }
        }
        requant_tab[1][7]   =  10;
        requant_tab[1][119] = 118;
        requant_tab[1][120] = 118;
        requant_tab[4][8]   =  10;

        Indeo3Decoder {
            info:       dummy_info,
            width:      0,
            height:     0,
            header:     Header::new(),
            frame0:     IV3Frame::new(),
            frame1:     IV3Frame::new(),
            do_crc:     false,
            ign_size:   false,
            requant_tab
        }
    }
}

impl NADecoder for Indeo3Decoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let w = vinfo.get_width();
            let h = vinfo.get_height();
            let fmt = formats::YUV410_FORMAT;
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(w, h, false, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.frame0.reset();
            self.frame1.reset();
            self.width  = w as u16;
            self.height = h as u16;
            self.frame0.alloc(w, h);
            self.frame1.alloc(w, h);
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);

        // read OS header
        let frameno                     = br.read_u32le()?;
        let hdr_2                       = br.read_u32le()?;
        let check                       = br.read_u32le()?;
        let size                        = br.read_u32le()?;

        let data_start = br.tell();

        if (frameno ^ hdr_2 ^ size ^ FRMH_TAG) != check {
            return Err(DecoderError::InvalidData);
        }
        if i64::from(size) > br.left() {
            return Err(DecoderError::InvalidData);
        }

        let ver                         = br.read_u16le()?;
        if ver != 32 { return Err(DecoderError::NotImplemented); }
        let flags                       = br.read_u16le()?;
        let size2                       = br.read_u32le()?;
        if size2 == 0x80 {
            let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::None);
            frm.set_keyframe(false);
            frm.set_frame_type(FrameType::Skip);
            return Ok(frm.into_ref());
        }
        validate!(((size2 + 7) >> 3) <= size);
        self.header.vq_offset           = br.read_byte()?;
                                          br.read_skip(1)?;
        let checksum                    = br.read_u16le()?;
        let height                      = br.read_u16le()?;
        let width                       = br.read_u16le()?;
        validate!((width  >= 16) && (width  <= 640));
        validate!((height >= 16) && (height <= 480));
        validate!(((width & 3) == 0) && ((height & 3) == 0));
        if !self.ign_size && (width != self.width || height != self.height) {
            self.width  = width;
            self.height = height;
            self.frame0.alloc(width as usize, height as usize);
            self.frame1.alloc(width as usize, height as usize);
            let newinfo = NAVideoInfo::new(width as usize, height as usize, false, formats::YUV410_FORMAT);
            self.info = NACodecInfo::new_ref(self.info.get_name(), NACodecTypeInfo::Video(newinfo), self.info.get_extradata()).into_ref();
        }

        let yoff                        = br.read_u32le()?;
        let voff                        = br.read_u32le()?;
        let uoff                        = br.read_u32le()?;
        validate!(yoff <= size && uoff <= size && voff <= size);

                                          br.read_skip(4)?;
                                          br.read_buf(&mut self.header.alt_quant)?;

        let mut yend = src.len() as u32;//size;
        if (uoff < yend) && (uoff > yoff) { yend = uoff; }
        if (voff < yend) && (voff > yoff) { yend = voff; }
        let mut uend = size;
        if (yoff < uend) && (yoff > uoff) { uend = yoff; }
        if (voff < uend) && (voff > uoff) { uend = voff; }
        let mut vend = size;
        if (yoff < vend) && (yoff > voff) { vend = yoff; }
        if (uoff < vend) && (uoff > voff) { vend = uoff; }

        let intra_frame = (flags & FLAG_KEYFRAME) != 0;

        let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 4)?;
        let mut buf = bufinfo.get_vbuf().unwrap();

        let ystart  = data_start + u64::from(yoff);
        let ustart  = data_start + u64::from(uoff);
        let vstart  = data_start + u64::from(voff);
        let yendpos = data_start + u64::from(yend);
        let uendpos = data_start + u64::from(uend);
        let vendpos = data_start + u64::from(vend);

        self.header.data_start = [ystart, ustart, vstart];
        self.header.data_end = [yendpos, uendpos, vendpos];
        self.header.is_intra = intra_frame;

        let (cur_frame, ref_frame) = if (flags & FLAG_BUFSEL) != 0 {
                (&mut self.frame0, &mut self.frame1)
            } else {
                (&mut self.frame1, &mut self.frame0)
            };
        cur_frame.decode_planes(ref_frame, &mut br, &mut self.header, &self.requant_tab)?;
        cur_frame.output_frame(&mut buf, (flags & FLAG_8BIT) != 0);
        if self.do_crc && checksum != 0 {
            let out_checksum = cur_frame.checksum((flags & FLAG_8BIT) != 0);
            if checksum != out_checksum && checksum.rotate_left(8) != out_checksum {
                println!("checksum {:04X} / {:04X}", checksum, out_checksum);
                return Err(DecoderError::ChecksumError);
            }
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(intra_frame);
        frm.set_frame_type(if intra_frame { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }

    fn flush(&mut self) {
        self.frame0.reset();
        self.frame1.reset();
    }
}

const DO_CRC_OPTION: &str = "checksum";
const IGNORE_SIZE_OPTION: &str = "ignore_size_change";

const DECODER_OPTS: &[NAOptionDefinition] = &[
    NAOptionDefinition {
        name: DO_CRC_OPTION, description: "Verify frame checksum",
        opt_type: NAOptionDefinitionType::Bool },
    NAOptionDefinition {
        name: IGNORE_SIZE_OPTION, description: "Ignore dimensions provided in the frame header",
        opt_type: NAOptionDefinitionType::Bool },
];

impl NAOptionHandler for Indeo3Decoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { DECODER_OPTS }
    fn set_options(&mut self, options: &[NAOption]) {
        for option in options.iter() {
            for opt_def in DECODER_OPTS.iter() {
                if opt_def.check(option).is_ok() {
                    match option.name {
                        DO_CRC_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.do_crc = val;
                            }
                        },
                        IGNORE_SIZE_OPTION => {
                            if let NAValue::Bool(val) = option.value {
                                self.ign_size = val;
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
            DO_CRC_OPTION => Some(NAValue::Bool(self.do_crc)),
            IGNORE_SIZE_OPTION => Some(NAValue::Bool(self.ign_size)),
            _ => None,
        }
    }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(Indeo3Decoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::indeo_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_indeo3_decoder() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        indeo_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/IV32/iv32_example.avi
        test_decoding("avi", "indeo3", "assets/Indeo/iv32_example.avi", Some(10),
                      &dmx_reg, &dec_reg, ExpectedTestResult::MD5Frames(vec![
                            [0xd710b489, 0xbeee8f99, 0xfbe34549, 0xaf5805af],
                            [0x2b56ad73, 0x1faea777, 0xed480d09, 0x801e5185],
                            [0x06baa992, 0x74eef5fa, 0xf9d39fb2, 0xfac872ae],
                            [0xadceb016, 0x1fbd67f9, 0xba3e6621, 0xd822a026],
                            [0x052244b7, 0x1e3bd7bb, 0xd5ad10cf, 0x9177dc3e],
                            [0x84cca4bc, 0x19ac192f, 0xb9281be7, 0x7ad6193e],
                            [0xcab74cf9, 0xd7c77c2a, 0x848cbfc9, 0x604a2718],
                            [0xe6d65b3b, 0x3f3ea0e1, 0x383cad01, 0x0788f3ac],
                            [0xb25d9b0c, 0xc784bf67, 0x6e86991d, 0x7c2d9a14],
                            [0x8c70aeae, 0xf95369a1, 0x31d60201, 0xe7e4acdb],
                            [0x5c63f1bb, 0x32ce48a4, 0x226d112e, 0x440a5bba]]));
    }
}
