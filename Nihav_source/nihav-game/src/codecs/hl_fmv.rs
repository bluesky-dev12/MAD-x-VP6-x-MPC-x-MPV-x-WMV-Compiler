use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

const FRAME_W: usize = 320;
const FRAME_H: usize = 240;
const HIST_SIZE: usize = 32768;

struct Pattern {
    len:        u8,
    pattern:    [u8; 16],
}

struct HighlanderDecoder {
    info:       NACodecInfoRef,
    hist:       [u8; HIST_SIZE],
    tmp1:       [u8; FRAME_W * FRAME_H],
    tmp2:       [u8; FRAME_W * FRAME_H * 17 / 16],
}

fn unpack(src: &[u8], dst: &mut [u8], hist: &mut [u8; HIST_SIZE]) -> DecoderResult<usize> {
    let mut mr = MemoryReader::new_read(src);
    let mut br = ByteReader::new(&mut mr);

    let mut mw = MemoryWriter::new_write(dst);
    let mut bw = ByteWriter::new(&mut mw);

    *hist = [0; HIST_SIZE];

    let mut pprev = 0;
    let mut prev  = 0;
    while br.left() > 0 {
        let mut flags               = br.read_byte()?;
        for _ in 0..8 {
            let idx = (usize::from(pprev) << 7) ^ usize::from(prev);
            if (flags & 1) == 0 {
                if br.left() == 0 {
                    break;
                }
                hist[idx]           = br.read_byte()?;
            }
            let val = hist[idx];
            bw.write_byte(val)?;

            flags >>= 1;
            pprev = prev;
            prev  = val;
        }
    }

    Ok(bw.tell() as usize)
}

fn paint_frame(dst: &mut [u8], stride: usize, src: &[u8]) -> DecoderResult<()> {
    let mut mr = MemoryReader::new_read(src);
    let mut br = ByteReader::new(&mut mr);

    let mut blk_offs = [0; 16];
    for (y, offs) in blk_offs.chunks_mut(4).enumerate() {
        offs[0] = stride * y;
        offs[1] = stride * y + 1;
        offs[2] = stride * y + 2;
        offs[3] = stride * y + 3;
    }

    for row in dst.chunks_mut(stride * 4).take(FRAME_H / 4) {
        for xoff in (0..FRAME_W).step_by(4) {
            let idx = br.read_byte()? as usize;
            validate!(idx < PAINT_MODE.len());
            let mode = &PAINT_MODE[idx];
            validate!(i64::from(mode.len) <= br.left());

            for (&blk_off, &idx) in blk_offs.iter().zip(mode.pattern.iter()) {
                if idx == 0xFF {
                    row[xoff + blk_off] = br.read_byte()?;
                }
            }
            for (&blk_off, &idx) in blk_offs.iter().zip(mode.pattern.iter()) {
                if idx != 0xFF {
                    row[xoff + blk_off] = row[xoff + blk_offs[idx as usize]];
                }
            }
        }
    }
    Ok(())
}

impl HighlanderDecoder {
    fn new() -> Self {
        Self {
            info:   NACodecInfoRef::default(),
            hist:   [0; HIST_SIZE],
            tmp1:   [0; FRAME_W * FRAME_H],
            tmp2:   [0; FRAME_W * FRAME_H * 17 / 16],
        }
    }
}

impl NADecoder for HighlanderDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(_vinfo) = info.get_properties() {
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(FRAME_W, FRAME_H, false, PAL8_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 4);

        let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
        let bufo = bufinfo.get_vbuf();
        let mut buf = bufo.unwrap();
        let paloff = buf.get_offset(1);
        let stride = buf.get_stride(0);
        let data = buf.get_data_mut().unwrap();
        let dst = data.as_mut_slice();

        validate!(src.len() > 4);

        let size = read_u32le(&src)? as usize;
        validate!(size <= src.len() - 4);

        let size2 = unpack(&src[4..][..size],   &mut self.tmp1, &mut self.hist)?;
        let size3 = unpack(&self.tmp1[..size2], &mut self.tmp2, &mut self.hist)?;
        paint_frame(dst, stride, &self.tmp2[..size3])?;

        let dpal = &mut dst[paloff..][..768];
        for (dst, &src) in dpal.iter_mut().zip(DEFAULT_PAL.iter()) {
            *dst = (src << 2) | (src >> 4);
        }

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(true);
        frm.set_frame_type(FrameType::I);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for HighlanderDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(HighlanderDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    // sample extracted from Highlander: The Last of the MacLeods unpublished game
    #[test]
    fn test_hl_fmv_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        test_decoding("hl-fmv", "hl-fmv-video", "assets/Game/0260.fmv", Some(10), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x369659f0, 0x417ad3a7, 0xc62dfc6f, 0x6e5fe871]));
    }
}

const PAINT_MODE: [Pattern; 9] = [
    Pattern {
        len: 1,
        pattern: [
            0xFF, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00
        ],
    },
    Pattern {
        len: 2,
        pattern: [
            0x05, 0x09, 0x05, 0x09,
            0x09, 0xFF, 0x09, 0x05,
            0x05, 0xFF, 0x05, 0x09,
            0x09, 0x05, 0x09, 0x05
        ],
    },
    Pattern {
        len: 2,
        pattern: [
            0xFF, 0xFF, 0x00, 0x01,
            0x01, 0x01, 0x01, 0x01,
            0x00, 0x01, 0x00, 0x01,
            0x01, 0x01, 0x01, 0x01
        ]
    },
    Pattern {
        len: 2,
        pattern: [
            0xFF, 0xFF, 0x00, 0x01,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x01, 0x00, 0x01,
            0x00, 0x00, 0x00, 0x00
        ],
    },
    Pattern {
        len: 2,
        pattern: [
            0x0E, 0x0E, 0x0E, 0x0E,
            0x0E, 0x0F, 0x0E, 0x0F,
            0x0E, 0x0E, 0x0E, 0x0E,
            0x0E, 0x0F, 0xFF, 0xFF
        ],
    },
    Pattern {
        len: 2,
        pattern: [
            0x0F, 0x0F, 0x0F, 0x0F,
            0x0E, 0x0F, 0x0E, 0x0F,
            0x0F, 0x0F, 0x0F, 0x0F,
            0x0E, 0x0F, 0xFF, 0xFF
        ],
    },
    Pattern {
        len: 5,
        pattern: [
            0xFF, 0xFF, 0x00, 0xFF,
            0x01, 0xFF, 0x01, 0xFF,
            0x00, 0x01, 0x00, 0x03,
            0x03, 0x07, 0x03, 0x07
        ],
    },
    Pattern {
        len: 8,
        pattern: [
            0xFF, 0xFF, 0xFF, 0xFF,
            0x01, 0x00, 0x03, 0x02,
            0xFF, 0xFF, 0xFF, 0xFF,
            0x09, 0x08, 0x0B, 0x0A
        ],
    },
    Pattern {
        len: 16,
        pattern: [
            0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF, 0xFF, 0xFF
        ],
    }
];

const DEFAULT_PAL: [u8; 768] = [
    0x00, 0x00, 0x00,
    0x00, 0x00, 0x20,
    0x00, 0x20, 0x00,
    0x00, 0x20, 0x20,
    0x20, 0x00, 0x00,
    0x20, 0x00, 0x20,
    0x20, 0x20, 0x00,
    0x30, 0x30, 0x30,
    0x30, 0x37, 0x30,
    0x3C, 0x32, 0x29,
    0x01, 0x01, 0x01,
    0x02, 0x02, 0x02,
    0x03, 0x03, 0x03,
    0x04, 0x04, 0x04,
    0x05, 0x05, 0x05,
    0x07, 0x07, 0x07,
    0x08, 0x08, 0x08,
    0x0A, 0x0A, 0x0A,
    0x15, 0x15, 0x15,
    0x13, 0x13, 0x13,
    0x10, 0x10, 0x10,
    0x0E, 0x0E, 0x0E,
    0x20, 0x20, 0x20,
    0x00, 0x00, 0x20,
    0x00, 0x20, 0x00,
    0x00, 0x20, 0x20,
    0x20, 0x00, 0x00,
    0x20, 0x00, 0x20,
    0x20, 0x20, 0x00,
    0x00, 0x00, 0x0C,
    0x00, 0x00, 0x19,
    0x00, 0x00, 0x26,
    0x00, 0x00, 0x33,
    0x00, 0x0C, 0x00,
    0x00, 0x0C, 0x0C,
    0x00, 0x0C, 0x19,
    0x00, 0x0C, 0x26,
    0x00, 0x0C, 0x33,
    0x00, 0x0C, 0x3F,
    0x00, 0x19, 0x00,
    0x00, 0x19, 0x0C,
    0x00, 0x19, 0x19,
    0x00, 0x19, 0x26,
    0x00, 0x19, 0x33,
    0x00, 0x19, 0x3F,
    0x00, 0x26, 0x00,
    0x00, 0x26, 0x0C,
    0x00, 0x26, 0x19,
    0x00, 0x26, 0x26,
    0x00, 0x26, 0x33,
    0x00, 0x26, 0x3F,
    0x00, 0x33, 0x00,
    0x00, 0x33, 0x0C,
    0x00, 0x33, 0x19,
    0x00, 0x33, 0x26,
    0x00, 0x33, 0x33,
    0x00, 0x33, 0x3F,
    0x00, 0x3F, 0x19,
    0x00, 0x3F, 0x26,
    0x00, 0x3F, 0x33,
    0x0C, 0x00, 0x00,
    0x0C, 0x00, 0x0C,
    0x0C, 0x00, 0x19,
    0x0C, 0x00, 0x26,
    0x0C, 0x00, 0x33,
    0x0C, 0x00, 0x3F,
    0x0C, 0x0C, 0x00,
    0x0C, 0x0C, 0x0C,
    0x0C, 0x0C, 0x19,
    0x0C, 0x0C, 0x26,
    0x0C, 0x0C, 0x33,
    0x0C, 0x0C, 0x3F,
    0x0C, 0x19, 0x00,
    0x0C, 0x19, 0x0C,
    0x0C, 0x19, 0x19,
    0x0C, 0x19, 0x26,
    0x0C, 0x19, 0x33,
    0x0C, 0x19, 0x3F,
    0x0C, 0x26, 0x00,
    0x0C, 0x26, 0x0C,
    0x0C, 0x26, 0x19,
    0x0C, 0x26, 0x26,
    0x0C, 0x26, 0x33,
    0x0C, 0x26, 0x3F,
    0x0C, 0x33, 0x00,
    0x0C, 0x33, 0x0C,
    0x0C, 0x33, 0x19,
    0x0C, 0x33, 0x26,
    0x0C, 0x33, 0x33,
    0x0C, 0x33, 0x3F,
    0x0C, 0x3F, 0x0C,
    0x0C, 0x3F, 0x19,
    0x0C, 0x3F, 0x26,
    0x0C, 0x3F, 0x33,
    0x0C, 0x3F, 0x3F,
    0x19, 0x00, 0x00,
    0x19, 0x00, 0x0C,
    0x19, 0x00, 0x19,
    0x19, 0x00, 0x26,
    0x19, 0x00, 0x33,
    0x19, 0x00, 0x3F,
    0x19, 0x0C, 0x00,
    0x19, 0x0C, 0x0C,
    0x19, 0x0C, 0x19,
    0x19, 0x0C, 0x26,
    0x19, 0x0C, 0x33,
    0x19, 0x0C, 0x3F,
    0x19, 0x19, 0x00,
    0x19, 0x19, 0x0C,
    0x19, 0x19, 0x19,
    0x19, 0x19, 0x26,
    0x19, 0x19, 0x33,
    0x19, 0x26, 0x00,
    0x19, 0x26, 0x0C,
    0x19, 0x26, 0x19,
    0x19, 0x26, 0x26,
    0x19, 0x26, 0x33,
    0x19, 0x26, 0x3F,
    0x19, 0x33, 0x00,
    0x19, 0x33, 0x0C,
    0x19, 0x33, 0x26,
    0x19, 0x33, 0x33,
    0x19, 0x33, 0x3F,
    0x19, 0x3F, 0x00,
    0x19, 0x3F, 0x0C,
    0x19, 0x3F, 0x26,
    0x19, 0x3F, 0x33,
    0x33, 0x00, 0x3F,
    0x3F, 0x00, 0x33,
    0x26, 0x26, 0x00,
    0x26, 0x0C, 0x26,
    0x26, 0x00, 0x26,
    0x26, 0x00, 0x33,
    0x26, 0x00, 0x00,
    0x26, 0x0C, 0x0C,
    0x26, 0x00, 0x19,
    0x26, 0x0C, 0x33,
    0x26, 0x00, 0x3F,
    0x26, 0x19, 0x00,
    0x26, 0x19, 0x0C,
    0x26, 0x0C, 0x19,
    0x26, 0x19, 0x26,
    0x26, 0x19, 0x33,
    0x26, 0x0C, 0x3F,
    0x26, 0x26, 0x0C,
    0x26, 0x26, 0x19,
    0x26, 0x26, 0x26,
    0x26, 0x26, 0x33,
    0x26, 0x26, 0x3F,
    0x26, 0x33, 0x00,
    0x26, 0x33, 0x0C,
    0x19, 0x33, 0x19,
    0x26, 0x33, 0x26,
    0x26, 0x33, 0x33,
    0x26, 0x33, 0x3F,
    0x26, 0x3F, 0x00,
    0x26, 0x3F, 0x0C,
    0x26, 0x33, 0x19,
    0x26, 0x3F, 0x26,
    0x26, 0x3F, 0x33,
    0x26, 0x3F, 0x3F,
    0x33, 0x00, 0x00,
    0x26, 0x00, 0x0C,
    0x33, 0x00, 0x19,
    0x33, 0x00, 0x26,
    0x33, 0x00, 0x33,
    0x26, 0x0C, 0x00,
    0x33, 0x0C, 0x0C,
    0x33, 0x0C, 0x19,
    0x33, 0x0C, 0x26,
    0x33, 0x0C, 0x33,
    0x33, 0x0C, 0x3F,
    0x33, 0x19, 0x00,
    0x33, 0x19, 0x0C,
    0x26, 0x19, 0x19,
    0x33, 0x19, 0x26,
    0x33, 0x19, 0x33,
    0x26, 0x19, 0x3F,
    0x33, 0x26, 0x00,
    0x33, 0x26, 0x0C,
    0x33, 0x26, 0x19,
    0x33, 0x26, 0x26,
    0x33, 0x26, 0x33,
    0x33, 0x26, 0x3F,
    0x33, 0x33, 0x00,
    0x33, 0x33, 0x0C,
    0x33, 0x33, 0x19,
    0x33, 0x33, 0x26,
    0x33, 0x33, 0x33,
    0x33, 0x33, 0x3F,
    0x33, 0x3F, 0x00,
    0x33, 0x3F, 0x0C,
    0x26, 0x3F, 0x19,
    0x33, 0x3F, 0x26,
    0x33, 0x3F, 0x33,
    0x33, 0x3F, 0x3F,
    0x33, 0x00, 0x0C,
    0x3F, 0x00, 0x19,
    0x3F, 0x00, 0x26,
    0x33, 0x0C, 0x00,
    0x3F, 0x0C, 0x0C,
    0x3F, 0x0C, 0x19,
    0x3F, 0x0C, 0x26,
    0x3F, 0x0C, 0x33,
    0x3F, 0x0C, 0x3F,
    0x3F, 0x19, 0x00,
    0x3F, 0x19, 0x0C,
    0x33, 0x19, 0x19,
    0x3F, 0x19, 0x26,
    0x3F, 0x19, 0x33,
    0x33, 0x19, 0x3F,
    0x3F, 0x26, 0x00,
    0x3F, 0x26, 0x0C,
    0x3F, 0x26, 0x19,
    0x3F, 0x26, 0x26,
    0x3F, 0x26, 0x33,
    0x3F, 0x26, 0x3F,
    0x3F, 0x33, 0x00,
    0x3F, 0x33, 0x0C,
    0x3F, 0x33, 0x19,
    0x3F, 0x33, 0x26,
    0x3F, 0x33, 0x33,
    0x3F, 0x33, 0x3F,
    0x3F, 0x3F, 0x0C,
    0x33, 0x3F, 0x19,
    0x3F, 0x3F, 0x26,
    0x3F, 0x3F, 0x33,
    0x19, 0x19, 0x3F,
    0x19, 0x3F, 0x19,
    0x19, 0x3F, 0x3F,
    0x3F, 0x19, 0x19,
    0x3F, 0x19, 0x3F,
    0x3F, 0x3F, 0x19,
    0x30, 0x30, 0x30,
    0x17, 0x17, 0x17,
    0x1D, 0x1D, 0x1D,
    0x21, 0x21, 0x21,
    0x25, 0x25, 0x25,
    0x32, 0x32, 0x32,
    0x2C, 0x2C, 0x2C,
    0x35, 0x35, 0x35,
    0x37, 0x37, 0x37,
    0x38, 0x38, 0x38,
    0x3A, 0x3A, 0x3A,
    0x3C, 0x3C, 0x3C,
    0x3E, 0x3E, 0x3E,
    0x3C, 0x3E, 0x3F,
    0x29, 0x28, 0x28,
    0x20, 0x20, 0x20,
    0x00, 0x00, 0x3F,
    0x00, 0x3F, 0x00,
    0x00, 0x3F, 0x3F,
    0x3F, 0x00, 0x00,
    0x3F, 0x00, 0x3F,
    0x3F, 0x3F, 0x00,
    0x3F, 0x3F, 0x3F
];
