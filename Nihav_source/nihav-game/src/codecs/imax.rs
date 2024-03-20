use nihav_core::codecs::*;
use nihav_core::io::byteio::*;

const FRAME_W: usize = 320;
const FRAME_H: usize = 160;

struct IMAXDecoder {
    info:       NACodecInfoRef,
    pal:        [u8; 768],
    frame:      [u8; FRAME_W * FRAME_H],
    hist:       [u8; 32768],
    hist_pos:   usize,
}

impl IMAXDecoder {
    fn new() -> Self {
        Self {
            info:       NACodecInfoRef::default(),
            pal:        [0; 768],
            frame:      [0; FRAME_W * FRAME_H],
            hist:       [0; 32768],
            hist_pos:   0,
        }
    }
}

impl NADecoder for IMAXDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(_vinfo) = info.get_properties() {
            /*let fmt = NAPixelFormaton::new(ColorModel::RGB(RGBSubmodel::RGB),
                                           Some(NAPixelChromaton::new(0, 0, true, 8, 0, 0, 3)),
                                           Some(NAPixelChromaton::new(0, 0, true, 8, 0, 1, 3)),
                                           Some(NAPixelChromaton::new(0, 0, true, 8, 0, 2, 3)),
                                           None, None,
                                           FORMATON_FLAG_PALETTE, 3);*/
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(FRAME_W, FRAME_H, false, PAL8_FORMAT));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 0);

        for sd in pkt.side_data.iter() {
            if let NASideData::Palette(true, ref pal) = sd {
                for (dst, src) in self.pal.chunks_mut(3).zip(pal.chunks(4)) {
                    dst[0] = src[0];
                    dst[1] = src[1];
                    dst[2] = src[2];
                }
                break;
            }
        }

        let mut mr = MemoryReader::new_read(&src);
        let mut br = ByteReader::new(&mut mr);

        let mut is_intra = true;
        let mut is_skip  = true;
        let mut idx = 0;
        while idx < self.frame.len() {
            let v                       = br.read_byte()?;
            let op  = v >> 6;
            let len = (v & 0x3F) as usize;
            match op {
                0 => {
                    validate!(idx + len <= self.frame.len());
                    idx += len;
                    is_intra = false;
                },
                1 => {
                    if len == 0 {
                        let off         = br.read_u16le()? as usize;
                        let len         = br.read_byte()? as usize;
                        validate!(idx + len <= self.frame.len());
                        validate!(off + len <= self.hist.len());
                        self.frame[idx..][..len].copy_from_slice(&self.hist[off..][..len]);
                    } else {
                        validate!(idx + len <= self.frame.len());
                                          br.read_buf(&mut self.frame[idx..][..len])?;
                        if self.hist_pos + len <= self.hist.len() {
                            self.hist[self.hist_pos..][..len].copy_from_slice(&self.frame[idx..][..len]);
                            self.hist_pos += len;
                        }
                        idx += len;
                    }
                    is_skip = false;
                },
                2 => {
                    let pix             = br.read_byte()?;
                    validate!(idx + len <= self.frame.len());
                    for _ in 0..len {
                        self.frame[idx] = pix;
                        idx += 1;
                    }
                    is_skip = false;
                },
                _ => {
                    let len2            = br.read_byte()? as usize;
                    let skip_len = len * 64 + len2;
                    validate!(idx + skip_len <= self.frame.len());
                    idx += skip_len;
                    is_intra = false;
                },
            };
        }

        let bufinfo = if !is_skip {
                let binfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 0)?;
                let mut vbuf = binfo.get_vbuf().unwrap();
                let paloff = vbuf.get_offset(1);
                let stride = vbuf.get_stride(0);
                let data = vbuf.get_data_mut().unwrap();
                for (drow, srow) in data.chunks_mut(stride).zip(self.frame.chunks(FRAME_W)) {
                    drow[..FRAME_W].copy_from_slice(srow);
                }
                data[paloff..][..768].copy_from_slice(&self.pal);
                binfo
            } else {
                NABufferType::None
            };

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), bufinfo);
        frm.set_keyframe(is_intra);
        let ftype = if is_skip { FrameType::Skip } else if is_intra { FrameType::I } else { FrameType::P };
        frm.set_frame_type(ftype);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for IMAXDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(IMAXDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use crate::game_register_all_demuxers;
    #[test]
    fn test_imax_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        game_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample from Fable game
        test_decoding("fable-imax", "fable-imax", "assets/Game/present.imx",
                      Some(64), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x775e1326, 0x7aa63674, 0x9b8aec54, 0x5caee2e3]));
    }
}
