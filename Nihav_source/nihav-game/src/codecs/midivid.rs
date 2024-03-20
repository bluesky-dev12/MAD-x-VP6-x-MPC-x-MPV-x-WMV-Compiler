use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_codec_support::codecs::HAMShuffler;

#[derive(Default)]
struct MidividDecoder {
    info:       NACodecInfoRef,
    hams:       HAMShuffler<u8>,
    lzbuf:      Vec<u8>,
    width:      usize,
    height:     usize,
}

impl MidividDecoder {
    fn new() -> Self {
        Self::default()
    }
}

fn lz_decompress(src: &[u8], dst: &mut [u8]) -> DecoderResult<()> {
    let mut spos = 0;
    let mut dpos = 0;
    let end = src.len();
    while spos < end {
        let oplo = u16::from(src[spos]);
        spos += 1;
        if spos >= end { return Err(DecoderError::ShortData); }
        let ophi = u16::from(src[spos]);
        spos += 1;
        let mut op = (ophi << 8) | oplo;
        for _ in 0..16 {
            if spos >= end { return Ok(()); }
            let b = src[spos];
            spos += 1;

            if (op & 1) == 0 {
                validate!(dpos < dst.len());
                dst[dpos] = b;
                dpos += 1;
            } else {
                validate!(spos < end);
                let bb = src[spos];
                spos += 1;

                let offset = (((b as usize) & 0xF0) << 4) | (bb as usize);
                let copy_len = ((b & 0xF) as usize) + 3;
                validate!(offset <= dpos);
                validate!(offset > 0);
                validate!(dpos + copy_len <= dst.len());
                for _ in 0..copy_len {
                    dst[dpos] = dst[dpos - offset];
                    dpos += 1;
                }
            }
            op >>= 1;
        }
    }
    Ok(())
}

#[allow(clippy::identity_op)]
fn decode_frame(frm: &mut NASimpleVideoFrame<u8>, src: &[u8], width: usize, height: usize) -> DecoderResult<bool> {
    validate!(src.len() > 8);
    let num_vec     = read_u16le(&src[0..])? as usize;
    validate!(num_vec <= 512);
    let is_intra    = read_u16le(&src[2..])? == 1;

    let (vecs, nblocks, idx_start) = if is_intra {
            (&src[4..], width / 2 * height / 2, num_vec * 12 + 4)
        } else {
            let num_blocks  = read_u32le(&src[4..])? as usize;
            let changeset_size = (width >> 5) * (height >> 2);
            (&src[8+changeset_size..], num_blocks, num_vec * 12 + 8 + changeset_size)
        };
    validate!(src.len() > idx_start);

    let src1 = if num_vec > 256 { &src[idx_start + (nblocks + 7)/8..] } else { &src[idx_start..] };
    let mut mr = MemoryReader::new_read(src1);
    let mut idx_br = ByteReader::new(&mut mr);
    let mut mr = MemoryReader::new_read(&src[idx_start..]);
    let mut idx9_br = ByteReader::new(&mut mr);
    let mut hi9 = 0u8;
    let mut bits = 0u8;
    for y in (0..height).step_by(2) {
        for x in (0..width).step_by(2) {
            if !is_intra {
                let x4 = x >> 2;
                let flag_b = src[8 + x4/8 + (y/4) * ((width + 31) >> 5)];
                if ((flag_b >> (x4 & 7)) & 1) == 0 {
                    continue;
                }
            }
            let idx = if num_vec <= 256 {
                    idx_br.read_byte()? as usize
                } else {
                    if bits == 0 {
                        hi9 = idx9_br.read_byte()?;
                        bits = 8;
                    }
                    bits -= 1;
                    let lo = idx_br.read_byte()? as usize;

                    ((((hi9 >> (7 - bits)) & 1) as usize) << 8) | lo
                };
            validate!(idx < num_vec);
            let vec = &vecs[idx * 12..];

            for comp in 0..3 {
                let dst = &mut frm.data[frm.offset[comp] + x + y * frm.stride[comp]..];
                dst[0] = vec[0 + comp];
                dst[1] = vec[3 + comp];
                dst[frm.stride[comp] + 0] = vec[6 + comp];
                dst[frm.stride[comp] + 1] = vec[9 + comp];
            }
        }
    }

    Ok(is_intra)
}

impl NADecoder for MidividDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Video(vinfo) = info.get_properties() {
            let fmt = NAPixelFormaton::new(ColorModel::YUV(YUVSubmodel::YCbCr),
                                           Some(NAPixelChromaton::new(0, 0, false, 8, 0, 0, 1)),
                                           Some(NAPixelChromaton::new(0, 0, false, 8, 0, 1, 1)),
                                           Some(NAPixelChromaton::new(0, 0, false, 8, 0, 2, 1)),
                                           None, None, 0, 3);
            self.width  = vinfo.get_width();
            self.height = vinfo.get_height();
            let myinfo = NACodecTypeInfo::Video(NAVideoInfo::new(self.width, self.height, true, fmt));
            self.info = NACodecInfo::new_ref(info.get_name(), myinfo, info.get_extradata()).into_ref();
            self.lzbuf = vec![0; self.width * self.height * 3];

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let src = pkt.get_buffer();
        validate!(src.len() > 4);

        let size = read_u32le(&src[0..])? as usize;
        validate!(size + 8 == src.len());
        let data_ptr;
        validate!(src.len() > 12);
        if read_u32le(&src[8..])? == 0 {
            lz_decompress(&src[12..], self.lzbuf.as_mut_slice())?;
            data_ptr = self.lzbuf.as_slice();
        } else {
            data_ptr = &src[12..];
        }

        let mut buf;
        let bufret = self.hams.clone_ref();
        if let Some(bbuf) = bufret {
            buf = bbuf;
        } else {
            let bufinfo = alloc_video_buffer(self.info.get_properties().get_video_info().unwrap(), 4)?;
            buf = bufinfo.get_vbuf().unwrap();
            self.hams.add_frame(buf);
            buf = self.hams.get_output_frame().unwrap();
        }

        let mut frm = NASimpleVideoFrame::from_video_buf(&mut buf).unwrap();
        let is_intra = decode_frame(&mut frm, data_ptr, self.width, self.height)?;

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.clone(), NABufferType::Video(buf));
        frm.set_keyframe(is_intra);
        frm.set_frame_type(if is_intra { FrameType::I } else { FrameType::P });
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.hams.clear();
    }
}

impl NAOptionHandler for MidividDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}


pub fn get_decoder_video() -> Box<dyn NADecoder + Send> {
    Box::new(MidividDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::game_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_midivid_video() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        game_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/V-codecs/MVDV.avi
        test_decoding("avi", "midivid", "assets/Game/MVDV.avi", Some(16), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5Frames(vec![
                        [0x383e1995, 0x32bf000d, 0x2067aa2e, 0x54425bd4],
                        [0x91d0bff6, 0x5106cb75, 0x463ba358, 0xdc17d126],
                        [0x4ce54833, 0xb4fd3e35, 0x639d3830, 0xb47f871b],
                        [0x7c0c5604, 0x3c89e3ff, 0x05ae09f5, 0x7b725143],
                        [0xc561ddd9, 0xa3515c8e, 0x6119b31a, 0xb1608e77],
                        [0xa49bb9aa, 0xaf57e55b, 0xf351d4b0, 0x6289cd91],
                        [0xc7add756, 0x45574231, 0x5f1d651b, 0x2ae29e0d],
                        [0x7dd57d54, 0x4ec83f80, 0xef2e870b, 0x6cc310fe],
                        [0xe9c5fed6, 0xa4a4bab2, 0x70f84ed6, 0xc9d8a010],
                        [0x586ba118, 0x623fd7b9, 0x480fe7ab, 0xa1a5ad6f]]));
    }
}
