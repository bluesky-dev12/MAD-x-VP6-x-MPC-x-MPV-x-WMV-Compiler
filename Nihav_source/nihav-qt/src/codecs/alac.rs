use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::intcode::*;
use std::str::FromStr;

struct ALACDecoder {
    ainfo:      NAAudioInfo,
    chmap:      NAChannelMap,
    bits:       u8,
    frame_len:  usize,
    samples_l:  Vec<i32>,
    samples_r:  Vec<i32>,
    cur_len:    usize,

    pb:         u8,
    kb:         u8,
    mb:         u8,
}

#[derive(Default,Clone,Copy)]
struct PredInfo {
    mode:       u8,
    shift:      u8,
    factor:     u8,
    order:      usize,
    filt:       [i32; 32],
}

fn read_code(br: &mut BitReader, k: u8, bits: u8) -> DecoderResult<u32> {
    let pfx                     = br.read_code(UintCodeType::LimitedUnary(9, 0))?;
    if pfx < 9 {
        if k > 1 {
            let tail            = br.read(k - 1)?;
            if tail != 0 {
                Ok((pfx << k) - pfx + tail * 2 + br.read(1)? - 1)
            } else {
                Ok((pfx << k) - pfx)
            }
        } else {
            Ok(pfx)
        }
    } else {
        Ok(br.read(bits)?)
    }
}

fn clip_sample(val: i32, bits: u8) -> i32 {
    val << (32 - bits) >> (32 - bits)
}

impl PredInfo {
    fn read_header(&mut self, br: &mut BitReader, pb: u8) -> DecoderResult<()> {
        self.mode               = br.read(4)? as u8;
        if self.mode != 0 && self.mode != 15 {
            return Err(DecoderError::NotImplemented);
        }
        self.shift              = br.read(4)? as u8;
        self.factor             = (br.read(3)? as u8) * pb / 4;
        self.order              = br.read(5)? as usize;
        for coef in self.filt.iter_mut().take(self.order).rev() {
            *coef               = br.read_s(16)?;
        }
        Ok(())
    }
    fn read_data(&mut self, br: &mut BitReader, dst: &mut [i32], kb: u8, mb: u8, bits: u8) -> DecoderResult<()> {
        let mut history = u32::from(mb);
        let mut add = 0;
        let dst_len = dst.len();
        let mut zero_run = 0;
        for (i, samp) in dst.iter_mut().enumerate() {
            if zero_run > 0 {
                *samp = 0;
                zero_run -= 1;
                continue;
            }
            let k = ((31 - ((history >> 9) + 3).leading_zeros()) as u8).min(kb);

            let val = read_code(br, k, bits)? + add;
            add = 0;
            *samp = if (val & 1) == 0 { val as i32 >> 1 } else { -(val as i32 >> 1) - 1 };

            if val > 0xFFFF {
                history = 0xFFFF;
            } else {
                history -= (history * u32::from(self.factor)) >> 9;
                history += val * u32::from(self.factor)
            }

            if history < 128 && (i + 1 < dst_len) {
                let k = (((history as u8).leading_zeros() as u8) + (((history + 16) >> 6) as u8)).min(kb);
                zero_run = read_code(br, k, 16)?;
                if zero_run < 0x10000 {
                    add = 1;
                }
                history = 0;
            }
        }
        Ok(())
    }
    fn lpc_pred(&mut self, samps: &mut [i32], bits: u8) {
        if self.mode == 15 || self.order == 31 {
            for i in 1..samps.len() {
                samps[i] = clip_sample(samps[i].wrapping_add(samps[i - 1]), bits);
            }
        }
        if self.order == 0 {
            return;
        }
        for i in 0..self.order {
            samps[i + 1] = clip_sample(samps[i + 1].wrapping_add(samps[i]), bits);
        }
        for i in self.order+1..samps.len() {
            let mut diff = samps[i];

            let el0 = samps[i - self.order - 1];
            let mut sum = 0i32;
            for j in 0..self.order {
                sum = sum.wrapping_add((samps[i - self.order + j] - el0).wrapping_mul(self.filt[j]));
            }
            sum = (sum + ((1 << self.shift) >> 1)) >> self.shift;
            samps[i] = clip_sample(samps[i].wrapping_add(el0).wrapping_add(sum), bits);

            if diff != 0 {
                let sign = if diff > 0 { 1 } else { -1 };
                for j in 0..self.order {
                    if diff * sign <= 0 {
                        break;
                    }
                    let cur_diff = el0 - samps[i - self.order + j];
                    let dsign = if cur_diff > 0 { 1 } else if cur_diff < 0 { -1 } else { 0 } * sign;
                    self.filt[j] -= dsign;
                    diff -= ((cur_diff * dsign) >> self.shift) * ((j + 1) as i32);
                }
            }
        }
    }
}

impl ALACDecoder {
    fn new() -> Self {
        Self {
            ainfo:      NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 1),
            chmap:      NAChannelMap::new(),
            bits:       0,
            frame_len:  0,
            samples_l:  Vec::new(),
            samples_r:  Vec::new(),
            cur_len:    0,

            pb:         0,
            mb:         0,
            kb:         0,
        }
    }
    #[allow(clippy::collapsible_if)]
    fn decode_elem(&mut self, br: &mut BitReader, stereo: bool) -> DecoderResult<()> {
        let _element_instance           = br.read(4)?;
                                          br.skip(12)?;
        let partial                     = br.read_bool()?;
        let shift                       = (br.read(2)? * 8) as u8;
        let escape                      = br.read_bool()?;
        self.cur_len = if partial {
                                          br.read(32)? as usize
            } else { self.frame_len };

        if !escape {
            validate!(shift <= self.bits);
            let sbits = self.bits - shift + if stereo { 1 } else { 0 };
            let channels = if stereo { 2 } else { 1 };

            let mix_bits                = br.read(8)?;
            let mix_res                 = br.read_s(8)?;
            let mut pinfo = [PredInfo::default(); 2];
            for info in pinfo.iter_mut().take(channels) {
                info.read_header(br, self.pb)?;
            }
            let ebits_pos = br.tell() as u32;
                                          br.skip((shift as u32) * (self.cur_len as u32) * (channels as u32))?;
            pinfo[0].read_data(br, &mut self.samples_l[..self.cur_len], self.kb, self.mb, sbits)?;
            pinfo[0].lpc_pred(&mut self.samples_l[..self.cur_len], sbits);
            if stereo {
                pinfo[1].read_data(br, &mut self.samples_r[..self.cur_len], self.kb, self.mb, sbits)?;
                pinfo[1].lpc_pred(&mut self.samples_r[..self.cur_len], sbits);
            }
            if stereo && mix_res != 0 {
                let weight = mix_res;
                for (l, r) in self.samples_l[..self.cur_len].iter_mut().zip(self.samples_r[..self.cur_len].iter_mut()) {
                    let mut a = *l;
                    let b = *r;
                    a -= (b * weight) >> mix_bits;
                    *l = a + b;
                    *r = a;
                }
            }
            if shift > 0 {
                let end_pos = br.tell() as u32;
                br.seek(ebits_pos)?;
                for i in 0..self.cur_len {
                    self.samples_l[i] = (self.samples_l[i] << shift) | (br.read(shift)? as i32);
                    if stereo {
                        self.samples_r[i] = (self.samples_r[i] << shift) | (br.read(shift)? as i32);
                    }
                }
                br.seek(end_pos)?;
            }
        } else {
            if !stereo {
                for dst in self.samples_l.iter_mut().take(self.cur_len) {
                    *dst                = br.read_s(self.bits)? << shift;
                }
            } else {
                for (l, r) in self.samples_l.iter_mut().zip(self.samples_r.iter_mut()).take(self.cur_len) {
                    *l                  = br.read_s(self.bits)? << shift;
                    *r                  = br.read_s(self.bits)? << shift;
                }
            }
        }


        Ok(())
    }
    fn output_i16(&self, dst: &mut [i16], stride: usize, shift: u8, stereo: bool) {
        if !stereo {
            for (dst, &src) in dst[..self.frame_len].iter_mut().zip(self.samples_l.iter()) {
                *dst = (src << shift) as i16;
            }
        } else {
            let (l, r) = dst.split_at_mut(stride);
            for i in 0..self.frame_len {
                l[i] = (self.samples_l[i] << shift) as i16;
                r[i] = (self.samples_r[i] << shift) as i16;
            }
        }
    }
    fn output_i32(&self, dst: &mut [i32], stride: usize, shift: u8, stereo: bool) {
        if !stereo {
            for (dst, &src) in dst[..self.frame_len].iter_mut().zip(self.samples_l.iter()) {
                *dst = src << shift;
            }
        } else {
            let (l, r) = dst.split_at_mut(stride);
            for i in 0..self.frame_len {
                l[i] = self.samples_l[i] << shift;
                r[i] = self.samples_r[i] << shift;
            }
        }
    }
}

fn skip_dse(br: &mut BitReader) -> DecoderResult<()> {
                                          br.skip(4)?;
    let align                           = br.read_bool()?;
    let mut count                       = br.read(8)?;
    if count == 255 {
        count                          += br.read(8)?;
    }
    if align {
                                          br.align();
    }
                                          br.skip(count * 8)?;
    Ok(())
}

fn skip_fil(br: &mut BitReader) -> DecoderResult<()> {
    let mut count                       = br.read(4)?;
    if count == 15 {
        count                           = 14 + br.read(8)?;
    }
                                          br.skip(count * 8)?;
    Ok(())
}

impl NADecoder for ALACDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let (NACodecTypeInfo::Audio(_), Some(edata)) = (info.get_properties(), info.get_extradata()) {
            validate!(edata.len() >= 12 + 24);

            let mut mr = MemoryReader::new_read(&edata);
            let mut br = ByteReader::new(&mut mr);
            let len                         = br.read_u32be()? as usize;
            validate!(len == 12 + 24);
            let fcc                         = br.read_tag()?;
            validate!(&fcc == b"alac");
            let _flags                      = br.read_u32be()?;

            // config
            let frame_len                   = br.read_u32be()? as usize;
            let version                     = br.read_byte()?;
            validate!(version == 0);
            let bits                        = br.read_byte()?;
            validate!((8..=32).contains(&bits));
            self.pb                         = br.read_byte()?;
            validate!(self.pb == 40);
            self.mb                         = br.read_byte()?;
            validate!(self.mb == 10);
            self.kb                         = br.read_byte()?;
            validate!(self.kb == 14);
            let channels                    = br.read_byte()?;
            validate!(channels > 0);
            let max_run                     = br.read_u16be()?;
            validate!(max_run == 255);
            let _max_frame_bytes            = br.read_u32be()? as usize;
            let _avg_bitrate                = br.read_u32be()? as usize;
            let sample_rate                 = br.read_u32be()?;

            let mut chmap = match channels {
                    1 => "C",
                    2 => "L,R",
                    3 => "C,L,R",
                    4 => "C,L,R,Cs",
                    5 => "C,L,R,Ls,Rs",
                    6 => "C,L,R,Ls,Rs,LFE",
                    7 => "C,L,R,Ls,Rs,Cs,LFE",
                    8 => "C,Lc,Rc,L,R,Ls,Rs,LFE",
                    _ => return Err(DecoderError::NotImplemented),
                };
            if len + 24 <= edata.len() {
                let len2                    = br.read_u32be()? as usize;
                let id                      = br.read_tag()?;
                let _flags                  = br.read_u32be()?;
                if (len2 == 24) && (&id == b"chan") {
                    let layout              = br.read_u32be()?;
                    let _rsvd1              = br.read_u32be()?; // 0
                    let _rsvd2              = br.read_u32be()?; // 0
                    if (layout as u8) == channels {
                        chmap = match layout {
                                0x640001 => "C",
                                0x640002 => "L,R",
                                0x710003 => "C,L,R",
                                0x740004 => "C,L,R,Cs",
                                0x780005 => "C,L,R,Ls,Rs",
                                0x7C0006 => "C,L,R,Ls,Rs,LFE",
                                0x8E0007 => "C,L,R,Ls,Rs,Cs,LFE",
                                0x7F0008 => "C,Lc,Rc,L,R,Ls,Rs,LFE",
                                _        => return Err(DecoderError::NotImplemented),
                            };
                    }
                }
            }

            self.ainfo = NAAudioInfo::new(sample_rate, channels, if bits <= 16 { SND_S16P_FORMAT } else { SND_S32P_FORMAT }, frame_len);
            self.chmap = NAChannelMap::from_str(chmap).unwrap();
            self.bits = bits;
            self.frame_len = frame_len;
            self.samples_l = vec![0; frame_len];
            self.samples_r = vec![0; frame_len];

            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let src = pkt.get_buffer();
            let channels = self.chmap.num_channels();
            let mut abuf = alloc_audio_buffer(self.ainfo, self.frame_len, self.chmap.clone())?;

            let mut br = BitReader::new(&src, BitReaderMode::BE);

            let mut channels_left = channels;
            if let Some(mut adata) = abuf.get_abuf_i16() {
                let shift = 16 - self.bits;
                let stride = adata.get_stride();
                let dst = adata.get_data_mut().unwrap();
                let mut off = 0;

                while br.left() >= 3 {
                    let tag             = br.read(3)?;
                    match tag {
                        0 | 3 => {
                            validate!(channels_left >= 1);
                            self.decode_elem(&mut br, false)?;
                            self.output_i16(&mut dst[off..], stride, shift, false);
                            off += stride;
                            channels_left -= 1;
                        },
                        1 => {
                            validate!(channels_left >= 2);
                            self.decode_elem(&mut br, true)?;
                            self.output_i16(&mut dst[off..], stride, shift, true);
                            off += stride * 2;
                            channels_left -= 2;
                        },
                        4 => skip_dse(&mut br)?,
                        6 => skip_fil(&mut br)?,
                        7 => break,
                        _ => return Err(DecoderError::InvalidData),
                    };
                }
            } else if let Some(mut adata) = abuf.get_abuf_i32() {
                let shift = 32 - self.bits;
                let stride = adata.get_stride();
                let dst = adata.get_data_mut().unwrap();
                let mut off = 0;

                while br.left() >= 3 {
                    let tag             = br.read(3)?;
                    match tag {
                        0 | 3 => {
                            validate!(channels_left >= 1);
                            self.decode_elem(&mut br, false)?;
                            self.output_i32(&mut dst[off..], stride, shift, false);
                            off += stride;
                            channels_left -= 1;
                        },
                        1 => {
                            validate!(channels_left >= 2);
                            self.decode_elem(&mut br, true)?;
                            self.output_i32(&mut dst[off..], stride, shift, true);
                            off += stride * 2;
                            channels_left -= 2;
                        },
                        4 => skip_dse(&mut br)?,
                        6 => skip_fil(&mut br)?,
                        7 => break,
                        _ => return Err(DecoderError::InvalidData),
                    };
                }
            } else {
                return Err(DecoderError::Bug);
            }
            abuf.truncate_audio(self.cur_len);

            let mut frm = NAFrame::new_from_pkt(pkt, info.replace_info(NACodecTypeInfo::Audio(self.ainfo)), abuf);
            frm.set_duration(Some(self.cur_len as u64));
            frm.set_keyframe(true);
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for ALACDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(ALACDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::*;
    use crate::qt_register_all_decoders;
    use nihav_commonfmt::generic_register_all_demuxers;
    #[test]
    fn test_alac_16bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.m4a
        test_decoding("mov", "alac", "assets/LLaudio/alac/luckynight.m4a", Some(48000 * 16), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x8b6562ac, 0x95981733, 0x47e14709, 0x45d4f05a]));
    }
    #[test]
    fn test_alac_6ch() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/ALAC/ALAC_6ch.mov
        test_decoding("mov", "alac", "assets/LLaudio/alac/ALAC_6ch.mov", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x0356ff3d, 0x1ddd3684, 0xb4da8b00, 0x8e8671a7]));
    }
    #[test]
    fn test_alac_24bit() {
        let mut dmx_reg = RegisteredDemuxers::new();
        generic_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        qt_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/A-codecs/lossless/ALAC/ALAC_24bits2.mov
        test_decoding("mov", "alac", "assets/LLaudio/alac/ALAC_24bits2.mov", None, &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0x15d58ed9, 0x9ee74f5e, 0x0fb82c0b, 0x27da35f9]));
    }
}
