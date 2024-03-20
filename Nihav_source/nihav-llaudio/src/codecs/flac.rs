use nihav_core::codecs::*;
use nihav_core::io::byteio::*;
use nihav_core::io::bitreader::*;
use nihav_core::io::intcode::*;

const MAX_SAMPLES: usize = 32768;

#[derive(Clone,Copy,PartialEq)]
enum StereoMode {
    Normal,
    LeftSide,
    SideRight,
    MidSide,
}

struct FlacDecoder {
    ainfo:          NAAudioInfo,
    chmap:          NAChannelMap,
    min_blk_size:   usize,
    max_blk_size:   usize,
    min_frm_size:   usize,
    max_frm_size:   usize,
    channels:       u8,
    bits:           u8,
    srate:          u32,
    residues:       [Vec<i32>; 8],
}

fn decode_partition(br: &mut BitReader, dst: &mut [i32], k: u8) -> DecoderResult<()> {
    for el in dst.iter_mut() {
        let val                         = br.read_code(UintCodeType::Rice(k))?;
        if (val & 1) == 0 {
            *el = (val >> 1) as i32;
        } else {
            *el = -(((val + 1) >> 1) as i32);
        }
    }
    Ok(())
}

fn decode_residual(br: &mut BitReader, dst: &mut [i32], order: usize) -> DecoderResult<()> {
    let mode                            = br.read(2)?;
    validate!(mode < 2);
    let rice_k = if mode == 0 { 4 } else { 5 };
    let esc = (1 << rice_k) - 1;

    let num_partitions                  = 1 << br.read(4)?;
    let tot_size = dst.len() + order;
    validate!((tot_size % num_partitions) == 0);
    let psize = tot_size / num_partitions;
    validate!(psize >= order);
    let mut off = psize - order;
    let k                               = br.read(rice_k)? as u8;
    if k != esc {
        decode_partition(br, &mut dst[..off], k)?;
    } else {
        let bits                        = br.read(5)? as u8;
        for el in dst.iter_mut().take(off) {
            *el                         = br.read_s(bits)?;
        }
    }
    for _ in 1..num_partitions {
        let k                           = br.read(rice_k)? as u8;
        if k != esc {
            decode_partition(br, &mut dst[off..][..psize], k)?;
        } else {
            let bits                    = br.read(5)? as u8;
            for el in dst[off..].iter_mut().take(psize) {
                *el                     = br.read_s(bits)?;
            }
        }
        off += psize;
    }

    Ok(())
}

fn apply_fixed_predictor(dst: &mut [i32], order: usize) {
    match order {
        1 => {
            let mut last = dst[0];
            for el in dst.iter_mut().skip(1) {
                *el += last;
                last = *el;
            }
        },
        2 => {
            let mut last0 = dst[1];
            let mut last1 = last0 - dst[0];
            for el in dst.iter_mut().skip(2) {
                last1 += *el;
                last0 += last1;
                *el = last0;
            }
        },
        3 => {
            let mut last0 = dst[2];
            let mut last1 = last0 - dst[1];
            let mut last2 = last1 - dst[1] + dst[0];
            for el in dst.iter_mut().skip(3) {
                last2 += *el;
                last1 += last2;
                last0 += last1;
                *el = last0;
            }
        },
        4 => {
            let mut last0 = dst[3];
            let mut last1 = last0 - dst[2];
            let mut last2 = last1 - dst[2] + dst[1];
            let mut last3 = last2 - dst[2] + 2 * dst[1] - dst[0];
            for el in dst.iter_mut().skip(4) {
                last3 += *el;
                last2 += last3;
                last1 += last2;
                last0 += last1;
                *el = last0;
            }
        },
        _ => unreachable!(),
    };
}

fn apply_lpc(dst: &mut [i32], filt: &[i32; 32], order: usize, shift: u8) {
    for i in order..dst.len() {
        let mut sum = 0i64;
        for (coef, filt) in dst[i - order..].iter().take(order).zip(filt.iter()) {
            sum += i64::from(*coef) * i64::from(*filt);
        }
        dst[i] += (sum >> shift) as i32;
    }
}

impl FlacDecoder {
    fn new() -> Self {
        Self {
            ainfo:          NAAudioInfo::new(0, 1, SND_S16P_FORMAT, 0),
            chmap:          NAChannelMap::new(),
            min_blk_size:   0,
            max_blk_size:   0,
            min_frm_size:   0,
            max_frm_size:   0,
            channels:       0,
            bits:           0,
            srate:          0,
            residues:       [vec![0; MAX_SAMPLES], vec![0; MAX_SAMPLES],
                             vec![0; MAX_SAMPLES], vec![0; MAX_SAMPLES],
                             vec![0; MAX_SAMPLES], vec![0; MAX_SAMPLES],
                             vec![0; MAX_SAMPLES], vec![0; MAX_SAMPLES]],
        }
    }
    fn apply_chmod(&mut self, blocksize: usize, chmod: StereoMode) {
        match chmod {
            StereoMode::Normal => {},
            StereoMode::LeftSide => {
                for i in 0..blocksize {
                    self.residues[1][i] = self.residues[0][i].wrapping_sub(self.residues[1][i]);
                }
            },
            StereoMode::SideRight => {
                for i in 0..blocksize {
                    self.residues[0][i] = self.residues[0][i].wrapping_add(self.residues[1][i]);
                }
            },
            StereoMode::MidSide => {
                for i in 0..blocksize {
                    let r = self.residues[0][i].wrapping_sub(self.residues[1][i] >> 1);
                    self.residues[0][i] = self.residues[1][i].wrapping_add(r);
                    self.residues[1][i] = r;
                }
            },
        };
    }
    fn decode_subframe(&mut self, br: &mut BitReader, channel: usize, blocksize: usize, mut samp_bits: u8) -> DecoderResult<()> {
        let marker                      = br.read(1)?;
        validate!(marker == 0);
        let sftype                      = br.read(6)?;

        if br.read_bool()? {
            let nbits                   = br.read_code(UintCodeType::UnaryZeroes)?;
            validate!(nbits < 32 && samp_bits > nbits as u8);
            samp_bits -= nbits as u8;
        }

        let dst = &mut self.residues[channel][..blocksize];
        match sftype {
            0x00 => {
                let val                 = br.read_s(samp_bits)?;
                for el in dst.iter_mut() {
                    *el = val;
                }
            },
            0x01 => {
                for el in dst.iter_mut() {
                    *el                 = br.read_s(samp_bits)?;
                }
            },
            0x08..=0x0C => {
                let order = (sftype - 0x08) as usize;
                for el in dst.iter_mut().take(order) {
                    *el                 = br.read_s(samp_bits)?;
                }
                decode_residual(br, &mut dst[order..], order)?;
                if order > 0 {
                    apply_fixed_predictor(dst, order);
                }
            },
            0x20..=0x3F => {
                let order = (sftype - 0x20) as usize + 1;
                for el in dst.iter_mut().take(order) {
                    *el                 = br.read_s(samp_bits)?;
                }
                let precision           = br.read(4)? as u8 + 1;
                validate!(precision < 16);
                let shift               = br.read(5)? as u8;
                let mut filter = [0i32; 32];
                for el in filter[..order].iter_mut().rev() {
                    *el                 = br.read_s(precision)?;
                }
                decode_residual(br, &mut dst[order..], order)?;
                apply_lpc(dst, &filter, order, shift);
            },
            _ => return Err(DecoderError::InvalidData),
        };

        Ok(())
    }
}

impl NADecoder for FlacDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        const DEFAULT_CHANNEL_MAPS: [&str; 8] = [
            "C",
            "L,R",
            "L,R,C",
            "L,R,Ls,Rs",
            "L,R,C,Ls,Rs",
            "L,R,C,LFE,Ls,Rs",
            "L,R,C,LFE,Cs,Ls,Rs",
            "L,R,C,LFE,Ls,Rs,Lss,Rss"
        ];
        if let NACodecTypeInfo::Audio(_ainfo) = info.get_properties() {
            if let Some(buf) = info.get_extradata() {
                validate!(buf.len() >= 22);

                let mut mr = MemoryReader::new_read(&buf);
                let mut br = ByteReader::new(&mut mr);

                self.min_blk_size       = br.read_u16be()? as usize;
                self.max_blk_size       = br.read_u16be()? as usize;
                self.min_frm_size       = br.read_u24be()? as usize;
                self.max_frm_size       = br.read_u24be()? as usize;
                let tmp                 = br.read_u64be()?;
                self.srate      = (tmp >> 44) as u32;
                self.channels   = (((tmp >> 41) & 7) + 1) as u8;
                self.bits       = (((tmp >> 36) & 0x1F) + 1) as u8;
                //let tot_samples        = tmp & ((1 << 36) - 1);

                self.chmap = NAChannelMap::from_str(DEFAULT_CHANNEL_MAPS[(self.channels - 1) as usize]).unwrap();
                let fmt = match self.bits {
                        8 | 12 | 16 => SND_S16P_FORMAT,
                        24 => SND_S32P_FORMAT,
                        _ => return Err(DecoderError::NotImplemented),
                    };

                self.ainfo = NAAudioInfo::new(self.srate, self.channels, fmt, self.max_blk_size.max(1));
                Ok(())
            } else {
                Err(DecoderError::InvalidData)
            }
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            let pktbuf = pkt.get_buffer();
            validate!(pktbuf.len() >= 9.max(self.min_frm_size));

            let ref_crc = read_u16be(&pktbuf[pktbuf.len() - 2..]).unwrap_or(0);
            let mut crc = 0;
            for el in pktbuf.iter().take(pktbuf.len() - 2) {
                crc = update_crc16(crc, *el);
            }
            if crc != ref_crc {
                return Err(DecoderError::ChecksumError);
            }

            let mut br = BitReader::new(&pktbuf, BitReaderMode::BE);

            let sync                    = br.read(14)?;
            validate!(sync == 0x3FFE);
                                          br.skip(1)?;
            let _blocking               = br.read(1)?;
            let bsize_idx               = br.read(4)?;
            let srate_idx               = br.read(4)?;
            let chan_idx                = br.read(4)?;
            let bits_idx                = br.read(3)?;
                                          br.skip(1)?;
            // UTF-8 encoded block or sample number
            let byte                    = br.read(8)? as u8;
            let len = (!byte).leading_zeros();
            validate!(len <= 5 && len != 1);
            if len > 1 {
                for _ in 1..len {
                    let byte            = br.read(8)?;
                    validate!((byte & 0xC0) == 0x80);
                }
            }
            let blocksize = match bsize_idx {
                    0       => return Err(DecoderError::InvalidData),
                    1       => 192,
                    2..=5   => 576 << (bsize_idx - 2),
                    6       =>            br.read(8)? as usize + 1,
                    7       =>            br.read(16)? as usize + 1,
                    _       => 256 << (bsize_idx - 8),
                };
            let srate = match srate_idx {
                     0 => self.srate,
                     1 => 88200,
                     2 => 176400,
                     3 => 192000,
                     4 => 8000,
                     5 => 16000,
                     6 => 22050,
                     7 => 24000,
                     8 => 32000,
                     9 => 44100,
                    10 => 48000,
                    11 => 96000,
                    12 =>                 br.read(8)? * 1000,
                    13 =>                 br.read(16)?,
                    14 =>                 br.read(16)? * 10,
                     _ => return Err(DecoderError::InvalidData),
                };
            validate!(srate != 0 && srate == self.srate);
            let (channels, chmod) = match chan_idx {
                    0..=7 => (chan_idx as u8 + 1, StereoMode::Normal),
                    8     => (2, StereoMode::LeftSide),
                    9     => (2, StereoMode::SideRight),
                    10    => (2, StereoMode::MidSide),
                    _     => return Err(DecoderError::InvalidData),
                };
            validate!(channels == self.channels);
            let bits = match bits_idx {
                    0 => self.bits,
                    1 => 8,
                    2 => 12,
                    4 => 16,
                    5 => 20,
                    6 => 24,
                    _ => return Err(DecoderError::InvalidData),
                };
            validate!(bits == self.bits);

            let end = br.tell() / 8;
            let ref_crc                 = br.read(8)? as u8;
            let mut crc = 0;
            for el in pktbuf.iter().take(end) {
                crc = update_crc8(crc, *el);
            }
            if crc != ref_crc {
                return Err(DecoderError::ChecksumError);
            }

            for ch in 0..(channels as usize) {
                let samp_bits = match (chmod, ch) {
                        (StereoMode::LeftSide,  1) |
                        (StereoMode::SideRight, 0) |
                        (StereoMode::MidSide,   1) => self.bits + 1,
                        _ => self.bits,
                    };
                validate!(samp_bits <= 32);
                self.decode_subframe(&mut br, ch, blocksize, samp_bits)?;
            }
            if channels == 2 {
                self.apply_chmod(blocksize, chmod);
            }

            let mut abuf = alloc_audio_buffer(self.ainfo, blocksize, self.chmap.clone())?;
            let postshift = if self.bits == 24 { 8 } else { 16 - self.bits };
            match abuf {
                NABufferType::AudioI16(ref mut adata) => {
                    let stride = adata.get_stride();
                    let dst = adata.get_data_mut().unwrap();
                    let mut off = 0;
                    for residues in self.residues.iter().take(channels as usize) {
                        let dst = &mut dst[off..][..blocksize];
                        for (dst, src) in dst.iter_mut().zip(residues.iter()) {
                            *dst = (*src << postshift) as i16;
                        }
                        off += stride;
                    }
                },
                NABufferType::AudioI32(ref mut adata) => {
                    let stride = adata.get_stride();
                    let dst = adata.get_data_mut().unwrap();
                    let mut off = 0;
                    for residues in self.residues.iter().take(channels as usize) {
                        let dst = &mut dst[off..][..blocksize];
                        for (dst, src) in dst.iter_mut().zip(residues.iter()) {
                            *dst = *src << postshift;
                        }
                        off += stride;
                    }
                },
                _ => unreachable!(),
            };

            let mut frm = NAFrame::new_from_pkt(pkt, info, abuf);
            frm.set_duration(Some(blocksize as u64));
            Ok(frm.into_ref())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn flush(&mut self) {
    }
}

impl NAOptionHandler for FlacDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(FlacDecoder::new())
}

#[derive(Clone,Copy,Default)]
struct FrameHeader {
    blocksize:      u32,
    srate:          u32,
    channels:       u8,
    bits:           u8,
    time:           u64,
    blk_strat:      bool,
}

#[derive(Default)]
struct FLACPacketiser {
    hdr:        FrameHeader,
    buf:        Vec<u8>,
    ref_crc:    u16,
    cur_crc:    u16,
    end:        usize,
    hdr_ok:     bool,
}

fn read_utf8(br: &mut BitReader) -> DecoderResult<u64> {
    let byte                    = br.read(8)? as u8;
    let len = (!byte).leading_zeros();
    if (len == 1) || (len > 5) {
        return Err(DecoderError::InvalidData);
    }
    if len > 1 {
        let mut val = u64::from(byte << len >> len);
        for _ in 1..len {
            let byte            = br.read(8)?;
            if (byte & 0xC0) != 0x80 {
                return Err(DecoderError::InvalidData);
            }
            val = (val << 6) | u64::from(byte & 0x3F);
        }
        Ok(val)
    } else {
        Ok(u64::from(byte))
    }
}

impl FLACPacketiser {
    fn new() -> Self { Self::default() }
    fn parse_header(&self) -> DecoderResult<FrameHeader> {
        if self.buf.len() < 5 {
            return Err(DecoderError::ShortData);
        }
        let mut br = BitReader::new(&self.buf, BitReaderMode::BE);
        let sync_code                   = br.read(14)?;
        if sync_code != 0x3FFE {
            return Err(DecoderError::InvalidData);
        }
        let marker                      = br.read(1)?;
        if marker != 0 {
            return Err(DecoderError::InvalidData);
        }
        let blk_strat                   = br.read_bool()?;
        let bsize                       = br.read(4)?;
        let srate_idx                   = br.read(4)? as u8;
        let chan_idx                    = br.read(4)? as u8;
        let bits = match br.read(3)? {
                0 => 0,
                1 => 8,
                2 => 12,
                4 => 16,
                5 => 20,
                6 => 24,
                _ => return Err(DecoderError::InvalidData),
            };
        let marker                      = br.read(1)?;
        if marker != 0 {
            return Err(DecoderError::InvalidData);
        }

        let time = read_utf8(&mut br)?;

        let blocksize = match bsize {
                1 => 192,
                2..=5 => 576 << (bsize - 2),
                6 => br.read(8)? + 1,
                7 => br.read(16)? + 1,
                8..=15 => 256 << (bsize - 8),
                _ => return Err(DecoderError::InvalidData),
            };
        let srate = match srate_idx {
                0 => 0,
                1 => 88200,
                2 => 176400,
                3 => 192000,
                4 => 8000,
                5 => 16000,
                6 => 22050,
                7 => 24000,
                8 => 32000,
                9 => 44100,
                10 => 48000,
                11 => 96000,
                12 => br.read(8)? * 1000,
                13 => br.read(16)?,
                14 => br.read(16)? * 10,
                _ => return Err(DecoderError::InvalidData),
            };
        let channels = match chan_idx {
                0..=7 => chan_idx + 1,
                8 | 9 | 10 => 2,
                _ => return Err(DecoderError::InvalidData),
            };

        let hdr_size = br.tell() / 8;
        let ref_crc                     = br.read(8)? as u8;
        let mut crc = 0;
        for &b in self.buf[..hdr_size].iter() {
            crc = update_crc8(crc, b);
        }
        if crc != ref_crc {
            return Err(DecoderError::ChecksumError);
        }

        Ok(FrameHeader{ blk_strat, time, srate, channels, bits, blocksize })
    }
}

impl NAPacketiser for FLACPacketiser {
    fn add_data(&mut self, src: &[u8]) -> bool {
        self.buf.extend_from_slice(src);
        self.buf.len() < 4096
    }
    fn parse_stream(&mut self, id: u32) -> DecoderResult<NAStreamRef> {
        let hdr = self.parse_header()?;
        let ainfo = NAAudioInfo::new(hdr.srate, hdr.channels, if hdr.bits <= 16 { SND_S16P_FORMAT } else { SND_S32P_FORMAT }, hdr.blocksize as usize);
        let info = NACodecInfo::new("flac", NACodecTypeInfo::Audio(ainfo), None);
        Ok(NAStream::new(StreamType::Audio, id, info, 1, hdr.srate, 0).into_ref())
    }
    fn skip_junk(&mut self) -> DecoderResult<usize> {
        Err(DecoderError::NotImplemented)
    }
    fn get_packet(&mut self, stream: NAStreamRef) -> DecoderResult<Option<NAPacket>> {
        if self.end == self.buf.len() || self.buf.len() < 5 {
            return Err(DecoderError::ShortData);
        }
        if !self.hdr_ok {
            self.hdr = self.parse_header()?;
            self.hdr_ok = true;
            self.cur_crc = 0;
            for i in 0..5 {
                self.cur_crc = update_crc16(self.cur_crc, self.buf[i]);
            }
            self.end = 5;
        }
        while self.end < self.buf.len() {
            let b = self.buf[self.end];
            self.end += 1;
            match self.end {
                0..=5 => unreachable!(),
                6 => self.ref_crc = u16::from(b),
                7 => self.ref_crc = (self.ref_crc << 8) | u16::from(b),
                _ => {
                    let bbb = (self.ref_crc >> 8) as u8;
                    self.ref_crc = (self.ref_crc << 8) | u16::from(b);
                    self.cur_crc = update_crc16(self.cur_crc, bbb);
                    let mut found = self.ref_crc == self.cur_crc;
                    if self.end + 2 < self.buf.len() {
                        let b1 = self.buf[self.end];
                        let b2 = self.buf[self.end + 1];
                        if b1 != 0xFF || (b2 & 0xFC) != 0xF8 {
                            found = false;
                        }
                    }
                    if found {
                        let mut data = Vec::with_capacity(self.end);
                        data.extend_from_slice(&self.buf[..self.end]);
                        self.buf.drain(..self.end);
                        let mut ts = NATimeInfo::new(None, None, Some(u64::from(self.hdr.blocksize)), 1, self.hdr.srate);
                        ts.pts = if self.hdr.blk_strat {
                                Some(self.hdr.time)
                            } else {
                                Some(self.hdr.time * u64::from(self.hdr.blocksize))
                            };
                        self.end = 0;
                        self.hdr_ok = false;

                        return Ok(Some(NAPacket::new(stream, ts, true, data)));
                    }
                },
            }
        }
        Ok(None)
    }
    fn reset(&mut self) {
        self.buf.clear();
        self.end = 0;
        self.hdr_ok = false;
    }
    fn bytes_left(&self) -> usize { self.buf.len() }
}

pub fn get_packetiser() -> Box<dyn NAPacketiser + Send> {
    Box::new(FLACPacketiser::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::*;
    use nihav_core::demuxers::*;
    use nihav_codec_support::test::dec_video::*;
    use crate::llaudio_register_all_decoders;
    use crate::llaudio_register_all_demuxers;
    // sample: https://samples.mplayerhq.hu/A-codecs/lossless/luckynight.flac
    #[test]
    fn test_flac() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        llaudio_register_all_decoders(&mut dec_reg);

        test_decoding("flac", "flac", "assets/LLaudio/luckynight.flac", Some(6), &dmx_reg, &dec_reg,
                      ExpectedTestResult::MD5([0xe689787a, 0x032a98f7, 0xeb6e64f4, 0xfa652132]));
    }
    use std::io::{Read, Seek, SeekFrom};
    #[test]
    fn test_flac_packetiser() {
        let mut dmx_reg = RegisteredDemuxers::new();
        llaudio_register_all_demuxers(&mut dmx_reg);
        let dmx_f = dmx_reg.find_demuxer("flac").unwrap();
        let mut file = std::fs::File::open("assets/LLaudio/luckynight.flac").unwrap();
        let mut fr = FileReader::new_read(&mut file);
        let mut br = ByteReader::new(&mut fr);
        let mut dmx = create_demuxer(dmx_f, &mut br).unwrap();

        let mut pkt_sizes = Vec::new();
        while let Ok(pkt) = dmx.get_frame() {
            pkt_sizes.push(pkt.get_buffer().len());
        }

        let mut file = std::fs::File::open("assets/LLaudio/luckynight.flac").unwrap();
        file.seek(SeekFrom::Start(0x115E)).unwrap();

        let mut pkts = super::FLACPacketiser::new();
        let mut buf = [0; 8192];
        for _ in 0..16 {
            file.read_exact(&mut buf).unwrap();
            pkts.add_data(&buf);
        }
        let stream = pkts.parse_stream(0).unwrap();
        let mut pkt_sizes2 = Vec::new();
        let mut piter = pkt_sizes.iter();
        loop {
            let res = pkts.get_packet(stream.clone());
            match res {
                Ok(Some(pkt)) => {
                    assert_eq!(*piter.next().unwrap(), pkt.get_buffer().len());
                    pkt_sizes2.push(pkt.get_buffer().len());
                    continue;
                },
                Ok(None) | Err(DecoderError::ShortData) => {},
                Err(err) => {
                    println!("error {:?}", err);
                    panic!("packetising error");
                },
            };
            let ret = file.read(&mut buf);
            match ret {
                Ok(0) => {
                    let res = pkts.get_packet(stream.clone());
                    match res {
                        Ok(Some(pkt)) => {
                            assert_eq!(*piter.next().unwrap(), pkt.get_buffer().len());
                            pkt_sizes2.push(pkt.get_buffer().len());
                            continue;
                        },
                        Ok(None) | Err(DecoderError::ShortData) => break,
                        Err(err) => {
                            println!("error {:?}", err);
                            panic!("packetising error");
                        },
                    };
                },
                Ok(size) => pkts.add_data(&buf[..size]),
                Err(err) => {
                    if err.kind() == std::io::ErrorKind::UnexpectedEof {
                        let res = pkts.get_packet(stream.clone());
                        match res {
                            Ok(Some(pkt)) => {
                                assert_eq!(*piter.next().unwrap(), pkt.get_buffer().len());
                                pkt_sizes2.push(pkt.get_buffer().len());
                                continue;
                            },
                            Ok(None) | Err(DecoderError::ShortData) => break,
                            Err(err) => {
                                println!("error {:?}", err);
                                panic!("packetising error");
                            },
                        };
                    } else {
                        println!(" {:?}", err.kind());
                        panic!("i/o error!");
                    }
                },
            };
        }
        assert_eq!(pkt_sizes.len(), pkt_sizes2.len());
    }
}

fn update_crc8(crc: u8, byte: u8) -> u8 {
    CRC8_TABLE[(crc ^ byte) as usize]
}

fn update_crc16(crc: u16, byte: u8) -> u16 {
    (crc << 8) ^ CRC16_TABLE[(((crc >> 8) as u8) ^ byte) as usize]
}

const CRC8_TABLE: [u8; 256] = [
    0x00, 0x07, 0x0E, 0x09, 0x1C, 0x1B, 0x12, 0x15,
    0x38, 0x3F, 0x36, 0x31, 0x24, 0x23, 0x2A, 0x2D,
    0x70, 0x77, 0x7E, 0x79, 0x6C, 0x6B, 0x62, 0x65,
    0x48, 0x4F, 0x46, 0x41, 0x54, 0x53, 0x5A, 0x5D,
    0xE0, 0xE7, 0xEE, 0xE9, 0xFC, 0xFB, 0xF2, 0xF5,
    0xD8, 0xDF, 0xD6, 0xD1, 0xC4, 0xC3, 0xCA, 0xCD,
    0x90, 0x97, 0x9E, 0x99, 0x8C, 0x8B, 0x82, 0x85,
    0xA8, 0xAF, 0xA6, 0xA1, 0xB4, 0xB3, 0xBA, 0xBD,
    0xC7, 0xC0, 0xC9, 0xCE, 0xDB, 0xDC, 0xD5, 0xD2,
    0xFF, 0xF8, 0xF1, 0xF6, 0xE3, 0xE4, 0xED, 0xEA,
    0xB7, 0xB0, 0xB9, 0xBE, 0xAB, 0xAC, 0xA5, 0xA2,
    0x8F, 0x88, 0x81, 0x86, 0x93, 0x94, 0x9D, 0x9A,
    0x27, 0x20, 0x29, 0x2E, 0x3B, 0x3C, 0x35, 0x32,
    0x1F, 0x18, 0x11, 0x16, 0x03, 0x04, 0x0D, 0x0A,
    0x57, 0x50, 0x59, 0x5E, 0x4B, 0x4C, 0x45, 0x42,
    0x6F, 0x68, 0x61, 0x66, 0x73, 0x74, 0x7D, 0x7A,
    0x89, 0x8E, 0x87, 0x80, 0x95, 0x92, 0x9B, 0x9C,
    0xB1, 0xB6, 0xBF, 0xB8, 0xAD, 0xAA, 0xA3, 0xA4,
    0xF9, 0xFE, 0xF7, 0xF0, 0xE5, 0xE2, 0xEB, 0xEC,
    0xC1, 0xC6, 0xCF, 0xC8, 0xDD, 0xDA, 0xD3, 0xD4,
    0x69, 0x6E, 0x67, 0x60, 0x75, 0x72, 0x7B, 0x7C,
    0x51, 0x56, 0x5F, 0x58, 0x4D, 0x4A, 0x43, 0x44,
    0x19, 0x1E, 0x17, 0x10, 0x05, 0x02, 0x0B, 0x0C,
    0x21, 0x26, 0x2F, 0x28, 0x3D, 0x3A, 0x33, 0x34,
    0x4E, 0x49, 0x40, 0x47, 0x52, 0x55, 0x5C, 0x5B,
    0x76, 0x71, 0x78, 0x7F, 0x6A, 0x6D, 0x64, 0x63,
    0x3E, 0x39, 0x30, 0x37, 0x22, 0x25, 0x2C, 0x2B,
    0x06, 0x01, 0x08, 0x0F, 0x1A, 0x1D, 0x14, 0x13,
    0xAE, 0xA9, 0xA0, 0xA7, 0xB2, 0xB5, 0xBC, 0xBB,
    0x96, 0x91, 0x98, 0x9F, 0x8A, 0x8D, 0x84, 0x83,
    0xDE, 0xD9, 0xD0, 0xD7, 0xC2, 0xC5, 0xCC, 0xCB,
    0xE6, 0xE1, 0xE8, 0xEF, 0xFA, 0xFD, 0xF4, 0xF3
];

const CRC16_TABLE: [u16; 256] = [
    0x0000, 0x8005, 0x800F, 0x000A, 0x801B, 0x001E, 0x0014, 0x8011,
    0x8033, 0x0036, 0x003C, 0x8039, 0x0028, 0x802D, 0x8027, 0x0022,
    0x8063, 0x0066, 0x006C, 0x8069, 0x0078, 0x807D, 0x8077, 0x0072,
    0x0050, 0x8055, 0x805F, 0x005A, 0x804B, 0x004E, 0x0044, 0x8041,
    0x80C3, 0x00C6, 0x00CC, 0x80C9, 0x00D8, 0x80DD, 0x80D7, 0x00D2,
    0x00F0, 0x80F5, 0x80FF, 0x00FA, 0x80EB, 0x00EE, 0x00E4, 0x80E1,
    0x00A0, 0x80A5, 0x80AF, 0x00AA, 0x80BB, 0x00BE, 0x00B4, 0x80B1,
    0x8093, 0x0096, 0x009C, 0x8099, 0x0088, 0x808D, 0x8087, 0x0082,
    0x8183, 0x0186, 0x018C, 0x8189, 0x0198, 0x819D, 0x8197, 0x0192,
    0x01B0, 0x81B5, 0x81BF, 0x01BA, 0x81AB, 0x01AE, 0x01A4, 0x81A1,
    0x01E0, 0x81E5, 0x81EF, 0x01EA, 0x81FB, 0x01FE, 0x01F4, 0x81F1,
    0x81D3, 0x01D6, 0x01DC, 0x81D9, 0x01C8, 0x81CD, 0x81C7, 0x01C2,
    0x0140, 0x8145, 0x814F, 0x014A, 0x815B, 0x015E, 0x0154, 0x8151,
    0x8173, 0x0176, 0x017C, 0x8179, 0x0168, 0x816D, 0x8167, 0x0162,
    0x8123, 0x0126, 0x012C, 0x8129, 0x0138, 0x813D, 0x8137, 0x0132,
    0x0110, 0x8115, 0x811F, 0x011A, 0x810B, 0x010E, 0x0104, 0x8101,
    0x8303, 0x0306, 0x030C, 0x8309, 0x0318, 0x831D, 0x8317, 0x0312,
    0x0330, 0x8335, 0x833F, 0x033A, 0x832B, 0x032E, 0x0324, 0x8321,
    0x0360, 0x8365, 0x836F, 0x036A, 0x837B, 0x037E, 0x0374, 0x8371,
    0x8353, 0x0356, 0x035C, 0x8359, 0x0348, 0x834D, 0x8347, 0x0342,
    0x03C0, 0x83C5, 0x83CF, 0x03CA, 0x83DB, 0x03DE, 0x03D4, 0x83D1,
    0x83F3, 0x03F6, 0x03FC, 0x83F9, 0x03E8, 0x83ED, 0x83E7, 0x03E2,
    0x83A3, 0x03A6, 0x03AC, 0x83A9, 0x03B8, 0x83BD, 0x83B7, 0x03B2,
    0x0390, 0x8395, 0x839F, 0x039A, 0x838B, 0x038E, 0x0384, 0x8381,
    0x0280, 0x8285, 0x828F, 0x028A, 0x829B, 0x029E, 0x0294, 0x8291,
    0x82B3, 0x02B6, 0x02BC, 0x82B9, 0x02A8, 0x82AD, 0x82A7, 0x02A2,
    0x82E3, 0x02E6, 0x02EC, 0x82E9, 0x02F8, 0x82FD, 0x82F7, 0x02F2,
    0x02D0, 0x82D5, 0x82DF, 0x02DA, 0x82CB, 0x02CE, 0x02C4, 0x82C1,
    0x8243, 0x0246, 0x024C, 0x8249, 0x0258, 0x825D, 0x8257, 0x0252,
    0x0270, 0x8275, 0x827F, 0x027A, 0x826B, 0x026E, 0x0264, 0x8261,
    0x0220, 0x8225, 0x822F, 0x022A, 0x823B, 0x023E, 0x0234, 0x8231,
    0x8213, 0x0216, 0x021C, 0x8219, 0x0208, 0x820D, 0x8207, 0x0202
];
