use nihav_core::codecs::*;

struct RawEncoder {
    stream: Option<NAStreamRef>,
    pkt:    Option<NAPacket>,
}

impl RawEncoder {
    fn new() -> Self {
        Self {
            stream: None,
            pkt:    None,
        }
    }
}

impl NAEncoder for RawEncoder {
    fn negotiate_format(&self, encinfo: &EncodeParameters) -> EncoderResult<EncodeParameters> {
        match encinfo.format {
            NACodecTypeInfo::None => {
                Ok(EncodeParameters {
                    format: NACodecTypeInfo::Video(NAVideoInfo::new(0, 0, false, YUV420_FORMAT)),
                    ..Default::default()
                })
            },
            NACodecTypeInfo::Video(_) => {
                let mut new_info = *encinfo;
                if let NACodecTypeInfo::Video(ref mut vinfo) = new_info.format {
                    if !vinfo.format.model.is_yuv() {
                        vinfo.format = YUV420_FORMAT;
                    }
                }
                Ok(new_info)
            },
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
        }
    }
    fn get_capabilities(&self) -> u64 { ENC_CAPS_CBR }
    fn init(&mut self, stream_id: u32, encinfo: EncodeParameters) -> EncoderResult<NAStreamRef> {
        match encinfo.format {
            NACodecTypeInfo::None => Err(EncoderError::FormatError),
            NACodecTypeInfo::Audio(_) => Err(EncoderError::FormatError),
            NACodecTypeInfo::Video(_) => {
                let info = NACodecInfo::new("rawvideo", encinfo.format, None);
                let mut stream = NAStream::new(StreamType::Video, stream_id, info, encinfo.tb_num, encinfo.tb_den, 0);
                stream.set_num(stream_id as usize);
                let stream = stream.into_ref();
                self.stream = Some(stream.clone());
                Ok(stream)
            }
        }
    }
    fn encode(&mut self, frm: &NAFrame) -> EncoderResult<()> {
        let buf = frm.get_buffer();
        let mut dbuf;
        match buf {
            NABufferType::Video(ref vbuf) => {
                let vinfo = vbuf.get_info();
                if !vinfo.format.model.is_yuv() || !vinfo.format.is_unpacked() {
                    return Err(EncoderError::NotImplemented);
                }

                let src = vbuf.get_data();
                dbuf = Vec::with_capacity(src.len());
                for (comp, cinfo) in vinfo.format.comp_info.iter().enumerate() {
                    if cinfo.is_none() {
                        continue;
                    }
                    let (width, height) = vbuf.get_dimensions(comp);
                    let off = vbuf.get_offset(comp);
                    let stride = vbuf.get_stride(comp);

                    for line in src[off..].chunks(stride).take(height) {
                        dbuf.extend_from_slice(&line[..width]);
                    }
                }
            },
            NABufferType::VideoPacked(ref _vbuf) => return Err(EncoderError::NotImplemented),
            NABufferType::Video16(ref _vbuf) => return Err(EncoderError::NotImplemented),
            NABufferType::Video32(ref _vbuf) => return Err(EncoderError::NotImplemented),
            NABufferType::None => {
                self.pkt = None;
                return Ok(());
            },
            _ => return Err(EncoderError::FormatError),
        };
        self.pkt = Some(NAPacket::new(self.stream.clone().unwrap(), frm.ts, true, dbuf));
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

impl NAOptionHandler for RawEncoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_encoder() -> Box<dyn NAEncoder + Send> {
    Box::new(RawEncoder::new())
}
