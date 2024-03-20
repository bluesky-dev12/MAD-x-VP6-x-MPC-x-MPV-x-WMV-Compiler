use crate::formats::*;
use super::*;
use super::kernel::Kernel;

fn convert_depth(val: u32, indepth: u8, outdepth: u8) -> u32 {
    if indepth >= outdepth {
        val >> (indepth - outdepth)
    } else {
        let mut val2 = val << (outdepth - indepth);
        let mut shift = outdepth - indepth;
        while shift >= indepth {
            shift -= indepth;
            val2 |= val << shift;
        }
        if shift > 0 {
            val2 |= val >> (indepth - shift);
        }
        val2
    }
}

#[derive(Default)]
struct PackKernel {
    shifts: [u8;  MAX_CHROMATONS],
    depths: [u8;  MAX_CHROMATONS],
    ncomps: usize,
    osize:  [u8;  MAX_CHROMATONS],
    ooff:   [usize; MAX_CHROMATONS],
}

impl PackKernel {
    fn new() -> Self { Self::default() }
}

impl Kernel for PackKernel {
    fn init(&mut self, in_fmt: &ScaleInfo, dest_fmt: &ScaleInfo, _options: &[(String, String)]) -> ScaleResult<NABufferType> {
        self.ncomps = in_fmt.fmt.components.min(dest_fmt.fmt.components) as usize;
        for i in 0..self.ncomps {
            let ichr = in_fmt.fmt.comp_info[i].unwrap();
            let ochr = dest_fmt.fmt.comp_info[i].unwrap();
            self.shifts[i] = ochr.shift;
            self.osize[i] = ochr.depth;
            self.depths[i] = ichr.depth;
            self.ooff[i] = ochr.comp_offs as usize;
        }
        let res = alloc_video_buffer(NAVideoInfo::new(in_fmt.width, in_fmt.height, false, dest_fmt.fmt), 3);
        if res.is_err() { return Err(ScaleError::AllocError); }
        Ok(res.unwrap())
    }
    fn process(&mut self, pic_in: &NABufferType, pic_out: &mut NABufferType) {
        if let Some(ref buf) = pic_in.get_vbuf() {
            if let Some(ref mut dbuf) = pic_out.get_vbuf() {
                let dstride = dbuf.get_stride(0);
                for comp in 0..self.ncomps {
                    let ioff = buf.get_offset(comp);
                    let istride = buf.get_stride(comp);
                    let step = dbuf.get_info().get_format().get_chromaton(comp).unwrap().get_step() as usize;
                    let (w, h) = dbuf.get_dimensions(comp);
                    let sdata = buf.get_data();
                    let sdata = &sdata[ioff..];
                    let ddata = dbuf.get_data_mut().unwrap();
                    for (src, dst) in sdata.chunks(istride).zip(ddata.chunks_mut(dstride)).take(h) {
                        for x in 0..w {
                            dst[x * step + self.ooff[comp]] = convert_depth(u32::from(src[x]), self.depths[comp], self.osize[comp]) as u8;
                        }
                    }
                }
            } else if let Some(ref mut dbuf) = pic_out.get_vbuf16() {
                let (w, h) = dbuf.get_dimensions(0);
                let dstride = dbuf.get_stride(0);
                let ddata = dbuf.get_data_mut().unwrap();
                let src = buf.get_data();
                let mut ioff: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
                let mut istride: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
                for comp in 0..self.ncomps {
                    ioff[comp] = buf.get_offset(comp);
                    istride[comp] = buf.get_stride(comp);
                }
                for dst in ddata.chunks_mut(dstride).take(h) {
                    for x in 0..w {
                        let mut elem: u32 = 0;
                        for comp in 0..self.ncomps {
                            let c = u32::from(src[ioff[comp] + x]);
                            elem |= convert_depth(c, self.depths[comp], self.osize[comp]) << self.shifts[comp];
                        }
                        dst[x] = elem as u16;
                    }
                    for comp in 0..self.ncomps {
                        ioff[comp] += istride[comp];
                    }
                }
            } else {
unimplemented!();
            }
        } else if let Some(ref _buf) = pic_in.get_vbuf16() {
unimplemented!();
        } else if let Some(ref _buf) = pic_in.get_vbuf32() {
unimplemented!();
        } else {
            unreachable!();
        }
    }
}

pub fn create_pack() -> Box<dyn Kernel> {
    Box::new(PackKernel::new())
}

#[derive(Default)]
struct UnpackKernel {
    shifts: [u8;  MAX_CHROMATONS],
    masks:  [u32; MAX_CHROMATONS],
    depths: [u8;  MAX_CHROMATONS],
    ncomps: usize,
    osize:  [u8;  MAX_CHROMATONS],
}

impl UnpackKernel {
    fn new() -> Self { Self::default() }
}

impl Kernel for UnpackKernel {
    fn init(&mut self, in_fmt: &ScaleInfo, dest_fmt: &ScaleInfo, options: &[(String, String)]) -> ScaleResult<NABufferType> {
        let mut debug = false;
        for (name, value) in options.iter() {
            match (name.as_str(), value.as_str()) {
                ("debug", "")     => { debug = true; },
                ("debug", "true") => { debug = true; },
                _ => {},
            }
        }

        self.ncomps = in_fmt.fmt.components.min(dest_fmt.fmt.components) as usize;
        let mut chr: Vec<Option<NAPixelChromaton>> = Vec::with_capacity(MAX_CHROMATONS);
        for i in 0..self.ncomps {
            let ichr = in_fmt.fmt.comp_info[i].unwrap();
            let ochr = dest_fmt.fmt.comp_info[i].unwrap();
            self.shifts[i] = ichr.shift;
            self.masks[i] = (1 << ichr.depth) - 1;
            if ochr.depth > ichr.depth {
                self.osize[i] = ochr.depth;
            } else {
                self.osize[i] = (ichr.depth + 7) & !7;
            }
            self.depths[i] = ichr.depth;
            let mut dchr = ichr;
            dchr.packed     = false;
            dchr.depth      = self.osize[i];
            dchr.shift      = 0;
            dchr.comp_offs  = 0;
            dchr.next_elem  = 0;
            chr.push(Some(dchr));
        }
        let mut df = in_fmt.fmt;
        df.comp_info[..self.ncomps].clone_from_slice(&chr[..self.ncomps]);
        df.components = self.ncomps as u8;
        df.palette = false;
        if debug {
            println!(" [intermediate format {}]", df);
        }
        let res = alloc_video_buffer(NAVideoInfo::new(in_fmt.width, in_fmt.height, false, df), 3);
        if res.is_err() { return Err(ScaleError::AllocError); }
        Ok(res.unwrap())
    }
    fn process(&mut self, pic_in: &NABufferType, pic_out: &mut NABufferType) {
        if let Some(ref buf) = pic_in.get_vbuf() {
            let step = buf.get_info().get_format().elem_size as usize;
            let mut soff: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
            for i in 0..self.ncomps {
                soff[i] = buf.get_info().get_format().get_chromaton(i).unwrap().get_offset() as usize;
            }
            let (w, h) = buf.get_dimensions(0);
            let ioff = buf.get_offset(0);
            let istride = buf.get_stride(0);
            let sdata1 = buf.get_data();
            let sdata = &sdata1[ioff..];
            let ychr = buf.get_info().get_format().get_chromaton(0).unwrap();
            if let Some(ref mut dbuf) = pic_out.get_vbuf() {
                let mut ostride: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
                let mut offs: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
                for i in 0..self.ncomps {
                    ostride[i] = dbuf.get_stride(i);
                    offs[i]    = dbuf.get_offset(i);
                }
                let dst = dbuf.get_data_mut().unwrap();
                if ychr.next_elem == 0 || usize::from(ychr.next_elem) == step {
                    for src in sdata.chunks(istride).take(h) {
                        for x in 0..w {
                            for i in 0..self.ncomps {
                                dst[offs[i] + x] = src[x * step + soff[i]];
                            }
                        }
                        for i in 0..self.ncomps { offs[i] += ostride[i]; }
                    }
                } else {
                    let mut steps: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
                    for i in 0..self.ncomps {
                        steps[i] = buf.get_info().get_format().get_chromaton(i).unwrap().next_elem as usize;
                    }

                    for src in sdata.chunks(istride).take(h) {
                        let mut x = offs;
                        for piece in src.chunks(step) {
                            for i in 0..self.ncomps {
                                let mut co = soff[i];
                                while co < step {
                                    dst[x[i]] = piece[co];
                                    x[i] += 1;
                                    co += steps[i];
                                }
                            }
                        }
                        for i in 0..self.ncomps { offs[i] += ostride[i]; }
                    }
                }
            } else {
unimplemented!();
            }
        } else if let Some(ref buf) = pic_in.get_vbuf16() {
            let (w, h) = buf.get_dimensions(0);
            let ioff = buf.get_offset(0);
            let istride = buf.get_stride(0);
            let sdata1 = buf.get_data();
            let sdata = &sdata1[ioff..];
            if let Some(ref mut dbuf) = pic_out.get_vbuf() {
                let mut ostride: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
                let mut offs: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
                for i in 0..self.ncomps {
                    ostride[i] = dbuf.get_stride(i);
                    offs[i]    = dbuf.get_offset(i);
                }
                let dst = dbuf.get_data_mut().unwrap();
                for src in sdata.chunks(istride).take(h) {
                    for x in 0..w {
                        let elem = u32::from(src[x]);
                        for i in 0..self.ncomps {
                            dst[offs[i] + x] = convert_depth((elem >> self.shifts[i]) & self.masks[i], self.depths[i], self.osize[i]) as u8;
                        }
                    }
                    for i in 0..self.ncomps { offs[i] += ostride[i]; }
                }
            } else {
unimplemented!();
            }
        } else if let Some(ref _buf) = pic_in.get_vbuf32() {
unimplemented!();
        } else {
            unreachable!();
        }
    }
}

pub fn create_unpack() -> Box<dyn Kernel> {
    Box::new(UnpackKernel::new())
}

#[derive(Default)]
struct DepalKernel {
    depths:     [u8; MAX_CHROMATONS],
    coffs:      [usize; MAX_CHROMATONS],
    ncomps:     usize,
    palstep:    usize,
}

impl DepalKernel {
    fn new() -> Self { Self::default() }
}

impl Kernel for DepalKernel {
    fn init(&mut self, in_fmt: &ScaleInfo, _dest_fmt: &ScaleInfo, options: &[(String, String)]) -> ScaleResult<NABufferType> {
        let mut debug = false;
        for (name, value) in options.iter() {
            match (name.as_str(), value.as_str()) {
                ("debug", "")     => { debug = true; },
                ("debug", "true") => { debug = true; },
                _ => {},
            }
        }

//todo select output more fitting for dest_fmt if possible
        self.ncomps = in_fmt.fmt.components as usize;
        let mut chr: Vec<Option<NAPixelChromaton>> = Vec::with_capacity(MAX_CHROMATONS);
        self.palstep = in_fmt.fmt.elem_size as usize;
        for i in 0..self.ncomps {
            let ichr = in_fmt.fmt.comp_info[i].unwrap();
            self.coffs[i]  = ichr.comp_offs as usize;
            self.depths[i] = ichr.depth;
            let mut dchr = ichr;
            dchr.packed     = false;
            dchr.depth      = 8;
            dchr.shift      = 0;
            dchr.comp_offs  = i as u8;
            dchr.next_elem  = 0;
            chr.push(Some(dchr));
        }
        let mut df = in_fmt.fmt;
        df.palette = false;
        df.comp_info[..self.ncomps].clone_from_slice(&chr[..self.ncomps]);
        if debug {
            println!(" [intermediate format {}]", df);
        }
        let res = alloc_video_buffer(NAVideoInfo::new(in_fmt.width, in_fmt.height, false, df), 3);
        if res.is_err() { return Err(ScaleError::AllocError); }
        Ok(res.unwrap())
    }
    fn process(&mut self, pic_in: &NABufferType, pic_out: &mut NABufferType) {
        if let (Some(ref sbuf), Some(ref mut dbuf)) = (pic_in.get_vbuf(), pic_out.get_vbuf()) {
            let ioff = sbuf.get_offset(0);
            let paloff = sbuf.get_offset(1);
            let (w, h) = sbuf.get_dimensions(0);
            let istride = sbuf.get_stride(0);
            let sdata1 = sbuf.get_data();
            let sdata = &sdata1[ioff..];
            let pal = &sdata1[paloff..];

            let mut ostride: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
            let mut offs: [usize; MAX_CHROMATONS] = [0; MAX_CHROMATONS];
            for i in 0..self.ncomps {
                ostride[i] = dbuf.get_stride(i);
                offs[i]    = dbuf.get_offset(i);
            }
            let dst = dbuf.get_data_mut().unwrap();
            for src in sdata.chunks(istride).take(h) {
                for x in 0..w {
                    let palidx = src[x] as usize;
                    for i in 0..self.ncomps {
                        let elem = u32::from(pal[palidx * self.palstep + self.coffs[i]]);
                        dst[offs[i] + x] = convert_depth(elem, self.depths[i], 8) as u8;
                    }
                }
                for i in 0..self.ncomps { offs[i] += ostride[i]; }
            }
        } else {
            unreachable!();
        }
    }
}

pub fn create_depal() -> Box<dyn Kernel> {
    Box::new(DepalKernel::new())
}
