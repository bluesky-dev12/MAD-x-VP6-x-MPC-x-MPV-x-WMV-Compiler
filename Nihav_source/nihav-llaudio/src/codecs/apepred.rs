const HISTORY_SIZE: usize = 512;

fn val2sign(val: i32) -> i32 {
    if val > 0 {
        -1
    } else if val < 0 {
        1
    } else {
        0
    }
}

pub struct OldFilt {
    version:        u16,
    compression:    u16,
}

pub struct NewFilt {
    version:        u16,
    filters:        [NFilterContext; 3],
    lfilt:          LastFilterContext,
    rfilt:          LastFilterContext,
}

#[allow(clippy::large_enum_variant)]
pub enum FilterMode {
    Old(OldFilt),
    New(NewFilt),
    None,
}

impl FilterMode {
    pub fn new(version: u16, compression: u16) -> Self {
        if version < 3930 {
            FilterMode::Old(OldFilt::new(version, compression))
        } else {
            FilterMode::New(NewFilt::new(version, compression))
        }
    }
    pub fn filter_mono(&mut self, l: &mut [i32]) {
        match *self {
            FilterMode::Old(ref mut ofilt) => ofilt.filter(l),
            FilterMode::New(ref mut nfilt) => nfilt.filter_mono(l),
            FilterMode::None => unreachable!(),
        };
    }
    pub fn filter_stereo(&mut self, l: &mut [i32], r: &mut [i32]) {
        match *self {
            FilterMode::Old(ref mut ofilt) => {
                ofilt.filter(l);
                ofilt.filter(r);
                for (l, r) in l.iter_mut().zip(r.iter_mut()) {
                    let new_l = *l - *r / 2;
                    let new_r = *r + new_l;
                    *l = new_l;
                    *r = new_r;
                }
            },
            FilterMode::New(ref mut nfilt) => {
                nfilt.filter_stereo(l, r);
                for (l, r) in l.iter_mut().zip(r.iter_mut()) {
                    let new_l = *r - *l / 2;
                    let new_r = *l + new_l;
                    *l = new_l;
                    *r = new_r;
                }
            },
            FilterMode::None => unreachable!(),
        };
    }
}

const NEW_FILTER_PARAMS: [[(u8, u8); 3]; 5] = [
    [ (0,  0), ( 0,  0), ( 0,  0) ],
    [ (1, 11), ( 0,  0), ( 0,  0) ],
    [ (4, 11), ( 0,  0), ( 0,  0) ],
    [ (2, 10), (16, 13), ( 0,  0) ],
    [ (1, 11), (16, 13), (80, 15) ],
];

#[derive(Clone,Default)]
struct NFilterContext {
    buf:        Vec<i16>,
    coeffs:     Vec<i16>,
    order:      usize,
    bits:       u8,
    avg:        i32,
    new:        bool,
}

#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

#[cfg(target_arch = "x86_64")]
fn adapt_loop(filt: &mut [i16], coeffs: &[i16], adapt: &[i16], val: i32) -> i32 {
    let mut sum = [0i32; 4];
    let iters = filt.len() / 16;
    unsafe {
        let mut sumv = _mm_setzero_si128();
        let mut fptr = filt.as_mut_ptr();
        let mut cptr = coeffs.as_ptr();
        let mut aptr = adapt.as_ptr();
        if val < 0 {
            for _ in 0..iters {
                let r0 = _mm_loadu_si128(cptr        as *const __m128i);
                let r1 = _mm_loadu_si128(cptr.add(8) as *const __m128i);
                let c0 = _mm_load_si128(fptr        as *const __m128i);
                let c1 = _mm_load_si128(fptr.add(8) as *const __m128i);
                sumv = _mm_add_epi32(sumv, _mm_madd_epi16(r0, c0));
                sumv = _mm_add_epi32(sumv, _mm_madd_epi16(r1, c1));
                let a0 = _mm_loadu_si128(aptr        as *const __m128i);
                let a1 = _mm_loadu_si128(aptr.add(8) as *const __m128i);
                let c0 = _mm_add_epi16(c0, a0);
                let c1 = _mm_add_epi16(c1, a1);
                _mm_store_si128(fptr        as *mut __m128i, c0);
                _mm_store_si128(fptr.add(8) as *mut __m128i, c1);
                fptr = fptr.add(16);
                cptr = cptr.add(16);
                aptr = aptr.add(16);
            }
        } else if val > 0 {
            for _ in 0..iters {
                let r0 = _mm_loadu_si128(cptr        as *const __m128i);
                let r1 = _mm_loadu_si128(cptr.add(8) as *const __m128i);
                let c0 = _mm_load_si128(fptr        as *const __m128i);
                let c1 = _mm_load_si128(fptr.add(8) as *const __m128i);
                sumv = _mm_add_epi32(sumv, _mm_madd_epi16(r0, c0));
                sumv = _mm_add_epi32(sumv, _mm_madd_epi16(r1, c1));
                let a0 = _mm_loadu_si128(aptr        as *const __m128i);
                let a1 = _mm_loadu_si128(aptr.add(8) as *const __m128i);
                let c0 = _mm_sub_epi16(c0, a0);
                let c1 = _mm_sub_epi16(c1, a1);
                _mm_store_si128(fptr        as *mut __m128i, c0);
                _mm_store_si128(fptr.add(8) as *mut __m128i, c1);
                fptr = fptr.add(16);
                cptr = cptr.add(16);
                aptr = aptr.add(16);
            }
        } else {
            for _ in 0..iters {
                let r0 = _mm_loadu_si128(cptr        as *const __m128i);
                let r1 = _mm_loadu_si128(cptr.add(8) as *const __m128i);
                let c0 = _mm_load_si128(fptr        as *const __m128i);
                let c1 = _mm_load_si128(fptr.add(8) as *const __m128i);
                sumv = _mm_add_epi32(sumv, _mm_madd_epi16(r0, c0));
                sumv = _mm_add_epi32(sumv, _mm_madd_epi16(r1, c1));
                fptr = fptr.add(16);
                cptr = cptr.add(16);
                aptr = aptr.add(16);
            }
        }
        _mm_storeu_si128(sum.as_mut_ptr() as *mut __m128i, sumv);
    }
    sum[0] + sum[1] + sum[2] + sum[3]
}

#[cfg(not(target_arch = "x86_64"))]
fn adapt_loop(filt: &mut [i16], coeffs: &[i16], adapt: &[i16], val: i32) -> i32 {
    let mut sum = 0i32;
    for (coef, (res, adapt)) in filt.iter_mut().zip(coeffs.iter().zip(adapt.iter())) {
        sum += i32::from(*coef) * i32::from(*res);
        if val < 0 {
            *coef += *adapt;
        } else if val > 0 {
            *coef -= *adapt;
        }
    }
    sum
}

impl NFilterContext {
    fn new(ord16: u8, bits: u8, new: bool) -> Self {
        let order = ord16 as usize * 16;
        Self {
            buf:        if order > 0 { vec![0; order * 2 + HISTORY_SIZE] } else { Vec::new() },
            coeffs:     vec![0; order],
            order,
            bits,
            avg:        0,
            new,
        }
    }
    fn reset(&mut self) {
        for el in self.buf[..self.order * 2].iter_mut() { *el = 0; }
        for el in self.coeffs.iter_mut() { *el = 0; }
        self.avg = 0;
    }
    fn apply(&mut self, dst: &mut [i32]) {
        if self.order == 0 { return; }
        let mut adapt_pos = self.order;
        let mut delay_pos = self.order * 2;
        for el in dst.iter_mut() {
            let sum = adapt_loop(&mut self.coeffs,
                                 &self.buf[delay_pos - self.order..],
                                 &self.buf[adapt_pos - self.order..], *el);
            let pred = (sum + (1 << (self.bits - 1))) >> self.bits;
            let val = *el + pred;
            *el = val;
            self.buf[delay_pos] = val.min(32767).max(-32768) as i16;
            if self.new {
                let aval = val.abs();
                let sign = val2sign(val) as i16;
                self.buf[adapt_pos] = if aval == 0 {
                        0
                    } else if aval <= self.avg * 4 / 3 {
                        sign * 8
                    } else if aval <= self.avg * 3 {
                        sign * 16
                    } else {
                        sign * 32
                    };
                self.avg += (aval - self.avg) / 16;
                self.buf[adapt_pos - 1] >>= 1;
                self.buf[adapt_pos - 2] >>= 1;
                self.buf[adapt_pos - 8] >>= 1;
            } else {
                self.buf[adapt_pos] = 4 * (val2sign(val) as i16);
                self.buf[adapt_pos - 4] >>= 1;
                self.buf[adapt_pos - 8] >>= 1;
            }
            delay_pos += 1;
            adapt_pos += 1;
            if delay_pos == HISTORY_SIZE + self.order * 2 {
                delay_pos = self.order * 2;
                adapt_pos = self.order;
                for i in 0..self.order * 2 {
                    self.buf[i] = self.buf[HISTORY_SIZE + i];
                }
            }
        }
    }
}

#[derive(Clone,Copy,Default)]
struct LastFilterContext {
    last_a:     i32,
    filter_a:   i32,
    filter_b:   i32,
    coeffs_a:   [i32; 4],
    coeffs_b:   [i32; 5],
    delay_a:    [i32; 4],
    adapt_a:    [i32; 4],
    delay_b:    [i32; 5],
    adapt_b:    [i32; 5],
}

impl LastFilterContext {
    fn init(&mut self) {
        const COEFFS_A_NEW: [i32; 4] = [360, 317, -109, 98];

        self.filter_a = 0;
        self.filter_b = 0;
        self.coeffs_a = COEFFS_A_NEW;
        self.coeffs_b = [0; 5];
        self.last_a   = 0;
        self.delay_a  = [0; 4];
        self.adapt_a  = [0; 4];
        self.delay_b  = [0; 5];
        self.adapt_b  = [0; 5];
    }
    fn predict_a(&mut self) -> i32 {
        for i in (0..3).rev() {
            self.delay_a[i + 1] = self.delay_a[i];
            self.adapt_a[i + 1] = self.adapt_a[i];
        }
        self.delay_a[0] = self.last_a;
        self.delay_a[1] = self.last_a - self.delay_a[1];
        self.adapt_a[0] = val2sign(self.delay_a[0]);
        self.adapt_a[1] = val2sign(self.delay_a[1]);

        self.delay_a[0] * self.coeffs_a[0] +
        self.delay_a[1] * self.coeffs_a[1] +
        self.delay_a[2] * self.coeffs_a[2] +
        self.delay_a[3] * self.coeffs_a[3]
    }
    fn predict_b(&mut self, other_a: i32) -> i32 {
        for i in (0..4).rev() {
            self.delay_b[i + 1] = self.delay_b[i];
            self.adapt_b[i + 1] = self.adapt_b[i];
        }
        self.delay_b[0] = other_a - ((self.filter_b * 31) >> 5);
        self.delay_b[1] = self.delay_b[0] - self.delay_b[1];
        self.adapt_b[0] = val2sign(self.delay_b[0]);
        self.adapt_b[1] = val2sign(self.delay_b[1]);

        self.filter_b = other_a;

        (self.delay_b[0] * self.coeffs_b[0] +
         self.delay_b[1] * self.coeffs_b[1] +
         self.delay_b[2] * self.coeffs_b[2] +
         self.delay_b[3] * self.coeffs_b[3] +
         self.delay_b[4] * self.coeffs_b[4]) >> 1
    }
    fn update_a(&mut self, pred: i32, diff: i32) -> i32 {
        self.last_a = diff + (pred >> 10);
        let sign = val2sign(diff);
        for i in 0..4 {
            self.coeffs_a[i] += self.adapt_a[i] * sign;
        }
        self.filter_a = self.last_a + ((self.filter_a * 31) >> 5);

        self.filter_a
    }
    fn update_b(&mut self, diff: i32) {
        let sign = val2sign(diff);
        for i in 0..5 {
            self.coeffs_b[i] += self.adapt_b[i] * sign;
        }
    }
    fn predict_3930(&mut self, diff: i32) -> i32 {
        for i in (0..3).rev() {
            self.delay_a[i + 1] = self.delay_a[i];
        }
        self.delay_a[0] = self.last_a;
        let d0 = self.delay_a[0];
        let d1 = self.delay_a[0] - self.delay_a[1];
        let d2 = self.delay_a[1] - self.delay_a[2];
        let d3 = self.delay_a[2] - self.delay_a[3];

        let pred = (self.coeffs_a[0] * d0 +
                    self.coeffs_a[1] * d1 +
                    self.coeffs_a[2] * d2 +
                    self.coeffs_a[3] * d3) >> 9;
        self.last_a = diff + pred;
        self.filter_a = self.last_a + ((self.filter_a * 31) >> 5);

        let sign = val2sign(diff);
        self.coeffs_a[0] += if d0 < 0 { sign } else { -sign };
        self.coeffs_a[1] += if d1 < 0 { sign } else { -sign };
        self.coeffs_a[2] += if d2 < 0 { sign } else { -sign };
        self.coeffs_a[3] += if d3 < 0 { sign } else { -sign };

        self.filter_a
    }
}

impl NewFilt {
    fn new(version: u16, compression: u16) -> Self {
        let cidx = (compression / 1000) as usize - 1;
        let mut obj = Self {
            version,
            filters:        [NFilterContext::default(), NFilterContext::default(), NFilterContext::default()],
            lfilt:          LastFilterContext::default(),
            rfilt:          LastFilterContext::default(),
        };
        obj.version = version;
        let new = version >= 3980;
        for i in 0..3 {
            let (ord16, bits) = NEW_FILTER_PARAMS[cidx][i];
            obj.filters[i] = NFilterContext::new(ord16, bits, new);
        }
        obj
    }
    fn filter_mono(&mut self, dst: &mut [i32]) {
        for filt in self.filters.iter_mut() {
            filt.reset();
            filt.apply(dst);
        }
        self.lfilt.init();
        if self.version >= 3950 {
            for el in dst.iter_mut() {
                let pred = self.lfilt.predict_a();
                *el = self.lfilt.update_a(pred, *el);
            }
        } else {
            for el in dst.iter_mut() {
                *el = self.lfilt.predict_3930(*el);
            }
        }
    }
    fn filter_stereo(&mut self, l: &mut [i32], r: &mut [i32]) {
        for filt in self.filters.iter_mut() {
            filt.reset();
            filt.apply(l);
            filt.reset();
            filt.apply(r);
        }
        self.lfilt.init();
        self.rfilt.init();
        if self.version >= 3950 {
            for (l, r) in l.iter_mut().zip(r.iter_mut()) {
                let mut pred = self.lfilt.predict_a();
                pred += self.lfilt.predict_b(self.rfilt.filter_a);
                let new_l = self.lfilt.update_a(pred, *l);
                self.lfilt.update_b(*l);
                *l = new_l;

                let mut pred = self.rfilt.predict_a();
                pred += self.rfilt.predict_b(self.lfilt.filter_a);
                let new_r = self.rfilt.update_a(pred, *r);
                self.rfilt.update_b(*r);
                *r = new_r;
            }
        } else {
            for (l, r) in l.iter_mut().zip(r.iter_mut()) {
                let new_l = self.lfilt.predict_3930(*r);
                let new_r = self.rfilt.predict_3930(*l);
                *l = new_l;
                *r = new_r;
            }
        }
    }
}

impl OldFilt {
    fn new(version: u16, compression: u16) -> Self {
        Self {
            version, compression
        }
    }
    fn filter(&mut self, dst: &mut [i32]) {
        match self.compression {
            1000 => {
                Self::filter_fast(dst);
            },
            2000 => {
                Self::filter_normal(dst, 4, 10);
            },
            3000 => {
                Self::filter_high(dst, 16, 9);
                Self::filter_normal(dst, 16, 10);
            },
            4000 => {
                if self.version < 3830 {
                    Self::filter_high(dst, 128, 11);
                    Self::filter_normal(dst, 128, 10);
                } else {
                    Self::filter_extra_high(dst);
                    Self::filter_high(dst, 256, 12);
                    Self::filter_normal(dst, 256, 11);
                }
            },
            _ => unreachable!(),
        };
    }
    fn filter_fast(dst: &mut [i32]) {
        const COEFF_A_FAST: i32 = 375;

        if dst.len() <= 3 {
            return;
        }
        let mut delay = [dst[1], dst[0]];
        let mut last = dst[2];
        let mut filter = dst[2];
        let mut weight = COEFF_A_FAST;
        for el in dst[3..].iter_mut() {
            delay[1] = delay[0];
            delay[0] = last;
            let pred = delay[0] * 2 - delay[1];
            last = *el + ((pred * weight) >> 9);
            if (*el ^ pred) > 0 {
                weight += 1;
            } else {
                weight -= 1;
            }
            filter += last;
            *el = filter;
        }
    }
    fn filter_normal(dst: &mut [i32], start: usize, shift: u8) {
        const COEFFS_A_NORMAL: [i32; 3] = [64, 115, 64];
        const COEFFS_B_NORMAL: [i32; 2] = [740, 0];

        let mut last = 0;
        let mut coeffs_a = COEFFS_A_NORMAL;
        let mut coeffs_b = COEFFS_B_NORMAL;
        let mut filter_a = 0;
        let mut filter_b = 0;
        let mut delay_a = [0; 3];
        let mut delay_b = [0; 2];

        for (i, el) in dst.iter_mut().enumerate() {
            delay_a[2] = delay_a[1]; delay_a[1] = delay_a[0]; delay_a[0] = last;
            delay_b[1] = delay_b[0]; delay_b[0] = filter_b;
            if i < start {
                let val = *el + filter_a;
                last     = *el;
                filter_b = *el;
                filter_a = val;
                *el = val;
                continue;
            }
            let a0 = delay_a[0] + (delay_a[2] - delay_a[1]) * 8;
            let a1 = (delay_a[0] - delay_a[1]) * 2;
            let a2 = delay_a[0];
            let b0 = delay_b[0] * 2 - delay_b[1];
            let b1 = delay_b[0];

            let pred_a = a0 * coeffs_a[0] + a1 * coeffs_a[1] + a2 * coeffs_a[2];
            let pred_b = b0 * coeffs_b[0] - b1 * coeffs_b[1];

            let sign = val2sign(*el);
            coeffs_a[0] += (((a0 >> 30) & 2) - 1) * sign;
            coeffs_a[1] += (((a1 >> 28) & 8) - 4) * sign;
            coeffs_a[2] += (((a2 >> 28) & 8) - 4) * sign;
            last = *el + (pred_a >> 11);

            let sign = val2sign(last);
            coeffs_b[0] += (((b0 >> 29) & 4) - 2) * sign;
            coeffs_b[1] -= (((b1 >> 30) & 2) - 1) * sign;

            filter_b = last + (pred_b >> shift);
            filter_a = filter_b + ((filter_a * 31) >> 5);

            *el = filter_a;
        }
    }
    fn filter_high(dst: &mut [i32], order: usize, shift: u8) {
        let mut coeffs = [0i32; 256];
        let mut delay  = [0i32; 256];
        if dst.len() <= order {
            return;
        }
        delay[..order].copy_from_slice(&dst[..order]);
        for el in dst[order..].iter_mut() {
            let sign = val2sign(*el);
            let mut sum = 0;
            for i in 0..order {
                sum += delay[i] * coeffs[i];
                coeffs[i] -= (((delay[i] >> 30) & 2) - 1) * sign;
            }
            *el -= sum >> shift;
            for i in 0..order-1 {
                delay[i] = delay[i + 1];
            }
            delay[order - 1] = *el;
        }
    }
    fn filter_extra_high(dst: &mut [i32]) {
        let mut coeffs = [0i32; 8];
        let mut delay  = [0i32; 8];
        for el in dst[256..].iter_mut() {
            let sign = val2sign(*el);
            let mut sum = 0;
            for i in 0..8 {
                sum += delay[i] * coeffs[i];
                coeffs[i] -= (((delay[i] >> 30) & 2) - 1) * sign;
            }
            for i in (0..7).rev() {
                delay[i + 1] = delay[i];
            }
            delay[0] = *el;
            *el -= sum >> 9;
        }
    }
}
