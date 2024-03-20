use nihav_core::formats::*;
use nihav_core::frame::*;
use nihav_core::codecs::*;
use nihav_core::io::bitreader::*;
use nihav_codec_support::dsp::fft::*;
use std::str::FromStr;
use std::f32::consts;

const BLOCK_LEN: usize = 256;
const NBLOCKS:   usize = 6;
const MAX_CHANNELS: usize = 5;
const MAX_CPLBANDS: usize = 18;
const MAX_BANDS:    usize = 50;

const MAGIC_BYTE0: u8 = 0x0B;
const MAGIC_BYTE1: u8 = 0x77;

const STRATEGY_REUSE: u8 = 0;

const LFE_CHANNEL:  usize = MAX_CHANNELS;
const CPL_CHANNEL:  usize = MAX_CHANNELS + 1;

struct IMDCTContext {
    xsincos:    [FFTComplex; BLOCK_LEN/2],
    fft:        FFT,
    size:       usize,
}

struct IMDCTWorkspace {
    z:      [FFTComplex; BLOCK_LEN / 2],
    y:      [FFTComplex; BLOCK_LEN / 2],
    out:    [f32; BLOCK_LEN * 2],
}

impl IMDCTContext {
    fn new(bits: usize) -> Self {
        let size = 1 << bits;
        let size4 = 1 << (bits - 2);
        let mut xsincos: [FFTComplex; 512/4] = [FFTC_ZERO; 512/4];
        for k in 0..size4 {
            let factor = 2.0 * consts::PI * ((8 * k + 1) as f32) / ((8 * size) as f32);
            xsincos[k].re = -factor.cos();
            xsincos[k].im = -factor.sin();
        }
        let fft = FFTBuilder::new_fft(size/4, false);
        IMDCTContext { xsincos, size, fft }
    }
    #[allow(non_snake_case)]
    fn do_imdct(&mut self, coeffs: &[i32; BLOCK_LEN], tmp: &mut IMDCTWorkspace) {
        do_imdct_core(&mut self.fft, &self.xsincos, self.size, false, coeffs, &mut tmp.z, &mut tmp.y);
        let w = &TS102366_WINDOW;
        let N2 = self.size / 2;
        let N4 = self.size / 4;
        let N8 = self.size / 8;
        for n in 0..N8 {
            tmp.out[         2 * n]     = -tmp.y[N8 + n]    .im * w[     2 * n];
            tmp.out[         2 * n + 1] =  tmp.y[N8 - n - 1].re * w[     2 * n + 1];
            tmp.out[    N4 + 2 * n]     = -tmp.y[     n]    .re * w[N4 + 2 * n];
            tmp.out[    N4 + 2 * n + 1] =  tmp.y[N4 - n - 1].im * w[N4 + 2 * n + 1];
            tmp.out[    N2 + 2 * n]     = -tmp.y[N8 + n]    .re * w[N2 - 2 * n - 1];
            tmp.out[    N2 + 2 * n + 1] =  tmp.y[N8 - n - 1].im * w[N2 - 2 * n - 2];
            tmp.out[3 * N4 + 2 * n]     =  tmp.y[     n]    .im * w[N4 - 2 * n - 1];
            tmp.out[3 * N4 + 2 * n + 1] = -tmp.y[N4 - n - 1].re * w[N4 - 2 * n - 2];
        }
    }
    #[allow(non_snake_case)]
    fn do_imdct_ileave(&mut self, coeffs: &[i32; BLOCK_LEN], tmp: &mut IMDCTWorkspace) {
        let mut ziter = tmp.z.chunks_mut(self.size / 4);
        let z1 = ziter.next().unwrap();
        let z2 = ziter.next().unwrap();
        let mut yiter = tmp.y.chunks_mut(self.size / 4);
        let y1 = yiter.next().unwrap();
        let y2 = yiter.next().unwrap();
        do_imdct_core(&mut self.fft, &self.xsincos, self.size, true, coeffs,       z1, y1);
        do_imdct_core(&mut self.fft, &self.xsincos, self.size, true, &coeffs[1..], z2, y2);
        let w = &TS102366_WINDOW;
        let N2 = self.size / 2;
        let N4 = self.size / 4;
        let N8 = self.size / 8;
        for n in 0..N8 {
            tmp.out[         2 * n]     = -y1[     n]    .im * w[     2 * n];
            tmp.out[         2 * n + 1] =  y1[N8 - n - 1].re * w[     2 * n + 1];
            tmp.out[    N4 + 2 * n]     = -y1[     n]    .re * w[N4 + 2 * n];
            tmp.out[    N4 + 2 * n + 1] =  y1[N8 - n - 1].im * w[N4 + 2 * n + 1];
            tmp.out[    N2 + 2 * n]     = -y2[     n]    .re * w[N2 - 2 * n - 1];
            tmp.out[    N2 + 2 * n + 1] =  y2[N8 - n - 1].im * w[N2 - 2 * n - 2];
            tmp.out[3 * N4 + 2 * n]     =  y2[     n]    .im * w[N4 - 2 * n - 1];
            tmp.out[3 * N4 + 2 * n + 1] = -y2[N8 - n - 1].re * w[N4 - 2 * n - 2];
        }
    }
}

#[allow(non_snake_case)]
fn do_imdct_core(fft: &mut FFT, xsc: &[FFTComplex; BLOCK_LEN/2], size: usize, ilace: bool, coeffs: &[i32], z: &mut [FFTComplex], y: &mut [FFTComplex]) {
    let N  = size;
    let N2 = size / 2;
    let N4 = size / 4;
    let scale = 1.0 / ((1 << 24) as f32);
    for k in 0..N4 {
        let (c0, c1) = if !ilace {
                ((coeffs[N2 - 2 * k - 1] as f32) * scale,
                 (coeffs[     2 * k]     as f32) * scale)
            } else {
                ((coeffs[N - 4 * k - 2] as f32) * scale,
                 (coeffs[    4 * k]     as f32) * scale)
            };
        let c = FFTComplex { re: c0, im: c1 };
        z[k] = c * xsc[k];
    }
    fft.do_ifft_inplace(z);
    for k in 0..N4 {
        y[k] = z[k] * xsc[k];
    }
}

struct AudioDecoder {
    info:       NACodecInfoRef,
    ablk:       AudioBlock,
    imdct512:   IMDCTContext,
    imdct256:   IMDCTContext,
    tmp:        IMDCTWorkspace,
    delay:      [[f32; BLOCK_LEN]; MAX_CHANNELS + 1],
}

impl AudioDecoder {
    fn new() -> Self {
        AudioDecoder {
            info:       NACodecInfo::new_dummy(),
            ablk:       AudioBlock::new(),
            imdct512:   IMDCTContext::new(9),
            imdct256:   IMDCTContext::new(8),
            tmp:        IMDCTWorkspace {
                            z:      [FFTC_ZERO; BLOCK_LEN / 2],
                            y:      [FFTC_ZERO; BLOCK_LEN / 2],
                            out:    [0.0; BLOCK_LEN * 2],
                        },
            delay:      [[0.0; BLOCK_LEN]; MAX_CHANNELS + 1],
        }
    }
}

const SAMPLE_RATES: [u32; 4] = [ 48000, 44100, 32000, 0 ];

const FRAME_SIZES: [[usize; 64]; 4] = [
    [
          64,   64,   80,   80,   96,   96,  112,  112,
         128,  128,  160,  160,  192,  192,  224,  224,
         256,  256,  320,  320,  384,  384,  448,  448,
         512,  512,  640,  640,  768,  768,  896,  896,
        1024, 1024, 1152, 1152, 1280, 1280,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ], [
          69,   70,   87,   88,  104,  105,  121,  122,
         139,  140,  174,  175,  208,  209,  243,  244,
         278,  279,  348,  349,  417,  418,  487,  488,
         557,  558,  696,  697,  835,  836,  975,  976,
        1114, 1115, 1253, 1254, 1393, 1394,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ], [
          96,   96,  120,  120,  144,  144,  168,  168,
         192,  192,  240,  240,  288,  288,  336,  336,
         384,  384,  480,  480,  576,  576,  672,  672,
         768,  768,  960,  960, 1152, 1152, 1344, 1344,
        1536, 1536, 1728, 1728, 1920, 1920,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ], [ 0; 64 ],
];

#[derive(Debug,Clone,Copy)]
#[allow(dead_code)]
struct Syncinfo {
    crc1:       u16,
    fscod:      u8,
    frmsizecod: u8,
    samplerate: u32,
    frame_size: usize,
}

impl Syncinfo {
    fn read(br: &mut BitReader) -> DecoderResult<Self> {
        let syncword        = br.read(16)?;
        validate!(syncword == (u32::from(MAGIC_BYTE0) * 256) + u32::from(MAGIC_BYTE1));
        let crc1            = br.read(16)? as u16;
        let fscod           = br.read(2)? as usize;
        let frmsizecod      = br.read(6)? as usize;
        Ok(Syncinfo { crc1, fscod: fscod as u8, frmsizecod: frmsizecod as u8,
                      samplerate: SAMPLE_RATES[fscod], frame_size: FRAME_SIZES[fscod][frmsizecod] * 2 })
    }
    fn is_valid(&self) -> bool {
        (self.samplerate != 0) && (self.frame_size != 0)
    }
}

trait ReadOptional {
    fn read_optional8(&mut self) -> BitReaderResult<Option<u8>>;
    fn read_optional16(&mut self, bits: u8) -> BitReaderResult<Option<u16>>;
}

impl<'a> ReadOptional for BitReader<'a> {
    fn read_optional8(&mut self) -> BitReaderResult<Option<u8>> {
        if self.read_bool()? {
            Ok(Some(self.read(8)? as u8))
        } else {
            Ok(None)
        }
    }
    fn read_optional16(&mut self, bits: u8) -> BitReaderResult<Option<u16>> {
        if self.read_bool()? {
            Ok(Some(self.read(bits)? as u16))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug,Clone,Copy,PartialEq)]
enum ACMode {
    DualMono,
    Mono,
    Stereo,
    Mode3_0,
    Mode2_1,
    Mode3_1,
    Mode2_2,
    Mode3_2,
}

impl ACMode {
    fn get_num_channels(self) -> usize {
        match self {
            ACMode::DualMono    => 2,
            ACMode::Mono        => 1,
            ACMode::Stereo      => 2,
            ACMode::Mode3_0     => 3,
            ACMode::Mode2_1     => 3,
            ACMode::Mode3_1     => 4,
            ACMode::Mode2_2     => 4,
            ACMode::Mode3_2     => 5,
        }
    }
    fn get_channel_map_str(self) -> &'static str {
        match self {
            ACMode::DualMono    => "C,C",
            ACMode::Mono        => "C",
            ACMode::Stereo      => "L,R",
            ACMode::Mode3_0     => "L,C,R",
            ACMode::Mode2_1     => "L,R,Cs",
            ACMode::Mode3_1     => "L,C,R,Cs",
            ACMode::Mode2_2     => "L,R,Ls,Rs",
            ACMode::Mode3_2     => "L,C,R,Ls,Rs",
        }
    }
    fn get_channel_map(self, has_lfe: bool) -> NAChannelMap {
        let mut chmap = NAChannelMap::from_str(self.get_channel_map_str()).unwrap();
        if has_lfe {
            chmap.add_channel(NAChannelType::LFE);
        }
        chmap
    }
    fn is_3_x(self) -> bool {
        matches!(self,
            ACMode::Mode3_0 |
            ACMode::Mode3_1 |
            ACMode::Mode3_2)
    }
    fn is_surround(self) -> bool {
        matches!(self,
            ACMode::Mode2_1 |
            ACMode::Mode3_1 |
            ACMode::Mode2_2 |
            ACMode::Mode3_2)
    }
}

const AC_MODES: [ACMode; 8] = [
    ACMode::DualMono, ACMode::Mono, ACMode::Stereo,
    ACMode::Mode3_0, ACMode::Mode2_1,
    ACMode::Mode3_1, ACMode::Mode2_2,
    ACMode::Mode3_2
];

#[derive(Debug,Clone,Copy)]
#[allow(dead_code)]
struct Mixinfo {
    dialnorm:   u8,
    compr:      Option<u8>,
    langcod:    Option<u8>,
    mixlevel:   Option<u8>,
    roomtyp:    Option<u8>,
}

impl Mixinfo {
    fn read(br: &mut BitReader) -> DecoderResult<Self> {
        let dialnorm                = br.read(5)? as u8;
        let compr                   = br.read_optional8()?;
        let langcod                 = br.read_optional8()?;
        let (mixlevel, roomtyp) = if br.read_bool()? {
                let mlev            = br.read(5)? as u8;
                let rt              = br.read(2)? as u8;
                validate!(rt < 3);
                (Some(mlev), Some(rt))
            } else {
                (None, None)
            };
        Ok(Mixinfo { dialnorm, compr, langcod, mixlevel, roomtyp })
    }
}

#[derive(Debug,Clone,Copy)]
#[allow(dead_code)]
struct BSI {
    bsid:       u8,
    shift:      u8,
    bsmod:      u8,
    acmod:      ACMode,
    cmixlev:    Option<u8>,
    surmixlev:  Option<u8>,
    dsurmod:    Option<u8>,
    lfeon:      bool,
    mixinfo:    Mixinfo,
    mixinfo2:   Option<Mixinfo>,
    copysmth:   bool,
    origbs:     bool,
    timecod1:   Option<u16>,
    timecod2:   Option<u16>,
    has_addb:   bool,
}

impl BSI {
    fn read(br: &mut BitReader) -> DecoderResult<BSI> {
        let bsid                    = br.read(5)? as u8;
        validate!(bsid <= 10);
        let shift = if bsid < 9 { 0 } else { bsid - 9 + 1 };
        let bsmod                   = br.read(3)? as u8;
        let acmod_id                = br.read(3)? as usize;
        validate!(acmod_id < AC_MODES.len());
        let acmod = AC_MODES[acmod_id];
        let cmixlev = if acmod.is_3_x() {
                let cl              = br.read(2)? as u8;
                validate!(cl < 3);
                Some(cl)
            } else { None };
        let surmixlev = if acmod.is_surround() {
                let sml             = br.read(2)? as u8;
                validate!(sml < 3);
                Some(sml)
            } else { None };
        let dsurmod = if acmod == ACMode::Stereo {
                let dsm             = br.read(2)? as u8;
                validate!(dsm < 3);
                Some(dsm)
            } else { None };
        let lfeon                   = br.read_bool()?;
        let mixinfo                 = Mixinfo::read(br)?;
        let mixinfo2 = if acmod == ACMode::DualMono {
                Some(Mixinfo::read(br)?)
            } else {
                None
            };
        let copysmth                = br.read_bool()?;
        let origbs                  = br.read_bool()?;
        let timecod1                = br.read_optional16(14)?;
        let timecod2                = br.read_optional16(14)?;
        let has_addb                = br.read_bool()?;
        Ok(BSI{
            bsid, shift, bsmod, acmod, lfeon,
            cmixlev, surmixlev, dsurmod,
            mixinfo, mixinfo2,
            copysmth, origbs, timecod1, timecod2, has_addb,
            })
    }
}

#[derive(Clone, Copy)]
struct ChannelData {
    blksw:      bool,
    dithflag:   bool,

    chincpl:    bool,
    cplcoe:     bool,
    cplcoexp:   [u8; MAX_CPLBANDS],
    cplcomant:  [u8; MAX_CPLBANDS],
    mstrcplco:  u8,

    chbwcod:    u8,

    expstr:     u8,
    gainrng:    u8,
    startmant:  usize,
    endmant:    usize,
    groups:     usize,

    fsnroffst:  u8,
    fgaincod:   usize,
    snroffset:  i32,

    deltbae:    u8,
    deltnseg:   usize,
    deltoffst:  [u8; 8],
    deltlen:    [usize; 8],
    deltba:     [u8; 8],

    bap:        [u8; 256],
    exps:       [u8; 256],
    mant:       [i32; 256],

    psd:        [i16; 256],
    bndpsd:     [i32; 64],
}

const GROUPS: [usize; 4] = [ 0, 3, 6, 12 ];
const GROUP_BIAS: [usize; 4] = [ 0, 0, 3, 9 ];

fn decode_exps(br: &mut BitReader, dst: &mut [u8], mut prev: u8, grplen: usize) -> DecoderResult<()> {
    let repeat = grplen / 3;

    for out in dst.chunks_mut(grplen) {
        let val                                         = br.read(7)? as u8;
        validate!(val < 125);
        let diff: [u8; 3] = [ val / 25,  (val / 5) % 5, val % 5 ];
        for (i, exps) in out.chunks_mut(repeat).enumerate() {
            exps[0] = (prev + diff[i]).wrapping_sub(2);
            validate!(exps[0] <= 24);
            for j in 1..repeat {
                exps[j] = exps[0];
            }
            prev = exps[0];
        }
    }

    Ok(())
}

macro_rules! read_bap {
    ($br: expr, $tab:ident, $nbits:expr) => ({
            let val                                     = $br.read($nbits)? as usize;
            validate!(val < $tab.len());
            $tab[val]
        });
    ($br:expr, $tab:ident, $nbits:expr, $bufidx:expr, $bap_buf:expr, $bap_buf_fill:expr) => (
            if $bap_buf_fill[$bufidx] > 0 {
                $bap_buf_fill[$bufidx] -= 1;
                $bap_buf[$bufidx][$bap_buf_fill[$bufidx]]
            } else {
                let val                                 = $br.read($nbits)? as usize;
                validate!(val < $tab.len());
                for i in 1..$tab[0].len() {
                    $bap_buf[$bufidx][i - 1] = $tab[val][i];
                }
                $bap_buf_fill[$bufidx] = $tab[0].len() - 1;
                $tab[val][0]
            }
        );
}

impl ChannelData {
    fn new() -> Self {
        ChannelData {
            blksw:      false,
            dithflag:   false,

            chincpl:    false,
            cplcoe:     false,
            cplcoexp:   [0; MAX_CPLBANDS],
            cplcomant:  [0; MAX_CPLBANDS],
            mstrcplco:  0,

            chbwcod:    0,

            expstr:     0,
            gainrng:    0,
            startmant:  0,
            endmant:    0,
            groups:     0,

            fsnroffst:  0,
            fgaincod:   0,
            snroffset:  0,

            deltbae:    0,
            deltnseg:   0,
            deltoffst:  [0; 8],
            deltlen:    [0; 8],
            deltba:     [0; 8],

            bap:        [0; 256],
            exps:       [0; 256],
            mant:       [0; 256],

            psd:        [0; 256],
            bndpsd:     [0; 64],
        }
    }
    fn read_strategy(&mut self, br: &mut BitReader, blk_no: usize) -> DecoderResult<()> {
        self.expstr                                     = br.read(2)? as u8;
        validate!(blk_no != 0 || self.expstr != STRATEGY_REUSE);
        if self.expstr != STRATEGY_REUSE {
            if self.startmant > 0 {
                self.groups = (self.endmant - self.startmant) / GROUPS[self.expstr as usize];
            } else if self.endmant != 0{
                let idx = self.expstr as usize;
                self.groups = (self.endmant + GROUP_BIAS[idx] - 1) / GROUPS[idx];
            } else {
                self.groups = 0;
            }
        }
        Ok(())
    }
    fn read_exps(&mut self, br: &mut BitReader, is_cpl: bool, is_lfe: bool) -> DecoderResult<()> {
        if self.expstr == STRATEGY_REUSE { return Ok(()); }
        let grpsize = GROUPS[self.expstr as usize];
        self.exps = [0; 256];
        if is_cpl {
            let first                                   = (br.read(4)? as u8) << 1;
            let out = &mut self.exps[self.startmant..self.endmant];
            decode_exps(br, out, first, grpsize)?;
        } else if !is_lfe {
            self.exps[0]                                = br.read(4)? as u8;
            let first = self.exps[0];
            let out = &mut self.exps[1..self.endmant];
            decode_exps(br, out, first, grpsize)?;
            self.gainrng                                = br.read(2)? as u8;
        } else {
            self.exps[0]                                = br.read(4)? as u8;
            let first = self.exps[0];
            let out = &mut self.exps[1..7];
            decode_exps(br, out, first, grpsize)?;
        }
        Ok(())
    }
    fn read_snr(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        self.fsnroffst                                  = br.read(4)? as u8;
        self.fgaincod                                   = br.read(3)? as usize;
        Ok(())
    }
    fn read_deltbai(&mut self, br: &mut BitReader) -> DecoderResult<()> {
        if self.deltbae == 1 {
            self.deltnseg                               = (br.read(3)? as usize) + 1;
            for seg in 0..self.deltnseg {
                self.deltoffst[seg]                     = br.read(5)? as u8;
                self.deltlen[seg]                       = br.read(4)? as usize;
                self.deltba[seg]                        = br.read(3)? as u8;
            }
        }
        Ok(())
    }

    fn compute_bndpsd(&mut self) {
        let start       = self.startmant;
        let end         = self.endmant;
        let exps        = &mut self.exps;
        let psd         = &mut self.psd;
        let bndpsd      = &mut self.bndpsd;

        for bin in start..end {
            psd[bin] = 3072 - (i16::from(exps[bin]) << 7);
        }

        let mut bin = start;
        let mut band = TS102366_BIN_TO_BAND[bin] as usize;
        let mut lastbin;
        loop {
            lastbin = ((TS102366_BAND_START[band] as usize) + (TS102366_BAND_SIZE[band] as usize)).min(end);
            bndpsd[band] = i32::from(psd[bin]);
            bin += 1;
            while bin < lastbin {
                bndpsd[band] = logadd(bndpsd[band], i32::from(psd[bin]));
                bin += 1;
            }
            band += 1;
            if lastbin >= end { break; }
        }
    }
    fn compute_mask(&mut self, mask: &mut [i32; MAX_BANDS], fscod: usize, sgain: u16, fdecay: u8, sdecay: u8,
                    dbknee: u16, cplfleak: u16, cplsleak: u16, shift: u8) {
        let fgain = i32::from(TS102366_FAST_GAIN[self.fgaincod]);

        let bndstart = TS102366_BIN_TO_BAND[self.startmant] as usize;
        let bndend   = (TS102366_BIN_TO_BAND[self.endmant - 1] as usize) + 1;

        let mut excite: [i32; MAX_BANDS] = [0; MAX_BANDS];

        let begin;
        let mut fast_leak;
        let mut slow_leak;
        if bndstart == 0 {
            let lowcomp0 = calc_lowcomp(0, self.bndpsd[0], self.bndpsd[1], 0);
            excite[0] = self.bndpsd[0] - fgain - lowcomp0;
            let lowcomp1 = calc_lowcomp(lowcomp0, self.bndpsd[1], self.bndpsd[2], 1);
            excite[1] = self.bndpsd[1] - fgain - lowcomp1;
            let mut sband = 7;
            let mut lowcomp = lowcomp1;
            fast_leak = 0;
            slow_leak = 0;
            for band in 2..7 {
                let not_lfe_case = (bndend != 7) || (band != 6);
                if not_lfe_case {
                    lowcomp = calc_lowcomp(lowcomp, self.bndpsd[band], self.bndpsd[band + 1], band);
                }
                fast_leak = self.bndpsd[band] - fgain;
                slow_leak = self.bndpsd[band] - i32::from(sgain);
                excite[band] = fast_leak - lowcomp;
                if not_lfe_case && (self.bndpsd[band] <= self.bndpsd[band + 1]) {
                    sband = band + 1;
                    break;
                }
            }
            for band in sband..bndend.min(22) {
                if (bndend != 7) || (band != 6) {
                    lowcomp = calc_lowcomp(lowcomp, self.bndpsd[band], self.bndpsd[band + 1], band);
                }
                fast_leak = (fast_leak - i32::from(fdecay)).max(self.bndpsd[band] - fgain);
                slow_leak = (slow_leak - i32::from(sdecay)).max(self.bndpsd[band] - i32::from(sgain));
                excite[band] = slow_leak.max(fast_leak - lowcomp);
            }
            begin = 22;
        } else {
            begin = bndstart;
            fast_leak = i32::from(cplfleak);
            slow_leak = i32::from(cplsleak);
        }
        for band in begin..bndend {
            fast_leak = (fast_leak - i32::from(fdecay)).max(self.bndpsd[band] - fgain);
            slow_leak = (slow_leak - i32::from(sdecay)).max(self.bndpsd[band] - i32::from(sgain));
            excite[band] = fast_leak.max(slow_leak);
        }
        for band in bndstart..bndend {
            if self.bndpsd[band] < i32::from(dbknee) {
                excite[band] += (i32::from(dbknee) - self.bndpsd[band]) >> 2;
            }
            mask[band] = excite[band].max(i32::from(TS102366_HTH[fscod][band >> shift]));
        }
    }
    fn apply_delta_info(&mut self, mask: &mut [i32; MAX_BANDS]) {
        if self.deltbae == 0 || self.deltbae == 1 {
            let mut band = TS102366_BIN_TO_BAND[self.startmant] as usize;
            for seg in 0..self.deltnseg {
                band += self.deltoffst[seg] as usize;
                let delta = if self.deltba[seg] >= 4 {
                        (i32::from(self.deltba[seg]) - 3) << 7
                    } else {
                        (i32::from(self.deltba[seg]) - 4) << 7
                    };
                if band + self.deltlen[seg] > MAX_BANDS { break; }
                for _ in 0..self.deltlen[seg] {
                    mask[band] += delta;
                    band += 1;
                }
            }
        }
    }
    fn calc_snr_offset(&mut self, csnroffst: u8) {
        self.snroffset = (((i32::from(csnroffst) - 15) << 4) + i32::from(self.fsnroffst)) << 2;
    }
    fn compute_bap(&mut self, mask: &mut [i32; MAX_BANDS], floor: u16) {
        let end = self.endmant;
        let mut band = TS102366_BIN_TO_BAND[self.startmant] as usize;
        let mut bin = self.startmant;
        let mut lastbin;
        loop {
            lastbin = ((TS102366_BAND_START[band] as usize) + (TS102366_BAND_SIZE[band] as usize)).min(end);
            mask[band] = (mask[band] - self.snroffset - i32::from(floor)).max(0) & 0x1FE0;
            mask[band] += i32::from(floor);
            while bin < lastbin {
                let addr = ((i32::from(self.psd[bin]) - mask[band]) >> 5).min(63).max(0) as usize;
                self.bap[bin] = TS102366_BAPTAB[addr];
                bin += 1;
            }
            if lastbin == end { break; }
            band += 1;
        }
    }
    fn read_mant(&mut self, br: &mut BitReader, bap_buf: &mut [[i32; 2]; 3], bap_buf_fill: &mut [usize; 3]) -> DecoderResult<()> {
        self.mant = [0; BLOCK_LEN];
        for bin in self.startmant..self.endmant {
            self.mant[bin] = match self.bap[bin] {
                    0 => {
                            if self.dithflag {
                                42 // todo dither
                            } else {
                                0
                            }
                        },
                    1 => { read_bap!(br, TS102366_QUANT3_MAP,  5, 0, bap_buf, bap_buf_fill) },
                    2 => { read_bap!(br, TS102366_QUANT5_MAP,  7, 1, bap_buf, bap_buf_fill) },
                    3 => { read_bap!(br, TS102366_QUANT7_MAP,  3) },
                    4 => { read_bap!(br, TS102366_QUANT11_MAP, 7, 2, bap_buf, bap_buf_fill) },
                    5 => { read_bap!(br, TS102366_QUANT15_MAP, 4) },
                    _ => {
                            validate!(self.bap[bin] < 15);
                            let nbits = TS102366_BAP_BITS[(self.bap[bin] as usize) - 6];
                            let val                     = br.read(nbits)? as i16;
                            i32::from(val << (16 - nbits)) << 9
                        },
                };
            self.mant[bin] >>= self.exps[bin];
        }
        Ok(())
    }

    fn synth(&mut self, imdct512: &mut IMDCTContext, imdct256: &mut IMDCTContext, tmp: &mut IMDCTWorkspace, delay: &mut [f32; BLOCK_LEN], dst: &mut [f32]) {
        if !self.blksw {
            imdct512.do_imdct(&self.mant, tmp);
        } else {
            imdct256.do_imdct_ileave(&self.mant, tmp);
        }
        overlap(delay, &tmp.out, dst);
    }
}

fn logadd(acc: i32, add: i32) -> i32 {
    let c = acc - add;
    let addr = (c.abs() >> 1).min(255);
    if c >= 0 {
        acc + i32::from(TS102366_LATAB[addr as usize])
    } else {
        add + i32::from(TS102366_LATAB[addr as usize])
    }
}

fn calc_lowcomp(a: i32, b0: i32, b1: i32, band: usize) -> i32 {
    if band < 7 {
        if (b0 + 256) == b1 {
            384
        } else if b0 > b1 {
            0.max(a - 64)
        } else {
            a
        }
    } else if band < 20 {
        if (b0 + 256) == b1 {
            320
        } else if b0 > b1 {
            0.max(a - 64)
        } else {
            a
        }
    } else {
        0.max(a - 128)
    }
}

fn overlap(delay: &mut [f32; BLOCK_LEN], src: &[f32; BLOCK_LEN * 2], out: &mut [f32]) {
    {
        let dly = &delay;
        for ((d, s), o) in dly.iter().zip(src.iter()).zip(out.iter_mut()) {
            *o = (*d + *s) * 2.0;
        }
    }
    delay.copy_from_slice(&src[BLOCK_LEN..]);
}

#[derive(Clone)]
struct AudioBlock {
    dynrng:     Option<u8>,
    dynrng2:    Option<u8>,

    cplstre:    bool,
    cplinu:     bool,
    phsflginu:  bool,
    cplbegf:    usize,
    cplendf:    usize,
    ncplsubnd:  usize,
    ncplbnd:    usize,
    cplbndstrc: [bool; MAX_CPLBANDS],

    phsflg:     [bool; MAX_CPLBANDS],
    rematstr:   bool,
    rematflg:   [bool; 4],

    chdata:     [ChannelData; MAX_CHANNELS + 2],

    baie:       bool,
    fdcycod:    usize,
    sdcycod:    usize,
    sgaincod:   usize,
    dbpbcod:    usize,
    floorcod:   usize,
    snroffste:  bool,
    csnroffst:  u8,
    cplleake:   bool,
    cplfleak:   u8,
    cplsleak:   u8,

    deltbaie:   bool,

    bap_buf:        [[i32; 2]; 3],
    bap_buf_fill:   [usize; 3],
}

impl AudioBlock {
    fn new() -> Self {
        AudioBlock {
            chdata:     [ChannelData::new(); MAX_CHANNELS + 2],

            dynrng:     None,
            dynrng2:    None,

            cplstre:    false,
            cplinu:     false,
            phsflginu:  false,
            cplbegf:    0,
            cplendf:    0,
            ncplsubnd:  0,
            ncplbnd:    0,
            cplbndstrc: [false; MAX_CPLBANDS],

            phsflg:     [false; MAX_CPLBANDS],
            rematstr:   false,
            rematflg:   [false; 4],

            baie:       false,
            sdcycod:    0,
            fdcycod:    0,
            sgaincod:   0,
            dbpbcod:    0,
            floorcod:   0,
            snroffste:  false,
            csnroffst:  0,
            cplleake:   false,
            cplfleak:   0,
            cplsleak:   0,

            deltbaie:   false,

            bap_buf:        [[0; 2]; 3],
            bap_buf_fill:   [0; 3],
        }
    }
    #[allow(clippy::cognitive_complexity)]
    fn read(&mut self, br: &mut BitReader, bsi: &BSI, fscod: usize, blk_no: usize) -> DecoderResult<bool> {
        let channels = bsi.acmod.get_num_channels();
        let is_stereo = bsi.acmod == ACMode::Stereo;

        for ch in 0..channels {
            self.chdata[ch].blksw                   = br.read_bool()?;
        }
        // dynamic range information
        for ch in 0..channels {
            self.chdata[ch].dithflag                = br.read_bool()?;
        }
        self.dynrng                                 = br.read_optional8()?;
        if bsi.acmod == ACMode::DualMono {
            self.dynrng2                            = br.read_optional8()?;
        }
        // coupling strategy information
        self.cplstre                                = br.read_bool()?;
        validate!((blk_no != 0) || self.cplstre);
        if self.cplstre {
            self.cplinu                             = br.read_bool()?;
            if self.cplinu {
                for ch in 0..channels {
                    self.chdata[ch].chincpl         = br.read_bool()?;
                }
                if is_stereo {
                    self.phsflginu                  = br.read_bool()?;
                }
                self.cplbegf                        = br.read(4)? as usize;
                self.cplendf                        = (br.read(4)? as usize) + 3;
                validate!(self.cplendf >= self.cplbegf);
                self.ncplsubnd = self.cplendf - self.cplbegf;
                self.ncplbnd = self.ncplsubnd;
                self.chdata[CPL_CHANNEL].startmant = self.cplbegf * 12 + 37;
                self.chdata[CPL_CHANNEL].endmant   = self.cplendf * 12 + 37;
                for bnd in 1..self.ncplsubnd {
                    self.cplbndstrc[bnd]            = br.read_bool()?;
                    if self.cplbndstrc[bnd] {
                        self.ncplbnd -= 1;
                    }
                }
            }
        }
        // coupling coordinates
        if self.cplinu {
            for c in 0..channels {
                let ch = &mut self.chdata[c];
                if ch.chincpl {
                    ch.cplcoe                       = br.read_bool()?;
                    if ch.cplcoe {
                        ch.mstrcplco                = br.read(2)? as u8;
                        for bnd in 0..self.ncplbnd {
                            ch.cplcoexp [bnd]       = br.read(4)? as u8;
                            ch.cplcomant[bnd]       = br.read(4)? as u8;
                        }
                    }
                }
            }
            if is_stereo && self.phsflginu && (self.chdata[0].cplcoe || self.chdata[1].cplcoe) {
                for bnd in 0..self.ncplbnd {
                    self.phsflg[bnd]                = br.read_bool()?;
                }
            }
        } else {
            for ch in 0..channels {
                self.chdata[ch].chincpl = false;
            }
        }
        // stereo rematrixing
        if is_stereo {
            self.rematstr                           = br.read_bool()?;
            if self.rematstr {
                if self.cplbegf > 2 || !self.cplinu {
                    for rbnd in 0..4 {
                        self.rematflg[rbnd]         = br.read_bool()?;
                    }
                }
                if self.cplbegf > 0 && self.cplbegf <= 2 && self.cplinu {
                    for rbnd in 0..3 {
                        self.rematflg[rbnd]         = br.read_bool()?;
                    }
                }
                if self.cplbegf == 0 && self.cplinu {
                    for rbnd in 0..2 {
                        self.rematflg[rbnd]         = br.read_bool()?;
                    }
                }
            }
        }
        // exponent strategy
        if self.cplinu {
            self.chdata[CPL_CHANNEL].read_strategy(br, blk_no)?;
        }
        for ch in 0..channels {
            self.chdata[ch].read_strategy(br, blk_no)?;
        }
        if bsi.lfeon {
            self.chdata[LFE_CHANNEL].expstr         = br.read(1)? as u8;
            validate!(blk_no != 0 || self.chdata[LFE_CHANNEL].expstr != STRATEGY_REUSE);
            self.chdata[LFE_CHANNEL].groups = 2;
            self.chdata[LFE_CHANNEL].startmant = 0;
            self.chdata[LFE_CHANNEL].endmant = 7;
        }
        let cpl_startmant = self.chdata[CPL_CHANNEL].startmant;
        for c in 0..channels {
            let ch = &mut self.chdata[c];
            if ch.expstr != STRATEGY_REUSE && !ch.chincpl {
                ch.chbwcod                          = br.read(6)? as u8;
            }
            if !ch.chincpl {
                ch.startmant = 0;
                ch.endmant   = ((ch.chbwcod as usize) + 12) * 3 + 37;
            } else {
                ch.startmant = 0;
                ch.endmant   = cpl_startmant;
            }
        }
        // set number of mantissas
        if self.cplinu {
            self.chdata[CPL_CHANNEL].read_exps(br, true, false)?;
        }
        for ch in 0..channels {
            self.chdata[ch].read_exps(br, false, false)?;
        }
        if bsi.lfeon {
            self.chdata[LFE_CHANNEL].read_exps(br, false, true)?;
        }
        // bit allocation parameters
        self.baie                                   = br.read_bool()?;
        if self.baie {
            self.sdcycod                            = br.read(2)? as usize;
            self.fdcycod                            = br.read(2)? as usize;
            self.sgaincod                           = br.read(2)? as usize;
            self.dbpbcod                            = br.read(2)? as usize;
            self.floorcod                           = br.read(3)? as usize;
        }
        self.snroffste                              = br.read_bool()?;
        if self.snroffste {
            self.csnroffst                          = br.read(6)? as u8;
            if self.cplinu {
                self.chdata[CPL_CHANNEL].read_snr(br)?;
            }
            for ch in 0..channels {
                self.chdata[ch].read_snr(br)?;
            }
            if bsi.lfeon {
                self.chdata[LFE_CHANNEL].read_snr(br)?;
            }
        }
        if self.cplinu {
            self.cplleake                           = br.read_bool()?;
            if self.cplleake {
                self.cplfleak                       = br.read(3)? as u8;
                self.cplsleak                       = br.read(3)? as u8;
            }
        }
        // delta bit allocation information
        self.deltbaie                               = br.read_bool()?;
        if self.deltbaie {
            if self.cplinu {
                self.chdata[CPL_CHANNEL].deltbae    = br.read(2)? as u8;
                validate!(blk_no != 0 || self.chdata[CPL_CHANNEL].deltbae != 0);
                validate!(self.chdata[CPL_CHANNEL].deltbae != 3);
            }
            for ch in 0..channels {
                self.chdata[ch].deltbae             = br.read(2)? as u8;
                validate!(blk_no != 0 || self.chdata[ch].deltbae != 0);
                validate!(self.chdata[ch].deltbae != 3);
            }
            if self.cplinu {
                self.chdata[CPL_CHANNEL].read_deltbai(br)?;
            }
            for ch in 0..channels {
                self.chdata[ch].read_deltbai(br)?;
            }
        }
        // dummy data
        if br.read_bool()? {
            let skipl                               = br.read(9)?;
            br.skip(skipl * 8)?;
        }

        let all_zero = self.calc_bitalloc(bsi, channels, fscod);
        if all_zero { return Ok(true); }

        // quantised mantissa values
        let mut got_cplchan = false;
        for i in 0..self.bap_buf_fill.len() { self.bap_buf_fill[i] = 0; }
        for c in 0..channels {
            {
                let ch = &mut self.chdata[c];
                ch.read_mant(br, &mut self.bap_buf, &mut self.bap_buf_fill)?;
            }
            if self.cplinu && self.chdata[c].chincpl && !got_cplchan {
                let cplch = &mut self.chdata[CPL_CHANNEL];
                cplch.read_mant(br, &mut self.bap_buf, &mut self.bap_buf_fill)?;
                got_cplchan = true;
            }
        }
        if bsi.lfeon {
            let lfech = &mut self.chdata[LFE_CHANNEL];
            lfech.read_mant(br, &mut self.bap_buf, &mut self.bap_buf_fill)?;
        }
        Ok(false)
    }
    fn calc_bitalloc(&mut self, bsi: &BSI, channels: usize, fscod: usize) -> bool {
        let sdecay1 = TS102366_SLOW_DECAY[self.sdcycod];
        let fdecay1 = TS102366_FAST_DECAY[self.fdcycod];

        let sdecay  = sdecay1 >> bsi.shift;
        let fdecay  = fdecay1 >> bsi.shift;
        let sgain   = TS102366_SLOW_GAIN[self.sgaincod];
        let dbknee  = TS102366_DBP_TAB[self.dbpbcod];
        let floor   = TS102366_FLOOR_TAB[self.floorcod];

        let mut all_zero = self.csnroffst == 0;
        if !all_zero && self.chdata[LFE_CHANNEL].fsnroffst == 0 {
            for ch in 0..channels {
                if self.chdata[ch].fsnroffst != 0 {
                    all_zero = false;
                    break;
                }
            }
        }
        if all_zero { return true; }

        let mut mask: [i32; MAX_BANDS] = [0; MAX_BANDS];

        if self.cplinu {
            self.chdata[CPL_CHANNEL].compute_bndpsd();
            self.chdata[CPL_CHANNEL].compute_mask(&mut mask, fscod, sgain, fdecay, sdecay, dbknee,
                                                  (u16::from(self.cplfleak) << 8) + 768, (u16::from(self.cplsleak) << 8) + 768, bsi.shift);
            self.chdata[CPL_CHANNEL].apply_delta_info(&mut mask);
            self.chdata[CPL_CHANNEL].calc_snr_offset(self.csnroffst);
            self.chdata[CPL_CHANNEL].compute_bap(&mut mask, floor);
        }
        for ch in 0..channels {
            self.chdata[ch].compute_bndpsd();
            self.chdata[ch].compute_mask(&mut mask, fscod, sgain, fdecay, sdecay, dbknee, 0, 0, bsi.shift);
            self.chdata[ch].apply_delta_info(&mut mask);
            self.chdata[ch].calc_snr_offset(self.csnroffst);
            self.chdata[ch].compute_bap(&mut mask, floor);
        }
        if bsi.lfeon {
            self.chdata[LFE_CHANNEL].compute_bndpsd();
            self.chdata[LFE_CHANNEL].compute_mask(&mut mask, fscod, sgain, fdecay, sdecay, dbknee,
                                                  0, 0, bsi.shift);
            self.chdata[LFE_CHANNEL].calc_snr_offset(self.csnroffst);
            self.chdata[LFE_CHANNEL].compute_bap(&mut mask, floor);
        }

        false
    }
    fn couple_channels(&mut self, acmod: ACMode) {
        if !self.cplinu { return; }
        for ch in 0..acmod.get_num_channels() {
            if !self.chdata[ch].chincpl { continue; }
            let mut pband = 0;
            for band in self.cplbegf..self.cplendf {
                let cband = band - self.cplbegf;
                let comant = self.chdata[ch].cplcomant[cband];
                let mut cotemp = i32::from(if self.chdata[ch].cplcoexp[cband] == 15 { comant << 1 } else { comant + 16 });
                if (acmod == ACMode::Stereo) && (ch == 1) && self.phsflginu && self.phsflg[pband] {
                    cotemp = -cotemp;
                }
                if !self.cplbndstrc[cband] {
                    pband += 1;
                }
                let exp = self.chdata[ch].cplcoexp[cband] + 3 * self.chdata[ch].mstrcplco + 5 - 3;
                let start = band * 12 + 37;
                for bin in 0..12 {
                    self.chdata[ch].mant[start + bin] = (self.chdata[CPL_CHANNEL].mant[start + bin] * cotemp) >> exp;
                }
//todo dither
            }
        }
    }
    fn rematrix(&mut self) {
        let maxbin = self.chdata[0].endmant.min(self.chdata[1].endmant);
        if self.rematflg[0] {
            let end = maxbin.min(25);
            for bin in 13..end {
                let s = self.chdata[0].mant[bin] + self.chdata[1].mant[bin];
                let d = self.chdata[0].mant[bin] - self.chdata[1].mant[bin];
                self.chdata[0].mant[bin] = d;
                self.chdata[1].mant[bin] = s;
            }
            if maxbin <= 25 { return; }
        }
        if self.rematflg[1] {
            let end = maxbin.min(37);
            for bin in 25..end {
                let s = self.chdata[0].mant[bin] + self.chdata[1].mant[bin];
                let d = self.chdata[0].mant[bin] - self.chdata[1].mant[bin];
                self.chdata[0].mant[bin] = d;
                self.chdata[1].mant[bin] = s;
            }
            if maxbin <= 37 { return; }
        }
        if self.rematflg[2] {
            let end = maxbin.min(61);
            for bin in 37..end {
                let s = self.chdata[0].mant[bin] + self.chdata[1].mant[bin];
                let d = self.chdata[0].mant[bin] - self.chdata[1].mant[bin];
                self.chdata[0].mant[bin] = d;
                self.chdata[1].mant[bin] = s;
            }
            if maxbin <= 61 { return; }
        }
        if self.rematflg[3] {
            let end = maxbin;
            for bin in 61..end {
                let s = self.chdata[0].mant[bin] + self.chdata[1].mant[bin];
                let d = self.chdata[0].mant[bin] - self.chdata[1].mant[bin];
                self.chdata[0].mant[bin] = d;
                self.chdata[1].mant[bin] = s;
            }
        }
    }
    fn synth_audio_block(&mut self, imdct512: &mut IMDCTContext, imdct256: &mut IMDCTContext, tmp: &mut IMDCTWorkspace, channel: usize, delay: &mut [f32; BLOCK_LEN], dst: &mut [f32]) {
        self.chdata[channel].synth(imdct512, imdct256, tmp, delay, dst);
    }
}

impl NADecoder for AudioDecoder {
    fn init(&mut self, _supp: &mut NADecoderSupport, info: NACodecInfoRef) -> DecoderResult<()> {
        if let NACodecTypeInfo::Audio(_) = info.get_properties() {
            self.info = info.clone();
            Ok(())
        } else {
            Err(DecoderError::InvalidData)
        }
    }
    fn decode(&mut self, _supp: &mut NADecoderSupport, pkt: &NAPacket) -> DecoderResult<NAFrameRef> {
        let info = pkt.get_stream().get_info();
        validate!(info.get_properties().is_audio());
        let pktbuf = pkt.get_buffer();
        validate!(pktbuf.len() > 5);

        let mut br;
        if (pktbuf[0] == MAGIC_BYTE0) && (pktbuf[1] == MAGIC_BYTE1) {
            br = BitReader::new(pktbuf.as_slice(), BitReaderMode::BE);
        } else if (pktbuf[0] == MAGIC_BYTE1) && (pktbuf[1] == MAGIC_BYTE0) {
            br = BitReader::new(pktbuf.as_slice(), BitReaderMode::LE16MSB);
        } else {
            return Err(DecoderError::InvalidData);
        }

        let sinfo = Syncinfo::read(&mut br)?;
        validate!(sinfo.is_valid());

        let bsi = BSI::read(&mut br)?;
        if bsi.has_addb {
            let len                 = br.read(6)?;
            br.skip((len + 1) * 8)?;
        }

        let duration = BLOCK_LEN * NBLOCKS;

        let core_channels = bsi.acmod.get_num_channels();
        let channels = core_channels + if bsi.lfeon { 1 } else { 0 };

        let ainfo = NAAudioInfo::new(sinfo.samplerate >> bsi.shift, channels as u8,
                                     SND_F32P_FORMAT, BLOCK_LEN);

        let abuf = alloc_audio_buffer(ainfo, duration, bsi.acmod.get_channel_map(bsi.lfeon))?;
        let mut adata = abuf.get_abuf_f32().unwrap();
        let output = adata.get_data_mut().unwrap();

        self.ablk = AudioBlock::new();
        for blk in 0..NBLOCKS {
            let all_zero = self.ablk.read(&mut br, &bsi, sinfo.fscod as usize, blk)?;
            let off = blk * BLOCK_LEN;
            self.ablk.couple_channels(bsi.acmod);
            if bsi.acmod == ACMode::Stereo {
                self.ablk.rematrix();
            }
            for ch in 0..core_channels {
                let dpos = abuf.get_offset(ch) + off;
                let dst = &mut output[dpos..][..BLOCK_LEN];
                if !all_zero {
                    self.ablk.synth_audio_block(&mut self.imdct512, &mut self.imdct256, &mut self.tmp, ch, &mut self.delay[ch], dst);
                } else {
                    self.delay[ch] = [0.0; BLOCK_LEN];
                    for el in dst.iter_mut().take(BLOCK_LEN) { *el = 0.0; }
                }
            }
            if bsi.lfeon {
                let dpos = abuf.get_offset(core_channels) + off;
                let dst = &mut output[dpos..][..BLOCK_LEN];
                if !all_zero {
                    self.ablk.synth_audio_block(&mut self.imdct512, &mut self.imdct256, &mut self.tmp, LFE_CHANNEL, &mut self.delay[LFE_CHANNEL], dst);
                } else {
                    self.delay[LFE_CHANNEL] = [0.0; BLOCK_LEN];
                    for el in dst.iter_mut().take(BLOCK_LEN) { *el = 0.0; }
                }
            }
        }
//todo skip auxdata
//todo do errorcheck

        let mut frm = NAFrame::new_from_pkt(pkt, self.info.replace_info(NACodecTypeInfo::Audio(ainfo)), abuf);
        frm.set_keyframe(true);
        Ok(frm.into_ref())
    }
    fn flush(&mut self) {
        self.delay = [[0.0; BLOCK_LEN]; MAX_CHANNELS + 1];
    }
}

impl NAOptionHandler for AudioDecoder {
    fn get_supported_options(&self) -> &[NAOptionDefinition] { &[] }
    fn set_options(&mut self, _options: &[NAOption]) { }
    fn query_option_value(&self, _name: &str) -> Option<NAValue> { None }
}

pub fn get_decoder() -> Box<dyn NADecoder + Send> {
    Box::new(AudioDecoder::new())
}

#[cfg(test)]
mod test {
    use nihav_core::codecs::RegisteredDecoders;
    use nihav_core::demuxers::RegisteredDemuxers;
    use nihav_codec_support::test::dec_video::test_decode_audio;
    use crate::generic_register_all_decoders;
    use nihav_realmedia::realmedia_register_all_demuxers;
    #[test]
    fn test_ts102366() {
        let mut dmx_reg = RegisteredDemuxers::new();
        realmedia_register_all_demuxers(&mut dmx_reg);
        let mut dec_reg = RegisteredDecoders::new();
        generic_register_all_decoders(&mut dec_reg);

        // sample: https://samples.mplayerhq.hu/real/VC-RV10/sp_sample1.rm
        let file = "assets/RV/sp_sample1.rm";
        test_decode_audio("realmedia", file, Some(12000), None/*Some("ac3")*/, &dmx_reg, &dec_reg);
    }
}

const TS102366_SLOW_DECAY: [u8; 4] = [ 0x0F, 0x11, 0x13, 0x15 ];
const TS102366_FAST_DECAY: [u8; 4] = [ 0x3F, 0x53, 0x67, 0x7B ];
const TS102366_SLOW_GAIN: [u16; 4] = [ 0x540, 0x4D8, 0x478, 0x410 ];
const TS102366_FAST_GAIN: [u16; 8] = [ 0x080, 0x100, 0x180, 0x200, 0x280, 0x300, 0x380, 0x400 ];
const TS102366_DBP_TAB:   [u16; 4] = [ 0x000, 0x700, 0x900, 0xB00 ];
const TS102366_FLOOR_TAB: [u16; 8] = [ 0x02F0, 0x02B0, 0x0270, 0x0230, 0x01F0, 0x0170, 0x00F0, 0xF800 ];

const TS102366_BIN_TO_BAND: [u8; 256] = [
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 28,
    28, 29, 29, 29, 30, 30, 30, 31, 31, 31,
    32, 32, 32, 33, 33, 33, 34, 34, 34, 35,
    35, 35, 35, 35, 35, 36, 36, 36, 36, 36,
    36, 37, 37, 37, 37, 37, 37, 38, 38, 38,
    38, 38, 38, 39, 39, 39, 39, 39, 39, 40,
    40, 40, 40, 40, 40, 41, 41, 41, 41, 41,
    41, 41, 41, 41, 41, 41, 41, 42, 42, 42,
    42, 42, 42, 42, 42, 42, 42, 42, 42, 43,
    43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
    43, 44, 44, 44, 44, 44, 44, 44, 44, 44,
    44, 44, 44, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 45, 46, 46, 46,
    46, 46, 46, 46, 46, 46, 46, 46, 46, 46,
    46, 46, 46, 46, 46, 46, 46, 46, 46, 46,
    46, 47, 47, 47, 47, 47, 47, 47, 47, 47,
    47, 47, 47, 47, 47, 47, 47, 47, 47, 47,
    47, 47, 47, 47, 47, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 0, 0, 0
];
const TS102366_BAND_SIZE: [u8; MAX_BANDS] = [
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    1,  1,  1,  1,  1,  1,  1,  1,  3,  3,
    3,  3,  3,  3,  3,  6,  6,  6,  6,  6,
    6, 12, 12, 12, 12, 24, 24, 24, 24, 24
];
const TS102366_BAND_START: [u8; MAX_BANDS] = [
     0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
    10,  11,  12,  13,  14,  15,  16,  17,  18,  19,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  31,
    34,  37,  40,  43,  46,  49,  55,  61,  67,  73,
    79,  85,  97, 109, 121, 133, 157, 181, 205, 229
];

const TS102366_LATAB: [u16; 256] = [
    0x0040, 0x003f, 0x003e, 0x003d, 0x003c, 0x003b, 0x003a, 0x0039,
    0x0038, 0x0037, 0x0036, 0x0035, 0x0034, 0x0034, 0x0033, 0x0032,
    0x0031, 0x0030, 0x002f, 0x002f, 0x002e, 0x002d, 0x002c, 0x002c,
    0x002b, 0x002a, 0x0029, 0x0029, 0x0028, 0x0027, 0x0026, 0x0026,
    0x0025, 0x0024, 0x0024, 0x0023, 0x0023, 0x0022, 0x0021, 0x0021,
    0x0020, 0x0020, 0x001f, 0x001e, 0x001e, 0x001d, 0x001d, 0x001c,
    0x001c, 0x001b, 0x001b, 0x001a, 0x001a, 0x0019, 0x0019, 0x0018,
    0x0018, 0x0017, 0x0017, 0x0016, 0x0016, 0x0015, 0x0015, 0x0015,
    0x0014, 0x0014, 0x0013, 0x0013, 0x0013, 0x0012, 0x0012, 0x0012,
    0x0011, 0x0011, 0x0011, 0x0010, 0x0010, 0x0010, 0x000f, 0x000f,
    0x000f, 0x000e, 0x000e, 0x000e, 0x000d, 0x000d, 0x000d, 0x000d,
    0x000c, 0x000c, 0x000c, 0x000c, 0x000b, 0x000b, 0x000b, 0x000b,
    0x000a, 0x000a, 0x000a, 0x000a, 0x000a, 0x0009, 0x0009, 0x0009,
    0x0009, 0x0009, 0x0008, 0x0008, 0x0008, 0x0008, 0x0008, 0x0008,
    0x0007, 0x0007, 0x0007, 0x0007, 0x0007, 0x0007, 0x0006, 0x0006,
    0x0006, 0x0006, 0x0006, 0x0006, 0x0006, 0x0006, 0x0005, 0x0005,
    0x0005, 0x0005, 0x0005, 0x0005, 0x0005, 0x0005, 0x0004, 0x0004,
    0x0004, 0x0004, 0x0004, 0x0004, 0x0004, 0x0004, 0x0004, 0x0004,
    0x0004, 0x0003, 0x0003, 0x0003, 0x0003, 0x0003, 0x0003, 0x0003,
    0x0003, 0x0003, 0x0003, 0x0003, 0x0003, 0x0003, 0x0003, 0x0002,
    0x0002, 0x0002, 0x0002, 0x0002, 0x0002, 0x0002, 0x0002, 0x0002,
    0x0002, 0x0002, 0x0002, 0x0002, 0x0002, 0x0002, 0x0002, 0x0002,
    0x0002, 0x0002, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001,
    0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001,
    0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001,
    0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001,
    0x0001, 0x0001, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000
];
const TS102366_HTH: [[u16; MAX_BANDS]; 3] = [
  [
    0x04D0, 0x04D0, 0x0440, 0x0400, 0x03E0, 0x03C0, 0x03B0, 0x03B0, 0x03A0, 0x03A0,
    0x03A0, 0x03A0, 0x03A0, 0x0390, 0x0390, 0x0390, 0x0380, 0x0380, 0x0370, 0x0370,
    0x0360, 0x0360, 0x0350, 0x0350, 0x0340, 0x0340, 0x0330, 0x0320, 0x0310, 0x0300,
    0x02F0, 0x02F0, 0x02F0, 0x02F0, 0x0300, 0x0310, 0x0340, 0x0390, 0x03E0, 0x0420,
    0x0460, 0x0490, 0x04A0, 0x0460, 0x0440, 0x0440, 0x0520, 0x0800, 0x0840, 0x0840,
  ], [
    0x04F0, 0x04F0, 0x0460, 0x0410, 0x03E0, 0x03D0, 0x03C0, 0x03B0, 0x03B0, 0x03A0,
    0x03A0, 0x03A0, 0x03A0, 0x03A0, 0x0390, 0x0390, 0x0390, 0x0380, 0x0380, 0x0380,
    0x0370, 0x0370, 0x0360, 0x0360, 0x0350, 0x0350, 0x0340, 0x0340, 0x0320, 0x0310,
    0x0300, 0x02F0, 0x02F0, 0x02F0, 0x02F0, 0x0300, 0x0320, 0x0350, 0x0390, 0x03E0,
    0x0420, 0x0450, 0x04A0, 0x0490, 0x0460, 0x0440, 0x0480, 0x0630, 0x0840, 0x0840,
  ], [
    0x0580, 0x0580, 0x04B0, 0x0450, 0x0420, 0x03F0, 0x03E0, 0x03D0, 0x03C0, 0x03B0,
    0x03B0, 0x03B0, 0x03A0, 0x03A0, 0x03A0, 0x03A0, 0x03A0, 0x03A0, 0x03A0, 0x03A0,
    0x0390, 0x0390, 0x0390, 0x0390, 0x0380, 0x0380, 0x0380, 0x0370, 0x0360, 0x0350,
    0x0340, 0x0330, 0x0320, 0x0310, 0x0300, 0x02F0, 0x02F0, 0x02F0, 0x0300, 0x0310,
    0x0330, 0x0350, 0x03C0, 0x0410, 0x0470, 0x04A0, 0x0460, 0x0440, 0x0450, 0x04E0,
  ]
];
const TS102366_BAPTAB: [u8; 64] = [
     0,  1,  1,  1,  1,  1,  2,  2,  3,  3,  3,  4,  4,  5,  5,  6,
     6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  8,  9,  9,  9,  9, 10,
    10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 13, 14,
    14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15
];

const TS102366_QUANT3_MAP: [[i32; 3]; 27] = [
 [ -0x0AAAAAA, -0x0AAAAAA, -0x0AAAAAA ],
 [ -0x0AAAAAA, -0x0AAAAAA, 0x00000000 ],
 [ -0x0AAAAAA, -0x0AAAAAA, 0x00AAAAAA ],
 [ -0x0AAAAAA, 0x00000000, -0x0AAAAAA ],
 [ -0x0AAAAAA, 0x00000000, 0x00000000 ],
 [ -0x0AAAAAA, 0x00000000, 0x00AAAAAA ],
 [ -0x0AAAAAA, 0x00AAAAAA, -0x0AAAAAA ],
 [ -0x0AAAAAA, 0x00AAAAAA, 0x00000000 ],
 [ -0x0AAAAAA, 0x00AAAAAA, 0x00AAAAAA ],
 [ 0x00000000, -0x0AAAAAA, -0x0AAAAAA ],
 [ 0x00000000, -0x0AAAAAA, 0x00000000 ],
 [ 0x00000000, -0x0AAAAAA, 0x00AAAAAA ],
 [ 0x00000000, 0x00000000, -0x0AAAAAA ],
 [ 0x00000000, 0x00000000, 0x00000000 ],
 [ 0x00000000, 0x00000000, 0x00AAAAAA ],
 [ 0x00000000, 0x00AAAAAA, -0x0AAAAAA ],
 [ 0x00000000, 0x00AAAAAA, 0x00000000 ],
 [ 0x00000000, 0x00AAAAAA, 0x00AAAAAA ],
 [ 0x00AAAAAA, -0x0AAAAAA, -0x0AAAAAA ],
 [ 0x00AAAAAA, -0x0AAAAAA, 0x00000000 ],
 [ 0x00AAAAAA, -0x0AAAAAA, 0x00AAAAAA ],
 [ 0x00AAAAAA, 0x00000000, -0x0AAAAAA ],
 [ 0x00AAAAAA, 0x00000000, 0x00000000 ],
 [ 0x00AAAAAA, 0x00000000, 0x00AAAAAA ],
 [ 0x00AAAAAA, 0x00AAAAAA, -0x0AAAAAA ],
 [ 0x00AAAAAA, 0x00AAAAAA, 0x00000000 ],
 [ 0x00AAAAAA, 0x00AAAAAA, 0x00AAAAAA ]
];
const TS102366_QUANT5_MAP: [[i32; 3]; 125] = [
 [ -0x0CCCCCC, -0x0CCCCCC, -0x0CCCCCC ], [ -0x0CCCCCC, -0x0CCCCCC, -0x0666666 ], [ -0x0CCCCCC, -0x0CCCCCC, 0x00000000 ],
                                         [ -0x0CCCCCC, -0x0CCCCCC, 0x00666666 ], [ -0x0CCCCCC, -0x0CCCCCC, 0x00CCCCCC ],
 [ -0x0CCCCCC, -0x0666666, -0x0CCCCCC ], [ -0x0CCCCCC, -0x0666666, -0x0666666 ], [ -0x0CCCCCC, -0x0666666, 0x00000000 ],
                                         [ -0x0CCCCCC, -0x0666666, 0x00666666 ], [ -0x0CCCCCC, -0x0666666, 0x00CCCCCC ],
 [ -0x0CCCCCC, 0x00000000, -0x0CCCCCC ], [ -0x0CCCCCC, 0x00000000, -0x0666666 ], [ -0x0CCCCCC, 0x00000000, 0x00000000 ],
                                         [ -0x0CCCCCC, 0x00000000, 0x00666666 ], [ -0x0CCCCCC, 0x00000000, 0x00CCCCCC ],
 [ -0x0CCCCCC, 0x00666666, -0x0CCCCCC ], [ -0x0CCCCCC, 0x00666666, -0x0666666 ], [ -0x0CCCCCC, 0x00666666, 0x00000000 ],
                                         [ -0x0CCCCCC, 0x00666666, 0x00666666 ], [ -0x0CCCCCC, 0x00666666, 0x00CCCCCC ],
 [ -0x0CCCCCC, 0x00CCCCCC, -0x0CCCCCC ], [ -0x0CCCCCC, 0x00CCCCCC, -0x0666666 ], [ -0x0CCCCCC, 0x00CCCCCC, 0x00000000 ],
                                         [ -0x0CCCCCC, 0x00CCCCCC, 0x00666666 ], [ -0x0CCCCCC, 0x00CCCCCC, 0x00CCCCCC ],
 [ -0x0666666, -0x0CCCCCC, -0x0CCCCCC ], [ -0x0666666, -0x0CCCCCC, -0x0666666 ], [ -0x0666666, -0x0CCCCCC, 0x00000000 ],
                                         [ -0x0666666, -0x0CCCCCC, 0x00666666 ], [ -0x0666666, -0x0CCCCCC, 0x00CCCCCC ],
 [ -0x0666666, -0x0666666, -0x0CCCCCC ], [ -0x0666666, -0x0666666, -0x0666666 ], [ -0x0666666, -0x0666666, 0x00000000 ],
                                         [ -0x0666666, -0x0666666, 0x00666666 ], [ -0x0666666, -0x0666666, 0x00CCCCCC ],
 [ -0x0666666, 0x00000000, -0x0CCCCCC ], [ -0x0666666, 0x00000000, -0x0666666 ], [ -0x0666666, 0x00000000, 0x00000000 ],
                                         [ -0x0666666, 0x00000000, 0x00666666 ], [ -0x0666666, 0x00000000, 0x00CCCCCC ],
 [ -0x0666666, 0x00666666, -0x0CCCCCC ], [ -0x0666666, 0x00666666, -0x0666666 ], [ -0x0666666, 0x00666666, 0x00000000 ],
                                         [ -0x0666666, 0x00666666, 0x00666666 ], [ -0x0666666, 0x00666666, 0x00CCCCCC ],
 [ -0x0666666, 0x00CCCCCC, -0x0CCCCCC ], [ -0x0666666, 0x00CCCCCC, -0x0666666 ], [ -0x0666666, 0x00CCCCCC, 0x00000000 ],
                                         [ -0x0666666, 0x00CCCCCC, 0x00666666 ], [ -0x0666666, 0x00CCCCCC, 0x00CCCCCC ],
 [ 0x00000000, -0x0CCCCCC, -0x0CCCCCC ], [ 0x00000000, -0x0CCCCCC, -0x0666666 ], [ 0x00000000, -0x0CCCCCC, 0x00000000 ],
                                         [ 0x00000000, -0x0CCCCCC, 0x00666666 ], [ 0x00000000, -0x0CCCCCC, 0x00CCCCCC ],
 [ 0x00000000, -0x0666666, -0x0CCCCCC ], [ 0x00000000, -0x0666666, -0x0666666 ], [ 0x00000000, -0x0666666, 0x00000000 ],
                                         [ 0x00000000, -0x0666666, 0x00666666 ], [ 0x00000000, -0x0666666, 0x00CCCCCC ],
 [ 0x00000000, 0x00000000, -0x0CCCCCC ], [ 0x00000000, 0x00000000, -0x0666666 ], [ 0x00000000, 0x00000000, 0x00000000 ],
                                         [ 0x00000000, 0x00000000, 0x00666666 ], [ 0x00000000, 0x00000000, 0x00CCCCCC ],
 [ 0x00000000, 0x00666666, -0x0CCCCCC ], [ 0x00000000, 0x00666666, -0x0666666 ], [ 0x00000000, 0x00666666, 0x00000000 ],
                                         [ 0x00000000, 0x00666666, 0x00666666 ], [ 0x00000000, 0x00666666, 0x00CCCCCC ],
 [ 0x00000000, 0x00CCCCCC, -0x0CCCCCC ], [ 0x00000000, 0x00CCCCCC, -0x0666666 ], [ 0x00000000, 0x00CCCCCC, 0x00000000 ],
                                         [ 0x00000000, 0x00CCCCCC, 0x00666666 ], [ 0x00000000, 0x00CCCCCC, 0x00CCCCCC ],
 [ 0x00666666, -0x0CCCCCC, -0x0CCCCCC ], [ 0x00666666, -0x0CCCCCC, -0x0666666 ], [ 0x00666666, -0x0CCCCCC, 0x00000000 ],
                                         [ 0x00666666, -0x0CCCCCC, 0x00666666 ], [ 0x00666666, -0x0CCCCCC, 0x00CCCCCC ],
 [ 0x00666666, -0x0666666, -0x0CCCCCC ], [ 0x00666666, -0x0666666, -0x0666666 ], [ 0x00666666, -0x0666666, 0x00000000 ],
                                         [ 0x00666666, -0x0666666, 0x00666666 ], [ 0x00666666, -0x0666666, 0x00CCCCCC ],
 [ 0x00666666, 0x00000000, -0x0CCCCCC ], [ 0x00666666, 0x00000000, -0x0666666 ], [ 0x00666666, 0x00000000, 0x00000000 ],
                                         [ 0x00666666, 0x00000000, 0x00666666 ], [ 0x00666666, 0x00000000, 0x00CCCCCC ],
 [ 0x00666666, 0x00666666, -0x0CCCCCC ], [ 0x00666666, 0x00666666, -0x0666666 ], [ 0x00666666, 0x00666666, 0x00000000 ],
                                         [ 0x00666666, 0x00666666, 0x00666666 ], [ 0x00666666, 0x00666666, 0x00CCCCCC ],
 [ 0x00666666, 0x00CCCCCC, -0x0CCCCCC ], [ 0x00666666, 0x00CCCCCC, -0x0666666 ], [ 0x00666666, 0x00CCCCCC, 0x00000000 ],
                                         [ 0x00666666, 0x00CCCCCC, 0x00666666 ], [ 0x00666666, 0x00CCCCCC, 0x00CCCCCC ],
 [ 0x00CCCCCC, -0x0CCCCCC, -0x0CCCCCC ], [ 0x00CCCCCC, -0x0CCCCCC, -0x0666666 ], [ 0x00CCCCCC, -0x0CCCCCC, 0x00000000 ],
                                         [ 0x00CCCCCC, -0x0CCCCCC, 0x00666666 ], [ 0x00CCCCCC, -0x0CCCCCC, 0x00CCCCCC ],
 [ 0x00CCCCCC, -0x0666666, -0x0CCCCCC ], [ 0x00CCCCCC, -0x0666666, -0x0666666 ], [ 0x00CCCCCC, -0x0666666, 0x00000000 ],
                                         [ 0x00CCCCCC, -0x0666666, 0x00666666 ], [ 0x00CCCCCC, -0x0666666, 0x00CCCCCC ],
 [ 0x00CCCCCC, 0x00000000, -0x0CCCCCC ], [ 0x00CCCCCC, 0x00000000, -0x0666666 ], [ 0x00CCCCCC, 0x00000000, 0x00000000 ],
                                         [ 0x00CCCCCC, 0x00000000, 0x00666666 ], [ 0x00CCCCCC, 0x00000000, 0x00CCCCCC ],
 [ 0x00CCCCCC, 0x00666666, -0x0CCCCCC ], [ 0x00CCCCCC, 0x00666666, -0x0666666 ], [ 0x00CCCCCC, 0x00666666, 0x00000000 ],
                                         [ 0x00CCCCCC, 0x00666666, 0x00666666 ], [ 0x00CCCCCC, 0x00666666, 0x00CCCCCC ],
 [ 0x00CCCCCC, 0x00CCCCCC, -0x0CCCCCC ], [ 0x00CCCCCC, 0x00CCCCCC, -0x0666666 ], [ 0x00CCCCCC, 0x00CCCCCC, 0x00000000 ],
                                         [ 0x00CCCCCC, 0x00CCCCCC, 0x00666666 ], [ 0x00CCCCCC, 0x00CCCCCC, 0x00CCCCCC ],
];
const TS102366_QUANT7_MAP: [i32; 7] = [
    -0xDB6DB6, -0x924924, -0x492492, 0x000000, 0x492492, 0x924924, 0xDB6DB6
];
const TS102366_QUANT11_MAP: [[i32; 2]; 121] = [
 [ -0x0E8BA2E, -0x0E8BA2E ], [ -0x0E8BA2E, -0x0BA2E8B ], [ -0x0E8BA2E, -0x08BA2E8 ],
 [ -0x0E8BA2E, -0x05D1745 ], [ -0x0E8BA2E, -0x02E8BA2 ], [ -0x0E8BA2E, 0x00000000 ],
 [ -0x0E8BA2E, 0x002E8BA2 ], [ -0x0E8BA2E, 0x005D1745 ], [ -0x0E8BA2E, 0x008BA2E8 ],
 [ -0x0E8BA2E, 0x00BA2E8B ], [ -0x0E8BA2E, 0x00E8BA2E ], [ -0x0BA2E8B, -0x0E8BA2E ],
 [ -0x0BA2E8B, -0x0BA2E8B ], [ -0x0BA2E8B, -0x08BA2E8 ], [ -0x0BA2E8B, -0x05D1745 ],
 [ -0x0BA2E8B, -0x02E8BA2 ], [ -0x0BA2E8B, 0x00000000 ], [ -0x0BA2E8B, 0x002E8BA2 ],
 [ -0x0BA2E8B, 0x005D1745 ], [ -0x0BA2E8B, 0x008BA2E8 ], [ -0x0BA2E8B, 0x00BA2E8B ],
 [ -0x0BA2E8B, 0x00E8BA2E ], [ -0x08BA2E8, -0x0E8BA2E ], [ -0x08BA2E8, -0x0BA2E8B ],
 [ -0x08BA2E8, -0x08BA2E8 ], [ -0x08BA2E8, -0x05D1745 ], [ -0x08BA2E8, -0x02E8BA2 ],
 [ -0x08BA2E8, 0x00000000 ], [ -0x08BA2E8, 0x002E8BA2 ], [ -0x08BA2E8, 0x005D1745 ],
 [ -0x08BA2E8, 0x008BA2E8 ], [ -0x08BA2E8, 0x00BA2E8B ], [ -0x08BA2E8, 0x00E8BA2E ],
 [ -0x05D1745, -0x0E8BA2E ], [ -0x05D1745, -0x0BA2E8B ], [ -0x05D1745, -0x08BA2E8 ],
 [ -0x05D1745, -0x05D1745 ], [ -0x05D1745, -0x02E8BA2 ], [ -0x05D1745, 0x00000000 ],
 [ -0x05D1745, 0x002E8BA2 ], [ -0x05D1745, 0x005D1745 ], [ -0x05D1745, 0x008BA2E8 ],
 [ -0x05D1745, 0x00BA2E8B ], [ -0x05D1745, 0x00E8BA2E ], [ -0x02E8BA2, -0x0E8BA2E ],
 [ -0x02E8BA2, -0x0BA2E8B ], [ -0x02E8BA2, -0x08BA2E8 ], [ -0x02E8BA2, -0x05D1745 ],
 [ -0x02E8BA2, -0x02E8BA2 ], [ -0x02E8BA2, 0x00000000 ], [ -0x02E8BA2, 0x002E8BA2 ],
 [ -0x02E8BA2, 0x005D1745 ], [ -0x02E8BA2, 0x008BA2E8 ], [ -0x02E8BA2, 0x00BA2E8B ],
 [ -0x02E8BA2, 0x00E8BA2E ], [ 0x00000000, -0x0E8BA2E ], [ 0x00000000, -0x0BA2E8B ],
 [ 0x00000000, -0x08BA2E8 ], [ 0x00000000, -0x05D1745 ], [ 0x00000000, -0x02E8BA2 ],
 [ 0x00000000, 0x00000000 ], [ 0x00000000, 0x002E8BA2 ], [ 0x00000000, 0x005D1745 ],
 [ 0x00000000, 0x008BA2E8 ], [ 0x00000000, 0x00BA2E8B ], [ 0x00000000, 0x00E8BA2E ],
 [ 0x002E8BA2, -0x0E8BA2E ], [ 0x002E8BA2, -0x0BA2E8B ], [ 0x002E8BA2, -0x08BA2E8 ],
 [ 0x002E8BA2, -0x05D1745 ], [ 0x002E8BA2, -0x02E8BA2 ], [ 0x002E8BA2, 0x00000000 ],
 [ 0x002E8BA2, 0x002E8BA2 ], [ 0x002E8BA2, 0x005D1745 ], [ 0x002E8BA2, 0x008BA2E8 ],
 [ 0x002E8BA2, 0x00BA2E8B ], [ 0x002E8BA2, 0x00E8BA2E ], [ 0x005D1745, -0x0E8BA2E ],
 [ 0x005D1745, -0x0BA2E8B ], [ 0x005D1745, -0x08BA2E8 ], [ 0x005D1745, -0x05D1745 ],
 [ 0x005D1745, -0x02E8BA2 ], [ 0x005D1745, 0x00000000 ], [ 0x005D1745, 0x002E8BA2 ],
 [ 0x005D1745, 0x005D1745 ], [ 0x005D1745, 0x008BA2E8 ], [ 0x005D1745, 0x00BA2E8B ],
 [ 0x005D1745, 0x00E8BA2E ], [ 0x008BA2E8, -0x0E8BA2E ], [ 0x008BA2E8, -0x0BA2E8B ],
 [ 0x008BA2E8, -0x08BA2E8 ], [ 0x008BA2E8, -0x05D1745 ], [ 0x008BA2E8, -0x02E8BA2 ],
 [ 0x008BA2E8, 0x00000000 ], [ 0x008BA2E8, 0x002E8BA2 ], [ 0x008BA2E8, 0x005D1745 ],
 [ 0x008BA2E8, 0x008BA2E8 ], [ 0x008BA2E8, 0x00BA2E8B ], [ 0x008BA2E8, 0x00E8BA2E ],
 [ 0x00BA2E8B, -0x0E8BA2E ], [ 0x00BA2E8B, -0x0BA2E8B ], [ 0x00BA2E8B, -0x08BA2E8 ],
 [ 0x00BA2E8B, -0x05D1745 ], [ 0x00BA2E8B, -0x02E8BA2 ], [ 0x00BA2E8B, 0x00000000 ],
 [ 0x00BA2E8B, 0x002E8BA2 ], [ 0x00BA2E8B, 0x005D1745 ], [ 0x00BA2E8B, 0x008BA2E8 ],
 [ 0x00BA2E8B, 0x00BA2E8B ], [ 0x00BA2E8B, 0x00E8BA2E ], [ 0x00E8BA2E, -0x0E8BA2E ],
 [ 0x00E8BA2E, -0x0BA2E8B ], [ 0x00E8BA2E, -0x08BA2E8 ], [ 0x00E8BA2E, -0x05D1745 ],
 [ 0x00E8BA2E, -0x02E8BA2 ], [ 0x00E8BA2E, 0x00000000 ], [ 0x00E8BA2E, 0x002E8BA2 ],
 [ 0x00E8BA2E, 0x005D1745 ], [ 0x00E8BA2E, 0x008BA2E8 ], [ 0x00E8BA2E, 0x00BA2E8B ],
 [ 0x00E8BA2E, 0x00E8BA2E ],
];
const TS102366_QUANT15_MAP: [i32; 15] = [
 -0x0EEEEEE, -0x0CCCCCC, -0x0AAAAAA, -0x0888888, -0x0666666, -0x0444444, -0x0222222, 0x00000000,
             0x00222222, 0x00444444, 0x00666666, 0x00888888, 0x00AAAAAA, 0x00CCCCCC, 0x00EEEEEE,
];
const TS102366_BAP_BITS: [u8; 10] = [ 5, 6, 7, 8, 9, 10, 11, 12, 14, 16 ];

const TS102366_WINDOW: [f32; 256] = [
    0.00014, 0.00024, 0.00037, 0.00051, 0.00067, 0.00086, 0.00107, 0.00130,
    0.00157, 0.00187, 0.00220, 0.00256, 0.00297, 0.00341, 0.00390, 0.00443,
    0.00501, 0.00564, 0.00632, 0.00706, 0.00785, 0.00871, 0.00962, 0.01061,
    0.01166, 0.01279, 0.01399, 0.01526, 0.01662, 0.01806, 0.01959, 0.02121,
    0.02292, 0.02472, 0.02662, 0.02863, 0.03073, 0.03294, 0.03527, 0.03770,
    0.04025, 0.04292, 0.04571, 0.04862, 0.05165, 0.05481, 0.05810, 0.06153,
    0.06508, 0.06878, 0.07261, 0.07658, 0.08069, 0.08495, 0.08935, 0.09389,
    0.09859, 0.10343, 0.10842, 0.11356, 0.11885, 0.12429, 0.12988, 0.13563,
    0.14152, 0.14757, 0.15376, 0.16011, 0.16661, 0.17325, 0.18005, 0.18699,
    0.19407, 0.20130, 0.20867, 0.21618, 0.22382, 0.23161, 0.23952, 0.24757,
    0.25574, 0.26404, 0.27246, 0.28100, 0.28965, 0.29841, 0.30729, 0.31626,
    0.32533, 0.33450, 0.34376, 0.35311, 0.36253, 0.37204, 0.38161, 0.39126,
    0.40096, 0.41072, 0.42054, 0.43040, 0.44030, 0.45023, 0.46020, 0.47019,
    0.48020, 0.49022, 0.50025, 0.51028, 0.52031, 0.53033, 0.54033, 0.55031,
    0.56026, 0.57019, 0.58007, 0.58991, 0.59970, 0.60944, 0.61912, 0.62873,
    0.63827, 0.64774, 0.65713, 0.66643, 0.67564, 0.68476, 0.69377, 0.70269,
    0.71150, 0.72019, 0.72877, 0.73723, 0.74557, 0.75378, 0.76186, 0.76981,
    0.77762, 0.78530, 0.79283, 0.80022, 0.80747, 0.81457, 0.82151, 0.82831,
    0.83496, 0.84145, 0.84779, 0.85398, 0.86001, 0.86588, 0.87160, 0.87716,
    0.88257, 0.88782, 0.89291, 0.89785, 0.90264, 0.90728, 0.91176, 0.91610,
    0.92028, 0.92432, 0.92822, 0.93197, 0.93558, 0.93906, 0.94240, 0.94560,
    0.94867, 0.95162, 0.95444, 0.95713, 0.95971, 0.96217, 0.96451, 0.96674,
    0.96887, 0.97089, 0.97281, 0.97463, 0.97635, 0.97799, 0.97953, 0.98099,
    0.98236, 0.98366, 0.98488, 0.98602, 0.98710, 0.98811, 0.98905, 0.98994,
    0.99076, 0.99153, 0.99225, 0.99291, 0.99353, 0.99411, 0.99464, 0.99513,
    0.99558, 0.99600, 0.99639, 0.99674, 0.99706, 0.99736, 0.99763, 0.99788,
    0.99811, 0.99831, 0.99850, 0.99867, 0.99882, 0.99895, 0.99908, 0.99919,
    0.99929, 0.99938, 0.99946, 0.99953, 0.99959, 0.99965, 0.99969, 0.99974,
    0.99978, 0.99981, 0.99984, 0.99986, 0.99988, 0.99990, 0.99992, 0.99993,
    0.99994, 0.99995, 0.99996, 0.99997, 0.99998, 0.99998, 0.99998, 0.99999,
    0.99999, 0.99999, 0.99999, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000,
    1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000,
];
