#[cfg(feature="decoder_smush_video")]
enum GlyphEdge {
    Left,
    Top,
    Right,
    Bottom,
    None
}

#[cfg(feature="decoder_smush_video")]
impl GlyphEdge {
    fn get(x: usize, y: usize, size: usize) -> Self {
        if y == 0 {
            GlyphEdge::Bottom
        } else if y == size - 1 {
            GlyphEdge::Top
        } else if x == 0 {
            GlyphEdge::Left
        } else if x == size - 1 {
            GlyphEdge::Right
        } else {
            GlyphEdge::None
        }
    }
}

#[cfg(feature="decoder_smush_video")]
enum GlyphDir {
    Left,
    Up,
    Right,
    Down,
    None
}

#[cfg(feature="decoder_smush_video")]
impl GlyphDir {
    fn get(edge0: GlyphEdge, edge1: GlyphEdge) -> Self {
        match (edge0, edge1) {
            (GlyphEdge::Left,  GlyphEdge::Right) |
            (GlyphEdge::Right, GlyphEdge::Left) => GlyphDir::Up,
            (GlyphEdge::Top, GlyphEdge::Bottom) |
            (GlyphEdge::Bottom, GlyphEdge::Top) => GlyphDir::Right,
            (GlyphEdge::Bottom, _) |
            (_, GlyphEdge::Bottom) => GlyphDir::Up,
            (GlyphEdge::Top, _) |
            (_, GlyphEdge::Top) => GlyphDir::Down,
            (GlyphEdge::Left, _) |
            (_, GlyphEdge::Left) => GlyphDir::Left,
            (GlyphEdge::Right, _) |
            (_, GlyphEdge::Right) => GlyphDir::Right,
            _ => GlyphDir::None,
        }
    }
}

#[cfg(feature="decoder_smush_video")]
const XVEC4: [usize; 16] = [0, 1, 2, 3, 3, 3, 3, 2, 1, 0, 0, 0, 1, 2, 2, 1];
#[cfg(feature="decoder_smush_video")]
const YVEC4: [usize; 16] = [0, 0, 0, 0, 1, 2, 3, 3, 3, 3, 2, 1, 1, 1, 2, 2];
#[cfg(feature="decoder_smush_video")]
const XVEC8: [usize; 16] = [0, 2, 5, 7, 7, 7, 7, 7, 7, 5, 2, 0, 0, 0, 0, 0];
#[cfg(feature="decoder_smush_video")]
const YVEC8: [usize; 16] = [0, 0, 0, 0, 1, 3, 4, 6, 7, 7, 7, 7, 6, 4, 3, 1];

#[cfg(feature="decoder_smush_video")]
fn make_glyphs_47(glyphs4: &mut [[u8; 16]; 256], glyphs8: &mut [[u8; 64]; 256]) {
    for (n, glyph) in glyphs4.iter_mut().enumerate() {
        let i = n >> 4;
        let j = n & 0xF;
        make_glyph_47(glyph, XVEC4[i], YVEC4[i], XVEC4[j], YVEC4[j], 4);
    }
    for (n, glyph) in glyphs8.iter_mut().enumerate() {
        let i = n >> 4;
        let j = n & 0xF;
        make_glyph_47(glyph, XVEC8[i], YVEC8[i], XVEC8[j], YVEC8[j], 8);
    }
}
#[cfg(feature="decoder_smush_video")]
fn make_glyph_47(dst: &mut [u8], xi: usize, yi: usize, xj: usize, yj: usize, size: usize) {
    let edge0 = GlyphEdge::get(xi, yi, size);
    let edge1 = GlyphEdge::get(xj, yj, size);
    let dir = GlyphDir::get(edge0, edge1);
    let npoints = if xi > xj { xi - xj } else { xj - xi }.max(if yi > yj { yi - yj } else { yj - yi });
    for ipoint in 0..=npoints {
        let (p0, p1) = if npoints > 0 {
                (interpolate(xi, xj, ipoint, npoints),
                 interpolate(yi, yj, ipoint, npoints))
            } else {
                (xi, yi)
            };
        let off = p0 + p1 * size;
        match dir {
            GlyphDir::Up => {
                for i in 0..=p1 {
                    dst[off - i * size] = 1;
                }
            },
            GlyphDir::Down => {
                for i in 0..size-p1 {
                    dst[off + i * size] = 1;
                }
            },
            GlyphDir::Left => {
                for i in 0..=p0 {
                    dst[off - i] = 1;
                }
            },
            GlyphDir::Right => {
                for i in 0..size-p0 {
                    dst[off + i] = 1;
                }
            },
            _ => {},
        };
    }
}
#[cfg(feature="decoder_smush_video")]
fn interpolate(a: usize, b: usize, pos1: usize, range: usize) -> usize {
    (a * pos1 + b * (range - pos1) + range / 2) / range
}

#[cfg(feature="decoder_smush_video")]
mod v1;
#[cfg(feature="decoder_smush_video")]
pub use v1::get_decoder_video_v1;
#[cfg(feature="decoder_smush_video")]
mod v2;
#[cfg(feature="decoder_smush_video")]
pub use v2::get_decoder_video_v2;

#[cfg(feature="decoder_smush_audio")]
mod iact;
#[cfg(feature="decoder_smush_audio")]
pub use iact::get_decoder_iact;
#[cfg(feature="decoder_smush_audio")]
mod vima;
#[cfg(feature="decoder_smush_audio")]
pub use vima::get_decoder_vima;
