#[allow(clippy::uninit_assumed_init)]
mod luma_mc;
pub use luma_mc::H264_LUMA_INTERP;
mod chroma_mc;
pub use chroma_mc::*;
mod blockdsp;
use blockdsp::*;

impl super::RegisterSIMD for super::H264MC {
    fn register_simd(&mut self) {
        self.avg[1] = avg_4;
        self.avg[2] = avg_8;
        self.avg[3] = avg_16;
        self.put_block_weighted[1] = put_block_weighted_4;
        self.put_block_weighted[2] = put_block_weighted_8;
        self.put_block_weighted[3] = put_block_weighted_16;
        self.put_block_weighted2[1] = put_block_weighted2_4;
        self.put_block_weighted2[2] = put_block_weighted2_8;
        self.put_block_weighted2[3] = put_block_weighted2_16;
    }
}
