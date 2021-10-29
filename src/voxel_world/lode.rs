use std::ops::Range;

use super::voxel::Block;

pub struct Lode {
    pub block: Block,
    pub range: Range<u8>,
    pub scale: f64,
    pub threshold: f64,
    pub offset: f64,
}
