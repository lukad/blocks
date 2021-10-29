use super::{lode::Lode, voxel::Block};

pub struct Biome {
    pub ground_height: u8,
    pub terrain_height: u8,
    pub terrain_scale: f64,
    pub surface_block: Block,
    pub near_surface_block: Block,
    pub other_block: Block,
    pub lodes: Vec<Lode>,
}

impl Default for Biome {
    fn default() -> Self {
        Self {
            ground_height: 64,
            terrain_height: 16,
            terrain_scale: 0.025,
            surface_block: Block::Grass,
            near_surface_block: Block::Dirt,
            other_block: Block::Stone,
            lodes: vec![
                // Cave
                Lode {
                    block: Block::Air,
                    range: 1..64,
                    scale: 0.1,
                    threshold: 0.55,
                    offset: 123.0,
                },
            ],
        }
    }
}
