mod biome;
mod chunk;
mod lode;
mod voxel;

use biome::Biome;
use voxel::Block;

use bevy::{
    math::{DVec2, DVec3, Vec3Swizzles},
    prelude::*,
};
use noise::{Fbm, NoiseFn};

pub struct VoxelPlugin;

impl Plugin for VoxelPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(ChunkWorld::default())
            .add_plugin(chunk::ChunkPlugin);
    }
}

pub(crate) struct ChunkWorld {
    pub(crate) fbm: Fbm,
}

impl Default for ChunkWorld {
    fn default() -> Self {
        Self {
            fbm: Default::default(),
        }
    }
}

impl ChunkWorld {
    pub(crate) fn get_noise_2d(&self, position: DVec2, scale: f64, offset: f64) -> f64 {
        let param = position * scale + DVec2::new(offset, offset);
        self.fbm.get([param.x, 0.0, param.y])
    }

    pub(crate) fn get_noise_3d(
        &self,
        position: DVec3,
        scale: f64,
        offset: f64,
        threshold: f64,
    ) -> bool {
        let param = position * scale + DVec3::new(offset, offset, offset);
        (self.fbm.get([param.x, param.y, param.z]) * 0.5 + 0.5) > threshold
    }

    pub(crate) fn get_height(&self, biome: &Biome, coord: &IVec2) -> i32 {
        let noise = self.get_noise_2d(coord.as_dvec2(), biome.terrain_scale, 0.0);
        (biome.terrain_height as f64 * noise) as i32 + biome.ground_height as i32
    }

    pub(crate) fn get_biome(&self, _coord: &IVec2) -> Biome {
        Biome::default()
    }

    pub(crate) fn get_block(&self, coord: &IVec3) -> Block {
        let height = self.get_height(&self.get_biome(&coord.xz()), &coord.xz());
        self.get_block_with_height(&coord, height)
    }

    pub(crate) fn get_block_with_height(&self, coord: &IVec3, height: i32) -> Block {
        let biome = self.get_biome(&coord.xz());
        let mut block = Block::Air;

        if coord.y == 0 {
            return Block::Bedrock;
        }

        if coord.y > height {
            return block;
        }

        if coord.y == height {
            block = biome.surface_block;
        } else if coord.y < height && coord.y > height - 4 {
            block = biome.near_surface_block;
        } else {
            block = biome.other_block
        }

        for lode in biome.lodes {
            if lode.range.contains(&(coord.y as u8)) {
                if self.get_noise_3d(coord.as_dvec3(), lode.scale, lode.offset, lode.threshold) {
                    block = lode.block;
                }
            }
        }

        block
    }
}
