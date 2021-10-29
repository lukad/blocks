#[repr(usize)]
pub enum Face {
    Front = 0,
    Back = 1,
    Left = 2,
    Right = 3,
    Top = 4,
    Bottom = 5,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(unused)]
#[repr(u8)]
pub enum Block {
    Air,
    Stone,
    Grass,
    Dirt,
    Bedrock,
}

impl Block {
    #[allow(unused)]
    pub fn is_solid(&self) -> bool {
        match self {
            Block::Air => false,
            _ => true,
        }
    }

    #[allow(unused)]
    pub fn is_air(&self) -> bool {
        match self {
            Block::Air => true,
            _ => false,
        }
    }

    #[inline(always)]
    pub fn uvs(&self, face: &Face) -> [[f32; 2]; 4] {
        match (self, face) {
            (Block::Air, _) => [[0.0, 0.0]; 4],
            (Block::Stone, _) => [[0.25, 0.0], [0.0, 0.0], [0.0, 0.25], [0.25, 0.25]],
            (Block::Grass, &Face::Top) => [[1.0, 0.25], [0.75, 0.25], [0.75, 0.5], [1.0, 0.5]],
            (Block::Grass, &Face::Bottom) => [[0.5, 0.0], [0.25, 0.0], [0.25, 0.25], [0.5, 0.25]],
            (Block::Grass, _) => [[0.75, 0.0], [0.5, 0.0], [0.5, 0.25], [0.75, 0.25]],
            (Block::Dirt, _) => [[0.5, 0.0], [0.25, 0.0], [0.25, 0.25], [0.5, 0.25]],
            (Block::Bedrock, _) => [[0.5, 0.5], [0.25, 0.5], [0.25, 0.75], [0.5, 0.75]],
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Voxel {
    pub block: Block,
    pub light: u8,
}

impl Default for Voxel {
    fn default() -> Self {
        Self {
            block: Block::Air,
            light: 0xF0,
        }
    }
}
