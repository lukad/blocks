use std::collections::{HashMap, HashSet};

use super::{
    biome::Biome,
    voxel::{Block, Face, Voxel},
};
use crate::voxel_world::ChunkWorld;

use bevy::{
    diagnostic::{Diagnostic, DiagnosticId, Diagnostics},
    ecs::system::{lifetimeless::SRes, SystemParamItem},
    math::Vec3Swizzles,
    pbr::MaterialPipeline,
    prelude::*,
    reflect::TypeUuid,
    render::{
        mesh::{Indices, InnerMeshVertexBufferLayout, MeshVertexAttribute, VertexAttributeValues},
        render_asset::{PrepareAssetError, RenderAsset, RenderAssets},
        render_resource::{
            std140::{AsStd140, Std140},
            BindGroup, BindGroupDescriptor, BindGroupEntry, BindGroupLayoutDescriptor,
            BindGroupLayoutEntry, BindingResource, BindingType, Buffer, BufferBindingType,
            BufferInitDescriptor, BufferSize, BufferUsages, PrimitiveTopology,
            RenderPipelineDescriptor, SamplerBindingType, ShaderStages,
            SpecializedMeshPipelineError, TextureSampleType, TextureViewDimension, VertexFormat,
        },
        renderer::RenderDevice,
    },
    utils::{Hashed, Instant},
};

use lazy_static::lazy_static;

const CHUNK_DATA_GENERATION_TIME: DiagnosticId =
    DiagnosticId::from_u128(74345255989977209082044397486567331055);
const CHUNK_MESH_GENERATION_TIME: DiagnosticId =
    DiagnosticId::from_u128(248235940287512337153113599483964887382);

const CHUNK_WIDTH: usize = 16;
const CHUNK_HEIGHT: usize = 128;

#[derive(Component)]
pub struct GenerateLightmap;
#[derive(Component)]
pub struct ChunkToSpawn(IVec2);
#[derive(Component)]
pub struct Chunk {
    pub coord: IVec2,
    pub biome: Biome,
}

#[derive(Component)]
pub struct ChunkData {
    voxels: HashMap<IVec3, Voxel>,
}

impl Default for ChunkData {
    fn default() -> Self {
        Self {
            voxels: HashMap::new(),
        }
    }
}

pub struct ChunkMeshData {
    pub vertices: Vec<Vec3>,
    pub indices: Vec<u32>,
    pub normals: Vec<[f32; 3]>,
    pub uvs: Vec<[f32; 2]>,
    pub data: Vec<u32>,
}

lazy_static! {
    static ref FACE_DATA: [(Face, [Vec3; 4], IVec3); 6] = [
        (Face::Front, [Vec3::new(1f32, 1f32, 0f32), Vec3::new(0f32, 1f32, 0f32), Vec3::new(0f32, 0f32, 0f32), Vec3::new(1f32, 0f32, 0f32)], IVec3::new(0, 0, 1)), // front
        (Face::Back, [Vec3::new(0f32, 1f32, -1f32), Vec3::new(1f32, 1f32, -1f32), Vec3::new(1f32, 0f32, -1f32), Vec3::new(0f32, 0f32, -1f32)], IVec3::new(0, 0, -1)), // back
        (Face::Left, [Vec3::new(0f32, 1f32, 0f32), Vec3::new(0f32, 1f32, -1f32), Vec3::new(0f32, 0f32, -1f32), Vec3::new(0f32, 0f32, 0f32)], IVec3::new(-1, 0, 0)), // left
        (Face::Right, [Vec3::new(1f32, 1f32, -1f32), Vec3::new(1f32, 1f32, 0f32), Vec3::new(1f32, 0f32, 0f32), Vec3::new(1f32, 0f32, -1f32)], IVec3::new(1, 0, 0)), // right
        (Face::Top, [Vec3::new(1f32, 1f32, -1f32), Vec3::new(0f32, 1f32, -1f32), Vec3::new(0f32, 1f32, 0f32), Vec3::new(1f32, 1f32, 0f32)], IVec3::new(0, 1, 0)), // top
        (Face::Bottom, [Vec3::new(0f32, 0f32, -1f32), Vec3::new(1f32, 0f32, -1f32), Vec3::new(1f32, 0f32, 0f32), Vec3::new(0f32, 0f32, 0f32)], IVec3::new(0, -1, 0)), // bottom
    ];

    static ref UVS: [Vec2; 4] = [
        Vec2::new(0.25, 0.0),
        Vec2::new(0.0, 0.0),
        Vec2::new(0.0, 0.25),
        Vec2::new(0.25, 0.25),
    ];
}

impl ChunkData {
    fn generate_mesh_data(&self, world: &ChunkWorld, chunk: &Chunk) -> ChunkMeshData {
        let mut vertices = Vec::new();
        let mut vertex_index = 0;
        let mut indices = Vec::new();
        let mut normals = Vec::new();
        let mut uvs = Vec::new();
        let mut data = Vec::new();

        for (local_coord, voxel) in self.voxels.iter() {
            if voxel.block == Block::Air {
                continue;
            }

            let global_coord = *local_coord + chunk.coord.extend(0).xzy() * CHUNK_WIDTH as i32;

            for (face, verts, normal) in FACE_DATA.iter() {
                let local_neighbord_coord = *local_coord + *normal;
                let neighbor = self
                    .voxels
                    .get(&local_neighbord_coord)
                    .map(|neighbor_voxel| neighbor_voxel.block)
                    .unwrap_or_else(|| world.get_block(&(global_coord + *normal)));

                if neighbor.is_solid() {
                    continue;
                }

                vertices.extend_from_slice(&verts.map(|v| v + local_coord.as_vec3()));

                indices.extend_from_slice(&[
                    vertex_index + 0,
                    vertex_index + 1,
                    vertex_index + 2,
                    vertex_index + 3,
                    vertex_index + 0,
                    vertex_index + 2,
                ]);

                normals.push(normal.as_vec3().into());

                uvs.extend_from_slice(&voxel.block.uvs(face));

                let x = (voxel.light as u32) << (8 * 3);
                data.extend_from_slice(&[x; 4]);

                vertex_index += 4;
            }
        }

        ChunkMeshData {
            vertices,
            indices,
            normals,
            uvs,
            data,
        }
    }
}

#[derive(Debug, Clone, TypeUuid)]
#[uuid = "3bf9e361-f29d-4d6c-92cf-93298466c620"]
pub struct ChunkMaterial {
    color: Color,
    albedo_texture: Handle<Image>,
}

const ATTRIBUTE_DATA: MeshVertexAttribute =
    MeshVertexAttribute::new("Data", 988540917, VertexFormat::Uint32);

#[derive(Clone)]
pub struct GpuChunkMaterial {
    pub _buffer: Buffer,
    pub bind_group: BindGroup,
    pub albedo_texture: Handle<Image>,
}

impl RenderAsset for ChunkMaterial {
    type ExtractedAsset = ChunkMaterial;

    type PreparedAsset = GpuChunkMaterial;

    type Param = (
        SRes<RenderDevice>,
        SRes<MaterialPipeline<Self>>,
        SRes<RenderAssets<Image>>,
    );

    fn extract_asset(&self) -> Self::ExtractedAsset {
        self.clone()
    }

    fn prepare_asset(
        extracted_asset: Self::ExtractedAsset,
        (render_device, material_pipeline, gpu_images): &mut SystemParamItem<Self::Param>,
    ) -> Result<
        Self::PreparedAsset,
        bevy::render::render_asset::PrepareAssetError<Self::ExtractedAsset>,
    > {
        let (albedo_texture_view, albedo_sampler) = if let Some(result) = material_pipeline
            .mesh_pipeline
            .get_image_texture(gpu_images, &Some(extracted_asset.albedo_texture.clone()))
        {
            result
        } else {
            return Err(PrepareAssetError::RetryNextUpdate(extracted_asset));
        };

        let color = Vec4::from_slice(&extracted_asset.color.as_linear_rgba_f32());
        let buffer = render_device.create_buffer_with_data(&BufferInitDescriptor {
            label: None,
            contents: color.as_std140().as_bytes(),
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        });
        let bind_group = render_device.create_bind_group(&BindGroupDescriptor {
            label: None,
            layout: &material_pipeline.material_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: buffer.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: BindingResource::TextureView(albedo_texture_view),
                },
                BindGroupEntry {
                    binding: 2,
                    resource: BindingResource::Sampler(albedo_sampler),
                },
            ],
        });

        Ok(GpuChunkMaterial {
            _buffer: buffer,
            bind_group,
            albedo_texture: extracted_asset.albedo_texture,
        })
    }
}

impl Material for ChunkMaterial {
    fn bind_group(
        material: &<Self as bevy::render::render_asset::RenderAsset>::PreparedAsset,
    ) -> &BindGroup {
        &material.bind_group
    }

    fn bind_group_layout(
        render_device: &RenderDevice,
    ) -> bevy::render::render_resource::BindGroupLayout {
        render_device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: None,
            entries: &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: BufferSize::new(Vec4::std140_size_static() as u64),
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: false },
                        view_dimension: TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 2,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Sampler(SamplerBindingType::NonFiltering),
                    count: None,
                },
            ],
        })
    }

    fn vertex_shader(asset_server: &AssetServer) -> Option<Handle<Shader>> {
        Some(asset_server.load("shaders/chunk.wgsl"))
    }

    fn fragment_shader(asset_server: &AssetServer) -> Option<Handle<Shader>> {
        Some(asset_server.load("shaders/chunk.wgsl"))
    }

    fn specialize(
        _pipeline: &MaterialPipeline<Self>,
        descriptor: &mut RenderPipelineDescriptor,
        layout: &Hashed<InnerMeshVertexBufferLayout>,
    ) -> Result<(), SpecializedMeshPipelineError> {
        let vertex_layout = layout.get_layout(&[
            Mesh::ATTRIBUTE_POSITION.at_shader_location(0),
            Mesh::ATTRIBUTE_UV_0.at_shader_location(1),
            ATTRIBUTE_DATA.at_shader_location(2),
        ])?;
        descriptor.vertex.buffers = vec![vertex_layout];
        Ok(())
    }
}

pub struct ChunkPlugin;

impl Plugin for ChunkPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugin(MaterialPlugin::<ChunkMaterial>::default())
            .insert_resource(ChunkWorld::default())
            .add_startup_system(chunk_startup)
            .add_startup_system(diagnostic_setup)
            .add_system(spawn_chunk)
            .add_system(generate_chunk_data)
            .add_system(generate_chunk_mesh);
    }
}

fn diagnostic_setup(mut diagnostics: ResMut<Diagnostics>) {
    diagnostics.add(Diagnostic::new(
        CHUNK_DATA_GENERATION_TIME,
        "chunk_data_generation_time",
        32,
    ));

    diagnostics.add(Diagnostic::new(
        CHUNK_MESH_GENERATION_TIME,
        "chunk_mesh_generation_time",
        32,
    ));
}

fn chunk_startup(
    mut commands: Commands,
    mut materials: ResMut<Assets<ChunkMaterial>>,
    asset_server: ResMut<AssetServer>,
) {
    let material = materials.add(ChunkMaterial {
        albedo_texture: asset_server.load("textures/blocks.png"),
        color: Color::ORANGE_RED,
    });

    commands.insert_resource(material);

    let mut coords = HashSet::new();
    for x in 0..4 {
        for y in 0..4 {
            coords.insert(IVec2::new(x, y));
        }
    }

    for coord in coords {
        commands.spawn().insert(ChunkToSpawn(coord));
    }
}

fn spawn_chunk(mut commands: Commands, query: Query<(Entity, &ChunkToSpawn)>) {
    for (entity, ChunkToSpawn(coord)) in query.iter() {
        let tf = Transform {
            translation: Vec3::new(
                (coord.x * CHUNK_WIDTH as i32) as f32,
                0.0,
                (coord.y * CHUNK_WIDTH as i32) as f32,
            ),
            ..Default::default()
        };
        commands
            .spawn()
            .insert(Chunk {
                coord: coord.clone(),
                biome: Biome::default(),
            })
            .insert(tf);
        commands.entity(entity).despawn();
    }
}

fn generate_chunk_data(
    mut commands: Commands,
    query: Query<(Entity, &Chunk), Without<ChunkData>>,
    world: Res<ChunkWorld>,
    mut diagnostics: ResMut<Diagnostics>,
) {
    for (entity, chunk) in query.iter() {
        let time_start = Instant::now();

        let mut voxels = HashMap::new();

        for z in 0..(CHUNK_WIDTH as i32) {
            for x in 0..(CHUNK_WIDTH as i32) {
                let height = world.get_height(
                    &chunk.biome,
                    &(IVec2::new(x, z) + chunk.coord * CHUNK_WIDTH as i32),
                );

                for y in 0..(CHUNK_HEIGHT as i32) {
                    let coord = IVec3::new(x, y, z);
                    let global_coord = chunk.coord.extend(0).xzy() * CHUNK_WIDTH as i32 + coord;
                    let block = world.get_block_with_height(&global_coord, height);
                    let voxel = Voxel { block, light: 0xF0 };
                    voxels.insert(coord, voxel);
                }
            }
        }

        let chunk_data = ChunkData { voxels };
        commands.entity(entity).insert(chunk_data);

        let end = Instant::now().duration_since(time_start);
        diagnostics.add_measurement(CHUNK_DATA_GENERATION_TIME, end.as_secs_f64());
    }
}

fn generate_chunk_mesh(
    mut commands: Commands,
    query: Query<(Entity, &Chunk, &ChunkData, &Transform), Without<Handle<Mesh>>>,
    mut meshes: ResMut<Assets<Mesh>>,
    material: Res<Handle<ChunkMaterial>>,
    world: Res<ChunkWorld>,
    mut diagnostics: ResMut<Diagnostics>,
) {
    for (entity, chunk, chunk_data, transform) in query.iter() {
        let time_start = Instant::now();

        let data = chunk_data.generate_mesh_data(&world, &chunk);

        let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);

        mesh.insert_attribute(
            Mesh::ATTRIBUTE_POSITION,
            VertexAttributeValues::Float32x3(
                data.vertices.iter().map(|v| [v.x, v.y, v.z]).collect(),
            ),
        );

        mesh.insert_attribute(
            Mesh::ATTRIBUTE_NORMAL,
            vec![[0.0, 0.0, 0.0]; data.vertices.len()],
        );

        mesh.insert_attribute(
            Mesh::ATTRIBUTE_UV_0,
            VertexAttributeValues::Float32x2(data.uvs),
        );

        mesh.insert_attribute(ATTRIBUTE_DATA, VertexAttributeValues::Uint32(data.data));

        mesh.set_indices(Some(Indices::U32(data.indices)));

        let mesh = meshes.add(mesh);

        commands
            .entity(entity)
            // .insert(material.clone())
            .insert_bundle(MaterialMeshBundle {
                mesh,
                material: material.clone(),
                transform: transform.clone(),
                ..Default::default()
            })
            .insert(GenerateLightmap);

        let end = Instant::now().duration_since(time_start);
        diagnostics.add_measurement(CHUNK_MESH_GENERATION_TIME, end.as_secs_f64());
    }
}
