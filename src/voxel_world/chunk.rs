use std::collections::{HashMap, HashSet};

use super::{
    biome::Biome,
    voxel::{Block, Face, Voxel},
};
use crate::voxel_world::ChunkWorld;

use bevy::{
    diagnostic::{Diagnostic, DiagnosticId, Diagnostics},
    math::Vec3Swizzles,
    prelude::*,
    reflect::TypeUuid,
    render::{
        mesh::{Indices, VertexAttributeValues},
        pipeline::{PipelineDescriptor, PrimitiveTopology, RenderPipeline},
        render_graph::{base, AssetRenderResourcesNode, RenderGraph},
        renderer::RenderResources,
        shader::ShaderStages,
    },
    utils::Instant,
};

use lazy_static::lazy_static;

const CHUNK_DATA_GENERATION_TIME: DiagnosticId =
    DiagnosticId::from_u128(74345255989977209082044397486567331055);
const CHUNK_MESH_GENERATION_TIME: DiagnosticId =
    DiagnosticId::from_u128(248235940287512337153113599483964887382);
const CHUNK_LIGHTMAP_GENERATION_TIME: DiagnosticId =
    DiagnosticId::from_u128(263687617396918363737600904402094845525);

const CHUNK_WIDTH: usize = 16;
const CHUNK_HEIGHT: usize = 128;

pub struct GenerateLightmap;
pub struct ChunkToSpawn(IVec2);
pub struct Chunk {
    pub coord: IVec2,
    pub biome: Biome,
}

pub struct ChunkData {
    voxels: HashMap<IVec3, Voxel>,
}

impl Default for ChunkData {
    fn default() -> Self {
        Self {
            voxels: HashMap::new(),
            ..Default::default()
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

                vertices.extend_from_slice(&verts.map(|v| v + local_coord.as_f32()));

                indices.extend_from_slice(&[
                    vertex_index + 0,
                    vertex_index + 1,
                    vertex_index + 2,
                    vertex_index + 3,
                    vertex_index + 0,
                    vertex_index + 2,
                ]);

                normals.push(normal.as_f32().into());

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

#[derive(RenderResources, Default, TypeUuid)]
#[uuid = "3bf9e361-f29d-4d6c-92cf-93298466c620"]
pub struct ChunkMaterial {
    pub texture: Handle<Texture>,
}

impl ChunkMaterial {
    pub const ATTRIBUTE_DATA: &'static str = "Vertex_Data";
}

struct ChunkRenderPipelines(RenderPipelines);

pub struct ChunkPlugin;

impl Plugin for ChunkPlugin {
    fn build(&self, app: &mut AppBuilder) {
        app.add_asset::<ChunkMaterial>()
            .insert_resource(ChunkWorld::default())
            .add_startup_system(add_chunk_pipeline.system())
            .add_startup_system(chunk_startup.system())
            .add_startup_system(diagnostic_setup.system())
            .add_system(spawn_chunk.system())
            .add_system(generate_chunk_data.system())
            .add_system(generate_chunk_mesh.system())
            .add_system(generate_lightmap.system());
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

pub fn add_chunk_pipeline(
    mut commands: Commands,
    mut pipelines: ResMut<Assets<PipelineDescriptor>>,
    asset_server: ResMut<AssetServer>,
    mut materials: ResMut<Assets<ChunkMaterial>>,
    mut render_graph: ResMut<RenderGraph>,
) {
    let pipeline_descriptor = PipelineDescriptor::default_config(ShaderStages {
        vertex: asset_server.load::<Shader, _>("shaders/chunk.vert"),
        fragment: Some(asset_server.load::<Shader, _>("shaders/chunk.frag")),
    });
    let pipeline_handle = pipelines.add(pipeline_descriptor);

    let render_pipelines =
        RenderPipelines::from_pipelines(vec![RenderPipeline::new(pipeline_handle)]);

    commands.insert_resource(ChunkRenderPipelines(render_pipelines));

    render_graph.add_system_node(
        "chunk_material",
        AssetRenderResourcesNode::<ChunkMaterial>::new(true),
    );

    render_graph
        .add_node_edge("chunk_material", base::node::MAIN_PASS)
        .unwrap();

    let texture = asset_server.load("textures/blocks.png");
    let material = materials.add(ChunkMaterial { texture });
    commands.insert_resource(material);
}

fn chunk_startup(mut commands: Commands) {
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
    render_pipelines: Res<ChunkRenderPipelines>,
    world: Res<ChunkWorld>,
    mut diagnostics: ResMut<Diagnostics>,
) {
    for (entity, chunk, chunk_data, transform) in query.iter() {
        let time_start = Instant::now();

        let data = chunk_data.generate_mesh_data(&world, &chunk);

        let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);

        mesh.set_attribute(
            Mesh::ATTRIBUTE_POSITION,
            VertexAttributeValues::Float3(data.vertices.iter().map(|v| [v.x, v.y, v.z]).collect()),
        );

        mesh.set_attribute(
            Mesh::ATTRIBUTE_NORMAL,
            vec![[0.0, 0.0, 0.0]; data.vertices.len()],
        );

        mesh.set_attribute(
            Mesh::ATTRIBUTE_UV_0,
            VertexAttributeValues::Float2(data.uvs),
        );

        mesh.set_attribute(
            ChunkMaterial::ATTRIBUTE_DATA,
            VertexAttributeValues::Uint(data.data),
        );

        mesh.set_indices(Some(Indices::U32(data.indices)));

        let mesh = meshes.add(mesh);

        commands
            .entity(entity)
            .insert(material.clone())
            .insert_bundle(MeshBundle {
                mesh,
                render_pipelines: render_pipelines.0.clone(),
                transform: transform.clone(),
                ..Default::default()
            })
            .insert(GenerateLightmap);

        let end = Instant::now().duration_since(time_start);
        diagnostics.add_measurement(CHUNK_MESH_GENERATION_TIME, end.as_secs_f64());
    }
}

fn generate_lightmap(
    mut commands: Commands,
    query: Query<(Entity, &ChunkData, &Handle<Mesh>), (With<GenerateLightmap>)>,
    mut diagnostics: ResMut<Diagnostics>,
) {
    for (entity, chunk_data, mesh) in query.iter() {
        let time_start = Instant::now();

        let end = Instant::now().duration_since(time_start);
        diagnostics.add_measurement(CHUNK_LIGHTMAP_GENERATION_TIME, end.as_secs_f64());
    }
}
