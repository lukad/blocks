#import bevy_pbr::mesh_view_bind_group
#import bevy_pbr::mesh_struct

struct Vertex {
    [[location(0)]] position: vec3<f32>;
    [[location(1)]] uv: vec2<f32>;
    [[location(2)]] data: u32;
};

struct ChunkMaterial {
    color: vec4<f32>;
};

[[group(2), binding(0)]]
var<uniform> mesh: Mesh;

struct VertexOutput {
    [[builtin(position)]] clip_position: vec4<f32>;
    [[location(0)]] uv: vec2<f32>;
    [[location(1)]] data: u32;
};

[[stage(vertex)]]
fn vertex(vertex: Vertex) -> VertexOutput {
    let world_position = mesh.model * vec4<f32>(vertex.position, 1.0);

    var out: VertexOutput;
    out.clip_position = view.view_proj * world_position;
    out.uv = vertex.uv;
    out.data = vertex.data;
    return out;
}

[[group(1), binding(0)]]
var<uniform> material: ChunkMaterial;

[[group(1), binding(1)]]
var albedo_texture: texture_2d<f32>;
[[group(1), binding(2)]]
var albedo_sampler: sampler;


struct FragmentInput {
    [[location(0)]] uv: vec2<f32>;
    [[location(1)]] data: u32;
};

[[stage(fragment)]]
fn fragment(in: FragmentInput) -> [[location(0)]] vec4<f32> {
    return textureSample(albedo_texture, albedo_sampler, in.uv);
}
