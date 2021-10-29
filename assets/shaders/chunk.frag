#version 450

layout(location = 0) in vec3 v_WorldPosition;
layout(location = 1) in vec2 v_Uv;
layout(location = 2) flat in uint v_Data;

layout(location = 0) out vec4 o_Target;

layout(set = 0, binding = 0) uniform CameraViewProj {
    mat4 ViewProj;
};

layout(std140, set = 0, binding = 1) uniform CameraPosition {
    vec4 CameraPos;
};

layout(set = 1, binding = 0) uniform texture2D ChunkMaterial_texture;
layout(set = 1, binding = 1) uniform sampler ChunkMaterial_texture_sampler;

void main() {
    float shade = float(v_Data >> (8*3)) / 128.0;
    vec3 color = texture(sampler2D(ChunkMaterial_texture, ChunkMaterial_texture_sampler), v_Uv).rgb;
    o_Target = vec4(color * (1.0 * shade), 1.0);
}
