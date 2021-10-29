#version 450

layout(location = 0) in vec3 Vertex_Position;
layout(location = 1) in vec2 Vertex_Uv;
layout(location = 2) in uint Vertex_Data;

layout(location = 0) out vec3 v_WorldPosition;
layout(location = 1) out vec2 v_Uv;
layout(location = 2) out uint v_Data;

layout(set = 0, binding = 0) uniform CameraViewProj {
    mat4 ViewProj;
};

layout(set = 2, binding = 0) uniform Transform {
    mat4 Model;
};

void main() {
    vec4 world_position = Model * vec4(Vertex_Position, 1.0);
    v_WorldPosition = world_position.xyz;
    v_Uv = Vertex_Uv;
    v_Data = Vertex_Data;
    gl_Position = ViewProj * world_position;
}
