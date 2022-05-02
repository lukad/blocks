mod voxel_world;

use voxel_world::VoxelPlugin;

use bevy::{
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    prelude::*,
};

fn main() {
    App::new()
        .insert_resource(WindowDescriptor {
            title: "voxels".into(),
            ..Default::default()
        })
        .add_plugins(DefaultPlugins)
        .add_plugin(bevy_flycam::PlayerPlugin)
        .add_plugin(FrameTimeDiagnosticsPlugin::default())
        .add_plugin(LogDiagnosticsPlugin::default())
        .add_plugin(VoxelPlugin)
        .add_system(bevy::input::system::exit_on_esc_system)
        .run();
}
