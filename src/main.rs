mod voxel_world;

use voxel_world::VoxelPlugin;

use bevy::{
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    prelude::*,
    render::camera::PerspectiveProjection,
};

use bevy_fly_camera::{FlyCamera, FlyCameraPlugin};

fn setup(mut commands: Commands) {
    commands
        .spawn_bundle(PerspectiveCameraBundle {
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 10.0))
                .looking_at(Vec3::new(0.0, 0.0, 0.0), Vec3::Y),
            perspective_projection: PerspectiveProjection {
                near: 0.001,
                far: 1000.0,
                ..Default::default()
            },
            ..Default::default()
        })
        .insert(FlyCamera {
            sensitivity: 25.0,
            ..Default::default()
        });
}

fn cursor_grab_system(mut windows: ResMut<Windows>, btn: Res<Input<MouseButton>>) {
    let window = windows.get_primary_mut().unwrap();

    if btn.just_pressed(MouseButton::Left) {
        window.set_cursor_lock_mode(true);
        window.set_cursor_visibility(false);
    }
}

fn main() {
    App::build()
        .insert_resource(WindowDescriptor {
            title: "voxels".into(),
            ..Default::default()
        })
        .add_plugins(DefaultPlugins)
        .add_plugin(FrameTimeDiagnosticsPlugin::default())
        .add_plugin(LogDiagnosticsPlugin::default())
        .add_plugin(VoxelPlugin)
        .add_plugin(FlyCameraPlugin)
        .add_startup_system(setup.system())
        .add_system(bevy::input::system::exit_on_esc_system.system())
        .add_system(cursor_grab_system.system())
        .run();
}
