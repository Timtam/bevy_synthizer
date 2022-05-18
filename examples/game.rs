use std::f32;

use bevy::{asset::LoadState, prelude::*};
use bevy_synthizer::*;

#[derive(Component, Deref, DerefMut)]
struct RotationTimer(Timer);

impl Default for RotationTimer {
    fn default() -> Self {
        Self(Timer::from_seconds(30., true))
    }
}

#[derive(Default)]
struct AssetHandles {
    sounds: Vec<HandleUntyped>,
    loaded: bool,
}

fn setup(asset_server: Res<AssetServer>, mut handles: ResMut<AssetHandles>) {
    handles.sounds = asset_server.load_folder(".").expect("Failed to load sfx");
}

fn load_and_create(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut handles: ResMut<AssetHandles>,
) {
    if handles.loaded {
        return;
    }
    handles.loaded = asset_server
        .get_group_load_state(handles.sounds.iter().map(|handle| handle.id))
        == LoadState::Loaded;
    if handles.loaded {
        commands
            .spawn()
            .insert(Listener)
            .insert(Transform::default())
            .insert(RotationTimer::default());
        let handle = handles.sounds[0].clone();
        let buffer = asset_server.get_handle(handle);
        commands
            .spawn()
            .insert(Transform::from_translation(Vec3::new(45., 0., 0.)))
            .insert(Sound {
                buffer,
                looping: true,
                ..default()
            });
    }
}

fn rotate_listener(time: Res<Time>, mut query: Query<(&mut RotationTimer, &mut Transform)>) {
    for (mut timer, mut transform) in query.iter_mut() {
        timer.tick(time.delta());
        let angle = f32::consts::PI * 2. * timer.percent();
        transform.rotation = Quat::from_rotation_z(angle);
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .insert_resource(SynthizerConfig { hrtf: true })
        .add_plugin(SynthizerPlugin)
        .add_system(bevy::input::system::exit_on_esc_system)
        .init_resource::<AssetHandles>()
        .add_startup_system(setup)
        .add_system(load_and_create)
        .add_system(rotate_listener)
        .run();
}
