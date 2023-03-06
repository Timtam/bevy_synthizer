use std::f32;

use bevy::{asset::LoadState, prelude::*};
use bevy_synthizer::*;

#[derive(Component, Deref, DerefMut)]
struct RotationTimer(Timer);

impl Default for RotationTimer {
    fn default() -> Self {
        Self(Timer::from_seconds(30., TimerMode::Repeating))
    }
}

#[derive(Resource, Default)]
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
        commands.spawn((
            TransformBundle::default(),
            Listener,
            RotationTimer::default(),
        ));
        let handle = handles.sounds[0].clone();
        let buffer = asset_server.get_handle(handle);
        commands.spawn((
            TransformBundle::from(Transform::from_translation(Vec3::new(10., 0., 0.))),
            Source::default(),
            Sound {
                audio: buffer.into(),
                looping: true,
                ..default()
            },
        ));
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
        .add_plugin(SynthizerPlugin {
            default_panner_strategy: Some(bevy_synthizer::syz::PannerStrategy::Hrtf),
            default_distance_model: Some(bevy_synthizer::syz::DistanceModel::Inverse),
            ..default()
        })
        .add_system(bevy::window::close_on_esc)
        .init_resource::<AssetHandles>()
        .add_startup_system(setup)
        .add_system(load_and_create)
        .add_system(rotate_listener)
        .run();
}
