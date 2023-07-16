use std::f32;

use bevy::prelude::*;
use bevy_synthizer::*;

#[derive(Component, Deref, DerefMut)]
struct RotationTimer(Timer);

impl Default for RotationTimer {
    fn default() -> Self {
        Self(Timer::from_seconds(30., TimerMode::Repeating))
    }
}

fn setup(mut commands: Commands, context: Res<Context>) {
    commands.spawn((
        TransformBundle::default(),
        Listener,
        RotationTimer::default(),
    ));
    let generator: syz::Generator = syz::FastSineBankGenerator::new_sine(&context, 440.)
        .expect("Failed to create generator")
        .into();
    commands.spawn((
        TransformBundle::from(Transform::from_translation(Vec3::new(10., 0., 0.))),
        Source::default(),
        Sound {
            audio: generator.into(),
            looping: true,
            ..default()
        },
    ));
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
        .add_plugins((
            DefaultPlugins,
            SynthizerPlugin {
                default_panner_strategy: Some(bevy_synthizer::syz::PannerStrategy::Hrtf),
                default_distance_model: Some(bevy_synthizer::syz::DistanceModel::Inverse),
                ..default()
            },
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, (bevy::window::close_on_esc, rotate_listener))
        .run();
}
