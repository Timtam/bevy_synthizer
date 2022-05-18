#![allow(clippy::type_complexity)]
use std::collections::HashMap;

use bevy::{
    asset::{AssetLoader, LoadContext, LoadedAsset},
    prelude::*,
    reflect::TypeUuid,
    transform::TransformSystem,
    utils::BoxedFuture,
};
pub use synthizer as syz;

#[derive(Clone, Deref, DerefMut, PartialEq, TypeUuid)]
#[uuid = "6b6b533a-bb1f-11ec-bda2-00155d8fdde9"]
pub struct Buffer(syz::Buffer);

#[derive(Clone, Copy, Debug, Default)]
struct BufferAssetLoader;

impl AssetLoader for BufferAssetLoader {
    fn load<'a>(
        &'a self,
        bytes: &'a [u8],
        load_context: &'a mut LoadContext,
    ) -> BoxedFuture<'a, Result<(), anyhow::Error>> {
        Box::pin(async move {
            let buffer: Option<Buffer> =
                match load_context.path().extension().unwrap().to_str().unwrap() {
                    "flac" | "mp3" | "wav" => {
                        syz::Buffer::from_encoded_data(bytes).map(Buffer).ok()
                    }
                    _ => None,
                };
            if let Some(buffer) = buffer {
                load_context.set_default_asset(LoadedAsset::new(buffer));
            }
            Ok(())
        })
    }

    fn extensions(&self) -> &[&str] {
        &["flac", "mp3", "wav"]
    }
}

#[derive(Component, Clone, Debug, Reflect)]
#[reflect(Component)]
pub struct Sound {
    pub buffer: Handle<Buffer>,
    pub gain: f64,
    pub pitch: f64,
    pub looping: bool,
    pub paused: bool,
    pub restart: bool,
    #[reflect(ignore)]
    pub source: Option<syz::Source>,
    #[reflect(ignore)]
    pub generator: Option<syz::BufferGenerator>,
}

impl Default for Sound {
    fn default() -> Self {
        Self {
            buffer: Default::default(),
            gain: 1.,
            pitch: 1.,
            looping: false,
            paused: false,
            restart: false,
            source: None,
            generator: None,
        }
    }
}

#[derive(Component, Clone, Copy, Debug, Deref, DerefMut)]
pub struct DistanceModel(syz::DistanceModel);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct DistanceRef(f64);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct DistanceMax(f64);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct Rolloff(f64);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct ClosenessBoost(f64);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct ClosenessBoostDistance(f64);

#[derive(Component, Clone, Copy, Debug, Default, Reflect)]
#[reflect(Component)]
pub struct Listener;

pub fn update_listener(
    context: ResMut<syz::Context>,
    listener: Query<(Option<&Transform>, Option<&GlobalTransform>), With<Listener>>,
) {
    if let Ok((transform, global_transform)) = listener.get_single() {
        let transform: Option<Transform> = global_transform
            .map(|v| {
                let transform: Transform = (*v).into();
                transform
            })
            .or_else(|| transform.cloned());
        if let Some(transform) = transform {
            let look = transform.local_x();
            let up = transform.local_z();
            if let Err(e) = context.position().set((
                transform.translation.x as f64,
                transform.translation.y as f64,
                transform.translation.z as f64,
            )) {
                error!("Error setting listener position: {:?}", e);
            }
            if let Err(e) = context.orientation().set((
                look.x as f64,
                look.y as f64,
                look.z as f64,
                up.x as f64,
                up.y as f64,
                up.z as f64,
            )) {
                error!("Error setting listener orientation: {:?}", e);
            }
        } else {
            context.position().set((0., 0., 0.)).ok();
            context.orientation().set((0., 0., 1., 0., 1., 0.)).ok();
        }
    } else {
        context.position().set((0., 0., 0.)).ok();
        context.orientation().set((0., 0., 1., 0., 1., 0.)).ok();
    }
}

#[derive(Default, Deref, DerefMut)]
struct LastBuffer(HashMap<Entity, Handle<Buffer>>);

fn swap_buffers(mut last_buffer: ResMut<LastBuffer>, mut query: Query<(Entity, &mut Sound)>) {
    for (entity, mut sound) in query.iter_mut() {
        let buffer = sound.buffer.clone();
        if let Some(l) = last_buffer.get(&entity) {
            if buffer != *l {
                sound.source = None;
                sound.generator = None;
            }
        }
        last_buffer.insert(entity, buffer);
    }
}

pub fn update_sound_properties(
    context: Res<syz::Context>,
    buffers: Res<Assets<Buffer>>,
    mut query: Query<(
        &mut Sound,
        Option<&DistanceModel>,
        Option<&DistanceRef>,
        Option<&DistanceMax>,
        Option<&Rolloff>,
        Option<&ClosenessBoost>,
        Option<&ClosenessBoostDistance>,
        Option<&Transform>,
        Option<&GlobalTransform>,
    )>,
) {
    for (
        mut sound,
        distance_model,
        distance_ref,
        distance_max,
        rolloff,
        closeness_boost,
        closeness_boost_distance,
        transform,
        global_transform,
    ) in query.iter_mut()
    {
        let Sound {
            gain,
            pitch,
            looping,
            ..
        } = *sound;
        if sound.restart {
            if let Some(generator) = sound.generator.as_mut() {
                generator
                    .playback_position()
                    .set(0.)
                    .expect("Failed to restart");
            }
            sound.restart = false;
        }
        if let Some(b) = buffers.get(sound.buffer.clone()) {
            if sound.source.is_none() {
                let generator =
                    syz::BufferGenerator::new(&context).expect("Failed to create generator");
                generator.buffer().set(&**b).expect("Unable to set buffer");
                let translation = global_transform
                    .map(|v| v.translation)
                    .or_else(|| transform.map(|v| v.translation));
                if let Some(translation) = translation {
                    let source = syz::Source3D::new(
                        &context,
                        syz::PannerStrategy::Delegate,
                        (
                            translation.x as f64,
                            translation.y as f64,
                            translation.z as f64,
                        ),
                    )
                    .expect("Failed to create source");
                    source
                        .add_generator(&generator)
                        .expect("Unable to add generator");
                    sound.source = Some(source.into());
                } else {
                    let source = syz::DirectSource::new(&context).expect("Failed to create source");
                    source
                        .add_generator(&generator)
                        .expect("Failed to add generator");
                    sound.source = Some(source.into());
                }
                sound.generator = Some(generator);
            }
        }
        if let Some(generator) = sound.generator.as_mut() {
            generator
                .pitch_bend()
                .set(pitch)
                .expect("Failed to set pitch");
            generator
                .looping()
                .set(looping)
                .expect("Failed to set looping");
        }
        if let Some(source) = sound.source.as_mut() {
            source.gain().set(gain).expect("Failed to set gain");
            let translation = global_transform
                .map(|v| v.translation)
                .or_else(|| transform.map(|v| v.translation));
            if let Some(translation) = translation {
                let source = source.cast_to::<syz::Source3D>().unwrap().unwrap();
                source
                    .position()
                    .set((
                        translation.x as f64,
                        translation.y as f64,
                        translation.z as f64,
                    ))
                    .expect("Failed to set position");
                if let Some(distance_model) = distance_model {
                    source
                        .distance_model()
                        .set(**distance_model)
                        .expect("Failed to set distance_model");
                }
                if let Some(distance_ref) = distance_ref {
                    source
                        .distance_ref()
                        .set(**distance_ref)
                        .expect("Failed to set distance_ref");
                }
                if let Some(distance_max) = distance_max {
                    source
                        .distance_max()
                        .set(**distance_max)
                        .expect("Failed to set distance_max");
                }
                if let Some(rolloff) = rolloff {
                    source
                        .rolloff()
                        .set(**rolloff)
                        .expect("Failed to set rolloff");
                }
                if let Some(closeness_boost) = closeness_boost {
                    source
                        .closeness_boost()
                        .set(**closeness_boost)
                        .expect("Failed to set closeness_boost");
                }
                if let Some(closeness_boost_distance) = closeness_boost_distance {
                    source
                        .closeness_boost_distance()
                        .set(**closeness_boost_distance)
                        .expect("Failed to set closeness_boost_distance");
                }
            }
        }
    }
}

pub fn update_playback_state(query: Query<&Sound>) {
    for sound in query.iter() {
        if let Some(generator) = &sound.generator {
            if sound.paused {
                generator.pause().expect("Failed to pause");
            } else {
                generator.play().expect("Failed to play");
            }
        }
    }
}

fn remove_sound(mut last_buffer: ResMut<LastBuffer>, removed: RemovedComponents<Sound>) {
    for entity in removed.iter() {
        last_buffer.remove(&entity);
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct SynthizerConfig {
    pub hrtf: bool,
}

pub struct SynthizerPlugin;

impl Plugin for SynthizerPlugin {
    fn build(&self, app: &mut App) {
        let guard = syz::initialize().expect("Failed to initialize Synthizer");
        let context = syz::Context::new().expect("Failed to create Synthizer context");
        if !app.world.contains_resource::<SynthizerConfig>() {
            app.insert_resource(SynthizerConfig::default());
        }
        let config = *app.world.get_resource::<SynthizerConfig>().unwrap();
        if config.hrtf {
            context
                .default_panner_strategy()
                .set(syz::PannerStrategy::Hrtf)
                .expect("Failed to set panner strategy");
        } else {
            context
                .default_panner_strategy()
                .set(syz::PannerStrategy::Stereo)
                .expect("Failed to set panner strategy");
        }
        app.add_asset::<Buffer>()
            .init_asset_loader::<BufferAssetLoader>()
            .register_type::<Listener>()
            .insert_resource(guard)
            .insert_resource(context)
            .init_resource::<LastBuffer>()
            .add_system_to_stage(
                CoreStage::PostUpdate,
                swap_buffers.before(update_sound_properties),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                update_listener
                    .after(TransformSystem::TransformPropagate)
                    .before(update_playback_state),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                update_sound_properties
                    .after(TransformSystem::TransformPropagate)
                    .before(update_playback_state),
            )
            .add_system_to_stage(CoreStage::PostUpdate, update_playback_state)
            .add_system_to_stage(CoreStage::PostUpdate, remove_sound);
    }
}
