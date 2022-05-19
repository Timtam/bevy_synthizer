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
pub struct AngularPan {
    pub azimuth: f64,
    pub elevation: f64,
}

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct ScalarPan(pub f64);

impl ScalarPan {
    pub fn left() -> Self {
        Self(-1.)
    }

    pub fn right() -> Self {
        Self(1.)
    }
}

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
        Option<&AngularPan>,
        Option<&ScalarPan>,
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
        angular_pan,
        scalar_pan,
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
        if sound.generator.is_none() {
            let generator =
                syz::BufferGenerator::new(&context).expect("Failed to create generator");
            sound.generator = Some(generator);
        }
        let translation = global_transform
            .map(|v| v.translation)
            .or_else(|| transform.map(|v| v.translation));
        if sound.source.is_none() {
            if let Some(b) = buffers.get(sound.buffer.clone()) {
                if let Some(generator) = sound.generator.as_mut() {
                    generator.buffer().set(&**b).expect("Unable to set buffer");
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
                            .add_generator(generator)
                            .expect("Unable to add generator");
                        sound.source = Some(source.into());
                    } else {
                        let source =
                            syz::DirectSource::new(&context).expect("Failed to create source");
                        source
                            .add_generator(generator)
                            .expect("Failed to add generator");
                        sound.source = Some(source.into());
                    }
                }
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
            let mut clear_source = false;
            if let Some(translation) = translation {
                if let Some(source) = source.cast_to::<syz::Source3D>().unwrap() {
                    source
                        .position()
                        .set((
                            translation.x as f64,
                            translation.y as f64,
                            translation.z as f64,
                        ))
                        .expect("Failed to set position");
                    let distance_model = distance_model
                        .cloned()
                        .map(|v| *v)
                        .unwrap_or_else(|| context.default_distance_modle().get().unwrap());
                    source
                        .distance_model()
                        .set(distance_model)
                        .expect("Failed to set distance_model");
                    let distance_ref = distance_ref
                        .map(|v| **v)
                        .unwrap_or_else(|| context.default_distance_ref().get().unwrap());
                    source
                        .distance_ref()
                        .set(distance_ref)
                        .expect("Failed to set distance_ref");
                    let distance_max = distance_max
                        .map(|v| **v)
                        .unwrap_or_else(|| context.default_distance_max().get().unwrap());
                    source
                        .distance_max()
                        .set(distance_max)
                        .expect("Failed to set distance_max");
                    let rolloff = rolloff
                        .map(|v| **v)
                        .unwrap_or_else(|| context.default_rolloff().get().unwrap());
                    source
                        .rolloff()
                        .set(rolloff)
                        .expect("Failed to set rolloff");
                    let closeness_boost = closeness_boost
                        .map(|v| **v)
                        .unwrap_or_else(|| context.default_closeness_boost().get().unwrap());
                    source
                        .closeness_boost()
                        .set(closeness_boost)
                        .expect("Failed to set closeness_boost");
                    let closeness_boost_distance =
                        closeness_boost_distance.map(|v| **v).unwrap_or_else(|| {
                            context.default_closeness_boost_distance().get().unwrap()
                        });
                    source
                        .closeness_boost_distance()
                        .set(closeness_boost_distance)
                        .expect("Failed to set closeness_boost_distance");
                } else {
                    clear_source = true;
                }
            } else if let Some(angular_pan) = angular_pan {
                if let Some(source) = source.cast_to::<syz::AngularPannedSource>().unwrap() {
                    source
                        .azimuth()
                        .set(angular_pan.azimuth)
                        .expect("Failed to set azimuth");
                    source
                        .elevation()
                        .set(angular_pan.elevation)
                        .expect("Failed to set elevation");
                } else {
                    clear_source = true;
                }
            } else if let Some(scalar_pan) = scalar_pan {
                if let Some(source) = source.cast_to::<syz::ScalarPannedSource>().unwrap() {
                    source
                        .panning_scalar()
                        .set(**scalar_pan)
                        .expect("Failed to set scalar panning");
                } else {
                    clear_source = true;
                }
            }
            if clear_source {
                sound.source = None;
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

#[derive(Clone, Copy, Debug)]
pub struct SynthizerConfig {
    pub default_panner_strategy: syz::PannerStrategy,
}

impl Default for SynthizerConfig {
    fn default() -> Self {
        Self {
            default_panner_strategy: syz::PannerStrategy::Stereo,
        }
    }
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
        context
            .default_panner_strategy()
            .set(config.default_panner_strategy)
            .expect("Failed to set panner strategy");
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
