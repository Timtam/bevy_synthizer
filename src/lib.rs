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

#[derive(Clone, Debug, Deref, DerefMut, PartialEq, TypeUuid)]
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
pub struct Source {
    pub gain: f64,
    pub paused: bool,
    #[reflect(ignore)]
    pub handle: Option<syz::Source>,
}

impl Default for Source {
    fn default() -> Self {
        Self {
            gain: 1.,
            paused: false,
            handle: None,
        }
    }
}

#[derive(Component, Clone, Copy, Debug, Deref, DerefMut)]
// #[reflect(Component)]
pub struct PannerStrategy(pub syz::PannerStrategy);

impl Default for PannerStrategy {
    fn default() -> Self {
        Self(syz::PannerStrategy::Delegate)
    }
}

#[derive(Component, Clone, Copy, Debug, Deref, DerefMut)]
pub struct DistanceModel(pub syz::DistanceModel);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct DistanceRef(pub f64);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct DistanceMax(pub f64);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct Rolloff(pub f64);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct ClosenessBoost(pub f64);

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct ClosenessBoostDistance(pub f64);

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

    pub fn center() -> Self {
        Self(0.)
    }

    pub fn right() -> Self {
        Self(1.)
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
    pub generator: Option<syz::BufferGenerator>,
}

impl Default for Sound {
    fn default() -> Self {
        Self {
            buffer: default(),
            gain: 1.,
            pitch: 1.,
            looping: false,
            paused: false,
            restart: false,
            generator: None,
        }
    }
}

pub enum SynthizerEvent {
    Finished(Entity),
    Looped(Entity),
}

#[derive(Component, Clone, Copy, Debug, Default, Reflect)]
#[reflect(Component)]
pub struct Listener;

fn update_listener(
    context: ResMut<syz::Context>,
    listener: Query<Option<&GlobalTransform>, With<Listener>>,
) {
    if let Ok(transform) = listener.get_single() {
        let transform: Transform = transform
            .map(|v| {
                let transform: Transform = (*v).into();
                transform
            })
            .unwrap_or_default();
        let look = transform.local_x();
        let up = transform.local_z();
        context
            .position()
            .set((
                transform.translation.x as f64,
                transform.translation.y as f64,
                transform.translation.z as f64,
            ))
            .expect("Failed to set listener position");
        context
            .orientation()
            .set((
                look.x as f64,
                look.y as f64,
                look.z as f64,
                up.x as f64,
                up.y as f64,
                up.z as f64,
            ))
            .expect("Failed to set listener orientation");
    }
}

fn add_source_handle(
    context: Res<syz::Context>,
    mut query: Query<(
        &mut Source,
        Option<&PannerStrategy>,
        Option<&GlobalTransform>,
        Option<&AngularPan>,
        Option<&ScalarPan>,
    )>,
) {
    for (mut source, panner_strategy, transform, angular_pan, scalar_pan) in &mut query {
        if source.handle.is_none() {
            let panner_strategy = panner_strategy.cloned().unwrap_or_default();
            let handle: syz::Source = if let Some(transform) = transform {
                let translation = transform.translation();
                syz::Source3D::new(
                    &context,
                    *panner_strategy,
                    (
                        translation.x as f64,
                        translation.y as f64,
                        translation.z as f64,
                    ),
                )
                .expect("Failed to create source")
                .into()
            } else if let Some(scalar_pan) = scalar_pan {
                syz::ScalarPannedSource::new(&context, *panner_strategy, **scalar_pan)
                    .expect("Failed to create source")
                    .into()
            } else if let Some(angular_pan) = angular_pan {
                syz::AngularPannedSource::new(
                    &context,
                    *panner_strategy,
                    angular_pan.azimuth,
                    angular_pan.elevation,
                )
                .expect("Failed to create source")
                .into()
            } else {
                syz::DirectSource::new(&context)
                    .expect("Failed to create source")
                    .into()
            };
            source.handle = Some(handle);
        }
    }
}

fn add_generator(
    context: Res<syz::Context>,
    buffers: Res<Assets<Buffer>>,
    mut query: Query<(Entity, Option<&Parent>, &mut Sound)>,
    mut sources: Query<&mut Source>,
    parents: Query<&Parent>,
) {
    for (entity, parent, mut sound) in &mut query {
        if sound.generator.is_none() {
            if let Some(b) = buffers.get(&sound.buffer) {
                let mut source = if let Ok(s) = sources.get_mut(entity) {
                    Some(s)
                } else if let Some(parent) = parent {
                    let mut parent: Option<&Parent> = Some(parent);
                    let mut target = None;
                    while let Some(p) = parent {
                        if sources.get(**p).is_ok() {
                            target = Some(**p);
                            break;
                        }
                        parent = parents.get(**p).ok();
                    }
                    target.map(|v| sources.get_mut(v).unwrap())
                } else {
                    None
                };
                if let Some(source) = source.as_mut() {
                    if let Some(handle) = source.handle.as_mut() {
                        let generator = syz::BufferGenerator::new(&context)
                            .expect("Failed to create generator");
                        generator.buffer().set(&**b).expect("Unable to set buffer");
                        handle
                            .add_generator(&generator)
                            .expect("Unable to add generator");
                        sound.generator = Some(generator);
                    }
                }
            }
        }
    }
}

fn add_sound_without_source(
    mut commands: Commands,
    query: Query<
        (Entity, Option<&Parent>, Option<&GlobalTransform>),
        (Added<Sound>, Without<Source>),
    >,
    parents: Query<&Parent>,
    sources: Query<&Source>,
    transforms: Query<&GlobalTransform>,
) {
    for (entity, parent, transform) in &query {
        let should_check_for_transform = transform.is_none();
        let mut has_transform = false;
        let source = if let Some(parent) = parent {
            if should_check_for_transform {
                has_transform = transforms.get(**parent).is_ok();
            }
            let mut target = None;
            let mut parent: Option<&Parent> = Some(parent);
            while let Some(p) = parent {
                if should_check_for_transform && !has_transform {
                    has_transform = transforms.get(**p).is_ok();
                }
                if sources.get(**p).is_ok() {
                    target = Some(**p);
                    break;
                }
                parent = parents.get(**p).ok();
            }
            target.map(|v| sources.get(v).unwrap())
        } else {
            None
        };
        if source.is_none() {
            let id = commands.entity(entity).insert(Source::default()).id();
            if has_transform {
                commands
                    .entity(id)
                    .insert_bundle(TransformBundle::default());
            }
        }
    }
}

#[derive(Default, Deref, DerefMut)]
struct LastBuffer(HashMap<Entity, Handle<Buffer>>);

fn swap_buffers(
    mut last_buffer: ResMut<LastBuffer>,
    mut query: Query<(Entity, &mut Sound), Changed<Sound>>,
) {
    for (entity, mut sound) in &mut query {
        if let Some(l) = last_buffer.get(&entity) {
            if sound.buffer != *l {
                sound.generator = None;
            }
        }
        last_buffer.insert(entity, sound.buffer.clone());
    }
}

fn change_panner_strategy(
    changed: Query<Entity, Changed<PannerStrategy>>,
    removed: RemovedComponents<PannerStrategy>,
    mut sources: Query<&mut Source>,
) {
    let mut check = vec![];
    for entity in &changed {
        check.push(entity);
    }
    for entity in removed.iter() {
        check.push(entity);
    }
    for entity in check.iter() {
        if let Ok(mut source) = sources.get_mut(*entity) {
            if source.handle.is_some() {
                source.handle = None;
            }
        }
    }
}

fn update_source_properties(
    context: Res<syz::Context>,
    mut query: Query<(
        &mut Source,
        Option<&DistanceModel>,
        Option<&DistanceRef>,
        Option<&DistanceMax>,
        Option<&Rolloff>,
        Option<&ClosenessBoost>,
        Option<&ClosenessBoostDistance>,
        Option<&AngularPan>,
        Option<&ScalarPan>,
        Option<&GlobalTransform>,
    )>,
) {
    for (
        mut source,
        distance_model,
        distance_ref,
        distance_max,
        rolloff,
        closeness_boost,
        closeness_boost_distance,
        angular_pan,
        scalar_pan,
        transform,
    ) in &mut query
    {
        let Source { gain, .. } = *source;
        assert!(gain >= 0.);
        if let Some(handle) = source.handle.as_mut() {
            handle.gain().set(gain).expect("Failed to set gain");
            let mut clear_source = false;
            if let Some(transform) = transform {
                if let Some(source) = handle.cast_to::<syz::Source3D>().unwrap() {
                    let translation = transform.translation();
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
                        .unwrap_or_else(|| context.default_distance_model().get().unwrap());
                    source
                        .distance_model()
                        .set(distance_model)
                        .expect("Failed to set distance_model");
                    let distance_ref = distance_ref
                        .map(|v| **v)
                        .unwrap_or_else(|| context.default_distance_ref().get().unwrap());
                    assert!(distance_ref >= 0.);
                    source
                        .distance_ref()
                        .set(distance_ref)
                        .expect("Failed to set distance_ref");
                    let distance_max = distance_max
                        .map(|v| **v)
                        .unwrap_or_else(|| context.default_distance_max().get().unwrap());
                    assert!(distance_max >= 0.);
                    source
                        .distance_max()
                        .set(distance_max)
                        .expect("Failed to set distance_max");
                    let rolloff = rolloff
                        .map(|v| **v)
                        .unwrap_or_else(|| context.default_rolloff().get().unwrap());
                    assert!(rolloff >= 0.);
                    source
                        .rolloff()
                        .set(rolloff)
                        .expect("Failed to set rolloff");
                    let closeness_boost = closeness_boost
                        .map(|v| **v)
                        .unwrap_or_else(|| context.default_closeness_boost().get().unwrap());
                    assert!(closeness_boost >= 0.);
                    source
                        .closeness_boost()
                        .set(closeness_boost)
                        .expect("Failed to set closeness_boost");
                    let closeness_boost_distance =
                        closeness_boost_distance.map(|v| **v).unwrap_or_else(|| {
                            context.default_closeness_boost_distance().get().unwrap()
                        });
                    assert!(closeness_boost_distance >= 0.);
                    source
                        .closeness_boost_distance()
                        .set(closeness_boost_distance)
                        .expect("Failed to set closeness_boost_distance");
                } else {
                    clear_source = true;
                }
            } else if let Some(angular_pan) = angular_pan {
                if let Some(source) = handle.cast_to::<syz::AngularPannedSource>().unwrap() {
                    assert!(angular_pan.azimuth >= 0. && angular_pan.azimuth <= 360.);
                    source
                        .azimuth()
                        .set(angular_pan.azimuth)
                        .expect("Failed to set azimuth");
                    assert!(angular_pan.elevation >= -90. && angular_pan.elevation <= 90.);
                    source
                        .elevation()
                        .set(angular_pan.elevation)
                        .expect("Failed to set elevation");
                } else {
                    clear_source = true;
                }
            } else if let Some(scalar_pan) = scalar_pan {
                if let Some(source) = handle.cast_to::<syz::ScalarPannedSource>().unwrap() {
                    assert!(**scalar_pan >= -1. && **scalar_pan <= 1.);
                    source
                        .panning_scalar()
                        .set(**scalar_pan)
                        .expect("Failed to set scalar panning");
                } else {
                    clear_source = true;
                }
            }
            if clear_source {
                source.handle = None;
            }
        }
    }
}

fn update_sound_properties(mut query: Query<&mut Sound>) {
    for mut sound in &mut query {
        let Sound {
            gain,
            pitch,
            looping,
            ..
        } = *sound;
        assert!(gain >= 0.);
        assert!(pitch >= 0.);
        if sound.restart {
            if let Some(generator) = sound.generator.as_mut() {
                generator
                    .playback_position()
                    .set(0.)
                    .expect("Failed to restart");
            }
            sound.restart = false;
        }
        if let Some(generator) = sound.generator.as_mut() {
            generator.gain().set(gain).expect("Failed to set gain");
            generator
                .pitch_bend()
                .set(pitch)
                .expect("Failed to set pitch");
            generator
                .looping()
                .set(looping)
                .expect("Failed to set looping");
        }
    }
}

fn update_source_playback_state(query: Query<&Source>) {
    for source in &query {
        if let Some(handle) = &source.handle {
            if source.paused {
                handle.pause().expect("Failed to pause");
            } else {
                handle.play().expect("Failed to play");
            }
        }
    }
}

fn update_sound_playback_state(query: Query<&Sound>) {
    for sound in &query {
        if let Some(generator) = &sound.generator {
            if sound.paused {
                generator.pause().expect("Failed to pause");
            } else {
                generator.play().expect("Failed to play");
            }
        }
    }
}

fn remove_sound(mut last_buffer: ResMut<LastBuffer>, removed: RemovedComponents<Source>) {
    for entity in removed.iter() {
        last_buffer.remove(&entity);
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct SynthizerConfig {
    pub default_panner_strategy: Option<syz::PannerStrategy>,
    pub default_distance_model: Option<syz::DistanceModel>,
    pub default_distance_ref: Option<f64>,
    pub default_distance_max: Option<f64>,
    pub default_rolloff: Option<f64>,
    pub default_closeness_boost: Option<f64>,
    pub default_closeness_boost_distance: Option<f64>,
}

#[derive(Debug)]
pub struct SynthizerDefaults {
    pub panner_strategy: syz::PannerStrategy,
    pub distance_model: syz::DistanceModel,
    pub distance_ref: f64,
    pub distance_max: f64,
    pub rolloff: f64,
    pub closeness_boost: f64,
    pub closeness_boost_distance: f64,
}

fn sync_config(
    context: Res<syz::Context>,
    config: Res<SynthizerConfig>,
    defaults: Res<SynthizerDefaults>,
) {
    if config.is_changed() {
        context
            .default_panner_strategy()
            .set(
                config
                    .default_panner_strategy
                    .unwrap_or(defaults.panner_strategy),
            )
            .expect("Failed to set panner strategy");
        context
            .default_distance_model()
            .set(
                config
                    .default_distance_model
                    .unwrap_or(defaults.distance_model),
            )
            .expect("Failed to set distance model");
        context
            .default_distance_ref()
            .set(config.default_distance_ref.unwrap_or(defaults.distance_ref))
            .expect("Failed to set distance_ref");
        context
            .default_distance_max()
            .set(config.default_distance_max.unwrap_or(defaults.distance_max))
            .expect("Failed to set distance_max");
        context
            .default_rolloff()
            .set(config.default_rolloff.unwrap_or(defaults.rolloff))
            .expect("Failed to set rolloff");
        context
            .default_closeness_boost()
            .set(
                config
                    .default_closeness_boost
                    .unwrap_or(defaults.closeness_boost),
            )
            .expect("Failed to set closeness_boost");
        context
            .default_closeness_boost_distance()
            .set(
                config
                    .default_closeness_boost_distance
                    .unwrap_or(defaults.closeness_boost_distance),
            )
            .expect("Failed to set closeness_boost_distance");
    }
}

fn events(
    context: Res<syz::Context>,
    sounds: Query<(Entity, &Sound)>,
    mut output: EventWriter<SynthizerEvent>,
) {
    context.get_events().for_each(|event| {
        if let Ok(event) = event {
            for (entity, sound) in &sounds {
                if let Some(generator) = &sound.generator {
                    if *generator.handle() == event.source {
                        match event.r#type {
                            syz::EventType::Finished => {
                                output.send(SynthizerEvent::Finished(entity));
                            }
                            syz::EventType::Looped => {
                                output.send(SynthizerEvent::Looped(entity));
                            }
                            _ => {}
                        }
                        break;
                    }
                }
            }
        }
    });
}

#[derive(SystemLabel, Clone, Hash, Debug, PartialEq, Eq)]
pub enum SynthizerSystems {
    UpdateHandles,
    UpdateProperties,
    UpdateState,
}

pub struct SynthizerPlugin;

impl Plugin for SynthizerPlugin {
    fn build(&self, app: &mut App) {
        let guard = syz::initialize().expect("Failed to initialize Synthizer");
        let context = syz::Context::new().expect("Failed to create Synthizer context");
        if !app.world.contains_resource::<SynthizerConfig>() {
            app.insert_resource(SynthizerConfig::default());
        }
        let defaults = SynthizerDefaults {
            panner_strategy: context.default_panner_strategy().get().unwrap(),
            distance_model: context.default_distance_model().get().unwrap(),
            distance_ref: context.default_distance_ref().get().unwrap(),
            distance_max: context.default_distance_max().get().unwrap(),
            rolloff: context.default_rolloff().get().unwrap(),
            closeness_boost: context.default_closeness_boost().get().unwrap(),
            closeness_boost_distance: context.default_closeness_boost_distance().get().unwrap(),
        };
        context.enable_events().expect("Failed to enable events");
        app.add_asset::<Buffer>()
            .init_asset_loader::<BufferAssetLoader>()
            .register_type::<Listener>()
            .insert_resource(guard)
            .insert_resource(context)
            .init_resource::<LastBuffer>()
            .insert_resource(defaults)
            .add_event::<SynthizerEvent>()
            .add_system_to_stage(CoreStage::PreUpdate, sync_config)
            .add_system_to_stage(
                CoreStage::PostUpdate,
                swap_buffers.before(SynthizerSystems::UpdateHandles),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                change_panner_strategy.before(SynthizerSystems::UpdateHandles),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                add_source_handle
                    .label(SynthizerSystems::UpdateHandles)
                    .before(SynthizerSystems::UpdateProperties),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                add_generator
                    .label(SynthizerSystems::UpdateHandles)
                    .before(SynthizerSystems::UpdateProperties),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                add_sound_without_source
                    .label(SynthizerSystems::UpdateHandles)
                    .before(SynthizerSystems::UpdateProperties),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                update_listener
                    .label(SynthizerSystems::UpdateProperties)
                    .after(TransformSystem::TransformPropagate)
                    .before(SynthizerSystems::UpdateState),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                update_source_properties
                    .label(SynthizerSystems::UpdateProperties)
                    .after(TransformSystem::TransformPropagate)
                    .before(SynthizerSystems::UpdateState),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                update_sound_properties
                    .label(SynthizerSystems::UpdateProperties)
                    .before(SynthizerSystems::UpdateState),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                update_source_playback_state.label(SynthizerSystems::UpdateState),
            )
            .add_system_to_stage(
                CoreStage::PostUpdate,
                update_sound_playback_state.label(SynthizerSystems::UpdateState),
            )
            .add_system_to_stage(CoreStage::PostUpdate, remove_sound)
            .add_system_to_stage(CoreStage::PreUpdate, events);
    }
}
