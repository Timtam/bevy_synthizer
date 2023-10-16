#![allow(clippy::type_complexity)]
use std::collections::HashMap;

use bevy::{
    asset::{AssetLoader, LoadContext, LoadedAsset},
    prelude::*,
    reflect::{TypePath, TypeUuid},
    transform::TransformSystem,
    utils::BoxedFuture,
};
pub use synthizer as syz;

#[derive(Clone, Debug, Deref, DerefMut, PartialEq, Eq, TypePath, TypeUuid)]
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

#[derive(Resource, Clone, Debug, Deref, DerefMut)]
pub struct Context(syz::Context);

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

#[derive(Component, Clone, Debug, PartialEq, Eq)]
pub enum Audio {
    Buffer(Handle<Buffer>),
    Generator(syz::Generator),
}

impl Default for Audio {
    fn default() -> Self {
        Self::Buffer(default())
    }
}

impl From<Handle<Buffer>> for Audio {
    fn from(value: Handle<Buffer>) -> Self {
        Audio::Buffer(value)
    }
}

impl From<syz::Generator> for Audio {
    fn from(value: syz::Generator) -> Self {
        Self::Generator(value)
    }
}

#[derive(Component, Clone, Debug)]
pub struct Sound {
    pub audio: Audio,
    pub gain: f64,
    pub pitch: f64,
    pub looping: bool,
    pub paused: bool,
    pub generator: Option<syz::Generator>,
}

impl Default for Sound {
    fn default() -> Self {
        Self {
            audio: default(),
            gain: 1.,
            pitch: 1.,
            looping: false,
            paused: false,
            generator: None,
        }
    }
}

#[derive(Event, Debug)]
pub enum SynthizerEvent {
    Finished(Entity),
    Looped(Entity),
}

#[derive(Component, Clone, Copy, Debug, Default, Reflect)]
#[reflect(Component)]
pub struct Listener;

fn update_listener(
    context: ResMut<Context>,
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
    context: Res<Context>,
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
    context: Res<Context>,
    buffers: Res<Assets<Buffer>>,
    mut query: Query<(Entity, Option<&Parent>, &mut Sound)>,
    mut sources: Query<&mut Source>,
    parents: Query<&Parent>,
) {
    for (entity, parent, mut sound) in &mut query {
        if sound.generator.is_none() {
            let mut source = if let Ok(s) = sources.get_mut(entity) {
                Some(s)
            } else if parent.is_some() {
                let mut parent = parent;
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
                    let generator: Option<syz::Generator> = match &sound.audio {
                        Audio::Buffer(buffer) => {
                            if let Some(b) = buffers.get(buffer) {
                                let generator = syz::BufferGenerator::new(&context)
                                    .expect("Failed to create generator");
                                generator.buffer().set(&**b).expect("Unable to set buffer");
                                Some(generator.into())
                            } else {
                                None
                            }
                        }
                        Audio::Generator(generator) => Some(generator.clone()),
                    };
                    if let Some(generator) = generator {
                        assert!(sound.gain >= 0.);
                        generator
                            .gain()
                            .set(sound.gain)
                            .expect("Failed to set gain");
                        assert!(sound.pitch > 0. && sound.pitch <= 2.);
                        generator
                            .pitch_bend()
                            .set(sound.pitch)
                            .expect("Failed to set pitch");
                        handle
                            .add_generator(generator.handle())
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
    query: Query<Entity, (Added<Sound>, Without<Source>)>,
    parents: Query<(&Parent, Option<&Source>)>,
) {
    for entity in &query {
        let mut has_source = false;
        let mut target = entity;
        while let Ok((parent, source)) = parents.get(target) {
            if source.is_some() {
                has_source = true;
                break;
            }
            target = **parent;
        }
        if !has_source {
            commands.entity(entity).insert(Source::default());
        }
    }
}

#[derive(Resource, Default, Deref, DerefMut)]
struct LastAudio(HashMap<Entity, Audio>);

fn swap_buffers(
    mut last_audio: ResMut<LastAudio>,
    mut query: Query<(Entity, &mut Sound), Changed<Sound>>,
) {
    for (entity, mut sound) in &mut query {
        if let Some(l) = last_audio.get(&entity) {
            if sound.generator.is_some() && sound.audio != *l {
                sound.generator = None;
            }
        }
        last_audio.insert(entity, sound.audio.clone());
    }
}

fn change_panner_strategy(
    changed: Query<(Entity, Ref<PannerStrategy>)>,
    mut removed: RemovedComponents<PannerStrategy>,
    mut sources: Query<&mut Source>,
) {
    let mut check = vec![];
    for (entity, change) in &changed {
        if !change.is_added() && change.is_changed() {
            check.push(entity);
        }
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
    context: Res<Context>,
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
                if let Some(source) = handle.cast_to::<syz::Source3D>().expect("Failed to cast") {
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
                if let Some(source) = handle
                    .cast_to::<syz::AngularPannedSource>()
                    .expect("Failed to cast")
                {
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
                if let Some(source) = handle
                    .cast_to::<syz::ScalarPannedSource>()
                    .expect("Failed to cast")
                {
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
        assert!(pitch > 0. && pitch <= 2.);
        if let Some(generator) = sound.generator.as_mut() {
            generator.gain().set(gain).expect("Failed to set gain");
            generator
                .pitch_bend()
                .set(pitch)
                .expect("Failed to set pitch");
            if let Some(generator) = generator
                .cast_to::<syz::BufferGenerator>()
                .expect("Failed to cast")
            {
                generator
                    .looping()
                    .set(looping)
                    .expect("Failed to set looping");
            }
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

fn remove_sound(mut last_buffer: ResMut<LastAudio>, mut removed: RemovedComponents<Sound>) {
    for entity in removed.iter() {
        last_buffer.remove(&entity);
    }
}

#[derive(Resource, Debug)]
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
    context: Res<Context>,
    config: Res<SynthizerPlugin>,
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
    context: Res<Context>,
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

#[derive(SystemSet, Clone, Hash, Debug, PartialEq, Eq)]
pub enum SynthizerSets {
    PreUpdate,
    UpdateHandles,
    UpdateProperties,
    UpdateState,
    Last,
}

#[derive(Resource)]
struct InitializationGuard(syz::InitializationGuard);

#[derive(Resource, Clone, Copy, Default, Debug)]
pub struct SynthizerPlugin {
    pub default_panner_strategy: Option<syz::PannerStrategy>,
    pub default_distance_model: Option<syz::DistanceModel>,
    pub default_distance_ref: Option<f64>,
    pub default_distance_max: Option<f64>,
    pub default_rolloff: Option<f64>,
    pub default_closeness_boost: Option<f64>,
    pub default_closeness_boost_distance: Option<f64>,
    pub log_level: syz::LogLevel,
    pub log_to_stderr: bool,
}

impl Plugin for SynthizerPlugin {
    fn build(&self, app: &mut App) {
        if !app.world.contains_resource::<SynthizerPlugin>() {
            app.insert_resource(*self);
        }
        let config = *app.world.get_resource::<SynthizerPlugin>().unwrap();
        let mut syz_config = syz::LibraryConfig::new();
        syz_config.log_level(config.log_level);
        if config.log_to_stderr {
            syz_config.log_to_stderr();
        }
        let guard = syz_config
            .initialize()
            .expect("Failed to initialize Synthizer");
        let guard = InitializationGuard(guard);
        let context = syz::Context::new().expect("Failed to create Synthizer context");
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
        let context = Context(context);
        app.add_asset::<Buffer>()
            .init_asset_loader::<BufferAssetLoader>()
            .register_type::<DistanceRef>()
            .register_type::<DistanceMax>()
            .register_type::<Rolloff>()
            .register_type::<ClosenessBoostDistance>()
            .register_type::<AngularPan>()
            .register_type::<ScalarPan>()
            .register_type::<Source>()
            .register_type::<Listener>()
            .insert_resource(guard)
            .insert_resource(context)
            .init_resource::<LastAudio>()
            .insert_resource(defaults)
            .add_event::<SynthizerEvent>()
            .add_systems(
                PreUpdate,
                (sync_config, swap_buffers, change_panner_strategy).in_set(SynthizerSets::PreUpdate),
            )
            .add_systems(
                PostUpdate,
                (add_sound_without_source, add_source_handle, add_generator)
                    .in_set(SynthizerSets::UpdateHandles),
            )
            .configure_set(
                PostUpdate,
                SynthizerSets::UpdateHandles.before(SynthizerSets::UpdateProperties),
            )
            .add_systems(
                PostUpdate,
                update_sound_properties.in_set(SynthizerSets::UpdateProperties),
            )
            .add_systems(
                PostUpdate,
                (update_listener, update_source_properties)
                    .in_set(SynthizerSets::UpdateProperties)
                    .after(TransformSystem::TransformPropagate),
            )
            .configure_set(
                PostUpdate,
                SynthizerSets::UpdateProperties.before(SynthizerSets::UpdateState),
            )
            .add_systems(
                PostUpdate,
                (update_source_playback_state, update_sound_playback_state)
                    .in_set(SynthizerSets::UpdateState),
            )
            .configure_set(
                PostUpdate,
                SynthizerSets::UpdateState.before(SynthizerSets::Last),
            )
            .add_systems(
                PostUpdate,
                (remove_sound, events).in_set(SynthizerSets::Last),
            );
    }
}
