use std::{
    any::{TypeId, type_name},
    cmp::Ordering,
    fmt,
    hash::Hash,
    ops::Deref,
};

use arid::{
    Arena, Handle, Object, ObjectArena, ObjectArenaSimpleSpawn, Strong, W, Wr, object,
    object_internals::TransparentWrapper,
};
use derive_where::derive_where;
use rustc_hash::FxHashMap;

use crate::archetype::{ArchetypeId, ArchetypeStore};

// === Entity === //

#[derive(Debug)]
pub struct Entity {
    archetype: ArchetypeId,
}

object!(Entity[EntityArena]);

impl EntityHandle {
    pub fn new(w: W) -> Strong<Self> {
        Entity {
            archetype: ArchetypeId::EMPTY,
        }
        .spawn(w)
    }

    pub fn add<T: NodeHandle>(self, handle: Strong<T>, w: W) -> T {
        handle.detach(w);

        let handle_ref = *handle;

        // Update annotation entry
        {
            let state = NodeArena::<T::Object>::arena_mut(w);

            if let Some(moved) = state.entity_map.insert(self, handle) {
                state.annotations[moved.raw().slot() as usize].entity = None;
            }

            state.annotations[self.raw().slot() as usize].entity = Some(self);
        }

        // Update archetype
        let old_arch = self.r(w).archetype;
        let new_arch = EntityArena::arena_mut(w)
            .archetypes
            .lookup_extend(old_arch, NodeId::of::<T::Object>());

        self.m(w).archetype = new_arch;

        handle_ref
    }

    pub fn try_get<T: NodeHandle>(self, w: Wr) -> Option<T> {
        NodeArena::<T::Object>::arena(w)
            .entity_map
            .get(&self)
            .map(|v| **v)
    }

    pub fn get<T: NodeHandle>(self, w: Wr) -> T {
        self.try_get(w).unwrap_or_else(|| {
            panic!(
                "entity {:?} does not have component of type `{}`",
                self.debug(w),
                type_name::<T::Object>(),
            )
        })
    }
}

mod entity_arena {
    use super::*;

    #[derive(Debug, Default)]
    pub struct EntityArena {
        pub(super) arena: Arena<Entity>,
        pub(super) archetypes: ArchetypeStore,
    }
}

use self::entity_arena::EntityArena;

impl ObjectArenaSimpleSpawn for EntityArena {
    fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle> {
        let (arena, manager) = Self::arena_and_manager_mut(w);

        let (handle, keep_alive) = arena.arena.insert(manager, Self::despawn, value);

        Strong::new(EntityHandle::wrap(handle), keep_alive)
    }
}

impl ObjectArena for EntityArena {
    type Object = Entity;
    type Handle = EntityHandle;

    fn despawn(handle: u32, w: W) {
        let entity = EntityHandle::wrap(Self::arena(w).arena.slot_to_handle(handle).unwrap());

        let arch = Self::arena_mut(w)
            .arena
            .remove_now(handle)
            .unwrap()
            .archetype;

        let comp_set = Self::arena(w).archetypes.node_set(arch).clone();

        for comp in comp_set.iter() {
            (comp.detach_during_delete)(entity, w);
        }
    }

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
        Self::arena(w).arena.get(handle.raw())
    }

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
        Self::arena_mut(w).arena.get_mut(handle.raw())
    }

    fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>> {
        Self::arena(w)
            .arena
            .upgrade(Self::manager(w), handle.raw())
            .map(|keep_alive| Strong::new(handle, keep_alive))
    }

    fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle> {
        Self::arena(w)
            .arena
            .slot_to_handle(slot_idx)
            .map(EntityHandle::wrap)
    }

    fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
        let mut f = f.debug_map();

        let arch = handle.r(w).archetype;
        let arch_set = Self::arena(w).archetypes.node_set(arch);

        for comp in arch_set.iter() {
            f.entry(&(comp.type_name)(), (comp.get_debug)(handle, w));
        }

        f.finish()
    }
}

// === Node === //

pub trait Node: Object<Arena = NodeArena<Self>> {}

impl<T: Object<Arena = NodeArena<Self>>> Node for T {}

pub trait NodeHandle: Handle<Object: Node> {
    fn try_entity(self, w: Wr) -> Option<EntityHandle>;

    fn entity(self, w: Wr) -> EntityHandle;

    fn detach(self, w: W);
}

impl<T: Handle<Object: Node>> NodeHandle for T {
    fn try_entity(self, w: Wr) -> Option<EntityHandle> {
        assert!(self.is_alive(w));

        NodeArena::<T::Object>::arena(w).annotations[self.raw().slot() as usize].entity
    }

    fn entity(self, w: Wr) -> EntityHandle {
        self.try_entity(w).unwrap_or_else(|| {
            panic!(
                "component {:?} does not have a parent entity",
                self.debug(w)
            )
        })
    }

    fn detach(self, w: W) {
        assert!(self.is_alive(w));

        // Update annotation entry
        let state = NodeArena::<Self::Object>::arena_mut(w);

        let entity = state.annotations[self.raw().slot() as usize].entity.take();

        let Some(entity) = entity else {
            return;
        };

        state.entity_map.remove(&entity);

        // Update archetype
        let old_arch = entity.r(w).archetype;
        let new_arch = EntityArena::arena_mut(w)
            .archetypes
            .lookup_remove(old_arch, NodeId::of::<Self::Object>());

        entity.m(w).archetype = new_arch;
    }
}

#[derive(Debug)]
#[derive_where(Default)]
pub struct NodeArena<T: Object> {
    arena: Arena<T>,
    annotations: Vec<NodeSlot>,
    entity_map: FxHashMap<EntityHandle, Strong<T::Handle>>,
}

#[derive(Debug, Default)]
struct NodeSlot {
    entity: Option<EntityHandle>,
}

impl<T: Object<Arena = Self>> ObjectArenaSimpleSpawn for NodeArena<T> {
    fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle> {
        let (state, manager) = Self::arena_and_manager_mut(w);

        let (handle, keep_alive) = state.arena.insert(manager, Self::despawn, value);

        state
            .annotations
            .resize_with(state.arena.len() as usize, NodeSlot::default);

        Strong::new(T::Handle::from_raw(handle), keep_alive)
    }
}

impl<T: Object<Arena = Self>> ObjectArena for NodeArena<T> {
    type Object = T;
    type Handle = T::Handle;

    fn despawn(slot_idx: u32, w: W) {
        T::Handle::from_slot(slot_idx, w).detach(w);

        drop(Self::arena_mut(w).arena.remove_now(slot_idx));
    }

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
        Self::arena(w).arena.get(handle.raw())
    }

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
        Self::arena_mut(w).arena.get_mut(handle.raw())
    }

    fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>> {
        Self::arena(w)
            .arena
            .upgrade(Self::manager(w), handle.raw())
            .map(|keep_alive| Strong::new(handle, keep_alive))
    }

    fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle> {
        Self::arena(w)
            .arena
            .slot_to_handle(slot_idx)
            .map(Self::Handle::from_raw)
    }
}

// === NodeInfo === //

#[derive(Copy, Clone)]
pub(crate) struct NodeId(&'static NodeInfo);

impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("NodeId").field(&(self.type_name)()).finish()
    }
}

impl NodeId {
    pub fn of<T: Node>() -> Self {
        struct Helper<T>(T);

        impl<T: Node> Helper<T> {
            const INFO: &'static NodeInfo = &NodeInfo {
                type_id: TypeId::of::<T>,
                type_name: type_name::<T>,
                detach_during_delete: |entity, w| {
                    let state = NodeArena::<T>::arena_mut(w);

                    let comp = state.entity_map.remove(&entity).unwrap();

                    state.annotations[comp.raw().slot() as usize].entity = None;
                },
                get_debug: |entity, w| NodeArena::<T>::arena(w).entity_map[&entity].r(w),
            };
        }

        Self(Helper::<T>::INFO)
    }
}

impl Deref for NodeId {
    type Target = NodeInfo;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl Hash for NodeId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.type_id)().hash(state);
    }
}

impl Eq for NodeId {}

impl PartialEq for NodeId {
    fn eq(&self, other: &Self) -> bool {
        (self.0.type_id)() == (other.0.type_id)()
    }
}

impl Ord for NodeId {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.0.type_id)().cmp(&(other.0.type_id)())
    }
}

impl PartialOrd for NodeId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub(crate) struct NodeInfo {
    pub type_id: fn() -> TypeId,
    pub type_name: fn() -> &'static str,
    pub detach_during_delete: fn(EntityHandle, W),
    pub get_debug: fn(EntityHandle, Wr<'_>) -> &'_ dyn fmt::Debug,
}
