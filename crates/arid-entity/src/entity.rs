use std::any::type_name;

use arid::{Arena, ArenaForHandle, Handle, Object, ObjectArena, Strong, W, World, Wr, object};
use derive_where::derive_where;
use rustc_hash::FxHashMap;

// === Entity === //

#[derive(Debug)]
#[non_exhaustive]
pub struct Entity {}

object!(Entity);

impl EntityHandle {
    pub fn new(w: W) -> Strong<Self> {
        Entity {}.spawn(w)
    }

    pub fn add<T: NodeHandle>(self, handle: Strong<T>, w: W) -> T {
        handle.detach_from_entity(w);

        let handle_ref = *handle;

        let state = ArenaForHandle::<T>::arena_mut(w);

        if let Some(moved) = state.entity_map.insert(self, handle) {
            state.annotations[moved.raw().slot() as usize].entity = None;
        }

        state.annotations[self.raw().slot() as usize].entity = Some(self);

        handle_ref
    }

    pub fn try_get<T: NodeHandle>(self, w: Wr) -> Option<T> {
        ArenaForHandle::<T>::arena(w)
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

// === Node === //

pub trait Node: Object<Arena = NodeArena<Self>> {}

impl<T: Object<Arena = NodeArena<Self>>> Node for T {}

pub trait NodeHandle: Handle<Object: Node> {
    fn try_entity(self, w: Wr) -> Option<EntityHandle>;

    fn entity(self, w: Wr) -> EntityHandle;

    fn detach_from_entity(self, w: W);
}

impl<T: Handle<Object: Node>> NodeHandle for T {
    fn try_entity(self, w: Wr) -> Option<EntityHandle> {
        assert!(self.is_alive(w));

        ArenaForHandle::<Self>::arena(w).annotations[self.raw().slot() as usize].entity
    }

    fn entity(self, w: Wr) -> EntityHandle {
        self.try_entity(w).unwrap_or_else(|| {
            panic!(
                "component {:?} does not have a parent entity",
                self.debug(w)
            )
        })
    }

    fn detach_from_entity(self, w: W) {
        assert!(self.is_alive(w));

        let state = ArenaForHandle::<Self>::arena_mut(w);

        let entity = state.annotations[self.raw().slot() as usize].entity.take();

        if let Some(entity) = entity {
            state.entity_map.remove(&entity);
        }
    }
}

#[derive(Debug)]
#[derive_where(Default)]
pub struct NodeArena<T: Object> {
    arena: Arena<T, World>,
    annotations: Vec<NodeSlot>,
    entity_map: FxHashMap<EntityHandle, Strong<T::Handle>>,
}

#[derive(Debug, Default)]
struct NodeSlot {
    entity: Option<EntityHandle>,
}

impl<T: Object<Arena = Self>> ObjectArena for NodeArena<T> {
    type Object = T;
    type Handle = T::Handle;

    fn insert(value: Self::Object, w: W) -> Strong<Self::Handle> {
        let (state, manager) = Self::arena_and_manager_mut(w);

        let (handle, keep_alive) = state.arena.insert(manager, Self::despawn_raw, value);

        state
            .annotations
            .resize_with(state.arena.len() as usize, NodeSlot::default);

        Strong::new(T::Handle::wrap_raw(handle), keep_alive)
    }

    fn despawn(handle: Self::Handle, w: W) {
        handle.detach_from_entity(w);

        drop(Self::arena_mut(w).arena.remove_now(handle.raw()));
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
}
