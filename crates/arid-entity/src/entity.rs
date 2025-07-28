use std::{
    any::{TypeId, type_name},
    borrow::Cow,
    cmp::Ordering,
    fmt,
    hash::Hash,
    marker::PhantomData,
    mem,
    ops::Deref,
    rc::Rc,
};

use arid::{
    Arena, Erased, ErasedHandle, Handle, Object, ObjectArena, ObjectArenaSimpleSpawn, Strong, W,
    Wr, object, object_internals::TransparentWrapper,
};
use derive_where::derive_where;
use rustc_hash::FxHashMap;

use crate::archetype::{ArchetypeId, ArchetypeStore};

// === Entity === //

#[derive(Debug)]
pub struct Entity {
    archetype: ArchetypeId,
    parent: Option<EntityHandle>,
    children: Rc<Vec<Strong<EntityHandle>>>,
    index_in_parent: usize,
}

object!(pub Entity[EntityArena]);

#[derive(Debug)]
pub struct DebugLabel(pub Cow<'static, str>);

component!(pub DebugLabel);

impl EntityHandle {
    pub fn new(w: W) -> Strong<Self> {
        Entity {
            archetype: ArchetypeId::EMPTY,
            parent: None,
            children: Rc::new(Vec::new()),
            index_in_parent: 0,
        }
        .spawn(w)
    }

    pub fn parent(self, w: Wr) -> Option<EntityHandle> {
        self.r(w).parent
    }

    pub fn set_parent(self, parent: Option<EntityHandle>, w: W) {
        if self.r(w).parent == parent {
            return;
        }

        if let Some(old_parent) = self.m(w).parent.take() {
            let index = self.r(w).index_in_parent;

            Rc::make_mut(&mut old_parent.m(w).children).swap_remove(index);

            if let Some(moved) = old_parent.r(w).children.get(index) {
                moved.m(w).index_in_parent = index;
            }
        }

        if let Some(new_parent) = parent {
            assert!(!self.is_ancestor_of(new_parent, w));

            let me = self.as_strong(w);

            self.m(w).index_in_parent = new_parent.r(w).children.len();
            self.m(w).parent = Some(new_parent);

            Rc::make_mut(&mut new_parent.m(w).children).push(me);
        }
    }

    pub fn is_ancestor_of(self, other: EntityHandle, w: Wr) -> bool {
        let mut iter = Some(other);

        while let Some(curr) = iter {
            if curr == self {
                return true;
            }

            iter = curr.parent(w);
        }

        false
    }

    pub fn is_descendant_of(self, other: EntityHandle, w: Wr) -> bool {
        other.is_ancestor_of(self, w)
    }

    pub fn add<T: ComponentHandle>(self, handle: Strong<T>, w: W) -> T {
        handle.detach(w);

        let handle_ref = *handle;

        // Update annotation entry
        {
            let state = ComponentArena::<T::Object>::arena_mut(w);

            if let Some(moved) = state.entity_map.insert(self, handle) {
                state.annotations[moved.raw().slot() as usize].entity = None;
            }

            state.annotations[self.raw().slot() as usize].entity = Some(self);
        }

        // Update archetype
        let old_arch = self.r(w).archetype;
        let new_arch = EntityArena::arena_mut(w)
            .archetypes
            .lookup_extend(old_arch, ComponentId::of::<T::Object>());

        self.m(w).archetype = new_arch;

        handle_ref
    }

    pub fn children(self, w: Wr<'_>) -> &Rc<Vec<Strong<EntityHandle>>> {
        &self.r(w).children
    }

    pub fn with_child(self, child: EntityHandle, w: W) -> EntityHandle {
        child.set_parent(Some(self), w);
        self
    }

    pub fn with_label(self, label: impl Into<Cow<'static, str>>, w: W) -> EntityHandle {
        if let Some(existing) = self.try_get::<DebugLabelHandle>(w) {
            existing.m(w).0 = label.into();
        } else {
            self.add(DebugLabel(label.into()).spawn(w), w);
        }

        self
    }

    pub fn with<T: ComponentHandle>(self, handle: Strong<T>, w: W) -> Self {
        self.add(handle, w);
        self
    }

    pub fn try_get<T: ComponentHandle>(self, w: Wr) -> Option<T> {
        ComponentArena::<T::Object>::arena(w)
            .entity_map
            .get(&self)
            .map(|v| **v)
    }

    pub fn get<T: ComponentHandle>(self, w: Wr) -> T {
        self.try_get(w).unwrap_or_else(|| {
            panic!(
                "entity {:?} does not have component of type `{}`",
                self.debug(w),
                type_name::<T::Object>(),
            )
        })
    }

    pub fn try_deep_get<T: ComponentHandle>(self, w: Wr) -> Option<T> {
        let mut iter = Some(self);

        while let Some(curr) = iter {
            if let Some(value) = curr.try_get(w) {
                return Some(value);
            }

            iter = curr.parent(w)
        }

        None
    }

    pub fn deep_get<T: ComponentHandle>(self, w: Wr) -> T {
        self.try_deep_get(w).unwrap_or_else(|| {
            panic!(
                "neither the entity {:?} nor its ancestors have a component of type `{}`",
                self.debug(w),
                type_name::<T::Object>(),
            )
        })
    }

    pub fn traits<E: ?Sized + ErasedHandle>(self, w: Wr) -> EntityTraitIter<E> {
        let arch = self.r(w).archetype;

        EntityArena::arena(w).archetypes.traits(arch).iter(self)
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

    fn despawn(slot_idx: u32, w: W) {
        let handle = EntityHandle::wrap(Self::arena(w).arena.slot_to_handle(slot_idx).unwrap());

        EntityHandle::invoke_pre_destructor(handle, w);

        let arch = Self::arena_mut(w)
            .arena
            .remove_now(slot_idx)
            .unwrap()
            .archetype;

        let comp_set = Self::arena(w).archetypes.component_set(arch).clone();

        for comp in comp_set.iter() {
            (comp.detach_during_delete)(handle, w);
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
        if !handle.is_alive(w) {
            return f.write_str("<dangling>");
        }

        let arch = handle.r(w).archetype;
        let arch_set = Self::arena(w).archetypes.component_set(arch);

        if !f.alternate() {
            let mut f = f.debug_tuple("");

            if arch_set.contains(&ComponentId::of::<DebugLabel>()) {
                f.field(&handle.get::<DebugLabelHandle>(w).r(w).0);
            }

            for comp in arch_set.iter() {
                if (comp.type_id)() == TypeId::of::<DebugLabel>() {
                    continue;
                }

                f.field(&format_args!("{}", (comp.type_name)()));
            }

            return f.finish();
        }

        let mut f = f.debug_map();

        if arch_set.contains(&ComponentId::of::<DebugLabel>()) {
            f.entry(
                &format_args!("<name>"),
                &handle.get::<DebugLabelHandle>(w).r(w).0,
            );
        }

        for comp in arch_set.iter() {
            if (comp.type_id)() == TypeId::of::<DebugLabel>() {
                continue;
            }

            f.entry(
                &format_args!("{}", (comp.type_name)()),
                (comp.get_debug)(handle, w),
            );
        }

        for (i, child) in handle.children(w).clone().iter().enumerate() {
            f.entry(&format_args!("<child {i}>"), child);
        }

        f.finish()
    }
}

// === Component === //

pub trait Component: Object<Arena = ComponentArena<Self>> + EnumerateTraits {}

impl<T> Component for T where T: Object<Arena = ComponentArena<Self>> + EnumerateTraits {}

pub trait ComponentHandle: Handle<Object: Component> {
    fn try_entity(self, w: Wr) -> Option<EntityHandle>;

    fn entity(self, w: Wr) -> EntityHandle;

    fn try_sibling<V: ComponentHandle>(self, w: Wr) -> Option<V>;

    fn sibling<V: ComponentHandle>(self, w: Wr) -> V;

    fn detach(self, w: W);
}

impl<T: Handle<Object: Component>> ComponentHandle for T {
    fn try_entity(self, w: Wr) -> Option<EntityHandle> {
        assert!(self.is_alive(w));

        ComponentArena::<T::Object>::arena(w).annotations[self.raw().slot() as usize].entity
    }

    fn entity(self, w: Wr) -> EntityHandle {
        self.try_entity(w).unwrap_or_else(|| {
            panic!(
                "component {:?} does not have a parent entity",
                self.debug(w)
            )
        })
    }

    fn try_sibling<V: ComponentHandle>(self, w: Wr) -> Option<V> {
        self.try_entity(w).and_then(|v| v.try_get(w))
    }

    fn sibling<V: ComponentHandle>(self, w: Wr) -> V {
        self.entity(w).get(w)
    }

    fn detach(self, w: W) {
        assert!(self.is_alive(w));

        // Update annotation entry
        let state = ComponentArena::<Self::Object>::arena_mut(w);

        let entity = state.annotations[self.raw().slot() as usize].entity.take();

        let Some(entity) = entity else {
            return;
        };

        state.entity_map.remove(&entity);

        // Update archetype
        let old_arch = entity.r(w).archetype;
        let new_arch = EntityArena::arena_mut(w)
            .archetypes
            .lookup_remove(old_arch, ComponentId::of::<Self::Object>());

        entity.m(w).archetype = new_arch;
    }
}

#[derive(Debug)]
#[derive_where(Default)]
pub struct ComponentArena<T: Object> {
    arena: Arena<T>,
    annotations: Vec<ComponentSlot>,
    entity_map: FxHashMap<EntityHandle, Strong<T::Handle>>,
}

#[derive(Debug, Default)]
struct ComponentSlot {
    entity: Option<EntityHandle>,
}

impl<T: Component> ObjectArenaSimpleSpawn for ComponentArena<T> {
    fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle> {
        let (state, manager) = Self::arena_and_manager_mut(w);

        let (handle, keep_alive) = state.arena.insert(manager, Self::despawn, value);

        state
            .annotations
            .resize_with(state.arena.len() as usize, ComponentSlot::default);

        Strong::new(T::Handle::from_raw(handle), keep_alive)
    }
}

impl<T: Component> ObjectArena for ComponentArena<T> {
    type Object = T;
    type Handle = T::Handle;

    fn despawn(slot_idx: u32, w: W) {
        let handle = T::Handle::from_slot(slot_idx, w);

        <T::Handle>::invoke_pre_destructor(handle, w);

        handle.detach(w);
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

// === EnumerateTraits === //

pub trait EnumerateTraits: Object {
    fn enumerate_traits(collector: &mut EntityTraits);
}

#[derive(Debug, Default)]
pub struct EntityTraits {
    traits: FxHashMap<TypeId, Rc<Vec<*const ()>>>,
}

impl EntityTraits {
    pub fn push<E>(&mut self, f: fn(EntityHandle, Wr) -> Erased<E>)
    where
        E: ?Sized + ErasedHandle,
    {
        Rc::make_mut(self.traits.entry(TypeId::of::<E>()).or_default()).push(f as *const ());
    }

    pub fn iter<E>(&self, entity: EntityHandle) -> EntityTraitIter<E>
    where
        E: ?Sized + ErasedHandle,
    {
        match self.traits.get(&TypeId::of::<E>()) {
            Some(list) => EntityTraitIter {
                _ty: PhantomData,
                entity,
                list: Some(list.clone()),
                index: 0,
            },
            None => EntityTraitIter {
                _ty: PhantomData,
                entity,
                list: None,
                index: 0,
            },
        }
    }
}

#[derive_where(Clone)]
pub struct EntityTraitIter<E: ?Sized + ErasedHandle> {
    _ty: PhantomData<fn(E) -> E>,
    entity: EntityHandle,
    list: Option<Rc<Vec<*const ()>>>,
    index: usize,
}

impl<E: ?Sized + ErasedHandle> fmt::Debug for EntityTraitIter<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EntityTraits").finish_non_exhaustive()
    }
}

impl<E: ?Sized + ErasedHandle> Iterator for EntityTraitIter<E> {
    type Item = EntityTraitItem<E>;

    fn next(&mut self) -> Option<Self::Item> {
        let to_erased = *self.list.as_ref()?.get(self.index)?;
        let to_erased =
            unsafe { mem::transmute::<*const (), fn(EntityHandle, Wr) -> Erased<E>>(to_erased) };

        self.index += 1;

        Some(EntityTraitItem {
            entity: self.entity,
            to_erased,
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.list.as_ref().map_or(0, |v| v.len()) - self.index;
        (len, Some(len))
    }
}

impl<E: ?Sized + ErasedHandle> ExactSizeIterator for EntityTraitIter<E> {}

impl<E: ?Sized + ErasedHandle> EntityTraitIter<E> {
    pub fn try_unique(&mut self, w: Wr) -> Option<Erased<E>> {
        if self.len() != 1 {
            return None;
        }

        Some(self.next().unwrap().get(w))
    }

    pub fn unique(&mut self, w: Wr) -> Erased<E> {
        self.try_unique(w).unwrap()
    }
}

#[derive_where(Debug, Copy, Clone)]
pub struct EntityTraitItem<E: ?Sized + ErasedHandle> {
    entity: EntityHandle,
    to_erased: fn(EntityHandle, Wr) -> Erased<E>,
}

impl<E: ?Sized + ErasedHandle> EntityTraitItem<E> {
    pub fn get(self, w: Wr) -> Erased<E> {
        (self.to_erased)(self.entity, w)
    }
}

// === ComponentInfo === //

#[derive(Copy, Clone)]
pub(crate) struct ComponentId(&'static ComponentInfo);

impl fmt::Debug for ComponentId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ComponentId")
            .field(&(self.type_name)())
            .finish()
    }
}

impl ComponentId {
    pub fn of<T: Component>() -> Self {
        struct Helper<T>(T);

        impl<T: Component> Helper<T> {
            const INFO: &'static ComponentInfo = &ComponentInfo {
                type_id: TypeId::of::<T>,
                type_name: type_name::<T>,
                detach_during_delete: |entity, w| {
                    let state = ComponentArena::<T>::arena_mut(w);

                    let comp = state.entity_map.remove(&entity).unwrap();

                    state.annotations[comp.raw().slot() as usize].entity = None;
                },
                get_debug: |entity, w| ComponentArena::<T>::arena(w).entity_map[&entity].r(w),
                enumerate_traits: T::enumerate_traits,
            };
        }

        Self(Helper::<T>::INFO)
    }
}

impl Deref for ComponentId {
    type Target = ComponentInfo;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl Hash for ComponentId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.type_id)().hash(state);
    }
}

impl Eq for ComponentId {}

impl PartialEq for ComponentId {
    fn eq(&self, other: &Self) -> bool {
        (self.0.type_id)() == (other.0.type_id)()
    }
}

impl Ord for ComponentId {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.0.type_id)().cmp(&(other.0.type_id)())
    }
}

impl PartialOrd for ComponentId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub(crate) struct ComponentInfo {
    pub type_id: fn() -> TypeId,
    pub type_name: fn() -> &'static str,
    pub detach_during_delete: fn(EntityHandle, W),
    pub get_debug: fn(EntityHandle, Wr<'_>) -> &'_ dyn fmt::Debug,
    pub enumerate_traits: fn(&mut EntityTraits),
}

// === Macro === //

#[doc(hidden)]
pub mod component_internals {
    pub use {
        super::{ComponentArena, EntityTraits, EnumerateTraits},
        arid::{ErasedHandle, Object, erase, object},
    };
}

#[macro_export]
macro_rules! component {
    (
        $(
            $vis:vis $name:ident
            $([
                $($erase_as:ty),*
                $(,)?
            ])?
        ),*
        $(,)?
    ) => {$(
        $crate::component_internals::object!($vis $name[$crate::component_internals::ComponentArena<Self>]);

        impl $crate::component_internals::EnumerateTraits for $name {
            fn enumerate_traits(collector: &mut $crate::component_internals::EntityTraits) {
                collector.push::<dyn $crate::component_internals::ErasedHandle>(|entity, w| {
                    $crate::component_internals::erase!(entity.get::<<Self as $crate::component_internals::Object>::Handle>(w))
                });

                $($(
                    collector.push::<$erase_as>(|entity, w| {
                        $crate::component_internals::erase!(entity.get::<<Self as $crate::component_internals::Object>::Handle>(w))
                    });
                )*)?
            }
        }
    )*};
}

pub use component;
