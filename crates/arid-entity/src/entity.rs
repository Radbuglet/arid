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
    Erased, ErasedHandle, Handle, MayDangle, Object, ObjectArena, RawArena, Strong, W, Wr, object,
    object_internals::TransparentWrapper,
};
use derive_where::derive_where;
use hashbrown::hash_map;

use crate::{
    archetype::{ArchetypeId, ArchetypeStore},
    utils::FxHashMap,
};

// === Entity === //

#[derive(Debug)]
pub struct DebugLabel(pub Cow<'static, str>);

component!(pub DebugLabel);

#[derive(Debug)]
pub struct Entity {
    archetype: ArchetypeId,
    parent: MayDangle<EntityHandle>,
}

object!(pub Entity[EntityArena]);

impl EntityHandle {
    pub fn new(parent: Option<EntityHandle>, w: W) -> Strong<Self> {
        let (arena, manager) = w.arena_and_manager_mut::<EntityArena>();

        let (handle, keep_alive) = arena.arena.spawn(
            manager,
            |handle, w| {
                let handle = EntityHandle::wrap(handle);

                EntityHandle::invoke_pre_destructor(handle, w);

                let arch = w
                    .arena_mut::<EntityArena>()
                    .arena
                    .despawn(handle.raw())
                    .unwrap()
                    .archetype;

                let comp_set = w
                    .arena::<EntityArena>()
                    .archetypes
                    .component_set(arch)
                    .clone();

                for comp in comp_set.iter() {
                    (comp.owner_destroyed)(handle, w);
                }
            },
            Entity {
                archetype: ArchetypeId::EMPTY,
                parent: MayDangle::DANGLING,
            },
        );

        let me = Strong::new(EntityHandle::wrap(handle), keep_alive);

        me.set_parent(parent, w);
        me
    }

    pub fn set_label(self, label: impl Into<Cow<'static, str>>, w: W) {
        if let Some(handle) = self.try_get::<DebugLabelHandle>(w) {
            handle.m(w).0 = label.into();
        } else {
            DebugLabel(label.into()).attach(self, w);
        }
    }

    pub fn parent(self, w: Wr) -> Option<EntityHandle> {
        self.r(w).parent.get(w)
    }

    pub fn set_parent(self, parent: Option<EntityHandle>, w: W) {
        assert!(parent.is_none_or(|parent| !self.is_ancestor_of(parent, w)));

        self.m(w).parent = parent.map_or(MayDangle::DANGLING, MayDangle::new);
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

    pub fn try_get<T: ComponentHandle>(self, w: Wr) -> Option<T> {
        w.arena::<ComponentArena<T::Object>>()
            .entity_map
            .get(&self)
            .copied()
    }

    pub fn get<T: ComponentHandle>(self, w: Wr) -> T {
        self.try_get(w).unwrap_or_else(|| {
            panic!(
                "entity {:?} does not have component of type `{}` or is dangling",
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

        w.arena::<EntityArena>().archetypes.traits(arch).iter(self)
    }
}

mod entity_arena {
    use arid::DefaultObjectArena;

    use super::*;

    #[derive(Debug, Default)]
    pub struct EntityArena {
        pub(super) arena: DefaultObjectArena<Entity>,
        pub(super) archetypes: ArchetypeStore,
    }
}

use self::entity_arena::EntityArena;

impl ObjectArena for EntityArena {
    type Object = Entity;
    type Handle = EntityHandle;

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
        w.arena::<Self>().arena.get(handle.raw())
    }

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
        w.arena_mut::<Self>().arena.get_mut(handle.raw())
    }

    fn as_strong_if_alive(handle: Self::Handle, w: W) -> Option<Strong<Self::Handle>> {
        let (arena, manager) = w.arena_and_manager_mut::<Self>();

        arena
            .arena
            .upgrade(manager, handle.raw())
            .map(|keep_alive| Strong::new(handle, keep_alive))
    }

    fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
        if !handle.is_alive(w) {
            return f.write_str("<dangling>");
        }

        let arch = handle.r(w).archetype;
        let arch_set = w.arena::<Self>().archetypes.component_set(arch);

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

        f.finish()
    }
}

// === Component === //

pub trait Component: Object<Arena = ComponentArena<Self>> + EnumerateTraits + fmt::Debug {
    fn attach(self, owner: EntityHandle, w: W) -> Strong<Self::Handle>;

    fn singleton(self, parent: Option<EntityHandle>, w: W) -> Strong<Self::Handle>;
}

impl<T> Component for T
where
    T: Object<Arena = ComponentArena<Self>> + EnumerateTraits + fmt::Debug,
{
    fn attach(self, owner: EntityHandle, w: W) -> Strong<Self::Handle> {
        // Obtain the owner's `KeepAlive`.
        let (entity_arena, manager) = w.arena_and_manager_mut::<EntityArena>();
        let keep_alive = entity_arena
            .arena
            .upgrade(manager, owner.raw())
            .unwrap_or_else(|| panic!("{owner:?} is dangling"));

        // Extend the owner's archetype.
        let entity_slot = entity_arena.arena.get_mut(owner.raw()).unwrap();

        entity_slot.archetype = entity_arena
            .archetypes
            .lookup_extend(entity_slot.archetype, ComponentId::of::<Self>());

        // Create the component handle and associate it to the entity.
        let arena = w.arena_mut::<Self::Arena>();

        let entry = match arena.entity_map.entry(owner) {
            hash_map::Entry::Vacant(entry) => entry,
            hash_map::Entry::Occupied(entry) => {
                let entry = *entry.get();

                panic!(
                    "{:?} already has component of type `{}`: {:#?}",
                    owner.debug(w),
                    type_name::<T>(),
                    entry.debug(w)
                );
            }
        };

        let handle = arena.arena.insert(ComponentState { value: self, owner });
        let handle = T::Handle::from_raw(handle);
        entry.insert(handle);

        Strong::new(handle, keep_alive)
    }

    fn singleton(self, parent: Option<EntityHandle>, w: W) -> Strong<Self::Handle> {
        self.attach(EntityHandle::new(parent, w).as_weak(), w)
    }
}

pub trait ComponentHandle: Handle<Object: Component> {
    fn entity(self, w: Wr) -> EntityHandle;

    fn try_sibling<V: ComponentHandle>(self, w: Wr) -> Option<V>;

    fn sibling<V: ComponentHandle>(self, w: Wr) -> V;
}

impl<T: Handle<Object: Component>> ComponentHandle for T {
    #[track_caller]
    fn entity(self, w: Wr) -> EntityHandle {
        w.arena::<ComponentArena<T::Object>>()
            .arena
            .get(self.raw())
            .unwrap_or_else(|| panic!("{self:?} is dangling"))
            .owner
    }

    fn try_sibling<V: ComponentHandle>(self, w: Wr) -> Option<V> {
        self.entity(w).try_get(w)
    }

    fn sibling<V: ComponentHandle>(self, w: Wr) -> V {
        self.entity(w).get(w)
    }
}

#[derive(Debug)]
#[derive_where(Default)]
pub struct ComponentArena<T: Object> {
    arena: RawArena<ComponentState<T>>,
    entity_map: FxHashMap<EntityHandle, T::Handle>,
}

struct ComponentState<T> {
    value: T,
    owner: EntityHandle,
}

impl<T: Component> ObjectArena for ComponentArena<T> {
    type Object = T;
    type Handle = T::Handle;

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
        w.arena::<Self>().arena.get(handle.raw()).map(|v| &v.value)
    }

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
        w.arena_mut::<Self>()
            .arena
            .get_mut(handle.raw())
            .map(|v| &mut v.value)
    }

    fn as_strong_if_alive(handle: Self::Handle, w: W) -> Option<Strong<Self::Handle>> {
        // Determine the entity's owner if it's still alive.
        let owner = w.arena::<Self>().arena.get(handle.raw())?.owner;

        // Obtain the entity's keep-alive.
        let (entity_arena, manager) = w.arena_and_manager_mut::<EntityArena>();
        let keep_alive = entity_arena.arena.upgrade(manager, owner.raw()).unwrap();

        Some(Strong::new(handle, keep_alive))
    }

    fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
        if let Some(alive) = handle.try_r(w) {
            alive.fmt(f)
        } else {
            f.write_str("<dangling>")
        }
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
                owner_destroyed: |entity, w| {
                    Handle::invoke_pre_destructor(
                        w.arena::<ComponentArena<T>>().entity_map[&entity],
                        w,
                    );

                    let state = w.arena_mut::<ComponentArena<T>>();
                    let comp = state.entity_map.remove(&entity).unwrap();
                    state.arena.remove(comp.raw());
                },
                get_debug: |entity, w| w.arena::<ComponentArena<T>>().entity_map[&entity].r(w),
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
    pub owner_destroyed: fn(EntityHandle, W),
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
