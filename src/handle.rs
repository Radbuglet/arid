use std::{fmt, hash};

use bytemuck::TransparentWrapper;
use derive_where::derive_where;
use late_struct::LateField;

use crate::{Arena, ArenaManager, RawHandle, Strong, W, World, Wr, world_ns};

// === DebugHandle === //

mod rich_fmt {
    use std::{
        any::{TypeId, type_name},
        cell::UnsafeCell,
        fmt::{self, Debug},
    };

    use rustc_hash::{FxBuildHasher, FxHashSet};

    use crate::{RawHandle, World};

    use super::Handle;

    thread_local! {
        static REENTRANT_DEBUGS: UnsafeCell<FxHashSet<(RawHandle, TypeId)>> =
            const { UnsafeCell::new(FxHashSet::with_hasher(FxBuildHasher) )};
    }

    #[must_use]
    fn reentrant_debug_guard<T: Handle>(handle: T) -> Option<impl Sized> {
        let was_inserted = REENTRANT_DEBUGS.with(|set| {
            unsafe { &mut *set.get() }.insert((handle.raw_handle(), TypeId::of::<T::Component>()))
        });

        if !was_inserted {
            return None;
        }

        Some(scopeguard::guard((), move |()| {
            REENTRANT_DEBUGS.with(|set| {
                unsafe { &mut *set.get() }
                    .remove(&(handle.raw_handle(), TypeId::of::<T::Component>()))
            });
        }))
    }

    pub fn format_handle<T: Handle>(f: &mut fmt::Formatter<'_>, handle: T) -> fmt::Result {
        World::fetch_tls(|cx| {
            f.write_str(type_name::<T::Component>())?;
            handle.raw_handle().fmt(f)?;

            if let Some(cx) = cx
                && let Some(_reentrancy_guard) = reentrant_debug_guard(handle)
            {
                f.write_str(": ")?;

                if let Some(alive) = handle.try_get(cx) {
                    alive.fmt(f)?;
                } else {
                    f.write_str("<dangling>")?;
                }
            }

            Ok(())
        })
    }
}

pub use self::rich_fmt::*;

#[derive_where(Copy, Clone)]
pub struct DebugHandle<'a, T: Handle> {
    pub world: &'a World,
    pub handle: T,
}

impl<T: Handle> fmt::Debug for DebugHandle<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.world.provide_tls(|| self.handle.fmt(f))
    }
}

// === Meta === //

#[derive_where(Debug, Default)]
pub struct ComponentArena<T: Component> {
    pub arena: Arena<ComponentSlot<T>, World>,
    pub meta: T::MetaArena,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct ComponentSlot<T: Component> {
    pub value: T,
    pub meta: T::Meta,
}

pub trait Meta<T: Handle>: Sized + 'static {
    type ArenaAnnotation: 'static + Default + fmt::Debug;

    fn despawn(handle: T, w: W);

    fn despawn_raw(handle: RawHandle, w: W) {
        Self::despawn(T::wrap(handle), w);
    }
}

impl<T: Handle> Meta<T> for () {
    type ArenaAnnotation = ();

    fn despawn(handle: T, w: W) {
        handle.despawn_now(w);
    }
}

// === Traits === //

pub trait Component:
    'static + Sized + fmt::Debug + LateField<world_ns::WorldNs, Value = ComponentArena<Self>>
{
    type MetaArena: 'static + Default + fmt::Debug;
    type Meta: Meta<Self::Handle, ArenaAnnotation = Self::MetaArena>;
    type Handle: Handle<MetaArena = Self::MetaArena, Meta = Self::Meta, Component = Self>;

    fn arena(w: Wr) -> &ComponentArena<Self> {
        w.inner.get::<Self>()
    }

    fn arena_mut(w: W) -> &mut ComponentArena<Self> {
        w.inner.get_mut::<Self>()
    }

    #[must_use]
    fn spawn(self, w: W) -> Strong<Self::Handle>
    where
        Self::Meta: Default,
    {
        self.spawn_with(Self::Meta::default(), w)
    }

    #[must_use]
    fn spawn_with(self, meta: Self::Meta, w: W) -> Strong<Self::Handle> {
        let (arena, manager) = w.inner.get_two::<Self, ArenaManager<World>>();

        let (keep_alive, handle) = arena.arena.insert(
            manager,
            <Self::Meta as Meta<Self::Handle>>::despawn_raw,
            ComponentSlot { value: self, meta },
        );

        Strong::new(Self::Handle::wrap(handle), keep_alive)
    }
}

pub trait Handle:
    'static
    + Sized
    + Send
    + Sync
    + fmt::Debug
    + Copy
    + Eq
    + hash::Hash
    + Ord
    + TransparentWrapper<RawHandle>
{
    type MetaArena: 'static + Default + fmt::Debug;
    type Meta: Meta<Self, ArenaAnnotation = Self::MetaArena>;
    type Component: Component<MetaArena = Self::MetaArena, Meta = Self::Meta, Handle = Self>;

    fn wrap_raw(raw: RawHandle) -> Self {
        TransparentWrapper::wrap(raw)
    }

    fn raw_handle(self) -> RawHandle {
        TransparentWrapper::peel(self)
    }

    fn try_slot(self, w: Wr) -> Option<&ComponentSlot<Self::Component>> {
        w.inner
            .get::<Self::Component>()
            .arena
            .get(self.raw_handle())
    }

    fn try_slot_mut(self, w: W) -> Option<&mut ComponentSlot<Self::Component>> {
        w.inner
            .get_mut::<Self::Component>()
            .arena
            .get_mut(self.raw_handle())
    }

    #[track_caller]
    fn slot(self, w: Wr) -> &ComponentSlot<Self::Component> {
        match self.try_slot(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    #[track_caller]
    fn slot_mut(self, w: W) -> &mut ComponentSlot<Self::Component> {
        match self.try_slot_mut(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    fn try_get(self, w: Wr) -> Option<&Self::Component> {
        self.try_slot(w).map(|v| &v.value)
    }

    fn try_get_mut(self, w: W) -> Option<&mut Self::Component> {
        self.try_slot_mut(w).map(|v| &mut v.value)
    }

    #[track_caller]
    fn get(self, w: Wr) -> &Self::Component {
        &self.slot(w).value
    }

    #[track_caller]
    fn get_mut(self, w: W) -> &mut Self::Component {
        &mut self.slot_mut(w).value
    }

    fn try_meta(self, w: Wr) -> Option<&Self::Meta> {
        self.try_slot(w).map(|v| &v.meta)
    }

    fn try_get_meta(self, w: W) -> Option<&mut Self::Meta> {
        self.try_slot_mut(w).map(|v| &mut v.meta)
    }

    #[track_caller]
    fn meta(self, w: Wr) -> &Self::Meta {
        &self.slot(w).meta
    }

    #[track_caller]
    fn meta_mut(self, w: W) -> &mut Self::Meta {
        &mut self.slot_mut(w).meta
    }

    #[track_caller]
    fn r(self, w: Wr) -> &Self::Component {
        self.get(w)
    }

    #[track_caller]
    fn m(self, w: W) -> &mut Self::Component {
        self.get_mut(w)
    }

    fn is_alive(self, w: Wr) -> bool {
        self.try_get(w).is_some()
    }

    #[track_caller]
    fn as_strong_if_alive(self, w: Wr) -> Option<Strong<Self>> {
        w.inner
            .get::<Self::Component>()
            .arena
            .upgrade(w.inner.get::<ArenaManager<World>>(), self.raw_handle())
            .map(|keep_alive| Strong::new(self, keep_alive))
    }

    #[track_caller]
    fn as_strong(self, w: Wr) -> Strong<Self> {
        match self.as_strong_if_alive(w) {
            Some(v) => v,
            None => panic!("attempted to upgrade dangling handle {self:?}"),
        }
    }

    #[must_use]
    fn debug(self, w: Wr) -> DebugHandle<'_, Self> {
        DebugHandle {
            world: w,
            handle: self,
        }
    }

    fn despawn_now(self, w: W) -> Option<ComponentSlot<Self::Component>> {
        w.inner
            .get_mut::<Self::Component>()
            .arena
            .remove_now(self.raw_handle())
    }
}

// === Macros === //

#[doc(hidden)]
pub mod component_internals {
    pub use {
        crate::{
            Component, ComponentArena, Handle, Meta, RawHandle, format_handle, world_ns::WorldNs,
        },
        bytemuck::TransparentWrapper,
        late_struct::late_field,
        paste::paste,
        std::{
            clone::Clone,
            cmp::{Eq, Ord, PartialEq, PartialOrd},
            fmt,
            hash::Hash,
            marker::Copy,
        },
    };
}

#[macro_export]
macro_rules! component {
    ( $( $ty:ident $([$meta:ty])? ),*$(,)? ) => {$(
        $crate::component_internals::paste! {
            #[derive(
                $crate::component_internals::Copy,
                $crate::component_internals::Clone,
                $crate::component_internals::Hash,
                $crate::component_internals::Eq,
                $crate::component_internals::PartialEq,
                $crate::component_internals::Ord,
                $crate::component_internals::PartialOrd,
                $crate::component_internals::TransparentWrapper,
            )]
            #[repr(transparent)]
            pub struct [<$ty Handle>]($crate::component_internals::RawHandle);

            impl $crate::component_internals::fmt::Debug for [<$ty Handle>] {
                fn fmt(&self, f: &mut $crate::component_internals::fmt::Formatter<'_>) -> $crate::component_internals::fmt::Result {
                    $crate::component_internals::format_handle(f, *self)
                }
            }

            $crate::component_internals::late_field!(
                $ty[$crate::component_internals::WorldNs] => $crate::component_internals::ComponentArena<$ty>
            );

            impl $crate::component_internals::Component for $ty {
                type MetaArena = <($($meta)?) as $crate::component_internals::Meta<[<$ty Handle>]>>::ArenaAnnotation;
                type Meta = ($($meta)?);
                type Handle = [<$ty Handle>];
            }

            impl $crate::component_internals::Handle for [<$ty Handle>] {
                type MetaArena = <($($meta)?) as $crate::component_internals::Meta<[<$ty Handle>]>>::ArenaAnnotation;
                type Meta = ($($meta)?);
                type Component = $ty;
            }
        }
    )*};
}
