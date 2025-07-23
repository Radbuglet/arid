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
            unsafe { &mut *set.get() }.insert((handle.raw_handle(), TypeId::of::<T::Object>()))
        });

        if !was_inserted {
            return None;
        }

        Some(scopeguard::guard((), move |()| {
            REENTRANT_DEBUGS.with(|set| {
                unsafe { &mut *set.get() }.remove(&(handle.raw_handle(), TypeId::of::<T::Object>()))
            });
        }))
    }

    pub fn format_handle<T: Handle>(f: &mut fmt::Formatter<'_>, handle: T) -> fmt::Result {
        World::fetch_tls(|cx| {
            f.write_str(type_name::<T::Object>())?;
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

pub trait ObjectArena: 'static + Default + fmt::Debug {
    type Object: Object<Handle = Self::Handle, Arena = Self>;
    type Handle: Handle<Object = Self::Object, Arena = Self>;

    // === Accessors === //

    #[must_use]
    fn arena(w: Wr<'_>) -> &Self {
        w.inner.get::<Self::Object>()
    }

    #[must_use]
    fn manager(w: Wr<'_>) -> &ArenaManager<World> {
        w.inner.get::<ArenaManager<World>>()
    }

    #[must_use]
    fn arena_mut(w: W<'_>) -> &mut Self {
        w.inner.get_mut::<Self::Object>()
    }

    #[must_use]
    fn arena_and_manager_mut(w: W<'_>) -> (&mut Self, &mut ArenaManager<World>) {
        w.inner.get_two::<Self::Object, ArenaManager<World>>()
    }

    // === Hooks === //

    fn insert(value: Self::Object, w: W) -> Strong<Self::Handle>;

    fn despawn(handle: Self::Handle, w: W);

    fn despawn_raw(handle: RawHandle, w: W) {
        Self::despawn(Self::Handle::wrap(handle), w);
    }

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object>;

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object>;

    fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>>;
}

impl<T: Object<Arena = Self>> ObjectArena for Arena<T, World> {
    type Object = T;
    type Handle = T::Handle;

    fn insert(value: Self::Object, w: W) -> Strong<Self::Handle> {
        let (arena, manager) = Self::arena_and_manager_mut(w);

        let (keep_alive, handle) = arena.insert(manager, Self::despawn_raw, value);

        Strong::new(Self::Handle::wrap(handle), keep_alive)
    }

    fn despawn(handle: Self::Handle, w: W) {
        Self::arena_mut(w).remove_now(handle.raw_handle());
    }

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
        Self::arena(w).get(handle.raw_handle())
    }

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
        Self::arena_mut(w).get_mut(handle.raw_handle())
    }

    fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>> {
        Self::arena(w)
            .upgrade(w.inner.get::<ArenaManager<World>>(), handle.raw_handle())
            .map(|keep_alive| Strong::new(handle, keep_alive))
    }
}

// === Traits === //

pub trait Object:
    'static + Sized + fmt::Debug + LateField<world_ns::WorldNs, Value = Self::Arena>
{
    type Arena: ObjectArena<Object = Self, Handle = Self::Handle>;
    type Handle: Handle<Arena = Self::Arena, Object = Self>;

    #[must_use]
    fn spawn(self, w: W) -> Strong<Self::Handle> {
        Self::Arena::insert(self, w)
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
    type Arena: ObjectArena<Object = Self::Object, Handle = Self>;
    type Object: Object<Arena = Self::Arena, Handle = Self>;

    fn wrap_raw(raw: RawHandle) -> Self {
        TransparentWrapper::wrap(raw)
    }

    fn raw_handle(self) -> RawHandle {
        TransparentWrapper::peel(self)
    }

    fn try_get(self, w: Wr) -> Option<&Self::Object> {
        Self::Arena::try_get(self, w)
    }

    fn try_get_mut(self, w: W) -> Option<&mut Self::Object> {
        Self::Arena::try_get_mut(self, w)
    }

    #[track_caller]
    fn get(self, w: Wr) -> &Self::Object {
        match self.try_get(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    #[track_caller]
    fn get_mut(self, w: W) -> &mut Self::Object {
        match self.try_get_mut(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    #[track_caller]
    fn r(self, w: Wr) -> &Self::Object {
        self.get(w)
    }

    #[track_caller]
    fn m(self, w: W) -> &mut Self::Object {
        self.get_mut(w)
    }

    fn is_alive(self, w: Wr) -> bool {
        self.try_get(w).is_some()
    }

    #[track_caller]
    fn as_strong_if_alive(self, w: Wr) -> Option<Strong<Self>> {
        Self::Arena::as_strong_if_alive(self, w)
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
}

// === Macros === //

#[doc(hidden)]
pub mod object_internals {
    use crate::World;
    use std::marker::PhantomData;

    pub use {
        crate::{Arena, Handle, Object, ObjectArena, RawHandle, format_handle, world_ns::WorldNs},
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

    pub type TakeSecond<L, R = L> = <(PhantomData<L>, PhantomData<R>) as TakeSecondHelper>::Output;

    pub trait TakeSecondHelper {
        type Output;
    }

    impl<L, R> TakeSecondHelper for (PhantomData<L>, PhantomData<R>) {
        type Output = R;
    }

    pub type CustomArenaOrDefault<Value, CustomArena = Arena<Value, World>> =
        TakeSecond<Value, CustomArena>;
}

#[macro_export]
macro_rules! object {
    ( $( $ty:ident $([$arena:ty])? ),*$(,)? ) => {$(
        $crate::object_internals::paste! {
            #[derive(
                $crate::object_internals::Copy,
                $crate::object_internals::Clone,
                $crate::object_internals::Hash,
                $crate::object_internals::Eq,
                $crate::object_internals::PartialEq,
                $crate::object_internals::Ord,
                $crate::object_internals::PartialOrd,
            )]
            #[repr(transparent)]
            pub struct [<$ty Handle>]($crate::object_internals::RawHandle);

            unsafe impl
                $crate::object_internals::TransparentWrapper<$crate::object_internals::RawHandle>
                for [<$ty Handle>]
            {
            }

            impl $crate::object_internals::fmt::Debug for [<$ty Handle>] {
                fn fmt(&self, f: &mut $crate::object_internals::fmt::Formatter<'_>) -> $crate::object_internals::fmt::Result {
                    $crate::object_internals::format_handle(f, *self)
                }
            }

            $crate::object_internals::late_field!(
                $ty[$crate::object_internals::WorldNs] =>
                    $crate::object_internals::CustomArenaOrDefault<$ty, $($arena)?>
            );

            impl $crate::object_internals::Object for $ty {
                type Arena = $crate::object_internals::CustomArenaOrDefault<$ty, $($arena)?>;
                type Handle = [<$ty Handle>];
            }

            impl $crate::object_internals::Handle for [<$ty Handle>] {
                type Arena = $crate::object_internals::CustomArenaOrDefault<$ty, $($arena)?>;
                type Object = $ty;
            }
        }
    )*};
}
