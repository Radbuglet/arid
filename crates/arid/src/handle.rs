use std::{
    fmt::{self, Debug},
    hash,
};

use bytemuck::TransparentWrapper;
use derive_where::derive_where;
use late_struct::LateField;

use crate::{Arena, ArenaManager, ArenaManagerWrapper, RawHandle, Strong, W, World, Wr, world_ns};

// === DebugHandle === //

mod rich_fmt {
    use std::{
        any::{TypeId, type_name},
        cell::UnsafeCell,
        fmt::{self, Debug},
    };

    use rustc_hash::{FxBuildHasher, FxHashSet};

    use crate::{ArenaForHandle, ObjectArena as _, RawHandle, World};

    use super::Handle;

    thread_local! {
        static REENTRANT_DEBUGS: UnsafeCell<FxHashSet<(RawHandle, TypeId)>> =
            const { UnsafeCell::new(FxHashSet::with_hasher(FxBuildHasher) )};
    }

    #[must_use]
    fn reentrant_debug_guard<T: Handle>(handle: T) -> Option<impl Sized> {
        let was_inserted = REENTRANT_DEBUGS.with(|set| {
            unsafe { &mut *set.get() }.insert((handle.raw(), TypeId::of::<T::Object>()))
        });

        if !was_inserted {
            return None;
        }

        Some(scopeguard::guard((), move |()| {
            REENTRANT_DEBUGS.with(|set| {
                unsafe { &mut *set.get() }.remove(&(handle.raw(), TypeId::of::<T::Object>()))
            });
        }))
    }

    pub fn format_handle<T: Handle>(f: &mut fmt::Formatter<'_>, handle: T) -> fmt::Result {
        World::fetch_tls(|cx| {
            f.write_str(type_name::<T::Object>())?;
            handle.raw().fmt(f)?;

            if let Some(cx) = cx
                && let Some(_reentrancy_guard) = reentrant_debug_guard(handle)
            {
                f.write_str(": ")?;

                ArenaForHandle::<T>::print_debug(f, handle, cx)?;
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

pub trait ObjectArenaSimpleSpawn: ObjectArena {
    fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle>;
}

pub trait ObjectArena: 'static + Default + fmt::Debug {
    type Object: Object<Handle = Self::Handle, Arena = Self>;
    type Handle: Handle<Object = Self::Object>;

    // === Accessors === //

    #[must_use]
    fn arena(w: Wr<'_>) -> &Self {
        w.inner.get::<Self::Object>()
    }

    #[must_use]
    fn manager(w: Wr<'_>) -> &ArenaManager {
        &w.inner.get::<ArenaManagerWrapper>().0
    }

    #[must_use]
    fn arena_mut(w: W<'_>) -> &mut Self {
        w.inner.get_mut::<Self::Object>()
    }

    #[must_use]
    fn arena_and_manager_mut(w: W<'_>) -> (&mut Self, &mut ArenaManager) {
        let (arena, manager) = w.inner.get_two::<Self::Object, ArenaManagerWrapper>();

        (arena, &mut manager.0)
    }

    // === Hooks === //

    fn despawn(slot_idx: u32, w: W);

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object>;

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object>;

    fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>>;

    fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle>;

    fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
        if let Some(alive) = handle.try_get(w) {
            alive.fmt(f)
        } else {
            f.write_str("<dangling>")
        }
    }
}

impl<T: Object<Arena = Self>> ObjectArenaSimpleSpawn for Arena<T> {
    fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle> {
        let (arena, manager) = Self::arena_and_manager_mut(w);

        let (handle, keep_alive) = arena.insert(manager, Self::despawn, value);

        Strong::new(Self::Handle::wrap(handle), keep_alive)
    }
}

impl<T: Object<Arena = Self>> ObjectArena for Arena<T> {
    type Object = T;
    type Handle = T::Handle;

    fn despawn(slot_idx: u32, w: W) {
        Self::arena_mut(w).remove_now(slot_idx);
    }

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
        Self::arena(w).get(handle.raw())
    }

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
        Self::arena_mut(w).get_mut(handle.raw())
    }

    fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>> {
        Self::arena(w)
            .upgrade(Self::manager(w), handle.raw())
            .map(|keep_alive| Strong::new(handle, keep_alive))
    }

    fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle> {
        Self::arena(w)
            .slot_to_handle(slot_idx)
            .map(T::Handle::from_raw)
    }
}

// === Traits === //

pub type ArenaForHandle<H> = <<H as Handle>::Object as Object>::Arena;

pub trait Object:
    'static + Sized + fmt::Debug + LateField<world_ns::WorldNs, Value = Self::Arena>
{
    type Arena: ObjectArena<Object = Self, Handle = Self::Handle>;
    type Handle: Handle<Object = Self>;

    #[must_use]
    fn spawn(self, w: W) -> Strong<Self::Handle>
    where
        Self::Arena: ObjectArenaSimpleSpawn,
    {
        Self::Arena::spawn(self, w)
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
    type Object: Object<Handle = Self>;

    fn from_raw(raw: RawHandle) -> Self {
        TransparentWrapper::wrap(raw)
    }

    fn raw(self) -> RawHandle {
        TransparentWrapper::peel(self)
    }

    fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self> {
        ArenaForHandle::<Self>::try_from_slot(slot_idx, w)
    }

    fn from_slot(slot_idx: u32, w: Wr) -> Self {
        Self::try_from_slot(slot_idx, w)
            .unwrap_or_else(|| panic!("slot index {slot_idx} is not occupied"))
    }

    fn try_get(self, w: Wr) -> Option<&Self::Object> {
        ArenaForHandle::<Self>::try_get(self, w)
    }

    fn try_get_mut(self, w: W) -> Option<&mut Self::Object> {
        ArenaForHandle::<Self>::try_get_mut(self, w)
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
        ArenaForHandle::<Self>::as_strong_if_alive(self, w)
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

    pub type CustomArenaOrDefault<Value, CustomArena = Arena<Value>> =
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
                type Object = $ty;
            }
        }
    )*};
}
