use std::{fmt, hash};

use bytemuck::TransparentWrapper;
use derive_where::derive_where;
use late_struct::LateField;

use crate::{
    ErasedHandle, KeepAlive, KeepAliveIndex, RawArena, RawHandle, Strong, W, WorldDebug,
    WorldKeepAliveManager, WorldKeepAliveUserdata, Wr, world_ns,
};

// === rich_fmt === //

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

// === ObjectArena === //

pub trait ObjectArenaSimpleSpawn: ObjectArena {
    fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle>;
}

pub trait ObjectArena: 'static + Default {
    type Object: Object<Handle = Self::Handle, Arena = Self>;
    type Handle: Handle<Object = Self::Object>;

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object>;

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object>;

    fn as_strong_if_alive(handle: Self::Handle, w: W) -> Option<Strong<Self::Handle>>;

    fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle>;

    fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result;
}

#[derive_where(Default)]
pub struct DefaultObjectArena<T> {
    arena: RawArena<T>,
    keep_alive: Vec<KeepAliveIndex>,
}

impl<T> fmt::Debug for DefaultObjectArena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DefaultObjectArena").finish_non_exhaustive()
    }
}

impl<T> DefaultObjectArena<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn spawn(
        &mut self,
        manager: &mut WorldKeepAliveManager,
        destructor: fn(RawHandle, W),
        value: T,
    ) -> (RawHandle, KeepAlive) {
        let handle = self.arena.insert(value);
        let keep_alive = manager.allocate(WorldKeepAliveUserdata { destructor, handle });

        self.resize_keep_alive();
        self.keep_alive[handle.slot() as usize] = keep_alive.index();

        (handle, keep_alive)
    }

    pub fn despawn(&mut self, handle: RawHandle) -> Option<T> {
        let value = self.arena.remove(handle);
        self.resize_keep_alive();

        value
    }

    pub fn slot_to_handle(&self, slot_idx: u32) -> Option<RawHandle> {
        self.arena.slot_to_handle(slot_idx)
    }

    pub fn upgrade(
        &mut self,
        manager: &mut WorldKeepAliveManager,
        handle: RawHandle,
    ) -> Option<KeepAlive> {
        _ = self.arena.get(handle)?;

        let keep_alive = self.keep_alive[handle.slot() as usize];
        let keep_alive = manager.upgrade(keep_alive);

        Some(keep_alive)
    }

    pub fn get(&self, handle: RawHandle) -> Option<&T> {
        self.arena.get(handle)
    }

    pub fn get_mut(&mut self, handle: RawHandle) -> Option<&mut T> {
        self.arena.get_mut(handle)
    }

    pub fn slot_count(&self) -> u32 {
        self.arena.slot_count()
    }

    fn resize_keep_alive(&mut self) {
        if self.keep_alive.len() == self.arena.slot_count() as usize {
            return;
        }

        self.keep_alive
            .resize(self.arena.slot_count() as usize, KeepAliveIndex::MAX);
    }
}

impl<T> ObjectArenaSimpleSpawn for DefaultObjectArena<T>
where
    T: Object<Arena = Self> + fmt::Debug,
{
    fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle> {
        let (arena, manager) = w.arena_and_manager_mut::<Self>();

        let (handle, keep_alive) = arena.spawn(
            manager,
            |handle, w| {
                let handle = <T::Handle>::from_raw(handle);

                <T::Handle>::invoke_pre_destructor(handle, w);

                w.arena_mut::<Self>().despawn(handle.raw());
            },
            value,
        );

        Strong::new(Self::Handle::from_raw(handle), keep_alive)
    }
}

impl<T> ObjectArena for DefaultObjectArena<T>
where
    T: Object<Arena = Self> + fmt::Debug,
{
    type Object = T;
    type Handle = T::Handle;

    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
        w.arena::<Self>().get(handle.raw())
    }

    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
        w.arena_mut::<Self>().get_mut(handle.raw())
    }

    fn as_strong_if_alive(handle: Self::Handle, w: W) -> Option<Strong<Self::Handle>> {
        let (arena, manager) = w.arena_and_manager_mut::<Self>();

        let keep_alive = arena.upgrade(manager, handle.raw())?;

        Some(Strong::new(handle, keep_alive))
    }

    fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle> {
        w.arena::<Self>()
            .slot_to_handle(slot_idx)
            .map(T::Handle::from_raw)
    }

    fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
        if let Some(alive) = handle.try_r(w) {
            alive.fmt(f)
        } else {
            f.write_str("<dangling>")
        }
    }
}

// === Traits === //

pub type ArenaForHandle<H> = <<H as Handle>::Object as Object>::Arena;

pub trait Destructor: Handle {
    fn pre_destroy(self, w: W);
}

pub trait Object: 'static + Sized + LateField<world_ns::WorldNs, Value = Self::Arena> {
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
    + ErasedHandle
    + TransparentWrapper<RawHandle>
{
    type Object: Object<Handle = Self>;

    fn invoke_pre_destructor(me: Self, w: W);

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

    fn try_r(self, w: Wr) -> Option<&Self::Object> {
        ArenaForHandle::<Self>::try_get(self, w)
    }

    fn try_m(self, w: W) -> Option<&mut Self::Object> {
        ArenaForHandle::<Self>::try_get_mut(self, w)
    }

    #[track_caller]
    fn r(self, w: Wr) -> &Self::Object {
        match self.try_r(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    #[track_caller]
    fn m(self, w: W) -> &mut Self::Object {
        match self.try_m(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    fn is_alive(self, w: Wr) -> bool {
        self.try_r(w).is_some()
    }

    #[track_caller]
    fn as_strong_if_alive(self, w: W) -> Option<Strong<Self>> {
        ArenaForHandle::<Self>::as_strong_if_alive(self, w)
    }

    #[track_caller]
    fn as_strong(self, w: W) -> Strong<Self> {
        match self.as_strong_if_alive(w) {
            Some(v) => v,
            None => panic!("attempted to upgrade dangling handle {self:?}"),
        }
    }

    fn debug(self, w: Wr) -> WorldDebug<'_, Self> {
        WorldDebug::new(self, w)
    }
}

// === Macros === //

#[doc(hidden)]
pub mod object_internals {
    use std::{marker::PhantomData, ops::Deref};

    use crate::DefaultObjectArena;

    use super::Destructor;

    pub use {
        super::rich_fmt::format_handle,
        crate::{Handle, Object, ObjectArena, RawArena, RawHandle, W, world_ns::WorldNs},
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

    pub type CustomArenaOrDefault<Value, CustomArena = DefaultObjectArena<Value>> =
        TakeSecond<Value, CustomArena>;

    pub trait PreDestroyDispatchTrait {
        fn exec_pre_destroy(&self, w: W);
    }

    pub struct PreDestroyDispatch<T>(PreDestroyDispatchInner<T>);

    impl<T> PreDestroyDispatch<T> {
        pub fn new(value: T) -> Self {
            Self(PreDestroyDispatchInner(value))
        }
    }

    impl<T> PreDestroyDispatchTrait for PreDestroyDispatch<T>
    where
        T: Destructor,
    {
        fn exec_pre_destroy(&self, w: W) {
            (self.0).0.pre_destroy(w);
        }
    }

    impl<T> Deref for PreDestroyDispatch<T> {
        type Target = PreDestroyDispatchInner<T>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    pub struct PreDestroyDispatchInner<T>(T);

    impl<T> PreDestroyDispatchTrait for PreDestroyDispatchInner<T> {
        fn exec_pre_destroy(&self, w: W) {
            _ = w;
        }
    }
}

#[macro_export]
macro_rules! object {
    ( $( $vis:vis $ty:ident $([$arena:ty])? ),*$(,)? ) => {$(
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
            $vis struct [<$ty Handle>]($crate::object_internals::RawHandle);

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

                fn invoke_pre_destructor(me: Self, w: $crate::object_internals::W) {
                    use $crate::object_internals::PreDestroyDispatchTrait as _;

                    $crate::object_internals::PreDestroyDispatch::new(me).exec_pre_destroy(w);
                }
            }
        }
    )*};
}
