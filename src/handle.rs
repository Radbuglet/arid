use std::{fmt, hash, mem::ManuallyDrop, thread::LocalKey};

use bytemuck::TransparentWrapper;
use derive_where::derive_where;

use crate::{Arena, KeepAlive, RawHandle, Strong, W, World, WorldCell, Wr};

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
            // SAFETY: the `REENTRANT_DEBUGS` TLS item is only ever accessed in this module
            // non-reentrantly.
            unsafe { &mut *set.get() }.insert((handle.raw_handle(), TypeId::of::<T::Component>()))
        });

        if !was_inserted {
            return None;
        }

        Some(scopeguard::guard((), move |()| {
            REENTRANT_DEBUGS.with(|set| {
                // SAFETY: the `REENTRANT_DEBUGS` TLS item is only ever accessed in this module
                // non-reentrantly.
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

// === ArenaTls === //

#[derive_where(Copy, Clone)]
pub struct ArenaTls<T: Component> {
    tls: &'static LocalKey<ArenaTlsPointee<T>>,
}

impl<T: Component> ArenaTls<T> {
    pub const fn wrap(tls: &'static LocalKey<ArenaTlsPointee<T>>) -> Self {
        Self { tls }
    }

    pub fn insert(self, value: T, w: W) -> (RawHandle, KeepAlive) {
        self.tls
            .with(|arena| arena.inner.borrow_mut(w).insert(value))
    }

    pub fn upgrade(self, handle: RawHandle, w: W) -> Option<KeepAlive> {
        self.tls
            .with(|arena| arena.inner.borrow_mut(w).upgrade(handle))
    }

    pub fn get<'a>(self, handle: RawHandle, w: Wr<'a>) -> Option<&'a T> {
        self.tls.with(|arena| {
            arena
                .inner
                .borrow(w)
                .get(handle)
                // SAFETY: This reference is derived from a `Vec` which is only dropped if the
                // `World` be acquired exclusively. This means that this borrow must end before the
                // vector is dropped.
                .map(|v| unsafe { &*(v as *const T) })
        })
    }

    pub fn get_mut<'a>(self, handle: RawHandle, w: W<'a>) -> Option<&'a mut T> {
        self.tls.with(|arena| {
            arena
                .inner
                .borrow_mut(w)
                .get_mut(handle)
                // SAFETY: This reference is derived from a `Vec` which is only dropped if the
                // `World` be acquired exclusively. This means that this borrow must end before the
                // vector is dropped.
                .map(|v| unsafe { &mut *(v as *mut T) })
        })
    }
}

pub struct ArenaTlsPointee<T: Component> {
    inner: ManuallyDrop<WorldCell<Arena<T>>>,
}

impl<T: Component> ArenaTlsPointee<T> {
    #[expect(clippy::new_without_default)]
    pub const fn new() -> Self {
        Self {
            inner: ManuallyDrop::new(WorldCell::new(Arena::new(Self::destructor))),
        }
    }

    fn destructor(handle: RawHandle, w: W) {
        T::tls().tls.with(|arena| {
            _ = arena.inner.borrow_mut(w).try_commit_removal(handle);
        });
    }
}

impl<T: Component> Drop for ArenaTlsPointee<T> {
    fn drop(&mut self) {
        if let Err(err) = World::try_acquire() {
            panic!("failed to acquire world token while dropping thread's TLS: {err:?}");
        }

        // SAFETY: we can drop this value at most once in this destructor.
        unsafe { ManuallyDrop::drop(&mut self.inner) }
    }
}

// === Traits === //

pub trait Component: 'static + Sized + fmt::Debug {
    type Handle: Handle<Component = Self>;

    fn tls() -> ArenaTls<Self>;

    #[must_use]
    fn spawn(self, w: W) -> Strong<Self::Handle> {
        let (handle, keep_alive) = Self::tls().insert(self, w);

        Strong::new(Self::Handle::wrap_raw(handle), keep_alive)
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
    type Component: Component<Handle = Self>;

    fn tls() -> ArenaTls<Self::Component> {
        Self::Component::tls()
    }

    fn wrap_raw(raw: RawHandle) -> Self {
        TransparentWrapper::wrap(raw)
    }

    fn raw_handle(self) -> RawHandle {
        TransparentWrapper::peel(self)
    }

    fn try_get(self, w: Wr<'_>) -> Option<&Self::Component> {
        Self::tls().get(self.raw_handle(), w)
    }

    fn try_get_mut(self, w: W<'_>) -> Option<&mut Self::Component> {
        Self::tls().get_mut(self.raw_handle(), w)
    }

    #[track_caller]
    fn get(self, w: Wr<'_>) -> &Self::Component {
        match self.try_get(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    #[track_caller]
    fn get_mut(self, w: W) -> &mut Self::Component {
        match self.try_get_mut(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
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
    fn as_strong_if_alive(self, w: W) -> Option<Strong<Self>> {
        Self::tls()
            .upgrade(self.raw_handle(), w)
            .map(|keep_alive| Strong::new(self, keep_alive))
    }

    #[track_caller]
    fn as_strong(self, w: W) -> Strong<Self> {
        match self.as_strong_if_alive(w) {
            Some(v) => v,
            None => panic!("attempted to upgrade dangling handle {self:?}"),
        }
    }

    #[must_use]
    fn debug<'a>(self, w: Wr<'a>) -> DebugHandle<'a, Self> {
        DebugHandle {
            world: w,
            handle: self,
        }
    }
}

// === Macros === //

#[doc(hidden)]
pub mod component_internals {
    pub use {
        crate::{ArenaTls, ArenaTlsPointee, Component, Handle, RawHandle, format_handle},
        bytemuck::TransparentWrapper,
        paste::paste,
        std::{
            clone::Clone,
            cmp::{Eq, Ord, PartialEq, PartialOrd},
            fmt,
            hash::Hash,
            marker::Copy,
            thread_local,
        },
    };
}

#[macro_export]
macro_rules! component {
    (
        $( $ty:ident $([$meta:ty])? ),*$(,)? in $namespace:ty
    ) => {$(
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

            impl $crate::component_internals::Component for $ty {
                type Handle = [<$ty Handle>];

                fn tls() -> $crate::component_internals::ArenaTls<$ty> {
                    $crate::component_internals::thread_local! {
                        static ARENA: $crate::component_internals::ArenaTlsPointee<$ty>
                            = const { $crate::component_internals::ArenaTlsPointee::<$ty>::new() };
                    }

                    $crate::component_internals::ArenaTls::wrap(&ARENA)
                }
            }

            impl $crate::component_internals::Handle for [<$ty Handle>] {
                type Component = $ty;
            }
        }
    )*};
}
