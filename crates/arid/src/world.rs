use std::{any::Any, cell::Cell, fmt, ptr::NonNull};

use late_struct::{LateInstance, late_field, late_struct};

use crate::{KeepAliveManager, ObjectArena, RawHandle};

// === World === //

thread_local! {
    static WORLD_TLS: Cell<Option<NonNull<World>>> = const { Cell::new(None) };
}

pub type W<'a> = &'a mut World;
pub type Wr<'a> = &'a World;

#[derive(Default)]
pub struct World {
    inner: LateInstance<world_ns::WorldNs>,
}

pub(crate) mod world_ns {
    #[non_exhaustive]
    pub struct WorldNs;
}

late_struct!(world_ns::WorldNs => dyn Any);

impl fmt::Debug for World {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("World").finish_non_exhaustive()
    }
}

impl World {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn provide_tls<R>(&self, f: impl FnOnce() -> R) -> R {
        let _guard = scopeguard::guard(WORLD_TLS.replace(Some(NonNull::from(self))), |old| {
            WORLD_TLS.set(old);
        });

        f()
    }

    pub fn fetch_tls<R>(f: impl FnOnce(Option<&World>) -> R) -> R {
        f(WORLD_TLS.get().map(|v| unsafe { v.as_ref() }))
    }

    pub fn debug<T>(&self, value: T) -> WorldDebug<'_, T> {
        WorldDebug::new(value, self)
    }

    pub fn arena<T: ObjectArena>(&self) -> &T {
        self.inner.get::<T::Object>()
    }

    pub fn manager(&self) -> &WorldKeepAliveManager {
        self.inner.get::<WorldKeepAliveManager>()
    }

    pub fn manager_mut(&mut self) -> &mut WorldKeepAliveManager {
        self.inner.get_mut::<WorldKeepAliveManager>()
    }

    pub fn arena_mut<T: ObjectArena>(&mut self) -> &mut T {
        self.inner.get_mut::<T::Object>()
    }

    pub fn arena_and_manager_mut<T: ObjectArena>(
        &mut self,
    ) -> (&mut T, &mut WorldKeepAliveManager) {
        self.inner.get_two::<T::Object, WorldKeepAliveManager>()
    }

    pub fn flush(&mut self) {
        while let Some((_, condemned)) = self.manager_mut().take_condemned() {
            (condemned.destructor)(condemned.handle, self);
        }
    }
}

// === WorldDebug === //

// Not `#[must_use]` because these are often printed using `dbg!()`.
#[derive(Copy, Clone)]
pub struct WorldDebug<'a, T> {
    world: &'a World,
    value: T,
}

impl<'a, T> WorldDebug<'a, T> {
    pub fn new(value: T, w: Wr<'a>) -> Self {
        Self { value, world: w }
    }
}

impl<T: fmt::Debug> fmt::Debug for WorldDebug<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.world.provide_tls(|| self.value.fmt(f))
    }
}

// === KeepAlive === //

pub type WorldKeepAliveManager = KeepAliveManager<WorldKeepAliveUserdata>;

late_field!(WorldKeepAliveManager[world_ns::WorldNs]);

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct WorldKeepAliveUserdata {
    pub destructor: fn(handle: RawHandle, w: W),
    pub handle: RawHandle,
}
