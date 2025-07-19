use std::{
    cell::{Cell, UnsafeCell},
    fmt,
    marker::PhantomData,
    panic::Location,
    ptr::NonNull,
};

use thiserror::Error;

// === World === //

thread_local! {
    static WORLD_ACQUIRED: Cell<Option<&'static Location<'static>>> = const { Cell::new(None) };
}

thread_local! {
    static WORLD_IN_TLS: Cell<u64> = const { Cell::new(0) };
}

pub type W<'a> = &'a mut World;
pub type Wr<'a> = &'a World;

#[derive(Debug, Clone, Error)]
#[error("thread world already acquired at {location}")]
pub struct WorldAlreadyAcquired {
    pub location: &'static Location<'static>,
}

#[derive(Debug)]
pub struct World {
    _not_send_sync: PhantomData<*const ()>,
}

impl World {
    #[track_caller]
    pub fn try_acquire() -> Result<World, WorldAlreadyAcquired> {
        if let Some(location) = WORLD_ACQUIRED.get() {
            return Err(WorldAlreadyAcquired { location });
        }

        WORLD_ACQUIRED.set(Some(Location::caller()));

        Ok(Self {
            _not_send_sync: PhantomData,
        })
    }

    #[track_caller]
    pub fn acquire() -> Self {
        Self::try_acquire().unwrap()
    }

    pub unsafe fn fake_token() -> &'static mut Self {
        unsafe { NonNull::<World>::dangling().as_mut() }
    }

    pub fn provide_tls<R>(&self, f: impl FnOnce() -> R) -> R {
        WORLD_IN_TLS.set(WORLD_IN_TLS.get() + 1);

        let _guard = scopeguard::guard((), |()| {
            WORLD_IN_TLS.set(WORLD_IN_TLS.get() - 1);
        });

        f()
    }

    pub fn fetch_tls<R>(f: impl FnOnce(Option<&Self>) -> R) -> R {
        f((WORLD_IN_TLS.get() > 0).then(|| unsafe { &*World::fake_token() }))
    }
}

impl Drop for World {
    fn drop(&mut self) {
        WORLD_ACQUIRED.set(None);
    }
}

// === WorldCell === //

#[derive(Default)]
#[repr(transparent)]
pub struct WorldCell<T: ?Sized> {
    cell: UnsafeCell<T>,
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for WorldCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        World::fetch_tls(|w| {
            if let Some(w) = w {
                self.borrow(w).fmt(f)
            } else {
                f.debug_struct("WorldCell").finish_non_exhaustive()
            }
        })
    }
}

impl<T> WorldCell<T> {
    pub const fn new(value: T) -> Self {
        Self {
            cell: UnsafeCell::new(value),
        }
    }

    pub fn into_inner(self) -> T
    where
        T: Sized,
    {
        self.cell.into_inner()
    }
}

impl<T: ?Sized> WorldCell<T> {
    pub const fn get_ptr(&self) -> *mut T {
        self.cell.get()
    }

    pub const fn get_mut(&mut self) -> &mut T {
        self.cell.get_mut()
    }

    pub fn borrow<'a>(&'a self, w: Wr<'a>) -> &'a T {
        _ = w;

        unsafe { &*self.get_ptr() }
    }

    pub fn borrow_mut<'a>(&'a self, w: W<'a>) -> &'a mut T {
        _ = w;

        unsafe { &mut *self.get_ptr() }
    }
}
