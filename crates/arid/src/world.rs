use std::{cell::Cell, fmt, ptr::NonNull};

use late_struct::{LateInstance, late_field, late_struct};

use crate::ArenaManager;

thread_local! {
    static WORLD_TLS: Cell<Option<NonNull<World>>> = const { Cell::new(None) };
}

pub type W<'a> = &'a mut World;
pub type Wr<'a> = &'a World;

#[derive(Default)]
pub struct World {
    pub(crate) inner: LateInstance<world_ns::WorldNs>,
}

pub(crate) mod world_ns {
    #[non_exhaustive]
    pub struct WorldNs;
}

late_struct!(world_ns::WorldNs);

late_field!(ArenaManager<World>[world_ns::WorldNs]);

impl fmt::Debug for World {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("World").finish_non_exhaustive()
    }
}

impl World {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn flush(&mut self) {
        while let Some(condemned) = self.inner.get_mut::<ArenaManager<World>>().take_condemned() {
            condemned.process(self);
        }
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
}
