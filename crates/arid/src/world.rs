use std::{
    cell::{self, Cell, RefCell},
    fmt, mem,
    ptr::NonNull,
};

use late_struct::{LateField, LateInstance, late_struct};

use crate::entity::guard_entity_reentrancy_checks;

pub type W<'a> = &'a mut World;
pub type Wr<'a> = &'a World;

#[derive(Debug, Default)]
#[repr(transparent)]
pub struct World {
    pub raw: LateInstance<Self>,
}

thread_local! {
    static WORLD_TLS: Cell<Option<NonNull<WorldTlsState>> >= const { Cell::new(None) };
}

struct WorldTlsState(RefCell<NonNull<World>>);

late_struct!(World);

impl World {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn wrap_ref(raw: &LateInstance<Self>) -> &Self {
        unsafe { &*(raw as *const LateInstance<Self> as *const Self) }
    }

    pub fn wrap_mut(raw: &mut LateInstance<Self>) -> &mut Self {
        unsafe { &mut *(raw as *mut LateInstance<Self> as *mut Self) }
    }

    pub fn resource<F: LateField<Self>>(&self) -> &F::Value {
        self.raw.get::<F>()
    }

    pub fn resource_mut<F: LateField<Self>>(&mut self) -> &mut F::Value {
        self.raw.get_mut::<F>()
    }

    pub fn resources_two<F, G>(&mut self) -> (&mut F::Value, &mut G::Value)
    where
        F: LateField<Self>,
        G: LateField<Self>,
    {
        self.raw.get_two::<F, G>()
    }

    pub fn resource_ptr<F: LateField<Self>>(&self) -> NonNull<F::Value> {
        self.raw.get_ptr::<F>()
    }

    fn provide_tls_state<R>(state: WorldTlsState, f: impl FnOnce() -> R) -> R {
        let old = WORLD_TLS.replace(Some(NonNull::from(&state)));
        let _restore_guard = scopeguard::guard(old, |old| {
            WORLD_TLS.set(old);
        });

        f()
    }

    fn try_fetch_tls_state<R>(f: impl FnOnce(Option<&WorldTlsState>) -> R) -> R {
        f(WORLD_TLS.get().map(|v| unsafe { v.as_ref() }))
    }

    fn fetch_tls_state<R>(f: impl FnOnce(&WorldTlsState) -> R) -> R {
        Self::try_fetch_tls_state(|state| f(state.expect("no world provided")))
    }

    pub fn provide_tls_ref<R>(&self, f: impl FnOnce() -> R) -> R {
        let new_state = WorldTlsState(RefCell::new(NonNull::from(self)));
        mem::forget(new_state.0.borrow()); // (ensure that the cell cannot be borrowed mutably)

        Self::provide_tls_state(new_state, f)
    }

    pub fn provide_tls_mut<R>(&mut self, f: impl FnOnce() -> R) -> R {
        let new_state = WorldTlsState(RefCell::new(NonNull::from(self)));
        // (this cell can be borrowed both mutably and immutably)

        Self::provide_tls_state(new_state, f)
    }

    pub fn try_fetch_tls_ref<R>(
        f: impl FnOnce(Option<Result<&World, cell::BorrowError>>) -> R,
    ) -> R {
        Self::try_fetch_tls_state(|state| {
            f(state.map(|state| state.0.try_borrow().map(|v| unsafe { v.as_ref() })))
        })
    }

    pub fn try_fetch_tls_mut<R>(
        f: impl FnOnce(Option<Result<&mut World, cell::BorrowMutError>>) -> R,
    ) -> R {
        Self::try_fetch_tls_state(|state| {
            f(state.map(|state| state.0.try_borrow_mut().map(|mut v| unsafe { v.as_mut() })))
        })
    }

    pub fn fetch_tls_ref<R>(f: impl FnOnce(&World) -> R) -> R {
        Self::fetch_tls_state(|state| f(unsafe { state.0.borrow().as_ref() }))
    }

    pub fn fetch_tls_mut<R>(f: impl FnOnce(&mut World) -> R) -> R {
        Self::fetch_tls_state(|state| f(unsafe { state.0.borrow_mut().as_mut() }))
    }
}

// === WorldDebug === //

impl World {
    pub fn debug<T>(&self, target: T) -> WorldDebug<'_, T> {
        WorldDebug {
            world: self,
            target,
        }
    }
}

#[derive(Copy, Clone)]
pub struct WorldDebug<'a, T> {
    pub world: &'a World,
    pub target: T,
}

impl<T: fmt::Debug> fmt::Debug for WorldDebug<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _guard = guard_entity_reentrancy_checks();
        self.world.provide_tls_ref(|| self.target.fmt(f))
    }
}
