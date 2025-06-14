use std::{
    any::{TypeId, type_name},
    fmt, mem,
    ops::Deref,
};

use derive_where::derive_where;
use thunderdome::Index;

use crate::entity::Handle;

// === ErasedHandle === //

pub unsafe trait ErasedHandle: 'static + fmt::Debug {
    fn raw(&self) -> Index;

    fn component_type_id(&self) -> TypeId;

    fn component_type_name(&self) -> &'static str;
}

unsafe impl<H: Handle> ErasedHandle for H {
    fn raw(&self) -> Index {
        (*self).raw()
    }

    fn component_type_id(&self) -> TypeId {
        TypeId::of::<H::Component>()
    }

    fn component_type_name(&self) -> &'static str {
        type_name::<H::Component>()
    }
}

// === Erased === //

#[derive_where(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Erased<T: ?Sized + ErasedHandle> {
    handle: Index,
    upcast: fn(&Index) -> &T,
}

impl<T: ?Sized + ErasedHandle> fmt::Debug for Erased<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        T::fmt(self, f)
    }
}

impl<T: ?Sized + ErasedHandle> Erased<T> {
    pub fn new<U: ErasedHandle>(handle: U, upcast: fn(&U) -> &T) -> Self {
        Self {
            handle: handle.raw(),
            upcast: unsafe {
                // Safety: `U` is `repr(transparent)` w.r.t `Index` and `U: Sized`.
                mem::transmute::<fn(&U) -> &T, fn(&Index) -> &T>(upcast)
            },
        }
    }

    pub fn try_downcast<U: Handle>(self) -> Option<U> {
        (self.component_type_id() == TypeId::of::<U::Component>()).then(|| U::wrap_raw(self.handle))
    }

    pub fn downcast<U: Handle>(self) -> U {
        assert_eq!(
            self.component_type_id(),
            TypeId::of::<U::Component>(),
            "expected handle of type `{}`, got `{}`",
            type_name::<U::Component>(),
            self.component_type_name()
        );

        U::wrap_raw(self.handle)
    }
}

impl<T: ?Sized + ErasedHandle> Deref for Erased<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        (self.upcast)(&self.handle)
    }
}

#[doc(hidden)]
pub mod erased_macro_internals {
    use super::{Erased, ErasedHandle};

    // We need a different argument order than `Erased::new` because `erased` takes in an arbitrary
    // token tree rather than an expression to improve rust-analyzer DX.
    pub fn new<T: ?Sized + ErasedHandle, U: ErasedHandle>(
        upcast: fn(&U) -> &T,
        handle: U,
    ) -> Erased<T> {
        Erased::new(handle, upcast)
    }
}

#[macro_export]
macro_rules! erased {
    ($($expr:tt)*) => {
        $crate::erased::erased_macro_internals::new(|v| v, $($expr)*)
    };
}

pub use erased;
