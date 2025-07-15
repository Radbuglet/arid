use std::{fmt, mem, ops::Deref};

use bytemuck::TransparentWrapper;
use derive_where::derive_where;
use late_struct::{LateInstance, LateStruct};

use crate::{Handle, KeepAlive, RawHandle};

// === Strong === //

#[derive_where(Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Strong<T: Handle> {
    handle: T,
    #[derive_where(skip)]
    keep_alive: KeepAlive,
}

impl<T: Handle> Deref for Strong<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.handle
    }
}

impl<T: Handle> Strong<T> {
    pub fn new(handle: T, keep_alive: KeepAlive) -> Self {
        Self { handle, keep_alive }
    }

    pub fn into_keep_alive(me: Self) -> KeepAlive {
        me.keep_alive
    }

    pub fn keep_alive(me: &Self) -> &KeepAlive {
        &me.keep_alive
    }

    pub fn as_raw(self) -> T {
        self.handle
    }
}

// === MayDangle === //

#[derive_where(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct MayDangle<T: Handle> {
    handle: T,
}

impl<T: Handle> From<T> for MayDangle<T> {
    fn from(handle: T) -> Self {
        Self { handle }
    }
}

impl<T: Handle> MayDangle<T> {
    pub fn new(handle: T) -> Self {
        Self { handle }
    }

    pub fn get(self, w: &LateInstance<T::Struct>) -> Option<T> {
        self.handle.is_alive(w).then_some(self.handle)
    }

    #[track_caller]
    pub fn unwrap(self, w: &LateInstance<T::Struct>) -> T {
        assert!(
            self.handle.is_alive(w),
            "attempted to unwrap dangling handle {:?}",
            self.handle
        );

        self.handle
    }

    pub fn unwrap_unchecked(self) -> T {
        self.handle
    }
}

// === ErasedHandle === //

pub trait ErasedHandle: 'static + Send + Sync + fmt::Debug + TransparentWrapper<RawHandle> {
    type Struct: LateStruct;

    fn is_alive(&self, w: &LateInstance<Self::Struct>) -> bool;
}

impl<T: Handle> ErasedHandle for T {
    type Struct = T::Struct;

    fn is_alive(&self, w: &LateInstance<Self::Struct>) -> bool {
        (*self).is_alive(w)
    }
}

// === Erased === //

#[derive_where(Copy, Clone)]
pub struct Erased<T: ErasedHandle> {
    handle: RawHandle,
    unerase: fn(&RawHandle) -> &T,
}

impl<T: ErasedHandle> Erased<T> {
    pub fn new<V: Handle>(handle: V, unerase: fn(&V) -> &T) -> Self {
        let unerase = unsafe { mem::transmute::<fn(&V) -> &T, fn(&RawHandle) -> &T>(unerase) };

        Self::new_raw(handle.unwrap_raw(), unerase)
    }

    pub fn new_raw(handle: RawHandle, unerase: fn(&RawHandle) -> &T) -> Self {
        Self { handle, unerase }
    }
}
