use std::{
    any::{TypeId, type_name},
    fmt, mem,
    ops::Deref,
    ptr,
};

use derive_where::derive_where;

use crate::{Handle, KeepAlive, RawHandle, Wr};

// === Strong === //

#[derive_where(Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Strong<T: Handle> {
    handle: T,
    #[derive_where(skip)]
    keep_alive: KeepAlive,
}

impl<T: Handle> fmt::Debug for Strong<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.handle.fmt(f)
    }
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

impl<T: Handle> fmt::Debug for MayDangle<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.handle.fmt(f)
    }
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

    pub fn get(self, w: Wr) -> Option<T> {
        self.handle.is_alive(w).then_some(self.handle)
    }

    #[track_caller]
    pub fn unwrap(self, w: Wr) -> T {
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

mod sealed {
    pub trait Sealed {}
}

/// ## Safety
///
/// This trait can only be implemented for [`Handle`]s.
///
pub unsafe trait ErasedHandle: 'static + Send + Sync + fmt::Debug + sealed::Sealed {
    fn is_alive(&self, w: Wr) -> bool;

    fn pointee_type(&self) -> TypeId;

    fn pointee_name(&self) -> &'static str;

    fn handle_type(&self) -> TypeId;

    fn handle_name(&self) -> &'static str;
}

impl<T: Handle> sealed::Sealed for T {}

unsafe impl<T: Handle> ErasedHandle for T {
    fn is_alive(&self, w: Wr) -> bool {
        (*self).is_alive(w)
    }

    fn pointee_type(&self) -> TypeId {
        TypeId::of::<T::Object>()
    }

    fn pointee_name(&self) -> &'static str {
        type_name::<T::Object>()
    }

    fn handle_type(&self) -> TypeId {
        TypeId::of::<T>()
    }

    fn handle_name(&self) -> &'static str {
        type_name::<T>()
    }
}

// === Erased === //

#[derive_where(Copy, Clone)]
pub struct Erased<T: ?Sized + ErasedHandle> {
    handle: RawHandle,
    unerase: fn(&RawHandle) -> &T,
}

impl<T: ?Sized + ErasedHandle> fmt::Debug for Erased<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.handle.fmt(f)
    }
}

impl<T: ?Sized + ErasedHandle> Eq for Erased<T> {}

impl<T: ?Sized + ErasedHandle> PartialEq for Erased<T> {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
            && (ptr::fn_addr_eq(self.unerase, other.unerase)
                || (*self).pointee_type() == (*other).pointee_type())
    }
}

impl<T: ?Sized + ErasedHandle> Erased<T> {
    pub fn new<V: Handle>(unerase: fn(&V) -> &T, handle: V) -> Self {
        let unerase = unsafe { mem::transmute::<fn(&V) -> &T, fn(&RawHandle) -> &T>(unerase) };

        Self {
            handle: handle.raw(),
            unerase,
        }
    }

    pub fn raw(self) -> RawHandle {
        self.handle
    }

    pub fn try_downcast<V: Handle>(self) -> Option<V> {
        (self.pointee_type() == TypeId::of::<V::Object>()).then(|| V::from_raw(self.raw()))
    }

    #[track_caller]
    pub fn downcast<V: Handle>(self) -> V {
        match self.try_downcast::<V>() {
            Some(v) => v,
            None => panic!(
                "attempted to downcast value of type {} into {}",
                self.pointee_name(),
                type_name::<V::Object>(),
            ),
        }
    }
}

impl<T: ?Sized + ErasedHandle> Deref for Erased<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        (self.unerase)(&self.handle)
    }
}

#[macro_export]
macro_rules! erase {
    (as $ty:ty, $($value:tt)*) => {
        $crate::Erased::<$ty>::new(|v| v, $($value)*)
    };
    (as $($ignored:tt)*) => {
        ::core::compile_error!("invalid syntax; expected `as $ty:ty, $expr:expr`");
    };
    ($($value:tt)*) => {
        $crate::Erased::new(|v| v, $($value)*)
    };
}

// === StrongErased === //

#[derive_where(Clone)]
pub struct StrongErased<T: ?Sized + ErasedHandle> {
    handle: RawHandle,
    keep_alive: KeepAlive,
    unerase: fn(&RawHandle) -> &T,
}

impl<T: ?Sized + ErasedHandle> fmt::Debug for StrongErased<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.handle.fmt(f)
    }
}

impl<T: ?Sized + ErasedHandle> Eq for StrongErased<T> {}

impl<T: ?Sized + ErasedHandle> PartialEq for StrongErased<T> {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
            && (ptr::fn_addr_eq(self.unerase, other.unerase)
                || (*self).pointee_type() == (*other).pointee_type())
    }
}

impl<T: ?Sized + ErasedHandle> StrongErased<T> {
    pub fn new<V: Handle>(unerase: fn(&V) -> &T, handle: Strong<V>) -> Self {
        let unerase = unsafe { mem::transmute::<fn(&V) -> &T, fn(&RawHandle) -> &T>(unerase) };

        let raw = handle.raw();
        let keep_alive = Strong::into_keep_alive(handle);

        Self {
            handle: raw,
            keep_alive,
            unerase,
        }
    }

    pub fn raw(&self) -> RawHandle {
        self.handle
    }

    pub fn keep_alive(&self) -> &KeepAlive {
        &self.keep_alive
    }

    pub fn downgrade(&self) -> Erased<T> {
        Erased {
            handle: self.raw(),
            unerase: self.unerase,
        }
    }

    pub fn try_downcast<V: Handle>(self) -> Result<Strong<V>, Self> {
        if self.pointee_type() == TypeId::of::<V::Object>() {
            Ok(Strong {
                handle: V::from_raw(self.raw()),
                keep_alive: self.keep_alive,
            })
        } else {
            Err(self)
        }
    }

    pub fn try_downcast_ref<V: Handle>(&self) -> Option<V> {
        self.downgrade().try_downcast::<V>()
    }

    #[track_caller]
    pub fn downcast<V: Handle>(self) -> Strong<V> {
        match self.try_downcast::<V>() {
            Ok(v) => v,
            Err(me) => panic!(
                "attempted to downcast value of type {} into {}",
                me.pointee_name(),
                type_name::<V::Object>(),
            ),
        }
    }

    #[track_caller]
    pub fn downcast_ref<V: Handle>(&self) -> V {
        self.downgrade().downcast::<V>()
    }
}

impl<T: ?Sized + ErasedHandle> Deref for StrongErased<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        (self.unerase)(&self.handle)
    }
}

#[macro_export]
macro_rules! erase_strong {
    (as $ty:ty, $($value:tt)*) => {
        $crate::StrongErased::<$ty>::new(|v| v, $($value)*)
    };
    (as $($ignored:tt)*) => {
        ::core::compile_error!("invalid syntax; expected `as $ty:ty, $expr:expr`");
    };
    ($($value:tt)*) => {
        $crate::StrongErased::new(|v| v, $($value)*)
    };
}
