use std::{
    any::{TypeId, type_name},
    fmt, mem,
    ops::Deref,
    ptr,
};

use derive_where::derive_where;

use crate::{Handle, KeepAlive, RawHandle, W, WorldDebug, Wr};

// === Strong === //

/// A strong reference to a [`Handle`].
///
/// See [crate-level documentation on the object lifecycle](index.html#lifecycle) for details.
///
/// A strong reference to a `Handle` is usually obtained by calling [`Handle::as_strong`]. This
/// value [`Deref`]'s to the weak handle.
///
/// This is comprised of two parts: the handle itself (`T`) and the [`KeepAlive`] guard keeping the
/// value alive.
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
    /// Construct a `Strong` handle from its raw parts. Most users will usually call
    /// [`Handle::as_strong`] to upgrade a weak reference to this strong variant.
    ///
    /// <div class="warning">
    /// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
    /// custom arena</a>.
    /// </div>
    pub fn new(handle: T, keep_alive: KeepAlive) -> Self {
        Self { handle, keep_alive }
    }

    /// Extracts ownership of the [`KeepAlive`] keeping the handle alive.
    ///
    /// Note that this is an associated function—not a method.
    pub fn into_keep_alive(me: Self) -> KeepAlive {
        me.keep_alive
    }

    /// Extracts an immutable reference to the [`KeepAlive`] keeping the handle alive.
    ///
    /// Note that this is an associated function—not a method.
    pub fn keep_alive(me: &Self) -> &KeepAlive {
        &me.keep_alive
    }

    /// Extracts the weak handle `T`. Same as [`Deref::deref`].
    pub fn as_weak(&self) -> T {
        self.handle
    }
}

// === MayDangle === //

/// An advisory wrapper around a [`Handle`] of type `T` indicating that it may dangle.
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
    /// Wraps `handle` into a `MayDangle`.
    pub fn new(handle: T) -> Self {
        Self { handle }
    }

    /// Obtains the raw handle `T`, returning `None` if it is no longer alive.
    pub fn get(self, w: Wr) -> Option<T> {
        self.handle.is_alive(w).then_some(self.handle)
    }

    /// Asserts that the raw handle `T` is alive, panicking if it isn't.
    #[track_caller]
    pub fn unwrap(self, w: Wr) -> T {
        assert!(
            self.handle.is_alive(w),
            "attempted to unwrap dangling handle {:?}",
            self.handle
        );

        self.handle
    }

    /// Asserts that the raw handle `T` is alive, ignoring if it isn't. This is safe to call because
    /// attempts to dereference the returned handle will still panic.
    pub fn unwrap_unchecked(self) -> T {
        self.handle
    }
}

// === ErasedHandle === //

mod sealed {
    pub trait Sealed {}
}

/// A variant of the [`Handle`] trait which is [`dyn Trait` compatible].
///
/// # Safety
///
/// This trait can only be implemented for [`Handle`]s.
///
/// [`dyn Trait` compatible]: https://doc.rust-lang.org/reference/items/traits.html#r-items.traits.dyn-compatible
pub unsafe trait ErasedHandle: 'static + Send + Sync + fmt::Debug + sealed::Sealed {
    /// Alias to [`Handle::is_alive`].
    fn is_alive(&self, w: Wr) -> bool;

    /// Fetches the [`Object`](crate::Object)'s [`TypeId`].
    fn pointee_type(&self) -> TypeId;

    /// Fetches the [`Object`](crate::Object)'s [`type_name`].
    fn pointee_name(&self) -> &'static str;

    /// Fetches the [`Handle`](crate::Object)'s [`TypeId`].
    fn handle_type(&self) -> TypeId;

    /// Fetches the [`Handle`](crate::Object)'s [`type_name`].
    fn handle_name(&self) -> &'static str;

    #[doc(hidden)]
    fn obtain_keep_alive_if_alive(&self, w: W) -> Option<KeepAlive>;
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

    fn obtain_keep_alive_if_alive(&self, w: W) -> Option<KeepAlive> {
        (*self)
            .as_strong_if_alive(w)
            .map(|v| Strong::into_keep_alive(v))
    }
}

// === Erased === //

/// A `Copy`able weak reference to a [`Handle`] which has been unsized into a `dyn T` trait object.
///
/// See the [crate-level documentation on polymorphism](index.html#polymorphism) for details.
///
/// `T` must implement [`ErasedHandle`].
///
/// This can be thought of as a more efficient and ergonomic form of a `Box<dyn T>`.
#[derive_where(Copy, Clone)]
pub struct Erased<T: ?Sized + ErasedHandle> {
    handle: RawHandle,
    unerase: fn(&RawHandle) -> &T,
}

impl<T: ?Sized + ErasedHandle> fmt::Debug for Erased<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
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
    /// A raw constructor for `Erased` instances. See [`erase!`](crate::erase!) for a higher-level
    /// constructor for `Erased` instances.
    ///
    /// Takes a concrete `handle` of type `V` and a function pointer `unerase` to turn `&V` into
    /// `&T`. This function pointer is generally implemented using a simple [unsizing coercion].
    ///
    /// [unsizing coercion]: https://doc.rust-lang.org/reference/type-coercions.html#unsized-coercions
    pub fn new<V: Handle>(unerase: fn(&V) -> &T, handle: V) -> Self {
        let unerase = unsafe { mem::transmute::<fn(&V) -> &T, fn(&RawHandle) -> &T>(unerase) };

        Self {
            handle: handle.raw(),
            unerase,
        }
    }

    /// Fetches the [`RawHandle`] underlying the [`Handle`] used to construct this handle.
    ///
    /// <div class="warning">
    /// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
    /// custom arena</a>.
    /// </div>
    pub fn raw(self) -> RawHandle {
        self.handle
    }

    /// Attempts to convert the weak handle into its [`StrongErased`] counterpart, returning `None`
    /// if the handle has been destroyed.
    pub fn as_strong_if_alive(self, w: W) -> Option<StrongErased<T>> {
        self.obtain_keep_alive_if_alive(w)
            .map(|keep_alive| StrongErased {
                handle: self.handle,
                keep_alive,
                unerase: self.unerase,
            })
    }

    /// Converts the weak handle into its [`StrongErased`] counterpart, panicking if the handle has
    /// been destroyed.
    #[track_caller]
    pub fn as_strong(self, w: W) -> StrongErased<T> {
        match self.as_strong_if_alive(w) {
            Some(v) => v,
            None => panic!("attempted to upgrade dangling handle {self:?}"),
        }
    }

    /// Attempts to downcast the erased handle into a concrete handle of type `V`, returning `None`
    /// on type mismatch.
    pub fn try_downcast<V: Handle>(self) -> Option<V> {
        (self.pointee_type() == TypeId::of::<V::Object>()).then(|| V::from_raw(self.raw()))
    }

    /// Downcasts the erased handle into a concrete handle of type `V`, panicking on type mismatch.
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

    /// Alias to [`Handle::debug`].
    pub fn debug(self, w: Wr<'_>) -> WorldDebug<'_, Erased<T>> {
        WorldDebug::new(self, w)
    }
}

impl<T: ?Sized + ErasedHandle> Deref for Erased<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        (self.unerase)(&self.handle)
    }
}

/// Constructs a [`Erased`] instance from the specified `$value` expression.
///
/// `$value` must have a value of type [`Handle`] which can perform an [unsizing coercion] into
/// `Erased<T>`'s trait object of type `T`.
///
/// The first form, which takes in an `as $ty,` prefix, allows the erased type of the handle
/// (e.g. `dyn Foo`) to be specified.
///
/// [unsizing coercion]: https://doc.rust-lang.org/reference/type-coercions.html#unsized-coercions
#[macro_export]
macro_rules! erase {
    (as $ty:ty, $($value:tt)*) => {
        $crate::Erased::<$ty>::new(|v| v, $($value)*)
    };
    ($($value:tt)*) => {
        $crate::Erased::new(|v| v, $($value)*)
    };
}

// === StrongErased === //

/// A strong reference to a [`Handle`] which has been unsized into a `dyn T` trait object.
///
/// See the [crate-level documentation on polymorphism](index.html#polymorphism) for details.
///
/// Just as [`Strong`] is a strong version of a weak `Handle` reference, so to does [`StrongErased`]
/// serve as a strong version of the weak [`Erased`] handle.
///
/// `T` must implement [`ErasedHandle`].
///
/// This is comprised of two parts: the handle itself and the [`KeepAlive`] guard keeping the value
/// alive.
///
/// This can be thought of as a more efficient and ergonomic form of a `Box<dyn T>`.
#[derive_where(Clone)]
pub struct StrongErased<T: ?Sized + ErasedHandle> {
    handle: RawHandle,
    keep_alive: KeepAlive,
    unerase: fn(&RawHandle) -> &T,
}

impl<T: ?Sized + ErasedHandle> fmt::Debug for StrongErased<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
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
    /// A raw constructor for `StrongErased` instances. See [`erase_strong!`](crate::erase_strong!)
    /// for a higher-level constructor for `StrongErased` instances.
    ///
    /// Takes a concrete `handle` of type `Strong<V>` and a function pointer `unerase` to turn `&V`
    /// into `&T`. This function pointer is generally implemented using a simple [unsizing coercion].
    ///
    /// [unsizing coercion]: https://doc.rust-lang.org/reference/type-coercions.html#unsized-coercions
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

    /// Fetches the [`RawHandle`] underlying the [`Handle`] used to construct this handle.
    ///
    /// <div class="warning">
    /// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
    /// custom arena</a>.
    /// </div>
    pub fn raw(&self) -> RawHandle {
        self.handle
    }

    /// Extracts ownership of the [`KeepAlive`] keeping the handle alive.
    pub fn into_keep_alive(self) -> KeepAlive {
        self.keep_alive
    }

    /// Extracts an immutable reference to the [`KeepAlive`] keeping the handle alive.
    pub fn keep_alive(&self) -> &KeepAlive {
        &self.keep_alive
    }

    /// Produces a weak [`Erased`] reference to the pointee.
    pub fn as_weak(&self) -> Erased<T> {
        Erased {
            handle: self.raw(),
            unerase: self.unerase,
        }
    }

    /// Attempts to downcast the erased handle into a strong concrete handle of type `Strong<V>`,
    /// returning `Err` with the original strong erased handle on type mismatch.
    ///
    /// This method takes ownership of `self` to avoid incrementing the strong reference-count of
    /// the handle.
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

    /// Attempts to downcast the erased handle into a weak concrete handle of type `V`, returning
    /// `None` on type mismatch.
    ///
    /// Unlike [`StrongErased::try_downcast`], this method borrows `self` rather than taking
    /// ownership of it since its return value doesn't need its own reference count.
    pub fn try_downcast_ref<V: Handle>(&self) -> Option<V> {
        self.as_weak().try_downcast::<V>()
    }

    /// Downcasts the erased handle into a strong concrete handle of type `Strong<V>`, panicking on
    /// type mismatch.
    ///
    /// This method takes ownership of `self` to avoid incrementing the strong reference-count of
    /// the handle.
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

    /// Downcasts the erased handle into a weak concrete handle of type `V`, panicking on type
    /// mismatch.
    ///
    /// Unlike [`StrongErased::downcast`], this method borrows `self` rather than taking ownership
    /// of it since its return value doesn't need its own reference count.
    #[track_caller]
    pub fn downcast_ref<V: Handle>(&self) -> V {
        self.as_weak().downcast::<V>()
    }

    /// Alias to [`Handle::debug`].
    pub fn debug<'w>(&self, w: Wr<'w>) -> WorldDebug<'w, Erased<T>> {
        WorldDebug::new(self.as_weak(), w)
    }
}

impl<T: ?Sized + ErasedHandle> Deref for StrongErased<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        (self.unerase)(&self.handle)
    }
}

/// Constructs a [`StrongErased`] instance from the specified `$value` expression.
///
/// `$value` must have a value of type [`Strong`] whose wrapped handle can perform an
/// [unsizing coercion] into `StrongErased<T>`'s trait object of type `T`.
///
/// The first form, which takes in an `as $ty,` prefix, allows the erased type of the handle
/// (e.g. `dyn Foo`) to be specified.
///
/// [unsizing coercion]: https://doc.rust-lang.org/reference/type-coercions.html#unsized-coercions
#[macro_export]
macro_rules! erase_strong {
    (as $ty:ty, $($value:tt)*) => {
        $crate::StrongErased::<$ty>::new(|v| v, $($value)*)
    };
    ($($value:tt)*) => {
        $crate::StrongErased::new(|v| v, $($value)*)
    };
}
