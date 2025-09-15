use std::{fmt, hash, mem};

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
        cell::RefCell,
        fmt::{self, Debug},
    };

    use rustc_hash::{FxBuildHasher, FxHashSet};

    use crate::{ArenaForHandle, ObjectArena as _, RawHandle, World};

    use super::Handle;

    thread_local! {
        static REENTRANT_DEBUGS: RefCell<FxHashSet<(RawHandle, TypeId)>> =
            const { RefCell::new(FxHashSet::with_hasher(FxBuildHasher) )};
    }

    #[must_use]
    fn reentrant_debug_guard<T: Handle>(handle: T) -> Option<impl Sized> {
        let was_inserted = REENTRANT_DEBUGS
            .with_borrow_mut(|set| set.insert((handle.raw(), TypeId::of::<T::Object>())));

        if !was_inserted {
            return None;
        }

        Some(scopeguard::guard((), move |()| {
            REENTRANT_DEBUGS
                .with_borrow_mut(|set| set.remove(&(handle.raw(), TypeId::of::<T::Object>())));
        }))
    }

    #[expect(missing_docs)] // (never exposed in the public API)
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

/// An [`ObjectArena`] which supports the [`Object::spawn`] method.
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
///
/// This trait is generally implemented on every object implementing the `ObjectArena` trait unless
/// the value's constructor needs additional arguments.
pub trait ObjectArenaSimpleSpawn: ObjectArena {
    /// Implements the [`Object::spawn`] operation.
    fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle>;
}

/// A custom arena managing a specific type of [`Object`].
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
pub trait ObjectArena: 'static + Default {
    /// The [`Object`] being managed by the arena.
    type Object: Object<Handle = Self::Handle, Arena = Self>;

    /// The [`Handle`] being managed by the arena.
    type Handle: Handle<Object = Self::Object>;

    /// Implements the [`Handle::try_r`] operation.
    fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object>;

    /// Implements the [`Handle::try_m`] operation.
    fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object>;

    /// Implements the [`Handle::as_strong_if_alive`] operation.
    fn as_strong_if_alive(handle: Self::Handle, w: W) -> Option<Strong<Self::Handle>>;

    /// Implements the [`Handle`]'s [`fmt::Debug`] operation.
    fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result;
}

/// The default implementation of [`ObjectArena`] used for [`Object`]s when no custom arena is
/// specified.
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
///
/// This type combines a [`RawArena`] managing values of type `T` with an out-of-band list of
/// [`KeepAliveIndex`]es to implement `ObjectArena`s operations. This type can be embedded in other
/// custom arenas which delegate their trait member implementations to this type to simplify the
/// creation of new arenas.
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
    /// Creates a new [`RawHandle`] and [`KeepAlive`] pair managed by the specified `manager`.
    ///
    /// The `destructor` function pointer is used as the `destructor` field in the
    /// [`WorldKeepAliveUserdata`] passed to the `manager` to create the returned [`KeepAlive`].
    /// This function pointer is called, for example, by [`World::flush`](crate::World::flush) once
    /// all remaining [`KeepAlive`]s to this value have been dropped, and it is up to the destructor
    /// to call [`DefaultObjectArena::despawn`] to ensure that the handle is properly marked as
    /// destroyed.
    pub fn spawn(
        &mut self,
        manager: &mut WorldKeepAliveManager,
        destructor: fn(handle: RawHandle, w: W),
        value: T,
    ) -> (RawHandle, KeepAlive) {
        let handle = self.arena.insert(value);
        let keep_alive = manager.allocate(WorldKeepAliveUserdata { destructor, handle });

        self.resize_keep_alive();
        self.keep_alive[handle.slot() as usize] = keep_alive.index();

        (handle, keep_alive)
    }

    /// Forwards to the internal arena's [`RawArena::slot_to_handle`] method.
    ///
    /// This method should be called in response to the `destructor` passed to
    /// [`DefaultObjectArena::spawn`] being called.
    ///
    /// Although it is possible to call this method against a given handle before that handle is
    /// confirmed destroyed by its associated [`KeepAliveManager`](crate::KeepAliveManager), doing
    /// so is a bit unconventional.
    pub fn despawn(&mut self, handle: RawHandle) -> Option<T> {
        let value = self.arena.remove(handle);
        self.resize_keep_alive();

        value
    }

    /// Attempts to produce a [`KeepAlive`] for a given [`RawHandle`] which has not yet been
    /// destroyed by [`DefaultObjectArena::despawn`]. Returns `None` if the `handle` is dangling.
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

    /// Forwards to the internal arena's [`RawArena::slot_to_handle`] method.
    pub fn slot_to_handle(&self, slot_idx: u32) -> Option<RawHandle> {
        self.arena.slot_to_handle(slot_idx)
    }

    /// Forwards to the internal arena's [`RawArena::get`] method.
    pub fn get(&self, handle: RawHandle) -> Option<&T> {
        self.arena.get(handle)
    }

    /// Forwards to the internal arena's [`RawArena::get_mut`] method.
    pub fn get_mut(&mut self, handle: RawHandle) -> Option<&mut T> {
        self.arena.get_mut(handle)
    }

    /// Forwards to the internal arena's [`RawArena::slot_count`] method.
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

    fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
        if let Some(alive) = handle.try_r(w) {
            alive.fmt(f)
        } else {
            f.write_str("<dangling>")
        }
    }
}

// === Traits === //

/// Fetches the [`Object::Arena`] associated type for a given type implementing [`Handle`].
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
pub type ArenaForHandle<H> = <<H as Handle>::Object as Object>::Arena;

/// Provides a destructor to a [`Handle`]'s associated [`Object`].
pub trait Destructor: Handle {
    /// Called right before the handle is destroyed.
    ///
    /// Although weak handles to the value are still valid at the point this method is called,
    /// attempts to "resurrect" the handle with [`Handle::as_strong`] will result in unspecified
    /// behavior.
    fn pre_destroy(self, w: W);
}

/// A value which can be allocated in `arid`'s object model.
///
/// This trait cannot be implemented manually and must be implemented through the
/// [`object!`](crate::object!) macro.
pub trait Object: 'static + Sized + LateField<world_ns::WorldNs, Value = Self::Arena> {
    /// The type of the [`ObjectArena`] in which the `Object` is stored.
    ///
    /// By default, if no custom arena is supplied to the [`object!`](crate::object!) macro, this
    /// type will be [`DefaultObjectArena`].
    type Arena: ObjectArena<Object = Self, Handle = Self::Handle>;

    /// The type of the [`Handle`] associated with the `Object`.
    ///
    /// This type is automatically generated at the site of the [`object!`](crate::object!) macro's
    /// expansion and will be named `<StructName>Handle`.
    type Handle: Handle<Object = Self>;

    /// Moves the value into the object's arena and returns a [`Strong`] handle to it.
    ///
    /// This method is only available if the [`Object::Arena`] implements
    /// [`ObjectArenaSimpleSpawn`]. The default arena, [`DefaultObjectArena`], implements this
    /// trait.
    #[must_use]
    fn spawn(self, w: W) -> Strong<Self::Handle>
    where
        Self::Arena: ObjectArenaSimpleSpawn,
    {
        Self::Arena::spawn(self, w)
    }
}

/// A weak handle to a value allocated within `arid`'s object model.
///
/// This trait cannot be implemented manually. Instead, a custom type named `<StructName>Handle`
/// implementing trait will be automatically generated at the site of the
/// [`object!`](crate::object!) macro's expansion.
///
/// Unlike many other smart-pointers in Rust, this handle is `Copy`able.
///
/// This handle does not contribute to the strong reference count of the pointee. To keep a value
/// in the arena alive, one can use the [`Strong`] handle wrapper, which is returned by the
/// [`Object::spawn`] method and can be obtained from a weak-handle using the [`Handle::as_strong`]
/// method. Do note, however, that objects without remaining strong handles will not be destroyed
/// until [`World::flush`](crate::World::flush) is called.
///
/// The behavior of `Handle`s associated with one [`World`](crate::World) when used against another
/// `World` is unspecified. Additionally, a given `Handle`'s value may be reused across multiple
/// `World`s.
///
/// Although handles implement the [`fmt::Debug`](fmt::Debug) trait, they will not perform
/// pretty-printing on the handle's underlying value unless the `World` owning the value is provided
/// over thread-local storage using either [`World::provide_tls`](crate::World::provide_tls) or the
/// [`WorldDebug`] wrapper obtained using [`Handle::debug`].
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
    /// A [`Handle`] which is always dangling.
    const DANGLING: Self = {
        assert!(mem::size_of::<RawHandle>() == mem::size_of::<Self>());
        assert!(mem::align_of::<RawHandle>() == mem::align_of::<Self>());

        unsafe {
            // Safety: provided by `TransparentWrapper<RawHandle>` bound.
            *(&RawHandle::DANGLING as *const RawHandle as *const Self)
        }
    };

    /// The type of the [`Object`] associated with the `Handle`.
    type Object: Object<Handle = Self>;

    /// Invokes the handle's custom destructor if the current handle implements the [`Destructor`]
    /// trait. Otherwise, the method does nothing.
    ///
    /// <div class="warning">
    /// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
    /// custom arena</a>.
    /// </div>
    fn invoke_pre_destructor(me: Self, w: W);

    /// Converts a [`RawHandle`] into this strongly-typed wrapper to the handle.
    ///
    /// <div class="warning">
    /// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
    /// custom arena</a>.
    /// </div>
    fn from_raw(raw: RawHandle) -> Self {
        TransparentWrapper::wrap(raw)
    }

    /// Fetches the [`RawHandle`] underlying this strongly-type wrapper to it.
    ///
    /// <div class="warning">
    /// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
    /// custom arena</a>.
    /// </div>
    fn raw(self) -> RawHandle {
        TransparentWrapper::peel(self)
    }

    /// Attempts to dereference the handle, returning an immutable reference to its associated
    /// [`Handle::Object`] if the handle is still alive or `None` if the value was destroyed.
    fn try_r(self, w: Wr<'_>) -> Option<&Self::Object> {
        ArenaForHandle::<Self>::try_get(self, w)
    }

    /// Attempts to dereference the handle, returning a mutable reference to its associated
    /// [`Handle::Object`] if the handle is still alive or `None` if the value was destroyed.
    fn try_m(self, w: W<'_>) -> Option<&mut Self::Object> {
        ArenaForHandle::<Self>::try_get_mut(self, w)
    }

    /// Dereferences the handle, returning an immutable reference to its associated
    /// [`Handle::Object`].
    ///
    /// Panics if the pointee was destroyed.
    #[track_caller]
    fn r(self, w: Wr<'_>) -> &Self::Object {
        match self.try_r(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    /// Dereferences the handle, returning a mutable reference to its associated [`Handle::Object`].
    ///
    /// Panics if the pointee was destroyed.
    #[track_caller]
    fn m(self, w: W<'_>) -> &mut Self::Object {
        match self.try_m(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    /// Returns whether the handle is still alive.
    fn is_alive(self, w: Wr) -> bool {
        self.try_r(w).is_some()
    }

    /// Attempts to convert the weak handle into its [`Strong`] counterpart, returning `None` if the
    /// handle has been destroyed.
    fn as_strong_if_alive(self, w: W) -> Option<Strong<Self>> {
        ArenaForHandle::<Self>::as_strong_if_alive(self, w)
    }

    /// Converts the weak handle into its [`Strong`] counterpart, panicking if the handle has been
    /// destroyed.
    #[track_caller]
    fn as_strong(self, w: W) -> Strong<Self> {
        match self.as_strong_if_alive(w) {
            Some(v) => v,
            None => panic!("attempted to upgrade dangling handle {self:?}"),
        }
    }

    /// Wraps the handle in a [`WorldDebug`] instance which, when printed using its [`fmt::Debug`]
    /// implementation, will pretty-print the value of the handle.
    fn debug(self, w: Wr<'_>) -> WorldDebug<'_, Self> {
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

/// Defines a new [`Object`] which can be allocated within `arid`'s object model.
///
/// `$vis` must match the visibility of the structure against which the [`Object`] trait is being
/// implemented.
///
/// `$ty` is the simple name—and not a path to!—the type against which the [`Object`] trait will
/// be implemented.
///
/// `$arena` is an optional type implementing the [`ObjectArena`] trait which indicates the value of
/// the [`Object::Arena`] associated type. This type indicates the custom arena in which the value
/// should be allocated. If omitted, this type will default to [`DefaultObjectArena`].
#[macro_export]
macro_rules! object {
    (
        $(
            $vis:vis $ty:ident $([$arena:ty])?
        ),*
        $(,)?
    ) => {$(
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
