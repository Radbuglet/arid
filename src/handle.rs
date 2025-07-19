use std::{fmt, hash};

use bytemuck::TransparentWrapper;
use derive_where::derive_where;
use late_struct::{LateField, LateInstance, LateStruct};

use crate::{Arena, ArenaManager, RawHandle, Strong};

// === DebugHandle === //

mod rich_fmt {
    use std::{
        any::{TypeId, type_name},
        cell::UnsafeCell,
        fmt::{self, Debug},
        ptr::NonNull,
    };

    use late_struct::{LateInstance, LateStruct};
    use rustc_hash::{FxBuildHasher, FxHashSet};

    use crate::RawHandle;

    use super::Handle;

    thread_local! {
        static FMT_CONTEXT: UnsafeCell<Option<RichFmtCtx>> = const { UnsafeCell::new(None) };
        static REENTRANT_DEBUGS: UnsafeCell<FxHashSet<(RawHandle, TypeId)>> =
            const { UnsafeCell::new(FxHashSet::with_hasher(FxBuildHasher) )};
    }

    struct RichFmtCtx {
        structure: NonNull<()>,
        structure_ty: TypeId,
    }

    pub fn with_fmt_context<T, R>(structure: &LateInstance<T>, f: impl FnOnce() -> R) -> R
    where
        T: LateStruct,
    {
        let _guard = FMT_CONTEXT.with(|cx| {
            let old_cx = unsafe { &mut *cx.get() }.replace(RichFmtCtx {
                structure: NonNull::from(structure).cast(),
                structure_ty: TypeId::of::<T>(),
            });

            scopeguard::guard(old_cx, |old_cx| {
                FMT_CONTEXT.with(|cx| {
                    unsafe { *cx.get() = old_cx };
                });
            })
        });

        f()
    }

    #[must_use]
    fn reentrant_debug_guard<T: Handle>(handle: T) -> Option<impl Sized> {
        let was_inserted = REENTRANT_DEBUGS.with(|set| {
            unsafe { &mut *set.get() }.insert((handle.raw_handle(), TypeId::of::<T::Component>()))
        });

        if !was_inserted {
            return None;
        }

        Some(scopeguard::guard((), move |()| {
            REENTRANT_DEBUGS.with(|set| {
                unsafe { &mut *set.get() }
                    .remove(&(handle.raw_handle(), TypeId::of::<T::Component>()))
            });
        }))
    }

    pub fn format_handle<T: Handle>(f: &mut fmt::Formatter<'_>, handle: T) -> fmt::Result {
        FMT_CONTEXT.with(|cx| {
            f.write_str(type_name::<T::Component>())?;
            handle.raw_handle().fmt(f)?;

            if let &Some(RichFmtCtx {
                structure,
                structure_ty,
            }) = unsafe { &*cx.get() }
                && structure_ty == TypeId::of::<T::Struct>()
                && let Some(_reentrancy_guard) = reentrant_debug_guard(handle)
            {
                f.write_str(": ")?;

                if let Some(alive) =
                    handle.try_get(unsafe { structure.cast::<LateInstance<T::Struct>>().as_ref() })
                {
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
    pub structure: &'a LateInstance<T::Struct>,
    pub handle: T,
}

impl<T: Handle> fmt::Debug for DebugHandle<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        with_fmt_context(self.structure, || self.handle.fmt(f))
    }
}

// === LateStructHasArenaManager === //

pub trait StructHasArenaManager: LateStruct {
    type ManagerField: LateField<Self, Value = ArenaManager<LateInstance<Self>>>;
}

impl<S> StructHasArenaManager for S
where
    S: LateStruct,
    ArenaManager<LateInstance<S>>: LateField<S, Value = ArenaManager<LateInstance<S>>>,
{
    type ManagerField = ArenaManager<LateInstance<S>>;
}

pub trait WorldFlushExt {
    fn flush(&mut self);
}

impl<S: StructHasArenaManager> WorldFlushExt for LateInstance<S> {
    fn flush(&mut self) {
        loop {
            let Some(condemned) = self.get_mut::<S::ManagerField>().take_condemned() else {
                break;
            };

            condemned.process(self);
        }
    }
}

// === Meta === //

#[derive_where(Debug, Default)]
pub struct ComponentArena<T: Component> {
    pub arena: Arena<ComponentSlot<T>, LateInstance<T::Struct>>,
    pub meta: T::MetaArena,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct ComponentSlot<T: Component> {
    pub value: T,
    pub meta: T::Meta,
}

pub trait Meta<T: Handle>: Sized + 'static {
    type ArenaAnnotation: 'static + Default + fmt::Debug;

    fn destroy(handle: T, w: &mut LateInstance<T::Struct>);

    fn destroy_raw(handle: RawHandle, w: &mut LateInstance<T::Struct>) {
        Self::destroy(T::wrap(handle), w);
    }
}

impl<T: Handle> Meta<T> for () {
    type ArenaAnnotation = ();

    fn destroy(handle: T, w: &mut LateInstance<T::Struct>) {
        w.get_mut::<T::Component>()
            .arena
            .remove_now(handle.raw_handle());
    }
}

// === Traits === //

pub trait Component:
    'static + Sized + fmt::Debug + LateField<Self::Struct, Value = ComponentArena<Self>>
{
    type Struct: StructHasArenaManager;
    type MetaArena: 'static + Default + fmt::Debug;
    type Meta: Meta<Self::Handle, ArenaAnnotation = Self::MetaArena>;
    type Handle: Handle<
            Struct = Self::Struct,
            MetaArena = Self::MetaArena,
            Meta = Self::Meta,
            Component = Self,
        >;

    #[must_use]
    fn spawn(self, w: &mut LateInstance<Self::Struct>) -> Strong<Self::Handle>
    where
        Self::Meta: Default,
    {
        self.spawn_raw(Self::Meta::default(), w)
    }

    #[must_use]
    fn spawn_raw(
        self,
        meta: Self::Meta,
        w: &mut LateInstance<Self::Struct>,
    ) -> Strong<Self::Handle> {
        let (arena, manager) =
            w.get_two::<Self, <Self::Struct as StructHasArenaManager>::ManagerField>();

        let (keep_alive, handle) = arena.arena.insert(
            manager,
            <Self::Meta as Meta<Self::Handle>>::destroy_raw,
            ComponentSlot { value: self, meta },
        );

        Strong::new(Self::Handle::wrap(handle), keep_alive)
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
    type Struct: StructHasArenaManager;
    type MetaArena: 'static + Default + fmt::Debug;
    type Meta: Meta<Self, ArenaAnnotation = Self::MetaArena>;
    type Component: Component<
            Struct = Self::Struct,
            MetaArena = Self::MetaArena,
            Meta = Self::Meta,
            Handle = Self,
        >;

    fn wrap_raw(raw: RawHandle) -> Self {
        TransparentWrapper::wrap(raw)
    }

    fn raw_handle(self) -> RawHandle {
        TransparentWrapper::peel(self)
    }

    fn try_slot(self, w: &LateInstance<Self::Struct>) -> Option<&ComponentSlot<Self::Component>> {
        w.get::<Self::Component>().arena.get(self.raw_handle())
    }

    fn try_slot_mut(
        self,
        w: &mut LateInstance<Self::Struct>,
    ) -> Option<&mut ComponentSlot<Self::Component>> {
        w.get_mut::<Self::Component>()
            .arena
            .get_mut(self.raw_handle())
    }

    #[track_caller]
    fn slot(self, w: &LateInstance<Self::Struct>) -> &ComponentSlot<Self::Component> {
        match self.try_slot(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    #[track_caller]
    fn slot_mut(self, w: &mut LateInstance<Self::Struct>) -> &mut ComponentSlot<Self::Component> {
        match self.try_slot_mut(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    fn try_get(self, w: &LateInstance<Self::Struct>) -> Option<&Self::Component> {
        self.try_slot(w).map(|v| &v.value)
    }

    fn try_get_mut(self, w: &mut LateInstance<Self::Struct>) -> Option<&mut Self::Component> {
        self.try_slot_mut(w).map(|v| &mut v.value)
    }

    #[track_caller]
    fn get(self, w: &LateInstance<Self::Struct>) -> &Self::Component {
        &self.slot(w).value
    }

    #[track_caller]
    fn get_mut(self, w: &mut LateInstance<Self::Struct>) -> &mut Self::Component {
        &mut self.slot_mut(w).value
    }

    fn try_meta(self, w: &LateInstance<Self::Struct>) -> Option<&Self::Meta> {
        self.try_slot(w).map(|v| &v.meta)
    }

    fn try_get_meta(self, w: &mut LateInstance<Self::Struct>) -> Option<&mut Self::Meta> {
        self.try_slot_mut(w).map(|v| &mut v.meta)
    }

    #[track_caller]
    fn meta(self, w: &LateInstance<Self::Struct>) -> &Self::Meta {
        &self.slot(w).meta
    }

    #[track_caller]
    fn meta_mut(self, w: &mut LateInstance<Self::Struct>) -> &mut Self::Meta {
        &mut self.slot_mut(w).meta
    }

    #[track_caller]
    fn r(self, w: &LateInstance<Self::Struct>) -> &Self::Component {
        self.get(w)
    }

    #[track_caller]
    fn m(self, w: &mut LateInstance<Self::Struct>) -> &mut Self::Component {
        self.get_mut(w)
    }

    fn is_alive(self, w: &LateInstance<Self::Struct>) -> bool {
        self.try_get(w).is_some()
    }

    #[track_caller]
    fn as_strong_if_alive(self, w: &LateInstance<Self::Struct>) -> Option<Strong<Self>> {
        w.get::<Self::Component>()
            .arena
            .upgrade(
                w.get::<<Self::Struct as StructHasArenaManager>::ManagerField>(),
                self.raw_handle(),
            )
            .map(|keep_alive| Strong::new(self, keep_alive))
    }

    #[track_caller]
    fn as_strong(self, w: &LateInstance<Self::Struct>) -> Strong<Self> {
        match self.as_strong_if_alive(w) {
            Some(v) => v,
            None => panic!("attempted to upgrade dangling handle {self:?}"),
        }
    }

    #[must_use]
    fn debug<'a>(self, w: &'a LateInstance<Self::Struct>) -> DebugHandle<'a, Self> {
        DebugHandle {
            structure: w,
            handle: self,
        }
    }
}

// === Macros === //

#[doc(hidden)]
pub mod component_internals {
    pub use {
        crate::{Component, ComponentArena, Handle, Meta, RawHandle, format_handle},
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

            $crate::component_internals::late_field!(
                $ty[$namespace] => $crate::component_internals::ComponentArena<$ty>
            );

            impl $crate::component_internals::Component for $ty {
                type Struct = $namespace;
                type MetaArena = <($($meta)?) as $crate::component_internals::Meta<[<$ty Handle>]>>::ArenaAnnotation;
                type Meta = ($($meta)?);
                type Handle = [<$ty Handle>];
            }

            impl $crate::component_internals::Handle for [<$ty Handle>] {
                type Struct = $namespace;
                type MetaArena = <($($meta)?) as $crate::component_internals::Meta<[<$ty Handle>]>>::ArenaAnnotation;
                type Meta = ($($meta)?);
                type Component = $ty;
            }
        }
    )*};
}
