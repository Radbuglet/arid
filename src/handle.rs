use std::{fmt, hash};

use bytemuck::TransparentWrapper;
use derive_where::derive_where;
use late_struct::{LateField, LateInstance, LateStruct};

use crate::{Arena, RawHandle, Strong};

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

// === Traits === //

pub trait Component:
    'static + Sized + fmt::Debug + LateField<Self::Struct, Value = Arena<Self>>
{
    type Struct: LateStruct;
    type Handle: Handle<Struct = Self::Struct, Component = Self>;

    #[must_use]
    fn spawn(self, w: &mut LateInstance<Self::Struct>) -> Strong<Self::Handle> {
        let (handle, keep_alive) = w.get_mut::<Self>().spawn(self);

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
    type Struct: LateStruct;
    type Component: Component<Struct = Self::Struct, Handle = Self>;

    fn wrap_raw(raw: RawHandle) -> Self {
        TransparentWrapper::wrap(raw)
    }

    fn raw_handle(self) -> RawHandle {
        TransparentWrapper::peel(self)
    }

    fn try_get(self, w: &LateInstance<Self::Struct>) -> Option<&Self::Component> {
        w.get::<Self::Component>().get(self.raw_handle())
    }

    fn try_get_mut(self, w: &mut LateInstance<Self::Struct>) -> Option<&mut Self::Component> {
        w.get_mut::<Self::Component>().get_mut(self.raw_handle())
    }

    #[track_caller]
    fn get(self, w: &LateInstance<Self::Struct>) -> &Self::Component {
        match self.try_get(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
    }

    #[track_caller]
    fn get_mut(self, w: &mut LateInstance<Self::Struct>) -> &mut Self::Component {
        match self.try_get_mut(w) {
            Some(v) => v,
            None => panic!("attempted to access dangling handle {self:?}"),
        }
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
            .upgrade(self.raw_handle())
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
        crate::{Arena, Component, Handle, RawHandle, format_handle},
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
        $namespace:ty =>
        $( $ty:ident ),*$(,)?
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
                $ty[$namespace] => $crate::component_internals::Arena<$ty>
            );

            impl $crate::component_internals::Component for $ty {
                type Struct = $namespace;
                type Handle = [<$ty Handle>];
            }

            impl $crate::component_internals::Handle for [<$ty Handle>] {
                type Struct = $namespace;
                type Component = $ty;
            }
        }
    )*};
}
