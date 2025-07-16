use std::{fmt, hash};

use bytemuck::TransparentWrapper;
use derive_where::derive_where;
use late_struct::{LateField, LateInstance, LateStruct};

use crate::{Arena, RawHandle, Strong};

// === Debug === //

#[derive_where(Copy, Clone)]
pub struct Debug<'a, T: Handle> {
    pub structure: &'a LateInstance<T::Struct>,
    pub handle: T,
}

impl<T: Handle> fmt::Debug for Debug<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
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
    fn debug<'a>(self, w: &'a LateInstance<Self::Struct>) -> Debug<'a, Self> {
        Debug {
            structure: w,
            handle: self,
        }
    }
}

// === Macros === //

#[doc(hidden)]
pub mod component_internals {
    pub use {
        crate::{Arena, Component, Handle, RawHandle},
        bytemuck::TransparentWrapper,
        late_struct::late_field,
        paste::paste,
        std::{
            clone::Clone,
            cmp::{Eq, Ord, PartialEq, PartialOrd},
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
                Debug,
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
