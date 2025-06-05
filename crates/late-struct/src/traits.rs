use crate::{
    LateFieldDescriptor, LateStructDescriptor, RawLateFieldDescriptor, RawLateStructDescriptor,
};

// === Trait Definitions === //

pub unsafe trait LateStruct: Sized + 'static {
    type EraseTo: ?Sized + 'static;

    fn raw_descriptor() -> &'static RawLateStructDescriptor;

    fn descriptor() -> &'static LateStructDescriptor<Self> {
        unsafe { Self::raw_descriptor().typed_unchecked() }
    }
}

pub unsafe trait LateField<S: LateStruct>: Sized + 'static {
    type Value: 'static + Default;

    fn raw_descriptor() -> &'static RawLateFieldDescriptor;

    fn descriptor() -> &'static LateFieldDescriptor<S> {
        unsafe { Self::raw_descriptor().typed_unchecked() }
    }

    fn coerce(value: *mut Self::Value) -> *mut S::EraseTo;
}

// === Implementation Macros === //

#[doc(hidden)]
pub mod late_macro_internals {
    use std::{any::TypeId, fmt};

    pub use {
        crate::{
            LateField, LateStruct, RawLateFieldDescriptor, RawLateStructDescriptor, late_field,
            late_struct,
        },
        linkme,
    };

    pub type DefaultEraseTo = dyn 'static + fmt::Debug;

    #[derive(Debug, Copy, Clone)]
    pub struct LateStructEntry {
        pub struct_type: TypeId,
        pub descriptor: &'static RawLateStructDescriptor,
    }

    impl LateStructEntry {
        pub fn of<S: LateStruct>() -> Self {
            Self {
                struct_type: TypeId::of::<S>(),
                descriptor: S::raw_descriptor(),
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    pub struct LateFieldEntry {
        pub struct_type: TypeId,
        pub descriptor: &'static RawLateFieldDescriptor,
    }

    impl LateFieldEntry {
        pub fn of<S: LateStruct, F: LateField<S>>() -> Self {
            Self {
                struct_type: TypeId::of::<S>(),
                descriptor: F::raw_descriptor(),
            }
        }
    }

    #[linkme::distributed_slice]
    pub static LATE_STRUCTS: [fn() -> LateStructEntry];

    #[linkme::distributed_slice]
    pub static LATE_FIELDS: [fn() -> LateFieldEntry];
}

#[macro_export]
macro_rules! late_struct {
    ($($ty:ty $(=> $erase_to:ty)?),*$(,)?) => {
        $(
            $crate::late_macro_internals::late_struct!(
                @single $ty $(=> $erase_to)?
            );
        )*
    };
    (@single $ty:ty) => {
        $crate::late_macro_internals::late_struct!(
            @single $ty => $crate::late_macro_internals::DefaultEraseTo
        );
    };
    (@single $ty:ty => $erase_to:ty) => {
        const _: () = {
            static DESCRIPTOR: $crate::late_macro_internals::RawLateStructDescriptor =
                $crate::late_macro_internals::RawLateStructDescriptor::new::<$ty>();

            #[$crate::late_macro_internals::linkme::distributed_slice(
                $crate::late_macro_internals::LATE_STRUCTS
            )]
            #[linkme(crate = $crate::late_macro_internals::linkme)]
            static ENTRY: fn() -> $crate::late_macro_internals::LateStructEntry =
                $crate::late_macro_internals::LateStructEntry::of::<$ty>;

            unsafe impl $crate::late_macro_internals::LateStruct for $ty {
                type EraseTo = $erase_to;

                fn raw_descriptor() -> &'static $crate::late_macro_internals::RawLateStructDescriptor {
                    &DESCRIPTOR
                }
            }
        };
    };
}

#[macro_export]
macro_rules! late_field {
    ($($ty:ty [$ns:ty] $(=> $val:ty)?),*$(,)?) => {$(
        $crate::late_macro_internals::late_field!(@single $ty [$ns] $(=> $val)?);
    )*};
    (@single $ty:ty [$ns:ty]) => {
        $crate::late_macro_internals::late_field!(@single $ty [$ns] => $ty);
    };
    (@single $ty:ty [$ns:ty] => $val:ty) => {
        const _: () = {
            static DESCRIPTOR: $crate::late_macro_internals::RawLateFieldDescriptor =
                $crate::late_macro_internals::RawLateFieldDescriptor::new::<$ns, $val>();

            #[$crate::late_macro_internals::linkme::distributed_slice(
                $crate::late_macro_internals::LATE_FIELDS
            )]
            #[linkme(crate = $crate::late_macro_internals::linkme)]
            static ENTRY: fn() -> $crate::late_macro_internals::LateFieldEntry =
                $crate::late_macro_internals::LateFieldEntry::of::<$ns, $ty>;

            unsafe impl $crate::late_macro_internals::LateField<$ns> for $ty {
                type Value = $val;

                fn raw_descriptor() -> &'static $crate::late_macro_internals::RawLateFieldDescriptor {
                    &DESCRIPTOR
                }

                fn coerce(value: *mut Self::Value) -> *mut <$ns as $crate::late_macro_internals::LateStruct>::EraseTo {
                    value
                }
            }
        };
    };
}
