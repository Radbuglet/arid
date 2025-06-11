use crate::{
    LateFieldDescriptor, LateStructDescriptor, RawLateFieldDescriptor, RawLateStructDescriptor,
};

// === Trait Definitions === //

/// A trait for a marker type representing a late-initialized structure.
///
/// This trait cannot be implemented manually. Instead, it should be implemented using the
/// [`late_struct!`] macro.
///
/// ## Safety
///
/// This trait can only be safely implemented through the `late_struct!` macro.
///
pub unsafe trait LateStruct: Sized + 'static + LateStructSealed {
    /// The type all fields are expected to coerce into.
    ///
    /// In the following invocation of [`late_struct!`]...
    ///
    /// ```
    /// # use std::any::Any;
    /// use late_struct::late_struct;
    /// # pub struct MyType;
    /// late_struct!(MyType => dyn Any + Send + Sync);
    /// ```
    ///
    /// ...the type `<MyType as LateStruct>::EraseTo` would be `dyn Any + Send + Sync`.
    ///
    /// If no erase-to type is specified in an invocation of `late_struct!`, a default
    /// `dyn 'static + fmt::Debug` type will be used.
    type EraseTo: ?Sized + 'static;

    /// Fetches the untyped descriptor associated with this structure.
    ///
    /// See [`LateStruct::descriptor`] for a typed version of this method.
    fn raw_descriptor() -> &'static RawLateStructDescriptor;

    /// Fetches the strongly-typed descriptor associated with this structure.
    fn descriptor() -> &'static LateStructDescriptor<Self> {
        unsafe { Self::raw_descriptor().typed_unchecked() }
    }
}

/// A trait for a marker type representing a field in a late-initialized structure.
///
/// This trait cannot be implemented manually. Instead, it should be implemented using the
/// [`late_field!`] macro.
///
/// ## Safety
///
/// This trait can only be safely implemented through the `late_field!` macro.
///
pub unsafe trait LateField<S: LateStruct>: Sized + 'static + LateFieldSealed<S> {
    /// The type of this field's value.
    type Value: 'static + Default;

    /// Fetches the untyped descriptor associated with this field.
    ///
    /// See [`LateField::descriptor`] for a typed version of this method.
    fn raw_descriptor() -> &'static RawLateFieldDescriptor;

    /// Fetches the strongly-typed descriptor associated with this field.
    fn descriptor() -> &'static LateFieldDescriptor<S> {
        unsafe { Self::raw_descriptor().typed_unchecked() }
    }

    /// Coerces a pointer to the field's concrete value to its unsized form expected used in
    /// [`LateStruct::EraseTo`].
    fn coerce(value: *mut Self::Value) -> *mut S::EraseTo;
}

// === Sealed Traits === //

pub mod sealed {
    use super::LateStruct;

    pub trait LateStructSealed {}

    pub trait LateFieldSealed<S: LateStruct> {}
}

pub(crate) use sealed::{LateFieldSealed, LateStructSealed};

// === Implementation Macros === //

#[doc(hidden)]
pub mod late_macro_internals {
    use std::{any::TypeId, fmt};

    pub use {
        super::sealed::{LateFieldSealed, LateStructSealed},
        crate::{
            LateField, LateStruct, RawLateFieldDescriptor, RawLateStructDescriptor, late_field,
            late_struct,
        },
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

    cfg_if::cfg_if! {
        if #[cfg(target_family = "wasm")] {
            pub use ::inventory;

            pub struct LateStructEntryWrapper(pub fn() -> LateStructEntry);
            pub struct LateFieldEntryWrapper(pub fn() -> LateFieldEntry);

            inventory::collect!(LateStructEntryWrapper);
            inventory::collect!(LateFieldEntryWrapper);

            pub fn iter_late_structs() -> impl Iterator<Item = LateStructEntry> {
                inventory::iter::<LateStructEntryWrapper>().map(|f| f.0())
            }

            pub fn iter_late_fields() -> impl Iterator<Item = LateFieldEntry> {
                inventory::iter::<LateFieldEntryWrapper>().map(|f| f.0())
            }
        } else {
            pub use ::linkme;

            #[linkme::distributed_slice]
            pub static LATE_STRUCTS: [fn() -> LateStructEntry];

            #[linkme::distributed_slice]
            pub static LATE_FIELDS: [fn() -> LateFieldEntry];

            pub fn iter_late_structs() -> impl Iterator<Item = LateStructEntry> {
                LATE_STRUCTS.iter().map(|&f| f())
            }

            pub fn iter_late_fields() -> impl Iterator<Item = LateFieldEntry> {
                LATE_FIELDS.iter().map(|&f| f())
            }
        }
    }
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

            impl $crate::late_macro_internals::LateStructSealed for $ty {}

            unsafe impl $crate::late_macro_internals::LateStruct for $ty {
                type EraseTo = $erase_to;

                fn raw_descriptor() -> &'static $crate::late_macro_internals::RawLateStructDescriptor {
                    &DESCRIPTOR
                }
            }

            #[cfg(target_family = "wasm")]
            $crate::late_macro_internals::inventory::submit! {
                $crate::late_macro_internals::LateStructEntryWrapper(
                    $crate::late_macro_internals::LateStructEntry::of::<$ty>,
                )
            }

            #[cfg(not(target_family = "wasm"))]
            #[$crate::late_macro_internals::linkme::distributed_slice(
                $crate::late_macro_internals::LATE_STRUCTS
            )]
            #[linkme(crate = $crate::late_macro_internals::linkme)]
            static ENTRY: fn() -> $crate::late_macro_internals::LateStructEntry =
                $crate::late_macro_internals::LateStructEntry::of::<$ty>;
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
                $crate::late_macro_internals::RawLateFieldDescriptor::new::<$ns, $ty>();

            impl $crate::late_macro_internals::LateFieldSealed<$ns> for $ty {}

            unsafe impl $crate::late_macro_internals::LateField<$ns> for $ty {
                type Value = $val;

                fn raw_descriptor() -> &'static $crate::late_macro_internals::RawLateFieldDescriptor {
                    &DESCRIPTOR
                }

                fn coerce(value: *mut Self::Value) -> *mut <$ns as $crate::late_macro_internals::LateStruct>::EraseTo {
                    value
                }
            }

            #[cfg(target_family = "wasm")]
            $crate::late_macro_internals::inventory::submit! {
                $crate::late_macro_internals::LateFieldEntryWrapper(
                    $crate::late_macro_internals::LateFieldEntry::of::<$ns, $ty>,
                )
            }

            #[cfg(not(target_family = "wasm"))]
            #[$crate::late_macro_internals::linkme::distributed_slice(
                $crate::late_macro_internals::LATE_FIELDS
            )]
            #[linkme(crate = $crate::late_macro_internals::linkme)]
            static ENTRY: fn() -> $crate::late_macro_internals::LateFieldEntry =
                $crate::late_macro_internals::LateFieldEntry::of::<$ns, $ty>;
        };
    };
}
