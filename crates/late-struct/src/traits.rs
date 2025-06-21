use crate::{
    LateFieldDescriptor, LateStructDescriptor, RawLateFieldDescriptor, RawLateStructDescriptor,
};

// === Trait Definitions === //

/// A trait for a marker type representing a late-initialized structure.
///
/// This trait cannot be implemented manually. Instead, it should be implemented using the
/// [`late_struct!`](crate::late_struct) macro. Most documentation for this trait can be found there.
///
/// ## Safety
///
/// This trait can only be safely implemented through the `late_struct!` macro.
///
pub unsafe trait LateStruct: Sized + 'static + LateStructSealed {
    /// The type all fields are expected to coerce into. This is usually a [trait object].
    ///
    /// In the following invocation of [`late_struct!`](crate::late_struct)...
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
    ///
    /// [trait object]: https://doc.rust-lang.org/reference/types/trait-object.html#r-type.trait-object
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
/// [`late_field!`](crate::late_field) macro. Most documentation for this trait can be found there.
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

mod sealed {
    use super::LateStruct;

    #[doc(hidden)]
    pub trait LateStructSealed {}

    #[doc(hidden)]
    pub trait LateFieldSealed<S: LateStruct> {}
}

pub(crate) use sealed::{LateFieldSealed, LateStructSealed};

// === Implementation Macros === //

#[doc(hidden)]
pub mod late_macro_internals {
    use std::{any::TypeId, fmt, marker::PhantomData};

    // === Re-exports === //

    pub use {
        super::sealed::{LateFieldSealed, LateStructSealed},
        crate::{
            LateField, LateStruct, RawLateFieldDescriptor, RawLateStructDescriptor, late_field,
            late_struct,
        },
    };

    pub type DefaultEraseTo = dyn 'static + fmt::Debug;

    pub const fn new_late_struct_descriptor<S: LateStruct>() -> RawLateStructDescriptor {
        RawLateStructDescriptor::new::<S>()
    }

    pub const fn new_late_field_descriptor<S, F>() -> RawLateFieldDescriptor
    where
        S: LateStruct,
        F: LateField<S>,
    {
        RawLateFieldDescriptor::new::<S, F>()
    }

    // === OrDefault === //

    pub type OrDefault<Default, T = Default> =
        <OrDefaultHelper<T, Default> as OrDefaultHelperTrait>::Out;

    pub struct OrDefaultHelper<T, Ignore>(PhantomData<T>, PhantomData<Ignore>)
    where
        T: ?Sized,
        Ignore: ?Sized;

    pub trait OrDefaultHelperTrait {
        type Out: ?Sized;
    }

    impl<T, Ignore> OrDefaultHelperTrait for OrDefaultHelper<T, Ignore>
    where
        T: ?Sized,
        Ignore: ?Sized,
    {
        type Out = T;
    }

    // === Entry management === //

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

/// Implements the [`LateStruct`] trait for the specified `$ty` type, turing it into a marker type
/// that can be used to refer to a late-initialized structure.
///
/// See the [crate level documentation](crate) for examples of this macro in action as well as a
/// step-by-step guide on how to use it.
///
/// You can attach fields onto this type using the [`late_field!`](crate::late_field) macro and can
/// instantiate the structure defined by this macro using the [`LateInstance`](super::LateInstance)
/// struct. Either operation can occur anywhere within the crate graph of a project; fields can be
/// added to the structure in crates which are downstream to crates which instantiate that
/// structure.
///
/// The `$ty` type need only be [`Sized`] and live for `'static`. No types in this macro invocation
/// may involve unbound generic parameters.
///
/// You can specify more than one type onto which `LateStruct` should be implemented with this
/// macro.
///
/// ## The `EraseTo` Parameter
///
/// The optional `$erase_to` type specifies the type all field values should be able to upcast (i.e.
/// "erase") to. Generally, this type is a [trait object]. For instance, if you specify `dyn Any +
/// Send + Sync`, all field values will have to implement [`Any`](std::any::Any), [`Send`], and
/// [`Sync`]. If `$erase_to` is omitted, the type will default to `dyn 'static + fmt::Debug`, which
/// would entail that all fields in the struct must implement the [`Debug`] trait.
///
/// In addition to the trait constraints `$erased_to` places on fields, `late_field!` automatically
/// enforces that all fields live for `'static`, be [`Sized`], and implement the [`Default`] trait.
/// It is this `Default` trait implementation which is used to initialize instances of this struct.
///
/// Traits such as [`Eq`], [`Clone`], and [`Hash`] are not [`dyn` compatible](dyn-compat) and thus
/// cannot be used directly inside the bounds of the `$erase_to` type. You can work around this
/// restriction by using the [`DynEq`](super::DynEq), [`DynClone`](super::DynClone), and
/// [`DynHash`](super::DynHash) traits respectively, which are `dyn` compatible while still encoding
/// the necessary constraints.
///
/// [trait object]: https://doc.rust-lang.org/reference/types/trait-object.html#r-type.trait-object
/// [dyn-compat]: https://doc.rust-lang.org/reference/items/traits.html#r-items.traits.dyn-compatible
#[macro_export]
macro_rules! late_struct {
    ($($ty:ty $(=> $erase_to:ty)?),*$(,)?) => {$(
        const _: () = {
            static DESCRIPTOR: $crate::late_macro_internals::RawLateStructDescriptor =
                $crate::late_macro_internals::new_late_struct_descriptor::<$ty>();

            impl $crate::late_macro_internals::LateStructSealed for $ty {}

            unsafe impl $crate::late_macro_internals::LateStruct for $ty {
                type EraseTo = $crate::late_macro_internals::OrDefault<
                    $crate::late_macro_internals::DefaultEraseTo,
                    $(, $erase_to)?
                >;

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
    )*};
}

/// Implements the [`LateField`] trait for the specified `$ty` type, turning it into a marker type
/// that can be used to refer to a field within a late-initialized structure.
///
/// See the [crate level documentation](crate) for examples of this macro in action as well as a
/// step-by-step guide on how to use it.
///
/// The `$ns` type specifies a [`LateStruct`] (defined by an earlier
/// [`late_struct!`](crate::late_struct) invocation) into which this field will be placed. The
/// optional `$val` type specifies the type of the value this field stores. If `$val` is omitted, it
/// will default to `$ty`.
///
/// `$val` (or `$ty`, if `$val` is omitted) is expected to live for `'static`, be [`Sized`], and
/// implement [`Default`]. It is this `Default` trait implementation which is used to initialize the
/// fields of a [`LateInstance`](super::LateInstance). In addition, the field value must be able to
/// coerce into the `$ns` structure type's [`LateStruct::EraseTo`] associated type. By default, the
/// `LateStruct::EraseTo` associated type is set to `dyn 'static + fmt::Debug` and thus `$val` will
/// be expected to implement [`Debug`] as well.
///
/// If `$val` is distinct from `$ty`, the `$ty` type need only be [`Sized`] and live for `'static`.
///
/// No types in this macro invocation may involve unbound generic parameters. You can specify more
/// than one type onto which `LateField` should be implemented with this macro.
#[macro_export]
macro_rules! late_field {
    (
        $($ty:ty [$ns:ty] $(=> $val:ty)?),*
        $(,)?
    ) => {$(
        const _: () = {
            static DESCRIPTOR: $crate::late_macro_internals::RawLateFieldDescriptor =
                $crate::late_macro_internals::new_late_field_descriptor::<$ns, $ty>();

            impl $crate::late_macro_internals::LateFieldSealed<$ns> for $ty {}

            unsafe impl $crate::late_macro_internals::LateField<$ns> for $ty {
                type Value = $crate::late_macro_internals::OrDefault<$ty $(, $val)?>;

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
    )*};
}
