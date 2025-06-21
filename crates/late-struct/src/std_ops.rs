use std::{any::Any, hash};

use crate::{LateInstance, LateStruct};

mod sealed {
    pub trait DynCloneSealed {}

    pub trait DynEqSealed {}

    pub trait DynPartialEqSealed {}

    pub trait DynHashSealed {}
}

// === DynClone === //

/// A bound for the [`LateStruct::EraseTo`] associated type which asserts that fields must have an
/// implementation of [`Clone`]. If `LateStruct::EraseTo` implements this trait, the
/// [`LateInstance`] instantiating that structure will implement `Clone`.
///
/// This trait is implemented for all types which implement `Clone`. Unlike `Eq`, however, it is
/// [`dyn` compatible](dyn-compat).
///
/// ## Safety
///
/// This trait is sealed and no downstream crates can implement it.
///
/// [dyn-compat]: https://doc.rust-lang.org/reference/items/traits.html#r-items.traits.dyn-compatible
pub unsafe trait DynClone: sealed::DynCloneSealed {
    #[doc(hidden)]
    unsafe fn dyn_clone_into(&self, other: *mut u8);
}

impl<T: Clone> sealed::DynCloneSealed for T {}

unsafe impl<T: Clone> DynClone for T {
    unsafe fn dyn_clone_into(&self, other: *mut u8) {
        unsafe { other.cast::<T>().write(self.clone()) };
    }
}

impl<S: LateStruct> Clone for LateInstance<S>
where
    S::EraseTo: DynClone,
{
    fn clone(&self) -> Self {
        unsafe {
            Self::new_custom(|field, init| {
                self.get_erased(field).dyn_clone_into(init);
            })
        }
    }
}

// === DynEq === //

/// A bound for the [`LateStruct::EraseTo`] associated type which asserts that fields must have an
/// implementation of [`Eq`] against itself. If `LateStruct::EraseTo` implements this trait, the
/// [`LateInstance`] instantiating that structure will implement `Eq`.
///
/// This trait is implemented for all types which implement `Eq<Self>`. Unlike `Eq<Self>`, however,
/// it is [`dyn` compatible](dyn-compat).
///
/// This trait is sealed and no downstream crates can implement it.
///
/// [dyn-compat]: https://doc.rust-lang.org/reference/items/traits.html#r-items.traits.dyn-compatible
pub trait DynEq: DynPartialEq + sealed::DynEqSealed {}

/// A bound for the [`LateStruct::EraseTo`] associated type which asserts that fields must have an
/// implementation of [`PartialEq`] against itself. If `LateStruct::EraseTo` implements this trait,
/// the [`LateInstance`] instantiating that structure will implement `PartialEq`.
///
/// This trait is implemented for all types which implement `PartialEq<Self>`. Unlike
/// `PartialEq<Self>`, however, it is [`dyn` compatible](dyn-compat).
///
/// This trait is sealed and no downstream crates can implement it.
///
/// [dyn-compat]: https://doc.rust-lang.org/reference/items/traits.html#r-items.traits.dyn-compatible
pub trait DynPartialEq: Any + sealed::DynPartialEqSealed {
    #[doc(hidden)]
    fn dyn_eq(&self, other: &dyn Any) -> bool;

    #[doc(hidden)]
    fn as_any(&self) -> &dyn Any;
}

impl<T: 'static + Eq> sealed::DynEqSealed for T {}

impl<T: 'static + Eq> DynEq for T {}

impl<T: 'static + PartialEq> sealed::DynPartialEqSealed for T {}

impl<T: 'static + PartialEq> DynPartialEq for T {
    fn dyn_eq(&self, other: &dyn Any) -> bool {
        other.downcast_ref::<T>().is_some_and(|rhs| self == rhs)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl<S: LateStruct> Eq for LateInstance<S> where S::EraseTo: DynEq {}

impl<S: LateStruct> PartialEq for LateInstance<S>
where
    S::EraseTo: DynEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.fields().iter().all(|&field| {
            self.get_erased(field)
                .dyn_eq(other.get_erased(field).as_any())
        })
    }
}

// === DynHash === //

/// A bound for the [`LateStruct::EraseTo`] associated type which asserts that fields must have an
/// implementation of [`Hash`]. If `LateStruct::EraseTo` implements this trait, the [`LateInstance`]
/// instantiating that structure will implement `Hash`.
///
/// This trait is implemented for all types which implement `Hash`. Unlike `Hash`, however,
/// it is [`dyn` compatible](dyn-compat).
///
/// This trait is sealed and no downstream crates can implement it.
///
/// [dyn-compat]: https://doc.rust-lang.org/reference/items/traits.html#r-items.traits.dyn-compatible
pub trait DynHash: sealed::DynHashSealed {
    #[doc(hidden)]
    fn dyn_hash(&self, hasher: &mut dyn hash::Hasher);
}

impl<T: hash::Hash> sealed::DynHashSealed for T {}

impl<T: hash::Hash> DynHash for T {
    fn dyn_hash(&self, mut hasher: &mut dyn hash::Hasher) {
        self.hash(&mut hasher);
    }
}

impl<S: LateStruct> hash::Hash for LateInstance<S>
where
    S::EraseTo: DynHash,
{
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        for field in self.fields() {
            self.get_erased(field).dyn_hash(state);
        }
    }
}
