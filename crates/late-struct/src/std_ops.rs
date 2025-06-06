use std::{any::Any, hash};

use crate::{LateInstance, LateStruct};

// === DynClone === //

pub unsafe trait DynClone {
    unsafe fn dyn_clone_into(&self, other: *mut u8);
}

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

pub trait DynEq: DynPartialEq {}

pub trait DynPartialEq: Any {
    fn dyn_eq(&self, other: &dyn Any) -> bool;

    fn as_any(&self) -> &dyn Any;
}

impl<T: 'static + Eq> DynEq for T {}

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

pub trait DynHash {
    fn dyn_hash(&self, hasher: &mut dyn hash::Hasher);
}

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
