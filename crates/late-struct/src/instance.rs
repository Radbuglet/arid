use std::{
    alloc,
    any::TypeId,
    cell::{self, Ref, RefCell, RefMut},
    convert::Infallible,
    fmt,
    marker::PhantomData,
    ptr::NonNull,
};

use scopeguard::ScopeGuard;

use crate::{LateField, LateFieldDescriptor, LateLayoutInitToken, LateStruct};

// === Exterior Mutability === //

/// An instance of a [`LateStruct`].
///
/// You can define a late-initialized structure with the [`late_struct!`](crate::late_struct) macro
/// and add fields to it with the [`late_field!`](crate::late_field) macro. You can then fetch these
/// fields using the [`get`](LateInstance::get) and [`get_mut`](LateInstance::get_mut) methods.
///
/// These methods only allow users to borrow one field mutably at a time. If this is too
/// restrictive, you can use the [`get_two`](LateInstance::get_two) method, which obtains two fields
/// mutably at the same time. If that is still too restrictive, consider using the
/// [`dynamic`](LateInstance::dynamic) method, which returns a [`LateInstanceDyn`] view of this
/// structure providing a [`RefCell`]-like runtime-borrow-checked API for accessing fields. If that
/// is still too restrictive, all fields in the structure exhibit
/// [`UnsafeCell`](std::cell::UnsafeCell)-like semantics and thus can be borrowed manually using the
/// [`get_ptr`](LateInstance::get_ptr) method.
///
/// ## Reflection Operations
///
/// Both [`LateInstance::fields`] and [`LateStruct::descriptor`] expose the set of fields in a given
/// structure as a list of [`LateFieldDescriptor`]s. These can be used alongside the
/// [`get_erased`](LateInstance::get_erased) and [`get_erased_mut`](LateInstance::get_erased_mut) to
/// obtain references to the [`S::EraseTo`](LateStruct::EraseTo) types to which all fields are
/// required to upcast.
///
/// ## Structural Traits
///
/// Trait implementations for this type are largely structural. If
/// [`S::EraseTo`](LateStruct::EraseTo) implements [`Send`], for instance, this type will implement
/// [`Send`]. Here is a full table of all of the structural trait implementations:
///
/// - `'static` and [`Default`], although since all fields are always required to implement these
///   two traits, so too will [`LateInstance`].
/// - [`Send`], [`Sync`], and [`Debug`] are all perfectly structural w.r.t `S::EraseTo`.
/// - [`Eq`] is implemented if `S::EraseTo` implements [`DynEq`](super::DynEq).
/// - [`PartialEq`] is implemented if `S::EraseTo` implements [`DynPartialEq`](super::DynPartialEq).
/// - [`Clone`] is implemented if `S::EraseTo` implements [`DynClone`](super::DynClone).
/// - [`Hash`] is implemented if `S::EraseTo` implements [`DynHash`](super::DynHash).
///
/// The [`DynEq`](super::DynEq), [`DynClone`](super::DynClone), and [`DynHash`](super::DynHash)
/// traits exist because [`Eq`], [`Clone`], and [`Hash`], are not [`dyn` compatible](dyn-compat) and
/// thus cannot directly be used in the bounds of a `dyn Trait` object.
///
/// We do not have a structural implementation for [`Ord`] since the order of fields inside this
/// structure is not defined.
///
/// By default, `S::EraseTo` is set to `dyn 'static + fmt::Debug`. This implies that the
/// `LateInstance` instantiating that `S` will implement [`Debug`] but will not implement, e.g.,
/// `Send` or `Sync`. See [this section](index.html#advanced-usage) of the crate level documentation
/// for information on how to change these bounds.
///
/// [dyn-compat]: https://doc.rust-lang.org/reference/items/traits.html#r-items.traits.dyn-compatible
pub struct LateInstance<S: LateStruct> {
    _ty: PhantomData<fn(S) -> S>,

    /// The ZST proving that we have initialized the late-bound layouts of all structures present in
    /// this application.
    init_token: LateLayoutInitToken,

    /// The base pointer to the boxed structure.
    data_base: NonNull<u8>,

    /// Reference cells tracking borrows of fields of this structure when accessed through
    /// [`LateInstanceDyn`]. Since `LateInstanceDyn` can only be constructed through a mutable
    /// reference to this structure and `LateInstanceDyn` is `!Sync`, it is safe to contain these
    /// objects in an otherwise `Sync` structure.
    cells: Box<[RefCell<()>]>,
}

unsafe impl<S: LateStruct> Send for LateInstance<S> where
    // `HashMap<TypeId, Box<S::EraseTo>>: Send` iff `S::EraseTo: Send`
    S::EraseTo: Send
{
}

unsafe impl<S: LateStruct> Sync for LateInstance<S> where
    // `HashMap<TypeId, Box<S::EraseTo>>: Sync` iff `S::EraseTo: Sync`
    S::EraseTo: Sync
{
}

impl<S: LateStruct> fmt::Debug for LateInstance<S>
where
    S::EraseTo: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("LateInstance");

        for field in self.fields() {
            f.field(field.key_type_name(), &self.get_erased_ptr(field));
        }

        f.finish()
    }
}

impl<S: LateStruct> Default for LateInstance<S> {
    fn default() -> Self {
        Self::new()
    }
}

/// Regular operations.
impl<S: LateStruct> LateInstance<S> {
    /// Instantiate a new structure where each field is initialized using its implementation of the
    /// [`Default`] trait.
    ///
    /// This is equivalent to the `LateInstance`'s implementation of `Default`.
    pub fn new() -> Self {
        unsafe {
            Self::new_custom(|field, ptr| {
                field.init(ptr);
            })
        }
    }

    /// Fetches an immutable reference to a field by its [`LateField`] marker type.
    pub fn get<F: LateField<S>>(&self) -> &F::Value {
        unsafe { self.get_ptr::<F>().as_ref() }
    }

    /// Fetches an mutable reference to a field by its [`LateField`] marker type.
    ///
    /// If you want to fetch more than one field at a time mutably, consider using the [`get_two`]
    /// or [`dynamic`] methods.
    ///
    /// [`get_two`]: LateInstance::get_two
    /// [`dynamic`]: LateInstance::dynamic
    pub fn get_mut<F: LateField<S>>(&mut self) -> &mut F::Value {
        unsafe { self.get_ptr::<F>().as_mut() }
    }

    /// Fetches a raw pointer to the structure's field by its [`LateField`] marker type. These
    /// fields act as if they were wrapped inside an [`UnsafeCell`](std::cell::UnsafeCell) and thus
    /// can be mutably dereferenced.
    pub fn get_ptr<F: LateField<S>>(&self) -> NonNull<F::Value> {
        unsafe {
            self.data_base
                .add(F::descriptor().offset(self.init_token))
                .cast()
        }
    }

    /// Fetches a pair of mutable references to distinct fields identified by their [`LateField`]
    /// marker types. This method panics if `F` and `G` are the same type.
    pub fn get_two<F, G>(&mut self) -> (&mut F::Value, &mut G::Value)
    where
        F: LateField<S>,
        G: LateField<S>,
    {
        assert_ne!(TypeId::of::<F>(), TypeId::of::<G>());

        unsafe { (self.get_ptr::<F>().as_mut(), self.get_ptr::<G>().as_mut()) }
    }

    /// Fetches a [`LateInstanceDyn`] view into the structure which exposes a [`RefCell`]-like API
    /// for dynamically borrowing multiple fields mutably at the same time.
    pub fn dynamic(&mut self) -> &mut LateInstanceDyn<S> {
        unsafe { &mut *(self as *mut Self as *mut LateInstanceDyn<S>) }
    }
}

/// Reflection operations.
impl<S: LateStruct> LateInstance<S> {
    /// Construct a new [`LateInstance`] using the `init` closure to initialize each field in the
    /// order they appear in the
    /// [`LateStructDescriptor::fields`](super::LateStructDescriptor::fields) list.
    ///
    /// ## Safety
    ///
    /// The pointee of each of the `*mut u8` pointers provided to `init` must be initialized to an
    /// instance of the corresponding field's value type.
    ///
    pub unsafe fn new_custom(
        mut init: impl FnMut(&'static LateFieldDescriptor<S>, *mut u8),
    ) -> Self {
        let Ok(res) = unsafe {
            Self::try_new_custom(|field, ptr| {
                init(field, ptr);
                Result::<(), Infallible>::Ok(())
            })
        };
        res
    }

    /// Construct a new [`LateInstance`] using the `init` closure to initialize each field in the
    /// order they appear in the
    /// [`LateStructDescriptor::fields`](super::LateStructDescriptor::fields) list. If `init`
    /// returns an `Err`, the construction of the structure is aborted and the error is forwarded
    /// to the caller.
    ///
    /// ## Safety
    ///
    /// The pointee of each of the `*mut u8` pointers provided to `init` must be initialized to an
    /// instance of the corresponding field's value type.
    ///
    pub unsafe fn try_new_custom<E>(
        mut init: impl FnMut(&'static LateFieldDescriptor<S>, *mut u8) -> Result<(), E>,
    ) -> Result<Self, E> {
        let init_token = LateLayoutInitToken::new();

        let struct_layout = S::descriptor().layout(init_token);
        let struct_fields = S::descriptor().fields(init_token);

        // Allocate space for the instance
        let Some(data_base) = NonNull::new(unsafe { alloc::alloc(struct_layout) }) else {
            panic!("out of memory");
        };

        let dealloc_guard = scopeguard::guard((), |()| {
            unsafe { alloc::dealloc(data_base.as_ptr(), struct_layout) };
        });

        // Initialize its fields
        let mut drop_guard = scopeguard::guard(0usize, |cnt| {
            for &field in &struct_fields[0..cnt] {
                unsafe { field.drop(data_base.add(field.offset(init_token)).as_ptr()) };
            }
        });

        for &field in struct_fields {
            init(field, unsafe {
                data_base.add(field.offset(init_token)).as_ptr()
            })?;
            *drop_guard += 1;
        }

        // Defuse guards
        ScopeGuard::into_inner(dealloc_guard);
        ScopeGuard::into_inner(drop_guard);

        Ok(Self {
            _ty: PhantomData,
            init_token,
            data_base,
            cells: Box::from_iter((0..struct_fields.len()).map(|_| RefCell::new(()))),
        })
    }

    /// Fetches a [`LateLayoutInitToken`] attesting to the fact that all late-initialized structure
    /// layouts and field offsets have been resolved.
    pub fn init_token(&self) -> LateLayoutInitToken {
        self.init_token
    }

    /// Fetches the set of fields comprising the structure. This is equivalent to calling the
    /// [`fields`](crate::LateStructDescriptor::fields) method on the
    /// [`LateStructDescriptor`](crate::LateStructDescriptor) instance returned by
    /// [`S::descriptor()`](LateStruct::descriptor).
    pub fn fields(&self) -> &'static [&'static LateFieldDescriptor<S>] {
        S::descriptor().fields(self.init_token)
    }

    /// Fetches the pointer to the base of the heap allocation containing the structure's values.
    pub fn base_ptr(&self) -> NonNull<u8> {
        self.data_base
    }

    /// Fetches an immutable reference to a field by its [`LateFieldDescriptor`] instance.
    pub fn get_erased(&self, field: &LateFieldDescriptor<S>) -> &S::EraseTo {
        unsafe { self.get_erased_ptr(field).as_ref() }
    }

    /// Fetches a mutable reference to a field by its [`LateFieldDescriptor`] instance.
    ///
    /// If you want to fetch more than one field at a time mutably, consider using the [`get_two`]
    /// or [`dynamic`] methods.
    ///
    /// [`get_two`]: LateInstance::get_two
    /// [`dynamic`]: LateInstance::dynamic
    pub fn get_erased_mut(&mut self, field: &LateFieldDescriptor<S>) -> &mut S::EraseTo {
        unsafe { self.get_erased_ptr(field).as_mut() }
    }

    /// Fetches a raw pointer to the structure's field identified by its [`LateFieldDescriptor`]
    /// instance. These fields act as if they were wrapped inside an
    /// [`UnsafeCell`](std::cell::UnsafeCell) and thus can be mutably dereferenced.
    pub fn get_erased_ptr(&self, field: &LateFieldDescriptor<S>) -> NonNull<S::EraseTo> {
        unsafe {
            NonNull::new_unchecked(
                field.erase_value(
                    self.data_base
                        .add(field.offset(self.init_token))
                        .cast()
                        .as_ptr(),
                ),
            )
        }
    }
}

impl<S: LateStruct> Drop for LateInstance<S> {
    fn drop(&mut self) {
        let struct_layout = S::descriptor().layout(self.init_token);
        let struct_fields = S::descriptor().fields(self.init_token);

        for field in struct_fields {
            unsafe { field.drop(self.data_base.add(field.offset(self.init_token)).as_ptr()) }
        }

        unsafe { alloc::dealloc(self.data_base.as_ptr(), struct_layout) };
    }
}

// === Interior Mutability === //

/// A view around a [`LateInstance`] which exposes a [`RefCell`]-like API for dynamically borrowing
/// multiple fields mutably at the same time.
#[repr(transparent)]
pub struct LateInstanceDyn<S: LateStruct> {
    _not_send_sync: PhantomData<*const ()>,
    inner: LateInstance<S>,
}

impl<S: LateStruct> fmt::Debug for LateInstanceDyn<S>
where
    S::EraseTo: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("LateInstanceDynamic");

        for field in self.fields() {
            f.field(field.key_type_name(), &self.try_borrow_erased(field));
        }

        f.finish()
    }
}

impl<S: LateStruct> LateInstanceDyn<S> {
    /// Obtains a reference to the underlying [`LateInstance`].
    pub fn non_dynamic(&mut self) -> &mut LateInstance<S> {
        &mut self.inner
    }

    /// Forwards to [`LateInstance::init_token`].
    pub fn init_token(&self) -> LateLayoutInitToken {
        self.inner.init_token
    }

    /// Forwards to [`LateInstance::fields`].
    pub fn fields(&self) -> &'static [&'static LateFieldDescriptor<S>] {
        self.inner.fields()
    }

    /// Forwards to [`LateInstance::get_ptr`].
    pub fn get_ptr<F: LateField<S>>(&self) -> NonNull<F::Value> {
        self.inner.get_ptr::<F>()
    }

    /// Forwards to [`LateInstance::get_erased_ptr`].
    pub fn get_erased_ptr(&self, field: &LateFieldDescriptor<S>) -> NonNull<S::EraseTo> {
        self.inner.get_erased_ptr(field)
    }

    /// Forwards to [`LateInstance::get_mut`].
    pub fn get_mut<F: LateField<S>>(&mut self) -> &mut F::Value {
        self.inner.get_mut::<F>()
    }

    /// Borrow the field identified by the supplied [`LateField`] marker type immutably, panicking
    /// if the borrow failed.
    ///
    /// The smart-pointers returned by this method are compatible with those returned by
    /// [`RefCell`].
    pub fn borrow<F: LateField<S>>(&self) -> Ref<'_, F::Value> {
        Ref::map(
            self.inner.cells[F::descriptor().index(self.inner.init_token)].borrow(),
            |()| unsafe { self.get_ptr::<F>().as_ref() },
        )
    }

    /// Borrow the field identified by the supplied [`LateField`] marker type mutably, panicking
    /// if the borrow failed.
    ///
    /// The smart-pointers returned by this method are compatible with those returned by
    /// [`RefCell`].
    pub fn borrow_mut<F: LateField<S>>(&self) -> RefMut<'_, F::Value> {
        RefMut::map(
            self.inner.cells[F::descriptor().index(self.inner.init_token)].borrow_mut(),
            |()| unsafe { self.get_ptr::<F>().as_mut() },
        )
    }

    /// Borrow the field identified by the supplied [`LateField`] marker type immutably or return a
    /// [`cell::BorrowError`] if the borrow failed.
    ///
    /// The smart-pointers and errors returned by this method are compatible with those returned by
    /// [`RefCell`].
    pub fn try_borrow<F: LateField<S>>(&self) -> Result<Ref<'_, F::Value>, cell::BorrowError> {
        self.inner.cells[F::descriptor().index(self.inner.init_token)]
            .try_borrow()
            .map(|field| Ref::map(field, |()| unsafe { self.get_ptr::<F>().as_ref() }))
    }

    /// Borrow the field identified by the supplied [`LateField`] marker type mutably or return a
    /// [`cell::BorrowError`] if the borrow failed.
    ///
    /// The smart-pointers and errors returned by this method are compatible with those returned by
    /// [`RefCell`].
    pub fn try_borrow_mut<F: LateField<S>>(
        &self,
    ) -> Result<RefMut<'_, F::Value>, cell::BorrowMutError> {
        self.inner.cells[F::descriptor().index(self.inner.init_token)]
            .try_borrow_mut()
            .map(|field| RefMut::map(field, |()| unsafe { self.get_ptr::<F>().as_mut() }))
    }

    /// Borrow the field identified by the supplied [`LateFieldDescriptor`] immutably, panicking
    /// if the borrow failed.
    ///
    /// The smart-pointers returned by this method are compatible with those returned by
    /// [`RefCell`].
    pub fn borrow_erased<'a>(&'a self, field: &LateFieldDescriptor<S>) -> Ref<'a, S::EraseTo> {
        Ref::map(
            self.inner.cells[field.index(self.inner.init_token)].borrow(),
            |()| unsafe { self.get_erased_ptr(field).as_ref() },
        )
    }

    /// Borrow the field identified by the supplied [`LateFieldDescriptor`] mutably, panicking
    /// if the borrow failed.
    ///
    /// The smart-pointers returned by this method are compatible with those returned by
    /// [`RefCell`].
    pub fn borrow_erased_mut<'a>(
        &'a self,
        field: &LateFieldDescriptor<S>,
    ) -> RefMut<'a, S::EraseTo> {
        RefMut::map(
            self.inner.cells[field.index(self.inner.init_token)].borrow_mut(),
            |()| unsafe { self.get_erased_ptr(field).as_mut() },
        )
    }

    /// Borrow the field identified by the supplied [`LateFieldDescriptor`] immutably or return a
    /// [`cell::BorrowError`] if the borrow failed.
    ///
    /// The smart-pointers and errors returned by this method are compatible with those returned by
    /// [`RefCell`].
    pub fn try_borrow_erased<'a>(
        &'a self,
        field: &LateFieldDescriptor<S>,
    ) -> Result<Ref<'a, S::EraseTo>, cell::BorrowError> {
        self.inner.cells[field.index(self.inner.init_token)]
            .try_borrow()
            .map(|field_guard| {
                Ref::map(field_guard, |()| unsafe {
                    self.get_erased_ptr(field).as_ref()
                })
            })
    }

    /// Borrow the field identified by the supplied [`LateFieldDescriptor`] mutably or return a
    /// [`cell::BorrowMutError`] if the borrow failed.
    ///
    /// The smart-pointers and errors returned by this method are compatible with those returned by
    /// [`RefCell`].
    pub fn try_borrow_erased_mut<'a>(
        &'a self,
        field: &LateFieldDescriptor<S>,
    ) -> Result<RefMut<'a, S::EraseTo>, cell::BorrowMutError> {
        self.inner.cells[field.index(self.inner.init_token)]
            .try_borrow_mut()
            .map(|field_guard| {
                RefMut::map(field_guard, |()| unsafe {
                    self.get_erased_ptr(field).as_mut()
                })
            })
    }
}
