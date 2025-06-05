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

impl<S: LateStruct> LateInstance<S> {
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
            init(&field, unsafe {
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

    pub fn new() -> Self {
        unsafe {
            Self::new_custom(|field, ptr| {
                field.init(ptr);
            })
        }
    }

    pub fn init_token(&self) -> LateLayoutInitToken {
        self.init_token
    }

    pub fn fields(&self) -> &'static [&'static LateFieldDescriptor<S>] {
        S::descriptor().fields(self.init_token)
    }

    pub fn base_ptr(&self) -> NonNull<u8> {
        self.data_base
    }

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

    pub fn get_erased(&self, field: &LateFieldDescriptor<S>) -> &S::EraseTo {
        unsafe { self.get_erased_ptr(field).as_ref() }
    }

    pub fn get_erased_mut(&mut self, field: &LateFieldDescriptor<S>) -> &mut S::EraseTo {
        unsafe { self.get_erased_ptr(field).as_mut() }
    }

    pub fn get_ptr<F: LateField<S>>(&self) -> NonNull<F::Value> {
        unsafe {
            self.data_base
                .add(F::descriptor().offset(self.init_token))
                .cast()
        }
    }

    pub fn get_two<F, G>(&mut self) -> (&mut F::Value, &mut G::Value)
    where
        F: LateField<S>,
        G: LateField<S>,
    {
        assert_ne!(TypeId::of::<F>(), TypeId::of::<G>());

        unsafe { (self.get_ptr::<F>().as_mut(), self.get_ptr::<G>().as_mut()) }
    }

    pub fn get<F: LateField<S>>(&self) -> &F::Value {
        unsafe { self.get_ptr::<F>().as_ref() }
    }

    pub fn get_mut<F: LateField<S>>(&mut self) -> &mut F::Value {
        unsafe { self.get_ptr::<F>().as_mut() }
    }

    pub fn dynamic(&mut self) -> &mut LateInstanceDyn<S> {
        unsafe { &mut *(self as *mut Self as *mut LateInstanceDyn<S>) }
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
    pub fn non_dynamic(&mut self) -> &mut LateInstance<S> {
        &mut self.inner
    }

    pub fn init_token(&self) -> LateLayoutInitToken {
        self.inner.init_token
    }

    pub fn fields(&self) -> &'static [&'static LateFieldDescriptor<S>] {
        self.inner.fields()
    }

    pub fn get_ptr<F: LateField<S>>(&self) -> NonNull<F::Value> {
        self.inner.get_ptr::<F>()
    }

    pub fn get_erased_ptr(&self, field: &LateFieldDescriptor<S>) -> NonNull<S::EraseTo> {
        self.inner.get_erased_ptr(field)
    }

    pub fn get_mut<F: LateField<S>>(&mut self) -> &mut F::Value {
        self.inner.get_mut::<F>()
    }

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

    pub fn borrow_erased<'a>(&'a self, field: &LateFieldDescriptor<S>) -> Ref<'a, S::EraseTo> {
        Ref::map(
            self.inner.cells[field.index(self.inner.init_token)].borrow(),
            |()| unsafe { self.get_erased_ptr(field).as_ref() },
        )
    }

    pub fn borrow_erased_mut<'a>(
        &'a self,
        field: &LateFieldDescriptor<S>,
    ) -> RefMut<'a, S::EraseTo> {
        RefMut::map(
            self.inner.cells[field.index(self.inner.init_token)].borrow_mut(),
            |()| unsafe { self.get_erased_ptr(field).as_mut() },
        )
    }

    pub fn try_borrow<F: LateField<S>>(&self) -> Result<Ref<'_, F::Value>, cell::BorrowError> {
        self.inner.cells[F::descriptor().index(self.inner.init_token)]
            .try_borrow()
            .map(|field| Ref::map(field, |()| unsafe { self.get_ptr::<F>().as_ref() }))
    }

    pub fn try_borrow_mut<F: LateField<S>>(
        &self,
    ) -> Result<RefMut<'_, F::Value>, cell::BorrowMutError> {
        self.inner.cells[F::descriptor().index(self.inner.init_token)]
            .try_borrow_mut()
            .map(|field| RefMut::map(field, |()| unsafe { self.get_ptr::<F>().as_mut() }))
    }

    pub fn borrow<F: LateField<S>>(&self) -> Ref<'_, F::Value> {
        Ref::map(
            self.inner.cells[F::descriptor().index(self.inner.init_token)].borrow(),
            |()| unsafe { self.get_ptr::<F>().as_ref() },
        )
    }

    pub fn borrow_mut<F: LateField<S>>(&self) -> RefMut<'_, F::Value> {
        RefMut::map(
            self.inner.cells[F::descriptor().index(self.inner.init_token)].borrow_mut(),
            |()| unsafe { self.get_ptr::<F>().as_mut() },
        )
    }
}
