use std::{
    alloc::Layout,
    any::{TypeId, type_name},
    marker::PhantomData,
    mem::MaybeUninit,
    ptr, slice,
    sync::atomic::{AtomicPtr, AtomicUsize, Ordering::*},
};

use crate::{LateField, LateLayoutInitToken, LateStruct};

// === Raw === //

#[derive(Debug)]
pub struct RawLateStructDescriptor {
    // These need to be accessed in `init`.
    pub(crate) size: AtomicUsize,
    pub(crate) align: AtomicUsize,
    pub(crate) fields: AtomicPtr<&'static [&'static RawLateFieldDescriptor]>,

    type_name: fn() -> &'static str,
    type_id: fn() -> TypeId,
}

impl RawLateStructDescriptor {
    pub const fn new<S: LateStruct>() -> Self {
        Self {
            size: AtomicUsize::new(0),
            align: AtomicUsize::new(0),
            fields: AtomicPtr::new(ptr::null_mut()),
            type_name: type_name::<S>,
            type_id: TypeId::of::<S>,
        }
    }

    pub fn type_name(&self) -> &'static str {
        (self.type_name)()
    }

    pub fn type_id(&self) -> TypeId {
        (self.type_id)()
    }

    pub fn layout(&self, token: LateLayoutInitToken) -> Layout {
        let _ = token;

        unsafe {
            Layout::from_size_align_unchecked(self.size.load(Relaxed), self.align.load(Relaxed))
        }
    }

    pub fn fields(&self, token: LateLayoutInitToken) -> &'static [&'static RawLateFieldDescriptor] {
        let _ = token;

        unsafe { &**self.fields.load(Relaxed) }
    }

    pub fn typed<S: LateStruct>(&self) -> &LateStructDescriptor<S> {
        LateStructDescriptor::wrap(self)
    }

    pub unsafe fn typed_unchecked<S: LateStruct>(&self) -> &LateStructDescriptor<S> {
        unsafe { LateStructDescriptor::wrap_unchecked(self) }
    }
}

#[derive(Debug)]
pub struct RawLateFieldDescriptor {
    /// The index of this field in the struct.
    // This needs to be accessed in `init`.
    pub(crate) index: AtomicUsize,

    /// The byte offset of this field in the struct.
    // This needs to be accessed in `init`.
    pub(crate) offset: AtomicUsize,

    /// The layout of this field's type.
    layout: Layout,

    /// Initializes the supplied `*mut MaybeUninit<Key::Value>`.
    init: unsafe fn(*mut u8),

    /// Drops the supplied `*mut Key::Value` in place.
    drop: unsafe fn(*mut u8),

    /// Transforms the supplied `*mut Key::Value` in the first parameter into a
    /// `*mut Struct::EraseTo` and writes it out into the `*mut *mut Struct::EraseTo` pointee in the
    /// second parameter.
    as_erased: fn(*mut u8, *mut ()),

    /// Fetches the type name of `Key`.
    key_type_name: fn() -> &'static str,

    /// Fetches the type ID of `Key`.
    key_type_id: fn() -> TypeId,

    /// Fetches the type ID of `Struct`.
    parent_struct: fn() -> &'static RawLateStructDescriptor,
}

impl RawLateFieldDescriptor {
    pub const fn new<S, F>() -> Self
    where
        S: LateStruct,
        F: LateField<S>,
    {
        Self {
            index: AtomicUsize::new(usize::MAX),
            offset: AtomicUsize::new(usize::MAX),
            layout: Layout::new::<F::Value>(),
            init: |ptr| unsafe { ptr.cast::<F::Value>().write(<F::Value>::default()) },
            drop: |ptr| unsafe { ptr.cast::<F::Value>().drop_in_place() },
            as_erased: |ptr, write_out| unsafe {
                write_out
                    .cast::<*mut S::EraseTo>()
                    .write(F::coerce(ptr.cast::<F::Value>()));
            },
            key_type_name: type_name::<F>,
            key_type_id: TypeId::of::<F>,
            parent_struct: S::descriptor,
        }
    }

    pub fn index(&self, token: LateLayoutInitToken) -> usize {
        let _ = token;

        self.index.load(Relaxed)
    }

    pub fn offset(&self, token: LateLayoutInitToken) -> usize {
        let _ = token;

        self.offset.load(Relaxed)
    }

    pub fn layout(&self) -> Layout {
        self.layout
    }

    pub unsafe fn init(&self, value: *mut u8) {
        unsafe { (self.init)(value) }
    }

    pub unsafe fn drop(&self, value: *mut u8) {
        unsafe { (self.drop)(value) }
    }

    pub unsafe fn erase_value<S: LateStruct>(&self, value: *mut u8) -> *mut S::EraseTo {
        debug_assert_eq!(TypeId::of::<S>(), self.parent_struct().type_id());

        let mut out = MaybeUninit::<*mut S::EraseTo>::uninit();

        unsafe {
            (self.as_erased)(value, out.as_mut_ptr().cast());
            out.assume_init()
        }
    }

    pub fn key_type_name(&self) -> &'static str {
        (self.key_type_name)()
    }

    pub fn key_type_id(&self) -> TypeId {
        (self.key_type_id)()
    }

    pub fn parent_struct(&self) -> &'static RawLateStructDescriptor {
        (self.parent_struct)()
    }

    pub fn typed<S: LateStruct>(&self) -> &LateFieldDescriptor<S> {
        LateFieldDescriptor::wrap(self)
    }

    pub unsafe fn typed_unchecked<S: LateStruct>(&self) -> &LateFieldDescriptor<S> {
        unsafe { LateFieldDescriptor::wrap_unchecked(self) }
    }
}

// === Newtypes === //

#[derive(Debug)]
#[repr(transparent)]
pub struct LateStructDescriptor<S: LateStruct> {
    _ty: PhantomData<fn(S) -> S>,
    raw: RawLateStructDescriptor,
}

// Conversions
impl<S: LateStruct> LateStructDescriptor<S> {
    pub fn wrap(raw: &RawLateStructDescriptor) -> &LateStructDescriptor<S> {
        assert_eq!(raw.type_id(), TypeId::of::<S>());

        unsafe { Self::wrap_unchecked(raw) }
    }

    pub const unsafe fn wrap_unchecked(raw: &RawLateStructDescriptor) -> &LateStructDescriptor<S> {
        unsafe { &*(raw as *const RawLateStructDescriptor as *const LateStructDescriptor<S>) }
    }

    pub const fn raw(&self) -> &RawLateStructDescriptor {
        &self.raw
    }
}

// Forwards
impl<S: LateStruct> LateStructDescriptor<S> {
    pub fn layout(&self, token: LateLayoutInitToken) -> Layout {
        self.raw.layout(token)
    }

    pub fn fields(&self, token: LateLayoutInitToken) -> &'static [&'static LateFieldDescriptor<S>] {
        unsafe { LateFieldDescriptor::wrap_slice_unchecked(self.raw.fields(token)) }
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct LateFieldDescriptor<S: LateStruct> {
    _ty: PhantomData<fn(S) -> S>,
    raw: RawLateFieldDescriptor,
}

// Conversions
impl<S: LateStruct> LateFieldDescriptor<S> {
    pub fn wrap(raw: &RawLateFieldDescriptor) -> &LateFieldDescriptor<S> {
        assert_eq!(raw.parent_struct().type_id(), TypeId::of::<S>());

        unsafe { Self::wrap_unchecked(raw) }
    }

    pub const unsafe fn wrap_unchecked(raw: &RawLateFieldDescriptor) -> &LateFieldDescriptor<S> {
        unsafe { &*(raw as *const RawLateFieldDescriptor as *const LateFieldDescriptor<S>) }
    }

    pub const unsafe fn wrap_slice_unchecked<'a, 'b>(
        raw: &'a [&'b RawLateFieldDescriptor],
    ) -> &'a [&'b LateFieldDescriptor<S>] {
        unsafe {
            slice::from_raw_parts(raw.as_ptr().cast::<&'b LateFieldDescriptor<S>>(), raw.len())
        }
    }

    pub const fn raw(&self) -> &RawLateFieldDescriptor {
        &self.raw
    }

    pub fn slice_as_raw<'a, 'b>(
        me: &'a [&'b LateFieldDescriptor<S>],
    ) -> &'a [&'b RawLateFieldDescriptor] {
        unsafe { slice::from_raw_parts(me.as_ptr().cast::<&'b RawLateFieldDescriptor>(), me.len()) }
    }
}

// Forwards
impl<S: LateStruct> LateFieldDescriptor<S> {
    pub fn parent_struct(&self) -> &'static LateStructDescriptor<S> {
        unsafe { self.raw.parent_struct().typed_unchecked() }
    }

    #[allow(clippy::not_unsafe_ptr_arg_deref)] // We don't dereference `valu`
    pub fn erase_value(&self, value: *mut u8) -> *mut S::EraseTo {
        unsafe { self.raw.erase_value::<S>(value) }
    }
}
