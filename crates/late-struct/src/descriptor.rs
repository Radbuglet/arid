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

/// A raw (untyped) descriptor for a [`LateStruct`].
///
/// You can obtain a reference to this descriptor using [`LateStruct::raw_descriptor`].
///
/// Unlike [`LateStructDescriptor`], the type of the `LateStruct` to which this descriptor belongs
/// is erased.
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
    // This needs to be accessed in `traits`
    pub(crate) const fn new<S: LateStruct>() -> Self {
        Self {
            size: AtomicUsize::new(0),
            align: AtomicUsize::new(0),
            fields: AtomicPtr::new(ptr::null_mut()),
            type_name: type_name::<S>,
            type_id: TypeId::of::<S>,
        }
    }

    /// Fetches the name of the type for which [`LateStruct`] is implemented.
    pub fn type_name(&self) -> &'static str {
        (self.type_name)()
    }

    /// Fetches the [`TypeId`] of the type for which [`LateStruct`] is implemented.
    pub fn type_id(&self) -> TypeId {
        (self.type_id)()
    }

    /// Fetches the overall layout of the structure generated dynamically for this structure.
    pub fn layout(&self, token: LateLayoutInitToken) -> Layout {
        let _ = token;

        unsafe {
            Layout::from_size_align_unchecked(self.size.load(Relaxed), self.align.load(Relaxed))
        }
    }

    /// Fetches the list of fields contained within this structure.
    ///
    /// The order of this list is in no way stable across multiple compilations.
    pub fn fields(&self, token: LateLayoutInitToken) -> &'static [&'static RawLateFieldDescriptor] {
        let _ = token;

        unsafe { &**self.fields.load(Relaxed) }
    }

    /// Downcasts the descriptor to a typed [`LateStructDescriptor`] instance.
    ///
    /// `S` must match the type of the [`LateStruct`] marker this descriptor is describing or the
    /// method will panic.
    pub fn typed<S: LateStruct>(&self) -> &LateStructDescriptor<S> {
        LateStructDescriptor::wrap(self)
    }

    /// Downcasts the descriptor to a typed [`LateStructDescriptor`] instance.
    ///
    /// This operation can be safely performed with [`RawLateStructDescriptor::typed`].
    ///
    /// ## Safety
    ///
    /// `S` must match the type of the [`LateStruct`] marker this descriptor is describing. Failing
    /// to do so will not cause immediate undefined behavior but could cause latent undefined
    /// behavior if passed to a method expecting the descriptor to belong to a certain
    /// [`LateStruct`] type.
    pub unsafe fn typed_unchecked<S: LateStruct>(&self) -> &LateStructDescriptor<S> {
        unsafe { LateStructDescriptor::wrap_unchecked(self) }
    }
}

/// A raw (untyped) descriptor for a [`LateField`].
///
/// You can obtain a reference to this descriptor using [`LateField::raw_descriptor`].
///
/// Unlike [`LateFieldDescriptor`], the type of the [`LateStruct`] to which this descriptor belongs
/// is erased. There is no descriptor which also specifies the type of the [`LateField`] since it
/// would be redundant.
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
    pub(crate) const fn new<S, F>() -> Self
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
            parent_struct: S::raw_descriptor,
        }
    }

    /// Fetches the descriptor of the struct to which this field belongs.
    pub fn parent_struct(&self) -> &'static RawLateStructDescriptor {
        (self.parent_struct)()
    }

    /// Fetches the index of the field within the [RawLateStructDescriptor::fields] list of the
    /// struct to which this field belongs. The ordering of these indices is not stable across
    /// compilations.
    pub fn index(&self, token: LateLayoutInitToken) -> usize {
        let _ = token;

        self.index.load(Relaxed)
    }

    /// Fetches the offset of this field's value within the overall struct.
    pub fn offset(&self, token: LateLayoutInitToken) -> usize {
        let _ = token;

        self.offset.load(Relaxed)
    }

    /// Fetches the layout of the field's value.
    pub fn layout(&self) -> Layout {
        self.layout
    }

    /// Initializes `value` to an instance of the field's value obtained by calling its [`Default`]
    /// implementation.
    ///
    /// ## Safety
    ///
    /// `value` must point to an uninitialized location in memory large enough to accommodate the
    /// field's value.
    pub unsafe fn init(&self, value: *mut u8) {
        unsafe { (self.init)(value) }
    }

    /// Drops the instance of the field's value pointed to by `value`.
    ///
    /// ## Safety
    ///
    /// `value` must point to an initialized instance of the field's value.
    pub unsafe fn drop(&self, value: *mut u8) {
        unsafe { (self.drop)(value) }
    }

    /// Adorns an untyped pointer in memory with the metadata required to become a pointer to
    /// the specified [`LateStruct`]'s [`S::EraseTo`](LateStruct::EraseTo) type.
    ///
    /// `value` need not point to a valid location in memory.
    ///
    /// This operation can be performed safely by converting this descriptor into a
    /// [`LateFieldDescriptor`] using the [`RawLateFieldDescriptor::typed`] method and calling its
    /// [`LateFieldDescriptor::erase_value`] method.
    ///
    /// ## Safety
    ///
    /// `S` must match the type of the [`LateStruct`] marker this descriptor is describing.
    ///
    pub unsafe fn erase_value<S: LateStruct>(&self, value: *mut u8) -> *mut S::EraseTo {
        debug_assert_eq!(TypeId::of::<S>(), self.parent_struct().type_id());

        let mut out = MaybeUninit::<*mut S::EraseTo>::uninit();

        unsafe {
            (self.as_erased)(value, out.as_mut_ptr().cast());
            out.assume_init()
        }
    }

    /// Fetches the [`type_name`] of the [`LateField`] instance—that is, the name of the marker type.
    pub fn key_type_name(&self) -> &'static str {
        (self.key_type_name)()
    }

    /// Fetches the [`TypeId`] of the [`LateField`] instance—that is, the ID of the marker type.
    pub fn key_type_id(&self) -> TypeId {
        (self.key_type_id)()
    }

    /// Downcasts the descriptor to a typed [`LateFieldDescriptor`] instance.
    ///
    /// `S` must match the type of the [`LateStruct`] marker to which this descriptor belongs or the
    /// method will panic.
    pub fn typed<S: LateStruct>(&self) -> &LateFieldDescriptor<S> {
        LateFieldDescriptor::wrap(self)
    }

    /// Downcasts the descriptor to a typed [`LateFieldDescriptor`] instance.
    ///
    /// This operation can be safely performed with [`RawLateFieldDescriptor::typed`].
    ///
    /// ## Safety
    ///
    /// `S` must match the type of the [`LateStruct`] marker to which this descriptor belongs.
    /// Failing to do so will not cause immediate undefined behavior but could cause latent
    /// undefined behavior if passed to a method expecting the descriptor to belong to a certain
    /// [`LateStruct`] type.
    pub unsafe fn typed_unchecked<S: LateStruct>(&self) -> &LateFieldDescriptor<S> {
        unsafe { LateFieldDescriptor::wrap_unchecked(self) }
    }
}

// === Newtypes === //

/// A typed descriptor for a [`LateStruct`].
///
/// You can obtain a reference to this descriptor using [`LateStruct::descriptor`].
///
/// Unlike [`RawLateStructDescriptor`], this descriptor encodes the type of the `LateStruct` it's
/// describing.
#[derive(Debug)]
#[repr(transparent)]
pub struct LateStructDescriptor<S: LateStruct> {
    _ty: PhantomData<fn(S) -> S>,
    raw: RawLateStructDescriptor,
}

// Conversions
impl<S: LateStruct> LateStructDescriptor<S> {
    fn wrap(raw: &RawLateStructDescriptor) -> &LateStructDescriptor<S> {
        assert_eq!(raw.type_id(), TypeId::of::<S>());

        unsafe { Self::wrap_unchecked(raw) }
    }

    const unsafe fn wrap_unchecked(raw: &RawLateStructDescriptor) -> &LateStructDescriptor<S> {
        unsafe { &*(raw as *const RawLateStructDescriptor as *const LateStructDescriptor<S>) }
    }

    /// Erases the type information from this descriptor, producing its equivalent
    /// [`RawLateStructDescriptor`].
    pub const fn raw(&self) -> &RawLateStructDescriptor {
        &self.raw
    }
}

// Forwards
impl<S: LateStruct> LateStructDescriptor<S> {
    /// Forwards to [`RawLateStructDescriptor::layout`].
    pub fn layout(&self, token: LateLayoutInitToken) -> Layout {
        self.raw.layout(token)
    }

    /// Forwards to [`RawLateStructDescriptor::fields`].
    pub fn fields(&self, token: LateLayoutInitToken) -> &'static [&'static LateFieldDescriptor<S>] {
        unsafe { LateFieldDescriptor::wrap_slice_unchecked(self.raw.fields(token)) }
    }
}

/// A typed descriptor for a [`LateField`].
///
/// You can obtain a reference to this descriptor using [`LateField::descriptor`].
///
/// Unlike [`LateFieldDescriptor`], this descriptor encodes the type of the `LateStruct` to which
/// the field belongs.
#[derive(Debug)]
#[repr(transparent)]
pub struct LateFieldDescriptor<S: LateStruct> {
    _ty: PhantomData<fn(S) -> S>,
    raw: RawLateFieldDescriptor,
}

// Conversions
impl<S: LateStruct> LateFieldDescriptor<S> {
    fn wrap(raw: &RawLateFieldDescriptor) -> &LateFieldDescriptor<S> {
        assert_eq!(raw.parent_struct().type_id(), TypeId::of::<S>());

        unsafe { Self::wrap_unchecked(raw) }
    }

    const unsafe fn wrap_unchecked(raw: &RawLateFieldDescriptor) -> &LateFieldDescriptor<S> {
        unsafe { &*(raw as *const RawLateFieldDescriptor as *const LateFieldDescriptor<S>) }
    }

    const unsafe fn wrap_slice_unchecked<'a, 'b>(
        raw: &'a [&'b RawLateFieldDescriptor],
    ) -> &'a [&'b LateFieldDescriptor<S>] {
        unsafe {
            slice::from_raw_parts(raw.as_ptr().cast::<&'b LateFieldDescriptor<S>>(), raw.len())
        }
    }

    /// Erases the type information from this descriptor, producing its equivalent
    /// [`RawLateFieldDescriptor`].
    pub const fn raw(&self) -> &RawLateFieldDescriptor {
        &self.raw
    }
}

// Forwards
impl<S: LateStruct> LateFieldDescriptor<S> {
    /// Forwards to [`RawLateFieldDescriptor::index`].
    pub fn index(&self, token: LateLayoutInitToken) -> usize {
        self.raw.index(token)
    }

    /// Forwards to [`RawLateFieldDescriptor::offset`].
    pub fn offset(&self, token: LateLayoutInitToken) -> usize {
        self.raw.offset(token)
    }

    /// Forwards to [`RawLateFieldDescriptor::layout`].
    pub fn layout(&self) -> Layout {
        self.raw.layout()
    }

    /// Forwards to [`RawLateFieldDescriptor::init`].
    ///
    /// ## Safety
    ///
    /// See safety docs of `RawLateFieldDescriptor::init`.
    ///
    pub unsafe fn init(&self, value: *mut u8) {
        unsafe { self.raw.init(value) }
    }

    /// Forwards to [`RawLateFieldDescriptor::drop`].
    ///
    /// ## Safety
    ///
    /// See safety docs of `RawLateFieldDescriptor::drop`.
    ///
    pub unsafe fn drop(&self, value: *mut u8) {
        unsafe { self.raw.drop(value) }
    }

    /// Forwards to [`RawLateFieldDescriptor::erase_value`] but uses type information to make the
    /// operation safe.
    #[allow(clippy::not_unsafe_ptr_arg_deref)] // We don't dereference `value`
    pub fn erase_value(&self, value: *mut u8) -> *mut S::EraseTo {
        unsafe { self.raw.erase_value::<S>(value) }
    }

    /// Forwards to [`RawLateFieldDescriptor::key_type_name`].
    pub fn key_type_name(&self) -> &'static str {
        self.raw.key_type_name()
    }

    /// Forwards to [`RawLateFieldDescriptor::key_type_id`].
    pub fn key_type_id(&self) -> TypeId {
        self.raw.key_type_id()
    }

    /// Forwards to [`RawLateFieldDescriptor::parent_struct`] but preserves the type annotation.
    pub fn parent_struct(&self) -> &'static LateStructDescriptor<S> {
        unsafe { self.raw.parent_struct().typed_unchecked() }
    }
}
