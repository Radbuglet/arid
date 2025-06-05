//! Late-bound structure definitions.
//!
//! This crate exposes the [`late_struct`] macro, which defines a structure whose set of fields
//! can be extended by any crate within a compiled artifact using the [`late_field`] macro.
//! Unlike regular structures, dependents on the crate which originally defined the structure are
//! allowed to extend it. Additionally, the structure we defined can be instantiated in any crate
//! using a [`LateInstance`], even if dependents of that crate are still extending it.
//!
//! ## Basic Usage
//!
//! For example, let's say we had a crate hierarchy where "`dependent` depends on `dependency`."
//!
//! In `dependency`, we could define a new late-struct marker...
//!
//! ```
//! use late_struct::late_struct;
//!
//! // Marker type for our application context.
//! // Any type could be used here.
//! pub struct AppContext;
//!
//! late_struct!(AppContext);
//! ```
//!
//! ...and then, in `dependent`, we could proceed to add a field to it like so:
//!
//! ```
//! # mod dependency {
//! #     use late_struct::late_struct;
//! #     pub struct AppContext;
//! #     late_struct!(AppContext);
//! # }
//! use late_struct::late_field;
//!
//! use dependency::AppContext;
//!
//! #[derive(Debug, Default)]  // Type must implement `Debug`, `Default`, and live for `'static`.
//! pub struct MyField(Vec<u32>);
//!
//! late_field!(MyField[AppContext]);
//! ```
//!
//! Just note that the field value must implement [`Debug`], [`Default`], and live for `'static`.
//!
//! We can then refer to the structure we've created with a [`LateInstance`]. For example, back in
//! `dependency`, we can write...
//!
//! ```
//! # use late_struct::late_struct;
//! # pub struct AppContext;
//! # late_struct!(AppContext);
//! use late_struct::LateInstance;
//!
//! pub fn create_my_instance() -> LateInstance<AppContext> {
//!     LateInstance::new()
//! }
//! ```
//!
//! ...even though downstream crates such as `dependent` are still adding fields to it. Finally, we
//! can access fields using the [`LateInstance::get`] and [`LateInstance::get_mut`] methods. For
//! example, in the `dependent` crate, we could write...
//!
//! ```
//! # mod dependency {
//! #     use late_struct::{late_struct, LateInstance};
//! #
//! #     pub struct AppContext;
//! #
//! #     late_struct!(AppContext);
//! #
//! #     pub fn create_my_instance() -> LateInstance<AppContext> {
//! #         LateInstance::new()
//! #     }
//! # }
//! use dependency::{AppContext, create_my_instance};
//! # use late_struct::late_field;
//! #
//! # #[derive(Debug, Default)]  // Type must implement `Default` and live for `'static`.
//! # pub struct MyField(Vec<u32>);
//! #
//! # late_field!(MyField[AppContext]);
//!
//! pub fn example() {
//!     let mut instance = create_my_instance();
//!
//!     instance.get_mut::<MyField>().0.push(1);
//!     instance.get_mut::<MyField>().0.push(2);
//!     instance.get_mut::<MyField>().0.push(3);
//!
//!     eprintln!("Our numbers are {:?}", instance.get::<MyField>());
//! }
//! ```
//!
//! See the documentation of [`LateInstance`] for more ways to access the instance.
//!
//! ## Advanced Usage
//!
//! TODO
//!
//! ## Limitations
//!
//! TODO
//!

use std::{
    alloc::{self, Layout},
    any::{TypeId, type_name},
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt,
    marker::PhantomData,
    mem::MaybeUninit,
    ptr::{self, NonNull},
    slice,
    sync::{
        Once,
        atomic::{AtomicPtr, AtomicUsize, Ordering::Relaxed},
    },
};

use scopeguard::ScopeGuard;

// === Definitions === //

pub unsafe trait LateStruct: 'static {
    type EraseTo: ?Sized + 'static;

    fn descriptor() -> &'static RawLateStructDescriptor;
}

pub unsafe trait LateField<S: LateStruct>: 'static {
    type Value: 'static + Default;

    fn descriptor() -> &'static RawLateFieldDescriptor;

    fn coerce(value: *mut Self::Value) -> *mut S::EraseTo;
}

#[derive(Debug)]
pub struct RawLateStructDescriptor {
    size: AtomicUsize,
    align: AtomicUsize,
    fields: AtomicPtr<&'static [&'static RawLateFieldDescriptor]>,
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
    index: AtomicUsize,

    /// The byte offset of this field in the struct.
    offset: AtomicUsize,

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

// === Descriptor New-types === //

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

// === LateLayoutInitToken === //

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub struct LateLayoutInitToken;

impl Default for LateLayoutInitToken {
    fn default() -> Self {
        Self::new()
    }
}

impl LateLayoutInitToken {
    pub unsafe fn new_unchecked() -> Self {
        Self
    }

    pub fn new() -> Self {
        static ONCE: Once = Once::new();

        ONCE.call_once(|| {
            use self::late_macro_internals::{LATE_FIELDS, LATE_STRUCTS};

            let mut structs = HashMap::<
                TypeId,
                (
                    &'static RawLateStructDescriptor,
                    Vec<&'static RawLateFieldDescriptor>,
                ),
            >::new();

            for entry in LATE_STRUCTS {
                let entry = entry();

                structs.insert(entry.struct_type, (entry.descriptor, Vec::new()));
            }

            for entry in LATE_FIELDS {
                let entry = entry();

                structs
                    .get_mut(&entry.struct_type)
                    .unwrap()
                    .1
                    .push(entry.descriptor);
            }

            for (struct_desc, struct_fields) in structs.into_values() {
                let struct_fields: &[_] = &*Box::leak(Box::from_iter(struct_fields));
                let struct_fields_p = Box::leak(Box::new(struct_fields));

                let mut overall_layout = Layout::new::<()>();

                for (i, field) in struct_fields.iter().enumerate() {
                    let (new_layout, offset) = overall_layout.extend(field.layout).unwrap();
                    field.index.store(i, Relaxed);
                    field.offset.store(offset, Relaxed);
                    overall_layout = new_layout;
                }

                struct_desc.size.store(overall_layout.size(), Relaxed);
                struct_desc.align.store(overall_layout.align(), Relaxed);
                struct_desc.fields.store(struct_fields_p, Relaxed);
            }
        });

        Self
    }
}

// === Macros === //

#[doc(hidden)]
pub mod late_macro_internals {
    use std::{any::TypeId, fmt};

    pub use {
        super::{
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
                descriptor: S::descriptor(),
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
                descriptor: F::descriptor(),
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

                fn descriptor() -> &'static $crate::late_macro_internals::RawLateStructDescriptor {
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

                fn descriptor() -> &'static $crate::late_macro_internals::RawLateFieldDescriptor {
                    &DESCRIPTOR
                }

                fn coerce(value: *mut Self::Value) -> *mut <$ns as $crate::late_macro_internals::LateStruct>::EraseTo {
                    value
                }
            }
        };
    };
}

// === Instances === //

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

        for field in S::descriptor().fields(self.init_token) {
            f.field(field.key_type_name(), &unsafe {
                self.get_erased_ptr_unchecked(field)
            });
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
    pub fn new() -> Self {
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
            unsafe { field.init(data_base.add(field.offset(init_token)).as_ptr()) };
            *drop_guard += 1;
        }

        // Defuse guards
        ScopeGuard::into_inner(dealloc_guard);
        ScopeGuard::into_inner(drop_guard);

        Self {
            _ty: PhantomData,
            init_token,
            data_base,
            cells: Box::from_iter((0..struct_fields.len()).map(|_| RefCell::new(()))),
        }
    }

    pub fn init_token(&self) -> LateLayoutInitToken {
        self.init_token
    }

    pub fn base_ptr(&self) -> NonNull<u8> {
        self.data_base
    }

    pub fn field_count(&self) -> usize {
        S::descriptor().fields(self.init_token).len()
    }

    pub unsafe fn get_erased_ptr_unchecked(
        &self,
        field: &RawLateFieldDescriptor,
    ) -> NonNull<S::EraseTo> {
        debug_assert_eq!(
            field.parent_struct() as *const _,
            S::descriptor() as *const _
        );

        unsafe {
            NonNull::new_unchecked(
                field.erase_value::<S>(
                    self.data_base
                        .add(field.offset(self.init_token))
                        .cast()
                        .as_ptr(),
                ),
            )
        }
    }

    pub fn get_erased_ptr(&self, i: usize) -> NonNull<S::EraseTo> {
        unsafe { self.get_erased_ptr_unchecked(S::descriptor().fields(self.init_token)[i]) }
    }

    pub fn get_erased(&self, i: usize) -> &S::EraseTo {
        unsafe { self.get_erased_ptr(i).as_ref() }
    }

    pub fn get_erased_mut(&mut self, i: usize) -> &mut S::EraseTo {
        unsafe { self.get_erased_ptr(i).as_mut() }
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

        for (field, cell) in S::descriptor()
            .fields(self.inner.init_token)
            .iter()
            .zip(&self.inner.cells)
        {
            let _guard = match cell.try_borrow() {
                Ok(v) => v,
                Err(err) => {
                    f.field(field.key_type_name(), &err);
                    continue;
                }
            };

            let instance = unsafe { self.inner.get_erased_ptr_unchecked(field).as_ref() };

            f.field(field.key_type_name(), &instance);
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

    pub fn get_ptr<F: LateField<S>>(&self) -> NonNull<F::Value> {
        self.inner.get_ptr::<F>()
    }

    pub fn get_mut<F: LateField<S>>(&mut self) -> &mut F::Value {
        self.inner.get_mut::<F>()
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
