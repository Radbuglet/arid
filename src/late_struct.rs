use std::{
    alloc::{self, Layout},
    any::{TypeId, type_name},
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt,
    marker::PhantomData,
    ptr::{self, NonNull},
    sync::{
        Once,
        atomic::{AtomicPtr, AtomicUsize, Ordering::Relaxed},
    },
};

use scopeguard::ScopeGuard;

// === Definitions === //

pub unsafe trait LateStruct: 'static {
    fn state() -> &'static LateStructState;
}

pub unsafe trait LateField<S: LateStruct>: 'static {
    type Value: 'static + Default + fmt::Debug;

    fn state() -> &'static LateFieldState;
}

#[derive(Debug)]
pub struct LateStructState {
    size: AtomicUsize,
    align: AtomicUsize,
    fields: AtomicPtr<&'static [&'static LateFieldState]>,
}

impl Default for LateStructState {
    fn default() -> Self {
        Self::new()
    }
}

impl LateStructState {
    pub const fn new() -> Self {
        Self {
            size: AtomicUsize::new(0),
            align: AtomicUsize::new(0),
            fields: AtomicPtr::new(ptr::null_mut()),
        }
    }

    pub fn layout(&self, token: LateLayoutInitToken) -> Layout {
        let _ = token;

        unsafe {
            Layout::from_size_align_unchecked(self.size.load(Relaxed), self.align.load(Relaxed))
        }
    }

    pub fn fields(&self, token: LateLayoutInitToken) -> &'static [&'static LateFieldState] {
        let _ = token;

        unsafe { &**self.fields.load(Relaxed) }
    }
}

#[derive(Debug)]
pub struct LateFieldState {
    index: AtomicUsize,
    offset: AtomicUsize,
    layout: Layout,
    init: unsafe fn(*mut u8),
    drop: unsafe fn(*mut u8),
    as_debug: fn(*const u8) -> *const dyn fmt::Debug,
    type_name: fn() -> &'static str,
    type_id: fn() -> TypeId,
}

impl LateFieldState {
    pub const fn new<V>() -> Self
    where
        V: 'static + fmt::Debug + Default,
    {
        Self {
            index: AtomicUsize::new(usize::MAX),
            offset: AtomicUsize::new(usize::MAX),
            layout: Layout::new::<V>(),
            init: |ptr| unsafe { ptr.cast::<V>().write(V::default()) },
            drop: |ptr| unsafe { ptr.cast::<V>().drop_in_place() },
            as_debug: |ptr| ptr.cast::<V>(),
            type_name: type_name::<V>,
            type_id: TypeId::of::<V>,
        }
    }

    pub fn index(&self) -> usize {
        self.index.load(Relaxed)
    }

    pub fn offset(&self) -> usize {
        self.offset.load(Relaxed)
    }

    pub fn layout(&self) -> Layout {
        self.layout
    }

    pub fn init(&self) -> unsafe fn(*mut u8) {
        self.init
    }

    pub fn drop(&self) -> unsafe fn(*mut u8) {
        self.drop
    }

    pub fn as_debug(&self) -> fn(*const u8) -> *const dyn fmt::Debug {
        self.as_debug
    }

    pub fn type_name(&self) -> fn() -> &'static str {
        self.type_name
    }

    pub fn type_id(&self) -> fn() -> TypeId {
        self.type_id
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

            let mut structs =
                HashMap::<TypeId, (&'static LateStructState, Vec<&'static LateFieldState>)>::new();

            for entry in LATE_STRUCTS {
                let entry = entry();

                structs.insert(entry.struct_type, (entry.state, Vec::new()));
            }

            for entry in LATE_FIELDS {
                let entry = entry();

                structs
                    .get_mut(&entry.struct_type)
                    .unwrap()
                    .1
                    .push(entry.state);
            }

            for (struct_state, struct_fields) in structs.into_values() {
                let struct_fields: &[_] = &*Box::leak(Box::from_iter(struct_fields));
                let struct_fields_p = Box::leak(Box::new(struct_fields));

                let mut overall_layout = Layout::new::<()>();

                for (i, field) in struct_fields.iter().enumerate() {
                    let (new_layout, offset) = overall_layout.extend(field.layout).unwrap();
                    field.index.store(i, Relaxed);
                    field.offset.store(offset, Relaxed);
                    overall_layout = new_layout;
                }

                struct_state.size.store(overall_layout.size(), Relaxed);
                struct_state.align.store(overall_layout.align(), Relaxed);
                struct_state.fields.store(struct_fields_p, Relaxed);
            }
        });

        Self
    }
}

// === Macros === //

#[doc(hidden)]
pub mod late_macro_internals {
    use std::any::TypeId;

    pub use {
        super::{LateField, LateFieldState, LateStruct, LateStructState},
        linkme,
    };

    #[derive(Debug, Copy, Clone)]
    pub struct LateStructEntry {
        pub struct_type: TypeId,
        pub state: &'static LateStructState,
    }

    impl LateStructEntry {
        pub fn of<S: LateStruct>() -> Self {
            Self {
                struct_type: TypeId::of::<S>(),
                state: S::state(),
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    pub struct LateFieldEntry {
        pub struct_type: TypeId,
        pub state: &'static LateFieldState,
    }

    impl LateFieldEntry {
        pub fn of<S: LateStruct, F: LateField<S>>() -> Self {
            Self {
                struct_type: TypeId::of::<S>(),
                state: F::state(),
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
    ($($ty:ty),*$(,)?) => {$(
        const _: () = {
            static STATE: $crate::late_struct::late_macro_internals::LateStructState =
                $crate::late_struct::late_macro_internals::LateStructState::new();

            #[$crate::late_struct::late_macro_internals::linkme::distributed_slice(
                $crate::late_struct::late_macro_internals::LATE_STRUCTS
            )]
            #[linkme(crate = $crate::late_struct::late_macro_internals::linkme)]
            static ENTRY: fn() -> $crate::late_struct::late_macro_internals::LateStructEntry =
                $crate::late_struct::late_macro_internals::LateStructEntry::of::<$ty>;

            unsafe impl $crate::late_struct::late_macro_internals::LateStruct for $ty {
                fn state() -> &'static $crate::late_struct::late_macro_internals::LateStructState {
                    &STATE
                }
            }
        };
    )*};
}

pub use late_struct;

#[macro_export]
macro_rules! late_field {
    ($($ty:ty [$ns:ty] => $val:ty),*$(,)?) => {$(
        const _: () = {
            static STATE: $crate::late_struct::late_macro_internals::LateFieldState =
                $crate::late_struct::late_macro_internals::LateFieldState::new::<$val>();

            #[$crate::late_struct::late_macro_internals::linkme::distributed_slice(
                $crate::late_struct::late_macro_internals::LATE_FIELDS
            )]
            #[linkme(crate = $crate::late_struct::late_macro_internals::linkme)]
            static ENTRY: fn() -> $crate::late_struct::late_macro_internals::LateFieldEntry =
                $crate::late_struct::late_macro_internals::LateFieldEntry::of::<$ns, $ty>;

            unsafe impl $crate::late_struct::late_macro_internals::LateField<$ns> for $ty {
                type Value = $val;

                fn state() -> &'static $crate::late_struct::late_macro_internals::LateFieldState {
                    &STATE
                }
            }
        };
    )*};
}

pub use late_field;

// === Instances === //

pub struct LateInstance<S: LateStruct> {
    _ty: PhantomData<fn(S) -> S>,
    init_token: LateLayoutInitToken,
    data_base: NonNull<u8>,
    cells: Box<[RefCell<()>]>,
}

impl<S: LateStruct> fmt::Debug for LateInstance<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("LateInstance");

        for field in S::state().fields(self.init_token) {
            let instance =
                unsafe { &*field.as_debug()(self.data_base.add(field.offset()).as_ptr()) };

            f.field(field.type_name()(), instance);
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

        let struct_layout = S::state().layout(init_token);
        let struct_fields = S::state().fields(init_token);

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
                unsafe { field.drop()(data_base.add(field.offset()).as_ptr()) };
            }
        });

        for &field in struct_fields {
            unsafe { field.init()(data_base.add(field.offset()).as_ptr()) };
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

    pub fn get_ptr<F: LateField<S>>(&self) -> NonNull<F::Value> {
        unsafe { self.data_base.add(F::state().offset()).cast() }
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
        let struct_layout = S::state().layout(self.init_token);
        let struct_fields = S::state().fields(self.init_token);

        for field in struct_fields {
            unsafe { field.drop()(self.data_base.add(field.offset()).as_ptr()) }
        }

        unsafe { alloc::dealloc(self.data_base.as_ptr(), struct_layout) };
    }
}

#[repr(transparent)]
pub struct LateInstanceDyn<S: LateStruct>(LateInstance<S>);

impl<S: LateStruct> fmt::Debug for LateInstanceDyn<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("LateInstanceDynamic");

        for (field, cell) in S::state()
            .fields(self.0.init_token)
            .iter()
            .zip(&self.0.cells)
        {
            let _guard = match cell.try_borrow() {
                Ok(v) => v,
                Err(err) => {
                    f.field(field.type_name()(), &err);
                    continue;
                }
            };

            let instance =
                unsafe { &*field.as_debug()(self.0.data_base.add(field.offset()).as_ptr()) };

            f.field(field.type_name()(), instance);
        }

        f.finish()
    }
}

impl<S: LateStruct> LateInstanceDyn<S> {
    pub fn non_dynamic(&mut self) -> &mut LateInstance<S> {
        &mut self.0
    }

    pub fn get_ptr<F: LateField<S>>(&self) -> NonNull<F::Value> {
        self.0.get_ptr::<F>()
    }

    pub fn get_mut<F: LateField<S>>(&mut self) -> &mut F::Value {
        self.0.get_mut::<F>()
    }

    pub fn borrow<F: LateField<S>>(&self) -> Ref<'_, F::Value> {
        Ref::map(self.0.cells[F::state().index()].borrow(), |()| unsafe {
            self.get_ptr::<F>().as_ref()
        })
    }

    pub fn borrow_mut<F: LateField<S>>(&self) -> RefMut<'_, F::Value> {
        RefMut::map(self.0.cells[F::state().index()].borrow_mut(), |()| unsafe {
            self.get_ptr::<F>().as_mut()
        })
    }
}
