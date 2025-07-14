use std::{
    alloc::{Layout, alloc, dealloc, handle_alloc_error},
    fmt,
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ptr::NonNull,
};

use scopeguard::ScopeGuard;

#[repr(C)]
pub struct Butterfly<T, H> {
    _ty: PhantomData<NonNull<H>>,
    data: NonNull<T>,
    len: usize,
}

impl<T, H> fmt::Debug for Butterfly<T, H>
where
    T: fmt::Debug,
    H: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Butterfly")
            .field("header", self.header())
            .field("data", &self.data())
            .finish()
    }
}

impl<T, H> Clone for Butterfly<T, H>
where
    T: Clone,
    H: Clone,
{
    fn clone(&self) -> Self {
        Self::new(self.len(), self.header().clone(), |idx| {
            self.data()[idx].clone()
        })
    }
}

unsafe impl<T, H> Send for Butterfly<T, H>
where
    T: Send,
    H: Send,
{
}

unsafe impl<T, H> Sync for Butterfly<T, H>
where
    T: Sync,
    H: Sync,
{
}

impl<T, H> Butterfly<T, H> {
    pub fn new(len: usize, header: H, init_elem: impl FnMut(usize) -> T) -> Self {
        Butterfly::new_uninit(len)
            .initialize_header(header)
            .init_data_closure(init_elem)
    }

    pub const unsafe fn from_raw(data: NonNull<T>, len: usize) -> Self {
        Self {
            _ty: PhantomData,
            data,
            len,
        }
    }

    pub const fn into_raw(self) -> (NonNull<T>, usize) {
        let raw = (self.data_ptr(), self.len());
        mem::forget(self);
        raw
    }

    pub const fn header_ptr(&self) -> NonNull<H> {
        unsafe { self.data.cast::<H>().sub(1) }
    }

    pub const fn data_ptr(&self) -> NonNull<T> {
        self.data
    }

    pub const fn data_slice_ptr(&self) -> NonNull<[T]> {
        NonNull::slice_from_raw_parts(self.data, self.len)
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub const fn pair(&self) -> (&H, &[T]) {
        unsafe { (self.header_ptr().as_ref(), self.data_slice_ptr().as_ref()) }
    }

    pub const fn pair_mut(&mut self) -> (&mut H, &mut [T]) {
        unsafe { (self.header_ptr().as_mut(), self.data_slice_ptr().as_mut()) }
    }

    pub const fn header(&self) -> &H {
        unsafe { self.header_ptr().as_ref() }
    }

    pub const fn header_mut(&mut self) -> &mut H {
        unsafe { self.header_ptr().as_mut() }
    }

    pub const fn data(&self) -> &[T] {
        unsafe { self.data_slice_ptr().as_ref() }
    }

    pub const fn data_mut(&mut self) -> &mut [T] {
        unsafe { self.data_slice_ptr().as_mut() }
    }

    pub fn truncate(&mut self, new_len: usize) {
        assert!(new_len <= self.len);

        if self.len == new_len {
            return;
        }

        // Set length before calling into userland.
        let old_len = mem::replace(&mut self.len, new_len);

        // Drop old values
        let drop_base = unsafe { self.data_ptr().add(new_len) };
        let drop_len = old_len - new_len;

        unsafe { NonNull::slice_from_raw_parts(drop_base, drop_len).drop_in_place() };
    }

    // TODO
}

impl<T, H> Butterfly<T, H> {
    const ALLOC_ALIGN_AND_OFFSET: (usize, usize) = {
        let align;
        let offset;

        if mem::align_of::<H>() < mem::align_of::<T>() {
            align = mem::align_of::<H>();
            offset = mem::size_of::<T>();
        } else {
            align = mem::align_of::<T>();
            offset = (mem::size_of::<H>() + (align - 1)) & !(align - 1);
        }

        (align, offset)
    };

    const ALIGN: usize = Self::ALLOC_ALIGN_AND_OFFSET.0;
    const OFFSET: usize = Self::ALLOC_ALIGN_AND_OFFSET.1;

    const fn layout(len: usize) -> Option<Layout> {
        let Some(len) = len.checked_mul(mem::size_of::<T>()) else {
            return None;
        };

        let Some(len) = len.checked_add(Self::OFFSET) else {
            return None;
        };

        Some(unsafe { Layout::from_size_align_unchecked(len, Self::ALIGN) })
    }
}

impl<T, H> Butterfly<MaybeUninit<T>, MaybeUninit<H>> {
    pub fn new_uninit(len: usize) -> Self {
        let layout = Self::layout(len).expect("allocation is too large");

        if layout.size() == 0 {
            return Self {
                _ty: PhantomData,
                data: unsafe { NonNull::new_unchecked(layout.align() as *mut _) },
                len,
            };
        }

        let Some(data) = NonNull::new(unsafe { alloc(layout) }) else {
            handle_alloc_error(layout);
        };

        Self {
            _ty: PhantomData,
            data: data.cast(),
            len,
        }
    }

    pub const unsafe fn assume_init(self) -> Butterfly<T, H> {
        unsafe { self.assume_data_init().assume_header_init() }
    }
}

impl<T, H> Butterfly<MaybeUninit<T>, H> {
    pub const unsafe fn assume_data_init(self) -> Butterfly<T, H> {
        let (base, len) = self.into_raw();

        unsafe { Butterfly::from_raw(base.cast(), len) }
    }

    pub fn init_data_closure(self, mut f: impl FnMut(usize) -> T) -> Butterfly<T, H> {
        let mut len_guard = scopeguard::guard((self, 0usize), |(this, len)| {
            let to_drop = NonNull::<[T]>::slice_from_raw_parts(this.data_ptr().cast(), len);

            unsafe { to_drop.drop_in_place() };

            drop(this);
        });

        let (data, len) = &mut *len_guard;

        for slot in data.data_mut() {
            slot.write(f(*len));
            *len += 1;
        }

        let (this, _len) = ScopeGuard::into_inner(len_guard);

        unsafe { this.assume_data_init() }
    }
}

impl<T, H> Butterfly<T, MaybeUninit<H>> {
    pub const unsafe fn assume_header_init(self) -> Butterfly<T, H> {
        let (base, len) = self.into_raw();

        unsafe { Butterfly::from_raw(base, len) }
    }

    pub fn initialize_header(mut self, header: H) -> Butterfly<T, H> {
        self.header_mut().write(header);

        unsafe { self.assume_header_init() }
    }
}

impl<T, H> Drop for Butterfly<T, H> {
    fn drop(&mut self) {
        unsafe { self.data_slice_ptr().drop_in_place() };

        let base_ptr = unsafe { self.data.sub(Self::OFFSET) };
        let layout = unsafe { Self::layout(self.len).unwrap_unchecked() };

        unsafe { dealloc(base_ptr.cast::<u8>().as_ptr(), layout) };
    }
}
