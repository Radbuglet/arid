use std::{
    fmt,
    ptr::{self, NonNull},
    sync::{
        Arc,
        atomic::{AtomicPtr, AtomicUsize, Ordering::*},
    },
};

use bumpalo::Bump;
use derive_where::derive_where;

// === KeepAliveList === //

pub struct KeepAliveList<T: Copy> {
    inner: Arc<KeepAliveListInner<T>>,
}

struct KeepAliveListInner<T: Copy> {
    /// Allocates [`KeepAliveSlot`]s which live for the duration of the [`KeepAliveListInner`].
    ///
    /// We take care to ensure that no destructors need to be called for elements of this allocator
    /// by the time it is dropped. All fields in the structure either have trivial destructors or
    /// have a `Copy` bound.
    ///
    /// To avoid misuse of the `!Sync` nature of `Bump`, this value must only be accessed by
    /// [`KeepAliveList`] while it's being borrowed mutably.
    bump: Bump,

    /// The first [`KeepAliveSlot`] in the list of potentially destroyed slots. These slots are only
    /// actually considered destroyed if the reference count at the time of iteration is properly
    /// zero. This is because users can resurrect dead slots using the [`KeepAliveList::upgrade`]
    /// method.
    ///
    /// This pointee is either `0x1` or a valid pointee guaranteed to be live for the duration of
    /// this [`KeepAliveListInner`] instance.
    head: AtomicPtr<KeepAliveSlot<T>>,
}

struct KeepAliveSlot<T: Copy> {
    /// The raw pointer derived from [`KeepAliveList::inner`]'s `Arc`.
    ///
    /// This counts towards the reference count while one or more `KeepAliveStrong` points to the
    /// slot. In fact, it may count towards the reference count twice if [`KeepAliveStrong::drop`]
    /// and [`KeepAliveList::upgrade`] race. Otherwise, the pointer potentially dangles.
    owner: *const KeepAliveListInner<T>,

    /// The number of [`KeepAliveStrong`]s pointing to this slot.
    refs: AtomicUsize,

    /// The next potentially destroyed slot in the linked list. See [`KeepAliveListInner::head`] for
    /// details on how to interpret this list.
    ///
    /// This pointer is `null` if the slot is not in the linked list, `0x1` if it's the last slot
    /// in the list, and a pointer to a [`KeepAliveSlot`] living
    next: AtomicPtr<KeepAliveSlot<T>>,

    /// User-defined information about the slot.
    userdata: T,
}

unsafe impl<T: Copy + Send> Send for KeepAliveList<T> {}

unsafe impl<T: Copy + Send> Sync for KeepAliveList<T> {}

impl<T: Copy> fmt::Debug for KeepAliveList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("KeepAliveList").finish_non_exhaustive()
    }
}

impl<T: Copy> Default for KeepAliveList<T> {
    fn default() -> Self {
        Self {
            inner: Arc::new(KeepAliveListInner {
                bump: Bump::new(),
                head: AtomicPtr::new(0x1 as *mut _),
            }),
        }
    }
}

impl<T: Copy> KeepAliveList<T> {
    /// Create a new slot with the supplied `userdata` and return a unique [`KeepAliveStrong`]
    /// reference to it.
    pub fn spawn(&mut self, userdata: T) -> KeepAliveStrong<T> {
        let cell = self.inner.bump.alloc(KeepAliveSlot {
            owner: Arc::into_raw(self.inner.clone()),
            refs: AtomicUsize::new(1),
            next: AtomicPtr::new(ptr::null_mut()),
            userdata,
        });

        let cell = NonNull::from(cell);

        KeepAliveStrong {
            ptr: KeepAlivePtr(cell),
        }
    }

    /// Upgrade a [`KeepAlivePtr`] into a [`KeepAliveStrong`], potentially resurrecting the slot
    /// from the dead.
    ///
    /// ## Safety
    ///
    /// `ptr` must be a slot owned by this [`KeepAliveList`].
    pub unsafe fn upgrade(&self, ptr: KeepAlivePtr<T>) -> KeepAliveStrong<T> {
        let slot = unsafe { ptr.0.as_ref() };

        if slot.refs.fetch_add(1, Relaxed) == 0 {
            _ = Arc::into_raw(self.inner.clone());
        }

        KeepAliveStrong { ptr }
    }

    /// Fetches the next slot to have been condemned or `None` if all condemned slots have been
    /// observed.
    pub fn take_condemned(&mut self) -> Option<(KeepAlivePtr<T>, T)> {
        loop {
            // Advance the head to the next element.
            let res = self.inner.head.fetch_update(Release, Acquire, |head| {
                debug_assert!(!head.is_null());

                if head as usize == 1 {
                    // This is the terminator of the list.
                    return None;
                }

                let slot = unsafe { &*head };

                let next = slot.next.load(Relaxed);
                slot.next.store(ptr::null_mut(), Relaxed);

                Some(next)
            });

            match res {
                Ok(prev_head) => {
                    let slot = KeepAlivePtr(unsafe { NonNull::new_unchecked(prev_head) });

                    if unsafe { slot.0.as_ref() }.refs.load(Relaxed) > 0 {
                        // This slot was a false positive.
                        continue;
                    }

                    break Some((slot, unsafe { slot.userdata() }));
                }
                Err(_) => {
                    break None;
                }
            }
        }
    }
}

// === KeepAliveStrong === //

#[derive_where(Copy, Clone, Hash, Eq, PartialEq)]
pub struct KeepAlivePtr<T: Copy>(NonNull<KeepAliveSlot<T>>);

unsafe impl<T: Copy + Send> Send for KeepAlivePtr<T> {}

impl<T: Copy> fmt::Debug for KeepAlivePtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: Copy> KeepAlivePtr<T> {
    /// Fetches the slot's userdata.
    ///
    /// ## Safety
    ///
    /// This pointee must be kept alive by either a [`KeepAliveStrong`] to the same slot or an
    /// owning [`KeepAliveList`].
    pub unsafe fn userdata(&self) -> T {
        unsafe { self.0.as_ref() }.userdata
    }
}

#[derive_where(Hash, Eq, PartialEq)]
pub struct KeepAliveStrong<T: Copy> {
    ptr: KeepAlivePtr<T>,
}

impl<T: Copy> fmt::Debug for KeepAliveStrong<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ptr.0.fmt(f)
    }
}

impl<T: Copy> KeepAliveStrong<T> {
    fn slot(&self) -> &KeepAliveSlot<T> {
        unsafe { self.ptr.0.as_ref() }
    }

    pub fn userdata(&self) -> T {
        self.slot().userdata
    }

    pub fn ptr(&self) -> KeepAlivePtr<T> {
        self.ptr
    }
}

impl<T: Copy> Clone for KeepAliveStrong<T> {
    fn clone(&self) -> Self {
        // Increment reference count.
        unsafe { self.ptr.0.as_ref() }.refs.fetch_add(1, Relaxed);

        Self { ptr: self.ptr }
    }
}

impl<T: Copy> Drop for KeepAliveStrong<T> {
    fn drop(&mut self) {
        let slot_ptr = self.ptr.0.as_ptr();
        let slot = self.slot();

        // Decrement reference count.
        if slot.refs.fetch_sub(1, Relaxed) > 1 {
            // (not a unique reference)
            return;
        }

        // Push the slot to the linked list of potentially destroyed slots.
        {
            let head = unsafe { &(*slot.owner).head };

            _ = head.fetch_update(Release, Acquire, |head| {
                if !slot.next.load(Relaxed).is_null() {
                    // (already in list)
                    return None;
                }

                // Update the head.
                slot.next.store(head, Relaxed);

                Some(slot_ptr)
            });
        }

        // Decrement the reference count for the owner structure.
        drop(unsafe { Arc::from_raw(slot.owner) });
    }
}
