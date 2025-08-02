use std::{
    fmt,
    ptr::NonNull,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering::*},
        mpsc,
    },
};

use bumpalo::Bump;
use derive_where::derive_where;

// === KeepAliveList === //

pub struct KeepAliveList<T: Copy> {
    inner: Arc<KeepAliveListInner<T>>,
}

struct KeepAliveListInner<T: Copy> {
    /// Allocates `KeepAliveSlot`s which live for the duration of the `KeepAliveListInner`.
    ///
    /// We take care to ensure that no destructors need to be called for elements of this allocator
    /// by the time it is dropped. All fields in the structure either have trivial destructors or
    /// have a `Copy` bound.
    ///
    /// To avoid misuse of the `!Sync` nature of `Bump`, this value must only be accessed by
    /// `KeepAliveList` while it's being borrowed mutably.
    bump: Bump,

    /// A sender for potentially condemned slots owned by us.
    sender: mpsc::Sender<KeepAlivePtr<T>>,

    /// A receiver for potentially condemned slots owned by us.
    ///
    /// To avoid misuse of the `!Sync` nature of `mpsc::Receiver`, this value must only be accessed
    /// by `KeepAliveList` while it's being borrowed mutably.
    receiver: mpsc::Receiver<KeepAlivePtr<T>>,
}

struct KeepAliveSlot<T: Copy> {
    /// The raw pointer derived from `KeepAliveList::inner`'s `Arc`.
    ///
    /// This counts towards the reference count when one or more `KeepAliveStrong`s point to the
    /// slot. In fact, it may count towards the reference count twice if `KeepAliveStrong::drop`
    /// and `KeepAliveList::upgrade` race. Otherwise, the pointer potentially dangles.
    owner: *const KeepAliveListInner<T>,

    /// The number of `KeepAliveStrong`s pointing to this slot. If a slot has been confirmed dead by
    /// a call to `take_condemned` and awaiting resurrection via `upgrade`, this value will be
    /// `usize::MAX`.
    strong_refs: AtomicUsize,

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
        let (sender, receiver) = mpsc::channel();

        Self {
            inner: Arc::new(KeepAliveListInner {
                bump: Bump::new(),
                sender,
                receiver,
            }),
        }
    }
}

impl<T: Copy> KeepAliveList<T> {
    /// Create a new dead slot with the supplied `userdata`.
    pub fn spawn(&mut self, userdata: T) -> KeepAliveStrong<T> {
        let cell = self.inner.bump.alloc(KeepAliveSlot {
            owner: Arc::into_raw(self.inner.clone()),
            strong_refs: AtomicUsize::new(1),
            userdata,
        });

        KeepAliveStrong {
            ptr: KeepAlivePtr(NonNull::from(cell)),
        }
    }

    /// Upgrade a [`KeepAlivePtr`] into a [`KeepAliveStrong`], potentially resurrecting the slot
    /// from the dead.
    ///
    /// ## Safety
    ///
    /// `ptr` must be a slot owned by this [`KeepAliveList`].
    ///
    pub unsafe fn upgrade(&self, ptr: KeepAlivePtr<T>) -> KeepAliveStrong<T> {
        // Safety: the caller assures us that we own this slot and our invariants tell us that
        // all slots we allocated will be alive as long as our `KeepAliveListInner` is alive.
        let slot = unsafe { ptr.0.as_ref() };

        if
        // If we resurrected a confirmed condemned slot...
        slot
            .strong_refs
            .compare_exchange(usize::MAX, 1, Relaxed, Relaxed)
            .is_ok()
            // ...or if we resurrected a slot which has not yet been confirmed dead...
            || slot.strong_refs.fetch_add(1, Relaxed) == 0
        {
            // ...bring back the strong reference to the list.
            _ = Arc::into_raw(self.inner.clone());
        }

        KeepAliveStrong { ptr }
    }

    /// Fetches the next slot to have been condemned or `None` if all condemned slots have been
    /// observed.
    pub fn take_condemned(&mut self) -> Option<(KeepAlivePtr<T>, T)> {
        loop {
            let next = self.inner.receiver.try_recv().ok()?;

            // Safety: the invariants assure us that we own this slot and that all slots we
            // allocated will be alive as long as our `KeepAliveListInner` is alive.
            let next_value = unsafe { next.0.as_ref() };

            if next_value.strong_refs.load(Relaxed) > 0 {
                // This cannot miss events because we always send a new event to the MPSC when the
                // reference count becomes zero.
                continue;
            }

            // Set the strong reference count to `usize::MAX` to ensure that, if the slot were in
            // the MPSC more than once, it would be skipped over after this. This operation is safe
            // because `strong_refs` is zero *iff* no remaining strong references could be cloned
            // and our mutable borrow of `self` prevents concurrent calls to `upgrade`.
            next_value.strong_refs.store(usize::MAX, Relaxed);

            return Some((next, next_value.userdata));
        }
    }
}

// === KeepAliveStrong === //

#[derive_where(Copy, Clone, Hash, Eq, PartialEq)]
pub struct KeepAlivePtr<T: Copy>(NonNull<KeepAliveSlot<T>>);

unsafe impl<T: Copy + Send> Send for KeepAlivePtr<T> {}
unsafe impl<T: Copy + Send> Sync for KeepAlivePtr<T> {}

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
    ///
    pub unsafe fn userdata(&self) -> T {
        // Safety: provided by caller
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
        // Safety: by the invariants of `KeepAliveSlot::owner`, we know that it will be keeping the
        // `owner` alive so long at least one strong reference is pointing to the slot. Since these
        // slots are owned by that owner, we know they'll stay alive.
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
        unsafe { self.ptr.0.as_ref() }
            .strong_refs
            .fetch_add(1, Relaxed);

        Self { ptr: self.ptr }
    }
}

impl<T: Copy> Drop for KeepAliveStrong<T> {
    fn drop(&mut self) {
        let slot = self.slot();

        // Decrement reference count.
        if slot.strong_refs.fetch_sub(1, Relaxed) > 1 {
            // (not a unique reference)
            return;
        }

        // Tell the MPSC about our death.
        unsafe { &*slot.owner }.sender.send(self.ptr).unwrap();

        // Decrement the reference count for the owner structure.
        drop(unsafe { Arc::from_raw(slot.owner) });
    }
}
