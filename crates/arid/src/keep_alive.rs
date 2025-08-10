use std::{
    fmt,
    num::NonZeroU32,
    sync::{Arc, Weak, mpsc},
};

// === KeepAliveManager === //

/// Manages a set of [`KeepAlive`]s, notifying when all `KeepAlive`s to a value are dropped.
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
///
/// A [`KeepAlive`] is a cloneable reference-counted guard indicating that a given value should not
/// yet be destroyed. You can create a new `KeepAlive` using [`KeepAliveManager::allocate`]
/// alongside some userdata of type `T` providing information about what the `KeepAlive` is keeping
/// alive. You can then scan for values which no longer have `KeepAlive` guards keeping them alive
/// using [`KeepAliveManager::take_condemned`]. This method identifies the value which has been
/// destroyed by passing ownership of the slot's userdata back to you.
///
/// Each `KeepAlive` has an associated [`KeepAliveIndex`] within its manager which can be obtained
/// using [`KeepAlive::index`]. Unlike [`RawHandle`](crate::RawHandle)s, `KeepAliveIndex`es may be
/// reused after they have been condemned.
///
/// Mirroring the [world lifecycle semantics](index.html#lifecycle) described in the crate-level
/// documentation, `KeepAlive`s are not actually marked as dead until `take_condemned` observes them
/// as being dead. During the time between the last `KeepAlive` to a given value being dropped and
/// the condemnation being observed by `take_condemned`, you can "resurrect" a given `KeepAlive`
/// using [`KeepAliveManager::upgrade`] and prevent it from being dropped.
pub struct KeepAliveManager<T> {
    slots: Vec<KeepAliveManagerSlot<T>>,
    free_slots: Vec<KeepAliveIndex>,
    candidate_sender: Arc<mpsc::Sender<KeepAliveIndex>>,
    candidate_receiver: mpsc::Receiver<KeepAliveIndex>,
}

struct KeepAliveManagerSlot<T> {
    pointee: Weak<KeepAlivePointee>,
    userdata: Option<T>,
}

impl<T> fmt::Debug for KeepAliveManager<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("KeepAliveManager").finish_non_exhaustive()
    }
}

impl<T> Default for KeepAliveManager<T> {
    fn default() -> Self {
        let (candidate_sender, candidate_receiver) = mpsc::channel();

        Self {
            slots: Vec::new(),
            free_slots: Vec::new(),
            candidate_sender: Arc::new(candidate_sender),
            candidate_receiver,
        }
    }
}

impl<T> KeepAliveManager<T> {
    /// Creates a new [`KeepAlive`] and associates it with the supplied `userdata`.
    pub fn allocate(&mut self, userdata: T) -> KeepAlive {
        if let Some(index) = self.free_slots.pop() {
            let pointee = Arc::new(KeepAlivePointee {
                candidate_sender: Arc::downgrade(&self.candidate_sender),
                index,
            });

            let slot = &mut self.slots[index.as_usize()];

            slot.pointee = Arc::downgrade(&pointee);
            slot.userdata = Some(userdata);

            return KeepAlive(pointee);
        }

        let index = KeepAliveIndex::from_usize(self.slots.len()).expect("allocated too many slots");
        let pointee = Arc::new(KeepAlivePointee {
            candidate_sender: Arc::downgrade(&self.candidate_sender),
            index,
        });

        self.slots.push(KeepAliveManagerSlot {
            pointee: Arc::downgrade(&pointee),
            userdata: Some(userdata),
        });

        KeepAlive(pointee)
    }

    /// Upgrades a [`KeepAliveIndex`] into a [`KeepAlive`].
    ///
    /// The `KeepAlive` being pointed to by the specified `KeepAliveIndex` must not yet have been
    /// observed as condemned by [`KeepAliveManager::take_condemned`] or the behavior will be
    /// unspecified.
    pub fn upgrade(&mut self, index: KeepAliveIndex) -> KeepAlive {
        let slot = &mut self.slots[index.as_usize()];
        debug_assert!(slot.userdata.is_some());

        if let Some(pointee) = slot.pointee.upgrade() {
            return KeepAlive(pointee);
        }

        let pointee = Arc::new(KeepAlivePointee {
            candidate_sender: Arc::downgrade(&self.candidate_sender),
            index,
        });
        slot.pointee = Arc::downgrade(&pointee);

        KeepAlive(pointee)
    }

    /// Fetches the userdata associated with a given [`KeepAliveIndex`].
    ///
    /// The `KeepAlive` being pointed to by the specified `KeepAliveIndex` must not yet have been
    /// observed as condemned by [`KeepAliveManager::take_condemned`] or the behavior will be
    /// unspecified.
    pub fn userdata(&self, index: KeepAliveIndex) -> &T {
        self.slots[index.as_usize()].userdata.as_ref().unwrap()
    }

    /// Polls the `KeepAliveManager` for any values lacking a remaining [`KeepAlive`].
    ///
    /// If the function returns `Some`, the returned `KeepAliveIndex` indicates the index which will
    /// no longer be valid until it is randomly reused by [`KeepAliveManager::allocate`] and the
    /// returned value `T` matches the `userdata` associated with the `KeepAlive` during the call to
    /// [`KeepAliveManager::allocate`].
    ///
    /// If the function returns `None`, the `KeepAliveManager` currently has no remaining
    /// `KeepAlive` slots to drop.
    ///
    /// This method should be called repeatedly until it returns `None` to ensure that the queue of
    /// destroyed `KeepAlive`s is processed in a timely manner.
    pub fn take_condemned(&mut self) -> Option<(KeepAliveIndex, T)> {
        while let Ok(candidate) = self.candidate_receiver.try_recv() {
            let slot = &mut self.slots[candidate.as_usize()];

            if slot.pointee.strong_count() > 0 {
                continue;
            }

            let Some(userdata) = slot.userdata.take() else {
                // Slot marked as a candidate more than once.
                continue;
            };

            // Set to a dangling slot to allow the previous pointee's memory allocation to be
            // released.
            slot.pointee = Weak::default();

            self.free_slots.push(candidate);

            return Some((candidate, userdata));
        }

        None
    }
}

struct KeepAlivePointee {
    candidate_sender: Weak<mpsc::Sender<KeepAliveIndex>>,
    index: KeepAliveIndex,
}

impl Drop for KeepAlivePointee {
    fn drop(&mut self) {
        let Some(candidate_sender) = self.candidate_sender.upgrade() else {
            return;
        };

        _ = candidate_sender.send(self.index);
    }
}

// === KeepAlive === //

/// An index to a not-yet-condemned [`KeepAlive`] in a [`KeepAliveManager`].
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
///
/// You can obtain a `KeepAliveIndex` from a `KeepAlive` using the [`KeepAlive::index`] method.
///
/// Unlike [`RawHandle`](crate::RawHandle)s, `KeepAliveIndex`es may be reused after they have
/// been condemned.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct KeepAliveIndex(NonZeroU32);

impl KeepAliveIndex {
    /// The maximum possible `KeepAliveIndex` one could observe. This is not guaranteed to always
    /// dangle.
    pub const MAX: Self = Self::from_usize((u32::MAX - 1) as usize).unwrap();

    /// Converts the index into a `usize`.
    pub const fn as_usize(self) -> usize {
        (self.0.get() - 1) as usize
    }

    /// Attempts to convert a `usize` into a `KeepAliveIndex`, returning `None` if the index is too
    /// big.
    ///
    /// See also, [`KeepAliveIndex::MAX`].
    pub const fn from_usize(idx: usize) -> Option<Self> {
        let idx = idx.saturating_add(1) as u64;

        if idx > u32::MAX as u64 {
            return None;
        }

        Some(Self(NonZeroU32::new(idx as u32).unwrap()))
    }
}

/// A cloneable guard keeping some logical value alive.
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
///
/// All `KeepAlive`s are managed by at most one [`KeepAliveManager`] and can be obtained by calling
/// [`KeepAliveManager::allocate`].
///
/// Each `KeepAlive` has an associated [`KeepAliveIndex`] within its manager which can be obtained
/// using [`KeepAlive::index`]. Unlike [`RawHandle`](crate::RawHandle)s, `KeepAliveIndex`es may be
/// reused after they have been condemned.
#[derive(Clone)]
pub struct KeepAlive(Arc<KeepAlivePointee>);

impl fmt::Debug for KeepAlive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("KeepAlive").field(&self.0.index).finish()
    }
}

impl Eq for KeepAlive {}

impl PartialEq for KeepAlive {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl KeepAlive {
    /// Fetches the `KeepAlive`'s associated `KeepAliveIndex`, which can be used to identify the
    /// value in calls to [`KeepAliveManager::upgrade`] without having to keep the guard alive.
    ///
    /// Note that, unlike [`RawHandle`](crate::RawHandle)s, `KeepAliveIndex`es may be reused after
    /// they have been condemned.
    pub fn index(&self) -> KeepAliveIndex {
        self.0.index
    }
}
