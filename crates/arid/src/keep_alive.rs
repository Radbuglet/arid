use std::{
    fmt,
    num::NonZeroU32,
    sync::{Arc, Weak, mpsc},
};

// === KeepAliveManager === //

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
        Self::new()
    }
}

impl<T> KeepAliveManager<T> {
    pub fn new() -> Self {
        let (candidate_sender, candidate_receiver) = mpsc::channel();

        Self {
            slots: Vec::new(),
            free_slots: Vec::new(),
            candidate_sender: Arc::new(candidate_sender),
            candidate_receiver,
        }
    }

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

    pub fn userdata(&self, index: KeepAliveIndex) -> &T {
        self.slots[index.as_usize()].userdata.as_ref().unwrap()
    }

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

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct KeepAliveIndex(NonZeroU32);

impl KeepAliveIndex {
    pub const MAX: Self = Self::from_usize((u32::MAX - 1) as usize).unwrap();

    pub const fn as_usize(self) -> usize {
        (self.0.get() - 1) as usize
    }

    pub const fn from_usize(idx: usize) -> Option<Self> {
        let idx = idx.saturating_add(1) as u64;

        if idx > u32::MAX as u64 {
            return None;
        }

        Some(Self(NonZeroU32::new(idx as u32).unwrap()))
    }
}

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
    pub fn index(&self) -> KeepAliveIndex {
        self.0.index
    }
}
