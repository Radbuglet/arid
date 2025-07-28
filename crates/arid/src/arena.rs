use std::{
    fmt,
    mem::MaybeUninit,
    num::{NonZeroU32, NonZeroU64},
    sync::atomic::{AtomicU64, Ordering::*},
};

use derive_where::derive_where;
use late_struct::late_field;

use crate::{
    W, World,
    utils::keep_alive::{KeepAliveList, KeepAlivePtr, KeepAliveStrong},
    world_ns,
};

// === ArenaManager === //

// Used in `crate::handle`.
#[derive(Debug)]
#[non_exhaustive]
pub(crate) struct ArenaManagerWrapper(pub(crate) ArenaManager);

#[derive(Debug)]
pub struct ArenaManager {
    uid: NonZeroU64,
    listener: KeepAliveList<KeepAliveSlot>,
}

#[derive(Debug, Copy, Clone)]
struct KeepAliveSlot {
    slot_idx: u32,
    destructor: fn(slot_idx: u32, w: W),
}

late_field!(ArenaManagerWrapper[world_ns::WorldNs]);

impl Default for ArenaManagerWrapper {
    fn default() -> Self {
        static UID_GEN: AtomicU64 = AtomicU64::new(1);

        Self(ArenaManager {
            uid: NonZeroU64::new(UID_GEN.fetch_add(1, Relaxed)).unwrap(),
            listener: KeepAliveList::default(),
        })
    }
}

impl World {
    pub fn flush(&mut self) {
        while let Some((_ptr, condemned)) = self
            .inner
            .get_mut::<ArenaManagerWrapper>()
            .0
            .listener
            .take_condemned()
        {
            (condemned.destructor)(condemned.slot_idx, self);
        }
    }
}

// === Arena === //

#[derive_where(Default)]
pub struct Arena<T> {
    header: Box<ArenaHeader>,
    slots: Vec<Slot<T>>,
}

#[derive(Default)]
struct ArenaHeader {
    manager_uid: Option<NonZeroU64>,
    keep_alive_slots: Vec<KeepAlivePtr<KeepAliveSlot>>,
    free_slots: Vec<u32>,
}

impl<T> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Arena").finish_non_exhaustive()
    }
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(
        &mut self,
        manager: &mut ArenaManager,
        destructor: fn(slot_idx: u32, w: W),
        value: T,
    ) -> (RawHandle, KeepAlive) {
        // Ensure that we're always allocating keep-alive slots from the same manager.
        if let Some(manager_uid) = self.header.manager_uid {
            assert_eq!(manager.uid, manager_uid);
        } else {
            self.header.manager_uid = Some(manager.uid);
        }

        // Attempt to reuse an existing empty slot.
        if let Some(slot_idx) = self.header.free_slots.pop() {
            let slot = &mut self.slots[slot_idx as usize];

            slot.generation += 1;

            let keep_alive = self.header.keep_alive_slots[slot_idx as usize];
            let keep_alive = KeepAlive(unsafe { manager.listener.upgrade(keep_alive) });

            slot.value.write(value);

            let handle = RawHandle {
                slot_idx,
                generation: unsafe { NonZeroU32::new_unchecked(slot.generation) },
            };

            return (handle, keep_alive);
        }

        // Otherwise, create a new slot.
        let slot_idx = u32::try_from(self.slots.len()).expect("too many slots");

        self.slots.push(Slot {
            generation: 1,
            value: MaybeUninit::new(value),
        });

        let handle = RawHandle {
            slot_idx,
            generation: NonZeroU32::new(1).unwrap(),
        };

        let keep_alive = KeepAlive(manager.listener.spawn(KeepAliveSlot {
            slot_idx,
            destructor,
        }));

        self.header.keep_alive_slots.push(keep_alive.0.ptr());

        (handle, keep_alive)
    }

    pub fn slot_to_handle(&self, slot_idx: u32) -> Option<RawHandle> {
        let state = &self.slots[slot_idx as usize];

        (state.generation % 2 == 1).then(|| RawHandle {
            slot_idx,
            generation: NonZeroU32::new(state.generation).unwrap(),
        })
    }

    pub fn remove_now(&mut self, slot_idx: u32) -> Option<T> {
        let slot = &mut self.slots[slot_idx as usize];

        assert!(slot.generation % 2 == 1, "attempted to remove a dead slot");

        // Take slot contents and mark it as invalid.
        slot.generation += 1;

        let value = unsafe { slot.value.assume_init_read() };

        // Ensure that the generation never reaches a point where it could never be safely
        // invalidated.
        if slot.generation != u32::MAX - 1 {
            self.header.free_slots.push(slot_idx);
        }

        Some(value)
    }

    pub fn upgrade(&self, manager: &ArenaManager, handle: RawHandle) -> Option<KeepAlive> {
        assert_eq!(Some(manager.uid), self.header.manager_uid);

        // Ensure the handle is live.
        if self
            .slots
            .get(handle.slot_idx as usize)
            .is_none_or(|v| v.generation != handle.generation.get())
        {
            return None;
        }

        // Upgrade the handle.
        let ptr = self.header.keep_alive_slots[handle.slot_idx as usize];

        Some(KeepAlive(unsafe { manager.listener.upgrade(ptr) }))
    }

    pub fn get(&self, handle: RawHandle) -> Option<&T> {
        self.slots
            .get(handle.slot_idx as usize)
            .filter(|v| v.generation == handle.generation.get())
            .map(|v| unsafe { v.value.assume_init_ref() })
    }

    pub fn get_mut(&mut self, handle: RawHandle) -> Option<&mut T> {
        self.slots
            .get_mut(handle.slot_idx as usize)
            .filter(|v| v.generation == handle.generation.get())
            .map(|v| unsafe { v.value.assume_init_mut() })
    }

    pub fn len(&self) -> u32 {
        self.slots.len() as u32
    }

    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }
}

struct Slot<T> {
    generation: u32,
    value: MaybeUninit<T>,
}

impl<T> Drop for Slot<T> {
    fn drop(&mut self) {
        if self.generation % 2 != 1 {
            return;
        }

        unsafe { MaybeUninit::assume_init_drop(&mut self.value) };
    }
}

// === Handles === //

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct KeepAlive(KeepAliveStrong<KeepAliveSlot>);

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct RawHandle {
    slot_idx: u32,
    generation: NonZeroU32,
}

impl fmt::Debug for RawHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, v{}]", self.slot_idx, self.generation.get())
    }
}

impl RawHandle {
    pub const DANGLING: Self = Self {
        slot_idx: u32::MAX,
        generation: NonZeroU32::new(u32::MAX).unwrap(),
    };

    pub const fn slot(self) -> u32 {
        self.slot_idx
    }

    pub const fn generation(self) -> NonZeroU32 {
        self.generation
    }
}
