use std::{fmt, mem::MaybeUninit, num::NonZeroU32};

use derive_where::derive_where;
use late_struct::{LateInstance, LateStruct};
use thin_vec::ThinVec;

use crate::utils::keep_alive::{KeepAliveList, KeepAlivePtr, KeepAliveStrong};

// === Flush === //

pub trait Flush {
    #[must_use]
    fn flush(&mut self) -> bool;

    fn flush_all(&mut self) {
        while self.flush() {}
    }
}

impl<S> Flush for LateInstance<S>
where
    S: LateStruct,
    S::EraseTo: Flush,
{
    fn flush(&mut self) -> bool {
        let mut made_progress = false;

        for field in self.fields() {
            made_progress |= self.get_erased_mut(field).flush();
        }

        made_progress
    }
}

// === Arena === //

#[derive_where(Default)]
pub struct Arena<T> {
    header: Box<ArenaHeader>,
    slots: ThinVec<Slot<T>>,
}

#[derive(Default)]
struct ArenaHeader {
    free_slots: Vec<u32>,
    condemn_slots: Vec<KeepAlivePtr<u32>>,
    condemn_listener: KeepAliveList<u32>,
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

    pub fn spawn(&mut self, value: T) -> (RawHandle, KeepAlive) {
        if let Some(slot_idx) = self.header.free_slots.pop() {
            let slot = &mut self.slots[slot_idx as usize];

            slot.generation += 1;

            let keep_alive = self.header.condemn_slots[slot_idx as usize];
            let keep_alive = KeepAlive(unsafe { self.header.condemn_listener.upgrade(keep_alive) });

            slot.value.write(value);

            let handle = RawHandle {
                slot_idx,
                generation: unsafe { NonZeroU32::new_unchecked(slot.generation) },
            };

            return (handle, keep_alive);
        }

        let slot_idx = u32::try_from(self.slots.len()).expect("too many slots");

        self.slots.push(Slot {
            generation: 1,
            value: MaybeUninit::new(value),
        });

        let keep_alive = KeepAlive(self.header.condemn_listener.spawn(slot_idx));

        let handle = RawHandle {
            slot_idx,
            generation: NonZeroU32::new(1).unwrap(),
        };

        (handle, keep_alive)
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

    pub fn upgrade(&self, handle: RawHandle) -> Option<KeepAlive> {
        if self
            .slots
            .get(handle.slot_idx as usize)
            .is_none_or(|v| v.generation != handle.generation.get())
        {
            return None;
        }

        let keep_alive = self.header.condemn_slots[handle.slot_idx as usize];
        let keep_alive = KeepAlive(unsafe { self.header.condemn_listener.upgrade(keep_alive) });

        Some(keep_alive)
    }
}

impl<T> Flush for Arena<T> {
    fn flush(&mut self) -> bool {
        let mut made_progress = false;

        loop {
            let Some((_, slot_idx)) = self.header.condemn_listener.take_condemned() else {
                break;
            };

            let slot = &mut self.slots[slot_idx as usize];

            // Mark slot contents as invalid.
            slot.generation += 1;

            // Drop them (could panic!)
            made_progress |= true;

            unsafe { slot.value.assume_init_drop() };

            // Ensure that the generation never reaches a point where it could never be safely
            // invalidated.
            if slot.generation == u32::MAX - 1 {
                continue;
            }

            // Add a new free slot.
            self.header.free_slots.push(slot_idx);
        }

        made_progress
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

// === RawHandle === //

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct KeepAlive(KeepAliveStrong<u32>);

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

// === Tests === //

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_flush() {
        let mut arena = Arena::new();

        let (handle, keep_alive) = arena.spawn(());

        assert!(arena.get(handle).is_some());

        arena.flush_all();

        assert!(arena.get(handle).is_some());

        drop(keep_alive);

        assert!(arena.get(handle).is_some());

        arena.flush_all();

        assert!(arena.get(handle).is_none());
    }
}
