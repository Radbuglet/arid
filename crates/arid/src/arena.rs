use std::{fmt, mem::MaybeUninit, num::NonZeroU32};

use derive_where::derive_where;

// === Arena === //

#[derive_where(Default)]
pub struct Arena<T> {
    free_slots: Vec<u32>,
    slots: Vec<Slot<T>>,
}

impl<T> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Arena").finish_non_exhaustive()
    }
}

impl<T> Arena<T> {
    pub fn insert(&mut self, value: T) -> RawHandle {
        // Attempt to reuse an existing empty slot.
        if let Some(slot_idx) = self.free_slots.pop() {
            let slot = &mut self.slots[slot_idx as usize];

            slot.generation += 1;

            slot.value.write(value);

            return RawHandle {
                slot_idx,
                generation: unsafe { NonZeroU32::new_unchecked(slot.generation) },
            };
        }

        // Otherwise, create a new slot.
        let slot_idx = u32::try_from(self.slots.len()).expect("too many slots");

        self.slots.push(Slot {
            generation: 1,
            value: MaybeUninit::new(value),
        });

        RawHandle {
            slot_idx,
            generation: NonZeroU32::new(1).unwrap(),
        }
    }

    pub fn slot_to_handle(&self, slot_idx: u32) -> Option<RawHandle> {
        let state = &self.slots[slot_idx as usize];

        (state.generation % 2 == 1).then(|| RawHandle {
            slot_idx,
            generation: NonZeroU32::new(state.generation).unwrap(),
        })
    }

    pub fn remove(&mut self, handle: RawHandle) -> Option<T> {
        _ = self.get(handle)?;

        let slot = &mut self.slots[handle.slot() as usize];

        // Take slot contents and mark it as invalid.
        slot.generation += 1;

        let value = unsafe { slot.value.assume_init_read() };

        // Ensure that the generation never reaches a point where it could never be safely
        // invalidated.
        if slot.generation != u32::MAX - 1 {
            self.free_slots.push(handle.slot());
        }

        Some(value)
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

// === RawHandle === //

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

    pub const fn new(slot_idx: u32, generation: NonZeroU32) -> Option<Self> {
        if generation.get() % 2 != 1 {
            return None;
        }

        Some(Self {
            slot_idx,
            generation,
        })
    }

    pub const fn slot(self) -> u32 {
        self.slot_idx
    }

    pub const fn generation(self) -> NonZeroU32 {
        self.generation
    }
}
