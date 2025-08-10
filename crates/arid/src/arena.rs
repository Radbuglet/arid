use std::{fmt, mem::MaybeUninit, num::NonZeroU32};

use derive_where::derive_where;

// === RawArena === //

/// A generational object arena mapping [`RawHandle`]s to values of type `T`.
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
///
/// A generational arena can be though of as an extremely efficient `HashMap` from [`RawHandle`]s to
/// values of type `T`. You can insert values through [`RawArena::insert`], get them using
/// [`RawArena::get`] and [`RawArena::get_mut`] and remove them using [`RawArena::remove`].
///
/// Generational arenas achieve their efficiency by storing entries in a single `Vec` of "slots,"
/// which contain values of type `T` alongside a `u32` describing that slot's "generation." Every
/// time a slot is `remove`d and reused, its generation number is incremented. A handle is a pairing
/// of the index of the value's slot in the array alongside the slot's generation at the time it was
/// allocated. When we wish to `get` a value from the arena, we check the `RawHandle`'s generation
/// against the slot's current generation. If they match, we know that the `RawHandle` is still
/// valid and return the value. If they mismatch, we know that the `RawHandle` given to us was
/// pointing to a value which had previously been `remove`d and then reallocated into, telling us to
/// return `None`.
#[derive_where(Default)]
pub struct RawArena<T> {
    free_slots: Vec<u32>,
    slots: Vec<Slot<T>>,
}

impl<T> fmt::Debug for RawArena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RawArena").finish_non_exhaustive()
    }
}

impl<T> RawArena<T> {
    /// Inserts the provided `value` into the arena, returning its [`RawHandle`].
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

    /// Converts a `slot_idx` into a [`RawHandle`] by extending it with the slot's current
    /// generation. If the slot is currently vacant, `None` is returned.
    ///
    /// This method could be used to compress a [`RawHandle`] that you know must be valid from two
    /// `u32`s to a single one.
    pub fn slot_to_handle(&self, slot_idx: u32) -> Option<RawHandle> {
        let state = &self.slots[slot_idx as usize];

        (state.generation % 2 == 1).then(|| RawHandle {
            slot_idx,
            generation: NonZeroU32::new(state.generation).unwrap(),
        })
    }

    /// Removes a value by its supplied `handle`, returning it if the `handle` was valid and `None`
    /// if it was not.
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

    /// Maps a [`RawHandle`] to an immutable reference to its corresponding value, returning `None`
    /// if the handle was [`remove`](RawArena::remove)d.
    pub fn get(&self, handle: RawHandle) -> Option<&T> {
        self.slots
            .get(handle.slot_idx as usize)
            .filter(|v| v.generation == handle.generation.get())
            .map(|v| unsafe { v.value.assume_init_ref() })
    }

    /// Maps a [`RawHandle`] to a mutable reference to its corresponding value, returning `None`
    /// if the handle was [`remove`](RawArena::remove)d.
    pub fn get_mut(&mut self, handle: RawHandle) -> Option<&mut T> {
        self.slots
            .get_mut(handle.slot_idx as usize)
            .filter(|v| v.generation == handle.generation.get())
            .map(|v| unsafe { v.value.assume_init_mut() })
    }

    /// Returns the number of slots in the arena.
    ///
    /// This could be used to ensure that auxiliary arrays matching the structure of the arena
    /// maintain the correctly length as the arena is [`insert`](RawArena::insert)ed into and
    /// [`remove`](RawArena::remove)d from.
    pub fn slot_count(&self) -> u32 {
        self.slots.len() as u32
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

/// A handle into a [`RawArena`].
///
/// <div class="warning">
/// This is likely only relevant to you if you are <a href="index.html#custom-arenas">implementing a
/// custom arena</a>.
/// </div>
///
/// Each [`Handle`](crate::Handle) is backed by one of these. You can convert a `RawHandle` into a
/// `Handle` using [`Handle::from_raw`](crate::Handle::from_raw) and a `Handle` back into a
/// `RawHandle` using [`Handle::raw`](crate::Handle::raw).
///
/// A handle is comprised of two `u32` parts: its `slot` index and its `generation`, the latter
/// always being an odd number. See the item documentation for [`RawArena`] for more details on how
/// to interpret these fields.
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
    /// A `RawHandle` which never points to anything.
    pub const DANGLING: Self = Self {
        slot_idx: u32::MAX,
        // This never points to anything because we never allocate a generation this large. This is
        // because generations that large can never be deallocated.
        generation: NonZeroU32::new(u32::MAX).unwrap(),
    };

    /// Attempts to convert a pair of `slot_idx` and `generation` into a `RawHandle`.
    ///
    /// If the provided `generation` is odd, `None` will be returned.
    pub const fn new(slot_idx: u32, generation: u32) -> Option<Self> {
        if generation % 2 != 1 {
            return None;
        }

        Some(Self {
            slot_idx,
            generation: NonZeroU32::new(generation).unwrap(),
        })
    }

    /// Fetches the handle's `slot`.
    pub const fn slot(self) -> u32 {
        self.slot_idx
    }

    /// Fetches the handle's `generation`.
    pub const fn generation(self) -> NonZeroU32 {
        self.generation
    }
}
