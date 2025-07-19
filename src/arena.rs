use std::{
    fmt, hash,
    marker::PhantomData,
    mem::MaybeUninit,
    num::NonZeroU32,
    rc::{Rc, Weak},
};

use crate::W;

// === KeepAlive === //

mod dtor_queue {
    use std::{cell::UnsafeCell, mem};

    use super::DestructorTask;

    thread_local! {
        static DESTRUCTOR_QUEUE: UnsafeCell<Vec<DestructorTask>>
            = const { UnsafeCell::new(Vec::new()) };
    }

    pub fn push_task(task: DestructorTask) {
        DESTRUCTOR_QUEUE.with(|queue| {
            // SAFETY: the `DESTRUCTOR_QUEUE` is only ever accessed in two places and both
            // places are non-reentrant with respect to one another.
            unsafe { &mut *queue.get() }.push(task);
        })
    }

    pub fn take_tasks() -> Vec<DestructorTask> {
        DESTRUCTOR_QUEUE.with(|queue| {
            // SAFETY: the `DESTRUCTOR_QUEUE` is only ever accessed in two places and both
            // places are non-reentrant with respect to one another.
            mem::take(unsafe { &mut *queue.get() })
        })
    }
}

pub fn flush(w: W) {
    loop {
        let tasks = dtor_queue::take_tasks();

        if tasks.is_empty() {
            break;
        }

        for task in tasks {
            (task.destructor)(task.handle, w);
        }
    }
}

#[derive(Debug, Clone)]
pub struct KeepAlive(Rc<KeepAlivePointee>);

impl Eq for KeepAlive {}

impl PartialEq for KeepAlive {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl hash::Hash for KeepAlive {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

#[derive(Debug)]
struct KeepAlivePointee {
    _not_send_sync: PhantomData<*const ()>,
    task: DestructorTask,
}

impl Drop for KeepAlivePointee {
    fn drop(&mut self) {
        dtor_queue::push_task(self.task);
    }
}

#[derive(Debug, Copy, Clone)]
struct DestructorTask {
    handle: RawHandle,
    destructor: fn(RawHandle, W),
}

fn make_keep_alive(handle: RawHandle, destructor: fn(RawHandle, W)) -> Rc<KeepAlivePointee> {
    Rc::new(KeepAlivePointee {
        _not_send_sync: PhantomData,
        task: DestructorTask { handle, destructor },
    })
}

// === Arena === //

pub struct Arena<T> {
    free_slots: Vec<u32>,
    keep_alive_slots: Vec<Weak<KeepAlivePointee>>,
    // FIXME: We may need a non-`Unique`-guarded vector.
    slots: Vec<Slot<T>>,
    destructor: fn(RawHandle, W),
}

impl<T> Arena<T> {
    pub const fn new(destructor: fn(RawHandle, W)) -> Self {
        Self {
            free_slots: Vec::new(),
            keep_alive_slots: Vec::new(),
            slots: Vec::new(),
            destructor,
        }
    }

    pub fn insert(&mut self, value: T) -> (RawHandle, KeepAlive) {
        // Allocate a handle
        let handle = if let Some(slot_idx) = self.free_slots.pop() {
            let slot = &mut self.slots[slot_idx as usize];

            slot.generation += 1;
            slot.value.write(value);

            RawHandle {
                slot_idx,
                // SAFETY: we ensure that our generation will never reach above `u32::MAX - 1` in
                // our `try_commit_removal` method.
                generation: unsafe { NonZeroU32::new_unchecked(slot.generation) },
            }
        } else {
            // Otherwise, create a new slot.
            let slot_idx = u32::try_from(self.slots.len()).expect("too many slots");

            self.keep_alive_slots.push(Weak::default());
            self.slots.push(Slot {
                generation: 1,
                value: MaybeUninit::new(value),
            });

            RawHandle {
                slot_idx,
                generation: NonZeroU32::new(1).unwrap(),
            }
        };

        // Insert a keep-alive.
        let keep_alive = make_keep_alive(handle, self.destructor);

        self.keep_alive_slots[handle.slot_idx as usize] = Rc::downgrade(&keep_alive);

        (handle, KeepAlive(keep_alive))
    }

    pub fn try_commit_removal(&mut self, handle: RawHandle) -> Option<T> {
        let slot = &mut self.slots[handle.slot_idx as usize];

        // Is the handle valid?
        if slot.generation != handle.generation.get() {
            return None;
        }

        // Is it being kept alive?
        if self.keep_alive_slots[handle.slot_idx as usize].strong_count() > 0 {
            // (looks like a resurrection?)
            return None;
        }

        // Take slot contents and mark it as invalid.
        slot.generation += 1;

        // SAFETY: we incremented our generation from odd to even, meaning that the slot went from
        // logically initialized to uninitialized.
        let value = unsafe { slot.value.assume_init_read() };

        // Ensure that the generation never reaches a point where it could never be safely
        // invalidated.
        if slot.generation != u32::MAX - 1 {
            self.free_slots.push(handle.slot_idx);
        }

        Some(value)
    }

    pub fn upgrade(&mut self, handle: RawHandle) -> Option<KeepAlive> {
        // Ensure the handle is live.
        if self
            .slots
            .get(handle.slot_idx as usize)
            .is_none_or(|v| v.generation != handle.generation.get())
        {
            return None;
        }

        // Upgrade the handle.
        let keep_alive_slot = &mut self.keep_alive_slots[handle.slot_idx as usize];
        let keep_alive = keep_alive_slot.upgrade().unwrap_or_else(|| {
            let keep_alive = make_keep_alive(handle, self.destructor);
            *keep_alive_slot = Rc::downgrade(&keep_alive);
            keep_alive
        });

        Some(KeepAlive(keep_alive))
    }

    pub fn get(&self, handle: RawHandle) -> Option<&T> {
        self.slots
            .get(handle.slot_idx as usize)
            .filter(|v| v.generation == handle.generation.get())
            // SAFETY: handle generations are odd, meaning that this value is initialized.
            .map(|v| unsafe { v.value.assume_init_ref() })
    }

    pub fn get_mut(&mut self, handle: RawHandle) -> Option<&mut T> {
        self.slots
            .get_mut(handle.slot_idx as usize)
            .filter(|v| v.generation == handle.generation.get())
            // SAFETY: handle generations are odd, meaning that this value is initialized.
            .map(|v| unsafe { v.value.assume_init_mut() })
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

        // SAFETY: by invariant, odd generations mean that this value is occupied.
        unsafe { self.value.assume_init_drop() };
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

    pub const fn slot(self) -> u32 {
        self.slot_idx
    }

    pub const fn generation(self) -> NonZeroU32 {
        self.generation
    }
}
