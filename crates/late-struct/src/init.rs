use std::{
    alloc::Layout,
    any::TypeId,
    collections::HashMap,
    sync::{Once, atomic::Ordering::*},
};

use crate::{RawLateFieldDescriptor, RawLateStructDescriptor};

/// A marker type indicating that the layouts for all [`late_struct!`](super::late_struct)s
/// throughout the compiled artifact have been determined at runtime.
///
/// This initialization process happens automatically upon the first instantiation of a
/// [`LateInstance`](super::LateInstance). Indeed, you can use the [`init_token`](super::LateInstance::init_token)
/// method of a `LateInstance` to obtain an instance of this struct for free.
#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub struct LateLayoutInitToken;

impl Default for LateLayoutInitToken {
    fn default() -> Self {
        Self::new()
    }
}

impl LateLayoutInitToken {
    /// Resolves all late-initialized structures in the compiled artifact and returns a token
    /// attesting to this fact. The late initialization process only happens once during the
    /// program's lifetime.
    ///
    /// The late-initialization routine does not makes uncontrolled calls to user-controlled
    /// functions.
    pub fn new() -> Self {
        static ONCE: Once = Once::new();

        ONCE.call_once(|| {
            use crate::late_macro_internals::{iter_late_fields, iter_late_structs};

            let mut structs = HashMap::<
                TypeId,
                (
                    &'static RawLateStructDescriptor,
                    Vec<&'static RawLateFieldDescriptor>,
                ),
            >::new();

            for entry in iter_late_structs() {
                structs.insert(entry.struct_type, (entry.descriptor, Vec::new()));
            }

            for entry in iter_late_fields() {
                structs
                    .get_mut(&entry.struct_type)
                    .unwrap()
                    .1
                    .push(entry.descriptor);
            }

            for (struct_desc, struct_fields) in structs.into_values() {
                let struct_fields: &[_] = &*Box::leak(Box::from_iter(struct_fields));
                let struct_fields_p = Box::leak(Box::new(struct_fields));

                let mut overall_layout = Layout::new::<()>();

                for (i, field) in struct_fields.iter().enumerate() {
                    let (new_layout, offset) = overall_layout.extend(field.layout()).unwrap();
                    field.index.store(i, Relaxed);
                    field.offset.store(offset, Relaxed);
                    overall_layout = new_layout;
                }

                struct_desc.size.store(overall_layout.size(), Relaxed);
                struct_desc.align.store(overall_layout.align(), Relaxed);
                struct_desc.fields.store(struct_fields_p, Relaxed);
            }
        });

        Self
    }

    /// Unsafely asserts that the layout for all late-initialized structures in the compiled
    /// artifact have already been resolved.
    ///
    /// Unless you have specific performance reasons to do so, you should probably be using
    /// [`LateLayoutInitToken::new`].
    ///
    /// ## Safety
    ///
    /// Although this method cannot, by itself, cause undefined behavior, passing this object to a
    /// method which expects the layout of late-initialized structure to already be resolved when
    /// the layouts are not yet resolved could easily cause undefined behavior.
    pub const unsafe fn new_unchecked() -> Self {
        Self
    }
}
