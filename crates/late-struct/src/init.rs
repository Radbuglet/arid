use std::{
    alloc::Layout,
    any::TypeId,
    collections::HashMap,
    sync::{Once, atomic::Ordering::*},
};

use crate::{RawLateFieldDescriptor, RawLateStructDescriptor};

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub struct LateLayoutInitToken;

impl Default for LateLayoutInitToken {
    fn default() -> Self {
        Self::new()
    }
}

impl LateLayoutInitToken {
    pub unsafe fn new_unchecked() -> Self {
        Self
    }

    pub fn new() -> Self {
        static ONCE: Once = Once::new();

        ONCE.call_once(|| {
            use crate::late_macro_internals::{LATE_FIELDS, LATE_STRUCTS};

            let mut structs = HashMap::<
                TypeId,
                (
                    &'static RawLateStructDescriptor,
                    Vec<&'static RawLateFieldDescriptor>,
                ),
            >::new();

            for entry in LATE_STRUCTS {
                let entry = entry();

                structs.insert(entry.struct_type, (entry.descriptor, Vec::new()));
            }

            for entry in LATE_FIELDS {
                let entry = entry();

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
}
