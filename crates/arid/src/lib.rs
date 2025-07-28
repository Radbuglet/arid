//! An ergonomic object-model for Rust.
//!
//! Behold, a doubly-linked-list! No `RefCell`s are required despite the data-structure being full
//! of reference cycles.
//!
//! ```
//! use arid::{Object as _, Handle as _, object, Strong, W};
//!
//! #[derive(Debug)]
//! pub struct List {
//!     head: Option<Strong<NodeHandle>>,
//!     tail: Option<NodeHandle>,
//! }
//!
//! #[derive(Debug)]
//! pub struct Node {
//!     list: Option<ListHandle>,
//!     value: u32,
//!     prev: Option<NodeHandle>,
//!     next: Option<Strong<NodeHandle>>,
//! }
//!
//! object!(pub List, pub Node);
//!
//! impl ListHandle {
//!     pub fn new(w: W) -> Strong<Self> {
//!         List { head: None, tail: None }.spawn(w)
//!     }
//!
//!     pub fn add_after(self, prev: Option<NodeHandle>, node: NodeHandle, w: W) {
//!         // Validate operation.
//!         assert!(node.r(w).list.is_none());
//!         assert!(prev.is_none_or(|prev| prev.r(w).list == Some(self)));
//!
//!         node.m(w).list = Some(self);
//!
//!         // Establish links with the previous node.
//!         node.m(w).prev = prev;  // prev <- node
//!
//!         // prev -> node
//!         let node_strong = node.as_strong(w);
//!         let next = if let Some(prev) = prev {
//!             prev.m(w).next.replace(node_strong)
//!         } else {
//!             self.m(w).head = Some(node_strong);
//!             None
//!         };
//!
//!         // Establish links with the next node.
//!         // node <- next
//!         if let Some(next) = next.as_ref() {
//!             next.m(w).prev = Some(node);
//!         } else {
//!             self.m(w).tail = Some(node);
//!         }
//!
//!         node.m(w).next = next;  // node -> next
//!     }
//! }
//! ```
//!
//! ## Basic Usage
//!
//! TODO
//!
//! ## Destructors
//!
//! TODO
//!
//! ## Polymorphism
//!
//! TODO
//!
//! ## Custom Arenas
//!
//! TODO
//!
//! ## Limitations
//!
//! TODO
//!

mod utils;

mod arena;
pub use self::arena::*;

mod handle;
pub use self::handle::*;

mod world;
pub use self::world::*;

mod wrappers;
pub use self::wrappers::*;
