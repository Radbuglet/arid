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
//! ## Motivation
//!
//! The core idea behind `arid` is to tie all object borrows to some parent [`World`] instance. That
//! is, we make each smart-pointer accessible by methods such as these...
//!
//! ```
//! # struct World;
//! # struct MyObjectHandle;
//! # struct MyObject;
//! impl MyObjectHandle {
//!     /// Borrow the smart-pointer's value immutably.
//!     fn r<'w>(self, world: &'w World) -> &'w MyObject {
//! # /*
//!         ...
//! # */ todo!()
//!     }
//!
//!     /// Borrow the smart-pointer's value mutably.
//!     fn m<'w>(self, world: &'w mut World) -> &'w mut MyObject {
//! # /*
//!         ...
//! # */ todo!()
//!     }
//! }
//! ```
//!
//! One major implication of this model is that no borrows have to be validated at runtime,
//! eliminating an entire class of runtime bugs and reducing runtime overhead slightly. This is
//! especially valuable since runtime borrow checker violations can happen at a distance, may
//! not always be exercised under all circumstances, and can be introduced invisibly depending on
//! the timing of [`Ref`](std::cell::Ref) guard drops.
//!
//! The other major implication of this model, however, is that *only one object can be borrowed at
//! a time!* The "magic" of `arid`, then, is the way it hides this restriction in practice. We do
//! this in three main ways:
//!
//! 1. First, we make smart pointers (a.k.a handles) [`Copy`]able.
//!
//!    This removes one incentive for users to create long-term borrows from a dereferenced object
//!    since handles can be implicitly copied out of the dereferenced object without needing an
//!    explicit `.clone()`. By making more borrow sites short-lived, we avoid a large number of
//!    borrow checker violations which may otherwise crop up from our strict "single borrow at a
//!    time" restriction.
//!
//! 2. Second, we allow smart pointers be to receivers on `impl` blocks.
//!
//!    This allows users to interweave multiple mutable borrows within a single method body while
//!    still keeping a subject-verb-style calling syntax for object methods.
//!
//! 3. Third, we set a convention to always pass the `world` at the end of each argument list.
//!
//!    This is quite important since arguments are evaluated in the order they appear in a function
//!    call. If, instead, we passed the `world` in the first argument of a call expression, the
//!    subsequent argument expressions would have to contend with a concurrent borrow in that first
//!    argument.
//!
//! These three decisions placate the borrow checker for most usage patterns, making the system
//! quite ergonomic compared to its more traditional alternatives.
//!
//! ## Basic Usage
//!
//! All object instances in the `arid` object model are owned by exactly one [`World`]. It can be
//! instantiated anywhere with...
//!
//! ```
//! use arid::World;
//!
//! let mut w = World::new();
//! let w = &mut w;
//! ```
//!
//! By convention, we try to ensure that the `world` for any given function body is named `w` and
//! corresponds to an (im)mutable borrow of the world.
//!
//! We can then define the object types which live inside a world using the [`object!`] macro like
//! so...
//!
//! ```
//! use arid::object;
//!
//! #[derive(Debug)]
//! pub struct MyObject {
//!     count: u32,
//! }
//!
//! object!(pub MyObject);
//! ```
//!
//! The `object!` macro takes the name of a structure within the current scope (e.g. `MyObject`) and
//! does a couple things...
//!
//! - It implements the [`Object`] trait for the target type `MyObject`.
//! - It defines a newtype for handles of that object and calls the newtype `<StructName>Handle`
//!   (in our case, `MyObjectHandle`). The visibility of this newtype is taken from the macro
//!   invocation and must match the visibility of the value structure.
//! - It implements the [`Handle`] trait for that handle newtype structure.
//!
//! The requirements for defining an object are minimal: it must be [`Sized`], live for `'static`,
//! and implement [`Debug`](std::fmt::Debug).
//!
//! The `Object` trait exposes an [`Object::spawn`] method to allocate an object instance into a
//! given `World`. We can use it like so...
//!
//! ```rust
//! # use arid::World;
//! #
//! # let mut w = World::new();
//! # let w = &mut w;
//! #
//! # use arid::object;
//! #
//! # #[derive(Debug)]
//! # pub struct MyObject {
//! #     count: u32,
//! # }
//! #
//! # object!(pub MyObject);
//! #
//! use arid::Object as _;
//!
//! let my_counter = MyObject { count: 1 }.spawn(w);
//! ```
//!
//! We can then access the handle's value immutably using the [`Handle::r`] method and mutably using
//! the [`Handle::m`] method. These methods are short aliases for the more conventionally-named
//! [`Handle::get`] and [`Handle::get_mut`] methods respectively.
//!
//! ```rust
//! # use arid::World;
//! #
//! # let mut w = World::new();
//! # let w = &mut w;
//! #
//! # use arid::object;
//! #
//! # #[derive(Debug)]
//! # pub struct MyObject {
//! #     count: u32,
//! # }
//! #
//! # object!(pub MyObject);
//! #
//! # use arid::Object as _;
//! #
//! # let my_counter = MyObject { count: 1 }.spawn(w);
//! use arid::Handle as _;
//!
//! my_counter.m(w).count += 1;
//! assert_eq!(my_counter.r(w).count, 2);
//! ```
//!
//! Since each object's corresponding handle newtype is declared in the crate which invoked the
//! `object!` macro, we are allowed to implement inherent methods and traits directly onto the
//! handle.
//!
//! ```rust
//! # use arid::World;
//! #
//! # let mut w = World::new();
//! # let w = &mut w;
//! #
//! # use arid::object;
//! #
//! # #[derive(Debug)]
//! # pub struct MyObject {
//! #     count: u32,
//! # }
//! #
//! # object!(pub MyObject);
//! #
//! # use arid::Object as _;
//! #
//! # let my_counter = MyObject { count: 1 }.spawn(w);
//! # use arid::Handle as _;
//! #
//! # my_counter.m(w).count += 1;
//! # assert_eq!(my_counter.r(w).count, 2);
//! use arid::{W, Wr};
//!
//! impl MyObjectHandle {
//!     pub fn increment(self, w: W) {
//!         self.m(w).count += 1;
//!     }
//!
//!     pub fn is_less_than(self, other: u32, w: Wr) -> bool {
//!         self.r(w).count < other
//!     }
//! }
//!
//! assert!(my_counter.is_less_than(3, w));
//! my_counter.increment(w);
//! assert!(!my_counter.is_less_than(3, w));
//! ```
//!
//! Note that [`W`] is just an alias to a `&mut World` and [`Wr`] is just an alias to a `&World`.
//! Rust allows you to elide the lifetime of these type aliases in most cases. Rust implicitly
//! reborrows references when they're passed directly to a function, which allows us to avoid
//! explicit `&mut *w` and `&*w` reborrowing syntax.
//!
//! <div class="warning">
//!
//! Also note that, by convention, ***the `world` parameter always goes last*** to help the borrow
//! checker understand more valid code.
//!
//! <details><summary><strong style="cursor: pointer">Justification</strong></summary>
//!
//! This convention is valuable because Rust always evaluates function call arguments in their
//! syntactic order. If the world were to be passed first, the function's borrow of that world would
//! happen before all subsequent arguments were evaluated, preventing those arguments from borrowing
//! the world mutably.
//!
//! This code disrespects the conventions and gets a borrow checker error:
//!
//! ```compile_fail
//! # use arid::World;
//! #
//! # let mut w = World::new();
//! # let w = &mut w;
//! #
//! # use arid::object;
//! #
//! # #[derive(Debug)]
//! # pub struct MyObject {
//! #     count: u32,
//! # }
//! #
//! # object!(pub MyObject);
//! #
//! # use arid::Object as _;
//! #
//! # let my_counter = MyObject { count: 1 }.spawn(w);
//! # use arid::Handle as _;
//! #
//! # my_counter.m(w).count += 1;
//! # assert_eq!(my_counter.r(w).count, 2);
//! # use arid::{W, Wr};
//! impl MyObjectHandle {
//!     pub fn increment_by(self, w: W, delta: u32) {
//!         self.m(w).count += delta;
//!     }
//! }
//!
//! // Double the count!
//! my_counter.increment_by(w, my_counter.m(w).count);
//! ```
//!
//! ```text
//! error[E0499]: cannot borrow `*w` as mutable more than once at a time
//!   --> convention.rs:33:41
//!    |
//! 33 | my_counter.increment_by(w, my_counter.m(w).count);
//!    |            ------------ -               ^ second mutable borrow occurs here
//!    |            |            |
//!    |            |            first mutable borrow occurs here
//!    |            first borrow later used by call
//!    |
//! help: try adding a local storing this argument...
//!   --> convention.rs:33:28
//!    |
//! 33 | my_counter.increment_by(w, my_counter.m(w).count);
//!    |                            ^^^^^^^^^^^^^^^
//! help: ...and then using that local as the argument to this call
//!   --> convention.rs:33:1
//!    |
//! 33 | my_counter.increment_by(w, my_counter.m(w).count);
//!    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//! ```
//!
//! If we reordered the arguments, the error would go away!
//!
//! ```
//! # use arid::World;
//! #
//! # let mut w = World::new();
//! # let w = &mut w;
//! #
//! # use arid::object;
//! #
//! # #[derive(Debug)]
//! # pub struct MyObject {
//! #     count: u32,
//! # }
//! #
//! # object!(pub MyObject);
//! #
//! # use arid::Object as _;
//! #
//! # let my_counter = MyObject { count: 1 }.spawn(w);
//! # use arid::Handle as _;
//! #
//! # my_counter.m(w).count += 1;
//! # assert_eq!(my_counter.r(w).count, 2);
//! # use arid::{W, Wr};
//! impl MyObjectHandle {
//!     pub fn increment_by(self, delta: u32, w: W) {
//!         self.m(w).count += delta;
//!     }
//! }
//!
//! // Double the count!
//! my_counter.increment_by(my_counter.m(w).count, w);
//! ```
//!
//! </details>
//! </div>
//!
//! TODO: `debug` method
//!
//! ## Lifecycle
//!
//! Objects in `arid` are reference-counted although their semantics are a bit special:
//!
//! - The `Handle` newtypes generated by the `object!` macro are weak references but are assumed to
//!   be valid. As such, you can call `.r()` and `.w()` on them directly. These types are `Copy`able.
//! - [`Strong`] wrappers around handles are, as their name suggest, strong references. These
//!   objects [`Deref`](std::ops::Deref) to their underlying weak `Handle` newtype. These types are
//!   *not* `Copy`able but are `Clone`able.
//! - [`MayDangle`] are wrappers around handle newtypes which force the user to explicitly check for
//!   dangling values using [`MayDangle::get`] or [`MayDangle::unwrap`] before dereferencing the
//!   value. These types are `Copy`able.
//!
//! The [`Object::spawn`] method returns a `Strong` directly and handle newtypes can be upgraded
//! into `Strong` references using the [`Handle::as_strong`] method.
//!
//! Objects are not destroyed immediately upon their reference count reaching zero. Instead, all
//! deletions are queued until the [`World::flush`] method is called. This means that objects
//! without any remaining strong references to them may be "resurrected" using the
//! `Handle::as_strong` method like so:
//!
//! ```rust
//! # use arid::World;
//! #
//! # let mut w = World::new();
//! # let w = &mut w;
//! #
//! # use arid::object;
//! #
//! # #[derive(Debug)]
//! # pub struct MyObject {
//! #     count: u32,
//! # }
//! #
//! # object!(pub MyObject);
//! use arid::{Handle as _, Object as _, Strong};
//!
//! let my_counter_strong: Strong<MyObjectHandle> = MyObject { count: 1 }.spawn(w);
//! let my_counter_weak: MyObjectHandle = *my_counter_strong;
//!
//! // We still have a strong reference to our counter so nothing gets deleted.
//! w.flush();
//! assert!(my_counter_weak.is_alive(w));
//!
//! // We dropped the last remaining strong reference but re-created it with `as_strong`
//! // before the next flush so the object is still alive.
//! drop(my_counter_strong);
//! assert!(my_counter_weak.is_alive(w));
//! let my_counter_strong = my_counter_weak.as_strong(w);
//! w.flush();
//! assert!(my_counter_weak.is_alive(w));
//!
//! // Finally, we can drop the value!
//! drop(my_counter_strong);
//! assert!(my_counter_weak.is_alive(w));
//! w.flush();
//! assert!(!my_counter_weak.is_alive(w));
//! ```
//!
//! You can define a custom destructor for a given object type by implementing the [`Destructor`]
//! trait on its handle. This method is called during the call to `World::flush` immediately before
//! the value is properly destroyed.
//!
//! ```rust
//! # use arid::World;
//! #
//! # let mut w = World::new();
//! # let w = &mut w;
//! use arid::{Destructor, Handle, Object, object, W};
//!
//! #[derive(Debug)]
//! pub struct DtorObserver {
//!     name: &'static str,
//! }
//!
//! object!(pub DtorObserver);
//!
//! impl Destructor for DtorObserverHandle {
//!     fn pre_destroy(self, w: W) {
//!         println!("Object named {} has been destroyed!", self.r(w).name);
//!     }
//! }
//!
//! let object = DtorObserver { name: "Max" }.spawn(w);
//!
//! drop(object);
//! w.flush();  // "Object named Max has been destroyed!"
//! ```
//!
//!
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
