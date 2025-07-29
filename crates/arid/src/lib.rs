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
//! # Motivation
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
//! 2. Second, we allow smart pointers to be receivers on `impl` blocks.
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
//! # Basic Usage
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
//! use arid::Object as _;
//!
//! let my_counter = MyObject { count: 1 }.spawn(w);
//! ```
//!
//! We can then access the handle's value immutably using the [`Handle::r`] method and mutably using
//! the [`Handle::m`] method. These methods are short aliases for the more conventionally-named
//! [`Handle::get`] and [`Handle::get_mut`] methods respectively.
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
//! To pretty-print a handle's value using [`Debug`](std::fmt::Debug), you must first wrap the
//! target handle in a [`WorldDebug`] wrapper type using the [`Handle::debug`] method. This wrapper
//! calls into the handle's regular `Debug::fmt` method but passes the [`World`] through
//! thread-local storage so the printed value may be accessed.
//!
//! The debug-printing logic for all `Handle`s ensures that cyclic formatting is handled properly.
//! For example, printing the following structure will not cause the program to stack-overflow:
//!
//! ```
//! # use arid::World;
//! #
//! # let mut w = World::new();
//! # let w = &mut w;
//! use arid::{Handle as _, Object as _, object};
//!
//! #[derive(Debug)]
//! pub struct Cycle {
//!     other: Option<CycleHandle>,
//! }
//!
//! object!(pub Cycle);
//!
//! let foo = Cycle { other: None }.spawn(w);
//! let bar = Cycle { other: None }.spawn(w);
//!
//! foo.m(w).other = Some(*bar);
//! bar.m(w).other = Some(*foo);
//!
//! dbg!(foo.debug(w));
//! ```
//!
//! ```text
//! [debug.rs:16:5] foo.debug(w) = debug::main::Cycle[0, v1]: Cycle {
//!     other: Some(
//!         debug::main::Cycle[1, v1]: Cycle {
//!             other: Some(
//!                 debug::main::Cycle[0, v1],
//!             ),
//!         },
//!     ),
//! }
//! ```
//!
//! # Lifecycle
//!
//! Objects in `arid` are reference-counted although their semantics are a bit special:
//!
//! - The [`Handle`] newtypes generated by the `object!` macro are weak references but are assumed
//!   to be valid. As such, you can call `.r()` and `.w()` on them directly. These types are
//!   `Copy`able.
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
//! ```
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
//!         println!("{} has been destroyed!", self.r(w).name);
//!     }
//! }
//!
//! let object = DtorObserver { name: "Max" }.spawn(w);
//!
//! drop(object);
//! w.flush();  // "Max has been destroyed!"
//! ```
//!
//! # Polymorphism
//!
//! Handle newtypes are not limited to harboring inherent `impl` blocks. Indeed, they can also
//! accommodate `dyn`-compatible `trait` implementations, providing a powerful mechanism for
//! polymorphism.
//!
//! We'll begin by defining a new trait and making it inherit the [`ErasedHandle`] trait implemented
//! by all [`Handle`]s.
//!
//! ```
//! use arid::{ErasedHandle, W, Wr};
//!
//! pub trait AbstractCounter: ErasedHandle {
//!     fn increment(&self, w: W);
//!
//!     fn get_count(&self, w: Wr) -> usize;
//! }
//! ```
//!
//! We can then implement that trait on a handle newtype.
//!
//! ```
//! # use arid::{ErasedHandle, W, Wr};
//! #
//! # pub trait AbstractCounter: ErasedHandle {
//! #     fn increment(&self, w: W);
//! #
//! #     fn get_count(&self, w: Wr) -> usize;
//! # }
//! use std::time::Instant;
//!
//! use arid::{object, Handle as _};
//!
//! #[derive(Debug, Default)]
//! pub struct SimpleCounter {
//!     count: usize,
//! }
//!
//! object!(pub SimpleCounter);
//!
//! impl AbstractCounter for SimpleCounterHandle {
//!     fn increment(&self, w: W) {
//!         self.m(w).count += 1;
//!     }
//!
//!     fn get_count(&self, w: Wr) -> usize {
//!         self.r(w).count
//!     }
//! }
//!
//! #[derive(Debug, Default)]
//! pub struct ComplexCounter {
//!     records: Vec<Instant>,
//! }
//!
//! object!(pub ComplexCounter);
//!
//! impl AbstractCounter for ComplexCounterHandle {
//!     fn increment(&self, w: W) {
//!         self.m(w).records.push(Instant::now());
//!     }
//!
//!     fn get_count(&self, w: Wr) -> usize {
//!         self.r(w).records.len()
//!     }
//! }
//! ```
//!
//! While it is perfectly valid to store these `dyn Trait`-objects in a `Box`, it is likely much
//! more ergonomic and performant to store them in an [`Erased`] handle wrapper, which is `Copy`able
//! and comes with some useful helper methods for down-casting types. You can instantiate an
//! `Erased` wrapper using the [`erase!`] macro like so:
//!
//! ```
//! # use arid::{ErasedHandle, W, Wr};
//! #
//! # pub trait AbstractCounter: ErasedHandle {
//! #     fn increment(&self, w: W);
//! #
//! #     fn get_count(&self, w: Wr) -> usize;
//! # }
//! # use std::time::Instant;
//! #
//! # use arid::{object, Handle as _};
//! #
//! # #[derive(Debug, Default)]
//! # pub struct SimpleCounter {
//! #     count: usize,
//! # }
//! #
//! # object!(pub SimpleCounter);
//! #
//! # impl AbstractCounter for SimpleCounterHandle {
//! #     fn increment(&self, w: W) {
//! #         self.m(w).count += 1;
//! #     }
//! #
//! #     fn get_count(&self, w: Wr) -> usize {
//! #         self.r(w).count
//! #     }
//! # }
//! #
//! # #[derive(Debug, Default)]
//! # pub struct ComplexCounter {
//! #     records: Vec<Instant>,
//! # }
//! #
//! # object!(pub ComplexCounter);
//! #
//! # impl AbstractCounter for ComplexCounterHandle {
//! #     fn increment(&self, w: W) {
//! #         self.m(w).records.push(Instant::now());
//! #     }
//! #
//! #     fn get_count(&self, w: Wr) -> usize {
//! #         self.r(w).records.len()
//! #     }
//! # }
//! #
//! # use arid::World;
//! # let mut w = World::new();
//! # let w = &mut w;
//! use arid::{erase, Object as _};
//!
//! let simple_counter = SimpleCounter::default().spawn(w);
//! let complex_counter = ComplexCounter::default().spawn(w);
//!
//! let mut counter = erase!(as dyn AbstractCounter, *simple_counter);
//!
//! counter.increment(w);
//! assert_eq!(counter.get_count(w), 1);
//! dbg!(counter.debug(w));
//! assert!(counter.try_downcast::<SimpleCounterHandle>().is_some());
//!
//! counter = erase!(as dyn AbstractCounter, *complex_counter);
//! counter.increment(w);
//! assert_eq!(counter.get_count(w), 1);
//! dbg!(counter.debug(w));
//! assert!(counter.try_downcast::<ComplexCounterHandle>().is_some());
//! ```
//!
//! ```text
//! [poly.rs:59:5] counter.debug(w) = poly::main::SimpleCounter[0, v1]: SimpleCounter {
//!     count: 1,
//! }
//! [poly.rs:65:5] counter.debug(w) = poly::main::ComplexCounter[0, v1]: ComplexCounter {
//!     records: [
//!         Instant {
//!             tv_sec: 85959,
//!             tv_nsec: 910593500,
//!         },
//!     ],
//! }
//! ```
//!
//! [`Erased`] is a weak-but-assumed-valid handle to a value—sort of like a `Handle`. You can use
//! [`StrongErased`] to create a strong reference to the value.
//!
//! # Custom Arenas
//!
//! `arid` allows us to define custom arenas to track a given object's instances, which can be
//! useful when trying to attach additional metadata to a variety of objects (e.g. widget parent and
//! child relationships in a UI framework) or when customizing those objects' deletion routine.
//!
//! To do this, we must first define a new arena structure and put a bare [`Arena`] into it. This is
//! necessary since only `Arena`s can allocate the raw [`KeepAlive`] guards required to construct a
//! [`Strong`] handle.
//!
//! In addition to the [`ObjectArena`] trait we'll soon implement, this structure must also be
//! [`Sized`], implement [`Debug`](std::fmt::Debug) and [`Default`], and live for `'static`.
//!
//! ```
//! use arid::{Arena, Object};
//!
//! #[derive(Debug)]
//! pub struct MyArena<T: Object> {
//!     arena: Arena<Slot<T>>,
//! }
//!
//! impl<T: Object> Default for MyArena<T> {
//!     fn default() -> Self {
//!         Self {
//!             arena: Arena::default(),
//!         }
//!     }
//! }
//!
//! #[derive(Debug)]
//! struct Slot<T: Object> {
//!     value: T,
//!     frobs: u32,
//! }
//! ```
//!
//! Now, let us implement the [`ObjectArena`] trait on the `MyArena` type for objects we wish to
//! support. This trait provides the [`ObjectArena::arena`] and [`ObjectArena::arena_mut`]
//! methods to gain access to the underlying arena instance for a given world and we're expected to
//! define how operations such as spawning, deleting, and accessing elements should work. Here's
//! some boilerplate for a minimal arena with very little customization:
//!
//! ```
//! # use arid::{Arena, Object};
//! #
//! # #[derive(Debug)]
//! # pub struct MyArena<T: Object> {
//! #     arena: Arena<Slot<T>>,
//! # }
//! #
//! # impl<T: Object> Default for MyArena<T> {
//! #     fn default() -> Self {
//! #         Self {
//! #             arena: Arena::default(),
//! #         }
//! #     }
//! # }
//! #
//! # #[derive(Debug)]
//! # struct Slot<T: Object> {
//! #     value: T,
//! #     frobs: u32,
//! # }
//! use std::fmt;
//!
//! use arid::{Handle, ObjectArena, ObjectArenaSimpleSpawn, Strong, W, Wr};
//!
//! impl<T: Object<Arena = Self>> ObjectArenaSimpleSpawn for MyArena<T>
//! where
//!     T: Object<Arena = Self> + fmt::Debug,
//! {
//!     fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle> {
//!         let (arena, manager) = Self::arena_and_manager_mut(w);
//!
//!         let (handle, keep_alive) = arena.arena.insert(manager, Self::despawn, Slot {
//!             value,
//!             frobs: 0,
//!         });
//!
//!         Strong::new(Self::Handle::from_raw(handle), keep_alive)
//!     }
//! }
//!
//! impl<T> ObjectArena for MyArena<T>
//! where
//!     T: Object<Arena = Self> + fmt::Debug,
//! {
//!     type Object = T;
//!     type Handle = T::Handle;
//!
//!     fn despawn(slot_idx: u32, w: W) {
//!         let handle = Self::try_from_slot(slot_idx, w).unwrap();
//!         <T::Handle>::invoke_pre_destructor(handle, w);
//!
//!         Self::arena_mut(w).arena.remove_now(slot_idx);
//!     }
//!
//!     fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
//!         Self::arena(w).arena.get(handle.raw()).map(|v| &v.value)
//!     }
//!
//!     fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
//!         Self::arena_mut(w).arena.get_mut(handle.raw()).map(|v| &mut v.value)
//!     }
//!
//!     fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>> {
//!         Self::arena(w)
//!             .arena
//!             .upgrade(Self::manager(w), handle.raw())
//!             .map(|keep_alive| Strong::new(handle, keep_alive))
//!     }
//!
//!     fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle> {
//!         Self::arena(w)
//!             .arena
//!             .slot_to_handle(slot_idx)
//!             .map(T::Handle::from_raw)
//!     }
//!
//!     fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
//!         if let Some(alive) = handle.try_get(w) {
//!             alive.fmt(f)
//!         } else {
//!             f.write_str("<dangling>")
//!         }
//!     }
//! }
//! ```
//!
//! Finally, we can create extension traits to define new methods on objects within our new arena.
//!
//! ```
//! # use arid::{Arena, Object};
//! #
//! # #[derive(Debug)]
//! # pub struct MyArena<T: Object> {
//! #     arena: Arena<Slot<T>>,
//! # }
//! #
//! # impl<T: Object> Default for MyArena<T> {
//! #     fn default() -> Self {
//! #         Self {
//! #             arena: Arena::default(),
//! #         }
//! #     }
//! # }
//! #
//! # #[derive(Debug)]
//! # struct Slot<T: Object> {
//! #     value: T,
//! #     frobs: u32,
//! # }
//! # use std::fmt;
//! #
//! # use arid::{Handle, ObjectArena, ObjectArenaSimpleSpawn, Strong, W, Wr};
//! #
//! # impl<T: Object<Arena = Self>> ObjectArenaSimpleSpawn for MyArena<T>
//! # where
//! #     T: Object<Arena = Self> + fmt::Debug,
//! # {
//! #     fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle> {
//! #         let (arena, manager) = Self::arena_and_manager_mut(w);
//! #
//! #         let (handle, keep_alive) = arena.arena.insert(manager, Self::despawn, Slot {
//! #             value,
//! #             frobs: 0,
//! #         });
//! #
//! #         Strong::new(Self::Handle::from_raw(handle), keep_alive)
//! #     }
//! # }
//! #
//! # impl<T> ObjectArena for MyArena<T>
//! # where
//! #     T: Object<Arena = Self> + fmt::Debug,
//! # {
//! #     type Object = T;
//! #     type Handle = T::Handle;
//! #
//! #     fn despawn(slot_idx: u32, w: W) {
//! #         let handle = Self::try_from_slot(slot_idx, w).unwrap();
//! #         <T::Handle>::invoke_pre_destructor(handle, w);
//! #
//! #         Self::arena_mut(w).arena.remove_now(slot_idx);
//! #     }
//! #
//! #     fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
//! #         Self::arena(w).arena.get(handle.raw()).map(|v| &v.value)
//! #     }
//! #
//! #     fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
//! #         Self::arena_mut(w).arena.get_mut(handle.raw()).map(|v| &mut v.value)
//! #     }
//! #
//! #     fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>> {
//! #         Self::arena(w)
//! #             .arena
//! #             .upgrade(Self::manager(w), handle.raw())
//! #             .map(|keep_alive| Strong::new(handle, keep_alive))
//! #     }
//! #
//! #     fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle> {
//! #         Self::arena(w)
//! #             .arena
//! #             .slot_to_handle(slot_idx)
//! #             .map(T::Handle::from_raw)
//! #     }
//! #
//! #     fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
//! #         if let Some(alive) = handle.try_get(w) {
//! #             alive.fmt(f)
//! #         } else {
//! #             f.write_str("<dangling>")
//! #         }
//! #     }
//! # }
//! pub trait MyObject: Object<Arena = MyArena<Self>> + fmt::Debug {}
//!
//! impl<T: Object<Arena = MyArena<Self>> + fmt::Debug> MyObject for T {}
//!
//! pub trait MyHandle: Handle<Object: MyObject> {
//!     fn frob(self, w: W);
//!
//!     fn get_frobs(self, w: Wr) -> u32;
//! }
//!
//! impl<T: Handle<Object: MyObject>> MyHandle for T {
//!     fn frob(self, w: W) {
//!         MyArena::<T::Object>::arena_mut(w)
//!             .arena
//!             .get_mut(self.raw())
//!             .expect("value is not alive")
//!             .frobs += 1;
//!     }
//!
//!     fn get_frobs(self, w: Wr) -> u32 {
//!         MyArena::<T::Object>::arena(w)
//!             .arena
//!             .get(self.raw())
//!             .expect("value is not alive")
//!             .frobs
//!     }
//! }
//! ```
//!
//! To define an object within the arena, we can use a second form of the [`object!`] macro to
//! specify a custom arena in which the object should be stored:
//!
//! ```
//! # use arid::{Arena, Object};
//! #
//! # #[derive(Debug)]
//! # pub struct MyArena<T: Object> {
//! #     arena: Arena<Slot<T>>,
//! # }
//! #
//! # impl<T: Object> Default for MyArena<T> {
//! #     fn default() -> Self {
//! #         Self {
//! #             arena: Arena::default(),
//! #         }
//! #     }
//! # }
//! #
//! # #[derive(Debug)]
//! # struct Slot<T: Object> {
//! #     value: T,
//! #     frobs: u32,
//! # }
//! # use std::fmt;
//! #
//! # use arid::{Handle, ObjectArena, ObjectArenaSimpleSpawn, Strong, W, Wr};
//! #
//! # impl<T: Object<Arena = Self>> ObjectArenaSimpleSpawn for MyArena<T>
//! # where
//! #     T: Object<Arena = Self> + fmt::Debug,
//! # {
//! #     fn spawn(value: Self::Object, w: W) -> Strong<Self::Handle> {
//! #         let (arena, manager) = Self::arena_and_manager_mut(w);
//! #
//! #         let (handle, keep_alive) = arena.arena.insert(manager, Self::despawn, Slot {
//! #             value,
//! #             frobs: 0,
//! #         });
//! #
//! #         Strong::new(Self::Handle::from_raw(handle), keep_alive)
//! #     }
//! # }
//! #
//! # impl<T> ObjectArena for MyArena<T>
//! # where
//! #     T: Object<Arena = Self> + fmt::Debug,
//! # {
//! #     type Object = T;
//! #     type Handle = T::Handle;
//! #
//! #     fn despawn(slot_idx: u32, w: W) {
//! #         let handle = Self::try_from_slot(slot_idx, w).unwrap();
//! #         <T::Handle>::invoke_pre_destructor(handle, w);
//! #
//! #         Self::arena_mut(w).arena.remove_now(slot_idx);
//! #     }
//! #
//! #     fn try_get(handle: Self::Handle, w: Wr<'_>) -> Option<&Self::Object> {
//! #         Self::arena(w).arena.get(handle.raw()).map(|v| &v.value)
//! #     }
//! #
//! #     fn try_get_mut(handle: Self::Handle, w: W<'_>) -> Option<&mut Self::Object> {
//! #         Self::arena_mut(w).arena.get_mut(handle.raw()).map(|v| &mut v.value)
//! #     }
//! #
//! #     fn as_strong_if_alive(handle: Self::Handle, w: Wr) -> Option<Strong<Self::Handle>> {
//! #         Self::arena(w)
//! #             .arena
//! #             .upgrade(Self::manager(w), handle.raw())
//! #             .map(|keep_alive| Strong::new(handle, keep_alive))
//! #     }
//! #
//! #     fn try_from_slot(slot_idx: u32, w: Wr) -> Option<Self::Handle> {
//! #         Self::arena(w)
//! #             .arena
//! #             .slot_to_handle(slot_idx)
//! #             .map(T::Handle::from_raw)
//! #     }
//! #
//! #     fn print_debug(f: &mut fmt::Formatter<'_>, handle: Self::Handle, w: Wr) -> fmt::Result {
//! #         if let Some(alive) = handle.try_get(w) {
//! #             alive.fmt(f)
//! #         } else {
//! #             f.write_str("<dangling>")
//! #         }
//! #     }
//! # }
//! # pub trait MyObject: Object<Arena = MyArena<Self>> + fmt::Debug {}
//! #
//! # impl<T: Object<Arena = MyArena<Self>> + fmt::Debug> MyObject for T {}
//! #
//! # pub trait MyHandle: Handle<Object: MyObject> {
//! #     fn frob(self, w: W);
//! #
//! #     fn get_frobs(self, w: Wr) -> u32;
//! # }
//! #
//! # impl<T: Handle<Object: MyObject>> MyHandle for T {
//! #     fn frob(self, w: W) {
//! #         MyArena::<T::Object>::arena_mut(w)
//! #             .arena
//! #             .get_mut(self.raw())
//! #             .expect("value is not alive")
//! #             .frobs += 1;
//! #     }
//! #
//! #     fn get_frobs(self, w: Wr) -> u32 {
//! #         MyArena::<T::Object>::arena(w)
//! #             .arena
//! #             .get(self.raw())
//! #             .expect("value is not alive")
//! #             .frobs
//! #     }
//! # }
//! # use arid::World;
//! # let mut w = World::new();
//! # let w = &mut w;
//! use arid::{object, Object as _};
//!
//! #[derive(Debug)]
//! pub struct MyThing {
//!     name: &'static str,
//! }
//!
//! object!(pub MyThing[MyArena<Self>]);
//!
//! let my_thing = MyThing { name: "Ryleigh" }.spawn(w);
//!
//! my_thing.frob(w);
//! my_thing.m(w).name = "Riley";
//! assert_eq!(my_thing.get_frobs(w), 1);
//! ```
//!
//! Happy hacking!
//!
//! # Limitations and Future Work
//!
//! `arid`'s largest limitation is its lack of support for generic [`Object`] definitions. This
//! limitation originates from our use of [`late_struct`] to build up our [`World`]s: each object
//! declaration defines a new field in our world and these late-bound field definitions cannot be
//! generic.
//!
//! Our use of `late_struct` also means that the size of each world is proportional to the number of
//! objects defined in the binary but the constant factor on this size is, intentionally, fairly
//! small (currently, 32 bytes per arena type).
//!
//! The use of arenas to implement this object model is also somewhat unfortunate. Arenas transform
//! the (small) cost of incrementing and decrementing reference counts into the (equally small) cost
//! of checking object generations before accessing a handle. This is a somewhat suspicious tradeoff
//! since I'd expect dereferences to happen more often than reference copies but that's just a
//! hunch.
//!
//! Really, arenas are just a work-around to give us `Copy`able handles for ergonomic purposes.
//! Hopefully, this need for arenas will be obviated by the [ergonomic ref-counting] team's efforts.
//! Likewise, the handle newtype system is really just a work-around for the lack of [arbitrary
//! `Self` types](https://github.com/rust-lang/rust/issues/44874).
//!
//! [ergonomic ref-counting]: https://rust-lang.github.io/rust-project-goals/2024h2/ergonomic-rc.html
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
