//! Late-bound structure definitions.
//!
//! This crate exposes the [`late_struct`] macro, which defines a structure whose set of fields
//! can be extended by any crate within a compiled artifact using the [`late_field`] macro.
//! Unlike regular structures, dependents on the crate which originally defined the structure are
//! allowed to extend it. Additionally, the structure we defined can be instantiated in any crate
//! using a [`LateInstance`], even if dependents of that crate are still extending it.
//!
//! ## Basic Usage
//!
//! For example, let's say we had a crate hierarchy where "`dependent` depends on `dependency`."
//!
//! In `dependency`, we could define a new late-struct marker using the [`late_struct!`] macro...
//!
//! ```
//! use late_struct::late_struct;
//!
//! // Marker type for our application context.
//! // Any type could be used here.
//! pub struct AppContext;
//!
//! late_struct!(AppContext);
//! ```
//!
//! ...and then, in `dependent`, we could proceed to add a field to it using the [`late_field!`]
//! macro:
//!
//! ```
//! # mod dependency {
//! #     use late_struct::late_struct;
//! #     pub struct AppContext;
//! #     late_struct!(AppContext);
//! # }
//! use late_struct::late_field;
//!
//! use dependency::AppContext;
//!
//! #[derive(Debug, Default)]  // Type must implement `Debug`, `Default`, and live for `'static`.
//! pub struct MyField(Vec<u32>);
//!
//! late_field!(MyField[AppContext]);
//! ```
//!
//! ...just note that, by default, the field value must implement [`Debug`], [`Default`], and live for
//! `'static`.
//!
//! We can then refer to the structure we've created with a [`LateInstance`]. For example, back in
//! `dependency`, we can write...
//!
//! ```
//! # use late_struct::late_struct;
//! # pub struct AppContext;
//! # late_struct!(AppContext);
//! use late_struct::LateInstance;
//!
//! pub fn create_my_instance() -> LateInstance<AppContext> {
//!     LateInstance::new()
//! }
//! ```
//!
//! ...even though downstream crates such as `dependent` are still adding fields to it. Finally, we
//! can access fields using the [`LateInstance::get`] and [`LateInstance::get_mut`] methods. For
//! example, in the `dependent` crate, we could write...
//!
//! ```
//! # mod dependency {
//! #     use late_struct::{late_struct, LateInstance};
//! #
//! #     pub struct AppContext;
//! #
//! #     late_struct!(AppContext);
//! #
//! #     pub fn create_my_instance() -> LateInstance<AppContext> {
//! #         LateInstance::new()
//! #     }
//! # }
//! use dependency::{AppContext, create_my_instance};
//! # use late_struct::late_field;
//! #
//! # #[derive(Debug, Default)]  // Type must implement `Default` and live for `'static`.
//! # pub struct MyField(Vec<u32>);
//! #
//! # late_field!(MyField[AppContext]);
//!
//! pub fn example() {
//!     let mut instance = create_my_instance();
//!
//!     instance.get_mut::<MyField>().0.push(1);
//!     instance.get_mut::<MyField>().0.push(2);
//!     instance.get_mut::<MyField>().0.push(3);
//!
//!     eprintln!("Our numbers are {:?}", instance.get::<MyField>());
//! }
//! ```
//!
//! See the documentation of [`LateInstance`] for more ways to access the instance.
//!
//! Note that the "key type" used to refer to a given field can be distinct from its value type. For
//! example, in the previous snippet, we could make `MyField` a zero-sized marker type and set it up
//! to refer to a value of type `Vec<u32>` instead. We do this by changing our `late_field!` macro
//! invocation like so...
//!
//! ```
//! # mod dependency {
//! #     use late_struct::{late_struct, LateInstance};
//! #
//! #     pub struct AppContext;
//! #
//! #     late_struct!(AppContext);
//! #
//! #     pub fn create_my_instance() -> LateInstance<AppContext> {
//! #         LateInstance::new()
//! #     }
//! # }
//! use dependency::{AppContext, create_my_instance};
//! # use late_struct::late_field;
//! #
//!
//! // The `#[non_exhaustive]` attribute helps ensure that other crates don't
//! // accidentally try to instantiate what should just be a marker type.
//! #[non_exhaustive]
//! pub struct MyField;
//!
//! late_field!(MyField[AppContext] => Vec<u32>);
//! //                              ^^^^^^^^^^^ this is how we specify the
//! //                                          field's value type explicitly.
//!
//! pub fn example() {
//!     let mut instance = create_my_instance();
//!
//!     // Notice that we're now accessing the `&mut Vec<u32>` directly
//!     // rather than the `MyField` wrapper.
//!     instance.get_mut::<MyField>().push(1);
//!     instance.get_mut::<MyField>().push(2);
//!     instance.get_mut::<MyField>().push(3);
//!
//!     eprintln!("Our numbers are {:?}", instance.get::<MyField>());
//! }
//! ```
//!
//! ## Advanced Usage
//!
//! By default, all fields of a given struct are required to implement [`Debug`], [`Default`], and
//! `'static`. These requirements, however, can be changed on a per-struct basis. For instance, we
//! can remove the `Debug` requirement and instead require [`Send`], [`Sync`], and a custom trait
//! `Reflect` with the following [`late_struct!`] definition...
//!
//! ```
//! use late_struct::late_struct;
//!
//! trait Reflect {
//!     fn say_hi(&self);
//! }
//!
//! struct MyStruct;
//!
//! late_struct!(MyStruct => dyn 'static + Reflect + Send + Sync);
//! ```
//!
//! The only mandatory requirements of a field are that it have a [`Default`] initializer and live
//! for `'static`.
//!
//! We can access the erased forms of these fields using the [`LateInstance::fields`],
//! [`LateInstance::get_erased`], and [`LateInstance::get_erased_mut`] methods like so...
//!
//! ```
//! # use late_struct::{late_field, late_struct, LateInstance};
//! # use std::sync::Arc;
//! #
//! # trait Reflect {
//! #     fn say_hi(&self);
//! # }
//! #
//! # struct MyStruct;
//! #
//! # late_struct!(MyStruct => dyn 'static + Reflect + Send + Sync);
//! #
//! #[derive(Default)]
//! struct MyField;
//!
//! impl Reflect for MyField {
//!    fn say_hi(&self) {
//!        println!("Hello!");
//!    }
//! }
//!
//! late_field!(MyField[MyStruct]);
//!
//! fn say_greetings_on_a_thread(instance: Arc<LateInstance<MyStruct>>) {
//!     std::thread::spawn(move || {
//!         for field in instance.fields() {
//!              instance.get_erased(field).say_hi();
//!         }
//!     })
//!     .join()
//!     .unwrap()
//! }
//! #
//! # say_greetings_on_a_thread(Arc::new(LateInstance::new()));
//! ```
//!
//! Struct members can also be made to satisfy non-dyn-compatible standard traits such as [`Eq`],
//! [`Hash`], and [`Clone`] by making the members implement the [`DynEq`], [`DynHash`], and
//! [`DynClone`] traits respectively. This lets us write, for instance...
//!
//! ```
//! use std::{fmt::Debug, collections::HashSet};
//! use late_struct::{late_field, late_struct, DynEq, DynHash, DynClone, LateInstance};
//!
//! trait MyStructMember: Debug + DynEq + DynHash + DynClone {}
//!
//! impl<T> MyStructMember for T
//! where
//!     T: Debug + DynEq + DynHash + DynClone,
//! {
//! }
//!
//! struct MyStruct;
//!
//! late_struct!(MyStruct => dyn MyStructMember);
//!
//! #[derive(Debug, Clone, Hash, Eq, PartialEq, Default)]
//! struct MyField(u32);
//!
//! late_field!(MyField[MyStruct]);
//!
//! fn demo() {
//!     // The struct implements `Default`...
//!     let my_instance = LateInstance::<MyStruct>::default();
//!
//!     // ...debug...
//!     eprintln!("{my_instance:?}");
//!
//!     // ...clone...
//!     let my_instance_2 = my_instance.clone();
//!
//!     // ...eq...
//!     assert_eq!(my_instance, my_instance_2);
//!
//!     // ...and hash!
//!     let mut map = HashSet::new();
//!
//!     assert!(map.insert(my_instance));
//!     assert!(!map.insert(my_instance_2));
//! }
//! # demo();
//! ```
//!
//! ## Internals
//!
//! Internally, each field we define with [`late_field!`] creates a `static` containing a
//! [`LateFieldDescriptor`] and uses [`linkme`] (or `inventory` on WebAssembly) to add it to a
//! global list of all fields in the crate. When our first [`LateInstance`] is instantiated, all
//! these `LateFieldDescriptor`s are collected and laid out into a structure at runtime, with each
//! fields' offset being written back into an `AtomicUsize` in the `LateFieldDescriptor`.
//!
//! From there, structure instantiation and field fetching work more-or-less like they would with a
//! regular structure. `LateInstance` creates one big heap allocation for the structure it
//! represents and initializes each field accordingly. To access a field, all we have to do is
//! offset the structure's base pointer by the dynamically-initialized offset stored in the field's
//! `LateFieldDescriptor`, making field accesses extremely cheap.
//!
//! Many of these internals are exposed to the end user. See [`LateStructDescriptor`] and
//! [`LateFieldDescriptor`] (which you can obtain from the [`LateStruct::descriptor`] and
//! [`LateField::descriptor`] methods respectively) to learn about various options for reflecting
//! upon the layout of a structure.

mod descriptor;
pub use self::descriptor::*;

mod init;
pub use self::init::*;

mod instance;
pub use self::instance::*;

mod std_ops;
pub use std_ops::*;

mod traits;
pub use self::traits::*;
