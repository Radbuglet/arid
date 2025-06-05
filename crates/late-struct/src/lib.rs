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
//! In `dependency`, we could define a new late-struct marker...
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
//! ...and then, in `dependent`, we could proceed to add a field to it like so:
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
//! Just note that the field value must implement [`Debug`], [`Default`], and live for `'static`.
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
//! ## Advanced Usage
//!
//! By default, all fields of a given struct are required to implement [`Debug`], [`Default`], and
//! `'static`. These requirements, however, can be changed on a per-struct basis. For instance, we
//! can remove the [`Debug`] requirement and instead require [`Send`], [`Sync`], and a custom trait
//! `Reflect` with the following `late_struct` definition...
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
//! TODO: Document `DynEq` and `DynClone`.
//!
//! Finally, we should note that field values can take on a different type than their "key" type.
//!
//! TODO
//!
//! ## Internals and Performance
//!
//! Conceptually, a `LateInstance` could be thought of as a `HashMap<TypeId, Box<dyn Any>>`. In
//! practice, however, TODO
//!

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
