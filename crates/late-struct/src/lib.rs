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
//! TODO
//!
//! ## Limitations
//!
//! TODO
//!

mod descriptor;
pub use self::descriptor::*;

mod init;
pub use self::init::*;

mod instance;
pub use self::instance::*;

mod traits;
pub use self::traits::*;
