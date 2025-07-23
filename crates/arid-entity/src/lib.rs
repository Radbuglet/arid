#![allow(clippy::missing_safety_doc)]

pub mod archetype;
pub mod entity;
pub mod erased;
pub mod query;
pub mod world;

mod utils;

pub mod prelude {
    pub use crate::{
        entity::{Component, Entity, Handle, component},
        erased::{Erased, ErasedHandle, erased},
        query::{Query, query_removed},
        world::{W, World, WorldDebug, Wr},
    };
}

pub use self::prelude::*;
