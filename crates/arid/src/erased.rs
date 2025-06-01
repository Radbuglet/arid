use std::fmt;

use thunderdome::Index;

use crate::entity::Handle;

pub trait ErasedHandle: 'static + fmt::Debug {
    fn raw(&self) -> Index;
}

impl<H: Handle> ErasedHandle for H {
    fn raw(&self) -> Index {
        (*self).raw()
    }
}
