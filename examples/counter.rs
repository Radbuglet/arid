use boao::{Component, Handle, Strong, component};
use late_struct::{LateInstance, late_struct};

#[non_exhaustive]
pub struct WorldNs;

late_struct!(WorldNs);

pub type World = LateInstance<WorldNs>;

pub type W<'a> = &'a mut World;
pub type Wr<'a> = &'a World;

#[derive(Debug)]
pub struct MyNode {
    counter: u32,
}

component!(WorldNs => MyNode);

impl MyNodeHandle {
    pub fn new(w: W) -> Strong<Self> {
        MyNode { counter: 1 }.spawn(w)
    }

    pub fn counter(self, w: Wr) -> u32 {
        self.r(w).counter
    }

    pub fn increment_counter(self, w: W) {
        self.m(w).counter += 1;
    }
}

fn main() {}
