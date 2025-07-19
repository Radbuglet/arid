use boao::{
    ArenaManager, Component, ErasedHandle, Handle, Meta, Strong, WorldFlushExt as _, component,
    erase_strong,
};
use late_struct::{LateInstance, late_field, late_struct};

#[non_exhaustive]
pub struct WorldNs;

late_struct!(WorldNs);

late_field!(ArenaManager<LateInstance<WorldNs>>[WorldNs]);

pub type World = LateInstance<WorldNs>;

pub type W<'a> = &'a mut World;
pub type Wr<'a> = &'a World;

#[derive(Debug)]
pub struct MyNode {
    counter: u32,
    my_list: Vec<u32>,
    chain: Option<MyNodeHandle>,
}

component!(MyNode[u32] in WorldNs);

impl Meta<MyNodeHandle> for u32 {
    type ArenaAnnotation = ();

    fn destroy(handle: MyNodeHandle, w: &mut LateInstance<<MyNodeHandle as Handle>::Struct>) {
        w.get_mut::<MyNode>().arena.remove_now(handle.raw_handle());
    }
}

impl MyNodeHandle {
    pub fn new(w: W) -> Strong<Self> {
        MyNode {
            counter: 1,
            my_list: Vec::new(),
            chain: None,
        }
        .spawn(w)
    }

    pub fn counter(self, w: Wr) -> u32 {
        self.r(w).counter
    }

    pub fn increment_counter(self, w: W) {
        self.m(w).counter += 1;
    }

    pub fn record(self, w: W) {
        let count = self.counter(w);

        self.m(w).my_list.push(count);
    }
}

pub trait AbstractCounter: ErasedHandle<Struct = WorldNs> {
    fn count(&self, w: W);
}

impl AbstractCounter for MyNodeHandle {
    fn count(&self, w: W) {
        self.increment_counter(w);
    }
}

fn main() {
    let mut w = World::new();
    let w = &mut w;

    let node = MyNodeHandle::new(w);

    let other = MyNodeHandle::new(w);
    other.m(w).chain = Some(*other);
    node.m(w).chain = Some(*other);

    *other.meta_mut(w) += 1;
    dbg!(node.meta(w));
    dbg!(other.meta(w));

    let node = erase_strong!(as dyn AbstractCounter, node);

    node.count(w);
    node.count(w);

    let node_ref = node.downcast_ref::<MyNodeHandle>();

    dbg!(node_ref.counter(w));
    dbg!(node_ref);
    dbg!(node_ref.debug(w));
    dbg!(node_ref.debug(w));

    let node = node.downcast::<MyNodeHandle>();

    dbg!(node.counter(w));

    drop(node);

    w.flush();

    dbg!(node_ref.debug(w));
    dbg!(other.debug(w));
}
