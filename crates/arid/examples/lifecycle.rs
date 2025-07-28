use std::mem;

use arid::{Destructor, Handle, Object, W, World, object};

fn main() {
    let mut w = World::new();
    let w = &mut w;

    // Example 1
    let registry = ColliderRegistry::default().spawn(w);
    let collider = Collider::default().spawn(w);

    collider.attach(*registry, w);
    dbg!(registry.debug(w));

    w.flush();

    dbg!(registry.debug(w));
    drop(collider);

    w.flush();

    dbg!(registry.debug(w));

    // Example 2
    let registry = ColliderRegistry::default().spawn(w);
    let collider = Collider::default().spawn(w);

    collider.attach(*registry, w);
    dbg!(registry.debug(w));

    w.flush();

    dbg!(registry.debug(w));
    drop(registry);

    w.flush();

    dbg!(collider.debug(w));
}

#[derive(Debug, Default)]
pub struct ColliderRegistry {
    colliders: Vec<ColliderHandle>,
}

object!(pub ColliderRegistry);

impl Destructor for ColliderRegistryHandle {
    fn pre_destroy(self, w: W) {
        for collider in mem::take(&mut self.m(w).colliders) {
            collider.m(w).registry = None;
        }
    }
}

#[derive(Debug, Default)]
pub struct Collider {
    registry: Option<ColliderRegistryHandle>,
    index_in_registry: usize,
}

object!(pub Collider);

impl ColliderHandle {
    pub fn attach(self, registry: ColliderRegistryHandle, w: W) {
        self.detach(w);

        self.m(w).registry = Some(registry);
        self.m(w).index_in_registry = registry.r(w).colliders.len();
        registry.m(w).colliders.push(self);
    }

    fn detach(self, w: W) {
        let Some(registry) = self.m(w).registry.take() else {
            return;
        };

        let index = self.r(w).index_in_registry;

        registry.m(w).colliders.swap_remove(index);

        if let Some(moved) = registry.r(w).colliders.get(index) {
            moved.m(w).index_in_registry = index;
        }
    }
}

impl Destructor for ColliderHandle {
    fn pre_destroy(self, w: W) {
        self.detach(w);
    }
}
