use arid::{ErasedHandle, Handle, Object, W, World};
use arid_entity::{EntityHandle, component};

fn main() {
    let mut w = World::new();
    let w = &mut w;

    let whee = EntityHandle::new(w)
        .with_label("whee", w)
        .with(Collider { whee: 0 }.spawn(w), w)
        .with_child(
            EntityHandle::new(w)
                .with_label("woo", w)
                .with(Collider { whee: 2 }.spawn(w), w),
            w,
        )
        .with_child(
            EntityHandle::new(w)
                .with_label("waz", w)
                .with(Collider { whee: 4 }.spawn(w), w),
            w,
        );

    let whee = whee.as_strong(w);

    w.flush();
    dbg!(whee.debug(w));

    let child = *whee.children(w)[0];

    eprintln!("{:?}", whee.debug(w));

    whee.traits::<dyn TransformListener>(w)
        .unique(w)
        .invalidate(w);

    for tra in whee.traits::<dyn TransformListener>(w) {
        tra.get(w).invalidate(w);
    }

    dbg!(whee.debug(w));

    drop(whee);
    dbg!(child.debug(w));
    w.flush();
    dbg!(child.debug(w));
}

pub trait TransformListener: ErasedHandle {
    fn invalidate(&self, w: W);
}

#[derive(Debug)]
pub struct Collider {
    whee: u32,
}

component!(Collider[dyn TransformListener]);

impl TransformListener for ColliderHandle {
    fn invalidate(&self, w: W) {
        self.m(w).whee = 42;
    }
}
