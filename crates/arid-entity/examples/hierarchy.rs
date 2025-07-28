use arid::{ErasedHandle, Handle, Object, W, World, object};
use arid_entity::{ComponentArena, EntityHandle};

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

object!(Collider[ComponentArena<Self>]);

impl TransformListener for ColliderHandle {
    fn invalidate(&self, w: W) {
        self.m(w).whee = 0xBAD;
    }
}
