use arid::{Handle, Object, World, object};
use arid_entity::{ComponentArena, ComponentHandle, EntityHandle};

#[derive(Debug)]
pub struct Pos(pub [f32; 2]);

object!(Pos[ComponentArena<Self>]);

fn main() {
    let mut w = World::new();
    let w = &mut w;

    let entity = EntityHandle::new(w);
    dbg!(entity.debug(w));
    entity.add(Pos([1.; 2]).spawn(w), w);
    dbg!(entity.debug(w));

    let pos = entity.get::<PosHandle>(w);

    dbg!(entity.get::<PosHandle>(w).debug(w));
    entity.get::<PosHandle>(w).detach(w);

    let pos2 = pos.as_strong(w);
    dbg!(pos.debug(w));
    w.flush();
    dbg!(pos.debug(w));
    dbg!(entity.try_get::<PosHandle>(w));

    entity.add(pos2, w);

    dbg!(entity.debug(w));

    drop(entity);
    dbg!(pos.debug(w));
    w.flush();
    dbg!(pos.debug(w));
}
