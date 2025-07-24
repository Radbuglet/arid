use arid::{Handle, Object, World, object};
use arid_entity::entity::{EntityHandle, NodeArena, NodeHandle};

#[derive(Debug)]
pub struct Pos(pub [f32; 2]);

object!(Pos[NodeArena<Self>]);

fn main() {
    let mut w = World::new();
    let w = &mut w;

    let entity = EntityHandle::new(w);
    entity.add(Pos([1.; 2]).spawn(w), w);

    dbg!(entity.get::<PosHandle>(w).debug(w));
    entity.get::<PosHandle>(w).detach_from_entity(w);
    dbg!(entity.try_get::<PosHandle>(w));
}
