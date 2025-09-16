use arid::{Handle, World};

use crate::{Component, EntityHandle, component};

#[derive(Debug)]
struct Pos(u32);

component!(Pos);

#[derive(Debug)]
struct Vel(u32);

component!(Vel);

#[test]
fn one_component() {
    let mut w = World::new();
    let w = &mut w;

    let (owner, owner_ref) = EntityHandle::new(None, w).split();
    let (pos, pos_ref) = Pos(4).attach(owner.as_weak(), w).split();

    assert_eq!(owner.get::<PosHandle>(w), pos_ref);
    assert_eq!(pos.r(w).0, 4);

    w.flush();

    assert!(owner.is_alive(w));
    assert!(pos.is_alive(w));
    assert_eq!(owner.get::<PosHandle>(w), pos_ref);

    drop(pos);

    w.flush();

    assert!(owner.is_alive(w));
    assert!(pos_ref.is_alive(w));
    assert_eq!(owner.get::<PosHandle>(w), pos_ref);

    drop(owner);

    w.flush();

    assert!(!owner_ref.is_alive(w));
    assert!(!pos_ref.is_alive(w));
    assert!(owner_ref.try_get::<PosHandle>(w).is_none());
}

#[test]
fn two_components() {
    let mut w = World::new();
    let w = &mut w;

    let (owner, owner_ref) = EntityHandle::new(None, w).split();
    let (vel, vel_ref) = Vel(5).attach(owner.as_weak(), w).split();
    let (pos, pos_ref) = Pos(4).attach(owner.as_weak(), w).split();

    assert_eq!(owner.get::<PosHandle>(w), pos_ref);
    assert_eq!(pos.r(w).0, 4);

    assert_eq!(owner.get::<VelHandle>(w), vel_ref);
    assert_eq!(vel.r(w).0, 5);

    w.flush();

    assert!(owner.is_alive(w));

    assert!(pos.is_alive(w));
    assert_eq!(owner.get::<PosHandle>(w), pos_ref);

    assert!(vel.is_alive(w));
    assert_eq!(owner.get::<VelHandle>(w), vel_ref);

    drop(pos);

    w.flush();

    assert!(owner.is_alive(w));

    assert!(pos_ref.is_alive(w));
    assert_eq!(owner.get::<PosHandle>(w), pos_ref);

    assert!(vel_ref.is_alive(w));
    assert_eq!(owner.get::<VelHandle>(w), vel_ref);

    drop(owner);

    w.flush();

    assert!(owner_ref.is_alive(w));

    assert!(pos_ref.is_alive(w));
    assert_eq!(owner_ref.get::<PosHandle>(w), pos_ref);

    assert!(vel_ref.is_alive(w));
    assert_eq!(owner_ref.get::<VelHandle>(w), vel_ref);

    drop(vel);

    w.flush();

    assert!(!owner_ref.is_alive(w));
    assert!(!pos_ref.is_alive(w));
    assert!(!vel_ref.is_alive(w));
    assert!(owner_ref.try_get::<PosHandle>(w).is_none());
    assert!(owner_ref.try_get::<VelHandle>(w).is_none());
}
