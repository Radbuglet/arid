use boao::{Arena, ArenaManager, Component, Handle, World, component};
use criterion::{Criterion, criterion_group, criterion_main};

#[derive(Debug)]
pub struct Counter(u32);

component!(Counter);

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("repeated_addition/direct_access", |b| {
        let mut counter = 0u32;

        b.iter(|| {
            counter += 1;
        });
    });

    c.bench_function("repeated_addition/local_deref_single", |b| {
        let mut arena_mgr = ArenaManager::<()>::new();
        let mut arena = Arena::new();

        let (_keep_alive, handle) = arena.insert(&mut arena_mgr, |_rh, _w| {}, 0u32);

        b.iter(|| {
            *arena.get_mut(handle).unwrap() += 1;
        });
    });

    c.bench_function("repeated_addition/local_deref_multi", |b| {
        let mut arena_mgr = ArenaManager::<()>::new();
        let mut arena = Arena::new();

        let (_keep_alive, handle) = arena.insert(&mut arena_mgr, |_rh, _w| {}, 0u32);

        b.iter(|| {
            *arena.get_mut(handle).unwrap() += 1;
            *arena.get_mut(handle).unwrap() += 1;
        });
    });

    c.bench_function("repeated_addition/global_deref_single", |b| {
        let mut w = World::new();
        let w = &mut w;

        let handle = Counter(0).spawn(w);

        b.iter(|| {
            handle.m(w).0 += 1;
        });
    });

    c.bench_function("repeated_addition/global_deref_multi", |b| {
        let mut w = World::new();
        let w = &mut w;

        let handle = Counter(0).spawn(w);

        b.iter(|| {
            handle.m(w).0 += 1;
            handle.m(w).0 += 1;
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
