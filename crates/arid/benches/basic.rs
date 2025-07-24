use arid::{Handle, Object, World, object};
use criterion::{Criterion, criterion_group, criterion_main};

#[derive(Debug)]
pub struct Counter(u32);

object!(Counter);

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("repeated_addition/direct_access", |b| {
        let mut counter = 0u32;

        b.iter(|| {
            counter += 1;
        });
    });

    c.bench_function("repeated_addition/deref_single", |b| {
        let mut w = World::new();
        let w = &mut w;

        let handle = Counter(0).spawn(w);

        b.iter(|| {
            handle.m(w).0 += 1;
        });
    });

    c.bench_function("repeated_addition/deref_multi", |b| {
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
