// use poinciana::{self, scaffold};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn big_tree(c: &mut Criterion) {
    let tree =
        std::fs::read_to_string("benches/bench_data/cancel.tree").unwrap();

    let cfg = Default::default();
    let mut group = c.benchmark_group("sample-size-10");
    group.bench_function("big-tree", |b| {
        // TODO: Implement the benchmark
    });
    group.finish();
}

criterion_group!(benches, big_tree);
criterion_main!(benches);
