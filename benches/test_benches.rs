use criterion::{criterion_group, criterion_main, Criterion};
use norem_lang::driver::action;

fn bench_append_bad(c: &mut Criterion) {
    c.bench_function("append_bad(td)", |b| {
        b.iter(|| action::test_bad_prog("append", "is_elem_after_append", 5, 100, 5).unwrap())
    });
}

fn bench_reverse_bad(c: &mut Criterion) {
    c.bench_function("reverse_bad(td)", |b| {
        b.iter(|| action::test_bad_prog("reverse", "twice_reverse", 5, 100, 5).unwrap())
    });
}

fn bench_tree_insert_bad(c: &mut Criterion) {
    c.bench_function("tree_insert_bad(td)", |b| {
        b.iter(|| action::test_bad_prog("tree_insert", "always_sorted", 5, 100, 5).unwrap())
    });
}

criterion_group!(
    benches,
    bench_append_bad,
    bench_reverse_bad,
    bench_tree_insert_bad,
);

criterion_main!(benches);
