use criterion::{criterion_group, criterion_main, Criterion};
use norem_lang::driver::action;

fn bench_append_good(c: &mut Criterion) {
    c.bench_function("append_good", |b| {
        b.iter(|| action::test_example_good_prog("append", 20).unwrap())
    });
}

fn bench_append_bad(c: &mut Criterion) {
    c.bench_function("append_bad", |b| {
        b.iter(|| action::test_example_bad_prog("append", 20).unwrap())
    });
}

fn bench_reverse_good(c: &mut Criterion) {
    c.bench_function("reverse_good", |b| {
        b.iter(|| action::test_example_good_prog("reverse", 20).unwrap())
    });
}

fn bench_reverse_bad(c: &mut Criterion) {
    c.bench_function("reverse_bad", |b| {
        b.iter(|| action::test_example_bad_prog("reverse", 20).unwrap())
    });
}

fn bench_tree_insert_good(c: &mut Criterion) {
    c.bench_function("tree_insert_good", |b| {
        b.iter(|| action::test_example_good_prog("tree_insert", 4).unwrap())
    });
}

fn bench_tree_insert_bad(c: &mut Criterion) {
    c.bench_function("tree_insert_bad", |b| {
        b.iter(|| action::test_example_bad_prog("tree_insert", 20).unwrap())
    });
}

criterion_group!(
    benches,
    bench_append_good,
    bench_append_bad,
    bench_reverse_good,
    bench_reverse_bad,
    bench_tree_insert_good,
    bench_tree_insert_bad,
);

criterion_main!(benches);
