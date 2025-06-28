use criterion::{criterion_group, criterion_main, Criterion};
use norem_lang::driver::action;

fn bench_append_bad(c: &mut Criterion) {
    c.bench_function("append_bad", |b| {
        b.iter(|| action::test_bad_prog("append", "is_elem_after_append", 5, 100, 5).unwrap())
    });
}

fn bench_append_good(c: &mut Criterion) {
    c.bench_function("append_good", |b| {
        b.iter(|| action::test_good_prog("append", "is_elem_after_append", 20, 21, 1).unwrap())
    });
}

fn bench_reverse_bad(c: &mut Criterion) {
    c.bench_function("reverse_bad", |b| {
        b.iter(|| action::test_bad_prog("reverse", "twice_reverse", 5, 100, 5).unwrap())
    });
}

fn bench_reverse_good(c: &mut Criterion) {
    c.bench_function("reverse_good", |b| {
        b.iter(|| action::test_good_prog("reverse", "twice_reverse", 20, 21, 1).unwrap())
    });
}

fn bench_tree_insert_bad(c: &mut Criterion) {
    c.bench_function("tree_insert_bad", |b| {
        b.iter(|| action::test_bad_prog("tree_insert", "always_sorted", 5, 100, 5).unwrap())
    });
}

fn bench_tree_insert_good(c: &mut Criterion) {
    c.bench_function("tree_insert_good", |b| {
        b.iter(|| action::test_good_prog("tree_insert", "always_sorted", 10, 11, 1).unwrap())
    });
}

fn bench_avl_tree_good(c: &mut Criterion) {
    c.bench_function("avl_tree_good", |b| {
        b.iter(|| action::test_good_prog("avl_tree", "always_sorted_balanced", 10, 11, 1).unwrap())
    });
}

criterion_group!(
    benches,
    bench_append_bad,
    bench_append_good,
    bench_reverse_bad,
    bench_reverse_good,
    bench_tree_insert_bad,
    bench_tree_insert_good,
    bench_avl_tree_good,
);

criterion_main!(benches);
