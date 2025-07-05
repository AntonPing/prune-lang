use criterion::{criterion_group, criterion_main, Criterion};
use norem_lang::driver::action;

fn bench_append(c: &mut Criterion) {
    c.bench_function("append", |b| {
        b.iter(|| action::test_unsat_prog("append", "is_elem_after_append", 30, 31, 1).unwrap())
    });
}

fn bench_append_bad(c: &mut Criterion) {
    c.bench_function("append_bad", |b| {
        b.iter(|| {
            action::test_sat_prog("append_bad", "is_elem_after_append", 10, 1000, 10).unwrap()
        })
    });
}

fn bench_reverse(c: &mut Criterion) {
    c.bench_function("reverse", |b| {
        b.iter(|| action::test_unsat_prog("reverse", "twice_reverse", 30, 31, 1).unwrap())
    });
}

fn bench_reverse_bad(c: &mut Criterion) {
    c.bench_function("reverse_bad", |b| {
        b.iter(|| action::test_sat_prog("reverse_bad", "twice_reverse", 10, 1000, 10).unwrap())
    });
}

fn bench_tree_insert(c: &mut Criterion) {
    c.bench_function("tree_insert", |b| {
        b.iter(|| action::test_unsat_prog("tree_insert", "always_sorted", 30, 31, 1).unwrap())
    });
}

fn bench_tree_insert_bad(c: &mut Criterion) {
    c.bench_function("tree_insert_bad", |b| {
        b.iter(|| action::test_sat_prog("tree_insert_bad", "always_sorted", 10, 1000, 10).unwrap())
    });
}

fn bench_avl_tree(c: &mut Criterion) {
    c.bench_function("avl_tree", |b| {
        b.iter(|| {
            action::test_unsat_prog("avl_tree", "always_sorted_balanced", 30, 31, 10).unwrap()
        })
    });
}

fn bench_avl_tree_bad(c: &mut Criterion) {
    c.bench_function("avl_tree_bad", |b| {
        b.iter(|| {
            action::test_sat_prog("avl_tree_bad", "always_sorted_balanced", 30, 31, 10).unwrap()
        })
    });
}

criterion_group!(
    benches,
    bench_append_bad,
    bench_append,
    bench_reverse_bad,
    bench_reverse,
    bench_tree_insert_bad,
    bench_tree_insert,
    bench_avl_tree,
    // bench_avl_tree_bad,
);

criterion_main!(benches);
