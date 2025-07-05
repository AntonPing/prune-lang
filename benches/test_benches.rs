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

fn bench_double_reverse(c: &mut Criterion) {
    c.bench_function("double_reverse", |b| {
        b.iter(|| {
            action::test_unsat_prog("double_reverse", "double_reverse_same", 30, 31, 1).unwrap()
        })
    });
}

fn bench_double_reverse_bad(c: &mut Criterion) {
    c.bench_function("double_reverse_bad", |b| {
        b.iter(|| {
            action::test_sat_prog("double_reverse_bad", "double_reverse_same", 10, 1000, 10)
                .unwrap()
        })
    });
}

fn bench_reverse_length(c: &mut Criterion) {
    c.bench_function("reverse_length", |b| {
        b.iter(|| {
            action::test_unsat_prog("reverse_length", "same_length_after_reverse", 30, 31, 1)
                .unwrap()
        })
    });
}

fn bench_reverse_length_bad(c: &mut Criterion) {
    c.bench_function("reverse_length_bad", |b| {
        b.iter(|| {
            action::test_sat_prog(
                "reverse_length_bad",
                "same_length_after_reverse",
                10,
                1000,
                10,
            )
            .unwrap()
        })
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

// fn bench_avl_tree_bad(c: &mut Criterion) {
//     c.bench_function("avl_tree_bad", |b| {
//         b.iter(|| {
//             action::test_sat_prog("avl_tree_bad", "always_sorted_balanced", 30, 31, 10).unwrap()
//         })
//     });
// }

criterion_group!(
    benches,
    bench_append,
    bench_append_bad,
    bench_double_reverse,
    bench_double_reverse_bad,
    bench_reverse_length,
    bench_reverse_length_bad,
    bench_tree_insert,
    bench_tree_insert_bad,
    bench_avl_tree,
    // bench_avl_tree_bad,
);

criterion_main!(benches);
