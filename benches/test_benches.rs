use criterion::{criterion_group, criterion_main, Criterion};
use prune_lang::driver::action;

fn bench_append_good(c: &mut Criterion) {
    c.bench_function("append_good", |b| {
        b.iter(|| action::test_sym_exec_good_prog("append_good").unwrap())
    });
}

fn bench_append_bad(c: &mut Criterion) {
    c.bench_function("append_bad", |b| {
        b.iter(|| action::test_sym_exec_bad_prog("append_bad").unwrap())
    });
}

fn bench_double_reverse_good(c: &mut Criterion) {
    c.bench_function("double_reverse_good", |b| {
        b.iter(|| action::test_sym_exec_good_prog("double_reverse_good").unwrap())
    });
}

fn bench_double_reverse_bad(c: &mut Criterion) {
    c.bench_function("double_reverse_bad", |b| {
        b.iter(|| action::test_sym_exec_bad_prog("double_reverse_bad").unwrap())
    });
}

fn bench_reverse_length_good(c: &mut Criterion) {
    c.bench_function("reverse_length_good", |b| {
        b.iter(|| action::test_sym_exec_good_prog("reverse_length_good").unwrap())
    });
}

fn bench_reverse_length_bad(c: &mut Criterion) {
    c.bench_function("reverse_length_bad", |b| {
        b.iter(|| action::test_sym_exec_bad_prog("reverse_length_bad").unwrap())
    });
}

fn bench_tree_insert_good(c: &mut Criterion) {
    c.bench_function("tree_insert_good", |b| {
        b.iter(|| action::test_sym_exec_good_prog("tree_insert_good").unwrap())
    });
}

fn bench_tree_insert_bad(c: &mut Criterion) {
    c.bench_function("tree_insert_bad", |b| {
        b.iter(|| action::test_sym_exec_bad_prog("tree_insert_bad").unwrap())
    });
}

fn bench_avl_tree_good(c: &mut Criterion) {
    c.bench_function("avl_tree_good", |b| {
        b.iter(|| action::test_sym_exec_good_prog("avl_tree_good").unwrap())
    });
}

// fn bench_avl_tree_bad(c: &mut Criterion) {
//     c.bench_function("avl_tree_bad", |b| {
//         b.iter(|| action::test_sat_prog("avl_tree_bad").unwrap())
//     });
// }

criterion_group!(
    benches,
    bench_append_good,
    bench_append_bad,
    bench_double_reverse_good,
    bench_double_reverse_bad,
    bench_reverse_length_good,
    bench_reverse_length_bad,
    bench_tree_insert_good,
    bench_tree_insert_bad,
    bench_avl_tree_good,
    // bench_avl_tree_bad,
);

criterion_main!(benches);
