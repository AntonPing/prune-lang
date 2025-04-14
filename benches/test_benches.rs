use criterion::{criterion_group, criterion_main, Criterion};
use norem_lang::driver::action;

fn bench_bu_append_bad(c: &mut Criterion) {
    c.bench_function("append_bad(bu)", |b| {
        b.iter(|| action::test_bu_bad_prog("append", 20).unwrap())
    });
}

fn bench_bu_reverse_bad(c: &mut Criterion) {
    c.bench_function("reverse_bad(bu)", |b| {
        b.iter(|| action::test_bu_bad_prog("reverse", 20).unwrap())
    });
}

fn bench_bu_tree_insert_bad(c: &mut Criterion) {
    c.bench_function("tree_insert_bad(bu)", |b| {
        b.iter(|| action::test_bu_bad_prog("tree_insert", 20).unwrap())
    });
}

fn bench_td_append_bad(c: &mut Criterion) {
    c.bench_function("append_bad(td)", |b| {
        b.iter(|| action::test_td_bad_prog("append", "is_elem_after_append", 20).unwrap())
    });
}

fn bench_td_reverse_bad(c: &mut Criterion) {
    c.bench_function("reverse_bad(td)", |b| {
        b.iter(|| action::test_td_bad_prog("reverse", "twice_reverse", 20).unwrap())
    });
}

fn bench_td_tree_insert_bad(c: &mut Criterion) {
    c.bench_function("tree_insert_bad(td)", |b| {
        b.iter(|| action::test_td_bad_prog("tree_insert", "always_sorted", 5).unwrap())
    });
}

criterion_group!(
    benches,
    bench_bu_append_bad,
    bench_bu_reverse_bad,
    bench_bu_tree_insert_bad,
    bench_td_append_bad,
    bench_td_reverse_bad,
    bench_td_tree_insert_bad,
);

criterion_main!(benches);
