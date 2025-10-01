use std::path::PathBuf;

use criterion::{criterion_group, criterion_main, Criterion};
use prune_lang::driver::cli;

fn bench_append_good(c: &mut Criterion) {
    c.bench_function("append_good", |b| {
        b.iter(|| {
            cli::run_cli_test(PathBuf::from("./examples/sym_exec_good/append_good.pr")).unwrap()
        })
    });
}

// todo: more benchmarks in real times

criterion_group!(benches, bench_append_good);

criterion_main!(benches);
