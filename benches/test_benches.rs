use std::path::PathBuf;

use criterion::{Criterion, criterion_group, criterion_main};
use prune_lang::cli;

fn bench_reverse_forward(c: &mut Criterion) {
    c.bench_function("reverse_forward", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/basic/reverse_forward.pr"))
                .unwrap()
        })
    });
}

fn bench_reverse_backward(c: &mut Criterion) {
    c.bench_function("reverse_backward", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/basic/reverse_backward.pr"))
                .unwrap()
        })
    });
}

fn bench_concat_forward(c: &mut Criterion) {
    c.bench_function("concat_forward", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/basic/concat_forward.pr"))
                .unwrap()
        })
    });
}

fn bench_concat_backward(c: &mut Criterion) {
    c.bench_function("concat_backward", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/basic/concat_backward.pr"))
                .unwrap()
        })
    });
}

fn bench_avl_tree_gen(c: &mut Criterion) {
    c.bench_function("avl_tree_gen", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/test_gen/avl_tree_gen.pr"))
                .unwrap()
        })
    });
}

fn bench_lambda_free_gen(c: &mut Criterion) {
    c.bench_function("lambda_free_gen", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/test_gen/lambda_free_gen.pr"))
                .unwrap()
        })
    });
}

fn bench_mini_lang_gen(c: &mut Criterion) {
    c.bench_function("mini_lang_gen", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/test_gen/mini_lang_gen.pr"))
                .unwrap()
        })
    });
}

fn bench_stlc_term_gen(c: &mut Criterion) {
    c.bench_function("stlc_term_gen", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/test_gen/stlc_term_gen.pr"))
                .unwrap()
        })
    });
}

fn bench_append_bad(c: &mut Criterion) {
    c.bench_function("append_bad", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/sym_exec_bad/append_bad.pr"))
                .unwrap()
        })
    });
}

fn bench_double_reverse_bad(c: &mut Criterion) {
    c.bench_function("double_reverse_bad", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from(
                "./examples/sym_exec_bad/double_reverse_bad.pr",
            ))
            .unwrap()
        })
    });
}

fn bench_tree_insert_bad(c: &mut Criterion) {
    c.bench_function("tree_insert_bad", |b| {
        b.iter(|| {
            cli::pipeline::run_cli_test(PathBuf::from("./examples/sym_exec_bad/tree_insert_bad.pr"))
                .unwrap()
        })
    });
}

// todo: more benchmarks in real times

criterion_group!(
    benches,
    bench_reverse_forward,
    bench_reverse_backward,
    bench_concat_forward,
    bench_concat_backward,
    bench_avl_tree_gen,
    bench_lambda_free_gen,
    bench_mini_lang_gen,
    bench_stlc_term_gen,
    bench_append_bad,
    bench_double_reverse_bad,
    bench_tree_insert_bad,
);

criterion_main!(benches);
