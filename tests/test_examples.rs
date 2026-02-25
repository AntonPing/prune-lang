use std::path::PathBuf;

use prune_lang::cli;

#[test]
fn test_unary_arith() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/arith/unary_arith.pr")).unwrap();
}

#[test]
fn test_binary_arith() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/arith/binary_arith.pr")).unwrap();
}

#[test]
fn test_binary_vec() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/arith/binary_vec.pr")).unwrap();
}

#[test]
fn test_reverse_forward() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/basic/reverse_forward.pr")).unwrap();
}

#[test]
fn test_reverse_backward() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/basic/reverse_backward.pr")).unwrap();
}

#[test]
fn test_concat_forward() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/basic/concat_forward.pr")).unwrap();
}

#[test]
fn test_concat_backward() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/basic/concat_backward.pr")).unwrap();
}

#[test]
fn test_polymorphism() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/features/polymorphism.pr")).unwrap();
}

#[test]
fn test_polymorphism_fail() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/features/polymorphism_fail.pr"))
        .expect_err("this should fail at type checking!");
}

#[test]
fn test_smt_sat() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/features/smt_sat.pr")).unwrap();
}

#[test]
fn test_smt_unsat() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/features/smt_unsat.pr")).unwrap();
}

#[test]
fn test_tree_insert_good() {
    let res = cli::pipeline::run_cli_test(PathBuf::from("./examples/sym_exec/tree_insert_good.pr"))
        .unwrap();
    assert!(res.iter().all(|p| *p == 0));
}

#[test]
fn test_tree_insert_bad() {
    let res = cli::pipeline::run_cli_test(PathBuf::from("./examples/sym_exec/tree_insert_bad.pr"))
        .unwrap();
    assert!(res.iter().any(|p| *p > 0));
}

#[test]
fn test_avl_tree_good() {
    let res =
        cli::pipeline::run_cli_test(PathBuf::from("./examples/sym_exec/avl_tree_good.pr")).unwrap();
    assert!(res.iter().all(|p| *p == 0));
}

// #[test]
// fn test_avl_tree_bad() {
//     let res =
//         cli::pipeline::run_cli_test(PathBuf::from("./examples/sym_exec/avl_tree_bad.pr")).unwrap();
//     assert!(res.iter().any(|p| *p > 0));
// }

#[test]
fn test_avl_tree_gen() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/test_gen/avl_tree_gen.pr")).unwrap();
}

#[test]
fn test_lambda_free_gen() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/test_gen/lambda_free_gen.pr")).unwrap();
}

#[test]
fn test_stlc_term_gen() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/test_gen/stlc_term_gen.pr")).unwrap();
}

#[test]
fn test_mini_lang_gen() {
    cli::pipeline::run_cli_test(PathBuf::from("./examples/test_gen/mini_lang_gen.pr")).unwrap();
}
