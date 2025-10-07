use std::path::PathBuf;

use prune_lang::driver::cli;

#[test]
fn test_append_good() {
    let res = cli::run_cli_test(PathBuf::from("./examples/sym_exec_good/append_good.pr")).unwrap();
    assert!(res.iter().all(|p| *p == 0));
}

#[test]
fn test_append_bad() {
    let res = cli::run_cli_test(PathBuf::from("./examples/sym_exec_bad/append_bad.pr")).unwrap();
    assert!(res.iter().any(|p| *p > 0));
}

#[test]
fn test_double_reverse_good() {
    let res = cli::run_cli_test(PathBuf::from(
        "./examples/sym_exec_good/double_reverse_good.pr",
    ))
    .unwrap();
    assert!(res.iter().all(|p| *p == 0));
}

#[test]
fn test_double_reverse_bad() {
    let res = cli::run_cli_test(PathBuf::from(
        "./examples/sym_exec_bad/double_reverse_bad.pr",
    ))
    .unwrap();
    assert!(res.iter().any(|p| *p > 0));
}

#[test]
fn test_reverse_length_good() {
    let res = cli::run_cli_test(PathBuf::from(
        "./examples/sym_exec_good/reverse_length_good.pr",
    ))
    .unwrap();
    assert!(res.iter().all(|p| *p == 0));
}

#[test]
fn test_reverse_length_bad() {
    let res = cli::run_cli_test(PathBuf::from(
        "./examples/sym_exec_bad/reverse_length_bad.pr",
    ))
    .unwrap();
    assert!(res.iter().any(|p| *p > 0));
}

#[test]
fn test_tree_insert_good() {
    let res = cli::run_cli_test(PathBuf::from(
        "./examples/sym_exec_good/tree_insert_good.pr",
    ))
    .unwrap();
    assert!(res.iter().all(|p| *p == 0));
}

#[test]
fn test_tree_insert_bad() {
    let res =
        cli::run_cli_test(PathBuf::from("./examples/sym_exec_bad/tree_insert_bad.pr")).unwrap();
    assert!(res.iter().any(|p| *p > 0));
}

#[test]
fn test_avl_tree_good() {
    let res =
        cli::run_cli_test(PathBuf::from("./examples/sym_exec_good/avl_tree_good.pr")).unwrap();
    assert!(res.iter().all(|p| *p == 0));
}

// #[test]
// fn test_avl_tree_bad() {
//     let res = cli::run_cli_test(PathBuf::from("./examples/sym_exec_bad/avl_tree_bad.pr")).unwrap();
//     assert!(res.iter().any(|p| *p > 0));
// }

#[test]
fn test_avl_tree_gen() {
    cli::run_cli_test(PathBuf::from("./examples/test_gen/avl_tree_gen.pr")).unwrap();
}

#[test]
fn test_lambda_free_gen() {
    cli::run_cli_test(PathBuf::from("./examples/test_gen/lambda_free_gen.pr")).unwrap();
}

#[test]
fn test_stlc_term_gen() {
    cli::run_cli_test(PathBuf::from("./examples/test_gen/stlc_term_gen.pr")).unwrap();
}

#[test]
fn test_mini_lang_gen() {
    cli::run_cli_test(PathBuf::from("./examples/test_gen/mini_lang_gen.pr")).unwrap();
}

#[test]
fn test_reverse_forward() {
    cli::run_cli_test(PathBuf::from("./examples/basic/reverse_forward.pr")).unwrap();
}

#[test]
fn test_reverse_backward() {
    cli::run_cli_test(PathBuf::from("./examples/basic/reverse_backward.pr")).unwrap();
}

#[test]
fn test_pythagorean() {
    cli::run_cli_test(PathBuf::from("./examples/other/pythagorean.pr")).unwrap();
}

#[test]
fn test_fermat_n3() {
    cli::run_cli_test(PathBuf::from("./examples/other/fermat_n3.pr")).unwrap();
}
