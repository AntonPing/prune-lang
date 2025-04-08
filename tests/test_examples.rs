use easy_smt::ContextBuilder;
use norem_lang::{logic, solver, syntax};
use std::{fs, path};

pub fn test_good_prog<S: AsRef<path::Path>>(prog_name: S, iter: usize) {
    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push(prog_name);
    path.push("good_prog");
    path.set_extension("nrm");
    let src = fs::read_to_string(&path).unwrap();
    let prog = syntax::parser::parser::ProgramParser::new()
        .parse(&src.as_str())
        .unwrap();

    let (succ_preds, fail_preds, check_preds) = logic::trans::prog_to_triple(&prog);
    let succ_preds = logic::trans::dnf_pred_dict(&succ_preds);
    let fail_preds = logic::trans::dnf_pred_dict(&fail_preds);
    let check_preds = logic::trans::dnf_pred_dict(&check_preds);
    let mut checker = solver::solver::Checker::new(&succ_preds, &fail_preds, &check_preds);

    let mut ctx = ContextBuilder::new()
        .solver("z3")
        .solver_args(["-smt2", "-in"])
        .build()
        .expect("failed to build smt solver context!");

    // println!("{:#?}", checker);
    for _k in 0..iter {
        // println!("iter={}", k + 1);
        checker.solve_step(&mut ctx);
        // checker.print_stat();
        // checker.drop_sols(1000);
        // checker.print_stat();
        if checker.check_counter_example() {
            checker.merge_print();
            assert!(false);
        }
    }
}

pub fn test_bad_prog<S: AsRef<path::Path>>(prog_name: S, iter: usize) {
    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push(prog_name);
    path.push("bad_prog");
    path.set_extension("nrm");
    let src = fs::read_to_string(&path).unwrap();
    let prog = syntax::parser::parser::ProgramParser::new()
        .parse(&src.as_str())
        .unwrap();

    let (succ_preds, fail_preds, check_preds) = logic::trans::prog_to_triple(&prog);
    let succ_preds = logic::trans::dnf_pred_dict(&succ_preds);
    let fail_preds = logic::trans::dnf_pred_dict(&fail_preds);
    let check_preds = logic::trans::dnf_pred_dict(&check_preds);
    let mut checker = solver::solver::Checker::new(&succ_preds, &fail_preds, &check_preds);

    let mut ctx = ContextBuilder::new()
        .solver("z3")
        .solver_args(["-smt2", "-in"])
        .build()
        .expect("failed to build smt solver context!");

    // println!("{:#?}", checker);
    for _k in 0..iter {
        // println!("iter={}", k + 1);
        checker.solve_step(&mut ctx);
        // checker.print_stat();
        // checker.drop_sols(1000);
        // checker.print_stat();
        if checker.check_counter_example() {
            // checker.merge_print();
            return;
        }
    }
    assert!(false);
}

pub fn test_prog<S: AsRef<path::Path>>(prog_name: S, iter: usize) {
    test_bad_prog(&prog_name, iter);
    test_good_prog(&prog_name, iter);
}

#[test]
fn test_append_good() {
    test_good_prog("append", 20);
}

#[test]
fn test_append_bad() {
    test_bad_prog("append", 20);
}

#[test]
fn test_reverse_good() {
    test_good_prog("reverse", 20);
}

#[test]
fn test_reverse_bad() {
    test_bad_prog("reverse", 20);
}

#[test]
fn test_tree_insert_good() {
    test_good_prog("tree_insert", 4);
}

#[test]
fn test_tree_insert_bad() {
    test_bad_prog("tree_insert", 10);
}
