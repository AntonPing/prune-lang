use norem_lang::{solver, syntax};
use smtlib;
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
    let (succ_preds, fail_preds, check_preds) = solver::logic::prog_to_triple(&prog);
    let succ_preds = solver::logic::dnf_pred_dict(&succ_preds);
    let fail_preds = solver::logic::dnf_pred_dict(&fail_preds);
    let check_preds = solver::logic::dnf_pred_dict(&check_preds);
    let st = smtlib::Storage::new();
    let mut solver = smtlib::Solver::new(
        &st,
        smtlib::backend::z3_binary::Z3Binary::new("D:/z3-4.14.1-x64-win/bin/z3.exe").unwrap(),
    )
    .unwrap();
    let mut checker = solver::solver::Checker::new(&succ_preds, &fail_preds, &check_preds);
    // println!("{:#?}", checker);
    for _k in 0..iter {
        // println!("iter={}", k + 1);
        checker.solve_step(&st, &mut solver);
        // checker.print_stat();
        checker.drop_sols(100);
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
    let (succ_preds, fail_preds, check_preds) = solver::logic::prog_to_triple(&prog);
    let succ_preds = solver::logic::dnf_pred_dict(&succ_preds);
    let fail_preds = solver::logic::dnf_pred_dict(&fail_preds);
    let check_preds = solver::logic::dnf_pred_dict(&check_preds);
    let st = smtlib::Storage::new();
    let mut solver = smtlib::Solver::new(
        &st,
        smtlib::backend::z3_binary::Z3Binary::new("D:/z3-4.14.1-x64-win/bin/z3.exe").unwrap(),
    )
    .unwrap();
    let mut checker = solver::solver::Checker::new(&succ_preds, &fail_preds, &check_preds);
    // println!("{:#?}", checker);
    for _k in 0..iter {
        // println!("iter={}", k + 1);
        checker.solve_step(&st, &mut solver);
        // checker.print_stat();
        checker.drop_sols(100);
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
    test_good_prog("tree_insert", 5);
}

#[test]
fn test_tree_insert_bad() {
    test_bad_prog("tree_insert", 10);
}
