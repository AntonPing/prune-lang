use crate::logic::trans;
use crate::solver_bu::{self, solver::Checker};
use crate::syntax::{self, ast};
use easy_smt::{Context, ContextBuilder};
use std::{fs, path};

pub fn parse_program<S: AsRef<path::Path>>(path: S) -> Result<ast::Program, ()> {
    let src = fs::read_to_string(path).unwrap();
    let prog = syntax::parser::parser::ProgramParser::new()
        .parse(&src.as_str())
        .map_err(|_err| ())?;
    Ok(prog)
}

pub fn build_checker(prog: &ast::Program) -> Result<Checker, ()> {
    let dict = trans::prog_to_dict(prog);
    let dict = trans::dnf_pred_dict(&dict);
    let chk = solver_bu::solver::Checker::new(&dict);
    Ok(chk)
}

pub fn build_smt_ctx() -> Result<Context, ()> {
    let ctx = ContextBuilder::new()
        .solver("z3")
        .solver_args(["-smt2", "-in"])
        .build()
        .map_err(|_err| ())?;
    Ok(ctx)
}

pub fn check_good_prog(chk: &mut Checker, ctx: &mut Context, iter: usize) -> Result<(), ()> {
    // println!("{:#?}", chk);
    for _k in 0..iter {
        // println!("iter={}", k + 1);
        chk.solve_step(ctx);
        // chk.print_stat();
        // chk.drop_sols(1000);
        // chk.print_stat();
        if chk.check_counter_example() {
            chk.merge_print();
            return Err(());
        }
    }
    Ok(())
}

pub fn check_bad_prog(chk: &mut Checker, ctx: &mut Context, iter: usize) -> Result<(), ()> {
    // println!("{:#?}", chk);
    for _k in 0..iter {
        // println!("iter={}", k + 1);
        chk.solve_step(ctx);
        // chk.print_stat();
        // chk.drop_sols(1000);
        // chk.print_stat();
        if chk.check_counter_example() {
            // chk.merge_print();
            return Ok(());
        }
    }
    Err(())
}

pub fn test_example_good_prog<S: AsRef<path::Path>>(prog_name: S, iter: usize) -> Result<(), ()> {
    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push(prog_name);
    path.push("good_prog");
    path.set_extension("nrm");
    let prog = parse_program(path)?;
    let mut chk = build_checker(&prog)?;
    let mut ctx = build_smt_ctx()?;
    check_good_prog(&mut chk, &mut ctx, iter)
}

pub fn test_example_bad_prog<S: AsRef<path::Path>>(prog_name: S, iter: usize) -> Result<(), ()> {
    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push(prog_name);
    path.push("bad_prog");
    path.set_extension("nrm");
    let prog = parse_program(path)?;
    let mut chk = build_checker(&prog)?;
    let mut ctx = build_smt_ctx()?;
    check_bad_prog(&mut chk, &mut ctx, iter)
}
