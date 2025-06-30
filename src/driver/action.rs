use crate::logic::trans::{self, PredIdent};
use crate::syntax::{self, ast};
use crate::utils::ident::Ident;
use crate::walker_new::{compile, walker::Walker};

use easy_smt::{Context, ContextBuilder};
use std::collections::HashMap;
use std::{fs, path};

pub fn parse_program<S: AsRef<path::Path>>(path: S) -> Result<ast::Program, String> {
    let src = fs::read_to_string(path).unwrap();
    let prog = syntax::parser::parser::ProgramParser::new()
        .parse(&src.as_str())
        .map_err(|err| format!("{}", err));
    prog
}

pub fn build_solver(prog: &ast::Program) -> Result<(Walker, HashMap<PredIdent, usize>), ()> {
    let dict = trans::prog_to_dict(prog);
    let (codes, entrys) = compile::compile_dict(&dict);
    let map = crate::logic::infer::infer_type_map(&dict);
    let chk = Walker::new(codes, map);
    Ok((chk, entrys))
}

pub fn build_smt_ctx() -> Result<Context, ()> {
    let ctx = ContextBuilder::new()
        .solver("z3")
        .solver_args(["-smt2", "-in"])
        .build()
        .map_err(|_err| ())?;
    Ok(ctx)
}

pub fn test_prog<S: AsRef<path::Path>>(
    prog_name: S,
    entry: &'static str,
    start: usize,
    end: usize,
    step: usize,
) -> Result<bool, String> {
    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push(prog_name);
    path.set_extension("nrm");
    let prog = parse_program(path)?;
    let (mut chk, map) = build_solver(&prog).map_err(|_| "failed to build solver".to_string())?;
    let entry = map[&PredIdent::Check(Ident::dummy(&entry))];
    // println!("{:#?}", chk);
    let res = chk.run_loop(entry, start, end, step);
    Ok(res)
}

pub fn test_good_prog<S: AsRef<path::Path>>(
    prog_name: S,
    entry: &'static str,
    start: usize,
    end: usize,
    step: usize,
) -> Result<(), ()> {
    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push(prog_name);
    path.push("good_prog");
    path.set_extension("nrm");
    let prog = parse_program(path).unwrap();
    let (mut chk, map) = build_solver(&prog).unwrap();
    let entry = map[&PredIdent::Check(Ident::dummy(&entry))];
    // println!("{:#?}", chk);
    if chk.run_loop(entry, start, end, step) {
        Err(())
    } else {
        Ok(())
    }
}

pub fn test_bad_prog<S: AsRef<path::Path>>(
    prog_name: S,
    entry: &'static str,
    start: usize,
    end: usize,
    step: usize,
) -> Result<(), ()> {
    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push(prog_name);
    path.push("bad_prog");
    path.set_extension("nrm");
    let prog = parse_program(path).unwrap();
    let (mut chk, map) = build_solver(&prog).unwrap();
    let entry = map[&PredIdent::Check(Ident::dummy(&entry))];
    // println!("{:#?}", chk);
    if chk.run_loop(entry, start, end, step) {
        Ok(())
    } else {
        Err(())
    }
}
