use crate::logic::ast::*;
use crate::syntax::{self, ast};
use crate::utils::ident::Ident;
use crate::walker::{compile, walker::Walker};

use easy_smt::{Context, ContextBuilder};
use std::path::{self, PathBuf};
use std::{fs, io};

pub fn parse_program<S: AsRef<path::Path>>(path: S) -> Result<ast::Program, String> {
    let src = fs::read_to_string(path).unwrap();
    let prog = syntax::parser::parser::ProgramParser::new()
        .parse(&src.as_str())
        .map_err(|err| format!("{}", err));
    prog
}

pub fn build_smt_ctx() -> Result<Context, ()> {
    let ctx = ContextBuilder::new()
        .solver("z3")
        .solver_args(["-smt2", "-in"])
        .build()
        .map_err(|_err| ())?;
    Ok(ctx)
}

pub fn test_prog<P: AsRef<path::Path>, Log: io::Write>(
    path: P,
    entry: &'static str,
    start: usize,
    end: usize,
    step: usize,
    log: &mut Log,
) -> Result<bool, String> {
    let prog = parse_program(path)?;
    let dict = crate::logic::transform::prog_to_dict(&prog);
    let (codes, entrys) = compile::compile_dict(&dict);
    let map = crate::logic::infer::infer_type_map(&dict);
    let mut wlk = Walker::new(codes, map, log);
    let entry = entrys[&PredIdent::Check(Ident::dummy(&entry))];
    let res = wlk.run_loop(entry, start, end, step);
    Ok(res)
}

pub fn test_unsat_prog<P: AsRef<path::Path>>(
    prog_name: P,
    entry: &'static str,
    start: usize,
    end: usize,
    step: usize,
) -> Result<(), String> {
    let mut path = PathBuf::new();
    path.push("examples");
    path.push("unsat");
    path.push(prog_name);
    path.set_extension("nrm");
    let mut log = io::empty();
    let res = test_prog(path, entry, start, end, step, &mut log)?;
    assert_eq!(res, false);
    Ok(())
}

pub fn test_sat_prog<P: AsRef<path::Path>>(
    prog_name: P,
    entry: &'static str,
    start: usize,
    end: usize,
    step: usize,
) -> Result<(), String> {
    let mut path = PathBuf::new();
    path.push("examples");
    path.push("sat");
    path.push(prog_name);
    path.set_extension("nrm");
    let mut log = io::empty();
    let res = test_prog(path, entry, start, end, step, &mut log)?;
    assert_eq!(res, true);
    Ok(())
}
