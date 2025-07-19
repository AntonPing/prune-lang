use crate::logic::ast::*;
use crate::syntax::{self, ast};
use crate::utils::ident::Ident;
use crate::walker::{compile, walker::Walker};

use std::collections::HashMap;
use std::path::{self, PathBuf};
use std::{fs, io};

use crate::tych::rename;

pub enum PipeError {
    FileNotExist(String),
    ParseError(String),
    RenameError(rename::RenameError),
}

pub struct Pipeline<'log, Log: io::Write> {
    error: Vec<PipeError>,
    log: &'log mut Log,
}

impl<'log, Log: io::Write> Pipeline<'log, Log> {
    pub fn new(log: &'log mut Log) -> Pipeline<'log, Log> {
        Pipeline {
            error: Vec::new(),
            log,
        }
    }
    pub fn parse_program<S: AsRef<path::Path>>(&mut self, path: S) -> Result<ast::Program, ()> {
        let src = fs::read_to_string(path).map_err(|err| {
            let s = err.to_string();
            self.error.push(PipeError::FileNotExist(s));
            ()
        })?;
        let prog = syntax::parser::parser::ProgramParser::new()
            .parse(&src.as_str())
            .map_err(|err| {
                let s = err.to_string();
                self.error.push(PipeError::ParseError(s));
                ()
            })?;
        Ok(prog)
    }

    pub fn rename_pass(&mut self, prog: &mut ast::Program) -> Result<HashMap<Ident, Ident>, ()> {
        let map = rename::rename_pass(prog).map_err(|errs| {
            errs.into_iter().for_each(|err| {
                self.error.push(PipeError::RenameError(err));
            });
        })?;
        Ok(map)
    }

    pub fn create_walker(
        &'log mut self,
        prog: &ast::Program,
    ) -> Result<(Walker<'log, Log>, HashMap<PredIdent, usize>), ()> {
        let dict = crate::logic::transform::prog_to_dict(&prog);
        let (codes, entrys) = compile::compile_dict(&dict);
        let wlk = Walker::new(codes, self.log);
        Ok((wlk, entrys))
    }
}

pub fn test_prog<P: AsRef<path::Path>, Log: io::Write>(
    path: P,
    entry: &'static str,
    start: usize,
    end: usize,
    step: usize,
    log: &mut Log,
) -> Result<bool, ()> {
    let mut pipe = Pipeline::new(log);
    let mut prog = pipe.parse_program(path)?;
    let map = pipe.rename_pass(&mut prog)?;
    let (mut wlk, entrys) = pipe.create_walker(&prog)?;
    let entry = entrys[&PredIdent::Check(map[&Ident::dummy(&entry)])];
    let res = wlk.run_loop(entry, start, end, step);
    Ok(res)
}

pub fn test_unsat_prog<P: AsRef<path::Path>>(
    prog_name: P,
    entry: &'static str,
    start: usize,
    end: usize,
    step: usize,
) -> Result<(), ()> {
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
) -> Result<(), ()> {
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
