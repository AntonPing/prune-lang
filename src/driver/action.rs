use crate::driver::diagnostic::Diagnostic;
use crate::logic::ast::*;
use crate::syntax::{self, ast};
use crate::utils::ident::Ident;
use crate::walker::{compile, walker::Walker};

use std::collections::HashMap;
use std::path::{self, PathBuf};
use std::{fs, io};

use crate::tych::rename;

pub struct Pipeline<'src, 'log, Log: io::Write> {
    src: &'src str,
    log: &'log mut Log,
    diags: Vec<Diagnostic>,
}

type PipeResult<T> = Result<T, ()>;

impl<'src, 'log, Log: io::Write> Pipeline<'src, 'log, Log> {
    pub fn new(src: &'src str, log: &'log mut Log) -> Pipeline<'src, 'log, Log> {
        Pipeline {
            src,
            log,
            diags: Vec::new(),
        }
    }

    pub fn check_diag(&mut self) -> PipeResult<()> {
        if self.diags.is_empty() {
            return Ok(());
        }
        for diag in &self.diags {
            write!(&mut self.log, "{}", diag.report(self.src, 10)).unwrap();
        }
        Err(())
    }

    // pub fn read_file<P: AsRef<path::Path>>(&mut self, path: P) -> PipeResult<String> {
    //     let src = fs::read_to_string(path).map_err(|_err| {
    //         self.diags.push(Diagnostic::error("file does not exist!"));
    //     })?;
    //     Ok(src)
    // }

    pub fn parse_program(&mut self) -> PipeResult<ast::Program> {
        let prog = syntax::parser::parse_program(&mut self.diags, &self.src);
        self.check_diag()?;
        Ok(prog)
    }

    pub fn rename_pass(&mut self, prog: &mut ast::Program) -> PipeResult<HashMap<Ident, Ident>> {
        let map = rename::rename_pass(prog).map_err(|errs| {
            errs.into_iter().for_each(|_err| {
                self.diags.push(Diagnostic::error("rename error!"));
            });
        })?;
        self.check_diag()?;
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

    pub fn test_prog(
        &'log mut self,
        entry: &'static str,
        start: usize,
        end: usize,
        step: usize,
    ) -> Result<bool, ()> {
        let mut prog = self.parse_program()?;
        let map = self.rename_pass(&mut prog)?;
        let (mut wlk, entrys) = self.create_walker(&prog)?;
        let entry = entrys
            .get(&PredIdent::Check(map[&Ident::dummy(&entry)]))
            .ok_or(())?;
        let res = wlk.run_loop(*entry, start, end, step);
        Ok(res)
    }
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
    let src = fs::read_to_string(path).map_err(|_err| ())?;
    let mut log = io::empty();
    let mut pipe = Pipeline::new(&src, &mut log);
    let res = pipe.test_prog(entry, start, end, step)?;
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
    let src = fs::read_to_string(path).map_err(|_err| ())?;
    let mut log = io::empty();
    let mut pipe = Pipeline::new(&src, &mut log);
    let res = pipe.test_prog(entry, start, end, step)?;
    assert_eq!(res, true);
    Ok(())
}
