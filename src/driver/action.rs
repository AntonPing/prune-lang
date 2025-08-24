use crate::driver::diagnostic::Diagnostic;
use crate::logic::ast::*;
use crate::syntax::{self, ast};
use crate::tych;
use crate::walker::{self, compile, walker::Walker};

use super::*;

use std::collections::HashMap;
use std::path::{self, PathBuf};
use std::{fs, io};

pub struct Pipeline<'src, 'log, Log: io::Write> {
    src: &'src str,
    log: &'log mut Log,
    verbosity: u8,
    diags: Vec<Diagnostic>,
}

type PipeResult<T> = Result<T, ()>;

impl<'src, 'log, Log: io::Write> Pipeline<'src, 'log, Log> {
    pub fn new(src: &'src str, log: &'log mut Log) -> Pipeline<'src, 'log, Log> {
        Pipeline {
            src,
            log,
            verbosity: 10,
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
        let (prog, errs) = syntax::parser::parse_program(&self.src);
        if errs.is_empty() {
            Ok(prog)
        } else {
            for err in errs {
                let diag: Diagnostic = err.into();
                write!(&mut self.log, "{}", diag.report(self.src, self.verbosity)).unwrap();
            }
            Err(())
        }
    }

    pub fn rename_pass(&mut self, prog: &mut ast::Program) -> PipeResult<HashMap<Ident, Ident>> {
        let (map, errs) = tych::rename::rename_pass(prog);
        if errs.is_empty() {
            Ok(map)
        } else {
            for err in errs {
                let diag: Diagnostic = err.into();
                write!(&mut self.log, "{}", diag.report(self.src, self.verbosity)).unwrap();
            }
            Err(())
        }
    }

    pub fn check_pass(&mut self, prog: &mut ast::Program) -> PipeResult<HashMap<Ident, TypeId>> {
        let (map, errs) = tych::check::check_pass(prog);
        if errs.is_empty() {
            Ok(map)
        } else {
            for err in errs {
                let diag: Diagnostic = err.into();
                write!(&mut self.log, "{}", diag.report(self.src, self.verbosity)).unwrap();
            }
            Err(())
        }
    }

    pub fn create_walker(
        &'log mut self,
        prog: &ast::Program,
    ) -> (Walker<'log, Log>, HashMap<PredIdent, usize>) {
        let dict = crate::logic::transform::prog_to_dict(&prog);
        let (codes, map) = compile::compile_dict(&dict);
        let wlk = Walker::new(codes, self.log);
        (wlk, map)
    }

    pub fn create_walker_new(
        &'log mut self,
        prog: &ast::Program,
    ) -> (
        walker::walker_new::Walker<'log, Log>,
        HashMap<PredIdent, usize>,
    ) {
        let dict = crate::logic::transform::prog_to_dict(&prog);
        let (codes, map) = compile::compile_dict(&dict);
        let wlk = walker::walker_new::Walker::new(codes, self.log);
        (wlk, map)
    }

    pub fn test_prog(&'log mut self) -> Result<Vec<bool>, ()> {
        let mut prog = self.parse_program()?;
        let _rename_map = self.rename_pass(&mut prog)?;
        let _check_map = self.check_pass(&mut prog)?;

        let (mut wlk, map) = self.create_walker_new(&prog);
        let mut res_vec = Vec::new();
        for entry_decl in prog.entrys {
            let entry = map[&PredIdent::Pos(entry_decl.entry)];
            let res = wlk.run_loop(
                entry,
                entry_decl.iter_start,
                entry_decl.iter_end,
                entry_decl.iter_step,
            );
            res_vec.push(res);
        }
        Ok(res_vec)
    }
}

pub fn test_unsat_prog<P: AsRef<path::Path>>(prog_name: P) -> Result<(), ()> {
    let mut path = PathBuf::new();
    path.push("examples");
    path.push("unsat");
    path.push(prog_name);
    path.set_extension("pr");
    let src = fs::read_to_string(path).map_err(|_err| ())?;
    let mut log = io::empty();
    let mut pipe = Pipeline::new(&src, &mut log);
    let res_vec = pipe.test_prog()?;
    assert!(res_vec.iter().any(|p| !*p));
    Ok(())
}

pub fn test_sat_prog<P: AsRef<path::Path>>(prog_name: P) -> Result<(), ()> {
    let mut path = PathBuf::new();
    path.push("examples");
    path.push("sat");
    path.push(prog_name);
    path.set_extension("pr");
    let src = fs::read_to_string(path).map_err(|_err| ())?;
    let mut log = io::empty();
    let mut pipe = Pipeline::new(&src, &mut log);
    let res_vec = pipe.test_prog()?;
    assert!(res_vec.iter().all(|p| *p));
    Ok(())
}
