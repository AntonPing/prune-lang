use crate::driver::diagnostic::Diagnostic;
use crate::syntax::{self, ast};
use crate::tych;
use crate::walker;

use std::fs;
use std::path::{self, PathBuf};

pub struct Pipeline<'src> {
    src: &'src str,
    verbosity: u8,
    diags: Vec<Diagnostic>,
}

type PipeResult<T> = Result<T, ()>;

impl<'src> Pipeline<'src> {
    pub fn new(src: &'src str) -> Pipeline<'src> {
        Pipeline {
            src,
            verbosity: 10,
            diags: Vec::new(),
        }
    }

    pub fn check_diag(&mut self) -> PipeResult<()> {
        if self.diags.is_empty() {
            return Ok(());
        }
        for diag in &self.diags {
            eprintln!("{}", diag.report(self.src, 10));
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
                eprintln!("{}", diag.report(self.src, self.verbosity));
            }
            Err(())
        }
    }

    pub fn rename_pass(&mut self, prog: &mut ast::Program) -> PipeResult<()> {
        let errs = tych::rename::rename_pass(prog);
        if errs.is_empty() {
            Ok(())
        } else {
            for err in errs {
                let diag: Diagnostic = err.into();
                eprintln!("{}", diag.report(self.src, self.verbosity));
            }
            Err(())
        }
    }

    pub fn check_pass(&mut self, prog: &mut ast::Program) -> PipeResult<()> {
        let errs = tych::check::check_pass(prog);
        if errs.is_empty() {
            Ok(())
        } else {
            for err in errs {
                let diag: Diagnostic = err.into();
                eprintln!("{}", diag.report(self.src, self.verbosity));
            }
            Err(())
        }
    }

    pub fn test_prog(&mut self) -> Result<Vec<usize>, ()> {
        let mut prog = self.parse_program()?;
        self.rename_pass(&mut prog)?;
        self.check_pass(&mut prog)?;

        let prog = crate::logic::transform::logic_translation(&prog);
        let map = crate::tych::elab::elab_pass(&prog);
        let dict = crate::walker::compile::compile_dict(&prog, &map);
        let mut wlk = walker::walker::Walker::new(&dict);

        let mut res_vec = Vec::new();
        for query_decl in prog.querys {
            wlk.config_reset_default();
            for param in query_decl.params.iter() {
                wlk.config_set_param(param);
            }
            let res = wlk.run_loop(query_decl.entry);
            res_vec.push(res);
        }
        Ok(res_vec)
    }
}

pub fn test_sym_exec_good_prog<P: AsRef<path::Path>>(prog_name: P) -> Result<(), ()> {
    let mut path = PathBuf::new();
    path.push("examples");
    path.push("sym_exec_good");
    path.push(prog_name);
    path.set_extension("pr");
    let src = fs::read_to_string(path).map_err(|_err| ())?;
    let mut pipe = Pipeline::new(&src);
    let res_vec = pipe.test_prog()?;
    assert!(res_vec.iter().all(|p| *p == 0));
    Ok(())
}

pub fn test_sym_exec_bad_prog<P: AsRef<path::Path>>(prog_name: P) -> Result<(), ()> {
    let mut path = PathBuf::new();
    path.push("examples");
    path.push("sym_exec_bad");
    path.push(prog_name);
    path.set_extension("pr");
    let src = fs::read_to_string(path).map_err(|_err| ())?;
    let mut pipe = Pipeline::new(&src);
    let res_vec = pipe.test_prog()?;
    assert!(res_vec.iter().any(|p| *p > 0));
    Ok(())
}
