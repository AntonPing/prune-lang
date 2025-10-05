use super::cli::CliArgs;
use super::diagnostic::{DiagLevel, Diagnostic};
use super::*;
use crate::{syntax, tych, walker};

pub struct Pipeline {
    pub args: CliArgs,
    pub diags: Vec<Diagnostic>,
}

impl Pipeline {
    pub fn new(args: CliArgs) -> Pipeline {
        Pipeline {
            diags: Vec::new(),
            args,
        }
    }

    fn emit_diags<D: Into<Diagnostic>>(&mut self, diags: Vec<D>) -> bool {
        let mut flag = false;
        for diag in diags.into_iter() {
            let diag = diag.into();
            if diag.level == DiagLevel::Error
                || (self.args.warn_as_err && diag.level == DiagLevel::Warn)
            {
                flag = true;
            }
            self.diags.push(diag);
        }
        flag
    }

    pub fn run_pipline<'src>(&mut self, src: &'src str) -> Result<Vec<usize>, io::Error> {
        let mut prog = self.parse_program(&src)?;

        self.rename_pass(&mut prog)?;

        self.check_pass(&mut prog)?;

        let res = self.run_backend(&prog);
        Ok(res)
    }

    pub fn parse_program<'src>(
        &mut self,
        src: &'src str,
    ) -> Result<syntax::ast::Program, io::Error> {
        let (prog, errs) = syntax::parser::parse_program(src);
        if self.emit_diags(errs) {
            return Err(io::Error::other("failed to parse program!"));
        }
        Ok(prog)
    }

    pub fn rename_pass(&mut self, prog: &mut syntax::ast::Program) -> Result<(), io::Error> {
        let errs = tych::rename::rename_pass(prog);
        if self.emit_diags(errs) {
            return Err(io::Error::other("failed in binding analysis pass!"));
        }
        Ok(())
    }

    pub fn check_pass(&mut self, prog: &mut syntax::ast::Program) -> Result<(), io::Error> {
        let errs = tych::check::check_pass(prog);
        if self.emit_diags(errs) {
            return Err(io::Error::other("failed in type checking pass!"));
        }
        Ok(())
    }

    pub fn run_backend(&self, prog: &syntax::ast::Program) -> Vec<usize> {
        let prog = crate::logic::transform::logic_translation(&prog);
        let map = crate::tych::elab::elab_pass(&prog);
        let dict = crate::walker::block::compile_dict(&prog, &map);
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
        res_vec
    }
}
