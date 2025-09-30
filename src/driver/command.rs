use super::diagnostic::Diagnostic;
use crate::*;
use std::{io, path};

pub fn read_file(input: &path::PathBuf) -> Result<String, io::Error> {
    std::fs::read_to_string(input)
}

pub fn parse_program<'src>(src: &'src str) -> Result<syntax::ast::Program, Vec<Diagnostic>> {
    let (prog, errs) = syntax::parser::parse_program(src);
    if errs.is_empty() {
        Ok(prog)
    } else {
        Err(errs.into_iter().map(|err| err.into()).collect())
    }
}

pub fn rename_pass(prog: &mut syntax::ast::Program) -> Result<(), Vec<Diagnostic>> {
    let errs = tych::rename::rename_pass(prog);
    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs.into_iter().map(|err| err.into()).collect())
    }
}

pub fn check_pass(prog: &mut syntax::ast::Program) -> Result<(), Vec<Diagnostic>> {
    let errs = tych::check::check_pass(prog);
    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs.into_iter().map(|err| err.into()).collect())
    }
}

pub fn run_backend(prog: &syntax::ast::Program) -> Vec<usize> {
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
