use super::args::CliArgs;
use super::diagnostic::{DiagLevel, Diagnostic};
use super::*;
use crate::cli::replay::ReplayWriter;
use crate::{interp, logic, syntax, tych};

pub struct PipeIO {
    pub output: Box<dyn Write>,
    pub stat: Box<dyn Write>,
    pub prog: Box<dyn Write>,
}

impl PipeIO {
    pub fn empty() -> PipeIO {
        PipeIO {
            output: Box::new(io::empty()),
            stat: Box::new(io::empty()),
            prog: Box::new(io::empty()),
        }
    }
}

pub struct Pipeline<'arg> {
    pub args: &'arg CliArgs,
    pub diags: Vec<Diagnostic>,
}

impl<'arg> Pipeline<'arg> {
    pub fn new(args: &'arg CliArgs) -> Pipeline<'arg> {
        Pipeline {
            args,
            diags: Vec::new(),
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

    pub fn run_pipline(
        &mut self,
        src: &str,
        pipe_io: &mut PipeIO,
    ) -> Result<Vec<usize>, io::Error> {
        let mut prog = self.parse_program(src)?;

        self.rename_pass(&mut prog)?;

        self.check_pass(&mut prog)?;

        let prog = self.compile_pass(&prog);

        writeln!(pipe_io.prog, "{}", prog).unwrap();

        let res = self.run_backend(&prog, pipe_io);
        Ok(res)
    }

    pub fn parse_program(&mut self, src: &str) -> Result<syntax::ast::Program, io::Error> {
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

    pub fn compile_pass(&mut self, prog: &syntax::ast::Program) -> logic::ast::Program {
        let mut prog = logic::compile::compile_pass(prog);

        logic::elaborate::elaborate_pass(&mut prog);

        prog
    }

    pub fn run_backend(&self, prog: &logic::ast::Program, pipe_io: &mut PipeIO) -> Vec<usize> {
        let mut res_vec = Vec::new();

        let mut runner = interp::runner::RunnerState::new(
            prog,
            pipe_io,
            self.args.solver,
            self.args.heuristic,
            self.args.debug_mode,
        );

        for query_decl in &prog.querys {
            for param in query_decl.params.iter() {
                runner.config_set_param(param);
            }
            let res = runner.run_iddfs_loop(query_decl.entry);
            res_vec.push(res);
        }
        res_vec
    }
}

fn create_dump_dir(src_path: &PathBuf) -> Result<PathBuf, io::Error> {
    use std::fs;

    if src_path.extension().and_then(|ext| ext.to_str()) != Some("pr") {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("source file extension is not \".pr\"!: {:?}", src_path),
        ));
    }

    if !src_path.exists() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("file \"{:?}\" doesn't exist!", src_path),
        ));
    }

    if !src_path.is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("path \"{:?}\" exists, but it is not a file!", src_path),
        ));
    }

    let file_stem = src_path.file_stem().unwrap().to_os_string();

    let mut dir_path = src_path.clone();
    dir_path.pop();
    dir_path.push(file_stem);

    if !dir_path.exists() {
        fs::create_dir(&dir_path)?;
    } else if !dir_path.is_dir() {
        return Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            format!("path \"{:?}\" exist, but it is not a directory!", dir_path),
        ));
    }

    Ok(dir_path)
}

pub fn run_pipline(args: &CliArgs) -> Result<Vec<usize>, io::Error> {
    let src_path = PathBuf::from(&args.input);

    let mut pipe_io = PipeIO::empty();
    if args.dump_file {
        let dir_path = create_dump_dir(&src_path)?;

        let output = File::create(dir_path.join("output.txt"))?;
        if args.show_output {
            pipe_io.output = Box::new(ReplayWriter::replay_stdout(output));
        } else {
            pipe_io.output = Box::new(output);
        }

        let stat = File::create(dir_path.join("stat.txt"))?;
        if args.show_stat {
            pipe_io.stat = Box::new(ReplayWriter::replay_stdout(stat));
        } else {
            pipe_io.stat = Box::new(stat);
        }

        let prog = File::create(dir_path.join("prog.txt"))?;
        if args.show_prog {
            pipe_io.prog = Box::new(ReplayWriter::replay_stdout(prog));
        } else {
            pipe_io.prog = Box::new(prog);
        }
    } else {
        if args.show_output {
            pipe_io.output = Box::new(io::stdout());
        }

        if args.show_stat {
            pipe_io.stat = Box::new(io::stdout());
        }

        if args.show_prog {
            pipe_io.prog = Box::new(io::stdout());
        }
    }

    let src = std::fs::read_to_string(src_path)?;
    let mut pipe = Pipeline::new(args);
    match pipe.run_pipline(&src, &mut pipe_io) {
        Ok(res) => {
            for diag in pipe.diags.into_iter() {
                eprintln!("{}", diag.report(&src, args.verbosity));
            }
            Ok(res)
        }
        Err(err) => {
            for diag in pipe.diags.into_iter() {
                eprintln!("{}", diag.report(&src, args.verbosity));
            }
            Err(err)
        }
    }
}

pub fn run_cli() -> Result<Vec<usize>, io::Error> {
    let args = args::parse_cli_args();
    let res = pipeline::run_pipline(&args)?;
    Ok(res)
}

pub fn run_cli_test(prog_name: PathBuf) -> Result<Vec<usize>, io::Error> {
    let args = args::get_test_cli_args(prog_name);
    let res = pipeline::run_pipline(&args)?;
    Ok(res)
}
