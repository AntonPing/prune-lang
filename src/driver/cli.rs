use super::*;
use crate::driver::command::Pipeline;
use clap::{Parser, ValueEnum};

#[derive(ValueEnum, Copy, Clone, Debug, PartialEq, Eq)]
pub enum SmtBackend {
    Z3,
    Z3Single,
    CVC5,
    CVC5Single,
    NoSmt,
}

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    pub input: PathBuf,

    #[arg(short, long, value_name = "FILE")]
    pub output: Option<PathBuf>,

    #[arg(long, value_name = "FILE")]
    pub stat_log: Option<PathBuf>,

    #[arg(short, long, default_value_t = 10, value_name = "INT")]
    pub verbosity: u8,

    #[arg(long, value_enum)]
    pub backend: SmtBackend,

    #[arg(long, default_value_t = false)]
    pub mute_output: bool,

    #[arg(long, default_value_t = false)]
    pub mute_stat_log: bool,

    #[arg(long, default_value_t = false)]
    pub warn_as_err: bool,
}

pub struct PipeIO {
    pub output: Box<dyn Write>,
    pub stat_log: Box<dyn Write>,
}

impl PipeIO {
    pub fn empty() -> PipeIO {
        PipeIO {
            output: Box::new(io::empty()),
            stat_log: Box::new(io::empty()),
        }
    }

    pub fn set_output(&mut self, pipe: Box<dyn Write>) {
        self.output = pipe;
    }
}

pub fn run_cli() -> Result<Vec<usize>, io::Error> {
    let args = CliArgs::parse();
    let res = run_pipline(&args)?;
    Ok(res)
}

pub fn run_cli_test(prog_name: PathBuf) -> Result<Vec<usize>, io::Error> {
    let args = CliArgs {
        input: prog_name,
        output: None,
        stat_log: None,
        verbosity: 10,
        backend: SmtBackend::Z3,
        mute_output: true,
        mute_stat_log: true,
        warn_as_err: true,
    };
    let res = run_pipline(&args)?;
    Ok(res)
}

pub fn run_pipline(args: &CliArgs) -> Result<Vec<usize>, io::Error> {
    let src = std::fs::read_to_string(&args.input)?;

    let mut pipe_io = PipeIO::empty();

    if args.mute_output {
        pipe_io.output = Box::new(std::io::empty());
    } else if let Some(path) = &args.output {
        pipe_io.output = Box::new(File::create(path)?);
    } else {
        pipe_io.output = Box::new(std::io::stdout());
    }

    if args.mute_stat_log {
        pipe_io.stat_log = Box::new(std::io::empty());
    } else if let Some(path) = &args.stat_log {
        pipe_io.stat_log = Box::new(File::create(path)?);
    } else {
        pipe_io.stat_log = Box::new(std::io::stdout());
    }

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
