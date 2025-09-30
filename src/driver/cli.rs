use crate::driver::command;
use clap::Parser;
use std::io;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    pub input: PathBuf,

    #[arg(short, long, value_name = "FILE")]
    pub output: Option<PathBuf>,

    #[arg(short, long, default_value_t = 10, value_name = "INT")]
    pub verbosity: u8,
}

pub fn run_cli() -> Result<Vec<usize>, io::Error> {
    let args = CliArgs::parse();

    let res = run_pipline(&args)?;

    Ok(res)
}

pub enum TestMode {
    Unsat,
    Sat,
    NoTest,
}

pub fn run_cli_test(prog_name: PathBuf, mode: TestMode) -> Result<(), io::Error> {
    let args = CliArgs {
        input: prog_name,
        output: None,
        verbosity: 10,
    };
    let res = run_pipline(&args)?;
    match mode {
        TestMode::Unsat => {
            assert!(res.iter().all(|p| *p == 0));
        }
        TestMode::Sat => {
            assert!(res.iter().any(|p| *p > 0));
        }
        TestMode::NoTest => {
            // do nothing
        }
    }
    Ok(())
}

pub fn run_pipline(args: &CliArgs) -> Result<Vec<usize>, io::Error> {
    let src = command::read_file(&args.input)?;

    let mut prog = command::parse_program(&src).map_err(|diags| {
        for diag in diags {
            eprintln!("{}", diag.report(&src, args.verbosity));
        }
        io::Error::new(io::ErrorKind::Other, "failed to parse program!")
    })?;

    command::rename_pass(&mut prog).map_err(|diags| {
        for diag in diags {
            eprintln!("{}", diag.report(&src, args.verbosity));
        }
        io::Error::new(io::ErrorKind::Other, "found error in varible binding!")
    })?;

    command::check_pass(&mut prog).map_err(|diags| {
        for diag in diags {
            eprintln!("{}", diag.report(&src, args.verbosity));
        }
        io::Error::new(io::ErrorKind::Other, "failed to check types!")
    })?;

    let res = command::run_backend(&prog);
    Ok(res)
}
