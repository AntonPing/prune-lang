use super::*;
use clap::{Parser, ValueEnum};

#[derive(ValueEnum, Copy, Clone, Debug, PartialEq, Eq)]
pub enum Solver {
    Z3,
    CVC5,
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

    #[arg(long, default_value = "no-smt", value_name = "SOLVER")]
    pub solver: Solver,

    #[arg(short, long, default_value_t = 10, value_name = "INT")]
    pub verbosity: u8,

    #[arg(long, default_value_t = false)]
    pub mute_output: bool,

    #[arg(long, default_value_t = false)]
    pub mute_stat_log: bool,

    #[arg(long, default_value_t = false)]
    pub warn_as_err: bool,
}

pub fn parse_cli_args() -> CliArgs {
    CliArgs::parse()
}

pub fn get_test_cli_args(prog_name: PathBuf) -> CliArgs {
    CliArgs {
        input: prog_name,
        output: None,
        stat_log: None,
        solver: Solver::Z3,
        verbosity: 10,
        mute_output: true,
        mute_stat_log: true,
        warn_as_err: true,
    }
}
