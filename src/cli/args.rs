use super::*;
use clap::{Parser, ValueEnum};

#[derive(ValueEnum, Copy, Clone, Debug, PartialEq, Eq)]
pub enum Solver {
    Z3,
    CVC5,
    NoSmt,
}

#[derive(ValueEnum, Copy, Clone, Debug, PartialEq, Eq)]
pub enum Heuristic {
    LeftBiased,
    Interleave,
    StructRecur,
    LookAhead,
    Random,
}

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    pub input: PathBuf,

    #[arg(long, default_value = "no-smt", value_name = "SOLVER")]
    pub solver: Solver,

    #[arg(long, default_value = "interleave", value_name = "HEURISTIC")]
    pub heuristic: Heuristic,

    #[arg(short, long, default_value_t = 10, value_name = "INT")]
    pub verbosity: u8,

    #[arg(long, default_value_t = true)]
    pub dump_file: bool,

    #[arg(long, default_value_t = true)]
    pub debug_mode: bool,

    #[arg(long, default_value_t = false)]
    pub show_output: bool,

    #[arg(long, default_value_t = true)]
    pub show_stat: bool,

    #[arg(long, default_value_t = false)]
    pub show_prog: bool,

    #[arg(long, default_value_t = false)]
    pub warn_as_err: bool,
}

pub fn parse_cli_args() -> CliArgs {
    CliArgs::parse()
}

pub fn get_test_cli_args(prog_name: PathBuf) -> CliArgs {
    CliArgs {
        input: prog_name,
        solver: Solver::Z3,
        heuristic: Heuristic::LookAhead,
        verbosity: 10,
        dump_file: false,
        debug_mode: false,
        show_output: false,
        show_stat: false,
        show_prog: false,
        warn_as_err: true,
    }
}
