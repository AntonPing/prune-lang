use super::*;
use clap::{Parser, ValueEnum};

#[derive(ValueEnum, Copy, Clone, Debug, PartialEq, Eq)]
pub enum SmtBackend {
    Z3Inc,
    Z3Sq,
    CVC5Inc,
    CVC5Sq,
    NoSmt,
}

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    #[arg(value_enum)]
    pub backend: SmtBackend,

    pub input: PathBuf,

    #[arg(short, long, value_name = "FILE")]
    pub output: Option<PathBuf>,

    #[arg(long, value_name = "FILE")]
    pub stat_log: Option<PathBuf>,

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
        verbosity: 10,
        backend: SmtBackend::Z3Inc,
        mute_output: true,
        mute_stat_log: true,
        warn_as_err: true,
    }
}
