use super::*;
use crate::driver::command::Pipeline;
use clap::Parser;

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    pub input: PathBuf,

    #[arg(short, long, value_name = "FILE")]
    pub output: Option<PathBuf>,

    #[arg(short, long, default_value_t = 10, value_name = "INT")]
    pub verbosity: u8,

    #[arg(long, default_value_t = false)]
    pub warn_as_err: bool,
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
        verbosity: 10,
        warn_as_err: true,
    };
    let res = run_pipline(&args)?;
    Ok(res)
}

pub fn run_pipline(args: &CliArgs) -> Result<Vec<usize>, io::Error> {
    let src = std::fs::read_to_string(&args.input)?;

    let mut pipe = Pipeline::new(args.clone());
    match pipe.run_pipline(&src) {
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
