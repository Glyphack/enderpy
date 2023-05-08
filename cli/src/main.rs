use clap::Parser;
use cli::{Cli, Commands};

mod cli;

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize => todo!(),
        Commands::Parse => todo!(),
        Commands::Check => todo!(),
        Commands::Watch => todo!(),
    }
}
