use std::path::PathBuf;

use clap::{Parser, Subcommand};

/// Enderpy CLI
#[derive(Parser)]
#[command(name = "Enderpy", author, version, about)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
    #[arg(short, long)]
    pub file: Option<PathBuf>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Print lexer tokens
    Tokenize {},
    /// Print abstract syntax tree
    Parse {
        /// Path to source file
        file: PathBuf,
    },
    /// Type check
    Check { path: PathBuf },
    /// Translate to C++
    Translate { path: PathBuf },
    ///  Symbol table
    Symbols { path: PathBuf },

    /// Watch changes to type check
    Watch,
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert()
}
