use std::path::PathBuf;

use clap::{Parser, Subcommand};

/// Enderpy CLI
#[derive(Parser)]
#[command(name = "Enderpy", author, version, about)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Print lexer tokens
    Tokenize {
        /// Path to source file
        file: PathBuf,
    },
    /// Print abstract syntax tree
    Parse {
        /// Path to source file
        file: PathBuf,
    },
    /// Type check
    Check { path: PathBuf },
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
