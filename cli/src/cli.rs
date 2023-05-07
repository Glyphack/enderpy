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
    Tokenize,
    /// Print abstract syntax tree
    Parse,
    /// Type check
    Check,
    /// Watch changes to type check
    Watch,
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert()
}
