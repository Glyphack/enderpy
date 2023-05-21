use anyhow::Result;
use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use parser::{token, Lexer, Parser};
use std::{fs, path::PathBuf};

mod cli;

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize { file } => tokenize(file),
        Commands::Parse { file } => parse(file),
        Commands::Check => check(),
        Commands::Watch => watch(),
    }
}

fn tokenize(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file)?;
    let mut lexer = Lexer::new(&source);
    let mut tokens = Vec::new();
    while let Ok(token) = lexer.next_token() {
        tokens.push(token.clone());
        if token.kind == token::Kind::Eof {
            break;
        }
    }
    println!("{:#?}", tokens);
    Ok(())
}

fn parse(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file)?;
    let mut parser = Parser::new(source);
    let ast = parser.parse();
    println!("{:#?}", ast);
    Ok(())
}

fn check() -> Result<()> {
    todo!()
}

fn watch() -> Result<()> {
    todo!()
}
