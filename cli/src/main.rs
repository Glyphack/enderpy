use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use parser::{token, Lexer, Parser};
use std::{fs, path::PathBuf};

mod cli;

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize { file } => tokenize(file),
        Commands::Parse { file } => parse(file),
        Commands::Check => check(),
        Commands::Watch => watch(),
    }
}

fn tokenize(file: &PathBuf) {
    let source = fs::read_to_string(file).unwrap();
    let mut lexer = Lexer::new(&source);
    let mut tokens = Vec::new();
    while let Ok(token) = lexer.next_token() {
        tokens.push(token.clone());
        if token.kind == token::Kind::Eof {
            break;
        }
    }
    println!("{:#?}", tokens);
}

fn parse(file: &PathBuf) {
    let source = fs::read_to_string(file).unwrap();
    let mut parser = Parser::new(source);
    let ast = parser.parse();
    println!("{:#?}", ast);
}

fn check() {
    todo!()
}

fn watch() {
    todo!()
}
