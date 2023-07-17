use anyhow::{anyhow, Result};
use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use parser::{token, Lexer, Parser};
use std::{fs, path::PathBuf};
use typechecker::{
    build::{BuildManager, BuildSource},
    semantic_analyzer::SemanticAnalyzer,
    settings::{ImportDiscovery, Settings},
};

mod cli;

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize { file } => tokenize(file),
        Commands::Parse { file } => parse(file),
        Commands::Check { path } => check(path),
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

fn check(path: &PathBuf) -> Result<()> {
    if path.is_dir() {
        return Err(anyhow!("typechecking a path is not supported"));
    }
    let source = fs::read_to_string(path)?;
    let initial_source = BuildSource {
        path: path.to_owned(),
        module: None,
        source,
        followed: false,
    };
    let options = Settings {
        debug: true,
        import_discovery: ImportDiscovery {
            python_executable: PathBuf::from(
                // TODO: discover executable
                // Python has sys.executable to do this
                "'/Users/shooshyari/.pyenv/versions/3.11.3/bin/python'",
            ),
        },
    };
    let semantic_analyzer = SemanticAnalyzer {};
    let build_manager = BuildManager::new(vec![initial_source], semantic_analyzer, options);
    build_manager.build();

    // TODO: check the build_manager errors and show to user
    Ok(())
}

fn watch() -> Result<()> {
    todo!()
}
