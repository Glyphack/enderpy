use anyhow::{anyhow, Result};
use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use parser::{token, Lexer, Parser};
use std::{fs, path::PathBuf};
use typechecker::{
    build::{BuildManager, BuildSource},
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
        Commands::Symbols { path } => symbols(path),
    }
}

fn symbols(path: &PathBuf) -> std::result::Result<(), anyhow::Error> {
    let source = fs::read_to_string(path)?;
    let initial_source = BuildSource {
        path: path.to_owned(),
        module: String::from("test"),
        source,
        followed: false,
    };
    let dir_of_path = path.parent().unwrap();
    let python_executable = get_python_executable()?;
    let settings = Settings { debug: true, root: dir_of_path.to_path_buf(), import_discovery: ImportDiscovery { python_executable } };

    let mut manager = BuildManager::new(vec![initial_source], settings);
    manager.build();

    for (name, module) in manager.modules.iter() {
        println!("{}", name);
        println!("{}", module.get_symbol_table());
    }

    Ok(())
}

fn get_python_executable() -> Result<PathBuf> {
    let output = std::process::Command::new("python")
        .arg("-c")
        .arg("import sys; print(sys.executable)")
        .output()?;
    let path = String::from_utf8(output.stdout)?;
    Ok(PathBuf::from(path))
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
        module: String::from("test"),
        source,
        followed: false,
    };
    let root = path
        .ancestors()
        .find(|p| p.join("__init__.py").exists())
        .ok_or_else(|| anyhow!("could not find root of project"))?;
    let python_executable = get_python_executable()?;
    let settings = Settings { debug: true, root: PathBuf::from(root), import_discovery: ImportDiscovery { python_executable } };
    let mut build_manager = BuildManager::new(vec![initial_source], settings);
    build_manager.build();

    // TODO: check the build_manager errors and show to user
    Ok(())
}

fn watch() -> Result<()> {
    todo!()
}
