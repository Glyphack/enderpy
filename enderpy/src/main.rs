use anyhow::{Result, bail};
use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use enderpy_python_parser::{token, Lexer, Parser};
use std::{fs, path::PathBuf};
use enderpy_python_type_checker::{
    build::{BuildManager, BuildSource},
    settings::{ImportDiscovery, Settings}, project::find_project_root,
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
    let python_executable = Some( get_python_executable()? );
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
    for token in tokens {
        println!("{}", token);
    }
    Ok(())
}

fn parse(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file)?;
    let file_path = match file.to_str() {
        Some(path) => path,
        None =>  "",
    };
    let mut parser = Parser::new(source, file_path.into());
    let ast = parser.parse();
    println!("{:#?}", ast);
    Ok(())
}

fn check(path: &PathBuf) -> Result<()> {
    if path.is_dir() {
        bail!("Path must be a file");
    }
    let source = fs::read_to_string(path)?;
    let initial_source = BuildSource {
        path: path.to_owned(),
        module: String::from("test"),
        source,
        followed: false,
    };
    let root = find_project_root(path);
    let python_executable = Some( get_python_executable()? );
    let settings = Settings { debug: true, root: PathBuf::from(root), import_discovery: ImportDiscovery { python_executable } };
    let mut build_manager = BuildManager::new(vec![initial_source], settings);
    build_manager.type_check();

    for err in build_manager.get_errors() {
        println!("{:#?}", err);
    }

    Ok(())
}

fn watch() -> Result<()> {
    todo!()
}
