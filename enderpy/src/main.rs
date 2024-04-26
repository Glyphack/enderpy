use std::{
    fs,
    path::{Path, PathBuf},
};

use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use enderpy_python_parser::{Lexer, Parser};
use enderpy_python_type_checker::{
    build::BuildManager, build_source::BuildSource, find_project_root, settings::Settings,
};
use miette::{bail, IntoDiagnostic, Result};

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

fn symbols(path: &Path) -> Result<()> {
    let initial_source = BuildSource::from_path(path.to_path_buf(), false).unwrap();
    let dir_of_path = path.parent().unwrap();
    let typeshed_path = get_typeshed_path()?;
    let settings = Settings { typeshed_path };
    let manager = BuildManager::new(vec![initial_source], settings);
    let root = find_project_root(dir_of_path);
    manager.build(root);

    let module = manager.get_state(path.to_str().expect(""));
    println!("{}", module.module_name());
    println!("{}", module.get_symbol_table());

    Ok(())
}

fn get_python_executable() -> Result<PathBuf> {
    let output = std::process::Command::new("python")
        .arg("-c")
        .arg("import sys; print(sys.executable)")
        .output()
        .into_diagnostic()?;
    let path = String::from_utf8(output.stdout).into_diagnostic()?;
    Ok(PathBuf::from(path))
}

fn get_typeshed_path() -> Result<PathBuf> {
    // imagine the path is in the same directory as user ran this command
    let path = std::env::current_dir().into_diagnostic()?;
    Ok(path.join("typeshed"))
}

fn tokenize(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;
    let mut lexer = Lexer::new(&source);
    let tokens = enderpy_python_parser::utils::lex(&mut lexer);
    for token in tokens {
        println!("{}", token);
    }
    Ok(())
}

fn parse(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;
    let file_path = file.to_str().unwrap_or("");
    let mut parser = Parser::new(source, file_path.into());
    let ast = parser.parse();
    for err in parser.errors {
        println!("{:#?}", err);
    }
    println!("{:#?}", ast);
    Ok(())
}

fn check(path: &Path) -> Result<()> {
    if path.is_dir() {
        bail!("Path must be a file");
    }
    let initial_source = BuildSource::from_path(path.to_path_buf(), false).unwrap();
    let root = find_project_root(path);
    let _python_executable = Some(get_python_executable()?);
    let typeshed_path = get_typeshed_path()?;
    let settings = Settings { typeshed_path };
    let build_manager = BuildManager::new(vec![initial_source], settings);
    build_manager.build(root);
    build_manager.type_check();

    for (path, errors) in build_manager.diagnostics {
        for err in errors {
            println!("{:#?}: line {}: {}", path, err.range.start.line, err.body);
        }
    }

    Ok(())
}

fn watch() -> Result<()> {
    todo!()
}
