use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use enderpy_python_parser::{Lexer, Parser};
use enderpy_python_type_checker::{
    build::BuildManager,
    build_source::BuildSource,
    project::find_project_root,
    settings::{ImportDiscovery, Settings},
};
use miette::{bail, IntoDiagnostic, Result};
use std::{fs, path::PathBuf};

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

fn symbols(path: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(path).into_diagnostic()?;
    let initial_source = BuildSource {
        path: path.to_owned(),
        module: String::from("test"),
        source,
        followed: false,
    };
    let dir_of_path = path.parent().unwrap();
    let python_executable = Some(get_python_executable()?);
    let settings = Settings {
        debug: false,
        root: dir_of_path.to_path_buf(),
        import_discovery: ImportDiscovery { python_executable },
    };

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
        .output()
        .into_diagnostic()?;
    let path = String::from_utf8(output.stdout).into_diagnostic()?;
    Ok(PathBuf::from(path))
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

fn check(path: &PathBuf) -> Result<()> {
    if path.is_dir() {
        bail!("Path must be a file");
    }
    let source = fs::read_to_string(path).into_diagnostic()?;
    let initial_source = BuildSource {
        path: path.to_owned(),
        module: String::from("test"),
        source,
        followed: false,
    };
    let root = find_project_root(path);
    let python_executable = Some(get_python_executable()?);
    let settings = Settings {
        debug: true,
        root: PathBuf::from(root),
        import_discovery: ImportDiscovery { python_executable },
    };
    let mut build_manager = BuildManager::new(vec![initial_source], settings);
    build_manager.type_check();

    for file_result in build_manager.get_result() {
        for err in file_result.diagnostics {
            println!(
                "{:#?}: line {}: {}",
                file_result.file.path(),
                err.range.start.line,
                err.body
            );
        }
    }

    Ok(())
}

fn watch() -> Result<()> {
    todo!()
}
