use std::{
    fs, io,
    path::{Path, PathBuf},
};

use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use enderpy_python_parser::{Lexer, Parser};
use enderpy_python_type_checker::{build::BuildManager, find_project_root, settings::Settings};
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
    let dir_of_path = path.parent().unwrap();
    let typeshed_path = get_typeshed_path()?;
    let settings = Settings::from_typeshed(typeshed_path);
    let manager = BuildManager::new(settings);

    let root = find_project_root(dir_of_path);
    manager.build(root);
    manager.build_one(root, path);

    let module = manager.get_state(path);
    println!("{}", module.module_name());
    println!("{}", module.get_symbol_table());

    Ok(())
}

fn get_python_executable() -> Result<PathBuf> {
    let possible_executables = ["python3", "python"];
    for executable_name in possible_executables {
        let res = std::process::Command::new(executable_name)
            .arg("-c")
            .arg("import sys; print(sys.executable)")
            .output();
        match res {
            Ok(output) => {
                let mut path = String::from_utf8(output.stdout).into_diagnostic()?;
                // Like calling trim but I didn't want to re-allocate the str slice
                while path.ends_with("\n") || path.ends_with("\r") {
                    path.pop();
                }
                return Ok(PathBuf::from(path));
            }
            Err(e) => {
                if e.kind() != io::ErrorKind::NotFound {
                    bail!("Unknown error when looking for python executable: {e}");
                }
            }
        }
    }

    bail!("Failed to find Python executable.");
}

fn get_typeshed_path() -> Result<PathBuf> {
    // imagine the path is in the same directory as user ran this command
    let path = std::env::current_dir().into_diagnostic()?;
    Ok(path.join("typeshed"))
}

fn tokenize(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex();
    for token in tokens {
        let (start_line_num, start_line_offset) =
            match lexer.line_starts.binary_search(&token.start) {
                Ok(idx) => (idx, lexer.line_starts[idx]),
                Err(idx) => (idx - 1, lexer.line_starts[idx - 1]),
            };
        let start_line_column = token.start - start_line_offset;
        let (end_line_num, end_line_offset) = match lexer.line_starts.binary_search(&token.end) {
            Ok(idx) => (idx, lexer.line_starts[idx]),
            Err(idx) => (idx - 1, lexer.line_starts[idx - 1]),
        };
        let end_line_column = token.end - end_line_offset;
        println!(
            "{}-{}, {}-{}:   {} {} {} {}",
            start_line_num,
            start_line_column,
            end_line_num,
            end_line_column,
            token.kind,
            token.value,
            token.start,
            token.end,
        );
    }
    Ok(())
}

fn parse(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;
    let file_path = file.to_str().unwrap_or("");
    let mut parser = Parser::new(source, file_path.into());
    let ast = parser.parse();
    println!("{:#?}", ast);
    Ok(())
}

fn check(path: &Path) -> Result<()> {
    if path.is_dir() {
        bail!("Path must be a file");
    }
    let root = find_project_root(path);
    let python_executable = Some(get_python_executable()?);
    let typeshed_path = get_typeshed_path()?;
    let settings = Settings {
        typeshed_path,
        python_executable,
    };
    let build_manager = BuildManager::new(settings);
    build_manager.build(root);
    build_manager.build_one(root, path);
    build_manager.type_check(path);

    if build_manager.diagnostics.is_empty() {
        println!("zero errors");
    }

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

#[cfg(test)]
mod tests {
    use crate::get_python_executable;

    #[test]
    fn test_get_python_successfully() {
        let executable = get_python_executable().expect("No python executable found!");
        // Makes sure the python executable is working, without relying on a specific filename
        assert!(executable.is_file());
        let output = std::process::Command::new(executable)
            .arg("-c")
            .arg("print('Working')")
            .output()
            .unwrap();
        assert_eq!(String::from_utf8(output.stdout).unwrap().trim(), "Working");
    }
}
