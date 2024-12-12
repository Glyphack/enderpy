use std::{
    fs::{self, File},
    io::{self, Read},
    path::{Path, PathBuf},
    sync::Arc,
};

use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use enderpy_python_parser::{get_row_col_position, parser::parser::Parser, Lexer};
use enderpy_python_type_checker::{build::BuildManager, find_project_root, settings::Settings};
use corepy_python_translator::translator::CppTranslator;
use miette::{bail, IntoDiagnostic, Result};

mod cli;

fn main() -> Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Tokenize {} => tokenize(),
        Commands::Parse { file } => parse(file),
        Commands::Check { path } => check(path),
        Commands::Translate { path } => translate(path),
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

    println!("{}", manager.get_symbol_table_by_path(path));

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
                while path.ends_with('\n') || path.ends_with('\r') {
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

fn tokenize() -> Result<()> {
    let cli = Cli::parse();
    let mut source = String::new();
    match cli.file {
        Some(path) => {
            File::open(path)
                .into_diagnostic()?
                .read_to_string(&mut source)
                .into_diagnostic()?;
        }
        None => {
            io::stdin().read_to_string(&mut source).into_diagnostic()?;
        }
    }
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex();
    for token in tokens {
        let (start_line_num, start_line_column, end_line_num, end_line_column) =
            get_row_col_position(token.start, token.end, &lexer.line_starts);
        println!(
            "{}-{}, {}-{}:   {} {} {}",
            start_line_num,
            start_line_column,
            end_line_num,
            end_line_column,
            token.kind,
            token.start,
            token.end,
        );
    }
    Ok(())
}

fn parse(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;
    let mut parser = Parser::new(&source);
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
    let id = build_manager.paths.get(path).unwrap();
    let file = build_manager.files.get(&id).unwrap();
    let checker = build_manager.type_check(path, &file);
    print!("{}", checker.dump_types());

    Ok(())
}

fn translate(path: &Path) -> Result<()> {
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
    let id = build_manager.paths.get(path).unwrap();
    let file = build_manager.files.get(&id).unwrap();
    let checker = Arc::new(build_manager.type_check(path, &file));
    let mut translator = CppTranslator::new(checker.clone(), &file);
    translator.translate();
    println!("{:?}", file.tree);
    println!("====");
    println!("{}", translator.output);
    println!("====");
    print!("{}", checker.clone().dump_types());
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
