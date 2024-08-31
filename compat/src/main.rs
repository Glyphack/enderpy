use enderpy_python_parser::Lexer;
use miette::{IntoDiagnostic, Result};
use reqwest::blocking::get;
use std::fs;
use std::io::Cursor;
use std::path::Path;
use zip::ZipArchive;

use self::lexer_compat::{assert_tokens_eq, lex_python_source};
use self::parser_compat::python_parser_test_ast;

pub mod lexer_compat;
pub mod parser_compat;
pub mod runpython;

fn main() -> Result<()> {
    let url = "https://github.com/python/mypy/archive/refs/heads/master.zip";
    let repo_path = "mypy-master";

    // Download the ZIP file
    if !Path::new(repo_path).exists() {
        println!("Downloading repository...");
        let response = get(url).into_diagnostic()?;
        let mut zip =
            ZipArchive::new(Cursor::new(response.bytes().into_diagnostic()?)).into_diagnostic()?;

        // Extract the ZIP file
        println!("Extracting repository...");
        zip.extract(".").into_diagnostic()?;
    } else {
        println!("Repository already downloaded and extracted.");
    }

    // Get all Python files in the extracted repo
    let python_files = get_python_files(repo_path)?;

    // Run the compatibility tests on each Python file
    for file in python_files {
        run_compatibility_test(&file)?;
    }

    Ok(())
}

/// Recursively finds all Python files in the specified directory.
fn get_python_files(dir: &str) -> Result<Vec<String>> {
    let mut python_files = Vec::new();
    for entry in fs::read_dir(dir).into_diagnostic()? {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();

        if path.is_dir() {
            python_files.extend(get_python_files(path.to_str().unwrap())?);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("py") {
            python_files.push(path.to_str().unwrap().to_string());
        }
    }
    Ok(python_files)
}

/// Runs the compatibility test on a single Python file.
fn run_compatibility_test(file: &str) -> Result<()> {
    println!("Running compatibility test on {}", file);

    let source = fs::read_to_string(file).into_diagnostic()?;
    let mut lexer = Lexer::new(&source);
    let enderpy_tokens = lexer.lex();
    let python_tokens = lex_python_source(&source)?;

    assert_tokens_eq(python_tokens, enderpy_tokens, &lexer);
    // python_parser_test_ast(&vec![source.as_str()]);

    Ok(())
}
