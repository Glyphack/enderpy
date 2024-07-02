use std::process::{Child, Command, Stdio};
use std::path::{Path, PathBuf};
use miette::{bail, IntoDiagnostic, Result, WrapErr};
use which::which;

pub fn spawn_python_script_command<S, P>(script_path: S, args: Vec<&str>, python_path: P) -> Result<Child>
where S: AsRef<Path>, P: AsRef<Path> {
    let script_path = if script_path.as_ref().is_relative() {
        let enderpy_dir = enderpy_root_path()?;
        enderpy_dir.join(script_path)
    } else {
        script_path.as_ref().to_path_buf()
    };
    script_path.try_exists().into_diagnostic()?;

    Command::new(python_path.as_ref())
        .arg(&script_path)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .into_diagnostic()
}

fn enderpy_root_path() -> Result<PathBuf> {
    let mut path = std::env::current_exe().into_diagnostic()?;
    let enderpy_dir = loop {
        path.push("Cargo.toml");
        if path.is_file() {
            break path.parent().unwrap().to_path_buf();
        }
        // Pop off "Cargo.toml" and the current directory name.
        if !path.pop() || !path.pop() {
            bail!("Unable to find enderpy_dir.");
        }
    };
    Ok(enderpy_dir)
}

pub fn default_python_path() -> Result<PathBuf> {
    let mut result = which("python");
    if result.is_err() {
        result = which("python3");
    }
    result
        .into_diagnostic()
        .wrap_err("Unable to find python binary.")
}
