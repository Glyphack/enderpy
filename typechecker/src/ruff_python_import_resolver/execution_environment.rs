use std::path::PathBuf;

use crate::ruff_python_import_resolver::{python_version::PythonVersion, python_platform::PythonPlatform};


#[derive(Debug)]
pub struct ExecutionEnvironment {
    /// The root directory of the execution environment.
    pub root: PathBuf,

    /// The Python version of the execution environment.
    pub python_version: PythonVersion,

    /// The Python platform of the execution environment.
    pub python_platform: PythonPlatform,

    /// The extra search paths of the execution environment.
    pub extra_paths: Vec<PathBuf>,
}
