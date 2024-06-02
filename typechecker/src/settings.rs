use std::{env, path::PathBuf};

use config::{Config, ConfigError, File};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
#[allow(unused)]
pub struct Settings {
    pub typeshed_path: PathBuf,
    pub python_executable: Option<PathBuf>,
}

impl Settings {
    pub fn new() -> Result<Self, ConfigError> {
        let _run_mode = env::var("RUN_MODE").unwrap_or_else(|_| "development".into());
        let s = Config::builder()
            // Start off by merging in the "default" configuration file
            .add_source(File::with_name("examples/hierarchical-env/config/default"))
            .build()?;

        s.try_deserialize()
    }

    pub fn from_typeshed(typeshed_path: PathBuf) -> Self {
        Settings {
            typeshed_path,
            python_executable: None,
        }
    }

    pub fn test_settings() -> Self {
        let file_dir = env::current_dir().unwrap();
        Settings {
            typeshed_path: file_dir.parent().unwrap().join("typeshed"),
            python_executable: None,
        }
    }
}
