use std::{env, path::PathBuf};

use config::{Config, ConfigError, File};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
#[allow(unused)]
pub struct ImportDiscovery {
    pub python_executable: Option<PathBuf>,
    pub typeshed_path: Option<PathBuf>,
}

#[derive(Debug, Deserialize)]
#[allow(unused)]
pub struct Settings {
    pub debug: bool,
    pub root: PathBuf,
    pub import_discovery: ImportDiscovery,
    // Indicates whether to check imports
    pub follow_imports: FollowImports,
}

#[derive(Debug, Deserialize)]
#[allow(unused)]
pub enum FollowImports {
    #[serde(rename = "all")]
    All,
    #[serde(rename = "skip")]
    Skip,
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

    pub fn test_settings() -> Self {
        let file_dir = env::current_dir().unwrap();
        Settings {
            debug: false,
            root: PathBuf::from(""),
            follow_imports: FollowImports::All,
            import_discovery: ImportDiscovery {
                python_executable: None,
                typeshed_path: Some(file_dir.parent().unwrap().join("typeshed")),
            },
        }
    }
}
