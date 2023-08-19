use config::{Config, ConfigError, File};
use serde::Deserialize;
use std::{env, path::PathBuf};

#[derive(Debug, Deserialize)]
#[allow(unused)]
pub struct ImportDiscovery {
    pub python_executable: PathBuf,
}

#[derive(Debug, Deserialize)]
#[allow(unused)]
pub struct Settings {
    pub debug: bool,
    pub import_discovery: ImportDiscovery,
}

impl Settings {
    pub fn new() -> Result<Self, ConfigError> {
        let run_mode = env::var("RUN_MODE").unwrap_or_else(|_| "development".into());
        let s = Config::builder()
            // Start off by merging in the "default" configuration file
            .add_source(File::with_name("examples/hierarchical-env/config/default"))
            .build()?;

        s.try_deserialize()
    }

    pub fn default() -> Self {
        Settings {
            debug: false,
            import_discovery: ImportDiscovery {
                python_executable: PathBuf::from("python"),
            },
        }
    }

    pub fn test_settings() -> Self {
        Settings {
            debug: true,
            import_discovery: ImportDiscovery {
                python_executable: PathBuf::from("python"),
            },
        }
    }
}
