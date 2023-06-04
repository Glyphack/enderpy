use std::path::{PathBuf};

use crate::settings::Settings;

struct BuildSource {
    path: PathBuf,
    module: String
}

struct BuildError {
    title: &str,
    code: u8,
    line: u8,
    column: u8,
    end_line: u8,
    end_column: u8
}


struct BuildManager {
    errors: Vec<String>

}

struct BuildManager

fn build(source: BuildSource, options: Settings) -> AnalysisResult {

}
