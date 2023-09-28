use std::path::{Path, PathBuf};
const PROJECT_ROOT_MARKERS: [&str; 1] = ["pyproject.toml"];

pub fn find_project_root(path: &PathBuf) -> &Path {
    let root = path
        .ancestors()
        .find(|p| PROJECT_ROOT_MARKERS.iter().any(|m| p.join(m).exists()));
    match root {
        Some(root) => root,
        None => {
            if path.is_dir() {
                path
            } else {
                path.parent().unwrap_or(path)
            }
        }
    }
}
