use enderpy_python_type_checker::build::BuildManager;
use enderpy_python_type_checker::settings::Settings;
use std::fs;
use std::path::PathBuf;
use std::time::Duration;

// Use criterion locally and codspeed on CI.
#[cfg(not(feature = "codspeed"))]
pub use criterion::*;

#[cfg(not(feature = "codspeed"))]
pub type BenchmarkGroup<'a> = criterion::BenchmarkGroup<'a, measurement::WallTime>;

#[cfg(feature = "codspeed")]
pub use codspeed_criterion_compat::*;

pub fn benchmark_type_checker(c: &mut Criterion) {
    let mut group = c.benchmark_group("type_checker");
    let mut paths = vec!["test_data/types_very_fast.py"];
    let conformance_tests = list_python_files("../typechecker/test_data/inputs/conformance_tests/");
    paths.extend(conformance_tests.iter().map(|s| s.as_str()));
    for path in paths {
        let mut parts = path.rsplit('/').collect::<Vec<_>>();
        parts.reverse();
        let file_ext = parts.pop().unwrap_or_default();
        let dir2 = parts.pop().unwrap_or_default();
        let new_path = format!("{}/{}", dir2, file_ext);
        group.bench_with_input(
            BenchmarkId::from_parameter(new_path.to_string()),
            &path,
            |b, path| {
                b.iter(|| {
                    let builder = BuildManager::new(Settings::test_settings());
                    let file_path = PathBuf::from(path);
                    builder.build_one(&PathBuf::from("../../"), &file_path);
                    let id = builder.paths.get(&file_path).unwrap();
                    let file = builder.files.get(&id).unwrap();
                    builder.type_check(&file_path, &file);

                    0
                });
            },
        );
    }
}

fn list_python_files(dir: &str) -> Vec<String> {
    let mut paths = Vec::new();
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries {
            let Ok(entry) = entry else { continue };
            let path = entry.path();
            if path.extension() == Some(std::ffi::OsStr::new("py")) {
                paths.push(path.to_string_lossy().into_owned());
            }
        }
    }
    paths
}

fn get_config() -> Criterion {
    Criterion::default()
        .sample_size(100)
        .measurement_time(Duration::from_secs(60))
        .warm_up_time(Duration::from_secs(3))
}

criterion_group! {
    name = benches;
    config = get_config();
    targets = benchmark_type_checker
}
criterion_main!(benches);
