use enderpy_python_type_checker::build::BuildManager;
use enderpy_python_type_checker::settings::Settings;
use std::path::PathBuf;
use std::time::Duration;

// Use criterion locally and codspeed on CI.
#[cfg(not(codspeed))]
pub use criterion::*;

#[cfg(not(codspeed))]
pub type BenchmarkGroup<'a> = criterion::BenchmarkGroup<'a, measurement::WallTime>;

#[cfg(codspeed)]
pub use codspeed_criterion_compat::*;

pub fn benchmark_type_checker(c: &mut Criterion) {
    let mut group = c.benchmark_group("type_checker");
    let paths = vec![
        "test_data/types_very_fast.py",
        "../typechecker/test_data/inputs/conformance_tests/annotations_coroutine.py",
    ];
    for path in paths {
        group.bench_with_input(
            BenchmarkId::from_parameter(path.to_string()),
            &path,
            |b, path| {
                b.iter(|| {
                    let builder = BuildManager::new(Settings::test_settings());
                    let file_path = PathBuf::from(path);
                    builder.build_one(&PathBuf::from("../../"), &file_path);
                    builder.type_check(&file_path);

                    0
                });
            },
        );
    }
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
