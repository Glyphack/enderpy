use enderpy_python_parser::*;
use reqwest::blocking::Client;
use std::fs::read_to_string;
use std::fs::remove_file;
use std::fs::File;
use std::io::copy;

// Use criterion locally and codspeed on CI.
#[cfg(not(codspeed))]
pub use criterion::*;

#[cfg(not(codspeed))]
pub type BenchmarkGroup<'a> = criterion::BenchmarkGroup<'a, measurement::WallTime>;

#[cfg(codspeed)]
pub use codspeed_criterion_compat::*;

fn try_download(path: &str, url: &str) -> String {
    let client = Client::new();
    let mut response = client.get(url).send().unwrap();

    if response.status().is_success() {
        let mut dest = File::create(path).unwrap();
        copy(&mut response, &mut dest).unwrap();
        println!("Downloaded file to {:?}", path);
    } else {
        println!("Failed to download file: {}", response.status());
    }

    path.to_string()
}

fn create_test_cases() -> Vec<String> {
    vec![
        try_download(
            "pydantic_types.py",
            "https://raw.githubusercontent.com/pydantic/pydantic/83b3c49e99ceb4599d9286a3d793cea44ac36d4b/pydantic/types.py",
        ),
        try_download("dataset.py", "https://raw.githubusercontent.com/DHI/mikeio/b7d26418f4db2909b0aa965253dbe83194d7bb5b/tests/test_dataset.py")
    ]
}

pub fn benchmark_parser(c: &mut Criterion) {
    let tests = create_test_cases();
    let mut group = c.benchmark_group("parser");

    for path in tests.iter() {
        let source = read_to_string(path).expect("cannot read file");

        group.bench_with_input(
            BenchmarkId::from_parameter(path.to_string()),
            &source,
            |b, source| {
                b.iter(|| {
                    let mut parser = Parser::new(source, path);
                    parser.parse().unwrap();

                    0
                });
            },
        );

        remove_file(path).expect("cannot delete file");
    }
    group.finish()
}

criterion_group!(benches, benchmark_parser);
criterion_main!(benches);
