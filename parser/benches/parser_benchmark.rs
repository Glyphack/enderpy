use codspeed_criterion_compat::{criterion_group, criterion_main, Criterion};
use enderpy_python_parser::*;
use reqwest::blocking::Client;
use std::fs::read_to_string;
use std::fs::remove_file;
use std::fs::File;
use std::io::copy;

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
    ]
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let tests = create_test_cases();
    let path = tests.last().unwrap();
    let source = read_to_string(path).expect("cannot read file");
    let mut parser = Parser::new(source, path.to_string());

    c.bench_function("parse pydantic", |b| b.iter(|| parser.parse().unwrap()));

    remove_file(path).expect("cannot delete file");
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
