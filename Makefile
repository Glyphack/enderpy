.PHONY: all
all: test

.PHONY: build
build:
	@cargo build --all

.PHONY: test
test:
	@cargo test --all

.PHONY: check
check:
	@cargo check --all

.PHONY: format
format:
	@cargo +nightly fmt --all

.PHONY: format-check
format-check:
	cargo +nightly fmt --all -- --check

.PHONY: lint
lint:
	@cargo +nightly clippy --all --all-features --tests -- -D warnings
