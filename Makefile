.PHONY: all
all: test check format-check lint

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
	@cargo fmt --all

.PHONY: format-check
format-check:
	@cargo fmt --all -- --check

.PHONY: lint
lint:
	@cargo clippy --all --all-features --tests
