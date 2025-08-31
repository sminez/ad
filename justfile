
export RUSTDOCFLAGS := "-D warnings -D rustdoc::broken-intra-doc-links"

# Install tooling used in other Just targets
install-tools:
	cargo install cargo-nextest
	cargo install typos-cli

# Check for outstanding TODO comments
todo:
	rg 'TODO|FIXME|todo!' src crates

# Run workspace tests using nextest
test FILTER="":
	cargo nextest run --workspace {{FILTER}}

# Run criterion benchmarks and open the report in firefox
bench FILTER="":
	cargo bench -- {{FILTER}}
	firefox --new-tab target/criterion/report/index.html

# Use entr to run tests every time git tracked files are modified
watch-tests FILTER="":
	git ls-files | entr -ac cargo nextest run --workspace {{FILTER}}

# Run rustdoc and open local docs in a browser
open-docs:
	cargo doc --all-features --open &

# Format all Rust files using cargo fmt
format:
	cargo fmt --all

# Fix spelling mistakes with 'typos'
fix-spelling:
	typos --write-changes

# Check all Rust files using clippy
check-clippy:
	cargo clippy --workspace --all-targets --all-features --examples --tests -- -D warnings

# Check that Rust files are idempotent under cargo fmt
check-fmt:
	cargo fmt --all -- --check

# Check that cargo doc has no warnings
check-docs:
	cargo doc --all-features --workspace

# Check for spelling mistakes using 'typos'
check-spelling:
	typos

# Run all check targets
check-all: check-clippy check-fmt check-docs check-spelling

# List open GitHub issues using gh
list-issues:
	gh issue list

# List open GitHub PRs using gh
list-prs:
	gh pr list

# Open an new GitHub issue using gh
new-issue:
	gh issue create

# Open an new GitHub PR using gh
pr:
	gh pr create
