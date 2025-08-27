
export RUSTDOCFLAGS := "-D warnings -D rustdoc::broken-intra-doc-links"

# Check for outstanding TODO comments
todo:
	rg 'TODO|FIXME|todo!' src crates

# Run workspace tests using nextest
test FILTER="":
	cargo nextest run --workspace {{FILTER}}

# Use entr to run tests every time git tracked files are modified
watch-tests FILTER="":
	git ls-files | entr -ac cargo nextest run --workspace {{FILTER}}

# Run rustdoc and open local docs in a browser
open-docs:
	cargo doc --all-features --open &

# Format all Rust files using cargo fmt
format:
	cargo fmt --all

# Check all Rust files using clippy
check-clippy:
	cargo clippy --workspace --all-targets --all-features --examples --tests -- -D warnings

# Check that Rust files are idempotent under cargo fmt
check-fmt:
	cargo fmt --all -- --check

# Check that cargo doc has no warnings
check-docs:
	cargo doc --all-features --workspace

# Run all check targets
check-all: check-clippy check-fmt check-docs

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
