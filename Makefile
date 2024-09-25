.PHONY: test
test:
	cargo nextest run --workspace $(ARGS)


.PHONY: watch-test
watch-test:
	cargo nextest run --workspace
	git ls-files | entr -ac cargo nextest run --workspace $(ARGS)

.PHONY: doc
doc:
	cargo doc --all-features --open &

.PHONY: examples
examples:
	cargo build --examples

.PHONY: format
format:
	cargo fmt

.PHONY: clippy
clippy:
	cargo clippy --workspace --all-targets --all-features --examples --tests

.PHONY: check-all
check-all:
	cargo fmt --all -- --check
	cargo clippy --workspace --all-targets --all-features --examples --tests
	cargo rustdoc --all-features -- -D warnings
	cargo test --workspace --all-features

.PHONY: audit-dependencies
audit-dependencies:
	cargo audit

.PHONY: upgrade-check
upgrade-check:
	cargo upgrade --workspace --dry-run

.PHONY: todo
todo:
	rg 'TODO|FIXME|todo!' src

.PHONY: ensure-mountpoint
ensure-mountpoint:
	mkdir -p $$HOME/.ad/mnt

.PHONY: backup-current-config
backup-current-config: ensure-mountpoint
	[ -f $$HOME/.ad/init.conf ] && mv $$HOME/.ad/init.conf $$HOME/.ad/init.conf.bck

.PHONY: copy-default-config
copy-default-config: ensure-mountpoint backup-current-config
	cp data/init.conf $$HOME/.ad

.PHONY: copy-rust-config
copy-rust-config: ensure-mountpoint backup-current-config
	cp data/init-rust.conf $$HOME/.ad/init.conf

.PHONY: copy-bin
copy-bin: ensure-mountpoint
	cp -r data/bin $$HOME/.ad
	cp -r data/lib $$HOME/.ad

.PHONY: setup-dotfiles
setup-dotfiles: copy-default-config copy-bin

.PHONY: force-unmount
force-unmount:
	fusermount -u $$HOME/.ad/mnt

# GitHub helpers using the official gh GitHub CLI
.PHONY: list-issues
list-issues:
	gh issue list

.PHONY: list-prs
list-prs:
	gh pr list

.PHONY: new-issue
new-issue:
	gh issue create

.PHONY: pr
pr:
	gh pr create
