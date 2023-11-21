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

.PHONY: setup-dotfiles
setup-dotfiles: copy-default-config copy-bin

.PHONY: force-unmount
force-unmount:
	fusermount -u $$HOME/.ad/mnt
