.PHONY: audit-dependencies
audit-dependencies:
	cargo audit

.PHONY: upgrade-check
upgrade-check:
	cargo upgrade --workspace --dry-run

.PHONY: todo
todo:
	rg 'TODO|FIXME|todo!' src

.PHONY: force-unmount
force-unmount:
	fusermount -u $$HOME/.ad/mnt
