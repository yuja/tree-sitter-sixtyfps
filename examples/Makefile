GIT = git

.PHONY: all
all: sixtyfps

.PHONY: sixtyfps
sixtyfps:
	[ -d $@ ] || $(GIT) clone https://github.com/sixtyfpsui/sixtyfps.git $@
	$(GIT) -C $@ fetch
	$(GIT) -C $@ reset --hard 75ba29cf2fc593e824718bc2b21036b4ee2430f4
