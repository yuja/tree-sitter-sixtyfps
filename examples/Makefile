GIT = git

.PHONY: all
all: sixtyfps

.PHONY: sixtyfps
sixtyfps:
	[ -d $@ ] || $(GIT) clone https://github.com/sixtyfpsui/sixtyfps.git $@
	$(GIT) -C $@ fetch
	$(GIT) -C $@ reset --hard 4f5df180b866a239a696c8e4b3e4dac37792cb35
