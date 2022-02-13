GIT = git

.PHONY: all
all: slint

.PHONY: slint
slint:
	[ -d $@ ] || $(GIT) clone https://github.com/slint-ui/slint.git $@
	$(GIT) -C $@ fetch
	$(GIT) -C $@ reset --hard 136f2686b4e33241da8e7ab97bf27712a8d5bf56
