TREE_SITTER = node_modules/.bin/tree-sitter

.PHONY: help
help:
	@echo 'Make targets:'
	@echo '  generate   - generate parser sources'
	@echo '  tests      - run all tests'
	@echo '  fetch-examples - fetch example repositories'
	@echo '  parse-examples - parse example files'

.PHONY: generate
generate:
	$(TREE_SITTER) generate

.PHONY: tests
tests:
	$(TREE_SITTER) test $(TESTFLAGS)

.PHONY: fetch-examples
fetch-examples:
	$(MAKE) -C examples all

.PHONY: parse-examples
parse-examples:
	cat examples/known-failures.txt \
		| sed 's|^|!examples/|' \
		| xargs $(TREE_SITTER) parse -q 'examples/**/*.60'
