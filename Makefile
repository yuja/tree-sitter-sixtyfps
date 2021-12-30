TREE_SITTER = node_modules/.bin/tree-sitter

.PHONY: help
help:
	@echo 'Make targets:'
	@echo '  generate   - generate parser sources'
	@echo '  tests      - run all tests'

.PHONY: generate
generate:
	$(TREE_SITTER) generate

.PHONY: tests
tests:
	$(TREE_SITTER) test $(TESTFLAGS)
