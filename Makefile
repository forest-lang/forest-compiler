
default: build test

build:
	stack build

test:
	stack test && ruby test/integration.rb

.PHONY: build test
