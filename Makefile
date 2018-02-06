
default: build test

build:
	stack build

test: unit integration

unit:
	stack test

integration:
	ruby test/integration.rb

.PHONY: build test unit integration
