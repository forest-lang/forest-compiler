
default: build-bins test

build-bins:
	stack build --copy-bins

build:
	stack build

test: unit integration

unit:
	stack test

integration:
	nix-shell ./nix/wabt-shell.nix --run "ruby test/integration.rb"

.PHONY: build test unit integration
