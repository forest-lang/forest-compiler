{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  dependencies = import ./dependencies.nix {};
in
stdenv.mkDerivation {
  name = "test-environment";
  buildInputs = dependencies;
}

