{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  wabt = import ./wabt.nix {};
in
self.stdenv.mkDerivation {
  name = "wabt-environment";
  buildInputs = [wabt];
}

