{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  pinned =
    import (builtins.fetchTarball {
      name = "nixos-unstable-2019-10-10";
      url = https://github.com/NixOS/nixpkgs/archive/19.09.zip;
      # Hash obtained using `nix-prefetch-url --unpack <url>`
      sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
    }) {};
in
  [ pinned.nodejs-10_x pinned.ruby pinned.wabt pinned.libffi ]
