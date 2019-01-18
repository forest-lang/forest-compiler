{ pkgs ? import <nixpkgs> {} }:

with pkgs;

[ nodejs-10_x ruby wabt ]
