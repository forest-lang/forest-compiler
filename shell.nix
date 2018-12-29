{ghc}:
with (import <nixpkgs> {});

let
  wabt = import ./nix/wabt.nix {};
in
haskell.lib.buildStackProject {
  inherit ghc;
  name = "forest-compiler";
  buildInputs = [ nodejs-10_x ruby wabt ];
}
