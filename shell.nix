{ghc}:
with (import <nixpkgs> {});

let
  dependencies = import ./nix/dependencies.nix {};
in
haskell.lib.buildStackProject {
  inherit ghc;
  name = "forest-compiler";
  buildInputs = dependencies;
}
