{ghc}:
with (import <nixpkgs> {});

let
  wabt = self.stdenv.mkDerivation {
    name = "wabt";
    src = fetchFromGitHub {
      owner = "WebAssembly";
      repo = "wabt";
      rev = "71ce746f1be4290b8d20449ff35b852b5cc374d2";
      sha256 = "0szkr01vdigs3h68qnfzhcl385394b4cfbdd14s3hkk7jm61z0a2";
    };
    nativeBuildInputs = [ cmake python ];
    enableParallelBuilding = true;
    cmakeFlags = ["-DBUILD_TESTS=OFF"];
  };
in
haskell.lib.buildStackProject {
  inherit ghc;
  name = "forest-compiler";
  buildInputs = [ nodejs-10_x ruby wabt ];
}
