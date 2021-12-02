with import <nixpkgs> {};

let
  easy-ps = import (fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "7802db65618c2ead3a55121355816b4c41d276d9";
    sha256 = "0n99hxxcp9yc8yvx7bx4ac6askinfark7dnps3hzz5v9skrvq15q";
  }) {
    inherit pkgs;
  };
in
stdenv.mkDerivation {
  name = "advent-of-code-2017";

  buildInputs = [
    easy-ps.purs
    easy-ps.purs-tidy
    easy-ps.spago
  ];
}
