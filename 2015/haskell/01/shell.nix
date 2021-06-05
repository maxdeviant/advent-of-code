with import <nixpkgs> {};

let
  haskellDeps = ps: with ps; [
    hspec
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;
in
stdenv.mkDerivation {
  name = "advent-of-code-2015";

  buildInputs = [
    ghc
    haskellPackages.cabal-install
    ormolu
  ];
}