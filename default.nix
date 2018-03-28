{ mkDerivation, aeson, ansi-wl-pprint, base, base-unicode-symbols
, Cabal, cabal2nix, containers, control-bool, data-default-class
, data-fix, directory, distribution-nixpkgs, filepath, generics-sop
, github, hashable, hnix, hourglass, language-nix, lens
, optparse-applicative, parsers, pretty, protolude, req, semigroups
, stdenv, temporary, text, text-format, trifecta, turtle
, unordered-containers
}:
mkDerivation {
  pname = "nh";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base base-unicode-symbols Cabal cabal2nix
    containers control-bool data-default-class data-fix directory
    distribution-nixpkgs filepath generics-sop github hashable hnix
    hourglass language-nix lens optparse-applicative parsers pretty
    protolude req semigroups temporary text text-format trifecta turtle
    unordered-containers
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint base base-unicode-symbols Cabal cabal2nix
    containers control-bool data-default-class distribution-nixpkgs
    generics-sop github hashable hnix hourglass language-nix lens
    optparse-applicative pretty protolude req temporary text
    text-format turtle unordered-containers
  ];
  description = "Nix/Haskell tooling";
  license = stdenv.lib.licenses.agpl3;
}
