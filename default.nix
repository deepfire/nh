{ mkDerivation, aeson, ansi-wl-pprint, base, base-unicode-symbols
, Cabal, cabal2nix, containers, data-default-class, directory
, distribution-nixpkgs, filepath, hnix, hourglass, language-nix
, lens, optparse-applicative, parsers, pretty, req, semigroups
, stdenv, temporary, text, text-format, trifecta
}:
mkDerivation {
  pname = "nh";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base base-unicode-symbols Cabal cabal2nix
    containers data-default-class directory distribution-nixpkgs
    filepath hnix hourglass language-nix lens optparse-applicative
    parsers pretty req semigroups temporary text text-format trifecta
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint base base-unicode-symbols Cabal cabal2nix
    containers data-default-class distribution-nixpkgs hnix hourglass
    language-nix lens optparse-applicative pretty req temporary text
    text-format
  ];
  description = "Nix/Haskell tooling";
  license = stdenv.lib.licenses.agpl3;
}
