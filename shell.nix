{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc841"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, tools       ? false
, intero      ? tools
}:
let

  ghc     = import ./packages.nix { inherit nixpkgs pkgs haskell compiler ghcOrig; };
  default = import ./.;
  drv     = ghc.callPackage default {};
  drv'    = haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends =
                [ ghc.cabal-install ];
             });
in
  drv'.env
