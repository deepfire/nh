{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc841"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
}:

ghcOrig.override (oldArgs: {
    overrides = new: old:
    import ./overrides.nix { inherit pkgs; self = new; super = old; haskellLib = haskell.lib; };
  })
