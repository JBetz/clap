let
  pkgs = import <nixpkgs> {};  # import ./pkgs.nix;
  clap = pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "clap" ./. { }) {
    src = if pkgs.lib.inNixShell then null else ./.;
  };
in
  pkgs.mkShell {
    inputsFrom = [
      clap.env
    ];
    buildInputs = [
      pkgs.cmake
      pkgs.haskellPackages.c2hsc
      pkgs.ghcid
      pkgs.cabal-install
      pkgs.stylish-haskell
      pkgs.haskellPackages.ghc
      pkgs.haskellPackages.hoogle
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.c2hs
    ];
  }
