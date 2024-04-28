let
  pkgs = import ./pkgs.nix;
  clap = pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "clap" ./. { }) {
    src = if pkgs.lib.inNixShell then null else ./.;
  };
in
  pkgs.mkShell {
    inputsFrom = [
      clap.env
    ];
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
      pkgs.haskellPackages.ghc
      pkgs.haskellPackages.hoogle
      pkgs.haskellPackages.hlint
    ];
  }
