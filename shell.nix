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
      pkgs.xorg.libX11
      pkgs.xorg.xcbutil
      pkgs.xcb-util-cursor
      pkgs.xorg.xcbutilkeysyms
      pkgs.libxkbcommon_7
      pkgs.freetype
      pkgs.pkg-config
      pkgs.glib
      pkgs.cairo
      pkgs.pango
      pkgs.pcre
      pkgs.gtkmm3
      pkgs.sqlite
      pkgs.portaudio

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
