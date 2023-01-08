let
  nixpkgs-repo = systemPkgs.fetchFromGitHub (
    let json = builtins.fromJSON (builtins.readFile ./dep/nixpkgs/github.json);
    in { inherit (json) owner repo rev sha256;
        private = json.private or false;
      }
  );
  config = {
    allowBroken = true;
    allowUnfree = true;
  };
  systemPkgs = import <nixpkgs> {};
  pkgs = import nixpkgs-repo { inherit config; };
in pkgs
