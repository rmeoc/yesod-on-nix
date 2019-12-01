# default.nix
let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv: pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck drv);
  }
