# default.nix
let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskell.lib.overrideCabal 
    (pkgs.haskellPackages.callCabal2nix "my-web-app" ./. { })
    (drv: {
      doCheck = false;
      doHaddock = false;
    })
