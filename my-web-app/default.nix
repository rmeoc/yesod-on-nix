{ pkgs ? import <nixpkgs> {}
, callCabal2nix ? pkgs.haskellPackages.callCabal2nix
, haskell ? pkgs.haskell
}:
  haskell.lib.overrideCabal
    (callCabal2nix "my-web-app" ./. { })
    (drv: {
      doCheck = false;
      doHaddock = false;
    })
