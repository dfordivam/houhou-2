{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  drv = ghc.callPackage ./default.nix {
    houhou2-shared = ghc.callPackage ../shared {};
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
