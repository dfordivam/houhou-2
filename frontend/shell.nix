{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  reflex-websocket-interface-shared = ghc.callPackage ~/repos/reflex/reflex-websocket-interface/typeclassbased/shared {};
  reflex-websocket-interface = ghc.callPackage ~/repos/reflex/reflex-websocket-interface/typeclassbased/frontend {inherit reflex-websocket-interface-shared;};

  drv = ghc.callPackage ./default.nix {
    inherit reflex-websocket-interface-shared reflex-websocket-interface;
    houhou2-shared = ghc.callPackage ../shared {inherit reflex-websocket-interface-shared;};
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
