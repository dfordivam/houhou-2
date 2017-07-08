{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghcjs }:

let

  inherit (nixpkgs) pkgs;

  reflex-websocket-interface-shared = ghc.callPackage ~/repos/reflex/reflex-websocket-interface/shared {};
  reflex-websocket-interface = ghc.callPackage ~/repos/reflex/reflex-websocket-interface/reflex {inherit reflex-websocket-interface-shared;};
  reflex-dom-semui = nixpkgs.haskell.lib.dontCheck (ghc.callPackage ~/repos/reflex/reflex-dom-semui {});

  drv = ghc.callPackage ./default.nix {
    inherit reflex-websocket-interface-shared reflex-websocket-interface reflex-dom-semui;
    houhou2-shared = ghc.callPackage ../shared {inherit reflex-websocket-interface-shared;};
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
