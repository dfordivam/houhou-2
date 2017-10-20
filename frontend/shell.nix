{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghcjs }:

let

  inherit (nixpkgs) pkgs;

  reflex-websocket-interface-shared = ghc.callPackage ../reflex-websocket-interface/shared {};
  reflex-websocket-interface = ghc.callPackage ../reflex-websocket-interface/reflex {inherit reflex-websocket-interface-shared;};
  reflex-dom-semui = nixpkgs.haskell.lib.dontCheck (ghc.callPackage ~/repos/reflex/reflex-dom-semui {});

  hsmfcc = ghc.callPackage ../hsmfcc {};

  drv = ghc.callPackage (ghc.haskellSrc2nix
  {name = "houhou2-frontend";
  src = ./.;}) {
    inherit reflex-websocket-interface-shared reflex-websocket-interface
    hsmfcc reflex-dom-semui;
    houhou2-shared = ghc.callPackage (ghc.haskellSrc2nix {name =
    "houhou2-shared"; src = ../shared; }) {inherit reflex-websocket-interface-shared;};
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
