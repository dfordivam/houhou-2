{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  beam-core = ghc.callPackage ~/repos/beam/beam-core        {};
  beam-migrate = ghc.callPackage ~/repos/beam/beam-migrate {inherit beam-core;};
  beam-sqlite = ghc.callPackage ~/repos/beam/beam-sqlite {inherit beam-core beam-migrate;};
  dbmodel = ghc.callPackage ../dbmodel {inherit beam-core beam-sqlite;};
  dbinterface = ghc.callPackage ../dbinterface {inherit beam-core beam-sqlite dbmodel;};

  houhou2-shared = ghc.callPackage ../shared {inherit reflex-websocket-interface-shared;};

  reflex-websocket-interface-shared = ghc.callPackage ~/repos/reflex/reflex-websocket-interface/typeclassbased/shared {};
  reflex-websocket-interface-server = ghc.callPackage ~/repos/reflex/reflex-websocket-interface/typeclassbased/server {inherit reflex-websocket-interface-shared;};

  drv = ghc.callPackage ./default.nix {
    inherit dbmodel dbinterface houhou2-shared reflex-websocket-interface-shared reflex-websocket-interface-server;
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
