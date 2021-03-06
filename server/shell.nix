{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  pretty-simple = ghc.callPackage ~/repos/pretty-simple        {};

  beam-core = ghc.callPackage ~/repos/beam/beam-core        {};
  beam-migrate = ghc.callPackage ~/repos/beam/beam-migrate {inherit beam-core;};
  beam-sqlite = ghc.callPackage ~/repos/beam/beam-sqlite {inherit beam-core beam-migrate;};
  dbmodel = ghc.callPackage (ghc.haskellSrc2nix {name =
      "dbmodel"; src = ../dbmodel;}) {inherit beam-core beam-sqlite;};
  dbinterface = ghc.callPackage
    (ghc.haskellSrc2nix {name =
      "dbinterface"; src = ../dbinterface; })
      {inherit beam-core beam-sqlite dbmodel;};

  houhou2-shared = ghc.callPackage (ghc.haskellSrc2nix {name =
      "houhou2-shared"; src = ../shared; })
      {inherit reflex-websocket-interface-shared;};

  reflex-websocket-interface-shared = ghc.callPackage ../reflex-websocket-interface/shared {};
  reflex-websocket-interface-server = ghc.callPackage ../reflex-websocket-interface/server {inherit reflex-websocket-interface-shared;};


  julius = ghc.callPackage ../julius {};
  hsjulius = ghc.callPackage ../hsjulius {inherit julius pretty-simple;};

  hsmecab = ghc.callPackage ../hsmecab {inherit (pkgs) mecab;};

  drv = ghc.callPackage (ghc.haskellSrc2nix
  {name = "houhou2-server";
  src = ./.;}) {
    inherit dbmodel dbinterface houhou2-shared reflex-websocket-interface-shared
    reflex-websocket-interface-server pretty-simple hsjulius hsmecab;
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
