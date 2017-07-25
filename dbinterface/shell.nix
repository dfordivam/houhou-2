{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  beam-core = ghc.callPackage ~/repos/beam/beam-core        {};
  beam-migrate = ghc.callPackage ~/repos/beam/beam-migrate {inherit beam-core;};
  beam-sqlite = ghc.callPackage ~/repos/beam/beam-sqlite {inherit beam-core beam-migrate;};
  drv = ghc.callPackage ./default.nix {
    inherit beam-core beam-sqlite;
    dbmodel = ghc.callPackage (ghc.haskellSrc2nix {name = "dbmodel"; src=
    ../dbmodel;} ) {inherit beam-core beam-sqlite;};
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
