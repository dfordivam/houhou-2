{ mkDerivation, aeson, base, beam-core, beam-sqlite, dbmodel, lens
, protolude, sqlite-simple, stdenv
}:
mkDerivation {
  pname = "dbinterface";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base beam-core beam-sqlite dbmodel lens protolude
    sqlite-simple
  ];
  homepage = "https://github.com/dfordivam/dbinterface#readme";
  license = stdenv.lib.licenses.unfree;
}
