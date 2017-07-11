{ mkDerivation, aeson, base, beam-core, beam-sqlite, lens
, protolude, sqlite-simple, stdenv, time
}:
mkDerivation {
  pname = "dbmodel";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base beam-core beam-sqlite lens protolude sqlite-simple time
  ];
  homepage = "https://github.com/dfordivam/dbmodel#readme";
  license = stdenv.lib.licenses.unfree;
}
