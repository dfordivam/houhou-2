{ mkDerivation, aeson, base, beam-core, beam-sqlite
, bytestring-trie, containers, dbmodel, lens, protolude
, sqlite-simple, stdenv, text
}:
mkDerivation {
  pname = "dbinterface";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base beam-core beam-sqlite bytestring-trie containers dbmodel
    lens protolude sqlite-simple text
  ];
  homepage = "https://github.com/dfordivam/dbinterface#readme";
  license = stdenv.lib.licenses.unfree;
}
