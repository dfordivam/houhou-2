{ mkDerivation, aeson, base, beam-core, beam-sqlite
, bytestring-trie, containers, dbmodel, lens, pretty-simple
, protolude, sqlite-simple, stdenv, text, time
}:
mkDerivation {
  pname = "dbinterface";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base beam-core beam-sqlite bytestring-trie containers dbmodel
    lens pretty-simple protolude sqlite-simple text time
  ];
  homepage = "https://github.com/dfordivam/dbinterface#readme";
  license = stdenv.lib.licenses.unfree;
}
