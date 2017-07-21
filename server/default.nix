{ mkDerivation, aeson, base, bytestring, containers, dbinterface
, dbmodel, houhou2-shared, http-types, lens, mtl, pretty-simple
, protolude, reflex-websocket-interface-server
, reflex-websocket-interface-shared, sqlite-simple, stdenv, tasty
, tasty-hunit, text, time, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "houhou2-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers dbinterface dbmodel houhou2-shared http-types
    lens mtl pretty-simple protolude reflex-websocket-interface-server
    reflex-websocket-interface-shared sqlite-simple text time wai
    wai-websockets warp websockets
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring dbinterface houhou2-shared mtl pretty-simple
    sqlite-simple tasty tasty-hunit text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
