{ mkDerivation, aeson, base, bytestring, dbinterface, dbmodel
, houhou2-shared, lens, mtl, protolude
, reflex-websocket-interface-server
, reflex-websocket-interface-shared, sqlite-simple, stdenv, tasty
, tasty-hunit, text, time, wai-websockets
}:
mkDerivation {
  pname = "houhou2-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base dbinterface dbmodel houhou2-shared lens mtl protolude
    reflex-websocket-interface-server reflex-websocket-interface-shared
    sqlite-simple text wai-websockets
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring tasty tasty-hunit text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
