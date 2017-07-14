{ mkDerivation, aeson, base, containers, data-default, protolude
, reflex-websocket-interface-shared, stdenv
}:
mkDerivation {
  pname = "houhou2-shared";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers data-default protolude
    reflex-websocket-interface-shared
  ];
  license = stdenv.lib.licenses.bsd3;
}
