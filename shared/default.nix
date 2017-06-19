{ mkDerivation, aeson, base, classy-prelude, containers, stdenv
, text
}:
mkDerivation {
  pname = "houhou2-shared";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base classy-prelude containers text
  ];
  license = stdenv.lib.licenses.bsd3;
}
