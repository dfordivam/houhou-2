{ mkDerivation, aeson, base, bytestring, containers, file-embed
, ghcjs-dom, houhou2-shared, reflex, reflex-dom, reflex-dom-core
, stdenv, text, uri-bytestring
}:
mkDerivation {
  pname = "houhou2-frontend";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers file-embed ghcjs-dom
    houhou2-shared reflex reflex-dom reflex-dom-core text
    uri-bytestring
  ];
  description = "TODO";
  license = stdenv.lib.licenses.unfree;
}
