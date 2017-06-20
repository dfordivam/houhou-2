{ mkDerivation, aeson, base, beam-core, lens, protolude, stdenv }:
mkDerivation {
  pname = "dbmodel";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base beam-core lens protolude ];
  homepage = "https://github.com/dfordivam/dbmodel#readme";
  license = stdenv.lib.licenses.unfree;
}
