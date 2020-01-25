{ mkDerivation, base, bytestring, hpack, optparse-applicative
, stdenv, yaml
}:
mkDerivation {
  pname = "prom-docs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring optparse-applicative yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring optparse-applicative yaml
  ];
  testHaskellDepends = [ base bytestring optparse-applicative yaml ];
  prePatch = "hpack";
  homepage = "https://github.com/parry84/prom-docs#readme";
  license = stdenv.lib.licenses.asl20;
}
