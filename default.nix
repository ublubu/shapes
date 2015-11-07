{ mkDerivation, base, containers, criterion, either, lens, linear
, stdenv, vector
}:
mkDerivation {
  pname = "shapes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers either lens linear vector
  ];
  executableHaskellDepends = [
    base containers criterion either lens linear vector
  ];
  homepage = "https://github.com/ublubu/shapes";
  description = "physics engine and other tools for 2D shapes";
  license = stdenv.lib.licenses.unfree;
}
