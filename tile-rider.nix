{ mkDerivation, base, containers, either, lens, linear, ListZipper
, monad-extras, mtl, sdl2, stdenv, vector, zippers
}:
mkDerivation {
  pname = "tile-rider";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers either lens linear ListZipper monad-extras mtl sdl2
    vector zippers
  ];
  homepage = "https://github.com/ublubu/tile-rider";
  description = "a simple puzzle game in Haskell with SDL2";
  license = stdenv.lib.licenses.unfree;
}
