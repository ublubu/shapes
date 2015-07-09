{ mkDerivation, base, lens, linear, ListZipper, monad-extras, mtl
, sdl2, stdenv, vector, SDL2
, cabal-install, alex, happy, ghc-mod, hlint, stylish-haskell, hasktags
}:
mkDerivation {
  pname = "tile-rider";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    SDL2 base lens linear ListZipper monad-extras mtl sdl2 vector
  ];
  buildTools = [
    cabal-install alex happy ghc-mod hlint stylish-haskell hasktags
  ];
  homepage = "https://github.com/ublubu/tile-rider";
  description = "a simple puzzle game in Haskell with SDL2";
  license = stdenv.lib.licenses.unfree;
}
