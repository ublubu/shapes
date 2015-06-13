{ mkDerivation, base, containers, lens, linear, ListZipper, monad-extras, mtl
, sdl2, stdenv, vector, SDL2, zippers, either
, cabal-install, alex, happy, ghc-mod, hlint, stylish-haskell, hasktags
, emacs
}:
mkDerivation {
  pname = "tile-rider";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    SDL2 base containers lens linear ListZipper monad-extras mtl sdl2 vector zippers either
  ];
  buildTools = [
    emacs cabal-install alex happy ghc-mod hlint stylish-haskell hasktags
  ];
  homepage = "https://github.com/ublubu/tile-rider";
  description = "a simple puzzle game in Haskell with SDL2";
  license = stdenv.lib.licenses.unfree;
}
