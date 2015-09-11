{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, either, lens, linear
      , ListZipper, monad-extras, mtl, sdl2, stdenv, vector, zippers
      , emacs, cabal-install, alex, happy, ghc-mod, hlint, stylish-haskell, hasktags
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
        buildTools = [
          emacs cabal-install alex happy ghc-mod hlint stylish-haskell hasktags
        ];
        homepage = "https://github.com/ublubu/tile-rider";
        description = "a simple puzzle game in Haskell with SDL2";
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
