{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc784" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
