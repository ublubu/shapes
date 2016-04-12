module Main where

import Test.Hspec

import qualified Physics.Broadphase.AabbSpec

main :: IO ()
main = hspec $
  describe "TemplateSpec" Physics.Broadphase.AabbSpec.spec
