module Main where

import Test.Hspec

import qualified Physics.Broadphase.Opt.AabbSpec

main :: IO ()
main = hspec $
  describe "TemplateSpec" Physics.Broadphase.Opt.AabbSpec.spec
