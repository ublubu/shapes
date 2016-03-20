module Main where

import Test.Hspec

import qualified Shapes.Linear.TemplateSpec

main :: IO ()
main = hspec $ do
  describe "TemplateSpec" Shapes.Linear.TemplateSpec.spec
