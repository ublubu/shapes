{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.TemplateSpec where

import Test.Hspec

import GHC.Types (Double(..))

import Shapes.Linear.Template (makeVectorType)
import Shapes.Linear.ValueInfos (doubleInfo)

$(makeVectorType doubleInfo 2)

spec :: Spec
spec = do
  it "fromListV2 -> toListV2" $ toListV2 (fromListV2 [1, 2]) `shouldBe` [1, 2]
  it "dotV2" $ D# (fromListV2 [0, 2] `dotV2` fromListV2 [1, 3]) `shouldBe` 6
