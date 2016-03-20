{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.TemplateSpec where

import Test.Hspec
import Test.QuickCheck

import GHC.Types (Double(..))
import Control.Monad

import Shapes.Linear.Template (makeVectorType)
import Shapes.Linear.ValueInfos (doubleInfo)

import qualified Linear.Metric as L
import qualified Linear.V2 as L

$(makeVectorType doubleInfo 2)

spec :: Spec
spec = do
  it "toListV2 fromListV2 == id" $ property $
    \xy -> let xy' = pairToList xy in toListV2 (fromListV2 xy') == xy'
  it "dotV2 == L.dot" $ property $
    \(v1, v2) -> D# (v1 `dotV2` v2) == toLV2 v1 `L.dot` toLV2 v2

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

instance Arbitrary V2 where
  arbitrary = fromListV2 <$> replicateM 2 arbitrary

toLV2 :: V2 -> L.V2 Double
toLV2 = (\[x, y] -> L.V2 x y) . toListV2

toListLV2 :: L.V2 Double -> [Double]
toListLV2 (L.V2 x y) = [x, y]
