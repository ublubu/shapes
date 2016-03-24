{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.TemplateSpec where

import Test.Hspec
import Test.QuickCheck

import GHC.Types (Double(..))

import Shapes.Linear.Template (makeVectorType, defineJoinSplit)
import Shapes.Linear.MatrixTemplate (makeMatrixType, defineMatrixMul)
import Shapes.Linear.ValueInfos (doubleInfo)

import qualified Linear.Metric as L
import qualified Linear.V2 as L
import qualified Linear.Matrix as L

$(makeVectorType doubleInfo 2)
$(makeMatrixType doubleInfo (2, 2))
$(defineMatrixMul doubleInfo (2, 2, 2))
$(makeVectorType doubleInfo 4)
$(defineJoinSplit doubleInfo (2, 2))

spec :: Spec
spec = do
  it "toListV2 fromListV2 == id" $ property $
    \xy -> let xy' = pairToList xy in toListV2 (fromListV2 xy') == xy'
  it "dotV2 == L.dot" $ property $
    \(v1, v2) -> D# (v1 `dotV2` v2) == toLV2 v1 `L.dot` toLV2 v2
  it "show" $ show (V2 0.0## 1.0##) `shouldBe` "V2 0.0 1.0"
  it "2x2 * 2x2" $ property $
    \(m1, m2) -> toListM2x2 (m1 `mul2x2x2` m2) == toListLM22 (toLM22 m1 L.!*! toLM22 m2)
  it "split 4 into 2+2 and join back into 4" $ property $
    \v -> (toListV4 . uncurry join2v2 . split2v2 $ v) == toListV4 v

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

toLV2 :: V2 -> L.V2 Double
toLV2 = (\[x, y] -> L.V2 x y) . toListV2

toLM22 :: M2x2 -> L.M22 Double
toLM22 = (\[a, b, c, d] -> L.V2 (L.V2 a b) (L.V2 c d)) . toListM2x2

toListLV2 :: L.V2 Double -> [Double]
toListLV2 (L.V2 x y) = [x, y]

toListLM22 :: L.M22 Double -> [Double]
toListLM22 (L.V2 (L.V2 a b) (L.V2 c d)) = [a, b, c, d]
