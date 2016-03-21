{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Types (Double(D#))

import Criterion.Main

import qualified Shapes.Linear.Double as D
import qualified Shapes.Linear.Boxed as B
import qualified Linear.V2 as L
import qualified Linear.Matrix as L
import qualified Linear.Metric as L

main :: IO ()
main = defaultMain (benchLinear ++ benchShapesMath ++ benchShapesMathBoxed)

theLV2 :: L.V2 Double
theLV2 = L.V2 1 2

theLV2' :: L.V2 Double
theLV2' = L.V2 3 4

theLM2x2 :: L.V2 (L.V2 Double)
theLM2x2 = L.V2 (L.V2 1 2) (L.V2 3 4)

benchLinear :: [Benchmark]
benchLinear =
  [ bench "L.dot" $ whnf (uncurry L.dot) (theLV2, theLV2')
  , bench "L.!*!" $ whnf (uncurry (L.!*!)) (theLM2x2, theLM2x2)
  ]

theDV2 :: D.V2
theDV2 = D.V2 1.0## 2.0##

theDV2' :: D.V2
theDV2' = D.V2 3.0## 4.0##

theDM2x2 :: D.M2x2
theDM2x2 = D.fromListM2x2 [1, 2, 3, 4]

benchShapesMath :: [Benchmark]
benchShapesMath =
  [ bench "D.dotV2" $ whnf (\(x, y) -> D# (x `D.dotV2` y)) (theDV2, theDV2')
  , bench "D.mul2x2x2" $ whnf (uncurry D.mul2x2x2) (theDM2x2, theDM2x2)
  ]

theBV2 :: B.V2
theBV2 = B.V2 1 2

theBV2' :: B.V2
theBV2' = B.V2 3 4

benchShapesMathBoxed :: [Benchmark]
benchShapesMathBoxed =
  [ bench "B.dot" $ whnf (uncurry B.dot) (theBV2, theBV2') ]
