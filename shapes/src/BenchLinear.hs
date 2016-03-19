{-# LANGUAGE MagicHash #-}

module BenchLinear where

import GHC.Types (Double(D#))

import Criterion.Main
import qualified Data.Vector.Unboxed as U
import Linear.Affine
import Linear.V2
import Linear.V3
import Linear.Matrix

import Physics.Linear
import qualified Utils.Linear as UL

thePoint :: P2 Double
thePoint = P (V2 1 2)

theDiff :: Diff V2 Double
theDiff = V2 3 4

thePoint' :: UL.P2
thePoint' = UL.P2 1.0## 2.0##

thePoint'' :: U.Vector Double
thePoint'' = U.fromList [1, 2]

theDiff' :: UL.V2
theDiff' = UL.V2 3.0## 4.0##

theDiff'' :: U.Vector Double
theDiff'' = U.fromList [3, 4]

theMat33 :: M33 Double
theMat33 = V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9)

theMat33' :: UL.M33
theMat33' = UL.M33 1.0## 2.0## 3.0## 4.0## 5.0## 6.0## 7.0## 8.0## 9.0##

theMat33'' :: UL.M33'
theMat33'' = UL.M33' 1 2 3 4 5 6 7 8 9

uvAfdot :: U.Vector Double -> U.Vector Double -> Double
uvAfdot = ((.).(.)) (U.foldl1 (+)) (U.zipWith (*))

benchy :: [Benchmark]
benchy = [ bench "afdot" $ whnf (uncurry afdot) (thePoint, theDiff)
         , bench "!*!" $ whnf (uncurry (!*!)) (theMat33, theMat33) ]

benchy' :: [Benchmark]
benchy' = [ bench "UL.afdot#" $ whnf (\(x, y) -> D# (x `UL.afdot#` y)) (thePoint', theDiff')
          , bench "UL.afdot'" $ whnf (\(x, y) -> D# (x `UL.afdot'` y)) (thePoint', theDiff')
          , bench "UL.mul33#" $ whnf (uncurry UL.mul33#) (theMat33', theMat33')
          , bench "UL.mul33'" $ whnf (uncurry UL.mul33') (theMat33'', theMat33'')
          ]

benchy'' :: [Benchmark]
benchy'' = [ bench "uvAfdot" $ whnf (uncurry uvAfdot) (thePoint'', theDiff'')]

main :: IO ()
main = defaultMain (benchy ++ benchy' ++ benchy'')
