module BenchLinear where

import Criterion.Main
import Physics.Linear
import Linear.Affine
import Linear.V2
import qualified Utils.Linear as UL

thePoint :: P2 Double
thePoint = P (V2 1 2)

theDiff :: Diff V2 Double
theDiff = V2 3 4

thePoint' :: UL.P2
thePoint' = UL.P2 1 2

theDiff' :: UL.D2
theDiff' = UL.D2 3 4

benchy :: Benchmark
benchy = bench "afdot" $ whnf (uncurry afdot) (thePoint, theDiff)

benchy' :: Benchmark
benchy' = bench "UL.afdot" $ whnf (uncurry UL.afdot) (thePoint', theDiff')

main :: IO ()
main = defaultMain [benchy, benchy']
