module Physics.TestBenchGeometry where

import Criterion.Main
import Linear.V2
import Physics.BenchGeometry
import Physics.Transform
import Debug.Trace

makeBox :: Double -> Double -> Double -> Double -> ShapeInfo Double
makeBox x y w h = trace "makeBox" $ shapeInfo shape
  where hull = rectangleHull w h
        shape = transform (translateTransform (V2 x y)) hull

benchy :: Benchmark
benchy = bench "contact" $ whnf (uncurry contact) (makeBox 0 0 4 4, makeBox 1 3 2 2)
