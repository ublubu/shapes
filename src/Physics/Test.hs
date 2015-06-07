module Physics.Test where

import Linear.Affine
import Linear.Matrix
import Linear.V2
import Physics.Linear
import Physics.Transform
import Physics.Geometry

boxA :: (Floating a) => LocalT a (ConvexHull a)
boxA = LocalT (toTransform (V2 0 0) 0) (rectangleHull 1 1)

main :: IO ()
main = do
  let v = V2 3 4
      h = horizontalizer22 v
    in print (h !* v)
  let p = P $ V2 0 0
      n = V2 0 1 :: V2 Double
      p' = P $ V2 2 0
      n' = V2 (-1) 1
    in print (intersect2 (Line2 p n) (Line2 p' n'))
