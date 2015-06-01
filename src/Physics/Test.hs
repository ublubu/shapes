module Physics.Test where

import Linear.V2
import Physics.Geometry

boxA :: (Fractional a) => WorldT ConvexHull a
boxA = WorldT (rectangleHull 1 1) (WorldTransform (V2 0 0) 0)

boxB :: (Fractional a) => WorldT ConvexHull a
boxB = WorldT (rectangleHull 1.0001 1.0001) (WorldTransform (V2 1 0) 0)

boxC :: (Fractional a) => WorldT ConvexHull a
boxC = WorldT (rectangleHull 1 1) (WorldTransform (V2 2 0) 0)

boxD :: (Fractional a) => WorldT ConvexHull a
boxD = WorldT (rectangleHull 1 1) (WorldTransform (V2 1 1) 0)

boxE :: (Fractional a) => WorldT ConvexHull a
boxE = WorldT (rectangleHull 1 1) (WorldTransform (V2 1 (-1)) 0)

main :: IO ()
main = do
  print $ boxA `overlaps` boxB
  print $ boxA `overlaps` boxC
  print $ boxB `overlaps` boxC
  print $ boxB `overlaps` boxD
  print $ boxB `overlaps` boxE
