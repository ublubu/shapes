module SideScroller.TestMain where

import Debug.Trace
import Directional
import SideScroller.RectangularWorld

main :: IO ()
main = do
  print $ hasCollision (Rectangular 1 1 0 0) (Rectangular 2 2 1 1) -- True
  print $ hasCollision (Rectangular 1 1 0 0) (Rectangular 0.8 0.8 0.2 0.2) -- True
  print $ hasCollision (Rectangular 0.8 0.8 0.2 0.2) (Rectangular 1 1 0 0) -- True
  print $ hasCollision (Rectangular 1 1 0 0) (Rectangular 3 3 2 2) -- False
  print $ overlap (0, 1) (0.5, 1.5)
  print $ overlap (0, 1) (0.2, 0.8)
  print $ overlap (0.2, 0.8) (0, 1)
  print $ checkCollision (Rectangular 1 1 0 0) (Rectangular 3 1 2 0) (2, 0)
  print $ checkCollision (Rectangular 1 1 0 0) (Rectangular 3 1 2 0) (2, 2)
  print $ checkCollision (Rectangular 1 1 0 0) (Rectangular 3 2 2 1) (2, 1)

