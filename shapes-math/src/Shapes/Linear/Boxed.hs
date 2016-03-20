module Shapes.Linear.Boxed where

data V2 = V2 Double Double deriving Show

dot :: V2 -> V2 -> Double
(V2 x0 y0) `dot` (V2 x1 y1) = (x0 * x1) + (y0 * y1)
