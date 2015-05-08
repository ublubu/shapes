module GameInput where

import SDL.Geometry

data GridInput = GridInput { gridDrag :: Maybe (Point (Point Double)) } deriving Show
