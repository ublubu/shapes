module GameInput where

import SDL.Geometry
import Directional
import Drag

data GridInput = GridInput { gridDrag :: Maybe (Point (Point Double))
                           , gridPartialMove :: Maybe (PartialMoveResult Double)} deriving Show

defaultGridInput :: GridInput
defaultGridInput = GridInput { gridDrag = Nothing
                             , gridPartialMove = Nothing }

