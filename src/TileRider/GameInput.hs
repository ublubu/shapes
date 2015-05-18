module TileRider.GameInput where

import SDL.Geometry
import Directional
import TileRider.Drag

data GridInput = GridInput { gridDrag :: Maybe (Point (Point Double))
                           , gridPartialMove :: Maybe (PartialMoveResult Double)} deriving Show

defaultGridInput :: GridInput
defaultGridInput = GridInput { gridDrag = Nothing
                             , gridPartialMove = Nothing }

