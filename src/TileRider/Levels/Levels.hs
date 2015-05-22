module TileRider.Levels.Levels where

import qualified TileRider.Levels.Level1 as Level1
import qualified TileRider.Levels.Level2 as Level2
import qualified TileRider.Levels.Level3 as Level3
import TileRider.GameState

levels :: [GridState]
levels = [Level1.level, Level2.level, Level3.level]

