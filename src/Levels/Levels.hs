module Levels.Levels where

import qualified Levels.Level1 as Level1
import qualified Levels.Level2 as Level2
import GameState

levels :: [GridState]
levels = [Level1.level, Level2.level]
