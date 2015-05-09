module Levels.Level1 where

import Grid
import SlidingGrid
import GameTile

level :: TileZipper GameTile
level = fromRows [ [ FixedTile $ spawnAt tileE
                   , SlidingTile tileEW
                   , EmptyTile
                   , FixedTile $ goalAt tileW ]
                 , [ FixedTile $ nothingAt tileNone
                   , SlidingTile tileEW
                   , SlidingTile tileSW
                   , FixedTile $ nothingAt tileNone ] ]
