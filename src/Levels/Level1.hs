module Levels.Level1 where

import Grid
import SlidingGrid
import GameTile
import GameState

level :: GridState
level = GridState { gridPlayer = (0, 0)
                  , gridPlayerTileWasSliding = False
                  , gridTiles = tiles }
  where tiles = fromRows [ [ FixedTile $ spawnAt tileE
                           , SlidingTile tileEW
                           , EmptyTile
                           , FixedTile $ goalAt tileW ]
                         , [ FixedTile $ nothingAt tileNone
                           , SlidingTile tileEW
                           , SlidingTile tileSW
                           , FixedTile $ nothingAt tileNone ] ]
