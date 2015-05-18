module TileRider.Levels.Level3 where

import TileRider.Grid
import TileRider.SlidingGrid
import TileRider.GameTile
import TileRider.GameState

level :: GridState
level = GridState { gridPlayer = (0, 1)
                  , gridPlayerTileWasSliding = False
                  , gridTiles = tiles }
  where tiles = fromRows [ [ FixedTile $ nothingAt tileNone
                           , FixedTile $ nothingAt tileNone
                           , FixedTile $ nothingAt tileNone
                           , SlidingTile tileSW
                           , SlidingTile tileNW ]
                         , [ FixedTile $ spawnAt tileE
                           , SlidingTile tileNE
                           , SlidingTile tileEW
                           , EmptyTile
                           , SlidingTile tileSE ]
                         , [ FixedTile $ nothingAt tileNone
                           , SlidingTile tileEW
                           , SlidingTile tileNone
                           , SlidingTile tileNone
                           , FixedTile $ goalAt tileN ]]
