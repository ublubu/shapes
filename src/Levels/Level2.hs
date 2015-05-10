module Levels.Level2 where

import Grid
import SlidingGrid
import GameTile
import GameState

level :: GridState
level = GridState { gridPlayer = (0, 1)
                  , gridPlayerTileWasSliding = False
                  , gridTiles = fromRows [ [ FixedTile $ nothingAt tileNone
                                           , SlidingTile tileNone
                                           , SlidingTile tileNone
                                           , EmptyTile
                                           , FixedTile $ nothingAt tileNone ]
                                         , [ FixedTile $ spawnAt tileE
                                           , SlidingTile tileEW
                                           , SlidingTile tileNW
                                           , SlidingTile tileNone
                                           , FixedTile $ nothingAt tileNone ]
                                         , [ FixedTile $ nothingAt tileNone
                                           , SlidingTile tileNone
                                           , SlidingTile tileSW
                                           , SlidingTile tileNone
                                           , FixedTile $ nothingAt tileNone ]
                                         , [ FixedTile $ nothingAt tileNone
                                           , SlidingTile tileNone
                                           , SlidingTile tileNone
                                           , SlidingTile tileNone
                                           , FixedTile $ nothingAt tileNone ]
                                         , [ FixedTile $ nothingAt tileNone
                                           , FixedTile $ nothingAt tileNone
                                           , FixedTile $ goalAt tileNone
                                           , FixedTile $ nothingAt tileNone
                                           , FixedTile $ nothingAt tileNone ] ] }
