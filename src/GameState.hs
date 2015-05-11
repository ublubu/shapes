module GameState where

import Data.Maybe
import SDL.Geometry
import Utils.Utils
import Directional
import Grid
import SlidingGrid
import GameTile

data GridDrawInfo a = GridDrawInfo { tileSize :: Point a
                                   , gridOrigin :: Point a } deriving Show

data GridState = GridState { gridPlayer :: Point Int
                           , gridPlayerTileWasSliding :: Bool
                           , gridTiles :: TileZipper GameTile } deriving Show

instance Functor GridDrawInfo where
  fmap f (GridDrawInfo scale origin) = GridDrawInfo (pairMap f scale) (pairMap f origin)

fromTileSize :: (a -> b) -> GridDrawInfo a -> Point b
fromTileSize f i = pairMap f (tileSize i)

fromGridOrigin :: (a -> b) -> GridDrawInfo a -> Point b
fromGridOrigin f i = pairMap f (gridOrigin i)

canMove :: Point Int -> GridDirection -> TileZipper GameTile -> Bool
canMove player dir tile = if player == gridCoord tile
                          then couldMovePlayer dir tile
                          else isJust (slideList dir tile)

couldMovePlayer :: GridDirection -> TileZipper GameTile -> Bool
couldMovePlayer dir tile = case tileM' of
  Nothing -> False
  Just tile' -> test dir gameTile && test dir' gameTile'
    where gameTile' = tileItem tile'
  where tileM' = moveNext dir tile
        dir' = reverseDirection dir
        gameTile = tileItem tile
        test d mt = case mt of
          Nothing -> False
          Just (GameTile paths _) -> extract d paths

movePlayer :: GridDirection -> GridState -> GridState
movePlayer dir (GridState player wasSliding tiles) =
  GridState player' wasSliding' tiles'
  where tiles' = fromMaybe (error "this move should not have failed") (moveNext dir $
                 if wasSliding then changeItem toSlidingTile tiles
                 else tiles)
        wasSliding' = tileIsSliding $ gridItem tiles'
        player' = gridCoord tiles'

