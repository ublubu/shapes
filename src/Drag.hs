module Drag where

import Control.Applicative
import Control.Monad
import Data.Maybe
import SDL.Geometry
import Directional
import GameState
import Utils.Utils

class Draggable d where
  setCoord :: Point Int -> d -> Maybe d
  applyMove :: GridDirection -> d -> d
  checkMove :: GridDirection -> d -> Bool

data DragResult a b = DragResult (Point (Point a)) b (Maybe GridDirection)

toTileCoord :: RealFrac a => GridDrawInfo a -> Point a -> Point Int
toTileCoord (GridDrawInfo scale origin) click = pairMap floor (pairAp (pairMap (/) (click - origin)) scale)

clickTile :: (RealFrac a, Draggable t) =>  GridDrawInfo a -> Point a -> t -> Maybe t
clickTile drawInfo click = setCoord (toTileCoord drawInfo click)

dragDistance :: Num a => Point (Point a) -> Point a
dragDistance (x, x') = x' - x

dragDirection :: (Num a, Ord a) => Point (Point a) -> GridDirection
dragDirection d
  | abs x > abs y = if x > 0 then GridRight
                    else GridLeft
  | y > 0 = GridDown
  | otherwise = GridUp
  where (x, y) = dragDistance d

-- a rectangle on the edge of the tile, but not outside
tileRect :: Num a => GridDrawInfo a -> Point Int -> Rectangular a
tileRect (GridDrawInfo scale origin) coord =
  (+) <$> fromBottomRight scale' <*> degenerateRect origin'
  where scale' = scale - (1, 1)
        coord' = pairMap fromIntegral coord
        origin' = (origin + (scale * coord'))

-- a rectangle just outside the tile
tileBoundingRect :: Num a => GridDrawInfo a -> Point Int -> Rectangular a
tileBoundingRect drawInfo coord = (+) <$> signedRect <*> tileRect drawInfo coord

toBoundingRect :: (Ord a, RealFrac a) => GridDrawInfo a -> Point Int -> Point a -> GridDirection -> Rectangular a
toBoundingRect drawInfo@(GridDrawInfo scale _) coord click dir = extend <*> rect' <*> scaledSignedRect scale
  where rect = tileBoundingRect drawInfo coord
        click' = (+) <$> signedRect <*> degenerateRect click
        rect' = clip <*> click' <*> rect
        clip = injectOriented_ (GridOriented dir const) (\_ x -> x)
        extend = injectOriented_ (GridOriented dir (+)) const

dragResultHasMove :: DragResult a b -> Bool
dragResultHasMove (DragResult _ _ m) = isJust m

resultDrag :: DragResult a b -> Point (Point a)
resultDrag (DragResult drag _ _) = drag

resultTile :: DragResult a b -> b
resultTile (DragResult _ tile _) = tile

resultDir :: DragResult a b -> Maybe GridDirection
resultDir (DragResult _ _ dir) = dir

dragResult :: (Ord a, RealFrac a, Draggable b) => GridDrawInfo a -> Point (Point a) -> b -> DragResult a b
dragResult drawInfo drag@(click, end) z =
  case intersection of
    Nothing -> DragResult drag z Nothing
    Just (GridOriented intersectDir click') ->
      if intersectDir == dir && canDrag then DragResult drag' tile' (Just dir)
      else DragResult drag' tile' Nothing
      where drag' = (click', end)
            tile' = fromMaybe z tileM
  where dir = dragDirection drag
        coord = toTileCoord drawInfo click
        tileM = setCoord coord z
        canDrag = case tileM of
          Nothing -> False
          Just tile -> checkMove dir tile
        rect = if canDrag then toBoundingRect drawInfo coord click dir
               else tileBoundingRect drawInfo coord
        intersection = drag `intersect` rect

completelyApplyDrag :: (Ord a, RealFrac a, Draggable b) => GridDrawInfo a -> Point (Point a) -> b -> (Point (Point a), b)
completelyApplyDrag drawInfo drag z =
  if drag == drag' then (drag, z)
  else completelyApplyDrag drawInfo drag' z'
  where (DragResult drag' clickZ move) = dragResult drawInfo drag z
        z' = maybe clickZ (`applyMove` clickZ) move

