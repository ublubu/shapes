module TileRider.Drag where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Directional
import Geometry
import Utils.Utils

class Draggable d where
  setCoord :: Pair Int -> d -> Maybe d
  applyMove :: GridDirection -> d -> d
  checkMove :: GridDirection -> d -> Bool

data MoveAmount a = FullMove | PartialMove a deriving Show
type DragMove a = (GridDirection, MoveAmount a, Pair Int)
data DragResult a b = DragResult (Pair (Pair a)) b (Maybe (DragMove a)) deriving Show
data PartialMoveResult a = PartialMoveResult GridDirection a (Pair Int) deriving Show
data FullMoveResult = FullMoveResult GridDirection (Pair Int) deriving Show

toTileCoord :: RealFrac a => Transform a -> Pair a -> Pair Int
toTileCoord (Transform scale origin) click = fmap floor ((/) <$> (click - origin) <*> scale)

clickTile :: (RealFrac a, Draggable t) =>  Transform a -> Pair a -> t -> Maybe t
clickTile drawInfo click = setCoord (toTileCoord drawInfo click)

dragDistance :: Num a => Pair (Pair a) -> Pair a
dragDistance (Pair x x') = x' - x

dragDirection :: (Num a, Ord a, Draggable t) => Pair (Pair a) -> t -> Maybe (GridOriented a)
dragDirection d z = collapse (<) (toMaybe <$> trySlide <*> dist)
  where dist = degenerateRect (dragDistance d)
        canSlide = generateRect (`checkMove` z)
        trySlide = (&&) <$> ((>0) <$> ((*) <$> dist <*> signedRect)) <*> canSlide

-- a rectangle on the edge of the tile, but not outside
tileRect :: Num a => Transform a -> Pair Int -> Rectangular a
tileRect (Transform scale origin) coord =
  (+) <$> fromBottomRight scale' <*> degenerateRect origin'
  where scale' = scale - (Pair 1 1)
        coord' = fmap fromIntegral coord
        origin' = (origin + (scale * coord'))

-- a rectangle just outside the tile
tileBoundingRect :: Num a => Transform a -> Pair Int -> Rectangular a
tileBoundingRect drawInfo coord = (+) <$> signedRect <*> tileRect drawInfo coord

toBoundingRect :: (Ord a, RealFrac a) => Transform a -> Pair Int -> Pair a -> GridDirection -> Rectangular a
toBoundingRect drawInfo@(Transform scale _) coord click dir = extend <*> rect' <*> scaledSignedRect scale
  where rect = tileBoundingRect drawInfo coord
        click' = (+) <$> signedRect <*> degenerateRect click
        rect' = clip <*> click' <*> rect
        clip = injectOriented_ (GridOriented dir const) (\_ x -> x)
        extend = injectOriented_ (GridOriented dir (+)) const

dragResultHasFullMove :: DragResult a b -> Bool
dragResultHasFullMove (DragResult _ _ move) = maybe False test move
  where test (_, m, _) = case m of FullMove -> True
                                   _ -> False

resultDrag :: DragResult a b -> Pair (Pair a)
resultDrag (DragResult drag _ _) = drag

resultTile :: DragResult a b -> b
resultTile (DragResult _ tile _) = tile

resultDir :: DragResult a b -> Maybe GridDirection
resultDir (DragResult _ _ move) = fmap (\(x, _, _) -> x) move

dragResult :: (Ord a, RealFrac a, Draggable b) => Transform a -> Pair (Pair a) -> b -> DragResult a b
dragResult drawInfo drag@(Pair click end) z = case dirM of
  Just (GridOriented dir dragDist) -> case intersection of
    Nothing -> DragResult drag z $ Just (dir, PartialMove dragDist, coord)
    Just (GridOriented intersectDir click') ->
      if completedMove then DragResult drag' tile' $ Just (dir, FullMove, coord)
      else DragResult drag' tile' Nothing
      where drag' = Pair click' end
            tile' = fromMaybe z tileM
            completedMove = intersectDir == dir || shouldConvert drawInfo (GridOriented dir dragDist)
    where intersection = drag `intersect` (toBoundingRect drawInfo coord click dir)
  Nothing -> case intersection of
    Nothing -> DragResult drag z Nothing
    Just (GridOriented intersectDir click') -> DragResult drag' tile' Nothing
      where drag' = Pair click' end
            tile' = fromMaybe z tileM
    where intersection = drag `intersect` (tileBoundingRect drawInfo coord)
  where dirM = dragDirection drag =<< tileM
        coord = toTileCoord drawInfo click
        tileM = setCoord coord z

completelyApplyDrag :: (Ord a, RealFrac a, Draggable b) => Transform a -> Pair (Pair a) -> b -> (Pair (Pair a), Maybe (PartialMoveResult a), b)
completelyApplyDrag drawInfo drag z =
  case move of
    Nothing -> (drag', Nothing, clickZ)
    Just (dir, amount, coord) -> case amount of
      FullMove -> completelyApplyDrag drawInfo drag' z'
        where z' = applyMove dir clickZ
      PartialMove x -> (drag', Just (PartialMoveResult dir x coord), clickZ)
  where (DragResult drag' clickZ move) = dragResult drawInfo drag z

shouldConvert :: (Ord a, RealFrac a) => Transform a -> GridOriented a -> Bool
shouldConvert (Transform scale _) (GridOriented dir dist) =
  (axisScale / 2) < abs dist
  where axisScale = extract dir (degenerateRect scale)

convertPartialMove :: (Ord a, RealFrac a) => Transform a -> PartialMoveResult a -> Maybe FullMoveResult
convertPartialMove drawInfo (PartialMoveResult dir dist coord) =
  if shouldConvert drawInfo (GridOriented dir dist) then Just $ FullMoveResult dir coord else Nothing

applyFullMove :: Draggable t => FullMoveResult -> t -> t
applyFullMove (FullMoveResult dir coord) z = applyMove dir z'
  where z' = fromMaybe (error "full move should've been checked beforehand") $
             setCoord coord z

