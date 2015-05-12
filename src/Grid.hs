module Grid where

import Data.List.Zipper
import Data.Foldable hiding (toList)
import qualified Data.Foldable as Fld
import Data.Monoid
import Control.Monad
import SDL.Geometry hiding (moveTo)
import Directional
import FreezableT

type IndexedZipper a = (Int, Zipper a)
type GridZipper a = IndexedZipper (IndexedZipper a)

safePrev :: IndexedZipper a -> Maybe (IndexedZipper a)
safePrev (i, z) = do
  guard (not $ beginp z)
  return $ (i - 1, left z)

safeNext :: IndexedZipper a -> Maybe (IndexedZipper a)
safeNext (i, z) = do
  guard (not $ endp z')
  return (i + 1, z')
    where z' = right z

gridCoord :: GridZipper a -> Point Int
gridCoord z = (fst $ gridRow z, fst z)

gridRow :: GridZipper a -> IndexedZipper a
gridRow z = cursor (snd z)

gridItem :: GridZipper a -> a
gridItem z = cursor $ snd (gridRow z)

replaceCurrent :: IndexedZipper a -> GridZipper a -> GridZipper a
replaceCurrent r = fmap (replace r)

replaceItem :: a -> GridZipper a -> GridZipper a
replaceItem i z = replaceCurrent (fmap (replace i) (gridRow z)) z

changeItem :: (a -> a) -> GridZipper a -> GridZipper a
changeItem f z = replaceItem (f . gridItem $ z) z

indexedFromList :: [a] -> IndexedZipper a
indexedFromList xs = (0, fromList xs)

indexedToList :: IndexedZipper a -> [a]
indexedToList = toList . snd

fromRows :: [[a]] -> GridZipper a
fromRows rows = indexedFromList $ fmap indexedFromList rows

toRowsFrom :: GridZipper a -> [[a]]
toRowsFrom = fmap indexedToList . indexedToList

moveTo :: Point Int -> GridZipper a -> Maybe (GridZipper a)
moveTo t@(x', y') z
  | y == y' =
    if x == x' then Just z else
      if x < x' then moveTo t =<< moveRight z
        else moveTo t =<< moveLeft z
  | y < y' = moveTo t =<< moveDownRow z
  | otherwise = moveTo t =<< moveUpRow z
    where (x, y) = gridCoord z

moveInRow :: (IndexedZipper a -> Maybe (IndexedZipper a)) -> GridZipper a -> Maybe (GridZipper a)
moveInRow advanceItem z = do
  iz <- advanceItem (gridRow z)
  return $ replaceCurrent iz z

moveRight :: GridZipper a -> Maybe (GridZipper a)
moveRight = moveInRow safeNext

moveLeft :: GridZipper a -> Maybe (GridZipper a)
moveLeft = moveInRow safePrev

moveDownRow :: GridZipper a -> Maybe (GridZipper a)
moveDownRow = safeNext

moveUpRow :: GridZipper a -> Maybe (GridZipper a)
moveUpRow = safePrev

moveDown :: GridZipper a -> Maybe (GridZipper a)
moveDown z = moveTo (x, y + 1) z
  where (x, y) = gridCoord z

moveUp :: GridZipper a -> Maybe (GridZipper a)
moveUp z = moveTo (x, y - 1) z
  where (x, y) = gridCoord z

moveDownZ :: GridZipper a -> Maybe (GridZipper a)
moveDownZ z = moveTo (0, y + 1) z
  where (_, y) = gridCoord z

moveUpZ :: GridZipper a -> Maybe (GridZipper a)
moveUpZ z = moveTo (0, y - 1) z
  where (_, y) = gridCoord z

moveOne :: Rectangular (GridZipper a -> Maybe (GridZipper a))
moveOne = Rectangular moveRight moveDown moveLeft moveUp

moveNext :: GridDirection -> GridZipper a -> Maybe (GridZipper a)
moveNext = flip extract moveOne

movePrev :: GridDirection -> GridZipper a -> Maybe (GridZipper a)
movePrev = flip extract (rotate . rotate $ moveOne)

gridMap :: (a -> b) -> GridZipper a -> GridZipper b
gridMap f = fmap . fmap . fmap . fmap $ f

gridFoldFrom :: (b -> GridZipper a -> b) -> b -> GridZipper a -> b
gridFoldFrom f a z = case moveRight z of
  Nothing -> case moveDownZ z of
    Nothing -> a'
    Just z' -> gridFoldFrom f a' z'
  Just z' -> gridFoldFrom f a' z'
  where a' = f a z

gridSequenceFrom :: Monad m => (GridZipper a -> m ()) -> GridZipper a -> m ()
gridSequenceFrom f = gridFoldFrom (\a z' -> f z' >> a) (return ())

data GridSequence a = GridSequence GridDirection (GridZipper a)

gridNext :: GridSequence a -> Maybe (GridSequence a)
gridNext (GridSequence dir z) = do
  z' <- moveNext dir z
  return $ GridSequence dir z'

gridPrev :: GridSequence a -> Maybe (GridSequence a)
gridPrev (GridSequence dir z) = do
  z' <- movePrev dir z
  return $ GridSequence dir z'

instance Foldable GridSequence where
  foldMap f (GridSequence dir z) = case moveNext dir z of
    Just z' -> mappend a (foldMap f (GridSequence dir z'))
    Nothing -> a
    where a = f $ gridItem z

applyChanges :: [a -> a] -> GridSequence a -> Maybe (GridSequence a)
applyChanges [] s = Just s
applyChanges (f:fs) (GridSequence dir z) = do
  zz <- moveNext dir $ changeItem f z
  gridPrev =<< applyChanges fs (GridSequence dir zz)

atCoord :: (GridZipper a -> Maybe (GridZipper a)) -> Point Int -> GridZipper a -> Maybe (GridZipper a)
atCoord f coordA z = do
  za <- moveTo coordA z
  f za
  where coordB = gridCoord z

