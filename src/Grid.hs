module Grid where

import Data.List.Zipper
import Control.Monad

type RowZipper a = Zipper [a]

data GridZipper a = GridZipper (RowZipper a) (Zipper a) (Int, Int)

safePrev :: Zipper a -> Maybe (Zipper a)
safePrev z = do
  guard (not $ beginp z)
  return $ left z

safeNext :: Zipper a -> Maybe (Zipper a)
safeNext z = do
  guard (not $ endp z')
  return z'
    where z' = right z

itemCursor :: RowZipper a -> Maybe (Zipper a)
itemCursor = (fmap fromList) . safeCursor

fromRows :: [[a]] -> Maybe (GridZipper a)
fromRows rows = do
  itemZ <- itemCursor rowZ
  return $ GridZipper rowZ itemZ (0, 0)
  where rowZ = fromList rows

moveTo :: (Int, Int) -> GridZipper a -> Maybe (GridZipper a)
moveTo t@(x', y') z@(GridZipper r i (x, y))
  | y == y' =
    if x == x' then Just z else
      if x < x' then moveTo t =<< moveRight z
        else moveTo t =<< moveLeft z
  | y < y' =
    do r' <- safeNext r
       i' <- itemCursor r'
       moveTo t $ GridZipper r' i' (0, y + 1)
  | otherwise =
    do r' <- safePrev r
       i' <- itemCursor r'
       moveTo t $ GridZipper r' i' (0, y - 1)

moveRight :: GridZipper a -> Maybe (GridZipper a)
moveRight (GridZipper r i (x, y)) = do
  i' <- safeNext i
  return $ GridZipper r i' (x + 1, y)

moveLeft :: GridZipper a -> Maybe (GridZipper a)
moveLeft (GridZipper r i (x, y)) = do
  i' <- safePrev i
  return $ GridZipper r i' (x - 1, y)

moveDown :: GridZipper a -> Maybe (GridZipper a)
moveDown z@(GridZipper _ _ (x, y)) = moveTo (x, y + 1) z

moveUp :: GridZipper a -> Maybe (GridZipper a)
moveUp z@(GridZipper _ _ (x, y)) = moveTo (x, y - 1) z

item :: GridZipper a -> a
item (GridZipper _ iz _) = cursor iz

replaceItem :: a -> GridZipper a -> GridZipper a
replaceItem i (GridZipper rz iz t) = GridZipper rz (replace i iz) t


