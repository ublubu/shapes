module SlidingGrid where

import Grid
import FreezableT (foldMapUntil)

data Tile a = EmptyTile | FixedTile a | SlidingTile a

type TileZipper a = GridZipper (Tile a)
type TileZipperTrans a = TileZipper a -> Maybe (TileZipper a)

slideRight :: TileZipper a -> Maybe (TileZipper a)
slideRight z = do
  z' <- moveRight z
  zz' <- slideInto moveRight moveLeft x z'
  fmap (replaceItem EmptyTile) (moveLeft zz')
  where x = item z

slideInto :: TileZipperTrans a -> TileZipperTrans a -> Tile a -> TileZipper a -> Maybe (TileZipper a)
slideInto next prev x z = case x' of
  SlidingTile _ -> do
    z' <- next z
    zz' <- slideInto next prev x' z'
    fmap (replaceItem x) (prev zz')
  EmptyTile -> Just $ replaceItem x z
  FixedTile _ -> Nothing -- can't slide into a fixed tile
  where x' = item z

slideRightInto :: Tile a -> TileZipper a -> Maybe (TileZipper a)
slideRightInto = slideInto moveRight moveLeft

slideLeftInto :: Tile a -> TileZipper a -> Maybe (TileZipper a)
slideLeftInto = slideInto moveLeft moveRight

slideDownInto :: Tile a -> TileZipper a -> Maybe (TileZipper a)
slideDownInto = slideInto moveDown moveUp

slideUpInto :: Tile a -> TileZipper a -> Maybe (TileZipper a)
slideUpInto = slideInto moveUp moveDown

class Connectable c where
  connected :: c -> c -> Bool

pushWith :: (Tile b -> Tile b) -> GridSequence (Tile b) -> Maybe (GridSequence (Tile b))
pushWith f gs = applyChanges (foldMapUntil test f' gs) gs
  where f' _ = [f]
        test x = case x of
          EmptyTile -> False
          _ -> True

