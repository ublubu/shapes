module SlidingGrid where

import Data.Maybe
import Grid
import FreezableT (foldlUntil, foldMapUntil)

data Tile a = EmptyTile | FixedTile a | SlidingTile a deriving Show

instance Functor Tile where
  fmap f t = case t of
    EmptyTile -> EmptyTile
    FixedTile x -> FixedTile (f x)
    SlidingTile x -> SlidingTile (f x)

type TileZipper a = GridZipper (Tile a)
type TileZipperTrans a = GridZipperTrans (Tile a)

rowToString :: Show a => [Tile a] -> String
rowToString = foldl f ""
  where f accum t = case t of
          EmptyTile -> accum ++ "Empty "
          FixedTile x -> accum ++ "Fixed (" ++ show x ++ ") "
          SlidingTile x -> accum ++ "Slide (" ++ show x ++ ") "

tilesToString :: Show a => TileZipper a -> String
tilesToString z = show (gridCoord z) ++ " " ++ show (item z) ++ gridString
  where f accum row = accum ++ "\n" ++ rowToString row
        gridString = foldl f "" (toRowsFrom z)

printTiles :: Show a => TileZipper a -> IO ()
printTiles = putStrLn . tilesToString

slideRight :: TileZipper a -> Maybe (TileZipper a)
slideRight = slide moveRight moveLeft

slideLeft :: TileZipper a -> Maybe (TileZipper a)
slideLeft = slide moveLeft moveRight

slideDown :: TileZipper a -> Maybe (TileZipper a)
slideDown = slide moveDown moveUp

slideUp :: TileZipper a -> Maybe (TileZipper a)
slideUp = slide moveUp moveDown

data SlideDirection a = SlideDirection (TileZipperTrans a) (TileZipperTrans a)

rightSliding :: SlideDirection a
rightSliding = SlideDirection moveRight moveLeft

leftSliding :: SlideDirection a
leftSliding = SlideDirection moveLeft moveRight

downSliding :: SlideDirection a
downSliding = SlideDirection moveDown moveUp

upSliding :: SlideDirection a
upSliding = SlideDirection moveUp moveDown

slide :: TileZipperTrans a -> TileZipperTrans a -> TileZipper a -> Maybe (TileZipper a)
slide next prev z@(GridZipper _ _ coord) = moveTo coord =<< foldlUntil test accum pushEmpty s z
  where s = GridSequence next prev z
        test _ t = case t of
          EmptyTile -> False
          FixedTile _ -> False
          SlidingTile _ -> True
        accum f x z' = do
          zz' <- f z'
          z'' <- next zz'
          case item z'' of
            FixedTile _ -> Nothing
            _ -> return $ replaceItem x z''
        pushEmpty = return . replaceItem EmptyTile

data SlideList a = SlideList [TileZipper a] (TileZipper a) deriving Show

sliders :: SlideList a -> [TileZipper a]
sliders (SlideList zs _) = zs

theEmpty :: SlideList a -> TileZipper a
theEmpty (SlideList _ e) = e

slideList :: Show a => SlideDirection a -> TileZipper a -> Maybe (SlideList a)
slideList dir@(SlideDirection next _) z = case x of
  EmptyTile -> return $ SlideList [] z
  FixedTile _ -> Nothing
  SlidingTile _ -> do
    z' <- next z
    zs' <- slideList dir z'
    return $ SlideList (z:(sliders zs')) (theEmpty zs')
  where x = item z

--returns something if it was able to perform the map
slideMap_ :: SlideDirection a -> (TileZipper a -> Tile a) -> SlideList a -> TileZipper a -> Maybe (TileZipper a)
slideMap_ dir@(SlideDirection next prev) f l@(SlideList ss e) zz = case ss of
  [] -> return $ replaceItem (f e) zz
  x:xs -> do
    zz' <- slideMap_ dir f (SlideList xs e) =<< next zz
    fmap (replaceItem $ f x) (prev zz')

slideMap :: Show a => SlideDirection a -> (TileZipper a -> Tile a) -> TileZipper a -> Maybe (TileZipper a)
slideMap dir f z = do l <- slideList dir z
                      return $ fromMaybe (error "something broke the map")
                        (slideMap_ dir f l z)

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

