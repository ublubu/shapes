module SlidingGrid where

import Data.Maybe
import Grid
import SDL.Geometry
import Directional
import FreezableT (foldlUntil, foldMapUntil)

data Tile a = EmptyTile | FixedTile a | SlidingTile a deriving Show

instance Functor Tile where
  fmap f t = case t of
    EmptyTile -> EmptyTile
    FixedTile x -> FixedTile (f x)
    SlidingTile x -> SlidingTile (f x)

extractFromTile :: Tile a -> Maybe a
extractFromTile tile = case tile of
  EmptyTile -> Nothing
  FixedTile x -> Just x
  SlidingTile x -> Just x

tileIsSliding :: Tile a -> Bool
tileIsSliding (SlidingTile _) = True
tileIsSliding _ = False

tileIsFixed :: Tile a -> Bool
tileIsFixed (FixedTile _) = True
tileIsFixed _ = False

tileIsEmpty :: Tile a -> Bool
tileIsEmpty EmptyTile = True
tileIsEmpty _ = False

toSlidingTile :: Tile a -> Tile a
toSlidingTile (FixedTile a) = SlidingTile a
toSlidingTile (SlidingTile a) = SlidingTile a
toSlidingTile EmptyTile = error "can't convert empty tile to sliding"

toFixedTile :: Tile a -> Tile a
toFixedTile (SlidingTile a) = FixedTile a
toFixedTile (FixedTile a) = FixedTile a
toFixedTile EmptyTile = error "can't convert empty tile to fixed"

type TileZipper a = GridZipper (Tile a)

tileItem :: TileZipper a -> Maybe a
tileItem = extractFromTile . gridItem

tilesMap :: (a -> b) -> TileZipper a -> TileZipper b
tilesMap f = gridMap . fmap $ f

tileToString :: Show a => Tile a -> String
tileToString t = case t of
  EmptyTile -> "Empty"
  FixedTile x -> "Fixed (" ++ show x ++ ")"
  SlidingTile x -> "Slide (" ++ show x ++ ")"

rowToString :: Show a => [Tile a] -> String
rowToString = foldl f ""
  where f accum t = case t of
          EmptyTile -> accum ++ "Empty "
          FixedTile x -> accum ++ "Fixed (" ++ show x ++ ") "
          SlidingTile x -> accum ++ "Slide (" ++ show x ++ ") "

tilesToString :: Show a => TileZipper a -> String
tilesToString z = show (gridCoord z) ++ " " ++ show (gridItem z) ++ gridString
  where f accum row = accum ++ "\n" ++ rowToString row
        gridString = foldl f "" (toRowsFrom z)

printTiles :: Show a => TileZipper a -> IO ()
printTiles = putStrLn . tilesToString

slide_ :: GridDirection -> TileZipper a -> Maybe (TileZipper a)
slide_ dir z = Grid.moveTo coord =<< foldlUntil test accum pushEmpty s z
  where s = GridSequence dir z
        test _ t = case t of
          EmptyTile -> True
          FixedTile _ -> True
          SlidingTile _ -> False
        accum f x z0 = case x of
          EmptyTile -> f z0
          FixedTile _ -> Nothing
          SlidingTile _ -> do
            -- run the last transformer on the first tile to get the last tile
            zM <- f z0
            zN <- moveNext dir zM -- advance to this tile
            -- change this tile and return it as the new "last" tile
            case gridItem zN of
              FixedTile _ -> Nothing
              j -> return $ replaceItem x zN
        pushEmpty z0 = return $ replaceItem EmptyTile z0
        coord = gridCoord z

data SlideList a = SlideList [TileZipper a] (TileZipper a) deriving Show

sliders :: SlideList a -> [TileZipper a]
sliders (SlideList zs _) = zs

theEmpty :: SlideList a -> TileZipper a
theEmpty (SlideList _ e) = e

slideList :: GridDirection -> TileZipper a -> Maybe (SlideList a)
slideList dir z = case x of
  EmptyTile -> return $ SlideList [] z
  FixedTile _ -> Nothing
  SlidingTile _ -> do
    z' <- moveNext dir z
    zs' <- slideList dir z'
    return $ SlideList (z:sliders zs') (theEmpty zs')
  where x = gridItem z

--returns something if it was able to perform the map
slideMap_ :: GridDirection -> (TileZipper a -> Tile a) -> SlideList a -> TileZipper a -> Maybe (TileZipper a)
slideMap_ dir f l@(SlideList ss e) zz = case ss of
  [] -> return $ replaceItem (f e) zz
  x:xs -> do
    zz' <- slideMap_ dir f (SlideList xs e) =<< moveNext dir zz
    fmap (replaceItem $ f x) (movePrev dir zz')

slideMap :: GridDirection -> (TileZipper a -> Tile a) -> TileZipper a -> Maybe (TileZipper a)
slideMap dir f z = do l <- slideList dir z
                      return $ fromMaybe (error "something broke the map")
                        (slideMap_ dir f l z)

slideInto :: GridDirection -> Tile a -> TileZipper a -> Maybe (TileZipper a)
slideInto dir x z = case x' of
  SlidingTile _ -> do
    z' <- moveNext dir z
    zz' <- slideInto dir x' z'
    fmap (replaceItem x) (movePrev dir zz')
  EmptyTile -> Just $ replaceItem x z
  FixedTile _ -> Nothing -- can't slide into a fixed tile
  where x' = gridItem z

pushWith :: (Tile b -> Tile b) -> GridSequence (Tile b) -> Maybe (GridSequence (Tile b))
pushWith f gs = applyChanges (foldMapUntil test f' gs) gs
  where f' _ = [f]
        test x = case x of
          EmptyTile -> False
          _ -> True

