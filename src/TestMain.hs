module TestMain where

import Data.Maybe
import Debug.Trace
import Grid
import SlidingGrid
import SmoothSlidingGrid
import FreezableT
import Directional

printMaybeTiles :: Show a => Maybe (TileZipper a) -> IO ()
printMaybeTiles mz = case mz of
  Nothing -> print mz
  Just z -> printTiles z

twoString :: (Show a, Show b) => a -> b -> String
twoString a b = "(" ++ show a ++ ", " ++ show b ++ ")"

testFoldlUntil :: IO ()
testFoldlUntil = print $ foldlUntil (\a b -> trace (twoString (overThree a b) $ twoString a b) (overThree a b)) (\a b -> trace (twoString a b) (a + b)) 0 [1, 2, 3, 4, 5]
  where overThree _ b = b > 3

main :: IO ()
main = do
  --print items
  --print $ safePrev items
  --print $ safeNext items
  printTiles tiles'
  putStrLn ""
  printMaybeTiles (slide_ GridRight tiles)
  --printMaybeTiles (slide_ GridDown tiles)
  --printMaybeTiles (slide_ GridLeft tiles)
  --printMaybeTiles (slide_ GridUp tiles)
  putStrLn ""
  --printMaybeTiles (slide_ GridRight tiles')
  --printMaybeTiles (slide_ GridDown tiles')
  --printMaybeTiles (slide_ GridLeft tiles')
  printMaybeTiles (slide_ GridUp tiles')
  --printMaybeTiles (slide_ GridRight tiles)
  --print $ intersect ((0, 0), (2, 5)) (Rectangular 1 1 (-1) (-1))
  --print $ intersect ((0, 0), (2, 2)) (Rectangular 1 1 (-1) (-1))
  --print $ intersect ((0, 0), (2, 0)) (Rectangular 1 1 (-1) (-1))
  --print $ toBoundingRect (10, 10) (0, 0) (5, 5) GridRight
  putStrLn ""
  print drag
  printMaybeTiles mz
  putStrLn ""
  print drag'
  printMaybeTiles mz'
  putStrLn ""
  printMaybeTiles $ clickTile (10, 10) (0, 0) (15, 15) tiles
  print origin'
  print bounds
  print intersection
  putStrLn ""
  print $ toBoundingRect scale origin (5, 5) GridRight
  print $ toBoundingRect scale origin (5, 5) GridDown
  print $ toBoundingRect scale origin (5, 5) GridLeft
  print $ toBoundingRect scale origin (5, 5) GridUp

  where tiles = fromRows [ [ SlidingTile ()
                           , EmptyTile
                           , FixedTile () ]
                         , [SlidingTile ()
                           , SlidingTile ()
                           , SlidingTile () ]
                         , [SlidingTile ()
                           , SlidingTile ()
                           , SlidingTile () ] ]
        tiles' = fromJust $ Grid.moveTo (1, 1) tiles
        (drag, mz) = applyDrag (10, 10) (0, 0) ((5, 5), (20, 7)) tiles
        (drag', mz') = applyDrag (10, 10) (0, 0) ((15, 15), (17, 0)) tiles
        scale = (10, 10)
        origin = (0, 0)
        click = (15, 15)
        end = (17, 0)
        tile = clickTile scale origin click tiles
        origin' = fmap (tileOrigin scale origin) tile
        bounds = fmap (\o -> toBoundingRect scale o click dir) origin'
        intersection = fmap (drag `intersect`) bounds
        dir = SmoothSlidingGrid.toDirection (click, end)
