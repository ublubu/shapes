{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}

module Physics.Broadphase.Grid where

import           GHC.Generics                 (Generic)
import           GHC.Types                    (Double (D#))

import           Control.DeepSeq
import           Control.Lens
import           Data.Foldable                (foldl')
import qualified Data.IntMap.Strict           as IM
import           Data.List                    (sortBy)
import           Data.Maybe                   (mapMaybe)
import qualified Data.Vector.Unboxed          as V
import           Data.Vector.Unboxed.Deriving

import           Physics.Broadphase.Aabb      (Aabb (..), Bounds (..),
                                               aabbCheck, toTaggedAabbs)
import qualified Physics.Constraint           as C
import           Physics.Contact.ConvexHull
import           Physics.World
import           Physics.World.Object
import           Utils.Descending
import           Utils.Utils

{- |
The grid is indexed in row-major order:

3 4 5
0 1 2

(where X is horizontal and Y is vertical)

* The grid is only used for shape queries, so it should only contain AABBs.
* We may want a reverse-lookup from shape ID to grid squares in the future.
-}
data Grid = Grid
  { _gridSquares :: IM.IntMap (IM.IntMap TaggedAabb)
  , _gridX       :: !GridAxis
  , _gridY       :: !GridAxis
  } deriving (Eq, Show, Generic, NFData)

data GridAxis = GridAxis
  { _gridLength :: !Int
  , _gridUnit   :: !Double
  , _gridOrigin :: !Double
  } deriving (Eq, Show, Generic, NFData)

data TaggedAabb = TaggedAabb
  { _taggedStatic :: !Bool
  , _taggedBox    :: !Aabb
  } deriving (Eq, Show, Generic, NFData)

makeLenses ''Grid
makeLenses ''GridAxis

toGrid :: (GridAxis, GridAxis) -> World usr -> Grid
toGrid axes@(xAxis, yAxis) w = Grid (fromTaggedAabbs axes taggedAabbs) xAxis yAxis
  where taggedAabbs = toTaggedAabbs isStatic w
        isStatic WorldObj{..} = C.isStatic $ C._physObjInvMass _worldPhysObj

culledKeys :: Grid -> Descending (Int, Int)
culledKeys Grid{..} = Descending . uniq . sortBy f . concat $ culledKeys' <$> IM.elems _gridSquares
  where f x y = case compare x y of LT -> GT
                                    EQ -> EQ
                                    GT -> LT

culledKeys' :: IM.IntMap TaggedAabb -> [(Int, Int)]
culledKeys' square = mapMaybe colliding $ allPairs $ IM.toDescList square
  where colliding ((_, (TaggedAabb True _)), (_, (TaggedAabb True _))) = Nothing
        -- ^ Don't check two static shapes for collision.
        colliding ((a, (TaggedAabb _ boxA)), (b, (TaggedAabb _ boxB)))
          | aabbCheck boxA boxB = Just (a, b)
          | otherwise = Nothing

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x:xs) = f [] x xs
  where f accumPairs first [] = accumPairs
        f accumPairs first remaining@(x:xs) = f (foldl' g accumPairs remaining) x xs
          where g accumPairs x = (first, x):accumPairs

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:[]) = [x]
uniq (x:y:rest)
  | x == y = uniq (x:rest)
  | otherwise = x : uniq (y:rest)

fromTaggedAabbs :: (GridAxis, GridAxis) -> V.Vector (Int, Aabb, Bool) -> IM.IntMap (IM.IntMap TaggedAabb)
fromTaggedAabbs (x, y) = V.foldl' insertBox IM.empty
  where
    insertBox grid (key, box, isStatic) = foldl' insertBoxAt grid indices
      where
        indices = boxIndices (x, y) box
        insertBoxAt grid index =
          grid & at index . non IM.empty . at key .~ Just taggedBox
        taggedBox = TaggedAabb isStatic box

-- | Flatten a pair of axial indices to a single grid index.
flattenIndex :: Grid -> (Int, Int) -> Int
flattenIndex Grid{..} (x, y) = flattenIndex' _gridX (x, y)

-- | Flatten a pair of axial indices to a single grid index.
flattenIndex' :: GridAxis -> (Int, Int) -> Int
flattenIndex' xAxis@GridAxis{..} (x, y) = x + (y * _gridLength)

-- | Flattened grid index of a given point.
pointIndex :: Grid -> (Double, Double) -> Int
pointIndex grid@Grid{..} (x, y) = flattenIndex' _gridX (i, j)
  where i = axialIndex _gridX x
        j = axialIndex _gridY y

-- | Index along a single axis.
axialIndex :: GridAxis -> Double -> Int
axialIndex GridAxis{..} val =
  floor $ (val - _gridOrigin) / _gridUnit

-- | All flattened grid indices that match a given 'Aabb'.
boxIndices :: (GridAxis, GridAxis) -> Aabb -> [Int]
boxIndices (xAxis, yAxis) Aabb {..} = do
  x <- axisRange _aabbx xAxis
  y <- axisRange _aabby yAxis
  return $ flattenIndex' xAxis (x, y)
  where
    axisRange (Bounds min max) axis = [minIx .. maxIx]
      where
        minIx = axialIndex axis (D# min)
        maxIx = axialIndex axis (D# max)
