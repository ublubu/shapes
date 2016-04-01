{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Physics.Broadphase.Opt.Aabb where

import GHC.Types (Double(D#))

import Control.Lens (view, _2, _3, over, (^?))
import Data.Array (elems)
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Slice (Any(..))
import Data.Vector.Unboxed.Deriving
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Physics.Linear.Opt
import Physics.Contact.Opt
import Physics.Contact.Opt.ConvexHull
import Physics.World.Opt

data Bounds = Bounds { _bmin :: Double
                     , _bmax :: Double
                     } deriving Show

derivingUnbox "Bounds"
  [t| Bounds -> (Double, Double) |]
  [| \Bounds{..} -> (_bmin, _bmax) |]
  [| \(bmin', bmax') -> Bounds bmin' bmax' |]

data Aabb = Aabb { _aabbx :: Bounds
                 , _aabby :: Bounds
                 } deriving Show

derivingUnbox "Aabb"
  [t| Aabb -> (Bounds, Bounds) |]
  [| \Aabb{..} -> (_aabbx, _aabby) |]
  [| uncurry Aabb |]

culledPairs :: (Contactable a) => World a -> [WorldPair (a, a)]
culledPairs world =
  catMaybes (f <$> culledKeys world)
  where f ij = WorldPair ij <$> world ^? worldPair ij
{-# INLINE culledPairs #-}

culledKeys :: (Contactable a) => World a -> [(Int, Int)]
culledKeys = overlappingPairs . pairOverlapChecks . toAabbs
{-# INLINE culledKeys #-}

pairOverlapChecks :: R.Array R.U R.DIM1 (Int, Aabb) -> R.Array R.D R.DIM2 (Int, Int, Bool)
pairOverlapChecks a = R.zipWith f rows cols
  where rows = R.extend (Any :. dim) a
        cols = R.transpose rows
        (Z :. dim) = R.extent a
        {-# INLINE f #-}
        f (i, x) (j, y) = if i < j then (i, j, aabbCheck x y) else (i, j, False)
{-# INLINE pairOverlapChecks #-}

overlappingPairs :: (R.Source r (Int, Int, Bool)) => R.Array r R.DIM2 (Int, Int, Bool) -> [(Int, Int)]
overlappingPairs = fmap (\(x, y, _) -> (x, y)). filter (view _3) . R.toList
{-# INLINE overlappingPairs #-}

joinRepa :: (R.Shape sh, R.Source r e) => R.Array r (sh :. Int :. Int) e -> R.Array R.D (sh :. Int) e
joinRepa a = R.reshape (sh :. (nrows * ncols)) a
  where (sh :. nrows :. ncols) = R.extent a
{-# INLINE joinRepa #-}

flattenRepa :: (R.Shape sh, R.Source r e) => R.Array r sh e -> R.Array R.D (Z :. Int) e
flattenRepa a = R.reshape (Z :. R.size (R.extent a)) a
{-# INLINE flattenRepa #-}

toAabbs :: (Contactable a) => World a -> R.Array R.U R.DIM1 (Int, Aabb)
toAabbs world = R.fromListUnboxed (Z :. IM.size objs). over (traverse._2) (toAabb . contactHull) . IM.toList $ objs
  where objs = _worldObjs world
{-# INLINE toAabbs #-}

toAabb :: ConvexHull -> Aabb
toAabb hull = foldl1 mergeAabb aabbs
  where aabbs = fmap toAabb_ . elems . _hullVertices $ hull
{-# INLINE toAabb #-}

toAabb_ :: P2 -> Aabb
toAabb_ (P2 (V2 a' b')) = Aabb (Bounds a a) (Bounds b b)
  where a = D# a'
        b = D# b'
{-# INLINE toAabb_ #-}

mergeAabb :: Aabb -> Aabb -> Aabb
mergeAabb (Aabb ax ay) (Aabb bx by) =
  Aabb (mergeRange ax bx) (mergeRange ay by)
{-# INLINE mergeAabb #-}

mergeRange :: Bounds -> Bounds -> Bounds
mergeRange (Bounds a b) (Bounds c d) = Bounds minx maxx
  where minx = if a < c then a else c
        maxx = if b > d then b else d
{-# INLINE mergeRange #-}

aabbCheck :: Aabb -> Aabb -> Bool
aabbCheck (Aabb xBounds yBounds) (Aabb xBounds' yBounds') =
  boundsOverlap xBounds xBounds' && boundsOverlap yBounds yBounds'
{-# INLINE aabbCheck #-}

boundsOverlap :: Bounds -> Bounds -> Bool
boundsOverlap (Bounds a b) (Bounds c d) =
  not (c > b || d < a)
{-# INLINE boundsOverlap #-}
