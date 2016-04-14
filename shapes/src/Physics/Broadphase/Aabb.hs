{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Physics.Broadphase.Aabb where

import GHC.Prim (Double#, (>##), (<##))
import GHC.Types (Double(D#), isTrue#)
import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens ((^.), itoListOf)
import Data.Array (elems)
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving
import Physics.Linear
import Physics.Contact.ConvexHull
import Physics.World.Class

import Utils.Descending

-- TODO: explore rewrite rules or other alternatives to manually using primops

data Bounds = Bounds { _bmin :: Double#
                     , _bmax :: Double#
                     } deriving (Show, Eq)

derivingUnbox "Bounds"
  [t| Bounds -> (Double, Double) |]
  [| \Bounds{..} -> (D# _bmin, D# _bmax) |]
  [| \(D# bmin', D# bmax') -> Bounds bmin' bmax' |]

instance NFData Bounds where
  rnf (Bounds _ _) = ()

data Aabb = Aabb { _aabbx :: {-# UNPACK #-} !Bounds
                 , _aabby :: {-# UNPACK #-} !Bounds
                 } deriving (Eq, Generic, NFData)

derivingUnbox "Aabb"
  [t| Aabb -> (Bounds, Bounds) |]
  [| \Aabb{..} -> (_aabbx, _aabby) |]
  [| uncurry Aabb |]

instance Show Aabb where
  show (Aabb (Bounds x0 x1) (Bounds y0 y1)) =
    "Aabb " ++ show (D# x0, D# x1) ++ " " ++ show (D# y0, D# y1)

makeAabb :: Double -> Double -> Double -> Double -> Aabb
makeAabb (D# a) (D# b) (D# c) (D# d) =
  Aabb (Bounds a b) (Bounds c d)

boundsOverlap :: Bounds -> Bounds -> Bool
boundsOverlap (Bounds a b) (Bounds c d) =
  not $ isTrue# (c >## b) || isTrue# (d <## a)
{-# INLINE boundsOverlap #-}

aabbCheck :: Aabb -> Aabb -> Bool
aabbCheck (Aabb xBounds yBounds) (Aabb xBounds' yBounds') =
  boundsOverlap xBounds xBounds' && boundsOverlap yBounds yBounds'
{-# INLINE aabbCheck #-}

toAabb :: ConvexHull -> Aabb
toAabb hull = foldl1 mergeAabb aabbs
  where aabbs = fmap toAabb_ . elems . _hullVertices $ hull
{-# INLINE toAabb #-}

toAabb_ :: P2 -> Aabb
toAabb_ (P2 (V2 a b))= Aabb (Bounds a a) (Bounds b b)
{-# INLINE toAabb_ #-}

mergeAabb :: Aabb -> Aabb -> Aabb
mergeAabb (Aabb ax ay) (Aabb bx by) =
  Aabb (mergeRange ax bx) (mergeRange ay by)
{-# INLINE mergeAabb #-}

mergeRange :: Bounds -> Bounds -> Bounds
mergeRange (Bounds a b) (Bounds c d) = Bounds minx maxx
  where minx = if isTrue# (a <## c) then a else c
        maxx = if isTrue# (b >## d) then b else d
{-# INLINE mergeRange #-}

-- assumes wObjs is in ascending key order
toAabbs :: (V.Unbox k, PhysicsWorld k w o) => w -> V.Vector (k, Aabb)
toAabbs = V.fromList . fmap f . itoListOf wObjs
  where f (k, obj) = (k, toAabb $ obj ^. woShape)
{-# INLINE toAabbs #-}

unorderedPairs :: Int -> [(Int, Int)]
unorderedPairs n
  | n < 2 = []
  | otherwise = f (n - 1) (n - 2)
  where f 1 0 = [(1, 0)]
        f x 0 = (x, 0) : f (x - 1) (x - 2)
        f x y = (x, y) : f x (y - 1)
        {-# INLINE f #-}
{-# INLINE unorderedPairs #-}

culledKeys :: (V.Unbox k, PhysicsWorld k w o) => w -> Descending (k, k)
culledKeys w = Descending . catMaybes $ fmap f ijs
  where aabbs = toAabbs w
        ijs = unorderedPairs $ V.length aabbs
        f (i, j) = if aabbCheck a b then Just (i', j') else Nothing
          where (i', a) = aabbs V.! i
                (j', b) = aabbs V.! j
        {-# INLINE f #-}
{-# INLINE culledKeys #-}
