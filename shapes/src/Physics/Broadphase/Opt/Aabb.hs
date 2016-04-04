{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Physics.Broadphase.Opt.Aabb where

import GHC.Prim (Double#, (>##), (<##))
import GHC.Types (Double(D#), isTrue#)

import Control.Lens (over, _2, (^?), (%~), (&))
import Data.Array (elems)
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving
import Physics.Linear.Opt
import Physics.Contact.Opt
import Physics.Contact.Opt.ConvexHull
import Physics.World.Opt

import Utils.Descending
import Utils.Utils

-- TODO: explore rewrite rules or other alternatives to manually using primops

data Bounds = Bounds { _bmin :: Double#
                     , _bmax :: Double#
                     }

derivingUnbox "Bounds"
  [t| Bounds -> (Double, Double) |]
  [| \Bounds{..} -> (D# _bmin, D# _bmax) |]
  [| \(D# bmin', D# bmax') -> Bounds bmin' bmax' |]

data Aabb = Aabb { _aabbx :: {-# UNPACK #-} !Bounds
                 , _aabby :: {-# UNPACK #-} !Bounds
                 }

derivingUnbox "Aabb"
  [t| Aabb -> (Bounds, Bounds) |]
  [| \Aabb{..} -> (_aabbx, _aabby) |]
  [| uncurry Aabb |]

instance Show Aabb where
  show (Aabb (Bounds x0 x1) (Bounds y0 y1)) =
    "Aabb " ++ show (D# x0, D# x1) ++ " " ++ show (D# y0, D# y1)

boundsOverlap :: Bounds -> Bounds -> Bool
boundsOverlap (Bounds a b) (Bounds c d) =
  not $ isTrue# (c >## b) || isTrue# (d <## a)

aabbCheck :: Aabb -> Aabb -> Bool
aabbCheck (Aabb xBounds yBounds) (Aabb xBounds' yBounds') =
  boundsOverlap xBounds xBounds' && boundsOverlap yBounds yBounds'

toAabb :: ConvexHull -> Aabb
toAabb hull = foldl1 mergeAabb aabbs
  where aabbs = fmap toAabb_ . elems . _hullVertices $ hull

toAabb_ :: P2 -> Aabb
toAabb_ (P2 (V2 a b))= Aabb (Bounds a a) (Bounds b b)

mergeAabb :: Aabb -> Aabb -> Aabb
mergeAabb (Aabb ax ay) (Aabb bx by) =
  Aabb (mergeRange ax bx) (mergeRange ay by)

mergeRange :: Bounds -> Bounds -> Bounds
mergeRange (Bounds a b) (Bounds c d) = Bounds minx maxx
  where minx = if isTrue# (a <## c) then a else c
        maxx = if isTrue# (b >## d) then b else d

toAabbs :: (Contactable a) => World a -> V.Vector (Int, Aabb)
toAabbs = V.fromList . over (traverse . _2) (toAabb . contactHull) . IM.toList . _worldObjs

unorderedPairs :: Int -> [(Int, Int)]
unorderedPairs n
  | n < 2 = []
  | otherwise = f (n - 1) (n - 2)
  where f 1 0 = [(1, 0)]
        f x 0 = (x, 0) : f (x - 1) (x - 2)
        f x y = (x, y) : f x (y - 1)

culledKeys :: (Contactable a) => World a -> Descending (Int, Int)
culledKeys w = Descending . catMaybes $ fmap f ijs
  where aabbs = toAabbs w
        ijs = unorderedPairs $ V.length aabbs
        f (i, j) = if aabbCheck a b then Just (i', j') else Nothing
          where (i', a) = aabbs V.! i
                (j', b) = aabbs V.! j

culledPairs :: (Contactable a) => World a -> Descending (WorldPair (a, a))
culledPairs world =
  culledKeys world & descList %~ catMaybes . fmap f
  where f ij = WorldPair ij <$> world ^? worldPair ij
