{-# LANGUAGE MagicHash #-}

module Physics.Broadphase.Opt.Aabb where

import GHC.Prim (Double#, (>##), (<##))
import GHC.Types (Double(D#), isTrue#)

import Control.Lens (view)
import Data.Array (elems)
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Physics.Linear.Opt
import Physics.Contact.Opt
import Physics.Contact.Opt.ConvexHull
import Physics.World.Opt

-- TODO: explore rewrite rules or other alternatives to manually using primops

data Bounds = Bounds { _bmin :: Double#
                     , _bmax :: Double#
                     }

data Aabb = Aabb { _aabbx :: {-# UNPACK #-} !Bounds
                 , _aabby :: {-# UNPACK #-} !Bounds
                 }

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

culledPairs :: (Contactable a) => World a -> [WorldPair (a, a)]
culledPairs w = filter f (allPairs w)
  where aabbs = toAabbs w
        f (WorldPair (i, j) _) = fromMaybe False (do
          a <- IM.lookup i aabbs
          b <- IM.lookup j aabbs
          return (aabbCheck a b))

toAabbs :: (Contactable a) => World a -> IM.IntMap Aabb
toAabbs = fmap (toAabb . contactHull) . view worldObjs

culledKeys :: (Contactable a) => World a -> [(Int, Int)]
culledKeys = fmap pairIndex . culledPairs
