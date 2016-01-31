module Physics.Broadphase where

import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Linear.Affine
import Linear.V2
import Physics.Contact
import Physics.Geometry
import Physics.World

type Aabb n = V2 (n, n)

aabbCheck :: (Ord n) => Aabb n -> Aabb n -> Bool
aabbCheck (V2 x y) (V2 x' y') = overlapTest x x' && overlapTest y y'

toAabb :: (Ord n) => ShapeInfo n -> Aabb n
toAabb (_, feats) = foldl1 mergeAabb aabbs
  where aabbs = fmap toAabb_ feats

toAabb_ :: Feature n (V2 n) -> Aabb n
toAabb_ = fmap (\a -> (a, a)) . view _Point . vertex . fst

mergeAabb :: (Ord n) => Aabb n -> Aabb n -> Aabb n
mergeAabb a b = mergeRange <$> a <*> b

mergeRange :: (Ord n) => (n, n) -> (n, n) -> (n, n)
mergeRange (a, b) (c, d) = (min a c, max b d)

culledPairs :: (Contactable n a, Ord n) => World a -> [WorldPair (a, a)]
culledPairs w = filter f (allPairs w)
  where aabbs = fmap (toAabb . contactHull) (view worldObjs w)
        f (WorldPair (i, j) _) = fromMaybe False (do
          a <- IM.lookup i aabbs
          b <- IM.lookup j aabbs
          return (aabbCheck a b))

culledKeys :: (Contactable n a, Ord n) => World a -> [(Int, Int)]
culledKeys = fmap pairIndex . culledPairs
