module Physics.Broadphase where

import Control.Lens (view)
import Data.Array (elems)
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Linear.Affine
import Linear.V2
import Physics.Contact
import Physics.ConvexHull
import Physics.SAT
import Physics.Linear
import Physics.World

type Aabb n = V2 (n, n)

aabbCheck :: (Ord n) => Aabb n -> Aabb n -> Bool
aabbCheck (V2 x y) (V2 x' y') = overlapTest x x' && overlapTest y y'

toAabb :: (Floating n, Ord n) => ConvexHull n -> Aabb n
toAabb hull = foldl1 mergeAabb aabbs
  where aabbs = fmap toAabb_ . elems . _hullVertices $ hull

toAabb_ :: P2 n -> Aabb n
toAabb_ = fmap (\a -> (a, a)) . view _Point

mergeAabb :: (Ord n) => Aabb n -> Aabb n -> Aabb n
mergeAabb a b = mergeRange <$> a <*> b

mergeRange :: (Ord n) => (n, n) -> (n, n) -> (n, n)
mergeRange (a, b) (c, d) = (min a c, max b d)

culledPairs :: (Floating n, Contactable n a, Ord n) => World a -> [WorldPair (a, a)]
culledPairs w = filter f (allPairs w)
  where aabbs = toAabbs w
        f (WorldPair (i, j) _) = fromMaybe False (do
          a <- IM.lookup i aabbs
          b <- IM.lookup j aabbs
          return (aabbCheck a b))

toAabbs :: (Floating n, Contactable n a, Ord n) => World a -> IM.IntMap (Aabb n)
toAabbs = fmap (toAabb . contactHull) . view worldObjs

culledKeys :: (Floating n, Contactable n a, Ord n) => World a -> [(Int, Int)]
culledKeys = fmap pairIndex . culledPairs
