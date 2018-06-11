{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{- |
"Aabb" is "Axis-aligned bounding box".
The "broadphase" of collision detection is a conservative estimate of which bodies may be in contact.
-}
module Physics.Broadphase.Aabb where

import           GHC.Generics                 (Generic)
import           GHC.Prim                     (Double#, (+##), (-##), (<##),
                                               (>##))
import           GHC.Types                    (Double (D#), isTrue#)

import           Control.DeepSeq
import           Control.Lens                 (itoListOf, (^.))
import           Control.Monad.Primitive
import           Data.Array                   (elems)
import           Data.Maybe
import qualified Data.Vector.Unboxed          as V
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Physics.Constraint           as C
import           Physics.Contact
import           Physics.Contact.Circle
import           Physics.Contact.ConvexHull
import           Physics.Linear
import           Physics.World
import           Utils.Descending
import qualified Utils.EmptiesVector          as E

-- TODO: explore rewrite rules or other alternatives to manually using primops

-- | An interval, bounded above and below
data Bounds = Bounds { _bmin :: Double# -- ^ lower bound
                     , _bmax :: Double# -- ^ upper bound
                     } deriving (Eq, Generic)

instance NFData Bounds where
  rnf (Bounds _ _) = ()

derivingUnbox "Bounds"
  [t| Bounds -> (Double, Double) |]
  [| \Bounds{..} -> (D# _bmin, D# _bmax) |]
  [| \(D# bmin', D# bmax') -> Bounds bmin' bmax' |]

-- | An axis-aligned bounding box (AABB)
data Aabb = Aabb { _aabbx :: {-# UNPACK #-} !Bounds -- ^ bounds on x axis
                 , _aabby :: {-# UNPACK #-} !Bounds -- ^ bounds on y axis
                 } deriving (Eq, Generic, NFData)

derivingUnbox "Aabb"
  [t| Aabb -> (Bounds, Bounds) |]
  [| \Aabb{..} -> (_aabbx, _aabby) |]
  [| uncurry Aabb |]

instance Show Aabb where
  show (Aabb (Bounds x0 x1) (Bounds y0 y1)) =
    "Aabb " ++ show (D# x0, D# x1) ++ " " ++ show (D# y0, D# y1)

-- | Do a pair of intervals overlap?
boundsOverlap :: Bounds -> Bounds -> Bool
boundsOverlap (Bounds a b) (Bounds c d) =
  not $ isTrue# (c >## b) || isTrue# (d <## a)
{-# INLINE boundsOverlap #-}

-- | Do a pair of AABBs overlap?
aabbCheck :: Aabb -> Aabb -> Bool
aabbCheck (Aabb xBounds yBounds) (Aabb xBounds' yBounds') =
  boundsOverlap xBounds xBounds' && boundsOverlap yBounds yBounds'
{-# INLINE aabbCheck #-}

-- | Find the AABB for a convex polygon.
hullToAabb :: ConvexHull -> Aabb
hullToAabb hull = foldl1 mergeAabb aabbs
  where aabbs = fmap toAabb_ . elems . _hullVertices $ hull
{-# INLINE hullToAabb #-}

circleToAabb :: Circle -> Aabb
circleToAabb (Circle (P2 (V2 x y)) (D# r)) =
  Aabb (Bounds (x -## r) (x +## r)) (Bounds (y -## r) (y +## r))

toAabb :: Shape -> Aabb
toAabb (HullShape hull)     = hullToAabb hull
toAabb (CircleShape circle) = circleToAabb circle

-- | Get the (degenerate) AABB for a single point.
toAabb_ :: P2 -> Aabb
toAabb_ (P2 (V2 a b))= Aabb (Bounds a a) (Bounds b b)
{-# INLINE toAabb_ #-}

-- | Find the AABB of a pair of AABBs.
mergeAabb :: Aabb -> Aabb -> Aabb
mergeAabb (Aabb ax ay) (Aabb bx by) =
  Aabb (mergeRange ax bx) (mergeRange ay by)
{-# INLINE mergeAabb #-}

-- | Find the interval that contains a pair of intervals.
mergeRange :: Bounds -> Bounds -> Bounds
mergeRange (Bounds a b) (Bounds c d) = Bounds minx maxx
  where minx = if isTrue# (a <## c) then a else c
        maxx = if isTrue# (b >## d) then b else d
{-# INLINE mergeRange #-}

{- |
Find the AABB for each object in a world.

Build a vector of these AABBs, each identified by its key in the world.

Objects are ordered using the world's traversal order
-}
toAabbs :: (PrimMonad m) => World (PrimState m) label -> m (V.Vector (Int, Aabb))
toAabbs world@World {..} = V.unsafeFreeze =<< E.mapM f _wEmpties
  where
    f i = do
      shape <- readShape world i
      return (i, toAabb shape)
{-# INLINE toAabbs #-}

{- |
Given a world:

  *Find the AABB for each object.
  *Extract a tag from each object.
  *Build a vector of these tagged AABBs, each identified by its key in the world.

Objects are ordered using the world's traversal order
-}
toTaggedAabbs :: (V.Unbox tag, PrimMonad m)
  => (Int -> m tag)
  -> World (PrimState m) label
  -> m (V.Vector (Int, Aabb, tag))
toTaggedAabbs toTag world@World {..} = V.unsafeFreeze =<< E.mapM f _wEmpties
  where
    f i = do
      tag <- toTag i
      shape <- readShape world i
      return (i, toAabb shape, tag)
{-# INLINE toTaggedAabbs #-}

{- |
Called \"unordered\" because (x, y) is equivalent to (y, x)

Given an 'Int' n, find all choices of two different 'Int's [0, n - 1]

These pairs (x, y) are in decreasing order, where x is the most significant value and y is the least significant value.
-}
unorderedPairs :: Int -> [(Int, Int)]
unorderedPairs n
  | n < 2 = []
  | otherwise = f (n - 1) (n - 2)
  where f 1 0 = [(1, 0)]
        f x 0 = (x, 0) : f (x - 1) (x - 2)
        f x y = (x, y) : f x (y - 1)
        {-# INLINE f #-}
{-# INLINE unorderedPairs #-}

-- | Find pairs of objects with overlapping AABBs.
-- Note: Pairs of static objects are excluded.
-- These pairs are in descending order according to 'unorderedPairs', where \"ascending\" is the world's traversal order.
culledKeys :: (PrimMonad m) => World (PrimState m) label -> m (Descending (Int, Int))
culledKeys world = do
  taggedAabbs <- toTaggedAabbs isStatic world
  let ijs = unorderedPairs $ V.length taggedAabbs
      -- NOTE: don't aabbCheck static objects, otherwise the sim explodes
      f (i, j) =
        if not (isStaticA && isStaticB) && aabbCheck a b
          then Just (i', j')
          else Nothing
        where
          (i', a, isStaticA) = taggedAabbs V.! i
          (j', b, isStaticB) = taggedAabbs V.! j
  return $ Descending . catMaybes $ fmap f ijs
  where
    isStatic i = (C.isStatic . C._physObjInvMass) <$> readPhysObj world i
{-# INLINE culledKeys #-}
