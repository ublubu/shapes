{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.SAT where

import Control.Lens ((^.), view, makeLenses)
import Data.Either.Combinators
import Data.Maybe
import Linear.Affine
import Linear.Epsilon
import Linear.Metric
import Linear.V2
import Utils.Utils
import Physics.Linear
import Physics.Transform
import Physics.ConvexHull

data Overlap a = Overlap { _overlapEdge :: !(Neighborhood a)
                         , _overlapDepth :: !a
                         , _overlapPenetrator :: !(Neighborhood a)
                         } deriving (Show, Eq)
makeLenses ''Overlap

-- assumes pairs are (min, max)
overlapTest :: (Ord a) => (a, a) -> (a, a) -> Bool
overlapTest (a, b) (c, d) = not (c > b || d < a)

-- intervals are of distance along edge normal of shape X
overlapAmount :: (Ord a, Num a) => (a, a) -> (a, a) -> Maybe a
overlapAmount x@(_, edge) y@(penetrator, _) = toMaybe (overlapTest x y) (edge - penetrator)

overlapNormal :: Overlap a -> V2 a
overlapNormal = _neighborhoodUnitNormal . _overlapEdge


overlap :: forall a . (Num a, Ord a) => ConvexHull a -> Neighborhood a -> ConvexHull a -> Maybe (Overlap a)
overlap sEdge edge sPen =
  fmap (\oval' -> Overlap edge oval' penetrator ) oval
  where dir = _neighborhoodUnitNormal edge
        extentS = extentAlongSelf sEdge (edge ^. neighborhoodIndex, dir)
        extentP = extentAlong sPen (dir)
        penetrator = extentP ^. extentMin
        oval = overlapAmount (extentS ^. extentProjection) (extentP ^. extentProjection)

minOverlap :: (Num a, Ord a) => ConvexHull a -> [Neighborhood a] -> ConvexHull a -> Maybe (Overlap a)
minOverlap sEdge edges sPen = foldl1 f os
  where os = fmap (\edge -> overlap sEdge edge sPen) edges
        f !mino !o = do
          mino' <- mino
          o' <- o
          return (if _overlapDepth o' < _overlapDepth mino' then o' else mino')

minOverlap' :: (Num a, Ord a) => ConvexHull a -> ConvexHull a -> Maybe (Overlap a)
minOverlap' a b = minOverlap a (neighborhoods a) b

data Contact a = Contact { contactPoints :: Either (Neighborhood a) (Neighborhood a, Neighborhood a)
                           , contactNormal :: V2 a
                           } deriving Show

contactDepth :: (Floating a) => Neighborhood a -> P2 a -> a
contactDepth neighborhood p = f v - f p
  where f = afdot' n
        n = _neighborhoodUnitNormal neighborhood
        v = _neighborhoodCenter neighborhood

penetratingEdge :: (Floating a, Ord a) => Overlap a -> (Neighborhood a, Neighborhood a)
penetratingEdge (Overlap edge depth b) =
  if bcn < abn then (b, c)
  else (a, b)
  where c = _neighborhoodNext b
        a = _neighborhoodPrev b
        cc = _neighborhoodCenter c
        bb = _neighborhoodCenter b
        aa = _neighborhoodCenter a
        abn = abs $ (bb .-. aa) `dot` n
        bcn = abs $ (cc .-. bb) `dot` n
        n = _neighborhoodUnitNormal edge

penetratedEdge :: (Floating a) => Overlap a -> (Neighborhood a, Neighborhood a)
penetratedEdge (Overlap edgeStart _ _) = (edgeStart, _neighborhoodNext edgeStart)

contactPoints' :: Contact a -> Either (P2 a) (P2 a, P2 a)
contactPoints' = mapBoth f g . contactPoints
  where f = _neighborhoodCenter
        g = pairMap f

flattenContactPoints :: Contact a -> [Neighborhood a]
flattenContactPoints (Contact (Left p) _) = [p]
flattenContactPoints (Contact (Right (p1, p2)) _) = [p1, p2]

clipEdge :: (Epsilon a, Floating a, Ord a) => (Neighborhood a, Neighborhood a) -> V2 a -> (Neighborhood a, Neighborhood a) -> Maybe (Contact a)
clipEdge (aa, bb) n inc_ = do
  inc' <- lApplyClip' l (clipSegment aBound (cd', inc)) inc_
  inc'' <- lApplyClip' l (clipSegment bBound (cd', f inc')) inc'
  contacts <- applyClip'' (clipSegment abBound (cd', f inc'')) inc''
  return Contact { contactPoints = contacts
                  , contactNormal = n }
  where aBound = perpLine2 a b
        bBound = perpLine2 b a
        abBound = Line2 a (-n)
        cd' = toLine2 c d
        inc@(c, d) = f inc_
        (a, b) = f (aa, bb)
        f = pairMap (view neighborhoodCenter)
        l = neighborhoodCenter

-- 'Flipping' indicates the direction of the collision. 'Same' means the first object overlaps into the second.
-- TODO: use a type that documents wtf snd of the result is.
contact :: (Epsilon a, Floating a, Ord a) => ConvexHull a -> ConvexHull a -> Maybe (Flipping (Contact a, Neighborhood a))
contact a b = either (fmap Same . contact_) (fmap Flip . contact_) =<< ovl
  where ovlab = minOverlap' a b
        ovlba = minOverlap' b a
        ovl = maybeBranch (\oab oba -> _overlapDepth oab < _overlapDepth oba) ovlab ovlba

contactDebug :: (Epsilon a, Floating a, Ord a) => ConvexHull a -> ConvexHull a -> (Maybe (Flipping (Contact a, Neighborhood a)), Maybe (Overlap a), Maybe (Overlap a))
contactDebug a b = (either (fmap Same . contact_) (fmap Flip . contact_) =<< ovl, ovlab, ovlba)
  where ovlab = minOverlap' a b
        ovlba = minOverlap' b a
        ovl = maybeBranchBoth (\oab oba -> _overlapDepth oab < _overlapDepth oba) ovlab ovlba

contact_ :: (Epsilon a, Floating a, Ord a) => Overlap a -> Maybe (Contact a, Neighborhood a)
contact_ ovl = fmap f (clipEdge edge n pen)
  where edge = penetratedEdge ovl
        pen = penetratingEdge ovl
        n = overlapNormal ovl
        f c = (c, _overlapEdge ovl)
