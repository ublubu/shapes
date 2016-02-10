{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- Separating Axis Test
module Physics.SAT where

import Control.Lens ((^.), view, makeLenses, _1)
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

data SATResult a = Separated (Neighborhood a) | MinOverlap (Overlap a)
                 deriving (Show, Eq)

type ContactPoints a = Either (Neighborhood a) (Neighborhood a, Neighborhood a)

data Contact a = Contact { _contactEdge :: !(Neighborhood a)
                         , _contactPenetrator :: !(ContactPoints a)
                         , _contactDepth :: !a
                         } deriving (Show, Eq)
makeLenses ''Contact

satToEither :: SATResult a -> Either (Neighborhood a) (Overlap a)
satToEither (Separated x) = Left x
satToEither (MinOverlap x) = Right x

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

minOverlap :: forall a . (Num a, Ord a)
           => ConvexHull a
           -> [Neighborhood a]
           -> ConvexHull a
           -> SATResult a
minOverlap sEdge edges sPen =
  foldl1 f os -- lazy fold for early exit?
  where os = fmap (\edge -> maybe (Separated edge) MinOverlap $ overlap sEdge edge sPen) edges
        f :: SATResult a -> SATResult a -> SATResult a
        f sep@(Separated _) _ = sep
        f _ sep@(Separated _) = sep
        f mino@(MinOverlap mino') o@(MinOverlap o') =
          if _overlapDepth o' < _overlapDepth mino' then o else mino

minOverlap' :: (Num a, Ord a) => ConvexHull a -> ConvexHull a -> SATResult a
minOverlap' a b = minOverlap a (neighborhoods a) b

--contactDepth :: (Floating a) => Neighborhood a -> P2 a -> a
--contactDepth neighborhood p = f v - f p
  --where f = afdot' n
        --n = _neighborhoodUnitNormal neighborhood
        --v = _neighborhoodCenter neighborhood

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

contactPoints' :: ContactPoints a -> Either (P2 a) (P2 a, P2 a)
contactPoints' = mapBoth f g
  where f = _neighborhoodCenter
        g = pairMap f

flattenContactPoints :: ContactPoints a -> [Neighborhood a]
flattenContactPoints (Left p) = [p]
flattenContactPoints (Right (p1, p2)) = [p1, p2]

clipEdge :: (Epsilon a, Floating a, Ord a) => (Neighborhood a, Neighborhood a) -> V2 a -> (Neighborhood a, Neighborhood a) -> Maybe (ContactPoints a)
clipEdge (aa, bb) n inc_ = do
  inc' <- lApplyClip' l (clipSegment aBound (cd', inc)) inc_
  inc'' <- lApplyClip' l (clipSegment bBound (cd', f inc')) inc'
  applyClip'' (clipSegment abBound (cd', f inc'')) inc''
  where aBound = perpLine2 a b
        bBound = perpLine2 b a
        abBound = Line2 a (-n)
        cd' = toLine2 c d
        inc@(c, d) = f inc_
        (a, b) = f (aa, bb)
        f = pairMap (view neighborhoodCenter)
        l = neighborhoodCenter

convertContactResult :: Flipping (Either (Neighborhood a) (Maybe (Contact a)))
                     -> Maybe (Flipping (Either (Neighborhood a) (Contact a)))
convertContactResult = flipInjectF . fmap liftRightMaybe

-- 'Flipping' indicates the direction of the collision. 'Same' means 'a' is penetrated by 'b'.
-- TODO: use a type that documents wtf snd of the result is.
-- TODO: return all useful values (e.g. overlap depth)
contactDebug :: forall a . (Epsilon a, Floating a, Ord a)
        => ConvexHull a
        -> ConvexHull a
        -> (Maybe (Flipping (Either (Neighborhood a) (Contact a))), SATResult a, SATResult a)
contactDebug a b = (convertContactResult $ fmap (mapRight contact_) ovl, ovlab, ovlba)
  where ovlab = minOverlap' a b
        ovlba = minOverlap' b a
        ovlab' = satToEither ovlab
        ovlba' = satToEither ovlba
        ovl :: Flipping (Either (Neighborhood a) (Overlap a))
        ovl = eitherBranchBoth (\oab oba -> _overlapDepth oab < _overlapDepth oba) ovlab' ovlba'

contact :: forall a . (Epsilon a, Floating a, Ord a)
        => ConvexHull a
        -> ConvexHull a
        -> Maybe (Flipping (Either (Neighborhood a) (Contact a)))
contact a b = contactDebug a b ^. _1

contact_ :: (Epsilon a, Floating a, Ord a) => Overlap a -> Maybe (Contact a)
contact_ ovl@Overlap{..} = fmap f (clipEdge edge n pen)
  where edge = penetratedEdge ovl
        pen = penetratingEdge ovl
        n = overlapNormal ovl
        f c = Contact _overlapEdge c _overlapDepth
