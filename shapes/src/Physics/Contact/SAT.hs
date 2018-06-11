{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Separating Axis Test (SAT).
A separating axis is a direction along which the projections of two shapes do not overlap.
Alternately, a separating axis is a line between two shapes that do not intersect.

If no separating axis is found, use the axis of smallest overlap to determine
which features of the objects are involved in the collision (e.g. calculate contact points and normals).
-}
module Physics.Contact.SAT where

import           GHC.Types                  (Double (D#))

import           Control.Lens               (makeLenses, makePrisms, view, (^.),
                                             _1)
import           Data.Either.Combinators
import           Data.Function              (on)
import           Physics.Contact.ConvexHull
import           Physics.Linear
import           Utils.Descending
import           Utils.Utils

-- | An overlap between two shapes.
data Overlap = Overlap { _overlapEdge       :: !Neighborhood
                       -- ^ the first vertex of the penetrated edge
                       , _overlapDepth      :: !Double
                       , _overlapPenetrator :: !Neighborhood
                       -- ^ the vertex that penetrates the edge
                       } deriving Show
makeLenses ''Overlap

-- | Either the separating axis or the smallest overlap between two shapes.
data SATResult
  = Separated Neighborhood
  -- ^ the edge that forms a separating axis between the two shapes.
  | MinOverlap Overlap
  -- ^ the smallest overlap
  deriving (Show)
makePrisms ''SATResult

{- |
A contact manifold can contain either a single point or a pair of points.
For example, a pair of touching edges can be described by a pair of points.
A vertex touching an edge can be described by a single point.
-}
type ContactPoints = Either Neighborhood (SP Neighborhood Neighborhood)

-- | A contact manifold
data Contact =
  Contact { _contactEdge            :: !Neighborhood
          -- ^ the first vertex of the edge being penetrated
          , _contactPenetrator      :: !ContactPoints
          -- ^ the points of the contact manifold (after clipping the penetrating edge to the penetrated edge)
          , _contactPenetratingEdge :: !(SP Neighborhood Neighborhood)
          -- ^ the edge that penetrates '_contactEdge'
          } deriving Show
makeLenses ''Contact

-- | One side of an isomorphism.
satToEither :: SATResult -> Either Neighborhood Overlap
satToEither (Separated x)  = Left x
satToEither (MinOverlap x) = Right x

-- | assumes pairs are (min, max)
overlapTest ::
     (Ord a)
  => SP a a
  -- ^ an interval
  -> SP a a
  -- ^ another interval
  -> Bool
  -- ^ Do the intervals overlap?
overlapTest (SP a b) (SP c d) = not (c > b || d < a)

-- | assumes pairs are (min, max)
overlapAmount ::
     (Ord a, Num a)
  => SP a a
  -- ^ an interval (e.g. the projection of a shape onto an axis)
  -> SP a a
  -- ^ another interval
  -> Maybe a
  -- ^ If the intervals overlap, by how much?
overlapAmount x@(SP _ edge) y@(SP penetrator _) = toMaybe (overlapTest x y) (edge - penetrator)

-- | get the normal from the overlap
overlapNormal :: Overlap -> V2
overlapNormal = _neighborhoodUnitNormal . _overlapEdge

-- | Check for overlap along a single axis (edge normal).
overlap :: ConvexHull
  -- ^ The receiving shape "sEdge".
  -> Neighborhood
  -- ^ An edge normal from the receiving shape.
  -> ConvexHull
  -- ^ The penetrating shape "sPen".
  -> Maybe Overlap
  -- ^ Any overlap from "sPen" into "sEdge".
overlap sEdge edge sPen =
  fmap (\oval' -> Overlap edge oval' penetrator ) oval
  where dir = _neighborhoodUnitNormal edge
        extentS = extentAlongSelf sEdge (edge ^. neighborhoodIndex, dir)
        extentP = extentAlong sPen dir
        penetrator = extentP ^. extentMin
        oval = overlapAmount (extentS ^. extentProjection) (extentP ^. extentProjection)

-- | Find the axis (edge normal) with the smallest overlap between the two shapes.
minOverlap :: ConvexHull
           -- ^ The receiving shape "sEdge".
           -> [Neighborhood]
           -- ^ Edge normals from the receiving shape.
           -> ConvexHull
           -- ^ The penetrating shape "sPen".
           -> SATResult
           -- ^ Axis of smallest overlap or separating axis.
minOverlap sEdge edges sPen =
  foldl1 f os -- lazy fold for early exit?
  where os = fmap (\edge -> maybe (Separated edge) MinOverlap $ overlap sEdge edge sPen) edges
        f :: SATResult -> SATResult -> SATResult
        f sep@(Separated _) _ = sep
        f _ sep@(Separated _) = sep
        f mino@(MinOverlap mino') o@(MinOverlap o') =
          if _overlapDepth o' < _overlapDepth mino' then o else mino

-- | Wrapper for 'minOverlap'.
minOverlap' :: ConvexHull -> ConvexHull -> SATResult
minOverlap' a = minOverlap a (neighborhoods a)

{- |
Choose the best edge to act as a penetrator.
The overlap test yields a penetrating vertex, but this vertex belongs to two edges.

Choose the edge that is closest to perpendicular to the overlap normal vector.
i.e. the edge that is closest to parallel with the penetrated edge
-}
penetratingEdge :: Overlap
  -> SP Neighborhood Neighborhood
  -- ^ the two vertices that define the edge (in order)
penetratingEdge (Overlap edge _ b) =
  if bcn < abn then SP b c
  else SP a b
  where c = _neighborhoodNext b
        a = _neighborhoodPrev b
        cc = _neighborhoodCenter c
        bb = _neighborhoodCenter b
        aa = _neighborhoodCenter a
        abn = abs (D# ((bb `diffP2` aa) `dotV2` n))
        bcn = abs (D# ((cc `diffP2` bb) `dotV2` n))
        n = _neighborhoodUnitNormal edge

-- | Extract the endpoints of the penetrated edge.
penetratedEdge :: Overlap -> SP Neighborhood Neighborhood
penetratedEdge (Overlap edgeStart _ _) = SP edgeStart (_neighborhoodNext edgeStart)

-- | Extract just the point data from 'ContactPoints'.
contactPoints' :: ContactPoints -> Either P2 (SP P2 P2)
contactPoints' = mapBoth f g
  where f = _neighborhoodCenter
        g = spMap f

-- | Sort 'ContactPoints' by decreasing feature index.
flattenContactPoints :: ContactPoints -> Descending Neighborhood
flattenContactPoints (Left p) = Descending [p]
flattenContactPoints (Right (SP p1 p2)) =
  if _neighborhoodIndex p1 > _neighborhoodIndex p2
  then Descending [p1, p2]
  else Descending [p2, p1]

-- | Clip a pair of edges into a contact manifold.
clipEdge ::
     SP Neighborhood Neighborhood
  -- ^ the penetrated edge
  -> V2
  -- ^ the normal vector for the overlap
  -> SP Neighborhood Neighborhood
  -- ^ the penetrating edge ("incident" edge)
  -> Maybe ContactPoints
clipEdge (SP aa bb) n inc_ = do
  -- "a" and "b" are the vertices of the penetrated edge.
  -- We're clipping the incident edge to the bounds of the penetrated edge.
  -- clip the incident edge using the bounding plane at point "a"
  inc' <- lApplyClip' l (clipSegment aBound (SP cd' inc)) inc_
  -- clip the incident edge using the bounding plane at point "b"
  inc'' <- lApplyClip' l (clipSegment bBound (SP cd' (f inc'))) inc'
  applyClip'' (clipSegment abBound (SP cd' (f inc''))) inc''
  where aBound = perpLine2 a b
        -- ^ bounding plane against going past point "a" along the edge
        bBound = perpLine2 b a
        -- ^ bounding plane against going past point "b" along the edge
        abBound = Line2 a (negateV2 n)
        -- ^ bounding plane facing into the penetrated object (against going outside the object)
        cd' = toLine2 c d
        inc@(SP c d) = f inc_
        -- ^ the incident edge
        (SP a b) = f (SP aa bb)
        f = spMap (view neighborhoodCenter)
        l = neighborhoodCenter

-- | Pull out  the inner 'Maybe'.
convertContactResult :: Flipping (Either Neighborhood (Maybe Contact))
                     -> Maybe (Flipping (Either Neighborhood Contact))
convertContactResult = flipInjectF . fmap liftRightMaybe

{- |
'Flipping' indicates the direction of the collision.
'Same' means `a` is penetrated by `b`.
'Flipped' means `b` is penetrated by `a`.

How it works:

1. Find the smallest overlap along the axes of each shape's edges.
2. Clip this overlap to a contact manifold.

The result should probably never be 'Nothing', but I don't know if that's guaranteed.
-}
contactDebug :: ConvexHull
             -> ConvexHull
             -> (Maybe (Flipping (Either Neighborhood Contact)), SATResult, SATResult)
             -- ^ 'Either' of separating axis (the normal at the 'Neighborhood') or a contact manifold
contactDebug a b = (convertContactResult $ fmap (mapRight contact_) ovl, ovlab, ovlba)
  where ovlab = minOverlap' a b
        ovlba = minOverlap' b a
        ovlab' = satToEither ovlab
        ovlba' = satToEither ovlba
        ovl :: Flipping (Either Neighborhood Overlap)
        ovl = eitherBranchBoth ((<) `on` _overlapDepth) ovlab' ovlba'

contact :: ConvexHull
        -- ^ shape "a"
        -> ConvexHull
        -- ^ shape "b"
        -> Maybe (Flipping (Either Neighborhood Contact))
        -- ^ 'Either' of separating axis (the normal at the 'Neighborhood') or a contact manifold
contact a b = contactDebug a b ^. _1

-- | Use clipping to calculate the contact manifold for a given overlap.
contact_ :: Overlap -> Maybe Contact
contact_ ovl@Overlap{..} = fmap f (clipEdge edge n pen)
  where edge = penetratedEdge ovl
        pen = penetratingEdge ovl
        n = overlapNormal ovl
        f c = Contact _overlapEdge c pen
