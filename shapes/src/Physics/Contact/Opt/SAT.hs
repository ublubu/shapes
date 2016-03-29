{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

-- Separating Axis Test
module Physics.Contact.Opt.SAT where

import GHC.Types (Double(D#))

import Control.Lens ((^.), view, makeLenses, _1, makePrisms)
import Data.Either.Combinators
import Data.Function (on)
import Physics.Contact.Opt.ConvexHull
import Physics.Linear.Opt
import Utils.Utils

data Overlap = Overlap { _overlapEdge :: !Neighborhood
                       , _overlapDepth :: !Double
                       , _overlapPenetrator :: !Neighborhood
                       } deriving Show
makeLenses ''Overlap

data SATResult = Separated Neighborhood | MinOverlap Overlap
                 deriving Show
makePrisms ''SATResult

type ContactPoints = Either Neighborhood (SP Neighborhood Neighborhood)

data Contact =
  Contact { _contactEdge :: !Neighborhood
          , _contactPenetrator :: !ContactPoints
          , _contactPenetratingEdge :: !(SP Neighborhood Neighborhood)
          } deriving Show
makeLenses ''Contact

satToEither :: SATResult -> Either Neighborhood Overlap
satToEither (Separated x) = Left x
satToEither (MinOverlap x) = Right x

-- assumes pairs are (min, max)
overlapTest :: (Ord a) => SP a a -> SP a a -> Bool
overlapTest (SP a b) (SP c d) = not (c > b || d < a)

-- intervals are of distance along edge normal of shape X
overlapAmount :: (Ord a, Num a) => SP a a -> SP a a -> Maybe a
overlapAmount x@(SP _ edge) y@(SP penetrator _) = toMaybe (overlapTest x y) (edge - penetrator)

overlapNormal :: Overlap -> V2
overlapNormal = _neighborhoodUnitNormal . _overlapEdge


overlap :: ConvexHull -> Neighborhood -> ConvexHull -> Maybe Overlap
overlap sEdge edge sPen =
  fmap (\oval' -> Overlap edge oval' penetrator ) oval
  where dir = _neighborhoodUnitNormal edge
        extentS = extentAlongSelf sEdge (edge ^. neighborhoodIndex, dir)
        extentP = extentAlong sPen dir
        penetrator = extentP ^. extentMin
        oval = overlapAmount (extentS ^. extentProjection) (extentP ^. extentProjection)

minOverlap :: ConvexHull
           -> [Neighborhood]
           -> ConvexHull
           -> SATResult
minOverlap sEdge edges sPen =
  foldl1 f os -- lazy fold for early exit?
  where os = fmap (\edge -> maybe (Separated edge) MinOverlap $ overlap sEdge edge sPen) edges
        f :: SATResult -> SATResult -> SATResult
        f sep@(Separated _) _ = sep
        f _ sep@(Separated _) = sep
        f mino@(MinOverlap mino') o@(MinOverlap o') =
          if _overlapDepth o' < _overlapDepth mino' then o else mino

minOverlap' :: ConvexHull -> ConvexHull -> SATResult
minOverlap' a = minOverlap a (neighborhoods a)

penetratingEdge :: Overlap -> SP Neighborhood Neighborhood
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

penetratedEdge :: Overlap -> SP Neighborhood Neighborhood
penetratedEdge (Overlap edgeStart _ _) = SP edgeStart (_neighborhoodNext edgeStart)

contactPoints' :: ContactPoints -> Either P2 (SP P2 P2)
contactPoints' = mapBoth f g
  where f = _neighborhoodCenter
        g = spMap f

flattenContactPoints :: ContactPoints -> [Neighborhood]
flattenContactPoints (Left p) = [p]
flattenContactPoints (Right (SP p1 p2)) = [p1, p2]

clipEdge :: SP Neighborhood Neighborhood -> V2 -> SP Neighborhood Neighborhood -> Maybe ContactPoints
clipEdge (SP aa bb) n inc_ = do
  inc' <- lApplyClip' l (clipSegment aBound (SP cd' inc)) inc_
  inc'' <- lApplyClip' l (clipSegment bBound (SP cd' (f inc'))) inc'
  applyClip'' (clipSegment abBound (SP cd' (f inc''))) inc''
  where aBound = perpLine2 a b
        bBound = perpLine2 b a
        abBound = Line2 a (negateV2 n)
        cd' = toLine2 c d
        inc@(SP c d) = f inc_
        (SP a b) = f (SP aa bb)
        f = spMap (view neighborhoodCenter)
        l = neighborhoodCenter

convertContactResult :: Flipping (Either Neighborhood (Maybe Contact))
                     -> Maybe (Flipping (Either Neighborhood Contact))
convertContactResult = flipInjectF . fmap liftRightMaybe

-- 'Flipping' indicates the direction of the collision. 'Same' means 'a' is penetrated by 'b'.
-- TODO: return all useful values (e.g. overlap depth)
contactDebug :: ConvexHull
             -> ConvexHull
             -> (Maybe (Flipping (Either Neighborhood Contact)), SATResult, SATResult)
contactDebug a b = (convertContactResult $ fmap (mapRight contact_) ovl, ovlab, ovlba)
  where ovlab = minOverlap' a b
        ovlba = minOverlap' b a
        ovlab' = satToEither ovlab
        ovlba' = satToEither ovlba
        ovl :: Flipping (Either Neighborhood Overlap)
        ovl = eitherBranchBoth ((<) `on` _overlapDepth) ovlab' ovlba'

contact :: ConvexHull
        -> ConvexHull
        -> Maybe (Flipping (Either Neighborhood Contact))
contact a b = contactDebug a b ^. _1

contact_ :: Overlap -> Maybe Contact
contact_ ovl@Overlap{..} = fmap f (clipEdge edge n pen)
  where edge = penetratedEdge ovl
        pen = penetratingEdge ovl
        n = overlapNormal ovl
        f c = Contact _overlapEdge c pen
