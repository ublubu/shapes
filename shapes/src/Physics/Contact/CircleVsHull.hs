{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Physics.Contact.CircleVsHull where

import           GHC.Types                  (Double (..))

import           Data.Either
import           Physics.Contact.Circle
import           Physics.Contact.ConvexHull
import           Physics.Contact.GJK
import           Physics.Contact.Types
import           Physics.Linear
import           Utils.Utils

-- | There's only one contact point between a circle and a convex hull.
generateContacts :: Circle -> ConvexHull -> Maybe (Int, Contact')
  -- ^ (hull feature index, contact manifold) -- circle is always the penetrator
generateContacts circle@Circle {..} hull = convertSimplex circle simplex
  where
    simplex = closestSimplex hull _circleCenter

-- TODO: handle the "deep overlap" case (3-point simplex)
convertSimplex :: Circle -> Simplex -> Maybe (Int, Contact')
convertSimplex circle (Simplex' simplex) = convertSimplex12 circle simplex
convertSimplex _ (Simplex3' _)           = Nothing

convertSimplex12 :: Circle -> Simplex12 -> Maybe (Int, Contact')
convertSimplex12 circle = either (processSimplex1 circle) (processSimplex2 circle)

processSimplex1 :: Circle -> Simplex1 -> Maybe (Int, Contact')
processSimplex1 circle (Simplex1 aa) =
  (_neighborhoodIndex aa, ) <$> processSimplex_ circle (_neighborhoodCenter aa)

processSimplex2 :: Circle -> Simplex2 -> Maybe (Int, Contact')
processSimplex2 circle@Circle {..} simplex@(Simplex2 feature _) =
  (_neighborhoodIndex feature, ) <$> processSimplex_ circle a
  where
    a = _circleCenter `closestAlong` simplex

processSimplex_ :: Circle -> P2 -> Maybe Contact'
processSimplex_ Circle {..} a
  | sqRadius < abSq = Nothing -- distance greater than circle radius
  | otherwise =
    Just $
    Contact'
    { _contactEdgeNormal' = negateV2 normal
    , _contactPenetrator' = a
    , _contactDepth' = _circleRadius - abLength
    }
  where
    b = _circleCenter
    sqRadius = _circleRadius * _circleRadius
    ab = diffP2 b a
    abSq = sqLengthV2 ab
    abLength = sqrt abSq
    normal = sdivV2 abLength ab

closestAlong :: P2 -- ^ target point
  -> Simplex2 -- ^ line segment
  -> P2
o `closestAlong` (Simplex2 aa bb) = aoAlong `vplusP2` a
  where a = _neighborhoodCenter aa
        b = _neighborhoodCenter bb
        ao = diffP2 o a
        ab = diffP2 b a
        abNorm = normalizeV2 ab
        aoAlong = D# (ao `dotV2` abNorm) `smulV2` abNorm
