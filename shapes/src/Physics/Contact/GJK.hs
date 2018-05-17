{-# LANGUAGE MagicHash #-}

{- |
Gilbert-Johnson-Keerthi (GJK) for finding the closest part of a Minkowski space
to the origin.

Choose a simplex using opposite extents along some axis.
Extend the simplex in the direction of the origin.
If the simplex encloses the origin, stop.

based on slides/video by Casey Muratori:
* https://www.youtube.com/watch?v=Qupqu1xe7Io

The loop of GJK is:

- extend the simplex along the search direction
- shift to the closest component of the simplex
- do this until we can't extend the simplex any more
  (search stopped short of the origin)

-}

module Physics.Contact.GJK where

import           GHC.Prim
import           GHC.Types                  (Double (D#), isTrue#)

import           Physics.Contact.ConvexHull
import           Physics.Linear
import           Utils.Utils

-- | 2-simplex. The first element is the most recently added. (like the head of a list)
data Simplex3 =
  Simplex3 !Neighborhood
           !Neighborhood
           !Neighborhood
  deriving (Show)
data Simplex2 =
  Simplex2 !Neighborhood
           !Neighborhood
  deriving (Show)
data Simplex1 =
  Simplex1 !Neighborhood
  deriving (Show)
type Simplex12 = Either Simplex1 Simplex2
type Simplex23 = Either Simplex2 Simplex3
data Simplex
  = Simplex' Simplex12
  | Simplex3' Simplex3
  deriving (Show)

closestSimplex :: ConvexHull -> P2 -> Simplex
closestSimplex hull origin = loop (Left $ Simplex1 a) d
  where
    a = _hullNeighborhood 0 hull
    d = diffP2 origin $ _neighborhoodCenter a
    loop :: Simplex12 -> V2 -> Simplex
    loop simplex d =
      case extendSimplex simplex aa of
        Nothing -> Simplex' simplex -- search failed
        Just simplex ->
          case shiftSimplex simplex origin of
            Right simplex     -> Simplex3' simplex -- enclosed the origin
            Left (simplex, d) -> loop simplex d
      where
        aa = support hull d
        a = _neighborhoodCenter aa
        ao = diffP2 a origin

extendSimplex :: Simplex12 -> Neighborhood -> Maybe Simplex23
extendSimplex (Left simplex) aa =
  Left <$> extendSimplex1 simplex aa
extendSimplex (Right simplex) aa = Right <$> extendSimplex2 simplex aa

extendSimplex1 :: Simplex1 -> Neighborhood -> Maybe Simplex2
extendSimplex1 simplex@(Simplex1 bb) aa
  | _neighborhoodIndex bb == _neighborhoodIndex aa = Nothing -- it's a repeat
  | otherwise = Just $ mkSimplex2 aa simplex

extendSimplex2 :: Simplex2 -> Neighborhood -> Maybe Simplex3
extendSimplex2 simplex@(Simplex2 bb cc) aa
  | bi == ai || ci == ai = Nothing -- it's a repeat
  | otherwise = Just $ mkSimplex3 aa simplex
  where ai = _neighborhoodIndex aa
        bi = _neighborhoodIndex bb
        ci = _neighborhoodIndex cc

shiftSimplex :: Simplex23 -> P2 -> Either (Simplex12, V2) Simplex3
shiftSimplex (Left simplex) origin = Left $ shiftSimplex2 simplex origin
shiftSimplex (Right simplex) origin =
  case shiftSimplex3 simplex origin of
    Nothing     -> Right simplex
    Just result -> Left result

shiftSimplex2 :: Simplex2
  -- ^ 1D simplex of 2 points
  -> P2
  -- ^ origin (the target point)
  -> (Simplex12, V2)
  -- ^ new simplex, new search direction
shiftSimplex2 aabb@(Simplex2 aa bb) origin
  | sameDirection ab ao = (Right aabb, crossV2V2 ab ao ab)
  -- ^ search perpendicular to AB toward the origin.
  | otherwise = (Left $ Simplex1 aa, ao) -- throw out B, search from A toward the origin.
  where
    a = _neighborhoodCenter aa
    b = _neighborhoodCenter bb
    ab = diffP2 b a
    ao = diffP2 origin a

shiftSimplex3 :: Simplex3
  -- ^ 2D simplex of 3 points
  -> P2
  -- ^ origin (the target point)
  -> Maybe (Simplex12, V2)
  -- ^ (new simplex, new search direction) OR simplex encloses the origin!
shiftSimplex3 (Simplex3 aa bb cc) origin
  | sameDirection abcac ao =
    if sameDirection ac ao
      then Just (Right $ Simplex2 aa cc, crossV2V2 ac ao ac)
      else Just star
  | sameDirection ababc ao = Just star
  | otherwise = Nothing -- simplex encloses the origin
  where
    a = _neighborhoodCenter aa
    b = _neighborhoodCenter bb
    c = _neighborhoodCenter cc
    ab = diffP2 b a
    ac = diffP2 c a
    ao = diffP2 origin a
    abc = ab `crossV2` ac
    abcac = abc `zcrossV2` ac
    ababc = ab `crosszV2` abc
    star =
      if sameDirection ab ao
        then (Right $ Simplex2 aa bb, crossV2V2 ab ao ab)
        else (Left $ Simplex1 aa, ao)

sameDirection :: V2 -> V2 -> Bool
sameDirection a b = isTrue# (a `dotV2` b >## 0.0##)

mkSimplex3 :: Neighborhood -> Simplex2 -> Simplex3
mkSimplex3 aa (Simplex2 bb cc) = Simplex3 aa bb cc

mkSimplex2 :: Neighborhood -> Simplex1 -> Simplex2
mkSimplex2 aa (Simplex1 bb) = Simplex2 aa bb
