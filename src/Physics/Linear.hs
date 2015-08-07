{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}

module Physics.Linear where

import GHC.TypeLits (Nat, type(+))
import Control.Lens
import qualified Data.Vector as Vec
import Data.Vector ((!))
import Data.Maybe
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Metric

type V6 a = V 6 a
type M66 a = V6 (V6 a)
type P2 a = Point V2 a

join22 :: V2 a -> V2 a -> V4 a
join22 (V2 ax ay) (V2 bx by) = V4 ax ay bx by

join33 :: V3 a -> V3 a -> V 6 a
join33 (V3 a b c) (V3 d e f) = listToV [a, b, c, d, e, f]

split33 :: V6 a -> (V3 a, V3 a)
split33 v = Vec.splitAt 3 (toVector v) & both %~ (\v' -> V3 (v' ! 0) (v' ! 1) (v' ! 2))

flip33 :: V6 a -> V6 a
flip33 v = listToV [d, e, f, a, b, c]
  where (a, b, c, d, e, f) = extract6 v

extract6 :: V6 a -> (a, a, a, a, a, a)
extract6 v6 = (v ! 0, v ! 1, v ! 2, v ! 3, v ! 4, v ! 5)
  where v = toVector v6

join44 :: V4 a -> V4 a -> V 8 a
join44 (V4 a b c d) (V4 e f g h) = listToV [a, b, c, d, e, f, g, h]

cross22 :: Num a => V2 a -> V2 a -> a
cross22 (V2 ax ay) (V2 bx by) = (ax * by) - (ay * bx)

append2 :: V2 a -> a -> V3 a
append2 (V2 a b) = V3 a b

split3 :: V3 a -> (V2 a, a)
split3 (V3 a b c) = (V2 a b, c)

listToV :: Dim n => [a] -> V (n :: Nat) a
listToV = fromJust . fromVector . Vec.fromList

rotate22_ :: (Num a) => a -> a -> M22 a
rotate22_ cosv sinv = V2 (V2 cosv (-sinv)) (V2 sinv cosv)

rotate22 :: (Floating a) => a -> M22 a
rotate22 ori = rotate22_ c s
  where c = cos ori
        s = sin ori

afmat33 :: (Num a) => M22 a -> M33 a
afmat33 (V2 x y) = (V3 (append2 x 0) (append2 y 0) (V3 0 0 1))

aftranslate33 :: (Num a) => V2 a -> M33 a
aftranslate33 (V2 x y) = V3 (V3 1 0 x) (V3 0 1 y) (V3 0 0 1)

afrotate33 :: (Floating a) => a -> M33 a
afrotate33 = afmat33 . rotate22

afscale33 :: (Num a) => V2 a -> M33 a
afscale33 (V2 x y) = V3 (V3 x 0 0) (V3 0 y 0) (V3 0 0 1)

dfa :: (Num a) => Diff V2 a -> V3 a
dfa (V2 x y) = V3 x y 0

afd :: V3 a -> Diff V2 a
afd = view _xy

pfa :: (Num a) => P2 a -> V3 a
pfa (P (V2 x y)) = V3 x y 1

afp :: V3 a -> P2 a
afp = P . view _xy

afdot :: (Num a) => P2 a -> Diff V2 a -> a
afdot a b = view _Point a `dot` b

afdot' :: (Num a) => Diff V2 a -> P2 a -> a
afdot' = flip afdot


class AffineTrans t a where
  afmul :: M33 a -> t -> t

instance (Num a) => AffineTrans (Point V2 a) a where
  afmul m = afp . (m !*) . pfa

instance (Num a) => AffineTrans (V2 a) a where
  afmul m = afd . (m !*) . dfa

clockwise22 :: Num a => M22 a
clockwise22 = V2 (V2 0 1) (V2 (-1) 0)

clockwise2 :: Num a => V2 a -> V2 a
clockwise2 (V2 x y) = V2 y (-x)

anticlockwise22 :: Num a => M22 a
anticlockwise22 = V2 (V2 0 1) (V2 (-1) 0)

anticlockwise2 :: Num a => V2 a -> V2 a
anticlockwise2 (V2 x y) = V2 (-y) x

perpendicularTowards :: (Num a, Ord a) => V2 a -> V2 a -> V2 a
a `perpendicularTowards` b = rot b
  where rot = if a `cross22` b > 0 then clockwise2
              else anticlockwise2

similarDir :: (Metric t, Num a, Ord a) => t a -> t a -> Bool
similarDir a b = a `dot` b > 0

horizontalizer22 :: (Floating a) => V2 a -> M22 a
horizontalizer22 d@(V2 a o) = rotate22_ cosv (-sinv)
  where sinv = o / h
        cosv = a / h
        h = norm d

data Line2 a = Line2 { linePoint :: P2 a
                     , lineNormal :: V2 a }

toLine2 :: (Num a) => P2 a -> P2 a -> Line2 a
toLine2 a b = Line2 { linePoint = a
                    , lineNormal = clockwise2 (b .-. a) }

perpLine2 :: (Num a) => P2 a -> P2 a -> Line2 a
perpLine2 a b = Line2 { linePoint = a
                      , lineNormal = b .-. a }

-- solving some `mx = b` up in here
intersect2 :: (Floating a, Epsilon a) => Line2 a -> Line2 a -> Maybe (P2 a)
intersect2 (Line2 p n) (Line2 p' n') = do
  m' <- inv22 m
  return $ P (m' !* b)
  where b = V2 (p `afdot` n) (p' `afdot` n')
        m = V2 n n'

center2 :: (Fractional a) => P2 a -> P2 a -> P2 a
center2 a b = fmap (/2) (a + b)

data ClipResult a = ClipLeft a | ClipRight a | ClipBoth a | ClipNone

-- replace clipped points with the intersection point
-- if both points were clipped (entire segment behind the bound) return just the intersection
applyClip :: ClipResult a -> (a, a) -> Either a (a, a)
applyClip res (a, b) = case res of
  ClipLeft c -> Right (c, b)
  ClipRight c -> Right (a, c)
  ClipBoth c -> Left c
  ClipNone -> Right (a, b)

-- if the entire segment was behind the bound, return Nothing
applyClip' :: ClipResult a -> (a, a) -> Maybe (a, a)
applyClip' (ClipBoth _) _ = Nothing
applyClip' res seg = either (const Nothing) Just (applyClip res seg)

-- remove clipped points
applyClip'' :: ClipResult a -> (s, s) -> Maybe (Either s (s, s))
applyClip'' res (a, b) = case res of
  ClipLeft _ -> Just $ Left b
  ClipRight _ -> Just $ Left a
  ClipBoth _ -> Nothing
  ClipNone -> Just $ Right (a, b)

lApplyClip :: ASetter' s a -> ClipResult a -> (s, s) -> Either s (s, s)
lApplyClip l res (a, b) = case res of
  ClipLeft c -> Right (set l c a, b)
  ClipRight c -> Right (a, set l c b)
  ClipBoth c -> Left (set l c a) -- use the 'first' vertex by default
  ClipNone -> Right (a, b)

-- if the entire segment was behind the bound, return Nothing
lApplyClip' :: ASetter' s a -> ClipResult a -> (s, s) -> Maybe (s, s)
lApplyClip' _ (ClipBoth _) _ = Nothing
lApplyClip' l res seg = either (const Nothing) Just (lApplyClip l res seg)

clipSegment :: (Floating a, Epsilon a, Ord a) => Line2 a -> (Line2 a, (P2 a, P2 a)) -> ClipResult (P2 a)
clipSegment boundary (incident, (a, b)) = case intersect2 boundary incident of
  Nothing -> ClipNone
  Just c | a' < c' -> if b' < c' then ClipBoth c
                      else ClipLeft c
         | b' < c' -> ClipRight c
         | otherwise -> ClipNone
    where n = lineNormal boundary
          a' = a `afdot` n
          b' = b `afdot` n
          c' = c `afdot` n
