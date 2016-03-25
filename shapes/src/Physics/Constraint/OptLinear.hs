{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Physics.Constraint.OptLinear where

import GHC.Prim
import GHC.Types (Double(D#))

import Control.Lens

import Shapes.Linear.Template (makeVectorType, defineJoinSplit)
import Shapes.Linear.MatrixTemplate
import Shapes.Linear.ValueInfos (doubleInfo)

import Utils.Utils

$(makeVectorType doubleInfo 2)
$(makeVectorType doubleInfo 3)
$(makeVectorType doubleInfo 6)
$(makeMatrixType doubleInfo (2, 2))
$(makeMatrixType doubleInfo (6, 6))
$(defineMatrixMul doubleInfo (2, 2, 2))
$(defineJoinSplit doubleInfo (3, 3))

newtype Diag6 = Diag6 V6 deriving Show

newtype P2 = P2 V2 deriving Show

append2 :: V2 -> Double -> V3
(V2 a b) `append2` (D# c) = V3 a b c

split3 :: V3 -> (V2, Double)
split3 (V3 a b c) = (V2 a b, D# c)

smulV2 :: Double -> V2 -> V2
smulV2 (D# s) = liftV2 (*## s)

sdivV2 :: Double -> V2 -> V2
sdivV2 (D# s) = liftV2 (/## s)

smulV6 :: Double -> V6 -> V6
smulV6 (D# s) = liftV6 (*## s)

smulV6' :: V6 -> Double -> V6
smulV6' = flip smulV6

smulM2x2 :: Double -> M2x2 -> M2x2
smulM2x2 (D# s) = liftM2x2 (*## s)

smulM2x2' :: M2x2 -> Double -> M2x2
smulM2x2' = flip smulM2x2

plusV2 :: V2 -> V2 -> V2
plusV2 = lift2V2 (+##)

plusV6 :: V6 -> V6 -> V6
plusV6 = lift2V6 (+##)

minusV2 :: V2 -> V2 -> V2
minusV2 = lift2V2 (-##)

vmulDiag6 :: V6 -> Diag6 -> V6
vmulDiag6 v (Diag6 m) = lift2V6 (*##) v m

vmulDiag6' :: Diag6 -> V6 -> V6
vmulDiag6' (Diag6 m) v = lift2V6 (*##) v m

afdot :: P2 -> V2 -> Double
afdot (P2 v0) v1 = D# (v0 `dotV2` v1)

afdot' :: V2 -> P2 -> Double
afdot' = flip afdot

clockwiseV2 :: V2 -> V2
clockwiseV2 (V2 x y) = V2 y (negateDouble# x)

normalizeV2 :: V2 -> V2
normalizeV2 (V2 x y) = V2 (x /## n) (y /## n)
  where n = sqrtDouble# ((x *## x) +## (y *## y))

diffP2 :: P2 -> P2 -> V2
diffP2 (P2 v0) (P2 v1) = v0 `minusV2` v1

invM2x2 :: M2x2 -> M2x2
invM2x2 (M2x2 a b c d) =
  D# invDet `smulM2x2` M2x2 d (negateDouble# b) (negateDouble# c) a
  where det = (a *## d) -## (b *## c)
        invDet = 1.0## /## det

negateV2 :: V2 -> V2
negateV2 = liftV2 negateDouble#

data Line2 = Line2 { linePoint :: !P2
                   , lineNormal :: !V2 }

toLine2 :: P2 -> P2 -> Line2
toLine2 a b = Line2 { linePoint = a
                    , lineNormal = clockwiseV2 (b `diffP2` a) }

perpLine2 :: P2 -> P2 -> Line2
perpLine2 a b = Line2 { linePoint = a
                      , lineNormal = b `diffP2` a }

-- solving some `mx = b` up in here
intersect2 :: Line2 -> Line2 -> P2
intersect2 (Line2 p n@(V2 n0 n1)) (Line2 p' n'@(V2 n2 n3)) =
  P2 (invM2x2 m `mul2x2c` b)
  where b = V2 b0 b1
        !(D# b0) = p `afdot` n
        !(D# b1) = p' `afdot` n'
        m = M2x2 n0 n1 n2 n3

data ClipResult a = ClipLeft !a | ClipRight !a | ClipBoth !a | ClipNone

-- replace clipped points with the intersection point
-- if both points were clipped (entire segment behind the bound) return just the intersection
applyClip :: ClipResult a -> SP a a -> Either a (SP a a)
applyClip res (SP a b) = case res of
  ClipLeft c -> Right (SP c b)
  ClipRight c -> Right (SP a c)
  ClipBoth c -> Left c
  ClipNone -> Right (SP a b)

-- if the entire segment was behind the bound, return Nothing
applyClip' :: ClipResult a -> SP a a -> Maybe (SP a a)
applyClip' (ClipBoth _) _ = Nothing
applyClip' res seg = either (const Nothing) Just (applyClip res seg)

-- remove clipped points
applyClip'' :: ClipResult a -> SP s s -> Maybe (Either s (SP s s))
applyClip'' res (SP a b) = case res of
  ClipLeft _ -> Just $ Left b
  ClipRight _ -> Just $ Left a
  ClipBoth _ -> Nothing
  ClipNone -> Just $ Right (SP a b)

lApplyClip :: ASetter' s a -> ClipResult a -> (SP s s) -> Either s (SP s s)
lApplyClip l res (SP a b) = case res of
  ClipLeft c -> Right (SP (set l c a) b)
  ClipRight c -> Right (SP a (set l c b))
  ClipBoth c -> Left (set l c a) -- use the 'first' vertex by default
  ClipNone -> Right (SP a b)

-- if the entire segment was behind the bound, return Nothing
lApplyClip' :: ASetter' s a -> ClipResult a -> (SP s s) -> Maybe (SP s s)
lApplyClip' _ (ClipBoth _) _ = Nothing
lApplyClip' l res seg = either (const Nothing) Just (lApplyClip l res seg)

clipSegment :: Line2 -> SP Line2 (SP P2 P2) -> ClipResult P2
clipSegment boundary (SP incident (SP a b))
  | a' < c' = if b' < c' then ClipBoth c
              else ClipLeft c
  | b' < c' = ClipRight c
  | otherwise = ClipNone
  where c = intersect2 boundary incident
        n = lineNormal boundary
        a' = a `afdot` n
        b' = b `afdot` n
        c' = c `afdot` n
