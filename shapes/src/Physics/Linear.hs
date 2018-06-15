{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Arithmetic utility functions for vectors and matrices.
-}
module Physics.Linear where

import GHC.Generics (Generic)
import GHC.Prim
import GHC.Types (Double(D#))

import Control.DeepSeq
import Control.Lens
import Data.Vector.Unboxed.Deriving

import Shapes.Linear.Template (makeVectorType, defineJoinSplit)
import Shapes.Linear.MatrixTemplate
import Shapes.Linear.ValueInfos (doubleInfo)

import Utils.Utils

$(makeVectorType doubleInfo 2)
$(makeVectorType doubleInfo 3)
$(makeVectorType doubleInfo 6)
$(makeMatrixType doubleInfo (2, 2))
$(makeMatrixType doubleInfo (3, 3))
$(makeMatrixType doubleInfo (6, 6))
$(defineMatrixMul doubleInfo (2, 2, 2))
$(defineMatrixMul doubleInfo (3, 3, 3))
$(defineJoinSplit doubleInfo (3, 3))

newtype Diag6 = Diag6 V6 deriving Show

instance NFData V2 where
  rnf (V2 _ _) = ()
  {-# INLINE rnf #-}

newtype P2 = P2 V2 deriving (Generic, Show, NFData)

makeLenses ''P2

derivingUnbox "V2"
  [t| V2 -> (Double, Double) |]
  [| \(V2 a b) -> (D# a, D# b) |]
  [| \(D# a, D# b) -> V2 a b |]

derivingUnbox "P2"
  [t| P2 -> V2 |]
  [| \(P2 v) -> v |]
  [| P2 |]

derivingUnbox "V6"
  [t| V6 -> (Double, Double, Double, Double, Double, Double) |]
  [| \(V6 a b c d e f) -> (D# a, D# b, D# c, D# d, D# e, D# f) |]
  [| \(D# a, D# b, D# c, D# d, D# e, D# f) -> V6 a b c d e f |]

append2 :: V2 -> Double -> V3
(V2 a b) `append2` (D# c) = V3 a b c
{-# INLINE append2 #-}

split3 :: V3 -> (V2, Double)
split3 (V3 a b c) = (V2 a b, D# c)
{-# INLINE split3 #-}

smulV2 :: Double -> V2 -> V2
smulV2 (D# s) = liftV2 (*## s)
{-# INLINE smulV2 #-}

smulV2' :: V2 -> Double -> V2
smulV2' = flip smulV2
{-# INLINE smulV2' #-}

sdivV2 :: Double -> V2 -> V2
sdivV2 (D# s) = liftV2 (/## s)
{-# INLINE sdivV2 #-}

smulV6 :: Double -> V6 -> V6
smulV6 (D# s) = liftV6 (*## s)
{-# INLINE smulV6 #-}

smulV6' :: V6 -> Double -> V6
smulV6' = flip smulV6
{-# INLINE smulV6' #-}

smulM2x2 :: Double -> M2x2 -> M2x2
smulM2x2 (D# s) = liftM2x2 (*## s)
{-# INLINE smulM2x2 #-}

smulM2x2' :: M2x2 -> Double -> M2x2
smulM2x2' = flip smulM2x2
{-# INLINE smulM2x2' #-}

plusV2 :: V2 -> V2 -> V2
plusV2 = lift2V2 (+##)
{-# INLINE plusV2 #-}

plusV6 :: V6 -> V6 -> V6
plusV6 = lift2V6 (+##)
{-# INLINE plusV6 #-}

zeroV2 :: V2
zeroV2 = V2 0.0## 0.0##

zeroP2 :: P2
zeroP2 = P2 zeroV2

minusV2 :: V2 -> V2 -> V2
minusV2 = lift2V2 (-##)
{-# INLINE minusV2 #-}

crossV2 :: V2 -> V2 -> Double
crossV2 (V2 ax ay) (V2 bx by) = D# ((ax *## by) -## (ay *## bx))
{-# INLINE crossV2 #-}

crosszV2 :: V2 -> Double -> V2
crosszV2 (V2 ax ay) (D# bz) = V2 x y
  where x = ay *## bz
        y = negateDouble# (ax *## bz)

zcrossV2 :: Double -> V2 -> V2
zcrossV2 (D# az) (V2 bx by) = V2 x y
  where x = negateDouble# (az *## by)
        y = az *## bx

unitV2 :: Double -> V2
unitV2 (D# theta) = V2 (cosDouble# theta) (sinDouble# theta)

crossV2V2 :: V2 -> V2 -> V2 -> V2
crossV2V2 (V2 ax ay) (V2 bx by) (V2 cx cy) = V2 abcx abcy
  where abz = ax *## by -## ay *## bx
        abcx = negateDouble# (abz *## cy)
        abcy = abz *## cx

vmulDiag6 :: V6 -> Diag6 -> V6
vmulDiag6 v (Diag6 m) = lift2V6 (*##) v m
{-# INLINE vmulDiag6 #-}

vmulDiag6' :: Diag6 -> V6 -> V6
vmulDiag6' (Diag6 m) v = lift2V6 (*##) v m
{-# INLINE vmulDiag6' #-}

flip3v3 :: V6 -> V6
flip3v3 (V6 a b c d e f) = V6 d e f a b c
{-# INLINE flip3v3 #-}

afdot :: P2 -> V2 -> Double
afdot (P2 v0) v1 = D# (v0 `dotV2` v1)
{-# INLINE afdot #-}

afdot' :: V2 -> P2 -> Double
afdot' = flip afdot
{-# INLINE afdot' #-}

clockwiseV2 :: V2 -> V2
clockwiseV2 (V2 x y) = V2 y (negateDouble# x)
{-# INLINE clockwiseV2 #-}

normalizeV2 :: V2 -> V2
normalizeV2 (V2 x y) = V2 (x /## n) (y /## n)
  where n = sqrtDouble# ((x *## x) +## (y *## y))
{-# INLINE normalizeV2 #-}

-- | Length of a vector.
lengthV2 :: V2 -> Double
lengthV2 (V2 x y) = D# (sqrtDouble# ((x *## x) +## (y *## y)))

-- | Squared length of a vector.
sqLengthV2 :: V2 -> Double
sqLengthV2 (V2 x y) = D# ((x *## x) +## (y *## y))

diffP2 :: P2 -> P2 -> V2
diffP2 (P2 v0) (P2 v1) = v0 `minusV2` v1
{-# INLINE diffP2 #-}

midpointP2 :: P2 -> P2 -> P2
midpointP2 (P2 v0) (P2 v1) = P2 (2 `sdivV2` (v0 `plusV2` v1))

vplusP2 :: V2 -> P2 -> P2
vplusP2 v0 (P2 v1) = P2 (v0 `plusV2` v1)

pminusV2 :: P2 -> V2 -> P2
pminusV2 (P2 v0) v1 = P2 (v0 `minusV2` v1)

pplusV2 :: P2 -> V2 -> P2
pplusV2 (P2 v0) v1 = P2 (v0 `plusV2` v1)

invM2x2 :: M2x2 -> M2x2
invM2x2 (M2x2 a b c d) =
  D# invDet `smulM2x2` M2x2 d (negateDouble# b) (negateDouble# c) a
  where det = (a *## d) -## (b *## c)
        invDet = 1.0## /## det
{-# INLINE invM2x2 #-}

negateV2 :: V2 -> V2
negateV2 = liftV2 negateDouble#
{-# INLINE negateV2 #-}

identity2x2 :: M2x2
identity2x2 = M2x2 1.0## 0.0## 0.0## 1.0##
{-# INLINE identity2x2 #-}

identity3x3 :: M3x3
identity3x3 =
  M3x3
  1.0## 0.0## 0.0##
  0.0## 1.0## 0.0##
  0.0## 0.0## 1.0##
{-# INLINE identity3x3 #-}

afmul :: M3x3 -> V2 -> V2
afmul t (V2 a b) = V2 x y
  where !(V3 x y _) = t `mul3x3c` V3 a b 1.0##
{-# INLINE afmul #-}

afmul' :: M3x3 -> P2 -> P2
afmul' t (P2 v) = P2 $ t `afmul` v
{-# INLINE afmul' #-}

{-
WORKING WITH LINES
-}

data Line2 = Line2 { linePoint :: !P2
                   , lineNormal :: !V2 }

toLine2 :: P2 -> P2 -> Line2
toLine2 a b = Line2 { linePoint = a
                    , lineNormal = clockwiseV2 (b `diffP2` a) }
{-# INLINE toLine2 #-}

perpLine2 :: P2 -> P2 -> Line2
perpLine2 a b = Line2 { linePoint = a
                      , lineNormal = b `diffP2` a }
{-# INLINE perpLine2 #-}

-- solving some `mx = b` up in here
intersect2 :: Line2 -> Line2 -> P2
intersect2 (Line2 p n@(V2 n0 n1)) (Line2 p' n'@(V2 n2 n3)) =
  P2 (invM2x2 m `mul2x2c` b)
  where b = V2 b0 b1
        !(D# b0) = p `afdot` n
        !(D# b1) = p' `afdot` n'
        m = M2x2 n0 n1 n2 n3
{-# INLINE intersect2 #-}

{-
CLIPPING LINE SEGMENTS
-}

data ClipResult a
  = ClipLeft !a -- ^ clip the left side to this new endpoint
  | ClipRight !a -- ^ clip the right side to this new endpoint
  | ClipBoth !a -- ^ the entire segment was out-of-bounds
  | ClipNone -- ^ the entire segment was in-bounds

{- |
Apply a 'ClipResult' to a line segment. Replaces clipped endpoints.
If both endpoints (entire segment) clipped, use 'Left'ed clip point.

TODO: Delete this function?
-}
applyClip ::
     ClipResult a
  -> SP a a
  -> Either a (SP a a)
applyClip res (SP a b) = case res of
  ClipLeft c -> Right (SP c b)
  ClipRight c -> Right (SP a c)
  ClipBoth c -> Left c
  ClipNone -> Right (SP a b)
{-# INLINE applyClip #-}

-- | Alternate form of 'applyClip'. 'Nothing' if entire segment clipped.
applyClip' :: ClipResult a -> SP a a -> Maybe (SP a a)
applyClip' (ClipBoth _) _ = Nothing -- redundant definition
applyClip' res seg = either (const Nothing) Just (applyClip res seg)
{-# INLINE applyClip' #-}

-- | Alternate form of 'applyClip'. Removes clipped points.
applyClip'' :: ClipResult a -> SP s s -> Maybe (Either s (SP s s))
applyClip'' res (SP a b) = case res of
  ClipLeft _ -> Just $ Left b
  ClipRight _ -> Just $ Left a
  ClipBoth _ -> Nothing
  ClipNone -> Just $ Right (SP a b)
{-# INLINE applyClip'' #-}

{- |
Alternate form of 'applyClip'. Applies clipping using the given lens.

If 'ClipBoth', then use only the 'first' vertex of the line segment
and change it to use the clipping point. (TODO: Why?)

TODO: Delete this function?
-}
lApplyClip :: ASetter' s a
  -- ^ lens to access the "point" data to apply the clipping
  -> ClipResult a
  -- ^ clipping
  -> SP s s
  -- ^ line segment with endpoints that contain "point" data
  -> Either s (SP s s)
lApplyClip l res (SP a b) = case res of
  ClipLeft c -> Right (SP (set l c a) b)
  ClipRight c -> Right (SP a (set l c b))
  ClipBoth c -> Left (set l c a) -- use the 'first' vertex by default
  ClipNone -> Right (SP a b)
{-# INLINE lApplyClip #-}

-- | Alternate form of 'lApplyClip'. If the entire segment was behind the bound, use 'Nothing'.
lApplyClip' :: ASetter' s a -> ClipResult a -> SP s s -> Maybe (SP s s)
lApplyClip' _ (ClipBoth _) _ = Nothing -- redundant definition
lApplyClip' l res seg = either (const Nothing) Just (lApplyClip l res seg)
{-# INLINE lApplyClip' #-}

{- |
Given a bounding plane (expressed as a point and a normal),
figure out how to clip a line segment so it is on the positive side of the plane.
-}
clipSegment :: Line2
  -- ^ bounding plane
  -> SP Line2 (SP P2 P2)
  -- ^ (plane of the line segment, endpoints of the line segment)
  -> ClipResult P2
  -- ^ which endpoint(s) to clip, and what point to clip to
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
{-# INLINE clipSegment #-}

{-
TRANSFORMS
-}

rotate22_ :: Double# -> Double# -> M2x2
rotate22_ cosv sinv = M2x2 cosv (negateDouble# sinv) sinv cosv
{-# INLINE rotate22_ #-}

rotate22 :: Double# -> M2x2
rotate22 ori = rotate22_ c s
  where c = cosDouble# ori
        s = sinDouble# ori
{-# INLINE rotate22 #-}

afmat33 :: M2x2 -> M3x3
afmat33 (M2x2 x0 x1 y0 y1) =
  M3x3
  x0 x1 zer
  y0 y1 zer
  zer zer one
  where !one = 1.0##
        !zer = 0.0##
{-# INLINE afmat33 #-}

aftranslate33 :: V2 -> M3x3
aftranslate33 (V2 x y) =
  M3x3
  one zer x
  zer one y
  zer zer one
  where !one = 1.0##
        !zer = 0.0##
{-# INLINE aftranslate33 #-}

afrotate33 :: Double# -> M3x3
afrotate33 ori = afmat33 (rotate22 ori)
{-# INLINE afrotate33 #-}

afscale33 :: V2 -> M3x3
afscale33 (V2 x y) =
  M3x3
  x zer zer
  zer y zer
  zer zer one
  where !one = 1.0##
        !zer = 0.0##
{-# INLINE afscale33 #-}
