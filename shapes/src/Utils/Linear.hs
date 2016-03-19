{-# LANGUAGE MagicHash #-}

module Utils.Linear where

import GHC.Prim

data P2 = P2 { _p2x :: Double#
             , _p2y :: Double#
             }

data V2 = V2 { _v2x :: Double#
             , _v2y :: Double#
             }

data V3 = V3 { _v3x :: Double#
             , _v3y :: Double#
             , _v3z :: Double#
             }

data V3' = V3' { _v3x' :: Double
               , _v3y' :: Double
               , _v3z' :: Double
               }

data V6 = V6 { _v6a :: Double#
             , _v6b :: Double#
             , _v6c :: Double#
             , _v6d :: Double#
             , _v6e :: Double#
             , _v6f :: Double#
             }

-- row-major
data M22 = M22 { _m22_00 :: Double#
               , _m22_01 :: Double#
               , _m22_10 :: Double#
               , _m22_11 :: Double#
               }

data M33 = M33 { _m33_00 :: Double#
               , _m33_01 :: Double#
               , _m33_02 :: Double#
               , _m33_10 :: Double#
               , _m33_11 :: Double#
               , _m33_12 :: Double#
               , _m33_20 :: Double#
               , _m33_21 :: Double#
               , _m33_22 :: Double#
               }

data M33' = M33' { _m33_00' :: Double
                 , _m33_01' :: Double
                 , _m33_02' :: Double
                 , _m33_10' :: Double
                 , _m33_11' :: Double
                 , _m33_12' :: Double
                 , _m33_20' :: Double
                 , _m33_21' :: Double
                 , _m33_22' :: Double
                 }

{-# INLINE ap2# #-}
ap2# :: (Double# -> Double#) -> V2 -> V2
f `ap2#` (V2 a b) = V2 (f a) (f b)

{-# INLINE lift2_2# #-}
lift2_2# :: (Double# -> Double# -> Double#) -> V2 -> V2 -> V2
lift2_2# f (V2 a b) (V2 c d) = V2 (f a c) (f b d)

{-# INLINE fold1_2# #-}
fold1_2# :: (Double# -> Double# -> Double#) -> V2 -> Double#
fold1_2# f (V2 a b) = f a b

{-# INLINE ap3# #-}
ap3# :: (Double# -> Double#) -> V3 -> V3
f `ap3#` (V3 a b c) = V3 (f a) (f b) (f c)

{-# INLINE lift2_3# #-}
lift2_3# :: (Double# -> Double# -> Double#) -> V3 -> V3 -> V3
lift2_3# ff (V3 a b c) (V3 d e f) = V3 (ff a d) (ff b e) (ff c f)

{-# INLINE ap3' #-}
ap3' :: (Double -> Double) -> V3' -> V3'
f `ap3'` (V3' a b c) = V3' (f a) (f b) (f c)

{-# INLINE lift2_3' #-}
lift2_3' :: (Double -> Double -> Double) -> V3' -> V3' -> V3'
lift2_3' ff (V3' a b c) (V3' d e f) = V3' (ff a d) (ff b e) (ff c f)

{-# INLINE fold1_3# #-}
fold1_3# :: (Double# -> Double# -> Double#) -> V3 -> Double#
fold1_3# f (V3 a b c) = f (f a b) c

{-# INLINE fold1_3' #-}
fold1_3' :: (Double -> Double -> Double) -> V3' -> Double
fold1_3' f (V3' a b c) = f (f a b) c

{-# INLINE ap22# #-}
ap22# :: (Double# -> Double#) -> M22 -> M22
f `ap22#` (M22 a00 a01 a10 a11) = M22 (f a00) (f a01) (f a10) (f a11)

{-# INLINE lift2_22# #-}
lift2_22# :: (Double# -> Double# -> Double#) -> M22 -> M22 -> M22
lift2_22# f (M22 a00 a01 a10 a11) (M22 b00 b01 b10 b11) =
  M22 (f a00 b00) (f a01 b01) (f a10 b10) (f a11 b11)

{-# INLINE fold1_22# #-}
fold1_22# :: (Double# -> Double# -> Double#) -> M22 -> Double#
fold1_22# f (M22 a b c d) = f (f (f a b) c) d

plus2# :: V2 -> V2 -> V2
plus2# = lift2_2# (+##)

minus2# :: V2 -> V2 -> V2
minus2# = lift2_2# (-##)

{-# INLINE dot2# #-}
dot2# :: V2 -> V2 -> Double#
dot2# (V2 a b) (V2 c d) = (a *## c) +## (b *## d)

{-# INLINE dot2'# #-}
dot2'# :: V2 -> V2 -> Double#
dot2'# x y = fold1_2# (+##) (lift2_2# (*##) x y)

{-# INLINE dot3# #-}
dot3# :: V3 -> V3 -> Double#
dot3# x y = fold1_3# (+##) (lift2_3# (*##) x y)

{-# INLINE dot3' #-}
dot3' :: V3' -> V3' -> Double
dot3' x y = fold1_3' (+) (lift2_3' (*) x y)

{-# INLINE mul22# #-}
mul22# :: M22 -> M22 -> M22
(M22 a00 a01 a10 a11) `mul22#` (M22 b00 b01 b10 b11) =
  M22 (a0 `dot2#` b0) (a0 `dot2#` b1) (a1 `dot2#` b0) (a1 `dot2#` b1)
  where a0 = V2 a00 a01
        a1 = V2 a10 a11
        b0 = V2 b00 b10
        b1 = V2 b01 b11

{-# INLINE mul33# #-}
mul33# :: M33 -> M33 -> M33
(M33 a00 a01 a02 a10 a11 a12 a20 a21 a22) `mul33#` (M33 b00 b01 b02 b10 b11 b12 b20 b21 b22) =
  M33 (f a0 b0) (f a0 b1) (f a0 b2) (f a1 b0) (f a1 b1) (f a1 b2) (f a2 b0) (f a2 b1) (f a2 b2)
  where a0 = V3 a00 a01 a02
        a1 = V3 a10 a11 a12
        a2 = V3 a20 a21 a22
        b0 = V3 b00 b10 b20
        b1 = V3 b01 b11 b21
        b2 = V3 b02 b12 b22
        f = dot3#

{-# INLINE mul33' #-}
mul33' :: M33' -> M33' -> M33'
(M33' a00 a01 a02 a10 a11 a12 a20 a21 a22) `mul33'` (M33' b00 b01 b02 b10 b11 b12 b20 b21 b22) =
  M33' (f a0 b0) (f a0 b1) (f a0 b2) (f a1 b0) (f a1 b1) (f a1 b2) (f a2 b0) (f a2 b1) (f a2 b2)
  where a0 = V3' a00 a01 a02
        a1 = V3' a10 a11 a12
        a2 = V3' a20 a21 a22
        b0 = V3' b00 b10 b20
        b1 = V3' b01 b11 b21
        b2 = V3' b02 b12 b22
        f = dot3'

{-# INLINE afdot# #-}
afdot# :: P2 -> V2 -> Double#
afdot# (P2 x y) = dot2# (V2 x y)

{-# INLINE afdot' #-}
afdot' :: P2 -> V2 -> Double#
afdot' (P2 x y) = dot2'# (V2 x y)

{-
  (V2 a b) - (V2 c d) = V2 (a - c) (b - d)
  (V2 a b) * (V2 c d) = V2 (a * c) (b * d)
  abs (V2 a b) = V2 (abs a) (abs b)
  negate (V2 a b) = V2 (negate a) (negate b)
  signum (V2 a b) = V2 (signum a) (signum b)
  fromInteger x = V2 (fromInteger x) 0

dot3 :: V3 -> V3 -> Double#
dot3 (V3 a b c) (V3 d e f) = (a * d) + (b * e) + (c * f)

rows3 :: M33 -> [V3]
rows3 (M33 a0 b0 c0 a1 b1 c1 a2 b2 c2) = [ V3 a0 b0 c0
                                         , V3 a1 b1 c1
                                         , V3 a2 b2 c2 ]

cols3 :: M33 -> [V3]
cols3 (M33 a0 b0 c0 a1 b1 c1 a2 b2 c2) = [ V3 a0 a1 a2
                                         , V3 b0 b1 b2
                                         , V3 c0 c1 c2 ]

testM33 :: M33
testM33 = M33 1 2 3 4 5 6 7 8 9

(!*!) :: M33 -> M33 -> M33
--a !*! b = fromList33 $ dot3 <$> rows3 a <*> cols3 b
(M33 a0 b0 c0 a1 b1 c1 a2 b2 c2) !*! (M33 d0 e0 f0 d1 e1 f1 d2 e2 f2) =
  M33 (a0 * d0 + b0 * d1 + c0 * d2)
      (a0 * e0 + b0 * e1 + c0 * e2)
      (a0 * f0 + b0 * f1 + c0 * f2)
      (a1 * d0 + b1 * d1 + c1 * d2)
      (a1 * e0 + b1 * e1 + c1 * e2)
      (a1 * f0 + b1 * f1 + c1 * f2)
      (a2 * d0 + b2 * d1 + c2 * d2)
      (a2 * e0 + b2 * e1 + c2 * e2)
      (a2 * f0 + b2 * f1 + c2 * f2)

(!**!) :: M22 -> M22 -> M22
(M22 a0 b0 a1 b1) !**! (M22 c0 d0 c1 d1) =
  M22 (a0 * c0 + b0 * c1)
      (a0 * d0 + b0 * d1)
      (a1 * c0 + b1 * c1)
      (a1 * d0 + b1 * d1)

afdot :: P2 -> V2 -> Double
afdot (P2 x y) (V2 x' y') = (x * x') + (y * y')

afscale33 :: V2 -> M33
afscale33 (V2 x y) = M33 x 0 0
                         0 y 0
                         0 0 1

rotate22_ :: Double -> Double -> M22
rotate22_ cosv sinv = M22 cosv (-sinv)
                          sinv cosv

rotate22 :: Double -> M22
rotate22 ori = rotate22_ c s
  where c = cos ori
        s = sin ori

aftranslate33 :: V2 -> M33
aftranslate33 (V2 x y) = M33 1 0 x
                             0 1 y
                             0 0 1

afrotate33_ :: Double -> Double -> M33
afrotate33_ cosv sinv = M33 cosv (-sinv) 0
                            sinv cosv    0
                            0    0       1

afrotate33 :: Double -> M33
afrotate33 ori = M33 c (-s) 0
                     s c    0
                     0 0    1
  where c = cos ori
        s = sin ori

identity33 :: M33
identity33 = M33 1 0 0
                 0 1 0
                 0 0 1

(!*) :: M33 -> V3 -> V3
(M33 a0 b0 c0 a1 b1 c1 a2 b2 c2) !* (V3 d0 d1 d2) =
  V3 (a0 * d0 + b0 * d1 + c0 * d2)
     (a1 * d0 + b1 * d1 + c1 * d2)
     (a2 * d0 + b2 * d1 + c2 * d2)

afmul' :: M33 -> V2 -> V2
afmul' (M33 a0 b0 _ a1 b1 _ _ _ _) (V2 d0 d1) =
  V2 (a0 * d0 + b0 * d1)
     (a1 * d0 + b1 * d1)

afmul :: M33 -> P2 -> P2
afmul (M33 a0 b0 c0 a1 b1 c1 _ _ _) (P2 d0 d1) =
  P2 (a0 * d0 + b0 * d1 + c0)
     (a1 * d0 + b1 * d1 + c1)

-}
