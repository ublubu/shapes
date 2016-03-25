{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Physics.Constraint.OptLinear where

import GHC.Prim
import GHC.Types (Double(D#))

import Shapes.Linear.Template (makeVectorType, defineJoinSplit)
import Shapes.Linear.MatrixTemplate
import Shapes.Linear.ValueInfos (doubleInfo)

$(makeVectorType doubleInfo 2)
$(makeVectorType doubleInfo 3)
$(makeVectorType doubleInfo 6)
$(makeMatrixType doubleInfo (2, 2))
$(makeMatrixType doubleInfo (6, 6))
$(defineMatrixMul doubleInfo (2, 2, 2))
$(defineJoinSplit doubleInfo (3, 3))

newtype Diag6 = Diag6 V6

append2 :: V2 -> Double -> V3
(V2 a b) `append2` (D# c) = V3 a b c

split3 :: V3 -> (V2, Double)
split3 (V3 a b c) = (V2 a b, D# c)

smulV2 :: Double -> V2 -> V2
smulV2 (D# lagr) = liftV2 (\x -> x *## lagr)

smulV6 :: Double -> V6 -> V6
smulV6 (D# lagr) = liftV6 (\x -> x *## lagr)

smulV6' :: V6 -> Double -> V6
smulV6' j (D# lagr) = liftV6 (\x -> x *## lagr) j

plusV2 :: V2 -> V2 -> V2
plusV2 = lift2V2 (+##)

plusV6 :: V6 -> V6 -> V6
plusV6 = lift2V6 (+##)

vmulDiag6 :: V6 -> Diag6 -> V6
vmulDiag6 v (Diag6 m) = lift2V6 (*##) v m

vmulDiag6' :: Diag6 -> V6 -> V6
vmulDiag6' (Diag6 m) v = lift2V6 (*##) v m
