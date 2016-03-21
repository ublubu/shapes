{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.Double where

import GHC.Prim
import GHC.Types (Double(..))

import Shapes.Linear.Template (makeVectorType)
import Shapes.Linear.MatrixTemplate
import Shapes.Linear.ValueInfos (doubleInfo)

$(makeVectorType doubleInfo 2)
$(makeMatrixType doubleInfo (2, 2))
$(defineMatrixMul doubleInfo (2, 2, 2))

testV2 :: V2
testV2 = V2 0.0## 1.0##

testV2' :: V2
testV2' = liftV2 (+## 1.0##) testV2

testV2'' :: V2
testV2'' = lift2V2 (+##) testV2 testV2

testDot :: Double
testDot = D# (testV2 `dotV2` testV2')

testM2x2 :: M2x2
testM2x2 = M2x2 0.0## 1.0## 2.0## 3.0##

idM2x2 :: M2x2
idM2x2 = fromListM2x2 [1, 0, 0, 1]
