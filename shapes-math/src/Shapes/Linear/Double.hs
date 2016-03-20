{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.Double where

import GHC.Prim
import GHC.Types (Double(..))

import Shapes.Linear.Template (makeVectorType)
import Shapes.Linear.ValueInfos (doubleInfo)

$(makeVectorType doubleInfo 2)

testV2 :: V2
testV2 = V2 0.0## 1.0##

testV2' :: V2
testV2' = liftV2 (+## 1.0##) testV2

testV2'' :: V2
testV2'' = lift2V2 (+##) testV2 testV2

testDot :: Double
testDot = D# (testV2 `dotV2` testV2')
