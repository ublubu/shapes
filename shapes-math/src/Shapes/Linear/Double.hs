{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.Double where

import GHC.Prim
import GHC.Types (Double(..))

import Shapes.Linear.Template (makeVectorType)

$(makeVectorType ''Double# 2 'D#)

testV2 :: V2
testV2 = V2 0.0## 1.0##

testV2' :: V2
testV2' = liftV2 (+## 1.0##) testV2
