{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.Double where

import GHC.Prim
import GHC.Types (Double(..))

import Shapes.Linear.Template (makeVectorType)

$(makeVectorType 2 ''Double# 'D#)

testV2 :: V2
testV2 = V2 0.0## 0.0##
