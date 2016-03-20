{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.Double where

import GHC.Prim

import Shapes.Linear.Template

$(makeVectorType 2 ''Double#)

testV2 :: V2
testV2 = V2 0.0## 0.0##
