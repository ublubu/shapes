{-# LANGUAGE PatternSynonyms #-}

module Physics.Test where

import Linear.V2
import Physics.TestWorld
import qualified Physics.TestContact as TC
import GameInit

main :: IO ()
main = runMain "physics test" (V2 400 300) testMain
--main = runMain "contact test" (V2 800 600) TC.testMain
