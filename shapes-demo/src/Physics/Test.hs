{-# LANGUAGE PatternSynonyms #-}

module Physics.Test where

import Linear.V2
import Physics.TestWorld
import GameInit

main :: IO ()
main = runMain "physics test" (V2 400 300) testMain
