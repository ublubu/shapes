{-# LANGUAGE PatternSynonyms #-}

module Main where

import GameInit
import Linear.V2

import Physics.Engine.SimpleMain
import Physics.Demo.SimpleWorld()
import qualified Physics.Demo.World as World

main :: IO ()
main = runMain "physics test" (V2 400 300) $ World.demoMain engine
--main = runMain "contact test" (V2 800 600) TC.testMain
