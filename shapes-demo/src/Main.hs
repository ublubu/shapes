{-# LANGUAGE PatternSynonyms #-}

module Main where

import GameInit
import Linear.V2

import qualified Physics.Engine.SimpleMain as Simple
import Physics.Demo.SimpleWorld()
import qualified Physics.Engine.OptMain as Opt
import Physics.Demo.OptWorld()
import qualified Physics.Demo.World as World

main :: IO ()
main = runMain "physics test" (V2 400 300) $ World.demoMain Opt.engine
--main = runMain "contact test" (V2 800 600) TC.testMain
