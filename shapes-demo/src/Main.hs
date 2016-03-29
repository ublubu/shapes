{-# LANGUAGE PatternSynonyms #-}

module Main where

import GameInit
import Linear.V2

import qualified Physics.Engine.Simple.Main as Simple
import Physics.Demo.SimpleWorld()
import Physics.Demo.SimpleContact()
import qualified Physics.Engine.Opt.Main as Opt
import Physics.Demo.OptWorld()
import Physics.Demo.OptContact()
import qualified Physics.Demo.Contact as Contact
import qualified Physics.Demo.World as World

main :: IO ()
main = runMain "physics test" (V2 400 300) $ World.demoMain Opt.engine
--main = runMain "contact test" (V2 800 600) $ Contact.demoMain Opt.engine
