{-# LANGUAGE PatternSynonyms #-}

module Main where

import GameInit
import Linear.V2

import qualified Physics.Engine.Main as Opt
import Physics.Demo.OptWorld()
import Physics.Demo.OptContact()
import qualified Physics.Demo.Contact as Contact
import qualified Physics.Demo.IOWorld as IOWorld

main :: IO ()
main =
  let window = V2 800 600
      scale = V2 40 40
  in runMain "physics test" window $ IOWorld.demoMain Opt.engineP (fmap fromIntegral window) scale
--main = runMain "contact test" (V2 800 600) $ Contact.demoMain Opt.engineP
