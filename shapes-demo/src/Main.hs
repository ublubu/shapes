module Main where

import           GameInit
import           Linear.V2

import qualified Physics.Demo.IOWorld as IOWorld

main :: IO ()
main =
  let window = V2 800 600
      scale = V2 40 40
  in runMain "physics test" window $ IOWorld.demoMain (fmap fromIntegral window) scale
