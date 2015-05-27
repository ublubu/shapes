module Main where

import qualified Graphics.UI.SDL.Timer as SDL.Timer
import GHC.Word
import Debug.Trace
import SDL.Geometry
import Directional
import GameLoop
import GameInit
import Geometry
import qualified SideScroller.Game as Game

main :: IO ()
main = runMain "SideScroller" window (Game.main window)
  where window = Pair 800 600
