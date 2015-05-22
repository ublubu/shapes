module Main where

import qualified Graphics.UI.SDL.Timer as SDL.Timer
import GHC.Word
import Debug.Trace
import SDL.Geometry
import Directional
import GameLoop
import SideScroller.RectangularWorld

advanceFrame :: (Word32, Word32) -> Word32 -> IO (Word32, Word32)
advanceFrame (t0, t) t' = do
  print (t' - t0)
  return (t0, t')

testFrame :: (Word32, Word32) -> Bool
testFrame (t, t') = t' - t < 1000

main :: IO ()
main = do
  t0 <- SDL.Timer.getTicks
  timedRunUntil t0 200 (t0, t0) testFrame advanceFrame
