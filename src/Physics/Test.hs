{-# LANGUAGE PatternSynonyms #-}

module Physics.Test where

import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Enum as SDL.E
import GHC.Word
import Linear.Affine
import Linear.Matrix
import Linear.V2
import Physics.Linear
import Physics.Transform
import Physics.Geometry
import Physics.Draw
import qualified SDL.Draw as D
import SDL.Event
import GameInit
import GameLoop
import Geometry

main :: IO ()
main = do
  let v = V2 3 4
      h = horizontalizer22 v
    in print (h !* v)
  let p = P $ V2 0 0
      n = V2 0 1 :: V2 Double
      p' = P $ V2 2 0
      n' = V2 (-1) 1
    in print (intersect2 (Line2 p n) (Line2 p' n'))
  runMain "physics test" (Pair 400 300) testMain

data TestState = TestState { testFinished :: Bool
                           , testPosB :: V2 Double
                           , testAngleB :: Double }

pink :: D.Colour
pink = D.CustomRGBA 0xFF 0x3E 0x96 0xFF

renderTest :: SDL.T.Renderer -> TestState -> IO ()
renderTest r (TestState _ posB angleB) = do
  D.setColor r D.Black
  drawConvexHull r (wExtract_ (transform vt boxA))
  drawConvexHull r (wExtract_ (transform vt boxB))
  D.setColor r pink
  maybe (print "no contact") drawC c

  where vt = viewTransform (V2 400 300) (V2 40 40) (V2 0 2) :: WorldTransform Double
        sa = shapeInfo boxA
        sb = shapeInfo boxB
        boxA = LocalT (toTransform (V2 0 0) 0) (rectangleHull 4 4)
        boxB = LocalT (toTransform posB angleB) (rectangleHull 2 2)
        c = contact sa sb
        drawC = either f f
          where f = drawContact r . LocalT vt . iExtract

testMain :: SDL.T.Renderer -> IO ()
testMain r = runUntil (TestState False (V2 0 2.9) 0) testFinished (updater $ testStep r)

testStep :: SDL.T.Renderer -> TestState -> Word32 -> IO TestState
testStep r s0 _ = do
  events <- flushEvents
  let s = foldl handleEvent s0 events
  D.withBlankScreen r (renderTest r s)
  return s

handleEvent :: TestState -> SDL.T.Event -> TestState
handleEvent s0 (SDL.T.QuitEvent _ _) = s0 { testFinished = True }
handleEvent s0 (SDL.T.KeyboardEvent evtType _ _ _ _ key)
  | evtType == SDL.E.SDL_KEYDOWN = handleKeypress s0 (SDL.T.keysymScancode key)
  | otherwise = s0
handleEvent s0 _ = s0

handleKeypress :: TestState -> SDL.E.Scancode -> TestState
handleKeypress state SDL.E.SDL_SCANCODE_H =
  state { testPosB = (testPosB state) + (V2 (-0.1) 0) }
handleKeypress state SDL.E.SDL_SCANCODE_J =
  state { testPosB = (testPosB state) + (V2 0 (-0.1)) }
handleKeypress state SDL.E.SDL_SCANCODE_K =
  state { testPosB = (testPosB state) + (V2 0 0.1) }
handleKeypress state SDL.E.SDL_SCANCODE_L =
  state { testPosB = (testPosB state) + (V2 0.1 0) }
handleKeypress state SDL.E.SDL_SCANCODE_R =
  state { testAngleB = (testAngleB state) + 0.1 }
handleKeypress state SDL.E.SDL_SCANCODE_U =
  state { testAngleB = (testAngleB state) - 0.1 }
handleKeypress state _ = state

