{-# LANGUAGE PatternSynonyms #-}

module Physics.TestContact where

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
import Utils.Utils

data TestState = TestState { testFinished :: Bool
                           , testPosB :: V2 Double
                           , testAngleB :: Double }

pink :: D.Colour
pink = D.CustomRGBA 0xFF 0x3E 0x96 0xFF

boxA :: TestState -> LocalT Double (ConvexHull Double)
boxA _ = LocalT (toTransform (V2 0 0) 0) (rectangleHull 4 4)

boxB :: TestState -> LocalT Double (ConvexHull Double)
boxB (TestState _ posB angleB) = LocalT (toTransform posB angleB) (rectangleHull 2 2)

vt :: WorldTransform Double
vt = viewTransform (V2 400 300) (V2 40 40) (V2 0 2)

overlapTest :: SDL.T.Renderer -> TestState -> IO ()
overlapTest r state = do
  D.setColor r D.Red
  maybe (print "no overlap") (drawOverlap r . LocalT vt) ovl
  D.setColor r D.Green
  maybe (return ()) (drawLine_ r . transform vt . iExtract) pene
  D.setColor r D.Blue
  maybe (return ()) (drawLine_ r . transform vt . iExtract) edge

  where va = lmap vertices (boxA state)
        vb = lmap vertices (boxB state)
        supa = support' va
        supb = support' vb
        nas = unitEdgeNormals va
        ovl = minOverlap supa nas supb
        pene = fmap penetratingEdge ovl
        edge = fmap penetratedEdge ovl

contactTest :: SDL.T.Renderer -> TestState -> IO ()
contactTest r state = do
  D.setColor r pink
  maybe (print "no contact") drawC c
  where sa = shapeInfo (boxA state)
        sb = shapeInfo (boxB state)
        c = fmap flipAsEither $ contact sa sb
        drawC = either f f
          where f = drawContact r . LocalT vt . iExtract

renderTest :: SDL.T.Renderer -> TestState -> IO ()
renderTest r state = do
  D.setColor r D.Black
  drawConvexHull r (wExtract_ (transform vt (boxA state)))
  drawConvexHull r (wExtract_ (transform vt (boxB state)))
  contactTest r state

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

testMain :: SDL.T.Renderer -> IO ()
testMain r = runUntil (TestState False (V2 0 2.9) 0) testFinished (updater $ testStep r)
