module Physics.Test where

import qualified Graphics.UI.SDL.Types as SDL.T
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

boxA ::  LocalT Double (ConvexHull Double)
boxA = LocalT (toTransform (V2 0 0) 0) (rectangleHull 20 20)

boxB :: LocalT Double (ConvexHull Double)
boxB = LocalT (toTransform (V2 0 14) 0) (rectangleHull 10 10)

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
  runMain "physics test" (Pair 800 600) testMain

data TestState = TestState { testFinished :: Bool }

pink :: D.Colour
pink = D.CustomRGBA 0xFF 0x3E 0x96 0xFF

renderTest :: SDL.T.Renderer -> IO ()
renderTest r = do
  D.setColor r D.Black
  drawConvexHull r (wExtract_ (transform vt boxA))
  drawConvexHull r (wExtract_ (transform vt boxB))
  D.setColor r D.Red
  maybe (print "no overlap") (drawOverlap r . LocalT vt) ovl
  D.setColor r D.Green
  maybe (return ()) (drawLine_ r . transform vt . iExtract) pene
  D.setColor r pink
  maybe (print "no contact") drawC c

  where vt = viewTransform (V2 800 600) (V2 8 8) (V2 0 0) :: WorldTransform Double
        sa = shapeInfo boxA
        sb = shapeInfo boxB
        ovl = minOverlap' sa sb
        pene = fmap penetratingEdge ovl
        c = contact sa sb
        drawC = either f f
          where f = drawContact r . LocalT vt . iExtract

testMain :: SDL.T.Renderer -> IO ()
testMain r = do
  D.withBlankScreen r (renderTest r)
  runUntil (TestState False) testFinished (updater $ testStep r)

testStep :: SDL.T.Renderer -> TestState -> Word32 -> IO TestState
testStep r s0 _ = do
  events <- flushEvents
  let s = foldl handleEvent s0 events
  return s

handleEvent :: TestState -> SDL.T.Event -> TestState
handleEvent s0 (SDL.T.QuitEvent _ _) = s0 { testFinished = True }
handleEvent s0 _ = s0

