{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Physics.TestContact where

import Control.Monad
import Control.Lens (makeLenses, (&), (.~), (^.), (%~), (^?))
import Data.Array
import EasySDL.Draw
import GHC.Word
import Linear.Affine
import Linear.Matrix
import Linear.V2
import Physics.Constraint (PhysicalObj(..))
import Physics.Contact (generateContacts, unwrapContactResult)
import Physics.ConvexHull
import Physics.Linear
import Physics.Object
import Physics.SAT (contactDebug, minOverlap', penetratingEdge, penetratedEdge, Overlap(..), _MinOverlap, Contact(..))
import Physics.Draw
import Physics.Draw.Canonical (transform, transCanon)
import Physics.Draw.Simple
import SDL.Event
import GameInit
import GameLoop hiding (testStep)
import qualified SDL.Event as E
import qualified SDL.Time as T
import qualified SDL.Video.Renderer as R
import qualified SDL.Input.Keyboard as K
import qualified SDL.Input.Keyboard.Codes as KC
import Utils.Utils

data TestState = TestState { _testFinished :: Bool
                           , _testPosB :: V2 Double
                           , _testAngleB :: Double }
makeLenses ''TestState

makeBox :: V2 Double -> Double -> (Double, Double) -> WorldObj Double
makeBox pos rot (w, h) =
  makeWorldObj phys 0.2 (rectangleHull w h)
  where phys = PhysicalObj (V2 0 0) 0 pos rot (1, 1)

boxA :: TestState -> WorldObj Double
boxA _ = makeBox (V2 0 0) 0 (4, 4)

boxB :: TestState -> WorldObj Double
boxB (TestState _ posB angleB) = makeBox posB angleB (2, 2)

vt :: M33 Double
vt = fst $ viewTransform (V2 800 600) (V2 40 40) (V2 0 0)

overlapTest :: R.Renderer -> ConvexHull Double -> ConvexHull Double -> IO ()
overlapTest r sa sb = do
  renderOverlap r (minOverlap' sa sb ^? _MinOverlap)
  renderOverlap r (minOverlap' sb sa ^? _MinOverlap)

renderOverlap :: R.Renderer -> Maybe (Overlap Double) -> IO ()
renderOverlap r ovl = do
  setColor r red
  maybe (print "no overlap") (drawOverlap r . transCanon vt) ovl
  maybe (return ()) (drawLine_ r . transform vt) pene
  maybe (return ()) (drawLine_ r . transform vt) edge

  where f = pairMap _neighborhoodCenter
        pene = fmap (f . penetratingEdge) ovl
        edge = fmap (f . penetratedEdge) ovl

contactTest :: R.Renderer -> ConvexHull Double -> ConvexHull Double -> IO ()
contactTest r sa sb = do
  setColor r lime
  renderOverlap r $ ovlab ^? _MinOverlap
  renderOverlap r $ ovlba ^? _MinOverlap
  setColor r orange
  maybe (print "no contact") drawC c
  where (mFlipContact, ovlab, ovlba) = contactDebug sa sb
        c :: Maybe (Either (Contact Double) (Contact Double))
        c = fmap flipAsEither . unwrapContactResult $ mFlipContact
        drawC = either f f
          where f = drawContact r . transCanon vt

contactTest' :: R.Renderer -> WorldObj Double -> WorldObj Double -> IO ()
contactTest' r a b = do
  setColor r cyan
  mapM_ f contacts
  where contacts = generateContacts (a, b)
        f = drawContact r . transCanon vt . flipExtract

renderTest :: R.Renderer -> TestState -> IO ()
renderTest r state = do
  setColor r black
  draw $ sa
  draw $ sb
  --overlapTest r sa sb
  contactTest r sa sb
  --contactTest' r a b
  where draw = drawConvexHull r . transform vt . elems . _hullVertices
        a = boxA state
        b = boxB state
        sa = _worldShape a
        sb = _worldShape b

testStep :: R.Renderer -> TestState -> Word32 -> IO TestState
testStep r s0 _ = do
  events <- E.pollEvents
  let s = foldl handleEvent s0 events
  withBlankScreen r (renderTest r s)
  return s

handleEvent :: TestState -> E.Event -> TestState
handleEvent s0 (E.Event _ E.QuitEvent) = s0 { _testFinished = True }
handleEvent s0 (E.Event _ (E.KeyboardEvent (E.KeyboardEventData _ motion _ key)))
  | motion == E.Pressed = handleKeypress s0 (K.keysymScancode key) (K.keysymModifier key)
  | otherwise = s0
handleEvent s0 _ = s0

handleKeypress :: TestState -> K.Scancode -> K.KeyModifier -> TestState
handleKeypress state KC.ScancodeH _ =
  state & testPosB %~ (+ (V2 (-0.1) 0))
handleKeypress state KC.ScancodeJ _ =
  state & testPosB %~ (+ (V2 0 (-0.1)))
handleKeypress state KC.ScancodeK _ =
  state & testPosB %~ (+ (V2 0 0.1))
handleKeypress state KC.ScancodeL _ =
  state & testPosB %~ (+ (V2 0.1 0))
handleKeypress state KC.ScancodeR _ =
  state & testAngleB %~ (+ 0.1)
handleKeypress state KC.ScancodeU _ =
  state & testAngleB %~ (subtract 0.1)
handleKeypress state _ _ = state

testMain :: R.Renderer -> IO ()
testMain r = runUntil (TestState False (V2 0 2.9) 0) _testFinished (updater $ testStep r)
