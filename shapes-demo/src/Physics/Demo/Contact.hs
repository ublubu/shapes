{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.Demo.Contact where

import Control.Lens (makeLenses, (&), (%~))
import Data.Proxy
import GHC.Word

import Linear.Matrix
import Linear.V2
import Physics.Linear

import EasySDL.Draw
import GameLoop hiding (testStep)
import qualified SDL.Event as E
import qualified SDL.Video.Renderer as R
import qualified SDL.Input.Keyboard as K
import qualified SDL.Input.Keyboard.Codes as KC

import Physics.Draw
import Physics.Draw.Canonical
import Physics.Engine.Class
import Utils.Utils

class (ToCanonical (CDOverlap e), ToCanonical (PEConvexHull e), PhysicsEngine e, Canonical (CDOverlap e) ~ Overlap, Canonical (PEConvexHull e) ~ Polygon) => ContactDemo e where
  type CDOverlap e
  makeBox :: Proxy e -> V2' -> Double -> (Double, Double) -> PEWorldObj e
  checkOverlap :: Proxy e -> PEConvexHull e -> PEConvexHull e -> Maybe (CDOverlap e)
  checkContact :: Proxy e -> PEConvexHull e -> PEConvexHull e -> (Maybe (Flipping Contact), Maybe (CDOverlap e), Maybe (CDOverlap e))
  penetratingEdge :: Proxy e -> CDOverlap e -> (P2 Double, P2 Double)
  penetratedEdge :: Proxy e -> CDOverlap e -> (P2 Double, P2 Double)
  generateContacts :: Proxy e -> PEWorldObj e -> PEWorldObj e -> [Contact]
  objHull :: Proxy e -> PEWorldObj e -> PEConvexHull e

data DemoState =
  DemoState { _testFinished :: Bool
            , _testPosB :: V2 Double
            , _testAngleB :: Double
            }
makeLenses ''DemoState

boxA :: (ContactDemo e) => Proxy e -> DemoState -> PEWorldObj e
boxA p _ = makeBox p (V2 0 0) 0 (4, 4)

boxB :: (ContactDemo e) => Proxy e -> DemoState -> PEWorldObj e
boxB p (DemoState _ posB angleB) = makeBox p posB angleB (2, 2)

vt :: M33 Double
vt = fst $ viewTransform (V2 800 600) (V2 40 40) (V2 0 0)

overlapTest :: (ContactDemo e) => Proxy e -> R.Renderer -> PEConvexHull e -> PEConvexHull e -> IO ()
overlapTest p r sa sb = do
  renderOverlap p r (checkOverlap p sa sb)
  renderOverlap p r (checkOverlap p sb sa)

renderOverlap :: (ContactDemo e) => Proxy e -> R.Renderer -> Maybe (CDOverlap e) -> IO ()
renderOverlap p r ovl = do
  setColor r red
  maybe (print "no overlap") (drawOverlap r . transCanon vt) ovl
  maybe (return ()) (drawLine_ r . transform vt) pene
  maybe (return ()) (drawLine_ r . transform vt) edge

  where pene = fmap (penetratingEdge p) ovl
        edge = fmap (penetratedEdge p) ovl

contactTest :: (ContactDemo e) => Proxy e -> R.Renderer -> PEConvexHull e -> PEConvexHull e -> IO ()
contactTest p r sa sb = do
  setColor r lime
  renderOverlap p r ovlab
  renderOverlap p r ovlba
  setColor r orange
  maybe (print "no contact") drawC c
  where (mFlipContact, ovlab, ovlba) = checkContact p sa sb
        c :: Maybe (Either Contact Contact)
        c = flipAsEither <$> mFlipContact
        drawC = either f f
          where f = drawContact r . transform vt

contactTest' :: (ContactDemo e) => Proxy e -> R.Renderer -> PEWorldObj e -> PEWorldObj e -> IO ()
contactTest' p r a b = do
  setColor r cyan
  mapM_ f contacts
  where contacts = generateContacts p a b
        f = drawContact r . transform vt

renderTest :: (ContactDemo e) => Proxy e -> R.Renderer -> DemoState -> IO ()
renderTest p r state = do
  setColor r black
  draw sa
  draw sb
  --overlapTest p r sa sb
  --contactTest p r sa sb
  contactTest' p r a b
  where draw = drawConvexHull r . transCanon vt
        a = boxA p state
        b = boxB p state
        sa = objHull p a
        sb = objHull p b

testStep :: (ContactDemo e) => Proxy e -> R.Renderer -> DemoState -> Word32 -> IO DemoState
testStep p r s0 _ = do
  events <- E.pollEvents
  let s = foldl handleEvent s0 events
  withBlankScreen r (renderTest p r s)
  return s

handleEvent :: DemoState -> E.Event -> DemoState
handleEvent s0 (E.Event _ E.QuitEvent) = s0 { _testFinished = True }
handleEvent s0 (E.Event _ (E.KeyboardEvent (E.KeyboardEventData _ motion _ key)))
  | motion == E.Pressed = handleKeypress s0 (K.keysymScancode key) (K.keysymModifier key)
  | otherwise = s0
handleEvent s0 _ = s0

handleKeypress :: DemoState -> K.Scancode -> K.KeyModifier -> DemoState
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
  state & testAngleB %~ subtract 0.1
handleKeypress state _ _ = state

demoMain :: (ContactDemo e) => Proxy e -> R.Renderer -> IO ()
demoMain p r = runUntil (DemoState False (V2 0 2.9) 0) _testFinished (updater $ testStep p r)
