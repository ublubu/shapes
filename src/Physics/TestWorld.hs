{-# LANGUAGE PatternSynonyms, TemplateHaskell #-}

module Physics.TestWorld where

import Control.Applicative
import Control.Monad
import Control.Lens
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import GHC.Word
import Linear.Affine
import Linear.Epsilon
import Linear.Matrix
import Linear.V2
import Physics.Constraint
import Physics.Contact
import Physics.Solvers
import Physics.Linear
import Physics.Solver
import Physics.Transform
import Physics.Geometry hiding (Contact)
import Physics.Draw
import Physics.DrawWorld
import Physics.External
import Physics.World hiding (testWorld)
import qualified SDL.Draw as D
import SDL.Event
import GameInit
import GameLoop hiding (testStep)
import Geometry
import Utils.Utils

pink = D.CustomRGBA 0xFF 0x3E 0x96 0xFF

data TestState = TestState { _testWorld :: (World (PhysicalObj Double), WorldBehavior Double (PhysicalObj Double))
                           , _testFinished :: Bool }
makeLenses ''TestState

boxA = PhysicalObj { _physObjVel = V2 1 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 (-5) 0
                   , _physObjRotPos = 0
                   , _physObjHull = rectangleHull 4 4
                   , _physObjInvMass = toInvMass2 (2, 1) }

boxB = PhysicalObj { _physObjVel = V2 (-4) 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 5 2
                   , _physObjRotPos = 0
                   , _physObjHull = rectangleHull 2 2
                   , _physObjInvMass = toInvMass2 (1, 0.5) }

boxC = PhysicalObj { _physObjVel = V2 0 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 0 (-6)
                   , _physObjRotPos = 0
                   , _physObjHull = rectangleHull 18 1
                   , _physObjInvMass = toInvMass2 (0, 0) }

boxD = PhysicalObj { _physObjVel = V2 0 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 (-5) (-4)
                   , _physObjRotPos = 0
                   , _physObjHull = rectangleHull 0.4 3
                   , _physObjInvMass = toInvMass2 (1, 0) }

initialWorld = fromList [boxA, boxB, boxC, boxD]
initialBehavior = WorldBehavior [contactSolver (ContactBehavior 0.01 0.02)] [constantAccel (V2 0 (-2))] (const . const $ True) 5

vt :: WorldTransform Double
vt = viewTransform (V2 400 300) (V2 20 20) (V2 0 0)

initialState = TestState (initialWorld, initialBehavior) False

timeStep :: Num a => a
timeStep = 10

renderTest :: SDL.T.Renderer -> TestState -> IO ()
renderTest r state = do
  D.setColor r D.Black
  drawWorld r vt (state ^. testWorld . _1)

renderContacts :: SDL.T.Renderer -> [WorldPair [Flipping (Contact Double)]] -> IO ()
renderContacts r ps = sequence_ . join $ fmap f ps
  where f (WorldPair _ fcs) = fmap g fcs
        g = drawContact' r . LocalT vt . flipExtract

testStep :: SDL.T.Renderer -> TestState -> Word32 -> IO TestState
testStep r s0 _ = do
  events <- flushEvents
  let cs = fmap generateContacts <$> allPairs (s ^. testWorld . _1)
      s = foldl handleEvent s0 events & testWorld %~ uncurry (updateWorld dt)
  D.withBlankScreen r (do
                           renderTest r s0
                           D.setColor r pink
                           renderContacts r cs)
  return s
  where dt = fromIntegral timeStep / 1000

handleEvent :: TestState -> SDL.T.Event -> TestState
handleEvent s0 (SDL.T.QuitEvent _ _) = s0 { _testFinished = True }
handleEvent s0 (SDL.T.KeyboardEvent evtType _ _ _ _ key)
  | evtType == SDL.E.SDL_KEYDOWN = handleKeypress s0 (SDL.T.keysymScancode key)
  | otherwise = s0
handleEvent s0 _ = s0

handleKeypress :: TestState -> SDL.E.Scancode -> TestState
handleKeypress state SDL.E.SDL_SCANCODE_R = initialState
handleKeypress state _ = state

testMain :: SDL.T.Renderer -> IO ()
testMain r = do
  t0 <- SDL.Timer.getTicks
  timedRunUntil t0 timeStep initialState _testFinished (testStep r)
