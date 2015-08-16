{-# LANGUAGE PatternSynonyms, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

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
import Physics.ConstraintSolver
import Physics.ContactSolver
import Physics.Contact
import Physics.Linear
import Physics.Object
import Physics.Solver
import qualified Physics.Solvers as S
import Physics.Transform
import Physics.Geometry hiding (Contact)
import Physics.Draw
import Physics.DrawWorld
import Physics.External
import Physics.World hiding (testWorld)
import Physics.WorldSolver
import qualified SDL.Draw as D
import SDL.Event
import GameInit
import GameLoop hiding (testStep)
import Geometry
import Utils.Utils

import Physics.Scenes.Scene
import Physics.Scenes.Scenes
import qualified Physics.Scenes.Rolling as SC

pink = D.CustomRGBA 0xFF 0x3E 0x96 0xFF

data TestState = TestState { _testWorldState :: (World (WorldObj Double), State Double (Cache Double (WorldObj Double)))
                           , _testFinished :: Bool
                           , _testScene :: Scene (WorldObj Double) Double Double Double
                           , _testSceneIndex :: Int }
makeLenses ''TestState

updateWorld :: (Contactable n a, Epsilon n, Floating n, Ord n) => Scene a n n n -> n -> World a -> State n (Cache n a) -> (World a, State n (Cache n a))
updateWorld scene dt w s = (advanceWorld dt w', s')
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 5
        worldChanged = const . const $ True
        solver = S.contactSolver' (scene ^. scContactBeh)
        (w', s') = wsolve' solver worldChanged maxSolverIterations (allKeys w1) worldPair w1 dt s

vt :: WorldTransform Double
vt = viewTransform (V2 400 300) (V2 20 20) (V2 0 0)

initialState :: Int -> TestState
initialState i = TestState (scene ^. scWorld, emptyState) False scene i
  where scene = scenes !! i

timeStep :: Num a => a
timeStep = 10

renderTest :: SDL.T.Renderer -> TestState -> IO ()
renderTest r state = do
  D.setColor r D.Black
  drawWorld r vt (state ^. testWorldState . _1)

renderContacts :: SDL.T.Renderer -> [WorldPair [Flipping (Contact Double)]] -> IO ()
renderContacts r ps = sequence_ . join $ fmap f ps
  where f (WorldPair _ fcs) = fmap g fcs
        g = drawContact' r . LocalT vt . flipExtract

testStep :: SDL.T.Renderer -> TestState -> Word32 -> IO TestState
testStep r s0 _ = do
  events <- flushEvents
  let cs = fmap (generateContacts . toCP) <$> allPairs (s ^. testWorldState . _1)
      s = foldl handleEvent s0 events & testWorldState %~ uncurry (updateWorld (s0 ^. testScene) dt)
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
handleKeypress state SDL.E.SDL_SCANCODE_R = initialState (state ^. testSceneIndex)
handleKeypress state SDL.E.SDL_SCANCODE_N = initialState ((state ^. testSceneIndex + 1) `mod` length (scenes :: [Scene (WorldObj Double) Double Double Double]))
handleKeypress state _ = state

testMain :: SDL.T.Renderer -> IO ()
testMain r = do
  t0 <- SDL.Timer.getTicks
  timedRunUntil t0 timeStep (initialState 0) _testFinished (testStep r)
