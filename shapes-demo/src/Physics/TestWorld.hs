{-# LANGUAGE PatternSynonyms, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Physics.TestWorld where

import Control.Monad
import Control.Lens
import EasySDL.Draw
import GHC.Word
import Linear.Epsilon
import Linear.V2
import Physics.Broadphase
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.ContactSolver
import Physics.Contact
import Physics.Object
import qualified Physics.Solvers as S
import Physics.Transform
import Physics.Draw
import Physics.DrawWorld
import Physics.World hiding (testWorld)
import Physics.WorldSolver
import GameLoop hiding (testStep)
import qualified SDL.Event as E
import qualified SDL.Time as T
import qualified SDL.Video.Renderer as R
import qualified SDL.Input.Keyboard as K
import qualified SDL.Input.Keyboard.Codes as KC
import Utils.Utils

import Physics.Scenes.Scene
import Physics.Scenes.Scenes

data TestState = TestState { _testWorldState :: (World (WorldObj Double), State Double (Cache Double (WorldObj Double)))
                           , _testFinished :: Bool
                           , _testScene :: Scene Double (WorldObj Double)
                           , _testSceneIndex :: Int }
makeLenses ''TestState

updateWorld :: (Epsilon n, Floating n, Ord n) => Scene n (WorldObj n) -> n -> (World (WorldObj n), State n (Cache n (WorldObj n))) -> (World (WorldObj n), State n (Cache n (WorldObj n)))
updateWorld scene dt (w, s) = (w''', s')
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 3
        worldChanged = const . const $ True
        solver = S.contactSolver' (scene ^. scContactBeh)
        (w', s') = wsolve' solver worldChanged maxSolverIterations (culledKeys w1) worldPair w1 dt s
        w'' = advanceWorld dt w'
        w''' = w'' & worldObjs %~ fmap updateShape

vt :: WorldTransform Double
vt = viewTransform (V2 400 300) (V2 20 20) (V2 0 0)

initialState :: Int -> TestState
initialState i = TestState (scene ^. scWorld, emptyState) False scene i
  where scene = scenes !! i

timeStep :: Num a => a
timeStep = 10

renderTest :: R.Renderer -> TestState -> IO ()
renderTest r state = do
  setColor r black
  drawWorld r vt (state ^. testWorldState . _1)

--renderContacts :: R.Renderer -> [WorldPair [Flipping (Contact Double)]] -> IO ()
--renderContacts r ps = sequence_ . join $ fmap f ps
  --where f (WorldPair _ fcs) = fmap g fcs
        --g = drawContact' r . LocalT vt . flipExtract

testStep :: R.Renderer -> TestState -> Word32 -> IO TestState
testStep r s0 _ = do
  events <- E.pollEvents
  let cs = fmap generateContacts <$> culledPairs (s ^. testWorldState . _1)
      s = foldl handleEvent s0 events & testWorldState %~ (updateWorld scene dt)
  withBlankScreen r (renderTest r s0)
  return s
  where dt = fromIntegral timeStep / 1000
        scene = s0 ^. testScene

handleEvent :: TestState -> E.Event -> TestState
handleEvent s0 (E.Event _ E.QuitEvent) = s0 { _testFinished = True }
handleEvent s0 (E.Event _ (E.KeyboardEvent (E.KeyboardEventData _ motion _ key)))
  | motion == E.Pressed = handleKeypress s0 (K.keysymScancode key) (K.keysymModifier key)
  | otherwise = s0
handleEvent s0 _ = s0

handleKeypress :: TestState -> K.Scancode -> K.KeyModifier -> TestState
handleKeypress state KC.ScancodeR _ = initialState (state ^. testSceneIndex)
handleKeypress state KC.ScancodeN km
  | K.keyModifierLeftShift km || K.keyModifierRightShift km =
    initialState ((state ^. testSceneIndex - 1)
                  `posMod` length (scenes :: [Scene Double (WorldObj Double)]))
  | otherwise = initialState ((state ^. testSceneIndex + 1) `mod` length (scenes :: [Scene Double (WorldObj Double)]))
handleKeypress state _ _ = state

testMain :: R.Renderer -> IO ()
testMain r = do
  t0 <- T.ticks
  timedRunUntil t0 timeStep (initialState 0) _testFinished (testStep r)
