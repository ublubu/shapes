{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.Demo.World where

import Control.Monad
import Control.Lens ((^.), (.~), (%~), (&), makeLenses)
import Data.Proxy
import GHC.Word

import Linear.V2
import Linear.Matrix (M33)

import EasySDL.Draw
import qualified SDL.Event as E
import qualified SDL.Time as T
import qualified SDL.Video.Renderer as R
import qualified SDL.Input.Keyboard as K
import qualified SDL.Input.Keyboard.Codes as KC
import GameLoop hiding (testStep)

import Physics.Draw
import Physics.Draw.Canonical
import Physics.Engine.Class
import Utils.Utils

import Physics.Scenes.Scene
import Physics.Scenes.Scenes

class (PhysicsEngine e) => Demo e where
  type DEngineState e
  initialEngineState :: Proxy e -> Scene e -> DEngineState e
  drawWorld :: Proxy e -> R.Renderer -> M33 Double -> PEWorld' e -> IO ()
  demoWorld :: Proxy e -> DEngineState e -> PEWorld' e
  worldContacts :: Proxy e -> PEWorld' e -> [Contact]
  worldAabbs :: Proxy e -> PEWorld' e -> [Aabb]
  debugEngineState :: Proxy e -> DEngineState e -> String
  updateWorld :: Proxy e -> Scene e -> Double -> DEngineState e -> DEngineState e

data DemoState e =
  DemoState { _demoWorldState :: DEngineState e
            , _demoFinished :: Bool
            , _demoScene :: Scene e
            , _demoSceneIndex :: Int
            , _demoDrawDebug :: Bool
            , _demoPrintDebug :: Bool
            }
makeLenses ''DemoState

vt :: M33 Double
vt = fst $ viewTransform (V2 400 300) (V2 20 20) (V2 0 0)

initialState :: (Demo e) => Proxy e -> Int -> DemoState e
initialState p i =
  DemoState (initialEngineState p scene) False scene i True False
  where scene = scenes p !! i

nextInitialState :: (Demo e) => Proxy e -> DemoState e -> Int -> DemoState e
nextInitialState p state0 i =
  initialState p i
  & demoDrawDebug .~ (state0 ^. demoDrawDebug)
  & demoPrintDebug .~ (state0 ^. demoPrintDebug)

timeStep :: Num a => a
timeStep = 10

renderWorld :: (Demo e) => Proxy e -> R.Renderer -> DemoState e -> IO ()
renderWorld p r DemoState{..} = do
  setColor r black
  drawWorld p r vt $ demoWorld p _demoWorldState

renderContacts :: (Demo e) => Proxy e -> R.Renderer -> DemoState e -> IO ()
renderContacts p r DemoState{..} = do
  setColor r pink
  mapM_ (drawContact r . transform vt) $ worldContacts p (demoWorld p _demoWorldState)

renderAabbs :: (Demo e) => Proxy e -> R.Renderer -> DemoState e -> IO ()
renderAabbs p r DemoState{..} = do
  setColor r silver
  mapM_ (drawAabb r . transform vt) $ worldAabbs p (demoWorld p _demoWorldState)

demoStep :: (Demo e) => Proxy e -> R.Renderer -> DemoState e -> Word32 -> IO (DemoState e)
demoStep p r s0@DemoState{..} _ = do
  events <- E.pollEvents
  withBlankScreen r $ do
    renderWorld p r s0
    when (s0 ^. demoDrawDebug) $ do
      renderContacts p r s0
      renderAabbs p r s0
    when (s0 ^. demoPrintDebug) $
      print . debugEngineState p $ _demoWorldState
  let s = foldl (handleEvent p) s0 events & demoWorldState %~ updateWorld p _demoScene dt
  return s
  where dt = fromIntegral (timeStep :: Int) / 1000

handleEvent :: (Demo e) => Proxy e -> DemoState e -> E.Event -> DemoState e
handleEvent _ s0 (E.Event _ E.QuitEvent) =
  s0 { _demoFinished = True }
handleEvent p s0 (E.Event _ (E.KeyboardEvent (E.KeyboardEventData _ motion _ key)))
  | motion == E.Pressed =
    handleKeypress p s0 (K.keysymScancode key) (K.keysymModifier key)
  | otherwise = s0
handleEvent _ s0 _ = s0

handleKeypress :: (Demo e)
               => Proxy e
               -> DemoState e
               -> K.Scancode
               -> K.KeyModifier
               -> DemoState e
handleKeypress p state KC.ScancodeR _ =
  nextInitialState p state (state ^. demoSceneIndex)
handleKeypress p state KC.ScancodeN km
  | K.keyModifierLeftShift km || K.keyModifierRightShift km =
    nextInitialState p state $
    (state ^. demoSceneIndex - 1)
    `posMod` sceneCount
  | otherwise =
    nextInitialState p state $
    (state ^. demoSceneIndex + 1)
    `mod` sceneCount
  where sceneCount = length $ scenes p
handleKeypress _ state KC.ScancodeD _ =
  state & demoDrawDebug %~ not
handleKeypress _ state KC.ScancodeP _ =
  state & demoPrintDebug %~ not
handleKeypress _ state _ _ = state

demoMain :: (Demo e) => Proxy e -> R.Renderer -> IO ()
demoMain p r = do
  t0 <- T.ticks
  timedRunUntil t0 timeStep (initialState p 0) _demoFinished (demoStep p r)
