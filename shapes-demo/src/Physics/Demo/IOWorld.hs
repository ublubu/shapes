{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Physics.Demo.IOWorld where

import           Control.Lens               (makeLenses, view, (%~), (&), (.~),
                                             (^.), _1)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Maybe
import qualified Data.Vector.Unboxed        as V

import           Linear.Matrix              (M33)
import           Linear.V2

import           EasySDL.Draw
import           GameLoop                   hiding (testStep)
import qualified SDL.Event                  as E
import qualified SDL.Input.Keyboard         as K
import qualified SDL.Input.Keyboard.Codes   as KC
import qualified SDL.Time                   as T
import qualified SDL.Video.Renderer         as R

import qualified Physics.Broadphase.Aabb    as B
import qualified Physics.Broadphase.Grid    as G
import           Physics.Contact
import           Physics.Contact.ConvexHull
import           Physics.Contact.Types
import           Physics.Demo.Scenes
import           Physics.Draw
import           Physics.Draw.Canonical
import qualified Physics.Draw.Opt           as D
import           Physics.Engine
import qualified Physics.Engine.Main        as OM
import           Physics.Scenes.Scene
import           Physics.World
import qualified Physics.Solvers.Contact as S

import           Utils.Descending
import           Utils.Utils

type DemoM = ReaderT OM.EngineConfig (StateT (OM.EngineState () RealWorld) IO)

convertEngineT :: OM.EngineST () RealWorld a -> DemoM a
convertEngineT action =
  ReaderT (\config -> StateT (\state -> stToIO $ runStateT (runReaderT action config) state))

runDemo :: Scene RealWorld () -> DemoM a -> IO a
runDemo scene@Scene{..} action = do
  eState <- liftIO . stToIO $ OM.initEngine scene
  evalStateT (runReaderT action eConfig) eState
  where eConfig = OM.EngineConfig 0.01 _scContactBeh

resetEngine :: Scene RealWorld () -> DemoM ()
resetEngine scene =
  convertEngineT $ OM.changeScene scene

drawWorld :: R.Renderer -> M33 Double -> DemoM ()
drawWorld r vt = do
  world <- demoWorld
  liftIO $ D.drawWorld r vt world

demoWorld :: DemoM (World RealWorld ())
demoWorld = view _1 <$> get

worldContacts :: DemoM [Contact]
worldContacts = do
  world <- demoWorld
  keys <- lifty $ G.culledKeys <$> G.toGrid OM.gridAxes world
  contacts_ <- lifty $ S.prepareFrame keys world
  let contacts :: Descending Contact'
      contacts = fmap (flipExtractUnsafe . snd) contacts_
  return . _descList $ toCanonical <$> contacts

lifty :: (MonadIO m) => ST RealWorld a -> m a
lifty = liftIO . stToIO

worldAabbs :: DemoM [Aabb]
worldAabbs = do
  world <- demoWorld
  aabbs <- lifty $ B.toAabbs world
  return $ toCanonical . snd <$> V.toList aabbs

debugEngineState :: DemoM String
debugEngineState = return "<insert debug trace here>"

updateWorld :: DemoM ()
updateWorld = void . convertEngineT $ OM.updateWorld

data DemoState =
  DemoState { _demoFinished      :: Bool
            , _demoSceneIndex    :: Int
            , _demoDrawDebug     :: Bool
            , _demoPrintDebug    :: Bool
            , _demoViewTransform :: M33 Double
            }
makeLenses ''DemoState

getViewTransform :: V2 Double -> V2 Double -> M33 Double
getViewTransform window scale = fst $ viewTransform window scale (V2 0 0)

initialState :: Int -> M33 Double -> DemoState
initialState i = DemoState False i True False

nextInitialState :: DemoState -> Int -> DemoM DemoState
nextInitialState DemoState{..} i = do
  scene <- lifty $ scenes i
  resetEngine scene
  return $ initialState i _demoViewTransform
    & demoDrawDebug .~ _demoDrawDebug
    & demoPrintDebug .~ _demoPrintDebug

timeStep :: Num a => a
timeStep = 10

renderWorld :: R.Renderer -> DemoState -> DemoM ()
renderWorld r DemoState{..} = do
  liftIO $ setColor r black
  drawWorld r _demoViewTransform

renderContacts :: R.Renderer -> DemoState -> DemoM ()
renderContacts r DemoState{..} = do
  liftIO $ setColor r pink
  contacts <- worldContacts
  liftIO $ mapM_ (drawContact r . transform _demoViewTransform) contacts

renderAabbs :: R.Renderer -> DemoState -> DemoM ()
renderAabbs r DemoState{..} = do
  liftIO $ setColor r silver
  aabbs <- worldAabbs
  liftIO $ mapM_ (drawAabb r . transform _demoViewTransform) aabbs

demoStep :: R.Renderer -> DemoState -> DemoM DemoState
demoStep r s0@DemoState {..} = do
  events <- liftIO E.pollEvents
  liftIO $ clearScreen r
  renderWorld r s0
  when (s0 ^. demoDrawDebug) $ do
    renderContacts r s0
    renderAabbs r s0
  when (s0 ^. demoPrintDebug) $ do
    debug <- debugEngineState
    liftIO $ print debug
  s1 <- foldM handleEvent s0 events
  updateWorld
  liftIO $ R.present r
  return s1

handleEvent :: DemoState -> E.Event -> DemoM DemoState
handleEvent s0 (E.Event _ E.QuitEvent) =
  return s0 { _demoFinished = True }
handleEvent s0 (E.Event _ (E.KeyboardEvent (E.KeyboardEventData _ motion _ key)))
  | motion == E.Pressed =
    handleKeypress s0 (K.keysymScancode key) (K.keysymModifier key)
  | otherwise = return s0
handleEvent s0 _ = return s0

handleKeypress :: DemoState
               -> K.Scancode
               -> K.KeyModifier
               -> DemoM DemoState
handleKeypress state KC.ScancodeR _ =
  nextInitialState state (state ^. demoSceneIndex)
handleKeypress state KC.ScancodeN km
  | K.keyModifierLeftShift km || K.keyModifierRightShift km =
    nextInitialState state $
    (state ^. demoSceneIndex - 1)
    `posMod` sceneCount
  | otherwise =
    nextInitialState state $
    (state ^. demoSceneIndex + 1)
    `mod` sceneCount
handleKeypress state KC.ScancodeD _ =
  return $ state & demoDrawDebug %~ not
handleKeypress state KC.ScancodeP _ =
  return $ state & demoPrintDebug %~ not
handleKeypress state _ _ = return state

demoMain :: V2 Double -> V2 Double -> R.Renderer -> IO ()
demoMain window scale r = do
  t0 <- T.ticks
  let demo = timedRunUntil t0 timeStep (initialState 0 $ getViewTransform window scale) _demoFinished (\s _ -> demoStep r s)
  scene <- lifty $ scenes 0
  runDemo scene demo
