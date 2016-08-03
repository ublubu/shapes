{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Engine.Main ( module Physics.Engine.Main
                           , module Physics.Engine
                           ) where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.ST
import Control.Monad.Reader
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV

import Physics.Broadphase.Aabb
import Physics.Constraint
import Physics.Constraints.Contact
import Physics.Constraints.Types
import Physics.Contact (ContactBehavior)
import Physics.World
import Physics.World.Class
import Physics.World.Object
import Physics.Solvers.Contact

import Physics.Engine
import Physics.Scenes.Scene

type World' = World WorldObj

type EngineCache s = V.MVector s (ObjectFeatureKey Int, ContactResult Lagrangian)
type EngineState s = (World', EngineCache s, [External])
data EngineConfig =
  EngineConfig { _engineTimestep :: Double
               , _engineContactBeh :: ContactBehavior
               } deriving Show
type EngineT s = ReaderT EngineConfig (StateT (EngineState s) (ST s))

initEngine :: Scene Engine -> ST s (EngineState s)
initEngine Scene{..} = do
  cache <- MV.new 0
  return (_scWorld, cache, _scExts)

changeScene :: Scene Engine -> EngineT s ()
changeScene scene = do
  eState <- lift . lift $ initEngine scene
  put eState

-- TODO: can I do this with _1?
wrapUpdater :: V.Vector (ContactResult Constraint)
            -> (EngineCache s -> V.Vector (ContactResult Constraint) -> World' -> ST s World')
            -> EngineT s ()
wrapUpdater constraints f = do
  (world, cache, externals) <- get
  world' <- lift . lift $ f cache constraints world
  put (world', cache, externals)

wrapUpdater' :: (World' -> ST s World') -> EngineT s World'
wrapUpdater' f = do
  (world, cache, externals) <- get
  world' <- lift . lift $ f world
  put (world', cache, externals)
  return world'

wrapInitializer :: (EngineCache s -> World' -> ST s (EngineCache s, V.Vector (ContactResult Constraint), World'))
                -> EngineT s (V.Vector (ContactResult Constraint))
wrapInitializer f = do
  (world, cache, externals) <- get
  (cache', constraints, world') <- lift . lift $ f cache world
  put (world', cache', externals)
  return constraints

updateWorld :: EngineT s World'
updateWorld = do
  EngineConfig{..} <- ask
  (world, _, exts) <- get
  let keys = culledKeys world
      kContacts = prepareFrame keys world
  void . wrapUpdater' $ return . wApplyExternals exts _engineTimestep
  constraints <- wrapInitializer $ applyCachedSlns _engineContactBeh _engineTimestep kContacts
  wrapUpdater constraints $ improveWorld solutionProcessor kContacts
  wrapUpdater constraints $ improveWorld solutionProcessor kContacts
  void . wrapUpdater' $ return . wAdvance _engineTimestep
  wrapUpdater' $ return . over worldObjs (fmap woUpdateShape)

stepWorld :: Int -> EngineT s World'
stepWorld steps = do
  replicateM_ steps updateWorld
  view _1 <$> get

runEngineT :: Double -> Scene Engine -> (forall s. EngineT s a) -> a
runEngineT dt scene@Scene{..} action = runST $ do
  state' <- initEngine scene
  evalStateT (runReaderT action engineConfig) state'
  where engineConfig = EngineConfig dt _scContactBeh

runWorld :: Double -> Scene Engine -> Int -> World'
runWorld dt scene steps = runEngineT dt scene $ stepWorld steps
