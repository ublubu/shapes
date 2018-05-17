{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This is an example of how to use this library to create and simulate a world.
It's more documentation than an API I actually expect people to use.
-}
module Physics.Engine.Main ( module Physics.Engine.Main
                           , module Physics.Engine
                           ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as V

import           Physics.Broadphase.Aabb
import qualified Physics.Broadphase.Grid     as G
import           Physics.Constraint
import           Physics.Constraints.Contact
import           Physics.Constraints.Types
import           Physics.Contact.Types       (ContactBehavior)
import           Physics.Solvers.Contact
import           Physics.World
import           Physics.World.Class
import           Physics.World.Object

import           Physics.Engine
import           Physics.Scenes.Scene

type World' a = World (WorldObj a)

type EngineCache s = V.MVector s (ObjectFeatureKey Int, ContactResult Lagrangian)
type EngineState a s = (World' a, EngineCache s, [External])
data EngineConfig =
  EngineConfig { _engineTimestep   :: Double
               , _engineContactBeh :: ContactBehavior
               } deriving Show
type EngineST a s = ReaderT EngineConfig (StateT (EngineState a s) (ST s))

gridAxes :: (G.GridAxis, G.GridAxis)
gridAxes = (G.GridAxis 20 1 (-10), G.GridAxis 20 1 (-10))

initEngine :: Scene (Engine a) -> ST s (EngineState a s)
initEngine Scene{..} = do
  cache <- MV.new 0
  return (_scWorld, cache, _scExts)

changeScene :: Scene (Engine a) -> EngineST a s ()
changeScene scene = do
  eState <- lift . lift $ initEngine scene
  put eState

-- TODO: can I do this with _1?
wrapUpdater :: V.Vector (ContactResult Constraint)
            -> (EngineCache s -> V.Vector (ContactResult Constraint) -> World' a -> ST s (World' a))
            -> EngineST a s ()
wrapUpdater constraints f = do
  (world, cache, externals) <- get
  world' <- lift . lift $ f cache constraints world
  put (world', cache, externals)

wrapUpdater' :: (World' a -> ST s (World' a)) -> EngineST a s (World' a)
wrapUpdater' f = do
  (world, cache, externals) <- get
  world' <- lift . lift $ f world
  put (world', cache, externals)
  return world'

wrapInitializer :: (EngineCache s -> (World' a) -> ST s (EngineCache s, V.Vector (ContactResult Constraint), (World' a)))
                -> EngineST a s (V.Vector (ContactResult Constraint))
wrapInitializer f = do
  (world, cache, externals) <- get
  (cache', constraints, world') <- lift . lift $ f cache world
  put (world', cache', externals)
  return constraints

updateWorld :: EngineST a s (World' a)
updateWorld = do
  EngineConfig{..} <- ask
  (world, _, exts) <- get
  let keys = G.culledKeys (G.toGrid gridAxes world)
      kContacts = prepareFrame keys world
  void . wrapUpdater' $ return . wApplyExternals exts _engineTimestep
  constraints <- wrapInitializer $ applyCachedSlns _engineContactBeh _engineTimestep kContacts
  wrapUpdater constraints $ improveWorld solutionProcessor kContacts
  wrapUpdater constraints $ improveWorld solutionProcessor kContacts
  void . wrapUpdater' $ return . wAdvance _engineTimestep
  wrapUpdater' $ return . over worldObjs (fmap woUpdateShape)

stepWorld :: Int -> EngineST a s (World' a)
stepWorld steps = do
  replicateM_ steps updateWorld
  view _1 <$> get

runEngineST :: Double -> Scene (Engine a) -> (forall s. EngineST a s b) -> b
runEngineST dt scene@Scene{..} action = runST $ do
  state' <- initEngine scene
  evalStateT (runReaderT action engineConfig) state'
  where engineConfig = EngineConfig dt _scContactBeh

runWorld :: Double -> Scene (Engine a) -> Int -> (World' a)
runWorld dt scene steps = runEngineST dt scene $ stepWorld steps
