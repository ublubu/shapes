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
import           Control.Monad.Primitive
import           Control.Monad.Reader
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

import           Physics.Engine
import           Physics.Scenes.Scene

type EngineCache s
   = V.MVector s (ObjectFeatureKey Int, ContactResult Lagrangian)
type EngineState label s = (World s label, EngineCache s, External)
data EngineConfig = EngineConfig
  { _engineTimestep :: Double
  , _engineContactBeh :: ContactBehavior
  } deriving (Show)
type EngineM label m = ReaderT EngineConfig (StateT (EngineState label (PrimState m)) m)

gridAxes :: (G.GridAxis, G.GridAxis)
gridAxes = (G.GridAxis 20 1 (-10), G.GridAxis 20 1 (-10))

initEngine ::
     (PrimMonad m)
  => Scene (PrimState m) label
  -> m (EngineState label (PrimState m))
initEngine Scene{..} = do
  cache <- MV.new 0
  return (_scWorld, cache, _scExts)

changeScene :: (PrimMonad m) => Scene (PrimState m) label -> EngineM label m ()
changeScene scene = do
  eState <- lift . lift $ initEngine scene
  put eState

type EngineUpdater label m
   = EngineCache (PrimState m) -> V.Vector (ContactResult Constraint) -> World (PrimState m) label -> m ()

wrapUpdater ::
     (PrimMonad m)
  => V.Vector (ContactResult Constraint)
  -> EngineUpdater label m
  -> EngineM label m ()
wrapUpdater constraints f = do
  (world, cache, externals) <- get
  lift . lift $ f cache constraints world

type EngineInitializer label m
   = EngineCache (PrimState m) -> (World (PrimState m) label) -> m ( EngineCache (PrimState m)
                                                                   , V.Vector (ContactResult Constraint))

wrapInitializer ::
     (PrimMonad m)
  => EngineInitializer label m
  -> EngineM label m (V.Vector (ContactResult Constraint))
wrapInitializer f = do
  (world, cache, externals) <- get
  (cache', constraints) <- lift . lift $ f cache world
  put (world, cache', externals)
  return constraints

updateWorld :: (PrimMonad m) => EngineM label m ()
updateWorld = do
  EngineConfig {..} <- ask
  (world, _, exts) <- get
  keys <- lifty $ G.culledKeys <$> G.toGrid gridAxes world
  lifty $ applyExternal exts _engineTimestep world
  kContacts <- lifty $ prepareFrame keys world
  constraints <-
    wrapInitializer $
    applyCachedSlns _engineContactBeh _engineTimestep kContacts
  wrapUpdater constraints $ improveWorld solutionProcessor kContacts
  wrapUpdater constraints $ improveWorld solutionProcessor kContacts
  lifty $ advance _engineTimestep world
  lifty $ moveShapes world
  where
    lifty = lift . lift

stepWorld :: (PrimMonad m) => Int -> EngineM label m ()
stepWorld steps = replicateM_ steps updateWorld

runEngine ::
     (PrimMonad m)
  => Double
  -> Scene (PrimState m) label
  -> EngineM label m b
  -> m b
runEngine dt scene@Scene{..} action = do
  state' <- initEngine scene
  evalStateT (runReaderT action engineConfig) state'
  where engineConfig = EngineConfig dt _scContactBeh

runWorld :: (PrimMonad m) => Double -> Scene (PrimState m) label -> Int -> m ()
runWorld dt scene steps = runEngine dt scene $ stepWorld steps
