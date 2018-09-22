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

import           Physics.Engine
import           Physics.Scenes.Scene

type EngineCache s = V.MVector s (ObjectFeatureKey Int, ContactLagrangian)
type EngineState label s = (World s label, EngineCache s, External)
data EngineConfig = EngineConfig
  { _engineTimestep :: Double
  , _engineContactBeh :: ContactBehavior
  } deriving (Show)
type EngineST label s = ReaderT EngineConfig (StateT (EngineState label s) (ST s))

gridAxes :: (G.GridAxis, G.GridAxis)
gridAxes = (G.GridAxis 20 1 (-10), G.GridAxis 20 1 (-10))

initEngine :: Scene s label -> ST s (EngineState label s)
initEngine Scene{..} = do
  cache <- MV.new 0
  return (_scWorld, cache, _scExts)

changeScene :: Scene s label -> EngineST label s ()
changeScene scene = do
  eState <- lift . lift $ initEngine scene
  put eState

wrapUpdater ::
     V.Vector ContactConstraint
  -> (EngineCache s -> V.Vector ContactConstraint -> World s label -> ST s ())
  -> EngineST label s ()
wrapUpdater constraints f = do
  (world, cache, externals) <- get
  lift . lift $ f cache constraints world

wrapInitializer ::
     (EngineCache s -> World s label -> ST s ( EngineCache s
                                             , V.Vector ContactConstraint))
  -> EngineST label s (V.Vector ContactConstraint)
wrapInitializer f = do
  (world, cache, externals) <- get
  (cache', constraints) <- lift . lift $ f cache world
  put (world, cache', externals)
  return constraints

updateWorld :: EngineST label s ()
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

stepWorld :: Int -> EngineST label s ()
stepWorld steps = replicateM_ steps updateWorld

runEngine :: Double -> Scene s label -> EngineST label s b -> ST s b
runEngine dt scene@Scene{..} action = do
  state' <- initEngine scene
  evalStateT (runReaderT action engineConfig) state'
  where engineConfig = EngineConfig dt _scContactBeh

runWorld :: Double -> Scene s label -> Int -> ST s ()
runWorld dt scene steps = runEngine dt scene $ stepWorld steps
