{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Engine.Main ( module Physics.Engine.Main
                           , module Physics.Engine
                           ) where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV

import Physics.Broadphase.Aabb
import Physics.Constraint
import Physics.Constraints.Contact
import Physics.Constraints.Types
import Physics.Contact (ContactBehavior)
import Physics.World
import Physics.World.Object
import Physics.Solvers.Contact

import Physics.Engine
import Physics.Scenes.Scene

type World' = World WorldObj

type EngineCache s = V.MVector s (ObjectFeatureKey, ContactResult Lagrangian)
type EngineState s = (World', EngineCache s, ContactBehavior, [External WorldObj])
type EngineT s = StateT (EngineState s) (ST s)

initEngine :: Scene Engine -> ST s (EngineState s)
initEngine Scene{..} = do
  cache <- MV.new 0
  return (_scWorld, cache, _scContactBeh, _scExts)

changeScene :: Scene Engine -> EngineT s ()
changeScene scene = do
  eState <- lift $ initEngine scene
  put eState

-- TODO: can I do this with _1?
wrapUpdater :: V.Vector (ContactResult Constraint)
            -> (EngineCache s -> V.Vector (ContactResult Constraint) -> World' -> ST s World')
            -> EngineT s ()
wrapUpdater constraints f = do
  (world, cache, x, y) <- get
  world' <- lift $ f cache constraints world
  put (world', cache, x, y)

wrapUpdater' :: (World' -> ST s World') -> EngineT s World'
wrapUpdater' f = do
  (world, cache, x, y) <- get
  world' <- lift $ f world
  put (world', cache, x, y)
  return world'

wrapInitializer :: (EngineCache s -> World' -> ST s (EngineCache s, V.Vector (ContactResult Constraint), World'))
                -> EngineT s (V.Vector (ContactResult Constraint))
wrapInitializer f = do
  (world, cache, x, y) <- get
  (cache', constraints, world') <- lift $ f cache world
  put (world', cache', x, y)
  return constraints

updateWorld :: Double -> EngineT s World'
updateWorld dt = do
  (world, _, beh, exts) <- get
  let keys = culledKeys world
      kContacts = prepareFrame keys world
  void . wrapUpdater' $ return . applyExternals exts dt
  constraints <- wrapInitializer $ applyCachedSlns beh dt kContacts
  wrapUpdater constraints $ improveWorld solutionProcessor kContacts
  wrapUpdater constraints $ improveWorld solutionProcessor kContacts
  void . wrapUpdater' $ return . advanceWorld dt
  wrapUpdater' $ return . over worldObjs (fmap updateShape)

stepWorld :: Int -> EngineT s World'
stepWorld 0 = view _1 <$> get
stepWorld x = updateWorld 0.01 >> stepWorld (x - 1)

runEngineT :: Scene Engine -> (forall s. EngineT s a) -> a
runEngineT scene action = runST $ do
  state' <- initEngine scene
  evalStateT action state'

runWorld :: Scene Engine -> Int -> World'
runWorld scene steps = runEngineT scene $ stepWorld steps
