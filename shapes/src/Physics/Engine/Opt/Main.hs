{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Engine.Opt.Main ( module Physics.Engine.Opt.Main
                               , module Physics.Engine.Opt
                               ) where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H

import Physics.Broadphase.Opt.Aabb
import Physics.World.Opt.Object
import Physics.Contact.Opt (ContactBehavior)
import Physics.Constraints.Opt.Contact
import Physics.Solvers.Opt
import Physics.Solvers.Opt.SolutionProcessors (contactSlnProc)
import Physics.World.Opt

import Physics.Engine.Opt
import Physics.Scenes.Scene

type World' = World WorldObj

type EngineState s = (World', H.HashTable s ObjectFeatureKey ContactSolution, ContactBehavior, [External WorldObj])
type EngineT s = StateT (EngineState s) (ST s)

initEngine :: Scene Engine -> ST s (EngineState s)
initEngine Scene{..} = do
  cache <- H.new
  return (_scWorld, cache, _scContactBeh, _scExts)

-- TODO: can I do this with _1?
wrapUpdater :: (World' -> ST s World') -> EngineT s World'
wrapUpdater f = do
  (world, x, y, z) <- get
  world' <- lift $ f world
  put (world', x, y, z)
  return world'

updateWorld :: Double -> EngineT s World'
updateWorld dt = do
  (world, cache, beh, exts) <- get
  let keys = culledKeys world
      kContacts = prepareFrame keys world
  void . wrapUpdater $ return . applyExternals exts dt
  void . wrapUpdater $ applyCachedSlns (fst <$> kContacts) cache
  void . wrapUpdater $ improveWorld beh contactSlnProc dt kContacts cache
  void . wrapUpdater $ improveWorld beh contactSlnProc dt kContacts cache
  void . wrapUpdater $ improveWorld beh contactSlnProc dt kContacts cache
  void . wrapUpdater $ return . advanceWorld dt
  wrapUpdater $ return . over worldObjs (fmap updateShape)

stepWorld :: Int -> EngineT s World'
stepWorld 0 = view _1 <$> get
stepWorld x = updateWorld 0.01 >> stepWorld (x - 1)

runEngineT :: Scene Engine -> (forall s. EngineT s a) -> a
runEngineT scene action = runST $ do
  state' <- initEngine scene
  evalStateT action state'

runWorld :: Scene Engine -> Int -> World'
runWorld scene steps = runEngineT scene $ stepWorld steps
