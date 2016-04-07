{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Engine.Opt.Main ( module Physics.Engine.Opt.Main
                               , module Physics.Engine.Opt
                               ) where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV

import Physics.Broadphase.Opt.Aabb
import Physics.Contact.Opt (ContactBehavior)
import Physics.Constraint.Opt
import Physics.Constraints.Opt.Contact
import Physics.Solvers.Opt
import Physics.Solvers.Opt.SolutionProcessors (contactSlnProc)
import Physics.World.Opt

import Physics.Engine.Opt
import Physics.Scenes.Scene

type EngineCache s = V.MVector s (ObjectFeatureKey, ContactSolution)
type EngineState s = (World s, EngineCache s, ContactBehavior, [External PhysicalObj])
type EngineT s = StateT (EngineState s) (ST s)

initEngine :: Scene Engine -> ST s (EngineState s)
initEngine Scene{..} = do
  cache <- MV.new 0
  world <- fromList _scWorld
  return (world, cache, _scContactBeh, _scExts)

changeScene :: Scene Engine -> EngineT s ()
changeScene scene = do
  eState <- lift $ initEngine scene
  put eState

-- TODO: use MonadState and lenses in Solvers.Opt to prevent mixups with slnCache(')
updateWorld :: Double -> EngineT s ()
updateWorld dt = do
  (world, slnCache, beh, exts) <- get
  let keys = culledKeys world
  let kContacts = prepareFrame keys world
  lift $ applyExternals exts dt world
  slnCache' <- lift $ applyCachedSlns beh dt kContacts slnCache world
  lift $ improveWorld contactSlnProc kContacts slnCache' world
  lift $ improveWorld contactSlnProc kContacts slnCache' world
  lift $ advanceWorld dt world
  world' <- lift $ updateShapes world
  put (world', slnCache', beh, exts)

stepWorld :: Int -> EngineT s ()
stepWorld 0 = return ()
stepWorld x = updateWorld 0.01 >> stepWorld (x - 1)

runEngineT :: Scene Engine -> (forall s. EngineT s a) -> a
runEngineT scene action = runST $ do
  state' <- initEngine scene
  evalStateT action state'

runWorld :: Scene Engine -> Int -> ()
runWorld scene steps = runEngineT scene $ stepWorld steps
