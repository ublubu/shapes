{-# LANGUAGE TypeFamilies #-}

module Physics.Demo.OptWorld where

import Control.Lens
import Control.Monad
import qualified Data.IntMap.Strict as IM

import qualified Physics.Broadphase.OptAabb as B
import Physics.Contact.OptContact
import Physics.Engine.Opt
import qualified Physics.Engine.OptMain as OM
import Physics.World.OptWorld
import Physics.Solvers.OptSolvers (toShowableSolverState)

import Physics.Draw.Canonical
import qualified Physics.Draw.Opt as D
import Physics.Demo.World (Demo(..))

import Utils.Utils

instance Demo Engine where
  type DEngineState Engine = OM.EngineState
  initialEngineState _ = OM.defaultInitialState
  drawWorld _ = D.drawWorld
  demoWorld _ = _spFst
  worldContacts _ world =
    join $ f <$> cs
    where f (WorldPair _ fcs) = fmap (toCanonical . flipExtract) fcs
          cs = fmap generateContacts <$> B.culledPairs world
  worldAabbs _ world =
    toCanonical <$> IM.elems (B.toAabbs world)
  debugEngineState _ = show . over spSnd toShowableSolverState
  updateWorld _ = OM.updateWorld
